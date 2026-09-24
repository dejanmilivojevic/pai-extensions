;;; pai-mcp.el --- Token-efficient MCP adapter for pai -*- lexical-binding: t; -*-

;; Port of https://github.com/nicobailon/pi-mcp-adapter (MIT) to the pai
;; extension API.  Instead of registering every MCP server's tools (which burns
;; context), it exposes ONE proxy tool `mcp' with search/describe/call actions.
;; Servers are lazy: an MCP server connects automatically the first time one
;; of its tools is called, and metadata is cached so search/describe work
;; without a live connection.  Everything is asynchronous, so the UI never
;; blocks while a server starts or a tool runs.

;;; Commentary:

;; Entry point: wires the config (`pai-mcp-config') and async client
;; (`pai-mcp-client') into the `mcp' proxy tool, the `/mcp' slash command, and
;; the extension registration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai)
(require 'pai-ext)
(require 'pai-tools)
(require 'pai-mcp-config)
(require 'pai-mcp-client)
(require 'pai-mcp-direct)
(require 'pai-mcp-search)

;;;; Rendering helpers for search/describe/list

(defun pai-mcp--schema-summary (schema)
  "Return a short parameter summary string for a JSON SCHEMA plist."
  (let ((props (plist-get schema :properties))
        (required (plist-get schema :required))
        lines)
    (while props
      (let* ((pname (substring (symbol-name (car props)) 1))
             (pdef (cadr props))
             (type (or (plist-get pdef :type) "any"))
             (req (if (member pname required) " (required)" ""))
             (desc (plist-get pdef :description)))
        (push (format "    %s (%s)%s%s" pname type req
                      (if desc (concat " - " desc) ""))
              lines))
      (setq props (cddr props)))
    (string-join (nreverse lines) "\n")))

(defun pai-mcp--describe-tool (server tool)
  "Return a describe string for TOOL (plist) owned by SERVER."
  (format "%s  [%s]\n  %s\n\n  Parameters:\n%s"
          (plist-get tool :name) server
          (or (plist-get tool :description) "")
          (let ((s (pai-mcp--schema-summary (plist-get tool :inputSchema))))
            (if (string-empty-p s) "    (none)" s))))

(defun pai-mcp--search (keyword &optional dir)
  "Return a search-result string for KEYWORD over cached tool metadata."
  (let* ((kw (downcase (string-trim (or keyword ""))))
         (matches
          (seq-filter
           (lambda (pair)
             (let ((tool (cdr pair)))
               (or (string-empty-p kw)
                   (string-match-p (regexp-quote kw)
                                   (downcase (concat (or (plist-get tool :name) "") " "
                                                     (or (plist-get tool :description) "")))))))
           (pai-mcp--all-tools dir))))
    (if (null matches)
        (if (pai-mcp--all-tools dir)
            (format "No MCP tools match \"%s\"." keyword)
          "No MCP tool metadata cached yet. Run mcp({\"action\":\"refresh\"}) or call a tool to populate it.")
      (string-join
       (mapcar (lambda (pair)
                 (let ((tool (cdr pair)))
                   (format "%s  [%s]\n  %s" (plist-get tool :name) (car pair)
                           (or (plist-get tool :description) ""))))
               matches)
       "\n\n"))))

(defun pai-mcp--status-text (&optional dir)
  "Return a status line per configured server."
  (let ((config (pai-mcp-load-config dir)))
    (if (null config)
        "No MCP servers configured. Add .mcp.json or ~/.config/mcp/mcp.json."
      (string-join
       (mapcar (lambda (pair)
                 (let* ((name (car pair)) (def (cdr pair))
                        (status (if (pai-mcp--disabled-p def) "disabled"
                                  (symbol-name (pai-mcp-server-status name))))
                        (n (length (pai-mcp--server-tools name)))
                        (kind (cond ((plist-get def :command) "stdio")
                                    ((plist-get def :url) "http")
                                    ((plist-get def :socket) "socket (unsupported)")
                                    (t "?"))))
                   (format "%-20s %-10s %-18s %d tool(s)" name status kind n)))
               config)
       "\n"))))

;;;; The proxy tool

(defconst pai-mcp--call-control-keys
  '(:action :search :describe :instructions :tool :server :regex
    :limit :offset :includeSchemas :args :arguments)
  "Proxy control keys that are never part of a called tool's arguments.")

(defun pai-mcp--decode-object (value)
  "Return VALUE as a decoded object plist, parsing a JSON string when needed."
  (if (stringp value) (ignore-errors (pai-json-decode value)) value))

(defun pai-mcp--object-nonempty-p (value)
  "Return non-nil when VALUE is a non-empty decoded JSON object (plist)."
  (and (consp value) (keywordp (car value))))

(defun pai-mcp--inline-args (args tool-key-p)
  "Collect ARGS's non-control keys as inline tool arguments.
When TOOL-KEY-P is nil, :name doubled as the tool-name alias and is excluded."
  (let ((control (if tool-key-p pai-mcp--call-control-keys
                   (cons :name pai-mcp--call-control-keys)))
        out)
    (while args
      (unless (memq (car args) control)
        (setq out (append out (list (car args) (cadr args)))))
      (setq args (cddr args)))
    out))

(defun pai-mcp--call-arguments (args)
  "Resolve the arguments object for a call from proxy ARGS.
Prefer the nested :args (or MCP-native :arguments) object; otherwise treat any
inlined non-control keys as the tool arguments, so a model that puts parameters
at the top level still calls the tool correctly."
  (let ((explicit (pai-mcp--decode-object
                   (or (plist-get args :args) (plist-get args :arguments)))))
    (if (pai-mcp--object-nonempty-p explicit)
        explicit
      (pai-mcp--inline-args args (and (plist-get args :tool) t)))))

(defun pai-mcp--tool-execute (args ctx _on-update on-done)
  "Execute the `mcp' proxy tool.  All actions are non-blocking."
  (let* ((dir (pai-tool-ctx-cwd ctx))
         (action (or (plist-get args :action)
                     (cond ((plist-get args :tool) "call")
                           ((plist-member args :search) "search")
                           ((plist-get args :describe) "describe")
                           ((plist-get args :instructions) "instructions")
                           (t "list")))))
    (pcase action
      ("list"
       (funcall on-done (pai-tool-ok-result (pai-mcp--status-text dir))))
      ("search"
       (let* ((kw (plist-get args :search))
              (added (pai-mcp-activate-search-tools kw dir))
              (res (pai-mcp-search kw
                                   :regex (eq (plist-get args :regex) t)
                                   :limit (or (plist-get args :limit) pai-mcp-search-limit)
                                   :offset (or (plist-get args :offset) 0)
                                   :include-schemas (eq (plist-get args :includeSchemas) t)
                                   :dir dir))
              (text (plist-get res :text)))
         (funcall on-done
                  (pai-tool-ok-result
                   (if added (format "%s\n\nActivated direct tools: %s"
                                     text (string-join added ", "))
                     text)
                   (append (list :count (plist-get res :count))
                           (when (plist-get res :next-offset)
                             (list :nextOffset (plist-get res :next-offset)))
                           (when added (list :addedToolNames added)))))))
      ("instructions"
       (let ((text (pai-mcp--server-instructions (plist-get args :instructions))))
         (funcall on-done (pai-tool-ok-result (or text "(no instructions)")))))
      ("describe"
       (let* ((tname (plist-get args :describe))
              (server (pai-mcp--find-server-for-tool tname dir))
              (tool (and server (seq-find (lambda (tl) (equal (plist-get tl :name) tname))
                                          (pai-mcp--server-tools server)))))
         (funcall on-done
                  (if tool (pai-tool-ok-result (pai-mcp--describe-tool server tool))
                    (pai-tool-error-result
                     (format "Unknown MCP tool \"%s\". Try mcp({\"action\":\"search\"})." tname))))))
      ("refresh"
       (pai-mcp-refresh
        (lambda (summary)
          (pai-mcp-register-direct-tools dir)
          (funcall on-done (pai-tool-ok-result summary)))
        dir))
      ("connect"
       (let ((server (plist-get args :server)))
         (cond
          ((null server) (funcall on-done (pai-tool-error-result "connect requires :server")))
          ((eq (pai-mcp-server-status server) 'ready)
           (pai-mcp-refresh-catalog
            server
            (lambda (added error)
              (funcall on-done
                       (if error (pai-tool-error-result (format "connect %s failed: %s" server error))
                         (pai-tool-ok-result
                          (format "%s refreshed: %d tool(s)%s" server
                                  (length (pai-mcp--server-tools server))
                                  (if added (format "; added: %s" (string-join added ", ")) ""))))))))
          (t (pai-mcp-ensure
              server
              (lambda () (funcall on-done (pai-tool-ok-result
                                           (format "%s connected: %d tool(s)" server
                                                   (length (pai-mcp--server-tools server))))))
              (lambda (err) (funcall on-done (pai-tool-error-result
                                              (format "connect %s failed: %s" server err))))
              dir)))))
      ((or "call" "tool")
       (let* ((tname (or (plist-get args :tool) (plist-get args :name)))
              (server (or (plist-get args :server)
                          (pai-mcp--find-server-for-tool tname dir)))
              (targs (pai-mcp--call-arguments args)))
         (cond
          ((null tname) (funcall on-done (pai-tool-error-result "mcp call requires :tool")))
          ((null server)
           (pai-mcp-refresh
            (lambda (_summary)
              (let ((server (pai-mcp--find-server-for-tool tname dir)))
                (if server (pai-mcp-call server tname targs on-done dir)
                  (funcall on-done (pai-tool-error-result
                                    (format "No MCP server exposes tool \"%s\"." tname))))))
            dir))
          (t (pai-mcp-call server tname targs on-done dir)))))
      ("enable" (pai-mcp--set-disabled (plist-get args :server) nil dir on-done))
      ("disable" (pai-mcp--set-disabled (plist-get args :server) t dir on-done))
      ("stop"
       (pai-mcp-stop (plist-get args :server))
       (funcall on-done (pai-tool-ok-result (format "Stopped %s" (plist-get args :server)))))
      (_ (funcall on-done (pai-tool-error-result (format "Unknown mcp action: %s" action)))))))

(defun pai-mcp--set-disabled (name disabled dir on-done)
  "Persist the `disabled' flag for server NAME to project .pi/mcp.json."
  (if (or (null name) (string-empty-p name))
      (funcall on-done (pai-tool-error-result "enable/disable requires :server"))
    (let* ((file (expand-file-name ".pi/mcp.json" dir))
           (json (or (pai-mcp--read-json-file file) '()))
           (servers (plist-get json :mcpServers))
           (key (intern (concat ":" name)))
           (entry (or (plist-get servers key) '())))
      (setq entry (plist-put entry :disabled (if disabled t :false)))
      (setq servers (plist-put (or servers '()) key entry))
      (setq json (plist-put json :mcpServers servers))
      (condition-case err
          (progn
            (make-directory (file-name-directory file) t)
            (with-temp-file file (insert (pai-json-encode json)))
            (when disabled (pai-mcp-stop name))
            (funcall on-done (pai-tool-ok-result
                              (format "%s %s (run /reload if tools changed)"
                                      (if disabled "Disabled" "Enabled") name))))
        (error (funcall on-done (pai-tool-error-result
                                 (format "cannot write %s: %s" file
                                         (error-message-string err)))))))))

(pai-register-tool
 (list :name "mcp"
       :label "MCP"
       :description
       "Access MCP (Model Context Protocol) servers without registering every tool. Actions: list (configured servers), search {search: keyword} (find tools), describe {describe: tool_name} (show schema), call {tool: name, args: {...}} (invoke; the owning server starts automatically), connect {server}, instructions {instructions: server}, refresh (probe servers to cache metadata), enable/disable/stop {server}. Servers start lazily on first use and never block the session."
       :prompt-snippet "mcp: search and call MCP server tools through one proxy"
       :execution-mode 'parallel
       :parameters
       (pai-object-schema
        (list :action (pai-string-schema "list | search | describe | call | connect | instructions | refresh | enable | disable | stop.")
              :search (pai-string-schema "Keyword to search tool names/descriptions.")
              :describe (pai-string-schema "Tool name to show the full schema for.")
              :instructions (pai-string-schema "Server name to show usage instructions for.")
              :tool (pai-string-schema "Tool name to call.")
              :server (pai-string-schema "Server name (for call/connect/enable/disable/stop; optional for call).")
              :regex (pai-boolean-schema "Treat the search keyword as a regular expression.")
              :limit (pai-number-schema "Max search results per page.")
              :offset (pai-number-schema "Result offset for pagination.")
              :includeSchemas (pai-boolean-schema "Include a compact parameter shape in search results.")
              :args (append
                     (pai-object-schema nil)
                     (list :description
                           "Arguments for the selected tool, as an object matching its input schema. For call, include all required tool parameters here; e.g. greet uses args: {\"name\": \"Alice\"}. Use describe to discover required parameters."
                           :additionalProperties t)))
        nil)
       :execute #'pai-mcp--tool-execute))

;;;; Slash command

(defun pai-mcp-command (args _ctx)
  "Handler for `/mcp': status, or `/mcp <verb> [server]'."
  (let* ((parts (split-string (string-trim args) "[ \t]+" t))
         (verb (car parts))
         (server (cadr parts)))
    (pcase verb
      ((or `nil "" "list" "status")
       (list :message (pai-mcp--status-text default-directory)))
      ("refresh"
       (pai-mcp-refresh
        (lambda (summary) (pai--render-note (format "MCP refresh:\n%s" summary)))
        default-directory)
       (list :message "Refreshing MCP servers…"))
      ((or "reconnect" "connect")
       (if server
           (progn (pai-mcp-stop server)
                  (pai-mcp-ensure server
                                  (lambda () (pai--render-note (format "%s reconnected" server)))
                                  (lambda (err) (pai--render-note (format "%s: %s" server err)))
                                  default-directory)
                  (list :message (format "Reconnecting %s…" server)))
         (list :message "Usage: /mcp reconnect <server>")))
      ("stop" (pai-mcp-stop server) (list :message (format "Stopped %s" server)))
      ("enable" (pai-mcp--set-disabled server nil default-directory
                                       (lambda (r) (pai--render-note (pai-content-text (plist-get r :content)))))
       (list :message (format "Enabling %s…" server)))
      ("disable" (pai-mcp--set-disabled server t default-directory
                                        (lambda (r) (pai--render-note (pai-content-text (plist-get r :content)))))
       (list :message (format "Disabling %s…" server)))
      (_ (list :message "Usage: /mcp [list|refresh|reconnect SERVER|enable SERVER|disable SERVER|stop SERVER]")))))

;;;; Extension entry point

(pai-register-extension
 (lambda (api)
   (pai-ext-register-command api "mcp"
                             :description "MCP servers: status/refresh/reconnect/enable/disable/stop"
                             :handler #'pai-mcp-command
                             :arg-completions
                             (pai-command-completion-tree
                              (let ((servers (lambda ()
                                               (ignore-errors
                                                 (mapcar #'car (pai-mcp-load-config default-directory))))))
                                `("list" "refresh"
                                  ,@(mapcar (lambda (v) (cons v servers))
                                            '("reconnect" "stop" "enable" "disable"))))))
   (pai-mcp--load-metadata)
   ;; Register direct tools from cache (no connection needed).
   (pai-mcp-register-direct-tools default-directory)
   ;; Connect eager/keep-alive servers at load without blocking the UI.
   (pai-mcp-startup-connect default-directory))
 "mcp")

(provide 'pai-mcp)
;;; pai-mcp.el ends here
