;;; pai-mcp-surface.el --- MCP panel, trace, and native scripts -*- lexical-binding: t; -*-

;;; Commentary:
;; `/mcp' opens a non-connecting, button-driven server panel.  Setup always
;; previews the complete proposed file before writing it and never launches a
;; discovered command.  Protocol tracing persists only allowlisted metadata.
;;
;; mcpScript is native Emacs Lisp, NOT JavaScript.  Its code must evaluate to
;; (lambda (call search describe emit done) ...).  CALL takes PATH ARGS CALLBACK,
;; SEARCH takes a query plist and CALLBACK, DESCRIBE takes PATH CALLBACK.
;; Responses are (:ok t :data VALUE) or (:ok :false :error (:code ... :message ...)).
;; EMIT adds output; DONE finishes with a value.  Ordinary lexical closures,
;; loops and counters compose sequential or concurrent calls.  Example:
;; (lambda (call search describe emit done)
;;   (funcall call "docs_lookup" '(:query "Emacs")
;;            (lambda (result) (funcall done (plist-get result :data)))))
;; Scripts run in a separate `emacs -Q --batch' process, with a hard deadline,
;; so even infinite Lisp loops cannot block the UI.  This is trusted code, not
;; a security sandbox: native Lisp can access the filesystem and subprocesses.
;; Only the supplied CALL function is routed through MCP approval and lifecycle.
;; Successful intermediate transfers share a 16 MiB UTF-8 JSON budget.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'button)
(require 'pai-tools)
(require 'pai-mcp-client)
(require 'pai-mcp-direct)
(require 'pai-mcp-search)
(require 'pai-mcp-guard)

(declare-function pai-mcp-auth-login "pai-mcp-auth" (name &optional done error dir))
(declare-function pai-mcp-auth-store-bearer "pai-mcp-auth" (name token &optional dir))
(declare-function pai-mcp-auth-bearer-status "pai-mcp-auth" (name &optional dir))
(declare-function pai-mcp-auth-remove-bearer "pai-mcp-auth" (name &optional dir))
(declare-function pai-mcp-discover-host-configs "pai-mcp-sources" (&optional dir))
(declare-function pai-mcp-auth-logout "pai-mcp-auth" (name &optional dir))
(declare-function pai-mcp-call-raw "pai-mcp-client" (server tool args done &optional dir))
(declare-function pai-mcp--set-disabled "pai-mcp" (name disabled dir done))

(defvar-local pai-mcp-surface--directory nil)
(defvar-local pai-mcp-surface--notice "")

(defun pai-mcp-surface--notify (buffer text)
  "Display TEXT without selecting BUFFER or entering the minibuffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq pai-mcp-surface--notice text)
      (when (derived-mode-p 'pai-mcp-panel-mode)
        (pai-mcp-panel-refresh))))
  (message "MCP: %s" text))

(defun pai-mcp-surface--button (label action)
  "Return a tabulated-list LABEL button invoking ACTION."
  (list label 'action (lambda (_button) (funcall action)) 'follow-link t))

(defun pai-mcp-surface--action (action name dir buffer)
  "Perform explicit user ACTION on NAME in DIR; update BUFFER asynchronously."
  (let ((done (lambda (&optional value)
                (pai-mcp-surface--notify
                 buffer (if (stringp value) value (format "%s: %s complete" name action)))))
        (fail (lambda (error) (pai-mcp-surface--notify buffer (format "%s: %s" name error)))))
    (condition-case error
        (pcase action
          ('reconnect
           (pai-mcp-stop name)
           (pai-mcp-ensure name done fail dir))
          ('stop (pai-mcp-stop name) (funcall done))
          ('login (require 'pai-mcp-auth) (pai-mcp-auth-login name done fail dir))
          ('logout (require 'pai-mcp-auth) (pai-mcp-auth-logout name dir)
                   (pai-mcp-stop name) (funcall done))
          ((or 'enable 'disable)
           (pai-mcp--set-disabled
            name (eq action 'disable) dir
            (lambda (result)
              (funcall done (pai-content-text (plist-get result :content)))))))
      (error (funcall fail (error-message-string error))))))

(defun pai-mcp-panel-refresh ()
  "Refresh cached status in this panel without connecting servers."
  (interactive)
  (let ((dir pai-mcp-surface--directory)
        (buffer (current-buffer)))
    (setq tabulated-list-entries
          (mapcar
           (lambda (pair)
             (let* ((name (car pair)) (def (cdr pair))
                    (disabled (pai-mcp--disabled-p def)))
               (list name
                     (vector
                      name (if disabled "disabled" (symbol-name (pai-mcp-server-status name)))
                      (number-to-string (length (pai-mcp--server-tools name)))
                      (pai-mcp-surface--button "Reconnect" (lambda () (pai-mcp-surface--action 'reconnect name dir buffer)))
                      (pai-mcp-surface--button "Login" (lambda () (pai-mcp-surface--action 'login name dir buffer)))
                      (pai-mcp-surface--button "Logout" (lambda () (pai-mcp-surface--action 'logout name dir buffer)))
                      (pai-mcp-surface--button "Stop" (lambda () (pai-mcp-surface--action 'stop name dir buffer)))
                      (pai-mcp-surface--button (if disabled "Enable" "Disable")
                                               (lambda () (pai-mcp-surface--action
                                                           (if disabled 'enable 'disable) name dir buffer)))))))
           (pai-mcp-load-config dir)))
    (setq header-line-format
          (concat "MCP: g refresh cached status; s setup; i import; q quit.  "
                  ;; a literal `%' would start a mode-line directive
                  (replace-regexp-in-string "%" "%%" (or pai-mcp-surface--notice "") t t)))
    (tabulated-list-print t)))

(define-derived-mode pai-mcp-panel-mode tabulated-list-mode "MCP"
  "Cached MCP server panel.  Buttons initiate asynchronous operations."
  (setq tabulated-list-format [("Server" 24 t) ("Status" 12 t) ("Tools" 7 t)
                               ("Connection" 12 nil) ("Auth" 8 nil) ("Credentials" 9 nil)
                               ("Process" 7 nil) ("Enabled" 8 nil)])
  (setq tabulated-list-padding 2)
  (setq revert-buffer-function (lambda (&rest _) (pai-mcp-panel-refresh)))
  (tabulated-list-init-header))

(define-key pai-mcp-panel-mode-map (kbd "g") #'pai-mcp-panel-refresh)
(define-key pai-mcp-panel-mode-map (kbd "s") #'pai-mcp-setup)
(define-key pai-mcp-panel-mode-map (kbd "i") #'pai-mcp-init)

(defun pai-mcp-panel (&optional dir)
  "Open the MCP panel for DIR without starting servers or host discovery."
  (interactive)
  (let ((dir (or dir default-directory))
        (buffer (get-buffer-create "*pai MCP*")))
    (with-current-buffer buffer
      (pai-mcp-panel-mode)
      (setq default-directory dir pai-mcp-surface--directory dir)
      (pai-mcp-panel-refresh))
    (pop-to-buffer buffer)
    buffer))

(defun pai-mcp-surface--read-config (file)
  "Read FILE for a write proposal, refusing to overwrite malformed JSON."
  (if (file-exists-p file)
      (or (pai-mcp--read-json-file file) (user-error "Cannot parse %s; repair it first" file))
    (list :mcpServers (pai-json-empty-object))))

(defun pai-mcp-surface--preview (file value)
  "Preview VALUE for FILE, requiring an explicit Apply button before writing."
  (let* ((buffer (generate-new-buffer "*MCP setup preview*"))
         (before (and (file-exists-p file)
                      (with-temp-buffer (insert-file-contents-literally file) (buffer-string))))
         (text (concat (pai-json-encode value) "\n")))
    (with-current-buffer buffer
      (insert (format "Proposed write: %s\n\nNo servers will be started.\n\n%s\n" file text))
      (insert-text-button
       "Apply" 'follow-link t
       'action
       (lambda (_)
         (let ((current (and (file-exists-p file)
                             (with-temp-buffer (insert-file-contents-literally file) (buffer-string)))))
           (unless (equal before current)
             (user-error "Config changed since preview; create a new proposal"))
           (make-directory (file-name-directory file) t)
           (with-temp-file file (insert text))
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (erase-buffer) (insert (format "Wrote %s\nUse /mcp to inspect; servers remain lazy.\n" file)))))))
      (insert "   ")
      (insert-text-button "Cancel" 'follow-link t 'action (lambda (_) (kill-buffer buffer)))
      (special-mode))
    (pop-to-buffer buffer)
    buffer))

(defconst pai-mcp-surface--presets
  '(("deepwiki" :url "https://mcp.deepwiki.com/mcp" :protocolVersion "auto")
    ("context7" :url "https://mcp.context7.com/mcp" :protocolVersion "auto")
    ("parallel-search" :url "https://search.parallel.ai/mcp" :protocolVersion "auto" :directTools t)
    ("notion" :url "https://mcp.notion.com/mcp" :auth "oauth" :protocolVersion "auto")
    ("github" :url "https://api.githubcopilot.com/mcp" :auth "oauth" :protocolVersion "auto")
    ("chrome-devtools" :command "npx" :args ["-y" "chrome-devtools-mcp@1.6.0"]))
  "Explicitly selected upstream setup presets; selecting only previews a write.")

(defun pai-mcp-surface--inspect (dir)
  "Show effective configuration sources and explicit host discovery for DIR."
  (require 'pai-mcp-sources)
  (let ((buffer (get-buffer-create "*MCP config discovery*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "MCP sources (later sources win)\n\n")
        (dolist (file (pai-mcp--config-files dir))
          (insert-text-button file 'follow-link t 'action (lambda (_) (find-file-other-window file)))
          (insert (if (file-exists-p file) "\n" " (absent)\n")))
        (insert "\nHost configurations (discovery does not activate imports)\n\n")
        (dolist (source (pai-mcp-discover-host-configs dir))
          (insert (format "%s: %s (%d servers)\n" (plist-get source :kind)
                          (plist-get source :path) (plist-get source :serverCount))))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun pai-mcp-setup (&optional dir)
  "Interactively scaffold or add a server, with an exact write preview.
Only shared project/global configurations are normal setup targets.  Commands
are supplied explicitly by the user; discovery never starts a process."
  (interactive)
  (let* ((dir (or dir pai-mcp-surface--directory default-directory))
         (choice (completing-read "MCP setup: " '("Scaffold" "Add known server" "Quick-add RepoPrompt" "Add HTTP server" "Add stdio server" "Import host configs" "Inspect sources") nil t)))
    (cond
     ((equal choice "Import host configs") (pai-mcp-init dir))
     ((equal choice "Inspect sources") (pai-mcp-surface--inspect dir))
     (t
      (let* ((scope (completing-read "Write target: " '("Project .mcp.json" "Global ~/.config/mcp/mcp.json") nil t))
             (file (if (string-prefix-p "Project" scope) (expand-file-name ".mcp.json" dir)
                     (expand-file-name "~/.config/mcp/mcp.json")))
             (json (pai-mcp-surface--read-config file)))
        (unless (equal choice "Scaffold")
          (let* ((preset (when (equal choice "Add known server")
                           (assoc (completing-read "Known server: " pai-mcp-surface--presets nil t)
                                  pai-mcp-surface--presets)))
                 (name (cond (preset (car preset))
                             ((equal choice "Quick-add RepoPrompt") "repoprompt")
                             (t (string-trim (read-string "Server name: ")))))
                 (key (intern (concat ":" name)))
                 (servers (plist-get json :mcpServers))
                 (def (cond
                       (preset (copy-sequence (cdr preset)))
                       ((equal choice "Quick-add RepoPrompt")
                        (let ((path (seq-find #'file-executable-p
                                              (list (expand-file-name "~/RepoPrompt/repoprompt_cli")
                                                    "/Applications/Repo Prompt.app/Contents/MacOS/repoprompt-mcp"))))
                          (unless path (user-error "RepoPrompt executable not found; use Add stdio server for a custom installation"))
                          (list :command path :args [] :lifecycle "lazy")))
                       ((equal choice "Add HTTP server")
                        (let ((url (read-string "HTTP(S) MCP URL: ")))
                          (unless (string-match-p "\\`https?://" url) (user-error "Expected an HTTP(S) URL"))
                          (list :url url)))
                       (t
                        (let ((command (read-string "Executable (no shell): "))
                              (args (read-string "Arguments (shell-style quoting, no shell execution): ")))
                          (when (string-empty-p command) (user-error "Executable is required"))
                          (list :command command :args (vconcat (split-string-and-unquote args))))))))
            (when (string-empty-p name) (user-error "Server name is required"))
            (when (and (not (hash-table-p servers)) (plist-get servers key)
                       (not (yes-or-no-p (format "Replace %s in this file? " name))))
              (user-error "Cancelled"))
            (when (hash-table-p servers) (setq servers nil))
            (setq json (plist-put json :mcpServers (plist-put servers key def)))))
        (pai-mcp-surface--preview file json))))))

(defun pai-mcp-init (&optional dir discover-host-configs)
  "Select detected host imports and preview a Pi-owned config update.
DIR defaults to the panel/project directory.  With prefix argument
DISCOVER-HOST-CONFIGS, also opt into future host fallback discovery.  No
external host files are modified and credentials are never copied."
  (interactive (list nil current-prefix-arg))
  (require 'pai-mcp-sources)
  (let* ((dir (or dir pai-mcp-surface--directory default-directory))
         (found (pai-mcp-discover-host-configs dir))
         (labels (mapcar (lambda (source)
                           (cons (format "%s: %s (%d servers)" (plist-get source :kind)
                                         (plist-get source :path) (plist-get source :serverCount))
                                 (plist-get source :kind))) found))
         (selected (and labels (completing-read-multiple "Import host configs (comma separated): " labels nil t)))
         (global (equal (completing-read "Import scope: " '("Global Pi override" "Project Pi override") nil t)
                        "Global Pi override"))
         (file (if global (expand-file-name "mcp.json" pai-directory) (expand-file-name ".pi/mcp.json" dir)))
         (json (pai-mcp-surface--read-config file))
         (imports (append (plist-get json :imports) nil)))
    (dolist (label selected)
      (let ((kind (cdr (assoc label labels))))
        (unless (member kind imports) (setq imports (append imports (list kind))))))
    (when imports (setq json (plist-put json :imports (vconcat imports))))
    (when discover-host-configs
      (setq json (plist-put json :settings
                            (plist-put (plist-get json :settings) :hostConfigDiscovery "on"))))
    (pai-mcp-surface--preview file json)))

(defun pai-mcp-token (action name &optional dir)
  "Interactively ACTION (set/status/remove) bearer credentials for NAME.
Tokens are read only using `read-passwd', never from command arguments."
  (interactive
   (list (completing-read "Token action: " '("set" "status" "remove") nil t)
         (completing-read "MCP server: " (mapcar #'car (pai-mcp-load-config)) nil t)))
  (require 'pai-mcp-auth)
  (let ((dir (or dir pai-mcp-surface--directory default-directory)))
    (pcase action
      ("set" (let ((token (read-passwd "Bearer token (hidden): ")))
               (unwind-protect (pai-mcp-auth-store-bearer name token dir)
                 (clear-string token)))
       (message "MCP bearer token stored for %s" name))
      ("status" (message "MCP bearer token for %s: %s" name (pai-mcp-auth-bearer-status name dir)))
      ("remove" (pai-mcp-auth-remove-bearer name dir) (message "MCP bearer token removed for %s" name))
      (_ (user-error "Token action must be set, status or remove")))))

(defun pai-mcp-surface-command (args ctx)
  "Handle `/mcp' ARGS in CTX, returning immediately for server operations."
  (let* ((parts (split-string-and-unquote (string-trim args)))
         (verb (car parts)) (name (cadr parts))
         (dir (pai-tool-ctx-cwd ctx)) (buffer (current-buffer)))
    (condition-case error
        (pcase verb
          ((or `nil "" "list" "status") (pai-mcp-panel dir) (list :message "Opened MCP server panel."))
          ("setup" (pai-mcp-setup dir) (list :message "MCP setup preview opened."))
          ("init" (pai-mcp-init dir (member "--discover-host-configs" (cdr parts)))
           (list :message "MCP imports preview opened; Apply writes the proposal."))
          ("token"
           (unless (and (= (length parts) 3) (member name '("set" "status" "remove")))
             (user-error "Usage: /mcp token set|status|remove SERVER; never pass a token as an argument"))
           (pai-mcp-token name (nth 2 parts) dir) (list :message "MCP token command complete."))
          ("refresh"
           (pai-mcp-refresh (lambda (summary) (pai-mcp-surface--notify buffer summary)) dir)
           (list :message "Refreshing MCP servers asynchronously."))
          ((or "reconnect" "connect" "login" "auth" "logout" "stop" "enable" "disable")
           (unless (and name (= (length parts) 2)) (user-error "Usage: /mcp %s SERVER" verb))
           (unless (pai-mcp--server-def name dir) (user-error "Unknown MCP server: %s" name))
           (pai-mcp-surface--action (pcase verb ((or "connect" "reconnect") 'reconnect)
                                     ((or "login" "auth") 'login) (_ (intern verb))) name dir buffer)
           (list :message (format "MCP %s requested for %s." verb name)))
          (_ (list :message "Usage: /mcp [setup|init|refresh|reconnect|login|logout|stop|enable|disable SERVER|token set/status/remove SERVER]")))
      (error (list :message (error-message-string error))))))

;;;; Metadata-only JSONL tracing

(defvar pai-mcp-trace--writers (make-hash-table :test 'equal))
(defvar pai-mcp-trace--session (format-time-string "%Y%m%dT%H%M%S"))

(defun pai-mcp-trace--text (text &optional limit)
  "Redact secrets and URLs from metadata TEXT before persistence."
  (let ((case-fold-search t) (text (format "%s" text)))
    (if (string-match-p "\\b\\(?:token\\|secret\\|password\\|passwd\\|api[_-]?key\\|authorization\\|cookie\\)\\b" text)
        "[REDACTED]"
      (setq text (replace-regexp-in-string "[a-z][a-z0-9+.-]*://[^[:space:]\"'<>]+" "[REDACTED_URL]" text))
      (setq text (replace-regexp-in-string "\\b\\(?:bearer\\|basic\\)[[:space:]]+[A-Za-z0-9._~+/=-]+" "[REDACTED_AUTH]" text))
      (setq text (replace-regexp-in-string "[[:cntrl:]]" "?" text))
      (truncate-string-to-width text (or limit 120) nil nil "..."))))

(defun pai-mcp-trace--event (server direction message)
  "Build an allowlisted trace event, never persisting MESSAGE payloads."
  (let* ((def (plist-get server :def))
         (method (plist-get message :method))
         (id (plist-get message :id))
         (event (list :version 1 :timestamp (format-time-string "%FT%T.%3NZ" nil t)
                      :direction (if (member direction '(outbound "outbound")) "outbound" "inbound")
                      :server (pai-mcp-trace--text (plist-get server :name))
                      :transport (cond ((plist-get def :url) "streamable-http")
                                       ((plist-get def :socketPath) "unix-socket") (t "stdio"))
                      :kind (if method (if (plist-member message :id) "request" "notification") "response")
                      :status (if (plist-get message :error) "error"
                                (if (member direction '(outbound "outbound")) "sent" "received")))))
    (when (stringp method) (setq event (plist-put event :method (pai-mcp-trace--text method))))
    (when (plist-member message :id)
      (setq event (plist-put event :id (cond ((numberp id) id) ((stringp id) "[REDACTED_ID]") (t :null)))))
    (when (numberp (plist-get (plist-get message :error) :code))
      (setq event (plist-put event :errorCode (plist-get (plist-get message :error) :code))))
    event))

(defun pai-mcp-surface--emacs-command (form)
  "Return an isolated Emacs command evaluating FORM."
  (list (expand-file-name invocation-name invocation-directory) "-Q" "--batch" "--eval" (prin1-to-string form)))

(defun pai-mcp-trace--writer (file)
  "Start a pipe writer that performs FILE I/O outside the UI process."
  (let ((form `(progn
                 (make-directory ,(file-name-directory file) t)
                 (let ((coding-system-for-write 'utf-8-unix))
                   (with-temp-file ,file)
                   (set-file-modes ,file #o600)
                   (condition-case nil
                       (while t
                         (let ((line (read-from-minibuffer "")))
                           (write-region (concat line "\n") nil ,file t 'silent)))
                     (end-of-file nil))))))
    (make-process :name "pai-mcp-trace" :command (pai-mcp-surface--emacs-command form)
                  :connection-type 'pipe :coding 'utf-8-unix :noquery t
                  :filter #'ignore :sentinel #'ignore)))

(defun pai-mcp-trace (server direction message)
  "Queue redacted protocol metadata for SERVER, DIRECTION and MESSAGE.
Tracing is opt-in (:trace t on a server or settings.trace.enabled).  Raw
payloads, URLs, string IDs and error messages never enter the writer process.
Trace failures never affect transport.  Caps are per destination per session."
  (condition-case nil
      (let* ((server (if (stringp server) (pai-mcp--server server) server))
             (def (plist-get server :def))
             (dir (or (plist-get server :dir) (plist-get def :cwd) default-directory))
             (settings (plist-get (pai-mcp-settings dir) :trace))
             (enabled (if (plist-member def :trace) (eq (plist-get def :trace) t)
                        (and (listp settings) (eq (plist-get settings :enabled) t)))))
        (when enabled
          (let* ((file (expand-file-name (or (plist-get settings :file)
                                              (format ".pi/mcp-traces/mcp-%s-%d.jsonl" pai-mcp-trace--session (emacs-pid))) dir))
                 (state (or (gethash file pai-mcp-trace--writers)
                            (let ((s (list :process (pai-mcp-trace--writer file) :bytes 0 :events 0 :disabled nil)))
                              (puthash file s pai-mcp-trace--writers) s)))
                 (max-bytes (plist-get settings :maxBytes)) (max-events (plist-get settings :maxEvents)))
            (unless (and (numberp max-bytes) (> max-bytes 0)) (setq max-bytes 262144))
            (unless (and (numberp max-events) (> max-events 0)) (setq max-events 10000))
            (when (and (not (plist-get state :disabled)) (< (plist-get state :events) max-events))
              (let* ((line (concat (pai-json-encode (pai-mcp-trace--event server direction message)) "\n"))
                     (bytes (string-bytes line))
                     (process (plist-get state :process)))
                (if (or (> (+ bytes (plist-get state :bytes)) max-bytes) (not (process-live-p process)))
                    (plist-put state :disabled t)
                  (plist-put state :bytes (+ bytes (plist-get state :bytes)))
                  (plist-put state :events (1+ (plist-get state :events)))
                  (process-send-string process line)))))))
    (error nil)))

(defun pai-mcp-trace-close ()
  "Close trace writer input; queued metadata finishes asynchronously."
  (maphash (lambda (_ state)
             (let ((process (plist-get state :process)))
               (when (process-live-p process) (process-send-eof process))))
           pai-mcp-trace--writers)
  (clrhash pai-mcp-trace--writers))

;;;; Native asynchronous scripting

(defun pai-mcp-script--worker ()
  "Run the isolated script worker.  Only called by a child batch Emacs."
  (require 'json)
  (require 'cl-lib)
  (let ((next-id 0) (pending (make-hash-table :test 'eql)) (finished nil))
    (cl-labels
        ((send (object) (princ (concat (pai-json-encode object) "\n")))
         (request (op input callback)
           (let ((id (cl-incf next-id)))
             (puthash id callback pending)
             (send (list :type op :id id :input input))))
         (finish (value) (unless finished (setq finished t) (send (list :type "done" :value value)))))
      (condition-case error
          (let* ((init (json-parse-string (read-from-minibuffer "") :object-type 'plist :array-type 'list :null-object :null :false-object :false))
                 (code (plist-get init :code))
                 (parsed (read-from-string code))
                 (function (eval (car parsed) t)))
            (unless (string-match-p "\\`[[:space:]]*\\'" (substring code (cdr parsed)))
              (error "Expected a single lambda expression"))
            (unless (functionp function) (error "Script must evaluate to a function"))
            (funcall function
                     (lambda (path args callback) (request "call" (list :path path :args args) callback))
                     (lambda (query callback) (request "search" query callback))
                     (lambda (path callback) (request "describe" (list :path path) callback))
                     (lambda (value) (send (list :type "emit" :value value)))
                     #'finish)
            (while (not finished)
              (let* ((reply (json-parse-string (read-from-minibuffer "") :object-type 'plist :array-type 'list :null-object :null :false-object :false))
                     (id (plist-get reply :id)) (callback (gethash id pending)))
                (when callback
                  (remhash id pending)
                  (funcall callback (plist-get reply :result))))))
        (error (send (list :type "error" :message (error-message-string error))))))))

(defun pai-mcp-script--resolve (path dir)
  "Resolve PATH against enabled cached MCP tools; reject ambiguous aliases."
  (let ((settings (pai-mcp-settings dir)) exact aliases)
    (dolist (pair (pai-mcp-load-config dir))
      (unless (pai-mcp--disabled-p (cdr pair))
        (dolist (tool (pai-mcp--server-tools (car pair)))
          (let* ((original (plist-get tool :name))
                 (prefixed (pai-mcp--prefixed-name (car pair) (cdr pair) settings original))
                 (match (list :server (car pair) :tool tool :path prefixed)))
            (when (equal path prefixed) (push match exact))
            (when (equal path original) (push match aliases))))))
    (let ((matches (or exact aliases)))
      (when (= (length matches) 1) (car matches)))))

(defun pai-mcp-script--error (code message)
  "Construct a script error envelope with CODE and MESSAGE."
  (list :ok :false :error (list :code code :message message)))

(defun pai-mcp-script--dispatch (operation input dir callback)
  "Dispatch MCP-only OPERATION with INPUT in DIR to CALLBACK."
  (condition-case error
      (pcase operation
        ("call"
         (let* ((path (plist-get input :path)) (resolved (and (stringp path) (pai-mcp-script--resolve path dir))))
           (if (not resolved)
               (funcall callback (pai-mcp-script--error "tool_not_found" "Unknown or ambiguous MCP tool; use search."))
             (pai-mcp-call-raw
              (plist-get resolved :server) (plist-get (plist-get resolved :tool) :name)
              (plist-get input :args)
              (lambda (result)
                (funcall callback
                         (if (or (eq (plist-get result :isError) t) (eq (plist-get result :is-error) t))
                             (pai-mcp-script--error "tool_error" (pai-content-text (plist-get result :content)))
                           (list :ok t :data result)))) dir))))
        ("describe"
         (let* ((resolved (pai-mcp-script--resolve (plist-get input :path) dir))
                (tool (plist-get resolved :tool)))
           (funcall callback
                    (if tool (list :ok t :data (list :path (plist-get resolved :path)
                                                   :name (plist-get tool :name) :server (plist-get resolved :server)
                                                   :description (or (plist-get tool :description) "")
                                                   :inputSchema (or (plist-get tool :inputSchema) (pai-json-empty-object))
                                                   :outputSchema (or (plist-get tool :outputSchema) :null)))
                      (pai-mcp-script--error "tool_not_found" "Unknown or ambiguous MCP tool.")))))
        ("search"
         (let* ((query (or (plist-get input :query) "")) (server (plist-get input :server))
                (limit (max 1 (min 100 (or (plist-get input :limit) 12))))
                (offset (max 0 (or (plist-get input :offset) 0)))
                (settings (pai-mcp-settings dir)) items)
           (unless (string-empty-p (string-trim query))
             (dolist (entry (let ((pai-mcp-search-include-pai-tools nil)) (pai-mcp--search-entries dir)))
               (let* ((name (plist-get entry :server)) (def (pai-mcp--server-def name dir))
                      (score (and (not (pai-mcp--disabled-p def)) (or (null server) (equal server name))
                                  (pai-mcp--score entry query))))
                 (when score
                   (push (list :path (pai-mcp--prefixed-name name def settings (plist-get entry :name))
                               :name (plist-get entry :name) :server name :description (plist-get entry :description)
                               :score score) items)))))
           (setq items (sort items (lambda (a b) (if (= (plist-get a :score) (plist-get b :score))
                                                    (string-lessp (plist-get a :path) (plist-get b :path))
                                                  (> (plist-get a :score) (plist-get b :score))))))
           (let* ((total (length items)) (end (min total (+ offset limit))))
             (funcall callback (list :ok t :data (list :items (vconcat (seq-subseq items (min offset total) end))
                                                       :total total :hasMore (if (< end total) t :false)
                                                       :nextOffset (if (< end total) end :null)))))))
        (_ (funcall callback (pai-mcp-script--error "invalid_operation" "Only call, search and describe are available."))))
    (error (funcall callback (pai-mcp-script--error "script_error" (error-message-string error))))))

(defvar pai-mcp-script-dispatch-function #'pai-mcp-script--dispatch
  "Function (OP INPUT DIR CALLBACK) routing script operations to MCP only.
CALLBACK receives (:ok t :data RAW-MCP-RESULT) or (:ok :false :error ...).
Successful results must not be presentation-truncated or output-guarded.")

(defun pai-mcp-script-run (code on-done &optional dir timeout-ms on-update)
  "Execute native Lisp CODE asynchronously; call ON-DONE once with a tool result.
Return a cancellation function.  DIR is the working directory.  TIMEOUT-MS
is a hard deadline, default 30000.  ON-UPDATE receives emitted output."
  (let ((dir (or dir default-directory)) (timeout (if (and (numberp timeout-ms) (> timeout-ms 0)) timeout-ms 30000))
        process timer output calls (bytes 0) (finished nil) (partial ""))
    (cl-labels
        ((block (value)
           (cond
            ((and (listp value) (equal (plist-get value :type) "text")) (pai-text (plist-get value :text)))
            ((and (listp value) (equal (plist-get value :type) "image"))
             (list :type 'image :data (plist-get value :data) :mime-type (plist-get value :mimeType)))
            (t (pai-text (if (stringp value) value (pai-json-encode value))))))
         (finish (error-code value)
           (unless finished
             (setq finished t)
             (when timer (cancel-timer timer))
             (when (and process (process-live-p process)) (delete-process process))
             (when value (push (block value) output))
             (funcall on-done (list :content (nreverse output) :is-error (and error-code t)
                                    :details (list :error (or error-code :null) :calls (vconcat (nreverse calls)))))))
         (receive (message)
           (unless finished
             (pcase (plist-get message :type)
               ("emit" (push (block (plist-get message :value)) output)
                (when on-update (funcall on-update (list :content (reverse output)))))
               ("done" (finish nil (plist-get message :value)))
               ("error" (finish "script_error" (plist-get message :message)))
               ((or "call" "search" "describe")
                (let* ((operation (plist-get message :type)) (input (plist-get message :input))
                       (id (plist-get message :id)) (start (float-time))
                       (record (list :operation operation :path (or (plist-get input :path) (plist-get input :query) "")
                                     :ok :false :error "incomplete" :durationMs 0)))
                  (push record calls)
                  (funcall pai-mcp-script-dispatch-function
                           operation input dir
                           (lambda (result)
                             (unless finished
                               (condition-case error
                                   (let* ((success (eq (plist-get result :ok) t))
                                          (data-json (and success (pai-json-encode (plist-get result :data))))
                                          (size (if data-json (string-bytes data-json) 0)))
                                     (if (> (+ bytes size) (* 16 1024 1024))
                                         (setq result (pai-mcp-script--error "intermediate_result_too_large" "MCP intermediate transfer exceeds 16 MiB per script."))
                                       (cl-incf bytes size))
                                     (plist-put record :ok (plist-get result :ok))
                                     (plist-put record :error (or (plist-get (plist-get result :error) :code) :null))
                                     (plist-put record :durationMs (* 1000 (- (float-time) start)))
                                     (process-send-string process (concat (pai-json-encode (list :id id :result result)) "\n")))
                                 (error (finish "script_error" (error-message-string error)))))))))))))
      (condition-case error
          (let ((default-directory dir))
            (setq process
                  (make-process
                   :name "pai-mcp-script" :connection-type 'pipe :coding 'utf-8-unix :noquery t
                   :command (pai-mcp-surface--emacs-command
                             `(progn
                                (add-to-list 'load-path ,(file-name-directory (locate-library "pai-core")))
                                (add-to-list 'load-path ,(file-name-directory (locate-library "pai-mcp-surface")))
                                (require 'pai-mcp-surface)
                                (pai-mcp-script--worker)))
                   :filter (lambda (_ chunk)
                             (setq partial (concat partial chunk))
                             (condition-case error
                                 (while (string-match "\n" partial)
                                   (let ((line (substring partial 0 (match-beginning 0))))
                                     (setq partial (substring partial (match-end 0)))
                                     (unless (string-empty-p line) (receive (pai-json-decode line)))))
                               (error (finish "script_error" (error-message-string error)))))
                   :sentinel (lambda (proc _event)
                               (when (memq (process-status proc) '(exit signal failed))
                                 (finish "script_error" "Script worker exited before calling done.")))))
            (setq timer (run-at-time (/ timeout 1000.0) nil (lambda () (finish "timeout" (format "mcpScript timed out after %sms" timeout)))))
            (process-send-string process (concat (pai-json-encode (list :code code)) "\n")))
        (error (finish "script_error" (error-message-string error))))
      (lambda () (finish "aborted" "mcpScript cancelled")))))

(defun pai-mcp-script-execute (args ctx on-update on-done)
  "Tool executor for native Lisp mcpScript, guarding final output only."
  (require 'pai-mcp-guard)
  (let ((dir (pai-tool-ctx-cwd ctx)))
    (pai-mcp-script-run
     (plist-get args :code)
     (lambda (result)
       (funcall on-done
                (pai-mcp-guard-result
                 (list :content
                       (mapcar (lambda (block)
                                 (if (eq (plist-get block :type) 'image)
                                     (list :type "image" :data (plist-get block :data)
                                           :mimeType (plist-get block :mime-type))
                                   (list :type "text" :text (plist-get block :text))))
                               (plist-get result :content))
                       :isError (if (plist-get result :is-error) t :false)
                       :scriptDetails (plist-get result :details)) dir)))
     dir (plist-get args :timeoutMs) on-update)))

(defun pai-mcp-surface-register (&optional dir)
  "Register the native script tool unless settings.scriptMode is false."
  (if (eq (plist-get (pai-mcp-settings dir) :scriptMode) :false)
      (pai-unregister-tool "mcpScript")
    (pai-register-tool
     (list :name "mcpScript" :label "MCP script" :execution-mode 'parallel
           :description "Run trusted native Emacs Lisp asynchronously in a child Emacs (NOT JavaScript). Code is (lambda (call search describe emit done) ...). call PATH ARGS CALLBACK, search QUERY-PLIST CALLBACK, describe PATH CALLBACK return envelopes (:ok t :data VALUE) or (:ok :false :error ...). emit VALUE streams; done VALUE finishes. Compose calls with lexical callbacks; call done exactly once. 30s default deadline, 16 MiB intermediate JSON budget. Not a security sandbox."
           :parameters (pai-object-schema
                        (list :code (pai-string-schema "Single native Emacs Lisp lambda expression")
                              :timeoutMs (pai-number-schema "Hard deadline in milliseconds; default 30000"))
                        '("code"))
           :execute #'pai-mcp-script-execute))))

(provide 'pai-mcp-surface)
;;; pai-mcp-surface.el ends here
