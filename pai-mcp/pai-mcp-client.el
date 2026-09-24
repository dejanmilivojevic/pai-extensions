;;; pai-mcp-client.el --- Async MCP stdio client for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Non-blocking MCP client: newline-delimited JSON-RPC 2.0 over a `make-process'
;; stdio pipe, with per-request callbacks.  Servers are lazy and auto-start on
;; first use; metadata is cached to disk so search/describe work offline.  The
;; UI never blocks: every operation returns immediately and delivers through a
;; callback.  Part of the pai-mcp extension (port of nicobailon/pi-mcp-adapter).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-core)
(require 'pai-config)
(require 'pai-mcp-config)
(require 'pai-mcp-negotiation)

(declare-function pai-mcp-interactions-dispatch "pai-mcp-interactions" (server message))
(declare-function pai-mcp-interactions-cancel-server "pai-mcp-interactions" (server))
(declare-function pai-mcp-trace "pai-mcp-surface" (server direction message))
(declare-function pai-mcp-catalog-refresh "pai-mcp-catalog" (name on-done))
(declare-function pai-mcp-catalog-resource-tools "pai-mcp-catalog" (name))
(declare-function pai-mcp-catalog--list "pai-mcp-catalog" (server method key callback &optional cursor seen collected))
(declare-function pai-mcp-catalog-expand-template "pai-mcp-catalog" (template arguments))
(declare-function pai-mcp-catalog-validate-result "pai-mcp-catalog" (server tool result))
(declare-function pai-mcp-guard-approve "pai-mcp-guard" (server tool args on-allow on-deny &optional dir))
(declare-function pai-mcp-guard-result "pai-mcp-guard" (result &optional dir def))

(declare-function pai-mcp-http-start "pai-mcp-http" (name def))
(declare-function pai-mcp-http-send "pai-mcp-http" (server object))
(declare-function pai-mcp-http-stop "pai-mcp-http" (name))

(defcustom pai-mcp-protocol-version "2024-11-05"
  "MCP protocol version advertised in the initialize handshake."
  :type 'string :group 'pai-mcp)

(defcustom pai-mcp-startup-timeout 30
  "Seconds to wait for a server handshake before marking it failed."
  :type 'number :group 'pai-mcp)

(defcustom pai-mcp-default-idle-timeout 600
  "Default seconds an idle lazy server stays connected before disconnecting.
Overridden per server by the config `idleTimeout' (minutes)."
  :type 'number :group 'pai-mcp)

;;;; State (process-global: MCP servers are shared OS processes)

(defvar pai-mcp--servers (make-hash-table :test 'equal)
  "Server name -> state plist.
Keys: :name :def :process :buffer :status :next-id :pending :tools
:instructions :on-ready :error :idle-timer :last-used.  :status is one of
`stopped', `starting', `ready', `failed'.  :pending maps id -> callback.")

(defvar pai-mcp--metadata nil
  "Cached tool metadata: alist of (SERVER . PLIST) where PLIST has :tools
and :instructions.")

(defvar pai-mcp--metadata-loaded nil
  "Non-nil once `pai-mcp--metadata' has been read from disk this session.")

(defun pai-mcp--server (name)
  "Return the state plist for NAME, creating a stopped entry if needed."
  (or (gethash name pai-mcp--servers)
      (puthash name (list :name name :status 'stopped :next-id 0
                          :pending (make-hash-table :test 'eql)
                          :on-ready nil :tools nil :buffer "")
               pai-mcp--servers)))

(defun pai-mcp--set (server key value)
  "Set KEY to VALUE in SERVER state plist in place; return VALUE."
  (plist-put server key value) value)

(defun pai-mcp-server-status (name)
  "Return the status symbol for server NAME."
  (plist-get (pai-mcp--server name) :status))

;;;; JSON-RPC framing

(defun pai-mcp--send (server object)
  "Send JSON-RPC OBJECT through SERVER's active transport."
  (setq object (pai-mcp-protocol-decorate server object))
  (when (fboundp 'pai-mcp-trace) (pai-mcp-trace server 'outbound object))
  (if (eq (plist-get server :transport) 'http)
      (pai-mcp-http-send server object)
    (let ((proc (plist-get server :process)))
      (if (and proc (process-live-p proc))
          (process-send-string proc (concat (pai-json-encode object) "\n"))
        (pai-mcp--fail (plist-get server :name) "Transport is closed")))))

(defun pai-mcp--request (server method params callback)
  "Send a JSON-RPC request METHOD/PARAMS on SERVER; CALLBACK gets (RESULT ERROR)."
  (let ((id (pai-mcp--set server :next-id (1+ (plist-get server :next-id)))))
    (puthash id callback (plist-get server :pending))
    (pai-mcp--send server (list :jsonrpc "2.0" :id id :method method
                                :params (or params (pai-json-empty-object))))
    id))

(defun pai-mcp--notify (server method &optional params)
  "Send a JSON-RPC notification METHOD/PARAMS on SERVER (no id, no reply)."
  (pai-mcp--send server (list :jsonrpc "2.0" :method method
                              :params (or params (pai-json-empty-object)))))

(defun pai-mcp--dispatch (server message)
  "Dispatch server requests separately from JSON-RPC responses."
  (when (fboundp 'pai-mcp-trace) (pai-mcp-trace server 'inbound message))
  (if (plist-member message :method)
      (progn
        (require 'pai-mcp-interactions)
        (pai-mcp-interactions-dispatch server message))
    (when (plist-member message :id)
    (let* ((id (plist-get message :id))
           (pending (plist-get server :pending))
           (cb (and id (gethash id pending))))
      (when cb
        (remhash id pending)
        (funcall cb (plist-get message :result)
                 (let ((err (plist-get message :error)))
                   (if (plist-get server :protocol-probing) err
                     (and err (pai-mcp-protocol-error-hint err))))))))))

(defun pai-mcp--filter (name chunk)
  "Process filter for server NAME accumulating CHUNK into JSON-RPC lines."
  (let* ((server (pai-mcp--server name))
         (buf (concat (plist-get server :buffer) chunk))
         (lines (split-string buf "\n")))
    (pai-mcp--set server :buffer (car (last lines)))
    (dolist (line (butlast lines))
      (let ((trimmed (string-trim line)))
        (unless (string-empty-p trimmed)
          (condition-case err
              (pai-mcp--dispatch server (pai-json-decode trimmed))
            (error (message "pai-mcp[%s]: bad frame: %s" name
                            (error-message-string err)))))))))

;;;; Lifecycle

(defun pai-mcp--fail (name error-message)
  "Close NAME and reject its detached waiters and pending requests."
  (let* ((server (pai-mcp--server name))
         (pending (plist-get server :pending))
         (waiters (plist-get server :on-ready))
         (proc (plist-get server :process))
         callbacks)
    (when-let* ((cancel (plist-get server :protocol-cancel)))
      (pai-mcp--set server :protocol-cancel nil)
      (funcall cancel))
    (when (fboundp 'pai-mcp-interactions-cancel-server)
      (pai-mcp-interactions-cancel-server server))
    (pai-mcp--set server :status 'failed)
    (pai-mcp--set server :error error-message)
    (pai-mcp--set server :generation (1+ (or (plist-get server :generation) 0)))
    (dolist (key '(:idle-timer :startup-timer))
      (when (plist-get server key) (cancel-timer (plist-get server key)))
      (pai-mcp--set server key nil))
    (maphash (lambda (_id cb) (push cb callbacks)) pending)
    (clrhash pending)
    (pai-mcp--set server :on-ready nil)
    (when (and (eq (plist-get server :transport) 'http)
               (fboundp 'pai-mcp-http-stop))
      (pai-mcp-http-stop name))
    (pai-mcp--set server :process nil)
    (when (and proc (process-live-p proc)) (delete-process proc))
    (dolist (cb callbacks) (funcall cb nil error-message))
    (dolist (waiter (nreverse waiters)) (funcall (cdr waiter) error-message))))

(defun pai-mcp--exit-event-p (event)
  "Return non-nil when EVENT string denotes a process ending."
  (string-match-p "\\(finished\\|exited\\|died\\|broken\\|killed\\|terminated\\)" event))

(defun pai-mcp--sentinel (name _proc event)
  "Reject outstanding work when server NAME exits with EVENT."
  (when (and (pai-mcp--exit-event-p event)
             (memq (pai-mcp-server-status name) '(starting ready)))
    (pai-mcp--fail name (format "server %s exited (%s)" name (string-trim event)))))

(defun pai-mcp--flush-ready (name)
  "Deliver readiness to the detached waiters queued on NAME."
  (let* ((server (pai-mcp--server name))
         (waiters (plist-get server :on-ready)))
    (when (plist-get server :startup-timer)
      (cancel-timer (plist-get server :startup-timer))
      (pai-mcp--set server :startup-timer nil))
    (pai-mcp--set server :on-ready nil)
    (dolist (waiter (nreverse waiters)) (funcall (car waiter)))))

(defun pai-mcp--handshake (name)
  "Negotiate NAME, discover tools and optional catalogs, then release callers."
  (require 'pai-mcp-catalog)
  (let* ((server (pai-mcp--server name))
         (generation (plist-get server :generation))
         (origin (plist-get server :origin-buffer)))
    (pai-mcp--set server :client-capabilities
                  (list :sampling (pai-json-empty-object)
                        :elicitation (list :form (pai-json-empty-object)
                                           :url (pai-json-empty-object))))
    (pai-mcp-protocol-connect
     server
     (lambda ()
       (pai-mcp-catalog--list
        server "tools/list" :tools
        (lambda (tools error)
          (when (equal generation (plist-get server :generation))
            (if error (pai-mcp--fail name (pai-mcp-protocol-error-hint error))
              (pai-mcp--set server :tools tools)
              (pai-mcp--set server :status 'ready)
              (pai-mcp--cache-tools name tools (plist-get server :instructions))
              (with-current-buffer (if (buffer-live-p origin) origin (current-buffer))
                (let ((default-directory (plist-get server :directory)))
                  (pai-mcp-catalog-refresh
                   name (lambda (_catalog error)
                          (when error (message "MCP %s catalog: %s" name error))
                          (when (equal generation (plist-get server :generation))
                            (pai-mcp--flush-ready name)))))))))))
     (lambda (error) (pai-mcp--fail name error)))))

(defun pai-mcp--arm-startup (server generation)
  "Bound SERVER startup owned by GENERATION."
  (when-let* ((timer (plist-get server :startup-timer))) (cancel-timer timer))
  (pai-mcp--set
   server :startup-timer
   (run-at-time pai-mcp-startup-timeout nil
                (lambda ()
                  (when (and (= generation (plist-get server :generation))
                             (plist-get server :on-ready))
                    (pai-mcp--fail (plist-get server :name) "handshake timed out"))))))

(defun pai-mcp--start (name def)
  "Connect server NAME from DEF without blocking the UI."
  (let* ((server (pai-mcp--server name))
         (command (plist-get def :command))
         (generation (1+ (or (plist-get server :generation) 0))))
    (pai-mcp--set server :generation generation)
    (pai-mcp--set server :def def)
    (pai-mcp--set server :origin-buffer (current-buffer))
    (pai-mcp--set server :directory default-directory)
    (pai-mcp--set server :catalog-loaded nil)
    (pai-mcp--set server :protocol-version nil)
    (pai-mcp--set server :buffer "")
    (pai-mcp--set server :status 'starting)
    (pai-mcp--set server :error nil)
    (condition-case err
        (progn
          (cond
           (command
            (pai-mcp--set server :transport 'stdio)
            (pai-mcp--set
             server :protocol-start-session
             (lambda ()
               (when-let* ((data (plist-get def :pluginDataDir)))
                 (make-directory data t))
               (let* ((process-environment (pai-mcp--process-env def))
                      (default-directory
                       (if (plist-get def :cwd)
                           (file-name-as-directory
                            (expand-file-name (pai-mcp--interpolate (plist-get def :cwd))
                                              (plist-get server :directory)))
                         (plist-get server :directory)))
                      (args (if (plist-get def :literalEnv) (append (plist-get def :args) nil)
                              (mapcar #'pai-mcp--interpolate (plist-get def :args))))
                      (proc (make-process
                             :name (format "pai-mcp-%s" name)
                             :command (cons (pai-mcp--interpolate command) args)
                             :connection-type 'pipe :noquery t :coding 'utf-8
                             :filter (lambda (_p chunk)
                                       (when (= generation (plist-get server :generation))
                                         (pai-mcp--filter name chunk)))
                             :sentinel (lambda (p event)
                                         (when (= generation (plist-get server :generation))
                                           (pai-mcp--sentinel name p event))))))
                 (pai-mcp--set server :process proc)
                 (pai-mcp--arm-startup server generation))))
            (pai-mcp--handshake name))
           ((plist-get def :url)
            (pai-mcp--set server :transport 'http)
            (require 'pai-mcp-http)
            (pai-mcp--arm-startup server generation)
            (pai-mcp-http-start name def))
           (t (error "Server %s has no command or URL" name))))
      (error (pai-mcp--fail name (format "failed to start: %s"
                                       (error-message-string err)))))))

(defun pai-mcp-ensure (name on-ready on-error &optional dir)
  "Ensure server NAME is connected; call ON-READY, else ON-ERROR with a string.
Auto-starts a stopped or failed server (the core lazy-start behavior)."
  (let* ((def (pai-mcp--server-def name dir))
         (server (pai-mcp--server name)))
    (cond
     ((null def) (funcall on-error (format "unknown MCP server: %s" name)))
     ((pai-mcp--disabled-p def) (funcall on-error (format "server %s is disabled" name)))
     ((eq (plist-get server :status) 'ready)
      (pai-mcp--touch name) (funcall on-ready))
     ((eq (plist-get server :status) 'starting)
      (push (cons on-ready on-error) (plist-get server :on-ready)))
     (t
      (push (cons on-ready on-error) (plist-get server :on-ready))
      (let ((default-directory (file-name-as-directory (expand-file-name (or dir default-directory)))))
        (pai-mcp--start name def))))))

;;;; Idle disconnect

(defun pai-mcp--idle-timeout (def)
  "Return the idle timeout in seconds for DEF."
  (let ((mins (plist-get def :idleTimeout)))
    (if (numberp mins) (* 60 mins) pai-mcp-default-idle-timeout)))

(defun pai-mcp--touch (name)
  "Record activity on server NAME and (re)arm its idle-disconnect timer."
  (let* ((server (pai-mcp--server name))
         (def (plist-get server :def))
         (lifecycle (plist-get def :lifecycle))
         (old (plist-get server :idle-timer)))
    (pai-mcp--set server :last-used (float-time))
    (when old (cancel-timer old))
    (unless (member lifecycle '("keep-alive" "eager" "lazy-keep-alive"))
      (pai-mcp--set server :idle-timer
                    (run-at-time (pai-mcp--idle-timeout def) nil
                                 #'pai-mcp-stop name)))))

(defun pai-mcp-stop (name)
  "Disconnect NAME and reject outstanding calls; retain cached metadata."
  (pai-mcp--fail name "Server stopped")
  (pai-mcp--set (pai-mcp--server name) :status 'stopped)
  name)

;;;; Metadata cache (persisted; search/describe need no live connection)

(defun pai-mcp--metadata-file ()
  "Return the on-disk metadata cache path."
  (expand-file-name "mcp-metadata.json" pai-directory))

(defun pai-mcp--load-metadata ()
  "Load the metadata cache from disk once per session."
  (unless pai-mcp--metadata-loaded
    (setq pai-mcp--metadata-loaded t)
    (let ((json (pai-mcp--read-json-file (pai-mcp--metadata-file))))
      (setq pai-mcp--metadata (pai-mcp--plist-to-alist json))))
  pai-mcp--metadata)

(defun pai-mcp--cache-tools (name tools &optional instructions)
  "Cache TOOLS and INSTRUCTIONS for server NAME in memory and on disk."
  (pai-mcp--load-metadata)
  (setq pai-mcp--metadata
        (cons (cons name (plist-put (plist-put
                                    (copy-sequence (cdr (assoc name pai-mcp--metadata)))
                                    :tools tools) :instructions instructions))
              (assoc-delete-all name pai-mcp--metadata)))
  (condition-case err
      (let ((obj (apply #'append
                        (mapcar (lambda (pair)
                                  (list (intern (concat ":" (car pair))) (cdr pair)))
                                pai-mcp--metadata))))
        (make-directory (file-name-directory (pai-mcp--metadata-file)) t)
        (with-temp-file (pai-mcp--metadata-file)
          (insert (pai-json-encode (or obj (pai-json-empty-object))))))
    (error (message "pai-mcp: cannot write metadata cache: %s"
                    (error-message-string err)))))

(defun pai-mcp--server-tools (name)
  "Return the cached or live tool list for server NAME."
  (let ((server (gethash name pai-mcp--servers)))
    (append (or (and server (plist-get server :tools))
                (plist-get (cdr (assoc name (pai-mcp--load-metadata))) :tools))
            (when (fboundp 'pai-mcp-catalog-resource-tools)
              (pai-mcp-catalog-resource-tools name)))))

(defun pai-mcp--server-instructions (name)
  "Return cached or live instructions text for server NAME."
  (let ((server (gethash name pai-mcp--servers)))
    (or (and server (plist-get server :instructions))
        (plist-get (cdr (assoc name (pai-mcp--load-metadata))) :instructions))))

(defun pai-mcp--all-tools (&optional dir)
  "Return a list of (SERVER . TOOL) across configured servers from cache."
  (let (out)
    (dolist (pair (pai-mcp-load-config dir))
      (dolist (tool (pai-mcp--server-tools (car pair)))
        (push (cons (car pair) tool) out)))
    (nreverse out)))

;;;; Tool invocation

(defun pai-mcp--find-server-for-tool (tool-name &optional dir)
  "Return the server name that owns TOOL-NAME from cached metadata, or nil."
  (car (seq-find (lambda (pair) (equal tool-name (plist-get (cdr pair) :name)))
                 (pai-mcp--all-tools dir))))

(defun pai-mcp--request-timeout-ms (def &optional dir)
  "Return the live-request timeout in ms for DEF, or nil for no timeout.
Per-server `requestTimeoutMs' overrides the global setting; <= 0 disables."
  (let ((ms (or (plist-get def :requestTimeoutMs)
                (plist-get (pai-mcp-settings dir) :requestTimeoutMs))))
    (and (numberp ms) (> ms 0) ms)))

(defun pai-mcp--request-timed (server method params timeout-ms callback)
  "Send request METHOD/PARAMS on SERVER; reject after TIMEOUT-MS with a timeout.
CALLBACK gets (RESULT ERROR); at most one of the reply/timeout fires it."
  (let* ((pending (plist-get server :pending))
         (fired nil) (timer nil) id)
    (cl-flet ((settle (result error)
                (unless fired
                  (setq fired t)
                  (when timer (cancel-timer timer))
                  (funcall callback result error))))
      (setq id (pai-mcp--request server method params
                                 (lambda (result error) (settle result error))))
      (when timeout-ms
        (setq timer (run-at-time (/ timeout-ms 1000.0) nil
                                 (lambda ()
                                   (when (gethash id pending) (remhash id pending))
                                   (settle nil (format "request timed out after %dms" timeout-ms))))))
      id)))

(defun pai-mcp-call-raw (server tool args on-done &optional dir)
  "Call TOOL on SERVER after approval; deliver its untruncated MCP result."
  (require 'pai-mcp-guard)
  (require 'pai-mcp-catalog)
  (let ((origin (current-buffer))
        (directory (or dir default-directory)) settled)
    (cl-labels
        ((finish (result)
           (unless settled
             (setq settled t)
             (funcall on-done result)))
         (fail (error)
           (finish (list :isError t :content
                         (list (list :type "text" :text
                                     (pai-mcp-protocol-error-hint error))))))
         (execute ()
           (let* ((state (pai-mcp--server server))
                  (metadata (cl-find tool (pai-mcp--server-tools server)
                                     :key (lambda (item) (plist-get item :name)) :test #'equal))
                  (resource (plist-get metadata :pai-mcp-resource))
                  (params (if resource
                              (list :uri (or (plist-get resource :uri)
                                             (pai-mcp-catalog-expand-template
                                              (plist-get resource :uriTemplate) args)))
                            (list :name tool :arguments (or args (pai-json-empty-object))))))
             (pai-mcp--set state :origin-buffer origin)
             (pai-mcp--touch server)
             (pai-mcp--request-timed
              state (if resource "resources/read" "tools/call") params
              (pai-mcp--request-timeout-ms (or (plist-get state :def)
                                              (pai-mcp--server-def server directory)) directory)
              (lambda (result error)
                (if error (fail error)
                  (condition-case error
                      (finish
                       (if resource
                           (list :content (mapcar (lambda (content)
                                                    (list :type "resource" :resource content))
                                                  (plist-get result :contents)))
                         (pai-mcp-catalog-validate-result server metadata result)))
                    (error (fail (error-message-string error))))))))))
      (pai-mcp-ensure
       server
       (lambda ()
         (if (not (buffer-live-p origin)) (fail "Origin buffer was closed")
           (with-current-buffer origin
             (let ((default-directory directory)
                   (generation (plist-get (pai-mcp--server server) :generation)))
               (pai-mcp-guard-approve
                server tool args
                (lambda ()
                  (if (not (equal generation (plist-get (pai-mcp--server server) :generation)))
                      (fail "Connection changed during approval")
                    (condition-case error (execute)
                      (error (fail (error-message-string error))))))
                (lambda () (fail "MCP tool approval denied")) directory)))))
       #'fail directory))))

(defun pai-mcp-call (server tool args on-done &optional dir)
  "Call TOOL on SERVER and bound its output for the originating pai session."
  (pai-mcp-call-raw
   server tool args
   (lambda (result)
     (let ((converted (condition-case error
                          (pai-mcp-guard-result result dir
                                                (plist-get (pai-mcp--server server) :def))
                        (error (pai-tool-error-result (error-message-string error))))))
       (funcall on-done converted))) dir))

(defun pai-mcp--result->tool (result)
  "Convert protocol RESULT through the output guard."
  (require 'pai-mcp-guard)
  (pai-mcp-guard-result result))

;;;; refresh: probe enabled stdio servers to fill the metadata cache

(defun pai-mcp-refresh (on-done &optional dir)
  "Connect every enabled server, cache its tools, then ON-DONE a summary.
Non-blocking: ON-DONE fires once all probes settle."
  (let* ((config (seq-filter
                  (lambda (pair) (and (or (plist-get (cdr pair) :command)
                                          (plist-get (cdr pair) :url))
                                      (not (pai-mcp--disabled-p (cdr pair)))))
                  (pai-mcp-load-config dir)))
         (pending (length config))
         (results '()))
    (if (zerop pending)
        (funcall on-done "No enabled MCP servers to refresh.")
      (dolist (pair config)
        (let ((name (car pair)))
          (pai-mcp-ensure
           name
           (lambda ()
             (push (format "%s: %d tool(s)" name (length (pai-mcp--server-tools name))) results)
             (when (zerop (setq pending (1- pending)))
               (funcall on-done (string-join (nreverse results) "\n"))))
           (lambda (err)
             (push (format "%s: %s" name err) results)
             (when (zerop (setq pending (1- pending)))
               (funcall on-done (string-join (nreverse results) "\n"))))
           dir))))))

;;;; Lifecycle: eager/keep-alive startup and catalog refresh

(defun pai-mcp--tool-names (tools)
  "Return the list of tool name strings in TOOLS."
  (mapcar (lambda (tl) (plist-get tl :name)) tools))

(defun pai-mcp-refresh-catalog (name on-done)
  "Re-run tools/list on the ready server NAME; ON-DONE gets (ADDED-NAMES ERROR).
ADDED-NAMES are tool names newly present since the last catalog."
  (let ((server (pai-mcp--server name)))
    (if (not (eq (plist-get server :status) 'ready))
        (funcall on-done nil "not connected")
      (let ((old (pai-mcp--tool-names (plist-get server :tools))))
        (pai-mcp--request
         server "tools/list" nil
         (lambda (result error)
           (if error
               (funcall on-done nil error)
             (let ((tools (plist-get result :tools)))
               (pai-mcp--set server :tools tools)
               (pai-mcp--cache-tools name tools (plist-get server :instructions))
               (funcall on-done
                        (seq-difference (pai-mcp--tool-names tools) old)
                        nil)))))))))

(defun pai-mcp--keep-alive-p (def)
  "Return non-nil when DEF uses a keep-alive lifecycle."
  (member (plist-get def :lifecycle) '("keep-alive" "lazy-keep-alive")))

(defun pai-mcp-startup-connect (&optional dir)
  "Connect enabled servers whose lifecycle requests eager startup.
`eager' and `keep-alive' connect at startup; `lazy'/`lazy-keep-alive' do not."
  (dolist (pair (pai-mcp-load-config dir))
    (let ((name (car pair)) (def (cdr pair)))
      (when (and (or (plist-get def :command) (plist-get def :url))
                 (not (pai-mcp--disabled-p def))
                 (member (plist-get def :lifecycle) '("eager" "keep-alive")))
        (pai-mcp-ensure name #'ignore #'ignore dir)))))

(provide 'pai-mcp-client)
;;; pai-mcp-client.el ends here
