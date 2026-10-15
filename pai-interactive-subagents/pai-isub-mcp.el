;;; pai-isub-mcp.el --- MCP tools for external subagents -*- lexical-binding: t; -*-

;;; Commentary:

;; An external agent (run by the agent-shell backend) cannot be handed an
;; Emacs Lisp tool directly, but every ACP agent accepts stdio MCP servers.
;; So the child is given one: `bin/pai-isub-mcp-bridge.el', a tiny batch
;; Emacs that relays MCP JSON-RPC lines from its stdin to a local TCP server
;; in this Emacs and prints the answers.  The server speaks just enough MCP
;; (initialize, tools/list, tools/call, ping) to offer `reply_to_parent'.
;;
;; Every child gets its own random token, passed to its bridge on the
;; command line and prefixed to each relayed line, which both authenticates
;; the bridge and tells the server which child is talking.  The server only
;; listens on 127.0.0.1, is started on demand, and stops when its last child
;; is gone.  Nothing here blocks: the filter answers from what it has and
;; hands replies to the parent from a timer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst pai-isub-mcp-server-name "pai-parent"
  "MCP server name the child agents see (tools appear under it).")

(defconst pai-isub-mcp--bridge-file
  (expand-file-name "bin/pai-isub-mcp-bridge.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The stdio relay script the child agent launches.")

(defvar pai-isub-mcp--server nil "The listening network process, or nil.")

(defvar pai-isub-mcp--children (make-hash-table :test 'equal)
  "Token -> function (TOOL-NAME ARGS) -> (TEXT . IS-ERROR), one per child.")

(defconst pai-isub-mcp--tools
  (vector
   (list :name "reply_to_parent"
         :description "Send a message to the parent agent that spawned this subagent session. Use it to ask a question, surface a decision you need, report a partial finding, or warn about something before the task ends. The parent receives it as soon as its current turn allows."
         :inputSchema (list :type "object"
                            :properties (list :message (list :type "string"
                                                             :description "What to tell the parent agent."))
                            :required (vector "message"))))
  "The MCP tools offered to child agents.")

;;;; Server lifecycle

(defun pai-isub-mcp-port ()
  "Return the port of the running server, starting it first if needed."
  (unless (process-live-p pai-isub-mcp--server)
    (setq pai-isub-mcp--server
          (make-network-process
           :name "pai-isub-mcp" :server t :host "127.0.0.1" :service t
           :family 'ipv4 :coding 'utf-8-unix :noquery t
           :filter #'pai-isub-mcp--filter)))
  (process-contact pai-isub-mcp--server :service))

(defun pai-isub-mcp--maybe-stop ()
  "Stop the server when no child uses it."
  (when (and (zerop (hash-table-count pai-isub-mcp--children))
             (process-live-p pai-isub-mcp--server))
    (delete-process pai-isub-mcp--server)
    (setq pai-isub-mcp--server nil)))

(defun pai-isub-mcp-register (handler)
  "Register a child whose tool calls go to HANDLER; return its token.
HANDLER is called with the tool name and its argument plist and returns
\(TEXT . IS-ERROR)."
  (let ((token (md5 (format "%s%s%s" (random) (float-time) (emacs-pid)))))
    (puthash token handler pai-isub-mcp--children)
    token))

(defun pai-isub-mcp-unregister (token)
  "Forget the child with TOKEN; stop the server when it was the last."
  (when token
    (remhash token pai-isub-mcp--children)
    (pai-isub-mcp--maybe-stop)))

(defun pai-isub-mcp-server-config (token)
  "Return the ACP stdio MCP server entry (an alist) for the child with TOKEN."
  `((name . ,pai-isub-mcp-server-name)
    (command . ,(expand-file-name invocation-name invocation-directory))
    (args . ("-Q" "--batch" "-l" ,pai-isub-mcp--bridge-file
             ,(number-to-string (pai-isub-mcp-port)) ,token))
    (env . ())))

;;;; Protocol

(defun pai-isub-mcp--result (id result)
  "Return a JSON-RPC success response for ID carrying RESULT."
  (list :jsonrpc "2.0" :id id :result result))

(defun pai-isub-mcp--error (id code message)
  "Return a JSON-RPC error response for ID."
  (list :jsonrpc "2.0" :id id :error (list :code code :message message)))

(defun pai-isub-mcp-handle (token message)
  "Answer MCP MESSAGE (a plist) from the child with TOKEN; nil for notifications."
  (let ((id (plist-get message :id))
        (method (plist-get message :method))
        (params (plist-get message :params))
        (handler (gethash token pai-isub-mcp--children)))
    (when id
      (cond
       ((null handler) (pai-isub-mcp--error id -32001 "Unknown subagent session"))
       ((equal method "initialize")
        (pai-isub-mcp--result
         id (list :protocolVersion (or (plist-get params :protocolVersion) "2024-11-05")
                  :capabilities (list :tools (make-hash-table))
                  :serverInfo (list :name pai-isub-mcp-server-name :version "1"))))
       ((equal method "ping") (pai-isub-mcp--result id (make-hash-table)))
       ((equal method "tools/list") (pai-isub-mcp--result id (list :tools pai-isub-mcp--tools)))
       ((equal method "tools/call")
        (let* ((reply (condition-case err
                          (funcall handler (plist-get params :name)
                                   (plist-get params :arguments))
                        (error (cons (error-message-string err) t)))))
          (pai-isub-mcp--result
           id (list :content (vector (list :type "text" :text (or (car reply) "")))
                    :isError (if (cdr reply) t :false)))))
       (t (pai-isub-mcp--error id -32601 (format "Method not found: %s" method)))))))

(defun pai-isub-mcp--line (proc line)
  "Process one relayed LINE (\"TOKEN\\tJSON\") from connection PROC."
  (when (string-match "\\`\\([^\t]+\\)\t\\(.*\\)\\'" line)
    (let* ((token (match-string 1 line))
           (json (match-string 2 line))
           (message (ignore-errors
                      (json-parse-string json :object-type 'plist
                                         :null-object nil :false-object :false)))
           (response (if message
                         (pai-isub-mcp-handle token message)
                       (pai-isub-mcp--error :null -32700 "Parse error"))))
      (when response
        (process-send-string
         proc (concat (json-serialize response :null-object :null :false-object :false)
                      "\n"))))))

(defun pai-isub-mcp--filter (proc string)
  "Accumulate STRING from connection PROC and answer each complete line."
  (let ((pending (concat (or (process-get proc 'pending) "") string)))
    (save-match-data
      (while (string-match "\n" pending)
        (let ((line (substring pending 0 (match-beginning 0))))
          (setq pending (substring pending (match-end 0)))
          (condition-case err
              (pai-isub-mcp--line proc line)
            (error (message "pai-isub-mcp: %s" (error-message-string err)))))))
    (process-put proc 'pending pending)))

(provide 'pai-isub-mcp)
;;; pai-isub-mcp.el ends here
