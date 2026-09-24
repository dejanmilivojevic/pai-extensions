;;; pai-mcp-fake-server.el --- Minimal stdio MCP server for tests -*- lexical-binding: t; -*-

;;; Commentary:

;; A network-free MCP server used by pai-mcp tests.  Run with:
;;   emacs -Q --batch -l extensions/pai-mcp/test/fixtures/pai-mcp-fake-server.el
;; It speaks newline-delimited JSON-RPC 2.0 on stdio and implements
;; initialize, notifications/initialized, tools/list, and tools/call.
;; Set PAI_MCP_FAKE_SLEEP to add a startup delay (seconds) and
;; PAI_MCP_FAKE_INSTRUCTIONS to advertise server instructions.

;;; Code:

(require 'json)

(defvar pai-mcp-fake--tools
  '((:name "echo"
     :description "Echo back the given text."
     :inputSchema (:type "object"
                   :properties (:text (:type "string" :description "Text to echo"))
                   :required ("text")))
    (:name "add"
     :description "Add two numbers."
     :inputSchema (:type "object"
                   :properties (:a (:type "number") :b (:type "number"))
                   :required ("a" "b")))))

(defun pai-mcp-fake--send (object)
  "Encode OBJECT as one JSON line to stdout."
  (princ (concat (json-encode object) "\n"))
  ;; batch stdout is line-buffered via princ; nothing else needed.
  )

(defun pai-mcp-fake--handle (msg)
  "Handle one decoded JSON-RPC MSG (alist)."
  (let* ((id (cdr (assq 'id msg)))
         (method (cdr (assq 'method msg)))
         (params (cdr (assq 'params msg))))
    (when-let* ((file (getenv "PAI_MCP_FAKE_LOG")))
      (with-temp-buffer
        (insert method "\n")
        (write-region (point-min) (point-max) file t 'silent)))
    (pcase method
      ("initialize"
       (pai-mcp-fake--send
        `((jsonrpc . "2.0") (id . ,id)
          (result . ((protocolVersion . "2024-11-05")
                     (capabilities . ((tools . ,(make-hash-table))))
                     (serverInfo . ((name . "fake") (version . "1.0")))
                     ,@(when (getenv "PAI_MCP_FAKE_INSTRUCTIONS")
                         `((instructions . ,(getenv "PAI_MCP_FAKE_INSTRUCTIONS")))))))))
      ("notifications/initialized" nil)     ; no reply to notifications
      ("tools/list"
       (pai-mcp-fake--send
        `((jsonrpc . "2.0") (id . ,id)
          (result . ((tools . ,(vconcat (mapcar #'pai-mcp-fake--tool-alist
                                                pai-mcp-fake--tools))))))))
      ("tools/call"
       (let* ((name (cdr (assq 'name params)))
              (arguments (cdr (assq 'arguments params)))
              (text (pcase name
                      ("echo" (format "%s" (cdr (assq 'text arguments))))
                      ("add" (number-to-string
                              (+ (or (cdr (assq 'a arguments)) 0)
                                 (or (cdr (assq 'b arguments)) 0))))
                      (_ nil))))
         (if text
             (pai-mcp-fake--send
              `((jsonrpc . "2.0") (id . ,id)
                (result . ((content . [((type . "text") (text . ,text))])
                           (isError . :json-false)))))
           (pai-mcp-fake--send
            `((jsonrpc . "2.0") (id . ,id)
              (result . ((content . [((type . "text") (text . "unknown tool"))])
                         (isError . t))))))))
      (_ (when id
           (pai-mcp-fake--send
            `((jsonrpc . "2.0") (id . ,id)
              (error . ((code . -32601) (message . "method not found"))))))))))

(defun pai-mcp-fake--tool-alist (plist)
  "Convert a tool PLIST to a JSON-encodable alist."
  (let (out (p plist))
    (while p
      (push (cons (intern (substring (symbol-name (car p)) 1)) (cadr p)) out)
      (setq p (cddr p)))
    (nreverse out)))

(let ((delay (getenv "PAI_MCP_FAKE_SLEEP")))
  (when (and delay (> (string-to-number delay) 0))
    (sleep-for (string-to-number delay))))

;; Main loop: read one JSON line per iteration until EOF.
(let ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol)
      line)
  (while (setq line (ignore-errors (read-from-minibuffer "")))
    (let ((trimmed (string-trim line)))
      (unless (string-empty-p trimmed)
        (condition-case nil
            (pai-mcp-fake--handle (json-read-from-string trimmed))
          (error nil))))))

;;; pai-mcp-fake-server.el ends here
