;;; pai-mcp-http-server.el --- Local MCP HTTP fixture -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(cl-defstruct pai-mcp-http-fixture
  listener clients timers requests stream handler)

(defun pai-mcp-http-fixture-start (handler)
  "Listen on an ephemeral loopback port, serving HANDLER (FIXTURE CLIENT REQUEST)."
  (let ((fixture (make-pai-mcp-http-fixture :handler handler)))
    (setf (pai-mcp-http-fixture-listener fixture)
          (make-network-process
           :name "pai-mcp-http-fixture" :server t :host "127.0.0.1"
           :service t :family 'ipv4 :coding 'binary :noquery t
           :log (lambda (_server client _message)
                  (push client (pai-mcp-http-fixture-clients fixture))
                  (set-process-query-on-exit-flag client nil)
                  (set-process-coding-system client 'binary 'binary)
                  (set-process-filter
                   client (lambda (process chunk)
                            (pai-mcp-http-fixture-filter fixture process chunk))))))
    fixture))

(defun pai-mcp-http-fixture-url (fixture &optional path)
  (format "http://127.0.0.1:%s%s"
          (process-contact (pai-mcp-http-fixture-listener fixture) :service)
          (or path "/mcp")))

(defun pai-mcp-http-fixture-stop (fixture)
  (dolist (timer (pai-mcp-http-fixture-timers fixture)) (cancel-timer timer))
  (dolist (process (cons (pai-mcp-http-fixture-listener fixture)
                         (pai-mcp-http-fixture-clients fixture)))
    (when (process-live-p process) (delete-process process))))

(defun pai-mcp-http-fixture-later (fixture delay function)
  "Run FUNCTION later and own its timer until teardown."
  (push (run-at-time delay nil function) (pai-mcp-http-fixture-timers fixture)))

(defun pai-mcp-http-fixture-filter (fixture client chunk)
  "Parse one complete binary HTTP request and dispatch it."
  (unless (process-get client 'handled)
    (let ((bytes (concat (or (process-get client 'bytes) "") chunk)))
      (process-put client 'bytes bytes)
      (when (string-match "\r\n\r\n" bytes)
        (let* ((end (match-end 0))
               (lines (split-string (substring bytes 0 (- end 4)) "\r\n"))
               (first (split-string (pop lines) " "))
               (headers (mapcar (lambda (line)
                                  (let ((colon (string-match ":" line)))
                                    (cons (downcase (substring line 0 colon))
                                          (string-trim (substring line (1+ colon))))))
                                lines))
               (length (string-to-number (or (cdr (assoc "content-length" headers)) "0"))))
          (when (>= (- (length bytes) end) length)
            (process-put client 'handled t)
            (let* ((body (substring bytes end (+ end length)))
                   (request (list :method (car first) :path (cadr first)
                                  :headers headers :body body
                                  :json (unless (string-empty-p body)
                                          (json-parse-string
                                           (decode-coding-string body 'utf-8)
                                           :object-type 'plist :array-type 'list
                                           :null-object nil :false-object :json-false)))))
              (push request (pai-mcp-http-fixture-requests fixture))
              (funcall (pai-mcp-http-fixture-handler fixture) fixture client request))))))))

(defun pai-mcp-http-fixture-response (client status body &optional headers)
  "Send a finite response, counting bytes rather than Lisp characters."
  (when (process-live-p client)
    (let ((bytes (encode-coding-string body 'utf-8)))
      (process-send-string
       client (concat (format "HTTP/1.1 %d Fixture\r\nContent-Length: %d\r\nConnection: close\r\n"
                              status (length bytes))
                      (mapconcat (lambda (header) (concat (car header) ": " (cdr header) "\r\n"))
                                 headers "")
                      "\r\n" bytes))
      (process-send-eof client))))

(defun pai-mcp-http-fixture-fragments (fixture client chunks)
  "Send CHUNKS in separate timer turns, then close the response."
  (if (null chunks)
      (when (process-live-p client) (process-send-eof client))
    (pai-mcp-http-fixture-later
     fixture 0.01
     (lambda ()
       (when (process-live-p client)
         (process-send-string client (car chunks))
         (pai-mcp-http-fixture-fragments fixture client (cdr chunks)))))))

(defun pai-mcp-http-fixture-result (request)
  "Build a genuine MCP response for REQUEST."
  (let* ((json (plist-get request :json))
         (method (plist-get json :method)))
    (when (plist-member json :id)
      (list :jsonrpc "2.0" :id (plist-get json :id)
            :result
            (pcase method
              ("initialize"
               (list :protocolVersion (plist-get (plist-get json :params) :protocolVersion)
                     :capabilities (list :tools (make-hash-table))
                     :serverInfo (list :name "local-http-fixture" :version "1")))
              ("tools/list"
               (list :tools (vector (list :name "echo" :description "Echo text"
                                         :inputSchema (list :type "object"
                                                            :properties (make-hash-table))))))
              ("tools/call"
               (list :content
                     (vector (list :type "text"
                                   :text (plist-get
                                          (plist-get (plist-get json :params) :arguments)
                                          :text)))))
              (_ (make-hash-table)))))))

(defun pai-mcp-http-fixture-modern (fixture client request &optional fragmented)
  "Serve modern MCP, rejecting missing session/version headers after initialize."
  (let* ((json (plist-get request :json))
         (method (plist-get json :method))
         (headers (plist-get request :headers))
         (result (pai-mcp-http-fixture-result request)))
    (cond
     ((equal (plist-get request :method) "DELETE")
      (pai-mcp-http-fixture-response client 204 ""))
     ((and (not (equal method "initialize"))
           (or (not (equal (cdr (assoc "mcp-session-id" headers)) "fixture-session"))
               (not (cdr (assoc "mcp-protocol-version" headers)))))
      (pai-mcp-http-fixture-response client 400 "Session or protocol header missing"))
     ((null result) (pai-mcp-http-fixture-response client 202 ""))
     ((and fragmented (equal method "tools/call"))
      (let* ((data (encode-coding-string (json-serialize result) 'utf-8))
             (unicode (string-match (unibyte-string #xce #xbb) data))
             (split (if unicode (1+ unicode) (/ (length data) 2))))
        ;; Split both the SSE delimiter and a UTF-8 character across writes.
        (process-send-string client "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\n")
        (pai-mcp-http-fixture-fragments
         fixture client (list ": heartbeat\r\n\r\nevent: message\r\ndat" "a: "
                              (substring data 0 split) (substring data split)
                              "\r" "\n\r" "\n"))))
     (t
      (pai-mcp-http-fixture-response
       client 200 (json-serialize result)
       '(("Content-Type" . "application/json") ("Mcp-Session-Id" . "fixture-session")))))))

(defun pai-mcp-http-fixture-legacy (fixture client request)
  "Reject modern POST, advertise a legacy endpoint, and answer on the GET stream."
  (cond
   ((and (equal (plist-get request :path) "/mcp")
         (equal (plist-get request :method) "POST"))
    (pai-mcp-http-fixture-response client 405 "Use SSE"))
   ((equal (plist-get request :method) "GET")
    (setf (pai-mcp-http-fixture-stream fixture) client)
    (process-send-string client "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\nevent: endpoint\r\ndata: /messages?session=legacy\r\n\r\n"))
   ((equal (plist-get request :path) "/messages?session=legacy")
    (pai-mcp-http-fixture-response client 202 "")
    (let ((result (pai-mcp-http-fixture-result request))
          (stream (pai-mcp-http-fixture-stream fixture)))
      (when result
        (pai-mcp-http-fixture-later
         fixture 0.01
         (lambda ()
           (when (process-live-p stream)
             (process-send-string stream (encode-coding-string
                                          (concat "event: message\ndata: "
                                                  (json-serialize result) "\n\n") 'utf-8))))))))
   (t (pai-mcp-http-fixture-response client 404 "Unknown endpoint"))))

(provide 'pai-mcp-http-server)
;;; pai-mcp-http-server.el ends here
