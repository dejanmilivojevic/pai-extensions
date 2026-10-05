;;; pai-web-http.el --- A small HTTP/1.1 server that never blocks Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; An HTTP/1.1 server built on `make-network-process' (no external
;; program).  Requests are parsed incrementally in the connection filter;
;; complete requests are handed to `pai-web-http-handler' as a plist
;;
;;   (:proc PROCESS :method "GET" :path "/api/x" :query ALIST
;;    :headers ALIST :body UNIBYTE-STRING :peer "1.2.3.4")
;;
;; Header and query keys are lower-case strings.  The handler answers with
;; `pai-web-http-respond', now or later (long polling keeps the request).
;;
;; Why responses are capped: Emacs writes to a socket with
;; `process-send-string', which does not return until every byte went out.
;; A peer that stops reading (a phone that lost its network) would freeze
;; Emacs once the kernel's socket buffer is full.  Every response is
;; therefore kept below `pai-web-http-max-response' bytes -- far below that
;; buffer -- and only written in answer to a request that just arrived on
;; a keep-alive connection, which proves the previous response was read.
;; Larger payloads are served in pieces the client asks for one by one
;; (see "blobs" in pai-web-bus.el).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)

(defconst pai-web-http-max-response (* 48 1024)
  "Largest response body written in one go; bigger ones are refused.
Callers split larger payloads (see `pai-web-bus-respond-json').")

(defconst pai-web-http-max-header 16384
  "Largest request head (request line and headers) accepted.")

(defvar pai-web-http-max-body (* 40 1024 1024)
  "Largest request body accepted (uploads).")

(defconst pai-web-http-max-connections 64
  "Connections beyond this many are closed right away.")

(defconst pai-web-http-idle-timeout 75
  "Seconds an idle keep-alive connection is kept open.")

(defvar pai-web-http-handler nil
  "Function called with each complete request plist.")

(defvar pai-web-http--server nil
  "The listening server process, or nil.")

(defvar pai-web-http--sweeper nil
  "Timer closing idle connections.")

(defconst pai-web-http--reasons
  '((200 . "OK") (202 . "Accepted") (204 . "No Content") (302 . "Found")
    (304 . "Not Modified") (400 . "Bad Request") (401 . "Unauthorized")
    (403 . "Forbidden") (404 . "Not Found") (405 . "Method Not Allowed")
    (409 . "Conflict") (413 . "Payload Too Large") (429 . "Too Many Requests")
    (431 . "Request Header Fields Too Large") (500 . "Internal Server Error")
    (503 . "Service Unavailable"))
  "Reason phrases of the status codes used.")

;;;; Server lifecycle

(defun pai-web-http-running-p ()
  "Return non-nil when the server is listening."
  (and pai-web-http--server (process-live-p pai-web-http--server)))

(defun pai-web-http-start (host port)
  "Listen on HOST (an address string) and PORT; return the server process.
Signals an error when the port cannot be bound (e.g. already in use)."
  (when (pai-web-http-running-p)
    (error "The pai-web server is already running"))
  (setq pai-web-http--server
        (make-network-process
         :name "pai-web" :server 16 :host host :service port :family 'ipv4
         :coding 'binary :noquery t :reuseaddr t
         :filter #'pai-web-http--filter
         :sentinel #'pai-web-http--sentinel
         :log #'pai-web-http--accept))
  (unless (timerp pai-web-http--sweeper)
    (setq pai-web-http--sweeper (run-at-time 30 30 #'pai-web-http--sweep)))
  pai-web-http--server)

(defun pai-web-http-stop ()
  "Stop listening and close every connection."
  (when (timerp pai-web-http--sweeper) (cancel-timer pai-web-http--sweeper))
  (setq pai-web-http--sweeper nil)
  (when pai-web-http--server
    (ignore-errors (delete-process pai-web-http--server)))
  (setq pai-web-http--server nil)
  (dolist (p (pai-web-http-connections))
    (ignore-errors (delete-process p))))

(defun pai-web-http-connections ()
  "Return the live client connections."
  (seq-filter (lambda (p) (and (process-get p 'pai-web-conn) (process-live-p p)))
              (process-list)))

(defun pai-web-http-port ()
  "Return the port the server listens on, or nil."
  (when (pai-web-http-running-p)
    (process-contact pai-web-http--server :service)))

;;;; Connections

(defun pai-web-http--accept (_server proc _message)
  "Set up the new connection PROC."
  (if (> (length (pai-web-http-connections)) pai-web-http-max-connections)
      (ignore-errors (delete-process proc))
    (process-put proc 'pai-web-conn t)
    (process-put proc 'last (float-time))
    (set-process-query-on-exit-flag proc nil)))

(defun pai-web-http--sentinel (proc _event)
  "Clean up after the connection PROC closed."
  (unless (process-live-p proc)
    (let ((buf (process-get proc 'buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (process-put proc 'buf nil)
    (when-let ((fn (process-get proc 'on-close)))
      (process-put proc 'on-close nil)
      (ignore-errors (funcall fn proc)))))

(defun pai-web-http--sweep ()
  "Close connections idle for longer than `pai-web-http-idle-timeout'."
  (let ((limit (- (float-time) pai-web-http-idle-timeout)))
    (dolist (p (pai-web-http-connections))
      (when (and (not (process-get p 'held))
                 (< (or (process-get p 'last) 0) limit))
        (ignore-errors (delete-process p))))))

(defun pai-web-http--buffer (proc)
  "Return PROC's unibyte input buffer, creating it."
  (let ((buf (process-get proc 'buf)))
    (unless (buffer-live-p buf)
      (setq buf (generate-new-buffer " *pai-web-conn*" t))
      (with-current-buffer buf
        (set-buffer-multibyte nil)
        (setq buffer-undo-list t))
      (process-put proc 'buf buf))
    buf))

(defun pai-web-http--filter (proc string)
  "Collect STRING from PROC and dispatch every complete request."
  (process-put proc 'last (float-time))
  (let ((buf (pai-web-http--buffer proc)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert string))
    (condition-case err
        (pai-web-http--drain proc buf)
      (error
       (message "pai-web: bad request: %s" (error-message-string err))
       (ignore-errors (pai-web-http--fail proc 400 "Bad request"))))))

(defun pai-web-http--drain (proc buf)
  "Parse and dispatch the complete requests buffered in BUF for PROC."
  (let ((continue t))
    (while (and continue (process-live-p proc) (buffer-live-p buf))
      (setq continue nil)
      (let ((head (process-get proc 'head)))
        (unless head
          (with-current-buffer buf
            (goto-char (point-min))
            (if (search-forward "\r\n\r\n" nil t)
                (let ((text (buffer-substring-no-properties (point-min) (- (point) 4))))
                  (delete-region (point-min) (point))
                  (setq head (pai-web-http--parse-head text))
                  (process-put proc 'head head))
              (when (> (buffer-size) pai-web-http-max-header)
                (pai-web-http--fail proc 431 "Header too large")))))
        (when head
          (let ((len (plist-get head :length)))
            (cond
             ((> len pai-web-http-max-body)
              (pai-web-http--fail proc 413 "Body too large"))
             ((>= (buffer-size buf) len)
              (let ((body (with-current-buffer buf
                            (prog1 (buffer-substring-no-properties
                                    (point-min) (+ (point-min) len))
                              (delete-region (point-min) (+ (point-min) len))))))
                (process-put proc 'head nil)
                (pai-web-http--dispatch
                 (append (list :proc proc :body body
                               :peer (car (process-contact proc)))
                         head))
                (setq continue t))))))))))

(defun pai-web-http--parse-head (text)
  "Parse the request head TEXT into (:method :path :query :headers :length)."
  (let* ((lines (split-string text "\r\n"))
         (request (split-string (car lines) " "))
         (method (nth 0 request))
         (target (or (nth 1 request) "/"))
         (qpos (string-search "?" target))
         (path (decode-coding-string
                (url-unhex-string (substring target 0 qpos)) 'utf-8))
         (query (and qpos (pai-web-http-parse-query (substring target (1+ qpos)))))
         (headers nil))
    (unless (and method (string-match-p "\\`[A-Z]+\\'" method))
      (error "Malformed request line"))
    (dolist (line (cdr lines))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
        (push (cons (downcase (match-string 1 line)) (match-string 2 line)) headers)))
    (let ((len (string-to-number (or (cdr (assoc "content-length" headers)) "0"))))
      (list :method method :path path :query query :headers (nreverse headers)
            :length (max 0 len)))))

(defun pai-web-http-parse-query (string)
  "Parse the query STRING (or a form body) into an alist of strings."
  (let (out)
    (dolist (pair (split-string string "&" t))
      (let* ((eq (string-search "=" pair))
             (k (if eq (substring pair 0 eq) pair))
             (v (if eq (substring pair (1+ eq)) "")))
        (push (cons (pai-web-http--unescape k) (pai-web-http--unescape v)) out)))
    (nreverse out)))

(defun pai-web-http--unescape (string)
  "Decode a URL-encoded STRING (with `+' as space) as UTF-8."
  (decode-coding-string
   (url-unhex-string (replace-regexp-in-string "+" " " string t t)) 'utf-8))

(defun pai-web-http--dispatch (req)
  "Hand REQ to `pai-web-http-handler', answering 500 when it fails."
  (condition-case err
      (if pai-web-http-handler
          (funcall pai-web-http-handler req)
        (pai-web-http-respond req 503 :body "No handler"))
    (error
     (message "pai-web: handler error on %s: %s" (plist-get req :path)
              (error-message-string err))
     (ignore-errors
       (pai-web-http-respond req 500 :type "text/plain; charset=utf-8"
                             :body (format "Error: %s" (error-message-string err)))))))

;;;; Requests

(defun pai-web-http-header (req name)
  "Return header NAME (lower case) of REQ, or nil."
  (cdr (assoc name (plist-get req :headers))))

(defun pai-web-http-query (req name)
  "Return query parameter NAME of REQ, or nil."
  (cdr (assoc name (plist-get req :query))))

(defun pai-web-http-cookie (req name)
  "Return the value of cookie NAME sent with REQ, or nil."
  (let ((header (pai-web-http-header req "cookie")))
    (when header
      (seq-some (lambda (part)
                  (let ((part (string-trim part)))
                    (and (string-prefix-p (concat name "=") part)
                         (substring part (1+ (length name))))))
                (split-string header ";")))))

;;;; Responses

(cl-defun pai-web-http-respond (req status &key body (type "application/json; charset=utf-8")
                                    headers close)
  "Answer REQ with STATUS, BODY (string) of content TYPE and extra HEADERS.
HEADERS is an alist of (NAME . VALUE).  With CLOSE, close the connection
afterwards.  The body must fit in `pai-web-http-max-response' bytes.
Returns nil when the connection is gone."
  (let ((proc (plist-get req :proc)))
    (when (and proc (process-live-p proc))
      (process-put proc 'held nil)
      (let* ((body (or body ""))
             (bytes (if (multibyte-string-p body) (encode-coding-string body 'utf-8) body)))
        (when (> (length bytes) pai-web-http-max-response)
          (error "Response too large for one write (%d bytes)" (length bytes)))
        (let ((head (concat
                     (format "HTTP/1.1 %d %s\r\n" status
                             (or (cdr (assq status pai-web-http--reasons)) "Status"))
                     (format "Content-Type: %s\r\n" type)
                     (format "Content-Length: %d\r\n" (length bytes))
                     "Cache-Control: no-store\r\n"
                     "X-Content-Type-Options: nosniff\r\n"
                     "Referrer-Policy: no-referrer\r\n"
                     (if close "Connection: close\r\n" "Connection: keep-alive\r\n")
                     (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h)))
                                headers "")
                     "\r\n")))
          (process-put proc 'last (float-time))
          (process-send-string proc (concat (encode-coding-string head 'utf-8) bytes))
          (when close (ignore-errors (delete-process proc)))
          t)))))

(defun pai-web-http--fail (proc status message)
  "Answer PROC with STATUS and MESSAGE, then close it."
  (process-put proc 'head nil)
  (pai-web-http-respond (list :proc proc) status
                        :type "text/plain; charset=utf-8" :body message :close t))

(defun pai-web-http-hold (req &optional on-close)
  "Keep REQ's connection open for a later answer (long polling).
ON-CLOSE, a function of the process, runs if the client goes away first."
  (let ((proc (plist-get req :proc)))
    (process-put proc 'held t)
    (process-put proc 'on-close on-close)))

(provide 'pai-web-http)
;;; pai-web-http.el ends here
