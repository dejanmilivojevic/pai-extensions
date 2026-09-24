;;; pai-mcp-http.el --- Asynchronous MCP HTTP transport -*- lexical-binding: t; -*-

;;; Commentary:
;; curl owns HTTP framing and TLS; Emacs filters own JSON and SSE delivery.
;; Every request is a detached curl process; responses arrive through filters,
;; so connecting, handshaking and tool calls never block the UI.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'url-expand)
(require 'pai-mcp-client)
(require 'pai-mcp-auth)
(require 'pai-mcp-negotiation)

(defvar pai-mcp-http--headers-timeout 10000
  "Default `requestHeadersCommand' timeout in milliseconds.")

(defvar pai-mcp-http--headers-output-limit 65536
  "Maximum `requestHeadersCommand' stdout bytes accepted.")

(defun pai-mcp-http-stop (name)
  "Cancel transport processes and timers for NAME.
Resource cleanup only: pending callbacks are delivered by the client."
  (let ((server (pai-mcp--server name)))
    (pai-mcp--set server :http-generation (1+ (or (plist-get server :http-generation) 0)))
    (dolist (timer (plist-get server :http-timers))
      (when timer (cancel-timer timer)))
    (pai-mcp--set server :http-timers nil)
    (pai-mcp--set server :http-initializing nil)
    (pai-mcp--set server :http-startup-queue nil)
    (pai-mcp--set server :protocol-on-notification-accepted nil)
    (dolist (proc (plist-get server :http-processes))
      (when (process-live-p proc) (delete-process proc)))
    (pai-mcp--set server :http-processes nil)))

(defun pai-mcp-http--active-p (server generation)
  "Whether GENERATION still owns SERVER's transport."
  (= generation (or (plist-get server :http-generation) 0)))

(defun pai-mcp-http--reject (server object reason)
  "Reject OBJECT on SERVER with structured REASON when a callback exists.
Transport failures without a pending request fail a starting server."
  (let* ((id (plist-get object :id))
         (callback (and id (gethash id (plist-get server :pending)))))
    (cond
     (callback
      (remhash id (plist-get server :pending))
      (funcall callback nil reason))
     ((or (eq (plist-get server :status) 'starting)
          (equal (plist-get object :method) "notifications/initialized"))
      (pai-mcp--fail (plist-get server :name)
                     (pai-mcp-protocol-error-hint reason))))))

(defun pai-mcp-http--headers (server &optional object)
  "Return session and protocol headers derived from SERVER and OBJECT."
  (append (when (plist-get server :http-session-id)
            (list (cons "Mcp-Session-Id" (plist-get server :http-session-id))))
          (when (and (plist-get server :protocol-version)
                     (not (equal (plist-get server :protocol-version)
                                 pai-mcp-protocol-modern-version)))
            (list (cons "MCP-Protocol-Version"
                        (plist-get server :protocol-version))))
          (pai-mcp-protocol-headers server object)))

(defun pai-mcp-http--signer-spec (server method url raw-body)
  "Return the `requestHeadersCommand' spec for one request, or nil.
A string spec runs through the shell; an object spec runs argv directly.
The stdin envelope carries the exact request bytes so callers can bind
signatures; stdout is a direct JSON header map per the upstream contract."
  (let* ((spec (plist-get (plist-get server :def) :requestHeadersCommand))
         (plist (if (stringp spec) (list :command spec) spec))
         (command (plist-get plist :command)))
    (when command
      (list :command command
            :argv (if (plist-get plist :args)
                      (cons (pai-mcp--interpolate command)
                            (mapcar #'pai-mcp--interpolate (plist-get plist :args)))
                    (list shell-file-name shell-command-switch
                          (pai-mcp--interpolate command)))
            :env (plist-get plist :env)
            :timeout (or (plist-get plist :timeoutMs) pai-mcp-http--headers-timeout)
            :stdin (pai-json-encode
                    (list :version 1 :method (upcase method) :url url
                          :bodyBase64 (if (or (not raw-body) (string-empty-p raw-body))
                                          ""
                                        (base64-encode-string
                                         (encode-coding-string raw-body 'utf-8) t))))))))

(defun pai-mcp-http--sse (server proc)
  "Dispatch complete SSE frames buffered on PROC for SERVER."
  (let ((text (process-get proc 'body))
        (generation (or (plist-get server :http-generation) 0)))
    (while (and (pai-mcp-http--active-p server generation)
                (string-match "\r?\n\r?\n" text))
      (let* ((split (match-end 0))
             (frame (substring text 0 (match-beginning 0)))
             (event "message")
             data)
        (setq text (substring text split))
        (dolist (line (split-string frame "\r?\n"))
          (cond ((string-prefix-p "event:" line)
                 (setq event (string-trim (substring line 6))))
                ((string-prefix-p "data:" line)
                 (push (string-remove-prefix " " (substring line 5)) data))))
        (when data
          (let ((value (string-join (nreverse data) "\n")))
            (if (equal event "endpoint")
                (let* ((base (plist-get server :http-url))
                       (target (url-expand-file-name value base))
                       (a (url-generic-parse-url base))
                       (b (url-generic-parse-url target)))
                  (unless (and (equal (url-type a) (url-type b))
                               (equal (url-host a) (url-host b))
                               (equal (url-port a) (url-port b)))
                    (error "cross-origin SSE endpoint rejected"))
                  (unless (plist-get server :http-endpoint)
                    (pai-mcp--set server :http-endpoint target)
                    (let ((retry (plist-get server :http-retry)))
                      (pai-mcp--set server :http-retry nil)
                      (when retry (pai-mcp-http-send server retry)))))
              (pai-mcp--dispatch server (pai-json-decode value)))))))
    (process-put proc 'body text)))

(defun pai-mcp-http--headers-done (server proc status lines)
  "Consume the complete response header block on PROC for SERVER."
  (let ((object (process-get proc 'object))
        (def (plist-get server :def)))
    (process-put proc 'headers t)
    (process-put proc 'status status)
    (dolist (line (cdr lines))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)" line)
        (let ((key (downcase (match-string 1 line)))
              (value (string-trim (match-string 2 line))))
          (cond ((equal key "retry-after")
                 (process-put proc 'retry-after value))
                ((and (< status 300) (equal key "content-type"))
                 (process-put proc 'sse
                              (string-prefix-p "text/event-stream" value)))
                ((and (< status 300) (equal key "mcp-session-id"))
                 (pai-mcp--set server :http-session-id value))))))
    (when (and (memq status '(404 405 406 415))
               (equal (plist-get object :method) "initialize")
               (eq (plist-get server :http-mode) 'streamable)
               (not (plist-get def :httpTransport))
               (not (equal (plist-get def :protocolVersion)
                           pai-mcp-protocol-modern-version)))
      (process-put proc 'handled t)
      (pai-mcp--set server :http-mode 'sse)
      (pai-mcp--set server :http-retry object)
      (pai-mcp-http--launch server "GET" (plist-get server :http-url) nil))))

(defun pai-mcp-http--filter (server proc chunk)
  "Parse response headers and streaming events from CHUNK on PROC."
  (condition-case err
      (progn
        (process-put proc 'body (concat (process-get proc 'body) chunk))
        (unless (process-get proc 'headers)
          (let ((text (process-get proc 'body)))
            (when (string-match "\r?\n\r?\n" text)
              (let* ((split (match-end 0))
                     (head (substring text 0 (match-beginning 0)))
                     (lines (split-string head "\r?\n"))
                     (status (string-to-number
                              (or (cadr (split-string (car lines))) "0"))))
                (process-put proc 'body (substring text split))
                (if (< status 200)
                    (pai-mcp-http--filter server proc "")
                  (pai-mcp-http--headers-done server proc status lines))))))
        (when (and (process-get proc 'sse) (not (process-get proc 'handled)))
          (pai-mcp-http--sse server proc)))
    (error (process-put proc 'handled t)
           (pai-mcp-http--reject
            server (process-get proc 'object)
            (list :kind 'shape :message (error-message-string err))))))

(defun pai-mcp-http--settled (server proc)
  "Finish one curl response on PROC for SERVER."
  (condition-case failure
      (let ((object (process-get proc 'object))
            (status (process-get proc 'status))
            (generation (or (plist-get server :http-generation) 0)))
        (cond
         ((and status (>= status 300))
          (pai-mcp-http--reject
           server object
           (list :kind 'http :status status
                 :retryAfter (process-get proc 'retry-after)
                 :body (process-get proc 'body)
                 :message (format "HTTP %d%s" status
                                  (if (= status 503)
                                      ": server temporarily unavailable" "")))))
         ((not (zerop (process-exit-status proc)))
          (pai-mcp-http--reject
           server object
           (list :kind 'network
                 :message (format "curl failed: %s"
                                  (with-current-buffer (process-get proc 'stderr)
                                    (string-trim (buffer-string)))))))
         ((not (process-get proc 'headers))
          (error "HTTP response has no headers"))
         (t
          (cond
           ((and (not (process-get proc 'sse))
                 (not (string-empty-p (process-get proc 'body))))
            (pai-mcp--dispatch server (pai-json-decode (process-get proc 'body))))
           ((and object (plist-member object :id)
                 (not (eq (plist-get server :http-mode) 'sse))
                 (gethash (plist-get object :id) (plist-get server :pending)))
            (error "HTTP response closed without a JSON-RPC result")))
          (when (and (pai-mcp-http--active-p server generation)
                     (equal (plist-get object :method) "notifications/initialized"))
            (let ((queued (plist-get server :http-startup-queue))
                  (accepted (plist-get server :protocol-on-notification-accepted)))
              (pai-mcp--set server :http-initializing nil)
              (pai-mcp--set server :http-startup-queue nil)
              (pai-mcp--set server :protocol-on-notification-accepted nil)
              (when accepted (funcall accepted))
              (dolist (request queued)
                (when (pai-mcp-http--active-p server generation)
                  (pai-mcp-http-send server request))))))))
    (error (pai-mcp-http--reject
            server (process-get proc 'object)
            (list :kind 'shape :message (error-message-string failure))))))

(defun pai-mcp-http--merge-headers (&rest layers)
  "Merge header LAYERS into one alist, case-insensitive, last layer wins."
  (let ((by-name (make-hash-table :test 'equal))
        (order '())
        out)
    (dolist (layer layers)
      (dolist (h layer)
        (let ((key (downcase (car h))))
          (unless (gethash key by-name)
            (push key order))
          (puthash key h by-name))))
    (dolist (key order)
      (push (gethash key by-name) out))
    out))

(defun pai-mcp-http--curl (server method url raw-body extra &optional object)
  "Resolve auth for every request, then send unchanged RAW-BODY to URL.
EXTRA contains signer headers and remains the final, winning header layer."
  (let ((generation (or (plist-get server :http-generation) 0)))
    (pai-mcp-auth-headers
     (plist-get server :name) (plist-get server :def) url
     (lambda (headers)
       (when (pai-mcp-http--active-p server generation)
         (pai-mcp-http--curl-authorized
          server method url raw-body headers extra object)))
     (lambda (message)
       (when (pai-mcp-http--active-p server generation)
         (pai-mcp-http--reject server object (list :kind 'auth :message message))))
     (plist-get server :directory))))

(defun pai-mcp-http--curl-authorized (server method url raw-body auth extra object)
  "Run curl for METHOD/URL with RAW-BODY, AUTH and final signer EXTRA."
  (condition-case err
      (let* ((generation (or (plist-get server :http-generation) 0))
             (body (if raw-body (encode-coding-string raw-body 'utf-8) ""))
             (stderr (generate-new-buffer " *pai-mcp-http-stderr*"))
             (headers (pai-mcp-http--merge-headers
                       (list (cons "Content-Type" "application/json")
                             (cons "Accept" "application/json, text/event-stream")
                             (cons "Expect" ""))
                       (plist-get server :http-headers)
                       auth
                       (pai-mcp-http--headers server object)
                       extra))
             (args (append (list "curl" "--silent" "--show-error" "--no-buffer"
                                 "--include" "--request" method "--url" url)
                           (when (plist-get server :http-ca)
                             (list "--cacert" (plist-get server :http-ca)))
                           (apply #'append
                                  (mapcar (lambda (h)
                                            (list "--header"
                                                  (concat (car h) ": " (cdr h))))
                                          headers))
                           (when raw-body '("--data-binary" "@-"))))
             (proc (make-process
                    :name "pai-mcp-http" :command args :connection-type 'pipe
                    :coding '(utf-8-unix . no-conversion) :noquery t :stderr stderr
                    :filter (lambda (p chunk)
                              (when (pai-mcp-http--active-p server generation)
                                (pai-mcp-http--filter server p chunk)))
                    :sentinel (lambda (p _event)
                                (when (memq (process-status p) '(exit signal))
                                  (unwind-protect
                                      (when (and (pai-mcp-http--active-p
                                                  server generation)
                                                 (not (process-get p 'handled)))
                                        (pai-mcp-http--settled server p))
                                    (when (buffer-live-p stderr)
                                      (kill-buffer stderr))
                                    (pai-mcp--set
                                     server :http-processes
                                     (delq p (plist-get server :http-processes)))))))))
        (process-put proc 'stderr stderr)
        (process-put proc 'body "")
        (process-put proc 'object object)
        (push proc (plist-get server :http-processes))
        (when raw-body (process-send-string proc body))
        (process-send-eof proc))
    (error (pai-mcp-http--reject
            server object (list :kind 'network :message (error-message-string err))))))

(defun pai-mcp-http--launch (server method url object)
  "Send METHOD/URL/OBJECT asynchronously, honoring the optional signer."
  (let* ((raw-body (and object (pai-json-encode object)))
         (spec (pai-mcp-http--signer-spec server method url raw-body)))
    (if (not spec)
        (pai-mcp-http--curl server method url raw-body nil object)
      (let ((stderr (generate-new-buffer " *pai-mcp-http-signer*"))
            (generation (or (plist-get server :http-generation) 0)))
        (condition-case err
            (let ((proc
                   (make-process
                    :name "pai-mcp-http-signer" :noquery t
                    :connection-type 'pipe :coding 'utf-8-unix :stderr stderr
                    :command (plist-get spec :argv)
                    :filter (lambda (p chunk)
                              (let ((buf (process-get p 'out)))
                                (when (< (length buf)
                                         pai-mcp-http--headers-output-limit)
                                  (process-put p 'out (concat buf chunk)))))
                    :sentinel
                    (lambda (p _event)
                      (when (memq (process-status p) '(exit signal))
                        (unless (process-get p 'signer-timeout)
                          (unwind-protect
                              (when (pai-mcp-http--active-p server generation)
                                (pai-mcp-http--signer-settled
                                 server method url raw-body object p stderr))
                            (when (buffer-live-p stderr)
                              (kill-buffer stderr)))))))))
              (process-put proc 'out "")
              (push proc (plist-get server :http-processes))
              (process-send-string proc (plist-get spec :stdin))
              (process-send-eof proc)
              (let ((timeout (plist-get spec :timeout)))
                (push (run-at-time (/ timeout 1000.0) nil
                                   (lambda (p)
                                     (when (and (pai-mcp-http--active-p server generation)
                                                (process-live-p p))
                                       (process-put p 'signer-timeout t)
                                       (delete-process p)
                                       (when (buffer-live-p stderr) (kill-buffer stderr))
                                       (pai-mcp-http--reject
                                        server object "signer timed out")))
                                   proc)
                      (plist-get server :http-timers))))
          (error (when (buffer-live-p stderr) (kill-buffer stderr))
                 (pai-mcp-http--reject server object
                                       (error-message-string err))))))))

(defun pai-mcp-http--signer-settled (server method url raw-body object proc stderr)
  "Validate signer output on PROC and launch SERVER's decorated request."
  (condition-case reason
      (progn
        (when (not (zerop (process-exit-status proc)))
          (error "signer failed: %s"
                 (with-current-buffer stderr (string-trim (buffer-string)))))
        (when (> (length (process-get proc 'out)) pai-mcp-http--headers-output-limit)
          (error "signer output exceeds 64KiB"))
        (when (string-empty-p (string-trim (process-get proc 'out)))
          (error "signer produced no output"))
        (let ((json (json-parse-string (process-get proc 'out)
                                       :object-type 'hash-table
                                       :array-type 'array))
              out)
          (unless (hash-table-p json)
            (error "signer output is not a JSON object"))
          (maphash
           (lambda (key value)
             (unless (stringp value)
               (error "signer header %s is not a string" key))
             (push (cons key value) out))
           json)
          (pai-mcp-http--curl server method url raw-body out object)))
    (error (pai-mcp-http--reject server object (error-message-string reason)))))

(defun pai-mcp-http-send (server object)
  "Send JSON-RPC OBJECT through SERVER without blocking.
Hold startup traffic until the initialized notification is accepted."
  (cond
   ((and (eq (plist-get server :http-mode) 'sse)
         (not (plist-get server :http-endpoint)))
    (pai-mcp--set server :http-retry object))
   ((plist-get server :http-initializing)
    (pai-mcp--set server :http-startup-queue
                  (nconc (plist-get server :http-startup-queue) (list object))))
   (t
    (when (equal (plist-get object :method) "notifications/initialized")
      (pai-mcp--set server :http-initializing t))
    (pai-mcp-http--launch server "POST"
                          (or (plist-get server :http-endpoint)
                              (plist-get server :http-url))
                          object))))

(defun pai-mcp-http--begin (server headers)
  "Install resolved HEADERS on SERVER and start its handshake."
  (pai-mcp--set server :http-headers headers)
  (when (eq (plist-get server :http-mode) 'sse)
    (pai-mcp-http--launch server "GET" (plist-get server :http-url) nil))
  (pai-mcp--handshake (plist-get server :name)))


(defun pai-mcp-http--secret-process (server generation command stderr on-resolve)
  "Run SERVER's `!' header helper COMMAND asynchronously.
ON-RESOLVE receives the resolved secret value once the helper exits."
  (make-process
   :name "pai-mcp-http-secret" :noquery t
   :connection-type 'pipe :coding 'utf-8-unix :stderr stderr
   :command (list shell-file-name shell-command-switch command)
   :filter (lambda (p chunk)
             (process-put p 'out (concat (process-get p 'out) chunk)))
   :sentinel (lambda (p _event)
               (when (memq (process-status p) '(exit signal))
                 (unwind-protect
                     (when (pai-mcp-http--active-p server generation)
                       (let ((settled (process-get p 'settled)))
                         (unless settled
                           (process-put p 'settled t)
                           (funcall on-resolve p))))
                   (when (buffer-live-p stderr)
                     (kill-buffer stderr)))))))


(defun pai-mcp-http--with-secrets (server plain secrets)
  "Resolve SECRET header commands in SECRETS asynchronously.
PLAIN are the literal headers; every helper must resolve before the
handshake begins on SERVER."
  (if (not secrets)
      (pai-mcp-http--begin server plain)
    (let ((pending (length secrets))
          (resolved '())
          (generation (or (plist-get server :http-generation) 0)))
      (dolist (secret secrets)
        (let* ((key (car secret))
               (value (cdr secret))
               (command (substring value 1))
               (stderr (generate-new-buffer " *pai-mcp-http-secret*"))
               (helper
                (pai-mcp-http--secret-process
                 server generation command stderr
                 (lambda (proc)
                   (let ((value (string-trim (process-get proc 'out))))
                     (cond
                      ((not (zerop (process-exit-status proc)))
                       (pai-mcp--fail
                        (plist-get server :name)
                        (format "header %s: %s" key
                                (with-current-buffer stderr
                                  (string-trim (buffer-string))))))
                      ((string-empty-p value)
                       (pai-mcp--fail
                        (plist-get server :name)
                        (format "header %s: command produced an empty value" key)))
                      (t
                       (push (cons key value) resolved)
                       (setq pending (1- pending))
                       (when (zerop pending)
                         (pai-mcp-http--begin
                          server (append plain resolved))))))))))
          (process-put helper 'out "")
          (push helper (plist-get server :http-processes)))))))

(defun pai-mcp-http-start (name def)
  "Connect NAME from HTTP DEF and begin the asynchronous handshake."
  (let* ((server (pai-mcp--server name))
         (url (pai-mcp--interpolate (plist-get def :url)))
         (parsed (url-generic-parse-url url))
         (ca (plist-get def :caFile))
         (pairs (mapcar (lambda (pair)
                          (cons (car pair) (pai-mcp--interpolate (cdr pair))))
                        (pai-mcp--plist-to-alist (plist-get def :headers)))))
    (unless (and (member (url-type parsed) '("http" "https")) (url-host parsed))
      (error "Invalid MCP HTTP URL"))
    (when (string-match-p "\\${\\|\\$env:" url)
      (error "URL has unresolved environment variables"))
    (when ca
      (setq ca (expand-file-name (pai-mcp--interpolate ca)))
      (unless (and (equal (url-type parsed) "https") (file-readable-p ca))
        (error "caFile requires HTTPS and a readable certificate bundle")))
    (pai-mcp--set server :http-url url)
    (pai-mcp--set server :http-ca ca)
    (pai-mcp--set server :http-mode
                  (if (equal (plist-get def :httpTransport) "sse") 'sse 'streamable))
    (pai-mcp--set server :http-session-id nil)
    (pai-mcp--set server :protocol-version nil)
    (pai-mcp--set server :http-endpoint nil)
    (pai-mcp--set server :http-retry nil)
    (let (plain secrets)
      (dolist (pair pairs)
        (let ((value (cdr pair)))
          (cond
           ((string-prefix-p "!!" value)
            (push (cons (car pair) (substring value 1)) plain))
           ((string-prefix-p "!" value) (push pair secrets))
           (t (push pair plain)))))
      (pai-mcp-http--with-secrets server (nreverse plain) (nreverse secrets)))))

(provide 'pai-mcp-http)
;;; pai-mcp-http.el ends here
