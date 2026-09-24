;;; pai-dap-client.el --- DAP protocol client for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; A Debug Adapter Protocol client, ported from oh-my-pi's `dap/client.ts'.
;; Speaks the DAP wire protocol (Content-Length framed JSON) over one of three
;; transports: stdio (default), a unix socket (dlv on Linux), or TCP (js-debug).
;;
;; oh-my-pi is promise/AbortSignal based; Emacs tool calls are synchronous, so
;; this client is driven by a pump loop: `pai-dap-client-request' writes a
;; request and pumps `accept-process-output' until the matching response
;; arrives or the deadline elapses.  Events dispatch to handlers as they are
;; parsed inside the process filter, so pre-subscribed waiters (stopped,
;; initialized) never miss an event that shares a buffer with a response.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(defconst pai-dap-client--default-timeout 30.0
  "Default per-request timeout in seconds.")

(defconst pai-dap-client--socket-ready-timeout 10.0
  "Seconds to wait for a socket/tcp adapter to become reachable.")

(cl-defstruct (pai-dap-client (:constructor pai-dap-client--make))
  adapter          ; resolved adapter plist
  cwd
  proc             ; adapter OS process (make-process)
  transport        ; process the DAP bytes flow over (proc for stdio, net proc otherwise)
  port             ; TCP port for child sessions (tcp mode)
  (seq 0)
  (pending (make-hash-table :test 'eql))  ; seq -> slot vector [state body errmsg]
  (buffer (unibyte-string))               ; unparsed incoming bytes
  capabilities
  (event-handlers (make-hash-table :test 'equal)) ; event name -> list of fns
  (reverse-handlers (make-hash-table :test 'equal))
  (disposed nil)
  (adapter-exited nil))

;;;; JSON helpers

(defconst pai-dap-client--empty-object (make-hash-table :test 'equal)
  "Encodes as an empty JSON object {}.")

(defun pai-dap-client--encode (message)
  "Encode MESSAGE plist to a JSON string."
  (let ((json-encoding-pretty-print nil)
        (json-false :json-false)
        (json-null nil))
    (json-encode message)))

(defun pai-dap-client--decode (text)
  "Decode JSON TEXT to a plist tree."
  (let ((json-object-type 'plist)
        (json-key-type 'keyword)
        (json-array-type 'vector)
        (json-false :json-false)
        (json-null nil))
    (json-read-from-string text)))

;;;; Framing / reader

(defun pai-dap-client--filter (client chunk)
  "Append CHUNK (unibyte) to CLIENT's buffer and dispatch complete messages."
  (setf (pai-dap-client-buffer client)
        (concat (pai-dap-client-buffer client) chunk))
  (pai-dap-client--drain client))

(defun pai-dap-client--drain (client)
  "Parse and dispatch every complete framed message buffered in CLIENT."
  (catch 'incomplete
    (while t
      (let* ((buf (pai-dap-client-buffer client))
             (sep (string-search "\r\n\r\n" buf)))
        (unless sep (throw 'incomplete nil))
        (let* ((header (substring buf 0 sep))
               (len nil))
          (dolist (line (split-string header "\r\n" t))
            (when (string-match "\\`[Cc]ontent-[Ll]ength:[ \t]*\\([0-9]+\\)" line)
              (setq len (string-to-number (match-string 1 line)))))
          (if (null len)
              ;; Junk header block: drop past it and resync.
              (setf (pai-dap-client-buffer client) (substring buf (+ sep 4)))
            (let ((body-start (+ sep 4)))
              (when (< (- (length buf) body-start) len)
                (throw 'incomplete nil))
              (let ((body (substring buf body-start (+ body-start len))))
                (setf (pai-dap-client-buffer client) (substring buf (+ body-start len)))
                (condition-case err
                    (pai-dap-client--handle
                     client (pai-dap-client--decode (decode-coding-string body 'utf-8)))
                  (error
                   (message "pai-dap: message handling failed: %s"
                            (error-message-string err))))))))))))

(defun pai-dap-client--handle (client message)
  "Dispatch a decoded DAP MESSAGE for CLIENT."
  (pcase (plist-get message :type)
    ("response" (pai-dap-client--handle-response client message))
    ("event" (pai-dap-client--dispatch-event client message))
    ("request" (pai-dap-client--handle-reverse client message))))

(defun pai-dap-client--handle-response (client message)
  "Resolve the pending request matching response MESSAGE."
  (let* ((seq (plist-get message :request_seq))
         (slot (gethash seq (pai-dap-client-pending client))))
    (when slot
      (remhash seq (pai-dap-client-pending client))
      (if (eq (plist-get message :success) t)
          (progn (aset slot 0 'ok) (aset slot 1 (plist-get message :body)))
        (aset slot 0 'err)
        (aset slot 2 (or (plist-get message :message)
                         (format "DAP request %s failed" (plist-get message :command))))))))

(defun pai-dap-client--dispatch-event (client message)
  "Run handlers for event MESSAGE on CLIENT."
  (let ((event (plist-get message :event))
        (body (plist-get message :body)))
    (dolist (handler (gethash event (pai-dap-client-event-handlers client)))
      (condition-case err
          (funcall handler body)
        (error (message "pai-dap: event handler for %s failed: %s"
                        event (error-message-string err)))))))

(defun pai-dap-client--handle-reverse (client message)
  "Answer an adapter-initiated reverse request MESSAGE."
  (let* ((command (plist-get message :command))
         (handler (gethash command (pai-dap-client-reverse-handlers client))))
    (condition-case err
        (if handler
            (pai-dap-client--send-response
             client message t (funcall handler (plist-get message :arguments)))
          (pai-dap-client--send-response
           client message :json-false
           (list :error (list :id 1 :format (format "Unsupported DAP request: %s" command)))
           (format "Unsupported DAP request: %s" command)))
      (error
       (pai-dap-client--send-response
        client message :json-false
        (list :error (list :id 1 :format (error-message-string err)))
        (error-message-string err))))))

;;;; Writing

(defun pai-dap-client--raw-write (client text)
  "Frame and write TEXT (a JSON string) to CLIENT's transport."
  (let* ((bytes (encode-coding-string text 'utf-8))
         (frame (concat (format "Content-Length: %d\r\n\r\n" (length bytes)) bytes))
         (proc (pai-dap-client-transport client)))
    (process-send-string proc frame)))

(defun pai-dap-client--send-response (client request success &optional body message)
  "Send a response to reverse REQUEST with SUCCESS, BODY, MESSAGE."
  (let ((resp (list :seq (cl-incf (pai-dap-client-seq client))
                    :type "response"
                    :request_seq (plist-get request :seq)
                    :success success
                    :command (plist-get request :command))))
    (when message (setq resp (append resp (list :message message))))
    (when body (setq resp (append resp (list :body body))))
    (pai-dap-client--raw-write client (pai-dap-client--encode resp))))

;;;; Liveness / pump

(defun pai-dap-client-live-p (client)
  "Return non-nil if CLIENT's adapter process is alive and not disposed."
  (and (not (pai-dap-client-disposed client))
       (let ((proc (pai-dap-client-proc client)))
         (and proc (process-live-p proc)))))

(defun pai-dap-client-pump (client predicate deadline)
  "Pump CLIENT's transport until PREDICATE returns non-nil or DEADLINE (abs time).
Return non-nil if PREDICATE was satisfied."
  (let ((proc (pai-dap-client-transport client)))
    (catch 'done
      (while t
        (when (funcall predicate) (throw 'done t))
        (when (>= (float-time) deadline) (throw 'done nil))
        (unless (pai-dap-client-live-p client)
          ;; Drain any final buffered bytes, then stop.
          (accept-process-output proc 0.02)
          (throw 'done (funcall predicate)))
        (accept-process-output proc 0.05 nil t)))))

;;;; Requests

(defun pai-dap-client-request (client command &optional args timeout)
  "Send COMMAND with ARGS to CLIENT and synchronously wait for the response.
Return the response body, or signal an error on failure/timeout.  TIMEOUT is in
seconds (default `pai-dap-client--default-timeout')."
  (unless (pai-dap-client-live-p client)
    (error "DAP adapter %s is not running" (plist-get (pai-dap-client-adapter client) :name)))
  (let* ((timeout (or timeout pai-dap-client--default-timeout))
         (seq (cl-incf (pai-dap-client-seq client)))
         (slot (vector 'pending nil nil))
         (deadline (+ (float-time) timeout)))
    (puthash seq slot (pai-dap-client-pending client))
    (pai-dap-client--raw-write
     client (pai-dap-client--encode
             (list :seq seq :type "request" :command command
                   :arguments (or args pai-dap-client--empty-object))))
    (pai-dap-client-pump client (lambda () (not (eq (aref slot 0) 'pending))) deadline)
    (pcase (aref slot 0)
      ('ok (aref slot 1))
      ('err (error "%s" (aref slot 2)))
      (_ (remhash seq (pai-dap-client-pending client))
         (error "DAP request %s timed out after %ss" command timeout)))))

(defun pai-dap-client-send-async (client command &optional args)
  "Write COMMAND with ARGS to CLIENT and return (SEQ . SLOT) without waiting.
Await the result later with `pai-dap-client-await'.  Used for launch/attach,
whose response many adapters defer until after the configuration handshake."
  (unless (pai-dap-client-live-p client)
    (error "DAP adapter %s is not running" (plist-get (pai-dap-client-adapter client) :name)))
  (let ((seq (cl-incf (pai-dap-client-seq client)))
        (slot (vector 'pending nil nil)))
    (puthash seq slot (pai-dap-client-pending client))
    (pai-dap-client--raw-write
     client (pai-dap-client--encode
             (list :seq seq :type "request" :command command
                   :arguments (or args pai-dap-client--empty-object))))
    (cons seq slot)))

(defun pai-dap-client-slot-settled-p (slot)
  "Return non-nil if async request SLOT has settled."
  (not (eq (aref slot 0) 'pending)))

(defun pai-dap-client-await (client seq slot command timeout)
  "Pump CLIENT until async SLOT (with SEQ) settles or TIMEOUT elapses.
Return the body, or signal COMMAND's error/timeout."
  (let ((deadline (+ (float-time) (or timeout pai-dap-client--default-timeout))))
    (pai-dap-client-pump client (lambda () (pai-dap-client-slot-settled-p slot)) deadline)
    (pcase (aref slot 0)
      ('ok (aref slot 1))
      ('err (error "%s" (aref slot 2)))
      (_ (remhash seq (pai-dap-client-pending client))
         (error "DAP request %s timed out after %ss" command timeout)))))

(defun pai-dap-client-notify (client command &optional args)
  "Fire-and-forget: send COMMAND with ARGS to CLIENT, no response wait."
  (when (pai-dap-client-live-p client)
    (let ((seq (cl-incf (pai-dap-client-seq client))))
      (pai-dap-client--raw-write
       client (pai-dap-client--encode
               (list :seq seq :type "request" :command command
                     :arguments (or args pai-dap-client--empty-object)))))))

;;;; Events

(defun pai-dap-client-on-event (client event handler)
  "Register HANDLER (called with the event body) for EVENT on CLIENT."
  (push handler (gethash event (pai-dap-client-event-handlers client))))

(defun pai-dap-client-on-reverse-request (client command handler)
  "Register HANDLER (called with request arguments) for reverse COMMAND."
  (puthash command handler (pai-dap-client-reverse-handlers client)))

(defun pai-dap-client-initialize (client args &optional timeout)
  "Send the initialize request with ARGS and record CLIENT capabilities."
  (let ((caps (pai-dap-client-request client "initialize" args timeout)))
    (setf (pai-dap-client-capabilities client) (or caps '()))
    (pai-dap-client-capabilities client)))

;;;; Spawn / transports

(defun pai-dap-client--non-interactive-env ()
  "Environment entries that keep the debuggee away from a controlling TTY."
  (append (list "PAGER=cat" "GIT_PAGER=cat" "TERM=dumb"
                "DEBUGINFOD_URLS=" "NODE_NO_READLINE=1")
          process-environment))

(defun pai-dap-client--start-process (name command args cwd)
  "Start adapter process NAME running COMMAND ARGS in CWD.
Emacs subprocesses have no controlling terminal, so no `setsid' is needed;
wrapping in `setsid' additionally breaks some adapters (e.g. debugpy)."
  (let* ((default-directory (file-name-as-directory cwd))
         (process-environment (pai-dap-client--non-interactive-env)))
    (make-process
     :name name
     :command (cons command args)
     :coding 'binary
     :connection-type 'pipe
     :noquery t
     :stderr (get-buffer-create (format " *%s-stderr*" name)))))

(defun pai-dap-client--attach-stdio-filter (client)
  "Attach the framing filter to CLIENT's stdio adapter process."
  (let ((proc (pai-dap-client-proc client)))
    (set-process-filter proc (lambda (_p chunk) (pai-dap-client--filter client chunk)))
    (set-process-sentinel
     proc (lambda (_p _e) (unless (process-live-p proc)
                            (setf (pai-dap-client-adapter-exited client) t))))))

(defun pai-dap-client-spawn (adapter cwd &optional socket-ready-timeout)
  "Spawn ADAPTER in CWD and return a connected `pai-dap-client'.
Dispatches on ADAPTER's :connect-mode (stdio, socket, or tcp)."
  (pcase (plist-get adapter :connect-mode)
    ('socket (pai-dap-client--spawn-socket adapter cwd socket-ready-timeout))
    ('tcp (pai-dap-client--spawn-tcp adapter cwd socket-ready-timeout))
    (_ (pai-dap-client--spawn-stdio adapter cwd))))

(defun pai-dap-client--spawn-stdio (adapter cwd)
  "Spawn ADAPTER over stdio in CWD."
  (let* ((name (format "pai-dap-%s" (plist-get adapter :name)))
         (proc (pai-dap-client--start-process
                name (plist-get adapter :resolved-command) (plist-get adapter :args) cwd))
         (client (pai-dap-client--make :adapter adapter :cwd cwd :proc proc :transport proc)))
    (pai-dap-client--attach-stdio-filter client)
    client))

(defun pai-dap-client--wait-file (path deadline)
  "Poll until PATH exists or DEADLINE passes.  Return non-nil on success."
  (catch 'ok
    (while (< (float-time) deadline)
      (when (file-exists-p path) (throw 'ok t))
      (sleep-for 0.03))
    (file-exists-p path)))

(defun pai-dap-client--spawn-socket (adapter cwd socket-ready-timeout)
  "Spawn socket-mode ADAPTER (e.g. dlv) in CWD via a unix domain socket."
  (let* ((timeout (or socket-ready-timeout pai-dap-client--socket-ready-timeout))
         (sock (format "/tmp/pai-dap-%s-%d-%d.sock"
                       (plist-get adapter :name) (emacs-pid) (random 100000)))
         (name (format "pai-dap-%s" (plist-get adapter :name)))
         (proc (pai-dap-client--start-process
                name (plist-get adapter :resolved-command)
                (append (plist-get adapter :args) (list (format "--listen=unix:%s" sock)))
                cwd)))
    (condition-case err
        (progn
          (unless (pai-dap-client--wait-file sock (+ (float-time) timeout))
            (error "dap adapter %s did not create its socket" (plist-get adapter :name)))
          (let* ((client (pai-dap-client--make :adapter adapter :cwd cwd :proc proc))
                 (net (make-network-process
                       :name (format "%s-sock" name)
                       :family 'local :service sock :coding 'binary :noquery t
                       :filter (lambda (_p chunk) (pai-dap-client--filter client chunk)))))
            (setf (pai-dap-client-transport client) net)
            (set-process-sentinel
             proc (lambda (_p _e) (unless (process-live-p proc)
                                    (setf (pai-dap-client-adapter-exited client) t))))
            client))
      (error (ignore-errors (delete-process proc)) (signal (car err) (cdr err))))))

(defun pai-dap-client--free-tcp-port ()
  "Reserve and immediately release a free TCP port; return its number."
  (let* ((server (make-network-process
                  :name "pai-dap-portprobe" :server t :host "127.0.0.1"
                  :service t :family 'ipv4 :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defun pai-dap-client--spawn-tcp (adapter cwd socket-ready-timeout)
  "Spawn tcp-mode ADAPTER (e.g. js-debug) in CWD, connecting to its TCP port."
  (let* ((timeout (or socket-ready-timeout pai-dap-client--socket-ready-timeout))
         (port (pai-dap-client--free-tcp-port))
         (args (mapcar (lambda (a) (if (equal a "${port}") (number-to-string port) a))
                       (plist-get adapter :args)))
         (name (format "pai-dap-%s" (plist-get adapter :name)))
         (proc (pai-dap-client--start-process
                name (plist-get adapter :resolved-command) args cwd)))
    (condition-case err
        (let* ((client (pai-dap-client--make :adapter adapter :cwd cwd :proc proc :port port))
               (deadline (+ (float-time) timeout))
               (net nil))
          (while (and (not net) (< (float-time) deadline) (process-live-p proc))
            (condition-case nil
                (setq net (make-network-process
                           :name (format "%s-tcp" name)
                           :host "127.0.0.1" :service port :family 'ipv4 :coding 'binary :noquery t
                           :filter (lambda (_p chunk) (pai-dap-client--filter client chunk))))
              (error (sleep-for 0.05))))
          (unless net (error "could not connect to %s on port %d" (plist-get adapter :name) port))
          (setf (pai-dap-client-transport client) net)
          (set-process-sentinel
           proc (lambda (_p _e) (unless (process-live-p proc)
                                  (setf (pai-dap-client-adapter-exited client) t))))
          client)
      (error (ignore-errors (delete-process proc)) (signal (car err) (cdr err))))))

(defun pai-dap-client-connect (adapter cwd host port)
  "Connect to an existing DAP server for ADAPTER at HOST:PORT (child sessions)."
  (let* ((name (format "pai-dap-%s-child" (plist-get adapter :name)))
         (client (pai-dap-client--make :adapter adapter :cwd cwd :port port))
         (net (make-network-process
               :name name :host host :service port :family 'ipv4 :coding 'binary :noquery t
               :filter (lambda (_p chunk) (pai-dap-client--filter client chunk)))))
    ;; No owning OS process; treat the socket as the liveness proc.
    (setf (pai-dap-client-proc client) net
          (pai-dap-client-transport client) net)
    client))

(defun pai-dap-client-dispose (client)
  "Terminate CLIENT's transport and adapter process."
  (unless (pai-dap-client-disposed client)
    (setf (pai-dap-client-disposed client) t)
    ;; Fail any in-flight requests.
    (maphash (lambda (seq slot)
               (ignore seq)
               (when (eq (aref slot 0) 'pending)
                 (aset slot 0 'err)
                 (aset slot 2 (format "DAP adapter %s disposed"
                                      (plist-get (pai-dap-client-adapter client) :name)))))
             (pai-dap-client-pending client))
    (let ((net (pai-dap-client-transport client))
          (proc (pai-dap-client-proc client)))
      (when (and net (process-live-p net) (not (eq net proc)))
        (ignore-errors (delete-process net)))
      (when (and proc (process-live-p proc))
        (ignore-errors (delete-process proc))))))

(defun pai-dap-client-peek-stderr (client)
  "Return trimmed stderr text captured from CLIENT's adapter, if any."
  (let ((buf (get-buffer (format " *pai-dap-%s-stderr*" (plist-get (pai-dap-client-adapter client) :name)))))
    (when (buffer-live-p buf)
      (string-trim (with-current-buffer buf (buffer-string))))))

(provide 'pai-dap-client)
;;; pai-dap-client.el ends here
