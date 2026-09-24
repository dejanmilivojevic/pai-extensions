;;; pai-mcp-negotiation.el --- MCP protocol era negotiation -*- lexical-binding: t; -*-

;;; Commentary:
;; SDK 2.0.0 semantics: legacy initialize by default; auto and pin discover
;; 2026-07-28 via a disposable stdio sibling or the live HTTP connection.
;; Sources: @modelcontextprotocol/client@2.0.0/dist/index.mjs (probeClassifier,
;; versionNegotiation, _applyBodyDerivedHeaders), and its src-D_zzAWoS.mjs
;; wire codec, available at https://unpkg.com/@modelcontextprotocol/client@2.0.0/.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-core)
(require 'pai-mcp-config)

(defvar pai-mcp-protocol-version)
(defvar pai-mcp-startup-timeout)
(declare-function pai-mcp--set "pai-mcp-client" (server key value))
(declare-function pai-mcp--request "pai-mcp-client" (server method params callback))
(declare-function pai-mcp--notify "pai-mcp-client" (server method &optional params))

(defconst pai-mcp-protocol-modern-version "2026-07-28")

(defun pai-mcp-protocol--client-info ()
  "Return this client's implementation identity."
  (list :name "pai" :version (or (bound-and-true-p pai-version) "0")))

(defun pai-mcp-protocol--meta (server)
  "Return the modern request envelope for SERVER."
  (list :io.modelcontextprotocol/protocolVersion pai-mcp-protocol-modern-version
        :io.modelcontextprotocol/clientInfo (pai-mcp-protocol--client-info)
        :io.modelcontextprotocol/clientCapabilities
        (or (plist-get server :client-capabilities) (pai-json-empty-object))))

(defun pai-mcp-protocol--copy-object (object)
  "Shallow-copy JSON OBJECT into a plist, preserving value identities."
  (if (hash-table-p object)
      (let (out)
        (maphash (lambda (key value)
                   (setq out (plist-put out (if (keywordp key) key
                                             (intern (concat ":" key))) value)))
                 object)
        out)
    (copy-sequence object)))

(defun pai-mcp-protocol-decorate (server object)
  "Return outgoing JSON-RPC OBJECT with SERVER's modern envelope, if needed.
Call from the common send path, for requests AND notifications, not responses.
Existing caller _meta keys take precedence, as in SDK 2.0.  Legacy traffic is
returned unchanged.  No argument or nested caller object is mutated."
  (if (and (plist-get object :method)
           (or (plist-get server :protocol-probing)
               (equal (plist-get server :protocol-version)
                      pai-mcp-protocol-modern-version)))
      (let* ((out (copy-sequence object))
             (params (pai-mcp-protocol--copy-object (plist-get object :params)))
             (meta (pai-mcp-protocol--copy-object (plist-get params :_meta)))
             (defaults (pai-mcp-protocol--meta server)))
        (while defaults
          (unless (plist-member meta (car defaults))
            (setq meta (plist-put meta (car defaults) (cadr defaults))))
          (setq defaults (cddr defaults)))
        (plist-put out :params (plist-put params :_meta meta)))
    object))

(defun pai-mcp-protocol--header-value (value)
  "Encode VALUE using MCP's UTF-8 base64 HTTP field sentinel when necessary."
  (if (or (string-empty-p value)
          (not (equal value (string-trim value)))
          (string-match-p "[^\t -~]" value)
          (and (string-prefix-p "=?base64?" value)
               (string-suffix-p "?=" value)))
      (concat "=?base64?" (base64-encode-string
                           (encode-coding-string value 'utf-8) t) "?=")
    value))

(defun pai-mcp-protocol-headers (_server object)
  "Return body-derived HTTP headers for decorated request OBJECT.
Merge these after configured/session headers.  Like SDK 2.0, notifications
and responses have no body-derived headers; a request's envelope, rather than
connection state, supplies its protocol version."
  (let* ((params (plist-get object :params))
         (version (plist-get (plist-get params :_meta)
                             :io.modelcontextprotocol/protocolVersion))
         (method (plist-get object :method))
         (name (plist-get params (if (equal method "resources/read") :uri :name))))
    (when (and method (plist-member object :id) (stringp version))
      (append (list (cons "MCP-Protocol-Version" version)
                    (cons "Mcp-Method" method))
              (when (stringp name)
                (list (cons "Mcp-Name" (pai-mcp-protocol--header-value name))))))))

(defun pai-mcp-protocol--object-p (value)
  "Whether VALUE represents a JSON object in the client's decoded format."
  (or (null value) (hash-table-p value)
      (and (listp value) (cl-evenp (length value))
           (cl-loop for (key _value) on value by #'cddr always (keywordp key)))))

(defun pai-mcp-protocol--get (object key)
  "Read KEY from a decoded or constructed JSON OBJECT."
  (if (hash-table-p object) (gethash (substring (symbol-name key) 1) object)
    (plist-get object key)))

(defun pai-mcp-protocol--capabilities-p (caps)
  "Validate the known modern capability fields of CAPS."
  (and (pai-mcp-protocol--object-p caps)
       (cl-every
        (lambda (key)
          (let ((value (pai-mcp-protocol--get caps key)))
            (or (null value)
                (and (pai-mcp-protocol--object-p value)
                     (cl-every
                      (lambda (flag)
                        (let ((v (pai-mcp-protocol--get value flag)))
                          (memq v '(nil t :false))))
                      (if (eq key :resources) '(:listChanged :subscribe)
                        (when (memq key '(:tools :prompts)) '(:listChanged))))))))
        '(:experimental :logging :completions :prompts :resources :tools :extensions))))

(defun pai-mcp-protocol--discover-p (result)
  "Whether RESULT satisfies the SDK modern discovery result contract."
  (and (listp result) (plist-member result :supportedVersions)
       (let ((versions (plist-get result :supportedVersions)))
         (and (or (listp versions) (vectorp versions))
              (cl-every #'stringp versions)))
       (plist-member result :capabilities)
       (pai-mcp-protocol--capabilities-p (plist-get result :capabilities))
       (or (not (plist-member result :instructions))
           (stringp (plist-get result :instructions)))
       (or (not (plist-member result :resultType))
           (stringp (plist-get result :resultType)))
       (or (not (plist-member result :_meta))
           (pai-mcp-protocol--object-p (plist-get result :_meta)))))

(defun pai-mcp-protocol-error-hint (error)
  "Describe structured probe ERROR without mistaking outages for legacy MCP."
  (if (stringp error) error
    (let ((status (plist-get error :status))
          (message (or (plist-get error :message) "MCP protocol negotiation failed")))
      (cond
       ((eq status 503)
        "HTTP 503: server temporarily unavailable; MCP endpoint shape could not be determined")
       ((memq status '(401 403))
        (format "HTTP %s: %s; check MCP authentication and access" status message))
       ((eq (plist-get error :kind) 'shape)
        (format "%s; response is not a JSON-RPC 2.0 MCP envelope; check the endpoint URL" message))
       (status (format "HTTP %s: %s" status message))
       (t message)))))

(defun pai-mcp-protocol--verdict (server result error)
  "Classify a discovery RESULT or ERROR for SERVER, following SDK 2.0.
Return modern, legacy, corrective, or an error string."
  (cond
   ((not error)
    (if (and (pai-mcp-protocol--discover-p result)
             (member pai-mcp-protocol-modern-version
                     (append (plist-get result :supportedVersions) nil)))
        'modern 'legacy))
   ((stringp error) (pai-mcp-protocol-error-hint error))
   ((memq (plist-get error :kind) '(network shape auth))
    (pai-mcp-protocol-error-hint error))
   ((memq (plist-get error :kind) '(timeout closed))
    (if (eq (plist-get server :transport) 'stdio) 'legacy
      (pai-mcp-protocol-error-hint error)))
   ((and (numberp (plist-get error :status))
         (or (memq (plist-get error :status) '(401 403))
             (>= (plist-get error :status) 500)))
    (pai-mcp-protocol-error-hint error))
   ((eq (plist-get error :kind) 'http)
    (let* ((body (plist-get error :body))
           (rpc (and (stringp body)
                     (ignore-errors (plist-get (pai-json-decode body) :error)))))
      (if (and (listp rpc) (numberp (plist-get rpc :code)))
          (pai-mcp-protocol--verdict server nil rpc) 'legacy)))
   ((eq (plist-get error :code) -32022)
    (let ((supported (plist-get (plist-get error :data) :supported)))
      (cond
       ((not (and (or (listp supported) (vectorp supported))
                  (> (length supported) 0) (cl-every #'stringp supported))) 'legacy)
       ((member pai-mcp-protocol-modern-version (append supported nil)) 'corrective)
       ((cl-some (lambda (version) (not (string< version pai-mcp-protocol-modern-version)))
                 supported)
        (format "Unsupported MCP protocol version: server supports %s" supported))
       (t 'legacy))))
   ((numberp (plist-get error :code)) 'legacy)
   (t (pai-mcp-protocol-error-hint error))))

(defun pai-mcp-protocol--stdio-window (server on-open on-error)
  "Open SERVER's disposable sibling and call ON-OPEN with an exchange function.
The exchange function accepts a (RESULT ERROR) callback; it can be called again
for the SDK's single corrective probe.  Install :protocol-cancel for reaping."
  (let ((def (plist-get server :def)) proc stderr timer callback (buffer "") (counter 0))
    (cl-labels
        ((cleanup ()
           (when timer (cancel-timer timer) (setq timer nil))
           (setq callback nil)
           (when (and proc (process-live-p proc)) (delete-process proc))
           (when (and stderr (process-live-p stderr)) (delete-process stderr)))
         (deliver (result error)
           (when callback
             (let ((cb callback))
               (setq callback nil)
               (when timer (cancel-timer timer) (setq timer nil))
               (funcall cb result error))))
         (exchange (cb)
           (setq callback cb counter (1+ counter))
           (setq timer
                 (run-at-time pai-mcp-startup-timeout nil
                              (lambda () (deliver nil '(:kind timeout :message "Discovery probe timed out")))))
           (condition-case error
               (process-send-string
                proc (concat (pai-json-encode
                              (list :jsonrpc "2.0" :id (format "server-discover-probe-%d" counter)
                                    :method "server/discover"
                                    :params (list :_meta (pai-mcp-protocol--meta server)))) "\n"))
             (error (deliver nil (list :kind 'network :message (error-message-string error)))))))
      (pai-mcp--set server :protocol-cancel #'cleanup)
      (condition-case error
          (let* ((process-environment (pai-mcp--process-env def))
                 (default-directory
                  (if (plist-get def :cwd)
                      (file-name-as-directory (pai-mcp--interpolate (plist-get def :cwd)))
                    default-directory)))
            (setq stderr (make-pipe-process :name "pai-mcp-probe-stderr"
                                            :noquery t :filter #'ignore))
            (setq proc
                  (make-process
                   :name (format "pai-mcp-probe-%s" (plist-get server :name))
                   :command (cons (pai-mcp--interpolate (plist-get def :command))
                                  (mapcar #'pai-mcp--interpolate (plist-get def :args)))
                   :connection-type 'pipe :noquery t :coding 'utf-8 :stderr stderr
                   :filter
                   (lambda (_proc chunk)
                     (setq buffer (concat buffer chunk))
                     (while (string-match "\n" buffer)
                       (let ((line (substring buffer 0 (match-beginning 0))))
                         (setq buffer (substring buffer (match-end 0)))
                         (let ((message (ignore-errors (pai-json-decode line))))
                           (when (and (listp message) (equal (plist-get message :jsonrpc) "2.0")
                                      (not (plist-get message :method))
                                      (equal (plist-get message :id)
                                             (format "server-discover-probe-%d" counter))
                                      (or (plist-member message :result) (plist-member message :error)))
                             (deliver (plist-get message :result) (plist-get message :error)))))))
                   :sentinel (lambda (process _event)
                               (when (memq (process-status process) '(exit signal closed failed))
                                 (deliver nil '(:kind closed :message "Discovery probe connection closed"))))))
            (funcall on-open #'exchange))
        (error (cleanup) (funcall on-error (error-message-string error)))))))

(defun pai-mcp-protocol--http-exchange (server callback)
  "Make one in-place HTTP discovery exchange with CALLBACK."
  (let (id timer settled)
    (cl-labels ((finish (result error)
                 (unless settled
                   (setq settled t)
                   (when timer (cancel-timer timer))
                   (funcall callback result error))))
      (setq id (pai-mcp--request server "server/discover"
                                 (list :_meta (pai-mcp-protocol--meta server)) #'finish))
      (unless settled
        (setq timer
              (run-at-time pai-mcp-startup-timeout nil
                           (lambda ()
                             (remhash id (plist-get server :pending))
                             (finish nil '(:kind timeout :message "Discovery probe timed out"))))))
      (unless settled
        (pai-mcp--set server :protocol-cancel
                      (lambda ()
                        (setq settled t)
                        (cancel-timer timer)
                        (remhash id (plist-get server :pending))))))))

(defun pai-mcp-protocol-connect (server on-ready on-error)
  "Negotiate SERVER asynchronously; call ON-READY () or ON-ERROR (STRING).
Populate :protocol-version, :capabilities, :instructions and :server-info.
Optional :protocol-start-session is a no-argument deferred stdio spawn hook.
The client must invoke :protocol-cancel when stopping/failing during a probe.
During :protocol-probing, request errors must preserve RPC code/data or use
plists (:kind http :status N :body STRING :message STRING), (:kind network),
(:kind shape), (:kind timeout), or (:kind closed), with a :message.
HTTP startup notification ordering is enforced by the transport send queue."
  (let ((mode (or (plist-get (plist-get server :def) :protocolVersion) "legacy"))
        done corrective-used)
    (cl-labels
        ((cancel-probe ()
           (when-let* ((cancel (plist-get server :protocol-cancel))) (funcall cancel))
           (pai-mcp--set server :protocol-cancel nil)
           (pai-mcp--set server :protocol-probing nil))
         (fail (message)
           (unless done (setq done t) (cancel-probe) (funcall on-error message)))
         (start-session ()
           (when-let* ((start (plist-get server :protocol-start-session)))
             (pai-mcp--set server :protocol-start-session nil)
             (funcall start)))
         (ready (result version modern)
           (unless done
             (pai-mcp--set server :protocol-version version)
             (pai-mcp--set server :capabilities (plist-get result :capabilities))
             (pai-mcp--set server :instructions (plist-get result :instructions))
             (pai-mcp--set server :server-info
                           (if modern (plist-get (plist-get result :_meta) :io.modelcontextprotocol/serverInfo)
                             (plist-get result :serverInfo)))
             (unless modern (pai-mcp--notify server "notifications/initialized"))
             (setq done t)
             (funcall on-ready)))
         (legacy ()
           (cancel-probe)
           (pai-mcp--set server :protocol-version nil)
           (start-session)
           (pai-mcp--request
            server "initialize"
            (list :protocolVersion pai-mcp-protocol-version
                  :capabilities (or (plist-get server :client-capabilities) (pai-json-empty-object))
                  :clientInfo (pai-mcp-protocol--client-info))
            (lambda (result error)
              (cond (error (fail (format "initialize failed: %s" (pai-mcp-protocol-error-hint error))))
                    ((not (and (listp result) (stringp (plist-get result :protocolVersion))
                               (string< (plist-get result :protocolVersion) pai-mcp-protocol-modern-version)))
                     (fail "initialize returned no supported legacy protocolVersion"))
                    (t (ready result (plist-get result :protocolVersion) nil))))))
         (probe (exchange)
           (funcall exchange
                    (lambda (result error)
                      (unless done
                        (let ((verdict (pai-mcp-protocol--verdict server result error)))
                          (pcase verdict
                            ('modern
                             (cancel-probe)
                             (condition-case error
                                 (progn (start-session)
                                        (ready result pai-mcp-protocol-modern-version t))
                               (error (fail (error-message-string error)))))
                            ('corrective
                             (if corrective-used
                                 (fail "Server repeatedly rejected the offered MCP protocol version")
                               (setq corrective-used t) (probe exchange)))
                            ('legacy
                             (if (equal mode "auto")
                                 (condition-case error (legacy)
                                   (error (fail (error-message-string error))))
                               (fail (format "Server did not offer pinned MCP %s via server/discover; no fallback in pin mode"
                                             pai-mcp-protocol-modern-version))))
                            (_ (fail verdict)))))))))
      (condition-case error
          (cond
           ((equal mode "legacy") (legacy))
           ((member mode (list "auto" pai-mcp-protocol-modern-version))
            (pai-mcp--set server :protocol-version nil)
            (pai-mcp--set server :protocol-probing t)
            (if (eq (plist-get server :transport) 'stdio)
                (pai-mcp-protocol--stdio-window server #'probe #'fail)
              (probe (lambda (callback) (pai-mcp-protocol--http-exchange server callback)))))
           (t (fail (format "Invalid MCP protocolVersion: %s" mode))))
        (error (fail (error-message-string error)))))))

(provide 'pai-mcp-negotiation)
;;; pai-mcp-negotiation.el ends here
