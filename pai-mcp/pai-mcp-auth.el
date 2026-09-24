;;; pai-mcp-auth.el --- Async bearer and OAuth authentication -*- lexical-binding: t; -*-

;;; Commentary:
;; All network and secret-helper work is asynchronous.  Credentials are stored
;; in URL-bound, owner-only files in `pai-mcp-auth-directory', not in config.
;; This native backend intentionally uses private files rather than upstream's
;; Node-specific OS keyring.  Only explicitly trusted config may contain !cmd.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'browse-url)
(require 'pai-mcp-config)

(defcustom pai-mcp-auth-directory nil
  "Private credential directory, or nil for mcp-auth under `pai-directory'."
  :type '(choice (const nil) directory) :group 'pai-mcp)
(defcustom pai-mcp-auth-timeout 300
  "Seconds to allow an OAuth authorization flow."
  :type 'number :group 'pai-mcp)
(defvar pai-mcp-auth--flows (make-hash-table :test 'equal))
(defconst pai-mcp-auth--limit (* 1024 1024))

(defun pai-mcp-auth--origin (value)
  "Return canonical HTTP origin of VALUE, rejecting unsafe URLs."
  (unless (stringp value) (error "Authentication URL is missing"))
  (let* ((u (url-generic-parse-url value)) (host (url-host u))
         (scheme (url-type u)))
    (unless (and (member scheme '("http" "https")) host
                 (not (url-user u)) (not (url-password u))
                 (not (url-target u))
                 (not (string-match-p "[\r\n]" value)))
      (error "Invalid authentication URL"))
    (format "%s://%s:%s" scheme (downcase host) (url-port u))))

(defun pai-mcp-auth--secure-url (value)
  "Validate VALUE for credential or discovery transport."
  (pai-mcp-auth--origin value)
  (let ((u (url-generic-parse-url value)))
    (unless (or (equal (url-type u) "https")
                (member (url-host u) '("localhost" "127.0.0.1" "::1" "[::1]")))
      (error "Authentication requires HTTPS (except loopback)")))
  value)

(defun pai-mcp-auth--file (name)
  "Return the credential file for NAME without interpreting NAME as a path."
  (expand-file-name (concat (secure-hash 'sha256 name) ".json")
                    (or pai-mcp-auth-directory
                        (expand-file-name "mcp-auth" pai-directory))))

(defun pai-mcp-auth--load (name url)
  "Read private credentials for NAME, bound to the exact URL."
  (let ((file (pai-mcp-auth--file name)))
    (when (file-exists-p file)
      (when (or (file-symlink-p file)
                (/= (logand (file-modes file) #o077) 0)
                (not (equal (file-attribute-user-id (file-attributes file))
                            (user-uid))))
        (error "Credential file must be owned by you and mode 0600"))
      (when (> (file-attribute-size (file-attributes file)) pai-mcp-auth--limit)
        (error "Credential file exceeds size limit"))
      (let ((entry (condition-case nil
                       (with-temp-buffer
                         (insert-file-contents file)
                         (json-parse-buffer :object-type 'plist :array-type 'list
                                            :null-object nil :false-object :false))
                     (error (error "Credential file is malformed or unreadable")))))
        (unless (equal url (plist-get entry :serverUrl))
          (error "Stored credentials belong to a different server URL"))
        entry))))

(defun pai-mcp-auth--save (name entry)
  "Atomically persist ENTRY for NAME with mode 0600."
  (let* ((file (pai-mcp-auth--file name))
         (dir (file-name-directory file)) temp)
    (when (file-symlink-p (directory-file-name dir))
      (error "Credential directory may not be a symlink"))
    (make-directory dir t)
    (set-file-modes dir #o700)
    (unwind-protect
        (progn
          (setq temp (make-temp-file (expand-file-name ".credential-" dir)))
          (set-file-modes temp #o600)
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (pai-json-encode entry) nil temp nil 'silent))
          (rename-file temp file t))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(defun pai-mcp-auth--clean-token (value)
  "Validate VALUE before using it in an HTTP header."
  (unless (and (stringp value) (not (string-empty-p value))
               (not (string-match-p "[[:space:][:cntrl:]]" value)))
    (error "Authentication token is empty or contains invalid characters"))
  value)

(defun pai-mcp-auth--finish (flow headers error-text)
  "Settle FLOW exactly once with HEADERS or ERROR-TEXT."
  (unless (plist-get flow :done)
    (setf (plist-get flow :done) t)
    (when (eq (gethash (plist-get flow :name) pai-mcp-auth--flows) flow)
      (remhash (plist-get flow :name) pai-mcp-auth--flows))
    (dolist (timer (plist-get flow :timers)) (cancel-timer timer))
    (dolist (proc (plist-get flow :processes))
      (when (process-live-p proc) (delete-process proc)))
    (let ((waiters (plist-get flow :waiters)))
      (setf (plist-get flow :waiters) nil)
      (dolist (pair (nreverse waiters))
        (condition-case nil
            (if error-text (funcall (cdr pair) error-text)
              (funcall (car pair) headers))
          (error nil))))))

(defun pai-mcp-auth--guard (flow function)
  "Call FUNCTION only for an active FLOW; route errors to its error callback."
  (unless (plist-get flow :done)
    (condition-case reason (funcall function)
      (error (pai-mcp-auth--finish flow nil (error-message-string reason))))))

(defun pai-mcp-auth-resolve-secret (value on-done on-error &optional dir)
  "Resolve configured VALUE, including !command, asynchronously.
!! escapes a literal !.  Commands have closed stdin, suppressed stderr,
10-second deadline and 1 MiB output limit.  Return the helper process or nil.
DIR is the working directory; only call this on explicitly trusted config."
  (let ((success on-done) (failure on-error) settled)
    (setq on-done (lambda (result)
                    (unless settled
                      (setq settled t)
                      (condition-case nil (funcall success result) (error nil))))
          on-error (lambda (text)
                     (unless settled
                       (setq settled t)
                       (condition-case nil (funcall failure text) (error nil))))))
  (condition-case nil
      (let ((text (pai-mcp--interpolate value)))
        (cond
         ((not (stringp text)) (funcall on-error "Secret must be a string"))
         ((string-prefix-p "!!" text) (funcall on-done (substring text 1)))
         ((not (string-prefix-p "!" text)) (funcall on-done text))
         (t
          (let ((default-directory (file-name-as-directory (or dir default-directory)))
                (out "") done timer proc)
            (cl-labels ((finish (error-text)
                         (unless done
                           (setq done t)
                           (when timer (cancel-timer timer))
                           (when (and proc (process-live-p proc)) (delete-process proc))
                           (if error-text (funcall on-error error-text)
                             (let ((secret (string-trim out)))
                               (if (string-empty-p secret)
                                   (funcall on-error "Secret command produced empty output")
                                 (funcall on-done secret)))))))
              (setq proc
                    (make-process
                     :name "pai-mcp-secret" :noquery t :connection-type 'pipe
                     :coding 'utf-8-unix :stderr nil
                     :command (list shell-file-name shell-command-switch
                                    (concat "exec 2>/dev/null; " (substring text 1)))
                     :filter (lambda (_p chunk)
                               (unless done
                                 (if (> (+ (string-bytes out) (string-bytes chunk))
                                        pai-mcp-auth--limit)
                                     (finish "Secret command output exceeds 1 MiB")
                                   (setq out (concat out chunk)))))
                     :sentinel (lambda (p _event)
                                 (when (memq (process-status p) '(exit signal))
                                   (finish (unless (zerop (process-exit-status p))
                                             "Secret command failed"))))))
              (setq timer (run-at-time 10 nil (lambda () (finish "Secret command timed out"))))
              (process-send-eof proc)
              proc)))))
    (error (funcall on-error "Unable to resolve authentication secret"))))

(defun pai-mcp-auth--quote (value)
  "Quote VALUE for curl's stdin configuration, never shell syntax."
  (concat "\"" (replace-regexp-in-string
                  "[\\\"\n\r]"
                  (lambda (s) (pcase s ("\n" "\\n") ("\r" "\\r") (_ (concat "\\" s))))
                  value t t) "\""))

(defun pai-mcp-auth--http (flow url method body content-type callback &optional extra-headers)
  "Fetch URL asynchronously, calling CALLBACK with status, headers and body.
Credentials are passed through stdin, not argv; redirects are never followed."
  (pai-mcp-auth--guard
   flow
   (lambda ()
     (pai-mcp-auth--secure-url url)
     (let* ((out "") (overflow nil) (delivered nil)
            (config (concat "url = " (pai-mcp-auth--quote url) "\nrequest = " method
                            "\nheader = \"Accept: application/json\"\n"
                            (mapconcat (lambda (h)
                                         (concat "header = " (pai-mcp-auth--quote
                                                               (concat (car h) ": " (cdr h))) "\n"))
                                       extra-headers "")
                            (when content-type
                              (concat "header = " (pai-mcp-auth--quote
                                                   (concat "Content-Type: " content-type)) "\n"))
                            (when body (concat "data-binary = " (pai-mcp-auth--quote body) "\n"))))
            (ca (plist-get (plist-get flow :def) :caFile))
            (proc
             (make-process
              :name "pai-mcp-oauth" :noquery t :connection-type 'pipe
              :coding 'utf-8-unix
              :command (append '("curl" "--disable" "--silent" "--include"
                                 "--max-time" "30" "--max-redirs" "0" "--config" "-")
                               (when ca (list "--cacert" (expand-file-name
                                                         (pai-mcp--interpolate ca)
                                                         (plist-get flow :dir)))))
              :filter (lambda (p chunk)
                        (if (> (+ (string-bytes out) (string-bytes chunk)) pai-mcp-auth--limit)
                            (progn (setq overflow t) (delete-process p))
                          (setq out (concat out chunk))))
              :sentinel
              (lambda (p _event)
                (when (memq (process-status p) '(exit signal))
                  (pai-mcp-auth--guard
                   flow
                   (lambda ()
                     (when (or overflow (not (zerop (process-exit-status p))))
                       (error "OAuth HTTP request failed%s" (if overflow " (response too large)" "")))
                     ;; curl may prefix proxy CONNECT or interim 1xx headers.
                     (while (string-match "\\`HTTP/[0-9.]+ \\([0-9]+\\)[^\n]*\r?\n" out)
                       (let ((status (string-to-number (match-string 1 out))))
                         (unless (string-match "\r?\n\r?\n" out)
                           (error "Malformed OAuth HTTP response"))
                         (let ((headers (substring out 0 (match-beginning 0))))
                           (setq out (substring out (match-end 0)))
                           (unless (or (< status 200)
                                       (and (= status 200) (string-prefix-p "HTTP/" out)))
                             (funcall callback status headers out)
                             (setq delivered t)
                             (setq out "")))))
                     (unless delivered (error "Malformed OAuth HTTP response")))))))))
       (push proc (plist-get flow :processes))
       (process-send-string proc config)
       (process-send-eof proc)))))

(defun pai-mcp-auth--json (status body)
  "Decode successful OAuth STATUS and BODY without exposing response secrets."
  (unless (<= 200 status 299) (error "OAuth endpoint returned HTTP %d" status))
  (condition-case nil
      (let ((json (json-parse-string body :object-type 'plist :array-type 'list
                                     :null-object nil :false-object :false)))
        (unless (listp json) (error "Not an object")) json)
    (error (error "OAuth endpoint returned invalid JSON"))))

(defun pai-mcp-auth--form (pairs)
  "Encode non-nil PAIRS as application/x-www-form-urlencoded."
  (mapconcat (lambda (pair) (concat (url-hexify-string (car pair)) "="
                                    (url-hexify-string (format "%s" (cdr pair)))))
             (cl-remove-if-not #'cdr pairs) "&"))

(defun pai-mcp-auth--issuer-from-metadata (url)
  "Infer expected issuer from OAuth/OIDC metadata URL."
  (let* ((u (url-generic-parse-url url))
         (path (car (split-string (url-filename u) "?")))
         (origin (replace-regexp-in-string "/+\\'" "" (url-recreate-url
                   (let ((copy (copy-sequence u))) (setf (url-filename copy) "") copy))))
         (marker "/.well-known/openid-configuration"))
    (cond
     ((string-match "\\`/.well-known/\\(?:oauth-authorization-server\\|openid-configuration\\)\\(.*\\)" path)
      (concat origin (match-string 1 path)))
     ((string-suffix-p marker path) (concat origin (substring path 0 (- (length marker)))))
     (t origin))))

(defun pai-mcp-auth--metadata (flow urls issuer callback)
  "Try authorization metadata URLS for FLOW with expected ISSUER."
  (unless urls (error "OAuth authorization-server discovery failed"))
  (pai-mcp-auth--http
   flow (car urls) "GET" nil nil
   (lambda (status _headers body)
     (if (and (memq status '(404 405)) (cdr urls))
         (pai-mcp-auth--metadata flow (cdr urls) issuer callback)
       (let* ((meta (pai-mcp-auth--json status body))
              (actual (plist-get meta :issuer))
              (oauth (plist-get (plist-get flow :def) :oauth)))
         (pai-mcp-auth--secure-url actual)
         (unless (or (eq (plist-get oauth :skipIssuerMetadataValidation) t)
                     (equal (string-trim-right issuer "/+") (string-trim-right actual "/+")))
           (error "OAuth metadata issuer mismatch"))
         (pai-mcp-auth--secure-url (plist-get meta :token_endpoint))
         (setf (plist-get flow :metadata) meta)
         (funcall callback))))))

(defun pai-mcp-auth--discover-issuer (flow issuer callback)
  "Discover authorization metadata for ISSUER."
  (pai-mcp-auth--secure-url issuer)
  (let* ((u (url-generic-parse-url issuer))
         (path (string-trim-right (url-filename u) "/+"))
         (base (substring issuer 0 (- (length issuer) (length (url-filename u))))))
    (pai-mcp-auth--metadata
     flow (delete-dups (list (concat base "/.well-known/oauth-authorization-server" path)
                            (concat base "/.well-known/openid-configuration" path)
                            (concat (string-trim-right issuer "/+") "/.well-known/openid-configuration")))
     issuer callback)))

(defun pai-mcp-auth--discover (flow callback)
  "Discover RFC 9728 resource and RFC 8414/OIDC authorization metadata."
  (let* ((url (plist-get flow :url))
         (oauth (plist-get (plist-get flow :def) :oauth))
         (explicit (plist-get oauth :authServerMetadataUrl))
         (u (url-generic-parse-url url))
         (path (car (split-string (url-filename u) "?")))
         (base (substring url 0 (- (length url) (length (url-filename u))))))
    (if explicit
        (pai-mcp-auth--metadata flow (list explicit)
                                (pai-mcp-auth--issuer-from-metadata explicit) callback)
      (cl-labels
          ((resource (target fallback)
             (pai-mcp-auth--http
              flow target "GET" nil nil
              (lambda (status _headers body)
                (if (and (memq status '(404 405)) fallback)
                    (resource fallback nil)
                  (if (memq status '(404 405))
                      (pai-mcp-auth--discover-issuer flow base callback)
                    (let* ((meta (pai-mcp-auth--json status body))
                           (bound (plist-get meta :resource))
                           (issuer (car (plist-get meta :authorization_servers))))
                      (unless (equal bound url) (error "OAuth resource metadata URL mismatch"))
                      (unless issuer (error "OAuth resource metadata has no authorization server"))
                      (pai-mcp-auth--discover-issuer flow issuer callback))))))))
        ;; This preliminary probe deliberately carries no configured secrets.
        (pai-mcp-auth--http
         flow url "GET" nil nil
         (lambda (_status headers _body)
           (if (let ((case-fold-search t))
                 (string-match "^WWW-Authenticate:.*resource_metadata=\"\\([^\"]+\\)\"" headers))
               (resource (match-string 1 headers) nil)
             (resource (concat base "/.well-known/oauth-protected-resource" path)
                       (concat base "/.well-known/oauth-protected-resource")))))))))

(defun pai-mcp-auth--random ()
  "Return 256 cryptographically random bits encoded as base64url."
  (pai-mcp-auth--base64url
   (if (fboundp 'gnutls-random) (gnutls-random 32)
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (insert-file-contents-literally "/dev/urandom" nil 0 32)
       (unless (= (buffer-size) 32) (error "Secure random source unavailable"))
       (buffer-string)))))

(defun pai-mcp-auth--base64url (bytes)
  "Encode BYTES as unpadded URL-safe base64."
  (string-trim-right (subst-char-in-string ?/ ?_ (subst-char-in-string ?+ ?- (base64-encode-string bytes t))) "=+"))

(defun pai-mcp-auth--token (flow pairs)
  "Exchange PAIRS for tokens using FLOW's authenticated client."
  (let* ((client (plist-get flow :client))
         (meta (plist-get flow :metadata))
         (oauth (plist-get (plist-get flow :def) :oauth))
         (secret (plist-get client :client_secret))
         (methods (plist-get meta :token_endpoint_auth_methods_supported))
         (method (or (plist-get client :token_endpoint_auth_method)
                     (if secret
                         (if (and methods (not (member "client_secret_post" methods)))
                             "client_secret_basic" "client_secret_post")
                       "none")))
         (basic (equal method "client_secret_basic")))
    (unless (and (member method '("none" "client_secret_post" "client_secret_basic"))
                 (or (not methods) (member method methods)))
      (error "OAuth server requires an unsupported client authentication method"))
    (when (and (not (equal method "none")) (not secret))
      (error "OAuth client authentication requires a client secret"))
    (unless (equal (pai-mcp-auth--origin (plist-get meta :issuer))
                   (pai-mcp-auth--origin (plist-get meta :token_endpoint)))
      (error "Refusing client credentials across issuer origins"))
    (pai-mcp-auth--http
     flow (plist-get meta :token_endpoint) "POST"
     (pai-mcp-auth--form
      (append pairs (list (cons "client_id" (plist-get client :client_id))
                          (cons "client_secret" (and (equal method "client_secret_post") secret))
                          (cons "resource" (plist-get flow :url))
                          (cons "scope" (plist-get oauth :scope)))))
     "application/x-www-form-urlencoded"
     (lambda (status _headers body)
       (let* ((token (pai-mcp-auth--json status body))
              (access (pai-mcp-auth--clean-token (plist-get token :access_token)))
              (kind (plist-get token :token_type))
              (expiry (plist-get token :expires_in))
              (old (plist-get flow :entry)))
         (unless (and (stringp kind) (equal (downcase kind) "bearer"))
           (error "OAuth endpoint returned unsupported token type"))
         (when expiry
           (unless (and (numberp expiry) (> expiry 0)) (error "Invalid OAuth token lifetime"))
           (setq token (plist-put token :expires_at (+ (float-time) expiry))))
         (unless (plist-get token :refresh_token)
           (setq token (plist-put token :refresh_token
                                  (plist-get (plist-get old :tokens) :refresh_token))))
         (pai-mcp-auth--save
          (plist-get flow :name)
          (list :serverUrl (plist-get flow :url) :tokens token :client client
                :issuer (plist-get meta :issuer) :metadata meta))
         (pai-mcp-auth--finish flow (list (cons "Authorization" (concat "Bearer " access))) nil)))
     (when basic
       (list (cons "Authorization"
                   (concat "Basic " (base64-encode-string
                                     (encode-coding-string
                                      (concat (url-hexify-string (plist-get client :client_id)) ":"
                                              (url-hexify-string secret)) 'utf-8) t))))))))

(defun pai-mcp-auth--callback (flow proc chunk)
  "Handle bounded loopback HTTP callback data CHUNK on PROC for FLOW."
  (let ((text (concat (process-get proc 'input) chunk)))
    (process-put proc 'input text)
    (cond
     ((> (length text) 16384) (delete-process proc))
     ((string-match "\r\n\r\n" text)
      (pai-mcp-auth--guard
       flow
       (lambda ()
         (unless (process-get proc 'callback-handled)
           (process-put proc 'callback-handled t)
         (let* ((target (and (string-match "\\`GET \\([^ ]+\\) HTTP/1\\.[01]\r\n" text)
                             (match-string 1 text)))
                (parts (and target (split-string target "?")))
                (params (and (cadr parts) (url-parse-query-string (cadr parts))))
                (state (cadr (assoc "state" params)))
                (code (cadr (assoc "code" params)))
                (valid (and (stringp state) (stringp (plist-get flow :state))
                            (equal (car parts) (plist-get flow :callback-path))
                            (equal state (plist-get flow :state)))))
           (process-send-string proc
                                (concat "HTTP/1.1 " (if valid "200 OK" "400 Bad Request")
                                        "\r\nContent-Type: text/plain\r\nConnection: close\r\n"
                                        "Cache-Control: no-store\r\n\r\n"
                                        (if valid "You may close this window." "Invalid authorization callback.")))
           (process-send-eof proc)
           (when (and valid (not (plist-get flow :exchanging)))
             (setf (plist-get flow :exchanging) t)
             (when (assoc "error" params) (error "OAuth authorization was denied"))
             (unless (and code (not (string-empty-p code))) (error "OAuth callback has no code"))
             (pai-mcp-auth--token
              flow (list (cons "grant_type" "authorization_code") (cons "code" code)
                         (cons "code_verifier" (plist-get flow :verifier))
                         (cons "redirect_uri" (plist-get flow :redirect)))))))))))))

(defun pai-mcp-auth--listener (flow)
  "Start FLOW's loopback listener and set its concrete redirect URI."
  (let* ((oauth (plist-get (plist-get flow :def) :oauth))
         (template (or (plist-get oauth :redirectUri)
                       (if (plist-get oauth :clientId)
                           (format "http://localhost:%s/callback"
                                   (or (getenv "MCP_OAUTH_CALLBACK_PORT") "19876"))
                         "http://127.0.0.1:{port}/callback")))
         (dynamic (string-match-p "{port}" template))
         (u (url-generic-parse-url (replace-regexp-in-string "{port}" "0" template t t)))
         (host (url-host u)) (path (url-filename u)))
    (pai-mcp-auth--origin (replace-regexp-in-string "{port}" "0" template t t))
    (unless (and (equal (url-type u) "http")
                 (member host '("localhost" "127.0.0.1" "::1" "[::1]"))
                 (string-prefix-p "/" path) (not (string-match-p "[?#]" path)))
      (error "OAuth redirectUri must be an HTTP loopback URL"))
    (let* ((server (make-network-process
                    :name "pai-mcp-oauth-callback" :server t :noquery t
                    :host (if (equal host "localhost") "127.0.0.1" host)
                    :family (if (member host '("::1" "[::1]")) 'ipv6 'ipv4)
                    :service (if dynamic t (url-port u)) :coding 'utf-8-unix
                    :log (lambda (_server client _message)
                           (push client (plist-get flow :processes))
                           (set-process-filter client
                                               (lambda (p s) (pai-mcp-auth--callback flow p s))))))
           (port (process-contact server :service)))
      (push server (plist-get flow :processes))
      (setf (plist-get flow :callback-path) path
            (plist-get flow :redirect)
            (if dynamic (replace-regexp-in-string "{port}" (number-to-string port) template t t) template)))))

(defun pai-mcp-auth--client (flow callback)
  "Resolve or dynamically register FLOW's client, then CALLBACK."
  (let* ((oauth (plist-get (plist-get flow :def) :oauth))
         (meta (plist-get flow :metadata))
         (id (plist-get oauth :clientId))
         (cimd (plist-get oauth :clientMetadataUrl))
         (secret (plist-get oauth :clientSecret))
         (machine (equal (plist-get oauth :grantType) "client_credentials")))
    (when (and cimd secret (not id)) (error "clientMetadataUrl with clientSecret requires explicit clientId"))
    (when (and cimd (not id) (eq (plist-get meta :client_id_metadata_document_supported) t))
      (pai-mcp-auth--secure-url cimd)
      (let ((u (url-generic-parse-url cimd)))
        (unless (and (equal (url-type u) "https")
                     (not (member (url-filename u) '("" "/"))))
          (error "clientMetadataUrl requires HTTPS and a non-root path")))
      (setq id cimd))
    (if id
        (let ((helper (pai-mcp-auth-resolve-secret
         (or secret "")
         (lambda (resolved)
           (pai-mcp-auth--guard
            flow (lambda ()
                   (setf (plist-get flow :client)
                         (list :client_id id :client_secret (unless (string-empty-p resolved) resolved)))
                   (funcall callback))))
         (lambda (text) (pai-mcp-auth--finish flow nil text)) (plist-get flow :dir))))
          (when (processp helper) (push helper (plist-get flow :processes))))
      (let ((endpoint (plist-get meta :registration_endpoint)))
        (unless endpoint (error "OAuth server requires a configured clientId (no registration endpoint)"))
        (pai-mcp-auth--http
         flow endpoint "POST"
         (pai-json-encode
          (append (list :client_name (or (plist-get oauth :clientName) "Emacs pai")
                        :grant_types (if machine ["client_credentials"] ["authorization_code" "refresh_token"])
                        :token_endpoint_auth_method (if machine "client_secret_post" "none"))
                  (unless machine (list :redirect_uris (vector (plist-get flow :redirect))
                                        :response_types ["code"]))
                  (when (plist-get oauth :clientUri) (list :client_uri (plist-get oauth :clientUri)))
                  (when (plist-get oauth :logoUri) (list :logo_uri (plist-get oauth :logoUri)))))
         "application/json"
         (lambda (status _headers body)
           (let ((client (pai-mcp-auth--json status body)))
             (unless (stringp (plist-get client :client_id)) (error "OAuth registration returned no client ID"))
             (setf (plist-get flow :client) client)
             (funcall callback))))))))

(defun pai-mcp-auth--authorize (flow)
  "Start browser authorization or client credentials for FLOW."
  (let* ((oauth (plist-get (plist-get flow :def) :oauth))
         (grant (or (plist-get oauth :grantType) "authorization_code"))
         (machine (equal grant "client_credentials")))
    (unless (member grant '("authorization_code" "client_credentials"))
      (error "Unsupported OAuth grantType"))
    (unless machine (pai-mcp-auth--listener flow))
    (pai-mcp-auth--discover
     flow
     (lambda ()
       (pai-mcp-auth--client
        flow
        (lambda ()
          (if machine
              (pai-mcp-auth--token flow '(("grant_type" . "client_credentials")))
            (let* ((meta (plist-get flow :metadata))
                   (endpoint (pai-mcp-auth--secure-url (plist-get meta :authorization_endpoint)))
                   (state (pai-mcp-auth--random)) (verifier (pai-mcp-auth--random))
                   (params (list (cons "response_type" "code")
                                 (cons "client_id" (plist-get (plist-get flow :client) :client_id))
                                 (cons "redirect_uri" (plist-get flow :redirect))
                                 (cons "scope" (plist-get oauth :scope))
                                 (cons "resource" (plist-get flow :url))
                                 (cons "state" state) (cons "code_challenge_method" "S256")
                                 (cons "code_challenge" (pai-mcp-auth--base64url
                                                        (secure-hash 'sha256 verifier nil nil t))))))
              (when (and (plist-get meta :code_challenge_methods_supported)
                         (not (member "S256" (plist-get meta :code_challenge_methods_supported))))
                (error "OAuth server does not support mandatory S256 PKCE"))
              (dolist (pair (pai-mcp--plist-to-alist (plist-get oauth :authorizationParams)))
                (when (or (assoc (car pair) params)
                          (assoc (car pair) (url-parse-query-string
                                             (cadr (split-string endpoint "?")))))
                  (error "authorizationParams cannot override OAuth flow parameters"))
                (unless (stringp (cdr pair)) (error "authorizationParams values must be strings"))
                (push pair params))
              (setf (plist-get flow :state) state (plist-get flow :verifier) verifier)
              (browse-url (concat endpoint (if (string-match-p "?" endpoint) "&" "?")
                                  (pai-mcp-auth--form params)))))))))))

(defun pai-mcp-auth--begin (name def url on-done on-error dir login)
  "Join or start NAME's authentication flow; LOGIN permits browser opening."
  (let ((existing (gethash name pai-mcp-auth--flows)))
    (if existing
        (if (equal url (plist-get existing :url))
            (push (cons on-done on-error) (plist-get existing :waiters))
          (funcall on-error "Another authentication flow is bound to a different URL"))
      (let ((flow (list :name name :def def :url url :dir dir
                        :waiters (list (cons on-done on-error)) :done nil
                        :processes nil :timers nil :metadata nil :client nil
                        :entry nil :callback-path nil :redirect nil :state nil
                        :verifier nil :exchanging nil)))
        (puthash name flow pai-mcp-auth--flows)
        (push (run-at-time pai-mcp-auth-timeout nil
                           (lambda () (pai-mcp-auth--finish flow nil "OAuth authentication timed out")))
              (plist-get flow :timers))
        (pai-mcp-auth--guard
         flow
         (lambda ()
           (let* ((entry (unless login (pai-mcp-auth--load name url)))
                  (tokens (plist-get entry :tokens))
                  (expiry (plist-get tokens :expires_at))
                  (access (plist-get tokens :access_token))
                  (oauth (plist-get def :oauth)))
             (setf (plist-get flow :entry) entry)
             (cond
              ((and (not login) access (or (not expiry) (> expiry (+ (float-time) 60))))
               (pai-mcp-auth--finish flow (list (cons "Authorization"
                                                       (concat "Bearer " (pai-mcp-auth--clean-token access)))) nil))
              ((and (not login) (plist-get tokens :refresh_token))
               ;; Persisted metadata was validated when authorized, is URL-bound,
               ;; and is never replaced by an unsolicited resource challenge.
               (setf (plist-get flow :metadata) (plist-get entry :metadata)
                     (plist-get flow :client) (plist-get entry :client))
               (let ((exchange (lambda ()
                                 (pai-mcp-auth--token
                                  flow (list (cons "grant_type" "refresh_token")
                                             (cons "refresh_token" (plist-get tokens :refresh_token)))))))
                 (if (plist-get oauth :clientId)
                     (pai-mcp-auth--client flow exchange)
                   (funcall exchange))))
              ((or login (equal (plist-get oauth :grantType) "client_credentials"))
               (pai-mcp-auth--authorize flow))
              (t (pai-mcp-auth--finish flow nil "needs-auth: run pai-mcp-auth-login"))))))))))

(defun pai-mcp-auth-headers (name def url on-done on-error &optional dir)
  "Resolve Authorization headers for NAME/DEF, destined for URL.
Call ON-DONE with a header alist, or ON-ERROR with a redacted error string.
Bearer tokens never leave the configured resource origin.  OAuth refresh is
automatic; browser authorization requires explicit `pai-mcp-auth-login'."
  (let ((success on-done) (failure on-error) settled)
    (setq on-done (lambda (result)
                    (unless settled
                      (setq settled t)
                      (condition-case nil (funcall success result) (error nil))))
          on-error (lambda (text)
                     (unless settled
                       (setq settled t)
                       (condition-case nil (funcall failure text) (error nil))))))
  (condition-case reason
      (let* ((configured (pai-mcp--interpolate (plist-get def :url)))
             (auth (plist-get def :auth))
             (bearer (plist-get def :bearerToken))
             (env (plist-get def :bearerTokenEnv)))
        (unless (equal (pai-mcp-auth--origin configured) (pai-mcp-auth--origin url))
          (error "Refusing credentials across resource origins"))
        (cond
         ((eq auth :false) (funcall on-done nil))
         ((or bearer env (eq (plist-get def :bearerTokenStore) t) (equal auth "bearer"))
          (pai-mcp-auth--secure-url url)
          (let ((deliver (lambda (token)
                           (condition-case nil
                               (funcall on-done (list (cons "Authorization"
                                                           (concat "Bearer " (pai-mcp-auth--clean-token token)))))
                             (error (funcall on-error "Bearer token is missing or invalid"))))))
            (cond
             (bearer (pai-mcp-auth-resolve-secret bearer deliver on-error dir))
             (env (funcall deliver (getenv env)))
             ((eq (plist-get def :bearerTokenStore) t)
              (funcall deliver (plist-get (pai-mcp-auth--load name configured) :bearerToken)))
             (t (funcall on-error "Bearer token source is missing")))))
         ((or (equal auth "oauth") (plist-get def :oauth)
              (file-exists-p (pai-mcp-auth--file name)))
          (pai-mcp-auth--secure-url url)
          (pai-mcp-auth--begin name def configured on-done on-error dir nil))
         (t (funcall on-done nil))))
    (error (funcall on-error (error-message-string reason)))))

(defun pai-mcp-auth-login (name &optional on-done on-error dir)
  "Authenticate configured server NAME without blocking Emacs.
ON-DONE receives a header alist; ON-ERROR receives a redacted string."
  (interactive (list (completing-read "Authenticate MCP server: " (pai-mcp-load-config) nil t)))
  (let ((def (pai-mcp--server-def name dir)))
    (unless def (user-error "Unknown MCP server"))
    (let ((url (pai-mcp--interpolate (plist-get def :url))))
      (pai-mcp-auth--secure-url url)
      (pai-mcp-auth--begin name def url
                          (or on-done (lambda (_headers) (message "MCP authentication completed")))
                          (or on-error (lambda (text) (message "MCP authentication: %s" text)))
                          dir t))))

(defun pai-mcp-auth-logout (name &optional _dir)
  "Cancel active authentication for NAME and delete all its stored credentials.
Earlier asynchronous work cannot recreate credentials after this boundary."
  (interactive (list (completing-read "Log out MCP server: " (pai-mcp-load-config) nil t)))
  (let ((flow (gethash name pai-mcp-auth--flows)) (file (pai-mcp-auth--file name)))
    (when flow (pai-mcp-auth--finish flow nil "Authentication cancelled by logout"))
    (when (file-exists-p file) (delete-file file)))
  (when (called-interactively-p 'interactive) (message "MCP credentials removed")))

(defun pai-mcp-auth--bearer-url (name dir)
  "Return NAME's URL after checking explicit credential-store opt-in."
  (let ((def (pai-mcp--server-def name dir)))
    (unless (and (equal (plist-get def :auth) "bearer")
                 (eq (plist-get def :bearerTokenStore) t))
      (user-error "Token storage requires auth=bearer and bearerTokenStore=true"))
    (pai-mcp-auth--secure-url (pai-mcp--interpolate (plist-get def :url)))))

(defun pai-mcp-auth-bearer-status (name &optional dir)
  "Return present or missing for NAME's explicitly enabled bearer store."
  (let ((entry (pai-mcp-auth--load name (pai-mcp-auth--bearer-url name dir))))
    (if (not entry) 'missing
      (pai-mcp-auth--clean-token (plist-get entry :bearerToken))
      'present)))

(defun pai-mcp-auth-remove-bearer (name &optional dir)
  "Remove NAME's explicitly enabled stored bearer token."
  (pai-mcp-auth--bearer-url name dir)
  (let ((file (pai-mcp-auth--file name)))
    (when (file-exists-p file) (delete-file file))))

(defun pai-mcp-auth-store-bearer (name token &optional dir)
  "Persist TOKEN for configured NAME; the interactive prompt masks input."
  (interactive (list (completing-read "MCP server: " (pai-mcp-load-config) nil t)
                     (read-passwd "Bearer token: ")))
  (let ((url (pai-mcp-auth--bearer-url name dir)))
    (pai-mcp-auth--save name (list :serverUrl url :bearerToken (pai-mcp-auth--clean-token token)))))

(provide 'pai-mcp-auth)
;;; pai-mcp-auth.el ends here
