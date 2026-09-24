;;; pai-mcp-auth-test.el --- Local OAuth integration tests -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-mcp-auth)

(defun pai-mcp-auth-test--pump (predicate)
  (let ((deadline (+ (float-time) 8)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defun pai-mcp-auth-test--respond (proc status body &optional headers)
  (let ((data (encode-coding-string body 'utf-8)))
    (process-send-string proc
                         (concat (format "HTTP/1.1 %d Fixture\r\nContent-Length: %d\r\nConnection: close\r\n" status (length data))
                                 (mapconcat (lambda (p) (concat (car p) ": " (cdr p) "\r\n")) headers "")
                                 "\r\n" data))
    (process-send-eof proc)))

(defun pai-mcp-auth-test--server (handler)
  "Start a real local HTTP fixture accepting JSON and form request bodies."
  (make-network-process
   :name "pai-auth-fixture" :server t :noquery t :host "127.0.0.1"
   :family 'ipv4 :service t :coding 'binary
   :log (lambda (server client _message)
          (process-put server 'clients (cons client (process-get server 'clients)))
          (set-process-filter
           client
           (lambda (p chunk)
             (unless (process-get p 'handled)
               (let ((text (concat (process-get p 'input) chunk)))
                 (process-put p 'input text)
                 (when (string-match "\r\n\r\n" text)
                   (let* ((end (match-end 0))
                          (header (substring text 0 end))
                          (length (let ((case-fold-search t))
                                    (if (string-match "Content-Length: \\([0-9]+\\)" header)
                                        (string-to-number (match-string 1 header)) 0))))
                     (when (>= (- (length text) end) length)
                       (process-put p 'handled t)
                       (funcall handler p header (decode-coding-string (substring text end (+ end length)) 'utf-8))))))))))))

(defmacro pai-mcp-auth-test--isolated (&rest body)
  (declare (indent 0))
  `(let* ((dir (make-temp-file "pai-auth-test" t))
          (pai-mcp-auth-directory dir)
          (pai-mcp-auth--flows (make-hash-table :test 'equal))
          (process-environment (copy-sequence process-environment))
          fixture)
     (setenv "NO_PROXY" "127.0.0.1,localhost")
     (setenv "no_proxy" "127.0.0.1,localhost")
     (unwind-protect
         (progn ,@body)
       (maphash (lambda (_name flow) (pai-mcp-auth--finish flow nil "Test cleanup")) pai-mcp-auth--flows)
       (when fixture
         (dolist (p (cons fixture (process-get fixture 'clients)))
           (when (process-live-p p) (delete-process p))))
       (delete-directory dir t))))

(ert-deftest pai-mcp-auth-bearer-precedence-binding-and-privacy ()
  (pai-mcp-auth-test--isolated
    (let* ((url "https://example.org/mcp")
           (def (list :url url :auth "bearer" :bearerTokenStore t)) result failure)
      (cl-letf (((symbol-function 'pai-mcp--server-def) (lambda (&rest _) def)))
        (pai-mcp-auth-store-bearer "test" "stored")
        (should (eq 'present (pai-mcp-auth-bearer-status "test")))
        (should (= #o600 (file-modes (pai-mcp-auth--file "test"))))
        (pai-mcp-auth-headers "test" def url (lambda (h) (setq result h))
                              (lambda (e) (setq failure e)))
        (should-not failure)
        (should (equal (cdr (assoc "Authorization" result)) "Bearer stored"))
        (setq result nil)
        (pai-mcp-auth-headers "test" def "https://other.example/mcp"
                              (lambda (h) (setq result h)) (lambda (e) (setq failure e)))
        (should failure) (should-not result)
        (setenv "PAI_AUTH_TEST_TOKEN" "environment")
        (setq def (append '(:bearerToken "literal" :bearerTokenEnv "PAI_AUTH_TEST_TOKEN") def))
        (pai-mcp-auth-headers "test" def url (lambda (h) (setq result h)) #'ignore)
        (should (equal (cdr (assoc "Authorization" result)) "Bearer literal"))
        (pai-mcp-auth-remove-bearer "test")
        (should (eq 'missing (pai-mcp-auth-bearer-status "test")))))))

(ert-deftest pai-mcp-auth-secret-command-async-and-redacted ()
  (let (result failure (count 0))
    (pai-mcp-auth-resolve-secret "!printf '  command-token  '" (lambda (s) (setq result s) (cl-incf count))
                                 (lambda (e) (setq failure e) (cl-incf count)))
    (should-not result)
    (should (pai-mcp-auth-test--pump (lambda () (or result failure))))
    (should-not failure)
    (should (equal result "command-token"))
    (should (= count 1))
    (setq result nil failure nil count 0)
    (pai-mcp-auth-resolve-secret "!printf 'secret-leak' >&2; exit 9" (lambda (s) (setq result s))
                                 (lambda (e) (setq failure e) (cl-incf count)))
    (should (pai-mcp-auth-test--pump (lambda () failure)))
    (should-not result)
    (should-not (string-match-p "secret-leak" failure))
    (should (= count 1))))

(ert-deftest pai-mcp-auth-client-credentials-refresh-and-logout ()
  (skip-unless (executable-find "curl"))
  (pai-mcp-auth-test--isolated
    (let (base def result failure grants (count 0))
      (setq fixture
            (pai-mcp-auth-test--server
             (lambda (client header body)
               (cond
                ((string-prefix-p "GET /.well-known/oauth-authorization-server " header)
                 (pai-mcp-auth-test--respond client 200
                  (pai-json-encode (list :issuer base :token_endpoint (concat base "/token")))))
                ((string-prefix-p "POST /token " header)
                 (let* ((form (url-parse-query-string body))
                        (grant (cadr (assoc "grant_type" form))))
                   (push grant grants)
                   (should (equal "fixture-secret" (cadr (assoc "client_secret" form))))
                   (should (equal (concat base "/mcp") (cadr (assoc "resource" form))))
                   (pai-mcp-auth-test--respond client 200
                    (pai-json-encode (list :access_token (if (equal grant "refresh_token") "refreshed" "initial")
                                          :token_type "Bearer" :expires_in 3600 :refresh_token "refresh-secret")))))
                (t (pai-mcp-auth-test--respond client 404 "{}"))))))
      (setq base (format "http://127.0.0.1:%s" (process-contact fixture :service))
            def (list :url (concat base "/mcp") :auth "oauth"
                      :oauth (list :grantType "client_credentials" :clientId "fixture-client"
                                   :clientSecret "fixture-secret"
                                   :authServerMetadataUrl (concat base "/.well-known/oauth-authorization-server"))))
      (cl-letf (((symbol-function 'pai-mcp--server-def) (lambda (&rest _) def)))
        (pai-mcp-auth-login "machine" (lambda (h) (setq result h) (cl-incf count))
                            (lambda (e) (setq failure e) (cl-incf count)))
        (should-not result)
        (should (pai-mcp-auth-test--pump (lambda () (or result failure))))
        (should-not failure)
        (should (= count 1))
        (should (equal "Bearer initial" (cdr (assoc "Authorization" result))))
        (let* ((entry (pai-mcp-auth--load "machine" (plist-get def :url)))
               (tokens (plist-get entry :tokens)))
          (setf (plist-get tokens :expires_at) 1)
          (pai-mcp-auth--save "machine" entry))
        (setq result nil)
        (pai-mcp-auth-headers "machine" def (plist-get def :url)
                              (lambda (h) (setq result h)) (lambda (e) (setq failure e)))
        (should (pai-mcp-auth-test--pump (lambda () (or result failure))))
        (should-not failure)
        (should (equal "Bearer refreshed" (cdr (assoc "Authorization" result))))
        (should (equal grants '("refresh_token" "client_credentials")))
        (pai-mcp-auth-logout "machine")
        (should-not (file-exists-p (pai-mcp-auth--file "machine")))))))

(ert-deftest pai-mcp-auth-browser-dcr-pkce-state-and-loopback ()
  (skip-unless (executable-find "curl"))
  (pai-mcp-auth-test--isolated
    (let (base def result failure auth-url redirect challenge callback-client (exchanges 0))
      (setq fixture
            (pai-mcp-auth-test--server
             (lambda (client header body)
               (cond
                ((string-prefix-p "GET /mcp " header)
                 (pai-mcp-auth-test--respond client 401 "{}"
                                            (list (cons "WWW-Authenticate" (format "Bearer resource_metadata=\"%s/resource\"" base)))))
                ((string-prefix-p "GET /resource " header)
                 (pai-mcp-auth-test--respond client 200
                  (pai-json-encode (list :resource (concat base "/mcp") :authorization_servers (vector base)))))
                ((string-prefix-p "GET /.well-known/oauth-authorization-server " header)
                 (pai-mcp-auth-test--respond client 200
                  (pai-json-encode (list :issuer base :token_endpoint (concat base "/token")
                                        :authorization_endpoint (concat base "/authorize")
                                        :registration_endpoint (concat base "/register")
                                        :code_challenge_methods_supported ["S256"])) ))
                ((string-prefix-p "POST /register " header)
                 (let ((registration (json-parse-string body :object-type 'plist :array-type 'list)))
                   (setq redirect (car (plist-get registration :redirect_uris)))
                   (pai-mcp-auth-test--respond client 201 "{\"client_id\":\"dynamic-client\"}")))
                ((string-prefix-p "POST /token " header)
                 (cl-incf exchanges)
                 (let* ((form (url-parse-query-string body))
                        (verifier (cadr (assoc "code_verifier" form))))
                   (should (equal "good-code" (cadr (assoc "code" form))))
                   (should (equal redirect (cadr (assoc "redirect_uri" form))))
                   (should (equal challenge (pai-mcp-auth--base64url (secure-hash 'sha256 verifier nil nil t))))
                   (pai-mcp-auth-test--respond client 200 "{\"access_token\":\"browser-token\",\"token_type\":\"Bearer\"}")))
                (t (pai-mcp-auth-test--respond client 404 "{}"))))))
      (setq base (format "http://127.0.0.1:%s" (process-contact fixture :service))
            def (list :url (concat base "/mcp")))
      (cl-letf (((symbol-function 'pai-mcp--server-def) (lambda (&rest _) def))
                ((symbol-function 'browse-url) (lambda (url &rest _) (setq auth-url url))))
        (pai-mcp-auth-login "browser" (lambda (h) (setq result h)) (lambda (e) (setq failure e)))
        (should (pai-mcp-auth-test--pump (lambda () (or auth-url failure))))
        (should-not failure)
        (let* ((query (url-parse-query-string (cadr (split-string auth-url "?"))))
               (state (cadr (assoc "state" query)))
               (u (url-generic-parse-url redirect)))
          (setq challenge (cadr (assoc "code_challenge" query)))
          (cl-labels ((callback (sent-state)
                        (setq callback-client
                              (make-network-process :name "pai-auth-browser" :noquery t
                                                    :host "127.0.0.1" :service (url-port u)
                                                    :filter #'ignore))
                        (push callback-client (process-get fixture 'clients))
                        (process-send-string callback-client
                                             (format "GET %s?code=good-code&state=%s HTTP/1.1\r\nHost: localhost\r\n\r\n"
                                                     (url-filename u) sent-state))))
            (callback "wrong-state")
            (should (pai-mcp-auth-test--pump (lambda () (not (process-live-p callback-client)))))
            (should (= exchanges 0))
            (should-not failure)
            (callback state)
            (should (pai-mcp-auth-test--pump (lambda () (or result failure))))
            (should-not failure)
            (should (= exchanges 1))
            (should (equal "Bearer browser-token" (cdr (assoc "Authorization" result))))))))))

(ert-deftest pai-mcp-auth-logout-cancels-in-flight-and-callback-once ()
  (skip-unless (executable-find "curl"))
  (pai-mcp-auth-test--isolated
    (let (received (count 0) failure def base)
      (setq fixture (pai-mcp-auth-test--server (lambda (_client _header _body) (setq received t))))
      (setq base (format "http://127.0.0.1:%s" (process-contact fixture :service))
            def (list :url (concat base "/mcp") :auth "oauth"
                      :oauth (list :grantType "client_credentials" :clientId "client"
                                   :authServerMetadataUrl (concat base "/.well-known/oauth-authorization-server"))))
      (cl-letf (((symbol-function 'pai-mcp--server-def) (lambda (&rest _) def)))
        (pai-mcp-auth-login "cancel" (lambda (_) (cl-incf count))
                            (lambda (e) (setq failure e) (cl-incf count)))
        (should (pai-mcp-auth-test--pump (lambda () received)))
        (pai-mcp-auth-logout "cancel")
        (accept-process-output nil 0.05)
        (should failure)
        (should (= count 1))
        (should-not (file-exists-p (pai-mcp-auth--file "cancel")))))))

(ert-deftest pai-mcp-auth-basic-negotiation-and-untrusted-token-origin ()
  (skip-unless (executable-find "curl"))
  (pai-mcp-auth-test--isolated
    (let (base def result failure hostile (exchanges 0))
      (setq fixture
            (pai-mcp-auth-test--server
             (lambda (client header body)
               (if (string-prefix-p "GET /.well-known/oauth-authorization-server " header)
                   (pai-mcp-auth-test--respond
                    client 200
                    (pai-json-encode
                     (list :issuer base
                           :token_endpoint (if hostile "https://untrusted.invalid/token" (concat base "/token"))
                           :token_endpoint_auth_methods_supported ["client_secret_basic"])))
                 (cl-incf exchanges)
                 (should (string-match-p
                          (regexp-quote (concat "Authorization: Basic "
                                                (base64-encode-string "client:password" t))) header))
                 (should-not (assoc "client_secret" (url-parse-query-string body)))
                 (pai-mcp-auth-test--respond client 200
                                            "{\"access_token\":\"basic-token\",\"token_type\":\"Bearer\"}")))))
      (setq base (format "http://127.0.0.1:%s" (process-contact fixture :service))
            def (list :url (concat base "/mcp") :auth "oauth"
                      :oauth (list :grantType "client_credentials" :clientId "client" :clientSecret "password"
                                   :authServerMetadataUrl (concat base "/.well-known/oauth-authorization-server"))))
      (cl-letf (((symbol-function 'pai-mcp--server-def) (lambda (&rest _) def)))
        (pai-mcp-auth-login "basic" (lambda (h) (setq result h)) (lambda (e) (setq failure e)))
        (should (pai-mcp-auth-test--pump (lambda () (or result failure))))
        (should-not failure)
        (should (equal "Bearer basic-token" (cdr (assoc "Authorization" result))))
        (setq hostile t result nil)
        (pai-mcp-auth-login "hostile" (lambda (h) (setq result h)) (lambda (e) (setq failure e)))
        (should (pai-mcp-auth-test--pump (lambda () (or result failure))))
        (should failure)
        (should-not result)
        (should (= exchanges 1))
        (should-not (file-exists-p (pai-mcp-auth--file "hostile")))))))

(provide 'pai-mcp-auth-test)
;;; pai-mcp-auth-test.el ends here
