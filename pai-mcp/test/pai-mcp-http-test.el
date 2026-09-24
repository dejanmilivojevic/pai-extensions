;;; pai-mcp-http-test.el --- HTTP MCP integration tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-mcp-client)
(require 'pai-mcp)
(load (expand-file-name "fixtures/pai-mcp-http-server.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun pai-mcp-http-test--pump (predicate &optional timeout)
  "Pump process output until PREDICATE or TIMEOUT, as in the stdio MCP tests."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

(defmacro pai-mcp-http-test--with-server (handler extra &rest body)
  "Run BODY in an isolated project with local HANDLER and config EXTRA."
  (declare (indent 2))
  `(progn
     (skip-unless (executable-find "curl"))
     (let* ((dir (file-name-as-directory (make-temp-file "pai-mcp-http" t)))
            (pai-directory (expand-file-name ".state" dir))
            (pai-mcp--servers (make-hash-table :test 'equal))
            (pai-mcp--metadata nil)
            (pai-mcp--metadata-loaded nil)
            (default-directory dir)
            (process-environment (copy-sequence process-environment))
            (fixture (pai-mcp-http-fixture-start ,handler)))
       ;; Never route fixture traffic through the developer's proxy.
       (setenv "NO_PROXY" "127.0.0.1")
       (setenv "no_proxy" "127.0.0.1")
       (unwind-protect
           (progn
             (pai-mcp-http-test--config dir fixture ,extra)
             ,@body)
         (maphash (lambda (name _server) (ignore-errors (pai-mcp-stop name)))
                  pai-mcp--servers)
         (pai-mcp-http-fixture-stop fixture)
         (delete-directory dir t)))))

(defun pai-mcp-http-test--config (dir fixture extra)
  (with-temp-file (expand-file-name ".mcp.json" dir)
    (insert (pai-json-encode
             (list :mcpServers
                   (list :http (append extra
                                       (list :url (pai-mcp-http-fixture-url fixture)
                                             :requestTimeoutMs 3000))))))))

(defun pai-mcp-http-test--text (result)
  (pai-content-text (plist-get result :content)))

(ert-deftest pai-mcp-http-initialized-precedes-tools-list ()
  "A delayed notification signer cannot let catalog discovery overtake it."
  (let (initialized)
    (pai-mcp-http-test--with-server
        (lambda (server client request)
          (let ((method (plist-get (plist-get request :json) :method)))
            (when (equal method "notifications/initialized")
              (setq initialized t))
            (if (and (equal method "tools/list") (not initialized))
                (pai-mcp-http-fixture-response client 409 "Not initialized")
              (pai-mcp-http-fixture-modern server client request))))
        (list :requestHeadersCommand
              (pai-mcp-http-test--command
               '(progn
                  (require 'json)
                  (let* ((envelope (json-parse-string (read-from-minibuffer "")
                                                     :object-type 'plist))
                         (body (json-parse-string
                                (base64-decode-string (plist-get envelope :bodyBase64))
                                :object-type 'plist)))
                    (when (equal (plist-get body :method) "notifications/initialized")
                      (sleep-for 0.3))
                    (princ "{}")))))
      (let (result)
        (pai-mcp-call "http" "echo" '(:text "ordered")
                      (lambda (value) (setq result value)) dir)
        (should-not result)
        (should (pai-mcp-http-test--pump (lambda () result)))
        (should-not (eq (plist-get result :is-error) t))
        (should (equal (pai-mcp-http-test--text result) "ordered"))))))

(ert-deftest pai-mcp-http-autostart-session-and-fragmented-sse ()
  "Lazy startup and a fragmented UTF-8 POST SSE reply never block the caller."
  (pai-mcp-http-test--with-server
      (lambda (server client request)
        (pai-mcp-http-fixture-modern server client request t)) nil
    (pai-mcp-startup-connect dir)
    (should (eq (pai-mcp-server-status "http") 'stopped))
    (should-not (pai-mcp-http-fixture-requests fixture))
    (let (result (count 0))
      (pai-mcp-call "http" "echo" '(:text "hello λ世界")
                    (lambda (value) (setq result value) (cl-incf count)) dir)
      ;; A blocking transport cannot complete: the fixture runs in this Emacs.
      (should-not result)
      (should (pai-mcp-http-test--pump (lambda () result)))
      (should-not (eq (plist-get result :is-error) t))
      (should (equal (pai-mcp-http-test--text result) "hello λ世界"))
      (should (eq (pai-mcp-server-status "http") 'ready))
      ;; Fixture rejects every post-initialize request without session/version.
      (should (= count 1))
      (pai-mcp-stop "http")
      (should (eq (pai-mcp-server-status "http") 'stopped))
      (should (= count 1)))))

(ert-deftest pai-mcp-http-stop-pending-and-reconnect ()
  "Stopping settles a pending call once; a late old reply cannot poison restart."
  (let ((old-request nil) (late-attempt nil))
    (pai-mcp-http-test--with-server
        (lambda (server client request)
          (if (and (equal (plist-get (plist-get request :json) :method) "tools/call")
                   (equal (plist-get (plist-get (plist-get (plist-get request :json) :params)
                                               :arguments) :text) "old"))
              (progn
                (setq old-request request)
                (pai-mcp-http-fixture-later
                 server 0.3
                 (lambda ()
                   (setq late-attempt t)
                   (pai-mcp-http-fixture-modern server client request))))
            (pai-mcp-http-fixture-modern server client request))) nil
      (let (old-result new-result (old-count 0) (new-count 0))
        (pai-mcp-call "http" "echo" '(:text "old")
                      (lambda (value) (setq old-result value) (cl-incf old-count)) dir)
        (should (pai-mcp-http-test--pump (lambda () old-request)))
        (should-not old-result)
        (pai-mcp-stop "http")
        (should (pai-mcp-http-test--pump (lambda () old-result)))
        (should (eq (plist-get old-result :is-error) t))
        (should (= old-count 1))
        (pai-mcp-call "http" "echo" '(:text "new")
                      (lambda (value) (setq new-result value) (cl-incf new-count)) dir)
        (should (pai-mcp-http-test--pump (lambda () (and new-result late-attempt))))
        (should-not (eq (plist-get new-result :is-error) t))
        (should (equal (pai-mcp-http-test--text new-result) "new"))
        (should (eq (pai-mcp-server-status "http") 'ready))
        (should (= old-count 1))
        (should (= new-count 1))))))

(ert-deftest pai-mcp-http-legacy-sse-fallback ()
  "A definitive modern rejection discovers the legacy endpoint and stream."
  (pai-mcp-http-test--with-server #'pai-mcp-http-fixture-legacy nil
    (let (result)
      (pai-mcp-call "http" "echo" '(:text "legacy λ")
                    (lambda (value) (setq result value)) dir)
      (should-not result)
      (should (pai-mcp-http-test--pump (lambda () result)))
      (should-not (eq (plist-get result :is-error) t))
      (should (equal (pai-mcp-http-test--text result) "legacy λ"))
      (let ((requests (reverse (pai-mcp-http-fixture-requests fixture))))
        (should (equal (mapcar (lambda (r) (plist-get r :method))
                              (cl-subseq requests 0 3)) '("POST" "GET" "POST")))
        (should (equal (plist-get (nth 2 requests) :path) "/messages?session=legacy"))
        (should (equal (plist-get (plist-get (plist-get (nth 2 requests) :json) :params)
                                 :protocolVersion) pai-mcp-protocol-version))))))

(ert-deftest pai-mcp-http-auth-and-unavailable-do-not-fallback ()
  "Authentication and transient service failures must not probe legacy SSE."
  (dolist (status '(401 403 503))
    (pai-mcp-http-test--with-server
        (lambda (_fixture client _request)
          (pai-mcp-http-fixture-response client status "Rejected")) nil
      (let (result (count 0))
        (pai-mcp-call "http" "echo" '(:text "denied")
                      (lambda (value) (setq result value) (cl-incf count)) dir)
        (should (pai-mcp-http-test--pump (lambda () result)))
        (should (eq (plist-get result :is-error) t))
        (should (string-match-p (number-to-string status) (pai-mcp-http-test--text result)))
        (should (= count 1))
        (should-not (cl-find "GET" (pai-mcp-http-fixture-requests fixture)
                             :key (lambda (request) (plist-get request :method))
                             :test #'equal))))))

(ert-deftest pai-mcp-http-ca-boundaries-reject-before-traffic ()
  "CA configuration cannot silently weaken TLS or apply to cleartext HTTP."
  (pai-mcp-http-test--with-server #'pai-mcp-http-fixture-modern nil
    (let ((certificate (expand-file-name "ca.pem" dir)))
      (with-temp-file certificate (insert "fixture, not a certificate"))
      (dolist (extra (list (list :caFile certificate)
                          (list :url (replace-regexp-in-string
                                      "^http:" "https:" (pai-mcp-http-fixture-url fixture))
                                :caFile (expand-file-name "missing-ca.pem" dir))))
        (pai-mcp-http-test--config dir fixture extra)
        (let (result (count 0))
          (pai-mcp-call "http" "echo" '(:text "must not send")
                        (lambda (value) (setq result value) (cl-incf count)) dir)
          (should (pai-mcp-http-test--pump (lambda () result)))
          (should (eq (plist-get result :is-error) t))
          (should (string-match-p "[Cc][Aa]\\|certificate" (pai-mcp-http-test--text result)))
          (should (= count 1))
          (should-not (pai-mcp-http-fixture-requests fixture))
          (pai-mcp-stop "http"))))))

(defun pai-mcp-http-test--command (form)
  "Return a shell command evaluating FORM in a fresh batch Emacs."
  (mapconcat #'shell-quote-argument
             (list (expand-file-name invocation-name invocation-directory)
                   "-Q" "--batch" "--eval" (prin1-to-string form)) " "))

(ert-deftest pai-mcp-http-signed-established-error-settles-once ()
  "An HTTP error after signed startup settles the call, not its timeout."
  (pai-mcp-http-test--with-server
      (lambda (server client request)
        (let* ((json (plist-get request :json))
               (text (plist-get (plist-get (plist-get json :params) :arguments) :text)))
          (if (and (equal (plist-get json :method) "tools/call")
                   (equal text "denied"))
              (pai-mcp-http-fixture-response client 403 "Forbidden")
            (pai-mcp-http-fixture-modern server client request))))
      (list :requestHeadersCommand
            (pai-mcp-http-test--command '(princ "{\"X-Signed\":\"yes\"}"))
            :requestTimeoutMs 10000)
    (let (ready-result result drained (count 0))
      (pai-mcp-call "http" "echo" '(:text "ready")
                    (lambda (value) (setq ready-result value)) dir)
      (should (pai-mcp-http-test--pump (lambda () ready-result)))
      (should-not (eq (plist-get ready-result :is-error) t))
      (should (equal (pai-mcp-http-test--text ready-result) "ready"))
      (should (eq (pai-mcp-server-status "http") 'ready))
      (pai-mcp-call "http" "echo" '(:text "denied")
                    (lambda (value) (setq result value) (cl-incf count)) dir)
      (should-not result)
      ;; Leave a wide margin below the ten-second request timeout.
      (should (pai-mcp-http-test--pump (lambda () result) 3))
      (should (eq (plist-get result :is-error) t))
      (should (string-match-p "403" (pai-mcp-http-test--text result)))
      (should-not (string-match-p "[Tt]ime" (pai-mcp-http-test--text result)))
      (should (= count 1))
      (pai-mcp-stop "http")
      (pai-mcp-http-fixture-later fixture 0.1 (lambda () (setq drained t)))
      (should (pai-mcp-http-test--pump (lambda () drained)))
      (should (= count 1)))))

(ert-deftest pai-mcp-http-headers-and-exact-utf8-signing ()
  "Every request is signed over its exact transmitted bytes, asynchronously."
  (let* ((command
          (pai-mcp-http-test--command
           '(progn
              (require 'json)
              (let* ((input (json-parse-string (read-from-minibuffer "") :object-type 'plist))
                     (body (base64-decode-string (plist-get input :bodyBase64))))
                (unless (and (= (plist-get input :version) 1)
                             (member (plist-get input :method) '("POST" "GET" "DELETE"))
                             (string-prefix-p "http://127.0.0.1:" (plist-get input :url)))
                  (kill-emacs 2))
                (sleep-for 0.08)
                (princ (json-serialize
                        (list :X-Signature (secure-hash 'sha256 body)
                              :X-Signed-Method (plist-get input :method)
                              :X-Signed-Url (plist-get input :url)
                              :X-Override "signed")))))))
         (dynamic (concat "!" (pai-mcp-http-test--command '(princ "dynamic-token")))))
    (pai-mcp-http-test--with-server
        (lambda (server client request)
          (let ((headers (plist-get request :headers)))
            (if (and (equal (cdr (assoc "x-static" headers)) "static-token")
                     (equal (cdr (assoc "x-dynamic" headers)) "dynamic-token")
                     (equal (cdr (assoc "x-literal" headers)) "!literal-token")
                     (equal (cdr (assoc "x-override" headers)) "signed")
                     (equal (cdr (assoc "x-signature" headers))
                            (secure-hash 'sha256 (plist-get request :body)))
                     (equal (cdr (assoc "x-signed-method" headers)) (plist-get request :method))
                     (equal (cdr (assoc "x-signed-url" headers))
                            (pai-mcp-http-fixture-url server (plist-get request :path))))
                (pai-mcp-http-fixture-modern server client request)
              (pai-mcp-http-fixture-response client 401 "Signature or header mismatch"))))
        (list :headers (list :X-Static "static-token" :X-Dynamic dynamic
                             :X-Literal "!!literal-token" :X-Override "static")
              :requestHeadersCommand command)
      (let (result heartbeat)
        (pai-mcp-call "http" "echo" '(:text "λ世界\n\"signed\"")
                      (lambda (value) (setq result value)) dir)
        (should-not result)
        (pai-mcp-http-fixture-later fixture 0.01 (lambda () (setq heartbeat t)))
        (should (pai-mcp-http-test--pump (lambda () heartbeat)))
        (should-not result)
        (should (pai-mcp-http-test--pump (lambda () result)))
        (should-not (eq (plist-get result :is-error) t))
        (should (equal (pai-mcp-http-test--text result) "λ世界\n\"signed\""))
        ;; A second body must get a fresh signature, not cached command output.
        (setq result nil)
        (pai-mcp-call "http" "echo" '(:text "different")
                      (lambda (value) (setq result value)) dir)
        (should (pai-mcp-http-test--pump (lambda () result)))
        (should-not (eq (plist-get result :is-error) t))
        (should (equal (pai-mcp-http-test--text result) "different"))))))

(ert-deftest pai-mcp-http-header-helper-failure-never-sends ()
  "Failing or empty header commands settle once, without sending any traffic."
  (dolist (form '((kill-emacs 7) (princ "")))
    (pai-mcp-http-test--with-server #'pai-mcp-http-fixture-modern
        (list :headers
              (list :Authorization
                    (concat "!" (pai-mcp-http-test--command form))))
      (let (result (count 0))
        (pai-mcp-call "http" "echo" '(:text "denied")
                      (lambda (value) (setq result value) (cl-incf count)) dir)
        (should-not result)
        (should (pai-mcp-http-test--pump (lambda () result)))
        (should (eq (plist-get result :is-error) t))
        (should-not (pai-mcp-http-fixture-requests fixture))
        (pai-mcp-stop "http")
        (should (= count 1))))))

(ert-deftest pai-mcp-http-signer-malformed-output-never-sends ()
  "Malformed or non-string signing output is an error, not a silently unsigned request."
  (dolist (output '("not-json" "[]" "[{}]" "{\"Authorization\":12}"))
    (pai-mcp-http-test--with-server #'pai-mcp-http-fixture-modern
        (list :requestHeadersCommand (pai-mcp-http-test--command `(princ ,output)))
      (let (result)
        (pai-mcp-call "http" "echo" '(:text "denied")
                      (lambda (value) (setq result value)) dir)
        (should (pai-mcp-http-test--pump (lambda () result)))
        (should (eq (plist-get result :is-error) t))
        (should-not (pai-mcp-http-fixture-requests fixture))))))

(ert-deftest pai-mcp-http-signer-empty-object-succeeds ()
  "An empty JSON object is a valid signer result with no extra headers."
  (pai-mcp-http-test--with-server #'pai-mcp-http-fixture-modern
      (list :requestHeadersCommand (pai-mcp-http-test--command '(princ "{}")))
    (let (result (count 0))
      (pai-mcp-call "http" "echo" '(:text "empty signature")
                    (lambda (value) (setq result value) (cl-incf count)) dir)
      (should (pai-mcp-http-test--pump (lambda () result)))
      (should-not (eq (plist-get result :is-error) t))
      (should (equal (pai-mcp-http-test--text result) "empty signature"))
      (pai-mcp-stop "http")
      (should (= count 1)))))

(ert-deftest pai-mcp-http-signed-legacy-sse-fallback ()
  "Legacy fallback signs both the discovery GET and every JSON POST."
  (pai-mcp-http-test--with-server
      (lambda (server client request)
        (if (equal (cdr (assoc "x-signed-method" (plist-get request :headers)))
                   (plist-get request :method))
            (pai-mcp-http-fixture-legacy server client request)
          (pai-mcp-http-fixture-response client 401 "Missing request signature")))
      (list :requestHeadersCommand
            (pai-mcp-http-test--command
             '(progn
                (require 'json)
                (let ((input (json-parse-string (read-from-minibuffer "")
                                               :object-type 'plist)))
                  (princ (json-serialize
                          (list :X-Signed-Method (plist-get input :method))))))))
    (let (result (count 0))
      (pai-mcp-call "http" "echo" '(:text "signed legacy λ")
                    (lambda (value) (setq result value) (cl-incf count)) dir)
      (should-not result)
      (should (pai-mcp-http-test--pump (lambda () result)))
      (should-not (eq (plist-get result :is-error) t))
      (should (equal (pai-mcp-http-test--text result) "signed legacy λ"))
      (should (cl-find "GET" (pai-mcp-http-fixture-requests fixture)
                       :key (lambda (request) (plist-get request :method))
                       :test #'equal))
      (should (cl-find "/messages?session=legacy" (pai-mcp-http-fixture-requests fixture)
                       :key (lambda (request) (plist-get request :path))
                       :test #'equal))
      (pai-mcp-stop "http")
      (should (= count 1)))))

(ert-deftest pai-mcp-http-pinned-modern-does-not-fallback ()
  "Explicit modern protocol selection never probes the legacy SSE endpoint."
  (pai-mcp-http-test--with-server #'pai-mcp-http-fixture-legacy
      '(:protocolVersion "2026-07-28")
    (let (result)
      (pai-mcp-call "http" "echo" '(:text "pinned")
                    (lambda (value) (setq result value)) dir)
      (should (pai-mcp-http-test--pump (lambda () result)))
      (should (eq (plist-get result :is-error) t))
      (should-not (cl-find "GET" (pai-mcp-http-fixture-requests fixture)
                           :key (lambda (request) (plist-get request :method))
                           :test #'equal)))))

(ert-deftest pai-mcp-http-signer-timeout-is-nonblocking ()
  "An unresponsive helper has a bounded lifetime while Emacs timers run."
  (pai-mcp-http-test--with-server #'pai-mcp-http-fixture-modern
      (list :requestHeadersCommand
            (list :command (expand-file-name invocation-name invocation-directory)
                  :args (list "-Q" "--batch" "--eval" "(sleep-for 30)")
                  :timeoutMs 200))
    (let (result heartbeat (count 0))
      (pai-mcp-call "http" "echo" '(:text "never transmitted")
                    (lambda (value) (setq result value) (cl-incf count)) dir)
      (should-not result)
      (pai-mcp-http-fixture-later fixture 0.01 (lambda () (setq heartbeat t)))
      (should (pai-mcp-http-test--pump (lambda () heartbeat)))
      (should-not result)
      (should (pai-mcp-http-test--pump (lambda () result) 3))
      (should (eq (plist-get result :is-error) t))
      (should (string-match-p "[Tt]ime" (pai-mcp-http-test--text result)))
      (should-not (pai-mcp-http-fixture-requests fixture))
      (pai-mcp-stop "http")
      (should (= count 1)))))

(ert-deftest pai-mcp-http-stop-during-signer-ignores-stale-work ()
  "Stopping during a real command cancels its request across a new generation."
  (pai-mcp-http-test--with-server #'pai-mcp-http-fixture-modern nil
    (let* ((marker (expand-file-name "signer-started" dir))
           (command (pai-mcp-http-test--command
                     `(progn (with-temp-file ,marker (insert "started"))
                             (sleep-for 0.3) (princ "{}"))))
           old-result new-result (count 0) elapsed)
      (pai-mcp-http-test--config dir fixture (list :requestHeadersCommand command))
      (pai-mcp-call "http" "echo" '(:text "old signer")
                    (lambda (value) (setq old-result value) (cl-incf count)) dir)
      (should (pai-mcp-http-test--pump (lambda () (file-exists-p marker))))
      (should-not old-result)
      (pai-mcp-stop "http")
      (should (eq (plist-get old-result :is-error) t))
      (pai-mcp-http-test--config dir fixture nil)
      (pai-mcp-call "http" "echo" '(:text "new connection")
                    (lambda (value) (setq new-result value)) dir)
      (pai-mcp-http-fixture-later fixture 0.4 (lambda () (setq elapsed t)))
      (should (pai-mcp-http-test--pump (lambda () (and elapsed new-result))))
      (should (= count 1))
      (should-not (eq (plist-get new-result :is-error) t))
      (should (equal (pai-mcp-http-test--text new-result) "new connection"))
      (should-not
       (cl-find "old signer" (pai-mcp-http-fixture-requests fixture)
                :key (lambda (request)
                       (plist-get (plist-get (plist-get (plist-get request :json)
                                                         :params) :arguments) :text))
                :test #'equal)))))

(provide 'pai-mcp-http-test)
;;; pai-mcp-http-test.el ends here
