;;; pai-kimi-test.el --- Tests for the Kimi Code provider -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-auth)
(require 'pai-kimi)

(defmacro pai-kimi-test--sandbox (&rest body)
  "Run BODY with a throwaway pai directory and credential store."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "pai-kimi" t)))
          (pai-directory dir)
          (pai-auth-storage 'plaintext)
          (pai-auth-gpg-recipient nil)
          (pai-auth--store 'unloaded)
          (auth-sources nil)
          (pai-kimi--device-id nil)
          (pai-kimi--login nil)
          (pai-kimi--refreshing nil)
          (pai-kimi--rejected-refresh nil)
          (pai-kimi--refresh-timer nil))
     (unwind-protect (progn ,@body)
       (when (timerp pai-kimi--refresh-timer) (cancel-timer pai-kimi--refresh-timer))
       (when (timerp (plist-get pai-kimi--login :timer))
         (cancel-timer (plist-get pai-kimi--login :timer)))
       (delete-directory dir t))))

(defun pai-kimi-test--header (req name)
  "Return header NAME (case-insensitive) of request REQ."
  (cdr (assoc-string name (plist-get req :headers) t)))

(defconst pai-kimi-test--model
  '(:id "kimi-for-coding" :provider "kimi" :api openai-completions
    :base-url "https://api.kimi.ai/coding/v1" :reasoning t :reasoning-off "none"))

(defun pai-kimi-test--request (messages reasoning)
  "Build a Kimi request over MESSAGES with thinking level REASONING."
  (cl-letf (((symbol-function 'pai-kimi--tick) #'ignore))
    (pai-kimi-build-request pai-kimi-test--model (list :messages messages)
                            (list :api-key "TOKEN" :reasoning reasoning))))

;;;; Requests

(ert-deftest pai-kimi-request-identity-headers ()
  "Requests carry the Bearer token and Kimi CLI's identity headers."
  (pai-kimi-test--sandbox
    (let ((req (pai-kimi-test--request (list (pai-user-message "hi")) 'high)))
      (should (equal (plist-get req :url) "https://api.kimi.ai/coding/v1/chat/completions"))
      (should (equal (pai-kimi-test--header req "Authorization") "Bearer TOKEN"))
      (should (equal (pai-kimi-test--header req "User-Agent")
                     (concat "KimiCLI/" pai-kimi-cli-version)))
      (should (equal (pai-kimi-test--header req "X-Msh-Platform") "kimi_cli"))
      (should (equal (pai-kimi-test--header req "X-Msh-Version") pai-kimi-cli-version))
      (should (string-match-p "\\`[0-9a-f]\\{32\\}\\'"
                              (pai-kimi-test--header req "X-Msh-Device-Id")))
      ;; the device id persists
      (should (equal (pai-kimi-test--header req "X-Msh-Device-Id")
                     (let ((pai-kimi--device-id nil)) (pai-kimi--device-id))))
      (dolist (h (plist-get req :headers))
        (should (string-match-p "\\`[[:ascii:]]*\\'" (cdr h)))))))

(ert-deftest pai-kimi-request-thinking-switch ()
  "Thinking uses Kimi's `thinking.type', never `reasoning_effort'."
  (pai-kimi-test--sandbox
    (let ((on (plist-get (pai-kimi-test--request (list (pai-user-message "hi")) 'high) :body))
          (off (plist-get (pai-kimi-test--request (list (pai-user-message "hi")) nil) :body)))
      (should (equal (plist-get on :thinking) '(:type "enabled")))
      (should (equal (plist-get off :thinking) '(:type "disabled")))
      (should-not (plist-member on :reasoning_effort))
      (should-not (plist-member off :reasoning_effort))
      (should (equal (plist-get on :model) "kimi-for-coding")))))

(ert-deftest pai-kimi-request-reasoning-content ()
  "Assistant turns that thought send their reasoning back."
  (pai-kimi-test--sandbox
    (let* ((assistant (list :role 'assistant
                            :content (list (pai-thinking "plan it")
                                           (list :type 'tool-call :id "c1" :name "ls"
                                                 :arguments '(:path ".")))))
           (plain (list :role 'assistant :content (list (pai-text "done"))))
           (body (plist-get (pai-kimi-test--request
                             (list (pai-user-message "hi") assistant plain) 'high)
                            :body))
           (msgs (plist-get body :messages)))
      (should (= (length msgs) 3))
      (should (equal (plist-get (nth 1 msgs) :reasoning_content) "plan it"))
      (should (plist-get (nth 1 msgs) :tool_calls))
      (should-not (plist-member (nth 2 msgs) :reasoning_content)))))

;;;; Credentials and refresh

(ert-deftest pai-kimi-refresh-due ()
  "Refresh once less than max(300s, half the lifetime) remains."
  (let ((now (pai-now-ms)))
    (should-not (pai-kimi--refresh-due-p
                 (list :refresh "r" :lifetime 3600 :expires (+ now (* 3000 1000)))))
    (should (pai-kimi--refresh-due-p
             (list :refresh "r" :lifetime 3600 :expires (+ now (* 1700 1000)))))
    (should (pai-kimi--refresh-due-p
             (list :refresh "r" :lifetime 100 :expires (+ now (* 250 1000)))))
    (should-not (pai-kimi--refresh-due-p
                 (list :lifetime 100 :expires now)))))

(ert-deftest pai-kimi-refresh-async-stores-token ()
  "A refresh stores the new token and keeps the old refresh token if omitted."
  (pai-kimi-test--sandbox
    (pai-auth-set "kimi" (list :type "oauth" :access "old" :refresh "R1"
                               :expires 0 :lifetime 900))
    (let (sent got)
      (cl-letf (((symbol-function 'pai-kimi--post-form)
                 (lambda (path params cb)
                   (setq sent (list path params))
                   (funcall cb 200 '(:access_token "new" :expires_in 900))
                   'proc)))
        (pai-kimi-refresh (lambda (c) (setq got c))))
      (should (equal (car sent) "/api/oauth/token"))
      (should (equal (cdr (assoc "grant_type" (cadr sent))) "refresh_token"))
      (should (equal (cdr (assoc "refresh_token" (cadr sent))) "R1"))
      (should (equal (plist-get got :access) "new"))
      (should (equal (pai-auth-api-key "kimi") "new"))
      (should (equal (plist-get (pai-auth-get "kimi") :refresh) "R1"))
      (should-not pai-kimi--refreshing))))

(ert-deftest pai-kimi-rejected-refresh-not-retried ()
  "A rejected refresh token is not sent again."
  (pai-kimi-test--sandbox
    (pai-auth-set "kimi" (list :type "oauth" :access "old" :refresh "R1" :expires 0))
    (let ((calls 0))
      (cl-letf (((symbol-function 'pai-kimi--post-form)
                 (lambda (_path _params cb) (cl-incf calls)
                   (funcall cb 401 '(:error "invalid_grant")) 'proc))
                ((symbol-function 'message) #'ignore))
        (pai-kimi-refresh)
        (pai-kimi-refresh))
      (should (= calls 1)))))

(ert-deftest pai-kimi-expired-token-never-blocks ()
  "Resolving an expired token starts a background refresh and returns at once."
  (pai-kimi-test--sandbox
    (let ((pai-auth-oauth-refresh-handlers
           (list (cons "kimi" #'pai-kimi--refresh-handler)))
          started)
      (pai-auth-set "kimi" (list :type "oauth" :access "stale" :refresh "R" :expires 1))
      (cl-letf (((symbol-function 'pai-kimi--post-form)
                 (lambda (&rest _) (setq started t) 'proc)))
        (should (equal (pai-auth-api-key "kimi") "stale")))
      (should started))))

;;;; Device login

(ert-deftest pai-kimi-device-login-flow ()
  "Device authorization, pending polls, then success stores the token."
  (pai-kimi-test--sandbox
    (let ((responses
           (list (cons 200 '(:device_code "DC" :user_code "AB-12" :interval 1
                             :expires_in 600
                             :verification_uri_complete "https://www.kimi.ai/code/authorize_device?user_code=AB-12"))
                 (cons 400 '(:error "authorization_pending"))
                 (cons 400 '(:error "slow_down"))
                 (cons 200 '(:access_token "ACC" :refresh_token "REF"
                             :expires_in 900 :token_type "Bearer"))))
          sent opened notes)
      (cl-letf (((symbol-function 'pai-kimi--post-form)
                 (lambda (path params cb)
                   (push (cons path params) sent)
                   (let ((r (pop responses))) (funcall cb (car r) (cdr r)))
                   'proc))
                ((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url)))
                ((symbol-function 'pai-kimi--note) (lambda (_b text) (push text notes)))
                ((symbol-function 'run-at-time) (lambda (&rest _) 'timer)))
        (should (stringp (pai-kimi-login)))
        (should (string-match-p "AB-12" (car notes)))
        (should (string-prefix-p "https://www.kimi.ai/code/authorize_device" opened))
        (should (equal (plist-get pai-kimi--login :device-code) "DC"))
        (pai-kimi--poll pai-kimi--login)   ; pending
        (should pai-kimi--login)
        (pai-kimi--poll pai-kimi--login)   ; slow_down
        (should (= (plist-get pai-kimi--login :interval) 6))
        (pai-kimi--poll pai-kimi--login))  ; success
      (should-not pai-kimi--login)
      (should (string-match-p "Logged in to Kimi" (car notes)))
      (let ((cred (pai-auth-get "kimi")))
        (should (equal (plist-get cred :type) "oauth"))
        (should (equal (plist-get cred :access) "ACC"))
        (should (equal (plist-get cred :refresh) "REF"))
        (should (equal (plist-get cred :lifetime) 900)))
      (should (equal (pai-auth-api-key "kimi") "ACC"))
      (should (equal (car (last sent)) (cons "/api/oauth/device_authorization"
                                             `(("client_id" . ,pai-kimi--client-id)))))
      (should (equal (cdr (assoc "grant_type" (cdar sent)))
                     "urn:ietf:params:oauth:grant-type:device_code")))))

(ert-deftest pai-kimi-device-login-denied ()
  "A denied authorization stops polling and reports the failure."
  (pai-kimi-test--sandbox
    (let (notes)
      (cl-letf (((symbol-function 'pai-kimi--post-form)
                 (lambda (_p _params cb) (funcall cb 400 '(:error "access_denied")) 'proc))
                ((symbol-function 'pai-kimi--note) (lambda (_b text) (push text notes)))
                ((symbol-function 'run-at-time) (lambda (&rest _) 'timer)))
        (pai-kimi--poll (setq pai-kimi--login
                              (list :device-code "DC" :interval 5
                                    :deadline (+ (float-time) 100)))))
      (should-not pai-kimi--login)
      (should (string-match-p "access_denied" (car notes)))
      (should-not (pai-auth-get "kimi")))))

(ert-deftest pai-kimi-login-command-shows-async-message ()
  "`/login kimi' shows the handler's status string instead of success/failure."
  (pai-kimi-test--sandbox
    (let ((pai-auth-oauth-handlers
           (list (cons "kimi" (lambda () "Kimi login started via X…")))))
      (should (equal (plist-get (pai-auth-login-command "kimi" nil) :message)
                     "Kimi login started via X…")))
    (let ((pai-auth-oauth-handlers (list (cons "kimi" (lambda () t)))))
      (should (equal (plist-get (pai-auth-login-command "kimi" nil) :message)
                     "Logged in to kimi")))))

;;;; Registration

(ert-deftest pai-kimi-extension-registers-provider ()
  "The extension registers the provider, a model, login and refresh."
  (pai-kimi-test--sandbox
    (let ((pai--providers (make-hash-table :test 'equal))
          (pai--models (make-hash-table :test 'equal))
          (pai-provider-env-keys nil)
          (pai-auth-oauth-handlers nil)
          (pai-auth-oauth-refresh-handlers nil))
      (pai-kimi-extension nil)
      (let ((provider (pai-provider "kimi")))
        (should (eq (plist-get provider :build-request) #'pai-kimi-build-request))
        (should (equal (plist-get provider :base-url) pai-kimi-base-url)))
      (should (pai-model "kimi/kimi-for-coding"))
      (should (pai-model-reasoning-p (pai-model "kimi/kimi-for-coding")))
      (should (eq (cdr (assoc "kimi" pai-auth-oauth-handlers)) #'pai-kimi-login))
      (should (member "kimi" (pai-auth--login-completions ""))))))

(provide 'pai-kimi-test)
;;; pai-kimi-test.el ends here
