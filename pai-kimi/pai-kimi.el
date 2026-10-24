;;; pai-kimi.el --- Kimi Code provider for pai (Kimi account login) -*- lexical-binding: t; -*-

;;; Commentary:

;; Talks to Kimi the way the official Kimi Code CLI (kimi-cli) does, so a
;; Kimi membership works without a separate API key:
;;
;; - `/login kimi' runs Kimi Code's OAuth device flow against
;;   `pai-kimi-oauth-host': pai shows a verification URL (and opens it), you
;;   confirm in the browser where you are signed in to Kimi, and pai polls
;;   for the token in the background.  Tokens are stored in ~/.pai/auth.json.
;; - Requests go to the Kimi Code OpenAI-compatible endpoint
;;   (`pai-kimi-base-url', /chat/completions) with the access token as Bearer
;;   and Kimi CLI's identity headers (User-Agent KimiCLI/<version> and the
;;   X-Msh-* device headers).
;; - Thinking is switched with `thinking: {type: enabled|disabled}' and the
;;   reasoning of earlier assistant turns is sent back as `reasoning_content',
;;   as Kimi requires for tool-call turns with thinking on.
;; - The token is refreshed asynchronously before it expires (Kimi CLI's
;;   rule: once less than max(300s, half its lifetime) remains).  Nothing
;;   here blocks the UI: every HTTP exchange runs in curl (`pai-http-request').
;; - `/usage' (and the header line) shows the Kimi Code quota windows from
;;   the /usages endpoint, like Kimi CLI's /usage.
;;
;; Outside mainland China the kimi.ai hosts are used (the default); set
;; `pai-kimi-oauth-host' / `pai-kimi-base-url' to the kimi.com hosts if
;; needed.  A Kimi Code API key also works: KIMI_API_KEY, `pai-api-keys', or
;; a stored api_key credential for "kimi".

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-http)
(require 'pai-usage)
(require 'pai)
(require 'pai-providers)
(require 'pai-provider-openai)
(require 'pai-auth)

(defgroup pai-kimi nil
  "Kimi Code provider for pai."
  :group 'pai
  :prefix "pai-kimi-")

(defcustom pai-kimi-oauth-host "https://auth.kimi.ai"
  "Kimi OAuth host (device authorization and token endpoints).
Use \"https://auth.kimi.com\" for the kimi.com region."
  :type 'string)

(defcustom pai-kimi-base-url "https://api.kimi.ai/coding/v1"
  "Kimi Code API base URL (OpenAI-compatible).
Use \"https://api.kimi.com/coding/v1\" for the kimi.com region.  Takes effect
when the extension is (re)loaded."
  :type 'string)

(defcustom pai-kimi-cli-version "1.52.0"
  "Kimi CLI version reported in the User-Agent and X-Msh-Version headers."
  :type 'string)

(defconst pai-kimi--client-id "17e5f671-d194-4dfb-9706-5516cb48c098"
  "Public OAuth client id of the Kimi Code CLI.")

(defcustom pai-kimi-max-output-tokens 32768
  "Output token limit requested for Kimi Code replies.
Kimi's /models does not declare one; pai's generic default (8192) cut long
replies and tool calls short."
  :type 'integer)

(defcustom pai-kimi-thinking-required-regexp "\\`k3\\.1-pro"
  "Regexp of model ids that refuse thinking \"disabled\".
For them thinking is always enabled, even at thinking level off (K3.1 Pro
answers \"does not support disabled thinking mode\" otherwise)."
  :type 'regexp)

(defconst pai-kimi--fallback-models
  '((:id "kimi-for-coding" :name "Kimi for Coding" :context-window 262144
     :max-tokens 32768 :reasoning t))
  "Models offered before the account's model list is known.
The window is deliberately low: underestimating only compacts sooner.")

(defconst pai-kimi--http-timeout 30
  "Seconds before an OAuth HTTP exchange is abandoned.")

;;;; Identity headers

(defvar pai-kimi--device-id nil "Cached device id (see `pai-kimi--device-id').")

(defun pai-kimi--device-id ()
  "Return this machine's persistent Kimi device id (a random hex string)."
  (or pai-kimi--device-id
      (setq pai-kimi--device-id
            (let ((file (expand-file-name "device_id" (pai-state-directory "kimi"))))
              (or (and (file-readable-p file)
                       (let ((s (string-trim (with-temp-buffer
                                               (insert-file-contents file)
                                               (buffer-string)))))
                         (unless (string-empty-p s) s)))
                  (let ((id (pai-kimi--random-hex 16)))
                    (with-temp-file file (insert id))
                    (set-file-modes file #o600)
                    id))))))

(defun pai-kimi--random-hex (n)
  "Return N random bytes as lowercase hex."
  (mapconcat (lambda (b) (format "%02x" b))
             (string-to-list (pai-auth--random-bytes n)) ""))

(defun pai-kimi--ascii (s)
  "Return S stripped of non-ASCII characters, or \"unknown\" when empty."
  (let ((out (string-trim (replace-regexp-in-string "[^[:ascii:]]" "" (or s "")))))
    (if (string-empty-p out) "unknown" out)))

(defun pai-kimi--device-model ()
  "Return a Kimi CLI style device model string, e.g. \"Linux x86_64\"."
  (let ((arch (car (split-string system-configuration "-")))
        (os (pcase system-type
              ('gnu/linux "Linux") ('darwin "macOS") ('windows-nt "Windows")
              (_ (capitalize (symbol-name system-type))))))
    (string-trim (format "%s %s" os (or arch "")))))

(defun pai-kimi--common-headers ()
  "Return the X-Msh-* identity headers Kimi CLI sends, as an alist."
  (mapcar (lambda (kv) (cons (car kv) (pai-kimi--ascii (cdr kv))))
          `(("X-Msh-Platform" . "kimi_cli")
            ("X-Msh-Version" . ,pai-kimi-cli-version)
            ("X-Msh-Device-Name" . ,(system-name))
            ("X-Msh-Device-Model" . ,(pai-kimi--device-model))
            ("X-Msh-Os-Version" . ,(or (bound-and-true-p operating-system-release)
                                       (symbol-name system-type)))
            ("X-Msh-Device-Id" . ,(pai-kimi--device-id)))))

(defun pai-kimi--user-agent ()
  "Return the User-Agent Kimi CLI sends."
  (format "KimiCLI/%s" pai-kimi-cli-version))

;;;; Async OAuth HTTP

(defun pai-kimi--post-form (path params callback)
  "POST PARAMS (alist) form-encoded to PATH on `pai-kimi-oauth-host'.
CALLBACK receives (STATUS DATA): the HTTP status (nil on network failure or
timeout) and the decoded JSON plist.  It is called exactly once.  The
request runs in curl (`pai-http-request'), so not even the name lookup
blocks Emacs.  Return the process, or nil when the request could not start."
  (let* ((done nil)
         (finish (lambda (status data)
                   (unless done
                     (setq done t)
                     (condition-case err (funcall callback status data)
                       (error (message "pai-kimi: %s" (error-message-string err))))))))
    (condition-case err
        (pai-http-request
         :url (concat (string-remove-suffix "/" pai-kimi-oauth-host) path)
         :method "POST"
         :headers (append '(("Content-Type" . "application/x-www-form-urlencoded")
                            ("Accept" . "application/json"))
                          (pai-kimi--common-headers))
         :body (pai-auth--url-encode-params params)
         :timeout pai-kimi--http-timeout
         :on-done (lambda (status _headers text)
                    (let* ((text (and text (string-trim text)))
                           (data (and text (not (string-empty-p text))
                                      (ignore-errors (pai-json-decode text)))))
                      (funcall finish status (and (listp data) data)))))
      (error (funcall finish nil (list :error_description (error-message-string err)))
             nil))))

;;;; Credentials

(defun pai-kimi--store-token (data &optional previous)
  "Store token response DATA as the kimi oauth credential; return it.
PREVIOUS is the credential being refreshed (its refresh token is kept when
DATA has none)."
  (let ((access (plist-get data :access_token))
        (lifetime (plist-get data :expires_in)))
    (when (stringp access)
      (setq lifetime (if (numberp lifetime) lifetime 0))
      (pai-auth-set "kimi"
                    (list :type "oauth"
                          :access access
                          :refresh (or (plist-get data :refresh_token)
                                       (plist-get previous :refresh))
                          :expires (+ (pai-now-ms) (* lifetime 1000))
                          :lifetime lifetime)))))

(defun pai-kimi--oauth-cred ()
  "Return the stored kimi oauth credential, or nil."
  (let ((cred (pai-auth-get "kimi")))
    (and (equal (plist-get cred :type) "oauth") cred)))

(defun pai-kimi--refresh-due-p (cred)
  "Return non-nil when oauth CRED should be refreshed now.
Kimi CLI's rule: refresh once less than max(300s, half the lifetime) is left."
  (let ((expires (plist-get cred :expires))
        (lifetime (or (plist-get cred :lifetime) 0)))
    (and (plist-get cred :refresh) (numberp expires)
         (< (- expires (pai-now-ms))
            (* 1000 (max 300 (* 0.5 (if (numberp lifetime) lifetime 0))))))))

;;;; Token refresh (async)

(defvar pai-kimi--refreshing nil "Non-nil while a refresh request is in flight.")
(defvar pai-kimi--rejected-refresh nil
  "Refresh token the server rejected; not retried until a new login.")
(defvar pai-kimi--refresh-timer nil "Periodic refresh check timer.")

(defun pai-kimi-refresh (&optional callback)
  "Refresh the kimi oauth token asynchronously.
CALLBACK, when non-nil, receives the new credential or nil."
  (let* ((cred (pai-kimi--oauth-cred))
         (refresh (plist-get cred :refresh))
         (cb (lambda (new) (when callback (funcall callback new)))))
    (cond
     ((or (not (stringp refresh)) (equal refresh pai-kimi--rejected-refresh))
      (funcall cb nil))
     (pai-kimi--refreshing (funcall cb nil))
     (t
      (setq pai-kimi--refreshing t)
      (unless (pai-kimi--post-form
               "/api/oauth/token"
               `(("client_id" . ,pai-kimi--client-id)
                 ("grant_type" . "refresh_token")
                 ("refresh_token" . ,refresh))
               (lambda (status data)
                 (setq pai-kimi--refreshing nil)
                 (cond
                  ((and (eql status 200) (plist-get data :access_token))
                   (funcall cb (pai-kimi--store-token data cred)))
                  ((memq status '(400 401 403))
                   (setq pai-kimi--rejected-refresh refresh)
                   (message "pai-kimi: session expired (%s); run /login kimi"
                            (or (plist-get data :error_description) status))
                   (funcall cb nil))
                  (t (funcall cb nil)))))
        (setq pai-kimi--refreshing nil))))))

(defun pai-kimi--refresh-handler (_cred)
  "`pai-auth-oauth-refresh-handlers' entry: refresh in the background.
Return nil so the request in progress is never blocked; the periodic check
normally refreshes well before expiry."
  (pai-kimi-refresh)
  nil)

(defun pai-kimi--tick ()
  "Refresh the kimi token when due."
  (let ((cred (ignore-errors (pai-kimi--oauth-cred))))
    (when (and cred (pai-kimi--refresh-due-p cred))
      (pai-kimi-refresh))))

(defun pai-kimi--ensure-timer ()
  "Start the periodic (60s) refresh check unless it is running."
  (unless (timerp pai-kimi--refresh-timer)
    (setq pai-kimi--refresh-timer (run-at-time 60 60 #'pai-kimi--tick))))

;;;; Device-flow login (async)

(defvar pai-kimi--login nil
  "Plist of the device login in progress: :device-code :interval :deadline
:timer :buffer.")

(defun pai-kimi--note (buffer text)
  "Show TEXT in pai chat BUFFER when live, and in the echo area."
  (message "%s" text)
  (when (and (buffer-live-p buffer) (fboundp 'pai--render-note))
    (with-current-buffer buffer
      (ignore-errors (pai--render-note text)))))

(defun pai-kimi--cancel-login ()
  "Stop a device login in progress."
  (when (timerp (plist-get pai-kimi--login :timer))
    (cancel-timer (plist-get pai-kimi--login :timer)))
  (setq pai-kimi--login nil))

(defun pai-kimi--schedule-poll (login)
  "Schedule the next token poll of LOGIN."
  (setq pai-kimi--login
        (plist-put login :timer
                   (run-at-time (plist-get login :interval) nil
                                #'pai-kimi--poll login))))

(defun pai-kimi--poll (login)
  "Poll the token endpoint once for device LOGIN."
  (when (eq login pai-kimi--login)
    (let ((buffer (plist-get login :buffer)))
      (if (> (float-time) (plist-get login :deadline))
          (progn (pai-kimi--cancel-login)
                 (pai-kimi--note buffer "Kimi login timed out; run /login kimi again."))
        (pai-kimi--post-form
         "/api/oauth/token"
         `(("client_id" . ,pai-kimi--client-id)
           ("device_code" . ,(plist-get login :device-code))
           ("grant_type" . "urn:ietf:params:oauth:grant-type:device_code"))
         (lambda (status data)
           (when (eq login pai-kimi--login)
             (let ((err (plist-get data :error)))
               (cond
                ((and (eql status 200) (plist-get data :access_token))
                 (pai-kimi--cancel-login)
                 (setq pai-kimi--rejected-refresh nil)
                 (pai-kimi--store-token data)
                 (pai-kimi--ensure-timer)
                 (pai-kimi-refresh-models t)
                 (pai-kimi--note buffer "Logged in to Kimi.  Pick a model with /model kimi/…"))
                ((member err '("authorization_pending" nil))
                 ;; nil: network hiccup or 5xx -- keep polling until the deadline
                 (pai-kimi--schedule-poll login))
                ((equal err "slow_down")
                 (pai-kimi--schedule-poll
                  (plist-put login :interval (+ 5 (plist-get login :interval)))))
                (t
                 (pai-kimi--cancel-login)
                 (pai-kimi--note buffer
                                 (format "Kimi login failed: %s"
                                         (or (plist-get data :error_description)
                                             err status)))))))))))))

(defun pai-kimi-login ()
  "Start the Kimi Code device login; return a status message.
The flow finishes in the background and reports in the chat buffer."
  (interactive)
  (pai-kimi--cancel-login)
  (let ((buffer (current-buffer)))
    (if (pai-kimi--post-form
         "/api/oauth/device_authorization"
         `(("client_id" . ,pai-kimi--client-id))
         (lambda (status data)
           (let ((url (plist-get data :verification_uri_complete))
                 (code (plist-get data :user_code)))
             (if (and (eql status 200) (plist-get data :device_code) (stringp url))
                 (let ((login (list :device-code (plist-get data :device_code)
                                    :interval (max 1 (or (plist-get data :interval) 5))
                                    :deadline (+ (float-time)
                                                 (or (plist-get data :expires_in) 1800))
                                    :buffer buffer)))
                   (pai-kimi--note
                    buffer
                    (format "Kimi login: open %s\nand confirm code %s (signed in to Kimi).  Waiting…"
                            url code))
                   (ignore-errors (browse-url url))
                   (pai-kimi--schedule-poll login))
               (pai-kimi--note buffer
                               (format "Kimi login failed: %s"
                                       (or (plist-get data :error_description)
                                           (plist-get data :error) status)))))))
        (format "Kimi login started via %s…" pai-kimi-oauth-host)
      "Kimi login failed: could not reach the Kimi auth server")))

;;;; Requests

(defun pai-kimi--thinking-required-p (model)
  "Return non-nil when MODEL refuses thinking disabled."
  (or (pai-truthy (plist-get model :thinking-required))
      (and (stringp (plist-get model :id))
           (string-match-p pai-kimi-thinking-required-regexp (plist-get model :id)))))

(defun pai-kimi--thinking-text (message)
  "Return the concatenated thinking text of assistant MESSAGE, or nil."
  (let ((parts (delq nil (mapcar (lambda (b)
                                   (and (eq (pai-block-type b) 'thinking)
                                        (plist-get b :thinking)))
                                 (pai-message-content message)))))
    (when parts (apply #'concat parts))))

(defun pai-kimi--messages (messages)
  "Translate unified MESSAGES like `pai-openai--messages', adding the
`reasoning_content' of Kimi's own earlier turns, as Kimi CLI does.  The
reasoning of other providers' turns (a conversation continued from Claude)
is not sent: Kimi does not need it, and it can fill a large part of the
context window."
  (cl-mapcan (lambda (m)
               (let ((out (pai-openai--messages (list m)))
                     (thinking (and (eq (pai-message-role m) 'assistant)
                                    (equal (plist-get m :provider) "kimi")
                                    (pai-kimi--thinking-text m))))
                 (if (and out thinking (not (string-empty-p thinking)))
                     (list (append (car out) (list :reasoning_content thinking)))
                   out)))
             messages))

(defun pai-kimi--plist-remove (plist key)
  "Return PLIST without KEY."
  (cl-loop for (k v) on plist by #'cddr
           unless (eq k key) append (list k v)))

(defun pai-kimi-build-request (model context options)
  "Build a Kimi Code chat request for MODEL, CONTEXT, OPTIONS.
The OpenAI request, with Kimi CLI's headers, its thinking switch instead of
`reasoning_effort', and reasoning sent back on earlier assistant turns."
  (let* ((req (pai-openai-build-request model context options))
         (body (pai-kimi--plist-remove (plist-get req :body) :reasoning_effort))
         (thinking (and (pai-model-reasoning-p model)
                        (or (plist-get options :reasoning)
                            (pai-kimi--thinking-required-p model)))))
    (setq body (plist-put body :messages
                          (pai-kimi--messages (plist-get context :messages))))
    (when (pai-model-reasoning-p model)
      (setq body (plist-put body :thinking
                            (list :type (if thinking "enabled" "disabled")))))
    ;; keep the token fresh for the next turn without delaying this one
    (ignore-errors (pai-kimi--tick))
    (list :url (plist-get req :url)
          :headers (append (plist-get req :headers)
                           (list (cons "User-Agent" (pai-kimi--user-agent)))
                           (pai-kimi--common-headers))
          :body body)))

;;;; Models (asynchronous discovery, cached)

;; pai's generic discovery runs curl synchronously and reads only a model's
;; id and window, giving the 8192 default output cap and no thinking flags.
;; Kimi's /models declares more (limit.context, reasoning, image input), and
;; its windows depend on the plan (K3 is 256K on lower tiers, 1M above).  The
;; list is therefore fetched in the background -- after login, at startup
;; and whenever `/model' asks -- cached in ~/.pai/kimi/models.eld and served
;; from that cache by the provider's :list-models callback, so every chat
;; sees the account's real models without waiting on the network.

(defun pai-kimi--models-file ()
  "Return the file caching the account's model list."
  (expand-file-name "models.eld" (pai-state-directory "kimi")))

(defun pai-kimi--model-spec (item)
  "Return a pai model spec for a Kimi /models ITEM, or nil."
  (let ((id (plist-get item :id)))
    (when (and (stringp id) (not (string-empty-p id)))
      (let* ((limit (plist-get item :limit))
             (window (seq-find (lambda (v) (and (integerp v) (> v 0)))
                               (list (and (listp limit) (plist-get limit :context))
                                     (plist-get item :context_length))))
             (reasoning (eq (plist-get item :supports_reasoning) t))
             (name (plist-get item :display_name)))
        (append (list :id id)
                (when (stringp name) (list :name (format "%s (%s)" name id)))
                (when window (list :context-window window))
                (list :max-tokens pai-kimi-max-output-tokens
                      :reasoning (if reasoning t :false)
                      :input (if (eq (plist-get item :supports_image_in) t)
                                 '("text" "image")
                               '("text")))
                (when (string-match-p pai-kimi-thinking-required-regexp id)
                  (list :thinking-required t)))))))

(defun pai-kimi--parse-models (resp)
  "Return the model specs of a Kimi /models reply RESP; signal if malformed."
  (let ((data (plist-get resp :data)))
    (unless (and data (listp data)) (error "Malformed Kimi model list"))
    (or (delq nil (mapcar #'pai-kimi--model-spec data))
        (error "Kimi listed no models"))))

(defvar pai-kimi--models nil "Cached model specs, or nil when not loaded.")

(defun pai-kimi--cached-models ()
  "Return the cached model specs, reading the cache file once."
  (or pai-kimi--models
      (setq pai-kimi--models
            (let ((file (pai-kimi--models-file)))
              (and (file-readable-p file)
                   (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents file)
                       (let ((specs (read (current-buffer))))
                         (and (listp specs) (seq-every-p #'listp specs) specs)))))))))

(defun pai-kimi--list-models (_provider)
  "`:list-models' callback: the cached account models (no network).
Also starts a background refresh, picked up by the next `/model'."
  (pai-kimi-refresh-models)
  (or (pai-kimi--cached-models) pai-kimi--fallback-models))

(defvar pai-kimi--refresh-models-inhibit nil
  "Non-nil while applying fetched models (no new fetch is started).")
(defvar pai-kimi--models-fetching nil "Non-nil while /models is being fetched.")
(defvar pai-kimi--models-fetched-at 0 "`float-time' of the last successful fetch.")

(defun pai-kimi--apply-models ()
  "Register the cached models in the global registry and every pai chat."
  (let ((apply (lambda ()
                 (when-let* ((provider (pai-provider "kimi")))
                   (ignore-errors
                     (let ((pai-kimi--refresh-models-inhibit t))
                       (pai-providers-discover "kimi" provider)))))))
    ;; a temp buffer has no instance-local registries: this sets the defaults
    (with-temp-buffer (funcall apply))
    (dolist (buffer (buffer-list))
      (when (local-variable-p 'pai--models buffer)
        (with-current-buffer buffer (funcall apply))))))

(defun pai-kimi-refresh-models (&optional force)
  "Fetch the account's models from Kimi in the background.
At most once a minute unless FORCE.  On success the cache is replaced and
the models are registered everywhere."
  (interactive (list t))
  (let ((key (ignore-errors (pai-api-key "kimi"))))
    (when (and key (not pai-kimi--refresh-models-inhibit)
               (not pai-kimi--models-fetching)
               (or force (> (- (float-time) pai-kimi--models-fetched-at) 60)))
      (setq pai-kimi--models-fetching t)
      (condition-case nil
          (pai-http-request
           :url (concat (string-remove-suffix
                         "/" (or (ignore-errors (pai-provider-base-url "kimi"))
                                 pai-kimi-base-url))
                        "/models")
           :headers (append (list (cons "Authorization" (concat "Bearer " key))
                                  (cons "User-Agent" (pai-kimi--user-agent)))
                            (pai-kimi--common-headers))
           :timeout 20
           :on-done
           (lambda (status _headers text)
             (setq pai-kimi--models-fetching nil)
             (let ((specs (and (eql status 200) text
                               (ignore-errors
                                 (pai-kimi--parse-models (pai-json-decode text))))))
               (when specs
                 (setq pai-kimi--models specs
                       pai-kimi--models-fetched-at (float-time))
                 (ignore-errors
                   (with-temp-file (pai-kimi--models-file)
                     (let ((print-length nil) (print-level nil))
                       (prin1 specs (current-buffer)))))
                 (pai-kimi--apply-models)))))
        (error (setq pai-kimi--models-fetching nil))))))

;;;; Usage (/usage and the header line)

(defun pai-kimi--number (v)
  "Return V (a number or numeric string) as a number, or nil."
  (cond ((numberp v) v)
        ((and (stringp v) (string-match-p "\\`[0-9.]+\\'" v)) (string-to-number v))))

(defun pai-kimi--window-label (key)
  "Return a short label for a Kimi `usages' KEY such as :limit_5h."
  (let ((name (string-remove-prefix "limit_" (substring (symbol-name key) 1))))
    (pcase name
      ("5h" "5-hour")
      ("month_total" "month")
      ("month_code" "month (code)")
      ("week" "7-day")
      (_ (replace-regexp-in-string "_" " " name)))))

(defun pai-kimi--limit-label (item)
  "Return a label for a Kimi `limits' ITEM from its window, like Kimi CLI."
  (let* ((window (plist-get item :window))
         (duration (pai-kimi--number (plist-get window :duration)))
         (unit (or (plist-get window :timeUnit) "")))
    (cond ((null duration) "limit")
          ((string-match-p "MINUTE" unit)
           (if (and (>= duration 60) (zerop (% (truncate duration) 60)))
               (format "%dh requests" (/ (truncate duration) 60))
             (format "%dm requests" duration)))
          ((string-match-p "HOUR" unit) (format "%dh requests" duration))
          ((string-match-p "DAY" unit) (format "%dd requests" duration))
          (t (format "%ds requests" duration)))))

(defun pai-kimi--usage-parse (resp)
  "Return the pai usage result for Kimi's /usages reply RESP."
  (let ((usages (plist-get resp :usages))
        (limits (plist-get resp :limits))
        rows pct-5h pct-month)
    (cl-loop for (key val) on (and (listp usages) usages) by #'cddr
             for ratio = (pai-kimi--number (plist-get val :used_ratio))
             when ratio do
             (let ((pct (min 100 (max 0 (round (* 100 ratio))))))
               (pcase key
                 (:limit_5h (setq pct-5h pct))
                 (:limit_month_total (setq pct-month pct)))
               (push (format "  %-13s %3d%% used%s" (pai-kimi--window-label key) pct
                             (let ((r (pai-usage--reset-string (plist-get val :reset_time))))
                               (if r (format " (resets %s)" r) "")))
                     rows)))
    (dolist (item (and (listp limits) limits))
      (let* ((detail (or (plist-get item :detail) item))
             (limit (pai-kimi--number (plist-get detail :limit)))
             (remaining (pai-kimi--number (plist-get detail :remaining)))
             (used (or (pai-kimi--number (plist-get detail :used))
                       (and limit remaining (- limit remaining)))))
        (when limit
          (push (format "  %-13s %d/%d%s" (pai-kimi--limit-label item) (or used 0) limit
                        (let ((r (pai-usage--reset-string
                                  (or (plist-get detail :resetTime)
                                      (plist-get detail :reset_time)))))
                          (if r (format " (resets %s)" r) "")))
                rows))))
    (unless rows (error "no Kimi usage data"))
    (list :summary (string-join
                    (delq nil (list "Kimi"
                                    (and pct-5h (format "5h %d%%" pct-5h))
                                    (and pct-month (format "mo %d%%" pct-month))))
                    " ")
          :detail (concat "Kimi Code\n" (string-join (nreverse rows) "\n")))))

(defun pai-kimi--usage (key _cred)
  "Return the request for Kimi Code usage, authorized with token KEY."
  (unless (and (stringp key) (not (string-empty-p key)))
    (error "not logged in"))
  (list :request (list :url (concat (string-remove-suffix
                                     "/" (or (ignore-errors (pai-provider-base-url "kimi"))
                                             pai-kimi-base-url))
                                    "/usages")
                       :headers (append (list (cons "Authorization" (concat "Bearer " key))
                                              (cons "User-Agent" (pai-kimi--user-agent)))
                                        (pai-kimi--common-headers)))
        :parse #'pai-kimi--usage-parse))

;;;; Registration

(defun pai-kimi-extension (_pi)
  "Register the Kimi Code provider, its login and token refresh."
  (let ((provider (pai-register-provider-config
                   (list :id "kimi" :api 'openai-completions
                         :base-url pai-kimi-base-url
                         :env-key "KIMI_API_KEY"
                         :reasoning t
                         :list-models #'pai-kimi--list-models
                         :models (or (pai-kimi--cached-models)
                                     pai-kimi--fallback-models)))))
    (pai-register-provider (plist-put provider :build-request #'pai-kimi-build-request)))
  (setf (alist-get "kimi" pai-auth-oauth-handlers nil nil #'equal) #'pai-kimi-login)
  (setf (alist-get "kimi" pai-auth-oauth-refresh-handlers nil nil #'equal)
        #'pai-kimi--refresh-handler)
  (pai-register-usage-provider "kimi" #'pai-kimi--usage)
  (setf (alist-get "kimi" pai-usage-placeholders nil nil #'equal) "Kimi 5h --% mo --%")
  (when (ignore-errors (pai-kimi--oauth-cred))
    (pai-kimi--ensure-timer))
  ;; learn the account's models without delaying startup
  (run-at-time 2 nil #'pai-kimi-refresh-models))

(pai-register-extension #'pai-kimi-extension "kimi")

(provide 'pai-kimi)
;;; pai-kimi.el ends here
