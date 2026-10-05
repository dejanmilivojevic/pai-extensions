;;; pai-web-auth.el --- Settings, password and login sessions for pai-web -*- lexical-binding: t; -*-

;;; Commentary:

;; Settings live under the `:web' key of the global ~/.pai/settings.json:
;;
;;   :auto-start     start the server with the first pai session (default off)
;;   :port           TCP port (default 8765)
;;   :password       (:salt HEX :hash HEX :iterations N), never the password
;;   :upload-mode    "uploads" (~/.pai/web/uploads) or "project" (a subfolder)
;;   :upload-subdir  the project subfolder for "project" (default "uploads")
;;   :allowed-hosts  extra host names the page may be opened by (VPN names)
;;
;; They are read from the file (cached by modification time), not from a pai
;; buffer's copy, because the server runs outside every pai buffer.
;;
;; Without a password the server only listens on 127.0.0.1 and needs no
;; login; with one it listens on every interface and a browser logs in with
;; the password once, getting a session cookie.  Session tokens are stored
;; hashed in ~/.pai/web/sessions.eld; changing the password or "log out
;; everywhere" drops them all.  Failed logins are answered after a delay
;; and an address that keeps failing is locked out for a while.
;;
;; Every request must name an allowed Host (an IP address, localhost or an
;; :allowed-hosts entry): that defeats DNS rebinding, where a web page you
;; visit points its own host name at your machine.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-config)
(require 'pai-settings)
(require 'pai-web-util)
(require 'pai-web-http)

(defconst pai-web-default-port 8765 "Default TCP port.")
(defconst pai-web-cookie "pai_web" "Name of the session cookie.")
(defconst pai-web-password-iterations 20000 "SHA-256 rounds of a password hash.")
(defconst pai-web-session-ttl (* 180 24 3600) "Seconds a login stays valid.")

;;;; Settings

(defvar pai-web--settings-cache nil "(MTIME . WEB-PLIST) read from the settings file.")

(defun pai-web-settings ()
  "Return the `:web' settings plist, read from the global settings file."
  (let* ((file (pai-settings-global-file))
         (mtime (file-attribute-modification-time (file-attributes file))))
    (if (and pai-web--settings-cache (equal (car pai-web--settings-cache) mtime))
        (cdr pai-web--settings-cache)
      (let* ((all (ignore-errors (pai-settings--read file)))
             (web (plist-get all :web))
             (web (and (consp web) (keywordp (car web)) web)))
        (setq pai-web--settings-cache (cons mtime web))
        web))))

(defun pai-web-setting (key &optional default)
  "Return web setting KEY, or DEFAULT when unset."
  (let ((web (pai-web-settings)))
    (if (plist-member web key) (plist-get web key) default)))

(defun pai-web-set-setting (key value)
  "Persist web setting KEY as VALUE in the global settings."
  (let ((web (copy-sequence (pai-web-settings))))
    (pai-settings-set :web (plist-put web key value) 'global)
    (setq pai-web--settings-cache nil)
    value))

(defun pai-web-port ()
  "Return the configured port."
  (let ((p (pai-web-setting :port pai-web-default-port)))
    (if (and (integerp p) (< 0 p 65536)) p pai-web-default-port)))

(defun pai-web-auto-start-p ()
  "Return non-nil when the server starts with the first pai session."
  (pai-truthy (pai-web-setting :auto-start nil)))

(defun pai-web-upload-mode ()
  "Return where uploaded files go: \"uploads\" or \"project\"."
  (if (equal (pai-web-setting :upload-mode "uploads") "project") "project" "uploads"))

(defun pai-web-upload-subdir ()
  "Return the project subfolder that receives uploads in \"project\" mode."
  (let ((dir (pai-web-setting :upload-subdir "uploads")))
    (if (and (stringp dir) (not (string-empty-p (string-trim dir)))) (string-trim dir) "uploads")))

(defun pai-web-allowed-hosts ()
  "Return the extra allowed host names (list of lower-case strings)."
  (let ((v (pai-web-setting :allowed-hosts "")))
    (mapcar #'downcase (split-string (if (stringp v) v "") "[ ,]+" t))))

(defun pai-web-directory ()
  "Return pai-web's state directory (~/.pai/web/)."
  (expand-file-name "web/" pai-directory))

;;;; Password

(defun pai-web--hash (password salt iterations)
  "Return the hex hash of PASSWORD with SALT after ITERATIONS rounds."
  (let ((h (secure-hash 'sha256 (concat salt (encode-coding-string password 'utf-8)))))
    (dotimes (_ (1- iterations))
      (setq h (secure-hash 'sha256 (concat salt h))))
    h))

(defun pai-web-password-set-p ()
  "Return non-nil when a password is configured."
  (let ((p (pai-web-setting :password)))
    (and (consp p) (stringp (plist-get p :hash)) t)))

(defun pai-web-set-password (password)
  "Store a salted hash of PASSWORD (nil or \"\" removes it); log everyone out."
  (if (or (null password) (string-empty-p password))
      (pai-web-set-setting :password nil)
    (let ((salt (pai-web-random-hex 16)))
      (pai-web-set-setting :password
                           (list :salt salt :iterations pai-web-password-iterations
                                 :hash (pai-web--hash password salt pai-web-password-iterations)))))
  (pai-web-sessions-clear))

(defun pai-web-check-password (password)
  "Return non-nil when PASSWORD matches the stored hash."
  (let ((p (pai-web-setting :password)))
    (and (consp p) (stringp password)
         (let ((salt (plist-get p :salt))
               (n (plist-get p :iterations)))
           (and (stringp salt) (integerp n) (> n 0)
                (string= (pai-web--hash password salt n) (plist-get p :hash)))))))

;;;; Login sessions

(defvar pai-web--sessions 'unloaded
  "List of (TOKEN-HASH . CREATED) login sessions, or `unloaded'.")

(defun pai-web--sessions-file ()
  "Return the file login sessions are kept in."
  (expand-file-name "sessions.eld" (pai-web-directory)))

(defun pai-web--sessions ()
  "Return the login sessions, loading them on first use."
  (when (eq pai-web--sessions 'unloaded)
    (setq pai-web--sessions
          (let ((file (pai-web--sessions-file)))
            (and (file-readable-p file)
                 (ignore-errors
                   (with-temp-buffer
                     (insert-file-contents file)
                     (let ((v (read (current-buffer)))) (and (listp v) v))))))))
  pai-web--sessions)

(defun pai-web--sessions-save ()
  "Write the login sessions to disk (owner-only)."
  (let ((file (pai-web--sessions-file)))
    (make-directory (file-name-directory file) t)
    (let ((print-length nil) (print-level nil)
          (content (prin1-to-string (pai-web--sessions))))
      (with-file-modes #o600
        (with-temp-file file (insert content))))))

(defun pai-web-session-create ()
  "Create a login session; return its token (stored only hashed)."
  (let* ((token (pai-web-random-hex 32))
         (now (float-time)))
    (setq pai-web--sessions
          (cons (cons (secure-hash 'sha256 token) now)
                (seq-filter (lambda (s) (> (+ (cdr s) pai-web-session-ttl) now))
                            (pai-web--sessions))))
    (pai-web--sessions-save)
    token))

(defun pai-web-session-valid-p (token)
  "Return the session id (token hash) when TOKEN is a live login, else nil."
  (when (and (stringp token) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" token))
    (let* ((hash (secure-hash 'sha256 token))
           (s (assoc hash (pai-web--sessions))))
      (and s (> (+ (cdr s) pai-web-session-ttl) (float-time)) hash))))

(defun pai-web-session-drop (session)
  "Forget the login SESSION (a token hash)."
  (setq pai-web--sessions (assoc-delete-all session (pai-web--sessions)))
  (pai-web--sessions-save))

(defun pai-web-sessions-clear ()
  "Forget every login session."
  (setq pai-web--sessions nil)
  (pai-web--sessions-save))

;;;; Requests

(defun pai-web-host-allowed-p (req)
  "Return non-nil when REQ's Host header names an allowed host."
  (let* ((host (downcase (or (pai-web-http-header req "host") "")))
         (name (cond ((string-match "\\`\\[\\([^]]+\\)\\]\\(?::[0-9]+\\)?\\'" host)
                      (match-string 1 host))
                     ((string-match "\\`\\([^:]+\\)\\(?::[0-9]+\\)?\\'" host)
                      (match-string 1 host))
                     (t host))))
    (or (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" name)
        (string-match-p "\\`[0-9a-f:]+\\'" name)          ; IPv6 literal
        (member name '("localhost"))
        (member name (pai-web-allowed-hosts)))))

(defun pai-web-request-session (req)
  "Return REQ's login session id, `open' when no password is set, or nil."
  (if (not (pai-web-password-set-p))
      'open
    (pai-web-session-valid-p (pai-web-http-cookie req pai-web-cookie))))

(defun pai-web-session-cookie (token)
  "Return the Set-Cookie value carrying login TOKEN."
  (format "%s=%s; Path=/; HttpOnly; SameSite=Strict; Max-Age=%d"
          pai-web-cookie token pai-web-session-ttl))

(defun pai-web-expired-cookie ()
  "Return a Set-Cookie value that removes the session cookie."
  (format "%s=; Path=/; HttpOnly; SameSite=Strict; Max-Age=0" pai-web-cookie))

;;;; Failed logins

(defvar pai-web--failures (make-hash-table :test 'equal)
  "Map of peer address to (COUNT . FIRST-FAILURE-TIME).")

(defconst pai-web-lockout-failures 5 "Failed logins that lock an address out.")
(defconst pai-web-lockout-seconds 300 "Seconds an address stays locked out.")

(defun pai-web-locked-out-p (peer)
  "Return non-nil when PEER failed too many logins recently."
  (let ((f (gethash peer pai-web--failures)))
    (and f (>= (car f) pai-web-lockout-failures)
         (< (- (float-time) (cdr f)) pai-web-lockout-seconds))))

(defun pai-web-note-failure (peer)
  "Count a failed login from PEER."
  (let ((f (gethash peer pai-web--failures)))
    (if (or (null f) (> (- (float-time) (cdr f)) pai-web-lockout-seconds))
        (puthash peer (cons 1 (float-time)) pai-web--failures)
      (setcar f (1+ (car f))))))

(defun pai-web-clear-failures (peer)
  "Forget PEER's failed logins."
  (remhash peer pai-web--failures))

(provide 'pai-web-auth)
;;; pai-web-auth.el ends here
