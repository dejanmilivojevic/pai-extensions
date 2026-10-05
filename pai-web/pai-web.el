;;; pai-web.el --- Monitor and control every pai instance from a browser -*- lexical-binding: t; -*-

;;; Commentary:

;; A web server inside Emacs serving a mobile-first page that shows every
;; pai instance (subagents nested under their parent) and does what a pai
;; buffer does: prompts and steering, interrupt, slash commands and !shell
;; with the same completion, model and thinking level, new/close/resume,
;; ask_user questions, minibuffer prompts, image and file attachments, and
;; pai's other buffers (/menu, /todo edit, memory review...) as live,
;; clickable text with a key bar.
;;
;;   /web start|stop|restart|status|open|password [clear]|logout-all
;;
;; Settings: /menu -> Web.  At most one server runs per Emacs, started with
;; /web start or by the first pai session when "Start automatically" is on.
;; Without a password it listens on 127.0.0.1 only; with one, on every
;; interface, and browsers log in with it.
;;
;; Nothing here blocks Emacs: the network is asynchronous, responses are
;; small (pai-web-http.el explains why that matters), actions run from
;; timers, and change detection is a cheap tick that only does work while
;; a page is connected.
;;
;; Modules: pai-web-http (server), pai-web-bus (clients, long polling),
;; pai-web-auth (settings, password, logins), pai-web-instances (instance
;; list, transcript logs), pai-web-prompt (minibuffer prompts, ask_user),
;; pai-web-buffers (remote buffers), pai-web-actions (what pages can do);
;; the page itself is in static/.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-ext)
(require 'pai-commands)
(require 'pai-settings)
(require 'pai-web-util)
(require 'pai-web-http)
(require 'pai-web-bus)
(require 'pai-web-auth)
(require 'pai-web-instances)
(require 'pai-web-prompt)
(require 'pai-web-buffers)
(require 'pai-web-actions)

(declare-function pai-settings-ui-register-section "pai-settings-ui")
(declare-function pai-settings-ui-register-subsection "pai-settings-ui")
(declare-function pai-settings-ui-register-item "pai-settings-ui")
(declare-function pai-interrupt "pai-ui" ())
(declare-function pai-set-model "pai-ui" (id))
(declare-function pai-set-thinking "pai-ui" (level))
(declare-function pai-new-session "pai-ui" (&optional cwd))
(defvar pai-thinking-levels)

(defconst pai-web-version "1" "Protocol version the page checks.")

(defconst pai-web-static-directory
  (expand-file-name "static" (file-name-directory (or load-file-name buffer-file-name "")))
  "Directory of the page's files.")

(defconst pai-web-tick-interval 0.5 "Seconds between change checks.")

(defvar pai-web-content-security-policy
  "default-src 'self'; img-src 'self' data: blob:; style-src 'self' 'unsafe-inline'; connect-src 'self'; frame-ancestors 'none'"
  "Content-Security-Policy of the page: only its own scripts run.
Nil sends none (e.g. for a test browser that injects scripts).")

(defvar pai-web--host nil "Address the server listens on.")
(defvar pai-web--tick-timer nil "Timer of `pai-web--tick'.")
(defvar pai-web--auto-start-failed nil "Non-nil after an automatic start failed.")

(defvar pai-web--last nil
  "Plist of the last states sent to the pages, to send only changes.")

;;;; Lifecycle

(defun pai-web-running-p ()
  "Return non-nil when the server runs."
  (pai-web-http-running-p))

(defun pai-web-start ()
  "Start the pai-web server (at most one per Emacs).  Return a status message."
  (interactive)
  (if (pai-web-running-p)
      (pai-web-status-message)
    (let* ((host (if (pai-web-password-set-p) "0.0.0.0" "127.0.0.1"))
           (port (pai-web-port)))
      (condition-case err
          (pai-web-http-start host port)
        (error
         (user-error "pai-web: cannot listen on %s:%d: %s" host port
                     (error-message-string err))))
      (setq pai-web--host host
            pai-web--auto-start-failed nil
            pai-web-http-handler #'pai-web--handle)
      (pai-web-instances-install)
      (pai-web-prompt-install)
      (pai-web-buffers-install)
      (pai-web-actions-install)
      (pai-web-face-cache-clear)
      (setq pai-web--tick-timer (run-at-time pai-web-tick-interval pai-web-tick-interval
                                             #'pai-web--tick))
      (let ((msg (pai-web-status-message)))
        (when (called-interactively-p 'any) (message "%s" msg))
        msg))))

(defun pai-web-stop ()
  "Stop the pai-web server."
  (interactive)
  (when (timerp pai-web--tick-timer) (cancel-timer pai-web--tick-timer))
  (setq pai-web--tick-timer nil)
  (pai-web-bus-reset)
  (pai-web-http-stop)
  (pai-web-instances-uninstall)
  (pai-web-prompt-uninstall)
  (pai-web-buffers-uninstall)
  (pai-web-actions-uninstall)
  (setq pai-web--host nil pai-web--last nil)
  (when (called-interactively-p 'any) (message "pai-web stopped"))
  "pai-web stopped")

(defun pai-web-restart ()
  "Restart the server (after a port or password change)."
  (interactive)
  (pai-web-stop)
  (pai-web-start))

(defun pai-web-urls ()
  "Return the URLs the page can be opened at."
  (let ((port (or (pai-web-http-port) (pai-web-port))))
    (cons (format "http://localhost:%d/" port)
          (when (equal pai-web--host "0.0.0.0")
            (delq nil
                  (mapcar (lambda (iface)
                            (let ((addr (cdr iface)))
                              (when (and (vectorp addr) (= (length addr) 5)
                                         (not (string-prefix-p "lo" (car iface)))
                                         (not (= (aref addr 0) 127)))
                                (format "http://%d.%d.%d.%d:%d/" (aref addr 0) (aref addr 1)
                                        (aref addr 2) (aref addr 3) port))))
                          (ignore-errors (network-interface-list nil 'ipv4))))))))

(defun pai-web-status-message ()
  "Return a description of the server's state."
  (if (not (pai-web-running-p))
      (format "pai-web is stopped (port %d, %s)." (pai-web-port)
              (if (pai-web-password-set-p) "password set" "no password: localhost only"))
    (format "pai-web is running on %s:%d -- %s\nOpen: %s\nPages connected: %d"
            pai-web--host (pai-web-http-port)
            (if (pai-web-password-set-p) "password login"
              "no password, so only this machine can connect (set one with /web password)")
            (string-join (pai-web-urls) "  ")
            (length (pai-web-bus-clients)))))

(defun pai-web--auto-start ()
  "Start the server for the first pai session when configured to."
  (when (and (pai-web-auto-start-p) (not (pai-web-running-p))
             (not pai-web--auto-start-failed))
    (condition-case err
        (message "%s" (car (split-string (pai-web-start) "\n")))
      (error (setq pai-web--auto-start-failed t)
             (message "%s" (error-message-string err))))))

;;;; Change detection

(defun pai-web--changed-p (key value)
  "Return non-nil when VALUE differs from the last one stored under KEY."
  (unless (equal (plist-get pai-web--last key) value)
    (setq pai-web--last (plist-put pai-web--last key value))
    t))

(defun pai-web--tick ()
  "Send what changed to the pages; time out polls; forget silent clients."
  (condition-case err
      (progn
        (pai-web-bus-sweep)
        (when (pai-web-bus-active-p)
          (let ((instances (pai-web-instance-list)))
            (when (pai-web--changed-p :instances (pai-web-json instances))
              (pai-web-bus-broadcast (list :t "instances" :list instances))))
          (let ((asks (pai-web-asks)))
            (when (pai-web--changed-p :asks (pai-web-json asks))
              (pai-web-bus-broadcast (list :t "asks" :list asks))))
          (pai-web--prompts-prune)
          (let ((buffers (pai-web-buffer-list)))
            (when (pai-web--changed-p :buffers (pai-web-json buffers))
              (pai-web-bus-broadcast (list :t "buffers" :list buffers))))
          (let ((viewed nil) (shown nil))
            (dolist (c (pai-web-bus-clients))
              (when (pai-web-client-view c) (cl-pushnew (pai-web-client-view c) viewed :test #'equal))
              (dolist (b (pai-web-client-buffers c)) (cl-pushnew b shown :test #'equal)))
            (dolist (i viewed)
              (when-let ((buf (pai-web-instance i)))
                (let ((chrome (pai-web-instance-chrome buf)))
                  (when (pai-web--changed-p (intern (concat ":chrome-" i)) (pai-web-json chrome))
                    (pai-web-bus-broadcast (list :t "chrome" :i i :chrome chrome))))))
            (dolist (b shown)
              (when-let ((buf (pai-web-buffer b)))
                (when (pai-web--changed-p (intern (concat ":sig-" b))
                                          (pai-web-buffer-signature buf))
                  (pai-web-bus-broadcast (list :t "buffer-changed" :b b))))))))
    (error (message "pai-web tick: %s" (error-message-string err)))))

;;;; Static files

(defvar pai-web--static-cache (make-hash-table :test 'equal)
  "Map of static file name to (MTIME . BYTES).")

(defun pai-web--static (req name)
  "Answer REQ with the static file NAME."
  (let ((file (expand-file-name name pai-web-static-directory)))
    (if (not (and (file-in-directory-p file pai-web-static-directory)
                  (file-regular-p file)))
        (pai-web-http-respond req 404 :type "text/plain" :body "Not found")
      (let* ((mtime (file-attribute-modification-time (file-attributes file)))
             (hit (gethash name pai-web--static-cache))
             (bytes (if (and hit (equal (car hit) mtime))
                        (cdr hit)
                      (let ((b (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally file)
                                 (buffer-string))))
                        (puthash name (cons mtime b) pai-web--static-cache)
                        b))))
        (pai-web-http-respond
         req 200 :body bytes
         :type (pcase (file-name-extension name)
                 ("html" "text/html; charset=utf-8")
                 ("js" "text/javascript; charset=utf-8")
                 ("css" "text/css; charset=utf-8")
                 ("svg" "image/svg+xml")
                 ("png" "image/png")
                 ("webmanifest" "application/manifest+json")
                 (_ "application/octet-stream"))
         :headers (and pai-web-content-security-policy
                       `(("Content-Security-Policy" . ,pai-web-content-security-policy))))))))

;;;; Requests

(defun pai-web--json-body (req)
  "Return REQ's JSON body as a plist (nil when empty or invalid)."
  (let ((body (plist-get req :body)))
    (and body (> (length body) 0)
         (ignore-errors (pai-web-json-read body)))))

(defun pai-web--error (req status text)
  "Answer REQ with STATUS and an error TEXT."
  (pai-web-bus-respond-json req (list :error text) status))

(defun pai-web--handle (req)
  "Answer the HTTP request REQ."
  (let ((path (plist-get req :path))
        (method (plist-get req :method)))
    (cond
     ((not (pai-web-host-allowed-p req))
      (pai-web-http-respond req 403 :type "text/plain"
                            :body "Host not allowed (add it under /menu -> Web)"))
     ((and (equal method "POST") (not (pai-web-http-header req "x-pai")))
      (pai-web-http-respond req 403 :type "text/plain" :body "Missing X-Pai header"))
     ((member path '("/" "/index.html")) (pai-web--static req "index.html"))
     ((string-match "\\`/static/\\([a-z0-9-]+\\.[a-z]+\\)\\'" path)
      (pai-web--static req (match-string 1 path)))
     ((equal path "/api/auth")
      (pai-web-bus-respond-json req (list :password (pai-web-bool (pai-web-password-set-p))
                                          :authed (pai-web-bool (pai-web-request-session req))
                                          :version pai-web-version)))
     ((equal path "/api/login") (pai-web--login req))
     (t
      (let ((session (pai-web-request-session req)))
        (if (not session)
            (pai-web--error req 401 "Please log in")
          (condition-case err
              (pai-web--api req session path)
            (error (pai-web--error req 400 (error-message-string err))))))))))

(defun pai-web--login (req)
  "Check the password posted in REQ and set the session cookie."
  (let ((peer (or (plist-get req :peer) "?"))
        (password (plist-get (pai-web--json-body req) :password)))
    (cond
     ((not (pai-web-password-set-p))
      (pai-web-bus-respond-json req (list :ok t)))
     ((pai-web-locked-out-p peer)
      (pai-web--error req 429 "Too many failed logins; try again in a few minutes"))
     ((pai-web-check-password password)
      (pai-web-clear-failures peer)
      (pai-web-http-respond req 200 :body "{\"ok\":true}"
                            :headers `(("Set-Cookie" . ,(pai-web-session-cookie
                                                         (pai-web-session-create))))))
     (t
      (pai-web-note-failure peer)
      ;; answer later, without blocking: slows down guessing
      (pai-web-http-hold req)
      (run-at-time 1.5 nil (lambda () (pai-web--error req 401 "Wrong password")))))))

(defun pai-web--q (req name)
  "Return query parameter NAME of REQ."
  (pai-web-http-query req name))

(defun pai-web--need-instance (id)
  "Return the chat buffer with ID or signal an error."
  (or (pai-web-instance id) (error "No such pai instance (it was closed?)")))

(defun pai-web--need-buffer (id)
  "Return the shown buffer with ID or signal an error."
  (let ((b (pai-web-buffer id)))
    (unless (and b (pai-web-buffer-related-p b))
      (error "That buffer cannot be shown here (closed?)"))
    b))

(defun pai-web--state ()
  "Return everything a page needs after connecting."
  (list :instances (pai-web-instance-list)
        :prompts (pai-web-prompts-json)
        :asks (pai-web-asks)
        :buffers (pai-web-buffer-list)
        :colors (pai-web-default-colors)
        :levels (vconcat pai-thinking-levels)
        :password (pai-web-bool (pai-web-password-set-p))
        :seq pai-web-bus--seq))

(defun pai-web--api (req session path)
  "Answer the API request REQ of login SESSION for PATH."
  (let ((body (and (equal (plist-get req :method) "POST") (pai-web--json-body req))))
    (pcase path
      ("/api/hello"
       (let ((client (pai-web-bus-new-client session)))
         ;; the page starts from this state; drop what was queued before it
         (pai-web-bus-respond-json
          req (append (list :client (pai-web-client-id client) :version pai-web-version)
                      (pai-web--state)))))
      ("/api/state" (pai-web-bus-respond-json req (pai-web--state)))
      ("/api/poll"
       (let ((client (pai-web-bus-client (pai-web--q req "c"))))
         (if (not client)
             (pai-web--error req 410 "Unknown client; reconnect")
           (pai-web-bus-poll req client (string-to-number (or (pai-web--q req "ack") "0"))))))
      ("/api/blob" (pai-web-bus-serve-blob req))
      ("/api/transcript"
       (let ((buf (pai-web--need-instance (pai-web--q req "i")))
             (before (and (pai-web--q req "before") (string-to-number (pai-web--q req "before")))))
         (pai-web-bus-respond-json req (pai-web-log-snapshot buf before))))
      ("/api/tool"
       (let ((buf (pai-web--need-instance (pai-web--q req "i"))))
         (pai-web-bus-respond-json
          req (pai-web-tool-detail buf (string-to-number (or (pai-web--q req "id") "0"))))))
      ("/api/chrome"
       (pai-web-bus-respond-json
        req (pai-web-instance-chrome (pai-web--need-instance (pai-web--q req "i")))))
      ("/api/models"
       (pai-web-bus-respond-json req (pai-web-models (pai-web--need-instance (pai-web--q req "i")))))
      ("/api/dirs" (pai-web-bus-respond-json req (list :dirs (pai-web-known-dirs))))
      ("/api/buffers" (pai-web-bus-respond-json req (list :list (pai-web-buffer-list))))
      ("/api/buffer"
       (let ((buf (pai-web--need-buffer (pai-web--q req "b"))))
         (pai-web-bus-respond-json req (pai-web-buffer-render buf))))
      ("/api/keykind"
       (let ((buf (pai-web--need-buffer (pai-web--q req "b"))))
         (pai-web-bus-respond-json
          req (list :kind (pai-web-buffer-key-kind buf (or (pai-web--q req "keys") ""))))))
      ("/api/field"
       (let ((buf (pai-web--need-buffer (pai-web--q req "b"))))
         (pai-web-bus-respond-json
          req (or (pai-web-buffer-field buf (string-to-number (or (pai-web--q req "p") "0")))
                  (list :error "No editable field there")))))
      ("/api/view"
       (let ((client (pai-web-bus-client (plist-get body :c))))
         (when client
           (setf (pai-web-client-view client) (plist-get body :i)
                 (pai-web-client-buffers client) (seq-filter #'stringp (plist-get body :buffers)))
           ;; make the next tick send the viewed instance's chrome again
           (when (plist-get body :i)
             (setq pai-web--last (plist-put pai-web--last
                                            (intern (concat ":chrome-" (plist-get body :i))) nil))))
         (pai-web-bus-respond-json req (list :ok t))))
      ("/api/complete"
       (let ((buf (pai-web--need-instance (plist-get body :i))))
         (pai-web-bus-respond-json
          req (or (pai-web-complete buf (or (plist-get body :text) "")
                                    (or (plist-get body :pos) 0))
                  (list :items [])))))
      ("/api/prompt-complete"
       (pai-web-bus-respond-json
        req (list :candidates (or (pai-web-prompt-complete (plist-get body :id)
                                                           (or (plist-get body :input) ""))
                                  []))))
      ("/api/upload"
       (let ((buf (pai-web--need-instance (pai-web--q req "i"))))
         (pai-web-bus-respond-json
          req (pai-web-upload buf (or (pai-web--q req "name") "upload") (plist-get req :body)))))
      ("/api/action"
       (pai-web--action body)
       (pai-web-bus-respond-json req (list :ok t)))
      ("/api/logout"
       (unless (eq session 'open) (pai-web-session-drop session))
       (pai-web-bus-drop-session-clients session)
       (pai-web-http-respond req 200 :body "{\"ok\":true}"
                             :headers `(("Set-Cookie" . ,(pai-web-expired-cookie)))))
      (_ (pai-web--error req 404 "Unknown API")))))

(defun pai-web--action (body)
  "Run the action described by BODY (a plist from a page)."
  (let* ((client (plist-get body :c))
         (inst (lambda () (pai-web--need-instance (plist-get body :i))))
         (buf (lambda () (pai-web--need-buffer (plist-get body :b))))
         (pos (lambda () (let ((p (plist-get body :p))) (if (numberp p) p 1)))))
    (pcase (plist-get body :a)
      ("send"
       (let ((b (funcall inst)))
         (pai-web-run (lambda () (pai-web-send b (plist-get body :text) (plist-get body :images)) nil)
                      b client)))
      ("interrupt" (pai-web-run (lambda () (pai-interrupt) nil) (funcall inst) client))
      ("model" (let ((id (plist-get body :id)))
                 (pai-web-run (lambda () (pai-set-model id)) (funcall inst) client)))
      ("thinking" (let ((level (plist-get body :level)))
                    (unless (member level pai-thinking-levels) (error "Unknown level"))
                    (pai-web-run (lambda () (pai-set-thinking level) nil) (funcall inst) client)))
      ("new"
       (let ((dir (expand-file-name (or (plist-get body :dir) "~/"))))
         (unless (file-directory-p dir) (error "No such directory: %s" dir))
         (pai-web-run (lambda ()
                        (let ((b (pai-new-session dir)))
                          (when (buffer-live-p b)
                            (pai-web-bus-broadcast
                             (list :t "focus" :i (pai-web-id b))
                             (lambda (c) (equal (pai-web-client-id c) client)))))
                        nil)
                      nil client)))
      ("close" (let ((b (funcall inst)))
                 (pai-web-run (lambda () (kill-buffer b) nil) nil client)))
      ("prompt"
       (let ((id (plist-get body :id))
             (value (if (pai-truthy (plist-get body :cancel)) nil
                      (or (plist-get body :value) ""))))
         (pai-web-run (lambda () (pai-web-prompt-answer id value)) nil client)))
      ("ask"
       (let ((id (plist-get body :id)))
         (pai-web-run (lambda ()
                        (pai-web-ask-answer id (seq-filter #'integerp (plist-get body :choices))
                                            (plist-get body :other) (plist-get body :text)
                                            (pai-truthy (plist-get body :cancel))))
                      nil client)))
      ("click" (let ((b (funcall buf)) (p (funcall pos)))
                 (pai-web-run (lambda () (pai-web-buffer-click b p) nil) nil client)))
      ("key" (let ((b (funcall buf)) (keys (or (plist-get body :keys) "")))
               (pai-web-run (lambda () (pai-web-buffer-key b keys)) nil client)))
      ("type" (let ((b (funcall buf)) (text (or (plist-get body :text) "")))
                (pai-web-run (lambda () (pai-web-buffer-type b text) nil) nil client)))
      ("field" (let ((b (funcall buf)) (p (funcall pos)) (value (or (plist-get body :value) "")))
                 (pai-web-run (lambda () (pai-web-buffer-set-field b p value)) nil client)))
      ("goto" (let ((b (funcall buf)) (p (funcall pos)))
                (pai-web-run (lambda () (pai-web-buffer-goto b p) nil) nil client)))
      ("kill-buffer" (let ((b (funcall buf)))
                       (pai-web-run (lambda () (kill-buffer b) nil) nil client)))
      (other (error "Unknown action: %s" other)))))

;;;; The /web command

(defconst pai-web-completion-tree
  '("start" "stop" "restart" "status" "open" ("password" "clear") "logout-all")
  "Arguments of /web.")

(defun pai-web-set-password-interactively (&optional clear)
  "Ask for a new password (or CLEAR it), then rebind a running server."
  (if clear
      (pai-web-set-password nil)
    (let ((pw (read-passwd "New pai-web password: " t)))
      (when (< (length pw) 8)
        (user-error "Use at least 8 characters"))
      (pai-web-set-password pw)
      (clear-string pw)))
  (pai-web-bus-drop-session-clients t)
  (if (pai-web-running-p)
      (progn (pai-web-restart)
             (concat (if clear "Password removed; " "Password set; ") (pai-web-status-message)))
    (if clear "Password removed: the server will only listen on 127.0.0.1."
      "Password set: the server will listen on every interface.")))

(defun pai-web-command (args _ctx)
  "Handler for /web with ARGS."
  (let* ((words (split-string (string-trim (or args "")) "[ \t]+" t))
         (sub (or (car words) "status")))
    (list :message
          (pcase sub
            ("start" (pai-web-start))
            ("stop" (pai-web-stop))
            ("restart" (pai-web-restart))
            ("status" (pai-web-status-message))
            ("open" (unless (pai-web-running-p) (pai-web-start))
             (browse-url (car (pai-web-urls)))
             (format "Opening %s" (car (pai-web-urls))))
            ("password" (pai-web-set-password-interactively (equal (cadr words) "clear")))
            ("logout-all" (pai-web-sessions-clear)
             (pai-web-bus-drop-session-clients t)
             "Every browser was logged out.")
            (_ (format "Unknown subcommand %s; use start, stop, restart, status, open, password or logout-all"
                       sub))))))

;;;; Extension

(defun pai-web-extension (api)
  "Register /web and the automatic start with the extension API."
  (pai-ext-register-command
   api "web"
   :description "Web UI to monitor and control pai: start|stop|restart|status|open|password|logout-all"
   :handler #'pai-web-command
   :arg-completions (pai-command-completion-tree pai-web-completion-tree))
  (pai-ext-on api 'session-start
              (lambda (_event _ctx)
                (when (and (pai-web-auto-start-p) (not (pai-web-running-p)))
                  (run-at-time 0 nil #'pai-web--auto-start))
                nil)))

(pai-register-extension #'pai-web-extension "web")

(with-eval-after-load 'pai-settings-ui
  (pai-settings-ui-register-section 'web "Web" 60)
  (pai-settings-ui-register-subsection 'web 'server "Server" 10)
  (pai-settings-ui-register-item
   'web 'server
   :key 'web-toggle :type 'action
   :label (lambda () (if (pai-web-running-p) "Stop the server" "Start the server"))
   :doc "One server per Emacs; /web status shows the addresses"
   :action (lambda () (message "%s" (if (pai-web-running-p) (pai-web-stop) (pai-web-start)))))
  (pai-settings-ui-register-item
   'web 'server
   :key :web-auto-start :type 'boolean :label "Start automatically"
   :doc "Start the server when the first pai session opens"
   :get #'pai-web-auto-start-p
   :set (lambda (v) (pai-web-set-setting :auto-start (if v t :false))))
  (pai-settings-ui-register-item
   'web 'server
   :key :web-port :type 'number :label "Port"
   :doc "TCP port (restart the server to apply)"
   :get #'pai-web-port
   :set (lambda (v) (pai-web-set-setting :port (if (and (integerp v) (< 0 v 65536)) v
                                                pai-web-default-port))))
  (pai-settings-ui-register-item
   'web 'server
   :key :web-allowed-hosts :type 'string :label "Allowed host names"
   :doc "Names besides IP addresses and localhost the page is opened by (e.g. a VPN name), comma-separated"
   :get (lambda () (let ((v (pai-web-setting :allowed-hosts ""))) (if (stringp v) v "")))
   :set (lambda (v) (pai-web-set-setting :allowed-hosts (or v ""))))
  (pai-settings-ui-register-subsection 'web 'login "Login" 20)
  (pai-settings-ui-register-item
   'web 'login
   :key 'web-password :type 'action
   :label (lambda () (if (pai-web-password-set-p) "Change the password" "Set a password"))
   :doc "Without a password the server only accepts connections from this machine"
   :action (lambda () (message "%s" (pai-web-set-password-interactively))))
  (pai-settings-ui-register-item
   'web 'login
   :key 'web-password-clear :type 'action :label "Remove the password"
   :doc "Back to localhost-only, without login"
   :action (lambda () (message "%s" (pai-web-set-password-interactively t))))
  (pai-settings-ui-register-item
   'web 'login
   :key 'web-logout-all :type 'action :label "Log out every browser"
   :action (lambda () (pai-web-sessions-clear) (pai-web-bus-drop-session-clients t)
             (message "Every browser was logged out")))
  (pai-settings-ui-register-subsection 'web 'uploads "Attachments" 30)
  (pai-settings-ui-register-item
   'web 'uploads
   :key :web-upload-mode :type 'choice :label "Save attached files in"
   :doc "uploads: ~/.pai/web/uploads/DATE/ -- project: a folder of the instance's project"
   :choices '("uploads" "project")
   :get #'pai-web-upload-mode
   :set (lambda (v) (pai-web-set-setting :upload-mode (if (equal v "project") "project" "uploads"))))
  (pai-settings-ui-register-item
   'web 'uploads
   :key :web-upload-subdir :type 'string :label "Project folder"
   :doc "Folder of the project that receives attached files in project mode"
   :get #'pai-web-upload-subdir
   :set (lambda (v) (pai-web-set-setting :upload-subdir (if (and (stringp v) (not (string-empty-p (string-trim v))))
                                                           (string-trim v) "uploads")))))

(provide 'pai-web)
;;; pai-web.el ends here
