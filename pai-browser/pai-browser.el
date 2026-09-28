;;; pai-browser.el --- Browser bridge for pai over Playwright / chrome-devtools MCP -*- lexical-binding: t; -*-

;;; Commentary:
;; Gives the agent a real Chromium browser (Edge by default) through an
;; existing MCP server run by pai-mcp: Playwright MCP or chrome-devtools-mcp,
;; headed, headless, or attached to the user's own browser.  Adds
;; backend-neutral tools (browser_page_info/exec/fetch/screenshot/act/
;; annotations), a live screenshot view, `/tab' and `/annotate' to hand page
;; context to the next message, and `/browser' to switch backend or mode.
;; Settings live under the `pai-browser' key; see README.md.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-core)
(require 'pai-ext)
(require 'pai-settings)
(require 'pai-browser-core)
(require 'pai-browser-js)
(require 'pai-browser-tools)
(require 'pai-browser-view)

(declare-function pai-settings-ui-register-section "pai-settings-ui")
(declare-function pai-settings-ui-register-subsection "pai-settings-ui")
(declare-function pai-settings-ui-register-item "pai-settings-ui")
(declare-function pai-command-completion-tree "pai-commands")

;;;; Stash injected into the next user message

(defvar pai-browser--stash nil
  "Alist (KIND . TEXT) of captured page context for the next message.")

(defun pai-browser--stash-put (kind text)
  "Stash TEXT under KIND (replacing an earlier capture of that kind)."
  (setq pai-browser--stash (append (assq-delete-all kind pai-browser--stash)
                                   (list (cons kind text)))))

(defun pai-browser--attr (s)
  "Escape S for a double-quoted pseudo-XML attribute."
  (replace-regexp-in-string "\"" "&quot;" (or s "")))

(defun pai-browser-tab-block (url title selection)
  "Return the <browser-tab> block for URL, TITLE and SELECTION."
  (format "<browser-tab url=\"%s\" title=\"%s\">\n%s\n</browser-tab>"
          (pai-browser--attr url) (pai-browser--attr title)
          (if (string-empty-p (or selection "")) "(no selection)" selection)))

(defun pai-browser-annotations-block (url annotations &optional screenshot)
  "Return the <browser-annotations> block for URL, ANNOTATIONS and SCREENSHOT file."
  (format "<browser-annotations url=\"%s\"%s>\n%s\n</browser-annotations>"
          (pai-browser--attr url)
          (if screenshot (format " screenshot=\"%s\"" (pai-browser--attr screenshot)) "")
          (pai-json-encode (or annotations []))))

(defun pai-browser--save-screenshot (img)
  "Save IMG (DATA . MIME) under the profile root; return the file or nil."
  (when (consp img)
    (let* ((dir (pai-browser--dir (pai-browser-settings) "annotations"))
           (file (expand-file-name (format-time-string "annotations-%Y%m%dT%H%M%S.")
                                   dir)))
      (setq file (concat file (if (string-match-p "png" (cdr img)) "png" "jpg")))
      (condition-case nil (progn (pai-browser--write-body file (car img) t) file)
        (error nil)))))

(defun pai-browser--input (event _ctx)
  "Append stashed page context to the outgoing message EVENT, once."
  (when pai-browser--stash
    (let ((text (plist-get event :text))
          (blocks (mapconcat #'cdr pai-browser--stash "\n\n")))
      (setq pai-browser--stash nil)
      (list :action 'transform
            :text (if (string-empty-p (string-trim (or text ""))) blocks
                    (concat text "\n\n" blocks))))))

(defun pai-browser--notify (ctx text &optional type)
  "Tell the user TEXT via CTX's UI."
  (if ctx (pai-ext-ui-notify ctx text type) (message "%s" text)))

;;;; Commands

(defun pai-browser--cmd-tab (args ctx)
  "Handler for `/tab [clear]'."
  (if (equal (string-trim args) "clear")
      (progn (setq pai-browser--stash (assq-delete-all 'tab pai-browser--stash))
             (list :message "Browser tab context cleared."))
    (pai-browser-page-info
     (lambda (res)
       (if (not (car res))
           (pai-browser--notify ctx (format "/tab failed: %s" (cdr res)) 'error)
         (let* ((p (cdr res)) (sel (plist-get p :selection)))
           (pai-browser--stash-put 'tab (pai-browser-tab-block (plist-get p :url) (plist-get p :title) sel))
           (pai-browser--notify ctx (format "Captured tab for the next message: %s (%s)%s"
                                            (plist-get p :title) (plist-get p :url)
                                            (if (string-empty-p sel) "" (format ", %d chars selected" (length sel)))))))))
    (list :message "Capturing the current browser tab…")))

(defun pai-browser--cmd-annotate (args ctx)
  "Handler for `/annotate [done|cancel]'."
  (pcase (string-trim args)
    ((or "" "start")
     (pai-browser-eval (pai-browser-js-annotate-start)
                       (lambda (res)
                         (pai-browser--notify
                          ctx (if (car res)
                                  (concat "Annotation mode on in the browser: click elements, type notes, Esc to stop; then /annotate done."
                                          (if (equal (pai-browser-get :mode) "headless") " (headless: nobody can click -- switch to headed)" ""))
                                (format "/annotate failed: %s" (cdr res)))
                          (unless (car res) 'error))))
     (list :message "Starting annotation mode…"))
    ((or "done" "cancel")
     (let ((cancel (equal (string-trim args) "cancel")))
       (cl-flet ((collect (shot)
                   (pai-browser-annotations
                    t t
                    (lambda (res)
                      (cond
                       ((not (car res)) (pai-browser--notify ctx (format "/annotate failed: %s" (cdr res)) 'error))
                       (cancel (pai-browser--notify ctx "Annotations discarded."))
                       (t
                        (let ((list (plist-get (cdr res) :annotations)))
                          (pai-browser--stash-put
                           'annotations (pai-browser-annotations-block (plist-get (cdr res) :url) list shot))
                          (pai-browser--notify ctx (format "Captured %d annotation(s) for the next message%s."
                                                           (length list) (if shot (concat "; screenshot " shot) "")))
                          (when (fboundp 'pai-browser-view-request-refresh) (pai-browser-view-request-refresh)))))))))
         ;; Screenshot first, while the numbered badges are still on the page.
         (if cancel (collect nil)
           (pai-browser-screenshot nil (lambda (img) (collect (pai-browser--save-screenshot img)))))))
     (list :message "Collecting annotations…"))
    (_ (list :message "Usage: /annotate [start|done|cancel]"))))

(defun pai-browser-status-text ()
  "Return a one-paragraph status of the bridge."
  (let* ((name (pai-browser-current-server)))
    (format "Backend: %s  Mode: %s  Server: %s (%s)\nLast URL: %s\nStash: %s\nLive view: %s"
            (pai-browser-backend) (pai-browser-get :mode) name
            (or (pai-mcp-server-status name) 'stopped)
            (or pai-browser-last-url "-")
            (if pai-browser--stash (mapconcat (lambda (c) (symbol-name (car c))) pai-browser--stash ", ") "empty")
            (if pai-browser-view--timer "on" "off"))))

(defun pai-browser--cmd-browser (args _ctx)
  "Handler for `/browser [status|backend B|mode M|restart|stop|view|live]'."
  (let* ((parts (split-string (string-trim args) "[ \t]+" t))
         (verb (car parts)) (arg (cadr parts)))
    (pcase verb
      ((or 'nil "status") (list :message (pai-browser-status-text)))
      ("backend" (if (member arg pai-browser-backends)
                     (progn (pai-browser-set :backend arg) (pai-browser-apply)
                            (list :message (format "Browser backend: %s" arg)))
                   (list :message (format "Usage: /browser backend %s" (string-join pai-browser-backends "|")))))
      ("mode" (if (member arg pai-browser-modes)
                  (progn (pai-browser-set :mode arg) (pai-browser-apply)
                         (list :message (format "Browser mode: %s (restarts on next use)" arg)))
                (list :message (format "Usage: /browser mode %s" (string-join pai-browser-modes "|")))))
      ("restart" (pai-browser-restart) (list :message "Restarting the browser backend…"))
      ("stop" (pai-browser-stop) (list :message "Stopping the browser backend…"))
      ("view" (pai-browser-view-show) (list :message "Showing *pai-browser*."))
      ("live" (pai-browser-live-view) (list :message "Toggled the live view."))
      (_ (list :message "Usage: /browser [status|backend B|mode M|restart|stop|view|live]")))))

;;;; Interactive commands

(defun pai-browser-set-backend (backend)
  "Switch the browser BACKEND (persisted globally)."
  (interactive (list (completing-read "Backend: " pai-browser-backends nil t)))
  (pai-browser-set :backend backend)
  (pai-browser-apply)
  (message "pai-browser backend: %s" backend))

(defun pai-browser-set-mode (mode)
  "Switch the browser MODE (persisted globally)."
  (interactive (list (completing-read "Mode: " pai-browser-modes nil t)))
  (pai-browser-set :mode mode)
  (pai-browser-apply)
  (message "pai-browser mode: %s" mode))

(defun pai-browser-restart ()
  "Restart the backend server now (async)."
  (interactive)
  (let ((name (pai-browser-apply)))
    (pai-browser--graceful-stop
     name (lambda ()
            (with-current-buffer (pai-browser--origin)
              (pai-mcp-ensure name
                              (lambda () (pai-browser--refresh-direct-tools)
                                (message "pai-browser: %s ready" name))
                              (lambda (err) (message "pai-browser: %s failed: %s" name err))))))))

(defun pai-browser-stop ()
  "Stop the backend server (and its browser)."
  (interactive)
  (pai-browser-stop-all))

;;;; Settings

(defun pai-browser--on-settings-changed ()
  "Re-register the server and sync the live view after a settings change."
  (condition-case err
      (progn (pai-browser-apply) (pai-browser-view-sync-settings))
    (error (message "pai-browser: settings: %s" (error-message-string err)))))

(defun pai-browser--ui-item (sub key type label doc &optional choices)
  "Register settings UI item KEY (TYPE, LABEL, DOC, CHOICES) under SUB."
  (pai-settings-ui-register-item
   'pai-browser sub
   :key (intern (format ":pai-browser-%s" (substring (symbol-name key) 1)))
   :type type :label label :doc doc :choices choices
   :get (lambda ()
          (let ((v (pai-browser-get key)))
            (pcase type
              ('boolean (pai-truthy v))
              ('string (if (listp v) (combine-and-quote-strings (seq-filter #'stringp v)) v))
              (_ v))))
   :set (lambda (v)
          (pai-browser-set key (if (eq type 'boolean) (if v t :false) v)))))

(defun pai-browser--ui-nested (sub parent child label doc)
  "Register a string item for nested PARENT/CHILD under SUB."
  (pai-settings-ui-register-item
   'pai-browser sub
   :key (intern (format ":pai-browser-%s-%s" (substring (symbol-name parent) 1) (substring (symbol-name child) 1)))
   :type 'string :label label :doc doc
   :get (lambda () (let ((v (plist-get (pai-browser-get parent) child)))
                     (if (listp v) (combine-and-quote-strings (seq-filter #'stringp v)) v)))
   :set (lambda (v)
          (let ((plist (copy-sequence (pai-browser-get parent))))
            (pai-browser-set parent (plist-put plist child
                                               (if (eq parent :extra-args) (split-string-and-unquote (or v "")) v)))))))

(defun pai-browser--register-settings-ui ()
  "Register the Browser bridge section in the pai settings UI."
  (when (condition-case nil (require 'pai-settings-ui) (error nil)) ; needs vui
    (pai-settings-ui-register-section 'pai-browser "Browser bridge" 57)
    (pai-settings-ui-register-subsection 'pai-browser 'backend "Backend" 10)
    (pai-browser--ui-item 'backend :backend 'choice "Backend" "Playwright MCP or chrome-devtools-mcp" pai-browser-backends)
    (pai-browser--ui-item 'backend :mode 'choice "Mode" "headed (visible), headless, or attach to your own browser" pai-browser-modes)
    (pai-browser--ui-item 'backend :playwright-browser 'choice "Playwright browser" "Browser/channel for Playwright"
                          '("msedge" "chrome" "chromium" "firefox" "webkit"))
    (pai-browser--ui-item 'backend :executable 'string "Browser executable" "Browser binary launched by chrome-devtools-mcp")
    (pai-browser--ui-item 'backend :cdp-url 'string "CDP URL (attach)" "chrome-devtools attach endpoint")
    (pai-browser--ui-item 'backend :profile-root 'string "Profile root" "Dedicated profiles/outputs live here, never your main profile")
    (pai-browser--ui-item 'backend :viewport 'string "Viewport" "WIDTHxHEIGHT")
    (pai-browser--ui-item 'backend :npx 'string "npx path" "Blank: found on exec-path")
    (pai-browser--ui-nested 'backend :packages :playwright "Playwright package" "npm package spec")
    (pai-browser--ui-nested 'backend :packages :chrome-devtools "chrome-devtools package" "npm package spec")
    (pai-browser--ui-nested 'backend :extra-args :playwright "Playwright extra args" "Appended to the server argv")
    (pai-browser--ui-nested 'backend :extra-args :chrome-devtools "chrome-devtools extra args" "Appended to the server argv")
    (pai-settings-ui-register-subsection 'pai-browser 'behavior "Behavior" 20)
    (pai-browser--ui-item 'behavior :direct-tools 'boolean "Expose backend tools" "Register the backend's full MCP tool set as direct pai tools")
    (pai-browser--ui-item 'behavior :auto-approve 'boolean "Auto-approve" "Never ask pai-mcp approval for browser tool calls")
    (pai-browser--ui-item 'behavior :bypass-csp 'boolean "Bypass CSP" "Disable page CSP (Playwright)")
    (pai-browser--ui-item 'behavior :request-timeout-ms 'number "Request timeout (ms)" "Per MCP request")
    (pai-settings-ui-register-subsection 'pai-browser 'view "Live view & screenshots" 30)
    (pai-browser--ui-item 'view :live-view 'boolean "Live view" "Refresh *pai-browser* while visible")
    (pai-browser--ui-item 'view :live-view-interval 'number "Live view interval (s)" "Seconds between captures")
    (pai-browser--ui-item 'view :screenshot-format 'choice "Screenshot format" "Image format" '("jpeg" "png" "webp"))
    (pai-browser--ui-item 'view :screenshot-max-width 'number "Screenshot max width" "chrome-devtools downscale (px)")))

;;;; Extension entry point

(unless (memq #'pai-browser--kill-emacs kill-emacs-hook)
  (add-hook 'kill-emacs-hook #'pai-browser--kill-emacs))
(add-hook 'pai-settings-changed-hook #'pai-browser--on-settings-changed)

(pai-register-extension
 (lambda (api)
   (dolist (tool (pai-browser-tool-specs)) (pai-ext-register-tool api tool))
   (pai-ext-register-command api "tab" :description "Attach the current browser tab (URL, title, selection) to the next message; /tab clear"
                             :handler #'pai-browser--cmd-tab
                             :arg-completions (lambda (_p) '("clear")))
   (pai-ext-register-command api "annotate" :description "Mark page elements in the browser, then /annotate done to attach them"
                             :handler #'pai-browser--cmd-annotate
                             :arg-completions (lambda (_p) '("start" "done" "cancel")))
   (pai-ext-register-command api "browser" :description "Browser bridge: status, backend, mode, restart, stop, view, live"
                             :handler #'pai-browser--cmd-browser
                             :arg-completions
                             (if (fboundp 'pai-command-completion-tree)
                                 (pai-command-completion-tree
                                  `("status" "restart" "stop" "view" "live"
                                    ("backend" . ,pai-browser-backends) ("mode" . ,pai-browser-modes)))
                               (lambda (_p) '("status" "backend" "mode" "restart" "stop" "view" "live"))))
   (pai-ext-on api 'input #'pai-browser--input)
   (pai-ext-on api 'reload (lambda (_e _c) (pai-browser-stop-all)))
   (condition-case err (pai-browser-apply)
     (error (message "pai-browser: %s" (error-message-string err))))
   (pai-browser--register-settings-ui))
 "pai-browser")

(provide 'pai-browser)
;;; pai-browser.el ends here
