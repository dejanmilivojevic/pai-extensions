;;; pai-browser-core.el --- Settings and MCP backend management for pai-browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Turns the `pai-browser' settings into one runtime pai-mcp server (Playwright
;; MCP or chrome-devtools-mcp) and offers async helpers to call it.  Nothing
;; here blocks: servers start lazily through `pai-mcp-ensure'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-settings)
;; pai-mcp is a sibling extension; it may not be on `load-path' yet.
(let ((mcp (expand-file-name "../pai-mcp" (file-name-directory
                                            (or load-file-name buffer-file-name default-directory)))))
  (when (file-directory-p mcp) (add-to-list 'load-path mcp)))
(require 'pai-mcp-client)
(require 'pai-mcp-config)
(require 'pai-mcp-sources)
(require 'pai-mcp-direct)

(defconst pai-browser-backends '("playwright" "chrome-devtools"))
(defconst pai-browser-modes '("headed" "headless" "attach"))

(defconst pai-browser-defaults
  (list :backend "playwright"
        :mode "headed"
        :executable "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
        :playwright-browser "msedge"
        :profile-root "~/.pai/browser/"
        :cdp-url "http://127.0.0.1:9222"
        :viewport "1280x800"
        :packages (list :playwright "@playwright/mcp@0.0.82"
                        :chrome-devtools "chrome-devtools-mcp@latest")
        :extra-args (list :playwright nil :chrome-devtools nil)
        :npx nil
        :direct-tools t
        :auto-approve :false
        :bypass-csp t
        :live-view :false
        :live-view-interval 2
        :screenshot-format "jpeg"
        :screenshot-max-width 1280
        :request-timeout-ms 180000)
  "Defaults for the `pai-browser' settings section.")

(defun pai-browser--merge (base over)
  "Deep-merge plist OVER onto BASE; nested keyword plists merge recursively."
  (let ((out (copy-sequence base)))
    (while over
      (let* ((k (car over)) (v (cadr over)) (old (plist-get out k)))
        (setq out (plist-put out k (if (and (pai-mcp--object-p old) old
                                            (pai-mcp--object-p v) v)
                                       (pai-browser--merge old v)
                                     v))))
      (setq over (cddr over)))
    out))

(defun pai-browser-settings ()
  "Return effective settings: defaults < global < project `pai-browser'."
  (let ((global (ignore-errors (pai-settings-scope-value :pai-browser 'global)))
        (project (ignore-errors (pai-settings-scope-value :pai-browser 'project))))
    (pai-browser--merge (pai-browser--merge pai-browser-defaults
                                            (and (pai-mcp--object-p global) global))
                        (and (pai-mcp--object-p project) project))))

(defun pai-browser-get (key &optional settings)
  "Return setting KEY from SETTINGS (default: current effective settings)."
  (plist-get (or settings (pai-browser-settings)) key))

(defun pai-browser-set (key value)
  "Persist KEY=VALUE in the global `pai-browser' section (other keys kept)."
  (let* ((raw (ignore-errors (pai-settings-scope-value :pai-browser 'global)))
         (plist (copy-sequence (and (pai-mcp--object-p raw) raw))))
    (pai-settings-set :pai-browser (plist-put plist key (if (null value) :false value)) 'global)))

;;;; Server definitions

(defun pai-browser-server-name (&optional backend)
  "Return the pai-mcp server name used for BACKEND."
  (concat "pai-" (or backend "playwright")))

(defun pai-browser--dir (settings name)
  "Return profile-root subdirectory NAME under SETTINGS, created on demand."
  (let ((dir (expand-file-name name (expand-file-name (pai-browser-get :profile-root settings)))))
    (make-directory dir t)
    dir))

(defun pai-browser--npx (settings)
  "Return the npx executable for SETTINGS."
  (or (pai-browser-get :npx settings) (executable-find "npx") "npx"))

(defun pai-browser--extra (settings backend)
  "Return extra argv strings for BACKEND from SETTINGS."
  (let ((v (plist-get (pai-browser-get :extra-args settings) (intern (concat ":" backend)))))
    (cond ((stringp v) (split-string-and-unquote v))
          ((or (listp v) (vectorp v)) (seq-filter #'stringp (append v nil))))))

(defun pai-browser--pw-config (settings)
  "Write the Playwright MCP config file for SETTINGS and return its path."
  (let ((file (expand-file-name "playwright-config.json" (pai-browser--dir settings "")))
        (json (pai-json-encode
               (list :browser
                     (list :contextOptions
                           (list :bypassCSP (if (pai-truthy (pai-browser-get :bypass-csp settings)) t :false)))))))
    (unless (and (file-exists-p file)
                 (equal json (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (with-temp-file file (insert json)))
    file))

(defun pai-browser--args (settings)
  "Return the server argv (after npx) for SETTINGS."
  (let* ((backend (pai-browser-get :backend settings))
         (mode (pai-browser-get :mode settings))
         (pkg (plist-get (pai-browser-get :packages settings) (intern (concat ":" backend))))
         (viewport (pai-browser-get :viewport settings)))
    (append
     (list "-y" pkg)
     (pcase backend
       ("chrome-devtools"
        (append
         (list "--no-usage-statistics" "--no-performance-crux" "--no-page-id-routing"
               "--experimentalVision")
         (if (equal mode "attach")
             (list "--browserUrl" (pai-browser-get :cdp-url settings))
           (append (list "--executablePath" (expand-file-name (pai-browser-get :executable settings))
                         "--userDataDir" (pai-browser--dir settings "chrome-devtools-profile"))
                   (when viewport (list "--viewport" viewport))
                   (when (equal mode "headless") (list "--headless"))))
         (list "--screenshotFormat" (pai-browser-get :screenshot-format settings))
         (when-let ((w (pai-browser-get :screenshot-max-width settings)))
           (list "--screenshotMaxWidth" (number-to-string w)))))
       (_
        (append
         (list "--browser" (pai-browser-get :playwright-browser settings)
               "--caps" "vision,devtools"
               "--output-dir" (pai-browser--dir settings "playwright-output"))
         (if (equal mode "attach")
             (list "--extension")
           (append (list "--user-data-dir" (pai-browser--dir settings "playwright-profile")
                         "--config" (pai-browser--pw-config settings))
                   (when viewport (list "--viewport-size" viewport))
                   (when (equal mode "headless") (list "--headless")))))))
     (pai-browser--extra settings backend))))

(defun pai-browser-server-def (&optional settings)
  "Return the pai-mcp server definition plist for SETTINGS."
  (let* ((settings (or settings (pai-browser-settings)))
         (npx (pai-browser--npx settings))
         (bin (and (file-name-absolute-p npx) (file-name-directory npx))))
    (append
     ;; sh keeps stderr (banners, warnings) out of the JSON-RPC stdout stream;
     ;; `exec' keeps one process, so stopping it still stops the server.
     (list :command "/bin/sh"
           :args (append (list "-c" "exec \"$0\" \"$@\" 2>>\"$PAI_BROWSER_LOG\"" npx)
                         (pai-browser--args settings))
           :cwd (pai-browser--dir settings "")
           :lifecycle "lazy-keep-alive"
           :requestTimeoutMs (pai-browser-get :request-timeout-ms settings)
           :toolPrefix "short"
           :directTools (if (pai-truthy (pai-browser-get :direct-tools settings)) t :false))
     ;; npx's `#!/usr/bin/env node' needs node on PATH even in a GUI Emacs.
     (list :env (append
                 (list :PAI_BROWSER_LOG (expand-file-name (concat (pai-browser-get :backend settings) ".log")
                                                          (pai-browser--dir settings "")))
                 (when bin
                   (list :PATH (concat (directory-file-name bin) path-separator (or (getenv "PATH") ""))))))
     (when (pai-truthy (pai-browser-get :auto-approve settings))
       (list :approveTools :false)))))

;;;; Registration and lifecycle

(defvar pai-browser--active nil
  "(NAME . DEF) of the currently registered backend server, or nil.")

(defvar pai-browser--registration nil
  "Registration plist returned by `pai-mcp-register-server'.")

(defvar pai-browser--prepared nil
  "Server generation that already had per-connection setup applied.")

(defvar pai-browser-last-url nil "Last page URL seen by pai-browser.")

(defun pai-browser-current-server ()
  "Return the registered server name, registering it first if needed."
  (unless pai-browser--active (pai-browser-apply))
  (car pai-browser--active))

(defun pai-browser--pai-buffers ()
  "Return live pai session buffers."
  (seq-filter (lambda (b) (with-current-buffer b (derived-mode-p 'pai-mode))) (buffer-list)))

(defun pai-browser--refresh-direct-tools ()
  "Re-register pai-mcp direct tools in every pai session buffer."
  (dolist (b (pai-browser--pai-buffers))
    (with-current-buffer b
      (ignore-errors (pai-mcp-register-direct-tools default-directory)))))

(defun pai-browser--graceful-stop (name &optional then)
  "Close NAME's stdin so it can shut its browser down, then stop it; call THEN."
  (let* ((server (and (fboundp 'pai-mcp--server) (gethash name pai-mcp--servers)))
         (proc (and server (plist-get server :process))))
    (if (not (and proc (process-live-p proc)))
        (progn (ignore-errors (pai-mcp-stop name)) (when then (funcall then)))
      (ignore-errors (process-send-eof proc))
      (let ((tries 0) timer)
        (setq timer
              (run-at-time
               0.2 0.2
               (lambda ()
                 (when (or (not (process-live-p proc)) (>= (cl-incf tries) 25))
                   (cancel-timer timer)
                   (ignore-errors (pai-mcp-stop name))
                   (when then (funcall then))))))))))

(defun pai-browser-apply (&optional force)
  "Register the server for the current settings; restart when it changed.
With FORCE, stop and re-register even when unchanged.  Never connects."
  (let* ((settings (pai-browser-settings))
         (name (pai-browser-server-name (pai-browser-get :backend settings)))
         (def (pai-browser-server-def settings))
         (old pai-browser--active))
    (when (or force (not (equal old (cons name def))))
      (when old
        (let ((old-name (car old)))
          (pai-browser--graceful-stop old-name)
          (unless (equal old-name name) (pai-mcp-unregister-server old-name 'pai-browser))))
      (setq pai-browser--registration (pai-mcp-register-server name def 'pai-browser)
            pai-browser--active (cons name def)
            pai-browser--prepared nil)
      (pai-browser--refresh-direct-tools))
    name))

(defun pai-browser-stop-all ()
  "Stop the backend server (graceful)."
  (when pai-browser--active (pai-browser--graceful-stop (car pai-browser--active))))

(defun pai-browser--kill-emacs ()
  "Close backend stdin and kill it when Emacs exits."
  (when-let* ((name (car pai-browser--active))
              (server (gethash name pai-mcp--servers))
              (proc (plist-get server :process)))
    (when (process-live-p proc)
      (ignore-errors (process-send-eof proc))
      (ignore-errors (delete-process proc)))))

;;;; Calling the backend

(defun pai-browser--origin ()
  "Return a live buffer to originate calls from (pai-mcp needs one)."
  (if (derived-mode-p 'pai-mode) (current-buffer)
    (or (car (pai-browser--pai-buffers))
        (get-buffer-create " *pai-browser-origin*"))))

(defun pai-browser-backend ()
  "Return the backend of the registered server."
  (if pai-browser--active
      (string-remove-prefix "pai-" (car pai-browser--active))
    (pai-browser-get :backend)))

(defun pai-browser-has-tool (tool)
  "Return non-nil when the backend's cached catalog lists TOOL."
  (seq-some (lambda (tl) (equal (plist-get tl :name) tool))
            (pai-mcp--server-tools (pai-browser-current-server))))

(defun pai-browser-call (tool args on-done)
  "Call backend TOOL with ARGS plist; ON-DONE gets the raw MCP result."
  (let ((name (pai-browser-current-server)))
    (with-current-buffer (pai-browser--origin)
      (pai-mcp-call-raw name tool (or args (pai-json-empty-object)) on-done))))

(defun pai-browser-result-text (result)
  "Return the concatenated text blocks of raw MCP RESULT."
  (mapconcat (lambda (b) (or (plist-get b :text) ""))
             (seq-filter (lambda (b) (equal (plist-get b :type) "text"))
                         (append (plist-get result :content) nil))
             "\n"))

(defun pai-browser-result-image (result)
  "Return (DATA . MIME) of the first image block in raw MCP RESULT, or nil."
  (when-let ((b (seq-find (lambda (b) (equal (plist-get b :type) "image"))
                          (append (plist-get result :content) nil))))
    (cons (plist-get b :data) (or (plist-get b :mimeType) "image/png"))))

(defun pai-browser-result-error-p (result)
  "Return non-nil when raw MCP RESULT is an error."
  (eq (plist-get result :isError) t))

(defun pai-browser-extract-json (text)
  "Extract the JSON value a backend printed for an evaluation in TEXT.
Playwright prints `### Result' sections; chrome-devtools prints ```json fences."
  (let ((raw (cond
              ((string-match "```json\n\\(\\(?:.\\|\n\\)*?\\)\n```" text) (match-string 1 text))
              ((string-match "### Result\n\\(\\(?:.\\|\n\\)*?\\)\\(?:\n### \\|\\'\\)" text) (match-string 1 text))
              (t (string-trim text)))))
    (condition-case nil (pai-json-decode (string-trim raw)) (error nil))))

(defun pai-browser-decode-eval (result)
  "Decode the {ok ...} object a pai-browser JS snippet returned in RESULT.
Return (OK . PLIST) or (nil . ERROR-STRING)."
  (let* ((text (pai-browser-result-text result))
         (value (pai-browser-extract-json text))
         (obj (if (stringp value) (condition-case nil (pai-json-decode value) (error nil)) value)))
    (cond
     ((pai-browser-result-error-p result) (cons nil (string-trim text)))
     ((not (pai-mcp--object-p obj)) (cons nil (format "unexpected backend output: %s"
                                                     (truncate-string-to-width text 500))))
     ((pai-truthy (plist-get obj :ok)) (cons t obj))
     (t (cons nil (or (plist-get obj :error) "evaluation failed"))))))

(defun pai-browser--prepare (then)
  "Run per-connection setup once per server generation, then call THEN."
  (let* ((name (pai-browser-current-server))
         (gen (plist-get (gethash name pai-mcp--servers) :generation)))
    (if (or (equal gen pai-browser--prepared)
            (not (equal (pai-browser-backend) "playwright"))
            (not (equal (pai-browser-get :mode) "attach"))
            (not (pai-truthy (pai-browser-get :bypass-csp))))
        (progn (setq pai-browser--prepared gen) (funcall then))
      ;; Launch modes get bypassCSP from the config file; attach needs CDP.
      (pai-browser-call (pai-browser-run-code-tool) (list :code (pai-browser-js-pw-bypass-csp))
                        (lambda (_r) (setq pai-browser--prepared gen) (funcall then))))))

(defun pai-browser-run-code-tool ()
  "Return Playwright's run-code tool name for the installed version."
  (if (pai-browser-has-tool "browser_run_code") "browser_run_code" "browser_run_code_unsafe"))

(defun pai-browser-eval (js on-done)
  "Evaluate page JS (a function source) and call ON-DONE with (OK . VALUE)."
  (pai-browser--prepare
   (lambda ()
     (pai-browser-call
      (if (equal (pai-browser-backend) "chrome-devtools") "evaluate_script" "browser_evaluate")
      (list :function js)
      (lambda (result) (funcall on-done (pai-browser-decode-eval result)))))))

(defun pai-browser-pw-code (code on-done)
  "Run Playwright page CODE (see `pai-browser-js-pw'); ON-DONE gets (OK . VALUE)."
  (pai-browser--prepare
   (lambda ()
     (pai-browser-call (pai-browser-run-code-tool) (list :code code)
                       (lambda (result) (funcall on-done (pai-browser-decode-eval result)))))))

(require 'pai-browser-js)
(provide 'pai-browser-core)
;;; pai-browser-core.el ends here
