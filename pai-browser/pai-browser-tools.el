;;; pai-browser-tools.el --- Backend-neutral browser tools for pai -*- lexical-binding: t; -*-

;;; Commentary:
;; browser_page_info, browser_exec, browser_fetch, browser_screenshot,
;; browser_act and browser_annotations.  Each translates to the active
;; backend (Playwright MCP or chrome-devtools-mcp).  The backends' own tools
;; stay available as pai-mcp direct tools for anything not covered here.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-tools)
(require 'pai-browser-core)
(require 'pai-browser-js)

(declare-function pai-browser-view-update "pai-browser-view")
(declare-function pai-browser-view-request-refresh "pai-browser-view")

(defconst pai-browser-inline-body-max 20000
  "Characters of a fetched body returned inline (the rest needs `save_to').")

(defun pai-browser--ok (text &optional details)
  "Return an ok tool result with TEXT and DETAILS."
  (pai-tool-ok-result text details))

(defun pai-browser--err (text)
  "Return an error tool result with TEXT."
  (pai-tool-error-result text))

(defun pai-browser--json (value)
  "Pretty-ish JSON for VALUE."
  (pai-json-encode value))

;;;; page_info

(defun pai-browser--tabs (on-done)
  "Call ON-DONE with the backend's tab listing text (or an error string)."
  (if (equal (pai-browser-backend) "chrome-devtools")
      (pai-browser-call "list_pages" nil (lambda (r) (funcall on-done (pai-browser-result-text r))))
    (pai-browser-call "browser_tabs" (list :action "list")
                      (lambda (r) (funcall on-done (pai-browser-result-text r))))))

(defun pai-browser-page-info (on-done)
  "Call ON-DONE with (OK . PLIST) holding url, title, selection and tabs."
  (pai-browser-eval
   (pai-browser-js-page-info)
   (lambda (res)
     (if (not (car res)) (funcall on-done res)
       (setq pai-browser-last-url (plist-get (cdr res) :url))
       (pai-browser--tabs
        (lambda (tabs)
          (funcall on-done (cons t (plist-put (cdr res) :tabs (string-trim tabs))))))))))

(defun pai-browser--tool-page-info (_args _ctx _on-update on-done)
  "Execute browser_page_info."
  (pai-browser-page-info
   (lambda (res)
     (funcall on-done
              (if (car res)
                  (let ((p (cdr res)))
                    (pai-browser--ok
                     (format "URL: %s\nTitle: %s\nReady: %s\nSelection: %s\nBackend: %s (%s)\n\nTabs:\n%s"
                             (plist-get p :url) (plist-get p :title) (plist-get p :readyState)
                             (let ((s (plist-get p :selection))) (if (string-empty-p s) "(none)" s))
                             (pai-browser-backend) (pai-browser-get :mode)
                             (plist-get p :tabs))
                     (list :url (plist-get p :url) :title (plist-get p :title))))
                (pai-browser--err (cdr res)))))))

;;;; exec

(defun pai-browser--tool-exec (args _ctx _on-update on-done)
  "Execute browser_exec."
  (let ((code (plist-get args :code))
        (ms (* 1000 (or (plist-get args :timeout_s) 30))))
    (if (or (null code) (string-empty-p (string-trim code)))
        (funcall on-done (pai-browser--err "browser_exec requires `code'"))
      (pai-browser-eval
       (pai-browser-js-exec code ms)
       (lambda (res)
         (funcall on-done
                  (if (car res)
                      (pai-browser--ok (pai-browser--json (plist-get (cdr res) :value)))
                    (pai-browser--err (format "browser_exec failed: %s" (cdr res))))))))))

;;;; fetch

(defun pai-browser--write-body (file body base64)
  "Write BODY (BASE64-encoded when BASE64) to FILE; return its sha256."
  (make-directory (file-name-directory file) t)
  (let ((bytes (if base64 (base64-decode-string body)
                 (encode-coding-string body 'utf-8))))
    (let ((coding-system-for-write 'no-conversion))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert bytes)
        (write-region (point-min) (point-max) file nil 'silent)))
    (secure-hash 'sha256 bytes)))

(defun pai-browser--tool-fetch (args ctx _on-update on-done)
  "Execute browser_fetch."
  (let* ((url (plist-get args :url))
         (method (upcase (or (plist-get args :method) "GET")))
         (save-to (plist-get args :save_to))
         (base64 (pai-truthy (plist-get args :base64))))
    (cond
     ((or (null url) (string-empty-p url)) (funcall on-done (pai-browser--err "browser_fetch requires `url'")))
     ((not (member method '("GET" "HEAD")))
      (funcall on-done (pai-browser--err "browser_fetch is read-only: method must be GET or HEAD")))
     (t
      (pai-browser-eval
       (pai-browser-js-fetch url method (plist-get args :headers) base64
                             (* 1000 (or (plist-get args :timeout_s) 60)))
       (lambda (res)
         (funcall
          on-done
          (if (not (car res))
              (pai-browser--err (format "browser_fetch failed: %s" (cdr res)))
            (let* ((p (cdr res))
                   (body (or (plist-get p :body) ""))
                   (b64 (equal (plist-get p :encoding) "base64"))
                   (file (and save-to (pai-tool-resolve-path ctx save-to)))
                   (sha (and file (condition-case err (pai-browser--write-body file body b64)
                                    (error (format "write failed: %s" (error-message-string err))))))
                   (inline (if (> (length body) pai-browser-inline-body-max)
                               (concat (substring body 0 pai-browser-inline-body-max)
                                       (format "\n[... truncated: %d of %d chars shown%s]"
                                               pai-browser-inline-body-max (length body)
                                               (if file "; full body saved" "; pass save_to for the full body")))
                             body)))
              (pai-browser--ok
               (concat (format "HTTP %s %s\nURL: %s\nContent-Type: %s\n"
                               (plist-get p :status) (or (plist-get p :statusText) "")
                               (plist-get p :url) (or (plist-get p :contentType) "?"))
                       (when file (format "Saved: %s\nSHA-256: %s\n" file sha))
                       "\n" inline)
               (list :status (plist-get p :status) :url (plist-get p :url)
                     :headers (plist-get p :headers) :saved file :sha256 sha)))))))))))

;;;; screenshot

(defun pai-browser-screenshot (args on-done)
  "Take a screenshot per ARGS (:full_page :selector).
ON-DONE gets (DATA . MIME) or an error string."
  (let* ((fmt (or (pai-browser-get :screenshot-format) "jpeg"))
         (full (pai-truthy (plist-get args :full_page)))
         (selector (plist-get args :selector))
         (cdt (equal (pai-browser-backend) "chrome-devtools")))
    (if (and cdt selector)
        (funcall on-done "selector screenshots are unsupported on chrome-devtools; use its take_snapshot uid with the direct take_screenshot tool")
      (pai-browser-call
       (if cdt "take_screenshot" "browser_take_screenshot")
       (if cdt
           (append (list :format fmt) (unless (equal fmt "png") (list :quality 70))
                   (when full (list :fullPage t)))
         (append (list :type fmt :scale "css")
                 (when full (list :fullPage t))
                 (when selector (list :target selector :element selector))))
       (lambda (r)
         (let ((img (pai-browser-result-image r)))
           (funcall on-done
                    (cond ((pai-browser-result-error-p r) (pai-browser-result-text r))
                          (img img)
                          (t (format "no image returned: %s" (pai-browser-result-text r)))))))))))

(defun pai-browser--tool-screenshot (args _ctx _on-update on-done)
  "Execute browser_screenshot."
  (pai-browser-screenshot
   args
   (lambda (img)
     (if (stringp img) (funcall on-done (pai-browser--err img))
       (when (fboundp 'pai-browser-view-update) (pai-browser-view-update (car img) (cdr img)))
       (funcall on-done
                (list :content (list (pai-text (format "Screenshot (%s, %d bytes base64) of %s"
                                                       (cdr img) (length (car img))
                                                       (or pai-browser-last-url "current page")))
                                     (pai-image (car img) (cdr img)))
                      :is-error :false))))))

;;;; act

(defconst pai-browser-actions
  '("navigate" "back" "reload" "click" "dblclick" "hover" "type" "press" "select"
    "drag" "scroll" "tap" "wait" "new_tab" "select_tab" "close_tab"))

(defun pai-browser--need (args &rest keys)
  "Return an error string unless ARGS has one of KEYS."
  (unless (seq-some (lambda (k) (plist-get args k)) keys)
    (format "requires %s" (mapconcat (lambda (k) (substring (symbol-name k) 1)) keys " or "))))

(defun pai-browser-act-plan (action args backend)
  "Translate ACTION with ARGS for BACKEND.
Return (:tool NAME :args PLIST), (:eval JS), (:pw CODE),
\(:click-at SELECTOR ...) or (:error MESSAGE)."
  (let ((sel (plist-get args :selector)) (x (plist-get args :x)) (y (plist-get args :y))
        (text (plist-get args :text)) (pw (not (equal backend "chrome-devtools"))))
    (cl-flet ((need (&rest keys) (apply #'pai-browser--need args keys))
              (unsupported () (list :error (format "`%s' is unsupported on the %s backend; use its direct tools (take_snapshot uids) instead" action backend))))
      (let ((missing
             (pcase action
               ("navigate" (need :url))
               ((or "click" "dblclick" "hover" "tap") (unless (or sel (and x y)) "requires selector or x+y"))
               ("type" (need :text))
               ("press" (need :key))
               ("select" (or (need :selector) (need :text)))
               ("drag" (unless (or (and sel (plist-get args :to_selector))
                                   (and x y (plist-get args :to_x) (plist-get args :to_y)))
                         "requires selector+to_selector or x,y,to_x,to_y"))
               ((or "select_tab") (need :index))
               ((pred (lambda (a) (not (member a pai-browser-actions))))
                (format "unknown action; one of: %s" (string-join pai-browser-actions ", "))))))
        (if missing (list :error (format "browser_act %s %s" action missing))
          (if pw
              (pcase action
                ("navigate" (list :tool "browser_navigate" :args (list :url (plist-get args :url))))
                ("back" (list :tool "browser_navigate_back" :args nil))
                ("reload" (list :pw (pai-browser-js-pw "await page.reload(); return {url: page.url()};")))
                ((or "click" "dblclick")
                 (let ((dbl (equal action "dblclick")))
                   (if sel
                       (list :tool "browser_click"
                             :args (append (list :target sel :element sel) (when dbl (list :doubleClick t))))
                     (list :pw (pai-browser-js-pw
                                (format "%s await page.mouse.click(x, y, {clickCount: %d}); return {clicked: [x, y]};"
                                        (pai-browser-js-pw-point nil x y) (if dbl 2 1)))))))
                ("hover" (if sel (list :tool "browser_hover" :args (list :target sel :element sel))
                           (list :pw (pai-browser-js-pw (format "%s await page.mouse.move(x, y); return {hovered: [x, y]};"
                                                                (pai-browser-js-pw-point nil x y))))))
                ("type" (if sel (list :tool "browser_type" :args (list :target sel :element sel :text text))
                          (list :pw (pai-browser-js-pw (format "await page.keyboard.type(%s); return {typed: true};"
                                                               (pai-browser-js-str text))))))
                ("press" (list :tool "browser_press_key" :args (list :key (plist-get args :key))))
                ("select" (list :tool "browser_select_option" :args (list :target sel :element sel :values (list text))))
                ("drag" (if sel
                            (list :tool "browser_drag" :args (list :startTarget sel :startElement sel
                                                                   :endTarget (plist-get args :to_selector)
                                                                   :endElement (plist-get args :to_selector)))
                          (list :pw (pai-browser-js-pw
                                     (format "await page.mouse.move(%s, %s); await page.mouse.down();
                                              await page.mouse.move(%s, %s, {steps: 12}); await page.mouse.up();
                                              return {dragged: true};"
                                             x y (plist-get args :to_x) (plist-get args :to_y))))))
                ("scroll" (list :eval (pai-browser-js-scroll sel (plist-get args :delta_x) (or (plist-get args :delta_y) 600))))
                ("tap" (list :pw (pai-browser-js-pw-tap sel x y)))
                ("wait" (if text (list :tool "browser_wait_for" :args (list :text text))
                          (list :eval (pai-browser-js-sleep (or (plist-get args :ms) 1000)))))
                ("new_tab" (list :tool "browser_tabs" :args (append (list :action "new")
                                                                    (when (plist-get args :url) (list :url (plist-get args :url))))))
                ("select_tab" (list :tool "browser_tabs" :args (list :action "select" :index (plist-get args :index))))
                ("close_tab" (list :tool "browser_tabs" :args (append (list :action "close")
                                                                      (when (plist-get args :index) (list :index (plist-get args :index)))))))
            (pcase action
              ("navigate" (list :tool "navigate_page" :args (list :type "url" :url (plist-get args :url))))
              ("back" (list :tool "navigate_page" :args (list :type "back")))
              ("reload" (list :tool "navigate_page" :args (list :type "reload")))
              ((or "click" "dblclick")
               (list :click-at sel :x x :y y :dbl (equal action "dblclick")))
              ("type" (if sel (list :click-at sel :then (list :tool "type_text" :args (list :text text)))
                        (list :tool "type_text" :args (list :text text))))
              ("press" (list :tool "press_key" :args (list :key (plist-get args :key))))
              ("select" (list :eval (pai-browser-js-select sel text)))
              ("scroll" (list :eval (pai-browser-js-scroll sel (plist-get args :delta_x) (or (plist-get args :delta_y) 600))))
              ("wait" (if text (list :tool "wait_for" :args (list :text (list text)))
                        (list :eval (pai-browser-js-sleep (or (plist-get args :ms) 1000)))))
              ("new_tab" (list :tool "new_page" :args (list :url (or (plist-get args :url) "about:blank"))))
              ("select_tab" (list :tool "select_page" :args (list :pageId (plist-get args :index) :bringToFront t)))
              ("close_tab" (if (plist-get args :index)
                               (list :tool "close_page" :args (list :pageId (plist-get args :index)))
                             (list :error "close_tab on chrome-devtools requires index (a pageId from browser_page_info)")))
              (_ (unsupported)))))))))

(defun pai-browser--short (text)
  "Trim backend TEXT for the model."
  (let ((s (string-trim (or text ""))))
    (if (> (length s) 4000) (concat (substring s 0 4000) "\n[... backend output truncated]") s)))

(defun pai-browser-run-plan (plan on-done)
  "Execute PLAN from `pai-browser-act-plan'; ON-DONE gets (OK . TEXT)."
  (cond
   ((plist-get plan :error) (funcall on-done (cons nil (plist-get plan :error))))
   ((plist-get plan :tool)
    (pai-browser--prepare
     (lambda ()
       (pai-browser-call (plist-get plan :tool) (plist-get plan :args)
                         (lambda (r) (funcall on-done (cons (not (pai-browser-result-error-p r))
                                                            (pai-browser--short (pai-browser-result-text r)))))))))
   ((plist-get plan :eval)
    (pai-browser-eval (plist-get plan :eval)
                      (lambda (res) (funcall on-done (cons (car res) (if (car res) (pai-browser--json (cdr res)) (cdr res)))))))
   ((plist-get plan :pw)
    (pai-browser-pw-code (plist-get plan :pw)
                         (lambda (res) (funcall on-done (cons (car res) (if (car res) (pai-browser--json (cdr res)) (cdr res)))))))
   ((plist-member plan :click-at)
    (let ((click (lambda (x y)
                   (pai-browser-call "click_at" (append (list :x x :y y) (when (plist-get plan :dbl) (list :dblClick t)))
                                     (lambda (r)
                                       (if (or (pai-browser-result-error-p r) (not (plist-get plan :then)))
                                           (funcall on-done (cons (not (pai-browser-result-error-p r))
                                                                  (pai-browser--short (pai-browser-result-text r))))
                                         (pai-browser-run-plan (plist-get plan :then) on-done)))))))
      (if (plist-get plan :click-at)
          (pai-browser-eval (pai-browser-js-element-center (plist-get plan :click-at))
                            (lambda (res) (if (car res) (funcall click (plist-get (cdr res) :x) (plist-get (cdr res) :y))
                                            (funcall on-done res))))
        (funcall click (plist-get plan :x) (plist-get plan :y)))))
   (t (funcall on-done (cons nil "empty plan")))))

(defun pai-browser--tool-act (args _ctx _on-update on-done)
  "Execute browser_act."
  (let* ((action (plist-get args :action))
         (plan (pai-browser-act-plan action args (pai-browser-backend))))
    (pai-browser-run-plan
     plan
     (lambda (res)
       (when (fboundp 'pai-browser-view-request-refresh) (pai-browser-view-request-refresh))
       (funcall on-done (if (car res) (pai-browser--ok (format "%s: ok\n%s" action (cdr res)))
                          (pai-browser--err (format "%s failed: %s" action (cdr res)))))))))

;;;; annotations

(defun pai-browser-annotations (finish clear on-done)
  "Collect page annotations; FINISH removes the overlay, CLEAR resets.
ON-DONE gets (OK . PLIST)."
  (pai-browser-eval (pai-browser-js-annotations finish clear) on-done))

(defun pai-browser--tool-annotations (args _ctx _on-update on-done)
  "Execute browser_annotations."
  (pai-browser-annotations
   nil (pai-truthy (plist-get args :clear))
   (lambda (res)
     (funcall on-done
              (if (car res)
                  (pai-browser--ok (pai-browser--json (list :url (plist-get (cdr res) :url)
                                                           :active (plist-get (cdr res) :active)
                                                           :annotations (plist-get (cdr res) :annotations))))
                (pai-browser--err (cdr res)))))))

;;;; registration

(defun pai-browser-tool-specs ()
  "Return the native pai-browser tool plists."
  (list
   (list :name "browser_page_info" :label "Browser page info"
         :description "Show the current browser tab: URL, title, readyState, selected text, backend/mode and the list of open tabs. Read-only; call first to confirm which page the other browser_* tools act on."
         :execution-mode 'sequential
         :parameters (pai-object-schema nil)
         :execute #'pai-browser--tool-page-info)
   (list :name "browser_exec" :label "Browser JS"
         :description "Run JavaScript in the current page (CDP evaluation, so page CSP does not block it) and return the JSON-serialized result. `code' is a function source such as \"() => document.title\" or \"async () => { const r = await fetch('/x'); return r.status }\", or a plain expression. Non-serializable values (DOM nodes) fail; return plain data."
         :execution-mode 'sequential
         :parameters (pai-object-schema
                      (list :code (pai-string-schema "JavaScript function source or expression evaluated in the page")
                            :timeout_s (pai-number-schema "Timeout in seconds (default 30)"))
                      '("code"))
         :execute #'pai-browser--tool-exec)
   (list :name "browser_fetch" :label "Browser fetch"
         :description "Read-only, same-origin HTTP GET/HEAD performed inside the current page with the page's cookies/session (credentials: include) -- e.g. Azure DevOps REST calls from an open, logged-in dev.azure.com tab, without exposing tokens. Refuses other origins and methods. Returns status, content type and body (inline body truncated to 20000 chars; pass `save_to' to write the full body to a file and get its SHA-256)."
         :execution-mode 'sequential
         :parameters (pai-object-schema
                      (list :url (pai-string-schema "Absolute or page-relative URL on the page's origin")
                            :method (pai-string-schema "GET (default) or HEAD" :enum ["GET" "HEAD"])
                            :headers (list :type "object" :description "Extra request headers, e.g. {\"Accept\": \"application/json\"}")
                            :save_to (pai-string-schema "File path to write the full response body to")
                            :base64 (pai-boolean-schema "Fetch the body as base64 (binary content)")
                            :timeout_s (pai-number-schema "Timeout in seconds (default 60)"))
                      '("url"))
         :execute #'pai-browser--tool-fetch)
   (list :name "browser_screenshot" :label "Browser screenshot"
         :description "Screenshot the current page viewport (or the full page, or one element by CSS selector on Playwright) and return it as an image. Also refreshes the *pai-browser* live view."
         :execution-mode 'sequential
         :parameters (pai-object-schema
                      (list :full_page (pai-boolean-schema "Capture the full scrollable page")
                            :selector (pai-string-schema "CSS selector of one element to capture (Playwright only)")))
         :execute #'pai-browser--tool-screenshot)
   (list :name "browser_act" :label "Browser action"
         :description (concat "Drive the browser like a user. action: navigate(url) | back | reload | click/dblclick/hover/tap(selector or x+y) | type(text, optional selector to focus first) | press(key, e.g. Enter, Control+A) | select(selector, text=option value) | drag(selector+to_selector, or x,y,to_x,to_y) | scroll(selector into view, or delta_x/delta_y px) | wait(ms, or text to appear) | new_tab(url?) | select_tab(index) | close_tab(index?). "
                              "Selectors are CSS. Coordinates are viewport CSS pixels. On chrome-devtools, hover/drag/tap are unsupported (use its direct uid-based tools). Irreversible actions (submitting forms, posting comments) need the user's explicit authorization.")
         :execution-mode 'sequential
         :parameters (pai-object-schema
                      (list :action (pai-string-schema "Action to perform" :enum (vconcat pai-browser-actions))
                            :selector (pai-string-schema "CSS selector of the target element")
                            :to_selector (pai-string-schema "CSS selector of the drop target (drag)")
                            :x (pai-number-schema "Viewport x") :y (pai-number-schema "Viewport y")
                            :to_x (pai-number-schema "Drag end x") :to_y (pai-number-schema "Drag end y")
                            :delta_x (pai-number-schema "Scroll delta x") :delta_y (pai-number-schema "Scroll delta y (default 600)")
                            :text (pai-string-schema "Text to type / option value / text to wait for")
                            :key (pai-string-schema "Key to press")
                            :url (pai-string-schema "URL for navigate/new_tab")
                            :index (pai-number-schema "Tab index (Playwright) or pageId (chrome-devtools)")
                            :ms (pai-number-schema "Milliseconds to wait"))
                      '("action"))
         :execute #'pai-browser--tool-act)
   (list :name "browser_annotations" :label "Browser annotations"
         :description "Read the elements the user marked in the current page with /annotate: number, CSS selector, text, the user's note, page rect and an HTML snippet. clear=true resets them."
         :execution-mode 'sequential
         :parameters (pai-object-schema (list :clear (pai-boolean-schema "Reset the annotation list after reading")))
         :execute #'pai-browser--tool-annotations)))

(provide 'pai-browser-tools)
;;; pai-browser-tools.el ends here
