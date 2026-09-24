;;; xwidget-browser.el --- Give the agent a real browser via xwidget-webkit -*- lexical-binding: t; -*-

;;; Commentary:

;; Exposes the `browser' tool: full control of Emacs' built-in WebKit widget.
;; The agent can open pages, read what is on them, inspect and interact with
;; the DOM, run arbitrary JavaScript and take screenshots that are handed to
;; image-capable models as real image blocks.
;;
;; Sessions are ordinary `xwidget-webkit' buffers, so the browser the *user*
;; opened with `M-x xwidget-webkit-browse-url' is just another session: the
;; tool attaches to the session most recently used unless a specific one is
;; pinned with the `attach' action or named with the `session' argument.
;; Sessions the agent creates itself are head-less (they load, run scripts and
;; render without being displayed) until someone asks to `show' them.
;;
;; Screenshots are rendered in-page with html2canvas, downloaded once into
;; `xwidget-browser-cache-directory'.  Emacs has no native webkit snapshot
;; primitive, and a pgtk frame cannot be grabbed with X tools, so this is the
;; portable route; it also means captures work for never-displayed sessions.
;;
;; Gated by the `:browser (:enabled t)' setting.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai)
(require 'pai-ext)
(require 'pai-settings)
(require 'xwidget-browser-core)

(declare-function pai-settings-ui-register-section "pai-settings-ui")
(declare-function pai-settings-ui-register-subsection "pai-settings-ui")
(declare-function pai-settings-ui-register-item "pai-settings-ui")

;;;; Settings

(defun xwidget-browser-ext--setting (key default)
  "Return browser setting KEY, or DEFAULT when unset."
  (let* ((plist (pai-settings-get :browser))
         (value (plist-get plist key)))
    (cond ((null value) default)
          ((eq value :false) nil)
          (t value))))

(defun xwidget-browser-ext--apply-settings ()
  "Push the `:browser' settings into the engine's variables."
  (setq xwidget-browser-viewport-width
        (xwidget-browser-ext--setting :viewport-width 1280)
        xwidget-browser-viewport-height
        (xwidget-browser-ext--setting :viewport-height 900)
        xwidget-browser-console-capture
        (xwidget-browser-ext--setting :console-capture t)))

;;;; Result formatting

(defun xwidget-browser-ext--json (value)
  "Render VALUE (decoded JSON) as compact text."
  (cond
   ((null value) "null")
   ((eq value t) "true")
   ((eq value :false) "false")
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   (t (condition-case nil (pai-json-encode value) (error (format "%S" value))))))

(defun xwidget-browser-ext--truncate (text)
  "Truncate TEXT to the shared tool limits."
  (plist-get (pai-tools-truncate text) :text))

(defun xwidget-browser-ext--session-line (info)
  "Return a one-line description of session INFO."
  (format "%s%s  %s  %s%s%s%s"
          (plist-get info :id)
          (if (plist-get info :current) " *" "  ")
          (or (plist-get info :title) "(untitled)")
          (or (plist-get info :url) "")
          (if (eq (plist-get info :owner) 'agent) "  [agent]" "  [user]")
          (if (plist-get info :visible) " [visible]" " [head-less]")
          (if (plist-get info :pinned) " [pinned]" "")))

(defun xwidget-browser-ext--sessions-text ()
  "Return the session list as text."
  (let ((sessions (xwidget-browser-sessions)))
    (if (null sessions)
        "No browser sessions.  Use action `open' (or M-x xwidget-webkit-browse-url) to start one."
      (concat "Sessions (* = current):\n"
              (mapconcat (lambda (b)
                           (concat "  " (xwidget-browser-ext--session-line
                                         (xwidget-browser-session-info b))))
                         sessions "\n")))))

(defun xwidget-browser-ext--page-line (buffer state)
  "Return a summary line for BUFFER showing page STATE."
  (format "[%s] %s — %s%s"
          (xwidget-browser--ensure-id buffer)
          (or (plist-get state :title) "(untitled)")
          (or (plist-get state :url) "about:blank")
          (if (equal (plist-get state :readyState) "complete") ""
            (format " (readyState: %s)" (plist-get state :readyState)))))

(defun xwidget-browser-ext--element-line (el)
  "Format element description EL as one line."
  (let ((parts (list (format "[%s] <%s>" (plist-get el :index) (plist-get el :tag)))))
    (dolist (key '(:id :name :type :value :placeholder :href :label))
      (when (plist-get el key)
        (push (format "%s=%s" (substring (symbol-name key) 1)
                      (truncate-string-to-width (format "%s" (plist-get el key)) 80 nil nil t))
              parts)))
    (when (and (plist-get el :text) (not (string-empty-p (plist-get el :text))))
      (push (format "%S" (plist-get el :text)) parts))
    (when (eq (plist-get el :disabled) t) (push "disabled" parts))
    (when (plist-member el :checked)
      (push (if (eq (plist-get el :checked) t) "checked" "unchecked") parts))
    (unless (eq (plist-get el :visible) t) (push "hidden" parts))
    (let ((rect (plist-get el :rect)))
      (when (and rect (eq (plist-get el :visible) t))
        (push (format "@%s,%s %sx%s" (plist-get rect :x) (plist-get rect :y)
                      (plist-get rect :w) (plist-get rect :h))
              parts)))
    (string-join (nreverse parts) "  ")))

;;;; Session resolution

(defun xwidget-browser-ext--session (args)
  "Return the session buffer for ARGS, or a string describing the problem."
  (let* ((spec (plist-get args :session))
         (buffer (xwidget-browser-resolve spec)))
    (or buffer
        (if spec
            (format "No browser session matches %S.\n%s" spec
                    (xwidget-browser-ext--sessions-text))
          (concat "No browser session is open.  Use action `open' with a url"
                  " (or ask the user to run M-x xwidget-webkit-browse-url).")))))

;;;; Actions

(defun xwidget-browser-ext--done (on-done text &optional details)
  "Finish a tool call through ON-DONE with TEXT and optional DETAILS."
  (funcall on-done (pai-tool-ok-result (xwidget-browser-ext--truncate text) details)))

(defun xwidget-browser-ext--fail (on-done text)
  "Finish a tool call through ON-DONE with error TEXT."
  (funcall on-done (pai-tool-error-result (xwidget-browser-ext--truncate text))))

(defun xwidget-browser-ext--reply (on-done format-fn)
  "Return a callback finishing via ON-DONE, rendering values with FORMAT-FN.
A `:error' key inside the page's own result is reported as a failure."
  (lambda (result)
    (let ((value (plist-get result :value)))
      (cond
       ((not (plist-get result :ok))
        (xwidget-browser-ext--fail on-done (or (plist-get result :error) "browser action failed")))
       ((and (consp value) (plist-get value :error))
        (xwidget-browser-ext--fail on-done (format "%s" (plist-get value :error))))
       (t (funcall format-fn value))))))

(defun xwidget-browser-ext--open (args on-done)
  "Handle the `open' action described by ARGS, finishing via ON-DONE."
  (let* ((url (plist-get args :url))
         (spec (plist-get args :session))
         (new (pai-truthy (plist-get args :new)))
         (existing (xwidget-browser-resolve spec))
         (buffer (cond
                  ((and existing (not new)) existing)
                  ((and spec (not existing) (not new))
                   nil)
                  (t (xwidget-browser-create nil 'agent)))))
    (cond
     ((null buffer)
      (xwidget-browser-ext--fail on-done (format "No browser session matches %S" spec)))
     ((null url)
      (xwidget-browser-attach buffer)
      (xwidget-browser-state
       buffer
       (xwidget-browser-ext--reply
        on-done
        (lambda (state)
          (when (pai-truthy (plist-get args :visible)) (xwidget-browser-show buffer))
          (xwidget-browser-ext--done on-done (xwidget-browser-ext--page-line buffer state))))))
     (t
      (xwidget-browser-attach buffer)
      (xwidget-browser-goto
       buffer (xwidget-browser-ext--normalize-url url)
       (lambda (result)
         (if (not (plist-get result :ok))
             (xwidget-browser-ext--fail on-done (plist-get result :error))
           (when (pai-truthy (plist-get args :visible)) (xwidget-browser-show buffer))
           (let ((state (plist-get result :value)))
             (xwidget-browser-ext--done
              on-done
              (concat (xwidget-browser-ext--page-line buffer state)
                      (when (plist-get result :timed-out)
                        (format "\nNote: the page was still loading after %ss; content may be incomplete."
                                (plist-get args :timeout)))
                      "\nUse action `text' to read it or `screenshot' to look at it.")
              (list :session (xwidget-browser--ensure-id buffer)
                    :url (plist-get state :url))))))
       (or (plist-get args :timeout) xwidget-browser-load-timeout))))))

(defun xwidget-browser-ext--normalize-url (url)
  "Add a scheme to URL when it looks naked."
  (if (string-match-p "\\`[A-Za-z][-+.A-Za-z0-9]*:" url) url (concat "https://" url)))

(defun xwidget-browser-ext--screenshot (buffer args on-done)
  "Capture BUFFER as configured by ARGS and finish via ON-DONE."
  (xwidget-browser-screenshot
   buffer
   (list :selector (plist-get args :selector)
         :full-page (pai-truthy (plist-get args :full_page))
         :max-width (or (plist-get args :max_width)
                        (xwidget-browser-ext--setting :screenshot-max-width 1400))
         :max-height (or (plist-get args :max_height) 4000)
         :format (or (plist-get args :format) "png")
         :quality (plist-get args :quality)
         :file (plist-get args :save_path))
   (lambda (result)
     (if (not (plist-get result :ok))
         (xwidget-browser-ext--fail
          on-done (format "screenshot failed: %s" (plist-get result :error)))
       (let* ((shot (plist-get result :value))
              (summary (format "Screenshot of %s (%s) — %sx%s px%s%s"
                               (or (plist-get shot :title) "page")
                               (or (plist-get shot :url) "")
                               (plist-get shot :width) (plist-get shot :height)
                               (if (and (plist-get shot :source-width)
                                        (/= (plist-get shot :source-width)
                                            (plist-get shot :width)))
                                   (format " (scaled from %sx%s)"
                                           (plist-get shot :source-width)
                                           (plist-get shot :source-height))
                                 "")
                               (if (plist-get shot :file)
                                   (format ", saved to %s" (plist-get shot :file)) ""))))
         (funcall on-done
                  (list :content (list (pai-text summary)
                                       (pai-image (plist-get shot :data) (plist-get shot :mime)))
                        :is-error :false
                        :details (list :width (plist-get shot :width)
                                       :height (plist-get shot :height)
                                       :file (plist-get shot :file)))))))))

(defun xwidget-browser-ext--history (buffer args on-done)
  "Run the history action named in ARGS on BUFFER, finishing via ON-DONE."
  (let ((action (plist-get args :action))
        (xw (xwidget-browser--xwidget buffer)))
    ;; Use the primitives directly: the interactive commands act on the
    ;; current buffer's session and their signatures vary across releases.
    (pcase action
      ("back" (xwidget-webkit-goto-history xw -1))
      ("forward" (xwidget-webkit-goto-history xw 1))
      ("reload" (xwidget-webkit-goto-history xw 0))
      ("stop" (xwidget-webkit-stop-loading xw)))
    (run-at-time
     (if (equal action "stop") 0.1 0.6) nil
     (lambda ()
       (xwidget-browser-state
        buffer
        (xwidget-browser-ext--reply
         on-done
         (lambda (state)
           (xwidget-browser-ext--done
            on-done (concat action ": " (xwidget-browser-ext--page-line buffer state))))))))))

(defun xwidget-browser-ext--dispatch (args ctx on-done)
  "Execute the browser ARGS in CTX, finishing through ON-DONE."
  (ignore ctx)
  (let* ((action (or (plist-get args :action) ""))
         (standalone (member action '("sessions" "attach" "open")))
         (buffer (unless standalone (xwidget-browser-ext--session args))))
    (if (and (not standalone) (stringp buffer))
        ;; `--session' returned an explanation instead of a session.
        (xwidget-browser-ext--fail on-done buffer)
      ;; Keep the resolved session current so follow-up calls stay on it.
      (when (bufferp buffer) (xwidget-browser-set-current buffer))
      (xwidget-browser-ext--act action buffer args on-done))))

(defun xwidget-browser-ext--act (action buffer args on-done)
  "Run ACTION on session BUFFER with ARGS, finishing through ON-DONE."
  (pcase action
      ("sessions" (xwidget-browser-ext--done on-done (xwidget-browser-ext--sessions-text)))
      ("open" (xwidget-browser-ext--open args on-done))
      ("attach"
       (let ((target (xwidget-browser-attach (plist-get args :session))))
         (if (not target)
             (xwidget-browser-ext--fail
              on-done (concat "Could not attach.\n" (xwidget-browser-ext--sessions-text)))
           (xwidget-browser-ext--done
            on-done (concat "Attached to "
                            (xwidget-browser-ext--session-line
                             (xwidget-browser-session-info target))
                            "\n" (xwidget-browser-ext--sessions-text))))))
      ("close"
       (let ((info (xwidget-browser-session-info buffer)))
         (xwidget-browser-close buffer)
         (xwidget-browser-ext--done
          on-done (format "Closed session %s (%s).\n%s" (plist-get info :id)
                          (or (plist-get info :url) "")
                          (xwidget-browser-ext--sessions-text)))))
      ("show"
       (xwidget-browser-show buffer)
       (xwidget-browser-ext--done
        on-done (format "Showing session %s in a window."
                        (xwidget-browser--ensure-id buffer))))
      ("hide"
       (xwidget-browser-hide buffer)
       (xwidget-browser-ext--done
        on-done (format "Session %s is no longer displayed (it keeps running)."
                        (xwidget-browser--ensure-id buffer))))
      ((or "back" "forward" "reload" "stop")
       (xwidget-browser-ext--history buffer args on-done))
      ("info"
       (xwidget-browser-state
        buffer
        (xwidget-browser-ext--reply
         on-done
         (lambda (state)
           (xwidget-browser-ext--done
            on-done
            (format "%s\nviewport %sx%s, scroll %s/%s\n%s"
                    (xwidget-browser-ext--page-line buffer state)
                    (plist-get state :width) (plist-get state :height)
                    (plist-get state :scrollY) (plist-get state :scrollHeight)
                    (xwidget-browser-ext--sessions-text)))))))
      ("screenshot" (xwidget-browser-ext--screenshot buffer args on-done))
      ("text"
       (xwidget-browser-text
        buffer (plist-get args :selector) (or (plist-get args :max_chars) 20000)
        (xwidget-browser-ext--reply
         on-done
         (lambda (page)
           (xwidget-browser-ext--done
            on-done
            (format "%s — %s\n\n%s%s"
                    (or (plist-get page :title) "") (plist-get page :url)
                    (plist-get page :text)
                    (if (eq (plist-get page :truncated) t)
                        (format "\n\n[truncated: %s characters total; raise max_chars or use a selector]"
                                (plist-get page :length))
                      "")))))))
      ("html"
       (xwidget-browser-html
        buffer (plist-get args :selector) (or (plist-get args :max_chars) 20000)
        (xwidget-browser-ext--reply
         on-done
         (lambda (page)
           (xwidget-browser-ext--done
            on-done (concat (plist-get page :html)
                            (if (eq (plist-get page :truncated) t)
                                (format "\n[truncated: %s characters total]"
                                        (plist-get page :length))
                              "")))))))
      ("links"
       (xwidget-browser-links
        buffer (or (plist-get args :limit) 100)
        (xwidget-browser-ext--reply
         on-done
         (lambda (page)
           (xwidget-browser-ext--done
            on-done
            (if (null (plist-get page :links)) "No links on this page."
              (mapconcat (lambda (l) (format "%s\n    %s"
                                             (if (string-empty-p (plist-get l :text))
                                                 "(no text)" (plist-get l :text))
                                             (plist-get l :href)))
                         (plist-get page :links) "\n")))))))
      ("elements"
       (xwidget-browser-elements
        buffer (plist-get args :selector) (or (plist-get args :limit) 50)
        (xwidget-browser-ext--reply
         on-done
         (lambda (page)
           (xwidget-browser-ext--done
            on-done
            (if (null (plist-get page :elements))
                (format "No elements match %s" (plist-get page :selector))
              (format "%s of %s elements matching %s:\n%s"
                      (plist-get page :shown) (plist-get page :total)
                      (plist-get page :selector)
                      (mapconcat #'xwidget-browser-ext--element-line
                                 (plist-get page :elements) "\n"))))))))
      ("click"
       (xwidget-browser-click
        buffer (plist-get args :selector) (plist-get args :text) (plist-get args :index)
        (xwidget-browser-ext--reply
         on-done
         (lambda (res)
           (xwidget-browser-ext--settle
            buffer
            (lambda (state)
              (xwidget-browser-ext--done
               on-done
               (format "Clicked %s\nNow on %s"
                       (xwidget-browser-ext--element-line (plist-get res :clicked))
                       (xwidget-browser-ext--page-line buffer state)))))))))
      ("fill"
       (xwidget-browser-fill
        buffer (plist-get args :selector) (plist-get args :text) (plist-get args :value)
        (pai-truthy (plist-get args :submit))
        (xwidget-browser-ext--reply
         on-done
         (lambda (res)
           (xwidget-browser-ext--settle
            buffer
            (lambda (state)
              (xwidget-browser-ext--done
               on-done
               (format "Filled %s%s\nNow on %s"
                       (xwidget-browser-ext--element-line (plist-get res :filled))
                       (if (eq (plist-get res :submitted) t) " and submitted the form" "")
                       (xwidget-browser-ext--page-line buffer state)))))))))
      ("select"
       (xwidget-browser-select
        buffer (plist-get args :selector) (plist-get args :value)
        (xwidget-browser-ext--reply
         on-done
         (lambda (res)
           (let ((sel (plist-get res :selected)))
             (xwidget-browser-ext--done
              on-done (format "Selected %S (value %s)" (plist-get sel :text)
                              (plist-get sel :value))))))))
      ("key"
       (xwidget-browser-key
        buffer (or (plist-get args :key) "Enter") (plist-get args :selector)
        (xwidget-browser-ext--reply
         on-done
         (lambda (res)
           (xwidget-browser-ext--settle
            buffer
            (lambda (state)
              (xwidget-browser-ext--done
               on-done (format "Sent %s to %s\nNow on %s"
                               (plist-get res :key)
                               (xwidget-browser-ext--element-line (plist-get res :target))
                               (xwidget-browser-ext--page-line buffer state)))))))))
      ("scroll"
       (xwidget-browser-scroll
        buffer (plist-get args :to) (plist-get args :by)
        (xwidget-browser-ext--reply
         on-done
         (lambda (res)
           (xwidget-browser-ext--done
            on-done (format "Scrolled to y=%s of %s (viewport %s px)"
                            (plist-get res :scrollY) (plist-get res :scrollHeight)
                            (plist-get res :innerHeight)))))))
      ("js"
       (let ((script (plist-get args :script)))
         (if (or (null script) (string-empty-p script))
             (xwidget-browser-ext--fail on-done "js requires a `script' argument")
           (xwidget-browser-js
            buffer script
            (xwidget-browser-ext--reply
             on-done
             (lambda (value)
               (xwidget-browser-ext--done
                on-done (xwidget-browser-ext--json value))))
            (or (plist-get args :timeout) xwidget-browser-js-timeout)))))
      ("wait"
       (xwidget-browser-wait
        buffer (cond ((plist-get args :selector) (list :selector (plist-get args :selector)))
                     ((plist-get args :script) (list :js (plist-get args :script)))
                     (t (list :load t)))
        (or (plist-get args :timeout) 15)
        (lambda (result)
          (if (not (plist-get result :ok))
              (xwidget-browser-ext--fail on-done (plist-get result :error))
            (xwidget-browser-state
             buffer
             (xwidget-browser-ext--reply
              on-done
              (lambda (state)
                (xwidget-browser-ext--done
                 on-done (concat "Condition met.\n"
                                 (xwidget-browser-ext--page-line buffer state))))))))))
      ("console"
       (xwidget-browser-console
        buffer (or (plist-get args :limit) 50)
        (xwidget-browser-ext--reply
         on-done
         (lambda (logs)
           (xwidget-browser-ext--done
            on-done
            (cond
             ((null logs)
              "No console output captured for this document (the hook is installed on load).")
             (t (mapconcat (lambda (entry)
                             (format "[%s] %s" (plist-get entry :level) (plist-get entry :text)))
                           logs "\n"))))))))
      (_ (xwidget-browser-ext--fail
          on-done (format "Unsupported browser action: %s"
                          (if (string-empty-p action) "(missing)" action))))))

(defun xwidget-browser-ext--settle (buffer cb)
  "Give BUFFER a moment to navigate, then call CB with its page state."
  (run-at-time
   0.5 nil
   (lambda ()
     (xwidget-browser-state
      buffer (lambda (result) (funcall cb (or (plist-get result :value) '())))))))

(defun xwidget-browser-ext--execute (args ctx _on-update on-done)
  "Entry point of the `browser' tool for ARGS in CTX, finishing via ON-DONE."
  (cond
   ((not (xwidget-browser-ext--setting :enabled t))
    (xwidget-browser-ext--fail
     on-done "Browser support is disabled (:browser :enabled).  Enable it in settings."))
   ((not (featurep 'xwidget-internal))
    (xwidget-browser-ext--fail
     on-done "This Emacs was built without xwidget support, so there is no browser to drive."))
   ((not (display-graphic-p))
    (xwidget-browser-ext--fail
     on-done "xwidget-webkit needs a graphical Emacs frame; this session is text-only."))
   (t
    (xwidget-browser-ext--apply-settings)
    (condition-case err
        (xwidget-browser-ext--dispatch args ctx on-done)
      (error (xwidget-browser-ext--fail on-done (error-message-string err)))))))

;;;; Tool registration

(pai-register-tool
 (list
  :name "browser"
  :label "Browser"
  :description "Control a real WebKit browser (Emacs xwidget-webkit): navigate, read pages, inspect and interact with the DOM, run JavaScript, and take screenshots returned as images. Sessions the user opened by hand are shared, and actions target the current session unless `session' names another (see the `sessions' action). Sessions the agent opens are head-less until `show'. Actions: sessions, attach, open, close, show, hide, back, forward, reload, stop, info, text, html, links, elements, screenshot, click, fill, select, key, scroll, wait, js, console."
  :prompt-snippet "browser: drive a real WebKit browser (pages, DOM, JS, screenshots)"
  :prompt-guidelines
  '("Prefer `browser' over curl/fetch when a page needs JavaScript, a login session, or when you need to see how it looks."
    "Read pages with `text' (cheap) and only use `screenshot' when layout or visuals matter."
    "Use `elements' to discover selectors before `click'/`fill'; `click' and `fill' also accept plain visible text.")
  :execution-mode 'sequential
  :parameters
  (pai-object-schema
   (list :action (pai-string-schema
                  "Browser action to perform."
                  :enum '("sessions" "attach" "open" "close" "show" "hide"
                          "back" "forward" "reload" "stop" "info"
                          "text" "html" "links" "elements" "screenshot"
                          "click" "fill" "select" "key" "scroll" "wait" "js" "console"))
         :url (pai-string-schema "URL to open (action `open').")
         :session (pai-string-schema
                   "Session to act on: id (s1), buffer name, or a substring of its url/title. Defaults to the current session.")
         :new (pai-boolean-schema "With `open': force a brand new session instead of reusing the current one.")
         :visible (pai-boolean-schema "With `open': also display the session in an Emacs window.")
         :selector (pai-string-schema "CSS selector for the action's target element.")
         :text (pai-string-schema "Visible text used to find the element when no selector is given (click/fill).")
         :value (pai-string-schema "Value to type (`fill') or option to choose (`select').")
         :submit (pai-boolean-schema "With `fill': submit the surrounding form afterwards.")
         :index (pai-number-schema "Which match to use when several elements match (default 0).")
         :key (pai-string-schema "Key to send with `key', e.g. Enter, Tab, Escape, ArrowDown, a.")
         :script (pai-string-schema
                  "JavaScript to evaluate in the page (`js'), or a condition expression (`wait'). An expression value or a promise is returned; `return' may be used for multi-statement code.")
         :to (pai-string-schema "With `scroll': \"top\", \"bottom\", or a selector to scroll into view.")
         :by (pai-number-schema "With `scroll': pixels to scroll by (default one viewport).")
         :full_page (pai-boolean-schema "With `screenshot': capture the whole document instead of the viewport.")
         :max_width (pai-number-schema "With `screenshot': downscale so the image is at most this wide (default 1400).")
         :max_height (pai-number-schema "With `screenshot': downscale so the image is at most this tall (default 4000).")
         :format (pai-string-schema "With `screenshot': \"png\" (default) or \"jpeg\"." :enum '("png" "jpeg"))
         :quality (pai-number-schema "JPEG quality between 0 and 1 (default 0.85).")
         :save_path (pai-string-schema "With `screenshot': also write the image to this file.")
         :max_chars (pai-number-schema "Maximum characters returned by `text'/`html' (default 20000).")
         :limit (pai-number-schema "Maximum entries returned by `links'/`elements'/`console'.")
         :timeout (pai-number-schema "Seconds to wait for this action (load, js, wait)."))
   '("action"))
  :execute #'xwidget-browser-ext--execute))

;;;; Interactive commands

;;;###autoload
(defun xwidget-browser-open (url &optional headless)
  "Open URL in a browser session and display it.
With a prefix argument (HEADLESS non-nil), keep the session off-screen."
  (interactive (list (read-string "URL: " "https://") current-prefix-arg))
  (let ((buffer (xwidget-browser-create nil 'user)))
    (xwidget-browser-goto buffer (xwidget-browser-ext--normalize-url url)
                          (lambda (result)
                            (message "xwidget-browser: %s"
                                     (xwidget-browser-ext--page-line
                                      buffer (plist-get result :value)))))
    (unless headless (xwidget-browser-show buffer))
    buffer))

;;;###autoload
(defun xwidget-browser-attach-session (buffer)
  "Pin BUFFER as the session the agent acts on."
  (interactive
   (list (let* ((sessions (xwidget-browser-sessions))
                (names (mapcar (lambda (b)
                                 (cons (xwidget-browser-ext--session-line
                                        (xwidget-browser-session-info b))
                                       b))
                               sessions)))
           (unless names (user-error "No live xwidget-webkit sessions"))
           (cdr (assoc (completing-read "Attach agent to session: " names nil t)
                       names)))))
  (xwidget-browser-attach buffer)
  (message "Agent attached to %s" (buffer-name buffer)))

;;;###autoload
(defun xwidget-browser-capture (file &optional full-page)
  "Save a screenshot of the current session to FILE and show it.
With a prefix argument, capture the FULL-PAGE document."
  (interactive
   (list (read-file-name "Save screenshot to: "
                         (file-name-as-directory xwidget-browser-cache-directory)
                         nil nil
                         (format-time-string "shot-%Y%m%d-%H%M%S.png"))
         current-prefix-arg))
  (let ((buffer (xwidget-browser-resolve)))
    (unless buffer (user-error "No live xwidget-webkit session"))
    (xwidget-browser-screenshot
     buffer (list :file file :full-page full-page)
     (lambda (result)
       (if (plist-get result :ok)
           (progn (message "Saved %s" (plist-get (plist-get result :value) :file))
                  (find-file-other-window (plist-get (plist-get result :value) :file)))
         (message "Screenshot failed: %s" (plist-get result :error)))))))

;;;; Slash command

(defun xwidget-browser-ext--command (args _ctx)
  "Handle the /browser slash command with ARGS."
  (let* ((args (string-trim (or args "")))
         (parts (split-string args " " t)))
    (pcase (car parts)
      ('nil (list :message (xwidget-browser-ext--sessions-text)))
      ("attach"
       (let ((buffer (xwidget-browser-attach (cadr parts))))
         (list :message (if buffer
                            (format "Agent attached to %s" (buffer-name buffer))
                          "No matching session."))))
      ("show"
       (let ((buffer (xwidget-browser-resolve (cadr parts))))
         (when buffer (xwidget-browser-show buffer))
         (list :message (if buffer (format "Showing %s" (buffer-name buffer))
                          "No matching session."))))
      ("close"
       (let ((n (xwidget-browser-cleanup-owned)))
         (list :message (format "Closed %d agent session(s)." n))))
      (_ (xwidget-browser-open args)
         (list :message (format "Opening %s" args))))))

;;;; Extension registration

(pai-register-extension
 (lambda (pi)
   (pai-ext-register-command
    pi "browser"
    :description "List browser sessions, /browser URL to open one, /browser attach [id]"
    :handler #'xwidget-browser-ext--command)
   (pai-ext-on pi 'session-end
               (lambda (_event _ctx) (xwidget-browser-cleanup-owned)))))

;;;; Settings UI

(with-eval-after-load 'pai-settings-ui
  (pai-settings-ui-register-section 'browser "Browser" 58)
  (pai-settings-ui-register-subsection 'browser 'general "General" 10)
  (pai-settings-ui-register-item
   'browser 'general
   :key :browser-enabled :type 'boolean :label "Enable browser tool"
   :doc "Expose the xwidget-webkit `browser' tool to the agent"
   :get (lambda () (xwidget-browser-ext--setting :enabled t))
   :set (lambda (v)
          (let ((plist (copy-sequence (pai-settings-get :browser))))
            (pai-settings-set :browser (plist-put plist :enabled (if v t :false)) 'project))))
  (pai-settings-ui-register-item
   'browser 'general
   :key :browser-console :type 'boolean :label "Capture console output"
   :doc "Hook console.* and errors in visited pages so `console' can report them"
   :get (lambda () (xwidget-browser-ext--setting :console-capture t))
   :set (lambda (v)
          (let ((plist (copy-sequence (pai-settings-get :browser))))
            (pai-settings-set :browser (plist-put plist :console-capture (if v t :false)) 'project))))
  (pai-settings-ui-register-subsection 'browser 'viewport "Viewport" 20)
  (pai-settings-ui-register-item
   'browser 'viewport
   :key :browser-viewport-width :type 'number :label "Viewport width"
   :doc "CSS width of head-less sessions the agent opens"
   :get (lambda () (xwidget-browser-ext--setting :viewport-width 1280))
   :set (lambda (v)
          (let ((plist (copy-sequence (pai-settings-get :browser))))
            (pai-settings-set :browser (plist-put plist :viewport-width v) 'project))))
  (pai-settings-ui-register-item
   'browser 'viewport
   :key :browser-viewport-height :type 'number :label "Viewport height"
   :doc "CSS height of head-less sessions the agent opens"
   :get (lambda () (xwidget-browser-ext--setting :viewport-height 900))
   :set (lambda (v)
          (let ((plist (copy-sequence (pai-settings-get :browser))))
            (pai-settings-set :browser (plist-put plist :viewport-height v) 'project))))
  (pai-settings-ui-register-item
   'browser 'viewport
   :key :browser-screenshot-width :type 'number :label "Screenshot max width"
   :doc "Screenshots are downscaled to at most this many pixels wide"
   :get (lambda () (xwidget-browser-ext--setting :screenshot-max-width 1400))
   :set (lambda (v)
          (let ((plist (copy-sequence (pai-settings-get :browser))))
            (pai-settings-set :browser (plist-put plist :screenshot-max-width v) 'project)))))

(provide 'xwidget-browser)
;;; xwidget-browser.el ends here
