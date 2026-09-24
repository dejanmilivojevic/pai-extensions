;;; xwidget-browser-core.el --- Drive xwidget-webkit sessions from Lisp -*- lexical-binding: t; -*-

;;; Commentary:

;; The engine behind the `browser' tool: a thin, fully asynchronous control
;; layer over Emacs' built-in `xwidget-webkit' widget.
;;
;; Design notes:
;;
;; * A *session* is simply a buffer holding a live webkit xwidget — exactly
;;   what `xwidget-webkit-browse-url' creates.  That means sessions the user
;;   opened by hand are first-class: `xwidget-browser-resolve' attaches to the
;;   user's current session by default, and only creates one when asked.
;;
;; * Sessions do not need to be displayed in a window.  A buffer created with
;;   `xwidget-webkit--create-new-session-buffer' loads pages, runs JavaScript,
;;   fires timers and renders off-screen, so the agent can browse headlessly
;;   and the user can pop the buffer up at any time (`xwidget-browser-show').
;;
;; * `xwidget-webkit-execute-script' only hands back the *synchronous* value of
;;   a script, and only in a few types.  `xwidget-browser-js' therefore wraps
;;   every script in an envelope that (a) JSON-encodes the result, (b) parks
;;   promises in a window slot that Lisp polls until settled, and (c) streams
;;   oversized payloads (screenshot data URLs) back in chunks.
;;
;; * Screenshots: this Emacs has no native webkit snapshot primitive, and a
;;   pgtk/Wayland frame cannot be captured with X tools, so pixels are produced
;;   inside the page by html2canvas (downloaded once and cached under
;;   `xwidget-browser-cache-directory') and returned as PNG/JPEG data.
;;
;; Every public entry point is callback based: CB is called with a plist
;; `(:ok t :value V)' or `(:ok nil :error STRING)'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xwidget)

(defgroup xwidget-browser nil
  "Programmatic control of xwidget-webkit sessions."
  :group 'external
  :prefix "xwidget-browser-")

(defcustom xwidget-browser-viewport-width 1280
  "CSS width requested for head-less sessions created by this package."
  :type 'integer)

(defcustom xwidget-browser-viewport-height 900
  "CSS height requested for head-less sessions created by this package."
  :type 'integer)

(defcustom xwidget-browser-js-timeout 20
  "Seconds to wait for a JavaScript evaluation to produce a value."
  :type 'number)

(defcustom xwidget-browser-load-timeout 30
  "Seconds to wait for a navigation to finish loading."
  :type 'number)

(defcustom xwidget-browser-poll-interval 0.1
  "Seconds between polls when waiting for promises, loads or conditions."
  :type 'number)

(defcustom xwidget-browser-cache-directory
  (expand-file-name "xwidget-browser"
                    (or (bound-and-true-p pai-directory) user-emacs-directory))
  "Directory holding downloaded assets (html2canvas) and saved screenshots."
  :type 'directory)

(defcustom xwidget-browser-html2canvas-url
  "https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js"
  "Where to fetch html2canvas from the first time a screenshot is taken."
  :type 'string)

(defcustom xwidget-browser-html2canvas-file nil
  "Local html2canvas file to use instead of the downloaded copy."
  :type '(choice (const :tag "Use cached download" nil) file))

(defcustom xwidget-browser-console-capture t
  "When non-nil, hook `console' and error events in visited pages."
  :type 'boolean)

(defconst xwidget-browser--inline-limit 60000
  "Envelope size (JS string length) above which results are chunked.")

(defconst xwidget-browser--chunk-size 200000
  "Number of JS string units transferred per chunk.")

;;;; Sessions --------------------------------------------------------------

(defvar-local xwidget-browser--id nil
  "Stable short identifier of the session in this buffer.")

(defvar-local xwidget-browser--owner nil
  "Who created this session: `agent' or nil (the user).")

(defvar-local xwidget-browser--hooked nil
  "Non-nil once the console/error hook has been installed for this session.")

(defvar-local xwidget-browser-private nil
  "Non-nil in a session used internally by another package.
Private sessions are left out of `xwidget-browser-sessions', so they are
never listed, picked as the default session, or navigated by the agent;
they can still be driven by passing their buffer explicitly.")

(defvar xwidget-browser--id-counter 0
  "Counter backing `xwidget-browser--id'.")

(defvar xwidget-browser--attached nil
  "Session buffer most recently acted on, when live.")

(defvar xwidget-browser--pinned nil
  "Session buffer explicitly pinned with `xwidget-browser-attach'.
A pin survives until it dies or until the user starts a browser session
of their own, which is taken as a request to follow them.")

(defvar xwidget-browser--internal nil
  "Bound to non-nil while this package drives `xwidget-webkit' itself.")

(defun xwidget-browser--xwidget (buffer)
  "Return the live webkit xwidget of BUFFER, or nil."
  (when (buffer-live-p buffer)
    (let ((xw (with-current-buffer buffer (xwidget-at (point-min)))))
      (when (and xw (xwidget-live-p xw)) xw))))

(defun xwidget-browser-session-p (buffer)
  "Return non-nil when BUFFER holds a live webkit session."
  (and (xwidget-browser--xwidget buffer) t))

(defun xwidget-browser--ensure-id (buffer)
  "Return the stable session id of BUFFER, assigning one if needed."
  (with-current-buffer buffer
    (or xwidget-browser--id
        (setq xwidget-browser--id
              (format "s%d" (cl-incf xwidget-browser--id-counter))))))

(defun xwidget-browser-sessions ()
  "Return all live webkit sessions, most recently used first."
  (let ((last xwidget-webkit-last-session-buffer)
        (bufs (seq-filter (lambda (b)
                            (and (xwidget-browser-session-p b)
                                 (not (buffer-local-value 'xwidget-browser-private b))))
                          (buffer-list))))
    (dolist (b bufs)
      (xwidget-browser--ensure-id b)
      (xwidget-browser-watch b))
    (if (and last (memq last bufs))
        (cons last (delq last bufs))
      bufs)))

(defun xwidget-browser-session-info (buffer)
  "Return a plist describing session BUFFER."
  (let ((xw (xwidget-browser--xwidget buffer)))
    (list :id (xwidget-browser--ensure-id buffer)
          :buffer (buffer-name buffer)
          :url (and xw (xwidget-webkit-uri xw))
          :title (and xw (xwidget-webkit-title xw))
          :owner (or (buffer-local-value 'xwidget-browser--owner buffer) 'user)
          :visible (and (get-buffer-window buffer t) t)
          :pinned (eq buffer xwidget-browser--pinned)
          :current (eq buffer (xwidget-browser-resolve)))))

(defun xwidget-browser-resolve (&optional spec)
  "Return the session buffer designated by SPEC, or nil.

SPEC may be nil, a buffer, a session id (\"s2\"), a buffer name, or a
substring of the session's URL or title.  With SPEC nil the session is,
in order: the pinned one, the one most recently used (including sessions
the user opened by hand), the one last acted on, or any live session."
  (cond
   ((bufferp spec) (and (xwidget-browser-session-p spec) spec))
   ((null spec)
    (or (and (xwidget-browser-session-p xwidget-browser--pinned) xwidget-browser--pinned)
        (and (xwidget-browser-session-p xwidget-webkit-last-session-buffer)
             xwidget-webkit-last-session-buffer)
        (and (xwidget-browser-session-p xwidget-browser--attached) xwidget-browser--attached)
        (car (xwidget-browser-sessions))))
   ((stringp spec)
    (let ((sessions (xwidget-browser-sessions))
          (needle (downcase spec)))
      (or (seq-find (lambda (b) (equal spec (xwidget-browser--ensure-id b))) sessions)
          (seq-find (lambda (b) (equal spec (buffer-name b))) sessions)
          (seq-find (lambda (b)
                      (let ((info (xwidget-browser-session-info b)))
                        (or (string-search needle (downcase (or (plist-get info :url) "")))
                            (string-search needle (downcase (or (plist-get info :title) "")))
                            (string-search needle (downcase (buffer-name b))))))
                    sessions))))))

(defun xwidget-browser-create (&optional url owner)
  "Create a new head-less session showing URL and return its buffer.
OWNER defaults to `agent' and marks the session for automatic cleanup."
  (let* ((xwidget-browser--internal t)
         (buffer (save-window-excursion
                   (xwidget-webkit--create-new-session-buffer (or url "about:blank")))))
    (with-current-buffer buffer
      (setq xwidget-browser--owner (or owner 'agent)))
    (xwidget-browser--ensure-id buffer)
    (xwidget-browser-watch buffer)
    (setq xwidget-browser--attached buffer
          xwidget-browser--pinned buffer)
    (xwidget-browser-set-viewport buffer
                                  xwidget-browser-viewport-width
                                  xwidget-browser-viewport-height)
    (when url (xwidget-webkit-goto-uri (xwidget-browser--xwidget buffer) url))
    buffer))

(defun xwidget-browser-attach (spec)
  "Pin the session designated by SPEC and return its buffer."
  (let ((buffer (xwidget-browser-resolve spec)))
    (when buffer
      (setq xwidget-browser--attached buffer
            xwidget-browser--pinned buffer))
    buffer))

(defun xwidget-browser-set-current (buffer)
  "Remember BUFFER as the session last acted on."
  (when (buffer-live-p buffer) (setq xwidget-browser--attached buffer))
  buffer)

(defun xwidget-browser--user-navigation (&rest _)
  "Release the pin when the user drives `xwidget-webkit' themselves.
After this the tool follows whatever session the user is working in."
  (unless xwidget-browser--internal (setq xwidget-browser--pinned nil)))

(advice-add 'xwidget-webkit-new-session :before #'xwidget-browser--user-navigation)
(advice-add 'xwidget-webkit-goto-url :before #'xwidget-browser--user-navigation)
(advice-add 'xwidget-webkit-browse-url :before #'xwidget-browser--user-navigation)

(defun xwidget-browser-show (buffer)
  "Display session BUFFER in a window."
  (when (buffer-live-p buffer)
    (let ((window (display-buffer buffer)))
      (when window
        (with-selected-window window
          (xwidget-webkit-adjust-size-to-window (xwidget-browser--xwidget buffer) window)))
      window)))

(defun xwidget-browser-hide (buffer)
  "Remove session BUFFER from any window showing it."
  (dolist (window (get-buffer-window-list buffer nil t))
    (when (window-live-p window)
      (quit-window nil window))))

(defun xwidget-browser-close (buffer)
  "Kill session BUFFER."
  (when (buffer-live-p buffer)
    (when (eq buffer xwidget-browser--attached) (setq xwidget-browser--attached nil))
    (when (eq buffer xwidget-browser--pinned) (setq xwidget-browser--pinned nil))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))
    t))

(defun xwidget-browser-cleanup-owned ()
  "Kill every session this package created.  Return how many were killed."
  (let ((n 0))
    (dolist (buffer (xwidget-browser-sessions))
      (when (eq (buffer-local-value 'xwidget-browser--owner buffer) 'agent)
        (when (xwidget-browser-close buffer) (cl-incf n))))
    n))

(defun xwidget-browser-set-viewport (buffer width height)
  "Resize the xwidget in BUFFER so the page viewport is WIDTH x HEIGHT CSS px.
Emacs sizes xwidgets in device pixels, so scale factors (HiDPI) are
compensated by `xwidget-browser--scale'."
  (let ((xw (xwidget-browser--xwidget buffer))
        (scale (xwidget-browser--scale)))
    (when xw
      (xwidget-resize xw (round (* width scale)) (round (* height scale))))))

(defvar xwidget-browser--scale-cache nil
  "Measured ratio of xwidget device pixels to page CSS pixels.")

(defun xwidget-browser--scale ()
  "Return the device-pixel scale factor between Emacs and page CSS pixels."
  (or xwidget-browser--scale-cache
      (or (alist-get 'scale-factor (frame-monitor-attributes)) 1.0)))

(defun xwidget-browser-note-scale (requested actual)
  "Learn the pixel scale from a REQUESTED device size and the ACTUAL CSS size."
  (when (and (numberp requested) (numberp actual) (> actual 0) (> requested 0))
    (let ((scale (/ (float requested) actual)))
      (when (and (> scale 0.2) (< scale 5.0))
        (setq xwidget-browser--scale-cache scale)))))

(defun xwidget-browser--widget-size (buffer)
  "Return (WIDTH . HEIGHT), the device size of BUFFER's xwidget, or nil."
  (let ((xw (xwidget-browser--xwidget buffer)))
    (when xw
      (let ((info (xwidget-info xw)))
        (cons (aref info 2) (aref info 3))))))

;;;; JavaScript bridge -----------------------------------------------------

(defun xwidget-browser--js-literal (value)
  "Return VALUE as a JavaScript literal."
  (cond
   ((null value) "null")
   ((eq value t) "true")
   ((eq value :false) "false")
   ((eq value :null) "null")
   ((numberp value) (number-to-string value))
   ((stringp value) (json-serialize value))
   ((keywordp value) (json-serialize (substring (symbol-name value) 1)))
   ((symbolp value) (json-serialize (symbol-name value)))
   ((and (consp value) (keywordp (car value)))
    (json-serialize (xwidget-browser--plist-to-table value)))
   ((listp value) (json-serialize (apply #'vector (mapcar #'identity value))))
   (t (json-serialize value))))

(defun xwidget-browser--plist-to-table (plist)
  "Convert PLIST into a hash table suitable for `json-serialize'."
  (let ((table (make-hash-table :test 'equal)))
    (while plist
      (let ((key (car plist)) (value (cadr plist)))
        (puthash (if (keywordp key) (substring (symbol-name key) 1) (format "%s" key))
                 (cond ((null value) :null)
                       ((and (consp value) (keywordp (car value)))
                        (xwidget-browser--plist-to-table value))
                       ((listp value) (apply #'vector value))
                       (t value))
                 table))
      (setq plist (cddr plist)))
    table))

(defun xwidget-browser--template (template &rest bindings)
  "Expand $NAME placeholders in TEMPLATE using BINDINGS, a plist of Lisp values.
Values are inserted as JavaScript literals."
  (replace-regexp-in-string
   "\\$\\([A-Z][A-Z0-9_]*\\)"
   (lambda (match)
     (let* ((name (intern (concat ":" (substring match 1))))
            (value (plist-get bindings name)))
       (if (plist-member bindings name)
           (xwidget-browser--js-literal value)
         match)))
   template t t))

(defconst xwidget-browser--js-pack "
function __xwbPack(o, lim) {
  var s;
  try { s = JSON.stringify(o); }
  catch (e) {
    try { s = JSON.stringify({state: 'ok', value: String(o && o.value),
                              note: 'result was not JSON-serialisable'}); }
    catch (e2) { s = JSON.stringify({state: 'error', error: 'unserialisable result: ' + String(e)}); }
  }
  if (typeof s !== 'string') s = JSON.stringify({state: 'ok', value: null});
  if (s.length > lim) {
    window.__xwb_n = (window.__xwb_n || 0) + 1;
    var id = '__xwb_buf_' + window.__xwb_n;
    window[id] = s;
    return JSON.stringify({state: 'chunked', id: id, len: s.length});
  }
  return s;
}
function __xwbErr(e) {
  var message;
  try {
    message = e && e.message !== undefined
      ? (e.name ? e.name + ': ' : '') + e.message
      : String(e);
  } catch (x) { message = 'error'; }
  var stack = '';
  try { if (e && e.stack) stack = String.fromCharCode(10) + String(e.stack).slice(0, 400); }
  catch (x) {}
  return {state: 'error', error: message + stack};
}
"
  "JavaScript helpers embedded in every bridge script.")

(defun xwidget-browser--wrap (code)
  "Return the envelope script evaluating CODE inside the page."
  (xwidget-browser--template
   (concat "(function(){" xwidget-browser--js-pack "
var __code = $CODE, __lim = $LIMIT, __f = null;
try { __f = new Function('return (' + __code + '\\n);'); } catch (e) { __f = null; }
if (!__f) { try { __f = new Function(__code); } catch (e) { return __xwbPack(__xwbErr(e), __lim); } }
var __v;
try { __v = __f(); } catch (e) { return __xwbPack(__xwbErr(e), __lim); }
if (__v && typeof __v.then === 'function') {
  window.__xwb_n = (window.__xwb_n || 0) + 1;
  var __id = '__xwb_res_' + window.__xwb_n;
  window[__id] = {state: 'pending'};
  __v.then(function (r) { window[__id] = {state: 'ok', value: (r === undefined ? null : r)}; },
           function (e) { window[__id] = __xwbErr(e); });
  return JSON.stringify({state: 'promise', id: __id});
}
return __xwbPack({state: 'ok', value: (__v === undefined ? null : __v)}, __lim);
})()")
   :CODE code :LIMIT xwidget-browser--inline-limit))

(defun xwidget-browser--poll-script (id)
  "Return the script fetching the settled promise stored under ID."
  (xwidget-browser--template
   (concat "(function(){" xwidget-browser--js-pack "
var r = window[$ID];
if (!r) return JSON.stringify({state: 'error', error: 'result slot lost (did the page navigate?)'});
if (r.state === 'pending') return JSON.stringify({state: 'pending'});
try { delete window[$ID]; } catch (e) { window[$ID] = null; }
return __xwbPack(r, $LIMIT);
})()")
   :ID id :LIMIT xwidget-browser--inline-limit))

(defun xwidget-browser--chunk-script (id offset)
  "Return the script reading a chunk of the payload ID starting at OFFSET."
  (xwidget-browser--template
   "(function(){
var s = window[$ID];
if (typeof s !== 'string') return '0|';
var off = $OFF, end = Math.min(s.length, off + $SIZE);
if (end < s.length) { var c = s.charCodeAt(end - 1); if (c >= 0xD800 && c <= 0xDBFF) end--; }
var out = s.slice(off, end);
if (end >= s.length) { try { delete window[$ID]; } catch (e) { window[$ID] = null; } }
return String(end) + '|' + out;
})()"
   :ID id :OFF offset :SIZE xwidget-browser--chunk-size))

(defun xwidget-browser--exec (xw script timeout cb)
  "Run SCRIPT in XW, calling CB with the raw value or an error after TIMEOUT."
  (if (not (and xw (xwidget-live-p xw)))
      (funcall cb (list :ok nil :error "browser session is not live"))
    (let (done timer)
      (setq timer (run-at-time
                   (max 0.1 timeout) nil
                   (lambda ()
                     (unless done
                       (setq done t)
                       (funcall cb (list :ok nil :error
                                         (format "JavaScript did not return within %ss"
                                                 timeout)))))))
      (condition-case err
          (xwidget-webkit-execute-script
           xw script
           (lambda (value)
             (unless done
               (setq done t)
               (cancel-timer timer)
               (funcall cb (list :ok t :value value)))))
        (error
         (unless done
           (setq done t)
           (cancel-timer timer)
           (funcall cb (list :ok nil :error (error-message-string err)))))))))

(defun xwidget-browser-js (buffer code cb &optional timeout)
  "Evaluate CODE in session BUFFER and call CB with a result plist.
CODE may be an expression or a statement body using `return', and may
evaluate to a promise, which is awaited.  CB receives
`(:ok t :value VALUE)' or `(:ok nil :error MESSAGE)'."
  (let ((xw (xwidget-browser-watch buffer))
        (timeout (or timeout xwidget-browser-js-timeout)))
    (if (not xw)
        (funcall cb (list :ok nil :error "browser session is not live"))
      (let ((deadline (+ (float-time) timeout)))
        (xwidget-browser--exec
         xw (xwidget-browser--wrap code) timeout
         (lambda (result)
           (if (not (plist-get result :ok))
               (funcall cb result)
             (xwidget-browser--handle-raw xw (plist-get result :value) deadline cb))))))))

(defun xwidget-browser--handle-raw (xw raw deadline cb)
  "Decode RAW envelope text from XW and continue, calling CB when settled."
  (cond
   ((not (stringp raw))
    (funcall cb (list :ok nil :error
                      (format "unexpected result from page: %S" raw))))
   (t
    (let ((envelope (condition-case err
                        (json-parse-string raw :object-type 'plist :array-type 'list
                                           :null-object nil :false-object :false)
                      (error (list :state "error"
                                   :error (format "could not decode page result: %s"
                                                  (error-message-string err)))))))
      (xwidget-browser--handle-envelope xw envelope deadline cb)))))

(defun xwidget-browser--handle-envelope (xw envelope deadline cb)
  "Act on ENVELOPE from XW, polling or fetching chunks until CB can be called."
  (pcase (plist-get envelope :state)
    ("ok" (funcall cb (list :ok t :value (plist-get envelope :value)
                            :note (plist-get envelope :note))))
    ("error" (funcall cb (list :ok nil :error (or (plist-get envelope :error)
                                                  "JavaScript error"))))
    ("promise"
     (xwidget-browser--poll-promise xw (plist-get envelope :id) deadline cb))
    ("chunked"
     (xwidget-browser--fetch-chunks xw (plist-get envelope :id)
                                    (plist-get envelope :len) 0 nil deadline cb))
    ("pending"
     (funcall cb (list :ok nil :error "result is still pending")))
    (_ (funcall cb (list :ok nil :error (format "unknown result envelope: %S" envelope))))))

(defun xwidget-browser--poll-promise (xw id deadline cb)
  "Poll the promise slot ID in XW until DEADLINE, then call CB."
  (if (> (float-time) deadline)
      (funcall cb (list :ok nil :error "timed out waiting for the page to settle"))
    (run-at-time
     xwidget-browser-poll-interval nil
     (lambda ()
       (xwidget-browser--exec
        xw (xwidget-browser--poll-script id) (max 1 (- deadline (float-time)))
        (lambda (result)
          (if (not (plist-get result :ok))
              (funcall cb result)
            (let ((raw (plist-get result :value)))
              (if (and (stringp raw) (string-prefix-p "{\"state\":\"pending\"" raw))
                  (xwidget-browser--poll-promise xw id deadline cb)
                (xwidget-browser--handle-raw xw raw deadline cb))))))))))

(defun xwidget-browser--fetch-chunks (xw id len offset acc deadline cb)
  "Collect the chunked payload ID of LEN units from XW starting at OFFSET.
ACC holds the chunks collected so far, newest first."
  (cond
   ((> (float-time) deadline)
    (funcall cb (list :ok nil :error "timed out transferring the result")))
   ((>= offset len)
    (xwidget-browser--handle-raw xw (apply #'concat (nreverse acc))
                                 (+ (float-time) 5) cb))
   (t
    (xwidget-browser--exec
     xw (xwidget-browser--chunk-script id offset) (max 1 (- deadline (float-time)))
     (lambda (result)
       (if (not (plist-get result :ok))
           (funcall cb result)
         (let* ((raw (or (plist-get result :value) ""))
                (sep (string-search "|" raw))
                (next (and sep (string-to-number (substring raw 0 sep))))
                (data (and sep (substring raw (1+ sep)))))
           (cond
            ((or (null sep) (null next) (<= next offset))
             (funcall cb (list :ok nil :error "the page stopped streaming the result")))
            (t (xwidget-browser--fetch-chunks xw id len next (cons data acc)
                                              deadline cb))))))))))

;;;; Page state and navigation ---------------------------------------------

(defconst xwidget-browser--js-state "
return {
  url: location.href, title: document.title, readyState: document.readyState,
  token: (window.__xwb_token || null), width: window.innerWidth, height: window.innerHeight,
  scrollY: Math.round(window.scrollY),
  scrollHeight: (document.documentElement ? document.documentElement.scrollHeight : 0)
};"
  "Script returning a snapshot of the page state.")

(defun xwidget-browser-state (buffer cb)
  "Call CB with a plist describing the current page of session BUFFER."
  (xwidget-browser-js buffer xwidget-browser--js-state cb 8))

(defun xwidget-browser-goto (buffer url cb &optional timeout)
  "Navigate session BUFFER to URL, calling CB when the load settles."
  (let ((xwidget-browser--internal t)
        (xw (xwidget-browser--xwidget buffer))
        (token (format "t%d" (random 100000000)))
        (timeout (or timeout xwidget-browser-load-timeout)))
    (if (not xw)
        (funcall cb (list :ok nil :error "browser session is not live"))
      (xwidget-browser--exec
       xw (xwidget-browser--template "window.__xwb_token = $TOK; 'ok'" :TOK token) 5
       (lambda (_)
         (condition-case err
             (progn
               (xwidget-webkit-goto-uri xw url)
               (xwidget-browser--await-load buffer token (+ (float-time) timeout) cb))
           (error (funcall cb (list :ok nil :error (error-message-string err))))))))))

(defun xwidget-browser--await-load (buffer token deadline cb)
  "Wait until session BUFFER shows a document different from TOKEN.
Give up at DEADLINE and report the page state anyway."
  (xwidget-browser-state
   buffer
   (lambda (result)
     (let* ((state (plist-get result :value))
            (fresh (and state (not (equal (plist-get state :token) token))))
            (complete (equal (plist-get state :readyState) "complete")))
       (cond
        ((and state fresh complete)
         (xwidget-browser--after-load buffer state cb))
        ((> (float-time) deadline)
         (xwidget-browser--after-load
          buffer (or state '())
          (lambda (r) (funcall cb (append r (list :timed-out t))))))
        (t (run-at-time xwidget-browser-poll-interval nil
                        (lambda () (xwidget-browser--await-load buffer token deadline cb)))))))))

(defun xwidget-browser--after-load (buffer state cb)
  "Calibrate, install per-document hooks in BUFFER, then call CB with STATE."
  (let* ((size (xwidget-browser--widget-size buffer))
         (css-width (plist-get state :width))
         (retarget (xwidget-browser--recalibrate buffer size css-width)))
    (xwidget-browser-install-hooks
     buffer
     (lambda (_)
       (if (not retarget)
           (funcall cb (list :ok t :value state))
         ;; The viewport was off target (HiDPI scaling): resize and re-read it
         ;; once layout has settled.
         (run-at-time 0.2 nil
                      (lambda ()
                        (xwidget-browser-state
                         buffer
                         (lambda (r)
                           (funcall cb (list :ok t :value (or (plist-get r :value) state))))))))))))

(defun xwidget-browser--recalibrate (buffer size css-width)
  "Learn the pixel scale of BUFFER from SIZE and CSS-WIDTH.
Resize head-less sessions that drifted from the configured viewport.
Return non-nil when a resize was issued."
  (when (and size (numberp css-width) (> css-width 0))
    (xwidget-browser-note-scale (car size) css-width)
    (when (and (eq (buffer-local-value 'xwidget-browser--owner buffer) 'agent)
               (not (get-buffer-window buffer t))
               (> (abs (- css-width xwidget-browser-viewport-width)) 2))
      (xwidget-browser-set-viewport buffer
                                    xwidget-browser-viewport-width
                                    xwidget-browser-viewport-height)
      t)))

(defconst xwidget-browser--js-hooks "
if (window.__xwb_hooked) return 'already';
window.__xwb_hooked = true;
window.__xwb_logs = [];
var push = function (level, args) {
  try {
    var text = Array.prototype.map.call(args, function (a) {
      if (typeof a === 'string') return a;
      try { return JSON.stringify(a); } catch (e) { return String(a); }
    }).join(' ');
    window.__xwb_logs.push({time: Date.now(), level: level, text: text});
    if (window.__xwb_logs.length > 300) window.__xwb_logs.shift();
  } catch (e) {}
};
['log', 'info', 'warn', 'error', 'debug'].forEach(function (level) {
  var original = console[level];
  console[level] = function () { push(level, arguments); return original.apply(console, arguments); };
});
window.addEventListener('error', function (e) {
  push('error', [(e.message || 'error') + ' @ ' + (e.filename || '') + ':' + (e.lineno || 0)]);
});
window.addEventListener('unhandledrejection', function (e) {
  push('error', ['unhandled rejection: ' + String(e.reason)]);
});
return 'ok';"
  "Script installing console and error capture in the current document.")

(defun xwidget-browser-install-hooks (buffer cb)
  "Install console capture in the current document of BUFFER, then call CB."
  (if (not xwidget-browser-console-capture)
      (funcall cb (list :ok t :value "disabled"))
    (xwidget-browser-js buffer xwidget-browser--js-hooks cb 8)))

(defun xwidget-browser-watch (buffer)
  "Arrange for console capture to be installed early in BUFFER's documents.
The widget's callback is wrapped (once) so that the hook is injected as
soon as a new document is committed, which is before most page scripts
run.  The original callback keeps working."
  (let ((xw (xwidget-browser--xwidget buffer)))
    (when (and xw (not (xwidget-get xw 'xwidget-browser-wrapped)))
      (let ((original (xwidget-get xw 'callback)))
        (xwidget-put xw 'xwidget-browser-wrapped t)
        (xwidget-put
         xw 'callback
         (lambda (widget event-type)
           (when (functionp original)
             (condition-case err (funcall original widget event-type)
               (error (xwidget-log "xwidget-browser: callback error: %s"
                                   (error-message-string err)))))
           (xwidget-browser--on-widget-event widget event-type)))))
    xw))

(defun xwidget-browser--on-widget-event (xw event-type)
  "Inject the console hook into XW when EVENT-TYPE signals a new document."
  (when (and xwidget-browser-console-capture
             (eq event-type 'load-changed)
             (member (and (consp last-input-event) (nth 3 last-input-event))
                     '("load-committed" "load-finished")))
    (ignore-errors
      (xwidget-webkit-execute-script
       xw (concat "(function(){" xwidget-browser--js-hooks "})()")))))

(defun xwidget-browser-console (buffer limit cb)
  "Call CB with the last LIMIT console entries captured in BUFFER."
  (xwidget-browser-js
   buffer
   (xwidget-browser--template
    "var l = window.__xwb_logs; if (!l) return null; return l.slice(-$LIMIT);"
    :LIMIT (or limit 50))
   cb 8))

(defun xwidget-browser-wait (buffer spec timeout cb)
  "Wait in BUFFER until SPEC holds, or TIMEOUT seconds elapse, then call CB.
SPEC is a plist with `:selector', `:js' (a JavaScript expression) or
`:load' (wait for `readyState' to be complete)."
  (let* ((deadline (+ (float-time) (or timeout 15)))
         (condition
          (cond
           ((plist-get spec :selector)
            (xwidget-browser--template
             "return !!document.querySelector($SEL);" :SEL (plist-get spec :selector)))
           ((plist-get spec :js)
            (format "return !!(%s);" (plist-get spec :js)))
           (t "return document.readyState === 'complete';"))))
    (cl-labels
        ((tick ()
           (xwidget-browser-js
            buffer condition
            (lambda (result)
              (cond
               ((and (plist-get result :ok) (eq (plist-get result :value) t))
                (funcall cb (list :ok t :value t)))
               ((> (float-time) deadline)
                (funcall cb (list :ok nil :error
                                  (format "condition not met within %ss%s"
                                          timeout
                                          (if (plist-get result :error)
                                              (format " (last error: %s)"
                                                      (plist-get result :error))
                                            "")))))
               (t (run-at-time xwidget-browser-poll-interval nil #'tick))))
            (max 2 (round (- deadline (float-time)))))))
      (tick))))

;;;; Screenshots -----------------------------------------------------------

(defun xwidget-browser--download (url file)
  "Download URL into FILE.  Signal an error on failure."
  (make-directory (file-name-directory file) t)
  (let ((tmp (concat file ".part")))
    (if (executable-find "curl")
        (let ((status (call-process "curl" nil nil nil "-fsSL" "--max-time" "60"
                                    "-o" tmp url)))
          (unless (eq status 0)
            (error "Downloading %s failed (curl exit %s)" url status)))
      (url-copy-file url tmp t))
    (rename-file tmp file t)
    file))

(defun xwidget-browser--html2canvas-source ()
  "Return the html2canvas source text, downloading and caching it if needed."
  (let ((file (or xwidget-browser-html2canvas-file
                  (expand-file-name "html2canvas.min.js"
                                    xwidget-browser-cache-directory))))
    (unless (and (file-readable-p file) (> (file-attribute-size (file-attributes file)) 10000))
      (when xwidget-browser-html2canvas-file
        (error "html2canvas file not found: %s" file))
      (message "xwidget-browser: downloading html2canvas...")
      (xwidget-browser--download xwidget-browser-html2canvas-url file))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun xwidget-browser--ensure-html2canvas (buffer cb)
  "Make sure html2canvas is loaded in BUFFER's document, then call CB."
  (xwidget-browser-js
   buffer "return typeof html2canvas === 'function';"
   (lambda (result)
     (cond
      ((eq (plist-get result :value) t) (funcall cb (list :ok t :value t)))
      ((not (plist-get result :ok)) (funcall cb result))
      (t
       (condition-case err
           (let ((source (xwidget-browser--html2canvas-source))
                 (xw (xwidget-browser--xwidget buffer)))
             (xwidget-browser--exec
              xw source 30
              (lambda (_)
                (xwidget-browser-js
                 buffer "return typeof html2canvas === 'function';"
                 (lambda (check)
                   (if (eq (plist-get check :value) t)
                       (funcall cb (list :ok t :value t))
                     (funcall cb (list :ok nil :error
                                       "html2canvas did not load in the page"))))
                 8))))
         (error (funcall cb (list :ok nil :error (error-message-string err))))))))
   8))

(defconst xwidget-browser--js-capture "
var o = $OPTS;
var el = o.selector ? document.querySelector(o.selector) : document.documentElement;
if (!el) throw new Error('no element matches ' + o.selector);
var opts = {logging: false, useCORS: true, scale: 1, backgroundColor: o.background};
if (!o.selector && !o.fullPage) {
  opts.x = window.scrollX; opts.y = window.scrollY;
  opts.width = window.innerWidth; opts.height = window.innerHeight;
  opts.windowWidth = window.innerWidth; opts.windowHeight = window.innerHeight;
}
return html2canvas(el, opts).then(function (canvas) {
  var out = canvas, scale = 1;
  if (o.maxWidth && canvas.width > o.maxWidth) scale = o.maxWidth / canvas.width;
  if (o.maxHeight && canvas.height * scale > o.maxHeight) scale = o.maxHeight / canvas.height;
  if (scale < 1) {
    var small = document.createElement('canvas');
    small.width = Math.max(1, Math.round(canvas.width * scale));
    small.height = Math.max(1, Math.round(canvas.height * scale));
    var ctx = small.getContext('2d');
    ctx.fillStyle = o.background || '#ffffff';
    ctx.fillRect(0, 0, small.width, small.height);
    ctx.drawImage(canvas, 0, 0, small.width, small.height);
    out = small;
  }
  return {width: out.width, height: out.height, sourceWidth: canvas.width,
          sourceHeight: canvas.height, url: location.href, title: document.title,
          data: (o.format === 'jpeg' ? out.toDataURL('image/jpeg', o.quality)
                                     : out.toDataURL('image/png'))};
});"
  "Script rendering the page (or an element) to a data URL via html2canvas.")

(defun xwidget-browser-screenshot (buffer opts cb)
  "Capture a screenshot of session BUFFER and call CB with the result.

OPTS is a plist accepting `:selector', `:full-page', `:max-width',
`:max-height', `:format' (\"png\" or \"jpeg\"), `:quality' and `:file'.
On success CB receives `(:ok t :value (:data BASE64 :mime ... :width ...))'."
  (xwidget-browser--ensure-html2canvas
   buffer
   (lambda (ready)
     (if (not (plist-get ready :ok))
         (funcall cb ready)
       (let* ((format (or (plist-get opts :format) "png"))
              (js-opts (list :selector (plist-get opts :selector)
                             :fullPage (if (plist-get opts :full-page) t :false)
                             :maxWidth (or (plist-get opts :max-width) 1400)
                             :maxHeight (or (plist-get opts :max-height) 4000)
                             :format format
                             :quality (or (plist-get opts :quality) 0.85)
                             :background (or (plist-get opts :background) "#ffffff"))))
         (xwidget-browser-js
          buffer
          (xwidget-browser--template xwidget-browser--js-capture :OPTS js-opts)
          (lambda (result)
            (if (not (plist-get result :ok))
                (funcall cb result)
              (funcall cb (xwidget-browser--decode-shot (plist-get result :value)
                                                        (plist-get opts :file)))))
          (max 30 xwidget-browser-js-timeout)))))))

(defun xwidget-browser--decode-shot (shot file)
  "Turn SHOT (the JS capture result) into a result plist, saving to FILE if given."
  (let* ((data-url (and shot (plist-get shot :data)))
         (comma (and (stringp data-url) (string-search "," data-url))))
    (if (not comma)
        (list :ok nil :error "the page returned no image data")
      (let* ((mime (if (string-search "image/jpeg" (substring data-url 0 comma))
                       "image/jpeg" "image/png"))
             (base64 (substring data-url (1+ comma)))
             (path (when file
                     (let ((path (expand-file-name file)))
                       (make-directory (file-name-directory path) t)
                       (with-temp-file path
                         (set-buffer-multibyte nil)
                         (insert (base64-decode-string base64)))
                       path))))
        (list :ok t
              :value (list :data base64 :mime mime :file path
                           :width (plist-get shot :width)
                           :height (plist-get shot :height)
                           :source-width (plist-get shot :sourceWidth)
                           :source-height (plist-get shot :sourceHeight)
                           :url (plist-get shot :url)
                           :title (plist-get shot :title)))))))

;;;; Page inspection and interaction ---------------------------------------

(defconst xwidget-browser--js-dom "
function __xwbVisible(el) {
  var r = el.getBoundingClientRect();
  if (!r.width && !r.height) return false;
  var s = window.getComputedStyle(el);
  return s.visibility !== 'hidden' && s.display !== 'none' && s.opacity !== '0';
}
function __xwbText(el) {
  return ((el.innerText || el.textContent || el.value || '') + '').replace(/\\s+/g, ' ').trim();
}
function __xwbDescribe(el, index) {
  var r = el.getBoundingClientRect();
  var d = {index: index, tag: el.tagName.toLowerCase(), text: __xwbText(el).slice(0, 120),
           visible: __xwbVisible(el),
           rect: {x: Math.round(r.x), y: Math.round(r.y),
                  w: Math.round(r.width), h: Math.round(r.height)}};
  ['id', 'name', 'type', 'href', 'value', 'placeholder', 'title'].forEach(function (a) {
    var v = el[a] !== undefined && el[a] !== null && el[a] !== '' ? el[a] : el.getAttribute && el.getAttribute(a);
    if (v) d[a] = String(v).slice(0, 200);
  });
  if (el.className && typeof el.className === 'string' && el.className.trim())
    d['class'] = el.className.trim().slice(0, 120);
  var label = el.getAttribute && (el.getAttribute('aria-label') || el.getAttribute('alt'));
  if (label) d.label = String(label).slice(0, 120);
  if (el.disabled) d.disabled = true;
  if (el.checked !== undefined && el.type && (el.type === 'checkbox' || el.type === 'radio'))
    d.checked = !!el.checked;
  return d;
}
function __xwbFind(selector, text, index) {
  if (selector) {
    var list = document.querySelectorAll(selector);
    if (text) {
      var want = String(text).toLowerCase();
      for (var j = 0; j < list.length; j++)
        if (__xwbText(list[j]).toLowerCase().indexOf(want) >= 0) return list[j];
      return null;
    }
    return list[index || 0] || null;
  }
  if (!text) return null;
  var want = String(text).trim().toLowerCase();
  var candidates = document.querySelectorAll(
    'a, button, input[type=submit], input[type=button], input[type=reset], [role=button],' +
    '[role=link], [role=tab], [role=menuitem], summary, label, option, li, td, th, h1, h2, h3, span, div');
  var exact = [], partial = [];
  for (var i = 0; i < candidates.length; i++) {
    var el = candidates[i], t = __xwbText(el).toLowerCase();
    if (!t || !__xwbVisible(el)) continue;
    if (t === want) exact.push(el);
    else if (t.indexOf(want) >= 0) partial.push(el);
  }
  var pool = exact.length ? exact : partial;
  pool.sort(function (a, b) { return __xwbText(a).length - __xwbText(b).length; });
  return pool[index || 0] || null;
}
"
  "DOM helper functions shared by the interaction scripts.")

(defun xwidget-browser--dom (body &rest bindings)
  "Build a DOM script from BODY (a template using $NAME) and BINDINGS."
  (concat xwidget-browser--js-dom (apply #'xwidget-browser--template body bindings)))

(defun xwidget-browser-text (buffer selector max-chars cb)
  "Call CB with the rendered text of SELECTOR (or the page) in BUFFER."
  (xwidget-browser-js
   buffer
   (xwidget-browser--dom "
var sel = $SEL, max = $MAX;
var el = sel ? document.querySelector(sel) : (document.body || document.documentElement);
if (!el) return {error: 'no element matches ' + sel};
var text = (el.innerText || el.textContent || '').replace(/[ \\t]+\\n/g, '\\n').replace(/\\n{3,}/g, '\\n\\n').trim();
return {url: location.href, title: document.title, length: text.length,
        truncated: text.length > max, text: text.slice(0, max)};"
                         :SEL selector :MAX (or max-chars 20000))
   cb))

(defun xwidget-browser-html (buffer selector max-chars cb)
  "Call CB with the HTML of SELECTOR (or the document) in BUFFER."
  (xwidget-browser-js
   buffer
   (xwidget-browser--dom "
var sel = $SEL, max = $MAX;
var el = sel ? document.querySelector(sel) : document.documentElement;
if (!el) return {error: 'no element matches ' + sel};
var html = el.outerHTML || '';
return {url: location.href, length: html.length, truncated: html.length > max,
        html: html.slice(0, max)};"
                         :SEL selector :MAX (or max-chars 20000))
   cb))

(defun xwidget-browser-links (buffer limit cb)
  "Call CB with up to LIMIT links found in BUFFER."
  (xwidget-browser-js
   buffer
   (xwidget-browser--dom "
var out = [], seen = {}, list = document.querySelectorAll('a[href]');
for (var i = 0; i < list.length && out.length < $LIMIT; i++) {
  var a = list[i], href = a.href;
  if (!href || href.indexOf('javascript:') === 0 || seen[href]) continue;
  seen[href] = true;
  out.push({text: __xwbText(a).slice(0, 100), href: href, visible: __xwbVisible(a)});
}
return {url: location.href, count: out.length, links: out};"
                         :LIMIT (or limit 100))
   cb))

(defun xwidget-browser-elements (buffer selector limit cb)
  "Call CB with a description of the elements matching SELECTOR in BUFFER."
  (xwidget-browser-js
   buffer
   (xwidget-browser--dom "
var sel = $SEL || 'a, button, input, select, textarea, [role=button], [contenteditable=true]';
var list = document.querySelectorAll(sel), out = [];
for (var i = 0; i < list.length && out.length < $LIMIT; i++) out.push(__xwbDescribe(list[i], i));
return {selector: sel, total: list.length, shown: out.length, elements: out};"
                         :SEL selector :LIMIT (or limit 50))
   cb))

(defun xwidget-browser-click (buffer selector text index cb)
  "Click the element matching SELECTOR/TEXT/INDEX in BUFFER, then call CB."
  (xwidget-browser-js
   buffer
   (xwidget-browser--dom "
var el = __xwbFind($SEL, $TEXT, $INDEX);
if (!el) return {error: 'no element found for selector=' + $SEL + ' text=' + $TEXT};
el.scrollIntoView({block: 'center', inline: 'center'});
var r = el.getBoundingClientRect();
var opts = {bubbles: true, cancelable: true, view: window,
            clientX: Math.round(r.left + r.width / 2), clientY: Math.round(r.top + r.height / 2)};
try { el.focus({preventScroll: true}); } catch (e) {}
['pointerdown', 'mousedown', 'pointerup', 'mouseup', 'click'].forEach(function (type) {
  var Ctor = type.indexOf('pointer') === 0 && window.PointerEvent ? PointerEvent : MouseEvent;
  el.dispatchEvent(new Ctor(type, opts));
});
return {clicked: __xwbDescribe(el, 0), url: location.href};"
                         :SEL selector :TEXT text :INDEX (or index 0))
   cb))

(defun xwidget-browser-fill (buffer selector text value submit cb)
  "Type VALUE into the field matching SELECTOR/TEXT in BUFFER, then call CB.
When SUBMIT is non-nil, submit the surrounding form afterwards."
  (xwidget-browser-js
   buffer
   (xwidget-browser--dom "
var el = __xwbFind($SEL, $TEXT, 0) ||
         (!$SEL && !$TEXT ? document.querySelector('input:not([type=hidden]), textarea') : null);
if (!el) return {error: 'no input found for selector=' + $SEL + ' text=' + $TEXT};
var value = $VALUE;
el.scrollIntoView({block: 'center'});
try { el.focus({preventScroll: true}); } catch (e) {}
if (el.isContentEditable) {
  el.textContent = value;
} else {
  var proto = el instanceof HTMLTextAreaElement ? HTMLTextAreaElement.prototype
            : el instanceof HTMLSelectElement ? HTMLSelectElement.prototype
            : HTMLInputElement.prototype;
  var desc = Object.getOwnPropertyDescriptor(proto, 'value');
  if (desc && desc.set) desc.set.call(el, value); else el.value = value;
}
el.dispatchEvent(new Event('input', {bubbles: true}));
el.dispatchEvent(new Event('change', {bubbles: true}));
if ($SUBMIT) {
  var form = el.form;
  var key = {bubbles: true, cancelable: true, key: 'Enter', code: 'Enter', keyCode: 13, which: 13};
  el.dispatchEvent(new KeyboardEvent('keydown', key));
  el.dispatchEvent(new KeyboardEvent('keyup', key));
  if (form) { if (form.requestSubmit) form.requestSubmit(); else form.submit(); }
}
return {filled: __xwbDescribe(el, 0), submitted: !!$SUBMIT};"
                         :SEL selector :TEXT text :VALUE (or value "")
                         :SUBMIT (if submit t :false))
   cb))

(defun xwidget-browser-select (buffer selector value cb)
  "Choose VALUE (value or label) in the select matching SELECTOR in BUFFER."
  (xwidget-browser-js
   buffer
   (xwidget-browser--dom "
var el = document.querySelector($SEL);
if (!el) return {error: 'no element matches ' + $SEL};
var want = String($VALUE), matched = null;
for (var i = 0; i < el.options.length; i++) {
  var o = el.options[i];
  if (o.value === want || __xwbText(o) === want) { matched = o; break; }
  if (!matched && __xwbText(o).toLowerCase().indexOf(want.toLowerCase()) >= 0) matched = o;
}
if (!matched) return {error: 'no option matching ' + want};
el.value = matched.value;
el.dispatchEvent(new Event('input', {bubbles: true}));
el.dispatchEvent(new Event('change', {bubbles: true}));
return {selected: {value: matched.value, text: __xwbText(matched)}};"
                         :SEL selector :VALUE value)
   cb))

(defun xwidget-browser-key (buffer key selector cb)
  "Send KEY to SELECTOR (or the focused element) in BUFFER, then call CB."
  (xwidget-browser-js
   buffer
   (xwidget-browser--dom "
var key = String($KEY);
var el = $SEL ? document.querySelector($SEL) : (document.activeElement || document.body);
if (!el) return {error: 'no target element'};
var codes = {Enter: 13, Tab: 9, Escape: 27, ArrowUp: 38, ArrowDown: 40,
             ArrowLeft: 37, ArrowRight: 39, Backspace: 8, Delete: 46, ' ': 32};
var code = codes[key] || (key.length === 1 ? key.toUpperCase().charCodeAt(0) : 0);
var init = {bubbles: true, cancelable: true, key: key, code: key.length === 1 ? 'Key' + key.toUpperCase() : key,
            keyCode: code, which: code};
['keydown', 'keypress', 'keyup'].forEach(function (type) {
  el.dispatchEvent(new KeyboardEvent(type, init));
});
if (key === 'Enter' && el.form) { if (el.form.requestSubmit) el.form.requestSubmit(); else el.form.submit(); }
return {key: key, target: __xwbDescribe(el, 0)};"
                         :KEY key :SEL selector)
   cb))

(defun xwidget-browser-scroll (buffer to by cb)
  "Scroll BUFFER to TO (\"top\", \"bottom\" or a selector) or BY pixels."
  (xwidget-browser-js
   buffer
   (xwidget-browser--dom "
var to = $TO, by = $BY;
if (to === 'top') window.scrollTo(0, 0);
else if (to === 'bottom') window.scrollTo(0, document.documentElement.scrollHeight);
else if (to) {
  var el = document.querySelector(to);
  if (!el) return {error: 'no element matches ' + to};
  el.scrollIntoView({block: 'center'});
} else window.scrollBy(0, by || window.innerHeight);
return {scrollY: Math.round(window.scrollY), innerHeight: window.innerHeight,
        scrollHeight: document.documentElement.scrollHeight};"
                         :TO to :BY by)
   cb))

(provide 'xwidget-browser-core)
;;; xwidget-browser-core.el ends here
