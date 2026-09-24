;;; pai-learn-render.el --- Render Mermaid/SVG diagrams to PNG inside Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Replaces the `visual-tools' extension of the learn system, which shelled
;; out to mermaid-cli (Node + a bundled Chrome) and rsvg-convert/ImageMagick.
;; Here everything happens inside Emacs: one head-less `xwidget-webkit'
;; session (driven through xwidget-browser-core) loads a local page with
;; mermaid.js, Mermaid source is turned into SVG by mermaid.js, and every SVG
;; is rasterised to PNG on a canvas by WebKit itself.  The only thing fetched
;; from outside is mermaid.min.js, downloaded once with `url-copy-file' into
;; `pai-learn-render-cache-directory'.
;;
;; Entry points:
;;   `pai-learn-render'        async: (KIND SOURCE CB) -> (:ok t :data B64 ...)
;;   `pai-learn-render-save'   write a rendered result to a PNG file
;;   `pai-learn-render-tool'   the `render_diagram' tool for the maker roles

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'pai-core)
(require 'pai-tools)
(require 'xwidget-browser-core nil t)

(declare-function xwidget-browser-create "xwidget-browser-core" (&optional url owner))
(declare-function xwidget-browser-goto "xwidget-browser-core" (buffer url cb &optional timeout))
(declare-function xwidget-browser-js "xwidget-browser-core" (buffer code cb &optional timeout))
(declare-function xwidget-browser-session-p "xwidget-browser-core" (buffer))
(declare-function xwidget-browser--template "xwidget-browser-core" (template &rest bindings))
(defvar xwidget-browser--attached)
(defvar xwidget-browser-private)
(defvar xwidget-browser--pinned)
(defvar xwidget-webkit-last-session-buffer)
(defvar pai-directory)

(defgroup pai-learn nil
  "The learning system: teaching, quizzes, diagrams and lesson logs."
  :group 'pai)

(defcustom pai-learn-render-cache-directory
  (expand-file-name "learn" (or (bound-and-true-p pai-directory) user-emacs-directory))
  "Directory holding mermaid.min.js and the render page."
  :type 'directory)

(defcustom pai-learn-mermaid-url
  "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.min.js"
  "Where mermaid.js is downloaded from the first time a diagram is rendered."
  :type 'string)

(defcustom pai-learn-mermaid-theme "default"
  "Mermaid theme used for diagrams (default, neutral, forest, dark, base)."
  :type 'string)

(defcustom pai-learn-render-scale 2
  "Device-pixel scale PNGs are rendered at."
  :type 'number)

(defcustom pai-learn-render-max-size 1800
  "Largest PNG side in pixels; bigger diagrams are scaled down to fit."
  :type 'integer)

(defcustom pai-learn-render-timeout 40
  "Seconds to wait for one render."
  :type 'number)

;;;; The render page

(defconst pai-learn-render--page
  "<!doctype html>
<html><head><meta charset=\"utf-8\">
<script src=\"mermaid.min.js\"></script>
<script>
if (window.mermaid) {
  mermaid.initialize({startOnLoad: false, securityLevel: 'strict', theme: %S,
                      htmlLabels: false, flowchart: {htmlLabels: false}});
}
</script></head>
<body style=\"margin:0;background:#fff\"></body></html>
"
  "Render page; `format'ted with the Mermaid theme.
HTML labels are off so Mermaid emits plain SVG text: SVG containing
foreignObject would taint the canvas and could not be exported.")

(defconst pai-learn-render--js "
var kind = $KIND, src = $SRC, scale = $SCALE, maxSize = $MAX;
function dim(v) { return (v && !/%$/.test(v)) ? parseFloat(v) : NaN; }
function raster(text) {
  return new Promise(function (resolve, reject) {
    var open = (text.match(/<svg\\b[^>]*>/) || [''])[0];
    if (open && !/\\sxmlns=/.test(open))
      text = text.replace(/<svg\\b/, '<svg xmlns=\"http://www.w3.org/2000/svg\"');
    if (open && /xlink:/.test(text) && !/xmlns:xlink=/.test(open))
      text = text.replace(/<svg\\b/, '<svg xmlns:xlink=\"http://www.w3.org/1999/xlink\"');
    var doc = new DOMParser().parseFromString(text, 'image/svg+xml');
    var bad = doc.getElementsByTagName('parsererror')[0];
    if (bad) { reject(new Error('SVG parse error: ' + bad.textContent.slice(0, 500))); return; }
    var svg = doc.documentElement;
    if (!svg || svg.localName !== 'svg') { reject(new Error('the root element is not <svg>')); return; }
    if (svg.getElementsByTagName('foreignObject').length)
      { reject(new Error('<foreignObject> is not supported; use <text> elements')); return; }
    var w = dim(svg.getAttribute('width')), h = dim(svg.getAttribute('height'));
    var vb = (svg.getAttribute('viewBox') || '').trim().split(/[\\s,]+/).map(parseFloat);
    if (!(w > 0 && h > 0) && vb.length === 4 && vb[2] > 0 && vb[3] > 0) { w = vb[2]; h = vb[3]; }
    if (!(w > 0 && h > 0)) { reject(new Error('the SVG needs width/height or a viewBox')); return; }
    svg.setAttribute('width', w); svg.setAttribute('height', h);
    svg.removeAttribute('style');
    var img = new Image();
    img.onload = function () {
      try {
        var s = Math.min(scale, maxSize / Math.max(w, h));
        var c = document.createElement('canvas');
        c.width = Math.round(w * s); c.height = Math.round(h * s);
        var g = c.getContext('2d');
        g.fillStyle = '#ffffff'; g.fillRect(0, 0, c.width, c.height);
        g.drawImage(img, 0, 0, c.width, c.height);
        resolve({width: c.width, height: c.height, png: c.toDataURL('image/png')});
      } catch (e) { reject(e); }
    };
    img.onerror = function () { reject(new Error('WebKit could not load the SVG as an image')); };
    img.src = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(new XMLSerializer().serializeToString(svg));
  });
}
if (kind === 'mermaid') {
  if (!window.mermaid) throw new Error('mermaid.js is not loaded');
  window.__lrn = (window.__lrn || 0) + 1;
  var id = 'lrn' + window.__lrn;
  return mermaid.render(id, src).then(function (r) { return raster(r.svg); }, function (e) {
    ['d' + id, id].forEach(function (x) { var n = document.getElementById(x); if (n) n.remove(); });
    throw new Error((e && (e.message || e.str)) || String(e));
  });
}
return raster(src);
"
  "Script rendering $SRC ($KIND mermaid or svg) to a PNG data URL.")

(defconst pai-learn-render--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory this file was loaded from.")

(defvar pai-learn-render--buffer nil "The head-less webkit session used for rendering.")
(defvar pai-learn-render--state nil "nil, `loading' or `ready'.")
(defvar pai-learn-render--waiting nil "Callbacks waiting for the page to load.")

(defun pai-learn-render--require-core ()
  "Load xwidget-browser-core, from the sibling extension if need be."
  (or (featurep 'xwidget-browser-core)
      (require 'xwidget-browser-core nil t)
      (let ((dir (expand-file-name "../xwidget-browser/" pai-learn-render--directory)))
        (and (file-directory-p dir)
             (let ((load-path (cons dir load-path)))
               (require 'xwidget-browser-core nil t))))))

(defun pai-learn-render-available-p ()
  "Return non-nil when this Emacs can render diagrams."
  (and (featurep 'xwidget-internal)
       (display-graphic-p)
       (pai-learn-render--require-core)
       (fboundp 'xwidget-browser-create)))

(defun pai-learn-render--ensure-assets ()
  "Make sure mermaid.min.js and the render page exist; return the page file."
  (let* ((dir (file-name-as-directory pai-learn-render-cache-directory))
         (js (expand-file-name "mermaid.min.js" dir))
         (page (expand-file-name "render.html" dir)))
    (make-directory dir t)
    (unless (and (file-readable-p js)
                 (> (file-attribute-size (file-attributes js)) 100000))
      (message "pai-learn: downloading mermaid.js (one time)...")
      (url-copy-file pai-learn-mermaid-url js t))
    (with-temp-file page
      (insert (format pai-learn-render--page pai-learn-mermaid-theme)))
    page))

(defun pai-learn-render--settle (result)
  "Deliver RESULT to every callback waiting for the render page."
  (let ((waiting (nreverse pai-learn-render--waiting)))
    (setq pai-learn-render--waiting nil
          pai-learn-render--state (and (plist-get result :ok) 'ready))
    (dolist (cb waiting) (funcall cb result))))

(defun pai-learn-render--session (cb)
  "Call CB with (:ok t) once the render page is loaded, or with an error."
  (cond
   ((not (pai-learn-render-available-p))
    (funcall cb (list :ok nil :error "diagram rendering needs a graphical Emacs with xwidget-webkit")))
   ((and (eq pai-learn-render--state 'ready)
         (xwidget-browser-session-p pai-learn-render--buffer))
    (funcall cb (list :ok t)))
   ((and (eq pai-learn-render--state 'loading)
         (buffer-live-p pai-learn-render--buffer))
    (push cb pai-learn-render--waiting))
   (t
    (push cb pai-learn-render--waiting)
    (setq pai-learn-render--state 'loading)
    (condition-case err
        (let ((page (pai-learn-render--ensure-assets))
              ;; Do not become the browser tool's "current" session.
              (attached xwidget-browser--attached)
              (pinned xwidget-browser--pinned)
              (last xwidget-webkit-last-session-buffer))
          (when (buffer-live-p pai-learn-render--buffer)
            (let ((kill-buffer-query-functions nil))
              (kill-buffer pai-learn-render--buffer)))
          (setq pai-learn-render--buffer (xwidget-browser-create nil 'pai-learn))
          (with-current-buffer pai-learn-render--buffer
            (setq-local xwidget-browser-private t)
            (rename-buffer " *pai-learn render*" t))
          (setq xwidget-browser--attached attached
                xwidget-browser--pinned pinned
                xwidget-webkit-last-session-buffer last)
          (xwidget-browser-goto
           pai-learn-render--buffer (concat "file://" page)
           (lambda (result)
             (if (not (plist-get result :ok))
                 (pai-learn-render--settle result)
               (xwidget-browser-js
                pai-learn-render--buffer "return !!window.mermaid;"
                (lambda (r)
                  (pai-learn-render--settle
                   (if (eq (plist-get r :value) t) (list :ok t)
                     (list :ok nil :error "mermaid.js failed to load in the render page")))))))))
      (error (pai-learn-render--settle
              (list :ok nil :error (error-message-string err))))))))

(defun pai-learn-render--clean-error (message)
  "Return MESSAGE without the JavaScript stack trace the bridge appends."
  (string-trim
   (replace-regexp-in-string
    "\\`Error: " ""
    (mapconcat #'identity
               (seq-remove (lambda (line)
                             (string-match-p "\\`\\(?:[[:alnum:]_$.]*@\\|global code@\\|Below is a rendering\\)" line))
                           (split-string message "\n"))
               "\n"))))

(defun pai-learn-render (kind source cb)
  "Render SOURCE, a KIND (\"mermaid\" or \"svg\") diagram, to PNG.
Call CB with (:ok t :data BASE64 :width W :height H) or (:ok nil :error MSG)."
  (if (not (member kind '("mermaid" "svg")))
      (funcall cb (list :ok nil :error (format "unknown diagram kind %S (mermaid or svg)" kind)))
    (pai-learn-render--session
     (lambda (ready)
       (if (not (plist-get ready :ok))
           (funcall cb ready)
         (xwidget-browser-js
          pai-learn-render--buffer
          (xwidget-browser--template pai-learn-render--js
                                     :KIND kind :SRC source
                                     :SCALE pai-learn-render-scale
                                     :MAX pai-learn-render-max-size)
          (lambda (result)
            (let* ((value (plist-get result :value))
                   (url (and (plist-get result :ok) (plist-get value :png))))
              (if (and (stringp url) (string-match "\\`data:image/png;base64," url))
                  (funcall cb (list :ok t :data (substring url (match-end 0))
                                    :width (plist-get value :width)
                                    :height (plist-get value :height)))
                (funcall cb (list :ok nil
                                  :error (pai-learn-render--clean-error
                                          (or (plist-get result :error)
                                              "the renderer returned no image")))))))
          pai-learn-render-timeout))))))

(defun pai-learn-render-save (data file)
  "Write base64 PNG DATA to FILE, creating its directory; return FILE."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert (base64-decode-string data))))
  file)

(defun pai-learn-render-slug (text)
  "Return TEXT as a short kebab-case file name component."
  (let ((slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase (or text "")))))
    (setq slug (string-trim slug "-+" "-+"))
    (if (string-empty-p slug) "diagram" (truncate-string-to-width slug 40))))

;;;; The render_diagram tool (maker roles only)

(defvar pai-learn-viz-directory-function nil
  "Function returning the directory published diagrams are written to.")

(defvar-local pai-learn-render--last nil
  "(KIND . SOURCE) of the last diagram rendered in this session.")

(defun pai-learn-render--tool-execute (args _ctx _on-update on-done)
  "Execute `render_diagram' with ARGS, finishing through ON-DONE."
  (let* ((kind (or (plist-get args :kind) (car pai-learn-render--last)))
         (source (or (let ((s (plist-get args :source)))
                       (and (stringp s) (not (string-empty-p (string-trim s))) s))
                     (and (equal kind (car pai-learn-render--last))
                          (cdr pai-learn-render--last))))
         (save-as (let ((s (plist-get args :save_as)))
                    (and (stringp s) (not (string-empty-p (string-trim s))) s)))
         (buffer (current-buffer)))
    (if (not (and kind source))
        (funcall on-done (pai-tool-error-result
                          "render_diagram needs kind and source (source may be omitted only to publish the last render)"))
      (pai-learn-render
       kind source
       (lambda (result)
         (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
           (if (not (plist-get result :ok))
               (funcall on-done (pai-tool-error-result
                                 (format "Render failed: %s\nFix the source and render again."
                                         (plist-get result :error))))
             (setq pai-learn-render--last (cons kind source))
             (let* ((size (format "%sx%s px" (plist-get result :width) (plist-get result :height)))
                    (file (when save-as
                            (pai-learn-render-save
                             (plist-get result :data)
                             (expand-file-name
                              (format "viz-%s-%s.png" (pai-learn-render-slug save-as)
                                      (format-time-string "%Y%m%d-%H%M%S"))
                              (if pai-learn-viz-directory-function
                                  (funcall pai-learn-viz-directory-function)
                                (expand-file-name "viz" default-directory)))))))
               (funcall on-done
                        (pai-tool-ok-result
                         (list (pai-text
                                (if file
                                    (format "Published (%s). Check this final image once more.\nfilename: %s\npath: %s"
                                            size (file-name-nondirectory file) file)
                                  (format "Preview of the %s diagram (%s), not saved. LOOK at it: is every element correct and legible? Fix and re-render, or publish with save_as."
                                          kind size)))
                               (pai-image (plist-get result :data) "image/png"))
                         (append (list :kind kind :width (plist-get result :width)
                                       :height (plist-get result :height))
                                 (when file (list :path file)))))))))))))

(defconst pai-learn-render-tool
  (list
   :name "render_diagram"
   :label "Render diagram"
   :description
   (concat
    "Render a Mermaid or hand-written SVG diagram to a PNG and return the image so "
    "you can look at it.  Without save_as it is a preview only; with save_as it is "
    "also published as viz-<save_as>-<timestamp>.png and the filename and absolute "
    "path are returned.  To publish exactly what you just previewed, call again with "
    "kind and save_as and omit source.  Mermaid is rendered with plain SVG labels "
    "(no HTML inside labels); SVG must have width/height or a viewBox and must not "
    "use <foreignObject>.")
   :prompt-snippet "render_diagram: render Mermaid/SVG source to a PNG you can see, then publish it"
   :deferred nil
   :execution-mode 'sequential
   :parameters
   (pai-object-schema
    (list :kind (pai-string-schema "\"mermaid\" or \"svg\"." :enum ["mermaid" "svg"])
          :source (pai-string-schema "The complete Mermaid source or the complete <svg>...</svg> document.")
          :save_as (pai-string-schema "Short kebab-case topic; when given the PNG is published."))
    '("kind"))
   :execute #'pai-learn-render--tool-execute)
  "The `render_diagram' tool definition.")

(provide 'pai-learn-render)
;;; pai-learn-render.el ends here
