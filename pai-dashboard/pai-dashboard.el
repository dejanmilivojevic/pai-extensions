;;; pai-dashboard.el --- A welcome dashboard at the top of new conversations -*- lexical-binding: t; -*-

;;; Commentary:

;; Inspired by the Spacemacs home buffer: a new pai conversation opens with a
;; logo, the active model and project, and what is at hand -- skills,
;; extensions and prompt snippets -- each a button.
;;
;;   - The logo is drawn with Emacs' built-in `svg.el' in the colours of the
;;     current theme (an `M-x' key set in a neural net, under an AI
;;     sparkle; and
;;     a gradient wordmark), so it matches any theme.  Terminals and Emacsen
;;     without SVG get a text logo instead.
;;   - Skills come from the session's skill directories; RET on one inserts
;;     `/skill:NAME ' in the prompt.
;;   - Extensions are listed from the extension directories, described by the
;;     summary line of their main file (the standard `;;; NAME --- SUMMARY'
;;     header, read with `lm-summary'); RET opens its directory in Dired.
;;   - Prompt snippets (when that extension is loaded) are listed; RET on one
;;     opens its file.
;;
;; The dashboard is inserted into the transcript when a conversation starts
;; (a new buffer, or /new) -- not when a session is resumed -- and scrolls
;; away like any other output.  It never reaches the model: it is display
;; only.  `/dashboard' shows it again; `pai-dashboard-enable' turns it off.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'svg)
(require 'dom)
(require 'lisp-mnt)
(require 'dired)
(require 'pai-core)
(require 'pai-ext)
(require 'pai-skills)

(defvar pai--output-marker)
(defvar pai--input-marker)
(defvar pai--model)
(defvar pai-directory)
(declare-function pai--insert "pai-ui" (text &optional face))
(declare-function pai--skill-dirs "pai-ui" ())
(declare-function pai-model-key "pai-models" (model))
(declare-function pai-prompt-snippets--load "pai-prompt-snippets" ())
(declare-function pai-prompt-snippet-name "pai-prompt-snippets" (snippet))
(declare-function pai-prompt-snippet-path "pai-prompt-snippets" (snippet))

(defgroup pai-dashboard nil
  "The welcome dashboard of new pai conversations."
  :group 'pai)

(defcustom pai-dashboard-enable t
  "Whether new conversations open with the dashboard."
  :type 'boolean :group 'pai-dashboard)

(defcustom pai-dashboard-max-items 20
  "Most items listed per section; the rest are summarized as a count."
  :type 'integer :group 'pai-dashboard)

(defcustom pai-dashboard-tagline "an agent that lives in emacs"
  "Line under the logo's wordmark."
  :type 'string :group 'pai-dashboard)

(defface pai-dashboard-heading '((t :inherit font-lock-keyword-face :weight bold))
  "Face of the dashboard's section headings.")

(defface pai-dashboard-name '((t :inherit font-lock-function-name-face))
  "Face of skill and extension names.")

(defface pai-dashboard-muted '((t :inherit shadow))
  "Face of descriptions and hints.")

;;;; Colours

(defun pai-dashboard--color (face attribute fallback)
  "Return FACE's ATTRIBUTE as a colour string, or FALLBACK."
  (let ((c (face-attribute face attribute nil t)))
    (if (and (stringp c) (not (string-prefix-p "unspecified" c))) c fallback)))

(defun pai-dashboard--palette ()
  "Return (BG FG ACCENT-1 ACCENT-2) from the current theme."
  (list (pai-dashboard--color 'default :background "#292b2e")
        (pai-dashboard--color 'default :foreground "#b2b2b2")
        (pai-dashboard--color 'font-lock-keyword-face :foreground "#4f97d7")
        (pai-dashboard--color 'font-lock-function-name-face :foreground "#bc6ec5")))

;;;; The logo

(defun pai-dashboard--gradient (svg id from to x2 y2)
  "Add to SVG a linear gradient ID from colour FROM to TO toward (X2, Y2).
`svg-gradient' only draws top-to-bottom; the direction is set on its node."
  (svg-gradient svg id 'linear `((0 . ,from) (100 . ,to)))
  (let ((node (car (dom-search svg (lambda (n) (equal (dom-attr n 'id) id))))))
    ;; `svg-gradient' builds every gradient from the same quoted attribute
    ;; list; editing it in place (`dom-set-attribute') would change them all.
    (setcar (cdr node) (copy-alist (dom-attributes node)))
    (dom-set-attribute node 'x2 x2)
    (dom-set-attribute node 'y2 y2)))

(defun pai-dashboard--sparkle (svg cx cy r fill outline)
  "Draw on SVG a four-pointed sparkle at (CX, CY) of radius R.
It is filled with FILL and outlined in OUTLINE, to stand off what it covers."
  (svg-node svg 'path
            :d (format "M%s %s Q%s %s %s %s Q%s %s %s %s Q%s %s %s %s Q%s %s %s %sZ"
                       cx (- cy r)  cx cy (+ cx r) cy  cx cy cx (+ cy r)
                       cx cy (- cx r) cy  cx cy cx (- cy r))
            :fill fill :stroke outline :stroke-width (min 3 (/ r 5.0)) :stroke-linejoin "round"))

(defun pai-dashboard--path (svg d stroke width &rest args)
  "Stroke on SVG the path D in STROKE, WIDTH wide, round-capped; ARGS add attributes."
  (apply #'svg-node svg 'path :d d :fill "none" :stroke stroke :stroke-width width
         :stroke-linecap "round" :stroke-linejoin "round" args))

(defun pai-dashboard--neuron (svg x y r fill outline &optional ring)
  "Draw on SVG a node at (X, Y) of radius R filled with FILL.
With RING, outline it in OUTLINE that wide (an active node)."
  (if ring
      (svg-circle svg x y r :fill fill :stroke outline :stroke-width ring)
    (svg-circle svg x y r :fill fill)))

(defconst pai-dashboard--mesh
  '((top (38 34) (50 28) (62 35) (75 28) (87 35) (98 29))
    (bottom (40 98) (54 94) (68 99) (82 94) (96 98))
    ;; synapses: a neuron of a row to a point by the letters
    (wires ((38 34) . (42 52)) ((62 35) . (58 52)) ((75 28) . (80 52)) ((98 29) . (95 52))
           ((54 94) . (48 82)) ((68 99) . (68 82)) ((82 94) . (88 82))))
  "The neurons around the key's `M-x': a row above, a row below, and the
wires from them to the letters.")

(defun pai-dashboard--chain (points)
  "Return an SVG path through POINTS, each (X Y)."
  (concat "M" (mapconcat (lambda (p) (format "%s %s" (car p) (cadr p))) points " L")))

(defun pai-dashboard-logo-svg ()
  "Return the logo as an `svg.el' object in the current theme's colours.
An `M-x' key set in a neural net: a row of neurons above the letters and
one below, wired to them; an AI sparkle sits on the key's corner."
  (pcase-let* ((`(,bg ,fg ,a1 ,a2) (pai-dashboard--palette))
               (font (or (face-attribute 'default :family nil t) "monospace"))
               (svg (svg-create 380 140))
               (top (alist-get 'top pai-dashboard--mesh))
               (bottom (alist-get 'bottom pai-dashboard--mesh)))
    (pai-dashboard--gradient svg "pai-tile" a1 a2 1 1)
    (pai-dashboard--gradient svg "pai-word" a1 a2 1 0)
    ;; the key: its side (a shadow in the second accent), then its top
    (svg-rectangle svg 22 30 92 86 :rx 22 :fill a2 :opacity 0.45)
    (svg-rectangle svg 22 20 92 88 :rx 22 :fill "url(#pai-tile)")
    (svg-rectangle svg 30 24 76 26 :rx 13 :fill "#ffffff" :opacity 0.08)
    ;; the net: rows of neurons, wired to the letters
    (pai-dashboard--path svg (pai-dashboard--chain top) bg 1.5 :opacity 0.35)
    (pai-dashboard--path svg (pai-dashboard--chain bottom) bg 1.5 :opacity 0.35)
    (dolist (w (alist-get 'wires pai-dashboard--mesh))
      (pai-dashboard--path svg (pai-dashboard--chain (list (car w) (cdr w))) bg 1.5
                           :opacity 0.35)
      (pai-dashboard--neuron svg (car (cdr w)) (cadr (cdr w)) 1.8 bg bg))
    (dolist (p (append top bottom))
      (pai-dashboard--neuron svg (car p) (cadr p) 2.6 bg bg))
    ;; one active neuron in each row
    (pai-dashboard--neuron svg 75 28 3.6 a2 bg 2)
    (pai-dashboard--neuron svg 54 94 3.6 a2 bg 2)
    ;; the key's legend
    (svg-text svg "M-x" :x 68 :y 76 :text-anchor "middle" :font-family font
              :font-size 32 :font-weight "bold" :fill bg)
    ;; the AI sparkle on the key's corner, and a small one beside it
    (pai-dashboard--sparkle svg 114 24 16 "url(#pai-word)" bg)
    (pai-dashboard--sparkle svg 131 46 7 "url(#pai-word)" bg)
    ;; wordmark, rule and tagline
    (svg-text svg "pai" :x 144 :y 88 :font-family font :font-size 78 :font-weight "bold"
              :fill "url(#pai-word)")
    (svg-rectangle svg 148 101 204 2 :rx 1 :fill "url(#pai-word)" :opacity 0.55)
    (svg-text svg pai-dashboard-tagline :x 148 :y 122 :font-family font :font-size 14
              :fill fg :opacity 0.7)
    svg))

(defconst pai-dashboard--text-logo
  '("╭─·─·─·─╮✦"
    "│ M-x ✦ │  p a i"
    "╰───────╯")
  "Logo for displays without SVG.")

(defun pai-dashboard--logo-string ()
  "Return the logo as a string to insert (an image, or text)."
  (if (and (display-graphic-p) (image-type-available-p 'svg))
      (propertize " " 'display (svg-image (pai-dashboard-logo-svg) :ascent 'center)
                  'rear-nonsticky t)
    (pcase-let ((`(,_bg ,_fg ,a1 ,a2) (pai-dashboard--palette)))
      (concat (propertize (nth 0 pai-dashboard--text-logo) 'face `(:foreground ,a1)) "\n"
              (propertize (nth 1 pai-dashboard--text-logo) 'face `(:foreground ,a2 :weight bold)) "\n"
              (propertize (nth 2 pai-dashboard--text-logo) 'face `(:foreground ,a1))))))


;;;; Contents

(defun pai-dashboard-skills ()
  "Return the skills of this session as (NAME . DESCRIPTION), sorted by name."
  (let ((dirs (if (fboundp 'pai--skill-dirs) (pai--skill-dirs) (pai-skills-default-dirs))))
    (sort (mapcar (lambda (s) (cons (plist-get s :name) (or (plist-get s :description) "")))
                  (pai-discover-skills dirs))
          (lambda (a b) (string< (car a) (car b))))))

(defun pai-dashboard--extension-dirs ()
  "Return the extension directories of this session."
  (seq-filter #'file-directory-p
              (list (expand-file-name "extensions" pai-directory)
                    (expand-file-name ".pai/extensions" default-directory))))

(defun pai-dashboard-extensions ()
  "Return the installed extensions as (NAME FILE . SUMMARY), sorted by name.
An extension is a directory holding NAME.el (or a top-level NAME.el); its
summary is the `;;; NAME --- SUMMARY' header line of that file.  Disabled
extensions are left out unless an enabled one requires them (see
`pai-ext-visible-names')."
  (let ((seen (make-hash-table :test 'equal)) (out '())
        (visible (pai-ext-visible-names (pai-dashboard--extension-dirs))))
    (dolist (dir (pai-dashboard--extension-dirs))
      (dolist (entry (directory-files dir t "\\`[^.]"))
        (let* ((name (file-name-base entry))
               (file (if (file-directory-p entry)
                         (expand-file-name (concat name ".el") entry)
                       (and (string-suffix-p ".el" entry) entry))))
          (when (and file (file-readable-p file) (not (gethash name seen))
                     (member name visible))
            (puthash name t seen)
            (push (cons name (cons file (or (ignore-errors (lm-summary file)) ""))) out)))))
    (sort out (lambda (a b) (string< (car a) (car b))))))

(defun pai-dashboard-snippets ()
  "Return the prompt snippets as (NAME . FILE), or nil when there are none."
  (when (and (fboundp 'pai-prompt-snippets--load)
             (pai-ext-visible-p "pai-prompt-snippets" (pai-dashboard--extension-dirs)))
    (ignore-errors
      (mapcar (lambda (s) (cons (pai-prompt-snippet-name s) (pai-prompt-snippet-path s)))
              (pai-prompt-snippets--load)))))

;;;; Actions

(defun pai-dashboard--put-in-prompt (text)
  "Replace the prompt of the pai buffer at point with TEXT, and go there."
  (let ((inhibit-read-only t))
    (delete-region pai--input-marker (point-max))
    (goto-char (point-max))
    (insert text)))

(defun pai-dashboard--open-in-dired (file)
  "Show FILE's directory in Dired in another window, with point on FILE."
  (dired-other-window (file-name-directory file))
  (dired-goto-file file))

(defun pai-dashboard--button (label face action help)
  "Return LABEL as a button in FACE that calls ACTION; HELP is its tooltip."
  (let ((map (make-sparse-keymap))
        (fn (lambda (&optional _event) (interactive) (funcall action))))
    (define-key map (kbd "RET") fn)
    (define-key map [mouse-1] fn)
    (propertize label 'face face 'mouse-face 'highlight 'help-echo help
                'keymap map 'follow-link t 'pai-dashboard-button t)))

;;;; Layout

(defun pai-dashboard--width ()
  "Return the width to lay the dashboard out in."
  (let ((w (get-buffer-window (current-buffer) t)))
    (max 40 (min 110 (if w (window-body-width w) 80)))))

(defun pai-dashboard--indent (half)
  "Return a space that stretches to HALF left of the window's centre.
HALF is an `:align-to' expression (a number of columns, or `(0.5 . IMAGE)').
The display engine re-evaluates it on every redisplay, so the dashboard
stays centred when the window is resized, like the Spacemacs home buffer."
  (propertize " " 'display `(space :align-to (- center ,half))
              'rear-nonsticky t))

(defun pai-dashboard--center (line &optional _width)
  "Return LINE, kept horizontally centred in the window."
  (concat (pai-dashboard--indent (/ (string-width line) 2.0)) line))

(defun pai-dashboard--section (title items width render &optional empty)
  "Return section TITLE listing ITEMS (at most `pai-dashboard-max-items').
RENDER turns an item into a line; lines are cut to WIDTH.  Without items,
the section shows EMPTY (a hint) instead."
  (let* ((shown (seq-take items pai-dashboard-max-items))
         (more (- (length items) (length shown))))
    (concat (propertize (format "%s (%d)" title (length items)) 'face 'pai-dashboard-heading)
            "\n"
            (if (and (null items) empty)
                (propertize (concat "  " empty) 'face 'pai-dashboard-muted)
              "")
            (mapconcat (lambda (item) (truncate-string-to-width
                                       (concat "  " (funcall render item)) width nil nil "…"))
                       shown "\n")
            (if (> more 0) (propertize (format "\n  … and %d more" more) 'face 'pai-dashboard-muted) ""))))

(defun pai-dashboard--name-column (names)
  "Return the width of a name column for NAMES (longer names are cut)."
  (min 22 (apply #'max 8 (mapcar #'string-width names))))

(defun pai-dashboard--fit (name width)
  "Return NAME cut or padded to exactly WIDTH columns."
  (string-pad (truncate-string-to-width name width nil nil "…") width))

(defun pai-dashboard-render ()
  "Return the dashboard as a propertized string for the current pai buffer."
  (let* ((width (pai-dashboard--width))
         (skills (pai-dashboard-skills))
         (extensions (pai-dashboard-extensions))
         (snippets (pai-dashboard-snippets))
         (model (if (and (boundp 'pai--model) pai--model (fboundp 'pai-model-key))
                    (pai-model-key pai--model) "no model selected"))
         (project (abbreviate-file-name default-directory))
         (column (pai-dashboard--name-column (append (mapcar #'car skills) (mapcar #'car extensions))))
         (body-width (min width 96))
         (logo (pai-dashboard--logo-string)))
    (concat
     "\n"
     (if (string-search "\n" logo)
         (pai-dashboard--center-block logo width)
       ;; an image: centre it by its own (scaled) pixel width
       (concat (pai-dashboard--indent `(0.5 . ,(get-text-property 0 'display logo))) logo))
     "\n\n"
     (pai-dashboard--center
      (concat (propertize model 'face 'pai-dashboard-name)
              (propertize "  ·  " 'face 'pai-dashboard-muted)
              (propertize project 'face 'pai-dashboard-muted))
      width)
     "\n"
     (pai-dashboard--center
      (propertize "RET on an item to use it · /dashboard shows this again · /hotkeys for keys"
                  'face 'pai-dashboard-muted)
      width)
     "\n\n"
     (pai-dashboard--center-block (pai-dashboard--lists skills extensions snippets column body-width) width)
     "\n")))

(defun pai-dashboard--lists (skills extensions snippets column body-width)
  "Return the Skills, Extensions and Prompt snippets sections, stacked."
  (concat
   (pai-dashboard--section
    "Skills" skills body-width
    (lambda (s)
      (concat (pai-dashboard--button
               (pai-dashboard--fit (car s) column) 'pai-dashboard-name
               (lambda () (pai-dashboard--put-in-prompt (format "/skill:%s " (car s))))
               (format "Insert /skill:%s in the prompt" (car s)))
              "  " (propertize (cdr s) 'face 'pai-dashboard-muted)))
    "none yet -- teach one with /learn, or add one under ~/.pai/skills")
   "\n\n"
   (pai-dashboard--section
    "Extensions" extensions body-width
    (lambda (e)
      (concat (pai-dashboard--button
               (pai-dashboard--fit (car e) column) 'pai-dashboard-name
               (lambda () (pai-dashboard--open-in-dired (cadr e)))
               (format "Open %s in Dired"
                       (abbreviate-file-name (file-name-directory (cadr e)))))
              "  " (propertize (cddr e) 'face 'pai-dashboard-muted))))
   (if snippets
       (concat "\n\n"
               (pai-dashboard--section
                "Prompt snippets" snippets body-width
                (lambda (s)
                  (pai-dashboard--button
                   (car s) 'pai-dashboard-name
                   (lambda () (find-file-other-window (cdr s)))
                   (format "Open %s" (abbreviate-file-name (cdr s)))))))
     "")))

(defun pai-dashboard--center-block (text width)
  "Return TEXT with every line shifted right so the block stays centred.
The block's width is its widest line, so its columns stay aligned."
  (ignore width)
  (let* ((lines (split-string text "\n"))
         (half (/ (apply #'max 0 (mapcar #'string-width lines)) 2.0)))
    (mapconcat (lambda (l) (if (string-empty-p l) l (concat (pai-dashboard--indent half) l)))
               lines "\n")))

;;;; Showing it

(defun pai-dashboard-insert ()
  "Insert the dashboard into the current pai buffer's transcript."
  (when (and (boundp 'pai--output-marker) (markerp pai--output-marker)
             (marker-buffer pai--output-marker))
    (condition-case err
        (pai--insert (pai-dashboard-render))
      (error (message "pai-dashboard: %s" (error-message-string err))))))

(defun pai-dashboard--on-session-start (event ctx)
  "Show the dashboard when a conversation starts (EVENT's reason is not resume)."
  (when (and pai-dashboard-enable
             (memq (plist-get event :reason) '(startup new))
             (not noninteractive))
    (let ((buffer (plist-get ctx :buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          ;; Only on an empty transcript, and never in subagent sessions.
          (when (and (not (bound-and-true-p pai-isub--parent))
                     (= (marker-position pai--output-marker) (point-min)))
            (pai-dashboard-insert)))))))

(defun pai-dashboard--command (_args ctx)
  "Handler for `/dashboard': show the dashboard in this buffer."
  (let ((buffer (plist-get ctx :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (pai-dashboard-insert)))
    nil))

(pai-register-extension
 (lambda (api)
   (pai-ext-on api 'session-start #'pai-dashboard--on-session-start)
   (pai-ext-register-command api "dashboard"
                             :description "Show the dashboard (logo, skills, extensions)"
                             :handler #'pai-dashboard--command))
 "dashboard")

(provide 'pai-dashboard)
;;; pai-dashboard.el ends here
