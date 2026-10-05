;;; pai-web-util.el --- Helpers for pai-web: JSON, HTML, faces, ids -*- lexical-binding: t; -*-

;;; Commentary:

;; Small, pure helpers shared by the pai-web modules:
;;
;; - `pai-web-json': encode a value with the native `json-serialize', after
;;   making every string valid UTF-8 (tool output may carry raw bytes).
;;   Objects are plists (keyword keys), arrays are vectors; use `:false' and
;;   `:null' for JSON false and null.
;; - `pai-web-html-escape' and `pai-web-propertized-html': text with face
;;   properties to HTML spans with inline CSS, so code highlighted by Emacs'
;;   own major modes and diffs keep their colours in the browser.
;; - `pai-web-random-hex': tokens from /dev/urandom.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'color)

;;;; JSON

(defconst pai-web--invalid-char-re "[^\0-\uD7FF\uE000-\U0010FFFF]"
  "Characters JSON cannot carry: surrogates, raw bytes, Emacs-only chars.")

(defun pai-web-utf8 (string)
  "Return STRING as a valid UTF-8 (multibyte) string without properties.
Raw bytes and other characters outside Unicode become U+FFFD."
  (let ((s (substring-no-properties string)))
    (unless (multibyte-string-p s)
      (setq s (decode-coding-string s 'utf-8)))
    (if (string-match-p pai-web--invalid-char-re s)
        (replace-regexp-in-string pai-web--invalid-char-re "\uFFFD" s t t)
      s)))

(defun pai-web-json-clean (value)
  "Return VALUE with every string made valid for `json-serialize'.
Plists stay plists, vectors stay vectors; other lists are taken as plists."
  (cond
   ((stringp value) (pai-web-utf8 value))
   ((vectorp value) (vconcat (mapcar #'pai-web-json-clean value)))
   ((and (consp value) (keywordp (car value)))
    (let ((out nil))
      (while value
        (push (car value) out)
        (push (pai-web-json-clean (cadr value)) out)
        (setq value (cddr value)))
      (nreverse out)))
   ((null value) :null)
   ((memq value '(t :false :null)) value)
   ((numberp value) value)
   ((symbolp value) (symbol-name value))
   ((consp value) (vconcat (mapcar #'pai-web-json-clean value)))
   (t (format "%s" value))))

(defun pai-web-json (value)
  "Return VALUE encoded as a JSON string (multibyte).
See `pai-web-json-clean' for the accepted shapes; nil becomes null."
  (json-serialize (pai-web-json-clean value)))

(defun pai-web-json-read (string)
  "Parse the JSON STRING (bytes or text) into plists, vectors become lists.
JSON null is nil and false is `:false'."
  (json-parse-string (if (multibyte-string-p string) string
                       (decode-coding-string string 'utf-8))
                     :object-type 'plist :array-type 'list
                     :null-object nil :false-object :false))

(defun pai-web-bool (value)
  "Return VALUE as a JSON boolean."
  (if (and value (not (eq value :false))) t :false))

;;;; Random tokens

(defun pai-web-random-hex (bytes)
  "Return BYTES random bytes from the kernel as a hex string."
  (let ((raw (condition-case nil
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally "/dev/urandom" nil 0 bytes)
                   (buffer-string))
               (error nil))))
    (if (and raw (= (length raw) bytes))
        (mapconcat (lambda (c) (format "%02x" c)) raw "")
      ;; no /dev/urandom: hash plenty of state, seeded from the system
      (random t)
      (substring (secure-hash 'sha512
                              (format "%s%s%s%s" (random) (random) (float-time)
                                      (emacs-pid)))
                 0 (* 2 bytes)))))

;;;; HTML

(defun pai-web-html-escape (string)
  "Return STRING with HTML special characters escaped."
  (let ((s (pai-web-utf8 (or string ""))))
    (if (string-match-p "[&<>\"]" s)
        (replace-regexp-in-string
         "[&<>\"]"
         (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (">" "&gt;") (_ "&quot;")))
         s t t)
      s)))

(defvar pai-web--face-cache (make-hash-table :test 'equal)
  "Map of face specs to their inline CSS (or \"\").")

(defun pai-web-face-cache-clear ()
  "Forget the cached face styles and glyph widths (after a theme change)."
  (clrhash pai-web--face-cache)
  (when (boundp 'pai-web--glyph-cols) (clrhash pai-web--glyph-cols)))

(defun pai-web--color (color)
  "Return COLOR (an Emacs colour name or #hex) as a CSS colour, or nil."
  (when (and (stringp color) (not (string-prefix-p "unspecified" color)))
    (if (string-prefix-p "#" color)
        (if (= (length color) 13)          ; #RRRRGGGGBBBB
            (format "#%s%s%s" (substring color 1 3) (substring color 5 7)
                    (substring color 9 11))
          color)
      (let ((rgb (ignore-errors (color-name-to-rgb color))))
        (if rgb
            (apply #'color-rgb-to-hex (append rgb '(2)))
          (and (string-match-p "\\`[A-Za-z]+\\'" color) color))))))

(defun pai-web--face-list (face)
  "Return FACE (a face spec from a `face' property) as a list of faces."
  (cond
   ((null face) nil)
   ((and (consp face) (keywordp (car face))) (list face))   ; anonymous face
   ((and (consp face) (memq (car face) '(foreground-color background-color)))
    (list face))
   ((consp face) face)
   (t (list face))))

(defun pai-web--face-attr (face attr)
  "Return attribute ATTR of FACE (a named or anonymous face), or nil."
  (let ((v (cond
            ((and (consp face) (keywordp (car face)))
             (or (plist-get face attr)
                 (let ((inherit (plist-get face :inherit)))
                   (and inherit
                        (seq-some (lambda (f) (pai-web--face-attr f attr))
                                  (if (listp inherit) inherit (list inherit)))))))
            ((and (consp face) (eq (car face) 'foreground-color))
             (and (eq attr :foreground) (cdr face)))
            ((and (consp face) (eq (car face) 'background-color))
             (and (eq attr :background) (cdr face)))
            ((and (symbolp face) (facep face))
             (face-attribute face attr nil t)))))
    (unless (or (null v) (eq v 'unspecified)) v)))

(defun pai-web-face-style (face)
  "Return the inline CSS for the face spec FACE, \"\" when it has none."
  (or (gethash face pai-web--face-cache)
      (puthash
       face
       (condition-case nil
           (let* ((faces (pai-web--face-list face))
                  (get (lambda (attr) (seq-some (lambda (f) (pai-web--face-attr f attr)) faces)))
                  (inverse (funcall get :inverse-video))
                  (fg (pai-web--color (funcall get :foreground)))
                  (bg (pai-web--color (funcall get :background)))
                  (weight (funcall get :weight))
                  (slant (funcall get :slant))
                  (underline (funcall get :underline))
                  (strike (funcall get :strike-through))
                  (height (funcall get :height))
                  (parts nil))
             (when inverse (cl-rotatef fg bg))
             (when fg (push (format "color:%s" fg) parts))
             (when bg (push (format "background:%s" bg) parts))
             (when (memq weight '(bold extra-bold ultra-bold semi-bold heavy black))
               (push "font-weight:bold" parts))
             (when (memq slant '(italic oblique))
               (push "font-style:italic" parts))
             (when (or underline strike)
               (push (format "text-decoration:%s"
                             (string-join (delq nil (list (and underline "underline")
                                                          (and strike "line-through")))
                                          " "))
                     parts))
             (when (and (floatp height) (> height 1.05))
               (push (format "font-size:%.2fem" (min height 2.0)) parts))
             (string-join (nreverse parts) ";"))
         (error ""))
       pai-web--face-cache)))

(defvar pai-web--glyph-cols (make-hash-table :test 'eql)
  "Map of character to the columns Emacs displays it in (see below).")

(defconst pai-web--glyph-re "[\u2190-\u2bff\ue000-\uf8ff\U0001F000-\U0001FAFF]"
  "Symbols, arrows, dingbats, emoji and icon-font characters.
Browsers draw these with fallback fonts whose widths differ from the
monospace cell (and from Emacs'), which breaks column layouts such as the
/context grid; each is boxed to the width Emacs gives it.")

(defun pai-web--glyph-width (char)
  "Return the width CHAR takes in Emacs, in columns (may be fractional)."
  (or (gethash char pai-web--glyph-cols)
      (puthash char
               (let ((cols (char-width char)))
                 (or (and (display-graphic-p) (fboundp 'string-pixel-width)
                          (> (frame-char-width) 0)
                          (let ((px (ignore-errors (string-pixel-width (string char)))))
                            (and (numberp px) (> px 0)
                                 (/ (fround (* 10.0 (/ (float px) (frame-char-width)))) 10.0))))
                     cols))
               pai-web--glyph-cols)))

(defun pai-web--col-after (text col)
  "Return the column after TEXT when it starts at COL."
  (let ((nl (string-search "\n" text)))
    (if (not nl)
        (+ col (string-width text))
      (string-width (substring text (1+ (or (cl-position ?\n text :from-end t) nl)))))))

(defun pai-web--text-html (text col)
  "Return (HTML . COLUMN) for plain TEXT starting at column COL.
Symbol glyphs are boxed to the width Emacs gives them (`pai-web--glyph-re')."
  (let ((start 0) (parts nil))
    (while (string-match pai-web--glyph-re text start)
      (let* ((pos (match-beginning 0))
             (before (substring text start pos))
             (char (aref text pos))
             (w (pai-web--glyph-width char)))
        (push (pai-web-html-escape before) parts)
        (setq col (pai-web--col-after before col))
        (push (format "<span class=\"g\" style=\"display:inline-block;text-align:center;width:%gch\">%s</span>"
                      w (pai-web-html-escape (string char)))
              parts)
        (setq col (+ col w) start (1+ pos))))
    (let ((rest (substring text start)))
      (push (pai-web-html-escape rest) parts)
      (setq col (pai-web--col-after rest col)))
    (cons (apply #'concat (nreverse parts)) col)))

(defun pai-web--space-width (spec col)
  "Return the width in columns of display SPEC (space ...) at column COL."
  (let* ((props (cdr spec))
         (fcw (max 1 (if (display-graphic-p) (frame-char-width) 1)))
         (cols (lambda (v) (cond ((numberp v) v)
                                 ((and (consp v) (numberp (car v))) (/ (float (car v)) fcw))))))
    (cond
     ((plist-get props :width) (funcall cols (plist-get props :width)))
     ((plist-get props :align-to)
      (let ((to (funcall cols (plist-get props :align-to))))
        (and to (max 0 (- to col))))))))

(defun pai-web--display-spec (display)
  "Return what DISPLAY shows instead of the text: a string, a (space ...)
spec, or nil to show the text itself."
  (cond
   ((stringp display) display)
   ((memq (car-safe display) '(space)) display)
   ((eq (car-safe display) 'image) "[image]")
   ((and (consp display) (not (symbolp (car display))))   ; a list of specs
    (seq-some #'pai-web--display-spec display))))

(defun pai-web-segment-html (text display col)
  "Return (HTML . COLUMN) for TEXT with `display' property DISPLAY at COL."
  (let ((spec (and display (pai-web--display-spec display))))
    (cond
     ((stringp spec) (pai-web--text-html (substring-no-properties spec) col))
     ((consp spec)
      (let ((w (pai-web--space-width spec col)))
        (if w
            (cons (format "<span style=\"display:inline-block;width:%.2fch\"></span>" w) (+ col w))
          (cons " " (1+ col)))))
     (t (pai-web--text-html text col)))))

(defun pai-web-propertized-html (string &optional max-chars)
  "Return STRING as HTML, its `face' and `font-lock-face' as inline CSS.
Invisible text is left out; `display' strings and spaces are honoured.
With MAX-CHARS, only that many characters are converted and an ellipsis
marks the cut."
  (let* ((string (or string ""))
         (len (length string))
         (end (if (and max-chars (> len max-chars)) max-chars len))
         (pos 0) (col 0)
         (parts nil))
    (while (< pos end)
      (let* ((next (min end
                        (next-single-property-change pos 'face string end)
                        (next-single-property-change pos 'font-lock-face string end)
                        (next-single-property-change pos 'invisible string end)
                        (next-single-property-change pos 'display string end)))
             (next (if (> next pos) next (1+ pos))))
        (unless (get-text-property pos 'invisible string)
          (let* ((face (or (get-text-property pos 'face string)
                           (get-text-property pos 'font-lock-face string)))
                 (style (if face (pai-web-face-style face) ""))
                 (seg (pai-web-segment-html (substring-no-properties string pos next)
                                            (get-text-property pos 'display string) col)))
            (setq col (cdr seg))
            (push (if (string-empty-p style)
                      (car seg)
                    (concat "<span style=\"" style "\">" (car seg) "</span>"))
                  parts)))
        (setq pos next)))
    (when (< end len) (push "…" parts))
    (apply #'concat (nreverse parts))))

(defun pai-web-default-colors ()
  "Return (:fg COLOR :bg COLOR) of the default face, as CSS colours."
  (list :fg (or (pai-web--color (face-attribute 'default :foreground nil t)) "")
        :bg (or (pai-web--color (face-attribute 'default :background nil t)) "")))

(provide 'pai-web-util)
;;; pai-web-util.el ends here
