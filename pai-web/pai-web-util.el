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
  "Forget the cached face styles (after a theme change)."
  (clrhash pai-web--face-cache))

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

(defun pai-web--span (text face)
  "Return TEXT (unescaped) as HTML, wrapped in a span styled for FACE."
  (let ((style (if face (pai-web-face-style face) "")))
    (if (string-empty-p style)
        (pai-web-html-escape text)
      (concat "<span style=\"" style "\">" (pai-web-html-escape text) "</span>"))))

(defun pai-web-propertized-html (string &optional max-chars)
  "Return STRING as HTML, its `face' and `font-lock-face' as inline CSS.
Invisible text is left out.  With MAX-CHARS, only that many characters
are converted and an ellipsis marks the cut."
  (let* ((string (or string ""))
         (len (length string))
         (end (if (and max-chars (> len max-chars)) max-chars len))
         (pos 0)
         (parts nil))
    (while (< pos end)
      (let* ((next (min end
                        (next-single-property-change pos 'face string end)
                        (next-single-property-change pos 'font-lock-face string end)
                        (next-single-property-change pos 'invisible string end)))
             (next (if (> next pos) next (1+ pos))))
        (unless (get-text-property pos 'invisible string)
          (push (pai-web--span (substring-no-properties string pos next)
                               (or (get-text-property pos 'face string)
                                   (get-text-property pos 'font-lock-face string)))
                parts))
        (setq pos next)))
    (when (< end len) (push "…" parts))
    (apply #'concat (nreverse parts))))

(defun pai-web-default-colors ()
  "Return (:fg COLOR :bg COLOR) of the default face, as CSS colours."
  (list :fg (or (pai-web--color (face-attribute 'default :foreground nil t)) "")
        :bg (or (pai-web--color (face-attribute 'default :background nil t)) "")))

(provide 'pai-web-util)
;;; pai-web-util.el ends here
