;;; pai-web-buffers.el --- Emacs buffers shown and driven from the browser -*- lexical-binding: t; -*-

;;; Commentary:

;; Some of pai's UI lives in buffers of its own: the /menu settings screen,
;; `/todo edit', the memory review, the ask_user dialog, the compose buffer,
;; diffs...  The browser shows such a buffer as live text with its faces
;; (`pai-web-buffer-render') and drives it the Emacs way: a tap on a button
;; or link runs it, a tap on text moves point, keys from the key bar run
;; their bindings at point, typed text runs each character's binding (so a
;; letter inserts in an Org buffer and acts as a command in a dialog), and
;; an editable widget field takes a new value.
;;
;; Shown are buffers opened by something a page asked for (the buffer is
;; displayed while `pai-web--origin' is bound) and pai-related buffers (a
;; `pai-' major mode, or a "*...pai...*" or "*MCP...*" name), but never
;; other Emacs buffers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'button)
(require 'wid-edit)
(require 'pai-web-util)
(require 'pai-web-bus)
(require 'pai-web-instances)
(require 'pai-web-prompt)

(defconst pai-web-buffer-render-chars 150000
  "Characters of a buffer rendered at most (around point).")

(defvar pai-web--opened (make-hash-table :test 'eq :weakness 'key)
  "Buffers opened by actions of a page.")

;;;; Which buffers

(defun pai-web-buffer-related-p (buffer)
  "Return non-nil when BUFFER may be shown in the browser."
  (and (buffer-live-p buffer)
       (not (eq (buffer-local-value 'major-mode buffer) 'pai-mode))
       (not (minibufferp buffer))
       (let ((name (buffer-name buffer)))
         (and (not (string-prefix-p " " name))
              (not (string-prefix-p "*helm" name))   ; completion UI internals
              (or (gethash buffer pai-web--opened)
                  (string-prefix-p "pai-" (symbol-name (buffer-local-value 'major-mode buffer)))
                  (string-match-p "\\`\\*\\(?:.*pai\\|MCP\\)" name))))))

(setq pai-web-buffer-related-p-function #'pai-web-buffer-related-p)

(defun pai-web-buffer-list ()
  "Return the buffers the browser may show, as a vector of entries."
  (vconcat (mapcar #'pai-web-buffer-entry
                   (seq-filter #'pai-web-buffer-related-p (buffer-list)))))

(defun pai-web--mode-name (buffer)
  "Return BUFFER's mode name as shown in its mode line."
  (let ((s (string-trim (or (ignore-errors (format-mode-line (buffer-local-value 'mode-name buffer)
                                                             nil nil buffer))
                            ""))))
    (if (string-empty-p s)
        (string-remove-suffix "-mode" (symbol-name (buffer-local-value 'major-mode buffer)))
      s)))

(defun pai-web-buffer-entry (buffer)
  "Return the list entry of BUFFER."
  (list :b (pai-web-id buffer) :name (buffer-name buffer)
        :mode (pai-web--mode-name buffer)
        :opened (pai-web-bool (gethash buffer pai-web--opened))))

(defun pai-web--note-displayed (buffer)
  "Remember BUFFER when an action of a page displayed it; tell the pages."
  (when (and pai-web--origin (buffer-live-p buffer)
             (not (eq (buffer-local-value 'major-mode buffer) 'pai-mode))
             (not (minibufferp buffer))
             (not (string-prefix-p " " (buffer-name buffer))))
    (unless (gethash buffer pai-web--opened)
      (puthash buffer t pai-web--opened)
      (pai-web-bus-broadcast (list :t "opened" :buffer (pai-web-buffer-entry buffer))))))

(defun pai-web--advise-display-buffer (orig buffer-or-name &rest args)
  "Call ORIG (`display-buffer') with BUFFER-OR-NAME and ARGS; note the buffer."
  (prog1 (apply orig buffer-or-name args)
    (ignore-errors (pai-web--note-displayed (get-buffer buffer-or-name)))))

(defun pai-web--advise-switch-to-buffer (orig buffer-or-name &rest args)
  "Call ORIG (`switch-to-buffer') with BUFFER-OR-NAME and ARGS; note the buffer."
  (let ((result (apply orig buffer-or-name args)))
    (ignore-errors (pai-web--note-displayed (window-buffer (selected-window))))
    result))

(defun pai-web-buffers-install ()
  "Start noting buffers displayed by page actions."
  (advice-add 'display-buffer :around #'pai-web--advise-display-buffer)
  (advice-add 'switch-to-buffer :around #'pai-web--advise-switch-to-buffer))

(defun pai-web-buffers-uninstall ()
  "Stop noting displayed buffers."
  (advice-remove 'display-buffer #'pai-web--advise-display-buffer)
  (advice-remove 'switch-to-buffer #'pai-web--advise-switch-to-buffer))

;;;; Rendering

(defun pai-web--faces-at (pos)
  "Return the faces at POS: overlays' (by priority) before the text's."
  (let* ((ovs (sort (seq-filter (lambda (o) (overlay-get o 'face)) (overlays-at pos))
                    (lambda (a b) (> (or (overlay-get a 'priority) 0)
                                     (or (overlay-get b 'priority) 0)))))
         (faces (append (mapcar (lambda (o) (overlay-get o 'face)) ovs)
                        (let ((f (or (get-text-property pos 'face)
                                     (get-text-property pos 'font-lock-face))))
                          (and f (list f))))))
    (cond ((null faces) nil)
          ((null (cdr faces)) (car faces))
          (t (apply #'append (mapcar #'pai-web--face-list faces))))))

(defun pai-web--actionable (pos)
  "Return how POS reacts to a tap: \"field\", \"button\" or nil."
  (cond
   ((ignore-errors (widget-field-at pos)) "field")
   ((or (button-at pos)
        (get-char-property pos 'button)
        (get-char-property pos 'mouse-face)
        (get-char-property pos 'keymap)
        (get-char-property pos 'local-map)
        (get-char-property pos 'follow-link))
    "button")))

(defun pai-web--overlay-strings (beg end)
  "Return an alist of POS -> HTML of overlay before/after strings in BEG..END."
  (let (out)
    (dolist (o (overlays-in beg end))
      (when-let ((s (overlay-get o 'before-string)))
        (push (list (overlay-start o) 0 (pai-web-propertized-html s 4000)) out))
      (when-let ((s (overlay-get o 'after-string)))
        (push (list (overlay-end o) 1 (pai-web-propertized-html s 4000)) out)))
    (sort out (lambda (a b) (or (< (car a) (car b))
                                (and (= (car a) (car b)) (> (nth 1 a) (nth 1 b))))))))

(defun pai-web--display-text (display text)
  "Return what DISPLAY (a `display' property) shows instead of TEXT, or nil."
  (cond
   ((stringp display) display)
   ((and (consp display) (eq (car display) 'image)) "[image]")
   ((and (consp display) (eq (car display) 'space)) " ")
   ((and (consp display) (stringp (car (last display)))) (car (last display)))
   (t (ignore text) nil)))

(defun pai-web-buffer-render (buffer)
  "Return BUFFER rendered for the browser (a plist for JSON)."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let* ((win (get-buffer-window buffer t))
             (pt (if win (window-point win) (point)))
             (half (/ pai-web-buffer-render-chars 2))
             (beg (save-excursion (goto-char (max (point-min) (- pt half)))
                                  (line-beginning-position)))
             (end (save-excursion (goto-char (min (point-max) (+ beg pai-web-buffer-render-chars)))
                                  (line-end-position)))
             (strings (pai-web--overlay-strings beg end))
             (parts nil)
             (pos beg)
             (cursor-done nil))
        (while (< pos end)
          (while (and strings (<= (caar strings) pos))
            (push (nth 2 (pop strings)) parts))
          (let* ((next (min end (next-char-property-change pos end)
                            (if strings (max (1+ pos) (caar strings)) end)))
                 (next (if (and (> pt pos) (< pt next)) pt next))
                 (display (get-char-property pos 'display)))
            (unless (invisible-p pos)
              (let* ((text (buffer-substring-no-properties pos next))
                     (shown (or (and display (pai-web--display-text display text)) text))
                     (face (pai-web--faces-at pos))
                     (style (if face (pai-web-face-style face) ""))
                     (act (pai-web--actionable pos)))
                (when (and (= pos pt) (not cursor-done))
                  (setq cursor-done t)
                  (push "<span class=\"pt\"></span>" parts))
                (push (format "<span data-p=\"%d\"%s%s>%s</span>"
                              pos
                              (if act (format " class=\"%s\"" act) "")
                              (if (string-empty-p style) "" (format " style=\"%s\"" style))
                              (pai-web-html-escape shown))
                      parts)))
            (setq pos next)))
        (dolist (s strings) (push (nth 2 s) parts))
        (unless cursor-done (push "<span class=\"pt\"></span>" parts))
        (list :b (pai-web-id buffer) :name (buffer-name buffer)
              :mode (pai-web--mode-name buffer)
              :point pt :beg beg :end end :size (buffer-size)
              :readonly (pai-web-bool buffer-read-only)
              :colors (pai-web-default-colors)
              :html (apply #'concat (nreverse parts)))))))

(defun pai-web-buffer-signature (buffer)
  "Return a value that changes when BUFFER's rendering may have changed."
  (with-current-buffer buffer
    (let ((win (get-buffer-window buffer t)))
      (list (buffer-modified-tick) (buffer-chars-modified-tick)
            (if win (window-point win) (point))
            (length (overlays-in (point-min) (point-max)))
            (buffer-name buffer)))))

(defun pai-web-buffer-field (buffer pos)
  "Return the editable field at POS of BUFFER as (:start :end :value), or nil."
  (with-current-buffer buffer
    (let ((w (ignore-errors (widget-field-at pos))))
      (when w
        (list :start (widget-field-start w) :end (widget-field-end w)
              :value (or (ignore-errors (widget-value w)) ""))))))

;;;; Actions (run from a timer with `pai-web--origin' bound)

(defmacro pai-web--in-buffer (buffer &rest body)
  "Run BODY in BUFFER, in its window when it has one."
  (declare (indent 1) (debug t))
  `(let ((win (get-buffer-window ,buffer t)))
     (if (window-live-p win)
         (with-selected-window win (with-current-buffer ,buffer ,@body))
       (with-current-buffer ,buffer ,@body))))

(defun pai-web--goto (pos)
  "Move point to POS in the current buffer (and its selected window)."
  (goto-char (max (point-min) (min (point-max) pos))))

(defun pai-web--run-command (cmd keys)
  "Run command CMD as if KEYS (a vector or string) had been typed."
  (let* ((keys (vconcat keys))      ; `kbd' gives a string for C-c C-c
         (this-command cmd)
        (real-this-command cmd)
        (last-command-event (aref keys (1- (length keys))))
         (current-prefix-arg nil))
    (call-interactively cmd nil keys)
    (setq last-command cmd)))

(defun pai-web-buffer-click (buffer pos)
  "Act on a tap at POS in BUFFER: push the button there or move point."
  (pai-web--in-buffer buffer
    (pai-web--goto pos)
    (let ((button (button-at (point)))
          (widget (ignore-errors (widget-at (point)))))
      (cond
       (button (push-button (point)))
       ((and widget (not (widget-field-at (point))))
        (widget-apply-action widget))
       (t
        (let* ((map (or (get-char-property (point) 'keymap)
                        (get-char-property (point) 'local-map)))
               (cmd (and (keymapp map)
                         (or (lookup-key map (kbd "RET"))
                             (lookup-key map [return])))))
          (when (and (commandp cmd) (not (numberp cmd)))
            (pai-web--run-command cmd (kbd "RET")))))))))

(defun pai-web-buffer-key-kind (buffer keys)
  "Return what KEYS (a `kbd' description) are at point in BUFFER.
One of \"command\", \"prefix\" (more keys must follow) or \"undefined\".
Only looks the keys up; nothing runs."
  (pai-web--in-buffer buffer
    (let* ((vec (condition-case nil (kbd keys) (error nil)))
           (cmd (and vec (> (length vec) 0) (key-binding vec t))))
      (cond ((keymapp cmd) "prefix")
            ((and cmd (not (numberp cmd)) (commandp cmd)) "command")
            (t "undefined")))))

(defun pai-web-buffer-key (buffer keys)
  "Run the binding of KEYS (a `kbd' description) at point in BUFFER.
Return nil or an error message."
  (pai-web--in-buffer buffer
    (let* ((vec (condition-case nil (kbd keys) (error nil)))
           (cmd (and vec (> (length vec) 0) (key-binding vec t))))
      (cond
       ((null vec) (format "Not a key: %s" keys))
       ((or (null cmd) (numberp cmd)) (format "%s is undefined here" keys))
       ((keymapp cmd) (format "%s is a prefix key; send the whole sequence" keys))
       ((not (commandp cmd)) (format "%s is not bound to a command" keys))
       (t (pai-web--run-command cmd vec) nil)))))

(defun pai-web-buffer-type (buffer text)
  "Type TEXT at point in BUFFER: each character runs its binding."
  (pai-web--in-buffer buffer
    (dolist (ch (string-to-list text))
      (let* ((vec (vector (if (eq ch ?\n) ?\r ch)))
             (cmd (key-binding vec t)))
        (if (or (null cmd) (numberp cmd) (keymapp cmd) (not (commandp cmd)))
            (let ((inhibit-read-only nil)) (insert (char-to-string ch)))
          (pai-web--run-command cmd vec))))))

(defun pai-web-buffer-set-field (buffer pos value)
  "Set the editable field at POS of BUFFER to VALUE (the field's hooks run).
Return nil or an error message."
  (with-current-buffer buffer
    (let ((w (ignore-errors (widget-field-at pos))))
      (if (not w)
          "No editable field there"
        (let ((start (widget-field-start w))
              (end (widget-field-end w)))
          (save-excursion
            (goto-char start)
            (delete-region start end)
            (insert value))
          nil)))))

(defun pai-web-buffer-goto (buffer pos)
  "Move point to POS in BUFFER."
  (pai-web--in-buffer buffer (pai-web--goto pos)))

(provide 'pai-web-buffers)
;;; pai-web-buffers.el ends here
