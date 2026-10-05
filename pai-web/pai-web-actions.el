;;; pai-web-actions.el --- What a browser page can do to pai instances -*- lexical-binding: t; -*-

;;; Commentary:

;; Actions run as commands of Emacs' command loop, the way a key you type
;; does -- never inside the network filter, and never from a timer.  A
;; timer (or filter) runs wherever Emacs happens to wait, including inside
;; other code's `accept-process-output' or `sit-for'; an action that opens
;; the minibuffer there would hold that code up until answered, and
;; answering it (`exit-minibuffer' throws) would unwind that code half-way.
;; So `pai-web-run' queues the action and puts the event `pai-web-run' in
;; `unread-command-events' (not recorded in macros or `recent-keys'); the
;; command loop -- at top level or in a minibuffer -- reads it and runs the
;; queue as a command (`pai-web-run-pending', bound in an emulation map).
;;
;; Actions run with `pai-web--origin' bound, so the prompts they open are
;; shown in the browser (pai-web-prompt.el) and the buffers they display
;; can be opened there (pai-web-buffers.el).  Errors come back to the pages
;; as a toast event.
;;
;; Input completion (`pai-web-complete') runs the chat buffer's own
;; `completion-at-point-functions' on the page's text: slash commands and
;; their arguments at every level, @file and *buffer mentions, and
;; extension providers complete exactly as in Emacs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-config)
(require 'pai-web-util)
(require 'pai-web-bus)
(require 'pai-web-auth)
(require 'pai-web-instances)
(require 'pai-web-prompt)

(defvar pai--input-marker)
(defvar pai--model)
(defvar pai-thinking-levels)
(declare-function pai-send-message "pai-ui" (text &optional buffer images))
(declare-function pai-interrupt "pai-ui" ())
(declare-function pai-set-model "pai-ui" (id))
(declare-function pai-set-thinking "pai-ui" (level))
(declare-function pai-new-session "pai-ui" (&optional cwd))
(declare-function pai-model-keys "pai-models" ())
(declare-function pai-model-key "pai-models" (model))
(declare-function pai-image "pai-core" (data mime-type))
(declare-function pai-session-directory "pai-session" (cwd))
(declare-function pai-history-add "pai-history" (dir text))

(defconst pai-web-complete-limit 200 "Completion candidates sent to a page.")

;;;; Running actions

(defun pai-web-toast (text &optional level client)
  "Show TEXT (LEVEL info, warning or error) on the pages, or only CLIENT's."
  (pai-web-bus-broadcast (list :t "toast" :text text :level (symbol-name (or level 'info)))
                         (and client (lambda (c) (equal (pai-web-client-id c) client)))))

(defvar pai-web--queue nil
  "Actions waiting for the command loop: list of functions, oldest first.")

(defvar pai-web--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [pai-web-run] #'pai-web-run-pending)
    map)
  "Keymap binding the event that runs queued actions.")

(defvar pai-web--emulation-alist (list (cons t pai-web--keymap))
  "Emulation map alist making `pai-web--keymap' active everywhere.")

(defun pai-web-actions-install ()
  "Make the command loop run queued actions."
  (add-to-list 'emulation-mode-map-alists 'pai-web--emulation-alist))

(defun pai-web-actions-uninstall ()
  "Stop running queued actions; forget the queue."
  (setq pai-web--queue nil)
  (setq unread-command-events
        (seq-remove (lambda (e) (equal e '(no-record . pai-web-run))) unread-command-events))
  (setq emulation-mode-map-alists (delq 'pai-web--emulation-alist emulation-mode-map-alists)))

(defun pai-web--wake ()
  "Ask the command loop to run the queue (once, after pending input).
Batch Emacs has no command loop (and no minibuffer): a timer runs it."
  (if noninteractive
      (run-at-time 0 nil #'pai-web-run-pending)
    (unless (member '(no-record . pai-web-run) unread-command-events)
    (setq unread-command-events
          (append unread-command-events (list '(no-record . pai-web-run)))))))

(defun pai-web-run-pending ()
  "Run the actions pages queued.  Bound to the `pai-web-run' event."
  (interactive)
  (let ((last last-command)
        (enable-recursive-minibuffers t))
    (unwind-protect
        (while pai-web--queue
          (funcall (pop pai-web--queue)))
      ;; an action may leave by a non-local exit (a prompt it answered)
      (when pai-web--queue (pai-web--wake))
      ;; invisible to the command you type next (`last-command' chains)
      (setq this-command last))))

(defun pai-web-run (thunk &optional buffer client)
  "Queue THUNK as an action of a page (CLIENT), run by the command loop.
In BUFFER when given (the action is dropped when it is gone).  A string
returned by THUNK is shown to CLIENT as an error."
  (setq pai-web--queue
        (append pai-web--queue
                (list (lambda ()
                        (let ((pai-web--origin t))
                          (condition-case err
                              (let ((result (if buffer
                                                (if (buffer-live-p buffer)
                                                    (with-current-buffer buffer (funcall thunk))
                                                  "That buffer is gone")
                                              (funcall thunk))))
                                (when (stringp result) (pai-web-toast result 'error client)))
                            (quit (pai-web-toast "Cancelled" 'warning client))
                            (error (pai-web-toast (error-message-string err) 'error client))))))))
  (pai-web--wake))

;;;; Chat actions

(defun pai-web--images (images)
  "Return image blocks from the page's IMAGES list of (:data B64 :mime TYPE)."
  (delq nil
        (mapcar (lambda (img)
                  (let ((data (plist-get img :data))
                        (mime (plist-get img :mime)))
                    (and (stringp data) (stringp mime)
                         (member mime '("image/png" "image/jpeg" "image/gif" "image/webp"))
                         (pai-image data mime))))
                images)))

(defun pai-web-send (buffer text images)
  "Submit TEXT with IMAGES to the chat BUFFER, keeping a draft typed in Emacs."
  (with-current-buffer buffer
    (let* ((start (marker-position pai--input-marker))
           (draft (buffer-substring start (point-max))))
      (let ((inhibit-read-only t)) (delete-region start (point-max)))
      ;; what you send from a page is your input too: M-p finds it
      (when (and (stringp text) (not (string-empty-p (string-trim text))))
        (ignore-errors (pai-history-add default-directory (string-trim text))))
      (unwind-protect
          (pai-send-message (or text "") buffer (pai-web--images images))
        (when (and (buffer-live-p buffer) (not (string-empty-p draft)))
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-max))
              (insert draft))))))))

(defun pai-web-models (buffer)
  "Return the models of chat BUFFER and its current one."
  (with-current-buffer buffer
    (list :models (vconcat (ignore-errors (pai-model-keys)))
          :current (if pai--model (pai-model-key pai--model) :null)
          :levels (vconcat pai-thinking-levels))))

;;;; Completion

(defun pai-web--capf-result ()
  "Return the first `completion-at-point-functions' result at point, or nil."
  (let (result)
    (run-hook-wrapped 'completion-at-point-functions
                      (lambda (fn)
                        (let ((r (ignore-errors (funcall fn))))
                          (when (and (consp r) (integerp (car r)))
                            (setq result r)
                            t))))
    result))

(defun pai-web-complete (buffer text pos)
  "Return completions of TEXT at character POS as typed in chat BUFFER.
A plist (:beg :end :items [(:v CANDIDATE :a ANNOTATION)...]) where BEG..END
is the part of TEXT a candidate replaces, or nil."
  (with-current-buffer buffer
    (when (and (markerp pai--input-marker) (marker-buffer pai--input-marker))
      (let* ((start (marker-position pai--input-marker))
             (inhibit-read-only t)
             (inhibit-modification-hooks t)
             (buffer-undo-list t)
             (modified (buffer-modified-p))
             (saved (buffer-substring start (point-max)))
             (saved-point (point))
             (pos (max 0 (min pos (length text)))))
        (unwind-protect
            (progn
              (delete-region start (point-max))
              (goto-char (point-max))
              (insert text)
              (goto-char (+ start pos))
              (let ((r (pai-web--capf-result)))
                (when r
                  (let* ((beg (nth 0 r)) (end (nth 1 r)) (table (nth 2 r))
                         (props (nthcdr 3 r))
                         (prefix (buffer-substring-no-properties beg end))
                         (pred (plist-get props :predicate))
                         (annotate (plist-get props :annotation-function))
                         (all (ignore-errors (all-completions prefix table pred)))
                         (all (seq-take (sort (delete-dups (copy-sequence all)) #'string<)
                                        pai-web-complete-limit)))
                    (when all
                      (list :beg (- beg start) :end (- end start)
                            :items (vconcat
                                    (mapcar (lambda (c)
                                              (list :v (substring-no-properties c)
                                                    :a (or (and annotate
                                                                (ignore-errors
                                                                  (let ((a (funcall annotate c)))
                                                                    (and (stringp a)
                                                                         (string-trim
                                                                          (string-trim-left a "[  —-]+"))))))
                                                           "")))
                                            all))))))))
          (delete-region start (point-max))
          (goto-char (point-max))
          (insert saved)
          (goto-char (min saved-point (point-max)))
          (set-buffer-modified-p modified))))))

;;;; Project directories for new instances

(defun pai-web--session-cwd (dir)
  "Return the project directory recorded in the newest session file of DIR."
  (let ((files (directory-files dir t "\\.jsonl\\'")))
    (when files
      (let ((newest (car (sort files #'file-newer-than-file-p))))
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents newest nil 0 4096)
            (goto-char (point-min))
            (let ((line (buffer-substring-no-properties (point) (line-end-position))))
              (plist-get (pai-web-json-read line) :cwd))))))))

(defun pai-web-known-dirs ()
  "Return project directories for a new instance, as a vector."
  (let ((dirs (mapcar (lambda (b) (abbreviate-file-name (buffer-local-value 'default-directory b)))
                      (pai-web-instances)))
        (root (expand-file-name "sessions" pai-directory)))
    (when (file-directory-p root)
      (dolist (d (directory-files root t "\\`[^.]"))
        (when (file-directory-p d)
          (let ((cwd (pai-web--session-cwd d)))
            (when (and (stringp cwd) (file-directory-p cwd))
              (push (abbreviate-file-name (file-name-as-directory cwd)) dirs))))))
    (vconcat (delete-dups (nreverse dirs)))))

;;;; Uploads

(defun pai-web--safe-name (name)
  "Return file NAME made safe: no directories, no spaces or odd characters."
  (let* ((base (file-name-nondirectory (or name "")))
         (base (replace-regexp-in-string "[^[:alnum:]._-]+" "_" base))
         (base (string-trim base "[._]+" "[_]+")))
    (if (string-empty-p base) "upload" base)))

(defun pai-web-upload (buffer name bytes)
  "Save BYTES as file NAME for chat BUFFER; return (:path P :mention S)."
  (let* ((cwd (buffer-local-value 'default-directory buffer))
         (dir (if (equal (pai-web-upload-mode) "project")
                  (expand-file-name (pai-web-upload-subdir) cwd)
                (expand-file-name (format-time-string "uploads/%Y-%m-%d/")
                                  (pai-web-directory))))
         (name (pai-web--safe-name name))
         (file (expand-file-name name dir))
         (n 1))
    (make-directory dir t)
    (while (file-exists-p file)
      (setq file (expand-file-name (format "%s-%d%s" (file-name-sans-extension name) n
                                           (if (file-name-extension name)
                                               (concat "." (file-name-extension name)) ""))
                                   dir)
            n (1+ n)))
    (let ((coding-system-for-write 'no-conversion))
      (write-region bytes nil file nil 'silent))
    (let ((shown (if (file-in-directory-p file cwd)
                     (file-relative-name file cwd)
                   (abbreviate-file-name file))))
      (list :path shown :mention (concat "@" shown)))))

(provide 'pai-web-actions)
;;; pai-web-actions.el ends here
