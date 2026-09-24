;;; pai-memory-helpers.el --- Shared fixtures for pai-memory tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Not a test file (no -test suffix), so `make test' does not load it on its
;; own; test files `require' it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)

;; A pai buffer like test/pai-ui-test.el's harness.  That file is not
;; required: `make test' loads every test file, and loading one twice fails.
(defmacro pai-memory-test--with-pai-buffer (buf dir &rest body)
  "Bind DIR to a temp project and BUF to a fresh faux-backed pai buffer; run BODY."
  (declare (indent 2))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-memui" t)))
          (pai-directory (expand-file-name ".pai-state" ,dir))
          (pai-default-model "faux")
          (,buf nil))
     (unwind-protect
         (progn
           (pai-ext-reset)
           (setq ,buf (get-buffer-create (generate-new-buffer-name "*pai-memory-test*")))
           (with-current-buffer ,buf
             (setq default-directory ,dir)
             (pai--setup ,dir))
           ,@body)
       (when (buffer-live-p ,buf) (kill-buffer ,buf))
       (ignore-errors (delete-directory ,dir t)))))

;;;; Fixtures

(defvar pai-memory-test--ts 1790000000000
  "Base message timestamp (ms) for deterministic fixtures.")

(defmacro pai-memory-test--with-settings (memory &rest body)
  "Run BODY with global `:memory' settings MEMORY and no project settings."
  (declare (indent 1))
  `(let ((pai-settings--global (list :memory ,memory))
         (pai-settings--project nil))
     ,@body))

(defmacro pai-memory-test--with-session (s dir &rest body)
  "Run BODY with S bound to a fresh session under a temp pai home DIR."
  (declare (indent 2))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-memt" t)))
          (pai-directory ,dir)
          (pai-memory-test--minute 0)
          (,s (pai-session-new ,dir)))
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defun pai-memory-test--msg (msg n)
  "Return MSG stamped with the Nth fixture timestamp."
  (plist-put msg :timestamp (+ pai-memory-test--ts (* n 60000))))

(defun pai-memory-test--turns (session n &optional chars)
  "Append N user/assistant turns of CHARS characters each to SESSION.
Return the appended entries in order."
  (let ((chars (or chars 300)) (out '()))
    (dotimes (i n)
      (push (pai-session-append-message
             session (pai-memory-test--msg
                      (pai-user-message (format "u%d %s" i (make-string chars ?u))) (* 2 i)))
            out)
      (push (pai-session-append-message
             session (pai-memory-test--msg
                      (pai-assistant-message :content (list (pai-text (format "a%d %s" i (make-string chars ?a)))))
                      (1+ (* 2 i))))
            out))
    (nreverse out)))

(defvar pai-memory-test--minute 0
  "Fixture clock: every committed observation gets the next minute.")

(defun pai-memory-test--commit (session run from to &rest contents)
  "Commit observations with CONTENTS covering FROM..TO entries in SESSION.
Timestamps increase across calls, like real observations."
  (pai-memory-commit-observations
   session run (plist-get from :id) (plist-get to :id)
   (mapcar (lambda (c)
             (let ((m (cl-incf pai-memory-test--minute)))
               (list :timestamp (format "2026-09-22 %02d:%02d" (+ 10 (/ m 60)) (% m 60))
                     :content c)))
           contents)))

(defvar pai-memory-test--model nil)
(defun pai-memory-test--model ()
  (or pai-memory-test--model
      (progn (pai-register-model (pai-make-model :id "faux-mem" :provider "faux" :api 'faux
                                                 :context-window 20000
                                                 :cost (list :input 1.0 :output 5.0)))
             (setq pai-memory-test--model (pai-model "faux/faux-mem")))))

(defmacro pai-memory-test--with-owner (buf dir &rest body)
  "Run BODY in a pai-like buffer BUF owning a session in temp DIR."
  (declare (indent 2))
  `(pai-memory-test--with-session session ,dir
     (let ((,buf (generate-new-buffer " *memory-test*")))
       (unwind-protect
           (with-current-buffer ,buf
             (pai-faux-reset)
             (setq default-directory ,dir)
             (insert "history\n\n" pai-prompt-string)
             (setq-local pai--input-marker (copy-marker (point) nil))
             (setq-local pai--model (pai-memory-test--model))
             (setq-local pai--session session)
             (setq-local pai--context-messages nil)
             ,@body)
         (with-current-buffer ,buf
           (when (timerp pai-activity--timer) (cancel-timer pai-activity--timer)))
         (kill-buffer ,buf)
         (pai-faux-reset)))))

(defun pai-memory-test--observe-response (&rest contents)
  "Script one observer run that records CONTENTS, then confirms."
  (pai-faux-push
   (list :tool-calls
         (list (list :id "r1" :name "record_observations"
                     :arguments (list :observations
                                      (mapcar (lambda (c) (list :timestamp "2026-09-22 10:00"
                                                                :content c))
                                              contents)))))
   '(:text "Covered." :stop-reason stop)))

(provide 'pai-memory-helpers)
;;; pai-memory-helpers.el ends here
