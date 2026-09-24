;;; pai-memory-recall-test.el --- Tests for automatic recall (V2 A2) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-rec--with-home (dir &rest body)
  "Run BODY with a temp pai home DIR, recall on, and a project cwd."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-rec" t)))
          (pai-directory ,dir)
          (default-directory (file-name-as-directory (expand-file-name "proj" ,dir)))
          (pai-settings--global '(:memory (:search (:recall t))))
          (pai-settings--project nil))
     (make-directory default-directory t)
     (cl-letf (((symbol-function 'pai-memory--skill-dirs) (lambda () nil)))
       (unwind-protect (progn ,@body)
         (pai-memory-index-close)
         (delete-directory ,dir t)))))

(defun pai-memory-rec--past (cwd)
  "Create an earlier session in CWD with an observation about postgres."
  (let ((s (pai-session-new cwd)))
    (pai-session-append-message s (pai-user-message "set up the database"))
    (pai-session-append-custom
     s "memory.observations"
     (list :runId "r" :coversFromId "a" :coversUpToId "b"
           :observations (vector (list :id "r.1" :timestamp "2026-09-20 10:00"
                                       :content "completed: migrated the postgres schema with alembic upgrade head"))))
    s))

(ert-deftest pai-memory-recall-terms-and-trivial ()
  (should (equal (pai-memory-recall-terms "How do we migrate the Postgres schema, please?")
                 '("migrate" "postgres" "schema")))
  (should (pai-memory-recall-trivial-p "ok"))
  (should (pai-memory-recall-trivial-p "yes."))
  (should (pai-memory-recall-trivial-p "/memory status"))
  (should (pai-memory-recall-trivial-p "fix it"))
  (should-not (pai-memory-recall-trivial-p "run the postgres migration again")))

(ert-deftest pai-memory-recall-finds-other-sessions-only ()
  (pai-memory-rec--with-home dir
    (pai-memory-rec--past default-directory)
    (let ((current (pai-session-new default-directory)))
      (pai-session-append-message current (pai-user-message "postgres alembic question here"))
      (pai-memory-index-update)
      (let ((r (pai-memory-recall-compute "how did we run alembic for postgres" current)))
        (should r)
        (should (string-match-p "<memory-context>" (car r)))
        (should (string-match-p "alembic upgrade head" (car r)))
        ;; the current session's own messages are not recalled
        (should-not (seq-find (lambda (h) (equal (plist-get h :session) (pai-session-id current)))
                              (cdr r))))
      (should-not (pai-memory-recall-compute "nothing about zebras whatsoever here" current)))))

(ert-deftest pai-memory-recall-is-stable-and-persisted ()
  (pai-memory-rec--with-home dir
    (pai-memory-rec--past default-directory)
    (pai-memory-index-update)
    (let* ((s (pai-session-new default-directory))
           (u1 (pai-user-message "how did we run alembic for postgres"))
           (msgs (list (pai-system-message "SYS") u1)))
      (pai-session-append-message s u1)
      (with-temp-buffer
        (let* ((r1 (pai-memory-recall-apply msgs s t))
               (sent (pai-message-content (nth 1 (car r1)))))
          (should (cdr r1))
          (should (string-match-p "\\`how did we run alembic for postgres\n\n<memory-context>" sent))
          ;; the stored message is untouched
          (should (equal (pai-message-content u1) "how did we run alembic for postgres"))
          ;; later requests: same text, no new search, no new entry
          (let* ((u2 (pai-user-message "thanks, and the index?"))
                 (r2 (pai-memory-recall-apply (append msgs (list (pai-assistant-message) u2)) s t)))
            (should (equal (pai-message-content (nth 1 (car r2))) sent))
            (should-not (cdr r2)))
          (should (= 2 (cl-count "memory.recall" (pai-session-entries s)
                                 :key (lambda (e) (plist-get e :customType)) :test #'equal)))
          ;; a resumed session reproduces it without searching
          (let ((loaded (pai-session-load (pai-session-file s))))
            (setq pai-memory--recall-table nil)
            (cl-letf (((symbol-function 'pai-memory-search) (lambda (&rest _) (error "no search"))))
              (should (equal (pai-message-content
                              (nth 1 (car (pai-memory-recall-apply msgs loaded nil))))
                             sent)))))))))

(ert-deftest pai-memory-recall-in-a-real-run ()
  "The model sees the recall block; the transcript does not."
  (pai-faux-reset)
  (pai-memory-test--with-pai-buffer buf dir
    (with-current-buffer buf
      (let ((pai-settings--global '(:memory (:search (:recall t)))))
        (cl-letf (((symbol-function 'pai-memory--skill-dirs) (lambda () nil)))
          (pai-register-extension #'pai-memory-extension "memory")
          (pai-memory-rec--past default-directory)
          (pai-memory-index-update)
          (pai-faux-push '(:text "We used alembic." :stop-reason stop))
          (goto-char (point-max))
          (insert "how did we run alembic for postgres")
          (pai-send)
          (let ((sent (car (last (plist-get pai-faux-last-context :messages)))))
            (should (string-match-p "<memory-context>" (pai-content-text (pai-message-content sent)))))
          (should (string-match-p "🧠 recalled 1 from 1 session" (buffer-string)))
          (let ((stored (seq-find #'pai-user-message-p (reverse pai--context-messages))))
            (should (equal (pai-message-content stored) "how did we run alembic for postgres")))
          (pai-memory-index-close))))))

(ert-deftest pai-memory-recall-off-by-default ()
  (pai-memory-rec--with-home dir
    (setq pai-settings--global nil)
    (let ((s (pai-session-new default-directory)))
      (should-not (pai-memory-recall-context-handler
                   (list :messages (list (pai-user-message "alembic postgres migration")))
                   (list :session s))))))

(provide 'pai-memory-recall-test)
;;; pai-memory-recall-test.el ends here
