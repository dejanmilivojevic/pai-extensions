;;; pai-memory-browse-test.el --- Tests for the memory browser (V2 F1) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defun pai-memory-brw--goto (text)
  "Move to the line containing TEXT."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(ert-deftest pai-memory-browse-sections-and-actions ()
  (pai-memory-test--with-owner owner dir
    (cl-letf (((symbol-function 'pai-memory--skill-dirs) (lambda () (list (expand-file-name "skills" dir))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (let ((e (pai-memory-test--turns session 1)))
        (pai-memory-apply-change '(:action add :target user :content "Prefers terse answers"))
        (pai-memory-commit-observations session "r" (plist-get (nth 0 e) :id) (plist-get (nth 1 e) :id)
                                        (list (list :timestamp "2026-09-22 10:00" :content "User likes tea")))
        (with-temp-file (expand-file-name "build.md" (pai-memory-session-dir session t))
          (insert "---\nid: build\ntitle: Build notes\nsummary: make test\n---\nx\n"))
        (let ((skill (expand-file-name "skills/learned/tea/SKILL.md" dir)))
          (make-directory (file-name-directory skill) t)
          (with-temp-file skill (insert "---\nname: tea\ndescription: brew tea\norigin: learned\n---\nboil\n")))
        (let ((buf (pai-memory-browse owner)))
          (unwind-protect
              (with-current-buffer buf
                (let ((text (buffer-string)))
                  (should (string-match-p "Long-term memory" text))
                  (should (string-match-p "Prefers terse answers" text))
                  (should (string-match-p "Build notes  make test" text))
                  (should (string-match-p "Observations on this branch (1)" text))
                  (should (string-match-p "tea — brew tea  learned · 0 views" text))
                  (should (string-match-p "Pending proposals (0)" text)))
                ;; source of an observation
                (pai-memory-brw--goto "User likes tea")
                (let ((src (pai-memory-browse-source)))
                  (should (string-match-p "Distilled from" (with-current-buffer src (buffer-string))))
                  (should (string-match-p "user: u0" (with-current-buffer src (buffer-string)))))
                ;; edit a memory entry (logged)
                (pai-memory-brw--goto "Prefers terse answers")
                (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Prefers short answers")))
                  (pai-memory-browse-edit))
                (should (equal (pai-memory-read 'user default-directory) '("Prefers short answers")))
                (should (string-match-p "Prefers short answers" (buffer-string)))
                ;; remove it
                (pai-memory-brw--goto "Prefers short answers")
                (pai-memory-browse-delete)
                (should-not (pai-memory-read 'user default-directory))
                ;; hide the observation
                (pai-memory-brw--goto "User likes tea")
                (pai-memory-browse-delete)
                (should-not (pai-memory-pool (pai-session-get-branch session)))
                (should (string-match-p "Observations on this branch (0)" (buffer-string)))
                ;; archive the learned skill
                (pai-memory-brw--goto "tea — brew tea")
                (pai-memory-browse-delete)
                (should (member "tea" (pai-memory-archived-skills)))
                ;; search
                (let ((hits (pai-memory-browse-search "u0")))
                  (should (string-match-p "match" (with-current-buffer hits (buffer-string)))))
                ;; lines without an item
                (goto-char (point-min))
                (should-error (pai-memory-browse-edit) :type 'user-error))
            (kill-buffer buf)
            (dolist (b '("*pai memory source*" "*pai memory search*"))
              (when (get-buffer b) (kill-buffer b)))
            (pai-memory-index-close)))))))

(provide 'pai-memory-browse-test)
;;; pai-memory-browse-test.el ends here
