;;; pai-memory-privacy-test.el --- Tests for forget, private, redaction (V2 G1, G3) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-prv--with-home (dir &rest body)
  "Run BODY with a temp pai home DIR and a project cwd."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-prv" t)))
          (pai-directory ,dir)
          (default-directory (file-name-as-directory (expand-file-name "proj" ,dir)))
          (pai-settings--global nil)
          (pai-settings--project nil))
     (make-directory default-directory t)
     (cl-letf (((symbol-function 'pai-memory--skill-dirs)
                (lambda () (list (expand-file-name "skills" ,dir)))))
       (unwind-protect (progn ,@body)
         (pai-memory-index-close)
         (delete-directory ,dir t)))))

;;;; Redaction (G3)

(ert-deftest pai-memory-custom-redaction ()
  (pai-memory-prv--with-home dir
    (setq pai-settings--global '(:memory (:redact-patterns ["ACME-[0-9]+" ["client" "Contoso"]])))
    (should (equal (pai-memory-redact "ticket ACME-1234 for Contoso")
                   "ticket [redacted:custom] for [redacted:client]"))
    ;; invalid patterns are ignored, never an error
    (setq pai-settings--global '(:memory (:redact-patterns ["[unclosed"])))
    (should (equal (pai-memory-redact "fine") "fine"))
    ;; memory writes and the index are redacted
    (setq pai-settings--global '(:memory (:redact-patterns ["Contoso"])))
    (pai-memory-apply-change '(:action add :target project :content "Client is Contoso"))
    (should (equal (pai-memory-read 'project default-directory) '("Client is [redacted:custom]")))
    (let ((s (pai-session-new default-directory)))
      (pai-session-append-message s (pai-user-message "Contoso invoice numbat")))
    (pai-memory-index-update)
    (should (pai-memory-search "numbat"))
    (should-not (pai-memory-search "Contoso"))))

;;;; Private sessions (G3)

(ert-deftest pai-memory-private-session ()
  (pai-memory-prv--with-home dir
    (let ((s (pai-session-new default-directory)))
      (pai-session-append-message s (pai-user-message "secret project walrus"))
      (pai-memory-index-update)
      (should (pai-memory-search "walrus"))
      (should (pai-memory-session-enabled-p s))
      (should (string-match-p "private" (pai-memory-set-private s t)))
      (should (pai-memory-private-p s))
      (should-not (pai-memory-session-enabled-p s))
      (should-not (pai-memory-learning-enabled-p s))
      (should-not (pai-memory-recall-enabled-p s))
      ;; its rows are gone and new lines are not indexed
      (should-not (pai-memory-search "walrus"))
      (pai-session-append-message s (pai-user-message "more walrus plans"))
      (pai-memory-index-update)
      (should-not (pai-memory-search "walrus"))
      ;; a fresh index honours the private line too
      (pai-memory-index-reset)
      (pai-memory-index-update)
      (should-not (pai-memory-search "walrus"))
      ;; public again: the whole file is indexed again
      (pai-memory-set-private s nil)
      (pai-memory-index-update)
      (should (= (length (pai-memory-search "walrus" :limit 10)) 2))
      (pai-memory-index-update)
      (should (= (length (pai-memory-search "walrus" :limit 10)) 2)))))

(ert-deftest pai-memory-private-command-and-widget ()
  (pai-memory-test--with-owner buf dir
    (should (string-match-p "private" (plist-get (pai-memory-command "private" (list :buffer buf)) :message)))
    (should (string-match-p "🔒" (pai-memory-widget-text)))
    (should (string-match-p "PRIVATE" (plist-get (pai-memory-command "" (list :buffer buf)) :message)))
    (pai-memory-test--turns session 2 3000)
    (should (= (pai-memory-observer-tick t) 0))
    (should-not (pai-memory-promote-due-p session 'session-end))
    (should (string-match-p "no longer private"
                            (plist-get (pai-memory-command "private off" (list :buffer buf)) :message)))))

;;;; Forget (G1)

(ert-deftest pai-memory-forget-everywhere ()
  (pai-memory-prv--with-home dir
    (let* ((s (pai-session-new default-directory))
           (u (pai-session-append-message s (pai-user-message "my old address is 12 Wombat Road"))))
      ;; long-term memory, a topic file, observations, a learned skill, usage notes
      (pai-memory-apply-change '(:action add :target user :content "Lives at 12 Wombat Road"))
      (pai-memory-apply-change '(:action add :target user :content "Prefers terse answers"))
      (let ((tdir (pai-memory-session-dir s t)))
        (with-temp-file (expand-file-name "home.md" tdir)
          (insert "---\nid: home\n---\n# Home\nThe user lives at 12 Wombat Road.\nThey have a cat.\n")))
      (pai-memory-commit-observations s "r1" (plist-get u :id) (plist-get u :id)
                                      (list (list :timestamp "2026-09-22 10:00"
                                                  :content "User stated they live at 12 Wombat Road")
                                            (list :timestamp "2026-09-22 10:01"
                                                  :content "User has a cat")))
      (let ((skill (expand-file-name "skills/learned/mail/SKILL.md" dir)))
        (make-directory (file-name-directory skill) t)
        (with-temp-file skill
          (insert "---\nname: mail\ndescription: send mail\norigin: learned\n---\nSend to 12 Wombat Road\nUse the post office\n")))
      (pai-memory-record-outcomes (list (list :content "skill-used: mail - failed: 12 Wombat Road unknown")))
      (pai-memory-index-update)
      ;; dry run changes nothing
      (let ((plan-text (pai-memory-forget '("Wombat" "Road" "--dry-run"))))
        (should (string-match-p "USER.md: 1 entry" plan-text))
        (should (string-match-p "home.md: 1 line" plan-text))
        (should (string-match-p "observations in 1 session(s): 1" plan-text))
        (should (string-match-p "skill usage notes: mail" plan-text)))
      (should (member "Lives at 12 Wombat Road" (pai-memory-read 'user default-directory)))
      ;; declined
      (should (equal (pai-memory-forget '("Wombat" "Road") (lambda (_) nil)) "Nothing forgotten"))
      ;; applied
      (should (string-match-p "Forgot" (pai-memory-forget '("Wombat" "Road") (lambda (_) t))))
      (should (equal (pai-memory-read 'user default-directory) '("Prefers terse answers")))
      (let ((topic (pai-memory--read-file (expand-file-name "home.md" (pai-memory-session-dir s)))))
        (should-not (string-match-p "Wombat" topic))
        (should (string-match-p "They have a cat" topic)))
      (let ((skill (pai-memory--read-file (expand-file-name "skills/learned/mail/SKILL.md" dir))))
        (should (string-match-p "\\`---\nname: mail" skill))
        (should-not (string-match-p "Wombat" skill))
        (should (string-match-p "post office" skill)))
      (should-not (seq-some (lambda (n) (string-match-p "Wombat" n))
                            (append (plist-get (pai-memory-skill-usage "mail") :notes) nil)))
      ;; observations hidden, on the session's branch
      (let ((pool (pai-memory-pool (pai-session-get-branch (pai-session-load (pai-session-file s))))))
        (should (equal (mapcar (lambda (o) (plist-get o :content)) pool) '("User has a cat"))))
      ;; the index forgets it, and a reindex does not bring it back
      (should-not (pai-memory-search "Wombat"))
      (pai-memory-index-reset)
      (pai-memory-index-update)
      (should-not (pai-memory-search "Wombat"))
      ;; backups exist; undo restores a rewritten file
      (should (directory-files-recursively (pai-memory-dir "backups") "home\\.md\\'"))
      (should (string-match-p "Undid" (pai-memory-undo)))
      ;; nothing left to forget
      (should (string-match-p "Nothing in memory matches"
                              (pai-memory-forget '("zebra" "crossing") (lambda (_) t))))
      (should (string-match-p "Usage" (pai-memory-forget '("ab")))))))

(ert-deftest pai-memory-forget-updates-a-live-session ()
  "A session open in a buffer gets the redaction on its live object and branch."
  (pai-memory-test--with-owner buf dir
    (let ((e (pai-memory-test--turns session 1)))
      (pai-memory-commit-observations session "r" (plist-get (nth 0 e) :id) (plist-get (nth 1 e) :id)
                                      (list (list :timestamp "t" :content "User named the dog Rex")))
      (should (string-match-p "Forgot" (pai-memory-forget '("Rex") (lambda (_) t))))
      (should-not (pai-memory-pool (pai-session-get-branch session)))
      ;; the live leaf is the redaction entry, so later turns stay on this branch
      (should (equal (plist-get (gethash (pai-session-leaf-id session) (pai-session-by-id session))
                                :customType)
                     "memory.redacted")))))

(provide 'pai-memory-privacy-test)
;;; pai-memory-privacy-test.el ends here
