;;; pai-memory-skills-test.el --- Tests for pai-memory Phase 5 -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-kt--with-skills (dir skills &rest body)
  "Run BODY with a temp pai home DIR and a skill dir SKILLS (both bound)."
  (declare (indent 2))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-sk" t)))
          (pai-directory ,dir)
          (,skills (expand-file-name "skills" ,dir))
          (default-directory ,dir)
          (pai-settings--global nil)
          (pai-settings--project nil))
     (cl-letf (((symbol-function 'pai-memory--skill-dirs) (lambda () (list ,skills))))
       (unwind-protect (progn ,@body)
         (delete-directory ,dir t)))))

(defun pai-memory-kt--skill (skills name &optional learned created)
  "Write skill NAME under SKILLS; LEARNED adds origin: learned; CREATED a date."
  (let ((file (expand-file-name (format "%s%s/SKILL.md" (if learned "learned/" "") name) skills)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (format "---\nname: %s\ndescription: does %s\n%s%s---\nsteps\n" name name
                      (if learned "origin: learned\n" "")
                      (if created (format "created: %s\n" created) ""))))
    file))

(defun pai-memory-kt--age (file days)
  "Set FILE's modification time DAYS in the past."
  (set-file-times file (time-subtract nil (* days 86400))))

;;;; Tracking

(ert-deftest pai-memory-usage-views-uses-outcomes ()
  (pai-memory-kt--with-skills dir skills
    (let ((file (pai-memory-kt--skill skills "ert-batch" t)))
      ;; a read of the skill file is a view; the next other tool call a use
      (with-temp-buffer
        (pai-memory-track-tool-start (list :tool-name "read" :args (list :path file)))
        (pai-memory-track-tool-start (list :tool-name "read" :args (list :path "/tmp/other")))
        (pai-memory-track-tool-start (list :tool-name "bash" :args (list :command "make test")))
        (pai-memory-track-tool-start (list :tool-name "bash" :args (list :command "again"))))
      (let ((r (pai-memory-skill-usage "ert-batch")))
        (should (= (plist-get r :views) 1))
        (should (= (plist-get r :uses) 1))
        (should (plist-get r :last_used)))
      ;; /skill:NAME input; a bare /name is a command, not a skill
      (pai-memory-track-input "/skill:ert-batch now")
      (pai-memory-track-input "/ert-batch")
      (pai-memory-track-input "/skill:unknown-skill")
      (pai-memory-track-input "no slash")
      (should (= (plist-get (pai-memory-skill-usage "ert-batch") :uses) 2))
      (should-not (pai-memory-skill-usage "unknown-skill"))
      ;; outcomes from observations
      (pai-memory-record-outcomes
       (list (list :content "skill-used: ert-batch - followed: all green")
             (list :content "skill-used: ert-batch — deviated: needed -L extensions")
             (list :content "skill-used: ert-batch - failed: make missing")
             (list :content "skill-used: nope - failed: x")
             (list :content "User asked something")))
      (let ((r (pai-memory-skill-usage "ert-batch")))
        (should (equal (plist-get r :outcomes) '(:followed 1 :deviated 1 :failed 1)))
        (should (= (plist-get (plist-get r :review) :failed) 1))
        (should (= (length (plist-get r :notes)) 3))))))

(ert-deftest pai-memory-parse-outcome ()
  (should (equal (pai-memory-parse-outcome "skill-used: a-b - Failed: x") '("a-b" . "failed")))
  (should (equal (pai-memory-parse-outcome "skill-used: a: followed") '("a" . "followed")))
  (should-not (pai-memory-parse-outcome "skill-used: a - tried it"))
  (should-not (pai-memory-parse-outcome "correction: x")))

(ert-deftest pai-memory-review-candidates-reset-on-change ()
  (pai-memory-kt--with-skills dir skills
    (let ((file (pai-memory-kt--skill skills "flaky" t))
          (manual (pai-memory-kt--skill skills "handmade")))
      (ignore manual)
      (pai-memory-record-outcomes
       (list (list :content "skill-used: flaky - failed: a")
             (list :content "skill-used: handmade - failed: a")
             (list :content "skill-used: handmade - failed: b")))
      (should-not (pai-memory-review-candidates))
      (pai-memory-record-outcomes (list (list :content "skill-used: flaky - deviated: b")))
      ;; learned skills only
      (should (equal (mapcar (lambda (c) (plist-get (car c) :name)) (pai-memory-review-candidates))
                     '("flaky")))
      ;; the skill is patched: its review counts start over
      (with-temp-file file (insert "---\nname: flaky\ndescription: d\norigin: learned\n---\nfixed\n"))
      (should-not (pai-memory-review-candidates))
      (pai-memory-record-outcomes (list (list :content "skill-used: flaky - failed: c")))
      (should (= (plist-get (plist-get (pai-memory-skill-usage "flaky") :review) :failed) 1))
      (should (= (plist-get (plist-get (pai-memory-skill-usage "flaky") :outcomes) :failed) 2)))))

;;;; Curator

;;;; Curator

(defun pai-memory-kt--sessions (n &optional cwd)
  "Count N new sessions in project CWD (default `default-directory')."
  (dotimes (_ n)
    (let ((s (pai-session-new (or cwd default-directory) 'memory)))
      (pai-memory-count-session s))))

(ert-deftest pai-memory-count-session-once ()
  (pai-memory-kt--with-skills dir skills
    (let ((s (pai-session-new default-directory 'memory)))
      (should (pai-memory-count-session s))
      ;; a resumed session is not counted again
      (should-not (pai-memory-count-session s))
      (pai-memory-kt--sessions 2 (expand-file-name "other" dir))
      (should (= (pai-memory-session-count) 3))
      (should (= (pai-memory-session-count default-directory) 1))
      (should (= (pai-memory-session-count (expand-file-name "other" dir)) 2)))))

(ert-deftest pai-memory-curator-counts-sessions-not-time ()
  (pai-memory-kt--with-skills dir skills
    (setq pai-settings--global
          '(:memory (:long-term (:stale-after-sessions 3 :stale-after-days 1
                                 :archive-after-sessions 6 :archive-after-days 2))))
    (let ((old (pai-memory-kt--skill skills "old-one" t "2020-01-01"))
          (keeper (pai-memory-kt--skill skills "keeper" t "2020-01-01"))
          (manual (pai-memory-kt--skill skills "manual")))
      (dolist (f (list old keeper manual)) (pai-memory-kt--age f 400))
      (pai-memory-set-pinned "keeper" t)
      ;; first sight: counting starts now, nothing happens
      (should (equal (pai-memory-curate) '(:stale 0 :archived 0 :expired 0 :flagged 0)))
      ;; a year away from Emacs with no sessions: still nothing
      (should (equal (pai-memory-curate) '(:stale 0 :archived 0 :expired 0 :flagged 0)))
      ;; three sessions without using it: stale
      (pai-memory-kt--sessions 3)
      (should (= (plist-get (pai-memory-curate) :stale) 1))
      (should (equal (plist-get (pai-memory-skill-usage "old-one") :state) "stale"))
      ;; used again: active, counting restarts
      (pai-memory-record-use "old-one")
      (should (equal (plist-get (pai-memory-skill-usage "old-one") :state) "active"))
      (pai-memory-kt--sessions 5)
      (should (= (plist-get (pai-memory-curate) :archived) 0))
      (pai-memory-kt--sessions 1)
      ;; enough sessions, but it was used today: the day floor holds it
      (should (= (plist-get (pai-memory-curate) :archived) 0))
      (setq pai-settings--global
            '(:memory (:long-term (:stale-after-sessions 3 :stale-after-days 0
                                   :archive-after-sessions 6 :archive-after-days 0))))
      (should (= (plist-get (pai-memory-curate) :archived) 1))
      (should-not (file-exists-p old))
      (should (file-exists-p keeper))
      (should (file-exists-p manual))
      (should (equal (pai-memory-archived-skills) '("old-one")))
      ;; restore
      (should (string-match-p "Restored old-one" (pai-memory-restore-skill "old-one")))
      (should (file-exists-p old))
      (should-not (pai-memory-archived-skills))
      (should (string-match-p "No archived skill" (pai-memory-restore-skill "old-one")))
      ;; the curator ran: not due again until the interval passes
      (should-not (pai-memory-curator-due-p))
      (setq pai-settings--global '(:memory (:long-term (:curator-interval-days 0))))
      (should (pai-memory-curator-due-p)))))

(ert-deftest pai-memory-curator-day-floor ()
  "A burst of sessions in one day does not make a fresh skill stale."
  (pai-memory-kt--with-skills dir skills
    (setq pai-settings--global '(:memory (:long-term (:stale-after-sessions 2 :stale-after-days 14))))
    (pai-memory-kt--skill skills "fresh" t (format-time-string "%Y-%m-%d"))
    (pai-memory-curate)
    (pai-memory-kt--sessions 10)
    (should (= (plist-get (pai-memory-curate) :stale) 0))))

(ert-deftest pai-memory-curator-project-skills-count-their-project ()
  (pai-memory-kt--with-skills dir skills
    (setq pai-settings--global
          '(:memory (:long-term (:stale-after-sessions 2 :stale-after-days 0))))
    (let* ((proj (file-name-as-directory (expand-file-name "proj" dir)))
           (pskills (expand-file-name ".pai/skills" proj))
           (default-directory proj))
      (make-directory proj t)
      (pai-memory-kt--skill pskills "proj-skill" t "2020-01-01")
      (cl-letf (((symbol-function 'pai-memory--skill-dirs) (lambda () (list pskills))))
        (pai-memory-curate)
        ;; sessions in another project do not age it
        (pai-memory-kt--sessions 5 (expand-file-name "elsewhere" dir))
        (should (= (plist-get (pai-memory-curate) :stale) 0))
        (pai-memory-kt--sessions 2 proj)
        (should (= (plist-get (pai-memory-curate) :stale) 1))))))

;;;; Promoter integration

(ert-deftest pai-memory-promoter-gets-flagged-skills ()
  (pai-memory-test--with-owner buf dir
    (let* ((skills (expand-file-name "skills" dir)))
      (cl-letf (((symbol-function 'pai-memory--skill-dirs) (lambda () (list skills))))
        (pai-memory-kt--skill skills "flaky" t)
        (pai-memory-test--turns session 1)
        (pai-memory-record-outcomes
         (list (list :content "skill-used: flaky - failed: path wrong")
               (list :content "skill-used: flaky - deviated: needed sudo")))
        ;; promoted before: the digest is unchanged, but a new candidate makes it due
        (let ((d (pai-memory-session-digest session)))
          (pai-memory--state-update :promotions (pai-session-id session)
                                    (list :hash (plist-get d :hash) :tokens (plist-get d :tokens))))
        (let ((pai-settings--global '(:memory (:long-term (:promote-min-session-tokens 0)))))
          (should (pai-memory-promote-due-p session 'session-end))
          (pai-faux-push '(:tool-calls ((:id "d" :name "done" :arguments (:summary "x")))))
          (pai-memory-promote session :force t)
          (let ((task (pai-content-text (pai-message-content
                                         (cadr (plist-get pai-faux-last-context :messages))))))
            (should (string-match-p "## Skills flagged for review (REQUIRED)" task))
            (should (string-match-p "flaky .*deviated 1, failed 1" task))
            (should (string-match-p "path wrong" task))
            (should (string-match-p "usage: 0 views, 0 uses; outcomes followed 0, deviated 1, failed 1" task)))
          ;; recorded with the candidate: no longer due for it
          (should-not (pai-memory-promote-due-p session 'session-end)))))))

;;;; Commands and wiring

(ert-deftest pai-memory-skill-commands ()
  (pai-memory-kt--with-skills dir skills
    (pai-memory-kt--skill skills "tool-x" t)
    (let ((pin (pai-memory--skill-command (lambda (n) (pai-memory-set-pinned n t)))))
      (should (equal (plist-get (funcall pin "" nil) :message) "Give a skill name"))
      (should (equal (plist-get (funcall pin "tool-x" nil) :message) "Pinned tool-x")))
    (let ((text (pai-memory-skills-status-text)))
      (should (string-match-p "tool-x +learned +active views 0 uses 0" text))
      (should-not (string-match-p "unused" text))
      (should (string-match-p "pinned" text)))))

(ert-deftest pai-memory-committed-observations-count-outcomes ()
  (pai-memory-test--with-owner buf dir
    (let ((skills (expand-file-name "skills" dir)))
      (cl-letf (((symbol-function 'pai-memory--skill-dirs) (lambda () (list skills))))
        (pai-memory-kt--skill skills "ert-batch" t)
        (let ((e (pai-memory-test--turns session 1)))
          (pai-memory-commit-observations session "r1" (plist-get (nth 0 e) :id)
                                          (plist-get (nth 1 e) :id)
                                          (list (list :timestamp "2026-09-22 10:00"
                                                      :content "skill-used: ert-batch - failed: x"))))
        (should (= (plist-get (plist-get (pai-memory-skill-usage "ert-batch") :outcomes) :failed) 1))))))

(provide 'pai-memory-skills-test)
;;; pai-memory-skills-test.el ends here
