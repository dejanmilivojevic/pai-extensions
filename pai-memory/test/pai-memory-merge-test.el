;;; pai-memory-merge-test.el --- Tests for merging learned skills (V2 C1) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-mt--with-skills (dir skills &rest body)
  (declare (indent 2))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-mrg" t)))
          (pai-directory ,dir)
          (,skills (expand-file-name "skills" ,dir))
          (default-directory ,dir)
          (pai-settings--global nil) (pai-settings--project nil))
     (cl-letf (((symbol-function 'pai-memory--skill-dirs) (lambda () (list ,skills))))
       (unwind-protect (progn ,@body) (delete-directory ,dir t)))))

(defun pai-memory-mt--skill (skills name desc &optional learned extra)
  "Write skill NAME with DESC under SKILLS; return its file."
  (let ((file (expand-file-name (format "%s%s/SKILL.md" (if learned "learned/" "") name) skills)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (format "---\nname: %s\ndescription: %s\n%s%s---\n1. step for %s\n"
                      name desc (if learned "origin: learned\n" "") (or extra "") name)))
    file))

(defconst pai-memory-mt--umbrella
  "---\ndescription: Use when running or debugging pai's ERT tests\n---\n# When to use\nTests.\n# Steps\n1. make test\n2. rerun one test with ert-run-tests-batch\n")

(ert-deftest pai-memory-merge-clusters ()
  (pai-memory-mt--with-skills dir skills
    (pai-memory-mt--skill skills "ert-batch" "Run the ERT test suite in batch mode" t)
    (pai-memory-mt--skill skills "ert-debug" "Debug one failing ERT test" t)
    (pai-memory-mt--skill skills "deploy-site" "Deploy the website with rsync" t)
    (pai-memory-mt--skill skills "ert-manual" "Hand-written ERT notes")
    (let ((clusters (pai-memory-merge-clusters)))
      (should (= (length clusters) 1))
      (should (equal (sort (mapcar (lambda (s) (plist-get s :name)) (car clusters)) #'string<)
                     '("ert-batch" "ert-debug"))))
    ;; pinned skills are left alone
    (pai-memory-set-pinned "ert-debug" t)
    (should-not (pai-memory-merge-clusters))
    (should (string-match-p "No overlapping" (pai-memory-merge :force t)))))

(ert-deftest pai-memory-merge-apply-and-undo ()
  (pai-memory-mt--with-skills dir skills
    (let ((a (pai-memory-mt--skill skills "ert-batch" "Run the ERT suite" t))
          (b (pai-memory-mt--skill skills "ert-debug" "Debug one ERT test" t))
          (c (pai-memory-mt--skill skills "release" "Cut a release" t "related: [ert-debug, other]\n")))
      (pai-memory-record-view "ert-batch")
      (pai-memory-record-use "ert-debug") (pai-memory-record-use "ert-debug")
      ;; validation
      (should-error (pai-memory-make-merge-proposal :sources '("ert-batch") :name "ert" :rationale "r"
                                                    :cwd dir)
                    :type 'user-error)
      (should-error (pai-memory-make-merge-proposal :sources '("ert-batch" "nope") :name "ert"
                                                    :rationale "r" :cwd dir)
                    :type 'user-error)
      (should-error (pai-memory-make-merge-proposal :sources '("ert-batch" "ert-debug") :name "release"
                                                    :content pai-memory-mt--umbrella :rationale "r" :cwd dir)
                    :type 'user-error)
      (let* ((p (pai-memory-add-proposal
                 (pai-memory-make-merge-proposal :sources '("ert-batch" "ert-debug") :name "ert-batch"
                                                 :content pai-memory-mt--umbrella :rationale "same topic"
                                                 :cwd dir)))
             (r (pai-memory-proposal-accept (plist-get p :id))))
        (should (plist-get r :ok))
        ;; umbrella written into the existing source, the other archived
        (should (string-match-p "rerun one test" (pai-memory--read-file a)))
        (should (string-match-p "^origin: learned$" (pai-memory--read-file a)))
        (should-not (file-exists-p b))
        (should (file-exists-p (expand-file-name "skill-archive/ert-debug/SKILL.md" (pai-memory-dir))))
        ;; related links point to the umbrella; usage moved over
        (should (string-match-p "^related: \\[ert-batch, other\\]$" (pai-memory--read-file c)))
        (should (= (plist-get (pai-memory-skill-usage "ert-batch") :uses) 2))
        (should (= (plist-get (pai-memory-skill-usage "ert-batch") :views) 1))
        (should (equal (plist-get (pai-memory-skill-usage "ert-debug") :merged_into) "ert-batch"))
        ;; a backup exists
        (should (directory-files-recursively (pai-memory-dir "backups") "SKILL\\.md\\'"))
        ;; one undo reverts everything
        (should (string-match-p "Undid" (pai-memory-undo)))
        (should (string-match-p "step for ert-batch" (pai-memory--read-file a)))
        (should (file-exists-p b))
        (should (string-match-p "related: \\[ert-debug, other\\]" (pai-memory--read-file c)))
        (should (= (plist-get (pai-memory-skill-usage "ert-debug") :uses) 2))
        (should-not (plist-get (pai-memory-skill-usage "ert-batch") :uses))))))

(ert-deftest pai-memory-merge-new-umbrella-and-stale ()
  (pai-memory-mt--with-skills dir skills
    (let ((a (pai-memory-mt--skill skills "ert-batch" "Run the ERT suite" t))
          (b (pai-memory-mt--skill skills "ert-debug" "Debug one ERT test" t)))
      (let ((p (pai-memory-make-merge-proposal :sources '("ert-batch" "ert-debug") :name "ert-tests"
                                               :content pai-memory-mt--umbrella :rationale "r" :cwd dir)))
        (pai-memory-add-proposal p)
        ;; a source changed after the proposal: refused and stale
        (with-temp-file b (insert "---\nname: ert-debug\ndescription: edited\norigin: learned\n---\nx\n"))
        (should (string-match-p "stale" (plist-get (pai-memory-proposal-accept (plist-get p :id)) :error)))
        (should (equal (plist-get (pai-memory-proposal-load (plist-get p :id)) :status) "stale")))
      (let ((p (pai-memory-add-proposal
                (pai-memory-make-merge-proposal :sources '("ert-batch" "ert-debug") :name "ert-tests"
                                                :content pai-memory-mt--umbrella :rationale "r" :cwd dir))))
        (should (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok))
        (should (file-exists-p (expand-file-name "learned/ert-tests/SKILL.md" skills)))
        (should-not (file-exists-p a))
        (should-not (file-exists-p b))
        (pai-memory-undo)
        (should-not (file-exists-p (expand-file-name "learned/ert-tests/SKILL.md" skills)))
        (should (file-exists-p a))
        (should (file-exists-p b))))))

(ert-deftest pai-memory-archive-proposal ()
  (pai-memory-mt--with-skills dir skills
    (let ((a (pai-memory-mt--skill skills "old-thing" "Something obsolete" t)))
      (pai-memory-mt--skill skills "manual" "Hand-written")
      (should-error (pai-memory-make-archive-proposal :name "manual" :reason "r" :cwd dir) :type 'user-error)
      (let ((p (pai-memory-add-proposal (pai-memory-make-archive-proposal :name "old-thing" :reason "unused"
                                                                          :cwd dir))))
        ;; never applied by a review policy
        (should (= (pai-memory-auto-apply (list p) 'none) 0))
        (should (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok))
        (should-not (file-exists-p a))
        (should (member "old-thing" (pai-memory-archived-skills)))
        (pai-memory-undo)
        (should (file-exists-p a))))))

(ert-deftest pai-memory-merger-run ()
  (pai-memory-test--with-owner buf dir
    (let ((skills (expand-file-name "skills" dir)))
      (cl-letf (((symbol-function 'pai-memory--skill-dirs) (lambda () (list skills))))
        (pai-memory-mt--skill skills "ert-batch" "Run the ERT test suite in batch mode" t)
        (pai-memory-mt--skill skills "ert-debug" "Debug one failing ERT test" t)
        (pai-faux-push
         (list :tool-calls
               (list (list :id "m" :name "propose_merge"
                           :arguments (list :sources ["ert-batch" "ert-debug"] :name "ert-tests"
                                            :content pai-memory-mt--umbrella :rationale "overlap"))
                     (list :id "d" :name "done" :arguments '(:summary "one merge")))))
        (should (string-match-p "Merger started on 1 group" (plist-get (pai-memory-command "merge" (list :buffer buf)) :message)))
        (let ((task (pai-content-text (pai-message-content (cadr (plist-get pai-faux-last-context :messages))))))
          (should (string-match-p "## Group 1" task))
          (should (string-match-p "ert-debug: Debug one failing" task)))
        (let ((pending (pai-memory-proposals "pending")))
          (should (= (length pending) 1))
          (should (equal (plist-get (car pending) :kind) "skill-merge")))
        (should (plist-get (pai-memory-state-read) :merge_last))
        (should (equal (plist-get (car (last (pai-memory-cost-entries session))) :role) "merger"))
        ;; the review buffer describes it
        (let ((rb (pai-memory-review)))
          (unwind-protect
              (with-current-buffer rb
                (should (string-match-p "merge ert-batch, ert-debug → ert-tests" (buffer-string)))
                (pai-memory-review-toggle)
                (should (string-match-p "Archives: ert-batch, ert-debug" (buffer-string))))
            (kill-buffer rb)))))))

(ert-deftest pai-memory-merge-due ()
  (pai-memory-mt--with-skills dir skills
    (should-not (pai-memory-merge-due-p))
    (setq pai-settings--global '(:memory (:long-term (:merge-interval-days 7))))
    (should (pai-memory-merge-due-p))
    (pai-memory-state-write (plist-put (pai-memory-state-read) :merge_last (pai-memory--now)))
    (should-not (pai-memory-merge-due-p))))

(provide 'pai-memory-merge-test)
;;; pai-memory-merge-test.el ends here
