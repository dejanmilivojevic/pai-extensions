;;; pai-memory-topics-test.el --- Tests for the project topic tree (V2 B1) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defun pai-memory-tt--session-topic (session name body)
  (let ((dir (pai-memory-session-dir session t)))
    (with-temp-file (expand-file-name name dir)
      (insert (format "---\nid: %s\ntitle: %s\nsummary: about %s\nupdated: now\n---\n%s"
                      (file-name-base name) (file-name-base name) (file-name-base name) body)))))

(defconst pai-memory-tt--build
  "---\nid: build\ntitle: Build\nsummary: How the project builds and tests\nupdated: now\n---\nRun make test; make compile must be clean.\n")

(ert-deftest pai-memory-topic-merge-run ()
  (pai-memory-test--with-owner buf dir
    (pai-memory-tt--session-topic session "build.md" "make test runs 700 tests\n")
    (let ((tree (pai-memory-project-topics-dir (pai-session-cwd session))))
      (should (equal (mapcar #'car (pai-memory-topic-changes session)) '("build.md")))
      (pai-faux-push
       (list :tool-calls
             (list (list :id "r" :name "read"
                         :arguments (list :path (expand-file-name "build.md" (pai-memory-session-dir session))))
                   (list :id "w" :name "write" :arguments (list :path "build.md" :content pai-memory-tt--build))
                   ;; outside the tree, and INDEX.md: refused
                   (list :id "x" :name "write"
                         :arguments (list :path (expand-file-name "evil.md" (pai-memory-session-dir session)) :content "x"))
                   (list :id "i" :name "write" :arguments (list :path "INDEX.md" :content "x"))
                   (list :id "d" :name "done" :arguments '(:summary "merged")))))
      ;; the consolidation hook starts it
      (run-hooks 'pai-memory-consolidated-hook)
      (let ((task (pai-content-text (pai-message-content (cadr (plist-get pai-faux-last-context :messages))))))
        (should (string-match-p "Session topics that changed" task))
        (should (string-match-p "(none yet" task)))
      (let ((text (pai-memory--read-file (expand-file-name "build.md" tree))))
        (should (string-match-p "make compile must be clean" text))
        (should (string-match-p (format "^sources: \\[%s\\]$" (regexp-quote (pai-session-id session))) text)))
      (should-not (file-exists-p (expand-file-name "evil.md" (pai-memory-session-dir session))))
      (should (string-match-p "How the project builds" (pai-memory--read-file (expand-file-name "INDEX.md" tree))))
      ;; merged: nothing changed now
      (should-not (pai-memory-topic-changes session))
      (should (string-match-p "No session topics changed"
                              (plist-get (pai-memory-command "merge-topics" (list :buffer buf)) :message)))
      ;; the snapshot lists the project topics
      (let ((snap (pai-memory-snapshot (list :cwd (pai-session-cwd session)))))
        (should (string-match-p "## Project topics" snap))
        (should (string-match-p "build.md: How the project builds" snap)))
      ;; and search indexes them
      (let ((files (mapcar #'car (pai-memory--markdown-files))))
        (should (member (expand-file-name "build.md" tree) files))))))

(ert-deftest pai-memory-topic-conflict-proposal ()
  (pai-memory-test--with-owner buf dir
    (let ((tree (pai-memory-project-topics-dir (pai-session-cwd session))))
      (make-directory tree t)
      (with-temp-file (expand-file-name "build.md" tree) (insert pai-memory-tt--build))
      (pai-memory-tt--session-topic session "build.md" "tests run with make check\n")
      (let ((both (concat pai-memory-tt--build "conflict:\n- project said: make test\n- session says: make check\n"))
            (resolved (replace-regexp-in-string "make test" "make check" pai-memory-tt--build)))
        (pai-faux-push
         (list :tool-calls
               (list (list :id "w" :name "write" :arguments (list :path "build.md" :content both))
                     (list :id "c" :name "conflict"
                           :arguments (list :topic "build.md" :summary "make test or make check?" :resolution resolved))
                     (list :id "d" :name "done" :arguments '(:summary "one conflict")))))
        (should (string-match-p "Topic merger started"
                                (plist-get (pai-memory-command "merge-topics" (list :buffer buf)) :message)))
        (let ((p (car (pai-memory-proposals "pending"))))
          (should (equal (plist-get p :kind) "topic-conflict"))
          ;; never auto-applied, whatever the policy
          (should (= (pai-memory-auto-apply (list p) 'none) 0))
          (let ((rb (pai-memory-review)))
            (unwind-protect
                (with-current-buffer rb (should (string-match-p "topic conflict in build: make test or make check" (buffer-string))))
              (kill-buffer rb)))
          (should (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok))
          (let ((text (pai-memory--read-file (expand-file-name "build.md" tree))))
            (should (string-match-p "make check" text))
            (should-not (string-match-p "conflict:" text)))
          ;; undo restores both versions
          (should (string-match-p "Undid" (pai-memory-undo)))
          (should (string-match-p "conflict:" (pai-memory--read-file (expand-file-name "build.md" tree)))))))))

(ert-deftest pai-memory-topic-merge-preset-and-budget ()
  (pai-memory-test--with-owner buf dir
    (pai-memory-tt--session-topic session "a.md" "x\n")
    (pai-memory-set-session-state session :preset "economy")
    (should-not (pai-memory-get :session :topic-merge session))
    (should-not (pai-memory-topic-merge session))
    ;; forced it runs even in economy
    (pai-faux-push (list :tool-calls (list (list :id "d" :name "done" :arguments '(:summary "nothing")))))
    (should (pai-memory-topic-merge session t))
    ;; a completed run that wrote nothing still records the merge
    (should-not (pai-memory-topic-changes session))))

(provide 'pai-memory-topics-test)
;;; pai-memory-topics-test.el ends here
