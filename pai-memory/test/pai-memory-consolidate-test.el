;;; pai-memory-consolidate-test.el --- Tests for pai-memory Phase 2 -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defun pai-memory-ctest--write-call (id path content)
  "Return a scripted `write' tool call."
  (list :id id :name "write" :arguments (list :path path :content content)))

(defun pai-memory-ctest--topic (id title summary body)
  "Return a topic file's text."
  (format "---\nid: %s\ntitle: %s\nsummary: %s\nupdated: 2026-09-22 12:00\n---\n%s\n"
          id title summary body))

(defun pai-memory-ctest--consolidate-response (&rest files)
  "Script one consolidator run writing FILES ((PATH . CONTENT) ...), then confirming."
  (let ((n 0))
    (pai-faux-push
     (list :tool-calls (mapcar (lambda (f) (pai-memory-ctest--write-call
                                            (format "w%d" (cl-incf n)) (car f) (cdr f)))
                               files))
     '(:text "Filed." :stop-reason stop))))

(defun pai-memory-ctest--fill-pool (session n &optional chars)
  "Commit N observations of CHARS characters each to SESSION (one batch per turn)."
  (let ((e (pai-memory-test--turns session n 50)))
    (dotimes (i n)
      (pai-memory-test--commit session (format "r%d" i) (nth (* 2 i) e) (nth (1+ (* 2 i)) e)
                               (format "obs%d %s" i (make-string (or chars 300) ?o))))
    e))

;;;; Topic files and index

(ert-deftest pai-memory-topics-and-index ()
  (let ((dir (file-name-as-directory (make-temp-file "pai-topics" t))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "build.md" dir)
            (insert (pai-memory-ctest--topic "build" "Build" "How to build and test" "make test")))
          (with-temp-file (expand-file-name "auth.md" dir)
            (insert "---\nid: auth\ntitle: \"Auth flow\"\nsummary: 'JWT middleware'\n---\nbody"))
          (with-temp-file (expand-file-name "loose.md" dir) (insert "no front-matter"))
          (with-temp-file (expand-file-name "JOURNEY.md" dir) (insert "\n## 2026\nStarted.\n"))
          (with-temp-file (expand-file-name "observations-archive.md" dir) (insert "x"))
          (let ((topics (pai-memory-topics dir)))
            (should (equal (mapcar (lambda (tp) (plist-get tp :id)) topics)
                           '("auth" "build" "loose")))
            (should (equal (plist-get (car topics) :title) "Auth flow"))
            (should (equal (plist-get (car topics) :summary) "JWT middleware"))
            (should (equal (plist-get (nth 2 topics) :title) "loose")))
          (pai-memory-render-index dir)
          (let ((index (with-temp-buffer (insert-file-contents (expand-file-name "INDEX.md" dir))
                                         (buffer-string))))
            (should (string-match-p "- Build (build.md) — How to build and test" index))
            (should-not (string-match-p "JOURNEY" index))
            (should-not (string-match-p "INDEX" (substring index 10))))
          (should (equal (pai-memory-journey dir) "## 2026\nStarted.")))
      (delete-directory dir t))))

(ert-deftest pai-memory-consolidation-batch-takes-oldest ()
  (let* ((pool (cl-loop for i below 10 collect
                        (list :id (format "o%d" i) :timestamp "t" :content (make-string 100 ?x))))
         (per (pai-memory-observation-tokens (list (car pool))))
         (batch (pai-memory-consolidation-batch pool (* 4 per))))
    (should (equal (mapcar (lambda (o) (plist-get o :id)) batch)
                   '("o0" "o1" "o2" "o3" "o4" "o5")))
    ;; within the target: everything (forced run)
    (should (= (length (pai-memory-consolidation-batch pool 100000)) 10))))

;;;; Consolidator runs

(ert-deftest pai-memory-consolidator-files-and-drops ()
  (pai-memory-test--with-settings '(:preset "custom"
                                    :session (:observe "off" :consolidate t :topic-merge :false
                                              :consolidate-at-pool-tokens 300
                                              :pool-target-tokens 150))
    (pai-memory-test--with-owner buf dir
      (pai-memory-ctest--fill-pool session 5)
      ;; below the threshold nothing starts
      (let ((pai-settings--global '(:memory (:preset "custom"
                                             :session (:consolidate-at-pool-tokens 100000)))))
        (should-not (pai-memory-consolidator-tick)))
      (pai-memory-ctest--consolidate-response
       (cons "build.md" (pai-memory-ctest--topic "build" "Build" "make test runs ERT" "Details."))
       (cons "JOURNEY.md" "## 2026-09-22 12:00\nDuring this period the user set up tests."))
      (should (pai-memory-consolidator-tick))
      (let* ((branch (pai-session-get-branch session))
             (pool (pai-memory-pool branch))
             (dir* (pai-memory-session-dir session))
             (dropped (seq-find (lambda (e) (equal (plist-get e :customType) "memory.dropped"))
                                (pai-session-entries session))))
        ;; the oldest observations left the pool, the newest stayed
        (should dropped)
        (should (member "r0.1" (plist-get (plist-get dropped :data) :ids)))
        (should (< (length pool) 5))
        (should (equal (plist-get (car (last pool)) :id) "r4.1"))
        (should (<= (pai-memory-observation-tokens pool) 150))
        ;; files were written in the confined directory and indexed
        (should (file-exists-p (expand-file-name "build.md" dir*)))
        (should (string-match-p "build.md"
                                (with-temp-buffer
                                  (insert-file-contents (expand-file-name "INDEX.md" dir*))
                                  (buffer-string))))
        ;; the worker was told what to file
        (let ((task (pai-content-text (pai-message-content
                                       (cadr (plist-get pai-faux-last-context :messages))))))
          (should (string-match-p "BEGIN OBSERVATIONS" task))
          (should (string-match-p "obs0" task))
          (should (string-match-p "JOURNEY.md token budget" task)))
        (should-not pai-memory--consolidating)
        (should (= pai-memory--con-failures 0))
        (should (equal (plist-get (car (last (pai-memory-cost-entries session))) :role)
                       "consolidator"))))))

(ert-deftest pai-memory-consolidator-that-writes-nothing-keeps-the-pool ()
  (pai-memory-test--with-settings '(:preset "custom"
                                    :session (:observe "off" :consolidate-at-pool-tokens 100))
    (pai-memory-test--with-owner buf dir
      (pai-memory-ctest--fill-pool session 3)
      (pai-faux-push '(:text "Nothing to file." :stop-reason stop))
      (should (pai-memory-consolidator-tick))
      (should (= (length (pai-memory-pool (pai-session-get-branch session))) 3))
      (should (= pai-memory--con-failures 1))
      ;; backing off
      (should-not (pai-memory-consolidator-tick)))))

(ert-deftest pai-memory-consolidator-cannot-write-generated-files ()
  (let* ((dir (file-name-as-directory (make-temp-file "pai-con" t)))
         (written (list nil))
         (tools (pai-memory-consolidator-tools dir written))
         (write (seq-find (lambda (x) (equal (plist-get x :name) "write")) tools))
         (res nil))
    (unwind-protect
        (progn
          (funcall (plist-get write :execute) (list :path "INDEX.md" :content "x")
                   nil nil (lambda (r) (setq res r)))
          (should (eq (plist-get res :is-error) t))
          (should-not (car written))
          (funcall (plist-get write :execute) (list :path "../escape.md" :content "x")
                   nil nil (lambda (r) (setq res r)))
          (should (eq (plist-get res :is-error) t))
          (should-not (car written))
          (funcall (plist-get write :execute) (list :path "ok.md" :content "x")
                   nil nil (lambda (r) (setq res r)))
          (should-not (eq (plist-get res :is-error) t))
          (should (car written))
          (should (equal (sort (mapcar (lambda (x) (plist-get x :name)) tools) #'string<)
                         '("edit" "grep" "ls" "read" "write"))))
      (delete-directory dir t))))

(ert-deftest pai-memory-consolidator-disabled-and-forced ()
  (pai-memory-test--with-settings '(:preset "custom"
                                    :session (:observe "off" :consolidate :false
                                              :consolidate-at-pool-tokens 10))
    (pai-memory-test--with-owner buf dir
      (pai-memory-ctest--fill-pool session 2)
      (should-not (pai-memory-consolidator-tick))
      ;; /memory consolidate forces a run over the whole pool
      (pai-memory-ctest--consolidate-response (cons "JOURNEY.md" "## t\nStarted."))
      (should (string-match-p "Consolidator started"
                              (plist-get (pai-memory-command "consolidate" (list :buffer buf))
                                         :message)))
      (should-not (pai-memory-pool (pai-session-get-branch session)))
      (should (string-match-p "Nothing to consolidate"
                              (plist-get (pai-memory-command "consolidate" (list :buffer buf))
                                         :message))))))

(ert-deftest pai-memory-observer-commit-triggers-consolidation ()
  (pai-memory-test--with-settings '(:preset "custom"
                                    :session (:chunk-tokens 100000 :observer-concurrency 1
                                              :consolidate-at-pool-tokens 20
                                              :pool-target-tokens 5))
    (pai-memory-test--with-owner buf dir
      (pai-memory-test--turns session 1 200)
      (pai-memory-test--observe-response (concat "User set up the build " (make-string 100 ?b)))
      (pai-memory-ctest--consolidate-response
       (cons "build.md" (pai-memory-ctest--topic "build" "Build" "setup" "x")))
      (should (= (pai-memory-observer-tick t) 1))
      ;; the commit hook started the consolidator, which filed the observation
      (should (file-exists-p (expand-file-name "build.md" (pai-memory-session-dir session))))
      (should-not (pai-memory-pool (pai-session-get-branch session))))))

;;;; Compaction with topics and journey

(ert-deftest pai-memory-compact-renders-map-and-journey ()
  (pai-memory-test--with-settings '(:session (:tail-tokens 10))
    (pai-memory-test--with-session s dir
      (let* ((e (pai-memory-test--turns s 3))
             (mdir (pai-memory-session-dir s t)))
        (with-temp-file (expand-file-name "build.md" mdir)
          (insert (pai-memory-ctest--topic "build" "Build" "make test runs ERT" "x")))
        (with-temp-file (expand-file-name "JOURNEY.md" mdir)
          (insert "## 2026-09-22 12:00\nDuring this period tests were set up."))
        (pai-memory-test--commit s "r1" (nth 0 e) (nth 3 e) "User asked for docs")
        (let* ((res (pai-memory-compact (pai-session-context-messages s) s nil))
               (text (plist-get res :summary)))
          (should (string-match-p "## Memory map" text))
          (should (string-match-p (regexp-quote (abbreviate-file-name mdir)) text))
          (should (string-match-p "- build.md — Build: make test runs ERT" text))
          (should (string-match-p "## Journey\n## 2026-09-22 12:00" text))
          (should (string-match-p "## Observations\n.*User asked for docs" text))
          ;; sections in the specified order
          (should (< (string-match "## Memory map" text) (string-match "## Journey" text)))
          (should (< (string-match "## Journey" text) (string-match "## Observations" text))))))))

;;;; Fork

(ert-deftest pai-memory-fork-carries-ledger-and-topics ()
  (pai-memory-test--with-session s dir
    (let* ((e (pai-memory-test--turns s 2)))
      (pai-memory-test--commit s "r1" (nth 0 e) (nth 1 e) "first")
      (pai-memory-test--commit s "r2" (nth 2 e) (nth 3 e) "second")
      (pai-session-append-custom s "memory.dropped" (list :runId "c" :ids '("r1.1")))
      (pai-session-append-custom s "memory.cost" (list :role "observer" :cost 0.5))
      (pai-memory-set-session-state s :preset "thorough")
      (with-temp-file (expand-file-name "build.md" (pai-memory-session-dir s t))
        (insert "topic"))
      (let* ((fork (pai-session-fork s (pai-session-leaf-id s)))
             (branch (pai-session-get-branch fork)))
        ;; batches carried with ids remapped to the fork's entries
        (should (= (length (pai-memory-batches branch)) 2))
        (should (equal (mapcar (lambda (o) (plist-get o :content)) (pai-memory-pool branch))
                       '("second")))
        (should (equal (plist-get (nth (pai-memory-watermark branch) branch) :id)
                       (plist-get (car (last (seq-filter #'pai-memory-source-entry-p branch))) :id)))
        ;; overrides carried, costs not
        (should (eq (pai-memory-preset fork) 'thorough))
        (should-not (pai-memory-cost-entries fork))
        ;; topics copied into the fork's own directory
        (should (file-exists-p (expand-file-name "build.md" (pai-memory-session-dir fork))))
        (should-not (equal (pai-memory-session-dir fork) (pai-memory-session-dir s)))))))

(ert-deftest pai-memory-fork-before-a-batch-drops-it ()
  "A fork that ends before a batch's covered range does not carry the batch."
  (pai-memory-test--with-session s dir
    (let* ((e (pai-memory-test--turns s 2)))
      (pai-memory-test--commit s "r1" (nth 0 e) (nth 3 e) "all")
      (let ((fork (pai-session-fork s (plist-get (nth 1 e) :id))))
        (should-not (pai-memory-batches (pai-session-get-branch fork)))))))

(provide 'pai-memory-consolidate-test)
;;; pai-memory-consolidate-test.el ends here
