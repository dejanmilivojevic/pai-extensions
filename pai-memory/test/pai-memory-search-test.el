;;; pai-memory-search-test.el --- Tests for memory_search (V2 A1) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-srch--with-home (dir &rest body)
  "Run BODY with a temp pai home DIR, a project cwd and a fresh index."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-srch" t)))
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

(defun pai-memory-srch--session (cwd &rest texts)
  "Create a session for CWD holding user/assistant TEXTS alternately; return it."
  (let ((s (pai-session-new cwd)) (user t))
    (pai-session-append-message s (pai-system-message "SYSTEM PROMPT SECRETIVE"))
    (dolist (text texts)
      (pai-session-append-message s (if user (pai-user-message text)
                                      (pai-assistant-message :content (list (pai-text text)))))
      (setq user (not user)))
    s))

(ert-deftest pai-memory-search-indexes-sessions-and-memory ()
  (pai-memory-srch--with-home dir
    (let ((s (pai-memory-srch--session default-directory
                                       "the build fails with TS2322 in src/auth.ts"
                                       "fixed by narrowing the type")))
      (pai-memory-apply-change '(:action add :target user :content "Prefers ripgrep over grep"))
      (let ((skill (expand-file-name "skills/db/SKILL.md" dir)))
        (make-directory (file-name-directory skill) t)
        (with-temp-file skill (insert "---\nname: db\ndescription: migrations\n---\nRun alembic upgrade head\n")))
      (should (eq (pai-memory-index-update) 'done))
      ;; messages, the system prompt excluded
      (let ((hits (pai-memory-search "TS2322")))
        (should (= (length hits) 1))
        (should (equal (plist-get (car hits) :kind) "message"))
        (should (equal (plist-get (car hits) :role) "user"))
        (should (equal (plist-get (car hits) :session) (pai-session-id s))))
      (should-not (pai-memory-search "SECRETIVE"))
      ;; long-term memory and skills
      (should (equal (plist-get (car (pai-memory-search "ripgrep")) :kind) "memory"))
      (should (equal (plist-get (car (pai-memory-search "alembic")) :kind) "skill"))
      ;; punctuation needs quoting; short terms fall back to LIKE
      (should (pai-memory-search "src/auth.ts"))
      (should (pai-memory-search "TS"))
      ;; kinds filter
      (should-not (pai-memory-search "ripgrep" :kinds '("message"))))))

(ert-deftest pai-memory-search-is-incremental-and-tracks-changes ()
  (pai-memory-srch--with-home dir
    (let ((s (pai-memory-srch--session default-directory "first message about zebras")))
      (pai-memory-index-update)
      (should (= (length (pai-memory-search "zebras")) 1))
      ;; appended lines are picked up; old ones are not indexed twice
      (pai-session-append-message s (pai-user-message "second message about zebras"))
      (pai-memory-index-update)
      (should (= (length (pai-memory-search "zebras" :limit 10)) 2))
      (pai-memory-index-update)
      (should (= (length (pai-memory-search "zebras" :limit 10)) 2))
      ;; observations
      (pai-session-append-custom s "memory.observations"
                                 (list :runId "r" :coversFromId "a" :coversUpToId "b"
                                       :observations (vector (list :id "r.1" :timestamp "2026-09-22 10:00"
                                                                   :content "User adopted giraffes"))))
      (pai-memory-index-update)
      (should (equal (plist-get (car (pai-memory-search "giraffes")) :kind) "observation"))
      ;; a changed Markdown file is re-read, a removed one forgotten
      (pai-memory-apply-change '(:action add :target memory :content "okapi is installed"))
      (pai-memory-index-update)
      (should (pai-memory-search "okapi"))
      (pai-memory-apply-change '(:action replace :target memory :old "okapi" :content "tapir is installed"))
      (pai-memory-index-update)
      (should-not (pai-memory-search "okapi"))
      (should (pai-memory-search "tapir"))
      (delete-file (pai-memory-target-file 'memory default-directory))
      (pai-memory-index-update)
      (should-not (pai-memory-search "tapir"))
      ;; reset rebuilds everything
      (pai-memory-index-reset)
      (pai-memory-index-update)
      (should (= (length (pai-memory-search "zebras" :limit 10)) 2)))))

(ert-deftest pai-memory-search-scopes-and-dedup ()
  (pai-memory-srch--with-home dir
    (let* ((other (file-name-as-directory (expand-file-name "other" dir)))
           (s1 (pai-memory-srch--session default-directory "alpha kiwi one" "alpha kiwi two"
                                         "alpha kiwi three")))
      (make-directory other t)
      (pai-memory-srch--session other "kiwi elsewhere")
      (pai-memory-index-update)
      ;; at most two hits per session
      (should (= (length (pai-memory-search "kiwi" :limit 10)) 2))
      (should (= (length (pai-memory-search "kiwi" :scope "all" :limit 10)) 3))
      (should (= (length (pai-memory-search "kiwi" :scope "session" :session (pai-session-id s1)
                                            :limit 10))
                 2))
      ;; worker transcripts are never indexed
      (let ((w (pai-session-new default-directory
                                (expand-file-name "memory-workers/w.jsonl"
                                                  (pai-session-directory default-directory)))))
        (pai-session-append-message w (pai-user-message "kiwi in a worker")))
      (pai-memory-index-update)
      (should (= (length (pai-memory-search "kiwi" :scope "all" :limit 10)) 3)))))

(ert-deftest pai-memory-search-tool-modes ()
  (pai-memory-srch--with-home dir
    (let* ((s (pai-memory-srch--session default-directory "we chose PostgreSQL 16"
                                        "noted" "then migrated" "done"))
           (run (lambda (args &optional session)
                  (let (res)
                    (pai-memory--tool-execute-search args (list :cwd default-directory :session session)
                                                     nil (lambda (r) (setq res r)))
                    res)))
           (text (lambda (r) (pai-content-text (plist-get r :content)))))
      ;; search (indexes on demand)
      (let ((out (funcall text (funcall run '(:query "PostgreSQL")))))
        (should (string-match-p "1 match(es)" out))
        (should (string-match-p "we chose PostgreSQL 16" out))
        (should (string-match-p "\"session_id\"" out)))
      ;; scroll around the hit
      (let* ((hit (car (pai-memory-search "PostgreSQL")))
             (out (funcall text (funcall run (list :session_id (plist-get hit :session)
                                                   :around (plist-get hit :entry) :window 1)))))
        (should (string-match-p "▶ .*user: we chose PostgreSQL 16" out))
        (should (string-match-p "assistant: noted" out))
        (should-not (string-match-p "then migrated" out)))
      ;; read memory files only
      (pai-memory-apply-change '(:action add :target user :content "Name is Ana"))
      (should (string-match-p "Name is Ana" (funcall text (funcall run '(:path "USER.md")))))
      (should (eq t (plist-get (funcall run (list :path (expand-file-name "auth.json" dir))) :is-error)))
      ;; no query
      (should (eq t (plist-get (funcall run '(:query "")) :is-error)))
      ;; hits on other branches of the current session are labelled
      (let ((first (car (pai-session-get-branch s))))
        (pai-session-branch s (plist-get first :id))
        (pai-session-append-message s (pai-user-message "a different path"))
        (should (string-match-p "(other branch)"
                                (funcall text (funcall run '(:query "PostgreSQL") s))))))))

(ert-deftest pai-memory-search-topic-sections ()
  (pai-memory-srch--with-home dir
    (let* ((s (pai-memory-srch--session default-directory "hello"))
           (tdir (pai-memory-session-dir s t)))
      (with-temp-file (expand-file-name "build.md" tdir)
        (insert "---\nid: build\n---\n# Build\nintro\n## Tests\nmake test runs quokka\n## Deploy\nrsync\n"))
      (with-temp-file (expand-file-name "INDEX.md" tdir) (insert "quokka index"))
      (pai-memory-index-update)
      (let ((hits (pai-memory-search "quokka")))
        (should (= (length hits) 1))
        (should (equal (plist-get (car hits) :kind) "topic"))
        (should (string-prefix-p "## Tests" (plist-get (car hits) :text)))
        (should-not (string-match-p "rsync" (plist-get (car hits) :text))))
      (should (equal (pai-memory--md-sections "a\n# b\nc" "topic") '("a" "# b\nc"))))))

(ert-deftest pai-memory-search-tolerates-odd-lines ()
  "Old sessions saved some notices as bare strings; broken lines are skipped."
  (pai-memory-srch--with-home dir
    (let* ((s (pai-memory-srch--session default-directory "hello"))
           (file (pai-session-file s)))
      (with-temp-buffer
        (insert "{\"id\":\"x1\",\"type\":\"message\",\"message\":\"[subagent sub-1 done] wombat report\"}\n"
                "{\"id\":\"x2\",\"type\":\"message\",\"message\":42}\n"
                "not json at all\n")
        (append-to-file (point-min) (point-max) file))
      (pai-session-append-message s (pai-user-message "after the odd lines numbat"))
      (should (eq (pai-memory-index-update) 'done))
      (should (pai-memory-search "wombat"))
      (should (pai-memory-search "numbat")))))

(defun pai-memory-srch--rows (session)
  "Return how many index rows SESSION has."
  (caar (sqlite-select (pai-memory--db) "SELECT count(*) FROM docs WHERE session=?"
                       (list (pai-session-id session)))))

(ert-deftest pai-memory-search-private-sessions-are-read-once ()
  "A private session is recorded as read: later passes do not re-read it whole.
They used to, every slice, so indexing never finished and kept a core busy."
  (pai-memory-srch--with-home dir
    (let ((s (pai-memory-srch--session default-directory "private walrus plans")))
      (pai-memory-set-session-state s :private t)
      (should (eq (pai-memory-index-update) 'done))
      (should (= (pai-memory-srch--rows s) 0))
      (let* ((file (pai-session-file s))
             (state (pai-memory--file-state (pai-memory--db) file)))
        (should (equal (car state) (file-attribute-size (file-attributes file)))))
      (let ((reads 0))
        (cl-letf* ((orig (symbol-function 'pai-memory-file-private-p))
                   ((symbol-function 'pai-memory-file-private-p)
                    (lambda (f) (cl-incf reads) (funcall orig f))))
          (pai-memory-index-update)
          (pai-memory-index-update)
          (should (= reads 0))
          ;; new lines that do not touch the state: still not re-read, not indexed
          (pai-session-append-message s (pai-user-message "more walrus"))
          (should (eq (pai-memory-index-update) 'done))
          (should (= reads 0))
          (should (= (pai-memory-srch--rows s) 0))
          ;; public again: the whole file is indexed, once
          (pai-memory-set-session-state s :private :false)
          (pai-memory-index-update)
          (should (= reads 1))
          (should (= (pai-memory-srch--rows s) 2))
          (pai-memory-index-update)
          (should (= (pai-memory-srch--rows s) 2)))))))

(ert-deftest pai-memory-search-slices-resume-and-finish ()
  "Slices that run out of time go on where the last one stopped and finish."
  (pai-memory-srch--with-home dir
    (let* ((pai-memory-index-chunk-bytes 300)
           (pai-memory--index-resume nil)
           (sessions (cl-loop for i below 4
                              collect (apply #'pai-memory-srch--session default-directory
                                             (cl-loop for j below 6
                                                      collect (format "slice %d message %d %s" i j
                                                                      (make-string 80 ?x))))))
           (slices 0) (status 'more))
      ;; a zero budget: every slice stops after one chunk
      (while (and (eq status 'more) (< slices 500))
        (setq status (pai-memory-index-update 0) slices (1+ slices)))
      (should (eq status 'done))
      (should (> slices 4))
      (should (= (pai-memory-index-percent) 100))
      (dolist (s sessions) (should (= (pai-memory-srch--rows s) 6)))
      ;; nothing is indexed twice
      (should (eq (pai-memory-index-update) 'done))
      (dolist (s sessions) (should (= (pai-memory-srch--rows s) 6))))))

(ert-deftest pai-memory-search-interrupted-file-is-not-indexed-twice ()
  "An error or a quit after rows went in undoes them with the file's slice."
  (pai-memory-srch--with-home dir
    (let ((s (pai-memory-srch--session default-directory "aardvark one" "aardvark two"))
          (fail 'error))
      (cl-letf* ((orig (symbol-function 'pai-memory--set-file-state))
                 ((symbol-function 'pai-memory--set-file-state)
                  (lambda (db path &rest args)
                    (when (and fail (string-suffix-p ".jsonl" path))
                      (signal fail (list "interrupted")))
                    (apply orig db path args))))
        (pai-memory-index-update)
        (should (= (pai-memory-srch--rows s) 0))
        (setq fail 'quit)
        (should (condition-case nil (progn (pai-memory-index-update) nil) (quit t)))
        (should (= (pai-memory-srch--rows s) 0))
        (setq fail nil)
        (should (eq (pai-memory-index-update) 'done))
        (should (= (pai-memory-srch--rows s) 2))
        (pai-memory-index-update)
        (should (= (pai-memory-srch--rows s) 2))))))

(ert-deftest pai-memory-search-reports-progress ()
  (let ((pai-memory--index-reported nil) (shown '()))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) shown)))
              ((symbol-function 'pai-memory-index-size-mb) (lambda () 1.5)))
      (let ((pai-memory-index-progress '(25 . 100)))
        (pai-memory--index-report 'more)
        ;; throttled to once a second
        (pai-memory--index-report 'more))
      (should (equal shown '("pai-memory: indexing sessions for search… 25%")))
      (let ((pai-memory-index-progress '(100 . 100)))
        (pai-memory--index-report 'done))
      (should (equal (car shown) "pai-memory: search index up to date (1.5 MB)"))
      ;; a pass that never showed progress ends quietly
      (pai-memory--index-report 'done)
      (should (= (length shown) 2)))))

(ert-deftest pai-memory-search-off ()
  (pai-memory-srch--with-home dir
    (setq pai-settings--global '(:memory (:search (:enabled :false))))
    (let (res)
      (pai-memory--tool-execute-search '(:query "x") nil nil (lambda (r) (setq res r)))
      (should (eq (plist-get res :is-error) t)))))

(ert-deftest pai-memory-search-tool-is-deferred ()
  (should (pai-tool-deferred-p pai-memory-search-tool-def)))

(provide 'pai-memory-search-test)
;;; pai-memory-search-test.el ends here
