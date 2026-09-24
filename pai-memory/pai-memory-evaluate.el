;;; pai-memory-evaluate.el --- Test-run a proposed skill before accepting it -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 C4.  A skill-create or skill-patch proposal may carry a `check':
;; (:task TEXT :criteria TEXT), filed by the promoter or typed in review.
;; `t' in /memory-review runs it:
;;
;;   1. a scratch copy of the project: a detached `git worktree' of HEAD in a
;;      git repository (uncommitted changes are not in it), otherwise a copy
;;      of the directory (refused above `pai-memory-evaluate-max-files'
;;      files), or an empty directory with `:empty t';
;;   2. the proposed skill (and its reference files) written into the copy at
;;      .pai/skills/<name>/;
;;   3. an `evaluator' worker (the budgeted background worker runtime, model
;;      role :memory-evaluator) follows the skill to do the task, with
;;      read/grep/ls/write/edit confined to the copy and bash started in it,
;;      and ends with `verdict' (pass or fail, and why);
;;   4. the result -- pass/fail, notes, transcript path, time -- is saved on
;;      the proposal and shown in review.  It never accepts anything; the
;;      scratch copy is removed.
;;
;; bash is not sandboxed: it starts in the copy but can reach the rest of the
;; system, which is why a run from review always asks first, and proposals
;; with blocking security findings are never run.
;;
;; Automatic runs (`:long-term :auto-test', off by default): after the
;; promoter files proposals, each skill proposal that carries a check and has
;; NO security findings at all (not even warnings) is tested without asking,
;; one at a time, within the memory budget.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-activity)
(require 'pai-memory-settings)
(require 'pai-memory-store)
(require 'pai-memory-worker)
(require 'pai-memory-budget)
(require 'pai-memory-proposals)

(defvar pai--session)
(defvar pai-memory-change-hook)
(declare-function pai-memory-git-root "pai-memory-share" (file))
(declare-function pai-memory--run "pai-memory-share" (program &rest args))

(defconst pai-memory-evaluate-max-files 5000
  "Largest non-git project copied for a skill check.")

(defvar pai-memory-evaluate-finished-functions nil
  "Abnormal hook run with the proposal id and its evaluation plist.")

;;;; Scratch copies

(defun pai-memory--count-files (dir limit)
  "Return the number of files under DIR, stopping past LIMIT."
  (let ((n 0))
    (catch 'done
      (dolist (_f (directory-files-recursively
                   dir "" nil (lambda (d) (and (not (member (file-name-nondirectory d) '(".git" "node_modules")))
                                               (progn (when (> n limit) (throw 'done n)) t)))))
        (cl-incf n)
        (when (> n limit) (throw 'done n))))
    n))

(defun pai-memory-scratch-copy (cwd &optional empty)
  "Return (DIR . CLEANUP) for a scratch copy of project CWD.
CLEANUP is a function removing it.  EMPTY gives an empty directory."
  (let ((cwd (file-name-as-directory (expand-file-name cwd)))
        (root (and (not empty) (fboundp 'pai-memory-git-root) (pai-memory-git-root cwd))))
    (cond
     (empty
      (let ((dir (file-name-as-directory (make-temp-file "pai-check" t))))
        (cons dir (lambda () (delete-directory dir t)))))
     (root
      (let* ((tmp (make-temp-file "pai-check" t))
             (dir (file-name-as-directory (expand-file-name "wt" tmp))))
        (pai-memory--run "git" "-C" root "worktree" "add" "--detach" "--quiet" dir "HEAD")
        (cons (file-name-as-directory (expand-file-name (file-relative-name cwd root) dir))
              (lambda ()
                (ignore-errors (pai-memory--run "git" "-C" root "worktree" "remove" "--force" dir))
                (ignore-errors (delete-directory tmp t))
                (ignore-errors (pai-memory--run "git" "-C" root "worktree" "prune"))))))
     (t
      (when (> (pai-memory--count-files cwd pai-memory-evaluate-max-files) pai-memory-evaluate-max-files)
        (user-error "%s has more than %d files and is not a git repository; check in an empty directory instead"
                    (abbreviate-file-name cwd) pai-memory-evaluate-max-files))
      (let* ((tmp (make-temp-file "pai-check" t))
             (dir (file-name-as-directory (expand-file-name (file-name-nondirectory (directory-file-name cwd)) tmp))))
        (copy-directory cwd dir nil t t)
        (cons dir (lambda () (delete-directory tmp t))))))))

;;;; Worker

(defconst pai-memory-evaluator-system
  "You are testing a proposed skill for a coding assistant before the user accepts it. A skill is a Markdown file of instructions for a recurring task. You are given the skill and a task; do the task by following the skill, in a SCRATCH COPY of the project (your working directory), and report whether the skill worked.

Rules:
- Follow the skill as written. Where it is wrong, missing a step or unclear, note it: that is what this test is for. You may work around a problem to see how far the rest goes, but say so.
- Stay in the working directory. Do not push, publish, deploy, send messages, or change anything outside it, even if the skill says to; stop there and count that step as not tested.
- Keep it short: the smallest run that shows whether the skill works.
- Finish with verdict: pass when the task succeeded by following the skill and the success criteria are met; otherwise fail. Notes: what worked, what did not, and concrete fixes for the skill."
  "System prompt of the skill evaluator.")

(defun pai-memory-evaluator-prompt (p skill-path task criteria)
  "Return the evaluator's task for proposal P, installed at SKILL-PATH."
  (concat
   (format "Skill under test: %s (%s)\nInstalled at: %s\n\n"
           (plist-get p :name) (plist-get p :kind) skill-path)
   "===== BEGIN SKILL =====\n" (plist-get p :after) "\n===== END SKILL =====\n\n"
   (let ((refs (append (plist-get p :references) nil)))
     (when refs
       (concat "Reference files next to it: "
               (mapconcat (lambda (r) (plist-get r :path)) refs ", ") "\n\n")))
   "## Task\n" task "\n\n"
   "## Success criteria\n" (if (string-empty-p (string-trim (or criteria "")))
                               "The task is completed by following the skill."
                             criteria)))

(defun pai-memory-evaluator-tools (dir verdict)
  "Return the evaluator's tools in scratch DIR; the verdict goes to VERDICT's car."
  (append
   (pai-memory-confined-tools dir '("read" "grep" "ls" "write" "edit" "bash"))
   (list (pai-memory-tool
          "verdict" "Report the result and end the run."
          (list :pass (list :type "boolean" :description "true when the skill worked for the task")
                :notes (pai-string-schema "What worked, what did not, and fixes for the skill."))
          '("pass" "notes")
          (lambda (args)
            (setcar verdict (list :pass (eq (plist-get args :pass) t) :notes (or (plist-get args :notes) "")))
            "Recorded.")
          :terminal t))))

(defun pai-memory--save-evaluation (id evaluation)
  "Store EVALUATION on proposal ID (whatever its status now)."
  (let ((p (pai-memory-proposal-load id)))
    (when p
      (pai-memory-proposal-save (plist-put (copy-sequence p) :evaluation evaluation)))
    (run-hook-with-args 'pai-memory-evaluate-finished-functions id evaluation)))

(cl-defun pai-memory-evaluate (id &key task criteria empty)
  "Test-run pending skill proposal ID; return the worker entry.
TASK and CRITERIA default to the proposal's check.  EMPTY checks in an
empty directory instead of a copy of the project."
  (let* ((p (or (pai-memory-proposal-load id) (user-error "No proposal %s" id)))
         (check (plist-get p :check))
         (task (or task (plist-get check :task)))
         (criteria (or criteria (plist-get check :criteria) ""))
         (session (and (boundp 'pai--session) pai--session))
         (reason nil))
    (unless (member (plist-get p :kind) '("skill-create" "skill-patch"))
      (user-error "Only new or changed skills can be test-run"))
    (unless (equal (plist-get p :status) "pending") (user-error "%s is %s" id (plist-get p :status)))
    (unless (seq-empty-p (plist-get p :block))
      (user-error "Not running a skill with blocking security findings: %s"
                  (string-join (append (plist-get p :block) nil) ", ")))
    (when (or (null task) (string-empty-p (string-trim task)))
      (user-error "Give a task to test the skill with"))
    (when (and session (setq reason (pai-memory-budget-exceeded session)))
      (user-error "Memory budget reached (%s)" reason))
    (let* ((scratch (pai-memory-scratch-copy (plist-get p :cwd) empty))
           (dir (car scratch)) (cleanup (cdr scratch))
           (skill-dir (expand-file-name (concat ".pai/skills/" (plist-get p :name) "/") dir))
           (skill-path (expand-file-name "SKILL.md" skill-dir))
           (verdict (list nil)))
      (condition-case err
          (progn
            (pai-memory--write-file skill-path (plist-get p :after))
            (dolist (r (append (plist-get p :references) nil))
              (pai-memory--write-file (expand-file-name (plist-get r :path) skill-dir) (plist-get r :content)))
            (pai-memory--save-evaluation id (list :status "running" :task task :criteria criteria
                                                  :time (format-time-string "%FT%T%z")))
            (pai-memory-worker-launch
             'evaluator
             :system pai-memory-evaluator-system
             :prompt (pai-memory-evaluator-prompt p skill-path task criteria)
             :tools (pai-memory-evaluator-tools dir verdict)
             :cwd dir
             :detail (format "test-run skill %s" (plist-get p :name))
             :timeout (pai-memory-get :long-term :evaluator-timeout session)
             :max-turns 40
             :on-done (lambda (status _messages entry)
                        (unwind-protect
                            (let ((v (car verdict)))
                              (pai-memory--save-evaluation
                               id (list :status (cond ((null v) (if (equal status "completed") "no-verdict" status))
                                                      ((plist-get v :pass) "pass")
                                                      (t "fail"))
                                        :notes (or (plist-get v :notes) "")
                                        :task task :criteria criteria
                                        :transcript (let ((tr (plist-get entry :transcript)))
                                                      (and tr (abbreviate-file-name tr)))
                                        :scratch (if empty "empty directory" "copy of the project")
                                        :time (format-time-string "%FT%T%z"))))
                          (funcall cleanup)
                          (run-hooks 'pai-memory-change-hook)))))
        (error (funcall cleanup)
               (pai-memory--save-evaluation id (list :status "error" :notes (error-message-string err)
                                                     :task task :time (format-time-string "%FT%T%z")))
               (signal (car err) (cdr err)))))))

(defun pai-memory-evaluation-text (ev)
  "Return review text for evaluation EV, or nil."
  (when ev
    (concat
     (format "    Test run: %s"
             (pcase (plist-get ev :status)
               ("pass" "✓ passed") ("fail" "✗ failed") ("running" "running…")
               ("no-verdict" "ended without a verdict") (s s)))
     (let ((tm (plist-get ev :time))) (if tm (format " (%s)" (substring tm 0 (min 16 (length tm)))) ""))
     "\n"
     (format "      task: %s\n" (plist-get ev :task))
     (let ((n (plist-get ev :notes)))
       (when (and n (not (string-empty-p n)))
         (concat "      " (replace-regexp-in-string "\n" "\n      " n) "\n")))
     (let ((tr (plist-get ev :transcript)))
       (when tr (format "      transcript: %s\n" tr))))))

;;;; Automatic runs

(defvar-local pai-memory--auto-test-queue nil
  "Proposal ids waiting for an automatic test run in this buffer.")

(defvar-local pai-memory--auto-testing nil
  "Id of the proposal being tested automatically, or nil.")

(defun pai-memory-auto-test-eligible-p (p)
  "Return non-nil when proposal P may be tested without asking."
  (and (member (plist-get p :kind) '("skill-create" "skill-patch"))
       (equal (plist-get p :status) "pending")
       (let ((task (plist-get (plist-get p :check) :task)))
         (and (stringp task) (not (string-empty-p (string-trim task)))))
       (seq-empty-p (plist-get p :risk))
       (seq-empty-p (plist-get p :block))
       (null (plist-get p :evaluation))))

(defun pai-memory-auto-test-next ()
  "Start the next queued automatic test run, if none is running."
  (unless pai-memory--auto-testing
    (let ((buf (current-buffer)) (started nil))
      (while (and pai-memory--auto-test-queue (not started))
        (let* ((id (pop pai-memory--auto-test-queue))
               (p (pai-memory-proposal-load id)))
          (when (and p (pai-memory-auto-test-eligible-p p))
            (condition-case err
                (progn
                  (setq pai-memory--auto-testing id started t)
                  (pai-memory-evaluate id)
                  ;; a run that finished synchronously has already cleared the flag
                  )
              (error (setq pai-memory--auto-testing nil started nil)
                     (message "pai-memory: automatic test of %s not run: %s" id (error-message-string err))))))
        (unless (buffer-live-p buf) (setq pai-memory--auto-test-queue nil))))))

(defun pai-memory--auto-test-finished (id _evaluation)
  "Continue the queue after proposal ID's run finished."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (equal pai-memory--auto-testing id)
                 (not (equal (plist-get (plist-get (pai-memory-proposal-load id) :evaluation) :status) "running")))
        (setq pai-memory--auto-testing nil)
        (run-at-time 0 nil (lambda (b) (when (buffer-live-p b) (with-current-buffer b (pai-memory-auto-test-next))))
                     buf)))))

(add-hook 'pai-memory-evaluate-finished-functions #'pai-memory--auto-test-finished)

(defun pai-memory-auto-test (proposals)
  "Queue automatic test runs for PROPOSALS when `:auto-test' is on; return how many."
  (let ((session (and (boundp 'pai--session) pai--session)))
    (if (not (pai-truthy (pai-memory-get :long-term :auto-test session)))
        0
      (let ((ids (mapcar (lambda (p) (plist-get p :id))
                         (seq-filter #'pai-memory-auto-test-eligible-p proposals))))
        (setq pai-memory--auto-test-queue (append pai-memory--auto-test-queue ids))
        (pai-memory-auto-test-next)
        (length ids)))))

(provide 'pai-memory-evaluate)
;;; pai-memory-evaluate.el ends here
