;;; pai-todo-test.el --- Tests for the todo extension -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-todo)

(defconst pai-todo-test--init
  '(:op "init" :list ((:phase "Foundation" :items ("Scaffold crate" "Wire workspace"))
                      (:phase "Verification" :items ("Run tests")))))

(defun pai-todo-test--apply (phases &rest ops)
  "Apply OPS in turn to PHASES; return the last (NEW . ERRORS)."
  (let ((r (cons phases nil)))
    (dolist (op ops) (setq r (pai-todo-apply (car r) op)))
    r))

(defun pai-todo-test--statuses (phases)
  (mapcar (lambda (tk) (cons (plist-get tk :content) (plist-get tk :status)))
          (pai-todo--tasks phases)))

;;;; Operations

(ert-deftest pai-todo-init-promotes-the-first-task ()
  (let ((r (pai-todo-apply nil pai-todo-test--init)))
    (should-not (cdr r))
    (should (equal (pai-todo-test--statuses (car r))
                   '(("Scaffold crate" . "in_progress") ("Wire workspace" . "pending")
                     ("Run tests" . "pending"))))
    ;; flat items make one phase
    (should (equal (plist-get (car (car (pai-todo-apply nil '(:op "init" :items ("a" "b"))))) :name)
                   "Tasks"))))

(ert-deftest pai-todo-done-start-and-auto-advance ()
  (let* ((p (car (pai-todo-apply nil pai-todo-test--init)))
         (p (car (pai-todo-apply p '(:op "done" :task "Scaffold crate")))))
    (should (equal (cdr (assoc "Wire workspace" (pai-todo-test--statuses p))) "in_progress"))
    ;; start moves the pointer; only one in progress
    (setq p (car (pai-todo-apply p '(:op "start" :task "Run tests"))))
    (should (equal (seq-count (lambda (s) (equal (cdr s) "in_progress")) (pai-todo-test--statuses p)) 1))
    (should (equal (cdr (assoc "Run tests" (pai-todo-test--statuses p))) "in_progress"))
    ;; a whole phase
    (setq p (car (pai-todo-apply p '(:op "done" :phase "Foundation"))))
    (should (equal (cdr (assoc "Wire workspace" (pai-todo-test--statuses p))) "completed"))))

(ert-deftest pai-todo-block-unblock-and-reasons ()
  (let* ((p (car (pai-todo-apply nil pai-todo-test--init)))
         (p (car (pai-todo-apply p '(:op "block" :task "Scaffold crate" :reason "waiting on\n  an API key")))))
    (let ((tk (car (pai-todo--find-task p "Scaffold crate"))))
      (should (equal (plist-get tk :status) "blocked"))
      (should (equal (plist-get tk :blocker) "waiting on an API key")))
    ;; blocking the active task hands progress to the next pending, never back
    (should (equal (cdr (assoc "Wire workspace" (pai-todo-test--statuses p))) "in_progress"))
    (setq p (car (pai-todo-apply p '(:op "unblock" :task "Scaffold crate"))))
    (let ((tk (car (pai-todo--find-task p "Scaffold crate"))))
      (should (equal (plist-get tk :status) "pending"))
      (should-not (plist-get tk :blocker)))
    ;; blocking a phase leaves its finished tasks alone
    (setq p (car (pai-todo-test--apply p '(:op "done" :task "Wire workspace")
                                       '(:op "block" :phase "Foundation"))))
    (should (equal (cdr (assoc "Wire workspace" (pai-todo-test--statuses p))) "completed"))
    (should (equal (cdr (assoc "Scaffold crate" (pai-todo-test--statuses p))) "blocked"))))

(ert-deftest pai-todo-append-rm-and-errors ()
  (let ((p (car (pai-todo-apply nil pai-todo-test--init))))
    (setq p (car (pai-todo-apply p '(:op "append" :phase "Docs" :items ("Write README")))))
    (should (equal (plist-get (car (last p)) :name) "Docs"))
    (should (pai-todo--find-task p "Write README"))
    ;; errors change nothing and say why
    (let ((r (pai-todo-apply p '(:op "append" :phase "Docs" :items ("Write README")))))
      (should (equal (cdr r) '("Task \"Write README\" already exists"))))
    (should (string-match-p "referenced by their text"
                            (car (cdr (pai-todo-apply p '(:op "done" :task "task-3"))))))
    (should (string-match-p "Duplicate task"
                            (car (cdr (pai-todo-apply nil '(:op "init" :items ("a" "a")))))))
    (should (cdr (pai-todo-apply p '(:op "done"))))
    (should (cdr (pai-todo-apply p '(:op "wobble"))))
    ;; rm a task, a phase, everything
    (setq p (car (pai-todo-apply p '(:op "rm" :task "Run tests"))))
    (should-not (pai-todo--find-task p "Run tests"))
    (setq p (car (pai-todo-apply p '(:op "rm" :phase "Docs"))))
    (should-not (pai-todo--find-phase p "Docs"))
    (should-not (car (pai-todo-apply p '(:op "rm"))))))

(ert-deftest pai-todo-infers-a-missing-op ()
  (should (equal (pai-todo-infer-op '(:list ((:phase "A" :items ("x")))) t) "init"))
  (should (equal (pai-todo-infer-op '(:items ("x") :phase "A") t) "append"))
  (should (equal (pai-todo-infer-op '(:items ("x")) nil) "init"))
  (should-not (pai-todo-infer-op '(:items ("x")) t))   ; would overwrite a list
  (should-not (pai-todo-infer-op '(:task "x") nil)))

(ert-deftest pai-todo-summary-text ()
  (let* ((p (car (pai-todo-test--apply nil pai-todo-test--init
                                       '(:op "done" :task "Scaffold crate")
                                       '(:op "block" :task "Run tests" :reason "CI down"))))
         (s (pai-todo-summary p)))
    (should (string-match-p "^Remaining items (1):\n  - Wire workspace \\[in_progress\\] (Foundation)" s))
    (should (string-match-p "^Overall: 1/3 done, 1 open, 1 blocked\\.$" s))
    (should (string-match-p "^Active phase 1/2 \"Foundation\" (1/2)\\.$" s))
    (should (string-match-p "- \\[X\\] Scaffold crate$" s))
    (should (string-match-p "Run tests (blocked: CI down)" s))
    (should (equal (pai-todo-summary nil) "Todo list cleared."))
    (should (equal (pai-todo-summary nil nil t) "Todo list is empty."))))

;;;; Tool

(defun pai-todo-test--run-tool (session args)
  (let (res)
    (pai-todo--execute args (list :session session) nil (lambda (r) (setq res r)))
    res))

(ert-deftest pai-todo-tool-keeps-state-and-fails-atomically ()
  (let* ((pai-directory (make-temp-file "pai-todo" t))
         (s (pai-session-new pai-directory 'memory)))
    (unwind-protect
        (progn
          (let ((r (pai-todo-test--run-tool s '(:list [(:phase "A" :items ["one" "two"])]))))
            (should-not (eq (plist-get r :is-error) t))          ; op inferred, vectors ok
            (should (equal (plist-get (plist-get r :details) :op) "init")))
          (let ((r (pai-todo-test--run-tool s '(:op "done" :task "nope"))))
            (should (eq (plist-get r :is-error) t))
            (should (string-match-p "Task \"nope\" not found" (pai-content-text (plist-get r :content)))))
          (should (= (length (pai-todo--tasks (pai-todo-phases s))) 2))
          (let ((r (pai-todo-test--run-tool s '(:op "view"))))
            (should (equal (plist-get (plist-get r :details) :op) "view"))
            (should (string-match-p "Overall: 0/2" (pai-content-text (plist-get r :content)))))
          (should (eq (plist-get (pai-todo-test--run-tool s '(:task "one")) :is-error) t)))
      (delete-directory pai-directory t))))

(ert-deftest pai-todo-state-comes-from-the-branch ()
  "The list is the latest todo result or /todo edit on the current branch."
  (let* ((pai-directory (make-temp-file "pai-todo" t))
         (file (expand-file-name "s.jsonl" pai-directory))
         (s (pai-session-new pai-directory file)))
    (unwind-protect
        (let* ((u (pai-session-append-message s (pai-user-message "go")))
               (p1 (car (pai-todo-apply nil '(:op "init" :items ("one" "two"))))))
          (pai-session-append-message
           s (pai-tool-result-message :tool-call-id "t1" :tool-name "todo" :content "ok"
                                      :details (list :op "init" :phases p1)))
          ;; a view and a failed call do not count
          (pai-session-append-message
           s (pai-tool-result-message :tool-call-id "t2" :tool-name "todo" :content "v"
                                      :details (list :op "view" :phases nil)))
          (pai-session-append-message
           s (pai-tool-result-message :tool-call-id "t3" :tool-name "todo" :content "x" :is-error t
                                      :details (list :phases nil)))
          (should (equal (pai-todo-test--statuses (pai-todo--from-branch s))
                         '(("one" . "in_progress") ("two" . "pending"))))
          ;; a user edit
          (pai-session-append-custom s "todo" (list :op "edit" :phases (car (pai-todo-apply p1 '(:op "done" :task "one")))))
          (should (equal (cdr (assoc "one" (pai-todo-test--statuses (pai-todo--from-branch s)))) "completed"))
          ;; from the saved file, too
          (should (equal (pai-todo-test--statuses (pai-todo--from-branch (pai-session-load file)))
                         '(("one" . "completed") ("two" . "in_progress"))))
          ;; another branch has no list
          (pai-session-branch s (plist-get u :id))
          (pai-session-append-message s (pai-user-message "other way"))
          (should-not (pai-todo--from-branch s))
          ;; a cleared list survives the JSON round trip
          (pai-session-append-custom s "todo" (list :op "edit" :phases nil))
          (should-not (pai-todo--from-branch (pai-session-load file))))
      (delete-directory pai-directory t))))

;;;; Org

(ert-deftest pai-todo-org-round-trip ()
  (let* ((p (car (pai-todo-test--apply nil pai-todo-test--init
                                       '(:op "done" :task "Scaffold crate")
                                       '(:op "block" :task "Run tests" :reason "CI down")
                                       '(:op "drop" :task "Wire workspace"))))
         (org (pai-todo-to-org p)))
    (should (string-match-p "^\\* Foundation\n\\*\\* DONE Scaffold crate\n\\*\\* DROPPED Wire workspace" org))
    (should (string-match-p "^\\*\\* BLOCKED Run tests\n   Blocker: CI down$" org))
    (let ((back (pai-todo-from-org org)))
      (should-not (cdr back))
      (should (equal (car back) p))))
  ;; checkboxes, tags, notes; unknown lines are errors
  (let ((r (pai-todo-from-org "- [X] a\n- [ ] b\n* P\n** c  :tag:\n   a note\n")))
    (should-not (cdr r))
    (should (equal (pai-todo-test--statuses (car r))
                   '(("a" . "completed") ("b" . "in_progress") ("c" . "pending")))))
  (should (cdr (pai-todo-from-org "* P\nstray text\n")))
  (should (cdr (pai-todo-from-org "** a\n** a\n"))))

;;;; /todo

(defmacro pai-todo-test--with-session (&rest body)
  "Run BODY in a buffer with a session (`s') and no widgets."
  `(let* ((pai-directory (make-temp-file "pai-todo" t))
          (s (pai-session-new pai-directory 'memory)))
     (unwind-protect
         (with-temp-buffer
           (setq-local pai--session s)
           (cl-letf (((symbol-function 'pai-todo--refresh-widgets) #'ignore))
             ,@body))
       (delete-directory pai-directory t))))

(defun pai-todo-test--cmd (args)
  (plist-get (pai-todo-command args (list :buffer (current-buffer))) :message))

(ert-deftest pai-todo-command-changes-the-list ()
  (pai-todo-test--with-session
   (should (equal (pai-todo-test--cmd "") "The todo list is empty."))
   (pai-todo-test--run-tool s pai-todo-test--init)
   ;; fuzzy, case-insensitive
   (pai-todo-test--cmd "done scaffold")
   (should (equal (cdr (assoc "Scaffold crate" (pai-todo-test--statuses (pai-todo-phases s)))) "completed"))
   ;; no argument: the task in progress
   (pai-todo-test--cmd "done")
   (should (equal (cdr (assoc "Wire workspace" (pai-todo-test--statuses (pai-todo-phases s)))) "completed"))
   (should (string-match-p "No task or phase matches" (pai-todo-test--cmd "drop zzz")))
   (should (string-match-p "/todo clear" (pai-todo-test--cmd "rm")))
   ;; append to a named phase, or to the current task's phase
   (pai-todo-test--cmd "append foundation: Add CI")
   (should (equal (plist-get (cdr (pai-todo--find-task (pai-todo-phases s) "Add CI")) :name) "Foundation"))
   (pai-todo-test--cmd "append Fix flaky test")
   (should (equal (plist-get (cdr (pai-todo--find-task (pai-todo-phases s) "Fix flaky test")) :name)
                  "Verification"))   ; "Run tests" is in progress there
   ;; edits are session entries, so they survive /tree and /resume
   (should (equal (plist-get (car (last (pai-session-entries s))) :customType) "todo"))
   (pai-todo--forget s)
   (should (pai-todo--find-task (pai-todo-phases s) "Fix flaky test"))
   (should (equal (pai-todo-test--cmd "clear") "Todo list cleared"))
   (should-not (pai-todo-phases s))
   (should (string-match-p "Usage: /todo" (pai-todo-test--cmd "wobble")))))

(ert-deftest pai-todo-command-export-import ()
  (pai-todo-test--with-session
   (let ((default-directory (file-name-as-directory pai-directory)))
     (pai-todo-test--run-tool s pai-todo-test--init)
     (should (string-match-p "Wrote 3 tasks to .*TODO.org" (pai-todo-test--cmd "export")))
     (pai-todo-test--cmd "clear")
     (should (string-match-p "Imported 3 tasks" (pai-todo-test--cmd "import")))
     (should (= (length (pai-todo--tasks (pai-todo-phases s))) 3))
     (should (string-match-p "No file" (pai-todo-test--cmd "import nope.org"))))))

(ert-deftest pai-todo-command-reminders-setting ()
  (let* ((pai-directory (make-temp-file "pai-todo" t))
         (pai-settings--global nil) (pai-settings--project nil))
    (unwind-protect
        (with-temp-buffer
          (setq-local pai--session (pai-session-new pai-directory 'memory))
          (should (pai-todo-reminders-p))
          (should (equal (pai-todo-test--cmd "reminders off") "Todo reminders off"))
          (should-not (pai-todo-reminders-p))
          (pai-todo-test--cmd "reminders on")
          (should (pai-todo-reminders-p)))
      (delete-directory pai-directory t))))

(ert-deftest pai-todo-every-subcommand-completes ()
  (dolist (w '("show" "start" "done" "drop" "block" "unblock" "rm" "append"
               "clear" "edit" "export" "import" "reminders"))
    (should (member w pai-todo-subcommands))
    (should (string-match-p (concat "\\b" w "\\b") (pai-todo--dispatch nil "bogus" "")))))

(ert-deftest pai-todo-completes-task-names-with-spaces ()
  "Task names complete as one argument, however many words are typed."
  (let* ((pai-directory (make-temp-file "pai-todo" t))
         (pai-default-model "faux")
         (buf (get-buffer-create (generate-new-buffer-name "*pai-todo-test*"))))
    (unwind-protect
        (with-current-buffer buf
          (setq default-directory (file-name-as-directory pai-directory))
          (pai--setup pai-directory)
          (load "pai-todo" nil t)
          (pai-todo-test--run-tool pai--session pai-todo-test--init)
          (let ((at (lambda (text)
                      (goto-char (point-max))
                      (let ((inhibit-read-only t)) (delete-region pai--input-marker (point-max)))
                      (insert text)
                      (let ((c (pai-arg-completion-at-point)))
                        (list (buffer-substring-no-properties (nth 0 c) (nth 1 c)) (nth 2 c))))))
            (should (equal (cadr (funcall at "/todo done "))
                           '("Scaffold crate" "Wire workspace" "Run tests" "Foundation" "Verification")))
            ;; two words typed: the whole "wire wo" is replaced, case ignored
            (should (equal (funcall at "/todo done wire wo") '("wire wo" ("Wire workspace"))))
            (should (equal (cadr (funcall at "/todo start ")) '("Wire workspace" "Run tests")))
            (should (equal (cadr (funcall at "/todo append ")) '("Foundation: " "Verification: ")))
            (should (equal (cadr (funcall at "/todo reminders ")) '("on" "off")))))
      (kill-buffer buf)
      (delete-directory pai-directory t))))

;;;; End to end: the agent keeps a list; the status bar and reminders follow

(ert-deftest pai-todo-agent-run-widget-and-reminder ()
  (pai-faux-reset)
  (pai-faux-push
   '(:tool-calls ((:id "c1" :name "todo"
                   :arguments (:op "init" :list ((:phase "Work" :items ("First step" "Second step"))))))
     :stop-reason tool-use)
   '(:text "Started." :stop-reason stop)
   ;; reminded: acts on it
   '(:tool-calls ((:id "c2" :name "todo" :arguments (:op "done" :task "First step")))
     :stop-reason tool-use)
   '(:text "Done with the first." :stop-reason stop)
   ;; reminded again (2/2): stops without acting
   '(:text "I will continue later." :stop-reason stop))
  (let* ((pai-directory (make-temp-file "pai-todo" t))
         (pai-default-model "faux")
         (pai-settings--global nil) (pai-settings--project nil)
         (buf (get-buffer-create (generate-new-buffer-name "*pai-todo-test*"))))
    (unwind-protect
        (with-current-buffer buf
          (setq default-directory (file-name-as-directory pai-directory))
          (pai--setup pai-directory)
          (load "pai-todo" nil t)
          (goto-char (point-max)) (insert "do the thing") (pai-send)
          (should (string-match-p "⚙ todo" (buffer-string)))
          (should (equal (cdr (assoc "todo" pai--widgets)) "☑ 0/2 · First step"))
          ;; the reminder comes from a timer after the run settled
          (dotimes (_ 5) (accept-process-output nil 0.02))
          (should (string-match-p "\\[todo reminder 1/2\\] You stopped with 2 open todo items:\n- Work\n  - First step\n  - Second step"
                                  (buffer-string)))
          (should (equal (cdr (assoc "todo" pai--widgets)) "☑ 1/2 · Second step"))
          (dotimes (_ 5) (accept-process-output nil 0.02))
          (should (string-match-p "\\[todo reminder 2/2\\]" (buffer-string)))
          ;; the budget is spent: no third reminder
          (dotimes (_ 5) (accept-process-output nil 0.02))
          (should-not (string-match-p "\\[todo reminder 3" (buffer-string)))
          (should-not pai--active)
          ;; the list is in the session file
          (should (equal (pai-todo-test--statuses (pai-todo--from-branch (pai-session-load (pai-session-file pai--session))))
                         '(("First step" . "completed") ("Second step" . "in_progress")))))
      (kill-buffer buf)
      (delete-directory pai-directory t))))

(ert-deftest pai-todo-reminder-not-after-a-question-or-when-off ()
  (let* ((pai-directory (make-temp-file "pai-todo" t))
         (pai-settings--global nil) (pai-settings--project nil))
    (unwind-protect
        (pai-todo-test--with-session
         (setq-local pai--active nil)
         (setq-local pai--steering-queue nil)
         (pai-todo-test--run-tool s pai-todo-test--init)
         (setq-local pai--context-messages
                     (list (pai-assistant-message :content (list (pai-text "Working on it.")))))
         (should (pai-todo--reminder-due))
         (setq pai--context-messages
               (list (pai-assistant-message :content (list (pai-text "Plan ready.\nShould I continue?")))))
         (should-not (pai-todo--reminder-due))
         (setq pai--context-messages
               (list (pai-assistant-message :content (list (pai-text "x")) :stop-reason 'aborted)))
         (should-not (pai-todo--reminder-due))
         (setq pai--context-messages (list (pai-assistant-message :content (list (pai-text "x")))))
         (pai-todo--set-setting :reminders :false)
         (should-not (pai-todo--reminder-due))
         (pai-todo--set-setting :reminders t)
         ;; blocked-only lists do not remind
         (pai-todo-test--run-tool s '(:op "block" :phase "Foundation"))
         (pai-todo-test--run-tool s '(:op "block" :phase "Verification"))
         (should-not (pai-todo--reminder-due)))
      (delete-directory pai-directory t))))

(provide 'pai-todo-test)
;;; pai-todo-test.el ends here
