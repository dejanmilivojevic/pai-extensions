;;; pai-todo.el --- A phased todo list the agent keeps while it works -*- lexical-binding: t; -*-

;; A port of oh-my-pi's `todo' tool (packages/coding-agent/src/tools/todo.ts,
;; session/todo-tracker.ts and the /todo slash command).

;;; Commentary:

;; The agent tracks multi-step work in a phased todo list with the `todo'
;; tool; you see it in the status bar and change it with `/todo'.
;;
;; Model: phases (named groups, in order) of tasks, each with a status:
;;   pending  in_progress  completed  abandoned  blocked (+ optional reason)
;; Tasks and phases are addressed by their exact text, never by ids.
;;
;; Tool `todo', one operation per call (as upstream):
;;   init    {list: [{phase, items}]} or {items} -- replace the whole list
;;   start   {task}                             -- mark in progress
;;   done    {task} or {phase}                  -- mark completed
;;   drop    {task} or {phase}                  -- mark abandoned
;;   block   {task} or {phase}, reason?         -- waiting on something external
;;   unblock {task} or {phase}                  -- blocked -> pending
;;   rm      {task} or {phase}, or nothing      -- remove (nothing: clear)
;;   append  {phase, items}                     -- add tasks (creates the phase)
;;   view                                       -- read-only
;; After every change exactly one task is in progress: the earliest pending
;; one is promoted when none is.  A call with any error changes nothing.  A
;; missing `op' is inferred when the arguments leave no doubt.
;;
;; State lives in the session: the latest successful `todo' result on the
;; current branch (its :details carry the phases), or the latest edit made
;; with `/todo' (a `custom' entry of type "todo").  So /resume, /tree and
;; forks bring back the list that belongs to that point of the conversation.
;;
;; Reminder: when a run stops with open tasks, the agent is sent a reminder
;; listing them (at most `:reminders-max' times per prompt of yours).  Not
;; after an interrupt or error, not when its answer ends with a question to
;; you, and not again while it has not acted on the previous one.
;;
;; Panel: above the input line, like oh-my-pi's todo HUD -- the active
;; phase with its open tasks, later phases as one line each (see "Panel
;; above the prompt" below).  The status-bar widget shows when it does not.
;;
;; /todo: show | start|done|drop|block|unblock|rm TASK-or-PHASE |
;;        append [PHASE:] TASK | clear | edit | export [FILE] | import [FILE] |
;;        expand | collapse | hud on|off | reminders on|off.  Task and phase names complete, spaces and all.
;; `/todo edit' opens the list as an Org outline (C-c C-c saves); export and
;; import use the same format (default file TODO.org).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-tools)
(require 'pai-ext)
(require 'pai-commands)
(require 'pai-settings)
(require 'pai-session)

(declare-function pai--set-widget "pai-ui" (key content))
(declare-function pai--set-panel "pai-ui" (key text))
(declare-function pai--render-note "pai-ui" (text &optional face))
(declare-function pai--start-run "pai-ui" (text))
(declare-function pai-settings-ui-register-section "pai-settings-ui")
(declare-function pai-settings-ui-register-subsection "pai-settings-ui")
(declare-function pai-settings-ui-register-item "pai-settings-ui")
(defvar pai--session)
(defvar pai--active)
(defvar pai--steering-queue)
(defvar pai--context-messages)

(defconst pai-todo-statuses '("pending" "in_progress" "completed" "abandoned" "blocked")
  "The statuses a task can have.")

(defconst pai-todo-ops '("init" "start" "done" "drop" "block" "unblock" "rm" "append" "view")
  "The operations of the `todo' tool.")

(defconst pai-todo-default-phase "Tasks"
  "Phase of a flat `init' or `append' without a phase.")

;;;; Settings

(defun pai-todo--setting (key default)
  "Return todo setting KEY (from the :todo settings plist), or DEFAULT."
  (let ((s (pai-settings-get :todo)))
    (if (and (listp s) (plist-member s key)) (plist-get s key) default)))

(defun pai-todo--set-setting (key value)
  "Set todo setting KEY to VALUE in the global settings."
  (pai-settings-set :todo (plist-put (copy-sequence (or (pai-settings-get :todo) '())) key value)
                    'global))

(defun pai-todo-reminders-p ()
  "Return non-nil when stop-time reminders are on."
  (pai-truthy (pai-todo--setting :reminders t)))

(defun pai-todo-reminders-max ()
  "Return how many reminders one prompt may get."
  (let ((n (pai-todo--setting :reminders-max 2)))
    (if (numberp n) (max 0 n) 2)))

;;;; Data

(defun pai-todo--list (x)
  "Return X (a list or vector) as a list."
  (if (vectorp x) (append x nil) x))

(defun pai-todo-copy (phases)
  "Return a deep copy of PHASES, normalized to plists of strings."
  (mapcar (lambda (ph)
            (list :name (format "%s" (plist-get ph :name))
                  :tasks (mapcar (lambda (tk)
                                   (let ((out (list :content (format "%s" (plist-get tk :content))
                                                    :status (format "%s" (or (plist-get tk :status) "pending")))))
                                     (when (and (stringp (plist-get tk :blocker))
                                                (not (string-empty-p (plist-get tk :blocker))))
                                       (setq out (append out (list :blocker (plist-get tk :blocker)))))
                                     out))
                                 (pai-todo--list (plist-get ph :tasks)))))
          (pai-todo--list phases)))

(defun pai-todo--tasks (phases)
  "Return every task of PHASES, in order."
  (apply #'append (mapcar (lambda (ph) (plist-get ph :tasks)) phases)))

(defun pai-todo--status (task) (plist-get task :status))

(defun pai-todo--open-p (task)
  "Return non-nil when TASK is pending or in progress."
  (member (pai-todo--status task) '("pending" "in_progress")))

(defun pai-todo--set-status (task status)
  "Set TASK's STATUS (destructively); leaving blocked drops the reason."
  (plist-put task :status status)
  (unless (equal status "blocked") (plist-put task :blocker nil))
  task)

(defun pai-todo-normalize (phases)
  "Keep exactly one task in progress in PHASES (destructively); return PHASES.
Extra in-progress tasks go back to pending; with none, the earliest pending
task is promoted.  Blocked tasks are never promoted."
  (let* ((tasks (pai-todo--tasks phases))
         (active (seq-filter (lambda (tk) (equal (pai-todo--status tk) "in_progress")) tasks)))
    (dolist (tk (cdr active)) (pai-todo--set-status tk "pending"))
    (unless active
      (let ((first (seq-find (lambda (tk) (equal (pai-todo--status tk) "pending")) tasks)))
        (when first (pai-todo--set-status first "in_progress")))))
  phases)

(defun pai-todo-current (phases)
  "Return the task in progress in PHASES, else the first pending one."
  (let ((tasks (pai-todo--tasks phases)))
    (or (seq-find (lambda (tk) (equal (pai-todo--status tk) "in_progress")) tasks)
        (seq-find (lambda (tk) (equal (pai-todo--status tk) "pending")) tasks))))

;;;; Applying operations

(defun pai-todo--find-task (phases content)
  "Return (TASK . PHASE) whose text is CONTENT, or nil."
  (catch 'hit
    (dolist (ph phases)
      (dolist (tk (plist-get ph :tasks))
        (when (equal (plist-get tk :content) content) (throw 'hit (cons tk ph)))))
    nil))

(defun pai-todo--find-phase (phases name)
  "Return the phase of PHASES named NAME, or nil."
  (seq-find (lambda (ph) (equal (plist-get ph :name) name)) phases))

(defun pai-todo--resolve-task (phases content errors)
  "Return (TASK . PHASE) for CONTENT, or push an error onto ERRORS (a cons cell)."
  (cond
   ((or (null content) (string-empty-p content))
    (push "Missing task content" (car errors)) nil)
   ((pai-todo--find-task phases content))
   (t (push (cond
             ((string-match-p "\\`task-[0-9]+\\'" content)
              (format "Task \"%s\" not found. Tasks are referenced by their text, not by ids: pass the task's full text from the previous result." content))
             ((null (pai-todo--tasks phases))
              (format "Task \"%s\" not found (the todo list is empty: was it replaced or not yet created?)" content))
             (t (format "Task \"%s\" not found" content)))
            (car errors))
      nil)))

(defun pai-todo--targets (phases args errors)
  "Return the tasks ARGS target in PHASES (its :task, else its :phase)."
  (cond
   ((plist-get args :task)
    (let ((hit (pai-todo--resolve-task phases (plist-get args :task) errors)))
      (and hit (list (car hit)))))
   ((plist-get args :phase)
    (let ((ph (pai-todo--find-phase phases (plist-get args :phase))))
      (if ph (copy-sequence (plist-get ph :tasks))
        (push (format "Phase \"%s\" not found" (plist-get args :phase)) (car errors))
        nil)))
   (t (push (format "%s needs a task or a phase" (plist-get args :op)) (car errors)) nil)))

(defun pai-todo--init (args errors)
  "Return the phases of an `init' from ARGS."
  (let ((list (or (pai-todo--list (plist-get args :list))
                  (and (pai-todo--list (plist-get args :items))
                       (list (list :phase (or (plist-get args :phase) pai-todo-default-phase)
                                   :items (plist-get args :items))))))
        (seen-phases '()) (seen-tasks '()))
    (if (null list)
        (progn (push "Missing list for init" (car errors)) nil)
      (dolist (entry list)
        (let ((name (plist-get entry :phase)))
          (unless (and (stringp name) (not (string-empty-p name)))
            (push "Every phase of init needs a name" (car errors)))
          (when (member name seen-phases)
            (push (format "Duplicate phase \"%s\" in init list" name) (car errors)))
          (push name seen-phases)
          (unless (pai-todo--list (plist-get entry :items))
            (push (format "Phase \"%s\" has no items" name) (car errors)))
          (dolist (c (pai-todo--list (plist-get entry :items)))
            (when (member c seen-tasks)
              (push (format "Duplicate task \"%s\" in init list" c) (car errors)))
            (push c seen-tasks))))
      (mapcar (lambda (entry)
                (list :name (format "%s" (plist-get entry :phase))
                      :tasks (mapcar (lambda (c) (list :content (format "%s" c) :status "pending"))
                                     (pai-todo--list (plist-get entry :items)))))
              list))))

(defun pai-todo--append (phases args errors)
  "Append ARGS' :items to its :phase in PHASES; return the phases."
  (let ((name (plist-get args :phase))
        (items (pai-todo--list (plist-get args :items))))
    (cond
     ((not (and (stringp name) (not (string-empty-p name))))
      (push "Missing phase name for append" (car errors)) phases)
     ((null items) (push "Missing items for append" (car errors)) phases)
     (t
      (let ((seen '()) (dup nil))
        (dolist (c items)
          (when (or (member c seen) (pai-todo--find-task phases c))
            (push (format "Task \"%s\" already exists" c) (car errors))
            (setq dup t))
          (push c seen))
        (if dup phases
          (let ((ph (pai-todo--find-phase phases name)))
            (unless ph
              (setq ph (list :name name :tasks nil))
              (setq phases (append phases (list ph))))
            (plist-put ph :tasks (append (plist-get ph :tasks)
                                         (mapcar (lambda (c) (list :content c :status "pending"))
                                                 items)))
            phases)))))))

(defun pai-todo--rm (phases args errors)
  "Remove what ARGS target from PHASES (nothing targeted: everything)."
  (cond
   ((plist-get args :task)
    (let ((hit (pai-todo--resolve-task phases (plist-get args :task) errors)))
      (when hit (plist-put (cdr hit) :tasks (delq (car hit) (plist-get (cdr hit) :tasks))))
      phases))
   ((plist-get args :phase)
    (let ((ph (pai-todo--find-phase phases (plist-get args :phase))))
      (if ph (delq ph phases)
        (push (format "Phase \"%s\" not found" (plist-get args :phase)) (car errors))
        phases)))
   (t nil)))

(defun pai-todo-apply (phases args)
  "Apply one operation ARGS to a copy of PHASES.  Return (NEW . ERRORS).
NEW is normalized (see `pai-todo-normalize'); on errors it is meaningless
and callers keep PHASES."
  (let* ((phases (pai-todo-copy phases))
         (errors (list nil))
         (op (plist-get args :op))
         (new
          (pcase op
            ("init" (pai-todo--init args errors))
            ("start"
             (let ((hit (pai-todo--resolve-task phases (plist-get args :task) errors)))
               (when hit
                 (dolist (tk (pai-todo--tasks phases))
                   (when (and (equal (pai-todo--status tk) "in_progress") (not (eq tk (car hit))))
                     (pai-todo--set-status tk "pending")))
                 (pai-todo--set-status (car hit) "in_progress"))
               phases))
            ("done" (dolist (tk (pai-todo--targets phases args errors))
                      (pai-todo--set-status tk "completed"))
             phases)
            ("drop" (dolist (tk (pai-todo--targets phases args errors))
                      (pai-todo--set-status tk "abandoned"))
             phases)
            ("block"
             (let ((reason (and (stringp (plist-get args :reason))
                                (string-trim (replace-regexp-in-string
                                              "[ \t\n\r]+" " " (plist-get args :reason))))))
               ;; only open work is blocked: a phase keeps its finished tasks
               (dolist (tk (pai-todo--targets phases args errors))
                 (when (member (pai-todo--status tk) '("pending" "in_progress" "blocked"))
                   (pai-todo--set-status tk "blocked")
                   (plist-put tk :blocker (and reason (not (string-empty-p reason)) reason)))))
             phases)
            ("unblock" (dolist (tk (pai-todo--targets phases args errors))
                         (when (equal (pai-todo--status tk) "blocked")
                           (pai-todo--set-status tk "pending")))
             phases)
            ("rm" (pai-todo--rm phases args errors))
            ("append" (pai-todo--append phases args errors))
            ("view" phases)
            (_ (push (format "Unknown op \"%s\"; use one of %s" op (string-join pai-todo-ops ", "))
                     (car errors))
               phases))))
    ;; the copy also drops keys left empty (a cleared :blocker)
    (cons (pai-todo-copy (pai-todo-normalize new)) (nreverse (car errors)))))

(defun pai-todo-infer-op (args has-phases)
  "Return the op ARGS clearly mean when they have none, or nil.
A `list' means init; `items' with a `phase' means append; bare `items'
mean init only when there is no list yet (nothing to overwrite)."
  (cond
   ((pai-todo--list (plist-get args :list)) "init")
   ((pai-todo--list (plist-get args :items))
    (cond ((and (stringp (plist-get args :phase)) (not (string-empty-p (plist-get args :phase))))
           "append")
          ((not has-phases) "init")))))

;;;; Text

(defun pai-todo-summary (phases &optional errors read-only)
  "Return the result text for PHASES (and ERRORS) shown to the model."
  (let ((tasks (pai-todo--tasks phases)))
    (if (null tasks)
        (cond (errors (concat "Errors: " (string-join errors "; ")))
              (read-only "Todo list is empty.")
              (t "Todo list cleared."))
      (let* ((open (seq-filter #'pai-todo--open-p tasks))
             (closed (seq-count (lambda (tk) (member (pai-todo--status tk) '("completed" "abandoned")))
                                tasks))
             (blocked (seq-count (lambda (tk) (equal (pai-todo--status tk) "blocked")) tasks))
             (idx (or (seq-position phases nil
                                    (lambda (ph _) (seq-some #'pai-todo--open-p (plist-get ph :tasks))))
                      (1- (length phases))))
             (current (nth idx phases))
             (lines '()))
        (when errors (push (concat "Errors: " (string-join errors "; ")) lines))
        (if (null open)
            (push "Remaining items: none." lines)
          (push (format "Remaining items (%d):" (length open)) lines)
          (dolist (ph phases)
            (dolist (tk (plist-get ph :tasks))
              (when (pai-todo--open-p tk)
                (push (format "  - %s [%s] (%s)" (plist-get tk :content) (pai-todo--status tk)
                              (plist-get ph :name))
                      lines)))))
        (push (format "Overall: %d/%d done, %d open%s." closed (length tasks) (length open)
                      (if (> blocked 0) (format ", %d blocked" blocked) ""))
              lines)
        (push (format "Active phase %d/%d \"%s\" (%d/%d)."
                      (1+ idx) (length phases) (plist-get current :name)
                      (seq-count (lambda (tk) (member (pai-todo--status tk) '("completed" "abandoned")))
                                 (plist-get current :tasks))
                      (length (plist-get current :tasks)))
              lines)
        (dolist (ph phases)
          (push (format "  %s:" (plist-get ph :name)) lines)
          (dolist (tk (plist-get ph :tasks))
            (push (format "    - %s %s%s"
                          (if (equal (pai-todo--status tk) "completed") "[X]" "[ ]")
                          (plist-get tk :content)
                          (pcase (pai-todo--status tk)
                            ("in_progress" " (in progress)")
                            ("abandoned" " (dropped)")
                            ("blocked" (if (plist-get tk :blocker)
                                           (format " (blocked: %s)" (plist-get tk :blocker))
                                         " (blocked)"))
                            (_ "")))
                  lines)))
        (string-join (nreverse lines) "\n")))))

(defconst pai-todo--icons
  '(("pending" . "☐") ("in_progress" . "▶") ("completed" . "☑")
    ("abandoned" . "✗") ("blocked" . "⛔"))
  "Status marks in the list shown to the user.")

(defun pai-todo-display (phases)
  "Return PHASES as text for the user (the /todo note)."
  (if (null (pai-todo--tasks phases))
      "The todo list is empty."
    (mapconcat
     (lambda (ph)
       (concat (propertize (plist-get ph :name) 'face 'bold) "\n"
               (mapconcat (lambda (tk)
                            (format "  %s %s%s" (cdr (assoc (pai-todo--status tk) pai-todo--icons))
                                    (propertize (plist-get tk :content)
                                                'face (pcase (pai-todo--status tk)
                                                        ("in_progress" 'bold)
                                                        ((or "completed" "abandoned") 'shadow)
                                                        ("blocked" 'warning)
                                                        (_ 'default)))
                                    (if (plist-get tk :blocker)
                                        (format " — %s" (plist-get tk :blocker)) "")))
                          (plist-get ph :tasks) "\n")))
     phases "\n")))

;;;; State: per session, from its branch

(defvar pai-todo--state (make-hash-table :test 'eq :weakness 'key)
  "Session -> its current phases (the cached result of `pai-todo--from-branch').")

(defun pai-todo--canonical (entry)
  "Return (PHASES) when session ENTRY sets the todo list, else nil."
  (pcase (plist-get entry :type)
    ("custom"
     (when (equal (plist-get entry :customType) "todo")
       (list (plist-get (plist-get entry :data) :phases))))
    ("message"
     (let* ((m (plist-get entry :message))
            (details (plist-get m :details)))
       (when (and (member (format "%s" (plist-get m :role)) '("tool-result"))
                  (equal (plist-get m :tool-name) "todo")
                  (not (eq (plist-get m :is-error) t))
                  (plist-get details :op)
                  (not (equal (plist-get details :op) "view")))
         (list (plist-get details :phases)))))))

(defun pai-todo--from-branch (session)
  "Return the todo phases of SESSION's current branch."
  (catch 'found
    (dolist (e (reverse (pai-session-get-branch session)))
      (let ((hit (pai-todo--canonical e)))
        (when hit (throw 'found (pai-todo-copy (car hit))))))
    nil))

(defun pai-todo-phases (session)
  "Return SESSION's todo phases (a copy)."
  (if (null session) nil
    (pai-todo-copy
     (or (gethash session pai-todo--state)
         (puthash session (pai-todo--from-branch session) pai-todo--state)))))

(defun pai-todo--store (session phases)
  "Make PHASES SESSION's list (in memory) and refresh its buffers."
  (puthash session (pai-todo-copy phases) pai-todo--state)
  (pai-todo--refresh-widgets session))

(defun pai-todo--forget (session)
  "Drop the cached list of SESSION (it is read from the branch again)."
  (when session (remhash session pai-todo--state)))

(defun pai-todo--buffers (session)
  "Return the live pai buffers showing SESSION."
  (seq-filter (lambda (b) (and (eq (buffer-local-value 'major-mode b) 'pai-mode)
                               (eq (buffer-local-value 'pai--session b) session)))
              (buffer-list)))

;;;; Panel above the prompt (oh-my-pi's todo HUD)

;; Collapsed (default): the active phase -- the first with open tasks --
;; with its last closed task for context and up to `pai-todo-hud-task-cap'
;; open tasks (the one in progress first), then up to
;; `pai-todo-hud-phase-cap' following phases as one line each.  Finished
;; phases above it are left out.  `/todo expand' shows everything.  The rail
;; on the left fills with overall progress.  Once every task is closed the
;; panel goes away after `:hud-clear-delay' seconds.

(defconst pai-todo-hud-task-cap 5 "Open tasks the collapsed panel shows.")
(defconst pai-todo-hud-phase-cap 4 "Following phases the collapsed panel shows.")

(defface pai-todo-hud-title '((t :inherit bold)) "Title of the todo panel." :group 'pai)
(defface pai-todo-hud-rail-done '((t :inherit success)) "Done part of the panel rail." :group 'pai)
(defface pai-todo-hud-rail '((t :inherit shadow)) "Open part of the panel rail." :group 'pai)
(defface pai-todo-hud-active-phase '((t :inherit (bold font-lock-function-name-face)))
  "The active phase in the todo panel." :group 'pai)

(defun pai-todo-hud-p ()
  "Return non-nil when the panel above the prompt is on."
  (pai-truthy (pai-todo--setting :hud t)))

(defun pai-todo-hud-clear-delay ()
  "Seconds after which a finished list leaves the panel (negative: never)."
  (let ((n (pai-todo--setting :hud-clear-delay 60))) (if (numberp n) n 60)))

(defvar-local pai-todo--expanded nil "Non-nil when the panel shows every phase and task.")
(defvar-local pai-todo--hud-hidden nil "Finished list (its phases) whose panel was cleared.")
(defvar-local pai-todo--clear-timer nil "Timer clearing the panel of a finished list.")

(defun pai-todo--closed-p (task)
  "Return non-nil when TASK is completed or abandoned."
  (member (pai-todo--status task) '("completed" "abandoned")))

(defun pai-todo--roman (n)
  "Return N (1..3999) as a Roman numeral."
  (let ((out "") (table '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD") (100 . "C")
                          (90 . "XC") (50 . "L") (40 . "XL") (10 . "X") (9 . "IX")
                          (5 . "V") (4 . "IV") (1 . "I"))))
    (dolist (e table out)
      (while (>= n (car e)) (setq out (concat out (cdr e)) n (- n (car e)))))))

(defun pai-todo--hud-task-line (task width)
  "Return the panel line of TASK, at most WIDTH wide."
  (let* ((st (pai-todo--status task))
         (face (pcase st
                 ("in_progress" 'bold)
                 ((or "completed" "abandoned") '(shadow (:strike-through t)))
                 ("blocked" 'warning)
                 (_ 'default)))
         (text (concat (plist-get task :content)
                       (if (plist-get task :blocker) (concat " — " (plist-get task :blocker)) ""))))
    (concat (cdr (assoc st pai-todo--icons)) " "
            (propertize (truncate-string-to-width text (max 10 (- width 2)) nil nil "…") 'face face))))

(defun pai-todo--hud-tasks (tasks)
  "Return (TASKS-TO-SHOW . HIDDEN-COUNT) of an active phase's TASKS, collapsed."
  (if (<= (length tasks) pai-todo-hud-task-cap)
      (cons tasks 0)
    (let* ((open (seq-remove #'pai-todo--closed-p tasks))
           (lead (last (seq-filter #'pai-todo--closed-p tasks) 1))
           (cur (seq-position open (seq-find (lambda (tk) (equal (pai-todo--status tk) "in_progress")) open)))
           ;; the task in progress leads, then those after it, then the rest
           (ordered (if cur (append (nthcdr cur open) (seq-take open cur)) open))
           (shown (seq-take ordered pai-todo-hud-task-cap))
           ;; keep list order among what is shown
           (shown (seq-filter (lambda (tk) (memq tk shown)) open)))
      (cons (append lead shown) (- (length open) (length shown))))))

(defun pai-todo-hud-text (phases &optional expanded width)
  "Return the panel for PHASES (EXPANDED: everything), WIDTH columns, or nil."
  (let* ((phases (seq-filter (lambda (ph) (plist-get ph :tasks)) phases))
         (width (or width 80))
         (multi (> (length phases) 1)))
    (when phases
      (let* ((active (or (seq-position phases nil
                                       (lambda (ph _) (seq-some #'pai-todo--open-p (plist-get ph :tasks))))
                         (1- (length phases))))
             (base (if expanded 0 active))
             (slice (if expanded phases
                      (seq-take (nthcdr base phases) (1+ pai-todo-hud-phase-cap))))
             (hidden-phases (- (length phases) base (length slice)))
             (rows '()))                ; (FIRST-ROW-OF-BLOCK-P . TEXT), reversed
        (cl-loop
         for ph in slice for i from base
         do (let* ((tasks (plist-get ph :tasks))
                   (is-active (= i active))
                   (label (if multi (format "%s. %s" (pai-todo--roman (1+ i)) (plist-get ph :name))
                            (plist-get ph :name)))
                   (progress (propertize (format " · %d/%d" (seq-count #'pai-todo--closed-p tasks)
                                                 (length tasks))
                                         'face 'shadow)))
              (push (cons t (concat (propertize label 'face (if is-active 'pai-todo-hud-active-phase 'shadow))
                                    progress))
                    rows)
              (when (or expanded is-active)
                (let ((sel (if expanded (cons tasks 0) (pai-todo--hud-tasks tasks))))
                  (dolist (tk (car sel))
                    (push (cons nil (concat "  " (pai-todo--hud-task-line tk (- width 8)))) rows))
                  (when (> (cdr sel) 0)
                    (push (cons nil (propertize (format "  … %d more" (cdr sel)) 'face 'shadow)) rows))))))
        (when (> hidden-phases 0)
          (push (cons t (propertize (format "… %d more phase%s" hidden-phases (if (= hidden-phases 1) "" "s"))
                                    'face 'shadow))
                rows))
        (setq rows (nreverse rows))
        (let* ((tasks (pai-todo--tasks phases))
               (closed (seq-count #'pai-todo--closed-p tasks))
               (tail "╰────")
               (path (+ (length rows) (length tail)))
               (filled (round (* path (/ (float closed) (length tasks)))))
               (filled (cond ((and (> closed 0) (< filled 1)) 1)
                             ((and (< closed (length tasks)) (>= filled path)) (1- path))
                             (t filled)))
               (i -1))
          (concat
           (propertize "TODO" 'face 'pai-todo-hud-title)
           (propertize (format "  %d/%d%s" closed (length tasks)
                               (if expanded "  (/todo collapse)" ""))
                       'face 'shadow)
           "
"
           (mapconcat (lambda (row)
                        (cl-incf i)
                        (concat " " (propertize (if (car row) "├─ " "│  ")
                                                'face (if (< i filled) 'pai-todo-hud-rail-done 'pai-todo-hud-rail))
                                (cdr row)))
                      rows "
")
           "
 "
           (let ((n (max 0 (min (length tail) (- filled (length rows))))))
             (concat (propertize (substring tail 0 n) 'face 'pai-todo-hud-rail-done)
                     (propertize (substring tail n) 'face 'pai-todo-hud-rail)))))))))

(defun pai-todo--finished-p (phases)
  "Return non-nil when PHASES has tasks and every one is closed."
  (let ((tasks (pai-todo--tasks phases)))
    (and tasks (seq-every-p #'pai-todo--closed-p tasks))))

(defun pai-todo--schedule-clear (buf phases)
  "Clear BUF's panel of finished PHASES after the configured delay."
  (let ((delay (pai-todo-hud-clear-delay)))
    (with-current-buffer buf
      (when (timerp pai-todo--clear-timer) (cancel-timer pai-todo--clear-timer))
      (setq pai-todo--clear-timer
            (and (>= delay 0)
                 (run-at-time
                  delay nil
                  (lambda ()
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (setq pai-todo--clear-timer nil)
                        (when (and pai--session (equal (pai-todo-phases pai--session) phases))
                          (setq pai-todo--hud-hidden phases)
                          (pai-todo--refresh-widgets pai--session)))))))))))

(defun pai-todo--hud-for-buffer (phases)
  "Return the panel text for PHASES in the current buffer, or nil."
  (when (and (pai-todo-hud-p) phases
             (not (equal phases pai-todo--hud-hidden)))
    (let ((win (get-buffer-window (current-buffer) t)))
      (pai-todo-hud-text phases pai-todo--expanded (if win (window-width win) 80)))))

;;;; Status bar

(defun pai-todo-widget-text (phases)
  "Return the status-bar text for PHASES, or nil when there are no tasks."
  (let ((tasks (pai-todo--tasks phases)))
    (when tasks
      (let* ((closed (seq-count (lambda (tk) (member (pai-todo--status tk) '("completed" "abandoned")))
                                tasks))
             (blocked (seq-count (lambda (tk) (equal (pai-todo--status tk) "blocked")) tasks))
             (current (pai-todo-current phases)))
        (propertize
         (concat (format "☑ %d/%d" closed (length tasks))
                 (if (> blocked 0) (format " ⛔%d" blocked) "")
                 (if current
                     (concat " · " (truncate-string-to-width (plist-get current :content) 32 nil nil "…"))
                   (if (= blocked 0) " ✓" "")))
         'help-echo (concat (substring-no-properties (pai-todo-display phases))
                            "\n\n/todo shows and changes the list"))))))

(defun pai-todo--refresh-widgets (session)
  "Update the todo panel and status-bar widget in the buffers showing SESSION.
The widget shows only while the panel does not (panel off, or cleared)."
  (let* ((phases (pai-todo-phases session))
         (finished (pai-todo--finished-p phases)))
    (dolist (b (pai-todo--buffers session))
      (with-current-buffer b
        (unless (equal phases pai-todo--hud-hidden) (setq pai-todo--hud-hidden nil))
        (when (and (not finished) (timerp pai-todo--clear-timer))
          (cancel-timer pai-todo--clear-timer)
          (setq pai-todo--clear-timer nil))
        (let ((hud (pai-todo--hud-for-buffer phases)))
          (when (and hud finished (not (timerp pai-todo--clear-timer)))
            (pai-todo--schedule-clear b phases))
          (when (fboundp 'pai--set-panel) (pai--set-panel "todo" hud))
          (when (fboundp 'pai--set-widget)
            (pai--set-widget "todo" (and (not hud) (pai-todo-widget-text phases)))))))))

;;;; The tool

(defconst pai-todo-description
  (concat
   "Track multi-step work as a phased todo list. Tasks are referenced by their exact text (never ids like \"task-1\"); pass the full text in `task`. One operation per call.\n\n"
   "After each successful change: if nothing is in_progress, the earliest pending task becomes in_progress; if several are, only the earliest stays. Blocked tasks never auto-promote (unblock first). Completed tasks never revert.\n\n"
   "Operations:\n"
   "- init: {list: [{phase, items: [text...]}]} (or {items} for one phase) -- replace the whole list\n"
   "- start: {task} -- mark in progress\n"
   "- done / drop: {task} or {phase} -- mark completed / abandoned\n"
   "- block: {task} or {phase}, optional reason -- waiting on something you cannot act on (a user decision, another agent, a service); excluded from the stop reminder\n"
   "- unblock: {task} or {phase} -- blocked -> pending\n"
   "- rm: {task} or {phase}; neither clears the list\n"
   "- append: {phase, items} -- add tasks to a phase (created if missing)\n"
   "- view -- read-only; echoes the list (the user may also change it with /todo)\n\n"
   "Task text: 5-10 words, what not how, unique. Phase names: short noun phrases (Foundation, Auth, Verification), never numbered.\n\n"
   "Rules:\n"
   "- Create a list when a request needs 3+ distinct steps, when the user asks for one, or gives a set of tasks. When the user gives a multi-step plan or N items, init EVERY item as its own task; never summarize them into fewer.\n"
   "- Mark tasks done right after finishing them; complete phases in order.\n"
   "- Never make a todo call the turn's only tool call: batch it with real work.\n"
   "- Keep task and phase texts stable. Lost the exact text? Use view; never guess.")
  "Description of the `todo' tool.")

(defun pai-todo--args (raw)
  "Return tool arguments RAW with lists as lists and the op inferred if clear."
  (let ((args (copy-sequence raw)))
    (dolist (k '(:list :items))
      (when (plist-get args k)
        (setq args (plist-put args k (pai-todo--list (plist-get args k))))))
    (when (plist-get args :list)
      (setq args (plist-put args :list
                            (mapcar (lambda (e) (plist-put (copy-sequence e) :items
                                                           (pai-todo--list (plist-get e :items))))
                                    (plist-get args :list)))))
    args))

(defun pai-todo--execute (raw ctx _on-update on-done)
  "Run the `todo' tool with RAW arguments in CTX; finish with ON-DONE."
  (let* ((session (plist-get ctx :session))
         (previous (pai-todo-phases session))
         (args (pai-todo--args raw))
         (op (or (and (stringp (plist-get args :op)) (not (string-empty-p (plist-get args :op)))
                      (plist-get args :op))
                 (pai-todo-infer-op args (and previous t)))))
    (if (null op)
        (funcall on-done
                 (pai-tool-error-result
                  (format "Missing op: use one of %s" (string-join pai-todo-ops ", "))
                  (list :phases previous)))
      (setq args (plist-put args :op op))
      (let* ((read-only (equal op "view"))
             (result (if read-only (cons previous nil) (pai-todo-apply previous args)))
             (errors (cdr result))
             (effective (if errors previous (car result))))
        (when (and session (not read-only) (not errors))
          (pai-todo--store session effective))
        (funcall on-done
                 (if errors
                     (pai-tool-error-result (pai-todo-summary effective errors read-only)
                                            (list :phases effective))
                   (pai-tool-ok-result (pai-todo-summary effective nil read-only)
                                       (list :op op :phases effective))))))))

(defconst pai-todo-tool
  (list
   :name "todo"
   :label "Todo"
   :description pai-todo-description
   :prompt-snippet "todo: keep a phased todo list of multi-step work (init, start, done, drop, block, append, view)"
   :execution-mode 'sequential
   ;; used early and often: send the schema up front, no reveal round trip
   :deferred nil
   :parameters
   (pai-object-schema
    (list :op (pai-string-schema "Operation to apply." :enum (vconcat pai-todo-ops))
          :list (pai-array-schema
                 "Phased task list (init)."
                 (pai-object-schema
                  (list :phase (pai-string-schema "Phase name.")
                        :items (pai-array-schema "Tasks of this phase." (pai-string-schema "Task text.")))
                  '("phase" "items")))
          :task (pai-string-schema "Task text (exactly as in the list).")
          :phase (pai-string-schema "Phase name.")
          :items (pai-array-schema "Tasks for a one-phase init, or for append."
                                   (pai-string-schema "Task text."))
          :reason (pai-string-schema "Why the task is blocked (block)."))
    '("op"))
   :execute #'pai-todo--execute)
  "The `todo' tool.")

;;;; Reminders

(defvar-local pai-todo--reminders 0 "Reminders sent since the user's last prompt.")
(defvar-local pai-todo--awaiting nil "Non-nil after a reminder, until the agent acts.")

(defun pai-todo--asking-p (message)
  "Return non-nil when assistant MESSAGE ends with a question to the user."
  (let* ((text (string-trim (or (ignore-errors (pai-content-text (pai-message-content message))) "")))
         (last-line (car (last (split-string text "\n" t "[ \t]+")))))
    (and last-line (string-match-p "[?？][*_)\"'`]*\\'" last-line))))

(defun pai-todo-reminder-text (phases n max)
  "Return reminder N of MAX about the open tasks of PHASES."
  (let ((open (seq-count #'pai-todo--open-p (pai-todo--tasks phases))))
    (concat
     (format "[todo reminder %d/%d] You stopped with %d open todo item%s:\n"
             n max open (if (= open 1) "" "s"))
     (mapconcat (lambda (ph)
                  (let ((tasks (seq-filter #'pai-todo--open-p (plist-get ph :tasks))))
                    (when tasks
                      (concat "- " (plist-get ph :name) "\n"
                              (mapconcat (lambda (tk) (concat "  - " (plist-get tk :content)))
                                         tasks "\n")
                              "\n"))))
                phases "")
     "Continue working on them, or mark each done, dropped or blocked with the todo tool.")))

(defun pai-todo--reminder-due ()
  "Return the reminder text to send now in this buffer, or nil."
  (let* ((phases (and pai--session (pai-todo-phases pai--session)))
         (last (seq-find #'pai-assistant-message-p (reverse pai--context-messages)))
         (max (pai-todo-reminders-max)))
    (when (and (pai-todo-reminders-p)
               (not pai--active)
               (null pai--steering-queue)
               (not pai-todo--awaiting)
               (< pai-todo--reminders max)
               (seq-some #'pai-todo--open-p (pai-todo--tasks phases))
               last
               (not (memq (plist-get last :stop-reason) '(aborted error)))
               (not (pai-todo--asking-p last)))
      (pai-todo-reminder-text phases (1+ pai-todo--reminders) max))))

(defun pai-todo--on-settled (_event ctx)
  "After a run settles, remind the agent of open tasks when due."
  (let ((buf (plist-get ctx :buffer)))
    (when (buffer-live-p buf)
      ;; after the other `agent-settled' handlers, and only if still idle
      (run-at-time
       0 nil
       (lambda ()
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let ((text (pai-todo--reminder-due)))
               (when text
                 (cl-incf pai-todo--reminders)
                 (setq pai-todo--awaiting t)
                 (condition-case err (pai--start-run text)
                   (error (message "pai-todo: %s" (error-message-string err)))))))))))))

;;;; /todo

(defun pai-todo--match-task (phases query &optional pred)
  "Return the task of PHASES matching QUERY (exact, else unique substring).
PRED, when given, limits the candidates.  Case is ignored."
  (let* ((q (downcase (string-trim query)))
         (tasks (seq-filter (or pred #'identity) (pai-todo--tasks phases))))
    (unless (string-empty-p q)
      (or (seq-find (lambda (tk) (equal (downcase (plist-get tk :content)) q)) tasks)
          (let ((subs (seq-filter (lambda (tk) (string-search q (downcase (plist-get tk :content))))
                                  tasks)))
            (cond ((= (length subs) 1) (car subs))
                  (t (let ((open (seq-filter #'pai-todo--open-p subs)))
                       (and (= (length open) 1) (car open))))))))))

(defun pai-todo--match-phase (phases query)
  "Return the phase of PHASES matching QUERY (exact, unique prefix or substring)."
  (let ((q (downcase (string-trim query))))
    (unless (string-empty-p q)
      (or (seq-find (lambda (ph) (equal (downcase (plist-get ph :name)) q)) phases)
          (let ((pre (seq-filter (lambda (ph) (string-prefix-p q (downcase (plist-get ph :name)))) phases)))
            (and (= (length pre) 1) (car pre)))
          (let ((subs (seq-filter (lambda (ph) (string-search q (downcase (plist-get ph :name)))) phases)))
            (and (= (length subs) 1) (car subs)))))))

(defun pai-todo--target-args (phases verb query)
  "Return tool args for VERB on QUERY in PHASES, or an error string.
Without QUERY, done/drop/block mean the task in progress."
  (if (string-empty-p (string-trim query))
      (let ((cur (and (member verb '("done" "drop" "block")) (pai-todo-current phases))))
        (if cur (list :op verb :task (plist-get cur :content))
          (format "Usage: /todo %s TASK-or-PHASE" verb)))
    (let ((task (pai-todo--match-task phases query))
          (phase (pai-todo--match-phase phases query)))
      (cond
       (task (list :op verb :task (plist-get task :content)))
       ((and phase (not (equal verb "start"))) (list :op verb :phase (plist-get phase :name)))
       (t (format "No %s matches \"%s\"" (if (equal verb "start") "task" "task or phase")
                  (string-trim query)))))))

(defun pai-todo--commit (session phases)
  "Save PHASES as the user's edit of SESSION's list."
  (pai-session-append-custom session "todo" (list :op "edit" :phases phases))
  (pai-todo--store session phases))

(defun pai-todo--change (session args)
  "Apply tool ARGS to SESSION's list as a user edit; return a message."
  (let* ((result (pai-todo-apply (pai-todo-phases session) args)))
    (if (cdr result)
        (string-join (cdr result) "; ")
      (pai-todo--commit session (car result))
      (pai-todo-display (car result)))))

;;;;; Org format (edit, export, import)

(defconst pai-todo--org-keywords
  '(("pending" . "TODO") ("in_progress" . "DOING") ("blocked" . "BLOCKED")
    ("completed" . "DONE") ("abandoned" . "DROPPED"))
  "Org keyword of each status.")

(defun pai-todo-to-org (phases)
  "Return PHASES as an Org outline."
  (concat
   "#+TODO: TODO DOING BLOCKED | DONE DROPPED\n"
   (mapconcat (lambda (ph)
                (concat "* " (plist-get ph :name) "\n"
                        (mapconcat (lambda (tk)
                                     (concat "** " (cdr (assoc (pai-todo--status tk) pai-todo--org-keywords))
                                             " " (plist-get tk :content) "\n"
                                             (if (plist-get tk :blocker)
                                                 (format "   Blocker: %s\n" (plist-get tk :blocker))
                                               "")))
                                   (plist-get ph :tasks) "")))
              phases "")))

(defun pai-todo-from-org (text)
  "Parse Org TEXT into phases.  Return (PHASES . ERRORS).
`* Phase' headings hold `** KEYWORD task' headings (TODO DOING BLOCKED DONE
DROPPED; none means pending) or checkbox items (`- [ ]', `- [X]', `- [-]'
in progress).  A `Blocker: REASON' line gives a blocked task's reason."
  (let ((phases '()) (errors '()) (phase nil) (task nil) (n 0)
        (kw-status (mapcar (lambda (c) (cons (cdr c) (car c))) pai-todo--org-keywords)))
    (dolist (line (split-string text "\n"))
      (cl-incf n)
      (let ((case-fold-search nil)
            (add (lambda (content status)
                   (unless phase
                     (setq phase (list :name pai-todo-default-phase :tasks nil))
                     (push phase phases))
                   (setq task (list :content content :status status))
                   (plist-put phase :tasks (append (plist-get phase :tasks) (list task))))))
        (cond
         ((string-match "\\`\\* +\\(.+?\\)[ \t]*\\'" line)
          (setq phase (list :name (match-string 1 line) :tasks nil) task nil)
          (push phase phases))
         ((string-match "\\`\\*\\*+ +\\(?:\\(TODO\\|DOING\\|BLOCKED\\|DONE\\|DROPPED\\) +\\)?\\(.+?\\)\\(?:[ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*\\'" line)
          (funcall add (match-string 2 line)
                   (if (match-string 1 line) (cdr (assoc (match-string 1 line) kw-status)) "pending")))
         ((string-match "\\`[ \t]*[-+] +\\[\\([ xX-]\\)\\] +\\(.+?\\)[ \t]*\\'" line)
          (funcall add (match-string 2 line)
                   (pcase (match-string 1 line) ((or "x" "X") "completed") ("-" "in_progress") (_ "pending"))))
         ((and task (string-match "\\`[ \t]+Blocker: *\\(.*?\\)[ \t]*\\'" line))
          (plist-put task :blocker (match-string 1 line)))
         ((or (string-match-p "\\`[ \t]*\\'" line) (string-prefix-p "#" line)) nil)
         ((string-match-p "\\`[ \t]" line) nil)          ; notes under a heading
         (t (push (format "Line %d: not a phase, task or note: %s" n line) errors)))))
    (let ((phases (nreverse phases)) (seen '()))
      (dolist (tk (pai-todo--tasks phases))
        (when (member (plist-get tk :content) seen)
          (push (format "Duplicate task \"%s\"" (plist-get tk :content)) errors))
        (push (plist-get tk :content) seen))
      (cons (pai-todo-normalize (pai-todo-copy phases)) (nreverse errors)))))

(defvar-local pai-todo--edit-session nil "Session whose list this edit buffer changes.")

(defvar pai-todo-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pai-todo-edit-save)
    (define-key map (kbd "C-c C-k") #'pai-todo-edit-cancel)
    map)
  "Keys of a todo edit buffer.")

(define-minor-mode pai-todo-edit-mode
  "Edit a pai todo list: C-c C-c saves, C-c C-k cancels."
  :lighter " Todo-edit" :keymap pai-todo-edit-mode-map)

(defun pai-todo-edit-save ()
  "Save the edited todo list to its session."
  (interactive)
  (let ((parsed (pai-todo-from-org (buffer-substring-no-properties (point-min) (point-max)))))
    (if (cdr parsed)
        (user-error "%s" (string-join (cdr parsed) "; "))
      (pai-todo--commit pai-todo--edit-session (car parsed))
      (message "Todo list saved (%d tasks)" (length (pai-todo--tasks (car parsed))))
      (quit-window t))))

(defun pai-todo-edit-cancel ()
  "Close the todo edit buffer without saving."
  (interactive)
  (quit-window t))

(defun pai-todo--edit (session)
  "Open SESSION's todo list for editing in Org."
  (let ((buf (generate-new-buffer "*pai todo*")))
    (with-current-buffer buf
      (insert (pai-todo-to-org (pai-todo-phases session)))
      (if (fboundp 'org-mode) (org-mode) (text-mode))
      (pai-todo-edit-mode 1)
      (setq pai-todo--edit-session session)
      (setq header-line-format "Todo list: C-c C-c saves, C-c C-k cancels")
      (goto-char (point-min))
      (forward-line 1))
    ;; select its window, but leave the chat current: the command's note goes there
    (save-current-buffer (pop-to-buffer buf))
    "Editing the todo list in *pai todo* (C-c C-c saves, C-c C-k cancels)"))

(defun pai-todo--file (arg)
  "Return the export/import file named by ARG (default TODO.org)."
  (expand-file-name (if (string-empty-p (string-trim arg)) "TODO.org" (string-trim arg))))

(defun pai-todo--dispatch (session verb rest)
  "Run `/todo VERB REST' on SESSION's list; return a message."
  (let ((phases (pai-todo-phases session)))
    (pcase verb
      ((or "" "show") (pai-todo-display phases))
      ((or "start" "done" "drop" "block" "unblock" "rm")
       (if (and (equal verb "rm") (string-empty-p (string-trim rest)))
           "Usage: /todo rm TASK-or-PHASE (/todo clear removes everything)"
         (let ((args (pai-todo--target-args phases verb rest)))
           (if (stringp args) args (pai-todo--change session args)))))
      ("append"
       (let* ((colon (string-match "\\`\\([^:]+\\): *\\(.+\\)\\'" (string-trim rest)))
              (phase (and colon (pai-todo--match-phase phases (match-string 1 (string-trim rest)))))
              (text (if phase (match-string 2 (string-trim rest)) (string-trim rest)))
              (cur (pai-todo-current phases))
              (target (or phase
                          (and cur (seq-find (lambda (ph) (memq cur (plist-get ph :tasks))) phases))
                          (car (last phases)))))
         (if (string-empty-p text) "Usage: /todo append [PHASE:] TASK"
           (pai-todo--change session (list :op "append"
                                           :phase (if target (plist-get target :name)
                                                    pai-todo-default-phase)
                                           :items (list text))))))
      ("clear" (pai-todo--commit session nil) "Todo list cleared")
      ("edit" (pai-todo--edit session))
      ("export"
       (let ((file (pai-todo--file rest)))
         (with-temp-file file (insert (pai-todo-to-org phases)))
         (format "Wrote %d tasks to %s" (length (pai-todo--tasks phases)) (abbreviate-file-name file))))
      ("import"
       (let ((file (pai-todo--file rest)))
         (if (not (file-readable-p file))
             (format "No file %s" (abbreviate-file-name file))
           (let ((parsed (pai-todo-from-org (with-temp-buffer (insert-file-contents file) (buffer-string)))))
             (if (cdr parsed) (string-join (cdr parsed) "; ")
               (pai-todo--commit session (car parsed))
               (format "Imported %d tasks from %s\n%s" (length (pai-todo--tasks (car parsed)))
                       (abbreviate-file-name file) (pai-todo-display (car parsed))))))))
      ((or "expand" "collapse")
       (setq pai-todo--expanded (equal verb "expand") pai-todo--hud-hidden nil)
       (pai-todo--refresh-widgets session)
       (if (pai-todo-hud-p)
           (format "Todo panel %s" (if pai-todo--expanded "expanded: every phase and task" "collapsed"))
         "The todo panel is off (/todo hud on)"))
      ("hud"
       (pcase (string-trim rest)
         ("on" (pai-todo--set-setting :hud t) (setq pai-todo--hud-hidden nil)
          (pai-todo--refresh-widgets session) "Todo panel on")
         ("off" (pai-todo--set-setting :hud :false)
          (pai-todo--refresh-widgets session) "Todo panel off (the status bar shows the list)")
         (_ (format "The todo panel is %s (/todo hud on|off)" (if (pai-todo-hud-p) "on" "off")))))
      ("reminders"
       (pcase (string-trim rest)
         ("on" (pai-todo--set-setting :reminders t) "Todo reminders on")
         ("off" (pai-todo--set-setting :reminders :false) "Todo reminders off")
         (_ (format "Todo reminders are %s (/todo reminders on|off)"
                    (if (pai-todo-reminders-p) "on" "off")))))
      (_ (concat "Usage: /todo [show | start|done|drop|block|unblock|rm TASK-or-PHASE | "
                 "append [PHASE:] TASK | clear | edit | export [FILE] | import [FILE] | "
                 "expand | collapse | hud on|off | reminders on|off]")))))

(defun pai-todo-command (args ctx)
  "Handler of `/todo' with ARGS in CTX."
  (let* ((buf (plist-get ctx :buffer))
         (session (and (buffer-live-p buf) (buffer-local-value 'pai--session buf)))
         (text (string-trim (or args "")))
         (verb (car (split-string text "[ \t]+" t)))
         (rest (if verb (string-trim (substring text (length verb))) "")))
    (list :message (if session (pai-todo--dispatch session (or verb "") rest)
                     "No pai session"))))

;;;;; Completion

(defun pai-todo--completion-phases ()
  "Return the current buffer's todo phases."
  (and (boundp 'pai--session) pai--session (pai-todo-phases pai--session)))

(defun pai-todo--names (pred &optional phases-too)
  "Return a function listing task texts matching PRED (and phase names)."
  (lambda ()
    (let ((phases (pai-todo--completion-phases)))
      (append (mapcar (lambda (tk) (plist-get tk :content))
                      (seq-filter pred (pai-todo--tasks phases)))
              (and phases-too (mapcar (lambda (ph) (plist-get ph :name)) phases))))))

(defconst pai-todo-completion-tree
  `("show"
    ("start" (:line . ,(pai-todo--names (lambda (tk) (equal (pai-todo--status tk) "pending")))))
    ("done" (:line . ,(pai-todo--names #'pai-todo--open-p t)))
    ("drop" (:line . ,(pai-todo--names #'pai-todo--open-p t)))
    ("block" (:line . ,(pai-todo--names #'pai-todo--open-p t)))
    ("unblock" (:line . ,(pai-todo--names (lambda (tk) (equal (pai-todo--status tk) "blocked")) t)))
    ("rm" (:line . ,(pai-todo--names #'identity t)))
    ("append" (:line . ,(lambda () (mapcar (lambda (ph) (concat (plist-get ph :name) ": "))
                                           (pai-todo--completion-phases)))))
    "clear" "edit"
    ("export" (:rest)) ("import" (:rest))
    "expand" "collapse"
    ("hud" "on" "off")
    ("reminders" "on" "off"))
  "What `/todo' completes at each argument position.")

(defconst pai-todo-subcommands
  (mapcar (lambda (n) (if (consp n) (car n) n)) pai-todo-completion-tree)
  "Every `/todo' subcommand.")

;;;; Extension

(defun pai-todo--refresh-buffer (_event ctx)
  "Re-read the list of CTX's session from its branch and redraw."
  (let ((buf (plist-get ctx :buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq pai-todo--reminders 0 pai-todo--awaiting nil)
        (when pai--session
          (pai-todo--forget pai--session)
          (pai-todo--refresh-widgets pai--session))))))

(pai-register-extension
 (lambda (api)
   (pai-ext-register-tool api pai-todo-tool)
   (pai-ext-register-command
    api "todo"
    :description "Show or change the agent's todo list"
    :handler #'pai-todo-command
    :arg-completions (pai-command-completion-tree pai-todo-completion-tree))
   (dolist (ev '(session-start session-tree reload))
     (pai-ext-on api ev #'pai-todo--refresh-buffer))
   ;; a prompt of yours starts a fresh reminder budget
   (pai-ext-on api 'input
               (lambda (_event ctx)
                 (let ((buf (plist-get ctx :buffer)))
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (setq pai-todo--reminders 0 pai-todo--awaiting nil))))
                 nil))
   ;; the agent acted after a reminder
   (pai-ext-on api 'tool-result
               (lambda (_event ctx)
                 (let ((buf (plist-get ctx :buffer)))
                   (when (buffer-live-p buf)
                     (with-current-buffer buf (setq pai-todo--awaiting nil))))
                 nil))
   (pai-ext-on api 'agent-settled #'pai-todo--on-settled))
 "todo")

(with-eval-after-load 'pai-settings-ui
  (pai-settings-ui-register-section 'todo "Todo" 49)
  (pai-settings-ui-register-subsection 'todo 'panel "Panel above the prompt" 5)
  (pai-settings-ui-register-item
   'todo 'panel
   :key :todo-hud :type 'boolean :label "Show the todo panel"
   :doc "The current phase and its open tasks, above the input line (/todo expand shows all)"
   :get #'pai-todo-hud-p
   :set (lambda (v) (pai-todo--set-setting :hud (if v t :false))
          (dolist (b (buffer-list))
            (when (and (eq (buffer-local-value 'major-mode b) 'pai-mode)
                       (buffer-local-value 'pai--session b))
              (pai-todo--refresh-widgets (buffer-local-value 'pai--session b))))))
  (pai-settings-ui-register-item
   'todo 'panel
   :key :todo-hud-clear-delay :type 'number :label "Hide a finished list after (s)"
   :doc "Seconds after every task is closed before the panel goes away (-1: never)"
   :get #'pai-todo-hud-clear-delay
   :set (lambda (v) (pai-todo--set-setting :hud-clear-delay (or v 60))))
  (pai-settings-ui-register-subsection 'todo 'reminders "Reminders" 10)
  (pai-settings-ui-register-item
   'todo 'reminders
   :key :todo-reminders :type 'boolean :label "Remind about open tasks"
   :doc "When the agent stops with open todo tasks, send it a reminder"
   :get #'pai-todo-reminders-p
   :set (lambda (v) (pai-todo--set-setting :reminders (if v t :false))))
  (pai-settings-ui-register-item
   'todo 'reminders
   :key :todo-reminders-max :type 'number :label "Reminders per prompt"
   :doc "At most this many reminders after each prompt of yours"
   :get #'pai-todo-reminders-max
   :set (lambda (v) (pai-todo--set-setting :reminders-max (or v 0)))))

(provide 'pai-todo)
;;; pai-todo.el ends here
