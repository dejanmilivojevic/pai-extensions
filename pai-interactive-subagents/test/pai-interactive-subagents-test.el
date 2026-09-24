;;; pai-interactive-subagents-test.el --- Tests for interactive subagents -*- lexical-binding: t; -*-

;;; Commentary:

;; The extension is deliberately split so the parts can be tested without a
;; live LLM: a mock BACKEND stands in for a child session, and the child-side
;; hooks are plain functions over buffer-local state.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-interactive-subagents)

;;;; Harness

(defvar pai-isub-test--events nil "Backend calls recorded by the mock backend.")
(defvar pai-isub-test--emit nil "The last child's `emit' callback.")
(defvar pai-isub-test--buffer nil "Buffer standing in for the child session.")

(defun pai-isub-test--install-mock-backend ()
  "Register a mock backend that records what the runs layer asks of it."
  (setq pai-isub-test--events nil
        pai-isub-test--emit nil
        pai-isub-test--buffer (generate-new-buffer " *isub-child*"))
  (pai-isub-register-backend
   (list :name "mock"
         :label "mock session"
         :start (lambda (spec)
                  (push (cons 'start spec) pai-isub-test--events)
                  (setq pai-isub-test--emit (plist-get spec :emit))
                  (list :buffer pai-isub-test--buffer :spec spec))
         :send (lambda (_handle text)
                 (push (cons 'send text) pai-isub-test--events) t)
         :interrupt (lambda (_handle)
                      (push (cons 'interrupt nil) pai-isub-test--events) t)
         :close (lambda (_handle)
                  (push (cons 'close nil) pai-isub-test--events)
                  (when (buffer-live-p pai-isub-test--buffer)
                    (kill-buffer pai-isub-test--buffer)))
         :transcript (lambda (_handle &optional _n) "TRANSCRIPT"))))

(defun pai-isub-test--sent ()
  "Return every text handed to the mock backend, oldest first."
  (reverse (mapcar #'cdr (seq-filter (lambda (e) (eq (car e) 'send))
                                     pai-isub-test--events))))

(defun pai-isub-test--setup (&optional config)
  "Create a parent-like pai instance buffer using CONFIG as settings."
  (pai-isub-test--install-mock-backend)
  (let ((buf (generate-new-buffer " *isub-parent*")))
    (with-current-buffer buf
      (pai-ext-initialize-instance)
      (unless (pai-model "faux/faux")
        (pai-register-model (pai-make-model :id "faux" :provider "faux" :api 'faux)))
      (setq pai--model (pai-model "faux/faux")
            pai--active nil
            pai--context-messages nil
            pai--trusted t)
      (setq-local pai-isub--runs nil)
      (setq-local pai-isub--roles nil)
      (setq-local pai-settings--project
                  (list pai-isub-settings-key
                        (append '(:default-backend "mock") config))))
    buf))

(defun pai-isub-test--teardown (buf)
  "Kill BUF and the mock child buffer."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (timerp pai-isub--ui-timer) (cancel-timer pai-isub--ui-timer)))
    (kill-buffer buf))
  (when (buffer-live-p pai-isub-test--buffer) (kill-buffer pai-isub-test--buffer)))

(defun pai-isub-test--execute (args buf)
  "Call the subagent tool in BUF with ARGS; return (RESULT DELIVERED)."
  (let (result delivered)
    (with-current-buffer buf
      (funcall (plist-get pai-isub-tool :execute)
               args
               (list :cwd default-directory :model pai--model)
               nil
               (lambda (r) (setq result r delivered t))))
    (list result delivered)))

(defmacro pai-isub-test--with-quiet-parent (&rest body)
  "Run BODY with the parent's rendering and run-start stubbed out."
  (declare (indent 0))
  `(let ((pai-isub-test--started nil))
     (cl-letf (((symbol-function 'pai--render-note) (lambda (&rest _) nil))
               ((symbol-function 'pai--start-run)
                (lambda (text) (push text pai-isub-test--started) t)))
       ,@body)))

(defvar pai-isub-test--started nil "Texts passed to a stubbed `pai--start-run'.")

;;;; Launching and talking

(ert-deftest pai-isub-launch-opens-session-and-sends-task ()
  "A launch starts a backend session, sends the task, and answers at once."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (let* ((res (nth 0 (pai-isub-test--execute
                              (list :agent "scout" :task "map the repo") buf)))
                 (text (pai-content-text (plist-get res :content))))
            (should (eq (plist-get res :is-error) :false))
            (should (string-match-p "Opened subagent sub-" text))
            ;; the task reached the child, tagged as coming from the parent
            (should (string-match-p "map the repo" (car (pai-isub-test--sent))))
            (should (string-match-p "parent agent" (car (pai-isub-test--sent))))
            (with-current-buffer buf
              (let ((entry (car pai-isub--runs)))
                (should (equal (plist-get entry :role) "scout"))
                (should (equal (plist-get entry :backend) "mock"))
                (should (plist-get entry :awaiting))
                (should (pai-isub-live-p entry))))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-turn-end-delivers-to-parent ()
  "A finished child turn is delivered as a follow-up parent turn."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (funcall pai-isub-test--emit 'turn-end :text "found three files")
          (should (= 1 (length pai-isub-test--started)))
          (should (string-match-p "found three files" (car pai-isub-test--started)))
          (with-current-buffer buf
            (let ((entry (car pai-isub--runs)))
              (should (equal (plist-get entry :status) "idle"))
              (should-not (plist-get entry :awaiting))
              (should (equal (plist-get entry :last-output) "found three files"))
              (should (= 1 (plist-get entry :turns))))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-user-driven-turn-is-not-reported ()
  "Turns the parent did not ask for stay in the child unless configured."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (funcall pai-isub-test--emit 'turn-end :text "first")
          (setq pai-isub-test--started nil)
          ;; a turn the user typed into the child buffer: nothing awaited
          (funcall pai-isub-test--emit 'turn-end :text "user driven")
          (should (null pai-isub-test--started))
          ;; an explicit reply always reaches the parent
          (funcall pai-isub-test--emit 'reply :text "question for you")
          (should (= 1 (length pai-isub-test--started)))
          (should (string-match-p "question for you" (car pai-isub-test--started))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-report-all-turns-setting ()
  "With :report-all-turns every child turn is delivered."
  (let ((buf (pai-isub-test--setup '(:report-all-turns t))))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (funcall pai-isub-test--emit 'turn-end :text "first")
          (setq pai-isub-test--started nil)
          (funcall pai-isub-test--emit 'turn-end :text "user driven")
          (should (= 1 (length pai-isub-test--started))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-steers-a-busy-parent ()
  "When the parent is mid-run the child's output is queued as steering."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (with-current-buffer buf (setq pai--active t))
          (funcall pai-isub-test--emit 'turn-end :text "done while busy")
          (should (null pai-isub-test--started))
          (with-current-buffer buf
            (should (= 1 (length pai--steering-queue)))
            (should (string-match-p "done while busy"
                                    (pai-message-content (car pai--steering-queue))))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-foreground-call-waits-for-the-turn ()
  "async false keeps the tool call pending until the child's turn ends."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (let ((res (pai-isub-test--execute
                      (list :agent "oracle" :task "t" :async :false) buf)))
            (should-not (nth 1 res)))          ; nothing delivered yet
          (funcall pai-isub-test--emit 'turn-end :text "considered opinion")
          ;; the pending call completed with the child's text, not a receipt
          (with-current-buffer buf
            (should-not (plist-get (car pai-isub--runs) :on-done))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-say-continues-a-live-session ()
  "say sends another message to an existing session and awaits its turn."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (funcall pai-isub-test--emit 'turn-end :text "first")
          (let* ((id (with-current-buffer buf (plist-get (car pai-isub--runs) :id)))
                 (res (nth 0 (pai-isub-test--execute
                              (list :action "say" :id id :task "and the tests?") buf))))
            (should (eq (plist-get res :is-error) :false))
            (should (string-match-p "and the tests?" (car (last (pai-isub-test--sent)))))
            (with-current-buffer buf
              (should (plist-get (car pai-isub--runs) :awaiting)))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-stop-keeps-session-close-ends-it ()
  "stop interrupts the turn; close tears the session down."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (let ((id (with-current-buffer buf (plist-get (car pai-isub--runs) :id))))
            (pai-isub-test--execute (list :action "stop" :id id) buf)
            (should (assq 'interrupt pai-isub-test--events))
            (with-current-buffer buf
              (should (equal (plist-get (car pai-isub--runs) :status) "idle"))
              (should (pai-isub-live-p (car pai-isub--runs))))
            (pai-isub-test--execute (list :action "close" :id id) buf)
            (should (assq 'close pai-isub-test--events))
            (with-current-buffer buf
              (should (equal (plist-get (car pai-isub--runs) :status) "closed"))
              (should-not (pai-isub-live-p (car pai-isub--runs))))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-exit-completes-a-pending-call ()
  "Closing a session with a foreground call pending does not hang it."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (let (result)
            (with-current-buffer buf
              (funcall (plist-get pai-isub-tool :execute)
                       (list :agent "scout" :task "t" :async :false)
                       (list :cwd default-directory :model pai--model) nil
                       (lambda (r) (setq result r))))
            (should-not result)
            (funcall pai-isub-test--emit 'exit :reason 'killed)
            (should result)
            (should (string-match-p "closed"
                                    (pai-content-text (plist-get result :content))))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-status-read-and-list-actions ()
  "status, read and list answer without touching a model."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (let ((r (nth 0 (pai-isub-test--execute (list :action "status") buf))))
            (should (string-match-p "No subagent sessions"
                                    (pai-content-text (plist-get r :content)))))
          (let ((r (nth 0 (pai-isub-test--execute (list :action "list") buf))))
            (should (string-match-p "scout:" (pai-content-text (plist-get r :content))))
            (should (string-match-p "oracle:" (pai-content-text (plist-get r :content)))))
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (let ((r (nth 0 (pai-isub-test--execute (list :action "read") buf))))
            (should (equal (pai-content-text (plist-get r :content)) "TRANSCRIPT")))
          (let ((r (nth 0 (pai-isub-test--execute (list :action "status") buf))))
            (should (string-match-p "scout" (pai-content-text (plist-get r :content))))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-unknown-role-and-backend-are-errors ()
  "Bad input comes back as a tool error, never as a signal."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (let ((r (nth 0 (pai-isub-test--execute
                           (list :agent "nope" :task "t") buf))))
            (should (eq (plist-get r :is-error) t)))
          (let ((r (nth 0 (pai-isub-test--execute
                           (list :agent "scout" :task "t" :backend "nowhere") buf))))
            (should (eq (plist-get r :is-error) t)))
          (let ((r (nth 0 (pai-isub-test--execute
                           (list :action "say" :id "sub-999" :task "x") buf))))
            (should (eq (plist-get r :is-error) t))))
      (pai-isub-test--teardown buf))))

;;;; Roles, overrides and resolution

(ert-deftest pai-isub-override-round-trips-through-settings ()
  "Overrides are stored as a JSON object so they survive a settings reload."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (with-current-buffer buf
          (pai-register-model (pai-make-model :id "m2" :provider "faux"))
          (cl-letf (((symbol-function 'pai-settings-save) (lambda (&rest _) nil)))
            (pai-isub-set-override "reviewer" :model "faux/m2")
            (pai-isub-set-override "reviewer" :thinking "high")
            (pai-isub-set-override "reviewer" :backend "mock")
            ;; simulate a settings file round trip
            (let* ((json (pai-json-encode
                          (list pai-isub-settings-key (pai-isub-config))))
                   (back (pai-json-decode json)))
              (setq-local pai-settings--project back))
            (should (equal (plist-get (pai-isub-override "reviewer") :model) "faux/m2"))
            (should (equal (pai-isub-role-thinking-display "reviewer") "high"))
            (should (equal (pai-isub-resolve-backend "reviewer") "mock"))
            (let ((resolved (pai-isub-resolve-model "reviewer" nil nil)))
              (should (equal (pai-model-key (car resolved)) "faux/m2"))
              (should (eq (cadr resolved) 'high)))
            ;; clearing falls back through the chain
            (pai-isub-set-override "reviewer" :model "inherit")
            (should (equal (pai-model-key
                            (car (pai-isub-resolve-model "reviewer" nil pai--model)))
                           "faux/faux"))))
      (remhash "faux/m2" pai--models)
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-resolution-precedence ()
  "Per-run > override > frontmatter > default > parent, for model and backend."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (with-current-buffer buf
          (pai-register-model (pai-make-model :id "m1" :provider "faux"))
          (pai-register-model (pai-make-model :id "m3" :provider "faux"))
          (cl-letf (((symbol-function 'pai-settings-save) (lambda (&rest _) nil)))
            (pai-isub-config-set :default-model "faux/m1")
            (should (equal (pai-model-key (car (pai-isub-resolve-model "scout" nil nil)))
                           "faux/m1"))
            (pai-isub-set-override "scout" :model "faux/m3")
            (should (equal (pai-model-key (car (pai-isub-resolve-model "scout" nil nil)))
                           "faux/m3"))
            ;; per-run wins and may carry a thinking suffix
            (let ((r (pai-isub-resolve-model "scout" "faux/m1:low" nil)))
              (should (equal (pai-model-key (car r)) "faux/m1"))
              (should (eq (cadr r) 'low)))
            ;; backend: default from settings, per-run beats it
            (should (equal (pai-isub-resolve-backend "scout") "mock"))
            (should (equal (pai-isub-resolve-backend "scout" "pai") "pai"))))
      (remhash "faux/m1" pai--models)
      (remhash "faux/m3" pai--models)
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-role-files-load-with-backend ()
  "Role files provide prompt, model, tools, context and backend."
  (let ((buf (pai-isub-test--setup))
        (dir (make-temp-file "pai-isub-roles" t)))
    (unwind-protect
        (with-current-buffer buf
          (make-directory (expand-file-name "subagents" dir) t)
          (with-temp-file (expand-file-name "subagents/planner.md" dir)
            (insert "---\nname: planner\ndescription: Plans work\n"
                    "model: faux/faux\nthinking: high\nbackend: mock\n"
                    "tools: read, grep\ncontext: fork\n---\n"
                    "You are planner. Break the task into steps."))
          (let ((pai-directory dir))
            (pai-isub-load-roles))
          (let ((role (pai-isub-role "planner")))
            (should role)
            (should (equal (plist-get role :backend) "mock"))
            (should (equal (plist-get role :tools) '("read" "grep")))
            (should (eq (plist-get role :context) 'fork))
            (should (string-match-p "Break the task" (plist-get role :prompt))))
          (should (equal (pai-isub-resolve-backend "planner") "mock"))
          (should (eq (pai-isub-role-context-mode "planner") 'fork))
          (should (eq (pai-isub-role-context-mode "planner" "fresh") 'fresh)))
      (delete-directory dir t)
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-disabled-roles-disappear ()
  "A disabled builtin is no longer offered or launchable."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'pai-settings-save) (lambda (&rest _) nil)))
            (pai-isub-set-role-disabled "oracle" t)
            (should-not (pai-isub-role "oracle"))
            (should-not (member "oracle" (pai-isub-role-names)))
            (pai-isub-set-role-disabled "oracle" nil)
            (should (pai-isub-role "oracle"))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-fork-context-is-handed-to-the-backend ()
  "The fork context mode passes the parent's non-system messages to the child."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (with-current-buffer buf
            (setq pai--context-messages
                  (list (pai-system-message "PARENT-SYS") (pai-user-message "hi"))))
          (pai-isub-test--execute (list :agent "worker" :task "t" :context "fork") buf)
          (let* ((spec (cdr (assq 'start pai-isub-test--events)))
                 (inherited (plist-get spec :context-messages)))
            (should (= 1 (length inherited)))
            (should (equal (pai-message-content (car inherited)) "hi"))
            (should (string-match-p "implementation agent"
                                    (plist-get spec :role-prompt))))
          ;; fresh (the default for scout) inherits nothing
          (setq pai-isub-test--events nil)
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (should-not (plist-get (cdr (assq 'start pai-isub-test--events))
                                 :context-messages)))
      (pai-isub-test--teardown buf))))

;;;; Child-side behaviour (the pai backend)

(ert-deftest pai-isub-context-hook-injects-the-role-prompt ()
  "The role prompt is folded into the system message at request time only."
  (with-temp-buffer
    (setq pai-isub--id "sub-9" pai-isub--role "reviewer"
          pai-isub--role-prompt "You are reviewer.")
    (let* ((messages (list (pai-system-message "BASE") (pai-user-message "hi")))
           (out (plist-get (pai-isub-session-context-hook
                            (list :type 'context :messages messages) nil)
                           :messages))
           (system (car out)))
      (should (pai-system-message-p system))
      (should (string-match-p "BASE" (pai-message-content system)))
      (should (string-match-p "You are reviewer." (pai-message-content system)))
      (should (string-match-p "reply_to_parent" (pai-message-content system)))
      ;; the original messages are untouched (nothing is persisted)
      (should (equal (pai-message-content (car messages)) "BASE"))
      (should (= (length out) 2)))
    ;; outside a subagent session the hook does nothing
    (setq pai-isub--role-prompt nil)
    (should-not (pai-isub-session-context-hook
                 (list :type 'context :messages nil) nil))))

(ert-deftest pai-isub-child-hooks-report-and-measure ()
  "The child's hooks emit busy/turn-end and track output tokens."
  (with-temp-buffer
    (let (events)
      (setq pai-isub--id "sub-4"
            pai-isub--role "worker"
            pai-isub--started (- (float-time) 4.0)
            pai-isub--emit (lambda (type &rest props) (push (cons type props) events)))
      (pai-isub-session-agent-start-hook nil nil)
      (should (eq (caar events) 'busy))
      (pai-isub-session-message-update-hook
       '(:type message-update :event (:type text-delta :delta "hello world!!")) nil)
      (pai-isub-session-message-update-hook
       '(:type message-update :event (:type thinking-delta :delta "abcd")) nil)
      (should (= pai-isub--stream-chars 17))
      (pai-isub-session-message-end-hook
       (list :type 'message-end
             :message (pai-assistant-message :usage (pai-usage :output 40)))
       nil)
      (should (= pai-isub--usage-tokens 40))
      (should (= pai-isub--stream-chars 0))
      (let ((metrics (pai-isub-session-metrics (list :buffer (current-buffer)))))
        (should (= (plist-get metrics :tokens) 40))
        (should (> (plist-get metrics :tps) 0)))
      (pai-isub-session-agent-end-hook
       (list :type 'agent-end
             :messages (list (pai-assistant-message
                              :content (list (pai-text "final report")))))
       nil)
      (should (eq (caar events) 'turn-end))
      (should (equal (plist-get (cdar events) :text) "final report")))))

(ert-deftest pai-isub-reply-tool-needs-a-parent ()
  "`reply_to_parent' emits upstream, and refuses when there is no parent."
  (with-temp-buffer
    (let (events result)
      (funcall (plist-get pai-isub-session-reply-tool :execute)
               (list :message "hi") nil nil (lambda (r) (setq result r)))
      (should (eq (plist-get result :is-error) t))
      (setq pai-isub--id "sub-5"
            pai-isub--emit (lambda (type &rest props) (push (cons type props) events)))
      (funcall (plist-get pai-isub-session-reply-tool :execute)
               (list :message "  ") nil nil (lambda (r) (setq result r)))
      (should (eq (plist-get result :is-error) t))
      (funcall (plist-get pai-isub-session-reply-tool :execute)
               (list :message "need a decision") nil nil (lambda (r) (setq result r)))
      (should (eq (plist-get result :is-error) :false))
      (should (eq (caar events) 'reply))
      (should (equal (plist-get (cdar events) :text) "need a decision")))))

(ert-deftest pai-isub-parent-command-reports-and-sends ()
  "/parent shows the link with no argument and messages the parent with one."
  (with-temp-buffer
    (let (events)
      (should (string-match-p "not a subagent"
                              (plist-get (pai-isub-session-parent-command "" nil) :message)))
      (setq pai-isub--id "sub-6" pai-isub--role "scout"
            pai-isub--parent (current-buffer)
            pai-isub--emit (lambda (type &rest props) (push (cons type props) events)))
      (should (string-match-p "sub-6"
                              (plist-get (pai-isub-session-parent-command "" nil) :message)))
      (pai-isub-session-parent-command "look at foo.el" nil)
      (should (eq (caar events) 'reply))
      (should (equal (plist-get (cdar events) :text) "look at foo.el")))))

(ert-deftest pai-isub-timeout-interrupts-but-keeps-the-session ()
  "A turn timeout interrupts the child and reports, leaving it usable."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t" :timeout 0.05) buf)
          (sleep-for 0.1)
          (pai-isub-test--pump 5)
          (should (assq 'interrupt pai-isub-test--events))
          (should (string-match-p "timeout" (car pai-isub-test--started)))
          (with-current-buffer buf
            (let ((entry (car pai-isub--runs)))
              (should (equal (plist-get entry :status) "idle"))
              (should-not (plist-get entry :awaiting))
              (should (pai-isub-live-p entry)))))
      (pai-isub-test--teardown buf))))

;;;; End to end, through the real pai backend

(defun pai-isub-test--pump (&optional n)
  "Let asynchronous callbacks run N times."
  (dotimes (_ (or n 20)) (accept-process-output nil 0.005)))

(ert-deftest pai-isub-end-to-end-pai-session ()
  "A real pai-backed child: own buffer, own turns, reporting back home."
  (let* ((home (make-temp-file "pai-isub-home" t))
         (project (file-name-as-directory (make-temp-file "pai-isub-proj" t)))
         (pai-directory home)
         (default-directory project)
         (pai-default-model "faux/faux")
         (parent (generate-new-buffer "*pai: isub-parent*"))
         child)
    (unwind-protect
        (progn
          (pai-faux-reset)
          (with-current-buffer parent
            (pai--setup project)
            (setq pai--model (pai-model "faux/faux"))
            (pai-faux-push '(:text "child says hello" :stop-reason stop))
            (let (receipt)
              (funcall (plist-get pai-isub-tool :execute)
                       (list :agent "scout" :task "look at foo.el")
                       (list :cwd default-directory :model pai--model) nil
                       (lambda (r) (setq receipt r)))
              (should (string-match-p "Opened subagent"
                                      (pai-content-text (plist-get receipt :content)))))
            (pai-isub-test--pump)
            (let ((entry (car pai-isub--runs)))
              (setq child (pai-isub-entry-buffer entry))
              (should (buffer-live-p child))
              (should (equal (plist-get entry :status) "idle"))
              (should (equal (plist-get entry :last-output) "child says hello"))
              ;; the child is a real, live chat session
              (with-current-buffer child
                (should (eq major-mode 'pai-mode))
                (should (equal pai-isub--role "scout"))
                (should (eq pai-isub--parent parent))
                (should (string-match-p "look at foo.el" (buffer-string)))
                (should (string-match-p "child says hello" (buffer-string)))
                ;; recursion guard, plus the way home
                (should-not (pai-tool-get "subagent"))
                (should (pai-tool-get pai-isub-reply-tool-name)))
              ;; the child's answer was delivered into the parent conversation
              (should (string-match-p "child says hello" (buffer-string)))
              ;; a turn the user drives in the child is not reported home
              (let ((before (buffer-string)))
                (pai-faux-push '(:text "answering the user" :stop-reason stop))
                (with-current-buffer child
                  (goto-char (point-max))
                  (insert "what about bar.el?")
                  (pai-send))
                (pai-isub-test--pump)
                (should (string-match-p "answering the user"
                                        (with-current-buffer child (buffer-string))))
                (should (equal before (buffer-string))))
              ;; ... but an explicit reply is
              (with-current-buffer child
                (funcall (plist-get (pai-tool-get pai-isub-reply-tool-name) :execute)
                         (list :message "I need a decision") nil nil #'ignore))
              (pai-isub-test--pump)
              (should (string-match-p "I need a decision" (buffer-string)))
              ;; and the parent can keep the session going
              (pai-faux-push '(:text "follow-up done" :stop-reason stop))
              (funcall (plist-get pai-isub-tool :execute)
                       (list :action "say" :id (plist-get entry :id) :task "check baz")
                       (list :cwd default-directory :model pai--model) nil #'ignore)
              (pai-isub-test--pump)
              (should (string-match-p "check baz"
                                      (with-current-buffer child (buffer-string))))
              (should (string-match-p "follow-up done" (buffer-string)))
              ;; closing takes the buffer with it
              (pai-isub-close entry)
              (should-not (buffer-live-p child))
              (should (equal (plist-get entry :status) "closed")))))
      (when (buffer-live-p parent)
        (with-current-buffer parent
          (when (timerp pai-isub--ui-timer) (cancel-timer pai-isub--ui-timer)))
        (kill-buffer parent))
      (when (buffer-live-p child) (kill-buffer child))
      (pai-faux-reset)
      (delete-directory home t)
      (delete-directory project t))))

;;;; Status block

(ert-deftest pai-isub-status-block-lists-live-sessions ()
  "Live sessions render one per line above the prompt; closed ones drop out."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (with-current-buffer buf
            (let ((text (pai-isub--block-string (pai-isub--shown-runs))))
              (should (string-match-p "sub-" text))
              (should (string-match-p "scout" text)))
            (pai-isub-close (car pai-isub--runs))
            (should (null (pai-isub--shown-runs)))
            (should (equal (pai-isub--block-string (pai-isub--shown-runs)) "\n"))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-display-halves-parent-window ()
  "Each child splits the parent's window in half, like `split-window-right'."
  (let ((parent (get-buffer-create " *isub-parent*"))
        (a (get-buffer-create " *isub-a*"))
        (b (get-buffer-create " *isub-b*"))
        (window-combination-resize t))
    (save-window-excursion
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer parent)
            (let* ((pwin (selected-window))
                   (w0 (window-total-width pwin))
                   (wa (pai-isub-display a parent))
                   (w1 (window-total-width pwin))
                   (wa1 (window-total-width wa))
                   (wb (pai-isub-display b parent)))
              (should (<= (abs (- w1 (/ w0 2))) 1))
              ;; The second child only takes space from the parent.
              (should (= (window-total-width wa) wa1))
              (should (<= (abs (- (window-total-width wb)
                                  (window-total-width pwin)))
                          1))
              (should (eq (window-in-direction 'right pwin) wb))))
        (mapc #'kill-buffer (list parent a b))))))

(provide 'pai-interactive-subagents-test)
;;; pai-interactive-subagents-test.el ends here
