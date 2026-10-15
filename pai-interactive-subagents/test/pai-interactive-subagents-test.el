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
      ;; like a real pai buffer, keep settings here: reading them must not
      ;; lazily load the global file into the global default (a leak into
      ;; later tests)
      (setq-local pai-settings--global nil)
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

(ert-deftest pai-isub-role-file-shadows-builtin-once ()
  "A role file overriding a builtin is listed once and reports its source."
  (let ((buf (pai-isub-test--setup))
        (home (make-temp-file "pai-isub-home" t))
        (proj (make-temp-file "pai-isub-proj" t)))
    (unwind-protect
        (with-current-buffer buf
          (make-directory (expand-file-name "subagents" home) t)
          (make-directory (expand-file-name ".pai/subagents" proj) t)
          (with-temp-file (expand-file-name "subagents/worker.md" home)
            (insert "---\nname: worker\ncontext: fresh\n---\nYou are worker."))
          (with-temp-file (expand-file-name ".pai/subagents/checker.md" proj)
            (insert "---\nname: checker\n---\nYou are checker."))
          (let ((pai-directory home))
            (pai-isub-load-roles proj))
          (should (= 1 (cl-count "worker" (pai-isub-role-names) :test #'equal)))
          (should (eq (pai-isub-role-context-mode "worker") 'fresh))
          (should (equal (pai-isub-role-source "worker") "user↻builtin"))
          (should (equal (pai-isub-role-source "checker") "project"))
          (should (equal (pai-isub-role-source "scout") "builtin"))
          (should-not (pai-isub-role-source "nonexistent")))
      (delete-directory home t)
      (delete-directory proj t)
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
                ;; marked as a subagent's session (memory leaves it alone)
                (should (eq pai-subagent-session parent))
                (should-not (pai-subagent-session-p parent))
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
  "The first child halves the parent's window; later ones share that half equally."
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
              (should (<= (abs (- wa1 w1)) 1))
              ;; The second child takes its room from the first, not the
              ;; parent, and both end up equally wide.
              (should (= (window-total-width pwin) w1))
              (should (<= (abs (- (window-total-width wb) (window-total-width wa))) 1))
              (should (eq (window-in-direction 'right pwin) wa))
              (should (eq (window-in-direction 'right wa) wb))))
        (mapc #'kill-buffer (list parent a b))))))

(ert-deftest pai-isub-child-drops-tools-kept-from-subagents ()
  "A child session never keeps a tool marked `:subagent-exclude'."
  (with-temp-buffer
    (pai-ext-initialize-instance)
    (pai-register-tool (list :name "x-memory" :description "d" :subagent-exclude t
                             :parameters (pai-object-schema nil) :execute #'ignore))
    (pai-register-tool (list :name "x-read" :description "d"
                             :parameters (pai-object-schema nil) :execute #'ignore))
    (pai-isub-session--restrict-tools nil)
    (should-not (pai-tool-get "x-memory"))
    (should (pai-tool-get "x-read"))
    (pai-unregister-tool "x-read")))

;;;; Window layout

(defun pai-isub-test--widths (parent)
  "Return (PARENT-WIDTH . SUBAGENT-WIDTHS) of the selected frame, left to right."
  (cons (window-total-width (get-buffer-window parent))
        (mapcar #'window-total-width (pai-isub-sibling-windows parent))))

(defun pai-isub-test--equal-p (widths)
  "Return non-nil when WIDTHS differ by at most one column."
  (<= (- (apply #'max widths) (apply #'min widths)) 1))

(ert-deftest pai-isub-subagent-windows-share-width-equally ()
  "Every new subagent joins the others; all are equally wide, the parent keeps half."
  (let ((parent (get-buffer-create " *isub-layout-parent*"))
        (subs (mapcar (lambda (i) (get-buffer-create (format " *isub-layout-%d*" i))) '(1 2 3)))
        (frame-cols (frame-width)))
    (save-window-excursion
      (unwind-protect
          (progn
            (switch-to-buffer parent)
            (delete-other-windows)
            (let ((half (window-total-width)))
              (dolist (b subs) (pai-isub-display b parent))
              (let ((w (pai-isub-test--widths parent)))
                (should (= (length (cdr w)) 3))
                ;; the parent kept the half it gave the first subagent
                (should (<= (abs (- (car w) (/ frame-cols 2))) 1))
                (should (pai-isub-test--equal-p (cdr w))))
              ;; closing one gives its room to the other subagents only
              (pai-isub-retire-windows (nth 1 subs) parent)
              (kill-buffer (nth 1 subs))
              (accept-process-output nil 0.05)
              (let ((w (pai-isub-test--widths parent)))
                (should (= (length (cdr w)) 2))
                (should (pai-isub-test--equal-p (cdr w)))
                (should (<= (abs (- (car w) (/ frame-cols 2))) 1))
                ;; no stray window was left showing the parent twice
                (should (= 1 (length (get-buffer-window-list parent nil t)))))
              (ignore half)))
        (dolist (b (cons parent subs)) (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest pai-isub-retire-keeps-a-window-reused-for-something-else ()
  (let ((parent (get-buffer-create " *isub-layout-parent*"))
        (sub (get-buffer-create " *isub-layout-sub*"))
        (other (get-buffer-create " *isub-layout-other*")))
    (save-window-excursion
      (unwind-protect
          (progn
            (switch-to-buffer parent)
            (delete-other-windows)
            (let ((win (pai-isub-display sub parent)))
              (should (window-parameter win 'pai-isub-window))
              ;; the user opens something else in that window
              (set-window-buffer win other)
              (pai-isub-retire-windows sub parent)
              (kill-buffer sub)
              (accept-process-output nil 0.05)
              (should (window-live-p win))
              (should (eq (window-buffer win) other))))
        (dolist (b (list parent sub other)) (when (buffer-live-p b) (kill-buffer b)))))))

;;;; Closing idle sessions

(defun pai-isub-test--wait (seconds)
  "Let timers run for SECONDS."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end) (accept-process-output nil 0.02))))

(ert-deftest pai-isub-idle-close-setting ()
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (with-current-buffer buf
          (should (= (pai-isub-idle-close-seconds) 60))
          (setq-local pai-settings--project
                      (list pai-isub-settings-key '(:default-backend "mock" :idle-close-seconds 0)))
          (should-not (pai-isub-idle-close-seconds))
          (setq-local pai-settings--project
                      (list pai-isub-settings-key '(:default-backend "mock" :idle-close-seconds 90)))
          (should (= (pai-isub-idle-close-seconds) 90)))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-idle-timer-outliving-its-parent-does-nothing ()
  "With the parent gone, an idle check neither closes nor reads settings elsewhere."
  (let* ((dead (generate-new-buffer " *isub-dead-parent*"))
         (entry (list :id "sub-x" :status "idle" :parent dead :backend "mock")))
    (kill-buffer dead)
    (should-not (pai-isub-idle-close-seconds dead))
    (cl-letf (((symbol-function 'pai-settings-get)
               (lambda (&rest _) (error "settings read without a parent")))
              ((symbol-function 'pai-isub-close)
               (lambda (&rest _) (error "closed without a parent"))))
      (pai-isub--idle-check entry))
    (should-not (plist-get entry :idle-timer))))

(ert-deftest pai-isub-finished-subagent-closes-when-left-alone ()
  (let ((buf (pai-isub-test--setup '(:idle-close-seconds 0.2))))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (let ((entry (with-current-buffer buf (car pai-isub--runs))))
            ;; still working: never closed
            (funcall pai-isub-test--emit 'busy)
            (pai-isub-test--wait 0.35)
            (should (pai-isub-live-p entry))
            ;; done and untouched: closed after the delay
            (funcall pai-isub-test--emit 'turn-end :text "done")
            (should (pai-isub-live-p entry))
            (pai-isub-test--wait 0.4)
            (should (equal (plist-get entry :status) "closed"))
            (should (assq 'close pai-isub-test--events))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-input-in-the-subagent-keeps-it-open ()
  (let ((buf (pai-isub-test--setup '(:idle-close-seconds 0.3))))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (let ((entry (with-current-buffer buf (car pai-isub--runs))))
            (funcall pai-isub-test--emit 'turn-end :text "done")
            (pai-isub-test--wait 0.2)
            ;; a command in the subagent's buffer restarts the clock
            (with-current-buffer pai-isub-test--buffer (run-hooks 'post-command-hook))
            (pai-isub-test--wait 0.2)
            (should (pai-isub-live-p entry))
            ;; unsent text at its prompt keeps it open indefinitely
            (cl-letf (((symbol-function 'pai-isub--pending-input-p) (lambda (_) t)))
              (pai-isub-test--wait 0.5)
              (should (pai-isub-live-p entry)))
            ;; left alone again: it closes
            (pai-isub-test--wait 0.5)
            (should (equal (plist-get entry :status) "closed"))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-idle-close-off-and-waiting-parent ()
  "With 0 nothing closes; a subagent the parent still waits for never closes."
  (let ((buf (pai-isub-test--setup '(:idle-close-seconds 0))))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (let ((entry (with-current-buffer buf (car pai-isub--runs))))
            (funcall pai-isub-test--emit 'turn-end :text "done")
            (should-not (plist-get entry :idle-timer))
            (pai-isub-test--wait 0.2)
            (should (pai-isub-live-p entry))))
      (pai-isub-test--teardown buf)))
  (let ((buf (pai-isub-test--setup '(:idle-close-seconds 0.1))))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (let ((entry (with-current-buffer buf (car pai-isub--runs))))
            ;; the task turn is still awaited by the parent
            (should (plist-get entry :awaiting))
            (pai-isub-test--wait 0.3)
            (should (pai-isub-live-p entry))))
      (pai-isub-test--teardown buf))))

;;;; Rate limits

(ert-deftest pai-isub-rate-limit-wait-keeps-the-child-open ()
  "A child waiting out a rate limit is neither idle-closed nor timed out."
  (let ((buf (pai-isub-test--setup '(:idle-close-seconds 0.1))))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--execute (list :agent "scout" :task "t" :timeout 0.15) buf)
          (let ((entry (with-current-buffer buf (car pai-isub--runs)))
                (until (+ (float-time) 3600)))
            (funcall pai-isub-test--emit 'busy)
            (funcall pai-isub-test--emit 'waiting :until until :text "rate limited")
            (should (equal (plist-get entry :status) "waiting"))
            (should (= (plist-get entry :retry-at) until))
            (should (string-match-p "rate limited, retry"
                                    (pai-isub--run-line entry)))
            ;; longer than both the idle delay and the turn timeout
            (pai-isub-test--wait 0.35)
            (should (pai-isub-live-p entry))
            (should-not (assq 'interrupt pai-isub-test--events))
            (should (plist-get entry :awaiting))
            ;; retrying: running again, and the turn timeout is back
            (funcall pai-isub-test--emit 'busy)
            (should (equal (plist-get entry :status) "running"))
            (should-not (plist-get entry :retry-at))
            (pai-isub-test--wait 0.3)
            (should (assq 'interrupt pai-isub-test--events))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-end-to-end-rate-limited-child-resumes ()
  "A real pai child hit by a rate limit waits, retries, and reports home."
  (let* ((home (make-temp-file "pai-isub-home" t))
         (project (file-name-as-directory (make-temp-file "pai-isub-proj" t)))
         (pai-directory home)
         (default-directory project)
         (pai-default-model "faux/faux")
         (pai-rate-limit-backoff-initial 0.3)
         (parent (generate-new-buffer "*pai: isub-rl-parent*"))
         child)
    (unwind-protect
        (progn
          (pai-faux-reset)
          (with-current-buffer parent
            (pai--setup project)
            (setq pai--model (pai-model "faux/faux"))
            (pai-faux-push '(:error "HTTP 429: {\"type\":\"rate_limit_error\"}")
                           '(:text "child made it after the wait" :stop-reason stop))
            (funcall (plist-get pai-isub-tool :execute)
                     (list :agent "scout" :task "look at foo.el")
                     (list :cwd default-directory :model pai--model) nil #'ignore)
            (pai-isub-test--pump 5)
            (let ((entry (car pai-isub--runs)))
              (setq child (pai-isub-entry-buffer entry))
              (should (equal (plist-get entry :status) "waiting"))
              (with-current-buffer child
                (should pai--active)
                (should (string-match-p "Rate limited by faux" (buffer-string))))
              (pai-isub-test--wait 0.6)
              (should (equal (plist-get entry :status) "idle"))
              (with-current-buffer child
                (should-not pai--active)
                (should (string-match-p "retrying the turn" (buffer-string))))
              (should (equal (plist-get entry :last-output) "child made it after the wait"))
              (should (string-match-p "child made it after the wait" (buffer-string))))))
      (when (buffer-live-p parent)
        (with-current-buffer parent
          (when (timerp pai-isub--ui-timer) (cancel-timer pai-isub--ui-timer)))
        (kill-buffer parent))
      (when (buffer-live-p child) (kill-buffer child))
      (pai-faux-reset)
      (delete-directory home t)
      (delete-directory project t))))

;;;; Per-model backends

(ert-deftest pai-isub-model-backend-mapping-round-trips ()
  "A model's backend entry persists through JSON and can be removed."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'pai-settings-save) (lambda (&rest _) nil)))
            (pai-isub-set-model-backend "faux/faux" (list :backend "agent-shell"
                                                          :agent "claude-code"
                                                          :agent-model ""))
            (pai-isub-set-model-backend "other/x:free" (list :backend "pai"))
            (let* ((json (pai-json-encode (list pai-isub-settings-key (pai-isub-config))))
                   (back (pai-json-decode json)))
              (setq-local pai-settings--project back))
            (should (equal (pai-isub-model-backend pai--model)
                           '(:backend "agent-shell" :agent "claude-code")))
            (should (equal (plist-get (pai-isub-model-backend "other/x:free") :backend) "pai"))
            (should (equal (mapcar #'car (pai-isub-model-backends))
                           '("faux/faux" "other/x:free")))
            ;; "inherit" removes the mapping, others stay
            (pai-isub-set-model-backend "faux/faux" (list :backend "inherit"))
            (should-not (pai-isub-model-backend "faux/faux"))
            (should (pai-isub-model-backend "other/x:free"))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-model-backend-precedence ()
  "The model's backend beats the default, but not a per-run or role choice."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'pai-settings-save) (lambda (&rest _) nil)))
            (pai-isub-set-model-backend "faux/faux" (list :backend "agent-shell" :agent "gemini-cli"))
            (should (equal (pai-isub-resolve-backend "scout" nil pai--model) "agent-shell"))
            (should (equal (pai-isub-resolve-backend "scout" nil "faux/faux") "agent-shell"))
            ;; no model: the default backend
            (should (equal (pai-isub-resolve-backend "scout") "mock"))
            (should (equal (pai-isub-resolve-backend "scout" "pai" pai--model) "pai"))
            (pai-isub-set-override "scout" :backend "pai")
            (should (equal (pai-isub-resolve-backend "scout" nil pai--model) "pai"))
            ;; options only for the backend the model is mapped to
            (should (equal (plist-get (pai-isub-backend-options "agent-shell" pai--model) :agent)
                           "gemini-cli"))
            (should-not (pai-isub-backend-options "pai" pai--model))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-launch-uses-model-backend-and-options ()
  "A launch on a mapped model runs in its backend with its options."
  (let ((buf (pai-isub-test--setup '(:default-backend "pai"))))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (with-current-buffer buf
            (cl-letf (((symbol-function 'pai-settings-save) (lambda (&rest _) nil)))
              (pai-isub-set-model-backend "faux/faux" (list :backend "mock" :agent "a1"))))
          (pai-isub-test--execute (list :agent "scout" :task "t") buf)
          (let ((spec (cdr (assq 'start pai-isub-test--events)))
                (entry (with-current-buffer buf (car pai-isub--runs))))
            (should (equal (plist-get entry :backend) "mock"))
            (should (equal (plist-get (plist-get spec :backend-options) :agent) "a1"))))
      (pai-isub-test--teardown buf))))

;;;; agent-shell backend (agent-shell itself stubbed)

(defvar pai-isub-test--shell-sub nil "The backend's agent-shell subscriber.")
(defvar pai-isub-test--shell-inserted nil "Texts submitted to the fake shell.")
(defvar pai-isub-test--shell-status 'ready "What the fake `agent-shell-status' says.")
(defvar pai-isub-test--shell-config nil "Config the fake shell was started with.")
(defvar pai-isub-test--shell-buffer nil "The fake shell buffer.")
(defvar pai-isub-test--shell-responses nil "Permission answers sent by the backend.")

(defmacro pai-isub-test--with-fake-agent-shell (&rest body)
  "Run BODY with agent-shell's entry points replaced by fakes."
  (declare (indent 0))
  `(let ((pai-isub-test--shell-sub nil)
         (pai-isub-test--shell-buffer nil)
         (pai-isub-test--shell-responses nil)
         (pai-isub-test--shell-inserted nil)
         (pai-isub-test--shell-status 'ready)
         (pai-isub-test--shell-config nil))
     (cl-letf (((symbol-function 'pai-isub-agent-shell-available-p) (lambda () t))
               ((symbol-function 'agent-shell--resolved-agent-configs)
                (lambda () (list '((:identifier . claude-code) (:buffer-name . "Claude"))
                                 '((:identifier . gemini-cli)))))
               ((symbol-function 'agent-shell--start)
                (lambda (&rest args)
                  (setq pai-isub-test--shell-config (plist-get args :config))
                  (setq pai-isub-test--shell-buffer (generate-new-buffer " *fake-agent-shell*"))))
               ((symbol-function 'agent-shell--send-permission-response)
                (lambda (&rest args)
                  (push (plist-get args :option-id) pai-isub-test--shell-responses)))
               ((symbol-function 'agent-shell-subscribe-to)
                (lambda (&rest args) (setq pai-isub-test--shell-sub (plist-get args :on-event)) 1))
               ((symbol-function 'agent-shell-status) (lambda (&rest _) pai-isub-test--shell-status))
               ((symbol-function 'agent-shell-insert)
                (lambda (&rest args)
                  (push (plist-get args :text) pai-isub-test--shell-inserted)))
               ((symbol-function 'agent-shell-interrupt) (lambda (&rest _) nil))
               ((symbol-function 'pai-isub-display) (lambda (&rest _) nil)))
       ,@body)))

(defun pai-isub-test--shell-event (event &rest data)
  "Send EVENT with DATA (an alist) to the backend's subscriber."
  (with-current-buffer pai-isub-test--shell-buffer
    (funcall pai-isub-test--shell-sub
             (append (list (cons :event event)) (when data (list (cons :data (car data))))))))

(ert-deftest pai-isub-agent-shell-backend-round-trip ()
  "An agent-shell child gets the role preamble, reports its final message,
queues while busy, and reports its exit."
  (let ((buf (pai-isub-test--setup '(:default-backend "agent-shell"))))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--with-fake-agent-shell
            (with-current-buffer buf
              (cl-letf (((symbol-function 'pai-settings-save) (lambda (&rest _) nil)))
                (pai-isub-set-model-backend "faux/faux" (list :backend "agent-shell"
                                                              :agent "claude-code"
                                                              :agent-model "opus"))))
            (let* ((res (car (pai-isub-test--execute
                              (list :agent "scout" :task "map the repo") buf)))
                   (entry (with-current-buffer buf (car pai-isub--runs)))
                   (handle (plist-get entry :handle)))
              (should (eq (plist-get res :is-error) :false))
              (should (equal (plist-get entry :backend) "agent-shell"))
              (should (equal (plist-get entry :model) "claude-code/opus"))
              ;; the agent's model is selected through the copied config
              (should (equal (funcall (alist-get :default-model-id pai-isub-test--shell-config))
                             "opus"))
              ;; nothing is submitted before the session is ready
              (pai-isub-test--wait 0.05)
              (should-not pai-isub-test--shell-inserted)
              (pai-isub-test--shell-event 'init-finished)
              (pai-isub-test--wait 0.05)
              (should (= 1 (length pai-isub-test--shell-inserted)))
              (let ((first (car pai-isub-test--shell-inserted)))
                (should (string-match-p "<subagent_session role=\"scout\"" first))
                (should (string-match-p "map the repo" first)))
              (pai-isub-test--shell-event 'input-submitted)
              (should (equal (plist-get entry :status) "running"))
              ;; a follow-up while busy is queued, without the preamble
              (pai-isub-say entry "and the tests" nil nil)
              (pai-isub-test--wait 0.05)
              (should (= 1 (length pai-isub-test--shell-inserted)))
              ;; only the message after the last tool call is the report
              (pai-isub-test--shell-event 'agent-message-chunk '((:text-chunk . "Let me look.")))
              (pai-isub-test--shell-event 'tool-call-update)
              (pai-isub-test--shell-event 'agent-message-chunk '((:text-chunk . "Found ")))
              (pai-isub-test--shell-event 'agent-message-chunk '((:text-chunk . "3 files.")))
              (pai-isub-test--shell-event 'turn-complete '((:stop-reason . "end_turn")))
              (should (equal (plist-get entry :last-output) "Found 3 files."))
              (should (string-match-p "Found 3 files\\." (car pai-isub-test--started)))
              ;; the queued message goes out once the turn is over
              (pai-isub-test--wait 0.05)
              (should (= 2 (length pai-isub-test--shell-inserted)))
              (should (string-match-p "and the tests" (car pai-isub-test--shell-inserted)))
              (should-not (string-match-p "subagent_session" (car pai-isub-test--shell-inserted)))
              ;; killing the shell ends the session
              (pai-isub-agent-shell-close handle)
              (should (equal (plist-get entry :status) "closed")))))
      (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-agent-shell-needs-an-agent ()
  "Starting without a configured agent is a clear error."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (pai-isub-test--with-quiet-parent
          (pai-isub-test--with-fake-agent-shell
            (let ((res (car (pai-isub-test--execute
                             (list :agent "scout" :task "t" :backend "agent-shell") buf))))
              (should (eq (plist-get res :is-error) t))
              (should (string-match-p "No agent-shell agent"
                                      (pai-content-text (plist-get res :content)))))
            ;; the setting supplies one
            (with-current-buffer buf
              (setq-local pai-settings--project
                          (list pai-isub-settings-key
                                '(:default-backend "agent-shell" :agent-shell-agent "gemini-cli"))))
            (let ((res (car (pai-isub-test--execute (list :agent "scout" :task "t") buf))))
              (should (eq (plist-get res :is-error) :false))
              (should (eq (alist-get :identifier pai-isub-test--shell-config) 'gemini-cli))
              (pai-isub-agent-shell-close
               (plist-get (with-current-buffer buf (car pai-isub--runs)) :handle)))))
      (pai-isub-test--teardown buf))))

(defmacro pai-isub-test--with-shell-child (spec-args &rest body)
  "Launch an agent-shell child with SPEC-ARGS bound as ENTRY and HANDLE in BODY."
  (declare (indent 1))
  `(let ((buf (pai-isub-test--setup)))
     (unwind-protect
         (pai-isub-test--with-quiet-parent
           (pai-isub-test--with-fake-agent-shell
             (with-current-buffer buf
               (cl-letf (((symbol-function 'pai-settings-save) (lambda (&rest _) nil)))
                 (pai-isub-set-model-backend "faux/faux" (list :backend "agent-shell"
                                                               :agent "claude-code"))))
             (pai-isub-test--execute (append (list :agent "scout" :task "go") ,spec-args) buf)
             (let* ((entry (with-current-buffer buf (car pai-isub--runs)))
                    (handle (plist-get entry :handle)))
               (ignore entry handle)
               (unwind-protect (progn ,@body)
                 (pai-isub-agent-shell-close handle)))))
       (pai-isub-test--teardown buf))))

(ert-deftest pai-isub-agent-shell-reply-to-parent-over-mcp ()
  "The child gets a pai-parent MCP server whose reply_to_parent reaches the parent."
  (pai-isub-test--with-shell-child nil
    (let* ((servers (alist-get :mcp-servers pai-isub-test--shell-config))
           (server (seq-find (lambda (s) (equal (alist-get 'name s) "pai-parent")) servers))
           (token (plist-get handle :token)))
      (should server)
      (should (member token (alist-get 'args server)))
      (let* ((list (pai-isub-mcp-handle token '(:id 1 :method "tools/list")))
             (tools (plist-get (plist-get list :result) :tools)))
        (should (equal (plist-get (aref tools 0) :name) "reply_to_parent")))
      (let ((res (pai-isub-mcp-handle
                  token '(:id 2 :method "tools/call"
                          :params (:name "reply_to_parent" :arguments (:message "need a decision"))))))
        (should (eq (plist-get (plist-get res :result) :isError) :false)))
      (pai-isub-test--wait 0.05)
      (should (string-match-p "says\\]\nneed a decision" (car pai-isub-test--started)))
      ;; an unknown token is refused; closing forgets the child's token
      (should (plist-get (pai-isub-mcp-handle "nope" '(:id 3 :method "tools/list")) :error))
      (pai-isub-agent-shell-close handle)
      (should-not (gethash token pai-isub-mcp--children)))))

(ert-deftest pai-isub-agent-shell-parent-command ()
  "`/parent TEXT' typed in the shell goes to the parent, not the agent."
  (pai-isub-test--with-shell-child nil
    (with-current-buffer pai-isub-test--shell-buffer
      (should pai-isub-agent-shell-child-mode)
      (let ((input "/parent please check the API") (called nil))
        (cl-letf (((symbol-function 'pai-isub-agent-shell--input-text) (lambda () input))
                  ((symbol-function 'pai-isub-agent-shell--input-bounds) (lambda () nil)))
          (pai-isub-agent-shell--submit-advice (lambda () (setq called t)))
          (should-not called)
          (setq input "just for the agent")
          (pai-isub-agent-shell--submit-advice (lambda () (setq called t)))
          (should called))))
    (should (string-match-p "please check the API" (car pai-isub-test--started)))))

(ert-deftest pai-isub-agent-shell-thought-level-mapping ()
  "pai thinking levels pick the agent's matching or nearest thought level."
  (let ((option '((:options . (((:value . "low") (:name . "Low"))
                               ((:value . "medium") (:name . "Medium"))
                               ((:value . "high") (:name . "High")))))))
    (should (equal (pai-isub-agent-shell--thought-level option 'high) "high"))
    (should (equal (pai-isub-agent-shell--thought-level option 'off) "low"))
    (should (equal (pai-isub-agent-shell--thought-level option 'max) "high"))
    (should (equal (pai-isub-agent-shell--thought-level option 'minimal) "low"))))

(ert-deftest pai-isub-agent-shell-applies-thinking-before-first-prompt ()
  "The role's thinking level is selected before the task is submitted."
  (let (set-to on-success)
    (cl-letf (((symbol-function 'agent-shell--config-option-by-category)
               (lambda (&rest _) '((:current-value . "low")
                                   (:options . (((:value . "low")) ((:value . "high")))))))
              ((symbol-function 'agent-shell--config-option-set-thought-level-id)
               (lambda (&rest args)
                 (setq set-to (plist-get args :thought-level-id)
                       on-success (plist-get args :on-success)))))
      (pai-isub-test--with-shell-child '(:model "faux/faux:high")
        (pai-isub-test--shell-event 'init-finished)
        (should (equal set-to "high"))
        (pai-isub-test--wait 0.05)
        (should-not pai-isub-test--shell-inserted)
        (funcall on-success)
        (pai-isub-test--wait 0.05)
        (should (= 1 (length pai-isub-test--shell-inserted)))))))

(ert-deftest pai-isub-agent-shell-permission-policy ()
  "Own tool approved, tools outside the role's allowlist rejected, others asked."
  (pai-isub-test--with-shell-child nil
    (with-current-buffer pai-isub-test--shell-buffer
      (setq-local agent-shell--state
                  (list (cons :client 'c)
                        (cons :tool-calls
                              (list (cons "t1" (list (cons :permission-actions
                                                           '(((:kind . "allow_once") (:option-id . "yes"))
                                                             ((:kind . "reject_once") (:option-id . "no")))))))))))
    (plist-put handle :tools '("read" "grep"))
    (cl-flet ((ask (kind title)
                (pai-isub-test--shell-event
                 'permission-request
                 `((:request-id . 7) (:tool-call-id . "t1")
                   (:tool-call . ((:kind . ,kind) (:title . ,title))))))
              (last-answer () (prog1 (car pai-isub-test--shell-responses)
                                (setq pai-isub-test--shell-responses nil))))
      (ask "other" "mcp__pai-parent__reply_to_parent")
      (should (equal (last-answer) "yes"))
      (ask "execute" "rm -rf /")
      (should (equal (last-answer) "no"))
      (should (string-match-p "rejected" (plist-get entry :last-output)))
      (ask "search" "grep foo")
      (should-not (last-answer))
      (should (string-match-p "waiting for permission" (plist-get entry :last-output)))
      (plist-put handle :auto-approve t)
      (ask "read" "Read a.el")
      (should (equal (last-answer) "yes")))))

(ert-deftest pai-isub-agent-shell-queued-messages-each-get-answered ()
  "Parent messages queued while the agent is busy are all answered,
and a foreground call gets the answer to its own message."
  (pai-isub-test--with-shell-child nil
    (pai-isub-test--shell-event 'init-finished)
    (pai-isub-test--wait 0.05)
    (pai-isub-test--shell-event 'input-submitted)
    (cl-flet ((finish (text)
                (pai-isub-test--shell-event 'agent-message-chunk `((:text-chunk . ,text)))
                (pai-isub-test--shell-event 'turn-complete '((:stop-reason . "end_turn")))
                (pai-isub-test--wait 0.05)
                (pai-isub-test--shell-event 'input-submitted)))
      ;; task turn running; two more messages, the second one foreground
      (let (fg)
        (pai-isub-say entry "second" nil nil)
        (pai-isub-say entry "third" nil (lambda (r) (setq fg r)))
        (should (= 2 (length (plist-get handle :queue))))
        (finish "ANSWER-1")
        (should (string-match-p "ANSWER-1" (car pai-isub-test--started)))
        (should (plist-get entry :awaiting))
        (should-not fg)
        (finish "ANSWER-2")
        (should (string-match-p "ANSWER-2" (car pai-isub-test--started)))
        (should-not fg)
        (pai-isub-test--shell-event 'agent-message-chunk '((:text-chunk . "ANSWER-3")))
        (pai-isub-test--shell-event 'turn-complete '((:stop-reason . "end_turn")))
        (should (equal (pai-content-text (plist-get fg :content)) "ANSWER-3"))
        (should-not (plist-get entry :awaiting))
        (should (equal (plist-get entry :status) "idle"))
        (should (= 3 (length (pai-isub-test--sent-to-shell))))))))

(defun pai-isub-test--sent-to-shell ()
  "Return the texts submitted to the fake shell, oldest first."
  (reverse pai-isub-test--shell-inserted))

(ert-deftest pai-isub-agent-shell-keeps-typed-input ()
  "A parent message stashes what the user typed and gives it back after the turn."
  (pai-isub-test--with-shell-child nil
    (pai-isub-test--shell-event 'init-finished)
    (pai-isub-test--wait 0.05)
    (pai-isub-test--shell-event 'input-submitted)
    (pai-isub-test--shell-event 'turn-complete '((:stop-reason . "end_turn")))
    (pai-isub-test--wait 0.05)
    (with-current-buffer pai-isub-test--shell-buffer
      (erase-buffer)
      (insert "PROMPT> half typed")
      (let ((start 9))
        (cl-letf (((symbol-function 'pai-isub-agent-shell--input-bounds)
                   (lambda () (and (<= start (point-max)) (cons start (point-max))))))
          (should (pai-isub-agent-shell-pending-input-p handle))
          (pai-isub-say entry "more work" nil nil)
          (pai-isub-test--wait 0.05)
          (should (equal (buffer-string) "PROMPT> "))
          (should (equal (plist-get handle :stash) "half typed"))
          (should (pai-isub-agent-shell-pending-input-p handle))
          (pai-isub-test--shell-event 'input-submitted)
          (pai-isub-test--shell-event 'turn-complete '((:stop-reason . "end_turn")))
          (pai-isub-test--wait 0.05)
          (should (equal (buffer-string) "PROMPT> half typed"))
          (should-not (plist-get handle :stash)))))))

(ert-deftest pai-isub-model-backend-command-is-offered ()
  "`/subagents-model-backend' completes \"list\" and model keys."
  (let ((buf (pai-isub-test--setup)))
    (unwind-protect
        (with-current-buffer buf
          (let ((cmd (gethash "subagents-model-backend" pai--commands)))
            (should cmd)
            (let ((completions (funcall (plist-get cmd :arg-completions) "")))
              (should (member "list" completions))
              (should (member "faux/faux" completions))))
          (should (string-match-p "No per-model backends"
                                  (plist-get (pai-isub-model-backend-command "list" nil)
                                             :message))))
      (pai-isub-test--teardown buf))))

(provide 'pai-interactive-subagents-test)
;;; pai-interactive-subagents-test.el ends here
