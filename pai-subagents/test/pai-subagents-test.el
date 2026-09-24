;;; pai-subagents-test.el --- Tests for the subagents extension -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-subagents)

(defun pai-subagents-test--setup ()
  "Create a faux-backed pai-like buffer; return it."
  (let ((buf (get-buffer-create (generate-new-buffer-name " *subagents-test*"))))
    (with-current-buffer buf
      (pai-faux-reset)
      ;; minimal instance state: buffer-local registries like a real pai buffer
      (pai-ext-initialize-instance)
      (setq pai--model (or (pai-model "faux/faux")
                           (progn (pai-register-model
                                   (pai-make-model :id "faux" :provider "faux"
                                                   :api 'faux))
                                  (pai-model "faux/faux")))
            pai--active nil pai--context-messages nil pai--trusted t))
    buf))

(defun pai-subagents-test--teardown (buf)
  (when (buffer-live-p buf) (kill-buffer buf))
  (pai-faux-reset))

(defun pai-subagents-test--execute (args buf)
  "Call the subagent tool in BUF with ARGS; return (RESULT DELIVERED)."
  (let (result delivered)
    (with-current-buffer buf
      (funcall (plist-get (pai-tool-get "subagent") :execute)
               args
               (list :cwd default-directory :model pai--model)
               nil
               (lambda (r) (setq result r delivered t))))
    (list result delivered)))

(ert-deftest pai-subagents-resolve-precedence ()
  "Per-run > override > frontmatter > default > parent."
  (pai-subagents-test--setup)
  (unwind-protect
      (progn
        (pai-register-model (pai-make-model :id "m1" :provider "faux"))
        (pai-register-model (pai-make-model :id "m2" :provider "faux"))
        (pai-register-model (pai-make-model :id "m3" :provider "faux"))
        ;; default-model only
        (cl-letf (((symbol-function 'pai-settings-get)
                   (lambda (k &optional _d) (when (eq k :subagents) (list :default-model "faux/m1")))))
          (should (equal (pai-model-key (car (pai-subagents--resolve-model "scout" nil nil)))
                         "faux/m1")))
        ;; override beats default
        (cl-letf (((symbol-function 'pai-settings-get)
                   (lambda (k) (when (eq k :subagents)
                            (list :default-model "faux/m1"
                                  :agent-overrides '(("scout" . (:model "faux/m2" :thinking high))))))))
          (let ((r (pai-subagents--resolve-model "scout" nil nil)))
            (should (equal (pai-model-key (car r)) "faux/m2"))
            (should (eq (cadr r) 'high))))
        ;; per-run beats override, thinking suffix parsed
        (cl-letf (((symbol-function 'pai-settings-get)
                   (lambda (k) (when (eq k :subagents)
                            (list :agent-overrides '(("scout" . (:model "faux/m2"))))))))
          (let ((r (pai-subagents--resolve-model "scout" "faux/m3:low" nil)))
            (should (equal (pai-model-key (car r)) "faux/m3"))
            (should (eq (cadr r) 'low))))
        ;; inherit falls back to parent model
        (cl-letf (((symbol-function 'pai-settings-get) (lambda (_k) nil)))
          (let ((r (pai-subagents--resolve-model "scout" nil (pai-model "faux/m1"))))
            (should (equal (pai-model-key (car r)) "faux/m1")))))
    (remhash "faux/m1" pai--models) (remhash "faux/m2" pai--models)
    (remhash "faux/m3" pai--models)))

(ert-deftest pai-subagents-async-launch-nonblocking ()
  "Async launch returns a receipt immediately; UI never blocks."
  (let ((buf (pai-subagents-test--setup)))
    (unwind-protect
        (cl-letf (((symbol-function 'pai--start-run) (lambda (_notice) t)))
          (pai-faux-push '(:text "child-done" :stop-reason stop))
          (let ((result (nth 0 (pai-subagents-test--execute
                                (list :agent "scout" :task "explore") buf))))
            ;; receipt delivered synchronously, no blocking wait
            (should result)
            (should (string-match-p "Started subagent sub-" (pai-content-text (plist-get result :content)))))
          ;; pump until the child finishes
          (accept-process-output nil 0.2)
          (with-current-buffer buf
            (should (= 1 (length pai-subagents--runs)))
            (should (equal (plist-get (car pai-subagents--runs) :status) "completed"))
            (should (equal (plist-get (car pai-subagents--runs) :output) "child-done"))
            (should (equal (plist-get (car pai-subagents--runs) :model) "faux/faux"))
            (should (eq (plist-get (car pai-subagents--runs) :thinking) 'low)))))
    (pai-subagents-test--teardown buf)))

(ert-deftest pai-subagents-foreground-delivers-result ()
  "Foreground launch keeps the tool pending and delivers the child output."
  (let ((buf (pai-subagents-test--setup)))
    (unwind-protect
        (progn
          (pai-faux-push '(:text "fg-result" :stop-reason stop))
          (let* ((delivered nil) result)
            (with-current-buffer buf
              (funcall (plist-get (pai-tool-get "subagent") :execute)
                       (list :agent "oracle" :task "x" :async :false)
                       (list :cwd default-directory :model pai--model) nil
                       (lambda (r) (setq result r delivered t))))
            (should delivered)
            (should (equal (pai-content-text (plist-get result :content)) "fg-result"))
            ;; oracle's builtin thinking applies
            (with-current-buffer buf
              (should (equal (plist-get (car pai-subagents--runs) :status) "completed")))))
      (pai-subagents-test--teardown buf))))

(ert-deftest pai-subagents-recursion-guard-and-stop ()
  "Children never see the subagent tool; stop terminates a run."
  (let ((buf (pai-subagents-test--setup)))
    (unwind-protect
        (progn
          ;; tool allowlist excludes subagent, includes everything else
          (let ((names (mapcar (lambda (tool) (plist-get tool :name))
                               (pai-subagents--child-tools "worker"))))
            (should-not (member "subagent" names))
            (should (member "bash" names)))
          ;; stop a fake run id -> error result
          (let ((r (nth 0 (pai-subagents-test--execute
                           (list :action "stop" :id "sub-999") buf))))
            (should (eq (plist-get r :is-error) t)))
          ;; launch a run that stays open (stub the agent loop so it never
          ;; completes), then stop it.
          (cl-letf (((symbol-function 'pai-agent-run) (lambda (&rest _) 'fake-run))
                    ((symbol-function 'pai-agent-abort) (lambda (_) nil)))
            (pai-subagents-test--execute (list :agent "scout" :task "t") buf)
            (let ((id (plist-get (car (with-current-buffer buf pai-subagents--runs)) :id)))
              (with-current-buffer buf
                (should (equal (plist-get (car pai-subagents--runs) :status) "running")))
              (let ((r (nth 0 (pai-subagents-test--execute (list :action "stop" :id id) buf))))
                (should (eq (plist-get r :is-error) :false)))
              (with-current-buffer buf
                (should (equal (plist-get (car pai-subagents--runs) :status) "stopped"))))))
      (pai-subagents-test--teardown buf))))

(ert-deftest pai-subagents-role-files-load ()
  "Role files load from a directory and resolve their model."
  (let ((buf (pai-subagents-test--setup))
        (dir (make-temp-file "pai-roles" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "plan.md" dir)
            (insert "---\nname: planner\ndescription: Plans work\nmodel: faux/m2\nthinking: high\ntools: read, grep\n---\nYou are planner. Break the task into steps."))
          (with-current-buffer buf
            (let ((old pai-directory))
              (unwind-protect
                  (progn
                    (setq pai-directory dir)
                    (make-directory (expand-file-name "subagents" dir) t)
                    (copy-file (expand-file-name "plan.md" dir)
                               (expand-file-name "plan.md" (expand-file-name "subagents" dir)) t)
                    (pai-subagents-load-roles)
                    (should (pai-subagents-role "planner"))
                    (should (equal (plist-get (pai-subagents-role "planner") :model) "faux/m2"))
                    (should (eq (plist-get (pai-subagents-role "planner") :thinking) 'high))
                    (should (equal (plist-get (pai-subagents-role "planner") :tools) '("read" "grep")))
                    (should (string-match-p "Break the task" (plist-get (pai-subagents-role "planner") :prompt))))
                (setq pai-directory old)))))
      (delete-directory dir t)
      (pai-subagents-test--setup))))

(ert-deftest pai-subagents-child-context-modes ()
  "fork inherits parent messages; fresh starts clean; children lack subagent tool."
  (let ((buf (pai-subagents-test--setup)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq pai--context-messages
                  (list (pai-system-message "PARENT-CTX") (pai-user-message "hi"))))
          (cl-letf (((symbol-function 'pai--start-run) (lambda (_n) t))
                    ((symbol-function 'pai-build-system-prompt)
                     (lambda (&rest _) "SYS")))
            (pai-faux-push '(:text "fork-ok" :stop-reason stop))
            (pai-subagents-test--execute (list :agent "worker" :task "t" :context "fork") buf)
            ;; first message is the child system prompt
            (should (pai-system-message-p (car (plist-get pai-faux-last-context :messages))))
            ;; fork: user messages inherited after system
            (let ((msgs (plist-get pai-faux-last-context :messages)))
              (should (>= (length msgs) 3))  ; sys + inherited sys+user + task
              (should (seq-find (lambda (m) (and (pai-user-message-p m)
                                                 (equal (pai-message-content m) "hi")))
                                msgs))
              ;; no nested subagent tool
              (should-not (seq-find (lambda (d) (equal (plist-get d :name) "subagent"))
                                    (plist-get pai-faux-last-context :tools))))
            (accept-process-output nil 0.1)
            (pai-faux-reset)
            (pai-faux-push '(:text "fresh-ok" :stop-reason stop))
            (pai-subagents-test--execute (list :agent "scout" :task "t") buf)
            (let ((msgs (plist-get pai-faux-last-context :messages)))
              (should-not (seq-find (lambda (m) (and (pai-user-message-p m)
                                                     (equal (pai-message-content m) "hi")))
                                    msgs)))))
      (pai-subagents-test--teardown buf))))

(ert-deftest pai-subagents-status-and-list-actions ()
  "status and list actions answer without launching anything."
  (let ((buf (pai-subagents-test--setup)))
    (unwind-protect
        (progn
          (let ((r (nth 0 (pai-subagents-test--execute (list :action "status") buf))))
            (should (string-match-p "No subagent runs" (pai-content-text (plist-get r :content)))))
          (let ((r (nth 0 (pai-subagents-test--execute (list :action "list") buf))))
            (should (string-match-p "scout:" (pai-content-text (plist-get r :content))))
            (should (string-match-p "oracle:" (pai-content-text (plist-get r :content))))))
      (pai-subagents-test--teardown buf))))

(ert-deftest pai-subagents-metrics-fold-usage-and-estimate ()
  "Output tokens come from finalized usage plus a live per-turn estimate."
  (let ((e (list :id "sub-1" :role "worker" :status "running"
                 :started (- (float-time) 4.0) :usage-tokens 0 :stream-chars 0)))
    (pai-subagents--observe e '(:type message-update :event (:type text-delta :delta "hello world!!")))
    (pai-subagents--observe e '(:type message-update :event (:type thinking-delta :delta "abcd")))
    (should (= (plist-get e :stream-chars) 17))
    ;; unrelated stream events do not perturb the counters
    (pai-subagents--observe e '(:type tool-execution-start))
    (should (= (plist-get e :stream-chars) 17))
    (pai-subagents--observe e (list :type 'message-end
                                    :message (pai-assistant-message :usage (pai-usage :output 40))))
    ;; finalized turn resets the character estimate and banks real usage
    (should (= (plist-get e :stream-chars) 0))
    (should (= (plist-get e :usage-tokens) 40))
    ;; a fresh in-flight turn adds ~1 token per 4 streamed characters on top
    (pai-subagents--observe e (list :type 'message-update
                                    :event (list :type 'text-delta :delta (make-string 20 ?x))))
    (should (= (pai-subagents--tokens e) 45))
    (should (> (pai-subagents--tps e) 0))))

(ert-deftest pai-subagents-block-lists-only-running ()
  "Running runs render one-per-line above the prompt and clear when none remain."
  (let ((buf (pai-subagents-test--setup)))
    (unwind-protect
        (with-current-buffer buf
          ;; emulate the pai prompt so the overlay has an anchor
          (insert "history\n\n" pai-prompt-string)
          (setq-local pai--input-marker (copy-marker (point) nil))
          (setq pai-subagents--runs
                (list (list :id "sub-2" :role "scout" :status "running"
                            :started (- (float-time) 2.0) :usage-tokens 1200 :stream-chars 0)
                      (list :id "sub-3" :role "worker" :status "completed"
                            :started (- (float-time) 5.0) :ended (float-time)
                            :usage-tokens 800 :stream-chars 0)))
          ;; only running runs are in the block, one per line
          (let ((txt (pai-subagents--block-string (pai-subagents--running))))
            (should (string-match-p "sub-2" txt))
            (should (string-match-p "scout" txt))
            (should-not (string-match-p "sub-3" txt))
            (should (= 1 (cl-count ?\n txt))))
          (should (pai-subagents--display buf))
          (let ((ov pai-subagents--overlay))
            (should (overlayp ov))
            ;; anchored immediately before the prompt string
            (should (= (overlay-start ov)
                       (- (marker-position pai--input-marker) (length pai-prompt-string))))
            (should (string-match-p "sub-2" (overlay-get ov 'before-string))))
          ;; once no run is active the overlay is removed entirely
          (dolist (e pai-subagents--runs) (plist-put e :status "completed"))
          (should-not (pai-subagents--display buf))
          (should-not (overlayp pai-subagents--overlay)))
      (pai-subagents-test--teardown buf))))

(ert-deftest pai-subagents-busy-parent-gets-a-user-message ()
  "A background result for a busy parent is queued as a user message, not a string."
  (let ((buf (pai-subagents-test--setup)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local pai--active t)
          (setq-local pai--steering-queue nil)
          (let ((entry (list :id "sub-9" :role "scout" :status "running" :parent buf
                             :started (float-time) :usage-tokens 0 :stream-chars 0)))
            (pai-subagents--finish entry (list :text "found it" :is-error :false)))
          (should (= (length pai--steering-queue) 1))
          (let ((m (car pai--steering-queue)))
            (should (pai-user-message-p m))
            (should (string-match-p "found it" (pai-content-text (pai-message-content m))))))
      (pai-subagents-test--teardown buf))))

(ert-deftest pai-subagents-registers-settings-section ()
  "Loading the extension registers a Subagents section in the settings screen."
  (require 'seq)
  (require 'pai-settings-ui)
  (let ((sec (seq-find (lambda (s) (eq (pai-settings-ui-section-id s) 'subagents))
                       pai-settings-ui--sections)))
    (should sec)
    (should (equal "Subagents" (pai-settings-ui-section-label sec)))
    (let ((keys (mapcar #'pai-settings-ui-item-key
                        (pai-settings-ui-subsection-items
                         (car (pai-settings-ui-section-subsections sec))))))
      (should (memq :subagents-default-model keys))
      (should (memq :subagents-default-thinking keys)))))

(ert-deftest pai-subagents-settings-item-roundtrips ()
  "The default-thinking settings item persists through the :subagents config."
  (require 'seq)
  (require 'pai-settings-ui)
  (let* ((dir (file-name-as-directory (make-temp-file "pai-sa-set" t)))
         (pai-directory (expand-file-name "state" dir))
         (pai-settings--global nil) (pai-settings--project nil)
         (pai-settings--project-dir nil))
    (unwind-protect
        (progn
          (pai-settings-load nil)
          (let* ((sec (seq-find (lambda (s) (eq (pai-settings-ui-section-id s) 'subagents))
                                pai-settings-ui--sections))
                 (sub (car (pai-settings-ui-section-subsections sec)))
                 (item (seq-find (lambda (i)
                                   (eq (pai-settings-ui-item-key i) :subagents-default-thinking))
                                 (pai-settings-ui-subsection-items sub))))
            (funcall (pai-settings-ui-item-set item) "high")
            (should (equal "high" (funcall (pai-settings-ui-item-get item))))
            (should (eq (plist-get (pai-settings-get :subagents) :default-thinking) 'high))))
      (delete-directory dir t))))

(ert-deftest pai-subagents-settings-roles-dynamic-and-override ()
  "The Roles subsection is dynamic, produces one row per role, and per-role
model and thinking overrides set through the settings helpers persist."
  (require 'seq)
  (require 'pai-settings-ui)
  (let* ((sec (seq-find (lambda (s) (eq (pai-settings-ui-section-id s) 'subagents))
                        pai-settings-ui--sections))
         (roles-sub (seq-find (lambda (s) (eq (pai-settings-ui-subsection-id s) 'roles))
                              (pai-settings-ui-section-subsections sec))))
    (should (functionp (pai-settings-ui-subsection-items-fn roles-sub))))
  (let* ((dir (file-name-as-directory (make-temp-file "pai-role" t)))
         (pai-directory (expand-file-name "state" dir))
         (pai-settings--global nil) (pai-settings--project nil)
         (pai-settings--project-dir nil))
    (unwind-protect
        (progn
          (pai-settings-load nil)
          (let ((items (pai-subagents--settings-role-items)))
            ;; builtin roles appear as generated custom rows (one per role)
            (should (seq-find (lambda (p) (eq (plist-get p :key) :role-worker)) items))
            (should (seq-find (lambda (p) (eq (plist-get p :key) :role-scout)) items))
            (should (cl-every (lambda (p) (eq (plist-get p :type) 'custom)) items)))
          ;; per-role model override round-trips
          (pai-subagents--set-override-model "worker" "openai/gpt-4o")
          (should (equal "openai/gpt-4o" (pai-subagents--role-model-display "worker")))
          (should (equal "openai/gpt-4o"
                         (plist-get (cdr (assoc "worker"
                                                (plist-get (pai-settings-get :subagents)
                                                           :agent-overrides)))
                                    :model)))
          ;; per-role thinking override round-trips, and "inherit" clears it
          (pai-subagents--set-override-thinking "worker" "high")
          (should (equal "high" (pai-subagents--role-thinking-display "worker")))
          (should (eq 'high
                      (plist-get (cdr (assoc "worker"
                                             (plist-get (pai-settings-get :subagents)
                                                        :agent-overrides)))
                                 :thinking)))
          (pai-subagents--set-override-thinking "worker" "inherit")
          (should (null (plist-get (cdr (assoc "worker"
                                               (plist-get (pai-settings-get :subagents)
                                                          :agent-overrides)))
                                   :thinking))))
      (delete-directory dir t))))

(ert-deftest pai-subagents-role-crud ()
  "Roles can be disabled/re-enabled, created via file + reload, and deleted."
  (require 'seq)
  (let* ((dir (file-name-as-directory (make-temp-file "pai-role-crud" t)))
         (pai-directory (expand-file-name "state" dir))
         (pai-settings--global nil) (pai-settings--project nil)
         (pai-settings--project-dir nil)
         (pai-subagents--roles nil)
         (default-directory dir))
    (unwind-protect
        (progn
          (pai-settings-load nil)
          ;; disable a builtin -> filtered out; re-enable -> back
          (should (assoc "oracle" (pai-subagents-roles)))
          (pai-subagents--set-role-disabled "oracle" t)
          (should-not (assoc "oracle" (pai-subagents-roles)))
          (pai-subagents--set-role-disabled "oracle" nil)
          (should (assoc "oracle" (pai-subagents-roles)))
          ;; create a user role file, reload -> appears
          (with-temp-file (expand-file-name "myrole.md" (pai-subagents--roles-dir))
            (insert "---\nname: myrole\ndescription: d\nthinking: low\n---\n\nbody\n"))
          (pai-subagents-reload-roles)
          (should (assoc "myrole" (pai-subagents-roles)))
          ;; delete the user role -> gone and file removed
          (pai-subagents--delete-role "myrole")
          (should-not (assoc "myrole" (pai-subagents-roles)))
          (should-not (file-exists-p (expand-file-name "myrole.md"
                                                       (pai-subagents--roles-dir))))
          ;; delete a builtin -> disabled and hidden
          (pai-subagents--delete-role "scout")
          (should-not (assoc "scout" (pai-subagents-roles)))
          (should (member "scout" (pai-subagents--disabled-roles)))
          ;; editing recreates an override file and re-enables the role
          (cl-letf (((symbol-function 'find-file) #'ignore))
            (pai-subagents--edit-role "scout"))
          (should-not (member "scout" (pai-subagents--disabled-roles)))
          (should (file-exists-p (expand-file-name "scout.md"
                                                   (pai-subagents--roles-dir)))))
      (delete-directory dir t))))

(provide 'pai-subagents-test)
;;; pai-subagents-test.el ends here
