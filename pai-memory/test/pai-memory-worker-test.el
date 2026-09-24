;;; pai-memory-worker-test.el --- Tests for pai-memory's worker runtime -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)

(defvar pai-memory-worker-test--model nil)

(defun pai-memory-worker-test--model ()
  "Return a faux model that has per-million costs."
  (or pai-memory-worker-test--model
      (progn
        (pai-register-model (pai-make-model :id "faux-priced" :provider "faux" :api 'faux
                                            :cost (list :input 1.0 :output 5.0)))
        (setq pai-memory-worker-test--model (pai-model "faux/faux-priced")))))

(defmacro pai-memory-worker-test--with-owner (buf dir &rest body)
  "Run BODY in a pai-like owner buffer BUF with a session under temp DIR."
  (declare (indent 2))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-mem" t)))
          (pai-directory ,dir)
          (,buf (generate-new-buffer " *memory-owner*")))
     (unwind-protect
         (with-current-buffer ,buf
           (pai-faux-reset)
           (setq default-directory ,dir)
           (insert "history\n\n" pai-prompt-string)
           (setq-local pai--input-marker (copy-marker (point) nil))
           (setq-local pai--model (pai-memory-worker-test--model))
           (setq-local pai--session (pai-session-new ,dir))
           ,@body)
       (with-current-buffer ,buf
         (when (timerp pai-activity--timer) (cancel-timer pai-activity--timer)))
       (kill-buffer ,buf)
       (pai-faux-reset)
       (delete-directory ,dir t))))

(defun pai-memory-worker-test--cost-entries ()
  (pai-memory-cost-entries pai--session))

(defun pai-memory-worker-test--async-tool (name holder)
  "Return a tool NAME that never finishes on its own; HOLDER gets its on-done."
  (list :name name :description "hangs"
        :parameters (pai-object-schema nil nil)
        :execute (lambda (_args _ctx _upd on-done) (setcar holder on-done))))

(ert-deftest pai-memory-worker-completes-and-records ()
  "A finished worker saves its transcript, records its cost and calls back."
  (pai-memory-worker-test--with-owner buf dir
    (pai-faux-push '(:text "All observed." :stop-reason stop))
    (let* ((result nil)
           (entry (pai-memory-worker-launch
                   'observer :system "You observe." :prompt "Transcript here"
                   :detail "8k tokens" :model (pai-memory-worker-test--model)
                   :on-done (lambda (status msgs e) (setq result (list status msgs e))))))
      (should result)
      (should (equal (car result) "completed"))
      (should (eq (nth 2 result) entry))
      (should (equal (plist-get entry :status) "completed"))
      (should (string-prefix-p "obs-" (plist-get entry :id)))
      (should-not (pai-memory-worker-running))
      ;; the worker saw its own system prompt and task, nothing of the owner
      (should (equal (pai-content-text (pai-message-content (car (plist-get pai-faux-last-context :messages))))
                     "You observe."))
      ;; transcript: a hidden session file with system + prompt + answer
      (let ((file (plist-get entry :transcript)))
        (should (file-exists-p file))
        (should (string-match-p "/memory-workers/" file))
        (let ((msgs (pai-session-context-messages (pai-session-load file))))
          (should (= (length msgs) 3))
          (should (pai-system-message-p (car msgs))))
        (should-not (member file (pai-session-list dir))))
      ;; cost entry in the owning session
      (let ((cost (car (pai-memory-worker-test--cost-entries))))
        (should cost)
        (should (equal (plist-get cost :role) "observer"))
        (should (equal (plist-get cost :status) "completed"))
        (should (equal (plist-get cost :model) "faux/faux-priced"))
        ;; faux usage: 10 in / 5 out at $1 / $5 per M
        (should (< (abs (- (plist-get cost :cost) 3.5e-5)) 1e-9))
        (should (string-match-p "memory-workers" (plist-get cost :transcript)))))))

(ert-deftest pai-memory-worker-terminal-tool-ends-run ()
  "Calling a :terminal tool stops the run after that turn."
  (pai-memory-worker-test--with-owner buf dir
    (let* ((recorded nil)
           (tools (list (pai-memory-tool "finish" "Finish." nil nil
                                         (lambda (args) (setq recorded args) "done")
                                         :terminal t))))
      (pai-faux-push '(:tool-calls ((:id "t1" :name "finish" :arguments (:x 1))))
                     '(:text "should never be requested" :stop-reason stop))
      (let ((entry (pai-memory-worker-launch 'consolidator :system "s" :prompt "p"
                                             :tools tools :model (pai-memory-worker-test--model))))
        (should (equal (plist-get entry :status) "completed"))
        (should (equal (plist-get recorded :x) 1))
        ;; the second scripted response was not consumed
        (should (= (length pai-faux-responses) 1))))))

(ert-deftest pai-memory-worker-tools-are-never-deferred ()
  "Worker tools execute on their first call: no deferred-schema reveal turn."
  (pai-memory-worker-test--with-owner buf dir
    (let* ((ran nil)
           ;; an extension-style tool without :deferred would normally be a stub
           (tool (list :name "ext_tool" :description "x" :parameters (pai-object-schema nil nil)
                       :execute (lambda (_a _c _u done) (setq ran t)
                                  (funcall done (pai-tool-ok-result "ok"))))))
      (should (pai-tool-deferred-p tool))
      (pai-faux-push '(:tool-calls ((:id "e" :name "ext_tool" :arguments (:a 1))))
                     '(:text "done" :stop-reason stop))
      (pai-memory-worker-launch 'observer :system "s" :prompt "p" :tools (list tool)
                                :model (pai-memory-worker-test--model))
      (should ran)
      ;; the model was given the full declaration, not the stub
      (let ((decl (seq-find (lambda (d) (equal (plist-get d :name) "ext_tool"))
                            (plist-get pai-faux-last-context :tools))))
        (should decl)
        (should-not (string-match-p "Schema not loaded" (plist-get decl :description)))))))

(ert-deftest pai-memory-worker-max-turns ()
  (pai-memory-worker-test--with-owner buf dir
    (let ((tools (list (pai-memory-tool "note" "Note." nil nil (lambda (_a) "ok")))))
      (pai-faux-push '(:tool-calls ((:id "a" :name "note" :arguments (:k 1))))
                     '(:tool-calls ((:id "b" :name "note" :arguments (:k 2))))
                     '(:tool-calls ((:id "c" :name "note" :arguments (:k 3)))))
      (pai-memory-worker-launch 'observer :system "s" :prompt "p" :tools tools
                                :max-turns 2 :model (pai-memory-worker-test--model))
      (should (= (length pai-faux-responses) 1)))))

(ert-deftest pai-memory-worker-stop ()
  "Stopping a worker completes it once, as stopped, even if the run reports later."
  (pai-memory-worker-test--with-owner buf dir
    (let* ((holder (list nil))
           (calls 0) (status nil)
           (tools (list (pai-memory-worker-test--async-tool "wait" holder))))
      (pai-faux-push '(:tool-calls ((:id "w" :name "wait" :arguments (:a 1)))))
      (let ((entry (pai-memory-worker-launch
                    'observer :system "s" :prompt "p" :tools tools
                    :model (pai-memory-worker-test--model)
                    :on-done (lambda (s _m _e) (cl-incf calls) (setq status s)))))
        (should (car holder))
        (should (equal (pai-memory-worker-running) (list entry)))
        (should (string-match-p "observer" (overlay-get pai-activity--overlay 'before-string)))
        (should (string-match-p "Stopped" (pai-memory--stop "obs")))
        (should (equal status "stopped"))
        (should (equal (plist-get entry :status) "stopped"))
        ;; the strip keeps the stopped worker for a moment, then drops it
        (should (string-match-p "✗ obs-[0-9]+ +observer .* · stopped"
                                (overlay-get pai-activity--overlay 'before-string)))
        (let ((pai-memory-viz-settle-seconds 0))
          (pai-activity-refresh buf)
          (should-not (overlayp pai-activity--overlay)))
        ;; the transcript keeps what happened before the stop
        (should (file-exists-p (plist-get entry :transcript)))
        ;; a late tool result must not complete it again
        (ignore-errors (funcall (car holder) (pai-tool-ok-result "late")))
        (should (= calls 1))
        (should (equal (plist-get (car (pai-memory-worker-test--cost-entries)) :status)
                       "stopped"))))))

(ert-deftest pai-memory-worker-timeout ()
  (pai-memory-worker-test--with-owner buf dir
    (let* ((holder (list nil)) (status nil)
           (tools (list (pai-memory-worker-test--async-tool "wait" holder))))
      (pai-faux-push '(:tool-calls ((:id "w" :name "wait" :arguments (:a 1)))))
      (pai-memory-worker-launch 'observer :system "s" :prompt "p" :tools tools
                                :timeout 0.1 :model (pai-memory-worker-test--model)
                                :on-done (lambda (s _m _e) (setq status s)))
      (let ((deadline (+ (float-time) 3)))
        (while (and (not status) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (equal status "timeout")))))

(ert-deftest pai-memory-worker-uses-role-model ()
  "Each role resolves through its scoped-model role, then :task, then :main."
  (let ((pai-model-roles (copy-sequence pai-model-roles))
        (pai-model-role-fallbacks (copy-alist pai-model-role-fallbacks)))
    (pai-memory-worker-register-model-roles)
    (should (memq :memory-observer pai-model-roles))
    (should (eq (alist-get :memory-promoter pai-model-role-fallbacks) :task))
    (pai-memory-worker-test--model)
    (cl-letf (((symbol-function 'pai-settings-get)
               (lambda (k &optional _d)
                 (when (eq k :scoped-models)
                   (list :main "faux/faux" :memory-observer "faux/faux-priced")))))
      (should (equal (pai-model-key (pai-memory-worker-model 'observer)) "faux/faux-priced"))
      (should (equal (pai-model-key (pai-memory-worker-model 'promoter)) "faux/faux")))))

(ert-deftest pai-memory-worker-configured-model-never-falls-back ()
  "A configured but unavailable model stops the worker instead of using the main model.
Regression: observers configured for a local model whose provider was not
discovered yet silently ran on the (expensive) main model."
  (pai-memory-worker-test--with-owner buf dir
    (let ((pai-model-roles (copy-sequence pai-model-roles))
          (pai-model-role-fallbacks (copy-alist pai-model-role-fallbacks)))
      (pai-memory-worker-register-model-roles)
      (cl-letf (((symbol-function 'pai-settings-get)
                 (lambda (k &optional _d)
                   (when (eq k :scoped-models) '(:memory-observer "local/not-there")))))
        (should-error (pai-memory-worker-model 'observer) :type 'pai-memory-model-unavailable)
        (should-error (pai-memory-worker-launch 'observer :system "s" :prompt "p")
                      :type 'pai-memory-model-unavailable)
        ;; no model call was made, the problem is recorded and shown
        (should-not pai-faux-last-context)
        (should (string-match-p "local/not-there" pai-memory-model-problem))
        (should (string-match-p "⚠ model" (pai-memory-widget-text)))
        (should (string-match-p "UNAVAILABLE"
                                (plist-get (pai-memory-command "" (list :buffer buf)) :message))))
      ;; nothing scoped: the session's model, as before
      (cl-letf (((symbol-function 'pai-settings-get) (lambda (&rest _) nil)))
        (should (eq (pai-memory-worker-model 'observer) pai--model))
        ;; a successful launch clears the problem
        (pai-faux-push '(:text "ok" :stop-reason stop))
        (pai-memory-worker-launch 'observer :system "s" :prompt "p")
        (should-not pai-memory-model-problem)))))

(ert-deftest pai-memory-worker-unknown-role ()
  (should-error (pai-memory-worker-role 'nope)))

;;;; Confined tools

(ert-deftest pai-memory-confined-tools-stay-inside ()
  (let* ((root (file-name-as-directory (make-temp-file "pai-mem-root" t)))
         (outside (make-temp-file "pai-mem-outside" nil ".md" "secret"))
         (tools (pai-memory-confined-tools root '("read" "write" "ls")))
         (tool (lambda (name) (seq-find (lambda (x) (equal (plist-get x :name) name)) tools)))
         (call (lambda (name args)
                 (let (res)
                   (funcall (plist-get (funcall tool name) :execute) args
                            (list :cwd "/") nil (lambda (r) (setq res r)))
                   res))))
    (unwind-protect
        (progn
          ;; relative paths resolve inside root, whatever the caller's cwd
          (should-not (eq t (plist-get (funcall call "write" (list :path "topic.md" :content "hi"))
                                       :is-error)))
          (should (file-exists-p (expand-file-name "topic.md" root)))
          (should (equal (pai-content-text
                          (plist-get (funcall call "read" (list :path "topic.md")) :content))
                         "hi"))
          (should-not (eq t (plist-get (funcall call "write" (list :path "sub/new.md" :content "x"))
                                       :is-error)))
          ;; escapes are refused: .., absolute paths, symlinks
          (should (eq t (plist-get (funcall call "read" (list :path "../x")) :is-error)))
          (should (eq t (plist-get (funcall call "read" (list :path outside)) :is-error)))
          (make-symbolic-link outside (expand-file-name "link.md" root))
          (let ((r (funcall call "read" (list :path "link.md"))))
            (should (eq t (plist-get r :is-error)))
            (should (string-match-p "outside" (pai-content-text (plist-get r :content)))))
          (make-symbolic-link temporary-file-directory (expand-file-name "tmpdir" root))
          (should (eq t (plist-get (funcall call "write" (list :path "tmpdir/evil.md" :content "x"))
                                   :is-error)))
          ;; ls with no path lists root
          (should (string-match-p "topic.md" (pai-content-text
                                              (plist-get (funcall call "ls" nil) :content)))))
      (delete-directory root t)
      (delete-file outside))))

(ert-deftest pai-memory-tool-errors-become-results ()
  (let* ((tool (pai-memory-tool "t" "d" nil nil (lambda (_a) (error "bad input"))))
         (res nil))
    (funcall (plist-get tool :execute) nil nil nil (lambda (r) (setq res r)))
    (should (eq (plist-get res :is-error) t))
    (should (string-match-p "bad input" (pai-content-text (plist-get res :content))))))

;;;; /memory command

(ert-deftest pai-memory-command-status-and-stop ()
  (pai-memory-worker-test--with-owner buf dir
    (pai-faux-push '(:text "ok" :stop-reason stop))
    (pai-memory-worker-launch 'promoter :system "s" :prompt "p"
                              :model (pai-memory-worker-test--model))
    (let ((text (plist-get (pai-memory-command "" (list :buffer buf)) :message)))
      (should (string-match-p "Workers: 0 running" text))
      (should (string-match-p "pro-" text))
      (should (string-match-p "promoter" text))
      (should (string-match-p "Background spend: \\$0.0000 this session (1 run" text)))
    (should (string-match-p "No memory worker was running; memory is stopped"
                            (plist-get (pai-memory-command "stop" (list :buffer buf)) :message)))
    (should (string-match-p "Usage"
                            (plist-get (pai-memory-command "bogus" (list :buffer buf)) :message)))))

(provide 'pai-memory-worker-test)
;;; pai-memory-worker-test.el ends here
