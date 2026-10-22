;;; pai-harness2-test.el --- Tests for the Harness² integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Hooks (matching, parsing, decisions, dispatch), the batch runner (argument
;; parsing, the isolated home, real headless rollouts with the faux provider)
;; and the `/harness2' command surface.

;;; Code:

(require 'ert)
(require 'pai)
(require 'pai-harness2)
(require 'pai-harness2-hooks)
(require 'pai-harness2-batch)

(defconst pai-harness2-test--dir
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name))))
  "The extension directory.")

(defmacro pai-harness2-test--with-dir (var &rest body)
  "Bind VAR to a fresh temporary directory around BODY."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "pai-h2-test" t))))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(defun pai-harness2-test--write (file text)
  "Write TEXT to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

;;;; Hooks

(ert-deftest pai-harness2-hooks-matcher ()
  (should (pai-harness2-hooks-matches-p nil "bash"))
  (should (pai-harness2-hooks-matches-p "" "bash"))
  (should (pai-harness2-hooks-matches-p "*" "edit"))
  (should (pai-harness2-hooks-matches-p "Bash" "bash"))
  (should (pai-harness2-hooks-matches-p "Edit|Write" "write"))
  (should (pai-harness2-hooks-matches-p "mcp__.*" "mcp__x"))
  (should-not (pai-harness2-hooks-matches-p "Bash" "bashful"))
  (should-not (pai-harness2-hooks-matches-p "Edit|Write" "read"))
  (should-not (pai-harness2-hooks-matches-p "[" "bash")))

(ert-deftest pai-harness2-hooks-load-parses-and-filters ()
  (pai-harness2-test--with-dir dir
    (should-not (pai-harness2-hooks-load dir))
    (pai-harness2-test--write
     (expand-file-name ".pai/hooks.json" dir)
     "{\"hooks\": {\"PostToolUse\": [{\"matcher\": \"bash\", \"hooks\": [
        {\"type\": \"command\", \"command\": \"echo hi\"},
        {\"type\": \"prompt\", \"command\": \"ignored\"}]}],
       \"Bogus\": [{\"hooks\": [{\"type\": \"command\", \"command\": \"x\"}]}],
       \"Stop\": [{\"hooks\": [{\"type\": \"command\", \"command\": \"true\", \"timeout\": 5}]}]}}")
    (let ((hooks (pai-harness2-hooks-load dir)))
      (should (equal (mapcar #'car hooks) '("PostToolUse" "Stop")))
      (should (equal (pai-harness2-hooks-for hooks "PostToolUse" "bash")
                     '((:command "echo hi" :timeout nil))))
      (should-not (pai-harness2-hooks-for hooks "PostToolUse" "read"))
      (should (equal (pai-harness2-hooks-for hooks "Stop")
                     '((:command "true" :timeout 5)))))
    (pai-harness2-test--write (expand-file-name ".pai/hooks.json" dir) "{not json")
    (should-not (pai-harness2-hooks-load dir))))

(ert-deftest pai-harness2-hooks-decisions ()
  (should (equal (pai-harness2-hooks-decision '(:exit 2 :stdout "" :stderr "nope\n" :json nil))
                 '(:block t :reason "nope")))
  (should (plist-get (pai-harness2-hooks-decision
                      (list :exit 0 :stdout "" :stderr ""
                            :json '((decision . "block") (reason . "why"))))
                     :block))
  (should (equal (plist-get (pai-harness2-hooks-decision
                             (list :exit 0 :stdout "" :stderr ""
                                   :json '((hookSpecificOutput
                                            (permissionDecision . "deny")
                                            (permissionDecisionReason . "denied")))))
                            :reason)
                 "denied"))
  (should (equal (pai-harness2-hooks-decision '(:exit 0 :stdout "extra context\n" :stderr "" :json nil))
                 '(:block nil :context "extra context")))
  (should (equal (pai-harness2-hooks-decision '(:exit 1 :stdout "ignored" :stderr "" :json nil))
                 '(:block nil :context nil))))

(ert-deftest pai-harness2-hooks-dispatch-feeds-json-on-stdin ()
  (pai-harness2-test--with-dir dir
    (let* ((hooks `(("PreToolUse"
                     (:matcher "bash"
                      :hooks ((:command "python3 -c \"import json,sys; d=json.load(sys.stdin); print(json.dumps({'decision':'block','reason':d['hook_event_name']+':'+d['tool_input']['command']}))\""))))))
           (d (pai-harness2-hooks-dispatch hooks "PreToolUse" dir
                                           '((tool_name . "bash")
                                             (tool_input . ((command . "ls"))))
                                           "bash")))
      (should (plist-get d :block))
      (should (equal (plist-get d :reason) "PreToolUse:ls")))))

;;;; Batch runner

(ert-deftest pai-harness2-batch-parse-args ()
  (let ((default-directory "/tmp/"))
    (let ((o (pai-harness2-batch-parse-args
              '("--cwd" "ws" "--prompt" "hi" "--timeout" "5" "--events" "e.jsonl"
                "--disable-ext" "a" "--enable-ext" "b" "--no-isolate"))))
      (should (equal (plist-get o :cwd) "/tmp/ws"))
      (should (equal (plist-get o :events) "/tmp/e.jsonl"))
      (should (equal (plist-get o :timeout) 5))
      (should (equal (plist-get o :disable) '("a")))
      (should (equal (plist-get o :enable) '("b")))
      (should-not (plist-get o :isolate))))
  (should-error (pai-harness2-batch-parse-args '("--prompt" "x")) :type 'user-error)
  (should-error (pai-harness2-batch-parse-args '("--cwd" "x")) :type 'user-error)
  (should-error (pai-harness2-batch-parse-args '("--cwd" "x" "--prompt" "p" "--bogus"))
                :type 'user-error)
  (should-error (pai-harness2-batch-parse-args '("--cwd" "x" "--prompt")) :type 'user-error))

(ert-deftest pai-harness2-batch-isolated-settings ()
  (let* ((src (make-hash-table :test 'equal))
         (exts (make-hash-table :test 'equal)))
    (puthash "pai-subagents" :false exts)
    (puthash "extensions" exts src)
    (puthash "model" "x/y" src)
    (let* ((s (pai-harness2-batch-isolated-settings src '("pai-learn") '("pai-lsp") "a/b"))
           (e (gethash "extensions" s)))
      (should (eq (gethash "pai-memory" e) :false))
      (should (eq (gethash "pai-lsp" e) :false))
      (should (eq (gethash "pai-learn" e) t))       ; --enable-ext wins
      (should (eq (gethash "pai-subagents" e) t))   ; headless subagents on
      (should (equal (gethash "model" s) "a/b"))
      (should (eq (gethash "enabled" (gethash "session" (gethash "memory" s))) :false))
      ;; the user's settings are untouched
      (should (eq (gethash "pai-subagents" exts) :false))
      (should (equal (gethash "model" src) "x/y")))))

(defun pai-harness2-test--run (dir faux &rest args)
  "Run the launcher in DIR with FAUX responses and ARGS; return (EXIT . EVENTS)."
  (let ((faux-file (expand-file-name "faux.eld" dir))
        (events (expand-file-name "events.jsonl" dir)))
    (with-temp-file faux-file (prin1 faux (current-buffer)))
    (let ((exit (apply #'call-process "timeout" nil nil nil "120"
                       (expand-file-name "bin/pai-harness2-run" pai-harness2-test--dir)
                       "--faux" faux-file "--events" events args)))
      (cons exit
            (with-temp-buffer
              (when (file-exists-p events) (insert-file-contents events))
              (mapcar (lambda (l) (json-parse-string l :object-type 'alist))
                      (split-string (buffer-string) "\n" t)))))))

(defun pai-harness2-test--of-type (type events)
  "EVENTS whose type is TYPE."
  (seq-filter (lambda (e) (equal (alist-get 'type e) type)) events))

(ert-deftest pai-harness2-batch-rollout-records-trajectory ()
  "A headless rollout reads AGENTS.md, runs tools and records every step."
  (pai-harness2-test--with-dir dir
    (let ((ws (expand-file-name "ws/" dir)))
      (pai-harness2-test--write (expand-file-name "AGENTS.md" ws) "HARNESS-RULE-42\n")
      (pcase-let ((`(,exit . ,events)
                   (pai-harness2-test--run
                    dir '((:tool-calls ((:id "c1" :name "bash"
                                         :arguments (:command "echo out > answer.txt && cat answer.txt"))))
                          (:text "all done" :stop-reason stop))
                    "--cwd" ws "--prompt" "do it" "--final" (expand-file-name "final.txt" dir))))
        (should (eql exit 0))
        (let ((session (car (pai-harness2-test--of-type "session" events))))
          (should (string-match-p "HARNESS-RULE-42" (alist-get 'system_prompt session))))
        (should (equal (alist-get 'output (car (pai-harness2-test--of-type "tool_result" events)))
                       "out\n"))
        (should (file-exists-p (expand-file-name "answer.txt" ws)))
        (let ((done (car (last events))))
          (should (equal (alist-get 'type done) "done"))
          (should (equal (alist-get 'status done) "success"))
          (should (equal (alist-get 'final done) "all done")))
        (should (equal (with-temp-buffer
                         (insert-file-contents (expand-file-name "final.txt" dir))
                         (buffer-string))
                       "all done"))))))

(ert-deftest pai-harness2-batch-rollout-runs-hooks ()
  "PreToolUse denies, PostToolUse annotates, Stop sends the agent back once."
  (pai-harness2-test--with-dir dir
    (let ((ws (expand-file-name "ws/" dir)))
      (pai-harness2-test--write
       (expand-file-name ".pai/hooks.json" ws)
       "{\"hooks\": {
          \"PreToolUse\": [{\"matcher\": \"Bash\", \"hooks\": [{\"type\": \"command\",
             \"command\": \"grep -q forbidden && echo '{\\\"decision\\\":\\\"block\\\",\\\"reason\\\":\\\"not that\\\"}' || true\"}]}],
          \"PostToolUse\": [{\"matcher\": \"bash\", \"hooks\": [{\"type\": \"command\",
             \"command\": \"echo checked >&2; exit 2\"}]}],
          \"Stop\": [{\"hooks\": [{\"type\": \"command\",
             \"command\": \"test -f done || echo '{\\\"decision\\\":\\\"block\\\",\\\"reason\\\":\\\"make done\\\"}'\"}]}]}}")
      (pcase-let ((`(,exit . ,events)
                   (pai-harness2-test--run
                    dir '((:tool-calls ((:id "c1" :name "bash" :arguments (:command "echo forbidden"))))
                          (:tool-calls ((:id "c2" :name "bash" :arguments (:command "echo fine"))))
                          (:text "finished" :stop-reason stop)
                          (:tool-calls ((:id "c3" :name "bash" :arguments (:command "touch done"))))
                          (:text "really finished" :stop-reason stop))
                    "--cwd" ws "--prompt" "go")))
        (should (eql exit 0))
        (let ((results (pai-harness2-test--of-type "tool_result" events)))
          (should (equal (alist-get 'output (nth 0 results))
                         "Blocked by PreToolUse hook: not that"))
          (should (string-match-p "fine\n\n\\[PostToolUse hook\\] checked"
                                  (alist-get 'output (nth 1 results)))))
        (should (seq-find (lambda (e) (eq (alist-get 'continue e) t))
                          (pai-harness2-test--of-type "hook" events)))
        (should (file-exists-p (expand-file-name "done" ws)))
        (should (equal (alist-get 'final (car (last events))) "really finished"))))))

(ert-deftest pai-harness2-batch-rollout-failure-and-timeout ()
  (pai-harness2-test--with-dir dir
    (let ((ws (expand-file-name "ws/" dir)))
      (make-directory ws t)
      (pcase-let ((`(,exit . ,events)
                   (pai-harness2-test--run dir '((:error "boom")) "--cwd" ws "--prompt" "x")))
        (should (eql exit 1))
        (should (equal (alist-get 'status (car (last events))) "failed")))
      (pcase-let ((`(,exit . ,events)
                   (pai-harness2-test--run
                    dir '((:tool-calls ((:id "s" :name "bash" :arguments (:command "sleep 30")))))
                    "--cwd" ws "--prompt" "x" "--timeout" "2")))
        (should (eql exit 124))
        (should (equal (alist-get 'status (car (last events))) "timeout")))
      (pcase-let ((`(,exit . ,_)
                   (pai-harness2-test--run dir '() "--cwd" (expand-file-name "nope" dir)
                                           "--prompt" "x")))
        (should (eql exit 2))))))

;;;; Command surface

(ert-deftest pai-harness2-command-is-registered-with-completions ()
  (pai-harness2-extension (pai-ext-api-create :id "harness2"))
  (let* ((cmd (pai-command-get "harness2"))
         (complete (plist-get cmd :arg-completions)))
    (should cmd)
    (should (eq (plist-get cmd :handler) #'pai-harness2-command))
    (cl-letf (((symbol-function 'pai-command-arg-words) (lambda () nil)))
      (should (equal (sort (funcall complete "") #'string<)
                     (sort (copy-sequence pai-harness2-subcommands) #'string<)))
      (should (equal (funcall complete "de") '("demo"))))
    (cl-letf (((symbol-function 'pai-command-arg-words) (lambda () '("setup"))))
      (should (equal (funcall complete "") '("jb" "lab" "wb"))))
    (cl-letf (((symbol-function 'pai-command-arg-words) (lambda () '("run" "--bench" "jb"))))
      (should (member "--k" (funcall complete "--")))
      (should (member "parallel" (funcall complete "par"))))
    (cl-letf (((symbol-function 'pai-command-arg-words) (lambda () '("demo"))))
      (should (equal (funcall complete "--i") '("--improver"))))))

(ert-deftest pai-harness2-dispatch-without-install ()
  (pai-harness2-test--with-dir dir
    (let ((pai-harness2-directory (expand-file-name "h2" dir))
          (started nil))
      (cl-letf (((symbol-function 'pai-harness2--start)
                 (lambda (label command &optional _dir) (push (cons label command) started) nil)))
        (should (string-match-p "not installed"
                                (plist-get (pai-harness2-dispatch "run --bench jb") :message)))
        (should (string-match-p "Unknown /harness2 subcommand"
                                (plist-get (pai-harness2-dispatch "frob") :message)))
        (should (string-match-p "Subcommands:" (plist-get (pai-harness2-dispatch "") :message)))
        (should-not started)
        (pai-harness2-dispatch "install")
        (should (equal (caar started) "install"))
        (should (string-match-p "git clone .*harness2" (cdar started)))
        (should (string-match-p "uv pip install .*-e .*python" (cdar started)))))))

(ert-deftest pai-harness2-dispatch-runs-cli-with-environment ()
  (pai-harness2-test--with-dir dir
    (let* ((pai-harness2-directory (expand-file-name "h2" dir))
           (pai-harness2-model "prov/model-a")
           (pai-harness2-improver-model nil)
           (bin (expand-file-name ".venv/bin/harness2-pai" pai-harness2-directory))
           (started nil))
      (pai-harness2-test--write bin "#!/bin/sh\n")
      (set-file-modes bin #o755)
      (cl-letf (((symbol-function 'pai-harness2--start)
                 (lambda (label command &optional _dir) (push (cons label command) started) nil)))
        (pai-harness2-dispatch "run --bench jb --k 1")
        (should (equal (caar started) "run --bench jb --k 1"))
        (should (string-match-p "harness2-pai run --bench jb --k 1\\'" (cdar started))))
      (let ((env (pai-harness2-environment)))
        (should (member "HARNESS2_PAI_MODEL=prov/model-a" env))
        (should (member "HARNESS2_PAI_IMPROVER_MODEL=prov/model-a" env))
        (should (member "HARNESS2_IMPROVER_CHANNEL=pai" env))
        (should (seq-find (lambda (e) (string-prefix-p "PAI_HARNESS2_RUNNER=" e)) env))))))

(provide 'pai-harness2-test)
;;; pai-harness2-test.el ends here
