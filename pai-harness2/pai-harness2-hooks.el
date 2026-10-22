;;; pai-harness2-hooks.el --- Claude-Code style lifecycle hooks for headless pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Harness² ("recursive agent harnessing", google-research/harness2) edits
;; eight components of an agent harness.  The eighth, the only one that acts
;; *inside* the tool loop, is a lifecycle-hooks file.  For the pai substrate it
;; is `.pai/hooks.json' in the workspace, in the Claude-Code hook schema that
;; Harness² already validates for Codex:
;;
;;   {"hooks": {"PostToolUse": [{"matcher": "bash",
;;                               "hooks": [{"type": "command",
;;                                          "command": "python3 scripts/check.py"}]}]}}
;;
;; Supported events:
;;
;;   PreToolUse        before a tool runs; a block denies the call, the reason
;;                     becomes the tool's error result.
;;   PostToolUse       after a tool ran; a block appends the reason to the
;;                     tool result the model reads.
;;   SessionStart      once, before the prompt; stdout is added as context.
;;   UserPromptSubmit  before the prompt is sent; stdout is added as context,
;;                     a block refuses the prompt.
;;   Stop              when the agent settles; a block sends the reason back
;;                     as a new user turn (at most `pai-harness2-hooks-max-stop-continuations').
;;
;; Each hook is a shell command receiving the event as JSON on stdin
;; (`hook_event_name', `cwd', `tool_name', `tool_input', `tool_response').
;; It blocks by exiting with status 2 (stderr is the reason) or by printing
;; {"decision": "block", "reason": "..."} (PreToolUse also accepts
;; {"hookSpecificOutput": {"permissionDecision": "deny", ...}}).
;;
;; Hooks run synchronously, so they are only ever installed by the headless
;; batch runner (`pai-harness2-batch'), never in an interactive Emacs: a slow
;; hook must not freeze the UI.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'pai-core)
(require 'pai-ext)

(defconst pai-harness2-hooks-events
  '("PreToolUse" "PostToolUse" "SessionStart" "UserPromptSubmit" "Stop")
  "Hook events the pai substrate dispatches.")

(defvar pai-harness2-hooks-timeout 60
  "Seconds one hook command may run before it is killed.")

(defvar pai-harness2-hooks-max-stop-continuations 3
  "How many times a blocking Stop hook may send the agent back to work.")

(defvar pai-harness2-hooks-log-function nil
  "When non-nil, a function of (EVENT-NAME COMMAND RESULT) called per hook run.
The batch runner uses it to record hook activity in the trajectory.")

;;;; Loading and matching

(defun pai-harness2-hooks-file (cwd)
  "Return the hooks file path for workspace CWD."
  (expand-file-name ".pai/hooks.json" cwd))

(defun pai-harness2-hooks-load (cwd)
  "Return the parsed `hooks' alist of CWD's hooks file, or nil.
The value maps an event name (string) to a list of matcher entries, each a
plist (:matcher STRING :hooks (PLIST...)).  A malformed file yields nil."
  (let ((file (pai-harness2-hooks-file cwd)))
    (when (file-readable-p file)
      (condition-case nil
          (let* ((doc (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'alist :array-type 'list
                                           :null-object nil :false-object nil)))
                 (hooks (alist-get 'hooks doc)))
            (when (consp hooks)
              (delq nil
                    (mapcar
                     (lambda (cell)
                       (let ((event (symbol-name (car cell))))
                         (when (and (member event pai-harness2-hooks-events)
                                    (listp (cdr cell)))
                           (cons event
                                 (delq nil
                                       (mapcar
                                        (lambda (entry)
                                          (when (consp entry)
                                            (list :matcher (alist-get 'matcher entry)
                                                  :hooks
                                                  (delq nil
                                                        (mapcar
                                                         (lambda (h)
                                                           (when (and (consp h)
                                                                      (equal (alist-get 'type h) "command")
                                                                      (stringp (alist-get 'command h))
                                                                      (not (string-blank-p (alist-get 'command h))))
                                                             (list :command (alist-get 'command h)
                                                                   :timeout (alist-get 'timeout h))))
                                                         (alist-get 'hooks entry))))))
                                        (cdr cell)))))))
                     hooks))))
        (error nil)))))

(defun pai-harness2-hooks-matches-p (matcher tool-name)
  "Return non-nil if hook MATCHER selects TOOL-NAME.
An empty, missing or \"*\" matcher selects every tool.  Otherwise MATCHER is
a `|'-separated list of names or regexps, compared case-insensitively against
the whole name, so the Claude-Code names \"Bash\", \"Edit|Write\" also select
pai's `bash', `edit' and `write' tools."
  (or (null matcher)
      (not (stringp matcher))
      (string-blank-p matcher)
      (equal matcher "*")
      (and (stringp tool-name)
           (let ((case-fold-search t))
             (seq-some
              (lambda (alt)
                (let ((alt (string-trim alt)))
                  (or (string-equal-ignore-case alt tool-name)
                      (condition-case nil
                          (string-match-p (concat "\\`\\(?:" alt "\\)\\'") tool-name)
                        (error nil)))))
              (split-string matcher "|" t))))))

(defun pai-harness2-hooks-for (hooks event &optional tool-name)
  "Return the hook command plists in HOOKS for EVENT selecting TOOL-NAME."
  (let ((out '()))
    (dolist (entry (cdr (assoc event hooks)))
      (when (or (not (member event '("PreToolUse" "PostToolUse")))
                (pai-harness2-hooks-matches-p (plist-get entry :matcher) tool-name))
        (setq out (append out (plist-get entry :hooks)))))
    out))

;;;; Running

(defun pai-harness2-hooks--parse-output (stdout)
  "Parse hook STDOUT as a JSON object (alist), or nil when it is not one."
  (let ((s (string-trim (or stdout ""))))
    (when (string-prefix-p "{" s)
      (condition-case nil
          (let ((v (json-parse-string s :object-type 'alist :array-type 'list
                                      :null-object nil :false-object nil)))
            (and (consp v) v))
        (error nil)))))

(defun pai-harness2-hooks-run-command (command input cwd &optional timeout)
  "Run hook COMMAND in CWD with INPUT (a string) on stdin.
CWD may be remote (TRAMP): the hook then runs on that host, next to the
workspace it inspects, exactly like the agent's own bash tool.
Return a plist (:exit N :stdout S :stderr S :json ALIST-OR-NIL)."
  (let* ((default-directory (file-name-as-directory cwd))
         (local (file-local-name (directory-file-name default-directory)))
         (in (make-nearby-temp-file "pai-hook-in"))
         (err (make-nearby-temp-file "pai-hook-err"))
         (process-environment (append (list (concat "CLAUDE_PROJECT_DIR=" local)
                                            (concat "PAI_PROJECT_DIR=" local))
                                      process-environment)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8)) (write-region input nil in nil 'silent))
          (with-temp-buffer
            (let* ((exit (condition-case e
                             (process-file "timeout" in (list t err) nil
                                           (number-to-string (or timeout pai-harness2-hooks-timeout))
                                           "sh" "-c" command)
                           (error (format "%s" (error-message-string e)))))
                   (stdout (buffer-string))
                   (stderr (with-temp-buffer
                             (ignore-errors (insert-file-contents err))
                             (buffer-string))))
              (list :exit (if (integerp exit) exit 1)
                    :stdout stdout
                    :stderr (if (integerp exit) stderr (format "%s%s" stderr exit))
                    :json (pai-harness2-hooks--parse-output stdout)))))
      (ignore-errors (delete-file in))
      (ignore-errors (delete-file err)))))

(defun pai-harness2-hooks-decision (result)
  "Return (:block BOOL :reason STRING :context STRING) for a hook RESULT."
  (let* ((json (plist-get result :json))
         (specific (alist-get 'hookSpecificOutput json))
         (exit (plist-get result :exit))
         (deny (or (equal (alist-get 'decision json) "block")
                   (equal (alist-get 'permissionDecision specific) "deny")
                   (eq (alist-get 'continue json) :false)))
         (reason (or (alist-get 'reason json)
                     (alist-get 'permissionDecisionReason specific)
                     (alist-get 'stopReason json))))
    (cond
     ((eql exit 2)
      (list :block t :reason (string-trim (or (plist-get result :stderr) reason ""))))
     (deny (list :block t :reason (or reason "Blocked by a hook.")))
     (t (list :block nil
              :context (or (alist-get 'additionalContext specific)
                           (and (null json)
                                (eql exit 0)
                                (let ((s (string-trim (plist-get result :stdout))))
                                  (unless (string-empty-p s) s)))))))))

(defun pai-harness2-hooks-dispatch (hooks event cwd payload &optional tool-name)
  "Run HOOKS' commands for EVENT in CWD with PAYLOAD (an alist).
Return a plist (:block BOOL :reason STRING :context STRING) folding all
matching hooks: the first block wins; contexts are concatenated."
  (let ((input (json-serialize
                (append `((hook_event_name . ,event) (cwd . ,cwd)) payload)
                :null-object :null :false-object :false))
        (contexts '()) (decision nil))
    (catch 'done
      (dolist (h (pai-harness2-hooks-for hooks event tool-name))
        (let* ((result (pai-harness2-hooks-run-command
                        (plist-get h :command) input cwd (plist-get h :timeout)))
               (d (pai-harness2-hooks-decision result)))
          (when pai-harness2-hooks-log-function
            (funcall pai-harness2-hooks-log-function event (plist-get h :command) result))
          (when (plist-get d :context) (push (plist-get d :context) contexts))
          (when (plist-get d :block)
            (setq decision d)
            (throw 'done nil)))))
    (list :block (and decision t)
          :reason (plist-get decision :reason)
          :context (and contexts (string-join (nreverse contexts) "\n")))))

;;;; Wiring into a pai instance

(defun pai-harness2-hooks--json-value (value)
  "Return VALUE (pai internal data) as something `json-serialize' accepts."
  (condition-case nil
      (json-parse-string (pai-json-encode value) :object-type 'alist :array-type 'array
                         :null-object :null :false-object :false)
    (error (format "%S" value))))

(defun pai-harness2-hooks--content-text (content)
  "Return the text of tool result CONTENT."
  (cond ((stringp content) content)
        ((listp content) (or (ignore-errors (pai-content-text content)) ""))
        (t "")))

(defun pai-harness2-hooks-install (hooks cwd)
  "Install tool-loop handlers for HOOKS (see `pai-harness2-hooks-load') in CWD.
Must be called in the pai buffer whose instance should run them."
  (when (pai-harness2-hooks-for-any-p hooks '("PreToolUse"))
    (pai-ext-on
     nil 'tool-call
     (lambda (event _ctx)
       (let* ((name (plist-get event :name))
              (id (plist-get (plist-get event :tool-call) :id))
              (d (pai-harness2-hooks-dispatch
                  hooks "PreToolUse" cwd
                  `((tool_name . ,name)
                    (tool_input . ,(pai-harness2-hooks--json-value (plist-get event :input))))
                  name)))
         (when (plist-get d :block)
           (push id pai-harness2-hooks--blocked)
           (list :block t :reason (format "Blocked by PreToolUse hook: %s"
                                          (plist-get d :reason))))))))
  (when (pai-harness2-hooks-for-any-p hooks '("PostToolUse"))
    (pai-ext-on
     nil 'tool-result
     (lambda (event _ctx)
       (let* ((call-id (plist-get event :tool-call-id))
              (blocked (member call-id pai-harness2-hooks--blocked))
              (name (or (plist-get event :tool-name)
                        (pai-harness2-hooks--tool-name-for call-id)))
              (content (plist-get event :content))
              (d (and
                  (not blocked)
                  (pai-harness2-hooks-dispatch
                  hooks "PostToolUse" cwd
                  `((tool_name . ,(or name :null))
                    (tool_input . ,(pai-harness2-hooks--json-value (plist-get event :input)))
                    (tool_response . ,(pai-harness2-hooks--content-text content)))
                  name))))
         (when (or (plist-get d :block) (plist-get d :context))
           (let ((note (if (plist-get d :block) (plist-get d :reason) (plist-get d :context))))
             (list :content (append (if (stringp content) (list (pai-text content)) content)
                                    (list (pai-text (format "\n[PostToolUse hook] %s" note)))))))))))
  hooks)

(defvar-local pai-harness2-hooks--blocked nil
  "Tool-call ids a PreToolUse hook denied (PostToolUse skips them).")

(defvar-local pai-harness2-hooks--tool-names nil
  "Alist of tool-call id -> tool name seen in this instance.")

(defun pai-harness2-hooks--tool-name-for (id)
  "Return the tool name recorded for tool-call ID, or nil."
  (cdr (assoc id pai-harness2-hooks--tool-names)))

(defun pai-harness2-hooks-note-tool-start (id name)
  "Record that tool-call ID runs tool NAME (PostToolUse needs the name)."
  (push (cons id name) pai-harness2-hooks--tool-names))

(defun pai-harness2-hooks-for-any-p (hooks events)
  "Return non-nil if HOOKS has an entry for any of EVENTS."
  (seq-some (lambda (e) (cdr (assoc e hooks))) events))

(provide 'pai-harness2-hooks)
;;; pai-harness2-hooks.el ends here
