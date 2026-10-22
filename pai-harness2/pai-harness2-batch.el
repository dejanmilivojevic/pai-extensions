;;; pai-harness2-batch.el --- Headless pai runs for Harness² rollouts -*- lexical-binding: t; -*-

;;; Commentary:

;; The executor side of the pai substrate for Harness²
;; (google-research/harness2): one non-interactive pai run per invocation,
;; driven exactly like an interactive chat (a real pai buffer, the user's
;; providers, the project's AGENTS.md, skills, subagent roles and extensions)
;; and recorded as a JSONL event stream that the Python `PaiSubstrate'
;; renders into Harness²'s trajectory grammar.
;;
;; A rollout is a child Emacs; `pai-harness2-batch-command' returns its
;; command line (the load path this Emacs loaded pai from), which `/harness2'
;; hands to Harness² as PAI_HARNESS2_EMACS_ARGV:
;;
;;   emacs -Q --batch -L <pai>/lisp ... -l pai-harness2-batch \
;;     -f pai-harness2-batch-main \
;;     --cwd WORKSPACE --prompt-file PROMPT.txt --model anthropic/claude-x \
;;     --events traj.jsonl --timeout 3600
;;
;; The workspace may be remote: with --cwd /docker:USER@CONTAINER:/workspace/
;; pai runs here but its tools act inside the container through TRAMP.
;;
;; Options:
;;   --cwd DIR            workspace (the harness files live at its root)
;;   --prompt TEXT        the task prompt, or
;;   --prompt-file FILE   read it from FILE
;;   --model ID           pai model id (default: the configured model)
;;   --thinking LEVEL     off|minimal|low|medium|high|xhigh
;;   --timeout SECS       wall-clock budget (default 3600); exit 124 when hit
;;   --events FILE        JSONL event stream (default: stdout)
;;   --final FILE         write the final assistant message there
;;   --session-out FILE   copy the pai session file there when done
;;   --pai-home DIR       isolated pai state directory to use (created)
;;   --no-isolate         use the real ~/.pai (sessions and memory included)
;;   --inherit-skills     keep the user's personal skills visible
;;   --enable-ext NAME    force an extension on   (repeatable)
;;   --disable-ext NAME   force an extension off  (repeatable)
;;   --endpoint URL       an OpenAI-compatible endpoint; --model endpoint/ID
;;   --endpoint-key KEY   its API key
;;   --deny-tool NAME     remove a tool from the run (repeatable)
;;   --deny-host HOST     refuse tool calls whose arguments name HOST (repeatable)
;;   --faux FILE          scripted faux provider responses (tests)
;;
;; Isolation (the default): a throw-away pai directory receives copies of the
;; user's settings.json, auth.json and mcp.json, and a link to the extensions
;; directory, so rollouts reach the same providers but never write sessions,
;; history, trust decisions or memory into ~/.pai.  The learning extensions
;; (memory and friends) and UI-only ones are switched off there, because the
;; harness under test is what Harness² edits, not what pai learned elsewhere.
;;
;; Exit status: 0 the agent finished, 1 it failed (provider error, abort),
;; 2 bad invocation, 124 timeout.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'pai)
(require 'tramp)
(require 'tramp-container nil t)
(defvar tramp-histfile-override)
(require 'pai-harness2-hooks)

(defvar pai-faux-responses)

(defconst pai-harness2-batch--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory holding this file.")

(defvar pai-harness2-batch-disabled-extensions
  '("pai-memory" "pai-web" "pai-dashboard" "pai-learn" "pai-browser"
    "xwidget-browser" "pai-interactive-subagents" "pai-harness2")
  "Extensions switched off in an isolated rollout home.
Learning extensions would carry knowledge across Harness² probes (which
must differ only by their harness), and UI-only ones have nothing to drive
in batch.  `pai-interactive-subagents' opens windows; the headless
`pai-subagents' serves the harness's sub_agents/ roles instead.")

(defvar pai-harness2-batch-enabled-extensions '("pai-subagents")
  "Extensions switched on in an isolated rollout home.")

(defvar pai-harness2-batch-interactive-tools '("ask_user_question")
  "Tools removed from a headless run: nobody is there to answer them.")

(defvar pai-harness2-batch--events-file nil
  "File the JSONL events go to, or nil for stdout.")

(defvar pai-harness2-batch--state nil
  "Plist of the running rollout: :done :status :buffer :started.")

;;;; Arguments

(defun pai-harness2-batch-parse-args (args)
  "Parse command-line ARGS (a list of strings) into an options plist.
Signals `user-error' on unknown options or missing values."
  (let ((opts (list :timeout 3600 :isolate t))
        (enable '()) (disable '()) (deny-tools '()) (deny-hosts '()))
    (while args
      (let ((a (pop args)))
        (cl-flet ((val () (or (pop args) (user-error "Option %s needs a value" a))))
          (pcase a
            ("--cwd" (setq opts (plist-put opts :cwd (val))))
            ("--prompt" (setq opts (plist-put opts :prompt (val))))
            ("--prompt-file" (setq opts (plist-put opts :prompt-file (val))))
            ("--model" (setq opts (plist-put opts :model (val))))
            ("--thinking" (setq opts (plist-put opts :thinking (val))))
            ("--timeout" (setq opts (plist-put opts :timeout (string-to-number (val)))))
            ("--events" (setq opts (plist-put opts :events (val))))
            ("--final" (setq opts (plist-put opts :final (val))))
            ("--session-out" (setq opts (plist-put opts :session-out (val))))
            ("--pai-home" (setq opts (plist-put opts :pai-home (val))))
            ("--no-isolate" (setq opts (plist-put opts :isolate nil)))
            ("--inherit-skills" (setq opts (plist-put opts :inherit-skills t)))
            ("--enable-ext" (push (val) enable))
            ("--disable-ext" (push (val) disable))
            ("--faux" (setq opts (plist-put opts :faux (val))))
            ("--endpoint" (setq opts (plist-put opts :endpoint (val))))
            ("--endpoint-key" (setq opts (plist-put opts :endpoint-key (val))))
            ("--deny-tool" (push (val) deny-tools))
            ("--deny-host" (push (val) deny-hosts))
            ("--" nil)
            (_ (user-error "Unknown option %s" a))))))
    (setq opts (plist-put opts :enable (nreverse enable)))
    (setq opts (plist-put opts :disable (nreverse disable)))
    (setq opts (plist-put opts :deny-tools (nreverse deny-tools)))
    (setq opts (plist-put opts :deny-hosts (nreverse deny-hosts)))
    ;; Paths are relative to where the runner was invoked, not the workspace.
    (dolist (k '(:cwd :prompt-file :events :final :session-out :pai-home :faux))
      (when (plist-get opts k)
        (setq opts (plist-put opts k (expand-file-name (plist-get opts k))))))
    (when (and (plist-get opts :endpoint)
               (not (string-prefix-p "endpoint/" (or (plist-get opts :model) ""))))
      (user-error "--endpoint needs --model endpoint/MODEL-ID"))
    (unless (plist-get opts :cwd) (user-error "--cwd is required"))
    (unless (or (plist-get opts :prompt) (plist-get opts :prompt-file))
      (user-error "--prompt or --prompt-file is required"))
    (unless (> (plist-get opts :timeout) 0) (user-error "--timeout must be positive"))
    opts))

(defun pai-harness2-batch--read-file (file)
  "Return FILE's contents as a string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
    (buffer-string)))

;;;; Events

(defun pai-harness2-batch-emit (plist)
  "Write PLIST as one JSON line to the events file (or stdout)."
  (let ((line (concat (pai-json-encode (append plist (list :ts (pai-now-ms)))) "\n")))
    (if pai-harness2-batch--events-file
        (let ((coding-system-for-write 'utf-8))
          (write-region line nil pai-harness2-batch--events-file t 'silent))
      (princ line #'external-debugging-output))))

(defun pai-harness2-batch--clip (s n)
  "Return string S clipped to N characters (with a marker)."
  (let ((s (or s "")))
    (if (> (length s) n)
        (concat (substring s 0 n) (format "…(+%d chars)" (- (length s) n)))
      s)))

(defconst pai-harness2-batch--max-output 200000
  "Characters of one tool output kept in the event stream.")

(defun pai-harness2-batch--blocks (content)
  "Return CONTENT (a string or block list) as JSON-ready block plists."
  (if (stringp content)
      (list (list :type "text" :text content))
    (delq nil
          (mapcar
           (lambda (b)
             (pcase (pai-block-type b)
               ('text (list :type "text" :text (plist-get b :text)))
               ('thinking (let ((th (plist-get b :thinking)))
                            (unless (string-empty-p (or th ""))
                              (list :type "thinking" :text th))))
               ('tool-call (list :type "tool_call" :id (plist-get b :id)
                                 :name (plist-get b :name)
                                 :arguments (or (plist-get b :arguments)
                                                (pai-json-empty-object))))
               ('image (list :type "image" :mime_type (plist-get b :mime-type)))
               (_ nil)))
           content))))

(defun pai-harness2-batch--content-text (content)
  "Return the plain text of CONTENT."
  (cond ((stringp content) content)
        ((null content) "")
        (t (or (ignore-errors (pai-content-text content)) ""))))

(defun pai-harness2-batch--usage (usage)
  "Return USAGE as a JSON-ready plist."
  (when usage
    (list :input (plist-get usage :input)
          :output (plist-get usage :output)
          :cache_read (plist-get usage :cache-read)
          :cache_write (plist-get usage :cache-write)
          :total (plist-get usage :total-tokens))))

(defun pai-harness2-batch--on-message-end (event _ctx)
  "Record the message completed in EVENT."
  (let* ((m (plist-get event :message))
         (role (pai-message-role m)))
    (pcase role
      ('assistant
       (pai-harness2-batch-emit
        (list :type "message" :role "assistant"
              :content (pai-harness2-batch--blocks (pai-message-content m))
              :model (plist-get m :model)
              :stop_reason (plist-get m :stop-reason)
              :error (plist-get m :error-message)
              :usage (pai-harness2-batch--usage (plist-get m :usage)))))
      ('user
       (pai-harness2-batch-emit
        (list :type "message" :role "user"
              :text (pai-harness2-batch--content-text (pai-message-content m))))))))

(defun pai-harness2-batch--on-tool-start (event _ctx)
  "Record the tool call starting in EVENT."
  (when (fboundp 'pai-harness2-hooks-note-tool-start)
    (pai-harness2-hooks-note-tool-start (plist-get event :tool-call-id)
                                        (plist-get event :tool-name)))
  (pai-harness2-batch-emit
   (list :type "tool_start" :id (plist-get event :tool-call-id)
         :name (plist-get event :tool-name)
         :arguments (or (plist-get event :args) (pai-json-empty-object)))))

(defun pai-harness2-batch--on-tool-end (event _ctx)
  "Record the tool result in EVENT."
  (let ((result (plist-get event :result)))
    (pai-harness2-batch-emit
     (list :type "tool_result" :id (plist-get event :tool-call-id)
           :name (plist-get event :tool-name)
           :is_error (if (pai-truthy (plist-get event :is-error)) t :false)
           :output (pai-harness2-batch--clip
                    (pai-harness2-batch--content-text (plist-get result :content))
                    pai-harness2-batch--max-output)))))

(defun pai-harness2-batch--on-agent-end (event _ctx)
  "Record the end of one agent run in EVENT."
  (pai-harness2-batch-emit
   (list :type "agent_end"
         :aborted (if (plist-get event :aborted) t :false)
         :error (let ((e (plist-get event :error))) (and e (format "%s" e)))))
  (when (plist-get event :aborted)
    (setq pai-harness2-batch--state
          (plist-put pai-harness2-batch--state :failed
                     (format "%s" (or (plist-get event :error) "aborted"))))))

(defun pai-harness2-batch--last-assistant (messages)
  "Return the last assistant message in MESSAGES, or nil."
  (seq-find #'pai-assistant-message-p (reverse messages)))

(defun pai-harness2-batch--on-settled (_event _ctx)
  "The agent settled: run Stop hooks, then finish or continue."
  (let ((buf (plist-get pai-harness2-batch--state :buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((reason (pai-harness2-batch--stop-hook-reason)))
          (if reason
              (progn
                (pai-harness2-batch-emit (list :type "hook" :event "Stop" :continue t
                                               :reason reason))
                ;; Outside the agent-end handler that is still unwinding.
                (run-at-time 0 nil (lambda ()
                                     (when (buffer-live-p buf)
                                       (pai-send-message (format "[Stop hook] %s" reason) buf)))))
            (setq pai-harness2-batch--state
                  (plist-put pai-harness2-batch--state :done t))))))))

;;;; Hooks

(defvar pai-harness2-batch--hooks nil
  "Parsed hooks of the running workspace (see `pai-harness2-hooks-load').")

(defvar pai-harness2-batch--stop-continuations 0
  "Times a Stop hook has sent the agent back to work.")

(defun pai-harness2-batch--stop-hook-reason ()
  "Return a blocking Stop hook's reason, or nil to let the run finish."
  (when (and pai-harness2-batch--hooks
             (< pai-harness2-batch--stop-continuations
                pai-harness2-hooks-max-stop-continuations)
             (not (plist-get pai-harness2-batch--state :failed)))
    (let* ((last (pai-harness2-batch--last-assistant pai--context-messages))
           (d (pai-harness2-hooks-dispatch
               pai-harness2-batch--hooks "Stop" default-directory
               `((stop_hook_active . ,(if (> pai-harness2-batch--stop-continuations 0) t :false))
                 (last_assistant_message
                  . ,(pai-harness2-batch--content-text
                      (and last (pai-message-content last))))))))
      (when (plist-get d :block)
        (cl-incf pai-harness2-batch--stop-continuations)
        (or (plist-get d :reason) "A Stop hook asked you to continue.")))))

(defun pai-harness2-batch--log-hook (event command result)
  "Record one hook run (EVENT, COMMAND, RESULT) in the event stream."
  (pai-harness2-batch-emit
   (list :type "hook" :event event :command command
         :exit (plist-get result :exit)
         :stdout (pai-harness2-batch--clip (plist-get result :stdout) 4000)
         :stderr (pai-harness2-batch--clip (plist-get result :stderr) 4000))))

(defun pai-harness2-batch--prompt-with-hooks (prompt cwd)
  "Return PROMPT with SessionStart/UserPromptSubmit hook context, or nil if refused."
  (let ((parts '()) (refused nil))
    (dolist (event '("SessionStart" "UserPromptSubmit"))
      (unless refused
        (let ((d (pai-harness2-hooks-dispatch
                  pai-harness2-batch--hooks event cwd
                  (if (equal event "SessionStart") '((source . "startup"))
                    `((prompt . ,prompt))))))
          (when (plist-get d :context) (push (plist-get d :context) parts))
          (when (and (plist-get d :block) (equal event "UserPromptSubmit"))
            (setq refused (or (plist-get d :reason) "refused"))))))
    (if refused
        (progn (pai-harness2-batch-emit (list :type "hook" :event "UserPromptSubmit"
                                              :refused refused))
               nil)
      (if parts
          (concat prompt "\n\n<hook_context>\n" (string-join (nreverse parts) "\n")
                  "\n</hook_context>")
        prompt))))

;;;; Isolated pai home

(defun pai-harness2-batch--json-file (file)
  "Return FILE parsed as a JSON object (hash table), or an empty one."
  (or (and (file-readable-p file)
           (ignore-errors
             (with-temp-buffer
               (insert-file-contents file)
               (json-parse-buffer :object-type 'hash-table :array-type 'array
                                  :null-object :null :false-object :false))))
      (make-hash-table :test 'equal)))

(defun pai-harness2-batch--endpoint-provider (url model-id)
  "A custom-provider settings entry for the OpenAI-compatible URL serving MODEL-ID."
  (let ((p (make-hash-table :test 'equal))
        (m (make-hash-table :test 'equal)))
    (puthash "id" model-id m)
    (puthash "id" "endpoint" p)
    (puthash "base-url" url p)
    (puthash "api" "openai-completions" p)
    (puthash "models" (vector m) p)
    p))

(defun pai-harness2-batch-isolated-settings (source enable disable &optional model endpoint
                                                   endpoint-model)
  "Return the settings hash for an isolated home, derived from SOURCE.
SOURCE is the user's settings (a hash table, not modified).  Memory is
switched off, extensions in DISABLE off and in ENABLE on.  MODEL, when
given, becomes the default model (setup then resolves it directly).
ENDPOINT, a URL, adds the `endpoint' provider serving ENDPOINT-MODEL
\(endpoint/ID, default MODEL)."
  (let* ((settings (copy-hash-table source))
         (exts (let ((e (gethash "extensions" settings)))
                 (if (hash-table-p e) (copy-hash-table e) (make-hash-table :test 'equal))))
         (off (make-hash-table :test 'equal)))
    (puthash "enabled" :false off)
    (dolist (name (seq-difference (append pai-harness2-batch-disabled-extensions disable) enable))
      (puthash name :false exts))
    (dolist (name (append pai-harness2-batch-enabled-extensions enable))
      (unless (member name disable) (puthash name t exts)))
    (puthash "extensions" exts settings)
    (let ((mem (make-hash-table :test 'equal)))
      (puthash "session" off mem)
      (puthash "long-term" off mem)
      (puthash "enabled" :false mem)
      (puthash "memory" mem settings))
    (puthash "default-project-trust" "always" settings)
    (when model (puthash "model" model settings))
    (when endpoint
      (let ((others (seq-remove (lambda (p) (and (hash-table-p p)
                                                 (equal (gethash "id" p) "endpoint")))
                                (append (gethash "custom-providers" settings) nil))))
        (puthash "custom-providers"
                 (vconcat others
                          (list (pai-harness2-batch--endpoint-provider
                                 endpoint (string-remove-prefix
                                           "endpoint/" (or endpoint-model model "")))))
                 settings)))
    settings))

(defun pai-harness2-batch-make-home (source-dir home enable disable &optional model
                                                endpoint endpoint-key endpoint-model)
  "Populate HOME as an isolated pai directory seeded from SOURCE-DIR.
Copies auth and MCP configuration, links the extensions directory and
writes settings from `pai-harness2-batch-isolated-settings' (with MODEL
and ENDPOINT, whose ENDPOINT-KEY goes into auth.json).  Return HOME."
  (make-directory home t)
  (dolist (f '("auth.json" "mcp.json" "models.json"))
    (let ((src (expand-file-name f source-dir)))
      (when (file-readable-p src)
        (copy-file src (expand-file-name f home) t))))
  (let ((ext (expand-file-name "extensions" source-dir))
        (dst (expand-file-name "extensions" home)))
    (when (and (file-directory-p ext) (not (file-exists-p dst)))
      (make-symbolic-link (file-truename ext) dst)))
  (let ((settings (pai-harness2-batch-isolated-settings
                   (pai-harness2-batch--json-file (expand-file-name "settings.json" source-dir))
                   enable disable model endpoint endpoint-model)))
    (with-temp-file (expand-file-name "settings.json" home)
      (insert (json-serialize settings :null-object :null :false-object :false))))
  (when endpoint
    (let ((auth (pai-harness2-batch--json-file (expand-file-name "auth.json" home)))
          (entry (make-hash-table :test 'equal)))
      (puthash "type" "api_key" entry)
      (puthash "key" (or endpoint-key "none") entry)
      (puthash "endpoint" entry auth)
      (with-temp-file (expand-file-name "auth.json" home)
        (insert (json-serialize auth :null-object :null :false-object :false)))
      (set-file-modes (expand-file-name "auth.json" home) #o600)))
  home)

;;;; The run

(defun pai-harness2-batch--load-faux (file)
  "Install the faux provider with the response specs read from FILE."
  (require 'pai-faux)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (setq pai-faux-responses (read (current-buffer)))))

(defun pai-harness2-batch--remove-tools (names)
  "Unregister the tools in NAMES (and those that would wait for a human)."
  (dolist (name (append pai-harness2-batch-interactive-tools names))
    (when (pai-tool-get name) (pai-unregister-tool name))))

(defun pai-harness2-batch-host-denied (hosts args)
  "Return the first of HOSTS that tool-call ARGS mention, or nil.
A host matches itself and its subdomains, as a whole name in the
arguments' JSON text (so github.com also catches raw.githubusercontent.com
only when listed)."
  (let ((text (downcase (or (ignore-errors (pai-json-encode args)) (format "%S" args)))))
    (seq-find (lambda (host)
                (string-match-p (concat "\\(?:\\`\\|[^a-z0-9-]\\)\\(?:[a-z0-9-]+\\.\\)*"
                                        (regexp-quote (downcase host))
                                        "\\(?:\\'\\|[^a-z0-9.-]\\|\\.[^a-z0-9]\\)")
                                text))
              hosts)))

(defun pai-harness2-batch--install-host-guard (hosts)
  "Refuse every tool call whose arguments name one of HOSTS."
  (when hosts
    (pai-ext-on nil 'tool-call
                (lambda (event _ctx)
                  (when-let ((host (pai-harness2-batch-host-denied hosts (plist-get event :input))))
                    (pai-harness2-batch-emit (list :type "hook" :event "DenyHost" :host host
                                                   :tool (plist-get event :name)))
                    (list :block t
                          :reason (format "Network access to %s is not allowed in this run." host)))))))

(defun pai-harness2-batch--install-handlers ()
  "Subscribe this instance's event handlers for the trajectory."
  (pai-ext-on nil 'message-end #'pai-harness2-batch--on-message-end)
  (pai-ext-on nil 'tool-execution-start #'pai-harness2-batch--on-tool-start)
  (pai-ext-on nil 'tool-execution-end #'pai-harness2-batch--on-tool-end)
  (pai-ext-on nil 'agent-end #'pai-harness2-batch--on-agent-end)
  (pai-ext-on nil 'agent-settled #'pai-harness2-batch--on-settled))

(defun pai-harness2-batch--wait (deadline)
  "Process events until the rollout is done or DEADLINE passes.
Return `done' or `timeout'."
  (catch 'out
    (while t
      (when (plist-get pai-harness2-batch--state :done) (throw 'out 'done))
      (when (> (float-time) deadline) (throw 'out 'timeout))
      (accept-process-output nil 0.1))))

(defun pai-harness2-batch-run (opts)
  "Run one headless rollout described by OPTS.  Return the exit status."
  ;; A remote workspace: TRAMP must never stop to ask anything, nor keep
  ;; shell history in the container (wherever HOME points there).
  (setq tramp-verbose 1 non-essential nil tramp-histfile-override t)
  (let* ((cwd (file-name-as-directory (expand-file-name (plist-get opts :cwd))))
         (source-dir pai-directory)
         (home (and (plist-get opts :isolate)
                    (expand-file-name (or (plist-get opts :pai-home)
                                          (make-temp-file "pai-harness2-home" t)))))
         (prompt (or (plist-get opts :prompt)
                     (pai-harness2-batch--read-file (plist-get opts :prompt-file))))
         (started (float-time))
         (deadline (+ started (plist-get opts :timeout)))
         (status nil) (buffer nil))
    (unless (file-directory-p cwd) (user-error "No such workspace: %s" cwd))
    (setq pai-harness2-batch--events-file
          (and (plist-get opts :events) (expand-file-name (plist-get opts :events))))
    (when pai-harness2-batch--events-file
      (make-directory (file-name-directory pai-harness2-batch--events-file) t)
      (write-region "" nil pai-harness2-batch--events-file nil 'silent))
    (when home
      (pai-harness2-batch-make-home source-dir home (plist-get opts :enable)
                                    (plist-get opts :disable)
                                    (if (plist-get opts :faux) "faux" (plist-get opts :model))
                                    (plist-get opts :endpoint)
                                    (plist-get opts :endpoint-key)
                                    (plist-get opts :model))
      (setq pai-directory home)
      (unless (plist-get opts :inherit-skills)
        (setq pai-skills-shared-dirs nil)))
    (when (plist-get opts :faux)
      (pai-harness2-batch--load-faux (plist-get opts :faux)))
    (setq pai-harness2-batch--state (list :started started)
          pai-harness2-batch--stop-continuations 0
          pai-harness2-batch--hooks (pai-harness2-hooks-load cwd)
          pai-harness2-hooks-log-function #'pai-harness2-batch--log-hook)
    (setq buffer (generate-new-buffer "*pai-harness2*"))
    (setq pai-harness2-batch--state (plist-put pai-harness2-batch--state :buffer buffer))
    (with-current-buffer buffer
      (setq default-directory cwd)
      ;; The workspace is a disposable sandbox Harness² built: trust it
      ;; without asking (and without persisting the decision).
      (cl-letf (((symbol-function 'pai--project-trusted-p) (lambda () t))
                ((symbol-function 'pai-trust-trusted-p) (lambda (&rest _) t)))
        (pai--setup cwd))
      (let* ((id (or (and (plist-get opts :faux) "faux") (plist-get opts :model)))
             (model (and id (or (pai-model id)
                                (progn (pai-models-refresh) (pai-model id))))))
        (when (and id (null model)) (user-error "Unknown pai model: %s" id))
        (when model (setq pai--model model)))
      (unless pai--model (user-error "No pai model configured; pass --model"))
      (when-let ((lvl (plist-get opts :thinking)))
        (setq pai--reasoning (and (not (equal lvl "off")) (intern lvl))))
      (pai-harness2-batch--remove-tools (plist-get opts :deny-tools))
      (pai-harness2-batch--install-handlers)
      (pai-harness2-batch--install-host-guard (plist-get opts :deny-hosts))
      (when pai-harness2-batch--hooks
        (pai-harness2-hooks-install pai-harness2-batch--hooks cwd))
      (pai-harness2-batch-emit
       (list :type "session" :cwd cwd :model (plist-get pai--model :id)
             :provider (plist-get pai--model :provider)
             :thinking (and pai--reasoning (symbol-name pai--reasoning))
             :session_id (and pai--session (pai-session-id pai--session))
             :hooks (mapcar #'car pai-harness2-batch--hooks)
             :system_prompt (pai-harness2-batch--content-text
                             (pai-message-content (car pai--context-messages)))))
      (let ((text (if pai-harness2-batch--hooks
                      (pai-harness2-batch--prompt-with-hooks prompt cwd)
                    prompt)))
        (if (null text)
            (setq status 'refused)
          (pai-send-message text buffer))))
    (unless status
      (setq status (pai-harness2-batch--wait deadline)))
    (when (eq status 'timeout)
      (with-current-buffer buffer (ignore-errors (pai-interrupt))))
    (with-current-buffer buffer
      (let* ((last (pai-harness2-batch--last-assistant pai--context-messages))
             (final (pai-harness2-batch--content-text (and last (pai-message-content last))))
             (failed (or (plist-get pai-harness2-batch--state :failed)
                         (and last (memq (plist-get last :stop-reason) '(error aborted))
                              (or (plist-get last :error-message) "provider error"))))
             (code (pcase status
                     ('timeout 124)
                     ('refused 1)
                     (_ (if failed 1 0)))))
        (when (plist-get opts :final)
          (let ((coding-system-for-write 'utf-8))
            (write-region final nil (expand-file-name (plist-get opts :final)) nil 'silent)))
        (when-let ((out (plist-get opts :session-out))
                   (file (and pai--session (pai-session-file pai--session))))
          (when (file-readable-p file) (copy-file file (expand-file-name out) t)))
        (pai-harness2-batch-emit
         (list :type "done"
               :status (pcase code (0 "success") (124 "timeout") (_ "failed"))
               :exit code
               :error (and (not (eql code 0)) (if (eq status 'timeout) "timeout" failed))
               :final final
               :wall_s (round (- (float-time) started))
               :usage (pai-harness2-batch--usage pai--usage-total)
               :cost_usd pai--cost-total))
        code))))

(defun pai-harness2-batch-run-and-clean (opts)
  "Run OPTS like `pai-harness2-batch-run'; delete a home it created itself."
  (let ((opts (if (and (plist-get opts :isolate) (not (plist-get opts :pai-home)))
                  (plist-put (copy-sequence opts) :pai-home
                             (make-temp-file "pai-harness2-home" t))
                opts))
        (own (and (plist-get opts :isolate) (not (plist-get opts :pai-home)))))
    (unwind-protect (pai-harness2-batch-run opts)
      (when own
        (ignore-errors
          (let ((ext (expand-file-name "extensions" (plist-get opts :pai-home))))
            ;; Never follow the link into the real extensions directory.
            (when (file-symlink-p ext) (delete-file ext)))
          (delete-directory (plist-get opts :pai-home) t))))))

(defun pai-harness2-batch--library-dir (feature)
  "The directory FEATURE is loaded from, or nil."
  (when-let ((lib (locate-library (symbol-name feature))))
    (directory-file-name (file-name-directory lib))))

(defun pai-harness2-batch-command (&optional emacs)
  "The command line (a list) of a child Emacs running one rollout.
EMACS defaults to this Emacs's own executable; the load path is the one
pai, vui, the faux test provider and this extension were loaded from here."
  (append
   (list (or emacs (expand-file-name invocation-name invocation-directory))
         "-Q" "--batch" "--eval" "(setq load-prefer-newer t)")
   (apply #'append
          (mapcar (lambda (dir) (list "-L" dir))
                  (delete-dups
                   (delq nil (list (pai-harness2-batch--library-dir 'pai)
                                   (pai-harness2-batch--library-dir 'vui)
                                   (pai-harness2-batch--library-dir 'pai-faux)
                                   (directory-file-name pai-harness2-batch--dir))))))
   (list "-l" "pai-harness2-batch" "-f" "pai-harness2-batch-main")))

;;;###autoload
(defun pai-harness2-batch-main ()
  "Command-line entry point: run one rollout and exit with its status."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (let ((code
           (condition-case err
               (pai-harness2-batch-run-and-clean (pai-harness2-batch-parse-args args))
             (user-error
              (ignore-errors
                (pai-harness2-batch-emit (list :type "error" :message (error-message-string err))))
              (message "pai-harness2: %s" (error-message-string err))
              2)
             (error
              (ignore-errors
                (pai-harness2-batch-emit (list :type "error" :message (error-message-string err))))
              (message "pai-harness2: %s" (error-message-string err))
              1))))
      (kill-emacs code))))

(provide 'pai-harness2-batch)
;;; pai-harness2-batch.el ends here
