;;; pai-subagents.el --- Async subagent delegation for pai -*- lexical-binding: t; -*-

;; Port of https://github.com/nicobailon/pi-subagents (MIT) to the pai
;; extension API.  The parent session delegates focused tasks to child agent
;; runs.  Children never block the UI: async launches return a receipt
;; immediately and completion is delivered as a follow-up parent turn;
;; foreground launches (async: false) keep the tool call pending while the
;; child streams, driven entirely by async callbacks.

;;; Commentary:

;; Roles: builtins (scout, researcher, evidence-auditor, worker, reviewer,
;; oracle, delegate) plus user roles from ~/.pai/subagents/*.md and, in
;; trusted projects, <project>/.pai/subagents/*.md.  A role file is markdown
;; with frontmatter (name, description, model, thinking, tools, context) and
;; a body that is the role's system prompt.
;;
;; Which model fills which role, strongest first:
;;   1. per-run tool arg: model "provider/id[:thinking]"
;;   2. settings :subagents (:agent-overrides (("worker" . (:model M :thinking L))))
;;   3. role frontmatter :model
;;   4. settings :subagents (:default-model M)
;;   5. the parent session model ("inherit")
;; Easiest setup: /subagents-model ROLE  -> pick role, pick model.  Or
;; /subagents-models to see the live mapping.  /subagents-reload re-reads
;; role files; /subagents lists runs (arg: id prefix to stop).
;;
;; The `subagent' tool: {agent, task, model?, async?, context?, action?,
;; id?, max-lines?, timeout?}.  Actions: launch (default), status, stop,
;; list.  Children never get the subagent tool (recursion guard).

;;; Code:

(require 'pai)
(require 'pai-ext)
(require 'pai-agent)
(require 'pai-models)
(require 'pai-settings)
(require 'pai-skills)

(declare-function pai-settings-ui-register-section "pai-settings-ui")
(declare-function pai-settings-ui-register-subsection "pai-settings-ui")
(declare-function pai-settings-ui-register-item "pai-settings-ui")
(declare-function pai-settings-ui-register-dynamic-items "pai-settings-ui")
(declare-function pai--menu-buffer "pai-ui")

(defconst pai-subagents--builtins
  '(("scout"
     (:description "Fast local codebase recon: relevant files, entry points, data flow, risks."
      :thinking low :context fresh
      :prompt "You are scout, a fast codebase recon agent. Map the relevant files, entry points, and data flow for the task. Read-only: report findings with file:line references and name risks. Never edit files."))
    ("researcher"
     (:description "Research with sources; concise research brief."
      :thinking low :context fresh
      :prompt "You are researcher. Answer the research question with a concise brief; cite sources for every factual claim and separate facts from inference. Never edit files."))
    ("evidence-auditor"
     (:description "Independently checks whether claims are supported by sources."
      :thinking medium :context fresh
      :prompt "You are evidence-auditor. For each claim given, independently check whether the cited sources support it. Report supported / unsupported / contradicted with evidence. Never edit files."))
    ("worker"
     (:description "Implementation work: edits files, validates, escalates decisions."
      :thinking medium :context fork
      :prompt "You are worker, an implementation agent. Make minimal precise edits for the task, then validate (compile/tests). Do not expand scope; instead of guessing on unapproved decisions, report the decision you need."))
    ("reviewer"
     (:description "Code review and small fixes: correctness, tests, edge cases, simplicity."
      :thinking high :context fork
      :prompt "You are reviewer. Review the change or code under the task: correctness, missing tests, edge cases, needless complexity. Findings first, ordered by severity, each with file:line and a concrete fix. Apply only small obvious fixes; report the rest."))
    ("oracle"
     (:description "A second opinion: challenges assumptions without editing."
      :thinking high :context fork
      :prompt "You are oracle, a senior reviewer giving a second opinion. Challenge assumptions, name risks and what is missing, and give a clear recommendation. You never edit files."))
    ("delegate"
     (:description "Lightweight general delegate close to the parent session."
      :thinking medium :context fork
      :prompt "You are delegate, a focused general-purpose assistant. Complete the delegated task with the parent's conventions; report concisely what you did and any follow-ups.")))
  "Builtin role definitions: NAME -> plist.")

(defconst pai-subagents--thinking-levels
  '(off minimal low medium high xhigh max)
  "Supported thinking levels, least to most.")

(defvar-local pai-subagents--roles nil
  "User-defined roles loaded from disk for this instance: NAME -> plist.")

(defvar-local pai-subagents--runs nil
  "Child runs for this instance, newest first.
Each entry: (:id :role :task :model :thinking :status :output :run
:parent :on-done :foreground :started :ended :usage-tokens :stream-chars
:max-lines :timeout-timer).")

(defvar-local pai-subagents--ui-timer nil
  "Repeating timer refreshing the running-subagents display.")

(defvar-local pai-subagents--overlay nil
  "Overlay rendering running subagents on their own lines above the prompt.")

(defvar pai-subagents--counter 0
  "Monotonic child run id counter (process-global, ids stay unique).")

;;;; Roles

(defun pai-subagents--disabled-roles ()
  "Return the list of role names disabled via settings."
  (plist-get (pai-subagents--config) :disabled-roles))

(defun pai-subagents-roles ()
  "Return the role alist for this instance (user roles first).
Both user and builtin entries are (NAME . PLIST).  Roles listed in the
settings `:disabled-roles' are omitted."
  (let ((disabled (pai-subagents--disabled-roles)))
    (cl-remove-if
     (lambda (r) (member (car r) disabled))
     (append pai-subagents--roles
             (cl-loop for (name . rest) in pai-subagents--builtins
                      collect (cons name (car rest)))))))
(defun pai-subagents-role (name)
  "Return the role plist for NAME, or nil."
  (cdr (assoc name (pai-subagents-roles))))

(defun pai-subagents-role-names ()
  "Return sorted role names."
  (sort (mapcar #'car (pai-subagents-roles)) #'string-lessp))

(defun pai-subagents--parse-role-file (file)
  "Parse role FILE (markdown + frontmatter); return (NAME . PLIST) or nil."
  (condition-case err
      (let* ((text (with-temp-buffer (insert-file-contents file) (buffer-string)))
             (parsed (pai-skills--parse-frontmatter text))
             (meta (car parsed))
             (body (string-trim (cdr parsed)))
             (name (cdr (assoc "name" meta))))
        (when (and name (not (string-empty-p name))
                   body (not (string-empty-p body)))
          (cons name
                (append
                 (list :description (or (cdr (assoc "description" meta)) "")
                       :prompt body)
                 (when (cdr (assoc "model" meta))
                   (list :model (cdr (assoc "model" meta))))
                 (when (cdr (assoc "thinking" meta))
                   (list :thinking (intern (cdr (assoc "thinking" meta)))))
                 (when (cdr (assoc "tools" meta))
                   (list :tools (split-string (cdr (assoc "tools" meta)) "[, ]+" t)))
                 (when (cdr (assoc "context" meta))
                   (list :context (intern (cdr (assoc "context" meta)))))))))
    (error (message "pai-subagents: bad role file %s: %s" file
                    (error-message-string err))
           nil)))

(defun pai-subagents-load-roles (&optional project-dir)
  "Load user roles into this instance from the home dir, then PROJECT-DIR.
Project roles override home roles with the same name."
  (let ((roles pai-subagents--roles)
        (dirs (list (expand-file-name "subagents" pai-directory))))
    (when project-dir
      (push (expand-file-name ".pai/subagents" project-dir) dirs))
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.md\\'"))
          (let ((role (pai-subagents--parse-role-file file)))
            (when role
              (setq roles (cons role (cl-remove (car role) roles
                                                :key #'car :test #'equal))))))))
    (setq pai-subagents--roles roles)
    (length roles)))

;;;; Model resolution

(defun pai-subagents--parse-model-spec (spec)
  "Split \"provider/id[:thinking]\" SPEC into (MODEL-KEY THINKING-SYMBOL)."
  (save-match-data
    (if (string-match
         "\\`\\(.+?\\):\\(off\\|minimal\\|low\\|medium\\|high\\|xhigh\\|max\\)\\'" spec)
        (list (match-string 1 spec) (intern (match-string 2 spec)))
      (list spec nil))))

(defun pai-subagents--config ()
  "Return this instance's :subagents settings plist."
  (or (pai-settings-get :subagents) '()))

(defun pai-subagents--config-set (key value)
  "Set KEY to VALUE in the :subagents settings plist and persist (project)."
  (pai-settings-set :subagents
                    (plist-put (copy-sequence (pai-subagents--config)) key value)
                    'project))

(defun pai-subagents--override-for (role)
  "Return the settings override plist for ROLE, or nil."
  (cdr (assoc role (plist-get (pai-subagents--config) :agent-overrides))))

(defun pai-subagents--resolve-model (role &optional per-run parent-model)
  "Resolve the model plist and thinking for ROLE.
PER-RUN wins, then the settings role override, then role frontmatter,
then settings :default-model, then PARENT-MODEL.  Return
\(MODEL-PLIST THINKING) or nil when nothing resolves."
  (let* ((override (pai-subagents--override-for role))
         (role-def (pai-subagents-role role))
         (spec (or per-run
                   (plist-get override :model)
                   (plist-get role-def :model)
                   (plist-get (pai-subagents--config) :default-model)
                   (and parent-model "inherit")))
         (parsed (and spec (pai-subagents--parse-model-spec spec)))
         (key (car parsed))
         (thinking (or (cadr parsed)
                       (plist-get override :thinking)
                       (plist-get role-def :thinking)
                       (plist-get (pai-subagents--config) :default-thinking)))
         (model (pcase key
                  ((or "inherit" `nil "") parent-model)
                  (_ (or (pai-model key)
                         (and parent-model
                              (equal key (pai-model-key parent-model))
                              parent-model))))))
    (when (or (memq thinking pai-subagents--thinking-levels) (null thinking))
      (and model (list model thinking)))))

(defun pai-subagents--child-tools (role)
  "Return the tool plists a child of ROLE may use.
Everything except `subagent' (recursion guard), restricted to the role's
or its override's :tools allowlist when one is set."
  (let* ((allow (or (plist-get (pai-subagents--override-for role) :tools)
                    (plist-get (pai-subagents-role role) :tools))))
    (seq-filter (lambda (tool)
                  (and (not (equal (plist-get tool :name) "subagent"))
                       (or (null allow)
                           (member (plist-get tool :name) allow))))
                (pai-tools-all))))

;;;; Live metrics and above-prompt display

(defvar pai--input-marker)
(defvar pai-prompt-string)

(defface pai-subagents-status-face '((t :inherit font-lock-comment-face))
  "Face for the running-subagents lines shown above the prompt."
  :group 'pai-mcp)

(defun pai-subagents--fmt-count (n)
  "Format token count N compactly (e.g. 1.2k, 45k, 1.2M)."
  (let ((n (max 0 (round n))))
    (cond ((< n 1000) (number-to-string n))
          ((< n 100000) (format "%.1fk" (/ n 1000.0)))
          ((< n 1000000) (format "%dk" (round (/ n 1000.0))))
          (t (format "%.1fM" (/ n 1000000.0))))))

(defun pai-subagents--fmt-duration (seconds)
  "Format elapsed SECONDS compactly (e.g. 12s, 3m04s)."
  (let ((s (max 0 (round seconds))))
    (if (< s 60) (format "%ds" s)
      (format "%dm%02ds" (/ s 60) (% s 60)))))

(defun pai-subagents--elapsed (entry)
  "Return ENTRY's wall-clock runtime in seconds (frozen once :ended)."
  (max 0.001 (- (or (plist-get entry :ended) (float-time))
                (or (plist-get entry :started) (float-time)))))

(defun pai-subagents--tokens (entry)
  "Return ENTRY's best-known output token count.
Finalized turns use real usage; the in-flight turn is estimated from
streamed characters (~4 chars/token) so the display stays live."
  (+ (or (plist-get entry :usage-tokens) 0)
     (max 0 (round (/ (or (plist-get entry :stream-chars) 0) 4.0)))))

(defun pai-subagents--tps (entry)
  "Return ENTRY's output tokens-per-second over its runtime."
  (round (/ (pai-subagents--tokens entry) (pai-subagents--elapsed entry))))

(defun pai-subagents--observe (entry event)
  "Fold child EVENT into ENTRY's token metrics.  Mutates ENTRY in place."
  (pcase (plist-get event :type)
    ('message-update
     (let ((se (plist-get event :event)))
       (when (memq (plist-get se :type) '(text-delta thinking-delta))
         (plist-put entry :stream-chars
                    (+ (or (plist-get entry :stream-chars) 0)
                       (length (or (plist-get se :delta) "")))))))
    ('message-end
     (let ((m (plist-get event :message)))
       (when (pai-assistant-message-p m)
         (plist-put entry :usage-tokens
                    (+ (or (plist-get entry :usage-tokens) 0)
                       (or (plist-get (plist-get m :usage) :output) 0)))
         (plist-put entry :stream-chars 0))))))

(defun pai-subagents--running ()
  "Return running run entries for this instance, oldest first."
  (reverse (seq-filter (lambda (e) (equal (plist-get e :status) "running"))
                       pai-subagents--runs)))

(defun pai-subagents--run-line (entry)
  "Return a single status line for run ENTRY."
  (format "⛭ %-6s %-9s %6st · %4d tok/s · %s"
          (plist-get entry :id) (plist-get entry :role)
          (pai-subagents--fmt-count (pai-subagents--tokens entry))
          (pai-subagents--tps entry)
          (pai-subagents--fmt-duration (pai-subagents--elapsed entry))))

(defun pai-subagents--block-string (running)
  "Return the multi-line overlay text for RUNNING runs (one per line)."
  (propertize
   (concat (mapconcat #'pai-subagents--run-line running "\n") "\n")
   'face 'pai-subagents-status-face))

(defun pai-subagents--remove-overlay ()
  "Delete the above-prompt subagents overlay in the current buffer, if any."
  (when (overlayp pai-subagents--overlay)
    (delete-overlay pai-subagents--overlay))
  (setq pai-subagents--overlay nil))

(defun pai-subagents--prompt-start ()
  "Return the buffer position where the prompt string begins, or nil."
  (when (and (boundp 'pai--input-marker) (markerp pai--input-marker)
             (marker-buffer pai--input-marker))
    (max (point-min)
         (- (marker-position pai--input-marker)
            (length (if (boundp 'pai-prompt-string) pai-prompt-string ""))))))

(defun pai-subagents--render-overlay ()
  "Show running runs on their own lines just above the prompt.
With no running runs, or in a buffer without a prompt, remove the overlay."
  (let ((running (pai-subagents--running))
        (pos (pai-subagents--prompt-start)))
    (if (and running pos)
        (progn
          (unless (overlayp pai-subagents--overlay)
            (setq pai-subagents--overlay (make-overlay pos pos nil t nil)))
          (move-overlay pai-subagents--overlay pos pos)
          (overlay-put pai-subagents--overlay 'before-string
                       (pai-subagents--block-string running)))
      (pai-subagents--remove-overlay))))

(defun pai-subagents--display (buffer)
  "Refresh the above-prompt subagents block in BUFFER; return non-nil if any run.
Runs while active regardless of BUFFER's major mode; a killed BUFFER is a no-op."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (pai-subagents--render-overlay)
         (and (pai-subagents--running) t))))

(defun pai-subagents--ensure-timer (buffer)
  "Start the header refresh timer for BUFFER unless one is already live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (timerp pai-subagents--ui-timer)
        (let (tmr)
          (setq tmr
                (run-at-time
                 0.4 0.4
                 (lambda ()
                   ;; Stop once the buffer dies or no run remains active.
                   (unless (pai-subagents--display buffer)
                     (cancel-timer tmr)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (setq pai-subagents--ui-timer nil)))))))
          (setq pai-subagents--ui-timer tmr))))))

(defun pai-subagents--ui-refresh (buffer)
  "Update BUFFER's header now and ensure the refresh timer runs while active."
  (when (pai-subagents--display buffer)
    (pai-subagents--ensure-timer buffer)))

;;;; Run tracking

(defun pai-subagents--record (entry)
  "Upsert run ENTRY into this instance's run list by :id."
  (setq pai-subagents--runs
        (cons entry (cl-remove (plist-get entry :id) pai-subagents--runs
                               :key (lambda (e) (plist-get e :id)) :test #'equal))))

(defun pai-subagents--run-by-id (id)
  "Return the run entry whose :id equals ID, or nil."
  (seq-find (lambda (e) (equal (plist-get e :id) id)) pai-subagents--runs))

(defun pai-subagents--status-text (&optional id)
  "One-line-per-run status, optionally only for ID."
  (let* ((runs (if id
                   (seq-filter (lambda (e) (string-prefix-p id (plist-get e :id)))
                               pai-subagents--runs)
                 pai-subagents--runs)))
    (if (null runs)
        "No subagent runs"
      (string-join
       (mapcar (lambda (e)
                 (let* ((raw (replace-regexp-in-string
                              "\n" " "
                              (or (plist-get e :output) (plist-get e :task) "")))
                        (len (length raw))
                        (tail (if (> len 60) (substring raw 0 60) raw)))
                   (format "%s  %-10s %-9s %-9s %6st %4d/s %6s  %s"
                           (plist-get e :id) (plist-get e :role)
                           (plist-get e :model) (plist-get e :status)
                           (pai-subagents--fmt-count (pai-subagents--tokens e))
                           (pai-subagents--tps e)
                           (pai-subagents--fmt-duration (pai-subagents--elapsed e))
                           (if (> len 60) (concat tail "…") tail))))
               runs)
       "\n"))))

;;;; Child completion and delivery

(defun pai-subagents--result-text (msgs)
  "Return the final assistant TEXT from child MESSAGES."
  (let ((last (seq-find #'pai-assistant-message-p (reverse msgs))))
    (if last (or (pai-content-text (pai-message-content last)) "") "(no response)")))

(defun pai-subagents--finish (entry result)
  "Complete run ENTRY with tool RESULT; deliver per delivery mode.
Idempotent: a second call for a finished entry is ignored."
  (when (equal (plist-get entry :status) "running")
    (let* ((timer (plist-get entry :timeout-timer)))
      (when timer (cancel-timer timer))
      (plist-put entry :timeout-timer nil))
    (let* ((errp (eq (plist-get result :is-error) t))
           (trunc (pai-tools-truncate (or (plist-get result :text) "")
                                      (plist-get entry :max-lines) nil 'head))
           (body (plist-get trunc :text)))
      (plist-put entry :ended (float-time))
      (plist-put entry :status (if errp "failed" "completed"))
      (plist-put entry :output body)
      (when (buffer-live-p (plist-get entry :parent))
        (with-current-buffer (plist-get entry :parent)
          (pai-subagents--record entry)
          (let ((on-done (plist-get entry :on-done))
                (notice (format "[subagent %s (%s) %s · %st in %s]\n%s"
                                (plist-get entry :id) (plist-get entry :role)
                                (plist-get entry :status)
                                (pai-subagents--fmt-count (pai-subagents--tokens entry))
                                (pai-subagents--fmt-duration (pai-subagents--elapsed entry))
                                body)))
            (pai-subagents--ui-refresh (plist-get entry :parent))
            (cond
             ;; Foreground: the pending tool call completes with the result.
             (on-done (funcall on-done
                               (list :content (list (pai-text body))
                                     :is-error (if errp t :false))))
             ;; Background + parent idle: wake the parent with the notice.
             ((and (boundp 'pai--active) (not pai--active)
                   (fboundp 'pai--start-run))
              (condition-case err (pai--start-run notice)
                (error (message "pai-subagents: %s" (error-message-string err)))))
             ;; Background + parent busy: steer the live run.
             ((and (boundp 'pai--active) pai--active)
              (setq pai--steering-queue (cons (pai-user-message notice) pai--steering-queue))))))))))

(defun pai-subagents--launch (role task args ctx on-done)
  "Launch a child run for ROLE with TASK; return the run entry.
ARGS/CTX come from the subagent tool call; ON-DONE completes a
foreground tool call (nil for background runs)."
  (let* ((parent (current-buffer))
         (resolved (pai-subagents--resolve-model role (plist-get args :model)
                                                 (plist-get ctx :model))))
    (unless resolved
      (error "No model resolved for role %s; set one with /subagents-model" role))
    (unless (pai-subagents-role role)
      (error "Unknown subagent role %s; see /subagents-roles" role))
    (let* ((context-mode (or (let ((c (plist-get args :context)))
                               (cond ((stringp c) (intern c))
                                     ((symbolp c) c)))
                             (plist-get (pai-subagents-role role) :context)
                             'fresh))
           (inherited (when (eq context-mode 'fork)
                        (seq-filter (lambda (m) (not (pai-system-message-p m)))
                                    pai--context-messages)))
           (sys (pai-system-message
                 (pai-build-system-prompt
                  :cwd default-directory
                  :tools (pai-tools-all)
                  :addendum (plist-get (pai-subagents-role role) :prompt))))
           (messages (append (list sys) inherited (list (pai-user-message task))))
           (tools (pai-subagents--child-tools role))
           (id (format "sub-%d" (cl-incf pai-subagents--counter)))
           (entry (list :id id :role role :task task :status "running"
                        :model (pai-model-key (car resolved))
                        :parent parent :on-done on-done
                        :started (float-time) :ended nil
                        :usage-tokens 0 :stream-chars 0))
           (timeout (plist-get args :timeout)))
      (when (cadr resolved)
        (plist-put entry :thinking (cadr resolved)))
      (setf (plist-get entry :run)
            (pai-agent-run
             (list) (pai-context messages tools)
             (append (list :model (car resolved)
                           :tool-execution 'sequential
                           :cwd default-directory)
                     (when (cadr resolved) (list :reasoning (cadr resolved))))
             (lambda (ev) (pai-subagents--observe entry ev))
             (lambda (msgs)
               (pai-subagents--finish
                entry
                (list :text (pai-subagents--result-text msgs)
                      :is-error :false)))))
      (when (and (numberp timeout) (> timeout 0))
        (plist-put
         entry :timeout-timer
         (run-at-time timeout nil
                      (lambda ()
                        (when (equal (plist-get entry :status) "running")
                          (ignore-errors (pai-agent-abort (plist-get entry :run)))
                          (pai-subagents--finish
                           entry
                           (list :text "run exceeded timeout" :is-error t)))))))
      (pai-subagents--record entry)
      (pai-subagents--ui-refresh parent)
      entry)))

(defun pai-subagents--stop (entry)
  "Abort running ENTRY and mark it stopped; return non-nil when stopped."
  (when (and entry (equal (plist-get entry :status) "running"))
    (ignore-errors (pai-agent-abort (plist-get entry :run)))
    (let ((timer (plist-get entry :timeout-timer)))
      (when timer (cancel-timer timer)))
    (plist-put entry :ended (float-time))
    (plist-put entry :status "stopped")
    (plist-put entry :output "stopped by user")
    ;; Foreground tools must not hang: deliver the stop as the result.
    (let ((on-done (plist-get entry :on-done)))
      (when on-done
        (plist-put entry :on-done nil)
        (funcall on-done (list :content (list (pai-text "subagent stopped"))
                               :is-error t))))
    (pai-subagents--record entry)
    (pai-subagents--ui-refresh (plist-get entry :parent))
    t))

;;;; Tool

(defun pai-subagents--status-result (id)
  "Tool result plist for the status action, optionally filtered by ID."
  (list :content (list (pai-text (pai-subagents--status-text id))) :is-error :false))

(defun pai-subagents--tool-execute (args ctx _on-update on-done)
  "Execute the subagent tool for ARGS in CTX via ON-DONE."
  (let ((action (or (plist-get args :action) "launch")))
    (pcase action
      ("status"
       (funcall on-done (pai-subagents--status-result (plist-get args :id))))
      ("stop"
       (if (pai-subagents--stop (pai-subagents--run-by-prefix (plist-get args :id)))
           (funcall on-done
                    (list :content (list (pai-text "stopped")) :is-error :false))
         (funcall on-done
                  (pai-tool-error-result
                   (format "No running subagent matching %s" (or (plist-get args :id) "(none)"))))))
      ("list"
       (funcall on-done
                (list :content
                      (list (pai-text
                             (string-join
                              (mapcar (lambda (pair)
                                        (format "%s: %s" (car pair)
                                                (plist-get (cdr pair) :description)))
                                      (pai-subagents-roles))
                              "\n")))
                      :is-error :false)))
      (_
       (let* ((role (plist-get args :agent))
              (task (or (plist-get args :task) "")))
         (if (or (null role) (string-empty-p role))
             (funcall on-done (pai-tool-error-result "subagent requires :agent"))
           (let ((entry (pai-subagents--launch role task args ctx
                                               (if (eq (plist-get args :async) :false)
                                                   on-done nil))))
             (unless (eq (plist-get args :async) :false)
               (funcall on-done
                        (list :content
                              (list (pai-text
                                     (format "Started subagent %s (%s, model %s) in the background. It will be delivered here when it finishes; subagent {\"action\":\"status\"} to poll."
                                             (plist-get entry :id) role
                                             (plist-get entry :model))))
                              :is-error :false
                              :details (list :run-id (plist-get entry :id))))))))))))

(defun pai-subagents--run-by-prefix (prefix)
  "Return the newest run entry whose :id starts with PREFIX."
  (seq-find (lambda (e) (and prefix (string-prefix-p prefix (plist-get e :id))))
            pai-subagents--runs))

;;;; Interactive setup

(defun pai-subagents--set-override-model (role spec)
  "Set the per-ROLE model override to SPEC (\"provider/id[:thinking]\") and persist.
SPEC of \"inherit\" makes ROLE follow the parent session model.  Return SPEC."
  (let* ((parsed (pai-subagents--parse-model-spec spec))
         (config (copy-sequence (pai-subagents--config)))
         (overrides (plist-get config :agent-overrides))
         (entry (copy-sequence (cdr (assoc role overrides)))))
    (setq entry (plist-put (or entry '()) :model (car parsed)))
    (when (cadr parsed) (setq entry (plist-put entry :thinking (cadr parsed))))
    (setq config (plist-put config :agent-overrides
                            (cons (cons role entry)
                                  (cl-remove role overrides :key #'car :test #'equal))))
    (pai-settings-set :subagents config 'project)
    spec))

(defun pai-subagents--role-model-display (role)
  "Return the effective model spec to show for ROLE in the settings screen."
  (or (plist-get (pai-subagents--override-for role) :model)
      (plist-get (pai-subagents-role role) :model)
      (plist-get (pai-subagents--config) :default-model)
      "inherit"))

(defun pai-subagents--set-override-thinking (role level)
  "Set the per-ROLE thinking override to LEVEL and persist.
LEVEL of \"inherit\" (or blank) clears the override so ROLE falls through to its
frontmatter, then the default thinking level.  Return LEVEL."
  (let* ((config (copy-sequence (pai-subagents--config)))
         (overrides (plist-get config :agent-overrides))
         (entry (copy-sequence (cdr (assoc role overrides)))))
    (setq entry (plist-put (or entry '()) :thinking
                           (unless (member level '("inherit" "" nil))
                             (intern level))))
    (setq config (plist-put config :agent-overrides
                            (cons (cons role entry)
                                  (cl-remove role overrides :key #'car :test #'equal))))
    (pai-settings-set :subagents config 'project)
    level))

(defun pai-subagents--role-thinking-display (role)
  "Return the effective thinking level to show for ROLE in the settings screen."
  (let ((v (or (plist-get (pai-subagents--override-for role) :thinking)
               (plist-get (pai-subagents-role role) :thinking)
               (plist-get (pai-subagents--config) :default-thinking))))
    (if v (symbol-name v) "inherit")))

;;;; Role definition management (create / override / delete)

(defun pai-subagents--roles-dir ()
  "Return the global roles directory, creating it on demand."
  (let ((dir (expand-file-name "subagents" pai-directory)))
    (make-directory dir t)
    dir))

(defun pai-subagents--role-file (role)
  "Return an existing role file for ROLE (project first, then global), or nil."
  (let ((cands (list (and (boundp 'pai--trusted) pai--trusted
                          (expand-file-name (format ".pai/subagents/%s.md" role)
                                            default-directory))
                     (expand-file-name (format "%s.md" role)
                                       (expand-file-name "subagents" pai-directory)))))
    (seq-find (lambda (f) (and f (file-readable-p f))) cands)))

(defun pai-subagents--role-template (role)
  "Return markdown template text for a new ROLE, seeded from a builtin if any."
  (let* ((builtin (car (cdr (assoc role pai-subagents--builtins))))
         (desc (or (plist-get builtin :description) "One-line description of the role."))
         (thinking (or (plist-get builtin :thinking) 'medium))
         (prompt (or (plist-get builtin :prompt)
                     "You are ROLE. Describe the role's job and constraints here.")))
    (format (concat "---\n"
                    "name: %s\n"
                    "description: %s\n"
                    "thinking: %s\n"
                    "# model: provider/id   # optional default model for this role\n"
                    "# tools: bash, read_file   # optional allowlist\n"
                    "# context: fork   # fresh | fork\n"
                    "---\n\n%s\n")
            role desc thinking prompt)))

(defun pai-subagents--edit-role (role)
  "Open ROLE's definition file for editing, creating it from a template first.
Creating a file for a builtin name overrides the builtin; the role is also
re-enabled if it was disabled.  Reloads roles when the buffer is saved."
  (let ((file (or (pai-subagents--role-file role)
                  (let ((f (expand-file-name (format "%s.md" role)
                                             (pai-subagents--roles-dir))))
                    (unless (file-exists-p f)
                      (with-temp-file f (insert (pai-subagents--role-template role))))
                    f))))
    (pai-subagents--set-role-disabled role nil)
    (find-file file)
    (add-hook 'after-save-hook #'pai-subagents-reload-roles nil t)
    (message "Editing role %s; save to apply." role)))

(defun pai-subagents--delete-role (role)
  "Delete ROLE: remove any user role file(s) and disable a builtin of that name.
Reloads roles afterwards."
  (dolist (f (list (expand-file-name (format ".pai/subagents/%s.md" role)
                                     default-directory)
                   (expand-file-name (format "%s.md" role)
                                     (expand-file-name "subagents" pai-directory))))
    (when (and f (file-exists-p f)) (delete-file f)))
  (when (assoc role pai-subagents--builtins)
    (pai-subagents--set-role-disabled role t))
  (pai-subagents-reload-roles)
  (message "Deleted role %s." role))

(defun pai-subagents--set-role-disabled (role disabled)
  "Add or remove ROLE from the settings `:disabled-roles' list per DISABLED."
  (let* ((cur (pai-subagents--disabled-roles))
         (new (if disabled (cons role (remove role cur)) (remove role cur))))
    (pai-subagents--config-set :disabled-roles (delete-dups new))))

(defun pai-subagents-reload-roles ()
  "Reload role files for the current instance (project roles only when trusted)."
  (setq pai-subagents--roles nil)
  (pai-subagents-load-roles (when (and (boundp 'pai--trusted) pai--trusted)
                              default-directory)))

(defun pai-subagents--settings-role-items ()
  "Return one custom row per role: a model chooser and a thinking chooser.
Each row runs its reads and writes in the session buffer (`custom' items are
rendered there); callbacks re-enter it explicitly since they fire later."
  (mapcar
   (lambda (role)
     (list
      :key (intern (format ":role-%s" role)) :type 'custom
      :render
      (lambda (refresh)
        (let ((target (current-buffer))     ; custom render runs in the session buffer
              (desc (plist-get (pai-subagents-role role) :description)))
          (cl-flet ((in-target (fn v)
                      (when (buffer-live-p target)
                        (with-current-buffer target (funcall fn role v)))
                      (funcall refresh)))
            (apply
             #'vui-hstack
             (delq nil
                   (list
                    (vui-box (vui-text role) :width 18 :align :left)
                    (vui-select
                     :value (pai-subagents--role-model-display role)
                     :options (cons "inherit" (pai-model-keys))
                     :on-change (lambda (v) (in-target #'pai-subagents--set-override-model v)))
                    (vui-text "think:")
                    (vui-select
                     :value (pai-subagents--role-thinking-display role)
                     :options (cons "inherit"
                                    (mapcar #'symbol-name pai-subagents--thinking-levels))
                     :on-change (lambda (v) (in-target #'pai-subagents--set-override-thinking v)))
                    (when desc
                      (vui-muted (concat " " (truncate-string-to-width desc 28 nil nil "…"))))))))))))
   (pai-subagents-role-names)))

(defun pai-subagents--settings-manage-items ()
  "Return dynamic action items for managing role definitions.
Includes per-role Edit/Delete and re-enable actions for disabled roles."
  (append
   (list (list :key :roles-new :type 'action
               :label "New role…"
               :doc "Create a role definition file and open it"
               :action (lambda ()
                         (let ((name (read-string "New role name: ")))
                           (when (and name (not (string-empty-p name)))
                             (pai-subagents--edit-role name)))))
         (list :key :roles-edit :type 'action
               :label "Edit role…"
               :doc "Open (or create, overriding a builtin) a role file"
               :action (lambda ()
                         (let ((name (completing-read "Edit role: "
                                                      (pai-subagents-role-names) nil nil)))
                           (when (and name (not (string-empty-p name)))
                             (pai-subagents--edit-role name)))))
         (list :key :roles-delete :type 'action
               :label "Delete role…"
               :doc "Remove a user role file / disable a builtin"
               :action (lambda ()
                         (let ((name (completing-read "Delete role: "
                                                      (pai-subagents-role-names) nil t)))
                           (when (and name (not (string-empty-p name))
                                      (yes-or-no-p (format "Delete role %s? " name)))
                             (pai-subagents--delete-role name))))))
   (mapcar (lambda (role)
             (list :key (intern (format ":role-enable-%s" role)) :type 'action
                   :label (format "Re-enable %s" role)
                   :doc "Restore this disabled role"
                   :action (lambda () (pai-subagents--set-role-disabled role nil))))
           (pai-subagents--disabled-roles))))

(defun pai-subagents-set-model (&optional role)
  "Interactively set the model (and thinking) for subagent ROLE.
Writes the project settings override.  With ROLE non-nil skip the prompt."
  (interactive)
  (let* ((role (or role (completing-read "Role: " (pai-subagents-role-names) nil t)))
         (spec (completing-read (format "Model for %s (provider/id[:thinking], RET = inherit): " role)
                                (append '("inherit")
                                        (mapcar #'symbol-name pai-subagents--thinking-levels)
                                        (pai-model-keys))
                                nil nil nil nil "inherit"))
         (parsed (pai-subagents--parse-model-spec spec)))
    (pai-subagents--set-override-model role spec)
    (message "subagent %s -> %s%s" role (car parsed)
             (if (cadr parsed) (format " :%s" (cadr parsed)) ""))))

;;;; Slash commands

(defun pai-subagents-command (args _ctx)
  "Handler for `/subagents': list runs; with ARGS stop the matching id."
  (if (string-empty-p (string-trim args))
      (list :message (pai-subagents--status-text))
    (if (pai-subagents--stop (pai-subagents--run-by-prefix (string-trim args)))
        (list :message (format "Stopped %s" (string-trim args)))
      (list :message (format "No running subagent matching %s" (string-trim args))))))

(defun pai-subagents-models-command (args _ctx)
  "Handler for `/subagents-models [role]': show the live role-model mapping."
  (let* ((role (string-trim args))
         (parent (and (boundp 'pai--model) pai--model))
         (names (if (string-empty-p role) (pai-subagents-role-names) (list role))))
    (list :message
          (string-join
           (mapcar (lambda (name)
                     (let ((resolved (pai-subagents--resolve-model name nil parent)))
                       (format "%-16s %s%s" name
                               (if resolved (pai-model-key (car resolved)) "(unresolved)")
                               (if (and resolved (cadr resolved))
                                   (format " :%s" (cadr resolved)) ""))))
                   names)
           "\n"))))

(defun pai-subagents-reload-command (_args _ctx)
  "Handler for `/subagents-reload': re-read role files into this instance."
  (let ((n (pai-subagents-load-roles default-directory)))
    (list :message (format "Loaded %d user role(s)%s" n
                           (if pai-subagents--roles
                               (format ": %s" (string-join (mapcar #'car pai-subagents--roles) ", "))
                             "")))))

(defun pai-subagents-roles-command (_args _ctx)
  "Handler for `/subagents-roles': list roles with descriptions."
  (list :message
        (string-join
         (mapcar (lambda (pair) (format "%s: %s" (car pair)
                                        (plist-get (cdr pair) :description)))
                 (pai-subagents-roles))
         "\n")))

;;;; Tool and extension registration

(pai-register-tool
 (list :name "subagent"
       :label "Subagent"
       :description
       "Delegate a focused task to a child agent run. Roles: scout (fast codebase recon), researcher (web/docs research), evidence-auditor (claim checking), worker (implementation), reviewer (code review), oracle (second opinion), delegate (general). Use for second opinions, reviews, parallel audits, and research. Children run without blocking the session; async completions are delivered as a follow-up."
       :prompt-snippet "subagent: delegate a task to a focused child agent"
       :execution-mode 'parallel
       :parameters
       (pai-object-schema
        (list :agent (pai-string-schema "Role name (scout, worker, reviewer, oracle, or a user-defined role).")
              :task (pai-string-schema "The task for the child agent.")
              :model (pai-string-schema "Optional model override \"provider/id\" or \"provider/id:thinking\".")
              :async (pai-boolean-schema "Run in background (default true). false keeps the tool call pending until the child finishes.")
              :context (pai-string-schema "\"fork\" inherits the parent conversation (role default); \"fresh\" starts clean.")
              :action (pai-string-schema "launch (default), status, stop, or list.")
              :id (pai-string-schema "Run id for the status/stop actions (prefix ok).")
              :max-lines (pai-number-schema "Max output lines returned to the parent.")
              :timeout (pai-number-schema "Max runtime in seconds."))
        '("agent"))
       :execute #'pai-subagents--tool-execute))

;;;###autoload
(defun pai-subagents-setup-role-model (role)
  "Set the model for subagent ROLE from outside a chat buffer."
  (interactive "SRole: ")
  (pai-subagents-set-model (symbol-name role)))

;;;; Extension entry point

(pai-register-extension
 (lambda (api)
   (pai-ext-register-command api "subagents"
                             :description "List subagent runs (arg: id prefix to stop)"
                             :handler #'pai-subagents-command
                             :arg-completions (lambda (_p) (mapcar (lambda (e) (plist-get e :id)) pai-subagents--runs)))
   (pai-ext-register-command api "subagents-models"
                             :description "Show role-to-model mapping (arg: role)"
                             :handler #'pai-subagents-models-command
                             :arg-completions (lambda (_p) (pai-subagents-role-names)))
   (pai-ext-register-command api "subagents-model"
                             :description "Set the model for a role"
                             :handler (lambda (args _ctx) (pai-subagents-set-model (string-trim args)))
                             :arg-completions (lambda (_p) (pai-subagents-role-names)))
   (pai-ext-register-command api "subagents-roles"
                             :description "List available roles"
                             :handler (lambda (_args _ctx)
                                        (list :message (string-join (mapcar #'car (pai-subagents-roles)) ", "))))
   (pai-ext-register-command api "subagents-reload"
                             :description "Reload subagent role definitions"
                             :handler #'pai-subagents-reload-command)
   ;; Load user roles for this instance (project roles only when trusted).
   (pai-subagents-load-roles (when (and (boundp 'pai--trusted) pai--trusted)
                               default-directory)))
 "subagents")

;;;; Settings screen integration
;; When the vui settings screen is available, expose the subagent defaults so
;; every session shows and edits them from `/menu'.  `with-eval-after-load'
;; keeps this a soft dependency: the extension works without the screen too.
(with-eval-after-load 'pai-settings-ui
  (pai-settings-ui-register-section 'subagents "Subagents" 50)
  (pai-settings-ui-register-subsection 'subagents 'defaults "Defaults" 10)
  (pai-settings-ui-register-item
   'subagents 'defaults
   :key :subagents-default-model :type 'choice :label "Default model"
   :doc "Model for roles without an explicit model (\"inherit\" = parent)"
   :choices (lambda () (cons "inherit" (pai-model-keys)))
   :get (lambda () (or (plist-get (pai-subagents--config) :default-model) "inherit"))
   :set (lambda (v) (pai-subagents--config-set :default-model v)))
  (pai-settings-ui-register-item
   'subagents 'defaults
   :key :subagents-default-thinking :type 'choice :label "Default thinking"
   :doc "Reasoning level applied to subagents by default"
   :choices (lambda () (mapcar #'symbol-name pai-subagents--thinking-levels))
   :get (lambda ()
          (let ((s (plist-get (pai-subagents--config) :default-thinking)))
            (if s (symbol-name s) "off")))
   :set (lambda (v) (pai-subagents--config-set :default-thinking (intern v))))
  (pai-settings-ui-register-subsection 'subagents 'roles "Roles (model · thinking)" 20)
  (pai-settings-ui-register-dynamic-items
   'subagents 'roles #'pai-subagents--settings-role-items)
  (pai-settings-ui-register-subsection 'subagents 'manage "Manage roles" 30)
  (pai-settings-ui-register-dynamic-items
   'subagents 'manage #'pai-subagents--settings-manage-items))

(provide 'pai-subagents)
;;; pai-subagents.el ends here
