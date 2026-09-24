;;; pai-memory-worker.el --- Background workers for pai-memory -*- lexical-binding: t; -*-

;;; Commentary:

;; The worker runtime of the learning-memory extension (SPEC §4.3).
;;
;; A worker is a background, in-process `pai-agent-run' -- the same mechanism
;; pai-subagents uses -- with a role-specific system prompt and a small,
;; restricted tool set.  It never blocks the UI and never touches the owning
;; buffer's live context; results reach the owner through the ON-DONE
;; callback, which runs in the owning buffer.
;;
;; While a worker runs it is shown above the prompt through `pai-activity',
;; one line per worker, like running subagents:
;;
;;   🧠 obs-2   observer      310t ·   42 tok/s · 7s · 8.4k tokens of transcript
;;
;; `pai-activity-stop' (and `/memory stop') aborts a worker.
;;
;; Every finished run:
;;   * saves its transcript as an ordinary session file in the owning session's
;;     `memory-workers/' subdirectory (hidden from /resume), and
;;   * appends a `memory.cost' custom entry -- run id, role, model, status,
;;     usage, dollar cost, duration, transcript path -- to the owning session.
;;
;; File tools handed to a worker are confined to one directory by
;; `pai-memory-confined-tools': any path that resolves outside it (after
;; symlinks) is refused.  Workers get no shell, elisp, buffer, MCP or subagent
;; tools.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-models)
(require 'pai-tools)
(require 'pai-agent)
(require 'pai-session)
(require 'pai-activity)
(require 'pai-model-resolver)

(defvar pai--model)
(defvar pai--session)

;;;; Roles

(defconst pai-memory-worker-roles
  '((observer     :prefix "obs" :label "observer"     :model-role :memory-observer
                  :description "Memory: turns conversation into notes (frequent; use a cheap model)")
    (consolidator :prefix "con" :label "consolidator" :model-role :memory-consolidator
                  :description "Memory: files notes into topic files, writes reflections (occasional; mid-tier model)")
    (promoter     :prefix "pro" :label "promoter"     :model-role :memory-promoter
                  :description "Memory: proposes skills and long-term memory (rare)")
    (merger       :prefix "mrg" :label "merger"       :model-role :memory-merger
                  :description "Memory: merges skills and project topics (occasional)")
    (topics       :prefix "top" :label "topic merger" :model-role :memory-merger
                  :description "Memory: merges skills and project topics (occasional)")
    (reflector    :prefix "ref" :label "reflector"    :model-role :memory-consolidator
                  :description "Memory: files notes into topic files, writes reflections (occasional; mid-tier model)")
    (evaluator    :prefix "chk" :label "skill check"  :model-role :memory-evaluator
                  :description "Memory: test-runs proposed skills (on demand; needs a capable model)"))
  "Worker roles: activity id prefix, display label, scoped-model role, description.")

(defun pai-memory-worker-role (role)
  "Return the plist describing worker ROLE, or signal an error."
  (or (alist-get role pai-memory-worker-roles)
      (error "Unknown memory worker role: %S" role)))

(defun pai-memory-worker-register-model-roles ()
  "Register the memory model roles, each falling back to `:task'."
  (dolist (r pai-memory-worker-roles)
    (pai-register-model-role (plist-get (cdr r) :model-role) :task
                             (plist-get (cdr r) :description))))

(define-error 'pai-memory-model-unavailable "Memory model unavailable")

(defvar-local pai-memory-model-problem nil
  "Why the last memory worker could not start for want of its model, or nil.
Reported once, shown by /memory and the widget, cleared on the next launch.")

(defun pai-memory-worker--resolve-model (role)
  "Return ROLE's model, recording (and reporting once) when it is unavailable."
  (condition-case err
      (prog1 (pai-memory-worker-model role)
        (setq pai-memory-model-problem nil))
    (pai-memory-model-unavailable
     (let ((why (cadr err)))
       (unless (equal why pai-memory-model-problem)
         (message "pai-memory: %s" why))
       (setq pai-memory-model-problem why)
       (signal (car err) (cdr err))))))

(defun pai-memory-worker-model (role &optional fallback)
  "Return the model for worker ROLE.
When a scoped model is configured for ROLE's chain (the role itself, `:task'
or `:main'), that model is used -- discovered on demand when its provider's
models are not registered yet -- and signal `pai-memory-model-unavailable'
when it still cannot be found: silently running on another (often far more
expensive) model is worse than not running.  With nothing scoped, use
FALLBACK or the session's current model."
  (let* ((res (pai-scoped-model-resolution (plist-get (pai-memory-worker-role role) :model-role)))
         (id (car res)) (source (cdr res)))
    (if (keywordp source)
        (or (pai-model-ensure id)
            (signal 'pai-memory-model-unavailable
                    (list (format "%s model %s (from %s) is not available; check the provider or /scoped-models"
                                  role id (pai-model-role-name source)))))
      (or fallback (and (boundp 'pai--model) pai--model) (and id (pai-model-ensure id))))))

;;;; Confined tools

(defun pai-memory--inside-p (path root)
  "Return non-nil when PATH resolves to ROOT or something inside it.
Both are resolved with `file-truename', so symlinks cannot escape."
  (let ((true (file-name-as-directory (file-truename path)))
        (base (file-name-as-directory (file-truename root))))
    (string-prefix-p base true)))

(defun pai-memory--confine (tool root &optional extra-roots)
  "Return a copy of TOOL whose paths must resolve inside ROOT or EXTRA-ROOTS.
Relative paths resolve against ROOT."
  (let ((execute (plist-get tool :execute))
        (name (plist-get tool :name))
        (roots (cons root extra-roots)))
    (plist-put
     (copy-sequence tool) :execute
     (lambda (args ctx on-update on-done)
       (let* ((raw (or (plist-get args :path) "."))
              (full (expand-file-name raw root)))
         (if (not (seq-some (lambda (r) (pai-memory--inside-p full r)) roots))
             (funcall on-done
                      (pai-tool-error-result
                       (format "%s: path %s is outside the allowed directories: %s"
                               name raw (mapconcat #'abbreviate-file-name roots ", "))))
           (funcall execute args (plist-put (copy-sequence ctx) :cwd root)
                    on-update on-done)))))))

(defun pai-memory-confined-tools (root names &optional extra-roots)
  "Return the built-in tools NAMES (strings) confined to directory ROOT.
Paths resolve relative to ROOT, and anything resolving outside ROOT and
EXTRA-ROOTS (including through symlinks) is refused.  ROOT is created when
missing; EXTRA-ROOTS that do not exist are dropped."
  (let ((root (file-name-as-directory (expand-file-name root)))
        (extra (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
                       (seq-filter #'file-directory-p extra-roots))))
    (make-directory root t)
    (mapcar (lambda (name)
              (let ((tool (pai-tool-get name)))
                (unless tool (error "No built-in tool named %s" name))
                (pai-memory--confine tool root extra)))
            names)))

(cl-defun pai-memory-tool (name description properties required fn &key terminal)
  "Return an executable tool plist NAME for a worker.
DESCRIPTION and PROPERTIES (a plist of JSON schemas) describe it; REQUIRED
lists required property names.  FN is called with the decoded argument plist
and returns the result text; a signalled error becomes an error result.
TERMINAL non-nil marks a tool whose call ends the worker's run."
  (list :name name :label name :description description
        :parameters (pai-object-schema properties required)
        :terminal terminal
        :deferred :false
        :execute (lambda (args _ctx _on-update on-done)
                   (funcall on-done
                            (condition-case err
                                (pai-tool-ok-result (or (funcall fn args) "ok"))
                              (error (pai-tool-error-result
                                      (error-message-string err))))))))

;;;; Transcript and cost

(defun pai-memory-worker--usage (messages)
  "Return the summed usage of the assistant MESSAGES."
  (let ((total (pai-usage)))
    (dolist (m messages)
      (when (and (pai-assistant-message-p m) (plist-get m :usage))
        (setq total (pai-usage-add total (plist-get m :usage)))))
    total))

(defun pai-memory-worker--transcript-dir (owner-session cwd)
  "Return the directory for worker transcripts of OWNER-SESSION (or CWD)."
  (expand-file-name "memory-workers"
                    (if owner-session
                        (file-name-directory (pai-session-file owner-session))
                      (pai-session-directory cwd))))

(defun pai-memory-worker--save-transcript (entry messages)
  "Save ENTRY's worker MESSAGES as a session file; return its path or nil."
  (let* ((data (plist-get entry :data))
         (dir (pai-memory-worker--transcript-dir (plist-get data :owner-session)
                                                 (plist-get data :cwd)))
         (file (expand-file-name (concat (plist-get data :run-id) ".jsonl") dir)))
    (condition-case err
        (let ((s (pai-session-new (plist-get data :cwd) file)))
          (pai-session-set-name s (format "memory %s" (plist-get entry :label)))
          (dolist (m messages) (pai-session-append-message s m))
          file)
      (error (message "pai-memory: could not save worker transcript: %s"
                      (error-message-string err))
             nil))))

(defvar pai-memory-worker-cost-functions nil
  "Abnormal hook run with each finished worker run's `memory.cost' data plist.
Runs for every run, whether or not it has an owning session.")

(defun pai-memory-worker--record-cost (entry status usage transcript)
  "Record ENTRY's cost: a `memory.cost' entry in its owning session, and hooks."
  (let* ((data (plist-get entry :data))
         (session (plist-get data :owner-session))
         (model (plist-get data :model))
         (cost (list :runId (plist-get data :run-id)
                     :role (symbol-name (plist-get data :role))
                     :model (and model (pai-model-key model))
                     :status status
                     :usage usage
                     :cost (if model (pai-usage-cost usage model) 0.0)
                     :durationMs (round (* 1000 (pai-activity-elapsed entry)))
                     :transcript (and transcript (abbreviate-file-name transcript)))))
    (plist-put entry :cost (plist-get cost :cost))
    (when session
      (pai-session-append-custom session "memory.cost" cost))
    (condition-case err
        (run-hook-with-args 'pai-memory-worker-cost-functions cost)
      (error (message "pai-memory: cost hook failed: %s" (error-message-string err))))))

;;;; Launch and completion

(defun pai-memory-worker--status (entry messages)
  "Return the final status string of ENTRY given its run MESSAGES."
  (let ((last (seq-find #'pai-assistant-message-p (reverse messages))))
    (cond ((plist-get entry :stop-requested) "stopped")
          ((plist-get entry :timed-out) "timeout")
          ((null last) "failed")
          ((memq (plist-get last :stop-reason) '(error aborted)) "failed")
          (t "completed"))))

(defun pai-memory-worker--complete (entry messages)
  "Finish ENTRY once with its run MESSAGES; notify the owner."
  (unless (plist-get entry :completed)
    (plist-put entry :completed t)
    (let ((timer (plist-get entry :timeout-timer)))
      (when (timerp timer) (cancel-timer timer)))
    (let* ((data (plist-get entry :data))
           (all (append (plist-get data :initial-messages) messages))
           (status (pai-memory-worker--status entry messages))
           (usage (pai-memory-worker--usage messages))
           (transcript (pai-memory-worker--save-transcript entry all)))
      (plist-put entry :usage usage)
      (plist-put entry :transcript transcript)
      (pai-memory-worker--record-cost entry status usage transcript)
      (pai-activity-finish entry status)
      (let ((on-done (plist-get data :on-done))
            (owner (plist-get entry :buffer)))
        (when (and on-done (buffer-live-p owner))
          (with-current-buffer owner
            (condition-case err (funcall on-done status messages entry)
              (error (message "pai-memory: %s worker callback failed: %s"
                              (plist-get entry :label) (error-message-string err))))
            ;; show the result (+N) the callback recorded
            (pai-activity-refresh owner)))))))

(defun pai-memory-worker--abort (entry)
  "Abort ENTRY's run and complete it with the messages it produced so far.
An aborted stream may never report back, so completion happens here; a late
report from the run is then ignored."
  (let* ((run (plist-get entry :run))
         (so-far (and run (copy-sequence (pai-run-new-messages run)))))
    (when run (ignore-errors (pai-agent-abort run)))
    (pai-memory-worker--complete entry so-far)))

(defun pai-memory-worker--stop-after-turn (entry max-turns)
  "Return a :should-stop-after-turn hook for ENTRY capped at MAX-TURNS."
  (let ((turns 0))
    (lambda (last-turn)
      (setq turns (1+ turns))
      (or (plist-get entry :terminal-called)
          (and max-turns (>= turns max-turns))
          ;; a turn that called no tools is the model's final answer
          (null (plist-get last-turn :tool-results))))))

(defun pai-memory-worker--prepare-tools (entry tools)
  "Return worker TOOLS ready for ENTRY's run.
Every tool is eager -- a worker's run is short and its tool set fixed, so the
deferred-schema reveal round trip would only waste a turn -- and calling a
:terminal tool flags ENTRY to stop after that turn."
  (mapcar (lambda (tool)
            (let ((tool (plist-put (copy-sequence tool) :deferred :false)))
              (if (not (plist-get tool :terminal))
                  tool
                (let ((execute (plist-get tool :execute)))
                  (plist-put tool :execute
                             (lambda (args ctx on-update on-done)
                               (plist-put entry :terminal-called t)
                               (funcall execute args ctx on-update on-done)))))))
          tools))

(cl-defun pai-memory-worker-launch (role &key system prompt tools cwd detail model
                                         (timeout 300) (max-turns 20) on-done)
  "Launch a background memory worker for ROLE and return its activity entry.
ROLE is a symbol from `pai-memory-worker-roles'.  SYSTEM is the system prompt
text and PROMPT the user message.  TOOLS are executable tool plists (see
`pai-memory-confined-tools' and `pai-memory-tool'); CWD is the run's working
directory (default `default-directory').  DETAIL is the text shown after the
metrics in the activity line.  MODEL overrides the role's scoped model.
The run is aborted after TIMEOUT seconds and after MAX-TURNS turns.

ON-DONE is called once, in the owning (current) buffer, with STATUS
\(\"completed\", \"failed\", \"stopped\" or \"timeout\"), the run's new
MESSAGES and the activity ENTRY."
  (let* ((spec (pai-memory-worker-role role))
         (model (or model (pai-memory-worker--resolve-model role)))
         (cwd (file-name-as-directory (expand-file-name (or cwd default-directory))))
         (owner-session (and (boundp 'pai--session) pai--session)))
    (unless model
      (error "No model for memory %s; set one with /scoped-models" (plist-get spec :label)))
    (let* ((entry (pai-activity-start
                   :prefix (plist-get spec :prefix) :kind "memory" :glyph "🧠"
                   :label (plist-get spec :label) :detail detail))
           (sys (pai-system-message system))
           (user (pai-user-message prompt))
           (tools (pai-memory-worker--prepare-tools entry tools)))
      (plist-put entry :data
                 (list :role role :model model :cwd cwd :on-done on-done
                       :owner-session owner-session
                       :initial-messages (list sys)
                       :run-id (format "%s-%s" (format-time-string "%Y%m%dT%H%M%S")
                                       (plist-get entry :id))))
      (plist-put entry :on-stop
                 (lambda (e)
                   (plist-put e :stop-requested t)
                   (pai-memory-worker--abort e)))
      (when (and (numberp timeout) (> timeout 0))
        (plist-put entry :timeout-timer
                   (run-at-time timeout nil
                                (lambda ()
                                  (unless (plist-get entry :completed)
                                    (plist-put entry :timed-out t)
                                    (pai-memory-worker--abort entry))))))
      (plist-put entry :run
                 (pai-agent-run
                  (list user)
                  (pai-context (list sys) tools)
                  (list :model model
                        :tool-execution 'sequential
                        :cwd cwd
                        :should-stop-after-turn
                        (pai-memory-worker--stop-after-turn entry max-turns))
                  (lambda (ev) (pai-activity-observe entry ev))
                  (lambda (msgs) (pai-memory-worker--complete entry msgs))))
      entry)))

(defun pai-memory-worker-running (&optional buffer)
  "Return the memory workers running in BUFFER (default current), oldest first."
  (pai-activity-running "memory" buffer))

(defun pai-memory-worker-stop-all (&optional buffer)
  "Stop every memory worker running in BUFFER; return how many were stopped."
  (let ((n 0))
    (dolist (e (pai-memory-worker-running buffer))
      (when (pai-activity-stop e) (cl-incf n)))
    n))

(provide 'pai-memory-worker)
;;; pai-memory-worker.el ends here
