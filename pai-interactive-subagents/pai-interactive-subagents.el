;;; pai-interactive-subagents.el --- Live subagent sessions in sibling buffers -*- lexical-binding: t; -*-

;; Successor to `pai-subagents' (itself a port of
;; https://github.com/nicobailon/pi-subagents, MIT).  Same roles, same model
;; resolution, same non-blocking delivery -- but a subagent is no longer a
;; headless run hidden inside the parent's buffer.  Each one is a *live agent
;; session* opened in a buffer next to the parent's, which means:
;;
;;   * you can read what the subagent is doing while it works;
;;   * you can type into it yourself -- it is a normal chat session;
;;   * it can talk back to the agent that spawned it (`reply_to_parent');
;;   * it stays alive between turns, so the parent can keep delegating to it
;;     with the `say' action instead of starting from scratch.
;;
;; How a session is created and driven is a pluggable BACKEND (see
;; `pai-isub-backend'): "pai" (a pai chat buffer) and "agent-shell" (an
;; external ACP agent in an `agent-shell' buffer) ship here.  The backend is
;; chosen per run or per role.  An agent-shell role runs a HARNESS (Claude
;; Code, Gemini CLI, Codex, ...) with that harness's own models, chosen with
;; `/subagents-harness'; pai models only apply to the pai backend.

;;; Commentary:

;; Entry point: wires roles (`pai-isub-roles'), the backend protocol
;; (`pai-isub-backend'), the built-in pai backend (`pai-isub-session') and the
;; run bookkeeping (`pai-isub-runs') into the `subagent' tool, the
;; `/subagents*' slash commands, the child-side `/parent' command, and the
;; settings screen.
;;
;; The `subagent' tool: {agent, task, id?, action?, model?, backend?, harness?, async?,
;; context?, max-lines?, timeout?}.  Actions:
;;
;;   launch (default)  open a new subagent session and give it a task
;;   say               send another message to a live session
;;   read              read the tail of a session's transcript
;;   status            list this parent's sessions
;;   stop              interrupt a session's current turn (it stays alive)
;;   close             end a session and close its buffer
;;   list              list the available roles
;;
;; Everything is asynchronous by default: launch/say return a receipt at once
;; and the child's answer arrives as a follow-up parent turn.  With
;; `async: false' the tool call is held open until that turn finishes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai)
(require 'pai-ext)
(require 'pai-models)
(require 'pai-settings)
(require 'pai-isub-backend)
(require 'pai-isub-roles)
(require 'pai-isub-session)
(require 'pai-isub-agent-shell)
(require 'pai-isub-runs)

(declare-function pai-settings-ui-register-section "pai-settings-ui")
(declare-function pai-settings-ui-register-subsection "pai-settings-ui")
(declare-function pai-settings-ui-register-item "pai-settings-ui")
(declare-function pai-settings-ui-register-dynamic-items "pai-settings-ui")
(declare-function vui-hstack "vui")
(declare-function vui-box "vui")
(declare-function vui-text "vui")
(declare-function vui-select "vui")
(declare-function vui-muted "vui")

;;;; Tool

(defun pai-isub--ok (text &optional details)
  "Return a successful tool result carrying TEXT and optional DETAILS."
  (append (list :content (list (pai-text text)) :is-error :false)
          (when details (list :details details))))

(defun pai-isub--foreground-p (args)
  "Return non-nil when ARGS asks for a blocking (async false) call."
  (eq (plist-get args :async) :false))

(defun pai-isub--say-text (args)
  "Return the message text in ARGS for the say action."
  (string-trim (or (plist-get args :task) (plist-get args :message) "")))

(defun pai-isub--require-run (args)
  "Return the run entry addressed by ARGS, or signal an error."
  (let* ((id (plist-get args :id))
         (entry (pai-isub-run-by-prefix id)))
    (unless entry
      (error "No subagent matching %s; see subagent {\"action\":\"status\"}"
             (or id "(newest)")))
    entry))

(defun pai-isub--launch-receipt (entry)
  "Return the receipt text handed back for a background launch of ENTRY."
  (let ((buffer (pai-isub-entry-buffer entry)))
    (format "Opened subagent %s (role %s, backend %s, model %s)%s.%s It is running your task now; its answer will be delivered here when the turn ends. Continue the conversation with subagent {\"action\":\"say\",\"id\":\"%s\",\"task\":\"…\"}; inspect it with action \"read\"."
            (plist-get entry :id) (plist-get entry :role)
            (plist-get entry :backend) (or (plist-get entry :model) "-")
            (if (buffer-live-p buffer) (format " in buffer %s" (buffer-name buffer)) "")
            (if (plist-get entry :note) (format " Note: %s." (plist-get entry :note)) "")
            (plist-get entry :id))))

(defun pai-isub--roles-text (&optional with-source)
  "Return the role list as NAME: DESCRIPTION lines.
WITH-SOURCE appends each role's origin (see `pai-isub-role-source')."
  (string-join
   (mapcar (lambda (pair)
             (format "%s: %s%s" (car pair) (plist-get (cdr pair) :description)
                     (if with-source (format " [%s]" (pai-isub-role-source (car pair))) "")))
           (pai-isub-roles))
   "\n"))

(defun pai-isub--tool-launch (args ctx on-done)
  "Handle the launch action with ARGS in CTX, completing through ON-DONE."
  (let ((role (plist-get args :agent))
        (task (string-trim (or (plist-get args :task) ""))))
    (cond
     ((or (null role) (string-empty-p role))
      (funcall on-done (pai-tool-error-result "subagent launch requires \"agent\"")))
     ((string-empty-p task)
      (funcall on-done (pai-tool-error-result "subagent launch requires \"task\"")))
     (t
      (let* ((foreground (pai-isub--foreground-p args))
             (entry (pai-isub-launch role task args ctx (and foreground on-done))))
        (unless foreground
          (funcall on-done (pai-isub--ok (pai-isub--launch-receipt entry)
                                         (list :run-id (plist-get entry :id))))))))))

(defun pai-isub--tool-say (args on-done)
  "Handle the say action with ARGS, completing through ON-DONE."
  (let ((text (pai-isub--say-text args)))
    (if (string-empty-p text)
        (funcall on-done (pai-tool-error-result "subagent say requires \"task\""))
      (let* ((entry (pai-isub--require-run args))
             (foreground (pai-isub--foreground-p args)))
        (pai-isub-say entry text args (and foreground on-done))
        (unless foreground
          (funcall on-done
                   (pai-isub--ok
                    (format "Sent to subagent %s (%s). Its reply will be delivered here."
                            (plist-get entry :id) (plist-get entry :role))
                    (list :run-id (plist-get entry :id)))))))))

(defun pai-isub--tool-read (args on-done)
  "Handle the read action with ARGS, completing through ON-DONE."
  (let* ((entry (pai-isub--require-run args))
         (backend (pai-isub-entry-backend entry))
         (text (and backend
                    (pai-isub-backend-transcript backend (plist-get entry :handle)
                                                 (plist-get args :max-lines)))))
    (funcall on-done
             (pai-isub--ok
              (or text
                  (plist-get entry :last-output)
                  (format "Subagent %s has produced no output yet."
                          (plist-get entry :id)))))))

(defun pai-isub--tool-execute (args ctx _on-update on-done)
  "Execute the `subagent' tool for ARGS in CTX, completing through ON-DONE."
  (condition-case err
      (pcase (or (plist-get args :action) "launch")
        ("status" (funcall on-done (pai-isub--ok
                                    (pai-isub-status-text (plist-get args :id)))))
        ("list" (funcall on-done (pai-isub--ok (pai-isub--roles-text))))
        ("say" (pai-isub--tool-say args on-done))
        ("read" (pai-isub--tool-read args on-done))
        ("stop"
         (let ((entry (pai-isub--require-run args)))
           (pai-isub-stop entry)
           (funcall on-done (pai-isub--ok
                             (format "Interrupted subagent %s; the session is still open."
                                     (plist-get entry :id))))))
        ("close"
         (let ((entry (pai-isub--require-run args)))
           (pai-isub-close entry)
           (funcall on-done (pai-isub--ok
                             (format "Closed subagent %s." (plist-get entry :id))))))
        (_ (pai-isub--tool-launch args ctx on-done)))
    (error (funcall on-done (pai-tool-error-result (error-message-string err))))))

(defvar pai-isub-tool
  (list :name "subagent"
        :label "Subagent"
        :description
        "Open and talk to a child agent session running in its own buffer next to yours. Roles: scout (fast codebase recon), researcher (research with sources), evidence-auditor (claim checking), worker (implementation), reviewer (code review), oracle (second opinion), delegate (general), plus any user-defined role. A session stays alive between turns: launch it once, then keep using action \"say\" with its id. The human user can read and type into that session too, and the subagent can message you back at any time. Launches never block: the answer is delivered to you as a follow-up turn unless you pass async false."
        :prompt-snippet "subagent: open/talk to a child agent session in its own buffer"
        :execution-mode 'parallel
        :parameters
        (pai-object-schema
         (list :agent (pai-string-schema "Role name for launch (scout, worker, reviewer, oracle, or a user-defined role).")
               :task (pai-string-schema "The task (launch) or the message to send (say).")
               :id (pai-string-schema "Subagent id for say/read/stop/close/status (prefix ok; defaults to the newest live session).")
               :action (pai-string-schema "launch (default), say, read, status, stop, close, or list.")
               :model (pai-string-schema "Model override; omit it to use the role's configured model. For the pai backend a pai model \"provider/id[:thinking]\"; for agent-shell roles the harness's own model id (e.g. \"opus\" or \"opus:high\"), never a pai model key.")
               :backend (pai-string-schema "Session backend to run the role in: \"pai\" or \"agent-shell\" (default: the one configured for the role).")
               :harness (pai-string-schema "agent-shell harness (external agent) to run, e.g. \"claude-code\", \"gemini-cli\", \"codex\"; implies backend agent-shell.")
               :async (pai-boolean-schema "Run in background (default true). false keeps the tool call pending until the subagent finishes this turn.")
               :context (pai-string-schema "\"fork\" starts the session with your conversation so far; \"fresh\" starts clean.")
               :max-lines (pai-number-schema "Max output lines returned to you.")
               :timeout (pai-number-schema "Max seconds to wait for the turn you just asked for."))
         '("agent"))
        :execute #'pai-isub--tool-execute)
  "The `subagent' tool as this extension defines it.
Kept in a variable so it can be inspected and tested independently of the
instance registry: the older `pai-subagents' extension registers a tool of
the same name, so enable one or the other, not both.")

(pai-register-tool pai-isub-tool)

;;;; Slash commands

(defun pai-isub--run-ids ()
  "Return the ids of this buffer's subagent sessions."
  (mapcar (lambda (e) (plist-get e :id)) pai-isub--runs))

(defun pai-isub-subagents-command (args _ctx)
  "Handler for `/subagents [id]': list this session's subagents."
  (list :message (pai-isub-status-text (string-trim args))))

(defun pai-isub-open-command (args _ctx)
  "Handler for `/subagents-open [id]': show a subagent's buffer."
  (let* ((entry (pai-isub-run-by-prefix (string-trim args)))
         (buffer (and entry (pai-isub-entry-buffer entry))))
    (if (buffer-live-p buffer)
        (progn (pai-isub-display buffer)
               (list :message (format "Showing %s" (buffer-name buffer))))
      (list :message (format "No subagent buffer for %s"
                             (if (string-empty-p (string-trim args))
                                 "(newest)" (string-trim args)))))))

(defun pai-isub-stop-command (args _ctx)
  "Handler for `/subagents-stop [id]': interrupt a subagent's turn."
  (let ((entry (pai-isub-run-by-prefix (string-trim args))))
    (if (and entry (pai-isub-stop entry))
        (list :message (format "Interrupted %s" (plist-get entry :id)))
      (list :message "No matching subagent"))))

(defun pai-isub-close-command (args _ctx)
  "Handler for `/subagents-close [id]': end a subagent session."
  (let ((entry (pai-isub-run-by-prefix (string-trim args))))
    (if (and entry (pai-isub-close entry))
        (list :message (format "Closed %s" (plist-get entry :id)))
      (list :message "No matching subagent"))))

(defun pai-isub--role-runs-in-text (role &optional parent)
  "Return \"MODEL BACKEND[ :THINKING]\" describing what ROLE runs on.
PARENT is the parent session's model, for \"inherit\"."
  (let ((backend (pai-isub-resolve-backend role)))
    (if (pai-isub-backend-own-model-p backend)
        (let ((h (pai-isub-resolve-harness role)))
          (format "%-28s %s%s"
                  (format "%s/%s" (or (plist-get h :harness) "(no harness)")
                          (or (plist-get h :model) "default"))
                  backend
                  (if (plist-get h :thinking) (format " :%s" (plist-get h :thinking)) "")))
      (let ((resolved (pai-isub-resolve-model role nil parent)))
        (format "%-28s %s%s"
                (if resolved (pai-model-key (car resolved)) "(unresolved)")
                backend
                (if (and resolved (cadr resolved)) (format " :%s" (cadr resolved)) ""))))))

(defun pai-isub-models-command (args _ctx)
  "Handler for `/subagents-models [role]': show the live role-model mapping.
agent-shell roles show HARNESS/MODEL instead of a pai model."
  (let* ((role (string-trim args))
         (parent (and (boundp 'pai--model) pai--model))
         (names (if (string-empty-p role) (pai-isub-role-names) (list role))))
    (list :message
          (mapconcat (lambda (name)
                       (format "%-16s %s" name (pai-isub--role-runs-in-text name parent)))
                     names "\n"))))

(defun pai-isub--read-role (args)
  "Return the role named in ARGS, or read one."
  (let ((role (string-trim (or args ""))))
    (if (string-empty-p role)
        (completing-read "Role: " (pai-isub-role-names) nil t)
      role)))

(defun pai-isub--harness-message (role)
  "Return a message saying which harness and model ROLE runs."
  (let ((h (pai-isub-resolve-harness role)))
    (format "subagent %s runs in agent-shell harness %s, model %s" role
            (or (plist-get h :harness) "(none: set a default)")
            (or (plist-get h :model) "default"))))

(defun pai-isub-read-harness-for-role (role)
  "Read a harness and its model for ROLE; run ROLE there."
  (let* ((old (pai-isub-resolve-harness role))
         (harness (pai-isub-agent-shell-read-harness
                   (plist-get (pai-isub-override role) :harness))))
    (pai-isub-set-override role :backend pai-isub-agent-shell-name)
    (pai-isub-set-override role :harness harness)
    (pai-isub-set-override role :harness-model nil)
    (let* ((now (pai-isub-resolve-harness role))
           (effective (plist-get now :harness)))
      (when effective
        (pai-isub-set-override
         role :harness-model
         (pai-isub-agent-shell-read-model
          effective (and (equal effective (plist-get old :harness))
                         (plist-get old :model))))))))

(defun pai-isub-set-model-command (args _ctx)
  "Handler for `/subagents-model [role]': set the model used for a role.
A role in the agent-shell backend picks among its harness's models."
  (let* ((role (pai-isub--read-role args))
         (backend (pai-isub-resolve-backend role)))
    (if (pai-isub-backend-own-model-p backend)
        (let* ((h (pai-isub-resolve-harness role))
               (harness (or (plist-get h :harness)
                            (user-error "Pick a harness for %s first: /subagents-harness %s"
                                        role role))))
          (pai-isub-set-override role :harness-model
                                 (pai-isub-agent-shell-read-model harness (plist-get h :model)))
          (list :message (pai-isub--harness-message role)))
      (let ((spec (completing-read
                   (format "Model for %s (provider/id[:thinking], RET = inherit): " role)
                   (cons "inherit" (pai-model-keys))
                   nil nil nil nil "inherit")))
        (pai-isub-set-override role :model spec)
        (list :message (format "subagent %s -> %s" role spec))))))

(defun pai-isub-set-backend-command (args _ctx)
  "Handler for `/subagents-backend [role]': set the backend used for a role.
Choosing agent-shell goes on to ask for the harness and its model."
  (let* ((role (pai-isub--read-role args))
         (backend (completing-read (format "Backend for %s: " role)
                                   (cons "inherit" (pai-isub-backend-names))
                                   nil t)))
    (pai-isub-set-override role :backend backend)
    (if (pai-isub-backend-own-model-p (pai-isub-resolve-backend role))
        (progn (when (equal backend pai-isub-agent-shell-name)
                 (pai-isub-read-harness-for-role role))
               (list :message (pai-isub--harness-message role)))
      (list :message (format "subagent %s runs in backend %s" role
                             (pai-isub-resolve-backend role))))))

(defun pai-isub-harness-command (args _ctx)
  "Handler for `/subagents-harness [role]': run a role in an agent-shell harness.
Asks for the harness (Claude Code, Gemini CLI, ...) and which of its models
to use; the role then runs in the agent-shell backend."
  (let ((role (pai-isub--read-role args)))
    (pai-isub-agent-shell--require)
    (pai-isub-read-harness-for-role role)
    (list :message (pai-isub--harness-message role))))

(defun pai-isub-roles-command (_args _ctx)
  "Handler for `/subagents-roles': list roles with descriptions."
  (list :message (pai-isub--roles-text t)))

(defun pai-isub-reload-command (_args _ctx)
  "Handler for `/subagents-reload': re-read role files into this instance."
  (let ((n (pai-isub-load-roles (when (and (boundp 'pai--trusted) pai--trusted)
                                  default-directory))))
    (list :message (format "Loaded %d user role(s)%s" n
                           (if pai-isub--roles
                               (format ": %s"
                                       (string-join (mapcar #'car pai-isub--roles) ", "))
                             "")))))

;;;; Settings screen

(defun pai-isub--settings-role-items ()
  "Return one settings row per role: model, thinking level, and backend.
A role in the agent-shell backend shows its harness and the harness's model
instead of a pai model."
  (mapcar
   (lambda (role)
     (list
      :key (intern (format ":isub-role-%s" role)) :type 'custom
      :render
      (lambda (refresh)
        (let* ((target (current-buffer))
               (desc (plist-get (pai-isub-role role) :description))
               (own (pai-isub-backend-own-model-p (pai-isub-role-backend-display role)))
               (harness (and own (pai-isub-resolve-harness role))))
          (cl-flet ((write (key value)
                      (when (buffer-live-p target)
                        (with-current-buffer target
                          (pai-isub-set-override role key value)))
                      (funcall refresh)))
            (apply
             #'vui-hstack
             (delq nil
                   (list
                    (vui-box (vui-text role) :width 18 :align :left)
                    (vui-box (vui-muted (pai-isub-role-source role)) :width 16 :align :left)
                    (if own
                        (pai-isub--settings-harness-selects
                         harness
                         (lambda (v)
                           (when (buffer-live-p target)
                             (with-current-buffer target
                               (pai-isub-set-override role :harness v)
                               ;; another harness has other models
                               (pai-isub-set-override role :harness-model nil)))
                           (funcall refresh))
                         (lambda (v) (write :harness-model v)))
                      (vui-select
                       :value (pai-isub-role-model-display role)
                       :options (cons "inherit" (pai-model-keys))
                       :on-change (lambda (v) (write :model v))))
                    (vui-text "think:")
                    (vui-select
                     :value (pai-isub-role-thinking-display role)
                     :options (cons "inherit"
                                    (mapcar #'symbol-name pai-isub-thinking-levels))
                     :on-change (lambda (v) (write :thinking v)))
                    (vui-text "in:")
                    (vui-select
                     :value (pai-isub-role-backend-display role)
                     :options (cons "inherit" (pai-isub-backend-names))
                     :on-change (lambda (v) (write :backend v)))
                    (when desc
                      (vui-muted
                       (concat " " (truncate-string-to-width desc 24 nil nil "…"))))))))))))
   (pai-isub-role-names)))

(defun pai-isub--other-model (harness)
  "Read a model id for HARNESS typed by hand; nil when empty."
  (pai-isub--nonempty
   (string-trim (read-string (format "%s model id (empty = its default): " harness)))))

(defun pai-isub--harness-model-options (harness)
  "Return select options for HARNESS's models: default, known ones, other."
  (append '(("default" . "default"))
          (mapcar (lambda (m)
                    (cons (car m)
                          (if (string-empty-p (or (cdr m) ""))
                              (car m)
                            (format "%s (%s)" (car m) (cdr m)))))
                  (and harness (pai-isub-agent-shell-models harness)))
          '((other . "other…"))))

(defun pai-isub--settings-harness-selects (resolved on-harness on-model)
  "Return \"harness: [H] model: [M]\" selects for a RESOLVED harness plist.
ON-HARNESS and ON-MODEL receive the new harness / model id (nil = inherit
/ default)."
  (let ((harness (plist-get resolved :harness))
        (model (plist-get resolved :model)))
    (vui-hstack
     (vui-text "harness:")
     (vui-select
      :value (or harness "inherit")
      :options (cons "inherit" (pai-isub-agent-shell-agent-names))
      :on-change (lambda (v) (funcall on-harness (pai-isub--nonempty v))))
     (vui-text "model:")
     (vui-select
      :value (or model "default")
      :options (let ((opts (pai-isub--harness-model-options harness)))
                 (if (or (null model) (assoc model opts))
                     opts
                   (append opts (list (cons model model)))))
      :on-change (lambda (v)
                   (funcall on-model
                            (cond ((eq v 'other) (and harness (pai-isub--other-model harness)))
                                  (t (pai-isub--nonempty v)))))))))

(defun pai-isub--settings-manage-items ()
  "Return dynamic action items for managing role definitions."
  (append
   (list (list :key :isub-roles-new :type 'action
               :label "New role…"
               :doc "Create a role definition file and open it"
               :action (lambda ()
                         (let ((name (read-string "New role name: ")))
                           (when (and name (not (string-empty-p name)))
                             (pai-isub-edit-role name)))))
         (list :key :isub-roles-edit :type 'action
               :label "Edit role…"
               :doc "Open (or create, overriding a builtin) a role file"
               :action (lambda ()
                         (let ((name (completing-read "Edit role: "
                                                      (pai-isub-role-names) nil nil)))
                           (when (and name (not (string-empty-p name)))
                             (pai-isub-edit-role name)))))
         (list :key :isub-roles-delete :type 'action
               :label "Delete role…"
               :doc "Remove a user role file / disable a builtin"
               :action (lambda ()
                         (let ((name (completing-read "Delete role: "
                                                      (pai-isub-role-names) nil t)))
                           (when (and name (not (string-empty-p name))
                                      (yes-or-no-p (format "Delete role %s? " name)))
                             (pai-isub-delete-role name))))))
   (mapcar (lambda (role)
             (list :key (intern (format ":isub-role-enable-%s" role)) :type 'action
                   :label (format "Re-enable %s" role)
                   :doc "Restore this disabled role"
                   :action (lambda () (pai-isub-set-role-disabled role nil))))
           (pai-isub-disabled-roles))))

;;;; Extension entry point

(pai-register-extension
 (lambda (api)
   ;; Child-side wiring: these handlers are no-ops in a buffer that is not a
   ;; subagent session, and do the reporting back home in one that is.
   (pai-ext-on api 'context #'pai-isub-session-context-hook)
   (pai-ext-on api 'agent-start #'pai-isub-session-agent-start-hook)
   (pai-ext-on api 'message-update #'pai-isub-session-message-update-hook)
   (pai-ext-on api 'message-end #'pai-isub-session-message-end-hook)
   (pai-ext-on api 'agent-end #'pai-isub-session-agent-end-hook)

   (pai-ext-register-command api "subagents"
                             :description "List this session's subagents (arg: id prefix)"
                             :handler #'pai-isub-subagents-command
                             :arg-completions (lambda (_p) (pai-isub--run-ids)))
   (pai-ext-register-command api "subagents-open"
                             :description "Show a subagent's buffer"
                             :handler #'pai-isub-open-command
                             :arg-completions (lambda (_p) (pai-isub--run-ids)))
   (pai-ext-register-command api "subagents-stop"
                             :description "Interrupt a subagent's current turn"
                             :handler #'pai-isub-stop-command
                             :arg-completions (lambda (_p) (pai-isub--run-ids)))
   (pai-ext-register-command api "subagents-close"
                             :description "Close a subagent session"
                             :handler #'pai-isub-close-command
                             :arg-completions (lambda (_p) (pai-isub--run-ids)))
   (pai-ext-register-command api "subagents-models"
                             :description "Show role-to-model/backend mapping (arg: role)"
                             :handler #'pai-isub-models-command
                             :arg-completions (lambda (_p) (pai-isub-role-names)))
   (pai-ext-register-command api "subagents-model"
                             :description "Set the model for a role"
                             :handler #'pai-isub-set-model-command
                             :arg-completions (lambda (_p) (pai-isub-role-names)))
   (pai-ext-register-command api "subagents-backend"
                             :description "Set the session backend for a role"
                             :handler #'pai-isub-set-backend-command
                             :arg-completions (lambda (_p) (pai-isub-role-names)))
   (pai-ext-register-command api "subagents-harness"
                             :description "Run a role in an agent-shell harness (Claude Code, Gemini CLI, ...) and pick its model"
                             :handler #'pai-isub-harness-command
                             :arg-completions (lambda (_p) (pai-isub-role-names)))
   (pai-ext-register-command api "subagents-roles"
                             :description "List available subagent roles"
                             :handler #'pai-isub-roles-command)
   (pai-ext-register-command api "subagents-reload"
                             :description "Reload subagent role definitions"
                             :handler #'pai-isub-reload-command)
   (pai-ext-register-command api "parent"
                             :description "Message the agent that spawned this session"
                             :handler #'pai-isub-session-parent-command)

   ;; Load user roles for this instance (project roles only when trusted).
   (pai-isub-load-roles (when (and (boundp 'pai--trusted) pai--trusted)
                          default-directory)))
 "interactive-subagents")

;;;; Settings screen registration
;; Soft dependency: the extension works without the vui settings screen.
(with-eval-after-load 'pai-settings-ui
  (pai-settings-ui-register-section 'interactive-subagents "Interactive subagents" 50)
  (pai-settings-ui-register-subsection 'interactive-subagents 'defaults "Defaults" 10)
  (pai-settings-ui-register-item
   'interactive-subagents 'defaults
   :key :isub-default-model :type 'choice :label "Default model"
   :doc "Model for roles without an explicit model (\"inherit\" = parent)"
   :choices (lambda () (cons "inherit" (pai-model-keys)))
   :get (lambda () (or (plist-get (pai-isub-config) :default-model) "inherit"))
   :set (lambda (v) (pai-isub-config-set :default-model v)))
  (pai-settings-ui-register-item
   'interactive-subagents 'defaults
   :key :isub-default-thinking :type 'choice :label "Default thinking"
   :doc "Reasoning level applied to subagent sessions by default"
   :choices (lambda () (cons "inherit" (mapcar #'symbol-name pai-isub-thinking-levels)))
   :get (lambda () (or (plist-get (pai-isub-config) :default-thinking) "inherit"))
   :set (lambda (v) (pai-isub-config-set :default-thinking v)))
  (pai-settings-ui-register-item
   'interactive-subagents 'defaults
   :key :isub-default-backend :type 'choice :label "Default backend"
   :doc "Which kind of session a subagent runs in"
   :choices (lambda () (pai-isub-backend-names))
   :get (lambda () (or (plist-get (pai-isub-config) :default-backend) "pai"))
   :set (lambda (v) (pai-isub-config-set :default-backend v)))
  (pai-settings-ui-register-item
   'interactive-subagents 'defaults
   :key :isub-report-all :type 'boolean :label "Report every turn"
   :doc "Also deliver turns the user drove in a subagent buffer to the parent"
   :get (lambda () (pai-truthy (plist-get (pai-isub-config) :report-all-turns)))
   :set (lambda (v) (pai-isub-config-set :report-all-turns (if v t :false))))
  (pai-settings-ui-register-item
   'interactive-subagents 'defaults
   :key :isub-idle-close :type 'number :label "Close idle after (s)"
   :doc "Close a finished subagent when its buffer gets no input for this long (0 keeps it open)"
   :get (lambda () (or (pai-isub-idle-close-seconds) 0))
   :set (lambda (v) (pai-isub-config-set :idle-close-seconds
                                         (if (numberp v) v (string-to-number (format "%s" v))))))
  (pai-settings-ui-register-item
   'interactive-subagents 'defaults
   :key :isub-allow-nested :type 'boolean :label "Allow nested subagents"
   :doc "Let subagent sessions keep the subagent tool themselves"
   :get (lambda () (pai-isub-nested-allowed-p))
   :set (lambda (v) (pai-isub-config-set :allow-nested (if v t :false))))
  (pai-settings-ui-register-item
   'interactive-subagents 'defaults
   :key :isub-agent-shell-agent :type 'choice :label "agent-shell harness"
   :doc "Harness (external agent) for agent-shell roles that name none (inherit = agent-shell's preferred)"
   :choices (lambda () (cons "inherit" (pai-isub-agent-shell-agent-names)))
   :get (lambda () (or (plist-get (pai-isub-config) :agent-shell-agent) "inherit"))
   :set (lambda (v)
          (unless (equal (pai-isub--nonempty v)
                         (pai-isub--nonempty (plist-get (pai-isub-config) :agent-shell-agent)))
            ;; another harness has other models
            (pai-isub-config-set :agent-shell-model nil))
          (pai-isub-config-set :agent-shell-agent (pai-isub--nonempty v))))
  (pai-settings-ui-register-item
   'interactive-subagents 'defaults
   :key :isub-agent-shell-model :type 'choice :label "agent-shell model"
   :doc "Model of the default harness (models are listed once a session of it has run)"
   :choices (lambda ()
              (let ((current (plist-get (pai-isub-config) :agent-shell-model)))
                (delete-dups
                 (append (list "default")
                         (mapcar #'car (and (pai-isub-default-harness)
                                            (pai-isub-agent-shell-models
                                             (pai-isub-default-harness))))
                         (and current (list current))
                         (list "other…")))))
   :get (lambda () (or (plist-get (pai-isub-config) :agent-shell-model) "default"))
   :set (lambda (v)
          (pai-isub-config-set
           :agent-shell-model
           (if (equal v "other…")
               (let ((h (pai-isub-default-harness)))
                 (and h (pai-isub--other-model h)))
             (pai-isub--nonempty v)))))
  (pai-settings-ui-register-item
   'interactive-subagents 'defaults
   :key :isub-agent-shell-auto-approve :type 'boolean :label "agent-shell auto-approve"
   :doc "Approve agent-shell subagents' tool permission requests (within the role's tools) instead of asking"
   :get (lambda () (pai-truthy (plist-get (pai-isub-config) :agent-shell-auto-approve)))
   :set (lambda (v) (pai-isub-config-set :agent-shell-auto-approve (if v t :false))))
  (pai-settings-ui-register-subsection
   'interactive-subagents 'roles "Roles (model or harness · thinking · backend)" 20)
  (pai-settings-ui-register-dynamic-items
   'interactive-subagents 'roles #'pai-isub--settings-role-items)
  (pai-settings-ui-register-subsection 'interactive-subagents 'manage "Manage roles" 30)
  (pai-settings-ui-register-dynamic-items
   'interactive-subagents 'manage #'pai-isub--settings-manage-items))

(provide 'pai-interactive-subagents)
;;; pai-interactive-subagents.el ends here
