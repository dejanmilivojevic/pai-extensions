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
;; `pai-isub-backend'): "pai" ships here, and another -- an `agent-shell'
;; buffer, say -- can be registered without touching anything else.

;;; Commentary:

;; Entry point: wires roles (`pai-isub-roles'), the backend protocol
;; (`pai-isub-backend'), the built-in pai backend (`pai-isub-session') and the
;; run bookkeeping (`pai-isub-runs') into the `subagent' tool, the
;; `/subagents*' slash commands, the child-side `/parent' command, and the
;; settings screen.
;;
;; The `subagent' tool: {agent, task, id?, action?, model?, backend?, async?,
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
    (format "Opened subagent %s (role %s, backend %s, model %s)%s. It is running your task now; its answer will be delivered here when the turn ends. Continue the conversation with subagent {\"action\":\"say\",\"id\":\"%s\",\"task\":\"…\"}; inspect it with action \"read\"."
            (plist-get entry :id) (plist-get entry :role)
            (plist-get entry :backend) (or (plist-get entry :model) "-")
            (if (buffer-live-p buffer) (format " in buffer %s" (buffer-name buffer)) "")
            (plist-get entry :id))))

(defun pai-isub--roles-text ()
  "Return the role list as NAME: DESCRIPTION lines."
  (string-join
   (mapcar (lambda (pair)
             (format "%s: %s" (car pair) (plist-get (cdr pair) :description)))
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
               :model (pai-string-schema "Model override \"provider/id\" or \"provider/id:thinking\".")
               :backend (pai-string-schema "Session backend to run the role in (default \"pai\").")
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

(defun pai-isub-models-command (args _ctx)
  "Handler for `/subagents-models [role]': show the live role-model mapping."
  (let* ((role (string-trim args))
         (parent (and (boundp 'pai--model) pai--model))
         (names (if (string-empty-p role) (pai-isub-role-names) (list role))))
    (list :message
          (string-join
           (mapcar (lambda (name)
                     (let ((resolved (pai-isub-resolve-model name nil parent)))
                       (format "%-16s %-28s %s" name
                               (if resolved (pai-model-key (car resolved)) "(unresolved)")
                               (format "%s%s"
                                       (pai-isub-resolve-backend name)
                                       (if (and resolved (cadr resolved))
                                           (format " :%s" (cadr resolved)) "")))))
                   names)
           "\n"))))

(defun pai-isub-set-model-command (args _ctx)
  "Handler for `/subagents-model [role]': set the model used for a role."
  (let* ((role (string-trim args))
         (role (if (string-empty-p role)
                   (completing-read "Role: " (pai-isub-role-names) nil t)
                 role))
         (spec (completing-read
                (format "Model for %s (provider/id[:thinking], RET = inherit): " role)
                (cons "inherit" (pai-model-keys))
                nil nil nil nil "inherit")))
    (pai-isub-set-override role :model spec)
    (list :message (format "subagent %s -> %s" role spec))))

(defun pai-isub-set-backend-command (args _ctx)
  "Handler for `/subagents-backend [role]': set the backend used for a role."
  (let* ((role (string-trim args))
         (role (if (string-empty-p role)
                   (completing-read "Role: " (pai-isub-role-names) nil t)
                 role))
         (backend (completing-read (format "Backend for %s: " role)
                                   (cons "inherit" (pai-isub-backend-names))
                                   nil t)))
    (pai-isub-set-override role :backend backend)
    (list :message (format "subagent %s runs in backend %s" role
                           (pai-isub-resolve-backend role)))))

(defun pai-isub-roles-command (_args _ctx)
  "Handler for `/subagents-roles': list roles with descriptions."
  (list :message (pai-isub--roles-text)))

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
  "Return one settings row per role: model, thinking level, and backend."
  (mapcar
   (lambda (role)
     (list
      :key (intern (format ":isub-role-%s" role)) :type 'custom
      :render
      (lambda (refresh)
        (let ((target (current-buffer))
              (desc (plist-get (pai-isub-role role) :description)))
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
                    (vui-select
                     :value (pai-isub-role-model-display role)
                     :options (cons "inherit" (pai-model-keys))
                     :on-change (lambda (v) (write :model v)))
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
   :key :isub-allow-nested :type 'boolean :label "Allow nested subagents"
   :doc "Let subagent sessions keep the subagent tool themselves"
   :get (lambda () (pai-isub-nested-allowed-p))
   :set (lambda (v) (pai-isub-config-set :allow-nested (if v t :false))))
  (pai-settings-ui-register-subsection
   'interactive-subagents 'roles "Roles (model · thinking · backend)" 20)
  (pai-settings-ui-register-dynamic-items
   'interactive-subagents 'roles #'pai-isub--settings-role-items)
  (pai-settings-ui-register-subsection 'interactive-subagents 'manage "Manage roles" 30)
  (pai-settings-ui-register-dynamic-items
   'interactive-subagents 'manage #'pai-isub--settings-manage-items))

(provide 'pai-interactive-subagents)
;;; pai-interactive-subagents.el ends here
