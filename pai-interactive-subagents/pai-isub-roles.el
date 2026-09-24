;;; pai-isub-roles.el --- Roles and resolution for interactive subagents -*- lexical-binding: t; -*-

;;; Commentary:

;; Roles describe *what* a subagent is: a name, a description, a system
;; prompt, and optional defaults (model, thinking level, backend, tool
;; allowlist, context mode).  Builtin roles ship here; user roles are markdown
;; files with frontmatter under ~/.pai/subagents/*.md and, in trusted
;; projects, <project>/.pai/subagents/*.md (the same files the original
;; subagents extension used).
;;
;; Resolution order for the model that fills a role, strongest first:
;;   1. per-run tool argument:   model "provider/id[:thinking]"
;;   2. settings override:       :overrides (:reviewer (:model M :thinking L))
;;   3. role frontmatter:        model: provider/id
;;   4. settings default:        :default-model
;;   5. the parent session's model ("inherit")
;;
;; Backends resolve the same way (per-run arg, override, frontmatter,
;; :default-backend, then "pai").
;;
;; Settings live under the `:interactive-subagents' key.  Overrides are stored
;; as a JSON object keyed by role name so they survive a settings round trip:
;;
;;   :interactive-subagents (:default-model "inherit" :default-thinking "medium"
;;                           :default-backend "pai" :disabled-roles ("oracle")
;;                           :overrides (:reviewer (:model "x/y" :thinking "high")))

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai)
(require 'pai-models)
(require 'pai-settings)
(require 'pai-skills)
(require 'pai-isub-backend)

(defconst pai-isub-settings-key :interactive-subagents
  "Settings key holding this extension's configuration plist.")

(defconst pai-isub-thinking-levels
  '(off minimal low medium high xhigh max)
  "Supported thinking levels, least to most.")

(defconst pai-isub--builtins
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

(defvar-local pai-isub--roles nil
  "User-defined roles loaded from disk for this instance: NAME -> plist.")

;;;; Settings

(defun pai-isub-config ()
  "Return this instance's configuration plist."
  (or (pai-settings-get pai-isub-settings-key) '()))

(defun pai-isub-config-set (key value)
  "Set KEY to VALUE in the configuration plist and persist it (project scope)."
  (pai-settings-set pai-isub-settings-key
                    (plist-put (copy-sequence (pai-isub-config)) key value)
                    'project)
  value)

(defun pai-isub--as-symbol (value)
  "Return VALUE as a symbol, or nil for nil/empty/\"inherit\"."
  (cond
   ((null value) nil)
   ((symbolp value) (unless (memq value '(inherit off-inherit)) value))
   ((stringp value) (unless (member value '("" "inherit")) (intern value)))
   (t nil)))

(defun pai-isub--role-key (role)
  "Return the settings key (a keyword) used to store ROLE's override."
  (intern (concat ":" role)))

(defun pai-isub-override (role)
  "Return the settings override plist for ROLE, or nil."
  (plist-get (plist-get (pai-isub-config) :overrides) (pai-isub--role-key role)))

(defun pai-isub-set-override (role key value)
  "Set KEY to VALUE in ROLE's override plist and persist it.
A VALUE of nil, \"\" or \"inherit\" clears the entry so ROLE falls through to
its frontmatter and then the configured defaults.  Return VALUE."
  (let* ((config (copy-sequence (pai-isub-config)))
         (overrides (copy-sequence (plist-get config :overrides)))
         (entry (copy-sequence (pai-isub-override role)))
         (clear (or (null value) (member value '("" "inherit")))))
    (setq entry (plist-put (or entry '()) key (unless clear value)))
    (setq overrides (plist-put (or overrides '()) (pai-isub--role-key role) entry))
    (pai-isub-config-set :overrides overrides)
    value))

(defun pai-isub-disabled-roles ()
  "Return the list of role names disabled via settings."
  (plist-get (pai-isub-config) :disabled-roles))

(defun pai-isub-set-role-disabled (role disabled)
  "Add or remove ROLE from the `:disabled-roles' setting per DISABLED."
  (let* ((current (pai-isub-disabled-roles))
         (new (if disabled (cons role (remove role current)) (remove role current))))
    (pai-isub-config-set :disabled-roles (delete-dups new))))

;;;; Role lookup

(defun pai-isub-roles ()
  "Return the role alist for this instance (user roles first).
Both user and builtin entries are (NAME . PLIST); disabled roles are omitted."
  (let ((disabled (pai-isub-disabled-roles)))
    (cl-remove-if
     (lambda (r) (member (car r) disabled))
     (append pai-isub--roles
             (cl-loop for (name . rest) in pai-isub--builtins
                      collect (cons name (car rest)))))))

(defun pai-isub-role (name)
  "Return the role plist for NAME, or nil."
  (cdr (assoc name (pai-isub-roles))))

(defun pai-isub-role-names ()
  "Return sorted role names."
  (sort (mapcar #'car (pai-isub-roles)) #'string-lessp))

;;;; Role files

(defun pai-isub--parse-role-file (file)
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
                 (when (cdr (assoc "backend" meta))
                   (list :backend (cdr (assoc "backend" meta))))
                 (when (cdr (assoc "thinking" meta))
                   (list :thinking (intern (cdr (assoc "thinking" meta)))))
                 (when (cdr (assoc "tools" meta))
                   (list :tools (split-string (cdr (assoc "tools" meta)) "[, ]+" t)))
                 (when (cdr (assoc "context" meta))
                   (list :context (intern (cdr (assoc "context" meta)))))))))
    (error (message "pai-isub: bad role file %s: %s" file
                    (error-message-string err))
           nil)))

(defun pai-isub-roles-dir ()
  "Return the global roles directory, creating it on demand."
  (let ((dir (expand-file-name "subagents" pai-directory)))
    (make-directory dir t)
    dir))

(defun pai-isub-load-roles (&optional project-dir)
  "Load user roles into this instance from the home dir, then PROJECT-DIR.
Project roles override home roles with the same name.  Return their count."
  (let ((roles pai-isub--roles)
        (dirs (list (expand-file-name "subagents" pai-directory))))
    (when project-dir
      (setq dirs (append dirs (list (expand-file-name ".pai/subagents" project-dir)))))
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.md\\'"))
          (let ((role (pai-isub--parse-role-file file)))
            (when role
              (setq roles (cons role (cl-remove (car role) roles
                                                :key #'car :test #'equal))))))))
    (setq pai-isub--roles roles)
    (length roles)))

(defun pai-isub-reload-roles ()
  "Reload role files for the current instance (project roles only when trusted)."
  (setq pai-isub--roles nil)
  (pai-isub-load-roles (when (and (boundp 'pai--trusted) pai--trusted)
                         default-directory)))

(defun pai-isub--role-file (role)
  "Return an existing role file for ROLE (project first, then global), or nil."
  (let ((candidates
         (list (and (boundp 'pai--trusted) pai--trusted
                    (expand-file-name (format ".pai/subagents/%s.md" role)
                                      default-directory))
               (expand-file-name (format "%s.md" role)
                                 (expand-file-name "subagents" pai-directory)))))
    (seq-find (lambda (f) (and f (file-readable-p f))) candidates)))

(defun pai-isub--role-template (role)
  "Return markdown template text for a new ROLE, seeded from a builtin if any."
  (let* ((builtin (car (cdr (assoc role pai-isub--builtins))))
         (desc (or (plist-get builtin :description) "One-line description of the role."))
         (thinking (or (plist-get builtin :thinking) 'medium))
         (prompt (or (plist-get builtin :prompt)
                     "You are ROLE. Describe the role's job and constraints here.")))
    (format (concat "---\n"
                    "name: %s\n"
                    "description: %s\n"
                    "thinking: %s\n"
                    "# model: provider/id      # optional default model for this role\n"
                    "# backend: pai            # session backend to run this role in\n"
                    "# tools: bash, read       # optional allowlist\n"
                    "# context: fork           # fresh | fork\n"
                    "---\n\n%s\n")
            role desc thinking prompt)))

(defun pai-isub-edit-role (role)
  "Open ROLE's definition file for editing, creating it from a template first.
Creating a file for a builtin name overrides the builtin; the role is also
re-enabled if it was disabled.  Reloads roles when the buffer is saved."
  (let ((file (or (pai-isub--role-file role)
                  (let ((f (expand-file-name (format "%s.md" role)
                                             (pai-isub-roles-dir))))
                    (unless (file-exists-p f)
                      (with-temp-file f (insert (pai-isub--role-template role))))
                    f))))
    (pai-isub-set-role-disabled role nil)
    (find-file file)
    (add-hook 'after-save-hook #'pai-isub-reload-roles nil t)
    (message "Editing role %s; save to apply." role)))

(defun pai-isub-delete-role (role)
  "Delete ROLE: remove user role file(s) and disable a builtin of that name."
  (dolist (f (list (expand-file-name (format ".pai/subagents/%s.md" role)
                                     default-directory)
                   (expand-file-name (format "%s.md" role)
                                     (expand-file-name "subagents" pai-directory))))
    (when (and f (file-exists-p f)) (delete-file f)))
  (when (assoc role pai-isub--builtins)
    (pai-isub-set-role-disabled role t))
  (pai-isub-reload-roles)
  (message "Deleted role %s." role))

;;;; Model / thinking / backend resolution

(defun pai-isub-parse-model-spec (spec)
  "Split \"provider/id[:thinking]\" SPEC into (MODEL-KEY THINKING-SYMBOL)."
  (save-match-data
    (if (string-match
         "\\`\\(.+?\\):\\(off\\|minimal\\|low\\|medium\\|high\\|xhigh\\|max\\)\\'" spec)
        (list (match-string 1 spec) (intern (match-string 2 spec)))
      (list spec nil))))

(defun pai-isub-resolve-model (role &optional per-run parent-model)
  "Resolve the model plist and thinking level for ROLE.
PER-RUN wins, then the settings override, then role frontmatter, then
`:default-model', then PARENT-MODEL.  Return (MODEL-PLIST THINKING) or nil."
  (let* ((override (pai-isub-override role))
         (role-def (pai-isub-role role))
         (spec (or per-run
                   (plist-get override :model)
                   (plist-get role-def :model)
                   (plist-get (pai-isub-config) :default-model)
                   (and parent-model "inherit")))
         (parsed (and spec (pai-isub-parse-model-spec spec)))
         (key (car parsed))
         (thinking (or (cadr parsed)
                       (pai-isub--as-symbol (plist-get override :thinking))
                       (pai-isub--as-symbol (plist-get role-def :thinking))
                       (pai-isub--as-symbol
                        (plist-get (pai-isub-config) :default-thinking))))
         (model (pcase key
                  ((or "inherit" `nil "") parent-model)
                  (_ (or (pai-model key)
                         (and parent-model
                              (equal key (pai-model-key parent-model))
                              parent-model))))))
    (when (or (memq thinking pai-isub-thinking-levels) (null thinking))
      (and model (list model thinking)))))

(defun pai-isub-resolve-backend (role &optional per-run)
  "Return the backend name to run ROLE in.
PER-RUN wins, then the settings override, then role frontmatter, then
`:default-backend', then \"pai\"."
  (or per-run
      (plist-get (pai-isub-override role) :backend)
      (plist-get (pai-isub-role role) :backend)
      (plist-get (pai-isub-config) :default-backend)
      "pai"))

(defun pai-isub-role-context-mode (role &optional per-run)
  "Return the context mode symbol (`fresh' or `fork') for ROLE.
PER-RUN, a string or symbol, overrides the role's frontmatter."
  (or (cond ((stringp per-run) (intern per-run))
            ((symbolp per-run) per-run))
      (plist-get (pai-isub-role role) :context)
      'fresh))

(defun pai-isub-role-tools (role)
  "Return ROLE's tool allowlist (override first, then frontmatter), or nil."
  (or (plist-get (pai-isub-override role) :tools)
      (plist-get (pai-isub-role role) :tools)))

(defun pai-isub-nested-allowed-p ()
  "Return non-nil when subagents may themselves spawn subagents.
The `:allow-nested' setting wins; otherwise `pai-isub-allow-nested' applies."
  (let ((config (pai-isub-config)))
    (if (plist-member config :allow-nested)
        (pai-truthy (plist-get config :allow-nested))
      (bound-and-true-p pai-isub-allow-nested))))

;;;; Display helpers (settings screen)

(defun pai-isub-role-model-display (role)
  "Return the effective model spec to show for ROLE."
  (or (plist-get (pai-isub-override role) :model)
      (plist-get (pai-isub-role role) :model)
      (plist-get (pai-isub-config) :default-model)
      "inherit"))

(defun pai-isub-role-thinking-display (role)
  "Return the effective thinking level to show for ROLE."
  (let ((v (or (pai-isub--as-symbol (plist-get (pai-isub-override role) :thinking))
               (pai-isub--as-symbol (plist-get (pai-isub-role role) :thinking))
               (pai-isub--as-symbol (plist-get (pai-isub-config) :default-thinking)))))
    (if v (symbol-name v) "inherit")))

(defun pai-isub-role-backend-display (role)
  "Return the effective backend name to show for ROLE."
  (pai-isub-resolve-backend role))

(provide 'pai-isub-roles)
;;; pai-isub-roles.el ends here
