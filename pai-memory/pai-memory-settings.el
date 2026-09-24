;;; pai-memory-settings.el --- Settings, presets and overrides for pai-memory -*- lexical-binding: t; -*-

;;; Commentary:

;; Settings live under the `:memory' key (SPEC §9.2):
;;
;;   (:memory (:preset balanced
;;             :budget (:session-usd 1.0 :daily-usd 5.0 ...)
;;             :session (:enabled t :observe continuous :chunk-tokens 8000 ...)
;;             :search (:enabled t :max-mb 500 ...)
;;             :long-term (:enabled t :promote (consolidation session-end) ...)))
;;
;; Resolution of one knob, first match wins:
;;   1. the session's own override (`memory.state' entries: /memory session
;;      on|off, /memory preset NAME), for the keys that have one;
;;   2. for knobs a preset controls, the effective preset's value -- unless the
;;      preset is `custom';
;;   3. the explicit settings value, project scope over global, merged one
;;      level deep so a project can override a single knob;
;;   4. the built-in default (the `balanced' preset's values).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-config)
(require 'pai-settings)
(require 'pai-session)

(defconst pai-memory-preset-names '(off economy balanced thorough custom)
  "Preset names, in display order.")

(defconst pai-memory-presets
  '((off      :session (:observe off :chunk-tokens 12000 :consolidate nil
                        :consolidate-at-pool-tokens 40000 :topic-merge nil)
              :long-term (:promote (manual)))
    (economy  :session (:observe near-compaction :chunk-tokens 12000 :consolidate t
                        :consolidate-at-pool-tokens 40000 :topic-merge nil)
              :long-term (:promote (session-end)))
    (balanced :session (:observe continuous :chunk-tokens 8000 :consolidate t
                        :consolidate-at-pool-tokens 20000 :topic-merge t)
              :long-term (:promote (consolidation session-end)))
    (thorough :session (:observe continuous :chunk-tokens 4000 :consolidate t
                        :consolidate-at-pool-tokens 15000 :topic-merge t)
              :long-term (:promote (consolidation session-end))))
  "Knob values set by each preset (SPEC §4.5).  `custom' sets nothing.")

(defconst pai-memory-defaults
  '(:preset balanced
    :budget (:session-usd 1.0 :daily-usd 5.0 :session-tokens 1000000 :daily-tokens 5000000)
    :session (:enabled t
              :observe continuous
              :observe-start-ratio 0.6
              :chunk-tokens 8000
              :observer-concurrency 3
              :observer-tool-result-chars 2000
              :observer-timeout 300
              :tail-tokens 20000
              :consolidate t
              :consolidator-timeout 600
              :pool-target-tokens 10000
              :consolidate-at-pool-tokens 20000
              :max-observation-tokens 30000
              :journey-target-tokens 1000
              :topic-merge t
              :topic-merger-timeout 600
              :reflect nil
              :reflect-every-tokens 40000
              :reflector-timeout 300)
    :search (:enabled t
             :max-mb 500
             :tool-result-chars 2000
             :recall nil
             :recall-hits 3
             :recall-chars 400
             :embedder nil
             :embed-url nil
             :embed-model nil
             :embed-key-env nil
             :embed-batch 32
             :embed-timeout 5
             :embed-daily-tokens 2000000
             :embed-scan 3000
             :embed-min-similarity 0.3)
    :long-term (:enabled t
                :promote (consolidation session-end)
                :promote-min-new-tokens 1500
                :promote-min-session-tokens 4000
                :discovery-min-failures 3
                :learn-snippets t
                :max-snippet-chars 1200
                :promote-transcript-tokens 30000
                :promoter-timeout 600
                :review-policy all
                :memory-tool-policy direct
                :user-char-limit 1500
                :memory-char-limit 2200
                :project-char-limit 3000
                :team-char-limit 4000
                :ltm-injection full
                :retrieval-entries 12
                :project-index-chars 2000
                :curator-interval-days 7
                :stale-after-sessions 20
                :stale-after-days 14
                :archive-after-sessions 60
                :archive-after-days 45
                :patch-threshold 2
                :max-skill-chars 8000
                :merge-interval-days nil
                :merge-threshold 0.34
                :merger-timeout 600
                :evaluator-timeout 600
                :auto-test nil
                :commit-project-skills ask
                :provider nil))
  "Built-in defaults for every `:memory' knob.")

;;;; Paths (FC6: every memory path goes through `pai-memory-dir')

(defun pai-memory-dir (&rest segments)
  "Return SEGMENTS under pai-memory's root (~/.pai/memory), creating nothing."
  (apply #'file-name-concat (expand-file-name "memory" pai-directory) segments))

(defun pai-memory-project-dir (cwd)
  "Return the project memory directory for project CWD."
  (pai-memory-dir "projects" (pai-session--slug cwd)))

;;;; Raw settings

(defun pai-memory--merge (base over)
  "Return plist BASE with OVER's keys applied; nested plists merge one level."
  (let ((out (copy-sequence base)))
    (while over
      (let ((k (car over)) (v (cadr over)))
        (setq out (plist-put out k
                             (if (and (memq k '(:session :long-term :budget :search))
                                      (listp v) (listp (plist-get out k)))
                                 (pai-memory--merge (plist-get out k) v)
                               v))))
      (setq over (cddr over)))
    out))

(defvar pai-memory--settings-cwd nil
  "Project directory whose settings `pai-memory-settings' reads, when bound.
Callers that know the project but may run outside its pai buffer (workers,
review buffers, timers) bind it, or pass a session, so they see the
project's knobs rather than only the global ones.")

(defvar pai-memory--project-settings-cache (make-hash-table :test 'equal)
  "FILE -> (MTIME . MEMORY-PLIST) of project settings read from disk.")

(defun pai-memory--project-memory-settings (&optional session)
  "Return the project-scope `:memory' settings for SESSION's project.
The project is `pai-memory--settings-cwd', else SESSION's cwd.  When it is
the project this buffer loaded (a pai buffer), the in-memory settings are
used; otherwise its settings file is read (cached by modification time).
With no project known, or no settings file, use this buffer's project settings."
  (let* ((dir (or pai-memory--settings-cwd
                  (and session (pai-session-p session) (pai-session-cwd session))))
         (dir (and dir (file-name-as-directory (expand-file-name dir)))))
    (if (or (null dir)
            (and pai-settings--project-dir
                 (equal dir (file-name-as-directory pai-settings--project-dir))))
        (pai-settings-scope-value :memory 'project)
      (let* ((file (pai-settings-project-file dir))
             (mtime (pai-settings--mtime file))
             (hit (gethash file pai-memory--project-settings-cache)))
        (cond ((null mtime) (pai-settings-scope-value :memory 'project))
              ((and hit (equal (car hit) mtime)) (cdr hit))
              (t (let ((v (plist-get (pai-settings--read file) :memory)))
                   (puthash file (cons mtime v) pai-memory--project-settings-cache)
                   v)))))))

(defun pai-memory-settings (&optional session)
  "Return the explicit `:memory' settings, project merged over global.
SESSION (or `pai-memory--settings-cwd') selects the project."
  (pai-memory--merge (or (pai-settings-scope-value :memory 'global) '())
                     (or (pai-memory--project-memory-settings session) '())))

(defun pai-memory--symbol (v)
  "Return V as a symbol when it is a string (settings JSON stores strings)."
  (if (stringp v) (intern v) v))

;;;; Session overrides

(defun pai-memory-session-state (session)
  "Return SESSION's folded `memory.state' overrides (latest value per key wins).
Keys: :session :learning :preset :budgetResumed.  A key's value of `:null'
clears the override."
  (let ((state '()))
    (when session
      (dolist (e (pai-session-entries session))
        (when (and (equal (plist-get e :type) "custom")
                   (equal (plist-get e :customType) "memory.state"))
          (let ((d (plist-get e :data)))
            (while d
              (setq state (plist-put state (car d) (cadr d)))
              (setq d (cddr d)))))))
    state))

(defun pai-memory-set-session-state (session &rest kv)
  "Record the overrides KV (key value ...) for SESSION."
  (when session
    (pai-session-append-custom session "memory.state" kv)))

(defun pai-memory--override (session key)
  "Return (VALUE) when SESSION has an override for KEY, else nil."
  (let ((state (pai-memory-session-state session)))
    (when (and (plist-member state key) (not (eq (plist-get state key) :null)))
      (list (plist-get state key)))))

;;;; Resolution

(defun pai-memory-preset (&optional session)
  "Return the effective preset symbol for SESSION."
  (let ((p (pai-memory--symbol
            (or (car (pai-memory--override session :preset))
                (plist-get (pai-memory-settings session) :preset)
                (plist-get pai-memory-defaults :preset)))))
    (if (memq p pai-memory-preset-names) p 'balanced)))

(defun pai-memory-preset-key-p (group key)
  "Return non-nil when a preset controls KEY of GROUP."
  (plist-member (plist-get (alist-get 'balanced pai-memory-presets) group) key))

(defun pai-memory-get (group key &optional session)
  "Return the effective value of knob KEY in GROUP (:session :long-term :budget :search).
SESSION supplies per-session overrides and the per-session preset."
  (let* ((preset (pai-memory-preset session))
         (preset-values (plist-get (alist-get preset pai-memory-presets) group))
         (explicit (plist-get (pai-memory-settings session) group)))
    (cond
     ((and (eq group :session) (eq key :enabled) (pai-memory--override session :session))
      (pai-truthy (car (pai-memory--override session :session))))
     ((and (not (eq preset 'custom)) (plist-member preset-values key))
      (plist-get preset-values key))
     ((plist-member explicit key)
      (let ((v (plist-get explicit key)))
        (if (eq v :false) nil v)))
     (t (plist-get (plist-get pai-memory-defaults group) key)))))

(defun pai-memory-stopped-p (&optional session)
  "Return non-nil when background memory work was stopped with `/memory stop'.
Nothing new starts until `/memory start' (or `/memory resume'); what is
already remembered is still used."
  (and session (pai-truthy (car (pai-memory--override session :stopped)))))

(defun pai-memory-private-p (&optional session)
  "Return non-nil when SESSION was made private with `/memory private'."
  (and session (pai-truthy (car (pai-memory--override session :private)))))

(defun pai-memory-session-enabled-p (&optional session)
  "Return non-nil when the session layer is on for SESSION (never when private)."
  (and (not (pai-memory-private-p session))
       (pai-truthy (pai-memory-get :session :enabled session))))

(defun pai-memory-learning-enabled-p (&optional session)
  "Return non-nil when the long-term layer and automatic learning are on.
Never for a private session."
  (and (not (pai-memory-private-p session))
       (pai-truthy (pai-memory-get :long-term :enabled session))
       (let ((o (pai-memory--override session :learning)))
         (if o (pai-truthy (car o)) t))))

(defun pai-memory-promote-triggers (&optional session)
  "Return SESSION's promotion triggers: a list of `consolidation', `session-end',
`manual' (settings JSON stores them as strings)."
  (let ((v (pai-memory-get :long-term :promote session)))
    (delq nil (mapcar (lambda (x) (let ((s (pai-memory--symbol x)))
                                    (and (memq s '(consolidation session-end manual)) s)))
                      (if (listp v) v (append v nil))))))

(defun pai-memory-observe-mode (&optional session)
  "Return the observer mode for SESSION: `continuous', `near-compaction' or `off'."
  (let ((m (pai-memory--symbol (pai-memory-get :session :observe session))))
    (if (memq m '(continuous near-compaction off)) m 'continuous)))

(defun pai-memory-set (group key value &optional scope)
  "Persist knob KEY of GROUP as VALUE in SCOPE (default `project').
Setting a knob a preset controls switches that scope's preset to `custom',
seeded with the knobs of the preset that was in effect."
  (let* ((scope (or scope 'project))
         (current (or (pai-settings-scope-value :memory scope) '()))
         (effective (pai-memory-preset))
         (new current))
    (when (and (pai-memory-preset-key-p group key) (not (eq effective 'custom)))
      ;; keep today's behaviour for the other preset knobs
      (dolist (g '(:session :long-term))
        (setq new (plist-put new g (pai-memory--merge
                                    (or (plist-get new g) '())
                                    (plist-get (alist-get effective pai-memory-presets) g)))))
      (setq new (plist-put new :preset "custom")))
    (setq new (plist-put new group (plist-put (copy-sequence (or (plist-get new group) '()))
                                              key value)))
    (pai-settings-set :memory new scope)
    value))

(defun pai-memory-unset (group key &optional scope)
  "Remove knob KEY of GROUP from SCOPE (default `project'), restoring its default."
  (let* ((scope (or scope 'project))
         (current (copy-sequence (or (pai-settings-scope-value :memory scope) '())))
         (g (copy-sequence (plist-get current group))))
    (when (plist-member g key)
      (setq g (cl-loop for (k v) on g by #'cddr unless (eq k key) append (list k v)))
      (pai-settings-set :memory (plist-put current group g) scope))
    nil))

(defun pai-memory-set-preset (preset &optional scope)
  "Persist PRESET (a symbol) in SCOPE (default `project')."
  (unless (memq preset pai-memory-preset-names)
    (error "Unknown memory preset: %s" preset))
  (let ((current (or (pai-settings-scope-value :memory (or scope 'project)) '())))
    (pai-settings-set :memory (plist-put (copy-sequence current) :preset (symbol-name preset))
                      (or scope 'project))
    preset))

(provide 'pai-memory-settings)
;;; pai-memory-settings.el ends here
