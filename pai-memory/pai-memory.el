;;; pai-memory.el --- Learning memory for pai (observational + long-term) -*- lexical-binding: t; -*-

;;; Commentary:

;; Entry point of the learning-memory extension, specified in
;; docs/SPEC-learning-memory.md.  It combines
;;
;;   * session memory (pi-observational-memory): background observers distil
;;     the transcript into observations, compaction renders them without an
;;     LLM call, a consolidator folds them into per-session topic files; and
;;   * long-term learning (Hermes' closed learning loop): a promoter proposes
;;     long-term memory entries and skills, reviewed before they apply.
;;
;; Implemented so far (Phase 1):
;;   pai-memory-worker    background workers, shown above the prompt
;;   pai-memory-settings  `:memory' settings, presets, per-session overrides
;;   pai-memory-ledger    observation ledger folded from the session
;;   pai-memory-observer  observer clock, workers and commits
;;   pai-memory-compact   observational compaction (`compact' handler)
;;   pai-memory-budget    spend, budget caps, redaction
;;
;; Phase 2:
;;   pai-memory-consolidate  consolidator: topic files, INDEX.md, JOURNEY.md,
;;                           fork support
;;
;; Phase 3:
;;   pai-memory-store     long-term Markdown memory (USER.md, MEMORY.md,
;;                        project MEMORY.md), provider API, change log and
;;                        undo, the `memory' tool, the system-prompt snapshot
;;
;; Phase 4:
;;   pai-memory-proposals proposal queue: validate, store, apply, risk scan,
;;                        learned-skill writer
;;   pai-memory-promote   promoter: digest, triggers, pending/catch-up
;;   pai-memory-review    /memory-review buffer
;;
;; Phase 5:
;;   pai-memory-skills    skill usage tracking and the curator
;;
;; V2:
;;   pai-memory-search    memory_search: full-text index and tool (A1)
;;   pai-memory-recall    automatic per-turn recall (A2, opt-in)
;;   pai-memory-insights  /memory insights: spend report and suggestions (E3)
;;   pai-memory-privacy   /memory private and /memory forget (G3, G1); custom
;;                        redaction lives in pai-memory-budget
;;   pai-memory-browse    /memory-browse buffer (F1)
;;   pai-memory-quality   skill lint and security scan (C2), /memory lint
;;   pai-memory-merge     merging overlapping learned skills (C1), /memory merge
;;   pai-memory-learn     /learn --from URL|BUFFER|FILE|DIR (C3)
;;   pai-memory-share     /skills-export, /skills-import (C5); git staging (C6)
;;   pai-memory-entries   entry metadata, expiry, retrieval mode, team memory (B2, B3, G2)
;;   pai-memory-why       "why do you know this?" (F2), /memory why
;;   pai-memory-topics    project topic tree across sessions (B1), /memory merge-topics
;;   pai-memory-graph     learning timeline and graph (F3), /memory timeline, /memory graph
;;   pai-memory-evaluate  test-run a proposed skill in a scratch copy (C4), t in review
;;   pai-memory-reflect   reflections over a long session's observations (B4), /memory reflect
;;   pai-memory-embed     optional semantic search: embedders, vectors, fusion (A3)
;;   pai-memory-viz       worker lines, footer gauges, status pipeline and timeline
;;
;; Commands:
;;   /memory                       status
;;   /memory stop [ID|all]         stop running workers
;;   /memory observe               observe everything waiting now
;;   /memory consolidate           file the whole pool into topics now
;;   /memory compact               compact now (observational when possible)
;;   /memory session on|off        session layer for this session
;;   /memory learning on|off       automatic learning for this session
;;   /memory preset NAME           preset for this session
;;   /memory resume                lift a budget pause for this session
;;   /memory show                  long-term memory as saved now
;;   /memory undo [ID]             undo the newest (or a given) memory change
;;   /memory promote               run the promoter over this session now
;;   /memory review, /memory-review   review pending proposals
;;   /learn [DESCRIPTION]          capture a skill from this session
;;   /memory skills                skill usage, outcomes, state
;;   /memory curate                run the curator now
;;   /memory-pin, /memory-unpin NAME, /memory-restore-skill NAME

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-ext)
(require 'pai-commands)
(require 'pai-session)
(require 'pai-activity)
(require 'pai-memory-worker)
(require 'pai-memory-settings)
(require 'pai-memory-ledger)
(require 'pai-memory-budget)
(require 'pai-memory-observer)
(require 'pai-memory-compact)
(require 'pai-memory-consolidate)
(require 'pai-memory-store)
(require 'pai-memory-proposals)
(require 'pai-memory-snippets)
(require 'pai-memory-promote)
(require 'pai-memory-review)
(require 'pai-memory-skills)
(require 'pai-memory-search)
(require 'pai-memory-recall)
(require 'pai-memory-insights)
(require 'pai-memory-privacy)
(require 'pai-memory-browse)
(require 'pai-memory-quality)
(require 'pai-memory-merge)
(require 'pai-memory-learn)
(require 'pai-memory-share)
(require 'pai-memory-entries)
(require 'pai-memory-why)
(require 'pai-memory-topics)
(require 'pai-memory-graph)
(require 'pai-memory-evaluate)
(require 'pai-memory-reflect)
(require 'pai-memory-embed)
(require 'pai-memory-viz)

(defvar pai--session)
(declare-function pai--set-widget "pai-ui" (key content))
(declare-function pai--compact-now "pai-ui" (custom-instructions &optional reason))
(declare-function pai-settings-ui-register-section "pai-settings-ui")
(declare-function pai-settings-ui-register-subsection "pai-settings-ui")
(declare-function pai-settings-ui-register-item "pai-settings-ui")

;;;; Mode-line widget

(defun pai-memory--widget-state (text face help)
  "Return widget TEXT in FACE with tooltip HELP."
  (propertize text 'face face 'help-echo help))

(defun pai-memory-widget-text ()
  "Return the mode-line widget text for the current pai buffer, or nil.
Memory never just disappears: stopped (`/memory stop') and switched off
\(both layers off in settings) have their own states."
  (let ((session (and (boundp 'pai--session) pai--session)))
    (cond
     ((null session) nil)
     ((pai-memory-stopped-p session)
      (pai-memory--widget-state
       "🧠 ⏹ stopped" 'warning
       "Background memory is stopped for this session: nothing is observed or learned. /memory start resumes it."))
     ((not (or (pai-memory-session-enabled-p session)
               (pai-memory-learning-enabled-p session)
               (pai-memory-private-p session)))
      (pai-memory--widget-state
       "🧠 off" 'shadow
       "Memory is switched off (session memory and long-term learning are both off). Turn it on with /memory on, or in /menu > Memory."))
     (t
      (let* ((gauges (and (pai-memory-session-enabled-p session)
                          (pai-memory-viz-footer-gauges session)))
             (all (pai-memory-spend session))
             (spend (plist-get all :cost))
             (paused (pai-memory-budget-exceeded session))
             (pending (pai-memory-pending-count)))
        (string-join
         (delq nil (list (if (pai-memory-private-p session) "🧠🔒" "🧠")
                         (when (and gauges (not (string-empty-p gauges))) gauges)
                         (cond ((> (plist-get all :unpriced) 0)
                                (format "%st" (pai-activity-fmt-count (plist-get all :tokens))))
                               ((> spend 0) (format "$%.2f" spend)))
                         (when (> pending 0) (format "prop:%d" pending))
                         (when paused "⏸ budget")
                         (when pai-memory-model-problem "⚠ model")))
         " "))))))

(defun pai-memory-refresh-widget ()
  "Redraw the memory widget in the current pai buffer."
  (when (fboundp 'pai--set-widget)
    (pai--set-widget "memory" (ignore-errors (pai-memory-widget-text)))))

(add-hook 'pai-memory-change-hook #'pai-memory-refresh-widget)

;;;; Status

(defun pai-memory--worker-line (entry)
  "Return one status line for worker ENTRY (running or finished)."
  (format "  %-7s %-12s %-9s %6st %6s %s  %s"
          (plist-get entry :id)
          (plist-get entry :label)
          (plist-get entry :status)
          (pai-activity-fmt-count (pai-activity-tokens entry))
          (pai-activity-fmt-duration (pai-activity-elapsed entry))
          (if (plist-get entry :cost) (format "$%.4f" (plist-get entry :cost)) "")
          (or (plist-get entry :detail) "")))

(defun pai-memory--layer-line (session)
  "Return the layers/preset line for SESSION."
  (format "%s%sSession layer: %s (observe: %s) · Learning: %s · Preset: %s"
          (if (pai-memory-stopped-p session)
              "⏹ STOPPED: no background work until /memory start · " "")
          (if (pai-memory-private-p session) "🔒 PRIVATE session · " "")
          (if (pai-memory-session-enabled-p session) "on" "off")
          (pai-memory-observe-mode session)
          (if (pai-memory-learning-enabled-p session) "on" "off")
          (pai-memory-preset session)))

(defun pai-memory--ledger-lines (session)
  "Return ledger status lines for SESSION's current branch."
  (let* ((branch (pai-session-get-branch session))
         (batches (pai-memory-batches branch))
         (pool (pai-memory-pool branch))
         (runs (pai-memory-unobserved-runs
                branch (mapcar (lambda (r) (cons (plist-get r :from) (plist-get r :to)))
                               pai-memory--in-flight))))
    (list (format "Observations: %d in pool (%s tokens) from %d batch(es)"
                  (length pool) (pai-activity-fmt-count (pai-memory-observation-tokens pool))
                  (length batches))
          (let* ((dir (pai-memory-session-dir session))
                 (topics (pai-memory-topics dir))
                 (journey (pai-memory-journey dir)))
            (format "Topics: %d%s · journey: %s tokens · consolidator: %s"
                    (length topics)
                    (if topics (format " in %s" (abbreviate-file-name dir)) "")
                    (pai-activity-fmt-count (if journey (pai-estimate-tokens-from-chars
                                                          (length journey))
                                              0))
                    (cond ((and pai-memory--consolidating
                                (equal (plist-get pai-memory--consolidating :status) "running"))
                           "running")
                          ((not (pai-truthy (pai-memory-get :session :consolidate session))) "off")
                          ((> pai-memory--con-failures 0)
                           (format "%d failure(s), retry after %s" pai-memory--con-failures
                                   (format-time-string "%H:%M:%S" pai-memory--con-backoff-until)))
                          (t (format "at %s pool tokens"
                                     (pai-activity-fmt-count
                                      (pai-memory-get :session :consolidate-at-pool-tokens session)))))))
          (format "Unobserved: %s tokens · in flight: %d slice(s)%s"
                  (pai-activity-fmt-count (pai-memory-unobserved-tokens runs))
                  (length pai-memory--in-flight)
                  (if (> pai-memory--failures 0)
                      (format " · %d failure(s), retry after %s"
                              pai-memory--failures
                              (format-time-string "%H:%M:%S" pai-memory--backoff-until))
                    "")))))

(defun pai-memory--long-term-lines (session)
  "Return long-term memory status lines for SESSION."
  (when (pai-truthy (pai-memory-get :long-term :enabled session))
    (list (format "Long-term: %s · provider: markdown%s%s"
                  (mapconcat (lambda (target)
                               (format "%s %d/%s chars"
                                       (pai-memory-target-file-name target)
                                       (length (pai-memory--read-file
                                                (pai-memory-target-file target default-directory)))
                                       (pai-memory-target-limit target session)))
                             (pai-memory-active-targets default-directory) " · ")
                  (let ((ext (pai-memory-active-provider session)))
                    (if ext (format " + %s" (plist-get ext :name)) ""))
                  (if (> pai-memory--applied-this-session 0)
                      (format " · %d change(s) apply next session" pai-memory--applied-this-session)
                    ""))
          (let ((last (pai-memory-last-promotion (pai-session-id session))))
            (format "Learning: %s · promote on: %s · %d proposal(s) pending%s · last promotion: %s"
                    (if (pai-memory-learning-enabled-p session) "on" "off")
                    (mapconcat #'symbol-name (pai-memory-promote-triggers session) ", ")
                    (pai-memory-pending-count)
                    (if (and pai-memory--promoting
                             (equal (plist-get pai-memory--promoting :status) "running"))
                        " · promoter running" "")
                    (or (plist-get last :time) "never"))))))

(defun pai-memory--spend-lines (session)
  "Return spend and budget lines for SESSION."
  (let* ((spend (pai-memory-spend session))
         (daily (pai-memory-budget-daily))
         (paused (pai-memory-budget-exceeded session)))
    (append
     (list (format "Background spend: $%.4f this session (%d run(s), %s billable tokens) · $%.4f / %s tokens today"
                   (plist-get spend :cost) (plist-get spend :runs)
                   (pai-activity-fmt-count (plist-get spend :tokens))
                   (plist-get daily :usd) (pai-activity-fmt-count (plist-get daily :tokens))))
     (when (> (plist-get spend :unpriced) 0)
       (list (format "  prices unknown for %d run(s): their dollar cost is not counted; the token caps still apply"
                     (plist-get spend :unpriced))))
     (mapcar (lambda (r)
               (format "  %-12s $%.4f  %d run(s)  %s tokens" (car r)
                       (plist-get (cdr r) :cost) (plist-get (cdr r) :runs)
                       (pai-activity-fmt-count (plist-get (cdr r) :tokens))))
             (plist-get spend :by-role))
     (list (format "Budget: $%s/session, $%s/day, %s/%s tokens%s"
                   (or (pai-memory-get :budget :session-usd session) "∞")
                   (or (pai-memory-get :budget :daily-usd session) "∞")
                   (let ((v (pai-memory-get :budget :session-tokens session)))
                     (if v (pai-activity-fmt-count v) "∞"))
                   (let ((v (pai-memory-get :budget :daily-tokens session)))
                     (if v (pai-activity-fmt-count v) "∞"))
                   (if paused (format " · PAUSED: %s (/memory resume)" paused) ""))))))

(defun pai-memory--promoter-gauge (session)
  "Return SESSION's promoter gauge (NEW-TOKENS MIN-NEW STATE), or nil when learning is off.
NEW-TOKENS is how far the session digest grew since the last promotion."
  (when (and (pai-memory-learning-enabled-p session)
             (pai-truthy (pai-memory-get :long-term :enabled session)))
    (let* ((last (pai-memory-last-promotion (pai-session-id session)))
           (digest (pai-memory-session-digest session)))
      (list (max 0 (- (plist-get digest :tokens) (or (plist-get last :tokens) 0)))
            (or (pai-memory-get :long-term :promote-min-new-tokens session) 0)
            (format "on %s · last %s"
                    (mapconcat #'symbol-name (pai-memory-promote-triggers session) ", ")
                    (or (plist-get last :time) "never"))))))

(defun pai-memory--role-model-text (role)
  "Return the model worker ROLE would use now, or why it cannot run."
  (condition-case err
      (let ((m (pai-memory-worker-model role)))
        (if m (pai-model-key m) "none"))
    (pai-memory-model-unavailable (format "UNAVAILABLE (%s)" (cadr err)))))

(defun pai-memory-status-text ()
  "Return the /memory status report for the current pai buffer."
  (let* ((session (and (boundp 'pai--session) pai--session))
         (running (pai-memory-worker-running))
         (recent (seq-take (seq-remove (lambda (e) (equal (plist-get e :status) "running"))
                                       (pai-activity-entries "memory"))
                           8)))
    (string-join
     (delq nil
           (append
            (when session (list (pai-memory--layer-line session)))
            (when (and session (pai-memory-session-enabled-p session))
              (pai-memory--ledger-lines session))
            (when (and session (pai-memory-session-enabled-p session))
              (ignore-errors
                (pai-memory-viz-pipeline-lines
                 session :promoter (pai-memory--promoter-gauge session))))
            (list (format "Workers: %d running" (length running)))
            (list (format "Models: observer %s · consolidator %s"
                          (pai-memory--role-model-text 'observer)
                          (pai-memory--role-model-text 'consolidator)))
            (when pai-memory-model-problem
              (list (concat "⚠ " pai-memory-model-problem)))
            (mapcar #'pai-memory--worker-line running)
            (when recent (cons "Recent:" (mapcar #'pai-memory--worker-line recent)))
            (when session (pai-memory--long-term-lines session))
            (when (pai-memory-search-available-p)
              (list (format "Search index: %.1f MB%s" (pai-memory-index-size-mb)
                            (pcase pai-memory-index-status
                              ('more " · indexing") ('full " · FULL (raise the size limit or /memory reindex)")
                              (_ "")))))
            (when session (pai-memory--spend-lines session))))
     "\n")))

;;;; Commands

(defun pai-memory-target-file-name (target)
  "Return a short display name for TARGET's file."
  (pcase target ('user "USER.md") ('memory "MEMORY.md") ('project "project MEMORY.md")
    ('team "PROJECT.md")))

(defun pai-memory-show-text ()
  "Return long-term memory as currently saved, for `/memory show'."
  (string-join
   (mapcar (lambda (target)
             (let ((entries (pai-memory-read target default-directory))
                   (file (pai-memory-target-file target default-directory)))
               (format "%s (%s)\n%s" (pai-memory-target-title target) (abbreviate-file-name file)
                       (if entries
                           (mapconcat (lambda (e) (concat "  - " e)) entries "\n")
                         "  (empty)"))))
           (pai-memory-active-targets default-directory))
   "\n\n"))

(defun pai-memory--stop (arg)
  "Stop the worker whose id starts with ARG, or stop memory for this session.
Without ARG (or with \"all\") every worker is stopped and background memory
work is paused for the session, until `/memory start'."
  (if (or (null arg) (string-empty-p arg) (equal arg "all"))
      (let ((session (and (boundp 'pai--session) pai--session))
            (n (pai-memory-worker-stop-all)))
        (when session (pai-memory-set-session-state session :stopped t))
        (setq pai-memory--budget-notice nil)
        (format "%s; memory is stopped for this session (⏹ in the status bar). /memory start resumes it"
                (if (> n 0) (format "Stopped %d memory worker(s)" n) "No memory worker was running")))
    (let ((entry (seq-find (lambda (e) (string-prefix-p arg (plist-get e :id)))
                           (pai-memory-worker-running))))
      (if (and entry (pai-activity-stop entry))
          (format "Stopped %s" (plist-get entry :id))
        (format "No running memory worker matches %s" arg)))))

(defun pai-memory-switch (on)
  "Switch memory on or off (ON nil) everywhere; return a message.
Both layers are set in the global settings; this project's own `enabled'
values and this session's overrides (session/learning, stop) are cleared
so they cannot contradict it.  Turning off also stops every worker."
  (let ((session (and (boundp 'pai--session) pai--session))
        (n (if on 0 (pai-memory-worker-stop-all))))
    (dolist (group '(:session :long-term))
      (pai-memory-set group :enabled (if on t :false) 'global)
      (pai-memory-unset group :enabled 'project))
    (when session
      (pai-memory-set-session-state session :session :null :learning :null :stopped :false))
    (setq pai-memory--budget-notice nil)
    (if on
        (progn (pai-memory-tick)
               "Memory is on (session memory and learning, global settings). /memory off turns it off")
      (format "Memory is off everywhere (global settings)%s: nothing is observed or learned. /memory on turns it back on"
              (if (> n 0) (format "; stopped %d worker(s)" n) "")))))

(defun pai-memory--on-off (word)
  "Return t for \"on\", :false for \"off\", or signal a user error."
  (pcase word ("on" t) ("off" :false)
    (_ (user-error "Expected on or off"))))

(defun pai-memory--dispatch (words)
  "Run the /memory subcommand WORDS in the current pai buffer; return a message."
  (let ((session (and (boundp 'pai--session) pai--session))
        (arg (cadr words)))
    (pcase (car words)
      ((or 'nil "status") (pai-memory-status-text))
      ("on" (pai-memory-switch t))
      ("off" (pai-memory-switch nil))
      ("stop" (pai-memory--stop arg))
      ("observe"
       (if (not session) "No session"
         (let ((n (pai-memory-observer-tick t)))
           (if (> n 0) (format "Started %d observer(s)" n) "Nothing to observe"))))
      ("consolidate"
       (cond ((not session) "No session")
             ((pai-memory-consolidator-tick t) "Consolidator started")
             ((and pai-memory--consolidating
                   (equal (plist-get pai-memory--consolidating :status) "running"))
              "The consolidator is already running")
             (t "Nothing to consolidate")))
      ("compact"
       (if (fboundp 'pai--compact-now)
           (pcase (pai--compact-now nil 'manual)
             ('queued "Compaction queued: the run compacts at its next turn boundary")
             ('nil "Nothing to compact")
             (_ "Compaction done"))
         "Compaction is not available here"))
      ((and (or "session" "learning")
            (guard (equal (nth 2 words) "--global")))
       (let* ((group (if (equal (car words) "session") :session :long-term))
              (v (pai-memory--on-off arg)))
         (pai-memory-set group :enabled v 'global)
         (pai-memory-unset group :enabled 'project)
         (when session
           (pai-memory-set-session-state session (if (eq group :session) :session :learning) :null))
         (when (and (eq group :session) (eq v :false)) (pai-memory-worker-stop-all))
         (format "%s %s everywhere (global settings)"
                 (if (eq group :session) "Session memory" "Learning")
                 (if (eq v t) "on" "off"))))
      ((or "session" "learning")
       (if (not session) "No session"
         (let ((v (pai-memory--on-off arg)))
           (pai-memory-set-session-state session (if (equal (car words) "session")
                                                     :session :learning)
                                         v)
           (when (and (equal (car words) "session") (eq v :false))
             (pai-memory-worker-stop-all))
           (format "%s %s for this session"
                   (if (equal (car words) "session") "Session memory" "Learning")
                   (if (eq v t) "on" "off")))))
      ("preset"
       (let ((p (and arg (intern arg))))
         (cond ((not session) "No session")
               ((not (memq p pai-memory-preset-names))
                (format "Presets: %s" (mapconcat #'symbol-name pai-memory-preset-names " ")))
               (t (pai-memory-set-session-state session :preset (symbol-name p))
                  (format "Preset %s for this session" p)))))
      ("show" (pai-memory-show-text))
      ("promote"
       (cond ((not session) "No session")
             ((pai-memory-promote session :reason 'manual :force t) "Promoter started")
             ((and pai-memory--promoting
                   (equal (plist-get pai-memory--promoting :status) "running"))
              "The promoter is already running")
             (t "Long-term memory is off")))
      ("review" (pai-memory-review) "Opened the review buffer")
      ("skills" (concat "Skills:\n" (pai-memory-skills-status-text)))
      ("merge" (pai-memory-merge :force t))
      ("reflect"
       (cond ((not session) "No session")
             ((pai-memory-reflect t) "Reflector started")
             ((and pai-memory--reflecting (equal (plist-get pai-memory--reflecting :status) "running"))
              "The reflector is already running")
             (t "No new observations to reflect on")))
      ("timeline"
       (let ((days (and arg (string-to-number arg))))
         (pai-memory-timeline (and days (> days 0) days) default-directory)
         "Opened *pai learning*"))
      ("graph" (pai-memory-graph default-directory))
      ("merge-topics"
       (cond ((not session) "No session")
             ((pai-memory-topic-merge session t) "Topic merger started")
             ((and pai-memory--topic-merging (equal (plist-get pai-memory--topic-merging :status) "running"))
              "The topic merger is already running")
             (t "No session topics changed since the last merge")))
      ("curate"
       (if (equal arg "--consolidate") (pai-memory-merge :force t)
        (let ((r (pai-memory-curate)))
         (format "Curator: %d skill(s) newly stale, %d archived, %d flagged for review; %d expired entr%s proposed for removal"
                 (plist-get r :stale) (plist-get r :archived) (plist-get r :flagged)
                 (plist-get r :expired) (if (= (plist-get r :expired) 1) "y" "ies")))))
      ("search"
       (let ((query (string-join (cdr words) " ")))
         (if (string-empty-p query) "Usage: /memory search WORDS"
           (pai-memory-index-update 2.0)
           (pai-memory-format-hits (pai-memory-search query :limit 10 :semantic t) query))))
      ("private"
       (if (not session) "No session"
         (pai-memory-set-private session (not (equal arg "off")))))
      ("forget" (pai-memory-forget (cdr words)))
      ("why"
       (let ((q (string-join (cdr words) " ")))
         (if (string-empty-p q) "Usage: /memory why QUOTE-OF-ENTRY|SKILL-NAME"
           (pai-memory-why q default-directory)
           "Opened *pai-memory-why*")))
      ("lint"
       (let ((skills (pai-discover-skills (pai-memory--skill-dirs))))
         (pai-memory-lint-report
          (if arg (seq-filter (lambda (s) (equal (plist-get s :name) arg)) skills) skills)
          default-directory)))
      ("insights"
       (let ((days (and arg (string-to-number arg))))
         (pai-memory-insights-text (and days (> days 0) days))))
      ("reindex"
       (pai-memory-index-reset)
       (let ((r (pai-memory-index-update 10.0)))
         (when (eq r 'more) (pai-memory-index-schedule))
         (format "Index %s (%.1f MB)"
                 (pcase r ('done "rebuilt") ('more "rebuilding in the background") (_ "full"))
                 (pai-memory-index-size-mb))))
      ("undo" (pai-memory-undo arg (list :session session)))
      ((or "start" "resume")
       (if (not session) "No session"
         (let ((was-stopped (pai-memory-stopped-p session)))
           (if was-stopped
               (pai-memory-set-session-state session :stopped :false)
             (pai-memory-set-session-state session :budgetResumed t))
           (setq pai-memory--budget-notice nil)
           (pai-memory-tick)
           (cond (was-stopped "Memory resumed for this session")
                 ((equal (car words) "start") "Memory was not stopped; budget pause lifted for this session")
                 (t "Budget pause lifted for this session")))))
      (_ (concat "Usage: /memory [status | on | off | stop [ID|all] | start | observe | consolidate | compact | "
                 "session on|off [--global] | learning on|off [--global] | preset NAME | resume | show | undo [ID] | promote | review | skills | curate | search WORDS | reindex | insights [DAYS] | private [on|off] | lint [SKILL] | merge | merge-topics | why QUOTE|SKILL | timeline [DAYS] | graph | reflect | forget TEXT [--regex] [--all] [--dry-run]]")))))

(defun pai-memory-command (args ctx)
  "Handle `/memory' with ARGS in the pai buffer from CTX."
  (let ((buf (plist-get ctx :buffer)))
    (if (not (buffer-live-p buf))
        (list :message "No active pai buffer")
      (with-current-buffer buf
        (prog1 (list :message (condition-case err
                                  (pai-memory--dispatch (split-string (or args "") nil t))
                                (user-error (error-message-string err))))
          (pai-memory-refresh-widget))))))



(defun pai-memory--undo-ids ()
  "Return the ids of the newest memory changes, newest first."
  (ignore-errors (seq-take (nreverse (mapcar (lambda (r) (plist-get r :id)) (pai-memory-log-read))) 20)))

(defun pai-memory--skill-name-list ()
  "Return the skill names."
  (ignore-errors (mapcar (lambda (s) (plist-get s :name))
                         (pai-discover-skills (pai-memory--skill-dirs)))))

(defconst pai-memory-completion-tree
  (let ((on-off-global '(("on" "--global") ("off" "--global"))))
    `("status" "on" "off"
      ("stop" "all" ,(lambda () (mapcar (lambda (e) (plist-get e :id))
                                        (ignore-errors (pai-memory-worker-running)))))
      "start" "observe" "consolidate" "compact"
      ("session" ,@on-off-global)
      ("learning" ,@on-off-global)
      ("preset" ,(lambda () (mapcar #'symbol-name pai-memory-preset-names)))
      "resume" "show"
      ("undo" ,#'pai-memory--undo-ids)
      "promote" "review" "skills"
      ("curate" "--consolidate")
      ("search" (:rest))
      "reindex"
      ("insights" "7" "30" "90")
      ("private" "on" "off")
      ("forget" (:rest "--regex" "--all" "--dry-run"))
      ("lint" ,#'pai-memory--skill-name-list)
      "merge" "merge-topics"
      ("why" ,#'pai-memory--skill-name-list)
      ("timeline" "7" "30" "90")
      "graph" "reflect"))
  "What `/memory' completes at each argument position.")

(defconst pai-memory-subcommands
  (mapcar (lambda (n) (if (consp n) (car n) n)) pai-memory-completion-tree)
  "Every `/memory' subcommand, for completion and the usage line.")

(defun pai-memory--completions (prefix)
  "Complete `/memory' arguments for PREFIX, at every level."
  (funcall (pai-command-completion-tree pai-memory-completion-tree) prefix))

;;;; /learn

(defun pai-memory-learn-command (args ctx)
  "Handle `/learn DESCRIPTION [--from SOURCE...]': capture a skill.
Without sources the promoter learns from this session; with them it also
reads the URLs, buffers, files and directories named."
  (let ((buf (plist-get ctx :buffer)))
    (if (not (buffer-live-p buf))
        (list :message "No active pai buffer")
      (with-current-buffer buf
        (list :message
              (condition-case err
                  (let* ((parsed (pai-memory-learn-parse args))
                         (sources (and (cdr parsed)
                                       (pai-memory-learn-snapshot
                                        (mapcar (lambda (s) (pai-memory-learn-resolve s default-directory))
                                                (cdr parsed))))))
                    (cond ((not pai--session) "No session")
                          ((pai-memory-promote pai--session :reason 'learn :force t
                                               :learn (car parsed) :sources sources)
                           (if sources
                               (format "Learning from %d source(s); the proposals open for review when it finishes"
                                       (length sources))
                             "Learning from this session; the proposals open for review when it finishes"))
                          ((and pai-memory--promoting
                                (equal (plist-get pai-memory--promoting :status) "running"))
                           "The promoter is already running; try again when it finishes")
                          (t "Long-term memory is off")))
                (user-error (error-message-string err))))))))

;;;; Event handlers

(defun pai-memory--in-buffer (ctx fn)
  "Call FN in CTX's pai buffer, reporting errors instead of raising them."
  (let ((buf (plist-get ctx :buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (condition-case err (funcall fn)
          (error (message "pai-memory: %s" (error-message-string err))))))))

(defun pai-memory-tick ()
  "Run every memory clock in the current pai buffer."
  (pai-memory-observer-tick)
  (pai-memory-consolidator-tick)
  (pai-memory-refresh-widget))

(add-hook 'pai-memory-commit-hook #'pai-memory-consolidator-tick)
(add-hook 'pai-memory-commit-hook #'pai-memory-reflect-maybe)
(add-hook 'pai-memory-proposals-hook #'pai-memory-auto-test)
(add-hook 'pai-memory-observations-committed-functions
          (lambda (obs) (ignore-errors (pai-memory-record-outcomes obs))))
(add-hook 'pai-memory-consolidated-hook
          (lambda () (pai-memory-promote-maybe 'consolidation)))

(defun pai-memory--on-settled (_event ctx)
  "After a run settles: count the session, observe, consolidate if due."
  (pai-memory--in-buffer ctx (lambda ()
                               (when pai--session (pai-memory-count-session pai--session))
                               (pai-memory-tick)
                               (pai-memory-note-pending)
                               (pai-memory-index-schedule))))

(defun pai-memory--on-turn-end (_event ctx)
  "After each turn: launch observers and consolidators that are now due.
Every completed message is already in the session by now (see
`pai--commit-message'), and a turn boundary is a consistent point -- the
tool results of the turn are in -- so workers need not wait for the whole
run to finish.  Per-run bookkeeping stays in `pai-memory--on-settled'."
  (pai-memory--in-buffer ctx #'pai-memory-tick))

(defun pai-memory--on-session-end (_event ctx)
  "Before leaving a session (/new, /resume), promote it when due."
  (pai-memory--in-buffer ctx (lambda () (pai-memory-promote-maybe 'session-end))))

(defun pai-memory--on-session-start (_event ctx)
  "On session start or switch, forget per-session state and catch up."
  (pai-memory--in-buffer ctx (lambda ()
                               (setq pai-memory--in-flight nil pai-memory--failures 0
                                     pai-memory--backoff-until 0 pai-memory--budget-notice nil
                                     pai-memory--consolidating nil pai-memory--con-failures 0
                                     pai-memory--con-backoff-until 0
                                     pai-memory--applied-this-session 0)
                               (pai-memory-tick)
                               (pai-memory-catch-up)
                               (pai-memory-curator-maybe (current-buffer))
                               (pai-memory-index-schedule))))

(defun pai-memory--on-tool-start (event ctx)
  "Track skill views and uses."
  (pai-memory--in-buffer ctx (lambda ()
                               (when (pai-truthy (pai-memory-get :long-term :enabled pai--session))
                                 (pai-memory-track-tool-start event)))))

(defun pai-memory--on-agent-start (_event ctx)
  "A new run: views from earlier runs no longer turn into uses."
  (pai-memory--in-buffer ctx (lambda () (setq pai-memory--run-views nil))))

(defun pai-memory--on-input (event ctx)
  "Count `/skill:NAME' inputs as uses; never changes the input."
  (pai-memory--in-buffer ctx (lambda ()
                               (when (pai-truthy (pai-memory-get :long-term :enabled pai--session))
                                 (pai-memory-track-input (plist-get event :text)))))
  nil)

(defun pai-memory--in-pai-buffer (fn)
  "Return a command handler calling FN with the argument string in the pai buffer.
FN returns the message; a `user-error' becomes the message."
  (lambda (args ctx)
    (let ((buf (plist-get ctx :buffer)))
      (list :message
            (condition-case err
                (if (buffer-live-p buf)
                    (with-current-buffer buf (funcall fn (or args "")))
                  (funcall fn (or args "")))
              (user-error (error-message-string err)))))))

(defun pai-memory--skill-command (fn)
  "Return a command handler calling FN with the trimmed argument in the pai buffer."
  (lambda (args ctx)
    (let ((name (string-trim (or args ""))))
      (list :message
            (if (string-empty-p name) "Give a skill name"
              (let ((buf (plist-get ctx :buffer)))
                (if (buffer-live-p buf)
                    (with-current-buffer buf (funcall fn name))
                  (funcall fn name))))))))

(defun pai-memory--pin (arg pin)
  "Pin or unpin the skill named ARG, or else the memory entry quoting ARG."
  (if (seq-find (lambda (s) (equal (plist-get s :name) arg)) (ignore-errors (pai-memory--skills)))
      (pai-memory-set-pinned arg pin)
    (condition-case err (pai-memory-pin-entry arg default-directory pin)
      (user-error (format "No skill named %s, and %s" arg (error-message-string err))))))

(defun pai-memory--skill-names (_prefix)
  "Complete skill names."
  (ignore-errors (mapcar (lambda (s) (plist-get s :name)) (pai-memory--skills))))

(defun pai-memory--system-sections (_event ctx)
  "Contribute the long-term memory snapshot to a new session's system prompt."
  (let ((text (condition-case err
                  (pai-memory-snapshot (list :cwd (plist-get ctx :cwd)
                                             :session (plist-get ctx :session)))
                (error (message "pai-memory: snapshot failed: %s" (error-message-string err))
                       nil))))
    (and text (list :memory text))))

(defun pai-memory--on-refresh (_event ctx)
  "Redraw the widget after a change elsewhere."
  (pai-memory--in-buffer ctx #'pai-memory-refresh-widget))

;;;; Settings screen

(defconst pai-memory-menu-session-items
  '((:observe choice "Observe"
     "When observers run; setting this switches the preset to custom"
     ("continuous" "near-compaction" "off"))
    (:observe-start-ratio number "Near-compaction start"
     "near-compaction mode: start observing at this fraction of the compaction threshold (0-1)")
    (:chunk-tokens number "Chunk tokens"
     "New conversation per observer call; bigger = fewer calls (preset knob)")
    (:observer-concurrency number "Observers at once"
     "How many observers may run at the same time; lower = gentler on the provider, same cost")
    (:observer-tool-result-chars number "Tool result chars"
     "How much of each tool result an observer sees")
    (:observer-timeout number "Observer timeout (s)"
     "Seconds before a running observer is stopped")
    (:tail-tokens number "Verbatim tail tokens"
     "Recent conversation kept word for word after compaction")
    (:consolidate boolean "Consolidate"
     "File old observations into topic files (preset knob)")
    (:consolidate-at-pool-tokens number "Consolidate at pool tokens"
     "Stored-observation size that starts the consolidator (preset knob)")
    (:pool-target-tokens number "Pool target tokens"
     "How far the consolidator shrinks the stored observations")
    (:max-observation-tokens number "Max observation tokens"
     "Observations kept in the compaction block; older ones move to an archive file")
    (:journey-target-tokens number "Journey tokens"
     "Target size of JOURNEY.md")
    (:consolidator-timeout number "Consolidator timeout (s)"
     "Seconds before a running consolidator is stopped")
    (:topic-merge boolean "Merge into project topics"
     "After consolidation, fold changed session topics into the project's topic tree (preset knob)")
    (:topic-merger-timeout number "Topic merger timeout (s)"
     "Seconds before a running topic merger is stopped")
    (:reflect boolean "Reflections (experimental)"
     "Condense a long session's observations into patterns shown above them after compaction")
    (:reflect-every-tokens number "Reflect every (observation tokens)"
     "New observations that trigger the next reflection")
    (:reflector-timeout number "Reflector timeout (s)"
     "Seconds before a running reflector is stopped"))
  "Session-layer knobs in /menu: (KEY TYPE LABEL DOC [CHOICES]).")

(defconst pai-memory-menu-long-term-items
  '((:user-char-limit number "USER.md limit (chars)" "Maximum size of USER.md")
    (:memory-char-limit number "MEMORY.md limit (chars)" "Maximum size of the global MEMORY.md")
    (:project-char-limit number "Project MEMORY.md limit (chars)"
     "Maximum size of each project's MEMORY.md")
    (:team-char-limit number "Team PROJECT.md limit (chars)"
     "Maximum size of a repository's shared .pai/memory/PROJECT.md")
    (:ltm-injection choice "Memory in the prompt"
     "full: every entry; retrieval: pinned entries plus the best few, the rest through search"
     ("full" "retrieval"))
    (:retrieval-entries number "Retrieval: entries shown"
     "In retrieval mode, how many unpinned entries (best confidence and recency) the prompt gets")
    (:project-index-chars number "Project topic index (chars)"
     "How much of the project topic index goes into the prompt")
    (:memory-tool-policy choice "Memory tool"
     "direct: the agent's memory tool writes at once (logged, undoable); propose: queue for review"
     ("direct" "propose"))
    (:review-policy choice "Review policy"
     "all: every proposal waits for you; skills: memory entries apply automatically; none: everything applies (risky ones still wait)"
     ("all" "skills" "none"))
    (:promote-min-new-tokens number "Promote after new tokens"
     "After consolidation, promote once the session digest grew by this much")
    (:promote-min-session-tokens number "Min session tokens"
     "Sessions shorter than this are not promoted automatically")
    (:discovery-min-failures number "Failures for a discovery"
     "A method the assistant found by trial and error may become a skill when this many tool calls failed on the way (0: never)")
    (:learn-snippets boolean "Learn prompt snippets"
     "Propose prompt snippets (see /snippets) from instructions you keep adding to your messages")
    (:max-snippet-chars number "Max snippet size (chars)"
     "Largest instruction text a learned prompt snippet may have")
    (:promote-transcript-tokens number "Transcript digest tokens"
     "Without session memory, how much of the transcript the promoter reads")
    (:promoter-timeout number "Promoter timeout (s)"
     "Seconds before a running promoter is stopped")
    (:curator-interval-days number "Curator every (days)"
     "How often the curator checks learned skills")
    (:stale-after-sessions number "Stale after (sessions)"
     "Learned skills unused for this many sessions are marked stale (project skills count their project's sessions)")
    (:stale-after-days number "…and at least (days)"
     "Minimum days before a skill can go stale, so a burst of short sessions cannot do it")
    (:archive-after-sessions number "Archive after (sessions)"
     "Learned skills unused for this many sessions move to the archive (restorable)")
    (:archive-after-days number "…and at least (days)"
     "Minimum days before a skill can be archived")
    (:merge-interval-days number "Merge skills every (days)"
     "Look for overlapping learned skills this often (blank = only /memory merge)")
    (:merge-threshold number "Merge overlap threshold"
     "How similar two learned skills must be (0-1) to be offered to the merger")
    (:merger-timeout number "Merger timeout (s)" "Seconds before a running merger is stopped")
    (:auto-test boolean "Test new skills automatically"
     "Test-run each new skill proposal that has a check and no security findings, without asking (its bash commands are not sandboxed)")
    (:evaluator-timeout number "Skill check timeout (s)"
     "Seconds before a running skill test-run (t in /memory-review) is stopped")
    (:commit-project-skills choice "Project skills in git"
     "ask: offer to git add accepted project skills (never commits); never: don't" ("ask" "never"))
    (:max-skill-chars number "Skill body limit (chars)"
     "Longer skill bodies get a style finding: move detail to references/")
    (:patch-threshold number "Review after deviations/failures"
     "Deviated + failed uses since a skill last changed that send it to the promoter"))
  "Long-term knobs in /menu: (KEY TYPE LABEL DOC [CHOICES]).")

(defun pai-memory--menu-item (subsection group key type label doc &optional choices)
  "Register the /menu item for knob KEY of GROUP under SUBSECTION.
A blank number restores the built-in default."
  (pai-settings-ui-register-item
   'memory subsection
   :key (intern (format ":memory-%s" (substring (symbol-name key) 1)))
   :type type :label label :doc doc
   :choices choices
   :get (pcase type
          ('boolean (lambda () (pai-truthy (pai-memory-get group key))))
          ('choice (lambda () (format "%s" (pai-memory-get group key))))
          (_ (lambda () (pai-memory-get group key))))
   :set (pcase type
          ('boolean (lambda (v) (pai-memory-set group key (if v t :false))))
          ('number (lambda (v) (if v (pai-memory-set group key v) (pai-memory-unset group key))))
          ('string (lambda (v) (if (or (null v) (string-empty-p (string-trim v)))
                                   (pai-memory-unset group key)
                                 (pai-memory-set group key (string-trim v)))))
          (_ (lambda (v) (pai-memory-set group key v))))))

(with-eval-after-load 'pai-settings-ui
  (pai-settings-ui-register-section 'memory "Memory" 45)
  (pai-settings-ui-register-subsection 'memory 'general "General" 10)
  (pai-settings-ui-register-item
   'memory 'general :key :memory-preset :type 'choice :label "Preset"
   :doc "How often memory calls a model: off, economy, balanced, thorough, custom"
   :choices (lambda () (mapcar #'symbol-name pai-memory-preset-names))
   :get (lambda () (symbol-name (pai-memory-preset)))
   :set (lambda (v) (pai-memory-set-preset (intern v))))
  (pai-settings-ui-register-item
   'memory 'general :key :memory-session :type 'boolean :label "Session memory"
   :doc "Observers + observational compaction (per-session default)"
   :get (lambda () (pai-truthy (pai-memory-get :session :enabled)))
   :set (lambda (v) (pai-memory-set :session :enabled (if v t :false))))
  (pai-settings-ui-register-item
   'memory 'general :key :memory-long-term :type 'boolean :label "Long-term learning"
   :doc "Long-term memory and skill proposals"
   :get (lambda () (pai-truthy (pai-memory-get :long-term :enabled)))
   :set (lambda (v) (pai-memory-set :long-term :enabled (if v t :false))))
  (pai-settings-ui-register-item
   'memory 'general :key :memory-session-usd :type 'number :label "Budget $/session"
   :doc "Background spend cap per session (blank = none)"
   :get (lambda () (pai-memory-get :budget :session-usd))
   :set (lambda (v) (pai-memory-set :budget :session-usd v)))
  (pai-settings-ui-register-item
   'memory 'general :key :memory-daily-usd :type 'number :label "Budget $/day"
   :doc "Background spend cap per day (blank = none)"
   :get (lambda () (pai-memory-get :budget :daily-usd))
   :set (lambda (v) (pai-memory-set :budget :daily-usd v)))
  (pai-settings-ui-register-item
   'memory 'general :key :memory-session-tokens :type 'number :label "Budget tokens/session"
   :doc "Billable-token cap per session; the only cap for models without prices"
   :get (lambda () (pai-memory-get :budget :session-tokens))
   :set (lambda (v) (pai-memory-set :budget :session-tokens v)))
  (pai-settings-ui-register-item
   'memory 'general :key :memory-daily-tokens :type 'number :label "Budget tokens/day"
   :doc "Billable-token cap per day"
   :get (lambda () (pai-memory-get :budget :daily-tokens))
   :set (lambda (v) (pai-memory-set :budget :daily-tokens v)))
  (pai-settings-ui-register-subsection 'memory 'advanced "Advanced: session memory" 20)
  (dolist (spec pai-memory-menu-session-items)
    (apply #'pai-memory--menu-item 'advanced :session spec))
  (pai-settings-ui-register-subsection 'memory 'long-term "Advanced: long-term memory" 30)
  (dolist (spec pai-memory-menu-long-term-items)
    (apply #'pai-memory--menu-item 'long-term :long-term spec))
  (pai-settings-ui-register-subsection 'memory 'search "Search" 40)
  (dolist (spec '((:enabled boolean "Search index" "Full-text index of sessions and memory for memory_search")
                  (:max-mb number "Index size limit (MB)" "Indexing stops when the index reaches this size")
                  (:tool-result-chars number "Tool result chars indexed" "How much of each tool result is searchable")
                  (:recall boolean "Automatic recall" "Add related notes from earlier sessions to each prompt (no model call)")
                  (:recall-hits number "Recall hits" "How many notes automatic recall adds")
                  (:recall-chars number "Recall chars per hit" "Maximum length of each recalled note")
                  (:embedder string "Semantic search: embedder"
                   "Blank = off.  \"openai\" = any OpenAI-compatible /v1/embeddings endpoint (local servers too), or a name registered with pai-memory-register-embedder")
                  (:embed-url string "Embedding URL" "e.g. http://localhost:8080/v1/embeddings")
                  (:embed-model string "Embedding model" "Model name sent to the endpoint")
                  (:embed-key-env string "API key variable" "Environment variable holding the API key (blank for none)")
                  (:embed-batch number "Embedding batch size" "Texts per background request")
                  (:embed-timeout number "Query embedding timeout (s)" "How long a search waits for the query vector before falling back to full text")
                  (:embed-daily-tokens number "Embedding tokens per day" "Background and query embedding stop for the day at this many tokens")
                  (:embed-scan number "Vector candidates" "Newest embedded notes compared with each query (cost grows with this)")
                  (:embed-min-similarity number "Minimum similarity" "Cosine similarity (0-1) a note needs to count as a semantic match; depends on the embedding model")))
    (apply #'pai-memory--menu-item 'search :search spec))
  (pai-settings-ui-register-item
   'memory 'long-term :key :memory-promote :type 'choice :label "Promote when"
   :doc "When the promoter runs automatically (a preset knob); manual = only /learn and /memory promote"
   :choices '("consolidation+session-end" "session-end" "consolidation" "manual")
   :get (lambda () (let ((ts (pai-memory-promote-triggers)))
                     (if (and (memq 'consolidation ts) (memq 'session-end ts))
                         "consolidation+session-end"
                       (symbol-name (or (car ts) 'manual)))))
   :set (lambda (v) (pai-memory-set :long-term :promote
                                    (vconcat (split-string v "\\+")))))
  (pai-settings-ui-register-item
   'memory 'long-term :key :memory-provider :type 'choice :label "External provider"
   :doc "Extra long-term memory provider next to the Markdown files (none = Markdown only)"
   :choices (lambda () (cons "none" (mapcar #'car pai-memory--providers)))
   :get (lambda () (or (pai-memory-get :long-term :provider) "none"))
   :set (lambda (v) (pai-memory-set :long-term :provider (if (equal v "none") nil v)))))

;;;; Extension entry point

(pai-memory-worker-register-model-roles)

(defun pai-memory-extension (api)
  "Register pai-memory's handlers and commands on extension API."
  (pai-ext-on api 'agent-settled #'pai-memory--on-settled)
  (pai-ext-on api 'session-start #'pai-memory--on-session-start)
  (pai-ext-on api 'session-compact #'pai-memory--on-refresh)
  (pai-ext-on api 'turn-end #'pai-memory--on-turn-end) ; workers + gauges mid-run
  (pai-ext-on api 'session-tree #'pai-memory--on-refresh)
  (pai-ext-on api 'reload #'pai-memory--on-refresh)
  (pai-ext-on api 'compact #'pai-memory-compact-handler)
  (pai-ext-on api 'system-prompt-sections #'pai-memory--system-sections)
  (pai-ext-on api 'session-shutdown #'pai-memory--on-session-end)
  (pai-ext-on api 'session-before-switch #'pai-memory--on-session-end)
  (pai-ext-register-command
   api "skills-export" :description "Export skills: /skills-export NAME...|--learned|--all DEST[.tar.gz]"
   :handler (pai-memory--in-pai-buffer
             (lambda (args)
               (let* ((words (split-string args nil t)) (dest (car (last words))) (names (butlast words)))
                 (if (or (null words) (null names)) "Usage: /skills-export NAME...|--learned|--all DEST"
                   (pai-memory-export-skills (cond ((equal names '("--learned")) 'learned)
                                                   ((equal names '("--all")) 'all)
                                                   (t names))
                                             dest))))))
  (pai-ext-register-command
   api "skills-import" :description "Import skills as proposals: /skills-import DIR|FILE.tar.gz|GIT-URL"
   :handler (pai-memory--in-pai-buffer
             (lambda (args)
               (if (string-empty-p (string-trim args)) "Usage: /skills-import DIR|FILE.tar.gz|GIT-URL"
                 (pai-memory-import-skills (string-trim args) default-directory)))))
  (pai-ext-register-command
   api "memory-browse" :description "Browse what pai remembers: memory, topics, observations, skills"
   :handler (lambda (_args ctx)
              (let ((buf (plist-get ctx :buffer)))
                (pai-memory-browse (and (buffer-live-p buf) buf)))
              nil))
  (pai-ext-register-command
   api "memory-review" :description "Review pending memory and skill proposals"
   :handler (lambda (_args _ctx) (pai-memory-review) nil))
  (pai-ext-on api 'tool-execution-start #'pai-memory--on-tool-start)
  (pai-ext-on api 'agent-start #'pai-memory--on-agent-start)
  (pai-ext-on api 'input #'pai-memory--on-input)
  (pai-ext-register-command
   api "memory-pin" :description "Pin a skill (never archived) or a memory entry (always in the snapshot): /memory-pin NAME|QUOTE"
   :arg-completions #'pai-memory--skill-names
   :handler (pai-memory--skill-command (lambda (n) (pai-memory--pin n t))))
  (pai-ext-register-command
   api "memory-unpin" :description "Unpin a skill or memory entry: /memory-unpin NAME|QUOTE"
   :arg-completions #'pai-memory--skill-names
   :handler (pai-memory--skill-command (lambda (n) (pai-memory--pin n nil))))
  (pai-ext-register-command
   api "memory-restore-skill" :description "Bring back an archived skill: /memory-restore-skill NAME"
   :arg-completions (lambda (_p) (ignore-errors (pai-memory-archived-skills)))
   :handler (pai-memory--skill-command #'pai-memory-restore-skill))
  (pai-ext-register-command
   api "learn" :description "Capture a reusable skill: /learn [what] [--from URL|BUFFER|FILE|DIR ...]"
   :handler #'pai-memory-learn-command)
  (pai-ext-register-tool api pai-memory-tool-def)
  (pai-ext-register-tool api pai-memory-search-tool-def)
  (pai-ext-on api 'context #'pai-memory-recall-context-handler)
  (pai-ext-register-command
   api "memory"
   :description "Learning memory: status, observe, compact, show, undo, presets, budget"
   :arg-completions #'pai-memory--completions
   :arg-positional t
   :handler #'pai-memory-command))

(pai-register-extension #'pai-memory-extension "memory")

(provide 'pai-memory)
;;; pai-memory.el ends here
