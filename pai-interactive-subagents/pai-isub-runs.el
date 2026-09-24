;;; pai-isub-runs.el --- Child session bookkeeping and delivery -*- lexical-binding: t; -*-

;;; Commentary:

;; The backend-agnostic half of interactive subagents: it owns the parent's
;; list of child sessions, launches them through a backend, routes everything
;; a child says back into the parent's conversation, and renders the live
;; status block above the parent's prompt.
;;
;; A child never blocks the parent's UI.  A launch or follow-up returns a
;; receipt immediately (async, the default) and the child's answer arrives
;; later as a follow-up parent turn; with async false the tool call is kept
;; pending and completed when the child finishes that turn.
;;
;; Children are long-lived and interactive: after a turn they stay alive and
;; idle, the user can type into their buffer, and the parent can send more
;; work with the `say' action.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai)
(require 'pai-isub-backend)
(require 'pai-isub-roles)

(defvar pai--input-marker)
(defvar pai-prompt-string)

(defface pai-isub-status-face '((t :inherit font-lock-comment-face))
  "Face for the subagent status lines shown above the prompt."
  :group 'pai-isub)

(defvar-local pai-isub--runs nil
  "Child sessions owned by this buffer, newest first.
Each entry is a plist: (:id :role :backend :handle :task :status :model
:thinking :parent :on-done :awaiting :started :ended :last-output :max-lines
:timer :turns).")

(defvar-local pai-isub--ui-timer nil
  "Repeating timer refreshing the subagent status block in this buffer.")

(defvar-local pai-isub--overlay nil
  "Overlay rendering subagent status lines above this buffer's prompt.")

(defvar pai-isub--counter 0
  "Monotonic child run id counter (process-global, so ids stay unique).")

;;;; Entry bookkeeping

(defun pai-isub--record (entry)
  "Upsert ENTRY into the current buffer's run list, keyed by :id."
  (setq pai-isub--runs
        (cons entry (cl-remove (plist-get entry :id) pai-isub--runs
                               :key (lambda (e) (plist-get e :id)) :test #'equal))))

(defun pai-isub-run (id)
  "Return the run entry whose :id equals ID, or nil."
  (seq-find (lambda (e) (equal (plist-get e :id) id)) pai-isub--runs))

(defun pai-isub-run-by-prefix (prefix)
  "Return the newest run entry whose :id starts with PREFIX, or nil.
With PREFIX nil or empty, return the newest live run."
  (if (or (null prefix) (string-empty-p (string-trim prefix)))
      (seq-find #'pai-isub-live-p pai-isub--runs)
    (let ((prefix (string-trim prefix)))
      (seq-find (lambda (e) (string-prefix-p prefix (plist-get e :id)))
                pai-isub--runs))))

(defun pai-isub-entry-backend (entry)
  "Return ENTRY's backend plist, or nil when it is no longer registered."
  (pai-isub-backend (plist-get entry :backend)))

(defun pai-isub-live-p (entry)
  "Return non-nil while ENTRY's child session still exists."
  (let ((backend (pai-isub-entry-backend entry)))
    (and backend
         (not (member (plist-get entry :status) '("closed" "failed")))
         (pai-isub-backend-live-p backend (plist-get entry :handle)))))

(defun pai-isub-entry-buffer (entry)
  "Return the buffer showing ENTRY's child session, or nil."
  (let ((backend (pai-isub-entry-backend entry)))
    (and backend (pai-isub-backend-buffer backend (plist-get entry :handle)))))

;;;; Formatting helpers

(defun pai-isub--fmt-count (n)
  "Format token count N compactly (e.g. 1.2k, 45k, 1.2M)."
  (let ((n (max 0 (round (or n 0)))))
    (cond ((< n 1000) (number-to-string n))
          ((< n 100000) (format "%.1fk" (/ n 1000.0)))
          ((< n 1000000) (format "%dk" (round (/ n 1000.0))))
          (t (format "%.1fM" (/ n 1000000.0))))))

(defun pai-isub--fmt-duration (seconds)
  "Format elapsed SECONDS compactly (e.g. 12s, 3m04s)."
  (let ((s (max 0 (round (or seconds 0)))))
    (if (< s 60) (format "%ds" s)
      (format "%dm%02ds" (/ s 60) (% s 60)))))

(defun pai-isub--elapsed (entry)
  "Return ENTRY's wall-clock lifetime in seconds (frozen once :ended)."
  (max 0.001 (- (or (plist-get entry :ended) (float-time))
                (or (plist-get entry :started) (float-time)))))

(defun pai-isub--metrics (entry)
  "Return ENTRY's live metrics plist, or nil when the backend has none."
  (let ((backend (pai-isub-entry-backend entry)))
    (and backend (pai-isub-backend-metrics backend (plist-get entry :handle)))))

(defun pai-isub--one-line (text &optional width)
  "Return TEXT collapsed to one line, truncated to WIDTH (default 60)."
  (let* ((width (or width 60))
         (raw (string-trim (replace-regexp-in-string "[ \t\n]+" " " (or text "")))))
    (truncate-string-to-width raw width nil nil "…")))

;;;; Status display above the parent prompt

(defun pai-isub--status-glyph (entry)
  "Return a short glyph for ENTRY's status."
  (pcase (plist-get entry :status)
    ("running" "⛭")
    ("idle" "•")
    ("stopped" "◼")
    ("failed" "✗")
    ("closed" "·")
    (_ "?")))

(defun pai-isub--run-line (entry)
  "Return a single status line for run ENTRY."
  (let ((metrics (pai-isub--metrics entry)))
    (format "%s %-6s %-10s %-8s %6s%s · %s"
            (pai-isub--status-glyph entry)
            (plist-get entry :id)
            (pai-isub--one-line (plist-get entry :role) 10)
            (plist-get entry :status)
            (pai-isub--fmt-count (plist-get metrics :tokens))
            (if metrics (format "t · %4d tok/s" (or (plist-get metrics :tps) 0)) "")
            (pai-isub--fmt-duration (pai-isub--elapsed entry)))))

(defun pai-isub--shown-runs ()
  "Return the entries worth showing above the prompt, oldest first."
  (reverse (seq-filter #'pai-isub-live-p pai-isub--runs)))

(defun pai-isub--block-string (runs)
  "Return the multi-line overlay text for RUNS (one line each)."
  (propertize (concat (mapconcat #'pai-isub--run-line runs "\n") "\n")
              'face 'pai-isub-status-face))

(defun pai-isub--prompt-start ()
  "Return the position where this buffer's prompt begins, or nil."
  (when (and (boundp 'pai--input-marker) (markerp pai--input-marker)
             (marker-buffer pai--input-marker))
    (max (point-min)
         (- (marker-position pai--input-marker)
            (length (if (boundp 'pai-prompt-string) pai-prompt-string ""))))))

(defun pai-isub--remove-overlay ()
  "Delete this buffer's subagent overlay, if any."
  (when (overlayp pai-isub--overlay) (delete-overlay pai-isub--overlay))
  (setq pai-isub--overlay nil))

(defun pai-isub--render-overlay ()
  "Show live child sessions on their own lines just above the prompt."
  (let ((runs (pai-isub--shown-runs))
        (pos (pai-isub--prompt-start)))
    (if (and runs pos)
        (progn
          (unless (overlayp pai-isub--overlay)
            (setq pai-isub--overlay (make-overlay pos pos nil t nil)))
          (move-overlay pai-isub--overlay pos pos)
          (overlay-put pai-isub--overlay 'before-string (pai-isub--block-string runs)))
      (pai-isub--remove-overlay))))

(defun pai-isub--display (buffer)
  "Refresh BUFFER's status block; return non-nil while it has live children."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (pai-isub--render-overlay)
         (and (pai-isub--shown-runs) t))))

(defun pai-isub--busy-p (buffer)
  "Return non-nil when BUFFER has a child mid-turn (so the block must tick)."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (seq-find (lambda (e) (equal (plist-get e :status) "running"))
                        pai-isub--runs)
              t))))

(defun pai-isub--ensure-timer (buffer)
  "Start BUFFER's status refresh timer unless one is already live.
The timer only ticks while a child is mid-turn; idle sessions are redrawn on
their own events, so a parked subagent costs nothing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (timerp pai-isub--ui-timer)
        (let (timer)
          (setq timer
                (run-at-time
                 0.5 0.5
                 (lambda ()
                   (let ((live (pai-isub--display buffer)))
                     (unless (and live (pai-isub--busy-p buffer))
                       (cancel-timer timer)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (setq pai-isub--ui-timer nil))))))))
          (setq pai-isub--ui-timer timer))))))

(defun pai-isub-ui-refresh (buffer)
  "Update BUFFER's status block now, ticking while a child is mid-turn."
  (when (and (pai-isub--display buffer) (pai-isub--busy-p buffer))
    (pai-isub--ensure-timer buffer)))

;;;; Delivery into the parent conversation

(defun pai-isub--notice (entry kind text)
  "Return the parent-facing message for KIND and TEXT from ENTRY."
  (format "[subagent %s (%s) %s]\n%s\n\nReply with the `subagent' tool (action \"say\", id \"%s\") to continue this session, or ignore it."
          (plist-get entry :id) (plist-get entry :role)
          (pcase kind
            ('reply "says")
            ('turn-end "finished a turn")
            ('error "failed")
            ('exit "session ended")
            (_ (format "%s" kind)))
          (if (string-empty-p (string-trim (or text ""))) "(no output)" text)
          (plist-get entry :id)))

(defun pai-isub--complete-pending (entry text is-error)
  "Complete ENTRY's pending foreground tool call with TEXT.
Return non-nil when a call was waiting.  IS-ERROR marks it as a failure."
  (let ((on-done (plist-get entry :on-done)))
    (when on-done
      (plist-put entry :on-done nil)
      (funcall on-done (list :content (list (pai-text text))
                             :is-error (if is-error t :false)))
      t)))

(defun pai-isub--deliver (entry kind text)
  "Deliver TEXT of KIND from ENTRY into its parent conversation."
  (let ((parent (plist-get entry :parent)))
    (when (buffer-live-p parent)
      (with-current-buffer parent
        (let* ((max-lines (plist-get entry :max-lines))
               (body (plist-get (pai-tools-truncate (or text "") max-lines nil 'head)
                                :text)))
          (plist-put entry :last-output body)
          (pai-isub--record entry)
          (unless (and (memq kind '(turn-end error exit))
                       (pai-isub--complete-pending entry body (eq kind 'error)))
            (let ((notice (pai-isub--notice entry kind body)))
              (cond
               ((and (boundp 'pai--active) pai--active)
                (push (pai-user-message notice) pai--steering-queue)
                (pai--render-note
                 (format "[subagent %s] %s (queued for this turn)"
                         (plist-get entry :id) (pai-isub--one-line body))))
               ((fboundp 'pai--start-run)
                (pai--render-note
                 (format "[subagent %s] %s" (plist-get entry :id)
                         (pai-isub--one-line body)))
                (condition-case err (pai--start-run notice)
                  (error (message "pai-isub: %s" (error-message-string err)))))
               (t nil))))
          (pai-isub-ui-refresh parent))))))

;;;; Child events

(defun pai-isub--clear-timer (entry)
  "Cancel ENTRY's pending turn timeout, if any."
  (let ((timer (plist-get entry :timer)))
    (when (timerp timer) (cancel-timer timer))
    (plist-put entry :timer nil)))

(defun pai-isub--report-all-p (parent)
  "Return non-nil when PARENT wants every child turn reported."
  (and (buffer-live-p parent)
       (with-current-buffer parent
         (pai-truthy (plist-get (pai-isub-config) :report-all-turns)))))

(defun pai-isub-on-child-event (entry type props)
  "Handle child event TYPE with PROPS for run ENTRY.
This is the single funnel every backend reports through."
  (let ((parent (plist-get entry :parent))
        (text (plist-get props :text)))
    (pcase type
      ('ready
       (plist-put entry :status "idle"))
      ('busy
       (plist-put entry :status "running"))
      ('status
       (plist-put entry :last-output text))
      ('reply
       (pai-isub--deliver entry 'reply text))
      ('turn-end
       (pai-isub--clear-timer entry)
       (plist-put entry :status "idle")
       (plist-put entry :turns (1+ (or (plist-get entry :turns) 0)))
       (let ((awaiting (plist-get entry :awaiting)))
         (plist-put entry :awaiting nil)
         (if (or awaiting (plist-get entry :on-done)
                 (pai-isub--report-all-p parent))
             (pai-isub--deliver entry 'turn-end text)
           (plist-put entry :last-output text))))
      ('error
       ;; An interrupted or failed turn does not necessarily end the session:
       ;; keep it usable when its buffer is still there.
       (pai-isub--clear-timer entry)
       (plist-put entry :awaiting nil)
       (plist-put entry :status "idle")
       (pai-isub--deliver entry 'error text)
       (unless (pai-isub-live-p entry)
         (plist-put entry :status "failed")
         (plist-put entry :ended (float-time))))
      ('exit
       (pai-isub--clear-timer entry)
       (plist-put entry :status "closed")
       (plist-put entry :ended (float-time))
       (when (or (plist-get entry :awaiting) (plist-get entry :on-done))
         (plist-put entry :awaiting nil)
         (pai-isub--deliver entry 'exit
                            (or text "the subagent session was closed")))))
    (when (buffer-live-p parent)
      (with-current-buffer parent
        (pai-isub--record entry)
        (pai-isub-ui-refresh parent)))
    entry))

;;;; Launch / talk / stop

(defun pai-isub--inherited-messages (context-mode)
  "Return the parent messages a child should inherit for CONTEXT-MODE."
  (when (and (eq context-mode 'fork) (boundp 'pai--context-messages))
    (seq-filter (lambda (m) (not (pai-system-message-p m))) pai--context-messages)))

(defun pai-isub--arm-timeout (entry timeout)
  "Arm a TIMEOUT (seconds) for the turn ENTRY is currently awaiting."
  (when (and (numberp timeout) (> timeout 0))
    (plist-put
     entry :timer
     (run-at-time
      timeout nil
      (lambda ()
        (when (plist-get entry :awaiting)
          (let ((backend (pai-isub-entry-backend entry)))
            (when backend
              (ignore-errors
                (pai-isub-backend-interrupt backend (plist-get entry :handle)))))
          (plist-put entry :awaiting nil)
          (pai-isub-on-child-event
           entry 'error (list :text "the subagent exceeded its timeout"))))))))

(defun pai-isub-launch (role task args ctx on-done)
  "Start a child session for ROLE with TASK; return the run entry.
ARGS is the tool call's argument plist, CTX the tool context, and ON-DONE the
continuation of a foreground (async false) call, or nil."
  (let* ((parent (current-buffer))
         (role-def (pai-isub-role role)))
    (unless role-def
      (error "Unknown subagent role %s; see /subagents-roles" role))
    (let* ((backend-name (pai-isub-resolve-backend role (plist-get args :backend)))
           (backend (pai-isub-backend backend-name))
           (resolved (pai-isub-resolve-model role (plist-get args :model)
                                             (plist-get ctx :model)))
           (context-mode (pai-isub-role-context-mode role (plist-get args :context)))
           (id (format "sub-%d" (cl-incf pai-isub--counter))))
      (unless backend
        (error "Unknown subagent backend %s; known: %s" backend-name
               (string-join (pai-isub-backend-names) ", ")))
      (unless (or resolved (not (equal backend-name "pai")))
        (error "No model resolved for role %s; set one with /subagents-model" role))
      (let ((entry (list :id id :role role :backend backend-name :handle nil
                         :task task :status "starting"
                         :model (and resolved (pai-model-key (car resolved)))
                         :thinking (and resolved (cadr resolved))
                         :parent parent :on-done on-done :awaiting t
                         :started (float-time) :ended nil :last-output nil
                         :max-lines (plist-get args :max-lines)
                         :timer nil :turns 0)))
        (pai-isub--record entry)
        (plist-put
         entry :handle
         (pai-isub-backend-start
          backend
          (list :id id :role role
                :role-prompt (plist-get role-def :prompt)
                :description (plist-get role-def :description)
                :parent parent :cwd (or (plist-get ctx :cwd) default-directory)
                :model (car resolved) :thinking (cadr resolved)
                :tools (pai-isub-role-tools role)
                :context-messages (pai-isub--inherited-messages context-mode)
                :emit (lambda (type &rest props)
                        (pai-isub-on-child-event entry type props)))))
        (pai-isub--record entry)
        (pai-isub--arm-timeout entry (plist-get args :timeout))
        (pai-isub-backend-send backend (plist-get entry :handle)
                               (pai-isub--task-message entry task))
        (pai-isub-ui-refresh parent)
        entry))))

(defun pai-isub--task-message (entry task)
  "Return the text delivered to ENTRY's child for TASK."
  (format "Task from the parent agent (you are subagent %s, role %s):\n\n%s"
          (plist-get entry :id) (plist-get entry :role) task))

(defun pai-isub--say-message (entry text)
  "Return the text delivered to ENTRY's child for parent message TEXT."
  (format "Message from the parent agent (subagent %s):\n\n%s"
          (plist-get entry :id) text))

(defun pai-isub-say (entry text args on-done)
  "Send TEXT from the parent to ENTRY's child session.
ARGS is the tool call's argument plist; ON-DONE, when non-nil, keeps the tool
call pending until the child finishes the resulting turn."
  (let ((backend (pai-isub-entry-backend entry)))
    (unless (and backend (pai-isub-live-p entry))
      (error "Subagent %s is no longer running" (plist-get entry :id)))
    (when (and on-done (plist-get entry :on-done))
      (error "Subagent %s already has a foreground call pending" (plist-get entry :id)))
    (pai-isub--clear-timer entry)
    (plist-put entry :awaiting t)
    (when on-done (plist-put entry :on-done on-done))
    (when (plist-member args :max-lines)
      (plist-put entry :max-lines (plist-get args :max-lines)))
    (pai-isub--arm-timeout entry (plist-get args :timeout))
    (pai-isub-backend-send backend (plist-get entry :handle)
                           (pai-isub--say-message entry text))
    (pai-isub--record entry)
    (pai-isub-ui-refresh (plist-get entry :parent))
    entry))

(defun pai-isub-stop (entry)
  "Interrupt the turn ENTRY's child is running; keep the session alive."
  (when entry
    (let ((backend (pai-isub-entry-backend entry)))
      (pai-isub--clear-timer entry)
      (when backend
        (ignore-errors (pai-isub-backend-interrupt backend (plist-get entry :handle))))
      (plist-put entry :status "idle")
      (plist-put entry :awaiting nil)
      (pai-isub--complete-pending entry "The subagent turn was interrupted." t)
      (pai-isub--record entry)
      (pai-isub-ui-refresh (plist-get entry :parent))
      t)))

(defun pai-isub-close (entry)
  "Close ENTRY's child session entirely."
  (when entry
    (let ((backend (pai-isub-entry-backend entry)))
      (pai-isub--clear-timer entry)
      (plist-put entry :awaiting nil)
      (pai-isub--complete-pending entry "The subagent session was closed." nil)
      (when backend
        (ignore-errors (pai-isub-backend-close backend (plist-get entry :handle))))
      (plist-put entry :status "closed")
      (plist-put entry :ended (float-time))
      (pai-isub--record entry)
      (pai-isub-ui-refresh (plist-get entry :parent))
      t)))

;;;; Status text

(defun pai-isub-status-text (&optional id)
  "Return one line per child session, optionally filtered by id prefix ID."
  (let ((runs (if (and id (not (string-empty-p id)))
                  (seq-filter (lambda (e) (string-prefix-p id (plist-get e :id)))
                              pai-isub--runs)
                pai-isub--runs)))
    (if (null runs)
        "No subagent sessions"
      (string-join
       (mapcar
        (lambda (e)
          (let ((buffer (pai-isub-entry-buffer e))
                (metrics (pai-isub--metrics e)))
            (format "%-6s %-10s %-6s %-8s %-18s %6s %6s  %s"
                    (plist-get e :id)
                    (pai-isub--one-line (plist-get e :role) 10)
                    (plist-get e :backend)
                    (plist-get e :status)
                    (pai-isub--one-line (or (plist-get e :model) "-") 18)
                    (pai-isub--fmt-count (plist-get metrics :tokens))
                    (pai-isub--fmt-duration (pai-isub--elapsed e))
                    (pai-isub--one-line
                     (or (plist-get e :last-output)
                         (and (buffer-live-p buffer) (buffer-name buffer))
                         (plist-get e :task))
                     50))))
        runs)
       "\n"))))

(provide 'pai-isub-runs)
;;; pai-isub-runs.el ends here
