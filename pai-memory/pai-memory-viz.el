;;; pai-memory-viz.el --- Visualising the memory pipeline -*- lexical-binding: t; -*-

;;; Commentary:

;; How the memory pipeline is drawn, modelled on pi-observational-memory:
;;
;;   * Workers above the prompt, one line each: a status mark, then id,
;;     role, output tokens, tok/s, elapsed time and what it works on.
;;     Finished ones linger a few seconds with their result
;;     (+N observations, +N filed, +N proposals) or why they stopped:
;;
;;       🧠 ◐ obs-2   observer        310t ·   42 tok/s · 7s · 8.4k tokens of transcript
;;       🧠 ✓ con-1   consolidator   1.2kt ·   30 tok/s · 41s · 12 observations → topics · +12
;;       🧠 ✗ pro-1   promoter        900t ·   15 tok/s · 1m00s · consolidation · timeout
;;
;;   * Footer gauges: how full each clock is toward its next firing:
;;
;;       O▕███░░░░░▏ C▕██████░░▏ X▕██░░░░░░▏
;;
;;     O unobserved transcript toward the next observer chunk,
;;     C observation pool toward the consolidator threshold,
;;     X live context toward the compaction threshold.
;;     A full bar turns to the warning face.
;;
;;   * /memory status: the same gauges with numbers, plus a promoter gauge
;;     and a timeline strip of the whole branch, one cell per observed batch
;;     (about `:chunk-tokens' each):
;;
;;       ▓▓▓▚▒▒◆▒◌◌░┊░▶
;;       ▓ filed into topics  ▚ partly filed  ▒ in the pool  ◌ being observed
;;       ░ not observed yet  ┊ compaction cut  ◆ promotion  ▶ now
;;
;; Everything here only reads state; nothing changes the ledger.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-session)
(require 'pai-compaction)
(require 'pai-activity)
(require 'pai-memory-settings)
(require 'pai-memory-ledger)

(defvar pai--session)
(defvar pai--model)
(defvar pai--context-messages)
(defvar pai-memory--in-flight)

;;;; Faces and knobs

(defgroup pai-memory-viz nil
  "How the memory pipeline is drawn."
  :group 'pai)

(defface pai-memory-viz-gauge '((t :inherit shadow))
  "Face of a gauge that is not full yet.")

(defface pai-memory-viz-gauge-full '((t :inherit warning))
  "Face of a full gauge: its clock fires (or waits to fire).")

(defface pai-memory-viz-running '((t :inherit font-lock-keyword-face))
  "Face of a running worker's spinner.")

(defface pai-memory-viz-done '((t :inherit success))
  "Face of a worker that finished well.")

(defface pai-memory-viz-failed '((t :inherit error))
  "Face of a worker that failed, timed out or was stopped.")

(defcustom pai-memory-viz-settle-seconds 5
  "Seconds a finished worker stays above the prompt."
  :type 'number :group 'pai-memory-viz)

(defcustom pai-memory-viz-gauge-cells 8
  "Cells of a footer gauge."
  :type 'integer :group 'pai-memory-viz)

(defcustom pai-memory-viz-timeline-width nil
  "Cells per row of the /memory status timeline.
nil fits the rows to the width of the window showing the pai buffer."
  :type '(choice (const :tag "Fit the window" nil) integer) :group 'pai-memory-viz)

(defun pai-memory-viz--timeline-width ()
  "Return the cells per timeline row for the current buffer.
Fits the window showing it (less the indent and the tip), 60 without a window."
  (or pai-memory-viz-timeline-width
      (let ((win (get-buffer-window (current-buffer) t)))
        (if win (max 20 (- (window-body-width win) 4)) 60))))

(defconst pai-memory-viz-spinner ["◐" "◓" "◑" "◒"]
  "Spinner frames of a running worker.")

(defconst pai-memory-viz-glyphs
  '((filed . "▓") (partial . "▚") (pool . "▒") (observing . "◌")
    (raw . "░") (cut . "┊") (promoted . "◆") (tip . "▶"))
  "Timeline glyphs.")

(defun pai-memory-viz--glyph (kind)
  "Return the timeline glyph of KIND."
  (alist-get kind pai-memory-viz-glyphs))

;;;; Gauges

(defun pai-memory-viz-bar (value max &optional cells)
  "Return a fill bar of CELLS (default `pai-memory-viz-gauge-cells') for VALUE of MAX."
  (let* ((cells (or cells pai-memory-viz-gauge-cells))
         (frac (if (and max (> max 0)) (max 0.0 (/ (float value) max)) 0.0))
         (filled (min cells (round (* (min 1.0 frac) cells)))))
    (propertize (concat "▕" (make-string filled ?█) (make-string (- cells filled) ?░) "▏")
                'face (if (>= frac 1.0) 'pai-memory-viz-gauge-full 'pai-memory-viz-gauge))))

(defun pai-memory-viz-context-tokens ()
  "Return (TOKENS . THRESHOLD) of the current buffer's live context, or nil."
  (let* ((model (and (boundp 'pai--model) pai--model))
         (window (and model (plist-get model :context-window)))
         (threshold (and window (> window 0) (pai-compaction-threshold-tokens window))))
    (when (and threshold (> threshold 0))
      (cons (pai-estimate-context-tokens (and (boundp 'pai--context-messages)
                                              pai--context-messages))
            threshold))))

(defun pai-memory-viz-in-flight-ranges ()
  "Return the current buffer's in-flight observer ranges as (FROM-ID . TO-ID)."
  (mapcar (lambda (r) (cons (plist-get r :from) (plist-get r :to)))
          (and (boundp 'pai-memory--in-flight) pai-memory--in-flight)))

(defun pai-memory-viz-gauges (session &optional branch)
  "Return SESSION's clock gauges in the current buffer.
A plist of (VALUE . MAX) conses: :next (unobserved tokens toward a chunk),
:pool (pool tokens toward consolidation), :ctx (context toward compaction,
nil when unknown).  BRANCH defaults to SESSION's current branch."
  (let* ((branch (or branch (pai-session-get-branch session)))
         (runs (pai-memory-unobserved-runs branch (pai-memory-viz-in-flight-ranges))))
    (list :next (cons (pai-memory-unobserved-tokens runs)
                      (or (pai-memory-get :session :chunk-tokens session) 8000))
          :pool (cons (pai-memory-observation-tokens (pai-memory-pool branch))
                      (or (pai-memory-get :session :consolidate-at-pool-tokens session) 20000))
          :ctx (pai-memory-viz-context-tokens))))

(defun pai-memory-viz--gauge (letter gauge what)
  "Return footer gauge LETTER for GAUGE (VALUE . MAX) with WHAT as its tooltip."
  (when gauge
    (propertize (concat (propertize letter 'face 'shadow)
                        (pai-memory-viz-bar (car gauge) (cdr gauge)))
                'help-echo (format "%s: %s / %s tokens" what
                                   (pai-activity-fmt-count (car gauge))
                                   (pai-activity-fmt-count (cdr gauge))))))

(defun pai-memory-viz-footer-gauges (session)
  "Return SESSION's footer gauges \"O▕…▏ C▕…▏ X▕…▏\" for the current buffer."
  (let ((g (pai-memory-viz-gauges session)))
    (string-join
     (delq nil
           (list (pai-memory-viz--gauge "O" (plist-get g :next) "Observer: unobserved transcript toward the next chunk")
                 (and (pai-truthy (pai-memory-get :session :consolidate session))
                      (pai-memory-viz--gauge "C" (plist-get g :pool) "Consolidator: observation pool toward its threshold"))
                 (pai-memory-viz--gauge "X" (plist-get g :ctx) "Compaction: live context toward the threshold")))
     " ")))

;;;; Workers above the prompt

(defun pai-memory-viz--settling-p (entry now)
  "Return non-nil when ENTRY runs, or finished less than the settle time before NOW."
  (or (equal (plist-get entry :status) "running")
      (let ((ended (plist-get entry :ended)))
        (and ended (< (- now ended) pai-memory-viz-settle-seconds)))))

(defun pai-memory-viz-worker-mark (entry &optional now)
  "Return the status mark of activity ENTRY at time NOW: spinner, ✓ or ✗."
  (let ((now (or now (float-time))))
    (pcase (plist-get entry :status)
      ("running"
       (propertize (aref pai-memory-viz-spinner
                         (mod (floor (/ now (max 0.1 pai-activity-refresh-interval)))
                              (length pai-memory-viz-spinner)))
                   'face 'pai-memory-viz-running))
      ("completed" (propertize "✓" 'face 'pai-memory-viz-done))
      (_ (propertize "✗" 'face 'pai-memory-viz-failed)))))

(defun pai-memory-viz-worker-line (entry &optional now)
  "Return the line of memory worker ENTRY at time NOW.
The activity line (id, role, tokens, tok/s, elapsed, detail) led by a status
mark; a finished worker adds its result: +N, or why it did not complete."
  (let* ((status (plist-get entry :status))
         (delta (plist-get entry :delta))
         (line (pai-activity-line
                (plist-put (copy-sequence entry) :glyph
                           (concat "🧠 " (pai-memory-viz-worker-mark entry now))))))
    (add-face-text-property 0 (length line) 'pai-activity-face t line)
    (concat line
            (pcase status
              ("running" "")
              ("completed"
               (if (and (numberp delta) (> delta 0))
                   (propertize (format " · +%d" delta) 'face 'pai-memory-viz-done)
                 (propertize " · done" 'face 'pai-memory-viz-done)))
              (_ (propertize (format " · %s" status) 'face 'pai-memory-viz-failed))))))

(defun pai-memory-viz-workers-line (&optional now)
  "Return the memory worker lines for the current buffer at time NOW, or nil.
One line per running worker, plus workers that finished less than
`pai-memory-viz-settle-seconds' ago, oldest first."
  (let* ((now (or now (float-time)))
         (shown (seq-filter (lambda (e) (pai-memory-viz--settling-p e now))
                            (reverse (pai-activity-entries "memory")))))
    (when shown
      (mapconcat (lambda (e) (pai-memory-viz-worker-line e now)) shown "\n"))))

;; Memory workers are always shown above the prompt.
(setf (alist-get "memory" pai-activity-compact-renderers nil nil #'equal)
      #'pai-memory-viz-workers-line)

(defun pai-memory-viz-set-delta (entry n)
  "Record that worker ENTRY produced N things (shown as +N in the strip)."
  (when entry (plist-put entry :delta n)))

;;;; Timeline

(defun pai-memory-viz--cells (tokens chunk)
  "Return how many timeline cells TOKENS of transcript take at CHUNK tokens a cell."
  (max 1 (ceiling tokens (max 1 chunk))))

(defun pai-memory-viz-segments (branch chunk &optional in-flight)
  "Return BRANCH's timeline as a list of (INDEX GLYPH-KIND CELLS), in branch order.
CHUNK is the tokens per cell; IN-FLIGHT the (FROM-ID . TO-ID) ranges being
observed.  Batches take one cell each, glyph `filed', `partial' or `pool' by
how many of their observations the consolidator dropped; unobserved runs and
in-flight ranges take one cell per CHUNK tokens.  Compaction cuts and
promotions are zero-width marks (CELLS 0)."
  (let* ((index (pai-memory-index-map branch))
         (dropped (pai-memory-dropped-ids branch))
         (flying (delq nil (mapcar (lambda (r)
                                     (let ((from (gethash (car r) index))
                                           (to (gethash (cdr r) index)))
                                       (and from to (cons from to))))
                                   in-flight)))
         (entries (vconcat branch))
         (segs '()))
    (dolist (b (pai-memory-batches branch index))
      (let* ((obs (append (plist-get (plist-get b :data) :observations) nil))
             (n (seq-count (lambda (o) (gethash (plist-get o :id) dropped)) obs)))
        (push (list (plist-get b :from)
                    (cond ((or (null obs) (= n 0)) 'pool) ((= n (length obs)) 'filed) (t 'partial))
                    1)
              segs)))
    (dolist (r flying)
      (let ((tokens (cl-loop for i from (car r) to (cdr r)
                             sum (pai-memory-entry-tokens (aref entries i)))))
        (push (list (car r) 'observing (pai-memory-viz--cells tokens chunk)) segs)))
    (dolist (run (pai-memory-unobserved-runs branch in-flight))
      (push (list (car (car run)) 'raw
                  (pai-memory-viz--cells (pai-memory-slice-tokens run) chunk))
            segs))
    (let ((i 0))
      (dolist (e branch)
        (cond ((equal (plist-get e :type) "compaction")
               (let ((kept (gethash (plist-get e :firstKeptEntryId) index)))
                 (when kept (push (list kept 'cut 0) segs))))
              ((pai-memory--custom-p e "memory.promoted")
               (push (list i 'promoted 0) segs)))
        (setq i (1+ i))))
    ;; stable sort by position; at a tie the mark precedes the cells it starts
    (sort (nreverse segs)
          (lambda (a b) (or (< (car a) (car b))
                            (and (= (car a) (car b)) (= (nth 2 a) 0) (> (nth 2 b) 0)))))))

(defun pai-memory-viz-timeline (branch chunk &optional in-flight width)
  "Return the timeline block of BRANCH (header, strip, legend) as lines.
CHUNK is the tokens per cell, IN-FLIGHT the observer ranges and WIDTH the
cells per row (default `pai-memory-viz--timeline-width')."
  (let* ((width (or width (pai-memory-viz--timeline-width)))
         (segs (pai-memory-viz-segments branch chunk in-flight))
         (cells (mapcan (lambda (s)
                          (make-list (max 1 (nth 2 s)) (pai-memory-viz--glyph (nth 1 s))))
                        segs))
         (count (lambda (kind) (seq-count (lambda (s) (eq (nth 1 s) kind)) segs)))
         (raw (apply #'+ (mapcar #'pai-memory-entry-tokens branch)))
         (rows '()))
    (while cells
      (push (apply #'concat (seq-take cells width)) rows)
      (setq cells (nthcdr width cells)))
    (setq rows (nreverse rows))
    (if rows
        (setcar (last rows) (concat (car (last rows)) (pai-memory-viz--glyph 'tip)))
      (setq rows (list "(nothing yet)")))
    (append
     (list (format "Timeline · 1 cell ≈ %s tokens · %s tokens of transcript · %d compaction(s) · %d promotion(s)"
                   (pai-activity-fmt-count chunk) (pai-activity-fmt-count raw)
                   (funcall count 'cut) (funcall count 'promoted)))
     (mapcar (lambda (r) (concat "  " r)) rows)
     (list (format "  %s in topics (%d)  %s partly filed (%d)  %s pool (%d)  %s observing (%d)  %s unobserved  %s compaction  %s promotion  %s now"
                   (pai-memory-viz--glyph 'filed) (funcall count 'filed)
                   (pai-memory-viz--glyph 'partial) (funcall count 'partial)
                   (pai-memory-viz--glyph 'pool) (funcall count 'pool)
                   (pai-memory-viz--glyph 'observing) (funcall count 'observing)
                   (pai-memory-viz--glyph 'raw) (pai-memory-viz--glyph 'cut)
                   (pai-memory-viz--glyph 'promoted) (pai-memory-viz--glyph 'tip))))))

;;;; Status block

(defun pai-memory-viz--gauge-line (letter name gauge unit state)
  "Return a status line: NAME, gauge LETTER for GAUGE, UNIT text and STATE."
  (format "  %-12s %s%s %9s / %-6s %-26s %s"
          name letter
          (pai-memory-viz-bar (or (car gauge) 0) (or (cdr gauge) 0))
          (if gauge (pai-activity-fmt-count (car gauge)) "?")
          (if gauge (pai-activity-fmt-count (cdr gauge)) "?")
          unit state))

(defun pai-memory-viz--role-state (role)
  "Return \"N running\" for memory workers of ROLE in the current buffer, or nil."
  (let ((n (seq-count (lambda (e) (equal (plist-get e :label) role))
                      (pai-activity-running "memory"))))
    (and (> n 0) (format "%s %d running" (aref pai-memory-viz-spinner 0) n))))

(cl-defun pai-memory-viz-pipeline-lines (session &key promoter)
  "Return the /memory status pipeline block of SESSION in the current buffer.
PROMOTER, when non-nil, is (VALUE MAX STATE) for the promoter gauge."
  (let* ((branch (pai-session-get-branch session))
         (g (pai-memory-viz-gauges session branch))
         (chunk (or (pai-memory-get :session :chunk-tokens session) 8000)))
    (append
     (list "Pipeline:"
           (pai-memory-viz--gauge-line
            "O" "observer" (plist-get g :next) "unobserved → next chunk"
            (or (pai-memory-viz--role-state "observer")
                (format "observe: %s" (pai-memory-observe-mode session))))
           (pai-memory-viz--gauge-line
            "C" "consolidator" (plist-get g :pool) "pool → consolidation"
            (or (pai-memory-viz--role-state "consolidator")
                (if (pai-truthy (pai-memory-get :session :consolidate session)) "idle" "off")))
           (pai-memory-viz--gauge-line
            "X" "compaction" (plist-get g :ctx) "context → compaction" ""))
     (when promoter
       (list (pai-memory-viz--gauge-line
              "P" "promoter" (cons (nth 0 promoter) (nth 1 promoter)) "new digest → promotion"
              (or (pai-memory-viz--role-state "promoter") (nth 2 promoter)))))
     (pai-memory-viz-timeline branch chunk (pai-memory-viz-in-flight-ranges)))))

(provide 'pai-memory-viz)
;;; pai-memory-viz.el ends here
