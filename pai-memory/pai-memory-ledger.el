;;; pai-memory-ledger.el --- Observation ledger for pai-memory -*- lexical-binding: t; -*-

;;; Commentary:

;; The observation ledger (SPEC §5.1) lives in the session JSONL as custom
;; entries, chained like every other entry, so it is branch-local for free:
;;
;;   memory.observations  {runId, coversFromId, coversUpToId,
;;                         observations: [{id, timestamp, content}]}
;;   memory.dropped       {runId, ids: [...]}            (consolidator, Phase 2)
;;
;; Everything here is a pure function of a branch -- the list of entries from
;; the root to the leaf, as `pai-session-get-branch' returns it -- so the fold
;; is deterministic and testable without a live buffer.
;;
;; Terms:
;;   source entry  an entry that becomes an LLM message (user, assistant, tool
;;                 result, custom message), except the system prompt;
;;   batch         one committed `memory.observations' entry, covering the
;;                 source entries from coversFromId to coversUpToId;
;;   watermark     the last source entry of the *contiguous* covered prefix;
;;                 batches may commit out of order, and a gap stops it;
;;   pool          the observations of committed batches not yet dropped.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-memory-injected)
(require 'pai-session)
(require 'pai-compaction)

;;;; Entries

(defvar pai-memory--entry-message-cache (make-hash-table :test 'eq :weakness 'key)
  "Messages of session entries: ENTRY -> (SOURCE . MESSAGE).
The ledger walks the whole branch several times per turn (gauges, observer
slices, cuts); converting an entry copies and re-symbolizes its content, so
the result is kept per entry object (weakly).  SOURCE, the entry's raw
`:message' or `:content', must still be the same object for a hit.")

(defun pai-memory-entry-message (entry)
  "Return the LLM message ENTRY becomes, or nil (system prompt excluded).
Memoized per entry (see `pai-memory--entry-message-cache'); callers must
not modify the returned message."
  (let* ((source (or (plist-get entry :message) (plist-get entry :content)))
         (hit (gethash entry pai-memory--entry-message-cache)))
    (if (and hit (eq (car hit) source))
        (cdr hit)
      (let* ((m (pai-session--entry-to-message entry))
             (m (and m (not (pai-system-message-p m)) m)))
        (puthash entry (cons source m) pai-memory--entry-message-cache)
        m))))

(defun pai-memory-source-entry-p (entry)
  "Return non-nil when ENTRY is part of the observable transcript."
  (and (pai-memory-entry-message entry) t))

(defun pai-memory-entry-tokens (entry)
  "Return the estimated tokens of source ENTRY's message (0 for others)."
  (let ((m (pai-memory-entry-message entry)))
    (if m (pai-estimate-tokens m) 0)))

(defun pai-memory-valid-cut-p (entry)
  "Return non-nil when the verbatim tail may start at ENTRY.
A tail must not open with a tool result: its tool call would be cut off."
  (let ((m (pai-memory-entry-message entry)))
    (and m (not (eq (pai-message-role m) 'tool-result)))))

(defun pai-memory--custom-p (entry type)
  "Return non-nil when ENTRY is a custom entry of TYPE."
  (and (equal (plist-get entry :type) "custom")
       (equal (plist-get entry :customType) type)))

(defun pai-memory-index-map (branch)
  "Return a hash table from entry id to its index in BRANCH."
  (let ((h (make-hash-table :test 'equal)) (i 0))
    (dolist (e branch)
      (puthash (plist-get e :id) i h)
      (setq i (1+ i)))
    h))

;;;; Batches, coverage, pool

(defun pai-memory-batches (branch &optional index)
  "Return BRANCH's committed batches in commit order.
Each is (:from I :to J :entry-index K :data DATA): I and J index BRANCH.
Batches whose covered range is not on BRANCH are skipped.  INDEX is an
optional precomputed `pai-memory-index-map'."
  (let ((index (or index (pai-memory-index-map branch)))
        (k 0) (out '()))
    (dolist (e branch)
      (when (pai-memory--custom-p e "memory.observations")
        (let* ((d (plist-get e :data))
               (from (gethash (plist-get d :coversFromId) index))
               (to (gethash (plist-get d :coversUpToId) index)))
          (when (and from to (<= from to))
            (push (list :from from :to to :entry-index k :data d) out))))
      (setq k (1+ k)))
    (nreverse out)))

(defun pai-memory-dropped-ids (branch)
  "Return a hash set of observation ids dropped or forgotten on BRANCH.
`memory.dropped' comes from the consolidator, `memory.redacted' from
`/memory forget'."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e branch)
      (when (or (pai-memory--custom-p e "memory.dropped") (pai-memory--custom-p e "memory.redacted"))
        (dolist (id (append (plist-get (plist-get e :data) :ids) nil))
          (puthash id t h))))
    h))

(defun pai-memory-covered-p-vector (branch batches)
  "Return a bool vector: element I is non-nil when BRANCH entry I is covered."
  (let ((v (make-bool-vector (length branch) nil)))
    (dolist (b batches)
      (cl-loop for i from (plist-get b :from) to (plist-get b :to)
               do (aset v i t)))
    v))

(defun pai-memory-watermark (branch &optional batches)
  "Return the index of the last source entry of BRANCH's covered prefix, or nil.
Source entries are covered by BATCHES (default: BRANCH's batches); the first
uncovered source entry ends the prefix, so out-of-order commits leave no gap."
  (let* ((batches (or batches (pai-memory-batches branch)))
         (covered (pai-memory-covered-p-vector branch batches))
         (mark nil) (i 0))
    (catch 'gap
      (dolist (e branch)
        (when (pai-memory-source-entry-p e)
          (if (aref covered i) (setq mark i) (throw 'gap nil)))
        (setq i (1+ i))))
    mark))

(defun pai-memory-pool (branch &optional before)
  "Return BRANCH's active observations, oldest first.
Only batches whose coverage ends before index BEFORE count (all when nil).
Dropped observations are excluded.  Order: by timestamp, then commit order."
  (let* ((dropped (pai-memory-dropped-ids branch))
         (seq 0) (obs '()))
    (dolist (b (pai-memory-batches branch))
      (when (or (null before) (< (plist-get b :to) before))
        (dolist (o (plist-get (plist-get b :data) :observations))
          (unless (gethash (plist-get o :id) dropped)
            (push (cons (cl-incf seq) o) obs)))))
    (mapcar #'cdr
            (sort obs (lambda (a b)
                        (let ((ta (or (plist-get (cdr a) :timestamp) ""))
                              (tb (or (plist-get (cdr b) :timestamp) "")))
                          (if (string= ta tb) (< (car a) (car b)) (string< ta tb))))))))

(defun pai-memory-observation-tokens (observations)
  "Return the estimated tokens of OBSERVATIONS as rendered."
  (pai-estimate-tokens-from-chars
   (apply #'+ (mapcar (lambda (o) (+ 20 (length (or (plist-get o :content) ""))))
                      observations))))

;;;; Unobserved work

;; Callers pass `pai-memory-unobserved-runs' output to `pai-memory-slices'.

(defun pai-memory-unobserved-runs (branch &optional in-flight)
  "Return BRANCH's source entries still to observe, as runs, oldest first.
An entry is still to observe when no committed batch and no IN-FLIGHT range
covers it; IN-FLIGHT is a list of (FROM-ID . TO-ID).  A run is a list of
\(INDEX . ENTRY) pairs with no covered source entry between them, so a slice
taken from one run never overlaps observed work."
  (let* ((index (pai-memory-index-map branch))
         (batches (pai-memory-batches branch index))
         (covered (pai-memory-covered-p-vector branch batches)))
    (dolist (r in-flight)
      (let ((from (gethash (car r) index)) (to (gethash (cdr r) index)))
        (when (and from to)
          (cl-loop for i from from to to do (aset covered i t)))))
    (let ((i 0) (runs '()) (run '()))
      (dolist (e branch)
        (when (pai-memory-source-entry-p e)
          (if (aref covered i)
              (when run (push (nreverse run) runs) (setq run '()))
            (push (cons i e) run)))
        (setq i (1+ i)))
      (when run (push (nreverse run) runs))
      (nreverse runs))))

(defun pai-memory-unobserved-tokens (runs)
  "Return the estimated tokens of every entry in RUNS."
  (apply #'+ (mapcar #'pai-memory-slice-tokens runs)))

(defun pai-memory-slices (runs chunk-tokens max-slices &optional flush)
  "Cut unobserved RUNS into at most MAX-SLICES slices, oldest first.
Each slice holds about CHUNK-TOKENS and ends where the next entry is a valid
cut point, so observed ranges line up with possible compaction cuts.  The
remainder of a run followed by observed work is always sliced (it can never
grow); the remainder of the last run -- the live tip -- only when FLUSH is
non-nil.  Each slice is a list of (INDEX . ENTRY) pairs."
  (let ((slices '()))
    (while (and runs (< (length slices) max-slices))
      (let ((run (pop runs)) (current '()) (tokens 0))
        (while (and run (< (length slices) max-slices))
          (let* ((p (pop run)) (next (car run)))
            (push p current)
            (setq tokens (+ tokens (pai-memory-entry-tokens (cdr p))))
            (when (and (>= tokens chunk-tokens)
                       (or (null next) (pai-memory-valid-cut-p (cdr next))))
              (push (nreverse current) slices)
              (setq current '() tokens 0))))
        (when (and current (< (length slices) max-slices) (or runs flush))
          (push (nreverse current) slices))))
    (nreverse slices)))

(defun pai-memory-slice-tokens (slice)
  "Return the estimated tokens of SLICE's entries."
  (apply #'+ (mapcar (lambda (p) (pai-memory-entry-tokens (cdr p))) slice)))

;;;; Serialization for the observer

(defun pai-memory--time (ms)
  "Format epoch milliseconds MS as local \"YYYY-MM-DD HH:MM\"."
  (if (numberp ms)
      (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time (/ ms 1000.0)))
    "????-??-?? ??:??"))

(defun pai-memory--clip (text limit)
  "Return TEXT clipped to LIMIT characters with a marker."
  (if (and limit (> (length text) limit))
      (format "%s … [truncated %d chars]" (substring text 0 limit) (- (length text) limit))
    text))

(defun pai-memory-serialize-entry (entry &optional tool-result-chars)
  "Return the observer text for source ENTRY, or nil when it has none.
Tool results are clipped to TOOL-RESULT-CHARS characters."
  (let* ((m (pai-memory-entry-message entry))
         (time (pai-memory--time (plist-get m :timestamp)))
         (body
          (pcase (pai-message-role m)
            ('user
             (let ((content (pai-message-content m)))
               (format "[User @ %s]: %s" time
                       (if (and (pai-tool-schema-message-p m) (consp content))
                           (pai-content-text (list (car content)))
                         (pai-memory-strip-injected (pai-content-text content))))))
            ('assistant
             (let ((text (string-trim
                          (concat (pai-content-text (pai-message-content m))
                                  (mapconcat
                                   (lambda (tc)
                                     (format "\n[%s(%s)]" (plist-get tc :name)
                                             (condition-case nil
                                                 (pai-memory--clip
                                                  (pai-json-encode (or (plist-get tc :arguments)
                                                                       (pai-json-empty-object)))
                                                  tool-result-chars)
                                               (error "{}"))))
                                   (pai-message-tool-calls m) "")))))
               (unless (string-empty-p text)
                 (format "[Assistant @ %s]: %s" time text))))
            ('tool-result
             (format "[Tool result for %s @ %s]: %s" (or (plist-get m :tool-name) "?") time
                     (if (pai-tool-schema-message-p m)
                         "(tool definition loaded)"
                       (pai-memory--clip (pai-content-text (plist-get m :content))
                                         tool-result-chars))))
            (_ nil))))
    (and body (format "[Source entry id: %s]\n%s" (plist-get entry :id) body))))

(defun pai-memory-serialize-slice (slice &optional tool-result-chars)
  "Return the observer chunk text for SLICE's (INDEX . ENTRY) pairs."
  (mapconcat #'identity
             (delq nil (mapcar (lambda (p) (pai-memory-serialize-entry (cdr p) tool-result-chars))
                               slice))
             "\n\n"))

(provide 'pai-memory-ledger)
;;; pai-memory-ledger.el ends here
