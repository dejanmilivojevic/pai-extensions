;;; pai-memory-compact.el --- Observational compaction for pai-memory -*- lexical-binding: t; -*-

;;; Commentary:

;; Observational compaction (SPEC §5.3): a `compact' extension handler that
;; replaces the LLM summary with a deterministic render of the observation
;; ledger.
;;
;; The cut is snapped to a committed batch boundary inside the contiguously
;; observed prefix, so nothing is both rendered as an observation and kept
;; verbatim, and nothing falls between the two.  Among the boundaries whose
;; next source entry may open a tail (not a tool result), the one whose
;; verbatim tail is closest to `:tail-tokens' wins.
;;
;; When the observers lag -- the tail after the best boundary is still more
;; than twice `:tail-tokens' -- the unobserved part of that tail is summarized
;; by the LLM (the same call `/compact' makes) and rendered after the
;; observations; strategy "observational+summary".  With no usable boundary
;; the handler declines, and the built-in LLM compaction runs as today.
;;
;; The pool is capped at `:max-observation-tokens': older observations move to
;; an archive file in the session's memory directory, which the block points
;; to, so the agent can still read them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-tools)
(require 'pai-config)
(require 'pai-session)
(require 'pai-compaction)
(require 'pai-memory-settings)
(require 'pai-memory-ledger)

(defconst pai-memory-compact-header
  "These are condensed memories of the earlier part of this session; the raw messages were removed to save context.

- Memory map: topic files holding detailed, current-state notes; read or grep them when a subject comes up.
- Journey: how the work reached its current state.
- Reflections (when present): patterns across the session so far.
- Observations: timestamped events not yet filed into topics, oldest first.

Treat all of this as past records. When entries conflict, the most recent one reflects the latest known state. Work described as completed must not be redone unless the user asks to revisit it."
  "Opening of the observational compaction block.")

;;;; Paths

(defun pai-memory-session-dir (session &optional create)
  "Return SESSION's memory directory, creating it when CREATE (FC6)."
  (let ((dir (file-name-as-directory
              (file-name-concat (pai-memory-project-dir (pai-session-cwd session))
                                "sessions" (pai-session-id session)))))
    (when create (make-directory dir t))
    dir))

;;;; Rendering

(declare-function pai-memory-topics "pai-memory-consolidate" (dir))
(declare-function pai-memory-journey "pai-memory-consolidate" (dir))

(defun pai-memory-session-topics (dir)
  "Return DIR's topics when the consolidator module is loaded, else nil."
  (and (fboundp 'pai-memory-topics) (pai-memory-topics dir)))

(defun pai-memory-session-journey (dir)
  "Return DIR's journey when the consolidator module is loaded, else nil."
  (and (fboundp 'pai-memory-journey) (pai-memory-journey dir)))

(defun pai-memory-observation-line (o)
  "Return the rendered line for observation O."
  (format "%s  %s" (or (plist-get o :timestamp) "????-??-?? ??:??") (plist-get o :content)))

(defun pai-memory--cap-pool (observations cap)
  "Split OBSERVATIONS at CAP tokens: return (ARCHIVED . KEPT), newest kept."
  (if (or (null cap) (<= (pai-memory-observation-tokens observations) cap))
      (cons nil observations)
    (let ((kept '()) (acc 0) (rest (reverse observations)))
      (while (and rest (<= (+ acc (pai-memory-observation-tokens (list (car rest)))) cap))
        (setq acc (+ acc (pai-memory-observation-tokens (list (car rest)))))
        (push (pop rest) kept))
      (cons (nreverse rest) kept))))

(cl-defun pai-memory-render (observations &key topics dir journey reflections
                                          archive-file archived-count gap-summary)
  "Return the compaction block text for OBSERVATIONS.
TOPICS (see `pai-memory-topics') in memory directory DIR form the memory map;
JOURNEY is the JOURNEY.md text; REFLECTIONS a list of strings (V2 B4).  ARCHIVE-FILE and ARCHIVED-COUNT describe
observations moved out of the block; GAP-SUMMARY is an LLM summary of
unobserved messages.  The result depends only on the arguments."
  (string-join
   (delq nil
         (list pai-memory-compact-header
               (when topics
                 (concat (format "## Memory map\nTopic files in %s:\n"
                                 (abbreviate-file-name (file-name-as-directory dir)))
                         (mapconcat (lambda (tp)
                                      (format "- %s — %s: %s"
                                              (file-name-nondirectory (plist-get tp :path))
                                              (plist-get tp :title) (plist-get tp :summary)))
                                    topics "\n")))
               (when journey (concat "## Journey\n" journey))
               (when reflections
                 (concat "## Reflections\n" (mapconcat (lambda (r) (concat "- " r)) reflections "\n")))
               (when (and archive-file archived-count (> archived-count 0))
                 (format "## Older observations\n%d older observations are in %s; read or grep it when you need them."
                         archived-count (abbreviate-file-name archive-file)))
               (when observations
                 (concat "## Observations\n"
                         (mapconcat #'pai-memory-observation-line observations "\n")))
               (when (and gap-summary (not (string-empty-p (string-trim gap-summary))))
                 (concat "## Recent (summarized, not yet observed)\n" (string-trim gap-summary)))))
   "\n\n"))

(defun pai-memory--branch-reflections (branch)
  "Return BRANCH's latest reflections (V2 B4), or nil."
  (let ((found nil))
    (dolist (e branch)
      (when (pai-memory--custom-p e "memory.reflections") (setq found e)))
    (mapcar (lambda (r) (plist-get r :content))
            (append (plist-get (plist-get found :data) :reflections) nil))))

(defun pai-memory--write-archive (session archived)
  "Write ARCHIVED observations to SESSION's archive file; return its path."
  (let ((file (expand-file-name "observations-archive.md" (pai-memory-session-dir session t))))
    (with-temp-file file
      (insert "# Archived observations\n\n"
              (mapconcat #'pai-memory-observation-line archived "\n") "\n"))
    file))

;;;; Cut selection

(defun pai-memory--same-message-p (a b)
  "Return non-nil when live message A is the session message B."
  (and (eq (pai-message-role a) (pai-message-role b))
       (equal (plist-get a :timestamp) (plist-get b :timestamp))
       (equal (pai-content-text (pai-message-content a))
              (pai-content-text (pai-message-content b)))))

(defun pai-memory-compaction-cut (branch tail-target)
  "Return the best observational cut on BRANCH for TAIL-TARGET tokens, or nil.
The result is (:boundary B :first-kept ENTRY :tail-entries ENTRIES :tail-tokens T)
where B indexes BRANCH, and TAIL-ENTRIES are the source entries after B."
  (let* ((index (pai-memory-index-map branch))
         (batches (pai-memory-batches branch index))
         (mark (pai-memory-watermark branch batches))
         (vec (vconcat branch))
         (best nil) (best-delta nil))
    (when mark
      (dolist (b (delete-dups (sort (mapcar (lambda (x) (plist-get x :to)) batches) #'<)))
        (when (<= b mark)
          (let* ((tail (seq-filter #'pai-memory-source-entry-p
                                   (append (seq-drop vec (1+ b)) nil)))
                 (first (car tail)))
            (when (and first (pai-memory-valid-cut-p first))
              (let* ((tokens (apply #'+ (mapcar #'pai-memory-entry-tokens tail)))
                     (delta (abs (- tokens tail-target))))
                (when (or (null best-delta) (< delta best-delta))
                  (setq best-delta delta
                        best (list :boundary b :first-kept first
                                   :tail-entries tail :tail-tokens tokens)))))))))
    best))

;;;; Handler

(defun pai-memory--summary-message (text carried)
  "Return the summary user message for TEXT, carrying deferred schemas CARRIED."
  (if carried
      (pai-user-message (list (pai-text text) (pai-text (plist-get carried :text)))
                        :deferred-schemas (plist-get carried :names))
    (pai-user-message text)))

(defun pai-memory-compact (messages session model &optional summarize-fn)
  "Compact live MESSAGES of SESSION from its observations; return a result or nil.
The result has the shape `pai-ext-run-compact' expects.  MODEL summarizes
the unobserved gap when observers lag, through SUMMARIZE-FN (default
`pai-compaction-summarize', called with messages and model)."
  (let* ((branch (pai-session-get-branch session))
         (target (or (pai-memory-get :session :tail-tokens session) 20000))
         (cut (pai-memory-compaction-cut branch target)))
    (when cut
      (let* ((system (seq-take-while #'pai-system-message-p messages))
             (rest (seq-drop-while #'pai-system-message-p messages))
             (tail-entries (plist-get cut :tail-entries))
             ;; compare against the context as edited (e.g. by /shake)
             (edits (pai-session-replacement-table branch))
             (tail-msgs (mapcar (lambda (e) (pai-session-entry-message e edits)) tail-entries))
             (k (length tail-msgs)))
        ;; the kept tail must be exactly the end of the live context, and the
        ;; cut must actually drop something
        (when (and (< k (length rest))
                   (cl-every #'pai-memory--same-message-p (last rest k) tail-msgs))
          (let* ((kept (last rest k))
                 (first-kept (plist-get (plist-get cut :first-kept) :id))
                 (gap-summary nil) (usage nil)
                 (strategy "observational"))
            ;; observers lag: summarize the unobserved part of a long tail
            (when (and model (> (plist-get cut :tail-tokens) (* 2 target)))
              (let ((recent (pai-compaction--find-cut-index kept target)))
                (when (and (> recent 0) (pai-memory-valid-cut-p (nth recent tail-entries)))
                  (let ((result (funcall (or summarize-fn #'pai-compaction-summarize)
                                         (seq-take kept recent) model)))
                    (when (and (plist-get result :text)
                               (not (string-empty-p (string-trim (plist-get result :text)))))
                      (setq gap-summary (plist-get result :text)
                            usage (plist-get result :usage)
                            strategy "observational+summary"
                            first-kept (plist-get (nth recent tail-entries) :id)
                            kept (nthcdr recent kept)))))))
            (let* ((pool (pai-memory-pool branch (1+ (plist-get cut :boundary))))
                   (capped (pai-memory--cap-pool
                            pool (pai-memory-get :session :max-observation-tokens session)))
                   (archive (and (car capped) (pai-memory--write-archive session (car capped))))
                   (dir (pai-memory-session-dir session))
                   (text (pai-memory-render
                          (cdr capped)
                          :topics (pai-memory-session-topics dir) :dir dir
                          :journey (pai-memory-session-journey dir)
                          :reflections (pai-memory--branch-reflections branch)
                          :archive-file archive :archived-count (length (car capped))
                          :gap-summary gap-summary))
                   (dropped (seq-take rest (- (length rest) (length kept))))
                   (carried (pai-tool-carried-schemas dropped kept)))
              (list :messages (append system
                                      (list (pai-memory--summary-message text carried))
                                      (pai-invalidate-usage-anchors kept))
                    :summary text
                    :strategy strategy
                    :first-kept-entry-id first-kept
                    :tokens-before (pai-estimate-context-tokens messages)
                    :usage usage))))))))

(defun pai-memory-compact-handler (event ctx)
  "The `compact' extension handler: observational compaction when possible.
Declines (returns nil) when the session layer is off, when `/compact' was
given custom instructions (the user asked for an LLM summary), or when no
observations can replace the older context yet."
  (let ((session (plist-get ctx :session))
        (instructions (plist-get event :custom-instructions)))
    (when (and session
               (pai-memory-session-enabled-p session)
               (or (null instructions) (string-empty-p (string-trim instructions))))
      (condition-case err
          (pai-memory-compact (plist-get event :messages) session (plist-get event :model))
        (error (message "pai-memory: observational compaction failed, using summary: %s"
                        (error-message-string err))
               nil)))))

(provide 'pai-memory-compact)
;;; pai-memory-compact.el ends here
