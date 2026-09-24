;;; pai-shake.el --- Surgical context reduction ("/shake") for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Port of oh-my-pi's `/shake' command (packages/agent/src/compaction/shake.ts
;; plus the `shake' orchestration in session-maintenance.ts) to the pai
;; extension API.
;;
;; Where `/compact' asks a model to summarize the transcript, `/shake' drops
;; heavy content *mechanically*: no LLM call, no waiting, nothing rewritten in
;; the user's own words.  Three modes:
;;
;;   /shake            (= /shake elide)
;;       Replace whole tool-call results and large fenced/XML blocks with a
;;       short placeholder.  The originals are written to a recovery artifact
;;       file first, and every placeholder names that file, so the agent can
;;       `read' back anything it turns out to still need.
;;   /shake images     Strip image blocks from the context.
;;   /shake thinking   Drop assistant reasoning blocks.
;;
;; Design notes (mirroring the upstream implementation):
;;
;;   * The most recent `:protect-tokens' of context is never touched, so the
;;     tool results the agent is currently working from survive the shake.
;;   * Tool results are dropped whole; `:pruned-at' marks them so a second
;;     shake does not re-elide its own placeholders.
;;   * Only *large* fenced/XML blocks (>= `:fence-min-tokens') are eligible,
;;     and only complete, properly terminated ones: block detection never
;;     spans a message boundary and unterminated fences yield no region.
;;   * Tool-call blocks are never touched, so tool-call/result pairing stays
;;     intact and the transcript remains a valid provider request.
;;   * The system prompt is never shaken.
;;   * Deferred tool definitions (schema reveals, and compaction summaries
;;     that carry them) are never shaken, so a loaded tool stays loaded.
;;
;; The pure layer (`pai-shake-scan-block-ranges', `pai-shake-collect-regions',
;; `pai-shake-apply-regions', `pai-shake-run') is functional: it takes a
;; message list and returns a new one, doing no I/O, so it is directly
;; testable.  The command layer swaps the result into the live buffer context,
;; records a session entry, and reports a one-line summary.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'pai)
(require 'pai-core)
(require 'pai-config)
(require 'pai-settings)
(require 'pai-session)
(require 'pai-ext)
(require 'pai-compaction)
(require 'pai-tools)

(declare-function pai-settings-ui-register-item "pai-settings-ui")

;;;; Artifacts

(defun pai-shake-artifact-directory (&optional create)
  "Return the directory holding shake recovery artifacts.
With CREATE non-nil the directory is created if missing."
  (if create
      (pai-state-directory "artifacts")
    (file-name-concat pai-directory "artifacts")))

(defun pai-shake-artifact-recovery-p (_message tool-call)
  "Return non-nil when TOOL-CALL reads back a shake recovery artifact.
Re-eliding the recovery read would undo the recovery the agent just made."
  (and tool-call
       (equal (plist-get tool-call :name) "read")
       (let ((path (plist-get (plist-get tool-call :arguments) :path)))
         (and (stringp path)
              (string-prefix-p (file-name-as-directory
                                (expand-file-name (pai-shake-artifact-directory)))
                               (expand-file-name path))))))

(defun pai-shake-artifact-text (regions)
  "Return the recovery-artifact body holding REGIONS' original content."
  (let ((i 0) (parts nil))
    (dolist (r regions)
      (setq i (1+ i))
      (push (format "### region %d (%s, ~%d tok)\n\n%s\n"
                    i (plist-get r :label) (plist-get r :tokens) (plist-get r :text))
            parts))
    (string-join (nreverse parts) "\n")))

(defun pai-shake-save-artifact (regions)
  "Write REGIONS' originals to a recovery artifact.  Return its path, or nil.
Failing to persist the artifact is never fatal: the shake still proceeds, with
placeholders that simply carry no recovery pointer."
  (condition-case nil
      (let* ((dir (pai-shake-artifact-directory t))
             (path (expand-file-name
                    (format "shake-%s-%04x.md"
                            (format-time-string "%Y%m%dT%H%M%S")
                            (random 65536))
                    dir)))
        (let ((coding-system-for-write 'utf-8))
          (with-temp-file path (insert (pai-shake-artifact-text regions))))
        path)
    (error nil)))

;;;; Configuration

(defconst pai-shake-default-protect-tokens 4000
  "Tokens of the most recent context that `/shake' never touches.
Mirrors oh-my-pi's AGGRESSIVE_SHAKE_CONFIG: a manual shake reaches across the
whole history but still keeps a small live tail, so it cannot strip the tool
results the agent is working from right now.")

(defconst pai-shake-default-fence-min-tokens 400
  "Minimum estimated tokens for a fenced/XML block to be eligible.")

(defconst pai-shake-default-min-savings 0
  "Minimum total estimated savings before a manual shake does anything.
Zero: a manual `/shake' is the user's explicit escape hatch.")

(defconst pai-shake-placeholder-tokens 16
  "Rough token cost of one placeholder line, used for the savings gate.")

(defconst pai-shake-protected-tools
  (list "skill" #'pai-shake-artifact-recovery-p)
  "Tool-result protection matchers.
A string protects every result of that tool; a function is called with
\(MESSAGE TOOL-CALL) and protects the result when it returns non-nil.")

(defun pai-shake--settings ()
  "Return this instance's `:shake' settings plist."
  (or (pai-settings-get :shake) '()))

(defun pai-shake--setting (key default)
  "Return KEY from the `:shake' settings plist, or DEFAULT when unset."
  (let ((v (plist-get (pai-shake--settings) key)))
    (if (numberp v) v default)))

(defun pai-shake-config (&optional overrides)
  "Return the effective shake config, with OVERRIDES taking precedence.
Keys: `:protect-tokens', `:min-savings', `:fence-min-tokens',
`:protected-tools'."
  (cl-flet ((pick (key fallback)
              (if (plist-member overrides key) (plist-get overrides key) fallback)))
    (list :protect-tokens (pick :protect-tokens
                                (pai-shake--setting :protect-tokens
                                                    pai-shake-default-protect-tokens))
          :min-savings (pick :min-savings
                             (pai-shake--setting :min-savings
                                                 pai-shake-default-min-savings))
          :fence-min-tokens (pick :fence-min-tokens
                                  (pai-shake--setting :fence-min-tokens
                                                      pai-shake-default-fence-min-tokens))
          :protected-tools (pick :protected-tools pai-shake-protected-tools))))

;;;; Token estimation

(defun pai-shake--text-tokens (text)
  "Estimate the tokens of TEXT with the shared context estimator."
  (pai-estimate-tokens-from-chars (length text)))

;;;; Fenced / XML block detection

(defconst pai-shake--opening-xml "\\`<\\([a-z_-]+\\)\\(?:[[:space:]]+[^>]*\\)?>\\'"
  "Match a line that opens a top-level XML element.  Lowercase tags only.")

(defconst pai-shake--closing-xml "\\`</\\([a-z_-]+\\)>\\'"
  "Match a line that closes a top-level XML element.")

(defun pai-shake--merge-ranges (ranges)
  "Sort RANGES ascending and drop any that overlaps an already-kept range.
Fence and XML spans are properly nested (XML detection is suppressed inside
fences), so overlap means containment and the outermost span is kept."
  (let ((sorted (sort (copy-sequence ranges) (lambda (a b) (< (car a) (car b)))))
        (kept nil) (last-end -1))
    (dolist (r sorted (nreverse kept))
      (unless (< (car r) last-end)
        (push r kept)
        (setq last-end (cdr r))))))

(defun pai-shake-scan-block-ranges (text)
  "Return non-overlapping (START . END) character ranges of blocks in TEXT.
A block is a fenced code block (``` or ~~~) or a top-level XML element span;
the range covers the opening and closing lines but not the trailing newline.
Conservative by design: unterminated fences/tags yield no range, and XML
detection is suppressed inside fences."
  (let ((len (length text)) (line-start 0)
        (in-fence nil) (fence-start -1)
        (tag-stack nil) (xml-start -1)
        (ranges nil)
        ;; Tag matching is case-sensitive: uppercase/mixed-case tags are
        ;; ignored, as upstream does.
        (case-fold-search nil))
    (while (<= line-start len)
      (let* ((nl (string-search "\n" text line-start))
             (line-end (or nl len))
             (line (substring text line-start line-end))
             (trimmed (string-trim-left line)))
        (cond
         ;; Fence toggle.
         ((or (string-prefix-p "```" trimmed) (string-prefix-p "~~~" trimmed))
          (if in-fence
              (progn (push (cons fence-start line-end) ranges)
                     (setq in-fence nil fence-start -1))
            (setq in-fence t fence-start line-start)))
         (in-fence nil)
         ;; Opening tag: must start at column 0, like prompt rendering does.
         ((and (= (length line) (length trimmed))
               (string-match pai-shake--opening-xml trimmed))
          (when (null tag-stack) (setq xml-start line-start))
          (push (match-string 1 trimmed) tag-stack))
         ;; Closing tag for the innermost open element.
         ((and (string-match pai-shake--closing-xml trimmed)
               tag-stack
               (equal (car tag-stack) (match-string 1 trimmed)))
          (pop tag-stack)
          (when (and (null tag-stack) (>= xml-start 0))
            (push (cons xml-start line-end) ranges)
            (setq xml-start -1))))
        (setq line-start (1+ line-end))))
    (pai-shake--merge-ranges (nreverse ranges))))

;;;; Region detection

(defun pai-shake--tool-calls-by-id (messages)
  "Return a hash table mapping tool-call id to its tool-call block in MESSAGES."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (m messages map)
      (when (pai-assistant-message-p m)
        (dolist (tc (pai-message-tool-calls m))
          (puthash (plist-get tc :id) tc map))))))

(defun pai-shake--protected-p (message tool-call config)
  "Return non-nil when tool-result MESSAGE is protected by CONFIG.
TOOL-CALL is the paired tool-call block, when known."
  (let ((name (plist-get message :tool-name)))
    (seq-some (lambda (matcher)
                (cond ((stringp matcher) (equal matcher name))
                      ((functionp matcher) (funcall matcher message tool-call))))
              (plist-get config :protected-tools))))

(defun pai-shake--tool-result-text (message)
  "Return the joined text of tool-result MESSAGE, or nil when it has none.
Non-text blocks (images) are ignored here: they are preserved on elide."
  (let ((parts nil))
    (dolist (b (pai-normalize-content (plist-get message :content)))
      (let ((text (and (eq (pai-block-type b) 'text) (plist-get b :text))))
        (when (and (stringp text) (> (length text) 0))
          (push text parts))))
    (when parts (string-join (nreverse parts) "\n"))))

(defun pai-shake--scan-text (index block-index text fence-min label)
  "Return block regions found in TEXT, in document order.
INDEX is the message's position, BLOCK-INDEX its content-block index (-1 for
string content), FENCE-MIN the eligibility threshold and LABEL a human tag."
  (let (out)
    (dolist (range (pai-shake-scan-block-ranges text) (nreverse out))
      (let* ((slice (substring text (car range) (cdr range)))
             (tokens (pai-shake--text-tokens slice)))
        (when (and (> (length slice) 0) (>= tokens fence-min))
          (push (list :kind 'block :index index :block-index block-index
                      :start (car range) :end (cdr range)
                      :tokens tokens :text slice :label label)
                out))))))

(defun pai-shake--block-regions (message index fence-min)
  "Return the eligible block regions of MESSAGE at INDEX, in document order.
FENCE-MIN is the minimum token size for a block to qualify."
  (let ((label (symbol-name (pai-message-role message)))
        (content (pai-message-content message))
        (out nil))
    (cond
     ((stringp content)
      (setq out (pai-shake--scan-text index -1 content fence-min label)))
     ((listp content)
      (let ((bi -1))
        (dolist (b content)
          (setq bi (1+ bi))
          (let ((text (and (eq (pai-block-type b) 'text) (plist-get b :text))))
            (when (stringp text)
              (setq out (append out (pai-shake--scan-text index bi text
                                                          fence-min label)))))))))
    out))

(defun pai-shake--suffix-tokens (messages)
  "Return a vector where element I is the estimated tokens of MESSAGES after I."
  (let* ((n (length messages))
         (after (make-vector (max n 1) 0))
         (acc 0))
    (cl-loop for i from (1- n) downto 0 do
             (aset after i acc)
             (cl-incf acc (pai-estimate-tokens (nth i messages))))
    after))

(defun pai-shake-collect-regions (messages &optional config)
  "Locate every eligible shake region in MESSAGES, in document order.
CONFIG overrides the effective `pai-shake-config'.  Returns nil when the
combined estimated savings falls below `:min-savings'.

Walks past the protect-recent window, collecting the text of eligible
tool-result messages (honoring protected tools and already-pruned results) and
large fenced/XML blocks inside user and assistant messages.  The system prompt
and tool-call blocks are never eligible, and no region spans a message
boundary.  Messages carrying a deferred tool's full definition (see
`pai-tool-schema-message-p') are never eligible either: shaking them would
leave the model with only the tool's stub."
  (let* ((config (pai-shake-config config))
         (protect (plist-get config :protect-tokens))
         (fence-min (plist-get config :fence-min-tokens))
         (after (pai-shake--suffix-tokens messages))
         (calls (pai-shake--tool-calls-by-id messages))
         (index -1)
         (regions nil))
    (dolist (m messages)
      (setq index (1+ index))
      (unless (or (pai-system-message-p m)
                  (pai-tool-schema-message-p m)
                  (< (aref after index) protect))
        (if (pai-tool-result-message-p m)
            (unless (or (plist-get m :pruned-at)
                        (pai-shake--protected-p
                         m (gethash (plist-get m :tool-call-id) calls) config))
              (when-let ((text (pai-shake--tool-result-text m)))
                (push (list :kind 'tool-result :index index
                            :tokens (pai-shake--text-tokens text)
                            :text text
                            :label (or (plist-get m :tool-name) "tool"))
                      regions)))
          (dolist (r (pai-shake--block-regions m index fence-min))
            (push r regions)))))
    (setq regions (nreverse regions))
    (let ((savings 0))
      (dolist (r regions)
        (cl-incf savings (max 0 (- (plist-get r :tokens) pai-shake-placeholder-tokens))))
      (when (>= savings (plist-get config :min-savings))
        regions))))

;;;; Applying regions

(defun pai-shake-placeholder (region index &optional artifact)
  "Return the placeholder text replacing REGION, the INDEX-th one shaken.
ARTIFACT, when given, is the recovery file the original was written to."
  (if artifact
      (format "[shaken ~%d tokens — recover: read %s (region %d)]"
              (plist-get region :tokens) artifact (1+ index))
    (format "[shaken ~%d tokens]" (plist-get region :tokens))))

(defun pai-shake--splice (text items)
  "Replace every region in ITEMS within TEXT.
ITEMS is a list of (REGION . REPLACEMENT).  Highest start first, so splicing
one region never shifts the offsets of another in the same text."
  (let ((ordered (sort (copy-sequence items)
                       (lambda (a b) (> (plist-get (car a) :start)
                                        (plist-get (car b) :start))))))
    (dolist (it ordered text)
      (setq text (concat (substring text 0 (plist-get (car it) :start))
                         (cdr it)
                         (substring text (plist-get (car it) :end)))))))

(defun pai-shake--apply-tool-result (message replacement)
  "Return a copy of tool-result MESSAGE with its text replaced by REPLACEMENT.
The first non-empty text block becomes the placeholder, the other text blocks
are dropped, and every non-text block (images) is preserved.  The copy is
stamped `:pruned-at' so later shakes leave it alone."
  (let ((kept nil) (done nil))
    (dolist (b (pai-normalize-content (plist-get message :content)))
      (cond
       ((not (eq (pai-block-type b) 'text)) (push b kept))
       ((and (not done) (> (length (or (plist-get b :text) "")) 0))
        (setq done t)
        (push (pai-text replacement) kept))))
    (let ((m (copy-sequence message)))
      (setq m (plist-put m :content (nreverse kept)))
      (plist-put m :pruned-at (pai-now-ms)))))

(defun pai-shake--apply-blocks (message items)
  "Return a copy of MESSAGE with the block regions in ITEMS spliced out.
ITEMS is a list of (REGION . REPLACEMENT) belonging to this message."
  (let ((content (pai-message-content message))
        (m (copy-sequence message)))
    (if (stringp content)
        (plist-put m :content (pai-shake--splice content items))
      (let ((bi -1))
        (plist-put
         m :content
         (mapcar (lambda (b)
                   (setq bi (1+ bi))
                   (let ((group (seq-filter
                                 (lambda (it) (eql (plist-get (car it) :block-index) bi))
                                 items)))
                     (if (or (null group) (not (eq (pai-block-type b) 'text)))
                         b
                       (let ((nb (copy-sequence b)))
                         (plist-put nb :text
                                    (pai-shake--splice (plist-get b :text) group))))))
                 content))))))

(defun pai-shake-apply-regions (messages items)
  "Return MESSAGES with every region in ITEMS replaced.
ITEMS is a list of (REGION . REPLACEMENT).  Messages are copied rather than
mutated, so session entries sharing the same plists keep their originals."
  (let ((by-index (make-hash-table :test 'eql))
        (index -1))
    (dolist (it items)
      (push it (gethash (plist-get (car it) :index) by-index)))
    (mapcar (lambda (m)
              (setq index (1+ index))
              (let ((group (nreverse (gethash index by-index))))
                (cond
                 ((null group) m)
                 ((eq (plist-get (car (car group)) :kind) 'tool-result)
                  (pai-shake--apply-tool-result m (cdr (car group))))
                 (t (pai-shake--apply-blocks m group)))))
            messages)))

;;;; Images / thinking modes

(defun pai-shake-drop-images (messages)
  "Return (NEW-MESSAGES . COUNT) with every image block removed from MESSAGES.
A message left with no blocks keeps a `[image removed]' marker: providers
reject zero-block user/tool messages, and the model still needs to see that
something was there."
  (let ((count 0))
    (cons (mapcar
           (lambda (m)
             (let ((content (pai-message-content m)))
               (if (or (not (listp content)) (null content))
                   m
                 (let* ((kept (seq-remove (lambda (b) (eq (pai-block-type b) 'image))
                                          content))
                        (dropped (- (length content) (length kept))))
                   (if (= dropped 0)
                       m
                     (cl-incf count dropped)
                     (plist-put (copy-sequence m) :content
                                (or kept (list (pai-text "[image removed]")))))))))
           messages)
          count)))

(defun pai-shake-drop-thinking (messages)
  "Return (NEW-MESSAGES . COUNT) with assistant reasoning blocks removed.
No replacement text is invented: provider serializers omit empty assistant
turns rather than inventing model-authored content."
  (let ((count 0))
    (cons (mapcar
           (lambda (m)
             (let ((content (pai-message-content m)))
               (if (not (and (pai-assistant-message-p m) (listp content)))
                   m
                 (let* ((kept (seq-remove (lambda (b) (eq (pai-block-type b) 'thinking))
                                          content))
                        (dropped (- (length content) (length kept))))
                   (if (= dropped 0)
                       m
                     (cl-incf count dropped)
                     (plist-put (copy-sequence m) :content kept))))))
           messages)
          count)))

;;;; Running a shake

(defun pai-shake--first-change (old new)
  "Return the index of the first message that differs between OLD and NEW.
Unchanged messages are returned by identity, so `eq' is the test."
  (let ((i 0) (found nil))
    (while (and old new (null found))
      (unless (eq (car old) (car new)) (setq found i))
      (setq old (cdr old) new (cdr new) i (1+ i)))
    found))

(defun pai-shake-parse-mode (args)
  "Parse ARGS into a shake mode symbol, or return (:error MESSAGE).
An empty argument means `elide'."
  (let ((verb (downcase (string-trim (or args "")))))
    (cond
     ((or (string-empty-p verb) (equal verb "elide")) 'elide)
     ((equal verb "images") 'images)
     ((equal verb "thinking") 'thinking)
     ((equal verb "all") 'all)
     (t (list :error (format "Unknown /shake mode \"%s\". Use elide, images, thinking, or all."
                             verb))))))

(cl-defun pai-shake-run (messages mode &key config (artifact-fn #'pai-shake-save-artifact))
  "Shake MESSAGES with MODE (`elide', `images', `thinking' or `all').
`all' does the three in one pass: large tool results and blocks, then
images, then thinking blocks (the protected recent tail is kept).
CONFIG overrides the effective `pai-shake-config'.  ARTIFACT-FN is called with
the eligible regions and returns the recovery file path (or nil); pass nil to
skip artifact persistence entirely.

Returns a result plist: `:messages' (the new list), `:mode', and the counts
`:tool-results-dropped', `:blocks-dropped', `:images-dropped',
`:thinking-dropped', `:tokens-freed' and `:artifact'."
  (let ((result (list :mode mode :messages messages
                      :tool-results-dropped 0 :blocks-dropped 0
                      :images-dropped 0 :thinking-dropped 0
                      :tokens-freed 0 :artifact nil))
        (original messages))
    (pcase mode
      ('all
       (let* ((elided (pai-shake-run messages 'elide :config config :artifact-fn artifact-fn))
              (images (pai-shake-drop-images (plist-get elided :messages)))
              (thinking (pai-shake-drop-thinking (car images))))
         (dolist (k '(:tool-results-dropped :blocks-dropped :tokens-freed :artifact))
           (setq result (plist-put result k (plist-get elided k))))
         (setq result (plist-put result :images-dropped (cdr images)))
         (setq result (plist-put result :thinking-dropped (cdr thinking)))
         (setq result (plist-put result :messages (car thinking)))))
      ('images
       (let ((out (pai-shake-drop-images messages)))
         (setq result (plist-put result :messages (car out)))
         (plist-put result :images-dropped (cdr out))))
      ('thinking
       (let ((out (pai-shake-drop-thinking messages)))
         (setq result (plist-put result :messages (car out)))
         (plist-put result :thinking-dropped (cdr out))))
      (_
       (let ((regions (pai-shake-collect-regions messages config)))
         (if (null regions)
             result
           (let* ((artifact (and artifact-fn (funcall artifact-fn regions)))
                  (index -1)
                  (original-tokens 0) (replaced 0)
                  (tool-results 0) (blocks 0)
                  (items nil))
             (dolist (r regions)
               (setq index (1+ index))
               (let* ((text (pai-shake-placeholder r index artifact))
                      (tokens (pai-shake--text-tokens text)))
                 (if (eq (plist-get r :kind) 'tool-result)
                     (cl-incf tool-results)
                   (cl-incf blocks))
                 (cl-incf original-tokens (plist-get r :tokens))
                 (cl-incf replaced tokens)
                 (push (cons r text) items)))
             (setq result (plist-put result :messages
                                     (pai-shake-apply-regions messages (nreverse items))))
             (setq result (plist-put result :tool-results-dropped tool-results))
             (setq result (plist-put result :blocks-dropped blocks))
             (setq result (plist-put result :tokens-freed (max 0 (- original-tokens replaced))))
             (plist-put result :artifact artifact))))))
    ;; A provider usage report counts the context as it was when the request
    ;; was made; we just rewrote part of that prefix, so the affected anchors
    ;; have to stop speaking for it or the meter will not move until the next
    ;; turn.
    (when-let ((from (pai-shake--first-change original (plist-get result :messages))))
      (setq result (plist-put result :messages
                              (pai-invalidate-usage-anchors
                               (plist-get result :messages) from))))
    result))

(defun pai-shake-dropped-count (result)
  "Return the total number of items dropped in shake RESULT."
  (+ (plist-get result :tool-results-dropped)
     (plist-get result :blocks-dropped)
     (plist-get result :images-dropped)
     (plist-get result :thinking-dropped)))

(defun pai-shake-format-summary (result)
  "Return a one-line operator summary of shake RESULT."
  (pcase (plist-get result :mode)
    ('all
     (let* ((one (lambda (n what) (and (> n 0) (format "%d %s%s" n what (if (= n 1) "" "s")))))
            (parts (delq nil (list (funcall one (plist-get result :tool-results-dropped) "tool result")
                                   (funcall one (plist-get result :blocks-dropped) "block")
                                   (funcall one (plist-get result :images-dropped) "image")
                                   (funcall one (plist-get result :thinking-dropped) "thinking block")))))
       (if (null parts)
           "Nothing to shake."
         (concat (format "Shook %s (~%d tokens freed from tool results and blocks)."
                         (string-join parts " + ") (plist-get result :tokens-freed))
                 (if-let ((artifact (plist-get result :artifact)))
                     (format "\nOriginals: %s" artifact)
                   "")))))
    ('images
     (let ((n (plist-get result :images-dropped)))
       (if (= n 0) "No images found in this session."
         (format "Dropped %d image%s from this session." n (if (= n 1) "" "s")))))
    ('thinking
     (let ((n (plist-get result :thinking-dropped)))
       (if (= n 0) "No thinking blocks found in this session."
         (format "Dropped %d thinking block%s from this session." n (if (= n 1) "" "s")))))
    (_
     (let* ((tr (plist-get result :tool-results-dropped))
            (bl (plist-get result :blocks-dropped))
            (parts (delq nil
                         (list (when (> tr 0)
                                 (format "%d tool result%s" tr (if (= tr 1) "" "s")))
                               (when (> bl 0)
                                 (format "%d block%s" bl (if (= bl 1) "" "s")))))))
       (if (null parts)
           "Nothing to shake."
         (concat (format "Shook %s (~%d tokens freed)."
                         (string-join parts " + ") (plist-get result :tokens-freed))
                 (if-let ((artifact (plist-get result :artifact)))
                     (format "\nOriginals: %s" artifact)
                   "")))))))

;;;; Command

(defun pai-shake--run-in-buffer (args)
  "Shake the live context of the current pai buffer according to ARGS.
Renders a note with the outcome and returns nil (nothing left to display)."
  (let ((mode (pai-shake-parse-mode args)))
    (if (consp mode)
        (list :message (plist-get mode :error))
      (let ((messages pai--context-messages))
        (if (null messages)
            (progn (pai--render-note "Nothing to shake.") nil)
          (let ((result (pai-shake-run messages mode)))
            (if (= (pai-shake-dropped-count result) 0)
                (pai--render-note (pai-shake-format-summary result))
              ;; The edits are recorded per session entry so the shaken
              ;; context is rebuilt on /resume (`pai-session-context-pairs').
              (let ((replacements (and pai--session
                                       (pai-session-replacements
                                        pai--session messages (plist-get result :messages)))))
                (setq pai--context-messages (plist-get result :messages))
                (pai--refresh-context-tokens)
                (when pai--session
                  (pai-session-append
                   pai--session
                   (append
                    (list :type "shake"
                          :mode (symbol-name mode)
                          :toolResults (plist-get result :tool-results-dropped)
                          :blocks (plist-get result :blocks-dropped)
                          :images (plist-get result :images-dropped)
                          :thinking (plist-get result :thinking-dropped)
                          :tokensFreed (plist-get result :tokens-freed)
                          :artifact (or (plist-get result :artifact) ""))
                    (when (consp replacements)
                      (list :replacements (vconcat replacements)))))))
              (pai-ext-emit 'session-shake (pai--ext-context)
                            :mode mode :result result)
              ;; A run in flight keeps the context snapshot it started with;
              ;; its turn is appended to the shaken list when it settles.
              (pai--render-note
               (concat (pai-shake-format-summary result)
                       (when pai--active
                         "\nThe active run keeps its snapshot; this applies from the next turn."))))
            nil))))))

(defun pai-shake-command (args ctx)
  "Handler for `/shake': drop heavy content from the live context.
ARGS selects the mode (`elide', `images', `thinking' or `all'); CTX carries the
originating pai buffer."
  (let ((buf (plist-get ctx :buffer)))
    (if (buffer-live-p buf)
        (with-current-buffer buf (pai-shake--run-in-buffer args))
      (list :message "No active pai session to shake"))))

(defun pai-shake--completions (prefix)
  "Return the `/shake' mode completions matching PREFIX."
  (seq-filter (lambda (s) (string-prefix-p prefix s))
              '("elide" "images" "thinking" "all")))

;;;; Extension entry point

(pai-register-extension
 (lambda (api)
   (pai-ext-register-command
    api "shake"
    :description "Drop heavy content from context (tool results, large blocks)"
    :arg-completions #'pai-shake--completions
    :handler #'pai-shake-command))
 "shake")

;; Expose the knobs on the settings screen (`/menu') when it is available, so
;; every session can tune the protected tail and block threshold.
(with-eval-after-load 'pai-settings-ui
  (pai-settings-ui-register-item
   'session 'context
   :key :shake-protect-tokens :type 'number :label "Shake: protect recent tokens"
   :doc "Tokens of the newest context that /shake never touches"
   :get (lambda () (pai-shake--setting :protect-tokens pai-shake-default-protect-tokens))
   :set (lambda (v)
          (pai-settings-set :shake
                            (plist-put (copy-sequence (pai-shake--settings))
                                       :protect-tokens v)
                            'project)))
  (pai-settings-ui-register-item
   'session 'context
   :key :shake-fence-min-tokens :type 'number :label "Shake: block threshold"
   :doc "Minimum tokens for a fenced/XML block to be shaken"
   :get (lambda () (pai-shake--setting :fence-min-tokens pai-shake-default-fence-min-tokens))
   :set (lambda (v)
          (pai-settings-set :shake
                            (plist-put (copy-sequence (pai-shake--settings))
                                       :fence-min-tokens v)
                            'project))))

(provide 'pai-shake)
;;; pai-shake.el ends here
