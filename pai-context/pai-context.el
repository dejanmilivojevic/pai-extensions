;;; pai-context.el --- Context-usage report and full-context dump for pai -*- lexical-binding: t; -*-

;; Port of the idea behind oh-my-pi's `/context' command to the pai extension
;; API.  It provides two things:
;;
;;   /context        A compact token-usage breakdown by category (system
;;                   prompt, tools, per-role messages, autocompact reserve,
;;                   free space), rendered as a bar/grid plus a legend with
;;                   token counts and percentages, mirroring the categories in
;;                   oh-my-pi's `renderContextUsage'.
;;
;;   /context-dump   Opens a new Emacs buffer with the *entire* assembled
;;                   context exactly as it would be sent to the LLM: the system
;;                   prompt, the tool definitions (JSON schema), and every
;;                   message (role + content) with clear separators, so you can
;;                   debug what the model actually receives.
;;
;; `/context dump' (or `/context full') is accepted as an alias for
;; `/context-dump'.

;;; Commentary:

;; Token accounting reuses pai's own estimators so the numbers match what the
;; agent loop and compaction logic see: `pai-estimate-context-tokens' for the
;; anchored total, `pai-estimate-tokens' for per-message splits, and the
;; compaction helpers (`pai-compaction-threshold-tokens') for the reserved
;; autocompact buffer.  That buffer is `window - threshold' -- the headroom
;; held back so compaction can run -- and is therefore a budget allowance, not
;; content; `/context-dump' cannot show it because nothing backs it.
;; Tool-schema tokens are estimated from the provider tool
;; declarations, and the system-prompt category is the leading system
;; message with the tool tokens subtracted so the categories do not
;; double-count the tools section already embedded in it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai)
(require 'pai-ext)
(require 'pai-core)
(require 'pai-models)
(require 'pai-prompt)
(require 'pai-tools)
(require 'pai-compaction)

(defvar-local pai--context-messages nil)
(defvar-local pai--context-tokens 0)
(defvar-local pai--model nil)

;;;; Grid / rendering constants (mirroring oh-my-pi's context-usage.ts)

(defconst pai-context--grid-cols 20 "Columns in the usage grid.")
(defconst pai-context--grid-rows 10 "Rows in the usage grid.")
(defconst pai-context--grid-gutter "   " "Gap between the grid and the legend.")

(defconst pai-context--cell-filled "⛁" "Glyph for a category cell.")
(defconst pai-context--cell-messages "⛃" "Glyph for message cells.")
(defconst pai-context--cell-free "⛶" "Glyph for a free-space cell.")
(defconst pai-context--cell-buffer "⛝" "Glyph for the autocompact reserve cell.")
(defconst pai-context--cell-overhead "⛂" "Glyph for reported-minus-estimated cells.")

;; Each category gets an explicit, distinct color so the grid and legend stay
;; clearly differentiated on any theme (light or dark), rather than inheriting
;; from theme-dependent font-lock faces that can look similar.
(defface pai-context-system-prompt-face
  '((((background dark)) :foreground "#7aa2f7")   ; blue
    (t :foreground "#2b5fd9"))
  "Face for the system-prompt category." :group 'pai)
(defface pai-context-tools-face
  '((((background dark)) :foreground "#e0af68")   ; amber
    (t :foreground "#b26a00"))
  "Face for the tools category." :group 'pai)
(defface pai-context-user-face
  '((((background dark)) :foreground "#9ece6a")   ; green
    (t :foreground "#4c8f1f"))
  "Face for user messages." :group 'pai)
(defface pai-context-assistant-face
  '((((background dark)) :foreground "#bb9af7")   ; purple
    (t :foreground "#7b3fe4"))
  "Face for assistant messages." :group 'pai)
(defface pai-context-tool-result-face
  '((((background dark)) :foreground "#2ac3de")   ; cyan
    (t :foreground "#0088a8"))
  "Face for tool-result messages." :group 'pai)
(defface pai-context-overhead-face
  '((((background dark)) :foreground "#8a8f98")   ; grey
    (t :foreground "#6b7078"))
  "Face for the difference between the reported and estimated totals."
  :group 'pai)
(defface pai-context-reserve-face
  '((((background dark)) :foreground "#f7768e")   ; red/pink
    (t :foreground "#c02040"))
  "Face for the autocompact reserve." :group 'pai)
(defface pai-context-free-face '((t :inherit shadow))
  "Face for free space." :group 'pai)

;;;; Token accounting

(defun pai-context--tool-tokens ()
  "Estimate the tokens of the provider tool declarations.
Reuses the shared core estimator so the panel matches the header bar."
  (pai-estimate-tool-tokens (pai-tools-all)))

(defun pai-context--messages ()
  "Return the live context messages for this session (may be nil)."
  (and (boundp 'pai--context-messages) pai--context-messages))

(defun pai-context--model ()
  "Return the model plist for this session, or nil."
  (and (boundp 'pai--model) pai--model))

(defun pai-context--role-tokens (messages role)
  "Return the summed estimated tokens of non-system MESSAGES with ROLE."
  (let ((sum 0))
    (dolist (m messages)
      (when (eq (pai-message-role m) role)
        (cl-incf sum (pai-estimate-tokens m))))
    sum))

(defun pai-context-breakdown ()
  "Compute the context-usage breakdown plist for the current session.
Returns a plist with :model :context-window :used :reserve :free and
:categories (a list of (:id :label :tokens :glyph :face) entries)."
  (let* ((messages (pai-context--messages))
         (model (pai-context--model))
         (window (or (plist-get model :context-window) 0))
         ;; The leading system message holds the assembled system prompt; its
         ;; <tools> section is a short human-readable list.  The full tool
         ;; JSON schemas are sent to the provider separately (pai-context
         ;; MESSAGES TOOLS), so they are their own category and are NOT part of
         ;; the system-prompt tokens.
         (system-msgs (seq-filter #'pai-system-message-p messages))
         (system-prompt-tokens (let ((s 0))
                                 (dolist (m system-msgs) (cl-incf s (pai-estimate-tokens m)))
                                 s))
         (tools-tokens (pai-context--tool-tokens))
         (user-tokens (pai-context--role-tokens messages 'user))
         (assistant-tokens (pai-context--role-tokens messages 'assistant))
         (tool-result-tokens (pai-context--role-tokens messages 'tool-result))
         ;; Anchored message/system total from pai's own estimator (uses
         ;; provider usage when available); this is what the agent loop and
         ;; compaction see.  Tools go over the wire separately, so add them to
         ;; match oh-my-pi's usedTokens (system + tools + messages).
         (messages-used (if messages (pai-estimate-context-tokens messages)
                          (+ system-prompt-tokens user-tokens
                             assistant-tokens tool-result-tokens)))
         (used (+ messages-used tools-tokens))
         ;; What the categories below add up to.  `used' is anchored on the
         ;; provider's own count, which includes request framing and whatever
         ;; the chars-per-token heuristic misjudges, so the two rarely match
         ;; exactly.  Carry the difference as its own category instead of
         ;; printing a legend that cannot account for its own total.
         (estimated (+ system-prompt-tokens tools-tokens user-tokens
                       assistant-tokens tool-result-tokens))
         (unaccounted (max 0 (- used estimated)))
         ;; The autocompact buffer is the distance from the window to the
         ;; compaction trigger, not the raw reserve: that way a configured
         ;; `:compact-threshold' is reflected in the panel.  When compaction
         ;; is off nothing is held back, so the buffer is zero.
         (reserve (min (if (pai-compaction-enabled-p)
                           (max 0 (- window (pai-compaction-threshold-tokens window)))
                         0)
                       (max 0 (- window used))))
         (free (max 0 (- window used reserve)))
         (categories
          (list
           (list :id 'system-prompt :label "System prompt"
                 :tokens system-prompt-tokens
                 :glyph pai-context--cell-filled :face 'pai-context-system-prompt-face)
           (list :id 'tools :label "Tools"
                 :tokens tools-tokens
                 :glyph pai-context--cell-filled :face 'pai-context-tools-face)
           (list :id 'user :label "User messages"
                 :tokens user-tokens
                 :glyph pai-context--cell-messages :face 'pai-context-user-face)
           (list :id 'assistant :label "Assistant messages"
                 :tokens assistant-tokens
                 :glyph pai-context--cell-messages :face 'pai-context-assistant-face)
           (list :id 'tool-result :label "Tool results"
                 :tokens tool-result-tokens
                 :glyph pai-context--cell-messages :face 'pai-context-tool-result-face))))
    ;; Only worth a row when it is big enough to explain a visible gap.
    (when (> unaccounted (max 1 (/ window 1000)))
      (setq categories
            (append categories
                    (list (list :id 'unaccounted :label "Provider overhead (reported)"
                                :tokens unaccounted
                                :glyph pai-context--cell-overhead
                                :face 'pai-context-overhead-face)))))
    (list :model model :context-window window
          :used used :reserve reserve :free free
          :estimated estimated :unaccounted unaccounted
          :categories categories)))

;;;; Grid planning

(defun pai-context--plan-cells (breakdown)
  "Return a list of (GLYPH . FACE) cells for BREAKDOWN's grid."
  (let* ((window (plist-get breakdown :context-window))
         (total-cells (* pai-context--grid-cols pai-context--grid-rows))
         (cells '()))
    (if (<= window 0)
        (dotimes (_ total-cells)
          (push (cons pai-context--cell-free 'pai-context-free-face) cells))
      (let* ((per-cell (/ (float window) total-cells))
             (ratio (lambda (tokens)
                      (if (<= tokens 0) 0 (max 1 (round (/ tokens per-cell))))))
             (counts (mapcar (lambda (c)
                               (cons c (funcall ratio (plist-get c :tokens))))
                             (plist-get breakdown :categories)))
             (buffer-count (funcall ratio (plist-get breakdown :reserve)))
             (used-count (apply #'+ (mapcar #'cdr counts)))
             (max-usable (- total-cells buffer-count)))
        ;; Trim from the largest categories first so small ones stay visible.
        (when (> used-count max-usable)
          (let ((overflow (- used-count max-usable))
                (order (sort (copy-sequence counts)
                             (lambda (a b) (> (cdr a) (cdr b))))))
            (dolist (entry order)
              (while (and (> overflow 0) (> (cdr entry) 1))
                (setcdr entry (1- (cdr entry)))
                (setq overflow (1- overflow))))
            (setq used-count (apply #'+ (mapcar #'cdr counts)))
            (when (> (+ used-count buffer-count) total-cells)
              (setq buffer-count (max 0 (- total-cells used-count))))))
        (dolist (entry counts)
          (let ((cat (car entry)))
            (dotimes (_ (cdr entry))
              (push (cons (plist-get cat :glyph) (plist-get cat :face)) cells))))
        (let ((free-count (max 0 (- total-cells (length cells) buffer-count))))
          (dotimes (_ free-count)
            (push (cons pai-context--cell-free 'pai-context-free-face) cells)))
        (dotimes (_ buffer-count)
          (push (cons pai-context--cell-buffer 'pai-context-reserve-face) cells))
        (while (< (length cells) total-cells)
          (push (cons pai-context--cell-free 'pai-context-free-face) cells))))
    (seq-take (nreverse cells) total-cells)))

;;;; Formatting helpers

(defun pai-context--fmt-count (n)
  "Format token count N with thousands separators."
  (let* ((s (number-to-string (max 0 (round n))))
         (out "") (len (length s)))
    (dotimes (i len)
      (when (and (> i 0) (= 0 (% (- len i) 3)))
        (setq out (concat out ",")))
      (setq out (concat out (substring s i (1+ i)))))
    out))

(defun pai-context--pct (part whole)
  "Return PART/WHOLE as a percent string."
  (if (<= whole 0) "0%"
    (let ((p (* 100.0 (/ (float part) whole))))
      (cond ((and (> p 0) (< p 0.05)) "<0.1%")
            (t (format "%.1f%%" p))))))

(defun pai-context--cell-pixel-width ()
  "Return the pixel width one grid cell should occupy, or nil.

The cell glyphs live in different Unicode blocks and are therefore often
served by different fallback fonts, whose advance widths do not match
\(e.g. U+26C1 at 14px next to U+26F6 at 20px).  Emacs lays the grid out in
character columns, so those uneven glyphs make every row drift sideways.
On graphical frames we pad each cell to a common pixel width instead.
Returns nil on ttys, where character cells are uniform anyway."
  (when (and (display-graphic-p) (fboundp 'string-pixel-width))
    (let ((w 0))
      (dolist (glyph (list pai-context--cell-filled
                           pai-context--cell-messages
                           pai-context--cell-overhead
                           pai-context--cell-free
                           pai-context--cell-buffer))
        (setq w (max w (string-pixel-width glyph))))
      (+ w (frame-char-width)))))

(defun pai-context--stretch (pixels)
  "Return a space displayed as exactly PIXELS pixels wide."
  (propertize " " 'display (list 'space :width (list (max 0 pixels)))))

(defun pai-context--dot (glyph face &optional pad)
  "Return GLYPH propertized with FACE, followed by its cell padding.
With PAD (a pixel width) the glyph plus padding occupies exactly PAD
pixels; otherwise a single ordinary space is appended."
  (let ((cell (propertize glyph 'face face)))
    (concat cell
            (if pad
                (pai-context--stretch (- pad (string-pixel-width glyph)))
              " "))))

;;;; Legend

(defun pai-context--legend-lines (breakdown &optional pad)
  "Return the legend lines (list of strings) for BREAKDOWN.
PAD is the cell pixel width used to align the legend glyphs, if any."
  (let* ((model (plist-get breakdown :model))
         (window (plist-get breakdown :context-window))
         (used (plist-get breakdown :used))
         (reserve (plist-get breakdown :reserve))
         (free (plist-get breakdown :free))
         (win-label (pai-context--fmt-count window))
         (name (or (plist-get model :name) (plist-get model :id) "no model"))
         (key (if model (pai-model-key model) "unknown"))
         (lines '()))
    (push (concat (propertize name 'face 'bold)
                  (propertize (format " (%s context)" win-label) 'face 'shadow))
          lines)
    (push (propertize key 'face 'shadow) lines)
    (push (concat (propertize (pai-context--fmt-count used) 'face 'bold)
                  (propertize (format "/%s tokens " win-label) 'face 'shadow)
                  (propertize (format "(%s)" (pai-context--pct used window)) 'face 'shadow))
          lines)
    (push "" lines)
    (push (propertize (if (> (or (plist-get breakdown :unaccounted) 0) 0)
                          "Usage by category (reported)"
                        "Estimated usage by category")
                      'face 'shadow)
          lines)
    (dolist (cat (plist-get breakdown :categories))
      (push (format "%s%s: %s %s"
                    (pai-context--dot (plist-get cat :glyph) (plist-get cat :face) pad)
                    (plist-get cat :label)
                    (propertize (pai-context--fmt-count (plist-get cat :tokens)) 'face 'bold)
                    (propertize (format "tokens (%s)"
                                        (pai-context--pct (plist-get cat :tokens) window))
                                'face 'shadow))
            lines))
    (push (format "%sFree space: %s %s"
                  (pai-context--dot pai-context--cell-free 'pai-context-free-face pad)
                  (propertize (pai-context--fmt-count free) 'face 'bold)
                  (propertize (format "(%s)" (pai-context--pct free window)) 'face 'shadow))
          lines)
    (when (> reserve 0)
      (push (format "%sAutocompact buffer: %s %s"
                    (pai-context--dot pai-context--cell-buffer 'pai-context-reserve-face pad)
                    (propertize (pai-context--fmt-count reserve) 'face 'bold)
                    (propertize (format "tokens (%s)" (pai-context--pct reserve window))
                                'face 'shadow))
            lines))
    (nreverse lines)))

;;;; Panel

(defun pai-context--render (breakdown)
  "Render BREAKDOWN as a multi-line propertized string (grid + legend)."
  (if (<= (plist-get breakdown :context-window) 0)
      (propertize
       "Context usage is unavailable: no model is selected for this session."
       'face 'shadow)
    (let* ((pad (pai-context--cell-pixel-width))
           (cells (pai-context--plan-cells breakdown))
           (legend (pai-context--legend-lines breakdown pad))
           (total-lines (max pai-context--grid-rows (length legend)))
           (blank (if pad
                      (pai-context--stretch (* pai-context--grid-cols pad))
                    (make-string (* pai-context--grid-cols 2) ?\s)))
           (out '()))
      (dotimes (row total-lines)
        (let ((grid
               (if (< row pai-context--grid-rows)
                   (mapconcat
                    (lambda (col)
                      (let ((cell (nth (+ (* row pai-context--grid-cols) col) cells)))
                        (pai-context--dot (car cell) (cdr cell) pad)))
                    (number-sequence 0 (1- pai-context--grid-cols))
                    "")
                 blank))
              (leg (or (nth row legend) "")))
          (push (if (> (length leg) 0)
                    (concat grid pai-context--grid-gutter leg)
                  grid)
                out)))
      (string-join (nreverse out) "\n"))))

;;;; Full-context debug dump

(defun pai-context--dump-tool (tool)
  "Return a readable text block for a single TOOL declaration."
  (let ((decl (pai-tool-declaration tool)))
    (format "### %s\n%s\n\nParameters:\n%s\n"
            (plist-get decl :name)
            (or (plist-get decl :description) "")
            (condition-case nil
                (let ((json-encoding-pretty-print t))
                  (if (fboundp 'json-encode)
                      (json-encode (plist-get decl :parameters))
                    (pai-json-encode (plist-get decl :parameters))))
              (error "(unencodable)")))))

(defun pai-context--dump-message (m index)
  "Return a readable text block for message M at INDEX."
  (let* ((role (pai-message-role m))
         (header (format "════════ [%d] %s ════════" index (upcase (symbol-name role)))))
    (pcase role
      ('assistant
       (let* ((text (pai-content-text (pai-message-content m)))
              (thinking (mapconcat
                         (lambda (b)
                           (when (eq (pai-block-type b) 'thinking)
                             (concat "[thinking]\n" (or (plist-get b :thinking) ""))))
                         (pai-message-content m) ""))
              (calls (mapconcat
                      (lambda (tc)
                        (format "[tool_call %s]\n%s"
                                (plist-get tc :name)
                                (condition-case nil
                                    (pai-json-encode (or (plist-get tc :arguments)
                                                         (pai-json-empty-object)))
                                  (error "{}"))))
                      (pai-message-tool-calls m) "\n")))
         (string-join
          (delq nil (list header
                          (unless (string-empty-p (string-trim thinking)) thinking)
                          (unless (string-empty-p (string-trim text)) text)
                          (unless (string-empty-p (string-trim calls)) calls)))
          "\n")))
      ('tool-result
       (format "%s\ntool: %s%s\n%s"
               header
               (or (plist-get m :tool-name) "")
               (if (eq (plist-get m :is-error) t) " (error)" "")
               (pai-content-text (plist-get m :content))))
      (_ (format "%s\n%s" header (pai-content-text (pai-message-content m)))))))

(defun pai-context--dump-text ()
  "Return the full assembled context as a plain-text string."
  (let* ((messages (pai-context--messages))
         (model (pai-context--model))
         (tools (pai-tools-all))
         (breakdown (pai-context-breakdown))
         (parts '()))
    (push (format "# pai context dump\n\nModel: %s\nContext window: %s tokens\nEstimated used: %s tokens (%s)\n"
                  (if model (pai-model-key model) "(none)")
                  (pai-context--fmt-count (plist-get breakdown :context-window))
                  (pai-context--fmt-count (plist-get breakdown :used))
                  (pai-context--pct (plist-get breakdown :used)
                                    (plist-get breakdown :context-window)))
          parts)
    (push (format "\n═══════════════════════════════════════\n TOOL DEFINITIONS (%d)\n═══════════════════════════════════════\n"
                  (length tools))
          parts)
    (push (mapconcat #'pai-context--dump-tool tools "\n") parts)
    (push (format "\n═══════════════════════════════════════\n MESSAGES (%d)\n═══════════════════════════════════════\n"
                  (length messages))
          parts)
    (let ((i 0))
      (dolist (m messages)
        (push (pai-context--dump-message m i) parts)
        (push "" parts)
        (setq i (1+ i))))
    (string-join (nreverse parts) "\n")))

(defun pai-context--open-dump ()
  "Open a new buffer showing the full assembled context, and return its name."
  (let* ((text (pai-context--dump-text))
         (buf (get-buffer-create "*pai context dump*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (when (fboundp 'markdown-mode)
        (ignore-errors (markdown-mode)))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))
    (display-buffer buf)
    (buffer-name buf)))

;;;; Command handlers

(declare-function pai--insert "pai-ui")
(declare-function pai--ensure-fresh-line "pai-ui")
(defvar pai--output-marker)

(defun pai-context-command (_args _ctx)
  "Handler for `/context': show the token-usage breakdown panel.
The panel is inserted directly into the transcript so each category keeps its
own color; `pai--render-note' would flatten the per-cell faces to a single
note face.  Returns nil so the dispatcher does not render it again."
  (let ((panel (pai-context--render (pai-context-breakdown))))
    (if (and (boundp 'pai--output-marker) (marker-buffer pai--output-marker)
             (fboundp 'pai--insert))
        (progn
          (pai--ensure-fresh-line)
          (pai--insert (concat "\n" (string-trim-right panel) "\n"))
          nil)
      ;; No live transcript (e.g. batch/test): fall back to :message.
      (list :message panel))))

(defun pai-context-dump-command (_args _ctx)
  "Handler for `/context-dump': open the whole context in a new buffer."
  (list :message (format "Opened full context in %s" (pai-context--open-dump))))

;;;; Extension entry point

(pai-register-extension
 (lambda (api)
   (pai-ext-register-command
    api "context"
    :description "Show the context-usage breakdown by category"
    :handler #'pai-context-command)
   (pai-ext-register-command
    api "context-dump"
    :description "Open the full assembled context in a new buffer for debugging"
    :handler #'pai-context-dump-command))
 "context")

(provide 'pai-context)
;;; pai-context.el ends here
