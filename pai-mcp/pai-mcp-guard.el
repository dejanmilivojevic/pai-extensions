;;; pai-mcp-guard.el --- MCP approval and output boundaries -*- lexical-binding: t; -*-

;;; Commentary:
;; Non-modal approval broker and bounded MCP tool output.  Approvals never
;; prompt in a minibuffer; closing a pending approval buffer denies it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'button)
(require 'pai-core)
(require 'pai-mcp-config)
(require 'pai-mcp-direct)
(require 'pai-session)

(defcustom pai-mcp-output-max-bytes (* 50 1024)
  "Default maximum UTF-8 bytes of model-facing MCP text."
  :type 'natnum :group 'pai-mcp)
(defcustom pai-mcp-output-max-lines 2000
  "Default maximum lines of model-facing MCP text."
  :type 'natnum :group 'pai-mcp)
(defcustom pai-mcp-output-details-max-bytes (* 16 1024)
  "Default maximum UTF-8 JSON bytes of raw MCP result details."
  :type 'natnum :group 'pai-mcp)

(defvar pai-mcp-guard--approvals (make-hash-table :test 'equal)
  "Session-scoped approval identities.  Never persisted as blanket trust.")
(defvar-local pai-mcp-guard--settle nil
  "Function accepting an approval decision for this pending buffer.")
(defvar pai--session)

(defun pai-mcp-guard--scope (dir)
  "Return the current conversation and project approval scope for DIR."
  (list (or (and (boundp 'pai--session) pai--session
                 (pai-session-id pai--session))
            (current-buffer))
        (expand-file-name (or dir default-directory))))

(defun pai-mcp-guard-clear-approvals ()
  "Forget all MCP session approvals.  Pending dialogs remain pending."
  (interactive)
  (clrhash pai-mcp-guard--approvals))

(defun pai-mcp-guard--closed ()
  "Deny a request whose buffer is being killed."
  (when pai-mcp-guard--settle (funcall pai-mcp-guard--settle 'deny)))

(define-derived-mode pai-mcp-approval-mode special-mode "MCP Approval"
  "Non-modal MCP approval dialog.  Activate a button with RET."
  (add-hook 'kill-buffer-hook #'pai-mcp-guard--closed nil t))

(defun pai-mcp-guard-request (key title body on-allow on-deny &optional dir)
  "Ask asynchronously to approve KEY, displaying TITLE and BODY.
ON-ALLOW and ON-DENY receive no arguments and are called at most once.
Allow once does not cache; Allow for session remembers KEY in the current
conversation and DIR.  A nil KEY disables session approval entirely.
Return the pending button buffer, or nil for a cached or headless decision.
Noninteractive Emacs denies requests rather than waiting for unavailable UI."
  (let ((identity (and key (list (pai-mcp-guard--scope dir) key))))
    (cond
     ((and identity (gethash identity pai-mcp-guard--approvals))
      (funcall on-allow) nil)
     (noninteractive (funcall on-deny) nil)
     (t
      (let ((buffer (generate-new-buffer "*MCP Approval*"))
            (origin (current-buffer))
            (settled nil))
        (with-current-buffer buffer
          (pai-mcp-approval-mode)
          (setq-local pai-mcp-guard--settle
                      (lambda (decision)
                        (unless settled
                          (setq settled t)
                          (when (and identity (buffer-live-p origin)
                                     (eq decision 'session))
                            (puthash identity t pai-mcp-guard--approvals))
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (setq pai-mcp-guard--settle nil)
                              (let ((inhibit-read-only t))
                                (goto-char (point-max))
                                (insert (format "\nDecision: %s\n" decision)))))
                          (if (buffer-live-p origin)
                              (with-current-buffer origin
                                (funcall (if (memq decision '(once session))
                                             on-allow on-deny)))
                            (funcall on-deny)))))
          (let ((inhibit-read-only t)
                (settle pai-mcp-guard--settle))
            (insert title "\n\n" body "\n\n")
            (dolist (choice (append '(("Allow once" . once))
                                    (when key '(("Allow for session" . session)))
                                    '(("Deny" . deny))))
              (let ((decision (cdr choice)))
                (insert-text-button (car choice) 'follow-link t
                                    'action (lambda (_) (funcall settle decision))))
              (insert "    "))
            (goto-char (point-min))))
        (display-buffer buffer)
        buffer)))))

(defun pai-mcp-guard--canonical (value)
  "Normalize JSON VALUE's object key order without copying closures."
  (cond
   ((hash-table-p value)
    (let (pairs)
      (maphash (lambda (k v) (push (cons (format "%s" k) v) pairs)) value)
      (let ((table (make-hash-table :test 'equal)))
        (dolist (pair (sort pairs (lambda (a b) (string< (car a) (car b)))))
          (puthash (car pair) (pai-mcp-guard--canonical (cdr pair)) table))
        table)))
   ((and (consp value) (keywordp (car value)))
    (let (pairs out)
      (while value
        (push (cons (car value) (cadr value)) pairs)
        (setq value (cddr value)))
      (dolist (pair (sort pairs (lambda (a b) (string< (symbol-name (car a))
                                                      (symbol-name (car b))))))
        (setq out (nconc out (list (car pair) (pai-mcp-guard--canonical (cdr pair))))))
      out))
   ((vectorp value) (apply #'vector (mapcar #'pai-mcp-guard--canonical value)))
   ((consp value) (mapcar #'pai-mcp-guard--canonical value))
   (t value)))

(defun pai-mcp-guard--hash (value)
  "Return a stable digest of JSON VALUE."
  (secure-hash 'sha256 (pai-json-encode (pai-mcp-guard--canonical value))))

(defun pai-mcp-guard--names (server tool settings &optional legacy)
  "Return SERVER TOOL approval candidates under SETTINGS.
LEGACY includes previous dot/hyphen-to-underscore emitted aliases."
  (let ((names (list tool)))
    (dolist (mode '("server" "short" "mcp" "none"))
      (push (pai-mcp--prefixed-name server (list :toolPrefix mode)
                                    settings tool) names)
      (when legacy
        (push (pai-mcp--prefixed-name
               server (list :toolPrefix mode) settings
               (replace-regexp-in-string "[.-]" "_" tool)) names)))
    (when legacy
      (setq names (append names (mapcar (lambda (name)
                                         (replace-regexp-in-string "-" "_" name)) names))))
    (delete-dups names)))

(defun pai-mcp-guard--required-p (server tool def settings &optional dir)
  "Return whether SERVER TOOL needs approval under DEF and SETTINGS for DIR."
  (require 'pai-mcp-direct)
  (let* ((local (plist-member def :approveTools))
         (approval (if local (plist-get def :approveTools)
                     (plist-get settings :approveTools))))
    (or (eq approval t)
        (and (or (listp approval) (vectorp approval))
             (let ((patterns (seq-filter #'stringp approval))
                   (names (pai-mcp-guard--names server tool settings)))
               (or (apply #'pai-mcp--glob-match-any patterns names)
                   (let ((legacy (cl-set-difference
                                  (pai-mcp-guard--names server tool settings t)
                                  names :test #'equal))
                         others)
                     (dolist (pair (if local (list (cons server def))
                                     (pai-mcp-load-config dir)))
                       (dolist (metadata (pai-mcp--server-tools (car pair)))
                         (setq others
                               (append (pai-mcp-guard--names
                                        (car pair) (plist-get metadata :name) settings)
                                       others))))
                     (setq others (cl-set-difference others names :test #'equal))
                     (seq-some
                      (lambda (pattern)
                        (and (apply #'pai-mcp--glob-match-any (list pattern) legacy)
                             (not (apply #'pai-mcp--glob-match-any (list pattern) others))))
                      patterns))))))))

(defun pai-mcp-guard-approve (server tool args on-allow on-deny &optional dir)
  "Gate SERVER TOOL with ARGS using approveTools, then call ON-ALLOW/ON-DENY.
Callbacks receive no arguments.  Session approval covers this exact tool
schema, server definition and arguments, never all future calls to a tool."
  (let* ((def (or (plist-get (pai-mcp--server server) :def)
                  (pai-mcp--server-def server dir)))
         (settings (pai-mcp-settings dir)))
    (if (not (pai-mcp-guard--required-p server tool def settings dir))
        (progn (funcall on-allow) nil)
      (let* ((metadata (seq-find (lambda (item) (equal tool (plist-get item :name)))
                                 (pai-mcp--server-tools server)))
             (key (list 'tool server tool
                        (pai-mcp-guard--hash (list :server def :tool metadata))
                        (pai-mcp-guard--hash (or args (pai-json-empty-object)))))
             (json (pai-json-encode (or args (pai-json-empty-object)))))
        (pai-mcp-guard-request
         key (format "MCP: %s wants to run %s" server tool)
         (concat "Arguments:\n" (if (> (length json) 2000)
                                     (concat (substring json 0 2000)
                                             "\n[Preview truncated; session approval covers these exact arguments.]")
                                   json))
         on-allow on-deny dir)))))

;;;; Output boundary

(defun pai-mcp-guard--bytes (text)
  "Return TEXT's UTF-8 byte count."
  (string-bytes (encode-coding-string text 'utf-8 t)))

(defun pai-mcp-guard--prefix (text max-bytes max-lines)
  "Return a valid Unicode prefix of TEXT within MAX-BYTES and MAX-LINES."
  (let ((end (length text)) (start 0) (lines 1))
    (if (or (<= max-bytes 0) (<= max-lines 0)) ""
      (while (and (< lines max-lines) (string-match "\n" text start))
        (setq start (match-end 0) lines (1+ lines)))
      (when (and (= lines max-lines) (string-match "\n" text start))
        (setq end (match-beginning 0)))
      (if (<= (pai-mcp-guard--bytes (substring text 0 end)) max-bytes)
          (substring text 0 end)
        (let ((low 0) (high end))
          (while (< low high)
            (let ((mid (/ (+ low high 1) 2)))
              (if (<= (pai-mcp-guard--bytes (substring text 0 mid)) max-bytes)
                  (setq low mid)
                (setq high (1- mid)))))
          (substring text 0 low))))))

(defun pai-mcp-guard--positive (value fallback)
  "Use positive numeric VALUE, rounded down, or FALLBACK."
  (if (and (numberp value) (>= value 1)) (floor value) (max 1 fallback)))

(defun pai-mcp-guard--options (dir def)
  "Resolve outputGuard options in DIR, with optional per-server DEF override."
  (let* ((settings (pai-mcp-settings dir))
         (configured (if (plist-member def :outputGuard)
                         (plist-get def :outputGuard)
                       (plist-get settings :outputGuard)))
         (tuning (and (listp configured) configured))
         (override (downcase (string-trim (or (getenv "MCP_OUTPUT_GUARD") "")))))
    (list :enabled (cond ((member override '("0" "false" "no" "off")) nil)
                          ((member override '("1" "true" "yes" "on")) t)
                          (t (not (eq configured :false))))
          :maxBytes (pai-mcp-guard--positive (plist-get tuning :maxBytes)
                                             pai-mcp-output-max-bytes)
          :maxLines (pai-mcp-guard--positive (plist-get tuning :maxLines)
                                             pai-mcp-output-max-lines)
          :detailsMaxBytes (pai-mcp-guard--positive (plist-get tuning :detailsMaxBytes)
                                                    pai-mcp-output-details-max-bytes))))

(defun pai-mcp-guard--save (text suffix &optional binary)
  "Save TEXT privately with SUFFIX; BINARY selects unibyte data.
Return (:path FILE) or (:error MESSAGE).  Paths are local, random, and do
not incorporate server-supplied names, URIs, or MIME types."
  (let (directory path)
    (condition-case err
        (let ((default-directory temporary-file-directory))
          (setq directory (make-temp-file "pai-mcp-output-" t)
                path (make-temp-file (expand-file-name "output-" directory) nil suffix))
          (set-file-modes directory #o700)
          (let ((coding-system-for-write (if binary 'no-conversion 'utf-8-unix)))
            (with-temp-buffer
              (when binary (set-buffer-multibyte nil))
              (insert text)
              (write-region (point-min) (point-max) path nil 'silent)))
          (set-file-modes path #o600)
          (list :path path))
      (error
       (when directory (ignore-errors (delete-directory directory t)))
       (list :error (error-message-string err))))))

(defun pai-mcp-guard--reference (saved label)
  "Describe SAVED artifact with LABEL, including honest write failures."
  (if-let ((path (plist-get saved :path)))
      (format "[%s saved to: %s]" label path)
    (format "[%s could not be saved: %s]" label (plist-get saved :error))))

(defun pai-mcp-guard--binary (data mime)
  "Decode base64 DATA to a private file and return a reference for MIME."
  (condition-case err
      (pai-mcp-guard--reference
       (pai-mcp-guard--save (base64-decode-string data) ".bin" t)
       (format "Binary %s" (or mime "application/octet-stream")))
    (error (format "[Invalid binary content: %s]" (error-message-string err)))))

(defun pai-mcp-guard-result (result &optional dir def)
  "Convert protocol RESULT to a bounded pai tool result for DIR and DEF.
Text exceeding outputGuard.maxBytes/maxLines is saved privately, as are
binary blocks too large for the aggregate image budget.  Small images stay
native.  Oversized raw details are replaced with a file reference bounded
by detailsMaxBytes.  Protocol isError survives every conversion path.
Spill files are retained for later read/grep; the user may remove them."
  (let* ((options (pai-mcp-guard--options dir def))
         (enabled (plist-get options :enabled))
         (max-bytes (plist-get options :maxBytes))
         (max-lines (plist-get options :maxLines))
         (details-max (plist-get options :detailsMaxBytes))
         (image-bytes 0) texts images details guard)
    (dolist (block (append (plist-get result :content) nil))
      (pcase (plist-get block :type)
        ("text" (push (or (plist-get block :text) "") texts))
        ("image"
         (let* ((data (or (plist-get block :data) ""))
                (mime (or (plist-get block :mimeType) "image/png"))
                (bytes (string-bytes data)))
           (if (or (not enabled) (<= (+ image-bytes bytes) max-bytes))
               (progn (cl-incf image-bytes bytes) (push (pai-image data mime) images))
             (push (pai-mcp-guard--binary data mime) texts))))
        ("audio" (push (pai-mcp-guard--binary (or (plist-get block :data) "")
                                               (plist-get block :mimeType)) texts))
        ("resource"
         (let ((resource (plist-get block :resource)))
           (push (if (plist-member resource :text)
                     (or (plist-get resource :text) "")
                   (pai-mcp-guard--binary (or (plist-get resource :blob) "")
                                          (plist-get resource :mimeType))) texts)))
        ("resource_link"
         (push (format "Resource: %s (%s)" (or (plist-get block :name) "")
                       (or (plist-get block :uri) "")) texts))
        (_ (push (pai-json-encode block) texts))))
    (when (and (null texts) (null images) (plist-member result :structuredContent))
      (push (pai-json-encode (plist-get result :structuredContent)) texts))
    (let* ((text (string-join (nreverse texts) "\n"))
           (errp (eq (plist-get result :isError) t))
           (raw (pai-json-encode result)))
      (when (and (string-empty-p text) (null images))
        (setq text (if errp "MCP tool error" "(no output)")))
      (when enabled
        (let ((preview (pai-mcp-guard--prefix text max-bytes max-lines)))
          (unless (equal text preview)
            (let* ((saved (pai-mcp-guard--save text ".txt"))
                   (notice (pai-mcp-guard--reference saved "Full MCP output"))
                   (budget (- max-bytes (pai-mcp-guard--bytes notice) 1)))
              (setq guard (append (list :truncated t :originalBytes (pai-mcp-guard--bytes text)
                                        :maxBytes max-bytes :maxLines max-lines)
                                  saved))
              ;; Tiny limits cannot hold a path: retain the full reference in
              ;; outputGuard details and keep model-facing text strictly bounded.
              (setq text
                    (if (and (> budget 0) (> max-lines 1))
                        (let ((prefix (pai-mcp-guard--prefix text budget (1- max-lines))))
                          (if (string-empty-p prefix) notice (concat prefix "\n" notice)))
                      (pai-mcp-guard--prefix notice max-bytes max-lines)))))))
      (if (or (not enabled) (<= (pai-mcp-guard--bytes raw) details-max))
          (setq details (list :mcpResult result))
        (let* ((saved (pai-mcp-guard--save raw ".json"))
               (summary (append (list :omitted t :isError (if errp t :false)) saved)))
          (when (> (pai-mcp-guard--bytes (pai-json-encode summary)) details-max)
            (setq summary '(:omitted t)))
          (when (<= (pai-mcp-guard--bytes (pai-json-encode summary)) details-max)
            (setq details (list :mcpResult summary)))
          (unless (plist-get summary :path)
            ;; Even very small details limits must not orphan the full artifact.
            (setq guard (append guard (list :resultArtifact saved))))))
      (when guard (setq details (plist-put details :outputGuard guard)))
      (list :content (append (unless (string-empty-p text) (list (pai-text text)))
                             (nreverse images))
            :is-error (if errp t :false) :details details))))

(provide 'pai-mcp-guard)
;;; pai-mcp-guard.el ends here
