;;; pai-memory-privacy.el --- Forget, private sessions, custom redaction -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 G3 and G1.
;;
;; Redaction: `:memory :redact-patterns' (strings, or [NAME, REGEXP] pairs)
;; extends the built-in secret filter (`pai-memory-redact', which the search
;; index applies too); text forgotten with `/memory forget' joins it, so it
;; does not come back through a reindex or a later memory write.
;;
;; Private sessions: `/memory private' records a `memory.state {private: t}'
;; override.  A private session is not observed, consolidated, promoted,
;; recalled into or indexed, and its existing index rows are dropped.
;;
;; Forgetting: `/memory forget TEXT' finds TEXT (or a regexp) in everything
;; memory derived from conversations -- long-term memory, topic files and
;; journeys, the observation archive, learned skills, skill usage notes,
;; observation ledgers and the search index -- lists it, and after
;; confirmation:
;;   * rewrites the Markdown files without the matching entries or lines,
;;     after copying them to ~/.pai/memory/backups/<time>/, and logs each
;;     rewrite so `/memory undo' restores it;
;;   * hides matching observations with a `memory.redacted' entry on every
;;     branch of their session (session files are append-only);
;;   * deletes matching index rows.
;; Raw conversation text in session files is not touched.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-session)
(require 'pai-memory-settings)
(require 'pai-memory-budget)
(require 'pai-memory-store)
(require 'pai-memory-ledger)
(require 'pai-memory-skills)
(require 'pai-memory-search)

(defvar pai--session)
(declare-function pai-memory--skill-dirs "pai-memory-promote" ())

;;;; Private sessions

(defun pai-memory-set-private (session private)
  "Make SESSION private (PRIVATE non-nil) or not; return a message.
The rules live where they apply: `pai-memory-private-p' gates the session
layer, learning and recall, and the indexer skips private sessions."
  (pai-memory-set-session-state session :private (if private t :false))
  (when (and private (pai-memory-search-available-p))
    (sqlite-execute (pai-memory--db) "INSERT OR REPLACE INTO private (session) VALUES (?)"
                    (list (pai-session-id session)))
    (sqlite-execute (pai-memory--db) "DELETE FROM docs WHERE session=?" (list (pai-session-id session))))
  (if private
      "This session is private: not observed, consolidated, promoted, recalled into or indexed"
    "This session is no longer private; it is indexed again from the next idle moment"))

;;;; Forget: finding matches

(defun pai-memory--matcher (text regexp)
  "Return a predicate matching TEXT literally, or as a regexp when REGEXP."
  (let ((re (if regexp text (regexp-quote text))))
    (lambda (s) (let ((case-fold-search t)) (and (stringp s) (string-match-p re s))))))

(defun pai-memory--forget-md-files (scope cwd)
  "Return (FILE . KIND) for Markdown memory files in SCOPE (project or all)."
  (let ((out '())
        (projects (if (equal scope "all")
                      (and (file-directory-p (pai-memory-dir "projects"))
                           (directory-files (pai-memory-dir "projects") t "\\`[^.]"))
                    (list (pai-memory-project-dir cwd)))))
    (dolist (f (list (pai-memory-dir "USER.md") (pai-memory-dir "MEMORY.md")))
      (when (file-exists-p f) (push (cons f 'entries) out)))
    (dolist (pd projects)
      (let ((mf (expand-file-name "MEMORY.md" pd)))
        (when (file-exists-p mf) (push (cons mf 'entries) out)))
      (let ((sd (expand-file-name "sessions" pd)))
        (when (file-directory-p sd)
          (dolist (sess (directory-files sd t "\\`[^.]"))
            (when (file-directory-p sess)
              (dolist (f (directory-files sess t "\\.md\\'"))
                (unless (equal (file-name-nondirectory f) "INDEX.md")
                  (push (cons f 'lines) out))))))))
    (dolist (s (ignore-errors (pai-discover-skills (and (fboundp 'pai-memory--skill-dirs)
                                                        (pai-memory--skill-dirs)))))
      (when (pai-memory-learned-skill-p s)
        (push (cons (plist-get s :path) 'skill) out)))
    (nreverse out)))

(defun pai-memory--forget-session-files (scope cwd)
  "Return the session files in SCOPE."
  (if (equal scope "all")
      (pai-memory--session-files)
    (let ((dir (pai-session-directory cwd)))
      (directory-files dir t "\\.jsonl\\'"))))

(defun pai-memory--matching-lines (text match)
  "Return the lines of TEXT that MATCH, excluding front-matter keys."
  (seq-filter match (split-string text "\n")))

(defun pai-memory-forget-plan (text &optional regexp scope cwd)
  "Return what `/memory forget' would change for TEXT.
The plan is (:files ((FILE KIND BEFORE AFTER N)...) :observations
\((SESSION-FILE . IDS)...) :index N :usage (NAMES...))."
  (let* ((match (pai-memory--matcher text regexp))
         (cwd (or cwd default-directory))
         (files '()) (obs '()) (usage '()))
    (dolist (spec (pai-memory--forget-md-files scope cwd))
      (let* ((file (car spec)) (before (pai-memory--read-file file)))
        (when (funcall match before)
          (let* ((after
                  (pcase (cdr spec)
                    ('entries (pai-memory-render-entries
                               (seq-remove match (pai-memory-parse-entries before))))
                    ('skill
                     ;; keep the front-matter intact; drop matching body lines
                     (let* ((parsed (pai-skills--parse-frontmatter before))
                            (head (substring before 0 (- (length before) (length (cdr parsed))))))
                       (concat head (string-join (seq-remove match (split-string (cdr parsed) "\n"))
                                                 "\n"))))
                    (_ (string-join (seq-remove match (split-string before "\n")) "\n"))))
                 (n (if (eq (cdr spec) 'entries)
                        (length (seq-filter match (pai-memory-parse-entries before)))
                      (length (pai-memory--matching-lines before match)))))
            (unless (equal before after)
              (push (list file (cdr spec) before after n) files))))))
    (dolist (sf (pai-memory--forget-session-files scope cwd))
      (let ((ids '()))
        (with-temp-buffer
          (insert-file-contents sf)
          (goto-char (point-min))
          (while (search-forward "\"memory.observations\"" nil t)
            (let ((e (ignore-errors (pai-json-decode (buffer-substring-no-properties
                                                      (line-beginning-position) (line-end-position))))))
              (dolist (o (append (plist-get (plist-get e :data) :observations) nil))
                (when (funcall match (plist-get o :content)) (push (plist-get o :id) ids))))
            (forward-line 1)))
        (when ids (push (cons sf (nreverse ids)) obs))))
    (let ((table (pai-memory-usage-read)))
      (while table
        (when (seq-some match (append (plist-get (cadr table) :notes) nil))
          (push (substring (symbol-name (car table)) 1) usage))
        (setq table (cddr table))))
    (list :files (nreverse files) :observations (nreverse obs) :usage usage
          :index (if (pai-memory-search-available-p)
                     (length (pai-memory--index-matches text regexp))
                   0))))

(defun pai-memory--index-matches (text regexp)
  "Return the rowids of index rows containing TEXT (a regexp when REGEXP)."
  (let ((match (pai-memory--matcher text regexp)))
    (delq nil (mapcar (lambda (r) (and (funcall match (cadr r)) (car r)))
                      (sqlite-select (pai-memory--db)
                                     (if regexp "SELECT rowid,text FROM docs"
                                       "SELECT rowid,text FROM docs WHERE text LIKE ?")
                                     (unless regexp (list (concat "%" text "%"))))))))

(defun pai-memory-forget-plan-text (plan text)
  "Return a human summary of PLAN for TEXT."
  (let ((files (plist-get plan :files)))
    (concat
     (format "Forget %S:\n" text)
     (if files
         (mapconcat (lambda (f) (format "  %s: %d %s" (abbreviate-file-name (nth 0 f)) (nth 4 f)
                                        (if (eq (nth 1 f) 'entries) "entry(ies)" "line(s)")))
                    files "\n")
       "  no memory files match")
     (format "\n  observations in %d session(s): %d"
             (length (plist-get plan :observations))
             (apply #'+ (mapcar (lambda (o) (length (cdr o))) (plist-get plan :observations))))
     (format "\n  skill usage notes: %s" (if (plist-get plan :usage)
                                              (string-join (plist-get plan :usage) ", ") "none"))
     (format "\n  search index rows: %d" (plist-get plan :index))
     "\n  (raw conversation text in session files is kept)")))

(defun pai-memory-forget-empty-p (plan)
  "Return non-nil when PLAN changes nothing."
  (and (null (plist-get plan :files)) (null (plist-get plan :observations))
       (null (plist-get plan :usage)) (= 0 (plist-get plan :index))))

;;;; Forget: applying

(defun pai-memory--live-session (file)
  "Return the live session object of session FILE in some pai buffer, or nil."
  (seq-some (lambda (b) (with-current-buffer b
                          (and (boundp 'pai--session) pai--session
                               (equal (pai-session-file pai--session) file)
                               pai--session)))
            (buffer-list)))

(defun pai-memory--session-leaves (session)
  "Return the ids of SESSION's leaf entries."
  (let ((parents (make-hash-table :test 'equal)))
    (dolist (e (pai-session-entries session))
      (when (plist-get e :parentId) (puthash (plist-get e :parentId) t parents)))
    (delq nil (mapcar (lambda (e) (unless (gethash (plist-get e :id) parents) (plist-get e :id)))
                      (pai-session-entries session)))))

(defun pai-memory--redact-observations (file ids)
  "Hide observation IDS of session FILE on every branch."
  (let* ((live (pai-memory--live-session file))
         (s (or live (pai-session-load file)))
         (leaf (pai-session-leaf-id s)))
    (dolist (l (pai-memory--session-leaves s))
      (pai-session-branch s l)
      (let ((e (pai-session-append-custom s "memory.redacted" (list :ids (vconcat ids)))))
        (when (equal l leaf) (setq leaf (plist-get e :id)))))
    ;; the live buffer stays on its (now extended) branch
    (pai-session-branch s leaf)))

(defun pai-memory-forget-apply (plan text &optional regexp)
  "Carry out PLAN for forgotten TEXT (a regexp when REGEXP); return a message."
  (let* ((stamp (format-time-string "%Y%m%dT%H%M%S"))
         (backup (pai-memory-dir "backups" stamp))
         (root (file-name-as-directory (expand-file-name pai-directory)))
         (changed 0))
    (dolist (f (plist-get plan :files))
      (let* ((file (nth 0 f))
             (rel (if (string-prefix-p root (expand-file-name file))
                      (substring (expand-file-name file) (length root))
                    (concat "other/" (file-name-nondirectory file))))
             (copy (expand-file-name rel backup)))
        (make-directory (file-name-directory copy) t)
        (copy-file file copy t)
        (pai-memory--write-file file (nth 3 f))
        (pai-memory--log (list :id (pai-memory--new-id) :time (format-time-string "%FT%T%z")
                               :action "forget" :target (symbol-name (nth 1 f))
                               :file (abbreviate-file-name file) :content "" :old ""
                               :origin "forget" :proposal_id "" :session ""
                               :entry_text_hash (secure-hash 'sha256 (nth 3 f))
                               :before (nth 2 f) :after (nth 3 f)))
        (cl-incf changed)))
    (dolist (o (plist-get plan :observations))
      (pai-memory--redact-observations (car o) (cdr o)))
    (let ((match (pai-memory--matcher text regexp)))
      (dolist (name (plist-get plan :usage))
        (pai-memory--usage-update
         name (lambda (r) (plist-put r :notes (vconcat (seq-remove match (append (plist-get r :notes) nil))))))))
    (when (pai-memory-search-available-p)
      (let ((rows (pai-memory--index-matches text regexp)))
        (dolist (id rows) (sqlite-execute (pai-memory--db) "DELETE FROM docs WHERE rowid=?" (list id)))
        (when rows (ignore-errors (sqlite-execute (pai-memory--db) "INSERT INTO docs(docs) VALUES('optimize')")))))
    ;; keep it from coming back through a reindex or a later write
    (let ((state (pai-memory-state-read)))
      (pai-memory-state-write
       (plist-put state :forgotten
                  (vconcat (delete-dups (append (append (plist-get state :forgotten) nil)
                                                (list (if regexp text (regexp-quote text)))))))))
    (format "Forgot %S: %d file(s) rewritten (backups in %s, /memory undo restores), %d session(s) of observations hidden, %d index row(s) removed"
            text changed (abbreviate-file-name backup)
            (length (plist-get plan :observations)) (plist-get plan :index))))

(defun pai-memory-forget (args &optional confirm-fn)
  "Run `/memory forget' with ARGS (a list of words); return a message.
CONFIRM-FN is called with the plan summary and returns non-nil to proceed
\(default `yes-or-no-p')."
  (let* ((regexp (member "--regex" args))
         (dry (member "--dry-run" args))
         (scope (if (member "--all" args) "all" "project"))
         (text (string-join (seq-remove (lambda (w) (string-prefix-p "--" w)) args) " ")))
    (cond
     ((< (length text) 3) "Usage: /memory forget TEXT [--regex] [--all] [--dry-run] (3+ characters)")
     ((and regexp (not (ignore-errors (string-match-p text "") t))) "Invalid regular expression")
     (t
      (let* ((plan (pai-memory-forget-plan text regexp scope))
             (summary (pai-memory-forget-plan-text plan text)))
        (cond ((pai-memory-forget-empty-p plan) (format "Nothing in memory matches %S" text))
              (dry summary)
              ((not (funcall (or confirm-fn (lambda (s) (yes-or-no-p (concat s "\nForget all of this? "))))
                             summary))
               "Nothing forgotten")
              (t (pai-memory-forget-apply plan text regexp))))))))

(provide 'pai-memory-privacy)
;;; pai-memory-privacy.el ends here
