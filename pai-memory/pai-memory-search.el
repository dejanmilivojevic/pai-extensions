;;; pai-memory-search.el --- memory_search: full-text search over memory -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 A1.  A full-text index of everything pai remembers, in
;; ~/.pai/memory/index.sqlite (Emacs 29's built-in SQLite, FTS5 trigram
;; tokenizer):
;;
;;   message      user / assistant / tool-result messages of every session
;;   observation  observations from session ledgers
;;   compaction   compaction summaries
;;   topic        sections of session topic files and JOURNEY.md
;;   memory       long-term memory entries (USER.md, MEMORY.md, project MEMORY.md)
;;   skill        SKILL.md files
;;
;; The index is derived data: delete it any time; `/memory reindex' rebuilds
;; it.  Indexing is incremental -- session files are append-only, so only the
;; bytes past the recorded offset are read; Markdown files are re-read when
;; their size or time changes -- and runs in short slices from an idle timer,
;; never during a turn.  Worker transcripts (memory-workers/) are skipped.
;;
;; The `memory_search' tool never calls a model.  Modes:
;;   discover  {query, scope?, kinds?, limit?}: ranked hits, two per session;
;;   scroll    {session_id, around, window?}: the messages around an entry;
;;   read      {path}: a file under ~/.pai/memory.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-memory-injected)
(require 'pai-core)
(require 'pai-config)
(require 'pai-session)
(require 'pai-tools)
(require 'pai-skills)
(require 'pai-memory-settings)
(require 'pai-memory-store)
(require 'pai-memory-budget)

(declare-function sqlite-open "sqlite.c")
(declare-function sqlite-close "sqlite.c")
(declare-function sqlite-execute "sqlite.c")
(declare-function sqlite-select "sqlite.c")
(declare-function sqlite-transaction "sqlite.c")
(declare-function sqlite-commit "sqlite.c")
(declare-function pai-memory--skill-dirs "pai-memory-promote" ())

(defvar pai--session)

;;;; Database

(defvar pai-memory--db nil "Open index database handle.")
(defvar pai-memory--db-file nil "File `pai-memory--db' was opened on.")

(defun pai-memory-index-file ()
  "Return the index database path."
  (pai-memory-dir "index.sqlite"))

(defun pai-memory-search-available-p ()
  "Return non-nil when search can work: enabled and SQLite present."
  (and (pai-truthy (pai-memory-get :search :enabled))
       (fboundp 'sqlite-available-p) (sqlite-available-p)))

(defun pai-memory--db ()
  "Return the open index database, creating the schema when needed."
  (let ((file (pai-memory-index-file)))
    (unless (and pai-memory--db (equal file pai-memory--db-file)
                 (file-exists-p file))
      (when pai-memory--db (ignore-errors (sqlite-close pai-memory--db)))
      (make-directory (file-name-directory file) t)
      (setq pai-memory--db (sqlite-open file) pai-memory--db-file file)
      (sqlite-execute pai-memory--db
                      "CREATE VIRTUAL TABLE IF NOT EXISTS docs USING fts5(
text, kind UNINDEXED, project UNINDEXED, session UNINDEXED, entry UNINDEXED,
role UNINDEXED, ts UNINDEXED, path UNINDEXED, tokenize='trigram')")
      (sqlite-execute pai-memory--db
                      "CREATE TABLE IF NOT EXISTS files (path TEXT PRIMARY KEY,
offset INTEGER, size INTEGER, mtime REAL)")
      (sqlite-execute pai-memory--db
                      "CREATE TABLE IF NOT EXISTS private (session TEXT PRIMARY KEY)"))
    pai-memory--db))

(defun pai-memory-index-close ()
  "Close the index database."
  (when pai-memory--db (ignore-errors (sqlite-close pai-memory--db)))
  (setq pai-memory--db nil pai-memory--db-file nil))

(defun pai-memory-index-reset ()
  "Delete the index; it is rebuilt from scratch."
  (pai-memory-index-close)
  (let ((file (pai-memory-index-file)))
    (dolist (f (list file (concat file "-wal") (concat file "-shm") (concat file "-journal")))
      (when (file-exists-p f) (delete-file f)))))

(defun pai-memory-index-size-mb ()
  "Return the index file size in megabytes."
  (let ((a (file-attributes (pai-memory-index-file))))
    (if a (/ (file-attribute-size a) 1048576.0) 0.0)))

(defun pai-memory--insert (db kind text &rest cols)
  "Insert one row of KIND with TEXT and COLS (:project :session ...) into DB."
  (when (and (stringp text) (not (string-empty-p (string-trim text))))
    ;; the index must not keep secrets or forgotten text
    (setq text (pai-memory-redact text))
    (sqlite-execute db "INSERT INTO docs (text,kind,project,session,entry,role,ts,path) VALUES (?,?,?,?,?,?,?,?)"
                    (list text kind (or (plist-get cols :project) "")
                          (or (plist-get cols :session) "") (or (plist-get cols :entry) "")
                          (or (plist-get cols :role) "") (or (plist-get cols :ts) "")
                          (or (plist-get cols :path) "")))))

;;;; What to index

(defun pai-memory--index-time (ms)
  "Format epoch milliseconds MS."
  (if (numberp ms) (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time (/ ms 1000.0))) ""))

(defun pai-memory--message-text (m cap)
  "Return searchable text of message M, tool results clipped to CAP chars."
  (pcase (pai-message-role m)
    ('user (pai-memory-strip-injected (pai-content-text (pai-message-content m))))
    ('assistant
     (concat (pai-content-text (pai-message-content m))
             (mapconcat (lambda (tc)
                          (format "\n[%s %s]" (plist-get tc :name)
                                  (truncate-string-to-width
                                   (condition-case nil (pai-json-encode (or (plist-get tc :arguments)
                                                                            (pai-json-empty-object)))
                                     (error "")) (or cap 2000))))
                        (pai-message-tool-calls m) "")))
    ('tool-result
     (unless (pai-tool-schema-message-p m)
       (truncate-string-to-width (pai-content-text (plist-get m :content)) (or cap 2000))))
    (_ nil)))

(defun pai-memory--index-entry (db obj project session cap path)
  "Index line OBJ of session file PATH (SESSION in PROJECT) into DB."
  (let ((cols (list :project project :session session :entry (or (plist-get obj :id) "")
                    :path path)))
    (pcase (plist-get obj :type)
      ("message"
       (let* ((raw (plist-get obj :message))
              ;; some older sessions saved a steering notice as a bare string
              (m (if (stringp raw) (pai-user-message raw) (pai-session-normalize-message raw))))
         (unless (pai-system-message-p m)
           (apply #'pai-memory--insert db "message" (pai-memory--message-text m cap)
                  :role (symbol-name (pai-message-role m))
                  :ts (pai-memory--index-time (plist-get m :timestamp)) cols))))
      ("custom_message"
       (apply #'pai-memory--insert db "message" (format "%s" (plist-get obj :content))
              :role "custom" cols))
      ("compaction"
       (apply #'pai-memory--insert db "compaction" (plist-get obj :summary) cols))
      ("custom"
       (when (equal (plist-get obj :customType) "memory.observations")
         (dolist (o (append (plist-get (plist-get obj :data) :observations) nil))
           (apply #'pai-memory--insert db "observation" (plist-get o :content)
                  :ts (or (plist-get o :timestamp) "") :role (or (plist-get o :id) "")
                  cols)))))))

(defun pai-memory--session-files ()
  "Return every session file of every project (worker transcripts excluded)."
  (let ((root (expand-file-name "sessions" pai-directory)))
    (when (file-directory-p root)
      (apply #'append
             (mapcar (lambda (d) (and (file-directory-p d) (directory-files d t "\\.jsonl\\'")))
                     (directory-files root t "\\`[^.]"))))))

(defun pai-memory--markdown-files ()
  "Return (PATH KIND PROJECT SESSION) for every Markdown file to index."
  (let ((out '())
        (projects (pai-memory-dir "projects")))
    (dolist (target '("USER.md" "MEMORY.md"))
      (let ((f (pai-memory-dir target)))
        (when (file-exists-p f) (push (list f "memory" "" "") out))))
    (when (file-directory-p projects)
      (dolist (pd (directory-files projects t "\\`[^.]"))
        (let ((slug (file-name-nondirectory pd)))
          (let ((mf (expand-file-name "MEMORY.md" pd)))
            (when (file-exists-p mf) (push (list mf "memory" slug "") out)))
          (let ((td (expand-file-name "topics" pd)))
            (when (file-directory-p td)
              (dolist (f (directory-files td t "\\.md\\'"))
                (unless (equal (file-name-nondirectory f) "INDEX.md")
                  (push (list f "topic" slug "") out)))))
          (let ((sd (expand-file-name "sessions" pd)))
            (when (file-directory-p sd)
              (dolist (sess (directory-files sd t "\\`[^.]"))
                (when (file-directory-p sess)
                  (dolist (f (directory-files sess t "\\.md\\'"))
                    (unless (member (file-name-nondirectory f) '("INDEX.md"))
                      (push (list f "topic" slug (file-name-nondirectory sess)) out))))))))))
    (dolist (dir (ignore-errors (if (fboundp 'pai-memory--skill-dirs) (pai-memory--skill-dirs)
                                  (pai-skills-default-dirs))))
      (dolist (s (ignore-errors (pai-discover-skills (list dir))))
        (push (list (plist-get s :path) "skill" "" (plist-get s :name)) out)))
    (nreverse out)))

(defun pai-memory--md-sections (text kind)
  "Split TEXT into indexable pieces for KIND."
  (if (equal kind "memory")
      (pai-memory-parse-entries text)
    (if (equal kind "skill")
        (list text)
      (let ((sections '()) (current '()))
        ;; one piece per heading (and the text before the first heading)
        (dolist (line (split-string text "\n"))
          (when (and (string-match-p "\\`#+ " line) current)
            (push (string-join (nreverse current) "\n") sections)
            (setq current '()))
          (push line current))
        (when current (push (string-join (nreverse current) "\n") sections))
        (seq-remove (lambda (x) (string-empty-p (string-trim x))) (nreverse sections))))))

;;;; Indexing

(defun pai-memory--file-state (db path)
  "Return (OFFSET SIZE MTIME) recorded for PATH, or nil."
  (car (sqlite-select db "SELECT offset,size,mtime FROM files WHERE path=?" (list path))))

(defun pai-memory--set-file-state (db path offset size mtime)
  "Record PATH's indexing state."
  (sqlite-execute db "INSERT OR REPLACE INTO files (path,offset,size,mtime) VALUES (?,?,?,?)"
                  (list path offset size mtime)))

(defun pai-memory-file-private-p (file)
  "Return non-nil when session FILE's last private-state override is on."
  (with-temp-buffer
    ;; raw bytes: no coding detection over a multi-megabyte file, only the
    ;; lines that mention the marker are decoded
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-max))
    (let ((found nil) (private nil))
      (while (and (not found) (search-backward "\"private\"" nil t))
        (let ((e (ignore-errors (pai-json-decode
                                 (decode-coding-string
                                  (buffer-substring-no-properties
                                   (line-beginning-position) (line-end-position))
                                  'utf-8)))))
          (when (and (equal (plist-get e :customType) "memory.state")
                     (plist-member (plist-get e :data) :private))
            (setq found t private (pai-truthy (plist-get (plist-get e :data) :private))))))
      private)))

(defun pai-memory--tail-mentions-private-p (file offset)
  "Return non-nil when session FILE past byte OFFSET mentions \"private\"."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil offset)
    (goto-char (point-min))
    (search-forward "\"private\"" nil t)))

(defun pai-memory--index-session-file (db file cap)
  "Index the unread tail of session FILE into DB, honouring private sessions.
A private session's rows are dropped and it is skipped; one that was private
and is public again is read from the start."
  (let* ((session (file-name-base file))
         (size (file-attribute-size (file-attributes file)))
         (offset (or (car (pai-memory--file-state db file)) 0))
         (was (and (sqlite-select db "SELECT 1 FROM private WHERE session=?" (list session)) t))
         ;; the state only changes when lines are appended: look at the whole
         ;; file only on a first read or when the new tail mentions it
         (now (cond ((= size offset) was)
                    ((or (= offset 0) (> offset size) (pai-memory--tail-mentions-private-p file offset))
                     (pai-memory-file-private-p file))
                    (t was))))
    (cond
     ((= size offset) nil)
     (now
      (unless was
        (sqlite-execute db "INSERT OR REPLACE INTO private (session) VALUES (?)" (list session)))
      (sqlite-execute db "DELETE FROM docs WHERE session=?" (list session))
      ;; the file is dealt with up to here: without this every pass re-read
      ;; every private session from the start, and never finished
      (pai-memory--set-file-state db file size size
                                  (float-time (file-attribute-modification-time
                                               (file-attributes file))))
      nil)
     (t
      (when was
        (sqlite-execute db "DELETE FROM private WHERE session=?" (list session))
        (sqlite-execute db "DELETE FROM docs WHERE path=?" (list file))
        (sqlite-execute db "DELETE FROM files WHERE path=?" (list file)))
      (pai-memory--index-session-file-1 db file cap)))))

(defvar pai-memory-index-chunk-bytes (* 512 1024)
  "Most bytes of one session file read in one go.
A big file is indexed over several calls, so no single call blocks long.")

(defun pai-memory--index-session-file-1 (db file cap)
  "Index the unread tail of session FILE into DB.
Return `partial' when only the first `pai-memory-index-chunk-bytes' of the
tail were read, non-nil when it read anything, nil otherwise.  Only complete
lines are read, so a line being written is picked up next time."
  (let* ((attrs (file-attributes file))
         (size (file-attribute-size attrs))
         (state (pai-memory--file-state db file))
         (offset (or (car state) 0)))
    (when (< size offset)                 ; rewritten: start over
      (sqlite-execute db "DELETE FROM docs WHERE path=?" (list file))
      (setq offset 0))
    (when (> size offset)
      (let ((project (file-name-nondirectory (directory-file-name (file-name-directory file))))
            (session (file-name-base file))
            (end (min size (+ offset (max 1 pai-memory-index-chunk-bytes))))
            (consumed 0))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file nil offset end)
          (goto-char (point-min))
          (when (and (< end size) (not (search-forward "\n" nil t)))
            ;; one line longer than a chunk: read all of it
            (erase-buffer)
            (setq end size)
            (insert-file-contents-literally file nil offset end))
          (goto-char (point-min))
          (let ((start (point)))
            (while (search-forward "\n" nil t)
              (let ((obj (ignore-errors
                           (pai-json-decode
                            (decode-coding-string
                             (buffer-substring-no-properties start (1- (point))) 'utf-8)))))
                (when obj
                  ;; one malformed line never stops the pass
                  (condition-case err (pai-memory--index-entry db obj project session cap file)
                    (error (message "pai-memory: skipped a line of %s: %s"
                                    (file-name-nondirectory file) (error-message-string err))))))
              (setq start (point)
                    consumed (1- (point))))))
        (pai-memory--set-file-state db file (+ offset consumed) size
                                    (float-time (file-attribute-modification-time attrs)))
        (cond ((and (< end size) (> consumed 0)) 'partial)
              (t (> consumed 0)))))))

(defun pai-memory--index-markdown (db spec)
  "Re-index Markdown file SPEC (PATH KIND PROJECT SESSION) when it changed."
  (let* ((path (nth 0 spec))
         (attrs (file-attributes path))
         (size (file-attribute-size attrs))
         (mtime (float-time (file-attribute-modification-time attrs)))
         (state (pai-memory--file-state db path)))
    (unless (and state (equal (nth 1 state) size) (equal (nth 2 state) mtime))
      (sqlite-execute db "DELETE FROM docs WHERE path=?" (list path))
      (dolist (piece (pai-memory--md-sections (pai-memory--read-file path) (nth 1 spec)))
        (pai-memory--insert db (nth 1 spec) piece :project (nth 2 spec) :session (nth 3 spec)
                            :path path :ts (format-time-string "%Y-%m-%d %H:%M"
                                                               (file-attribute-modification-time attrs))))
      (pai-memory--set-file-state db path size size mtime)
      t)))

(defun pai-memory--drop-missing (db)
  "Forget indexed files that no longer exist."
  (dolist (row (sqlite-select db "SELECT path FROM files"))
    (unless (file-exists-p (car row))
      (sqlite-execute db "DELETE FROM docs WHERE path=?" (list (car row)))
      (sqlite-execute db "DELETE FROM files WHERE path=?" (list (car row))))))

(defun pai-memory--index-atomically (db what fn)
  "Call FN with DB inside a savepoint; return its value.
When FN exits abnormally (an error, \\[keyboard-quit]) its writes are undone,
so rows are never committed without the offset that says they were read:
that would index them twice.  An error is reported, naming WHAT, and nil
returned; a quit goes on up."
  (let ((ok nil))
    (sqlite-execute db "SAVEPOINT pai_index_one")
    (condition-case err
        (unwind-protect (prog1 (funcall fn) (setq ok t))
          (if ok (sqlite-execute db "RELEASE pai_index_one")
            (sqlite-execute db "ROLLBACK TO pai_index_one")
            (sqlite-execute db "RELEASE pai_index_one")))
      (error (message "pai-memory: skipped indexing %s: %s"
                      (file-name-nondirectory what) (error-message-string err))
             nil))))

(defvar pai-memory--index-resume nil
  "Session file the next indexing slice starts at, or nil for a fresh pass.
A slice that runs out of time leaves this, so the next one goes on from there
instead of walking the same files again.")

(defvar pai-memory-index-progress nil
  "(DONE . TOTAL) bytes of the session files indexed, after the last slice.")

(defun pai-memory--index-measure (db files)
  "Return (DONE . TOTAL) bytes of session FILES that DB has read."
  (let ((offsets (make-hash-table :test #'equal)) (done 0) (total 0))
    (dolist (row (sqlite-select db "SELECT path,offset FROM files"))
      (puthash (car row) (cadr row) offsets))
    (dolist (f files)
      (let ((size (file-attribute-size (file-attributes f))))
        (when (numberp size)
          (setq total (+ total size)
                done (+ done (min size (let ((o (gethash f offsets))) (if (numberp o) o 0))))))))
    (cons done total)))

(defun pai-memory-index-percent ()
  "Return how much of the session files is indexed, 0-100, or nil when unknown."
  (let ((p pai-memory-index-progress))
    (and (consp p) (numberp (cdr p))
         (if (> (cdr p) 0) (min 100 (floor (* 100.0 (car p)) (cdr p))) 100))))

(defun pai-memory-index-update (&optional budget)
  "Bring the index up to date, spending at most BUDGET seconds (default no limit).
Return `done' when everything is indexed, `more' when time ran out, or
`full' when the index reached `:max-mb'.  A slice that runs out of time
records where it stopped; the next one starts there."
  (let* ((db (pai-memory--db))
         (deadline (and budget (+ (float-time) budget)))
         (cap (pai-memory-get :search :tool-result-chars))
         (max-mb (pai-memory-get :search :max-mb))
         (files (pai-memory--session-files))
         (tail (and pai-memory--index-resume (member pai-memory--index-resume files)))
         ;; resuming: the files from the resume point, then the ones before it
         (todo (if tail (append tail (seq-take files (- (length files) (length tail)))) files))
         (result 'done))
    (setq pai-memory--index-resume nil)
    (catch 'stop
      (sqlite-transaction db)
      (unwind-protect
          (progn
            (unless tail                ; a fresh pass
              (pai-memory--drop-missing db)
              (dolist (spec (pai-memory--markdown-files))
                ;; stop for time only after real work: this phase restarts
                ;; from the top, and unchanged files must not starve the rest
                (when (and (pai-memory--index-atomically
                            db (car spec) (lambda () (pai-memory--index-markdown db spec)))
                           deadline (> (float-time) deadline))
                  (setq result 'more) (throw 'stop nil))))
            (while todo
              (let ((file (pop todo)) (r 'partial))
                (when (and max-mb (> (pai-memory-index-size-mb) max-mb))
                  (setq result 'full) (throw 'stop nil))
                ;; a big file is read a chunk at a time, checking the clock
                ;; between chunks
                (while (eq r 'partial)
                  (setq r (pai-memory--index-atomically
                           db file (lambda () (pai-memory--index-session-file db file cap))))
                  ;; as above: a file with nothing new never ends a slice, or
                  ;; slices could go round the files forever saying `more'
                  (when (and r deadline (> (float-time) deadline)
                             (or (eq r 'partial) todo))
                    (setq result 'more
                          pai-memory--index-resume (if (eq r 'partial) file (car todo)))
                    (throw 'stop nil))))))
        ;; every file's rows and offset went in together (see
        ;; `pai-memory--index-atomically'), so what is here is consistent
        (sqlite-commit db)))
    (setq pai-memory-index-progress (ignore-errors (pai-memory--index-measure db files)))
    result))

(defvar pai-memory--index-timer nil "Pending idle indexing timer.")
(defvar pai-memory-index-status nil "Result of the last indexing pass.")
(defvar pai-memory--index-reported nil "Float time of the last progress message, or nil.")

(defun pai-memory--index-report (status)
  "Show indexing progress in the echo area for a background slice with STATUS.
Progress shows at most once a second and stays out of *Messages*; when a run
that showed progress ends, a line saying so is logged."
  (unless (active-minibuffer-window)
    (let ((pct (pai-memory-index-percent)) (now (float-time)))
      (pcase status
        ('more
         (when (or (null pai-memory--index-reported) (> (- now pai-memory--index-reported) 1.0))
           (let ((message-log-max nil))
             (message "pai-memory: indexing sessions for search… %s%%" (or pct "?")))
           (setq pai-memory--index-reported now)))
        (_
         (when pai-memory--index-reported
           (setq pai-memory--index-reported nil)
           (message "pai-memory: search index %s (%.1f MB)"
                    (if (eq status 'full) "is FULL" "up to date") (pai-memory-index-size-mb))))))))

(defun pai-memory-index-schedule ()
  "Index in short idle slices until the index is complete."
  (when (and (pai-memory-search-available-p) (not (timerp pai-memory--index-timer)))
    (setq pai-memory--index-timer
          (run-with-idle-timer
           1 nil
           (lambda ()
             (setq pai-memory--index-timer nil)
             (condition-case err
                 (progn
                   (setq pai-memory-index-status (pai-memory-index-update 0.2))
                   (pai-memory--index-report pai-memory-index-status)
                   (if (eq pai-memory-index-status 'more)
                       (pai-memory-index-schedule)
                     (when (fboundp 'pai-memory-embed-schedule) (pai-memory-embed-schedule))))
               (error (setq pai-memory--index-reported nil)
                      (message "pai-memory: indexing failed: %s" (error-message-string err)))))))))

;;;; Search

(defun pai-memory--terms (query)
  "Split QUERY into terms."
  (split-string (or query "") "[ \t\n]+" t "[\"']"))

(defun pai-memory--fts-query (terms &optional any)
  "Return the FTS5 MATCH string for TERMS of 3+ characters, or nil.
All terms must match, or any of them with ANY."
  (let ((long (seq-filter (lambda (s) (>= (length s) 3)) terms)))
    (and long (mapconcat (lambda (s) (concat "\"" (replace-regexp-in-string "\"" "\"\"" s) "\""))
                         long (if any " OR " " ")))))

(defun pai-memory--current-project ()
  "Return the current project's slug."
  (pai-session--slug default-directory))

(declare-function pai-memory-embedder "pai-memory-embed" ())
(declare-function pai-memory-embed-query "pai-memory-embed" (text))
(declare-function pai-memory-embed-rerank "pai-memory-embed" (db rows qvec &optional vec-rows))
(declare-function pai-memory-embed-ensure-schema "pai-memory-embed" (db))
(declare-function pai-memory-embed-schedule "pai-memory-embed" ())

(cl-defun pai-memory-search (query &key (scope "project") kinds session limit any
                                   exclude-session semantic)
  "Search the index for QUERY; return hit plists, best first.
SCOPE is session, project or all; KINDS limits the kinds; SESSION is the
session id for the session scope; LIMIT caps the hits (default 8).  ANY
matches any term instead of all (short terms are then ignored);
EXCLUDE-SESSION leaves one session's rows out.  SEMANTIC, when an embedder
is configured (V2 A3), widens the candidates to any term and fuses the
full-text ranking with vector similarity."
  (let* ((db (pai-memory--db))
         (qvec (and semantic (fboundp 'pai-memory-embedder) (pai-memory-embedder)
                    (progn (pai-memory-embed-ensure-schema db)
                           (pai-memory-embed-query query))))
         (any (or any (and qvec t)))
         (terms (pai-memory--terms query))
         (match (pai-memory--fts-query terms any))
         (short (unless any (seq-filter (lambda (s) (< (length s) 3)) terms)))
         (limit (or limit 8))
         ;; scope filters, shared by the full-text and the vector query
         (filters '()) (fargs '()))
    (unless terms (user-error "Give a query"))
    (pcase scope
      ("session" (push "d.session = ?" filters) (push (or session "") fargs))
      ("all" nil)
      (_ (push "(d.project = ? OR d.project = '')" filters) (push (pai-memory--current-project) fargs)))
    (when exclude-session (push "d.session != ?" filters) (push exclude-session fargs))
    (when kinds
      (push (format "d.kind IN (%s)" (mapconcat (lambda (_) "?") kinds ",")) filters)
      (dolist (k kinds) (push k fargs)))
    (setq filters (nreverse filters) fargs (nreverse fargs))
    (let* ((cols "d.rowid,d.kind,d.project,d.session,d.entry,d.role,d.ts,d.path,d.text")
           (fts-rows
            (let ((where (append (and match (list "docs MATCH ?"))
                                 (mapcar (lambda (_) "d.text LIKE ?") short)
                                 (and any (not match) (list "0"))
                                 filters)))
              (sqlite-select db (format "SELECT %s FROM docs d WHERE %s %s LIMIT ?" cols
                                        (if where (string-join where " AND ") "1")
                                        (if match "ORDER BY rank" "ORDER BY d.rowid DESC"))
                             (append (and match (list match))
                                     (mapcar (lambda (x) (concat "%" x "%")) short)
                                     fargs
                                     (list (if qvec (max 200 (* 5 limit)) (* 5 limit)))))))
           ;; semantic: the newest embedded rows in scope are candidates too,
           ;; so a match that shares no word with the query is found
           (vec-rows
            (and qvec
                 (sqlite-select db (format "SELECT %s FROM docs d JOIN vecs v ON v.docid = d.rowid WHERE %s ORDER BY d.rowid DESC LIMIT ?"
                                           cols (if filters (string-join filters " AND ") "1"))
                                (append fargs (list (or (pai-memory-get :search :embed-scan) 3000))))))
           (rows (if qvec
                     (mapcar #'cdr (pai-memory-embed-rerank db fts-rows qvec vec-rows))
                   (mapcar #'cdr fts-rows)))
           (per (make-hash-table :test 'equal))
           (hits '()))
      (dolist (r rows)
        (let ((key (if (member (nth 0 r) '("message" "observation" "compaction")) (nth 2 r) (nth 6 r))))
          (when (and (< (length hits) limit) (< (gethash key per 0) 2))
            (puthash key (1+ (gethash key per 0)) per)
            (push (list :kind (nth 0 r) :project (nth 1 r) :session (nth 2 r) :entry (nth 3 r)
                        :role (nth 4 r) :ts (nth 5 r) :path (nth 6 r) :text (nth 7 r))
                  hits))))
      (nreverse hits))))

(defun pai-memory--snippet (text terms &optional width)
  "Return about WIDTH characters of TEXT around the first of TERMS."
  (let* ((width (or width 240))
         (case-fold-search t)
         (pos (or (seq-some (lambda (tm) (string-match (regexp-quote tm) text)) terms) 0))
         (start (max 0 (- pos (/ width 3))))
         (end (min (length text) (+ start width))))
    (concat (if (> start 0) "…" "")
            (replace-regexp-in-string "[ \t\n]+" " " (substring text start end))
            (if (< end (length text)) "…" ""))))

(defun pai-memory-format-hits (hits query &optional branch-ids session-id)
  "Return the tool text for HITS of QUERY.
BRANCH-IDS are the current branch's entry ids of SESSION-ID; hits on other
branches of that session are labelled."
  (let ((terms (pai-memory--terms query)) (i 0))
    (if (null hits)
        (format "No matches for %S." query)
      (concat
       (format "%d match(es) for %S:\n" (length hits) query)
       (mapconcat
        (lambda (h)
          (cl-incf i)
          (concat
           (format "\n[%d] %s" i (plist-get h :kind))
           (let ((s (plist-get h :session)))
             (unless (string-empty-p s)
               (format " · session %s" (substring s 0 (min 8 (length s))))))
           (unless (string-empty-p (plist-get h :ts)) (format " · %s" (plist-get h :ts)))
           (when (equal (plist-get h :kind) "message") (format " · %s" (plist-get h :role)))
           (when (and session-id (equal (plist-get h :session) session-id)
                      (not (string-empty-p (plist-get h :entry)))
                      (not (member (plist-get h :entry) branch-ids)))
             " · (other branch)")
           "\n    " (pai-memory--snippet (plist-get h :text) terms)
           (cond ((member (plist-get h :kind) '("message" "observation" "compaction"))
                  (format "\n    more: memory_search {\"session_id\": %S, \"around\": %S}"
                          (plist-get h :session) (plist-get h :entry)))
                 ((not (string-empty-p (plist-get h :path)))
                  (format "\n    file: %s" (abbreviate-file-name (plist-get h :path)))))))
        hits "")))))

(defun pai-memory--find-session-file (id)
  "Return the session file with session ID, or nil."
  (seq-find (lambda (f) (equal (file-name-base f) id)) (pai-memory--session-files)))

(defun pai-memory-scroll (session-id around &optional window)
  "Return the messages of SESSION-ID within WINDOW (default 4) of entry AROUND."
  (let ((file (pai-memory--find-session-file session-id)))
    (if (not file)
        (format "No session %s" session-id)
      (let* ((s (pai-session-load file))
             (entries (seq-filter (lambda (e) (or (equal (plist-get e :id) around)
                                                  (let ((m (pai-session--entry-to-message e)))
                                                    (and m (not (pai-system-message-p m))))))
                                  (pai-session-entries s)))
             (idx (seq-position entries around (lambda (e id) (equal (plist-get e :id) id))))
             (window (or window 4)))
        (if (not idx)
            (format "No entry %s in session %s" around session-id)
          (mapconcat
           (lambda (e)
             (let ((m (pai-session--entry-to-message e)))
               (format "%s[%s] %s%s"
                       (if (equal (plist-get e :id) around) "▶ " "  ")
                       (plist-get e :id)
                       (if m (symbol-name (pai-message-role m)) (plist-get e :customType))
                       (let ((text (if m (pai-memory--message-text m 1500)
                                     (mapconcat (lambda (o) (plist-get o :content))
                                                (append (plist-get (plist-get e :data) :observations) nil)
                                                "\n"))))
                         (concat ": " (truncate-string-to-width (or text "") 1500 nil nil "…"))))))
           (seq-subseq entries (max 0 (- idx window)) (min (length entries) (+ idx window 1)))
           "\n"))))))

(defun pai-memory--tool-execute-search (args ctx _on-update on-done)
  "Execute memory_search with ARGS in tool CTX."
  (funcall
   on-done
   (condition-case err
       (cond
        ((not (pai-memory-search-available-p))
         (pai-tool-error-result "Memory search is off or SQLite is unavailable."))
        ((plist-get args :path)
         (let ((path (expand-file-name (plist-get args :path) (pai-memory-dir))))
           (if (not (and (file-readable-p path)
                         (string-prefix-p (file-truename (file-name-as-directory (pai-memory-dir)))
                                          (file-truename path))))
               (pai-tool-error-result "read mode only reads files under ~/.pai/memory")
             (pai-tool-ok-result (truncate-string-to-width (pai-memory--read-file path) 20000 nil nil "…")))))
        ((and (plist-get args :session_id) (plist-get args :around))
         (pai-tool-ok-result (pai-memory-scroll (plist-get args :session_id) (plist-get args :around)
                                                (plist-get args :window))))
        (t
         (let* ((session (plist-get ctx :session))
                (default-directory (or (plist-get ctx :cwd) default-directory)))
           (pai-memory-index-update 2.0)
           (let ((hits (pai-memory-search (plist-get args :query)
                                          :scope (or (plist-get args :scope) "project")
                                          :kinds (append (plist-get args :kinds) nil)
                                          :session (or (plist-get args :session_id)
                                                       (and session (pai-session-id session)))
                                          :limit (plist-get args :limit)
                                          :semantic t)))
             (pai-tool-ok-result
              (pai-memory-format-hits hits (plist-get args :query)
                                      (and session (mapcar (lambda (e) (plist-get e :id))
                                                           (pai-session-get-branch session)))
                                      (and session (pai-session-id session))))))))
     (user-error (pai-tool-error-result (error-message-string err))))))

(defconst pai-memory-search-tool-def
  (list :name "memory_search"
        :label "Memory search"
        :description "Search what pai remembers: past conversations of this project (or all projects), observations, compaction summaries, session topic files, long-term memory and skills. Full-text search without any model call; returns the stored text. Use it when the user refers to earlier work (\"like last time\", \"what did we decide about X\"). Modes: search {query, scope?: project|session|all, kinds?: [message, observation, compaction, topic, memory, skill], limit?}; scroll around a hit {session_id, around, window?}; read a memory file {path}."
        :prompt-snippet "memory_search: search past sessions and memory"
        :parameters (pai-object-schema
                     (list :query (pai-string-schema "Words to search for (3+ letters work best).")
                           :scope (pai-string-schema "project (default), session or all" :enum ["project" "session" "all"])
                           :kinds (pai-array-schema "Only these kinds." (pai-string-schema "A kind."))
                           :limit (pai-number-schema "Maximum hits (default 8).")
                           :session_id (pai-string-schema "Session to search or scroll.")
                           :around (pai-string-schema "Entry id to scroll around (from a hit).")
                           :window (pai-number-schema "Messages on each side when scrolling (default 4).")
                           :path (pai-string-schema "Memory file to read (relative to ~/.pai/memory or absolute)."))
                     nil)
        :execute #'pai-memory--tool-execute-search)
  "The `memory_search' tool (V2 A1); deferred until first use.")

(provide 'pai-memory-search)
;;; pai-memory-search.el ends here
