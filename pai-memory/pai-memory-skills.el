;;; pai-memory-skills.el --- Skill usage tracking and the curator -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage tracking (SPEC §8.1), in ~/.pai/memory/skills-usage.json, keyed by
;; skill name -- never in SKILL.md itself:
;;
;;   {views, last_viewed, uses, last_used,
;;    outcomes: {followed, deviated, failed},     all time
;;    review:   {deviated, failed, hash},          since the skill last changed
;;    notes: [...],                                newest outcome observations
;;    pinned, state, archived_from, archived_at}
;;
;;   view     the agent `read' a discovered skill's file;
;;   use      a `/skill:NAME' input, or a view followed by another tool call in
;;            the same run (counted once per run);
;;   outcome  a committed "skill-used: NAME - followed|deviated|failed: why"
;;            observation (needs the session layer).
;; `review' counts restart whenever the skill's file changes (its hash), so a
;; patched skill starts with a clean slate.
;;
;; The curator (SPEC §8.2) is deterministic -- no model calls.  At most once
;; per `:curator-interval-days', from an idle timer after a session starts, it
;; looks at learned skills only (`origin: learned'), skips pinned ones, and:
;;   active -> stale      after `:stale-after-sessions' sessions without a view
;;                        or use, and at least `:stale-after-days' days;
;;   stale  -> archived   after `:archive-after-sessions' sessions and at least
;;                        `:archive-after-days' days: the skill directory moves
;;                        to ~/.pai/memory/skill-archive/ (restore with
;;                        /memory-restore-skill).
;;
;; Idleness is measured in *sessions*, so time away from Emacs never ages a
;; skill: a session counts once, when its first run settles.  Global skills
;; count every session, project skills only their project's.  The day limits
;; are a floor, so many short sessions in one day cannot archive a skill
;; either.  A skill seen for the first time starts counting from then.
;; A stale skill that is used again is active again.  Skills whose review
;; counts reach `:patch-threshold' are handed to the promoter as forced review
;; candidates.  Nothing is ever deleted; hand-written skills are never touched.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-skills)
(require 'pai-session)
(require 'pai-memory-settings)
(require 'pai-memory-budget)
(require 'pai-memory-store)

(declare-function pai-memory-expire-proposals "pai-memory-entries" (cwd))
(declare-function pai-memory--skill-dirs "pai-memory-promote" ())
(declare-function pai-memory-merge "pai-memory-merge" (&rest args))
(declare-function pai-memory-merge-due-p "pai-memory-merge" ())

;;;; Storage

(defun pai-memory-usage-file ()
  "Return the path of skills-usage.json."
  (pai-memory-dir "skills-usage.json"))

(defun pai-memory-usage-read ()
  "Return the usage table: a plist keyed by :SKILL-NAME."
  (let ((file (pai-memory-usage-file)))
    (or (and (file-readable-p file)
             (ignore-errors (with-temp-buffer (insert-file-contents file)
                                              (pai-json-decode (buffer-string)))))
        '())))

(defun pai-memory-usage-write (table)
  "Write usage TABLE atomically."
  (pai-memory--write-file (pai-memory-usage-file) (pai-json-encode (or table (pai-json-empty-object)))))

(defun pai-memory--ukey (name)
  "Return the table key of skill NAME."
  (intern (concat ":" name)))

(defun pai-memory-skill-usage (name &optional table)
  "Return skill NAME's usage record from TABLE (default: read it)."
  (plist-get (or table (pai-memory-usage-read)) (pai-memory--ukey name)))

(defun pai-memory--usage-update (name fn)
  "Replace skill NAME's usage record R with (FN R) and save."
  (let* ((table (pai-memory-usage-read))
         (key (pai-memory--ukey name)))
    (pai-memory-usage-write (plist-put table key (funcall fn (copy-sequence (plist-get table key)))))))

(defun pai-memory--usage-set (name record)
  "Set skill NAME's usage RECORD (nil removes it)."
  (let* ((table (pai-memory-usage-read)) (key (pai-memory--ukey name)) (out '()))
    (while table
      (unless (eq (car table) key) (setq out (append out (list (car table) (cadr table)))))
      (setq table (cddr table)))
    (pai-memory-usage-write (if record (append out (list key record)) out))))

(defun pai-memory--now () (format-time-string "%FT%T%z"))

(defun pai-memory--inc (plist key &optional n)
  "Return PLIST with number KEY increased by N (default 1)."
  (plist-put plist key (+ (or (plist-get plist key) 0) (or n 1))))

;;;; Skill lookup

(defun pai-memory--skills (&optional dirs)
  "Return the discovered skills in DIRS (default the pai buffer's)."
  (pai-discover-skills (or dirs (and (fboundp 'pai-memory--skill-dirs) (pai-memory--skill-dirs)))))

(defun pai-memory--skill-by-path (path skills)
  "Return the skill in SKILLS whose file is PATH, or nil."
  (let ((true (ignore-errors (file-truename path))))
    (and true (seq-find (lambda (s) (equal (file-truename (plist-get s :path)) true)) skills))))

(defun pai-memory--file-hash (path)
  "Return PATH's content hash, or nil."
  (and path (file-readable-p path) (secure-hash 'sha256 (pai-memory--read-file path))))

;;;; Session counts

(defun pai-memory-session-counts (&optional state)
  "Return (:global N :projects PLIST) from STATE (default state.json)."
  (or (plist-get (or state (pai-memory-state-read)) :sessions)
      (list :global 0 :projects nil)))

(defun pai-memory--project-key (cwd)
  "Return the session-count key of project CWD."
  (intern (concat ":" (pai-session--slug cwd))))

(defun pai-memory-session-count (&optional cwd)
  "Return the counted sessions: of project CWD, or all when CWD is nil."
  (let ((c (pai-memory-session-counts)))
    (or (if cwd
            (plist-get (plist-get c :projects) (pai-memory--project-key cwd))
          (plist-get c :global))
        0)))

(defun pai-memory-count-session (session)
  "Count SESSION once (its first settled run), globally and for its project.
A session already counted -- resumed, say -- is not counted again."
  (when (and session
             (not (seq-find (lambda (e) (and (equal (plist-get e :type) "custom")
                                             (equal (plist-get e :customType) "memory.counted")))
                            (pai-session-entries session))))
    (let* ((state (pai-memory-state-read))
           (c (pai-memory-session-counts state))
           (key (pai-memory--project-key (pai-session-cwd session)))
           (projects (plist-get c :projects)))
      (setq projects (plist-put projects key (1+ (or (plist-get projects key) 0))))
      (pai-memory-state-write
       (plist-put state :sessions (list :global (1+ (or (plist-get c :global) 0))
                                        :projects projects)))
      (pai-session-append-custom session "memory.counted"
                                 (list :global (1+ (or (plist-get c :global) 0))))
      t)))

(defun pai-memory--mark-seen (r)
  "Return usage record R stamped with the session counts and time of now."
  (let ((c (pai-memory-session-counts)))
    (plist-put (plist-put r :seen_global (or (plist-get c :global) 0))
               :seen_projects (or (plist-get c :projects) (pai-json-empty-object)))))

(defun pai-memory--reactivate (r)
  "Return usage record R, active again when it was stale."
  (if (equal (plist-get r :state) "stale") (plist-put r :state "active") r))

;;;; Events

(defun pai-memory-record-view (name)
  "Record that skill NAME was read."
  (pai-memory--usage-update
   name (lambda (r) (pai-memory--reactivate
                     (pai-memory--mark-seen
                      (plist-put (pai-memory--inc r :views) :last_viewed (pai-memory--now)))))))

(defun pai-memory-record-use (name)
  "Record that skill NAME was used."
  (pai-memory--usage-update
   name (lambda (r) (pai-memory--reactivate
                     (pai-memory--mark-seen
                      (plist-put (pai-memory--inc r :uses) :last_used (pai-memory--now)))))))

(defconst pai-memory-outcome-regexp
  "\\`skill-used:[ \t]*\\([a-z0-9]+\\(?:-[a-z0-9]+\\)*\\)[ \t]*[-—–:][ \t]*\\(followed\\|deviated\\|failed\\)\\b"
  "Matches a skill-used observation: group 1 the skill, group 2 the outcome.")

(defun pai-memory-parse-outcome (content)
  "Return (NAME . OUTCOME) for a skill-used observation CONTENT, or nil."
  (let ((case-fold-search t))
    (when (string-match pai-memory-outcome-regexp (or content ""))
      (cons (downcase (match-string 1 content)) (downcase (match-string 2 content))))))

(defun pai-memory-record-outcomes (observations &optional skills)
  "Count the skill-used OBSERVATIONS against the known SKILLS."
  (let ((skills (or skills (pai-memory--skills))))
    (dolist (o observations)
      (let* ((parsed (pai-memory-parse-outcome (plist-get o :content)))
             (skill (and parsed (seq-find (lambda (s) (equal (plist-get s :name) (car parsed)))
                                          skills))))
        (when skill
          (let ((hash (pai-memory--file-hash (plist-get skill :path)))
                (outcome (intern (concat ":" (cdr parsed)))))
            (pai-memory--usage-update
             (car parsed)
             (lambda (r)
               (let ((review (plist-get r :review)))
                 ;; the skill changed since the review counts started: start over
                 (unless (equal (plist-get review :hash) hash)
                   (setq review (list :hash hash :deviated 0 :failed 0)))
                 (unless (eq outcome :followed)
                   (setq review (pai-memory--inc review outcome)))
                 (setq r (plist-put r :outcomes (pai-memory--inc (plist-get r :outcomes) outcome)))
                 (setq r (plist-put r :review review))
                 (plist-put r :notes (vconcat (seq-take (cons (plist-get o :content)
                                                              (append (plist-get r :notes) nil))
                                                        5))))))))))))

;;;; Live tracking in a pai buffer

(defvar-local pai-memory--run-views nil
  "Skills viewed in the current run and not yet counted as used.")

(defun pai-memory-track-tool-start (event)
  "Count views and uses from a tool-execution-start EVENT."
  (let ((name (plist-get event :tool-name))
        (args (plist-get event :args)))
    (if (and (equal name "read") (stringp (plist-get args :path)))
        (let* ((path (expand-file-name (plist-get args :path) default-directory))
               (skill (pai-memory--skill-by-path path (pai-memory--skills))))
          (when skill
            (pai-memory-record-view (plist-get skill :name))
            (cl-pushnew (plist-get skill :name) pai-memory--run-views :test #'equal)))
      ;; any other tool after a view: the skill is being applied
      (dolist (n pai-memory--run-views) (pai-memory-record-use n))
      (setq pai-memory--run-views nil))))

(defun pai-memory-track-input (text)
  "Count a `/skill:NAME' input TEXT as a use."
  (when (string-match "\\`[ \t]*/skill:\\([a-z0-9]+\\(?:-[a-z0-9]+\\)*\\)\\(?:[ \t]\\|\\'\\)" (or text ""))
    (let ((name (match-string 1 text)))
      (when (seq-find (lambda (s) (equal (plist-get s :name) name)) (pai-memory--skills))
        (pai-memory-record-use name)))))

;;;; Learned skills

(defun pai-memory-skill-fields (path)
  "Return SKILL file PATH's front-matter as an alist."
  (car (pai-skills--parse-frontmatter (pai-memory--read-file path))))

(defun pai-memory-learned-skill-p (skill)
  "Return non-nil when SKILL was created by the learning loop."
  (equal (cdr (assoc "origin" (pai-memory-skill-fields (plist-get skill :path)))) "learned"))

(defun pai-memory--days-since (time-string)
  "Return the days since TIME-STRING (ISO date or time), or nil."
  (when (and (stringp time-string) (not (string-empty-p time-string)))
    (let ((time (ignore-errors (date-to-time (if (string-match-p "T" time-string)
                                                 time-string
                                               (concat time-string "T00:00:00"))))))
      (and time (/ (float-time (time-subtract nil time)) 86400.0)))))

(defun pai-memory-skill-idle-days (skill record)
  "Return the days since SKILL was last viewed or used (or created)."
  (let ((ds (delq nil (list (pai-memory--days-since (plist-get record :last_used))
                            (pai-memory--days-since (plist-get record :last_viewed))
                            (pai-memory--days-since (cdr (assoc "created" (pai-memory-skill-fields
                                                                           (plist-get skill :path)))))
                            (let ((m (file-attribute-modification-time
                                      (file-attributes (plist-get skill :path)))))
                              (and m (/ (float-time (time-subtract nil m)) 86400.0)))))))
    (if ds (apply #'min ds) 0)))

(defun pai-memory-skill-project (skill cwd)
  "Return CWD when SKILL is one of that project's skills, else nil.
Project skills live in the project's skill directories (`.pai/skills',
`.skills'); everything else, `~/.pai/skills' included, is global."
  (let ((dir (file-name-as-directory (expand-file-name cwd)))
        (path (file-truename (plist-get skill :path))))
    (and (seq-some (lambda (sub)
                     (let ((d (expand-file-name sub dir)))
                       (and (file-directory-p d)
                            (string-prefix-p (file-name-as-directory (file-truename d)) path))))
                   '(".pai/skills" ".skills"))
         dir)))

(defun pai-memory-skill-idle-sessions (skill record cwd)
  "Return the counted sessions since SKILL was last seen, or nil when never stamped.
Project skills (see `pai-memory-skill-project') count CWD's sessions; others
count all sessions."
  (when (plist-member record :seen_global)
    (let ((project (pai-memory-skill-project skill cwd)))
      (if project
          ;; a project missing from the stamp had no sessions yet
          (- (pai-memory-session-count project)
             (or (plist-get (plist-get record :seen_projects) (pai-memory--project-key project)) 0))
        (- (pai-memory-session-count) (or (plist-get record :seen_global) 0))))))

(defun pai-memory-review-candidates (&optional skills table)
  "Return learned SKILLS whose review counts reach `:patch-threshold'.
Each is (SKILL . RECORD); counts from before the skill's last change do not count."
  (let ((table (or table (pai-memory-usage-read)))
        (threshold (or (pai-memory-get :long-term :patch-threshold) 2)))
    (delq nil
          (mapcar (lambda (s)
                    (let* ((r (pai-memory-skill-usage (plist-get s :name) table))
                           (review (plist-get r :review)))
                      (when (and review
                                 (equal (plist-get review :hash) (pai-memory--file-hash (plist-get s :path)))
                                 (>= (+ (or (plist-get review :failed) 0) (or (plist-get review :deviated) 0))
                                     threshold)
                                 (pai-memory-learned-skill-p s))
                        (cons s r))))
                  (or skills (pai-memory--skills))))))

;;;; Curator

(defun pai-memory-archive-dir ()
  "Return the skill archive directory."
  (pai-memory-dir "skill-archive"))

(defun pai-memory-archive-skill (skill)
  "Move learned SKILL's directory to the archive; return the new directory."
  (let* ((dir (directory-file-name (file-name-directory (plist-get skill :path))))
         (name (plist-get skill :name))
         (dest (expand-file-name name (pai-memory-archive-dir))))
    (when (file-exists-p dest)
      (setq dest (format "%s-%s" dest (format-time-string "%Y%m%d%H%M%S"))))
    (make-directory (pai-memory-archive-dir) t)
    (rename-file dir dest)
    (pai-memory--usage-update
     name (lambda (r) (plist-put (plist-put (plist-put r :state "archived") :archived_from dir)
                                 :archived_at (pai-memory--now))))
    dest))

(defun pai-memory-restore-skill (name)
  "Move archived skill NAME back where it was; return a message."
  (let* ((r (pai-memory-skill-usage name))
         (from (plist-get r :archived_from))
         (candidates (and (file-directory-p (pai-memory-archive-dir))
                          (sort (seq-filter (lambda (d) (string-match-p
                                                         (concat "\\`" (regexp-quote name) "\\(-[0-9]+\\)?\\'")
                                                         (file-name-nondirectory d)))
                                            (directory-files (pai-memory-archive-dir) t "\\`[^.]"))
                                #'string>)))
         (src (car candidates)))
    (cond ((null src) (format "No archived skill named %s" name))
          ((null from) (format "Don't know where %s came from; it is in %s" name
                               (abbreviate-file-name src)))
          ((file-exists-p from) (format "%s exists again; restore by hand from %s"
                                        (abbreviate-file-name from) (abbreviate-file-name src)))
          (t (make-directory (file-name-directory from) t)
             (rename-file src from)
             (pai-memory--usage-update
              name (lambda (r) (plist-put (plist-put (plist-put r :state "active") :archived_from nil)
                                          :last_used (pai-memory--now))))
             (format "Restored %s to %s; it shows up from the next session (or /reload)"
                     name (abbreviate-file-name from))))))

(defun pai-memory-archived-skills ()
  "Return the names of archived skills."
  (let ((table (pai-memory-usage-read)) (out '()))
    (while table
      (when (equal (plist-get (cadr table) :state) "archived")
        (push (substring (symbol-name (car table)) 1) out))
      (setq table (cddr table)))
    (nreverse out)))

(defun pai-memory-set-pinned (name pinned)
  "Set skill NAME's pinned flag to PINNED; return a message."
  (if (not (seq-find (lambda (s) (equal (plist-get s :name) name)) (pai-memory--skills)))
      (format "No skill named %s" name)
    (pai-memory--usage-update name (lambda (r) (plist-put r :pinned (if pinned t :false))))
    (format "%s %s" (if pinned "Pinned" "Unpinned") name)))

(defun pai-memory-curate (&optional skills cwd)
  "Run the curator over SKILLS (project CWD) now.
Return (:stale N :archived N :flagged N)."
  (let* ((cwd (or cwd default-directory))
         (skills (seq-filter #'pai-memory-learned-skill-p (or skills (pai-memory--skills))))
         (table (pai-memory-usage-read))
         (get (lambda (k d) (or (pai-memory-get :long-term k) d)))
         (stale-s (funcall get :stale-after-sessions 20))
         (stale-d (funcall get :stale-after-days 14))
         (archive-s (funcall get :archive-after-sessions 60))
         (archive-d (funcall get :archive-after-days 45))
         (stale 0) (archived 0))
    (dolist (s skills)
      (let* ((name (plist-get s :name))
             (r (pai-memory-skill-usage name table))
             (sessions (pai-memory-skill-idle-sessions s r cwd))
             (days (pai-memory-skill-idle-days s r)))
        (cond
         ((pai-truthy (plist-get r :pinned)) nil)
         ;; first sight (or a record from before session counting): start now
         ((null sessions)
          (pai-memory--usage-update name #'pai-memory--mark-seen))
         ((and (>= sessions archive-s) (>= days archive-d))
          (pai-memory-archive-skill s) (cl-incf archived))
         ((and (>= sessions stale-s) (>= days stale-d)
               (not (equal (plist-get r :state) "stale")))
          (pai-memory--usage-update name (lambda (x) (plist-put x :state "stale")))
          (cl-incf stale)))))
    (let ((state (pai-memory-state-read)))
      (pai-memory-state-write (plist-put state :curator_last (pai-memory--now))))
    (list :stale stale :archived archived
          :expired (if (fboundp 'pai-memory-expire-proposals)
                       (or (ignore-errors (pai-memory-expire-proposals cwd)) 0)
                     0)
          :flagged (length (pai-memory-review-candidates)))))

(defun pai-memory-curator-due-p ()
  "Return non-nil when the curator has not run within `:curator-interval-days'."
  (let ((days (pai-memory--days-since (plist-get (pai-memory-state-read) :curator_last))))
    (or (null days) (>= days (or (pai-memory-get :long-term :curator-interval-days) 7)))))

(defun pai-memory-curator-maybe (buffer)
  "In an idle moment, run the curator for pai BUFFER when it is due."
  (run-with-idle-timer
   2 nil
   (lambda ()
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (and (pai-truthy (pai-memory-get :long-term :enabled))
                    (pai-memory-curator-due-p))
           (condition-case err
               (let ((r (pai-memory-curate)))
                 (when (> (+ (plist-get r :stale) (plist-get r :archived)) 0)
                   (message "pai-memory curator: %d skill(s) stale, %d archived"
                            (plist-get r :stale) (plist-get r :archived))))
             (error (message "pai-memory curator failed: %s" (error-message-string err)))))
         (when (and (pai-truthy (pai-memory-get :long-term :enabled))
                    (fboundp 'pai-memory-merge-due-p) (pai-memory-merge-due-p))
           (condition-case err
               (let ((msg (pai-memory-merge)))
                 (unless (string-prefix-p "No overlapping" msg) (message "pai-memory: %s" msg))
                 ;; nothing to do still counts as a run, so it is not retried at once
                 (when (string-prefix-p "No overlapping" msg)
                   (let ((state (pai-memory-state-read)))
                     (pai-memory-state-write (plist-put state :merge_last (pai-memory--now))))))
             (error (message "pai-memory merge failed: %s" (error-message-string err))))))))))

(defun pai-memory-skills-status-text ()
  "Return the /memory skills listing."
  (let* ((table (pai-memory-usage-read))
         (skills (pai-memory--skills))
         (candidates (mapcar (lambda (c) (plist-get (car c) :name))
                             (pai-memory-review-candidates skills table)))
         (archived (pai-memory-archived-skills)))
    (concat
     (if skills
         (mapconcat
          (lambda (s)
            (let* ((r (pai-memory-skill-usage (plist-get s :name) table))
                   (o (plist-get r :outcomes)))
              (format "  %-24s %-8s %s views %d uses %d · outcomes %d/%d/%d%s%s%s"
                      (plist-get s :name)
                      (if (pai-memory-learned-skill-p s) "learned" "manual")
                      (or (plist-get r :state) "active")
                      (or (plist-get r :views) 0) (or (plist-get r :uses) 0)
                      (or (plist-get o :followed) 0) (or (plist-get o :deviated) 0)
                      (or (plist-get o :failed) 0)
                      (let ((n (pai-memory-skill-idle-sessions s r default-directory)))
                        (if n (format " · unused %d session(s)" n) ""))
                      (if (pai-truthy (plist-get r :pinned)) " · pinned" "")
                      (if (member (plist-get s :name) candidates) " · ⚠ needs review" ""))))
          skills "\n")
       "  (no skills)")
     "\n  (outcomes: followed/deviated/failed)"
     (if archived (format "\nArchived: %s" (string-join archived ", ")) ""))))

(provide 'pai-memory-skills)
;;; pai-memory-skills.el ends here
