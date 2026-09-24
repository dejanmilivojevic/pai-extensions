;;; pai-memory-why.el --- "Why do you know this?" provenance -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 F2.  For a long-term memory entry or a skill, show the chain that
;; produced it:
;;
;;   entry / skill  its metadata (B2): origin, confidence, confirmations, expiry
;;   changes        every logged change (log.jsonl) that touched it
;;   proposals      the proposal(s) behind it: rationale and evidence
;;   observations   evidence lines found in the source session's observations
;;   session        the session file, opened read-only at the source entry
;;
;; Entry points: `/memory why QUOTE', `w' in /memory-browse, and
;; `M-x pai-memory-why-at-point' in a USER.md/MEMORY.md/PROJECT.md or SKILL.md
;; buffer.  Every file and session in the report is a button.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'button)
(require 'pai-core)
(require 'pai-skills)
(require 'pai-memory-settings)
(require 'pai-memory-store)
(require 'pai-memory-entries)
(require 'pai-memory-proposals)

(declare-function pai-memory--skill-dirs "pai-memory-promote" ())
(declare-function pai-memory-skill-usage "pai-memory-skills" (name &optional table))

(defface pai-memory-why-heading '((t :inherit bold :height 1.1)) "Headings in the why buffer.")
(defface pai-memory-why-dim '((t :inherit shadow)) "Secondary text in the why buffer.")

;;;; Lookups

(defun pai-memory-session-file (id)
  "Return the session file of session ID, or nil."
  (and (stringp id) (not (string-empty-p id))
       (car (file-expand-wildcards
             (expand-file-name (concat "sessions/*/" id ".jsonl") pai-directory)))))

(defun pai-memory--log-records (ids)
  "Return the logged changes whose id is in IDS, oldest first."
  (seq-filter (lambda (r) (member (plist-get r :id) ids)) (pai-memory-log-read)))

(defun pai-memory--log-for-file (file)
  "Return logged changes (and change groups) that touched FILE, oldest first."
  (let ((key (pai-memory--file-key file)))
    (seq-filter (lambda (r)
                  (or (equal (pai-memory--file-key (or (plist-get r :file) "")) key)
                      (seq-some (lambda (op)
                                  (equal (pai-memory--file-key (or (plist-get op :file) "")) key))
                                (append (plist-get r :group) nil))))
                (pai-memory-log-read))))

(defun pai-memory-session-observations (file)
  "Return (OBSERVATION . BATCH-DATA) for every observation in session FILE."
  (let ((out '()))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (search-forward "\"memory.observations\"" nil t)
          (let ((e (ignore-errors (pai-json-decode (buffer-substring-no-properties
                                                     (line-beginning-position) (line-end-position))))))
            (when (equal (plist-get e :customType) "memory.observations")
              (dolist (o (append (plist-get (plist-get e :data) :observations) nil))
                (push (cons o (plist-get e :data)) out))))
          (forward-line 1))))
    (nreverse out)))

(defun pai-memory--evidence-observations (evidence file)
  "Return the observations of session FILE matching EVIDENCE strings."
  (let ((obs (pai-memory-session-observations file)) (out '()))
    (dolist (ev (pai-memory--string-list evidence))
      (let* ((needle (string-trim (replace-regexp-in-string "\\`\\[[^]]*\\] *" "" ev)))
             (needle (if (> (length needle) 60) (substring needle 0 60) needle)))
        (when (>= (length needle) 8)
          (dolist (o obs)
            (let ((content (or (plist-get (car o) :content) "")))
              (when (and (or (string-search needle content)
                             (string-search (substring content 0 (min 60 (length content))) ev))
                         (not (assq (car o) out)))
                (push o out)))))))
    (nreverse out)))

;;;; Opening things

(defun pai-memory-why-open-session (file &optional entry-id)
  "Open session FILE read-only, at ENTRY-ID's line when given."
  (find-file-read-only file)
  (goto-char (point-min))
  (when (and entry-id (not (string-empty-p entry-id)))
    (when (re-search-forward (format "\"id\":\"%s\"" (regexp-quote entry-id)) nil t)
      (beginning-of-line)
      (recenter 3))))

(defun pai-memory-why--button (label action &optional help)
  (insert-text-button label 'action (lambda (_b) (funcall action)) 'follow-link t
                      'help-echo (or help "Open")))

(defun pai-memory-why--file-button (file)
  (let ((f (expand-file-name file)))
    (pai-memory-why--button (abbreviate-file-name f) (lambda () (find-file f)))))

(defun pai-memory-why--session-button (id &optional entry-id label)
  (let ((file (pai-memory-session-file id)))
    (if file
        (pai-memory-why--button (or label (format "session %s" id))
                                (lambda () (pai-memory-why-open-session file entry-id))
                                "Open the session read-only")
      (insert (format "session %s (file not found)" id)))))

;;;; Rendering

(defun pai-memory-why--h (text)
  (insert "\n" (propertize text 'face 'pai-memory-why-heading) "\n"))

(defun pai-memory-why--kv (key value)
  (when (and value (not (equal value "")))
    (insert (propertize (format "  %-14s" key) 'face 'pai-memory-why-dim) (format "%s" value) "\n")))

(defun pai-memory-why--changes (records)
  (pai-memory-why--h "Changes")
  (if (null records)
      (insert "  (none logged)\n")
    (dolist (r records)
      (insert (format "  %s  %-14s %s" (substring (or (plist-get r :time) "") 0 (min 16 (length (or (plist-get r :time) ""))))
                      (plist-get r :action)
                      (let ((o (plist-get r :origin))) (if (member o '(nil "")) "" (format "by %s" o)))))
      (let ((s (plist-get r :session)))
        (when (and s (not (string-empty-p s)))
          (insert "  ") (pai-memory-why--session-button s)))
      (insert (propertize (format "  [%s]" (plist-get r :id)) 'face 'pai-memory-why-dim) "\n"))))

(defun pai-memory-why--proposal (id &optional title)
  "Insert proposal ID with its rationale, evidence and matching observations."
  (let ((p (and id (not (string-empty-p id)) (pai-memory-proposal-load id))))
    (when p
      (pai-memory-why--h (or title (format "Proposal %s" id)))
      (pai-memory-why--kv "kind" (plist-get p :kind))
      (pai-memory-why--kv "status" (plist-get p :status))
      (pai-memory-why--kv "created" (plist-get p :created))
      (pai-memory-why--kv "rationale" (plist-get p :rationale))
      (let ((ev (pai-memory--string-list (plist-get p :evidence))))
        (when ev
          (insert (propertize "  evidence\n" 'face 'pai-memory-why-dim))
          (dolist (e ev) (insert "    • " e "\n"))))
      (let* ((sid (plist-get p :session)) (file (pai-memory-session-file sid)))
        (when (and sid (not (string-empty-p sid)))
          (insert (propertize "  from          " 'face 'pai-memory-why-dim))
          (pai-memory-why--session-button sid)
          (insert "\n"))
        (let ((obs (and file (pai-memory--evidence-observations (plist-get p :evidence) file))))
          (when obs
            (insert (propertize "  observations\n" 'face 'pai-memory-why-dim))
            (dolist (o obs)
              (insert "    " (truncate-string-to-width (or (plist-get (car o) :content) "") 110 nil nil "…")
                      "\n      ")
              (pai-memory-why--button "→ in the conversation"
                                      (let ((from (plist-get (cdr o) :coversFromId)))
                                        (lambda () (pai-memory-why-open-session file from)))
                                      "Open the session where this was observed")
              (insert "\n")))))
      t)))

(defun pai-memory-why-entry-insert (r)
  "Insert the provenance chain of entry record R."
  (insert (propertize "Why do you know this?" 'face 'pai-memory-why-heading) "\n\n")
  (insert "  " (replace-regexp-in-string "\n" "\n  " (plist-get r :text)) "\n")
  (pai-memory-why--h "Entry")
  (pai-memory-why--kv "id" (plist-get r :id))
  (insert (propertize (format "  %-14s" "file") 'face 'pai-memory-why-dim))
  (pai-memory-why--file-button (plist-get r :file)) (insert "\n")
  (pai-memory-why--kv "origin" (pcase (plist-get r :origin)
                                 ("manual" "written by hand")
                                 ("memory-tool" "saved by the memory tool, at the user's request")
                                 ("proposal" "learned: an accepted proposal")
                                 ("migrated" "from the change log (before entry metadata existed)")
                                 (o o)))
  (pai-memory-why--kv "confidence" (format "%.1f" (pai-memory-entry-confidence r)))
  (pai-memory-why--kv "created" (plist-get r :created))
  (pai-memory-why--kv "updated" (unless (equal (plist-get r :updated) (plist-get r :created)) (plist-get r :updated)))
  (pai-memory-why--kv "expires" (let ((e (plist-get r :expires)))
                                  (and e (not (string-empty-p e))
                                       (if (pai-memory-entry-expired-p r) (concat e " (expired: not in the snapshot)") e))))
  (pai-memory-why--kv "pinned" (and (pai-memory-entry-pinned-p r) "yes"))
  (pai-memory-why--kv "removed" (let ((x (plist-get r :removed))) (and x (not (string-empty-p x)) x)))
  (let ((sid (plist-get r :source_session)))
    (when (and sid (not (string-empty-p sid)))
      (insert (propertize (format "  %-14s" "session") 'face 'pai-memory-why-dim))
      (pai-memory-why--session-button sid) (insert "\n")))
  (let ((conf (append (plist-get r :confirmed) nil)))
    (when conf
      (pai-memory-why--h (format "Confirmed %d time%s" (length conf) (if (cdr conf) "s" "")))
      (dolist (c conf)
        (insert (format "  %s  " (plist-get c :time)))
        (let ((s (plist-get c :session)))
          (when (and s (not (string-empty-p s))) (pai-memory-why--session-button s)))
        (let ((p (and (not (string-empty-p (or (plist-get c :proposal) ""))) (pai-memory-proposal-load (plist-get c :proposal)))))
          (when p (insert "  — " (plist-get p :rationale))))
        (insert "\n"))))
  (pai-memory-why--changes (pai-memory--log-records (append (plist-get r :changes) nil)))
  (let ((ids (delete-dups
              (delq nil (cons (let ((x (plist-get r :proposal_id))) (and x (not (string-empty-p x)) x))
                              (mapcar (lambda (c) (plist-get c :proposal_id))
                                      (pai-memory--log-records (append (plist-get r :changes) nil))))))))
    (setq ids (seq-remove #'string-empty-p ids))
    (unless (seq-filter #'identity (mapcar #'pai-memory-why--proposal ids))
      (pai-memory-why--h "Proposal")
      (insert "  (none: not learned from a session)\n"))))

(defun pai-memory-why-skill-insert (path)
  "Insert the provenance chain of the skill at PATH."
  (let* ((text (pai-memory--read-file path))
         (fm (car (pai-memory--frontmatter-alist text)))
         (name (or (cdr (assoc "name" fm)) (file-name-nondirectory (directory-file-name (file-name-directory path)))))
         (log (pai-memory--log-for-file path)))
    (insert (propertize (format "Why is there a skill %s?" name) 'face 'pai-memory-why-heading) "\n")
    (pai-memory-why--h "Skill")
    (insert (propertize (format "  %-14s" "file") 'face 'pai-memory-why-dim))
    (pai-memory-why--file-button path) (insert "\n")
    (pai-memory-why--kv "description" (cdr (assoc "description" fm)))
    (pai-memory-why--kv "origin" (or (cdr (assoc "origin" fm)) "written by hand"))
    (pai-memory-why--kv "created" (or (cdr (assoc "created" fm)) (cdr (assoc "learned" fm))))
    (pai-memory-why--kv "imported from" (cdr (assoc "imported-from" fm)))
    (let ((sid (cdr (assoc "source-session" fm))))
      (when sid
        (insert (propertize (format "  %-14s" "session") 'face 'pai-memory-why-dim))
        (pai-memory-why--session-button sid) (insert "\n")))
    (when (fboundp 'pai-memory-skill-usage)
      (let* ((u (pai-memory-skill-usage name)) (o (plist-get u :outcomes)))
        (when u
          (pai-memory-why--kv "usage" (format "%d views, %d uses; followed %d, deviated %d, failed %d"
                                              (or (plist-get u :views) 0) (or (plist-get u :uses) 0)
                                              (or (plist-get o :followed) 0) (or (plist-get o :deviated) 0)
                                              (or (plist-get o :failed) 0))))))
    (pai-memory-why--changes log)
    (let ((ids (delete-dups (seq-remove (lambda (x) (member x '(nil "")))
                                        (mapcar (lambda (r) (plist-get r :proposal_id)) log)))))
      (unless (seq-filter #'identity (mapcar #'pai-memory-why--proposal ids))
        (pai-memory-why--h "Proposal")
        (insert "  (none logged)\n")))))

(defun pai-memory-why--show (fn &rest args)
  "Show a why buffer filled by FN with ARGS; return the buffer."
  (let ((buf (get-buffer-create "*pai-memory-why*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply fn args)
        (goto-char (point-min)))
      (special-mode))
    (display-buffer buf)
    buf))

(defun pai-memory-why (quote &optional cwd)
  "Show why pai knows the memory entry containing QUOTE, or the skill named QUOTE."
  (let* ((cwd (or cwd default-directory))
         (skill (seq-find (lambda (s) (equal (plist-get s :name) quote))
                          (ignore-errors (pai-discover-skills (and (fboundp 'pai-memory--skill-dirs)
                                                                   (pai-memory--skill-dirs)))))))
    (if skill
        (pai-memory-why--show #'pai-memory-why-skill-insert (plist-get skill :path))
      (pai-memory-why--show #'pai-memory-why-entry-insert (pai-memory-entry-find quote cwd)))))

(defun pai-memory--entry-at-point ()
  "Return the § entry around point in a memory file buffer."
  (let ((sep "^[ \t]*§[ \t]*$"))
    (string-trim
     (buffer-substring-no-properties
      (save-excursion (if (re-search-backward sep nil t) (line-end-position) (point-min)))
      (save-excursion (if (re-search-forward sep nil t) (line-beginning-position) (point-max)))))))

;;;###autoload
(defun pai-memory-why-at-point ()
  "Explain the memory entry or skill in the current buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((and file (member (file-name-nondirectory file) '("SKILL.md")))
      (pai-memory-why--show #'pai-memory-why-skill-insert file))
     ((and file (member (file-name-nondirectory file) '("USER.md" "MEMORY.md" "PROJECT.md")))
      (let* ((text (pai-memory--entry-at-point))
             (hash (pai-memory--entry-hash text))
             (key (pai-memory--file-key file))
             (find (lambda ()
                     (seq-find (lambda (r) (and (equal (plist-get r :file) key) (pai-memory--live-p r)
                                                (equal (plist-get r :text_hash) hash)))
                               (pai-memory--meta-load))))
             (r (or (funcall find)
                    ;; not synced yet: reconcile the file's project, then look again
                    (progn (ignore-errors
                             (pai-memory-entries-all
                              (or (and (equal (file-name-nondirectory file) "PROJECT.md")
                                       (file-name-directory (directory-file-name (file-name-directory
                                                                                  (directory-file-name (file-name-directory file))))))
                                  default-directory)))
                           (funcall find)))))
        (unless r (user-error "No saved entry here (save the file first?)"))
        (pai-memory-why--show #'pai-memory-why-entry-insert r)))
     (t (user-error "Not a memory file or SKILL.md")))))

(provide 'pai-memory-why)
;;; pai-memory-why.el ends here
