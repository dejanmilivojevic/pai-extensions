;;; pai-memory-browse.el --- Memory browser buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 F1.  `/memory-browse' shows what pai remembers for the current session
;; and project, one line per item, in sections:
;;
;;   Long-term memory      entries of USER.md, MEMORY.md, project MEMORY.md
;;   Session topics        this session's topic files and JOURNEY.md
;;   Observations          the stored observations on the current branch
;;   Skills                every skill, with usage and curator state
;;   Proposals             pending proposals
;;
;; Keys:
;;   RET  open the item (file, source messages, review buffer)
;;   e    edit: a memory entry through the logged, undoable path; files open
;;   d    remove: a memory entry (logged), an observation (hidden with
;;        `memory.redacted'), a learned skill (archived), a proposal (rejected)
;;   s    show where an observation came from (the session's messages)
;;   /    search memory                g  refresh              q  quit

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-session)
(require 'pai-skills)
(require 'pai-memory-settings)
(require 'pai-memory-store)
(require 'pai-memory-entries)
(require 'pai-memory-ledger)
(require 'pai-memory-compact)
(require 'pai-memory-consolidate)
(require 'pai-memory-proposals)
(require 'pai-memory-skills)
(require 'pai-memory-search)

(defvar pai--session)
(declare-function pai-memory-review "pai-memory-review" ())
(declare-function pai-memory--skill-dirs "pai-memory-promote" ())

(defvar-local pai-memory-browse--owner nil
  "The pai buffer this browser shows memory for.")

(defface pai-memory-browse-heading '((t :inherit bold :height 1.1))
  "Face of section headings in the memory browser.")

(defface pai-memory-browse-dim '((t :inherit shadow))
  "Face of secondary text in the memory browser.")

(defvar pai-memory-browse-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'pai-memory-browse-open)
    (define-key m "e" #'pai-memory-browse-edit)
    (define-key m "d" #'pai-memory-browse-delete)
    (define-key m "s" #'pai-memory-browse-source)
    (define-key m "/" #'pai-memory-browse-search)
    (define-key m "w" #'pai-memory-browse-why)
    (define-key m "P" #'pai-memory-browse-pin)
    (define-key m "g" #'pai-memory-browse-refresh)
    (define-key m "n" #'next-line)
    (define-key m "p" #'previous-line)
    m)
  "Keymap of `pai-memory-browse-mode'.")

(define-derived-mode pai-memory-browse-mode special-mode "pai-memory"
  "Browse what pai remembers.
\\{pai-memory-browse-mode-map}"
  (setq truncate-lines t))

;;;; Context

(defmacro pai-memory-browse--in-owner (&rest body)
  "Run BODY in the owner pai buffer (or here when it is gone)."
  (declare (indent 0))
  `(let ((owner pai-memory-browse--owner))
     (if (buffer-live-p owner) (with-current-buffer owner ,@body) (progn ,@body))))

(defun pai-memory-browse--session ()
  "Return the owner's session, or nil."
  (pai-memory-browse--in-owner (and (boundp 'pai--session) pai--session)))

(defun pai-memory-browse--cwd ()
  "Return the owner's project directory."
  (pai-memory-browse--in-owner default-directory))

;;;; Rendering

(defun pai-memory-browse--line (item text &optional dim)
  "Insert one line for ITEM showing TEXT, followed by DIM text."
  (let ((start (point)))
    (insert "  " (truncate-string-to-width (replace-regexp-in-string "[ \t\n]+" " " text) 100 nil nil "…"))
    (when dim (insert "  " (propertize dim 'face 'pai-memory-browse-dim)))
    (insert "\n")
    (put-text-property start (point) 'pai-memory-item item)))

(defun pai-memory-browse--meta (r)
  "Return the short metadata note shown after entry record R."
  (string-join
   (delq nil (list (and (pai-memory-entry-pinned-p r) "📌")
                   (format "%.1f" (pai-memory-entry-confidence r))
                   (let ((n (length (plist-get r :confirmed)))) (and (> n 0) (format "✓%d" n)))
                   (let ((e (plist-get r :expires)))
                     (and (stringp e) (not (string-empty-p e))
                          (if (pai-memory-entry-expired-p r) (format "expired %s" e) (format "until %s" e))))))
   " "))

(defun pai-memory-browse--heading (text)
  "Insert section heading TEXT."
  (insert "\n" (propertize text 'face 'pai-memory-browse-heading) "\n"))

(defun pai-memory-browse--observation-batches (branch)
  "Return a hash table from observation id to (FROM-ID . TO-ID) on BRANCH."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e branch)
      (when (and (equal (plist-get e :type) "custom")
                 (equal (plist-get e :customType) "memory.observations"))
        (let ((d (plist-get e :data)))
          (dolist (o (append (plist-get d :observations) nil))
            (puthash (plist-get o :id) (cons (plist-get d :coversFromId) (plist-get d :coversUpToId)) h)))))
    h))

(defun pai-memory-browse--render ()
  "Fill the browser buffer."
  (let* ((session (pai-memory-browse--session))
         (cwd (pai-memory-browse--cwd))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Memory" 'face 'pai-memory-browse-heading)
            (propertize "   RET open · e edit · d remove · s source · w why · P pin · / search · g refresh · q quit\n"
                        'face 'pai-memory-browse-dim))
    ;; long-term memory
    (pai-memory-browse--heading "Long-term memory")
    (dolist (target (pai-memory-active-targets cwd))
      (let ((records (ignore-errors (pai-memory-entries target cwd)))
            (entries (pai-memory-read target cwd))
            (file (pai-memory-target-file target cwd)))
        (insert (propertize (format " %s  %s\n" (pai-memory-target-title target)
                                    (abbreviate-file-name file))
                            'face 'pai-memory-browse-dim))
        (if entries
            (dolist (e entries)
              (let ((r (seq-find (lambda (r) (equal (plist-get r :text) e)) records)))
                (pai-memory-browse--line (list :type 'ltm :target target :entry e :file file) e
                                         (and r (pai-memory-browse--meta r)))))
          (pai-memory-browse--line (list :type 'file :file file) "(empty)"))))
    ;; session topics
    (when session
      (let* ((dir (pai-memory-session-dir session))
             (topics (pai-memory-topics dir))
             (journey (expand-file-name "JOURNEY.md" dir)))
        (pai-memory-browse--heading (format "Session topics  %s" (abbreviate-file-name dir)))
        (dolist (tp topics)
          (pai-memory-browse--line (list :type 'file :file (plist-get tp :path))
                                   (plist-get tp :title) (plist-get tp :summary)))
        (when (file-exists-p journey)
          (pai-memory-browse--line (list :type 'file :file journey) "Journey" "JOURNEY.md"))
        (unless (or topics (file-exists-p journey))
          (insert (propertize "  (none yet)\n" 'face 'pai-memory-browse-dim)))))
    ;; observations
    (when session
      (let* ((branch (pai-session-get-branch session))
             (pool (pai-memory-pool branch))
             (batches (pai-memory-browse--observation-batches branch)))
        (pai-memory-browse--heading (format "Observations on this branch (%d)" (length pool)))
        (dolist (o pool)
          (pai-memory-browse--line (list :type 'observation :id (plist-get o :id)
                                         :range (gethash (plist-get o :id) batches)
                                         :content (plist-get o :content))
                                   (plist-get o :content) (plist-get o :timestamp)))
        (unless pool (insert (propertize "  (none)\n" 'face 'pai-memory-browse-dim)))))
    ;; skills
    (let ((skills (pai-discover-skills (pai-memory-browse--in-owner
                                         (and (fboundp 'pai-memory--skill-dirs) (pai-memory--skill-dirs)))))
          (table (pai-memory-usage-read)))
      (pai-memory-browse--heading (format "Skills (%d)" (length skills)))
      (dolist (s skills)
        (let* ((r (pai-memory-skill-usage (plist-get s :name) table))
               (learned (pai-memory-learned-skill-p s)))
          (pai-memory-browse--line
           (list :type 'skill :name (plist-get s :name) :file (plist-get s :path) :learned learned)
           (format "%s — %s" (plist-get s :name) (plist-get s :description))
           (format "%s%s · %d views · %d uses%s"
                   (if learned "learned" "manual")
                   (if (plist-get r :state) (format " · %s" (plist-get r :state)) "")
                   (or (plist-get r :views) 0) (or (plist-get r :uses) 0)
                   (if (pai-truthy (plist-get r :pinned)) " · pinned" "")))))
      (unless skills (insert (propertize "  (none)\n" 'face 'pai-memory-browse-dim))))
    ;; proposals
    (let ((pending (pai-memory-proposals "pending")))
      (pai-memory-browse--heading (format "Pending proposals (%d)" (length pending)))
      (dolist (p pending)
        (pai-memory-browse--line (list :type 'proposal :id (plist-get p :id))
                                 (format "%s %s" (plist-get p :kind)
                                         (or (plist-get p :name) (plist-get p :target)))
                                 (plist-get p :rationale)))
      (unless pending (insert (propertize "  (none)\n" 'face 'pai-memory-browse-dim))))
    (goto-char (point-min))))

(defun pai-memory-browse-refresh ()
  "Redraw the browser, keeping the line."
  (interactive)
  (let ((line (line-number-at-pos)))
    (pai-memory-browse--render)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun pai-memory-browse (&optional owner)
  "Open the memory browser for pai buffer OWNER (default the current buffer)."
  (interactive)
  (let ((owner (or owner (current-buffer)))
        (buf (get-buffer-create "*pai memory*")))
    (with-current-buffer buf
      (pai-memory-browse-mode)
      (setq pai-memory-browse--owner owner)
      (pai-memory-browse--render))
    (if noninteractive buf (pop-to-buffer buf))))

;;;; Actions

(defun pai-memory-browse--item ()
  "Return the item at point, or signal."
  (or (get-text-property (point) 'pai-memory-item) (user-error "No item on this line")))

(defun pai-memory-browse--show (name text)
  "Show TEXT in a read-only buffer NAME; return it."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer) (insert text))
      (special-mode)
      (goto-char (point-min)))
    (unless noninteractive (pop-to-buffer buf))
    buf))

(defun pai-memory-browse-open ()
  "Open the item at point."
  (interactive)
  (let ((item (pai-memory-browse--item)))
    (pcase (plist-get item :type)
      ((or 'file 'skill 'ltm)
       (let ((file (plist-get item :file)))
         (if (file-exists-p file)
             (progn (find-file-other-window file)
                    (when (plist-get item :entry)
                      (goto-char (point-min))
                      (search-forward (car (split-string (plist-get item :entry) "\n")) nil t)
                      (beginning-of-line)))
           (user-error "%s does not exist yet" (abbreviate-file-name file)))))
      ('observation (pai-memory-browse-source))
      ('proposal (pai-memory-review)))))

(defun pai-memory-browse-source ()
  "Show the messages an observation was distilled from."
  (interactive)
  (let* ((item (pai-memory-browse--item))
         (range (plist-get item :range))
         (session (pai-memory-browse--session)))
    (unless (eq (plist-get item :type) 'observation) (user-error "Only observations have a source"))
    (unless (and range session) (user-error "The source of this observation is unknown"))
    (let* ((branch (pai-session-get-branch session))
           (ids (mapcar (lambda (e) (plist-get e :id)) branch))
           (from (seq-position ids (car range)))
           (to (seq-position ids (cdr range))))
      (unless (and from to) (user-error "The source is not on this branch"))
      (pai-memory-browse--show
       "*pai memory source*"
       (concat (format "Observation: %s\nDistilled from:\n\n" (plist-get item :content))
               (mapconcat (lambda (e)
                            (let ((m (pai-memory-entry-message e)))
                              (if m (format "[%s] %s: %s" (plist-get e :id) (pai-message-role m)
                                            (truncate-string-to-width
                                             (or (pai-memory--message-text m 2000) "") 2000 nil nil "…"))
                                "")))
                          (seq-subseq branch from (1+ to)) "\n\n"))))))

(defun pai-memory-browse-edit ()
  "Edit the item at point: memory entries in the minibuffer, files in a buffer."
  (interactive)
  (let ((item (pai-memory-browse--item)))
    (pcase (plist-get item :type)
      ('ltm
       (let* ((old (plist-get item :entry))
              (new (read-string "Entry: " old)))
         (unless (equal new old)
           (let ((r (pai-memory-browse--in-owner
                      (pai-memory-apply-change
                       (list :action 'replace :target (plist-get item :target) :old old :content new
                             :origin "browser")
                       (list :cwd default-directory :session (and (boundp 'pai--session) pai--session))))))
             (message (if (plist-get r :ok) "Saved (/memory undo reverts it)" (plist-get r :error)))))
         (pai-memory-browse-refresh)))
      ((or 'file 'skill) (pai-memory-browse-open))
      (_ (user-error "Nothing to edit here")))))

(defun pai-memory-browse-delete ()
  "Remove the item at point (asks first)."
  (interactive)
  (let ((item (pai-memory-browse--item)))
    (pcase (plist-get item :type)
      ('ltm
       (when (yes-or-no-p "Remove this memory entry? ")
         (let ((r (pai-memory-browse--in-owner
                    (pai-memory-apply-change
                     (list :action 'remove :target (plist-get item :target) :old (plist-get item :entry)
                           :origin "browser")
                     (list :cwd default-directory :session (and (boundp 'pai--session) pai--session))))))
           (message (if (plist-get r :ok) "Removed (/memory undo restores it)" (plist-get r :error))))))
      ('observation
       (when (yes-or-no-p "Hide this observation from memory? ")
         (let ((session (pai-memory-browse--session)))
           (pai-session-append-custom session "memory.redacted"
                                      (list :ids (vector (plist-get item :id))))
           (message "Hidden"))))
      ('skill
       (if (not (plist-get item :learned))
           (user-error "Only learned skills can be archived here; edit hand-written skills yourself")
         (when (yes-or-no-p (format "Archive skill %s? " (plist-get item :name)))
           (pai-memory-archive-skill (list :name (plist-get item :name) :path (plist-get item :file)))
           (message "Archived; /memory-restore-skill %s brings it back" (plist-get item :name)))))
      ('proposal
       (pai-memory-proposal-reject (plist-get item :id) (read-string "Reason (optional): "))
       (message "Rejected"))
      (_ (user-error "Nothing to remove here")))
    (pai-memory-browse-refresh)))

(declare-function pai-memory-why "pai-memory-why" (quote &optional cwd))
(declare-function pai-memory-why--show "pai-memory-why" (fn &rest args))
(declare-function pai-memory-why-skill-insert "pai-memory-why" (path))

(defun pai-memory-browse-why ()
  "Show why pai knows the entry or skill at point (V2 F2)."
  (interactive)
  (let ((item (pai-memory-browse--item)))
    (pcase (plist-get item :type)
      ('ltm (pai-memory-why (plist-get item :entry) (pai-memory-browse--cwd)))
      ((guard (and (plist-get item :file) (equal (file-name-nondirectory (plist-get item :file)) "SKILL.md")))
       (pai-memory-why--show #'pai-memory-why-skill-insert (plist-get item :file)))
      (_ (user-error "`w' works on memory entries and skills")))))

(defun pai-memory-browse-pin ()
  "Toggle the pin of the memory entry at point (kept in the snapshot in retrieval mode)."
  (interactive)
  (let ((item (pai-memory-browse--item)))
    (unless (eq (plist-get item :type) 'ltm) (user-error "`P' pins memory entries"))
    (let* ((cwd (pai-memory-browse--cwd))
           (r (pai-memory-entry-find (plist-get item :entry) cwd)))
      (message "%s" (pai-memory-pin-entry (plist-get item :entry) cwd (not (pai-memory-entry-pinned-p r))))
      (pai-memory-browse-refresh))))

(defun pai-memory-browse-search (query)
  "Search memory for QUERY and show the hits."
  (interactive "sSearch memory: ")
  (let ((text (pai-memory-browse--in-owner
                (pai-memory-index-update 2.0)
                (pai-memory-format-hits (pai-memory-search query :limit 15 :semantic t) query))))
    (pai-memory-browse--show "*pai memory search*" text)))

(provide 'pai-memory-browse)
;;; pai-memory-browse.el ends here
