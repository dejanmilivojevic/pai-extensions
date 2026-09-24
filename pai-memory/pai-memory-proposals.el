;;; pai-memory-proposals.el --- Proposal queue: validate, store, apply -*- lexical-binding: t; -*-

;;; Commentary:

;; Proposals (SPEC §7.2) are pending changes to long-term memory or skills,
;; one JSON file each in ~/.pai/memory/proposals/:
;;
;;   {id, kind, target, name, scope, content, old, before, after, rationale,
;;    evidence, session, cwd, created, status, risk, block, lint, reason, applied}
;;
;; Skill proposals carry the results of `pai-memory-skill-check': `risk'
;; (every security finding), `block' (the blocking ones) and `lint'
;; (advisory).  A newer skill proposal for the same file replaces a pending
;; one.
;;
;; Kinds:
;;   skill-create    NAME, SCOPE (global|project), CONTENT = full SKILL.md
;;   skill-patch     TARGET = path of an existing skill, CONTENT = full new text
;;   snippet-create  NAME, SCOPE, CONTENT = the prompt snippet file
;;   snippet-patch   TARGET = path of an existing snippet, CONTENT = new text
;;                   (see `pai-memory-snippets')
;;   memory-add      TARGET = user|memory|project, CONTENT
;;   memory-replace  TARGET, OLD (unique quote of the entry), CONTENT
;;   memory-remove   TARGET, OLD
;;   memory-confirm  TARGET, OLD: the session confirmed the entry again (V2
;;                   B2); applied at once, it only adds to its metadata
;;   team-memory-add|replace|remove  like memory-*, for the repository's
;;                   PROJECT.md (V2 G2); always reviewed
;; memory-add and memory-replace may carry EXPIRES (YYYY-MM-DD).
;; Unknown kinds are kept and shown read-only (FC3).
;;
;; Status: pending -> accepted | rejected | stale.  `before' is the target
;; file's text when the proposal was made.  Skill proposals replace a whole
;; file, so applying one whose file changed since is refused and the proposal
;; marked stale; memory proposals are entry-level edits that are re-checked
;; against the file as it is now, so they never overwrite a later change.
;;
;; Every applied change goes through the change log (`pai-memory-dir'
;; log.jsonl) with the file before and after, so `/memory undo' reverts
;; skills too (a created skill is deleted again).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-skills)
(require 'pai-session)
(require 'pai-trust)
(require 'pai-memory-settings)
(require 'pai-memory-budget)
(require 'pai-memory-store)
(require 'pai-memory-quality)
(require 'pai-memory-entries)
(require 'pai-memory-snippets)

(defconst pai-memory-promoter-kinds
  '("skill-create" "skill-patch" "snippet-create" "snippet-patch" "memory-add" "memory-replace" "memory-remove" "memory-confirm"
    "team-memory-add" "team-memory-replace" "team-memory-remove")
  "Proposal kinds the promoter may file.")

(defconst pai-memory-proposal-kinds
  (append pai-memory-promoter-kinds '("skill-merge" "skill-archive" "topic-conflict"))
  "Proposal kinds this version can apply.")

(declare-function pai-memory-apply-merge "pai-memory-merge" (p &optional content))
(declare-function pai-memory-apply-archive "pai-memory-merge" (p))
(declare-function pai-memory-apply-topic-conflict "pai-memory-topics" (p &optional content))
(declare-function pai-memory-project-topics-dir "pai-memory-topics" (cwd))

;;;; Storage

(defun pai-memory-proposals-dir ()
  "Return the proposal directory."
  (pai-memory-dir "proposals"))

(defun pai-memory-proposal-file (id)
  "Return the file of proposal ID."
  (expand-file-name (concat id ".json") (pai-memory-proposals-dir)))

(defun pai-memory-proposal-save (proposal)
  "Write PROPOSAL atomically; return it."
  (let* ((file (pai-memory-proposal-file (plist-get proposal :id)))
         (dir (file-name-directory file))
         (tmp nil))
    (make-directory dir t)
    (setq tmp (make-temp-file (expand-file-name ".p-" dir)))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file tmp (insert (pai-json-encode proposal))))
    (rename-file tmp file t)
    proposal))

(defun pai-memory-proposal-load (id)
  "Return proposal ID, or nil."
  (let ((file (pai-memory-proposal-file id)))
    (and (file-readable-p file)
         (ignore-errors (with-temp-buffer (insert-file-contents file)
                                          (pai-json-decode (buffer-string)))))))

(defun pai-memory-proposals (&optional status)
  "Return proposals, oldest first; only those with STATUS when non-nil."
  (let ((dir (pai-memory-proposals-dir)))
    (when (file-directory-p dir)
      (seq-filter (lambda (p) (or (null status) (equal (plist-get p :status) status)))
                  (sort (delq nil (mapcar (lambda (f)
                                            (ignore-errors
                                              (with-temp-buffer (insert-file-contents f)
                                                                (pai-json-decode (buffer-string)))))
                                          (directory-files dir t "\\.json\\'")))
                        (lambda (a b) (string< (plist-get a :id) (plist-get b :id))))))))

(defun pai-memory-pending-count ()
  "Return how many proposals wait for review."
  (length (pai-memory-proposals "pending")))

(defun pai-memory-recent-rejections (&optional n)
  "Return the N (default 10) newest rejected proposals."
  (last (pai-memory-proposals "rejected") (or n 10)))

;;;; Skills

(defun pai-memory-learned-skills-dir (scope cwd)
  "Return the directory learned skills of SCOPE go to for project CWD.
A `project' scope is honoured only in projects the user trusts; otherwise
learned skills are global."
  (if (and (equal (format "%s" scope) "project") (eq (pai-trust-get cwd) 'yes))
      (expand-file-name ".pai/skills/learned" cwd)
    (expand-file-name "skills/learned" pai-directory)))

(defun pai-memory-skill-file (name scope cwd)
  "Return the SKILL.md path of learned skill NAME in SCOPE for project CWD."
  (expand-file-name (concat name "/SKILL.md") (pai-memory-learned-skills-dir scope cwd)))

(defun pai-memory--frontmatter-alist (text)
  "Return (FIELDS . BODY) of skill TEXT (see `pai-skills--parse-frontmatter')."
  (pai-skills--parse-frontmatter text))

(defun pai-memory-normalize-skill (text name session-id &optional provenance)
  "Return skill TEXT with learned-skill front-matter for NAME.
Keeps the model's fields, forces name, and records origin, created and
source-session.  PROVENANCE, an alist of (FIELD . VALUE), replaces those
three (e.g. origin: imported).  Signals a `user-error' when the description
is missing."
  (let* ((parsed (pai-memory--frontmatter-alist text))
         (fields (car parsed))
         (body (string-trim-left (cdr parsed)))
         (desc (cdr (assoc "description" fields))))
    (unless (and desc (not (string-empty-p desc)))
      (user-error "A skill needs a front-matter description (when to use it)"))
    (when (> (length desc) 1024)
      (user-error "The description is over 1024 characters"))
    (concat "---\n"
            (format "name: %s\n" name)
            (format "description: %s\n" desc)
            (mapconcat (lambda (f) (format "%s: %s\n" (car f) (cdr f)))
                       (seq-remove (lambda (f) (member (car f) (append '("name" "description" "origin"
                                                                         "created" "source-session")
                                                                       (mapcar #'car provenance))))
                                   fields)
                       "")
            (if provenance
                (mapconcat (lambda (f) (format "%s: %s\n" (car f) (cdr f))) provenance "")
              (concat "origin: learned\n"
                      (format "created: %s\n" (format-time-string "%Y-%m-%d"))
                      (format "source-session: %s\n" (or session-id "unknown"))))
            "---\n"
            body
            (if (string-suffix-p "\n" body) "" "\n"))))

(defconst pai-memory-provenance-fields
  '("origin" "created" "source-session" "learned-by" "learned" "imported" "imported-from")
  "Front-matter fields that say where a skill came from.")

(defun pai-memory--provenance-of (text)
  "Return TEXT's provenance fields as an alist when it states an origin, else nil."
  (let ((fields (car (pai-memory--frontmatter-alist text))))
    (when (assoc "origin" fields)
      (seq-filter (lambda (f) (member (car f) pai-memory-provenance-fields)) fields))))

(defun pai-memory-skill-paths (&optional dirs)
  "Return the SKILL paths discovered in DIRS (default the skill directories)."
  (mapcar (lambda (s) (plist-get s :path)) (pai-discover-skills dirs)))

;;;; Checks (pai-memory-quality)

(defun pai-memory--skill-checks (text cwd)
  "Return the proposal fields for skill TEXT: :risk (all), :block and :lint."
  (let ((r (pai-memory-skill-check text cwd)))
    (list :risk (vconcat (append (plist-get r :warn) (plist-get r :block)))
          :block (vconcat (plist-get r :block))
          :lint (vconcat (plist-get r :lint)))))

;;;; Creating proposals

(defvar pai-memory--proposal-seq 0
  "Per-process counter keeping proposal ids ordered within one second.")

(defun pai-memory--proposal-id ()
  "Return a fresh proposal id that sorts in creation order."
  (format "p-%s-%06d-%04x" (format-time-string "%Y%m%dT%H%M%S")
          (setq pai-memory--proposal-seq (% (1+ pai-memory--proposal-seq) 1000000))
          (random 65536)))

(defun pai-memory--string-list (value)
  "Return VALUE, a list of strings given loosely, as a list of strings.
Models sometimes send an array field as one string (often a bulleted
list): split it into its non-empty lines, dropping bullet markers.  A
list of character codes (such a string mangled by `append') is turned
back into a string first.  Vectors and lists of other values are
stringified element by element."
  (cond
   ((null value) nil)
   ((stringp value)
    (delq nil (mapcar (lambda (l)
                        (let ((l (string-trim (replace-regexp-in-string
                                               "\\`[ \t]*\\(?:[-*•]\\|[0-9]+[.)]\\)[ \t]+" "" l))))
                          (unless (string-empty-p l) l)))
                      (split-string value "\n"))))
   ((and (sequencep value) (> (length value) 0) (seq-every-p #'characterp value))
    (pai-memory--string-list (concat value)))
   ((sequencep value)
    (delq nil (mapcar (lambda (e) (cond ((stringp e) e) ((null e) nil) (t (format "%s" e))))
                      (append value nil))))
   (t (list (format "%s" value)))))

(defun pai-memory--dedup-key (p)
  "Return the key under which a new proposal replaces a pending one like P.
Skill proposals replace the pending one for the same file, so an improved
version supersedes the first; memory proposals only an identical change."
  (if (string-match-p "\\`\\(?:skill\\|snippet\\)-" (or (plist-get p :kind) ""))
      (list (plist-get p :kind) (plist-get p :target))
    (list (plist-get p :kind) (plist-get p :target) (plist-get p :after) (plist-get p :old))))

(defun pai-memory--clean-references (refs)
  "Validate REFS, a sequence of (:path P :content C); return them as a vector.
Paths are relative, under references/, end in .md, and stay inside the skill."
  (let ((out '()))
    (dolist (r (append refs nil))
      (let ((path (string-trim (or (plist-get r :path) ""))))
        (unless (string-prefix-p "references/" path) (setq path (concat "references/" path)))
        (unless (and (string-match-p "\\`references/[A-Za-z0-9._/-]+\\.md\\'" path)
                     (not (string-match-p "\\.\\." path)))
          (user-error "Reference path %s must be references/NAME.md" path))
        (when (string-empty-p (string-trim (or (plist-get r :content) "")))
          (user-error "Reference %s is empty" path))
        (when (assoc path out) (user-error "Reference %s is given twice" path))
        (push (cons path (plist-get r :content)) out)))
    (when (> (length out) 30) (user-error "At most 30 reference files"))
    (vconcat (mapcar (lambda (c) (list :path (car c) :content (cdr c))) (nreverse out)))))

(cl-defun pai-memory-make-proposal (&key kind target name scope content old rationale evidence
                                         session-id cwd skill-dirs references provenance
                                         skill-dir extra expires)
  "Validate and return a new proposal plist, or signal a `user-error'.
SKILL-DIRS are the discovered skill directories (for skill-patch targets).
REFERENCES, for skill-create, are extra (:path \"references/X.md\" :content C)
files of a knowledge-base skill.  PROVENANCE replaces the learned-skill
front-matter (see `pai-memory-normalize-skill'); SKILL-DIR overrides where a
created skill goes; EXTRA is a plist merged into the proposal."
  (let* ((cwd (file-name-as-directory (expand-file-name (or cwd default-directory))))
         (rationale (string-trim (or rationale "")))
         (evidence (pai-memory--string-list evidence))
         (base (list :id (pai-memory--proposal-id) :kind kind :rationale rationale
                     :evidence (vconcat evidence) :session (or session-id "")
                     :cwd cwd :created (format-time-string "%FT%T%z") :status "pending")))
    (when (string-empty-p rationale) (user-error "Give a rationale"))
    (pcase kind
      ("skill-create"
       (unless (pai-skills--valid-name-p name)
         (user-error "Skill name must be lowercase-kebab-case, at most 64 characters"))
       (let* ((scope (if (equal scope "project") "project" "global"))
              (file (if skill-dir (expand-file-name (concat name "/SKILL.md") skill-dir)
                      (pai-memory-skill-file name scope cwd)))
              (after (pai-memory-normalize-skill (or content "") name session-id provenance))
              (refs (pai-memory--clean-references references)))
         (when (or (file-exists-p file) (member name (mapcar (lambda (s) (plist-get s :name))
                                                             (pai-discover-skills skill-dirs))))
           (user-error "A skill named %s already exists; propose a skill-patch instead" name))
         (let ((checks (pai-memory--skill-checks after cwd))
               (ref-scan (pai-memory-security-scan
                          (mapconcat (lambda (r) (plist-get r :content)) refs "\n") cwd)))
           ;; references are scanned for danger too (lint is for SKILL.md)
           (append base (list :target file :name name :scope scope :content after
                              :before "" :after after :references refs
                              :risk (vconcat (delete-dups (append (plist-get checks :risk)
                                                                  (plist-get ref-scan :warn)
                                                                  (plist-get ref-scan :block))))
                              :block (vconcat (delete-dups (append (plist-get checks :block)
                                                                   (plist-get ref-scan :block))))
                              :lint (plist-get checks :lint))
                   extra))))
      ("skill-patch"
       (let ((file (and target (expand-file-name target cwd))))
         (unless (and file (member file (pai-memory-skill-paths skill-dirs)))
           (user-error "skill-patch target must be the path of an existing skill"))
         (let ((before (pai-memory--read-file file))
               (after (or content "")))
           (when (string-empty-p (string-trim after)) (user-error "Give the full new skill text"))
           (when (equal before after) (user-error "The patch changes nothing"))
           (unless (cdr (assoc "description" (car (pai-memory--frontmatter-alist after))))
             (user-error "Keep the skill's front-matter (name, description)"))
           (append base (list :target file :name (file-name-nondirectory
                                                  (directory-file-name (file-name-directory file)))
                              :content after :before before :after after)
                   (pai-memory--skill-checks after cwd)
                   extra))))
      ("snippet-create"
       (unless (pai-skills--valid-name-p name)
         (user-error "Snippet name must be lowercase-kebab-case, at most 64 characters"))
       (let* ((scope (if (equal scope "project") "project" "global"))
              (file (pai-memory-snippet-file name scope cwd))
              (after (pai-memory-normalize-snippet content name session-id provenance)))
         (when (or (file-exists-p file)
                   (member (concat name ".md")
                           (mapcar (lambda (s) (plist-get s :id)) (pai-memory-snippets cwd))))
           (user-error "A snippet named %s already exists; propose a snippet-patch instead" name))
         (append base (list :target file :name name :scope scope :content after
                            :before "" :after after)
                 (pai-memory-snippet-checks after cwd)
                 extra)))
      ("snippet-patch"
       (let ((file (and target (expand-file-name target cwd))))
         (unless (and file (member (file-truename file)
                                   (mapcar #'file-truename (pai-memory-snippet-paths cwd))))
           (user-error "snippet-patch target must be the path of an existing snippet"))
         (let ((before (pai-memory--read-file file))
               (after (or content "")))
           (when (string-empty-p (string-trim after)) (user-error "Give the full new snippet text"))
           (when (equal before after) (user-error "The patch changes nothing"))
           ;; validate only: a patch keeps the snippet's own front-matter
           (pai-memory-normalize-snippet after (file-name-base file) session-id '(("origin" . "x")))
           (append base (list :target file :name (file-name-base file)
                              :content after :before before :after after)
                   (pai-memory-snippet-checks after cwd)
                   extra))))
      ("topic-conflict"
       (let ((file (and target (expand-file-name target))))
         (unless (and file (file-exists-p file)
                      (string-prefix-p (pai-memory-project-topics-dir cwd) file))
           (user-error "topic-conflict target must be a project topic file"))
         (when (string-empty-p (string-trim (or content ""))) (user-error "Give the resolved text"))
         (append base (list :target file :name (file-name-base file) :content content
                            :before (pai-memory--read-file file) :after content
                            :risk [] :block [] :lint []))))
      ("memory-confirm"
       (let* ((tgt (symbol-name (pai-memory--target target)))
              (entries (pai-memory-read tgt cwd))
              (i (pai-memory--find-entry entries old)))
         (append base (list :target tgt :content (nth i entries) :old old
                            :before "" :after "" :risk [] :block [] :lint []))))
      ((or "memory-add" "memory-replace" "memory-remove"
           "team-memory-add" "team-memory-replace" "team-memory-remove")
       (let* ((team (string-prefix-p "team-" kind))
              (tgt (if team "team" (symbol-name (pai-memory--target target))))
              (action (substring kind (if team 12 7)))
              (expires (and expires (not (string-empty-p expires)) expires))
              (file (pai-memory-target-file tgt cwd))
              (before (pai-memory--read-file file))
              (entries (pai-memory-change-entries
                        (pai-memory-parse-entries before)
                        (list :action action :content content :old old)))
              (after (pai-memory-render-entries entries))
              (limit (pai-memory-target-limit tgt nil cwd)))
         (when (and (equal tgt "team") (not team))
           (user-error "Team memory is shared in the repository: use team-memory-%s" action))
         (when (and team (not (pai-memory-team-allowed-p cwd)))
           (user-error "Team memory is only used in trusted projects"))
         (when (and expires (not (pai-memory-valid-date-p expires)))
           (user-error "expires must be a date, YYYY-MM-DD"))
         (when (and limit (> (length after) limit) (not (pai-memory-retrieval-mode-p)))
           (user-error "%s would grow to %d characters, over its %d limit; propose a memory-replace or memory-remove instead"
                       (file-name-nondirectory file) (length after) limit))
         (append base (list :target tgt :content (pai-memory--clean content) :old (or old "")
                            :before before :after after :risk [] :block [] :lint [])
                 (and expires (list :expires expires)))))
      (_ (user-error "Unknown proposal kind %s" kind)))))

(defun pai-memory-add-proposal (proposal)
  "Store PROPOSAL, replacing a pending one with the same kind, target and result.
Return the stored proposal."
  (let ((key (pai-memory--dedup-key proposal)))
    (dolist (p (pai-memory-proposals "pending"))
      (when (equal (pai-memory--dedup-key p) key)
        (ignore-errors (delete-file (pai-memory-proposal-file (plist-get p :id))))))
    (pai-memory-proposal-save proposal)))

;;;; Applying

(defun pai-memory--set-status (proposal status &rest kv)
  "Save PROPOSAL with STATUS and KV; return it."
  (let ((p (plist-put (copy-sequence proposal) :status status)))
    (unless (equal status "pending")
      (setq p (plist-put p :decided (format-time-string "%FT%T%z"))))
    (while kv (setq p (plist-put p (car kv) (cadr kv)) kv (cddr kv)))
    (pai-memory-proposal-save p)))

(defun pai-memory--apply-skill (proposal text)
  "Write skill TEXT for PROPOSAL; return (:ok t :id ID) or (:error MSG)."
  (let* ((file (plist-get proposal :target))
         (now (pai-memory--read-file file))
         (creating (member (plist-get proposal :kind) '("skill-create" "snippet-create")))
         (label (if (string-prefix-p "snippet-" (plist-get proposal :kind)) "snippet" "skill")))
    (cond
     ((and creating (file-exists-p file))
      (list :error (format "%s exists now; the proposal is stale" (abbreviate-file-name file))))
     ((and (not creating) (not (equal now (plist-get proposal :before))))
      (list :error (format "%s changed since the proposal was made; it is stale"
                           (abbreviate-file-name file))))
     ((and creating (not (seq-empty-p (plist-get proposal :references))))
      ;; a knowledge-base skill: SKILL.md plus references, one undoable group
      (let ((dir (file-name-directory file)) (ops '()) (id (pai-memory--new-id)))
        (pai-memory--write-file file text)
        (push (list :op "write" :file file :before "" :after text :created t) ops)
        (dolist (r (append (plist-get proposal :references) nil))
          (let ((f (expand-file-name (plist-get r :path) dir)))
            (pai-memory--write-file f (plist-get r :content))
            (push (list :op "write" :file f :before "" :after (plist-get r :content) :created t) ops)))
        (pai-memory--log (list :id id :time (format-time-string "%FT%T%z")
                               :action "skill-create" :target "skill" :file ""
                               :origin "proposal" :proposal_id (plist-get proposal :id)
                               :session (plist-get proposal :session)
                               :group (vconcat (nreverse ops))))
        (list :ok t :id id)))
     (t
      (pai-memory--write-file file text)
      (let ((id (pai-memory--new-id)))
        (pai-memory--log (list :id id :time (format-time-string "%FT%T%z")
                               :action (plist-get proposal :kind)
                               :target label :file (abbreviate-file-name file)
                               :created (and creating t)
                               :content "" :old "" :origin "proposal"
                               :proposal_id (plist-get proposal :id)
                               :session (plist-get proposal :session)
                               :entry_text_hash (secure-hash 'sha256 text)
                               :before (if creating "" now) :after text))
        (list :ok t :id id))))))

(defvar pai-memory-proposal-accepted-functions nil
  "Abnormal hook run with each proposal plist after it was applied.")

(defun pai-memory-proposal-accept (id &optional content)
  "Apply pending proposal ID, with CONTENT replacing its text when given.
Return (:ok t :id CHANGE-ID) or (:error MESSAGE); a proposal whose target
moved on is marked stale."
  (let ((p (pai-memory-proposal-load id)))
    (cond
     ((null p) (list :error (format "No proposal %s" id)))
     ((not (equal (plist-get p :status) "pending"))
      (list :error (format "Proposal %s is %s" id (plist-get p :status))))
     ((not (member (plist-get p :kind) pai-memory-proposal-kinds))
      (list :error (format "Proposal kind %s needs a newer pai-memory" (plist-get p :kind))))
     (t
      (let* ((kind (plist-get p :kind))
             (result
              (cond
               ((equal kind "skill-merge") (pai-memory-apply-merge p content))
               ((equal kind "skill-archive") (pai-memory-apply-archive p))
               ((equal kind "topic-conflict") (pai-memory-apply-topic-conflict p content))
               ((equal kind "memory-confirm")
                (condition-case err
                    (progn (pai-memory-entry-confirm (intern (plist-get p :target)) (plist-get p :cwd)
                                                     (plist-get p :old) (plist-get p :session) id)
                           (list :ok t :id "confirmed"))
                  (user-error (list :error (concat "No entry contains that text; " (error-message-string err))))))
               ((string-prefix-p "team-memory-" kind)
                (pai-memory-apply-change
                 (list :action (substring kind 12) :target "team"
                       :content (or content (plist-get p :content)) :old (plist-get p :old)
                       :origin "proposal" :proposal-id id :session-id (plist-get p :session))
                 (list :cwd (plist-get p :cwd) :session nil)))
               ((string-prefix-p "snippet-" kind)
                (let ((text (if (not content) (plist-get p :after)
                              (condition-case err
                                  (if (equal kind "snippet-create")
                                      (pai-memory-normalize-snippet
                                       content (plist-get p :name) (plist-get p :session)
                                       (pai-memory--provenance-of content))
                                    (progn (pai-memory-normalize-snippet
                                            content (plist-get p :name) nil '(("origin" . "x")))
                                           content))
                                (user-error (list :bad (error-message-string err)))))))
                  (if (and (consp text) (eq (car text) :bad))
                      (list :error (cadr text))
                    (pai-memory--apply-skill p text))))
               ((string-prefix-p "skill-" kind)
                  (let ((text (if content
                                  (if (equal kind "skill-create")
                                      (condition-case err
                                          (pai-memory-normalize-skill
                                           content (plist-get p :name) (plist-get p :session)
                                           (pai-memory--provenance-of content))
                                        (user-error (list :bad (error-message-string err))))
                                    content)
                                (plist-get p :after))))
                    (if (and (consp text) (eq (car text) :bad))
                        (list :error (cadr text))
                      (pai-memory--apply-skill p text))))
               (t
                (pai-memory-apply-change
                 (list :action (substring kind 7) :target (plist-get p :target)
                       :content (or content (plist-get p :content)) :old (plist-get p :old)
                       :origin "proposal" :proposal-id id :session-id (plist-get p :session)
                       :expires (plist-get p :expires))
                 (list :cwd (plist-get p :cwd) :session nil))))))
        (if (plist-get result :ok)
            (progn
              (pai-memory--set-status p "accepted" :applied (plist-get result :id)
                                      :edited (and content t))
              (run-hook-with-args 'pai-memory-proposal-accepted-functions p))
          (when (string-match-p "stale\\|changed since\\|No entry contains\\|Already remembered"
                                (plist-get result :error))
            (pai-memory--set-status p "stale" :reason (plist-get result :error))))
        result)))))

(defun pai-memory-proposal-reject (id &optional reason)
  "Reject pending proposal ID, recording REASON for later promoter runs."
  (let ((p (pai-memory-proposal-load id)))
    (if (and p (equal (plist-get p :status) "pending"))
        (progn (pai-memory--set-status p "rejected" :reason (or reason "")) t)
      nil)))

(defun pai-memory-auto-apply (proposals policy)
  "Apply PROPOSALS that review POLICY lets through; return how many applied.
`all' applies nothing; `skills' applies memory proposals; `none' applies
everything.  Proposals with risk findings always wait for review."
  (let ((n 0))
    (dolist (p proposals n)
      (when (and (seq-empty-p (plist-get p :risk))
                 ;; merges and archives change several skills, team memory is
                 ;; a repository file: always reviewed
                 (not (member (plist-get p :kind) '("skill-merge" "skill-archive" "topic-conflict")))
                 (not (string-prefix-p "team-" (plist-get p :kind)))
                 (or (equal (plist-get p :kind) "memory-confirm") ; metadata only
                     (pcase policy
                       ('none t)
                       ('skills (string-prefix-p "memory-" (plist-get p :kind)))
                       (_ nil)))
                 (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok))
        (cl-incf n)))))

(provide 'pai-memory-proposals)
;;; pai-memory-proposals.el ends here
