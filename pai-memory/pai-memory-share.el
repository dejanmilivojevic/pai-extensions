;;; pai-memory-share.el --- Export and import skills -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 C5.
;;
;;   /skills-export NAME...|--learned|--all DEST
;;       copies each skill's directory (SKILL.md, references/, other files)
;;       to DEST -- a directory, or a .tar.gz -- with a pai-skills.json
;;       manifest of names and content hashes.
;;
;;   /skills-import SOURCE
;;       SOURCE is a directory, a .tar.gz/.tgz, or a git URL (cloned with
;;       --depth 1).  Every skill found becomes a PROPOSAL, checked by the C2
;;       gates -- imported text is someone else's instructions, so nothing is
;;       installed without review:
;;         new skill        skill-create into ~/.pai/skills/imported/, with
;;                          `origin: imported', `imported-from' and its
;;                          references/*.md files;
;;         upstream update  skill-patch of the installed copy; the rationale
;;                          says whether you edited your copy since (accepting
;;                          then replaces your edits);
;;         unchanged        skipped;
;;         name taken by a skill that was not imported: skipped.
;;       Only Markdown is imported: scripts and other files are listed and
;;       left out.  ~/.pai/memory/imported-skills.json records, per skill, the
;;       source, the upstream hash and the hash of what was installed.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-skills)
(require 'pai-memory-settings)
(require 'pai-memory-store)
(require 'pai-memory-proposals)

(declare-function pai-memory--skill-dirs "pai-memory-promote" ())

;;;; Helpers

(defun pai-memory--skill-root (skill)
  "Return SKILL's own directory, or nil for a loose NAME.md file."
  (let ((file (plist-get skill :path)))
    (and (equal (file-name-nondirectory file) "SKILL.md")
         (file-name-directory file))))

(defun pai-memory--hash (text) (secure-hash 'sha256 text))

(defun pai-memory--run (program &rest args)
  "Run PROGRAM with ARGS; return its output, or signal a `user-error'."
  (unless (executable-find program) (user-error "%s is not installed" program))
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil args)))
      (unless (eq status 0)
        (user-error "%s %s failed: %s" program (car args) (string-trim (buffer-string))))
      (buffer-string))))

(defun pai-memory-imported-skills-dir ()
  "Return where imported skills are installed."
  (expand-file-name "skills/imported" pai-directory))

;;;; Export

(defun pai-memory-export-skills (names dest &optional dirs)
  "Export skills NAMES (or `learned' / `all') found in DIRS to DEST.
DEST is a directory or a .tar.gz file.  Return a message."
  (let* ((all (pai-discover-skills (or dirs (pai-memory--skill-dirs))))
         (skills (pcase names
                   ('all all)
                   ('learned (seq-filter (lambda (s) (equal (cdr (assoc "origin" (car (pai-skills--parse-frontmatter
                                                                                     (pai-memory--read-file (plist-get s :path))))))
                                                            "learned"))
                                         all))
                   (_ (mapcar (lambda (n) (or (seq-find (lambda (s) (equal (plist-get s :name) n)) all)
                                              (user-error "No skill named %s" n)))
                              names))))
         (tarball (string-match-p "\\.t\\(ar\\.\\)?gz\\'" dest))
         (out (if tarball (make-temp-file "pai-export" t) (expand-file-name dest)))
         (manifest '()))
    (unless skills (user-error "No skills to export"))
    (make-directory out t)
    (dolist (s skills)
      (let ((root (pai-memory--skill-root s))
            (target (expand-file-name (plist-get s :name) out)))
        (when (file-exists-p target) (delete-directory target t))
        (if root
            (copy-directory root target nil t t)
          (make-directory target t)
          (copy-file (plist-get s :path) (expand-file-name "SKILL.md" target) t))
        (push (list :name (plist-get s :name)
                    :hash (pai-memory--hash (pai-memory--read-file (expand-file-name "SKILL.md" target))))
              manifest)))
    (pai-memory--write-file (expand-file-name "pai-skills.json" out)
                            (pai-json-encode (list :format 1 :exported (format-time-string "%FT%T%z")
                                                   :skills (vconcat (nreverse manifest)))))
    (when tarball
      (pai-memory--run "tar" "-czf" (expand-file-name dest) "-C" out ".")
      (delete-directory out t))
    (format "Exported %d skill(s) to %s" (length skills) (abbreviate-file-name (expand-file-name dest)))))

;;;; Import

(defun pai-memory-imported-read ()
  "Return the import manifest: a plist keyed by :SKILL-NAME."
  (let ((f (pai-memory-dir "imported-skills.json")))
    (or (and (file-readable-p f) (ignore-errors (pai-json-decode (pai-memory--read-file f)))) '())))

(defun pai-memory--imported-set (name record)
  "Record import manifest RECORD for skill NAME."
  (pai-memory--write-file (pai-memory-dir "imported-skills.json")
                          (pai-json-encode (plist-put (pai-memory-imported-read)
                                                      (intern (concat ":" name)) record))))

(defun pai-memory--import-fetch (source)
  "Return (DIR . CLEANUP) holding SOURCE's files; CLEANUP is a temp dir or nil."
  (cond
   ((string-match-p "\\`\\(https?://.*\\.git\\|git@\\|https?://\\(github\\|gitlab\\|codeberg\\)\\.\\)" source)
    (let ((tmp (make-temp-file "pai-import" t)))
      (pai-memory--run "git" "clone" "--depth" "1" "--quiet" source tmp)
      (cons tmp tmp)))
   ((string-match-p "\\.t\\(ar\\.\\)?gz\\'" source)
    (let ((file (expand-file-name source)) (tmp (make-temp-file "pai-import" t)))
      (unless (file-readable-p file) (user-error "No file %s" source))
      (pai-memory--run "tar" "-xzf" file "-C" tmp)
      (cons tmp tmp)))
   ((file-directory-p (expand-file-name source)) (cons (expand-file-name source) nil))
   (t (user-error "Unknown import source %s: a directory, a .tar.gz or a git URL" source))))

(defun pai-memory--import-files (skill)
  "Return (REFERENCES . SKIPPED) for SKILL's directory: references/*.md, other files."
  (let ((root (pai-memory--skill-root skill)) (refs '()) (skipped '()))
    (when root
      (dolist (f (directory-files-recursively root "" nil
                                              (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d))))))
        (let ((rel (file-relative-name f root)))
          (cond ((equal rel "SKILL.md") nil)
                ((string-match-p "\\`references/[A-Za-z0-9._/-]+\\.md\\'" rel)
                 (push (list :path rel :content (pai-memory--read-file f)) refs))
                (t (push rel skipped))))))
    (cons (nreverse refs) (nreverse skipped))))

(defun pai-memory-import-skills (source &optional cwd)
  "Import skills from SOURCE as proposals; return a report."
  (let* ((fetched (pai-memory--import-fetch source))
         (cwd (or cwd default-directory))
         (lines '()) (filed 0))
    (unwind-protect
        (let ((found (pai-discover-skills (list (car fetched))))
              (manifest (pai-memory-imported-read))
              (local (pai-discover-skills (pai-memory--skill-dirs))))
          (unless found (user-error "No skills found in %s" source))
          (dolist (s found)
            (let* ((name (plist-get s :name))
                   (text (pai-memory--read-file (plist-get s :path)))
                   (files (pai-memory--import-files s))
                   (upstream (pai-memory--hash (concat text (mapconcat (lambda (r) (plist-get r :content))
                                                                       (car files) ""))))
                   (rec (plist-get manifest (intern (concat ":" name))))
                   (mine (seq-find (lambda (l) (equal (plist-get l :name) name)) local))
                   (provenance (list (cons "origin" "imported") (cons "imported-from" source)
                                     (cons "imported" (format-time-string "%Y-%m-%d"))))
                   (skipped (if (cdr files) (format " (left out: %s)" (string-join (cdr files) ", ")) "")))
              (condition-case err
                  (cond
                   ((and mine (not rec))
                    (push (format "%s: skipped, a skill with that name exists and was not imported" name) lines))
                   ((and rec (equal upstream (plist-get rec :upstream)))
                    (push (format "%s: up to date" name) lines))
                   ((and rec mine)
                    (let* ((current (pai-memory--read-file (plist-get mine :path)))
                           (edited (not (equal (pai-memory--hash current) (plist-get rec :installed))))
                           (p (pai-memory-add-proposal
                               (pai-memory-make-proposal
                                :kind "skill-patch" :target (plist-get mine :path) :cwd cwd
                                :content (pai-memory-normalize-skill text name nil provenance)
                                :rationale (if edited
                                               (format "Upstream update from %s. You edited your copy since it was installed: accepting replaces your edits." source)
                                             (format "Upstream update from %s; your copy is unchanged." source))
                                :evidence (list source)
                                :skill-dirs (pai-memory--skill-dirs)
                                :extra (list :import (list :name name :source source :upstream upstream))))))
                      (ignore p) (cl-incf filed)
                      (push (format "%s: update proposed%s%s" name (if edited " (you edited your copy)" "") skipped)
                            lines)))
                   (t
                    (pai-memory-add-proposal
                     (pai-memory-make-proposal
                      :kind "skill-create" :name name :cwd cwd :content text
                      :rationale (format "Imported from %s." source) :evidence (list source)
                      :provenance provenance :skill-dir (pai-memory-imported-skills-dir)
                      :references (vconcat (car files))
                      :skill-dirs (pai-memory--skill-dirs)
                      :extra (list :import (list :name name :source source :upstream upstream))))
                    (cl-incf filed)
                    (push (format "%s: new skill proposed%s" name skipped) lines)))
                (user-error (push (format "%s: not imported: %s" name (error-message-string err)) lines)))))
          (concat (format "Import from %s: %d proposal(s) to review with /memory-review\n" source filed)
                  (mapconcat (lambda (l) (concat "  " l)) (nreverse lines) "\n")))
      (when (cdr fetched) (delete-directory (cdr fetched) t)))))

(defun pai-memory--record-import (p)
  "After an import proposal P is accepted, record it in the manifest."
  (let ((imp (plist-get p :import)))
    (when imp
      (pai-memory--imported-set
       (plist-get imp :name)
       (list :source (plist-get imp :source) :upstream (plist-get imp :upstream)
             :installed (pai-memory--hash (pai-memory--read-file (plist-get p :target)))
             :path (plist-get p :target) :time (format-time-string "%FT%T%z"))))))

(add-hook 'pai-memory-proposal-accepted-functions #'pai-memory--record-import)

;;;; Project skills in git (C6)

(defun pai-memory-git-root (file)
  "Return the git work tree containing FILE's directory, or nil."
  (let ((dir (file-name-directory file)))
    (while (and dir (not (file-directory-p dir)))
      (setq dir (file-name-directory (directory-file-name dir))))
    (and dir (executable-find "git")
         (with-temp-buffer
           (and (eq 0 (call-process "git" nil t nil "-C" dir "rev-parse" "--show-toplevel"))
                (file-name-as-directory (string-trim (buffer-string))))))))

(defun pai-memory-team-proposal-p (p)
  "Return non-nil when proposal P edits the repository's team memory (G2)."
  (string-prefix-p "team-memory-" (or (plist-get p :kind) "")))

(defun pai-memory--proposal-file (p)
  "Return the file proposal P writes."
  (if (pai-memory-team-proposal-p p)
      (pai-memory-target-file 'team (plist-get p :cwd))
    (plist-get p :target)))

(defun pai-memory-project-skill-in-git-p (p)
  "Return the git root when P writes a project skill or team memory in a git repo."
  (and (plist-get p :cwd)
       (or (pai-memory-team-proposal-p p)
           (and (member (plist-get p :kind) '("skill-create" "skill-patch"))
                (string-prefix-p (expand-file-name ".pai/skills/" (plist-get p :cwd))
                                 (expand-file-name (plist-get p :target)))))
       (pai-memory-git-root (pai-memory--proposal-file p))))

(defun pai-memory-for-teammates (text)
  "Return skill TEXT with session-specific front-matter replaced for a repo."
  (replace-regexp-in-string
   "^source-session:.*$"
   (format "learned-by: pai\nlearned: %s" (format-time-string "%Y-%m-%d"))
   text t t))

(defun pai-memory-git-add (p)
  "Stage what proposal P wrote; return a message.  Never commits."
  (let* ((file (pai-memory--proposal-file p))
         (root (pai-memory-git-root file))
         (what (if (pai-memory-team-proposal-p p) file (file-name-directory file))))
    (pai-memory--run "git" "-C" root "add" "--" (file-relative-name what root))
    (format "staged %s (not committed)" (file-relative-name what root))))

(provide 'pai-memory-share)
;;; pai-memory-share.el ends here
