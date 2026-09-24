;;; pai-memory-quality.el --- Skill quality gates: lint and security scan -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 C2.  Every skill that enters through the learning loop -- created,
;; patched, and later imported or merged -- is checked twice:
;;
;;   Lint (advisory): front-matter, a description that says when to use the
;;   skill, a "when to use" part and steps, size (long material belongs in
;;   references/), no hard-coded home paths.  Findings are shown in review
;;   and returned to the promoter so it can file a better version.
;;
;;   Security scan: patterns that make a skill dangerous to follow.
;;     warn   -- shell/elisp blocks, sudo, rm -rf, secret names, paths outside
;;               home and project, mentions of agent configuration;
;;     block  -- download-and-run, destructive deletes of / or ~, writes to
;;               agent configuration, encoded payloads, instruction overrides.
;;   A warning makes review ask before accepting.  A blocking finding also
;;   keeps the proposal out of "accept all" and every review policy, and
;;   needs an explicit confirmation naming the finding.
;;
;; `/memory lint [NAME]' runs both checks over existing skills.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-skills)
(require 'pai-memory-settings)

;;;; Security scan

(defconst pai-memory-warn-patterns
  '(("fenced shell/elisp block" . "^```[ \t]*\\(sh\\|bash\\|zsh\\|shell\\|console\\|elisp\\|emacs-lisp\\|lisp\\)\\b")
    ("rm -rf" . "\\brm[ \t]+-[a-zA-Z]*r[a-zA-Z]*f\\|\\brm[ \t]+-[a-zA-Z]*f[a-zA-Z]*r")
    ("sudo" . "\\bsudo\\b")
    ("credential or secret name" . "\\b[A-Z0-9_]*\\(API_KEY\\|TOKEN\\|SECRET\\|PASSWORD\\|PASSWD\\)[A-Z0-9_]*\\b")
    ("mentions agent configuration" . "\\(~/\\.pai\\b\\|\\.emacs\\.d\\|\\binit\\.el\\b\\)"))
  "Alist of (LABEL . REGEXP): skill text that deserves a careful look.")

(defconst pai-memory-block-patterns
  '(("download piped to a shell"
     . "\\b\\(curl\\|wget\\)\\b[^|\n]*|[ \t]*\\(sudo[ \t]+\\)?\\(ba\\|z\\|da\\)?sh\\b\\|\\b\\(ba\\|z\\)?sh[ \t]+<([ \t]*\\(curl\\|wget\\)\\b")
    ("destructive delete of / or home"
     . "\\brm[ \t]+-[a-zA-Z]*[rf][a-zA-Z]*[ \t]+\\(--no-preserve-root[ \t]+\\)?\\(/\\|~/?\\|\\$HOME/?\\)\\([ \t\n*]\\|\\'\\)")
    ("writes agent configuration"
     . "\\(>>?\\|\\btee\\b\\|\\bcp\\b\\|\\bmv\\b\\|\\bln\\b\\|sed[ \t]+-i\\|write-region\\|with-temp-file\\|append-to-file\\)[^\n]*\\(~/\\.pai/\\|\\.emacs\\.d/\\|\\binit\\.el\\b\\|\\.pai/\\(settings\\|trust\\|auth\\)\\)")
    ("encoded payload"
     . "base64[ \t]+\\(-d\\|--decode\\)[^\n]*|[ \t]*\\(ba\\|z\\)?sh\\b\\|\\(eval\\|exec\\)[^\n]*base64\\|[A-Za-z0-9+/]\\{200,\\}=\\{0,2\\}")
    ("instruction override"
     . "\\(ignore\\|disregard\\|forget\\)[ \t]+\\(all[ \t]+\\)?\\(the[ \t]+\\)?\\(previous\\|prior\\|above\\|earlier\\|system\\)[ \t]+\\(instructions\\|prompt\\|rules\\)\\|\\bdo not tell the user\\b\\|\\bwithout \\(asking\\|telling\\) the user\\b\\|\\byou are now\\b"))
  "Alist of (LABEL . REGEXP): skill text that must never be accepted in bulk.")

(defun pai-memory--outside-paths-p (text cwd)
  "Return non-nil when TEXT names absolute paths outside home, the project and /tmp."
  (let ((home (expand-file-name "~/"))
        (proj (and cwd (file-name-as-directory (expand-file-name cwd))))
        (start 0) (found nil))
    (while (and (not found)
                (string-match "\\(?:^\\|[ \t`\"'(]\\)\\(/[A-Za-z0-9._/-]+\\)" text start))
      (let ((path (match-string 1 text)))
        (setq start (match-end 0))
        (unless (or (string-prefix-p home path) (and proj (string-prefix-p proj path))
                    (string-prefix-p "/tmp" path)
                    (not (string-match-p "\\`/[A-Za-z]" path)))
          (setq found t))))
    found))

(defun pai-memory-security-scan (text &optional cwd)
  "Return (:warn LABELS :block LABELS) for skill TEXT; CWD is the project."
  (let ((case-fold-search nil) (text (or text "")) (warn '()) (block '()))
    (dolist (p pai-memory-warn-patterns)
      (when (string-match-p (cdr p) text) (push (car p) warn)))
    (when (pai-memory--outside-paths-p text cwd)
      (push "path outside home and project" warn))
    (let ((case-fold-search t))
      (dolist (p pai-memory-block-patterns)
        (when (string-match-p (cdr p) text) (push (car p) block))))
    (list :warn (nreverse warn) :block (nreverse block))))

(defun pai-memory-risk-scan (text &optional cwd)
  "Return every security label found in TEXT, warnings then blocking ones."
  (let ((r (pai-memory-security-scan text cwd)))
    (append (plist-get r :warn) (plist-get r :block))))

;;;; Lint

(defun pai-memory-skill-lint (text)
  "Return advisory findings for skill TEXT (a list of strings)."
  (let* ((parsed (pai-skills--parse-frontmatter (or text "")))
         (fields (car parsed))
         (body (string-trim (cdr parsed)))
         (desc (cdr (assoc "description" fields)))
         (limit (or (pai-memory-get :long-term :max-skill-chars) 8000))
         (home (expand-file-name "~/"))
         (case-fold-search t)
         (out '()))
    (cond
     ((null fields) (push "no front-matter: start with --- name/description ---" out))
     ((or (null desc) (string-empty-p desc)) (push "no description" out))
     (t
      (when (> (length desc) 1024) (push "description over 1024 characters" out))
      (when (< (length desc) 20) (push "description too short to say when to use the skill" out))
      (unless (string-match-p "\\b\\(use\\|when\\|whenever\\|for\\|if\\|before\\|after\\)\\b" desc)
        (push "description does not say when to use the skill (e.g. \"Use when ...\")" out))))
    (when (string-empty-p body) (push "empty body" out))
    (unless (or (string-match-p "^#+[ \t]*\\(when\\|use\\b\\|usage\\)" body)
                (and desc (string-match-p "\\bwhen\\b" desc)))
      (push "no \"When to use\" section" out))
    (unless (or (string-match-p "^#+[ \t]*\\(steps\\|procedure\\|how\\|instructions\\|process\\)" body)
                (string-match-p "^[ \t]*1[.)][ \t]" body))
      (push "no steps (a \"Steps\" heading or a numbered list)" out))
    (when (> (length body) limit)
      (push (format "body is %d characters, over %d: move detail to references/" (length body) limit) out))
    (let ((name (cdr (assoc "name" fields))))
      (when (and name (string-match-p
                       "\\`\\(?:fix\\|debug\\|audit\\|implement\\|add\\|todo\\|task\\|issue\\|pr\\)-\\|[0-9]\\{3,\\}"
                       name))
        (push (format "name %S looks like one task (fix-/implement-/ticket number), not a class of task" name) out)))
    (let ((refs 0) (pos 0) (prose (replace-regexp-in-string "```\\(?:.\\|\n\\)*?```" "" body)))
      (while (string-match "#[0-9]\\{3,6\\}\\b\\|\\b\\(?:PR\\|issue\\) ?#?[0-9]\\{2,6\\}\\b\\|\\b20[0-9][0-9]-[01][0-9]-[0-3][0-9]\\b"
                           prose pos)
        (setq refs (1+ refs) pos (match-end 0)))
      (when (>= refs 3)
        (push (format "%d ticket numbers/dates: state the rule, not the incident" refs) out))
      (when (string-match "\\b\\(this session\\|this conversation\\|we \\(?:added\\|implemented\\|fixed\\|changed\\)\\|was \\(?:implemented\\|added\\|fixed\\)\\|the user asked\\)\\b"
                          prose)
        (push (format "narrates history (%S): a skill states how to work, not what happened"
                      (match-string 1 prose))
              out)))
    (when (string-match (concat "\\(" (regexp-quote home) "[^ \t\n`\"')]*\\)") (or text ""))
      (push (format "hard-coded home path %s: use ~/ or a path relative to the project"
                    (match-string 1 text))
            out))
    (nreverse out)))

(defun pai-memory-skill-check (text &optional cwd)
  "Return (:lint FINDINGS :warn LABELS :block LABELS) for skill TEXT."
  (append (list :lint (pai-memory-skill-lint text)) (pai-memory-security-scan text cwd)))

(defun pai-memory-lint-report (skills &optional cwd)
  "Return the `/memory lint' report for SKILLS."
  (if (null skills)
      "No skills to check"
    (mapconcat
     (lambda (s)
       (let* ((r (pai-memory-skill-check (with-temp-buffer (insert-file-contents (plist-get s :path))
                                                           (buffer-string))
                                         cwd))
              (lines (append (mapcar (lambda (b) (concat "  ⛔ " b)) (plist-get r :block))
                             (mapcar (lambda (w) (concat "  ⚠ " w)) (plist-get r :warn))
                             (mapcar (lambda (l) (concat "  · " l)) (plist-get r :lint)))))
         (format "%s (%s)%s" (plist-get s :name) (abbreviate-file-name (plist-get s :path))
                 (if lines (concat "\n" (string-join lines "\n")) ": ok"))))
     skills "\n")))

(provide 'pai-memory-quality)
;;; pai-memory-quality.el ends here
