;;; pai-memory-snippets.el --- Learn prompt snippets like skills -*- lexical-binding: t; -*-

;;; Commentary:

;; The promoter learns prompt snippets (see the pai-prompt-snippets
;; extension) the way it learns skills.  A skill is a procedure for a kind
;; of task; a snippet is one short, standalone instruction the user adds to
;; SOME messages -- "keep it short", "ask before editing", "show the plan
;; first" -- and toggles per message with `/snippets'.
;;
;; The signal is the observer's `instruction:' observation: the user
;; attached such a reusable instruction to a request.  Like skills, a new
;; snippet must quote one of them in its evidence (checked in code), or come
;; from an explicit `/learn'.  An instruction the user wants ALWAYS is a
;; preference for user memory instead, not a snippet.
;;
;; Proposal kinds (see `pai-memory-proposals'):
;;   snippet-create  NAME (kebab-case file name), SCOPE, CONTENT = the snippet
;;                   file (front-matter: name, description, placement, order)
;;   snippet-patch   TARGET = path of an existing snippet, CONTENT = new text
;; Learned snippets go to ~/.pai/snippets/NAME.md, or the project's
;; .pai/snippets/NAME.md for a trusted project and scope "project".  They
;; carry origin: learned, created and source-session in their front-matter.
;;
;; Everything here works without pai-prompt-snippets loaded, but snippets
;; are only proposed when it is (they would do nothing otherwise).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-skills)
(require 'pai-trust)
(require 'pai-session)
(require 'pai-memory-settings)
(require 'pai-memory-store)
(require 'pai-memory-quality)

(declare-function pai-prompt-snippets-list "pai-prompt-snippets" (&optional cwd))
(declare-function pai-prompt-snippets-directories-for "pai-prompt-snippets" (&optional cwd))
(declare-function pai-memory-signals "pai-memory-promote" (branch &optional limit kinds))
(declare-function pai-memory-observation-line "pai-memory-compact" (o))

(defconst pai-memory-snippet-default-max-chars 1200
  "Default for `:max-snippet-chars': the largest snippet body.")

(defun pai-memory-snippets-available-p ()
  "Return non-nil when the prompt-snippets extension is loaded."
  (fboundp 'pai-prompt-snippets-list))

(defun pai-memory-snippets-enabled-p (&optional session)
  "Return non-nil when SESSION may learn snippets (`:learn-snippets')."
  (and (pai-memory-snippets-available-p)
       (pai-truthy (pai-memory-get :long-term :learn-snippets session))))

;;;; Places

(defun pai-memory-learned-snippets-dir (scope cwd)
  "Return the directory learned snippets of SCOPE go to for project CWD.
A `project' scope is honoured only in projects the user trusts."
  (file-name-as-directory
   (if (and (equal (format "%s" scope) "project") (eq (pai-trust-get cwd) 'yes))
       (expand-file-name ".pai/snippets" cwd)
     (expand-file-name "snippets" pai-directory))))

(defun pai-memory-snippet-file (name scope cwd)
  "Return the file of learned snippet NAME in SCOPE for project CWD."
  (expand-file-name (concat name ".md") (pai-memory-learned-snippets-dir scope cwd)))

(defun pai-memory-snippets (cwd)
  "Return the snippets visible in project CWD (plists), or nil."
  (and (pai-memory-snippets-available-p)
       (ignore-errors (pai-prompt-snippets-list cwd))))

(defun pai-memory-snippet-paths (cwd)
  "Return the files of the snippets visible in project CWD."
  (delq nil (mapcar (lambda (s) (plist-get s :path)) (pai-memory-snippets cwd))))

(defun pai-memory-snippet-dirs (cwd)
  "Return the directories snippets are read from in project CWD."
  (delete-dups
   (append (and (pai-memory-snippets-available-p)
                (ignore-errors (pai-prompt-snippets-directories-for cwd)))
           (list (pai-memory-learned-snippets-dir "global" cwd)
                 (file-name-as-directory (expand-file-name ".pai/snippets" cwd))))))

;;;; Text

(defun pai-memory-normalize-snippet (text name session-id &optional provenance)
  "Return snippet TEXT with learned front-matter; NAME is its file name.
Keeps the display name (default: NAME), description, placement and order;
records origin, created and source-session unless PROVENANCE (an alist)
gives them.  Signals a `user-error' when the snippet is not usable."
  (let* ((parsed (pai-skills--parse-frontmatter (or text "")))
         (fields (car parsed))
         (body (string-trim (cdr parsed)))
         (field (lambda (k) (let ((v (cdr (assoc k fields))))
                              (and v (not (string-empty-p v)) v))))
         (desc (funcall field "description"))
         (placement (or (funcall field "placement") "append"))
         (order (funcall field "order"))
         (max (or (pai-memory-get :long-term :max-snippet-chars)
                  pai-memory-snippet-default-max-chars)))
    (unless fields
      (user-error "A snippet needs front-matter (name, description, placement)"))
    (unless desc
      (user-error "A snippet needs a front-matter description (what it asks for)"))
    (unless (member placement '("prepend" "append"))
      (user-error "placement must be prepend or append"))
    (when (and order (not (string-match-p "\\`[+-]?[0-9]+\\'" order)))
      (user-error "order must be a whole number"))
    (when (string-empty-p body) (user-error "The snippet has no instruction text"))
    (when (> (length body) max)
      (user-error "A snippet is one short instruction: %d characters, over the %d limit (write a skill for a procedure)"
                  (length body) max))
    (when (string-match-p "</?prompt-snippet" body)
      (user-error "The snippet text may not contain <prompt-snippet> tags"))
    (concat "---\n"
            (format "name: %s\n" (or (funcall field "name")
                                     (capitalize (replace-regexp-in-string "-" " " name))))
            (format "description: %s\n" desc)
            (format "placement: %s\n" placement)
            (if order (format "order: %s\n" order) "")
            (if provenance
                (mapconcat (lambda (f) (format "%s: %s\n" (car f) (cdr f))) provenance "")
              (concat "origin: learned\n"
                      (format "created: %s\n" (format-time-string "%Y-%m-%d"))
                      (format "source-session: %s\n" (or session-id "unknown"))))
            "---\n" body "\n")))

(defun pai-memory-snippet-checks (text cwd)
  "Return the proposal check fields for snippet TEXT in project CWD."
  (let ((r (pai-memory-security-scan text cwd)))
    (list :risk (vconcat (append (plist-get r :warn) (plist-get r :block)))
          :block (vconcat (plist-get r :block))
          :lint [])))

;;;; Promoter input

(defun pai-memory-snippet-index (cwd)
  "Return the snippet index text for project CWD."
  (let ((snippets (pai-memory-snippets cwd)))
    (if (null snippets)
        "(no snippets yet)"
      (mapconcat (lambda (s)
                   (format "- %s%s (%s, %s): %s\n  path: %s"
                           (plist-get s :name)
                           (if (plist-get s :origin) (format " [%s]" (plist-get s :origin)) "")
                           (plist-get s :id) (plist-get s :placement)
                           (plist-get s :description)
                           (abbreviate-file-name (or (plist-get s :path) "?"))))
                 snippets "\n"))))

(defun pai-memory-instructions (session)
  "Return SESSION's `instruction:' observations, newest first."
  (pai-memory-signals (pai-session-get-branch session) 20 '("instruction")))

(defun pai-memory-snippet-creation-basis (session learn sources)
  "Return why snippets may be created in this promotion of SESSION.
`disabled' when snippets are off or the extension is not loaded;
`requested' for an explicit /learn (LEARN or SOURCES); a plist
\(:instructions OBS) of the user's per-message instructions; else nil."
  (cond
   ((not (pai-memory-snippets-enabled-p session)) 'disabled)
   ((or learn sources) 'requested)
   (t (let ((obs (pai-memory-instructions session)))
        (and obs (list :instructions obs))))))

(defun pai-memory-snippet-creation-text (basis cwd)
  "Return the promoter prompt sections on snippets for BASIS in project CWD."
  (if (eq basis 'disabled)
      nil
    (concat
     "## Snippet creation\n"
     (pcase basis
       ('nil "NOT available in this run: the user attached no reusable instruction to a message. Do not propose snippet-create (it will be refused); snippet-patch is still possible.")
       ('requested "Available only when the user's /learn request asks for a snippet (a short per-message instruction), not a skill.")
       (_ (concat "Available, but only for these instructions the user attached to their messages; a new snippet must quote one of them in evidence. Prefer an instruction that recurs (listed more than once, or phrased as something they often want).\n"
                  (mapconcat #'pai-memory-observation-line (plist-get basis :instructions) "\n"))))
     "\n\n## Existing prompt snippets\n"
     (pai-memory-snippet-index cwd))))

(provide 'pai-memory-snippets)
;;; pai-memory-snippets.el ends here
