;;; pai-memory-review.el --- Review buffer for memory proposals -*- lexical-binding: t; -*-

;;; Commentary:

;; `/memory-review' (SPEC §7.3) lists the pending proposals, one line each:
;;
;;   RET  show or hide the details: rationale, evidence, risk banner, diff
;;   a    accept (risky proposals ask first)
;;   r    reject, with an optional reason the promoter sees next time
;;   e    edit the proposed text, then C-c C-c accepts it (C-c C-k cancels)
;;   m    move a proposed entry to another memory (user, memory, project,
;;        team) before accepting it
;;   G    accept a project skill and `git add' it (never commits)
;;   A    accept every unflagged pending proposal of the same kind
;;        (⛔ blocking findings are never accepted in bulk)
;;   n/p  next/previous proposal     g  refresh     q  quit
;;
;; Accepted changes go through the change log, so `/memory undo' reverts them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-diff)
(require 'pai-memory-proposals)
(require 'pai-memory-quality)

(defvar-local pai-memory-review--expanded nil
  "Ids of proposals whose details are shown.")

(defface pai-memory-review-risk '((t :inherit error :weight bold))
  "Face of the risk banner in the review buffer.")

(defface pai-memory-review-kind '((t :inherit font-lock-keyword-face))
  "Face of a proposal's kind in the review buffer.")

(defvar pai-memory-review-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'pai-memory-review-toggle)
    (define-key m (kbd "TAB") #'pai-memory-review-toggle)
    (define-key m "a" #'pai-memory-review-accept)
    (define-key m "r" #'pai-memory-review-reject)
    (define-key m "e" #'pai-memory-review-edit)
    (define-key m "m" #'pai-memory-review-move)
    (define-key m "A" #'pai-memory-review-accept-kind)
    (define-key m "G" #'pai-memory-review-accept-git)
    (define-key m "t" #'pai-memory-review-test)
    (define-key m "n" #'pai-memory-review-next)
    (define-key m "p" #'pai-memory-review-previous)
    (define-key m "g" #'pai-memory-review-refresh)
    m)
  "Keymap of `pai-memory-review-mode'.")

(define-derived-mode pai-memory-review-mode special-mode "pai-review"
  "Review pending pai-memory proposals.
\\{pai-memory-review-mode-map}"
  (setq truncate-lines nil))

;;;; Rendering

(defun pai-memory-review--label (p)
  "Return a one-line label for proposal P."
  (let ((kind (plist-get p :kind)))
    (pcase kind
      ("skill-create" (format "new %s skill %s%s" (plist-get p :scope) (plist-get p :name)
                              (let ((n (length (plist-get p :references))))
                                (if (> n 0) (format " (+%d reference file%s)" n (if (= n 1) "" "s")) ""))))
      ("skill-patch" (format "patch skill %s" (plist-get p :name)))
      ("snippet-create" (format "new %s prompt snippet %s" (plist-get p :scope) (plist-get p :name)))
      ("snippet-patch" (format "patch prompt snippet %s" (plist-get p :name)))
      ("skill-merge" (format "merge %s → %s"
                             (mapconcat (lambda (s) (plist-get s :name)) (plist-get p :sources) ", ")
                             (plist-get p :name)))
      ("skill-archive" (format "archive skill %s" (plist-get p :name)))
      ("topic-conflict" (format "topic conflict in %s: %s" (plist-get p :name)
                                (truncate-string-to-width (or (plist-get p :rationale) "") 60 nil nil "…")))
      ("memory-confirm" (format "confirm %s: %s" (plist-get p :target)
                                (truncate-string-to-width (replace-regexp-in-string "\n" " " (or (plist-get p :content) ""))
                                                          70 nil nil "…")))
      ((pred (string-prefix-p "team-memory-"))
       (format "team %s PROJECT.md: %s" (substring kind 12)
               (truncate-string-to-width
                (replace-regexp-in-string "\n" " " (if (equal kind "team-memory-remove")
                                                       (plist-get p :old) (or (plist-get p :content) "")))
                70 nil nil "…")))
      ((pred (string-prefix-p "memory-"))
       (format "%s %s%s: %s" (substring kind 7) (plist-get p :target)
               (let ((e (plist-get p :expires))) (if e (format " (until %s)" e) ""))
               (truncate-string-to-width
                (replace-regexp-in-string "\n" " " (if (equal kind "memory-remove")
                                                       (plist-get p :old)
                                                     (or (plist-get p :content) "")))
                70 nil nil "…")))
      (_ (format "%s (needs a newer pai-memory; read-only)" kind)))))

(defun pai-memory-review--diff (p)
  "Return the rendered diff for proposal P."
  (let* ((kind (plist-get p :kind))
         (name (if (or (string-prefix-p "memory-" kind) (string-prefix-p "team-memory-" kind))
                   (file-name-nondirectory (pai-memory-target-file (plist-get p :target)
                                                                   (plist-get p :cwd)))
                 (abbreviate-file-name (or (plist-get p :target) "?")))))
    (pai-diff-render (or (plist-get p :before) "") (or (plist-get p :after) "")
                     (concat "a/" name) (concat "b/" name))))

(defun pai-memory-review--details (p)
  "Return the detail text of proposal P."
  (let ((risk (append (plist-get p :risk) nil))
        (evidence (pai-memory--string-list (plist-get p :evidence))))
    (concat
     (when (pai-memory-review--blocked-p p)
       (propertize (format "    ⛔ Blocking finding: %s. This skill could harm your system or steer the agent; accept it only if you understand exactly why.\n"
                           (string-join (append (plist-get p :block) nil) ", "))
                   'face 'pai-memory-review-risk))
     (when risk
       (propertize (format "    ⚠ Review carefully: %s\n" (string-join risk ", "))
                   'face 'pai-memory-review-risk))
     (let ((lint (append (plist-get p :lint) nil)))
       (when lint
         (concat "    Style:\n"
                 (mapconcat (lambda (l) (concat "      · " l)) lint "\n") "\n")))
     (format "    Why: %s\n" (plist-get p :rationale))
     (when evidence
       (concat "    Evidence:\n"
               (mapconcat (lambda (e) (concat "      - " (replace-regexp-in-string "\n" " " e)))
                          evidence "\n")
               "\n"))
     (format "    From session %s in %s\n" (plist-get p :session)
             (abbreviate-file-name (or (plist-get p :cwd) "?")))
     (when (equal (plist-get p :kind) "skill-merge")
       (format "    Archives: %s (backed up first; /memory undo reverts the whole merge)\n"
               (mapconcat (lambda (s) (plist-get s :name))
                          (seq-remove (lambda (s) (equal (plist-get s :name) (plist-get p :name)))
                                      (plist-get p :sources))
                          ", ")))
     (let ((c (plist-get p :check)))
       (when (and c (not (plist-get p :evaluation)))
         (format "    Check (t to run): %s%s\n" (plist-get c :task)
                 (let ((cr (plist-get c :criteria)))
                   (if (and cr (not (string-empty-p cr))) (format " — success: %s" cr) "")))))
     (pai-memory-evaluation-text (plist-get p :evaluation))
     (replace-regexp-in-string "^" "    " (pai-memory-review--diff p))
     (let ((refs (append (plist-get p :references) nil)))
       (when refs
         (concat "\n    Reference files:\n"
                 (mapconcat (lambda (r)
                              (concat (format "    + %s (%d chars)\n" (plist-get r :path)
                                              (length (plist-get r :content)))
                                      (replace-regexp-in-string
                                       "^" "      "
                                       (pai-diff-render "" (plist-get r :content)
                                                        "/dev/null" (concat "b/" (plist-get r :path))))))
                            refs "\n"))))
     "\n")))

(defun pai-memory-review-refresh ()
  "Redraw the review buffer, keeping point on the same proposal."
  (interactive)
  (let ((inhibit-read-only t)
        (keep (pai-memory-review--id-at-point))
        (pending (pai-memory-proposals "pending")))
    (erase-buffer)
    (insert (propertize (format "Memory proposals: %d pending" (length pending)) 'face 'bold)
            "   RET details · a accept · r reject · e edit · m move to another memory · t test-run · G accept + git add · A accept all of kind · q quit\n\n")
    (if (null pending)
        (insert "Nothing to review.\n")
      (dolist (p pending)
        (let ((start (point)) (id (plist-get p :id)))
          (insert (if (member id pai-memory-review--expanded) "▾ " "▸ ")
                  (propertize (format "%-15s" (plist-get p :kind)) 'face 'pai-memory-review-kind)
                  (pai-memory-review--label p)
                  (cond ((pai-memory-review--blocked-p p)
                         (propertize "  ⛔" 'face 'pai-memory-review-risk))
                        ((seq-empty-p (plist-get p :risk)) "")
                        (t (propertize "  ⚠" 'face 'pai-memory-review-risk)))
                  (pcase (plist-get (plist-get p :evaluation) :status)
                    ("pass" "  ✓ tested") ("fail" (propertize "  ✗ test failed" 'face 'pai-memory-review-risk))
                    ("running" "  … testing") ('nil (if (plist-get p :check) "  (check available)" ""))
                    (_ "  ? test incomplete"))
                  "\n")
          (when (member id pai-memory-review--expanded)
            (insert (pai-memory-review--details p)))
          (put-text-property start (point) 'pai-memory-proposal id))))
    (goto-char (point-min))
    (if-let ((pos (and keep (text-property-any (point-min) (point-max) 'pai-memory-proposal keep))))
        (goto-char pos)
      (pai-memory-review-next))))

(defun pai-memory-review ()
  "Open the review buffer for pending memory proposals."
  (interactive)
  (let ((buf (get-buffer-create "*pai memory review*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'pai-memory-review-mode) (pai-memory-review-mode))
      (pai-memory-review-refresh))
    (if noninteractive buf (pop-to-buffer buf))))

;;;; Navigation

(defun pai-memory-review--id-at-point ()
  "Return the id of the proposal at point, or nil."
  (get-text-property (point) 'pai-memory-proposal))

(defun pai-memory-review--start (id)
  "Return the start of proposal ID's line."
  (text-property-any (point-min) (point-max) 'pai-memory-proposal id))

(defun pai-memory-review-next ()
  "Move to the next proposal."
  (interactive)
  (let* ((here (pai-memory-review--id-at-point))
         (pos (if here
                  (next-single-property-change (point) 'pai-memory-proposal)
                (point))))
    (while (and pos (< pos (point-max)) (not (get-text-property pos 'pai-memory-proposal)))
      (setq pos (next-single-property-change pos 'pai-memory-proposal)))
    (when (and pos (< pos (point-max))) (goto-char pos))))

(defun pai-memory-review-previous ()
  "Move to the previous proposal."
  (interactive)
  (let ((here (pai-memory-review--id-at-point)))
    (when here (goto-char (pai-memory-review--start here)))
    (let ((pos (previous-single-property-change (point) 'pai-memory-proposal)))
      (while (and pos (> pos (point-min)) (not (get-text-property (1- pos) 'pai-memory-proposal)))
        (setq pos (previous-single-property-change pos 'pai-memory-proposal)))
      (when (and pos (> pos (point-min)))
        (goto-char (pai-memory-review--start (get-text-property (1- pos) 'pai-memory-proposal)))))))

;;;; Actions

(declare-function pai-memory-evaluate "pai-memory-evaluate" (id &rest args))
(declare-function pai-memory-evaluation-text "pai-memory-evaluate" (ev))

(defun pai-memory-review-test (&optional empty)
  "Test-run the skill proposal at point in a scratch copy of the project.
With a prefix argument (EMPTY), in an empty directory instead."
  (interactive "P")
  (let* ((p (pai-memory-review--require))
         (check (plist-get p :check))
         (task (read-string "Task to test the skill with: " (plist-get check :task)))
         (criteria (read-string "Success criteria (optional): " (plist-get check :criteria))))
    (when (yes-or-no-p (format "Run skill %s in a scratch %s? Its bash commands are not sandboxed. "
                               (plist-get p :name) (if empty "empty directory" "copy of the project")))
      (pai-memory-evaluate (plist-get p :id) :task task :criteria criteria :empty empty)
      (message "Test-running %s in the background; the result shows here (g to refresh)" (plist-get p :name))
      (pai-memory-review-refresh))))

(defun pai-memory-review--require ()
  "Return the proposal at point, or signal."
  (let ((id (pai-memory-review--id-at-point)))
    (unless id (user-error "No proposal here"))
    (or (pai-memory-proposal-load id) (user-error "Proposal %s is gone" id))))

(defun pai-memory-review-toggle ()
  "Show or hide the details of the proposal at point."
  (interactive)
  (let ((id (plist-get (pai-memory-review--require) :id)))
    (setq pai-memory-review--expanded
          (if (member id pai-memory-review--expanded)
              (delete id pai-memory-review--expanded)
            (cons id pai-memory-review--expanded)))
    (pai-memory-review-refresh)))

(defun pai-memory-review--report (id result)
  "Echo the outcome RESULT of applying proposal ID."
  (message (if (plist-get result :ok)
               (format "Applied %s (undo with /memory undo)" id)
             (format "Not applied: %s" (plist-get result :error)))))

(defun pai-memory-review--blocked-p (p)
  "Return non-nil when proposal P has a blocking finding."
  (not (seq-empty-p (plist-get p :block))))

(defun pai-memory-review--confirm (p &optional content)
  "Ask before applying proposal P (with edited CONTENT); return non-nil to go on.
Edited skill text is checked again; blocking findings need an explicit yes."
  (let* ((checks (if (and content (string-match-p "\\`\\(?:skill\\|snippet\\)-" (plist-get p :kind)))
                     (let ((r (if (string-prefix-p "snippet-" (plist-get p :kind))
                                  (pai-memory-security-scan content (plist-get p :cwd))
                                (pai-memory-skill-check content (plist-get p :cwd)))))
                       (list :block (plist-get r :block)
                             :risk (append (plist-get r :warn) (plist-get r :block))))
                   (list :block (append (plist-get p :block) nil)
                         :risk (append (plist-get p :risk) nil))))
         (block (plist-get checks :block))
         (risk (plist-get checks :risk)))
    (cond
     (block (yes-or-no-p (format "BLOCKING finding (%s). Apply it anyway? "
                                 (string-join block ", "))))
     (risk (yes-or-no-p (format "This proposal is flagged (%s). Apply it? "
                                (string-join risk ", "))))
     (t t))))

(declare-function pai-memory-project-skill-in-git-p "pai-memory-share" (p))
(declare-function pai-memory-for-teammates "pai-memory-share" (text))
(declare-function pai-memory-git-add "pai-memory-share" (p))

(defun pai-memory-review--git-policy (p)
  "Return `ask' or `never' for proposal P's project skill in git, or nil."
  (and (fboundp 'pai-memory-project-skill-in-git-p) (pai-memory-project-skill-in-git-p p)
       (if (equal (format "%s" (pai-memory-get :long-term :commit-project-skills)) "never")
           'never 'ask)))

(defun pai-memory-review--accept-into-git (p content)
  "Apply P with teammate-ready front-matter, then stage it; report."
  (let* ((text (if (string-prefix-p "team-" (plist-get p :kind))
                   content
                 (pai-memory-for-teammates (or content (plist-get p :after)))))
         (result (pai-memory-proposal-accept (plist-get p :id) text)))
    (if (plist-get result :ok)
        (message "Applied %s; %s" (plist-get p :id)
                 (condition-case err (pai-memory-git-add p)
                   (user-error (format "git add failed: %s" (error-message-string err)))))
      (pai-memory-review--report (plist-get p :id) result))))

(defun pai-memory-review-accept (&optional content)
  "Accept the proposal at point, using CONTENT instead of its text when given.
A project skill in a git repository may also be staged (see
`:commit-project-skills')."
  (interactive)
  (let ((p (pai-memory-review--require)))
    (when (pai-memory-review--confirm p content)
      (if (and (eq (pai-memory-review--git-policy p) 'ask)
               (y-or-n-p "This project skill is in a git repository. Also git add it (session details replaced)? "))
          (pai-memory-review--accept-into-git p content)
        (pai-memory-review--report (plist-get p :id)
                                   (pai-memory-proposal-accept (plist-get p :id) content)))
      (pai-memory-review-refresh))))

(defun pai-memory-review-accept-git ()
  "Accept the project skill at point and `git add' it."
  (interactive)
  (let ((p (pai-memory-review--require)))
    (pcase (pai-memory-review--git-policy p)
      ('nil (user-error "Not a project skill inside a git repository"))
      ('never (user-error "Staging project skills is off (:commit-project-skills never)"))
      (_ (when (pai-memory-review--confirm p)
           (pai-memory-review--accept-into-git p nil)
           (pai-memory-review-refresh))))))

(defun pai-memory-review-move (&optional target)
  "Move the proposed entry at point to memory TARGET (asked for) before accepting.
Only proposals that add an entry can move; the diff and checks are redone
for the new memory."
  (interactive)
  (let* ((p (pai-memory-review--require))
         (choices (or (pai-memory-proposal-retarget-choices p)
                      (user-error (if (member (plist-get p :kind) pai-memory-retargetable-kinds)
                                      "No other memory to move it to"
                                    "Only proposals that add an entry can move to another memory"))))
         (to (or target (pai-memory-read-target
                         (format "Move from %s to: " (plist-get p :target)) choices))))
    (pai-memory-proposal-retarget (plist-get p :id) to)
    (message "Proposal now goes to %s (%s); a accepts it" to
             (file-name-nondirectory (pai-memory-target-file to (plist-get p :cwd))))
    (pai-memory-review-refresh)))

(defun pai-memory-review-reject (&optional reason)
  "Reject the proposal at point with REASON (asked for interactively)."
  (interactive (list (read-string "Reason (optional, shown to the promoter): ")))
  (let ((p (pai-memory-review--require)))
    (pai-memory-proposal-reject (plist-get p :id) reason)
    (message "Rejected %s" (plist-get p :id))
    (pai-memory-review-refresh)))

(defun pai-memory-review-accept-kind ()
  "Accept every non-risky pending proposal of the kind at point."
  (interactive)
  (let* ((kind (plist-get (pai-memory-review--require) :kind))
         (all (seq-filter (lambda (p) (and (equal (plist-get p :kind) kind)
                                           (seq-empty-p (plist-get p :risk))))
                          (pai-memory-proposals "pending"))))
    (when (and all (y-or-n-p (format "Accept %d %s proposal(s)? " (length all) kind)))
      (let ((ok 0))
        (dolist (p all)
          (when (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok) (cl-incf ok)))
        (message "Applied %d of %d" ok (length all)))
      (pai-memory-review-refresh))))

(defvar-local pai-memory-review--edit-id nil
  "Proposal id being edited in this buffer.")

(defvar pai-memory-review-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'pai-memory-review-edit-finish)
    (define-key m (kbd "C-c C-k") #'pai-memory-review-edit-cancel)
    m)
  "Keymap of `pai-memory-review-edit-mode'.")

(define-minor-mode pai-memory-review-edit-mode
  "Edit a proposal's text; C-c C-c accepts it, C-c C-k cancels."
  :lighter " pai-edit")

(defun pai-memory-review-edit ()
  "Edit the text of the proposal at point in a separate buffer."
  (interactive)
  (let* ((p (pai-memory-review--require))
         (kind (plist-get p :kind))
         (buf (get-buffer-create (format "*pai proposal %s*" (plist-get p :id)))))
    (when (member kind '("memory-remove" "skill-archive")) (user-error "Nothing to edit here"))
    (with-current-buffer buf
      (erase-buffer)
      (insert (or (plist-get p :content) ""))
      (if (string-prefix-p "skill-" kind) (text-mode) (text-mode))
      (pai-memory-review-edit-mode 1)
      (setq pai-memory-review--edit-id (plist-get p :id))
      (setq header-line-format "Edit the proposal, then C-c C-c to accept it or C-c C-k to cancel")
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun pai-memory-review-edit-finish ()
  "Accept the edited proposal."
  (interactive)
  (let ((id pai-memory-review--edit-id)
        (text (buffer-string)))
    (pai-memory-review--report id (pai-memory-proposal-accept id text))
    (quit-window t)
    (when-let ((rb (get-buffer "*pai memory review*")))
      (with-current-buffer rb (pai-memory-review-refresh)))))

(defun pai-memory-review-edit-cancel ()
  "Discard the edit."
  (interactive)
  (quit-window t))

(provide 'pai-memory-review)
;;; pai-memory-review.el ends here
