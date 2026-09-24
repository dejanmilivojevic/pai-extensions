;;; pai-learn-log.el --- Mirror a teaching session into an Org file -*- lexical-binding: t; -*-

;;; Commentary:

;; Port of the `md-log' extension of the learn system.  Upstream mirrors the
;; session into a Markdown note read in Obsidian, because a terminal renders
;; neither markdown, math nor images.  In Emacs the natural reader is Org:
;; `/teach-log FILE' links an Org file to the session, fills it with the
;; lesson so far, and appends every later turn:
;;
;;   - your prompts and the tutor's replies (Markdown converted to Org);
;;   - ask_user_question and quiz questions with their answers, verdicts,
;;     notes and explanations (never before they were answered);
;;   - diagrams: `![caption](file.png)' becomes an inline Org image, and a
;;     ```mermaid block is rendered to a PNG next to the log (in viz/) with
;;     the in-Emacs renderer of `pai-learn-render'.
;;
;; Math stays LaTeX: Org shows $...$ natively, `org-pretty-entities' is turned
;; on in the log buffer (\alpha -> α, x^2 as a superscript) and C-c C-x C-l
;; previews fragments when a LaTeX installation is available.
;;
;; The log is written through the file's own Emacs buffer and saved after
;; every append, so it can be kept open beside the session.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-core)
(require 'pai-learn-render)

(declare-function org-display-inline-images "org" (&optional include-linked refresh beg end))
(declare-function org-restart-font-lock "org" ())
(defvar org-pretty-entities)
(defvar org-pretty-entities-include-sub-superscripts)
(defvar org-image-actual-width)
(defvar pai--context-messages)

(defcustom pai-learn-log-image-width 500
  "Display width (pixels) of diagrams embedded in the lesson log."
  :type 'integer :group 'pai-learn)

(defcustom pai-learn-log-display t
  "Whether linking a log also shows it in another window."
  :type 'boolean :group 'pai-learn)

(defvar-local pai-learn-log-file nil
  "Org file this session is mirrored into, or nil.")

(defvar-local pai-learn-log--calls nil
  "Hash of tool-call id to (NAME . ARGS) for question tools seen so far.")

(defconst pai-learn-log--qa-tools '("quiz" "ask_user_question")
  "Tools whose questions and answers are mirrored into the log.")

;;;; Markdown -> Org

(defun pai-learn-log--inline (text)
  "Convert Markdown inline markup in TEXT (one line, no code spans) to Org."
  (let ((case-fold-search nil))
    ;; Obsidian embeds ![[file|500]] and Markdown images ![alt](path).
    (setq text (replace-regexp-in-string
                "!\\[\\[\\([^]|]+\\)\\(?:|[^]]*\\)?\\]\\]" "[[file:\\1]]" text t))
    (setq text (replace-regexp-in-string
                "!\\[[^]]*\\](\\([^) ]+\\)\\(?: +\"[^\"]*\"\\)?)"
                (lambda (m)
                  (let ((target (match-string 1 m)))
                    (format "[[%s]]" (if (string-match-p "\\`[a-z]+:" target)
                                         target
                                       (concat "file:" target)))))
                text t t))
    ;; Links [text](url).
    (setq text (replace-regexp-in-string
                "\\[\\([^]]+\\)\\](\\([^) ]+\\))" "[[\\2][\\1]]" text t))
    ;; Italic *x* (not **), then bold **x** / __x__.
    (setq text (replace-regexp-in-string
                "\\(\\`\\|[^*\\\\]\\)\\*\\([^* \t][^*]*?\\)\\*\\([^*]\\|\\'\\)"
                "\\1/\\2/\\3" text t))
    (setq text (replace-regexp-in-string "\\*\\*\\([^*]+?\\)\\*\\*" "*\\1*" text t))
    (setq text (replace-regexp-in-string "__\\([^_]+?\\)__" "*\\1*" text t))
    ;; One-line display math $$x$$ -> \[x\].
    (setq text (replace-regexp-in-string "\\$\\$\\(.+?\\)\\$\\$" "\\\\[\\1\\\\]" text t))
    text))

(defun pai-learn-log--inline-line (line)
  "Convert LINE's inline markup, turning `code' spans into Org verbatim."
  (let ((parts (split-string line "`"))
        (out '())
        (code nil))
    (if (cl-evenp (length parts))
        ;; Unbalanced backticks: leave code spans alone.
        (pai-learn-log--inline line)
      (dolist (part parts)
        (push (if code
                  (if (string-search "=" part) (format "~%s~" part) (format "=%s=" part))
                (pai-learn-log--inline part))
              out)
        (setq code (not code)))
      (apply #'concat (nreverse out)))))

(defun pai-learn-log--table-rule-p (line)
  "Return non-nil when LINE is a Markdown table separator row."
  (string-match-p "\\`[ \t]*|?[ \t]*:?-+:?[ \t]*\\(|[ \t]*:?-+:?[ \t]*\\)+|?[ \t]*\\'" line))

(defun pai-learn-log-markdown-to-org (text &optional level)
  "Convert Markdown TEXT to Org.
Headings are demoted below LEVEL (default 1), so they nest under the entry
heading the text is logged under."
  (let ((level (or level 1))
        (out '())
        (fence nil)                     ; closing string while inside a fence
        (math nil)
        (quote nil))
    (cl-flet ((emit (s) (push s out))
              (end-quote () (when quote (push "#+end_quote" out) (setq quote nil))))
      (dolist (line (split-string (or text "") "\n"))
        (cond
         ;; Inside a fenced block: verbatim, with Org's comma escapes.
         (fence
          (if (string-match-p (concat "\\`[ \t]*" (regexp-quote (car fence)) "[ \t]*\\'") line)
              (progn (emit (cdr fence)) (setq fence nil))
            (emit (if (string-match-p "\\`[ \t]*\\(\\*\\|#\\+\\|,\\*\\|,#\\+\\)" line)
                      (concat "," line)
                    line))))
         ((string-match "\\`[ \t]*\\(```+\\|~~~+\\)[ \t]*\\([^ \t`]*\\)" line)
          (end-quote)
          (let ((marker (match-string 1 line)) (lang (match-string 2 line)))
            (if (string-empty-p lang)
                (progn (emit "#+begin_example") (setq fence (cons marker "#+end_example")))
              (emit (concat "#+begin_src " lang))
              (setq fence (cons marker "#+end_src")))))
         ;; Display math fenced by $$ lines.
         ((string-match-p "\\`[ \t]*\\$\\$[ \t]*\\'" line)
          (end-quote)
          (emit (if math "\\]" "\\["))
          (setq math (not math)))
         (math (emit line))
         ;; Block quotes.
         ((string-match "\\`[ \t]*> ?\\(.*\\)" line)
          (unless quote (emit "#+begin_quote") (setq quote t))
          (emit (pai-learn-log--inline-line (match-string 1 line))))
         (t
          (end-quote)
          (cond
           ((string-match "\\`\\(#+\\)[ \t]+\\(.*?\\)[ \t#]*\\'" line)
            (emit (concat (make-string (+ level (length (match-string 1 line))) ?*)
                          " " (pai-learn-log--inline-line (match-string 2 line)))))
           ((string-match-p "\\`[ \t]*\\([-*_]\\)\\([ \t]*\\1\\)\\{2,\\}[ \t]*\\'" line)
            (emit "-----"))
           ((pai-learn-log--table-rule-p line)
            (let ((cells (split-string (string-trim line "[ \t|]+" "[ \t|]+") "|")))
              (emit (concat "|"
                            (mapconcat (lambda (c) (make-string (max 3 (length (string-trim c))) ?-))
                                       cells "+")
                            "|"))))
           ((string-match "\\`\\([ \t]*\\)[*+][ \t]+\\(.*\\)" line)
            (emit (concat (match-string 1 line) "- "
                          (pai-learn-log--inline-line (match-string 2 line)))))
           ((string-match "\\`[ \t]*!\\[" line)
            (emit (format "#+attr_org: :width %d" pai-learn-log-image-width))
            (emit (pai-learn-log--inline-line line)))
           (t (emit (pai-learn-log--inline-line line)))))))
      (when fence (emit (cdr fence)))
      (when math (emit "\\]"))
      (end-quote))
    (string-trim (string-join (nreverse out) "\n"))))

;;;; Entries

(defun pai-learn-log--strip-skills (text)
  "Replace <skill ...>...</skill> blocks in TEXT with a short note."
  (replace-regexp-in-string
   "<skill\\b\\([^>]*\\)>\\(?:.\\|\n\\)*?</skill>"
   (lambda (m)
     (let ((name (save-match-data
                   (and (string-match "name=\"\\([^\"]+\\)\"" m) (match-string 1 m)))))
       (format "/(skill loaded: %s)/" (or name "?"))))
   text t t))

(defun pai-learn-log--question-lines (question context options)
  "Return Org lines presenting QUESTION, CONTEXT and numbered OPTIONS."
  (append (list (pai-learn-log-markdown-to-org question 2))
          (when context (list "" (pai-learn-log-markdown-to-org context 2)))
          (when options (list ""))
          (cl-loop for o in options for i from 1
                   collect (format "%d. %s" i (plist-get o :label)))))

(defun pai-learn-log--quiz-entry (args details)
  "Return the Org entry for a quiz with ARGS answered with DETAILS."
  (let* ((status (plist-get details :status))
         (options (or (append (plist-get details :options) nil)
                      (append (plist-get args :options) nil)))
         (correct (append (plist-get details :correct-indices) nil))
         (answers (append (plist-get details :answers) nil))
         (dont-know (pai-truthy (plist-get details :dont-know)))
         (right (pai-truthy (plist-get details :correct)))
         (title (cond ((not (equal status "answered")) (format "Quiz — %s" (or status "?")))
                      (dont-know "Quiz — I don't know")
                      (right "Quiz — correct ✓")
                      (t "Quiz — incorrect ✗"))))
    (string-join
     (append
      (list (concat "* " title))
      (pai-learn-log--question-lines (or (plist-get details :question) (plist-get args :question))
                                     (or (plist-get details :context) (plist-get args :details))
                                     options)
      (list "")
      (if (equal status "answered")
          (append
           (list (concat "- Your answer :: "
                         (if dont-know "I don't know"
                           (mapconcat (lambda (a) (format "%s. %s" (plist-get a :index)
                                                          (plist-get a :label)))
                                      answers ", ")))
                 (concat "- Correct answer :: "
                         (mapconcat (lambda (i) (format "%s" i)) correct ", ")))
           (when (plist-get details :note)
             (list (concat "- Note :: " (plist-get details :note))))
           (when (plist-get details :explanation)
             (list "" "#+begin_quote"
                   (pai-learn-log-markdown-to-org (plist-get details :explanation) 2)
                   "#+end_quote")))
        (list (format "/(%s)/" (or (plist-get details :message) "not answered")))))
     "\n")))

(defun pai-learn-log--ask-entry (args details)
  "Return the Org entry for an ask_user_question with ARGS answered with DETAILS."
  (let ((answers (append (plist-get details :answers) nil)))
    (string-join
     (append
      (list "* Question")
      (pai-learn-log--question-lines (plist-get args :question) (plist-get args :details)
                                     (append (plist-get args :options) nil))
      (list "")
      (if answers
          (mapcar (lambda (a)
                    (pcase (plist-get a :type)
                      ("option" (format "- Answer :: %s. %s" (plist-get a :index) (plist-get a :label)))
                      ("other" (concat "- Answer (other) :: " (plist-get a :label)))
                      (_ (concat "- Answer :: " (plist-get a :label)))))
                  answers)
        (list (format "/(%s)/" (or (plist-get details :message) "not answered")))))
     "\n")))

(defun pai-learn-log-entry (message calls)
  "Return the Org entry for MESSAGE, or nil when it is not logged.
CALLS is a hash of tool-call id to (NAME . ARGS); question tool calls of
assistant messages are recorded in it so their results can be paired."
  (pcase (pai-message-role message)
    ('user
     (let ((text (string-trim (pai-learn-log--strip-skills
                               (pai-content-text (pai-message-content message))))))
       (unless (string-empty-p text)
         (concat "* You\n" (pai-learn-log-markdown-to-org text)))))
    ('assistant
     (let ((texts '()))
       (dolist (block (pai-normalize-content (pai-message-content message)))
         (pcase (pai-block-type block)
           ('text (let ((tx (string-trim (or (plist-get block :text) ""))))
                    (unless (string-empty-p tx) (push tx texts))))
           ('tool-call (when (member (plist-get block :name) pai-learn-log--qa-tools)
                         (puthash (plist-get block :id)
                                  (cons (plist-get block :name) (plist-get block :arguments))
                                  calls)))))
       (when texts
         (concat "* Tutor\n"
                 (pai-learn-log-markdown-to-org (string-join (nreverse texts) "\n\n"))))))
    ('tool-result
     (let ((name (plist-get message :tool-name))
           (details (plist-get message :details)))
       (when (and (member name pai-learn-log--qa-tools)
                  ;; A deferred-schema reveal is not a question.
                  (not (plist-get details :deferred-schema))
                  (plist-get details :status))
         (let ((args (cdr (gethash (plist-get message :tool-call-id) calls))))
           (if (equal name "quiz")
               (pai-learn-log--quiz-entry args details)
             (pai-learn-log--ask-entry args details))))))))

;;;; Writing

(defun pai-learn-log--viz-directory (file)
  "Return the diagram directory belonging to log FILE."
  (expand-file-name "viz" (file-name-directory file)))

(defun pai-learn-log--buffer (file)
  "Return the live buffer visiting log FILE, set up for reading."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode) (org-mode))
      (setq-local org-image-actual-width nil)
      (unless (bound-and-true-p org-pretty-entities)
        (setq-local org-pretty-entities t
                    org-pretty-entities-include-sub-superscripts t)
        (org-restart-font-lock)))
    buffer))

(defun pai-learn-log--render-mermaid (buffer start end)
  "Render the mermaid source blocks between START and END of BUFFER.
Each PNG is saved in viz/ next to the log (named after a hash of its
source, so re-logging reuses it) and linked right after its block."
  (with-current-buffer buffer
    (save-excursion
      (goto-char start)
      (setq end (copy-marker end))
      (while (re-search-forward "^#\\+begin_src mermaid[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n#\\+end_src[ \t]*$" end t)
        (let* ((source (replace-regexp-in-string "^," "" (match-string-no-properties 1)))
               (after (copy-marker (match-end 0)))
               (name (format "viz-map-%s.png" (substring (md5 source) 0 10)))
               (file (expand-file-name name (pai-learn-log--viz-directory
                                             (buffer-file-name buffer)))))
          (cl-flet ((link ()
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (save-excursion
                            (goto-char after)
                            (insert (format "\n#+attr_org: :width %d\n[[file:viz/%s]]"
                                            pai-learn-log-image-width name))
                            (let ((inhibit-message t)) (save-buffer))
                            (ignore-errors (org-display-inline-images nil t)))))))
            (if (file-exists-p file)
                (link)
              (pai-learn-render
               "mermaid" source
               (lambda (result)
                 (if (plist-get result :ok)
                     (progn (pai-learn-render-save (plist-get result :data) file)
                            (link))
                   (message "pai-learn: mermaid block not rendered: %s"
                            (plist-get result :error))))))))))))

(defun pai-learn-log--write (file text &optional replace)
  "Append TEXT (Org) to log FILE; with REPLACE, make it the whole content."
  (let ((buffer (pai-learn-log--buffer file)))
    (with-current-buffer buffer
      (save-excursion
        (let ((inhibit-read-only t) start)
          (when replace (erase-buffer))
          (goto-char (point-max))
          (unless (bobp)
            (skip-chars-backward " \t\n")
            (delete-region (point) (point-max))
            (insert "\n\n"))
          (setq start (point))
          (insert text "\n")
          (let ((inhibit-message t)) (save-buffer))
          (ignore-errors (org-display-inline-images nil t))
          (when (pai-learn-render-available-p)
            (pai-learn-log--render-mermaid buffer start (point-max))))))
    buffer))

(defun pai-learn-log-append-message (message)
  "Append MESSAGE to this session's log, when one is linked."
  (when pai-learn-log-file
    (unless pai-learn-log--calls
      (setq pai-learn-log--calls (make-hash-table :test 'equal)))
    (condition-case err
        (when-let ((entry (pai-learn-log-entry message pai-learn-log--calls)))
          (pai-learn-log--write pai-learn-log-file entry))
      (error (message "pai-learn-log: %s" (error-message-string err))))))

(defun pai-learn-log--header (file)
  "Return the Org header written at the top of log FILE."
  (format "#+title: %s\n#+startup: inlineimages\n#+options: tex:t\n"
          (capitalize (replace-regexp-in-string
                       "[-_]" " " (file-name-base file)))))

(defun pai-learn-log-link (file)
  "Link log FILE to this session and fill it with the conversation so far.
The file is rewritten from the session (its buffer can undo that).
Return the number of logged entries."
  (let* ((file (expand-file-name file))
         (calls (make-hash-table :test 'equal))
         (entries (delq nil (mapcar (lambda (m) (pai-learn-log-entry m calls))
                                    pai--context-messages))))
    (make-directory (file-name-directory file) t)
    (setq pai-learn-log-file file
          pai-learn-log--calls calls)
    (let ((buffer (pai-learn-log--write
                   file (string-join (cons (pai-learn-log--header file) entries) "\n")
                   'replace)))
      (when pai-learn-log-display
        (display-buffer buffer '((display-buffer-reuse-window
                                  display-buffer-use-some-window)
                                 (inhibit-same-window . t)))))
    (length entries)))

(defun pai-learn-log-unlink ()
  "Stop mirroring this session.  Return the file that was linked."
  (prog1 pai-learn-log-file
    (setq pai-learn-log-file nil
          pai-learn-log--calls nil)))

(provide 'pai-learn-log)
;;; pai-learn-log.el ends here
