;;; pai-web-prompt.el --- Minibuffer prompts and questions answered from the browser -*- lexical-binding: t; -*-

;;; Commentary:

;; Minibuffer prompts.  While a page is connected, a minibuffer prompt is
;; also shown in the browser when it was opened
;;
;;   - by something the page asked for (an action runs with
;;     `pai-web--origin' bound: /model without argument, /resume, a trust
;;     question for a new instance, a key pressed in a remote buffer...), or
;;   - from a pai buffer (chat or remote) outside a command you typed in
;;     Emacs: a timer or an agent callback (an extension's confirm/select/
;;     input while a run goes on) -- `this-command' is nil there.
;;
;; The prompt stays in Emacs too; whichever side answers first wins.  An
;; answer from the page is put into the minibuffer and submitted with
;; `exit-minibuffer', as a command run by the minibuffer's own command loop
;; (see `pai-web-run'), never from a timer or filter: its throw must only
;; leave the minibuffer, not unwind whatever else Emacs was waiting in.
;; Cancel aborts the minibuffer like C-g.  When Emacs answers, the minibuffer's
;; exit hook takes the prompt off the page.
;;
;; `completing-read', `read-from-minibuffer', `read-string' (a primitive
;; that reaches `read-from-minibuffer' from C, past its advice),
;; `y-or-n-p', `yes-or-no-p' and `read-passwd' are advised to describe the
;; prompt (its kind, completion table, default) for the setup hook.  A
;; forwarded completion uses the default minibuffer completion UI instead
;; of Helm/Ivy-style frameworks, because only that one takes its answer
;; from the minibuffer text the page fills in.
;;
;; Questions of `ask_user_question' (pai-ask-user) are listed for the page
;; and answered through that extension's own functions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-web-util)
(require 'pai-web-bus)
(require 'pai-web-instances)

(declare-function pai-ask-user-pending "pai-ask-user" ())
(declare-function pai-ask-user-request-id "pai-ask-user" (req))
(declare-function pai-ask-user-request-question "pai-ask-user" (req))
(declare-function pai-ask-user-request-details "pai-ask-user" (req))
(declare-function pai-ask-user-request-mode "pai-ask-user" (req))
(declare-function pai-ask-user-request-options "pai-ask-user" (req))
(declare-function pai-ask-user-request-other-label "pai-ask-user" (req))
(declare-function pai-ask-user-request-chat-buffer "pai-ask-user" (req))
(declare-function pai-ask-user-request-done "pai-ask-user" (req))
(declare-function pai-ask-user--answer "pai-ask-user" (req answers))
(declare-function pai-ask-user--cancel "pai-ask-user" (req &optional message))
(declare-function pai-ask-user--option-answer "pai-ask-user" (req index))
(declare-function pai-ask-user--other-answer "pai-ask-user" (text))

(defvar pai-web--origin nil
  "Non-nil while running an action a browser page asked for.
The id of the page's client when known, else t.")

(defvar pai-web-buffer-related-p-function nil
  "Function telling whether a buffer is shown in the browser (set by pai-web-buffers).")

(defconst pai-web-prompt-candidates-limit 3000
  "Completion candidates sent with a prompt.")

;;;; Describing prompts

(defvar pai-web--pctx nil
  "Description of the minibuffer prompt about to open, or nil.
A plist (:kind KIND :caller BUFFER :origin BOOL :used BOOL ...).")

(defun pai-web--forward-p (caller)
  "Return non-nil when a prompt opened from buffer CALLER goes to the pages."
  (and (pai-web-bus-active-p)
       (or pai-web--origin
           (and (null this-command)
                (buffer-live-p caller)
                (or (eq (buffer-local-value 'major-mode caller) 'pai-mode)
                    (and pai-web-buffer-related-p-function
                         (funcall pai-web-buffer-related-p-function caller)))))))

(defun pai-web--describe (kind extra orig args)
  "Call ORIG with ARGS, describing the prompt it opens as KIND with EXTRA."
  (if (and pai-web--pctx (not (plist-get pai-web--pctx :used)))
      (apply orig args)                 ; an outer function describes it
    (let* ((caller (current-buffer))
           (forward (pai-web--forward-p caller))
           (pai-web--pctx (and forward
                               (append (list :kind kind :caller caller
                                             :origin (and pai-web--origin t) :used nil)
                                       extra)))
           (completing-read-function (if (and forward (eq kind 'completion))
                                         #'completing-read-default
                                       completing-read-function)))
      (apply orig args))))

(defun pai-web--advise-completing-read (orig prompt collection &optional predicate
                                             require-match initial hist def &rest rest)
  "Describe a `completing-read' (ORIG with PROMPT COLLECTION ... DEF REST)."
  (pai-web--describe 'completion
                     (list :table collection :pred predicate :require require-match
                           :default (if (consp def) (car def) def))
                     orig (append (list prompt collection predicate require-match
                                        initial hist def)
                                  rest)))

(defun pai-web--advise-read-from-minibuffer (orig &rest args)
  "Describe a `read-from-minibuffer' (ORIG with ARGS)."
  (pai-web--describe 'text (list :default (let ((d (nth 5 args))) (if (consp d) (car d) d)))
                     orig args))

(defun pai-web--advise-read-string (orig &rest args)
  "Describe a `read-string' (ORIG with ARGS).
It is a primitive calling `read-from-minibuffer' from C, past that advice."
  (pai-web--describe 'text (list :default (let ((d (nth 3 args))) (if (consp d) (car d) d)))
                     orig args))

(defun pai-web--advise-y-or-n-p (orig &rest args)
  "Describe a `y-or-n-p' (ORIG with ARGS)."
  (pai-web--describe 'y-or-n nil orig args))

(defun pai-web--advise-yes-or-no-p (orig &rest args)
  "Describe a `yes-or-no-p' (ORIG with ARGS)."
  (pai-web--describe 'yes-or-no nil orig args))

(defun pai-web--advise-read-passwd (orig &rest args)
  "Describe a `read-passwd' (ORIG with ARGS)."
  (pai-web--describe 'password nil orig args))

;;;; Open prompts

(cl-defstruct (pai-web-prompt (:constructor pai-web-prompt--create))
  "A minibuffer prompt shown in the browser."
  id minibuffer depth text kind initial default table pred require caller origin)

(defvar pai-web--prompts nil "Open forwarded prompts, oldest first.")

(defun pai-web--minibuffer-setup ()
  "Register the minibuffer that just opened when it is forwarded."
  (let ((ctx pai-web--pctx))
    (when (and ctx (not (plist-get ctx :used)) (pai-web-bus-active-p))
      (plist-put ctx :used t)
      (condition-case err
          (let ((p (pai-web-prompt--create
                    :id (pai-web-random-hex 8)
                    :minibuffer (current-buffer)
                    :depth (minibuffer-depth)
                    :text (substring-no-properties (or (minibuffer-prompt) ""))
                    :kind (plist-get ctx :kind)
                    :initial (minibuffer-contents-no-properties)
                    :default (let ((d (plist-get ctx :default)))
                               (and (stringp d) d))
                    :table (or (plist-get ctx :table) minibuffer-completion-table)
                    :pred (or (plist-get ctx :pred) minibuffer-completion-predicate)
                    :require (plist-get ctx :require)
                    :caller (plist-get ctx :caller)
                    :origin (plist-get ctx :origin))))
            (setq pai-web--prompts (append pai-web--prompts (list p)))
            (add-hook 'minibuffer-exit-hook #'pai-web--minibuffer-exit nil t)
            (pai-web-bus-broadcast (list :t "prompt" :prompt (pai-web-prompt-json p))))
        (error (message "pai-web: prompt not forwarded: %s" (error-message-string err)))))))

(defun pai-web--minibuffer-exit ()
  "The minibuffer closed (answered in Emacs or from a page, or aborted)."
  (let ((mb (current-buffer)) (depth (minibuffer-depth)))
    (dolist (p pai-web--prompts)
      (when (and (eq (pai-web-prompt-minibuffer p) mb)
                 (eql (pai-web-prompt-depth p) depth))
        (pai-web--prompt-closed p)))))

(defun pai-web--prompt-closed (prompt)
  "Take PROMPT off the pages."
  (setq pai-web--prompts (delq prompt pai-web--prompts))
  (pai-web-bus-broadcast (list :t "prompt-closed" :id (pai-web-prompt-id prompt))))

(defun pai-web--dynamic-table-p (table)
  "Return non-nil when completion TABLE is computed from the input."
  (and (functionp table) (not (hash-table-p table)) (not (obarrayp table))))

(defun pai-web--candidates (prompt input)
  "Return PROMPT's completions of INPUT as (BASE . CANDIDATES), or nil.
Matching follows Emacs (`pai-web-completions'): BASE is the part of INPUT
the candidates do not replace, e.g. the directory of a file name."
  (let ((table (pai-web-prompt-table prompt))
        (mb (pai-web-prompt-minibuffer prompt))
        (input (or input "")))
    (when table
      (condition-case nil
          (let ((res (if (buffer-live-p mb)
                         (with-current-buffer mb
                           (pai-web-completions input table (pai-web-prompt-pred prompt)))
                       (pai-web-completions input table (pai-web-prompt-pred prompt)))))
            (cons (substring input 0 (min (car res) (length input)))
                  (seq-take (cdr res) pai-web-prompt-candidates-limit)))
        (error nil)))))

(defun pai-web-prompt-json (prompt)
  "Return PROMPT as a plist for JSON."
  (let* ((table (pai-web-prompt-table prompt))
         (dynamic (pai-web--dynamic-table-p table))
         (cands (and table (pai-web--candidates
                            prompt (if dynamic (pai-web-prompt-initial prompt) "")))))
    (list :id (pai-web-prompt-id prompt)
          :instance (let ((c (pai-web-prompt-caller prompt)))
                      (if (and (buffer-live-p c) (eq (buffer-local-value 'major-mode c) 'pai-mode))
                          (pai-web-id c) :null))
          :caller (let ((c (pai-web-prompt-caller prompt)))
                    (if (buffer-live-p c) (buffer-name c) :null))
          :text (pai-web-prompt-text prompt)
          :kind (symbol-name (if (and (eq (pai-web-prompt-kind prompt) 'text) table)
                                 'completion
                               (pai-web-prompt-kind prompt)))
          :initial (or (pai-web-prompt-initial prompt) "")
          :default (or (pai-web-prompt-default prompt) :null)
          :require (pai-web-bool (and (pai-web-prompt-require prompt)
                                      (not (eq (pai-web-prompt-require prompt) :false))))
          :dynamic (pai-web-bool dynamic)
          :origin (pai-web-bool (pai-web-prompt-origin prompt))
          ;; candidates complete the input after BASE (a file name's directory)
          :base (or (car cands) "")
          :candidates (vconcat (cdr cands)))))

(defun pai-web-prompts-json ()
  "Return the open prompts as a vector."
  (pai-web--prompts-prune)
  (vconcat (mapcar #'pai-web-prompt-json pai-web--prompts)))

(defun pai-web--prompts-prune ()
  "Forget prompts whose minibuffer is no longer active."
  (dolist (p pai-web--prompts)
    (let ((mb (pai-web-prompt-minibuffer p)))
      (unless (and (buffer-live-p mb) (minibufferp mb)
                   (>= (minibuffer-depth) (pai-web-prompt-depth p)))
        (pai-web--prompt-closed p)))))

(defun pai-web-prompt (id)
  "Return the open prompt with ID, or nil."
  (seq-find (lambda (p) (equal (pai-web-prompt-id p) id)) pai-web--prompts))

(defun pai-web-prompt-complete (id input)
  "Return the completions of prompt ID for INPUT (for dynamic tables).
A plist (:base BASE :candidates [...]); see `pai-web--candidates'."
  (let* ((p (pai-web-prompt id))
         (res (and p (pai-web--candidates p input))))
    (list :base (or (car res) "") :candidates (vconcat (cdr res)))))

(defun pai-web--innermost-p (prompt)
  "Return non-nil when PROMPT's minibuffer is the innermost active one."
  (let ((win (active-minibuffer-window)))
    (and win
         (eq (window-buffer win) (pai-web-prompt-minibuffer prompt))
         (eql (minibuffer-depth) (pai-web-prompt-depth prompt)))))

(defun pai-web-prompt-answer (id value)
  "Answer prompt ID with VALUE (a string) or cancel it when VALUE is nil.
Must run as a command of the minibuffer's command loop (`pai-web-run').
Return nil or an error message."
  (let ((p (pai-web-prompt id)))
    (cond
     ((null p) "That prompt is no longer open")
     ((not (pai-web--innermost-p p))
      (pai-web--prompts-prune)
      (if (pai-web-prompt id)
          "Another prompt is open on top of this one; answer it first"
        "That prompt is no longer open"))
     (t
      (with-selected-window (active-minibuffer-window)
        (if (null value)
            (abort-recursive-edit)
          (delete-minibuffer-contents)
          (insert value)
          (exit-minibuffer)))
      nil))))

;;;; Install

(defconst pai-web--prompt-advice
  '((completing-read . pai-web--advise-completing-read)
    (read-from-minibuffer . pai-web--advise-read-from-minibuffer)
    (read-string . pai-web--advise-read-string)
    (y-or-n-p . pai-web--advise-y-or-n-p)
    (yes-or-no-p . pai-web--advise-yes-or-no-p)
    (read-passwd . pai-web--advise-read-passwd))
  "Prompt functions and their describing advice.")

(defun pai-web-prompt-install ()
  "Start forwarding minibuffer prompts."
  (dolist (a pai-web--prompt-advice)
    (advice-add (car a) :around (cdr a)))
  (add-hook 'minibuffer-setup-hook #'pai-web--minibuffer-setup))

(defun pai-web-prompt-uninstall ()
  "Stop forwarding minibuffer prompts."
  (dolist (a pai-web--prompt-advice)
    (advice-remove (car a) (cdr a)))
  (remove-hook 'minibuffer-setup-hook #'pai-web--minibuffer-setup)
  (setq pai-web--prompts nil))

;;;; ask_user_question

(defun pai-web-asks ()
  "Return the pending ask_user questions as a vector."
  (if (not (fboundp 'pai-ask-user-pending))
      []
    (vconcat
     (delq nil
           (mapcar
            (lambda (r)
              (unless (pai-ask-user-request-done r)
                (let ((chat (pai-ask-user-request-chat-buffer r)))
                  (list :id (pai-ask-user-request-id r)
                        :instance (if (buffer-live-p chat) (pai-web-id chat) :null)
                        :question (or (pai-ask-user-request-question r) "")
                        :details (or (pai-ask-user-request-details r) :null)
                        :mode (symbol-name (pai-ask-user-request-mode r))
                        :other (or (pai-ask-user-request-other-label r) "Other")
                        :options (vconcat
                                  (mapcar (lambda (o)
                                            (list :label (plist-get o :label)
                                                  :description (or (plist-get o :description) :null)))
                                          (pai-ask-user-request-options r)))))))
            (ignore-errors (pai-ask-user-pending)))))))

(defun pai-web-ask-answer (id choices other text cancel)
  "Answer ask_user question ID.
CHOICES are 1-based option numbers, OTHER a custom answer, TEXT the answer
of a free-form question; CANCEL cancels it.  Return nil or an error message."
  (let ((req (and (fboundp 'pai-ask-user-pending)
                  (seq-find (lambda (r) (equal (pai-ask-user-request-id r) id))
                            (pai-ask-user-pending)))))
    (cond
     ((null req) "That question is no longer open")
     (cancel (pai-ask-user--cancel req) nil)
     (t
      (let* ((mode (pai-ask-user-request-mode req))
             (answers
              (if (eq mode 'text)
                  (list (list :type "text" :label (or text "") :value (or text "")))
                (append (delq nil (mapcar (lambda (i) (pai-ask-user--option-answer req i))
                                          (if (eq mode 'single-select)
                                              (seq-take choices 1) choices)))
                        (and (stringp other) (not (string-empty-p (string-trim other)))
                             (or (null choices) (eq mode 'multi-select))
                             (list (pai-ask-user--other-answer (string-trim other))))))))
        (if (null answers)
            "Choose an option or write an answer"
          (pai-ask-user--answer req answers)
          nil))))))

(provide 'pai-web-prompt)
;;; pai-web-prompt.el ends here
