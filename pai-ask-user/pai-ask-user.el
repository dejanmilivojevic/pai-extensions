;;; pai-ask-user.el --- Ask the user a question and wait for the answer -*- lexical-binding: t; -*-

;; An Emacs port of the `ask_user_question' pi extension
;; (https://github.com/amosblomqvist/pi-config/blob/main/extensions/ask-user-question.ts).
;;
;; Upstream renders the question as a pi-tui overlay with an inline editor.
;; Emacs has better building blocks for the same job, so the dialog is a real
;; buffer: select modes render with `vui' (checkboxes, buttons, TAB
;; navigation, digit shortcuts) and free-form answers are composed in an
;; ordinary text buffer that is submitted with C-c C-c.  The tool contract --
;; parameters, modes, result text and the structured `details' payload -- is
;; the same as upstream's, so the model sees exactly what it sees there.

;;; Commentary:

;; Registers one tool:
;;
;;   ask_user_question {question, details?, options?, multiSelect?}
;;
;; Three modes follow from the arguments, exactly as upstream:
;;
;;   text           no options        free-form answer in an editor buffer
;;   single-select  options           pick one, or write a custom answer
;;   multi-select   options + multi   toggle several, plus a custom answer
;;
;; The tool call is asynchronous: it is held open (nothing is sent to the
;; model, the run just waits) until the user answers, cancels, the question
;; times out, or the run is interrupted.  An unanswered question never wedges
;; a session: killing its buffer cancels it, `pai-interrupt' cancels it, and
;; the optional timeout cancels it.
;;
;; Keys in a select dialog: 1-9 choose/toggle an option, TAB/S-TAB move, RET
;; activates, `o' writes a custom answer, C-c C-c submits (multi-select),
;; C-c C-k cancels.  `/ask' redisplays a question you navigated away from.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai)
(require 'pai-core)
(require 'pai-tools)
(require 'pai-agent)
(require 'pai-ext)
(require 'pai-settings)
(require 'pai-markdown)
(require 'vui)
(require 'vui-components)

(declare-function pai-settings-ui-register-section "pai-settings-ui")
(declare-function pai-settings-ui-register-subsection "pai-settings-ui")
(declare-function pai-settings-ui-register-item "pai-settings-ui")

;;;; Options

(defgroup pai-ask-user nil
  "Questions the agent asks the user through the `ask_user_question' tool."
  :group 'pai)

(defcustom pai-ask-user-fill-column 76
  "Column the question and details text is wrapped at."
  :type 'integer :group 'pai-ask-user)

(defcustom pai-ask-user-display-action
  '((display-buffer-reuse-window display-buffer-below-selected
     display-buffer-pop-up-window)
    (window-height . fit-window-to-buffer))
  "`display-buffer' action used to show a question."
  :type 'sexp :group 'pai-ask-user)

(defcustom pai-ask-user-select-window t
  "Whether to select the question window when a question is asked."
  :type 'boolean :group 'pai-ask-user)

(defcustom pai-ask-user-return-focus t
  "Whether to return point to the session buffer once a question is answered."
  :type 'boolean :group 'pai-ask-user)

(defcustom pai-ask-user-echo-answer t
  "Whether to echo the question and its answer into the session transcript.
The tool result already reports the answer to the model; the note is what
makes a decision findable when you scroll back through a long session."
  :type 'boolean :group 'pai-ask-user)

(defcustom pai-ask-user-remember-answers t
  "Whether to preselect the answer given last time a question was asked.
Memory is per session buffer and lives only as long as it does: a repeated
question starts on the previous answer, so RET repeats it."
  :type 'boolean :group 'pai-ask-user)

(defcustom pai-ask-user-timeout nil
  "Seconds to wait for an answer before cancelling the question.
nil (or a non-positive number) waits indefinitely.  The `:ask-user' setting's
`:timeout' key overrides this per project."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'pai-ask-user)

(defun pai-ask-user-config ()
  "Return the `:ask-user' settings plist."
  (let ((value (pai-settings-get :ask-user)))
    (and (consp value) (keywordp (car value)) value)))

(defun pai-ask-user-config-set (key value)
  "Persist KEY as VALUE in the project `:ask-user' settings plist."
  (pai-settings-set :ask-user
                    (plist-put (copy-sequence (pai-ask-user-config)) key value)
                    'project))

(defun pai-ask-user--timeout ()
  "Return the answer timeout in seconds, or nil when questions never expire."
  (let* ((configured (plist-get (pai-ask-user-config) :timeout))
         (seconds (if (numberp configured) configured pai-ask-user-timeout)))
    (and (numberp seconds) (> seconds 0) seconds)))

;;;; Requests

(cl-defstruct (pai-ask-user-request (:constructor pai-ask-user--request-create))
  "One question being asked of the user.
The struct is the dialog's single source of truth: the `vui' tree renders
from it and re-renders whenever a slot changes, so state survives the
detour through the separate editor buffer that composes custom answers."
  id question details mode options other-label
  selection other                       ; live answer state
  last-index                            ; option answered last time, if any
  buffer instance edit-buffer           ; user interface
  chat-buffer run on-done               ; agent plumbing
  deadline timer done)

(defvar pai-ask-user--pending (make-hash-table :test 'equal)
  "Map of tool-call id to the live `pai-ask-user-request' it is waiting on.")

(defvar-local pai-ask-user--request nil
  "The `pai-ask-user-request' this dialog buffer renders.")

(defun pai-ask-user-pending ()
  "Return the pending requests, oldest first."
  (sort (hash-table-values pai-ask-user--pending)
        (lambda (a b) (string< (pai-ask-user-request-id a)
                               (pai-ask-user-request-id b)))))

;;;; Arguments

(defun pai-ask-user--trim (value)
  "Return VALUE trimmed, or nil when it is not a non-blank string."
  (and (stringp value)
       (let ((trimmed (string-trim value)))
         (and (not (string-empty-p trimmed)) trimmed))))

(defun pai-ask-user--normalize-options (options)
  "Return OPTIONS as a list of (:label :value :description) plists.
OPTIONS is the decoded JSON array (a list or vector of plists).  Options
without a label are dropped, and a missing value defaults to the label."
  (delq nil
        (mapcar (lambda (option)
                  (let ((label (pai-ask-user--trim (plist-get option :label))))
                    (when label
                      (list :label label
                            :value (or (pai-ask-user--trim (plist-get option :value))
                                       label)
                            :description (pai-ask-user--trim
                                          (plist-get option :description))))))
                (append options nil))))

(defun pai-ask-user--other-label (options)
  "Return the label for the custom-answer entry alongside OPTIONS."
  (if (cl-some (lambda (option)
                 (string-equal-ignore-case (plist-get option :label) "other"))
               options)
      "Other (custom)"
    "Other"))

(defun pai-ask-user--mode (options multi)
  "Return the question mode for OPTIONS and the MULTI flag."
  (cond ((null options) 'text)
        ((pai-truthy multi) 'multi-select)
        (t 'single-select)))

;;;; Answers and results

(defun pai-ask-user--answer-rank (answer)
  "Return the sort rank of ANSWER: options in order, then other, then text."
  (pcase (plist-get answer :type)
    ("option" (or (plist-get answer :index) 0))
    ("other" most-positive-fixnum)
    (_ (1+ most-positive-fixnum))))

(defun pai-ask-user--sort-answers (answers)
  "Return ANSWERS ordered by their option index, custom answers last."
  (sort (copy-sequence answers)
        (lambda (a b) (< (pai-ask-user--answer-rank a)
                         (pai-ask-user--answer-rank b)))))

(defun pai-ask-user--format-answer (answer)
  "Return the model-facing rendering of ANSWER."
  (pcase (plist-get answer :type)
    ("option" (format "%d. %s" (plist-get answer :index) (plist-get answer :label)))
    ("other" (concat "Other: " (plist-get answer :label)))
    (_ (plist-get answer :label))))

(defun pai-ask-user--details (status question details mode answers &optional message)
  "Return the structured result payload for the model and the session log."
  (append (list :status status
                :question question
                :mode (symbol-name mode)
                :answers (or answers '()))
          (when details (list :context details))
          (when message (list :message message))))

(defun pai-ask-user--result (text status question details mode answers &optional message)
  "Return a tool result with TEXT and a structured payload built from the rest."
  (list :content (list (pai-text text))
        :is-error :false
        :details (pai-ask-user--details status question details mode answers message)))

(defun pai-ask-user--answered-text (mode answers)
  "Return the result text for ANSWERS given in MODE."
  (pcase mode
    ('text (let ((label (plist-get (car answers) :label)))
             (if (and label (not (string-empty-p label)))
                 (concat "User answered: " label)
               "User submitted an empty response")))
    ('single-select (concat "User selected: "
                            (pai-ask-user--format-answer (car answers))))
    (_ (concat "User selected:\n"
               (mapconcat (lambda (a) (concat "- " (pai-ask-user--format-answer a)))
                          answers "\n")))))

(defun pai-ask-user--answered-result (req answers)
  "Return the tool result answering REQ with ANSWERS."
  (let ((mode (pai-ask-user-request-mode req))
        (answers (pai-ask-user--sort-answers answers)))
    (pai-ask-user--result (pai-ask-user--answered-text mode answers)
                          "answered"
                          (pai-ask-user-request-question req)
                          (pai-ask-user-request-details req)
                          mode answers)))

(defun pai-ask-user--cancelled-result (req &optional message)
  "Return the tool result for REQ cancelled with an optional MESSAGE."
  (let ((message (or message "User cancelled the question")))
    (pai-ask-user--result message "cancelled"
                          (pai-ask-user-request-question req)
                          (pai-ask-user-request-details req)
                          (pai-ask-user-request-mode req)
                          nil message)))

(defun pai-ask-user--unavailable-result (question details mode)
  "Return the result used when QUESTION cannot be asked interactively."
  (let ((message "ask_user_question requires an interactive session"))
    (pai-ask-user--result message "unavailable" question details mode nil message)))

(defun pai-ask-user--option-answer (req index)
  "Return the answer plist for option INDEX (1-based) of REQ."
  (let ((option (nth (1- index) (pai-ask-user-request-options req))))
    (when option
      (list :type "option" :label (plist-get option :label)
            :value (plist-get option :value) :index index))))

(defun pai-ask-user--other-answer (text)
  "Return the answer plist for the custom answer TEXT."
  (list :type "other" :label text :value text))

;;;; Buffers and windows

(defun pai-ask-user--short (text width)
  "Return TEXT's first line, truncated to WIDTH columns."
  (truncate-string-to-width (car (split-string (or text "") "\n" t)) width nil nil t))

(defun pai-ask-user--wrap (text)
  "Return Markdown TEXT rendered, with long lines wrapped.
The layout the model wrote is kept: line breaks, lists, tables and fenced
code (syntax highlighted in its language's major mode) survive, and only
prose lines longer than `pai-ask-user-fill-column' are wrapped."
  (string-trim-right
   (pai-markdown-fill (pai-markdown-render (or text "")) pai-ask-user-fill-column)))

(defun pai-ask-user--under-prose (string face)
  "Add FACE beneath the prose of STRING, leaving code blocks alone."
  (let ((pos 0) (len (length string)))
    (while (< pos len)
      (let ((next (next-single-property-change pos 'pai-md-verbatim string len)))
        (unless (get-text-property pos 'pai-md-verbatim string)
          (add-face-text-property pos next face t string))
        (setq pos next))))
  string)

(defun pai-ask-user-rich (text &optional face)
  "Return a vui node showing Markdown TEXT, with FACE under its prose."
  (let ((string (copy-sequence (pai-ask-user--wrap text))))
    (vui-text (if face (pai-ask-user--under-prose string face) string))))

(defun pai-ask-user-inline (text &optional face)
  "Return one-line TEXT with inline Markdown (code, bold, links) rendered.
FACE, when given, is added beneath the inline styling."
  (let ((string (copy-sequence (pai-md--render-inline (or text "")))))
    (when face (add-face-text-property 0 (length string) face t string))
    string))

(defun pai-ask-user--display (buffer)
  "Show BUFFER using `pai-ask-user-display-action' and return its window."
  (let ((window (display-buffer buffer pai-ask-user-display-action)))
    (when (and window pai-ask-user-select-window (window-live-p window))
      (select-window window))
    window))

(defun pai-ask-user--quit-buffer (buffer)
  "Kill BUFFER and restore whatever its windows displayed before."
  (when (buffer-live-p buffer)
    (dolist (window (get-buffer-window-list buffer nil t))
      (ignore-errors (quit-restore-window window 'bury)))
    (kill-buffer buffer)))

(defun pai-ask-user--return-focus (req)
  "Select REQ's session window again, if it is still on screen."
  (when pai-ask-user-return-focus
    (let ((chat (pai-ask-user-request-chat-buffer req)))
      (when (buffer-live-p chat)
        (when-let ((window (get-buffer-window chat 0)))
          (ignore-errors (select-window window)))))))

;;;; Transcript note

(declare-function pai--render-note "pai-ui")
(defvar pai--output-marker)

(defun pai-ask-user--echo (req result)
  "Note REQ's outcome, taken from RESULT, in the session transcript."
  (let ((chat (pai-ask-user-request-chat-buffer req))
        (details (plist-get result :details)))
    (when (and pai-ask-user-echo-answer (buffer-live-p chat))
      (with-current-buffer chat
        (when (and (derived-mode-p 'pai-mode) pai--output-marker)
          (let* ((answers (plist-get details :answers))
                 (lines (if answers
                            (mapcar (lambda (a)
                                      (concat "   → " (pai-ask-user--format-answer a)))
                                    answers)
                          (list (concat "   → " (or (plist-get details :message)
                                                    "no answer"))))))
            (pai--render-note
             (string-join (cons (concat "❓ " (pai-ask-user-request-question req))
                                lines)
                          "\n"))))))))

;;;; Memory
;; Scoped to the session buffer, so answers never leak between sessions and
;; nothing has to be persisted or invalidated.

(defvar-local pai-ask-user--memory nil
  "Hash of question signature to the answer last given in this session.")

(defun pai-ask-user--signature (req)
  "Return the key REQ is remembered under.
The options are part of it, so a reworded choice list is a new question."
  (string-join (cons (pai-ask-user-request-question req)
                     (mapcar (lambda (o) (plist-get o :label))
                             (pai-ask-user-request-options req)))
               "\0"))

(defun pai-ask-user--remember (req answers)
  "Record ANSWERS as REQ's answer in its session buffer."
  (let ((chat (pai-ask-user-request-chat-buffer req)))
    (when (and pai-ask-user-remember-answers (buffer-live-p chat))
      (with-current-buffer chat
        (unless pai-ask-user--memory
          (setq pai-ask-user--memory (make-hash-table :test 'equal)))
        (puthash (pai-ask-user--signature req)
                 (list :selection (delq nil (mapcar (lambda (a) (plist-get a :index)) answers))
                       :other (cl-loop for a in answers
                                       when (equal (plist-get a :type) "other")
                                       return (plist-get a :label))
                       :text (cl-loop for a in answers
                                      when (equal (plist-get a :type) "text")
                                      return (plist-get a :label)))
                 pai-ask-user--memory)))))

(defun pai-ask-user--recall (req)
  "Return the answer last given to REQ in its session buffer, or nil."
  (let ((chat (pai-ask-user-request-chat-buffer req)))
    (when (and pai-ask-user-remember-answers (buffer-live-p chat))
      (with-current-buffer chat
        (and pai-ask-user--memory
             (gethash (pai-ask-user--signature req) pai-ask-user--memory))))))

(defun pai-ask-user--restore (req)
  "Prime REQ with the answer it was given last time, if any."
  (when-let ((previous (pai-ask-user--recall req)))
    (pcase (pai-ask-user-request-mode req)
      ('multi-select
       (setf (pai-ask-user-request-selection req) (plist-get previous :selection)
             (pai-ask-user-request-other req) (plist-get previous :other)))
      ('single-select
       (setf (pai-ask-user-request-last-index req) (car (plist-get previous :selection))))
      (_ nil))
    previous))

;;;; Lifecycle

(defun pai-ask-user--finish (req result)
  "Complete REQ with RESULT exactly once, tearing its user interface down."
  (unless (pai-ask-user-request-done req)
    (setf (pai-ask-user-request-done req) t)
    ;; Answering usually happens *in* the dialog buffer, which is about to be
    ;; killed: step into the session first, so tearing the dialog down cannot
    ;; strand the caller in whatever buffer Emacs happens to pick.
    (save-current-buffer
      (let ((chat (pai-ask-user-request-chat-buffer req)))
        (when (buffer-live-p chat) (set-buffer chat)))
      (remhash (pai-ask-user-request-id req) pai-ask-user--pending)
      (when-let ((timer (pai-ask-user-request-timer req)))
        (cancel-timer timer))
      (setf (pai-ask-user-request-timer req) nil)
      (pai-ask-user--quit-buffer (pai-ask-user-request-edit-buffer req))
      (pai-ask-user--quit-buffer (pai-ask-user-request-buffer req))
      (setf (pai-ask-user-request-edit-buffer req) nil
            (pai-ask-user-request-instance req) nil)
      (pai-ask-user--echo req result)
      (pai-ask-user--return-focus req)
      (let ((on-done (pai-ask-user-request-on-done req)))
        (setf (pai-ask-user-request-on-done req) nil)
        (when on-done (funcall on-done result))))))

(defun pai-ask-user--answer (req answers)
  "Finish REQ with ANSWERS."
  (pai-ask-user--remember req answers)
  (pai-ask-user--finish req (pai-ask-user--answered-result req answers)))

(defun pai-ask-user--cancel (req &optional message)
  "Finish REQ as cancelled, with an optional MESSAGE."
  (pai-ask-user--finish req (pai-ask-user--cancelled-result req message)))

(defun pai-ask-user-cancel-all (&optional message)
  "Cancel every pending question, reporting MESSAGE to the model."
  (dolist (req (pai-ask-user-pending))
    (pai-ask-user--cancel req message)))

(defun pai-ask-user--abandoned-p (req)
  "Return non-nil when nothing is waiting for REQ's answer any more."
  (let ((chat (pai-ask-user-request-chat-buffer req))
        (run (pai-ask-user-request-run req)))
    (or (and chat (not (buffer-live-p chat)))
        (and run (pai-agent-aborted-p run)))))

(defun pai-ask-user--watch (req)
  "Cancel REQ when its run went away or it waited too long."
  (condition-case err
      (cond
       ((pai-ask-user-request-done req)
        (when-let ((timer (pai-ask-user-request-timer req))) (cancel-timer timer)))
       ((pai-ask-user--abandoned-p req)
        (pai-ask-user--cancel req "The question was cancelled: the run ended before it was answered"))
       ((let ((deadline (pai-ask-user-request-deadline req)))
          (and deadline (> (float-time) deadline)))
        (pai-ask-user--cancel
         req (format "The question timed out after %s seconds without an answer"
                     (pai-ask-user--timeout)))))
    (error (message "pai-ask-user: %s" (error-message-string err)))))

;;;; Editor buffer (free-form answers)

(defvar-local pai-ask-user--edit-submit nil "Function called with the composed answer.")
(defvar-local pai-ask-user--edit-cancel nil "Function called when composing is abandoned.")
(defvar-local pai-ask-user--edit-done nil "Non-nil once this editor was submitted or cancelled.")

(defvar pai-ask-user-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pai-ask-user-edit-submit)
    (define-key map (kbd "C-c C-k") #'pai-ask-user-edit-cancel)
    map)
  "Keymap for `pai-ask-user-edit-mode'.")

(define-derived-mode pai-ask-user-edit-mode text-mode "pai-answer"
  "Major mode for composing a free-form answer to a pai question."
  (setq-local header-line-format
              "Answer the question — C-c C-c submit · C-c C-k cancel"))

(defun pai-ask-user--edit-finish (submitted)
  "Close this editor buffer, running the submit hook when SUBMITTED."
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (submit pai-ask-user--edit-submit)
        (cancel pai-ask-user--edit-cancel)
        (buffer (current-buffer)))
    (setq pai-ask-user--edit-done t)
    (pai-ask-user--quit-buffer buffer)
    (if submitted
        (when submit (funcall submit (string-trim text)))
      (when cancel (funcall cancel)))))

(defun pai-ask-user-edit-submit ()
  "Submit the answer composed in this buffer."
  (interactive)
  (pai-ask-user--edit-finish t))

(defun pai-ask-user-edit-cancel ()
  "Abandon the answer composed in this buffer."
  (interactive)
  (pai-ask-user--edit-finish nil))

(defun pai-ask-user--edit-killed ()
  "Treat killing an unfinished editor buffer as cancelling it."
  (unless pai-ask-user--edit-done
    (setq pai-ask-user--edit-done t)
    (when pai-ask-user--edit-cancel (funcall pai-ask-user--edit-cancel))))

(defun pai-ask-user--open-editor (req title initial on-submit on-cancel)
  "Open an editor buffer for REQ titled TITLE, primed with INITIAL.
ON-SUBMIT receives the trimmed text; ON-CANCEL is called when the user
backs out.  Only one editor is open per request at a time."
  (pai-ask-user--quit-buffer (pai-ask-user-request-edit-buffer req))
  (let ((buffer (generate-new-buffer
                 (format "*pai answer: %s*" (pai-ask-user--short title 40)))))
    (with-current-buffer buffer
      (pai-ask-user-edit-mode)
      (when initial (insert initial))
      (setq pai-ask-user--edit-submit on-submit
            pai-ask-user--edit-cancel on-cancel
            pai-ask-user--edit-done nil)
      (add-hook 'kill-buffer-hook #'pai-ask-user--edit-killed nil t))
    (setf (pai-ask-user-request-edit-buffer req) buffer)
    (pai-ask-user--display buffer)
    buffer))

;;;; Dialog state

(defun pai-ask-user--refresh (req)
  "Re-render REQ's dialog from the request struct."
  (let ((instance (pai-ask-user-request-instance req))
        (buffer (pai-ask-user-request-buffer req)))
    (when (and instance (buffer-live-p buffer))
      (with-current-buffer buffer (vui-rerender instance)))))

(defun pai-ask-user--toggle (req index)
  "Toggle option INDEX of multi-select REQ."
  (let ((selection (pai-ask-user-request-selection req)))
    (setf (pai-ask-user-request-selection req)
          (if (memq index selection) (delq index (copy-sequence selection))
            (cons index selection))))
  (pai-ask-user--refresh req))

(defun pai-ask-user--multi-answers (req)
  "Return the answers currently selected in multi-select REQ."
  (append (delq nil (mapcar (lambda (index) (pai-ask-user--option-answer req index))
                            (sort (copy-sequence (pai-ask-user-request-selection req)) #'<)))
          (when-let ((other (pai-ask-user-request-other req)))
            (list (pai-ask-user--other-answer other)))))

(defun pai-ask-user--submit-multi (req)
  "Submit multi-select REQ, or complain when nothing is selected."
  (let ((answers (pai-ask-user--multi-answers req)))
    (if answers
        (pai-ask-user--answer req answers)
      (message "Select at least one answer before submitting"))))

(defun pai-ask-user--edit-other (req)
  "Compose a custom answer for REQ in an editor buffer."
  (pai-ask-user--open-editor
   req (pai-ask-user-request-question req) (pai-ask-user-request-other req)
   (lambda (text)
     (unless (pai-ask-user-request-done req)
       (setf (pai-ask-user-request-edit-buffer req) nil)
       (cond
        ((string-empty-p text)
         (setf (pai-ask-user-request-other req) nil)
         (pai-ask-user--refresh req)
         (pai-ask-user--focus-dialog req))
        ((eq (pai-ask-user-request-mode req) 'single-select)
         (pai-ask-user--answer req (list (pai-ask-user--other-answer text))))
        (t
         (setf (pai-ask-user-request-other req) text)
         (pai-ask-user--refresh req)
         (pai-ask-user--focus-dialog req)))))
   (lambda ()
     (unless (pai-ask-user-request-done req)
       (setf (pai-ask-user-request-edit-buffer req) nil)
       (pai-ask-user--focus-dialog req)))))

(defun pai-ask-user--clear-other (req)
  "Drop REQ's custom answer."
  (setf (pai-ask-user-request-other req) nil)
  (pai-ask-user--refresh req))

(defun pai-ask-user--focus-dialog (req)
  "Show REQ's dialog buffer again."
  (when (buffer-live-p (pai-ask-user-request-buffer req))
    (pai-ask-user--display (pai-ask-user-request-buffer req))))

;;;; Dialog rendering

(defun pai-ask-user--option-rows (req index option multi)
  "Return the vui nodes rendering OPTION at 1-based INDEX of REQ.
MULTI selects checkbox rows over single-choice buttons."
  (let* ((label (pai-ask-user-inline (format "%d. %s" index (plist-get option :label))))
         (description (plist-get option :description))
         (row (if multi
                  (vui-checkbox
                   :key (format "option-%d" index)
                   :checked (and (memq index (pai-ask-user-request-selection req)) t)
                   :label label
                   :on-change (lambda (_value) (pai-ask-user--toggle req index)))
                (vui-hstack
                 (vui-button label
                   :key (format "option-%d" index)
                   :on-click (lambda ()
                               (pai-ask-user--answer
                                req (list (pai-ask-user--option-answer req index)))))
                 (when (eq index (pai-ask-user-request-last-index req))
                   (vui-muted "  (your answer last time)"))))))
    (delq nil (list row
                    (when description
                      (vui-text (pai-ask-user-inline (concat "     " description)
                                                     'vui-muted)))))))

(defun pai-ask-user--other-row (req multi)
  "Return the vui node offering REQ's custom answer.
MULTI renders it as a toggle, single-select as a button."
  (let ((other (pai-ask-user-request-other req))
        (label (pai-ask-user-request-other-label req)))
    (if multi
        (vui-checkbox
         :key "option-other"
         :checked (and other t)
         :label (if other (format "%s — %s" label other) label)
         :on-change (lambda (_value)
                      (if other (pai-ask-user--clear-other req)
                        (pai-ask-user--edit-other req))))
      (vui-button (concat label "…")
        :key "option-other"
        :on-click (lambda () (pai-ask-user--edit-other req))))))

(defun pai-ask-user--hint (multi)
  "Return the key hint line for a dialog, MULTI or not."
  (if multi
      "1-9 or RET toggle · o custom answer · TAB move · C-c C-c submit · C-c C-k cancel"
    "1-9 or RET select · o custom answer · TAB move · C-c C-k cancel"))

(vui-defcomponent pai-ask-user-dialog (request)
  "Render REQUEST, a `pai-ask-user-request', as a question dialog."
  :render
  (let* ((multi (eq (pai-ask-user-request-mode request) 'multi-select))
         (details (pai-ask-user-request-details request))
         (options (pai-ask-user-request-options request)))
    (vui-vstack
     :spacing 1
     (pai-ask-user-rich (pai-ask-user-request-question request) 'vui-heading-1)
     (when details (pai-ask-user-rich details 'vui-muted))
     (apply #'vui-vstack
            (append (cl-loop for option in options
                             for index from 1
                             append (pai-ask-user--option-rows request index option multi))
                    (list (pai-ask-user--other-row request multi))))
     (apply #'vui-hstack
            (delq nil
                  (list (when multi
                          (vui-button "Submit"
                            :key "submit"
                            :on-click (lambda () (pai-ask-user--submit-multi request))))
                        (vui-button "Cancel"
                          :key "cancel"
                          :on-click (lambda () (pai-ask-user--cancel request))))))
     (when (and multi (null (pai-ask-user--multi-answers request)))
       (vui-warning "Select at least one answer before submitting."))
     (vui-muted (pai-ask-user--hint multi)))))

;;;; Dialog buffer

(defun pai-ask-user--choose (index)
  "Choose or toggle option INDEX in the dialog shown in the current buffer."
  (let ((req pai-ask-user--request))
    (cond
     ((null req) (message "No question in this buffer"))
     ((null (nth (1- index) (pai-ask-user-request-options req)))
      (message "No option %d" index))
     ((eq (pai-ask-user-request-mode req) 'multi-select)
      (pai-ask-user--toggle req index))
     (t (pai-ask-user--answer req (list (pai-ask-user--option-answer req index)))))))

(defun pai-ask-user-dialog-other ()
  "Write a custom answer to the question shown in this buffer."
  (interactive)
  (when pai-ask-user--request (pai-ask-user--edit-other pai-ask-user--request)))

(defun pai-ask-user-dialog-submit ()
  "Submit the answers selected in this buffer."
  (interactive)
  (when-let ((req pai-ask-user--request))
    (if (eq (pai-ask-user-request-mode req) 'multi-select)
        (pai-ask-user--submit-multi req)
      (message "Press 1-9 or RET on a choice to answer"))))

(defun pai-ask-user-dialog-cancel ()
  "Cancel the question shown in this buffer."
  (interactive)
  (when pai-ask-user--request (pai-ask-user--cancel pai-ask-user--request)))

(defvar pai-ask-user-dialog-mode-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 9)
      (let ((index (1+ i)))
        (define-key map (kbd (number-to-string index))
                    (lambda () (interactive) (pai-ask-user--choose index)))))
    (define-key map (kbd "o") #'pai-ask-user-dialog-other)
    (define-key map (kbd "C-c C-c") #'pai-ask-user-dialog-submit)
    (define-key map (kbd "C-c C-k") #'pai-ask-user-dialog-cancel)
    map)
  "Keymap for `pai-ask-user-dialog-mode'.")

(define-derived-mode pai-ask-user-dialog-mode vui-mode "pai-ask"
  "Major mode for a question the agent is waiting on an answer to.")

(defun pai-ask-user--dialog-killed ()
  "Treat killing an unanswered dialog buffer as cancelling the question."
  (when-let ((req pai-ask-user--request))
    (setf (pai-ask-user-request-buffer req) nil)
    (pai-ask-user--cancel req)))

(defun pai-ask-user--open-dialog (req)
  "Create, mount and show REQ's select dialog."
  (let ((buffer (generate-new-buffer
                 (format "*pai ask: %s*"
                         (pai-ask-user--short (pai-ask-user-request-question req) 40)))))
    (setf (pai-ask-user-request-buffer req) buffer)
    (with-current-buffer buffer
      (pai-ask-user-dialog-mode)
      (setq pai-ask-user--request req)
      (add-hook 'kill-buffer-hook #'pai-ask-user--dialog-killed nil t))
    ;; `vui-mount' ends with `switch-to-buffer', which would take over the
    ;; selected window (and fail outright when it is dedicated).  Routing it
    ;; through `display-buffer' keeps the dialog in a window of its own.
    (let ((switch-to-buffer-obey-display-actions t)
          (display-buffer-overriding-action pai-ask-user-display-action))
      (setf (pai-ask-user-request-instance req)
            (vui-mount (vui-component 'pai-ask-user-dialog :request req)
                       (buffer-name buffer))))
    (pai-ask-user--goto-remembered req)
    (pai-ask-user--display buffer)
    buffer))

(defun pai-ask-user--goto-remembered (req)
  "Put point on the choice REQ was answered with last time.
Repeating a decision is then a single RET; changing it costs nothing."
  (let ((buffer (pai-ask-user-request-buffer req))
        (key (cond ((pai-ask-user-request-last-index req)
                    (format "option-%d" (pai-ask-user-request-last-index req)))
                   ((or (pai-ask-user-request-selection req)
                        (pai-ask-user-request-other req))
                    "submit"))))
    (when (and key (buffer-live-p buffer))
      (with-current-buffer buffer
        (ignore-errors (vui-goto-key key))))))

(defun pai-ask-user--open (req)
  "Show REQ and arm its watchdog."
  (when-let ((timeout (pai-ask-user--timeout)))
    (setf (pai-ask-user-request-deadline req) (+ (float-time) timeout)))
  (setf (pai-ask-user-request-timer req)
        (run-at-time 1 1 #'pai-ask-user--watch req))
  (let ((previous (pai-ask-user--restore req)))
    (if (eq (pai-ask-user-request-mode req) 'text)
        (pai-ask-user--open-editor
         req (pai-ask-user-request-question req) (plist-get previous :text)
         (lambda (text)
           (pai-ask-user--answer req (list (list :type "text" :label text :value text))))
         (lambda () (pai-ask-user--cancel req)))
      (pai-ask-user--open-dialog req)))
  req)

(defun pai-ask-user-show-pending ()
  "Redisplay the oldest question still waiting for an answer."
  (interactive)
  (if-let ((req (car (pai-ask-user-pending))))
      (pai-ask-user--display (or (pai-ask-user-request-edit-buffer req)
                                 (pai-ask-user-request-buffer req)))
    (message "No pending question")))

;;;; Tool

(defun pai-ask-user-available-p ()
  "Return non-nil when a question can actually be put to a user."
  (not noninteractive))

(defun pai-ask-user--execute (args ctx _on-update on-done)
  "Execute the `ask_user_question' tool with ARGS in CTX, finishing ON-DONE."
  (let* ((question (pai-ask-user--trim (plist-get args :question)))
         (details (pai-ask-user--trim (plist-get args :details)))
         (options (pai-ask-user--normalize-options (plist-get args :options)))
         (mode (pai-ask-user--mode options (plist-get args :multiSelect))))
    (cond
     ((null question)
      (funcall on-done (pai-tool-error-result
                        "ask_user_question requires a non-empty question")))
     ((not (pai-ask-user-available-p))
      (funcall on-done (pai-ask-user--unavailable-result question details mode)))
     (t
      (let ((req (pai-ask-user--request-create
                  :id (or (plist-get ctx :tool-call-id) (pai-uuidv7))
                  :question question :details details :mode mode :options options
                  :other-label (pai-ask-user--other-label options)
                  :chat-buffer (current-buffer)
                  :run (plist-get ctx :run)
                  :on-done on-done)))
        (puthash (pai-ask-user-request-id req) req pai-ask-user--pending)
        (condition-case err
            (pai-ask-user--open req)
          (error
           (pai-ask-user--finish
            req (pai-tool-error-result
                 (format "Failed to ask the question: %s" (error-message-string err))))))
        req)))))

(defconst pai-ask-user-guidelines
  '("Ask exactly one question per tool call."
    "If you need answers to multiple questions, make multiple separate ask_user_question tool calls instead of combining them into one prompt."
    "Users can always choose \"Other\" to provide a custom text answer when options are given."
    "Use multiSelect: true only when you need multiple answers to the same question."
    "If you recommend a specific option, make it the first option and add \"(Recommended)\" at the end of its label."
    "Prefer this tool over guessing when requirements, preferences, or implementation choices are unclear."
    "Use this tool when multiple valid implementation paths exist and the preferred path depends on a user choice.")
  "Guidelines appended to the tool description, mirroring the pi extension.")

(defconst pai-ask-user-tool
  (list
   :name "ask_user_question"
   :label "Ask user"
   :description
   (concat
    "Ask the user a single question and pause execution until they answer. "
    "Use this when requirements are ambiguous, user preferences are needed, a "
    "decision would materially affect implementation, or you need confirmation "
    "before proceeding. Ask exactly one question per tool call, and prefer "
    "multiple separate tool calls over bundling unrelated questions together.\n\n"
    "Guidelines:\n"
    (mapconcat (lambda (g) (concat "- " g)) pai-ask-user-guidelines "\n"))
   :prompt-snippet
   "ask_user_question: ask the user exactly one clarifying, missing-requirement, preference, or decision question and wait for the answer"
   :prompt-guidelines pai-ask-user-guidelines
   ;; Questions are put to a human one at a time: never race two dialogs.
   :execution-mode 'sequential
   :parameters
   (pai-object-schema
    (list :question
          (pai-string-schema
           "The single question to ask the user. Ask exactly one question per tool call.")
          :details
          (pai-string-schema
           "Optional extra context or instructions shown under the question.")
          :options
          (pai-array-schema
           "Optional multiple-choice options. Omit or pass an empty array for free-form text input. The user can always choose Other and type a custom answer."
           (pai-object-schema
            (list :label
                  (pai-string-schema
                   "Display label for the option. If you recommend an option, place it first and append \"(Recommended)\" to the label.")
                  :value
                  (pai-string-schema
                   "Optional machine-readable value returned for the option. Defaults to the label.")
                  :description
                  (pai-string-schema "Optional extra detail shown below the option."))
            '("label")))
          :multiSelect
          (pai-boolean-schema
           "Set to true to allow multiple answers to be selected for a question."))
    '("question"))
   :execute #'pai-ask-user--execute)
  "The `ask_user_question' tool definition.")

;;;; Registration

(defun pai-ask-user--command (_args _ctx)
  "Slash-command handler redisplaying a pending question."
  (if (pai-ask-user-pending)
      (progn (pai-ask-user-show-pending) nil)
    (list :message "No pending question")))

(pai-register-extension
 (lambda (api)
   (pai-ext-register-tool api pai-ask-user-tool)
   (pai-ext-register-command api "ask"
                             :description "Show the question the agent is waiting on"
                             :handler #'pai-ask-user--command)
   ;; A session that is going away is not going to answer anything.
   (pai-ext-on api 'session-shutdown
               (lambda (_event _ctx)
                 (pai-ask-user-cancel-all
                  "The question was cancelled: the session ended"))))
 "ask-user")

;;;; Settings screen
;; Soft dependency: the extension works without the vui settings screen.
(with-eval-after-load 'pai-settings-ui
  (pai-settings-ui-register-section 'ask-user "Ask user" 47)
  (pai-settings-ui-register-subsection 'ask-user 'questions "Questions" 10)
  (pai-settings-ui-register-item
   'ask-user 'questions
   :key :ask-user-timeout :type 'number :label "Answer timeout (s)"
   :doc "Cancel an unanswered question after this many seconds (0 = never)"
   :get (lambda () (or (pai-ask-user--timeout) 0))
   :set (lambda (v) (pai-ask-user-config-set :timeout (or v 0))))
  (pai-settings-ui-register-item
   'ask-user 'questions
   :key :ask-user-select-window :type 'boolean :label "Focus the question"
   :doc "Select the question window as soon as the agent asks"
   :get (lambda () pai-ask-user-select-window)
   :set (lambda (v) (setq pai-ask-user-select-window (and v t))))
  (pai-settings-ui-register-item
   'ask-user 'questions
   :key :ask-user-echo-answer :type 'boolean :label "Echo answers"
   :doc "Note the question and your answer in the transcript"
   :get (lambda () pai-ask-user-echo-answer)
   :set (lambda (v) (setq pai-ask-user-echo-answer (and v t))))
  (pai-settings-ui-register-item
   'ask-user 'questions
   :key :ask-user-remember :type 'boolean :label "Remember answers"
   :doc "Start a repeated question on the answer you gave last time"
   :get (lambda () pai-ask-user-remember-answers)
   :set (lambda (v) (setq pai-ask-user-remember-answers (and v t)))))

(provide 'pai-ask-user)
;;; pai-ask-user.el ends here
