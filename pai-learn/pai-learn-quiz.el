;;; pai-learn-quiz.el --- The graded `quiz' tool of the learning system -*- lexical-binding: t; -*-

;;; Commentary:

;; Port of the `quiz' extension of https://github.com/amosblomqvist/learn.
;;
;; `ask_user_question' (pai-ask-user) asks questions with no right answer;
;; `quiz' asks questions that have one.  The model supplies the options, the
;; correct option value(s) and an explanation; the user answers in a dialog
;; built from the same pieces as the ask-user dialog (vui, the same display
;; action, keys and window handling), is graded on the spot (✓/✗, the correct
;; answer, the explanation), and the verdict goes back to the model.
;;
;; Differences from ask_user_question:
;;   - options are shuffled for display (grading is keyed by value, so the
;;     order the user sees is always the order that is graded);
;;   - there is no free-text answer: an "I don't know" choice is always added
;;     so the user can report a genuine gap instead of guessing;
;;   - any answer may carry a free-text note (`n'), reported only when given;
;;   - after answering the dialog shows the feedback and stays open until the
;;     user continues, so the explanation is actually read.
;;
;; Keys: 1-9 answer/toggle, `?' I don't know, `n' note, TAB move, RET
;; activate, C-c C-c submit (multi-select) or continue, C-c C-k cancel.
;;
;; The tool is not registered globally: `pai-learn' registers it only in
;; sessions that are being taught (see `pai-learn-activate').

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-core)
(require 'pai-tools)
(require 'pai-agent)
(require 'pai-ask-user)
(require 'vui)
(require 'vui-components)

(declare-function pai--render-note "pai-ui")
(defvar pai--output-marker)

(defconst pai-learn-quiz-dont-know-label "I don't know"
  "Label of the always-present opt-out choice.")

(cl-defstruct (pai-learn-quiz (:constructor pai-learn-quiz--create))
  "One graded question put to the user.
Like `pai-ask-user-request', the struct is the dialog's single source of
truth; the vui tree renders from it."
  id question details mode options correct explanation
  selection dont-know note          ; live answer state
  phase                             ; `select' or `feedback'
  buffer instance                   ; user interface
  chat-buffer run on-done           ; agent plumbing
  timer result done)

(defvar pai-learn-quiz--pending (make-hash-table :test 'equal)
  "Map of tool-call id to the live `pai-learn-quiz' it is waiting on.")

(defvar-local pai-learn-quiz--current nil
  "The `pai-learn-quiz' this dialog buffer renders.")

(defvar pai-learn-quiz-question-functions nil
  "Hook run with a quiz just before it is shown (options in display order).")

;;;; Arguments

(defun pai-learn-quiz--shuffle (list)
  "Return a shuffled copy of LIST (Fisher-Yates)."
  (let ((v (vconcat list)))
    (cl-loop for i from (1- (length v)) downto 1
             do (let ((j (random (1+ i))))
                  (cl-rotatef (aref v i) (aref v j))))
    (append v nil)))

(defun pai-learn-quiz--coerce-values (value)
  "Return VALUE, the model's correctAnswer, as a list of strings.
Accepts a string, a list or vector of strings, or a JSON-encoded array
delivered as a string."
  (cond
   ((vectorp value) (pai-learn-quiz--coerce-values (append value nil)))
   ((listp value) (delq nil (mapcar #'pai-ask-user--trim value)))
   ((stringp value)
    (let ((trimmed (string-trim value)))
      (or (and (string-prefix-p "[" trimmed) (string-suffix-p "]" trimmed)
               (let ((parsed (ignore-errors (json-parse-string trimmed :array-type 'list))))
                 (and (listp parsed) (mapcar (lambda (v) (format "%s" v)) parsed))))
          (and (not (string-empty-p trimmed)) (list trimmed)))))
   (t nil)))

(defun pai-learn-quiz--resolve (values options)
  "Return the sorted 1-based indices of VALUES among OPTIONS.
Signal an error naming the offending value when one matches no option."
  (let ((indices '()))
    (dolist (value values)
      (let ((index (cl-position value options
                                :test (lambda (v o) (equal v (plist-get o :value))))))
        (unless index
          (error "correctAnswer %S does not match any option value (%s)"
                 value (mapconcat (lambda (o) (format "%S" (plist-get o :value)))
                                  options ", ")))
        (push (1+ index) indices)))
    (sort (delete-dups indices) #'<)))

(defun pai-learn-quiz--check-options (options)
  "Signal an error unless OPTIONS are at least two, with distinct values."
  (when (< (length options) 2)
    (error "quiz requires at least two options"))
  (let ((seen '()))
    (dolist (o options)
      (when (member (plist-get o :value) seen)
        (error "duplicate option value %S" (plist-get o :value)))
      (push (plist-get o :value) seen))))

;;;; Grading and results

(defun pai-learn-quiz--correct-p (quiz)
  "Return non-nil when QUIZ's answer is exactly the correct set."
  (and (not (pai-learn-quiz-dont-know quiz))
       (equal (sort (copy-sequence (pai-learn-quiz-selection quiz)) #'<)
              (pai-learn-quiz-correct quiz))))

(defun pai-learn-quiz--label (quiz index)
  "Return \"INDEX. label\" for option INDEX of QUIZ."
  (format "%d. %s" index
          (or (plist-get (nth (1- index) (pai-learn-quiz-options quiz)) :label)
              "(unknown)")))

(defun pai-learn-quiz--details (quiz status &optional message)
  "Return the structured result payload of QUIZ with STATUS."
  (let ((answered (equal status "answered")))
    (append
     (list :status status
           :question (pai-learn-quiz-question quiz)
           :mode (symbol-name (pai-learn-quiz-mode quiz))
           :options (cl-loop for o in (pai-learn-quiz-options quiz)
                             for i from 1
                             collect (list :index i :label (plist-get o :label)))
           :correct-indices (pai-learn-quiz-correct quiz))
     (when (pai-learn-quiz-details quiz)
       (list :context (pai-learn-quiz-details quiz)))
     (when answered
       (list :answers (mapcar (lambda (i)
                                (let ((o (nth (1- i) (pai-learn-quiz-options quiz))))
                                  (list :index i :label (plist-get o :label)
                                        :value (plist-get o :value))))
                              (sort (copy-sequence (pai-learn-quiz-selection quiz)) #'<))
             :correct (if (pai-learn-quiz--correct-p quiz) t :false)
             :dont-know (if (pai-learn-quiz-dont-know quiz) t :false)
             :explanation (pai-learn-quiz-explanation quiz)))
     (when (and answered (pai-learn-quiz-note quiz))
       (list :note (pai-learn-quiz-note quiz)))
     (when message (list :message message)))))

(defun pai-learn-quiz--answered-text (quiz)
  "Return the model-facing verdict for answered QUIZ."
  (let ((correct (mapconcat (lambda (i) (pai-learn-quiz--label quiz i))
                            (pai-learn-quiz-correct quiz) ", "))
        (note (pai-learn-quiz-note quiz)))
    (concat
     (if (pai-learn-quiz-dont-know quiz)
         "User selected \"I don't know\" — they did not attempt an answer (a genuine knowledge gap, not a wrong guess)."
       (format "User answered %s.\nSelected: %s"
               (if (pai-learn-quiz--correct-p quiz) "correctly" "incorrectly")
               (mapconcat (lambda (i) (pai-learn-quiz--label quiz i))
                          (sort (copy-sequence (pai-learn-quiz-selection quiz)) #'<)
                          ", ")))
     "\nCorrect: " correct
     (if note (concat "\nUser's note: " note) "")
     "\nExplanation: " (pai-learn-quiz-explanation quiz))))

(defun pai-learn-quiz--result (quiz status text &optional message)
  "Return the tool result for QUIZ with STATUS, TEXT and optional MESSAGE."
  (list :content (list (pai-text text))
        :is-error :false
        :details (pai-learn-quiz--details quiz status message)))

;;;; Lifecycle

(defun pai-learn-quiz--echo (quiz)
  "Note QUIZ's outcome in its session transcript."
  (let ((chat (pai-learn-quiz-chat-buffer quiz)))
    (when (and pai-ask-user-echo-answer (buffer-live-p chat))
      (with-current-buffer chat
        (when (and (derived-mode-p 'pai-mode) pai--output-marker)
          (pai--render-note
           (concat "🎓 " (pai-learn-quiz-question quiz) "\n   → "
                   (cond ((not (equal (plist-get (plist-get (pai-learn-quiz-result quiz) :details)
                                                 :status)
                                      "answered"))
                          "cancelled")
                         ((pai-learn-quiz-dont-know quiz) "I don't know")
                         ((pai-learn-quiz--correct-p quiz) "✓ correct")
                         (t "✗ incorrect"))
                   (if (pai-learn-quiz-note quiz)
                       (concat " · note: " (pai-learn-quiz-note quiz))
                     ""))))))))

(defun pai-learn-quiz--finish (quiz result)
  "Complete QUIZ with RESULT exactly once, tearing its dialog down."
  (unless (pai-learn-quiz-done quiz)
    (setf (pai-learn-quiz-done quiz) t
          (pai-learn-quiz-result quiz) result)
    (save-current-buffer
      (let ((chat (pai-learn-quiz-chat-buffer quiz)))
        (when (buffer-live-p chat) (set-buffer chat)))
      (remhash (pai-learn-quiz-id quiz) pai-learn-quiz--pending)
      (when-let ((timer (pai-learn-quiz-timer quiz))) (cancel-timer timer))
      (setf (pai-learn-quiz-timer quiz) nil)
      (let ((buffer (pai-learn-quiz-buffer quiz)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (setq pai-learn-quiz--current nil))
          (pai-ask-user--quit-buffer buffer)))
      (setf (pai-learn-quiz-instance quiz) nil)
      (pai-learn-quiz--echo quiz)
      (let ((chat (pai-learn-quiz-chat-buffer quiz)))
        (when (and pai-ask-user-return-focus (buffer-live-p chat))
          (when-let ((window (get-buffer-window chat 0)))
            (ignore-errors (select-window window)))))
      (let ((on-done (pai-learn-quiz-on-done quiz)))
        (setf (pai-learn-quiz-on-done quiz) nil)
        (when on-done (funcall on-done result))))))

(defun pai-learn-quiz--cancel (quiz &optional message)
  "Finish QUIZ as cancelled with an optional MESSAGE."
  (let ((message (or message "User cancelled the quiz")))
    (pai-learn-quiz--finish quiz (pai-learn-quiz--result quiz "cancelled" message message))))

(defun pai-learn-quiz-cancel-all (&optional message)
  "Cancel every pending quiz, reporting MESSAGE to the model."
  (dolist (quiz (hash-table-values pai-learn-quiz--pending))
    (pai-learn-quiz--cancel quiz message)))

(defun pai-learn-quiz--watch (quiz)
  "Cancel QUIZ once nothing is waiting for its answer any more."
  (condition-case err
      (let ((chat (pai-learn-quiz-chat-buffer quiz))
            (run (pai-learn-quiz-run quiz)))
        (cond
         ((pai-learn-quiz-done quiz)
          (when-let ((timer (pai-learn-quiz-timer quiz))) (cancel-timer timer)))
         ((or (and chat (not (buffer-live-p chat)))
              (and run (pai-agent-aborted-p run)))
          (pai-learn-quiz--cancel
           quiz "The quiz was cancelled: the run ended before it was answered"))))
    (error (message "pai-learn-quiz: %s" (error-message-string err)))))

;;;; Answering

(defun pai-learn-quiz--refresh (quiz)
  "Re-render QUIZ's dialog."
  (let ((instance (pai-learn-quiz-instance quiz))
        (buffer (pai-learn-quiz-buffer quiz)))
    (when (and instance (buffer-live-p buffer))
      (with-current-buffer buffer (vui-rerender instance)))))

(defun pai-learn-quiz--grade (quiz)
  "Move QUIZ to its feedback phase; the result is delivered on continue."
  (setf (pai-learn-quiz-phase quiz) 'feedback)
  (pai-learn-quiz--refresh quiz)
  (let ((buffer (pai-learn-quiz-buffer quiz)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (ignore-errors (vui-goto-key "continue"))
        ;; Keep the question in view above the verdict.
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-start window (point-min))
          (set-window-point window (point)))))))

(defun pai-learn-quiz--choose (quiz index)
  "Answer (single-select) or toggle (multi-select) option INDEX of QUIZ."
  (when (eq (pai-learn-quiz-phase quiz) 'select)
    (if (eq (pai-learn-quiz-mode quiz) 'multi-select)
        (let ((selection (pai-learn-quiz-selection quiz)))
          (setf (pai-learn-quiz-dont-know quiz) nil
                (pai-learn-quiz-selection quiz)
                (if (memq index selection) (remq index selection)
                  (cons index selection)))
          (pai-learn-quiz--refresh quiz))
      (setf (pai-learn-quiz-selection quiz) (list index)
            (pai-learn-quiz-dont-know quiz) nil)
      (pai-learn-quiz--grade quiz))))

(defun pai-learn-quiz--choose-dont-know (quiz)
  "Answer QUIZ with \"I don't know\" (exclusive of every other choice)."
  (when (eq (pai-learn-quiz-phase quiz) 'select)
    (setf (pai-learn-quiz-selection quiz) nil
          (pai-learn-quiz-dont-know quiz) t)
    (pai-learn-quiz--grade quiz)))

(defun pai-learn-quiz--submit (quiz)
  "Submit multi-select QUIZ, or complain when nothing is selected."
  (if (pai-learn-quiz-selection quiz)
      (pai-learn-quiz--grade quiz)
    (message "Select at least one answer (or ? for I don't know)")))

(defun pai-learn-quiz--continue (quiz)
  "Deliver QUIZ's graded result to the model."
  (pai-learn-quiz--finish
   quiz (pai-learn-quiz--result quiz "answered" (pai-learn-quiz--answered-text quiz))))

(defun pai-learn-quiz--edit-note (quiz)
  "Read a note for QUIZ in the minibuffer."
  (let ((note (string-trim (read-string "Note (optional, empty clears): "
                                        (pai-learn-quiz-note quiz)))))
    (setf (pai-learn-quiz-note quiz) (unless (string-empty-p note) note))
    (pai-learn-quiz--refresh quiz)))

;;;; Dialog rendering

(defun pai-learn-quiz--select-rows (quiz)
  "Return the vui nodes offering QUIZ's choices."
  (let ((multi (eq (pai-learn-quiz-mode quiz) 'multi-select)))
    (append
     (cl-loop
      for option in (pai-learn-quiz-options quiz)
      for index from 1
      for label = (pai-ask-user-inline (format "%d. %s" index (plist-get option :label)))
      collect
      (if multi
          (vui-checkbox :key (format "option-%d" index)
                        :checked (and (memq index (pai-learn-quiz-selection quiz)) t)
                        :label label
                        :on-change (lambda (_v) (pai-learn-quiz--choose quiz index)))
        (vui-button label
          :key (format "option-%d" index)
          :on-click (lambda () (pai-learn-quiz--choose quiz index))))
      when (plist-get option :description)
      collect (vui-text (pai-ask-user-inline (concat "     " (plist-get option :description))
                                             'vui-muted)))
     (list (vui-newline)
           (vui-button (concat "?  " pai-learn-quiz-dont-know-label)
             :key "dont-know"
             :on-click (lambda () (pai-learn-quiz--choose-dont-know quiz)))))))

(defun pai-learn-quiz--feedback-rows (quiz)
  "Return the vui nodes grading QUIZ."
  (let ((selected (pai-learn-quiz-selection quiz))
        (correct (pai-learn-quiz-correct quiz))
        (dont-know (pai-learn-quiz-dont-know quiz)))
    (append
     (cl-loop
      for index from 1 to (length (pai-learn-quiz-options quiz))
      for label = (pai-learn-quiz--label quiz index)
      for key = (memq index correct)
      for picked = (memq index selected)
      collect (vui-text
               (cond (key (pai-ask-user-inline (concat " ✓ " label) 'vui-success))
                     (picked (pai-ask-user-inline (concat " ✗ " label) 'vui-error))
                     (t (pai-ask-user-inline (concat "   " label) 'vui-muted)))))
     (list
      (vui-newline)
      (cond (dont-know (vui-warning "· You said: I don't know"))
            ((pai-learn-quiz--correct-p quiz) (vui-success "✓ Correct!"))
            (t (vui-error "✗ Incorrect.")))
      (when (pai-learn-quiz-note quiz)
        (vui-muted (concat "Your note: " (pai-learn-quiz-note quiz))))
      (vui-newline)
      (pai-ask-user-rich (pai-learn-quiz-explanation quiz))))))

(vui-defcomponent pai-learn-quiz-dialog (quiz)
  "Render QUIZ, a `pai-learn-quiz'."
  :render
  (let* ((feedback (eq (pai-learn-quiz-phase quiz) 'feedback))
         (multi (eq (pai-learn-quiz-mode quiz) 'multi-select)))
    (vui-vstack
     :spacing 1
     (pai-ask-user-rich (pai-learn-quiz-question quiz) 'vui-heading-1)
     (when (pai-learn-quiz-details quiz)
       (pai-ask-user-rich (pai-learn-quiz-details quiz) 'vui-muted))
     (apply #'vui-vstack
            (delq nil (if feedback
                          (pai-learn-quiz--feedback-rows quiz)
                        (pai-learn-quiz--select-rows quiz))))
     (unless feedback
       (vui-muted (concat "Note: " (or (pai-learn-quiz-note quiz) "(none — press n to add one)"))))
     (if feedback
         (vui-button "Continue"
           :key "continue"
           :on-click (lambda () (pai-learn-quiz--continue quiz)))
       (apply #'vui-hstack
              (delq nil
                    (list (when multi
                            (vui-button "Submit"
                              :key "submit"
                              :on-click (lambda () (pai-learn-quiz--submit quiz))))
                          (vui-button "Cancel"
                            :key "cancel"
                            :on-click (lambda () (pai-learn-quiz--cancel quiz)))))))
     (vui-muted
      (cond (feedback "RET or C-c C-c continue")
            (multi "1-9/RET toggle · ? I don't know · n note · C-c C-c submit · C-c C-k cancel")
            (t "1-9/RET answer · ? I don't know · n note · TAB move · C-c C-k cancel"))))))

;;;; Dialog buffer

(defmacro pai-learn-quiz--with-current (&rest body)
  "Run BODY with `quiz' bound to this buffer's quiz, if any."
  (declare (indent 0))
  `(let ((quiz pai-learn-quiz--current))
     (if (not quiz) (message "No quiz in this buffer")
       ,@body)))

(defun pai-learn-quiz-dialog-dont-know ()
  "Answer the quiz shown in this buffer with \"I don't know\"."
  (interactive)
  (pai-learn-quiz--with-current (pai-learn-quiz--choose-dont-know quiz)))

(defun pai-learn-quiz-dialog-note ()
  "Attach a note to the answer of the quiz shown in this buffer."
  (interactive)
  (pai-learn-quiz--with-current
    (if (eq (pai-learn-quiz-phase quiz) 'select)
        (pai-learn-quiz--edit-note quiz)
      (message "Already answered"))))

(defun pai-learn-quiz-dialog-submit ()
  "Submit the answer, or continue after the feedback."
  (interactive)
  (pai-learn-quiz--with-current
    (cond ((eq (pai-learn-quiz-phase quiz) 'feedback) (pai-learn-quiz--continue quiz))
          ((eq (pai-learn-quiz-mode quiz) 'multi-select) (pai-learn-quiz--submit quiz))
          (t (message "Press 1-9 or RET on a choice to answer")))))

(defun pai-learn-quiz-dialog-cancel ()
  "Cancel the quiz shown in this buffer (after grading: just continue)."
  (interactive)
  (pai-learn-quiz--with-current
    (if (eq (pai-learn-quiz-phase quiz) 'feedback)
        (pai-learn-quiz--continue quiz)
      (pai-learn-quiz--cancel quiz))))

(defvar pai-learn-quiz-dialog-mode-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 9)
      (let ((index (1+ i)))
        (define-key map (kbd (number-to-string index))
                    (lambda () (interactive)
                      (pai-learn-quiz--with-current
                        (if (nth (1- index) (pai-learn-quiz-options quiz))
                            (pai-learn-quiz--choose quiz index)
                          (message "No option %d" index)))))))
    (define-key map (kbd "?") #'pai-learn-quiz-dialog-dont-know)
    (define-key map (kbd "n") #'pai-learn-quiz-dialog-note)
    (define-key map (kbd "C-c C-c") #'pai-learn-quiz-dialog-submit)
    (define-key map (kbd "C-c C-k") #'pai-learn-quiz-dialog-cancel)
    map)
  "Keymap for `pai-learn-quiz-dialog-mode'.")

(define-derived-mode pai-learn-quiz-dialog-mode vui-mode "pai-quiz"
  "Major mode for a graded question the agent is waiting on.")

(defun pai-learn-quiz--dialog-killed ()
  "Treat killing an unfinished quiz buffer as cancelling (or continuing) it."
  (when-let ((quiz pai-learn-quiz--current))
    (setf (pai-learn-quiz-buffer quiz) nil)
    (if (eq (pai-learn-quiz-phase quiz) 'feedback)
        (pai-learn-quiz--continue quiz)
      (pai-learn-quiz--cancel quiz))))

(defun pai-learn-quiz--open (quiz)
  "Create, mount and show QUIZ's dialog; arm its watchdog."
  (let ((buffer (generate-new-buffer
                 (format "*pai quiz: %s*"
                         (pai-ask-user--short (pai-learn-quiz-question quiz) 40)))))
    (setf (pai-learn-quiz-buffer quiz) buffer
          (pai-learn-quiz-phase quiz) 'select
          (pai-learn-quiz-timer quiz) (run-at-time 1 1 #'pai-learn-quiz--watch quiz))
    (with-current-buffer buffer
      (pai-learn-quiz-dialog-mode)
      (setq pai-learn-quiz--current quiz)
      (add-hook 'kill-buffer-hook #'pai-learn-quiz--dialog-killed nil t))
    ;; Same window handling as the ask-user dialog: never take over the
    ;; selected (possibly dedicated) window.
    (let ((switch-to-buffer-obey-display-actions t)
          (display-buffer-overriding-action pai-ask-user-display-action))
      (setf (pai-learn-quiz-instance quiz)
            (vui-mount (vui-component 'pai-learn-quiz-dialog :quiz quiz)
                       (buffer-name buffer))))
    (with-current-buffer buffer (ignore-errors (vui-goto-key "option-1")))
    (pai-ask-user--display buffer)
    buffer))

(defun pai-learn-quiz-show-pending ()
  "Redisplay the quiz still waiting for an answer."
  (interactive)
  (if-let ((quiz (car (hash-table-values pai-learn-quiz--pending))))
      (pai-ask-user--display (pai-learn-quiz-buffer quiz))
    (message "No pending quiz")))

;;;; Tool

(defun pai-learn-quiz--execute (args ctx _on-update on-done)
  "Execute the `quiz' tool with ARGS in CTX, finishing through ON-DONE."
  (condition-case err
      (let* ((question (or (pai-ask-user--trim (plist-get args :question))
                           (error "quiz requires a non-empty question")))
             (explanation (or (pai-ask-user--trim (plist-get args :explanation))
                              (error "quiz requires an explanation")))
             (options (pai-ask-user--normalize-options (plist-get args :options)))
             (_ (pai-learn-quiz--check-options options))
             (options (if (eq (plist-get args :shuffle) :false) options
                        (pai-learn-quiz--shuffle options)))
             (values (or (pai-learn-quiz--coerce-values (plist-get args :correctAnswer))
                         (error "quiz requires correctAnswer")))
             (quiz (pai-learn-quiz--create
                    :id (or (plist-get ctx :tool-call-id) (pai-uuidv7))
                    :question question
                    :details (pai-ask-user--trim (plist-get args :details))
                    :mode (if (pai-truthy (plist-get args :multiSelect)) 'multi-select
                            'single-select)
                    :options options
                    :correct (pai-learn-quiz--resolve values options)
                    :explanation explanation
                    :chat-buffer (current-buffer)
                    :run (plist-get ctx :run)
                    :on-done on-done)))
        (if (not (pai-ask-user-available-p))
            (funcall on-done (pai-learn-quiz--result
                              quiz "unavailable" "quiz requires an interactive session"
                              "quiz requires an interactive session"))
          (puthash (pai-learn-quiz-id quiz) quiz pai-learn-quiz--pending)
          (run-hook-with-args 'pai-learn-quiz-question-functions quiz)
          (condition-case err
              (pai-learn-quiz--open quiz)
            (error (pai-learn-quiz--finish
                    quiz (pai-tool-error-result
                          (format "Failed to show the quiz: %s" (error-message-string err))))))
          quiz))
    (error (funcall on-done (pai-tool-error-result
                             (concat "quiz: " (error-message-string err)))))))

(defconst pai-learn-quiz-guidelines
  '("quiz is GRADED; ask_user_question is not. If the question has a correct answer, use quiz. For a preference, decision or open-ended input, use ask_user_question."
    "correctAnswer is REQUIRED: an array of option `value' strings (the label when no value is given), never position numbers. Single-select: exactly one value. A value matching no option is a hard error."
    "explanation is REQUIRED — say why the correct answer is correct. It is shown only after the user answers."
    "Multi-select (multiSelect: true, only when more than one option is correct) is graded as an exact-set match."
    "An \"I don't know\" choice is ALWAYS added automatically. Provide only real, gradable options (at least two); never add your own opt-out option."
    "A dontKnow result means the user honestly did not know and did not guess: a genuine gap to teach into, not a wrong answer."
    "Any answer may carry a free-text note from the user; when present it tells you what they were thinking — let it steer the follow-up."
    "Make each distractor a specific, believable misconception, so WHICH wrong answer is picked reveals WHICH part of the understanding is off. Every distractor must still be unambiguously wrong — no trick questions."
    "Don't let the correct answer stand out by form: keep options similar in length, specificity and phrasing, put no justification in any option, and bold nothing (or the parallel term in every option)."
    "Options are shuffled before display; pass shuffle: false only when order is meaningful (ordered values, \"all of the above\")."
    "Probe nuance with several quick quizzes, each adapted to the previous answer, rather than one giant question.")
  "Guidelines for the `quiz' tool, from the learn system.")

(defconst pai-learn-quiz-tool
  (list
   :name "quiz"
   :label "Quiz"
   :description
   (concat
    "Ask the user a GRADED multiple-choice question with a known correct answer, "
    "then grade it instantly and show feedback (✓/✗, the correct answer, your "
    "explanation). Use it to map what the learner already understands before "
    "teaching, and to check that each idea landed after teaching it. "
    "Unlike ask_user_question (no right answer), quiz always has one.\n\n"
    "Guidelines:\n"
    (mapconcat (lambda (g) (concat "- " g)) pai-learn-quiz-guidelines "\n"))
   :prompt-snippet "quiz: ask the user a graded multiple-choice question and give instant feedback"
   :prompt-guidelines pai-learn-quiz-guidelines
   ;; A quiz is a full schema from the start: its rules are what make the
   ;; questions diagnostic, so it must not be a stub.
   :deferred nil
   :execution-mode 'sequential
   :parameters
   (pai-object-schema
    (list :question (pai-string-schema "The question to ask. One question per call. Math in LaTeX ($...$).")
          :details (pai-string-schema "Optional context shown under the question (must not hint at the answer).")
          :options
          (pai-array-schema
           "The real, gradable options (at least two). Do not add an \"I don't know\" option."
           (pai-object-schema
            (list :label (pai-string-schema "Option text shown to the user.")
                  :value (pai-string-schema "Optional stable value used by correctAnswer. Defaults to the label.")
                  :description (pai-string-schema "Optional detail shown under the option."))
            '("label")))
          :multiSelect (pai-boolean-schema "True only when more than one option is correct (graded as an exact set).")
          :correctAnswer
          (pai-array-schema "Value(s) of the correct option(s). Single-select: exactly one."
                            (pai-string-schema "An option value."))
          :explanation (pai-string-schema "Why the correct answer is correct; shown after the user answers.")
          :shuffle (pai-boolean-schema "Shuffle options before display (default true)."))
    '("question" "options" "correctAnswer" "explanation"))
   :execute #'pai-learn-quiz--execute)
  "The `quiz' tool definition.")

(provide 'pai-learn-quiz)
;;; pai-learn-quiz.el ends here
