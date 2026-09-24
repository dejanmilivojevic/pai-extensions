;;; pai-ask-user-test.el --- Tests for the ask_user_question tool -*- lexical-binding: t; -*-

;;; Commentary:

;; The dialog is a buffer, so the whole question/answer round trip is
;; testable without a terminal: the harness executes the tool, drives the
;; resulting dialog (or editor) buffer with the same functions the keys and
;; buttons call, and inspects the tool result the model would have seen.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-ask-user)

;;;; Harness

(defmacro pai-ask-user-test--with-ui (&rest body)
  "Run BODY as if in an interactive session, without touching any window."
  (declare (indent 0))
  `(let ((noninteractive nil)
         (vui-render-delay nil)
         (pai-ask-user-select-window nil)
         (pai-ask-user-return-focus nil)
         (pai-ask-user-timeout nil)
         (pai-ask-user-display-action '(display-buffer-no-window
                                        (allow-no-window . t))))
     (unwind-protect (progn ,@body)
       (pai-ask-user-cancel-all)
       (clrhash pai-ask-user--pending))))

(defun pai-ask-user-test--ask (args)
  "Execute the tool with ARGS; return (REQUEST . RESULT-CELL).
RESULT-CELL's car holds the tool result once the question is finished."
  (let* ((cell (list nil))
         (req (pai-ask-user--execute
               args (list :tool-call-id (pai-uuidv7))
               nil (lambda (result) (setcar cell result)))))
    (cons req cell)))

(defun pai-ask-user-test--text (result)
  "Return RESULT's content as plain text."
  (pai-content-text (plist-get result :content)))

(defun pai-ask-user-test--options ()
  "Return a three-option list as the model would send it."
  (list (list :label "Rewrite it" :description "Start from scratch")
        (list :label "Patch it" :value "patch")
        (list :label "Leave it")))

;;;; Argument normalization

(ert-deftest pai-ask-user-test-normalize-options ()
  "Labels are trimmed, values default to the label, blank options vanish."
  (let ((options (pai-ask-user--normalize-options
                  (list (list :label "  Keep  " :description " why ")
                        (list :label "Drop" :value " drop-it ")
                        (list :label "   ")
                        (list :value "no-label")))))
    (should (equal options
                   '((:label "Keep" :value "Keep" :description "why")
                     (:label "Drop" :value "drop-it" :description nil))))))

(ert-deftest pai-ask-user-test-normalize-options-vector ()
  "A decoded JSON array may arrive as a vector."
  (should (equal (pai-ask-user--normalize-options (vector (list :label "A")))
                 '((:label "A" :value "A" :description nil)))))

(ert-deftest pai-ask-user-test-other-label ()
  "The custom entry is disambiguated when an option is called \"Other\"."
  (should (equal (pai-ask-user--other-label '((:label "A"))) "Other"))
  (should (equal (pai-ask-user--other-label '((:label "other"))) "Other (custom)")))

(ert-deftest pai-ask-user-test-mode ()
  "The mode follows from the options and the multiSelect flag."
  (should (eq (pai-ask-user--mode nil nil) 'text))
  (should (eq (pai-ask-user--mode nil t) 'text))
  (should (eq (pai-ask-user--mode '((:label "A")) nil) 'single-select))
  (should (eq (pai-ask-user--mode '((:label "A")) :false) 'single-select))
  (should (eq (pai-ask-user--mode '((:label "A")) t) 'multi-select)))

;;;; Result shaping

(ert-deftest pai-ask-user-test-sort-and-format-answers ()
  "Options keep their order, the custom answer sorts last."
  (let* ((answers (list (list :type "other" :label "Something else")
                        (list :type "option" :label "B" :index 2)
                        (list :type "option" :label "A" :index 1)))
         (sorted (pai-ask-user--sort-answers answers)))
    (should (equal (mapcar #'pai-ask-user--format-answer sorted)
                   '("1. A" "2. B" "Other: Something else")))))

(ert-deftest pai-ask-user-test-answered-text ()
  "Result text matches the wording the pi extension sends."
  (should (equal (pai-ask-user--answered-text 'text '((:type "text" :label "yes")))
                 "User answered: yes"))
  (should (equal (pai-ask-user--answered-text 'text '((:type "text" :label "")))
                 "User submitted an empty response"))
  (should (equal (pai-ask-user--answered-text
                  'single-select '((:type "option" :label "A" :index 1)))
                 "User selected: 1. A"))
  (should (equal (pai-ask-user--answered-text
                  'multi-select '((:type "option" :label "A" :index 1)
                                  (:type "other" :label "X")))
                 "User selected:\n- 1. A\n- Other: X")))

(ert-deftest pai-ask-user-test-unavailable-without-ui ()
  "Batch sessions report the question as unavailable instead of hanging."
  (let ((result nil))
    (pai-ask-user--execute '(:question "Ready?") nil nil
                           (lambda (r) (setq result r)))
    (should (equal (plist-get (plist-get result :details) :status) "unavailable"))
    (should (string-match-p "interactive" (pai-ask-user-test--text result)))
    (should (zerop (hash-table-count pai-ask-user--pending)))))

(ert-deftest pai-ask-user-test-rejects-empty-question ()
  "An empty question is a tool error, not a dialog."
  (let ((result nil))
    (pai-ask-user--execute '(:question "  ") nil nil (lambda (r) (setq result r)))
    (should (pai-truthy (plist-get result :is-error)))))

;;;; Single select

(ert-deftest pai-ask-user-test-single-select-choice ()
  "Choosing an option answers the call and tears the dialog down."
  (pai-ask-user-test--with-ui
    (let* ((asked (pai-ask-user-test--ask
                   (list :question "How should we fix it?"
                         :details "Pick one"
                         :options (pai-ask-user-test--options))))
           (req (car asked)) (cell (cdr asked))
           (buffer (pai-ask-user-request-buffer req)))
      (should (eq (pai-ask-user-request-mode req) 'single-select))
      (should (buffer-live-p buffer))
      (should (equal (hash-table-count pai-ask-user--pending) 1))
      ;; the dialog shows every option, the custom entry and the hint
      (let ((shown (with-current-buffer buffer (buffer-string))))
        (should (string-match-p "How should we fix it?" shown))
        (should (string-match-p "1\\. Rewrite it" shown))
        (should (string-match-p "Start from scratch" shown))
        (should (string-match-p "3\\. Leave it" shown))
        (should (string-match-p "Other…" shown)))
      (with-current-buffer buffer (pai-ask-user--choose 2))
      (let ((result (car cell)))
        (should (equal (pai-ask-user-test--text result) "User selected: 2. Patch it"))
        (let ((details (plist-get result :details)))
          (should (equal (plist-get details :status) "answered"))
          (should (equal (plist-get details :mode) "single-select"))
          (should (equal (plist-get details :context) "Pick one"))
          (should (equal (plist-get details :answers)
                         '((:type "option" :label "Patch it" :value "patch" :index 2))))))
      (should-not (buffer-live-p buffer))
      (should (zerop (hash-table-count pai-ask-user--pending))))))

(ert-deftest pai-ask-user-test-single-select-other ()
  "\"Other\" opens an editor whose text becomes the answer."
  (pai-ask-user-test--with-ui
    (let* ((asked (pai-ask-user-test--ask
                   (list :question "Which library?"
                         :options (pai-ask-user-test--options))))
           (req (car asked)) (cell (cdr asked)))
      (pai-ask-user--edit-other req)
      (let ((editor (pai-ask-user-request-edit-buffer req)))
        (should (buffer-live-p editor))
        (with-current-buffer editor
          (insert "  something entirely different  ")
          (pai-ask-user-edit-submit)))
      (let ((result (car cell)))
        (should (equal (pai-ask-user-test--text result)
                       "User selected: Other: something entirely different"))
        (should (equal (plist-get (plist-get result :details) :answers)
                       '((:type "other" :label "something entirely different"
                                :value "something entirely different"))))))))

(ert-deftest pai-ask-user-test-cancel-by-killing-the-dialog ()
  "Killing the dialog cancels the question rather than stranding the run."
  (pai-ask-user-test--with-ui
    (let* ((asked (pai-ask-user-test--ask
                   (list :question "Proceed?" :options (pai-ask-user-test--options))))
           (req (car asked)) (cell (cdr asked)))
      (kill-buffer (pai-ask-user-request-buffer req))
      (let ((result (car cell)))
        (should (equal (pai-ask-user-test--text result) "User cancelled the question"))
        (should (equal (plist-get (plist-get result :details) :status) "cancelled")))
      (should (zerop (hash-table-count pai-ask-user--pending))))))

;;;; Multi select

(ert-deftest pai-ask-user-test-multi-select ()
  "Toggling several options and a custom answer submits them in order."
  (pai-ask-user-test--with-ui
    (let* ((asked (pai-ask-user-test--ask
                   (list :question "Which ones?"
                         :options (pai-ask-user-test--options)
                         :multiSelect t)))
           (req (car asked)) (cell (cdr asked))
           (buffer (pai-ask-user-request-buffer req)))
      (should (eq (pai-ask-user-request-mode req) 'multi-select))
      ;; nothing selected yet: submitting warns instead of answering
      (with-current-buffer buffer
        (should (string-match-p "Select at least one answer" (buffer-string)))
        (pai-ask-user-dialog-submit))
      (should-not (car cell))
      (with-current-buffer buffer
        (pai-ask-user--choose 3)
        (pai-ask-user--choose 1)
        (pai-ask-user--choose 3)
        (pai-ask-user--choose 3))
      (pai-ask-user--edit-other req)
      (with-current-buffer (pai-ask-user-request-edit-buffer req)
        (insert "and something else")
        (pai-ask-user-edit-submit))
      ;; the editor closed but the dialog is still up, now showing the answer
      (should (buffer-live-p buffer))
      (should (string-match-p "and something else"
                              (with-current-buffer buffer (buffer-string))))
      (with-current-buffer buffer (pai-ask-user-dialog-submit))
      (let ((result (car cell)))
        (should (equal (pai-ask-user-test--text result)
                       "User selected:\n- 1. Rewrite it\n- 3. Leave it\n- Other: and something else"))
        (should (equal (plist-get (plist-get result :details) :mode) "multi-select"))))))

(ert-deftest pai-ask-user-test-multi-select-untoggle-other ()
  "Toggling a set custom answer off clears it without opening an editor."
  (pai-ask-user-test--with-ui
    (let* ((asked (pai-ask-user-test--ask
                   (list :question "Which ones?"
                         :options (pai-ask-user-test--options)
                         :multiSelect t)))
           (req (car asked)))
      (setf (pai-ask-user-request-other req) "custom")
      (pai-ask-user--clear-other req)
      (should-not (pai-ask-user-request-other req))
      (should-not (pai-ask-user-request-edit-buffer req)))))

;;;; Free-form text

(ert-deftest pai-ask-user-test-text-mode ()
  "A question without options is answered in an editor buffer."
  (pai-ask-user-test--with-ui
    (let* ((asked (pai-ask-user-test--ask (list :question "Name the release?")))
           (req (car asked)) (cell (cdr asked))
           (editor (pai-ask-user-request-edit-buffer req)))
      (should (eq (pai-ask-user-request-mode req) 'text))
      (should-not (pai-ask-user-request-buffer req))
      (should (buffer-live-p editor))
      (with-current-buffer editor
        (insert "Tangerine\nwith a second line")
        (pai-ask-user-edit-submit))
      (let ((result (car cell)))
        (should (equal (pai-ask-user-test--text result)
                       "User answered: Tangerine\nwith a second line"))
        (should (equal (plist-get (plist-get result :details) :mode) "text"))))))

(ert-deftest pai-ask-user-test-text-mode-cancel ()
  "Abandoning the editor cancels the question."
  (pai-ask-user-test--with-ui
    (let* ((asked (pai-ask-user-test--ask (list :question "Name the release?")))
           (req (car asked)) (cell (cdr asked)))
      (with-current-buffer (pai-ask-user-request-edit-buffer req)
        (pai-ask-user-edit-cancel))
      (should (equal (plist-get (plist-get (car cell) :details) :status) "cancelled")))))

;;;; Watchdog

(ert-deftest pai-ask-user-test-watchdog-cancels-aborted-run ()
  "Interrupting the run releases the question instead of leaving it open."
  (pai-ask-user-test--with-ui
    (let ((run (pai-run-create :origin-buffer (current-buffer))))
      (let* ((cell (list nil))
             (req (pai-ask-user--execute
                   (list :question "Still there?" :options (pai-ask-user-test--options))
                   (list :tool-call-id "call-1" :run run)
                   nil (lambda (result) (setcar cell result)))))
        (pai-ask-user--watch req)
        (should-not (car cell))
        (setf (pai-run-aborted run) t)
        (pai-ask-user--watch req)
        (should (equal (plist-get (plist-get (car cell) :details) :status) "cancelled"))
        (should (string-match-p "run ended" (pai-ask-user-test--text (car cell))))
        (should-not (buffer-live-p (pai-ask-user-request-buffer req)))))))

(ert-deftest pai-ask-user-test-watchdog-times-out ()
  "A question past its deadline is cancelled with a timeout message."
  (pai-ask-user-test--with-ui
    (let* ((pai-ask-user-timeout 30)
           (asked (pai-ask-user-test--ask
                   (list :question "Ready?" :options (pai-ask-user-test--options))))
           (req (car asked)) (cell (cdr asked)))
      (should (pai-ask-user-request-deadline req))
      (setf (pai-ask-user-request-deadline req) (- (float-time) 1))
      (pai-ask-user--watch req)
      (should (string-match-p "timed out" (pai-ask-user-test--text (car cell)))))))

(ert-deftest pai-ask-user-test-cancel-all ()
  "Ending a session cancels every question it left open."
  (pai-ask-user-test--with-ui
    (let ((first (pai-ask-user-test--ask (list :question "One?")))
          (second (pai-ask-user-test--ask (list :question "Two?"))))
      (should (equal (hash-table-count pai-ask-user--pending) 2))
      (pai-ask-user-cancel-all "session ended")
      (should (zerop (hash-table-count pai-ask-user--pending)))
      (dolist (asked (list first second))
        (should (equal (pai-ask-user-test--text (car (cdr asked))) "session ended"))))))

;;;; Transcript note

(defmacro pai-ask-user-test--with-session (buf &rest body)
  "Bind BUF to a real faux-backed pai session buffer and run BODY."
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "pai-ask" t)))
          (pai-directory (expand-file-name ".pai-state" dir))
          (pai-default-model "faux")
          (,buf nil))
     (unwind-protect
         (progn
           (pai-ext-reset)
           (setq ,buf (get-buffer-create (generate-new-buffer-name "*pai-ask-test*")))
           (with-current-buffer ,buf
             (setq default-directory dir)
             (pai--setup dir))
           ,@body)
       (when (buffer-live-p ,buf) (kill-buffer ,buf))
       (ignore-errors (delete-directory dir t)))))

(ert-deftest pai-ask-user-test-echoes-the-answer-into-the-transcript ()
  "An answered question leaves a findable note in the session buffer."
  (pai-ask-user-test--with-ui
    (pai-ask-user-test--with-session buf
      (with-current-buffer buf
        (let* ((asked (pai-ask-user-test--ask
                       (list :question "How should we fix it?"
                             :options (pai-ask-user-test--options))))
               (req (car asked)))
          (with-current-buffer (pai-ask-user-request-buffer req)
            (pai-ask-user--choose 2))
          (let ((transcript (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "❓ How should we fix it?" transcript))
            (should (string-match-p "→ 2\\. Patch it" transcript))))))))

(ert-deftest pai-ask-user-test-echoes-a-cancellation ()
  "A cancelled question says so in the transcript instead of going quiet."
  (pai-ask-user-test--with-ui
    (pai-ask-user-test--with-session buf
      (with-current-buffer buf
        (let* ((asked (pai-ask-user-test--ask
                       (list :question "Proceed?"
                             :options (pai-ask-user-test--options))))
               (req (car asked)))
          (pai-ask-user--cancel req)
          (should (string-match-p "→ User cancelled the question"
                                  (buffer-substring-no-properties (point-min) (point-max)))))))))

(ert-deftest pai-ask-user-test-echo-can-be-turned-off ()
  "Nothing is written to the transcript when the echo is disabled."
  (pai-ask-user-test--with-ui
    (pai-ask-user-test--with-session buf
      (with-current-buffer buf
        (let* ((pai-ask-user-echo-answer nil)
               (asked (pai-ask-user-test--ask (list :question "Quiet?")))
               (req (car asked)))
          (with-current-buffer (pai-ask-user-request-edit-buffer req)
            (insert "yes")
            (pai-ask-user-edit-submit))
          (should-not (string-match-p "❓" (buffer-substring-no-properties
                                           (point-min) (point-max)))))))))

;;;; Memory

(ert-deftest pai-ask-user-test-remembers-a-multi-select-answer ()
  "Asking the same question again starts from the previous answer."
  (pai-ask-user-test--with-ui
    (with-temp-buffer
      (let ((args (list :question "Which ones?"
                        :options (pai-ask-user-test--options)
                        :multiSelect t)))
        (let ((req (car (pai-ask-user-test--ask args))))
          (with-current-buffer (pai-ask-user-request-buffer req)
            (pai-ask-user--choose 1)
            (pai-ask-user--choose 3))
          (setf (pai-ask-user-request-other req) "plus this")
          (with-current-buffer (pai-ask-user-request-buffer req)
            (pai-ask-user-dialog-submit)))
        (let ((again (car (pai-ask-user-test--ask args))))
          (should (equal (sort (copy-sequence (pai-ask-user-request-selection again)) #'<)
                         '(1 3)))
          (should (equal (pai-ask-user-request-other again) "plus this"))
          ;; the dialog comes up with those boxes already ticked
          (should (string-match-p "\\[X\\] 1\\. Rewrite it"
                                  (with-current-buffer (pai-ask-user-request-buffer again)
                                    (buffer-string)))))))))

(ert-deftest pai-ask-user-test-marks-the-previous-single-choice ()
  "A repeated single-select question flags and focuses last time's choice."
  (pai-ask-user-test--with-ui
    (with-temp-buffer
      (let ((args (list :question "How should we fix it?"
                        :options (pai-ask-user-test--options))))
        (let ((req (car (pai-ask-user-test--ask args))))
          (with-current-buffer (pai-ask-user-request-buffer req)
            (pai-ask-user--choose 2)))
        (let* ((again (car (pai-ask-user-test--ask args)))
               (buffer (pai-ask-user-request-buffer again)))
          (should (equal (pai-ask-user-request-last-index again) 2))
          (with-current-buffer buffer
            (should (string-match-p "2\\. Patch it.*your answer last time" (buffer-string)))
            ;; point waits on that choice, so RET repeats it
            (should (string-match-p "Patch it"
                                    (buffer-substring (line-beginning-position)
                                                      (line-end-position))))))))))

(ert-deftest pai-ask-user-test-primes-a-repeated-text-question ()
  "A repeated free-form question opens with the previous answer to edit."
  (pai-ask-user-test--with-ui
    (with-temp-buffer
      (let ((args (list :question "Name the release?")))
        (let ((req (car (pai-ask-user-test--ask args))))
          (with-current-buffer (pai-ask-user-request-edit-buffer req)
            (insert "Tangerine")
            (pai-ask-user-edit-submit)))
        (let ((again (car (pai-ask-user-test--ask args))))
          (should (equal (with-current-buffer (pai-ask-user-request-edit-buffer again)
                           (buffer-string))
                         "Tangerine")))))))

(ert-deftest pai-ask-user-test-memory-is-per-question-and-per-session ()
  "Different options, or another session buffer, mean a fresh question."
  (pai-ask-user-test--with-ui
    (with-temp-buffer
      (let ((req (car (pai-ask-user-test--ask
                       (list :question "Which ones?"
                             :options (pai-ask-user-test--options))))))
        (with-current-buffer (pai-ask-user-request-buffer req)
          (pai-ask-user--choose 1)))
      ;; same question text, different choices on offer
      (let ((other-options (car (pai-ask-user-test--ask
                                 (list :question "Which ones?"
                                       :options (list (list :label "Something else")))))))
        (should-not (pai-ask-user-request-last-index other-options))))
    ;; a different session buffer remembers nothing
    (with-temp-buffer
      (let ((elsewhere (car (pai-ask-user-test--ask
                             (list :question "Which ones?"
                                   :options (pai-ask-user-test--options))))))
        (should-not (pai-ask-user-request-last-index elsewhere))))))

(ert-deftest pai-ask-user-test-memory-can-be-turned-off ()
  "With recall disabled a repeated question starts blank every time."
  (pai-ask-user-test--with-ui
    (with-temp-buffer
      (let ((pai-ask-user-remember-answers nil)
            (args (list :question "How should we fix it?"
                        :options (pai-ask-user-test--options))))
        (let ((req (car (pai-ask-user-test--ask args))))
          (with-current-buffer (pai-ask-user-request-buffer req)
            (pai-ask-user--choose 2)))
        (should-not (pai-ask-user-request-last-index
                     (car (pai-ask-user-test--ask args))))))))

;;;; The agent loop

(ert-deftest pai-ask-user-test-holds-the-turn-open ()
  "The run waits on the question and resumes with the answer as the result."
  (pai-ask-user-test--with-ui
    (pai-faux-reset)
    (pai-faux-push '(:tool-calls ((:id "c1" :name "ask_user_question"
                                   :arguments (:question "Ship it?"
                                               :options ((:label "Yes")
                                                         (:label "No")))))
                     :stop-reason tool-use)
                   '(:text "shipping" :stop-reason stop))
    (let ((messages nil) (done nil)
          ;; exercise execution directly, not the deferred-schema reveal
          (pai-defer-extension-tools nil))
      (pai-agent-run (list (pai-user-message "ready?"))
                     (pai-context nil (list pai-ask-user-tool))
                     (list :model (pai-model "faux"))
                     #'ignore
                     (lambda (msgs) (setq messages msgs done t)))
      ;; the faux provider is synchronous, so only the question can be
      ;; holding the turn open here
      (should-not done)
      (let ((req (car (pai-ask-user-pending))))
        (should req)
        (with-current-buffer (pai-ask-user-request-buffer req)
          (pai-ask-user--choose 1)))
      (should done)
      ;; user, assistant(tool call), tool-result, assistant(text)
      (should (= (length messages) 4))
      (let ((result (nth 2 messages)))
        (should (pai-tool-result-message-p result))
        (should (equal (pai-content-text (plist-get result :content))
                       "User selected: 1. Yes"))
        (should (equal (plist-get (plist-get result :details) :status) "answered")))
      (should (equal (pai-content-text (pai-message-content (nth 3 messages)))
                     "shipping")))))

;;;; Registration

(ert-deftest pai-ask-user-test-tool-declaration ()
  "The tool advertises the upstream parameter schema."
  (let* ((schema (plist-get pai-ask-user-tool :parameters))
         (props (plist-get schema :properties)))
    (should (equal (plist-get pai-ask-user-tool :name) "ask_user_question"))
    (should (equal (plist-get schema :required) '("question")))
    (should (plist-get props :question))
    (should (plist-get props :details))
    (should (plist-get props :multiSelect))
    (should (equal (plist-get (plist-get (plist-get props :options) :items) :required)
                   '("label")))
    ;; questions are asked one at a time, never in a parallel batch
    (should (eq (plist-get pai-ask-user-tool :execution-mode) 'sequential))))

(ert-deftest pai-ask-user-test-registers-into-an-instance ()
  "Loading the extension registers the tool and the `/ask' command."
  (with-temp-buffer
    (pai-ext-initialize-instance)
    (pai-register-extension
     (lambda (api)
       (pai-ext-register-tool api pai-ask-user-tool)
       (pai-ext-register-command api "ask" :description "x"
                                 :handler #'pai-ask-user--command))
     "ask-user-test")
    (should (pai-tool-get "ask_user_question"))
    (should (pai-command-get "ask"))
    (should (equal (plist-get (pai-ask-user--command "" nil) :message)
                   "No pending question"))))

(provide 'pai-ask-user-test)
;;; pai-ask-user-test.el ends here
