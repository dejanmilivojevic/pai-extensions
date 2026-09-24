;;; pai-learn-test.el --- Tests for the learning system extension -*- lexical-binding: t; -*-

;;; Commentary:

;; The quiz dialog is a buffer, so a whole question/grade/continue round trip
;; runs without a terminal, like the ask-user tests.  Rendering needs a
;; graphical Emacs with xwidgets and is not exercised here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-learn)

;;;; Harness

(defmacro pai-learn-test--with-ui (&rest body)
  "Run BODY as if interactive, without touching any window."
  (declare (indent 0))
  `(let ((noninteractive nil)
         (vui-render-delay nil)
         (pai-ask-user-select-window nil)
         (pai-ask-user-return-focus nil)
         (pai-ask-user-display-action '(display-buffer-no-window
                                        (allow-no-window . t))))
     (unwind-protect (progn ,@body)
       (pai-learn-quiz-cancel-all)
       (clrhash pai-learn-quiz--pending))))

(defun pai-learn-test--quiz (args)
  "Execute the quiz tool with ARGS; return (QUIZ . RESULT-CELL)."
  (let* ((cell (list nil))
         (quiz (pai-learn-quiz--execute
                args (list :tool-call-id (pai-uuidv7))
                nil (lambda (result) (setcar cell result)))))
    (cons quiz cell)))

(defun pai-learn-test--args (&rest extra)
  "Return quiz arguments for a planet question, plus EXTRA."
  (append extra
          (list :question "Closest planet to the Sun?"
                :options (vector (list :label "Mercury" :value "mercury")
                                 (list :label "Venus")
                                 (list :label "Mars"))
                :correctAnswer ["mercury"]
                :explanation "Mercury orbits closest."
                :shuffle :false)))

(defun pai-learn-test--text (result)
  "Return RESULT's content as text."
  (pai-content-text (plist-get result :content)))

;;;; Quiz arguments

(ert-deftest pai-learn-test-coerce-values ()
  "correctAnswer may be a string, a vector, a list or a JSON array string."
  (should (equal (pai-learn-quiz--coerce-values "a") '("a")))
  (should (equal (pai-learn-quiz--coerce-values ["a" " b "]) '("a" "b")))
  (should (equal (pai-learn-quiz--coerce-values "[\"a\", \"b\"]") '("a" "b")))
  (should (null (pai-learn-quiz--coerce-values ""))))

(ert-deftest pai-learn-test-resolve ()
  "Values resolve to sorted display indices; unknown values are errors."
  (let ((options '((:label "A" :value "a") (:label "B" :value "b") (:label "C" :value "c"))))
    (should (equal (pai-learn-quiz--resolve '("c" "a") options) '(1 3)))
    (should-error (pai-learn-quiz--resolve '("z") options))))

(ert-deftest pai-learn-test-shuffle-keeps-elements ()
  "Shuffling permutes without losing options."
  (let ((list (number-sequence 1 20)))
    (should (equal (sort (pai-learn-quiz--shuffle list) #'<) list))))

(ert-deftest pai-learn-test-invalid-arguments ()
  "Bad arguments come back as error results, without a dialog."
  (pai-learn-test--with-ui
    (let ((cell (cdr (pai-learn-test--quiz
                      (plist-put (pai-learn-test--args) :correctAnswer ["pluto"])))))
      (should (eq (plist-get (car cell) :is-error) t))
      (should (string-match-p "pluto" (pai-learn-test--text (car cell)))))
    (let ((cell (cdr (pai-learn-test--quiz
                      (plist-put (pai-learn-test--args) :options (vector (list :label "Only")))))))
      (should (string-match-p "at least two" (pai-learn-test--text (car cell)))))))

;;;; Quiz round trips

(ert-deftest pai-learn-test-single-correct ()
  "A right answer is graded after the user continues from the feedback."
  (pai-learn-test--with-ui
    (pcase-let ((`(,quiz . ,cell) (pai-learn-test--quiz (pai-learn-test--args))))
      (should (buffer-live-p (pai-learn-quiz-buffer quiz)))
      (pai-learn-quiz--choose quiz 1)
      (should (eq (pai-learn-quiz-phase quiz) 'feedback))
      (should-not (car cell))           ; waits for the user to read the feedback
      (pai-learn-quiz--continue quiz)
      (let* ((result (car cell)) (details (plist-get result :details)))
        (should (string-match-p "answered correctly" (pai-learn-test--text result)))
        (should (eq (plist-get details :correct) t))
        (should (equal (plist-get details :correct-indices) '(1)))
        (should-not (buffer-live-p (pai-learn-quiz-buffer quiz)))))))

(ert-deftest pai-learn-test-single-wrong-with-note ()
  "A wrong answer reports the selection, the key and the user's note."
  (pai-learn-test--with-ui
    (pcase-let ((`(,quiz . ,cell) (pai-learn-test--quiz (pai-learn-test--args))))
      (setf (pai-learn-quiz-note quiz) "confused by Venus")
      (pai-learn-quiz--choose quiz 2)
      (pai-learn-quiz--continue quiz)
      (let ((text (pai-learn-test--text (car cell))))
        (should (string-match-p "incorrectly" text))
        (should (string-match-p "Selected: 2. Venus" text))
        (should (string-match-p "Correct: 1. Mercury" text))
        (should (string-match-p "User's note: confused by Venus" text))
        (should (equal (plist-get (plist-get (car cell) :details) :note) "confused by Venus"))))))

(ert-deftest pai-learn-test-dont-know ()
  "\"I don't know\" is its own outcome, never a wrong answer."
  (pai-learn-test--with-ui
    (pcase-let ((`(,quiz . ,cell) (pai-learn-test--quiz (pai-learn-test--args))))
      (pai-learn-quiz--choose-dont-know quiz)
      (pai-learn-quiz--continue quiz)
      (let ((details (plist-get (car cell) :details)))
        (should (string-match-p "did not attempt" (pai-learn-test--text (car cell))))
        (should (eq (plist-get details :dont-know) t))
        (should (eq (plist-get details :correct) :false))))))

(ert-deftest pai-learn-test-multi-exact-set ()
  "Multi-select is graded as an exact set."
  (pai-learn-test--with-ui
    (pcase-let ((`(,quiz . ,cell)
                 (pai-learn-test--quiz
                  (pai-learn-test--args :multiSelect t :correctAnswer ["mercury" "Venus"]))))
      (pai-learn-quiz--choose quiz 1)
      (should (eq (pai-learn-quiz-phase quiz) 'select))
      (pai-learn-quiz--choose quiz 3)
      (pai-learn-quiz--choose quiz 3)   ; toggled off again
      (pai-learn-quiz--choose quiz 2)
      (pai-learn-quiz--submit quiz)
      (pai-learn-quiz--continue quiz)
      (should (eq (plist-get (plist-get (car cell) :details) :correct) t)))))

(ert-deftest pai-learn-test-cancel-and-kill ()
  "Cancelling or killing the dialog reports a cancelled quiz."
  (pai-learn-test--with-ui
    (pcase-let ((`(,quiz . ,cell) (pai-learn-test--quiz (pai-learn-test--args))))
      (kill-buffer (pai-learn-quiz-buffer quiz))
      (should (equal (plist-get (plist-get (car cell) :details) :status) "cancelled")))))

;;;; Formatting in the dialog

(ert-deftest pai-learn-test-dialog-keeps-code-layout ()
  "Code in the question keeps its lines and gets syntax faces in the dialog."
  (pai-learn-test--with-ui
    (pcase-let ((`(,quiz . ,_cell)
                 (pai-learn-test--quiz
                  (plist-put (pai-learn-test--args) :question
                             "What prints?\n\n```elisp\n(message \"hi\")\n(+ 1 2)\n```"))))
      (with-current-buffer (pai-learn-quiz-buffer quiz)
        (goto-char (point-min))
        (should (search-forward "(message \"hi\")\n(+ 1 2)" nil t))
        (goto-char (point-min))
        (search-forward "\"hi\"")
        (should (memq 'font-lock-string-face
                      (ensure-list (get-text-property (1- (point)) 'face))))))))

;;;; Markdown -> Org

(ert-deftest pai-learn-test-markdown-to-org ()
  "Common Markdown converts to Org; code stays verbatim."
  (should (equal (pai-learn-log-markdown-to-org
                  "# T\n**b** *i* `c` [l](http://u)\n* x\n```py\n* not a heading\n```")
                 "** T\n*b* /i/ =c= [[http://u][l]]\n- x\n#+begin_src py\n,* not a heading\n#+end_src"))
  (should (equal (pai-learn-log-markdown-to-org "$$\nx^2\n$$") "\\[\nx^2\n\\]"))
  (should (equal (pai-learn-log-markdown-to-org "|a|b|\n|---|:-:|")
                 "|a|b|\n|---+---|"))
  (should (string-match-p "\\[\\[file:/tmp/v.png\\]\\]"
                          (pai-learn-log-markdown-to-org "![v](/tmp/v.png)")))
  (should (equal (pai-learn-log-markdown-to-org "> q\n> r\nend")
                 "#+begin_quote\nq\nr\n#+end_quote\nend")))

;;;; Log entries

(ert-deftest pai-learn-test-log-entries ()
  "User, tutor, quiz and question entries are produced and paired."
  (let* ((calls (make-hash-table :test 'equal))
         (user (pai-user-message "<skill name=\"teach\" location=\"x\">body</skill>\n\nTCP"))
         (assistant (list :role 'assistant
                          :content (list (pai-text "Let's **start**.")
                                         (pai-tool-call "c1" "ask_user_question"
                                                        (list :question "Goal?"
                                                              :options (vector (list :label "Depth")))))))
         (result (list :role 'tool-result :tool-call-id "c1" :tool-name "ask_user_question"
                       :content (list (pai-text "x"))
                       :details (list :status "answered"
                                      :answers (list (list :type "option" :index 1 :label "Depth"))))))
    (should (string-match-p "skill loaded: teach" (pai-learn-log-entry user calls)))
    (should (string-match-p "\\`\\* Tutor\nLet's \\*start\\*\\." (pai-learn-log-entry assistant calls)))
    (let ((entry (pai-learn-log-entry result calls)))
      (should (string-match-p "Goal\\?" entry))
      (should (string-match-p "Answer :: 1. Depth" entry)))
    (let ((quiz (list :role 'tool-result :tool-call-id "c2" :tool-name "quiz"
                      :details (list :status "answered" :question "Q?"
                                     :options (list (list :index 1 :label "A") (list :index 2 :label "B"))
                                     :correct-indices '(2) :correct :false :dont-know :false
                                     :answers (list (list :index 1 :label "A"))
                                     :explanation "Because."))))
      (should (string-match-p "Quiz — incorrect ✗" (pai-learn-log-entry quiz calls)))
      (should (string-match-p "Because\\." (pai-learn-log-entry quiz calls))))))

(ert-deftest pai-learn-test-log-file ()
  "Linking writes a backfilled Org file; later messages are appended."
  (let* ((dir (make-temp-file "pai-learn" t))
         (file (expand-file-name "lesson.org" dir))
         (pai-learn-log-display nil))
    (unwind-protect
        (with-temp-buffer
          (setq-local pai--context-messages (list (pai-user-message "hello")))
          (should (= (pai-learn-log-link file) 1))
          (pai-learn-log-append-message (list :role 'assistant :content (list (pai-text "hi"))))
          (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (should (string-match-p "#\\+title: Lesson" text))
            (should (string-match-p "\\* You\nhello\n\n\\* Tutor\nhi" text))))
      (when-let ((b (get-file-buffer file))) (kill-buffer b))
      (delete-directory dir t))))

;;;; Activation

(ert-deftest pai-learn-test-activation-is-local ()
  "/teach registers quiz in this session only, and sends the skill."
  (let ((global-before (gethash "quiz" (default-value 'pai--tools))))
    (with-temp-buffer
      (pai-ext-initialize-instance)
      (should-not (pai-tool-get "quiz"))
      (let ((res (pai-learn--teach-command "TCP" nil)))
        (should (pai-tool-get "quiz"))
        (should (string-match-p "<skill name=\"teach\"" (plist-get res :send)))
        (should (string-match-p "no lesson log is linked" (plist-get res :send)))
        (should (string-match-p "TCP\\'" (plist-get res :send)))))
    (should (eq (gethash "quiz" (default-value 'pai--tools)) global-before))))

(ert-deftest pai-learn-test-rearm-from-history ()
  "A session whose history loaded the teach skill is re-armed before a run."
  (with-temp-buffer
    (pai-ext-initialize-instance)
    (setq-local pai--context-messages
                (list (pai-user-message "<skill name=\"teach\" location=\"x\">b</skill>")))
    (pai-learn--before-agent-start nil nil)
    (should (pai-tool-get "quiz"))))

(ert-deftest pai-learn-test-maker-gets-render-tool ()
  "Maker subagent sessions get render_diagram, and nothing else changes."
  (with-temp-buffer
    (pai-ext-initialize-instance)
    (setq-local pai-isub--role "mermaid-maker")
    (setq-local pai--context-messages nil)
    (pai-learn--before-agent-start nil nil)
    (should (pai-tool-get "render_diagram"))
    (should-not (pai-tool-get "quiz"))))

(ert-deftest pai-learn-test-bundled-files ()
  "The bundled skills and roles parse."
  (should (equal (plist-get (pai-learn-skill "teach") :name) "teach"))
  (should (equal (plist-get (pai-learn-skill "visualize") :name) "visualize"))
  (should (= (length (pai-learn--role-files)) 3)))

(provide 'pai-learn-test)
;;; pai-learn-test.el ends here
