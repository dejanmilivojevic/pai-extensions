;;; pai-memory-reflect-test.el --- Tests for the reflection tier (V2 B4) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defun pai-memory-rt--fill (session n)
  (let ((e (pai-memory-test--turns session n 50)))
    (dotimes (i n)
      (pai-memory-test--commit session (format "r%d" i) (nth (* 2 i) e) (nth (1+ (* 2 i)) e)
                               (format "obs%d the compile broke again %s" i (make-string 200 ?o))))
    e))

(defun pai-memory-rt--script (&rest reflections)
  (pai-faux-push
   (list :tool-calls (list (list :id "r" :name "reflect" :arguments (list :reflections (vconcat reflections)))))))

(ert-deftest pai-memory-reflect-run-and-render ()
  (pai-memory-test--with-settings '(:session (:reflect t :reflect-every-tokens 200 :consolidate :false
                                               :observe "off"))
    (pai-memory-test--with-owner buf dir
      (pai-memory-rt--script "Edits to the store keep breaking the compile; recompile after each.")
      ;; committing observations past the threshold starts it
      (pai-memory-rt--fill session 3)
      (run-hooks 'pai-memory-commit-hook)   ; as a finished observer run does
      (let ((branch (pai-session-get-branch session)))
        (should (equal (pai-memory-reflections branch)
                       '("Edits to the store keep breaking the compile; recompile after each.")))
        (should-not (pai-memory-observations-since-reflection branch)))
      (let ((task (pai-content-text (pai-message-content (cadr (plist-get pai-faux-last-context :messages))))))
        (should (string-match-p "(none yet)" task))
        (should (string-match-p "obs0 the compile broke" task)))
      ;; the next run sees the previous set and only new observations
      (should (string-match-p "No new observations" (plist-get (pai-memory-command "reflect" (list :buffer buf)) :message)))
      (pai-memory-rt--fill session 1)
      (pai-memory-rt--script "Compile breaks come from missing parens in python patches.")
      (should (string-match-p "Reflector started" (plist-get (pai-memory-command "reflect" (list :buffer buf)) :message)))
      (let ((task (pai-content-text (pai-message-content (cadr (plist-get pai-faux-last-context :messages))))))
        (should (string-match-p "- Edits to the store" task))
        (should (string-match-p "10:04  obs0" task))
        (should-not (string-match-p "10:01" task)))
      (should (equal (pai-memory-reflections (pai-session-get-branch session))
                     '("Compile breaks come from missing parens in python patches.")))
      ;; rendered above the observations
      (let ((text (pai-memory-render (list (list :timestamp "t" :content "an event"))
                                     :reflections (pai-memory--branch-reflections (pai-session-get-branch session)))))
        (should (< (string-search "## Reflections" text) (string-search "## Observations" text)))
        (should (string-match-p "- Compile breaks come" text)))
      ;; and in the promoter digest
      (should (string-match-p "## Reflections (patterns" (plist-get (pai-memory-session-digest session) :text))))))

(ert-deftest pai-memory-reflect-off-by-default-and-rules ()
  (pai-memory-test--with-owner buf dir
    (should-not (pai-memory-get :session :reflect session))
    (pai-memory-rt--fill session 3)
    (should-not (pai-memory-reflect))
    (should-not (pai-memory-reflections (pai-session-get-branch session)))
    ;; the tool rejects more than 12
    (let* ((result (list nil))
           (tool (car (pai-memory-reflector-tools result)))
           out)
      (funcall (plist-get tool :execute) (list :reflections (make-vector 13 "x")) nil nil (lambda (r) (setq out r)))
      (should (eq (plist-get out :is-error) t))
      (funcall (plist-get tool :execute) (list :reflections ["key sk-abcdefghijklmnopqrstu1234 used" " "]) nil nil
               (lambda (r) (setq out r)))
      (should (= (length (car (car result))) 1))
      (should-not (string-match-p "sk-abcdefghij" (car (car (car result))))))))

(provide 'pai-memory-reflect-test)
;;; pai-memory-reflect-test.el ends here
