;;; pai-memory-insights-test.el --- Tests for /memory insights (V2 E3) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defun pai-memory-ins--cost (session role cost tokens &optional day)
  "Record a memory.cost entry in SESSION."
  (pai-session-append-custom
   session "memory.cost"
   (list :runId (format "%sT100000-%s-1" (or day (format-time-string "%Y%m%d")) role)
         :role role :cost cost :usage (list :input tokens))))

(ert-deftest pai-memory-insights-figures-and-suggestions ()
  (let* ((dir (file-name-as-directory (make-temp-file "pai-ins" t)))
         (pai-directory dir)
         (pai-settings--global nil) (pai-settings--project nil))
    (unwind-protect
        (let ((cwd (file-name-as-directory (expand-file-name "proj" dir))))
          ;; four sessions with observer spend, one of which compacted (lagging)
          (dotimes (i 4)
            (let ((s (pai-session-new cwd)))
              (pai-session-append-message s (pai-user-message (format "task %d" i)))
              (pai-memory-ins--cost s "observer" 0.01 1000)
              (when (= i 0)
                (pai-session-append s (list :type "compaction" :strategy "observational+summary"
                                            :summary "s")))))
          (let ((s (pai-session-new cwd)))
            (pai-session-append-message s (pai-user-message "learn"))
            (pai-memory-ins--cost s "promoter" 0.05 5000)
            (pai-session-append-custom s "memory.promoted" (list :hash "h" :proposals []))
            ;; an old run outside the window is ignored
            (pai-memory-ins--cost s "observer" 9.0 999999 "20000101"))
          (let ((r (pai-memory-insights-collect 30)))
            (should (= (plist-get r :sessions) 5))
            (should (= (plist-get r :compacted) 1))
            (should (= (plist-get r :observer-calls) 4))
            (should (= (plist-get (alist-get "observer" (plist-get r :by-role) nil nil #'equal) :runs) 4))
            (should (< (abs (- (plist-get (alist-get "promoter" (plist-get r :by-role) nil nil #'equal) :usd)
                               0.05))
                       1e-9))
            (should (= (plist-get r :no-compaction-observer-tokens) 3000))
            (should (equal (plist-get r :strategies) '(("observational+summary" . 1))))
            (should (= (plist-get r :empty-promotions) 1)))
          (let ((text (pai-memory-insights-text 30)))
            (should (string-match-p "By role:" text))
            (should (string-match-p "observer +\\$ +0.0400" text))
            (should (string-match-p "80% of sessions never reached compaction" text))
            (should (string-match-p "\\$0.03, 3.0k tokens" text))))
      (delete-directory dir t))))

(ert-deftest pai-memory-insights-empty ()
  (let ((pai-directory (make-temp-file "pai-ins" t)))
    (unwind-protect
        (should (string-match-p "no background runs" (pai-memory-insights-text 7)))
      (delete-directory pai-directory t))))

(provide 'pai-memory-insights-test)
;;; pai-memory-insights-test.el ends here
