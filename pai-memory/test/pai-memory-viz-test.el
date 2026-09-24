;;; pai-memory-viz-test.el --- Tests for the memory pipeline visuals -*- lexical-binding: t; -*-

;;; Code:

(require 'pai-memory-helpers)
(require 'pai-memory-viz)

(ert-deftest pai-memory-viz-bar ()
  (should (equal (substring-no-properties (pai-memory-viz-bar 0 100 4)) "▕░░░░▏"))
  (should (equal (substring-no-properties (pai-memory-viz-bar 50 100 4)) "▕██░░▏"))
  (should (eq (get-text-property 0 'face (pai-memory-viz-bar 50 100 4)) 'pai-memory-viz-gauge))
  ;; over the max: full, warning face
  (let ((bar (pai-memory-viz-bar 300 100 4)))
    (should (equal (substring-no-properties bar) "▕████▏"))
    (should (eq (get-text-property 0 'face bar) 'pai-memory-viz-gauge-full)))
  (should (equal (substring-no-properties (pai-memory-viz-bar 5 0 2)) "▕░░▏")))

(ert-deftest pai-memory-viz-timeline-segments ()
  "Batches show filed/partial/pool; unobserved tail, cuts and promotions are placed."
  (pai-memory-test--with-session s dir
    (let* ((e (pai-memory-test--turns s 5)))
      (pai-memory-test--commit s "r1" (nth 0 e) (nth 1 e) "a" "b")
      (pai-memory-test--commit s "r2" (nth 2 e) (nth 3 e) "c" "d")
      (pai-memory-test--commit s "r3" (nth 4 e) (nth 5 e) "e")
      (pai-session-append-custom s "memory.dropped" (list :runId "c" :ids '("r1.1" "r1.2" "r2.1")))
      (pai-session-append-custom s "memory.promoted" (list :hash "h" :proposals []))
      (let* ((branch (pai-session-get-branch s))
             (in-flight (list (cons (plist-get (nth 6 e) :id) (plist-get (nth 7 e) :id))))
             (segs (pai-memory-viz-segments branch 100000 in-flight))
             (lines (pai-memory-viz-timeline branch 100000 in-flight)))
        (should (equal (mapcar #'cadr segs) '(filed partial pool observing raw promoted)))
        (should (equal (substring-no-properties (nth 1 lines)) "  ▓▚▒◌░◆▶"))
        (should (string-match-p "1 promotion" (car lines)))
        (should (string-match-p "▓ in topics (1)" (nth 2 lines)))))))

(ert-deftest pai-memory-viz-timeline-wraps-and-empty ()
  (pai-memory-test--with-session s dir
    (should (equal (nth 1 (pai-memory-viz-timeline (pai-session-get-branch s) 1000)) "  (nothing yet)"))
    (pai-memory-test--turns s 3 2000)
    ;; ~3k tokens unobserved at 100 tokens a cell, 10 cells a row
    (let ((lines (pai-memory-viz-timeline (pai-session-get-branch s) 100 nil 10)))
      (should (> (length lines) 4))
      (should (equal (nth 1 lines) "  ░░░░░░░░░░"))
      (should (string-suffix-p "▶" (nth (- (length lines) 2) lines))))))

(ert-deftest pai-memory-viz-timeline-width ()
  "The timeline fits the window by default; a number fixes it."
  (with-temp-buffer
    (let ((pai-memory-viz-timeline-width nil))
      (should (= (pai-memory-viz--timeline-width) 60)) ; no window in batch
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'win))
                ((symbol-function 'window-body-width) (lambda (&rest _) 150)))
        (should (= (pai-memory-viz--timeline-width) 146))))
    (let ((pai-memory-viz-timeline-width 30))
      (should (= (pai-memory-viz--timeline-width) 30)))))

(ert-deftest pai-memory-viz-worker-lines ()
  "One detailed line per worker; finished ones settle with their result."
  (with-temp-buffer
    (let* ((now (float-time))
           (a (pai-activity-start :prefix "obs" :kind "memory" :label "observer"
                                  :detail "8.4k tokens of transcript"))
           (b (pai-activity-start :prefix "con" :kind "memory" :label "consolidator"))
           (c (pai-activity-start :prefix "pro" :kind "memory" :label "promoter")))
      (plist-put b :delta 7)
      (pai-activity-finish b "completed")
      (pai-activity-finish c "timeout")
      (plist-put a :usage-tokens 300)
      (plist-put a :started (- now 10))
      (let ((lines (split-string (substring-no-properties (pai-memory-viz-workers-line now)) "\n")))
        (should (= (length lines) 3))
        ;; running: spinner, id, role, tokens, tok/s, elapsed, detail
        (should (string-match-p
                 "\\`🧠 [◐◓◑◒] obs-[0-9]+ +observer +300t · +30 tok/s · 10s · 8.4k tokens of transcript\\'"
                 (nth 0 lines)))
        (should (string-match-p "\\`🧠 ✓ con-[0-9]+ +consolidator .* tok/s · .* · \\+7\\'" (nth 1 lines)))
        (should (string-match-p "\\`🧠 ✗ pro-[0-9]+ +promoter .* · timeout\\'" (nth 2 lines))))
      ;; after the settle time only the running one is left
      (let ((line (pai-memory-viz-workers-line (+ now pai-memory-viz-settle-seconds 1))))
        (should-not (string-match-p "consolidator" line)))
      (pai-activity-finish a "completed")
      (should-not (pai-memory-viz-workers-line (+ now pai-memory-viz-settle-seconds 1))))))

(ert-deftest pai-memory-viz-footer-and-status ()
  "The widget shows the O/C/X gauges; /memory status the pipeline and timeline."
  (pai-memory-test--with-settings '(:preset "custom" :session (:chunk-tokens 100 :consolidate-at-pool-tokens 1000))
    (pai-memory-test--with-owner buf dir
      (let ((e (pai-memory-test--turns session 2)))
        (pai-memory-test--commit session "r1" (nth 0 e) (nth 1 e) "x"))
      (let ((w (pai-memory-widget-text)))
        (should (string-match-p "O▕████████▏ C▕░░░░░░░░▏ X▕" w))
        ;; the O bar is full: its clock is due, drawn in the warning face
        (should (eq (get-text-property (1+ (string-match "O" w)) 'face w)
                    'pai-memory-viz-gauge-full)))
      (let ((status (pai-memory-status-text)))
        (should (string-match-p "^Pipeline:" status))
        (should (string-match-p "observer +O▕" status))
        (should (string-match-p "promoter +P▕" status))
        (should (string-match-p "^Timeline · 1 cell ≈ 100 tokens" status))
        (should (string-match-p "▒░+▶" status))))))

(provide 'pai-memory-viz-test)
;;; pai-memory-viz-test.el ends here
