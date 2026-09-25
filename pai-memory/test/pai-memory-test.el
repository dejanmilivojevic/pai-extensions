;;; pai-memory-test.el --- Tests for pai-memory Phase 1 -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

;;;; Settings

(ert-deftest pai-memory-settings-defaults-and-presets ()
  (pai-memory-test--with-settings nil
    (should (eq (pai-memory-preset) 'balanced))
    (should (= (pai-memory-get :session :chunk-tokens) 8000))
    (should (eq (pai-memory-observe-mode) 'continuous))
    (should (= (pai-memory-get :budget :session-usd) 1.0))
    (should (pai-memory-session-enabled-p)))
  (pai-memory-test--with-settings '(:preset "economy")
    (should (eq (pai-memory-observe-mode) 'near-compaction))
    (should (= (pai-memory-get :session :chunk-tokens) 12000)))
  ;; a preset wins over an explicit preset-controlled knob ...
  (pai-memory-test--with-settings '(:preset "economy" :session (:chunk-tokens 999))
    (should (= (pai-memory-get :session :chunk-tokens) 12000)))
  ;; ... unless the preset is custom
  (pai-memory-test--with-settings '(:preset "custom" :session (:chunk-tokens 999 :observe "off"))
    (should (= (pai-memory-get :session :chunk-tokens) 999))
    (should (eq (pai-memory-observe-mode) 'off)))
  ;; knobs no preset controls always come from settings
  (pai-memory-test--with-settings '(:preset "thorough" :session (:tail-tokens 5000))
    (should (= (pai-memory-get :session :tail-tokens) 5000))))

(ert-deftest pai-memory-settings-project-merges-one-level ()
  (let ((pai-settings--global '(:memory (:preset "custom" :session (:chunk-tokens 111 :tail-tokens 222))))
        (pai-settings--project '(:memory (:session (:tail-tokens 333)))))
    (should (= (pai-memory-get :session :chunk-tokens) 111))
    (should (= (pai-memory-get :session :tail-tokens) 333))))

(ert-deftest pai-memory-set-switches-to-custom ()
  (let ((pai-settings--global nil) (pai-settings--project nil) (saved nil))
    (cl-letf (((symbol-function 'pai-settings-set)
               (lambda (key value &optional scope)
                 (setq saved (list key value scope))
                 (if (eq scope 'project)
                     (setq pai-settings--project (plist-put pai-settings--project key value))
                   (setq pai-settings--global (plist-put pai-settings--global key value)))
                 value)))
      (pai-memory-set :session :chunk-tokens 5000)
      (should (equal (nth 2 saved) 'project))
      (should (eq (pai-memory-preset) 'custom))
      (should (= (pai-memory-get :session :chunk-tokens) 5000))
      ;; the other knobs keep the balanced preset's values
      (should (eq (pai-memory-observe-mode) 'continuous))
      (should (= (pai-memory-get :session :consolidate-at-pool-tokens) 20000))
      ;; a knob no preset controls does not change the preset
      (pai-memory-set-preset 'economy)
      (pai-memory-set :session :tail-tokens 1234)
      (should (eq (pai-memory-preset) 'economy)))))

(ert-deftest pai-memory-session-overrides ()
  (pai-memory-test--with-settings nil
    (pai-memory-test--with-session s dir
      (should (pai-memory-session-enabled-p s))
      (pai-session-append-message s (pai-user-message "hi"))
      (pai-memory-set-session-state s :session :false)
      (should-not (pai-memory-session-enabled-p s))
      (pai-memory-set-session-state s :session t :preset "thorough")
      (should (pai-memory-session-enabled-p s))
      (should (= (pai-memory-get :session :chunk-tokens s) 4000))
      (pai-memory-set-session-state s :learning :false)
      (should-not (pai-memory-learning-enabled-p s))
      ;; overrides survive a reload
      (let ((loaded (pai-session-load (pai-session-file s))))
        (should (eq (pai-memory-preset loaded) 'thorough))
        (should-not (pai-memory-learning-enabled-p loaded))))))

;;;; Ledger

(ert-deftest pai-memory-ledger-watermark-is-contiguous ()
  (pai-memory-test--with-session s dir
    (let* ((e (pai-memory-test--turns s 3)))
      (should-not (pai-memory-watermark (pai-session-get-branch s)))
      ;; the second batch commits first: a gap, so no watermark yet
      (pai-memory-test--commit s "r2" (nth 2 e) (nth 3 e) "second")
      (should-not (pai-memory-watermark (pai-session-get-branch s)))
      (pai-memory-test--commit s "r1" (nth 0 e) (nth 1 e) "first")
      (let* ((branch (pai-session-get-branch s))
             (mark (pai-memory-watermark branch)))
        (should (equal (plist-get (nth mark branch) :id) (plist-get (nth 3 e) :id))))
      ;; pool is sorted by timestamp, then commit order
      (should (equal (mapcar (lambda (o) (plist-get o :content))
                             (pai-memory-pool (pai-session-get-branch s)))
                     '("second" "first"))))))

(ert-deftest pai-memory-ledger-pool-excludes-dropped-and-later-batches ()
  (pai-memory-test--with-session s dir
    (let* ((e (pai-memory-test--turns s 2))
           (b1 (pai-memory-test--commit s "r1" (nth 0 e) (nth 1 e) "one" "two"))
           (_b2 (pai-memory-test--commit s "r2" (nth 2 e) (nth 3 e) "three")))
      (pai-session-append-custom s "memory.dropped"
                                 (list :runId "c1" :ids (list (plist-get (car (plist-get (plist-get b1 :data) :observations)) :id))))
      (let ((branch (pai-session-get-branch s)))
        (should (equal (mapcar (lambda (o) (plist-get o :content)) (pai-memory-pool branch))
                       '("two" "three")))
        ;; only batches ending before an index
        (let ((idx (gethash (plist-get (nth 2 e) :id) (pai-memory-index-map branch))))
          (should (equal (mapcar (lambda (o) (plist-get o :content)) (pai-memory-pool branch idx))
                         '("two"))))))))

(ert-deftest pai-memory-ledger-is-branch-local ()
  (pai-memory-test--with-session s dir
    (let* ((e (pai-memory-test--turns s 2)))
      (pai-memory-test--commit s "r1" (nth 0 e) (nth 3 e) "seen")
      (should (pai-memory-pool (pai-session-get-branch s)))
      ;; a branch from before the commit does not see it
      (pai-session-branch s (plist-get (nth 3 e) :id))
      (pai-session-append-message s (pai-user-message "elsewhere"))
      (should-not (pai-memory-pool (pai-session-get-branch s))))))

(ert-deftest pai-memory-ledger-runs-and-slices ()
  (pai-memory-test--with-session s dir
    (let* ((e (pai-memory-test--turns s 4 300))
           (branch (pai-session-get-branch s))
           (runs (pai-memory-unobserved-runs branch)))
      (should (= (length runs) 1))
      (should (= (length (car runs)) 8))
      ;; ~100 tokens per message at 3 chars/token: 2 messages per 200-token slice
      (let ((slices (pai-memory-slices runs 200 10)))
        (should (= (length slices) 4))
        ;; every slice ends before a valid cut (a user message here)
        (dolist (sl slices)
          (should (eq (pai-message-role (pai-memory-entry-message (cdr (car sl)))) 'user))))
      ;; max slices
      (should (= (length (pai-memory-slices runs 200 2)) 2))
      ;; a partial tip is only taken with FLUSH
      (should (= (length (pai-memory-slices runs 10000 5)) 0))
      (should (= (length (pai-memory-slices runs 10000 5 t)) 1))
      ;; in-flight ranges split runs; a bounded remainder is always sliced
      (let ((runs2 (pai-memory-unobserved-runs
                    branch (list (cons (plist-get (nth 2 e) :id) (plist-get (nth 3 e) :id))))))
        (should (= (length runs2) 2))
        (should (= (length (pai-memory-slices runs2 10000 5)) 1))))))

(ert-deftest pai-memory-ledger-slices-never-open-tail-on-tool-result ()
  (pai-memory-test--with-session s dir
    (pai-session-append-message s (pai-memory-test--msg (pai-user-message (make-string 300 ?u)) 0))
    (pai-session-append-message
     s (pai-memory-test--msg
        (pai-assistant-message :content (list (list :type 'tool-call :id "c1" :name "read"
                                                    :arguments '(:path "x"))))
        1))
    (pai-session-append-message
     s (pai-memory-test--msg (pai-tool-result-message :tool-call-id "c1" :tool-name "read" :content (list (pai-text (make-string 300 ?r)))) 2))
    (pai-session-append-message
     s (pai-memory-test--msg (pai-assistant-message :content (list (pai-text "done"))) 3))
    (let* ((runs (pai-memory-unobserved-runs (pai-session-get-branch s)))
           (slices (pai-memory-slices runs 50 10 t)))
      (let ((starts (mapcar (lambda (sl) (pai-message-role (pai-memory-entry-message (cdr (car sl)))))
                            slices)))
        (should-not (memq 'tool-result starts))))))

(ert-deftest pai-memory-serialize-entry ()
  (pai-memory-test--with-session s dir
    (let* ((u (pai-session-append-message
               s (pai-memory-test--msg (pai-user-message "please fix src/a.el") 0)))
           (tr (pai-session-append-message
                s (pai-memory-test--msg (pai-tool-result-message :tool-call-id "c" :tool-name "bash" :content (list (pai-text (make-string 50 ?x)))) 1))))
      (let ((text (pai-memory-serialize-entry u)))
        (should (string-match-p (concat "\\[Source entry id: " (plist-get u :id) "\\]") text))
        (should (string-match-p "\\[User @ [0-9-]+ [0-9:]+\\]: please fix src/a.el" text)))
      (let ((text (pai-memory-serialize-entry tr 10)))
        (should (string-match-p "Tool result for bash" text))
        (should (string-match-p "truncated 40 chars" text))))))

;;;; Budget and redaction

(ert-deftest pai-memory-redact-masks-secrets ()
  (let ((out (pai-memory-redact
              "key sk-abcdefghijklmnop1234 and AKIAABCDEFGHIJKLMNOP and ghp_abcdefghijklmnopqrstuvwx and API_TOKEN=supersecret1 fine")))
    (should-not (string-match-p "sk-abcdefghijklmnop1234" out))
    (should-not (string-match-p "AKIAABCDEFGHIJKLMNOP" out))
    (should-not (string-match-p "ghp_" out))
    (should-not (string-match-p "supersecret1" out))
    (should (string-match-p "\\[redacted:api-key\\]" out))
    (should (string-match-p "fine\\'" out)))
  (should (equal (pai-memory-redact "nothing secret here") "nothing secret here")))

(ert-deftest pai-memory-budget-caps ()
  (pai-memory-test--with-settings '(:budget (:session-usd 0.01 :daily-usd 100))
    (pai-memory-test--with-session s dir
      (should-not (pai-memory-budget-exceeded s))
      (pai-session-append-custom s "memory.cost" (list :role "observer" :cost 0.02
                                                       :usage (pai-usage :input 10 :output 5 :total-tokens 15)))
      (should (string-match-p "session budget" (pai-memory-budget-exceeded s)))
      (pai-memory-set-session-state s :budgetResumed t)
      (should-not (pai-memory-budget-exceeded s))))
  ;; daily cap from state.json; token caps when no price is known
  (pai-memory-test--with-settings '(:budget (:session-usd nil :daily-usd 0.05 :daily-tokens 100))
    (pai-memory-test--with-session s dir
      (pai-memory-budget-record (list :cost 0.0 :usage (pai-usage :input 150)))
      (should (string-match-p "daily token budget" (pai-memory-budget-exceeded s)))
      (should (= (plist-get (pai-memory-budget-daily) :tokens) 150))
      (pai-memory-budget-record (list :cost 0.06 :usage (pai-usage :input 1)))
      (should (string-match-p "daily budget \\$0.05" (pai-memory-budget-exceeded s))))))

(ert-deftest pai-memory-budget-unpriced-models-hit-token-caps ()
  "Models without prices record $0, so the default token caps must bound them.
 Regression from the first real run: cache-heavy Anthropic usage, cost 0."
  (pai-memory-test--with-settings nil
    (pai-memory-test--with-session s dir
      (should (= (pai-memory-get :budget :session-tokens) 1000000))
      ;; cache reads count a tenth
      (should (= (pai-memory--usage-tokens (list :input 10 :output 100 :cache-write 1000
                                                 :cache-read 5000))
                 1610))
      (dotimes (_ 3)
        (pai-session-append-custom
         s "memory.cost"
         (list :role "observer" :cost 0.0
               :usage (list :input 50 :output 30000 :cache-write 300000 :cache-read 100000))))
      (let ((spend (pai-memory-spend s)))
        (should (= (plist-get spend :unpriced) 3))
        (should (= (plist-get spend :tokens) 1020150)))
      (should (string-match-p "session token budget" (pai-memory-budget-exceeded s))))))

;;;; Observer

(ert-deftest pai-memory-strip-injected-blocks ()
  "Snippets vanish, skills shrink to a marker, everything else is kept."
  (require 'pai-memory-injected)
  (should (equal (pai-memory-strip-injected "plain text") "plain text"))
  (should (equal (pai-memory-strip-injected
                  "<prompt-snippet name=\"C\">\nbe brief\n</prompt-snippet>\n\nmy words\n\n<prompt-snippet name=\"V\">\nverify\n</prompt-snippet>")
                 "my words"))
  (should (equal (pai-memory-strip-injected
                  "<skill name=\"teach\" location=\"~/x\">\nlong body\n</skill>\n\njavascript")
                 "[skill: teach]\n\njavascript"))
  ;; not blocks: unclosed, look-alike tags, and math are left alone
  (should (equal (pai-memory-strip-injected "<prompt-snippet name=\"x\"> never closed")
                 "<prompt-snippet name=\"x\"> never closed"))
  (should (equal (pai-memory-strip-injected "<skills> a < b </skill>") "<skills> a < b </skill>"))
  (should (equal (pai-memory-strip-injected "if a<b and c>d") "if a<b and c>d")))

(ert-deftest pai-memory-observer-ignores-snippets ()
  "The observer's view of a user message drops injected text."
  (let* ((entry (list :id "e1" :type "message"
                      :message (list :role 'user :timestamp 0
                                     :content "<prompt-snippet name=\"C\">\nbe brief\n</prompt-snippet>\n\nreal ask")))
         (text (pai-memory-serialize-entry entry)))
    (should (string-match-p "\\[User @ [^]]*\\]: real ask\\'" text))
    (should-not (string-match-p "be brief" text))))

(ert-deftest pai-memory-observer-starts-at-turn-end-mid-run ()
  "Observers launch at a turn boundary once a chunk is due, not only at settle."
  (pai-memory-test--with-settings '(:preset "custom" :session (:chunk-tokens 150 :observer-concurrency 2))
    (pai-memory-test--with-owner buf dir
      (let ((launched '()))
        (cl-letf (((symbol-function 'pai-memory-observer--launch)
                   (lambda (_session slice) (push slice launched))))
          ;; a run is still going (no agent-settled), but two turns are in
          (setq-local pai--active t)
          (pai-memory-test--turns session 2 300)
          (pai-memory--on-turn-end '(:type turn-end) (list :buffer buf))
          (should (= (length launched) 2)))))))

(ert-deftest pai-memory-turn-end-handler-registered ()
  "The extension ticks the memory clocks on every turn-end."
  (let ((pai--ext-handlers (make-hash-table :test 'eq)))
    (pai-memory-extension (pai-ext-api-create :id "memory-test"))
    (should (memq #'pai-memory--on-turn-end
                  (mapcar #'cdr (gethash 'turn-end pai--ext-handlers))))))

(ert-deftest pai-memory-observer-commits-a-slice ()
  (pai-memory-test--with-settings '(:preset "custom" :session (:chunk-tokens 150 :observer-concurrency 1))
    (pai-memory-test--with-owner buf dir
      (let ((e (pai-memory-test--turns session 2 300)))
        (pai-memory-test--observe-response "User asked for u0; key sk-abcdefghijklmnop1234"
                                           "completed: assistant answered a0")
        ;; the second slice gets a scripted empty run
        (pai-faux-push '(:text "Nothing new." :stop-reason stop))
        (should (= (pai-memory-observer-tick) 2))
        (let* ((branch (pai-session-get-branch session))
               (batches (pai-memory-batches branch))
               (pool (pai-memory-pool branch)))
          (should (= (length batches) 2))
          ;; the first batch covers the first turn
          (should (equal (plist-get (plist-get (car batches) :data) :coversFromId)
                         (plist-get (nth 0 e) :id)))
          (should (equal (plist-get (plist-get (car batches) :data) :coversUpToId)
                         (plist-get (nth 1 e) :id)))
          ;; the empty run still marks its slice observed
          (should-not (plist-get (plist-get (cadr batches) :data) :observations))
          (should (equal (plist-get (nth (pai-memory-watermark branch) branch) :id)
                         (plist-get (nth 3 e) :id)))
          (should (= (length pool) 2))
          ;; ids are unique and content is redacted
          (should (string-match-p "\\.1\\'" (plist-get (car pool) :id)))
          (should (string-match-p "\\[redacted:api-key\\]" (plist-get (car pool) :content)))
          ;; the observer saw only its chunk, fenced
          (let ((task (pai-content-text (pai-message-content
                                         (cadr (plist-get pai-faux-last-context :messages))))))
            (should (string-match-p "BEGIN CONVERSATION CHUNK" task))))
        ;; nothing left: no new launch
        (should (= (pai-memory-observer-tick) 0))
        (should-not pai-memory--in-flight)
        ;; costs were recorded per run
        (should (= (length (pai-memory-cost-entries session)) 2))))))

(ert-deftest pai-memory-observer-waits-for-a-full-chunk ()
  (pai-memory-test--with-settings '(:preset "custom" :session (:chunk-tokens 100000))
    (pai-memory-test--with-owner buf dir
      (pai-memory-test--turns session 2)
      (should (= (pai-memory-observer-tick) 0))
      ;; force observes the partial tip
      (pai-memory-test--observe-response "User said hi")
      (should (= (pai-memory-observer-tick t) 1))
      (should (= (length (pai-memory-pool (pai-session-get-branch session))) 1)))))

(ert-deftest pai-memory-observer-near-compaction-mode ()
  (pai-memory-test--with-settings '(:preset "custom"
                                    :session (:observe "near-compaction" :chunk-tokens 100000
                                              :observe-start-ratio 0.5))
    (pai-memory-test--with-owner buf dir
      (pai-memory-test--turns session 2)
      (setq pai--context-messages (pai-session-context-messages session))
      ;; far below the threshold: idle
      (should (= (pai-memory-observer-tick) 0))
      ;; near it: observe everything, even a partial chunk
      (cl-letf (((symbol-function 'pai-memory--context-ratio) (lambda () 0.7)))
        (pai-memory-test--observe-response "User said hi")
        (should (= (pai-memory-observer-tick) 1))))))

(ert-deftest pai-memory-observer-off-disabled-and-budget ()
  (pai-memory-test--with-settings '(:preset "off")
    (pai-memory-test--with-owner buf dir
      (pai-memory-test--turns session 3 3000)
      (should (= (pai-memory-observer-tick) 0))))
  (pai-memory-test--with-settings '(:preset "custom" :session (:chunk-tokens 10))
    (pai-memory-test--with-owner buf dir
      (pai-memory-test--turns session 1)
      (pai-memory-set-session-state session :session :false)
      (should (= (pai-memory-observer-tick) 0))))
  (pai-memory-test--with-settings '(:preset "custom" :session (:chunk-tokens 10)
                                    :budget (:session-usd 0.001))
    (pai-memory-test--with-owner buf dir
      (pai-memory-test--turns session 1)
      (pai-session-append-custom session "memory.cost" (list :role "observer" :cost 0.01))
      (should (= (pai-memory-observer-tick) 0))
      (should (string-match-p "session budget" pai-memory--budget-notice))
      (should (string-match-p "⏸ budget" (pai-memory-widget-text))))))

(ert-deftest pai-memory-observer-failure-backs-off ()
  (pai-memory-test--with-settings '(:preset "custom" :session (:chunk-tokens 10 :observer-concurrency 1))
    (pai-memory-test--with-owner buf dir
      (pai-memory-test--turns session 1)
      (pai-faux-push '(:error "provider down"))
      (should (= (pai-memory-observer-tick) 1))
      (should (= pai-memory--failures 1))
      (should (> pai-memory--backoff-until (float-time)))
      (should-not (pai-memory-batches (pai-session-get-branch session)))
      ;; backing off: the clock does not relaunch ...
      (should (= (pai-memory-observer-tick) 0))
      ;; ... but a forced observe does, and success resets the failures
      (pai-memory-test--observe-response "User said hi")
      (should (>= (pai-memory-observer-tick t) 1))
      (should (= pai-memory--failures 0))
      (should (pai-memory-batches (pai-session-get-branch session))))))

(ert-deftest pai-memory-observer-drops-results-off-branch ()
  (pai-memory-test--with-owner buf dir
    (let* ((e (pai-memory-test--turns session 2))
           (range (list :from (plist-get (nth 2 e) :id) :to (plist-get (nth 3 e) :id))))
      ;; the user moved to a branch without those entries
      (pai-session-branch session (plist-get (nth 1 e) :id))
      (pai-session-append-message session (pai-user-message "other way"))
      (push range pai-memory--in-flight)
      (pai-memory-observer--done session range "completed"
                                 (list (list :timestamp "2026-09-22 10:00" :content "x"))
                                 (list :data (list :run-id "r")))
      (should-not pai-memory--in-flight)
      (should-not (pai-memory-batches (pai-session-get-branch session)))
      ;; not in the session at all
      (should-not (seq-find (lambda (x) (equal (plist-get x :customType) "memory.observations"))
                            (pai-session-entries session))))))

(ert-deftest pai-memory-observer-tool-validates ()
  (let* ((store (list nil))
         (tool (pai-memory-observer-tool store "2026-01-01 00:00"))
         (res nil))
    (funcall (plist-get tool :execute)
             (list :observations (list (list :timestamp "bad" :content "line1\nline2")
                                       (list :timestamp "2026-09-22 10:00" :content "  ")
                                       (list :timestamp "2026-09-22 10:01" :content "dup")
                                       (list :timestamp "2026-09-22 10:01" :content "dup")))
             nil nil (lambda (r) (setq res r)))
    (should (= (length (car store)) 2))
    (let ((first (car (last (car store)))))
      (should (equal (plist-get first :timestamp) "2026-01-01 00:00"))
      (should (equal (plist-get first :content) "line1 line2")))
    (should (string-match-p "Recorded 2 observations (1 duplicate skipped)"
                            (pai-content-text (plist-get res :content))))))

;;;; Compaction

(defun pai-memory-test--live (session)
  "Return SESSION's context as the live context."
  (pai-session-context-messages session))

(ert-deftest pai-memory-compact-renders-observations ()
  (pai-memory-test--with-settings '(:session (:tail-tokens 150))
    (pai-memory-test--with-session s dir
      (pai-session-append-message s (pai-system-message "SYS"))
      (let* ((e (pai-memory-test--turns s 4 300)))
        (pai-memory-test--commit s "r1" (nth 0 e) (nth 1 e) "User asked for u0")
        (pai-memory-test--commit s "r2" (nth 2 e) (nth 3 e) "completed: a1")
        (pai-memory-test--commit s "r3" (nth 4 e) (nth 5 e) "User asked for u2")
        (let* ((live (pai-memory-test--live s))
               (res (pai-memory-compact live s nil)))
          (should res)
          (should (equal (plist-get res :strategy) "observational"))
          ;; tail closest to 150 tokens: the last turn (u3 a3), first kept is u3
          (should (equal (plist-get res :first-kept-entry-id) (plist-get (nth 6 e) :id)))
          (let ((msgs (plist-get res :messages)))
            (should (pai-system-message-p (car msgs)))
            (should (= (length msgs) 4))
            (let ((summary (pai-content-text (pai-message-content (nth 1 msgs)))))
              (should (string-match-p "## Observations" summary))
              (should (string-match-p "2026-09-22 10:01  User asked for u0" summary))
              (should (string-match-p "completed: a1" summary))
              (should (string-match-p "User asked for u2" summary))
              (should-not (string-match-p "u3" summary))))
          ;; deterministic
          (should (equal (plist-get res :summary)
                         (plist-get (pai-memory-compact live s nil) :summary))))))))

(ert-deftest pai-memory-compact-after-shake ()
  "A shaken tail message still matches its entry, so compaction proceeds."
  (pai-memory-test--with-settings '(:session (:tail-tokens 150))
    (pai-memory-test--with-session s dir
      (let ((e (pai-memory-test--turns s 4 300)))
        (pai-memory-test--commit s "r1" (nth 0 e) (nth 5 e) "observed")
        (pai-session-append s (list :type "shake" :replacements
                                    (vector (list :entryId (plist-get (nth 7 e) :id)
                                                  :message (pai-assistant-message
                                                            :content (list (pai-text "[elided]")))))))
        (let ((res (pai-memory-compact (pai-session-context-messages s) s nil)))
          (should res)
          (should (equal (pai-content-text (pai-message-content (car (last (plist-get res :messages)))))
                         "[elided]")))))))

(ert-deftest pai-memory-compact-declines-without-observations ()
  (pai-memory-test--with-session s dir
    (pai-memory-test--turns s 3)
    (should-not (pai-memory-compact (pai-memory-test--live s) s nil))))

(ert-deftest pai-memory-compact-declines-when-live-context-differs ()
  (pai-memory-test--with-settings '(:session (:tail-tokens 10))
    (pai-memory-test--with-session s dir
      (let ((e (pai-memory-test--turns s 3)))
        (pai-memory-test--commit s "r1" (nth 0 e) (nth 3 e) "obs")
        (let ((live (append (pai-memory-test--live s) (list (pai-user-message "live only")))))
          (should-not (pai-memory-compact live s nil)))))))

(ert-deftest pai-memory-compact-summarizes-lagging-gap ()
  (pai-memory-test--with-settings '(:session (:tail-tokens 100))
    (pai-memory-test--with-session s dir
      (let* ((e (pai-memory-test--turns s 6 300))
             (seen nil))
        ;; only the first turn is observed; the rest lags
        (pai-memory-test--commit s "r1" (nth 0 e) (nth 1 e) "User asked for u0")
        (let ((res (pai-memory-compact (pai-memory-test--live s) s 'model
                                       (lambda (msgs _model)
                                         (setq seen msgs)
                                         (list :text "gap summary" :usage (pai-usage :input 1))))))
          (should (equal (plist-get res :strategy) "observational+summary"))
          (should seen)
          (should (string-match-p "## Recent (summarized, not yet observed)\ngap summary"
                                  (plist-get res :summary)))
          ;; the kept tail is the recent part only
          (should (< (length (plist-get res :messages)) 12))
          ;; pi's cut: the tail may open mid-turn, never with a tool result
          (should (memq (pai-message-role (nth 1 (plist-get res :messages)))
                        '(user assistant))))))))

(ert-deftest pai-memory-compact-async-handler-does-not-block ()
  "With a :callback the handler answers (:async CANCEL) and finishes later."
  (pai-memory-test--with-settings '(:session (:tail-tokens 100))
    (pai-memory-test--with-session s dir
      (let* ((e (pai-memory-test--turns s 6 300))
             (emit nil) (got nil))
        (pai-memory-test--commit s "r1" (nth 0 e) (nth 1 e) "User asked for u0")
        (cl-letf (((symbol-function 'pai-provider-stream)
                   (lambda (_m _c _o e) (setq emit e) nil)))
          (let ((ret (pai-memory-compact-handler
                      (list :messages (pai-memory-test--live s) :model 'model
                            :callback (lambda (r) (setq got r)))
                      (list :session s))))
            (should (plist-member ret :async))
            (should-not got)
            ;; the gap summary streams in later (history, then a split
            ;; turn's prefix)
            (dotimes (_ 3)
              (unless got
                (funcall emit (list :type 'done :message
                                    (pai-assistant-message
                                     :content (list (pai-text "late gap"))
                                     :stop-reason 'stop)))))
            (should (equal (plist-get got :strategy) "observational+summary"))
            (should (string-match-p "late gap" (plist-get got :summary)))))))))

(ert-deftest pai-memory-compact-caps-the-pool ()
  (pai-memory-test--with-settings '(:session (:tail-tokens 10 :max-observation-tokens 30))
    (pai-memory-test--with-session s dir
      (let ((e (pai-memory-test--turns s 2)))
        (pai-memory-test--commit s "r1" (nth 0 e) (nth 1 e)
                                 (make-string 40 ?x) (make-string 40 ?y) "newest")
        (let* ((res (pai-memory-compact (pai-memory-test--live s) s nil))
               (summary (plist-get res :summary))
               (archive (expand-file-name "observations-archive.md" (pai-memory-session-dir s))))
          (should (string-match-p "newest" summary))
          (should-not (string-match-p (make-string 40 ?x) summary))
          (should (string-match-p "older observations are in" summary))
          (should (file-exists-p archive))
          (should (string-match-p (make-string 40 ?x)
                                  (with-temp-buffer (insert-file-contents archive) (buffer-string)))))))))

(ert-deftest pai-memory-compact-handler-declines ()
  (pai-memory-test--with-session s dir
    (let ((e (pai-memory-test--turns s 3)))
      (pai-memory-test--commit s "r1" (nth 0 e) (nth 3 e) "obs")
      (let ((event (list :messages (pai-memory-test--live s))))
        ;; custom instructions ask for an LLM summary
        (should-not (pai-memory-compact-handler (append event '(:custom-instructions "focus on X"))
                                                (list :session s)))
        ;; session layer off
        (pai-memory-set-session-state s :session :false)
        (should-not (pai-memory-compact-handler event (list :session s)))))))

(ert-deftest pai-memory-compact-end-to-end-replays ()
  "Through the core: observational compaction is recorded and replays on load."
  (pai-faux-reset)
  (pai-memory-test--with-pai-buffer buf dir
    (with-current-buffer buf
      (pai-register-extension #'pai-memory-extension "memory")
      (let ((pai-settings--global '(:auto-compact t :memory (:session (:tail-tokens 150))))
            (e (pai-memory-test--turns pai--session 4 300)))
        (setq pai--context-messages (pai-session-context-messages pai--session))
        (pai-memory-test--commit pai--session "r1" (nth 0 e) (nth 5 e) "User asked for u0 to u2")
        (should (pai--compact-now nil))
        ;; no LLM call was made
        (should-not pai-faux-last-context)
        (let ((entry (seq-find (lambda (x) (equal (plist-get x :type) "compaction"))
                               (pai-session-entries pai--session))))
          (should (equal (plist-get entry :strategy) "observational"))
          (should (equal (plist-get entry :firstKeptEntryId) (plist-get (nth 6 e) :id))))
        (should (string-match-p "Compacted context \\[observational\\]" (buffer-string)))
        (let ((replayed (pai-session-context-messages
                         (pai-session-load (pai-session-file pai--session)))))
          (should (equal (mapcar #'pai-message-role replayed)
                         (mapcar #'pai-message-role pai--context-messages)))
          (should (string-match-p "User asked for u0 to u2"
                                  (pai-content-text (pai-message-content (nth 1 replayed))))))))))

(ert-deftest pai-memory-observes-after-a-real-turn ()
  "A settled run triggers the observer through the extension event."
  (pai-faux-reset)
  (pai-memory-test--with-pai-buffer buf dir
    (with-current-buffer buf
      (pai-register-extension #'pai-memory-extension "memory")
      (let ((pai-settings--global '(:memory (:preset "custom"
                                             :session (:chunk-tokens 50 :observer-concurrency 1)))))
        ;; main agent answer, then the observer's run
        (pai-faux-push (list :text (concat "Here is the fix. " (make-string 400 ?x)) :stop-reason 'stop))
        (pai-memory-test--observe-response "User asked for a fix; assistant proposed one")
        (goto-char (point-max))
        (insert "please fix the bug in src/a.el")
        (pai-send)
        (let ((deadline (+ (float-time) 3)))
          (while (and (not (pai-memory-batches (pai-session-get-branch pai--session)))
                      (< (float-time) deadline))
            (accept-process-output nil 0.05)))
        (let ((pool (pai-memory-pool (pai-session-get-branch pai--session))))
          (should (= (length pool) 1))
          (should (string-match-p "assistant proposed one" (plist-get (car pool) :content))))
        ;; the widget reports the pool; the worker is gone from the prompt block
        (should (string-match-p "C▕" (or (pai-memory-widget-text) "")))
        (should-not (pai-memory-worker-running))
        (should (= 1 (length (pai-memory-cost-entries pai--session))))))))

;;;; /menu items

(defun pai-memory-test--menu-item (sub key)
  "Return the Memory section's item KEY in subsection SUB."
  (require 'pai-settings-ui)
  (let* ((sec (seq-find (lambda (s) (eq (pai-settings-ui-section-id s) 'memory))
                        pai-settings-ui--sections))
         (subsec (seq-find (lambda (s) (eq (pai-settings-ui-subsection-id s) sub))
                           (pai-settings-ui-section-subsections sec))))
    (seq-find (lambda (i) (eq (pai-settings-ui-item-key i) key))
              (pai-settings-ui-subsection-items subsec))))

(ert-deftest pai-memory-menu-has-every-knob ()
  "Every session and long-term knob of SPEC \u00a79.2 is editable in /menu."
  (require 'pai-settings-ui)
  (dolist (key '(:observe :observe-start-ratio :chunk-tokens :observer-concurrency
                 :observer-tool-result-chars :observer-timeout :tail-tokens :consolidate
                 :consolidate-at-pool-tokens :pool-target-tokens :max-observation-tokens
                 :journey-target-tokens :consolidator-timeout))
    (should (pai-memory-test--menu-item
             'advanced (intern (format ":memory-%s" (substring (symbol-name key) 1))))))
  (dolist (key '(:memory-user-char-limit :memory-memory-char-limit
                 :memory-project-char-limit :memory-memory-tool-policy :memory-provider))
    (should (pai-memory-test--menu-item 'long-term key)))
  (dolist (key '(:memory-preset :memory-session :memory-long-term :memory-session-usd
                 :memory-daily-usd :memory-session-tokens :memory-daily-tokens))
    (should (pai-memory-test--menu-item 'general key))))

(ert-deftest pai-memory-menu-item-set-get-and-reset ()
  (require 'pai-settings-ui)
  (let ((pai-settings--global nil) (pai-settings--project nil))
    (cl-letf (((symbol-function 'pai-settings-set)
               (lambda (key value &optional scope)
                 (if (eq scope 'project)
                     (setq pai-settings--project (plist-put pai-settings--project key value))
                   (setq pai-settings--global (plist-put pai-settings--global key value)))
                 value)))
      (let ((item (pai-memory-test--menu-item 'advanced :memory-observer-concurrency)))
        (should (= (funcall (pai-settings-ui-item-get item)) 3))
        (funcall (pai-settings-ui-item-set item) 1)
        (should (= (pai-memory-get :session :observer-concurrency) 1))
        ;; not a preset knob: the preset stays
        (should (eq (pai-memory-preset) 'balanced))
        ;; blank restores the default
        (funcall (pai-settings-ui-item-set item) nil)
        (should (= (pai-memory-get :session :observer-concurrency) 3))
        (should-not (plist-member (plist-get (plist-get pai-settings--project :memory) :session)
                                  :observer-concurrency)))
      (let ((item (pai-memory-test--menu-item 'long-term :memory-memory-tool-policy)))
        (should (equal (funcall (pai-settings-ui-item-get item)) "direct"))
        (funcall (pai-settings-ui-item-set item) "propose")
        (should (equal (pai-memory-get :long-term :memory-tool-policy) "propose")))
      (let ((item (pai-memory-test--menu-item 'long-term :memory-provider)))
        (should (equal (funcall (pai-settings-ui-item-get item)) "none"))
        (should (member "none" (funcall (pai-settings-ui-item-choices item))))))))

;;;; Commands

(ert-deftest pai-memory-completion-offers-every-subcommand ()
  "Each subcommand named in the usage line completes, and each is dispatched."
  (let ((usage (with-temp-buffer (pai-memory--dispatch '("bogus")))))
    (dolist (sub pai-memory-subcommands)
      (should (string-match-p (concat "\\_<" (regexp-quote sub) "\\_>") usage))
      (should (member sub (pai-memory--completions (substring sub 0 2))))))
  (should (member "skills" (pai-memory--completions "sk")))
  (should (member "curate" (pai-memory--completions "cu"))))

(ert-deftest pai-memory-completions-by-position ()
  (with-temp-buffer
    (insert "> ")
    (setq-local pai--input-marker (copy-marker (point)))
    (insert "/memory pr")
    (should (equal (pai-memory--completions "pr") '("preset" "promote" "private")))
    (insert "eset ")
    (should (member "economy" (pai-memory--completions "")))
    (erase-buffer) (insert "> ")
    (set-marker pai--input-marker (point))
    (insert "/memory session o")
    (should (equal (pai-memory--completions "o") '("on" "off")))))

(ert-deftest pai-memory-command-subcommands ()
  (pai-memory-test--with-settings nil
    (pai-memory-test--with-owner buf dir
      (let ((run (lambda (args) (plist-get (pai-memory-command args (list :buffer buf)) :message))))
        (should (string-match-p "Session layer: on" (funcall run "")))
        (should (string-match-p "Preset: balanced" (funcall run "status")))
        (should (string-match-p "off for this session" (funcall run "session off")))
        (should (string-match-p "Session layer: off" (funcall run "")))
        (should (string-match-p "Preset thorough" (funcall run "preset thorough")))
        (should (eq (pai-memory-preset session) 'thorough))
        (should (string-match-p "Presets:" (funcall run "preset nope")))
        (should (string-match-p "Expected on or off" (funcall run "learning maybe")))
        (should (string-match-p "lifted" (funcall run "resume")))
        (should (string-match-p "Usage" (funcall run "bogus")))))))

(ert-deftest pai-memory-stop-pauses-until-start ()
  "/memory stop keeps workers from starting and says so in the widget."
  (pai-memory-test--with-settings '(:preset "custom" :session (:chunk-tokens 10))
    (pai-memory-test--with-owner buf dir
      (let ((run (lambda (args) (plist-get (pai-memory-command args (list :buffer buf)) :message))))
        (pai-memory-test--turns session 2)
        (should (string-match-p "stopped for this session" (funcall run "stop")))
        (should (pai-memory-stopped-p session))
        ;; nothing new starts: observer, consolidator and promoter see the pause
        (should (= (pai-memory-observer-tick) 0))
        (should (string-match-p "stopped" (pai-memory-budget-exceeded session)))
        (should-not (pai-memory-promote-due-p session 'session-end))
        ;; the status bar and /memory status say so
        (let ((w (pai-memory-widget-text)))
          (should (string-match-p "⏹ stopped" w))
          (should (eq (get-text-property 0 'face w) 'warning))
          (should (string-match-p "/memory start" (get-text-property 0 'help-echo w))))
        (should (string-match-p "STOPPED" (funcall run "status")))
        ;; the budget override does not lift a stop; start does
        (pai-memory-set-session-state session :budgetResumed t)
        (should (pai-memory-budget-exceeded session))
        (pai-memory-test--observe-response "User said hi")
        (should (string-match-p "resumed" (funcall run "start")))
        (should-not (pai-memory-stopped-p session))
        (should-not (string-match-p "stopped" (or (pai-memory-widget-text) "")))
        ;; stopping one worker by id does not pause memory
        (funcall run "stop w-nope")
        (should-not (pai-memory-stopped-p session))
        ;; resume also lifts a stop
        (funcall run "stop")
        (should (string-match-p "resumed" (funcall run "resume")))))))

(ert-deftest pai-memory-widget-shows-when-switched-off ()
  "With both layers off in settings the widget says off instead of vanishing."
  (pai-memory-test--with-settings '(:session (:enabled :false) :long-term (:enabled :false))
    (pai-memory-test--with-owner buf dir
      (let ((w (pai-memory-widget-text)))
        (should (string-match-p "🧠 off" w))
        (should (eq (get-text-property 0 'face w) 'shadow))
        (should (string-match-p "/menu" (get-text-property 0 'help-echo w)))))))

(ert-deftest pai-memory-on-off-everywhere ()
  "/memory off switches memory off globally, beating project and session values."
  (let* ((pai-directory (file-name-as-directory (make-temp-file "pai-memonoff" t)))
         (pai-settings--global nil)
         (pai-settings--project '(:memory (:session (:enabled t)))))
    (unwind-protect
        (pai-memory-test--with-owner buf dir
          (let ((run (lambda (args) (plist-get (pai-memory-command args (list :buffer buf)) :message))))
            (pai-memory-set-session-state session :learning t)
            (should (string-match-p "off everywhere" (funcall run "off")))
            (should-not (pai-memory-session-enabled-p session))
            (should-not (pai-memory-learning-enabled-p session))
            (should (eq (plist-get (plist-get (pai-settings-scope-value :memory 'global) :session) :enabled)
                        :false))
            (should-not (plist-member (plist-get (pai-settings-scope-value :memory 'project) :session)
                                      :enabled))
            (should (string-match-p "🧠 off" (pai-memory-widget-text)))
            (should (string-match-p "/memory on" (get-text-property 0 'help-echo (pai-memory-widget-text))))
            (should (string-match-p "Memory is on" (funcall run "on")))
            (should (pai-memory-session-enabled-p session))
            (should (pai-memory-learning-enabled-p session))
            ;; on also lifts a stop
            (funcall run "stop")
            (funcall run "on")
            (should-not (pai-memory-stopped-p session))))
      (delete-directory pai-directory t))))

(ert-deftest pai-memory-every-subcommand-completes ()
  "Every subcommand /memory handles is offered in completion (and the usage line)."
  (let* ((file (expand-file-name "pai-memory.el"
                                 (file-name-directory (locate-library "pai-memory"))))
         (src (with-temp-buffer
                (insert-file-contents file)
                (emacs-lisp-mode)
                (goto-char (point-min))
                (re-search-forward "^(defun pai-memory--dispatch ")
                (let ((beg (match-beginning 0)))
                  (goto-char beg) (forward-sexp) (buffer-substring-no-properties beg (point)))))
         (handled '()) (pos 0))
    ;; pcase clauses: ("name" ...) and ((or "a" "b") ...)
    (while (string-match "^      (\\(?:(or \\([^)]*\\))\\|\\(\"[a-z-]+\"\\)\\)" src pos)
      (setq pos (match-end 0))
      (dolist (w (split-string (or (match-string 1 src) (match-string 2 src)) "[ \"]+" t))
        (unless (equal w "'nil") (push w handled))))
    (should (> (length handled) 25))
    (dolist (w handled)
      (should (member w pai-memory-subcommands))
      (should (string-match-p (concat "\\b" (regexp-quote w) "\\b")
                              (pai-memory--dispatch '("bogus-subcommand")))))
    (with-temp-buffer
      (insert "> ")
      (setq-local pai--input-marker (copy-marker (point)))
      (insert "/memory o")
      (should (equal (pai-memory--completions "o") '("on" "off" "observe")))
      ;; every level: session -> on|off -> --global
      (let ((at (lambda (text) (erase-buffer) (insert "> ")
                  (set-marker pai--input-marker (point)) (insert text)
                  (pai-memory--completions ""))))
        (should (equal (funcall at "/memory session ") '("on" "off")))
        (should (equal (funcall at "/memory session off ") '("--global")))
        (should (equal (funcall at "/memory learning on ") '("--global")))
        (should-not (funcall at "/memory session off --global "))
        (should (equal (funcall at "/memory forget some text ") '("--regex" "--all" "--dry-run")))
        (should (equal (funcall at "/memory forget x --all ") '("--regex" "--dry-run")))
        (should (equal (funcall at "/memory curate ") '("--consolidate")))
        (should-not (funcall at "/memory status "))))))

(ert-deftest pai-memory-session-off-global ()
  "/memory session off --global switches only the session layer off everywhere."
  (let* ((pai-directory (file-name-as-directory (make-temp-file "pai-memglob" t)))
         (pai-settings--global nil)
         (pai-settings--project '(:memory (:session (:enabled t)))))
    (unwind-protect
        (pai-memory-test--with-owner buf dir
          (let ((run (lambda (args) (plist-get (pai-memory-command args (list :buffer buf)) :message))))
            (pai-memory-set-session-state session :session t)
            (should (string-match-p "Session memory off everywhere" (funcall run "session off --global")))
            (should-not (pai-memory-session-enabled-p session))
            (should (pai-memory-learning-enabled-p session))
            (should-not (plist-member (plist-get (pai-settings-scope-value :memory 'project) :session)
                                      :enabled))
            (should (string-match-p "Learning off everywhere" (funcall run "learning off --global")))
            (should-not (pai-memory-learning-enabled-p session))
            (funcall run "session on --global")
            (should (pai-memory-session-enabled-p session))
            ;; without --global it stays a session override, as before
            (should (string-match-p "for this session" (funcall run "session off")))))
      (delete-directory pai-directory t))))

(provide 'pai-memory-test)
;;; pai-memory-test.el ends here
