;;; pai-memory-insights.el --- Spend reports and preset suggestions -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 E3.  `/memory insights [DAYS]' reads the sessions touched in the last
;; DAYS (default 30) and the proposal queue, and reports:
;;
;;   * background spend per role, per project and per day (dollars, billable
;;     tokens, runs), from `memory.cost' entries (dated by their run id);
;;   * observer calls per 100k tokens of transcript;
;;   * how compactions went: from observations alone, with an LLM summary of
;;     the part observers had not reached ("lagging"), or a plain LLM summary;
;;   * how many sessions ever reached compaction;
;;   * promoter runs that proposed nothing, and proposals accepted / rejected /
;;     stale / pending.
;;
;; It ends with suggestions from fixed rules.  Nothing is changed.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-config)
(require 'pai-compaction)
(require 'pai-memory-settings)
(require 'pai-memory-budget)
(require 'pai-memory-proposals)
(require 'pai-activity)

(defun pai-memory--insight-session-files (days)
  "Return session files modified in the last DAYS (worker transcripts excluded)."
  (let ((root (expand-file-name "sessions" pai-directory))
        (cutoff (time-subtract nil (* days 86400))))
    (when (file-directory-p root)
      (seq-filter (lambda (f) (time-less-p cutoff (file-attribute-modification-time (file-attributes f))))
                  (apply #'append
                         (mapcar (lambda (d) (and (file-directory-p d) (directory-files d t "\\.jsonl\\'")))
                                 (directory-files root t "\\`[^.]")))))))

(defun pai-memory--run-day (run-id)
  "Return the YYYY-MM-DD day encoded at the start of RUN-ID, or nil."
  (when (and (stringp run-id) (string-match "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T" run-id))
    (format "%s-%s-%s" (match-string 1 run-id) (match-string 2 run-id) (match-string 3 run-id))))

(defun pai-memory--add (alist key &rest kv)
  "Return ALIST with KEY's plist incremented by KV (key number ...)."
  (let ((cell (assoc key alist)))
    (unless cell (setq cell (cons key nil)) (setq alist (append alist (list cell))))
    (while kv
      (setcdr cell (plist-put (cdr cell) (car kv) (+ (or (plist-get (cdr cell) (car kv)) 0) (cadr kv))))
      (setq kv (cddr kv)))
    alist))

(defun pai-memory-insights-collect (&optional days)
  "Collect the insight figures for the last DAYS (default 30)."
  (let* ((days (or days 30))
         (cutoff (format-time-string "%Y-%m-%d" (time-subtract nil (* days 86400))))
         (by-role nil) (by-project nil) (by-day nil)
         (sessions 0) (compacted 0) (transcript-chars 0)
         (observer-calls 0) (unpriced 0)
         (strategies nil)
         (no-compaction-observer-usd 0.0) (no-compaction-observer-tokens 0)
         (promoter-runs 0) (empty-promotions 0) (promotions 0))
    (dolist (file (pai-memory--insight-session-files days))
      (let ((project (file-name-nondirectory (directory-file-name (file-name-directory file))))
            (had-compaction nil) (had-user nil) (obs-usd 0.0) (obs-tokens 0))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((bol (point)) (eol (line-end-position))
                   (probe (buffer-substring-no-properties bol (min eol (+ bol 200)))))
              (cond
               ((string-match-p "\"type\":\"message\"" probe)
                (setq transcript-chars (+ transcript-chars (- eol bol)))
                (when (string-match-p "\"role\":\"user\"" (buffer-substring-no-properties
                                                           bol (min eol (+ bol 400))))
                  (setq had-user t)))
               ((string-match-p "\"type\":\"compaction\"\\|\"memory\\.cost\"\\|\"memory\\.promoted\"" probe)
                (let ((e (ignore-errors (pai-json-decode (buffer-substring-no-properties bol eol)))))
                  (pcase (or (and (equal (plist-get e :type) "custom") (plist-get e :customType))
                             (plist-get e :type))
                    ("compaction"
                     (setq had-compaction t)
                     (let ((st (or (plist-get e :strategy) "summary")))
                       (setf (alist-get st strategies nil nil #'equal)
                             (1+ (alist-get st strategies 0 nil #'equal)))))
                    ("memory.promoted"
                     (cl-incf promotions)
                     (when (seq-empty-p (plist-get (plist-get e :data) :proposals))
                       (cl-incf empty-promotions)))
                    ("memory.cost"
                     (let* ((d (plist-get e :data))
                            (day (pai-memory--run-day (plist-get d :runId))))
                       (when (and day (not (string< day cutoff)))
                         (let ((usd (or (plist-get d :cost) 0.0))
                               (tk (pai-memory--usage-tokens (plist-get d :usage)))
                               (role (or (plist-get d :role) "?")))
                           (unless (pai-memory--priced-p d) (cl-incf unpriced))
                           (setq by-role (pai-memory--add by-role role :usd usd :tokens tk :runs 1)
                                 by-project (pai-memory--add by-project project :usd usd :tokens tk :runs 1)
                                 by-day (pai-memory--add by-day day :usd usd :tokens tk :runs 1))
                           (pcase role
                             ("observer" (cl-incf observer-calls)
                              (setq obs-usd (+ obs-usd usd) obs-tokens (+ obs-tokens tk)))
                             ("promoter" (cl-incf promoter-runs))))))))))))
            (forward-line 1)))
        (when had-user (cl-incf sessions))
        (if had-compaction
            (cl-incf compacted)
          (setq no-compaction-observer-usd (+ no-compaction-observer-usd obs-usd)
                no-compaction-observer-tokens (+ no-compaction-observer-tokens obs-tokens)))))
    (let ((props (seq-filter (lambda (p) (not (string< (substring (or (plist-get p :created) "0000-00-00") 0 10)
                                                        cutoff)))
                             (pai-memory-proposals))))
      (list :days days :sessions sessions :compacted compacted
            :transcript-tokens (pai-estimate-tokens-from-chars transcript-chars)
            :observer-calls observer-calls :unpriced unpriced
            :by-role by-role :by-project by-project
            :by-day (sort by-day (lambda (a b) (string< (car a) (car b))))
            :strategies strategies
            :no-compaction-observer-usd no-compaction-observer-usd
            :no-compaction-observer-tokens no-compaction-observer-tokens
            :promoter-runs promoter-runs :promotions promotions :empty-promotions empty-promotions
            :proposals (mapcar (lambda (st) (cons st (cl-count st props :key (lambda (p) (plist-get p :status))
                                                               :test #'equal)))
                               '("accepted" "rejected" "stale" "pending"))))))

(defun pai-memory--pct (n d)
  "Return N/D as a whole percentage (0 when D is 0)."
  (if (> d 0) (round (* 100.0 n) d) 0))

(defun pai-memory-insights-suggestions (r)
  "Return suggestion strings for insight figures R."
  (let* ((out '())
         (strategies (plist-get r :strategies))
         (total-compactions (apply #'+ (mapcar #'cdr strategies)))
         (lagging (+ (alist-get "observational+summary" strategies 0 nil #'equal)
                     (alist-get "summary" strategies 0 nil #'equal)))
         (sessions (plist-get r :sessions))
         (never (- sessions (plist-get r :compacted)))
         (props (plist-get r :proposals))
         (decided (+ (alist-get "accepted" props 0 nil #'equal) (alist-get "rejected" props 0 nil #'equal))))
    (when (and (> sessions 3) (>= (pai-memory--pct never sessions) 50)
               (> (plist-get r :observer-calls) 0)
               (eq (pai-memory-observe-mode) 'continuous))
      (push (format "%d%% of sessions never reached compaction. The `economy' preset (observe near compaction) would have skipped their observers: %s%s."
                    (pai-memory--pct never sessions)
                    (if (> (plist-get r :no-compaction-observer-usd) 0)
                        (format "$%.2f, " (plist-get r :no-compaction-observer-usd)) "")
                    (format "%s tokens" (pai-activity-fmt-count (plist-get r :no-compaction-observer-tokens))))
            out))
    (when (and (>= total-compactions 3) (>= (pai-memory--pct lagging total-compactions) 25))
      (push (format "%d%% of compactions needed an LLM summary because observers lagged or were off. Raise \"Observers at once\", use a faster observer model, or lower the chunk size."
                    (pai-memory--pct lagging total-compactions))
            out))
    (when (and (>= decided 4) (>= (pai-memory--pct (alist-get "rejected" props 0 nil #'equal) decided) 50))
      (push (format "You rejected %d%% of proposals. Promote less often (only at session end) or turn learning off where it doesn't help."
                    (pai-memory--pct (alist-get "rejected" props 0 nil #'equal) decided))
            out))
    (when (and (>= (plist-get r :promotions) 5)
               (>= (pai-memory--pct (plist-get r :empty-promotions) (plist-get r :promotions)) 80))
      (push (format "%d%% of promoter runs proposed nothing. Promoting only at session end would save most of their cost."
                    (pai-memory--pct (plist-get r :empty-promotions) (plist-get r :promotions)))
            out))
    (when (> (plist-get r :unpriced) 0)
      (push (format "%d run(s) used models without prices, so their dollars are not counted; the token caps still bound them (see /prices)."
                    (plist-get r :unpriced))
            out))
    (nreverse out)))

(defun pai-memory-insights-text (&optional days)
  "Return the `/memory insights' report for the last DAYS."
  (let* ((r (pai-memory-insights-collect days))
         (row (lambda (label p)
                (format "  %-28s $%8.4f  %8s tokens  %4d run(s)" label
                        (or (plist-get p :usd) 0.0) (pai-activity-fmt-count (or (plist-get p :tokens) 0))
                        (or (plist-get p :runs) 0))))
         (strategies (plist-get r :strategies))
         (suggestions (pai-memory-insights-suggestions r)))
    (string-join
     (delq nil
           (append
            (list (format "Memory insights, last %d day(s): %d session(s), ~%s transcript tokens"
                          (plist-get r :days) (plist-get r :sessions)
                          (pai-activity-fmt-count (plist-get r :transcript-tokens))))
            (list "By role:")
            (or (mapcar (lambda (c) (funcall row (car c) (cdr c))) (plist-get r :by-role))
                (list "  (no background runs)"))
            (list "By project:")
            (mapcar (lambda (c) (funcall row (car c) (cdr c))) (plist-get r :by-project))
            (list "By day:")
            (mapcar (lambda (c) (funcall row (car c) (cdr c))) (plist-get r :by-day))
            (list (format "Observer calls: %d (%s per 100k transcript tokens)"
                          (plist-get r :observer-calls)
                          (if (> (plist-get r :transcript-tokens) 0)
                              (format "%.1f" (/ (* 100000.0 (plist-get r :observer-calls))
                                                (plist-get r :transcript-tokens)))
                            "-"))
                  (format "Compactions: %s · sessions that reached compaction: %d of %d"
                          (if strategies
                              (mapconcat (lambda (c) (format "%s %d" (car c) (cdr c))) strategies ", ")
                            "none")
                          (plist-get r :compacted) (plist-get r :sessions))
                  (format "Promoter: %d run(s), %d with no proposal · proposals: %s"
                          (plist-get r :promoter-runs) (plist-get r :empty-promotions)
                          (mapconcat (lambda (c) (format "%s %d" (car c) (cdr c)))
                                     (plist-get r :proposals) ", ")))
            (if suggestions
                (cons "Suggestions:" (mapcar (lambda (s) (concat "  - " s)) suggestions))
              (list "Suggestions: none; the current settings look fine for how you work."))))
     "\n")))

(provide 'pai-memory-insights)
;;; pai-memory-insights.el ends here
