;;; pai-memory-budget.el --- Spend accounting, budget caps and redaction -*- lexical-binding: t; -*-

;;; Commentary:

;; Background spend (SPEC §4.5):
;;
;;   * per session: the `memory.cost' entries each worker run appends to the
;;     owning session (see `pai-memory-worker');
;;   * per day: a running total in ~/.pai/memory/state.json, fed by
;;     `pai-memory-worker-cost-functions'.
;;
;; `pai-memory-budget-exceeded' says whether a new background worker may
;; start.  Dollar caps apply when the model reports prices; for runs that
;; cost $0 (no price information) the token caps apply instead.  `/memory
;; resume' lifts the pause for one session.
;;
;; Redaction (SPEC §10): `pai-memory-redact' masks secrets before anything is
;; written to memory.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-config)
(require 'pai-session)
(require 'pai-memory-settings)
(require 'pai-memory-worker)

;;;; Session spend

(defun pai-memory-cost-entries (session)
  "Return the `memory.cost' data plists recorded in SESSION, oldest first."
  (and session
       (delq nil
             (mapcar (lambda (e)
                       (when (and (equal (plist-get e :type) "custom")
                                  (equal (plist-get e :customType) "memory.cost"))
                         (plist-get e :data)))
                     (pai-session-entries session)))))

(defun pai-memory--usage-tokens (usage)
  "Return USAGE's billable tokens: input + output + cache writes + 10% of cache reads.
Cache reads are billed at about a tenth of the input price by the providers
that report them, so counting them in full would overstate spend tenfold.
These tokens are what the token caps limit -- the only limit that works for
models without price information."
  (let ((g (lambda (k) (or (plist-get usage k) 0))))
    (round (+ (funcall g :input) (funcall g :output) (funcall g :cache-write)
              (* 0.1 (funcall g :cache-read))))))

(defun pai-memory--priced-p (data)
  "Return non-nil when cost DATA had a price: its cost is positive or it used nothing."
  (or (> (or (plist-get data :cost) 0.0) 0)
      (= 0 (pai-memory--usage-tokens (plist-get data :usage)))))

(defun pai-memory-spend (session)
  "Return SESSION's background spend.
The plist is (:cost DOLLARS :tokens N :runs N :unpriced N :by-role ALIST).
TOKENS are billable tokens (see `pai-memory--usage-tokens'); UNPRICED counts
runs whose model has no price, so their dollar cost is unknown (recorded as
0).  BY-ROLE maps a role name to (:cost C :tokens T :runs N)."
  (let ((cost 0.0) (tokens 0) (runs 0) (unpriced 0) (by-role '()))
    (dolist (d (pai-memory-cost-entries session))
      (unless (pai-memory--priced-p d) (setq unpriced (1+ unpriced)))
      (let* ((c (or (plist-get d :cost) 0.0))
             (tk (pai-memory--usage-tokens (plist-get d :usage)))
             (role (or (plist-get d :role) "?"))
             (cell (assoc role by-role)))
        (setq cost (+ cost c) tokens (+ tokens tk) runs (1+ runs))
        (if cell
            (setcdr cell (list :cost (+ (plist-get (cdr cell) :cost) c)
                               :tokens (+ (plist-get (cdr cell) :tokens) tk)
                               :runs (1+ (plist-get (cdr cell) :runs))))
          (push (cons role (list :cost c :tokens tk :runs 1)) by-role))))
    (list :cost cost :tokens tokens :runs runs :unpriced unpriced
          :by-role (nreverse by-role))))

;;;; Daily spend (state.json)

(defun pai-memory-state-file ()
  "Return the path of the pai-memory state file."
  (let ((dir (pai-memory-dir)))
    (make-directory dir t)
    (expand-file-name "state.json" dir)))

(defun pai-memory-state-read ()
  "Return the state plist, or a fresh one.  A newer version is read-only."
  (let ((file (pai-memory-state-file)))
    (or (and (file-readable-p file)
             (ignore-errors
               (with-temp-buffer
                 (insert-file-contents file)
                 (pai-json-decode (buffer-string)))))
        (list :version 1))))

(defun pai-memory-state-write (state)
  "Write STATE atomically unless it comes from a newer version (FC5)."
  (when (<= (or (plist-get state :version) 1) 1)
    (let* ((file (pai-memory-state-file))
           (tmp (concat file ".tmp")))
      (with-temp-file tmp (insert (pai-json-encode (plist-put state :version 1))))
      (rename-file tmp file t))))

(defun pai-memory--today ()
  "Return today's key in the daily table (a keyword like :2026-09-22)."
  (intern (concat ":" (format-time-string "%Y-%m-%d"))))

(defun pai-memory-budget-daily ()
  "Return today's background spend as (:usd D :tokens N)."
  (let ((day (plist-get (plist-get (pai-memory-state-read) :daily) (pai-memory--today))))
    (list :usd (or (plist-get day :usd) 0.0) :tokens (or (plist-get day :tokens) 0))))

(defun pai-memory-budget-record (data)
  "Add one worker run's cost DATA (a `memory.cost' plist) to today's total.
Days older than 30 are pruned."
  (let* ((state (pai-memory-state-read))
         (daily (plist-get state :daily))
         (key (pai-memory--today))
         (day (plist-get daily key))
         (cutoff (format-time-string "%Y-%m-%d" (time-subtract nil (* 30 86400))))
         (kept '()))
    (setq day (list :usd (+ (or (plist-get day :usd) 0.0) (or (plist-get data :cost) 0.0))
                    :tokens (+ (or (plist-get day :tokens) 0)
                               (pai-memory--usage-tokens (plist-get data :usage)))))
    (setq daily (plist-put daily key day))
    (while daily
      (unless (string< (substring (symbol-name (car daily)) 1) cutoff)
        (setq kept (append kept (list (car daily) (cadr daily)))))
      (setq daily (cddr daily)))
    (pai-memory-state-write (plist-put state :daily kept))))

(add-hook 'pai-memory-worker-cost-functions #'pai-memory-budget-record)

;;;; Caps

(defun pai-memory-budget-exceeded (session)
  "Return a reason string when a new worker for SESSION would break a cap, else nil.
Dollar caps only see models with prices; token caps always apply, so they
also bound models without price information.  `/memory resume' (the
`budgetResumed' override) lifts every cap for SESSION.  A session stopped
with `/memory stop' is paused regardless (see `pai-memory-stopped-p')."
  (if (pai-memory-stopped-p session)
      "stopped with /memory stop; /memory start resumes it"
  (unless (and session (pai-truthy (car (pai-memory--override session :budgetResumed))))
    (let* ((cap (lambda (k) (let ((v (pai-memory-get :budget k session)))
                              (and (numberp v) (> v 0) v))))
           (spend (pai-memory-spend session))
           (daily (pai-memory-budget-daily)))
      (cond
       ((and (funcall cap :session-usd) (>= (plist-get spend :cost) (funcall cap :session-usd)))
        (format "session budget $%.2f reached" (funcall cap :session-usd)))
       ((and (funcall cap :daily-usd) (>= (plist-get daily :usd) (funcall cap :daily-usd)))
        (format "daily budget $%.2f reached" (funcall cap :daily-usd)))
       ((and (funcall cap :session-tokens)
             (>= (plist-get spend :tokens) (funcall cap :session-tokens)))
        (format "session token budget %d reached" (funcall cap :session-tokens)))
       ((and (funcall cap :daily-tokens) (>= (plist-get daily :tokens) (funcall cap :daily-tokens)))
        (format "daily token budget %d reached" (funcall cap :daily-tokens))))))))

;;;; Redaction

(defconst pai-memory-redact-patterns
  '(("api-key" . "\\bsk-[A-Za-z0-9_-]\\{16,\\}")
    ("aws-key" . "\\bAKIA[0-9A-Z]\\{16\\}\\b")
    ("github-token" . "\\bgh[pousr]_[A-Za-z0-9]\\{20,\\}")
    ("slack-token" . "\\bxox[abprs]-[A-Za-z0-9-]\\{10,\\}")
    ("bearer" . "\\(?:[Bb]earer\\|[Tt]oken\\) +[A-Za-z0-9._~+/=-]\\{16,\\}")
    ("secret-assignment"
     . "\\b[A-Za-z0-9_]*\\(?:KEY\\|TOKEN\\|SECRET\\|PASSWORD\\|PASSWD\\)[A-Za-z0-9_]*[ \t]*[=:][ \t]*[\"']?[^ \t\n\"']\\{6,\\}"))
  "Alist of (NAME . REGEXP) for secrets masked before writing to memory.")

(defun pai-memory-user-redactions ()
  "Return (NAME . REGEXP) pairs from `:memory :redact-patterns' and forgotten text.
A pattern is a regexp string, or a [NAME, REGEXP] pair.  Invalid regexps are
ignored."
  (seq-filter
   (lambda (p) (ignore-errors (string-match-p (cdr p) "") t))
   (append
    (delq nil
          (mapcar (lambda (p)
                    (cond ((stringp p) (cons "custom" p))
                          ((and (sequencep p) (= (length p) 2))
                           (cons (format "%s" (elt p 0)) (format "%s" (elt p 1))))))
                  (append (plist-get (pai-memory-settings) :redact-patterns) nil)))
    (mapcar (lambda (f) (cons "forgotten" f))
            (append (plist-get (pai-memory-state-read) :forgotten) nil)))))

(defun pai-memory-redact (text)
  "Return TEXT with secrets and the user's redaction patterns masked.
Built-in patterns are `pai-memory-redact-patterns'; the user's come from
`pai-memory-user-redactions'."
  (let ((case-fold-search nil))
    (dolist (p (append pai-memory-redact-patterns (pai-memory-user-redactions)) text)
      (setq text (replace-regexp-in-string (cdr p) (format "[redacted:%s]" (car p))
                                           text t t)))))

(provide 'pai-memory-budget)
;;; pai-memory-budget.el ends here
