;;; pai-memory-entries.el --- Metadata for long-term memory entries -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 B2 (and the selection half of B3).
;;
;; The Markdown files stay clean -- they are what the model sees -- and every
;; entry's metadata lives in a sidecar, ~/.pai/memory/entries.json:
;;
;;   {"version": 1, "entries": [
;;     {id, target, file, text, text_hash, previous: [hash...], created,
;;      updated, origin, source_session, proposal_id, changes: [change id...],
;;      confidence, confirmed: [{time, session, proposal}], expires, pinned,
;;      removed, removed_by}]}
;;
;; Ids are stable: "e-" + a hash of the text and creation time, kept across
;; `replace' (the old text hash goes to `previous').  The sidecar is
;; reconciled with the files lazily, on every read (`pai-memory-entries'):
;;
;;   - an entry whose hash is known keeps its record;
;;   - an entry matching a record's `previous' hash, or a removed record, gets
;;     that record back (this is what makes `/memory undo' and hand edits that
;;     restore text work without special cases);
;;   - an unknown entry gets a new record; its origin, time, session and
;;     proposal are recovered from the change log when a logged change added
;;     that exact text (FC1) -- the migration of V1 memory -- and otherwise it
;;     counts as written by hand (`manual');
;;   - a record whose text left the file is marked removed, never deleted:
;;     "why do you know this" (F2) still finds it.
;;
;; Confidence starts from the origin -- written by hand 1.0, saved by the
;; memory tool at the user's request 0.9, recovered from an old log 0.7,
;; learned by the promoter 0.6 -- and each confirmation adds 0.1 (max 1.0).
;; Entries past `expires' (YYYY-MM-DD) are left out of the snapshot, and the
;; curator proposes removing them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-memory-settings)
(require 'pai-memory-store)

(declare-function pai-memory-make-proposal "pai-memory-proposals")
(declare-function pai-memory-add-proposal "pai-memory-proposals" (proposal))
(declare-function pai-memory-proposals "pai-memory-proposals" (&optional status))
(declare-function pai-memory-proposal-load "pai-memory-proposals" (id))

(defconst pai-memory-origin-confidence
  '(("manual" . 1.0) ("memory-tool" . 0.9) ("migrated" . 0.7) ("proposal" . 0.6))
  "Starting confidence of an entry, by origin.")

;;;; Store

(defun pai-memory-entries-file () (pai-memory-dir "entries.json"))

(defun pai-memory--meta-load ()
  "Return every metadata record, as a list of plists."
  (let ((f (pai-memory-entries-file)))
    (when (file-readable-p f)
      (append (plist-get (ignore-errors (pai-json-decode (pai-memory--read-file f))) :entries) nil))))

(defun pai-memory--meta-save (records)
  "Save RECORDS."
  (pai-memory--write-file (pai-memory-entries-file)
                          (pai-json-encode (list :version 1 :entries (vconcat records)))))

(defun pai-memory--entry-hash (text) (secure-hash 'sha256 (string-trim text)))

(defun pai-memory--file-key (file) (abbreviate-file-name (expand-file-name file)))

(defun pai-memory--vec (x) (append x nil))

;;;; Recovering history from the log (FC1)

(defun pai-memory--meta-recover (target file text log)
  "Return the logged change of TARGET's FILE that added TEXT, newest first, or nil.
The global targets match by target name, so a moved ~/.pai still recovers."
  (let ((key (pai-memory--file-key file)) (found nil)
        (global (memq target '(user memory))))
    (dolist (r (reverse log))
      (when (and (not found) (not (plist-get r :undoes)) (not (plist-get r :group))
                 (if global
                     (equal (plist-get r :target) (symbol-name target))
                   (equal (pai-memory--file-key (or (plist-get r :file) "")) key))
                 (member text (pai-memory-parse-entries (plist-get r :after)))
                 (not (member text (pai-memory-parse-entries (plist-get r :before)))))
        (setq found r)))
    found))

(defun pai-memory--meta-new (target file text log)
  "Return a new record for TEXT of TARGET's FILE, recovering it from LOG."
  (let* ((rec (pai-memory--meta-recover target file text log))
         (origin (cond ((null rec) "manual")
                       ((equal (plist-get rec :origin) "memory-tool") "memory-tool")
                       ((equal (plist-get rec :origin) "proposal") "proposal")
                       (t "migrated")))
         (created (or (plist-get rec :time) (format-time-string "%FT%T%z"))))
    (list :id (concat "e-" (substring (secure-hash 'sha256 (concat text created)) 0 12))
          :target (format "%s" target) :file (pai-memory--file-key file)
          :text text :text_hash (pai-memory--entry-hash text) :previous []
          :created created :updated created :origin origin
          :source_session (let ((sid (plist-get rec :session)) (pid (plist-get rec :proposal_id)))
                            (if (and (member sid '(nil "")) pid (not (equal pid ""))
                                     (fboundp 'pai-memory-proposal-load))
                                (or (plist-get (ignore-errors (pai-memory-proposal-load pid)) :session) "")
                              (or sid "")))
          :proposal_id (or (plist-get rec :proposal_id) "")
          :changes (if rec (vector (plist-get rec :id)) [])
          :confidence (alist-get origin pai-memory-origin-confidence 0.7 nil #'equal)
          :confirmed [] :expires "" :pinned :false :removed "")))

;;;; Reconciling

(defun pai-memory--live-p (r) (member (plist-get r :removed) '(nil "")))

(defun pai-memory--sync (records target cwd)
  "Reconcile RECORDS with TARGET's file for CWD.
Return (CHANGED RECORDS LIVE), LIVE being TARGET's records in file order."
  (let* ((file (pai-memory-target-file target cwd))
         (key (pai-memory--file-key file))
         (texts (pai-memory-parse-entries (pai-memory--read-file file)))
         (hashes (mapcar #'pai-memory--entry-hash texts))
         (mine (seq-filter (lambda (r) (equal (plist-get r :file) key)) records))
         (missing (seq-filter (lambda (r) (and (pai-memory--live-p r)
                                               (not (member (plist-get r :text_hash) hashes))))
                              mine))
         (changed nil) (log 'unread) (live '()) (new '())
         (now (format-time-string "%FT%T%z")))
    (cl-loop
     for text in texts for h in hashes do
     (let ((r (or (seq-find (lambda (r) (and (pai-memory--live-p r) (equal (plist-get r :text_hash) h)
                                             (not (memq r live))))
                            mine)
                  ;; text an earlier revision had (an undone replace)
                  (seq-find (lambda (r) (and (or (memq r missing) (not (pai-memory--live-p r)))
                                             (member h (pai-memory--vec (plist-get r :previous)))))
                            mine)
                  ;; a removed entry that came back
                  (seq-find (lambda (r) (and (not (pai-memory--live-p r)) (equal (plist-get r :text_hash) h)))
                            mine))))
       (cond
        ((null r)
         (when (eq log 'unread) (setq log (pai-memory-log-read)))
         (setq r (pai-memory--meta-new target file text log))
         (push r new) (setq changed t))
        ((not (and (pai-memory--live-p r) (equal (plist-get r :text_hash) h)))
         ;; revive / re-point
         (let ((prev (seq-remove (lambda (x) (equal x h)) (pai-memory--vec (plist-get r :previous)))))
           (unless (equal (plist-get r :text_hash) h) (push (plist-get r :text_hash) prev))
           (plist-put r :previous (vconcat (delete-dups prev))))
         (plist-put r :text text) (plist-put r :text_hash h)
         (plist-put r :removed "") (plist-put r :updated now)
         (setq missing (delq r missing) changed t)))
       (push r live)))
    (dolist (r missing)
      (when (pai-memory--live-p r)
        (plist-put r :removed now) (setq changed t)))
    (list changed (append records (nreverse new)) (nreverse live))))

(defun pai-memory-entries (target cwd)
  "Return TARGET's entry records for project CWD, in file order (synced)."
  (let* ((records (pai-memory--meta-load))
         (res (pai-memory--sync records target cwd)))
    (when (car res) (pai-memory--meta-save (nth 1 res)))
    (nth 2 res)))

(defun pai-memory-entries-all (cwd)
  "Return every live entry record of every target for CWD, synced once."
  (let ((records (pai-memory--meta-load)) (changed nil) (out '()))
    (dolist (target (pai-memory-active-targets cwd))
      (let ((res (pai-memory--sync records target cwd)))
        (setq records (nth 1 res) changed (or changed (car res)))
        (setq out (append out (nth 2 res)))))
    (when changed (pai-memory--meta-save records))
    out))

(defun pai-memory--update-record (target cwd pred fn)
  "Sync TARGET for CWD and call FN on the live record satisfying PRED.
Return FN's value, or signal a `user-error' when no record matches."
  (let* ((records (pai-memory--meta-load))
         (res (pai-memory--sync records target cwd))
         (r (seq-find pred (nth 2 res))))
    (unless r (user-error "No %s entry matches" target))
    (prog1 (funcall fn r)
      (pai-memory--meta-save (nth 1 res)))))

(defun pai-memory--quote-pred (old)
  "Return a predicate matching the one record whose text contains OLD."
  (lambda (r) (string-search (string-trim old) (plist-get r :text))))

(defun pai-memory-entry-find (quote cwd)
  "Return the live record whose text contains QUOTE, in any target, or signal."
  (let ((hits (seq-filter (pai-memory--quote-pred quote) (pai-memory-entries-all cwd))))
    (cond ((string-empty-p (string-trim (or quote ""))) (user-error "Quote part of the entry"))
          ((null hits) (user-error "No memory entry contains %S" quote))
          ((cdr hits) (user-error "%d entries contain %S; quote more" (length hits) quote))
          (t (car hits)))))

;;;; Changes (called by the Markdown provider after every applied change)

(defun pai-memory-entries-note-change (change record ctx)
  "Update metadata after CHANGE was applied and logged as RECORD, with CTX."
  (condition-case err
      (let* ((target (pai-memory--target (plist-get change :target)))
             (cwd (or (plist-get ctx :cwd) default-directory))
             (action (format "%s" (plist-get change :action)))
             (before (pai-memory-parse-entries (plist-get record :before)))
             (after (pai-memory-parse-entries (plist-get record :after)))
             (gone (seq-difference before after))
             (added (seq-difference after before))
             (key (pai-memory--file-key (pai-memory-target-file target cwd)))
             (records (pai-memory--meta-load))
             (now (format-time-string "%FT%T%z"))
             (session (or (plist-get change :session-id)
                          (let ((s (plist-get ctx :session))) (and s (pai-session-id s))) "")))
        ;; a replace keeps the entry's identity: re-point its record first
        (when (and (equal action "replace") gone added)
          (let ((r (seq-find (lambda (r) (and (equal (plist-get r :file) key) (pai-memory--live-p r)
                                              (equal (plist-get r :text_hash)
                                                     (pai-memory--entry-hash (car gone)))))
                             records)))
            (when r
              (plist-put r :previous (vconcat (delete-dups (cons (plist-get r :text_hash)
                                                                 (pai-memory--vec (plist-get r :previous))))))
              (plist-put r :text (car added))
              (plist-put r :text_hash (pai-memory--entry-hash (car added)))
              (plist-put r :updated now))))
        (let* ((res (pai-memory--sync records target cwd))
               (records (nth 1 res)))
          (dolist (text (append added gone))
            (let ((r (seq-find (lambda (r) (and (equal (plist-get r :file) key)
                                                (equal (plist-get r :text_hash) (pai-memory--entry-hash text))))
                               records)))
              (when r
                (plist-put r :changes (vconcat (delete-dups (append (pai-memory--vec (plist-get r :changes))
                                                                    (list (plist-get record :id))))))
                (if (member text gone)
                    (unless (pai-memory--live-p r) (plist-put r :removed_by (plist-get record :id)))
                  (when (and (equal action "add") (string-empty-p (or (plist-get r :source_session) "")))
                    (plist-put r :source_session session))
                  (when (plist-get change :proposal-id)
                    (plist-put r :proposal_id (plist-get change :proposal-id)))
                  (let ((exp (plist-get change :expires)))
                    (when exp (plist-put r :expires exp)))))))
          (pai-memory--meta-save records)))
    (error (message "pai-memory: entry metadata not updated: %s" (error-message-string err)))))

(defun pai-memory-entry-confirm (target cwd old &optional session proposal)
  "Record that entry OLD of TARGET was confirmed again; return its record."
  (pai-memory--update-record
   target cwd (pai-memory--quote-pred old)
   (lambda (r)
     (plist-put r :confirmed (vconcat (pai-memory--vec (plist-get r :confirmed))
                                      (list (list :time (format-time-string "%FT%T%z")
                                                  :session (or session "") :proposal (or proposal "")))))
     r)))

(defun pai-memory-entry-set (quote cwd prop value)
  "Set PROP to VALUE on the entry containing QUOTE; return a message."
  (let ((r (pai-memory-entry-find quote cwd)))
    (pai-memory--update-record (intern (plist-get r :target)) cwd
                               (lambda (x) (equal (plist-get x :id) (plist-get r :id)))
                               (lambda (x) (plist-put x prop value)))
    (plist-get r :id)))

;;;; Derived values

(defun pai-memory-entry-confidence (r)
  "Return R's effective confidence, 0.0-1.0."
  (min 1.0 (+ (or (plist-get r :confidence) 0.7) (* 0.1 (length (plist-get r :confirmed))))))

(defun pai-memory-entry-expired-p (r &optional today)
  "Return non-nil when R's expiry date is before TODAY (YYYY-MM-DD)."
  (let ((e (plist-get r :expires)))
    (and (stringp e) (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" e)
         (string< (substring e 0 10) (or today (format-time-string "%Y-%m-%d"))))))

(defun pai-memory-entry-pinned-p (r) (pai-truthy (plist-get r :pinned)))

(defun pai-memory--entry-age-days (r)
  "Days since R was created or last confirmed."
  (let ((last (or (plist-get (car (last (pai-memory--vec (plist-get r :confirmed)))) :time)
                  (plist-get r :updated) (plist-get r :created))))
    (or (ignore-errors (/ (float-time (time-subtract nil (date-to-time last))) 86400.0)) 0)))

(defun pai-memory-entry-score (r)
  "Return R's ranking score: confidence discounted by age (half at 60 days)."
  (/ (pai-memory-entry-confidence r) (+ 1.0 (/ (pai-memory--entry-age-days r) 60.0))))

(defun pai-memory-valid-date-p (s)
  "Return non-nil when S is a YYYY-MM-DD date."
  (and (stringp s) (string-match-p "\\`[0-9]\\{4\\}-[01][0-9]-[0-3][0-9]\\'" s)))

;;;; Snapshot selection (B2 expiry, B3 retrieval mode)

(defun pai-memory-snapshot-selection (cwd &optional session)
  "Return (ALIST . HIDDEN) of entries to inject for CWD.
ALIST maps each target to its texts in file order.  Expired entries are
always left out.  With `:ltm-injection retrieval', only pinned entries plus
the best `:retrieval-entries' others are kept; HIDDEN counts the rest."
  (let* ((all (seq-remove #'pai-memory-entry-expired-p (pai-memory-entries-all cwd)))
         (mode (format "%s" (or (pai-memory-get :long-term :ltm-injection session) "full")))
         (keep (if (not (equal mode "retrieval"))
                   all
                 (let* ((pinned (seq-filter #'pai-memory-entry-pinned-p all))
                        (rest (sort (seq-remove #'pai-memory-entry-pinned-p all)
                                    (lambda (a b) (> (pai-memory-entry-score a) (pai-memory-entry-score b)))))
                        (n (or (pai-memory-get :long-term :retrieval-entries session) 12)))
                   (append pinned (seq-take rest n))))))
    (cons (mapcar (lambda (target)
                    (cons target (delq nil (mapcar (lambda (r) (and (memq r keep)
                                                                    (equal (plist-get r :target) (symbol-name target))
                                                                    (plist-get r :text)))
                                                   all))))
                  (pai-memory-active-targets cwd))
          (- (length all) (length keep)))))

(defun pai-memory-retrieval-mode-p (&optional session)
  (equal (format "%s" (or (pai-memory-get :long-term :ltm-injection session) "full")) "retrieval"))

(defun pai-memory-pinned-chars (target cwd)
  "Return the characters TARGET's pinned entries take."
  (apply #'+ 0 (mapcar (lambda (r) (+ 3 (length (plist-get r :text))))
                       (seq-filter #'pai-memory-entry-pinned-p (pai-memory-entries target cwd)))))

(defun pai-memory-pin-entry (quote cwd pin)
  "Pin (PIN non-nil) or unpin the entry containing QUOTE; return a message.
In retrieval mode a target's pinned entries must fit its character limit."
  (let ((r (pai-memory-entry-find quote cwd)))
    (when (and pin (pai-memory-retrieval-mode-p))
      (let ((limit (pai-memory-target-limit (intern (plist-get r :target)) nil cwd)))
        (when (and limit (> (+ (pai-memory-pinned-chars (intern (plist-get r :target)) cwd)
                               (length (plist-get r :text)))
                            limit))
          (user-error "Pinned %s entries would exceed its %d-character limit; unpin one first"
                      (plist-get r :target) limit))))
    (pai-memory-entry-set quote cwd :pinned (if pin t :false))
    (format "%s %s entry: %s" (if pin "Pinned" "Unpinned") (plist-get r :target)
            (truncate-string-to-width (plist-get r :text) 70 nil nil "…"))))

;;;; Conflicts and expiry (inputs for the promoter and the curator)

(defconst pai-memory--overlap-stopwords
  '("the" "and" "for" "with" "that" "this" "from" "are" "was" "not" "use" "uses" "user"
    "when" "has" "have" "its" "into" "only" "all" "any" "can" "but" "you" "his" "her")
  "Words ignored when comparing entries.")

(defun pai-memory--entry-words (text)
  (seq-difference (delete-dups (seq-filter (lambda (w) (>= (length w) 3))
                                           (split-string (downcase text) "[^[:alnum:]_-]+" t)))
                  pai-memory--overlap-stopwords))

(defun pai-memory-entry-overlaps (cwd &optional threshold)
  "Return (TARGET A B SCORE) for entry pairs of one target that overlap.
SCORE is the Jaccard overlap of their words, at least THRESHOLD (0.35)."
  (let ((out '()) (threshold (or threshold 0.35)))
    (dolist (target (pai-memory-active-targets cwd))
      (let ((rs (mapcar (lambda (r) (cons (plist-get r :text) (pai-memory--entry-words (plist-get r :text))))
                        (pai-memory-entries target cwd))))
        (while rs
          (let ((a (pop rs)))
            (dolist (b rs)
              (let* ((inter (length (seq-intersection (cdr a) (cdr b))))
                     (union (length (delete-dups (append (cdr a) (cdr b)))))
                     (score (if (> union 0) (/ (float inter) union) 0)))
                (when (>= score threshold)
                  (push (list target (car a) (car b) score) out))))))))
    (sort out (lambda (x y) (> (nth 3 x) (nth 3 y))))))

(defun pai-memory-expire-proposals (cwd)
  "File a memory-remove proposal for every expired entry; return how many."
  (let ((n 0)
        (pending (mapcar (lambda (p) (plist-get p :old)) (ignore-errors (pai-memory-proposals "pending")))))
    (dolist (r (pai-memory-entries-all cwd) n)
      (when (and (pai-memory-entry-expired-p r)
                 (not (equal (plist-get r :target) "team"))
                 (not (member (plist-get r :text) pending)))
        (when (ignore-errors
                (pai-memory-add-proposal
                 (pai-memory-make-proposal
                  :kind "memory-remove" :target (plist-get r :target) :cwd cwd
                  :old (plist-get r :text)
                  :rationale (format "This entry expired on %s." (plist-get r :expires))
                  :evidence (list (concat "entries.json " (plist-get r :id))))))
          (cl-incf n))))))

(provide 'pai-memory-entries)
;;; pai-memory-entries.el ends here
