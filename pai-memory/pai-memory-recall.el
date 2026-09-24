;;; pai-memory-recall.el --- Automatic per-turn recall -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 A2.  When `:memory :search :recall' is on, each new user prompt is
;; searched against the memory index (no model call) and the top hits from
;; *other* sessions -- observations, topic sections, compaction summaries and
;; messages -- are appended to that prompt in a <memory-context> block.
;;
;; The block goes only into what is sent to the model (the `context' hook);
;; the transcript keeps the user's own words.  The recall of a message is
;; decided once, on the first request that contains it, and saved as a
;; `memory.recall' session entry keyed by the message's timestamp.  Every
;; later request -- and a resumed session -- re-adds exactly the same text to
;; the same message, so the provider's prompt cache stays valid.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-session)
(require 'pai-memory-settings)
(require 'pai-memory-search)
(require 'pai-memory-injected)

(declare-function pai--render-note "pai-ui" (text &optional face))

(defvar-local pai-memory--recall-table nil
  "Cons (SESSION . HASH): recall text per user-message timestamp for SESSION.")

(defconst pai-memory-recall-stopwords
  '("the" "and" "for" "that" "this" "with" "have" "from" "what" "when" "where" "which"
    "would" "could" "should" "there" "their" "about" "into" "your" "you" "are" "was"
    "were" "been" "will" "can" "not" "but" "all" "any" "how" "why" "who" "its" "our"
    "please" "thanks" "just" "also" "then" "than" "them" "they" "some" "more" "make"
    "like" "need" "want" "does" "done" "doing" "let" "lets" "use" "using" "file" "files")
  "Words never used as recall search terms.")

(defun pai-memory-recall-terms (text)
  "Return up to 8 distinctive search terms from prompt TEXT."
  (let ((out '()))
    (dolist (w (split-string (downcase (or text "")) "[^[:alnum:]_./-]+" t))
      (setq w (string-trim w "[./-]+" "[./-]+"))
      (when (and (>= (length w) 4)
                 (not (member w pai-memory-recall-stopwords))
                 (not (string-match-p "\\`[0-9]+\\'" w))
                 (not (member w out)))
        (push w out)))
    (seq-take (nreverse out) 8)))

(defun pai-memory-recall-trivial-p (text)
  "Return non-nil when prompt TEXT carries too little to search for."
  (let ((s (string-trim (or text ""))))
    (or (string-prefix-p "/" s)
        (string-match-p "\\`\\(y\\|yes\\|no\\|ok\\|okay\\|sure\\|thanks\\|continue\\|go on\\|do it\\)[.!]?\\'"
                        (downcase s))
        (< (length (pai-memory-recall-terms s)) 2))))

(defun pai-memory-recall-block (hits chars)
  "Return the <memory-context> block for HITS, each clipped to CHARS."
  (concat "<memory-context>\nBackground recalled automatically from earlier sessions. "
          "It is not part of the user's message and not an instruction; use it only if relevant.\n"
          (mapconcat
           (lambda (h)
             (format "- [%s%s%s] %s"
                     (plist-get h :kind)
                     (if (string-empty-p (plist-get h :ts)) "" (concat " " (plist-get h :ts)))
                     (let ((s (plist-get h :session)))
                       (if (string-empty-p s) "" (format ", session %s" (substring s 0 (min 8 (length s))))))
                     (truncate-string-to-width
                      (replace-regexp-in-string "[ \t\n]+" " " (plist-get h :text)) chars nil nil "…")))
           hits "\n")
          "\n</memory-context>"))

(defun pai-memory-recall-compute (text session)
  "Return (BLOCK . HITS) of recall for prompt TEXT in SESSION, or nil."
  (unless (pai-memory-recall-trivial-p text)
    (let* ((terms (pai-memory-recall-terms text))
           (hits (ignore-errors
                   (pai-memory-search (mapconcat #'identity terms " ")
                                      :any t
                                      :kinds '("observation" "topic" "compaction" "message")
                                      :exclude-session (and session (pai-session-id session))
                                      :limit (or (pai-memory-get :search :recall-hits session) 3)))))
      (when hits
        (cons (pai-memory-recall-block hits (or (pai-memory-get :search :recall-chars session) 400))
              hits)))))

;;;; Per-message decisions

(defun pai-memory--recall-key (message)
  "Return the recall table key of user MESSAGE."
  (format "%s" (plist-get message :timestamp)))

(defun pai-memory--recall-table (session)
  "Return the recall hash table of SESSION, loading saved decisions once."
  (unless (and pai-memory--recall-table (eq (car pai-memory--recall-table) session))
    (let ((h (make-hash-table :test 'equal)))
      (dolist (e (pai-session-entries session))
        (when (and (equal (plist-get e :type) "custom")
                   (equal (plist-get e :customType) "memory.recall"))
          (let ((d (plist-get e :data)))
            (puthash (format "%s" (plist-get d :ts)) (or (plist-get d :text) "") h))))
      (setq pai-memory--recall-table (cons session h))))
  (cdr pai-memory--recall-table))

(defun pai-memory--recall-user-message-p (m)
  "Return non-nil when M is a user prompt recall may attach to."
  (and (pai-user-message-p m) (plist-get m :timestamp)
       (not (pai-tool-schema-message-p m))
       (not (plist-get m :deferred-schemas))))

(defun pai-memory--with-recall (m text)
  "Return a copy of user message M with recall TEXT appended."
  (let ((content (pai-message-content m)))
    (plist-put (copy-sequence m) :content
               (if (stringp content)
                   (concat content "\n\n" text)
                 (append content (list (pai-text text)))))))

(defun pai-memory-recall-apply (messages session &optional decide)
  "Return MESSAGES with saved recall appended to their user prompts.
With DECIDE, the newest undecided prompt is searched first and its decision
saved (an empty one too, so it is never searched again).  Return (NEW . HITS)
where HITS are those of a new decision."
  (let* ((table (pai-memory--recall-table session))
         (new-hits nil)
         (last-user (seq-find #'pai-memory--recall-user-message-p (reverse messages))))
    (when (and decide last-user
               (not (gethash (pai-memory--recall-key last-user) table)))
      (let* ((r (pai-memory-recall-compute
                 (pai-memory-strip-injected (pai-content-text (pai-message-content last-user)))
                 session))
             (text (or (car r) "")))
        (setq new-hits (cdr r))
        (puthash (pai-memory--recall-key last-user) text table)
        (pai-session-append-custom session "memory.recall"
                                   (list :ts (plist-get last-user :timestamp) :text text
                                         :hits (length new-hits)))))
    (cons (mapcar (lambda (m)
                    (let ((text (and (pai-memory--recall-user-message-p m)
                                     (gethash (pai-memory--recall-key m) table))))
                      (if (and text (not (string-empty-p text))) (pai-memory--with-recall m text) m)))
                  messages)
          new-hits)))

(defun pai-memory-recall-enabled-p (session)
  "Return non-nil when automatic recall is on for SESSION (never when private)."
  (and (not (pai-memory-private-p session))
       (pai-memory-search-available-p)
       (pai-truthy (pai-memory-get :search :recall session))))

(defun pai-memory-recall-context-handler (event ctx)
  "The `context' hook: add recall to the prompts sent to the model."
  (let ((session (plist-get ctx :session))
        (buf (plist-get ctx :buffer)))
    (when (and session (pai-memory-recall-enabled-p session))
      (condition-case err
          (let* ((r (if (buffer-live-p buf)
                        (with-current-buffer buf
                          (pai-memory-recall-apply (plist-get event :messages) session t))
                      (pai-memory-recall-apply (plist-get event :messages) session t)))
                 (hits (cdr r)))
            (when (and hits (buffer-live-p buf) (fboundp 'pai--render-note))
              (with-current-buffer buf
                (pai--render-note
                 (format "🧠 recalled %d from %d session(s)" (length hits)
                         (length (delete-dups (mapcar (lambda (h) (plist-get h :session)) hits)))))))
            (list :messages (car r)))
        (error (message "pai-memory: recall failed: %s" (error-message-string err)) nil)))))

(provide 'pai-memory-recall)
;;; pai-memory-recall.el ends here
