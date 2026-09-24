;;; pai-memory-embed.el --- Optional semantic search for the memory index -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 A3.  Off unless `:search :embedder' names an embedder.
;;
;; Embedders are registered by name with `pai-memory-register-embedder': a
;; function called with a list of texts and a callback, which it calls later
;; with (:vectors LIST-OF-FLOAT-VECTORS :tokens N) or (:error MESSAGE).  The
;; built-in "openai" embedder posts to any OpenAI-compatible /v1/embeddings
;; endpoint -- a local llama.cpp/vLLM server as well as a hosted one -- with
;; `:embed-url', `:embed-model' and an API key from the environment variable
;; named by `:embed-key-env'.  The core carries no model or native code.
;;
;; What gets vectors: observations, topic sections, memory entries and
;; skills -- the distilled material, not raw messages.  Vectors are unit
;; length, quantized to int8 and stored base64-encoded in the `vecs' table of
;; index.sqlite (Emacs binds strings as text, not blobs), with a hash of the
;; text so a reused rowid is never matched to the wrong vector.  Embedding
;; runs asynchronously in idle time after indexing, one batch at a time, under
;; a daily token cap (`:embed-daily-tokens').
;;
;; Search (`pai-memory-search' with :semantic t, used by memory_search and
;; /memory search -- not by per-turn recall, which must stay instant): the
;; query is embedded (waiting at most `:embed-timeout' seconds; on failure
;; plain full-text search is used).  Candidates are a broad any-term
;; full-text query plus the newest `:embed-scan' embedded rows in scope, so
;; a note that shares no word with the query is still found; the ranking is
;; reciprocal-rank fusion of the full-text order and cosine similarity.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'pai-core)
(require 'pai-memory-settings)

(declare-function pai-memory--db "pai-memory-search" ())
(declare-function pai-memory-state-read "pai-memory-skills" ())
(declare-function pai-memory-state-write "pai-memory-skills" (state))
(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defconst pai-memory-embed-kinds '("observation" "topic" "memory" "skill")
  "Index kinds that get vectors.")

(defvar pai-memory--embedders nil "Registered embedders: alist (NAME . FUNCTION).")
(defvar pai-memory--embed-busy nil "Non-nil while a background batch is in flight.")
(defvar pai-memory--embed-timer nil "Pending background embedding timer.")
(defvar pai-memory-embed-status nil "Last background embedding result, for /memory status.")

(defun pai-memory-register-embedder (name fn)
  "Register embedder NAME (a string) as FN; see the commentary for FN's contract."
  (setf (alist-get name pai-memory--embedders nil nil #'equal) fn)
  name)

(defun pai-memory-embedder ()
  "Return the configured embedder function, or nil when semantic search is off."
  (let ((name (pai-memory-get :search :embedder)))
    (and (stringp name) (not (string-empty-p name))
         (alist-get name pai-memory--embedders nil nil #'equal))))

;;;; Built-in OpenAI-compatible embedder

(defun pai-memory--embed-openai (texts callback)
  "Embed TEXTS through the configured OpenAI-compatible endpoint; call CALLBACK."
  (let* ((url (pai-memory-get :search :embed-url))
         (model (pai-memory-get :search :embed-model))
         (key-env (pai-memory-get :search :embed-key-env))
         (key (and (stringp key-env) (not (string-empty-p key-env)) (getenv key-env)))
         (url-request-method "POST")
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json"))
                  (and key (list (cons "Authorization" (concat "Bearer " key))))))
         (url-request-data (encode-coding-string
                            (pai-json-encode (list :model (or model "") :input (vconcat texts)))
                            'utf-8)))
    (if (not (and (stringp url) (string-match-p "\\`https?://" url)))
        (funcall callback (list :error "Set :search :embed-url to an OpenAI-compatible /v1/embeddings URL"))
      (url-retrieve
       url
       (lambda (status)
         (let ((result
                (condition-case err
                    (cond
                     ((plist-get status :error)
                      (list :error (format "%s" (plist-get status :error))))
                     (t
                      (goto-char (or url-http-end-of-headers (point-min)))
                      (let* ((json (pai-json-decode (decode-coding-string
                                                     (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
                             (data (append (plist-get json :data) nil)))
                        (if (not data)
                            (list :error (format "no embeddings in the response: %s"
                                                 (truncate-string-to-width (format "%S" json) 200)))
                          (list :vectors (mapcar (lambda (d) (vconcat (plist-get d :embedding)))
                                                 (sort data (lambda (a b) (< (or (plist-get a :index) 0)
                                                                             (or (plist-get b :index) 0)))))
                                :tokens (or (plist-get (plist-get json :usage) :total_tokens)
                                            (apply #'+ (mapcar (lambda (s) (/ (length s) 4)) texts))))))))
                  (error (list :error (error-message-string err))))))
           (kill-buffer (current-buffer))
           (funcall callback result)))
       nil t t))))

(pai-memory-register-embedder "openai" #'pai-memory--embed-openai)

;;;; Vectors

(defun pai-memory-vec-encode (vec)
  "Return VEC (floats) normalized, quantized to int8 and base64-encoded."
  (let* ((norm (sqrt (cl-loop for x across vec sum (* x x))))
         (norm (if (> norm 0) norm 1.0)))
    (base64-encode-string
     (apply #'unibyte-string
            (cl-loop for x across vec
                     collect (let ((q (max -127 (min 127 (round (* 127 (/ x norm)))))))
                               (if (< q 0) (+ 256 q) q))))
     t)))

(defun pai-memory-vec-decode (s)
  "Return the int8 vector encoded in S."
  (let ((bytes (base64-decode-string s)))
    (cl-loop with v = (make-vector (length bytes) 0)
             for i from 0 below (length bytes)
             do (let ((b (aref bytes i))) (aset v i (if (> b 127) (- b 256) b)))
             finally return v)))

(defun pai-memory-vec-cosine (a b)
  "Return the cosine similarity of int8 vectors A and B (both unit length)."
  (if (/= (length a) (length b)) 0.0
    (/ (float (cl-loop for i from 0 below (length a) sum (* (aref a i) (aref b i))))
       (* 127.0 127.0))))

(defun pai-memory--text-hash (text) (substring (secure-hash 'sha1 text) 0 12))

(defun pai-memory-embed-ensure-schema (db)
  "Create the vector table in DB when missing."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS vecs (docid INTEGER PRIMARY KEY, h TEXT, v TEXT)"))

;;;; Daily cap

(defun pai-memory--embed-tokens-today ()
  (let ((st (pai-memory-state-read)))
    (if (equal (plist-get st :embed_day) (format-time-string "%F")) (or (plist-get st :embed_tokens) 0) 0)))

(defun pai-memory--embed-add-tokens (n)
  (let* ((st (pai-memory-state-read))
         (today (format-time-string "%F"))
         (have (if (equal (plist-get st :embed_day) today) (or (plist-get st :embed_tokens) 0) 0)))
    (pai-memory-state-write (plist-put (plist-put st :embed_day today) :embed_tokens (+ have (or n 0))))))

(defun pai-memory-embed-capped-p ()
  "Return non-nil when today's embedding tokens reached `:embed-daily-tokens'."
  (let ((cap (pai-memory-get :search :embed-daily-tokens)))
    (and (numberp cap) (>= (pai-memory--embed-tokens-today) cap))))

;;;; Background embedding

(defun pai-memory-embed-pending (db limit)
  "Return up to LIMIT (ROWID TEXT) rows of DB that need a vector."
  (sqlite-select
   db (format "SELECT rowid, text FROM docs WHERE kind IN (%s) AND rowid NOT IN (SELECT docid FROM vecs) LIMIT ?"
              (mapconcat (lambda (k) (format "'%s'" k)) pai-memory-embed-kinds ","))
   (list limit)))

(defun pai-memory-embed-store (db rows vectors)
  "Store VECTORS for ROWS (ROWID TEXT) in DB."
  (sqlite-transaction db)
  (unwind-protect
      (cl-loop for r in rows for v in vectors
               do (sqlite-execute db "INSERT OR REPLACE INTO vecs (docid, h, v) VALUES (?,?,?)"
                                  (list (car r) (pai-memory--text-hash (cadr r)) (pai-memory-vec-encode v))))
    (sqlite-commit db)))

(defun pai-memory-embed-update (&optional on-done)
  "Embed one batch of pending rows in the background.
ON-DONE is called with `done', `more', `capped', `off' or (error MSG)."
  (let ((fn (pai-memory-embedder))
        (finish (lambda (r) (setq pai-memory-embed-status r) (when on-done (funcall on-done r)))))
    (cond
     ((null fn) (funcall finish 'off))
     (pai-memory--embed-busy nil)
     ((pai-memory-embed-capped-p) (funcall finish 'capped))
     (t
      (let* ((db (pai-memory--db))
             (_ (pai-memory-embed-ensure-schema db))
             (_ (sqlite-execute db "DELETE FROM vecs WHERE docid NOT IN (SELECT rowid FROM docs)"))
             (rows (pai-memory-embed-pending db (or (pai-memory-get :search :embed-batch) 32))))
        (if (null rows)
            (funcall finish 'done)
          (setq pai-memory--embed-busy t)
          (condition-case err
              (funcall fn (mapcar (lambda (r) (truncate-string-to-width (cadr r) 4000)) rows)
                       (lambda (result)
                         (setq pai-memory--embed-busy nil)
                         (if (plist-get result :error)
                             (funcall finish (list 'error (plist-get result :error)))
                           (pai-memory-embed-store (pai-memory--db) rows (plist-get result :vectors))
                           (pai-memory--embed-add-tokens (plist-get result :tokens))
                           (funcall finish 'more))))
            (error (setq pai-memory--embed-busy nil)
                   (funcall finish (list 'error (error-message-string err)))))))))))

(defun pai-memory-embed-schedule ()
  "Embed pending rows in idle time, one batch at a time, until done or capped."
  (when (and (pai-memory-embedder) (not (timerp pai-memory--embed-timer)) (not pai-memory--embed-busy))
    (setq pai-memory--embed-timer
          (run-with-idle-timer
           2 nil
           (lambda ()
             (setq pai-memory--embed-timer nil)
             (pai-memory-embed-update
              (lambda (r) (when (eq r 'more) (pai-memory-embed-schedule)))))))))

;;;; Query

(defun pai-memory-embed-query (text)
  "Return TEXT's int8 vector, waiting at most `:embed-timeout' seconds, or nil."
  (let ((fn (pai-memory-embedder)) (result nil))
    (when (and fn (not (pai-memory-embed-capped-p)))
      (ignore-errors (funcall fn (list text) (lambda (r) (setq result (or r (list :error "nil"))))))
      (let ((deadline (+ (float-time) (or (pai-memory-get :search :embed-timeout) 5))))
        (while (and (null result) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (when (plist-get result :vectors)
        (pai-memory--embed-add-tokens (plist-get result :tokens))
        (pai-memory-vec-decode (pai-memory-vec-encode (car (plist-get result :vectors))))))))

(defun pai-memory-embed-rerank (db rows qvec &optional vec-rows)
  "Return the union of ROWS and VEC-ROWS ordered by reciprocal-rank fusion.
Each row starts with its rowid and ends with its text.  ROWS come in
full-text rank order; every row with a current vector is also ranked by
cosine similarity to QVEC.  A row's score is the sum of 1/(60 + rank) over
the rankings it appears in."
  (let* ((all (let ((seen (make-hash-table)) (out '()))
                (dolist (r (append rows vec-rows))
                  (unless (gethash (car r) seen) (puthash (car r) t seen) (push r out)))
                (nreverse out)))
         (ids (mapcar #'car all))
         (vecs (and ids (sqlite-select db (format "SELECT docid, h, v FROM vecs WHERE docid IN (%s)"
                                                  (mapconcat #'number-to-string ids ",")))))
         (table (make-hash-table))
         (sims '())
         (ftsrank (make-hash-table))
         (vrank (make-hash-table))
         (floor (or (pai-memory-get :search :embed-min-similarity) 0.3)))
    (dolist (v vecs) (puthash (car v) v table))
    (let ((i 0)) (dolist (r rows) (puthash (car r) (cl-incf i) ftsrank)))
    (dolist (r all)
      (let ((v (gethash (car r) table)))
        (when (and v (equal (nth 1 v) (pai-memory--text-hash (car (last r)))))
          (let ((sim (pai-memory-vec-cosine qvec (pai-memory-vec-decode (nth 2 v)))))
            ;; unrelated notes are not "semantic matches"
            (when (>= sim floor) (push (cons (car r) sim) sims))))))
    (let ((i 0))
      (dolist (s (sort sims (lambda (a b) (> (cdr a) (cdr b)))))
        (puthash (car s) (cl-incf i) vrank)))
    (let ((scored '()))
      (dolist (r all)
        (let ((f (gethash (car r) ftsrank)) (v (gethash (car r) vrank)))
          ;; a row only a dissimilar vector brought in is dropped
          (when (or f v)
            (push (cons (+ (if f (/ 1.0 (+ 60 f)) 0) (if v (/ 1.0 (+ 60 v)) 0)) r) scored))))
      (mapcar #'cdr (sort scored (lambda (a b) (> (car a) (car b))))))))

(provide 'pai-memory-embed)
;;; pai-memory-embed.el ends here
