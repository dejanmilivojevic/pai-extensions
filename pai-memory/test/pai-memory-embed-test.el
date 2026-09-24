;;; pai-memory-embed-test.el --- Tests for optional semantic search (V2 A3) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defconst pai-memory-emt--concepts
  '(("car" "automobile" "vehicle" "garage" "parked")
    ("build" "compile" "make" "tests")
    ("tea" "kettle" "brew"))
  "Word groups of the fake embedder: one dimension each.")

(defvar pai-memory-emt--calls 0)

(defun pai-memory-emt--vec (text)
  (let ((words (split-string (downcase text) "[^a-z]+" t)))
    (vconcat (mapcar (lambda (group) (float (+ 0.01 (seq-count (lambda (w) (member w group)) words))))
                     pai-memory-emt--concepts))))

(defun pai-memory-emt--fake (texts callback)
  (cl-incf pai-memory-emt--calls)
  (funcall callback (list :vectors (mapcar #'pai-memory-emt--vec texts) :tokens (* 3 (length texts)))))

(defmacro pai-memory-emt--with-home (dir &rest body)
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-emb" t)))
          (pai-directory ,dir)
          (default-directory (file-name-as-directory (expand-file-name "proj" ,dir)))
          (pai-settings--global '(:memory (:search (:embedder "fake"))))
          (pai-settings--project nil)
          (pai-memory--embed-busy nil)
          (pai-memory-emt--calls 0))
     (make-directory default-directory t)
     (pai-memory-register-embedder "fake" #'pai-memory-emt--fake)
     (cl-letf (((symbol-function 'pai-memory--skill-dirs)
                (lambda () (list (expand-file-name "skills" ,dir)))))
       (unwind-protect (progn ,@body)
         (pai-memory-index-close)
         (delete-directory ,dir t)))))

(defun pai-memory-emt--embed-all ()
  (let ((r nil) (n 0))
    (while (and (not (memq r '(done capped off))) (< (cl-incf n) 20))
      (pai-memory-embed-update (lambda (x) (setq r x))))
    r))

(ert-deftest pai-memory-embed-vectors ()
  (let* ((a (pai-memory-vec-decode (pai-memory-vec-encode [3.0 4.0 0.0])))
         (b (pai-memory-vec-decode (pai-memory-vec-encode [6.0 8.0 0.0])))
         (c (pai-memory-vec-decode (pai-memory-vec-encode [0.0 0.0 -1.0]))))
    (should (equal a [76 102 0]))
    (should (> (pai-memory-vec-cosine a b) 0.99))
    (should (< (abs (pai-memory-vec-cosine a c)) 0.01))
    (should (equal c [0 0 -127]))
    (should (= (pai-memory-vec-cosine a [1 2]) 0.0))))

(ert-deftest pai-memory-embed-semantic-search ()
  (pai-memory-emt--with-home dir
    (pai-memory-apply-change '(:action add :target memory :content "the automobile is parked in the garage")
                             (list :cwd default-directory))
    (pai-memory-apply-change '(:action add :target memory :content "the build uses make for the tests")
                             (list :cwd default-directory))
    (pai-memory-index-update)
    (should (eq (pai-memory-emt--embed-all) 'done))
    (should (= (caar (sqlite-select (pai-memory--db) "SELECT count(*) FROM vecs")) 2))
    ;; "car" shares no word with the entry: full text alone misses it
    (should-not (pai-memory-search "car" :kinds '("memory")))
    (let ((hits (pai-memory-search "car" :kinds '("memory") :semantic t)))
      (should hits)
      (should (string-match-p "automobile" (plist-get (car hits) :text))))
    ;; lexical and semantic agree: the build entry wins for "compile"
    (should (string-match-p "build" (plist-get (car (pai-memory-search "compile" :kinds '("memory") :semantic t)) :text)))
    ;; a changed row (same rowid, new text) never uses the stale vector
    (let ((row (caar (sqlite-select (pai-memory--db) "SELECT rowid FROM docs WHERE text LIKE '%automobile%'"))))
      (sqlite-execute (pai-memory--db) "UPDATE vecs SET h = 'stale' WHERE docid = ?" (list row))
      (should-not (pai-memory-search "car" :kinds '("memory") :semantic t)))
    ;; tokens counted
    (should (> (pai-memory--embed-tokens-today) 0))))

(ert-deftest pai-memory-embed-off-cap-and-failure ()
  (pai-memory-emt--with-home dir
    (pai-memory-apply-change '(:action add :target memory :content "kettle on the stove")
                             (list :cwd default-directory))
    (pai-memory-index-update)
    ;; daily cap
    (setq pai-settings--global '(:memory (:search (:embedder "fake" :embed-daily-tokens 1))))
    (pai-memory--embed-add-tokens 5)
    (should (eq (pai-memory-emt--embed-all) 'capped))
    (should (= pai-memory-emt--calls 0))
    ;; a failing embedder: search falls back to full text
    (setq pai-settings--global '(:memory (:search (:embedder "broken" :embed-timeout 0.2))))
    (pai-memory-register-embedder "broken" (lambda (_texts cb) (funcall cb '(:error "down"))))
    (should (equal (car (pai-memory-emt--embed-all)) 'error))
    (should (pai-memory-search "kettle" :semantic t))
    ;; off
    (setq pai-settings--global nil)
    (should-not (pai-memory-embedder))
    (should (eq (pai-memory-emt--embed-all) 'off))))

(ert-deftest pai-memory-embed-openai-client ()
  (let ((pai-settings--global '(:memory (:search (:embed-url "http://localhost:9/v1/embeddings"
                                                  :embed-model "nomic" :embed-key-env "PAI_TEST_KEY"))))
        (process-environment (cons "PAI_TEST_KEY=secret" process-environment))
        sent result)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url cb &rest _)
                 (setq sent (list url-request-method url-request-extra-headers
                                  (pai-json-decode (decode-coding-string url-request-data 'utf-8))))
                 (with-current-buffer (generate-new-buffer " *fake-http*")
                   (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n"
                           "{\"data\":[{\"index\":1,\"embedding\":[0,1]},{\"index\":0,\"embedding\":[1,0]}],\"usage\":{\"total_tokens\":7}}")
                   (setq-local url-http-end-of-headers (save-excursion (goto-char (point-min)) (search-forward "\n\n") (point)))
                   (funcall cb nil)))))
      (pai-memory--embed-openai '("a" "b") (lambda (r) (setq result r))))
    (should (equal (car sent) "POST"))
    (should (equal (cdr (assoc "Authorization" (nth 1 sent))) "Bearer secret"))
    (should (equal (plist-get (nth 2 sent) :model) "nomic"))
    (should (equal (plist-get result :vectors) '([1 0] [0 1])))
    (should (= (plist-get result :tokens) 7))
    ;; no URL
    (let ((pai-settings--global nil) r)
      (pai-memory--embed-openai '("a") (lambda (x) (setq r x)))
      (should (string-match-p "embed-url" (plist-get r :error))))))

(provide 'pai-memory-embed-test)
;;; pai-memory-embed-test.el ends here
