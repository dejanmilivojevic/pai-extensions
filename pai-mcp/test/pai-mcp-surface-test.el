;;; pai-mcp-surface-test.el --- MCP native surface contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'pai-mcp-surface)

(defun pai-mcp-surface-test--pump (predicate &optional seconds)
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(ert-deftest pai-mcp-surface-script-chains-real-worker-callbacks ()
  "A child worker receives data and composes two asynchronous MCP operations."
  (let* (result seen
        (pai-mcp-script-dispatch-function
         (lambda (op input _dir callback)
           (should (equal op "call"))
           (push (plist-get input :path) seen)
           (run-at-time 0.01 nil callback
                        (list :ok t :data (+ 1 (plist-get (plist-get input :args) :n)))))))
    (pai-mcp-script-run
     "(lambda (call search describe emit done)
        (funcall call \"first\" '(:n 1)
          (lambda (a)
            (funcall emit (plist-get a :data))
            (funcall call \"second\" (list :n (plist-get a :data))
              (lambda (b) (funcall done (plist-get b :data)))))))"
     (lambda (value) (setq result value)))
    (should-not result)
    (should (pai-mcp-surface-test--pump (lambda () result)))
    (should-not (plist-get result :is-error))
    (should (equal (nreverse seen) '("first" "second")))
    (should (equal (mapcar (lambda (b) (plist-get b :text)) (plist-get result :content)) '("2" "3")))))

(ert-deftest pai-mcp-surface-script-infinite-loop-cannot-block-emacs ()
  "A hard deadline terminates native user code, even without cooperative yields."
  (let (result tick (count 0))
    (run-at-time 0.01 nil (lambda () (setq tick t)))
    (pai-mcp-script-run "(lambda (call search describe emit done) (while t))"
                        (lambda (value) (cl-incf count) (setq result value)) nil 100)
    (should (pai-mcp-surface-test--pump (lambda () result)))
    (should tick)
    (should (= count 1))
    (should (equal (plist-get (plist-get result :details) :error) "timeout"))))

(ert-deftest pai-mcp-surface-script-cancel-settles-once ()
  (let (result (count 0))
    (let ((cancel (pai-mcp-script-run
                   "(lambda (call search describe emit done) (while t))"
                   (lambda (value) (cl-incf count) (setq result value)))))
      (funcall cancel)
      (funcall cancel)
      (should (= count 1))
      (should (equal (plist-get (plist-get result :details) :error) "aborted")))))

(ert-deftest pai-mcp-surface-trace-never-persists-payload-or-identifiers ()
  "A real JSONL writer retains metadata only and honors the event cap."
  (let* ((dir (make-temp-file "pai-mcp-trace-test-" t))
         (file (expand-file-name "trace.jsonl" dir))
         (pai-mcp-trace--writers (make-hash-table :test 'equal))
         (server (list :name "https://user:secret@example.invalid" :def '(:trace t) :dir dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'pai-mcp-settings)
                   (lambda (&optional _) (list :trace (list :file file :maxEvents 1)))))
          (pai-mcp-trace server 'outbound
                         '(:id "secret-correlation-id" :method "tools/call"
                           :params (:arguments (:password "swordfish") :url "https://private.invalid")))
          (pai-mcp-trace server 'outbound '(:id 2 :method "ping"))
          (let ((process (plist-get (gethash file pai-mcp-trace--writers) :process)))
            (pai-mcp-trace-close)
            (should (pai-mcp-surface-test--pump (lambda () (not (process-live-p process))))))
          (with-temp-buffer
            (insert-file-contents file)
            (should (= (count-lines (point-min) (point-max)) 1))
            (let ((event (pai-json-decode (string-trim (buffer-string)))))
              (should (equal (plist-get event :method) "tools/call"))
              (should (equal (plist-get event :id) "[REDACTED_ID]"))
              (should-not (plist-member event :params)))
            (should-not (string-match-p "swordfish\\|private\\|example\\|secret-correlation" (buffer-string)))))
      (pai-mcp-trace-close)
      (delete-directory dir t))))

(ert-deftest pai-mcp-surface-preview-requires-apply-and-rejects-stale-file ()
  (let* ((dir (make-temp-file "pai-mcp-preview-test-" t))
         (file (expand-file-name "mcp.json" dir)) buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (pai-mcp-surface--preview file '(:mcpServers (:demo (:command "emacs")))))
          (should-not (file-exists-p file))
          (with-temp-file file (insert "{\"settings\":{\"scriptMode\":false}}"))
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "Apply")
            (should-error (button-activate (button-at (1- (point)))) :type 'user-error))
          (should (eq (plist-get (plist-get (pai-mcp--read-json-file file) :settings) :scriptMode) :false)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest pai-mcp-surface-panel-is-offline-and-actions-async ()
  (let ((pai-mcp--servers (make-hash-table :test 'equal))
        (pai-mcp--metadata-loaded t) (pai-mcp--metadata nil)
        ready stopped buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'pai-mcp-load-config) (lambda (&optional _) '(("demo" :command "emacs"))))
                  ((symbol-function 'pai-mcp-ensure)
                   (lambda (name on-ready _on-error &optional _dir) (setq ready (cons name on-ready))))
                  ((symbol-function 'pai-mcp-stop) (lambda (name) (setq stopped name))))
          (save-window-excursion
            (setq buffer (pai-mcp-panel default-directory))
            (should-not ready)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "Reconnect")
              (button-activate (button-at (1- (point)))))
            (should (equal stopped "demo"))
            (should (equal (car ready) "demo"))
            (funcall (cdr ready))
            (should (buffer-live-p buffer))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest pai-mcp-surface-token-refuses-command-line-secrets ()
  (let ((asked nil))
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) (setq asked t) "secret")))
      (pai-mcp-surface-command "token set demo secret-on-command-line" nil)
      (should-not asked))))

(ert-deftest pai-mcp-surface-script-resolver-rejects-ambiguous-original ()
  (cl-letf (((symbol-function 'pai-mcp-load-config) (lambda (&optional _) '(("one") ("two"))))
            ((symbol-function 'pai-mcp-settings) (lambda (&optional _) nil))
            ((symbol-function 'pai-mcp--server-tools) (lambda (_) '((:name "echo")))))
    (should-not (pai-mcp-script--resolve "echo" default-directory))
    (should (equal (plist-get (pai-mcp-script--resolve "one_echo" default-directory) :server) "one"))))

(provide 'pai-mcp-surface-test)
;;; pai-mcp-surface-test.el ends here
