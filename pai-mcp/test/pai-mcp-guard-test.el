;;; pai-mcp-guard-test.el --- MCP guard contracts -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'pai-mcp-guard)

(defun pai-mcp-guard-test--click (buffer label)
  "Activate the visible button LABEL in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward label)
    (button-activate (button-at (1- (point))))))

(defmacro pai-mcp-guard-test--ui (&rest body)
  "Run BODY with a non-modal approval UI and isolated state."
  (declare (indent 0))
  `(let ((pai-mcp-guard--approvals (make-hash-table :test 'equal))
         (before (buffer-list))
         (noninteractive nil))
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Blocking prompt")))
               ((symbol-function 'yes-or-no-p) (lambda (&rest _) (ert-fail "Blocking prompt"))))
       (unwind-protect (progn ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer before) (kill-buffer buffer)))))))

(ert-deftest pai-mcp-guard-approval-once-session-deny ()
  "Visible choices settle exactly once; only session approval is reused."
  (pai-mcp-guard-test--ui
    (let ((allowed 0) (denied 0))
      (cl-labels ((request () (pai-mcp-guard-request
                               '(tool "server" "write" "args") "Write" "Arguments"
                               (lambda () (cl-incf allowed))
                               (lambda () (cl-incf denied)))))
        (let ((first (request)))
          (should (= allowed 0))
          (should (= denied 0))
          (pai-mcp-guard-test--click first "Allow once")
          (pai-mcp-guard-test--click first "Deny")
          (should (= allowed 1))
          (should (= denied 0)))
        (let ((second (request)))
          (should (buffer-live-p second))
          (pai-mcp-guard-test--click second "Deny")
          (should (= denied 1)))
        (let ((third (request)))
          (pai-mcp-guard-test--click third "Allow for session")
          (should (= allowed 2)))
        (should-not (request))
        (should (= allowed 3))
        (should (= denied 1))))))

(ert-deftest pai-mcp-guard-closed-dialog-and-headless-deny ()
  "A missing user decision cannot leave a tool waiting forever."
  (let ((denied 0) (allowed 0))
    (pai-mcp-guard-test--ui
      (let ((buffer (pai-mcp-guard-request nil "One request" "Body"
                                            (lambda () (cl-incf allowed))
                                            (lambda () (cl-incf denied)))))
        (with-current-buffer buffer
          (should-not (string-match-p "Allow for session" (buffer-string))))
        (kill-buffer buffer)
        (should (= denied 1))))
    (let ((noninteractive t))
      (should-not (pai-mcp-guard-request nil "Headless" "Body"
                                         (lambda () (cl-incf allowed))
                                         (lambda () (cl-incf denied)))))
    (should (= allowed 0))
    (should (= denied 2))))

(ert-deftest pai-mcp-guard-session-approval-scopes-arguments-and-schema ()
  "Changed arguments/schema/project/conversation need a new decision."
  (pai-mcp-guard-test--ui
    (let ((allowed 0) (denied 0)
          (schema '(:type "object" :properties (:value (:type "string"))))
          (pai--session (pai-session-create :id "guard-session-a")))
      (cl-letf (((symbol-function 'pai-mcp--server)
                 (lambda (_) '(:def (:approveTools t))))
                ((symbol-function 'pai-mcp-settings) (lambda (&optional _) nil))
                ((symbol-function 'pai-mcp--server-tools)
                 (lambda (_) (list (list :name "write" :inputSchema schema)))))
        (cl-labels ((request (args &optional dir)
                      (pai-mcp-guard-approve "server" "write" args
                                              (lambda () (cl-incf allowed))
                                              (lambda () (cl-incf denied)) dir)))
          (pai-mcp-guard-test--click (request '(:a 1 :b 2)) "Allow for session")
          (should-not (request '(:b 2 :a 1)))
          (should (= allowed 2))
          (pai-mcp-guard-test--click (request '(:a 2 :b 2)) "Deny")
          (setq schema '(:type "object" :required ["a"]))
          (pai-mcp-guard-test--click (request '(:a 1 :b 2)) "Deny")
          (pai-mcp-guard-test--click (request '(:a 1 :b 2) "/another-project/") "Deny")
          (setq pai--session (pai-session-create :id "guard-session-b"))
          (pai-mcp-guard-test--click (request '(:a 1 :b 2)) "Deny")
          (should (= allowed 2))
          (should (= denied 4)))))))

(ert-deftest pai-mcp-guard-approve-tools-precedence-and-patterns ()
  "Per-server false overrides global approval; glob names gate matching calls."
  (pai-mcp-guard-test--ui
    (let ((def '(:approveTools :false))
          (settings '(:approveTools t))
          (allowed 0) (denied 0))
      (cl-letf (((symbol-function 'pai-mcp--server) (lambda (_) (list :def def)))
                ((symbol-function 'pai-mcp-settings) (lambda (&optional _) settings))
                ((symbol-function 'pai-mcp--server-tools) (lambda (_) nil)))
        (cl-labels ((request (tool)
                      (pai-mcp-guard-approve "server" tool nil
                                              (lambda () (cl-incf allowed))
                                              (lambda () (cl-incf denied)))))
          (should-not (request "write"))
          (setq def nil settings '(:approveTools ["server_write*"]))
          (should-not (request "read"))
          (pai-mcp-guard-test--click (request "write_file") "Deny")
          (should (= allowed 2))
          (should (= denied 1)))))))

(ert-deftest pai-mcp-guard-legacy-approval-collision ()
  "A legacy underscore selector cannot accidentally select another real tool."
  (let ((metadata '((:name "delete-file"))))
    (cl-letf (((symbol-function 'pai-mcp--server-tools) (lambda (_) metadata)))
      (should (pai-mcp-guard--required-p
               "server" "delete-file" '(:approveTools ["delete_file"]) nil))
      (setq metadata '((:name "delete-file") (:name "delete_file")))
      (should-not (pai-mcp-guard--required-p
                   "server" "delete-file" '(:approveTools ["delete_file"]) nil))
      (should (pai-mcp-guard--required-p
               "server" "delete_file" '(:approveTools ["delete_file"]) nil)))))

(defmacro pai-mcp-guard-test--files (&rest body)
  "Run BODY with isolated local spill storage and no environment override."
  (declare (indent 0))
  `(let* ((temporary-file-directory (file-name-as-directory (make-temp-file "pai-guard-test-" t)))
          (process-environment (copy-sequence process-environment)))
     (setenv "MCP_OUTPUT_GUARD" nil)
     (cl-letf (((symbol-function 'pai-mcp-settings) (lambda (&optional _) nil)))
       (unwind-protect (progn ,@body)
         (delete-directory temporary-file-directory t)))))

(ert-deftest pai-mcp-guard-text-spill-is-bounded-private-and-lossless ()
  "Oversize error text stays bounded while its private artifact is lossless."
  (pai-mcp-guard-test--files
    (let* ((original (mapconcat (lambda (_) (make-string 80 ?λ)) (number-sequence 1 30) "\n"))
           (result (pai-mcp-guard-result
                    (list :content (list (list :type "text" :text original)) :isError t)
                    nil '(:outputGuard (:maxBytes 300 :maxLines 3 :detailsMaxBytes 200))))
           (text (pai-content-text (plist-get result :content)))
           (guard (plist-get (plist-get result :details) :outputGuard))
           (path (plist-get guard :path))
           (raw (plist-get (plist-get result :details) :mcpResult)))
      (should (eq (plist-get result :is-error) t))
      (should (<= (pai-mcp-guard--bytes text) 300))
      (should (<= (length (split-string text "\n")) 3))
      (should (string-match-p (regexp-quote path) text))
      (should (= (logand (file-modes path) #o777) #o600))
      (should (= (logand (file-modes (file-name-directory path)) #o777) #o700))
      (should (equal original (with-temp-buffer (insert-file-contents path) (buffer-string))))
      (should (<= (pai-mcp-guard--bytes (pai-json-encode raw)) 200))
      (should (plist-get raw :omitted)))))

(ert-deftest pai-mcp-guard-binary-spill-preserves-bytes-and-small-images ()
  "Binary data is not silently dropped; aggregate image size is bounded."
  (pai-mcp-guard-test--files
    (let* ((bytes (unibyte-string 0 1 255 128 10))
           (large (base64-encode-string (apply #'concat (make-list 200 bytes)) t))
           (result (pai-mcp-guard-result
                    (list :content (list (list :type "image" :data "AA==" :mimeType "image/png")
                                         (list :type "image" :data large :mimeType "image/png")))
                    nil '(:outputGuard (:maxBytes 256))))
           (content (plist-get result :content))
           (text (pai-content-text content)))
      (should (equal (plist-get (cadr content) :data) "AA=="))
      (should (string-match "saved to: \\([^]\n]+\\)" text))
      (let ((path (match-string 1 text)))
        (should (equal (base64-decode-string large)
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally path)
                         (buffer-string)))))
      (should (eq (plist-get result :is-error) :false)))))

(ert-deftest pai-mcp-guard-tiny-budgets-disable-and-write-failure ()
  "Tiny text budgets remain hard limits; disabled guard and write errors are explicit."
  (pai-mcp-guard-test--files
    (let* ((protocol (list :content (list (list :type "text" :text (make-string 1000 ?界)))))
           (tiny (pai-mcp-guard-result protocol nil '(:outputGuard (:maxBytes 2 :maxLines 1))))
           (unbounded (pai-mcp-guard-result protocol nil '(:outputGuard :false))))
      (should (<= (pai-mcp-guard--bytes (pai-content-text (plist-get tiny :content))) 2))
      (should (file-exists-p (plist-get (plist-get (plist-get tiny :details) :outputGuard) :path)))
      (should (equal (pai-content-text (plist-get unbounded :content)) (make-string 1000 ?界)))
      (cl-letf (((symbol-function 'make-temp-file) (lambda (&rest _) (error "Disk unavailable"))))
        (let* ((result (pai-mcp-guard-result protocol nil '(:outputGuard (:maxBytes 256))))
               (text (pai-content-text (plist-get result :content))))
          (should (string-match-p "could not be saved" text))
          (should (<= (pai-mcp-guard--bytes text) 256))
          (should (eq (plist-get result :is-error) :false)))))))

(provide 'pai-mcp-guard-test)
;;; pai-mcp-guard-test.el ends here
