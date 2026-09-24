;;; pai-mcp-interactions-test.el --- MCP interaction behavior -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'pai-mcp-interactions)
(require 'pai-mcp-guard)

(defmacro pai-mcp-interactions-test--with-state (&rest body)
  "Run BODY with isolated transports, providers, and captured replies."
  (declare (indent 0))
  `(let* ((pai-mcp-interactions--pending (make-hash-table :test 'equal))
          (pai--providers (make-hash-table :test 'equal))
          (pai--models (make-hash-table :test 'equal))
          (pai-api-keys '(("fixture" . "test-key")))
          (server (list :name "fixture" :status 'ready :generation 1 :origin-buffer (current-buffer)))
          (model (pai-make-model :id "model" :provider "fixture" :api "fixture"))
          replies)
     (pai-register-model model)
     (cl-letf (((symbol-function 'pai-mcp--send) (lambda (_server frame) (push frame replies)))
               ((symbol-function 'pai-mcp-settings) (lambda (&optional _dir) nil)))
       (unwind-protect (progn ,@body)
         (pai-mcp-interactions-cancel-server server)))))

(defconst pai-mcp-interactions-test--sample
  '(:id 12 :method "sampling/createMessage"
    :params (:maxTokens 24 :messages ((:role "user" :content (:type "text" :text "Summarize the weather"))))))

(defun pai-mcp-interactions-test--click (buffer label)
  "Activate the native widget with LABEL in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward label)
    (let ((widget (or (widget-at (1- (point))) (widget-at (- (point) (length label))))))
      (unless widget (error "No widget %s" label))
      (widget-apply-action widget))))

(ert-deftest pai-mcp-interactions-headless-does-not-sample ()
  "Headless requests never spend tokens absent explicit auto approval."
  (pai-mcp-interactions-test--with-state
    (let ((noninteractive t) called)
      (pai-register-provider (list :id "fixture" :stream (lambda (&rest _) (setq called t))))
      (should (pai-mcp-interactions-dispatch server pai-mcp-interactions-test--sample))
      (should-not called)
      (should (plist-get (car replies) :error))
      (should (= 12 (plist-get (car replies) :id))))))

(ert-deftest pai-mcp-interactions-sampling-two-approval-boundaries ()
  "The provider starts only after approval; output is not shared until approved."
  (pai-mcp-interactions-test--with-state
    (let (approvals emit received-context)
      (pai-register-provider
       (list :id "fixture" :stream (lambda (_model context _options callback)
                                     (setq received-context context emit callback))))
      (cl-letf (((symbol-function 'pai-mcp-guard-request)
                 (lambda (_key _title _body allow deny &optional _dir)
                   (setq approvals (append approvals (list (cons allow deny)))) nil)))
        (pai-mcp-interactions-dispatch server pai-mcp-interactions-test--sample)
        (should-not emit)
        (funcall (caar approvals))
        (should (equal "Summarize the weather"
                       (pai-content-text (plist-get (car (plist-get received-context :messages)) :content))))
        (funcall emit (list :type 'done :message
                            (pai-assistant-message :content (list (pai-thinking "private") (pai-text "Sunny"))
                                                   :stop-reason 'length)))
        (should-not replies)
        (should (= 2 (length approvals)))
        (funcall (car (nth 1 approvals)))
        (should (equal '(:role "assistant" :content (:type "text" :text "Sunny")
                        :model "fixture/model" :stopReason "maxTokens")
                       (plist-get (car replies) :result)))
        (funcall (car (nth 1 approvals)))
        (should (= 1 (length replies)))))))

(ert-deftest pai-mcp-interactions-sampling-denied-output-is-private ()
  (pai-mcp-interactions-test--with-state
    (let (approvals emit)
      (pai-register-provider (list :id "fixture" :stream (lambda (_m _c _o callback) (setq emit callback) nil)))
      (cl-letf (((symbol-function 'pai-mcp-guard-request)
                 (lambda (_key _title _body allow deny &optional _dir)
                   (push (cons allow deny) approvals) nil)))
        (pai-mcp-interactions-dispatch server pai-mcp-interactions-test--sample)
        (funcall (caar approvals))
        (funcall emit (list :type 'done :message (pai-assistant-message :content (list (pai-text "secret")) :stop-reason 'stop)))
        (funcall (cdar approvals))
        (should (plist-get (car replies) :error))
        (should-not (string-match-p "secret" (pai-json-encode replies)))))))

(ert-deftest pai-mcp-interactions-cancellation-aborts-and-discards-late-result ()
  (pai-mcp-interactions-test--with-state
    (let (emit aborted)
      (pai-register-provider (list :id "fixture" :stream (lambda (_m _c _o cb) (setq emit cb) 'handle)))
      (cl-letf (((symbol-function 'pai-mcp-settings) (lambda (&optional _) '(:samplingAutoApprove t)))
                ((symbol-function 'pai-provider-abort) (lambda (handle) (setq aborted handle))))
        (pai-mcp-interactions-dispatch server pai-mcp-interactions-test--sample)
        (pai-mcp-interactions-dispatch server '(:method "notifications/cancelled" :params (:requestId 12)))
        (should (eq 'handle aborted))
        (funcall emit (list :type 'done :message (pai-assistant-message :content (list (pai-text "late")) :stop-reason 'stop)))
        (should-not replies)
        (should (= 0 (hash-table-count pai-mcp-interactions--pending)))))))

(ert-deftest pai-mcp-interactions-restart-invalidates-approval ()
  (pai-mcp-interactions-test--with-state
    (let (allow started)
      (pai-register-provider (list :id "fixture" :stream (lambda (&rest _) (setq started t))))
      (cl-letf (((symbol-function 'pai-mcp-guard-request)
                 (lambda (_key _title _body yes _no &optional _dir) (setq allow yes) nil)))
        (pai-mcp-interactions-dispatch server pai-mcp-interactions-test--sample)
        (plist-put server :generation 2)
        (funcall allow)
        (should-not started)
        (should-not replies)))))

(ert-deftest pai-mcp-interactions-sampling-rejects-context-exfiltration ()
  (pai-mcp-interactions-test--with-state
    (let (started)
      (pai-register-provider (list :id "fixture" :stream (lambda (&rest _) (setq started t))))
      (cl-letf (((symbol-function 'pai-mcp-settings) (lambda (&optional _) '(:samplingAutoApprove t))))
        (pai-mcp-interactions-dispatch
         server '(:id 2 :method "sampling/createMessage" :params (:maxTokens 10 :includeContext "allServers")))
        (should-not started)
        (should (= -32602 (plist-get (plist-get (car replies) :error) :code)))))))

(ert-deftest pai-mcp-interactions-form-submit-validates-native-widgets ()
  (pai-mcp-interactions-test--with-state
    (let ((noninteractive nil) dialog)
      (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq dialog buffer))))
        (pai-mcp-interactions-dispatch
         server '(:id 0 :method "elicitation/create"
                  :params (:message "Preferences" :requestedSchema
                           (:type "object" :required ("age" "consent" "tags")
                            :properties (:age (:type "integer" :minimum 18 :default 21)
                                         :consent (:type "boolean" :default :false)
                                         :tags (:type "array" :items (:type "string" :enum ("a" "b")))
                                         :optional (:type "string"))))))
        (should-not replies)
        (pai-mcp-interactions-test--click dialog "Submit")
        (let ((content (plist-get (plist-get (car replies) :result) :content)))
          (should (= 21 (gethash "age" content)))
          (should (eq :false (gethash "consent" content)))
          (should (equal [] (gethash "tags" content)))
          (should-not (gethash "optional" content)))
        (should (= 0 (plist-get (car replies) :id)))
        (should-not (buffer-live-p dialog))))))

(ert-deftest pai-mcp-interactions-form-close-cancels-once ()
  (pai-mcp-interactions-test--with-state
    (let ((noninteractive nil) dialog)
      (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq dialog buffer))))
        (pai-mcp-interactions-dispatch server '(:id 1 :method "elicitation/create"
                                               :params (:message "Input" :requestedSchema (:type "object"))))
        (kill-buffer dialog)
        (should (equal '(:action "cancel") (plist-get (car replies) :result)))
        (should (= 1 (length replies)))))))

(ert-deftest pai-mcp-interactions-form-boundary-validation ()
  (should-error (pai-mcp-interactions--value "age" '(:type "integer" :minimum 18) "17"))
  (should-error (pai-mcp-interactions--value "age" '(:type "integer") "1.5"))
  (should-error (pai-mcp-interactions--value "number" '(:type "number") "invalid"))
  (should-error (pai-mcp-interactions--value "date" '(:type "string" :format "date") "2025-02-30"))
  (should-error (pai-mcp-interactions--value "choice" '(:type "string" :oneOf ((:const "a" :title "A"))) "b"))
  (should-error (pai-mcp-interactions--value "tags" '(:type "array" :minItems 1 :items (:enum ("a"))) nil))
  (should (equal "2024-02-29" (pai-mcp-interactions--value "date" '(:type "string" :format "date") "2024-02-29"))))

(ert-deftest pai-mcp-interactions-url-only-opens-after-click ()
  (pai-mcp-interactions-test--with-state
    (let ((noninteractive nil) dialog opened)
      (cl-letf (((symbol-function 'display-buffer) (lambda (buffer &rest _) (setq dialog buffer)))
                ((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (pai-mcp-interactions-dispatch
         server '(:id 2 :method "elicitation/create" :params (:mode "url" :url "https://example.org/consent"
                                                                  :elicitationId "consent" :message "Authorize")))
        (should-not opened)
        (should-not replies)
        (pai-mcp-interactions-test--click dialog "Open in browser")
        (should (equal opened "https://example.org/consent"))
        (should (equal '(:action "accept") (plist-get (car replies) :result)))))))

(ert-deftest pai-mcp-interactions-url-rejects-executable-schemes ()
  (pai-mcp-interactions-test--with-state
    (let (opened)
      (cl-letf (((symbol-function 'browse-url) (lambda (&rest _) (setq opened t))))
        (pai-mcp-interactions-dispatch
         server '(:id 3 :method "elicitation/create" :params (:mode "url" :url "javascript:alert(1)"
                                                                  :elicitationId "bad" :message "Open")))
        (should-not opened)
        (should (= -32602 (plist-get (plist-get (car replies) :error) :code)))))))

(ert-deftest pai-mcp-interactions-ui-notifications-do-not-send-protocol-responses ()
  (pai-mcp-interactions-test--with-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (should (pai-mcp-interactions-dispatch server '(:method "ui/message" :params (:type "notify" :message "Ready"))))
      (should-not replies)
      (should-not (pai-mcp-interactions-dispatch server '(:id 1 :result (:ok t)))))))

(provide 'pai-mcp-interactions-test)
;;; pai-mcp-interactions-test.el ends here
