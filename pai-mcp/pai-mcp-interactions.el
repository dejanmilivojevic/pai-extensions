;;; pai-mcp-interactions.el --- Server-originated MCP interactions -*- lexical-binding: t; -*-

;;; Commentary:
;; Requests are generation-bound and never read the minibuffer.  Elicitation
;; uses native widgets; sampling uses the actual asynchronous pai provider and
;; requires approval before both spending tokens and sharing the response.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'widget)
(require 'wid-edit)
(require 'browse-url)
(require 'url-parse)
(require 'pai-mcp-client)
(require 'pai-provider)
(require 'pai-model-resolver)

(declare-function pai-mcp-guard-request "pai-mcp-guard"
                  (key title body on-allow on-deny &optional dir))
(declare-function pai-send-message "pai-ui" (text &optional buffer))
(defvar pai--model)

(defvar pai-mcp-interactions--pending (make-hash-table :test 'equal)
  "Interaction keys to live request records.  Records contain closures.")

(defun pai-mcp-interactions--generation (server)
  "Return SERVER's current transport identity."
  (list (plist-get server :generation) (plist-get server :http-generation)
        (plist-get server :process)))

(defun pai-mcp-interactions--live-p (request)
  "Whether REQUEST still belongs to its original connection."
  (and (not (plist-get request :done))
       (equal (plist-get request :generation)
              (pai-mcp-interactions--generation (plist-get request :server)))
       (memq (plist-get (plist-get request :server) :status) '(ready starting))))

(defun pai-mcp-interactions--close (request)
  "Release REQUEST without sending a reply."
  (plist-put request :done t)
  (remhash (plist-get request :key) pai-mcp-interactions--pending)
  (when-let ((handle (plist-get request :handle)))
    (pai-provider-abort handle))
  (dolist (buffer (plist-get request :buffers))
    (when (buffer-live-p buffer) (kill-buffer buffer))))

(defun pai-mcp-interactions--reply (request result &optional error)
  "Finish REQUEST with RESULT or protocol ERROR, exactly once."
  (let ((live (pai-mcp-interactions--live-p request)))
    (unless (plist-get request :done)
      (pai-mcp-interactions--close request)
      (when (and live (plist-get request :has-id))
        (pai-mcp--send
         (plist-get request :server)
         (append (list :jsonrpc "2.0" :id (plist-get request :id))
                 (if error (list :error error) (list :result result))))))))

(defun pai-mcp-interactions--error (request text &optional code)
  "Reject REQUEST with TEXT and optional JSON-RPC CODE."
  (pai-mcp-interactions--reply request nil
                               (list :code (or code -32603) :message text)))

(defun pai-mcp-interactions-cancel-server (server)
  "Cancel pending dialogs and provider calls for SERVER without replying."
  (let (requests)
    (maphash (lambda (_key request)
               (when (eq server (plist-get request :server)) (push request requests)))
             pai-mcp-interactions--pending)
    (mapc #'pai-mcp-interactions--close requests)))

(defun pai-mcp-interactions--buffer (request title)
  "Make a native dialog for REQUEST with TITLE."
  (let ((buffer (generate-new-buffer (format "*MCP %s: %s*" title
                                            (plist-get (plist-get request :server) :name)))))
    (push buffer (plist-get request :buffers))
    (with-current-buffer buffer
      (setq-local default-directory (plist-get request :directory))
      (use-local-map widget-keymap)
      (insert title "\nServer: " (plist-get (plist-get request :server) :name) "\n\n")
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (pcase (plist-get request :method)
                    ("elicitation/create" (pai-mcp-interactions--reply request '(:action "cancel")))
                    ("sampling/createMessage" (pai-mcp-interactions--error request "Sampling cancelled"))
                    (_ (pai-mcp-interactions--reply request (pai-json-empty-object))))) nil t))
    buffer))

(defun pai-mcp-interactions--button (label action)
  "Insert a widget button LABEL invoking ACTION without arguments."
  (widget-create 'push-button :notify (lambda (&rest _) (funcall action)) label)
  (insert " "))

(defun pai-mcp-interactions--show (buffer)
  "Finish and display BUFFER without waiting for user input."
  (with-current-buffer buffer
    (widget-setup)
    (goto-char (point-min)))
  (display-buffer buffer))

(defun pai-mcp-interactions--choices (schema)
  "Return labeled string choices in primitive elicitation SCHEMA."
  (let ((variants (or (plist-get schema :oneOf) (plist-get schema :anyOf))))
    (if variants
        (mapcar (lambda (item) (cons (plist-get item :const)
                                    (or (plist-get item :title) (plist-get item :const)))) variants)
      (cl-loop for value in (plist-get schema :enum) for index from 0
               collect (cons value (or (nth index (plist-get schema :enumNames)) value))))))

(defun pai-mcp-interactions--field (schema)
  "Insert and return a native value widget for SCHEMA."
  (let* ((type (plist-get schema :type))
         (default (plist-get schema :default))
         (choices (pai-mcp-interactions--choices schema)))
    (cond
     ((equal type "boolean")
      (widget-create 'checkbox :value (eq default t)))
     ((and (equal type "string") choices)
      (apply #'widget-create 'radio-button-choice
             :value (or default (caar choices))
             (mapcar (lambda (pair) (list 'const :tag (cdr pair) (car pair))) choices)))
     ((equal type "array")
      (apply #'widget-create 'checklist :value default
             (mapcar (lambda (pair) (list 'const :tag (cdr pair) (car pair)))
                     (pai-mcp-interactions--choices (plist-get schema :items)))))
     ((member type '("string" "number" "integer"))
      (widget-create 'editable-field :size 45 :value (if (null default) "" (format "%s" default))))
     (t (error "Unsupported elicitation field type: %s" type)))))

(defun pai-mcp-interactions--bound (value schema minimum maximum name)
  "Validate VALUE against SCHEMA's MINIMUM and MAXIMUM, naming field NAME."
  (when (and (numberp (plist-get schema minimum)) (< value (plist-get schema minimum)))
    (error "%s is below %s" name (plist-get schema minimum)))
  (when (and (numberp (plist-get schema maximum)) (> value (plist-get schema maximum)))
    (error "%s exceeds %s" name (plist-get schema maximum))))

(defun pai-mcp-interactions--value (name schema value)
  "Coerce and validate elicitation VALUE according to primitive SCHEMA."
  (pcase (plist-get schema :type)
    ("string"
     (unless (stringp value) (error "%s must be text" name))
     (pai-mcp-interactions--bound (length value) schema :minLength :maxLength name)
     (when-let ((choices (pai-mcp-interactions--choices schema)))
       (unless (assoc value choices) (error "%s is not an allowed choice" name)))
     (when-let ((format (plist-get schema :format)))
       (unless
           (pcase format
             ("email" (string-match-p "\\`[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+\\'" value))
             ("uri" (string-match-p "\\`[A-Za-z][A-Za-z0-9+.-]*:[^[:space:]]+\\'" value))
             ((or "date" "date-time")
              (and (string-match-p
                    (if (equal format "date") "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
                      "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\(?:\\.[0-9]+\\)?\\(?:Z\\|[+-][0-9]\\{2\\}:[0-9]\\{2\\}\\)\\'") value)
                   (condition-case nil
                       (let* ((year (string-to-number (substring value 0 4)))
                              (month (string-to-number (substring value 5 7)))
                              (day (string-to-number (substring value 8 10)))
                              (decoded (decode-time (encode-time 0 0 12 day month year t) t)))
                         (and (= day (nth 3 decoded)) (= month (nth 4 decoded))
                              (= year (nth 5 decoded))
                              (or (equal format "date")
                                  (and (< (string-to-number (substring value 11 13)) 24)
                                       (< (string-to-number (substring value 14 16)) 60)
                                       (< (string-to-number (substring value 17 19)) 61)))))
                     (error nil))))
             (_ (error "Unsupported elicitation format: %s" format)))
         (error "%s is not a valid %s" name format)))
     value)
    ((or "number" "integer")
     (let ((number (if (numberp value) value
                     (condition-case nil (pai-json-decode value) (error nil)))))
       (unless (and (numberp number) (= number number)
                    (< (abs number) 1.0e+INF)
                    (or (equal (plist-get schema :type) "number") (= number (truncate number))))
         (error "%s must be a finite %s" name (plist-get schema :type)))
       (pai-mcp-interactions--bound number schema :minimum :maximum name)
       number))
    ("boolean" (if (eq value t) t :false))
    ("array"
     (pai-mcp-interactions--bound (length value) schema :minItems :maxItems name)
     (let ((choices (pai-mcp-interactions--choices (plist-get schema :items))))
       (unless (cl-every (lambda (item) (assoc item choices)) value)
         (error "%s contains an invalid selection" name)))
     (when (and (eq (plist-get schema :uniqueItems) t)
                (/= (length value) (length (delete-dups (copy-sequence value)))))
       (error "%s contains duplicate selections" name))
     (vconcat value))
    (_ (error "Unsupported elicitation field: %s" name))))

(defun pai-mcp-interactions--form (request params)
  "Display nonblocking form elicitation REQUEST with PARAMS."
  (let* ((schema (plist-get params :requestedSchema))
         (properties (plist-get schema :properties)))
    (unless (and (equal (plist-get schema :type) "object")
                 (or (null properties) (hash-table-p properties) (pai-json--plistp properties)))
      (error "Elicitation requires an object schema"))
    (if noninteractive
        (pai-mcp-interactions--reply request '(:action "decline"))
      (let ((buffer (pai-mcp-interactions--buffer request "Input request")) fields)
        (with-current-buffer buffer
          (insert (plist-get params :message) "\n\n")
          (dolist (pair (if (hash-table-p properties)
                           (let (pairs) (maphash (lambda (key value) (push (cons key value) pairs)) properties)
                                (nreverse pairs))
                         (pai-mcp--plist-to-alist properties)))
            (let* ((name (car pair)) (spec (cdr pair))
                   (required (member name (plist-get schema :required))))
              (insert (or (plist-get spec :title) name)
                      (if required " (required)" " (optional)") "\n")
              (when-let ((description (plist-get spec :description))) (insert description "\n"))
              (let ((included (unless required
                                (prog1 (widget-create 'checkbox :value (plist-member spec :default))
                                  (insert " Include this field\n"))))
                    (field (pai-mcp-interactions--field spec)))
                (push (list name spec included field) fields))
              (insert "\n\n")))
          (pai-mcp-interactions--button
           "Submit"
           (lambda ()
             (condition-case err
                 (let ((content (pai-json-empty-object)))
                   (dolist (field fields)
                     (when (or (not (nth 2 field)) (widget-value (nth 2 field)))
                       (puthash (car field)
                                (pai-mcp-interactions--value (car field) (nth 1 field)
                                                             (widget-value (nth 3 field))) content)))
                   (pai-mcp-interactions--reply request (list :action "accept" :content content)))
               (error (message "MCP input: %s" (error-message-string err))))))
          (pai-mcp-interactions--button "Decline" (lambda () (pai-mcp-interactions--reply request '(:action "decline"))))
          (pai-mcp-interactions--button "Cancel" (lambda () (pai-mcp-interactions--reply request '(:action "cancel")))))
        (pai-mcp-interactions--show buffer)))))

(defun pai-mcp-interactions--url (request params)
  "Display URL elicitation REQUEST without opening anything automatically."
  (let* ((url (plist-get params :url))
         (parsed (and (stringp url) (url-generic-parse-url url))))
    (unless (and parsed (member (url-type parsed) '("http" "https"))
                 (url-host parsed) (not (string-empty-p (url-host parsed)))
                 (not (string-match-p "[[:cntrl:]]" url))
                 (stringp (plist-get params :elicitationId)))
      (error "URL elicitation requires an HTTP(S) URL and elicitationId"))
    (if noninteractive
        (pai-mcp-interactions--reply request '(:action "decline"))
      (let ((buffer (pai-mcp-interactions--buffer request "Browser request")))
        (with-current-buffer buffer
          (insert (plist-get params :message) "\n\nHost: " (url-host parsed) "\nFull URL: " url "\n\n")
          (pai-mcp-interactions--button
           "Open in browser"
           (lambda ()
             (when (pai-mcp-interactions--live-p request)
               (condition-case err
                   (progn (browse-url url)
                          (pai-mcp-interactions--reply request '(:action "accept")))
                 (error (message "MCP browser: %s" (error-message-string err))
                        (pai-mcp-interactions--reply request '(:action "cancel")))))))
          (pai-mcp-interactions--button "Decline" (lambda () (pai-mcp-interactions--reply request '(:action "decline"))))
          (pai-mcp-interactions--button "Cancel" (lambda () (pai-mcp-interactions--reply request '(:action "cancel")))))
        (pai-mcp-interactions--show buffer)))))

(defun pai-mcp-interactions--approve (request title body continuation)
  "Ask for sampling approval or run CONTINUATION when explicitly configured."
  (when (pai-mcp-interactions--live-p request)
    (if (plist-get request :auto-approve)
        (funcall continuation)
      (require 'pai-mcp-guard)
      (let ((buffer
             (pai-mcp-guard-request
              nil title body
              (lambda () (when (pai-mcp-interactions--live-p request) (funcall continuation)))
              (lambda () (pai-mcp-interactions--error request "MCP sampling was declined"))
              (plist-get request :directory))))
        (when (buffer-live-p buffer) (push buffer (plist-get request :buffers)))))))

(defun pai-mcp-interactions--model (params origin)
  "Resolve sampling model hints in PARAMS with ORIGIN's model as fallback."
  (let* ((available (cl-remove-if-not #'pai-provider-for-model (pai-models)))
         (hints (plist-get (plist-get params :modelPreferences) :hints))
         (hinted
          (cl-loop for hint in hints for name = (plist-get hint :name)
                   when (and (stringp name) (not (string-empty-p (string-trim name))))
                   thereis (cl-find-if
                            (lambda (model)
                              (string-match-p (regexp-quote (downcase (string-trim name)))
                                              (downcase (concat (pai-model-key model) " "
                                                                (plist-get model :name))))) available))))
    (or hinted
        (and (buffer-live-p origin)
             (with-current-buffer origin
               (or (and (boundp 'pai--model) pai--model) (pai-scoped-model :main))))
        (car available)
        (error "No pai model is available for MCP sampling"))))

(defun pai-mcp-interactions--sampling-messages (params)
  "Convert PARAMS to pai messages, rejecting unsupported sampling features."
  (dolist (key '(:task :tools :toolChoice :stopSequences))
    (when (plist-get params key) (error "MCP sampling %s is not supported" key)))
  (when (and (plist-get params :includeContext)
             (not (equal (plist-get params :includeContext) "none")))
    (error "MCP sampling context inclusion is not supported"))
  (unless (and (integerp (plist-get params :maxTokens)) (> (plist-get params :maxTokens) 0))
    (error "MCP sampling maxTokens must be a positive integer"))
  (when (plist-member params :temperature)
    (unless (and (numberp (plist-get params :temperature))
                 (<= 0 (plist-get params :temperature) 2))
      (error "MCP sampling temperature must be between 0 and 2")))
  (let ((messages
         (mapcar
          (lambda (message)
            (let* ((raw (plist-get message :content))
                   (blocks (if (pai-json--plistp raw) (list raw) raw))
                   (content (mapcar
                             (lambda (block)
                               (unless (and (equal (plist-get block :type) "text")
                                            (stringp (plist-get block :text)))
                                 (error "MCP sampling supports text content only"))
                               (pai-text (plist-get block :text))) blocks)))
              (pcase (plist-get message :role)
                ("user" (pai-user-message content))
                ("assistant" (pai-assistant-message :content content :stop-reason 'stop))
                (_ (error "Invalid MCP sampling message role")))))
          (plist-get params :messages))))
    (when-let ((system (plist-get params :systemPrompt)))
      (unless (stringp system) (error "Sampling systemPrompt must be text"))
      (push (pai-system-message system) messages))
    messages))

(defun pai-mcp-interactions--sampling-result (message model)
  "Convert MESSAGE from MODEL into an MCP sampling response."
  (when (memq (plist-get message :stop-reason) '(error aborted))
    (error "%s" (or (plist-get message :error-message) "Sampling model failed")))
  (let ((text
         (string-trim
          (mapconcat
           #'identity
           (delq nil (mapcar (lambda (block)
                               (pcase (plist-get block :type)
                                 ('text (plist-get block :text))
                                 ('thinking nil)
                                 (_ (error "Sampling returned unsupported content"))))
                             (plist-get message :content))) "\n\n"))))
    (when (string-empty-p text) (error "Sampling result did not contain text"))
    (list :role "assistant" :content (list :type "text" :text text)
          :model (pai-model-key model)
          :stopReason (pcase (plist-get message :stop-reason)
                        ('length "maxTokens") ('stop "endTurn")
                        (_ (error "Unsupported sampling stop reason"))))))

(defun pai-mcp-interactions--with-key (request model callback)
  "Resolve MODEL credentials asynchronously and call CALLBACK with its key.
Only an explicitly configured credential command can start a shell."
  (let* ((provider (pai-model-provider model))
         (direct (cdr (assoc provider pai-api-keys)))
         (credential (and (not direct) (fboundp 'pai-auth-get) (pai-auth-get provider)))
         (raw (and (equal (plist-get credential :type) "api_key")
                   (plist-get credential :key))))
    (if (and (stringp raw) (string-prefix-p "!" raw))
        (let ((buffer (generate-new-buffer " *MCP sampling credential*")))
          (push buffer (plist-get request :buffers))
          (plist-put
           request :handle
           (make-process
            :name "pai-mcp-sampling-key" :buffer buffer :noquery t :connection-type 'pipe
            :command (list "sh" "-c" (substring raw 1))
            :sentinel
            (lambda (process _event)
              (when (and (memq (process-status process) '(exit signal))
                         (pai-mcp-interactions--live-p request))
                (plist-put request :handle nil)
                (if (and (eq (process-status process) 'exit) (= 0 (process-exit-status process)))
                    (funcall callback (with-current-buffer buffer (string-trim (buffer-string))))
                  (pai-mcp-interactions--error request "Sampling credential command failed")))))))
      (funcall callback
               (or direct
                   (and raw (pai-resolve-config-value raw))
                   (and (equal (plist-get credential :type) "oauth") (plist-get credential :access))
                   (seq-some (lambda (name) (let ((value (getenv name)))
                                             (and value (not (string-empty-p value)) value)))
                             (cdr (assoc provider pai-provider-env-keys)))
                   "")))))

(defun pai-mcp-interactions--stream (request params messages model key)
  "Start the provider stream for approved REQUEST using resolved KEY."
  (when (pai-mcp-interactions--live-p request)
    (let ((buffer (if noninteractive (generate-new-buffer " *MCP sampling*")
                    (pai-mcp-interactions--buffer request "Sampling in progress"))))
      (when noninteractive (push buffer (plist-get request :buffers)))
      (with-current-buffer buffer
        (setq-local default-directory (plist-get request :directory))
        (unless noninteractive
          (insert "Waiting for " (pai-model-key model) "\n\n")
          (pai-mcp-interactions--button "Cancel"
                                       (lambda () (pai-mcp-interactions--error request "Sampling cancelled")))
          (pai-mcp-interactions--show buffer))
        (condition-case err
            (let ((handle
                   (pai-provider-stream
                    model (pai-context messages)
                    (list :api-key key :max-tokens (plist-get params :maxTokens)
                          :temperature (plist-get params :temperature))
                    (lambda (event)
                      (when (and (pai-mcp-interactions--live-p request)
                                 (not (plist-get request :model-done))
                                 (memq (plist-get event :type) '(done error)))
                        (plist-put request :model-done t)
                        (plist-put request :handle nil)
                        (condition-case failure
                            (let ((result (pai-mcp-interactions--sampling-result
                                           (plist-get event :message) model)))
                              (pai-mcp-interactions--approve
                               request "Return MCP sampling response"
                               (format "%s will receive this response from %s:\n\n%s"
                                       (plist-get (plist-get request :server) :name) (pai-model-key model)
                                       (plist-get (plist-get result :content) :text))
                               (lambda () (pai-mcp-interactions--reply request result))))
                          (error (pai-mcp-interactions--error request (error-message-string failure)))))))))
              (unless (or (plist-get request :done) (plist-get request :model-done))
                (plist-put request :handle handle)))
          (error (pai-mcp-interactions--error request (error-message-string err))))))))

(defun pai-mcp-interactions--sample (request params)
  "Run an approved asynchronous model call for REQUEST and PARAMS."
  (let* ((messages (pai-mcp-interactions--sampling-messages params))
         (model (pai-mcp-interactions--model params (plist-get request :origin)))
         (server (plist-get request :server))
         (settings (pai-mcp-settings (plist-get request :directory))))
    (plist-put request :auto-approve (eq (plist-get settings :samplingAutoApprove) t))
    (pai-mcp-interactions--approve
     request "Approve MCP sampling request"
     (format "%s requests %s (maximum %s tokens).\n\n%s"
             (plist-get server :name) (pai-model-key model) (plist-get params :maxTokens)
             (mapconcat (lambda (m) (format "%s: %s" (plist-get m :role)
                                            (pai-content-text (plist-get m :content)))) messages "\n\n"))
     (lambda ()
       (condition-case err
           (pai-mcp-interactions--with-key
            request model
            (lambda (key) (pai-mcp-interactions--stream request params messages model key)))
         (error (pai-mcp-interactions--error request (error-message-string err))))))))

(defun pai-mcp-interactions--ui (request params context-p)
  "Present UI PARAMS; CONTEXT-P identifies a model-context update.
Only an explicit button press may send server-originated text to the agent."
  (let* ((server (plist-get request :server))
         (text (or (plist-get params :prompt)
                   (and (plist-get params :content)
                        (mapconcat (lambda (block) (or (plist-get block :text) ""))
                                   (plist-get params :content) "\n"))
                   (and (plist-get params :intent)
                        (format "Intent: %s\n%s" (plist-get params :intent)
                                (pai-json-encode (or (plist-get params :params) (pai-json-empty-object)))))
                   (plist-get params :message))))
    (when context-p
      (setq text (string-join
                  (delq nil (list text (and (plist-get params :structuredContent)
                                           (pai-json-encode (plist-get params :structuredContent))))) "\n")))
    (unless (and (stringp text) (not (string-empty-p text))) (error "UI message has no text"))
    (if (or noninteractive (equal (plist-get params :type) "notify"))
        (progn (message "MCP[%s]: %s" (plist-get server :name) text)
               (pai-mcp-interactions--reply request (pai-json-empty-object)))
      (let ((buffer (pai-mcp-interactions--buffer request (if context-p "UI context" "UI message"))))
        (with-current-buffer buffer
          (insert text "\n\nThis is server-provided content, not an instruction from you.\n\n")
          (pai-mcp-interactions--button
           "Send to pai"
           (lambda ()
             (when (pai-mcp-interactions--live-p request)
               (condition-case err
                   (let ((origin (plist-get request :origin)))
                     (unless (and (buffer-live-p origin)
                                  (with-current-buffer origin (derived-mode-p 'pai-mode)))
                       (error "The originating pai buffer is no longer available"))
                     (pai-send-message (format "MCP UI content from %s:\n%s" (plist-get server :name) text) origin)
                     (pai-mcp-interactions--reply request (pai-json-empty-object)))
                 (error (message "MCP UI: %s" (error-message-string err)))))))
          (pai-mcp-interactions--button "Dismiss"
                                       (lambda () (pai-mcp-interactions--reply request (pai-json-empty-object)))))
        (pai-mcp-interactions--show buffer)))))

(defun pai-mcp-interactions-dispatch (server message)
  "Handle a server-originated MESSAGE on SERVER; return non-nil if handled.
Call only for method-bearing frames, before normal JSON-RPC response routing."
  (let ((method (plist-get message :method)) (params (plist-get message :params)))
    (cond
     ((equal method "notifications/cancelled")
      (when-let ((request (gethash (list (plist-get server :name) (plist-get params :requestId))
                                   pai-mcp-interactions--pending)))
        (pai-mcp-interactions--close request))
      t)
     ((member method '("elicitation/create" "sampling/createMessage" "ui/message" "ui/update-model-context"))
      (let* ((id (plist-get message :id))
             (key (list (plist-get server :name) (if (plist-member message :id) id (make-symbol "notification"))))
             (origin (or (plist-get server :origin-buffer) (current-buffer)))
             (request (list :server server :id id :key key :done nil :buffers nil :handle nil
                            :has-id (plist-member message :id) :origin origin :method method
                            :directory (or (plist-get server :directory)
                                           (and (buffer-live-p origin) (buffer-local-value 'default-directory origin))
                                           default-directory)
                            :generation (pai-mcp-interactions--generation server))))
        (unless (gethash key pai-mcp-interactions--pending)
          (puthash key request pai-mcp-interactions--pending)
          (condition-case err
              (pcase method
                ("elicitation/create"
                 (unless (plist-member message :id) (error "Elicitation requires a request id"))
                 (unless (stringp (plist-get params :message)) (error "Elicitation message must be text"))
                 (pcase (plist-get params :mode)
                   ((or 'nil "form") (pai-mcp-interactions--form request params))
                   ("url" (pai-mcp-interactions--url request params))
                   (_ (error "Unknown elicitation mode"))))
                ("sampling/createMessage"
                 (unless (plist-member message :id) (error "Sampling requires a request id"))
                 (pai-mcp-interactions--sample request params))
                (_ (pai-mcp-interactions--ui request params (equal method "ui/update-model-context"))))
            (error (pai-mcp-interactions--error request (error-message-string err) -32602))))
        t))
     (t nil))))

(provide 'pai-mcp-interactions)
;;; pai-mcp-interactions.el ends here
