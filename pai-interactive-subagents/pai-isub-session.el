;;; pai-isub-session.el --- The built-in "pai" subagent backend -*- lexical-binding: t; -*-

;;; Commentary:

;; The default backend: every subagent is a full, live pai chat session in its
;; own buffer opened next to the parent's.  It is a normal `pai-mode' buffer,
;; so the user can read it, scroll it, and type into it exactly like any other
;; pai session -- the only differences are:
;;
;;   * a role system prompt is folded into its context at request time
;;     (see `pai-isub-session-context-hook'), never written to the session file;
;;   * it owns a `reply_to_parent' tool, the child's explicit way to talk to
;;     the agent that spawned it;
;;   * the outcome of every turn the parent asked for is reported back
;;     automatically (see `pai-isub-session-agent-end-hook');
;;   * the `subagent' tool is removed unless `pai-isub-allow-nested' is set.
;;
;; Everything here is registered as one backend plist (see `pai-isub-backend'),
;; so another backend -- an `agent-shell' buffer, a remote session, a plain
;; comint process -- can be dropped in beside it without touching the rest of
;; the extension.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai)
(require 'pai-tools)
(require 'pai-isub-backend)
(require 'pai-isub-roles)

(defcustom pai-isub-allow-nested nil
  "When non-nil, subagent sessions keep the `subagent' tool themselves.
Off by default: children cannot spawn grandchildren."
  :type 'boolean
  :group 'pai-isub)

(defconst pai-isub-reply-tool-name "reply_to_parent"
  "Name of the tool a child session uses to talk to its parent.")

;;;; Child-session state (buffer-local in the child's chat buffer)

(defvar-local pai-isub--emit nil
  "Function (TYPE &rest PROPS) reporting this child's events to its parent.")
(defvar-local pai-isub--parent nil "The parent chat buffer of this session.")
(defvar-local pai-isub--id nil "This child's run id, e.g. \"sub-2\".")
(defvar-local pai-isub--role nil "This child's role name.")
(defvar-local pai-isub--role-prompt nil "This child's role system prompt.")
(defvar-local pai-isub--stream-chars 0 "Characters streamed in the current turn.")
(defvar-local pai-isub--usage-tokens 0 "Output tokens billed to this session.")
(defvar-local pai-isub--started nil "Float time this session was created.")

(defun pai-isub-child-p (&optional buffer)
  "Return non-nil when BUFFER (default current) is a subagent session."
  (and (buffer-live-p (or buffer (current-buffer)))
       (buffer-local-value 'pai-isub--id (or buffer (current-buffer)))
       t))

(defun pai-isub-child-emit (type &rest props)
  "Report TYPE with PROPS from the current child session to its parent.
A no-op in a buffer that is not a subagent session."
  (when (functionp pai-isub--emit)
    (apply pai-isub--emit type props)))

;;;; Role prompt injection (context hook)

(defconst pai-isub--protocol-prompt
  "You are running as a subagent session spawned by a parent agent.
- The parent delegated a task to you; your final message of a turn the parent
  asked for is reported back to it automatically, so end such turns with the
  report you want the parent to read.
- A human user may also type directly into this session at any time. Treat
  those turns as coming from the user, not from the parent.
- Use the `reply_to_parent' tool whenever you want to tell the parent
  something in the middle of your work: a question, a decision you need, a
  partial finding, or a warning. Keep those messages short and specific.
- Stay inside the task you were given; report instead of expanding scope."
  "Protocol text appended to every subagent session's system prompt.")

(defun pai-isub-session--role-section ()
  "Return the system-prompt addendum describing this session's role."
  (format "<subagent_session role=\"%s\" id=\"%s\">\n%s\n\n%s\n</subagent_session>"
          (or pai-isub--role "subagent")
          (or pai-isub--id "?")
          (string-trim (or pai-isub--role-prompt ""))
          pai-isub--protocol-prompt))

(defun pai-isub-session--append-to-system (message extra)
  "Return MESSAGE (a system message) with EXTRA appended to its content."
  (let ((content (pai-message-content message)))
    (plist-put (copy-sequence message) :content
               (if (stringp content)
                   (concat content "\n\n" extra)
                 (append content (list (pai-text (concat "\n\n" extra))))))))

(defun pai-isub-session-context-hook (event _ctx)
  "Fold this session's role prompt into the system message of EVENT's messages.
Registered as a `context' handler; a no-op outside subagent sessions."
  (when pai-isub--role-prompt
    (let ((messages (plist-get event :messages))
          (extra (pai-isub-session--role-section))
          (done nil))
      (list :messages
            (if (seq-find #'pai-system-message-p messages)
                (mapcar (lambda (m)
                          (if (and (not done) (pai-system-message-p m))
                              (progn (setq done t)
                                     (pai-isub-session--append-to-system m extra))
                            m))
                        messages)
              (cons (pai-system-message extra) messages))))))

;;;; Turn observation (metrics + reporting back)

(defun pai-isub-session-agent-start-hook (_event _ctx)
  "Tell the parent a turn started.  A no-op outside subagent sessions."
  (when pai-isub--id
    (setq pai-isub--stream-chars 0)
    (pai-isub-child-emit 'busy)))

(defun pai-isub-session-message-update-hook (event _ctx)
  "Accumulate streamed characters from EVENT for the live token estimate."
  (when pai-isub--id
    (let ((se (plist-get event :event)))
      (when (memq (plist-get se :type) '(text-delta thinking-delta))
        (setq pai-isub--stream-chars
              (+ pai-isub--stream-chars (length (or (plist-get se :delta) ""))))))))

(defun pai-isub-session-message-end-hook (event _ctx)
  "Fold EVENT's real usage into this session's token counter."
  (when pai-isub--id
    (let ((m (plist-get event :message)))
      (when (pai-assistant-message-p m)
        (setq pai-isub--usage-tokens
              (+ pai-isub--usage-tokens
                 (or (plist-get (plist-get m :usage) :output) 0))
              pai-isub--stream-chars 0)))))

(defun pai-isub-session--final-text (messages)
  "Return the last assistant text in MESSAGES."
  (let ((last (seq-find #'pai-assistant-message-p (reverse messages))))
    (if last
        (string-trim (or (pai-content-text (pai-message-content last)) ""))
      "")))

(defun pai-isub-session-agent-end-hook (event _ctx)
  "Report the finished turn in EVENT back to the parent."
  (when pai-isub--id
    (pai-isub-child-emit 'turn-end
                         :text (pai-isub-session--final-text
                                (plist-get event :messages)))))

(defun pai-isub-session--on-kill ()
  "Tell the parent this session is gone."
  (when pai-isub--id (pai-isub-child-emit 'exit :reason 'killed)))

(defconst pai-isub-session--hooks
  '((context . pai-isub-session-context-hook)
    (agent-start . pai-isub-session-agent-start-hook)
    (message-update . pai-isub-session-message-update-hook)
    (message-end . pai-isub-session-message-end-hook)
    (agent-end . pai-isub-session-agent-end-hook))
  "Event -> handler pairs a child session needs to report home.")

(defun pai-isub-session-install-hooks ()
  "Make sure this buffer's instance runs the child-session handlers.
`pai-interactive-subagents' registers them for every instance it is loaded
into; installing them here as well (idempotently) keeps a child working even
when its own instance did not load the extension from disk."
  (pcase-dolist (`(,event . ,handler) pai-isub-session--hooks)
    (unless (rassq handler (gethash event pai--ext-handlers))
      (pai-ext-on nil event handler))))

;;;; The reply tool (child -> parent)

(defun pai-isub-session--reply-execute (args _ctx _on-update on-done)
  "Execute `reply_to_parent' with ARGS, completing through ON-DONE."
  (let ((text (string-trim (or (plist-get args :message) ""))))
    (cond
     ((not (functionp pai-isub--emit))
      (funcall on-done (pai-tool-error-result
                        "This session has no parent agent to reply to")))
     ((string-empty-p text)
      (funcall on-done (pai-tool-error-result "reply_to_parent requires a message")))
     (t
      (pai-isub-child-emit 'reply :text text)
      (funcall on-done
               (list :content (list (pai-text "Delivered to the parent agent."))
                     :is-error :false))))))

(defvar pai-isub-session-reply-tool
  (list :name pai-isub-reply-tool-name
        :label "Reply to parent"
        :description
        "Send a message to the parent agent that spawned this subagent session. Use it to ask a question, surface a decision you need, report a partial finding, or warn about something before the task ends. The parent receives it as soon as its current turn allows."
        :prompt-snippet "reply_to_parent: message the parent agent that spawned you"
        :execution-mode 'sequential
        :parameters (pai-object-schema
                     (list :message (pai-string-schema
                                     "What to tell the parent agent."))
                     '("message"))
        :execute #'pai-isub-session--reply-execute)
  "Tool plist registered inside every pai-backed subagent session.")

;;;; Slash command: /parent

(defun pai-isub-session-parent-command (args _ctx)
  "Handler for `/parent TEXT': send TEXT from this session to its parent."
  (let ((text (string-trim args)))
    (cond
     ((not pai-isub--id) (list :message "This buffer is not a subagent session"))
     ((string-empty-p text)
      (list :message (format "Subagent %s (%s); parent: %s"
                             pai-isub--id pai-isub--role
                             (if (buffer-live-p pai-isub--parent)
                                 (buffer-name pai-isub--parent)
                               "(gone)"))))
     (t (pai-isub-child-emit 'reply :text text)
        (list :message (format "Sent to the parent agent: %s" text))))))

;;;; Backend implementation

(defun pai-isub-session--buffer-name (spec)
  "Return a fresh buffer name for the child described by SPEC."
  (generate-new-buffer-name
   (format "*pai sub: %s [%s]*" (plist-get spec :role) (plist-get spec :id))))

(defun pai-isub-session--restrict-tools (spec)
  "Drop tools this child may not use, per SPEC, in the current buffer."
  (unless (pai-isub-nested-allowed-p) (pai-unregister-tool "subagent"))
  (let ((allow (plist-get spec :tools)))
    (when allow
      (dolist (tool (pai-tools-all))
        (let ((name (plist-get tool :name)))
          (unless (or (member name allow)
                      (equal name pai-isub-reply-tool-name))
            (pai-unregister-tool name))))))
  (pai-register-tool pai-isub-session-reply-tool))

(defun pai-isub-session--adopt-context (messages)
  "Append inherited MESSAGES to this session's context and redraw it."
  (when messages
    (setq pai--context-messages (append pai--context-messages messages))
    (when pai--session
      (dolist (m messages) (pai-session-append-message pai--session m)))
    (pai--rebuild-transcript)))

(defun pai-isub-session-start (spec)
  "Create a pai chat buffer for the child described by SPEC; return its handle."
  (let* ((parent (plist-get spec :parent))
         (cwd (or (plist-get spec :cwd) default-directory))
         (trusted (and (buffer-live-p parent)
                       (buffer-local-value 'pai--trusted parent)))
         (buffer (generate-new-buffer (pai-isub-session--buffer-name spec))))
    (with-current-buffer buffer
      ;; Inherit the parent's trust decision instead of prompting mid-tool-call.
      (cl-letf (((symbol-function 'pai--project-trusted-p) (lambda () trusted)))
        (pai--setup cwd))
      (setq pai-isub--emit (plist-get spec :emit)
            pai-isub--parent parent
            pai-isub--id (plist-get spec :id)
            pai-isub--role (plist-get spec :role)
            pai-isub--role-prompt (plist-get spec :role-prompt)
            pai-isub--stream-chars 0
            pai-isub--usage-tokens 0
            pai-isub--started (float-time))
      (when (plist-get spec :model) (setq pai--model (plist-get spec :model)))
      (when (plist-get spec :thinking) (setq pai--reasoning (plist-get spec :thinking)))
      (pai-isub-session-install-hooks)
      (pai-isub-session--restrict-tools spec)
      (pai-isub-session--adopt-context (plist-get spec :context-messages))
      (add-hook 'kill-buffer-hook #'pai-isub-session--on-kill nil t)
      (pai--render-note
       (format "subagent %s · role %s · parent %s\nType here to talk to this agent; /parent TEXT messages the parent."
               (plist-get spec :id) (plist-get spec :role)
               (if (buffer-live-p parent) (buffer-name parent) "(gone)")))
      (pai--set-status "idle"))
    (pai-isub-display buffer parent)
    (let ((emit (plist-get spec :emit)))
      (when (functionp emit) (funcall emit 'ready)))
    (list :buffer buffer :backend "pai")))

(defun pai-isub-session-send (handle text)
  "Submit TEXT as a user turn in child HANDLE's buffer.
Anything the user had half-typed at the prompt is preserved."
  (let ((buffer (plist-get handle :buffer)))
    (unless (buffer-live-p buffer)
      (error "Subagent session buffer is gone"))
    (when (string-empty-p (string-trim (or text "")))
      (error "Refusing to send an empty message to a subagent"))
    (with-current-buffer buffer
      (let ((pending (pai--input-text)))
        (pai--clear-input)
        (goto-char (point-max))
        (insert text)
        (pai-send)
        (when (and pending (not (string-empty-p pending)))
          (goto-char (point-max))
          (insert pending))))
    t))

(defun pai-isub-session-busy-p (handle)
  "Return non-nil while child HANDLE is mid-turn."
  (let ((buffer (plist-get handle :buffer)))
    (and (buffer-live-p buffer) (buffer-local-value 'pai--active buffer))))

(defun pai-isub-session-interrupt (handle)
  "Interrupt the turn running in child HANDLE."
  (let ((buffer (plist-get handle :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (pai-interrupt)))))

(defun pai-isub-session-metrics (handle)
  "Return (:tokens N :tps N) for child HANDLE."
  (let ((buffer (plist-get handle :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((tokens (+ pai-isub--usage-tokens
                          (max 0 (round (/ pai-isub--stream-chars 4.0)))))
               (elapsed (max 0.001 (- (float-time) (or pai-isub--started (float-time))))))
          (list :tokens tokens :tps (round (/ tokens elapsed))))))))

(defun pai-isub-session-transcript (handle &optional max-lines)
  "Return the tail of child HANDLE's transcript, at most MAX-LINES lines."
  (let ((buffer (plist-get handle :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((text (buffer-substring-no-properties
                     (point-min)
                     (if (and pai--input-marker (marker-position pai--input-marker))
                         (marker-position pai--input-marker)
                       (point-max)))))
          (plist-get (pai-tools-truncate text (or max-lines 120) nil 'tail) :text))))))

(pai-isub-register-backend
 (list :name "pai"
       :label "pai session buffer"
       :start #'pai-isub-session-start
       :send #'pai-isub-session-send
       :busy-p #'pai-isub-session-busy-p
       :interrupt #'pai-isub-session-interrupt
       :metrics #'pai-isub-session-metrics
       :transcript #'pai-isub-session-transcript))

(provide 'pai-isub-session)
;;; pai-isub-session.el ends here
