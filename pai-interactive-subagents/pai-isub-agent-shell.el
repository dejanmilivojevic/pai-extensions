;;; pai-isub-agent-shell.el --- agent-shell subagent backend -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs a subagent in an `agent-shell' buffer: an external ACP agent (Claude
;; Code, Gemini CLI, Codex, OpenCode, ...) driven by xenodium's agent-shell
;; package, opened next to the parent like the built-in pai backend.
;;
;; Which agent, and which of its models, is configured per pai model (see
;; `pai-isub-model-backends'), e.g. in settings:
;;
;;   :interactive-subagents
;;     (:model-backends (:anthropic/claude-opus-4 (:backend "agent-shell"
;;                                                :agent "claude-code"
;;                                                :agent-model "opus")))
;;
;; so a role resolving to that model runs in Claude Code.  A role forced into
;; this backend without such a mapping uses the `:agent-shell-agent' setting,
;; then `agent-shell-preferred-agent-config'.
;;
;; It does what the pai backend does, in agent-shell's terms:
;;
;;   * role prompt and protocol: the external agent owns its system prompt,
;;     so they (and, with context "fork", the parent conversation) open the
;;     first message instead;
;;   * `reply_to_parent': offered through a per-child stdio MCP server (see
;;     `pai-isub-mcp'); its calls are approved automatically;
;;   * `/parent TEXT' typed in the shell (or M-x `pai-isub-agent-shell-parent')
;;     messages the parent instead of the agent;
;;   * the turn's final message is reported back, turn by turn;
;;   * the role's thinking level selects the agent's nearest thought level;
;;   * a role's tool allowlist rejects permission requests for other kinds of
;;     tool; with `:agent-shell-auto-approve' the allowed ones are approved,
;;     like a pai child that never asks (otherwise you answer in the buffer
;;     and the parent's status shows it waiting);
;;   * text you half-typed at the prompt survives a message from the parent,
;;     and keeps an idle subagent open;
;;   * parent messages that arrive mid-turn are queued, not lost.
;;
;; agent-shell is a soft dependency: it is only loaded when a subagent is
;; started in it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'pai)
(require 'pai-tools)
(require 'pai-isub-backend)
(require 'pai-isub-roles)
(require 'pai-isub-mcp)

(defvar agent-shell--state)
(defvar agent-shell-cwd-function)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell-mcp-servers)
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--resolved-agent-configs "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell--config-option-by-category "agent-shell-config")
(declare-function agent-shell--config-option-set-thought-level-id "agent-shell")
(declare-function agent-shell--send-permission-response "agent-shell")

(defconst pai-isub-agent-shell-name "agent-shell"
  "Backend name of the agent-shell subagent backend.")

(defcustom pai-isub-agent-shell-context-chars 40000
  "Maximum characters of forked parent conversation sent to an agent-shell child."
  :type 'integer
  :group 'pai-isub)

(defconst pai-isub-agent-shell--protocol
  "You are running as a subagent session spawned by a parent agent.
- The parent delegated a task to you; your final message of a turn the parent
  asked for is reported back to it automatically, so end such turns with the
  report you want the parent to read.
- A human user may also type directly into this session at any time. Treat
  those turns as coming from the user, not from the parent.
- Use the `reply_to_parent' tool of the MCP server \"pai-parent\" (often
  named mcp__pai-parent__reply_to_parent) whenever you want
  to tell the parent something in the middle of your work: a question, a
  decision you need, a partial finding, or a warning. Keep those messages
  short and specific.
- Stay inside the task you were given; report instead of expanding scope.
- Do not spawn subagents of your own."
  "Protocol text prepended to the first message of an agent-shell subagent.")

(defconst pai-isub-agent-shell--tool-kinds
  '(("read" "read") ("ls" "read") ("find" "search" "read") ("grep" "search")
    ("edit" "edit" "move" "delete") ("write" "edit") ("bash" "execute")
    ("elisp_eval" "execute") ("browser" "fetch") ("browser_fetch" "fetch")
    ("web_fetch" "fetch") ("web_search" "fetch" "search"))
  "pai tool name -> ACP tool kinds it stands for, for role tool allowlists.")

(defconst pai-isub-agent-shell--free-kinds '("think" "switch_mode")
  "ACP tool kinds a tool allowlist never blocks.")

;;;; Agent configs

(defun pai-isub-agent-shell-available-p ()
  "Return non-nil when the agent-shell package can be loaded."
  (and (require 'agent-shell nil t) (fboundp 'agent-shell--start) t))

(defun pai-isub-agent-shell--require ()
  "Load agent-shell or signal a user-facing error."
  (unless (pai-isub-agent-shell-available-p)
    (error "The agent-shell backend needs the agent-shell package (M-x package-install RET agent-shell)")))

(defun pai-isub-agent-shell--id-name (id)
  "Return agent identifier ID (symbol or string) as a string, or nil."
  (cond ((null id) nil)
        ((symbolp id) (symbol-name id))
        ((stringp id) (unless (string-empty-p id) id))))

(defun pai-isub-agent-shell-configs ()
  "Return agent-shell's known agent configs (alists), or nil when unavailable."
  (when (pai-isub-agent-shell-available-p)
    (seq-filter (lambda (c) (and (listp c) (map-elt c :identifier)))
                (ignore-errors (agent-shell--resolved-agent-configs)))))

(defun pai-isub-agent-shell-agent-names ()
  "Return the identifiers of agent-shell's known agents, as strings."
  (sort (delq nil (mapcar (lambda (c) (pai-isub-agent-shell--id-name
                                       (map-elt c :identifier)))
                          (pai-isub-agent-shell-configs)))
        #'string-lessp))

(defun pai-isub-agent-shell-config (agent)
  "Return the agent-shell config alist whose identifier is AGENT, or nil."
  (let ((name (pai-isub-agent-shell--id-name agent)))
    (seq-find (lambda (c) (equal (pai-isub-agent-shell--id-name (map-elt c :identifier)) name))
              (pai-isub-agent-shell-configs))))

(defun pai-isub-agent-shell--parent-config (parent key)
  "Return setting KEY of this extension, read in PARENT."
  (and (buffer-live-p parent)
       (with-current-buffer parent (plist-get (pai-isub-config) key))))

(defun pai-isub-agent-shell--default-agent (parent)
  "Return the agent to use when no per-model one is configured, or nil.
Settings are read in PARENT."
  (or (pai-isub-agent-shell--id-name
       (pai-isub-agent-shell--parent-config parent :agent-shell-agent))
      (let ((pref (bound-and-true-p agent-shell-preferred-agent-config)))
        (cond ((symbolp pref) (pai-isub-agent-shell--id-name pref))
              ((and (consp pref) (symbolp (cdr pref))) (symbol-name (cdr pref)))
              ((consp pref) (pai-isub-agent-shell--id-name (map-elt pref :identifier)))))))

(defun pai-isub-agent-shell--make-config (agent agent-model &optional mcp-server)
  "Return a copy of AGENT's config, selecting AGENT-MODEL when non-nil.
MCP-SERVER, an ACP server alist, is added to the servers the agent gets."
  (let ((config (or (pai-isub-agent-shell-config agent)
                    (error "Unknown agent-shell agent %s; known: %s" agent
                           (string-join (pai-isub-agent-shell-agent-names) ", ")))))
    (setq config (copy-alist config))
    (when (and agent-model (not (string-empty-p agent-model)))
      (setf (alist-get :default-model-id config) (lambda () agent-model)))
    (when mcp-server
      (setf (alist-get :mcp-servers config)
            (append (or (map-elt config :mcp-servers)
                        (bound-and-true-p agent-shell-mcp-servers))
                    (list mcp-server))))
    config))

;;;; First-message preamble

(defun pai-isub-agent-shell--context-text (messages)
  "Return MESSAGES as a plain transcript, capped to the newest part."
  (let* ((text (mapconcat
                (lambda (m)
                  (let ((body (string-trim (or (ignore-errors
                                                 (pai-content-text (pai-message-content m)))
                                               ""))))
                    (unless (string-empty-p body)
                      (format "[%s]\n%s" (plist-get m :role) body))))
                messages "\n\n"))
         (max pai-isub-agent-shell-context-chars))
    (if (> (length text) max)
        (concat "[... earlier conversation omitted ...]\n"
                (substring text (- (length text) max)))
      text)))

(defun pai-isub-agent-shell--preamble (spec)
  "Return the text prepended to the first message for SPEC."
  (let ((context (and (plist-get spec :context-messages)
                      (pai-isub-agent-shell--context-text
                       (plist-get spec :context-messages))))
        (tools (plist-get spec :tools)))
    (concat
     (format "<subagent_session role=\"%s\" id=\"%s\">\n%s\n\n%s%s\n</subagent_session>\n\n"
             (or (plist-get spec :role) "subagent") (or (plist-get spec :id) "?")
             (string-trim (or (plist-get spec :role-prompt) ""))
             pai-isub-agent-shell--protocol
             (if tools
                 (format "\n- Only use these kinds of tools: %s (plus reply_to_parent); other tool calls will be rejected."
                         (string-join tools ", "))
               ""))
     (if (and context (not (string-empty-p context)))
         (format "<parent_conversation>\n%s\n</parent_conversation>\n\n" context)
       ""))))

;;;; Child buffer marker, /parent

(defvar-local pai-isub-agent-shell--handle nil
  "The subagent handle of this agent-shell buffer, or nil.")
(put 'pai-isub-agent-shell--handle 'permanent-local t)

(define-minor-mode pai-isub-agent-shell-child-mode
  "Marks an agent-shell buffer that runs a pai subagent.
Type `/parent TEXT' at its prompt (or \\[pai-isub-agent-shell-parent]) to
message the parent agent."
  :lighter (:eval (and pai-isub-agent-shell--handle
                       (format " sub:%s" (plist-get pai-isub-agent-shell--handle :id))))
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-S-p") #'pai-isub-agent-shell-parent)
            map))

(defun pai-isub-agent-shell--parent-info (handle)
  "Return a one-line description of HANDLE's subagent and parent."
  (let ((parent (plist-get handle :parent)))
    (format "Subagent %s (%s) in %s; parent: %s"
            (plist-get handle :id) (plist-get handle :role) (plist-get handle :model-label)
            (if (buffer-live-p parent) (buffer-name parent) "(gone)"))))

(defun pai-isub-agent-shell-parent (text)
  "Send TEXT from this agent-shell subagent to its parent agent."
  (interactive
   (progn
     (unless pai-isub-agent-shell--handle
       (user-error "This buffer is not a subagent session"))
     (list (read-string "Message to the parent agent: "))))
  (let ((handle pai-isub-agent-shell--handle)
        (text (string-trim (or text ""))))
    (cond
     ((null handle) (user-error "This buffer is not a subagent session"))
     ((string-empty-p text) (message "%s" (pai-isub-agent-shell--parent-info handle)))
     (t (pai-isub-agent-shell--emit handle 'reply :text text)
        (message "Sent to the parent agent: %s" text)))))

(defun pai-isub-agent-shell--input-bounds ()
  "Return (START . END) of the text typed at this shell's prompt, or nil."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (and proc (marker-position (process-mark proc))
               (<= (marker-position (process-mark proc)) (point-max)))
      (cons (marker-position (process-mark proc)) (point-max)))))

(defun pai-isub-agent-shell--input-text ()
  "Return the text typed at this shell's prompt (\"\" when none)."
  (let ((bounds (pai-isub-agent-shell--input-bounds)))
    (if bounds (buffer-substring-no-properties (car bounds) (cdr bounds)) "")))

(defun pai-isub-agent-shell--submit-advice (orig &rest args)
  "Around `agent-shell-submit': handle `/parent' in subagent buffers.
Calls ORIG with ARGS for everything else."
  (let ((input (and pai-isub-agent-shell--handle
                    (string-trim (pai-isub-agent-shell--input-text)))))
    (if (and input (string-match "\\`/parent\\(?:[ \t\n]+\\(\\(?:.\\|\n\\)*\\)\\)?\\'" input))
        (let ((text (or (match-string 1 input) ""))
              (bounds (pai-isub-agent-shell--input-bounds)))
          (when bounds
            (let ((inhibit-read-only t)) (delete-region (car bounds) (cdr bounds))))
          (pai-isub-agent-shell-parent text))
      (apply orig args))))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell-submit :around #'pai-isub-agent-shell--submit-advice))

;;;; Event handling
;;
;; HANDLE is a mutable plist: (:buffer :backend :id :role :parent :agent
;; :agent-model :model-label :emit :token :thinking :tools :auto-approve
;; :ready :busy :queue :preamble :stash :text :prev-text :after-tool :chars
;; :started :exited).

(defun pai-isub-agent-shell--emit (handle type &rest props)
  "Report TYPE with PROPS for HANDLE to the parent."
  (let ((emit (plist-get handle :emit)))
    (when (functionp emit) (apply emit type props))))

(defun pai-isub-agent-shell--final-text (handle)
  "Return the final message text of HANDLE's current turn."
  (let ((text (string-trim (or (plist-get handle :text) ""))))
    (if (string-empty-p text)
        (string-trim (or (plist-get handle :prev-text) ""))
      text)))

(defun pai-isub-agent-shell--on-chunk (handle chunk)
  "Accumulate streamed CHUNK into HANDLE's final-message text."
  (when (stringp chunk)
    ;; text after a tool call starts a new message; keep only the last one
    (when (plist-get handle :after-tool)
      (let ((text (string-trim (or (plist-get handle :text) ""))))
        (unless (string-empty-p text) (plist-put handle :prev-text text)))
      (plist-put handle :text "")
      (plist-put handle :after-tool nil))
    (plist-put handle :text (concat (plist-get handle :text) chunk))
    (plist-put handle :chars (+ (or (plist-get handle :chars) 0) (length chunk)))))

(defun pai-isub-agent-shell--set-ready (handle)
  "Mark HANDLE's session ready and send what waits for it."
  (unless (plist-get handle :ready)
    (plist-put handle :ready t)
    (pai-isub-agent-shell--flush handle)))

(defun pai-isub-agent-shell--thought-level (option level)
  "Return the value of thought-level OPTION closest to pai thinking LEVEL."
  (let* ((values (map-elt option :options))
         (name (symbol-name level))
         (exact (seq-find (lambda (v)
                            (or (equal (downcase (format "%s" (map-elt v :value))) name)
                                (equal (downcase (format "%s" (or (map-elt v :name) ""))) name)))
                          values)))
    (cond
     (exact (map-elt exact :value))
     (values
      (let* ((levels pai-isub-thinking-levels)
             (pos (/ (float (or (cl-position level levels) 0)) (max 1 (1- (length levels))))))
        (map-elt (nth (round (* pos (1- (length values)))) values) :value))))))

(defun pai-isub-agent-shell--apply-thinking (handle)
  "Select the agent's thought level for HANDLE's thinking, then become ready."
  (let* ((level (plist-get handle :thinking))
         (option (and level
                      (ignore-errors
                        (agent-shell--config-option-by-category
                         (and (boundp 'agent-shell--state) agent-shell--state)
                         "thought_level"))))
         (value (and option (pai-isub-agent-shell--thought-level option level))))
    (if (or (null value) (equal value (map-elt option :current-value)))
        (pai-isub-agent-shell--set-ready handle)
      (condition-case nil
          (agent-shell--config-option-set-thought-level-id
           :thought-level-id value
           :on-success (lambda () (pai-isub-agent-shell--set-ready handle))
           :on-failure (lambda (&rest _) (pai-isub-agent-shell--set-ready handle)))
        (error (pai-isub-agent-shell--set-ready handle))))))

(defun pai-isub-agent-shell--tool-allowed-p (handle kind)
  "Return non-nil when HANDLE's role tool allowlist admits ACP tool KIND."
  (let ((tools (plist-get handle :tools)))
    (or (null tools)
        (member kind pai-isub-agent-shell--free-kinds)
        (seq-some (lambda (tool)
                    (member kind (cdr (assoc tool pai-isub-agent-shell--tool-kinds))))
                  tools))))

(defun pai-isub-agent-shell--own-tool-p (tool-call)
  "Return non-nil when TOOL-CALL is this extension's `reply_to_parent'."
  (string-match-p "reply_to_parent" (format "%s %s" (or (map-elt tool-call :title) "")
                                            (or (map-elt tool-call :raw-input) ""))))

(defun pai-isub-agent-shell--respond (data kinds)
  "Answer permission request DATA with its first option of one of KINDS.
Return non-nil when an answer was sent."
  (let* ((state agent-shell--state)
         (tool-call-id (map-elt data :tool-call-id))
         (actions (map-nested-elt state (list :tool-calls tool-call-id :permission-actions)))
         (choice (seq-some (lambda (kind)
                             (seq-find (lambda (a) (equal (map-elt a :kind) kind)) actions))
                           kinds)))
    (when choice
      (agent-shell--send-permission-response
       :client (map-elt state :client)
       :request-id (map-elt data :request-id)
       :option-id (map-elt choice :option-id)
       :state state
       :tool-call-id tool-call-id)
      t)))

(defun pai-isub-agent-shell--on-permission (handle data)
  "Apply HANDLE's tool policy to the permission request DATA."
  (let* ((tool-call (map-elt data :tool-call))
         (kind (or (map-elt tool-call :kind) "other"))
         (title (or (map-elt tool-call :title) kind)))
    (cond
     ((pai-isub-agent-shell--own-tool-p tool-call)
      (pai-isub-agent-shell--respond data '("allow_always" "allow_once")))
     ((not (pai-isub-agent-shell--tool-allowed-p handle kind))
      (when (pai-isub-agent-shell--respond data '("reject_once" "reject_always"))
        (pai-isub-agent-shell--emit
         handle 'status :text (format "rejected %s (%s): not in the role's tools" title kind))))
     ((plist-get handle :auto-approve)
      (pai-isub-agent-shell--respond data '("allow_once" "allow_always")))
     (t
      (message "[subagent %s] waiting for your permission: %s" (plist-get handle :id) title)
      (pai-isub-agent-shell--emit
       handle 'status :text (format "waiting for permission: %s" title))))))

(defun pai-isub-agent-shell--on-event (handle event)
  "Handle agent-shell EVENT (an alist) for HANDLE."
  (let ((data (map-elt event :data)))
    (pcase (map-elt event :event)
      ('init-finished
       (pai-isub-agent-shell--apply-thinking handle))
      ('input-submitted
       (plist-put handle :busy t)
       (plist-put handle :text "")
       (plist-put handle :prev-text nil)
       (plist-put handle :after-tool nil)
       (pai-isub-agent-shell--emit handle 'busy))
      ('agent-message-chunk
       (pai-isub-agent-shell--on-chunk handle (map-elt data :text-chunk)))
      ('tool-call-update
       (plist-put handle :after-tool t))
      ('permission-request
       (pai-isub-agent-shell--on-permission handle data))
      ('turn-complete
       (plist-put handle :busy nil)
       (let* ((reason (map-elt data :stop-reason))
              (text (pai-isub-agent-shell--final-text handle)))
         (pai-isub-agent-shell--emit
          handle 'turn-end
          :text (if (or (null reason) (member reason '("end_turn" "")))
                    text
                  (format "%s\n\n(stop reason: %s)" text reason))))
       (pai-isub-agent-shell--after-turn handle))
      ('error
       (plist-put handle :busy nil)
       (pai-isub-agent-shell--emit
        handle 'error
        :text (format "agent-shell error%s: %s"
                      (if (map-elt data :code) (format " %s" (map-elt data :code)) "")
                      (or (map-elt data :message) "unknown")))
       (pai-isub-agent-shell--after-turn handle))
      ('clean-up
       (pai-isub-agent-shell--exit handle)))))

(defun pai-isub-agent-shell--exit (handle)
  "Report HANDLE's session as gone, once."
  (unless (plist-get handle :exited)
    (plist-put handle :exited t)
    (plist-put handle :queue nil)
    (pai-isub-mcp-unregister (plist-get handle :token))
    (pai-isub-agent-shell--emit handle 'exit :reason 'killed)))

(defun pai-isub-agent-shell--reply-tool (handle)
  "Return the MCP tool handler for HANDLE's child."
  (lambda (name args)
    (let ((text (string-trim (or (plist-get args :message) ""))))
      (cond
       ((not (equal name "reply_to_parent")) (cons (format "Unknown tool %s" name) t))
       ((string-empty-p text) (cons "reply_to_parent requires a message" t))
       ((plist-get handle :exited) (cons "This session has no parent agent to reply to" t))
       (t
        ;; deliver outside the network filter
        (run-at-time 0 nil #'pai-isub-agent-shell--emit handle 'reply :text text)
        (cons "Delivered to the parent agent." nil))))))

;;;; Sending

(defun pai-isub-agent-shell--shell-busy-p (buffer)
  "Return non-nil when agent-shell BUFFER is not ready for input."
  (and (buffer-live-p buffer)
       (not (eq (ignore-errors (agent-shell-status :shell-buffer buffer)) 'ready))))

(defun pai-isub-agent-shell--submit (handle text)
  "Submit TEXT in HANDLE's shell now, stashing what the user had typed."
  (let ((buffer (plist-get handle :buffer)))
    (plist-put handle :busy t)
    (with-current-buffer buffer
      (let ((bounds (pai-isub-agent-shell--input-bounds)))
        (when (and bounds (< (car bounds) (cdr bounds)))
          (let ((typed (buffer-substring (car bounds) (cdr bounds))))
            (unless (string-empty-p (string-trim typed))
              (plist-put handle :stash typed)))
          (let ((inhibit-read-only t)) (delete-region (car bounds) (cdr bounds)))))
      (agent-shell-insert :text text :submit t :no-focus t :shell-buffer buffer))))

(defun pai-isub-agent-shell--restore-stash (handle)
  "Put back what the user had typed before HANDLE's last parent message."
  (let ((stash (plist-get handle :stash))
        (buffer (plist-get handle :buffer)))
    (when (and stash (buffer-live-p buffer)
               (not (pai-isub-agent-shell--shell-busy-p buffer)))
      (plist-put handle :stash nil)
      (with-current-buffer buffer
        (when (string-empty-p (string-trim (pai-isub-agent-shell--input-text)))
          (save-excursion (goto-char (point-max)) (insert stash)))))))

(defun pai-isub-agent-shell--after-turn (handle)
  "After a turn: send the next queued message, or give back stashed input."
  (run-at-time 0 nil
               (lambda ()
                 (if (plist-get handle :queue)
                     (pai-isub-agent-shell--flush handle)
                   (pai-isub-agent-shell--restore-stash handle)))))

(defun pai-isub-agent-shell--flush (handle)
  "Submit HANDLE's next queued message once its shell can take it.
Runs from a timer so agent-shell finishes its own event handling first."
  (when (plist-get handle :queue)
    (run-at-time
     0 nil
     (lambda ()
       (let ((buffer (plist-get handle :buffer)))
         (when (and (plist-get handle :queue)
                    (plist-get handle :ready)
                    (not (plist-get handle :busy))
                    (buffer-live-p buffer)
                    (not (pai-isub-agent-shell--shell-busy-p buffer)))
           (let ((text (car (plist-get handle :queue))))
             (plist-put handle :queue (cdr (plist-get handle :queue)))
             (condition-case err
                 (pai-isub-agent-shell--submit handle text)
               (error
                (plist-put handle :busy nil)
                (pai-isub-agent-shell--emit
                 handle 'error :text (error-message-string err)))))))))))

(defun pai-isub-agent-shell-send (handle text)
  "Deliver TEXT to HANDLE's agent, queueing it while the agent is busy."
  (let ((buffer (plist-get handle :buffer)))
    (unless (buffer-live-p buffer)
      (error "Subagent agent-shell buffer is gone"))
    (when (string-empty-p (string-trim (or text "")))
      (error "Refusing to send an empty message to a subagent"))
    (let ((preamble (plist-get handle :preamble)))
      (when preamble
        (plist-put handle :preamble nil)
        (setq text (concat preamble text))))
    (plist-put handle :queue (append (plist-get handle :queue) (list text)))
    (pai-isub-agent-shell--emit handle 'busy)
    (pai-isub-agent-shell--flush handle)
    t))

;;;; Backend implementation

(defun pai-isub-agent-shell-start (spec)
  "Start an agent-shell session for SPEC; return its handle."
  (pai-isub-agent-shell--require)
  (let* ((parent (plist-get spec :parent))
         (options (plist-get spec :backend-options))
         (agent (or (pai-isub-agent-shell--id-name (plist-get options :agent))
                    (pai-isub-agent-shell--default-agent parent)
                    (error "No agent-shell agent configured; set one with /subagents-model-backend")))
         (agent-model (plist-get options :agent-model))
         (cwd (file-name-as-directory
               (expand-file-name (or (plist-get spec :cwd) default-directory))))
         (handle (list :buffer nil :backend pai-isub-agent-shell-name
                       :id (plist-get spec :id) :role (plist-get spec :role)
                       :parent parent :agent agent :agent-model agent-model
                       :model-label (if (and agent-model (not (string-empty-p agent-model)))
                                        (format "%s/%s" agent agent-model)
                                      agent)
                       :emit (plist-get spec :emit)
                       :thinking (plist-get spec :thinking)
                       :tools (plist-get spec :tools)
                       :auto-approve (pai-truthy
                                      (pai-isub-agent-shell--parent-config
                                       parent :agent-shell-auto-approve))
                       :ready nil :busy nil :queue nil :stash nil
                       :preamble (pai-isub-agent-shell--preamble spec)
                       :text "" :prev-text nil :after-tool nil :chars 0
                       :started (float-time) :exited nil))
         (token (pai-isub-mcp-register (pai-isub-agent-shell--reply-tool handle)))
         (config (condition-case err
                     (pai-isub-agent-shell--make-config
                      agent agent-model (pai-isub-mcp-server-config token))
                   (error (pai-isub-mcp-unregister token) (signal (car err) (cdr err)))))
         (buffer (condition-case err
                     (let ((default-directory cwd)
                           (agent-shell-cwd-function (lambda () cwd)))
                       (agent-shell--start :config config :no-focus t
                                           :new-session t :session-strategy 'new))
                   (error (pai-isub-mcp-unregister token) (signal (car err) (cdr err))))))
    (plist-put handle :token token)
    (unless (buffer-live-p buffer)
      (pai-isub-mcp-unregister token)
      (error "agent-shell did not create a session buffer"))
    (plist-put handle :buffer buffer)
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :on-event (lambda (event) (pai-isub-agent-shell--on-event handle event)))
    (with-current-buffer buffer
      (setq pai-isub-agent-shell--handle handle)
      (pai-isub-agent-shell-child-mode 1)
      ;; a fallback for `clean-up', which a failed start may never emit
      (add-hook 'kill-buffer-hook
                (lambda () (pai-isub-agent-shell--exit handle))
                nil t))
    (pai-isub-display buffer parent)
    handle))

(defun pai-isub-agent-shell-busy-p (handle)
  "Return non-nil while HANDLE's agent is mid-turn or has queued input."
  (or (plist-get handle :busy)
      (and (plist-get handle :queue) t)))

(defun pai-isub-agent-shell-pending-input-p (handle)
  "Return non-nil when text typed in HANDLE's shell was not sent yet."
  (let ((buffer (plist-get handle :buffer)))
    (or (plist-get handle :stash)
        (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (not (string-empty-p (string-trim (pai-isub-agent-shell--input-text)))))))))

(defun pai-isub-agent-shell-interrupt (handle)
  "Cancel HANDLE's current turn and drop its queued messages."
  (plist-put handle :queue nil)
  (let ((buffer (plist-get handle :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (agent-shell-interrupt t)))))

(defun pai-isub-agent-shell-close (handle)
  "Kill HANDLE's agent-shell buffer (and with it the agent process)."
  (plist-put handle :queue nil)
  (let ((buffer (plist-get handle :buffer)))
    (if (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))
      (pai-isub-agent-shell--exit handle))))

(defun pai-isub-agent-shell-metrics (handle)
  "Return (:tokens N :tps N) for HANDLE."
  (let* ((buffer (plist-get handle :buffer))
         (state (and (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (and (boundp 'agent-shell--state) agent-shell--state))))
         (usage (and state (map-elt state :usage)))
         (tokens (or (and usage (map-elt usage :output-tokens))
                     (round (/ (or (plist-get handle :chars) 0) 4.0))))
         (elapsed (max 0.001 (- (float-time) (or (plist-get handle :started) (float-time))))))
    (list :tokens tokens :tps (round (/ tokens elapsed)))))

(defun pai-isub-agent-shell-transcript (handle &optional max-lines)
  "Return the tail of HANDLE's shell buffer, at most MAX-LINES lines."
  (let ((buffer (plist-get handle :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (plist-get (pai-tools-truncate
                    (buffer-substring-no-properties (point-min) (point-max))
                    (or max-lines 120) nil 'tail)
                   :text)))))

(pai-isub-register-backend
 (list :name pai-isub-agent-shell-name
       :label "agent-shell (external ACP agent)"
       :start #'pai-isub-agent-shell-start
       :send #'pai-isub-agent-shell-send
       :busy-p #'pai-isub-agent-shell-busy-p
       :pending-input-p #'pai-isub-agent-shell-pending-input-p
       :interrupt #'pai-isub-agent-shell-interrupt
       :close #'pai-isub-agent-shell-close
       :metrics #'pai-isub-agent-shell-metrics
       :transcript #'pai-isub-agent-shell-transcript))

(provide 'pai-isub-agent-shell)
;;; pai-isub-agent-shell.el ends here
