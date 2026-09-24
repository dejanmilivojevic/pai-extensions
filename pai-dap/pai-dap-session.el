;;; pai-dap-session.el --- DAP debug session manager for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Debug session manager ported from oh-my-pi's `dap/session.ts'.  Owns the
;; live DAP session(s), drives the initialize -> initialized -> configurationDone
;; -> launch/attach handshake, tracks breakpoints/output/stop state, and turns
;; continue/step into a stop outcome by pumping for the next stopped event.
;;
;; Adapted to Emacs's synchronous tool model: instead of promises/AbortSignal,
;; requests block by pumping `accept-process-output' until their response (or
;; the next stop event) arrives or a per-request timeout elapses.  Timeouts here
;; are in SECONDS (the tool clamps and converts).
;;
;; The manager is a process-global singleton (one debug session at a time,
;; matching oh-my-pi), with optional child sessions for tcp adapters that use
;; the `startDebugging' reverse request (js-debug).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-dap-client)

(defconst pai-dap--idle-timeout 600.0 "Seconds before an idle session is reaped.")
(defconst pai-dap--cleanup-interval 30 "Seconds between idle-session sweeps.")
(defconst pai-dap--max-output-bytes (* 128 1024) "Cap on captured debuggee output.")
(defconst pai-dap--stop-capture-timeout 5.0 "Seconds to wait for an initial stop.")

;;;; Session model

(cl-defstruct (pai-dap-session (:constructor pai-dap-session--make))
  id adapter cwd program client
  (status 'launching)                 ; launching|configuring|stopped|running|terminated
  (launched-at (float-time))
  (last-used-at (float-time))
  (breakpoints (make-hash-table :test 'equal)) ; sourcePath -> list of record plists
  (function-breakpoints nil)
  (instruction-breakpoints nil)
  (data-breakpoints nil)
  (output "")                          ; captured debuggee output (byte-capped)
  (output-bytes 0)
  (output-truncated nil)
  (stop nil)                           ; plist: :threadId :frameId :reason ... :source :line :column
  (threads nil)
  (last-stack-frames nil)
  (exit-code nil)
  (capabilities nil)
  (initialized-seen nil)
  (needs-configuration-done nil)
  (configuration-done-sent nil)
  (parent-id nil)
  (child-ids nil)
  (port nil))

;;;; Manager state (singleton)

(defvar pai-dap--sessions (make-hash-table :test 'equal) "id -> session.")
(defvar pai-dap--active-id nil "Id of the active session.")
(defvar pai-dap--next-id 0 "Session id counter.")
(defvar pai-dap--cleanup-timer nil "Idle-session cleanup timer.")

(defun pai-dap--now () (float-time))

;;;; Output capture

(defun pai-dap--capture-output (session text)
  "Append TEXT to SESSION's output buffer, capping to `pai-dap--max-output-bytes'."
  (when (and text (> (length text) 0))
    (let* ((combined (concat (pai-dap-session-output session) text))
           (bytes (string-bytes combined)))
      (setf (pai-dap-session-output-bytes session)
            (+ (pai-dap-session-output-bytes session) (string-bytes text)))
      (when (> bytes pai-dap--max-output-bytes)
        (let* ((raw (encode-coding-string combined 'utf-8))
               (kept (substring raw (- (length raw) pai-dap--max-output-bytes))))
          (setq combined (decode-coding-string kept 'utf-8))
          (setf (pai-dap-session-output-truncated session) t)))
      (setf (pai-dap-session-output session) combined))))

;;;; Summary

(defun pai-dap--breakpoint-count (session)
  "Total number of source breakpoints recorded on SESSION."
  (let ((total 0))
    (maphash (lambda (_k v) (setq total (+ total (length v))))
             (pai-dap-session-breakpoints session))
    total))

(defun pai-dap--summary (session)
  "Build a summary plist (mirrors DapSessionSummary) for SESSION."
  (let ((stop (pai-dap-session-stop session)))
    (append
     (list :id (pai-dap-session-id session)
           :adapter (plist-get (pai-dap-session-adapter session) :name)
           :cwd (pai-dap-session-cwd session)
           :status (pai-dap-session-status session)
           :breakpointFiles (hash-table-count (pai-dap-session-breakpoints session))
           :breakpointCount (pai-dap--breakpoint-count session)
           :functionBreakpointCount (length (pai-dap-session-function-breakpoints session))
           :outputBytes (pai-dap-session-output-bytes session)
           :outputTruncated (pai-dap-session-output-truncated session)
           :needsConfigurationDone (and (pai-dap-session-needs-configuration-done session)
                                        (not (pai-dap-session-configuration-done-sent session))))
     (when (pai-dap-session-program session) (list :program (pai-dap-session-program session)))
     (when (plist-member stop :threadId) (list :threadId (plist-get stop :threadId)))
     (when (plist-member stop :frameId) (list :frameId (plist-get stop :frameId)))
     (when (plist-get stop :reason) (list :stopReason (plist-get stop :reason)))
     (let ((desc (or (plist-get stop :description) (plist-get stop :text))))
       (when desc (list :stopDescription desc)))
     (when (plist-get stop :frameName) (list :frameName (plist-get stop :frameName)))
     (when (plist-get stop :instructionPointerReference)
       (list :instructionPointerReference (plist-get stop :instructionPointerReference)))
     (when (plist-get stop :source) (list :source (plist-get stop :source)))
     (when (plist-member stop :line) (list :line (plist-get stop :line)))
     (when (plist-member stop :column) (list :column (plist-get stop :column)))
     (when (pai-dap-session-exit-code session) (list :exitCode (pai-dap-session-exit-code session)))
     (when (pai-dap-session-parent-id session) (list :parentSessionId (pai-dap-session-parent-id session)))
     (when (pai-dap-session-child-ids session) (list :childSessionIds (pai-dap-session-child-ids session))))))

;;;; Session lookup / tree

(defun pai-dap--active-session ()
  "Return the active session or nil."
  (when pai-dap--active-id
    (let ((s (gethash pai-dap--active-id pai-dap--sessions)))
      (unless s (setq pai-dap--active-id nil))
      s)))

(defun pai-dap--active-or-error ()
  "Return the active session, or signal an error."
  (or (pai-dap--active-session)
      (error "No active debug session. Launch or attach first.")))

(defun pai-dap--touch (session)
  "Mark SESSION and its ancestors as recently used."
  (let ((cur session))
    (while cur
      (setf (pai-dap-session-last-used-at cur) (pai-dap--now))
      (setq cur (and (pai-dap-session-parent-id cur)
                     (gethash (pai-dap-session-parent-id cur) pai-dap--sessions))))))

(defun pai-dap--touch-active ()
  "Return the active session, refreshing use time and terminated status."
  (let ((session (pai-dap--active-or-error)))
    (pai-dap--touch session)
    (when (and (not (eq (pai-dap-session-status session) 'terminated))
               (not (pai-dap-client-live-p (pai-dap-session-client session))))
      (setf (pai-dap-session-status session) 'terminated))
    session))

(defun pai-dap--root (session)
  "Return the root of SESSION's tree."
  (let ((root session))
    (while (pai-dap-session-parent-id root)
      (let ((parent (gethash (pai-dap-session-parent-id root) pai-dap--sessions)))
        (if parent (setq root parent) (setf (pai-dap-session-parent-id root) nil))))
    root))

(defun pai-dap--tree-sessions (session)
  "Return all sessions in SESSION's tree (root plus descendants)."
  (let ((acc nil) (pending (list (pai-dap--root session))))
    (while pending
      (let ((cur (pop pending)))
        (push cur acc)
        (dolist (cid (pai-dap-session-child-ids cur))
          (let ((child (gethash cid pai-dap--sessions)))
            (when child (push child pending))))))
    (nreverse acc)))

(defun pai-dap--live-tree-sessions (session)
  "Return live (non-terminated, connected) sessions in SESSION's tree, or SESSION."
  (let ((live (cl-remove-if-not
               (lambda (s) (and (not (eq (pai-dap-session-status s) 'terminated))
                                (pai-dap-client-live-p (pai-dap-session-client s))))
               (pai-dap--tree-sessions session))))
    (or live (list session))))

(defun pai-dap--has-live-stopped-active ()
  "Return non-nil when the active session is live and stopped."
  (let ((a (pai-dap--active-session)))
    (and a (eq (pai-dap-session-status a) 'stopped)
         (pai-dap-client-live-p (pai-dap-session-client a)))))

(defun pai-dap--reactivate-after-termination (session)
  "Point the active session at a live tree member when SESSION terminates."
  (when (equal pai-dap--active-id (pai-dap-session-id session))
    (let ((live (cl-remove-if-not
                 (lambda (s) (and (not (eq (pai-dap-session-status s) 'terminated))
                                  (pai-dap-client-live-p (pai-dap-session-client s))))
                 (pai-dap--tree-sessions session))))
      (when live
        (let ((repl (or (cl-find 'stopped live :key #'pai-dap-session-status)
                        (cl-find-if #'pai-dap-session-parent-id live)
                        (car live))))
          (setq pai-dap--active-id (pai-dap-session-id repl)))))))

;;;; Pumping the tree

(defun pai-dap--pump-tree (session predicate deadline)
  "Pump every live transport in SESSION's tree until PREDICATE or DEADLINE.
Return non-nil if PREDICATE was satisfied."
  (catch 'done
    (while t
      (when (funcall predicate) (throw 'done t))
      (when (>= (float-time) deadline) (throw 'done (funcall predicate)))
      (let ((any nil))
        (dolist (s (pai-dap--tree-sessions session))
          (let ((tp (pai-dap-client-transport (pai-dap-session-client s))))
            (when (and tp (process-live-p tp))
              (setq any t)
              (accept-process-output tp 0.03 nil t))))
        (unless any (throw 'done (funcall predicate)))))))

;;;; Event wiring

(defun pai-dap--handle-stopped (session body)
  "Update SESSION for a stopped-event BODY."
  (setf (pai-dap-session-status session) 'stopped
        (pai-dap-session-stop session)
        (list :threadId (plist-get body :threadId)
              :reason (plist-get body :reason)
              :description (plist-get body :description)
              :text (plist-get body :text))
        (pai-dap-session-last-stack-frames session) nil)
  (setq pai-dap--active-id (pai-dap-session-id session)))

(defun pai-dap--apply-top-frame (session frame)
  "Apply top stack FRAME to SESSION's stop location."
  (when frame
    (let ((stop (pai-dap-session-stop session)))
      (setq stop (plist-put stop :frameId (plist-get frame :id)))
      (setq stop (plist-put stop :frameName (plist-get frame :name)))
      (setq stop (plist-put stop :instructionPointerReference
                            (plist-get frame :instructionPointerReference)))
      (setq stop (plist-put stop :source (plist-get frame :source)))
      (setq stop (plist-put stop :line (plist-get frame :line)))
      (setq stop (plist-put stop :column (plist-get frame :column)))
      (setf (pai-dap-session-stop session) stop))))

(defun pai-dap--wire-events (session)
  "Attach DAP event and reverse-request handlers to SESSION's client."
  (let ((client (pai-dap-session-client session)))
    (pai-dap-client-on-reverse-request
     client "runInTerminal"
     (lambda (args) (pai-dap--run-in-terminal session args)))
    (pai-dap-client-on-reverse-request
     client "startDebugging"
     (lambda (args) (pai-dap--start-child session args) '()))
    (pai-dap-client-on-event
     client "output"
     (lambda (body) (pai-dap--capture-output session (or (plist-get body :output) ""))))
    (pai-dap-client-on-event
     client "initialized"
     (lambda (_body)
       (setf (pai-dap-session-initialized-seen session) t)
       (when (and (not (pai-dap-session-configuration-done-sent session))
                  (eq (pai-dap-session-status session) 'launching))
         (setf (pai-dap-session-status session) 'configuring))))
    (pai-dap-client-on-event
     client "stopped"
     (lambda (body) (pai-dap--handle-stopped session body)))
    (pai-dap-client-on-event
     client "continued"
     (lambda (body)
       (setf (pai-dap-session-status session) 'running
             (pai-dap-session-stop session) (list :threadId (plist-get body :threadId))
             (pai-dap-session-last-stack-frames session) nil)))
    (pai-dap-client-on-event
     client "exited"
     (lambda (body)
       (setf (pai-dap-session-exit-code session) (plist-get body :exitCode)
             (pai-dap-session-status session) 'terminated)
       (pai-dap--reactivate-after-termination session)))
    (pai-dap-client-on-event
     client "terminated"
     (lambda (_body)
       (setf (pai-dap-session-status session) 'terminated)
       (pai-dap--reactivate-after-termination session)))))

(defun pai-dap--run-in-terminal (session args)
  "Handle a runInTerminal reverse request for SESSION with ARGS."
  (let ((argv (plist-get args :args)))
    (when (or (null argv) (= (length argv) 0))
      (error "runInTerminal request did not include a command"))
    (let* ((argv (append argv nil))
           (cwd (expand-file-name (or (plist-get args :cwd) ".") (pai-dap-session-cwd session)))
           (default-directory (file-name-as-directory cwd))
           (extra (cl-loop for (k v) on (plist-get args :env) by #'cddr
                           when (and v (not (eq v :json-false)))
                           collect (format "%s=%s" (substring (symbol-name k) 1) v)))
           (process-environment (append extra (pai-dap-client--non-interactive-env)))
           (proc (make-process
                  :name (format "pai-dap-debuggee-%s" (pai-dap-session-id session))
                  :command argv :coding 'binary :connection-type 'pipe :noquery t
                  :filter (lambda (_p chunk)
                            (pai-dap--capture-output session (decode-coding-string chunk 'utf-8))))))
      (list :processId (process-id proc)))))

;;;; Registration / lifecycle

(defun pai-dap--register-session (client adapter cwd &optional program parent-id)
  "Create, wire, and register a session for CLIENT.  Return the session."
  (let ((session (pai-dap-session--make
                  :id (format "debug-%d" (cl-incf pai-dap--next-id))
                  :adapter adapter :cwd cwd :program program :client client
                  :parent-id parent-id :port (pai-dap-client-port client))))
    (pai-dap--wire-events session)
    (puthash (pai-dap-session-id session) session pai-dap--sessions)
    (when parent-id
      (let ((parent (gethash parent-id pai-dap--sessions)))
        (when parent
          (push (pai-dap-session-id session) (pai-dap-session-child-ids parent)))))
    (unless (pai-dap--has-live-stopped-active)
      (setq pai-dap--active-id (pai-dap-session-id session)))
    (pai-dap--ensure-cleanup-timer)
    session))

(defun pai-dap--dispose-session (session)
  "Remove SESSION (and children) from the manager and dispose its client."
  (when (gethash (pai-dap-session-id session) pai-dap--sessions)
    (dolist (cid (copy-sequence (pai-dap-session-child-ids session)))
      (let ((child (gethash cid pai-dap--sessions)))
        (when child (pai-dap--dispose-session child))))
    (remhash (pai-dap-session-id session) pai-dap--sessions)
    (when (pai-dap-session-parent-id session)
      (let ((parent (gethash (pai-dap-session-parent-id session) pai-dap--sessions)))
        (when parent
          (setf (pai-dap-session-child-ids parent)
                (delete (pai-dap-session-id session) (pai-dap-session-child-ids parent))))))
    (when (equal pai-dap--active-id (pai-dap-session-id session))
      (let ((parent (and (pai-dap-session-parent-id session)
                         (gethash (pai-dap-session-parent-id session) pai-dap--sessions))))
        (setq pai-dap--active-id
              (or (and parent (pai-dap-session-id parent))
                  (car (hash-table-keys pai-dap--sessions))))))
    (pai-dap-client-dispose (pai-dap-session-client session))))

(defun pai-dap--ensure-launch-slot ()
  "Reap dead sessions and error if a root session is still active."
  (dolist (s (hash-table-values pai-dap--sessions))
    (when (or (eq (pai-dap-session-status s) 'terminated)
              (not (pai-dap-client-live-p (pai-dap-session-client s))))
      (pai-dap--dispose-session s)))
  (let ((root (cl-find-if-not #'pai-dap-session-parent-id (hash-table-values pai-dap--sessions))))
    (when root
      (error "Debug session %s is still active. Terminate it before launching another."
             (pai-dap-session-id root)))))

(defun pai-dap--ensure-cleanup-timer ()
  "Start the idle-session cleanup timer if not running."
  (unless pai-dap--cleanup-timer
    (setq pai-dap--cleanup-timer
          (run-with-timer pai-dap--cleanup-interval pai-dap--cleanup-interval
                          #'pai-dap--cleanup-idle))))

(defun pai-dap--cleanup-idle ()
  "Dispose terminated, dead, or idle sessions."
  (let ((now (pai-dap--now)))
    (dolist (s (hash-table-values pai-dap--sessions))
      (when (or (eq (pai-dap-session-status s) 'terminated)
                (> (- now (pai-dap-session-last-used-at s)) pai-dap--idle-timeout)
                (not (pai-dap-client-live-p (pai-dap-session-client s))))
        (pai-dap--dispose-session s))))
  (when (zerop (hash-table-count pai-dap--sessions))
    (when pai-dap--cleanup-timer (cancel-timer pai-dap--cleanup-timer))
    (setq pai-dap--cleanup-timer nil)))

;;;; Initialize / handshake

(defun pai-dap--initialize-args (adapter)
  "Build initialize request arguments for ADAPTER."
  (list :clientID "pai"
        :clientName "pai (Emacs)"
        :adapterID (plist-get adapter :name)
        :locale "en-US"
        :linesStartAt1 t
        :columnsStartAt1 t
        :pathFormat "path"
        :supportsRunInTerminalRequest t
        :supportsStartDebuggingRequest t
        :supportsMemoryReferences t
        :supportsVariableType t
        :supportsInvalidatedEvent t))

(defun pai-dap--complete-handshake (session timeout)
  "Wait for the initialized event (if needed) and send configurationDone for SESSION."
  (when (not (pai-dap-session-configuration-done-sent session))
    (let ((client (pai-dap-session-client session)))
      (if (not (pai-dap-session-needs-configuration-done session))
          (progn
            (when (memq (pai-dap-session-status session) '(launching configuring))
              (setf (pai-dap-session-status session) 'running)))
        (unless (pai-dap-session-initialized-seen session)
          (pai-dap--pump-tree session
                              (lambda () (pai-dap-session-initialized-seen session))
                              (+ (float-time) timeout)))
        (when (pai-dap-session-initialized-seen session)
          (pai-dap-client-request client "configurationDone" nil timeout)
          (setf (pai-dap-session-configuration-done-sent session) t)
          (when (eq (pai-dap-session-status session) 'configuring)
            (setf (pai-dap-session-status session) 'running)))))))

(defun pai-dap--ensure-configuration-done (session timeout)
  "Send configurationDone for SESSION if required and not yet sent."
  (when (and (pai-dap-session-needs-configuration-done session)
             (not (pai-dap-session-configuration-done-sent session)))
    (pai-dap-client-request (pai-dap-session-client session) "configurationDone" nil timeout)
    (setf (pai-dap-session-configuration-done-sent session) t)
    (when (eq (pai-dap-session-status session) 'configuring)
      (setf (pai-dap-session-status session) 'running))))

(defun pai-dap--request (session command &optional args timeout)
  "Ensure configurationDone, then send COMMAND to SESSION's client and return body."
  (pai-dap--ensure-configuration-done session timeout)
  (prog1 (pai-dap-client-request (pai-dap-session-client session) command args timeout)
    (pai-dap--touch session)))

;;;; Launch / attach

(defun pai-dap-launch (options &optional timeout)
  "Launch a debug session.  OPTIONS is a plist:
\(:adapter RESOLVED :program PATH :args (STR...) :cwd DIR :extra PLIST).
Return a summary plist.  TIMEOUT is in seconds."
  (let* ((timeout (or timeout 30.0))
         (adapter (plist-get options :adapter))
         (cwd (plist-get options :cwd))
         (program (plist-get options :program)))
    (pai-dap--ensure-launch-slot)
    (let* ((client (pai-dap-client-spawn adapter cwd))
           (session (pai-dap--register-session client adapter cwd program)))
      (condition-case err
          (progn
            (setf (pai-dap-session-capabilities session)
                  (pai-dap-client-initialize client (pai-dap--initialize-args adapter) timeout))
            (setf (pai-dap-session-needs-configuration-done session)
                  (eq (plist-get (pai-dap-session-capabilities session)
                                 :supportsConfigurationDoneRequest) t))
            (let* ((launch-args (append (list :program program :cwd cwd)
                                        (when (plist-get options :args)
                                          (list :args (plist-get options :args)))
                                        (plist-get options :extra)
                                        (pai-dap-session-adapter-launch-defaults adapter)))
                   (sent (pai-dap-client-send-async client "launch" launch-args))
                   (seq (car sent)) (slot (cdr sent)))
              ;; Pump for the initialized event (or an early launch response).
              (pai-dap--pump-tree session
                                  (lambda () (or (pai-dap-session-initialized-seen session)
                                                 (pai-dap-client-slot-settled-p slot)))
                                  (+ (float-time) timeout))
              (pai-dap--complete-handshake session timeout)
              (pai-dap-client-await client seq slot "launch" timeout)
              (pai-dap--capture-initial-stop session timeout))
            (pai-dap--summary (or (pai-dap--active-session) session)))
        (error (pai-dap--dispose-session session)
               (signal (car err) (pai-dap--map-launch-error adapter err)))))))

(defun pai-dap-session-adapter-launch-defaults (adapter)
  "Return ADAPTER's launch defaults as a plist (already normalized)."
  (copy-sequence (plist-get adapter :launch-defaults)))

(defun pai-dap--map-launch-error (adapter err)
  "Map ERR to a friendlier debugpy hint for ADAPTER when applicable.
Return the cdr for `signal'."
  (let ((msg (error-message-string err)))
    (if (and (equal (plist-get adapter :name) "debugpy")
             (string-match-p "No module named ['\"]?debugpy" msg))
        (list "adapter 'debugpy' is not available: install with 'pip install debugpy'")
      (cdr err))))

(defun pai-dap--capture-initial-stop (session timeout)
  "Best-effort: wait briefly for an initial stop (stopOnEntry) and fetch its frame."
  (ignore-errors
    (pai-dap--pump-tree session
                        (lambda () (memq (pai-dap-session-status (or (pai-dap--active-session) session))
                                         '(stopped terminated)))
                        (+ (float-time) (min timeout pai-dap--stop-capture-timeout)))
    (let ((s (or (pai-dap--active-session) session)))
      (when (eq (pai-dap-session-status s) 'stopped)
        (pai-dap--fetch-top-frame s (min timeout pai-dap--stop-capture-timeout))))))

(defun pai-dap-attach (options &optional timeout)
  "Attach to a debug target.  OPTIONS is a plist:
\(:adapter RESOLVED :cwd DIR :pid N :port N :host STR).  Return a summary plist."
  (let* ((timeout (or timeout 30.0))
         (adapter (plist-get options :adapter))
         (cwd (plist-get options :cwd)))
    (pai-dap--ensure-launch-slot)
    (let* ((client (pai-dap-client-spawn adapter cwd))
           (session (pai-dap--register-session client adapter cwd)))
      (condition-case err
          (progn
            (setf (pai-dap-session-capabilities session)
                  (pai-dap-client-initialize client (pai-dap--initialize-args adapter) timeout))
            (setf (pai-dap-session-needs-configuration-done session)
                  (eq (plist-get (pai-dap-session-capabilities session)
                                 :supportsConfigurationDoneRequest) t))
            (let* ((attach-args (append (copy-sequence (plist-get adapter :attach-defaults))
                                        (list :cwd cwd)
                                        (when (plist-get options :pid)
                                          (list :pid (plist-get options :pid)
                                                :processId (plist-get options :pid)))
                                        (when (plist-get options :port)
                                          (list :port (plist-get options :port)))
                                        (when (plist-get options :host)
                                          (list :host (plist-get options :host)))))
                   (sent (pai-dap-client-send-async client "attach" attach-args))
                   (seq (car sent)) (slot (cdr sent)))
              (pai-dap--pump-tree session
                                  (lambda () (or (pai-dap-session-initialized-seen session)
                                                 (pai-dap-client-slot-settled-p slot)))
                                  (+ (float-time) timeout))
              (pai-dap--complete-handshake session timeout)
              (pai-dap-client-await client seq slot "attach" timeout)
              (pai-dap--capture-initial-stop session timeout))
            (pai-dap--summary (or (pai-dap--active-session) session)))
        (error (pai-dap--dispose-session session)
               (signal (car err) (pai-dap--map-launch-error adapter err)))))))

(defun pai-dap--start-child (parent args)
  "Handle a startDebugging reverse request from PARENT with ARGS (tcp trees)."
  (unless (and (eq (plist-get (pai-dap-session-adapter parent) :connect-mode) 'tcp)
               (pai-dap-session-port parent))
    (error "DAP adapter %s cannot accept child sessions"
           (plist-get (pai-dap-session-adapter parent) :name)))
  (let* ((request (if (equal (plist-get args :request) "attach") "attach" "launch"))
         (config (or (plist-get args :configuration) '()))
         (cwd (expand-file-name (or (plist-get config :cwd) ".") (pai-dap-session-cwd parent)))
         (client (pai-dap-client-connect (pai-dap-session-adapter parent) cwd "127.0.0.1"
                                         (pai-dap-session-port parent)))
         (child (pai-dap--register-session client (pai-dap-session-adapter parent) cwd
                                           (and (stringp (plist-get config :program))
                                                (plist-get config :program))
                                           (pai-dap-session-id parent))))
    (condition-case err
        (progn
          (setf (pai-dap-session-capabilities child)
                (pai-dap-client-initialize client (pai-dap--initialize-args (pai-dap-session-adapter parent)) 30.0))
          (setf (pai-dap-session-needs-configuration-done child)
                (eq (plist-get (pai-dap-session-capabilities child) :supportsConfigurationDoneRequest) t))
          (let* ((sent (pai-dap-client-send-async client request (append config (list :cwd cwd))))
                 (seq (car sent)) (slot (cdr sent)))
            (pai-dap--pump-tree child
                                (lambda () (or (pai-dap-session-initialized-seen child)
                                               (pai-dap-client-slot-settled-p slot)))
                                (+ (float-time) 30.0))
            (pai-dap--complete-handshake child 30.0)
            (pai-dap-client-await client seq slot request 30.0)))
      (error (pai-dap--dispose-session child) (signal (car err) (cdr err))))))

;;;; Breakpoints

(defun pai-dap--map-source-breakpoints (input response)
  "Merge INPUT breakpoint records with the adapter RESPONSE breakpoints vector."
  (cl-loop for entry in input for i from 0
           for rb = (and response (> (length response) i) (aref response i))
           collect (list :line (plist-get entry :line)
                         :condition (plist-get entry :condition)
                         :id (plist-get rb :id)
                         :verified (eq (plist-get rb :verified) t)
                         :message (plist-get rb :message))))

(defun pai-dap--map-function-breakpoints (input response)
  "Merge function-breakpoint INPUT records with the adapter RESPONSE vector."
  (cl-loop for entry in input for i from 0
           for rb = (and response (> (length response) i) (aref response i))
           collect (list :name (plist-get entry :name)
                         :condition (plist-get entry :condition)
                         :id (plist-get rb :id)
                         :verified (eq (plist-get rb :verified) t)
                         :message (plist-get rb :message))))

(defun pai-dap--map-instruction-breakpoints (input response)
  "Merge instruction-breakpoint INPUT records with the adapter RESPONSE vector."
  (cl-loop for entry in input for i from 0
           for rb = (and response (> (length response) i) (aref response i))
           collect (list :instructionReference (or (plist-get rb :instructionReference)
                                                   (plist-get entry :instructionReference))
                         :offset (or (plist-get rb :offset) (plist-get entry :offset))
                         :condition (plist-get entry :condition)
                         :hitCondition (plist-get entry :hitCondition)
                         :id (plist-get rb :id)
                         :verified (eq (plist-get rb :verified) t)
                         :message (plist-get rb :message))))

(defun pai-dap--map-data-breakpoints (input response)
  "Merge data-breakpoint INPUT records with the adapter RESPONSE vector."
  (cl-loop for entry in input for i from 0
           for rb = (and response (> (length response) i) (aref response i))
           collect (list :dataId (plist-get entry :dataId)
                         :accessType (plist-get entry :accessType)
                         :condition (plist-get entry :condition)
                         :hitCondition (plist-get entry :hitCondition)
                         :id (plist-get rb :id)
                         :verified (eq (plist-get rb :verified) t)
                         :message (plist-get rb :message))))

(defun pai-dap--source-breakpoint-args (source-path records)
  "Build setBreakpoints arguments for SOURCE-PATH from RECORDS."
  (list :source (list :path source-path :name (file-name-nondirectory source-path))
        :breakpoints (vconcat
                      (mapcar (lambda (e)
                                (append (list :line (plist-get e :line))
                                        (when (plist-get e :condition)
                                          (list :condition (plist-get e :condition)))))
                              records))))

(defun pai-dap-set-breakpoint (file line &optional condition timeout)
  "Set a source breakpoint at FILE:LINE (optional CONDITION).  Return a plist."
  (let* ((session (pai-dap--touch-active))
         (root (pai-dap--root session))
         (path (expand-file-name file))
         (current (cl-sort
                   (append (cl-remove line (gethash path (pai-dap-session-breakpoints root))
                                      :key (lambda (e) (plist-get e :line)))
                           (list (list :verified nil :line line :condition condition)))
                   #'< :key (lambda (e) (plist-get e :line))))
         (args (pai-dap--source-breakpoint-args path current))
         (response (plist-get (pai-dap--request session "setBreakpoints" args timeout) :breakpoints)))
    (puthash path (pai-dap--map-source-breakpoints current response)
             (pai-dap-session-breakpoints session))
    (list :snapshot (pai-dap--summary session)
          :breakpoints (gethash path (pai-dap-session-breakpoints session))
          :source-path path)))

(defun pai-dap-remove-breakpoint (file line &optional timeout)
  "Remove the source breakpoint at FILE:LINE.  Return a plist."
  (let* ((session (pai-dap--touch-active))
         (root (pai-dap--root session))
         (path (expand-file-name file))
         (current (cl-remove line (gethash path (pai-dap-session-breakpoints root))
                             :key (lambda (e) (plist-get e :line))))
         (args (pai-dap--source-breakpoint-args path current))
         (response (plist-get (pai-dap--request session "setBreakpoints" args timeout) :breakpoints)))
    (if (null current)
        (remhash path (pai-dap-session-breakpoints session))
      (puthash path (pai-dap--map-source-breakpoints current response)
               (pai-dap-session-breakpoints session)))
    (list :snapshot (pai-dap--summary session)
          :breakpoints (gethash path (pai-dap-session-breakpoints session))
          :source-path path)))

(defun pai-dap--function-breakpoint-args (records)
  "Build setFunctionBreakpoints arguments from RECORDS."
  (list :breakpoints (vconcat
                      (mapcar (lambda (e)
                                (append (list :name (plist-get e :name))
                                        (when (plist-get e :condition)
                                          (list :condition (plist-get e :condition)))))
                              records))))

(defun pai-dap-set-function-breakpoint (name &optional condition timeout)
  "Set a function breakpoint on NAME (optional CONDITION).  Return a plist."
  (let* ((session (pai-dap--touch-active))
         (root (pai-dap--root session))
         (current (cl-sort
                   (append (cl-remove name (pai-dap-session-function-breakpoints root)
                                      :key (lambda (e) (plist-get e :name)) :test #'equal)
                           (list (list :verified nil :name name :condition condition)))
                   #'string< :key (lambda (e) (plist-get e :name))))
         (response (plist-get (pai-dap--request session "setFunctionBreakpoints"
                                                (pai-dap--function-breakpoint-args current) timeout)
                              :breakpoints)))
    (setf (pai-dap-session-function-breakpoints session)
          (pai-dap--map-function-breakpoints current response))
    (list :snapshot (pai-dap--summary session)
          :breakpoints (pai-dap-session-function-breakpoints session))))

(defun pai-dap-remove-function-breakpoint (name &optional timeout)
  "Remove the function breakpoint on NAME.  Return a plist."
  (let* ((session (pai-dap--touch-active))
         (root (pai-dap--root session))
         (current (cl-remove name (pai-dap-session-function-breakpoints root)
                             :key (lambda (e) (plist-get e :name)) :test #'equal))
         (response (plist-get (pai-dap--request session "setFunctionBreakpoints"
                                                (pai-dap--function-breakpoint-args current) timeout)
                              :breakpoints)))
    (setf (pai-dap-session-function-breakpoints session)
          (pai-dap--map-function-breakpoints current response))
    (list :snapshot (pai-dap--summary session)
          :breakpoints (pai-dap-session-function-breakpoints session))))

(defun pai-dap--instruction-breakpoint-args (records)
  "Build setInstructionBreakpoints arguments from RECORDS."
  (list :breakpoints
        (vconcat (mapcar (lambda (e)
                           (append (list :instructionReference (plist-get e :instructionReference))
                                   (when (plist-get e :offset) (list :offset (plist-get e :offset)))
                                   (when (plist-get e :condition) (list :condition (plist-get e :condition)))
                                   (when (plist-get e :hitCondition)
                                     (list :hitCondition (plist-get e :hitCondition)))))
                         records))))

(defun pai-dap-set-instruction-breakpoint (ref &optional offset condition hit-condition timeout)
  "Set an instruction breakpoint at REF (+OFFSET).  Return a plist."
  (let* ((session (pai-dap--touch-active))
         (root (pai-dap--root session))
         (current (cl-sort
                   (append (cl-remove-if (lambda (e)
                                           (and (equal (plist-get e :instructionReference) ref)
                                                (equal (plist-get e :offset) offset)))
                                         (pai-dap-session-instruction-breakpoints root))
                           (list (list :instructionReference ref :offset offset
                                       :condition condition :hitCondition hit-condition)))
                   (lambda (a b)
                     (let ((c (string< (plist-get a :instructionReference)
                                       (plist-get b :instructionReference))))
                       (if (equal (plist-get a :instructionReference)
                                  (plist-get b :instructionReference))
                           (< (or (plist-get a :offset) 0) (or (plist-get b :offset) 0))
                         c)))))
         (response (plist-get (pai-dap--request session "setInstructionBreakpoints"
                                                (pai-dap--instruction-breakpoint-args current) timeout)
                              :breakpoints)))
    (setf (pai-dap-session-instruction-breakpoints session) current)
    (list :snapshot (pai-dap--summary session)
          :breakpoints (pai-dap--map-instruction-breakpoints current response))))

(defun pai-dap-remove-instruction-breakpoint (ref &optional offset timeout)
  "Remove the instruction breakpoint at REF (+OFFSET).  Return a plist."
  (let* ((session (pai-dap--touch-active))
         (root (pai-dap--root session))
         (current (cl-remove-if (lambda (e)
                                  (and (equal (plist-get e :instructionReference) ref)
                                       (or (null offset) (equal (plist-get e :offset) offset))))
                                (pai-dap-session-instruction-breakpoints root)))
         (response (plist-get (pai-dap--request session "setInstructionBreakpoints"
                                                (pai-dap--instruction-breakpoint-args current) timeout)
                              :breakpoints)))
    (setf (pai-dap-session-instruction-breakpoints session) current)
    (list :snapshot (pai-dap--summary session)
          :breakpoints (pai-dap--map-instruction-breakpoints current response))))

(defun pai-dap--data-breakpoint-args (records)
  "Build setDataBreakpoints arguments from RECORDS."
  (list :breakpoints
        (vconcat (mapcar (lambda (e)
                           (append (list :dataId (plist-get e :dataId))
                                   (when (plist-get e :accessType)
                                     (list :accessType (plist-get e :accessType)))
                                   (when (plist-get e :condition)
                                     (list :condition (plist-get e :condition)))
                                   (when (plist-get e :hitCondition)
                                     (list :hitCondition (plist-get e :hitCondition)))))
                         records))))

(defun pai-dap-data-breakpoint-info (name &optional variables-reference frame-id timeout)
  "Query data breakpoint info for NAME.  Return a plist (:snapshot :info)."
  (let* ((session (pai-dap--touch-active))
         (info (pai-dap--request
                session "dataBreakpointInfo"
                (append (list :name name)
                        (when variables-reference (list :variablesReference variables-reference))
                        (when frame-id (list :frameId frame-id)))
                timeout)))
    (list :snapshot (pai-dap--summary session) :info info)))

(defun pai-dap-set-data-breakpoint (data-id &optional access-type condition hit-condition timeout)
  "Set a data breakpoint on DATA-ID.  Return a plist."
  (let* ((session (pai-dap--touch-active))
         (root (pai-dap--root session))
         (current (cl-sort
                   (append (cl-remove data-id (pai-dap-session-data-breakpoints root)
                                      :key (lambda (e) (plist-get e :dataId)) :test #'equal)
                           (list (list :dataId data-id :accessType access-type
                                       :condition condition :hitCondition hit-condition)))
                   #'string< :key (lambda (e) (plist-get e :dataId))))
         (response (plist-get (pai-dap--request session "setDataBreakpoints"
                                                (pai-dap--data-breakpoint-args current) timeout)
                              :breakpoints)))
    (setf (pai-dap-session-data-breakpoints session) current)
    (list :snapshot (pai-dap--summary session)
          :breakpoints (pai-dap--map-data-breakpoints current response))))

(defun pai-dap-remove-data-breakpoint (data-id &optional timeout)
  "Remove the data breakpoint on DATA-ID.  Return a plist."
  (let* ((session (pai-dap--touch-active))
         (root (pai-dap--root session))
         (current (cl-remove data-id (pai-dap-session-data-breakpoints root)
                             :key (lambda (e) (plist-get e :dataId)) :test #'equal))
         (response (plist-get (pai-dap--request session "setDataBreakpoints"
                                                (pai-dap--data-breakpoint-args current) timeout)
                              :breakpoints)))
    (setf (pai-dap-session-data-breakpoints session) current)
    (list :snapshot (pai-dap--summary session)
          :breakpoints (pai-dap--map-data-breakpoints current response))))

;;;; Execution control

(defun pai-dap--resolve-thread-id (session timeout)
  "Return a usable thread id for SESSION, querying threads if needed."
  (let ((stop (pai-dap-session-stop session)))
    (cond
     ((plist-get stop :threadId) (plist-get stop :threadId))
     ((pai-dap-session-threads session) (plist-get (car (pai-dap-session-threads session)) :id))
     (t (let* ((resp (pai-dap-client-request (pai-dap-session-client session) "threads" nil timeout))
               (threads (append (plist-get resp :threads) nil)))
          (setf (pai-dap-session-threads session) threads)
          (or (plist-get (car threads) :id)
              (error "Debugger reported no threads.")))))))

(defun pai-dap--fetch-top-frame (session timeout)
  "Fetch SESSION's top stack frame and apply it to the stop location."
  (when (plist-get (pai-dap-session-stop session) :threadId)
    (ignore-errors
      (let* ((resp (pai-dap-client-request
                    (pai-dap-session-client session) "stackTrace"
                    (list :threadId (plist-get (pai-dap-session-stop session) :threadId) :levels 1)
                    timeout))
             (frames (append (plist-get resp :stackFrames) nil)))
        (setf (pai-dap-session-last-stack-frames session) frames)
        (pai-dap--apply-top-frame session (car frames))))))

(defun pai-dap--await-stop-outcome (session timeout)
  "Pump until SESSION's tree stops/terminates or TIMEOUT.  Return an outcome plist."
  (let* ((deadline (+ (float-time) timeout))
         (ok (pai-dap--pump-tree
              session
              (lambda () (let ((a (or (pai-dap--active-session) session)))
                           (memq (pai-dap-session-status a) '(stopped terminated))))
              deadline))
         (result (or (pai-dap--active-session) session)))
    (when (eq (pai-dap-session-status result) 'stopped)
      (pai-dap--fetch-top-frame result (min timeout pai-dap--stop-capture-timeout)))
    (let ((status (pai-dap-session-status result)))
      (list :snapshot (pai-dap--summary result)
            :state (cond ((eq status 'stopped) 'stopped)
                         ((eq status 'terminated) 'terminated)
                         (t 'running))
            :timed-out (and (not ok) (eq status 'running))))))

(defun pai-dap-continue (&optional timeout)
  "Continue execution and wait for the next stop.  Return an outcome plist."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (thread-id (pai-dap--resolve-thread-id session timeout)))
    (setf (pai-dap-session-stop session) nil
          (pai-dap-session-last-stack-frames session) nil
          (pai-dap-session-status session) 'running)
    (pai-dap--request session "continue" (list :threadId thread-id) timeout)
    (pai-dap--await-stop-outcome session timeout)))

(defun pai-dap--step (command &optional timeout)
  "Issue step COMMAND (next|stepIn|stepOut) and wait for the next stop."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (thread-id (pai-dap--resolve-thread-id session timeout)))
    (setf (pai-dap-session-stop session) nil
          (pai-dap-session-last-stack-frames session) nil
          (pai-dap-session-status session) 'running)
    (pai-dap--request session command (list :threadId thread-id) timeout)
    (pai-dap--await-stop-outcome session timeout)))

(defun pai-dap-step-over (&optional timeout) "Step over.  See `pai-dap-continue'." (pai-dap--step "next" timeout))
(defun pai-dap-step-in (&optional timeout) "Step in." (pai-dap--step "stepIn" timeout))
(defun pai-dap-step-out (&optional timeout) "Step out." (pai-dap--step "stepOut" timeout))

(defun pai-dap-pause (&optional timeout)
  "Pause execution and wait for the stop.  Return a summary plist."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active)))
    (unless (eq (pai-dap-session-status session) 'stopped)
      (let ((thread-id (pai-dap--resolve-thread-id session timeout)))
        (pai-dap--request session "pause" (list :threadId thread-id) timeout)
        (unless (eq (pai-dap-session-status session) 'stopped)
          (pai-dap--pump-tree session
                              (lambda () (eq (pai-dap-session-status session) 'stopped))
                              (+ (float-time) timeout)))))
    (pai-dap--summary session)))

;;;; Read-only inspection

(defun pai-dap-threads (&optional timeout)
  "Return (:snapshot :threads) aggregated across the live tree."
  (let* ((timeout (or timeout 30.0))
         (anchor (pai-dap--touch-active))
         (merged nil) (seen (make-hash-table :test 'equal)))
    (dolist (target (pai-dap--live-tree-sessions anchor))
      (condition-case nil
          (let ((threads (append (plist-get (pai-dap--request target "threads" nil timeout) :threads) nil)))
            (setf (pai-dap-session-threads target) threads)
            (dolist (th threads)
              (let ((key (format "%s\0%s" (pai-dap-session-id target) (plist-get th :id))))
                (unless (gethash key seen)
                  (puthash key t seen)
                  (push th merged)))))
        (error nil)))
    (list :snapshot (pai-dap--summary anchor) :threads (nreverse merged))))

(defun pai-dap-stack-trace (&optional levels timeout)
  "Return (:snapshot :stack-frames :total-frames) for the active thread."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (thread-id (pai-dap--resolve-thread-id session timeout))
         (resp (pai-dap--request session "stackTrace"
                                 (append (list :threadId thread-id)
                                         (when levels (list :levels levels)))
                                 timeout))
         (frames (append (plist-get resp :stackFrames) nil)))
    (setf (pai-dap-session-last-stack-frames session) frames)
    (pai-dap--apply-top-frame session (car frames))
    (list :snapshot (pai-dap--summary session)
          :stack-frames frames :total-frames (plist-get resp :totalFrames))))

(defun pai-dap-scopes (&optional frame-id timeout)
  "Return (:snapshot :scopes) for FRAME-ID (or the current stopped frame)."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (resolved (or frame-id (plist-get (pai-dap-session-stop session) :frameId))))
    (unless resolved
      (error "No active stack frame. Run stack_trace first or supply frame_id."))
    (let ((resp (pai-dap--request session "scopes" (list :frameId resolved) timeout)))
      (list :snapshot (pai-dap--summary session)
            :scopes (append (plist-get resp :scopes) nil)))))

(defun pai-dap-variables (variable-reference &optional timeout)
  "Return (:snapshot :variables) for VARIABLE-REFERENCE."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (resp (pai-dap--request session "variables"
                                 (list :variablesReference variable-reference) timeout)))
    (list :snapshot (pai-dap--summary session)
          :variables (append (plist-get resp :variables) nil))))

(defun pai-dap-evaluate (expression &optional context frame-id timeout)
  "Evaluate EXPRESSION in CONTEXT.  Return (:snapshot :evaluation)."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (effective (or frame-id (plist-get (pai-dap-session-stop session) :frameId)))
         (resp (pai-dap--request session "evaluate"
                                 (append (list :expression expression :context (or context "repl"))
                                         (when effective (list :frameId effective)))
                                 timeout)))
    (list :snapshot (pai-dap--summary session) :evaluation resp)))

(defun pai-dap-disassemble (memory-reference instruction-count &optional offset instruction-offset resolve-symbols timeout)
  "Disassemble INSTRUCTION-COUNT instructions at MEMORY-REFERENCE."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (resp (pai-dap--request
                session "disassemble"
                (append (list :memoryReference memory-reference :instructionCount instruction-count)
                        (when offset (list :offset offset))
                        (when instruction-offset (list :instructionOffset instruction-offset))
                        (when resolve-symbols (list :resolveSymbols t)))
                timeout)))
    (list :snapshot (pai-dap--summary session)
          :instructions (append (plist-get resp :instructions) nil))))

(defun pai-dap-read-memory (memory-reference count &optional offset timeout)
  "Read COUNT bytes at MEMORY-REFERENCE.  Return (:snapshot :address :data ...)."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (resp (pai-dap--request
                session "readMemory"
                (append (list :memoryReference memory-reference :count count)
                        (when offset (list :offset offset)))
                timeout)))
    (list :snapshot (pai-dap--summary session)
          :address (or (plist-get resp :address) memory-reference)
          :data (plist-get resp :data)
          :unreadable-bytes (plist-get resp :unreadableBytes))))

(defun pai-dap-write-memory (memory-reference data &optional offset allow-partial timeout)
  "Write base64 DATA at MEMORY-REFERENCE.  Return (:snapshot :bytes-written ...)."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (resp (pai-dap--request
                session "writeMemory"
                (append (list :memoryReference memory-reference :data data)
                        (when offset (list :offset offset))
                        (when allow-partial (list :allowPartial t)))
                timeout)))
    (list :snapshot (pai-dap--summary session)
          :offset (plist-get resp :offset)
          :bytes-written (plist-get resp :bytesWritten))))

(defun pai-dap-modules (&optional start-module module-count timeout)
  "List loaded modules.  Return (:snapshot :modules)."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (resp (pai-dap--request
                session "modules"
                (append (when start-module (list :startModule start-module))
                        (when module-count (list :moduleCount module-count)))
                timeout)))
    (list :snapshot (pai-dap--summary session)
          :modules (append (plist-get resp :modules) nil))))

(defun pai-dap-loaded-sources (&optional timeout)
  "List loaded sources.  Return (:snapshot :sources)."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (resp (pai-dap--request session "loadedSources" nil timeout)))
    (list :snapshot (pai-dap--summary session)
          :sources (append (plist-get resp :sources) nil))))

(defun pai-dap-custom-request (command &optional args timeout)
  "Send a custom COMMAND with ARGS.  Return (:snapshot :body)."
  (let* ((timeout (or timeout 30.0))
         (session (pai-dap--touch-active))
         (body (pai-dap--request session command args timeout)))
    (list :snapshot (pai-dap--summary session) :body body)))

(defun pai-dap-get-output (&optional _limit-bytes)
  "Return (:snapshot :output) for the active session."
  (let ((session (pai-dap--touch-active)))
    (list :snapshot (pai-dap--summary session) :output (pai-dap-session-output session))))

;;;; Termination / listing

(defun pai-dap--terminate-tree (session timeout)
  "Terminate SESSION and its descendants, then dispose them."
  (setf (pai-dap-session-status session) 'terminated)
  (dolist (cid (copy-sequence (pai-dap-session-child-ids session)))
    (let ((child (gethash cid pai-dap--sessions)))
      (when child (pai-dap--terminate-tree child timeout))))
  (let ((client (pai-dap-session-client session)))
    (when (eq (plist-get (pai-dap-session-capabilities session) :supportsTerminateRequest) t)
      (ignore-errors (pai-dap-client-request client "terminate" nil (min timeout 5.0))))
    (ignore-errors (pai-dap-client-request client "disconnect" (list :terminateDebuggee t) (min timeout 5.0))))
  (pai-dap--dispose-session session))

(defun pai-dap-terminate (&optional timeout)
  "Terminate the active debug session.  Return its final summary, or nil."
  (let ((session (pai-dap--active-session)))
    (when session
      (pai-dap--touch session)
      (let ((summary (pai-dap--summary session)))
        (pai-dap--terminate-tree (pai-dap--root session) (or timeout 30.0))
        summary))))

(defun pai-dap-list-sessions ()
  "Return a list of summary plists for all sessions."
  (mapcar #'pai-dap--summary (hash-table-values pai-dap--sessions)))

(defun pai-dap-active-summary ()
  "Return the active session summary, or nil."
  (let ((s (pai-dap--active-session)))
    (and s (pai-dap--summary s))))

(defun pai-dap-capabilities ()
  "Return the active session's capabilities plist, or nil."
  (let ((s (pai-dap--active-session)))
    (and s (pai-dap-session-capabilities s))))

(defun pai-dap-shutdown-all ()
  "Terminate and dispose every session (used on session end)."
  (dolist (s (hash-table-values pai-dap--sessions))
    (ignore-errors (pai-dap--dispose-session s)))
  (setq pai-dap--active-id nil)
  (when pai-dap--cleanup-timer (cancel-timer pai-dap--cleanup-timer))
  (setq pai-dap--cleanup-timer nil))

(provide 'pai-dap-session)
;;; pai-dap-session.el ends here
