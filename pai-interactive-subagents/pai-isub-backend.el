;;; pai-isub-backend.el --- Backend protocol for interactive subagents -*- lexical-binding: t; -*-

;;; Commentary:

;; A subagent is *some other interactive agent session* living in its own
;; buffer next to the parent's.  How that session is created and talked to is
;; delegated to a BACKEND, so the same roles, delivery and UI work for a pai
;; session today and (say) an `agent-shell' session tomorrow.
;;
;; A backend is a plist registered with `pai-isub-register-backend':
;;
;;   (:name       "pai"                  ; unique id used in settings/roles
;;    :label      "pai session buffer"   ; shown in the settings screen
;;    :start      (lambda (SPEC) -> HANDLE)            ; required
;;    :send       (lambda (HANDLE TEXT) -> any)        ; required
;;    :buffer     (lambda (HANDLE) -> buffer)          ; default: (:buffer HANDLE)
;;    :busy-p     (lambda (HANDLE) -> bool)            ; optional
;;    :interrupt  (lambda (HANDLE))                    ; optional
;;    :close      (lambda (HANDLE))                    ; default: kill the buffer
;;    :metrics    (lambda (HANDLE) -> (:tokens N :tps N))   ; optional
;;    :transcript (lambda (HANDLE &optional MAX-LINES) -> STRING)) ; optional
;;
;; HANDLE is whatever the backend wants; a plist carrying at least
;; `:buffer' is the conventional shape and makes the defaults work.
;;
;; SPEC (passed to :start) is a plist:
;;
;;   :id           run id, e.g. "sub-3"
;;   :role         role name ("reviewer")
;;   :role-prompt  the role's system prompt (may be nil)
;;   :description  the role's one-line description
;;   :parent       the parent chat buffer
;;   :cwd          working directory for the child
;;   :model        resolved model plist (may be nil for backends with own model)
;;   :thinking     resolved thinking level symbol, or nil
;;   :context-messages  parent messages to inherit ("fork" context), or nil
;;   :emit         (lambda (TYPE &rest PROPS)) -- the child's way back home
;;
;; The backend (or the child session itself) reports upstream through :emit:
;;
;;   (emit 'ready)                    session is up and idle
;;   (emit 'busy)                     a turn started
;;   (emit 'turn-end :text STR)       a turn finished with final text STR
;;   (emit 'reply :text STR)          explicit message aimed at the parent
;;   (emit 'status :text STR)         free-form status for the parent's UI
;;   (emit 'exit :reason SYM)         the session is gone
;;
;; Everything else (roles, model resolution, delivery into the parent turn,
;; foreground/background tool calls, the status overlay) is backend agnostic
;; and lives in `pai-isub-runs'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup pai-isub nil
  "Interactive subagents: child agent sessions in sibling buffers."
  :group 'pai)

(defcustom pai-isub-display-action
  '((display-buffer-reuse-window
     pai-isub-display-buffer-split-right
     display-buffer-pop-up-window)
    (inhibit-same-window . t))
  "`display-buffer' ACTION used to show a subagent session buffer.
The default splits the parent's window in half, like `split-window-right',
and shows the child on the right; other windows keep their size.
The alist also receives a `pai-isub-parent' entry naming the parent buffer."
  :type 'sexp
  :group 'pai-isub)

(defcustom pai-isub-select-new-window nil
  "When non-nil, select the subagent window as it is created."
  :type 'boolean
  :group 'pai-isub)

(defvar pai-isub--backends nil
  "Alist of BACKEND-NAME -> backend plist.
Backends are process-global: they describe *how* to run a child session, not
per-session state.")

;;;; Registry

(defun pai-isub-register-backend (backend)
  "Register BACKEND (a plist, see the Commentary) keyed by its :name.
Re-registering a name replaces the previous definition.  Return BACKEND."
  (let ((name (plist-get backend :name)))
    (unless (and (stringp name) (not (string-empty-p name)))
      (error "Subagent backend needs a :name"))
    (unless (functionp (plist-get backend :start))
      (error "Subagent backend %s needs a :start function" name))
    (unless (functionp (plist-get backend :send))
      (error "Subagent backend %s needs a :send function" name))
    (setq pai-isub--backends
          (cons (cons name backend)
                (assoc-delete-all name pai-isub--backends)))
    backend))

(defun pai-isub-backend (name)
  "Return the backend plist called NAME, or nil."
  (cdr (assoc name pai-isub--backends)))

(defun pai-isub-backend-names ()
  "Return the registered backend names, sorted."
  (sort (mapcar #'car pai-isub--backends) #'string-lessp))

(defun pai-isub-backend-label (name)
  "Return a human label for backend NAME."
  (or (plist-get (pai-isub-backend name) :label) name))

;;;; Protocol calls

(defun pai-isub-backend-start (backend spec)
  "Start a child session for SPEC using BACKEND; return its handle."
  (funcall (plist-get backend :start) spec))

(defun pai-isub-backend-send (backend handle text)
  "Deliver TEXT to the child session HANDLE through BACKEND."
  (funcall (plist-get backend :send) handle text))

(defun pai-isub-backend-buffer (backend handle)
  "Return the buffer showing child HANDLE, or nil."
  (let ((fn (plist-get backend :buffer)))
    (if fn (funcall fn handle) (plist-get handle :buffer))))

(defun pai-isub-backend-busy-p (backend handle)
  "Return non-nil when child HANDLE is mid-turn."
  (let ((fn (plist-get backend :busy-p)))
    (and fn (funcall fn handle))))

(defun pai-isub-backend-interrupt (backend handle)
  "Interrupt the child HANDLE's current turn, if the backend can."
  (let ((fn (plist-get backend :interrupt)))
    (when fn (funcall fn handle) t)))

(defun pai-isub-backend-close (backend handle)
  "Tear down child HANDLE.  Defaults to killing its buffer."
  (let ((fn (plist-get backend :close)))
    (if fn
        (funcall fn handle)
      (let ((buf (pai-isub-backend-buffer backend handle)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defun pai-isub-backend-metrics (backend handle)
  "Return live metrics (:tokens N :tps N) for HANDLE, or nil."
  (let ((fn (plist-get backend :metrics)))
    (and fn (ignore-errors (funcall fn handle)))))

(defun pai-isub-backend-transcript (backend handle &optional max-lines)
  "Return up to MAX-LINES of HANDLE's transcript text, or nil."
  (let ((fn (plist-get backend :transcript)))
    (and fn (ignore-errors (funcall fn handle max-lines)))))

(defun pai-isub-backend-live-p (backend handle)
  "Return non-nil while child HANDLE still exists."
  (let ((buf (pai-isub-backend-buffer backend handle)))
    (if (bufferp buf) (buffer-live-p buf) (and handle t))))

;;;; Display

(defun pai-isub-display-buffer-split-right (buffer alist)
  "Display action: split the parent's window in half and show BUFFER right.
The parent is ALIST's `pai-isub-parent' buffer; its window is split like
`split-window-right' does, but with `window-combination-resize' bound to
nil so only that window gives up space.  Falls back to the selected window
when the parent is not visible.  Return the new window, or nil."
  (let* ((parent (cdr (assq 'pai-isub-parent alist)))
         (pwin (or (and (buffer-live-p parent)
                        (get-buffer-window parent))
                   (selected-window)))
         (new (and (not (window-minibuffer-p pwin))
                   (ignore-errors
                     (let ((window-combination-resize nil))
                       (split-window pwin nil 'right))))))
    (when new
      (window--display-buffer buffer new 'window alist))))

(defun pai-isub-display (buffer &optional parent)
  "Show BUFFER next to PARENT's window, per `pai-isub-display-action'.
PARENT defaults to the current buffer."
  (when (buffer-live-p buffer)
    (let* ((action pai-isub-display-action)
           (action (cons (car action)
                         (cons (cons 'pai-isub-parent
                                     (or parent (current-buffer)))
                               (cdr action))))
           (win (display-buffer buffer action)))
      (when (and win pai-isub-select-new-window) (select-window win))
      win)))

(provide 'pai-isub-backend)
;;; pai-isub-backend.el ends here
