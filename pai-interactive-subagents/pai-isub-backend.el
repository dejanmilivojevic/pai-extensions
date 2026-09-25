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
(require 'seq)

(defgroup pai-isub nil
  "Interactive subagents: child agent sessions in sibling buffers."
  :group 'pai)

(defcustom pai-isub-display-action
  '((display-buffer-reuse-window
     pai-isub-display-buffer-split-right
     display-buffer-pop-up-window)
    (inhibit-same-window . t))
  "`display-buffer' ACTION used to show a subagent session buffer.
The default splits the parent's window in half for the first subagent and
shows it on the right; further subagents join it there, and all of them
share that side equally (see `pai-isub-display-buffer-split-right').
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
;;
;; The first subagent takes the right half of its parent's window.  Every
;; further one splits the rightmost subagent window; the first such split
;; nests the subagents into a window combination of their own, so they can
;; be balanced -- all equally wide -- without touching the parent.  They are
;; balanced again whenever one opens or closes.

(defvar-local pai-isub--shown-for nil
  "The parent chat buffer this buffer is shown next to as a subagent, or nil.")
(put 'pai-isub--shown-for 'permanent-local t)

(defun pai-isub-sibling-windows (parent &optional frame)
  "Return the windows on FRAME showing subagents of PARENT, left to right."
  (sort (seq-filter (lambda (w)
                      (let ((b (window-buffer w)))
                        (and (not (eq b parent))
                             (eq (buffer-local-value 'pai-isub--shown-for b) parent))))
                    (window-list frame 'no-mini))
        (lambda (a b) (< (window-left-column a) (window-left-column b)))))

(defun pai-isub-balance (parent &optional frame)
  "Give PARENT's subagent windows on FRAME equal widths.
Only when they form a window combination of their own, so the parent's
window and any others keep their size."
  (let* ((subs (pai-isub-sibling-windows parent frame))
         (group (and (cdr subs) (window-parent (car subs)))))
    (when (and group
               (window-combined-p (car subs) t)
               (seq-every-p (lambda (w) (eq (window-parent w) group)) subs)
               ;; nothing but subagents in the group
               (= (length subs) (window-child-count group)))
      (balance-windows group))))

(defun pai-isub-display-buffer-split-right (buffer alist)
  "Display action: show BUFFER as a subagent of ALIST's `pai-isub-parent'.
The first subagent splits the parent's window in half, like
`split-window-right', with `window-combination-resize' bound to nil so only
that window gives up space; later ones split the rightmost subagent window,
and the subagents are then balanced to equal widths (`pai-isub-balance').
Falls back to the selected window when the parent is not visible.  Return
the new window, or nil."
  (let* ((parent (cdr (assq 'pai-isub-parent alist)))
         (pwin (or (and (buffer-live-p parent)
                        (get-buffer-window parent))
                   (selected-window)))
         (subs (and (buffer-live-p parent)
                    (pai-isub-sibling-windows parent (window-frame pwin))))
         (target (if subs (car (last subs)) pwin))
         (new (or (and (not (window-minibuffer-p target))
                       (ignore-errors
                         (let ((window-combination-resize nil)
                               ;; nest the subagents into their own combination
                               ;; the first time one is split off the parent's
                               (window-combination-limit
                                (and subs (eq (window-parent target) (window-parent pwin)))))
                           (split-window target nil 'right))))
                  ;; the subagents are too narrow to split further: take the
                  ;; room from the parent rather than covering a subagent
                  (and subs (not (window-minibuffer-p pwin))
                       (ignore-errors
                         (let ((window-combination-resize nil))
                           (split-window pwin nil 'right)))))))
    (when new
      (set-window-parameter new 'pai-isub-window t)
      (prog1 (window--display-buffer buffer new 'window alist)
        (when (buffer-live-p parent) (pai-isub-balance parent (window-frame new)))))))

(defun pai-isub-display (buffer &optional parent)
  "Show BUFFER next to PARENT's window, per `pai-isub-display-action'.
PARENT defaults to the current buffer."
  (when (buffer-live-p buffer)
    (let* ((parent (or parent (current-buffer)))
           (action pai-isub-display-action)
           (action (cons (car action)
                         (cons (cons 'pai-isub-parent parent)
                               (cdr action)))))
      (with-current-buffer buffer (setq pai-isub--shown-for parent))
      (let ((win (display-buffer buffer action)))
        (when (and win pai-isub-select-new-window) (select-window win))
        win))))

(defun pai-isub-retire-windows (buffer parent)
  "Close the windows subagent BUFFER was shown in, then rebalance PARENT's.
Call while BUFFER is still displayed (before it is killed).  Only windows
created for a subagent are deleted, and only once they no longer show a
subagent -- a window you reused for something else, or the parent's own
window, stays.  The deletion runs right after the current command, when
killing BUFFER has put another buffer in those windows."
  (let ((wins (and (buffer-live-p buffer)
                   (seq-filter (lambda (w) (window-parameter w 'pai-isub-window))
                               (get-buffer-window-list buffer nil t)))))
    (run-at-time
     0 nil
     (lambda ()
       (dolist (w wins)
         (when (and (window-live-p w)
                    (not (buffer-local-value 'pai-isub--shown-for (window-buffer w)))
                    (not (one-window-p t (window-frame w))))
           (ignore-errors (delete-window w))))
       (when (buffer-live-p parent)
         (dolist (frame (frame-list))
           (pai-isub-balance parent frame)))))))

(provide 'pai-isub-backend)
;;; pai-isub-backend.el ends here
