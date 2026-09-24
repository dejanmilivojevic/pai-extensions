;;; pai-lsp.el --- LSP abstraction layer for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides an abstraction over LSP backends (eglot, lsp-mode).
;; Extensions and tools should use the API functions here rather than
;; backend-specific code.  The active backend is chosen via the setting
;; `:lsp (:backend (eglot|lsp-mode))`.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'pai-core)
(require 'pai-settings)

;;;; Backend interface

(cl-defstruct (pai-lsp--backend
               (:constructor pai-lsp--backend-create))
  id
  name
  available-p
  init
  shutdown
  get-client
  get-capabilities
  request
  notify
  text-document-did-open
  text-document-did-change
  text-document-did-save
  text-document-did-close
  diagnostics
  definition
  references
  hover
  rename
  code-actions
  rename-file
  symbols
  type-definition
  implementation
  reload
  status)

;;;; Backend registry

(defvar pai-lsp--backends '()
  "Alist of (ID . BACKEND-STRUCT) for registered backends.")

(defvar pai-lsp--current-backend nil
  "The currently active backend struct, or nil.")

(defun pai-lsp-register-backend (backend)
  "Register BACKEND (a `pai-lsp--backend' struct).
Activation is deferred to `pai-lsp--ensure-backend' (driven by the configured
`:lsp :backend' preference), so registration order never decides the winner."
  (let ((id (pai-lsp--backend-id backend)))
    (setq pai-lsp--backends
          (cons (cons id backend)
                (assq-delete-all id pai-lsp--backends)))))

(defun pai-lsp--activate-backend (backend)
  "Activate BACKEND as the current backend."
  (setq pai-lsp--current-backend backend)
  (message "LSP backend activated: %s" (pai-lsp--backend-name backend)))

(defun pai-lsp-get-backend (&optional id)
  "Return the backend struct for ID, or the current backend if ID is nil."
  (or (and id (cdr (assoc id pai-lsp--backends)))
      pai-lsp--current-backend))

(defun pai-lsp-available-backends ()
  "Return list of (ID NAME) for backends that are available."
  (cl-loop for (id . be) in pai-lsp--backends
           when (funcall (pai-lsp--backend-available-p be))
           collect (list id (pai-lsp--backend-name be))))

;;;; Public API

(defun pai-lsp-init (session)
  "Initialize LSP for SESSION using the active backend.
Returns t on success, nil if no backend available."
  (let ((be pai-lsp--current-backend))
    (when (and be (funcall (pai-lsp--backend-available-p be)))
      (funcall (pai-lsp--backend-init be) session))))

(defun pai-lsp-shutdown (session)
  "Shutdown LSP for SESSION."
  (when pai-lsp--current-backend
    (funcall (pai-lsp--backend-shutdown pai-lsp--current-backend) session)))

(defun pai-lsp-get-client (session project-root)
  "Get or create an LSP client for PROJECT-ROOT in SESSION."
  (when pai-lsp--current-backend
    (funcall (pai-lsp--backend-get-client pai-lsp--current-backend)
             session project-root)))

(defun pai-lsp-request (client method params &optional on-result on-error)
  "Send an async LSP request.
ON-RESULT is called with the result, ON-ERROR with an error object."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-request pai-lsp--current-backend)
             client method params on-result on-error)))

(defun pai-lsp-notify (client method params)
  "Send an LSP notification (fire-and-forget)."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-notify pai-lsp--current-backend)
             client method params)))

;;;; Document sync helpers

(defun pai-lsp-did-open (client file content language-id version)
  "Notify server that FILE was opened."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-text-document-did-open pai-lsp--current-backend)
             client file content language-id version)))

(defun pai-lsp-did-change (client file changes version)
  "Notify server that FILE changed. CHANGES is a list of change objects."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-text-document-did-change pai-lsp--current-backend)
             client file changes version)))

(defun pai-lsp-did-save (client file content)
  "Notify server that FILE was saved."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-text-document-did-save pai-lsp--current-backend)
             client file content)))

(defun pai-lsp-did-close (client file)
  "Notify server that FILE was closed."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-text-document-did-close pai-lsp--current-backend)
             client file)))

;;;; High-level actions (return t for async, result for sync)

(defun pai-lsp-diagnostics (client &optional file)
  "Get diagnostics for FILE or all files."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-diagnostics pai-lsp--current-backend)
             client file)))

(defun pai-lsp-definition (client file position)
  "Get definition locations for FILE at POSITION (0-based line/char)."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-definition pai-lsp--current-backend)
             client file position)))

(defun pai-lsp-references (client file position include-declaration)
  "Get reference locations."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-references pai-lsp--current-backend)
             client file position include-declaration)))

(defun pai-lsp-hover (client file position)
  "Get hover information."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-hover pai-lsp--current-backend)
             client file position)))

(defun pai-lsp-rename (client file position new-name)
  "Prepare rename workspace edit."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-rename pai-lsp--current-backend)
             client file position new-name)))

(defun pai-lsp-code-actions (client file range context)
  "Get code actions for RANGE in FILE."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-code-actions pai-lsp--current-backend)
             client file range context)))

(defun pai-lsp-rename-file (client old-uri new-uri)
  "Prepare file rename workspace edit."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-rename-file pai-lsp--current-backend)
             client old-uri new-uri)))

(defun pai-lsp-symbols (client &optional query file)
  "Get document (or, with QUERY, workspace) symbols.  FILE selects the server."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-symbols pai-lsp--current-backend)
             client query file)))

(defun pai-lsp-type-definition (client file position)
  "Get type definition locations."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-type-definition pai-lsp--current-backend)
             client file position)))

(defun pai-lsp-implementation (client file position)
  "Get implementation locations."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-implementation pai-lsp--current-backend)
             client file position)))

(defun pai-lsp-reload (client &optional file)
  "Reload/restart the LSP server serving FILE (or CLIENT's project)."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-reload pai-lsp--current-backend)
             client file)))

(defun pai-lsp-status (client &optional file)
  "Get server status for FILE (or CLIENT's project)."
  (when (and pai-lsp--current-backend client)
    (funcall (pai-lsp--backend-status pai-lsp--current-backend)
             client file)))

;;;; Settings integration

(defun pai-lsp--config ()
  "Return the :lsp settings plist."
  (or (pai-settings-get :lsp) '()))

(defun pai-lsp--configured-backend-id ()
  "Return the configured backend ID as a symbol, defaulting to `eglot'.
The value may arrive as a symbol (Lisp config) or a string (JSON settings or
the settings UI), so it is always coerced to a symbol."
  (let ((v (plist-get (pai-lsp--config) :backend)))
    (cond ((and v (symbolp v)) v)
          ((and (stringp v) (not (string-empty-p v))) (intern v))
          (t 'eglot))))

(defun pai-lsp--available-backend (id)
  "Return the registered backend for ID when it is available, else nil."
  (let ((be (cdr (assq id pai-lsp--backends))))
    (and be (funcall (pai-lsp--backend-available-p be)) be)))

(defun pai-lsp--first-available-backend ()
  "Return any available backend (registration-independent), preferring Eglot."
  (or (pai-lsp--available-backend 'eglot)
      (cl-loop for (_id . be) in pai-lsp--backends
               when (funcall (pai-lsp--backend-available-p be)) return be)))

(defun pai-lsp--ensure-backend ()
  "Activate the configured backend, or fall back to any available one.
Coerces the configured id, avoids re-activating the current backend, and emits
a single, non-contradictory message."
  (let* ((id (pai-lsp--configured-backend-id))
         (be (pai-lsp--available-backend id)))
    (cond
     (be
      (unless (eq pai-lsp--current-backend be) (pai-lsp--activate-backend be)))
     (t
      (let ((fallback (pai-lsp--first-available-backend)))
        (cond
         (fallback
          (unless (eq pai-lsp--current-backend fallback)
            (pai-lsp--activate-backend fallback))
          (message "pai-lsp: backend `%s' unavailable; using `%s' instead"
                   id (pai-lsp--backend-name fallback)))
         (t
          (setq pai-lsp--current-backend nil)
          (message "pai-lsp: no LSP backend available (configured `%s')" id))))))))

;; Call this after settings change
(defun pai-lsp-settings-changed ()
  "Re-evaluate backend choice after settings change."
  (pai-lsp--ensure-backend))

(provide 'pai-lsp-core)
;;; pai-lsp-core.el ends here