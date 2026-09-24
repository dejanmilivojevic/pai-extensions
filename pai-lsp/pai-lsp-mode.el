;;; pai-lsp-mode.el --- lsp-mode backend for pai-lsp -*- lexical-binding: t; -*-

;;; Commentary:

;; Implements the pai-lsp abstraction on top of lsp-mode
;; (https://github.com/emacs-lsp/lsp-mode).
;;
;; Like Eglot, lsp-mode is buffer-centric: it manages a workspace per project
;; and keeps documents in sync once a file buffer is visited and lsp is
;; connected.  This backend therefore derives the workspace from the target
;; file's buffer and issues synchronous `lsp-request's.  lsp-mode returns
;; results as hash-tables with string keys, so every result is converted to the
;; keyword-plist shape the pai-lsp tool expects (matching the Eglot backend).
;; Positions are LSP-style plists (:line L :character C), 0-based.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-lsp-core)

(declare-function lsp "lsp-mode" (&optional arg))
(declare-function lsp-request "lsp-mode" (method params &rest args))
(declare-function lsp-notify "lsp-mode" (method params))
(declare-function lsp-workspaces "lsp-mode" ())
(declare-function lsp-workspace-root "lsp-mode" (&optional path))
(declare-function lsp-workspace-restart "lsp-mode" (workspace))
(declare-function lsp-workspace-shutdown "lsp-mode" (workspace))
(declare-function lsp-diagnostics "lsp-diagnostics" (&optional current-workspace?))
(declare-function lsp--server-capabilities "lsp-mode" ())
(declare-function lsp--text-document-position-params "lsp-mode" (&optional identifier position))
(declare-function lsp--text-document-identifier "lsp-mode" ())
(declare-function lsp--path-to-uri "lsp-mode" (path))
(declare-function lsp--uri-to-path "lsp-mode" (uri))
(declare-function lsp--workspace-print "lsp-mode" (workspace))

(defvar lsp-auto-guess-root)
(defvar lsp-enable-file-watchers)
(defvar lsp-enable-snippet)
(defvar lsp-warn-no-matched-clients)
(defvar lsp-restart)
(defvar lsp-response-timeout)
(defvar lsp-enable-symbol-highlighting)
(defvar lsp-headerline-breadcrumb-enable)
(defvar lsp-modeline-diagnostics-enable)
(defvar lsp-signature-auto-activate)
(defvar lsp-keep-workspace-alive)

(defconst pai-lsp-mode--connect-timeout 20.0
  "Seconds to wait for an lsp-mode workspace to come up.")

;;;; Availability

(defun pai-lsp-mode--available-p ()
  "Return non-nil if lsp-mode is available."
  (and (locate-library "lsp-mode") t))

;;;; Result conversion (lsp-mode hash-tables -> keyword plists)

(defun pai-lsp-mode--to-plist (x)
  "Recursively convert lsp-mode result X (hash-tables/vectors) to keyword plists."
  (cond
   ((hash-table-p x)
    (let (out)
      (maphash (lambda (k v)
                 (setq out (plist-put out (intern (concat ":" (format "%s" k)))
                                      (pai-lsp-mode--to-plist v))))
               x)
      out))
   ((and (vectorp x) (not (stringp x)))
    (apply #'vector (mapcar #'pai-lsp-mode--to-plist (append x nil))))
   ((and (consp x) (not (keywordp (car x))))
    (mapcar #'pai-lsp-mode--to-plist x))
   (t x)))

;;;; Connection

(defmacro pai-lsp-mode--with-suppressed (&rest body)
  "Run BODY with lsp-mode's interactive prompts and UI churn suppressed."
  `(let ((lsp-auto-guess-root t)
         (lsp-enable-file-watchers nil)
         (lsp-enable-snippet nil)
         (lsp-warn-no-matched-clients nil)
         (lsp-restart 'ignore)
         (lsp-enable-symbol-highlighting nil)
         (lsp-headerline-breadcrumb-enable nil)
         (lsp-modeline-diagnostics-enable nil)
         (lsp-signature-auto-activate nil)
         (lsp-keep-workspace-alive nil)
         (lsp-response-timeout (max 15 (truncate pai-lsp-mode--connect-timeout))))
     ,@body))

(declare-function lsp--workspace-status "lsp-mode" (workspace))

(defun pai-lsp-mode--initialized-p (&optional ws)
  "Return non-nil once WS (or the current buffer's workspace) is usable.
lsp-mode creates the workspace in a `starting' state before the server's
`initialize' response registers capabilities; requests fail until then."
  (let ((ws (or ws (car (lsp-workspaces)))))
    (and ws
         (or (eq (ignore-errors (lsp--workspace-status ws)) 'initialized)
             (let ((caps (ignore-errors (lsp--server-capabilities))))
               (and (hash-table-p caps) (> (hash-table-count caps) 0)))))))

(defun pai-lsp-mode--ensure ()
  "Ensure lsp-mode has an initialized workspace for the current buffer; return it."
  (require 'lsp-mode)
  (unless (pai-lsp-mode--initialized-p)
    (unless buffer-file-name
      (error "pai-lsp: buffer %s has no file" (buffer-name)))
    (pai-lsp-mode--with-suppressed
     (unless (lsp-workspaces) (lsp))
     (let ((deadline (+ (float-time) pai-lsp-mode--connect-timeout)))
       (while (and (not (pai-lsp-mode--initialized-p)) (< (float-time) deadline))
         (accept-process-output nil 0.1)))))
  (or (car (lsp-workspaces))
      (error "pai-lsp: lsp-mode could not start a workspace for %s" buffer-file-name)))

(defmacro pai-lsp-mode--with-file (file &rest body)
  "Visit FILE, ensure an lsp-mode workspace, and run BODY in its buffer.
Within BODY, `workspace' is bound to the active workspace."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (find-file-noselect ,file)))
     (with-current-buffer buf
       (let ((workspace (pai-lsp-mode--ensure)))
         (ignore workspace)
         ,@body))))

(defun pai-lsp-mode--goto (position)
  "Move point to LSP POSITION (plist :line/:character, 0-based)."
  (when position
    (goto-char (point-min))
    (forward-line (or (plist-get position :line) 0))
    (let ((col (or (plist-get position :character) 0)))
      (forward-char (min col (max 0 (- (line-end-position) (point))))))))

(defun pai-lsp-mode--request (method &optional params)
  "Run synchronous METHOD with PARAMS, returning a converted keyword-plist."
  (pai-lsp-mode--with-suppressed
   (pai-lsp-mode--to-plist (lsp-request method (or params (list))))))

(defun pai-lsp-mode--position-request (file position method &optional extra)
  "Run METHOD at POSITION in FILE, returning a converted result."
  (pai-lsp-mode--with-file file
    (pai-lsp-mode--goto position)
    (pai-lsp-mode--request method (append (lsp--text-document-position-params) extra))))

;;;; Lifecycle

(defun pai-lsp-mode--init (_session) t)

(defun pai-lsp-mode--shutdown (_session)
  "Shut down every lsp-mode workspace this session started."
  (require 'lsp-mode)
  (dolist (ws (ignore-errors (lsp-workspaces)))
    (ignore-errors (lsp-workspace-shutdown ws)))
  t)

(defun pai-lsp-mode--get-client (_session project-root)
  "Return an opaque client handle (the project root) for PROJECT-ROOT."
  (or project-root default-directory))

(defun pai-lsp-mode--get-capabilities (client)
  "Return CLIENT's server capabilities plist (CLIENT is a file path)."
  (when (and client (stringp client) (file-exists-p client)
             (not (file-directory-p client)))
    (pai-lsp-mode--with-file client
      (pai-lsp-mode--to-plist (lsp--server-capabilities)))))

;;;; Low-level request/notify

(defun pai-lsp-mode--request-api (client method params &optional on-result on-error)
  "Send synchronous request METHOD with PARAMS via CLIENT's file workspace."
  (pai-lsp-mode--with-file client
    (condition-case err
        (let ((res (pai-lsp-mode--request method params)))
          (if on-result (progn (funcall on-result res) t) res))
      (error (if on-error (progn (funcall on-error err) t)
               (signal (car err) (cdr err)))))))

(defun pai-lsp-mode--notify (client method params)
  "Send notification METHOD with PARAMS via CLIENT's file workspace."
  (pai-lsp-mode--with-file client
    (pai-lsp-mode--with-suppressed (lsp-notify method params))))

;;;; Document sync (lsp-mode syncs automatically once the buffer is managed)

(defun pai-lsp-mode--did-open (client file _content _language-id _version)
  (pai-lsp-mode--with-file file (ignore client) t))
(defun pai-lsp-mode--did-change (client file _changes _version)
  (pai-lsp-mode--with-file file (ignore client) t))
(defun pai-lsp-mode--did-save (client file _content)
  (pai-lsp-mode--with-file file (ignore client) t))
(defun pai-lsp-mode--did-close (client file)
  (ignore client file) t)

;;;; High-level actions

(defun pai-lsp-mode--severity (n)
  "Map an LSP diagnostic severity number N to a keyword-ish string."
  (pcase n (1 "error") (2 "warning") (3 "information") (4 "hint") (_ (format "%s" n))))

(defun pai-lsp-mode--diagnostics (_client &optional file _timeout)
  "Return diagnostics for FILE as a list of (:line :severity :message) plists."
  (unless file (error "pai-lsp: diagnostics requires a file"))
  (pai-lsp-mode--with-file file
    (let* ((all (ignore-errors (lsp-diagnostics t)))
           (path (expand-file-name file))
           (diags (and all (or (gethash path all) (gethash file all)))))
      (cl-loop for d in (append diags nil)
               for dp = (pai-lsp-mode--to-plist d)
               for start = (plist-get (plist-get dp :range) :start)
               collect (list :line (1+ (or (plist-get start :line) 0))
                             :severity (pai-lsp-mode--severity (plist-get dp :severity))
                             :message (plist-get dp :message))))))

(defun pai-lsp-mode--definition (client file position)
  (ignore client)
  (pai-lsp-mode--position-request file position "textDocument/definition"))

(defun pai-lsp-mode--references (client file position include-declaration)
  (ignore client)
  (pai-lsp-mode--position-request
   file position "textDocument/references"
   (list :context (list :includeDeclaration (if include-declaration t :json-false)))))

(defun pai-lsp-mode--hover (client file position)
  (ignore client)
  (pai-lsp-mode--position-request file position "textDocument/hover"))

(defun pai-lsp-mode--type-definition (client file position)
  (ignore client)
  (pai-lsp-mode--position-request file position "textDocument/typeDefinition"))

(defun pai-lsp-mode--implementation (client file position)
  (ignore client)
  (pai-lsp-mode--position-request file position "textDocument/implementation"))

(defun pai-lsp-mode--rename (client file position new-name)
  (ignore client)
  (pai-lsp-mode--position-request file position "textDocument/rename"
                                  (list :newName new-name)))

(defun pai-lsp-mode--code-actions (client file range context)
  (ignore client)
  (pai-lsp-mode--with-file file
    (pai-lsp-mode--request
     "textDocument/codeAction"
     (list :textDocument (lsp--text-document-identifier)
           :range range
           :context (or context (list :diagnostics []))))))

(defun pai-lsp-mode--rename-file (client old-uri new-uri)
  (ignore client)
  (let ((old-path (if (string-prefix-p "file:" old-uri)
                      (progn (require 'lsp-mode) (lsp--uri-to-path old-uri)) old-uri)))
    (pai-lsp-mode--with-file old-path
      (pai-lsp-mode--request
       "workspace/willRenameFiles"
       (list :files (vector (list :oldUri (lsp--path-to-uri old-path)
                                  :newUri (if (string-prefix-p "file:" new-uri) new-uri
                                            (lsp--path-to-uri new-uri)))))))))

(defun pai-lsp-mode--symbols (_client &optional query file)
  "Return workspace symbols (with QUERY) or document symbols for FILE."
  (unless file (error "pai-lsp: symbols requires a file"))
  (pai-lsp-mode--with-file file
    (if (and query (not (string-empty-p query)))
        (pai-lsp-mode--request "workspace/symbol" (list :query query))
      (pai-lsp-mode--request "textDocument/documentSymbol"
                             (list :textDocument (lsp--text-document-identifier))))))

(defun pai-lsp-mode--reload (client &optional file)
  "Restart the lsp-mode workspace serving FILE (or CLIENT's project)."
  (let ((path (or file (and (stringp client) (file-exists-p client)
                            (not (file-directory-p client)) client))))
    (if path
        (pai-lsp-mode--with-file path
          (ignore-errors (lsp-workspace-restart workspace))
          t)
      (error "pai-lsp: reload requires a file"))))

(defun pai-lsp-mode--capability-summary (caps)
  "Return provider names advertised in CAPS (a converted capabilities plist)."
  (cl-loop for (key . name) in '((:hoverProvider . "hover")
                                 (:definitionProvider . "definition")
                                 (:typeDefinitionProvider . "typeDefinition")
                                 (:implementationProvider . "implementation")
                                 (:referencesProvider . "references")
                                 (:documentSymbolProvider . "documentSymbol")
                                 (:workspaceSymbolProvider . "workspaceSymbol")
                                 (:renameProvider . "rename")
                                 (:codeActionProvider . "codeAction")
                                 (:completionProvider . "completion"))
           for val = (plist-get caps key)
           when (and val (not (eq val :json-false)))
           collect name))

(defun pai-lsp-mode--status (client &optional file)
  "Return a status plist for the workspace serving FILE (or CLIENT)."
  (let ((path (or file (and (stringp client) (file-exists-p client)
                            (not (file-directory-p client)) client))))
    (unless path (error "pai-lsp: status requires a file"))
    (pai-lsp-mode--with-file path
      (list :backend "lsp-mode"
            :server (ignore-errors (lsp--workspace-print workspace))
            :project (ignore-errors (lsp-workspace-root path))
            :live (and (lsp-workspaces) t)
            :capabilities (pai-lsp-mode--capability-summary
                           (pai-lsp-mode--to-plist (lsp--server-capabilities)))))))

;;;; Register backend

(pai-lsp-register-backend
 (pai-lsp--backend-create
  :id 'lsp-mode
  :name "lsp-mode"
  :available-p #'pai-lsp-mode--available-p
  :init #'pai-lsp-mode--init
  :shutdown #'pai-lsp-mode--shutdown
  :get-client #'pai-lsp-mode--get-client
  :get-capabilities #'pai-lsp-mode--get-capabilities
  :request #'pai-lsp-mode--request-api
  :notify #'pai-lsp-mode--notify
  :text-document-did-open #'pai-lsp-mode--did-open
  :text-document-did-change #'pai-lsp-mode--did-change
  :text-document-did-save #'pai-lsp-mode--did-save
  :text-document-did-close #'pai-lsp-mode--did-close
  :diagnostics #'pai-lsp-mode--diagnostics
  :definition #'pai-lsp-mode--definition
  :references #'pai-lsp-mode--references
  :hover #'pai-lsp-mode--hover
  :rename #'pai-lsp-mode--rename
  :code-actions #'pai-lsp-mode--code-actions
  :rename-file #'pai-lsp-mode--rename-file
  :symbols #'pai-lsp-mode--symbols
  :type-definition #'pai-lsp-mode--type-definition
  :implementation #'pai-lsp-mode--implementation
  :reload #'pai-lsp-mode--reload
  :status #'pai-lsp-mode--status))

(provide 'pai-lsp-mode)
;;; pai-lsp-mode.el ends here
