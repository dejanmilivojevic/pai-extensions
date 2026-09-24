;;; pai-eglot.el --- Eglot backend for pai-lsp -*- lexical-binding: t; -*-

;;; Commentary:

;; Eglot (built into Emacs 29+) backend for the pai-lsp abstraction.
;;
;; Eglot is buffer-centric: it manages a language server per project/mode and
;; keeps documents in sync automatically once a file buffer is visited and
;; `eglot--managed-mode' is active.  This backend therefore derives the server
;; from the target file's buffer for every action and issues *synchronous*
;; `jsonrpc-request's, which is what a one-shot tool call wants.  The abstract
;; `client' handle is just the project root string; the real work keys off the
;; file.  Positions are LSP-style plists (:line L :character C), 0-based.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eglot)
(require 'jsonrpc)
(require 'flymake)
(require 'project)
(require 'pai-lsp-core)

;;;; Availability check

(defun pai-eglot--available-p ()
  "Return non-nil if Eglot is available."
  (and (locate-library "eglot") t))

;;;; Server acquisition

(defun pai-eglot--ensure-server ()
  "Return a live Eglot server for the current buffer, connecting if needed.
Signals an error if the buffer has no file or no server can be guessed."
  (or (eglot-current-server)
      (progn
        (unless buffer-file-name
          (error "pai-lsp: buffer %s has no file" (buffer-name)))
        ;; `eglot--guess-contact' (non-interactive) reads `eglot-server-programs'
        ;; for the buffer's major mode and errors if none matches.  `eglot--connect'
        ;; performs the initialize handshake synchronously and returns the server.
        (apply #'eglot--connect (eglot--guess-contact))
        (or (eglot-current-server)
            (error "pai-lsp: failed to start an Eglot server for %s"
                   buffer-file-name)))))

(defmacro pai-eglot--with-file (file &rest body)
  "Visit FILE, ensure an Eglot server, and evaluate BODY in its buffer.
Within BODY, `server' is bound to the live Eglot server object."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (find-file-noselect ,file)))
     (with-current-buffer buf
       (let ((server (pai-eglot--ensure-server)))
         (ignore server)
         ,@body))))

(defun pai-eglot--goto (position)
  "Move point to LSP POSITION (plist :line/:character, 0-based) in current buffer."
  (when position
    (goto-char (eglot--lsp-position-to-point position t))))

(defun pai-eglot--pump (server seconds)
  "Pump process output for SERVER for up to SECONDS, or until diagnostics arrive."
  (let ((deadline (+ (float-time) seconds))
        (proc (jsonrpc--process server)))
    (while (and (< (float-time) deadline)
                (process-live-p proc)
                (null eglot--diagnostics))
      (accept-process-output proc 0.05))
    ;; Give any trailing publishDiagnostics a brief window even once one arrived.
    (accept-process-output proc 0.05)))

;;;; Lifecycle

(defun pai-eglot--init (_session)
  "Initialize the Eglot backend for SESSION.  Nothing global to do."
  t)

(defun pai-eglot--shutdown (_session)
  "Shut down every Eglot server started for SESSION."
  (dolist (server (cl-remove-duplicates
                   (cl-loop for servers being the hash-values
                            of eglot--servers-by-project
                            append servers)))
    (ignore-errors (eglot-shutdown server nil nil t)))
  t)

;;;; Client handle

(defun pai-eglot--get-client (_session project-root)
  "Return an opaque client handle for PROJECT-ROOT.
Eglot derives the actual server from the file buffer, so the handle is just
the project root string."
  (or project-root default-directory))

(defun pai-eglot--get-capabilities (client)
  "Return CLIENT's server capabilities plist, if a server is running.
CLIENT here is expected to be a file path when called from the tool."
  (when (and client (stringp client) (file-exists-p client)
             (not (file-directory-p client)))
    (pai-eglot--with-file client
      (eglot--capabilities server))))

;;;; Low-level request/notify (thin, real wrappers over jsonrpc)

(defun pai-eglot--request (client method params &optional on-result on-error)
  "Send synchronous request METHOD with PARAMS via CLIENT's file server.
CLIENT must be a file path.  If ON-RESULT is given it is called with the
result and t is returned; otherwise the result is returned directly."
  (pai-eglot--with-file client
    (condition-case err
        (let ((res (jsonrpc-request server method params)))
          (if on-result (progn (funcall on-result res) t) res))
      (error
       (if on-error (progn (funcall on-error err) t)
         (signal (car err) (cdr err)))))))

(defun pai-eglot--notify (client method params)
  "Send notification METHOD with PARAMS via CLIENT's file server."
  (pai-eglot--with-file client
    (jsonrpc-notify server method params)))

;;;; Document sync (Eglot syncs automatically; these send explicit notifications)

(defun pai-eglot--did-open (client file _content language-id _version)
  "Send textDocument/didOpen for FILE via CLIENT's server."
  (ignore client language-id)
  (pai-eglot--with-file file
    ;; Visiting the buffer with a managed server already sent didOpen.
    server))

(defun pai-eglot--did-change (client file changes version)
  "Send textDocument/didChange for FILE with CHANGES at VERSION."
  (ignore client)
  (pai-eglot--with-file file
    (jsonrpc-notify server :textDocument/didChange
                    (list :textDocument (list :uri (eglot--path-to-uri file)
                                              :version (or version 0))
                          :contentChanges (vconcat changes)))))

(defun pai-eglot--did-save (client file _content)
  "Send textDocument/didSave for FILE."
  (ignore client)
  (pai-eglot--with-file file
    (jsonrpc-notify server :textDocument/didSave
                    (list :textDocument (eglot--TextDocumentIdentifier)))))

(defun pai-eglot--did-close (client file)
  "Send textDocument/didClose for FILE."
  (ignore client)
  (pai-eglot--with-file file
    (jsonrpc-notify server :textDocument/didClose
                    (list :textDocument (eglot--TextDocumentIdentifier)))))

;;;; High-level actions (synchronous, return parsed LSP results)

(defun pai-eglot--position-request (file position method &optional extra)
  "Run synchronous METHOD at POSITION in FILE, returning the parsed result.
EXTRA is appended to the TextDocumentPositionParams."
  (pai-eglot--with-file file
    (pai-eglot--goto position)
    (jsonrpc-request server method
                     (append (eglot--TextDocumentPositionParams) extra))))

(defun pai-eglot--diagnostics (_client &optional file timeout)
  "Return diagnostics for FILE as a list of plists.
Each entry is (:line L :severity S :message M :source SRC), L 1-based.
TIMEOUT bounds how long to wait for the server to publish (default 2s)."
  (unless file (error "pai-lsp: diagnostics requires a file"))
  (pai-eglot--with-file file
    (pai-eglot--pump server (or timeout 2.0))
    (cl-loop for d in eglot--diagnostics
             collect (list :line (line-number-at-pos (flymake-diagnostic-beg d))
                           :severity (pai-eglot--diag-severity
                                      (flymake-diagnostic-type d))
                           :message (flymake-diagnostic-text d)))))

(defun pai-eglot--diag-severity (type)
  "Map a flymake diagnostic TYPE symbol to an LSP-ish severity keyword."
  (pcase type
    ('eglot-error "error")
    ('eglot-warning "warning")
    ('eglot-note "note")
    (_ (format "%s" type))))

(defun pai-eglot--definition (client file position)
  "Return definition location(s) for FILE at POSITION."
  (ignore client)
  (pai-eglot--position-request file position :textDocument/definition))

(defun pai-eglot--references (client file position include-declaration)
  "Return reference location(s) for FILE at POSITION."
  (ignore client)
  (pai-eglot--position-request
   file position :textDocument/references
   (list :context (list :includeDeclaration (if include-declaration t :json-false)))))

(defun pai-eglot--hover (client file position)
  "Return hover information for FILE at POSITION."
  (ignore client)
  (pai-eglot--position-request file position :textDocument/hover))

(defun pai-eglot--type-definition (client file position)
  "Return type-definition location(s) for FILE at POSITION."
  (ignore client)
  (pai-eglot--position-request file position :textDocument/typeDefinition))

(defun pai-eglot--implementation (client file position)
  "Return implementation location(s) for FILE at POSITION."
  (ignore client)
  (pai-eglot--position-request file position :textDocument/implementation))

(defun pai-eglot--rename (client file position new-name)
  "Return the WorkspaceEdit renaming the symbol at POSITION to NEW-NAME."
  (ignore client)
  (pai-eglot--position-request file position :textDocument/rename
                               (list :newName new-name)))

(defun pai-eglot--code-actions (client file range context)
  "Return code actions for RANGE in FILE.
RANGE is an LSP Range; CONTEXT an optional CodeActionContext."
  (ignore client)
  (pai-eglot--with-file file
    (jsonrpc-request server :textDocument/codeAction
                     (list :textDocument (eglot--TextDocumentIdentifier)
                           :range range
                           :context (or context (list :diagnostics []))))))

(defun pai-eglot--rename-file (client old-uri new-uri)
  "Return the WorkspaceEdit for renaming OLD-URI to NEW-URI, if supported."
  (ignore client)
  (let ((old-path (if (string-prefix-p "file:" old-uri)
                      (eglot--uri-to-path old-uri) old-uri)))
    (pai-eglot--with-file old-path
      (if (eglot--server-capable :workspace :fileOperations :willRename)
          (jsonrpc-request server :workspace/willRenameFiles
                           (list :files (vector (list :oldUri (eglot--path-to-uri old-path)
                                                      :newUri (if (string-prefix-p "file:" new-uri)
                                                                  new-uri
                                                                (eglot--path-to-uri new-uri))))))
        (error "pai-lsp: server does not support willRenameFiles")))))

(defun pai-eglot--symbols (client &optional query file)
  "Return symbols.  With QUERY, workspace symbols; otherwise document symbols.
FILE selects the server (and, for document symbols, the document)."
  (ignore client)
  (unless file (error "pai-lsp: symbols requires a file"))
  (pai-eglot--with-file file
    (if (and query (not (string-empty-p query)))
        (jsonrpc-request server :workspace/symbol (list :query query))
      (jsonrpc-request server :textDocument/documentSymbol
                       (list :textDocument (eglot--TextDocumentIdentifier))))))

(defun pai-eglot--reload (client &optional file)
  "Reconnect the Eglot server for FILE (or CLIENT's project)."
  (let ((path (or file (and (stringp client) (file-exists-p client)
                            (not (file-directory-p client)) client))))
    (if path
        (pai-eglot--with-file path
          (eglot-reconnect server)
          t)
      (error "pai-lsp: reload requires a file"))))

(defun pai-eglot--status (client &optional file)
  "Return a status plist for the server serving FILE (or CLIENT)."
  (let ((path (or file (and (stringp client) (file-exists-p client)
                            (not (file-directory-p client)) client))))
    (unless path (error "pai-lsp: status requires a file"))
    (pai-eglot--with-file path
      (list :backend "eglot"
            :server (jsonrpc-name server)
            :project (when (eglot--project server)
                       (project-root (eglot--project server)))
            :live (process-live-p (jsonrpc--process server))
            :capabilities (pai-eglot--capability-summary
                           (eglot--capabilities server))))))

(defun pai-eglot--capability-summary (caps)
  "Return a list of provider names that CAPS advertises."
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

;;;; Register backend

(pai-lsp-register-backend
 (pai-lsp--backend-create
  :id 'eglot
  :name "Eglot (built-in)"
  :available-p #'pai-eglot--available-p
  :init #'pai-eglot--init
  :shutdown #'pai-eglot--shutdown
  :get-client #'pai-eglot--get-client
  :get-capabilities #'pai-eglot--get-capabilities
  :request #'pai-eglot--request
  :notify #'pai-eglot--notify
  :text-document-did-open #'pai-eglot--did-open
  :text-document-did-change #'pai-eglot--did-change
  :text-document-did-save #'pai-eglot--did-save
  :text-document-did-close #'pai-eglot--did-close
  :diagnostics #'pai-eglot--diagnostics
  :definition #'pai-eglot--definition
  :references #'pai-eglot--references
  :hover #'pai-eglot--hover
  :rename #'pai-eglot--rename
  :code-actions #'pai-eglot--code-actions
  :rename-file #'pai-eglot--rename-file
  :symbols #'pai-eglot--symbols
  :type-definition #'pai-eglot--type-definition
  :implementation #'pai-eglot--implementation
  :reload #'pai-eglot--reload
  :status #'pai-eglot--status))

(provide 'pai-eglot)
;;; pai-eglot.el ends here
