;;; pai-lsp.el --- LSP extension entry point -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the main extension file that loads the LSP abstraction and
;; backends, registers the `lsp' tool, and wires up settings UI.
;;
;; The `lsp' tool exposes: diagnostics, definition, references, hover,
;; rename, code_actions, rename_file, symbols, type_definition,
;; implementation, reload, status.  `file' is a path (absolute or relative
;; to the tool cwd); `line' is 1-based, `character' is 0-based (matching the
;; column LSP itself uses).

;;; Code:

(require 'pai)
(require 'pai-ext)
(require 'pai-lsp-core)
(require 'pai-lsp-mode)
(require 'pai-eglot)
(require 'pai-settings)
(require 'pai-settings-ui)

;; Load the appropriate backend based on settings
(pai-lsp-settings-changed)

;;;; Result formatting

(defun pai-lsp-ext--rel (path cwd)
  "Return PATH relative to CWD, or PATH itself if it can't be relativized."
  (condition-case nil (file-relative-name path cwd) (error path)))

(defun pai-lsp-ext--uri-path (uri)
  "Convert URI to a filesystem path."
  (if (fboundp 'eglot--uri-to-path) (eglot--uri-to-path uri) uri))

(defun pai-lsp-ext--locations (result cwd)
  "Normalize a Location/LocationLink RESULT (single, vector, or nil) to a list
of \"path:line:char\" strings relative to CWD."
  (let ((entries (cond ((null result) nil)
                       ((vectorp result) (append result nil))
                       ((listp result) (if (plist-get result :uri) (list result)
                                          (if (plist-get result :targetUri) (list result)
                                            result)))
                       (t (list result)))))
    (mapcar
     (lambda (loc)
       (let* ((uri (or (plist-get loc :uri) (plist-get loc :targetUri)))
              (range (or (plist-get loc :range) (plist-get loc :targetSelectionRange)
                        (plist-get loc :targetRange)))
              (start (plist-get range :start))
              (path (pai-lsp-ext--uri-path uri)))
         (format "%s:%d:%d" (pai-lsp-ext--rel path cwd)
                 (1+ (or (plist-get start :line) 0))
                 (or (plist-get start :character) 0))))
     entries)))

(defun pai-lsp-ext--format-locations (result cwd empty-msg)
  "Format a Location(s) RESULT as newline-separated text, or EMPTY-MSG."
  (let ((lines (pai-lsp-ext--locations result cwd)))
    (if lines (mapconcat #'identity lines "\n") empty-msg)))

(defun pai-lsp-ext--marked-string-text (m)
  "Extract plain text from an LSP MarkedString/MarkupContent M."
  (cond ((stringp m) m)
        ((and (listp m) (plist-get m :value)) (plist-get m :value))
        (t (format "%s" m))))

(defun pai-lsp-ext--format-hover (result)
  "Format a Hover RESULT as text, or a not-found message."
  (if (null result) "No hover information available."
    (let ((contents (plist-get result :contents)))
      (cond
       ((null contents) "No hover information available.")
       ((vectorp contents)
        (mapconcat #'pai-lsp-ext--marked-string-text (append contents nil) "\n\n"))
       (t (pai-lsp-ext--marked-string-text contents))))))

(defun pai-lsp-ext--format-workspace-edit (result cwd)
  "Format a WorkspaceEdit RESULT as a per-file edit-count summary."
  (if (null result) "No edit produced (rename not supported at this position?)."
    (let ((changes (plist-get result :changes))
          (doc-changes (plist-get result :documentChanges))
          (lines nil))
      (when changes
        (cl-loop for (uri . edits) in
                 (if (listp changes)
                     (cl-loop for (k v) on changes by #'cddr
                              collect (cons (substring (symbol-name k) 1) v))
                   nil)
                 do (push (format "%s: %d edit(s)"
                                 (pai-lsp-ext--rel (pai-lsp-ext--uri-path uri) cwd)
                                 (length edits))
                          lines)))
      (when doc-changes
        (dolist (dc (append doc-changes nil))
          (let ((td (plist-get dc :textDocument)))
            (if td
                (push (format "%s: %d edit(s)"
                              (pai-lsp-ext--rel
                               (pai-lsp-ext--uri-path (plist-get td :uri)) cwd)
                              (length (plist-get dc :edits)))
                      lines)
              (push (format "rename %s -> %s"
                            (pai-lsp-ext--rel (pai-lsp-ext--uri-path (plist-get dc :oldUri)) cwd)
                            (pai-lsp-ext--rel (pai-lsp-ext--uri-path (plist-get dc :newUri)) cwd))
                    lines)))))
      (if lines (mapconcat #'identity (nreverse lines) "\n") "No changes."))))

(defconst pai-lsp-ext--symbol-kinds
  ["" "File" "Module" "Namespace" "Package" "Class" "Method" "Property" "Field"
   "Constructor" "Enum" "Interface" "Function" "Variable" "Constant" "String"
   "Number" "Boolean" "Array" "Object" "Key" "Null" "EnumMember" "Struct"
   "Event" "Operator" "TypeParameter"]
  "SymbolKind names, indexed by the LSP integer code.")

(defun pai-lsp-ext--symbol-kind-name (kind)
  "Return the human-readable name for numeric SymbolKind KIND."
  (or (and (integerp kind) (>= kind 1) (< kind (length pai-lsp-ext--symbol-kinds))
          (aref pai-lsp-ext--symbol-kinds kind))
      (format "%s" kind)))

(defun pai-lsp-ext--format-symbols (result cwd default-file)
  "Format DocumentSymbol/SymbolInformation RESULT as \"name (kind) path:line\" lines.
DEFAULT-FILE is used for DocumentSymbol entries, which carry no location."
  (if (null result) "No symbols found."
    (let (lines)
      (cl-labels
          ((walk (sym file)
             (let* ((kind (pai-lsp-ext--symbol-kind-name (plist-get sym :kind)))
                    (name (plist-get sym :name))
                    (loc (plist-get sym :location))
                    (range (or (plist-get sym :range)
                              (plist-get sym :selectionRange)
                              (plist-get loc :range)))
                    (file (or (and loc (pai-lsp-ext--uri-path (plist-get loc :uri))) file))
                    (start (plist-get range :start)))
               (push (format "%s (%s) %s:%d" name kind
                             (pai-lsp-ext--rel file cwd)
                             (1+ (or (plist-get start :line) 0)))
                     lines)
               (dolist (child (append (plist-get sym :children) nil))
                 (walk child file)))))
        (dolist (sym (append result nil)) (walk sym default-file)))
      (mapconcat #'identity (nreverse lines) "\n"))))

(defun pai-lsp-ext--format-code-actions (result)
  "Format a CodeAction[] RESULT as \"title (kind)\" lines."
  (if (null result) "No code actions available."
    (mapconcat
     (lambda (a) (format "%s%s" (plist-get a :title)
                         (if (plist-get a :kind) (format " (%s)" (plist-get a :kind)) "")))
     (append result nil) "\n")))

(defun pai-lsp-ext--format-diagnostics (result cwd file)
  "Format a diagnostics RESULT list (from `pai-lsp-diagnostics') as text."
  (if (null result) "No diagnostics."
    (mapconcat
     (lambda (d) (format "%s:%d: %s: %s"
                         (pai-lsp-ext--rel file cwd)
                         (plist-get d :line) (plist-get d :severity)
                         (plist-get d :message)))
     result "\n")))

(defun pai-lsp-ext--format-status (result)
  "Format a status RESULT plist as text."
  (if (null result) "No server running."
    (format "backend: %s\nserver: %s\nproject: %s\nlive: %s\ncapabilities: %s"
            (or (plist-get result :backend) "?")
            (or (plist-get result :server) "?")
            (or (plist-get result :project) "?")
            (if (plist-get result :live) "yes" "no")
            (mapconcat #'identity (plist-get result :capabilities) ", "))))

;;;; Tool execute

(defun pai-lsp-ext--position (args)
  "Build an LSP position plist from ARGS' 1-based :line and 0-based :character."
  (list :line (max 0 (1- (or (plist-get args :line) 1)))
        :character (or (plist-get args :character) 0)))

(defun pai-lsp-ext--execute (args ctx _on-update on-done)
  "Execute the `lsp' tool for ARGS in CTX, finishing via ON-DONE."
  (if (not (plist-get (pai-settings-get :lsp) :enabled))
      (funcall on-done (pai-tool-error-result "LSP support is disabled (:lsp :enabled). Enable it in settings."))
    (let* ((action (or (plist-get args :action) "status"))
           (cwd (pai-tool-ctx-cwd ctx))
           (session (plist-get ctx :session))
           (file (and (plist-get args :file)
                      (pai-tool-resolve-path ctx (plist-get args :file))))
           (client (pai-lsp-get-client session (or (locate-dominating-file
                                                    (or file cwd) ".git")
                                                   cwd))))
    (condition-case err
        (funcall
         on-done
         (pai-tool-ok-result
          (pcase action
            ("diagnostics"
             (unless file (error "diagnostics requires \"file\""))
             (pai-lsp-ext--format-diagnostics
              (pai-lsp-diagnostics client file) cwd file))
            ("definition"
             (unless file (error "definition requires \"file\""))
             (pai-lsp-ext--format-locations
              (pai-lsp-definition client file (pai-lsp-ext--position args))
              cwd "No definition found."))
            ("type_definition"
             (unless file (error "type_definition requires \"file\""))
             (pai-lsp-ext--format-locations
              (pai-lsp-type-definition client file (pai-lsp-ext--position args))
              cwd "No type definition found."))
            ("implementation"
             (unless file (error "implementation requires \"file\""))
             (pai-lsp-ext--format-locations
              (pai-lsp-implementation client file (pai-lsp-ext--position args))
              cwd "No implementation found."))
            ("references"
             (unless file (error "references requires \"file\""))
             (pai-lsp-ext--format-locations
              (pai-lsp-references client file (pai-lsp-ext--position args)
                                  (if (plist-member args :include_declaration)
                                      (plist-get args :include_declaration) t))
              cwd "No references found."))
            ("hover"
             (unless file (error "hover requires \"file\""))
             (pai-lsp-ext--format-hover
              (pai-lsp-hover client file (pai-lsp-ext--position args))))
            ("rename"
             (unless file (error "rename requires \"file\""))
             (unless (plist-get args :new_name) (error "rename requires \"new_name\""))
             (pai-lsp-ext--format-workspace-edit
              (pai-lsp-rename client file (pai-lsp-ext--position args)
                             (plist-get args :new_name))
              cwd))
            ("code_actions"
             (unless file (error "code_actions requires \"file\""))
             (let ((pos (pai-lsp-ext--position args)))
               (pai-lsp-ext--format-code-actions
                (pai-lsp-code-actions client file (list :start pos :end pos) nil))))
            ("rename_file"
             (unless (plist-get args :new_file) (error "rename_file requires \"new_file\""))
             (unless file (error "rename_file requires \"file\""))
             (pai-lsp-ext--format-workspace-edit
              (pai-lsp-rename-file client file
                                   (pai-tool-resolve-path ctx (plist-get args :new_file)))
              cwd))
            ("symbols"
             (unless file (error "symbols requires \"file\" (used to select the server; add \"query\" for a workspace-wide search)"))
             (pai-lsp-ext--format-symbols
              (pai-lsp-symbols client (plist-get args :query) file) cwd file))
            ("reload"
             (if (pai-lsp-reload client file) "Server reloaded." "Reload failed."))
            ("status"
             (pai-lsp-ext--format-status (pai-lsp-status client file)))
            (_ (error "Unknown lsp action: %s" action)))))
      (error (funcall on-done (pai-tool-error-result
                               (format "lsp %s failed: %s" action
                                       (error-message-string err)))))))))

(pai-register-tool
 (list :name "lsp"
       :label "LSP"
       :description "Query a Language Server (via Eglot or lsp-mode) for code intelligence: diagnostics, definition, references, hover, rename, code_actions, rename_file, symbols, type_definition, implementation, reload, status. `line' is 1-based; `character' is 0-based."
       :prompt-snippet "lsp: language-server code intelligence"
       :parameters
       (pai-object-schema
        (list :action (pai-string-schema
                       "One of: diagnostics, definition, references, hover, rename, code_actions, rename_file, symbols, type_definition, implementation, reload, status."
                       :enum '("diagnostics" "definition" "references" "hover" "rename"
                               "code_actions" "rename_file" "symbols" "type_definition"
                               "implementation" "reload" "status"))
              :file (pai-string-schema "Path to the file to query (absolute or relative to cwd).")
              :line (pai-number-schema "1-based line number.")
              :character (pai-number-schema "0-based character offset on the line.")
              :new_name (pai-string-schema "New symbol name, for `rename'.")
              :new_file (pai-string-schema "New file path, for `rename_file'.")
              :include_declaration (pai-boolean-schema "Include the declaration in `references' (default true).")
              :query (pai-string-schema "Symbol search query for `symbols' (workspace search); omit for the current file's outline."))
        '("action"))
       :execute #'pai-lsp-ext--execute))

;;;; Extension registration

(pai-register-extension
 (lambda (pi)
   ;; Hook into session lifecycle
   (pai-ext-on pi 'session-start
               (lambda (_event ctx)
                 (when (plist-get (pai-settings-get :lsp) :enabled)
                   (pai-lsp-init (plist-get ctx :session)))))

   (pai-ext-on pi 'session-end
               (lambda (_event ctx)
                 (pai-lsp-shutdown (plist-get ctx :session))))))

;;;; Settings UI integration

(pai-settings-ui-register-section 'lsp "Language Server" 50)
(pai-settings-ui-register-subsection 'lsp 'backend "Backend" 10)
(pai-settings-ui-register-item
 'lsp 'backend
 :key :lsp-backend :type 'choice :label "Backend"
 :doc "LSP backend to use (requires restart)"
 :choices (lambda ()
            (when (fboundp 'pai-lsp-available-backends)
              (mapcar (lambda (b) (car b)) (pai-lsp-available-backends))))
 :get (lambda () (plist-get (pai-settings-get :lsp) :backend))
 :set (lambda (v)
        (let ((lsp (pai-settings-get :lsp)))
          (pai-settings-set :lsp (plist-put (copy-sequence lsp) :backend v) 'project))
        (when (fboundp 'pai-lsp-settings-changed)
          (pai-lsp-settings-changed))))
(pai-settings-ui-register-subsection 'lsp 'enabled "Enabled" 20)
(pai-settings-ui-register-item
 'lsp 'enabled
 :key :lsp-enabled :type 'boolean :label "Enable LSP"
 :doc "Enable Language Server Protocol support"
 :get (lambda () (plist-get (pai-settings-get :lsp) :enabled))
 :set (lambda (v)
        (let ((lsp (pai-settings-get :lsp)))
          (pai-settings-set :lsp (plist-put (copy-sequence lsp) :enabled v) 'project))))
(pai-settings-ui-register-subsection 'lsp 'servers "Servers" 30)
(pai-settings-ui-register-item
 'lsp 'servers
 :key :lsp-servers :type 'custom :label "Configured servers"
 :doc "Additional server configurations (JSON)"
 :get (lambda () (plist-get (pai-settings-get :lsp) :servers))
 :set (lambda (v)
        (let ((lsp (pai-settings-get :lsp)))
          (pai-settings-set :lsp (plist-put (copy-sequence lsp) :servers v) 'project)))
 :render (lambda (_refresh)
           (let ((val (plist-get (pai-settings-get :lsp) :servers)))
             (vui-text (format "Server configs: %s"
                               (if (listp val)
                                   (format "%d entries" (length val))
                                 "none"))))))

(provide 'pai-lsp)
;;; pai-lsp.el ends here
