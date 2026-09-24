;;; pai-mcp-sources.el --- Declarative MCP sources and runtime registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Files are parsed, never evaluated.  Discovery follows pi-mcp-adapter's
;; config.ts, package-mcp-loader.ts and plugin loaders.  Reading a registry or
;; status snapshot neither starts a process nor creates client state.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'pai-mcp-config)

(defvar pai-mcp--runtime-servers nil "Runtime server registrations, newest first.")
(defvar pai-mcp--provenance nil "Provenance for the last configuration load.")
(defvar pai-mcp-source-change-hook nil
  "Hook called with NAME and ACTION after runtime registration changes.")
(defconst pai-mcp-runtime-register-event "pi-mcp-adapter:runtime-register:v1")
(defconst pai-mcp-runtime-snapshot-event "pi-mcp-adapter:runtime-snapshot:v1")
(defconst pai-mcp-status-event "pi-mcp-adapter:status:v1")

(defun pai-mcp--jsonc (text)
  "Remove JSON comments and trailing commas from TEXT, preserving strings."
  (with-temp-buffer
    (insert text)
    (dotimes (_pass 2)
      (goto-char (point-min))
      (while (not (eobp))
      (cond
       ((eq (char-after) ?\") (forward-char) (while (and (not (eobp)) (not (eq (char-after) ?\")))
                                              (if (eq (char-after) ?\\) (forward-char (min 2 (- (point-max) (point)))) (forward-char)))
        (unless (eobp) (forward-char)))
       ((looking-at "//") (delete-region (point) (line-end-position)))
       ((looking-at "/\\*")
        (let ((start (point)))
          (unless (search-forward "*/" nil t) (error "Unterminated JSON comment"))
          (delete-region start (point)) (insert " ")))
       ((eq (char-after) ?,)
        (let ((start (point)))
          (forward-char) (skip-chars-forward " \t\r\n")
          (when (memq (char-after) '(?\] ?\})) (delete-region start (1+ start)))))
       (t (forward-char)))))
    (buffer-string)))

(defun pai-mcp--object-p (value)
  "Whether VALUE is a keyword property list, including the empty object."
  (and (listp value) (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr always (keywordp key))))

(defun pai-mcp--overlay (base next)
  "Return a shallow property overlay of NEXT onto BASE."
  (let ((out (copy-sequence base)))
    (while next (setq out (plist-put out (car next) (cadr next)) next (cddr next)))
    out))

(defun pai-mcp--without (object keys)
  "Return OBJECT without KEYS, without mutating OBJECT."
  (cl-loop for (key value) on object by #'cddr unless (memq key keys) append (list key value)))

(defun pai-mcp--merge-definition (base next)
  "Merge NEXT over BASE without leaking credentials across transports or URLs."
  (let ((safe base))
    (cond
     ((stringp (plist-get next :command))
      (setq safe (pai-mcp--without safe '(:url :headers :requestHeadersCommand :caFile :auth :bearerToken :bearerTokenEnv :bearerTokenStore :oauth :httpTransport :socket))))
     ((stringp (plist-get next :url))
      (setq safe (pai-mcp--without safe '(:command :args :env :cwd :pluginDataDir :literalEnv :inheritEnv :socket))))
     ((stringp (plist-get next :socket))
      (setq safe (pai-mcp--without safe '(:command :args :env :cwd :pluginDataDir :literalEnv :inheritEnv :url :headers :requestHeadersCommand :caFile :auth :bearerToken :bearerTokenEnv :bearerTokenStore :oauth :httpTransport)))))
    (when (and (stringp (plist-get next :url)) (not (equal (plist-get next :url) (plist-get base :url))))
      (setq safe (pai-mcp--without safe '(:headers :bearerToken :bearerTokenEnv :bearerTokenStore :requestHeadersCommand :caFile)))
      (unless (eq (plist-get safe :oauth) :false) (setq safe (pai-mcp--without safe '(:oauth)))))
    (when (and (plist-member next :env) (plist-get base :literalEnv) (not (plist-member next :literalEnv)))
      (setq safe (pai-mcp--without safe '(:literalEnv))))
    (pai-mcp--overlay safe next)))

(defun pai-mcp--merge-servers (base next &optional first-wins provenance)
  "Merge server alists BASE and NEXT.  FIRST-WINS keeps duplicates in BASE.
PROVENANCE is recorded for each accepted NEXT definition."
  (let ((out (copy-sequence base)))
    (dolist (pair next out)
      (when (and (stringp (car pair)) (pai-mcp--object-p (cdr pair))
                 (not (and first-wins (assoc (car pair) out))))
        (let ((entry (cons (car pair) (pai-mcp--merge-definition (cdr (assoc (car pair) out)) (cdr pair)))))
          (setq out (append (assoc-delete-all (car pair) out) (list entry))))
        (when provenance
          (setf (alist-get (car pair) pai-mcp--provenance nil nil #'equal) provenance))))))

(defun pai-mcp--identity (path)
  "Canonical PATH, with a lexical fallback for absent files."
  (condition-case nil (file-truename path) (file-error (expand-file-name path))))

(defun pai-mcp--contained-p (root path)
  "Whether canonical PATH is ROOT or contained within it."
  (let ((root (directory-file-name (pai-mcp--identity root))) (path (pai-mcp--identity path)))
    (or (equal root (directory-file-name path)) (string-prefix-p (file-name-as-directory root) path))))

(defun pai-mcp--source-files (dir)
  "Return standard sources for DIR, including explicitly trusted ancestors."
  (let* ((global (list (expand-file-name "~/.config/mcp/mcp.json")
                       (expand-file-name "~/.agents/mcp.json")
                       (expand-file-name "~/.agents/mcp/mcp.json")
                       (expand-file-name "mcp.json" pai-directory)))
         (project (list (expand-file-name ".mcp.json" dir) (expand-file-name ".pi/mcp.json" dir)))
         (cwd (directory-file-name (pai-mcp--identity dir))) roots valid ancestors seen)
    (dolist (file global)
      (let ((settings (plist-get (pai-mcp--read-json-file file) :settings)))
        (when (plist-member settings :ancestorConfigRoots) (setq roots (plist-get settings :ancestorConfigRoots)))))
    (dolist (root (and (listp roots) roots))
      (when (and (stringp root) (or (file-name-absolute-p root) (string-prefix-p "~/" root)))
        (setq root (pai-mcp--identity (expand-file-name root)))
        (when (and (file-directory-p root) (pai-mcp--contained-p (expand-file-name "~") root) (pai-mcp--contained-p root cwd))
          (push (directory-file-name root) valid))))
    (let ((root (car (sort valid (lambda (a b) (> (length a) (length b)))))))
      (when root
        (let ((parent (directory-file-name (file-name-directory cwd))) dirs)
          (while (and parent (not (equal cwd root)) (pai-mcp--contained-p root parent))
            (push parent dirs)
            (setq parent (unless (equal root parent)
                           (directory-file-name (file-name-directory parent)))))
          (setq seen (mapcar #'pai-mcp--identity (append global project)))
          (dolist (ancestor dirs)
            (dolist (rel '(".mcp.json" ".pi/mcp.json"))
              (let* ((file (expand-file-name rel ancestor)) (identity (pai-mcp--identity file)))
                (when (and (file-exists-p file) (not (member identity seen)))
                  (setq ancestors (append (cl-remove identity ancestors :key #'pai-mcp--identity :test #'equal) (list file))))))))))
    (delete-dups (append global ancestors project))))

(defun pai-mcp--host-paths (kind dir)
  "Return upstream-ordered import candidates for KIND and project DIR."
  (pcase kind
    ("cursor" (list (expand-file-name "~/.cursor/mcp.json")))
    ("claude-code" (mapcar #'expand-file-name '("~/.claude/mcp.json" "~/.claude.json" "~/.claude/claude_desktop_config.json")))
    ("claude-desktop" (list (expand-file-name "~/Library/Application Support/Claude/claude_desktop_config.json")))
    ("codex" (mapcar #'expand-file-name '("~/.codex/config.toml" "~/.codex/config.json")))
    ("windsurf" (list (expand-file-name "~/.windsurf/mcp.json")))
    ("vscode" (list (expand-file-name ".vscode/mcp.json" dir)))
    ("opencode"
     (let* ((root (locate-dominating-file dir ".git"))
            (current (file-name-as-directory (expand-file-name dir)))
            (project (expand-file-name "opencode.json" current)))
       (when root
         (while (and (not (file-exists-p project)) (not (equal current root)))
           (setq current (file-name-directory (directory-file-name current)) project (expand-file-name "opencode.json" current))))
       (list (expand-file-name "~/.config/opencode/opencode.json") project)))))

(defconst pai-mcp--host-kinds '("cursor" "claude-code" "claude-desktop" "codex" "opencode" "windsurf" "vscode"))

;; A focused, non-evaluating TOML reader.  Unsupported syntax is rejected rather
;; than guessed.  Scalar values, quoted/bare dotted keys, arrays, inline tables,
;; comments, and ordinary tables cover Codex MCP declarations.
(defun pai-mcp--toml-space ()
  "Skip whitespace and TOML comments in the current buffer."
  (while (progn (skip-chars-forward " \t\r\n") (when (eq (char-after) ?#) (forward-line 1) t))))

(defun pai-mcp--toml-string ()
  "Read a TOML single-line string at point."
  (let ((quote (char-after)) (start (point)))
    (forward-char)
    (when (and (eq (char-after) quote) (eq (char-after (1+ (point))) quote)) (error "Multiline TOML strings are unsupported"))
    (while (and (not (eobp)) (not (eq (char-after) quote)))
      (when (memq (char-after) '(?\n ?\r)) (error "Newline in TOML string"))
      (if (and (eq quote ?\") (eq (char-after) ?\\)) (forward-char 2) (forward-char)))
    (unless (eq (char-after) quote) (error "Unterminated TOML string"))
    (forward-char)
    (if (eq quote ?\") (pai-json-decode (buffer-substring-no-properties start (point)))
      (buffer-substring-no-properties (1+ start) (1- (point))))))

(defun pai-mcp--toml-key ()
  "Read a possibly dotted TOML key, returning keyword components."
  (let (keys more)
    (setq more t)
    (while more
      (skip-chars-forward " \t")
      (push (intern (concat ":" (cond ((memq (char-after) '(?\" ?')) (pai-mcp--toml-string))
                                     ((looking-at "[A-Za-z0-9_-]+") (prog1 (match-string 0) (goto-char (match-end 0))))
                                     (t (error "Invalid TOML key"))))) keys)
      (skip-chars-forward " \t")
      (setq more (eq (char-after) ?.)) (when more (forward-char)))
    (nreverse keys)))

(defun pai-mcp--toml-put (object keys value)
  "Set KEYS to VALUE in OBJECT, rejecting duplicate or scalar parents."
  (let ((key (car keys)))
    (when (and (cdr keys) (plist-member object key) (not (pai-mcp--object-p (plist-get object key)))) (error "Scalar TOML parent"))
    (if (cdr keys)
        (plist-put object key (pai-mcp--toml-put (plist-get object key) (cdr keys) value))
      (when (plist-member object key) (error "Duplicate TOML key %s" key))
      (plist-put object key value))))

(defun pai-mcp--toml-value ()
  "Read a supported TOML value at point."
  (pai-mcp--toml-space)
  (cond
   ((memq (char-after) '(?\" ?')) (pai-mcp--toml-string))
   ((eq (char-after) ?\[)
    (forward-char) (pai-mcp--toml-space)
    (let (values)
      (while (not (eq (char-after) ?\]))
        (push (pai-mcp--toml-value) values) (pai-mcp--toml-space)
        (cond ((eq (char-after) ?,) (forward-char) (pai-mcp--toml-space))
              ((not (eq (char-after) ?\])) (error "Expected TOML array comma"))))
      (forward-char) (nreverse values)))
   ((eq (char-after) ?\{)
    (forward-char) (skip-chars-forward " \t")
    (let (object)
      (while (not (eq (char-after) ?\}))
        (let ((keys (pai-mcp--toml-key)))
          (unless (eq (char-after) ?=) (error "Expected TOML equals"))
          (forward-char) (setq object (pai-mcp--toml-put object keys (pai-mcp--toml-value))))
        (skip-chars-forward " \t")
        (cond ((eq (char-after) ?,) (forward-char) (skip-chars-forward " \t")
               (when (eq (char-after) ?\}) (error "Trailing inline-table comma")))
              ((not (eq (char-after) ?\})) (error "Expected inline-table comma"))))
      (forward-char) object))
   ((looking-at "\\(?:true\\|false\\)\\_>") (prog1 (if (equal (match-string 0) "true") t :false) (goto-char (match-end 0))))
   ((looking-at "[-+]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][-+]?[0-9]+\\)?")
    (prog1 (string-to-number (match-string 0)) (goto-char (match-end 0))))
   (t (error "Unsupported TOML value at %d" (point)))))

(defun pai-mcp--read-toml-file (file)
  "Read unambiguous supported TOML from FILE, signaling malformed syntax."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (object section sections assignments)
      (pai-mcp--toml-space)
      (while (not (eobp))
        (if (eq (char-after) ?\[)
            (progn (forward-char)
                   (when (eq (char-after) ?\[) (error "TOML arrays of tables are unsupported"))
                   (setq section (pai-mcp--toml-key))
                   (unless (eq (char-after) ?\]) (error "Invalid TOML table"))
                   (when (member section sections) (error "Duplicate TOML table"))
                   (when (cl-some (lambda (keys)
                                    (and (<= (length keys) (length section))
                                         (equal keys (cl-subseq section 0 (length keys))))) assignments)
                     (error "TOML table redefines an assigned value"))
                   (push section sections) (forward-char))
          (let ((keys (pai-mcp--toml-key)))
            (unless (eq (char-after) ?=) (error "Expected TOML equals"))
            (forward-char)
            (let ((full-key (append section keys)))
              (setq object (pai-mcp--toml-put object full-key (pai-mcp--toml-value)))
              (push full-key assignments))))
        (skip-chars-forward " \t\r")
        (unless (or (eobp) (memq (char-after) '(?\n ?#))) (error "Unexpected TOML trailing text"))
        (pai-mcp--toml-space))
      object)))

(defun pai-mcp--extract-host (json kind)
  "Translate host JSON for KIND to the adapter's server alist."
  (let ((raw (pcase kind
               ("opencode" (plist-get json :mcp))
               ("codex" (or (plist-get json :mcp_servers) (plist-get json :mcpServers)))
               (_ (or (plist-get json :mcpServers) (plist-get json :mcp-servers) (and (equal kind "vscode") (plist-get json :servers)))))) out)
    (when (pai-mcp--object-p raw)
      (dolist (pair (pai-mcp--plist-to-alist raw))
        (let ((def (cdr pair)))
          (when (pai-mcp--object-p def)
            (pcase kind
              ("opencode"
               (setq def
                     (unless (eq (plist-get def :enabled) :false)
                       (pcase (plist-get def :type)
                         ("local" (let ((cmd (plist-get def :command)))
                                    (when (and (consp cmd) (cl-every #'stringp cmd))
                                      (list :command (car cmd) :args (cdr cmd) :env (plist-get def :environment) :cwd (plist-get def :cwd)))))
                         ("remote" (when (stringp (plist-get def :url))
                                     (append (list :url (plist-get def :url) :headers (plist-get def :headers))
                                             (when (plist-member def :oauth)
                                               (append (unless (eq (plist-get def :oauth) :false) '(:auth "oauth"))
                                                       (list :oauth (plist-get def :oauth)))))))))))
              ("codex"
               (let ((headers (pai-mcp--overlay (plist-get def :headers) (plist-get def :http_headers)))
                     (env (plist-get def :env_http_headers)) (token (plist-get def :bearer_token_env_var)))
                 (while env
                   (unless (plist-member headers (car env)) (setq headers (plist-put headers (car env) (concat "$env:" (cadr env)))))
                   (setq env (cddr env)))
                 (setq def (pai-mcp--without def '(:bearer_token_env_var :http_headers :env_http_headers)))
                 (when headers (setq def (plist-put def :headers headers)))
                 (when token (setq def (plist-put def :bearerTokenEnv token))
                       (unless (plist-member def :auth) (setq def (plist-put def :auth "bearer")))))))
            (when def (push (cons (car pair) def) out))))))
    (nreverse out)))

(defun pai-mcp--opencode-merge (base next)
  "Merge OpenCode NEXT over BASE while preserving partial host overrides."
  (let ((servers (plist-get base :mcp)))
    (dolist (pair (pai-mcp--plist-to-alist (plist-get next :mcp)))
      (let* ((key (intern (concat ":" (car pair)))) (old (plist-get servers key)) (new (cdr pair)))
        (when (and (plist-get new :type) (not (equal (plist-get new :type) (plist-get old :type))))
          (setq old (pai-mcp--without old '(:command :environment :cwd :url :headers :oauth))))
        (when (and (plist-get new :url) (not (equal (plist-get old :url) (plist-get new :url))))
          (setq old (pai-mcp--without old '(:headers :oauth))))
        (when (and (plist-member new :command) (not (equal (plist-get old :command) (plist-get new :command))))
          (setq old (pai-mcp--without old '(:environment :cwd))))
        (let ((merged (pai-mcp--overlay old new)))
          (dolist (field '(:environment :headers :oauth))
            (when (and (pai-mcp--object-p (plist-get old field)) (pai-mcp--object-p (plist-get new field)) (plist-member new field))
              (setq merged (plist-put merged field (pai-mcp--overlay (plist-get old field) (plist-get new field))))))
          (setq servers (plist-put servers key merged)))))
    (list :mcp servers)))

(defun pai-mcp--load-host (kind dir)
  "Read KIND's first valid source, or merged global/project OpenCode sources."
  (let (loaded path)
    (dolist (file (pai-mcp--host-paths kind dir))
      (when (and (file-readable-p file) (or (null loaded) (equal kind "opencode")))
        (condition-case err
            (let ((json (if (string-suffix-p ".toml" file) (pai-mcp--read-toml-file file) (pai-mcp--read-json-file file))))
              (when json (setq loaded (if (equal kind "opencode") (pai-mcp--opencode-merge loaded json) json) path file)))
          (error (message "pai-mcp: invalid %s import: %s" file (error-message-string err))))))
    (when loaded (list :path path :servers (pai-mcp--extract-host loaded kind)))))

(defun pai-mcp-discover-host-configs (&optional dir)
  "Return detected host config summaries without enabling or connecting them."
  (let* ((dir (or dir default-directory))
         (active (equal (plist-get (pai-mcp-settings dir) :hostConfigDiscovery) "on")) out)
    (dolist (kind pai-mcp--host-kinds (nreverse out))
      (let ((loaded (pai-mcp--load-host kind dir)))
        (when loaded
          (push (list :kind kind :path (plist-get loaded :path)
                      :serverCount (length (plist-get loaded :servers))
                      :active (if active t :false)) out))))))

(defun pai-mcp-add-imports (kinds &optional dir global)
  "Persist missing import KINDS, not host credentials or server definitions.
Write project .pi/mcp.json under DIR, or the user override when GLOBAL is set.
Return the written path.  Existing malformed files are never overwritten."
  (unless (and (listp kinds) (cl-every (lambda (kind) (member kind pai-mcp--host-kinds)) kinds))
    (error "Unknown MCP import kind"))
  (let* ((file (expand-file-name (if global "mcp.json" ".pi/mcp.json")
                                 (if global pai-directory (or dir default-directory))))
         (json (if (file-exists-p file)
                   (with-temp-buffer (insert-file-contents file)
                                     (pai-json-decode (pai-mcp--jsonc (buffer-string))))
                 nil)))
    (unless (pai-mcp--object-p json) (error "MCP configuration is not an object"))
    (setq json (plist-put json :imports (delete-dups (append (plist-get json :imports) kinds))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert (pai-json-encode json) "\n"))
    file))

(defun pai-mcp--namespace (name &optional fallback)
  "Normalize NAME for package/plugin server namespaces."
  (let ((s (replace-regexp-in-string "\\`[_-]+\\|[_-]+\\'" "" (replace-regexp-in-string "[^A-Za-z0-9_-]+" "_" name))))
    (if (string-empty-p s) (or fallback "server") s)))

(defun pai-mcp--declared-file (root relative)
  "Return a regular declared file RELATIVE strictly contained in ROOT."
  (let ((file (expand-file-name relative root)))
    (when (and (not (file-name-absolute-p relative)) (file-regular-p file) (pai-mcp--contained-p root file)) file)))

(defun pai-mcp--package-root (source base)
  "Resolve declared package SOURCE under BASE without installing anything."
  (cond
   ((string-prefix-p "npm:" source)
    (when (string-match "\\`npm:\\(@[^/@]+/[^@]+\\|[^@]+\\)" source)
      (let* ((root (expand-file-name "npm/node_modules" base)) (path (expand-file-name (match-string 1 source) root)))
        (when (pai-mcp--contained-p root path) path))))
   ((or (string-prefix-p "git:" source) (string-match-p "\\`\\(?:https?://\\|ssh://\\|git@\\)" source))
    (let* ((value (string-remove-prefix "git:" source))
           (value (replace-regexp-in-string "\\`ssh://git@\\|\\`[a-z]+://" "" value))
           (value (replace-regexp-in-string "\\`git@\\([^:]+\\):" "\\1/" value))
           (value (replace-regexp-in-string "@[^/]+\\'" "" value))
           (value (string-remove-suffix ".git" value))
           (root (expand-file-name "git" base)) (path (expand-file-name value root)))
      (when (pai-mcp--contained-p root path) path)))
   (t (expand-file-name source base))))

(defun pai-mcp--package-servers (dir)
  "Load only package.json pi.mcp declarations from configured installed packages."
  (let (servers seen)
    (dolist (base (list (expand-file-name ".pi" dir) pai-directory))
      (dolist (entry (plist-get (pai-mcp--read-json-file (expand-file-name "settings.json" base)) :packages))
        (let* ((source (if (stringp entry) entry (plist-get entry :source)))
               (root (and (stringp source) (pai-mcp--package-root source base))))
          (when (and root (not (member (pai-mcp--identity root) seen)))
            (push (pai-mcp--identity root) seen)
            (let* ((manifest (pai-mcp--read-json-file (expand-file-name "package.json" root)))
                   (name (plist-get manifest :name)) (paths (plist-get (plist-get manifest :pi) :mcp)))
              (when (stringp paths) (setq paths (list paths)))
              (when (and (stringp name) (listp paths) (cl-every #'stringp paths))
                (dolist (relative paths)
                  (let* ((file (pai-mcp--declared-file root relative))
                         (raw (plist-get (pai-mcp--read-json-file file) :mcpServers))
                         (defs (when (pai-mcp--object-p raw)
                                 (mapcar (lambda (pair) (cons (concat (pai-mcp--namespace name "package") "__" (pai-mcp--namespace (car pair))) (cdr pair))) (pai-mcp--plist-to-alist raw)))))
                    (setq servers (pai-mcp--merge-servers servers defs t (list :kind "package" :path file :package name)))))))))))
    servers))

(defun pai-mcp--replace-placeholders (value replacements)
  "Return JSON VALUE with literal string REPLACEMENTS applied recursively."
  (cond ((stringp value) (dolist (pair replacements value) (setq value (string-replace (car pair) (cdr pair) value))))
        ((functionp value) value)
        ((consp value) (mapcar (lambda (item) (pai-mcp--replace-placeholders item replacements)) value))
        ((vectorp value) (vconcat (mapcar (lambda (item) (pai-mcp--replace-placeholders item replacements)) value)))
        (t value)))

(defun pai-mcp--string-map-p (value)
  "Whether VALUE is an object containing only string values."
  (and (pai-mcp--object-p value) (cl-loop for (_key val) on value by #'cddr always (stringp val))))

(defun pai-mcp--agent-plugin-definition (raw root name)
  "Translate a strictly declarative Agent Plugin server RAW in ROOT named NAME."
  (let* ((type (plist-get raw :type))
         (data (expand-file-name (concat "agent-plugin-data/" name) pai-directory))
         (replace (list (cons "${PLUGIN_ROOT}" root) (cons "${PLUGIN_DATA}" data))))
    (cond
     ((equal type "stdio")
      (let ((command (plist-get raw :command)) (args (plist-get raw :args))
            (env (plist-get raw :env)) (cwd (or (plist-get raw :cwd) "${PLUGIN_ROOT}")))
        (unless (and (cl-loop for (key _v) on raw by #'cddr always (memq key '(:type :command :args :env :cwd)))
                     (stringp command) (not (string-empty-p command))
                     (listp args) (cl-every #'stringp args) (pai-mcp--string-map-p env)
                     (not (plist-member env :PLUGIN_ROOT)) (not (plist-member env :PLUGIN_DATA)) (stringp cwd))
          (error "Invalid Agent Plugin stdio declaration"))
        (if (string-prefix-p "./" command)
            (setq command (or (pai-mcp--declared-file root command) (error "Plugin command escapes its root")))
          (when (string-match-p "[/\\\\]\\|\\${PLUGIN_" command) (error "Plugin command must be bare or ./relative")))
        (let* ((expanded (pai-mcp--replace-placeholders cwd replace))
               (base (cond ((or (string-prefix-p "./" cwd) (equal cwd "${PLUGIN_ROOT}") (string-prefix-p "${PLUGIN_ROOT}/" cwd)) root)
                           ((or (equal cwd "${PLUGIN_DATA}") (string-prefix-p "${PLUGIN_DATA}/" cwd)) data)
                           (t (error "Unsupported plugin cwd"))))
               (resolved (expand-file-name expanded root)))
          (unless (pai-mcp--contained-p base resolved) (error "Plugin cwd escapes its root"))
          (list :command command :args (pai-mcp--replace-placeholders args replace)
                :env (pai-mcp--overlay (pai-mcp--replace-placeholders env replace) (list :PLUGIN_ROOT root :PLUGIN_DATA data))
                :cwd resolved :pluginDataDir data :literalEnv t))))
     ((member type '("streamable-http" "sse"))
      (let* ((url (plist-get raw :url)) (parsed (and (stringp url) (url-generic-parse-url url))) (headers (plist-get raw :headers)) seen)
        (unless (and (cl-loop for (key _v) on raw by #'cddr always (memq key '(:type :url :headers)))
                     parsed (member (url-type parsed) '("https" "http")) (url-host parsed)
                     (not (url-user parsed)) (not (url-password parsed)) (not (url-target parsed))
                     (not (string-match-p "\\${\\|\\$env:\\|{env:" url))
                     (or (equal (url-type parsed) "https") (member (url-host parsed) '("localhost" "::1" "[::1]")) (string-match-p "\\`127\\." (url-host parsed)))
                     (pai-mcp--string-map-p headers)) (error "Invalid Agent Plugin HTTP declaration"))
        (dolist (pair (pai-mcp--plist-to-alist headers))
          (let ((key (downcase (car pair))))
            (when (or (member key seen) (string-match-p "[\r\n]" (cdr pair)) (not (string-match-p "\\`[!#$%&'*+.^_`|~0-9A-Za-z-]+\\'" key))) (error "Invalid plugin header"))
            (push key seen)))
        (list :url url :headers headers :httpTransport type)))
     (t (error "Unsupported Agent Plugin transport")))))

(defun pai-mcp--manifest-fields-valid-p (manifest &optional claude)
  "Validate common declarative MANIFEST field types, with CLAUDE extensions."
  (and
   (cl-every (lambda (key) (or (not (plist-member manifest key)) (stringp (plist-get manifest key))))
             (append '(:version :description :homepage :repository :license)
                     (when claude '(:$schema :displayName))))
   (or (not (plist-member manifest :keywords))
       (let ((words (plist-get manifest :keywords))) (and (listp words) (cl-every #'stringp words))))
   (or (not (plist-member manifest :author))
       (let ((author (plist-get manifest :author)))
         (and (pai-mcp--string-map-p author)
              (if claude (stringp (plist-get author :name))
                (cl-loop for (key _v) on author by #'cddr always (memq key '(:name :email :url)))))))
   (or (not claude)
       (and (cl-every (lambda (key)
                        (or (not (plist-member manifest key))
                            (let ((value (plist-get manifest key)))
                              (or (stringp value) (and (listp value) (cl-every #'stringp value))))))
                      '(:commands :agents :skills :hooks :mcpServers :lspServers :outputStyles))
            (or (not (plist-member manifest :defaultEnabled)) (memq (plist-get manifest :defaultEnabled) '(t :false)))
            (or (not (plist-member manifest :metadata)) (pai-mcp--object-p (plist-get manifest :metadata)))))))

(defun pai-mcp--agent-plugin-servers (paths dir)
  "Load declared agent plugin PATHS relative to DIR; first normalized name wins."
  (let (servers)
    (dolist (path paths)
      (when (stringp path)
        (let* ((root (pai-mcp--identity (expand-file-name path dir)))
               (file (pai-mcp--declared-file root "plugin.json"))
               (manifest (pai-mcp--read-json-file file)) (name (plist-get manifest :name))
               (mcp (pai-mcp--declared-file root "mcp.json"))
               (json (pai-mcp--read-json-file mcp)))
          (when (and (equal (plist-get manifest :$schema) "https://agent-plugins.org/schemas/1.0.0/plugin.schema.json")
                     (stringp name) (<= 1 (length name) 64) (string-match-p "\\`[a-z0-9]\\(?:[a-z0-9.-]*[a-z0-9]\\)?\\'" name)
                     (not (string-match-p "--\\|\\.\\." name))
                     (pai-mcp--manifest-fields-valid-p manifest)
                     (equal (plist-get json :$schema) "https://agent-plugins.org/schemas/1.0.0/mcp.schema.json")
                     (cl-loop for (key _v) on json by #'cddr always (memq key '(:$schema :mcpServers))))
            (dolist (pair (pai-mcp--plist-to-alist (plist-get json :mcpServers)))
              (condition-case err
                  (let ((def (pai-mcp--agent-plugin-definition (cdr pair) root name))
                        (normalized (concat (pai-mcp--namespace name "plugin") "__" (pai-mcp--namespace (car pair)))))
                    (setq servers (pai-mcp--merge-servers servers (list (cons normalized def)) t (list :kind "agent-plugin" :path mcp :plugin name))))
                (error (message "pai-mcp: skipping Agent Plugin %s/%s: %s" name (car pair) (error-message-string err)))))))))
    servers))

(defun pai-mcp--claude-plugin-servers (plugins dir)
  "Load opt-in Claude plugin .mcp.json bundles without running plugin hooks."
  (let (servers roots namespaces)
    (dolist (plugin plugins)
      (when (and (stringp (plist-get plugin :path)) (eq (plist-get plugin :mcp) t))
        (let* ((root (pai-mcp--identity (expand-file-name (plist-get plugin :path) dir)))
               (file (pai-mcp--declared-file root ".mcp.json"))
               (manifest-path (expand-file-name ".claude-plugin/plugin.json" root))
               (manifest-file (pai-mcp--declared-file root ".claude-plugin/plugin.json"))
               (manifest (pai-mcp--read-json-file manifest-file)) (name (plist-get manifest :name)))
          (when (and file (not (member root roots))
                     (or (not (file-exists-p manifest-path))
                         (and manifest-file (stringp name) (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" name)
                              (pai-mcp--manifest-fields-valid-p manifest t))))
            (push root roots)
            (dolist (pair (pai-mcp--plist-to-alist (plist-get (pai-mcp--read-json-file file) :mcpServers)))
              (let* ((raw (cdr pair)) (namespace (pai-mcp--namespace (car pair))))
                (when (and (pai-mcp--object-p raw) (not (member namespace namespaces))
                           (= 1 (cl-count-if (lambda (key) (let ((v (plist-get raw key))) (and (stringp v) (not (string-empty-p (string-trim v)))))) '(:command :url :socket))))
                  (push namespace namespaces)
                  (let ((def (pai-mcp--replace-placeholders raw (list (cons "${CLAUDE_PLUGIN_ROOT}" root)))))
                    (when (plist-get def :command) (setq def (plist-put def :env (pai-mcp--overlay (plist-get def :env) (list :CLAUDE_PLUGIN_ROOT root)))))
                    (setq servers (pai-mcp--merge-servers servers (list (cons (car pair) def)) t (list :kind "claude-plugin" :path file :plugin (or name (file-name-nondirectory root)))))))))))))
    servers))

(defun pai-mcp--sources-load (dir)
  "Merge declarative sources and runtime overlays for DIR, recording provenance."
  (let* ((files (pai-mcp--source-files dir)) (settings (pai-mcp-settings dir))
         (global (expand-file-name "mcp.json" pai-directory)) configs plugins servers)
    (setq pai-mcp--provenance nil)
    (dolist (file files)
      (let ((json (pai-mcp--read-json-file file)))
        (push (cons file json) configs)
        (when (plist-member json :claudePlugins) (setq plugins (plist-get json :claudePlugins)))))
    (setq configs (nreverse configs))
    (setq servers (pai-mcp--claude-plugin-servers plugins dir))
    (let* ((packages (pai-mcp--package-servers dir))
           (agents (pai-mcp--agent-plugin-servers (plist-get settings :agentPluginPaths) dir)))
      (setq packages (cl-remove-if (lambda (pair) (assoc (car pair) agents)) packages))
      (setq servers (pai-mcp--merge-servers servers (pai-mcp--merge-servers packages agents))))
    (when (equal (plist-get settings :hostConfigDiscovery) "on")
      (dolist (kind pai-mcp--host-kinds)
        (let ((loaded (pai-mcp--load-host kind dir)))
          (setq servers (pai-mcp--merge-servers servers (plist-get loaded :servers) nil (list :kind "import" :importKind kind :path global :readPath (plist-get loaded :path)))))))
    (dolist (source configs)
      (let* ((file (car source)) (json (cdr source)) imported
             (provenance (cond ((equal file global) (list :kind "user" :path file :readPath file))
                               ((member file (cl-subseq files 0 (min 3 (length files))))
                                (list :kind "import" :path global :readPath file
                                      :importKind (cond ((equal file (expand-file-name "~/.agents/mcp.json")) ".agents MCP config")
                                                        ((equal file (expand-file-name "~/.agents/mcp/mcp.json")) ".agents/mcp MCP config")
                                                        (t "global MCP config"))))
                               (t (list :kind "project" :path file :readPath file)))))
        (dolist (kind (plist-get json :imports))
          (let ((host (pai-mcp--load-host kind dir)))
            (setq imported (pai-mcp--merge-servers imported (plist-get host :servers) t (list :kind "import" :importKind kind :path global :readPath (plist-get host :path))))))
        (setq servers (pai-mcp--merge-servers servers imported))
        (setq servers (pai-mcp--merge-servers servers (pai-mcp--plist-to-alist (or (plist-get json :mcpServers) (plist-get json :mcp-servers))) nil provenance))))
    ;; Higher-precedence names also shadow differently spelled Claude namespaces.
    (setq servers (cl-remove-if (lambda (pair)
                                 (and (equal (plist-get (cdr (assoc (car pair) pai-mcp--provenance)) :kind) "claude-plugin")
                                      (cl-some (lambda (other) (and (not (equal (car pair) (car other)))
                                                                    (equal (pai-mcp--namespace (car pair)) (pai-mcp--namespace (car other))))) servers))) servers))
    (dolist (entry (reverse pai-mcp--runtime-servers))
      (setq servers (pai-mcp--merge-servers servers (list (cons (car entry) (plist-get (cdr entry) :definition))) nil
                                          (list :kind "runtime" :source (plist-get (cdr entry) :source) :persisted :false))))
    servers))

(defun pai-mcp-server-provenance (name &optional dir)
  "Return NAME's source/writable override provenance without connecting."
  (pai-mcp-load-config dir)
  (copy-sequence (cdr (assoc name pai-mcp--provenance))))

(defun pai-mcp-register-server (name def &optional source)
  "Register runtime NAME with DEF from SOURCE, replacing its previous overlay.
Return a registration plist containing :dispose, an ownership-safe closure.
No connection is started; source-change consumers may invalidate cached tools."
  (unless (and (stringp name) (not (string-empty-p (string-trim name))) (pai-mcp--object-p def)
               (= 1 (cl-count-if (lambda (key) (let ((v (plist-get def key))) (and (stringp v) (not (string-empty-p v))))) '(:command :url :socket))))
    (error "Runtime registration requires a name and exactly one transport"))
  (let* ((token (make-symbol "mcp-registration"))
         (entry (list :definition (pai-mcp--replace-placeholders def nil) :source source :token token)))
    (setq pai-mcp--runtime-servers (cons (cons name entry) (assoc-delete-all name pai-mcp--runtime-servers)))
    (run-hook-with-args 'pai-mcp-source-change-hook name 'registered)
    (list :name name :dispose (lambda ()
                               (when (eq token (plist-get (cdr (assoc name pai-mcp--runtime-servers)) :token))
                                 (pai-mcp-unregister-server name))))))

(defun pai-mcp-unregister-server (name &optional source)
  "Remove NAME's runtime overlay, optionally only if owned by SOURCE."
  (let ((entry (cdr (assoc name pai-mcp--runtime-servers))))
    (when (and entry (or (null source) (equal source (plist-get entry :source))))
      (setq pai-mcp--runtime-servers (assoc-delete-all name pai-mcp--runtime-servers))
      (run-hook-with-args 'pai-mcp-source-change-hook name 'unregistered)
      t)))

(defun pai-mcp-runtime-snapshot (name)
  "Return a copy of NAME's runtime registration without starting the client."
  (let ((entry (cdr (assoc name pai-mcp--runtime-servers))))
    (unless entry (error "No runtime MCP server named %s" name))
    (list :name name :definition (pai-mcp--replace-placeholders (plist-get entry :definition) nil)
          :runtime t :persisted :false)))

(defun pai-mcp-status-snapshot (&optional dir)
  "Return a sanitized status snapshot without starting or allocating client state."
  (let (servers (total-tools 0) (total-resources 0) (connected 0) (disabled-count 0))
    (dolist (pair (pai-mcp-load-config dir))
      (let* ((name (car pair)) (def (cdr pair))
             (disabled (pai-mcp--disabled-p def))
             (state (and (boundp 'pai-mcp--servers) (gethash name pai-mcp--servers)))
             (metadata (and (boundp 'pai-mcp--metadata) (cdr (assoc name pai-mcp--metadata))))
             (status (cond (disabled "disabled")
                           ((eq (plist-get state :status) 'ready) "connected")
                           ((eq (plist-get state :status) 'needs-auth) "needs-auth")
                           ((eq (plist-get state :status) 'error) "failed")
                           ((or metadata (plist-get state :tools)) "cached")
                           (t "not-connected")))
             (tools (if disabled 0 (length (or (plist-get state :tools) (plist-get metadata :tools)))))
             (resources (if disabled 0 (length (plist-get state :resources)))))
        (when disabled (cl-incf disabled-count))
        (when (equal status "connected") (cl-incf connected))
        (cl-incf total-tools tools) (cl-incf total-resources resources)
        (push (list :name name :status status :disabled (if disabled t :false)
                    :listenState (if (equal status "connected") (or (plist-get state :listen-state) "connected") "disconnected")
                    :runtime (if (assoc name pai-mcp--runtime-servers) t :false)
                    :toolCount tools :resourceCount resources
                    :directToolCount (if disabled 0 (or (plist-get state :direct-tool-count) 0))
                    :source (copy-sequence (cdr (assoc name pai-mcp--provenance)))) servers)))
    (list :version 1 :servers (vconcat (nreverse servers)) :totalTools total-tools
          :totalResources total-resources :connectedCount connected :disabledCount disabled-count)))

(defun pai-mcp-handle-runtime-register (request)
  "Handle a versioned runtime registration REQUEST and return its result plist."
  (condition-case err
      (progn (unless (eq (plist-get request :version) 1) (error "Unsupported runtime registration version"))
             (list :ok t :registration (pai-mcp-register-server (plist-get request :name) (plist-get request :definition) (plist-get request :source))))
    (error (list :ok :false :error (error-message-string err)))))

(defun pai-mcp-handle-runtime-snapshot (request)
  "Handle a versioned runtime snapshot REQUEST without connection side effects."
  (condition-case err
      (progn (unless (eq (plist-get request :version) 1) (error "Unsupported runtime snapshot version"))
             (list :ok t :snapshot (pai-mcp-runtime-snapshot (plist-get request :name))))
    (error (list :ok :false :error (error-message-string err)))))

(provide 'pai-mcp-sources)
;;; pai-mcp-sources.el ends here
