;;; pai-dap-config.el --- DAP adapter configuration for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Adapter configuration for the pai DAP debugger, ported from oh-my-pi's
;; `dap/config.ts` and `dap/defaults.json'.  Provides the built-in adapter
;; catalog, config-source precedence (workspace/user `dap.json'), adapter
;; resolution against $PATH, and launch/attach auto-selection.
;;
;; An adapter config is a plist:
;;   (:command STR :args (STR...) :languages (STR...) :file-types (STR...)
;;    :root-markers (STR...) :launch-defaults PLIST :attach-defaults PLIST
;;    :connect-mode (stdio|socket|tcp) :accepts-directory-program BOOL)
;; A resolved adapter adds :name and :resolved-command (absolute path).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(defconst pai-dap-config--extensionless-order '("gdb" "lldb-dap")
  "Adapter preference order for programs without a distinguishing extension.")

(defconst pai-dap-config--js-debug-server-env "JS_DEBUG_DAP_SERVER"
  "Environment variable overriding the js-debug dapDebugServer.js path.")

(defconst pai-dap-config--port-token "${port}"
  "Placeholder replaced by the reserved TCP port in adapter args.")

;;;; Built-in defaults (ported from defaults.json)

(defconst pai-dap-config--defaults
  '(("gdb"
     :command "gdb" :args ("-i" "dap")
     :languages ("c" "cpp" "rust")
     :file-types (".c" ".cc" ".cpp" ".cxx" ".h" ".hh" ".hpp" ".hxx" ".rs")
     :root-markers ("Makefile" "CMakeLists.txt" "Cargo.toml" "compile_commands.json")
     :launch-defaults (:request "launch" :stopOnEntry t :stopAtBeginningOfMainSubprogram t)
     :attach-defaults (:request "attach"))
    ("lldb-dap"
     :command "lldb-dap" :args ()
     :languages ("c" "cpp" "objc" "swift" "rust" "zig")
     :file-types (".c" ".cc" ".cpp" ".cxx" ".m" ".mm" ".swift" ".rs" ".zig")
     :root-markers ("Package.swift" "Cargo.toml" "Makefile" "CMakeLists.txt" "build.zig")
     :launch-defaults (:request "launch" :stopOnEntry t)
     :attach-defaults (:request "attach"))
    ("codelldb"
     :command "codelldb" :args ("--port" "0")
     :languages ("c" "cpp" "rust" "zig")
     :file-types (".c" ".cc" ".cpp" ".cxx" ".rs" ".zig")
     :root-markers ("Cargo.toml" "CMakeLists.txt" "Makefile" "compile_commands.json" "build.zig")
     :launch-defaults (:request "launch" :stopOnEntry t)
     :attach-defaults (:request "attach"))
    ("debugpy"
     :command "python" :args ("-m" "debugpy.adapter")
     :languages ("python")
     :file-types (".py")
     :root-markers ("pyproject.toml" "setup.py" "requirements.txt" "Pipfile")
     :launch-defaults (:request "launch" :justMyCode :json-false :stopOnEntry t)
     :attach-defaults (:request "attach" :justMyCode :json-false))
    ("dlv"
     :command "dlv" :args ("dap") :connect-mode socket
     :languages ("go")
     :file-types (".go")
     :root-markers ("go.mod" "go.sum" "go.work")
     :accepts-directory-program t
     :launch-defaults (:request "launch" :mode "debug" :stopOnEntry t)
     :attach-defaults (:request "attach" :mode "local"))
    ("js-debug-adapter"
     :command "js-debug-adapter" :args ()
     :languages ("javascript" "typescript")
     :file-types (".js" ".jsx" ".ts" ".tsx" ".mjs" ".cjs")
     :root-markers ("package.json" "tsconfig.json" "jsconfig.json")
     :launch-defaults (:request "launch" :type "pwa-node" :stopOnEntry t)
     :attach-defaults (:request "attach" :type "pwa-node"))
    ("netcoredbg"
     :command "netcoredbg" :args ("--interpreter=vscode")
     :languages ("csharp" "fsharp")
     :file-types (".cs" ".csx" ".fs" ".fsx")
     :root-markers ("*.sln" "*.csproj" "*.fsproj" "global.json")
     :launch-defaults (:request "launch" :stopAtEntry t)
     :attach-defaults (:request "attach"))
    ("kotlin-debug-adapter"
     :command "kotlin-debug-adapter" :args ()
     :languages ("kotlin")
     :file-types (".kt" ".kts")
     :root-markers ("build.gradle" "build.gradle.kts" "pom.xml" "settings.gradle" "settings.gradle.kts")
     :launch-defaults (:request "launch" :mainClass "" :projectRoot "")
     :attach-defaults (:request "attach"))
    ("rdbg"
     :command "rdbg" :args ("--open" "--command" "--")
     :languages ("ruby")
     :file-types (".rb" ".rake" ".gemspec")
     :root-markers ("Gemfile" "Rakefile" ".ruby-version")
     :launch-defaults (:request "launch" :type "rdbg")
     :attach-defaults (:request "attach" :type "rdbg"))
    ("php-debug-adapter"
     :command "php-debug-adapter" :args ()
     :languages ("php")
     :file-types (".php" ".phtml")
     :root-markers ("composer.json" "composer.lock")
     :launch-defaults (:request "launch" :stopOnEntry t)
     :attach-defaults (:request "attach"))
    ("bash-debug-adapter"
     :command "bash-debug-adapter" :args ()
     :languages ("bash" "shell")
     :file-types (".sh" ".bash")
     :root-markers (".git")
     :launch-defaults (:request "launch" :type "bashdb" :pathBashdb "bashdb" :pathBash "bash")
     :attach-defaults (:request "attach"))
    ("dart-debug-adapter"
     :command "dart" :args ("debug_adapter")
     :languages ("dart")
     :file-types (".dart")
     :root-markers ("pubspec.yaml" "pubspec.lock")
     :launch-defaults (:request "launch" :stopOnEntry t)
     :attach-defaults (:request "attach"))
    ("elixir-ls-debugger"
     :command "elixir-ls-debugger" :args ()
     :languages ("elixir")
     :file-types (".ex" ".exs" ".heex" ".eex")
     :root-markers ("mix.exs" "mix.lock")
     :launch-defaults (:request "launch" :type "mix_task" :task "run" :stopOnEntry t)
     :attach-defaults (:request "attach")))
  "Built-in DAP adapter configurations, keyed by adapter name.")

;;;; Config normalization

(defun pai-dap-config--string-list (value)
  "Coerce VALUE to a list of non-empty strings."
  (delq nil (mapcar (lambda (x) (and (stringp x) (> (length x) 0) x))
                    (if (listp value) value (append value nil)))))

(defun pai-dap-config--plist-p (x)
  "Return non-nil if X looks like a plist (or nil)."
  (or (null x) (and (consp x) (keywordp (car x)))))

(defun pai-dap-config--normalize (name config)
  "Normalize a raw CONFIG plist for adapter NAME, or nil if invalid."
  (let ((command (plist-get config :command)))
    (when (and (stringp command) (> (length command) 0))
      (let ((mode (plist-get config :connect-mode)))
        (list :name name
              :command command
              :args (pai-dap-config--string-list (plist-get config :args))
              :languages (pai-dap-config--string-list (plist-get config :languages))
              :file-types (mapcar #'downcase
                                  (pai-dap-config--string-list (plist-get config :file-types)))
              :root-markers (pai-dap-config--string-list (plist-get config :root-markers))
              :launch-defaults (or (plist-get config :launch-defaults) '())
              :attach-defaults (or (plist-get config :attach-defaults) '())
              :accepts-directory-program (eq (plist-get config :accepts-directory-program) t)
              :connect-mode (cond ((memq mode '(socket tcp stdio)) mode)
                                  ((member mode '("socket" "tcp" "stdio")) (intern mode))
                                  (t 'stdio)))))))

(defun pai-dap-config--merge-one (base override)
  "Merge OVERRIDE plist over BASE plist, deep-merging launch/attach defaults."
  (let ((merged (copy-sequence base)))
    (cl-loop for (k v) on override by #'cddr do
             (setq merged
                   (plist-put merged k
                              (if (memq k '(:launch-defaults :attach-defaults))
                                  (let ((mb (copy-sequence (or (plist-get base k) '()))))
                                    (cl-loop for (dk dv) on v by #'cddr do
                                             (setq mb (plist-put mb dk dv)))
                                    mb)
                                v))))
    merged))

;;;; Config sources

(defun pai-dap-config--parse-file (file)
  "Parse dap.json FILE, returning an alist of (NAME . raw-plist) or nil."
  (when (file-readable-p file)
    (condition-case nil
        (let* ((json-object-type 'plist)
               (json-key-type 'keyword)
               (json-array-type 'list)
               (data (json-read-file file))
               ;; Support either {adapters:{...}} or a bare {name:{...}} map.
               (adapters (if (and (pai-dap-config--plist-p data)
                                  (plist-member data :adapters))
                             (plist-get data :adapters)
                           data)))
          (cl-loop for (k v) on adapters by #'cddr
                   collect (cons (substring (symbol-name k) 1) v)))
      (error nil))))

(defun pai-dap-config--source-files (cwd)
  "Return candidate dap config files for CWD, highest priority first."
  (let ((names '("dap.json" ".dap.json"))
        (dirs (list cwd
                    (expand-file-name ".pai" cwd)
                    (expand-file-name "pai" (or (getenv "XDG_CONFIG_HOME")
                                                (expand-file-name ".config" "~")))
                    (expand-file-name "omp" (or (getenv "XDG_CONFIG_HOME")
                                                (expand-file-name ".config" "~")))
                    (expand-file-name "~"))))
    (cl-loop for dir in dirs append
             (cl-loop for n in names collect (expand-file-name n dir)))))

(defun pai-dap-config-adapters (&optional cwd)
  "Return the merged adapter config alist (NAME . resolved-plist) for CWD."
  (let ((merged (cl-loop for (name . cfg) in pai-dap-config--defaults
                         collect (cons name (pai-dap-config--normalize name cfg)))))
    (when cwd
      ;; Lowest priority first so higher-priority files win via later merge.
      (dolist (file (reverse (pai-dap-config--source-files cwd)))
        (dolist (entry (pai-dap-config--parse-file file))
          (let* ((name (car entry))
                 (raw (cdr entry))
                 (existing (cdr (assoc name merged)))
                 (candidate (if existing
                                (pai-dap-config--merge-one existing raw)
                              (pai-dap-config--normalize name raw)))
                 (normalized (if existing
                                 (pai-dap-config--normalize name candidate)
                               candidate)))
            (when normalized
              (setf (alist-get name merged nil nil #'equal) normalized))))))
    merged))

;;;; Adapter resolution

(defun pai-dap-config--which (command)
  "Return the absolute path of COMMAND, or nil.  Absolute/relative paths pass through."
  (cond
   ((file-name-absolute-p command)
    (and (file-executable-p command) command))
   ((string-match-p "[/\\\\]" command)
    (let ((p (expand-file-name command)))
      (and (file-executable-p p) p)))
   (t (executable-find command))))

(defun pai-dap-config--js-debug-server ()
  "Resolve the js-debug dapDebugServer.js path, or nil."
  (let* ((configured (getenv pai-dap-config--js-debug-server-env))
         (data-home (or (getenv "XDG_DATA_HOME") (expand-file-name ".local/share" "~")))
         (candidates (delq nil
                           (list configured
                                 (expand-file-name "nvim/mason/packages/js-debug-adapter/js-debug/src/dapDebugServer.js" data-home)
                                 (expand-file-name ".local/opt/js-debug/src/dapDebugServer.js" "~")))))
    (cl-find-if #'file-exists-p candidates)))

(defun pai-dap-config--resolve-js-debug (config)
  "Return a resolved js-debug adapter for CONFIG, nil if server missing, or `none'."
  (if (not (equal (plist-get config :command) "js-debug-adapter"))
      'none
    (let ((server (pai-dap-config--js-debug-server))
          (node (executable-find "node")))
      (when server
        (append (copy-sequence config)
                (list :command (if node "node" "emacs")
                      :args (list server pai-dap-config--port-token "127.0.0.1")
                      :resolved-command (or node (executable-find "node") "node")
                      :connect-mode 'tcp))))))

(defun pai-dap-config-resolve (name &optional cwd configs)
  "Resolve adapter NAME against $PATH for CWD.  Return a resolved plist or nil."
  (let* ((configs (or configs (pai-dap-config-adapters cwd)))
         (config (cdr (assoc name configs))))
    (when config
      (let ((js (pai-dap-config--resolve-js-debug config)))
        (if (not (eq js 'none))
            js                          ; nil (unavailable) or resolved js-debug
          (let ((resolved (pai-dap-config--which (plist-get config :command))))
            (when resolved
              (plist-put (copy-sequence config) :resolved-command resolved))))))))

(defun pai-dap-config-available (&optional cwd)
  "Return the list of resolved adapters available for CWD."
  (let ((configs (pai-dap-config-adapters cwd)))
    (delq nil (cl-loop for (name . _) in configs
                       collect (pai-dap-config-resolve name cwd configs)))))

(defun pai-dap-config-available-names (&optional cwd)
  "Return names of resolvable adapters for CWD, comma-joined, or \"none\"."
  (let ((names (mapcar (lambda (a) (plist-get a :name)) (pai-dap-config-available cwd))))
    (if names (mapconcat #'identity names ", ") "none")))

;;;; Root markers / launch selection

(defun pai-dap-config--has-marker (dir markers)
  "Return non-nil if DIR contains any of MARKERS (glob patterns allowed)."
  (cl-some (lambda (m)
             (if (string-match-p "[*?]" m)
                 (directory-files dir nil (wildcard-to-regexp m) t)
               (file-exists-p (expand-file-name m dir))))
           markers))

(defun pai-dap-config--root-in-ancestry (program cwd markers program-kind)
  "Search PROGRAM's ancestry for a directory containing any of MARKERS.
Return the directory or nil.  PROGRAM-KIND is `directory', `file', or `missing'."
  (when markers
    (let ((dir (if (eq program-kind 'directory)
                   (expand-file-name program cwd)
                 (file-name-directory (expand-file-name program cwd)))))
      (catch 'found
        (while dir
          (when (pai-dap-config--has-marker dir markers)
            (throw 'found (directory-file-name dir)))
          (let ((parent (file-name-directory (directory-file-name dir))))
            (setq dir (unless (equal parent dir) parent))))
        nil))))

(defun pai-dap-config--program-extension (program)
  "Return the lowercased dotted extension of PROGRAM, or empty string."
  (let ((ext (file-name-extension program)))
    (if ext (downcase (concat "." ext)) "")))

(defun pai-dap-config--sort-for-launch (program cwd program-kind adapters)
  "Sort resolved ADAPTERS for launching PROGRAM (best first)."
  (let ((ext (pai-dap-config--program-extension program)))
    (cl-sort
     (copy-sequence adapters)
     (lambda (a b)
       (let* ((a-ext (and (> (length ext) 0) (member ext (plist-get a :file-types))))
              (b-ext (and (> (length ext) 0) (member ext (plist-get b :file-types))))
              (a-root (pai-dap-config--root-in-ancestry
                       program cwd (plist-get a :root-markers) program-kind))
              (b-root (pai-dap-config--root-in-ancestry
                       program cwd (plist-get b :root-markers) program-kind))
              (a-rank (or (cl-position (plist-get a :name) pai-dap-config--extensionless-order :test #'equal)
                          most-positive-fixnum))
              (b-rank (or (cl-position (plist-get b :name) pai-dap-config--extensionless-order :test #'equal)
                          most-positive-fixnum)))
         (cond ((not (eq (and a-ext t) (and b-ext t))) a-ext)
               ((not (eq (and a-root t) (and b-root t))) a-root)
               ((/= a-rank b-rank) (< a-rank b-rank))
               (t (string< (plist-get a :name) (plist-get b :name)))))))))

(defun pai-dap-config--select-automatic (program cwd program-kind configs)
  "Auto-select a launch adapter for PROGRAM.  Return (adapter . RESOLVED),
\(unavailable . NAME), or (none)."
  (let ((ext (pai-dap-config--program-extension program)))
    (or
     ;; Extension-driven match.
     (when (> (length ext) 0)
       (let (configured available)
         (cl-loop for (name . config) in configs do
                  (when (and config (member ext (plist-get config :file-types)))
                    (push name configured)
                    (let ((a (pai-dap-config-resolve name cwd configs)))
                      (when a (push a available)))))
         (let ((selected (car (pai-dap-config--sort-for-launch program cwd program-kind available))))
           (cond (selected (cons 'adapter selected))
                 (configured (cons 'unavailable (car (last configured))))
                 (t nil)))))
     ;; Root-marker / extensionless fallback.
     (let (available root-matches directory-matches)
       (cl-loop for (name . config) in configs do
                (when config
                  (let* ((root (pai-dap-config--root-in-ancestry
                                program cwd (plist-get config :root-markers) program-kind)))
                    (when root
                      (push name root-matches)
                      (when (eq (plist-get config :accepts-directory-program) t)
                        (push name directory-matches)))
                    (when (or (member name pai-dap-config--extensionless-order) root)
                      (let ((a (pai-dap-config-resolve name cwd configs)))
                        (when a (push a available)))))))
       (if (and (eq program-kind 'directory) directory-matches)
           (let* ((names directory-matches)
                  (dir-adapters (cl-remove-if-not
                                 (lambda (a) (and (plist-get a :accepts-directory-program)
                                                  (member (plist-get a :name) names)))
                                 available))
                  (selected (car (pai-dap-config--sort-for-launch program cwd program-kind dir-adapters))))
             (cond (selected (cons 'adapter selected))
                   (directory-matches (cons 'unavailable (car (last directory-matches))))
                   (t '(none))))
         (let* ((dir-adapters (if (eq program-kind 'directory)
                                  (cl-remove-if-not (lambda (a) (plist-get a :accepts-directory-program)) available)
                                available))
                (candidates (or dir-adapters available))
                (selected (car (pai-dap-config--sort-for-launch program cwd program-kind candidates))))
           (cond (selected (cons 'adapter selected))
                 (root-matches (cons 'unavailable (car (last root-matches))))
                 (t '(none))))))
     '(none))))

(defun pai-dap-config-select-launch (program cwd &optional adapter-name program-kind)
  "Select a launch adapter for PROGRAM in CWD.
Return (adapter . RESOLVED), (unavailable . NAME), or (none).
PROGRAM-KIND defaults to `file'."
  (let ((configs (pai-dap-config-adapters cwd))
        (program-kind (or program-kind 'file)))
    (if adapter-name
        (let ((config (cdr (assoc adapter-name configs))))
          (cond ((null config) '(none))
                (t (let ((a (pai-dap-config-resolve adapter-name cwd configs)))
                     (if a (cons 'adapter a) (cons 'unavailable adapter-name))))))
      (pai-dap-config--select-automatic program cwd program-kind configs))))

(defun pai-dap-config-select-attach (cwd &optional adapter-name port)
  "Select an attach adapter for CWD.  Return a resolved adapter plist or nil."
  (if adapter-name
      (pai-dap-config-resolve adapter-name cwd)
    (let ((available (pai-dap-config-available cwd)))
      (or (and port (cl-find "debugpy" available :key (lambda (a) (plist-get a :name)) :test #'equal))
          (cl-loop for pref in pai-dap-config--extensionless-order
                   thereis (cl-find pref available :key (lambda (a) (plist-get a :name)) :test #'equal))
          (car available)))))

(defun pai-dap-config-launch-overrides (adapter program program-kind)
  "Return adapter-specific launch overrides for ADAPTER launching PROGRAM."
  (if (equal (plist-get adapter :name) "dlv")
      (let ((ext (pai-dap-config--program-extension program)))
        (cond ((or (eq program-kind 'directory) (equal ext ".go")) (list :mode "debug"))
              ((eq program-kind 'file) (list :mode "exec"))
              (t '())))
    '()))

(provide 'pai-dap-config)
;;; pai-dap-config.el ends here
