;;; pai-mcp-test.el --- Tests for the MCP adapter extension -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-mcp-config)
(require 'pai-mcp-client)
(require 'pai-mcp-direct)
(require 'pai-mcp-search)
(require 'pai-mcp)
(require 'pai-mcp-catalog)

(defconst pai-mcp-test--fixture
  (expand-file-name "fixtures/pai-mcp-fake-server.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the fake stdio MCP server script.")

(defun pai-mcp-test--fake-config (dir &optional extra)
  "Write a .mcp.json in DIR whose one server runs the fake server via Emacs.
EXTRA is merged into the server definition plist."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (server (append (list :command emacs
                               :args (list "-Q" "--batch" "-l" pai-mcp-test--fixture))
                         extra))
         (json (list :mcpServers (list :fake server))))
    (make-directory dir t)
    (with-temp-file (expand-file-name ".mcp.json" dir)
      (insert (pai-json-encode json)))))

(defmacro pai-mcp-test--with-project (dir &rest body)
  "Run BODY with a temp project DIR and isolated MCP state."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-mcp" t)))
          (pai-directory (expand-file-name ".state" ,dir))
          (pai-mcp--servers (make-hash-table :test 'equal))
          (pai-mcp--metadata nil)
          (pai-mcp--metadata-loaded nil)
          (pai--tools (copy-hash-table pai--tools))
          (pai-mcp--direct-registered nil)
          (pai-mcp--search-held nil)
          (pai-mcp-catalog--commands nil)
          (default-directory ,dir))
     (unwind-protect (progn ,@body)
       (maphash (lambda (name _s) (pai-mcp-stop name)) pai-mcp--servers)
       (ignore-errors (delete-directory ,dir t)))))

(defun pai-mcp-test--pump (predicate &optional timeout)
  "Pump process output until PREDICATE returns non-nil or TIMEOUT elapses."
  (let ((deadline (+ (float-time) (or timeout 15))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

;;;; Proxy arguments

(ert-deftest pai-mcp-proxy-call-forwards-args ()
  "The proxy must pass nested tool arguments through unchanged."
  (let (captured result)
    (cl-letf (((symbol-function 'pai-mcp-call)
               (lambda (server tool args on-done &optional _dir)
                 (setq captured (list server tool args))
                 (funcall on-done (pai-tool-ok-result "Hello, Alice!")))))
      (pai-mcp--tool-execute
       '(:action "call" :server "demo-greeter" :tool "greet"
         :args (:name "Alice"))
       (list :buffer (current-buffer)) nil
       (lambda (value) (setq result value))))
    (should (equal captured '("demo-greeter" "greet" (:name "Alice"))))
    (should result)))

(ert-deftest pai-mcp-proxy-call-accepts-inlined-args ()
  "Parameters inlined at the top level (or the MCP-native `arguments' field, or
a JSON string) are forwarded as the tool arguments, not dropped."
  (cl-letf* ((captured nil)
             ((symbol-function 'pai-mcp-call)
              (lambda (server tool args on-done &optional _dir)
                (setq captured (list server tool args))
                (funcall on-done (pai-tool-ok-result "ok")))))
    ;; the exact shape the model produced: name at the top level, no :args
    (pai-mcp--tool-execute
     '(:action "call" :search "" :describe "" :instructions ""
       :tool "greet" :server "demo-greeter" :regex :false :name "Alice")
     (list :buffer (current-buffer)) nil #'ignore)
    (should (equal captured '("demo-greeter" "greet" (:name "Alice"))))
    ;; the MCP-native "arguments" field is honored too
    (setq captured nil)
    (pai-mcp--tool-execute
     '(:action "call" :tool "greet" :server "demo-greeter" :arguments (:name "Bob"))
     (list :buffer (current-buffer)) nil #'ignore)
    (should (equal captured '("demo-greeter" "greet" (:name "Bob"))))
    ;; a JSON-string args object is decoded
    (setq captured nil)
    (pai-mcp--tool-execute
     '(:action "call" :tool "greet" :server "demo-greeter" :args "{\"name\":\"Cy\"}")
     (list :buffer (current-buffer)) nil #'ignore)
    (should (equal captured '("demo-greeter" "greet" (:name "Cy"))))
    ;; a nested :args object still wins over any inlined keys
    (setq captured nil)
    (pai-mcp--tool-execute
     '(:action "call" :tool "greet" :server "demo-greeter" :name "ignored" :args (:name "Dee"))
     (list :buffer (current-buffer)) nil #'ignore)
    (should (equal captured '("demo-greeter" "greet" (:name "Dee"))))))

(ert-deftest pai-mcp-proxy-args-schema-guidance ()
  "The proxy advertises a free-form argument object with usage guidance."
  (let* ((parameters (plist-get (pai-tool-get "mcp") :parameters))
         (args (plist-get (plist-get parameters :properties) :args)))
    (should (equal (plist-get args :type) "object"))
    (should (eq (plist-get args :additionalProperties) t))
    (should (string-match-p "required tool parameters"
                            (plist-get args :description)))))

;;;; Config discovery

(ert-deftest pai-mcp-config-precedence ()
  "Later config files override earlier ones by server name."
  (pai-mcp-test--with-project dir
    ;; global (lowest) sets fake -> A; project (.mcp.json) overrides -> B
    (make-directory pai-directory t)
    (with-temp-file (expand-file-name "mcp.json" pai-directory)
      (insert (pai-json-encode (list :mcpServers (list :fake (list :command "A")
                                                       :only-global (list :command "G"))))))
    (with-temp-file (expand-file-name ".mcp.json" dir)
      (insert (pai-json-encode (list :mcpServers (list :fake (list :command "B"))))))
    (let ((config (pai-mcp-load-config dir)))
      (should (equal (plist-get (cdr (assoc "fake" config)) :command) "B"))
      (should (assoc "only-global" config)))))

(ert-deftest pai-mcp-interpolation ()
  "Environment interpolation expands ${VAR}, $env:VAR and ~."
  (let ((process-environment (cons "PAI_MCP_TEST_VAR=hello" process-environment)))
    (should (equal (pai-mcp--interpolate "${PAI_MCP_TEST_VAR}/x") "hello/x"))
    (should (equal (pai-mcp--interpolate "$env:PAI_MCP_TEST_VAR") "hello"))
    (should (string-prefix-p (expand-file-name "~/") (pai-mcp--interpolate "~/a")))))

(ert-deftest pai-mcp-disabled-flag ()
  "Only literal true disables a server."
  (should (pai-mcp--disabled-p '(:disabled t)))
  (should-not (pai-mcp--disabled-p '(:disabled :false)))
  (should-not (pai-mcp--disabled-p '(:command "x"))))

;;;; JSON-RPC framing (no process)

(ert-deftest pai-mcp-filter-dispatches-frames ()
  "The filter splits newline-delimited frames and fires request callbacks."
  (pai-mcp-test--with-project dir
    (let* ((server (pai-mcp--server "s"))
           (got nil))
      ;; register a pending request for id 7
      (puthash 7 (lambda (result _err) (setq got result)) (plist-get server :pending))
      ;; deliver a partial frame then its completion
      (pai-mcp--filter "s" "{\"jsonrpc\":\"2.0\",\"id\":7,\"resu")
      (should-not got)
      (pai-mcp--filter "s" "lt\":{\"ok\":true}}\n")
      (should (eq (plist-get got :ok) t)))))

;;;; End-to-end: auto-start + call (real subprocess, async, non-blocking)

(ert-deftest pai-mcp-negotiated-call-respects-approval ()
  "Real startup discovers tools, but denied calls never reach the process."
  (pai-mcp-test--with-project dir
    (let ((log (expand-file-name "wire.log" dir)) result)
      (pai-mcp-test--fake-config
       dir (list :approveTools t :env (list :PAI_MCP_FAKE_LOG log)))
      (pai-mcp-call "fake" "echo" '(:text "guarded")
                    (lambda (value) (setq result value)) dir)
      (should-not result)
      (should (pai-mcp-test--pump (lambda () result)))
      (should (eq (plist-get result :is-error) t))
      (should (equal (mapcar (lambda (tool) (plist-get tool :name))
                            (pai-mcp--server-tools "fake")) '("echo" "add")))
      (should (equal (with-temp-buffer (insert-file-contents log) (buffer-string))
                     "initialize\nnotifications/initialized\ntools/list\n"))
      (pai-mcp--set (pai-mcp--server "fake") :def
                    (plist-put (plist-get (pai-mcp--server "fake") :def) :approveTools :false))
      (setq result nil)
      (pai-mcp-call "fake" "echo" '(:text "guarded")
                    (lambda (value) (setq result value)) dir)
      (should (pai-mcp-test--pump (lambda () result)))
      (should (equal (pai-content-text (plist-get result :content)) "guarded")))))

(ert-deftest pai-mcp-autostart-and-call ()
  "Calling a tool auto-starts the server; the UI is not blocked."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)
    (let ((result nil) (delivered nil))
      ;; the server is not running yet
      (should (eq (pai-mcp-server-status "fake") 'stopped))
      (pai-mcp-call "fake" "echo" '(:text "hi there")
                    (lambda (r) (setq result r delivered t)) dir)
      ;; non-blocking: call returns before the child answers
      (should-not delivered)
      (should (memq (pai-mcp-server-status "fake") '(starting ready)))
      (should (pai-mcp-test--pump (lambda () delivered)))
      (should (equal (pai-content-text (plist-get result :content)) "hi there"))
      (should (eq (pai-mcp-server-status "fake") 'ready))
      ;; a second call reuses the running server
      (let ((r2 nil))
        (pai-mcp-call "fake" "add" '(:a 2 :b 3) (lambda (r) (setq r2 r)) dir)
        (should (pai-mcp-test--pump (lambda () r2)))
        (should (equal (pai-content-text (plist-get r2 :content)) "5"))))))

(ert-deftest pai-mcp-refresh-populates-cache ()
  "Refresh connects the server, caches tools, and search works from cache."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)
    (let ((summary nil))
      (pai-mcp-refresh (lambda (s) (setq summary s)) dir)
      (should (pai-mcp-test--pump (lambda () summary)))
      (should (string-match-p "fake: 2 tool(s)" summary))
      ;; cache persisted to disk
      (should (file-exists-p (pai-mcp--metadata-file)))
      ;; search finds a tool from cache after disconnect
      (pai-mcp-stop "fake")
      (should (equal (pai-mcp-server-status "fake") 'stopped))
      (should (string-match-p "echo" (pai-mcp--search "echo" dir))))))

(ert-deftest pai-mcp-tool-execute-list-and-search ()
  "The proxy tool answers list/search actions without launching servers."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)
    (let (out)
      (pai-mcp--tool-execute '(:action "list") (list :cwd dir) nil
                             (lambda (r) (setq out r)))
      (should (string-match-p "fake" (pai-content-text (plist-get out :content))))
      (should (eq (pai-mcp-server-status "fake") 'stopped)))))

(ert-deftest pai-mcp-tool-call-autostarts-via-proxy ()
  "The proxy `call' action auto-starts the owning server and returns output."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)
    ;; seed the cache so the proxy knows which server owns `echo'
    (let ((summary nil))
      (pai-mcp-refresh (lambda (s) (setq summary s)) dir)
      (should (pai-mcp-test--pump (lambda () summary))))
    (pai-mcp-stop "fake")
    (let ((out nil))
      (pai-mcp--tool-execute '(:action "call" :tool "echo" :args (:text "proxied"))
                             (list :cwd dir) nil (lambda (r) (setq out r)))
      (should (pai-mcp-test--pump (lambda () out)))
      (should (equal (pai-content-text (plist-get out :content)) "proxied")))))

;;;; Phase 2: lifecycle

(ert-deftest pai-mcp-eager-startup-connect ()
  "`eager' servers connect at startup; `lazy' servers do not."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir '(:lifecycle "eager"))
    (pai-mcp-startup-connect dir)
    (should (memq (pai-mcp-server-status "fake") '(starting ready)))
    (should (pai-mcp-test--pump (lambda () (eq (pai-mcp-server-status "fake") 'ready))))))

(ert-deftest pai-mcp-lazy-no-startup-connect ()
  "A lazy server stays stopped until first use."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)          ; default lifecycle = lazy
    (pai-mcp-startup-connect dir)
    (should (eq (pai-mcp-server-status "fake") 'stopped))))

(ert-deftest pai-mcp-request-timeout-resolution ()
  "Per-server requestTimeoutMs overrides the global setting; <=0 disables."
  (should (equal (pai-mcp--request-timeout-ms '(:requestTimeoutMs 500)) 500))
  (should (null (pai-mcp--request-timeout-ms '(:requestTimeoutMs 0))))
  (should (null (pai-mcp--request-timeout-ms '()))))

(ert-deftest pai-mcp-call-times-out ()
  "A tool call against a non-responding server fails via the per-call timeout."
  (pai-mcp-test--with-project dir
    ;; `cat' stays alive and echoes nothing back as JSON-RPC, so no reply comes.
    (pai-mcp-test--fake-config dir '(:requestTimeoutMs 300))
    (let* ((server (pai-mcp--server "fake"))
           (proc (make-process :name "pai-mcp-cat" :command '("cat")
                               :connection-type 'pipe :noquery t
                               :filter (lambda (_p c) (pai-mcp--filter "fake" c)))))
      ;; force a ready state with a live but silent process
      (pai-mcp--set server :process proc)
      (pai-mcp--set server :status 'ready)
      (let ((out nil))
        (pai-mcp-call "fake" "echo" '(:text "x") (lambda (r) (setq out r)) dir)
        (should (pai-mcp-test--pump (lambda () out) 5))
        (should (eq (plist-get out :is-error) t))
        (should (string-match-p "timed out" (pai-content-text (plist-get out :content)))))
      (ignore-errors (delete-process proc)))))

(ert-deftest pai-mcp-connect-refresh-reports-added ()
  "Reconnecting a ready server refreshes its catalog and reports added tools."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)
    (let ((done nil))
      (pai-mcp-ensure "fake" (lambda () (setq done t)) (lambda (_e) (setq done 'err)) dir)
      (should (pai-mcp-test--pump (lambda () done)))
      (should (eq done t)))
    ;; catalog refresh on an already-ready server; no tools added the 2nd time
    (let ((added 'unset) (err 'unset))
      (pai-mcp-refresh-catalog "fake" (lambda (a e) (setq added a err e)))
      (should (pai-mcp-test--pump (lambda () (not (eq added 'unset)))))
      (should (null err))
      (should (null added)))))

;;;; Phase 3: direct tools

(defun pai-mcp-test--seed-cache (dir)
  "Refresh the fake server so its tools are cached, then disconnect."
  (let ((summary nil))
    (pai-mcp-refresh (lambda (s) (setq summary s)) dir)
    (should (pai-mcp-test--pump (lambda () summary)))
    (pai-mcp-stop "fake")))

(ert-deftest pai-mcp-direct-registers-all ()
  "directTools:true registers every tool with the server prefix."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir '(:directTools t))
    (pai-mcp-test--seed-cache dir)
    (let ((pai--tools (copy-hash-table pai--tools)))
      (should (= 2 (pai-mcp-register-direct-tools dir)))
      (should (pai-tool-get "fake_echo"))
      (should (pai-tool-get "fake_add"))
      (should-not (pai-tool-get "echo")))))       ; unprefixed name not registered

(ert-deftest pai-mcp-direct-prefix-modes ()
  "toolPrefix none/mcp change the generated tool name."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir '(:directTools t :toolPrefix "none"))
    (pai-mcp-test--seed-cache dir)
    (let ((pai--tools (copy-hash-table pai--tools)))
      (pai-mcp-register-direct-tools dir)
      (should (pai-tool-get "echo"))
      (should-not (pai-tool-get "fake_echo")))))

(ert-deftest pai-mcp-direct-include-exclude ()
  "includeTools/excludeTools filter the registered set."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir '(:directTools t :excludeTools ("add")))
    (pai-mcp-test--seed-cache dir)
    (let ((pai--tools (copy-hash-table pai--tools)))
      (should (= 1 (pai-mcp-register-direct-tools dir)))
      (should (pai-tool-get "fake_echo"))
      (should-not (pai-tool-get "fake_add")))))

(ert-deftest pai-mcp-direct-list-selection ()
  "directTools as a name list registers only those tools."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir '(:directTools ("add")))
    (pai-mcp-test--seed-cache dir)
    (let ((pai--tools (copy-hash-table pai--tools)))
      (should (= 1 (pai-mcp-register-direct-tools dir)))
      (should (pai-tool-get "fake_add"))
      (should-not (pai-tool-get "fake_echo")))))

(ert-deftest pai-mcp-direct-search-mode-activates ()
  "directTools \"search\" holds tools inactive until a matching search."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir '(:directTools "search"))
    (pai-mcp-test--seed-cache dir)
    (let ((pai--tools (copy-hash-table pai--tools)))
      (should (= 0 (pai-mcp-register-direct-tools dir)))    ; none active yet
      (should-not (pai-tool-get "fake_echo"))
      (let ((added (pai-mcp-activate-search-tools "echo" dir)))
        (should (member "fake_echo" added))
        (should (pai-tool-get "fake_echo"))
        (should-not (pai-tool-get "fake_add"))))))          ; non-match stays held

(ert-deftest pai-mcp-direct-tool-invokes-server ()
  "A registered direct tool auto-starts the server and returns output."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir '(:directTools t))
    (pai-mcp-test--seed-cache dir)
    (let ((pai--tools (copy-hash-table pai--tools)))
      (pai-mcp-register-direct-tools dir)
      (let ((tool (pai-tool-get "fake_echo")) (out nil))
        (should (eq (pai-mcp-server-status "fake") 'stopped))
        (funcall (plist-get tool :execute) '(:text "direct!") (list :cwd dir) nil
                 (lambda (r) (setq out r)))
        (should (pai-mcp-test--pump (lambda () out)))
        (should (equal (pai-content-text (plist-get out :content)) "direct!"))))))

(ert-deftest pai-mcp-glob-translation ()
  "Glob to regexp handles `*' and anchors."
  (should (string-match-p (pai-mcp--glob-to-regexp "get_*") "get_file"))
  (should-not (string-match-p (pai-mcp--glob-to-regexp "get_*") "set_file"))
  (should (string-match-p (pai-mcp--glob-to-regexp "*_file") "get_file")))

;;;; Phase 4: search quality

(ert-deftest pai-mcp-search-ranks-exact-first ()
  "Exact name matches rank above substring/description matches."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)
    (pai-mcp-test--seed-cache dir)
    (let* ((pai-mcp-search-include-pai-tools nil)
           (res (pai-mcp-search "add" :dir dir)))
      (should (equal (car (plist-get res :names)) "add"))
      (should (= (plist-get res :count) 1)))))

(ert-deftest pai-mcp-search-pagination ()
  "Limit/offset paginate and report the next offset."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)
    (pai-mcp-test--seed-cache dir)
    (let* ((pai-mcp-search-include-pai-tools nil)
           (p1 (pai-mcp-search "" :limit 1 :offset 0 :dir dir)))
      (should (= (length (plist-get p1 :names)) 1))
      (should (= (plist-get p1 :count) 2))
      (should (= (plist-get p1 :next-offset) 1))
      (let ((p2 (pai-mcp-search "" :limit 1 :offset 1 :dir dir)))
        (should (= (length (plist-get p2 :names)) 1))
        (should (null (plist-get p2 :next-offset)))
        (should-not (equal (plist-get p1 :names) (plist-get p2 :names)))))))

(ert-deftest pai-mcp-search-regex ()
  "Regex queries match tool names."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)
    (pai-mcp-test--seed-cache dir)
    (let* ((pai-mcp-search-include-pai-tools nil)
           (res (pai-mcp-search "^ad" :regex t :dir dir)))
      (should (member "add" (plist-get res :names)))
      (should-not (member "echo" (plist-get res :names))))))

(ert-deftest pai-mcp-search-fuzzy-suggests ()
  "A near-miss returns suggestions instead of nothing."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)
    (pai-mcp-test--seed-cache dir)
    (let* ((pai-mcp-search-include-pai-tools nil)
           (res (pai-mcp-search "ecko" :dir dir)))    ; typo of echo
      (should (= (plist-get res :count) 0))
      (should (string-match-p "echo" (plist-get res :text))))))

(ert-deftest pai-mcp-search-keywords ()
  "Per-server searchKeywords make tools findable by synonym."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir '(:searchKeywords ("repeat")))
    (pai-mcp-test--seed-cache dir)
    (let* ((pai-mcp-search-include-pai-tools nil)
           (res (pai-mcp-search "repeat" :dir dir)))
      (should (member "echo" (plist-get res :names)))
      (should (member "add" (plist-get res :names))))))   ; keyword is server-wide

(ert-deftest pai-mcp-search-include-schemas ()
  "includeSchemas appends a compact parameter shape."
  (pai-mcp-test--with-project dir
    (pai-mcp-test--fake-config dir)
    (pai-mcp-test--seed-cache dir)
    (let* ((pai-mcp-search-include-pai-tools nil)
           (res (pai-mcp-search "echo" :include-schemas t :dir dir)))
      (should (string-match-p "text" (plist-get res :text)))
      (should (string-match-p "{" (plist-get res :text))))))

(provide 'pai-mcp-test)
;;; pai-mcp-test.el ends here
