;;; pai-mcp-direct.el --- Direct MCP tool registration for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Promote selected MCP tools from the `mcp' proxy to first-class pai tools that
;; appear directly in the agent's tool list.  Controlled by `directTools' (per
;; server or global `settings.directTools'): t, a name list, false, or "search"
;; (registered inactive, activated additively by `mcp({search})').  Honors
;; `toolPrefix', `includeTools'/`excludeTools' globs, the 75-tool advisory, and
;; `disableProxyTool'.  Registration is from the cached metadata, so no server
;; connection is needed at startup.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-core)
(require 'pai-tools)
(require 'pai-mcp-config)
(require 'pai-mcp-client)

(defvar-local pai-mcp--direct-registered nil
  "List of prefixed tool names this module registered in the current instance.")

(defvar-local pai-mcp--search-held nil
  "Alist of (PREFIXED . (SERVER . ORIGINAL)) for search-mode tools not yet active.")
(defvar pai-mcp-large-direct-threshold 75
  "Emit an advisory when at least this many direct tools resolve.")

;;;; Glob matching

(defun pai-mcp--glob-to-regexp (glob)
  "Translate a simple GLOB (only `*' is special) to an anchored regexp."
  (concat "\\`"
          (mapconcat (lambda (part) (if (equal part "*") ".*" (regexp-quote part)))
                     (let ((parts '()) (start 0))
                       (while (string-match "\\*" glob start)
                         (push (substring glob start (match-beginning 0)) parts)
                         (push "*" parts)
                         (setq start (match-end 0)))
                       (push (substring glob start) parts)
                       (nreverse parts))
                     "")
          "\\'"))

(defun pai-mcp--glob-match-any (patterns &rest names)
  "Return non-nil when any of NAMES matches any glob in PATTERNS."
  (seq-some (lambda (pat)
              (let ((re (pai-mcp--glob-to-regexp pat)))
                (seq-some (lambda (n) (and n (string-match-p re n))) names)))
            patterns))

;;;; Naming

(defun pai-mcp--sanitize (s)
  "Sanitize S into a tool-name-safe token."
  (replace-regexp-in-string "[^A-Za-z0-9_]" "_" s))

(defun pai-mcp--tool-prefix-mode (_server def settings)
  "Return the effective toolPrefix mode string for the server.
_SERVER is accepted for call-site symmetry with `pai-mcp--prefixed-name'."
  (or (plist-get def :toolPrefix) (plist-get settings :toolPrefix) "server"))

(defun pai-mcp--prefixed-name (server def settings original)
  "Return the direct-tool name for ORIGINAL on SERVER under the prefix mode."
  (pcase (pai-mcp--tool-prefix-mode server def settings)
    ("none" original)
    ("mcp" (format "mcp__%s__%s" (pai-mcp--sanitize server) original))
    ("short"
     (format "%s_%s" (pai-mcp--sanitize
                      (replace-regexp-in-string "[-_]mcp\\'" "" server))
             original))
    (_ (format "%s_%s" (pai-mcp--sanitize server) original))))

;;;; Selection

(defun pai-mcp--direct-mode (def settings)
  "Return the directTools mode for DEF: t, `search', a name list, or nil."
  (let ((v (if (plist-member def :directTools)
               (plist-get def :directTools)
             (plist-get settings :directTools))))
    (cond ((eq v t) t)
          ((equal v "search") 'search)
          ((and (listp v) v) v)             ; explicit name list
          (t nil))))                        ; :false / nil / "" -> proxy only

(defun pai-mcp--selected-tools (server def settings)
  "Return (ORIGINAL . PREFIXED) pairs to register directly for SERVER.
Applies directTools selection, then includeTools, then excludeTools."
  (let* ((mode (pai-mcp--direct-mode def settings))
         (tools (pai-mcp--server-tools server))
         (include (plist-get def :includeTools))
         (exclude (plist-get def :excludeTools)))
    (when mode
      (delq nil
            (mapcar
             (lambda (tool)
               (let* ((orig (plist-get tool :name))
                      (pref (pai-mcp--prefixed-name server def settings orig)))
                 (when (and (or (eq mode t) (eq mode 'search)
                                (and (listp mode) (member orig mode)))
                            (or (null include)
                                (pai-mcp--glob-match-any include orig pref))
                            (not (and exclude
                                      (pai-mcp--glob-match-any exclude orig pref))))
                   (cons orig pref))))
             tools)))))

;;;; Registration

(defun pai-mcp--make-direct-tool (server original prefixed schema)
  "Register a pai tool PREFIXED that proxies to ORIGINAL on SERVER."
  (pai-register-tool
   (list :name prefixed
         :label prefixed
         :description (format "[MCP %s] %s" server
                              (or (pai-mcp--tool-description server original) ""))
         :execution-mode 'parallel
         :parameters (or schema (pai-object-schema nil))
         :execute
         (lambda (args ctx _on-update on-done)
           (pai-mcp-call server original args on-done (pai-tool-ctx-cwd ctx)))))
  (push prefixed pai-mcp--direct-registered))

(defun pai-mcp--tool-description (server original)
  "Return the cached description for ORIGINAL on SERVER."
  (plist-get (seq-find (lambda (tl) (equal (plist-get tl :name) original))
                       (pai-mcp--server-tools server))
             :description))

(defun pai-mcp--tool-schema (server original)
  "Return the cached inputSchema for ORIGINAL on SERVER."
  (plist-get (seq-find (lambda (tl) (equal (plist-get tl :name) original))
                       (pai-mcp--server-tools server))
             :inputSchema))

(defun pai-mcp-unregister-direct-tools ()
  "Remove all direct tools this instance registered."
  (dolist (name pai-mcp--direct-registered) (pai-unregister-tool name))
  (setq pai-mcp--direct-registered nil
        pai-mcp--search-held nil))

(defun pai-mcp-register-direct-tools (&optional dir)
  "Register direct tools for all configured servers from cached metadata.
`search'-mode tools are held inactive until `pai-mcp-activate-search-tools'.
Returns the number of active direct tools registered."
  (pai-mcp-unregister-direct-tools)
  (let ((settings (pai-mcp-settings dir)) (count 0))
    (dolist (pair (pai-mcp-load-config dir))
      (let* ((server (car pair)) (def (cdr pair)))
        (unless (pai-mcp--disabled-p def)
          (let ((mode (pai-mcp--direct-mode def settings)))
            (dolist (sel (pai-mcp--selected-tools server def settings))
              (if (eq mode 'search)
                  (push (cons (cdr sel) (cons server (car sel))) pai-mcp--search-held)
                (pai-mcp--make-direct-tool
                 server (car sel) (cdr sel)
                 (pai-mcp--tool-schema server (car sel)))
                (setq count (1+ count))))))))
    (when (and (plist-get settings :warnOnLargeDirectTools)
               (>= count pai-mcp-large-direct-threshold))
      (message "pai-mcp: %d direct MCP tools registered; consider directTools \"search\" or includeTools."
               count))
    ;; disableProxyTool: hide the proxy once direct tools exist and no search mode.
    (when (and (eq (plist-get settings :disableProxyTool) t)
               (> count 0) (null pai-mcp--search-held))
      (pai-unregister-tool "mcp"))
    count))

(defun pai-mcp-activate-search-tools (keyword &optional _dir)
  "Register held search-mode tools whose name/description matches KEYWORD.
Return the list of newly activated prefixed names (additive)."
  (let* ((kw (downcase (string-trim (or keyword ""))))
         (added '()))
    (dolist (held (copy-sequence pai-mcp--search-held))
      (let* ((prefixed (car held))
             (server (cadr held))
             (original (cddr held))
             (desc (or (pai-mcp--tool-description server original) "")))
        (when (and (not (string-empty-p kw))
                   (string-match-p (regexp-quote kw)
                                   (downcase (concat prefixed " " original " " desc))))
          (pai-mcp--make-direct-tool server original prefixed
                                     (pai-mcp--tool-schema server original))
          (setq pai-mcp--search-held (assoc-delete-all prefixed pai-mcp--search-held))
          (push prefixed added))))
    (nreverse added)))

(provide 'pai-mcp-direct)
;;; pai-mcp-direct.el ends here
