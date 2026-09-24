;;; pai-mcp-config.el --- MCP config discovery for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Discovery and merge of standard MCP config files, plus environment
;; interpolation.  Later files win.  Part of the pai-mcp extension (port of
;; nicobailon/pi-mcp-adapter).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-core)
(require 'pai-config)
(declare-function pai-mcp--sources-load "pai-mcp-sources" (dir))
(declare-function pai-mcp--source-files "pai-mcp-sources" (dir))
(declare-function pai-mcp--jsonc "pai-mcp-sources" (text))
(declare-function pai-mcp--object-p "pai-mcp-sources" (value))

(defgroup pai-mcp nil
  "Token-efficient MCP adapter for pai." :group 'pai :prefix "pai-mcp-")

(defun pai-mcp--config-files (&optional dir)
  "Return MCP config file paths in ascending precedence for project DIR."
  (require 'pai-mcp-sources)
  (pai-mcp--source-files (or dir default-directory)))

(defun pai-mcp--read-json-file (file)
  "Read FILE as JSON into an internal plist, or nil on error/absence."
  (when (and file (file-readable-p file))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (let ((s (string-trim (buffer-string))))
            (unless (string-empty-p s)
              (require 'pai-mcp-sources)
              (let ((json (pai-json-decode (pai-mcp--jsonc s))))
                (when (pai-mcp--object-p json) json)))))
      (error (message "pai-mcp: bad config %s: %s" file (error-message-string err))
             nil))))

(defun pai-mcp--object-keys (plist)
  "Return the string keys of a decoded JSON object PLIST (keyword keys)."
  (let (keys)
    (while plist (push (substring (symbol-name (car plist)) 1) keys)
           (setq plist (cddr plist)))
    (nreverse keys)))

(defun pai-mcp--plist-to-alist (plist)
  "Convert decoded object PLIST to (NAME . VALUE) pairs; reject other values."
  (when (and (listp plist) (cl-evenp (length plist))
             (cl-loop for (key _value) on plist by #'cddr always (keywordp key)))
    (let (out)
      (while plist
        (push (cons (substring (symbol-name (car plist)) 1) (cadr plist)) out)
        (setq plist (cddr plist)))
      (nreverse out))))

(defun pai-mcp-load-config (&optional dir)
  "Return the merged server alist (NAME . DEF) for project DIR.
Source discovery and runtime overlays never start connections."
  (require 'pai-mcp-sources)
  (pai-mcp--sources-load (or dir default-directory)))

(defun pai-mcp-settings (&optional dir)
  "Return the merged `settings' plist across MCP config files for DIR."
  (let ((settings '()))
    (dolist (file (pai-mcp--config-files dir))
      (let ((s (plist-get (pai-mcp--read-json-file file) :settings)))
        (when (pai-mcp--object-p s)
          ;; later files win: overlay their keys
          (let ((p s))
            (while p (setq settings (plist-put settings (car p) (cadr p)))
                   (setq p (cddr p)))))))
    settings))

(defun pai-mcp--server-def (name &optional dir)
  "Return the definition plist for server NAME, or nil."
  (cdr (assoc name (pai-mcp-load-config dir))))

(defun pai-mcp--disabled-p (def)
  "Return non-nil when DEF has `disabled' literally true."
  (eq (plist-get def :disabled) t))

;;;; Environment interpolation

(defun pai-mcp--interpolate (value)
  "Expand ${VAR}, $env:VAR, {env:VAR} and a leading ~ in string VALUE."
  (if (not (stringp value))
      value
    (let ((s value))
      (setq s (replace-regexp-in-string
               "\\${\\([A-Za-z_][A-Za-z0-9_]*\\)}"
               (lambda (m) (or (getenv (match-string 1 m)) "")) s t t))
      (setq s (replace-regexp-in-string
               "\\$env:\\([A-Za-z_][A-Za-z0-9_]*\\)"
               (lambda (m) (or (getenv (match-string 1 m)) "")) s t t))
      (setq s (replace-regexp-in-string
               "{env:\\([A-Za-z_][A-Za-z0-9_]*\\)}"
               (lambda (m) (or (getenv (match-string 1 m)) "")) s t t))
      (if (string-prefix-p "~" s) (expand-file-name s) s))))

(defun pai-mcp--process-env (def)
  "Return the process-environment for DEF: host env plus interpolated `env'.
Honors `inheritEnv': false to drop host variables."
  (let ((extra '())
        (env (plist-get def :env)))
    (while env
      (push (format "%s=%s" (substring (symbol-name (car env)) 1)
                    (if (plist-get def :literalEnv) (cadr env)
                      (pai-mcp--interpolate (cadr env))))
            extra)
      (setq env (cddr env)))
    (if (eq (plist-get def :inheritEnv) :false)
        (append extra (list (concat "PATH=" (or (getenv "PATH") ""))
                            (concat "HOME=" (or (getenv "HOME") ""))))
      (append extra process-environment))))

(provide 'pai-mcp-config)
;;; pai-mcp-config.el ends here
