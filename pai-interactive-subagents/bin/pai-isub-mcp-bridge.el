;;; pai-isub-mcp-bridge.el --- stdio <-> pai MCP relay -*- lexical-binding: t; -*-

;;; Commentary:

;; Run by an external subagent as its stdio MCP server:
;;
;;   emacs -Q --batch -l pai-isub-mcp-bridge.el PORT TOKEN
;;
;; Relays every JSON-RPC line from stdin to the pai Emacs listening on
;; 127.0.0.1:PORT (prefixed with TOKEN and a tab) and prints the answer to
;; requests on stdout.  Notifications get no answer.  See `pai-isub-mcp'.

;;; Code:

(defvar pai-isub-bridge--lines nil "Complete response lines received, oldest first.")
(defvar pai-isub-bridge--pending "" "Partial response text.")

(defun pai-isub-bridge--filter (_proc string)
  "Collect complete lines of STRING."
  (setq pai-isub-bridge--pending (concat pai-isub-bridge--pending string))
  (while (string-match "\n" pai-isub-bridge--pending)
    (setq pai-isub-bridge--lines
          (append pai-isub-bridge--lines
                  (list (substring pai-isub-bridge--pending 0 (match-beginning 0))))
          pai-isub-bridge--pending (substring pai-isub-bridge--pending (match-end 0)))))

(defun pai-isub-bridge--request-p (line)
  "Return non-nil when LINE is a JSON-RPC request (it has an id)."
  (let ((obj (ignore-errors (json-parse-string line :object-type 'alist))))
    (and (consp obj) (assq 'id obj) (assq 'method obj) t)))

(defun pai-isub-bridge-main ()
  "Relay stdin to the pai server until stdin closes."
  (let* ((port (string-to-number (or (nth 0 command-line-args-left) "0")))
         (token (or (nth 1 command-line-args-left) ""))
         (proc (make-network-process :name "pai-bridge" :host "127.0.0.1" :service port
                                     :family 'ipv4 :coding 'utf-8-unix :noquery t
                                     :filter #'pai-isub-bridge--filter)))
    (setq command-line-args-left nil)
    (while t
      (let ((line (condition-case nil (read-from-minibuffer "")
                    (error (kill-emacs 0)))))
        (setq line (string-trim line))
        (unless (string-empty-p line)
          (process-send-string proc (concat token "\t" line "\n"))
          (when (pai-isub-bridge--request-p line)
            (while (and (null pai-isub-bridge--lines) (process-live-p proc))
              (accept-process-output proc 1))
            (unless pai-isub-bridge--lines (kill-emacs 1))
            (princ (concat (pop pai-isub-bridge--lines) "\n"))))))))

(when noninteractive
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8-unix)
  (setq locale-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (pai-isub-bridge-main))

;;; pai-isub-mcp-bridge.el ends here
