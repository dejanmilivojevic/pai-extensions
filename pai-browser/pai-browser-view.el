;;; pai-browser-view.el --- Live screenshot view for pai-browser -*- lexical-binding: t; -*-

;;; Commentary:
;; `*pai-browser*' shows the latest screenshot.  With the `live-view' setting
;; on it refreshes on a timer, but only while visible and never with a
;; capture already in flight.

;;; Code:

(require 'subr-x)
(require 'pai-browser-core)

(declare-function pai-browser-screenshot "pai-browser-tools")

(defconst pai-browser-view-buffer "*pai-browser*")

(defvar pai-browser-view--timer nil)
(defvar pai-browser-view--busy nil "Non-nil while a live-view capture runs.")
(defvar pai-browser-view--last nil "(DATA . MIME) of the last screenshot.")
(defvar pai-browser-view--status nil "Last live-view error, or nil.")

(define-derived-mode pai-browser-view-mode special-mode "pai-browser"
  "Live view of the pai-browser page."
  (setq-local cursor-type nil)
  (setq header-line-format '(:eval (pai-browser-view--header))))

(let ((map pai-browser-view-mode-map))
  (define-key map (kbd "g") #'pai-browser-view-refresh)
  (define-key map (kbd "l") #'pai-browser-live-view))

(defun pai-browser-view--header ()
  "Header line text for the view."
  (format " %s · %s · %s%s%s"
          (pai-browser-backend) (pai-browser-get :mode)
          (or pai-browser-last-url "(no page yet)")
          (if pai-browser-view--timer "  [live]" "")
          (if pai-browser-view--status (concat "  ! " pai-browser-view--status) "")))

(defun pai-browser-view--render ()
  "Redraw the view buffer from the last screenshot."
  (when-let ((buf (get-buffer pai-browser-view-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (win (get-buffer-window buf t)))
        (erase-buffer)
        (if (not (and pai-browser-view--last (display-images-p)))
            (insert (if pai-browser-view--last "(images cannot be displayed here)"
                      "No screenshot yet. g: refresh  l: toggle live view"))
          (let* ((type (if (string-match-p "png" (cdr pai-browser-view--last)) 'png 'jpeg))
                 (img (create-image (base64-decode-string (car pai-browser-view--last)) type t
                                    :max-width (and win (window-body-width win t))
                                    :max-height (and win (window-body-height win t)))))
            (insert-image img)))
        (goto-char (point-min))
        (force-mode-line-update)))))

(defun pai-browser-view-update (data mime)
  "Show screenshot DATA (base64) of MIME in the view."
  (setq pai-browser-view--last (cons data mime))
  (pai-browser-view--render))

(defun pai-browser-view--visible-p ()
  "Return non-nil when the view buffer is shown in some window."
  (when-let ((buf (get-buffer pai-browser-view-buffer))) (get-buffer-window buf t)))

(defun pai-browser-view-refresh ()
  "Capture a fresh screenshot into the view (async; skipped while one runs)."
  (interactive)
  (unless pai-browser-view--busy
    (setq pai-browser-view--busy t)
    (let ((done nil))
      ;; Never let a lost callback wedge the live view.
      (run-at-time 60 nil (lambda () (unless done (setq pai-browser-view--busy nil))))
      (condition-case err
          (pai-browser-screenshot
           nil (lambda (img)
                 (setq done t pai-browser-view--busy nil)
                 (if (stringp img) (setq pai-browser-view--status (truncate-string-to-width img 80))
                   (setq pai-browser-view--status nil)
                   (pai-browser-view-update (car img) (cdr img)))))
        (error (setq done t pai-browser-view--busy nil
                     pai-browser-view--status (error-message-string err)))))))

(defun pai-browser-view-request-refresh ()
  "Refresh after an action when the view is visible."
  (when (pai-browser-view--visible-p) (run-at-time 0.3 nil #'pai-browser-view-refresh)))

(defun pai-browser-view--tick ()
  "Timer body for the live view."
  (if (not (get-buffer pai-browser-view-buffer)) (pai-browser-view--stop-timer)
    (when (and (pai-browser-view--visible-p)
               (eq (pai-mcp-server-status (pai-browser-current-server)) 'ready))
      (pai-browser-view-refresh))))

(defun pai-browser-view--stop-timer ()
  "Cancel the live-view timer."
  (when pai-browser-view--timer (cancel-timer pai-browser-view--timer))
  (setq pai-browser-view--timer nil))

(defun pai-browser-view--start-timer ()
  "Start (or restart) the live-view timer from settings."
  (pai-browser-view--stop-timer)
  (let ((secs (max 0.5 (or (pai-browser-get :live-view-interval) 2))))
    (setq pai-browser-view--timer (run-at-time secs secs #'pai-browser-view--tick))))

(defun pai-browser-view-show ()
  "Display the view buffer."
  (interactive)
  (let ((buf (get-buffer-create pai-browser-view-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'pai-browser-view-mode) (pai-browser-view-mode))
      (add-hook 'kill-buffer-hook #'pai-browser-view--stop-timer nil t))
    (display-buffer buf)
    (pai-browser-view--render)
    buf))

(defun pai-browser-live-view (&optional arg)
  "Toggle live refreshing of `*pai-browser*' (show it too).
With positive ARG turn it on, with zero or negative ARG off."
  (interactive "P")
  (let ((on (if arg (> (prefix-numeric-value arg) 0) (not pai-browser-view--timer))))
    (pai-browser-view-show)
    (if on (progn (pai-browser-view--start-timer) (pai-browser-view-refresh))
      (pai-browser-view--stop-timer))
    (message "pai-browser live view %s" (if on "on" "off"))))

(defvar pai-browser-view--setting 'unset
  "The `live-view' setting last applied, so manual toggles survive other changes.")

(defun pai-browser-view-sync-settings ()
  "Start or stop the live view when the `live-view' setting changed."
  (let ((on (pai-truthy (pai-browser-get :live-view))))
    (unless (eq on pai-browser-view--setting)
      (setq pai-browser-view--setting on)
      (if on (pai-browser-view--start-timer) (pai-browser-view--stop-timer)))))

(provide 'pai-browser-view)
;;; pai-browser-view.el ends here
