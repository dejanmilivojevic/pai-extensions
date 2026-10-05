;;; pai-web-pty.el --- Child Emacs for `pai-web-prompt-forwarding-in-a-terminal' -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by an interactive Emacs in a pseudo-terminal (see the test).  It
;; starts the server, connects as a page with curl, and logs what happens to
;; the file named by $PAI_WEB_PTY_LOG:
;;
;;  1. /pick from the page opens a completing-read; the page answers banana.
;;  2. A timer in the chat asks y-or-n-p; the page answers y.
;;  3. You type M-: (read-string "Local: ") in Emacs: not forwarded.
;;  4. /pick from the page again; Emacs answers cherry first; the page is
;;     told the prompt closed.
;;  5. /pick from the page while other code waits in `accept-process-output'
;;     (as a tool does): the prompt must not open inside that wait, and the
;;     page's answer must not unwind it -- it opens when the wait is over.

;;; Code:

(require 'cl-lib)
(setq pai-directory (make-temp-file "pai-web-pty" t))
(require 'pai)
(require 'pai-faux)
(require 'pai-web)

(defvar pai-web-pty--log (or (getenv "PAI_WEB_PTY_LOG") "/tmp/pai-web-pty.log"))
(defvar pai-web-pty--port 18791)
(defvar pai-web-pty--client nil)
(defvar pai-web-pty--ack 0)
(defvar pai-web-pty--answered-pick nil)
(defvar pai-web-pty--emacs-pick nil)
(defvar pai-web-pty--picks 0)

(defun pai-web-pty--log (fmt &rest args)
  "Append FMT with ARGS to the log."
  (write-region (concat (apply #'format fmt args) "\n") nil pai-web-pty--log t 'silent))

(defun pai-web-pty--curl (args callback)
  "Run curl with ARGS; call CALLBACK with its output."
  (let ((out ""))
    (make-process :name "pai-web-pty-curl"
                  :command (append (list "curl" "-s" "-H" "X-Pai: 1") args)
                  :filter (lambda (_p s) (setq out (concat out s)))
                  :sentinel (lambda (_p _e) (funcall callback out)))))

(defun pai-web-pty--url (path)
  "Return the server URL of PATH."
  (format "http://127.0.0.1:%d%s" pai-web-pty--port path))

(defun pai-web-pty--action (json)
  "Post the action JSON (a format string taking the client id first)."
  (pai-web-pty--curl (list "-X" "POST" "-d" json (pai-web-pty--url "/api/action")) #'ignore))

(defun pai-web-pty--on-event (e)
  "Handle page event E."
  (pcase (plist-get e :t)
    ("prompt"
     (let ((p (plist-get e :prompt)))
       (pai-web-pty--log "PROMPT %s %S %S origin=%s" (plist-get p :kind) (plist-get p :text)
                         (plist-get p :candidates) (plist-get p :origin))
       (cond
        ((and (string-prefix-p "Pick" (plist-get p :text)) (= (cl-incf pai-web-pty--picks) 3))
         (pai-web-pty--log "THIRD PICK OPENED")
         (pai-web-pty--action (format "{\"c\":\"%s\",\"a\":\"prompt\",\"id\":\"%s\",\"value\":\"apple\"}"
                                      pai-web-pty--client (plist-get p :id))))
        ((and (string-prefix-p "Pick" (plist-get p :text)) (not pai-web-pty--answered-pick))
         (setq pai-web-pty--answered-pick t)
         (pai-web-pty--action (format "{\"c\":\"%s\",\"a\":\"prompt\",\"id\":\"%s\",\"value\":\"banana\"}"
                                      pai-web-pty--client (plist-get p :id))))
        ((string-prefix-p "Pick" (plist-get p :text))
         (setq pai-web-pty--emacs-pick (plist-get p :id)))
        ((string-prefix-p "Async" (plist-get p :text))
         (pai-web-pty--action (format "{\"c\":\"%s\",\"a\":\"prompt\",\"id\":\"%s\",\"value\":\"y\"}"
                                      pai-web-pty--client (plist-get p :id)))))))
    ("prompt-closed"
     (when (equal (plist-get e :id) pai-web-pty--emacs-pick)
       (pai-web-pty--log "CLOSED-AFTER-EMACS")))
    ("item"
     (let ((item (plist-get e :item)))
       (when (equal (plist-get item :kind) "note")
         (pai-web-pty--log "NOTE %s" (plist-get item :text)))))))

(defun pai-web-pty--poll ()
  "Long-poll for events forever."
  (pai-web-pty--curl
   (list (pai-web-pty--url (format "/api/poll?c=%s&ack=%d" pai-web-pty--client pai-web-pty--ack)))
   (lambda (out)
     (let* ((r (ignore-errors (pai-web-json-read out)))
            (events (plist-get r :events)))
       (when events (setq pai-web-pty--ack (plist-get r :seq)))
       (mapc #'pai-web-pty--on-event events)
       (pai-web-pty--poll)))))

(let ((dir (file-name-as-directory (make-temp-file "pai-web-pty-project" t))))
  (setq pai-default-model "faux")
  (pai-register-command "pick" :handler
                        (lambda (_args _ctx)
                          (list :message (format "picked %s"
                                                 (completing-read "Pick fruit: "
                                                                  '("apple" "banana" "cherry")
                                                                  nil t)))))
  (pai-web-set-setting :port pai-web-pty--port)
  (let ((buf (get-buffer-create "*pai: pty*")))
    (with-current-buffer buf
      (setq default-directory dir)
      (pai--setup dir))
    (switch-to-buffer buf)
    (pai-web-start)
    (pai-web-pty--curl
     (list (pai-web-pty--url "/api/hello"))
     (lambda (out)
       (setq pai-web-pty--client (plist-get (pai-web-json-read out) :client))
       (pai-web-pty--poll)
       (let ((send (format "{\"c\":\"%s\",\"a\":\"send\",\"i\":\"%s\",\"text\":\"/pick\"}"
                           pai-web-pty--client (pai-web-id buf))))
         (run-at-time 0.5 nil (lambda () (pai-web-pty--action send)))
         (run-at-time 2.5 nil (lambda ()
                                (with-current-buffer buf
                                  (pai-web-pty--log "ASYNC %S" (y-or-n-p "Async question? ")))))
         (run-at-time 4.5 nil (lambda ()
                                ;; typed in Emacs: M-: EXPR RET, then the answer
                                (setq unread-command-events
                                      (append (listify-key-sequence (kbd "M-:"))
                                              (string-to-list "(with-current-buffer \"*pai: pty*\" (pai-web-pty--log \"LOCAL %S\" (read-string \"Local: \")))")
                                              (list ?\r)
                                              (string-to-list "hello")
                                              (list ?\r)))))
         (run-at-time 6.5 nil (lambda () (pai-web-pty--action send)))
         (run-at-time 8.0 nil (lambda ()
                                (when (active-minibuffer-window)
                                  (with-selected-window (active-minibuffer-window)
                                    (insert "cherry")
                                    (exit-minibuffer)))))
         (run-at-time 9.5 nil (lambda ()
                                (run-at-time 0.3 nil (lambda () (pai-web-pty--action send)))
                                (pai-web-pty--log "WAIT-START")
                                (let ((end (+ (float-time) 2.5)) (ok nil))
                                  (unwind-protect
                                      (progn (while (< (float-time) end)
                                               (accept-process-output nil 0.05))
                                             (setq ok t))
                                    (pai-web-pty--log "WAIT-END ok=%s" ok)))))
         (run-at-time 15 nil (lambda ()
                                (pai-web-stop)
                                (pai-web-pty--log "DONE")
                                (kill-emacs 0))))))))

;;; pai-web-pty.el ends here
