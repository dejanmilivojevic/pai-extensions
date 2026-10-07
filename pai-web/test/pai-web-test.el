;;; pai-web-test.el --- Tests for the pai-web extension -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch tests for the web server, the bus, logins, the instance bridge,
;; ask_user and remote buffers.  HTTP is exercised with a real server and an
;; Emacs network client.  `pai-web-prompt-forwarding-in-a-terminal' drives a
;; child Emacs in a pseudo-terminal (needs `script'), because the minibuffer
;; does not exist in batch mode.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-ask-user)
(require 'pai-web)

(defconst pai-web-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "This test directory.")

(defvar pai-web-test--port 18790 "Port of the test server.")

(defmacro pai-web-test--with-state (&rest body)
  "Run BODY with pai and pai-web state in a temporary directory."
  (declare (indent 0))
  `(let* ((tmp (file-name-as-directory (make-temp-file "pai-web-test" t)))
          (pai-directory (expand-file-name ".pai" tmp))
          (pai-web--settings-cache nil)
          (pai-web--sessions 'unloaded)
          (pai-web--failures (make-hash-table :test 'equal)))
     (unwind-protect (progn ,@body)
       (when (pai-web-running-p) (pai-web-stop))
       (pai-web-bus-reset)
       (ignore-errors (delete-directory tmp t)))))

(defmacro pai-web-test--with-chat (buf &rest body)
  "Run BODY with BUF a faux-backed pai chat; the web advice installed."
  (declare (indent 1))
  `(pai-web-test--with-state
     (let ((pai-default-model "faux")
           (,buf (generate-new-buffer "*pai-web-test*")))
       (unwind-protect
           (progn
             (pai-ext-reset)
             (with-current-buffer ,buf
               (setq default-directory tmp)
               (pai--setup tmp))
             (pai-web-instances-install)
             ,@body)
         (pai-web-instances-uninstall)
         (when (buffer-live-p ,buf) (kill-buffer ,buf))))))

(defun pai-web-test--wait (pred &optional seconds)
  "Pump events until PRED is non-nil or SECONDS (default 5) pass."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall pred)))

(defun pai-web-test--events (client)
  "Return CLIENT's queued events as plists, oldest first."
  (mapcar (lambda (e) (pai-web-json-read (cdr e)))
          (reverse (pai-web-client-queue client))))

;;;; HTTP client

(defun pai-web-test--request (method path &optional body headers)
  "Send an HTTP request to the test server; return (STATUS HEADERS BODY)."
  (let* ((out (list ""))
         (proc (make-network-process
                :name "pai-web-test-client" :host "127.0.0.1"
                :service pai-web-test--port :coding 'binary
                :filter (lambda (_p s) (setcar out (concat (car out) s)))))
         (body (and body (encode-coding-string body 'utf-8))))
    (process-send-string
     proc (concat (format "%s %s HTTP/1.1\r\nHost: 127.0.0.1:%d\r\n" method path
                          pai-web-test--port)
                  (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h))) headers "")
                  (if body (format "Content-Length: %d\r\n" (length body)) "")
                  "\r\n" (or body "")))
    (pai-web-test--wait
     (lambda ()
       (let ((s (car out)))
         (and (string-match "\r\n\r\n" s)
              (let ((head-end (match-end 0)))
                (and (string-match "Content-Length: \\([0-9]+\\)" s)
                     (>= (- (length s) head-end)
                         (string-to-number (match-string 1 s)))))))))
    (delete-process proc)
    (let* ((s (car out))
           (split (string-search "\r\n\r\n" s))
           (head (substring s 0 split)))
      (list (string-to-number (cadr (split-string head " ")))
            head
            (decode-coding-string (substring s (+ split 4)) 'utf-8)))))

(defun pai-web-test--start ()
  "Start the server on the test port."
  (pai-web-set-setting :port pai-web-test--port)
  (pai-web-start))

;;;; Utilities

(ert-deftest pai-web-json-makes-strings-valid ()
  (should (equal (pai-web-json (list :a (string-to-multibyte "ok\377") :b [1 2] :c nil :d :false))
                 "{\"a\":\"ok\uFFFD\",\"b\":[1,2],\"c\":null,\"d\":false}"))
  (should (equal (pai-web-json-read "{\"x\":[1,{\"y\":false}],\"z\":null}")
                 '(:x (1 (:y :false)) :z nil))))

(ert-deftest pai-web-html-escapes-and-styles ()
  (should (equal (pai-web-html-escape "<a href=\"x\">&</a>")
                 "&lt;a href=&quot;x&quot;&gt;&amp;&lt;/a&gt;"))
  (let ((s (concat "plain " (propertize "<b>" 'face '(:foreground "#ff0000" :weight bold)))))
    (should (equal (pai-web-propertized-html s)
                   "plain <span style=\"color:#ff0000;font-weight:bold\">&lt;b&gt;</span>")))
  (should (equal (pai-web-propertized-html (concat "a" (propertize "hidden" 'invisible t) "b"))
                 "ab")))

(ert-deftest pai-web-html-honours-display-and-glyph-widths ()
  "Column layouts (the /context grid, aligned settings) survive in HTML."
  ;; a display string replaces the text
  (should (equal (pai-web-propertized-html (concat "a" (propertize "xyz" 'display "B") "c"))
                 "aBc"))
  ;; space specs become fixed-width boxes; :align-to counts from the column
  (should (string-match-p "width:3\\.00ch"
                          (pai-web-propertized-html (propertize " " 'display '(space :width 3)))))
  (should (string-match-p "width:6\\.00ch"
                          (pai-web-propertized-html
                           (concat "abcd" (propertize " " 'display '(space :align-to 10))))))
  ;; after a newline the column starts again
  (should (string-match-p "width:8\\.00ch"
                          (pai-web-propertized-html
                           (concat "abcd\nxy" (propertize " " 'display '(space :align-to 10))))))
  ;; symbol glyphs are boxed to their Emacs width
  (should (string-match-p "<span class=\"g\" style=\"[^\"]*width:1ch\">⛁</span>"
                          (pai-web-propertized-html "⛁ x"))))

(ert-deftest pai-web-random-hex-has-the-asked-length ()
  (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" (pai-web-random-hex 32)))
  (should-not (equal (pai-web-random-hex 16) (pai-web-random-hex 16))))

;;;; HTTP

(ert-deftest pai-web-http-parses-requests ()
  (let ((head (pai-web-http--parse-head
               "POST /api/x%20y?a=1&b=caf%C3%A9+ok HTTP/1.1\r\nHost: h\r\nContent-Length: 12\r\nCookie: a=1; pai_web=tok")))
    (should (equal (plist-get head :method) "POST"))
    (should (equal (plist-get head :path) "/api/x y"))
    (should (equal (plist-get head :query) '(("a" . "1") ("b" . "café ok"))))
    (should (equal (plist-get head :length) 12))
    (should (equal (pai-web-http-cookie (list :headers (plist-get head :headers)) "pai_web") "tok"))))

(ert-deftest pai-web-server-serves-the-page-and-guards-requests ()
  (pai-web-test--with-state
    (pai-web-test--start)
    (should (pai-web-running-p))
    ;; one server per Emacs: starting again reports the running one
    (should (string-match-p "running" (pai-web-start)))
    (should (equal (car (pai-web-test--request "GET" "/")) 200))
    (should (equal (car (pai-web-test--request "GET" "/static/app.js")) 200))
    (should (equal (car (pai-web-test--request "GET" "/static/../pai-web.el")) 404))
    ;; POST needs the custom header (cross-site forms cannot send it)
    (should (equal (car (pai-web-test--request "POST" "/api/action" "{}")) 403))
    ;; an unknown Host is refused (DNS rebinding)
    (let ((proc-host (pai-web-host-allowed-p
                      (list :headers '(("host" . "evil.example:8765"))))))
      (should-not proc-host))
    (should (pai-web-host-allowed-p (list :headers '(("host" . "192.168.1.170:8765")))))
    (should (pai-web-host-allowed-p (list :headers '(("host" . "localhost:8765")))))
    (pai-web-stop)
    (should-not (pai-web-running-p))))

(ert-deftest pai-web-every-static-file-fits-one-response ()
  "Static files are written in one go, so each must stay below the cap."
  (dolist (f (directory-files pai-web-static-directory t "\\`[^.]"))
    (should (< (file-attribute-size (file-attributes f)) pai-web-http-max-response))))

;;;; Logins

(ert-deftest pai-web-password-login-and-sessions ()
  (pai-web-test--with-state
    (should-not (pai-web-password-set-p))
    (should (eq (pai-web-request-session (list :headers nil)) 'open))
    (pai-web-set-password "correct horse")
    (should (pai-web-password-set-p))
    (should (pai-web-check-password "correct horse"))
    (should-not (pai-web-check-password "wrong"))
    ;; the password itself is never stored
    (should-not (string-match-p "correct horse"
                                (with-temp-buffer
                                  (insert-file-contents (pai-settings-global-file))
                                  (buffer-string))))
    (let ((token (pai-web-session-create)))
      (should (pai-web-session-valid-p token))
      (should (pai-web-request-session
               (list :headers `(("cookie" . ,(format "pai_web=%s" token))))))
      ;; a new password logs everybody out
      (pai-web-set-password "another one")
      (should-not (pai-web-session-valid-p token)))))

(ert-deftest pai-web-server-binds-by-password ()
  (pai-web-test--with-state
    (pai-web-test--start)
    (should (equal pai-web--host "127.0.0.1"))
    (pai-web-stop)
    (pai-web-set-password "correct horse")
    (pai-web-test--start)
    (should (equal pai-web--host "0.0.0.0"))
    (should (equal (car (pai-web-test--request "GET" "/api/hello")) 401))
    (let* ((login (pai-web-test--request "POST" "/api/login" "{\"password\":\"correct horse\"}"
                                         '(("X-Pai" . "1"))))
           (cookie (and (string-match "Set-Cookie: \\(pai_web=[0-9a-f]+\\)" (nth 1 login))
                        (match-string 1 (nth 1 login)))))
      (should (equal (car login) 200))
      (should cookie)
      (should (string-match-p "HttpOnly; SameSite=Strict" (nth 1 login)))
      (should (equal (car (pai-web-test--request "GET" "/api/hello" nil `(("Cookie" . ,cookie)))) 200)))))

(ert-deftest pai-web-failed-logins-lock-out ()
  (pai-web-test--with-state
    (dotimes (_ pai-web-lockout-failures) (pai-web-note-failure "1.2.3.4"))
    (should (pai-web-locked-out-p "1.2.3.4"))
    (should-not (pai-web-locked-out-p "5.6.7.8"))))

;;;; Bus

(ert-deftest pai-web-long-poll-gets-broadcast-events ()
  (pai-web-test--with-state
    (pai-web-test--start)
    (let* ((hello (pai-web-json-read (nth 2 (pai-web-test--request "GET" "/api/hello"))))
           (client (plist-get hello :client))
           (result nil))
      (should client)
      ;; the poll is held, then answered when something happens
      (run-at-time 0.2 nil (lambda () (pai-web-bus-broadcast '(:t "toast" :text "héllo"))))
      (setq result (pai-web-json-read
                    (nth 2 (pai-web-test--request "GET" (format "/api/poll?c=%s&ack=0" client)))))
      (should (seq-find (lambda (e) (equal (plist-get e :text) "héllo"))
                        (plist-get result :events)))
      ;; acknowledged events are not sent again
      (pai-web-bus--ack (pai-web-bus-client client) (plist-get result :seq))
      (should-not (pai-web-client-queue (pai-web-bus-client client))))))

(ert-deftest pai-web-big-payloads-become-blobs ()
  (pai-web-test--with-state
    (let ((client (pai-web-bus-new-client 'open)))
      (pai-web-bus-broadcast (list :t "toast" :text (make-string 50000 ?x)))
      (let ((e (car (pai-web-test--events client))))
        (should (equal (plist-get e :t) "blob"))
        (let ((bytes (car (gethash (plist-get e :blob) pai-web-bus--blobs))))
          (should (> (length bytes) 50000))
          (should (equal (plist-get (pai-web-json-read bytes) :t) "toast")))))))

(ert-deftest pai-web-a-silent-client-is-resynced-not-grown ()
  (pai-web-test--with-state
    (let ((client (pai-web-bus-new-client 'open))
          (pai-web-bus-queue-limit 2000))
      (dotimes (i 200) (pai-web-bus-broadcast (list :t "toast" :text (format "event %d" i))))
      (should (< (pai-web-client-bytes client) 2000))
      (should (seq-find (lambda (e) (equal (plist-get e :t) "resync"))
                        (pai-web-test--events client))))))

;;;; Instances

(ert-deftest pai-web-transcript-log-follows-the-chat ()
  (pai-faux-reset)
  (pai-faux-push '(:thinking "hmm" :text "Hello **world**\n\n```elisp\n(+ 1 2)\n```\n"
                   :stop-reason stop))
  (pai-web-test--with-chat buf
    (let ((client (pai-web-bus-new-client 'open)))
      (pai-web-send buf "hi there" nil)
      (pai-web-test--wait (lambda () (not (buffer-local-value 'pai--active buf))))
      (pai-web--send-deltas)
      (let* ((snap (pai-web-log-snapshot buf))
             (items (append (plist-get snap :items) nil)))
        (should (equal (mapcar (lambda (i) (plist-get i :kind)) items)
                       '("user" "assistant" "note")))
        (should (equal (plist-get (nth 0 items) :text) "hi there"))
        (let ((blocks (append (plist-get (nth 1 items) :blocks) nil)))
          (should (equal (plist-get (nth 0 blocks) :type) "thinking"))
          (should (string-match-p "Hello \\*\\*world" (plist-get (nth 1 blocks) :text))))
        (should (= (length (plist-get (nth 1 items) :fences)) 1)))
      ;; streamed text went out as deltas, before the final item
      (let ((kinds (mapcar (lambda (e) (plist-get e :t)) (pai-web-test--events client))))
        (should (member "delta" kinds))
        (should (< (cl-position "delta" kinds :test #'equal)
                   (cl-position "item" kinds :test #'equal :from-end t))))
      ;; clearing the transcript starts a new generation
      (let ((gen (plist-get (pai-web-log-snapshot buf) :g)))
        (with-current-buffer buf (pai-clear))
        (should (> (plist-get (pai-web-log-snapshot buf) :g) gen))
        (should (= (length (plist-get (pai-web-log-snapshot buf) :items)) 0))))))

(ert-deftest pai-web-tool-calls-become-tool-items ()
  (pai-faux-reset)
  (pai-faux-push '(:tool-calls ((:id "c1" :name "elisp_eval" :arguments (:form "(+ 1 2)")))
                  :stop-reason tool-use)
                 '(:text "The answer is 3." :stop-reason stop))
  (pai-web-test--with-chat buf
    (pai-web-send buf "add" nil)
    (pai-web-test--wait (lambda () (not (buffer-local-value 'pai--active buf))))
    (let* ((items (append (plist-get (pai-web-log-snapshot buf) :items) nil))
           (tool (seq-find (lambda (i) (equal (plist-get i :kind) "tool")) items)))
      (should tool)
      (should (equal (plist-get tool :name) "elisp_eval"))
      (should (equal (plist-get tool :status) "done"))
      (should (string-match-p "3" (plist-get tool :result)))
      (should (string-match-p "(\\+ 1 2)" (plist-get tool :args)))
      (should (string-match-p "3" (plist-get (pai-web-tool-detail buf (plist-get tool :id)) :result))))))

(ert-deftest pai-web-raw-transcript-inserts-become-notes ()
  "Text extensions insert with `pai--insert' (not a note) reaches the page."
  (pai-web-test--with-chat buf
    (with-current-buffer buf
      (pai--ensure-fresh-line)              ; a lone newline is no item
      (pai--insert "\n")
      (should (= (length (plist-get (pai-web-log-snapshot buf) :items)) 0))
      (pai--insert (concat "Context " (propertize "used" 'face '(:foreground "#ff0000")) "\n"))
      (pai--insert "second line\n")
      (let ((items (append (plist-get (pai-web-log-snapshot buf) :items) nil)))
        ;; consecutive inserts are one note, faces kept
        (should (= (length items) 1))
        (should (equal (plist-get (car items) :text) "Context used\nsecond line"))
        (should (string-match-p "color:#ff0000\">used<" (plist-get (car items) :html))))
      ;; a note in between starts a new one; rendered notes are not doubled
      (pai--render-note "a note")
      (pai--insert "third")
      (should (equal (mapcar (lambda (i) (plist-get i :text))
                             (plist-get (pai-web-log-snapshot buf) :items))
                     '("Context used\nsecond line" "a note" "third"))))))

(ert-deftest pai-web-shows-the-context-command ()
  (require 'pai-context)
  (pai-web-test--with-chat buf
    (with-current-buffer buf
      (should (null (pai-context-command "" (pai--ext-context)))))
    (let ((items (append (plist-get (pai-web-log-snapshot buf) :items) nil)))
      (should (= (length items) 1))
      (should (string-match-p "System prompt\\|Messages\\|Tool" (plist-get (car items) :text)))
      (should (string-match-p "style=" (plist-get (car items) :html))))))

(ert-deftest pai-web-log-is-built-from-an-existing-context ()
  (pai-faux-reset)
  (pai-faux-push '(:text "first answer" :stop-reason stop))
  (pai-web-test--with-chat buf
    (with-current-buffer buf (goto-char (point-max)) (insert "before the server") (pai-send))
    (pai-web-test--wait (lambda () (not (buffer-local-value 'pai--active buf))))
    (clrhash pai-web--logs)             ; as if the server started now
    (let ((items (append (plist-get (pai-web-log-snapshot buf) :items) nil)))
      (should (equal (mapcar (lambda (i) (plist-get i :kind)) items) '("user" "assistant")))
      (should (equal (plist-get (car items) :text) "before the server")))))

(ert-deftest pai-web-instance-info-and-chrome ()
  (pai-web-test--with-chat buf
    (let ((info (pai-web-instance-info buf))
          (chrome (pai-web-instance-chrome buf)))
      (should (string-prefix-p "i" (plist-get info :id)))
      (should (eq (pai-web-instance (plist-get info :id)) buf))
      (should (equal (plist-get info :model) "faux/faux"))
      (should (eq (plist-get info :active) :false))
      (should (plist-member chrome :above))
      (should (member (plist-get info :id)
                      (mapcar (lambda (i) (plist-get i :id)) (pai-web-instance-list)))))))

(ert-deftest pai-web-footer-is-shown-once ()
  "The footer (memory, todo widgets) is sent once, wherever it is drawn."
  (pai-web-test--with-chat buf
    (with-current-buffer buf
      (pai--set-widget "memory" "MEMWIDGET")
      (let ((count (lambda ()
                     (let ((c (pai-web-instance-chrome buf)) (n 0))
                       (dolist (k '(:above :footer))
                         (let ((s (plist-get c k)) (start 0))
                           (while (string-match "MEMWIDGET" s start)
                             (setq n (1+ n) start (match-end 0)))))
                       n))))
        (pai-settings-set :footer-position "above-prompt")
        (pai--refresh-footer)
        (should (= (funcall count) 1))
        (should (string-match-p "MEMWIDGET" (plist-get (pai-web-instance-chrome buf) :above)))
        (pai-settings-set :footer-position "mode-line")
        (pai--refresh-footer)
        (should (= (funcall count) 1))
        (should (string-match-p "MEMWIDGET" (plist-get (pai-web-instance-chrome buf) :footer)))))))

(ert-deftest pai-web-send-keeps-the-emacs-draft-and-attaches-images ()
  (pai-faux-reset)
  (pai-faux-push '(:text "seen" :stop-reason stop))
  (pai-web-test--with-chat buf
    (with-current-buffer buf (goto-char (point-max)) (insert "my draft"))
    (pai-web-send buf "look" (list (list :data (base64-encode-string "GIF89a\1\0\1\0")
                                         :mime "image/gif")
                                   (list :data "x" :mime "text/html")))
    (pai-web-test--wait (lambda () (not (buffer-local-value 'pai--active buf))))
    (with-current-buffer buf
      (should (equal (pai--input-text) "my draft"))
      (let ((user (seq-find #'pai-user-message-p (reverse pai--context-messages))))
        ;; only real image types are accepted
        (should (equal (mapcar #'pai-block-type (pai-message-content user)) '(text image)))))))

(ert-deftest pai-web-completes-like-the-chat-buffer ()
  (pai-web-test--with-chat buf
    (with-current-buffer buf (goto-char (point-max)) (insert "draft"))
    (let ((r (pai-web-complete buf "/mo" 3)))
      (should (equal (plist-get r :beg) 1))
      (should (seq-find (lambda (i) (equal (plist-get i :v) "model")) (plist-get r :items))))
    ;; arguments at every level, here the /web subcommands
    (let ((r (pai-web-complete buf "/web " 5)))
      (should (equal (sort (mapcar (lambda (i) (plist-get i :v)) (plist-get r :items)) #'string<)
                     (sort (list "start" "stop" "restart" "status" "open" "password" "logout-all")
                           #'string<))))
    (should (equal (mapcar (lambda (i) (plist-get i :v))
                           (plist-get (pai-web-complete buf "/web password " 14) :items))
                   '("clear")))
    ;; the user's own input is left as it was
    (should (equal (with-current-buffer buf (pai--input-text)) "draft"))))

(ert-deftest pai-web-completes-file-names-like-emacs ()
  (let ((dir (file-name-as-directory (make-temp-file "pai-web-files" t))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "lisp/sub" dir) t)
          (dolist (f '("lisp/pai-ui.el" "lisp/pai-util.el" "lisp/other.el" "top.el"))
            (write-region "" nil (expand-file-name f dir)))
          (pai-web-test--with-chat buf
            (with-current-buffer buf (setq default-directory dir))
            ;; only the last component is replaced, the directory stays
            (let ((r (pai-web-complete buf "see @lisp/pai-u" 15)))
              (should (equal (plist-get r :beg) 10))
              (should (equal (plist-get r :end) 15))
              (should (equal (mapcar (lambda (i) (plist-get i :v)) (plist-get r :items))
                             '("pai-ui.el" "pai-util.el"))))
            ;; completion styles apply (partial-completion here)
            (let ((completion-styles '(basic partial-completion)))
              (should (member "pai-util.el"
                              (mapcar (lambda (i) (plist-get i :v))
                                      (plist-get (pai-web-complete buf "@lisp/p-ut" 10) :items)))))
            ;; directories are offered with their slash, to drill down
            (should (member "sub/" (mapcar (lambda (i) (plist-get i :v))
                                           (plist-get (pai-web-complete buf "@lisp/" 6) :items)))))
          ;; a read-file-name prompt: candidates complete the input after its base
          (let* ((p (pai-web-prompt--create :id "x" :table #'read-file-name-internal
                                            :initial (concat dir "lisp/pai-u")))
                 (default-directory dir)
                 (pai-web--prompts (list p))
                 (r (pai-web-prompt-complete "x" (concat dir "lisp/pai-u"))))
            (should (equal (plist-get r :base) (concat dir "lisp/")))
            (should (equal (plist-get r :candidates) ["pai-ui.el" "pai-util.el"]))
            (let ((json (pai-web-prompt-json p)))
              (should (equal (plist-get json :base) (concat dir "lisp/")))
              (should (eq (plist-get json :dynamic) t)))))
      (delete-directory dir t))))

(ert-deftest pai-web-uploads-are-saved-safely ()
  (pai-web-test--with-chat buf
    (let ((r (pai-web-upload buf "../../etc/my file.txt" "data")))
      (should (string-match-p "my_file\\.txt\\'" (plist-get r :path)))
      (should (string-prefix-p "@" (plist-get r :mention)))
      (should (file-exists-p (expand-file-name (plist-get r :path) tmp)))
      (should-not (file-exists-p (expand-file-name "etc/my file.txt" tmp))))
    (pai-web-set-setting :upload-mode "project")
    (pai-web-set-setting :upload-subdir "inbox")
    (let ((r (pai-web-upload buf "a.txt" "1")))
      (should (equal (plist-get r :path) "inbox/a.txt"))
      (should (equal (plist-get (pai-web-upload buf "a.txt" "2") :path) "inbox/a-1.txt")))))

;;;; Prompts

(ert-deftest pai-web-forwarding-rules ()
  (pai-web-test--with-chat buf
    (let ((other (generate-new-buffer "unrelated")))
      (unwind-protect
          (progn
            ;; nobody connected: nothing is forwarded
            (let ((pai-web--origin t)) (should-not (pai-web--forward-p buf)))
            (pai-web-bus-new-client 'open)
            (let ((pai-web--origin t)) (should (pai-web--forward-p other)))
            (let ((this-command nil)) (should (pai-web--forward-p buf)))
            ;; typed in Emacs: stays in Emacs
            (let ((this-command 'pai-send)) (should-not (pai-web--forward-p buf)))
            (let ((this-command nil)) (should-not (pai-web--forward-p other))))
        (kill-buffer other)))))

(ert-deftest pai-web-prompt-forwarding-in-a-terminal ()
  "Drive a real minibuffer in a child Emacs: answers from the page and Emacs."
  (skip-unless (and (executable-find "script") (executable-find "curl")))
  (let* ((log (make-temp-file "pai-web-pty" nil ".log"))
         (root (expand-file-name "../../.." pai-web-test--dir))
         (cmd (format "TERM=xterm %s -Q -nw -L %s -L %s -L %s -L %s -L %s -L %s -l %s"
                      (shell-quote-argument (expand-file-name invocation-name invocation-directory))
                      (shell-quote-argument (expand-file-name "lisp" root))
                      (shell-quote-argument (expand-file-name "test" root))
                      (shell-quote-argument (expand-file-name ".." pai-web-test--dir))
                      (shell-quote-argument (expand-file-name "../../pai-ask-user" pai-web-test--dir))
                      (shell-quote-argument (expand-file-name "vendor/vui" root))
                      (shell-quote-argument pai-web-test--dir)
                      (shell-quote-argument (expand-file-name "pai-web-pty.el" pai-web-test--dir))))
         (process-environment (cons (concat "PAI_WEB_PTY_LOG=" log) process-environment)))
    (unwind-protect
        (progn
          (call-process "timeout" nil nil nil "60" "script" "-qc" cmd "/dev/null")
          (let ((out (with-temp-buffer (insert-file-contents log) (buffer-string))))
            ;; a prompt opened by the page is answered by the page
            (should (string-match-p "PROMPT completion \"Pick fruit: \" (\"apple\" \"banana\" \"cherry\") origin=t" out))
            (should (string-match-p "NOTE picked banana" out))
            ;; an agent callback's y-or-n-p is forwarded and answered
            (should (string-match-p "PROMPT y-or-n \"Async question\\? (y or n) \"" out))
            (should (string-match-p "ASYNC t" out))
            ;; answered in Emacs first: the page is told it closed
            (should (string-match-p "NOTE picked cherry" out))
            (should (string-match-p "CLOSED-AFTER-EMACS" out))
            ;; typed in Emacs: never forwarded
            (should-not (string-match-p "PROMPT text \"Local: \"" out))
            (should (string-match-p "LOCAL \"hello\"" out))
            ;; a page action never runs inside other code's wait, and its
            ;; answer never unwinds that code (it once crashed Emacs)
            (should (string-match-p "WAIT-END ok=t\\(.\\|\n\\)*THIRD PICK OPENED\\(.\\|\n\\)*NOTE picked apple" out))
            (should (string-match-p "DONE" out))))
      (delete-file log))))

;;;; ask_user

(defmacro pai-web-test--with-ask-ui (&rest body)
  "Run BODY as in an interactive session without touching windows."
  (declare (indent 0))
  `(let ((noninteractive nil)
         (vui-render-delay nil)
         (pai-ask-user-select-window nil)
         (pai-ask-user-return-focus nil)
         (pai-ask-user-timeout nil)
         (pai-ask-user-display-action '(display-buffer-no-window (allow-no-window . t))))
     (unwind-protect (progn ,@body)
       (pai-ask-user-cancel-all))))

(ert-deftest pai-web-answers-ask-user-questions ()
  (pai-web-test--with-chat buf
    (pai-web-test--with-ask-ui
      (let* ((cell (list nil))
             (req (with-current-buffer buf
                    (pai-ask-user--execute
                     (list :question "Which way?"
                           :options (list (list :label "Left") (list :label "Right")))
                     (list :tool-call-id (pai-uuidv7)) nil
                     (lambda (result) (setcar cell result)))))
             (asks (append (pai-web-asks) nil)))
        (should (= (length asks) 1))
        (should (equal (plist-get (car asks) :question) "Which way?"))
        (should (equal (plist-get (car asks) :mode) "single-select"))
        (should (equal (mapcar (lambda (o) (plist-get o :label)) (plist-get (car asks) :options))
                       '("Left" "Right")))
        (should (null (pai-web-ask-answer (pai-ask-user-request-id req) '(2) nil nil nil)))
        (should (string-match-p "2\\. Right" (pai-content-text (plist-get (car cell) :content))))
        (should (equal (pai-web-asks) []))
        (should (stringp (pai-web-ask-answer (pai-ask-user-request-id req) '(1) nil nil nil)))))))

(ert-deftest pai-web-answers-free-form-and-cancels ()
  (pai-web-test--with-chat buf
    (pai-web-test--with-ask-ui
      (let* ((cell (list nil))
             (ask (lambda (q) (with-current-buffer buf
                                (pai-ask-user--execute (list :question q)
                                                       (list :tool-call-id (pai-uuidv7)) nil
                                                       (lambda (result) (setcar cell result))))))
             (req (funcall ask "Name it?")))
        (pai-web-ask-answer (pai-ask-user-request-id req) nil nil "Zebra" nil)
        (should (string-match-p "Zebra" (pai-content-text (plist-get (car cell) :content))))
        (setq req (funcall ask "Again?"))
        (pai-web-ask-answer (pai-ask-user-request-id req) nil nil nil t)
        (should (equal (plist-get (plist-get (car cell) :details) :status) "cancelled"))))))

;;;; Remote buffers

(defmacro pai-web-test--with-remote (buf &rest body)
  "Run BODY with BUF a pai-looking buffer holding a button and a field."
  (declare (indent 1))
  `(let ((,buf (generate-new-buffer "*pai remote test*"))
         (pressed nil))
     (unwind-protect
         (progn
           (with-current-buffer ,buf
             (insert (propertize "Title" 'face '(:weight bold)) "\n")
             (insert-text-button "[Press]" 'action (lambda (_b) (setq pressed t)))
             (insert "\nName: ")
             (widget-create 'editable-field :size 10 "old")
             (widget-setup)
             (local-set-key (kbd "C-c C-c") (lambda () (interactive) (setq pressed 'key)))
             (goto-char (point-min)))
           ,@body)
       (kill-buffer ,buf))))

(ert-deftest pai-web-remote-buffer-renders-faces-buttons-and-fields ()
  (pai-web-test--with-remote buf
    (should (pai-web-buffer-related-p buf))
    (should-not (pai-web-buffer-related-p (get-buffer-create "*scratch*")))
    (let ((html (plist-get (pai-web-buffer-render buf) :html)))
      (should (string-match-p "font-weight:bold\">Title<" html))
      (should (string-match-p "class=\"button\"[^>]*>\\[Press\\]<" html))
      (should (string-match-p "class=\"field\"" html))
      (should (string-match-p "<span class=\"pt\"></span>" html)))))

(ert-deftest pai-web-remote-buffer-actions ()
  (pai-web-test--with-remote buf
    (pai-web-buffer-click buf (with-current-buffer buf (button-start (next-button (point-min)))))
    (should (eq pressed t))
    (should (equal (pai-web-buffer-key-kind buf "C-c") "prefix"))
    (should (equal (pai-web-buffer-key-kind buf "C-c C-c") "command"))
    (should (equal (pai-web-buffer-key-kind buf "C-c C-z") "undefined"))
    (should (null (pai-web-buffer-key buf "C-c C-c")))
    (should (eq pressed 'key))
    (should (stringp (pai-web-buffer-key buf "C-c C-z")))
    (let ((field-pos (with-current-buffer buf
                       (goto-char (point-min)) (search-forward "Name: ") (point))))
      (should (equal (plist-get (pai-web-buffer-field buf field-pos) :value) "old"))
      (should (null (pai-web-buffer-set-field buf field-pos "new")))
      (should (equal (plist-get (pai-web-buffer-field buf field-pos) :value) "new")))
    (with-current-buffer buf
      (let ((sig (pai-web-buffer-signature buf)))
        (pai-web-buffer-goto buf 3)
        (should (= (point) 3))
        (should-not (equal sig (pai-web-buffer-signature buf)))))))

;;;; Command

(ert-deftest pai-web-command-is-registered-with-completion ()
  (let ((pai--commands (make-hash-table :test 'equal)))
    (pai-web-extension (pai-ext-api-create :id "web"))
    (let ((cmd (pai-command-get "web")))
      (should cmd)
      (should (member "logout-all" (funcall (plist-get cmd :arg-completions) ""))))))

(ert-deftest pai-web-command-status-and-stop ()
  (pai-web-test--with-state
    (should (string-match-p "stopped" (plist-get (pai-web-command "status" nil) :message)))
    (pai-web-set-setting :port pai-web-test--port)
    (should (string-match-p "running" (plist-get (pai-web-command "start" nil) :message)))
    (should (string-match-p "stopped" (plist-get (pai-web-command "stop" nil) :message)))
    (should-not (pai-web-running-p))))

(ert-deftest pai-web-auto-start-only-when-enabled ()
  (pai-web-test--with-state
    (pai-web-set-setting :port pai-web-test--port)
    (pai-web--auto-start)
    (should-not (pai-web-running-p))
    (pai-web-set-setting :auto-start t)
    (pai-web--auto-start)
    (should (pai-web-running-p))))

(provide 'pai-web-test)
;;; pai-web-test.el ends here
