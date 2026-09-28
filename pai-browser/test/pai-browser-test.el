;;; pai-browser-test.el --- Tests for pai-browser -*- lexical-binding: t; -*-

;;; Commentary:
;; No real browser: settings merge, server argv, per-backend translation,
;; JS builders, result decoding and the next-message stash.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai-browser)

(defmacro pai-browser-test--with-settings (global project &rest body)
  "Run BODY with GLOBAL and PROJECT `pai-browser' sections."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'pai-settings-scope-value)
              (lambda (key scope)
                (when (eq key :pai-browser) (if (eq scope 'project) ,project ,global)))))
     ,@body))

(ert-deftest pai-browser-settings-deep-merge ()
  (pai-browser-test--with-settings
      '(:backend "chrome-devtools" :packages (:playwright "@playwright/mcp@9"))
      '(:mode "headless" :packages (:chrome-devtools "cdt@1"))
    (let ((s (pai-browser-settings)))
      (should (equal (plist-get s :backend) "chrome-devtools"))
      (should (equal (plist-get s :mode) "headless"))
      (should (equal (plist-get s :viewport) "1280x800"))
      (should (equal (plist-get (plist-get s :packages) :playwright) "@playwright/mcp@9"))
      (should (equal (plist-get (plist-get s :packages) :chrome-devtools) "cdt@1")))))

(ert-deftest pai-browser-playwright-argv ()
  (let ((root (make-temp-file "pb-root" t)))
    (unwind-protect
        (pai-browser-test--with-settings (list :profile-root root :mode "headless" :npx "/opt/bin/npx") nil
          (let* ((def (pai-browser-server-def))
                 (args (plist-get def :args)))
            (should (equal (plist-get def :command) "/bin/sh"))
            (should (equal (seq-take args 5) '("-c" "exec \"$0\" \"$@\" 2>>\"$PAI_BROWSER_LOG\""
                                               "/opt/bin/npx" "-y" "@playwright/mcp@0.0.82")))
            (should (string-suffix-p "playwright.log" (plist-get (plist-get def :env) :PAI_BROWSER_LOG)))
            (should (member "--headless" args))
            (should (equal (cadr (member "--browser" args)) "msedge"))
            (should (string-prefix-p root (cadr (member "--user-data-dir" args))))
            (should (file-exists-p (cadr (member "--config" args))))
            (should (string-match-p "bypassCSP\":true" (with-temp-buffer (insert-file-contents (cadr (member "--config" args))) (buffer-string))))
            (should (eq (plist-get def :directTools) t))
            (should-not (plist-member def :approveTools))
            (should (string-prefix-p "/opt/bin" (plist-get (plist-get def :env) :PATH)))))
      (delete-directory root t))))

(ert-deftest pai-browser-attach-and-cdt-argv ()
  (let ((root (make-temp-file "pb-root" t)))
    (unwind-protect
        (progn
          (pai-browser-test--with-settings (list :profile-root root :mode "attach") nil
            (let ((args (plist-get (pai-browser-server-def) :args)))
              (should (member "--extension" args))
              (should-not (member "--user-data-dir" args))))
          (pai-browser-test--with-settings (list :profile-root root :backend "chrome-devtools" :auto-approve t) nil
            (let* ((def (pai-browser-server-def)) (args (plist-get def :args)))
              (should (member "--no-page-id-routing" args))
              (should (member "--experimentalVision" args))
              (should (string-suffix-p "Microsoft Edge" (cadr (member "--executablePath" args))))
              (should-not (member "--headless" args))
              (should (eq (plist-get def :approveTools) :false))))
          (pai-browser-test--with-settings (list :profile-root root :backend "chrome-devtools" :mode "attach"
                                                 :extra-args '(:chrome-devtools "--foo \"a b\"")) nil
            (let ((args (plist-get (pai-browser-server-def) :args)))
              (should (equal (cadr (member "--browserUrl" args)) "http://127.0.0.1:9222"))
              (should-not (member "--executablePath" args))
              (should (equal (last args 2) '("--foo" "a b"))))))
      (delete-directory root t))))

(ert-deftest pai-browser-act-plan-playwright ()
  (let ((p (pai-browser-act-plan "click" '(:selector "#go") "playwright")))
    (should (equal (plist-get p :tool) "browser_click"))
    (should (equal (plist-get (plist-get p :args) :target) "#go")))
  (should (string-match-p "clickCount: 2"
                          (plist-get (pai-browser-act-plan "dblclick" '(:x 3 :y 4) "playwright") :pw)))
  (should (string-match-p "dispatchTouchEvent"
                          (plist-get (pai-browser-act-plan "tap" '(:selector "#b") "playwright") :pw)))
  (should (equal (plist-get (pai-browser-act-plan "select_tab" '(:index 1) "playwright") :args)
                 '(:action "select" :index 1)))
  (should (plist-get (pai-browser-act-plan "click" nil "playwright") :error))
  (should (plist-get (pai-browser-act-plan "fly" nil "playwright") :error)))

(ert-deftest pai-browser-act-plan-chrome-devtools ()
  (should (equal (pai-browser-act-plan "navigate" '(:url "https://x") "chrome-devtools")
                 '(:tool "navigate_page" :args (:type "url" :url "https://x"))))
  (let ((p (pai-browser-act-plan "type" '(:selector "#i" :text "hi") "chrome-devtools")))
    (should (equal (plist-get p :click-at) "#i"))
    (should (equal (plist-get (plist-get p :then) :tool) "type_text")))
  (should (string-match-p "unsupported" (plist-get (pai-browser-act-plan "hover" '(:selector "a") "chrome-devtools") :error)))
  (should (string-match-p "unsupported" (plist-get (pai-browser-act-plan "tap" '(:x 1 :y 2) "chrome-devtools") :error))))

(ert-deftest pai-browser-js-builders ()
  (should (equal (pai-browser-js-str "a\"b\n") "\"a\\\"b\\n\""))
  (should (equal (pai-browser-js-str nil) "null"))
  (let ((js (pai-browser-js-fetch "/_apis/x" "GET" '(:Accept "application/json") nil)))
    (should (string-match-p "credentials: 'include'" js))
    (should (string-match-p "cross-origin fetch refused" js))
    (should (string-match-p "\"Accept\":\"application/json\"" js))
    (should (string-prefix-p "async () =>" js)))
  (should (string-match-p "const __f = (() => 1)" (pai-browser-js-exec "() => 1")))
  (let ((ann (pai-browser-js-annotate-start)))
    (should (string-match-p "addEventListener" ann))
    (should-not (string-match-p "<style\\|<script\\|innerHTML\\|setAttribute('style'\\|onclick=" ann))))

(ert-deftest pai-browser-decode-results ()
  ;; Playwright prints a JSON string literal under "### Result".
  (let ((pw (list :content (vector (list :type "text"
                                         :text "### Result\n\"{\\\"ok\\\":true,\\\"value\\\":42}\"\n### Ran Playwright code\n```js\nx\n```")))))
    (should (equal (pai-browser-decode-eval pw) '(t :ok t :value 42))))
  ;; chrome-devtools fences it as ```json.
  (let ((cdt (list :content (list (list :type "text"
                                        :text "Script ran on page and returned:\n```json\n\"{\\\"ok\\\":false,\\\"error\\\":\\\"boom\\\"}\"\n```")))))
    (should (equal (pai-browser-decode-eval cdt) '(nil . "boom"))))
  (should-not (car (pai-browser-decode-eval (list :isError t :content (list (list :type "text" :text "bad")))))))

(ert-deftest pai-browser-input-stash-once ()
  (let ((pai-browser--stash nil))
    (should-not (pai-browser--input '(:type input :text "hello") nil))
    (pai-browser--stash-put 'tab (pai-browser-tab-block "https://a/?q=\"x\"" "T" ""))
    (pai-browser--stash-put 'annotations (pai-browser-annotations-block "https://a" '((:n 1 :note "fix")) "/tmp/s.jpg"))
    (let ((ret (pai-browser--input '(:type input :text "look") nil)))
      (should (eq (plist-get ret :action) 'transform))
      (should (string-prefix-p "look\n\n<browser-tab url=\"https://a/?q=&quot;x&quot;\"" (plist-get ret :text)))
      (should (string-match-p "(no selection)" (plist-get ret :text)))
      (should (string-match-p "<browser-annotations url=\"https://a\" screenshot=\"/tmp/s.jpg\">" (plist-get ret :text)))
      (should (string-match-p "\"note\":\"fix\"" (plist-get ret :text))))
    (should-not pai-browser--stash)
    (should-not (pai-browser--input '(:type input :text "again") nil))))

(ert-deftest pai-browser-fetch-rejects-writes ()
  (let (result)
    (pai-browser--tool-fetch '(:url "/x" :method "POST") nil nil (lambda (r) (setq result r)))
    (should (eq (plist-get result :is-error) t))
    (should (string-match-p "read-only" (pai-content-text (plist-get result :content))))))

(provide 'pai-browser-test)
;;; pai-browser-test.el ends here
