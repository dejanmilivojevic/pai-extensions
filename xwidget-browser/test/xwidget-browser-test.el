;;; xwidget-browser-test.el --- Tests for the xwidget-browser extension -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests cover the pure parts of the browser extension: the JavaScript
;; bridge's code generation, result decoding and the tool's formatting.
;; Driving a real webkit widget needs a graphical Emacs built with xwidget
;; support, so the whole file is skipped when `xwidget' is unavailable.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)

(defconst xwidget-browser-test--available
  (and (featurep 'xwidget-internal) (require 'xwidget-browser nil t) t)
  "Non-nil when the extension could be loaded in this Emacs.")

(defmacro xwidget-browser-test--deftest (name &rest body)
  "Define test NAME running BODY, skipped without xwidget support."
  (declare (indent 1))
  `(ert-deftest ,name ()
     (skip-unless xwidget-browser-test--available)
     ,@body))

;;;; JavaScript literals and templates

(xwidget-browser-test--deftest xwidget-browser-test-js-literal ()
  (should (equal (xwidget-browser--js-literal nil) "null"))
  (should (equal (xwidget-browser--js-literal t) "true"))
  (should (equal (xwidget-browser--js-literal :false) "false"))
  (should (equal (xwidget-browser--js-literal 42) "42"))
  (should (equal (xwidget-browser--js-literal "a\"b") "\"a\\\"b\""))
  (should (equal (xwidget-browser--js-literal "#id > .cls") "\"#id > .cls\"")))

(xwidget-browser-test--deftest xwidget-browser-test-js-literal-plist ()
  (let ((json (xwidget-browser--js-literal '(:selector "h1" :fullPage t))))
    (should (string-search "\"selector\":\"h1\"" json))
    (should (string-search "\"fullPage\":true" json))))

(xwidget-browser-test--deftest xwidget-browser-test-template ()
  (should (equal (xwidget-browser--template "var x = $VAL;" :VAL 3) "var x = 3;"))
  (should (equal (xwidget-browser--template "var x = $VAL;" :VAL nil) "var x = null;"))
  ;; Unknown placeholders are left alone rather than silently emptied.
  (should (equal (xwidget-browser--template "var x = $OTHER;" :VAL 3) "var x = $OTHER;"))
  ;; Values are escaped, never interpolated as code.
  (should (equal (xwidget-browser--template "f($S)" :S "');drop()//")
                 "f(\"');drop()//\")")))

(xwidget-browser-test--deftest xwidget-browser-test-wrap-embeds-code-as-string ()
  (let ((script (xwidget-browser--wrap "document.title")))
    (should (string-search "\"document.title\"" script))
    (should (string-search "new Function" script))
    ;; The envelope must be able to report promises and chunked payloads.
    (should (string-search "state: 'promise'" script))
    (should (string-search "__xwbPack" script))))

(xwidget-browser-test--deftest xwidget-browser-test-chunk-script ()
  (let ((script (xwidget-browser--chunk-script "__xwb_buf_1" 4096)))
    (should (string-search "\"__xwb_buf_1\"" script))
    (should (string-search "off = 4096" script))
    ;; Surrogate pairs must never be split across chunk boundaries.
    (should (string-search "0xD800" script))))

;;;; Screenshot decoding

(xwidget-browser-test--deftest xwidget-browser-test-decode-shot ()
  (let* ((png (base64-encode-string (unibyte-string 137 80 78 71 13 10 26 10) t))
         (shot (list :data (concat "data:image/png;base64," png)
                     :width 100 :height 50 :sourceWidth 200 :sourceHeight 100
                     :url "https://example.com/" :title "Example"))
         (result (xwidget-browser--decode-shot shot nil))
         (value (plist-get result :value)))
    (should (plist-get result :ok))
    (should (equal (plist-get value :mime) "image/png"))
    (should (equal (plist-get value :data) png))
    (should (equal (plist-get value :width) 100))
    (should (null (plist-get value :file)))))

(xwidget-browser-test--deftest xwidget-browser-test-decode-shot-writes-file ()
  (let* ((file (make-temp-file "xwb-shot" nil ".png"))
         (png (base64-encode-string (unibyte-string 137 80 78 71) t))
         (result (xwidget-browser--decode-shot
                  (list :data (concat "data:image/jpeg;base64," png)) file)))
    (unwind-protect
        (progn
          (should (plist-get result :ok))
          (should (equal (plist-get (plist-get result :value) :mime) "image/jpeg"))
          (should (equal (plist-get (plist-get result :value) :file) file))
          (should (> (file-attribute-size (file-attributes file)) 0)))
      (delete-file file))))

(xwidget-browser-test--deftest xwidget-browser-test-decode-shot-rejects-junk ()
  (let ((result (xwidget-browser--decode-shot (list :data "not-a-data-url") nil)))
    (should-not (plist-get result :ok))
    (should (stringp (plist-get result :error)))))

;;;; Tool-level formatting

(xwidget-browser-test--deftest xwidget-browser-test-normalize-url ()
  (should (equal (xwidget-browser-ext--normalize-url "example.com") "https://example.com"))
  (should (equal (xwidget-browser-ext--normalize-url "http://x.test") "http://x.test"))
  (should (equal (xwidget-browser-ext--normalize-url "file:///tmp/a.html") "file:///tmp/a.html")))

(xwidget-browser-test--deftest xwidget-browser-test-element-line ()
  (let ((line (xwidget-browser-ext--element-line
               '(:index 2 :tag "a" :text "Go to page two" :visible t
                 :href "https://example.com/two"
                 :rect (:x 10 :y 20 :w 100 :h 18)))))
    (should (string-search "[2] <a>" line))
    (should (string-search "href=https://example.com/two" line))
    (should (string-search "\"Go to page two\"" line))
    (should (string-search "@10,20 100x18" line)))
  (should (string-search "hidden"
                         (xwidget-browser-ext--element-line
                          '(:index 0 :tag "input" :visible :false)))))

(xwidget-browser-test--deftest xwidget-browser-test-session-line ()
  (let ((line (xwidget-browser-ext--session-line
               '(:id "s3" :buffer "*xwidget-webkit: Test*" :url "https://example.com/"
                 :title "Test" :owner agent :visible nil :pinned t :current t))))
    (should (string-prefix-p "s3 *" line))
    (should (string-search "[agent]" line))
    (should (string-search "[head-less]" line))
    (should (string-search "[pinned]" line))))

(xwidget-browser-test--deftest xwidget-browser-test-tool-registered ()
  (let ((tool (pai-tool-get "browser")))
    (should tool)
    (should (eq (plist-get tool :execution-mode) 'sequential))
    (let* ((schema (plist-get tool :parameters))
           (action (plist-get (plist-get schema :properties) :action)))
      (should (equal (plist-get schema :required) '("action")))
      (should (member "screenshot" (plist-get action :enum)))
      (should (member "attach" (plist-get action :enum))))))

(xwidget-browser-test--deftest xwidget-browser-test-tool-reports-missing-session ()
  ;; With no live session, a page action must fail with a helpful message
  ;; rather than signalling.
  (let (result)
    (cl-letf (((symbol-function 'xwidget-browser-resolve) (lambda (&optional _) nil))
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (funcall (plist-get (pai-tool-get "browser") :execute)
               '(:action "text") '(:cwd "/tmp") nil
               (lambda (r) (setq result r))))
    (should result)
    (should (eq (plist-get result :is-error) t))
    (should (string-search "No browser session"
                           (plist-get (car (plist-get result :content)) :text)))))

(xwidget-browser-test--deftest xwidget-browser-test-unknown-action ()
  (let (result)
    (cl-letf (((symbol-function 'xwidget-browser-resolve)
               (lambda (&optional _) (current-buffer)))
              ((symbol-function 'xwidget-browser-set-current) #'identity)
              ((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (funcall (plist-get (pai-tool-get "browser") :execute)
               '(:action "teleport") '(:cwd "/tmp") nil
               (lambda (r) (setq result r))))
    (should (eq (plist-get result :is-error) t))
    (should (string-search "teleport" (plist-get (car (plist-get result :content)) :text)))))

(provide 'xwidget-browser-test)
;;; xwidget-browser-test.el ends here
