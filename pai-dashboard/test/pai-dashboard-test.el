;;; pai-dashboard-test.el --- Tests for the welcome dashboard -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-dashboard)
;; Reuses the core harness for enabled/disabled extensions.
(require 'pai-ext-visible-test)

(defmacro pai-dashboard-test--with-home (dir &rest body)
  "Run BODY with DIR a temporary pai home holding two extensions and a skill."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-dash" t)))
          (pai-directory ,dir)
          (default-directory ,dir))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "extensions/pai-alpha" ,dir) t)
           (with-temp-file (expand-file-name "extensions/pai-alpha/pai-alpha.el" ,dir)
             (insert ";;; pai-alpha.el --- The alpha extension -*- lexical-binding: t; -*-\n"))
           (make-directory (expand-file-name "extensions/pai-beta" ,dir) t)
           (with-temp-file (expand-file-name "extensions/pai-beta/pai-beta.el" ,dir)
             (insert ";;; pai-beta.el --- Beta things\n"))
           ;; a directory without NAME.el is not an extension
           (make-directory (expand-file-name "extensions/notes" ,dir) t)
           (make-directory (expand-file-name "skills/greet" ,dir) t)
           (with-temp-file (expand-file-name "skills/greet/SKILL.md" ,dir)
             (insert "---\nname: greet\ndescription: Say hello properly\n---\nBody\n"))
           ,@body)
       (delete-directory ,dir t))))

(ert-deftest pai-dashboard-lists-extensions-with-summaries ()
  "Extensions come from the directories, described by their header line."
  (pai-dashboard-test--with-home dir
    (should (equal (mapcar (lambda (e) (cons (car e) (cddr e))) (pai-dashboard-extensions))
                   '(("pai-alpha" . "The alpha extension") ("pai-beta" . "Beta things"))))))

(ert-deftest pai-dashboard-lists-skills ()
  "Skills come from the skill directories with their descriptions."
  (pai-dashboard-test--with-home dir
    (cl-letf (((symbol-function 'pai--skill-dirs) (lambda () (list (expand-file-name "skills" dir)))))
      (should (equal (pai-dashboard-skills) '(("greet" . "Say hello properly")))))))

(ert-deftest pai-dashboard-render-sections ()
  "The rendering has the model line, both sections and the empty-skills hint."
  (pai-dashboard-test--with-home dir
    (cl-letf (((symbol-function 'pai-dashboard-skills) (lambda () nil)))
      (with-temp-buffer
        (let ((text (substring-no-properties (pai-dashboard-render))))
          (should (string-match-p "no model selected" text))
          (should (string-match-p "Skills (0)\n *none yet" text))
          (should (string-match-p "Extensions (2)\n *pai-alpha +The alpha extension" text))
          ;; the text logo keeps its box aligned
          (let ((lines (seq-filter (lambda (l) (string-match-p "[╭│╰]" l)) (split-string text "\n"))))
            (should (= (length lines) 3))
            (should (= 1 (length (delete-dups (mapcar (lambda (l) (string-match "[╭│╰]" l)) lines)))))))))))

(ert-deftest pai-dashboard-long-lists-are-capped ()
  "Beyond `pai-dashboard-max-items' the rest is counted, and names are cut."
  (let ((pai-dashboard-max-items 2))
    (let ((text (substring-no-properties
                 (pai-dashboard--section "Things" '("a" "b" "c" "d") 40 #'identity))))
      (should (string-match-p "Things (4)" text))
      (should (string-match-p "and 2 more" text))))
  (should (equal (pai-dashboard--fit "pai-interactive-subagents" 10) "pai-inter…")))

(ert-deftest pai-dashboard-skill-button-fills-prompt ()
  "RET on a skill puts /skill:NAME in the prompt."
  (pai-dashboard-test--with-home dir
    (let ((buf (generate-new-buffer "*pai-dash-test*")))
      (unwind-protect
          (with-current-buffer buf
            (let ((pai-default-model "faux")
                  (pai-dashboard-enable nil))
              (cl-letf (((symbol-function 'pai--project-trusted-p) (lambda () nil))
                        ((symbol-function 'pai--skill-dirs)
                         (lambda () (list (expand-file-name "skills" dir)))))
                (pai--setup dir)
                (pai-dashboard-insert)
                (goto-char (point-min))
                (search-forward "greet")
                (funcall (lookup-key (get-text-property (1- (point)) 'keymap) (kbd "RET")))
                (should (equal (pai--input-text) "/skill:greet")))))
        (let ((kill-buffer-query-functions nil)) (kill-buffer buf))))))

(ert-deftest pai-dashboard-extension-opens-dired-snippet-opens-file ()
  "RET on an extension shows its directory in Dired on its file; RET on a
snippet visits the snippet's file."
  (pai-dashboard-test--with-home dir
    (let* ((snippet (expand-file-name "concise.md" dir))
           (opened nil)
           (press (lambda (text label)
                    (with-temp-buffer
                      (insert text)
                      (goto-char (point-min))
                      (search-forward label)
                      (funcall (lookup-key (get-text-property (1- (point)) 'keymap)
                                           (kbd "RET")))))))
      (with-temp-file snippet (insert "---\nname: concise\n---\nBe brief.\n"))
      (cl-letf (((symbol-function 'pai-dashboard-skills) (lambda () nil))
                ((symbol-function 'pai-dashboard-snippets)
                 (lambda () (list (cons "concise" snippet))))
                ((symbol-function 'find-file-other-window)
                 (lambda (f &rest _) (setq opened f))))
        (let ((text (with-temp-buffer (pai-dashboard-render))))
          (save-window-excursion
            (funcall press text "pai-alpha")
            (with-current-buffer (window-buffer (selected-window))
              (should (derived-mode-p 'dired-mode))
              (should (equal (file-name-as-directory default-directory)
                             (expand-file-name "extensions/pai-alpha/" dir)))
              ;; point is on the extension's file (in the selected window)
              (goto-char (window-point))
              (should (equal (dired-get-filename)
                             (expand-file-name "extensions/pai-alpha/pai-alpha.el" dir)))
              (kill-buffer)))
          (funcall press text "concise")
          (should (equal opened snippet)))))))

(ert-deftest pai-dashboard-shown-only-on-new-conversations ()
  "startup and new show it; resume, subagents and a busy transcript do not."
  (let ((shown 0))
    (cl-letf (((symbol-function 'pai-dashboard-insert) (lambda () (cl-incf shown))))
      (with-temp-buffer
        (setq-local pai--output-marker (copy-marker (point-min)))
        (let ((noninteractive nil) (ctx (list :buffer (current-buffer))))
          (pai-dashboard--on-session-start '(:reason startup) ctx)
          (pai-dashboard--on-session-start '(:reason new) ctx)
          (pai-dashboard--on-session-start '(:reason resume) ctx)
          (should (= shown 2))
          (let ((pai-dashboard-enable nil))
            (pai-dashboard--on-session-start '(:reason new) ctx))
          (setq-local pai-isub--parent (current-buffer))
          (pai-dashboard--on-session-start '(:reason new) ctx)
          (should (= shown 2)))))))

(ert-deftest pai-dashboard-logo-uses-theme-colours ()
  "Regression: each gradient keeps its own direction (they once shared one
attribute list, so setting one changed both); the colours are the theme's."
  (let* ((svg (pai-dashboard-logo-svg))
         (grad (lambda (id) (car (dom-search svg (lambda (n) (equal (dom-attr n 'id) id))))))
         (dir (lambda (id) (list (dom-attr (funcall grad id) 'x2) (dom-attr (funcall grad id) 'y2)))))
    (should (equal (funcall dir "pai-tile") '(1 1)))
    (should (equal (funcall dir "pai-word") '(1 0)))
    (should (equal (dom-attr (car (dom-children (funcall grad "pai-tile"))) 'stop-color)
                   (nth 2 (pai-dashboard--palette))))))

(ert-deftest pai-dashboard-logo-is-an-m-x-key-in-a-net ()
  "The mark is an M-x key in plain type, set in a net of neurons."
  (let* ((svg (pai-dashboard-logo-svg))
         (a2 (nth 3 (pai-dashboard--palette))))
    (should (member "M-x" (mapcar #'dom-text (dom-by-tag svg 'text))))
    (should (member "pai" (mapcar #'dom-text (dom-by-tag svg 'text))))
    (should (> (length (dom-by-tag svg 'circle)) 15))   ; neurons
    (should (seq-find (lambda (c) (equal (dom-attr c 'fill) a2)) (dom-by-tag svg 'circle)))
    (should (string-match-p "M-x" (nth 1 pai-dashboard--text-logo)))))

(ert-deftest pai-ext-dashboard-lists-visible-extensions ()
  (require 'pai-dashboard)
  (pai-ext-visible-test--with-exts dir pai-ext-visible-test--specs
    (cl-letf (((symbol-function 'pai-dashboard--extension-dirs) (lambda () (list dir))))
      (pai-ext-visible-test--disable "ext-c" "ext-d")
      (should (equal (mapcar #'car (pai-dashboard-extensions)) '("ext-a" "ext-b" "ext-c")))
      (pai-ext-visible-test--disable "ext-a" "ext-d")
      (should (equal (mapcar #'car (pai-dashboard-extensions)) '("ext-b" "ext-c")))
      ;; the snippet list belongs to pai-prompt-snippets: hidden with it
      (cl-letf (((symbol-function 'pai-prompt-snippets--load) (lambda () (error "not reached"))))
        (should-not (pai-dashboard-snippets))))))

(provide 'pai-dashboard-test)
;;; pai-dashboard-test.el ends here
