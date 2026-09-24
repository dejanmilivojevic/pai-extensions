;;; pai-memory-snippets-test.el --- Tests for learning prompt snippets -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)
(require 'pai-prompt-snippets)

(defconst pai-memory-stest--snippet
  "---\nname: Plan first\ndescription: Show a short plan before changing code\nplacement: prepend\norder: 20\n---\nBefore editing anything, show a short numbered plan and wait for my go-ahead.\n")

(defmacro pai-memory-stest--with-owner (&rest body)
  "Run BODY like `pai-memory-test--with-owner', without the bundled snippets."
  `(pai-memory-test--with-settings '(:long-term (:promote-min-session-tokens 0))
     (pai-memory-test--with-owner buf dir
       (let ((pai-prompt-snippets--bundled-dir "/nonexistent-bundled")
             (pai-prompt-snippets-directories nil)
             (pai-skills-extra nil))
         ,@body))))

(defun pai-memory-stest--propose (&rest args)
  "Make and store a proposal from ARGS in the current project."
  (pai-memory-add-proposal
   (apply #'pai-memory-make-proposal
          (append args (list :cwd default-directory :session-id "S1")))))

(defun pai-memory-stest--promoter-response (&rest proposals)
  "Script a promoter run filing PROPOSALS, then done."
  (let ((n 0))
    (pai-faux-push
     (list :tool-calls
           (append (mapcar (lambda (a) (list :id (format "p%d" (cl-incf n)) :name "propose" :arguments a))
                           proposals)
                   (list (list :id "d" :name "done" :arguments '(:summary "ok"))))))))

(defun pai-memory-stest--task ()
  "Return the promoter task text of the last faux run."
  (pai-content-text (pai-message-content (cadr (plist-get pai-faux-last-context :messages)))))

;;;; Proposals

(ert-deftest pai-memory-snippet-create-normalizes-applies-and-undoes ()
  (pai-memory-stest--with-owner
   (let ((p (pai-memory-stest--propose :kind "snippet-create" :name "plan-first" :scope "global"
                                       :content pai-memory-stest--snippet :rationale "Asked often"
                                       :evidence '("instruction: show me the plan first"))))
     (should (equal (plist-get p :target) (expand-file-name "snippets/plan-first.md" dir)))
     (let ((text (plist-get p :after)))
       (should (string-match-p "\\`---\nname: Plan first\ndescription: Show a short plan" text))
       (should (string-match-p "^placement: prepend$" text))
       (should (string-match-p "^order: 20$" text))
       (should (string-match-p "^origin: learned$" text))
       (should (string-match-p "^source-session: S1$" text)))
     (should (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok))
     ;; the snippets extension sees it, as learned
     (let ((s (seq-find (lambda (s) (equal (plist-get s :id) "plan-first.md"))
                        (pai-prompt-snippets-list dir))))
       (should s)
       (should (equal (plist-get s :origin) "learned"))
       (should (equal (plist-get s :placement) "prepend")))
     ;; a second one of that name is refused
     (should-error (pai-memory-stest--propose :kind "snippet-create" :name "plan-first"
                                              :content pai-memory-stest--snippet :rationale "r")
                   :type 'user-error)
     (should (string-match-p "Undid" (pai-memory-undo)))
     (should-not (file-exists-p (plist-get p :target))))))

(ert-deftest pai-memory-snippet-validation ()
  (pai-memory-stest--with-owner
   (dolist (bad (list "no front-matter at all"
                      "---\nname: X\n---\nNo description."
                      "---\ndescription: d\nplacement: sideways\n---\nBody"
                      "---\ndescription: d\norder: soon\n---\nBody"
                      "---\ndescription: d\n---\n"
                      "---\ndescription: d\n---\n<prompt-snippet>x</prompt-snippet>"
                      (concat "---\ndescription: d\n---\n" (make-string 1300 ?x))))
     (should-error (pai-memory-stest--propose :kind "snippet-create" :name "x" :content bad
                                              :rationale "r")
                   :type 'user-error))
   (should-error (pai-memory-stest--propose :kind "snippet-create" :name "Bad Name"
                                            :content pai-memory-stest--snippet :rationale "r")
                 :type 'user-error)
   ;; patching needs an existing snippet
   (should-error (pai-memory-stest--propose :kind "snippet-patch" :target "/etc/passwd"
                                            :content pai-memory-stest--snippet :rationale "r")
                 :type 'user-error)))

(ert-deftest pai-memory-snippet-patch-edit-and-stale ()
  (pai-memory-stest--with-owner
   (let* ((sdir (expand-file-name "snippets" dir))
          (file (expand-file-name "terse.md" sdir)))
     (make-directory sdir t)
     (with-temp-file file (insert "---\nname: Terse\ndescription: Short answers\n---\nBe brief.\n"))
     (let ((p (pai-memory-stest--propose
               :kind "snippet-patch" :target file :rationale "r"
               :content "---\nname: Terse\ndescription: Short answers\n---\nBe brief; no preamble.\n")))
       (should (equal (plist-get p :name) "terse"))
       (should (string-match-p "patch prompt snippet terse" (pai-memory-review--label p)))
       ;; accept with edited text
       (should (plist-get (pai-memory-proposal-accept
                           (plist-get p :id)
                           "---\nname: Terse\ndescription: Short answers\n---\nBe very brief.\n")
                          :ok))
       (should (string-match-p "Be very brief" (pai-memory--read-file file))))
     ;; the file changed after the proposal: stale
     (let ((p (pai-memory-stest--propose
               :kind "snippet-patch" :target file :rationale "r"
               :content "---\nname: Terse\ndescription: Short answers\n---\nOne line.\n")))
       (with-temp-file file (insert "---\ndescription: changed\n---\nOther.\n"))
       (should (plist-get (pai-memory-proposal-accept (plist-get p :id)) :error))
       (should (equal (plist-get (pai-memory-proposal-load (plist-get p :id)) :status) "stale"))))))

(ert-deftest pai-memory-snippet-review-policy ()
  "Snippets wait for review under the `skills' policy, like skills."
  (pai-memory-stest--with-owner
   (let ((p (pai-memory-stest--propose :kind "snippet-create" :name "plan-first"
                                       :content pai-memory-stest--snippet :rationale "r")))
     (should (= (pai-memory-auto-apply (list p) 'skills) 0))
     (should (= (pai-memory-auto-apply (list p) 'none) 1)))))

;;;; Promoter

(ert-deftest pai-memory-promoter-snippets-need-instructions ()
  "A snippet is learned only from an instruction the user attached, quoted."
  (pai-memory-stest--with-owner
   (let ((e (pai-memory-test--turns session 2 200)))
     ;; no instruction: not available, and refused
     (should-not (pai-memory-snippet-creation-basis session nil nil))
     (should (eq (pai-memory-snippet-creation-basis session "a snippet" nil) 'requested))
     (pai-memory-stest--promoter-response
      (list :kind "snippet-create" :name "plan-first" :scope "global"
            :content pai-memory-stest--snippet :rationale "r"
            :evidence ["instruction: show me the plan first"]))
     (pai-memory-promote session :reason 'manual :force t)
     (should-not (pai-memory-proposals "pending"))
     (should (string-match-p "## Snippet creation\nNOT available" (pai-memory-stest--task)))
     (should (string-match-p "## Existing prompt snippets" (pai-memory-stest--task)))
     ;; the user attached an instruction: listed, and a quoting snippet is filed
     (pai-memory-test--commit session "r1" (nth 0 e) (nth 3 e)
                              "instruction: show me the plan first before editing any code")
     (should (plist-get (pai-memory-snippet-creation-basis session nil nil) :instructions))
     (pai-memory-stest--promoter-response
      (list :kind "snippet-create" :name "made-up" :scope "global"
            :content pai-memory-stest--snippet :rationale "r"
            :evidence ["the user likes plans"])
      (list :kind "snippet-create" :name "plan-first" :scope "global"
            :content pai-memory-stest--snippet :rationale "r"
            :evidence ["instruction: show me the plan first before editing any code"]))
     (pai-memory-promote session :reason 'manual :force t)
     (should (equal (mapcar (lambda (p) (plist-get p :name)) (pai-memory-proposals "pending"))
                    '("plan-first")))
     (should (string-match-p "## Snippet creation\nAvailable, but only" (pai-memory-stest--task)))
     (should (string-match-p "show me the plan first" (pai-memory-stest--task))))))

(ert-deftest pai-memory-promoter-snippets-can-be-turned-off ()
  (pai-memory-test--with-settings '(:long-term (:learn-snippets :false))
    (pai-memory-test--with-owner buf dir
      (should (eq (pai-memory-snippet-creation-basis session nil nil) 'disabled))
      (let* ((store (list nil))
             (tools (pai-memory-promoter-tools session nil store nil nil 'disabled))
             (propose (seq-find (lambda (x) (equal (plist-get x :name) "propose")) tools))
             res)
        (funcall (plist-get propose :execute)
                 (list :kind "snippet-create" :name "plan-first" :content pai-memory-stest--snippet
                       :rationale "r" :evidence ["x"])
                 nil nil (lambda (r) (setq res r)))
        (should (eq (plist-get res :is-error) t))
        (should (string-match-p "snippets is off" (pai-content-text (plist-get res :content))))
        (should-not (pai-memory-snippet-creation-text 'disabled dir))))))

(ert-deftest pai-memory-promoter-snippet-patch-needs-read ()
  (pai-memory-stest--with-owner
   (let* ((sdir (expand-file-name "snippets" dir))
          (file (expand-file-name "terse.md" sdir))
          (_ (progn (make-directory sdir t)
                    (with-temp-file file (insert "---\ndescription: Short\n---\nBe brief.\n"))))
          (store (list nil))
          (tools (pai-memory-promoter-tools session nil store nil nil nil))
          (call (lambda (name args)
                  (let (res)
                    (funcall (plist-get (seq-find (lambda (x) (equal (plist-get x :name) name)) tools)
                                        :execute)
                             args nil nil (lambda (r) (setq res r)))
                    res)))
          (patch (list :kind "snippet-patch" :target file :rationale "r"
                       :content "---\ndescription: Short\n---\nBe brief, no preamble.\n")))
     (should (eq (plist-get (funcall call "propose" patch) :is-error) t))
     ;; the snippet directory is readable by the promoter
     (should-not (eq (plist-get (funcall call "read" (list :path file)) :is-error) t))
     (should-not (eq (plist-get (funcall call "propose" patch) :is-error) t))
     (should (= (length (car store)) 1)))))

(provide 'pai-memory-snippets-test)
;;; pai-memory-snippets-test.el ends here
