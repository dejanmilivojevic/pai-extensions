;;; pai-memory-graph-test.el --- Tests for the learning timeline and graph (V2 F3) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-gt--with-home (dir &rest body)
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-gr" t)))
          (pai-directory ,dir)
          (default-directory (file-name-as-directory (expand-file-name "proj" ,dir)))
          (pai-settings--global nil) (pai-settings--project nil) (pai-memory--providers nil))
     (make-directory default-directory t)
     (cl-letf (((symbol-function 'pai-memory--skill-dirs)
                (lambda () (list (expand-file-name "skills" pai-directory)))))
       (unwind-protect (progn ,@body) (delete-directory ,dir t)))))

(defun pai-memory-gt--learn (kind &rest args)
  (let ((p (pai-memory-add-proposal (apply #'pai-memory-make-proposal :kind kind :rationale "r" args))))
    (should (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok))
    p))

(ert-deftest pai-memory-graph-nodes-edges-timeline ()
  (pai-memory-gt--with-home dir
    (pai-memory-gt--learn "memory-add" :target "memory" :content "the build uses make test for the ERT suite"
                          :session-id "S1")
    (pai-memory-gt--learn "skill-create" :name "ert-suite" :session-id "S1"
                          :content "---\ndescription: Use when running the ERT suite with make test\nrelated: [deploy]\n---\n1. make test\n")
    (pai-memory-gt--learn "skill-create" :name "deploy" :session-id "S2"
                          :content "---\ndescription: Use when deploying the site to production\n---\n1. make deploy\n")
    (let ((p (pai-memory-add-proposal (pai-memory-make-proposal :kind "memory-add" :target "user" :content "likes tabs" :rationale "r"))))
      (pai-memory-proposal-reject (plist-get p :id) "not true"))
    (let* ((nodes (pai-memory-graph-nodes default-directory))
           (edges (pai-memory-graph-edges nodes))
           (kinds (lambda (a b) (mapcar (lambda (e) (nth 2 e))
                                        (seq-filter (lambda (e) (equal (sort (list (nth 0 e) (nth 1 e)) #'string<)
                                                                       (sort (list a b) #'string<)))
                                                    edges))))
           (entry (plist-get (seq-find (lambda (n) (eq (plist-get n :type) 'entry)) nodes) :id)))
      (should (= (length nodes) 3))
      (should (equal (funcall kinds "skill:ert-suite" "skill:deploy") '(related)))
      ;; same session wins over similar, and each pair appears once
      (should (equal (funcall kinds entry "skill:ert-suite") '(session)))
      (should-not (funcall kinds entry "skill:deploy")))
    (let ((org (pai-memory-timeline-org default-directory)))
      (should (string-match-p "^\\* Week [0-9]\\{4\\}-W[0-9]\\{2\\}$" org))
      (should (string-match-p "learned memory entry: the build uses make test" org))
      (should (string-match-p "skill created: ert-suite" org))
      (should (string-match-p "rejected memory-add: likes tabs — not true" org))
      (should (string-match-p "^\\*\\* skill ert-suite\n   - related: skill deploy" org)))
    ;; the day filter
    (should (string-match-p "Nothing learned yet" (pai-memory-timeline-org default-directory 0)))
    (let ((dot (pai-memory-graph-dot default-directory)))
      (should (string-match-p "\"skill:ert-suite\" -- \"skill:deploy\" \\[style=bold\\]" dot)))
    ;; without Graphviz: only the source is written
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (let (final)
        (should (string-match-p "learning-graph.dot" (pai-memory-graph default-directory
                                                                       (lambda (m) (setq final m)))))
        (should (string-match-p "install Graphviz" final))
        (should (file-exists-p (pai-memory-dir "learning-graph.dot")))))
    (with-current-buffer (pai-memory-timeline nil default-directory)
      (should (string-match-p "Learning timeline" (buffer-string)))
      (kill-buffer))))

(ert-deftest pai-memory-graph-renders-asynchronously ()
  "With Graphviz, `dot' runs as a process and the result arrives later."
  (skip-unless (executable-find "dot"))
  (pai-memory-gt--with-home dir
    (pai-memory-gt--learn "memory-add" :target "memory" :content "the build uses make test"
                          :session-id "S1")
    (let* ((final nil)
           (now (pai-memory-graph default-directory (lambda (m) (setq final m)))))
      (should (string-match-p "Rendering the learning graph (1 nodes)" now))
      (should-not final)                ; nothing waited for dot
      (with-timeout (20 (ert-fail "dot did not finish"))
        (while (not final) (accept-process-output nil 0.05)))
      (should (string-match-p "Learning graph: 1 nodes, .*learning-graph.svg" final))
      (should (file-exists-p (pai-memory-dir "learning-graph.svg"))))))

(provide 'pai-memory-graph-test)
;;; pai-memory-graph-test.el ends here
