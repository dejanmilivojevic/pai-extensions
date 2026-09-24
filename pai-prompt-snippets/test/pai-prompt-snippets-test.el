;;; pai-prompt-snippets-test.el --- Tests for the prompt-snippets extension -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests cover the pure parts of the prompt-snippets extension: parsing
;; snippet frontmatter, loading and sorting snippets from disk, the active
;; widget rendering, and merging active snippet bodies into a message.  The
;; interactive `vui' menu is not exercised here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-prompt-snippets)

;;;; Parsing

(ert-deftest pai-prompt-snippets-test-parse-full ()
  (let ((s (pai-prompt-snippets--parse
            "concise.md"
            "---\nname: Concise\ndescription: Keep it short\nplacement: prepend\norder: 10\n---\nKeep it concise.")))
    (should s)
    (should (equal (pai-prompt-snippet-id s) "concise.md"))
    (should (equal (pai-prompt-snippet-name s) "Concise"))
    (should (equal (pai-prompt-snippet-description s) "Keep it short"))
    (should (eq (pai-prompt-snippet-placement s) 'prepend))
    (should (= (pai-prompt-snippet-order s) 10))
    (should (equal (pai-prompt-snippet-body s) "Keep it concise."))))

(ert-deftest pai-prompt-snippets-test-parse-defaults ()
  "Missing metadata falls back to filename, append, order 9999."
  (let ((s (pai-prompt-snippets--parse "my-rule.md" "---\n\n---\nBody text")))
    (should s)
    (should (equal (pai-prompt-snippet-name s) "my-rule"))
    (should (equal (pai-prompt-snippet-description s) ""))
    (should (eq (pai-prompt-snippet-placement s) 'append))
    (should (= (pai-prompt-snippet-order s) 9999))
    (should (equal (pai-prompt-snippet-body s) "Body text"))))

(ert-deftest pai-prompt-snippets-test-parse-strips-quotes ()
  "A single pair of surrounding quotes is stripped from values."
  (let ((s (pai-prompt-snippets--parse
            "q.md" "---\nname: \"My Name\"\norder: '5'\n---\nBody")))
    (should (equal (pai-prompt-snippet-name s) "My Name"))
    (should (= (pai-prompt-snippet-order s) 5))))

(ert-deftest pai-prompt-snippets-test-parse-bad-order ()
  "A non-numeric order falls back to the default 9999."
  (let ((s (pai-prompt-snippets--parse "x.md" "---\norder: soon\n---\nBody")))
    (should (= (pai-prompt-snippet-order s) 9999))))

(ert-deftest pai-prompt-snippets-test-parse-rejects-no-frontmatter ()
  (should-not (pai-prompt-snippets--parse "x.md" "just body, no frontmatter")))

(ert-deftest pai-prompt-snippets-test-parse-rejects-empty-body ()
  (should-not (pai-prompt-snippets--parse "x.md" "---\nname: X\n---\n   \n")))

(ert-deftest pai-prompt-snippets-test-parse-crlf ()
  "CRLF line endings are handled."
  (let ((s (pai-prompt-snippets--parse
            "x.md" "---\r\nname: X\r\nplacement: prepend\r\n---\r\nBody")))
    (should s)
    (should (equal (pai-prompt-snippet-name s) "X"))
    (should (eq (pai-prompt-snippet-placement s) 'prepend))))

;;;; Loading and sorting

(defmacro pai-prompt-snippets-test--with-dir (var &rest body)
  "Bind VAR to a fresh temp snippets dir and run BODY, cleaning up after.
`pai-prompt-snippets-directories' is set to (VAR) and the bundled dir is
suppressed so only VAR's files are seen."
  (declare (indent 1))
  `(let* ((,var (make-temp-file "pai-snippets-test" t))
          (pai-prompt-snippets-directories (list ,var))
          (pai-prompt-snippets--bundled-dir "/nonexistent-bundled-dir")
          (pai-directory "/nonexistent-pai-home/")
          (default-directory (make-temp-file "pai-proj" t)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t)
       (delete-directory default-directory t))))

(defun pai-prompt-snippets-test--write (dir filename content)
  "Write CONTENT to FILENAME in DIR."
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file (expand-file-name filename dir) (insert content))))

(ert-deftest pai-prompt-snippets-test-load-sorts-groups ()
  "Prepend group comes first, then append; each ordered by (order, name)."
  (pai-prompt-snippets-test--with-dir dir
    (pai-prompt-snippets-test--write
     dir "a.md" "---\nname: Zed\nplacement: append\norder: 5\n---\nA")
    (pai-prompt-snippets-test--write
     dir "b.md" "---\nname: Alpha\nplacement: prepend\norder: 20\n---\nB")
    (pai-prompt-snippets-test--write
     dir "c.md" "---\nname: Beta\nplacement: prepend\norder: 20\n---\nC")
    (pai-prompt-snippets-test--write
     dir "d.md" "---\nname: Anna\nplacement: append\norder: 5\n---\nD")
    (let ((names (mapcar #'pai-prompt-snippet-name (pai-prompt-snippets--load))))
      ;; prepend group (order 20, ties by name): Alpha, Beta
      ;; append group  (order 5,  ties by name): Anna, Zed
      (should (equal names '("Alpha" "Beta" "Anna" "Zed"))))))

(ert-deftest pai-prompt-snippets-test-load-ignores-non-md ()
  (pai-prompt-snippets-test--with-dir dir
    (pai-prompt-snippets-test--write dir "keep.md" "---\nname: Keep\n---\nBody")
    (pai-prompt-snippets-test--write dir "skip.txt" "---\nname: Skip\n---\nBody")
    (let ((names (mapcar #'pai-prompt-snippet-name (pai-prompt-snippets--load))))
      (should (equal names '("Keep"))))))

(ert-deftest pai-prompt-snippets-test-load-later-dir-wins ()
  "A later directory shadows an earlier one on filename collision."
  (let* ((low (make-temp-file "pai-snip-low" t))
         (high (make-temp-file "pai-snip-high" t))
         (pai-prompt-snippets--bundled-dir "/nonexistent")
         (pai-prompt-snippets-directories (list low high))
         (pai-directory "/nonexistent-pai-home/")
         (default-directory (make-temp-file "pai-proj" t)))
    (unwind-protect
        (progn
          (pai-prompt-snippets-test--write low "dup.md" "---\nname: Low\n---\nLow body")
          (pai-prompt-snippets-test--write high "dup.md" "---\nname: High\n---\nHigh body")
          (let ((snips (pai-prompt-snippets--load)))
            (should (= (length snips) 1))
            (should (equal (pai-prompt-snippet-name (car snips)) "High"))))
      (delete-directory low t)
      (delete-directory high t)
      (delete-directory default-directory t))))

;;;; Merging

(defun pai-prompt-snippets-test--snippet (id name placement order body)
  "Construct a snippet for tests."
  (pai-prompt-snippet--create :id id :name name :description ""
                              :placement placement :order order :body body))

(ert-deftest pai-prompt-snippets-test-merge-order ()
  "Prepend bodies come first (in the given order), then text, then appends."
  ;; `--merge' takes the already-loaded (grouped, ordered) active list.
  (let ((active
         (list (pai-prompt-snippets-test--snippet "p2.md" "P2" 'prepend 20 "PRE2")
               (pai-prompt-snippets-test--snippet "p1.md" "P1" 'prepend 10 "PRE1")
               (pai-prompt-snippets-test--snippet "a1.md" "A1" 'append 10 "APP1")
               (pai-prompt-snippets-test--snippet "a2.md" "A2" 'append 20 "APP2"))))
    (should (equal (pai-prompt-snippets--merge "MSG" active)
                   (concat "<prompt-snippet name=\"P2\">\nPRE2\n</prompt-snippet>\n\n"
                           "<prompt-snippet name=\"P1\">\nPRE1\n</prompt-snippet>\n\n"
                           "MSG\n\n"
                           "<prompt-snippet name=\"A1\">\nAPP1\n</prompt-snippet>\n\n"
                           "<prompt-snippet name=\"A2\">\nAPP2\n</prompt-snippet>")))))

(ert-deftest pai-prompt-snippets-test-merge-only-active ()
  "Only the passed-in active snippets are merged."
  (let ((active
         (list (pai-prompt-snippets-test--snippet "a.md" "A" 'append 10 "APP"))))
    (should (equal (pai-prompt-snippets--merge "MSG" active)
                   "MSG\n\n<prompt-snippet name=\"A\">\nAPP\n</prompt-snippet>"))))

(ert-deftest pai-prompt-snippets-test-merged-text-invisible-to-memory ()
  "Memory sees only the user's own words of a message with snippets."
  (require 'pai-memory-injected)
  (let ((active (list (pai-prompt-snippets-test--snippet "p.md" "Concise" 'prepend 10
                                                         "Keep it short.")
                      (pai-prompt-snippets-test--snippet "a.md" "Verify" 'append 10
                                                         "Verify, don't assume."))))
    (should (equal (pai-memory-strip-injected
                    (pai-prompt-snippets--merge "fix the parser" active))
                   "fix the parser"))))

(ert-deftest pai-prompt-snippets-test-merge-none ()
  "With no active snippets, the text is unchanged (single element join)."
  (should (equal (pai-prompt-snippets--merge "MSG" nil) "MSG")))

(ert-deftest pai-prompt-snippets-test-active-filters-by-enabled ()
  "`--active' selects the enabled members of a snippet list."
  (let* ((snippets
          (list (pai-prompt-snippets-test--snippet "p.md" "P" 'prepend 10 "PRE")
                (pai-prompt-snippets-test--snippet "a.md" "A" 'append 10 "APP")))
         (pai-prompt-snippets--enabled '("a.md")))
    (should (equal (mapcar #'pai-prompt-snippet-id
                           (pai-prompt-snippets--active snippets))
                   '("a.md")))))

;;;; Widget line

(ert-deftest pai-prompt-snippets-test-widget-line ()
  (let* ((snippets
          (list (pai-prompt-snippets-test--snippet "p.md" "Kickoff" 'prepend 10 "x")
                (pai-prompt-snippets-test--snippet "a.md" "Verify" 'append 10 "y")))
         (pai-prompt-snippets--enabled '("p.md" "a.md"))
         (line (pai-prompt-snippets--widget-line snippets)))
    (should (string-match-p "↑ prepend: Kickoff" line))
    (should (string-match-p "↓ append: Verify" line))))

(ert-deftest pai-prompt-snippets-test-widget-line-empty ()
  (let* ((snippets
          (list (pai-prompt-snippets-test--snippet "p.md" "Kickoff" 'prepend 10 "x")))
         (pai-prompt-snippets--enabled '()))
    (should-not (pai-prompt-snippets--widget-line snippets))))

(ert-deftest pai-prompt-snippets-test-user-dir-and-list ()
  "~/.pai/snippets/ is scanned; the public list carries path and origin."
  (let* ((home (file-name-as-directory (make-temp-file "pai-home" t)))
         (pai-directory home)
         (pai-prompt-snippets--bundled-dir "/nonexistent")
         (pai-prompt-snippets-directories nil)
         (proj (make-temp-file "pai-proj" t)))
    (unwind-protect
        (progn
          (make-directory (pai-prompt-snippets-user-dir) t)
          (should (equal (pai-prompt-snippets-user-dir) (expand-file-name "snippets/" home)))
          (pai-prompt-snippets-test--write
           (pai-prompt-snippets-user-dir) "plan-first.md"
           "---\nname: Plan first\ndescription: Plan before code\nplacement: prepend\norigin: learned\n---\nShow a plan first.")
          (let ((l (pai-prompt-snippets-list proj)))
            (should (= (length l) 1))
            (should (equal (plist-get (car l) :id) "plan-first.md"))
            (should (equal (plist-get (car l) :placement) "prepend"))
            (should (equal (plist-get (car l) :origin) "learned"))
            (should (equal (plist-get (car l) :path)
                           (expand-file-name "plan-first.md" (pai-prompt-snippets-user-dir)))))
          (should (member (file-name-as-directory (expand-file-name ".pai/snippets/" proj))
                          (mapcar #'file-name-as-directory
                                  (pai-prompt-snippets-directories-for proj)))))
      (delete-directory home t)
      (delete-directory proj t))))

(provide 'pai-prompt-snippets-test)
;;; pai-prompt-snippets-test.el ends here
