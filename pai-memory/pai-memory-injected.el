;;; pai-memory-injected.el --- Keep text the user did not write out of memory -*- lexical-binding: t; -*-

;;; Commentary:

;; Some of a user message is not the user's own words: prompt snippets
;; (pai-prompt-snippets wraps each body in <prompt-snippet name="...">)
;; and loaded skills (<skill name="..." ...>...</skill>).  Used often, such
;; text looks like a habit of the user's, and observers, recall and the
;; search index would learn from it ("the user always asks for concise
;; answers").  `pai-memory-strip-injected' removes it before any of them see
;; the message: snippet blocks disappear, a skill collapses to a one-line
;; `[skill: NAME]' marker (that a skill was used can matter; its body does
;; not).  The conversation itself -- what the model reads -- is untouched.
;;
;; The scan is linear: each step moves strictly forward through the text,
;; so it always ends, and no backtracking regexp runs over the blocks.

;;; Code:

(require 'subr-x)

(defcustom pai-memory-injected-tags
  '(("prompt-snippet" . nil)
    ("skill" . pai-memory--skill-marker))
  "Tagged blocks in user messages that memory must not learn from.
Each entry is (TAG . REPLACE): REPLACE is nil to drop the block, or a
function called with the block's opening tag returning the text to keep."
  :type '(alist :key-type string :value-type (choice (const nil) function))
  :group 'pai-memory)

(defun pai-memory--tag-attribute (open-tag name)
  "Return attribute NAME of OPEN-TAG (e.g. <skill name=\"x\">), or nil."
  (save-match-data
    (and (string-match (format "\\b%s=\"\\([^\"]*\\)\"" (regexp-quote name)) open-tag)
         (match-string 1 open-tag))))

(defun pai-memory--skill-marker (open-tag)
  "Return the marker kept for a skill block opened by OPEN-TAG."
  (format "[skill: %s]" (or (pai-memory--tag-attribute open-tag "name") "?")))

(defun pai-memory--strip-tag (text tag replace)
  "Return TEXT with every <TAG ...>...</TAG> block replaced.
REPLACE is nil (drop the block) or a function of the opening tag.  An
opening tag without its closing tag is left as it is."
  (let* ((open (concat "<" tag))
         (close (concat "</" tag ">"))
         (n (length text))
         (pos 0)
         (out '()))
    (while (let ((beg (string-search open text pos)))
             (when beg
               (let* ((after (+ beg (length open)))
                      (header-end (and (< after n)
                                       (memq (aref text after) '(?\s ?> ?\t ?\n))
                                       (string-search ">" text after)))
                      (end (and header-end (string-search close text header-end))))
                 (if (not end)
                     ;; Not a block: keep through the `<' and look further on.
                     (progn (push (substring text pos (1+ beg)) out)
                            (setq pos (1+ beg)))
                   (push (substring text pos beg) out)
                   (when replace
                     (push (funcall replace (substring text beg (1+ header-end))) out))
                   (setq pos (+ end (length close))))
                 t))))
    (push (substring text pos) out)
    (apply #'concat (nreverse out))))

(defun pai-memory-strip-injected (text)
  "Return TEXT without the injected blocks of `pai-memory-injected-tags'.
Blank lines left behind are collapsed."
  (if (not (and (stringp text) (string-search "<" text)))
      text
    (let ((result text))
      (dolist (entry pai-memory-injected-tags)
        (setq result (pai-memory--strip-tag result (car entry) (cdr entry))))
      (if (equal result text)
          text
        (string-trim (replace-regexp-in-string "\n\\(?:[ \t]*\n\\)\\{2,\\}" "\n\n" result))))))

(provide 'pai-memory-injected)
;;; pai-memory-injected.el ends here
