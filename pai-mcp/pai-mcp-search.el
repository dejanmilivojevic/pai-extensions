;;; pai-mcp-search.el --- Ranked MCP tool search for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Ranked, paginated search over cached MCP tool metadata (and, optionally, the
;; agent's own tools).  Supports weighted scoring, regex queries, fuzzy
;; hyphen/underscore matching with suggestions on a miss, per-server
;; searchKeywords, and optional compact schema shapes.  All from cache, so no
;; server connection is required.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-tools)
(require 'pai-mcp-config)
(require 'pai-mcp-client)

(defcustom pai-mcp-search-limit 10
  "Default number of results returned per search page."
  :type 'integer :group 'pai-mcp)

(defcustom pai-mcp-search-include-pai-tools t
  "When non-nil, MCP search also surfaces the agent's own registered tools."
  :type 'boolean :group 'pai-mcp)

(defun pai-mcp--normalize (s)
  "Lowercase S and collapse -/_/space so fuzzy matching ignores separators."
  (replace-regexp-in-string "[-_ ]+" " " (downcase (or s ""))))

(defun pai-mcp--search-entries (&optional dir)
  "Return searchable entries: MCP tools from cache plus optional pai tools.
Each entry is a plist (:name :server :description :keywords :schema :source)."
  (let ((entries '()))
    (dolist (pair (pai-mcp-load-config dir))
      (let* ((server (car pair))
             (kw (plist-get (cdr pair) :searchKeywords)))
        (dolist (tool (pai-mcp--server-tools server))
          (push (list :name (plist-get tool :name)
                      :server server
                      :description (or (plist-get tool :description) "")
                      :keywords kw
                      :schema (plist-get tool :inputSchema)
                      :source 'mcp)
                entries))))
    (when pai-mcp-search-include-pai-tools
      (dolist (tool (pai-tools-all))
        (push (list :name (plist-get tool :name)
                    :server "pi"
                    :description (or (plist-get tool :description) "")
                    :keywords nil :schema (plist-get tool :parameters)
                    :source 'pi)
              entries)))
    (nreverse entries)))

(defun pai-mcp--score (entry query)
  "Return a relevance score for ENTRY against QUERY, or nil when no match."
  (let* ((name (or (plist-get entry :name) ""))
         (desc (plist-get entry :description))
         (server (plist-get entry :server))
         (kws (mapconcat #'identity (plist-get entry :keywords) " "))
         (q (downcase query))
         (nq (pai-mcp--normalize query))
         (score 0))
    (cond ((equal (downcase name) q) (cl-incf score 100))
          ((string-prefix-p q (downcase name)) (cl-incf score 60))
          ((string-match-p (regexp-quote q) (downcase name)) (cl-incf score 40))
          ((string-match-p (regexp-quote nq) (pai-mcp--normalize name)) (cl-incf score 25)))
    (when (and kws (string-match-p (regexp-quote q) (downcase kws))) (cl-incf score 30))
    (when (string-match-p (regexp-quote q) (downcase server)) (cl-incf score 20))
    (when (and desc (string-match-p (regexp-quote q) (downcase desc))) (cl-incf score 10))
    (and (> score 0) score)))

(defun pai-mcp--regex-score (entry regex)
  "Return a score for ENTRY when REGEX matches its name or description."
  (condition-case nil
      (cond ((string-match-p regex (or (plist-get entry :name) "")) 50)
            ((string-match-p regex (or (plist-get entry :description) "")) 10)
            (t nil))
    (error nil)))

(defun pai-mcp--schema-shape (schema)
  "Return a compact one-line TypeScript-ish shape for SCHEMA, or \"\"."
  (let ((props (plist-get schema :properties))
        (required (plist-get schema :required))
        parts)
    (while props
      (let* ((pname (substring (symbol-name (car props)) 1))
             (type (or (plist-get (cadr props) :type) "any"))
             (opt (if (member pname required) "" "?")))
        (push (format "%s%s: %s" pname opt type) parts))
      (setq props (cddr props)))
    (if parts (format "{ %s }" (string-join (nreverse parts) "; ")) "")))

(defun pai-mcp--format-entry (entry include-schema)
  "Format ENTRY as a result block, optionally with its schema shape."
  (format "%s  [%s]%s\n  %s"
          (plist-get entry :name)
          (plist-get entry :server)
          (if (eq (plist-get entry :source) 'pi) " (pi tool)" "")
          (concat (or (plist-get entry :description) "")
                  (when include-schema
                    (let ((shape (pai-mcp--schema-shape (plist-get entry :schema))))
                      (unless (string-empty-p shape) (concat "\n  " shape)))))))

(defun pai-mcp--suggestions (query entries)
  "Return up to 3 tool names closest to QUERY by edit distance."
  (let* ((nq (pai-mcp--normalize query))
         (scored (mapcar (lambda (e)
                           (cons (string-distance nq (pai-mcp--normalize (plist-get e :name)))
                                 (plist-get e :name)))
                         entries)))
    (mapcar #'cdr (seq-take (sort scored (lambda (a b) (< (car a) (car b)))) 3))))

(cl-defun pai-mcp-search (query &key regex (limit pai-mcp-search-limit)
                                (offset 0) include-schemas dir)
  "Search cached MCP (and pai) tools for QUERY.
Return a plist (:text :count :next-offset :names).  With REGEX non-nil QUERY is
treated as a regexp.  Results are ranked and paginated by LIMIT/OFFSET."
  (let* ((entries (pai-mcp--search-entries dir))
         (scored (delq nil
                       (mapcar (lambda (e)
                                 (let ((s (if regex (pai-mcp--regex-score e query)
                                            (if (string-empty-p (string-trim (or query "")))
                                                1 (pai-mcp--score e query)))))
                                   (and s (cons s e))))
                               entries)))
         (ranked (sort scored (lambda (a b)
                                (if (= (car a) (car b))
                                    (string-lessp (plist-get (cdr a) :name)
                                                  (plist-get (cdr b) :name))
                                  (> (car a) (car b))))))
         (total (length ranked))
         (page (seq-subseq ranked (min offset total)
                           (min (+ offset limit) total)))
         (next (when (< (+ offset limit) total) (+ offset limit))))
    (cond
     ((null entries)
      (list :text "No MCP tool metadata cached yet. Run mcp({\"action\":\"refresh\"}) or call a tool."
            :count 0 :next-offset nil :names nil))
     ((null ranked)
      (let ((sugg (pai-mcp--suggestions query entries)))
        (list :text (format "No tools match \"%s\".%s" query
                            (if sugg (format " Did you mean: %s?" (string-join sugg ", ")) ""))
              :count 0 :next-offset nil :names nil)))
     (t
      (list :text (concat
                   (string-join (mapcar (lambda (p) (pai-mcp--format-entry (cdr p) include-schemas))
                                        page)
                                "\n\n")
                   (when next
                     (format "\n\n(%d more; use offset %d)" (- total (+ offset limit)) next)))
            :count total :next-offset next
            :names (mapcar (lambda (p) (plist-get (cdr p) :name)) page))))))

(provide 'pai-mcp-search)
;;; pai-mcp-search.el ends here
