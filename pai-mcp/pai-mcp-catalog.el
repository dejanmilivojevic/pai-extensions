;;; pai-mcp-catalog.el --- MCP resources, prompts and result contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Asynchronous optional catalogs, cached alongside tool metadata.  Resource
;; tools share the normal proxy/direct selection path.  No catalog request
;; starts a server or prompts synchronously from a process filter.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'pai-mcp-client)
(require 'pai-tools)
(require 'pai-commands)
(require 'pai-ext)

(declare-function pai-send-message "pai-ui" (text &optional buffer))
(declare-function pai-mcp-register-direct-tools "pai-mcp-direct" (&optional dir))

(define-error 'pai-mcp-schema-error "MCP output schema validation failed")

(defun pai-mcp-catalog--object-p (value)
  "Whether VALUE is a decoded JSON object (nil can denote an empty object)."
  (or (hash-table-p value) (null value) (pai-json--plistp value)))

(defun pai-mcp-catalog--pairs (object)
  "Return string-keyed pairs for JSON OBJECT."
  (if (hash-table-p object)
      (let (pairs) (maphash (lambda (k v) (push (cons k v) pairs)) object) pairs)
    (pai-mcp--plist-to-alist object)))

(defun pai-mcp-catalog--has (object key)
  "Whether JSON OBJECT has string KEY, including a null or false value."
  (if (hash-table-p object)
      (not (eq (gethash key object 'pai-mcp-missing) 'pai-mcp-missing))
    (plist-member object (intern (concat ":" key)))))

(defun pai-mcp-catalog--get (object key)
  "Get string KEY from JSON OBJECT."
  (if (hash-table-p object) (gethash key object)
    (plist-get object (intern (concat ":" key)))))

(defun pai-mcp-catalog--schema-fail (path format-string &rest args)
  "Signal a schema error at PATH with FORMAT-STRING and ARGS."
  (signal 'pai-mcp-schema-error
          (list (concat path ": " (apply #'format format-string args)))))

(defun pai-mcp-catalog--type-p (value type)
  "Check decoded JSON VALUE against schema TYPE."
  (pcase type
    ("object" (pai-mcp-catalog--object-p value))
    ("array" (or (vectorp value) (and (listp value) (not (pai-json--plistp value)))))
    ("string" (stringp value))
    ("integer" (and (numberp value) (= value (truncate value))))
    ("number" (numberp value))
    ("boolean" (memq value '(t :false)))
    ("null" (eq value :null))
    (_ (pai-mcp-catalog--schema-fail "$schema" "unknown type %S" type))))

(defun pai-mcp-catalog--regexp (pattern)
  "Translate common JSON Schema regular expression syntax in PATTERN.
Unsupported lookarounds and Unicode property escapes fail explicitly."
  (when (or (string-match-p (regexp-quote "(?") pattern) (string-match-p "\\\\[pP]{" pattern))
    (pai-mcp-catalog--schema-fail "$schema" "unsupported regular expression %S" pattern))
  (let ((index 0) (inside nil) parts)
    (while (< index (length pattern))
      (let ((char (aref pattern index)))
        (cond
         ((= char ?\\)
          (setq index (1+ index))
          (when (>= index (length pattern))
            (pai-mcp-catalog--schema-fail "$schema" "trailing regexp escape"))
          (push (pcase (aref pattern index)
                  (?d "[0-9]") (?D "[^0-9]") (?w "[A-Za-z0-9_]")
                  (?W "[^A-Za-z0-9_]") (?s "[[:space:]]") (?S "[^[:space:]]")
                  ((or ?\( ?\) ?\{ ?\} ?|) (char-to-string (aref pattern index)))
                  (_ (concat "\\" (char-to-string (aref pattern index))))) parts))
         ((= char ?\[) (setq inside t) (push "[" parts))
         ((= char ?\]) (setq inside nil) (push "]" parts))
         ((and (not inside) (memq char '(?\( ?\) ?\{ ?\} ?|)))
          (push (concat "\\" (char-to-string char)) parts))
         (t (push (char-to-string char) parts))))
      (setq index (1+ index)))
    (apply #'concat (nreverse parts))))

(defun pai-mcp-catalog--schema-matches (value schema root path depth)
  "Return whether VALUE matches SCHEMA within ROOT at PATH and DEPTH."
  (condition-case nil
      (progn (pai-mcp-catalog--validate value schema root path depth) t)
    (pai-mcp-schema-error nil)))

(defun pai-mcp-catalog--resolve-ref (root reference)
  "Resolve local JSON Pointer REFERENCE in ROOT without network access."
  (unless (string-prefix-p "#" reference)
    (pai-mcp-catalog--schema-fail "$schema" "external reference is unsupported: %s" reference))
  (let ((value root))
    (unless (equal reference "#")
      (unless (string-prefix-p "#/" reference)
        (pai-mcp-catalog--schema-fail "$schema" "unresolved reference %s" reference))
      (dolist (part (split-string (substring reference 2) "/"))
        (let ((key (replace-regexp-in-string
                    "~0" "~" (replace-regexp-in-string "~1" "/" (url-unhex-string part) t t) t t)))
          (unless (pai-mcp-catalog--has value key)
            (pai-mcp-catalog--schema-fail "$schema" "unresolved reference %s" reference))
          (setq value (pai-mcp-catalog--get value key)))))
    value))

(defun pai-mcp-catalog--json-equal (left right)
  "Compare JSON LEFT and RIGHT independently of object property order."
  (cond
   ((and (numberp left) (numberp right)) (= left right))
   ((and (pai-mcp-catalog--object-p left) (pai-mcp-catalog--object-p right))
    (let ((a (pai-mcp-catalog--pairs left)) (b (pai-mcp-catalog--pairs right)))
      (and (= (length a) (length b))
           (cl-every (lambda (pair)
                       (and (pai-mcp-catalog--has right (car pair))
                            (pai-mcp-catalog--json-equal (cdr pair) (pai-mcp-catalog--get right (car pair))))) a))))
   ((and (pai-mcp-catalog--type-p left "array") (pai-mcp-catalog--type-p right "array"))
    (and (= (length left) (length right))
         (cl-every #'identity (cl-mapcar #'pai-mcp-catalog--json-equal (append left nil) (append right nil)))))
   (t (equal left right))))

(defun pai-mcp-catalog--validate (value schema root path depth)
  "Validate VALUE with SCHEMA and ROOT at PATH, bounding recursion by DEPTH."
  (when (> depth 128) (pai-mcp-catalog--schema-fail path "schema recursion exceeds 128 levels"))
  (when (hash-table-p schema)
    (setq schema (apply #'append
                        (mapcar (lambda (pair) (list (intern (concat ":" (car pair))) (cdr pair)))
                                (pai-mcp-catalog--pairs schema)))))
  (when (and (listp schema) (stringp (plist-get schema :$schema))
             (not (member (string-remove-suffix "#" (plist-get schema :$schema))
                          '("http://json-schema.org/draft-07/schema"
                            "https://json-schema.org/draft-07/schema"
                            "https://json-schema.org/draft/2020-12/schema"))))
    (pai-mcp-catalog--schema-fail path "unsupported JSON Schema dialect: %s" (plist-get schema :$schema)))
  (cond
   ((eq schema t) t)
   ((eq schema :false) (pai-mcp-catalog--schema-fail path "value forbidden by schema"))
   ((not (pai-mcp-catalog--object-p schema))
    (pai-mcp-catalog--schema-fail path "invalid schema %S" schema))
   (t
    (let ((type (plist-get schema :type)))
      (when (and type (not (cl-some (lambda (kind) (pai-mcp-catalog--type-p value kind))
                                  (if (stringp type) (list type) type))))
        (pai-mcp-catalog--schema-fail path "expected %s, got %S" type value)))
    (dolist (key '(:unevaluatedProperties :unevaluatedItems :$dynamicRef :$recursiveRef))
      (when (plist-member schema key)
        (pai-mcp-catalog--schema-fail path "unsupported schema keyword %s" key)))
    (when-let ((ref (plist-get schema :$ref)))
      (pai-mcp-catalog--validate value (pai-mcp-catalog--resolve-ref root ref) root path (1+ depth)))
    (when (and (plist-member schema :const) (not (pai-mcp-catalog--json-equal value (plist-get schema :const))))
      (pai-mcp-catalog--schema-fail path "expected constant %S" (plist-get schema :const)))
    (when (and (plist-member schema :enum) (not (cl-member value (plist-get schema :enum) :test #'pai-mcp-catalog--json-equal)))
      (pai-mcp-catalog--schema-fail path "not one of %S" (plist-get schema :enum)))
    (dolist (sub (plist-get schema :allOf))
      (pai-mcp-catalog--validate value sub root path (1+ depth)))
    (dolist (key '(:anyOf :oneOf))
      (when (plist-member schema key)
        (let ((matches (cl-count-if
                        (lambda (sub) (pai-mcp-catalog--schema-matches value sub root path (1+ depth)))
                        (plist-get schema key))))
          (unless (if (eq key :oneOf) (= matches 1) (> matches 0))
            (pai-mcp-catalog--schema-fail path "%s matched %d branches" key matches)))))
    (when (and (plist-member schema :not)
               (pai-mcp-catalog--schema-matches value (plist-get schema :not) root path (1+ depth)))
      (pai-mcp-catalog--schema-fail path "matches forbidden schema"))
    (when (plist-member schema :if)
      (let ((branch (if (pai-mcp-catalog--schema-matches value (plist-get schema :if) root path (1+ depth))
                        :then :else)))
        (when (plist-member schema branch)
          (pai-mcp-catalog--validate value (plist-get schema branch) root path (1+ depth)))))
    (when (numberp value)
      (dolist (bound '((:minimum . >=) (:maximum . <=) (:exclusiveMinimum . >) (:exclusiveMaximum . <)))
        (when-let ((limit (plist-get schema (car bound))))
          (unless (and (numberp limit) (funcall (cdr bound) value limit))
            (pai-mcp-catalog--schema-fail path "%s %S violated" (car bound) limit))))
      (when-let ((multiple (plist-get schema :multipleOf)))
        (unless (and (numberp multiple) (> multiple 0)
                     (let ((ratio (/ (float value) multiple)))
                       (< (abs (- ratio (round ratio))) 1e-9)))
          (pai-mcp-catalog--schema-fail path "not a multiple of %S" multiple))))
    (when (stringp value)
      (dolist (bound '((:minLength . >=) (:maxLength . <=)))
        (when-let ((limit (plist-get schema (car bound))))
          (unless (funcall (cdr bound) (length value) limit)
            (pai-mcp-catalog--schema-fail path "%s %d violated" (car bound) limit))))
      (when-let ((pattern (plist-get schema :pattern)))
        (unless (let ((case-fold-search nil)) (string-match-p (pai-mcp-catalog--regexp pattern) value))
          (pai-mcp-catalog--schema-fail path "does not match %S" pattern))))
    (when (pai-mcp-catalog--object-p value)
      (let ((pairs (pai-mcp-catalog--pairs value))
            (properties (plist-get schema :properties))
            (patterns (pai-mcp-catalog--pairs (plist-get schema :patternProperties))))
        (dolist (key (plist-get schema :required))
          (unless (pai-mcp-catalog--has value key)
            (pai-mcp-catalog--schema-fail path "missing required property %s" key)))
        (dolist (bound '((:minProperties . >=) (:maxProperties . <=)))
          (when-let ((limit (plist-get schema (car bound))))
            (unless (funcall (cdr bound) (length pairs) limit)
              (pai-mcp-catalog--schema-fail path "%s %d violated" (car bound) limit))))
        (dolist (pair pairs)
          (let* ((key (car pair)) (item (cdr pair)) (child (concat path "." key))
                 (known (pai-mcp-catalog--has properties key)))
            (when known
              (pai-mcp-catalog--validate item (pai-mcp-catalog--get properties key) root child (1+ depth)))
            (dolist (pattern patterns)
              (when (let ((case-fold-search nil)) (string-match-p (pai-mcp-catalog--regexp (car pattern)) key))
                (setq known t)
                (pai-mcp-catalog--validate item (cdr pattern) root child (1+ depth))))
            (when (and (not known) (plist-member schema :additionalProperties))
              (pai-mcp-catalog--validate item (plist-get schema :additionalProperties) root child (1+ depth)))
            (when (plist-member schema :propertyNames)
              (pai-mcp-catalog--validate key (plist-get schema :propertyNames) root child (1+ depth)))))
        (dolist (dependency (pai-mcp-catalog--pairs (plist-get schema :dependentRequired)))
          (when (pai-mcp-catalog--has value (car dependency))
            (dolist (key (cdr dependency))
              (unless (pai-mcp-catalog--has value key)
                (pai-mcp-catalog--schema-fail path "%s requires %s" (car dependency) key)))))
        (dolist (dependency (append (pai-mcp-catalog--pairs (plist-get schema :dependentSchemas))
                                   (pai-mcp-catalog--pairs (plist-get schema :dependencies))))
          (when (pai-mcp-catalog--has value (car dependency))
            (let ((sub (cdr dependency)))
              (pai-mcp-catalog--validate
               value (if (and (listp sub) (stringp (car sub))) (list :required sub) sub)
               root path (1+ depth)))))))
    (when (pai-mcp-catalog--type-p value "array")
      (let* ((items (append value nil)) (size (length items))
             (prefix (or (plist-get schema :prefixItems)
                         (let ((sub (plist-get schema :items)))
                           (and (listp sub) (not (pai-json--plistp sub)) sub))))
             (item-schema (if prefix
                              (if (plist-member schema :prefixItems)
                                  (if (plist-member schema :items) (plist-get schema :items) t)
                                (if (plist-member schema :additionalItems) (plist-get schema :additionalItems) t))
                            (if (plist-member schema :items) (plist-get schema :items) t))))
        (dolist (bound '((:minItems . >=) (:maxItems . <=)))
          (when-let ((limit (plist-get schema (car bound))))
            (unless (funcall (cdr bound) size limit)
              (pai-mcp-catalog--schema-fail path "%s %d violated" (car bound) limit))))
        (when (and (eq (plist-get schema :uniqueItems) t)
                   (/= size (length (cl-remove-duplicates items :test #'pai-mcp-catalog--json-equal))))
          (pai-mcp-catalog--schema-fail path "array contains duplicate items"))
        (cl-loop for item in items for index from 0 do
                 (pai-mcp-catalog--validate item (if (< index (length prefix)) (nth index prefix) item-schema)
                                            root (format "%s[%d]" path index) (1+ depth)))
        (when (plist-member schema :contains)
          (let ((matches (cl-count-if
                          (lambda (item) (pai-mcp-catalog--schema-matches item (plist-get schema :contains) root path (1+ depth))) items)))
            (unless (and (>= matches (or (plist-get schema :minContains) 1))
                         (or (not (plist-member schema :maxContains)) (<= matches (plist-get schema :maxContains))))
              (pai-mcp-catalog--schema-fail path "contains matched %d items" matches)))))))))

(defun pai-mcp-catalog-validate-result (server tool result)
  "Return RESULT, validating TOOL's structuredContent against outputSchema.
SERVER is a name or state plist; TOOL is a raw name or tool metadata plist.
Error results need not satisfy the successful output contract."
  (let* ((name (if (stringp server) server (plist-get server :name)))
         (metadata (if (stringp tool)
                       (cl-find tool (pai-mcp--server-tools name) :key (lambda (item) (plist-get item :name)) :test #'equal)
                     tool))
         (schema (plist-get metadata :outputSchema)))
    (when (and (plist-member metadata :outputSchema) (not (eq (plist-get result :isError) t)))
      (condition-case err
          (progn
            (unless (plist-member result :structuredContent)
              (pai-mcp-catalog--schema-fail "$" "missing structuredContent"))
            (pai-mcp-catalog--validate (plist-get result :structuredContent) schema schema "$" 0))
        (error (signal 'pai-mcp-schema-error
                       (list (format "%s/%s: %s" name (plist-get metadata :name) (error-message-string err)))))))
    result))

;;;; Catalog discovery and persistence

(defvar-local pai-mcp-catalog--commands nil
  "Alist of (SERVER . COMMAND-NAMES) registered in this buffer.")

(defun pai-mcp-catalog--data (name)
  "Return live or offline catalog metadata for NAME."
  (let ((server (gethash name pai-mcp--servers)))
    (if (and server (plist-get server :catalog-loaded)) server
      (cdr (assoc name (pai-mcp--load-metadata))))))

(defun pai-mcp-catalog--cache (name catalog)
  "Merge CATALOG into NAME's existing metadata and persist the cache."
  (pai-mcp--load-metadata)
  (let ((metadata (copy-sequence (cdr (assoc name pai-mcp--metadata)))))
    (dolist (key '(:resources :resourceTemplates :prompts))
      (setq metadata (plist-put metadata key (vconcat (plist-get catalog key)))))
    (setq pai-mcp--metadata
          (cons (cons name metadata) (assoc-delete-all name pai-mcp--metadata)))
    (condition-case err
        (let ((object (apply #'append
                             (mapcar (lambda (pair)
                                       (list (intern (concat ":" (car pair))) (cdr pair)))
                                     pai-mcp--metadata))))
          (make-directory (file-name-directory (pai-mcp--metadata-file)) t)
          (with-temp-file (pai-mcp--metadata-file)
            (insert (pai-json-encode (or object (pai-json-empty-object))))))
      (error (message "pai-mcp: cannot cache catalogs: %s" (error-message-string err))))))

(defun pai-mcp-catalog--list (server method key callback &optional cursor seen collected)
  "Request every page of METHOD's KEY list on SERVER, then CALLBACK(items,error).
CURSOR, SEEN and COLLECTED are private pagination state."
  (pai-mcp--request-timed
   server method (and cursor (list :cursor cursor))
   (pai-mcp--request-timeout-ms (plist-get server :def))
   (lambda (result error)
     (if error (funcall callback nil error)
       (let* ((items (append collected (append (plist-get result key) nil)))
              (next (plist-get result :nextCursor)))
         (cond
          ((or (null next) (eq next :null) (equal next "")) (funcall callback items nil))
          ((or (not (stringp next)) (member next seen))
           (funcall callback nil (format "%s returned an invalid or repeated cursor" method)))
          (t (pai-mcp-catalog--list server method key callback next (cons next seen) items))))))))

(defun pai-mcp-catalog-refresh (name on-done)
  "Refresh optional catalogs on the already-ready NAME asynchronously.
ON-DONE receives (CATALOG ERROR).  Successful lists replace stale metadata;
failed lists retain their previous cache.  Registrations occur only in the
originating live buffer, never the process filter's current buffer."
  (let* ((server (pai-mcp--server name))
         (buffer (current-buffer)) (directory default-directory)
         (generation (plist-get server :generation))
         (capabilities (plist-get server :capabilities))
         (resources (and (not (eq (plist-get (plist-get server :def) :exposeResources) :false))
                         (plist-member capabilities :resources)))
         (prompts (plist-member capabilities :prompts))
         (catalog (copy-sequence (pai-mcp-catalog--data name)))
         (jobs (append (when resources '(("resources/list" . :resources)
                                         ("resources/templates/list" . :resourceTemplates)))
                       (when prompts '(("prompts/list" . :prompts)))))
         (pending (length jobs)) errors)
    (if (not (eq (plist-get server :status) 'ready))
        (funcall on-done nil "not connected")
      (unless resources
        (setq catalog (plist-put catalog :resources nil)
              catalog (plist-put catalog :resourceTemplates nil)))
      (unless prompts (setq catalog (plist-put catalog :prompts nil)))
      (cl-labels
          ((finish ()
             (if (not (equal generation (plist-get server :generation)))
                 (funcall on-done nil "connection changed during catalog refresh")
               (dolist (key '(:resources :resourceTemplates :prompts))
                 (pai-mcp--set server key (append (plist-get catalog key) nil)))
               (pai-mcp--set server :catalog-loaded t)
               (pai-mcp-catalog--cache name catalog)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (let ((default-directory directory)) (pai-mcp-catalog-register name buffer))))
               (funcall on-done catalog (and errors (string-join (nreverse errors) "; "))))))
        (if (zerop pending) (finish)
          (dolist (job jobs)
            (let ((key (cdr job)) (method (car job)))
              (pai-mcp-catalog--list
               server method key
               (lambda (items error)
                 (if error (push (format "%s: %s" method error) errors)
                   (setq catalog (plist-put catalog key items)))
                 (when (zerop (setq pending (1- pending))) (finish)))))))))))

;;;; Resource tools

(defun pai-mcp-catalog--resource-name (name)
  "Normalize resource NAME exactly as the upstream resource tool mapper."
  (let ((token (downcase (string-trim
                          (replace-regexp-in-string "[^A-Za-z0-9]+" "_" name) "_+" "_+"))))
    (when (string-empty-p token) (setq token "resource"))
    (when (string-match-p "\\`[0-9]" token) (setq token (concat "resource_" token)))
    (concat "read_" token)))

(defun pai-mcp-catalog--template-variables (template)
  "Return URI TEMPLATE's distinct variable names, in occurrence order."
  (let ((position 0) names)
    (while (string-match "{\\([^}]+\\)}" template position)
      (let ((expression (match-string 1 template)))
        (setq position (match-end 0))
        (when (memq (aref expression 0) '(?+ ?# ?. ?/ ?\; ?? ?&))
          (setq expression (substring expression 1)))
        (dolist (variable (split-string expression ","))
          (push (replace-regexp-in-string "\\(?::[0-9]+\\|\\*\\)\\'" "" variable) names))))
    (delete-dups (nreverse names))))

(defun pai-mcp-catalog--uri-encode (value reserved)
  "Percent encode VALUE, preserving reserved URI characters when RESERVED."
  (mapconcat (lambda (char)
               (if (or (and (>= char ?a) (<= char ?z))
                       (and (>= char ?A) (<= char ?Z))
                       (and (>= char ?0) (<= char ?9))
                       (memq char '(?- ?_ ?. ?~))
                       (and reserved (memq char '(?: ?/ ?? ?# ?\[ ?\] ?@ ?! ?$ ?& ?\' ?\( ?\) ?* ?+ ?, ?\; ?= ?%))))
                   (char-to-string char) (format "%%%02X" char)))
             (string-to-list (encode-coding-string value 'utf-8 t)) ""))

(defun pai-mcp-catalog-expand-template (template arguments)
  "Expand RFC 6570 scalar URI TEMPLATE using string ARGUMENTS.
Supports every operator, prefix modifiers and scalar explode modifiers.
Missing values are omitted; non-string values signal an actionable error."
  (replace-regexp-in-string
   "{\\([^}]+\\)}"
   (lambda (match)
     (save-match-data
     (let* ((expression (substring match 1 -1))
            (operator (and (memq (aref expression 0) '(?+ ?# ?. ?/ ?\; ?? ?&)) (aref expression 0)))
            (variables (split-string (if operator (substring expression 1) expression) ","))
            (separator (pcase operator ((or ?? ?&) "&") (?\; ";") (?/ "/") (?. ".") (_ ",")))
            (prefix (pcase operator (?+ "") ((pred null) "") (_ (char-to-string operator)))) values)
       (dolist (variable variables)
         (let* ((name (replace-regexp-in-string "\\(?::[0-9]+\\|\\*\\)\\'" "" variable))
                (value (pai-mcp-catalog--get arguments name)))
           (when (pai-mcp-catalog--has arguments name)
             (unless (stringp value) (error "Resource template argument %s must be a string" name))
             (when (string-match ":\\([0-9]+\\)\\'" variable)
               (setq value (substring value 0 (min (length value) (string-to-number (match-string 1 variable))))))
             (setq value (pai-mcp-catalog--uri-encode value (memq operator '(?+ ?#))))
             (push (if (memq operator '(?\; ?? ?&))
                       (concat (pai-mcp-catalog--uri-encode name nil)
                               (if (and (eq operator ?\;) (string-empty-p value)) "" (concat "=" value)))
                     value) values))))
       (if values (concat prefix (string-join (nreverse values) separator)) ""))))
   template t t))

(defun pai-mcp-catalog-resource-tools (name)
  "Return synthetic resource tool metadata for NAME without connecting it."
  (let* ((server (gethash name pai-mcp--servers))
         (def (or (pai-mcp--server-def name) (plist-get server :def)))
         (catalog (pai-mcp-catalog--data name))
         (seen (mapcar (lambda (tool) (plist-get tool :name))
                       (or (plist-get server :tools) (plist-get catalog :tools)))) tools)
    (unless (or (pai-mcp--disabled-p def) (eq (plist-get def :exposeResources) :false))
      (dolist (resource (append (plist-get catalog :resources) (plist-get catalog :resourceTemplates) nil))
        (let* ((raw (pai-mcp-catalog--resource-name (or (plist-get resource :name) "resource")))
               (template (plist-get resource :uriTemplate))
               (variables (and template (pai-mcp-catalog--template-variables template))))
          (unless (member raw seen)
            (push raw seen)
            (push (list :name raw
                        :description (or (plist-get resource :description)
                                         (concat "Read resource: " (or (plist-get resource :uri) template)))
                        :inputSchema (pai-object-schema
                                      (apply #'append (mapcar (lambda (variable)
                                                               (list (intern (concat ":" variable))
                                                                     (pai-string-schema "URI template value"))) variables))
                                      variables)
                        :pai-mcp-resource resource) tools)))))
    (nreverse tools)))

(defun pai-mcp-catalog-call-resource (name tool arguments on-done &optional directory)
  "Read synthetic TOOL on NAME with ARGUMENTS and deliver a pai result.
The normal caller must perform tool visibility and approval checks first."
  (pai-mcp-ensure
   name
   (lambda ()
     (let* ((server (pai-mcp--server name))
            (metadata (if (stringp tool)
                          (cl-find tool (pai-mcp-catalog-resource-tools name)
                                   :test #'equal :key (lambda (item) (plist-get item :name))) tool))
            (resource (plist-get metadata :pai-mcp-resource)))
       (condition-case err
           (progn
             (unless (and resource (plist-member (plist-get server :capabilities) :resources)
                          (not (eq (plist-get (plist-get server :def) :exposeResources) :false)))
               (error "Resources are unavailable on %s" name))
             (dolist (variable (plist-get (plist-get metadata :inputSchema) :required))
               (unless (pai-mcp-catalog--has arguments variable)
                 (error "Missing resource template argument %s" variable)))
             (let* ((template (plist-get resource :uriTemplate))
                    (uri (or (plist-get resource :uri)
                             (and template (pai-mcp-catalog-expand-template template arguments)))))
               (unless uri (error "Resource has no URI"))
               (pai-mcp--request-timed
                server "resources/read" (list :uri uri)
                (pai-mcp--request-timeout-ms (plist-get server :def) directory)
                (lambda (result error)
                  (funcall on-done
                           (if error (pai-tool-error-result error)
                             (pai-mcp--result->tool
                              (list :content (mapcar (lambda (content) (list :type "resource" :resource content))
                                                      (plist-get result :contents))))))))))
         (error (funcall on-done (pai-tool-error-result (error-message-string err)))))))
   (lambda (error) (funcall on-done (pai-tool-error-result error))) directory))

;;;; Prompt slash commands

(defun pai-mcp-catalog--tokens (input)
  "Tokenize upstream-style prompt INPUT, preserving quotes until resolution."
  (let ((index 0) quote escaped current tokens)
    (while (< index (length input))
      (let ((char (aref input index)))
        (cond
         (escaped (push char current) (setq escaped nil))
         ((and (= char ?\\) (not (eq quote ?\'))) (setq escaped t))
         ((and (null quote) (memq char '(?\s ?\t ?\n ?\r)))
          (when current (push (concat (nreverse current)) tokens) (setq current nil)))
         ((memq char '(?\' ?\"))
          (cond ((null quote) (setq quote char)) ((= quote char) (setq quote nil)))
          (push char current))
         (t (push char current))))
      (setq index (1+ index)))
    (when current (push (concat (nreverse current)) tokens))
    (nreverse tokens)))

(defun pai-mcp-catalog--unquote (text)
  "Strip one matching pair of outer quotes from TEXT."
  (let ((text (string-trim text)))
    (if (and (>= (length text) 2) (memq (aref text 0) '(?\' ?\"))
             (= (aref text 0) (aref text (1- (length text)))))
        (substring text 1 -1) text)))

(defun pai-mcp-catalog-prompt-arguments (prompt input)
  "Resolve INPUT into string arguments for PROMPT; reject missing required values."
  (let (named positional result missing)
    (dolist (token (pai-mcp-catalog--tokens input))
      (let ((index 0) quote split)
        (while (and (< index (length token)) (null split))
          (let ((char (aref token index)))
            (cond
             ((memq char '(?\' ?\"))
              (cond ((null quote) (setq quote char)) ((= quote char) (setq quote nil))))
             ((and (= char ?=) (null quote) (> index 0)) (setq split index))))
          (setq index (1+ index)))
        (if split
            (setf (alist-get (string-trim (substring token 0 split)) named nil nil #'equal)
                  (pai-mcp-catalog--unquote (substring token (1+ split))))
          (push (pai-mcp-catalog--unquote token) positional))))
    (setq positional (nreverse positional))
    (dolist (argument (plist-get prompt :arguments))
      (let* ((name (plist-get argument :name)) (pair (assoc name named))
             (value (if pair (cdr pair) (pop positional))))
        (when (and value (not (string-empty-p value))) (push (cons name value) result))
        (when (and (eq (plist-get argument :required) t) (or (null value) (string-empty-p value)))
          (push name missing))))
    (when missing
      (error "Missing required argument(s): %s. Usage: %s"
             (string-join (nreverse missing) ", ")
             (string-join (mapcar (lambda (argument)
                                   (format (if (eq (plist-get argument :required) t) "<%s>" "[%s]")
                                           (plist-get argument :name))) (plist-get prompt :arguments)) " ")))
    (dolist (pair named)
      (unless (cl-find (car pair) (plist-get prompt :arguments) :test #'equal
                       :key (lambda (argument) (plist-get argument :name)))
        (push pair result)))
    (or (apply #'append (mapcar (lambda (pair) (list (intern (concat ":" (car pair))) (cdr pair))) result))
        (pai-json-empty-object))))

(defun pai-mcp-catalog--prompt-content (content)
  "Render one MCP prompt CONTENT block as upstream-compatible text."
  (pcase (plist-get content :type)
    ("text" (or (plist-get content :text) ""))
    ("resource"
     (let ((resource (plist-get content :resource)))
       (concat (format "[resource %s]" (plist-get resource :uri))
               (when (plist-get resource :text) (concat "\n" (plist-get resource :text))))))
    ("resource_link" (format "[resource_link %s%s]" (plist-get content :uri)
                              (if (plist-get content :name) (concat " — " (plist-get content :name)) "")))
    ("image" (format "[image %s%s]" (or (plist-get content :mimeType) "unknown")
                       (if (plist-get content :data) " (embedded)" "")))
    ("audio" (format "[audio %s]" (or (plist-get content :mimeType) "unknown")))
    (_ "")))

(defun pai-mcp-catalog-render-prompt (result)
  "Flatten RESULT's prompt messages to a single role-marked user message."
  (let ((messages (plist-get result :messages)) rendered)
    (dolist (message messages)
      (let ((text (pai-mcp-catalog--prompt-content (plist-get message :content))))
        (unless (string-empty-p text)
          (push (if (and (= (length messages) 1) (equal (plist-get message :role) "user")) text
                  (format "[%s] %s" (plist-get message :role) text)) rendered))))
    (string-trim (string-join (nreverse rendered) "\n\n"))))

(defun pai-mcp-catalog--prompt-name (server prompt definition settings)
  "Return the namespaced slash command for SERVER/PROMPT."
  (let* ((sanitize (lambda (name)
                     (mapconcat (lambda (char)
                                  (if (string-match-p "[A-Za-z0-9_-]" (char-to-string char))
                                      (char-to-string char) (format "_%x_" char)))
                                (string-to-list name) "")))
         (mode (or (plist-get definition :toolPrefix) (plist-get settings :toolPrefix) "server"))
         (prefix (funcall sanitize server))
         (name (string-trim (replace-regexp-in-string "[^A-Za-z0-9_-]+" "_" prompt) "[_-]+" "[_-]+")))
    (when (equal mode "short")
      (setq prefix (let ((case-fold-search t)) (replace-regexp-in-string "-?mcp\\'" "" prefix)))
      (when (string-empty-p prefix) (setq prefix "mcp")))
    (when (equal mode "mcp") (setq prefix (concat "mcp__" prefix)))
    (when (string-empty-p prefix) (setq prefix "server"))
    (when (string-empty-p name) (setq name "prompt"))
    (when (string-match-p "\\`[0-9]" name) (setq name (concat "_" name)))
    (format "mcp__%s__%s" prefix name)))

(defun pai-mcp-catalog-get-prompt (name prompt input on-done &optional directory)
  "Resolve PROMPT/INPUT, ensure NAME, then ON-DONE(text,error) asynchronously."
  (condition-case err
      (let ((arguments (pai-mcp-catalog-prompt-arguments prompt input)))
        (pai-mcp-ensure
         name
         (lambda ()
           (let* ((server (pai-mcp--server name))
                  (current (cl-find (plist-get prompt :name) (plist-get server :prompts)
                                    :test #'equal :key (lambda (item) (plist-get item :name)))))
             (if (not (and current (plist-member (plist-get server :capabilities) :prompts)))
                 (funcall on-done nil (format "Prompt %s is no longer available on %s" (plist-get prompt :name) name))
               (pai-mcp--request-timed
                server "prompts/get"
                (append (list :name (plist-get prompt :name))
                        (unless (and (hash-table-p arguments) (zerop (hash-table-count arguments)))
                          (list :arguments arguments)))
                (pai-mcp--request-timeout-ms (plist-get server :def) directory)
                (lambda (result error)
                  (if error (funcall on-done nil error)
                    (funcall on-done (pai-mcp-catalog-render-prompt result) nil)))))))
         (lambda (error) (funcall on-done nil error)) directory))
    (error (funcall on-done nil (error-message-string err)))))

(defun pai-mcp-catalog-register (name &optional buffer)
  "Register NAME's cached/live prompts and direct resource tools in BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (command (cdr (assoc name pai-mcp-catalog--commands)))
          (when (equal (plist-get (pai-command-get command) :pai-mcp-server) name)
            (pai-unregister-command command)))
        (setq pai-mcp-catalog--commands (assoc-delete-all name pai-mcp-catalog--commands))
        (let ((definition (or (pai-mcp--server-def name) (plist-get (gethash name pai-mcp--servers) :def)))
              (settings (pai-mcp-settings)) names)
          (unless (pai-mcp--disabled-p definition)
            (dolist (prompt (append (plist-get (pai-mcp-catalog--data name) :prompts) nil))
              (let ((command (pai-mcp-catalog--prompt-name name (plist-get prompt :name) definition settings)))
                (unless (pai-command-get command)
                  (let ((registered
                         (pai-register-command
                          command :source 'extension :description (plist-get prompt :description)
                          :handler
                          (lambda (input context)
                            (let ((origin (or (plist-get context :buffer) (current-buffer)))
                                  (directory (or (plist-get context :cwd) default-directory)))
                              (pai-mcp-catalog-get-prompt
                               name prompt input
                               (lambda (text error)
                                 (when (buffer-live-p origin)
                                   (with-current-buffer origin
                                     (cond
                                      (error (pai-ext-ui-notify context error 'error))
                                      ((string-empty-p text) (pai-ext-ui-notify context "MCP prompt returned no content" 'warning))
                                      (t (run-at-time 0 nil
                                                      (lambda ()
                                                        (when (buffer-live-p origin) (pai-send-message text origin)))))))))
                               directory))
                            nil))))
                    (plist-put registered :pai-mcp-server name)
                    (push command names))))))
          (push (cons name names) pai-mcp-catalog--commands))
        (when (fboundp 'pai-mcp-register-direct-tools) (pai-mcp-register-direct-tools default-directory))))))

(provide 'pai-mcp-catalog)
;;; pai-mcp-catalog.el ends here
