;;; pai-mcp-catalog-test.el --- Catalog consumer contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'pai-mcp-catalog)

(defmacro pai-mcp-catalog-test--isolated (&rest body)
  "Run BODY with isolated server metadata and registries."
  (declare (indent 0))
  `(let ((pai-mcp--servers (make-hash-table :test 'equal))
         (pai-mcp--metadata nil) (pai-mcp--metadata-loaded t)
         (pai--tools (make-hash-table :test 'equal))
         (pai--commands (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'pai-mcp--server-def) (lambda (&rest _) nil))
               ((symbol-function 'pai-mcp-settings) (lambda (&rest _) nil))
               ((symbol-function 'pai-mcp-register-direct-tools) #'ignore))
       ,@body)))

(ert-deftest pai-mcp-catalog-validates-nested-output-contract ()
  (let* ((schema '(:type "object" :required ("items") :additionalProperties :false
                   :properties (:items (:type "array" :minItems 1 :uniqueItems t
                                        :items (:$ref "#/$defs/item")))
                   :$defs (:item (:type "object" :required ("id")
                                 :properties (:id (:type "integer" :minimum 1))))))
         (tool (list :name "query" :outputSchema schema))
         (valid '(:structuredContent (:items ((:id 2))))))
    (should (eq valid (pai-mcp-catalog-validate-result "s" tool valid)))
    (should-error (pai-mcp-catalog-validate-result "s" tool '(:structuredContent (:items ((:id 0)))))
                  :type 'pai-mcp-schema-error)
    (should-error (pai-mcp-catalog-validate-result "s" tool '(:structuredContent (:items ((:id 2)) :extra t)))
                  :type 'pai-mcp-schema-error)
    (should-error (pai-mcp-catalog-validate-result "s" tool '(:structuredContent (:items ((:id 2) (:id 2)))))
                  :type 'pai-mcp-schema-error)))

(ert-deftest pai-mcp-catalog-distinguishes-missing-and-falsy-output ()
  (let ((tool '(:name "flag" :outputSchema (:type "boolean"))))
    (should (equal (pai-mcp-catalog-validate-result "s" tool '(:structuredContent :false))
                   '(:structuredContent :false)))
    (should-error (pai-mcp-catalog-validate-result "s" tool '(:content nil)) :type 'pai-mcp-schema-error)
    (should (equal (pai-mcp-catalog-validate-result "s" tool '(:isError t)) '(:isError t)))))

(ert-deftest pai-mcp-catalog-validates-conditional-and-union-schemas ()
  (let ((tool '(:name "branch" :outputSchema
                (:type "object" :required ("kind")
                 :if (:properties (:kind (:const "count")))
                 :then (:required ("count") :properties (:count (:type "integer")))
                 :else (:required ("text") :properties (:text (:type "string" :pattern "^[a-z]+$")))))))
    (should-error (pai-mcp-catalog-validate-result "s" tool '(:structuredContent (:kind "count" :text "abc")))
                  :type 'pai-mcp-schema-error)
    (should-error (pai-mcp-catalog-validate-result "s" tool '(:structuredContent (:kind "text" :text "123")))
                  :type 'pai-mcp-schema-error)
    (should (equal (pai-mcp-catalog-validate-result "s" tool '(:structuredContent (:kind "count" :count 4)))
                   '(:structuredContent (:kind "count" :count 4))))))

(ert-deftest pai-mcp-catalog-rejects-unknown-dialect-and-unresolved-ref ()
  (should-error (pai-mcp-catalog-validate-result "s" '(:name "x" :outputSchema (:$schema "https://example.test/schema"))
                                               '(:structuredContent (:x 1))) :type 'pai-mcp-schema-error)
  (should-error (pai-mcp-catalog-validate-result "s" '(:name "x" :outputSchema (:$ref "#/$defs/missing"))
                                               '(:structuredContent (:x 1))) :type 'pai-mcp-schema-error))

(ert-deftest pai-mcp-catalog-resource-template-encoding ()
  (should (equal (pai-mcp-catalog-expand-template "repo://{owner}/{path}{?query}" '(:owner "a b" :path "src/x" :query "a&b"))
                 "repo://a%20b/src%2Fx?query=a%26b"))
  (should (equal (pai-mcp-catalog-expand-template "{+path}{#fragment}" '(:path "/src/main" :fragment "part 2"))
                 "/src/main#part%202"))
  (should (equal (pai-mcp-catalog-expand-template "{/name:3}{;empty}{?q}" '(:name "abcdef" :empty ""))
                 "/abc;empty")))

(ert-deftest pai-mcp-catalog-resource-policy-and-collisions ()
  (pai-mcp-catalog-test--isolated
    (let ((server (pai-mcp--server "docs")))
      (pai-mcp--set server :catalog-loaded t)
      (pai-mcp--set server :tools '((:name "read_about")))
      (pai-mcp--set server :resources '((:name "About" :uri "docs://about") (:name "API Guide" :uri "docs://api")))
      (should (equal (mapcar (lambda (tool) (plist-get tool :name)) (pai-mcp-catalog-resource-tools "docs"))
                     '("read_api_guide")))
      (pai-mcp--set server :def '(:exposeResources :false))
      (should-not (pai-mcp-catalog-resource-tools "docs")))))

(ert-deftest pai-mcp-catalog-refresh-pages-and-registers-in-origin ()
  (pai-mcp-catalog-test--isolated
    (let ((origin (generate-new-buffer " catalog-origin"))
          (other (generate-new-buffer " catalog-other")) requests finished)
      (unwind-protect
          (let ((server (pai-mcp--server "s")))
            (pai-mcp--set server :status 'ready)
            (pai-mcp--set server :capabilities '(:prompts nil))
            (cl-letf (((symbol-function 'pai-mcp-catalog--cache) #'ignore)
                      ((symbol-function 'pai-mcp--request-timed)
                       (lambda (_server method params _timeout callback)
                         (push (list method params callback) requests))))
              (with-current-buffer origin
                (setq-local pai--commands (make-hash-table :test 'equal))
                (pai-mcp-catalog-refresh "s" (lambda (catalog error) (setq finished (list catalog error)))))
              (should-not finished)
              (with-current-buffer other
                (funcall (nth 2 (pop requests)) '(:prompts ((:name "first")) :nextCursor "page2") nil)
                (should (equal (nth 1 (car requests)) '(:cursor "page2")))
                (funcall (nth 2 (pop requests)) '(:prompts ((:name "second"))) nil))
              (should finished)
              (with-current-buffer origin
                (should (pai-command-get "mcp__s__first"))
                (should (pai-command-get "mcp__s__second")))
              (with-current-buffer other (should-not (pai-command-get "mcp__s__first")))))
        (kill-buffer origin) (kill-buffer other)))))

(ert-deftest pai-mcp-catalog-unsupported-server-does-not-probe ()
  (pai-mcp-catalog-test--isolated
    (let ((server (pai-mcp--server "s")))
      (pai-mcp--set server :status 'ready)
      (cl-letf (((symbol-function 'pai-mcp-catalog--cache) #'ignore)
                ((symbol-function 'pai-mcp--request-timed) (lambda (&rest _) (ert-fail "unsupported catalog request"))))
        (pai-mcp-catalog-refresh "s" #'ignore)))))

(ert-deftest pai-mcp-catalog-repeated-pagination-cursor-errors ()
  (pai-mcp-catalog-test--isolated
    (let (error)
      (cl-letf (((symbol-function 'pai-mcp--request-timed)
                 (lambda (_server _method _params _timeout callback)
                   (funcall callback '(:prompts ((:name "x")) :nextCursor "same") nil))))
        (pai-mcp-catalog--list (pai-mcp--server "s") "prompts/list" :prompts
                               (lambda (_items failure) (setq error failure))))
      (should (string-match-p "repeated cursor" error)))))

(ert-deftest pai-mcp-catalog-prompt-named-arguments-do-not-consume-positionals ()
  (let ((prompt '(:name "review" :arguments ((:name "language" :required t) (:name "file" :required t)))))
    (should (equal (pai-mcp-catalog-prompt-arguments prompt "language=elisp 'my file.el' extra=\"two words\"")
                   '(:extra "two words" :file "my file.el" :language "elisp")))
    (should-error (pai-mcp-catalog-prompt-arguments prompt "language=elisp"))))

(ert-deftest pai-mcp-catalog-prompt-rendering-retains-roles-and-resources ()
  (should (equal (pai-mcp-catalog-render-prompt
                  '(:messages ((:role "user" :content (:type "text" :text "Hello"))))) "Hello"))
  (should (equal (pai-mcp-catalog-render-prompt
                  '(:messages ((:role "assistant" :content (:type "text" :text "Context"))
                               (:role "user" :content (:type "resource" :resource (:uri "docs://x" :text "Body"))))))
                 "[assistant] Context\n\n[user] [resource docs://x]\nBody")))

(ert-deftest pai-mcp-catalog-resource-call-uses-advertised-uri ()
  (pai-mcp-catalog-test--isolated
    (let ((server (pai-mcp--server "s")) request result)
      (pai-mcp--set server :capabilities '(:resources nil))
      (cl-letf (((symbol-function 'pai-mcp-ensure) (lambda (_name ready _error &optional _dir) (funcall ready)))
                ((symbol-function 'pai-mcp--request-timed)
                 (lambda (_server method params _timeout callback)
                   (setq request (list method params))
                   (funcall callback '(:contents ((:uri "docs://fixed" :text "hello"))) nil))))
        (pai-mcp-catalog-call-resource
         "s" '(:name "read_x" :pai-mcp-resource (:uri "docs://fixed")) '(:uri "docs://arbitrary")
         (lambda (value) (setq result value)))
        (should (equal request '("resources/read" (:uri "docs://fixed"))))
        (should-not (pai-truthy (plist-get result :is-error)))))))

(provide 'pai-mcp-catalog-test)
;;; pai-mcp-catalog-test.el ends here
