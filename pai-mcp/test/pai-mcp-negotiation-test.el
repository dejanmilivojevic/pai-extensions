;;; pai-mcp-negotiation-test.el --- MCP negotiation tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai-mcp-client)
(require 'pai-mcp-negotiation)

(defconst pai-mcp-negotiation-test--discover
  '(:supportedVersions ("2026-07-28") :capabilities (:tools (:listChanged t))
    :instructions "Use tools" :resultType "complete" :ttlMs 0 :cacheScope "private"
    :_meta (:io.modelcontextprotocol/serverInfo (:name "fixture" :version "1"))))

(defmacro pai-mcp-negotiation-test--with-http (mode responses &rest body)
  "Connect MODE to scripted RESPONSES, then run BODY with observable state."
  (declare (indent 2))
  `(let ((server (list :name "negotiation" :transport 'http
                       :def (list :protocolVersion ,mode)
                       :pending (make-hash-table :test 'equal)))
         (replies ,responses) requests notifications ready error)
     (cl-letf (((symbol-function 'pai-mcp--request)
                (lambda (_server method params callback)
                  (push (cons method params) requests)
                  (let ((reply (pop replies)))
                    (unless reply (ert-fail "Unexpected extra handshake request"))
                    (funcall callback (car reply) (cadr reply)))
                  1))
               ((symbol-function 'pai-mcp--notify)
                (lambda (_server method &optional _params) (push method notifications))))
       (pai-mcp-protocol-connect server (lambda () (setq ready t))
                                 (lambda (message) (setq error message)))
       ,@body)))

(ert-deftest pai-mcp-negotiation-legacy-preserves-offer-and-order ()
  (let ((pai-mcp-protocol-version "2024-11-05"))
    (pai-mcp-negotiation-test--with-http nil
        (list (list '(:protocolVersion "2024-11-05" :capabilities (:tools nil)) nil))
      (should ready)
      (should-not error)
      (should (equal (mapcar #'car requests) '("initialize")))
      (should (equal (plist-get (cdar requests) :protocolVersion) "2024-11-05"))
      (should (equal notifications '("notifications/initialized"))))))

(ert-deftest pai-mcp-negotiation-modern-discovery-does-not-initialize ()
  (pai-mcp-negotiation-test--with-http "auto"
      (list (list pai-mcp-negotiation-test--discover nil))
    (should ready)
    (should-not error)
    (should (equal (mapcar #'car requests) '("server/discover")))
    (should-not notifications)
    (should (equal (plist-get server :protocol-version) "2026-07-28"))
    (should (equal (plist-get server :instructions) "Use tools"))
    (should (eq (plist-get (plist-get (plist-get server :capabilities) :tools)
                          :listChanged) t))))

(ert-deftest pai-mcp-negotiation-auto-fallback-clears-modern-envelope ()
  (pai-mcp-negotiation-test--with-http "auto"
      (list (list nil '(:code -32601 :message "Unknown method"))
            (list '(:protocolVersion "2024-11-05" :capabilities nil) nil))
    (should ready)
    (should-not error)
    (should (equal (reverse (mapcar #'car requests)) '("server/discover" "initialize")))
    (should-not (plist-get (cdar requests) :_meta))
    (should-not (plist-get server :protocol-probing))
    (should (equal notifications '("notifications/initialized")))))

(ert-deftest pai-mcp-negotiation-pin-never-silently-downgrades ()
  (pai-mcp-negotiation-test--with-http "2026-07-28"
      ;; initialize-shaped success is NOT a DiscoverResult.
      (list (list '(:protocolVersion "2026-07-28" :capabilities nil) nil))
    (should-not ready)
    (should (string-match-p "no fallback" error))
    (should-not notifications)
    (should (equal (mapcar #'car requests) '("server/discover")))))

(ert-deftest pai-mcp-negotiation-corrective-version-request-is-bounded ()
  (let ((rejection '(:code -32022 :message "Unsupported protocol"
                    :data (:supported ("2026-07-28") :requested "2026-07-28"))))
    (pai-mcp-negotiation-test--with-http "auto"
        (list (list nil rejection) (list nil rejection))
      (should-not ready)
      (should error)
      (should (equal (mapcar #'car requests) '("server/discover" "server/discover")))
      (should-not notifications))))

(ert-deftest pai-mcp-negotiation-outage-and-auth-are-not-era-evidence ()
  (dolist (failure '((:kind http :status 503 :message "unavailable")
                     (:kind http :status 401 :message "unauthorized")
                     (:kind network :message "connection refused")
                     (:kind shape :message "HTML response")
                     (:kind timeout :message "timed out")))
    (pai-mcp-negotiation-test--with-http "auto" (list (list nil failure))
      (should-not ready)
      (should error)
      (should (equal (mapcar #'car requests) '("server/discover")))
      (when (eq (plist-get failure :status) 503)
        (should (string-match-p "temporarily unavailable" error)))
      (when (eq (plist-get failure :kind) 'shape)
        (should (string-match-p "endpoint URL" error))))))

(ert-deftest pai-mcp-negotiation-http-body-version-error-is-not-lost ()
  (pai-mcp-negotiation-test--with-http "auto"
      (list (list nil '(:kind http :status 400
                       :body "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32022,\"message\":\"Unsupported\",\"data\":{\"supported\":[\"2027-01-01\"]}}}")))
    (should-not ready)
    (should (string-match-p "Unsupported MCP" error))
    (should (equal (mapcar #'car requests) '("server/discover")))))

(ert-deftest pai-mcp-negotiation-discovery-requires-capability-schema ()
  (pai-mcp-negotiation-test--with-http "2026-07-28"
      (list (list '(:supportedVersions ("2026-07-28")
                    :capabilities (:tools (:listChanged "yes"))) nil))
    (should-not ready)
    (should error)))

(ert-deftest pai-mcp-negotiation-envelope-preserves-caller-meta-and-headers ()
  (let* ((server '(:protocol-version "2026-07-28"))
         (object '(:jsonrpc "2.0" :id 9 :method "tools/call"
                   :params (:name " café " :arguments (:x 1)
                            :_meta (:progressToken "progress"
                                    :io.modelcontextprotocol/clientInfo (:name "custom" :version "2")))))
         (decorated (pai-mcp-protocol-decorate server object))
         (wire (pai-json-decode (pai-json-encode decorated)))
         (meta (plist-get (plist-get wire :params) :_meta))
         (headers (pai-mcp-protocol-headers server decorated)))
    (should (equal (plist-get meta :progressToken) "progress"))
    (should (equal (plist-get (plist-get meta :io.modelcontextprotocol/clientInfo) :name) "custom"))
    (should (equal (plist-get meta :io.modelcontextprotocol/protocolVersion) "2026-07-28"))
    (should (plist-member meta :io.modelcontextprotocol/clientCapabilities))
    (should-not (plist-member (plist-get (plist-get object :params) :_meta)
                             :io.modelcontextprotocol/protocolVersion))
    (should (equal (cdr (assoc "Mcp-Method" headers)) "tools/call"))
    (should (equal (cdr (assoc "MCP-Protocol-Version" headers)) "2026-07-28"))
    (should (equal (decode-coding-string
                    (base64-decode-string (substring (cdr (assoc "Mcp-Name" headers)) 9 -2)) 'utf-8)
                   " café "))
    (should (eq object (pai-mcp-protocol-decorate '(:protocol-version "2024-11-05") object))))
  ;; The common client uses an empty hash table for parameterless requests.
  (let* ((request (list :id 1 :method "tools/list" :params (pai-json-empty-object)))
         (result (pai-mcp-protocol-decorate '(:protocol-version "2026-07-28") request)))
    (should (plist-get (plist-get result :params) :_meta))))

(ert-deftest pai-mcp-negotiation-sibling-exit-keeps-session-unspent ()
  "A legacy process that exits on discover must get a fresh session afterward."
  (let* ((pai-mcp-startup-timeout 2)
         (server (list :name "sibling" :transport 'stdio
                       :def (list :protocolVersion "auto"
                                  :command (expand-file-name invocation-name invocation-directory)
                                  :args '("-Q" "--batch" "--eval" "(progn (read-string \"\") (kill-emacs 0))"))))
         ready error events)
    (pai-mcp--set server :protocol-start-session (lambda () (push 'session-start events)))
    (cl-letf (((symbol-function 'pai-mcp--request)
               (lambda (_server method _params callback)
                 (should (equal method "initialize"))
                 (should (equal events '(session-start)))
                 (funcall callback '(:protocolVersion "2024-11-05" :capabilities nil) nil)))
              ((symbol-function 'pai-mcp--notify)
               (lambda (_server _method &optional _params) (push 'initialized events))))
      (unwind-protect
          (progn
            (pai-mcp-protocol-connect server (lambda () (setq ready t))
                                      (lambda (message) (setq error message)))
            (let ((deadline (+ (float-time) 5)))
              (while (and (not ready) (not error) (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (should-not error)
            (should ready)
            (should (equal events '(initialized session-start)))
            (should-not (plist-get server :protocol-cancel)))
        (when-let* ((cancel (plist-get server :protocol-cancel))) (funcall cancel))))))

(provide 'pai-mcp-negotiation-test)
;;; pai-mcp-negotiation-test.el ends here
