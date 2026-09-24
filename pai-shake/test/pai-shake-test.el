;;; pai-shake-test.el --- Tests for the shake extension -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-shake)

;;;; Fixtures

(defun pai-shake-test--big (n)
  "Return a line of N `x' characters."
  (make-string n ?x))

(defun pai-shake-test--fence (&optional n)
  "Return a fenced code block whose body is N (default 4000) characters."
  (concat "```elisp\n" (pai-shake-test--big (or n 4000)) "\n```"))

(defun pai-shake-test--messages ()
  "Return a small transcript: system, user with a fence, tool call + result."
  (list (pai-system-message "You are pai.")
        (pai-user-message (concat "look at this\n" (pai-shake-test--fence) "\nplease"))
        (pai-assistant-message
         :content (list (pai-text "on it")
                        (pai-tool-call "c1" "read" '(:path "/tmp/x.el"))))
        (pai-tool-result-message :tool-call-id "c1" :tool-name "read"
                                 :content (pai-shake-test--big 8000))))

(defun pai-shake-test--all-regions (messages)
  "Collect every region of MESSAGES with no recency protection."
  (pai-shake-collect-regions messages '(:protect-tokens 0)))

;;;; Block scanning

(ert-deftest pai-shake-scan-finds-fenced-block ()
  "A terminated fence yields one range covering both fence lines."
  (let* ((text "before\n```\nbody\n```\nafter")
         (ranges (pai-shake-scan-block-ranges text)))
    (should (= (length ranges) 1))
    (should (equal (substring text (caar ranges) (cdar ranges))
                   "```\nbody\n```"))))

(ert-deftest pai-shake-scan-ignores-unterminated-fence ()
  "An unterminated fence is never a region (conservative by design)."
  (should-not (pai-shake-scan-block-ranges "text\n```\nbody never closed\n")))

(ert-deftest pai-shake-scan-finds-xml-span ()
  "A top-level lowercase XML element yields one range."
  (let* ((text "intro\n<result>\ndata\n</result>\ntail")
         (ranges (pai-shake-scan-block-ranges text)))
    (should (= (length ranges) 1))
    (should (equal (substring text (caar ranges) (cdar ranges))
                   "<result>\ndata\n</result>"))))

(ert-deftest pai-shake-scan-keeps-outermost-xml-only ()
  "Nested elements collapse into the single outermost span."
  (let ((ranges (pai-shake-scan-block-ranges
                 "<outer>\n<inner>\nx\n</inner>\n</outer>")))
    (should (= (length ranges) 1))
    (should (equal (car ranges) (cons 0 (length "<outer>\n<inner>\nx\n</inner>\n</outer>"))))))

(ert-deftest pai-shake-scan-suppresses-xml-inside-fence ()
  "XML detection is off inside a fence; only the fence is a region."
  (let* ((text "```\n<tag>\nx\n</tag>\n```")
         (ranges (pai-shake-scan-block-ranges text)))
    (should (= (length ranges) 1))
    (should (= (caar ranges) 0))))

(ert-deftest pai-shake-scan-ignores-indented-and-uppercase-tags ()
  "Opening tags must start at column 0 and be lowercase."
  (should-not (pai-shake-scan-block-ranges "  <tag>\nx\n</tag>"))
  (should-not (pai-shake-scan-block-ranges "<TAG>\nx\n</TAG>")))

;;;; Region collection

(ert-deftest pai-shake-collects-tool-result-and-block ()
  "Both a big fenced block and a whole tool result are eligible."
  (let* ((regions (pai-shake-test--all-regions (pai-shake-test--messages)))
         (kinds (mapcar (lambda (r) (plist-get r :kind)) regions)))
    (should (equal kinds '(block tool-result)))
    (should (= (plist-get (nth 0 regions) :index) 1))
    (should (= (plist-get (nth 1 regions) :index) 3))
    (should (string-prefix-p "```elisp" (plist-get (nth 0 regions) :text)))
    (should (equal (plist-get (nth 1 regions) :label) "read"))))

(ert-deftest pai-shake-never-touches-the-system-prompt ()
  "System messages are skipped even when they carry a huge block."
  (let ((messages (list (pai-system-message
                         (concat "tools\n" (pai-shake-test--fence))))))
    (should-not (pai-shake-test--all-regions messages))))

(ert-deftest pai-shake-protects-the-recent-tail ()
  "Nothing within the protect-recent window is eligible."
  (should-not (pai-shake-collect-regions (pai-shake-test--messages)
                                         '(:protect-tokens 100000))))

(ert-deftest pai-shake-skips-small-blocks ()
  "A fenced block below the token threshold is left alone."
  (let ((messages (list (pai-user-message (concat "x\n" (pai-shake-test--fence 40))))))
    (should-not (pai-shake-collect-regions messages '(:protect-tokens 0)))))

(ert-deftest pai-shake-honors-min-savings ()
  "Below the savings gate, collection is a no-op."
  (should-not (pai-shake-collect-regions (pai-shake-test--messages)
                                         '(:protect-tokens 0 :min-savings 1000000))))

(ert-deftest pai-shake-protects-configured-tools ()
  "A protected tool's results are never collected."
  (let ((messages (list (pai-tool-result-message
                         :tool-call-id "c1" :tool-name "skill"
                         :content (pai-shake-test--big 8000)))))
    (should-not (pai-shake-test--all-regions messages))
    (should (pai-shake-collect-regions messages
                                       '(:protect-tokens 0 :protected-tools nil)))))

(ert-deftest pai-shake-protects-artifact-recovery-reads ()
  "Reading a recovery artifact back is protected from re-elision."
  (let* ((path (expand-file-name "shake-1.md" (pai-shake-artifact-directory)))
         (messages (list (pai-assistant-message
                          :content (list (pai-tool-call "c1" "read" (list :path path))))
                         (pai-tool-result-message :tool-call-id "c1" :tool-name "read"
                                                  :content (pai-shake-test--big 8000)))))
    (should-not (pai-shake-test--all-regions messages))))

(ert-deftest pai-shake-skips-already-pruned-results ()
  "A result shaken once is not shaken again."
  (let* ((messages (pai-shake-test--messages))
         (once (plist-get (pai-shake-run messages 'elide
                                         :config '(:protect-tokens 0)
                                         :artifact-fn nil)
                          :messages))
         (regions (pai-shake-test--all-regions once)))
    (should (plist-get (nth 3 once) :pruned-at))
    (should-not (seq-find (lambda (r) (eq (plist-get r :kind) 'tool-result)) regions))))

(defun pai-shake-test--reveal-messages ()
  "Return a transcript whose first tool result is a deferred-schema reveal."
  (let* ((tool (list :name "frob"
                     :description (concat "Drive the frobnicator.\n"
                                          (pai-shake-test--big 6000))
                     :parameters (pai-object-schema
                                  (list :action (pai-string-schema "Action.")))))
         (reveal (pai-tool-reveal-result tool)))
    (append (pai-shake-test--messages)
            (list (pai-assistant-message
                   :content (list (pai-tool-call "c2" "frob" '(:action "a"))))
                  (pai-tool-result-message :tool-call-id "c2" :tool-name "frob"
                                           :content (plist-get reveal :content)
                                           :details (plist-get reveal :details))))))

(ert-deftest pai-shake-never-removes-deferred-schema-reveal ()
  "A deferred tool's schema reveal survives an elide shake untouched."
  (let* ((messages (pai-shake-test--reveal-messages))
         (reveal (car (last messages)))
         (regions (pai-shake-test--all-regions messages))
         (shaken (plist-get (pai-shake-run messages 'elide
                                           :config '(:protect-tokens 0)
                                           :artifact-fn nil)
                            :messages)))
    (should-not (seq-find (lambda (r) (= (plist-get r :index) 5)) regions))
    ;; other heavy content is still shaken
    (should (plist-get (nth 3 shaken) :pruned-at))
    (should (equal (car (last shaken)) reveal))
    (should (pai-tool-revealed-p '(:name "frob") shaken))))

(ert-deftest pai-shake-never-removes-carried-schema ()
  "A compaction summary carrying tool definitions is never shaken."
  (let* ((carrier (plist-put (pai-user-message
                              (concat "summary\n" (pai-shake-test--fence)))
                             :deferred-schemas '("frob")))
         (messages (list carrier (pai-user-message "next")))
         (shaken (plist-get (pai-shake-run messages 'elide
                                           :config '(:protect-tokens 0 :min-savings 0)
                                           :artifact-fn nil)
                            :messages)))
    (should (equal (car shaken) carrier))))

;;;; Elide

(ert-deftest pai-shake-elide-replaces-regions-and-counts ()
  "Elide swaps placeholders in, preserving structure and reporting counts."
  (let* ((messages (pai-shake-test--messages))
         (result (pai-shake-run messages 'elide
                                :config '(:protect-tokens 0) :artifact-fn nil))
         (out (plist-get result :messages))
         (user (nth 1 out))
         (tool (nth 3 out)))
    (should (= (plist-get result :tool-results-dropped) 1))
    (should (= (plist-get result :blocks-dropped) 1))
    (should (> (plist-get result :tokens-freed) 2000))
    ;; The fence is gone, the surrounding prose is not.
    (should (string-match-p "\\[shaken ~[0-9]+ tokens\\]" (pai-message-content user)))
    (should (string-match-p "look at this" (pai-message-content user)))
    (should (string-match-p "please" (pai-message-content user)))
    ;; Tool result: a single placeholder text block, marked pruned.
    (should (= (length (plist-get tool :content)) 1))
    (should (string-match-p "\\[shaken" (pai-content-text (plist-get tool :content))))
    ;; Tool calls survive, so call/result pairing stays valid.
    (should (= (length (pai-message-tool-calls (nth 2 out))) 1))))

(ert-deftest pai-shake-elide-does-not-mutate-input ()
  "Shaking returns new messages; the originals keep their content."
  (let* ((messages (pai-shake-test--messages))
         (user-before (pai-message-content (nth 1 messages)))
         (tool-before (copy-sequence (plist-get (nth 3 messages) :content))))
    (pai-shake-run messages 'elide :config '(:protect-tokens 0) :artifact-fn nil)
    (should (equal (pai-message-content (nth 1 messages)) user-before))
    (should (equal (plist-get (nth 3 messages) :content) tool-before))
    (should-not (plist-get (nth 3 messages) :pruned-at))))

(ert-deftest pai-shake-elide-multiple-blocks-in-one-text ()
  "Two blocks in one message are both spliced, offsets staying valid."
  (let* ((text (concat "a\n" (pai-shake-test--fence) "\nb\n" (pai-shake-test--fence) "\nc"))
         (messages (list (pai-user-message text)))
         (result (pai-shake-run messages 'elide
                                :config '(:protect-tokens 0) :artifact-fn nil))
         (out (pai-message-content (car (plist-get result :messages)))))
    (should (= (plist-get result :blocks-dropped) 2))
    (should-not (string-match-p "xxxx" out))
    (should (string-match-p "\\`a\n\\[shaken" out))
    (should (string-match-p "\nc\\'" out))
    (should (string-match-p "\nb\n" out))))

(ert-deftest pai-shake-elide-reports-nothing-to-shake ()
  "An empty-handed elide reports zero counts and an empty summary."
  (let ((result (pai-shake-run (list (pai-user-message "hi")) 'elide
                               :config '(:protect-tokens 0) :artifact-fn nil)))
    (should (= (pai-shake-dropped-count result) 0))
    (should (equal (pai-shake-format-summary result) "Nothing to shake."))))

(ert-deftest pai-shake-placeholder-points-at-the-artifact ()
  "With an artifact, each placeholder carries its recovery pointer."
  (let* ((messages (pai-shake-test--messages))
         (result (pai-shake-run messages 'elide
                                :config '(:protect-tokens 0)
                                :artifact-fn (lambda (_regions) "/tmp/shake-x.md")))
         (tool (nth 3 (plist-get result :messages))))
    (should (equal (plist-get result :artifact) "/tmp/shake-x.md"))
    (should (string-match-p "recover: read /tmp/shake-x\\.md (region 2)"
                            (pai-content-text (plist-get tool :content))))))

(ert-deftest pai-shake-artifact-round-trips-to-disk ()
  "The saved artifact holds every original region body."
  (let* ((pai-directory (make-temp-file "pai-shake" t))
         (regions (pai-shake-test--all-regions (pai-shake-test--messages)))
         (path (pai-shake-save-artifact regions)))
    (unwind-protect
        (progn
          (should (file-exists-p path))
          (let ((body (with-temp-buffer (insert-file-contents path) (buffer-string))))
            (should (string-match-p "### region 1 (user" body))
            (should (string-match-p "### region 2 (read" body))
            (should (string-match-p (pai-shake-test--big 100) body))))
      (delete-directory pai-directory t))))

;;;; Images and thinking

(ert-deftest pai-shake-images-strips-image-blocks ()
  "Image blocks go; text stays, and an image-only message keeps a marker."
  (let* ((messages (list (pai-user-message (list (pai-text "see") (pai-image "AAA" "image/png")))
                         (pai-tool-result-message :tool-call-id "c1" :tool-name "shot"
                                                  :content (list (pai-image "BBB" "image/png")))))
         (result (pai-shake-run messages 'images))
         (out (plist-get result :messages)))
    (should (= (plist-get result :images-dropped) 2))
    (should (equal (pai-content-text (pai-message-content (nth 0 out))) "see"))
    (should (equal (pai-content-text (plist-get (nth 1 out) :content)) "[image removed]"))
    (should (equal (pai-shake-format-summary result)
                   "Dropped 2 images from this session."))))

(ert-deftest pai-shake-thinking-drops-reasoning-only ()
  "Thinking blocks are dropped; text and tool calls are untouched."
  (let* ((messages (list (pai-assistant-message
                          :content (list (pai-thinking "hmm")
                                         (pai-text "answer")
                                         (pai-tool-call "c1" "read" '(:path "/x"))))
                         (pai-user-message "next")))
         (result (pai-shake-run messages 'thinking))
         (out (plist-get result :messages)))
    (should (= (plist-get result :thinking-dropped) 1))
    (should (= (length (pai-message-content (nth 0 out))) 2))
    (should (equal (pai-content-text (pai-message-content (nth 0 out))) "answer"))
    (should (equal (pai-shake-format-summary result)
                   "Dropped 1 thinking block from this session."))))

(ert-deftest pai-shake-modes-report-empty-runs ()
  "Images/thinking on a clean transcript report nothing found."
  (let ((messages (list (pai-user-message "hi"))))
    (should (equal (pai-shake-format-summary (pai-shake-run messages 'images))
                   "No images found in this session."))
    (should (equal (pai-shake-format-summary (pai-shake-run messages 'thinking))
                   "No thinking blocks found in this session."))))

(ert-deftest pai-shake-all-does-every-kind ()
  "`all' elides large results and blocks and drops images and thinking at once."
  (let* ((messages (append (pai-shake-test--messages)
                           (list (pai-assistant-message
                                  :content (list (pai-thinking "hmm") (pai-text "done")))
                                 (pai-user-message (list (pai-text "see") (pai-image "AAA" "image/png"))))))
         (result (pai-shake-run messages 'all :config '(:protect-tokens 0) :artifact-fn nil))
         (out (plist-get result :messages)))
    (should (= (plist-get result :tool-results-dropped) 1))
    (should (= (plist-get result :blocks-dropped) 1))
    (should (= (plist-get result :images-dropped) 1))
    (should (= (plist-get result :thinking-dropped) 1))
    (should (> (plist-get result :tokens-freed) 0))
    (should (eq (plist-get result :mode) 'all))
    (should (equal (pai-content-text (pai-message-content (car (last out)))) "see"))
    (should (= (pai-shake-dropped-count result) 4))
    (should (string-match-p
             "\\`Shook 1 tool result \\+ 1 block \\+ 1 image \\+ 1 thinking block (~[0-9]+ tokens"
             (pai-shake-format-summary result)))
    (should (equal (pai-shake-format-summary
                    (pai-shake-run (list (pai-user-message "hi")) 'all :artifact-fn nil))
                   "Nothing to shake."))))

;;;; Mode parsing and summaries

(ert-deftest pai-shake-parses-modes ()
  "Empty means elide; unknown verbs report an error."
  (should (eq (pai-shake-parse-mode "") 'elide))
  (should (eq (pai-shake-parse-mode "  ") 'elide))
  (should (eq (pai-shake-parse-mode "Elide") 'elide))
  (should (eq (pai-shake-parse-mode "images") 'images))
  (should (eq (pai-shake-parse-mode "thinking") 'thinking))
  (should (eq (pai-shake-parse-mode "all") 'all))
  (should (string-match-p "Unknown /shake mode"
                          (plist-get (pai-shake-parse-mode "wobble") :error))))

(ert-deftest pai-shake-summary-lists-both-kinds ()
  "The elide summary names tool results and blocks, plus the artifact."
  (let ((result (list :mode 'elide :tool-results-dropped 1 :blocks-dropped 2
                      :images-dropped 0 :thinking-dropped 0
                      :tokens-freed 1234 :artifact "/tmp/a.md")))
    (should (string-match-p "Shook 1 tool result \\+ 2 blocks (~1234 tokens freed)."
                            (pai-shake-format-summary result)))
    (should (string-match-p "Originals: /tmp/a.md" (pai-shake-format-summary result)))))

;;;; Command surface

(ert-deftest pai-shake-command-is-registered ()
  "The extension registers `/shake' with mode completions."
  (let ((cmd (pai-command-get "shake")))
    (should cmd)
    (should (eq (plist-get cmd :handler) #'pai-shake-command))
    (should (equal (funcall (plist-get cmd :arg-completions) "i") '("images")))
    (should (equal (funcall (plist-get cmd :arg-completions) "a") '("all")))))

(ert-deftest pai-shake-command-without-buffer ()
  "Dispatching without a live pai buffer reports instead of failing."
  (should (equal (plist-get (pai-shake-command "" (list :buffer nil)) :message)
                 "No active pai session to shake")))

(ert-deftest pai-shake-command-shakes-the-live-context ()
  "The command swaps in the shaken context and records a session entry."
  (let ((pai-directory (make-temp-file "pai-shake" t)))
    (unwind-protect
        (with-temp-buffer
          (setq-local pai--session (pai-session-new default-directory 'memory))
          (setq-local pai--active nil)
          ;; A live tail bigger than the protect window, so the older turns are
          ;; actually reachable by a default-configuration shake.
          (setq-local pai--context-messages
                      (append (pai-shake-test--messages)
                              (list (pai-user-message (pai-shake-test--big 20000)))))
          (let ((before (pai-estimate-context-tokens pai--context-messages)))
            (should-not (pai-shake-command "" (list :buffer (current-buffer))))
            (should (< (pai-estimate-context-tokens pai--context-messages) before)))
          ;; Tail untouched, older tool result elided, entry persisted.
          (should (= (length (pai-message-content (nth 4 pai--context-messages))) 20000))
          (should (plist-get (nth 3 pai--context-messages) :pruned-at))
          (let ((entry (car (last (pai-session-entries pai--session)))))
            (should (equal (plist-get entry :type) "shake"))
            (should (equal (plist-get entry :mode) "elide"))
            (should (= (plist-get entry :toolResults) 1))
            (should (> (plist-get entry :tokensFreed) 0))
            (should (file-exists-p (plist-get entry :artifact)))))
      (delete-directory pai-directory t))))

(ert-deftest pai-shake-survives-resume ()
  "A shake of a session-backed context is rebuilt identically on reload (V2 E4)."
  (let* ((pai-directory (make-temp-file "pai-shake" t))
         (file (expand-file-name "s.jsonl" pai-directory)))
    (unwind-protect
        (with-temp-buffer
          (setq-local pai--session (pai-session-new pai-directory file))
          (setq-local pai--active nil)
          (setq-local pai--context-messages nil)
          (dolist (m (append (pai-shake-test--messages)
                             (list (pai-user-message (pai-shake-test--big 20000)))))
            (setq pai--context-messages (append pai--context-messages (list m)))
            (pai-session-append-message pai--session m))
          (pai-shake-command "" (list :buffer (current-buffer)))
          (let ((entry (car (last (pai-session-entries pai--session)))))
            ;; the fenced block in the user message and the tool result
            (should (= (length (plist-get entry :replacements)) 2)))
          ;; later turns after the shake keep working
          (let ((m (pai-user-message "after")))
            (setq pai--context-messages (append pai--context-messages (list m)))
            (pai-session-append-message pai--session m))
          (let ((reloaded (pai-session-context-messages (pai-session-load file))))
            (should (= (length reloaded) (length pai--context-messages)))
            (should (equal (mapcar (lambda (m) (pai-content-text (pai-message-content m))) reloaded)
                           (mapcar (lambda (m) (pai-content-text (pai-message-content m)))
                                   pai--context-messages)))
            (should (plist-get (nth 3 reloaded) :pruned-at))))
      (delete-directory pai-directory t))))

(ert-deftest pai-shake-command-rejects-unknown-mode ()
  "An unknown mode never touches the context."
  (with-temp-buffer
    (setq-local pai--context-messages (pai-shake-test--messages))
    (let ((out (pai-shake-command "wobble" (list :buffer (current-buffer)))))
      (should (string-match-p "Unknown /shake mode" (plist-get out :message))))))

(provide 'pai-shake-test)
;;; pai-shake-test.el ends here
