;;; pai-web-instances.el --- pai instances seen from the browser -*- lexical-binding: t; -*-

;;; Commentary:

;; Ids, the instance list, each instance's "chrome" (header, statuses,
;; panels above the prompt, footer) and a structured transcript log.
;;
;; The log mirrors what the chat buffer shows.  While the server runs, the
;; chat buffer's render primitives are advised (`pai-web-instances-install'):
;; a user prompt, a note, an assistant block (streamed deltas, then the
;; final text), a tool call and its result each become an item:
;;
;;   (:id N :kind "user"      :text S :images N)
;;   (:id N :kind "note"      :text S :html S :face S)
;;   (:id N :kind "assistant" :blocks [(:type "text"|"thinking" :text S)]
;;          :streaming BOOL :fences [HTML...])
;;   (:id N :kind "tool"      :call ID :name S :args S :status S
;;          :result S :truncated BOOL :html S)
;;
;; Rebuilding the transcript (/resume, /tree, /new, /clear) starts a new
;; generation of the log; the page then reloads it.  A buffer that had
;; content before the server started gets a log built from its context.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'pai-core)
(require 'pai-ext)
(require 'pai-web-util)
(require 'pai-web-bus)

(defvar pai--model)
(defvar pai--reasoning)
(defvar pai--status)
(defvar pai--working-message)
(defvar pai--active)
(defvar pai--compaction)
(defvar pai--context-tokens)
(defvar pai--context-messages)
(defvar pai--session)
(defvar pai--ext-statuses)
(defvar pai--ext-header)
(defvar pai--steering-queue)
(defvar pai--usage-summary)
(defvar pai--assistant-open)
(defvar pai-prompt-string)
(declare-function pai--cost-text "pai-ui" ())
(declare-function pai--mode-line "pai-ui" ())
(declare-function pai--prompt-start "pai-ui" ())
(declare-function pai--tool-result-mode "pai-ui" (name args))
(declare-function pai-model-key "pai-models" (model))
(declare-function pai-session-name "pai-session" (session))
(declare-function pai-diff-render "pai-diff" (old new &optional old-name new-name))
(declare-function pai-md-fontify "pai-markdown" (code mode))
(declare-function pai-md-fontify-lang "pai-markdown" (code lang))
(declare-function pai-tools-truncate "pai-tools" (text max-lines max-bytes &optional from))
(declare-function pai-ask-user-pending "pai-ask-user" ())
(declare-function pai-ask-user-request-chat-buffer "pai-ask-user" (req))

(defconst pai-web-item-text-limit 4000
  "Characters of a tool's arguments or result put in an item.")

(defconst pai-web-fence-limit 20000
  "Code fences longer than this are not highlighted.")

(defvar pai-web--cheap nil
  "Non-nil while a log is built from a context: skip highlighting.
Highlighting runs a major mode per code block; doing that for a whole
long session at once would stall Emacs.")

;;;; Ids

(defvar pai-web--ids (make-hash-table :test 'eq :weakness 'key)
  "Map of buffer to its web id.")

(defvar pai-web--by-id (make-hash-table :test 'equal :weakness 'value)
  "Map of web id to buffer.")

(defvar pai-web--next-id 0 "Last id number handed out.")

(defun pai-web-id (buffer)
  "Return BUFFER's web id: \"iN\" for pai chats, \"bN\" for other buffers."
  (or (gethash buffer pai-web--ids)
      (let ((id (format "%s%d" (if (eq (buffer-local-value 'major-mode buffer) 'pai-mode)
                                   "i" "b")
                        (cl-incf pai-web--next-id))))
        (puthash buffer id pai-web--ids)
        (puthash id buffer pai-web--by-id)
        id)))

(defun pai-web-buffer (id)
  "Return the live buffer with web ID, or nil."
  (let ((b (and (stringp id) (gethash id pai-web--by-id))))
    (and (buffer-live-p b) b)))

(defun pai-web-instance (id)
  "Return the live pai chat buffer with web ID, or nil."
  (let ((b (pai-web-buffer id)))
    (and b (eq (buffer-local-value 'major-mode b) 'pai-mode) b)))

;;;; Instances

(defun pai-web-instances ()
  "Return every live pai chat buffer, subagents included, most recent first."
  (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) 'pai-mode))
              (buffer-list)))

(defun pai-web--asks-for (buffer)
  "Return how many ask_user questions wait on BUFFER."
  (if (fboundp 'pai-ask-user-pending)
      (seq-count (lambda (r) (eq (pai-ask-user-request-chat-buffer r) buffer))
                 (ignore-errors (pai-ask-user-pending)))
    0))

(defun pai-web-instance-info (buffer)
  "Return the list entry of instance BUFFER (a plist for JSON)."
  (with-current-buffer buffer
    (let* ((window (or (and pai--model (plist-get pai--model :context-window)) 0))
           (parent (and (boundp 'pai-subagent-session) pai-subagent-session))
           (name (and pai--session (ignore-errors (pai-session-name pai--session)))))
      (list :id (pai-web-id buffer)
            :name (buffer-name buffer)
            :title (or name :null)
            :cwd (abbreviate-file-name default-directory)
            :project (file-name-nondirectory (directory-file-name default-directory))
            :model (if pai--model (pai-model-key pai--model) :null)
            :thinking (if pai--reasoning (symbol-name pai--reasoning) "off")
            :status (or pai--working-message pai--status "idle")
            :active (pai-web-bool pai--active)
            :compacting (pai-web-bool pai--compaction)
            :ctx (or pai--context-tokens 0)
            :window window
            :pct (if (> window 0) (round (* 100.0 (/ (float (or pai--context-tokens 0)) window))) 0)
            :cost (or (ignore-errors (pai--cost-text)) "")
            :parent (if (buffer-live-p parent) (pai-web-id parent) :null)
            :subagent (pai-web-bool parent)
            :asks (pai-web--asks-for buffer)
            :queued (length pai--steering-queue)))))

(defun pai-web-instance-list ()
  "Return the instance list as a vector of entries."
  (vconcat (mapcar #'pai-web-instance-info (pai-web-instances))))

(defun pai-web--prompt-overlays-html ()
  "Return what is drawn above the prompt (panels, footer, activity) as HTML."
  (let ((pos (ignore-errors (pai--prompt-start))))
    (if (not pos)
        ""
      (let* ((ovs (seq-filter (lambda (o) (or (overlay-get o 'before-string)
                                              (overlay-get o 'after-string)))
                              (overlays-in (max (point-min) (1- pos)) (1+ pos))))
             (ovs (sort ovs (lambda (a b) (< (or (overlay-get a 'priority) 0)
                                             (or (overlay-get b 'priority) 0))))))
        (mapconcat (lambda (o)
                     (concat (pai-web-propertized-html (or (overlay-get o 'before-string) "") 20000)
                             (pai-web-propertized-html (or (overlay-get o 'after-string) "") 20000)))
                   ovs "")))))

(defun pai-web-instance-chrome (buffer)
  "Return the detailed view state of instance BUFFER (a plist for JSON)."
  (with-current-buffer buffer
    (append
     (pai-web-instance-info buffer)
     (list :header (pai-web-propertized-html (or pai--ext-header "") 4000)
           :statuses (vconcat (mapcar (lambda (s) (pai-web-propertized-html (cdr s) 2000))
                                      (reverse pai--ext-statuses)))
           :usage (pai-web-propertized-html (or pai--usage-summary "") 2000)
           :above (pai-web--prompt-overlays-html)
           :footer (pai-web-propertized-html (or (ignore-errors (pai--mode-line)) "") 4000)))))

;;;; Transcript log

(cl-defstruct (pai-web-log (:constructor pai-web-log--create))
  "The browser's view of one chat transcript."
  (gen 0) (next 0)
  (items (make-hash-table :test 'eql))  ; id -> item plist
  (order nil)                           ; ids, newest first
  assistant                             ; id of the open assistant item
  stream                                ; open assistant blocks: list of [TYPE CHUNKS]
  (tools (make-hash-table :test 'equal))) ; tool-call id -> item id

(defvar pai-web--logs (make-hash-table :test 'eq :weakness 'key)
  "Map of chat buffer to its `pai-web-log'.")

(defvar pai-web--gen 0 "Last log generation handed out.")

(defconst pai-web-log-max-items 3000 "Items kept per log.")

(defun pai-web-log (buffer)
  "Return BUFFER's log, building it from the context when it has none."
  (or (gethash buffer pai-web--logs)
      (let ((log (pai-web-log--create :gen (cl-incf pai-web--gen))))
        (puthash buffer log pai-web--logs)
        (with-current-buffer buffer
          (let ((pai-web--cheap t))
            (dolist (m (last pai--context-messages 800))
              (unless (pai-system-message-p m)
                (pai-web--log-message buffer log m)))))
        log)))

(defun pai-web--log-message (buffer log message)
  "Add the items for context MESSAGE to LOG of BUFFER (no broadcast)."
  (pcase (pai-message-role message)
    ('user (pai-web--log-put buffer log (pai-web--user-item message) t))
    ('assistant
     (let ((blocks (pai-web--message-blocks message)))
       (when (> (length blocks) 0)
         (pai-web--log-put buffer log (list :kind "assistant" :blocks blocks :streaming :false
                                            :fences (pai-web--fences blocks))
                           t)))
     (dolist (tc (pai-message-tool-calls message))
       (pai-web--tool-start buffer log (plist-get tc :id) (plist-get tc :name)
                            (plist-get tc :arguments) t)))
    ('tool-result
     (pai-web--tool-end buffer log (plist-get message :tool-call-id)
                        (plist-get message :tool-name) message
                        (pai-truthy (plist-get message :is-error)) t))
    (_ nil)))

(defun pai-web--log-put (buffer log item &optional quiet)
  "Add ITEM to LOG of BUFFER; broadcast it unless QUIET.  Return its id."
  (let ((id (cl-incf (pai-web-log-next log))))
    (setq item (plist-put item :id id))
    (puthash id item (pai-web-log-items log))
    (push id (pai-web-log-order log))
    (when (> (length (pai-web-log-order log)) (+ pai-web-log-max-items 200))
      (let ((drop (nthcdr pai-web-log-max-items (pai-web-log-order log))))
        (dolist (old drop) (remhash old (pai-web-log-items log)))
        (setcdr (nthcdr (1- pai-web-log-max-items) (pai-web-log-order log)) nil)))
    (unless quiet (pai-web--broadcast-item buffer log item))
    id))

(defun pai-web--log-replace (buffer log id item &optional quiet)
  "Replace item ID of LOG (BUFFER) with ITEM; broadcast unless QUIET."
  (when (gethash id (pai-web-log-items log))
    (setq item (plist-put item :id id))
    (puthash id item (pai-web-log-items log))
    (unless quiet (pai-web--broadcast-item buffer log item))))

(defun pai-web--broadcast-item (buffer log item)
  "Send ITEM of LOG (chat BUFFER) to the pages."
  (pai-web-bus-broadcast (list :t "item" :i (pai-web-id buffer) :g (pai-web-log-gen log)
                               :item (pai-web--item-json log item))))

(defun pai-web--item-json (log item)
  "Return ITEM of LOG ready for JSON (the open assistant's text joined)."
  (if (and (equal (plist-get item :kind) "assistant")
           (eql (plist-get item :id) (pai-web-log-assistant log))
           (pai-web-log-stream log))
      (plist-put (copy-sequence item) :blocks
                 (vconcat (mapcar (lambda (b)
                                    (list :type (aref b 0)
                                          :text (apply #'concat (reverse (aref b 1)))))
                                  (reverse (pai-web-log-stream log)))))
    item))

(defun pai-web-log-snapshot (buffer &optional before limit)
  "Return BUFFER's transcript: up to LIMIT items older than id BEFORE."
  (let* ((log (pai-web-log buffer))
         (ids (pai-web-log-order log))
         (ids (if before (seq-filter (lambda (id) (< id before)) ids) ids))
         (limit (or limit 150))
         (page (seq-take ids limit)))
    (list :i (pai-web-id buffer) :g (pai-web-log-gen log)
          :more (pai-web-bool (> (length ids) limit))
          :items (vconcat (mapcar (lambda (id) (pai-web--item-json
                                                 log (gethash id (pai-web-log-items log))))
                                  (reverse page))))))

;;;; Items

(defun pai-web--user-item (message)
  "Return the item of user MESSAGE."
  (let ((content (pai-message-content message)))
    (list :kind "user" :text (pai-content-text content)
          :images (if (listp content)
                      (seq-count (lambda (b) (eq (pai-block-type b) 'image)) content)
                    0))))

(defun pai-web--message-blocks (message)
  "Return the text and thinking blocks of assistant MESSAGE as a vector."
  (vconcat
   (delq nil
         (mapcar (lambda (b)
                   (pcase (pai-block-type b)
                     ('text (let ((s (or (plist-get b :text) "")))
                              (unless (string-empty-p s) (list :type "text" :text s))))
                     ('thinking (let ((s (string-trim-right (or (plist-get b :thinking) ""))))
                                  (unless (string-empty-p s) (list :type "thinking" :text s))))))
                 (pai-message-content message)))))

(defun pai-web--fences (blocks)
  "Return the highlighted HTML of the code fences in BLOCKS' text, in order."
  (let (out)
    (seq-doseq (b blocks)
      (when (equal (plist-get b :type) "text")
        (let ((text (plist-get b :text)) (start 0))
          (while (string-match "^\\([ \t]*\\)\\(```+\\|~~~+\\)[ \t]*\\([^`\n]*\\)\n" text start)
            (let* ((fence (match-string 2 text))
                   (lang (string-trim (match-string 3 text)))
                   (body-start (match-end 0))
                   (close (string-match (concat "^[ \t]*" (regexp-quote fence) "[ \t]*$")
                                        text body-start))
                   (body (substring text body-start (or close (length text))))
                   ;; read before highlighting: that runs a major mode,
                   ;; which changes the match data
                   (next (if close (min (length text) (1+ (match-end 0))) (length text))))
              (push (if (or pai-web--cheap
                            (> (length body) pai-web-fence-limit)
                            (string-empty-p lang))
                        :null
                      (save-match-data
                        (pai-web-propertized-html
                         (or (ignore-errors (pai-md-fontify-lang (string-trim-right body "\n") lang))
                             body))))
                    out)
              (setq start next))))))
    (vconcat (nreverse out))))

(defun pai-web--clip (text)
  "Return (TEXT-CLIPPED . TRUNCATED-P) for an item."
  (if (> (length text) pai-web-item-text-limit)
      (cons (substring text 0 pai-web-item-text-limit) t)
    (cons text nil)))

(defun pai-web--args-text (args)
  "Return tool ARGS as indented JSON text."
  (condition-case nil
      (let ((json (pai-json-encode (or args (pai-json-empty-object)))))
        (if (or pai-web--cheap (> (length json) 100000))
            json
          (with-temp-buffer
            (insert json)
            (ignore-errors (json-pretty-print-buffer))
            (buffer-string))))
    (error (format "%S" args))))

(defun pai-web--tool-start (buffer log call-id name args &optional quiet)
  "Add a running tool item for CALL-ID (tool NAME with ARGS) to LOG."
  (let* ((clip (pai-web--clip (pai-web--args-text args)))
         (id (pai-web--log-put buffer log
                               (list :kind "tool" :call (or call-id "") :name (or name "?")
                                     :args (car clip) :argsTruncated (pai-web-bool (cdr clip))
                                     :status "running")
                               quiet)))
    (when call-id (puthash call-id id (pai-web-log-tools log)))
    (setf (pai-web-log-assistant log) nil)
    id))

(defun pai-web--tool-end (buffer log call-id name result is-error &optional quiet)
  "Record RESULT (IS-ERROR) of tool CALL-ID (NAME) in LOG of BUFFER."
  (let* ((id (or (and call-id (gethash call-id (pai-web-log-tools log)))
                 (pai-web--tool-start buffer log call-id name nil quiet)))
         (item (copy-sequence (gethash id (pai-web-log-items log))))
         (details (plist-get result :details))
         (old (and (listp details) (plist-get details :old)))
         (new (and (listp details) (plist-get details :new)))
         (text (pai-content-text (plist-get result :content)))
         (clip (pai-web--clip text))
         (html nil))
    (ignore-errors
      (cond
       (pai-web--cheap nil)
       ((and (not is-error) (stringp old) (stringp new) (not (equal old new))
             (< (+ (length old) (length new)) 400000))
        (let ((path (or (plist-get details :path) "")))
          (setq html (pai-web-propertized-html (pai-diff-render old new path path) 60000))))
       ((not is-error)
        (let ((mode (pai--tool-result-mode name (with-current-buffer buffer
                                                  (pai-web--call-args log call-id)))))
          (when mode
            (setq html (pai-web-propertized-html (pai-md-fontify (car clip) mode))))))))
    (pai-web--log-replace buffer log id
                          (plist-put (plist-put (plist-put (plist-put item :status (if is-error "error" "done"))
                                                           :result (car clip))
                                                :truncated (pai-web-bool (cdr clip)))
                                     :html (or html :null))
                          quiet)))

(defvar pai-web--call-args (make-hash-table :test 'equal :weakness 'value)
  "Map of tool-call id to its arguments, for highlighting results.")

(defun pai-web--call-args (_log call-id)
  "Return the arguments tool CALL-ID was called with, if known."
  (and call-id (gethash call-id pai-web--call-args)))

;;;; Advice on the chat buffer's rendering

(defvar pai-web--deltas nil
  "Pending streamed text: list of (I G ID BLOCK TYPE CHUNKS), newest first.")

(defvar pai-web--delta-timer nil "Timer sending `pai-web--deltas'.")

(defun pai-web--chat-p ()
  "Return non-nil in a pai chat buffer."
  (eq major-mode 'pai-mode))

(defun pai-web--after-init-buffer (&rest _)
  "The transcript was cleared: start a new log generation."
  (when (pai-web--chat-p)
    (let ((log (pai-web-log--create :gen (cl-incf pai-web--gen))))
      (puthash (current-buffer) log pai-web--logs)
      (pai-web-bus-broadcast (list :t "reset" :i (pai-web-id (current-buffer))
                                   :g (pai-web-log-gen log))))))

(defun pai-web--after-render-user (message &rest _)
  "Log the user MESSAGE just rendered."
  (when (pai-web--chat-p)
    (let ((log (pai-web-log (current-buffer))))
      (setf (pai-web-log-assistant log) nil)
      (pai-web--log-put (current-buffer) log (pai-web--user-item message)))))

(defun pai-web--after-render-note (text &optional face)
  "Log the note TEXT (with FACE) just rendered."
  (when (pai-web--chat-p)
    (let* ((log (pai-web-log (current-buffer)))
           (text (string-trim-right (or text ""))))
      (pai-web--log-put (current-buffer) log
                        (list :kind "note" :text (substring-no-properties text)
                              :html (pai-web-propertized-html text 20000)
                              :face (symbol-name (or face 'pai-note-face)))))))

(defun pai-web--around-open-assistant (orig &rest args)
  "Call ORIG with ARGS; log a new assistant block when one opened."
  (let ((was (and (pai-web--chat-p) pai--assistant-open)))
    (prog1 (apply orig args)
      (when (and (pai-web--chat-p) (not was) pai--assistant-open)
        (let ((log (pai-web-log (current-buffer))))
          (setf (pai-web-log-stream log) nil)
          (setf (pai-web-log-assistant log)
                (pai-web--log-put (current-buffer) log
                                  (list :kind "assistant" :blocks [] :streaming t :fences []))))))))

(defun pai-web--after-handle-update (event &rest _)
  "Log the streamed text of EVENT into the open assistant item."
  (when (pai-web--chat-p)
    (let ((type (pcase (plist-get event :type)
                  ('text-delta "text") ('thinking-delta "thinking")))
          (delta (plist-get event :delta)))
      (when (and type (stringp delta) (not (string-empty-p delta)))
        (let* ((log (pai-web-log (current-buffer)))
               (id (pai-web-log-assistant log)))
          (when id
            (let ((last (car (pai-web-log-stream log))))
              (if (and last (equal (aref last 0) type))
                  (aset last 1 (cons delta (aref last 1)))
                (push (vector type (list delta)) (pai-web-log-stream log))))
            (pai-web--queue-delta (pai-web-id (current-buffer)) (pai-web-log-gen log) id
                                  (1- (length (pai-web-log-stream log))) type delta)))))))

(defun pai-web--queue-delta (i g id block type text)
  "Queue streamed TEXT for block BLOCK (TYPE) of item ID in instance I, gen G."
  (when (pai-web-bus-active-p)
    (let ((head (car pai-web--deltas)))
      (if (and head (equal (nth 0 head) i) (eql (nth 2 head) id) (eql (nth 3 head) block)
               (eql (nth 1 head) g))
          (setcar (nthcdr 5 head) (cons text (nth 5 head)))
        (push (list i g id block type (list text)) pai-web--deltas)))
    (unless (timerp pai-web--delta-timer)
      (setq pai-web--delta-timer (run-at-time 0.05 nil #'pai-web--send-deltas)))))

(defun pai-web--send-deltas ()
  "Broadcast the queued streamed text."
  (setq pai-web--delta-timer nil)
  (let ((deltas (reverse pai-web--deltas)))
    (setq pai-web--deltas nil)
    (dolist (d deltas)
      (pai-web-bus-broadcast (list :t "delta" :i (nth 0 d) :g (nth 1 d) :id (nth 2 d)
                                   :b (nth 3 d) :k (nth 4 d)
                                   :s (apply #'concat (reverse (nth 5 d))))))))

(defun pai-web--after-insert-assistant-blocks (message &rest _)
  "Set the open assistant item to MESSAGE's final blocks."
  (when (pai-web--chat-p)
    (pai-web--send-deltas)              ; keep order: deltas before the final item
    (let* ((buffer (current-buffer))
           (log (pai-web-log buffer))
           (blocks (pai-web--message-blocks message))
           (item (list :kind "assistant" :blocks blocks :streaming :false
                       :fences (pai-web--fences blocks)))
           (id (pai-web-log-assistant log)))
      (setf (pai-web-log-stream log) nil)
      (if (and id (gethash id (pai-web-log-items log)))
          (pai-web--log-replace buffer log id item)
        (setf (pai-web-log-assistant log) (pai-web--log-put buffer log item))))))

(defun pai-web--after-finish-assistant (&rest _)
  "The assistant block was closed."
  (when (pai-web--chat-p)
    (let* ((log (pai-web-log (current-buffer)))
           (id (pai-web-log-assistant log))
           (item (and id (gethash id (pai-web-log-items log)))))
      ;; streamed but never formatted (e.g. aborted): keep the streamed text
      (when (and item (pai-truthy (plist-get item :streaming)))
        (pai-web--send-deltas)
        (let ((final (pai-web--item-json log item)))
          (setf (pai-web-log-stream log) nil)
          (pai-web--log-replace (current-buffer) log id
                                (plist-put (copy-sequence final) :streaming :false))))
      (setf (pai-web-log-assistant log) nil))))

(defun pai-web--after-render-tool-start (event &rest _)
  "Log the tool call of EVENT."
  (when (pai-web--chat-p)
    (pai-web--send-deltas)
    (let ((call (plist-get event :tool-call-id)))
      (when call (puthash call (plist-get event :args) pai-web--call-args))
      (pai-web--tool-start (current-buffer) (pai-web-log (current-buffer))
                           call (plist-get event :tool-name) (plist-get event :args)))))

(defun pai-web--after-render-tool-end (event &rest _)
  "Log the tool result of EVENT."
  (when (pai-web--chat-p)
    (pai-web--tool-end (current-buffer) (pai-web-log (current-buffer))
                       (plist-get event :tool-call-id) (plist-get event :tool-name)
                       (plist-get event :result) (pai-truthy (plist-get event :is-error)))))

(defconst pai-web--advice
  '((pai--init-buffer :after pai-web--after-init-buffer)
    (pai--render-user :after pai-web--after-render-user)
    (pai--render-note :after pai-web--after-render-note)
    (pai--open-assistant :around pai-web--around-open-assistant)
    (pai--handle-update :after pai-web--after-handle-update)
    (pai--insert-assistant-blocks :after pai-web--after-insert-assistant-blocks)
    (pai--finish-assistant :after pai-web--after-finish-assistant)
    (pai--render-tool-start :after pai-web--after-render-tool-start)
    (pai--render-tool-end :after pai-web--after-render-tool-end))
  "The chat buffer's render functions and how they are advised.")

(defun pai-web--safe (fn)
  "Return a function calling FN with its arguments, never signalling.
The chat buffer's rendering must not break because of the web view."
  (lambda (&rest args)
    (condition-case err
        (apply fn args)
      (error (message "pai-web: %s: %s" fn (error-message-string err)) nil))))

(defun pai-web--advice-function (fn how)
  "Return the advice for FN added HOW, error-proofed."
  (if (eq how :around)
      (lambda (orig &rest args)
        (let ((called nil) (result nil))
          (condition-case err
              (apply fn (lambda (&rest a) (setq called t result (apply orig a))) args)
            (error (message "pai-web: %s: %s" fn (error-message-string err))
                   (unless called (setq result (apply orig args)))))
          result))
    (pai-web--safe fn)))

(defvar pai-web--installed nil "Installed advice: list of (SYMBOL . FUNCTION).")

(defun pai-web-instances-install ()
  "Advise the chat buffer's rendering to keep transcript logs."
  (unless pai-web--installed
    (dolist (a pai-web--advice)
      (let ((fn (pai-web--advice-function (nth 2 a) (nth 1 a))))
        (advice-add (nth 0 a) (nth 1 a) fn '((name . pai-web)))
        (push (cons (nth 0 a) fn) pai-web--installed)))))

(defun pai-web-instances-uninstall ()
  "Remove the rendering advice and forget the logs."
  (dolist (a pai-web--installed)
    (advice-remove (car a) (cdr a)))
  (setq pai-web--installed nil)
  (clrhash pai-web--logs))

;;;; Tool details

(defun pai-web-tool-detail (buffer item-id)
  "Return the full arguments and result of tool item ITEM-ID in BUFFER."
  (let* ((log (pai-web-log buffer))
         (item (gethash item-id (pai-web-log-items log)))
         (call (plist-get item :call))
         (args nil) (result nil))
    (with-current-buffer buffer
      (dolist (m pai--context-messages)
        (pcase (pai-message-role m)
          ('assistant (dolist (tc (pai-message-tool-calls m))
                        (when (equal (plist-get tc :id) call)
                          (setq args (plist-get tc :arguments)))))
          ('tool-result (when (equal (plist-get m :tool-call-id) call)
                          (setq result (pai-content-text (plist-get m :content))))))))
    (list :args (if args (pai-web--args-text args) (or (plist-get item :args) ""))
          :result (let ((r (or result (plist-get item :result) "")))
                    (if (> (length r) 400000) (substring r 0 400000) r)))))

(provide 'pai-web-instances)
;;; pai-web-instances.el ends here
