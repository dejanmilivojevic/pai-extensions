;;; pai-dap.el --- DAP debugger tool for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; The `debug' tool: DAP (Debug Adapter Protocol) debugger access for the
;; agent, ported from oh-my-pi's `tools/debug.ts'.  One active session at a
;; time.  Actions cover launch/attach, breakpoints (source/function/instruction/
;; data), stepping/continue/pause, evaluate, stack/threads/scopes/variables,
;; disassembly, memory read/write, modules, loaded sources, custom requests,
;; output capture, terminate, and sessions.
;;
;; Gated by the `:debug (:enabled t)' setting.  Adapters are auto-selected by
;; file type (or named explicitly) from the built-in catalog plus any workspace
;; `dap.json'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai)
(require 'pai-ext)
(require 'pai-settings)
(require 'pai-settings-ui)
(require 'pai-dap-config)
(require 'pai-dap-client)
(require 'pai-dap-session)

(defconst pai-dap-ext--readonly-actions
  '("stack_trace" "threads" "scopes" "variables" "modules" "loaded_sources"
    "disassemble" "read_memory" "data_breakpoint_info" "sessions" "output")
  "Debug actions that only read program state.")

(defconst pai-dap-ext--max-timeout 120.0 "Upper bound for a single debug request, seconds.")
(defconst pai-dap-ext--default-timeout 30.0 "Default debug request timeout, seconds.")

;;;; Formatting helpers

(defun pai-dap-ext--format-location (snapshot)
  "Format SNAPSHOT's stop location as PATH:LINE[:COL], or nil."
  (let* ((source (plist-get snapshot :source))
         (path (plist-get source :path))
         (line (plist-get snapshot :line)))
    (when (and path line)
      (format "%s:%s%s" path line
              (if (plist-member snapshot :column)
                  (format ":%s" (plist-get snapshot :column)) "")))))

(defun pai-dap-ext--format-snapshot (snapshot)
  "Return SNAPSHOT formatted as a list of lines (mirrors formatSessionSnapshot)."
  (let ((lines (list (format "Session %s" (plist-get snapshot :id))
                     (format "Adapter: %s" (plist-get snapshot :adapter))
                     (format "Status: %s" (plist-get snapshot :status))
                     (format "CWD: %s" (plist-get snapshot :cwd)))))
    (when (plist-get snapshot :program)
      (setq lines (append lines (list (format "Program: %s" (plist-get snapshot :program))))))
    (when (plist-get snapshot :stopReason)
      (setq lines (append lines (list (format "Stop reason: %s" (plist-get snapshot :stopReason))))))
    (when (plist-get snapshot :frameName)
      (setq lines (append lines (list (format "Frame: %s" (plist-get snapshot :frameName))))))
    (when (plist-get snapshot :instructionPointerReference)
      (setq lines (append lines (list (format "Instruction pointer: %s"
                                              (plist-get snapshot :instructionPointerReference))))))
    (let ((loc (pai-dap-ext--format-location snapshot)))
      (when loc (setq lines (append lines (list (format "Location: %s" loc))))))
    (when (plist-get snapshot :needsConfigurationDone)
      (setq lines (append lines (list "Configuration: pending configurationDone; set breakpoints, then continue."))))
    (when (plist-member snapshot :exitCode)
      (setq lines (append lines (list (format "Exit code: %s" (plist-get snapshot :exitCode))))))
    lines))

(defun pai-dap-ext--snapshot-text (snapshot)
  "Return SNAPSHOT formatted as a single string."
  (mapconcat #'identity (pai-dap-ext--format-snapshot snapshot) "\n"))

(defun pai-dap-ext--bp-suffix (bp)
  "Return the verified/condition/message suffix for breakpoint plist BP."
  (concat (if (eq (plist-get bp :verified) t) "verified" "pending")
          (if (plist-get bp :condition) (format " if %s" (plist-get bp :condition)) "")
          (if (plist-get bp :hitCondition) (format " after %s" (plist-get bp :hitCondition)) "")
          (if (plist-get bp :message) (format " (%s)" (plist-get bp :message)) "")))

(defun pai-dap-ext--format-breakpoints (path breakpoints)
  "Format source BREAKPOINTS for PATH."
  (let ((lines (list (format "Breakpoints for %s:" path))))
    (if (null breakpoints)
        (append lines (list "(none)"))
      (append lines (mapcar (lambda (bp)
                              (format "- line %s: %s" (plist-get bp :line)
                                      (pai-dap-ext--bp-suffix bp)))
                            breakpoints)))))

(defun pai-dap-ext--format-function-breakpoints (breakpoints)
  "Format function BREAKPOINTS."
  (let ((lines (list "Function breakpoints:")))
    (if (null breakpoints)
        (mapconcat #'identity (append lines (list "(none)")) "\n")
      (mapconcat #'identity
                 (append lines (mapcar (lambda (bp)
                                         (format "- %s: %s" (plist-get bp :name)
                                                 (pai-dap-ext--bp-suffix bp)))
                                       breakpoints))
                 "\n"))))

(defun pai-dap-ext--format-instruction-breakpoints (breakpoints)
  "Format instruction BREAKPOINTS."
  (let ((lines (list "Instruction breakpoints:")))
    (if (null breakpoints)
        (mapconcat #'identity (append lines (list "(none)")) "\n")
      (mapconcat #'identity
                 (append lines
                         (mapcar (lambda (bp)
                                   (format "- %s%s: %s"
                                           (plist-get bp :instructionReference)
                                           (if (plist-get bp :offset)
                                               (format "+%s" (plist-get bp :offset)) "")
                                           (pai-dap-ext--bp-suffix bp)))
                                 breakpoints))
                 "\n"))))

(defun pai-dap-ext--format-data-breakpoints (breakpoints)
  "Format data BREAKPOINTS."
  (let ((lines (list "Data breakpoints:")))
    (if (null breakpoints)
        (mapconcat #'identity (append lines (list "(none)")) "\n")
      (mapconcat #'identity
                 (append lines
                         (mapcar (lambda (bp)
                                   (format "- %s: %s%s"
                                           (plist-get bp :dataId)
                                           (if (eq (plist-get bp :verified) t) "verified" "pending")
                                           (concat
                                            (if (plist-get bp :accessType)
                                                (format " (%s)" (plist-get bp :accessType)) "")
                                            (if (plist-get bp :condition)
                                                (format " if %s" (plist-get bp :condition)) "")
                                            (if (plist-get bp :hitCondition)
                                                (format " after %s" (plist-get bp :hitCondition)) "")
                                            (if (plist-get bp :message)
                                                (format " (%s)" (plist-get bp :message)) ""))))
                                 breakpoints))
                 "\n"))))

(defun pai-dap-ext--format-data-breakpoint-info (info)
  "Format dataBreakpointInfo response INFO."
  (let ((lines (list (format "Data breakpoint info: %s" (plist-get info :description))
                     (format "Data ID: %s" (or (plist-get info :dataId) "(not available)")))))
    (let ((types (append (plist-get info :accessTypes) nil)))
      (when types
        (setq lines (append lines (list (format "Access types: %s"
                                                (mapconcat #'identity types ", ")))))))
    (when (plist-member info :canPersist)
      (setq lines (append lines (list (format "Persistent: %s"
                                              (if (eq (plist-get info :canPersist) t) "yes" "no"))))))
    (mapconcat #'identity lines "\n")))

(defun pai-dap-ext--format-stack-frames (frames)
  "Format stack FRAMES vector/list."
  (let ((frames (append frames nil)))
    (if (null frames)
        "Stack trace:\n(empty)"
      (mapconcat
       #'identity
       (cons "Stack trace:"
             (mapcar (lambda (f)
                       (let* ((src (plist-get f :source))
                              (loc (if (plist-get src :path)
                                       (format "%s:%s:%s" (plist-get src :path)
                                               (plist-get f :line) (plist-get f :column))
                                     (format "<unknown>:%s:%s" (plist-get f :line) (plist-get f :column)))))
                         (format "- #%s %s @ %s" (plist-get f :id) (plist-get f :name) loc)))
                     frames))
       "\n"))))

(defun pai-dap-ext--format-threads (threads)
  "Format THREADS list."
  (if (null threads)
      "Threads:\n(none)"
    (mapconcat #'identity
               (cons "Threads:"
                     (mapcar (lambda (th) (format "- %s: %s" (plist-get th :id) (plist-get th :name)))
                             threads))
               "\n")))

(defun pai-dap-ext--format-scopes (scopes)
  "Format SCOPES list."
  (if (null scopes)
      "Scopes:\n(none)"
    (mapconcat #'identity
               (cons "Scopes:"
                     (mapcar (lambda (s)
                               (format "- %s: ref=%s, expensive=%s%s"
                                       (plist-get s :name)
                                       (plist-get s :variablesReference)
                                       (if (eq (plist-get s :expensive) t) "yes" "no")
                                       (if (plist-get s :presentationHint)
                                           (format ", hint=%s" (plist-get s :presentationHint)) "")))
                             scopes))
               "\n")))

(defun pai-dap-ext--format-variables (variables)
  "Format VARIABLES list."
  (if (null variables)
      "Variables:\n(none)"
    (mapconcat #'identity
               (cons "Variables:"
                     (mapcar (lambda (v)
                               (format "- %s = %s%s%s"
                                       (plist-get v :name) (plist-get v :value)
                                       (if (plist-get v :type) (format " (%s)" (plist-get v :type)) "")
                                       (if (and (numberp (plist-get v :variablesReference))
                                                (> (plist-get v :variablesReference) 0))
                                           (format " [ref=%s]" (plist-get v :variablesReference)) "")))
                             variables))
               "\n")))

(defun pai-dap-ext--format-evaluation (evaluation)
  "Format an evaluate response EVALUATION."
  (let ((lines (list (format "Result: %s" (plist-get evaluation :result)))))
    (when (plist-get evaluation :type)
      (setq lines (append lines (list (format "Type: %s" (plist-get evaluation :type))))))
    (when (and (numberp (plist-get evaluation :variablesReference))
               (> (plist-get evaluation :variablesReference) 0))
      (setq lines (append lines (list (format "Variables ref: %s"
                                              (plist-get evaluation :variablesReference))))))
    (mapconcat #'identity lines "\n")))

(defun pai-dap-ext--source-label (source line column)
  "Return a label for SOURCE at LINE/COLUMN, or nil."
  (let ((base (or (plist-get source :path) (plist-get source :name))))
    (when base
      (if line (format "%s:%s%s" base line (if column (format ":%s" column) "")) base))))

(defun pai-dap-ext--format-disassembly (instructions)
  "Format disassembled INSTRUCTIONS."
  (let ((instructions (append instructions nil)))
    (if (null instructions)
        "Disassembly:\n(empty)"
      (let ((addr-w (apply #'max (mapcar (lambda (i) (length (or (plist-get i :address) ""))) instructions)))
            (bytes-w (apply #'max 2 (mapcar (lambda (i) (length (or (plist-get i :instructionBytes) ""))) instructions))))
        (mapconcat
         #'identity
         (cons "Disassembly:"
               (mapcar
                (lambda (i)
                  (let* ((loc (pai-dap-ext--source-label (plist-get i :location)
                                                         (plist-get i :line) (plist-get i :column)))
                         (parts (list (string-pad (or (plist-get i :address) "") addr-w)
                                      (string-pad (or (plist-get i :instructionBytes) "") bytes-w)
                                      (plist-get i :instruction))))
                    (when (plist-get i :symbol)
                      (setq parts (append parts (list (format "<%s>" (plist-get i :symbol))))))
                    (when loc (setq parts (append parts (list (format "[%s]" loc)))))
                    (string-trim-right (mapconcat #'identity (cl-remove-if #'string-empty-p parts) "  "))))
                instructions))
         "\n")))))

(defun pai-dap-ext--format-memory (address data unreadable)
  "Format a memory read of base64 DATA at ADDRESS with UNREADABLE trailing bytes."
  (let* ((raw (if (and data (> (length data) 0))
                  (base64-decode-string data) ""))
         (lines (list (format "Memory at %s:" address))))
    (if (= (length raw) 0)
        (setq lines (append lines (list "(no readable bytes)")))
      (cl-loop for off from 0 below (length raw) by 16 do
               (let* ((chunk (substring raw off (min (+ off 16) (length raw))))
                      (hex (mapconcat (lambda (b) (format "%02x" b)) chunk " "))
                      (ascii (mapconcat (lambda (b) (if (and (>= b 32) (< b 127))
                                                        (char-to-string b) "."))
                                        chunk "")))
                 (setq lines (append lines
                                     (list (format "%s %s |%s|"
                                                   (string-pad (if (= off 0) address (format "+0x%x" off)) 18)
                                                   (string-pad hex 47) ascii)))))))
    (when (and unreadable (> unreadable 0))
      (setq lines (append lines (list (format "Unreadable bytes: %s" unreadable)))))
    (mapconcat #'identity lines "\n")))

(defun pai-dap-ext--format-table (headers rows)
  "Render a text table from HEADERS and ROWS (lists of strings)."
  (let* ((cols (length headers))
         (widths (cl-loop for c from 0 below cols
                          collect (apply #'max (length (nth c headers))
                                         (mapcar (lambda (r) (length (or (nth c r) ""))) rows))))
         (fmt-row (lambda (row)
                    (mapconcat #'identity
                               (cl-loop for c from 0 below cols
                                        collect (string-pad (or (nth c row) "") (nth c widths)))
                               "  "))))
    (mapconcat #'identity
               (append (list (funcall fmt-row headers)
                             (funcall fmt-row (mapcar (lambda (w) (make-string w ?-)) widths)))
                       (mapcar fmt-row rows))
               "\n")))

(defun pai-dap-ext--format-modules (modules)
  "Format MODULES list."
  (let ((modules (append modules nil)))
    (if (null modules)
        "Modules:\n(none)"
      (concat "Modules:\n"
              (pai-dap-ext--format-table
               '("ID" "Name" "Path" "Symbols" "Range")
               (mapcar (lambda (m)
                         (list (format "%s" (plist-get m :id))
                               (or (plist-get m :name) "")
                               (or (plist-get m :path) "")
                               (or (plist-get m :symbolStatus) "")
                               (or (plist-get m :addressRange) "")))
                       modules))))))

(defun pai-dap-ext--format-loaded-sources (sources)
  "Format loaded SOURCES list."
  (let ((sources (append sources nil)))
    (if (null sources)
        "Loaded sources:\n(none)"
      (mapconcat #'identity
                 (cons "Loaded sources:"
                       (mapcar (lambda (s)
                                 (format "- %s%s"
                                         (or (plist-get s :path) (plist-get s :name) "<unknown>")
                                         (if (plist-member s :sourceReference)
                                             (format " [ref=%s]" (plist-get s :sourceReference)) "")))
                               sources))
                 "\n"))))

(defun pai-dap-ext--format-sessions (sessions)
  "Format the SESSIONS summary list."
  (if (null sessions)
      "No debug sessions."
    (mapconcat
     (lambda (s)
       (let ((loc (pai-dap-ext--format-location s)))
         (mapconcat #'identity
                    (append (list (format "%s: %s" (plist-get s :id) (plist-get s :status))
                                  (format "  adapter=%s" (plist-get s :adapter))
                                  (format "  cwd=%s" (plist-get s :cwd)))
                            (when (plist-get s :program) (list (format "  program=%s" (plist-get s :program))))
                            (when loc (list (format "  location=%s" loc)))
                            (when (plist-get s :stopReason) (list (format "  reason=%s" (plist-get s :stopReason)))))
                    "\n")))
     sessions "\n\n")))

(defun pai-dap-ext--format-custom (command body)
  "Format a custom-request BODY for COMMAND."
  (format "%s response:\n%s" command
          (condition-case nil
              (let ((json-encoding-pretty-print t)) (json-encode body))
            (error (format "%S" body)))))

(defun pai-dap-ext--outcome-text (outcome timeout-sec verb)
  "Build the text for a continue/step OUTCOME with VERB and TIMEOUT-SEC."
  (let* ((snapshot (plist-get outcome :snapshot))
         (lines (pai-dap-ext--format-snapshot snapshot)))
    (cond
     ((plist-get outcome :timed-out)
      (mapconcat #'identity
                 (append lines (list (format "Program is still running after %ss. Use pause to interrupt and inspect state."
                                             timeout-sec)))
                 "\n"))
     ((eq (plist-get outcome :state) 'stopped)
      (mapconcat #'identity
                 (append lines (list (format "%s stopped at %s." verb
                                             (or (pai-dap-ext--format-location snapshot) "unknown location"))))
                 "\n"))
     ((eq (plist-get outcome :state) 'terminated)
      (mapconcat #'identity
                 (append lines (list (format "Program terminated%s."
                                             (if (plist-member snapshot :exitCode)
                                                 (format " with exit code %s" (plist-get snapshot :exitCode)) ""))))
                 "\n"))
     (t (mapconcat #'identity (append lines (list "Program is running.")) "\n")))))

;;;; Adapter availability messages

(defconst pai-dap-ext--unavailable-messages
  '(("debugpy" . "adapter 'debugpy' is not available: python not found in PATH")
    ("dlv" . "adapter 'dlv' is not available: install with 'go install github.com/go-delve/delve/cmd/dlv@latest'")
    ("rdbg" . "adapter 'rdbg' is not available: install with 'gem install debug'")
    ("js-debug-adapter" . "adapter 'js-debug-adapter' is not available: download it from https://github.com/microsoft/vscode-js-debug"))
  "Human hints for common unavailable adapters.")

(defun pai-dap-ext--adapter-unavailable (name cwd)
  "Return an error string for unavailable adapter NAME in CWD."
  (or (cdr (assoc name pai-dap-ext--unavailable-messages))
      (format "adapter '%s' is not available. Installed adapters: %s"
              name (pai-dap-config-available-names cwd))))

(defun pai-dap-ext--program-kind (program)
  "Classify PROGRAM path as `directory', `file', or `missing'."
  (cond ((file-directory-p program) 'directory)
        ((file-exists-p program) 'file)
        (t 'missing)))

;;;; Argument helpers

(defun pai-dap-ext--timeout (args)
  "Return a clamped request timeout (seconds) from ARGS."
  (let ((raw (plist-get args :timeout)))
    (max 1.0 (min pai-dap-ext--max-timeout
                  (if (numberp raw) (float raw) pai-dap-ext--default-timeout)))))

(defun pai-dap-ext--require-capability (capability description)
  "Signal unless the active session advertises CAPABILITY (named DESCRIPTION)."
  (unless (pai-dap--active-session)
    (error "No active debug session. Launch or attach first."))
  (unless (eq (plist-get (pai-dap-capabilities) capability) t)
    (error "Current adapter does not support %s" description)))

(defun pai-dap-ext--disassembly-reference (memory-reference)
  "Resolve the disassembly MEMORY-REFERENCE, defaulting to the stop instruction pointer."
  (or memory-reference
      (let ((snap (pai-dap-active-summary)))
        (or (plist-get snap :instructionPointerReference)
            (error "disassemble requires memory_reference unless the current stop location has an instruction pointer reference")))))

;;;; Tool dispatch

(defun pai-dap-ext--execute (args ctx _on-update on-done)
  "Execute the `debug' tool for ARGS in CTX, finishing via ON-DONE."
  (if (not (plist-get (pai-settings-get :debug) :enabled))
      (funcall on-done (pai-tool-error-result
                        "Debugger support is disabled (:debug :enabled). Enable it in settings."))
    (let* ((action (or (plist-get args :action) ""))
           (cwd (if (plist-get args :cwd)
                    (expand-file-name (plist-get args :cwd) (pai-tool-ctx-cwd ctx))
                  (pai-tool-ctx-cwd ctx)))
           (timeout (pai-dap-ext--timeout args)))
      (condition-case err
          (funcall
           on-done
           (pai-tool-ok-result (pai-dap-ext--dispatch action args ctx cwd timeout)))
        (error (funcall on-done (pai-tool-error-result
                                 (format "debug %s failed: %s"
                                         (if (string-empty-p action) "(missing action)" action)
                                         (error-message-string err)))))))))

(defun pai-dap-ext--dispatch (action args ctx cwd timeout)
  "Dispatch a single debug ACTION.  Return the result text."
  (pcase action
    ("launch" (pai-dap-ext--launch args ctx cwd timeout))
    ("attach" (pai-dap-ext--attach args cwd timeout))
    ("set_breakpoint" (pai-dap-ext--set-breakpoint args ctx timeout))
    ("remove_breakpoint" (pai-dap-ext--remove-breakpoint args ctx timeout))
    ("set_instruction_breakpoint"
     (pai-dap-ext--require-capability :supportsInstructionBreakpoints "instruction breakpoints")
     (unless (plist-get args :instruction_reference)
       (error "instruction_reference is required for set_instruction_breakpoint"))
     (pai-dap-ext--format-instruction-breakpoints
      (plist-get (pai-dap-set-instruction-breakpoint
                  (plist-get args :instruction_reference) (plist-get args :offset)
                  (plist-get args :condition) (plist-get args :hit_condition) timeout)
                 :breakpoints)))
    ("remove_instruction_breakpoint"
     (pai-dap-ext--require-capability :supportsInstructionBreakpoints "instruction breakpoints")
     (unless (plist-get args :instruction_reference)
       (error "instruction_reference is required for remove_instruction_breakpoint"))
     (pai-dap-ext--format-instruction-breakpoints
      (plist-get (pai-dap-remove-instruction-breakpoint
                  (plist-get args :instruction_reference) (plist-get args :offset) timeout)
                 :breakpoints)))
    ("data_breakpoint_info"
     (pai-dap-ext--require-capability :supportsDataBreakpoints "data breakpoints")
     (unless (plist-get args :name) (error "name is required for data_breakpoint_info"))
     (pai-dap-ext--format-data-breakpoint-info
      (plist-get (pai-dap-data-breakpoint-info
                  (plist-get args :name)
                  (or (plist-get args :variable_ref) (plist-get args :scope_id))
                  (plist-get args :frame_id) timeout)
                 :info)))
    ("set_data_breakpoint"
     (pai-dap-ext--require-capability :supportsDataBreakpoints "data breakpoints")
     (unless (plist-get args :data_id) (error "data_id is required for set_data_breakpoint"))
     (pai-dap-ext--format-data-breakpoints
      (plist-get (pai-dap-set-data-breakpoint
                  (plist-get args :data_id) (plist-get args :access_type)
                  (plist-get args :condition) (plist-get args :hit_condition) timeout)
                 :breakpoints)))
    ("remove_data_breakpoint"
     (pai-dap-ext--require-capability :supportsDataBreakpoints "data breakpoints")
     (unless (plist-get args :data_id) (error "data_id is required for remove_data_breakpoint"))
     (pai-dap-ext--format-data-breakpoints
      (plist-get (pai-dap-remove-data-breakpoint (plist-get args :data_id) timeout) :breakpoints)))
    ("continue" (pai-dap-ext--outcome-text (pai-dap-continue timeout) timeout "Continue"))
    ("step_over" (pai-dap-ext--outcome-text (pai-dap-step-over timeout) timeout "Step over"))
    ("step_in" (pai-dap-ext--outcome-text (pai-dap-step-in timeout) timeout "Step in"))
    ("step_out" (pai-dap-ext--outcome-text (pai-dap-step-out timeout) timeout "Step out"))
    ("pause"
     (let ((snapshot (pai-dap-pause timeout)))
       (mapconcat #'identity (append (pai-dap-ext--format-snapshot snapshot)
                                     (list "Program paused.")) "\n")))
    ("evaluate"
     (unless (plist-get args :expression) (error "expression is required for evaluate"))
     (pai-dap-ext--format-evaluation
      (plist-get (pai-dap-evaluate (plist-get args :expression)
                                   (plist-get args :context) (plist-get args :frame_id) timeout)
                 :evaluation)))
    ("stack_trace"
     (pai-dap-ext--format-stack-frames
      (plist-get (pai-dap-stack-trace (plist-get args :levels) timeout) :stack-frames)))
    ("threads"
     (pai-dap-ext--format-threads (plist-get (pai-dap-threads timeout) :threads)))
    ("scopes"
     (pai-dap-ext--format-scopes (plist-get (pai-dap-scopes (plist-get args :frame_id) timeout) :scopes)))
    ("variables"
     (let ((ref (or (plist-get args :variable_ref) (plist-get args :scope_id))))
       (unless ref (error "variables requires variable_ref or scope_id"))
       (pai-dap-ext--format-variables (plist-get (pai-dap-variables ref timeout) :variables))))
    ("disassemble"
     (pai-dap-ext--require-capability :supportsDisassembleRequest "disassembly")
     (unless (plist-get args :instruction_count) (error "instruction_count is required for disassemble"))
     (pai-dap-ext--format-disassembly
      (plist-get (pai-dap-disassemble
                  (pai-dap-ext--disassembly-reference (plist-get args :memory_reference))
                  (plist-get args :instruction_count) (plist-get args :offset)
                  (plist-get args :instruction_offset) (plist-get args :resolve_symbols) timeout)
                 :instructions)))
    ("read_memory"
     (pai-dap-ext--require-capability :supportsReadMemoryRequest "memory reads")
     (unless (plist-get args :memory_reference) (error "memory_reference is required for read_memory"))
     (unless (plist-get args :count) (error "count is required for read_memory"))
     (let ((r (pai-dap-read-memory (plist-get args :memory_reference)
                                   (plist-get args :count) (plist-get args :offset) timeout)))
       (pai-dap-ext--format-memory (plist-get r :address) (plist-get r :data)
                                   (plist-get r :unreadable-bytes))))
    ("write_memory"
     (pai-dap-ext--require-capability :supportsWriteMemoryRequest "memory writes")
     (unless (plist-get args :memory_reference) (error "memory_reference is required for write_memory"))
     (unless (plist-get args :data) (error "data is required for write_memory"))
     (let ((r (pai-dap-write-memory (plist-get args :memory_reference) (plist-get args :data)
                                    (plist-get args :offset) (plist-get args :allow_partial) timeout)))
       (mapconcat #'identity
                  (append (list "Memory write completed.")
                          (when (plist-get r :bytes-written)
                            (list (format "Bytes written: %s" (plist-get r :bytes-written))))
                          (when (plist-get r :offset)
                            (list (format "Offset: %s" (plist-get r :offset)))))
                  "\n")))
    ("modules"
     (pai-dap-ext--require-capability :supportsModulesRequest "module introspection")
     (pai-dap-ext--format-modules
      (plist-get (pai-dap-modules (plist-get args :start_module) (plist-get args :module_count) timeout)
                 :modules)))
    ("loaded_sources"
     (pai-dap-ext--require-capability :supportsLoadedSourcesRequest "loaded sources")
     (pai-dap-ext--format-loaded-sources (plist-get (pai-dap-loaded-sources timeout) :sources)))
    ("custom_request"
     (unless (plist-get args :command) (error "command is required for custom_request"))
     (let ((r (pai-dap-custom-request (plist-get args :command) (plist-get args :arguments) timeout)))
       (pai-dap-ext--format-custom (plist-get args :command) (plist-get r :body))))
    ("output"
     (let ((r (pai-dap-get-output)))
       (if (> (length (plist-get r :output)) 0) (plist-get r :output) "(no output captured)")))
    ("terminate"
     (let ((snapshot (pai-dap-terminate timeout)))
       (if (null snapshot) "No debug session to terminate."
         (mapconcat #'identity (append (pai-dap-ext--format-snapshot snapshot)
                                       (list "Debug session terminated.")) "\n"))))
    ("sessions" (pai-dap-ext--format-sessions (pai-dap-list-sessions)))
    (_ (error "Unsupported debug action: %s" (if (string-empty-p action) "(missing)" action)))))

(defun pai-dap-ext--launch (args ctx cwd timeout)
  "Handle the launch ACTION."
  (unless (plist-get args :program) (error "program is required for launch"))
  (let* ((program (expand-file-name (plist-get args :program) cwd))
         (kind (pai-dap-ext--program-kind program))
         (selection (pai-dap-config-select-launch program cwd (plist-get args :adapter) kind)))
    (ignore ctx)
    (pcase (car selection)
      ('unavailable (error "%s" (pai-dap-ext--adapter-unavailable (cdr selection) cwd)))
      ('none (error "No debugger adapter available. Installed adapters: %s"
                    (pai-dap-config-available-names cwd)))
      ('adapter
       (let ((adapter (cdr selection)))
         (when (and (eq kind 'directory) (not (plist-get adapter :accepts-directory-program)))
           (error "launch program resolves to a directory: %s. Pass an executable file path or choose an adapter that supports package directories."
                  (file-name-as-directory program)))
         (let ((snapshot (pai-dap-launch
                          (list :adapter adapter :program program
                                :args (and (plist-get args :args) (append (plist-get args :args) nil))
                                :cwd cwd
                                :extra (pai-dap-config-launch-overrides adapter program kind))
                          timeout)))
           (pai-dap-ext--snapshot-text snapshot)))))))

(defun pai-dap-ext--attach (args cwd timeout)
  "Handle the attach ACTION."
  (when (and (null (plist-get args :pid)) (null (plist-get args :port))
             (null (plist-get args :adapter)))
    (error "attach requires pid or port"))
  (let ((adapter (pai-dap-config-select-attach cwd (plist-get args :adapter) (plist-get args :port))))
    (unless adapter
      (if (plist-get args :adapter)
          (error "%s" (pai-dap-ext--adapter-unavailable (plist-get args :adapter) cwd))
        (error "No debugger adapter available. Installed adapters: %s"
               (pai-dap-config-available-names cwd))))
    (pai-dap-ext--snapshot-text
     (pai-dap-attach (list :adapter adapter :cwd cwd
                           :pid (plist-get args :pid) :port (plist-get args :port)
                           :host (plist-get args :host))
                     timeout))))

(defun pai-dap-ext--set-breakpoint (args ctx timeout)
  "Handle the set_breakpoint ACTION."
  (if (plist-get args :function)
      (pai-dap-ext--format-function-breakpoints
       (plist-get (pai-dap-set-function-breakpoint
                   (plist-get args :function) (plist-get args :condition) timeout)
                  :breakpoints))
    (unless (and (plist-get args :file) (plist-get args :line))
      (error "set_breakpoint requires file+line or function"))
    (let* ((file (pai-tool-resolve-path ctx (plist-get args :file)))
           (r (pai-dap-set-breakpoint file (plist-get args :line) (plist-get args :condition) timeout)))
      (mapconcat #'identity
                 (pai-dap-ext--format-breakpoints (plist-get r :source-path) (plist-get r :breakpoints))
                 "\n"))))

(defun pai-dap-ext--remove-breakpoint (args ctx timeout)
  "Handle the remove_breakpoint ACTION."
  (if (plist-get args :function)
      (pai-dap-ext--format-function-breakpoints
       (plist-get (pai-dap-remove-function-breakpoint (plist-get args :function) timeout) :breakpoints))
    (unless (and (plist-get args :file) (plist-get args :line))
      (error "remove_breakpoint requires file+line or function"))
    (let* ((file (pai-tool-resolve-path ctx (plist-get args :file)))
           (r (pai-dap-remove-breakpoint file (plist-get args :line) timeout)))
      (mapconcat #'identity
                 (pai-dap-ext--format-breakpoints (plist-get r :source-path) (plist-get r :breakpoints))
                 "\n"))))

;;;; Tool registration

(pai-register-tool
 (list :name "debug"
       :label "Debug"
       :description "Debugger access via DAP (Debug Adapter Protocol). Prefer over bash for program state, breakpoints, stepping, or thread inspection. Only one active session at a time. `program' is a target path, not a shell command. Directories need a directory-capable adapter (e.g. dlv). Actions: launch, attach, set_breakpoint, remove_breakpoint, set_instruction_breakpoint, remove_instruction_breakpoint, data_breakpoint_info, set_data_breakpoint, remove_data_breakpoint, continue, step_over, step_in, step_out, pause, evaluate, stack_trace, threads, scopes, variables, disassemble, read_memory, write_memory, modules, loaded_sources, custom_request, output, terminate, sessions."
       :prompt-snippet "debug: DAP debugger (breakpoints, stepping, state)"
       :execution-mode 'sequential
       :parameters
       (pai-object-schema
        (list :action (pai-string-schema
                       "Debug action to perform."
                       :enum '("launch" "attach" "set_breakpoint" "remove_breakpoint"
                               "set_instruction_breakpoint" "remove_instruction_breakpoint"
                               "data_breakpoint_info" "set_data_breakpoint" "remove_data_breakpoint"
                               "continue" "step_over" "step_in" "step_out" "pause" "evaluate"
                               "stack_trace" "threads" "scopes" "variables" "disassemble"
                               "read_memory" "write_memory" "modules" "loaded_sources"
                               "custom_request" "output" "terminate" "sessions"))
              :program (pai-string-schema "Debug target path; directory-capable adapters accept package dirs.")
              :args (pai-array-schema "Program arguments." (pai-string-schema "argument"))
              :adapter (pai-string-schema "Configured adapter id (gdb, lldb-dap, debugpy, dlv, rdbg, js-debug-adapter, or a dap.json entry).")
              :cwd (pai-string-schema "Working directory for the debug target.")
              :file (pai-string-schema "Source file for a breakpoint.")
              :line (pai-number-schema "Source line for a breakpoint.")
              :function (pai-string-schema "Function name for a function breakpoint.")
              :name (pai-string-schema "Variable or data name (data_breakpoint_info).")
              :condition (pai-string-schema "Breakpoint condition.")
              :hit_condition (pai-string-schema "Breakpoint hit condition.")
              :expression (pai-string-schema "Expression to evaluate.")
              :context (pai-string-schema "Evaluate context: watch | repl | hover | variables | clipboard.")
              :frame_id (pai-number-schema "Stack frame id.")
              :scope_id (pai-number-schema "Scope variables reference.")
              :variable_ref (pai-number-schema "Variable reference to expand.")
              :pid (pai-number-schema "Process id for attach.")
              :port (pai-number-schema "Remote attach port.")
              :host (pai-string-schema "Remote attach host.")
              :levels (pai-number-schema "Max stack frames.")
              :memory_reference (pai-string-schema "Memory reference or address.")
              :instruction_reference (pai-string-schema "Instruction reference for an instruction breakpoint.")
              :instruction_count (pai-number-schema "Instruction count for disassemble.")
              :instruction_offset (pai-number-schema "Instruction offset for disassemble.")
              :count (pai-number-schema "Bytes to read.")
              :data (pai-string-schema "Base64 memory payload for write_memory.")
              :data_id (pai-string-schema "Data breakpoint id.")
              :access_type (pai-string-schema "Data breakpoint access type: read | write | readWrite.")
              :command (pai-string-schema "Custom DAP request command.")
              :arguments (pai-object-schema nil)
              :offset (pai-number-schema "Byte/instruction offset.")
              :resolve_symbols (pai-boolean-schema "Resolve symbols in disassembly.")
              :allow_partial (pai-boolean-schema "Allow partial memory writes.")
              :start_module (pai-number-schema "First module index.")
              :module_count (pai-number-schema "Number of modules to return.")
              :timeout (pai-number-schema "Per-request timeout in seconds."))
        '("action"))
       :execute #'pai-dap-ext--execute))

;;;; Extension registration

(pai-register-extension
 (lambda (pi)
   (pai-ext-on pi 'session-end
               (lambda (_event _ctx) (pai-dap-shutdown-all)))))

;;;; Settings UI

(pai-settings-ui-register-section 'debug "Debugger" 55)
(pai-settings-ui-register-subsection 'debug 'enabled "Enabled" 10)
(pai-settings-ui-register-item
 'debug 'enabled
 :key :debug-enabled :type 'boolean :label "Enable debugger"
 :doc "Expose the DAP `debug' tool to the agent"
 :get (lambda () (plist-get (pai-settings-get :debug) :enabled))
 :set (lambda (v)
        (let ((debug (pai-settings-get :debug)))
          (pai-settings-set :debug (plist-put (copy-sequence debug) :enabled v) 'project))))
(pai-settings-ui-register-subsection 'debug 'adapters "Adapters" 20)
(pai-settings-ui-register-item
 'debug 'adapters
 :key :debug-adapters :type 'custom :label "Available adapters"
 :doc "Debug adapters resolvable on PATH"
 :render (lambda (_refresh)
           (vui-text (format "Available: %s"
                             (pai-dap-config-available-names default-directory)))))

(provide 'pai-dap)
;;; pai-dap.el ends here
