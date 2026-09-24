;;; pai-learn.el --- An AI learning system for pai -*- lexical-binding: t; -*-

;;; Commentary:

;; Port of https://github.com/amosblomqvist/learn (a pi configuration) to pai.
;;
;; What upstream ships, and what it became here:
;;
;;   skills/teach, skills/visualize   skills/ in this directory, adapted (Org
;;                                    log, our subagent and ask-user tools).
;;   extensions/ask-user-question     not ported: pai's own ask_user_question
;;                                    (pai-ask-user) is used instead.
;;   extensions/quiz                  pai-learn-quiz.el, a vui dialog built
;;                                    on the ask-user dialog pieces.
;;   extensions/md-log (Obsidian)     pai-learn-log.el: the lesson mirrored
;;                                    into an Org file (/teach-log).
;;   extensions/visual-tools          pai-learn-render.el: Mermaid and SVG
;;                                    rendered to PNG inside Emacs by
;;                                    xwidget-webkit (no Node/Chrome/rsvg).
;;   agents/                          agents/ in this directory: roles for
;;                                    the interactive subagents extension.
;;
;; Nothing is in every context.  Loading the extension only adds slash
;; commands (which the model never sees).  `/teach [topic]' turns the
;; current session into a teaching session: it registers the `quiz' tool in
;; this session only, makes the learn-researcher / mermaid-maker / svg-maker
;; roles available to its `subagent' tool, and sends the teach skill.  Maker
;; subagents get `render_diagram' in their own session only.  A session that
;; was taught before (resumed, or after /reload) is recognised from its
;; history and re-armed before its next run.
;;
;; Commands:
;;   /teach [topic]      start (or continue) learning in this session
;;   /teach-log FILE     mirror the lesson into an Org file (backfilled)
;;   /teach-unlog        stop mirroring

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai)
(require 'pai-core)
(require 'pai-ext)
(require 'pai-tools)
(require 'pai-skills)
(require 'pai-commands)
(require 'pai-learn-quiz)
(require 'pai-learn-render)
(require 'pai-learn-log)

(declare-function pai--render-note "pai-ui")
(declare-function pai-isub--parse-role-file "pai-isub-roles" (file))
(defvar pai-isub--roles)
(defvar pai-isub--role)
(defvar pai-isub--parent)
(defvar pai--tools)
(defvar pai--context-messages)

(defconst pai-learn-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this extension, its skills and its agent roles.")

(defcustom pai-learn-maker-roles '("mermaid-maker" "svg-maker")
  "Subagent roles that get the `render_diagram' tool."
  :type '(repeat string) :group 'pai-learn)

(defvar-local pai-learn--active nil
  "Non-nil in a session that is being taught.")

;;;; Skills and roles

(defun pai-learn-skill (name)
  "Return the bundled skill NAME (a skill plist) or signal an error."
  (or (pai-skill-from-file
       (expand-file-name (format "skills/%s/SKILL.md" name) pai-learn-directory))
      (error "pai-learn: skill %s is missing" name)))

(defun pai-learn--role-files ()
  "Return the bundled subagent role files."
  (directory-files (expand-file-name "agents" pai-learn-directory) t "\\.md\\'"))

(defun pai-learn--install-roles ()
  "Make the bundled roles available to this session's `subagent' tool.
Roles the user or project already defines under the same name win."
  (when (and (fboundp 'pai-isub--parse-role-file) (boundp 'pai-isub--roles))
    (dolist (file (pai-learn--role-files))
      (let ((role (pai-isub--parse-role-file file)))
        (when (and role (not (assoc (car role) pai-isub--roles)))
          (setq pai-isub--roles (append pai-isub--roles (list role))))))))

;;;; Activation

(defun pai-learn--instance-p ()
  "Return non-nil in a buffer that owns its own tool registry."
  (local-variable-p 'pai--tools))

(defun pai-learn-activate ()
  "Arm the current session for teaching (quiz tool and subagent roles)."
  (when (pai-learn--instance-p)
    (unless (pai-tool-get "quiz") (pai-register-tool pai-learn-quiz-tool))
    (pai-learn--install-roles)
    (setq pai-learn--active t)))

(defun pai-learn--taught-p ()
  "Return non-nil when this session's history loaded the teach skill."
  (cl-some (lambda (m)
             (and (eq (pai-message-role m) 'user)
                  (string-search "<skill name=\"teach\""
                                 (pai-content-text (pai-message-content m)))))
           pai--context-messages))

(defun pai-learn--maker-p ()
  "Return non-nil in a maker subagent's session."
  (and (bound-and-true-p pai-isub--role)
       (member pai-isub--role pai-learn-maker-roles)))

(defun pai-learn--before-agent-start (_event _ctx)
  "Re-arm this session's learning tools before a run, when it needs them."
  (when (pai-learn--instance-p)
    (cond
     ((pai-learn--maker-p)
      (unless (pai-tool-get "render_diagram")
        (pai-register-tool pai-learn-render-tool)))
     ((or pai-learn--active (pai-learn--taught-p))
      (pai-learn-activate)))))

(defun pai-learn-viz-directory ()
  "Return where published diagrams go.
Next to the lesson log when the teaching session (this one, or the parent
of this maker subagent) has one; otherwise viz/ in the working directory."
  (let* ((parent (and (bound-and-true-p pai-isub--parent)
                      (buffer-live-p pai-isub--parent)
                      pai-isub--parent))
         (log (or pai-learn-log-file
                  (and parent (buffer-local-value 'pai-learn-log-file parent)))))
    (if log (pai-learn-log--viz-directory log)
      (expand-file-name "viz" default-directory))))

(setq pai-learn-viz-directory-function #'pai-learn-viz-directory)

;;;; Commands

(defun pai-learn--teach-command (args _ctx)
  "Handler for `/teach [TOPIC]': arm the session and send the teach skill."
  (pai-learn-activate)
  (let ((topic (string-trim (or args ""))))
    (list :send (pai-skill-command-message
                 (pai-learn-skill "teach")
                 (concat (pai-learn--session-note)
                         (if (string-empty-p topic) ""
                           (concat "\n\n" topic)))))))

(defun pai-learn--session-note ()
  "Return the note telling the tutor about this session's setup."
  (format "(Session: %s; %s.)"
          (if pai-learn-log-file
              (format "the lesson is logged to %s" (abbreviate-file-name pai-learn-log-file))
            "no lesson log is linked")
          (if (fboundp 'pai-isub--parse-role-file)
              "subagents available: learn-researcher, mermaid-maker, svg-maker"
            "subagents are NOT available")))

(defun pai-learn--log-command (args _ctx)
  "Handler for `/teach-log FILE'."
  (let ((file (string-trim (or args ""))))
    (cond
     ((string-empty-p file)
      (list :message (if pai-learn-log-file
                         (format "Logging to %s  (/teach-log FILE to change, /teach-unlog to stop)"
                                 (abbreviate-file-name pai-learn-log-file))
                       "Usage: /teach-log FILE.org — mirror this lesson into an Org file")))
     ((bound-and-true-p pai--active)
      (list :message "Wait for the agent to finish before linking a log."))
     (t
      (let* ((path (expand-file-name
                    (if (file-name-extension file) file (concat file ".org"))))
             (n (pai-learn-log-link path)))
        (list :message (format "Lesson log: %s (%d entries)" (abbreviate-file-name path) n)))))))

(defun pai-learn--unlog-command (_args _ctx)
  "Handler for `/teach-unlog'."
  (let ((file (pai-learn-log-unlink)))
    (list :message (if file (format "Stopped logging to %s" (abbreviate-file-name file))
                     "No lesson log is linked"))))

(defun pai-learn--complete-file (prefix)
  "Complete PREFIX as a file name."
  (let* ((dir (or (file-name-directory prefix) ""))
         (names (ignore-errors (file-name-all-completions
                                (file-name-nondirectory prefix)
                                (expand-file-name dir)))))
    (mapcar (lambda (n) (concat dir n)) names)))

;;;; Registration

(pai-register-extension
 (lambda (api)
   (pai-ext-register-command api "teach"
                             :description "Learn something: start a teaching session (/teach [topic])"
                             :handler #'pai-learn--teach-command)
   (pai-ext-register-command api "teach-log"
                             :description "Mirror the lesson into an Org file: /teach-log FILE"
                             :handler #'pai-learn--log-command
                             :arg-completions #'pai-learn--complete-file)
   (pai-ext-register-command api "teach-unlog"
                             :description "Stop mirroring the lesson"
                             :handler #'pai-learn--unlog-command)
   (pai-ext-on api 'before-agent-start #'pai-learn--before-agent-start)
   (pai-ext-on api 'message-end
               (lambda (event _ctx)
                 (let ((message (plist-get event :message)))
                   (when (memq (pai-message-role message) '(user assistant))
                     (pai-learn-log-append-message message)))))
   (pai-ext-on api 'turn-end
               (lambda (event _ctx)
                 (dolist (result (plist-get event :tool-results))
                   (pai-learn-log-append-message result))))
   (pai-ext-on api 'session-shutdown
               (lambda (_event _ctx)
                 (pai-learn-quiz-cancel-all "The quiz was cancelled: the session ended"))))
 "learn")

(provide 'pai-learn)
;;; pai-learn.el ends here
