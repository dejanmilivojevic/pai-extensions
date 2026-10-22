;;; pai-harness2.el --- Harness² recursive harnessing with pai as the agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Integrates Harness² (google-research/harness2, "Recursive Agent
;; Harnessing for an Open World") with pai.  Harness² improves an agent's
;; *harness* -- system prompt, guardrails, memory, tool guidance, subagents,
;; skills, scripts and hooks -- per task, by proposing edits, probing them
;; with real rollouts and composing the winners, then keeps the lessons in an
;; experience library for the next task.  Upstream it drives OpenCode or
;; Codex; this extension makes pai a third substrate (and the improver too):
;;
;;   pai-harness2-batch.el   headless pai rollouts (child Emacs processes)
;;                           recorded as JSONL trajectories
;;   pai-harness2-hooks.el   .pai/hooks.json, Claude-Code style lifecycle
;;                           hooks dispatched inside pai's tool loop
;;   python/harness2_pai/    the `PaiSubstrate', the `pai' improver channel,
;;                           the JobBench launcher and the `harness2-pai' CLI
;;
;; In a pai chat, `/harness2' drives it all asynchronously (compilation
;; buffers; nothing blocks Emacs):
;;
;;   /harness2 install           clone Harness² and install both packages (uv)
;;   /harness2 setup jb|lab|wb   install a benchmark for pai (wb: --subset S)
;;   /harness2 check --bench B   preflight the pai cell, no model calls
;;   /harness2 demo [--k N]      the local ledger demo with real pai rollouts
;;   /harness2 run ARGS          harness2 --substrate pai ARGS (e.g. --bench lab --k 1;
;;                               WorkBuddy: --bench wb --subset office)
;;   /harness2 judge ARGS        harness2-judge for the pai cell
;;   /harness2 report ARGS       harness2-report for the pai cell
;;   /harness2 runs              open the runs directory
;;   /harness2 render FILE       show a rollout trajectory as Harness² renders it
;;   /harness2 status            the effective configuration
;;
;; Rollouts use this chat's model unless `pai-harness2-model' says otherwise.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'compile)
(require 'pai)
(require 'pai-ext)
(require 'pai-commands)
(require 'pai-settings)
(require 'pai-harness2-batch)

(defgroup pai-harness2 nil
  "Harness² recursive harnessing with pai as the executor."
  :group 'pai
  :prefix "pai-harness2-")

(defcustom pai-harness2-directory (expand-file-name "~/prj/harness2")
  "Harness² checkout (with its .venv) the commands run in."
  :type 'directory)

(defcustom pai-harness2-repository "https://github.com/google-research/harness2"
  "Where `/harness2 install' clones Harness² from."
  :type 'string)

(defcustom pai-harness2-model nil
  "pai model id for Harness² rollouts; nil means the chat's current model."
  :type '(choice (const :tag "Current model" nil) string))

(defcustom pai-harness2-improver-model nil
  "pai model id for Harness² improver sessions; nil means the rollout model."
  :type '(choice (const :tag "Rollout model" nil) string))

(defcustom pai-harness2-improver-channel "pai"
  "Channel the Harness² improver runs on: pai, opencode or claude_cli."
  :type '(choice (const "pai") (const "opencode") (const "claude_cli")))

(defcustom pai-harness2-thinking nil
  "Thinking level for rollouts (off, minimal, low, medium, high, xhigh), or nil."
  :type '(choice (const :tag "Model default" nil) string))

(defcustom pai-harness2-environment nil
  "Extra environment entries (\"NAME=value\") for Harness² commands.
Benchmarks need their judge configured here or in the checkout's .env,
e.g. HARNESS2_JUDGE_MODEL and GOOGLE_CLOUD_PROJECT."
  :type '(repeat string))

(defconst pai-harness2--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "This extension's directory.")

(defconst pai-harness2-subcommands
  '("install" "setup" "check" "demo" "run" "judge" "report" "runs" "render" "status" "help")
  "Subcommands of `/harness2'.")

(defconst pai-harness2--run-flags
  '("--bench" "jb" "lab" "wb" "--subset" "office" "web" "code"
    "--k" "--mode" "parallel" "sequential" "--seed" "--split" "all"
    "train" "test" "--max-tasks" "--only-domains" "--workers" "--tag" "--dry-run")
  "Flags offered after `/harness2 run', `judge' and `report'.")

;;;; Paths and environment

(defun pai-harness2--root ()
  "The pai checkout this Emacs loaded pai from."
  (let ((lib (locate-library "pai")))
    (and lib (file-name-directory (directory-file-name (file-name-directory lib))))))

(defun pai-harness2--venv-bin (name)
  "Path of NAME in the Harness² checkout's virtualenv."
  (expand-file-name (concat ".venv/bin/" name) pai-harness2-directory))

(defun pai-harness2--python-dir ()
  "This extension's Python package directory."
  (expand-file-name "python" pai-harness2--dir))

(defun pai-harness2-rollout-command ()
  "The child-Emacs command line Harness² runs a rollout with."
  (pai-harness2-batch-command))

(defun pai-harness2--model-id ()
  "The pai model id rollouts use."
  (or pai-harness2-model
      (and (bound-and-true-p pai--model)
           (plist-get pai--model :id)
           (pai-model-key pai--model))
      (ignore-errors (pai-settings-get :model))))

(defun pai-harness2-environment ()
  "Environment entries Harness² commands run with."
  (let ((model (pai-harness2--model-id)))
    (delq nil
          (append
           (list (and model (concat "HARNESS2_PAI_MODEL=" model))
                 (let ((imp (or pai-harness2-improver-model model)))
                   (and imp (concat "HARNESS2_PAI_IMPROVER_MODEL=" imp)))
                 (concat "HARNESS2_IMPROVER_CHANNEL=" pai-harness2-improver-channel)
                 (and pai-harness2-thinking
                      (concat "HARNESS2_PAI_THINKING=" pai-harness2-thinking))
                 (concat "PAI_HARNESS2_EMACS_ARGV="
                         (json-serialize (vconcat (pai-harness2-rollout-command))))
                 (concat "PAI_EMACS=" (expand-file-name invocation-name invocation-directory))
                 (and (pai-harness2--root) (concat "PAI_ROOT=" (pai-harness2--root))))
           pai-harness2-environment))))

;;;; Running commands (asynchronously)

(defun pai-harness2--start (label command &optional directory)
  "Run shell COMMAND asynchronously in a compilation buffer named after LABEL.
DIRECTORY defaults to the Harness² checkout.  Return the buffer."
  (let* ((default-directory (file-name-as-directory
                             (expand-file-name (or directory pai-harness2-directory))))
         (process-environment (append (pai-harness2-environment) process-environment))
         (compilation-buffer-name-function (lambda (_mode) (format "*harness2: %s*" label))))
    (save-window-excursion
      (let ((buf (compilation-start command 'compilation-mode)))
        (display-buffer buf '((display-buffer-reuse-window display-buffer-pop-up-window)
                              (inhibit-same-window . t)))
        buf))))

(defun pai-harness2--installed-p ()
  "Non-nil when the checkout's venv has `harness2-pai'."
  (file-executable-p (pai-harness2--venv-bin "harness2-pai")))

(defun pai-harness2-install-command ()
  "Shell command installing Harness² and this extension's package."
  (let ((dir (shell-quote-argument (expand-file-name pai-harness2-directory)))
        (py (shell-quote-argument (pai-harness2--python-dir))))
    (string-join
     (list (format "(test -d %s/.git || git clone %s %s)"
                   dir (shell-quote-argument pai-harness2-repository) dir)
           (format "cd %s" dir)
           "(test -x .venv/bin/python || uv venv --python 3.12 .venv)"
           (format "uv pip install --python .venv/bin/python -e '.[render]' -e %s pytest" py)
           ".venv/bin/harness2-pai status")
     " && ")))

(defun pai-harness2--cli (sub args)
  "Return the `harness2-pai SUB ARGS' shell command."
  (concat (shell-quote-argument (pai-harness2--venv-bin "harness2-pai")) " " sub
          (if (string-blank-p (or args "")) "" (concat " " args))))

(defun pai-harness2-status-text ()
  "A summary of the effective configuration."
  (string-join
   (list (format "Harness² checkout: %s%s" (abbreviate-file-name pai-harness2-directory)
                 (if (pai-harness2--installed-p) "" "  (not installed: /harness2 install)"))
         (format "rollout model:     %s" (or (pai-harness2--model-id) "(none — set one with /model)"))
         (format "improver:          %s/%s" pai-harness2-improver-channel
                 (or pai-harness2-improver-model (pai-harness2--model-id) "?"))
         (format "thinking:          %s" (or pai-harness2-thinking "model default"))
         (format "rollouts:          %s" (abbreviate-file-name (car (pai-harness2-rollout-command))))
         (format "pai:               %s" (or (and (pai-harness2--root)
                                                  (abbreviate-file-name (pai-harness2--root)))
                                             "?")))
   "\n"))

(defun pai-harness2--runs-directory ()
  "The Harness² runs directory."
  (expand-file-name (or (getenv "HARNESS2_RUNS_DIR")
                        (seq-some (lambda (e) (and (string-prefix-p "HARNESS2_RUNS_DIR=" e)
                                                   (substring e 18)))
                                  pai-harness2-environment)
                        "runs")
                    pai-harness2-directory))

(defun pai-harness2-dispatch (args)
  "Run `/harness2 ARGS'.  Return a result plist for the command system, or nil."
  (let* ((args (string-trim (or args "")))
         (sub (car (split-string args "[ \t]+" t)))
         (rest (string-trim (substring args (length (or sub ""))))))
    (cond
     ((or (null sub) (member sub '("help" "status")))
      (list :message (concat (pai-harness2-status-text)
                             (if (equal sub "status") ""
                               (concat "\n\nSubcommands: " (string-join pai-harness2-subcommands " "))))))
     ((not (member sub pai-harness2-subcommands))
      (list :message (format "Unknown /harness2 subcommand %S; try: %s"
                             sub (string-join pai-harness2-subcommands " "))))
     ((equal sub "install")
      (make-directory (file-name-directory (directory-file-name
                                            (expand-file-name pai-harness2-directory)))
                      t)
      (pai-harness2--start "install" (pai-harness2-install-command) "~")
      (list :message (format "Installing Harness² into %s …" (abbreviate-file-name pai-harness2-directory))))
     ((not (pai-harness2--installed-p))
      (list :message (format "Harness² is not installed in %s; run /harness2 install first"
                             (abbreviate-file-name pai-harness2-directory))))
     ((equal sub "runs")
      (let ((dir (pai-harness2--runs-directory)))
        (if (file-directory-p dir) (progn (dired-other-window dir) nil)
          (list :message (format "No runs yet in %s" (abbreviate-file-name dir))))))
     ((equal sub "setup")
      (let ((args (if (string-empty-p rest) "jb" rest)))
        (pai-harness2--start (concat "setup " args) (pai-harness2--cli "setup" args)))
      nil)
     ((equal sub "render")
      (if (string-empty-p rest)
          (list :message "Usage: /harness2 render TRAJECTORY.jsonl")
        (pai-harness2--start (concat "render " (file-name-nondirectory rest))
                             (pai-harness2--cli "render" (shell-quote-argument
                                                          (expand-file-name rest))))
        nil))
     ((and (member sub '("run" "demo")) (null (pai-harness2--model-id)))
      (list :message "No model for rollouts: pick one with /model or set pai-harness2-model"))
     (t
      (pai-harness2--start (string-trim (concat sub " " rest)) (pai-harness2--cli sub rest))
      nil))))

(defun pai-harness2-command (args ctx)
  "Handler for `/harness2 ARGS' from the pai buffer in CTX."
  (let ((buf (plist-get ctx :buffer)))
    (if (buffer-live-p buf)
        (with-current-buffer buf (pai-harness2-dispatch args))
      (pai-harness2-dispatch args))))

(defconst pai-harness2-completion-tree
  `(("install") ("status") ("help")
    ("check" (:rest "--bench" "jb" "lab" "wb" "--subset" "office" "web" "code"))
    ("setup" ("jb" (:rest "--dry-run")) ("lab" (:rest "--dry-run"))
             ("wb" (:rest "--subset" "office" "web" "code" "--dry-run")))
    ("demo" (:rest "--k" "--mode" "parallel" "sequential" "both" "--improver" "demo" "pai"
                   "--output"))
    ("run" (:rest ,@pai-harness2--run-flags))
    ("judge" (:rest ,@pai-harness2--run-flags))
    ("report" (:rest ,@pai-harness2--run-flags))
    ("runs")
    ("render"))
  "Argument completion for `/harness2'.")

;;;; Extension entry point

(defun pai-harness2-extension (api)
  "Register `/harness2' with API."
  (pai-ext-register-command
   api "harness2"
   :description "Harness² recursive harness improvement with pai rollouts (install, demo, run…)"
   :arg-completions (pai-command-completion-tree pai-harness2-completion-tree)
   :handler #'pai-harness2-command))

(pai-register-extension #'pai-harness2-extension "harness2")

(provide 'pai-harness2)
;;; pai-harness2.el ends here
