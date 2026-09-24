;;; pai-memory-store-test.el --- Tests for pai-memory Phase 3 (long-term) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-stest--with-home (dir &rest body)
  "Run BODY with a temp pai home DIR and a project under it as `default-directory'."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-ltm" t)))
          (pai-directory ,dir)
          (default-directory (file-name-as-directory (expand-file-name "proj" ,dir)))
          (pai-settings--global nil)
          (pai-settings--project nil)
          (pai-memory--providers nil))
     (make-directory default-directory t)
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defun pai-memory-stest--apply (&rest change)
  "Apply CHANGE in the current project; return the result."
  (pai-memory-apply-change change (list :cwd default-directory :session nil)))

;;;; Entries

(ert-deftest pai-memory-entries-roundtrip ()
  (should (equal (pai-memory-parse-entries "one\n§\ntwo\nlines\n  §  \nthree\n") '("one" "two\nlines" "three")))
  (should (equal (pai-memory-parse-entries "") nil))
  (should (equal (pai-memory-render-entries '("a" "b")) "a\n§\nb\n"))
  (should (equal (pai-memory-parse-entries (pai-memory-render-entries '("a" "b\nc"))) '("a" "b\nc"))))

(ert-deftest pai-memory-change-entries-rules ()
  (let ((e '("prefers terse answers" "uses Emacs 29")))
    (should (equal (pai-memory-change-entries e '(:action add :content "  likes ERT "))
                   '("prefers terse answers" "uses Emacs 29" "likes ERT")))
    (should (equal (pai-memory-change-entries e '(:action "replace" :old "Emacs 29" :content "uses Emacs 30"))
                   '("prefers terse answers" "uses Emacs 30")))
    (should (equal (pai-memory-change-entries e '(:action remove :old "terse")) '("uses Emacs 29")))
    (should-error (pai-memory-change-entries e '(:action add :content "")) :type 'user-error)
    (should-error (pai-memory-change-entries e '(:action add :content "uses Emacs 29")) :type 'user-error)
    (should-error (pai-memory-change-entries e '(:action remove :old "nope")) :type 'user-error)
    (should-error (pai-memory-change-entries e '(:action remove :old "s")) :type 'user-error)
    (should-error (pai-memory-change-entries e '(:action frob)) :type 'user-error)
    ;; content cannot smuggle a separator; secrets are redacted
    (let ((out (car (last (pai-memory-change-entries
                           nil '(:action add :content "a\n§\nb sk-abcdefghijklmnop1234"))))))
      (should (equal (length (pai-memory-parse-entries (pai-memory-render-entries (list out)))) 1))
      (should (string-match-p "\\[redacted:api-key\\]" out)))))

;;;; Store, limits, log, undo

(ert-deftest pai-memory-apply-writes-and-logs ()
  (pai-memory-stest--with-home dir
    (let ((r (pai-memory-stest--apply :action 'add :target 'user :content "Prefers terse answers")))
      (should (plist-get r :ok))
      (should (equal (pai-memory-read 'user default-directory) '("Prefers terse answers")))
      (should (file-exists-p (expand-file-name "memory/USER.md" dir)))
      (pai-memory-stest--apply :action 'add :target 'project :content "Tests: make test")
      (should (file-exists-p (pai-memory-target-file 'project default-directory)))
      (should (string-match-p "/memory/projects/.*proj/MEMORY.md\\'"
                              (pai-memory-target-file 'project default-directory)))
      ;; FC1: the log records before/after, target and a content hash
      (let ((rec (car (pai-memory-log-read))))
        (should (equal (plist-get rec :id) (plist-get r :id)))
        (should (equal (plist-get rec :target) "user"))
        (should (equal (plist-get rec :before) ""))
        (should (equal (plist-get rec :after) "Prefers terse answers\n"))
        (should (equal (plist-get rec :entry_text_hash)
                       (secure-hash 'sha256 "Prefers terse answers\n")))))))

(ert-deftest pai-memory-apply-enforces-limits ()
  (pai-memory-stest--with-home dir
    (setq pai-settings--global '(:memory (:long-term (:user-char-limit 30))))
    (should (plist-get (pai-memory-stest--apply :action 'add :target 'user :content "short entry") :ok))
    (let ((r (pai-memory-stest--apply :action 'add :target 'user :content (make-string 40 ?x))))
      (should (string-match-p "over its 30 limit; replace or merge" (plist-get r :error))))
    (should (equal (pai-memory-read 'user default-directory) '("short entry")))
    (should (= (length (pai-memory-log-read)) 1))))

(ert-deftest pai-memory-limit-uses-project-settings-outside-pai-buffer ()
  ;; a worker/timer/review buffer has no project settings loaded; the limit
  ;; must still come from the project's .pai/settings.json, not the default
  (pai-memory-stest--with-home dir
    (let ((cwd default-directory))
      (pai-settings--write (pai-settings-project-file cwd)
                           '(:memory (:long-term (:project-char-limit 20 :user-char-limit 25))))
      (with-temp-buffer
        (let ((default-directory temporary-file-directory)
              (pai-settings--project-dir nil))
          (should (= (pai-memory-target-limit 'project nil cwd) 20))
          (should (= (pai-memory-target-limit 'user nil cwd) 25))
          (should (= (pai-memory-target-limit 'project) 3000))
          (let ((r (pai-memory-apply-change (list :action 'add :target 'project
                                                  :content (make-string 30 ?x))
                                            (list :cwd cwd :session nil))))
            (should (string-match-p "over its 20 limit" (plist-get r :error))))
          ;; edits on disk are picked up
          (sleep-for 0.01)
          (pai-settings--write (pai-settings-project-file cwd)
                               '(:memory (:long-term (:project-char-limit 8000))))
          (set-file-times (pai-settings-project-file cwd) (time-add nil 5))
          (should (= (pai-memory-target-limit 'project nil cwd) 8000)))))))

(ert-deftest pai-memory-undo-restores-and-refuses-stale ()
  (pai-memory-stest--with-home dir
    (let ((r1 (pai-memory-stest--apply :action 'add :target 'memory :content "rg is installed"))
          (r2 (pai-memory-stest--apply :action 'add :target 'memory :content "fd is installed")))
      ;; undoing the older change while a newer one sits on top is refused
      (should (string-match-p "changed again" (pai-memory-undo (plist-get r1 :id))))
      ;; newest first
      (should (string-match-p "Undid" (pai-memory-undo)))
      (should (equal (pai-memory-read 'memory default-directory) '("rg is installed")))
      (should (string-match-p "already undone" (pai-memory-undo (plist-get r2 :id))))
      (should (string-match-p "Undid" (pai-memory-undo)))
      (should-not (pai-memory-read 'memory default-directory))
      (should (equal (pai-memory-undo) "Nothing to undo")))))

;;;; Snapshot and providers

(ert-deftest pai-memory-snapshot-contents ()
  (pai-memory-stest--with-home dir
    (pai-memory-stest--apply :action 'add :target 'user :content "Prefers terse answers")
    (pai-memory-stest--apply :action 'add :target 'project :content "Run make test\nbefore commits")
    (let ((text (pai-memory-snapshot (list :cwd default-directory))))
      (should (string-match-p "## About the user (USER.md)\n- Prefers terse answers" text))
      (should (string-match-p "## This project (project MEMORY.md)\n- Run make test\n  before commits" text))
      (should-not (string-match-p "Environment and tooling" text))
      (should (string-match-p "not instructions" text))
      (should (string-match-p "sessions/" text))
      (should (string-match-p "`memory' tool only when" text)))
    ;; empty memory still explains itself
    (should (string-match-p "nothing remembered yet"
                            (let ((pai-directory (make-temp-file "pai-empty" t)))
                              (pai-memory-snapshot (list :cwd default-directory)))))
    ;; long-term layer off: no section at all
    (setq pai-settings--global '(:memory (:long-term (:enabled :false))))
    (should-not (pai-memory-snapshot (list :cwd default-directory)))))

(ert-deftest pai-memory-external-provider ()
  (pai-memory-stest--with-home dir
    (let ((changes nil))
      (should-error (pai-memory-register-provider (list :snapshot #'ignore)))
      (should-error (pai-memory-register-provider (list :name "markdown")))
      (pai-memory-register-provider
       (list :name "fake" :api-version 1 :future-key 'kept
             :snapshot (lambda (_ctx) "remote says hi")
             :on-change (lambda (change _ctx) (push change changes))))
      ;; registered but not selected: inactive
      (should-not (pai-memory-active-provider))
      (should-not (string-match-p "remote says hi" (pai-memory-snapshot (list :cwd default-directory))))
      (setq pai-settings--global '(:memory (:long-term (:provider "fake"))))
      (should (eq (plist-get (pai-memory-active-provider) :future-key) 'kept))
      (should (string-match-p "remote says hi" (pai-memory-snapshot (list :cwd default-directory))))
      (pai-memory-stest--apply :action 'add :target 'user :content "x")
      ;; on-change is asynchronous
      (should-not changes)
      (accept-process-output nil 0.05)
      (should (equal (plist-get (car changes) :content) "x")))))

(ert-deftest pai-memory-provider-errors-are-isolated ()
  (pai-memory-stest--with-home dir
    (pai-memory-register-provider
     (list :name "broken" :snapshot (lambda (_c) (error "down"))
           :on-change (lambda (_c _x) (error "down"))))
    (setq pai-settings--global '(:memory (:long-term (:provider "broken"))))
    (should (plist-get (pai-memory-stest--apply :action 'add :target 'user :content "still saved") :ok))
    (accept-process-output nil 0.05)
    (should (string-match-p "still saved" (pai-memory-snapshot (list :cwd default-directory))))))

;;;; The memory tool

(defun pai-memory-stest--tool (args &optional session)
  "Run the memory tool with ARGS; return its result."
  (let (res)
    (pai-memory--tool-execute args (list :cwd default-directory :session session) nil
                              (lambda (r) (setq res r)))
    res))

(ert-deftest pai-memory-tool-writes-and-reports ()
  (pai-memory-stest--with-home dir
    (let ((r (pai-memory-stest--tool '(:action "add" :target "user" :content "Name is Dejan"))))
      (should-not (eq (plist-get r :is-error) t))
      (should (string-match-p "applies from the next session" (pai-content-text (plist-get r :content))))
      (should (equal (pai-memory-read 'user default-directory) '("Name is Dejan"))))
    (let ((r (pai-memory-stest--tool '(:action "remove" :target "user" :old "nobody"))))
      (should (eq (plist-get r :is-error) t))
      (should (string-match-p "No entry contains" (pai-content-text (plist-get r :content)))))
    (should (eq t (plist-get (pai-memory-stest--tool '(:action "add" :target "elsewhere" :content "x"))
                             :is-error)))
    ;; propose policy: queued for review, not written
    (setq pai-settings--global '(:memory (:long-term (:memory-tool-policy "propose"))))
    (let ((r (pai-memory-stest--tool '(:action "add" :target "user" :content "y"))))
      (should-not (eq t (plist-get r :is-error)))
      (should (string-match-p "Queued for the user's review" (pai-content-text (plist-get r :content))))
      (should (= (pai-memory-pending-count) 1)))
    ;; long-term off
    (setq pai-settings--global '(:memory (:long-term (:enabled :false))))
    (should (string-match-p "turned off"
                            (pai-content-text (plist-get (pai-memory-stest--tool '(:action "add" :target "user" :content "z"))
                                                         :content))))
    (should (equal (pai-memory-read 'user default-directory) '("Name is Dejan")))))

(ert-deftest pai-memory-tool-is-eager-and-registered ()
  (should-not (pai-tool-deferred-p pai-memory-tool-def))
  (let ((decl (pai-tool-declaration pai-memory-tool-def)))
    (should (equal (plist-get decl :name) "memory"))
    (should-not (string-match-p "Schema not loaded" (plist-get decl :description)))))

;;;; End to end: a new session's system prompt carries the snapshot

(ert-deftest pai-memory-new-session-prompt-has-snapshot ()
  (pai-memory-test--with-pai-buffer buf dir
    (with-current-buffer buf
      ;; register globally, as loading the extension does: `/new' re-copies
      ;; each instance's registrations from the global ones
      (with-temp-buffer (pai-register-extension #'pai-memory-extension "memory"))
      (pai-ext-initialize-instance)
      (unwind-protect
          (pai-memory-stest--new-session-body buf)
        (pai-ext-reset)))))

(defun pai-memory-stest--new-session-body (buf)
  "Body of `pai-memory-new-session-prompt-has-snapshot' in pai buffer BUF."
  (with-current-buffer buf
    (progn
      (pai-memory-apply-change '(:action add :target user :content "Prefers terse answers"))
      ;; the current session's prompt is frozen: no snapshot of the new entry
      (should-not (string-match-p "Prefers terse answers"
                                  (pai-message-content (car pai--context-messages))))
      (should (string-match-p "1 change(s) apply next session"
                              (plist-get (pai-memory-command "" (list :buffer buf)) :message)))
      ;; /new builds a fresh prompt with the snapshot
      (pai-new-command "" (list :buffer buf))
      (let ((prompt (pai-message-content (car pai--context-messages))))
        (should (string-match-p "<memory>" prompt))
        (should (string-match-p "- Prefers terse answers" prompt)))
      (should-not (string-match-p "apply next session"
                                  (plist-get (pai-memory-command "" (list :buffer buf)) :message)))
      ;; the tool is available to the main agent
      (should (pai-tool-get "memory"))
      ;; /memory show and undo
      (should (string-match-p "Prefers terse answers"
                              (plist-get (pai-memory-command "show" (list :buffer buf)) :message)))
      (should (string-match-p "Undid"
                              (plist-get (pai-memory-command "undo" (list :buffer buf)) :message)))
      (should (string-match-p "(empty)"
                              (plist-get (pai-memory-command "show" (list :buffer buf)) :message))))))

(ert-deftest pai-memory-system-sections-hook ()
  (pai-ext-reset)
  (pai-register-extension
   (lambda (api)
     (pai-ext-on api 'system-prompt-sections (lambda (_e _c) '(:alpha "A" :empty "  ")))
     (pai-ext-on api 'system-prompt-sections (lambda (_e _c) nil))
     (pai-ext-on api 'system-prompt-sections (lambda (_e _c) (error "boom")))
     (pai-ext-on api 'system-prompt-sections (lambda (_e _c) '(:beta "B")))))
  (should (equal (pai-ext-run-system-prompt-sections nil) '(:alpha "A" :beta "B")))
  (pai-ext-reset))

(provide 'pai-memory-store-test)
;;; pai-memory-store-test.el ends here
