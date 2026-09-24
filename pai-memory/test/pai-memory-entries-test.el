;;; pai-memory-entries-test.el --- Tests for entry metadata, retrieval mode, team memory (V2 B2, B3, G2) -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-et--with-home (dir &rest body)
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-ent" t)))
          (pai-directory ,dir)
          (default-directory (file-name-as-directory (expand-file-name "proj" ,dir)))
          (pai-settings--global nil) (pai-settings--project nil)
          (pai-memory--providers nil))
     (make-directory default-directory t)
     (unwind-protect (progn ,@body) (delete-directory ,dir t))))

(defun pai-memory-et--apply (&rest change)
  (pai-memory-apply-change change (list :cwd default-directory :session nil)))

(defun pai-memory-et--rec (quote)
  (pai-memory-entry-find quote default-directory))

(ert-deftest pai-memory-entries-track-add-replace-remove-undo ()
  (pai-memory-et--with-home dir
    (should (plist-get (pai-memory-et--apply :action 'add :target 'user :content "prefers terse answers"
                                             :origin "memory-tool") :ok))
    (let* ((r (pai-memory-et--rec "terse")) (id (plist-get r :id)))
      (should (string-prefix-p "e-" id))
      (should (equal (plist-get r :origin) "memory-tool"))
      (should (= (pai-memory-entry-confidence r) 0.9))
      (should (= (length (plist-get r :changes)) 1))
      ;; replace keeps the id
      (pai-memory-et--apply :action 'replace :target 'user :old "terse" :content "prefers very terse answers")
      (let ((r2 (pai-memory-et--rec "very terse")))
        (should (equal (plist-get r2 :id) id))
        (should (= (length (plist-get r2 :previous)) 1))
        (should (= (length (plist-get r2 :changes)) 2)))
      ;; undo the replace: the same record points at the old text again
      (should (string-match-p "Undid" (pai-memory-undo)))
      (should (equal (plist-get (pai-memory-et--rec "prefers terse") :id) id))
      ;; remove: the record stays, marked removed
      (pai-memory-et--apply :action 'remove :target 'user :old "terse")
      (should-error (pai-memory-et--rec "terse") :type 'user-error)
      (let ((all (pai-memory--meta-load)))
        (should (= (length all) 1))
        (should-not (pai-memory--live-p (car all)))
        (should (string-prefix-p "m-" (plist-get (car all) :removed_by))))
      ;; undo the remove: revived with its id
      (pai-memory-undo)
      (should (equal (plist-get (pai-memory-et--rec "terse") :id) id)))))

(ert-deftest pai-memory-entries-migrate-and-hand-edits ()
  (pai-memory-et--with-home dir
    ;; a V1 change in the log, before any metadata existed
    (pai-memory-et--apply :action 'add :target 'memory :content "uses Emacs 30" :origin "proposal"
                          :proposal-id "p-old")
    (delete-file (pai-memory-entries-file))
    ;; plus a hand-written entry
    (let ((f (pai-memory-target-file 'memory default-directory)))
      (with-temp-file f (insert "uses Emacs 30\n§\nhand written fact\n")))
    (let ((recovered (pai-memory-et--rec "Emacs 30"))
          (hand (pai-memory-et--rec "hand written")))
      (should (equal (plist-get recovered :origin) "proposal"))
      (should (equal (plist-get recovered :proposal_id) "p-old"))
      (should (= (length (plist-get recovered :changes)) 1))
      (should (equal (plist-get hand :origin) "manual"))
      (should (= (pai-memory-entry-confidence hand) 1.0)))))

(ert-deftest pai-memory-entries-confirm-expire-snapshot ()
  (pai-memory-et--with-home dir
    (pai-memory-et--apply :action 'add :target 'user :content "is on vacation" :expires "2020-01-01")
    (pai-memory-et--apply :action 'add :target 'user :content "likes ERT" :origin "proposal")
    (should (pai-memory-entry-expired-p (pai-memory-et--rec "vacation")))
    (let ((snap (pai-memory-snapshot (list :cwd default-directory))))
      (should (string-match-p "likes ERT" snap))
      (should-not (string-match-p "vacation" snap)))
    ;; confirmation via proposal: auto-applied whatever the policy
    (let ((p (pai-memory-add-proposal
              (pai-memory-make-proposal :kind "memory-confirm" :target "user" :old "ERT"
                                        :rationale "said so again" :session-id "S1"))))
      (should (= (pai-memory-auto-apply (list p) 'all) 1))
      (let ((r (pai-memory-et--rec "ERT")))
        (should (= (length (plist-get r :confirmed)) 1))
        (should (< (abs (- (pai-memory-entry-confidence r) 0.7)) 1e-9))))
    (should-error (pai-memory-make-proposal :kind "memory-add" :target "user" :content "x"
                                            :rationale "r" :expires "soon")
                  :type 'user-error)
    ;; curator proposes removing the expired entry, once
    (should (= (pai-memory-expire-proposals default-directory) 1))
    (should (= (pai-memory-expire-proposals default-directory) 0))
    (let ((p (car (pai-memory-proposals "pending"))))
      (should (equal (plist-get p :kind) "memory-remove"))
      (should (string-match-p "expired on 2020-01-01" (plist-get p :rationale))))))

(ert-deftest pai-memory-entries-overlaps-and-promoter-text ()
  (pai-memory-et--with-home dir
    (pai-memory-et--apply :action 'add :target 'memory :content "tests run with make test in the repo root")
    (pai-memory-et--apply :action 'add :target 'memory :content "tests run with make check in the repo root")
    (pai-memory-et--apply :action 'add :target 'memory :content "the laptop has 32 GB of RAM")
    (let ((ov (pai-memory-entry-overlaps default-directory)))
      (should (= (length ov) 1))
      (should (string-match-p "make" (nth 1 (car ov)))))
    (let ((text (pai-memory--ltm-text default-directory)))
      (should (string-match-p "\\[confidence 0.7, since " text)))
    (should (string-match-p "make check" (pai-memory--overlaps-text default-directory)))))

(ert-deftest pai-memory-retrieval-mode ()
  (pai-memory-et--with-home dir
    (setq pai-settings--global '(:memory (:long-term (:ltm-injection "retrieval" :retrieval-entries 2
                                                      :user-char-limit 50))))
    (dotimes (i 5)
      (should (plist-get (pai-memory-et--apply :action 'add :target 'user
                                               :content (format "fact number %d about the user" i))
                         :ok)))
    ;; caps don't apply to unpinned entries in retrieval mode
    (should (> (length (pai-memory--read-file (pai-memory-target-file 'user default-directory))) 50))
    (should (string-match-p "Pinned" (pai-memory-pin-entry "number 0" default-directory t)))
    ;; pinned entries do count against the cap
    (should-error (pai-memory-pin-entry "number 1" default-directory t) :type 'user-error)
    (let* ((sel (pai-memory-snapshot-selection default-directory))
           (texts (alist-get 'user (car sel))))
      (should (member "fact number 0 about the user" texts))
      (should (= (length texts) 3))
      (should (= (cdr sel) 2)))
    (should (string-match-p "2 more remembered entries"
                            (pai-memory-snapshot (list :cwd default-directory))))
    (pai-memory-pin-entry "number 0" default-directory nil)
    (should-not (pai-memory-entry-pinned-p (pai-memory-et--rec "number 0")))))

(ert-deftest pai-memory-team-memory ()
  (pai-memory-et--with-home dir
    (should-not (memq 'team (pai-memory-active-targets default-directory)))
    (should-error (pai-memory-make-proposal :kind "team-memory-add" :content "build with make"
                                            :rationale "r" :cwd default-directory)
                  :type 'user-error)
    (cl-letf (((symbol-function 'pai-trust-get) (lambda (_cwd) 'yes)))
      (should (memq 'team (pai-memory-active-targets default-directory)))
      ;; memory-add can't target the team file; the tool refuses it too
      (should-error (pai-memory-make-proposal :kind "memory-add" :target "team" :content "x"
                                              :rationale "r" :cwd default-directory)
                    :type 'user-error)
      (let ((p (pai-memory-add-proposal
                (pai-memory-make-proposal :kind "team-memory-add" :content "build with make -j8"
                                          :rationale "the whole team builds this way" :cwd default-directory))))
        ;; never auto-applied
        (should (= (pai-memory-auto-apply (list p) 'none) 0))
        (should (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok))
        (should (file-exists-p (expand-file-name ".pai/memory/PROJECT.md" default-directory))))
      ;; team memory goes into the snapshot before the personal project memory
      (pai-memory-et--apply :action 'add :target 'project :content "my own project note")
      (let ((snap (pai-memory-snapshot (list :cwd default-directory))))
        (should (< (string-search "make -j8" snap) (string-search "my own project note" snap)))))
    ;; untrusted again: not loaded
    (should-not (string-match-p "make -j8" (pai-memory-snapshot (list :cwd default-directory))))
    (let (result)
      (pai-memory--tool-execute (list :action "add" :target "team" :content "x")
                                (list :cwd default-directory :session nil) nil
                                (lambda (r) (setq result r)))
      (should (eq (plist-get result :is-error) t)))))

;;;; F2: why do you know this

(ert-deftest pai-memory-why-entry-chain ()
  (pai-memory-et--with-home dir
    ;; a session with an observation the proposal's evidence quotes
    (let* ((sid "01aaaaaa-0000-7000-8000-000000000001")
           (sdir (expand-file-name "sessions/proj" dir)))
      (make-directory sdir t)
      (with-temp-file (expand-file-name (concat sid ".jsonl") sdir)
        (insert (pai-json-encode (list :type "session" :version 1 :id sid :cwd default-directory)) "\n"
                (pai-json-encode (list :type "message" :id "msg00001" :message (list :role "user" :content "I prefer tabs")))
                "\n"
                (pai-json-encode (list :type "custom" :id "obs00001" :customType "memory.observations"
                                       :data (list :runId "r1" :coversFromId "msg00001" :coversUpToId "msg00001"
                                                   :observations (vector (list :id "o1" :timestamp "t"
                                                                               :content "user said they prefer tabs over spaces")))))
                "\n"))
      (let ((p (pai-memory-add-proposal
                (pai-memory-make-proposal :kind "memory-add" :target "user" :content "prefers tabs"
                                          :rationale "stated twice" :session-id sid
                                          :evidence (list "[t] user said they prefer tabs over spaces")))))
        (should (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok))
        (let ((r (pai-memory-et--rec "tabs")))
          (should (equal (plist-get r :source_session) sid))
          (should (equal (plist-get r :proposal_id) (plist-get p :id))))
        (let ((buf (pai-memory-why "tabs" default-directory)))
          (unwind-protect
              (with-current-buffer buf
                (let ((text (buffer-string)))
                  (should (string-match-p "learned: an accepted proposal" text))
                  (should (string-match-p "stated twice" text))
                  (should (string-match-p "prefer tabs over spaces" text))
                  (should (string-match-p "→ in the conversation" text)))
                ;; the observation button opens the session at the covered message
                (goto-char (point-min))
                (search-forward "→ in the conversation")
                (push-button (1- (point)))
                (should (string-suffix-p (concat sid ".jsonl") (buffer-file-name)))
                (should buffer-read-only)
                (should (looking-at ".*\"msg00001\""))
                (kill-buffer))
            (kill-buffer buf)))))))

(ert-deftest pai-memory-why-skill-and-at-point ()
  (pai-memory-et--with-home dir
    (cl-letf (((symbol-function 'pai-memory--skill-dirs)
               (lambda () (list (expand-file-name "skills" pai-directory)))))
      (let ((p (pai-memory-add-proposal
                (pai-memory-make-proposal :kind "skill-create" :name "brew-tea" :rationale "done it twice"
                                          :session-id "S9"
                                          :content "---\ndescription: Use when brewing tea for the team\n---\n# Steps\n1. boil\n"))))
        (pai-memory-proposal-accept (plist-get p :id))
        (let ((buf (pai-memory-why "brew-tea")))
          (unwind-protect
              (with-current-buffer buf
                (should (string-match-p "Why is there a skill brew-tea" (buffer-string)))
                (should (string-match-p "done it twice" (buffer-string)))
                (should (string-match-p "skill-create" (buffer-string))))
            (kill-buffer buf)))
        ;; at point in USER.md
        (pai-memory-et--apply :action 'add :target 'user :content "first fact" :origin "memory-tool")
        (pai-memory-et--apply :action 'add :target 'user :content "second fact" :origin "memory-tool")
        (with-current-buffer (find-file-noselect (pai-memory-target-file 'user default-directory))
          (goto-char (point-max)) (forward-line -1)
          (let ((buf (pai-memory-why-at-point)))
            (with-current-buffer buf (should (string-match-p "second fact" (buffer-string))))
            (kill-buffer buf))
          (kill-buffer))))))

(ert-deftest pai-memory-pin-command-entries-and-skills ()
  (pai-memory-et--with-home dir
    (cl-letf (((symbol-function 'pai-memory--skills) (lambda () nil)))
      (pai-memory-et--apply :action 'add :target 'memory :content "the NAS is at 10.0.0.5")
      (should (string-match-p "Pinned memory entry" (pai-memory--pin "10.0.0.5" t)))
      (should (pai-memory-entry-pinned-p (pai-memory-et--rec "NAS")))
      (should (string-match-p "No skill named nope" (pai-memory--pin "nope" t))))))

(ert-deftest pai-memory-team-memory-git-add ()
  (skip-unless (executable-find "git"))
  (pai-memory-et--with-home dir
    (call-process "git" nil nil nil "-C" default-directory "init" "-q")
    (cl-letf (((symbol-function 'pai-trust-get) (lambda (_cwd) 'yes)))
      (let ((p (pai-memory-add-proposal
                (pai-memory-make-proposal :kind "team-memory-add" :content "run make test before pushing"
                                          :rationale "team rule" :cwd default-directory))))
        (should (pai-memory-project-skill-in-git-p p))
        (let ((buf (pai-memory-review)))
          (unwind-protect
              (with-current-buffer buf
                (should (string-match-p "team add PROJECT.md" (buffer-string)))
                (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                  (pai-memory-review-accept-git)))
            (kill-buffer buf)))
        (should (string-match-p "run make test" (pai-memory--read-file (pai-memory-target-file 'team default-directory))))
        (should (string-match-p ".pai/memory/PROJECT.md"
                                (with-temp-buffer
                                  (call-process "git" nil t nil "-C" default-directory "diff" "--cached" "--name-only")
                                  (buffer-string))))))))

(provide 'pai-memory-entries-test)
;;; pai-memory-entries-test.el ends here
