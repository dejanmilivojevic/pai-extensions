;;; pai-memory-promote-test.el --- Tests for pai-memory Phase 4 -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-ptest--with-home (dir &rest body)
  "Run BODY with a temp pai home DIR and a project as `default-directory'."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-prom" t)))
          (pai-directory ,dir)
          (default-directory (file-name-as-directory (expand-file-name "proj" ,dir)))
          (pai-settings--global nil)
          (pai-settings--project nil))
     (make-directory default-directory t)
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defconst pai-memory-ptest--skill
  "---\ndescription: Run the ERT suite in batch mode and read failures\n---\n# Steps\n1. make test\n2. read FAILED lines\n")

(defun pai-memory-ptest--propose (&rest args)
  "Make and store a proposal from ARGS in the current project."
  (pai-memory-add-proposal
   (apply #'pai-memory-make-proposal
          (append args (list :cwd default-directory :session-id "S1"
                             :skill-dirs (list (expand-file-name "skills" pai-directory)))))))

;;;; Proposals: validation, storage, apply

(ert-deftest pai-memory-proposal-skill-create-normalizes-and-applies ()
  (pai-memory-ptest--with-home dir
    (let* ((p (pai-memory-ptest--propose :kind "skill-create" :name "ert-batch" :scope "global"
                                         :content pai-memory-ptest--skill
                                         :rationale "Worked twice" :evidence '("completed: make test"))))
      (should (equal (plist-get p :status) "pending"))
      (should (equal (plist-get p :target)
                     (expand-file-name "skills/learned/ert-batch/SKILL.md" dir)))
      (let ((text (plist-get p :after)))
        (should (string-match-p "\\`---\nname: ert-batch\ndescription: Run the ERT" text))
        (should (string-match-p "^origin: learned$" text))
        (should (string-match-p "^source-session: S1$" text))
        (should (string-match-p "# Steps" text)))
      (should (= (pai-memory-pending-count) 1))
      (should (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok))
      (should (equal (plist-get (pai-memory-proposal-load (plist-get p :id)) :status) "accepted"))
      ;; discoverable as a skill
      (let ((skill (seq-find (lambda (s) (equal (plist-get s :name) "ert-batch"))
                             (pai-discover-skills (list (expand-file-name "skills" dir))))))
        (should skill))
      ;; undo deletes the created skill
      (should (string-match-p "Undid" (pai-memory-undo)))
      (should-not (file-exists-p (plist-get p :target))))))

(ert-deftest pai-memory-proposal-validation-errors ()
  (pai-memory-ptest--with-home dir
    (should-error (pai-memory-ptest--propose :kind "skill-create" :name "Bad Name"
                                             :content pai-memory-ptest--skill :rationale "r")
                  :type 'user-error)
    (should-error (pai-memory-ptest--propose :kind "skill-create" :name "ok"
                                             :content "no front-matter" :rationale "r")
                  :type 'user-error)
    (should-error (pai-memory-ptest--propose :kind "skill-create" :name "ok"
                                             :content pai-memory-ptest--skill :rationale "")
                  :type 'user-error)
    (should-error (pai-memory-ptest--propose :kind "skill-patch" :target "/etc/passwd"
                                             :content "x" :rationale "r")
                  :type 'user-error)
    (should-error (pai-memory-ptest--propose :kind "memory-remove" :target "user" :old "nothing"
                                             :rationale "r")
                  :type 'user-error)
    (should-error (pai-memory-ptest--propose :kind "frob" :rationale "r") :type 'user-error)
    ;; limits are checked when proposing
    (setq pai-settings--global '(:memory (:long-term (:user-char-limit 10))))
    (should-error (pai-memory-ptest--propose :kind "memory-add" :target "user"
                                             :content (make-string 20 ?x) :rationale "r")
                  :type 'user-error)
    (should (= (pai-memory-pending-count) 0))))

(ert-deftest pai-memory-proposal-skill-patch-and-stale ()
  (pai-memory-ptest--with-home dir
    (let* ((file (expand-file-name "skills/db/SKILL.md" dir)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file (insert "---\nname: db\ndescription: database\n---\nold steps\n"))
      (let ((p (pai-memory-ptest--propose :kind "skill-patch" :target file
                                          :content "---\nname: db\ndescription: database\n---\nnew steps\n"
                                          :rationale "the old steps failed")))
        (should (equal (plist-get p :name) "db"))
        ;; the file changed after the proposal: refused and marked stale
        (with-temp-file file (insert "---\nname: db\ndescription: database\n---\nhand edit\n"))
        (should (string-match-p "stale" (plist-get (pai-memory-proposal-accept (plist-get p :id)) :error)))
        (should (equal (plist-get (pai-memory-proposal-load (plist-get p :id)) :status) "stale"))
        (should (string-match-p "hand edit" (pai-memory--read-file file))))
      ;; a fresh patch applies, and undo restores the old text
      (let ((p (pai-memory-ptest--propose :kind "skill-patch" :target file
                                          :content "---\nname: db\ndescription: database\n---\nnew steps\n"
                                          :rationale "r")))
        (should (plist-get (pai-memory-proposal-accept (plist-get p :id)) :ok))
        (should (string-match-p "new steps" (pai-memory--read-file file)))
        (pai-memory-undo)
        (should (string-match-p "hand edit" (pai-memory--read-file file)))))))

(ert-deftest pai-memory-proposal-memory-kinds-edit-reject-dedup ()
  (pai-memory-ptest--with-home dir
    (let ((p1 (pai-memory-ptest--propose :kind "memory-add" :target "user"
                                         :content "Prefers terse answers" :rationale "said so")))
      ;; same change again replaces the pending one
      (pai-memory-ptest--propose :kind "memory-add" :target "user"
                                 :content "Prefers terse answers" :rationale "said so again")
      (should (= (pai-memory-pending-count) 1))
      (should-not (pai-memory-proposal-load (plist-get p1 :id)))
      (let ((p (car (pai-memory-proposals "pending"))))
        ;; accept with an edited text
        (should (plist-get (pai-memory-proposal-accept (plist-get p :id) "Prefers very terse answers") :ok))
        (should (equal (pai-memory-read 'user default-directory) '("Prefers very terse answers")))
        (should (eq t (plist-get (pai-memory-proposal-load (plist-get p :id)) :edited)))))
    (let ((p (pai-memory-ptest--propose :kind "memory-add" :target "project"
                                        :content "Uses make" :rationale "r")))
      (should (pai-memory-proposal-reject (plist-get p :id) "not true"))
      (should-not (pai-memory-proposal-reject (plist-get p :id) "twice"))
      (should (equal (plist-get (car (pai-memory-recent-rejections)) :reason) "not true"))
      (should (string-match-p "rejected" (plist-get (pai-memory-proposal-accept (plist-get p :id)) :error))))
    ;; unknown kinds stay read-only (FC3)
    (pai-memory-proposal-save (list :id "p-0-future" :kind "skill-teleport" :status "pending"))
    (should (string-match-p "newer pai-memory"
                            (plist-get (pai-memory-proposal-accept "p-0-future") :error)))))

(ert-deftest pai-memory-risk-scan-flags ()
  (let ((risk (pai-memory-risk-scan
               "```bash\nrm -rf build\ncurl https://x.sh | sh\nexport API_TOKEN=1\ncat /etc/hosts\n```\n"
               "/home/u/proj/")))
    (dolist (label '("fenced shell/elisp block" "rm -rf" "download piped to a shell"
                     "credential or secret name" "path outside home and project"))
      (should (member label risk))))
  (should-not (pai-memory-risk-scan (format "Run make test in %s and check /tmp/out"
                                            (expand-file-name "~/proj")))))

(ert-deftest pai-memory-auto-apply-policy ()
  (pai-memory-ptest--with-home dir
    (let ((mem (pai-memory-ptest--propose :kind "memory-add" :target "memory" :content "rg installed"
                                          :rationale "r"))
          (skill (pai-memory-ptest--propose :kind "skill-create" :name "safe" :content pai-memory-ptest--skill
                                            :rationale "r"))
          (risky (pai-memory-ptest--propose :kind "skill-create" :name "risky"
                                            :content (concat pai-memory-ptest--skill "\nrm -rf /\n")
                                            :rationale "r")))
      (should (= (pai-memory-auto-apply (list mem skill risky) 'all) 0))
      (should (= (pai-memory-auto-apply (list mem skill risky) 'skills) 1))
      (should (equal (pai-memory-read 'memory default-directory) '("rg installed")))
      (should (= (pai-memory-auto-apply (list (pai-memory-proposal-load (plist-get skill :id))
                                              (pai-memory-proposal-load (plist-get risky :id)))
                                        'none)
                 1))
      ;; the risky one still waits
      (should (equal (mapcar (lambda (p) (plist-get p :name)) (pai-memory-proposals "pending"))
                     '("risky"))))))

;;;; Digest and due checks

(ert-deftest pai-memory-digest-sources ()
  (pai-memory-test--with-session s dir
    (pai-session-append-message s (pai-system-message "SYS"))
    (let ((e (pai-memory-test--turns s 2 100)))
      ;; nothing observed: the transcript fallback
      (let ((d (pai-memory-session-digest s)))
        (should (eq (plist-get d :source) 'transcript))
        (should (string-match-p "USER: u0" (plist-get d :text)))
        (should-not (string-match-p "SYS" (plist-get d :text))))
      ;; observed: topics, journey and unfiled observations
      (pai-memory-test--commit s "r1" (nth 0 e) (nth 1 e) "completed: built it"
                               "skill-used: ert-batch - deviated: needed -L extensions")
      (let ((mdir (pai-memory-session-dir s t)))
        (with-temp-file (expand-file-name "build.md" mdir)
          (insert "---\nid: build\ntitle: Build\nsummary: s\n---\nmake test\n"))
        (with-temp-file (expand-file-name "JOURNEY.md" mdir) (insert "## t\nBuilt.")))
      (let ((d (pai-memory-session-digest s)))
        (should (eq (plist-get d :source) 'topics))
        (should (string-match-p "## Journey\n## t\nBuilt." (plist-get d :text)))
        (should (string-match-p "## Topic file build.md" (plist-get d :text)))
        (should (string-match-p "completed: built it" (plist-get d :text))))
      (should (equal (mapcar (lambda (o) (plist-get o :content))
                             (pai-memory-signals (pai-session-get-branch s)))
                     '("skill-used: ert-batch - deviated: needed -L extensions"))))))

(ert-deftest pai-memory-promote-due-and-pending ()
  (pai-memory-test--with-settings '(:long-term (:promote-min-session-tokens 50
                                                :promote-min-new-tokens 10))
    (pai-memory-test--with-session s dir
      ;; too short
      (should-not (pai-memory-promote-due-p s 'session-end))
      (pai-memory-test--turns s 2 300)
      (should (pai-memory-promote-due-p s 'session-end))
      (should (pai-memory-promote-due-p s 'consolidation))
      ;; not a configured trigger
      (let ((pai-settings--global '(:memory (:preset "custom" :long-term (:promote ["manual"])))))
        (should-not (pai-memory-promote-due-p s 'session-end)))
      ;; learning off for this session
      (pai-memory-set-session-state s :learning :false)
      (should-not (pai-memory-promote-due-p s 'session-end))
      (pai-memory-set-session-state s :learning t)
      ;; after a promotion of the same digest: not due again
      (let ((d (pai-memory-session-digest s)))
        (pai-memory--state-update :promotions (pai-session-id s)
                                  (list :hash (plist-get d :hash) :tokens (plist-get d :tokens))))
      (should-not (pai-memory-promote-due-p s 'session-end))
      ;; pending bookkeeping
      (pai-memory-mark-pending s)
      (should (equal (car (car (pai-memory-pending-sessions (pai-session-cwd s))))
                     (pai-session-id s)))
      (should-not (pai-memory-pending-sessions (pai-session-cwd s) (pai-session-id s)))
      (pai-memory--state-update :pending (pai-session-id s) nil)
      (should-not (pai-memory-pending-sessions (pai-session-cwd s))))))

;;;; Promoter runs

(defun pai-memory-ptest--promoter-response (&rest proposals)
  "Script a promoter run filing PROPOSALS (arg plists), then done."
  (let ((n 0))
    (pai-faux-push
     (list :tool-calls
           (append (mapcar (lambda (a) (list :id (format "p%d" (cl-incf n)) :name "propose" :arguments a))
                           proposals)
                   (list (list :id "d" :name "done" :arguments '(:summary "ok"))))))))

(ert-deftest pai-memory-promoter-files-proposals ()
  (pai-memory-test--with-settings '(:long-term (:promote-min-session-tokens 0))
    (pai-memory-test--with-owner buf dir
      (let ((pai-skills-extra nil))
        (pai-memory-test--turns session 2 200)
        (pai-memory-ptest--promoter-response
         (list :kind "memory-add" :target "user" :content "Prefers terse answers"
               :rationale "User said so twice" :evidence ["User stated they want short answers"])
         (list :kind "skill-create" :name "ert-batch" :scope "global"
               :content pai-memory-ptest--skill :rationale "Worked" :evidence ["completed: make test"])
         ;; an invalid one: the tool reports an error, the run goes on
         (list :kind "memory-remove" :target "user" :old "nope" :rationale "x"))
        (let ((entry (pai-memory-promote session :reason 'manual :force t)))
          (should entry)
          (should (equal (plist-get entry :status) "completed")))
        (should-not pai-memory--promoting)
        ;; the skill rests on work done, not on the user's steering: refused
        (let ((pending (pai-memory-proposals "pending")))
          (should (= (length pending) 1))
          (should (equal (mapcar (lambda (p) (plist-get p :kind)) pending)
                         '("memory-add"))))
        ;; the promoter saw the digest, the memory files and the skill index
        (let ((task (pai-content-text (pai-message-content
                                       (cadr (plist-get pai-faux-last-context :messages))))))
          (should (string-match-p "## Skill creation\nNOT available" task))
          (should (string-match-p "BEGIN SESSION DIGEST" task))
          (should (string-match-p "## Long-term memory now" task))
          (should (string-match-p "## Existing skills" task)))
        ;; the promotion is recorded; not due again for the same digest
        (should (pai-memory-last-promotion (pai-session-id session)))
        (should (seq-find (lambda (e) (equal (plist-get e :customType) "memory.promoted"))
                          (pai-session-entries session)))
        (should-not (pai-memory-promote-due-p session 'session-end))
        (should (string-match-p "prop:1" (pai-memory-widget-text)))
        (should (equal (plist-get (car (last (pai-memory-cost-entries session))) :role) "promoter"))))))

(ert-deftest pai-memory-promoter-review-policy-skills ()
  (pai-memory-test--with-settings '(:long-term (:review-policy "skills"))
    (pai-memory-test--with-owner buf dir
      (pai-memory-test--turns session 1)
      (pai-memory-ptest--promoter-response
       (list :kind "memory-add" :target "memory" :content "fd is installed" :rationale "seen"))
      (pai-memory-promote session :force t)
      (should (= (pai-memory-pending-count) 0))
      (should (equal (pai-memory-read 'memory default-directory) '("fd is installed"))))))

(ert-deftest pai-memory-promoter-skills-need-steering ()
  "A skill is learned only from what the user taught or corrected, or /learn."
  (pai-memory-test--with-settings '(:long-term (:promote-min-session-tokens 0))
    (pai-memory-test--with-owner buf dir
      (let ((pai-skills-extra nil)
            (e (pai-memory-test--turns session 2 200)))
        ;; work alone: no basis for a skill
        (pai-memory-test--commit session "r1" (nth 0 e) (nth 1 e)
                                 "completed: implemented buffer mentions, tests pass")
        (should-not (pai-memory-skill-creation-basis session nil nil))
        ;; /learn is an explicit request
        (should (eq (pai-memory-skill-creation-basis session "capture it" nil) 'requested))
        ;; the user taught a method: allowed, and the task quotes it
        (pai-memory-test--commit session "r2" (nth 2 e) (nth 3 e)
                                 "taught: test risky elisp in batch emacs under timeout before loading it live")
        (should (equal (mapcar (lambda (o) (plist-get o :content))
                               (plist-get (pai-memory-skill-creation-basis session nil nil) :steering))
                       '("taught: test risky elisp in batch emacs under timeout before loading it live")))
        (pai-memory-ptest--promoter-response
         (list :kind "skill-create" :name "ert-batch" :scope "global"
               :content pai-memory-ptest--skill :rationale "User taught it"
               :evidence ["taught: test risky elisp in batch emacs under timeout before loading it live"]))
        (pai-memory-promote session :reason 'manual :force t)
        (should (equal (mapcar (lambda (p) (plist-get p :kind)) (pai-memory-proposals "pending"))
                       '("skill-create")))
        (let ((task (pai-content-text (pai-message-content
                                       (cadr (plist-get pai-faux-last-context :messages))))))
          (should (string-match-p "## Skill creation\nAvailable, but only" task))
          (should (string-match-p "taught: test risky elisp" task)))))))

(defun pai-memory-ptest--fail-turns (session n)
  "Append a user turn and N failed tool calls to SESSION; return all entry ids."
  (let ((ids (list (pai-session-append-message session (pai-user-message "render it")))))
    (dotimes (i n)
      (push (pai-session-append-message
             session (list :role 'tool-result :tool-call-id (format "t%d" i) :tool-name "bash"
                           :is-error t :content (list (pai-text "exit 1"))))
            ids))
    (push (pai-session-append-message
           session (pai-assistant-message :content (list (pai-text "it works now"))))
          ids)
    (nreverse ids)))

(ert-deftest pai-memory-promoter-discoveries-need-measured-failures ()
  "A trial-and-error discovery counts only with enough failed tool calls."
  (pai-memory-test--with-settings '(:long-term (:discovery-min-failures 3))
    (pai-memory-test--with-owner buf dir
      ;; two failures: the observer's word is not enough
      (let ((ids (pai-memory-ptest--fail-turns session 2)))
        (pai-memory-test--commit session "r1" (car ids) (car (last ids))
                                 "discovered: add xmlns to bare svg before rasterizing -- failed first: blank canvas")
        (should-not (pai-memory-discoveries session))
        (should-not (pai-memory-skill-creation-basis session nil nil)))
      ;; a stretch with three failures: counted, with the number shown
      (let ((ids (pai-memory-ptest--fail-turns session 3)))
        (pai-memory-test--commit session "r2" (car ids) (car (last ids))
                                 "discovered: strip foreignObject labels before canvas export -- failed first: tainted canvas")
        (let ((found (pai-memory-discoveries session)))
          (should (= (length found) 1))
          (should (= (plist-get (car found) :failures) 3))
          (should (string-match-p "foreignObject" (plist-get (car found) :content))))
        (let ((text (pai-memory--skill-creation-text (pai-memory-skill-creation-basis session nil nil))))
          (should (string-match-p "found by trial and error" text))
          (should (string-match-p "\\[3 failed tool calls\\]" text))))
      ;; 0 turns discoveries off
      (let ((pai-settings--global '(:memory (:long-term (:discovery-min-failures 0)))))
        (should-not (pai-memory-discoveries session))))))

(ert-deftest pai-memory-promoter-new-skill-must-quote-basis ()
  "Code, not the model, checks that a new skill rests on a listed observation."
  (pai-memory-ptest--with-home dir
    (let* ((basis (list :steering (list (list :content "taught: test risky elisp in batch emacs under timeout")) :discoveries nil))
           (store (list nil))
           (tools (pai-memory-promoter-tools (pai-session-new default-directory 'memory)
                                             (list (expand-file-name "skills" dir)) store nil basis))
           (propose (seq-find (lambda (x) (equal (plist-get x :name) "propose")) tools))
           (call (lambda (evidence)
                   (let (res)
                     (funcall (plist-get propose :execute)
                              (list :kind "skill-create" :name "elisp-live-testing" :scope "global"
                                    :content pai-memory-ptest--skill :rationale "r" :evidence evidence)
                              nil nil (lambda (r) (setq res r)))
                     res))))
      ;; made-up evidence: refused
      (let ((res (funcall call ["the user likes testing"])))
        (should (eq (plist-get res :is-error) t))
        (should (string-match-p "must quote" (pai-content-text (plist-get res :content)))))
      ;; quoting (part of) the observation, spacing and case aside: accepted
      (should-not (eq (plist-get (funcall call ["Test risky elisp in  batch Emacs"]) :is-error) t))
      (should (= (length (car store)) 1)))))

(ert-deftest pai-memory-promoter-read-tools-are-confined ()
  (pai-memory-ptest--with-home dir
    (let* ((skills (expand-file-name "skills" dir))
           (_ (make-directory skills t))
           (tools (pai-memory-promoter-tools (pai-session-new default-directory 'memory)
                                             (list skills) (list nil)))
           (read (seq-find (lambda (x) (equal (plist-get x :name) "read")) tools))
           (res nil))
      (should (equal (sort (mapcar (lambda (x) (plist-get x :name)) tools) #'string<)
                     '("done" "grep" "ls" "propose" "read")))
      (with-temp-file (expand-file-name "auth.json" dir) (insert "secret"))
      (funcall (plist-get read :execute) (list :path (expand-file-name "auth.json" dir)) nil nil
               (lambda (r) (setq res r)))
      (should (eq (plist-get res :is-error) t))
      (with-temp-file (expand-file-name "a.md" skills) (insert "skill text"))
      (funcall (plist-get read :execute) (list :path (expand-file-name "a.md" skills)) nil nil
               (lambda (r) (setq res r)))
      (should-not (eq (plist-get res :is-error) t)))))

(ert-deftest pai-memory-promoter-patch-needs-fresh-read ()
  "skill-patch is refused until the target was read in this run."
  (pai-memory-ptest--with-home dir
    (let* ((skills (expand-file-name "skills" dir))
           (skill-dir (expand-file-name "tests" skills))
           (file (expand-file-name "SKILL.md" skill-dir))
           (_ (progn (make-directory skill-dir t)
                     (with-temp-file file (insert pai-memory-ptest--skill))))
           (store (list nil))
           (tools (pai-memory-promoter-tools (pai-session-new default-directory 'memory)
                                             (list skills) store))
           (tool (lambda (name) (seq-find (lambda (x) (equal (plist-get x :name) name)) tools)))
           (call (lambda (name args)
                   (let (res)
                     (funcall (plist-get (funcall tool name) :execute) args nil nil
                              (lambda (r) (setq res r)))
                     res)))
           (patch (list :kind "skill-patch" :target file
                        :content (concat pai-memory-ptest--skill "3. rerun only the failing test\n")
                        :rationale "r" :evidence ["correction: x -> y"])))
      ;; not read yet: refused, nothing filed
      (let ((res (funcall call "propose" patch)))
        (should (eq (plist-get res :is-error) t))
        (should (string-match-p "read .* first" (pai-content-text (plist-get res :content))))
        (should-not (car store)))
      ;; after reading it in this run the patch is filed
      (should-not (eq (plist-get (funcall call "read" (list :path file)) :is-error) t))
      (should-not (eq (plist-get (funcall call "propose" patch) :is-error) t))
      (should (= (length (car store)) 1)))))

(ert-deftest pai-memory-promoter-lists-skills-used ()
  "The task lists skills used this session as first patch candidates."
  (pai-memory-test--with-owner buf dir
    (let ((e (pai-memory-test--turns session 1 200)))
      (pai-memory-test--commit session "r1" (nth 0 e) (nth 1 e)
                               "skill-used: ert-batch followed")
      (let ((task (pai-memory-promoter-prompt session "digest" nil)))
        (should (string-match-p "## Skills used in this session (first candidates for skill-patch)\n.*skill-used: ert-batch followed" task))))))

(ert-deftest pai-memory-session-end-and-catch-up ()
  (pai-memory-test--with-settings '(:long-term (:promote-min-session-tokens 0))
    (pai-memory-test--with-owner buf dir
      ;; a previous session of this project that never reached its end
      (let ((old (pai-session-new dir)))
        (pai-memory-test--turns old 2)
        (pai-memory-mark-pending old)
        (pai-memory-test--turns session 1)
        (pai-faux-push '(:tool-calls ((:id "d" :name "done" :arguments (:summary "nothing")))))
        (pai-memory-catch-up)
        ;; promoted in the background and no longer pending
        (should-not (pai-memory-pending-sessions dir))
        (should (pai-memory-last-promotion (pai-session-id old)))
        ;; leaving the current session promotes it
        (pai-faux-push '(:tool-calls ((:id "d" :name "done" :arguments (:summary "nothing")))))
        (pai-memory--on-session-end nil (list :buffer buf))
        (should (pai-memory-last-promotion (pai-session-id session)))))))

(ert-deftest pai-memory-promoter-budget-marks-pending ()
  (pai-memory-test--with-settings '(:budget (:session-usd 0.001))
    (pai-memory-test--with-owner buf dir
      (pai-memory-test--turns session 1)
      (pai-session-append-custom session "memory.cost" (list :role "observer" :cost 0.01))
      (should-not (pai-memory-promote session))
      (should (pai-memory-pending-sessions dir))
      ;; user-invoked runs are not held back
      (pai-faux-push '(:tool-calls ((:id "d" :name "done" :arguments (:summary "x")))))
      (should (pai-memory-promote session :force t)))))

;;;; Commands and review buffer

(ert-deftest pai-memory-learn-command-focuses-on-skills ()
  (pai-memory-test--with-owner buf dir
    (pai-memory-test--turns session 1)
    (pai-faux-push '(:tool-calls ((:id "d" :name "done" :arguments (:summary "x")))))
    (should (string-match-p "Learning from this session"
                            (plist-get (pai-memory-learn-command "how we run the tests"
                                                                 (list :buffer buf))
                                       :message)))
    (let ((task (pai-content-text (pai-message-content
                                   (cadr (plist-get pai-faux-last-context :messages))))))
      (should (string-match-p "/learn" task))
      (should (string-match-p "how we run the tests" task)))))

(ert-deftest pai-memory-review-buffer-actions ()
  (pai-memory-ptest--with-home dir
    (pai-memory-ptest--propose :kind "memory-add" :target "user" :content "Likes Emacs"
                               :rationale "said so" :evidence '("User stated they like Emacs"))
    (pai-memory-ptest--propose :kind "memory-add" :target "memory" :content "Has ripgrep"
                               :rationale "seen")
    (let ((buf (pai-memory-review)))
      (unwind-protect
          (with-current-buffer buf
            (should (string-match-p "2 pending" (buffer-string)))
            (should (pai-memory-review--id-at-point))
            ;; details
            (pai-memory-review-toggle)
            (should (string-match-p "Why: said so" (buffer-string)))
            (should (string-match-p "User stated they like Emacs" (buffer-string)))
            (should (string-match-p "\\+Likes Emacs" (buffer-string)))
            ;; accept the first, reject the second
            (pai-memory-review-accept)
            (should (equal (pai-memory-read 'user default-directory) '("Likes Emacs")))
            (should (string-match-p "1 pending" (buffer-string)))
            (pai-memory-review-reject "wrong")
            (should (string-match-p "Nothing to review" (buffer-string)))
            (should (equal (plist-get (car (pai-memory-recent-rejections)) :reason) "wrong")))
        (kill-buffer buf)))))

(ert-deftest pai-memory-string-list-normalizes-loose-evidence ()
  (should (equal (pai-memory--string-list nil) nil))
  (should (equal (pai-memory--string-list ["a" "b"]) '("a" "b")))
  (should (equal (pai-memory--string-list '("a")) '("a")))
  (should (equal (pai-memory--string-list "- \"one\"\n\n* two\n3. three")
                 '("\"one\"" "two" "three")))
  ;; a string mangled into character codes by `append'
  (should (equal (pai-memory--string-list (append "- x-y\n- z" nil)) '("x-y" "z"))))

(ert-deftest pai-memory-review-details-with-string-evidence ()
  "A model sending evidence as one string must not break /memory-review."
  (pai-memory-ptest--with-home dir
    (let ((p (pai-memory-ptest--propose :kind "memory-add" :target "user" :content "Likes Emacs"
                                        :rationale "said so"
                                        :evidence "- \"User stated it\"\n- Second line")))
      (should (equal (plist-get p :evidence) ["\"User stated it\"" "Second line"]))
      ;; proposals already stored with evidence as character codes still render
      (should (string-match-p "- Second line"
                              (pai-memory-review--details
                               (plist-put (copy-sequence p) :evidence
                                          (append "- a\n- Second line" nil))))))))

(ert-deftest pai-memory-review-edit-accepts-edited-text ()
  (pai-memory-ptest--with-home dir
    (pai-memory-ptest--propose :kind "memory-add" :target "user" :content "Likes vi" :rationale "r")
    (let ((buf (pai-memory-review)))
      (unwind-protect
          (with-current-buffer buf
            (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
              (pai-memory-review-edit)
              (should (equal (buffer-string) "Likes vi"))
              (erase-buffer) (insert "Likes Emacs")
              (cl-letf (((symbol-function 'quit-window) (lambda (&rest _) nil)))
                (pai-memory-review-edit-finish)))
            (should (equal (pai-memory-read 'user default-directory) '("Likes Emacs"))))
        (kill-buffer buf)
        (dolist (b (buffer-list))
          (when (string-prefix-p "*pai proposal" (buffer-name b)) (kill-buffer b)))))))

(provide 'pai-memory-promote-test)
;;; pai-memory-promote-test.el ends here
