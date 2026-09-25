;;; pai-memory-move-test.el --- Moving memory between targets -*- lexical-binding: t; -*-

;;; Commentary:

;; `m' in /memory-browse moves an entry to another memory file (one undoable
;; change that keeps the entry's metadata); `m' in /memory-review sends a
;; pending add proposal to another memory before it is accepted.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-mv--with-home (dir &rest body)
  "Run BODY with a temp pai home DIR and a project as `default-directory'."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-move" t)))
          (pai-directory ,dir)
          (default-directory (file-name-as-directory (expand-file-name "proj" ,dir)))
          (pai-settings--global nil)
          (pai-settings--project nil)
          (pai-memory--providers nil))
     (make-directory default-directory t)
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defun pai-memory-mv--add (target content)
  "Remember CONTENT in TARGET of the current project."
  (should (plist-get (pai-memory-apply-change (list :action 'add :target target :content content)
                                              (list :cwd default-directory :session nil))
                     :ok)))

(defun pai-memory-mv--ctx ()
  (list :cwd default-directory :session nil))

;;;; Moving entries

(ert-deftest pai-memory-move-entry-keeps-metadata-and-undoes-as-one ()
  (pai-memory-mv--with-home dir
    (pai-memory-mv--add 'memory "Makefile needs two compile passes")
    (pai-memory-mv--add 'memory "Uses Emacs 30")
    (pai-memory-pin-entry "two compile passes" default-directory t)
    (pai-memory-entry-confirm 'memory default-directory "two compile passes")
    (let* ((before (pai-memory-entry-find "two compile passes" default-directory))
           (r (pai-memory-move-entry "two compile" 'memory 'project (pai-memory-mv--ctx))))
      (should (plist-get r :ok))
      (should (equal (pai-memory-read 'memory default-directory) '("Uses Emacs 30")))
      (should (equal (pai-memory-read 'project default-directory) '("Makefile needs two compile passes")))
      ;; the same record, now in project memory, pin and confirmation kept
      (let ((after (pai-memory-entry-find "two compile passes" default-directory)))
        (should (equal (plist-get after :id) (plist-get before :id)))
        (should (equal (plist-get after :target) "project"))
        (should (pai-memory-entry-pinned-p after))
        (should (= 1 (length (plist-get after :confirmed))))
        (should (member (plist-get r :id) (append (plist-get after :changes) nil))))
      ;; logged once, as a move
      (let ((rec (car (last (pai-memory-log-read)))))
        (should (equal (plist-get rec :action) "move"))
        (should (equal (plist-get rec :from) "memory"))
        (should (equal (plist-get rec :target) "project")))
      ;; one undo moves it back, metadata included
      (should (string-match-p "Undid .* (move)" (pai-memory-undo nil (pai-memory-mv--ctx))))
      (should (equal (pai-memory-read 'memory default-directory)
                     '("Makefile needs two compile passes" "Uses Emacs 30")))
      (should-not (pai-memory-read 'project default-directory))
      (let ((back (pai-memory-entry-find "two compile passes" default-directory)))
        (should (equal (plist-get back :id) (plist-get before :id)))
        (should (equal (plist-get back :target) "memory"))
        (should (pai-memory-entry-pinned-p back))))))

(ert-deftest pai-memory-move-entry-refusals ()
  (pai-memory-mv--with-home dir
    (pai-memory-mv--add 'memory "Shared fact")
    (pai-memory-mv--add 'project "Shared fact")
    (pai-memory-mv--add 'user "Likes tea")
    ;; same target
    (should (string-match-p "already in memory"
                            (plist-get (pai-memory-move-entry "Shared" 'memory 'memory (pai-memory-mv--ctx))
                                       :error)))
    ;; the destination already holds it
    (should (string-match-p "already holds this entry"
                            (plist-get (pai-memory-move-entry "Shared" 'memory 'project (pai-memory-mv--ctx))
                                       :error)))
    ;; unknown quote
    (should (plist-get (pai-memory-move-entry "nothing like it" 'user 'project (pai-memory-mv--ctx))
                       :error))
    ;; team memory needs a trusted project
    (should (string-match-p "trusted"
                            (plist-get (pai-memory-move-entry "Likes tea" 'user 'team (pai-memory-mv--ctx))
                                       :error)))
    ;; nothing changed
    (should (equal (pai-memory-read 'user default-directory) '("Likes tea")))
    (should (equal (pai-memory-read 'memory default-directory) '("Shared fact")))))

(ert-deftest pai-memory-move-entry-respects-the-destination-limit ()
  (pai-memory-mv--with-home dir
    (pai-memory-mv--add 'user (make-string 200 ?x))
    (let ((pai-settings--global '(:memory (:long-term (:project-char-limit 50)))))
      (should (string-match-p "over its 50 limit"
                              (plist-get (pai-memory-move-entry "xxxx" 'user 'project (pai-memory-mv--ctx))
                                         :error))))
    (should (pai-memory-read 'user default-directory))))

(ert-deftest pai-memory-move-entry-into-team-when-trusted ()
  (pai-memory-mv--with-home dir
    (cl-letf (((symbol-function 'pai-memory-team-allowed-p) (lambda (_) t)))
      (pai-memory-mv--add 'project "Run make twice")
      (should (plist-get (pai-memory-move-entry "make twice" 'project 'team (pai-memory-mv--ctx)) :ok))
      (should (equal (pai-memory-read 'team default-directory) '("Run make twice")))
      (should (file-exists-p (expand-file-name ".pai/memory/PROJECT.md" default-directory))))))

;;;; Retargeting proposals

(defun pai-memory-mv--propose (&rest args)
  (pai-memory-add-proposal
   (apply #'pai-memory-make-proposal
          (append args (list :cwd default-directory :session-id "S1")))))

(ert-deftest pai-memory-proposal-retarget-rebuilds-for-the-new-memory ()
  (pai-memory-mv--with-home dir
    (pai-memory-mv--add 'project "Existing project note")
    (let* ((p (pai-memory-mv--propose :kind "memory-add" :target "memory"
                                      :content "Tests need a temp HOME"
                                      :rationale "Seen twice" :evidence '("make test")
                                      :expires "2099-01-01"))
           (id (plist-get p :id)))
      (should (equal (pai-memory-proposal-retarget-choices p) '("user" "project")))
      (let ((q (pai-memory-proposal-retarget id "project")))
        (should (equal (plist-get q :id) id))
        (should (equal (plist-get q :created) (plist-get p :created)))
        (should (equal (plist-get q :target) "project"))
        (should (equal (plist-get q :retargeted-from) "memory"))
        (should (equal (plist-get q :expires) "2099-01-01"))
        ;; the diff is against the project file now
        (should (equal (plist-get q :before) "Existing project note\n"))
        (should (string-match-p "Tests need a temp HOME" (plist-get q :after))))
      ;; still one pending proposal, and accepting writes the project memory
      (should (= 1 (length (pai-memory-proposals "pending"))))
      (should (plist-get (pai-memory-proposal-accept id) :ok))
      (should (equal (pai-memory-read 'project default-directory)
                     '("Existing project note" "Tests need a temp HOME")))
      (should-not (pai-memory-read 'memory default-directory)))))

(ert-deftest pai-memory-proposal-retarget-refusals ()
  (pai-memory-mv--with-home dir
    (pai-memory-mv--add 'user "Likes tea")
    (let ((add (pai-memory-mv--propose :kind "memory-add" :target "user" :content "Likes coffee"
                                       :rationale "said so"))
          (rm (pai-memory-mv--propose :kind "memory-remove" :target "user" :old "tea"
                                      :rationale "changed")))
      (should-not (pai-memory-proposal-retarget-choices rm))
      (should-error (pai-memory-proposal-retarget (plist-get rm :id) "project") :type 'user-error)
      (should-error (pai-memory-proposal-retarget (plist-get add :id) "user") :type 'user-error)
      ;; team memory: untrusted project
      (should-not (member "team" (pai-memory-proposal-retarget-choices add)))
      (should-error (pai-memory-proposal-retarget (plist-get add :id) "team") :type 'user-error)
      ;; unchanged after the refusals
      (should (equal (plist-get (pai-memory-proposal-load (plist-get add :id)) :target) "user")))))

(ert-deftest pai-memory-proposal-retarget-to-team-switches-kind ()
  (pai-memory-mv--with-home dir
    (cl-letf (((symbol-function 'pai-memory-team-allowed-p) (lambda (_) t)))
      (let* ((p (pai-memory-mv--propose :kind "memory-add" :target "project" :content "CI runs make test"
                                        :rationale "team fact"))
             (q (pai-memory-proposal-retarget (plist-get p :id) "team")))
        (should (equal (plist-get q :kind) "team-memory-add"))
        (should (equal (plist-get q :target) "team"))
        ;; and back
        (let ((r (pai-memory-proposal-retarget (plist-get p :id) "memory")))
          (should (equal (plist-get r :kind) "memory-add"))
          (should (equal (plist-get r :retargeted-from) "project")))))))

;;;; The keys

(ert-deftest pai-memory-review-m-moves-the-proposal ()
  (pai-memory-mv--with-home dir
    (let* ((p (pai-memory-mv--propose :kind "memory-add" :target "memory" :content "Project uses vui"
                                      :rationale "seen")))
      (let ((buf (pai-memory-review)))
        (unwind-protect
            (with-current-buffer buf
              (should (eq (lookup-key pai-memory-review-mode-map "m") #'pai-memory-review-move))
              (should (string-match-p "m move to another memory" (buffer-string)))
              (goto-char (point-min))
              (search-forward "Project uses vui")
              (let ((offered nil))
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (_prompt table &rest _)
                             (setq offered table)
                             (seq-find (lambda (c) (string-prefix-p "project" c)) table))))
                  (pai-memory-review-move))
                ;; the other memories are offered, with their titles
                (should (= 2 (length offered)))
                (should (seq-find (lambda (c) (string-match-p "\\`user +About the user" c)) offered)))
              (should (equal (plist-get (pai-memory-proposal-load (plist-get p :id)) :target) "project"))
              (should (string-match-p "add project: Project uses vui" (buffer-string))))
          (kill-buffer buf))))))

(ert-deftest pai-memory-review-m-refuses-non-add-proposals ()
  (pai-memory-mv--with-home dir
    (pai-memory-mv--add 'user "Likes tea")
    (pai-memory-mv--propose :kind "memory-remove" :target "user" :old "tea" :rationale "changed")
    (let ((buf (pai-memory-review)))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (search-forward "remove user: tea")
            (should-error (pai-memory-review-move) :type 'user-error))
        (kill-buffer buf)))))

(ert-deftest pai-memory-browse-m-moves-the-entry ()
  (pai-memory-test--with-owner owner dir
    (pai-memory-apply-change '(:action add :target memory :content "Build with make twice"))
    (let ((buf (pai-memory-browse owner)))
      (unwind-protect
          (with-current-buffer buf
            (should (eq (lookup-key pai-memory-browse-mode-map "m") #'pai-memory-browse-move))
            (should (string-match-p "m move" (buffer-string)))
            (goto-char (point-min))
            (search-forward "Build with make twice")
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt table &rest _)
                         (seq-find (lambda (c) (string-prefix-p "project" c)) table))))
              (pai-memory-browse-move))
            (should-not (pai-memory-read 'memory dir))
            (should (equal (pai-memory-read 'project dir) '("Build with make twice")))
            ;; point stays on the moved entry, now under project memory
            (let ((item (get-text-property (point) 'pai-memory-item)))
              (should (eq (plist-get item :target) 'project))
              (should (equal (plist-get item :entry) "Build with make twice")))
            ;; only memory entries move
            (goto-char (point-min))
            (should-error (pai-memory-browse-move) :type 'user-error))
        (kill-buffer buf)))))

(provide 'pai-memory-move-test)
;;; pai-memory-move-test.el ends here
