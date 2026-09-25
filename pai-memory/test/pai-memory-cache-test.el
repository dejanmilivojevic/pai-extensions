;;; pai-memory-cache-test.el --- Caches that keep long sessions cheap -*- lexical-binding: t; -*-

;;; Commentary:

;; The memory widget and the observer run after every turn.  Re-reading
;; every proposal file and re-converting every session entry each time made
;; long sessions pause for garbage collection every few seconds; these
;; caches must still see every change.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pai)
(require 'pai-faux)
(require 'pai-memory)
(require 'pai-memory-helpers)

(defmacro pai-memory-ct--with-home (dir &rest body)
  "Run BODY with a temp pai home DIR and a project as `default-directory'."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "pai-cache" t)))
          (pai-directory ,dir)
          (default-directory (file-name-as-directory (expand-file-name "proj" ,dir)))
          (pai-settings--global nil)
          (pai-settings--project nil)
          (pai-memory--proposals-cache nil))
     (make-directory default-directory t)
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defun pai-memory-ct--propose (content)
  (pai-memory-add-proposal
   (pai-memory-make-proposal :kind "memory-add" :target "user" :content content
                             :rationale "r" :cwd default-directory :session-id "S")))

(ert-deftest pai-memory-proposals-cache-sees-every-change ()
  (pai-memory-ct--with-home dir
    (let ((reads 0))
      (cl-letf* ((orig (symbol-function 'pai-json-decode))
                 ((symbol-function 'pai-json-decode)
                  (lambda (s) (cl-incf reads) (funcall orig s))))
        (let ((a (pai-memory-ct--propose "likes tea")))
          (pai-memory-ct--propose "likes coffee")
          (should (= (pai-memory-pending-count) 2))
          (setq reads 0)
          ;; unchanged directory: no file is read again
          (should (= (pai-memory-pending-count) 2))
          (should (= reads 0))
          ;; a status change (rewritten file) is seen
          (pai-memory-proposal-reject (plist-get a :id) "no")
          (should (= (pai-memory-pending-count) 1))
          (should (= (length (pai-memory-proposals "rejected")) 1))
          ;; a new proposal is seen
          (pai-memory-ct--propose "likes water")
          (should (= (pai-memory-pending-count) 2))
          ;; a deleted file is seen
          (delete-file (pai-memory-proposal-file (plist-get a :id)))
          (should-not (pai-memory-proposals "rejected")))))))

(ert-deftest pai-memory-entry-message-is-memoized-per-entry ()
  (pai-memory-test--with-session s dir
    (let* ((e (car (pai-memory-test--turns s 1)))
           (m1 (pai-memory-entry-message e))
           (m2 (pai-memory-entry-message e)))
      (should m1)
      (should (eq m1 m2))
      ;; and its token estimate is cached with it
      (should (= (pai-memory-entry-tokens e) (pai-estimate-tokens m1)))
      ;; a replaced message is converted again
      (let ((e2 (plist-put (copy-sequence e) :message
                           (list :role "user" :content "changed" :timestamp 1))))
        (should (equal (pai-content-text (pai-message-content (pai-memory-entry-message e2)))
                       "changed"))))))

(provide 'pai-memory-cache-test)
;;; pai-memory-cache-test.el ends here
