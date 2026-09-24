;;; pai-prompt-snippets.el --- Mix-and-match prompt rules toggled per message -*- lexical-binding: t; -*-

;; An Emacs port of the `prompt-snippets' pi extension
;; (https://github.com/amosblomqvist/pi-config/tree/main/extensions/prompt-snippets).

;;; Commentary:

;; Mix-and-match single-purpose prompt rules that are prepended or appended to
;; your message when you send it.  Unlike skills, each snippet is a tiny,
;; standalone instruction -- toggle exactly the ones you want per message.
;;
;;   * Press `C-c s' or run `/snippets' to open the toggle menu.  It is a
;;     `vui' buffer: checkboxes for each snippet, grouped into a PREPEND and an
;;     APPEND section, with a preview line for each.  TAB navigates, SPC/RET
;;     toggles, `p' previews the snippet at point in its own buffer, C-c C-c
;;     applies, C-c C-k / `q' cancels.
;;   * The active toggles show up as a widget in the mode line: `↑ prepend: …'
;;     (accent) and `↓ append: …' (warning), mirroring upstream.
;;   * When you send a message, the active snippet bodies are merged into the
;;     text: prepend group (sorted by `order') -> your text -> append group
;;     (sorted by `order'), separated by blank lines.
;;   * Toggles reset to all-off after each send and at session start.
;;
;; Snippets live in `snippets/' next to this file, in ~/.pai/snippets/ (where
;; pai-memory writes learned snippets), and optionally in `.pai/snippets/'
;; under the project and any directory in `pai-prompt-snippets-directories'.  Each is a markdown file with
;; frontmatter:
;;
;;   ---
;;   name: Concise
;;   description: Keep answers short and to the point
;;   placement: prepend
;;   order: 10
;;   ---
;;   Keep your response concise.  Skip preamble and unnecessary explanation.
;;
;; | Field       | Required | Notes                                             |
;; |-------------|----------|---------------------------------------------------|
;; | name        | no       | Display name; defaults to filename without `.md'  |
;; | description | no       | Shown next to the name in the menu                |
;; | placement   | no       | `prepend' or `append' (default: `append')         |
;; | order       | no       | Sorts within the group (default 9999, ties: name) |
;;
;; Files are re-scanned every time the menu opens and every time a message is
;; sent, so edits take effect immediately -- no `/reload' needed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'pai)
(require 'pai-core)
(require 'pai-ext)
(require 'vui)
(require 'vui-components)

(declare-function pai-settings-ui-register-section "pai-settings-ui")
(declare-function pai-settings-ui-register-subsection "pai-settings-ui")
(declare-function pai-settings-ui-register-item "pai-settings-ui")
(declare-function pai--set-widget "pai-ui")
(declare-function pai--render-note "pai-ui")

;;;; Options

(defgroup pai-prompt-snippets nil
  "Mix-and-match prompt rules toggled per message."
  :group 'pai)

(defface pai-prompt-snippets-prepend-face
  '((t :inherit font-lock-keyword-face))
  "Face for the prepend group in the active-snippets widget."
  :group 'pai-prompt-snippets)

(defface pai-prompt-snippets-append-face
  '((t :inherit warning))
  "Face for the append group in the active-snippets widget."
  :group 'pai-prompt-snippets)

(defconst pai-prompt-snippets--bundled-dir
  (expand-file-name "snippets"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "The `snippets/' directory bundled alongside this file.")

(defcustom pai-prompt-snippets-directories nil
  "Extra directories scanned for snippet `.md' files.
Searched in addition to the bundled `snippets/' directory, ~/.pai/snippets/
and the project's `.pai/snippets/'.  Later directories win when two files share a name, so a
project or user override shadows a bundled snippet of the same filename."
  :type '(repeat directory)
  :group 'pai-prompt-snippets)

(defconst pai-prompt-snippets--widget-key 'prompt-snippets
  "The `pai--set-widget' key used for the active-snippets indicator.")

;;;; Snippet model

(cl-defstruct (pai-prompt-snippet (:constructor pai-prompt-snippet--create))
  "One prompt snippet loaded from disk."
  id                                    ; filename, e.g. "concise.md"
  name description placement order body
  path                                  ; absolute file name
  origin)                               ; frontmatter `origin', e.g. "learned"

(defun pai-prompt-snippets--parse (filename raw)
  "Parse RAW markdown from FILENAME into a `pai-prompt-snippet', or nil.
Requires a `---' frontmatter block followed by a non-empty body."
  (when (string-match "\\`---\r?\n\\(\\(?:.\\|\n\\)*?\\)\r?\n---\r?\n?\\(\\(?:.\\|\n\\)*\\)\\'"
                      raw)
    (let ((meta-block (match-string 1 raw))
          (body (string-trim (match-string 2 raw)))
          (meta '()))
      (unless (string-empty-p body)
        (dolist (line (split-string meta-block "\r?\n"))
          (when (string-match "\\`\\([A-Za-z][[:alnum:]_-]*\\)[ \t]*:[ \t]*\\(.*\\)\\'" line)
            (let ((key (downcase (match-string 1 line)))
                  (val (string-trim (match-string 2 line))))
              ;; Strip a single pair of surrounding quotes, like upstream.
              (when (and (>= (length val) 2)
                         (memq (aref val 0) '(?\" ?'))
                         (eq (aref val 0) (aref val (1- (length val)))))
                (setq val (substring val 1 (1- (length val)))))
              (push (cons key val) meta))))
        (let* ((order-raw (cdr (assoc "order" meta)))
               (order (and order-raw (string-match-p "\\`[+-]?[0-9]+" order-raw)
                           (string-to-number order-raw)))
               (name (let ((n (cdr (assoc "name" meta))))
                       (if (and n (not (string-empty-p n))) n
                         (replace-regexp-in-string "\\.md\\'" "" filename))))
               (placement (if (equal (cdr (assoc "placement" meta)) "prepend")
                              'prepend 'append)))
          (pai-prompt-snippet--create
           :id filename
           :name name
           :description (or (cdr (assoc "description" meta)) "")
           :placement placement
           :order (if (integerp order) order 9999)
           :body body
           :origin (cdr (assoc "origin" meta))))))))

(defun pai-prompt-snippets-user-dir ()
  "Return the user's own snippet directory, ~/.pai/snippets/.
Learned snippets (see pai-memory) are written here."
  (file-name-as-directory (expand-file-name "snippets" pai-directory)))

(defun pai-prompt-snippets--dirs ()
  "Return the directories to scan for snippets, in precedence order.
Earlier entries are overridden by later ones when filenames collide."
  (delete-dups
   (delq nil
         (append
          (list pai-prompt-snippets--bundled-dir
                (pai-prompt-snippets-user-dir))
          pai-prompt-snippets-directories
          (list (expand-file-name ".pai/snippets/" default-directory))))))

(defun pai-prompt-snippets--load ()
  "Load every snippet from disk, deduped by filename and sorted.
Later directories win on filename collisions.  The result lists the prepend
group first, then the append group, each ordered by (order, name)."
  (let ((by-id (make-hash-table :test 'equal)))
    (dolist (dir (pai-prompt-snippets--dirs))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.md\\'" t))
          (when (file-regular-p file)
            (condition-case nil
                (let* ((filename (file-name-nondirectory file))
                       (raw (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string)))
                       (snippet (pai-prompt-snippets--parse filename raw)))
                  (when snippet
                    (setf (pai-prompt-snippet-path snippet) file)
                    (puthash filename snippet by-id)))
              (error nil))))))          ; skip unreadable files
    (let* ((snippets (hash-table-values by-id))
           (by-order (lambda (a b)
                       (let ((oa (pai-prompt-snippet-order a))
                             (ob (pai-prompt-snippet-order b)))
                         (if (/= oa ob) (< oa ob)
                           (string< (pai-prompt-snippet-name a)
                                    (pai-prompt-snippet-name b)))))))
      (append (sort (seq-filter (lambda (s) (eq (pai-prompt-snippet-placement s) 'prepend))
                                snippets)
                    by-order)
              (sort (seq-filter (lambda (s) (eq (pai-prompt-snippet-placement s) 'append))
                                snippets)
                    by-order)))))

(defun pai-prompt-snippets-list (&optional cwd)
  "Return the snippets visible in project CWD as plists, grouped and ordered.
Each is (:id :name :description :placement :order :path :origin); this is
the public view used by pai-memory to learn and patch snippets."
  (let ((default-directory (or cwd default-directory)))
    (mapcar (lambda (s)
              (list :id (pai-prompt-snippet-id s)
                    :name (pai-prompt-snippet-name s)
                    :description (pai-prompt-snippet-description s)
                    :placement (symbol-name (pai-prompt-snippet-placement s))
                    :order (pai-prompt-snippet-order s)
                    :path (pai-prompt-snippet-path s)
                    :origin (pai-prompt-snippet-origin s)))
            (pai-prompt-snippets--load))))

(defun pai-prompt-snippets-directories-for (&optional cwd)
  "Return the snippet directories for project CWD, in precedence order."
  (let ((default-directory (or cwd default-directory)))
    (pai-prompt-snippets--dirs)))

;;;; Per-session state
;; State is scoped to the pai session buffer, so toggles never leak between
;; sessions and reset naturally when a buffer starts fresh.

(defvar-local pai-prompt-snippets--enabled nil
  "List of snippet ids currently toggled on in this session buffer.")

(defun pai-prompt-snippets--active (snippets)
  "Return the members of SNIPPETS that are enabled in the current buffer."
  (seq-filter (lambda (s) (member (pai-prompt-snippet-id s)
                                  pai-prompt-snippets--enabled))
              snippets))

;;;; Active-snippets widget

(defun pai-prompt-snippets--widget-line (snippets)
  "Return the active-snippets widget string for SNIPPETS, or nil when none."
  (let* ((active (pai-prompt-snippets--active snippets))
         (prepends (seq-filter (lambda (s) (eq (pai-prompt-snippet-placement s) 'prepend))
                               active))
         (appends (seq-filter (lambda (s) (eq (pai-prompt-snippet-placement s) 'append))
                              active))
         (parts nil))
    (when prepends
      (push (propertize
             (concat "↑ prepend: "
                     (mapconcat #'pai-prompt-snippet-name prepends " · "))
             'face 'pai-prompt-snippets-prepend-face)
            parts))
    (when appends
      (push (propertize
             (concat "↓ append: "
                     (mapconcat #'pai-prompt-snippet-name appends " · "))
             'face 'pai-prompt-snippets-append-face)
            parts))
    (when parts (string-join (nreverse parts) "  "))))

(defun pai-prompt-snippets--update-widget (&optional snippets)
  "Refresh the active-snippets widget in the current pai buffer.
SNIPPETS defaults to a fresh load from disk."
  (when (and (derived-mode-p 'pai-mode) (fboundp 'pai--set-widget))
    (let ((snippets (or snippets (pai-prompt-snippets--load))))
      (pai--set-widget pai-prompt-snippets--widget-key
                       (pai-prompt-snippets--widget-line snippets)))))

;;;; Input transform

(defun pai-prompt-snippets--block (snippet)
  "Return SNIPPET's body wrapped in a <prompt-snippet> block.
The tag tells memory (see `pai-memory-strip-injected') that the user did
not write this text, so snippets used often are never learned as habits."
  (format "<prompt-snippet name=\"%s\">\n%s\n</prompt-snippet>"
          (replace-regexp-in-string "\"" "'" (pai-prompt-snippet-name snippet))
          (string-trim (pai-prompt-snippet-body snippet))))

(defun pai-prompt-snippets--merge (text active)
  "Return TEXT with the ACTIVE prepend/append snippet bodies merged in.
ACTIVE is the list of snippets to apply, already loaded (and thus grouped and
ordered).  Prepend bodies come first, then TEXT, then append bodies, each
separated by a blank line and wrapped in a <prompt-snippet> block."
  (let ((prepends (mapcar #'pai-prompt-snippets--block
                          (seq-filter (lambda (s)
                                        (eq (pai-prompt-snippet-placement s) 'prepend))
                                      active)))
        (appends (mapcar #'pai-prompt-snippets--block
                         (seq-filter (lambda (s)
                                       (eq (pai-prompt-snippet-placement s) 'append))
                                     active))))
    (string-join (append prepends (list text) appends) "\n\n")))

(defun pai-prompt-snippets--on-input (event ctx)
  "Merge active snippets into the input on EVENT, resetting toggles.
Runs in the pai buffer named by CTX; returns a transform action or nil."
  (let ((buf (plist-get ctx :buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when pai-prompt-snippets--enabled
          (let* ((snippets (pai-prompt-snippets--load))
                 (active (pai-prompt-snippets--active snippets))
                 (text (plist-get event :text)))
            ;; Toggles reset after each send, whether or not any survived.
            (setq pai-prompt-snippets--enabled nil)
            (pai-prompt-snippets--update-widget snippets)
            (when active
              (list :action 'transform
                    :text (pai-prompt-snippets--merge text active)))))))))

;;;; Toggle menu (vui)

(defvar-local pai-prompt-snippets--menu-source nil
  "The pai session buffer a menu buffer applies its toggles back to.")
(defvar-local pai-prompt-snippets--menu-snippets nil
  "The snippets a menu buffer is offering.")
(defvar-local pai-prompt-snippets--menu-working nil
  "Working set of enabled ids in a menu buffer, committed on apply.")
(defvar-local pai-prompt-snippets--menu-instance nil
  "The mounted `vui' instance of a menu buffer.")

(defun pai-prompt-snippets--menu-refresh ()
  "Re-render the current menu buffer from its working state."
  (when pai-prompt-snippets--menu-instance
    (vui-rerender pai-prompt-snippets--menu-instance)))

(defun pai-prompt-snippets--menu-toggle (id)
  "Toggle snippet ID in the current menu buffer's working set."
  (setq pai-prompt-snippets--menu-working
        (if (member id pai-prompt-snippets--menu-working)
            (remove id pai-prompt-snippets--menu-working)
          (cons id pai-prompt-snippets--menu-working)))
  (pai-prompt-snippets--menu-refresh))

(defun pai-prompt-snippets--menu-apply ()
  "Commit the menu's working set back to its source session and close it."
  (interactive)
  (let ((source pai-prompt-snippets--menu-source)
        (working pai-prompt-snippets--menu-working)
        (snippets pai-prompt-snippets--menu-snippets)
        (buffer (current-buffer)))
    (when (buffer-live-p source)
      (with-current-buffer source
        ;; Keep only ids that still exist, in the loaded (grouped) order.
        (setq pai-prompt-snippets--enabled
              (seq-filter (lambda (s) (member (pai-prompt-snippet-id s) working))
                          snippets))
        (setq pai-prompt-snippets--enabled
              (mapcar #'pai-prompt-snippet-id pai-prompt-snippets--enabled))
        (pai-prompt-snippets--update-widget snippets)))
    (pai-prompt-snippets--menu-quit buffer source)))

(defun pai-prompt-snippets--menu-cancel ()
  "Close the menu buffer without committing its working set."
  (interactive)
  (pai-prompt-snippets--menu-quit (current-buffer)
                                  pai-prompt-snippets--menu-source))

(defun pai-prompt-snippets--menu-quit (buffer source)
  "Kill menu BUFFER, restoring its window, and reselect SOURCE if visible."
  (when (buffer-live-p buffer)
    (dolist (window (get-buffer-window-list buffer nil t))
      (ignore-errors (quit-restore-window window 'bury)))
    (kill-buffer buffer))
  (when (buffer-live-p source)
    (when-let ((window (get-buffer-window source 0)))
      (ignore-errors (select-window window)))))

(defun pai-prompt-snippets--menu-preview ()
  "Show the snippet at point in a read-only preview buffer."
  (interactive)
  (let ((snippet (get-text-property (point) 'pai-prompt-snippet)))
    (unless snippet
      (save-excursion
        (beginning-of-line)
        (setq snippet (get-text-property (point) 'pai-prompt-snippet))))
    (if (not snippet)
        (message "No snippet at point")
      (let ((buffer (get-buffer-create
                     (format "*snippet: %s*" (pai-prompt-snippet-name snippet)))))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize (pai-prompt-snippet-name snippet) 'face 'bold) "\n")
            (insert (propertize
                     (format "%s · order %d · %s"
                             (pai-prompt-snippet-placement snippet)
                             (pai-prompt-snippet-order snippet)
                             (pai-prompt-snippet-id snippet))
                     'face 'shadow)
                    "\n")
            (when (and (pai-prompt-snippet-description snippet)
                       (not (string-empty-p (pai-prompt-snippet-description snippet))))
              (insert (propertize (pai-prompt-snippet-description snippet) 'face 'shadow)
                      "\n"))
            (insert (propertize (make-string 40 ?─) 'face 'shadow) "\n\n")
            (insert (pai-prompt-snippet-body snippet) "\n"))
          (goto-char (point-min))
          (view-mode 1))
        (display-buffer buffer '((display-buffer-reuse-window
                                  display-buffer-below-selected
                                  display-buffer-pop-up-window)
                                 (window-height . fit-window-to-buffer)))))))

(defun pai-prompt-snippets--menu-rows (snippets placement)
  "Return the vui checkbox rows for the PLACEMENT group of SNIPPETS."
  (cl-loop for s in snippets
           when (eq (pai-prompt-snippet-placement s) placement)
           append
           (let* ((id (pai-prompt-snippet-id s))
                  (desc (pai-prompt-snippet-description s))
                  (label (concat (pai-prompt-snippet-name s)
                                 (if (and desc (not (string-empty-p desc)))
                                     (concat " — " desc) ""))))
             (list
              (vui-checkbox
               :key (concat "snippet-" id)
               :checked (and (member id pai-prompt-snippets--menu-working) t)
               :label (propertize label 'pai-prompt-snippet s)
               :on-change (lambda (_v) (pai-prompt-snippets--menu-toggle id)))))))

(vui-defcomponent pai-prompt-snippets-menu ()
  "Render the prompt-snippets toggle menu from the buffer's working state."
  :render
  (let* ((snippets pai-prompt-snippets--menu-snippets)
         (prepends (seq-filter (lambda (s) (eq (pai-prompt-snippet-placement s) 'prepend))
                               snippets))
         (appends (seq-filter (lambda (s) (eq (pai-prompt-snippet-placement s) 'append))
                              snippets)))
    (vui-vstack
     :spacing 1
     (vui-heading-1 "Prompt snippets")
     (apply #'vui-vstack
            (append
             (list (vui-muted "↑ PREPEND — added before your message"))
             (or (pai-prompt-snippets--menu-rows prepends 'prepend)
                 (list (vui-muted "  (none)")))
             (list (vui-muted "↓ APPEND — added after your message"))
             (or (pai-prompt-snippets--menu-rows appends 'append)
                 (list (vui-muted "  (none)")))))
     (vui-hstack
      (vui-button "Apply" :key "apply"
                  :on-click #'pai-prompt-snippets--menu-apply)
      (vui-button "Cancel" :key "cancel"
                  :on-click #'pai-prompt-snippets--menu-cancel))
     (vui-muted
      "SPC/RET toggle · p preview · TAB move · C-c C-c apply · C-c C-k/q cancel"))))

(defvar pai-prompt-snippets-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'pai-prompt-snippets--menu-preview)
    (define-key map (kbd "q") #'pai-prompt-snippets--menu-cancel)
    (define-key map (kbd "C-c C-c") #'pai-prompt-snippets--menu-apply)
    (define-key map (kbd "C-c C-k") #'pai-prompt-snippets--menu-cancel)
    map)
  "Keymap for `pai-prompt-snippets-menu-mode'.")

(define-derived-mode pai-prompt-snippets-menu-mode vui-mode "pai-snippets"
  "Major mode for the prompt-snippets toggle menu.")

(defun pai-prompt-snippets--open-menu (ctx)
  "Open the toggle menu for the pai session named by CTX."
  (let ((source (plist-get ctx :buffer)))
    (unless (buffer-live-p source)
      (user-error "No active pai session"))
    (let ((snippets (with-current-buffer source (pai-prompt-snippets--load)))
          (enabled (with-current-buffer source
                     (copy-sequence pai-prompt-snippets--enabled))))
      (with-current-buffer source (pai-prompt-snippets--update-widget snippets))
      (if (null snippets)
          (progn
            (message "No snippets found in %s"
                     (string-join (pai-prompt-snippets--dirs) ", "))
            nil)
        (let ((buffer (generate-new-buffer "*pai snippets*")))
          (with-current-buffer buffer
            (pai-prompt-snippets-menu-mode)
            (setq pai-prompt-snippets--menu-source source
                  pai-prompt-snippets--menu-snippets snippets
                  pai-prompt-snippets--menu-working enabled)
            (let ((switch-to-buffer-obey-display-actions t)
                  (display-buffer-overriding-action
                   '((display-buffer-reuse-window display-buffer-below-selected
                      display-buffer-pop-up-window)
                     (window-height . fit-window-to-buffer))))
              (setq pai-prompt-snippets--menu-instance
                    (vui-mount (vui-component 'pai-prompt-snippets-menu)
                               (buffer-name buffer))))
            (ignore-errors (vui-goto-key "apply")))
          (let ((window (get-buffer-window buffer 0)))
            (when (window-live-p window) (select-window window)))
          buffer)))))

;;;; Registration

(defun pai-prompt-snippets--command (_args ctx)
  "Slash-command handler opening the toggle menu."
  (pai-prompt-snippets--open-menu ctx)
  nil)

(defun pai-prompt-snippets-open ()
  "Open the prompt-snippets toggle menu for the current pai buffer."
  (interactive)
  (pai-prompt-snippets--open-menu (list :buffer (current-buffer))))

(pai-register-extension
 (lambda (api)
   (pai-ext-register-command
    api "snippets"
    :description "Toggle mix-and-match prompt rules for your next message"
    :handler #'pai-prompt-snippets--command)
   ;; Upstream binds alt+s; in Emacs M-s is the search/occur prefix, so a free
   ;; pai chord is used instead (C-c C-s is already the settings menu).
   (pai-ext-register-shortcut api "C-c s" #'pai-prompt-snippets-open)
   ;; Toggles reset to all-off at session start; the widget is seeded then too.
   (pai-ext-on api 'session-start
               (lambda (_event ctx)
                 (let ((buf (plist-get ctx :buffer)))
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (setq pai-prompt-snippets--enabled nil)
                       (pai-prompt-snippets--update-widget))))))
   ;; /reload clears widgets; redraw ours.
   (pai-ext-on api 'reload
               (lambda (_event ctx)
                 (let ((buf (plist-get ctx :buffer)))
                   (when (buffer-live-p buf)
                     (with-current-buffer buf (pai-prompt-snippets--update-widget))))))
   ;; Merge active snippet bodies into the message and reset toggles on send.
   (pai-ext-on api 'input #'pai-prompt-snippets--on-input))
 "prompt-snippets")

;;;; Settings screen
;; Soft dependency: the extension works without the vui settings screen.
(with-eval-after-load 'pai-settings-ui
  (pai-settings-ui-register-section 'prompt-snippets "Prompt snippets" 48)
  (pai-settings-ui-register-subsection 'prompt-snippets 'sources "Sources" 10)
  (pai-settings-ui-register-item
   'prompt-snippets 'sources
   :key :prompt-snippets-dirs :type 'string :label "Extra snippet directories"
   :doc "Comma-separated directories scanned in addition to the bundled snippets"
   :get (lambda () (string-join pai-prompt-snippets-directories ", "))
   :set (lambda (v)
          (setq pai-prompt-snippets-directories
                (and v (seq-filter (lambda (s) (not (string-empty-p s)))
                                   (mapcar #'string-trim (split-string v "," t))))))))

(provide 'pai-prompt-snippets)
;;; pai-prompt-snippets.el ends here
