;;; pai-memory-graph.el --- Learning timeline and graph -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 F3 ("learning made visible", after Hermes' learning_graph.py).
;;
;; `/memory timeline [DAYS]' opens an Org buffer:
;;
;;   * Week 2026-W39
;;   ** 2026-09-23 Wed
;;      - 12:13 learned  user entry: "..."          (entry created)
;;      - 12:40 confirmed user entry: "..."
;;      - 13:02 skill created: deploy               (log.jsonl)
;;      - 13:05 rejected memory-add: "..." -- reason
;;   * Connections
;;   ** skill deploy
;;      - related: ...      (front-matter `related:')
;;      - similar: ...      (lexical overlap, top 4)
;;      - same session: ...
;;
;; `/memory graph' writes the same nodes and edges as Graphviz DOT to
;; ~/.pai/memory/learning-graph.dot, and renders it to SVG and shows it when
;; `dot' is installed.  No model calls.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pai-core)
(require 'pai-skills)
(require 'pai-memory-settings)
(require 'pai-memory-store)
(require 'pai-memory-entries)
(require 'pai-memory-proposals)

(declare-function pai-memory--skill-dirs "pai-memory-promote" ())
(declare-function org-mode "org" ())

;;;; Nodes

(defun pai-memory-graph-nodes (cwd)
  "Return the learning graph nodes for CWD.
Each is (:id ID :type entry|skill :label L :text T :session S :related NAMES :time T)."
  (append
   (mapcar (lambda (r)
             (list :id (plist-get r :id) :type 'entry
                   :label (format "%s: %s" (plist-get r :target)
                                  (truncate-string-to-width (replace-regexp-in-string "\n" " " (plist-get r :text))
                                                            50 nil nil "…"))
                   :text (plist-get r :text) :session (plist-get r :source_session)
                   :time (plist-get r :created)))
           (ignore-errors (pai-memory-entries-all cwd)))
   (mapcar (lambda (s)
             (let ((fm (car (pai-memory--frontmatter-alist (pai-memory--read-file (plist-get s :path))))))
               (list :id (concat "skill:" (plist-get s :name)) :type 'skill
                     :label (concat "skill " (plist-get s :name))
                     :text (concat (plist-get s :name) " " (plist-get s :description))
                     :session (or (cdr (assoc "source-session" fm)) "")
                     :origin (cdr (assoc "origin" fm))
                     :time (or (cdr (assoc "created" fm)) "")
                     :related (let ((r (cdr (assoc "related" fm))))
                                (and r (split-string r "[][, \t\"']+" t))))))
           (ignore-errors (pai-discover-skills (and (fboundp 'pai-memory--skill-dirs) (pai-memory--skill-dirs)))))))

;;;; Edges

(defun pai-memory-graph-edges (nodes)
  "Return edges (A B KIND) between NODES; KIND is related, similar or session."
  (let ((edges '())
        (words (mapcar (lambda (n) (cons (plist-get n :id) (pai-memory--entry-words (plist-get n :text)))) nodes)))
    ;; explicit related: links
    (dolist (n nodes)
      (dolist (r (plist-get n :related))
        (let ((other (concat "skill:" r)))
          (when (seq-find (lambda (m) (equal (plist-get m :id) other)) nodes)
            (push (list (plist-get n :id) other 'related) edges)))))
    ;; lexical overlap: each node's top 4 neighbours above a floor
    (dolist (n nodes)
      (let* ((mine (alist-get (plist-get n :id) words nil nil #'equal))
             (scored (delq nil
                           (mapcar (lambda (m)
                                     (unless (equal (plist-get m :id) (plist-get n :id))
                                       (let* ((theirs (alist-get (plist-get m :id) words nil nil #'equal))
                                              (inter (length (seq-intersection mine theirs)))
                                              (union (length (delete-dups (append mine theirs))))
                                              (score (if (> union 0) (/ (float inter) union) 0)))
                                         (and (>= score 0.2) (cons (plist-get m :id) score)))))
                                   nodes))))
        (dolist (c (seq-take (sort scored (lambda (a b) (> (cdr a) (cdr b)))) 4))
          (push (list (plist-get n :id) (car c) 'similar) edges))))
    ;; shared source sessions
    (let ((by-session (make-hash-table :test #'equal)))
      (dolist (n nodes)
        (let ((s (plist-get n :session)))
          (when (and s (not (string-empty-p s)) (not (equal s "unknown")))
            (push (plist-get n :id) (gethash s by-session)))))
      (maphash (lambda (_s ids)
                 (let ((ids (reverse ids)))
                   (while ids
                     (let ((a (pop ids)))
                       (dolist (b ids) (push (list a b 'session) edges))))))
               by-session))
    ;; one edge per pair, the strongest kind first
    (let ((seen (make-hash-table :test #'equal)) (out '()))
      (dolist (kind '(related session similar))
        (dolist (e (reverse edges))
          (when (eq (nth 2 e) kind)
            (let ((key (sort (list (nth 0 e) (nth 1 e)) #'string<)))
              (unless (gethash key seen)
                (puthash key t seen)
                (push e out))))))
      (nreverse out))))

;;;; Timeline

(defun pai-memory--time-key (time) (and (stringp time) (>= (length time) 16) (substring time 0 16)))

(defun pai-memory-timeline-events (cwd &optional days)
  "Return timeline events for CWD within DAYS (all when nil), oldest first.
Each is (TIME TEXT); TIME is \"YYYY-MM-DDTHH:MM\"."
  (let* ((cutoff (and days (format-time-string "%FT%R" (time-subtract nil (days-to-time days)))))
         (events '())
         (add (lambda (time text)
                (let ((k (pai-memory--time-key time)))
                  (when (and k (or (null cutoff) (string< cutoff k)))
                    (push (list k text) events)))))
         (short (lambda (s) (truncate-string-to-width (replace-regexp-in-string "[ \t\n]+" " " (or s "")) 70 nil nil "…"))))
    ;; entries: learned, confirmed, removed (all records, live or not)
    (ignore-errors (pai-memory-entries-all cwd))
    (dolist (r (pai-memory--meta-load))
      (funcall add (plist-get r :created)
               (format "%s %s entry: %s"
                       (pcase (plist-get r :origin)
                         ("proposal" "learned") ("memory-tool" "saved") ("manual" "written")
                         (_ "remembered"))
                       (plist-get r :target) (funcall short (plist-get r :text))))
      (dolist (c (append (plist-get r :confirmed) nil))
        (funcall add (plist-get c :time) (format "confirmed %s entry: %s" (plist-get r :target)
                                                 (funcall short (plist-get r :text)))))
      (let ((rm (plist-get r :removed)))
        (when (and rm (not (string-empty-p rm)))
          (funcall add rm (format "removed %s entry: %s" (plist-get r :target) (funcall short (plist-get r :text)))))))
    ;; skills and topics from the change log
    (dolist (rec (pai-memory-log-read))
      (let ((action (plist-get rec :action)) (file (or (plist-get rec :file) "")))
        (cond
         ((member action '("skill-create" "skill-patch"))
          (funcall add (plist-get rec :time)
                   (format "skill %s: %s" (if (equal action "skill-create") "created" "improved")
                           (or (and (not (string-empty-p file))
                                    (file-name-nondirectory (directory-file-name (file-name-directory file))))
                               (let ((op (seq-find (lambda (o) (string-suffix-p "SKILL.md" (or (plist-get o :file) "")))
                                                   (append (plist-get rec :group) nil))))
                                 (and op (file-name-nondirectory (directory-file-name (file-name-directory (plist-get op :file))))))
                               "?"))))
         ((member action '("snippet-create" "snippet-patch"))
          (funcall add (plist-get rec :time)
                   (format "prompt snippet %s: %s" (if (equal action "snippet-create") "created" "improved")
                           (file-name-base file))))
         ((member action '("skill-merge" "skill-archive"))
          (funcall add (plist-get rec :time) (format "skills %s" (if (equal action "skill-merge") "merged" "archived"))))
         ((equal action "topic-conflict")
          (funcall add (plist-get rec :time) (format "topic conflict resolved: %s" (file-name-nondirectory file))))
         ((equal action "forget")
          (funcall add (plist-get rec :time) "forgot text on request"))
         ((equal action "move")
          (funcall add (plist-get rec :time)
                   (format "moved entry %s → %s: %s" (plist-get rec :from) (plist-get rec :target)
                           (funcall short (or (plist-get rec :content) "")))))
         ((equal action "undo")
          (funcall add (plist-get rec :time) (format "undid %s" (plist-get rec :undoes)))))))
    ;; rejected proposals (the loop learning what not to learn)
    (dolist (p (ignore-errors (pai-memory-proposals "rejected")))
      (funcall add (or (plist-get p :decided) (plist-get p :created))
               (format "rejected %s: %s%s" (plist-get p :kind)
                       (funcall short (or (plist-get p :name) (plist-get p :content)))
                       (let ((r (plist-get p :reason)))
                         (if (and r (not (string-empty-p r))) (format " — %s" r) "")))))
    (sort (delete-dups events) (lambda (a b) (string< (car a) (car b))))))

(defun pai-memory--iso-week (day)
  "Return the ISO week label (YYYY-Www) of DAY, a YYYY-MM-DD string."
  (format-time-string "%G-W%V" (date-to-time (concat day "T12:00:00"))))

(defun pai-memory-timeline-org (cwd &optional days)
  "Return the Org text of the learning timeline and connections for CWD."
  (let* ((events (pai-memory-timeline-events cwd days))
         (nodes (pai-memory-graph-nodes cwd))
         (edges (pai-memory-graph-edges nodes))
         (week nil) (day nil) (out '()))
    (push (format "#+TITLE: Learning timeline%s\n" (if days (format " (last %d days)" days) "")) out)
    (if (null events)
        (push "\nNothing learned yet.\n" out)
      (dolist (e events)
        (let* ((d (substring (car e) 0 10)) (w (pai-memory--iso-week d)))
          (unless (equal w week) (setq week w) (push (format "* Week %s\n" w) out))
          (unless (equal d day)
            (setq day d)
            (push (format "** %s %s\n" d (format-time-string "%a" (date-to-time (concat d "T12:00:00")))) out))
          (push (format "   - %s %s\n" (substring (car e) 11 16) (cadr e)) out))))
    (when edges
      (push "* Connections\n" out)
      (dolist (n nodes)
        (let ((mine (seq-filter (lambda (e) (member (plist-get n :id) (list (nth 0 e) (nth 1 e)))) edges)))
          (when mine
            (push (format "** %s\n" (plist-get n :label)) out)
            (dolist (e mine)
              (let* ((other-id (if (equal (nth 0 e) (plist-get n :id)) (nth 1 e) (nth 0 e)))
                     (other (seq-find (lambda (m) (equal (plist-get m :id) other-id)) nodes)))
                (push (format "   - %s: %s\n" (pcase (nth 2 e) ('related "related") ('similar "similar")
                                                     ('session "same session"))
                              (plist-get other :label))
                      out)))))))
    (apply #'concat (nreverse out))))

(defun pai-memory-timeline (&optional days cwd)
  "Show the learning timeline for CWD (last DAYS days); return the buffer."
  (let ((text (pai-memory-timeline-org (or cwd default-directory) days))
        (buf (get-buffer-create "*pai learning*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (when (require 'org nil t) (org-mode))
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (unless noninteractive (display-buffer buf))
    buf))

;;;; Graphviz

(defun pai-memory--dot-quote (s)
  (concat "\"" (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" (or s "")) "\""))

(defun pai-memory-graph-dot (cwd)
  "Return the learning graph of CWD as Graphviz DOT text."
  (let* ((nodes (pai-memory-graph-nodes cwd))
         (edges (pai-memory-graph-edges nodes)))
    (concat
     "graph learning {\n  graph [overlap=false, splines=true, fontname=\"sans\"];\n"
     "  node [fontname=\"sans\", fontsize=10, style=filled];\n"
     (mapconcat (lambda (n)
                  (format "  %s [label=%s, shape=%s, fillcolor=%s];"
                          (pai-memory--dot-quote (plist-get n :id)) (pai-memory--dot-quote (plist-get n :label))
                          (if (eq (plist-get n :type) 'skill) "box" "ellipse")
                          (if (eq (plist-get n :type) 'skill)
                              (if (equal (plist-get n :origin) "learned") "\"#cde8c4\"" "\"#e0e0e0\"")
                            "\"#cfe0f5\"")))
                nodes "\n")
     "\n"
     (mapconcat (lambda (e)
                  (format "  %s -- %s [style=%s];" (pai-memory--dot-quote (nth 0 e)) (pai-memory--dot-quote (nth 1 e))
                          (pcase (nth 2 e) ('related "bold") ('session "dashed") (_ "dotted"))))
                edges "\n")
     "\n}\n")))

(defvar pai-memory-graph-render-timeout 30
  "Seconds before a running Graphviz render of the learning graph is killed.")

(defun pai-memory--graph-show (svg)
  "Show the rendered learning graph SVG (not in batch mode)."
  (unless noninteractive
    (if (image-type-available-p 'svg)
        (find-file-other-window svg)
      (browse-url-of-file svg))))

(defun pai-memory-graph (&optional cwd on-done)
  "Write the learning graph for CWD and render it with Graphviz when present.
Rendering runs `dot' asynchronously, so the UI never waits for it; the
SVG is shown when it is ready.  ON-DONE, when given, is called with the
final message (the rendered SVG, or why there is none).  Return the
immediate message."
  (let* ((cwd (or cwd default-directory))
         (dot-file (pai-memory-dir "learning-graph.dot"))
         (svg (pai-memory-dir "learning-graph.svg"))
         (nodes (length (pai-memory-graph-nodes cwd)))
         (finish (lambda (msg) (message "%s" msg) (when on-done (funcall on-done msg)))))
    (pai-memory--write-file dot-file (pai-memory-graph-dot cwd))
    (if (not (executable-find "dot"))
        (let ((msg (format "Wrote %s (%d nodes); install Graphviz (dot) to render it, or see /memory timeline"
                           (abbreviate-file-name dot-file) nodes)))
          (when on-done (funcall on-done msg))
          msg)
      (let* ((proc (make-process
                    :name "pai-memory-graph" :noquery t :connection-type 'pipe
                    :command (list "dot" "-Tsvg" "-Kneato" "-o" svg dot-file)
                    :sentinel
                    (lambda (p _event)
                      (unless (process-live-p p)
                        (let ((timer (process-get p 'timer)))
                          (when (timerp timer) (cancel-timer timer)))
                        (if (and (eq (process-status p) 'exit) (eq 0 (process-exit-status p)))
                            (progn (pai-memory--graph-show svg)
                                   (funcall finish (format "Learning graph: %d nodes, %s"
                                                           nodes (abbreviate-file-name svg))))
                          (funcall finish (format "dot failed; the graph source is in %s"
                                                  (abbreviate-file-name dot-file))))))))
             (timer (run-at-time pai-memory-graph-render-timeout nil
                                 (lambda () (when (process-live-p proc) (delete-process proc))))))
        (process-put proc 'timer timer)
        (format "Rendering the learning graph (%d nodes) with Graphviz…" nodes)))))

(provide 'pai-memory-graph)
;;; pai-memory-graph.el ends here
