;;; pai-web-bus.el --- Browser clients, event queues, long polling, blobs -*- lexical-binding: t; -*-

;;; Commentary:

;; Every open browser page is a client with an event queue.  Events are
;; plists broadcast to all clients (`pai-web-bus-broadcast'); a page fetches
;; them by long polling `/api/poll', acknowledging what it processed, so a
;; response is only ever written right after the page asked for it (see
;; pai-web-http.el for why that matters).  Each poll answer is at most
;; `pai-web-bus-poll-bytes' of events; an event bigger than that, and any
;; JSON answer bigger than `pai-web-bus-inline-bytes', becomes a "blob"
;; that the page downloads piece by piece from `/api/blob'.
;;
;; A client that stops polling stops costing anything: its queue is capped
;; (`pai-web-bus-queue-limit'); past that it is replaced by one `resync'
;; event, after which the page reloads its state.  Clients that have not
;; polled for `pai-web-bus-client-ttl' seconds are forgotten.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pai-web-util)
(require 'pai-web-http)

(defconst pai-web-bus-poll-bytes (* 16 1024)
  "Most bytes of events sent in one poll answer.")

(defconst pai-web-bus-inline-bytes (* 24 1024)
  "JSON answers above this size are served as a blob.")

(defconst pai-web-bus-blob-chunk (* 24 1024)
  "Bytes per blob download request.")

(defconst pai-web-bus-event-bytes (* 12 1024)
  "Events above this size are queued as a reference to a blob.")

(defconst pai-web-bus-queue-limit (* 4 1024 1024)
  "Bytes of unacknowledged events kept per client before a resync.")

(defconst pai-web-bus-poll-timeout 20
  "Seconds a poll is held open when there is nothing to send.")

(defconst pai-web-bus-client-ttl 90
  "Seconds after its last poll that a client is forgotten.")

(defconst pai-web-bus-blob-ttl 300
  "Seconds a blob stays downloadable.")

(cl-defstruct (pai-web-client (:constructor pai-web-client--create))
  "An open browser page."
  id session (last (float-time))
  (queue nil)                           ; ((SEQ . JSON) ...), newest first
  (bytes 0)
  poll poll-since                       ; held poll request and when
  view                                  ; instance id shown
  buffers)                              ; remote buffer ids shown

(defvar pai-web-bus--clients (make-hash-table :test 'equal)
  "Map of client id to `pai-web-client'.")

(defvar pai-web-bus--seq 0 "Sequence number of the last event.")

(defvar pai-web-bus--blobs (make-hash-table :test 'equal)
  "Map of blob id to (BYTES . EXPIRY).")

(defvar pai-web-bus--flush-timer nil "Pending flush of queued events.")

(defun pai-web-bus-reset ()
  "Forget every client and blob (the server stopped)."
  (maphash (lambda (_id c)
             (when-let ((req (pai-web-client-poll c)))
               (ignore-errors (delete-process (plist-get req :proc)))))
           pai-web-bus--clients)
  (clrhash pai-web-bus--clients)
  (clrhash pai-web-bus--blobs)
  (when (timerp pai-web-bus--flush-timer) (cancel-timer pai-web-bus--flush-timer))
  (setq pai-web-bus--flush-timer nil))

;;;; Clients

(defun pai-web-bus-new-client (session)
  "Register a new client of auth SESSION; return it."
  (let ((client (pai-web-client--create :id (pai-web-random-hex 12) :session session)))
    (puthash (pai-web-client-id client) client pai-web-bus--clients)
    client))

(defun pai-web-bus-client (id)
  "Return the client with ID, or nil."
  (and id (gethash id pai-web-bus--clients)))

(defun pai-web-bus-clients ()
  "Return all clients."
  (hash-table-values pai-web-bus--clients))

(defun pai-web-bus-active-p ()
  "Return non-nil when at least one page is connected."
  (> (hash-table-count pai-web-bus--clients) 0))

(defun pai-web-bus-drop-session-clients (session)
  "Forget the clients of auth SESSION (logged out); all when SESSION is t."
  (maphash (lambda (id c)
             (when (or (eq session t) (equal (pai-web-client-session c) session))
               (when-let ((req (pai-web-client-poll c)))
                 (ignore-errors
                   (pai-web-http-respond req 401 :body "{\"error\":\"logged out\"}")))
               (remhash id pai-web-bus--clients)))
           pai-web-bus--clients))

;;;; Blobs

(defun pai-web-bus-blob (bytes)
  "Store unibyte BYTES for chunked download; return the blob id."
  (let ((id (pai-web-random-hex 10)))
    (puthash id (cons bytes (+ (float-time) pai-web-bus-blob-ttl)) pai-web-bus--blobs)
    id))

(defun pai-web-bus-serve-blob (req)
  "Answer REQ with one chunk of a blob: query `id' and offset `o'."
  (let* ((entry (gethash (or (pai-web-http-query req "id") "") pai-web-bus--blobs))
         (offset (string-to-number (or (pai-web-http-query req "o") "0"))))
    (if (not entry)
        (pai-web-http-respond req 404 :body "{\"error\":\"no such blob\"}")
      (let* ((bytes (car entry))
             (total (length bytes))
             (offset (max 0 (min offset total)))
             (end (min total (+ offset pai-web-bus-blob-chunk))))
        (pai-web-http-respond req 200 :type "application/octet-stream"
                              :body (substring bytes offset end)
                              :headers `(("X-Total" . ,(number-to-string total))))))))

(defun pai-web-bus-respond-json (req value &optional status)
  "Answer REQ with VALUE as JSON (STATUS, default 200), as a blob when big."
  (let* ((json (pai-web-json value))
         (bytes (encode-coding-string json 'utf-8)))
    (if (<= (length bytes) pai-web-bus-inline-bytes)
        (pai-web-http-respond req (or status 200) :body bytes)
      (pai-web-http-respond
       req (or status 200)
       :body (format "{\"$blob\":\"%s\",\"size\":%d}"
                     (pai-web-bus-blob bytes) (length bytes))))))

;;;; Events

(defun pai-web-bus-broadcast (event &optional predicate)
  "Queue EVENT (a plist) for every client, or those satisfying PREDICATE."
  (when (pai-web-bus-active-p)
    (let* ((json (pai-web-json event))
           (bytes (encode-coding-string json 'utf-8))
           (seq (cl-incf pai-web-bus--seq)))
      (when (> (length bytes) pai-web-bus-event-bytes)
        (setq bytes (encode-coding-string
                     (format "{\"t\":\"blob\",\"blob\":\"%s\",\"size\":%d}"
                             (pai-web-bus-blob bytes) (length bytes))
                     'utf-8)))
      (maphash
       (lambda (_id c)
         (when (or (null predicate) (funcall predicate c))
           (if (> (+ (pai-web-client-bytes c) (length bytes)) pai-web-bus-queue-limit)
               (setf (pai-web-client-queue c) (list (cons seq "{\"t\":\"resync\"}"))
                     (pai-web-client-bytes c) 20)
             (push (cons seq bytes) (pai-web-client-queue c))
             (cl-incf (pai-web-client-bytes c) (length bytes)))))
       pai-web-bus--clients)
      (pai-web-bus--schedule-flush))))

(defun pai-web-bus--schedule-flush ()
  "Answer waiting polls shortly, batching events that arrive together."
  (unless (timerp pai-web-bus--flush-timer)
    (setq pai-web-bus--flush-timer
          (run-at-time 0.04 nil (lambda ()
                                  (setq pai-web-bus--flush-timer nil)
                                  (pai-web-bus-flush))))))

(defun pai-web-bus-flush ()
  "Answer every held poll that has events waiting."
  (maphash (lambda (_id c)
             (when (and (pai-web-client-poll c) (pai-web-client-queue c))
               (pai-web-bus--answer c)))
           pai-web-bus--clients))

(defun pai-web-bus--answer (client)
  "Answer CLIENT's held poll with its oldest queued events."
  (let ((req (pai-web-client-poll client))
        (events (reverse (pai-web-client-queue client)))
        (parts nil) (size 0) (last nil))
    (setf (pai-web-client-poll client) nil)
    (while (and events (or (null parts)
                           (<= (+ size (length (cdar events))) pai-web-bus-poll-bytes)))
      (push (cdar events) parts)
      (cl-incf size (1+ (length (cdar events))))
      (setq last (caar events))
      (setq events (cdr events)))
    (pai-web-http-respond
     req 200
     :body (concat (format "{\"seq\":%d,\"more\":%s,\"events\":["
                           (or last pai-web-bus--seq) (if events "true" "false"))
                   (mapconcat #'identity (nreverse parts) ",")
                   "]}"))))

(defun pai-web-bus--ack (client ack)
  "Drop CLIENT's events up to sequence number ACK."
  (when (and ack (> ack 0))
    (let ((kept (seq-filter (lambda (e) (> (car e) ack)) (pai-web-client-queue client))))
      (setf (pai-web-client-queue client) kept
            (pai-web-client-bytes client)
            (apply #'+ 0 (mapcar (lambda (e) (length (cdr e))) kept))))))

(defun pai-web-bus-poll (req client ack)
  "Handle the poll REQ of CLIENT, which processed events up to ACK."
  (setf (pai-web-client-last client) (float-time))
  (pai-web-bus--ack client ack)
  ;; a page has one poll in flight; an older one is answered empty
  (when-let ((old (pai-web-client-poll client)))
    (setf (pai-web-client-poll client) nil)
    (ignore-errors
      (pai-web-http-respond old 200 :body (format "{\"seq\":%d,\"more\":false,\"events\":[]}"
                                                  (or ack 0)))))
  (setf (pai-web-client-poll client) req
        (pai-web-client-poll-since client) (float-time))
  (if (pai-web-client-queue client)
      (pai-web-bus--answer client)
    (pai-web-http-hold req (lambda (_proc)
                             (when (eq (pai-web-client-poll client) req)
                               (setf (pai-web-client-poll client) nil))))))

(defun pai-web-bus-sweep ()
  "Time out held polls, forget silent clients and expired blobs."
  (let ((now (float-time)))
    (maphash
     (lambda (id c)
       (let ((req (pai-web-client-poll c)))
         (cond
          ((and req (> (- now (or (pai-web-client-poll-since c) now))
                       pai-web-bus-poll-timeout))
           (setf (pai-web-client-poll c) nil
                 (pai-web-client-last c) now)
           (ignore-errors
             (pai-web-http-respond req 200 :body (format "{\"seq\":%d,\"more\":false,\"events\":[]}"
                                                         pai-web-bus--seq))))
          ((and (null req) (> (- now (pai-web-client-last c)) pai-web-bus-client-ttl))
           (remhash id pai-web-bus--clients)))))
     pai-web-bus--clients)
    (maphash (lambda (id entry) (when (< (cdr entry) now) (remhash id pai-web-bus--blobs)))
             pai-web-bus--blobs)))

(provide 'pai-web-bus)
;;; pai-web-bus.el ends here
