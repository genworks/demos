;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; CLOSED WORK.  Code made in the lab is under the GNU AGPL unless its
;; author keeps the work closed from the start (the Closed source box,
;; before the first build).  A closed work is out of sight -- private, out
;; of the listings and of the archive's public face -- for *closed-days*
;; from when its session opened, the clock running per work.  Within that
;; time its author decides, by one of four exits:
;;
;;   HOST     a deployment with us (Monetize, where this lab deploys);
;;   DECLARE  a stated intention to host it themselves or distribute
;;            binaries built from it, with the box promising the quarterly
;;            report for Gendl-built binaries even when nothing is charged;
;;   PUBLIC   opened now, under the AGPL;
;;   DELETE   everything the lab keeps of it, gone.
;;
;; A work neither hosted nor declared when its time runs out is opened:
;; public, under the AGPL, its files' copyright lines given the license.
;; More time is asked for from the work's own panel and granted by the
;; house, case by case, through a door that wants the house's token.  The
;; author may take the source at any time (export.lisp): nothing here
;; unlocks it.
;;
;; A work outlives its session: when the session ends its record stays
;; closed in the archive, and the sweep (on the reaper's thread) finds it
;; there.  The deadline and the declaration ride in the record
;; ("closed_until", "declared") and, while the session lives, in the
;; tables below.
;;

(defvar *closed-until* (make-hash-table :test #'equal)
  "Session id -> the universal time its closed work is opened unless decided.")

(defvar *closed-declared* (make-hash-table :test #'equal)
  "Session id -> its author's declaration (a hash table: at, email).")

(defvar *closed-asked* (make-hash-table :test #'equal)
  "Session id -> when its author last asked the house for more time.")

(defvar *closed-lock* (bt:make-lock "prompt-lab closed works"))

(defvar *closed-swept* 0
  "When the sweep last walked the archive.")

(defparameter *closed-sweep-seconds* 3600
  "Integer.  The sweep walks the archive at most this often.")

(defparameter *closed-declaration-text*
  "I will host this myself or distribute binaries built from it, and I will make the quarterly report for Gendl-built binaries I host or distribute, even when I charge nothing for them."
  "String.  The words of the declaration's box, as the author ticks them;
kept with the declaration.")

(defun closed-offered? ()
  "Whether this lab offers a closed work at all: where it deploys and sells
closed, or in a free lab that keeps closed work for its author keys."
  (or (and *closed-source?* *deployments?*)
      (and *free-lab?* *free-lab-closed?*)))

(defun closed-allowed? (wallet)
  "Whether a session opened with WALLET may be closed: where this lab deploys
and sells closed, anyone; in a free lab, an author key, never a keyless
visitor (the user's ruling, 2026-11-12)."
  (or (and *closed-source?* *deployments?*)
      (and *free-lab?* *free-lab-closed?* (wallet-id? wallet) t)))

(defun closed-deadline (id)
  (bt:with-lock-held (*closed-lock*) (gethash id *closed-until*)))

(defun closed-declaration (id)
  (bt:with-lock-held (*closed-lock*) (gethash id *closed-declared*)))

(defun start-closed-clock! (session)
  "Start SESSION's closed work's clock, unless it has one already."
  (bt:with-lock-held (*closed-lock*)
    (unless (gethash (session-id session) *closed-until*)
      (setf (gethash (session-id session) *closed-until*)
            (+ (session-created session) (* *closed-days* 86400))))))

(defun end-closed-session! (session)
  "SESSION is ending: its closed mark goes with it.  The archived record
keeps the work closed, with its deadline, until the sweep or its author."
  (bt:with-lock-held (*kinds-lock*) (remhash (session-id session) *closed-sessions*))
  (forget-closed-work! (session-id session)))

(defun note-closed-record! (id json)
  "Take a restored record's deadline and declaration back into the tables."
  (let ((until (gethash "closed_until" json))
        (declared (gethash "declared" json)))
    (bt:with-lock-held (*closed-lock*)
      (when (integerp until) (setf (gethash id *closed-until*) until))
      (when (hash-table-p declared) (setf (gethash id *closed-declared*) declared)))))

(defun forget-closed-work! (id)
  (bt:with-lock-held (*closed-lock*)
    (remhash id *closed-until*)
    (remhash id *closed-declared*)
    (remhash id *closed-asked*)))

(defun record-deadline (json)
  "A record's deadline: its \"closed_until\", else its opening plus *closed-days*."
  (let ((until (gethash "closed_until" json))
        (created (gethash "created" json)))
    (cond ((integerp until) until)
          ((integerp created) (+ created (* *closed-days* 86400))))))

(defun record-closed? (json)
  (eq (gethash "closed" json) 'yason:true))

;;
;; The copyright line, opened.  A closed file reserves its rights
;; (tools.lisp, license-header); when the work is opened, each kept version
;; gets the AGPL lines in their place.
;;

(defun open-license-text (text)
  "TEXT, a model file, with a closed header's \"All rights reserved.\" line
replaced by the open one and the AGPL lines; TEXT itself when it has none."
  (let* ((mark ".  All rights reserved.")
         (end (search mark text))
         (start (and end (search ";; Copyright (C) " text :from-end t :end2 end))))
    (if (null start)
        text
        (concatenate 'string
                     (subseq text 0 start)
                     (format nil "~a.~%;;~%;; SPDX-License-Identifier: AGPL-3.0-or-later~%~
;; Free software under the GNU Affero General Public License, version 3 or~%~
;; (at your option) any later version: <https://www.gnu.org/licenses/agpl-3.0.html>."
                             (subseq text start end))
                     (subseq text (+ end (length mark)))))))

(defun open-license-in-files! (directory)
  "Give every model file in DIRECTORY (model.lisp and its versions) the open
license lines.  Never signals."
  (ignore-errors
   (dolist (file (append (directory (merge-pathnames "model.lisp" directory))
                         (directory (merge-pathnames "model-*.lisp" directory))))
     (ignore-errors
      (let* ((text (uiop:read-file-string file :external-format :utf-8))
             (opened (open-license-text text)))
        (unless (string= text opened)
          (write-text-file opened file)))))))

;;
;; Finding a work, for its owner.
;;

(defun live-session (id)
  "Session ID when it is live in the table here; nil otherwise (never a
restore from disk)."
  (bt:with-lock-held (*sessions-lock*) (gethash id *sessions*)))

(defun write-record (json file)
  (write-text-file (with-output-to-string (out) (yason:encode json out)) file))

(defun closed-work (id key)
  "The closed work ID, when KEY is its owner's.  Values: the live session or
nil, the archived record or nil, and its archive directory or nil.  Nil
when there is no such closed work or KEY is not its owner's."
  (when (and (session-id? id) (stringp key) (plusp (length key)))
    (let* ((session (live-session id))
           (directory (archived-directory id))
           (json (and directory (read-record (merge-pathnames "session.json" directory)))))
      (cond ((and session (session-closed? session))
             (and (equal (session-owner session) key) (values session json directory)))
            ((and json (record-closed? json) (equal (gethash "owner" json) key))
             (values nil json directory))))))

(defun work-decided? (id json)
  "Whether work ID is decided: declared, or hosted closed with us."
  (or (closed-declaration id)
      (and json (hash-table-p (gethash "declared" json)))
      (ignore-errors (closed-deployment? id))))

(defun closed-state (id key)
  "What work ID's owner (KEY) is shown of it, a hash table, or nil when it is
no closed work of theirs."
  (multiple-value-bind (session json) (closed-work id key)
    (when (or session json)
      (let* ((until (or (closed-deadline id) (and json (record-deadline json))))
             (declared (or (closed-declaration id) (and json (let ((d (gethash "declared" json))) (and (hash-table-p d) d)))))
             (hosted (ignore-errors (closed-deployment? id)))
             (left (and until (max 0 (ceiling (- until (get-universal-time)) 86400)))))
        (h "until" until
           "days_left" left
           "declared" (and declared (gethash "at" declared))
           "hosted" (if hosted t 'yason:false)
           "may_ask" (if (and left (<= left *closed-ask-days*) (not declared) (not hosted)) t 'yason:false)
           "deploys" (if *deployments?* t 'yason:false))))))

;;
;; The exits.
;;

(defun open-closed-work! (id &key session json directory (reason "opened by its author"))
  "Open work ID: public, under the AGPL, its files' copyright lines given the
license.  SESSION when it is live; JSON and DIRECTORY its archived record."
  (let ((note (format nil "This work was closed source; ~a, it is public now, under the GNU Affero General Public License." reason)))
    (when session
      (setf (session-private? session) nil)
      (bt:with-lock-held (*kinds-lock*) (remhash id *closed-sessions*))
      (let ((file (session-model-file session)))
        (when (probe-file file)
          (let* ((text (uiop:read-file-string file :external-format :utf-8))
                 (opened (open-license-text text)))
            (unless (string= text opened) (write-text-file opened file)))))
      (ignore-errors (log-event session :note "~a" note))
      (save-session! session))
    (let ((directory (or directory (archived-directory id))))
      (when directory
        (let* ((file (merge-pathnames "session.json" directory))
               (json (or (and (null session) json) (read-record file))))
          (when (and json (record-closed? json))
            (setf (gethash "private" json) 'yason:false
                  (gethash "closed" json) 'yason:false
                  (gethash "closed_until" json) nil
                  (gethash "log" json) (append (gethash "log" json)
                                               (list (list (get-universal-time) "note" note))))
            (write-record json file)))
        (open-license-in-files! directory)))
    (forget-closed-work! id)
    (ignore-errors (watch-event! :closed-opened nil :detail (format nil "~a: ~a" id reason)))
    t))

(defun delete-closed-work! (id &key session directory)
  "Delete everything the lab keeps of work ID: the live session (not
archived first), its replay, its archive directory with every version,
upload and thumbnail.  The monitor's audit log keeps its lines."
  (when session
    (bt:with-lock-held (*sessions-lock*)
      (remhash id *sessions*)
      (remhash id *session-locks*))
    (ignore-errors (forget-session-kind! session))
    (let ((package (find-package (session-package-name session))))
      (when package (ignore-errors (delete-package package))))
    (ignore-errors (uiop:delete-directory-tree (pathname (session-directory session))
                                               :validate t :if-does-not-exist :ignore)))
  (let ((replay (bt:with-lock-held (*replays-lock*) (gethash id *replays*))))
    (when replay (drop-replay replay)))
  (let ((directory (or directory (archived-directory id))))
    (when directory
      (ignore-errors (uiop:delete-directory-tree (pathname directory) :validate t :if-does-not-exist :ignore))))
  (bt:with-lock-held (*kinds-lock*) (remhash id *closed-sessions*))
  (forget-closed-work! id)
  (ignore-errors (watch-event! :closed-deleted nil :detail id))
  t)

(defun valid-email? (text)
  (and (stringp text) (<= 3 (length text) 200)
       (let ((at (position #\@ text)))
         (and at (plusp at) (< at (1- (length text)))
              (position #\. text :start at)
              (notany #'(lambda (c) (or (char= c #\Space) (< (char-code c) 32))) text)))))

(defun declare-closed-work! (id email &key session json directory)
  "Keep the author's declaration for work ID; the work stays closed."
  (let ((declaration (h "at" (get-universal-time) "email" email "words" *closed-declaration-text*)))
    (bt:with-lock-held (*closed-lock*) (setf (gethash id *closed-declared*) declaration))
    (if session
        (progn (ignore-errors (log-event session :note "You said you will host this work yourself or distribute binaries built from it; it stays closed."))
               (save-session! session))
        (let ((file (merge-pathnames "session.json" directory)))
          (setf (gethash "declared" json) declaration)
          (write-record json file)))
    (ignore-errors
     (gate-post "alert" (h "title" "Prompt lab: a closed work to be self-hosted or distributed"
                           "text" (format nil "Work ~a (~a lab): its author declared they will host it themselves or distribute binaries, and make the quarterly report.  Contact: ~a"
                                          id (engine-name) email)
                           "priority" "default" "tags" "page_facing_up")))
    declaration))

(defun ask-for-closed-time! (id)
  "Tell the house work ID's author asks for more closed time, with the line
that grants it.  Values: true, or nil and the reason."
  (let ((now (get-universal-time))
        (last (bt:with-lock-held (*closed-lock*) (gethash id *closed-asked*))))
    (cond ((and last (< (- now last) 86400))
           (values nil "You asked today already; the house answers by mail or by the date on this panel."))
          (t (multiple-value-bind (answer status)
                 (ignore-errors
                  (gate-post "alert" (h "title" "Prompt lab: more closed time asked for"
                                        "text" (format nil "Work ~a (~a lab) asks for more closed-source time.  To grant ~d days:~%curl -X POST -H \"X-Prompt-Lab-House: $PROMPT_LAB_HOUSE_TOKEN\" '~a~a/api/closed-extend?work=~a&days=~d'"
                                                       id (engine-name) *closed-extension-days*
                                                       (or *house-base* "") *url-prefix* id *closed-extension-days*)
                                        "priority" "default" "tags" "hourglass")))
               (declare (ignore answer))
               (if (eql status 200)
                   (progn (bt:with-lock-held (*closed-lock*) (setf (gethash id *closed-asked*) now))
                          t)
                   (values nil "The request could not be sent just now; try again later, or write to the lab's address.")))))))

(defun decide-closed-work! (id key action &key email box)
  "The owner's (KEY) exit for closed work ID: ACTION one of \"public\",
\"delete\", \"declare\" (with EMAIL and BOX true) or \"more-time\".  Values: a
notice for the author, or nil and the reason."
  (multiple-value-bind (session json directory) (closed-work id key)
    (cond ((not (or session json)) (values nil "That is no closed work of yours."))
          ((equal action "public")
           (open-closed-work! id :session session :json json :directory directory)
           "Your work is public now, under the GNU Affero General Public License.")
          ((equal action "delete")
           (if (ignore-errors (closed-deployment? id))
               (values nil "This work is deployed: take the deployment down first.")
               (progn (delete-closed-work! id :session session :directory directory)
                      "Your work is deleted: the lab keeps nothing of it.")))
          ((equal action "declare")
           (cond ((not box) (values nil "Tick the box first: the declaration is what keeps the work closed."))
                 ((not (valid-email? email)) (values nil "Give an address the house can write to about the quarterly report."))
                 (t (declare-closed-work! id email :session session :json json :directory directory)
                    "Declared: the work stays closed, and it is yours to host or distribute.")))
          ((equal action "more-time")
           (multiple-value-bind (ok? reason) (ask-for-closed-time! id)
             (if ok? "Asked: the house will look at it and extend the date if it can." (values nil reason))))
          (t (values nil "No such choice here.")))))

;;
;; The sweep: works whose time ran out, opened.
;;

(defun record-engine-mine? (json)
  "Whether a record is this room's to sweep: its engine, or none on the free one."
  (let ((engine (gethash "engine" json)))
    (if (stringp engine) (equal engine (engine-name)) (eq *engine* :gendl))))

(defun sweep-closed-works! (&key (now (get-universal-time)) force?)
  "Open every closed work whose time ran out undecided: live ones here, and
this room's archived ones.  At most every *closed-sweep-seconds* unless
FORCE?.  Answers the ids opened."
  (when (or force? (> (- now *closed-swept*) *closed-sweep-seconds*))
    (setq *closed-swept* now)
    (let ((opened nil))
      (dolist (session (bt:with-lock-held (*sessions-lock*)
                         (loop for s being the hash-values of *sessions* collect s)))
        (let* ((id (session-id session))
               (until (closed-deadline id)))
          (when (and (session-closed? session) until (> now until) (not (work-decided? id nil))
                     (not (session-busy? session)))
            (ignore-errors (open-closed-work! id :session session :reason "its thirty days ran out undecided"))
            (push id opened))))
      (when *archive-root*
        (dolist (file (directory (merge-pathnames "*/*/session.json" *archive-root*)))
          (ignore-errors
           ;; most records were never closed: read the text before parsing it
           (when (search "\"closed\":true" (uiop:read-file-string file :external-format :utf-8))
             (let* ((json (read-record file))
                    (id (and json (gethash "id" json)))
                    (until (and json (record-deadline json))))
               (when (and json (stringp id) (record-closed? json) (record-engine-mine? json)
                          (not (live-session id)) until (> now until)
                          (not (work-decided? id json)))
                 (open-closed-work! id :json json :directory (uiop:pathname-directory-pathname file)
                                       :reason "its thirty days ran out undecided")
                 (push id opened)))))))
      (nreverse opened))))

;;
;; The house's door: more closed time, case by case.
;;

(defun house-token ()
  "The house's token for this lab's house doors, the first line of
*house-token-file*, read at every request; nil when there is none."
  (ignore-errors
   (when (and *house-token-file* (probe-file *house-token-file*))
     (let ((line (with-open-file (in *house-token-file*) (read-line in nil))))
       (and line (let ((token (string-trim '(#\space #\tab #\return) line)))
                   (and (>= (length token) 16) token)))))))

(defun extend-closed-work! (id days &key (now (get-universal-time)))
  "Give work ID DAYS more closed time, from its deadline or from now,
whichever is later.  Answers the new deadline, or nil when ID is no closed
work here."
  (let* ((session (live-session id))
         (directory (archived-directory id))
         (file (and directory (merge-pathnames "session.json" directory)))
         (json (and file (read-record file))))
    (when (or (and session (session-closed? session)) (and json (record-closed? json)))
      (let* ((from (max now (or (closed-deadline id) (and json (record-deadline json)) now)))
             (until (+ from (* days 86400))))
        (bt:with-lock-held (*closed-lock*) (setf (gethash id *closed-until*) until))
        (cond (session
               (ignore-errors (log-event session :note "The house gave this work ~d more days closed." days))
               (save-session! session))
              (t (setf (gethash "closed_until" json) until)
                 (write-record json file)))
        until))))

(defun closed-extend-door (req ent)
  "POST <prefix>/api/closed-extend?work=<id>&days=<n> with the house's token
(X-Prompt-Lab-House, or token=): more closed time for a work.  404 where
there is no house token, 403 for a wrong one."
  (let ((token (house-token))
        (given (or (net.aserve:header-slot-value req :x-prompt-lab-house) (query-value req "token")))
        (id (query-value req "work"))
        (days (ignore-errors (parse-integer (or (query-value req "days") "")))))
    (cond ((null token) (refuse req ent net.aserve:*response-not-found* "Not here."))
          ((not (and (stringp given) (string= given token))) (refuse req ent net.aserve:*response-forbidden* "Not yours."))
          ((not (and (integerp days) (< 0 days 3651))) (refuse req ent "Say how many days, 1 to 3650."))
          (t (let ((until (and (session-id? id) (extend-closed-work! id days))))
               (if until
                   (respond-json req ent (h "work" id "closed_until" until))
                   (refuse req ent net.aserve:*response-not-found* "No closed work ~a here." id)))))))
