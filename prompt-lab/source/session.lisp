;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; A session is one visitor's workspace: a package of its own in the
;; running image and a directory holding the model's source file, which
;; the agent writes and the visitor may edit by hand.
;;

(defvar *sessions* (make-hash-table :test #'equal))

(defvar *sessions-lock* (bt:make-lock "prompt-lab sessions"))

(defstruct (session (:constructor make-session-internal))
  id
  package-name
  directory
  (created (get-universal-time))
  (last-used (get-universal-time))
  ;; The visitor's address as the session door saw it (guards.lisp).
  (address nil)
  ;; The secret the session door hands the browser that opened the
  ;; session: only a request carrying it may prompt, edit or pay here;
  ;; everyone else watches (browse.lisp).  Nil on a session opened
  ;; before owners were minted, whose own address stands in for it.
  (owner nil)
  ;; True when the owner, having bought credits, closed the session to
  ;; watchers: out of the listings, its URL answering no one else.
  (private? nil)
  ;; True for a replay: an archived session's model compiled again to be
  ;; drawn, never archived or metered (browse.lisp).
  (replay? nil)
  ;; The agent's conversation, append-only (thinking blocks included, as
  ;; returned), as yason hash tables.
  (messages nil)
  ;; Token totals across every call of the session, a plist.
  (usage (list :input 0 :output 0 :cache-read 0 :cache-write 0))
  ;; Symbols compiled and run, as reported to the gate's meter (meter.lisp),
  ;; and the credits the gate booked for them: a plist (:compile n :run n
  ;; :credits c).
  (meter nil)
  ;; What the gate reports back with each answer (guards.lisp): the
  ;; session's spend in cents, the free allowance, and the visitor's
  ;; wallet -- its id, credit left and what it has been charged.
  (cents 0)
  (allowance nil)
  (wallet nil)
  (credits nil)
  (charged 0)
  ;; The gate's last balance answer (a hash table): whether top-ups
  ;; are offered, the amounts, the markup.
  (balance nil)
  ;; What the page shows: (time kind text) entries, newest last.
  (log nil)
  ;; True while a prompt is being worked on.
  (busy? nil))

(defun new-session-id ()
  (format nil "~(~{~2,'0x~}~)" (loop repeat 6 collect (random 256 (make-random-state t)))))

(defun new-owner-key ()
  "32 hex digits from the kernel's random source, else from random."
  (let ((octets (or (ignore-errors
                     (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
                       (let ((v (make-array 16 :element-type '(unsigned-byte 8))))
                         (read-sequence v in)
                         v)))
                    (let ((state (make-random-state t)))
                      (loop repeat 16 collect (random 256 state))))))
    (format nil "~(~{~2,'0x~}~)" (coerce octets 'list))))

;; One lock per session, for what several threads write at once: the log
;; (the agent's thread, the external agent's tool calls, the doors) and
;; the session's record on disk (the agent's end, confirm, privacy).
;; Kept beside the struct rather than in it: a reload that adds a slot
;; to the struct strands the sessions already in the table, whose old
;; layout fails in the printer.
(defvar *session-locks* (make-hash-table :test #'equal))

(defun session-lock (session)
  (bt:with-lock-held (*sessions-lock*)
    (or (gethash (session-id session) *session-locks*)
        (setf (gethash (session-id session) *session-locks*)
              (bt:make-recursive-lock (format nil "prompt-lab session ~a" (session-id session)))))))

(defmacro with-session-lock ((session) &body body)
  `(bt:with-recursive-lock-held ((session-lock ,session)) ,@body))

;; Who wants to hear that a session changed: a page that shows it live
;; (the sheet, sheet/) pushes the change instead of waiting for a poll.
;; Called after every log entry and every save, from whichever thread
;; made the change; a hook that fails is ignored.
(defvar *session-change-hooks* nil
  "List of functions of one argument, a session, called when it changes.")

(defun session-changed! (session)
  (dolist (hook *session-change-hooks*)
    (ignore-errors (funcall hook session))))

(defun make-session (&key (id (new-session-id)) address wallet (owner (new-owner-key))
                          (register? t))
  "Create a session: a fresh package defined like gdl-user, and a directory
under *workspace-root*.  Returns the session.  REGISTER? nil leaves it out
of the table, for a caller that fills it in first (restore-session)."
  (let* ((keyword (intern (string-upcase (format nil "pl-~a" id)) :keyword))
         (package (progn (eval `(gdl:define-package ,keyword))
                         (find-package keyword)))
         (directory (merge-pathnames (format nil "~a/" id) *workspace-root*))
         (session (make-session-internal :id id :package-name (package-name package)
                                         :directory directory :address address :wallet wallet
                                         :owner owner)))
    (ensure-directories-exist directory)
    (when register? (register-session! session))
    session))

(defun register-session! (session)
  (bt:with-lock-held (*sessions-lock*)
    (setf (gethash (session-id session) *sessions*) session)))

(defvar *restore-lock* (bt:make-lock "prompt-lab restore"))

(defun find-session (id)
  "The live session ID, or the one restored from its directory when a
restart has emptied the table (see save-session!); nil when neither."
  (or (bt:with-lock-held (*sessions-lock*) (gethash id *sessions*))
      (bt:with-lock-held (*restore-lock*)
        (or (bt:with-lock-held (*sessions-lock*) (gethash id *sessions*))
            (restore-session id)))))


;;
;; Sessions survive a restart of the Lisp.  Everything a session is,
;; beyond its model file, is written beside that file as session.json
;; after every prompt and every edit; a request naming a session the
;; table has forgotten reads it back, makes the package afresh and
;; compiles the model file again, so the visitor's page and viewer carry
;; on across a raise of the workshop (a visitor lost an Arc de Triomphe
;; that way on 2026-09-27).  The conversation goes with it, thinking
;; blocks and rendered images included, so the agent's memory of the
;; session survives too.
;;

(defun session-id? (id)
  (and (stringp id) (= (length id) 12) (every #'(lambda (c) (digit-char-p c 16)) id)))

(defun session-state-file (session)
  (merge-pathnames "session.json" (session-directory session)))

(defun save-session! (session)
  "Write SESSION's record beside its model file, atomically, then its copy,
transcript and model file to the archive (archive.lisp).  Never signals.
One writer at a time per session: the agent's end, a confirm and a
privacy switch can all save at once, and shared session.tmp."
  (with-session-lock (session)
  (prog1
   (ignore-errors
   (let* ((file (session-state-file session))
          (tmp (make-pathname :type "tmp" :defaults file))
          (usage (session-usage session)))
     (ensure-directories-exist file)
     (with-open-file (out tmp :direction :output :if-exists :supersede :external-format :utf-8)
       (yason:encode
        (h "version" 1
           "id" (session-id session)
           "address" (session-address session)
           "owner" (session-owner session)
           "private" (if (session-private? session) t 'yason:false)
           "wallet" (session-wallet session)
           "created" (session-created session)
           "last_used" (session-last-used session)
           "usage" (h "input" (getf usage :input) "output" (getf usage :output)
                      "cache_read" (getf usage :cache-read) "cache_write" (getf usage :cache-write))
           "meter" (h "compile" (or (getf (session-meter session) :compile) 0)
                      "run" (or (getf (session-meter session) :run) 0)
                      "credits" (or (getf (session-meter session) :credits) 0))
           ;; the engine of the room that ran the session (parameters.lisp)
           "engine" (engine-name)
           ;; what the session builds: a model or a web app (kinds.lisp)
           "kind" (kind-name (session-kind session))
           ;; opened closed-source, and not yet reverted (deploy.lisp)
           "closed" (if (session-closed? session) t 'yason:false)
           "cents" (session-cents session)
           "allowance" (session-allowance session)
           "credits" (session-credits session)
           "charged" (session-charged session)
           "log" (mapcar #'(lambda (entry)
                             (list (first entry) (string-downcase (second entry)) (third entry)))
                         (session-log session))
           "messages" (session-messages session))
        out))
     (when (probe-file file) (delete-file file))
     (rename-file tmp file)
     file))
    (archive-session! session)
    (session-changed! session))))

(defun repair-messages (messages)
  "MESSAGES as parsed from a session file, with every tool_result's
is_error a JSON boolean again: a file written between the first
restore and this fix holds null there, and the API refuses null."
  (dolist (message messages messages)
    (when (hash-table-p message)
      (let ((content (gethash "content" message)))
        (when (listp content)
          (dolist (block content)
            (when (and (hash-table-p block) (equal (gethash "type" block) "tool_result"))
              (let ((flag (gethash "is_error" block)))
                (setf (gethash "is_error" block)
                      (if (or (eq flag t) (eq flag 'yason:true)) t 'yason:false))))))))))

(defun restore-session (id)
  "Bring session ID back from its directory: the record from session.json,
a fresh package, the model file compiled and loaded.  Nil when nothing
is on disk for it."
  (when (session-id? id)
    (let* ((directory (merge-pathnames (format nil "~a/" id) *workspace-root*))
           (file (merge-pathnames "session.json" directory))
           (json (and (probe-file file)
                      (ignore-errors
                       (with-open-file (in file :external-format :utf-8)
                         ;; booleans come back as yason:true / yason:false,
                         ;; not T / NIL: a tool_result's "is_error": false
                         ;; parsed as NIL re-encodes as null, which the API
                         ;; refuses ("Input should be a valid boolean" --
                         ;; the first restored session, 2026-09-27)
                         (let ((yason:*parse-json-booleans-as-symbols* t))
                           (yason:parse in)))))))
      (when (hash-table-p json)
        ;; filled in before it goes into the table: a request finding it
        ;; there must find all of it (find-session's first look is unlocked
        ;; by *restore-lock*)
        (let ((session (make-session :id id :register? nil :address (gethash "address" json)
                                     :wallet (let ((w (gethash "wallet" json))) (and (stringp w) w))
                                     ;; nil for a session older than owners
                                     :owner (let ((o (gethash "owner" json))) (and (stringp o) o)))))
          (flet ((number-or (key default) (let ((v (gethash key json))) (if (realp v) v default)))
                 (number-or-nil (key) (let ((v (gethash key json))) (and (realp v) v))))
            (setf (session-private? session) (eq (gethash "private" json) 'yason:true))
            (when (and (eq (gethash "closed" json) 'yason:true) (session-private? session))
              (close-session-source! session))
            (setf (session-created session) (number-or "created" (get-universal-time))
                  (session-cents session) (number-or "cents" 0)
                  (session-allowance session) (number-or-nil "allowance")
                  (session-credits session) (number-or-nil "credits")
                  (session-charged session) (number-or "charged" 0))
            (let ((usage (gethash "usage" json)))
              (when (hash-table-p usage)
                (setf (session-usage session)
                      (list :input (or (gethash "input" usage) 0) :output (or (gethash "output" usage) 0)
                            :cache-read (or (gethash "cache_read" usage) 0)
                            :cache-write (or (gethash "cache_write" usage) 0)))))
            (let ((meter (gethash "meter" json)))
              (when (hash-table-p meter)
                (setf (session-meter session)
                      (list :compile (let ((n (gethash "compile" meter))) (if (integerp n) n 0))
                            :run (let ((n (gethash "run" meter))) (if (integerp n) n 0))
                            :credits (let ((n (gethash "credits" meter))) (if (realp n) n 0))))))
            (setf (session-log session)
                  (loop for entry in (gethash "log" json)
                        when (and (listp entry) (= (length entry) 3) (stringp (second entry)))
                          collect (list (first entry) (intern (string-upcase (second entry)) :keyword) (third entry))))
            (setf (session-messages session) (repair-messages (gethash "messages" json))))
          ;; a web app's file compiles only in a package opened to GWL (kinds.lisp)
          (when (equal (gethash "kind" json) "app")
            (ignore-errors (set-session-kind! session :app)))
          (when (probe-file (session-model-file session))
            (ignore-errors (load-model-file session)))
          (log-event session :note "The workshop restarted; your session was restored from its file.")
          (save-session! session)
          (register-session! session)
          session)))))

(defun delete-session (session)
  "Forget SESSION, delete its package and its directory -- after a last
copy of what it holds goes to the archive.  A session opened
closed-source that never deployed goes there public (deploy.lisp)."
  (ignore-errors (revert-closed-session! session))
  (archive-session! session)
  (bt:with-lock-held (*sessions-lock*)
    (remhash (session-id session) *sessions*)
    (remhash (session-id session) *session-locks*))
  (forget-session-kind! session)
  (let ((package (find-package (session-package-name session))))
    (when package (delete-package package)))
  (let ((directory (session-directory session)))
    (when (probe-file directory)
      (uiop:delete-directory-tree (pathname directory) :validate t :if-does-not-exist :ignore))))

(defun session-package (session)
  (or (find-package (session-package-name session))
      (error "The package of session ~a is gone." (session-id session))))

(defun session-model-file (session)
  (merge-pathnames "model.lisp" (session-directory session)))

(defun touch (session)
  (setf (session-last-used session) (get-universal-time))
  session)


;;
;; The reaper: sessions unused for *session-lifetime* are deleted, and so
;; are workspace directories on disk that no session owns (left by a
;; Lisp that restarted) once they are that old.
;;

(defvar *reaper-thread* nil)

(defun stale-directories ()
  "Session directories under *workspace-root* that no live session owns."
  (let ((owned (bt:with-lock-held (*sessions-lock*)
                 (loop for session being the hash-values of *sessions*
                       collect (namestring (session-directory session))))))
    (remove-if #'(lambda (directory) (member (namestring directory) owned :test #'string=))
               (directory (merge-pathnames "*/" *workspace-root*)))))

(defun reap-sessions! (&key (lifetime *session-lifetime*) (now (get-universal-time)))
  "Delete every session unused for LIFETIME seconds that is not busy, and
every unowned session directory whose file is that old.  Returns the ids
and directories reaped."
  (let ((reaped nil))
    (dolist (session (bt:with-lock-held (*sessions-lock*)
                       (loop for session being the hash-values of *sessions* collect session)))
      (when (and (not (session-busy? session))
                 (> (- now (session-last-used session)) lifetime))
        (ignore-errors (delete-session session))
        (push (session-id session) reaped)))
    (dolist (directory (stale-directories))
      ;; an unowned directory is a session a restarted Lisp has not been
      ;; asked for yet: its age is its record's, when it has one
      (let* ((record (merge-pathnames "session.json" directory))
             (age (- now (or (ignore-errors (file-write-date (if (probe-file record) record directory))) now))))
        (when (> age lifetime)
          (archive-stale-directory! directory)
          ;; opened closed-source and never deployed: public in the archive
          (revert-closed-record! directory)
          (ignore-errors (uiop:delete-directory-tree (pathname directory) :validate t))
          (push (namestring directory) reaped))))
    (nreverse reaped)))

(defun start-reaper! ()
  "Run the reaper every *reaper-interval* seconds in a thread of its own;
a no-op when one is running."
  (unless (and *reaper-thread* (bt:thread-alive-p *reaper-thread*))
    (setf *reaper-thread*
          (bt:make-thread #'(lambda ()
                              (loop (sleep *reaper-interval*)
                                    (handler-case (progn (reap-external-claims!) (reap-sessions!)
                                                         (reap-replays!) (prune-addresses!))
                                      (error (condition)
                                        (format *error-output* "~&prompt-lab reaper: ~a~%" condition)))))
                          :name "prompt-lab reaper")))
  *reaper-thread*)

(defun stop-reaper! ()
  (when (and *reaper-thread* (bt:thread-alive-p *reaper-thread*))
    (bt:destroy-thread *reaper-thread*))
  (setf *reaper-thread* nil))
