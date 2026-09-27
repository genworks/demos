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
  ;; The agent's conversation, append-only (thinking blocks included, as
  ;; returned), as yason hash tables.
  (messages nil)
  ;; Token totals across every call of the session, a plist.
  (usage (list :input 0 :output 0 :cache-read 0 :cache-write 0))
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

(defun make-session (&key (id (new-session-id)) address wallet)
  "Create a session: a fresh package defined like gdl-user, and a directory
under *workspace-root*.  Returns the session."
  (let* ((keyword (intern (string-upcase (format nil "pl-~a" id)) :keyword))
         (package (progn (eval `(gdl:define-package ,keyword))
                         (find-package keyword)))
         (directory (merge-pathnames (format nil "~a/" id) *workspace-root*))
         (session (make-session-internal :id id :package-name (package-name package)
                                         :directory directory :address address :wallet wallet)))
    (ensure-directories-exist directory)
    (bt:with-lock-held (*sessions-lock*)
      (setf (gethash id *sessions*) session))
    session))

(defun find-session (id)
  (bt:with-lock-held (*sessions-lock*) (gethash id *sessions*)))

(defun delete-session (session)
  "Forget SESSION, delete its package and its directory."
  (bt:with-lock-held (*sessions-lock*)
    (remhash (session-id session) *sessions*))
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
      (let ((age (- now (or (ignore-errors (file-write-date directory)) now))))
        (when (> age lifetime)
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
                                    (handler-case (progn (reap-sessions!) (prune-addresses!))
                                      (error (condition)
                                        (format *error-output* "~&prompt-lab reaper: ~a~%" condition)))))
                          :name "prompt-lab reaper")))
  *reaper-thread*)

(defun stop-reaper! ()
  (when (and *reaper-thread* (bt:thread-alive-p *reaper-thread*))
    (bt:destroy-thread *reaper-thread*))
  (setf *reaper-thread* nil))
