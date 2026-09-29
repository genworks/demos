;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; Browsing other visitors' sessions, read-only.  Two listings: the
;; LIVE sessions (the table, and the workspace directories a restarted
;; Lisp has not been asked for yet) and the ARCHIVED ones (the archive,
;; every session ever opened here, live or long reaped).  A live session
;; opens at its own URL (?session=<id>) and a watcher sees what the
;; owner sees, less the owner's own things (page.lisp, session-state);
;; an archived one opens at ?archive=<id> with its log, each version of
;; its model file, and the model drawn in a REPLAY: the last version
;; compiled again into a package of its own under *replay-root*, never
;; archived, never metered, reaped like a session.  What a listing shows
;; is what the page shows -- prompts, the agent's replies, the code;
;; never an address, a wallet or an owner key.  *browsing?* nil shuts
;; every door here.
;;

(defun browsing-off (req ent)
  (refuse req ent net.aserve:*response-not-found* "Browsing sessions is off here."))

(defun read-record (file)
  "The session record in FILE (a session.json), a hash table; nil when
there is none or it does not parse."
  (ignore-errors
   (when (probe-file file)
     (with-open-file (in file :external-format :utf-8)
       (let ((yason:*parse-json-booleans-as-symbols* t))
         (let ((json (yason:parse in)))
           (and (hash-table-p json) json)))))))

(defparameter *title-length* 160
  "Integer. Characters of a session's first prompt a listing shows.")

(defun summary (&key id created last-used log engine model? busy?)
  "One line of a listing.  LOG as the struct keeps it or as a record
holds it: (time kind text), the kind a keyword or a string."
  (let ((prompts (loop for entry in log
                       when (and (listp entry) (= (length entry) 3)
                                 (string-equal (string (second entry)) "prompt"))
                         collect (third entry))))
    (h "id" id
       "created" (and (integerp created) (epoch-seconds created))
       "last_used" (and (integerp last-used) (epoch-seconds last-used))
       "prompts" (length prompts)
       "title" (let ((first (first prompts)))
                 (if (and (stringp first) (> (length first) *title-length*))
                     (concatenate 'string (subseq first 0 *title-length*) "…")
                     first))
       "engine" (or engine "gendl")
       "model" (if model? t 'yason:false)
       "busy" (if busy? t 'yason:false))))

(defun record-summary (json model-file)
  (summary :id (gethash "id" json) :created (gethash "created" json)
           :last-used (gethash "last_used" json) :log (gethash "log" json)
           :engine (gethash "engine" json) :model? (probe-file model-file)))

;; A record holds the whole conversation, rendered images and all; a
;; listing reads a summary once per version of the file.
(defvar *summaries* (make-hash-table :test #'equal)
  "namestring of a session.json -> (write-date . summary)")

(defvar *summaries-lock* (bt:make-lock "prompt-lab summaries"))

(defun file-summary (file)
  "The summary of the record in FILE, from the cache while the file is
unchanged; nil when it does not parse."
  (let* ((key (namestring file))
         (date (ignore-errors (file-write-date file)))
         (cached (bt:with-lock-held (*summaries-lock*) (gethash key *summaries*))))
    (if (and cached date (eql (car cached) date))
        (cdr cached)
        (let* ((json (read-record file))
               (summary (and json (record-summary json (merge-pathnames "model.lisp" file)))))
          (when (and summary date)
            (bt:with-lock-held (*summaries-lock*)
              (setf (gethash key *summaries*) (cons date summary))))
          summary))))

(defun worth-listing? (summary)
  "A session that never took a prompt and holds no model is only an
opened page."
  (or (plusp (gethash "prompts" summary)) (eq (gethash "model" summary) t)))

(defun newest-first (summaries)
  (let ((sorted (sort (copy-list summaries) #'>
                      :key #'(lambda (s) (or (gethash "last_used" s) (gethash "created" s) 0)))))
    (subseq sorted 0 (min (length sorted) *browse-limit*))))


;;
;; Live sessions.
;;

(defun live-summaries ()
  "The live sessions worth listing: the table's, and the workspace's
directories a restarted Lisp has not been asked for yet."
  (let* ((sessions (bt:with-lock-held (*sessions-lock*)
                     (loop for session being the hash-values of *sessions* collect session)))
         (ids (mapcar #'session-id sessions)))
    (remove-if-not
     #'worth-listing?
     (append
      (mapcar #'(lambda (session)
                  (summary :id (session-id session) :created (session-created session)
                           :last-used (session-last-used session) :log (session-log session)
                           :engine (engine-name) :model? (probe-file (session-model-file session))
                           :busy? (session-busy? session)))
              sessions)
      (loop for file in (directory (merge-pathnames "*/session.json" *workspace-root*))
            for summary = (file-summary file)
            when (and summary (not (member (gethash "id" summary) ids :test #'equal)))
              collect summary)))))

(defun live? (id)
  "True when session ID is live here: in the table, or on disk to restore."
  (and (session-id? id)
       (or (bt:with-lock-held (*sessions-lock*) (gethash id *sessions*))
           (probe-file (merge-pathnames (format nil "~a/session.json" id) *workspace-root*)))
       t))

(defun sessions-door (req ent)
  "GET <prefix>/api/sessions: the live sessions, newest first."
  (if (not *browsing?*)
      (browsing-off req ent)
      (respond-json req ent (h "engine" (engine-name)
                               "sessions" (coerce (newest-first (live-summaries)) 'vector)))))


;;
;; The archive.
;;

(defun archived-directory (id)
  "The archive directory of session ID, whatever day it opened; nil when
the archive holds none."
  (when (and *archive-root* (session-id? id))
    (first (directory (merge-pathnames (format nil "*/~a/" id) *archive-root*)))))

(defun archive-summaries ()
  (when *archive-root*
    (remove-if-not
     #'worth-listing?
     (remove nil (mapcar #'file-summary
                         (directory (merge-pathnames "*/*/session.json" *archive-root*)))))))

(defun archive-door (req ent)
  "GET <prefix>/api/archive: every archived session, newest first, each
marked live when it still is."
  (if (not *browsing?*)
      (browsing-off req ent)
      (let ((summaries (archive-summaries)))
        (respond-json req ent
                      (h "engine" (engine-name)
                         "total" (length summaries)
                         "sessions" (map 'vector
                                         #'(lambda (summary)
                                             (let ((copy (alexandria:copy-hash-table summary)))
                                               (setf (gethash "live" copy)
                                                     (if (live? (gethash "id" summary)) t 'yason:false))
                                               copy))
                                         (newest-first summaries)))))))

(defun model-versions (directory)
  "The model-NNN.lisp files in an archive DIRECTORY, oldest first."
  (sort (directory (merge-pathnames "model-*.lisp" directory)) #'string< :key #'namestring))

(defun archived-door (req ent)
  "GET <prefix>/api/archived?id=<id>[&version=<n>]: an archived session
as the page shows it, read-only -- its log, the numbers, the model file
as it last was or its Nth version, and whether a replay can draw it."
  (let* ((id (query-value req "id"))
         (directory (and *browsing?* (archived-directory id)))
         (json (and directory (read-record (merge-pathnames "session.json" directory)))))
    (cond ((not *browsing?*) (browsing-off req ent))
          ((null json) (refuse req ent net.aserve:*response-not-found* "No such archived session."))
          (t
           (let* ((versions (model-versions directory))
                  (version (ignore-errors (parse-integer (query-value req "version"))))
                  (file (if (and version (<= 1 version (length versions)))
                            (nth (1- version) versions)
                            (merge-pathnames "model.lisp" directory)))
                  (engine (or (gethash "engine" json) "gendl"))
                  (log (loop for entry in (gethash "log" json)
                             when (and (listp entry) (= (length entry) 3) (integerp (first entry)))
                               collect entry)))
             (respond-json req ent
                           (h "id" id
                              "archived" t
                              "live" (if (live? id) t 'yason:false)
                              "created" (let ((c (gethash "created" json))) (and (integerp c) (epoch-seconds c)))
                              "last_used" (let ((c (gethash "last_used" json))) (and (integerp c) (epoch-seconds c)))
                              "engine" engine
                              ;; a replay compiles here, so only on this room's engine
                              "replayable" (if (and (equal engine (engine-name)) (probe-file file)) t 'yason:false)
                              "usage" (gethash "usage" json)
                              "meter" (gethash "meter" json)
                              "log" (log-vector log)
                              "versions" (length versions)
                              "version" (and version (<= 1 version (length versions)) version)
                              "model_source" (file-model-body file))))))))


;;
;; Replays: an archived model compiled again to be drawn.
;;

(defvar *replays* (make-hash-table :test #'equal)
  "session id -> the replay session drawing its archived model")

(defvar *replays-lock* (bt:make-lock "prompt-lab replays"))

(defvar *replay-build-lock* (bt:make-lock "prompt-lab replay builds")
  "Held while a replay is built, so one id is compiled once.")

(defun find-replay (id)
  (let ((replay (bt:with-lock-held (*replays-lock*) (gethash id *replays*))))
    (and replay (touch replay))))

(defun drop-replay (replay)
  "Forget REPLAY, delete its package and its scratch directory."
  (bt:with-lock-held (*replays-lock*)
    (when (eq (gethash (session-id replay) *replays*) replay)
      (remhash (session-id replay) *replays*)))
  (let ((package (find-package (session-package-name replay))))
    (when package (ignore-errors (delete-package package))))
  (ignore-errors (uiop:delete-directory-tree (pathname (session-directory replay))
                                             :validate t :if-does-not-exist :ignore)))

(defun make-room-for-replay! ()
  "Drop the least recently used replays until there is room for one more."
  (loop (let ((replays (bt:with-lock-held (*replays-lock*)
                         (loop for r being the hash-values of *replays* collect r))))
          (if (< (length replays) *max-replays*)
              (return)
              (drop-replay (first (sort replays #'< :key #'session-last-used)))))))

(defun ensure-replay (id)
  "The replay of archived session ID, built when there is none: the last
version of its model file written under a replay header into a package
of its own, compiled and loaded.  Values: the replay (nil when the
archive holds no model for ID) and the compiler's text."
  (or (find-replay id)
      (bt:with-lock-held (*replay-build-lock*)
        (or (find-replay id)
            (let* ((directory (archived-directory id))
                   (json (and directory (read-record (merge-pathnames "session.json" directory))))
                   (source (and directory (file-model-body (merge-pathnames "model.lisp" directory)))))
              (when (and json (plusp (length source)))
                (make-room-for-replay!)
                (let* ((keyword (intern (string-upcase (format nil "pl-replay-~a" id)) :keyword))
                       (package (progn (let ((old (find-package keyword))) (when old (delete-package old)))
                                       (eval `(gdl:define-package ,keyword))
                                       (find-package keyword)))
                       (replay (make-session-internal
                                :id id :package-name (package-name package) :replay? t
                                :created (let ((c (gethash "created" json))) (if (integerp c) c (get-universal-time)))
                                :directory (merge-pathnames (format nil "~a/" id) *replay-root*))))
                  (ensure-directories-exist (session-directory replay))
                  (multiple-value-bind (blocks error?) (write-model replay source)
                    (declare (ignore error?))
                    (bt:with-lock-held (*replays-lock*)
                      (setf (gethash id *replays*) replay))
                    (values replay (or (cdr (assoc "text" (first blocks) :test #'string=)) ""))))))))))

(defun reap-replays! (&key (lifetime *session-lifetime*) (now (get-universal-time)))
  "Drop every replay unused for LIFETIME seconds."
  (dolist (replay (bt:with-lock-held (*replays-lock*)
                    (loop for r being the hash-values of *replays* collect r)))
    (when (> (- now (session-last-used replay)) lifetime)
      (drop-replay replay))))

(defun replay-door (req ent)
  "POST <prefix>/api/replay {id}: draw archived session ID -- its model
compiled again, if it is not already -- and answer the viewer's URL."
  (let* ((json (request-json req))
         (id (and json (gethash "id" json))))
    (cond ((not *browsing?*) (browsing-off req ent))
          ((not (archived-directory id)) (refuse req ent net.aserve:*response-not-found* "No such archived session."))
          (t (multiple-value-bind (replay text) (ensure-replay id)
               (if (null replay)
                   (refuse req ent "This session left no model to draw.")
                   (respond-json req ent (h "ok" (if (model-defined? replay) t 'yason:false)
                                            "text" (or text "")
                                            "viewer_url" (format nil "~a/viewer?replay=~a" *url-prefix* id)))))))))
