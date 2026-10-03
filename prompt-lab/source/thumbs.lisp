;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; What the archive listing shows of each session beyond its prompt: a
;; THUMBNAIL of the model, and whether the model wants SOLIDS.
;;
;; Thumbnails.  Each archive directory may hold thumb.png, a small line
;; drawing of the model as its last version builds with default inputs
;; (trimetric, hidden lines removed up to *hidden-lines-max-leaves*),
;; rasterised in process (raster.lisp).  A thread of each room's own
;; (start-thumbnailer!) keeps them current: every *thumb-interval*
;; seconds it walks the archive, newest first, and draws one thumbnail
;; at a time for every record built on THIS room's engine whose
;; thumb.png is missing or older than its model.lisp.  The model is
;; compiled into a package of its own, as a replay is, and the package
;; is deleted afterwards; nothing is metered.  A model that does not
;; build leaves thumb.failed, so it is not tried again until its model
;; file changes.  The thumb door serves the file to whoever may see the
;; record.
;;
;; Solids.  Every listed session says which of three it is:
;;   "needs"     the model is built from brep solids (box-solid,
;;               subtracted-solid ...) and runs only on an engine with them
;;   "benefits"  it runs without them but is better with them: it carries
;;               its own switch (a dual model, solids? read from the image),
;;               or the agent said what the free engine could not do (holes
;;               drawn, not cut; parts touching, not joined)
;;   "none"      it runs without them and they would add nothing
;; read from the model file and the agent's replies (solids-class), or
;; from a "solids" field in the record, which wins.
;;

(defparameter *thumbnails?* t
  "Boolean. Whether this room draws thumbnails for the archive.")

(defparameter *thumb-pixels* 320
  "Integer. The long side of a thumbnail, in pixels.")

(defparameter *thumb-seconds* 90
  "Integer. The most one thumbnail may take: compile, build and draw.")

(defparameter *thumb-pause* 2
  "Real. Seconds between two thumbnails, so visitors keep the processor.")

(defparameter *thumb-interval* 120
  "Integer. Seconds between two walks of the archive.")

(defvar *thumb-thread* nil)


;;
;; Solids.
;;

(defparameter *solid-types* '("brep-from-surface")
  "Type names that are brep solids without ending in -solid.")

(defun solid-types-named (source)
  "The brep solid types SOURCE makes: every quoted name ending in -solid,
and the few in *solid-types*."
  (let ((source (string-downcase source)) types)
    (loop for start = (position #\' source) then (position #\' source :start end)
          for end = (and start (or (position-if-not #'(lambda (c) (or (alphanumericp c) (find c "-:")))
                                                    source :start (1+ start))
                                   (length source)))
          while start
          do (let ((name (string-left-trim ":" (subseq source (1+ start) end))))
               (let ((colon (position #\: name :from-end t)))
                 (when colon (setq name (subseq name (1+ colon)))))
               (when (or (and (> (length name) 6) (string= "-solid" name :start2 (- (length name) 6)))
                         (member name *solid-types* :test #'string=))
                 (pushnew name types :test #'string=))))
    types))

(defparameter *solids-wanted-phrases*
  '("not cut" "can't be cut" "cannot be cut" "can not be cut" "not actually" "isn't actually"
    "no boolean" "without boolean" "boolean operation" "drawn but" "drawn, not"
    "no solid model" "solid modelling" "solid modeling" "not joined" "touching")
  "What the agent says on the free engine when a model wants what only
solids give: holes drawn but not cut, parts shown touching.")

(defun agent-text (log)
  "The agent's own words in a record's LOG, lower case."
  (string-downcase
   (format nil "~{~a~^ ~}"
           (loop for entry in log
                 when (and (listp entry) (= (length entry) 3)
                           (member (string (second entry)) '("text" "done") :test #'string-equal)
                           (stringp (third entry)))
                   collect (third entry)))))

(defun solids-class (source log)
  "\"needs\", \"benefits\" or \"none\" for a model whose file is SOURCE and
whose record's log is LOG; nil without a model."
  (when (and (stringp source) (plusp (length source)))
    (let ((types (solid-types-named source))
          (switch? (or (search ":smlib" source :test #'char-equal)
                       (search "solids?" source :test #'char-equal))))
      (cond ((and types switch?) "benefits")
            (types "needs")
            ((let ((text (agent-text log)))
               (some #'(lambda (phrase) (search phrase text)) *solids-wanted-phrases*))
             "benefits")
            (t "none")))))

(defun record-solids (json model-file)
  "The solids class of an archived record: its own \"solids\" field when it
names one, else what solids-class reads from the model file and the log."
  (let ((given (gethash "solids" json)))
    (if (member given '("needs" "benefits" "none") :test #'equal)
        given
        (solids-class (and (probe-file model-file)
                           (ignore-errors (uiop:read-file-string model-file :external-format :utf-8)))
                      (gethash "log" json)))))


;;
;; Thumbnails.
;;

(defun thumb-file (directory) (merge-pathnames "thumb.png" directory))

(defun thumb-failed-file (directory) (merge-pathnames "thumb.failed" directory))

(defun file-date (file) (and (probe-file file) (ignore-errors (file-write-date file))))

(defun thumb-stale? (directory)
  "True when DIRECTORY's model is newer than its thumbnail and than its
last failure to draw one."
  (let ((model (file-date (merge-pathnames "model.lisp" directory))))
    (and model
         (let ((thumb (file-date (thumb-file directory)))
               (failed (file-date (thumb-failed-file directory))))
           (and (or (null thumb) (< thumb model))
                (or (null failed) (< failed model)))))))

(defun model-thumbnail (session)
  "PNG octets: SESSION's model drawn small."
  (let* ((*package* (session-package session))
         (model (make-model session))
         (svg (merge-pathnames "thumb.svg" (session-directory session)))
         (hidden (<= (length (the-object model leaves)) *hidden-lines-max-leaves*)))
    (unwind-protect
         (progn
           (apply 'gdl-user::generate-single-view-drawing
                  :format :svg :output-file svg :object-roots (list model)
                  :projection-vector :trimetric
                  (when hidden (list :hidden-lines :remove)))
           (render-svg-to-png (uiop:read-file-string svg :external-format :utf-8)
                              :max-pixels *thumb-pixels* :fit t))
      (when (probe-file svg) (ignore-errors (delete-file svg))))))

(defun write-thumbnail! (directory)
  "Draw DIRECTORY's model into its thumb.png, or note in thumb.failed why
it could not be.  Values: true when drawn."
  (let* ((json (read-record (merge-pathnames "session.json" directory)))
         (id (and json (gethash "id" json)))
         (source (file-model-body (merge-pathnames "model.lisp" directory))))
    (when (and (stringp id) (plusp (length source)))
      (let* ((keyword (intern (string-upcase (format nil "pl-thumb-~a" id)) :keyword))
             (scratch (merge-pathnames (format nil "thumb-~a/" id) *replay-root*))
             (session nil))
        (unwind-protect
             (handler-case
                 (with-time-limit (*thumb-seconds* "thumbnail")
                   (let ((old (find-package keyword))) (when old (delete-package old)))
                   (eval `(gdl:define-package ,keyword))
                   (setq session (make-session-internal
                                  :id id :package-name (package-name (find-package keyword)) :replay? t
                                  :created (get-universal-time) :directory scratch))
                   (ensure-directories-exist scratch)
                   ;; a web app's file compiles in a package opened to GWL (kinds.lisp)
                   (set-session-kind! session (if (equal (gethash "kind" json) "app") :app :model))
                   (multiple-value-bind (blocks error?) (write-model session source)
                     (when error?
                       (error "the model does not compile: ~a"
                              (or (cdr (assoc "text" (first blocks) :test #'string=)) ""))))
                   (let ((png (model-thumbnail session))
                         (tmp (merge-pathnames "thumb.tmp" directory)))
                     (with-open-file (out tmp :direction :output :if-exists :supersede
                                              :element-type '(unsigned-byte 8))
                       (write-sequence png out))
                     (when (probe-file (thumb-file directory)) (delete-file (thumb-file directory)))
                     (rename-file tmp (thumb-file directory))
                     t))
               (error (condition)
                 (ignore-errors
                  (write-text-file (format nil "~a~%" condition) (thumb-failed-file directory)))
                 nil))
          (let ((package (find-package keyword)))
            (when package (ignore-errors (delete-package package))))
          (ignore-errors (uiop:delete-directory-tree (pathname scratch) :validate t
                                                     :if-does-not-exist :ignore)))))))

(defun thumbnail-directories ()
  "The archive directories this room should draw: built on its engine,
model newer than the thumbnail, newest first."
  (when *archive-root*
    (let ((here (engine-name)) found)
      (dolist (record (directory (merge-pathnames "*/*/session.json" *archive-root*)))
        (let* ((directory (make-pathname :name nil :type nil :defaults record))
               (json (and (thumb-stale? directory) (read-record record))))
          (when (and json (equal (or (gethash "engine" json) "gendl") here))
            (push (cons (or (file-date record) 0) directory) found))))
      (mapcar #'cdr (sort found #'> :key #'car)))))

(defun draw-thumbnails! ()
  "Draw every stale thumbnail of this room's engine, one at a time.
Values: how many were drawn and how many failed."
  (let ((drawn 0) (failed 0))
    (dolist (directory (thumbnail-directories))
      (if (write-thumbnail! directory) (incf drawn) (incf failed))
      (sleep *thumb-pause*))
    (values drawn failed)))

(defun start-thumbnailer! ()
  "Keep the archive's thumbnails current in a thread of their own; a no-op
when one runs, when thumbnails are off, or when there is no rasteriser."
  (when (and *thumbnails?* *archive-root* (raster-available?)
             (not (and *thumb-thread* (bt:thread-alive-p *thumb-thread*))))
    (setf *thumb-thread*
          (bt:make-thread #'(lambda ()
                              (loop (handler-case (draw-thumbnails!)
                                      (error (condition)
                                        (format *error-output* "~&prompt-lab thumbnails: ~a~%" condition)))
                                    (sleep *thumb-interval*)))
                          :name "prompt-lab thumbnails")))
  *thumb-thread*)

(defun stop-thumbnailer! ()
  (when (and *thumb-thread* (bt:thread-alive-p *thumb-thread*))
    (bt:destroy-thread *thumb-thread*))
  (setf *thumb-thread* nil))

(defun thumb-door (req ent)
  "GET <prefix>/api/thumb?id=<id>: the archived session's thumbnail, a PNG,
to whoever may see the record; 404 otherwise or when none is drawn yet."
  (let* ((id (query-value req "id"))
         (directory (and *browsing?* (archived-directory id)))
         (json (and directory (read-record (merge-pathnames "session.json" directory))))
         (file (and json (record-visible? json (request-owner-key req))
                    (probe-file (thumb-file directory)))))
    (if (null file)
        (refuse req ent net.aserve:*response-not-found* "No thumbnail.")
        (net.aserve:with-http-response (req ent :content-type "image/png" :format :binary)
          ;; the listing dates the address (&v=), so a kept copy is never stale
          (setf (net.aserve:reply-header-slot-value req :cache-control) "public, max-age=86400")
          (net.aserve:with-http-body (req ent)
            (write-sequence (alexandria:read-file-into-byte-vector file)
                            (net.aserve:request-reply-stream req)))))))
