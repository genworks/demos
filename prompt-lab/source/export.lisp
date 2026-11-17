;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; Downloads: the model as files to keep.  One door,
;; <prefix>/api/download?session=<id>&format=<name> (or replay=<id>),
;; builds (make-object 'model) with its default inputs and answers the
;; file as an attachment:
;;
;;   pdf   a sheet of four views, by the drawing system's pdf lens
;;   svg   one trimetric view, by its svg lens
;;   png   the same view rasterised in process (raster.lisp): no
;;         Ghostscript, as for the render tool
;;   step  the model's solids, on an engine with them (*engine* :solid)
;;   iges  the same
;;   stl   the same solids as one triangle mesh, in STL's text form
;;
;; STEP, IGES and STL carry solids only: a primitive of the free engine's
;; kind (box, cylinder, cone, sphere, torus, global-polygon-projection) is
;; written as the solid of the same shape, made for the purpose, and kept
;; only when its bounding box is the primitive's own; what is already a
;; solid is written as it is; anything else is left out and named in
;; the X-Prompt-Lab-Note header.  Hidden lines are removed in the
;; drawings up to *hidden-lines-max-leaves*, as in the viewer.
;;
;; Who may: whoever may see the session (visible-to?).  Building the
;; model is a run, metered as the viewer's draw is: for the owner only.
;;

(defparameter *download-formats*
  '(("pdf" "application/pdf" "pdf" "PDF drawing")
    ("svg" "image/svg+xml" "svg" "SVG drawing")
    ("png" "image/png" "png" "PNG picture")
    ("step" "model/step" "stp" "STEP solids")
    ("iges" "model/iges" "igs" "IGES solids")
    ("stl" "model/stl" "stl" "STL mesh"))
  "Each: the format's name on the address, its content type, the file's
extension, and the label the page shows.")

(defparameter *download-png-pixels* 2400
  "Integer. The long side of a downloaded PNG, in pixels.")

(defparameter *download-pdf-views* :standard-4view
  "Keyword. The drawing system's views-config for the PDF sheet.")

(defun solids-available? ()
  "Whether this engine writes solids: a brep type is defined."
  (let ((symbol (find-symbol (image-case "BREP") :surf)))
    (and symbol (find-class symbol nil) t)))

(defun download-formats ()
  "List of the formats this lab offers: the drawings everywhere, STEP,
IGES and STL where there are solids, PNG where the rasteriser is aboard."
  (remove-if #'(lambda (entry)
                 (let ((name (first entry)))
                   (or (and (member name '("step" "iges" "stl") :test #'string=)
                            (not (solids-available?)))
                       (and (string= name "png") (not (raster-available?))))))
             *download-formats*))

(defun downloads-state ()
  "List of hash tables for the state door: each offered format's name and label."
  (mapcar #'(lambda (entry) (h "format" (first entry) "label" (fourth entry)))
          (download-formats)))


;;
;; Solids for STEP and IGES.
;;

(defun surf-type (name)
  "The class named NAME in surf (the image's case), or nil on an engine
without it."
  (let ((symbol (find-symbol (image-case name) :surf)))
    (and symbol (find-class symbol nil))))

(defun same-box? (a b)
  "Whether objects A and B have the same bounding box, to a millionth of
its size."
  (let* ((box-a (the-object a bounding-box))
         (box-b (the-object b bounding-box))
         (tolerance (max 1d-9 (* 1d-6 (3d-distance (first box-a) (second box-a))))))
    (and (< (3d-distance (first box-a) (first box-b)) tolerance)
         (< (3d-distance (second box-a) (second box-b)) tolerance))))

(defun full-circle? (object)
  (< (abs (- (the-object object arc) (* 2 pi))) 1d-9))

(defun primitive-solid (leaf)
  "The solid of LEAF's shape for a free-engine primitive, or nil."
  (flet ((make (type &rest inputs)
           (let ((class (surf-type type)))
             (when class
               (apply #'make-object (class-name class)
                      :center (the-object leaf center)
                      :orientation (the-object leaf orientation)
                      :display-controls (the-object leaf display-controls)
                      inputs)))))
    (typecase leaf
      ;; a cone is a cylinder, so it is asked first
      (cone (when (full-circle? leaf)
              (make "CONE-SOLID"
                    :length (the-object leaf length)
                    :radius-1 (the-object leaf radius-1)
                    :radius-2 (the-object leaf radius-2)
                    :inner-radius-1 (the-object leaf inner-radius-1)
                    :inner-radius-2 (the-object leaf inner-radius-2))))
      (cylinder (when (full-circle? leaf)
                  (make "CYLINDER-SOLID"
                        :length (the-object leaf length)
                        :radius (the-object leaf radius)
                        :inner-radius (the-object leaf inner-radius))))
      (torus (when (full-circle? leaf)
               (make "TORUS-SOLID"
                     :major-radius (the-object leaf major-radius)
                     :minor-radius (the-object leaf minor-radius)
                     :inner-minor-radius (the-object leaf inner-minor-radius))))
      ;; surf has no sphere solid: a whole sphere's surface, sewn into a
      ;; closed brep
      (sphere (let ((surface (surf-type "SPHERICAL-SURFACE"))
                    (brep (surf-type "BREP-FROM-SURFACE")))
                (when (and surface brep
                           (null (the-object leaf inner-radius))
                           (full-circle? leaf)
                           (zerop (the-object leaf start-horizontal-arc))
                           (< (abs (- (the-object leaf end-horizontal-arc) (* 2 pi))) 1d-9)
                           (< (abs (+ (the-object leaf start-vertical-arc) (/ pi 2))) 1d-9)
                           (< (abs (- (the-object leaf end-vertical-arc) (/ pi 2))) 1d-9))
                  (make-object (class-name brep)
                               :surface (make-object (class-name surface)
                                                     :radius (the-object leaf radius)
                                                     :center (the-object leaf center)
                                                     :orientation (the-object leaf orientation))
                               :sew-and-orient-brep? t
                               :display-controls (the-object leaf display-controls)))))
      (global-polygon-projection
       (let ((curve (surf-type "B-SPLINE-CURVE"))
             (extruded (surf-type "EXTRUDED-SOLID")))
         (when (and curve extruded)
           (make-object (class-name extruded)
                        :profile (make-object (class-name curve)
                                              :control-points (the-object leaf vertex-list)
                                              :degree 1)
                        :axis-vector (the-object leaf projection-vector)
                        :distance (the-object leaf projection-depth)
                        :display-controls (the-object leaf display-controls)))))
      (box (make "BOX-SOLID"
                 :width (the-object leaf width)
                 :length (the-object leaf length)
                 :height (the-object leaf height))))))

(defun model-solids (model)
  "Values: the solids to write for MODEL's leaves, and the leaves left out."
  (let ((brep (surf-type "BREP")) solids left-out)
    (dolist (leaf (the-object model leaves))
      (let ((solid (cond ((and brep (typep leaf brep)) leaf)
                         (t (let ((made (ignore-errors (primitive-solid leaf))))
                              (and made (ignore-errors (same-box? leaf made)) made))))))
        (if solid (push solid solids) (push leaf left-out))))
    (values (nreverse solids) (nreverse left-out))))

(defun reference-of (object)
  "OBJECT's reference from its root, (the (legs 1) post): gwl's own where
the image has it (gendl since 2026-09-30), its root-path printed otherwise."
  (let ((gwl (find-symbol (image-case "ROOT-PATH-REFERENCE") :gwl)))
    (if (and gwl (fboundp gwl))
        (funcall gwl object)
        (format nil "~(~s~)" (cons 'the (reverse (the-object object root-path)))))))

(defun left-out-note (solids left-out)
  "A line saying which leaves a STEP or IGES file leaves out, or nil."
  (when left-out
    (format nil "~a of ~a parts written as solids; left out: ~{~a~^, ~}~@[ and ~a more~]"
            (length solids) (+ (length solids) (length left-out))
            (mapcar #'(lambda (leaf)
                        (format nil "~a (~(~a~))" (reference-of leaf)
                                (ignore-errors (the-object leaf type))))
                    (subseq left-out 0 (min 5 (length left-out))))
            (when (> (length left-out) 5) (- (length left-out) 5)))))


;;
;; Writing.
;;

(defun write-download (kind model file)
  "MODEL written in KIND (a name from *download-formats*) to FILE.
Values: true, and a note for the visitor or nil; nil and a reason when
there is nothing to write."
  (let ((hidden-lines (when (<= (length (the-object model leaves)) *hidden-lines-max-leaves*)
                        :remove)))
    (cond ((string= kind "pdf")
           (apply 'gdl-user::generate-multi-view-drawing
                  :format :pdf :output-file file :object-roots (list model)
                  :views-config *download-pdf-views*
                  (when hidden-lines (list :hidden-lines hidden-lines)))
           t)
          ((member kind '("svg" "png") :test #'string=)
           (let ((svg (if (string= kind "svg") file (make-pathname :type "svg" :defaults file))))
             (unwind-protect
                  (progn
                    (apply 'gdl-user::generate-single-view-drawing
                           :format :svg :output-file svg :object-roots (list model)
                           :projection-vector :trimetric
                           (when hidden-lines (list :hidden-lines hidden-lines)))
                    (when (string= kind "png")
                      (let ((png (render-svg-to-png (uiop:read-file-string svg :external-format :utf-8)
                                                    :max-pixels *download-png-pixels*)))
                        (with-open-file (out file :direction :output :if-exists :supersede
                                                  :element-type '(unsigned-byte 8))
                          (write-sequence png out))))
                    t)
               (when (and (string= kind "png") (probe-file svg))
                 (ignore-errors (delete-file svg))))))
          (t
           (multiple-value-bind (solids left-out) (model-solids model)
             (if (null solids)
                 (values nil (format nil "None of the model's ~a parts is a solid or the shape of one (a box, cylinder, cone, sphere, torus or extruded outline), so there is nothing to write as ~:@(~a~)."
                                     (length left-out) kind))
                 (progn
                   (cond ((string= kind "step")
                          (with-format (step file) (dolist (solid solids) (write-the-object solid cad-output))))
                         ((string= kind "iges")
                          (with-format (iges file) (dolist (solid solids) (write-the-object solid cad-output))))
                         (t
                          (with-format (stl file) (dolist (solid solids) (write-the-object solid cad-output)))))
                   ;; the stl lens warns and goes on when a solid does not
                   ;; tessellate, so a file may come out with no triangle
                   (if (and (string= kind "stl") (not (stl-facets? file)))
                       (values nil "None of the model's solids could be meshed, so there is nothing to write as STL.")
                       (values t (left-out-note solids left-out))))))))))

(defun stl-facets? (file)
  "Whether the text STL in FILE holds at least one triangle."
  (with-open-file (in file)
    (loop for line = (read-line in nil nil)
          while line
          thereis (search "facet normal" line))))


;;
;; The door.
;;

(defun source-download (req ent session key deployed?)
  "Answer SESSION's model file, header and all, to its owner (KEY): the
author may take their own source at any time, open or closed, and needs to
declare or unlock nothing for it.  Anyone else, and a deployed model's
viewer, is told there is no such session.  Not metered: nothing runs."
  (let ((file (and (not deployed?)
                   (if (session-replay? session)
                       ;; an archived work: its owner is the record's, and its file
                       ;; the archive's, not the replay's scratch copy
                       (let* ((directory (archived-directory (session-id session)))
                              (json (and directory (read-record (merge-pathnames "session.json" directory)))))
                         (and json (stringp key) (equal (gethash "owner" json) key)
                              (merge-pathnames "model.lisp" directory)))
                       (and (owner? session key) (session-model-file session))))))
    (cond ((null file) (no-such-session req ent))
          ((not (probe-file file)) (refuse req ent "No model has been written in this session yet."))
          (t (respond-deployment req ent (uiop:read-file-string file :external-format :utf-8)
                                 :attachment (format nil "prompt-lab-~a.lisp" (session-id session)))))))

(defun line-diff (old new)
  "The lines of NEW against OLD as a list of (:same|:add|:del . line), by
the longest common subsequence; nil when the two are too big to compare
(the record then gives the counts alone)."
  (let* ((a (coerce old 'vector)) (b (coerce new 'vector))
         (n (length a)) (m (length b)))
    (when (<= (* (1+ n) (1+ m)) 4000000)
      (let ((table (make-array (list (1+ n) (1+ m)) :element-type 'fixnum :initial-element 0)))
        (loop for i from (1- n) downto 0
              do (loop for j from (1- m) downto 0
                       do (setf (aref table i j)
                                (if (string= (aref a i) (aref b j))
                                    (1+ (aref table (1+ i) (1+ j)))
                                    (max (aref table (1+ i) j) (aref table i (1+ j)))))))
        (let ((i 0) (j 0) (out nil))
          (loop while (or (< i n) (< j m))
                do (cond ((and (< i n) (< j m) (string= (aref a i) (aref b j)))
                          (push (cons :same (aref a i)) out) (incf i) (incf j))
                         ((and (< j m) (or (>= i n) (>= (aref table i (1+ j)) (aref table (1+ i) j))))
                          (push (cons :add (aref b j)) out) (incf j))
                         (t (push (cons :del (aref a i)) out) (incf i))))
          (nreverse out))))))

(defparameter *writer-labels*
  '(("agent" . "the lab's agent")
    ("own-agent" . "the author's own agent, over MCP")
    ("author" . "the author, by hand")
    ("terminal" . "the author, by hand in the terminal")
    ("seed" . "a file the session started from")
    ("lab" . "the lab itself, declaring the results the request names"))
  "How the authorship record names each writer (archive.lisp *model-writer*).")

(defun authorship-versions (directory log)
  "The versions kept in archive DIRECTORY, oldest first, each a plist (:number
:file :writer :time :sha256 :inferred): from versions.jsonl where it has the
version, else inferred from LOG (the record's log) by the last edit or tool
call at or before the file's date."
  (let ((rows (make-hash-table)))
    (ignore-errors
     (with-open-file (in (merge-pathnames "versions.jsonl" directory) :external-format :utf-8 :if-does-not-exist nil)
       (when in
         (loop for line = (read-line in nil nil) while line
               do (let ((row (ignore-errors (yason:parse line))))
                    (when (and (hash-table-p row) (integerp (gethash "version" row)))
                      (setf (gethash (gethash "version" row) rows) row)))))))
    (loop for path in (sort (mapcar #'namestring (directory (merge-pathnames "model-*.lisp" directory))) #'string<)
          for number from 1
          collect (let ((row (gethash number rows))
                        (text (ignore-errors (uiop:read-file-string path :external-format :utf-8))))
                    (if row
                        (list :number number :file (file-namestring path) :writer (gethash "writer" row)
                              :time (gethash "time" row) :sha256 (gethash "sha256" row) :text text)
                        (let* ((date (ignore-errors (file-write-date path)))
                               (event (and date
                                           (find-if #'(lambda (e)
                                                        (and (consp e) (realp (first e)) (<= (first e) (+ date 2))
                                                             (member (second e) '("reload" "tool") :test #'equal)))
                                                    (reverse (coerce (or log '()) 'list)))))
                               (said (and event (third event))))
                          (list :number number :file (file-namestring path)
                                :writer (cond ((null event) "agent")
                                              ((equal (second event) "tool") "agent")
                                              ((and (stringp said) (search "Reloaded from disk" said)) "terminal")
                                              ((and (stringp said) (search "Started from a model file" said)) "seed")
                                              (t "author"))
                                :time date :sha256 (and text (watch-sha256-hex text)) :inferred t :text text)))))))

(defun authorship-text (id directory)
  "The authorship record of session ID from its archive DIRECTORY, Markdown:
the author's prompts, the uploads, each version with its writer, its hash
and its changes against the one before (hand edits marked), and the totals."
  (let* ((json (read-record (merge-pathnames "session.json" directory)))
         (log (and json (coerce (or (gethash "log" json) '()) 'list)))
         (versions (authorship-versions directory log))
         (totals (make-hash-table :test #'equal))
         (previous nil))
    (flet ((when-text (ut) (if (realp ut) (utc-time (round ut)) "time not kept"))
           (label (writer) (or (cdr (assoc writer *writer-labels* :test #'equal)) writer)))
      (with-output-to-string (out)
        (format out "# Authorship record: prompt-lab session ~a~%~%" id)
        (format out "What the author asked for, what the author wrote by hand and what an agent wrote, version by version, as the lab's archive keeps them; each version's SHA-256 is that of the file kept in the archive.  Writers marked 'inferred' are read from the session's log by time: the session predates the record kept with each version.~%~%")
        (format out "## The author's prompts~%~%")
        (let ((n 0))
          (dolist (e log)
            (when (and (consp e) (equal (second e) "prompt"))
              (format out "~d. ~a -- ~a~%" (incf n) (when-text (first e)) (third e))))
          (when (zerop n) (format out "None.~%")))
        (format out "~%## Files the author uploaded~%~%")
        (let ((any nil))
          (dolist (e log)
            (when (and (consp e) (equal (second e) "note") (stringp (third e)) (eql 0 (search "Uploaded " (third e))))
              (setq any t)
              (format out "- ~a -- ~a~%" (when-text (first e)) (third e))))
          (unless any (format out "None.~%")))
        (format out "~%## Versions~%")
        (dolist (v versions)
          (let* ((writer (getf v :writer))
                 (hand? (member writer '("author" "terminal") :test #'equal))
                 (lines (and (stringp (getf v :text)) (uiop:split-string (getf v :text) :separator '(#\Newline))))
                 (diff (line-diff (or previous '()) (or lines '())))
                 (added (count :add diff :key #'car))
                 (removed (count :del diff :key #'car)))
            (incf (gethash writer totals 0) added)
            (format out "~%### Version ~d -- ~a -- ~:[~;HAND EDIT: ~]~a~:[~; (inferred)~]~%~%"
                    (getf v :number) (when-text (getf v :time)) hand? (label writer) (getf v :inferred))
            (format out "File ~a, SHA-256 ~a.  ~d line~:p added, ~d removed against the version before.~%"
                    (getf v :file) (or (getf v :sha256) "not kept") added removed)
            (when (and diff (or (plusp added) (plusp removed)))
              (format out "~%```diff~%")
              (let ((shown 0))
                (dolist (d diff)
                  (unless (eq (car d) :same)
                    (when (< shown 400)
                      (format out "~a ~a~%" (if (eq (car d) :add) "+" "-") (cdr d)))
                    (incf shown)))
                (when (> shown 400) (format out "... ~d more changed lines~%" (- shown 400))))
              (format out "```~%"))
            (setq previous lines)))
        (when (null versions) (format out "~%No version of the model file was kept.~%"))
        (format out "~%## Totals~%~%Lines added, by writer, over every version:~%~%")
        (maphash #'(lambda (writer n) (format out "- ~a: ~d~%" (label writer) n)) totals)))))

(defun authorship-download (req ent session key deployed?)
  "Answer SESSION's authorship record to its owner (KEY), as the source
download does; anyone else, or a deployed model's viewer, is told there is
no such session."
  (let* ((replay? (session-replay? session))
         (directory (cond (deployed? nil)
                          (replay? (archived-directory (session-id session)))
                          (t (archive-directory session))))
         (json (and directory (read-record (merge-pathnames "session.json" directory))))
         (owner? (if replay?
                     (and json (stringp key) (equal (gethash "owner" json) key))
                     (owner? session key))))
    (cond ((not (and directory owner?)) (no-such-session req ent))
          ((not (probe-file (merge-pathnames "session.json" directory)))
           (refuse req ent "This session has no record in the archive yet."))
          (t (respond-deployment req ent (authorship-text (session-id session) directory)
                                 :attachment (format nil "authorship-~a.md" (session-id session)))))))

(defun download-door (req ent)
  "GET <prefix>/api/download?session=<id>&format=<name> (or replay=<id>
for an archived session's replay; owner=<key> for a private one; or
deployed=<name> for a deployed model, with grant=<id> or the deployment's
cookie for a file that was paid for): the model as a file.  400 for a
format not offered, 402 for a deployed model's priced file without a
grant, 404 for no such session or no model, 422 when the format has
nothing to write (STEP of a model with no solids), 429 when the owner's
credits are spent.  A grant is spent when its file is served."
  (let* ((id (query-value req "session"))
         (replay (query-value req "replay"))
         (key (request-owner-key req))
         ;; deployed=<name>: a deployed model, for whoever may use it (deploy.lisp)
         (deployed (query-value req "deployed"))
         (session (cond ((stringp id) (find-session id))
                        ((stringp replay) (find-replay replay))
                        ((stringp deployed) (deployed-for-viewer deployed))))
         (kind (string-downcase (or (query-value req "format") "")))
         (entry (assoc kind (download-formats) :test #'string=))
         (record (and session (stringp deployed) (deployment-record deployed)))
         (grant (and record (request-grant req deployed)))
         ;; the file is served on a grant bought for it: spent when it goes out
         (on-grant? (and record (deployment-file-price record kind)
                         (not (and (stringp (or key (query-value req "owner")))
                                   (equal (gethash "owner" record) (or key (query-value req "owner")))))
                         (grant-admits? grant deployed kind))))
    (cond ((or (null session) (not (visible-to? session key))) (no-such-session req ent))
          ;; the model file itself: its author's, at any time, open or
          ;; closed, with nothing to unlock (the user's ruling, 2026-11-12)
          ((equal kind "source") (source-download req ent session key (stringp deployed)))
          ;; the authorship record: prompts, versions, writers, hashes (ruling 96)
          ((equal kind "authorship") (authorship-download req ent session key (stringp deployed)))
          ((null entry) (refuse req ent "No such download here: ~a." kind))
          ;; a deployed model's priced download: its owner's, or bought on its page
          ((and record (not (deployment-admits? record kind (or key (query-value req "owner")) grant)))
           (refuse req ent (net.aserve::make-resp 402 "Payment Required")
                   "This file costs ~a: buy it on the model's page."
                   (money-text (* (deployment-file-price record kind) *unit-cents*))))
          ((not (model-defined? session)) (refuse req ent "No model has been built in this session yet."))
          (t
           ;; a deployed model's file is its author's compute (deploy.lisp, the hosting meter)
           (when (and record (not (meter-deployment! deployed :run (model-volume session))))
             (return-from download-door
               (refuse req ent net.aserve:*response-service-unavailable* "~a is paused: its hosting is spent." deployed)))
           (when (owner? session key)
             (multiple-value-bind (ok? reason) (meter! session :run (model-volume session))
               (unless ok?
                 (return-from download-door
                   (refuse req ent *response-too-many-requests* "~a" reason)))))
           (let ((file (merge-pathnames (format nil "download-~a.~a" (random 1000000000) (third entry))
                                        (uiop:temporary-directory))))
             (unwind-protect
                  (multiple-value-bind (ok? note)
                      (handler-case
                          (with-time-limit (*render-seconds* "download")
                            (let ((*package* (session-package session)))
                              (write-download kind (make-model session) file)))
                        ;; a stack overflow too, answered as a 422 (agent.lisp)
                        (serious-condition (condition) (values nil (format nil "The model did not write: ~a" condition))))
                    (if (not ok?)
                        (refuse req ent (net.aserve::make-resp 422 "Unprocessable Entity") "~a" note)
                        (progn
                          (when on-grant? (spend-grant! grant))
                          (net.aserve:with-http-response (req ent :content-type (second entry) :format :binary)
                            (setf (net.aserve:reply-header-slot-value req :content-disposition)
                                  ;; a deployment's file is named for it, nothing of the lab's
                                  (format nil "attachment; filename=\"~a.~a\""
                                          (if record deployed (format nil "prompt-lab-~a" (session-id session)))
                                          (third entry)))
                            (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
                            (when note
                              (setf (net.aserve:reply-header-slot-value req :x-prompt-lab-note)
                                    (remove-if-not #'(lambda (c) (< 31 (char-code c) 127)) note)))
                            (net.aserve:with-http-body (req ent)
                              (with-open-file (in file :element-type '(unsigned-byte 8))
                                (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
                                      (out (net.aserve:request-reply-stream req)))
                                  (loop for count = (read-sequence buffer in)
                                        while (plusp count)
                                        do (write-sequence buffer out :end count)))))))))
               (when (probe-file file) (ignore-errors (delete-file file)))))))))
