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

(defun download-door (req ent)
  "GET <prefix>/api/download?session=<id>&format=<name> (or replay=<id>
for an archived session's replay; owner=<key> for a private one): the
model as a file.  400 for a format not offered, 404 for no such session
or no model, 422 when the format has nothing to write (STEP of a model
with no solids), 429 when the owner's credits are spent."
  (let* ((id (query-value req "session"))
         (replay (query-value req "replay"))
         (key (request-owner-key req))
         ;; deployed=<name>: a deployed model, for whoever may use it (deploy.lisp)
         (deployed (query-value req "deployed"))
         (session (cond ((stringp id) (find-session id))
                        ((stringp replay) (find-replay replay))
                        ((stringp deployed)
                         (deployed-for-viewer deployed (or key (query-value req "owner"))))))
         (kind (string-downcase (or (query-value req "format") "")))
         (entry (assoc kind (download-formats) :test #'string=)))
    (cond ((or (null session) (not (visible-to? session key))) (no-such-session req ent))
          ((null entry) (refuse req ent "No such download here: ~a." kind))
          ((not (model-defined? session)) (refuse req ent "No model has been built in this session yet."))
          (t
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
                        (error (condition) (values nil (format nil "The model did not write: ~a" condition))))
                    (if (not ok?)
                        (refuse req ent (net.aserve::make-resp 422 "Unprocessable Entity") "~a" note)
                        (net.aserve:with-http-response (req ent :content-type (second entry) :format :binary)
                          (setf (net.aserve:reply-header-slot-value req :content-disposition)
                                (format nil "attachment; filename=\"prompt-lab-~a.~a\""
                                        (session-id session) (third entry)))
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
                                      do (write-sequence buffer out :end count))))))))
               (when (probe-file file) (ignore-errors (delete-file file)))))))))
