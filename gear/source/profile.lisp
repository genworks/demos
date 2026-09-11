;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; The gear as NURBS: one tooth period's segments become curves
;;;; (fitted through the involute and trochoid samples, true arcs for
;;;; the lands), every other tooth is those curves rotated about the
;;;; center, and the whole closed outline is one composed-curve that
;;;; writes itself out as STEP, IGES or DXF -- and extrudes into the
;;;; solid on the enterprise backend.

(in-package :gear)

(define-object gear-profile (base-object)

  :documentation
  (:description "One involute spur gear as a closed planar NURBS
outline in the XY plane, centered at the origin, one tooth on the +x
axis.  Inputs are the ISO 21771 quantities; the derived numbers are
in (the numbers) and (the report)."
   :author "Genworks International")

  :input-slots
  ((module 2d0 :settable)
   (teeth 20 :settable)
   (pressure-angle 20d0 :settable)
   (shift 0d0 :settable)
   (addendum 1d0 :settable)
   (dedendum 1.25d0 :settable)
   (fillet 0.38d0 :settable)
   (backlash 0d0 :settable)
   (involute-points 24 :settable)
   (fillet-points 12 :settable)
   ;; the pair's mate, when this gear is drawn at a working center
   ;; distance from another: nil, or the mate's tooth count
   (mate-teeth nil :settable)
   (mate-shift 0d0 :settable)
   ;; a face width makes the gear a solid (the outline extruded along
   ;; +z); nil keeps it a planar outline
   (face-width nil :settable))

  :computed-slots
  ((numbers (gear-numbers :module (the module) :teeth (the teeth)
                          :pressure-angle (the pressure-angle) :shift (the shift)
                          :addendum (the addendum) :dedendum (the dedendum)
                          :fillet (the fillet) :backlash (the backlash)))

   (pair (when (the mate-teeth)
           (append (list :mate-teeth (the mate-teeth))
                   (pair-numbers (the numbers)
                                 (gear-numbers :module (the module) :teeth (the mate-teeth)
                                               :pressure-angle (the pressure-angle)
                                               :shift (the mate-shift)
                                               :addendum (the addendum) :dedendum (the dedendum)
                                               :fillet (the fillet) :backlash (the backlash))))))

   (report (gear-report (the numbers) (the pair)))

   (segments (gear-profile-segments (the numbers)
                                    :involute-points (the involute-points)
                                    :fillet-points (the fillet-points)))

   (pitch-angle (getf (the numbers) :pitch-angle))

   ;; every tooth's curves in order, counter-clockwise around the gear
   (all-curves (loop for tooth in (list-elements (the teeth-curves))
                     append (the-object tooth ordered)))

   (outline-objects (list (the outline)))

   ;; what the CAD export writes: the solid when there is a face
   ;; width, else the outline
   (cad-objects (if (the face-width) (list (the solid)) (list (the outline)))))

  :objects
  ((tooth-0 :type 'tooth-curves
            :segments (the segments))

   (teeth-curves :type 'rotated-tooth
                 :sequence (:size (the teeth))
                 :source (the tooth-0)
                 :angle (* (the-child index) (the pitch-angle)))

   (outline :type 'composed-curve
            :curves (the all-curves)))

  :hidden-objects
  ((solid :type 'extruded-solid
          :display-controls (list :color :steel-blue)
          :profile (the outline)
          :axis-vector (make-vector 0 0 1)
          :distance (or (the face-width) 1))))

(define-object tooth-curves (base-object)
  :documentation (:description "The curves of one tooth period, from
the sampled segments: fitted curves through the fillet and flank
samples, true arcs for the two lands; (the ordered) lists the six in
profile order.")
  :input-slots (segments)
  :computed-slots
  ((point-segments (remove-if-not (lambda (s) (eq (getf s :kind) :points)) (the segments)))
   (arc-segments (remove-if-not (lambda (s) (eq (getf s :kind) :arc)) (the segments)))
   ;; lower fillet, lower flank, tip land, upper flank, upper fillet, root land
   (ordered (list (the (fitted 0)) (the (fitted 1)) (the (arcs 0))
                  (the (fitted 2)) (the (fitted 3)) (the (arcs 1)))))
  :objects
  ((fitted :type 'fitted-curve
           :sequence (:size (length (the point-segments)))
           :points (mapcar (lambda (p) (make-point (first p) (second p) 0d0))
                           (getf (nth (the-child index) (the point-segments)) :points))
           :degree 3)
   (arcs :type 'arc-curve
         :sequence (:size (length (the arc-segments)))
         :center (make-point 0 0 0)
         :radius (getf (nth (the-child index) (the arc-segments)) :radius)
         :start-angle (getf (nth (the-child index) (the arc-segments)) :start)
         :end-angle (getf (nth (the-child index) (the arc-segments)) :end))))

(define-object rotated-tooth (base-object)
  :documentation (:description "One tooth's curves rotated about the
gear center by ANGLE (radians): boxed-curves with a rotated
orientation, the same curve objects underneath.")
  :input-slots (source angle)
  :computed-slots
  ((rotation (alignment :rear (rotate-vector-d (make-vector 0 1 0)
                                               (rad->deg (the angle))
                                               (make-vector 0 0 1))
                        :top (make-vector 0 0 1)))
   (ordered (list-elements (the curves))))
  :objects
  ((curves :type 'boxed-curve
           :sequence (:size (length (the source ordered)))
           :curve-in (nth (the-child index) (the source ordered))
           :orientation (the rotation)
           :orientation-center (make-point 0 0 0))))
