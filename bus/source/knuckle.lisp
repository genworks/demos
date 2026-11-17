;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :genworks.demos.bus)

(define-object knuckle (base-object)

  :input-slots
  (side
   tie-rod-length
   spindle-downward-pitch
   tie-rod-arm-length
   tie-rod-arm-setting
   (spindle-base-radius 2)
   (spindle-end-radius 1)
   (spindle-length 12)
   (tie-rod-arm-start-offset 3)
   (tie-rod-arm-end-local (make-point (ecase (the :tie-rod-arm-length)
					(:short 7)
					(:medium 10)
					(:long 13))
				      (ecase (the :tie-rod-arm-setting)
					(:narrow 5)
					(:medium 3)
					(:wide 0))
				      3))
   (tie-rod-arm-radius 1.5)
   (tie-rod-arm-end (the :tie-rod-arm :end)))

  :computed-slots
  ((display-controls (list :color :green-forest))
   (spindle-vector (the :spindle :direction-vector))
   (spindle-center (the :spindle :center))
   (tie-rod-arm-vector (the :tie-rod-arm :direction-vector))
   (tie-rod-arm-axis-point (inter-line-plane (the :center)
					     (the (:face-normal-vector :top))
					     (the :tie-rod-arm-end)
					     (the (:face-normal-vector :top)))))

  ;; The steering linkage's constraints, as geometry: the circle this
  ;; knuckle's tie-rod arm end sweeps about the kingpin axis, and the
  ;; sphere the far end of the tie rod must lie on.  The chassis
  ;; intersects one knuckle's circle with the other knuckle's sphere.
  ;; A circle lies in its own right/rear plane, and these inherit the
  ;; knuckle's orientation, so the circle's axis is the knuckle's :top.
  :hidden-objects
  ((tie-rod-arm-circle :type 'circle
		       :center (the :tie-rod-arm-axis-point)
		       :radius (3d-distance (the :tie-rod-arm-axis-point)
					    (the :tie-rod-arm-end)))
   (tie-rod-sphere :type 'sphere
		   :center (the :tie-rod-arm-end)
		   :radius (the :tie-rod-length)))

  :objects
  ((tie-rod-ball :type 'sphere
		 :center (the :tie-rod-arm-end)
		 :radius 2)
   (tie-rod-arm :type 'c-cylinder
		:start (translate (the :center) :up (the :tie-rod-arm-start-offset))
		:end (translate (the :center) 
				:right (get-x (the :tie-rod-arm-end-local)) 
				:rear (get-y (the :tie-rod-arm-end-local)) 
				:up (get-z (the :tie-rod-arm-end-local)))
		:radius (the :tie-rod-arm-radius))
   (spindle :type 'c-cylinder
	    :start (the :center)
	    :end (translate-along-vector 
		  (the :center)
		  (rotate-vector-d (the (:face-normal-vector :front))
				   (the :spindle-downward-pitch)
				   (ecase (the :side)
				     (:left (the (:face-normal-vector :right)))
				     (:right (the (:face-normal-vector :left)))))
		  (the :spindle-length))
	    :radius (the :spindle-end-radius))))
