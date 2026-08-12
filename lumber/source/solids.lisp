;;;; -*- Mode: Lisp; Package: lumber -*-

;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;;
;;;; Solid (brep) lumber variants for surf/SMLib-capable images.
;;;; This file references surf:extruded-solid at read time, so it is
;;;; loaded only via solids-loader.lisp, which checks the running
;;;; image first.  Profiles are composed linear curves (there is no
;;;; unfilleted global-polyline-curve, and the filleted one rejects
;;;; a zero radius).
;;;;
;;;; lumber-type (lumber.gdl) returns these types when asked for
;;;; :solids? t on an image where they are defined.

(in-package :lumber)

(define-object profile-curve (surf:composed-curve)

  :documentation
  (:description "Closed profile curve through a list of corner
points, composed of linear segments.  Input corners: consecutive
distinct points; the closing segment back to the first corner is
implied."
   :author "Genworks International")

  :input-slots
  (corners)

  :computed-slots
  ((curves (list-elements (the segments))))

  :hidden-objects
  ((segments :type 'surf:linear-curve
             :sequence (:size (length (the corners)))
             :start (nth (the-child index) (the corners))
             :end (nth (mod (1+ (the-child index)) (length (the corners)))
                       (the corners)))))


(define-object lumber-solid (surf:box-solid lumber-mixin)

  :documentation
  (:description "Solid-brep square-cut board: a native brep box with
the cross-section from the nominal size.  Same inputs and
stock-keeping as lumber."
   :author "Genworks International"))


(define-object angle-cut-lumber-solid (surf:extruded-solid angle-cut-mixin lumber-mixin)

  :documentation
  (:description "Solid-brep angle-cut board: the angle-cut-mixin
profile extruded through the board as a brep.  Same inputs and
stock-keeping as angle-cut-lumber."
   :author "Genworks International")

  :computed-slots
  ((profile (the profile-shape))
   (axis-vector (the projection-vector))
   (distance (the projection-depth)))

  :hidden-objects
  ((profile-shape :type 'profile-curve
                  :corners (butlast (the vertex-list)))))
