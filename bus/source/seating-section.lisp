;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :genworks.demos.bus)

(define-object seating-section (base-object)

  :input-slots
  (fare-class
   usable-cabin-width
   body-reference-points
   max-reclined-angle
   number-of-rows
   minimum-inter-seat-clearance

   (use-local-box? nil)
   )

  :objects
  ((inter-seat-spacing-computation
    :type 'inter-seat-spacing
    :length 0 :width 0 :height 0
    :pass-down (:max-reclined-angle
                :usable-cabin-width :number-of-rows))

   (inter-seat-clearance-check
    :type 'inter-seat-clearance-check
    :inter-seat-spacing (the inter-seat-spacing-computation result)
    :clearance-extent-typical (the inter-seat-spacing-computation
                                clearance-extent-typical)
    :length 0 :width 0 :height 0
    :value (the minimum-inter-seat-clearance))

   (sides :type 'seating-side
          :fare-class (the fare-class)
          :sequence (:size 2)
          :side (ecase (the-child index) (0 :left) (1 :right))
          :display-controls (list :color :green)
          :body-reference-point (getf (the body-reference-points)
                                      (the-child side))
          :pass-down (:number-of-rows :reclined-angle)
          :inter-seat-spacing
          (the inter-seat-spacing-computation result)
          :x-max-typical
          (the inter-seat-spacing-computation x-max-typical)
          :x-vector (the (face-normal-vector :right)))))
