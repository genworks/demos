;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :genworks.demos.bus)

(define-object interior (application-mixin)

  :input-slots
  (firewall-base
   length
   width
   height

   (number-of-rows 10 :settable)
   (minimum-inter-seat-clearance 7 :settable))


  :computed-slots
  ((ui-display-list-objects (list (the :sections)))

   (reclined-angle 20 :settable)
   (max-reclined-angle 30 :settable)
   (use-local-box? nil))

  :objects
  ((sections :type 'seating-section
             :body-reference-points
             (list :left
                   (translate (the :firewall-base) :front
                              (half (the :length)) :right
                              (the :width))
                   :right
                   (translate (the :firewall-base) :rear
                              (half (the :length)) :right
                              (the :width)))
             :usable-cabin-width (the :width)
             :pass-down (number-of-rows reclined-angle max-reclined-angle
                         minimum-inter-seat-clearance))))
