;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(gwl:define-package :gear
  (:documentation "Involute spur gears as exact NURBS geometry: the
tooth flank from the base circle, the root fillet as the trochoid the
ISO 53 rack tip generates, profile shift, backlash; one closed profile
per gear, a meshing pair at its working center distance, and the
numbers a drawing needs.")
  (:export #:gear-numbers #:pair-numbers #:gear-profile-segments #:gear-report
           #:gear-profile #:meshed-mate #:involute-function #:undercut-shift-minimum
           #:publish-gear! #:respond-with-gear-export #:parse-gear-request #:gear-family))
