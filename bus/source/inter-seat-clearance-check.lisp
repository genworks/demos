;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :genworks.demos.bus)

(define-object inter-seat-clearance-check (gwl-rule-object)

  :input-slots
  (inter-seat-spacing
   clearance-extent-typical
   value)

  :computed-slots
  ((rule-title "Legroom")
   (rule-description "Distance from front of one seat to back of the seat fore of it.")
   (rule-result (number-format (the result) 2))
   (violated? (< (the result) (the value)))
   (result (- (the inter-seat-spacing)
              (the clearance-extent-typical)))))
