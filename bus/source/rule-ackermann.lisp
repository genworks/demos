;; Copyright (C) 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :genworks.demos.bus)


(define-object rule-ackermann (gwl-rule-object)

  :input-slots
  (ackermann-data-ideal
   ackermann-data)


  :computed-slots
  ((rule-title "Ackermann")
   (rule-description "Each right turn angle plotted against the corresponding
left turn angle should follow a certain relationship to aid in vehicle handling
and avoid tire scrub. In the diagram below, the red curve indicates actual Left
wheel turn-angles plotted against the corresonding Right wheel turn angle,
and the green curve indicates the Ideal according to Ackermann's rule.")
   (violated? nil))

  :hidden-objects
  ((graph :type 'graph
          :x-values (list (getf (the ackermann-data-ideal) :left)
                          (getf (the ackermann-data) :left))
          :y-values (list (getf (the ackermann-data-ideal) :right)
                          (getf (the :ackermann-data) :right))
          :colors (list :green :red)))

  :hidden-objects
  ((view-object :type 'web-drawing
                :object-roots (list (the :graph))
                :page-width 500
                :page-length 500)))
