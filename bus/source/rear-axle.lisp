;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :genworks.demos.bus)

(define-object rear-axle (base-object)

  :computed-slots
  ((wheel-centers (list :left
			(translate (the :center) :front (half (the :length)))
			:right
			(translate (the :center) :rear (half (the :length))))))

  :objects
  ((center-sphere :type 'sphere
		  :radius 9)
   (cylinder :type 'c-cylinder
	     :start (getf (the :wheel-centers) :left)
	     :end (getf (the :wheel-centers) :right)
	     :radius 2.5)))
