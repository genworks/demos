;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :genworks.demos.bus)

(define-object frame (base-object)

  :input-slots
  (thickness)

  :objects
  ((rails :type 'frame-rail
	  :sequence (:size 2)
	  :center (translate (the :center)
			     (ecase (the-child :index) (0 :front) (1 :rear))
			     (- (half (the :length)) (half (the :thickness))))
	  :length (the :thickness))))
