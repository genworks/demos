;; Copyright (C) 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :pamphlet)

(define-object bus (base-object)
  
  :objects
  ((bus :type 'bus:assembly :turn-angle -30)
   
   (robot :type 'robot:assembly
	  :width 25 :length 15 :height 55
	  :center (translate (the center) :rear 150
			     :right 250))

   
   (robot-2 :type 'robot:assembly
	    :width 25 :length 15 :height 55
	    :center (translate (the center) :rear 180
			       :right 340)
	    
	    :orientation (alignment :right (the (face-normal-vector :left))
				    :top (the (face-normal-vector :top)))
   
	    :arm-angle-right -90)
   
   (robot-3 :type 'robot:assembly
	    :width 25 :length 15 :height 55
	    :center (translate (the center) :rear 130
			       :right 320)
	    
	    :arm-angle-right -90)))
   
   
   
