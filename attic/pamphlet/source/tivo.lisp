;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :pamphlet)

#|

(define-object assembly (application-mixin)
  
  :computed-slots
  ((display-rules? nil)
   (display-tree? nil)
   (graphics-width (* 11 72))
   (graphics-height (* 8.5 72))
   
   (ui-display-list-leaves (the side-1))
   (view :top)
   
   )
  
  :hidden-objects
  ((side-1 :type 'side-1)))


(define-object side-1 (base-drawing)
  
  :computed-slots
  (
   ;;(fit-to-page? nil)
   (page-width (* 11 72))
   (page-length (* 8.5 72)))
   
  
  :objects
  ((panel-1 :type 'panel-1
	    :center (translate (the center) :left (the-child width))
	    :page-width (/ (the width) 3))
   
   (panel-2 :type 'panel-2
	    :page-width (/ (the width) 3))
   
   (panel-3 :type 'panel-3
	    :center (translate (the center) :right (the-child width))
	    :page-width (/ (the width) 3))))


(define-object panel-1 (base-view)

  :computed-slots ((fit-to-page? t) 
		   (object-roots (list (the box)))
		   
		   (projection-vector (getf *standard-views* :trimetric))
		   
		   )
  
  :objects ((box)))

(define-object panel-2 (base-view)

  :computed-slots ((fit-to-page? t)
		   (object-roots (list (the box)))
		   
		   (projection-vector (getf *standard-views* :tri-r-r))
		   )
  
  :objects ((box)))

(define-object panel-3 (base-view)

  :computed-slots ((fit-to-page? t)
		   (object-roots (list (the robot)))
		   (projection-vector (getf *standard-views* :trimetric ;;:trimetric
					    ))
		   )
  
  :objects ((robot :type 'gwl-user::robot-assembly)))





(publish :path "/tivo"
	 :function #'(lambda(req ent)
		       (gwl-make-part req ent "pamphlet:assembly")))
  
|#  
  
  

