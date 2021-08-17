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
   
   
   
