(in-package :pamphlet)

(define-object landmark (base-object)

  :objects
  ((profile :type 'landmark-profile)))


(define-object landmark-profile (base-object)

  :objects
  ((left :type 'bezier-curve

         ))
  
  )
