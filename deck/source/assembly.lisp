(in-package :deck)

(define-object assembly (base-object)

  :input-slots
  ((width (* 10 12) :settable)
   (length (* 20 12) :settable)
   (perimeter-points (let ((path (list (the (vertex :top :left :front))
                                       (the (edge-center :top :left))
                                       (the (edge-center :top :rear))
                                       (the (vertex :top :right :rear))
                                       (the (vertex :top :right :front)))))
                       (append path (list (first path)))))
   
   (joist-pitch-default 16) :settable

   (inner-width (- (the width) (the joist-reference height)))
   
   (number-of-joists
    (let ((nominal (1+ (ceiling
                        (/ (- (the width)
                              (twice (the joist-reference height)))
                           (the joist-pitch-default))))))
                       (let* ((gap-width (/ (the inner-width)
                                            (- nominal 1))))
                         (if (<= gap-width 16) nominal (1+ nominal)))))

   )



  :objects
  ((surface :type 'planking:planked-surface
            :plank-type 'lumber:2x6
            :display-controls (list :color :brown :transparency 0.5)
            :perimeter (the perimeter-points))

   (boundary :type 'global-polyline
             :vertex-list (the perimeter-points))

   (joist-reference :type 'lumber:2x6
                    :hidden? t)

   (joisting :type 'joisting
             :lumber-type 'lumber:2x6
             :pass-down (number-of-joists joist-pitch-default)
             )))

(define-object joisting (base-object)
  :input-slots
  (lumber-type number-of-joists joist-pitch-default)

  :computed-slots
  ((number-of-joists-effective (the number-of-joists) :settable)
   (joist-pitch (the joist-pitch-default) :settable))

  
  :objects
  ((joists :type 'lumber:2x6
           :sequence (:size (the number-of-joists-effective))
           :display-controls (list :color :blue)
           :orientation
           (alignment :top (the (face-normal-vector :right))
                      :rear (the (face-normal-vector :rear)))


           :center (cond ((the-child first?)
                          (translate (the (edge-center :top :right))
                                     :down (half (the-child width))
                                     :left (half (the-child height))))
                         ((the-child last?)
                          (translate (the (edge-center :top :left))
                                     :down (half (the-child width))
                                     :right (half (the-child height))))

                         ((eql (the-child) (the joists last previous))
                          (midpoint (the-child previous center)
                                    (the joists last center)))
                         
                         (t (translate (the-child previous center)
                                       :left (the joist-pitch)))))))

  
