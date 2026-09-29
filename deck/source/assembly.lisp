;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.


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
   
   (joist-pitch-default 16 :settable)

   (space-per-leg 120 :settable)

   (leg-pitch-half-default (/ (the space-per-leg)(1+ (/ (- (the number-of-legs-length) 4) 2))))

   (number-of-legs-length (+ 4(* 2 (floor(/ (the length) (the space-per-leg)) ))))

   (leg-height 36)

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
             )
  
   (beams-first :type 'beam-first
                :lumber-type 'lumber:2x6
                :length (the width)
                :deck-length (the length))
   
   (beams-second :type 'beam-second
                 :lumber-type 'lumber:2x6)
   
   (legs-length :type 'legs
                :lumber-type 'lumber:2x6
                :number-of-legs-lwise-effective (the number-of-legs-length)
                :deck-length (the length)
                :length (the leg-height))
   


   
   ))




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

(define-object beam-first (base-object)
  :input-slots
  ((lumber-type)
   (length)
   (deck-length))

  :objects
  ((beams :type 'lumber:2x6
          :sequence (:size 2)
          :display-controls (list :color :red)
          :orientation
          (alignment :top (the (face-normal-vector :rear))
                     :left (the (face-normal-vector :top)))

          :center (if (oddp (the-child index))
                      (translate (the center) :front (half (the deck-length)) :down (* 1.5(the-child width)))
                      (translate (the center) :back (half (the deck-length)) :down (* 1.5(the-child width)))))))


          
(define-object beam-second (base-object)
  :input-slots
  (lumber-type)

  :objects
  ((beams :type 'lumber:2x6
          :sequence (:size 2)
          :display-controls (list :color :yellow)
          :orientation
          (alignment :top (the (face-normal-vector :right))
                     :left (the (face-normal-vector :top)))


          :center (if (oddp (the-child index))
                      (translate (the center) :right (half (the width)) :down (* 1.5(the-child width)))
                      (translate (the center) :left (half (the width)) :down (* 1.5(the-child width)))))))



(define-object legs (base-object)
  :input-slots
  ((lumber-type)
   deck-length
   length 
   (beam-height (* 1.5(the beams-first length)))
   (number-of-legs-lwise-effective (the number-of-legs-length)))

  :computed-slots
  ((leg-pitch (/ (the deck-length)(1- (half (the number-of-legs-lwise-effective))))))


  :objects
  ((leg-seq-lengthwise :type 'lumber:4x4
            :sequence (:size (the number-of-legs-lwise-effective))
            :display-controls (list :color :green)

            
            :orientation
            (alignment :top (the (face-normal-vector :front))
                       :rear (the (face-normal-vector :top)))


            :center (if (oddp (the-child index)) (translate (the center)
                                                           :down (+ (half (the-child length))11)
                                                            :left (half (the width))
                                                           :front (half (the deck-length))
                                                           :back (* (half(1- (the-child index))) (the leg-pitch)))
                        (translate (the center)
                                   :down (+ (half (the-child length))11)
                                   :right (half (the width))
                                   :front (half (the deck-length))
                                   :back (* (half(the-child index)) (the leg-pitch)))
                                                                      

            ))))
                               
                            
                     
  

  
