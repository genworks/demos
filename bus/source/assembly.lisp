;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :genworks.demos.bus)


(define-object assembly (node-mixin)

  :input-slots
  (
   (strings-for-display "School Bus")
   (wheelbase 300 :settable)
   (track 96 :settable)
   (height 80 :settable) (length 0) (width 0)
   (turn-angle 0 :settable)
   (number-of-rows 10 :settable)
   (minimum-inter-seat-clearance 7 :settable)
   
   
   (display-controls (list :vrml-navigation-info (list "speed" 5)
                           :vrml-viewpoints (list (list "position" (translate (the center) :up 500
                                                                              :right (half (the wheelbase)))))))
   
   (show-display-list? t)
   
   (use-local-box? nil)
   
   (use-standard-saved-slots? t)
    
   )

  

  :trickle-down-slots (:show-display-list?)
  
  :objects
  ((body-frame-overhang :type 'body-frame-overhang-rule
                        :chassis-front-overhang (the chassis front-overhang)
                        :body-front-overhang (the body front-overhang))

   (chassis :type 'chassis
            :pass-down (wheelbase track turn-angle)
            :datum (the center)
            :height 20)
   
   (body :type 'body
         :pass-down (:wheelbase :track)
         :frame-width (the chassis frame-width)
         
         ;;:frame-overhang (- (the-child front-overhang) 
         ;;(the chassis front-overhang))
         
         
         :firewall-base (translate 
                         (the center) :up
                         (half (the chassis frame-height)) :right
                         (- (the-child cab-width)
                            (the-child body-frame-overhang result))))
   
   (interior :type 'interior
             :firewall-base (the body :firewall-base)
             :width (- (the body width) (the body cab-width))
             :length (the body length)
             :height (the body height)
             :number-of-rows (the number-of-rows)
             :minimum-inter-seat-clearance (the minimum-inter-seat-clearance)))
  
  
  :hidden-objects
  ((axis-x :type 'box 
           :length 1 :width 100 :height 1)
   (axis-y :type 'box 
           :length 100 :width 1 :height 1)
   (axis-z :type 'box 
           :length 1 :width 1 :height 100)))





(define-object body-frame-overhang-rule (gwl-rule-object)
  
  :input-slots (chassis-front-overhang body-front-overhang)
  
  :computed-slots ((target-value 15)
                   
                   (result (- (the body-front-overhang)
                              (the chassis-front-overhang)))
                   
                   (rule-result (the result))
                   
                   (rule-result-help "Computed by subtracting the front overhang
of the chassis from the front overhang of the body.")
                   
                   (violated? (< (the result) (the target-value)))
                   
                   (rule-description "Amount of overhang of body over axle")
                   
                   (rule-title "Chassis Front Overhang")))


