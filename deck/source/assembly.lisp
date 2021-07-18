(in-package :deck)

(define-object assembly (base-object)

  :input-slots
  ((width (* 10 12))
   (length (* 20 12))
   (perimeter-points (let ((path (list (the (vertex :top :left :front))
                                       (the (edge-center :top :left))
                                       (the (edge-center :top :rear))
                                       (the (vertex :top :right :rear))
                                       (the (vertex :top :right :front)))))
                       (append path (list (first path))))))


  :objects
  ((surface :type 'planking:planked-surface
            :display-controls (list :color :brown)
            :perimeter (the perimeter-points))

   (boundary :type 'global-polyline
              :vertex-list (the perimeter-points))))

