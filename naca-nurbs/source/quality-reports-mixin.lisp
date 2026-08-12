;;;; -*- Mode: Lisp; Package: naca-nurbs -*-

;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;;
;;;; Curve-quality reporting mixed into naca-nurbs-curves: health,
;;;; curvature, deviation, and continuity metrics over the child
;;;; curves named by child-keys / comparison-keys.  Nothing on the
;;;; demo page demands these directly; they serve REPL and MCP
;;;; exploration and the regression suite.

(in-package :naca-nurbs)

(define-object quality-reports-mixin ()

  :input-slots
  (child-keys

   ;; approximated/composed curve paired with its reference
   (comparison-keys '(:nose-upper-approx :nose-upper-fitted
                      :nose-lower-approx :nose-lower-fitted
                      :main-upper-approx :main-upper-fitted
                      :main-lower-approx :main-lower-fitted
                      :upper-composed :full-upper-fitted
                      :lower-composed :full-lower-fitted)))

  :computed-slots
  ((basic-health (the (apply-to-child-keys :function-key
                                           :curve-basic-health)))

   (curvature-quality (the (apply-to-child-keys :function-key
                                                :curve-curvature-quality)))

   (geometric-accuracy
    (mapcan #'(lambda (nurb fitted)
                (list nurb (the (curve-geometric-accuracy (the (evaluate nurb))
                                                          (the (evaluate fitted))))))
            (plist-keys (the comparison-keys))
            (plist-values (the comparison-keys))))

   ;; Arc-length based geometric deviation analysis
   (deviation-analysis
    (mapcan #'(lambda (nurb fitted)
                (list nurb (the (curve-deviation-analysis (the (evaluate nurb))
                                                          (the (evaluate fitted))))))
            (plist-keys (the comparison-keys))
            (plist-values (the comparison-keys))))

   (curvature-smoothness
    (mapcan #'(lambda (key)
                (list key (the (curve-curvature-smoothness (the (evaluate key))))))
            (the child-keys)))

   ;; Area preservation metrics
   (area-analysis
    (mapcan
     #'(lambda (nurb fitted)
         (list nurb
               (list :approx (the (curve-area-under-curve (the (evaluate nurb))))
                     :fitted (the (curve-area-under-curve (the (evaluate fitted)))))))
     (plist-keys (the comparison-keys))
     (plist-values (the comparison-keys))))

   ;; Quality report with all corrected metrics
   (quality-report (list :analyzed-curves
                         (the (apply-to-child-keys :function-key
                                                   :curve-quality-report))
                         :naca-junctures
                         (the naca-juncture-continuity)
                         :geometric-deviation
                         (the deviation-analysis)
                         :curvature-smoothness
                         (the curvature-smoothness)
                         :area-preservation
                         (the area-analysis)
                         :efficiency-metrics
                         (list :control-point-efficiency
                               (the control-point-efficiency-analysis))))

   ;; Juncture continuity analysis: upper-to-lower across the nose,
   ;; and nose-to-main on each surface.
   (naca-juncture-continuity
    (append
     (mapcan
      #'(lambda (upper-key lower-key)
          (list (make-keyword (format nil "nose/nose-~a/~a" upper-key lower-key))
                (list :position-continuity
                      (3d-distance (the (evaluate upper-key) start)
                                   (the (evaluate lower-key) start))
                      :tangent-continuity
                      (angle-between-vectors
                       (the (evaluate upper-key) (tangent (the (evaluate upper-key) u1)))
                       (reverse-vector
                        (the (evaluate lower-key) (tangent (the (evaluate lower-key) u1)))))
                      :enhanced-continuity-check
                      (the (evaluate upper-key) (check-continuity)))))
      '(:full-upper-fitted :nose-upper-fitted :nose-upper-approx :upper-composed)
      '(:full-lower-fitted :nose-lower-fitted :nose-lower-approx :lower-composed))

     (mapcan
      #'(lambda (nose-key main-key)
          (list (make-keyword (format nil "nose/main-~a/~a" nose-key main-key))
                (list :position-continuity
                      (3d-distance (the (evaluate nose-key) end)
                                   (the (evaluate main-key) start))
                      :tangent-continuity
                      (angle-between-vectors
                       (the (evaluate nose-key) (tangent (the (evaluate nose-key) u2)))
                       (the (evaluate main-key) (tangent (the (evaluate main-key) u1))))
                      :enhanced-continuity-check
                      (the (evaluate main-key) (check-continuity)))))
      '(:nose-lower-fitted :nose-upper-fitted :nose-lower-approx :nose-upper-approx)
      '(:main-lower-fitted :main-upper-fitted :main-lower-approx :main-upper-approx))))

   ;; Control point efficiency analysis
   (control-point-efficiency-analysis
    (flet ((total (keys)
             (reduce #'+ keys
                     :key #'(lambda (key)
                              (length (the (evaluate key) control-points))))))
      (let ((approx-total (total '(:nose-upper-approx :nose-lower-approx
                                   :main-upper-approx :main-lower-approx)))
            (fitted-total (total '(:nose-upper-fitted :nose-lower-fitted
                                   :main-upper-fitted :main-lower-fitted))))
        (list :approximated-total approx-total
              :fitted-total fitted-total
              :approx-vs-fitted-ratio (div approx-total fitted-total)
              :compression-ratio (div fitted-total approx-total))))))

  :functions ((apply-to-child-keys
               (&key function-key)
               (mapcan #'(lambda (key)
                           (list key
                                 (the ((evaluate function-key) (the (evaluate key))))))
                       (the child-keys)))

              (curve-quality-report
               (curve)
               (list
                :timestamp (get-universal-time)
                :curve-type (theo curve type)
                :basic-health (the (curve-basic-health curve))
                :curvature-quality (the (curve-curvature-quality curve))))

              (curve-basic-health
               (curve)
               (list
                :success? (theo curve success?)
                :control-point-count
                (length (theo curve control-points))
                :degree (theo curve degree)
                :rational? (theo curve rational?)
                :closed? (theo curve closed?)
                :closure-type (theo curve closure)
                :continuity (theo curve check-continuity)))

              (curve-curvature-quality
               (curve)
               (multiple-value-bind (min-radius location additional-params)
                   (theo curve minimum-radius)
                 (list
                  :minimum-radius min-radius
                  :minimum-radius-location location
                  :curvature-extrema-count (1+ (length additional-params))
                  :start-curvature (theo curve (curvature (theo curve u1)))
                  :end-curvature (theo curve (curvature (theo curve u2)))
                  :mid-curvature (theo curve (curvature
                                              (/ (+ (theo curve u1)
                                                    (theo curve u2)) 2))))))

              (curve-geometric-accuracy
               (curve reference-curve)
               (list
                :total-length (theo curve total-length)
                :reference-length (theo reference-curve total-length)
                :length-ratio (/ (theo curve total-length)
                                 (theo reference-curve total-length))
                :achieved-tolerance (when (typep curve 'approximated-curve)
                                      (theo curve achieved-tolerance))))

              ;; Deviation between geometrically corresponding points:
              ;; both curves sampled at the same relative arc-length
              ;; positions.
              (curve-deviation-analysis
               (curve reference-curve)
               (flet ((sample-points (c)
                        (let ((total (theo c total-length)))
                          (loop for i to 50
                                for frac = (/ (float i) 50)
                                collect (theo c (point
                                                 (cond ((= frac 0.0) (theo c u1))
                                                       ((= frac 1.0) (theo c u2))
                                                       (t (theo c (parameter-at-length
                                                                   (* frac total)))))))))))
                 (let* ((distances (mapcar #'3d-distance
                                           (sample-points curve)
                                           (sample-points reference-curve)))
                        (n (length distances))
                        (max-deviation (reduce #'max distances))
                        (mean-deviation (/ (reduce #'+ distances) n))
                        (variance (/ (reduce #'+ distances
                                             :key #'(lambda (d)
                                                      (expt (- d mean-deviation) 2)))
                                     n))
                        (ref-length (theo reference-curve total-length)))
                   (list :sample-count n
                         :maximum-deviation max-deviation
                         :minimum-deviation (reduce #'min distances)
                         :mean-deviation mean-deviation
                         :deviation-variance variance
                         :deviation-std-dev (sqrt variance)
                         :relative-max-deviation (/ max-deviation ref-length)
                         :length-ratio (/ (theo curve total-length) ref-length)))))

              (curve-curvature-smoothness
               (curve)
               (let* ((params (theo curve (equi-spaced-parameters 20 :spacing :arc-length)))
                      (curvatures (mapcar #'(lambda (param)
                                              (theo curve (curvature param)))
                                          params))
                      (changes (mapcar #'(lambda (a b) (abs (- b a)))
                                       curvatures (rest curvatures)))
                      (mean-curvature (/ (reduce #'+ curvatures) (length curvatures)))
                      (max-change (when changes (reduce #'max changes)))
                      (mean-change (when changes
                                     (/ (reduce #'+ changes) (length changes)))))
                 (list :sample-count (length params)
                       :mean-curvature mean-curvature
                       :curvature-variance (when (> (length curvatures) 1)
                                             (/ (reduce #'+ curvatures
                                                        :key #'(lambda (c)
                                                                 (expt (- c mean-curvature) 2)))
                                                (length curvatures)))
                       :maximum-curvature-change max-change
                       :mean-curvature-change mean-change
                       :smoothness-index (when (and max-change (> max-change 0))
                                           (/ mean-change max-change)))))

              ;; Trapezoidal integration of y against x.
              (curve-area-under-curve
               (curve)
               (let ((points (mapcar #'(lambda (param) (theo curve (point param)))
                                     (theo curve (equi-spaced-parameters
                                                  50 :spacing :arc-length)))))
                 (reduce #'+ (mapcar #'(lambda (p1 p2)
                                         (* (- (get-x p2) (get-x p1))
                                            (/ (+ (get-y p1) (get-y p2)) 2)))
                                     points (rest points)))))))
