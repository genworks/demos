(in-package :naca-nurbs)

(define-object quality-reports-mixin ()

  :input-slots
  (child-keys

   (comparison-keys '(:nose-upper-approx :nose-upper-fitted
                      :nose-lower-approx :nose-lower-fitted
                      :main-upper-approx :main-upper-fitted
                      :main-lower-approx :main-lower-fitted
                      :upper-composed :full-upper-fitted
                      :lower-composed :full-lower-fitted
                      :imported-0021-lower :full-lower-fitted
                      :imported-0021-upper :full-upper-fitted)))

  :computed-slots
  ((basic-health (the (apply-to-child-keys :function-key
                                           :curve-basic-health)))

   (curvature-quality (the (apply-to-child-keys :function-key
                                                :curve-curvature-quality)))

   (geometric-accuracy
    (mapcan #'(lambda(nurb fitted)
                (list nurb (the (curve-geometric-accuracy (the (evaluate nurb))
                                                          (the (evaluate fitted))))))
            (plist-keys (the comparison-keys))
            (plist-values (the comparison-keys))))

   ;; Arc-length based geometric deviation analysis
   (deviation-analysis
    (mapcan #'(lambda(nurb fitted)
                (list nurb (the (curve-deviation-analysis (the (evaluate nurb))
                                                          (the (evaluate fitted))))))
            (plist-keys (the comparison-keys))
            (plist-values (the comparison-keys))))

   ;; Curvature smoothness analysis
   (curvature-smoothness
    (mapcan #'(lambda(key)
                (list key (the (curve-curvature-smoothness (the (evaluate key))))))
            (the child-keys)))

   ;; Area preservation metrics
   (area-analysis
    (mapcan
     #'(lambda(nurb fitted)
         (list nurb
               (list :approx (the (curve-area-under-curve (the (evaluate nurb))))
                     :fitted (the (curve-area-under-curve (the (evaluate fitted)))))))
     (plist-keys (the comparison-keys))
     (plist-values (the comparison-keys))))

   ;; Processing efficiency metrics
   (efficiency-metrics (list :control-point-efficiency
                             (the control-point-efficiency-analysis)
                             ))

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
                         (the efficiency-metrics)))

   ;; Juncture continuity analysis
   (naca-juncture-continuity
    (append
     (mapcan
      #'(lambda(upper-key lower-key)
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
      '(:full-upper-fitted :nose-upper-fitted :nose-upper-approx
        :upper-composed :imported-0021-upper)
      '(:full-lower-fitted :nose-lower-fitted :nose-lower-approx
        :lower-composed :imported-0021-lower))

     (mapcan
      #'(lambda(nose-key main-key)
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
    (let ((approx-total (+ (length (the nose-upper-approx control-points))
                           (length (the nose-lower-approx control-points))
                           (length (the main-upper-approx control-points))
                           (length (the main-lower-approx control-points))))
          (fitted-total (+ (length (the nose-upper-fitted control-points))
                           (length (the nose-lower-fitted control-points))
                           (length (the main-upper-fitted control-points))
                           (length (the main-lower-fitted control-points))))
          (imported-total (+ (length (the imported-0021-lower control-points))
                             (length (the imported-0021-upper control-points)))))

      (list :approximated-total approx-total
            :fitted-total fitted-total
            :imported-total imported-total
            :approx-vs-fitted-ratio (div approx-total fitted-total)
            :approx-vs-imported-ratio (div approx-total imported-total)
            :compression-ratio (div fitted-total approx-total)
            :cad-improvement-ratio (div imported-total approx-total)))))

  :functions ((apply-to-child-keys
               (&key function-key)
               (mapcan #'(lambda(key)
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

              ;; Arc-length based deviation analysis (geometrically corresponding points)
              (curve-deviation-analysis
               (curve reference-curve)
               (let* ((sample-count 50)
                      (curve-total-length (theo curve total-length))
                      (ref-total-length (theo reference-curve total-length))
                      ;; Sample at same relative arc-length positions
                      (arc-length-fractions (let ((result nil))
                                              (dotimes (i (1+ sample-count))
                                                (push (/ (float i) sample-count) result))
                                              (nreverse result)))
                      ;; Get parameters for corresponding arc-length positions
                      (curve-params (mapcar #'(lambda(frac)
                                               (cond ((= frac 0.0) (theo curve u1))
                                                     ((= frac 1.0) (theo curve u2))
                                                     (t (theo curve (parameter-at-length (* frac curve-total-length))))))
                                           arc-length-fractions))
                      (ref-params (mapcar #'(lambda(frac)
                                             (cond ((= frac 0.0) (theo reference-curve u1))
                                                   ((= frac 1.0) (theo reference-curve u2))
                                                   (t (theo reference-curve (parameter-at-length (* frac ref-total-length))))))
                                         arc-length-fractions))
                      ;; Get points at corresponding arc-length positions
                      (curve-points (mapcar #'(lambda(param) (theo curve (point param))) curve-params))
                      (ref-points (mapcar #'(lambda(param) (theo reference-curve (point param))) ref-params))
                      (point-distances (mapcar #'3d-distance curve-points ref-points))
                      (max-deviation (reduce #'max point-distances))
                      (min-deviation (reduce #'min point-distances))
                      (mean-deviation (/ (reduce #'+ point-distances) (length point-distances)))
                      (deviation-variance (let ((variance-sum (reduce #'+ (mapcar #'(lambda(d)
                                                                                   (expt (- d mean-deviation) 2))
                                                                                 point-distances))))
                                            (/ variance-sum (length point-distances)))))
                 (list :sample-count (length point-distances)
                       :maximum-deviation max-deviation
                       :minimum-deviation min-deviation
                       :mean-deviation mean-deviation
                       :deviation-variance deviation-variance
                       :deviation-std-dev (sqrt deviation-variance)
                       :relative-max-deviation (/ max-deviation ref-total-length)
                       :length-ratio (/ curve-total-length ref-total-length))))

              ;; Curvature smoothness analysis
              (curve-curvature-smoothness
               (curve)
               (let* ((sample-params (theo curve (equi-spaced-parameters 20 :spacing :arc-length)))
                      (curvatures (mapcar #'(lambda(param)
                                              (theo curve (curvature param)))
                                          sample-params))
                      (curvature-changes (mapcar #'(lambda(i)
                                                     (abs (- (nth (1+ i) curvatures)
                                                             (nth i curvatures))))
                                                 (let ((result nil))
                                                   (dotimes (i (- (length curvatures) 1))
                                                     (push i result))
                                                   (nreverse result))))
                      (mean-curvature (/ (reduce #'+ curvatures) (length curvatures)))
                      (max-curvature-change (when curvature-changes (reduce #'max curvature-changes)))
                      (mean-curvature-change (when curvature-changes
                                               (/ (reduce #'+ curvature-changes)
                                                  (length curvature-changes)))))
                 (list :sample-count (length sample-params)
                       :mean-curvature mean-curvature
                       :curvature-variance (when (> (length curvatures) 1)
                                             (let ((variance-sum
                                                    (reduce #'+ (mapcar #'(lambda(c)
                                                                            (expt (- c mean-curvature) 2))
                                                                        curvatures))))
                                               (/ variance-sum (length curvatures))))
                       :maximum-curvature-change max-curvature-change
                       :mean-curvature-change mean-curvature-change
                       :smoothness-index (when (and max-curvature-change (> max-curvature-change 0))
                                           (/ mean-curvature-change max-curvature-change)))))

              ;; Area under curve calculation (y*dx integration approximation)
              (curve-area-under-curve
               (curve)
               (let* ((sample-params (theo curve (equi-spaced-parameters 50 :spacing :arc-length)))
                      (points (mapcar #'(lambda(param) (theo curve (point param))) sample-params)))
                 ;; Simple trapezoidal integration of y-coordinate vs x-coordinate
                 (do ((i 0 (1+ i))
                      (sum 0.0))
                     ((>= i (1- (length points))) sum)
                   (let* ((p1 (nth i points))
                          (p2 (nth (1+ i) points))
                          (dx (- (get-x p2) (get-x p1)))
                          (avg-y (/ (+ (get-y p1) (get-y p2)) 2)))
                     (incf sum (* dx avg-y))))))))
