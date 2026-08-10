(gwl:define-package :naca-nurbs
  (:export #:generate-naca-points
           #:naca-nurbs-curves
           #:seed-regression-tests
           #:run-regression-tests #:ui
           #:publish-ui!))

(in-package :naca-nurbs)

;; ============================================================
;; Index-based NACA NURBS builder (arrays + parametric math)
;; ============================================================

(define-object naca-nurbs-curves (base-object quality-reports-mixin)

  :input-slots
  ((airfoil :0021 :settable)
   (n-points 216 :settable)
   (cosine? t :settable)
   (closed? t :settable)
   (adaptive-split? t :settable)
   (split-x-default 0.15 :settable)
   (split-min-x 0.03 :settable)
   (split-max-x 0.25 :settable)
   (degree 5 :settable)
   (target-main-points 9 :settable)
   (target-nose-points 7 :settable)
   (approx-tolerance 0.0005 :settable)

   ;; Optional section-specific tolerances
   (use-section-tolerances? t :settable)

   (nose-upper-tolerance (the approx-tolerance) :settable)
   (nose-lower-tolerance (the approx-tolerance) :settable)
   (main-upper-tolerance (* (the approx-tolerance) 2)  :settable)
   (main-lower-tolerance (* (the approx-tolerance) 2) :settable)

   (debug? nil :settable)
   (split-with-analytical-curvature? t :settable)
   (use-analytical-tangents? t :settable)
   (numeric-curvature-sample-points 30 :settable)

   ;; Master samples (arrays, index-aligned)
   (naca-samples (generate-naca-samples (the airfoil)
                                        (the n-points)
                                        :cosine? (the cosine?)
                                        :closed? (the closed?)))

   (upper-array (getf (the naca-samples) :upper))
   (lower-array (getf (the naca-samples) :lower))
   (tee-samples (getf (the naca-samples) :tees))
   (x-samples   (getf (the naca-samples) :xs))

   ;; Compatibility lists (for downstream objects expecting lists)
   (naca-points
    (list :upper (let ((upper-points nil))
                   (dotimes (i (length (the upper-array)))
                     (push (aref (the upper-array) i) upper-points))
                   (nreverse upper-points))
          :lower (let ((lower-points nil))
                   (dotimes (i (length (the lower-array)))
                     (push (aref (the lower-array) i) lower-points))
                   (nreverse lower-points))))

   ;; Full-length tangents (lists), analytical or numeric
   (naca-tangents-analytical
    (let* ((n (length (the tee-samples)))
           (airfoil (the airfoil))
           (cosine? (the cosine?))
           (closed? (the closed?)))
      (labels ((tan-at (surface idx)
                 (if (= idx 0)
                     (the (compute-zero-tangent surface))
                     (analytical-tangent-parametric airfoil
                                                    (aref (the tee-samples) idx)
                                                    :surface surface
                                                    :cosine? cosine?
                                                    :closed? closed?))))
        (list :upper (let ((upper-points nil))
                       (dotimes (i n) (push (tan-at :upper i) upper-points))
                       (nreverse upper-points))
              :lower (let ((lower-points nil))
                       (dotimes (i n) (push (tan-at :lower i) lower-points))
                       (nreverse lower-points))))))

   (naca-tangents-numeric
    (labels ((unit-diff (p1 p2)
               (unitize-vector (subtract-vectors p2 p1))))
      (destructuring-bind (&key upper lower) (the naca-points)
        (let* ((u (cons (the (compute-zero-tangent :upper))
                        (mapcar #'unit-diff (rest upper) (rest (rest upper)))))
               (l (cons (the (compute-zero-tangent :lower))
                        (mapcar #'unit-diff (rest lower) (rest (rest lower))))))
          (list :upper (append u (last u))
                :lower (append l (last l)))))))

   (naca-tangents
    (if (the use-analytical-tangents?)
        (the naca-tangents-analytical)
        (the naca-tangents-numeric)))

   (child-keys '(:full-upper-fitted :full-lower-fitted
                 :main-upper-fitted :main-lower-fitted
                 :nose-upper-fitted :nose-lower-fitted
                 :main-upper-approx :main-lower-approx
                 :nose-upper-approx :nose-lower-approx

                 :upper-composed :lower-composed

                 :upper-elevated :lower-elevated

                 :imported-0021-lower :imported-0021-upper)))

  :computed-slots
  (;; --------------------------------------------------------
   ;; Split into sections using a t-domain curvature search
   ;; --------------------------------------------------------
   (sectioned-indices
    (multiple-value-bind (nu mu nl ml xupper xlower)
        (the split-airfoil-sections-indices-adaptive)
      (declare (ignore xupper xlower))
      (list :nose-upper-indices nu
            :main-upper-indices mu
            :nose-lower-indices nl
            :main-lower-indices ml)))

   (sectioned-points
    (flet ((from-indices (arr idxs)
             (mapcar (lambda (k) (aref arr k)) idxs)))
      (list
       :nose-upper (from-indices (the upper-array) (getf (the sectioned-indices) :nose-upper-indices))
       :main-upper (from-indices (the upper-array) (getf (the sectioned-indices) :main-upper-indices))
       :nose-lower (from-indices (the lower-array) (getf (the sectioned-indices) :nose-lower-indices))
       :main-lower (from-indices (the lower-array) (getf (the sectioned-indices) :main-lower-indices)))))

   ;; Build tangents section-by-section using indices → tee (no reverse lookup)
   (sectioned-tangents
    (let* ((airfoil (the airfoil))
           (cosine? (the cosine?))
           (closed? (the closed?))
           (tees (the tee-samples))
           (use-analytical? (the use-analytical-tangents?)))

      (labels ((mk-analytical (surface indices &key nose?)
                 (let ((out (if nose? (list (the (compute-zero-tangent surface))) nil)))
                   (dolist (idx (if nose? (rest indices) indices))
                     (push (analytical-tangent-parametric airfoil (aref tees idx)
                                                          :surface surface
                                                          :cosine? cosine?
                                                          :closed? closed?)
                           out))
                   (nreverse out)))

               (%segment-tangents (pts)
                 "Returns list of unit tangents for each consecutive segment (k-1 items)."
                 (let ((out nil))
                   (mapc (lambda (a b) (push (unitize-vector (subtract-vectors b a)) out))
                         pts (rest pts))
                   (nreverse out)))

               (%k-tangents (pts first-tan)
                 "Given k points and a proposed first tangent, return k tangents by
   using segment tangents for interior and duplicating the last one at the end."
                 (let* ((segs (%segment-tangents pts))            ; length k-1
                        (head (or first-tan (first segs)))
                        (tail (car (last segs))))
                   (append (list head)
                           (butlast segs) ; k-2
                           (list tail)))) ; total k

               (mk-numeric (points &key nose? surface)
                 (let ((first (and nose? (the (compute-zero-tangent surface)))))
                   (%k-tangents points first))))

        (let* ((nu (getf (the sectioned-indices) :nose-upper-indices))
               (mu (getf (the sectioned-indices) :main-upper-indices))
               (nl (getf (the sectioned-indices) :nose-lower-indices))
               (ml (getf (the sectioned-indices) :main-lower-indices))

               (p-nu (getf (the sectioned-points) :nose-upper))
               (p-mu (getf (the sectioned-points) :main-upper))
               (p-nl (getf (the sectioned-points) :nose-lower))
               (p-ml (getf (the sectioned-points) :main-lower)))

          (list
           :nose-upper (if use-analytical?
                           (mk-analytical :upper nu :nose? t)
                           (mk-numeric p-nu :nose? t :surface :upper))
           :main-upper (if use-analytical?
                           (mk-analytical :upper mu :nose? nil)
                           (mk-numeric p-mu :nose? nil :surface :upper))
           :nose-lower (if use-analytical?
                           (mk-analytical :lower nl :nose? t)
                           (mk-numeric p-nl :nose? t :surface :lower))
           :main-lower (if use-analytical?
                           (mk-analytical :lower ml :nose? nil)
                           (mk-numeric p-ml :nose? nil :surface :lower)))))))

   (airfoil-spec (get-airfoil-spec (the airfoil)))

   ;; Reported split-x pulled from the first main index on each surface
   (split-x
    (let* ((xs (the x-samples))
           (mu (getf (the sectioned-indices) :main-upper-indices))
           (ml (getf (the sectioned-indices) :main-lower-indices)))
      (list :upper (aref xs (first mu))
            :lower (aref xs (first ml))))))

  :objects
  ((full-upper-fitted :type 'fitted-curve
                      :display-controls (list :color :blue)
                      :vectors (getf (the naca-tangents) :upper)
                      :vector-type :tangents
                      :points (getf (the naca-points) :upper))

   (full-lower-fitted :type 'fitted-curve
                      :vectors (getf (the naca-tangents) :lower)
                      :vector-type :tangents
                      :points (getf (the naca-points) :lower))

   (main-upper-fitted :type 'fitted-curve
                      :points (getf (the sectioned-points) :main-upper)
                      :vectors (getf (the sectioned-tangents) :main-upper)
                      :vector-type :tangents
                      :parameterization :chord-length)

   (main-lower-fitted :type 'fitted-curve
                      :points (getf (the sectioned-points) :main-lower)
                      :vectors (getf (the sectioned-tangents) :main-lower)
                      :vector-type :tangents
                      :parameterization :chord-length)

   (nose-upper-fitted :type 'fitted-curve
                      :points (getf (the sectioned-points) :nose-upper)
                      :vectors (getf (the sectioned-tangents) :nose-upper)
                      :vector-type :tangents
                      :parameterization :chord-length)

   (nose-lower-fitted :type 'fitted-curve
                      :points (getf (the sectioned-points) :nose-lower)
                      :vectors (getf (the sectioned-tangents) :nose-lower)
                      :vector-type :tangents
                      :parameterization :chord-length)

   (main-upper-approx :type 'approximated-curve
                      :match-parameterization? t
                      :tolerance (if (the use-section-tolerances?)
                                     (the main-upper-tolerance)
                                     (the approx-tolerance))
                      :curve-in (the main-upper-fitted))

   (main-lower-approx :type 'approximated-curve
                      :match-parameterization? t
                      :tolerance (if (the use-section-tolerances?)
                                     (the main-lower-tolerance)
                                     (the approx-tolerance))
                      :curve-in (the main-lower-fitted))

   (nose-upper-approx :type 'approximated-curve
                      :match-parameterization? t
                      :display-controls (list :color :red :line-thickness 2)
                      :tolerance (if (the use-section-tolerances?)
                                     (the nose-upper-tolerance)
                                     (the approx-tolerance))
                      :curve-in (the nose-upper-fitted))

   (nose-lower-approx :type 'approximated-curve
                      :match-parameterization? t
                      :display-controls (list :color :green :line-thickness 2)
                      :tolerance (if (the use-section-tolerances?)
                                     (the nose-lower-tolerance)
                                     (the approx-tolerance))
                      :curve-in (the nose-lower-fitted))

   (upper-composed :type 'composed-curve
                   :display-controls (list :color :blue)
                   :curves (list (the nose-upper-approx)
                                 (the main-upper-approx)))

   (lower-composed :type 'composed-curve
                   :display-controls (list :color :green)
                   :curves (list (the nose-lower-approx)
                                 (the main-lower-approx)))

   (upper-elevated :type 'degree-elevated-curve
                   :target-degree 4
                   :curve-in (the upper-composed))

   (lower-elevated :type 'degree-elevated-curve
                   :target-degree 4
                   :curve-in (the lower-composed))

   (imported-0021 :type 'import-step)

   (imported-0021-lower :type 'boxed-curve
                        :scale 1/65
                        :orientation (alignment :rear (the (face-normal-vector :top))
                                                :right (the (face-normal-vector :right)))
                        :center (translate (the center)
                                           :left -0.06153862572593498)
                        :display-controls
                        (list :color :cyan)
                        :curve-in
                        (the imported-0021 (brep-1-edges 0)))

   (imported-0021-upper :type 'boxed-curve
                        :scale 1/65
                        :orientation (the imported-0021-lower orientation)
                        :center (translate (the center)
                                           :left -0.06153862572593498)
                        :display-controls
                        (list :color :cyan)
                        :curve-in
                        (the imported-0021 (brep-1-edges 4))))

  :functions
  ((split-airfoil-sections
    ()
    (the (split-airfoil-sections-indices-adaptive
          :hardcoded-x (the split-x-default))))

   (split-airfoil-sections-indices-adaptive
    (&key hardcoded-x)
    "Return (noseU mainU noseL mainL split-x-upper split-x-lower) as indices & x."
    (let* ((xs (the x-samples))
           (min-x (the split-min-x))
           (max-x (the split-max-x))
           (t-min (x->t min-x (the cosine?)))
           (t-max (x->t max-x (the cosine?)))
           (closed? (the closed?)))

      (labels
          ((find-split-tee (surface)
             (if hardcoded-x
                 (x->t hardcoded-x (the cosine?))
                 (if (the split-with-analytical-curvature?)
                     ;; analytical curvature in t
                     (let* ((samples 30)
                            (kappa (lambda (tt _)
                                     (declare (ignore _))
                                     (analytical-curvature-parametric (the airfoil) tt
                                                                      :surface surface
                                                                      :cosine? (the cosine?)
                                                                      :closed? closed?)))
                            (coarse (let ((r nil))
                                      (dotimes (i (1+ samples) (nreverse r))
                                        (let* ((tt (+ t-min (* (/ i samples) (- t-max t-min))))
                                               (kv (funcall kappa tt nil)))
                                          (push (list tt kv) r)))))
                            (focus (or (find-max-gradient-region coarse) (list t-min t-max))))
                       (ternary-search-maximum :func kappa :curve nil
                                               :min-x (first focus) :max-x (second focus)
                                               :tolerance 1d-3))
                     ;; Numeric fallback (still in t). For brevity we simply pick midpoint.
                     (/ (+ t-min t-max) 2.0d0))))
           (split-index (tt)
             (let* ((x* (t->x tt (the cosine?)))
                    (i (lower-bound xs x*)))
               (min i (1- (length xs)))))

           (make-ranges (idx n)
             (values (let ((first-list nil))
                       (dotimes (i (1+ idx))
                         (push i first-list))
                       (nreverse first-list))
                     (let ((second-list nil))
                       (dotimes (i (- n idx))
                         (push (+ idx i) second-list))
                       (nreverse second-list))))

           (surface-n (surface)
             (length (ecase surface
                       (:upper (the upper-array))
                       (:lower (the lower-array))))))

        (let* ((tU (find-split-tee :upper))
               (tL (find-split-tee :lower))
               (iU (split-index tU))
               (iL (split-index tL))
               (nU (surface-n :upper))
               (nL (surface-n :lower)))
          (multiple-value-bind (nu mu) (make-ranges iU nU)
            (multiple-value-bind (nl ml) (make-ranges iL nL)
              (values nu mu nl ml
                      (t->x tU (the cosine?)) (t->x tL (the cosine?)))))))))

   (find-precise-curvature-transition
    (&key fitted (region-bounds '(0.05 0.25)) (tolerance 0.001))
    (declare (ignore fitted))
    "Kept for compatibility / diagnostics; analytical path now works in t."
    (let* ((x-min (first region-bounds))
           (x-max (second region-bounds))
           (t-min (x->t x-min (the cosine?)))
           (t-max (x->t x-max (the cosine?)))
           (surface :upper) ;; caller not used here
           (kappa (lambda (tt _) (declare (ignore _))
                    (analytical-curvature-parametric (the airfoil) tt
                                                     :surface surface
                                                     :cosine? (the cosine?)
                                                     :closed? (the closed?))))
           (coarse (let ((r nil))
                     (dotimes (i 31 (nreverse r))
                       (let* ((tt (+ t-min (* (/ i 30.0d0) (- t-max t-min))))
                              (kv (funcall kappa tt nil)))
                         (push (list tt kv) r)))))
           (focus (or (find-max-gradient-region coarse) (list t-min t-max)))
           (t* (ternary-search-maximum :func kappa :curve nil
                                       :min-x (first focus) :max-x (second focus)
                                       :tolerance tolerance)))
      (t->x t* (the cosine?))))

   (compute-zero-tangent
    (surface-type)
    (let ((spec (the airfoil-spec)))
      (let* ((dcamber-at-0 (funcall (fifth spec) 0.0d0 (first spec) (second spec)))
             (theta (atan dcamber-at-0))
             (ct (cos theta))
             (st (sin theta)))
        (if (eq surface-type :upper)
            (make-vector (- st) ct 0.0d0)
            (make-vector st (- ct) 0.0d0)))))

   ;; --------------------------
   ;; (Optional) tolerance ops
   ;; --------------------------

   (optimize-tolerance
    (&key (max-nose-points (the target-nose-points))
          (max-main-points (the target-main-points))
          (tolerance-min 1d-6)
          (tolerance-max 1d-2)
          (max-iterations 20))
    (let ((high tolerance-max))
      (the (set-slot! :approx-tolerance high))
      (labels ((okp ()
                 (and (<= (length (the nose-upper-approx  control-points)) max-nose-points)
                      (<= (length (the nose-lower-approx  control-points)) max-nose-points)
                      (<= (length (the main-upper-approx  control-points)) max-main-points)
                      (<= (length (the main-lower-approx  control-points)) max-main-points))))
        (the (set-slot! :approx-tolerance tolerance-max))
        (let ((low tolerance-min) (high tolerance-max) (best nil))
          (dotimes (_ max-iterations (or best high))
            (let ((mid (/ (+ low high) 2d0)))
              (the (set-slot! :approx-tolerance mid))
              (if (okp) (setf best mid high mid) (setf low mid))
              (when (<= (abs (- high low)) (* tolerance-min 1d-3))
                (return (or best mid)))))))))

   (compute-optimized-tolerances
    (&key (efficiency-target 0.7d0) (safety-margin 1.2d0))
    "Compute section-specific optimized tolerances based on achieved values."
    (let* ((current (the approx-tolerance))
           (sections '(nose-upper-approx nose-lower-approx
                       main-upper-approx main-lower-approx))
           (results nil))
      (dolist (s sections)
        (let* ((ach (the (evaluate s) achieved-tolerance))
               (ratio (/ ach current))
               (opt (if (< ratio efficiency-target)
                        (* ach safety-margin)
                        current)))
          (push (list s opt ratio ach) results)))
      (let ((out nil))
        (dolist (r results)
          (let ((s (first r)) (opt (second r)))
            (push (ecase s
                    (nose-upper-approx :nose-upper-tolerance)
                    (nose-lower-approx :nose-lower-tolerance)
                    (main-upper-approx :main-upper-tolerance)
                    (main-lower-approx :main-lower-tolerance))
                  out)
            (push opt out)))
        (nreverse out))))))
