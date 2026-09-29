;;;; -*- Mode: Lisp; Package: naca-nurbs -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;;
;;;; Index-based NACA NURBS builder: master samples live in arrays
;;;; (points, t-values, x-values, index-aligned), sections split on a
;;;; t-domain curvature search, and each section is fitted then
;;;; approximated to tolerance.  Supporting math is in utilities.lisp.

(gwl:define-package :naca-nurbs
  (:export #:generate-naca-points
           #:naca-nurbs-curves
           #:seed-regression-tests
           #:run-regression-tests #:ui
           #:publish-ui!))

(in-package :naca-nurbs)

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
   (main-upper-tolerance (* (the approx-tolerance) 2) :settable)
   (main-lower-tolerance (* (the approx-tolerance) 2) :settable)

   (split-with-analytical-curvature? t :settable)
   (use-analytical-tangents? t :settable)

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
   (naca-points (list :upper (coerce (the upper-array) 'list)
                      :lower (coerce (the lower-array) 'list)))

   ;; Full-length tangents (lists), analytical or numeric; the
   ;; leading-edge tangent comes from the camber line either way.
   (naca-tangents-analytical
    (flet ((tangents (surface)
             (cons (the (compute-zero-tangent surface))
                   (loop for i from 1 below (length (the tee-samples))
                         collect (analytical-tangent-parametric
                                  (the airfoil) (aref (the tee-samples) i)
                                  :surface surface
                                  :cosine? (the cosine?)
                                  :closed? (the closed?))))))
      (list :upper (tangents :upper) :lower (tangents :lower))))

   (naca-tangents-numeric
    (flet ((tangents (surface points)
             (let* ((segments (mapcar #'(lambda (a b)
                                          (unitize-vector (subtract-vectors b a)))
                                      points (rest points)))
                    (tans (cons (the (compute-zero-tangent surface))
                                (rest segments))))
               (append tans (last tans)))))
      (list :upper (tangents :upper (getf (the naca-points) :upper))
            :lower (tangents :lower (getf (the naca-points) :lower)))))

   (naca-tangents
    (if (the use-analytical-tangents?)
        (the naca-tangents-analytical)
        (the naca-tangents-numeric)))

   (child-keys '(:full-upper-fitted :full-lower-fitted
                 :main-upper-fitted :main-lower-fitted
                 :nose-upper-fitted :nose-lower-fitted
                 :main-upper-approx :main-lower-approx
                 :nose-upper-approx :nose-lower-approx
                 :upper-composed :lower-composed)))

  :computed-slots
  (;; Section index ranges from the t-domain curvature split
   (sectioned-indices
    (multiple-value-bind (nose-upper main-upper nose-lower main-lower)
        (the split-airfoil-sections-indices-adaptive)
      (list :nose-upper nose-upper :main-upper main-upper
            :nose-lower nose-lower :main-lower main-lower)))

   (sectioned-points
    (flet ((points (arr key)
             (mapcar #'(lambda (k) (aref arr k))
                     (getf (the sectioned-indices) key))))
      (list :nose-upper (points (the upper-array) :nose-upper)
            :main-upper (points (the upper-array) :main-upper)
            :nose-lower (points (the lower-array) :nose-lower)
            :main-lower (points (the lower-array) :main-lower))))

   ;; Tangents per section: analytical from the t-samples, or numeric
   ;; from segment differences.  Nose sections lead with the
   ;; analytical zero-t tangent either way.
   (sectioned-tangents
    (labels ((analytical-tangents (surface key nose?)
               (append
                (when nose? (list (the (compute-zero-tangent surface))))
                (mapcar #'(lambda (idx)
                            (analytical-tangent-parametric
                             (the airfoil) (aref (the tee-samples) idx)
                             :surface surface
                             :cosine? (the cosine?)
                             :closed? (the closed?)))
                        (funcall (if nose? #'rest #'identity)
                                 (getf (the sectioned-indices) key)))))
             (numeric-tangents (surface key nose?)
               (let* ((points (getf (the sectioned-points) key))
                      (segments (mapcar #'(lambda (a b)
                                            (unitize-vector (subtract-vectors b a)))
                                        points (rest points))))
                 (append (list (if nose?
                                   (the (compute-zero-tangent surface))
                                   (first segments)))
                         (butlast segments)
                         (last segments))))
             (tangents (surface key nose?)
               (if (the use-analytical-tangents?)
                   (analytical-tangents surface key nose?)
                   (numeric-tangents surface key nose?))))
      (list :nose-upper (tangents :upper :nose-upper t)
            :main-upper (tangents :upper :main-upper nil)
            :nose-lower (tangents :lower :nose-lower t)
            :main-lower (tangents :lower :main-lower nil))))

   (airfoil-spec (get-airfoil-spec (the airfoil)))

   ;; Reported split-x pulled from the first main index on each surface
   (split-x
    (list :upper (aref (the x-samples)
                       (first (getf (the sectioned-indices) :main-upper)))
          :lower (aref (the x-samples)
                       (first (getf (the sectioned-indices) :main-lower))))))

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
                                 (the main-lower-approx))))

  :functions
  ((split-airfoil-sections-indices-adaptive
    ()
    "Return (noseU mainU noseL mainL) as index lists: each surface
splits where its analytical curvature peaks within [split-min-x,
split-max-x], found by a coarse scan plus ternary search in t."
    (let* ((xs (the x-samples))
           (t-min (x->t (the split-min-x) (the cosine?)))
           (t-max (x->t (the split-max-x) (the cosine?))))
      (labels
          ((kappa (surface)
             #'(lambda (tt curve)
                 (declare (ignore curve))
                 (analytical-curvature-parametric (the airfoil) tt
                                                  :surface surface
                                                  :cosine? (the cosine?)
                                                  :closed? (the closed?))))
           (find-split-tee (surface)
             (if (the split-with-analytical-curvature?)
                 (let* ((samples 30)
                        (kappa (kappa surface))
                        (coarse (loop for i to samples
                                      for tt = (+ t-min (* (/ i samples)
                                                           (- t-max t-min)))
                                      collect (list tt (funcall kappa tt nil))))
                        (focus (or (find-max-gradient-region coarse)
                                   (list t-min t-max))))
                   (ternary-search-maximum :func kappa :curve nil
                                           :min-x (first focus)
                                           :max-x (second focus)
                                           :tolerance 1d-3))
                 ;; Numeric fallback (still in t): just the midpoint.
                 (/ (+ t-min t-max) 2.0d0)))
           (split-index (tt)
             (min (lower-bound xs (t->x tt (the cosine?)))
                  (1- (length xs)))))
        (let ((index-upper (split-index (find-split-tee :upper)))
              (index-lower (split-index (find-split-tee :lower))))
          (values (list-of-numbers 0 index-upper)
                  (list-of-numbers index-upper (1- (length (the upper-array))))
                  (list-of-numbers 0 index-lower)
                  (list-of-numbers index-lower (1- (length (the lower-array)))))))))

   ;; Unit tangent at the leading edge (t = 0), where the parametric
   ;; formulas degenerate: the section curve leaves the nose
   ;; perpendicular to the camber line.
   (compute-zero-tangent
    (surface-type)
    (let* ((spec (the airfoil-spec))
           (dcamber-at-0 (funcall (fifth spec) 0.0d0 (first spec) (second spec)))
           (theta (atan dcamber-at-0))
           (ct (cos theta))
           (st (sin theta)))
      (if (eq surface-type :upper)
          (make-vector (- st) ct 0.0d0)
          (make-vector st (- ct) 0.0d0))))))
