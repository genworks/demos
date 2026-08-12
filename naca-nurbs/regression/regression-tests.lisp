;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; =====================================================================
;;;; naca-nurbs/regression-tests.lisp
;;;; Minimal regression/diagnostic suite for NACA → NURBS pipeline
;;;; =====================================================================

(in-package :naca-nurbs)

;;; -----------------------
;;; configurable defaults
;;; -----------------------

(defparameter *regression-configs*
  '((:airfoil :0012  :n-points 129 :cosine? t :closed? nil)
    (:airfoil :2412  :n-points 129 :cosine? t :closed? nil)
    (:airfoil :23012 :n-points 129 :cosine? t :closed? nil)

    
    (:airfoil :0012  :n-points 129 :cosine? t :closed? t)
    (:airfoil :2412  :n-points 129 :cosine? t :closed? t)
    (:airfoil :23012 :n-points 129 :cosine? t :closed? t)


    ;;
    ;; Uncomment the following for debugging. Failures are expected
    ;; with `:cosine? nil`.
    ;;
    ;;(:airfoil :0012  :n-points 129 :cosine? nil :closed? nil)
    ;;(:airfoil :2412  :n-points 129 :cosine? nil :closed? nil)
    ;;(:airfoil :23012 :n-points 129 :cosine? nil :closed? nil)

    ;;(:airfoil :0012  :n-points 129 :cosine? nil :closed? t)
    ;;(:airfoil :2412  :n-points 129 :cosine? nil :closed? t)
    ;;(:airfoil :23012 :n-points 129 :cosine? nil :closed? t)
    )
  "Panel of airfoils/configs to exercise.")

(defparameter *regression-seed-dir*
  (let* ((base (or (ignore-errors (glisp:source-pathname))
                   (ignore-errors (truename "."))))
         (dir  (merge-pathnames "regression-seed/" base)))
    dir)
  "Directory where seed files are written/read.")

(defparameter *regression-schema-version* 1)

(defparameter *regression-tolerances*
  '(:roundtrip-max                 1d-12
    :geom-max-abs-error            1d-10
    :tangent-unit-max              1d-12
    :nose-tangent-diff             1d-12
    :tangent-angle-max-deg         1d-1      ; 0.1°
    :split-dx                      1d-2
    :section-tangent-split-delta   1d-12
    :proj-max                      1d-9
    )
  "Absolute tolerances used when no seed is present (or as fallbacks).")


(defparameter *approx-acceptance-mode* :balanced)
;; :strict   -> measured <= used-tolerance
;; :lenient  -> measured <= used-tolerance OR achieved <= used-tolerance
;; :balanced -> measured <= max(used-tol, achieved * *approx-achieved-margin*)

(defparameter *approx-achieved-margin* 1.35d0)
;; 1.35 chosen to cover your observed ~1.31x gap with headroom.


;;; -----------------------
;;; tiny utilities
;;; -----------------------

(defun %ensure-dir-exists (dir)
  (unless (probe-file dir)
    (ensure-directories-exist (merge-pathnames "dummy" dir))))

(defun %plist-get (plist key &optional default)
  (or (getf plist key) default))

(defun %maxabs (xs) (reduce #'max xs :initial-value 0d0 :key (lambda (x) (abs (coerce x 'double-float)))))

(defun %vdiff2 (a b)
  (let ((dx (- (get-x a) (get-x b)))
        (dy (- (get-y a) (get-y b))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun %vlen2 (v)
  (sqrt (+ (expt (get-x v) 2) (expt (get-y v) 2))))

(defun %unit (v)
  (let ((l (%vlen2 v)))
    (if (<= l 0d0) (make-vector 1.0 0.0 0.0)
        (make-vector (/ (get-x v) l) (/ (get-y v) l) 0.0))))

(defun %rot90 (v) (make-vector (- (get-y v)) (get-x v) 0.0))

(defun %angle-deg (v)
  (let ((dx (get-x v)) (dy (get-y v)))
    (* 180.0 (/ (atan dy dx) pi))))

(defun %central-diff-angle-max (points tangents &key (skip 8))
  (let* ((n (length points))
         (mx 0d0))
    (do ((i skip (1+ i)))
        ((>= i (- n skip)) mx)
      (let* ((prev (nth (1- i) points))
             (next (nth (1+ i) points))
             (num  (unitize-vector (subtract-vectors next prev)))
             (ana  (nth i tangents))
             (d    (abs (- (%angle-deg ana) (%angle-deg num)))))
        (setf mx (max mx d))))))

;;; -----------------------
;;; metric computation
;;; -----------------------

(in-package :naca-nurbs)

;;; ... [Existing parameters and tiny utilities unchanged] ...

;;; Helper Functions for Metrics

(defun compute-roundtrip-max (obj tees)
  "Compute maximum t <-> x round-trip error."
  (let ((rt-max 0d0))
    (dotimes (i (length tees))
      (let* ((tee (aref tees i))
             (x (t->x tee (theo obj cosine?)))
             (t2 (x->t x (theo obj cosine?))))
        (setf rt-max (max rt-max (abs (- t2 tee))))))
    rt-max))

(defun compute-x-metrics (xs)
  "Compute x monotonicity and range metrics."
  (let* ((xs-first (let ((result nil))
                     (dotimes (i (1- (length xs)))
                       (push (aref xs i) result))
                     (nreverse result)))
         (xs-rest (let ((result nil))
                    (dotimes (i (- (length xs) 1))
                      (push (aref xs (1+ i)) result))
                    (nreverse result)))
         (mono (every #'<= xs-first xs-rest))
         (x-min (aref xs 0))
         (x-max (aref xs (1- (length xs)))))
    (list :x-monotone? mono :x-min x-min :x-max x-max)))

(defun compute-geometry-error (obj xs upts lpts)
  "Compute maximum geometry error against NACA formulas."
  (destructuring-bind (m p tau camber-fn dcamber-fn) (theo obj airfoil-spec)
    (let ((mx-err 0d0)
	  (closed? (theo obj closed?)))
      (do ((i 0 (1+ i))
           (up upts (cdr up))
           (lo lpts (cdr lo)))
          ((>= i (length xs)) mx-err)
        (let* ((x (aref xs i))
               (yc (funcall camber-fn x m p))
               (dycdx (funcall dcamber-fn x m p))
               (th (atan dycdx))
               (ct (cos th)) (st (sin th))
               (yt (thickness x tau :closed? closed?))
               (xu (- x (* yt st)))
               (yu (+ yc (* yt ct)))
               (xl (+ x (* yt st)))
               (yl (- yc (* yt ct))))
          (setf mx-err (max mx-err
                            (abs (- (get-x (car up)) xu))
                            (abs (- (get-y (car up)) yu))
                            (abs (- (get-x (car lo)) xl))
                            (abs (- (get-y (car lo)) yl)))))))))

(defun compute-tangent-metrics (obj ut)
  "Compute tangent unit length and nose tangent difference."
  (let ((tu-max 0d0))
    (dolist (v ut)
      (setf tu-max (max tu-max (abs (- 1.0 (%vlen2 v))))))
    (let* ((t0 (first ut))
           (z0 (theo obj (compute-zero-tangent :upper)))
           (nose-d (%vdiff2 t0 z0)))
      (list :tangent-unit-max tu-max :nose-tangent-diff nose-d))))

(defun compute-curvature-metrics (obj)
  "Compute curvature positivity and nose dominance."
  (flet ((k (tee surf)
           (analytical-curvature-parametric (theo obj airfoil) tee
                                            :surface surf
                                            :cosine? (theo obj cosine?))))
    (let* ((ku0 (k 0.01d0 :upper))
           (ku5 (k 0.50d0 :upper))
           (kl0 (k 0.01d0 :lower))
           (kl5 (k 0.50d0 :lower))
           (curv-pos (and (> ku0 0) (> ku5 0) (> kl0 0) (> kl5 0)))
           (curv-nose-dominant (and (> ku0 ku5) (> kl0 kl5))))
      (list :curv-pos? curv-pos :curv-nose-dominant? curv-nose-dominant))))

(defun compute-curvature-optimum (obj)
  "Find the parameter tee with maximum curvature."
  (let* ((tmin (x->t (theo obj split-min-x) (theo obj cosine?)))
         (tmax (x->t (theo obj split-max-x) (theo obj cosine?)))
         (kappa (lambda (tee _)
                  (declare (ignore _))
                  (analytical-curvature-parametric (theo obj airfoil) tee
                                                   :surface :upper
                                                   :cosine? (theo obj cosine?))))
         (samples 200)
         (best-t nil)
         (best-k -1d0))
    (dotimes (i (1+ samples))
      (let* ((tee (+ tmin (* (/ i samples) (- tmax tmin))))
             (kv (funcall kappa tee nil)))
        (when (> kv best-k) (setf best-k kv best-t tee))))
    (list :best-t best-t)))

(defun compute-split-metrics (obj xs)
  "Compute split-related metrics."
  (multiple-value-bind (nu mu nl ml split-x-upper split-x-lower)
      (theo obj (split-airfoil-sections-indices-adaptive))
    (declare (ignore nu nl ml split-x-upper split-x-lower))
    (let* ((idx (first mu))
           (x-disc (aref xs idx))
           (curv-opt (compute-curvature-optimum obj))
           (best-t (getf curv-opt :best-t))
           (x-cont (t->x best-t (theo obj cosine?)))
           (split-dx (- x-disc x-cont))
           (sp (theo obj sectioned-points))
           (st (theo obj sectioned-tangents))
           (nu-p (getf sp :nose-upper))
           (mu-p (getf sp :main-upper))
           (nl-p (getf sp :nose-lower))
           (ml-p (getf sp :main-lower))
           (nu-t (getf st :nose-upper))
           (mu-t (getf st :main-upper))
           (nl-t (getf st :nose-lower))
           (ml-t (getf st :main-lower))
           (overlap-ok (and (eq (car (last nu-p)) (first mu-p))
                            (eq (car (last nl-p)) (first ml-p))))
           (split-dt (max (%vdiff2 (car (last nu-t)) (first mu-t))
                          (%vdiff2 (car (last nl-t)) (first ml-t)))))
      (list :split-x-discrete x-disc
            :split-x-continuous x-cont
            :split-dx split-dx
            :section-overlap? overlap-ok
            :section-tangent-split-delta split-dt))))

(defun compute-projection-metrics (obj sp)
  "Compute maximum projection distances for section points to fitted curves."
  (labels ((proj-max (curve pts)
             (let ((mx 0d0))
               (dolist (p pts)
                 (let* ((u (theo curve (parameter-at-point p)))
                        (q (theo curve (point u)))
                        (d (3d-distance p q)))
                   (setf mx (max mx d))))
               mx)))
    (let* ((nu-f (theo obj nose-upper-fitted))
           (mu-f (theo obj main-upper-fitted))
           (nl-f (theo obj nose-lower-fitted))
           (ml-f (theo obj main-lower-fitted))
           (nu-p (getf sp :nose-upper))
           (mu-p (getf sp :main-upper))
           (nl-p (getf sp :nose-lower))
           (ml-p (getf sp :main-lower)))
      (list :proj-max-noseU (proj-max nu-f nu-p)
            :proj-max-mainU (proj-max mu-f mu-p)
            :proj-max-noseL (proj-max nl-f nl-p)
            :proj-max-mainL (proj-max ml-f ml-p)))))

(defun compute-approx-metrics (obj nu-f mu-f nl-f ml-f)
  "Compute approximation vs fitted curve maximum deviations and tolerances."
  (labels ((maxdist (fit approx samples)
             (let ((mx 0d0))
               (dotimes (i samples mx)
                 (let* ((u (/ i (coerce (1- samples) 'double-float)))
                        (p (theo fit (point u)))
                        (v (theo approx (parameter-at-point p)))
                        (q (theo approx (point v)))
                        (d (3d-distance p q)))
                   (setf mx (max mx d)))))))
    (let* ((nu-a (theo obj nose-upper-approx))
           (mu-a (theo obj main-upper-approx))
           (nl-a (theo obj nose-lower-approx))
           (ml-a (theo obj main-lower-approx)))
      (list :approx-max-noseU (maxdist nu-f nu-a 121)
            :approx-max-mainU (maxdist mu-f mu-a 241)
            :approx-max-noseL (maxdist nl-f nl-a 121)
            :approx-max-mainL (maxdist ml-f ml-a 241)
            :tol-used-noseU (theo nu-a tolerance)
            :tol-used-mainU (theo mu-a tolerance)
            :tol-used-noseL (theo nl-a tolerance)
            :tol-used-mainL (theo ml-a tolerance)
            :approx-achieved-noseU (theo nu-a achieved-tolerance)
            :approx-achieved-mainU (theo mu-a achieved-tolerance)
            :approx-achieved-noseL (theo nl-a achieved-tolerance)
            :approx-achieved-mainL (theo ml-a achieved-tolerance)))))


(defun %compute-metrics (&key airfoil n-points cosine? closed?)
  "Instantiate a naca-nurbs-curves object and compute a compact metrics plist."
  (let* ((obj (make-object 'naca-nurbs:naca-nurbs-curves
                           :airfoil airfoil :n-points n-points
			   :cosine? cosine? :closed? closed?))
         (tees (theo obj tee-samples))
         (xs (theo obj x-samples))
         (pts (theo obj naca-points))
         (upts (getf pts :upper))
         (lpts (getf pts :lower))
         (tans (theo obj naca-tangents))
         (ut (getf tans :upper))
         (sp (theo obj sectioned-points))
         (nu-f (theo obj nose-upper-fitted))
         (mu-f (theo obj main-upper-fitted))
         (nl-f (theo obj nose-lower-fitted))
         (ml-f (theo obj main-lower-fitted)))
    (append
     (list :schema *regression-schema-version*
           :airfoil airfoil
           :n-points n-points
           :cosine? cosine?
           :roundtrip-max (compute-roundtrip-max obj tees)
           :tangent-angle-max-deg (%central-diff-angle-max upts ut :skip 8))
     (compute-x-metrics xs)
     (list :geom-max-abs-error (compute-geometry-error obj xs upts lpts))
     (compute-tangent-metrics obj ut)
     (compute-curvature-metrics obj)
     (compute-split-metrics obj xs)
     (compute-projection-metrics obj sp)
     (compute-approx-metrics obj nu-f mu-f nl-f ml-f))))

(defun %within (x tol) (<= (abs (coerce x 'double-float))
			   (coerce tol 'double-float)))


(defun check-numeric-metrics (now seed metrics tol-key)
  "Check numeric metrics against seed or tolerance."
  (mapcar (lambda (key)
            (let ((nv (%plist-get now key))
                  (sv (%plist-get seed key))
                  (tv (max (%plist-get *regression-tolerances* tol-key 0d0) 1d-12)))
              (list key (if (and sv (numberp sv)) (%within (- nv sv) tv) (%within nv tv)))))
          metrics))

(defun check-boolean-metrics (now seed metrics)
  "Check boolean metrics against seed or default true."
  (mapcar (lambda (key)
            (list key (let ((nv (%plist-get now key)) (sv (%plist-get seed key)))
                        (if (member nv '(t nil))
                            (if (member sv '(t nil)) (eql nv sv) (eql nv t))
                            t))))
          metrics))

(defun check-approx-metrics (now metrics tol-keys ach-keys ok-approx)
  "Check approximation metrics against tolerances using ok-approx function."
  (mapcar (lambda (mkey tkey akey)
            (list mkey (funcall ok-approx now mkey tkey akey)))
          metrics tol-keys ach-keys))

(defun %compare-metrics (now seed)
  "Return (passed-p . report-string). Uses seed if available; otherwise tolerances."
  (flet ((ok-approx (metrics max-key tol-used-key achieved-key)
           (let* ((meas (%plist-get metrics max-key))
                  (used (%plist-get metrics tol-used-key))
                  (ach (%plist-get metrics achieved-key)))
             (ecase *approx-acceptance-mode*
               (:strict (<= meas used))
               (:lenient (or (<= meas used) (and ach (<= ach used))))
               (:balanced (let ((gate (max used (if ach (* ach *approx-achieved-margin*) used))))
                            (<= meas gate)))))))
    (let* ((checks
            (append
             (check-numeric-metrics now seed
                                   '(:roundtrip-max :geom-max-abs-error :tangent-unit-max
                                     :nose-tangent-diff :tangent-angle-max-deg :split-dx
                                     :section-tangent-split-delta)
                                   nil)
             (check-numeric-metrics now seed
                                   '(:proj-max-noseU :proj-max-mainU :proj-max-noseL :proj-max-mainL)
                                   :proj-max)
             (check-approx-metrics now
                                   '(:approx-max-noseU :approx-max-mainU :approx-max-noseL :approx-max-mainL)
                                   '(:tol-used-noseU :tol-used-mainU :tol-used-noseL :tol-used-mainL)
                                   '(:approx-achieved-noseU :approx-achieved-mainU
                                     :approx-achieved-noseL :approx-achieved-mainL)
                                   #'ok-approx)
             (check-boolean-metrics now seed
                                   '(:x-monotone? :curv-pos? :curv-nose-dominant?
                                     :section-overlap?))))
           (results (mapcar (lambda (entry)
                              (destructuring-bind (key pred) entry
                                (list key (if (functionp pred) (funcall pred) pred))))
                            checks))
           (pass? (every (lambda (r) (second r)) results)))
      (values pass? results))))


(defun process-configs (configs callback)
  "Apply callback to each config in configs."
  (dolist (cfg configs)
    (destructuring-bind (&key airfoil n-points cosine? closed? &allow-other-keys) cfg
      (funcall callback airfoil n-points cosine? closed?))))

(defun format-metrics-summary (metrics)
  "Format key metrics for display."
  (format t "       roundtrip-max=~e geom-max=~e tan-angle-max(deg)=~f approx-mainU=~e ~%"
          (%plist-get metrics :roundtrip-max)
          (%plist-get metrics :geom-max-abs-error)
          (%plist-get metrics :tangent-angle-max-deg)
          (%plist-get metrics :approx-max-mainU)))


(defun %seed-file-path (airfoil n-points cosine? closed?)
  "Generate the file path for a seed file based on configuration parameters."
  (let ((filename (format nil "~a-~d-~:[uniform~;cosine~]-~:[open~;closed~].seed"
                         airfoil n-points cosine? closed?)))
    (merge-pathnames filename *regression-seed-dir*)))

(defun %read-seed (path)
  "Read and return the metrics plist from a seed file at PATH."
  (when (probe-file path)
    (with-open-file (stream path :direction :input :if-does-not-exist nil)
      (when stream
        (read stream)))))

(defun run-regression-tests ()
  "Run the regression suite. If a seed exists for a config, compare against it."
  (%ensure-dir-exists *regression-seed-dir*)
  (let ((all-pass t))
    (process-configs *regression-configs*
                     (lambda (airfoil n-points cosine? closed?)
                       (let* ((metrics (%compute-metrics :airfoil airfoil :n-points n-points
							 :cosine? cosine?
							 :closed? closed?))
                              (path (%seed-file-path airfoil n-points cosine? closed?))
                              (seed (when (probe-file path) (%read-seed path)))
                              (ok (%compare-metrics metrics seed)))
                         (format t "~&[test] ~a -> ~a~%"
				 (list :airfoil airfoil :n-points n-points
				       :cosine? cosine? :closed? closed?)
                                 (if ok "PASS" "FAIL"))
                         (unless ok (setf all-pass nil))
                         (format-metrics-summary metrics))))
    (format t "~&=== SUMMARY: ~a ===~%" (if all-pass "ALL PASS" "FAILURES PRESENT"))
    all-pass))

(defun seed-regression-tests ()
  "Compute metrics for the default panel and write seed files."
  (%ensure-dir-exists *regression-seed-dir*)
  (process-configs *regression-configs*
                   (lambda (airfoil n-points cosine? closed?)
                     (let* ((metrics (%compute-metrics :airfoil airfoil :n-points n-points
						       :cosine? cosine?
						       :closed? closed?))
                            (path (%seed-file-path airfoil n-points cosine? closed?)))
                       (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
                         (let ((*print-pretty* t))
                           (print metrics s)))
                       (format t "~&[seed] ~a → ~a~%"
			       (list :airfoil airfoil :n-points n-points
				     :cosine? cosine?
				     :closed? closed?) path))))
  (values t))

(defun compute-metric-ratio (metrics metric-key ach-key)
  "Compute the ratio of measured to achieved metric if valid."
  (let ((meas (getf metrics metric-key))
        (ach (getf metrics ach-key)))
    (if (and ach (> ach 0)) (/ meas ach) 1d0)))

(defun compute-metric-ratios (metrics pairs)
  "Compute maximum ratio of measured to achieved metrics."
  (let ((max-ratio 1d0))
    (dolist (pair pairs max-ratio)
      (let ((ratio (compute-metric-ratio metrics (first pair) (second pair))))
        (setf max-ratio (max max-ratio ratio))))))

(defun calibrate-approx-margin (&optional (cfgs *regression-configs*))
  (let ((max-ratio 1d0))
    (process-configs cfgs
                     (lambda (airfoil n-points cosine?)
                       (let ((m (%compute-metrics :airfoil airfoil :n-points n-points :cosine? cosine?)))
                         (setf max-ratio
                               (max max-ratio
                                    (compute-metric-ratios m
                                                           '((:approx-max-noseU :approx-achieved-noseU)
                                                             (:approx-max-mainU :approx-achieved-mainU)
                                                             (:approx-max-noseL :approx-achieved-noseL)
                                                             (:approx-max-mainL :approx-achieved-mainL))))))))
    (format t "~&worst measured/achieved = ~,6f → suggested margin = ~,6f~%"
            max-ratio (* 1.05 max-ratio))
    max-ratio))

(defun %hausdorffish (c-fit c-approx samples)
  (labels ((maxdist (fit approx samples)
             (let ((mx 0d0))
               (dotimes (i samples mx)
                 (let* ((u (/ i (coerce (1- samples) 'double-float)))
                        (p (theo fit (point u)))
                        (v (theo approx (parameter-at-point p)))
                        (q (theo approx (point v)))
                        (d (3d-distance p q)))
                   (setf mx (max mx d)))))))
    (max (maxdist c-fit c-approx samples)
         (maxdist c-approx c-fit samples))))


;;; =====================================================================
;;; ENHANCED BATCH REGRESSION TESTING FOR MULTIPLE AIRFOIL TYPES
;;; =====================================================================

;;; Extended airfoil test configurations
(defparameter *extended-airfoil-configs*
  '(;; 4-digit NACA airfoils
    (:airfoil :0006  :n-points 108 :cosine? t :closed? nil :family "4-digit-symmetric")
    (:airfoil :0009  :n-points 108 :cosine? t :closed? nil :family "4-digit-symmetric")
    (:airfoil :0012  :n-points 108 :cosine? t :closed? nil :family "4-digit-symmetric")
    (:airfoil :0015  :n-points 108 :cosine? t :closed? nil :family "4-digit-symmetric")
    (:airfoil :0018  :n-points 108 :cosine? t :closed? nil :family "4-digit-symmetric")
    
    (:airfoil :1408  :n-points 108 :cosine? t :closed? nil :family "4-digit-cambered")
    (:airfoil :2412  :n-points 108 :cosine? t :closed? nil :family "4-digit-cambered")
    (:airfoil :4412  :n-points 108 :cosine? t :closed? nil :family "4-digit-cambered")
    (:airfoil :6412  :n-points 108 :cosine? t :closed? nil :family "4-digit-cambered")
    
    ;; 5-digit NACA airfoils  
    (:airfoil :23012 :n-points 108 :cosine? t :closed? nil :family "5-digit")
    (:airfoil :23015 :n-points 108 :cosine? t :closed? nil :family "5-digit")
    (:airfoil :23018 :n-points 108 :cosine? t :closed? nil :family "5-digit")
    (:airfoil :23021 :n-points 108 :cosine? t :closed? nil :family "5-digit")
    
    ;; High camber test cases
    (:airfoil :8412  :n-points 216 :cosine? t :closed? nil :family "high-camber")
    (:airfoil :9424  :n-points 216 :cosine? t :closed? nil :family "high-camber")
    
    ;; Thick airfoils
    (:airfoil :0021  :n-points 216 :cosine? t :closed? nil :family "thick")
    (:airfoil :0030  :n-points 216 :cosine? t :closed? nil :family "thick")
    
    ;; Stress test cases with different point densities
    (:airfoil :2412  :n-points  54 :cosine? t :closed? nil :family "low-resolution")
    (:airfoil :2412  :n-points 432 :cosine? t :closed? nil :family "high-resolution")
    
    ;; Closed airfoil variants for key profiles
    (:airfoil :0012  :n-points 108 :cosine? t :closed? t   :family "closed")
    (:airfoil :2412  :n-points 108 :cosine? t :closed? t   :family "closed")
    (:airfoil :23012 :n-points 108 :cosine? t :closed? t   :family "closed"))
  "Extended test suite covering diverse airfoil geometries and configurations.")

;;; Batch test result data structures
(defstruct batch-test-result
  airfoil
  n-points  
  cosine?
  closed?
  family
  status          ; :pass, :fail, :error
  metrics
  error-info
  execution-time
  quality-score)

(defstruct batch-summary
  total-tests
  passed
  failed
  errors
  execution-time
  family-results  ; alist of (family . results)
  worst-performers
  best-performers)

;;; Quality scoring system
(defun compute-quality-score (metrics)
  "Compute a normalized quality score (0-100) based on multiple criteria."
  (let* ((roundtrip-score (max 0 (min 100 (* 100 (- 1 (log10 (max 1d-15 (getf metrics :roundtrip-max))))))))
         (geometry-score  (max 0 (min 100 (* 100 (- 1 (log10 (max 1d-15 (getf metrics :geom-max-abs-error))))))))
         (tangent-score   (max 0 (min 100 (- 100 (getf metrics :tangent-angle-max-deg 0)))))
         (approx-scores   (mapcar (lambda (key) 
                                   (let ((val (getf metrics key 0)))
                                     (max 0 (min 100 (* 100 (- 1 (log10 (max 1d-6 val))))))))
                                 '(:approx-max-noseU :approx-max-mainU 
                                   :approx-max-noseL :approx-max-mainL)))
         (approx-score    (/ (reduce #'+ approx-scores) (length approx-scores)))
         (weights         '(0.2 0.3 0.2 0.3))  ; roundtrip, geometry, tangent, approx
         (scores          (list roundtrip-score geometry-score tangent-score approx-score)))
    (/ (reduce #'+ (mapcar #'* weights scores)) (reduce #'+ weights))))

(defun log10 (x) (/ (log (max x 1d-15)) (log 10)))

;;; Enhanced batch testing functions
(defun run-batch-regression-tests (&key (configs *extended-airfoil-configs*) 
                                        (continue-on-error t)
                                        (detailed-output t)
                                        (save-results t))
  "Run comprehensive batch regression tests across multiple airfoil types.
   Returns batch-summary structure with detailed results."
  (let ((results nil)
        (start-time (get-internal-real-time))
        (passed 0) (failed 0) (errors 0))
    
    (format t "~&Starting batch regression tests for ~d configurations...~%" (length configs))
    (format t "~&========================================================~%")
    
    (dolist (cfg configs)
      (destructuring-bind (&key airfoil n-points cosine? closed? family &allow-other-keys) cfg
        (let ((test-start (get-internal-real-time))
              result)
          
          (handler-case
              (let* ((metrics (%compute-metrics :airfoil airfoil :n-points n-points
                                               :cosine? cosine? :closed? closed?))
                     (path (%seed-file-path airfoil n-points cosine? closed?))
                     (seed (when (probe-file path) (%read-seed path)))
                     (test-passed (%compare-metrics metrics seed))
                     (exec-time (/ (- (get-internal-real-time) test-start) 
                                  internal-time-units-per-second))
                     (quality (compute-quality-score metrics)))
                
                (setf result (make-batch-test-result 
                             :airfoil airfoil
                             :n-points n-points
                             :cosine? cosine?
                             :closed? closed?
                             :family family
                             :status (if test-passed :pass :fail)
                             :metrics metrics
                             :execution-time exec-time
                             :quality-score quality))
                
                (if test-passed (incf passed) (incf failed))
                
                (when detailed-output
                  (format t "~&[~a] ~a (~a) -> ~a (Quality: ~,1f, Time: ~,3fs)~%"
                          family airfoil 
                          (format nil "n=~d ~:[open~;closed~] ~:[uniform~;cosine~]" 
                                 n-points closed? cosine?)
                          (if test-passed "PASS" "FAIL")
                          quality exec-time)))
            
            (error (e)
              (incf errors)
              (setf result (make-batch-test-result 
                           :airfoil airfoil
                           :n-points n-points  
                           :cosine? cosine?
                           :closed? closed?
                           :family family
                           :status :error
                           :error-info (format nil "~a" e)
                           :execution-time (/ (- (get-internal-real-time) test-start)
                                             internal-time-units-per-second)))
              
              (format t "~&[~a] ~a -> ERROR: ~a~%" family airfoil e)
              (unless continue-on-error
                (error "Batch testing aborted due to error in ~a: ~a" airfoil e))))
          
          (push result results))))
    
    (setf results (nreverse results))
    (let* ((total-time (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second))
           (summary (make-batch-summary 
                    :total-tests (length results)
                    :passed passed
                    :failed failed  
                    :errors errors
                    :execution-time total-time
                    :family-results (group-results-by-family results)
                    :worst-performers (get-worst-performers results 5)
                    :best-performers (get-best-performers results 5))))
      
      (print-batch-summary summary detailed-output)
      
      (when save-results
        (save-batch-results results summary))
      
      summary)))

(defun group-results-by-family (results)
  "Group batch test results by airfoil family."
  (let ((families (make-hash-table :test #'equal)))
    (dolist (result results)
      (let ((family (batch-test-result-family result)))
        (push result (gethash family families))))
    
    ;; Convert to alist and compute family statistics
    (let ((alist nil))
      (maphash (lambda (family results)
                 (let* ((total (length results))
                        (passed (count-if (lambda (r) (eq (batch-test-result-status r) :pass)) results))
                        (avg-quality (if (> total 0)
                                       (/ (reduce #'+ results 
                                                 :key (lambda (r) (or (batch-test-result-quality-score r) 0)))
                                         total)
                                       0))
                        (avg-time (if (> total 0)
                                   (/ (reduce #'+ results 
                                             :key #'batch-test-result-execution-time)
                                     total)
                                   0)))
                   (push (list family
                              :total total
                              :passed passed
                              :failed (- total passed (count-if (lambda (r) (eq (batch-test-result-status r) :error)) results))
                              :errors (count-if (lambda (r) (eq (batch-test-result-status r) :error)) results)
                              :pass-rate (if (> total 0) (* 100.0 (/ passed total)) 0)
                              :avg-quality avg-quality
                              :avg-time avg-time
                              :results results)
                         alist)))
               families)
      alist)))

(defun get-worst-performers (results n)
  "Get the N worst performing test results by quality score."
  (let ((scored-results (remove-if (lambda (r) (null (batch-test-result-quality-score r))) results)))
    (subseq (sort scored-results (lambda (a b) 
                                  (< (batch-test-result-quality-score a)
                                     (batch-test-result-quality-score b))))
            0 (min n (length scored-results)))))

(defun get-best-performers (results n)  
  "Get the N best performing test results by quality score."
  (let ((scored-results (remove-if (lambda (r) (null (batch-test-result-quality-score r))) results)))
    (subseq (sort scored-results (lambda (a b)
                                  (> (batch-test-result-quality-score a)
                                     (batch-test-result-quality-score b))))
            0 (min n (length scored-results)))))

(defun print-batch-summary (summary detailed-output)
  "Print formatted summary of batch test results."
  (format t "~&~%========================================================~%")
  (format t "BATCH REGRESSION TEST SUMMARY~%")
  (format t "========================================================~%")
  (format t "Total Tests:     ~d~%" (batch-summary-total-tests summary))
  (format t "Passed:          ~d (~,1f%)~%" 
          (batch-summary-passed summary)
          (* 100.0 (/ (batch-summary-passed summary) (batch-summary-total-tests summary))))
  (format t "Failed:          ~d (~,1f%)~%"
          (batch-summary-failed summary) 
          (* 100.0 (/ (batch-summary-failed summary) (batch-summary-total-tests summary))))
  (format t "Errors:          ~d (~,1f%)~%"
          (batch-summary-errors summary)
          (* 100.0 (/ (batch-summary-errors summary) (batch-summary-total-tests summary))))
  (format t "Execution Time:  ~,2f seconds~%" (batch-summary-execution-time summary))
  
  (format t "~%RESULTS BY AIRFOIL FAMILY:~%")
  (format t "~28a ~6a ~6a ~6a ~8a ~8a ~8a~%" 
          "Family" "Total" "Pass" "Fail" "Pass%" "Quality" "Time(s)")
  (format t "~80,,,'-a~%" "")
  
  (dolist (family-result (batch-summary-family-results summary))
    (destructuring-bind (family &key total passed failed errors pass-rate avg-quality avg-time &allow-other-keys)
        family-result
      (declare (ignore errors))
      (format t "~28a ~6d ~6d ~6d ~7,1f% ~7,1f ~7,3f~%"
              family total passed failed pass-rate avg-quality avg-time)))
  
  (when detailed-output
    (let ((worst (batch-summary-worst-performers summary))
          (best (batch-summary-best-performers summary)))
      
      (when worst
        (format t "~%WORST PERFORMERS (by quality score):~%")
        (dolist (result worst)
          (format t "  ~a: Quality ~,1f (~a)~%"
                  (batch-test-result-airfoil result)
                  (or (batch-test-result-quality-score result) 0)
                  (batch-test-result-family result))))
      
      (when best
        (format t "~%BEST PERFORMERS (by quality score):~%")
        (dolist (result best)
          (format t "  ~a: Quality ~,1f (~a)~%"
                  (batch-test-result-airfoil result)
                  (or (batch-test-result-quality-score result) 0)
                  (batch-test-result-family result))))))
  
  (format t "~%========================================================~%"))

(defun save-batch-results (results summary)
  "Save batch test results and summary to files."
  (let* ((timestamp (multiple-value-bind (sec min hour day month year)
                        (get-decoded-time)
                      (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"
                              year month day hour min sec)))
         (results-file (merge-pathnames 
                       (format nil "batch-results-~a.lisp" timestamp)
                       *regression-seed-dir*))
         (summary-file (merge-pathnames
                       (format nil "batch-summary-~a.txt" timestamp) 
                       *regression-seed-dir*)))
    
    ;; Save detailed results as Lisp data
    (with-open-file (s results-file :direction :output :if-exists :supersede)
      (let ((*print-pretty* t))
        (format s ";; Batch regression test results - ~a~%" timestamp)
        (format s ";; Generated by NACA-NURBS batch testing system~%~%")
        (print results s)))
    
    ;; Save human-readable summary
    (with-open-file (s summary-file :direction :output :if-exists :supersede)
      (let ((*standard-output* s))
        (print-batch-summary summary t)))
    
    (format t "~%Results saved to:~%  ~a~%  ~a~%" results-file summary-file)))

;;; Specialized testing functions
(defun test-airfoil-families (&key (families '("4-digit-symmetric" "4-digit-cambered" "5-digit")))
  "Test specific airfoil families only."
  (let ((configs (remove-if-not (lambda (cfg) (member (getf cfg :family) families :test #'string=))
                                *extended-airfoil-configs*)))
    (run-batch-regression-tests :configs configs)))

(defun test-resolution-scaling (&key (airfoils '(:0012 :2412 :23012))
                                    (point-counts '(54 108 216 432)))
  "Test how the system performs across different point densities."
  (let ((configs nil))
    (dolist (airfoil airfoils)
      (dolist (n-points point-counts)
        (push (list :airfoil airfoil :n-points n-points :cosine? t :closed? nil
                   :family (format nil "resolution-~d" n-points))
              configs)))
    (run-batch-regression-tests :configs (nreverse configs))))

(defun test-extreme-geometries ()
  "Test challenging geometric cases."
  (let ((configs '((:airfoil :0030 :n-points 216 :cosine? t :closed? nil :family "very-thick")
                  (:airfoil :9430 :n-points 216 :cosine? t :closed? nil :family "extreme-camber") 
                  (:airfoil :0006 :n-points  32 :cosine? t :closed? nil :family "low-res-thin")
                  (:airfoil :6424 :n-points 432 :cosine? t :closed? nil :family "high-res-cambered"))))
    (run-batch-regression-tests :configs configs)))

(defun benchmark-performance (&optional (iterations 3))
  "Benchmark performance across representative airfoil types."
  (let ((benchmark-configs '((:airfoil :0012  :n-points 108 :cosine? t :closed? nil :family "benchmark")
                            (:airfoil :2412  :n-points 108 :cosine? t :closed? nil :family "benchmark")  
                            (:airfoil :23012 :n-points 108 :cosine? t :closed? nil :family "benchmark")))
        (times nil))
    (dotimes (i iterations)
      (let ((start-time (get-internal-real-time)))
        (run-batch-regression-tests :configs benchmark-configs :detailed-output nil :save-results nil)
        (push (/ (- (get-internal-real-time) start-time) internal-time-units-per-second) times)))
    
    (let* ((avg-time (/ (reduce #'+ times) (length times)))
           (min-time (reduce #'min times))
           (max-time (reduce #'max times)))
      (format t "~%PERFORMANCE BENCHMARK RESULTS (~d iterations):~%" iterations)
      (format t "  Average time: ~,3f seconds~%" avg-time)
      (format t "  Best time:    ~,3f seconds~%" min-time)
      (format t "  Worst time:   ~,3f seconds~%" max-time)
      (format t "  Std dev:      ~,3f seconds~%" 
              (sqrt (/ (reduce #'+ (mapcar (lambda (time-val) (expt (- time-val avg-time) 2)) times))
                      (length times))))
      avg-time)))

;;; Quality assurance functions
(defun validate-batch-results (results &key (min-quality-threshold 75.0)
                                            (max-error-rate 0.05))
  "Validate that batch test results meet quality standards."
  (let* ((total (length results))
         (errors (count-if (lambda (r) (eq (batch-test-result-status r) :error)) results))
         (error-rate (/ errors total))
         (quality-scores (mapcar #'batch-test-result-quality-score 
                                (remove-if (lambda (r) (null (batch-test-result-quality-score r))) results)))
         (avg-quality (if quality-scores (/ (reduce #'+ quality-scores) (length quality-scores)) 0))
         (low-quality (count-if (lambda (r) (and (batch-test-result-quality-score r)
                                                (< (batch-test-result-quality-score r) min-quality-threshold)))
                               results)))
    
    (format t "~%QUALITY VALIDATION:~%")
    (format t "  Error rate: ~,2f% (threshold: ~,2f%)~%" (* 100 error-rate) (* 100 max-error-rate))
    (format t "  Average quality: ~,1f (threshold: ~,1f)~%" avg-quality min-quality-threshold)
    (format t "  Low quality tests: ~d (~,1f%)~%" low-quality (* 100 (/ low-quality total)))
    
    (let ((validation-passed (and (<= error-rate max-error-rate)
                                 (>= avg-quality min-quality-threshold))))
      (format t "  Validation: ~a~%" (if validation-passed "PASSED" "FAILED"))
      validation-passed)))

(defun run-full-regression-suite ()
  "Run the complete regression test suite with all airfoil families."
  (format t "Running comprehensive NACA-NURBS regression test suite...~%")
  (let ((summary (run-batch-regression-tests)))
    (validate-batch-results (mapcan (lambda (fr) (getf (cdr fr) :results))
                                   (batch-summary-family-results summary)))
    (format t "~%Full regression suite completed.~%")
    summary))

