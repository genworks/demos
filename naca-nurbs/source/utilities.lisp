;; Copyright (C) 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :naca-nurbs)

;;;; ============================================================
;;;; Parameterization & Airfoil Utilities (index-based + parametric)
;;;; ============================================================

;; ----------------------------------------
;; 4-digit airfoil camber & derivatives
;; ----------------------------------------

(defun camber-4digit (x m p)
  "Camber y_c for 4-digit NACA, m max camber, p position."
  (if (< x p)
      (* (/ m (expt p 2)) (- (* 2 p x) (expt x 2)))
      (* (/ m (expt (- 1 p) 2)) (+ (- 1 (* 2 p)) (* 2 p x) (-  (expt x 2))))))

(defun dcamber-4digit (x m p)
  "dy_c/dx for 4-digit."
  (if (< x p)
      (* (/ m (expt p 2)) (- (* 2 p) (* 2 x)))
      (* (/ m (expt (- 1 p) 2)) (- (* 2 p) (* 2 x)))))

(defun d2camber-4digit (x m p)
  "Second derivative of 4-digit camber function"
  (if (< x p)
      (* (/ m (expt p 2)) -2)
      (* (/ m (expt (- 1 p) 2)) -2)))

(defun d3camber-4digit (x m p)
  (declare (ignore x m p))
  ;; 4-digit camber is piecewise quadratic ⇒ third derivative is 0 a.e.
  0.0d0)

;; ----------------------------------------
;; 5-digit families (table + generated fns)
;; ----------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *naca-5digit-camber-table*
    '(
      ("210" :m 0.0158d0 :p 0.2025d0 :k1/6 1.8230d0 :aft-const 0.02118d0)
      ("220" :m 0.0174d0 :p 0.2025d0 :k1/6 2.3910d0 :aft-const 0.02169d0)
      ("230" :m 0.0180d0 :p 0.2025d0 :k1/6 2.6595d0 :aft-const 0.02208d0)
      ("240" :m 0.0205d0 :p 0.2025d0 :k1/6 3.2300d0 :aft-const 0.02251d0)
      ("250" :m 0.0226d0 :p 0.2025d0 :k1/6 3.6880d0 :aft-const 0.02301d0)
      )))

(defmacro define-5digit-camber-family (prefix)
  "Defines camber, dcamber, d2camber, and d3camber for a NACA 5-digit prefix (e.g., 230).
   Handles x<0 with linear/constant extrapolation."
  (let ((camber-name   (intern (format nil "camber-~a"  prefix) :naca-nurbs))
        (dcamber-name  (intern (format nil "dcamber-~a" prefix) :naca-nurbs))
        (d2camber-name (intern (format nil "d2camber-~a" prefix) :naca-nurbs))
        (d3camber-name (intern (format nil "d3camber-~a" prefix) :naca-nurbs)))
    (destructuring-bind (&key m p k1/6 aft-const)
        (rest (assoc prefix *naca-5digit-camber-table* :test #'string=))
      (declare (ignore m))
      (let ((mu p))
        `(progn
           (defun ,camber-name (x &optional _m _p)
             (declare (ignore _m _p))
             (cond
               ((< x 0d0) (* x (* ,k1/6 0.11471d0))) ; linear extrapolation from slope at 0
               ((<= x ,mu)
                (* ,k1/6 (+ (expt x 3) (* -0.6075d0 (expt x 2)) (* 0.11471d0 x))))
               (t
                (* ,aft-const (- 1d0 x)))))

           (defun ,dcamber-name (x &optional _m _p)
             (declare (ignore _m _p))
             (cond
               ((< x 0d0) (* ,k1/6 0.11471d0)) ; constant slope for x<0
               ((<= x ,mu)
                (* ,k1/6 (+ (* 3d0 (expt x 2)) (* -2d0 0.6075d0 x) 0.11471d0)))
               (t
                (- ,aft-const))))

           (defun ,d2camber-name (x)
             (cond
               ((< x 0d0) 0d0)
               ((<= x ,mu)
                (* ,k1/6 (- (* 6d0 x) (* 2d0 0.6075d0))))
               (t 0d0)))

           ;; third derivative used by θ''(x)
           (defun ,d3camber-name (x)
             (cond
               ((< x 0d0) 0d0)
               ((<= x ,mu) (* ,k1/6 6d0))
               (t 0d0))))))))



(defmacro define-5digit-camber-families ()
  `(progn
     ,@(mapcar (lambda (row)
                 `(define-5digit-camber-family ,(first row)))
               *naca-5digit-camber-table*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-5digit-camber-families))

;; ----------------------------------------
;; Parsing helpers for 4-/5-digit series
;; ----------------------------------------

(defun get-naca-4digit-spec (airfoil)
  "Parse 4-digit NACA code like :2412 -> (m p tau camber-fn dcamber-fn)"
  (let* ((code-string (if (symbolp airfoil) (symbol-name airfoil) (string airfoil)))
         (digits-string (if (and (> (length code-string) 0)
                                 (char= (char code-string 0) #\:))
                            (subseq code-string 1)
                            code-string)))
    (unless (= (length digits-string) 4)
      (error "NACA code ~a must be exactly 4 digits" airfoil))
    (unless (every #'digit-char-p digits-string)
      (error "NACA code ~a must contain only digits" airfoil))
    (let ((first-digit (digit-char-p (char digits-string 0)))
          (second-digit (digit-char-p (char digits-string 1)))
          (thickness-digits (parse-integer (subseq digits-string 2))))
      (list
       (/ first-digit 100.0d0)
       (/ second-digit 10.0d0)
       (/ thickness-digits 100.0d0)
       #'camber-4digit #'dcamber-4digit))))

(defun get-naca-5digit-spec (airfoil)
  "Parse 5-digit NACA code like :23012 -> (m μ tau camber-fn dcamber-fn).
μ here is the cubic splice (≈0.2025 for these families), not 'position of max camber'."
  (let* ((code-str (if (symbolp airfoil) (symbol-name airfoil) (string airfoil)))
         (digits (coerce (remove-if-not #'digit-char-p code-str) 'string)))
    (unless (= (length digits) 5)
      (error "NACA 5-digit code ~a must contain exactly 5 digits." airfoil))
    (let* ((prefix (subseq digits 0 3))
           (thickness (parse-integer (subseq digits 3 5)))
           (tau (/ thickness 100.0d0))
           (params (assoc prefix *naca-5digit-camber-table* :test #'string=)))
      (unless params
        (error "Unknown 5-digit NACA airfoil prefix: ~A" prefix))
      (destructuring-bind (&key m p &allow-other-keys) (rest params)
        (let* ((camber-fn-sym (intern (format nil "camber-~a" prefix) :naca-nurbs))
               (dcamber-fn-sym (intern (format nil "dcamber-~a" prefix) :naca-nurbs)))
          (unless (and (fboundp camber-fn-sym) (fboundp dcamber-fn-sym))
            (error "Missing camber functions: ~A, ~A" camber-fn-sym dcamber-fn-sym))
          (list m p tau
                (symbol-function camber-fn-sym)
                (symbol-function dcamber-fn-sym)))))))

(defun get-airfoil-spec (airfoil)
  (let* ((code-str (if (symbolp airfoil) (symbol-name airfoil) (string airfoil)))
         (digits (coerce (remove-if-not #'digit-char-p code-str) 'string)))
    (ecase (length digits)
      (4 (get-naca-4digit-spec airfoil))
      (5 (get-naca-5digit-spec airfoil)))))

;; ----------------------------------------
;; Thickness law + derivatives (respect :closed?)
;; ----------------------------------------

(defun %a5 (closed?) (if closed? -0.1036d0 -0.1015d0))

(defun thickness (x tau &key closed?)
  "Compute NACA thickness at x (0-1), tau is max thickness fraction.
   For x<=0, use linear extrapolation to avoid complex sqrt."
  (if (<= x 0d0)
      (* (/ tau 0.2d0) -0.1260d0 x)
      (let ((a5 (%a5 closed?)))
        (* (/ tau 0.2d0)
           (+ (* 0.2969d0 (sqrt x))
              (* -0.1260d0 x)
              (* -0.3516d0 (expt x 2))
              (*  0.2843d0 (expt x 3))
              (*  a5        (expt x 4)))))))

(defun dthickness-dx (x tau &key closed?)
  "First derivative of NACA thickness; extend smoothly for x<=0."
  (if (<= x 0d0)
      (* (/ tau 0.2d0) -0.1260d0)
      (let ((a5 (%a5 closed?)))
        (* (/ tau 0.2d0)
           (+ (* 0.2969d0 (/ 1d0 (* 2d0 (sqrt x))))
              -0.1260d0
              (* -0.3516d0 2d0 x)
              (*  0.2843d0 3d0 (expt x 2))
              (*  a5       4d0 (expt x 3)))))))

(defun d2thickness-dx2 (x tau &key closed?)
  "Second derivative of NACA thickness; avoid sqrt term for x<=0."
  (if (<= x 0d0)
      (* (/ tau 0.2d0) (* -0.3516d0 2d0))
      (let ((a5 (%a5 closed?)))
        (* (/ tau 0.2d0)
           (+ (* 0.2969d0 (/ -1d0 (* 4d0 (expt x 1.5d0))))
              (* -0.3516d0 2d0)
              (*  0.2843d0 6d0 x)
              (*  a5       12d0 (expt x 2)))))))

;; ----------------------------------------
;; t<->x parameterization + x(t) derivatives
;; ----------------------------------------

(defun t->x (tee cosine?)
  (if cosine?
      (* 0.5d0 (- 1d0 (cos (* pi tee))))
      tee))

(defun x->t (x cosine?)
  (max 0.0d0 (min 1.0d0
                 (if cosine?
                     (/ (acos (max -1.0d0 (min 1.0d0 (- 1.0d0 (* 2.0d0 x))))) pi)
                     x))))

(defun dxdt (tee cosine?)
  (if cosine?
      (* 0.5d0 pi (sin (* pi tee)))
      1.0d0))

(defun d2xdt2 (tee cosine?)
  (if cosine?
      (* 0.5d0 pi pi (cos (* pi tee)))
      0.0d0))

;; ----------------------------------------
;; Generic helpers across 4-/5-digit for higher derivatives of camber/theta
;; ----------------------------------------

(defun d2camber (airfoil x m p)
  "2nd derivative of camber for either 4- or 5-digit."
  (let* ((code-str (if (symbolp airfoil) (symbol-name airfoil) (string airfoil)))
         (digits (coerce (remove-if-not #'digit-char-p code-str) 'string)))
    (ecase (length digits)
      (4 (d2camber-4digit x m p))
      (5 (let* ((prefix (subseq digits 0 3))
                (sym (intern (format nil "d2camber-~a" prefix) :naca-nurbs)))
           (funcall (symbol-function sym) x))))))

(defun d3camber (airfoil x m p)
  "3rd derivative of camber for either 4- or 5-digit."
  (let* ((code-str (if (symbolp airfoil) (symbol-name airfoil) (string airfoil)))
         (digits (coerce (remove-if-not #'digit-char-p code-str) 'string)))
    (ecase (length digits)
      (4 (d3camber-4digit x m p))
      (5 (let* ((prefix (subseq digits 0 3))
                (sym (intern (format nil "d3camber-~a" prefix) :naca-nurbs)))
           (funcall (symbol-function sym) x))))))

(defun theta-2nd (airfoil x m p)
  "Unified θ''(x) for 4- and 5-digit using u = dyc/dx.
   θ'(x) = d2yc / (1 + (dyc/dx)^2);  θ'' uses d³yc as well."
  (let* ((spec (get-airfoil-spec airfoil))
         (dc-fn (fifth spec))
         (dc  (funcall dc-fn x m p))             ; dyc/dx
         (d2c (d2camber airfoil x m p))          ; d2yc/dx2
         (d3c (d3camber  airfoil x m p))         ; d3yc/dx3
         (den (+ 1d0 (* dc dc)))
         (den2 (* den den))
         (d2c2 (* d2c d2c)))
    (/ (- (* d3c den) (* 2d0 dc d2c2)) den2)))


;; ----------------------------------------
;; Analytical tangent & curvature (parametric in t)
;; ----------------------------------------

(defun analytical-tangent-parametric (airfoil tee &key (surface :upper) (cosine? t) (closed? nil))
  "Unit tangent at parameter t using parametric derivatives (stable near leading edge)."
  (when (or (< tee 0d0) (> tee 1d0))
    (error "t must be in [0,1]"))
  (destructuring-bind (m p tau _camber-fn dcamber-fn)
      (get-airfoil-spec airfoil)
    (declare (ignore _camber-fn))
    (let* ((x   (t->x tee cosine?))
           (xp  (dxdt tee cosine?))

           ;; camber + theta'
           (dycdx (funcall dcamber-fn x m p))
           (d2yc  (d2camber airfoil x m p))
           (theta (atan dycdx))
           (ct (cos theta))
           (st (sin theta))
           (thetad (/ d2yc (+ 1d0 (* dycdx dycdx))))

           ;; thickness
           (yt   (thickness x tau :closed? closed?))
           (dyt  (dthickness-dx x tau :closed? closed?))

           ;; signs per surface
           (sgn-x (if (eq surface :upper) -1d0 +1d0))
           (sgn-y (if (eq surface :upper) +1d0 -1d0))

           ;; d/dx (yt sinθ) and dY/dx
           (B  (+ (* dyt st) (* yt ct thetad)))
           (C  (+ dycdx (* sgn-y (- (* dyt ct) (* yt st thetad)))))

           ;; parametric first derivatives
           (xpu  (+ xp (* sgn-x xp B)))   ; X'(t)
           (ypu  (* xp C))                ; Y'(t)
           (len  (sqrt (+ (* xpu xpu) (* ypu ypu)))))
      (unitize-vector (make-vector (/ xpu len) (/ ypu len) 0d0)))))

(defun analytical-curvature-parametric (airfoil tee &key (surface :upper) (cosine? t) (closed? nil))
  "κ(t) = |x'y'' - y'x''| / (x'^2 + y'^2)^(3/2), using parametric derivatives."
  (when (or (< tee 0d0) (> tee 1d0))
    (error "t must be in [0,1]"))
  (destructuring-bind (m p tau _camber-fn dcamber-fn)
      (get-airfoil-spec airfoil)
    (declare (ignore _camber-fn))
    (let* ((x   (t->x tee cosine?))
           (xp  (dxdt tee cosine?))
           (xpp (d2xdt2 tee cosine?))

           (dycdx (funcall dcamber-fn x m p))
           (d2yc  (d2camber airfoil x m p))
           (theta (atan dycdx))
           (ct (cos theta))
           (st (sin theta))
           (thetad (/ d2yc (+ 1d0 (* dycdx dycdx))))
           (theta2 (theta-2nd airfoil x m p))

           (yt   (thickness x tau :closed? closed?))
           (dyt  (dthickness-dx x tau :closed? closed?))
           (d2yt (d2thickness-dx2 x tau :closed? closed?))

           (sgn-x (if (eq surface :upper) -1d0 +1d0))
           (sgn-y (if (eq surface :upper) +1d0 -1d0))

           (B  (+ (* dyt st) (* yt ct thetad)))
           (Bp (+ (* d2yt st)
                  (* 2d0 dyt ct thetad)
                  (* yt (- st) (* thetad thetad))
                  (* yt ct theta2)))

           (C  (+ dycdx (* sgn-y (- (* dyt ct) (* yt st thetad)))))
           (Cp (+ d2yc
                  (* sgn-y (+ (* d2yt ct)
                              (* -2d0 dyt st thetad)
                              (* -1d0 yt ct (* thetad thetad))
                              (* -1d0 yt st theta2)))))

           (xpu  (+ xp (* sgn-x xp B)))
           (ypu  (* xp C))
           (xppu (+ (* xpp (+ 1d0 (* sgn-x B)))
                    (* sgn-x (* xp xp Bp))))
           (yppu (+ (* xpp C) (* xp xp Cp)))

           (num (abs (- (* xpu yppu) (* ypu xppu))))
           (den (expt (+ (* xpu xpu) (* ypu ypu)) 1.5d0)))
      (/ num den))))

;; ----------------------------------------
;; Sampling (arrays) and compatibility shim
;; ----------------------------------------

(defun generate-naca-samples (airfoil n-points &key (cosine? t) (closed? nil))
  "Return plist with array samples: :upper :lower :tees :xs :cosine?."
  (let* ((den (max 1 (1- n-points)))
         (tees (make-array n-points :element-type 'double-float))
         (xs   (make-array n-points :element-type 'double-float))
         (upper (make-array n-points))
         (lower (make-array n-points)))
    (dotimes (i n-points)
      (setf (aref tees i)
            (/ (coerce i 'double-float) (coerce den 'double-float))))
    (dotimes (i n-points)
      (setf (aref xs i)
            (if cosine?
                (* 0.5d0 (- 1d0 (cos (* pi (aref tees i)))))
                (aref tees i))))
    (destructuring-bind (m p tau camber-fn dcamber-fn)
        (get-airfoil-spec airfoil)
      (dotimes (i n-points)
        (let* ((x (aref xs i))
               (yc (funcall camber-fn x m p))
               (dycdx (funcall dcamber-fn x m p))
               (theta (atan dycdx))
               (yt (thickness x tau :closed? closed?))
               (xu (- x (* yt (sin theta))))
               (yu (+ yc (* yt (cos theta))))
               (xl (+ x (* yt (sin theta))))
               (yl (- yc (* yt (cos theta)))))
          (setf (aref upper i) (make-point xu yu 0d0)
                (aref lower i) (make-point xl yl 0d0)))))
    (list :upper upper :lower lower :tees tees :xs xs :cosine? cosine?)))


(defun generate-naca-points (airfoil n-points &key (cosine? t) (closed? nil))
  "Compatibility shim returning two lists of points."
  (destructuring-bind (&key upper lower &allow-other-keys)
      (generate-naca-samples airfoil n-points :cosine? cosine? :closed? closed?)
    (values (coerce upper 'list) (coerce lower 'list))))

;; ----------------------------------------
;; Search helpers (binary lower-bound, ternary search, gradient window)
;; ----------------------------------------

(defun lower-bound (vec val)
  "Smallest i with vec[i] >= val; returns (length vec) if none. vec is a vector."
  (let ((lo 0)
        (hi (length vec)))
    (do ()
        ((>= lo hi) lo)
      (let ((mid (floor (+ lo hi) 2)))
        (if (>= (aref vec mid) val)
            (setf hi mid)
            (setf lo (1+ mid)))))))

(defun ternary-search-maximum (&key func curve min-x max-x (tolerance 1d-6) (depth 0))
  "Find the argmax in [MIN-X, MAX-X] for unimodal FUNC via ternary search."
  (cond ((> depth 50) (/ (+ min-x max-x) 2d0))
        ((<= (abs (- max-x min-x)) tolerance) (/ (+ min-x max-x) 2d0))
        (t
         (let* ((delta (/ (- max-x min-x) 3d0))
                (m1 (+ min-x delta))
                (m2 (- max-x delta))
                (f1 (funcall func m1 curve))
                (f2 (funcall func m2 curve)))
           (if (> f1 f2)
               (ternary-search-maximum :func func :curve curve
                                       :min-x min-x :max-x m2
                                       :tolerance tolerance :depth (1+ depth))
               (ternary-search-maximum :func func :curve curve
                                       :min-x m1 :max-x max-x
                                       :tolerance tolerance :depth (1+ depth)))))))
(defun find-max-gradient-region (pairs &key (window-size 3))
  "Return (s-low s-high) spanning WINDOW-SIZE over PAIRS = list of (s val),
   maximizing |Δval| = |val_high - val_low|. Single pass, O(n) time, O(1) space."
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (type list pairs)
           (type fixnum window-size))
  (when (or (< window-size 2)
            (< (length pairs) window-size))
    (return-from find-max-gradient-region nil))
  (let* ((k (cl:the fixnum window-size))
         (tail pairs)
         (head (nthcdr (1- k) pairs))
         (best-g -1d0)
         (best-low nil)
         (best-high nil))
    (loop while head do
          (let* ((p-low  (car tail))
                 (p-high (car head))
                 (s-low  (first p-low))
                 (s-high (first p-high))
                 (v-low  (second p-low))
                 (v-high (second p-high))
                 (g (abs (- (coerce v-high 'double-float)
                            (coerce v-low  'double-float)))))
            (when (> g best-g)
              (setf best-g g
                    best-low s-low
                    best-high s-high)))
          (setf tail (cdr tail)
                head (cdr head)))
    (and best-low (list best-low best-high))))

;; Source-pane support (function-source-string) now lives in
;; demos-common/source/source-strings.lisp, shared by all demos.
