;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; A wireframe drawing to a PNG, in process.  The drawing system's svg
;; lens writes the projected geometry as <path> polylines inside
;; translated <g> groups; this reads those and draws them, anti-aliased,
;; into a grayscale canvas that zpng writes out.  No Ghostscript, no
;; subprocess: on the public workshop every run-program stalls for
;; minutes (the org's :must: item, 2026-09-27) and the render tool went
;; with it -- four renders cost the first public build ten minutes and
;; returned nothing.  View labels (<text>) are not drawn; the agent
;; knows which view it asked for.
;;

(defparameter *raster-max-pixels* 1500
  "Integer. Longest side of the rendered image, in pixels (the Messages
API resizes anything over 1568).")

;;
;; zpng (salza2 under it) writes the PNG.  It joined the Quicklisp
;; tree the Gendl images are built from (genworks/quicklisp, dist
;; 2023-06-18) on 2026-09-27; images built before that carry salza2
;; but not zpng, and the public workshop's rooms cannot fetch it (no
;; network, by design).  So it is loaded on demand and named through
;; symbol-call: a lab on an older image still loads, and merely
;; withholds the render tool, rather than not loading at all.
;;

(defvar *raster-loaded?* nil)

(defun raster-available? ()
  "True when the PNG writer is loaded (loading it on the first call)."
  (or *raster-loaded?*
      (setf *raster-loaded?*
            (and (or (find-package :zpng)
                     (ignore-errors
                      (when (find-package :ql)
                        (uiop:symbol-call :ql :register-local-projects))
                      (uiop:symbol-call :asdf :load-system :zpng)
                      (find-package :zpng)))
                 (find-package :flexi-streams)
                 (find-package :cl-base64)
                 t))))

;;
;; Reading the svg.
;;

(defun svg-attribute (tag name)
  "The value of attribute NAME in the element text TAG, or nil."
  (let* ((key (format nil " ~a=\"" name))
         (start (search key tag)))
    (when start
      (let* ((from (+ start (length key)))
             (end (position #\" tag :start from)))
        (and end (subseq tag from end))))))

(defun svg-numbers (text)
  "Every number in TEXT, in order (separators: spaces, commas, letters)."
  (let ((numbers nil) (start nil))
    (flet ((flush (end)
             (when start
               (let ((n (ignore-errors
                         (let ((*read-default-float-format* 'double-float))
                           (read-from-string (subseq text start end))))))
                 (when (realp n) (push (coerce n 'double-float) numbers)))
               (setf start nil))))
      (loop for i from 0 below (length text)
            for c = (char text i)
            do (if (or (digit-char-p c) (member c '(#\. #\- #\+ #\e #\E)))
                   ;; an e is part of a number only when one is under way
                   (if (and (member c '(#\e #\E)) (null start))
                       (flush i)
                       (unless start (setf start i)))
                   (flush i))
            finally (flush (length text))))
    (nreverse numbers)))

(defun svg-path-polylines (d)
  "The polylines of a path's D attribute: a list of point lists (x . y).
Absolute M and L with implicit line-to, and Z; relative m/l as well."
  (let ((polylines nil) (current nil) (relative nil) (pending nil) (cx 0d0) (cy 0d0))
    (flet ((close-current ()
             (when (cdr current) (push (nreverse current) polylines))
             (setf current nil))
           (add-point (x y)
             (when relative (incf x cx) (incf y cy))
             (setf cx x cy y)
             (push (cons x y) current)))
      (loop for i from 0 below (length d)
            for c = (char d i)
            do (case c
                 ((#\M #\m #\L #\l)
                  (when pending (setf pending nil))
                  (when (member c '(#\M #\m)) (close-current))
                  (setf relative (lower-case-p c)))
                 ((#\Z #\z)
                  (when current (push (first (last current)) current))
                  (close-current))
                 ((#\Space #\Tab #\Newline #\Return #\,) nil)
                 (t
                  ;; a number: read the pair starting here
                  (let* ((end (or (position-if #'(lambda (ch) (member ch '(#\M #\m #\L #\l #\Z #\z))) d :start i)
                                  (length d)))
                         (numbers (svg-numbers (subseq d i end))))
                    (loop for (x y) on numbers by #'cddr
                          when y do (add-point x y))
                    (setf i (1- end))))))
      (close-current))
    (nreverse polylines)))

(defun svg-segments (svg)
  "Every line segment of the drawing SVG (a string), translated by the
<g> groups it sits in: a list of (x0 y0 x1 y1 dashed?), plus the
drawing's width and height as second and third values."
  (let ((segments nil) (offsets (list (cons 0d0 0d0)))
        (width 612d0) (height 792d0))
    (loop with start = 0
          for open = (position #\< svg :start start)
          while open
          do (let* ((close (or (position #\> svg :start open) (length svg)))
                    (tag (subseq svg open (1+ close))))
               (cond
                 ((and (> (length tag) 4) (string= "<svg" tag :end2 4))
                  (let ((box (svg-numbers (or (svg-attribute tag "viewBox") ""))))
                    (when (= (length box) 4)
                      (setf width (third box) height (fourth box)))))
                 ((and (> (length tag) 2) (string= "<g" tag :end2 2) (not (string= "<g>" tag)))
                  (let* ((transform (or (svg-attribute tag "transform") ""))
                         (numbers (and (search "translate" transform) (svg-numbers transform)))
                         (parent (first offsets)))
                    (push (cons (+ (car parent) (or (first numbers) 0d0))
                                (+ (cdr parent) (or (second numbers) 0d0)))
                          offsets)))
                 ((string= "<g>" tag) (push (first offsets) offsets))
                 ((and (> (length tag) 3) (string= "</g" tag :end2 3))
                  (when (cdr offsets) (pop offsets)))
                 ((and (> (length tag) 5) (string= "<path" tag :end2 5))
                  (let ((d (svg-attribute tag "d"))
                        (dashed (and (svg-attribute tag "stroke-dasharray") t))
                        (dx (car (first offsets))) (dy (cdr (first offsets))))
                    (when d
                      (dolist (polyline (svg-path-polylines d))
                        (loop for (a b) on polyline
                              when b do (push (list (+ dx (car a)) (+ dy (cdr a))
                                                    (+ dx (car b)) (+ dy (cdr b)) dashed)
                                              segments)))))))
               (setf start (1+ close))))
    (values (nreverse segments) width height)))

;;
;; Drawing.
;;

(defun make-canvas (width height)
  "A grayscale canvas, white."
  (let ((canvas (make-array (list height width) :element-type '(unsigned-byte 8) :initial-element 255)))
    canvas))

(defun plot (canvas x y coverage)
  "Darken pixel (X, Y) by COVERAGE in [0, 1]; the darkest ink wins."
  (let ((height (array-dimension canvas 0)) (width (array-dimension canvas 1)))
    (when (and (<= 0 x) (< x width) (<= 0 y) (< y height))
      (let ((ink (round (* 255 (- 1 (min 1d0 (max 0d0 coverage)))))))
        (when (< ink (aref canvas y x))
          (setf (aref canvas y x) ink))))))

(defun draw-line (canvas x0 y0 x1 y1 &key (weight 1d0))
  "An anti-aliased line (Xiaolin Wu), WEIGHT the ink strength in [0, 1]."
  (let ((steep (> (abs (- y1 y0)) (abs (- x1 x0)))))
    (when steep (rotatef x0 y0) (rotatef x1 y1))
    (when (> x0 x1) (rotatef x0 x1) (rotatef y0 y1))
    (let* ((dx (- x1 x0)) (dy (- y1 y0))
           (gradient (if (zerop dx) 1d0 (/ dy dx))))
      (flet ((put (x y c)
               (if steep (plot canvas y x (* weight c)) (plot canvas x y (* weight c)))))
        ;; endpoints
        (let* ((xend (round x0)) (yend (+ y0 (* gradient (- xend x0))))
               (xgap (- 1 (- (+ x0 0.5d0) (floor (+ x0 0.5d0)))))
               (xpxl1 xend) (ypxl1 (floor yend)))
          (put xpxl1 ypxl1 (* (- 1 (- yend ypxl1)) xgap))
          (put xpxl1 (1+ ypxl1) (* (- yend ypxl1) xgap))
          (let* ((intery (+ yend gradient))
                 (xend2 (round x1)) (yend2 (+ y1 (* gradient (- xend2 x1))))
                 (xgap2 (- (+ x1 0.5d0) (floor (+ x1 0.5d0))))
                 (xpxl2 xend2) (ypxl2 (floor yend2)))
            (put xpxl2 ypxl2 (* (- 1 (- yend2 ypxl2)) xgap2))
            (put xpxl2 (1+ ypxl2) (* (- yend2 ypxl2) xgap2))
            (loop for x from (1+ xpxl1) below xpxl2
                  do (let ((iy (floor intery)))
                       (put x iy (- 1 (- intery iy)))
                       (put x (1+ iy) (- intery iy)))
                     (incf intery gradient))))))))

(defun draw-dashed-line (canvas x0 y0 x1 y1 &key (dash 6d0) (weight 0.55d0))
  (let* ((length (sqrt (+ (expt (- x1 x0) 2) (expt (- y1 y0) 2)))))
    (if (< length (* 2 dash))
        (draw-line canvas x0 y0 x1 y1 :weight weight)
        (loop for s from 0d0 below length by (* 2 dash)
              do (let* ((e (min length (+ s dash)))
                        (ux (/ (- x1 x0) length)) (uy (/ (- y1 y0) length)))
                   (draw-line canvas (+ x0 (* ux s)) (+ y0 (* uy s)) (+ x0 (* ux e)) (+ y0 (* uy e))
                              :weight weight))))))

(defun render-svg-to-png (svg &key (max-pixels *raster-max-pixels*))
  "SVG (the drawing system's output, a string) as PNG octets."
  (multiple-value-bind (segments width height) (svg-segments svg)
    (let* ((scale (/ max-pixels (max width height 1d0)))
           (pw (max 1 (round (* width scale))))
           (ph (max 1 (round (* height scale))))
           (canvas (make-canvas pw ph)))
      (dolist (segment segments)
        (destructuring-bind (x0 y0 x1 y1 dashed) segment
          (if dashed
              (draw-dashed-line canvas (* x0 scale) (* y0 scale) (* x1 scale) (* y1 scale))
              (progn
                (draw-line canvas (* x0 scale) (* y0 scale) (* x1 scale) (* y1 scale))
                ;; a second pass one pixel over thickens the stroke to ~1.5 px
                (draw-line canvas (+ (* x0 scale) 0.5d0) (+ (* y0 scale) 0.5d0)
                           (+ (* x1 scale) 0.5d0) (+ (* y1 scale) 0.5d0) :weight 0.6d0)))))
      (unless (raster-available?)
        (error "No PNG writer is loaded (zpng)."))
      (let* ((png (make-instance (find-symbol "PNG" :zpng) :color-type :grayscale :width pw :height ph))
             (data (uiop:symbol-call :zpng :data-array png)))
        (dotimes (y ph)
          (dotimes (x pw)
            (setf (aref data y x 0) (aref canvas y x))))
        (let ((out (uiop:symbol-call :flexi-streams :make-in-memory-output-stream)))
          (uiop:symbol-call :zpng :write-png-stream png out)
          (uiop:symbol-call :flexi-streams :get-output-stream-sequence out))))))

(defun png-base64 (octets)
  (uiop:symbol-call :cl-base64 :usb8-array-to-base64-string octets))
