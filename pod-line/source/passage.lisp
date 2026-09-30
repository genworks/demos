;;;; -*- Mode: Lisp; Package: pod-line-demo -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;;
;;;; The passage check: does anything of a pod meet anything that holds
;;;; the track up?  Parts are compared as oriented boxes, by the
;;;; separating axis test, so a bogie tilted on a sloping cable and a
;;;; girder forty metres long are both judged as they lie.

(in-package :pod-line-demo)

(defun boxes-clash? (box-1 box-2 &optional (tolerance 1))
  "True when two bounding boxes overlap by more than TOLERANCE along every axis."
  (loop for axis from 0 to 2
        always (> (- (min (aref (second box-1) axis) (aref (second box-2) axis))
                     (max (aref (first box-1) axis) (aref (first box-2) axis)))
                  tolerance)))

(defun oriented-box (leaf)
  "Centre, three axes and three half-sizes of the box that bounds LEAF along
its own axes: a box itself, a cylinder or cone squared off, anything
else by its bounding box along the global axes."
  (flet ((own-axes ()
           (mapcar #'(lambda (face) (the-object leaf (face-normal-vector face)))
                   '(:right :rear :top))))
    (case (the-object leaf type)
      (box (list (the-object leaf center) (own-axes)
                 (list (half (the-object leaf width))
                       (half (the-object leaf length))
                       (half (the-object leaf height)))))
      ((cylinder cone)
       (let ((radius (if (eql (the-object leaf type) 'cone)
                         (max (the-object leaf radius-1) (the-object leaf radius-2))
                         (the-object leaf radius))))
         (list (the-object leaf center) (own-axes)
               (list radius (half (the-object leaf length)) radius))))
      (otherwise
       (destructuring-bind (low high) (the-object leaf bounding-box)
         (list (midpoint low high)
               (list (make-vector 1 0 0) (make-vector 0 1 0) (make-vector 0 0 1))
               (list (half (- (get-x high) (get-x low)))
                     (half (- (get-y high) (get-y low)))
                     (half (- (get-z high) (get-z low))))))))))

(defun oriented-boxes-clash? (box-1 box-2 &optional (tolerance 1))
  "True when two oriented boxes overlap by more than TOLERANCE, which is when
no axis separates them: none of either box, and none square to an edge
of each."
  (destructuring-bind (centre-1 axes-1 halves-1) box-1
    (destructuring-bind (centre-2 axes-2 halves-2) box-2
      (let ((between (subtract-vectors centre-2 centre-1)))
        (flet ((separates? (axis)
                 (flet ((reach (axes halves)
                          (loop for own in axes
                                for half in halves
                                sum (* half (abs (dot-vectors own axis))))))
                   (>= (abs (dot-vectors between axis))
                       (- (+ (reach axes-1 halves-1) (reach axes-2 halves-2))
                          tolerance)))))
          (not (or (some #'separates? axes-1)
                   (some #'separates? axes-2)
                   (loop for edge-1 in axes-1
                           thereis (loop for edge-2 in axes-2
                                         for square = (cross-vectors edge-1 edge-2)
                                           thereis (and (> (length-vector square) 1.0e-6)
                                                        (separates?
                                                         (unitize-vector square))))))))))))

(defun leaf-name (leaf)
  (let ((name (first (the-object leaf root-path))))
    (if (consp name) (first name) name)))

(defun find-clashes (pods structures)
  "The pairs of leaves, one of a pod in PODS and one under STRUCTURES, whose
oriented boxes overlap; each leaf is named by its path from the root.
A wheel is not held against what it runs on: the running heads, and the
shoes on which the cables lie."
  (let ((structure-leaves (loop for object in structures
                                append (the-object object leaves))))
    (loop for pod in pods
          for reach = (the-object pod bounding-box)
          for fixed = (loop for leaf in structure-leaves
                            when (boxes-clash? reach (the-object leaf bounding-box) 0)
                              collect (cons leaf (oriented-box leaf)))
          append (loop for leaf in (the-object pod leaves)
                       for box = (oriented-box leaf)
                       append (loop for (other . other-box) in fixed
                                    when (and (oriented-boxes-clash? box other-box)
                                              (not (and (eql (leaf-name leaf) :wheel)
                                                        (member (leaf-name other)
                                                                '(:head :shoe)))))
                                      collect (list (the-object leaf root-path)
                                                    (the-object other root-path)))))))
