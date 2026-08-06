;;;; -*- Mode: Lisp; Package: lumber -*-
;;;;
;;;; Load the solid lumber variants only on images whose surf
;;;; package actually exports extruded-solid (Enterprise/SMLib
;;;; builds); on free images solids.lisp is not even readable, and
;;;; lumber-type quietly falls back to the wireframe types.

(in-package :lumber)

(when (and (find-package :surf)
           (let ((symbol (find-symbol-ci "extruded-solid" :surf)))
             (and symbol (find-class symbol nil))))
  (load (merge-pathnames "solids.lisp"
                         (or *load-truename* *load-pathname*))))
