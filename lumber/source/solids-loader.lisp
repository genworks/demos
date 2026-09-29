;;;; -*- Mode: Lisp; Package: lumber -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;;
;;;; Load the solid lumber variants only on images whose surf
;;;; package actually exports extruded-solid (Enterprise/SMLib
;;;; builds); on free images solids.lisp is not even readable, and
;;;; lumber-type quietly falls back to the wireframe types.

(in-package :lumber)

(when (and (find-package :surf)
           (let ((symbol (find-symbol-ci "extruded-solid" :surf)))
             (and symbol (find-class symbol nil))))
  ;; Found through the system's own source directory: under ASDF on
  ;; Allegro, *load-truename* is this file's fasl in the output cache,
  ;; where no solids.lisp lives.
  (load (asdf:system-relative-pathname :lumber "source/solids.lisp")))
