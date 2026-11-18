;;;; -*- coding: utf-8 -*-

;;; Written by hand, and the only system definition here.  cl-lite would
;;; name a system after this directory, robot, and Gendl has a system of
;;; that name already (its own robot, which every Gendl image carries as
;;; loaded): do not regenerate a robot.asd beside this file.

(asdf:defsystem #:robot-demo :description
 "The Gendl™ robot demo -- the classic GDL example in the 2026 demo framework"
 :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20260811" :depends-on (:demos-common)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:gdl "source/assembly")
  (:gdl "source/ui") (:file "source/publish")))
