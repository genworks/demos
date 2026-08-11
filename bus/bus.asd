;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:bus :description
 "The Gendl® Wireframe School Bus Demo" :author
 "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20260811" :depends-on (:demos-common :gwl-graphics)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/body") (:file "source/chassis")
  (:file "source/interior") (:file "source/rule-ackermann")
  (:file "source/axle")
  (:file "source/frame-rail") (:file "source/frame")
  (:file "source/inter-seat-clearance-check")
  (:file "source/inter-seat-spacing") (:file "source/knuckle")
  (:file "source/rear-axle") (:file "source/seat")
  (:file "source/seating-section") (:file "source/seating-side")
  (:file "source/wheel") (:gdl "source/ui")
  (:file "source/publish")))
