;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:bench :description
 "The Gendl® bench Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20210718" :depends-on nil :defsystem-depends-on
 nil :components
 ((:gdl "source/lumber") (:gdl "source/package")
  (:gdl "source/planking") (:gdl "source/patches")
  (:gdl "source/product") (:gdl "source/planked-back")
  (:gdl "source/planked-base") (:gdl "source/back-frame")
  (:gdl "source/base-frame") (:gdl "source/leg-assy")
  (:gdl "source/node") (:gendl "source/plank-samples")
  (:gdl "source/process") (:gdl "source/processes")))
