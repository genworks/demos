;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:bench :description
 "The Gendl® bench Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20190919" :depends-on nil
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:gdl "source/lumber") (:gdl "source/package")
  (:gdl "source/patches") (:gdl "source/back-frame")
  (:gdl "source/base-frame") (:gdl "source/leg-assy")
  (:gdl "source/node") (:gdl "source/planked-back")
  (:gdl "source/planked-base") (:gdl "source/planking")
  (:gdl "source/process") (:gdl "source/product")))
