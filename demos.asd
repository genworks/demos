;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:demos :description
 "The Gendl® demos Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20190904" :depends-on nil
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:gdl "bench/source/lumber") (:gdl "bench/source/package")
  (:gdl "bench/source/assembly") (:gdl "bench/source/back-frame")
  (:gdl "bench/source/base-frame") (:gdl "bench/source/leg-assy")))
