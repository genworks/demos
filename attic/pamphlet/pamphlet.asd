;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:pamphlet :description
 "The Gendl® pamphlet Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20210719" :depends-on (:cl-interpol :gasap)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/side-1") (:file "source/side-2")))
