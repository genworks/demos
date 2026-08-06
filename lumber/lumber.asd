;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:lumber :description
 "The Gendl® lumber Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20260805" :depends-on nil :defsystem-depends-on
 nil :components
 ((:file "source/package") (:gdl "source/lumber")
  (:file "source/bom") (:file "source/solids-loader")))
