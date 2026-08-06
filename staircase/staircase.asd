;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:staircase :description
 "The Gendl® staircase demo" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20260805" :depends-on (:lumber)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:gdl "source/assembly")
  (:gdl "source/ui") (:file "source/publish")))
