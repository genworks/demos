;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:deck :description
 "The Gendl® deck Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "/Users/dcooper8/genworks/demos/deck/20210804" :depends-on (:pui :bench)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/assembly")
  (:gendl "source/ui")))
