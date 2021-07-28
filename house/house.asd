;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:house :description
 "The Gendl® house Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20210728" :depends-on (:pui :deck)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:gendl "source/assembly")
  (:gendl "source/brick-wall") (:file "source/initialize")
  (:gendl "source/ui")))
