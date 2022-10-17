;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:pui :description
 "The Gendl® pui Subsystem" :author "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20221017" :depends-on nil :components
 ((:file "source/package") (:gendl "source/application-mixin")
  (:file "source/initialize") (:gendl "source/tree")
  (:gendl "source/viewport")))
