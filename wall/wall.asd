;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:wall :description
 "The Gendl® wall Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20210715" :depends-on nil :defsystem-depends-on
 nil :components
 ((:file "source/package") (:gendl "source/brick-wall")
  (:gendl "source/house") (:file "source/initialize")
  (:gendl "source/ui")))
