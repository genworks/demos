;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:deck :description
 "The Gendl® deck Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20221017" :depends-on (:pui :bench) :components
 ((:file "source/package") (:file "source/assembly")
  (:gendl "source/ui")))
