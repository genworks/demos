;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:demos-common :description
 "Shared UI chrome and source-pane support for the public Gendl® demos"
 :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20260811" :depends-on nil
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/source-strings")
  (:gdl "source/ui-mixin")))
