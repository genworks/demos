;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:robot-demo :description
 "The Gendl® robot demo -- the classic GDL example in the 2026 demo framework"
 :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20260809" :depends-on nil
 :defsystem-depends-on nil :components
 ((:file "source/package") (:gdl "source/assembly")
  (:gdl "source/ui") (:file "source/publish")))
