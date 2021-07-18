;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:deck :description
 "The Gendl® deck Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20210718" :depends-on (:bench :wall)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/assembly")))
