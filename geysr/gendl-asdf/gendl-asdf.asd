;;;; -*- coding: utf-8 -*-
(in-package :cl-user)

(asdf:defsystem #:gendl-asdf
  :description "asdf gendl artifacts loading"
  :serial t 
  :version "20200720" 
  :depends-on nil
  :components ((:file "gendl-asdf")))
