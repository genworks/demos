;;;; -*- coding: utf-8 -*-
;;
;; Copyright 2020 Genworks International 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (Gendl).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 


(asdf:defsystem #:bench :description
 "The Gendl® bench Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20190920" :depends-on nil
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:gdl "source/lumber") (:gdl "source/package")
  (:gdl "source/patches") (:gdl "source/product")
  (:gdl "source/planked-back") (:gdl "source/planked-base")
  (:gdl "source/planking") (:gdl "source/back-frame")
  (:gdl "source/base-frame") (:gdl "source/leg-assy")
  (:gdl "source/node") (:gdl "source/process")
  (:gdl "source/processes")))
