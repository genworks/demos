;;;; -*- coding: utf-8 -*-
(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *asdf-found?* t)
  (unless (find-package :asdf)
    (setq *asdf-found?* nil)
    (defpackage :asdf (:nicknames :asdf/interface asdf/lisp-action))))

(when (find-package :asdf)
  (defclass asdf::gdl (asdf::cl-source-file) ((type :initform "gdl")))
  (defclass asdf::gendl (asdf::cl-source-file) ((type :initform "gendl")))
  (defclass asdf::lisp (asdf::cl-source-file) ()))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless *asdf-found?* (delete-package :asdf)))

