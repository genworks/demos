;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :gdl-user)

(defvar *naca-nurbs-dir*
  (make-pathname :defaults (glisp:source-pathname)
		 :name nil :type nil))

(defvar *naca-nurbs-source-dir*
  (make-pathname :defaults (merge-pathnames "source/" *naca-nurbs-dir*)
		 :name nil :type nil))

(let* ((base-name (lastcar (pathname-directory *naca-nurbs-dir*)))
       (asd-path (make-pathname :defaults *naca-nurbs-dir*
				:name base-name :type "asd")))
  (unless (probe-file asd-path)
    (cl-lite *naca-nurbs-dir* :create-asd-file? t)))

(load-quicklisp)
(pushnew *naca-nurbs-dir* ql:*local-project-directories* :test #'equalp)
(ql:quickload :naca-nurbs)

