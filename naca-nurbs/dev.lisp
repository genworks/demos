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

