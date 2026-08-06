(in-package :gdl-user)

(load-quicklisp)

(defvar *staircase-home* (make-pathname :name nil :type nil :defaults (glisp:source-pathname)))

(pushnew (namestring *staircase-home*) ql:*local-project-directories* :test #'equalp)

(ql:quickload :staircase)

(staircase-demo:publish-staircase!)

