;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :pui)

(defparameter *source-home* (make-pathname :defaults (glisp:source-pathname) :name nil :type nil))
(defparameter *system-home* (merge-pathnames "../" *source-home*))

(defun initialize! ()
	   
  (let ((exe-dir (glisp:executable-homedir-pathname))
	(system-home (namestring *system-home*)))

    (flet ((dir-search (subdir)
	     (let ((path (or (probe-file (merge-pathnames (format nil "~a/" subdir)  system-home))
			     (probe-file (merge-pathnames (format nil "pui-~a/" subdir) exe-dir)))))
	       (unless path (warn (format nil "~a not found in ~a or ~a~%" subdir system-home exe-dir)))
	       (when path (namestring path)))))
      (let ((pui-images (dir-search "images"))
	    (pui-static  (dir-search "static")))
	(with-all-servers (server)
	  (mapc #'(lambda(prefix file)
		    (publish-directory
                     :headers gwl::*publish-directory-headers*
                     ;;:headers (list (cons :cache-control "public, max-age=3600, must-revalidate"))
                     :prefix prefix :server server :destination file))
		`("/pui-static" "/pui-images")
		`(,pui-static ,pui-images)))))))

(initialize!)

