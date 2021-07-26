(in-package :house-ui)

(defparameter *source-home* (make-pathname :defaults (glisp:source-pathname) :name nil :type nil))
(defparameter *system-home* (merge-pathnames "../" *source-home*))

(defun initialize! ()
	   
  (let ((exe-dir (glisp:executable-homedir-pathname))
	(system-home (namestring *system-home*)))

    (flet ((dir-search (subdir)
	     (let ((path (or (probe-file (merge-pathnames (format nil "~a/" subdir)  system-home))
			     (probe-file (merge-pathnames (format nil "house-~a/" subdir) exe-dir)))))
	       (unless path (warn (format nil "~a not found in ~a or ~a~%" subdir system-home exe-dir)))
	       (when path (namestring path)))))
      (let ((house-images (dir-search "images"))
	    (house-static  (dir-search "static")))
    
	(with-all-servers (server)
	  (mapc #'(lambda(prefix file)
		
		    (publish-directory :prefix prefix :server server :destination file))
		`("/house-static" "/house-images")
		`(,house-static ,house-images))

          
	  (publish-gwl-app "/house" 'assembly))))))


(initialize!)
