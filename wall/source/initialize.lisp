(in-package :wall-ui)

(defun initialize! ()
	   
  (let ((exe-dir (glisp:executable-homedir-pathname))

	(app-directory (let ((path (probe-file "~/genworks/demos/wall/")))
			    (when path (namestring path)))))

    (flet ((dir-search (subdir)
	     (let ((path (or (probe-file (merge-pathnames (format nil "~a/" subdir)  app-directory))
			     (probe-file (merge-pathnames (format nil "wall-~a/" subdir) exe-dir)))))
	       (unless path (warn (format nil "~a not found in ~a or ~a~%" subdir app-directory exe-dir)))
	       (when path (namestring path)))))
      (let ((wall-images (dir-search "images"))
	    (wall-static  (dir-search "static")))
    
	(with-all-servers (server)
	  (mapc #'(lambda(prefix file)
		
		    (publish-directory :prefix prefix :server server :destination file))
		`("/wall-static" "/wall-images")
		`(,wall-static ,wall-images))

          
	  (publish-gwl-app "/wall" 'assembly))))))


(initialize!)
