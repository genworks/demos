(in-package :naca-nurbs)


(defparameter *step-dir*
  (let* ((base (glisp:source-pathname))
	 (base-dir (pathname-directory base)))
    (make-pathname :name nil :type nil
		   :directory (append (butlast base-dir) (list "step"))
		   :defaults base)))

(define-object import-step ()

  :input-slots ((step-dir *step-dir*))
  
  :computed-slots
  ((files  (remove-if-not
	    #'(lambda(path)
		(or (string-equal (pathname-type path) "step")
		    (string-equal (pathname-type path) "stp")))
	    (glisp:directory-list (the step-dir))))

   ;;
   ;; Assume common SolidWorks assy structure
   ;;
   (brep-list (mapcar #'(lambda (import)
		      (theo  import
			     (sub-assemblies 0) (sub-assemblies 0) (sub-assemblies 1)
			     (hw-objects 0) item))
		  (list-elements (the imports)))))
  
  :hidden-objects
  ((imports :type 'smlib::assembly-import
	    :sequence (:size (length (the files)))
	    :file-name (nth (the-child index) (the files))))
  
  :objects ((breps :type 'brep
		   :sequence (:size (the imports number-of-elements))
		   :built-from (nth (the-child index) (the brep-list)))


	    (brep-1-edges :type 'curve
			  :sequence (:size (theo (second (the brep-list))
						 edges number-of-elements))
			  :built-from (theo (second (the brep-list))
					    (edges (the-child index)))
			  :display-controls (list :color :orange
						  :line-thickness 2))))
  
  
