(in-package :pamphlet)

(defparameter *home* (let ((source-path (glisp:source-pathname)))
		       (make-pathname :defaults source-path :type nil :name nil
				      :directory (butlast (pathname-directory source-path)))))

(define-object assembly (document)
  
  :input-slots
  ((border-boxes? ;;t 
		  nil
		  )
   
   ;;(page-length (* (+ 8 17/32) 72))
   ;;(page-width (* (+ 11 3/64) 72))
   
   (page-length (* 8.5 72))
   (page-width (* 11.0 72))
   
   (x-percentage 1) (y-percentage 1)
   
   (text-x-scale 0.8)
   
   )

  
  :computed-slots
  ((pages (list  (the side-2) (the side-1) )))
  
  :trickle-down-slots (text-x-scale)
  
  :objects
  ((side-1 :type 'side-1 
	   :pass-down (border-boxes? page-length page-width x-percentage y-percentage text-x-scale)
	   )
   (side-2 :type 'side-2 
	   :pass-down (border-boxes? page-length page-width x-percentage y-percentage text-x-scale)
	   )))

(defun save (&key (pathname (let ((directory (ensure-directories-exist "~/pdfs/")))
				       (merge-pathnames "pamphlet.pdf" directory))))
  (with-format (pdf-multipage pathname)
    (write-the-object (make-object 'assembly) cad-output))

  (format t "~&~%Done. Saved pamphlet to ~a.~%" pathname))


(format t "~&~%Note: (pamphet:save) function has now been defined. 

So you can do (pamphlet:save) and it will create the PDF file
and will report the location.~%
" )
			   

