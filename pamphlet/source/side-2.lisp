(in-package :pamphlet)

(cl-interpol:enable-interpol-syntax)

(define-object side-2 (base-drawing)
  
  :input-slots
  ((background-color :green-forest)
   
   (border-boxes? ;;t
		  nil
		  )
   
   (page-length (* 8.5 72))
   (page-width (* 11.0 72))
   
   x-percentage y-percentage text-x-scale
   
   )
  
  :computed-slots
  (
   (display-controls (list :line-thickness 0.5))
   
   (panel-width (/ (* (the page-width) (the x-percentage)) 3))
   (panel-length (* (the page-length) (the y-percentage)))
   
   )

  :hidden-objects
  ((panel-1-note :type 'panel-1-note-s1
		 :width (the panel-1 width) :length (the panel-1 length))
   
   (bottom-color-bar :type 'global-polyline
		     :display-controls (list :color (the background-color) :fill-color (the background-color))
		     :vertex-list (list (the (vertex :top :left :rear))
					(the (vertex :top :right :rear))
					(the (vertex :top :right :front))
					(the (vertex :top :left :front))
					(the (vertex :top :left :rear))
					)
		     ;;:closed? t
		     :length (the bottom-panel page-length)
		     :width (the bottom-panel page-width))
   
   (panel-2-note :type 'panel-2-note-s1
		 :width (the panel-2 width) :length (the panel-2 length))
   
   
   (devo-image :type 'pdf-image
	       ;;:image-file "~/genworks/gdl/apps/pamphlet/images/cabin.jpg"
	       :image-file (merge-pathnames "images/cabin.jpg" *home*)
	       :width (* (the panel-2 page-width) 0.95)
	       :length (* (the-child width) 0.70)
	       :keep-aspect-ratio? nil
	       :center (make-point (- (* (the panel-2 page-width) 0.003))
				   (* (the panel-2 page-length) 0.210)))

   
   (panel-3-color-bar :type 'global-polyline
		      :display-controls (list :color (the background-color) :fill-color (the background-color))
		      :vertex-list (mapcar #'(lambda(point)
					       (subtract-vectors  (the center) point))
					   (list (the panel-2 (vertex :top :left :rear))
						 (the panel-2 (vertex :top :right :rear))
						 (the panel-2 (vertex :top :right :front))
						 (the panel-2 (vertex :top :left :front))
						 (the panel-2 (vertex :top :left :rear))
						 ))
		      ;;:closed? t
		      :length (the panel-3 page-length)
		      :width (the panel-3 page-width))
   
   (panel-3-note :type 'panel-3-note-s1
		 :width (the panel-3 width) :length (the panel-3 length))
   
   ;;(landmark :type 'landmark-with-circles)
   (landmark :type 'box)
   
   (bottom-note :type 'bottom-note-s1
		:width (the bottom-panel width) :length (the bottom-panel length))
   
   (bottom-middle-note :type 'bottom-middle-note
		       :width (the bottom-middle-panel width) 
		       :length (the bottom-middle-panel length)))


  :objects
  ((panel-1 :type 'base-view
	    :width (the panel-width)
	    :length (the panel-length)
	    :center (translate (the center) :left (the-child page-width))
	    :border-box? (the border-boxes?)
	    :projection-vector (getf *standard-views* :top)
	    ;;:fit-to-page? nil
	    :view-scale 1
	    :objects (list (the panel-1-note))
	    :object-roots (list ))

   (bottom-panel :type 'base-view
		 :width (* (the panel-1 page-width) 3)
		 :length (* (the panel-1 page-length) 0.10)
		 :center (translate (the center) 
				    :front (- (half (the panel-1 page-length)) 
					      (half (the-child page-length))))
		 :left-margin 0
		 ;;:bottom-margin 0
		 :view-scale 1
		 ;;:fit-to-page? nil
		 :objects (list (the bottom-color-bar) (the bottom-note))
		 :border-box? (the border-boxes?)
		 )
   
   (bottom-middle-panel :type 'base-view
			:width (the panel-2 width)
			:length (the bottom-panel length)
			:center (translate (the center) :front (- (half (the panel-1 page-length)) 
								  (half (the-child page-length))))
			:left-margin 0
			;;:bottom-margin 0
			:view-scale 1
			;;:fit-to-page? nil
			:objects (list (the bottom-middle-note))
			:border-box? (the border-boxes?)
			)

   (panel-2 :type 'base-view
	    :width (the panel-width)
	    :length (the panel-length)
	    :border-box? (the border-boxes?)
	    :projection-vector (getf *standard-views* :top)
	    ;;:fit-to-page? nil
	    :objects (list (the panel-2-note) (the devo-image)))

   
   (panel-3-background :type 'base-view
		       :width (the panel-3 page-width)
		       :length (the panel-3 page-length)
		       :border-box? (the border-boxes?)
		       :projection-vector (getf *standard-views* :top)
		       ;;:fit-to-page? nil
		       :left-margin 0 :front-margin 0
		       :center (the panel-3 center)
		       :objects (list (the panel-3-note) (the panel-3-color-bar)
				      ))
   
   (panel-3-tower :type 'base-view
		  :width (the panel-3 width)
		  ;;:page-length (+ (the panel-3 length) (twice 145))
		  :length (the panel-3 length)
		  :border-box? (the border-boxes?)
		  :projection-vector (getf *standard-views* :top)
		  :center (the panel-3 center)
		  :view-center (subtract-vectors (make-point 20 128 0) (the center))
		  ;;:fit-to-page? nil
		  :view-scale 1.15
		  :object-roots (list (the landmark))

		  #+nil
		  (let ((list (append (the landmark profile  ui-display-list-objects)
				      (the landmark stance  ui-display-list-objects)
				      (the landmark eye  ui-display-list-objects)
				      (the landmark lacing  ui-display-list-objects))))
		    (let* 
			((dims (remove-if-not #'(lambda(object) 
						  (or (typep object 'landmark::profile-annotations)
						      (typep object 'landmark::stance-annotations)
						      (typep object 'landmark::eye-annotation)))
					      list))
								    
			 (nondims (set-difference list dims)))
				    
		      (append nondims  (list-elements (the landmark circles)) dims ))))
   
   (panel-3 :type 'base-view
	    :width (the panel-width)
	    :length (the panel-length)
	    :border-box? (the border-boxes?)
	    :projection-vector (getf *standard-views* :top)
	    ;;:fit-to-page? nil
	    :center (translate (the center) :right (the-child page-width))
	    :objects nil #+nil (list (the panel-3-note) )
	    )))


;;(defmacro with-p (&rest args) `(typeset::paragraph ,@args))
;;(defmacro with-style (&rest args) `(typeset::with-style ,@args))
;;(defmacro vspace (&rest args) `(typeset::vspace ,@args))


(define-object panel-1-note-s1 (geom-base::typeset-block)
  :functions
  ((content 
    ()
    (let ((typeset::*leading-ratio* 1.27))
      (typeset::compile-text 
       ()
       (with-style (:h-align :justified :font-size 12 :left-margin 12 :right-margin 12 :text-x-scale (the text-x-scale))
	 (vspace 25) 
	 (with-p (:h-align :center :color (gethash :aquamarine-medium *color-table-decimal*) :font-size 27)
	   "Based on Free, Open-source Kernel")
	 (vspace 9)
	 (with-p (:font "Helvetica-Bold" :font-size 12 :color (gethash :orange *color-table-decimal*))
	   "Genworks GDL is based on The Gendl Project, a free, open-source software project")
	 (vspace 9)
	 (with-p ()
	   (with-style (:font "Helvetica-Bold" :font-size 12 :color (gethash :orange *color-table-decimal*))
	     "Zero-risk Investment")
	   (with-style (:font "Helvetica-Oblique") "gitlab.common-lisp.net/gendl/gendl.git") :eol
	   "By recording your executable corporate knowledge in Genworks GDL, you are guaranteeing its accessibility 
into the future, forever.")
	 (vspace 9)
	 (with-p ()
	   (with-style (:font "Helvetica-Bold" :font-size 12 :color (gethash :orange *color-table-decimal*))
	     "ANSI Standard")
	   (with-style (:font "Helvetica-Oblique") "common-lisp.net") :eol
	   "Genworks GDL is implemented in portable ANSI Common Lisp, an official language standard as certified by 
the American National Standards Institute (ANSI).")
	 (vspace 9)
	 (with-p ()
	   (with-style (:font "Helvetica-Bold" :font-size 12 :color (gethash :orange *color-table-decimal*))
	     "Happy Active Customers") :eol
	   (with-style (:font "Helvetica-Oblique") "  www.genworks.com") :eol
	   "For up-to-date contacts with current customers who would be open to discuss 
their ongoing Genworks experience on an individual basis, please visit the above web page.")
	 (vspace 9)
	 (with-p()
	   (with-style (:font "Helvetica-Bold" :font-size 12 :color (gethash :orange *color-table-decimal*))
	     "Active Developer Community")
	   (with-style (:font "Helvetica-Oblique" :font-size 12) "  www.genworks.com/contact") :eol
	   "This venue hosts discussions and announcements relevant to Genworks products as well as 
the Common Lisp developer community. Please do not hesitate to sign in and convey your interests.")))))))


(define-object panel-3-note-s1 (geom-base::typeset-block)
  :functions
  ((content
    ()
    (typeset::compile-text 
     ()
     (with-p 
      (:h-align :center :left-margin 12 :right-margin 12 :color (gethash :white *color-table-decimal*))
      (vspace 20)
      (with-style (:text-x-scale 0.9 :font "Helvetica-Bold" :font-size 25)
	"New Tagline Here"
	;;"Hey Now"
	))))

   
   #+nil
   (content
    ()
    (typeset::compile-text 
     ()
     (with-p 
      (:h-align :center :left-margin 12 :right-margin 12 :color (gethash :white *color-table-decimal*))
      (vspace 20)
      (with-style (:text-x-scale 0.9 :font "Helvetica-Bold" :font-size 25)
	"Reach new heights")
      (vspace 370)
      (hspace -12)
      (with-style (:text-x-scale 1.0 :font "Times-Italic" :font-size 5)
	"Landmark Type I Radio Tower" :eol
	(hspace -12)
	"www.landmarktower.com")
      
      (vspace 90)

      (with-style (:text-x-scale (the text-x-scale) :font-size 17)
	"...with advanced programming language technologies from Genworks."))))))

(define-lens (pdf panel-3-note-s1)()
  :output-functions
  ((additional-non-text
    ()
    #+nil
    (gdl::with-format-slots (view-scale)
      (let ((view-scale (or view-scale 1)))
	(pdftry::full-logo :start-x -176 :start-y -469 :scale-x (* view-scale 0.59) :scale-y (* view-scale 0.59) :color :black))))))


(define-object bottom-note-s1 (geom-base::typeset-block)
  :functions
  ((content 
    ()
    (let ((typeset::*leading-ratio* 1.27))
      (typeset::compile-text 
       ()
       (vspace 7)
       (with-p 
	(:left-margin 12 :right-margin 12  :color (gethash :white *color-table-decimal*)
		      :font "Helvetica-Oblique" :font-size 11)
	"Not sure who to call? Just contact Genworks" :eol
	"directly. We will listen to your concerns and" :eol
	"steer you in the right direction!"))))))

(define-object bottom-middle-note (geom-base::typeset-block)
  :functions
  ((content 
    ()
    (let ((typeset::*leading-ratio* 1.1))
      (typeset::compile-text 
       ()
       
       (vspace 3)
       
       (with-p
	   (:h-align :center :left-margin 12 :right-margin 12  :color (gethash :white *color-table-decimal*)
		     :font "Helvetica" :font-size 9)
	
	   #?"\251" "2004 Genworks International" :eol
	   "255 E Brown, Suite 310" :eol
	   "Birmingham, MI 48009 USA" :eol
	   "+1 248-910-0912, +1 248-330-2979" :eol)
	 
       (vspace 3)
       
       (with-p
	(:h-align :center :left-margin 12 :right-margin 12  :color (gethash :white *color-table-decimal*)
		  :font "Helvetica-Oblique" :font-size 9 :text-x-scale (the text-x-scale))
	"This pamphlet was created and typeset entirely with GDL/GWL."))))))


#+nil
(define-object landmark-with-circles (landmark:assembly)
  :objects
  ((circles :type 'ellipse
	    :sequence (:size 10)
	    :center (make-point 0 (the profile p1-y) 0)
	    :display-controls (list :line-thickness 0.5 
				    :color (ecase (the-child index)
					     (0 "#000000")
					     (1 "#002200")
					     (2 "#004A00")
					     (3 "#016001")
					     (4 "#016501")
					     (5 "#026A02")
					     (6 "#027002")
					     (7 "#057805")
					     (8 "#107C10")
					     (9 "#158315")))
	    
	    :major-axis-length (* (the-child radius) 3)
	    :minor-axis-length (* (the-child radius) 1.5)
	    
	    :radius (if (the-child first?) 10
		      (+ (the-child previous radius) 10
			 (* (the-child index) 7))))))



(defparameter *spec-table-lines* 
    '(("Supported Platforms (64-bit)" "Windows 10, Linux, MacOS")
      ("Memory Capability" "64-bit: 256GB+.")
      ("Geometry Kernel (optional)" "SMS NLib/TSNlib/SMLib provides NURBS, trimmed NURBS, intersections, Boolean solids, and 
standards-compliant geometry input/output.")
      ("Typesetting and vector graphics output" "built-in cl-typesetting and cl-pdf provides precision PDF textual and graphics output.")
      ("Output Protocols/Formats" "including HTTP, PDF, HTML, XML, SEXP, SOAP, DXF, VRML, PNG, JPEG, JSON, SQL, Unicode, OBJ, IGES, STEP, SAT, SMTP.")
      ("Input Protocols/Formats" "including HTTP, HTML, XML, SEXP, SOAP, SQL, Unicode, IGES, STEP, SAT, POP, IMAP.")
      ("Base Language Features" "dynamic typing, automatic memory management, incremental compilation,
source transformation macros, meta-object protocol, multithreading.")
      ("GDL/GWL Language Features" "in-memory object and value caching, dependency-tracking, demand-driven updating, 
primitives library, flexible output formats and skins, object-oriented database wrappers, tight object-to-URI mapping,
web session tracking.")
      ("Development Productivity Features" "graphical development inspector and browser, incremental compilation and updating of running application, context-sensitive colorizing and syntax-checking editing environment, client/server development with remote host.")
      ("Performance and Security Features" "optimizing native machine code compiler, automatic object and value caching, internal object and method caches, immune to buffer-overflow attacks and most other common exploits, support for Secure Socket Layer.")
      ("Support" "supported customers receive access to  unlimited technical support, available by 
telephone and email.")
      ("Runtime Distribution" "Enterprise GDL can generate small-footprint runtimes with zero-risk licensing terms.")))
	

(define-object panel-2-note-s1 (geom-base::typeset-block)
  :functions
  (
   (content
    ()
    (typeset::compile-text
     ()
     (with-p (:h-align :center :left-margin 12 :right-margin 12)
       (vspace 179)
       (with-style (:text-x-scale 0.69 :font "Helvetica-Bold" :font-size 13)
	 "The GDL/GWL Application Development System.")
       (vspace 1.5)
       (typeset::table
	(:padding 2 :cell-padding 1.5  :col-widths (list 60 160) :border 0.1 
		  :background-color (gethash :white *color-table-decimal*))
	(dolist (row *spec-table-lines*)
	  (typeset::row 
	   ()
	   (typeset::cell 
	    () 
	    (with-style 
		(:text-x-scale (the text-x-scale) :font "Helvetica-Bold" :font-size 7) 
	      (typeset::put-string (first row))))
	
	   (typeset::cell 
	    () 
	    (with-style 
		(:text-x-scale (the text-x-scale) :font "Helvetica" :font-size 7) 
	      (typeset::put-string (second row)))))))
       (vspace 24)
       (with-style (:text-x-scale 1.10 :font "Courier-BoldOblique" :font-size 13) 
	 "www.genworks.com"))))))


(define-lens (pdf panel-2-note-s1)()
  :output-functions
  ((additional-non-text
    ()

    #+nil
    (gdl::with-format-slots (view-scale)
      (let ((view-scale (or view-scale 1)))
	(pdftry::full-logo :start-x -169 :start-y -372 :scale-x (* view-scale 0.59) :scale-y (* view-scale 0.59)))))))

(cl-interpol:disable-interpol-syntax)
