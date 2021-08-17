(in-package :pamphlet)


(cl-interpol:enable-interpol-syntax)

(defconstant +odq+ #?"\223")
(defconstant +cdq+ #?"\224")

(cl-interpol:disable-interpol-syntax)

;;
;; FLAG -- following two go to primitives.
;;
(define-object pdf-image (base-object)
  :input-slots
  (image-file (keep-aspect-ratio? t)))

(define-lens (pdf pdf-image)()
  :output-functions
  ((cad-output
    ()
    (gdl::with-format-slots (view-scale)
      (let ((view-scale (or view-scale 1)))
	(let ((image (make-jpeg-image (the image-file))))
	  (add-images-to-page image)
	  (draw-image image (* (- (get-x (the center)) (half (the width))) view-scale)
		      (* (- (get-y (the center)) (half (the height))) view-scale)
		      (* (the width) view-scale) 
		      (* (the length) view-scale) 0 (the keep-aspect-ratio?))))))))


(define-object side-1 (base-drawing)
  :input-slots
  ((background-color :green-forest)
   (border-boxes?  ;;t 
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
   (panel-length (* (the page-length) (the y-percentage))))


  :hidden-objects
  (
   ;;(bracket :type 'bracket:assembly)
   
   (gauge :type 'gauge-plate:gauge-with-cost)

   ;;(gauge :type 'box :length 10 :width 20 :height 30)
   
   (panel-1-note :type 'panel-1-note
		 :length (the panel-1 length) :width (the panel-1 width))


   
   (bpo-note :type 'bpo-note
	     :length (* (the panel-1 page-length) 0.20)
	     :width (* (the panel-1 page-width) 0.45))
   
   
   (bottom-color-bar :type 'global-polyline
		     :display-controls (list :color (the background-color) :fill-color (the background-color))
		     :vertex-list (list (the (vertex :top :left :rear))
					(the (vertex :top :right :rear))
					(the (vertex :top :right :front))
					(the (vertex :top :left :front))
                                        (the (vertex :top :left :rear)))
		     ;;:closed? t
		     :length (the bottom-panel page-length)
		     :width (the bottom-panel page-width))
   (bottom-note :type 'bottom-note
		:length (the bottom-panel length) :width (the bottom-panel width))
   (panel-2-note :type 'panel-2-note
		 :length (the panel-2 length)  :width (the panel-2 width))
      
   ;;(bus :type 'bus)
   
   (panel-3-note :type 'panel-3-note
		 :length (the panel-3 length)  :width (the panel-3 width))
   (pie :type 'pie-sample :length (the pie-panel length) :width (the pie-panel width))

   #+allegro
   (mountain :type 'surf::test-fitted-surface
	     'test-trimmed-from-projected-2)

   #-allegro (mountain :type 'box :length 10 :width 20 :height 8)
   
   
   (bus-image :type 'pdf-image
	      ;;:image-file "~/genworks/gdl/apps/pamphlet/images/bus.jpg"
	      :image-file (merge-pathnames "images/bus.jpg" *home*)
	      :width (* (the panel-2 page-width) 0.8)
	      :length (the-child width)
	      :keep-aspect-ratio? t
	      :center (make-point (- (* (the panel-2 page-width) 0.003))
				  (* (the panel-2 page-length) 0.017)))
   
   )


  :objects
  ((panel-1 :type 'base-view
	    ;;:page-width (the panel-width)
	    ;;:page-length (the panel-length)
	    :width (the panel-width)
	    :length (the panel-length)
	    :center (translate (the center) :left (the-child page-width))
	    :border-box? (the border-boxes?)
	    :projection-vector (getf *standard-views* :top)
	    ;;:fit-to-page? nil
	    :view-scale 1
	    :objects (list (the panel-1-note))
	    :object-roots (list ))
   
   
   (bpo-note-panel :type 'base-view
		   :center (translate (the panel-1 center)
				      :front (* (the panel-1 page-length) 0.24)
				      :right (* (the panel-1 page-width) 0.24))
		   ;;:page-width (the bpo-note width)
		   ;;:page-length (the bpo-note length)
		   :width (the bpo-note width)
		   :length (the bpo-note length)
		   
		   ;;:fit-to-page? nil
		   :view-scale 1
		   :border-box? (the border-boxes?)
		   :objects (list (the bpo-note)))

   (gauge-panel :type 'base-view
		:center (translate (the panel-1 center)
				   :front (* (the panel-1 page-length) 0.235)
				   :left (* (the panel-1 page-width) 0.23))
		;;:page-width (* (the panel-2 page-width) 0.77)
		;;:page-length (* (the panel-2 page-length) 0.3)
		:width (* (the panel-2 page-width) 0.77)
		:length (* (the panel-2 page-length) 0.3)

		;;:objects (list (the gauge drawing gauge-views))
		;;:view-scale 1.5
		:user-scale 0.6
		:projection-vector (getf *standard-views* :top)
		:object-roots (list (the gauge plate))
		:border-box? nil
		;;:projection-vector (getf *standard-views* :top)
		)
   

   (bottom-panel :type 'base-view
		 ;;:page-width (* (the panel-1 page-width) 3)
		 ;;:page-length (* (the panel-1 page-length) 0.10)

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
		 :border-box? nil)

   (panel-2 :type 'base-view
	    ;;:page-width (the panel-width)
	    ;;:page-length (the panel-length)
	    :width (the panel-width)
	    :length (the panel-length)
	    :border-box? (the border-boxes?)
	    :projection-vector (getf *standard-views* :top)
	    ;;:fit-to-page? nil
	    :objects (list (the panel-2-note) (the bus-image)))

   
   (panel-3 :type 'base-view
	    ;;:page-width (the panel-width)
	    ;;:page-length (the panel-length)
	    :width (the panel-width)
	    :length (the panel-length)
	    
	    :border-box? (the border-boxes?)
	    :projection-vector (getf *standard-views* :top)
	    ;;:fit-to-page? nil
	    :center (translate (the center) :right (the-child page-width))
	    :objects (list (the panel-3-note)))
  
   (pie-panel :type 'base-view
	      ;;:page-width (* (the panel-3 page-width) 0.75)
	      ;;:page-length (* (the panel-3 page-length) 0.6)
	      :width (* (the panel-3 page-width) 0.75)
	      :length (* (the panel-3 page-length) 0.6)

	      :objects (list (the pie))
	      :center (translate (the panel-3 center) :rear 32 :left 50)
	      :border-box? nil
	      ;;:left-margin 5
	      :view-scale .4

	      :projection-vector (getf *standard-views* :top))
  
   (mountain-panel :type 'base-view
		   ;;:page-width (the pie-panel page-width)
		   ;;:page-length (* (the pie-panel page-length) 1.2)
		   :width (the pie-panel page-width)
		   :length (* (the pie-panel page-length) 1.2)

		   :object-roots (list (the mountain))
		   ;;:objects (list (the mountain) (the mountain basis-surface) 
		   ;;(the mountain raised-island) (the mountain raised-hole))
		   :left-margin 30
		   :center (translate (the pie-panel :center) :right 95 :front 11)
		   :border-box? nil :projection-vector (getf *standard-views* :trimetric))
   
))

;;
;; FLAG -- move to primitives
;;
(defmacro with-p (&rest args) `(typeset::paragraph ,@args))
(defmacro with-style (&rest args) `(typeset::with-style ,@args))
(defmacro vspace (&rest args) `(typeset::vspace ,@args))
(defmacro hspace (&rest args) `(typeset::hspace ,@args))


(define-object bpo-note (geom-base::typeset-block)
  :functions
  (
   (content
    ()
    (typeset::compile-text
     ()
     (with-p (:text-x-scale 0.90)
       (typeset::table
	(:padding 2 :cell-padding 2 :col-widths (list (* (the width) 0.30)
						      (* (the width) 0.10)
						      (* (the width) 0.25)
						      (* (the width) 0.25)) 
		  :border 0
		  :background-color (gethash "#66ccff" *color-table-decimal*))
	(typeset::row
	 ()
	 (typeset::cell (:col-span 4 :background-color (gethash :white *color-table-decimal*))
			(with-p (:font-size 13)			  
			  (with-style (:font "Times-Bold" :h-align :center) "Select a Project"))))
	(typeset::row
	 ()
	 (typeset::cell () (with-style (:font "Times-Bold" :h-align :center ) "Name"))
	 (typeset::cell () (with-style (:font "Times-Bold" :h-align :center) "A"))
	 (typeset::cell () (with-style (:font "Times-Bold" :h-align :center) "Own"))
	 (typeset::cell () (with-style (:font "Times-Bold" :h-align :center) "Desc")))
	
	(typeset::row
	 (:background-color (gethash :white *color-table-decimal*))
	 (typeset::cell () (with-style (:color (gethash :blue *color-table-decimal*)) 
			     (typeset::put-string "R&D")
			     
			     ))
	 (typeset::cell () "T")
	 (typeset::cell () "Star")
	 (typeset::cell () "Auto"))
	
	(typeset::row
	 (:background-color (gethash :white *color-table-decimal*))
	 (typeset::cell () (with-style (:color (gethash :blue *color-table-decimal*)) "Sales"))
	 (typeset::cell () "T")
	 (typeset::cell () "Hope")
	 (typeset::cell () "2003"))
	
	(typeset::row
	 (:background-color (gethash :white *color-table-decimal*))
	 (typeset::cell () (with-style (:color (gethash :blue *color-table-decimal*)) "Info"))
	 (typeset::cell () "T")
	 (typeset::cell () "Tom")
	 (typeset::cell () "Price"))
	
	(typeset::row
	 (:background-color (gethash :white *color-table-decimal*))
	 (typeset::cell () (with-style (:color (gethash :blue *color-table-decimal*)) "Cont"))
	 (typeset::cell () "T")
	 (typeset::cell () "Sue")
	 (typeset::cell () "Fax"))
	
	))))))

(define-lens (pdf bpo-note)()
  :output-functions
  ((additional-non-text
    ()
    (gdl::with-format-slots (view-scale)
      (let ((view-scale (or view-scale 1)))
	(scale view-scale view-scale)
	(apply #'set-rgb-stroke (gethash :blue *color-table-decimal*))
	(move-to -55 7)
	(line-to -33 7)
	(stroke)
	(move-to -55 -12)
	(line-to -28 -12)
	(stroke)
	(move-to -55 -30)
	(line-to -38 -30)
	(stroke)
	(move-to -55 -49)
	(line-to -32 -49)
	(stroke))))))

(define-object panel-1-note (geom-base::typeset-block)
  :functions
  ((content 
    ()
    (let ((typeset::*leading-ratio* 1.27))
      (typeset::compile-text 
       ()
       (with-style (:h-align  :justified :font-size 12 :left-margin 12 :right-margin 12 :text-x-scale (the text-x-scale))
	 (vspace 25)
	 (with-p (:h-align :center :color (gethash :red-violet-medium *color-table-decimal*) :font-size 27)
	   "What is GDL?")
	 (vspace 7)
	 (with-p ()
	   (with-style (:font "Helvetica-Bold" :font-size 12 :color (gethash :orange *color-table-decimal*))
	     "A no-nonsense approach for creating web-based and technical software applications.")
	   " GDL, or General-purpose Declarative Language, enables rapid development 
of many types of end-user computer applications. With the convenience of a spreadsheet, 
it delivers the flexibility and capacity of a state-of-the-art Internet-savvy programming 
environment.")
	 (with-p ()
	   (vspace 7)
	   (with-style (:font "Helvetica-Bold" :font-size 12 :color (gethash :orange *color-table-decimal*))
	     "Web-centric, " +odq+ "clientless" +cdq+ " deployment.")
	   (with-style (:font-size 12)
	     " Both GDL and the applications you create with it run "
	     +odq+ "out-of-the-box" +cdq+ " as pure web services, with no need for specialized 
software or configuration on users' computers. This ensures trouble-free 
and low-maintenance operation across all major computer varieties."))
	 (with-p ()
	   (vspace 7)
	   (with-style (:font "Helvetica-Bold" :font-size 12 :color (gethash :orange *color-table-decimal*))
	     "The best support available.")
	   (with-style (:font-size 12)
	     " GDL is built on a commercial-grade, standards-based foundation, backed by a support network with an average
of 16 years industry experience in engineering, business, and high-productivity programming tools."))
	 
	 (with-p (:h-align :center)
	   (vspace 7)
	   (with-style (:font "Helvetica-Bold" :font-size 12 :text-x-scale 0.85)
	     "Examples of Typical Applications:"))
	 
	 (vspace 141)
	 
	 (with-p (:h-align :left :font "Helvetica-Oblique" :font-size 10)
	   (hspace 0)
	   "Design & Engineering Automation." (hspace 22) "Business Project Tracking.")

	 ))))))


(define-object panel-2-note (geom-base::typeset-block)
  :functions
  ((content 
    ()
    (let ((typeset::*leading-ratio* 1.27))
      (typeset::compile-text 
       ()
       (with-style (:h-align  :justified :font-size 12 :left-margin 12 :right-margin 12 :text-x-scale (the text-x-scale))
	 (with-p (:h-align :center :color (gethash :orange *color-table-decimal*) :font-size 27)
	   (vspace 25) "What you need:")
	 (vspace 9)
	 (with-p ()
	   (with-style (:font "Helvetica-Bold"  :color (gethash :orange *color-table-decimal*))
	     "Smaller, cleaner solutions for business and engineering.")
	   " You are working under unprecedented time and budget constraints in this age 
of rapid globalization. Are your current tools up to the challenge?")
	 (vspace 175)
	 (with-p ()
	   (with-style (:font "Helvetica-Bold"  :color (gethash :orange *color-table-decimal*))
	     "Shorter development cycles.")
	   " Tools which cannot keep pace with today's extreme programming paradigms can pose an acute handicap. When
project schedules begin to slip, not only do costs increase, so do the risks of the project missing its "
	  +odq+ "window of opportunity." +cdq+
	  " You need a " +odq+ "secret weapon" +cdq+ " which arms you with the stability and flexibility to thrive in today's environment.")
	 (vspace 8)
	 (with-p ()
	   (with-style (:font "Helvetica-Bold"  :color (gethash :orange *color-table-decimal*))
	     "Low maintenance costs and high longevity.")
	   " In many cases, the cost of long-term maintenance for a software 
project can represent a staggering 80% of the overall cost. Compounding 
this, applications which are based on fleeting " +odq+ "fashionable" +cdq+ " 
technologies  become outdated remarkably soon and require even more costly 
and risky rework. You need tools which can handle long-term maintenance 
in a reliable and cost-effective manner.")))))))
  

(define-object panel-3-note (geom-base::typeset-block)
  :functions
  ((content 
    ()
    (let ((typeset::*leading-ratio* 1.27))
      (typeset::compile-text 
       ()
       (with-style
	(:h-align :justified :font-size 12 :left-margin 12 :right-margin 18 :text-x-scale (the text-x-scale))
	(vspace 25)
	(with-p (:h-align :center :color (gethash :blue *color-table-decimal*) :font-size 27)
	  "What you get:")
	(vspace 9)
	(with-p ()
	  (with-style (:font "Helvetica-Bold" :color (gethash :orange *color-table-decimal*))
	    "GDL's unique features... more than the sum of their parts.")
	  " General-purpose Declarative Language consists of an ANSI Standard dynamic object-oriented 
programming environment, transparently integrated with an extensible application code generator.
This means you can rapidly develop, incrementally test, and easily debug "
	  (with-style (:font "Helvetica-Bold") "any type")
	  " of web application, from art to architecture, business to engineering, scheduling to manufacturing.")
	(vspace 141)
	(with-p ()
	  (with-style (:font "Helvetica-Bold" :font-size 12 :color (gethash :orange *color-table-decimal*))
	    "Graphics and geometry at your service.")
	  " Your needs for geometry may stop at basic business charts and graphs, or extend all the
way into the wide world of complex curves, surfaces, and solids. Whatever your requirements, GDL 
applications will conveniently perform computations, generate output, and interface to a great 
variety of standard data formats.")
	(vspace 10)
	(with-p ()
	  (with-style (:font "Helvetica-Bold" :font-size 12 :color (gethash :orange *color-table-decimal*))
	    "Why Wait?")
	  " Schedule a "
	  (with-style (:font "Helvetica-Bold") "free")
	  " Trial evaluation of GDL or a "
	  (with-style (:font "Helvetica-Bold") "free")
	  " preliminary Project Impact Assessment.
Genworks and its worldwide representatives are here to listen to your needs and assist you each 
step of the way.")))))))


(define-object bottom-note (geom-base::typeset-block)
  :functions
  ((content 
    ()
    (typeset::compile-text 
     ()
     (vspace 17)
     (with-p 
      (:h-align :center :font "Helvetica-Bold"
		:color (gethash :white *color-table-decimal*) :font-size 17 :text-x-scale (the text-x-scale))
      "BOTTOM LINE? We don't like to brag. Our customers do it for us. Please ask us for a personal recommendation!")))))


(define-object pie-sample (pie-chart)
   :computed-slots
   ((data (list 12 12 33  (- 100 33 12 12)))
    (length 300) (width 300)
    (labels&colors '(("Expenses" :red) ("Revenue" :green-yellow) ("growth" :blue) ("r&d" :cyan)))
    (title "Cash Flow")
    (title-font "Times-Bold")
    (title-font-size 32)))


(publish :path "/side-1"
	 :function #'(lambda(req ent)
		       (gwl-make-part req ent "pamphlet::side-1")))

(publish :path "/assy"
	 :function #'(lambda(req ent)
		       (gwl-make-part req ent "pamphlet::assy")))


(excl:without-package-locks
  (define-object-amendment base-view ()
    :input-slots ((page-length (the length))
		  (page-width (the width))
		  )))


#+nil
(define-object test-trimmed-from-projected-2 (base-object) ;;(trimmed-surface)
  :computed-slots
  ((uv-inputs t)
   (island (the island-3d uv-curve))
   (holes (list (the hole uv-curve)))
   (display-controls (list :color :blue :line-thickness 2)))
  
  :objects
  ((basis-surface :type 'test-fitted-surface
                  :display-controls (list :color :pink)
                  :grid-length 10 :grid-width 10 :grid-height 5
                  )
   
   (raised-hole :type 'b-spline-curve
                :display-controls (list :color :grey-light-very)
                :control-points (list (make-point 3.5 4.5 7)
                                      (make-point 4.5 6 7) 
                                      (make-point 5.5 7 7) 
                                      (make-point 6 4.5 7) 
                                      (make-point 5.5 2 7) 
                                      (make-point 4.5 2 7) 
                                      (make-point 3.5 4.5 7)))

   (raised-island :type 'b-spline-curve
                  :display-controls (list :color :grey-light-very)
                  :control-points (list (make-point 3 5 7)
                                        (make-point 5 8 7) 
                                        (make-point 7 10 7) 
                                        (make-point 8 5 7) 
                                        (make-point 7 0 7) 
                                        (make-point 5 0 7) 
                                        (make-point 3 5 7))))
  :objects
  ((island-3d :type 'projected-curve
              :curve-in (the raised-island)
              :surface (the basis-surface)
              :projection-vector (make-vector 0 0 -1))
   

   (hole :type 'projected-curve
         :curve-in (the raised-hole)
         :surface (the basis-surface)
         :projection-vector (make-vector 0 0 -1))))
