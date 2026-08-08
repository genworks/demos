(in-package :naca-nurbs)

(define-object ui (session-control-mixin base-html-page)
  
  :input-slots
  ((title "NACA NURBS Playground")
   (objects (list (the nurbs upper-composed)
                  (the nurbs lower-composed)))
   
   (airfoil :0021 :settable)
   (n-points 216 :settable)
   (approx-tolerance 0.0005 :settable)
   (degree 5 :settable)
   (split-x-default 0.15 :settable)
   (cosine? t :settable)
   (closed? t :settable)
   (adaptive-split? t :settable)
   (target-main-points 9 :settable)
   (target-nose-points 7 :settable)
   (use-analytical-tangents? t :settable)
   (use-section-tolerances? t :settable))

  :computed-slots
  ((use-svgpanzoom? t)
   (use-x3dom? t)
   (use-raphael? nil)

   (selected-airfoil
    (let* ((raw (the airfoil-control value))
           (code-string (if (symbolp raw) (symbol-name raw) (string raw)))
           (digits (coerce (remove-if-not #'digit-char-p code-string) 'string)))
      (if (plusp (length digits))
          (intern digits :keyword)
          (the airfoil))))

   (additional-header-content
    (string-append
     (with-lhtml-string () (:script (str (the viewport-renderers-js-baseline))))
     (call-next-method)
     (with-lhtml-string ()
       (:link :href "/demo/css/demos-style.css"
              :rel "stylesheet" :type "text/css"))))

   (body
    (with-lhtml-string ()
      (:div :class "max-w-7xl mx-auto p-8"
        (:div :class "text-center mb-8"
          (:h1 :class "text-4xl font-bold text-gray-900 mb-2"
               "NACA NURBS Playground")
          (:p :class "text-lg text-gray-600"
              "Interactive NACA airfoil curve generation with NURBS approximation"))
        (:div :class "bg-white rounded-lg shadow-lg p-6 mb-8"
          (:h2 :class "text-xl font-semibold text-gray-800 mb-4"
               "Parameters")
          (str (the control-form)))
        (:div :class "flex flex-col lg:flex-row gap-8"
          (:div :class "flex-1 min-w-0 bg-white rounded-lg shadow-lg overflow-hidden"
            (:div :class "px-6 py-4 bg-gray-50 border-b border-gray-200"
              (:h3 :class "text-lg font-medium text-gray-900"
                   "Left Viewport")
              (:p :class "text-sm text-gray-600"
                  "Interactive view"))
            (:div :class "p-6 overflow-auto relative" :style "height: 600px;"
              (:div :class "param-field absolute top-2 left-2 z-20 bg-white/90 backdrop-blur px-2 py-1 rounded-md border border-gray-200 shadow-sm"
                (str (the left-format-selector form-control)))
              (str (the left-viewport-area div))))
          (:div :class "flex-1 min-w-0 bg-white rounded-lg shadow-lg overflow-hidden"
            (:div :class "px-6 py-4 bg-gray-50 border-b border-gray-200"
              (:h3 :class "text-lg font-medium text-gray-900"
                   "Right Viewport")
              (:p :class "text-sm text-gray-600"
                  "Interactive view"))
            (:div :class "p-6 overflow-auto relative" :style "height: 600px;"
              (:div :class "param-field absolute top-2 left-2 z-20 bg-white/90 backdrop-blur px-2 py-1 rounded-md border border-gray-200 shadow-sm"
                (str (the right-format-selector form-control)))
              (str (the right-viewport-area div)))))
        (:div :class "grid grid-cols-1 md:grid-cols-3 gap-6 mt-8"
          (str (the curve-stats-panel))
          (str (the quality-info-panel))
          (str (the split-info-panel))))))

   (control-form
    (with-form-string (:class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6")
      (:div :class "space-y-2"
            (:label :class "block text-sm font-medium text-gray-700" :for (the airfoil-control id)
                    "Airfoil Type")
            (:div :class "param-field block w-full rounded-md border border-gray-300 bg-white px-3 py-2 shadow-sm"
		  (str (the airfoil-control form-control))))
      (:div :class "space-y-2"
            (:label :class "block text-sm font-medium text-gray-700" :for (the n-points-control id)
                    "Number of Points")
            (:div :class "param-field block w-full rounded-md border border-gray-300 bg-white px-3 py-2 shadow-sm"
		  (str (the n-points-control form-control))))
      (:div :class "space-y-2"
            (:label :class "block text-sm font-medium text-gray-700" :for (the approx-tolerance-control id)
                    "Tolerance")
            (:div :class "param-field block w-full rounded-md border border-gray-300 bg-white px-3 py-2 shadow-sm"
		  (str (the approx-tolerance-control form-control))))
      (:div :class "flex items-end"
            (:button :type "submit"
                     :class "w-full bg-indigo-600 text-white px-4 py-2 rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 transition-colors"
                     "Update"))))

   (curve-stats-panel
    (with-lhtml-string ()
      (:div :class "bg-white rounded-lg shadow p-6"
        (:h4 :class "text-lg font-medium text-gray-900 mb-4" "Curve Statistics")
        (:div :class "space-y-3 text-sm"
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Upper Control Points:")
            (:span :class "font-medium" (fmt "~A" (length (the nurbs upper-composed control-points)))))
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Lower Control Points:")
            (:span :class "font-medium" (fmt "~A" (length (the nurbs lower-composed control-points)))))
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Upper Length:")
            (:span :class "font-medium" (fmt "~,4F" (the nurbs upper-composed total-length))))
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Lower Length:")
            (:span :class "font-medium" (fmt "~,4F" (the nurbs lower-composed total-length))))))))

   (quality-info-panel
    (with-lhtml-string ()
      (:div :class "bg-white rounded-lg shadow p-6"
        (:h4 :class "text-lg font-medium text-gray-900 mb-4" "Quality Metrics")
        (:div :class "space-y-3 text-sm"
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Nose Upper Tolerance:")
            (:span :class "font-medium" (fmt "~,1E" (the nurbs nose-upper-approx achieved-tolerance))))
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Nose Lower Tolerance:")
            (:span :class "font-medium" (fmt "~,1E" (the nurbs nose-lower-approx achieved-tolerance))))
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Main Upper Tolerance:")
            (:span :class "font-medium" (fmt "~,1E" (the nurbs main-upper-approx achieved-tolerance))))
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Main Lower Tolerance:")
            (:span :class "font-medium" (fmt "~,1E" (the nurbs main-lower-approx achieved-tolerance))))))))

   (split-info-panel
    (with-lhtml-string ()
      (:div :class "bg-white rounded-lg shadow p-6"
        (:h4 :class "text-lg font-medium text-gray-900 mb-4" "Split Analysis")
        (:div :class "space-y-3 text-sm"
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Upper Split X:")
            (:span :class "font-medium" (fmt "~,4F" (getf (the nurbs split-x) :upper))))
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Lower Split X:")
            (:span :class "font-medium" (fmt "~,4F" (getf (the nurbs split-x) :lower))))
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Adaptive Split:")
            (:span :class "font-medium" (str (if (the nurbs adaptive-split?) "Yes" "No"))))
          (:div :class "flex justify-between"
            (:span :class "text-gray-600" "Analytical Tangents:")
            (:span :class "font-medium" (str (if (the nurbs use-analytical-tangents?) "Yes" "No")))))))))

  :objects
  ((airfoil-control :type 'menu-form-control
                    :default (the airfoil)
                    :size 1
                    :choice-plist '(:0012 "NACA 0012"
                                   :0021 "NACA 0021"
                                   :2412 "NACA 2412"
                                   :23012 "NACA 23012"))

   (n-points-control :type 'number-form-control
                     :default (the n-points)
                     :domain :number
                     :min 50
                     :max 500
                     :step 1)

   (approx-tolerance-control :type 'number-form-control
                             :default (the approx-tolerance)
                             :domain :number
                             :min 0.0001
                             :max 0.01
                             :step 0.0001)

   (nurbs :type 'naca-nurbs-curves
          :airfoil (the selected-airfoil)
          :n-points (the n-points-control value)
          :approx-tolerance (the approx-tolerance-control value)
          :degree (the degree)
          :split-x-default (the split-x-default)
          :cosine? (the cosine?)
          :closed? (the closed?)
          :adaptive-split? (the adaptive-split?)
          :target-main-points (the target-main-points)
          :target-nose-points (the target-nose-points)
          :use-analytical-tangents? (the use-analytical-tangents?)
          :use-section-tolerances? (the use-section-tolerances?))

   (left-format-selector :type 'menu-form-control
                         :default :svg
                         :size 1
                         :ajax-submit-on-change? t
                         :choice-plist '(:svg "Wireframe"
                                        :x3dom "Shaded"))

   (right-format-selector :type 'menu-form-control
                          :default :x3dom
                          :size 1
                          :ajax-submit-on-change? t
                          :choice-plist '(:svg "Wireframe"
                                         :x3dom "Shaded"))

   (left-viewport-area :type 'base-html-div
                       :div-class "absolute inset-0"
                       :js-to-eval (ecase (the left-format-selector value)
                                     (:svg (the left-view-svg js-to-eval))
                                     (:x3dom (the left-view-x3dom js-to-eval)))
                       :inner-html (ecase (the left-format-selector value)
                                     (:svg (the left-view-svg inner-html))
                                     (:x3dom (the left-view-x3dom inner-html))))

   (right-viewport-area :type 'base-html-div
                        :div-class "absolute inset-0"
                        :js-to-eval (ecase (the right-format-selector value)
                                      (:svg (the right-view-svg js-to-eval))
                                      (:x3dom (the right-view-x3dom js-to-eval)))
                        :inner-html (ecase (the right-format-selector value)
                                      (:svg (the right-view-svg inner-html))
                                      (:x3dom (the right-view-x3dom inner-html))))

   (left-view-svg :type 'viewport-html-div
                  :dom-id (the left-viewport-area dom-id)
                  :image-format :svg
                  :projection-key :top
                  :display-list-objects (the objects))

   (left-view-x3dom :type 'viewport-html-div
                    :dom-id (the left-viewport-area dom-id)
                    :image-format :x3dom
                    :projection-key :top
                    :display-list-objects (the objects))

   (right-view-svg :type 'viewport-html-div
                   :dom-id (the right-viewport-area dom-id)
                   :image-format :svg
                   :projection-key :top
                   :display-list-objects (the objects))

   (right-view-x3dom :type 'viewport-html-div
                     :dom-id (the right-viewport-area dom-id)
                     :image-format :x3dom
                     :projection-key :top
                     :display-list-objects (the objects))))

