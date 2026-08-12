;;;; -*- Mode: Lisp; Package: naca-nurbs -*-

;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;;
;;;; Web UI on the shared demo chrome (demos-common:demo-ui-mixin):
;;;; this file keeps only the naca-specific page content, form
;;;; controls, and model hookup.  session-control-mixin keeps public
;;;; instances mortal: without it every visit and every crawler hit
;;;; mints an immortal instance.

(in-package :naca-nurbs)

(define-object ui (demos-common:demo-ui-mixin
                   session-control-mixin base-html-page)

  :documentation
  (:description "Interactive NACA airfoil demo page: analytical curve
generation with NURBS approximation, shown in two switchable 2D/3D
viewports with quality metrics below."
   :author "Genworks International")

  :input-slots
  ((title "NACA NURBS Playground")
   (objects (list (the nurbs upper-composed)
                  (the nurbs lower-composed)))

   (airfoil :0021 :settable)
   (n-points 216 :settable)
   (approx-tolerance 0.0005 :settable))

  :computed-slots
  ((use-raphael? nil)

   (source-pane-objects '(naca-nurbs-curves quality-reports-mixin
                          ui airfoil-viewport demos-common:demo-ui-mixin))
   (source-pane-functions '(generate-naca-samples get-airfoil-spec
                            analytical-tangent-parametric
                            analytical-curvature-parametric
                            x->t t->x lower-bound
                            ternary-search-maximum find-max-gradient-region
                            respond-with-cad-download))

   ;; The menu control round-trips its keyword through the form as a
   ;; string; normalize back to the catalog keyword.
   (selected-airfoil
    (let ((digits (remove-if-not #'digit-char-p
                                 (string (the airfoil-control value)))))
      (if (plusp (length digits))
          (intern digits :keyword)
          (the airfoil))))

   (body
    (with-lhtml-string ()
      (:div :class "max-w-7xl mx-auto p-8"
        (str (the (page-intro "NACA NURBS Playground"
                              "Interactive NACA airfoil curve generation with NURBS approximation")))
        (str (the (card :title "Parameters"
                        :content (with-lhtml-string ()
                                   (:div :class "p-6"
                                     (str (the control-form))
                                     (str (the download-buttons)))))))
        (:div :class "flex flex-col lg:flex-row gap-8"
          (dolist (viewport (list-elements (the viewports)))
            (htm
             (:div :class "flex-1 min-w-0 bg-white rounded-lg shadow-lg overflow-hidden"
               (:div :class "px-6 py-4 bg-gray-50 border-b border-gray-200"
                 (:h3 :class "text-lg font-medium text-gray-900"
                      (str (the-object viewport heading)))
                 (:p :class "text-sm text-gray-600"
                     "Switch between 2D vector and 3D interactive rendering"))
               (:div :class "p-6 overflow-auto relative" :style "height: 440px;"
                 (:div :class "param-field absolute top-2 left-2 z-20 bg-white/90 backdrop-blur px-2 py-1 rounded-md border border-gray-200 shadow-sm"
                   (str (the-object viewport format-selector form-control)))
                 (str (the-object viewport area div)))))))
        (str (the stats-section div))
        (:div :class "mt-8"
          (str (the (card :title "Source Code"
                          :subtitle "The complete GDL source for this demo: object definitions reconstituted live from the in-memory definitions serving this page, supporting functions via the Lisp's own source records."
                          :content (the source-code-panes))))))))

   ;; Ajax-refreshable section: with live-on-change parameters, the
   ;; stats row re-renders on every model rebuild (see stats-section).
   (stats-row
    (with-lhtml-string ()
      (:div :class "grid grid-cols-1 md:grid-cols-3 gap-6 mt-8"
          (str (the (stats-card "Curve Statistics"
                     (list (list "Upper Control Points:"
                                 (format nil "~a" (length (the nurbs upper-composed control-points))))
                           (list "Lower Control Points:"
                                 (format nil "~a" (length (the nurbs lower-composed control-points))))
                           (list "Upper Length:"
                                 (format nil "~,4f" (the nurbs upper-composed total-length)))
                           (list "Lower Length:"
                                 (format nil "~,4f" (the nurbs lower-composed total-length)))))))
          (str (the (stats-card "Quality Metrics"
                     (list (list "Nose Upper Tolerance:"
                                 (format nil "~,1e" (the nurbs nose-upper-approx achieved-tolerance)))
                           (list "Nose Lower Tolerance:"
                                 (format nil "~,1e" (the nurbs nose-lower-approx achieved-tolerance)))
                           (list "Main Upper Tolerance:"
                                 (format nil "~,1e" (the nurbs main-upper-approx achieved-tolerance)))
                           (list "Main Lower Tolerance:"
                                 (format nil "~,1e" (the nurbs main-lower-approx achieved-tolerance)))))))
          (str (the (stats-card "Split Analysis"
                     (list (list "Upper Split X:"
                                 (format nil "~,4f" (getf (the nurbs split-x) :upper)))
                           (list "Lower Split X:"
                                 (format nil "~,4f" (getf (the nurbs split-x) :lower)))
                           (list "Adaptive Split:"
                                 (if (the nurbs adaptive-split?) "Yes" "No"))
                           (list "Analytical Tangents:"
                                 (if (the nurbs use-analytical-tangents?) "Yes" "No")))))))))

   (control-form
    (with-form-string (:class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6")
      (str (the (param-field "Airfoil Type"      (the airfoil-control))))
      (str (the (param-field "Number of Points"  (the n-points-control))))
      (str (the (param-field "Tolerance"         (the approx-tolerance-control))))))

   ;; CAD export of the current session's curves; the server side is
   ;; respond-with-cad-download in publish.lisp (shown in the Source
   ;; Code panes) -- one with-format call per format.  The download
   ;; attribute names the file client-side; the server sends
   ;; content-disposition as well.
   (download-buttons
    (flet ((link (label format extension)
             (with-lhtml-string ()
               (:a :href (format nil "~a?iid=~a&format=~a"
                                 *cad-download-path* (the instance-id) format)
                   :download (format nil "naca-~a.~a"
                                     (remove-if-not #'digit-char-p
                                                    (string (the selected-airfoil)))
                                     extension)
                   :class "inline-flex items-center rounded-md border border-gray-300 bg-white px-4 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50"
                   (str label)))))
      (with-lhtml-string ()
        (:div :class "flex flex-wrap gap-6 mt-6 pt-6 border-t border-gray-200"
          (str (link "Download IGES" "iges" "igs"))
          (str (link "Download STEP" "step" "stp")))))))

  :functions
  (;; One small stat card: label/value rows under a heading.
   (stats-card
    (heading rows)
    (with-lhtml-string ()
      (:div :class "bg-white rounded-lg shadow p-6"
        (:h4 :class "text-lg font-medium text-gray-900 mb-4" (str heading))
        (:div :class "space-y-3 text-sm"
          (dolist (row rows)
            (htm (:div :class "flex justify-between"
                   (:span :class "text-gray-600" (str (first row)))
                   (:span :class "font-medium" (str (second row)))))))))))

  :objects
  ((airfoil-control :type 'menu-form-control
                    :default (the airfoil)
                    :size 1
                    :ajax-submit-on-change? t
                    :choice-plist '(:0012 "NACA 0012"
                                   :0021 "NACA 0021"
                                   :2412 "NACA 2412"
                                   :23012 "NACA 23012"))

   (n-points-control :type 'number-form-control
                     :default (the n-points)
                     :ajax-submit-on-change? t
                     :domain :number :min 50 :max 500 :step 1)

   (approx-tolerance-control :type 'number-form-control
                             :default (the approx-tolerance)
                             :ajax-submit-on-change? t
                             :domain :number
                             :min 0.0001 :max 0.01 :step 0.0001)

   (nurbs :type 'naca-nurbs-curves
          :airfoil (the selected-airfoil)
          :n-points (the n-points-control value)
          :approx-tolerance (the approx-tolerance-control value))

   (stats-section :type 'base-html-div
                  :inner-html (the stats-row))

   ;; The selectors live here (not inside airfoil-viewport) because
   ;; form-controls need an ajax-sheet ancestor to wire their events.
   (format-selectors :type 'menu-form-control
                     :sequence (:size 2)
                     :default (ecase (the-child index) (0 :svg) (1 :x3dom))
                     :size 1
                     :ajax-submit-on-change? t
                     :choice-plist '(:svg "2D Vector"
                                     :x3dom "3D Interactive"))

   (viewports :type 'airfoil-viewport
              :sequence (:size 2)
              :heading (ecase (the-child index)
                         (0 "Left Viewport") (1 "Right Viewport"))
              :format-selector (the (format-selectors (the-child index)))
              :display-list-objects (the objects))))


;; sheet-section (not plain base-object): gdlAjax's section discovery
;; only descends into sheet-sections, so the nested area divs would
;; otherwise never be replaced on ajax updates.
(define-object airfoil-viewport (sheet-section)

  :documentation
  (:description "One switchable viewport: the parent's menu
form-control flips the same div between a 2D vector (svg) and a 3D
interactive (x3dom) rendering of the curves."
   :author "Genworks International")

  :input-slots
  (heading display-list-objects format-selector
   ;; the demo page, for viewport-html-div's sheet-level settings
   (containing-page (the parent)))

  :objects
  ((area :type 'base-html-div
         :div-class "absolute inset-0"
         :js-to-eval (ecase (the format-selector value)
                       (:svg (the view-svg js-to-eval))
                       (:x3dom (the view-x3dom js-to-eval)))
         :inner-html (ecase (the format-selector value)
                       (:svg (the view-svg inner-html))
                       (:x3dom (the view-x3dom inner-html))))

   (view-svg :type 'viewport-html-div
             :dom-id (the area dom-id)
             :image-format :svg
             :page-width-pt 760 :page-length-pt 480
             :projection-key :top
             :pass-down (containing-page display-list-objects))

   (view-x3dom :type 'viewport-html-div
               :dom-id (the area dom-id)
               :image-format :x3dom
               :projection-key :top
               :pass-down (containing-page display-list-objects))))
