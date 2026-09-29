;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; Web UI on the shared demo chrome (demos-common:demo-ui-mixin):
;;;; the gear-specific page content, form controls and model hookup.
;;;; session-control-mixin keeps public instances mortal.

(in-package :gear)

(defparameter *cad-download-path* "/demo/gear/download")

;; The stateless export's path, declared in publish.lisp; defined here
;; because the page links to it and this file loads first.
(defparameter *cad-export-path* "/demo/gear/cad")

(define-object gear-ui (demos-common:demo-ui-mixin
                        session-control-mixin base-html-page)

  :documentation
  (:description "Interactive involute gear page: the ISO 21771 inputs,
the gear (and its mate) live in the viewport, the drawing numbers
below, a free download of what is on screen, and the paid API beside
it."
   :author "Genworks International")

  :input-slots
  ((title "Involute Gear")
   (demo-slug "gear")
   (view-projection-key :gear-view)
   (view-projection-vector (unitize-vector (make-vector 1 -1 0.7)))

   (module 2 :settable)
   (teeth 20 :settable)
   (pressure-angle 20 :settable)
   (shift 0 :settable)
   (face-width 10 :settable)
   (mate-teeth 0 :settable))

  :computed-slots
  ((source-pane-objects '(gear-profile tooth-curves rotated-tooth meshed-mate
                          gear-ui demos-common:demo-ui-mixin))
   (source-pane-functions '(gear-numbers pair-numbers gear-profile-segments gear-report
                            %involute-samples %fillet-samples
                            respond-with-gear-download
                            demos-common:respond-with-cad-export))

   ;; An undercut request is a message, not a broken page: the model
   ;; signals from its numbers, and the page shows the reason.
   (mate? (plusp (the mate-teeth-control value)))

   (problem (handler-case (progn (the model numbers)
                                 (when (the mate?) (the mate numbers))
                                 nil)
              (error (e) (princ-to-string e))))

   (objects (if (the problem)
                nil
                (append (the model cad-objects)
                        (when (the mate?) (the mate cad-objects)))))

   (body
    (with-lhtml-string ()
      (:div :class "max-w-6xl mx-auto p-8"
        (str (the (page-intro "Involute Gear"
                              "An ISO 21771 spur gear as exact NURBS: involute flanks from the base circle, the root fillet the basic rack's tip generates, profile shift, and a meshing mate at the working center distance.")))
        (str (the (card :title "Parameters"
                        :content (with-lhtml-string ()
                                   (:div :class "p-6"
                                     (str (the control-form))
                                     (str (the download-buttons)))))))
        (str (the (card :title "The gear"
                        :content (with-lhtml-string ()
                                   (:div :class "p-6 relative" :style "height: 560px;"
                                     (if (the problem)
                                         (htm (:div :class "rounded-md bg-amber-50 border border-amber-200 p-4 text-sm text-amber-900"
                                                (str (the problem))))
                                         (str (the viewport-area div))))))))
        (str (the stats-section div))
        (:div :class "mt-8 text-sm text-gray-500"
          "Every result can be traced: the free "
          (:a :href "/demo/gear/cad/trace" :class "text-indigo-600 hover:text-indigo-500" "verification record")
          " states the standards this geometry follows and the checks made on every result; add trace=1 to an API request to have the report and the STEP header name it.")
        ;; The Source Code card follows the declaration's :open flag
        ;; (open for now, the user, 2026-09-11): closing the gear later
        ;; is dropping :open in publish.lisp, nothing here.
        (when (demos-common:cad-export-open? :gear)
          (htm (:div :class "mt-4"
                 (str (the (card :title "Source Code"
                                 :subtitle "The complete GDL source for this demo: object definitions reconstituted live from the in-memory definitions serving this page, supporting functions via the Lisp's own source records."
                                 :content (the source-code-panes))))))))))

   (stats-row
    (if (the problem)
        ""
        (let ((n (the model numbers)) (pair (the model pair)))
          (with-lhtml-string ()
            (:div :class "grid grid-cols-1 md:grid-cols-3 gap-6 mt-8"
              (str (the (stats-card "Diameters (mm)"
                         (list (list "Pitch" (format nil "~,4f" (getf n :pitch-diameter)))
                               (list "Base" (format nil "~,4f" (getf n :base-diameter)))
                               (list "Tip" (format nil "~,4f" (getf n :tip-diameter)))
                               (list "Root" (format nil "~,4f" (getf n :root-diameter)))
                               (list "Form" (format nil "~,4f" (* 2 (getf n :form-radius))))))))
              (str (the (stats-card "Inspection"
                         (list (list "Tooth thickness at pitch" (format nil "~,4f mm" (getf n :tooth-thickness)))
                               (list (format nil "Span over ~a teeth" (getf n :span-teeth))
                                     (format nil "~,4f mm" (getf n :span)))
                               (list "Undercut below shift" (format nil "~,3f" (getf n :shift-minimum)))
                               (list "Tooth half-angle at tip" (format nil "~,3f deg" (rad->deg (getf n :theta-tip))))))))
              (str (the (stats-card (if pair "The pair" "The pair")
                         (if pair
                             (list (list "Ratio" (format nil "~,3f" (getf pair :ratio)))
                                   (list "Working pressure angle" (format nil "~,3f deg" (getf pair :working-pressure-angle)))
                                   (list "Center distance" (format nil "~,4f mm" (getf pair :center-distance)))
                                   (list "Contact ratio" (format nil "~,3f" (getf pair :contact-ratio))))
                             (list (list "Mate" "none -- set Mate teeth above")))))))))))

   (control-form
    (with-form-string (:class "grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-6")
      (str (the (param-field "Module (mm)"      (the module-control))))
      (str (the (param-field "Teeth"            (the teeth-control))))
      (str (the (param-field "Pressure angle"   (the pressure-angle-control))))
      (str (the (param-field "Profile shift x"  (the shift-control))))
      (str (the (param-field "Face width (mm)"  (the face-width-control))))
      (str (the (param-field "Mate teeth (0 = none)" (the mate-teeth-control))))))

   ;; The free download of what is on screen (session-bound, this
   ;; page's own gear), and the paid API's URL for the same request.
   (download-buttons
    (flet ((link (label format extension)
             (with-lhtml-string ()
               (:a :href (format nil "~a?iid=~a&format=~a"
                                 *cad-download-path* (the instance-id) format)
                   :download (format nil "gear-m~a-z~a.~a"
                                     (the module-control value) (the teeth-control value) extension)
                   :class "inline-flex items-center rounded-md border border-gray-300 bg-white px-4 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50"
                   (str label)))))
      (with-lhtml-string ()
        (:div :class "flex flex-wrap items-center gap-6 mt-6 pt-6 border-t border-gray-200"
          (str (link "Download STEP" "step" "stp"))
          (str (link "Download IGES" "iges" "igs"))
          (:span :class "text-sm text-gray-500"
            "For agents and scripts, the same gear from the metered API: "
            (:code :class "text-xs text-gray-700" (str (the api-url))))))))

   (api-url
    (format nil "~a?module=~a&teeth=~a&pressure_angle=~a&shift=~a&face_width=~a~@[&mate=~a~]"
            *cad-export-path*
            (the module-control value) (the teeth-control value)
            (the pressure-angle-control value) (the shift-control value)
            (the face-width-control value)
            (and (plusp (the mate-teeth-control value)) (the mate-teeth-control value)))))

  :functions
  ((stats-card
    (heading rows)
    (with-lhtml-string ()
      (:div :class "bg-white rounded-lg shadow p-6"
        (:h4 :class "text-lg font-medium text-gray-900 mb-4" (str heading))
        (:div :class "space-y-3 text-sm"
          (dolist (row rows)
            (htm (:div :class "flex justify-between gap-4"
                   (:span :class "text-gray-600" (str (first row)))
                   (:span :class "font-medium tabular-nums" (str (second row)))))))))))

  :objects
  ((module-control :type 'number-form-control
                   :default (the module)
                   :ajax-submit-on-change? t
                   :domain :number :min 0.5 :max 20 :step 0.5)

   (teeth-control :type 'number-form-control
                  :default (the teeth)
                  :ajax-submit-on-change? t
                  :domain :number :min 6 :max 120 :step 1)

   (pressure-angle-control :type 'menu-form-control
                           :default (the pressure-angle)
                           :size 1
                           :ajax-submit-on-change? t
                           :choice-plist '(14.5 "14.5 deg" 20 "20 deg" 25 "25 deg"))

   (shift-control :type 'number-form-control
                  :default (the shift)
                  :ajax-submit-on-change? t
                  :domain :number :min -0.5 :max 1 :step 0.05)

   (face-width-control :type 'number-form-control
                       :default (the face-width)
                       :ajax-submit-on-change? t
                       :domain :number :min 1 :max 60 :step 1)

   (mate-teeth-control :type 'number-form-control
                       :default (the mate-teeth)
                       :ajax-submit-on-change? t
                       :domain :number :min 0 :max 120 :step 1)

   (model :type 'gear-profile
          :module (the module-control value)
          :teeth (the teeth-control value)
          :pressure-angle (the pressure-angle-control value)
          :shift (the shift-control value)
          :face-width (the face-width-control value)
          :mate-teeth (and (plusp (the mate-teeth-control value))
                           (the mate-teeth-control value)))

   ;; referenced only when mate? -- an unreferenced child costs nothing
   (mate :type 'meshed-mate
         :driver (the model))

   (stats-section :type 'base-html-div
                  :inner-html (the stats-row))))


(defun respond-with-gear-download (req ent)
  "Stream the session's gear (and mate) as STEP or IGES: what the page
shows, for the person looking at it.  An expired session goes back to
the demo's start page, which mints a fresh one."
  (let* ((query (net.aserve:request-query req))
         (iid (cdr (assoc "iid" query :test #'string-equal)))
         (step? (equalp (cdr (assoc "format" query :test #'string-equal)) "step"))
         (self (and iid (first (gethash (gwl::make-keyword-sensitive iid)
                                        gwl:*instance-hash-table*)))))
    (if (or (null self) (the-object self problem))
        (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
          (setf (net.aserve:reply-header-slot-value req :location) "/demo/gear")
          (net.aserve:with-http-body (req ent)))
        (let* ((filename (format nil "gear-m~a-z~a.~a"
                                 (the-object self module-control value)
                                 (the-object self teeth-control value)
                                 (if step? "stp" "igs")))
               (temp-path (namestring (glisp:temporary-file))))
          (unwind-protect
               (progn
                 (demos-common:write-cad-export-file temp-path (if step? :step :iges)
                                                     (the-object self objects))
                 (net.aserve:with-http-response
                     (req ent :content-type (if step? "model/step" "model/iges"))
                   (setf (net.aserve:reply-header-slot-value req :content-disposition)
                         (format nil "attachment; filename=~s" filename))
                   (net.aserve:with-http-body (req ent)
                     (with-open-file (in temp-path :element-type '(unsigned-byte 8))
                       (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
                             (out (net.aserve:request-reply-stream req)))
                         (loop for count = (read-sequence buffer in)
                               while (plusp count)
                               do (write-sequence buffer out :end count)))))))
            (ignore-errors (delete-file temp-path)))))))
