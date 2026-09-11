;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; The stateless export, declared: /demo/gear/cad (+ .stp .igs .json)
;;;;
;;;;   ?module=2&teeth=20             the gear (module in mm)
;;;;   &pressure_angle=20             10..30 degrees
;;;;   &shift=0                       profile shift coefficient x
;;;;   &backlash=0                    tooth thickness reduction, mm
;;;;   &face_width=10                 a SOLID (extruded) instead of the outline
;;;;   &mate=40[&mate_shift=0]        a PAIR, meshed at the working center distance
;;;;   &format=step|iges|json         json = the drawing numbers only
;;;;
;;;; The declaration below is the whole endpoint: demos-common's
;;;; register-cad-export! owns the handler, the parsing, the file
;;;; streaming and the JSON report, and hands the same parameter
;;;; definitions to the cyclops :x402 rule's :discovery block.

(in-package :gear)

(defparameter *cad-export-path* "/demo/gear/cad")

(define-object meshed-mate (base-object)
  :documentation (:description "The driver's mate, built as its own
gear-profile and placed on the +x axis at the working center
distance, turned so a tooth space faces the driver's tooth.")
  :input-slots (driver)
  :computed-slots
  ((numbers (the mate numbers))
   (report (the mate report))
   (center-distance (getf (the driver pair) :center-distance))
   ;; a gear with a tooth on +x has a tooth on -x when its tooth count
   ;; is even; the mate needs a SPACE facing the driver
   (turn (if (evenp (the mate teeth)) (/ (the mate pitch-angle) 2) 0d0))
   (rotation (alignment :rear (rotate-vector-d (make-vector 0 1 0)
                                               (rad->deg (the turn))
                                               (make-vector 0 0 1))
                        :top (make-vector 0 0 1)))
   (outline (the placed))
   (cad-objects (if (the driver face-width) (list (the solid)) (list (the placed)))))
  :objects
  ((mate :type 'gear-profile
         :module (the driver module) :teeth (the driver mate-teeth)
         :pressure-angle (the driver pressure-angle)
         :shift (the driver mate-shift) :backlash (the driver backlash))
   (placed :type 'boxed-curve
           :curve-in (the mate outline)
           :orientation (the rotation)
           :orientation-center (make-point 0 0 0)
           :center (make-point (the center-distance) 0 0)))
  :hidden-objects
  ((solid :type 'extruded-solid
          :display-controls (list :color :goldenrod)
          :profile (the placed)
          :axis-vector (make-vector 0 0 1)
          :distance (or (the driver face-width) 1))))

(defun gear-family (spec)
  "The objects a request describes: the gear, and its mate meshed at
the working center distance when :mate is given.  Forces the numbers
so an undercut refusal surfaces here, as a 400, not in the file."
  (let* ((gear (make-object 'gear-profile
                            :module (getf spec :module) :teeth (getf spec :teeth)
                            :pressure-angle (getf spec :pressure-angle)
                            :shift (getf spec :shift) :backlash (getf spec :backlash)
                            :mate-teeth (getf spec :mate) :mate-shift (getf spec :mate-shift)
                            :face-width (getf spec :face-width)))
         (mate (when (getf spec :mate) (make-object 'meshed-mate :driver gear)))
         (objects (remove nil (list gear mate))))
    (dolist (object objects objects)
      (the-object object numbers))))

(defun gear-family-report (objects spec)
  "The drawing numbers: the gear's, the mate's under \"mate\"."
  (declare (ignore spec))
  (append '(("resource" . "gear") ("units" . "mm"))
          (the-object (first objects) report)
          (when (second objects)
            (list (cons "mate" (the-object (second objects) report))))))

(demos-common:register-cad-export! :gear
  :path *cad-export-path*
  :description "Involute spur gear as exact NURBS: the outline as STEP or IGES curves, a solid when a face width is given, a meshing pair at the working center distance, and the drawing numbers as JSON (module, teeth, pressure angle, profile shift, backlash)"
  :mime-type "model/step"
  :formats '(:step :iges :json)
  :parameters '(("teeth" :type :integer :required t :range (6 200) :example 20
                 :description "Number of teeth, 6 to 200")
                ("module" :type :number :default 1d0 :range (0.1 100) :example "2"
                 :description "Module in mm (pitch diameter / teeth); default 1")
                ("pressure_angle" :type :number :default 20d0 :range (10 30) :example "20"
                 :description "Pressure angle in degrees: 14.5, 20 or 25 are the standards; default 20")
                ("shift" :type :number :default 0d0 :range (-1 1.5) :example 0
                 :description "Profile shift coefficient x; a small tooth count needs a positive shift, and the refusal says how much")
                ("backlash" :type :number :default 0d0 :range (0 10) :example 0
                 :description "Tooth thickness reduction at the pitch circle, mm; default 0")
                ("face_width" :type :number :default nil :range (0.01 1000) :example "10"
                 :description "Face width in mm: when given, the file holds the extruded solid instead of the planar outline")
                ("mate" :type :integer :default nil :range (6 200) :example 40
                 :description "Tooth count of a mating gear: when given, the file holds both gears meshed at the working center distance and the report adds the pair's numbers")
                ("mate_shift" :type :number :default 0d0 :range (-1 1.5) :example 0
                 :description "The mate's profile shift coefficient; default 0")
                ("trace" :type :string :default nil :example "1"
                 :description "1 to be given the free verification record for this result (the contract, the standards, the checks, the numbers); omitted, nothing extra is included"))
  :build #'gear-family
  :leaves (lambda (objects) (loop for object in objects append (the-object object cad-objects)))
  :report #'gear-family-report
  ;; the verification record (free at /demo/gear/cad/trace): what the
  ;; geometry follows and what is checked.  OPEN for now (the user,
  ;; 2026-09-11: 'leave them open, consider closing the gear later if
  ;; we develop it more sophisticatedly'): the record appends the live
  ;; source and the page shows its Source Code card.  Closing it is
  ;; dropping :open and the panes in ui.lisp.
  :open t
  :sources '(gear-numbers pair-numbers gear-profile-segments gear-report
             %involute-samples %fillet-samples involute-function undercut-shift-minimum
             gear-profile tooth-curves rotated-tooth meshed-mate gear-family)
  :standards '("ISO 21771:2007 -- gears: cylindrical involute gears and gear pairs, concepts and geometry"
               "ISO 53:1998 -- cylindrical gears for general engineering: standard basic rack tooth profile (addendum 1 m, dedendum 1.25 m, root fillet 0.38 m)"
               "Tooth flank: the involute of the base circle r_b = r cos(alpha), from the form radius to the tip circle"
               "Root fillet: the trochoid generated by the basic rack's rounded tip rolling on the pitch circle (not a tangent arc)"
               "Tooth thickness at the pitch circle: s = m (pi/2 + 2 x tan(alpha)) - backlash"
               "Span measurement over k teeth: W_k = m cos(alpha) (pi (k - 1/2) + z inv(alpha) + 2 x tan(alpha))"
               "Pair: inv(alpha_w) = inv(alpha) + 2 (x1 + x2) tan(alpha) / (z1 + z2); center distance a = (z1 + z2) m cos(alpha) / (2 cos(alpha_w))")
  :checks '("Undercut refused when x < 1 - z sin^2(alpha) / 2 (the reply names the minimum shift)"
            "Every junction between fillet, flank, tip land and root land closes (1e-12 mm on the reference gear)"
            "The fillet meets the flank tangentially; the outline is a closed loop"
            "The solid is one closed shell of 2 + 6 z faces, re-read from the written STEP"
            "Reference gear m = 2, z = 20: span over 3 teeth 15.3209 mm, tooth thickness 3.1416 mm, against published tables")
  :filename (lambda (spec)
              (format nil "gear-m~a-z~a~@[-z~a~]~@[-b~a~]"
                      (getf spec :module) (getf spec :teeth) (getf spec :mate) (getf spec :face-width))))

;; The shared demos stylesheet lives at <demos>/css/ and serves at
;; /demo/css/; every demo publishes it (idempotent).
(defparameter *demos-dir*
  (let ((base (glisp:source-pathname)))
    (make-pathname :name nil :type nil
                   :directory (butlast (pathname-directory base) 2)
                   :defaults base)))

(defun publish-gear! (&key host)
  "The page (non-shared sessions, mortal through session-control-mixin),
its free session-bound download, the stylesheet, and the declared
stateless export."
  (demos-common:register-portal-demo! "gear" "Involute Gear")
  (with-all-servers (server)
    (publish-gwl-app "/demo/gear" 'gear-ui :server server :host host)
    (net.aserve:publish :path *cad-download-path*
                        :server server :host host
                        :function 'respond-with-gear-download)
    (publish-directory :prefix "/demo/css/"
                       :server server :host host
                       :destination (namestring (merge-pathnames "css/" *demos-dir*))))
  (demos-common:publish-cad-export! :gear :host host))
