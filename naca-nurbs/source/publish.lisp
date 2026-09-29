;;;; -*- Mode: Lisp; Package: naca-nurbs -*-

;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.


(in-package :naca-nurbs)

;; The shared demos stylesheet (compiled by build:demos in
;; /projects/apps/tailwind) lives at <demos>/css/ and serves at
;; /demo/css/.  Every demo publishes it (idempotent) so no demo
;; depends on another being loaded.
(defparameter *demos-dir*
  (let ((base (glisp:source-pathname)))
    (make-pathname :name nil :type nil
                   :directory (butlast (pathname-directory base) 2)
                   :defaults base)))

;; *cad-download-path* ("/demo/naca-nurbs/download") is defined in
;; ui.lisp, which loads first and links to it.

;; The stateless CAD export (2026-09-11): the airfoil family built from
;; query parameters alone, no minted instance, so an API caller (or an
;; agent paying through the x402 rule in front of it) gets the file in
;; one GET.  Declared below through demos-common's export declaration,
;; which owns the handler and hands the cyclops rule its discovery
;; block; /cad takes ?format=step|iges, /cad.stp and /cad.igs fix it.
(defparameter *cad-export-path* "/demo/naca-nurbs/cad")

(defparameter *cad-export-max-family* 12
  "Most airfoils one request may put in a single file.")

;; Individual (non-shared) sessions: each visitor's first hit on
;; /demo/naca-nurbs mints a fresh instance and redirects to its
;; /sessions/... URL.  session-control-mixin on ui keeps those
;; instances mortal for the standard GWL reaper.  Deployments call
;; this with :host to scope the publish to particular virtual hosts;
;; dev.lisp calls it with no host for load-and-go use.
(defun publish-ui! (&key host)
  (with-all-servers (server)
    (publish-gwl-app "/demo/naca-nurbs" 'ui
                     :server server
                     :host host)
    (net.aserve:publish :path *cad-download-path*
                        :server server
                        :host host
                        :function 'respond-with-cad-download)
    (publish-directory :prefix "/demo/css/"
                       :server server
                       :host host
                       :destination (namestring
                                     (merge-pathnames "css/" *demos-dir*))))
  (demos-common:publish-cad-export! :naca-nurbs :host host))

(defun respond-with-cad-download (req ent)
  "Stream the session's composed airfoil curves as IGES or STEP.
The entire CAD conversion is the with-format call below: the same
declarative curve objects the viewports display write themselves out
through the requested format's lens."
  (let* ((query (net.aserve:request-query req))
         (iid (cdr (assoc "iid" query :test #'string-equal)))
         (step? (equalp (cdr (assoc "format" query :test #'string-equal)) "step"))
         (self (and iid (first (gethash (gwl::make-keyword-sensitive iid)
                                        gwl:*instance-hash-table*)))))
    (if (null self)
        ;; expired or unknown session: back to the demo start page,
        ;; which mints a fresh one.
        (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
          (setf (net.aserve:reply-header-slot-value req :location) "/demo/naca-nurbs")
          (net.aserve:with-http-body (req ent)))
        (let* ((digits (remove-if-not #'digit-char-p
                                      (string (the-object self nurbs airfoil))))
               (filename (format nil "naca-~a.~a" digits (if step? "stp" "igs")))
               (temp-path (format nil "/tmp/~a-~a" iid filename)))
          (if step?
              (with-format (step temp-path)
                (write-the-object (the-object self nurbs upper-composed) cad-output)
                (write-the-object (the-object self nurbs lower-composed) cad-output))
              (with-format (iges temp-path)
                (write-the-object (the-object self nurbs upper-composed) cad-output)
                (write-the-object (the-object self nurbs lower-composed) cad-output)))
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
                        do (write-sequence buffer out :end count))))))
          (ignore-errors (delete-file temp-path))))))


;;;; The stateless export: /demo/naca-nurbs/cad
;;;;
;;;;   ?digits=2412              one airfoil (4- or 5-digit NACA code)
;;;;   ?digits=0012,2412,23012   a FAMILY: every profile in ONE file
;;;;   &chord=250                scale (default 1.0, the unit chord)
;;;;   &points=216               samples per airfoil, 50..500
;;;;   &tolerance=0.0005         NURBS approximation tolerance
;;;;   &format=step|iges         or ask for /cad.stp, /cad.igs

(defun parse-airfoil-digits (string)
  "\"0012,2412, 23012\" -> (:|0012| :|2412| :|23012|), each code checked
against the NACA 4-/5-digit parser so an unknown series fails here,
before any geometry is built."
  (let ((codes (remove ""
                       (mapcar #'(lambda (item)
                                   (remove-if-not #'digit-char-p item))
                               (glisp:split-regexp "[, ;+]+" string))
                       :test #'string=)))
    (when (null codes)
      (error "digits must name at least one NACA airfoil, e.g. digits=2412"))
    (when (> (length codes) *cad-export-max-family*)
      (error "at most ~a airfoils per request, got ~a"
             *cad-export-max-family* (length codes)))
    (dolist (code codes)
      (unless (member (length code) '(4 5))
        (error "~a is not a NACA 4- or 5-digit code" code))
      (handler-case (get-airfoil-spec code)
        (error (e)
          (error "~a is not a supported NACA code (~a)" code e))))
    (mapcar #'(lambda (code) (intern code :keyword)) codes)))

(defun export-airfoil-curves (airfoil &key (n-points 216) (chord 1) (tolerance 0.0005))
  "The upper and lower composed NURBS curves of one NACA airfoil, unit
chord along +x from the leading edge at the origin, scaled to CHORD
about that origin when CHORD is not 1.  Plain make-object: no session,
no instance table."
  (let* ((nurbs (make-object 'naca-nurbs-curves
                             :airfoil airfoil
                             :n-points n-points
                             :approx-tolerance tolerance))
         (curves (list (the-object nurbs upper-composed)
                       (the-object nurbs lower-composed))))
    (if (= chord 1)
        curves
        (mapcar #'(lambda (curve)
                    (make-object 'boxed-curve :curve-in curve :scale chord))
                curves))))

(defun airfoil-family (spec)
  "Every airfoil's two curves, in request order."
  (loop for airfoil in (getf spec :digits)
        append (export-airfoil-curves airfoil
                                      :n-points (getf spec :points)
                                      :chord (getf spec :chord)
                                      :tolerance (getf spec :tolerance))))

(demos-common:register-cad-export! :naca-nurbs
  :path *cad-export-path*
  :description "NACA 4/5-digit airfoil family as STEP or IGES NURBS curves (digits, chord, points, tolerance)"
  :mime-type "model/step"
  :formats '(:step :iges)
  :parameters (list (list "digits" :type :string :required t :example "2412"
                          :parse #'parse-airfoil-digits
                          :description "One NACA 4- or 5-digit code, or a comma-separated family of up to twelve, e.g. 0012,2412,23012")
                    (list "chord" :type :number :default 1 :range '(1d-6 1d9) :example 1
                          :description "Scale factor applied to the unit chord; default 1")
                    (list "points" :type :integer :default 216 :range '(50 500) :example 216
                          :description "Sample points per airfoil, 50 to 500; default 216")
                    (list "tolerance" :type :number :default 0.0005 :range '(0.0001 0.01) :example "0.0005"
                          :description "NURBS approximation tolerance, 0.0001 to 0.01; default 0.0005")
                    (list "trace" :type :string :default nil :example "1"
                          :description "1 to be given the free verification record for this result (the contract, the standards, the checks); omitted, nothing extra is included"))
  :build #'airfoil-family
  ;; the verification record (free at /demo/naca-nurbs/cad/trace);
  ;; :open, so it appends the live source, as the page's panes show it
  :open t
  :sources '(generate-naca-samples get-airfoil-spec analytical-tangent-parametric
             analytical-curvature-parametric naca-nurbs-curves
             parse-airfoil-digits export-airfoil-curves airfoil-family)
  :standards '("NACA 4-digit series: camber line y_c(x) piecewise parabolic (m, p), thickness y_t(x) the NACA polynomial with the closed-trailing-edge a5 = -0.1036"
               "NACA 5-digit series: the standard camber table (210, 220, 230, 240, 250 families)"
               "Points on the surfaces perpendicular to the camber line, cosine-spaced in x; unit chord from the leading edge at the origin, scaled by chord"
               "Each surface as two NURBS pieces (nose and main) fitted through the samples with analytical tangents, then approximated to the tolerance and composed")
  :checks '("The composed curve starts at the leading edge (0, 0, 0) and ends at the trailing edge (chord, ~0, 0)"
            "Each approximated piece reports its achieved tolerance against the fitted curve"
            "The written STEP re-reads as the same two curves (verified to 1e-15 at the ends and mid-curve on the reference airfoil)")
  :filename (lambda (spec)
              (format nil "naca-~{~a~^-~}" (mapcar #'symbol-name (getf spec :digits)))))
