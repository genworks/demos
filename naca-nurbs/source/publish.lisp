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

(defparameter *cad-download-path* "/demo/naca-nurbs/download")

;; The stateless CAD export (2026-09-11): the airfoil family built from
;; query parameters alone, no minted instance, so an API caller (or an
;; agent paying through the x402 rule in front of it) gets the file in
;; one GET.  /cad takes ?format=step|iges; /cad.stp and /cad.igs fix
;; the format by name.
(defparameter *cad-export-path* "/demo/naca-nurbs/cad")

(defparameter *cad-export-max-family* 12
  "Most airfoils one request may put in a single file.")
(defparameter *cad-export-points-range* '(50 . 500)
  "Allowed sample-point counts per airfoil (the UI's own range).")
(defparameter *cad-export-tolerance-range* '(0.0001 . 0.01)
  "Allowed approximation tolerances (the UI's own range).")

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
    (dolist (suffix '("" ".stp" ".igs"))
      (net.aserve:publish :path (concatenate 'string *cad-export-path* suffix)
                          :server server
                          :host host
                          :function 'respond-with-cad-export))
    (publish-directory :prefix "/demo/css/"
                       :server server
                       :host host
                       :destination (namestring
                                     (merge-pathnames "css/" *demos-dir*)))))

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
;;;;
;;;; Nothing is looked up and nothing is kept: each request builds
;;;; its naca-nurbs-curves objects, writes them through the format
;;;; lens, streams the file, and lets the objects go.

(defun %export-query-value (query name)
  (let ((value (cdr (assoc name query :test #'string-equal))))
    (and value (plusp (length (string-trim " " value)))
         (string-trim " " value))))

(defun %export-parse-number (string name)
  "A plain decimal number from a query parameter, or an error naming it."
  (let ((cleaned (string-trim " " string)))
    (unless (and (plusp (length cleaned))
                 (every #'(lambda (c) (or (digit-char-p c) (find c "+-.eE"))) cleaned))
      (error "~a must be a number, got ~s" name cleaned))
    (let ((value (let ((*read-eval* nil)
                       (*read-default-float-format* 'double-float))
                   (ignore-errors (read-from-string cleaned)))))
      (unless (realp value)
        (error "~a must be a number, got ~s" name cleaned))
      value)))

(defun %export-parse-digits (string)
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

(defun %export-check-range (value range name)
  (unless (<= (car range) value (cdr range))
    (error "~a must be between ~a and ~a, got ~a" name (car range) (cdr range) value))
  value)

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

(defun write-airfoil-cad-file (path format curves)
  "Write CURVES to PATH through the STEP or IGES lens -- the same
with-format call the session download makes, one entity per curve, all
of them in the one file."
  (ecase format
    (:step (with-format (step path)
             (dolist (curve curves) (write-the-object curve cad-output))))
    (:iges (with-format (iges path)
             (dolist (curve curves) (write-the-object curve cad-output))))))

(defun %export-respond-error (req ent message)
  (net.aserve:with-http-response (req ent :response net.aserve:*response-bad-request*
                                          :content-type "text/plain")
    (net.aserve:with-http-body (req ent)
      (format (net.aserve:request-reply-stream req)
              "~a~%~%usage: ~a?digits=2412[,0012,...]&chord=1&points=216&tolerance=0.0005&format=step|iges~%"
              message *cad-export-path*))))

(defun respond-with-cad-export (req ent)
  "GET handler for the stateless export: parse and check the query,
build the family, stream it as an attachment.  A bad parameter is a
400 with the reason in plain text; nothing about the request survives
the response."
  (let* ((query (net.aserve:request-query req))
         (path (net.uri:uri-path (net.aserve:request-uri req)))
         (format-param (%export-query-value query "format")))
    (multiple-value-bind (spec problem)
        (ignore-errors
          (let* ((format (cond ((and format-param (string-equal format-param "iges")) :iges)
                               ((and format-param (string-equal format-param "step")) :step)
                               (format-param (error "format must be step or iges, got ~s" format-param))
                               ((glisp:match-regexp "\\.igs$" path) :iges)
                               (t :step)))
                 (airfoils (%export-parse-digits
                            (or (%export-query-value query "digits")
                                (error "digits is required, e.g. digits=2412"))))
                 (chord (let ((c (%export-query-value query "chord")))
                          (if c (%export-parse-number c "chord") 1)))
                 (points (let ((p (%export-query-value query "points")))
                           (if p (round (%export-parse-number p "points")) 216)))
                 (tolerance (let ((tol (%export-query-value query "tolerance")))
                              (if tol (%export-parse-number tol "tolerance") 0.0005))))
            (unless (plusp chord) (error "chord must be positive, got ~a" chord))
            (%export-check-range points *cad-export-points-range* "points")
            (%export-check-range tolerance *cad-export-tolerance-range* "tolerance")
            (list :format format :airfoils airfoils :chord chord
                  :points points :tolerance tolerance)))
      (if (null spec)
          (%export-respond-error req ent (princ-to-string problem))
          (destructuring-bind (&key format airfoils chord points tolerance) spec
            (let* ((extension (ecase format (:step "stp") (:iges "igs")))
                   (filename (format nil "naca-~{~a~^-~}.~a"
                                     (mapcar #'symbol-name airfoils) extension))
                   (temp-path (namestring (glisp:temporary-file))))
              (unwind-protect
                   (progn
                     (write-airfoil-cad-file
                      temp-path format
                      (loop for airfoil in airfoils
                            append (export-airfoil-curves airfoil
                                                          :n-points points
                                                          :chord chord
                                                          :tolerance tolerance)))
                     (net.aserve:with-http-response
                         (req ent :content-type (ecase format
                                                  (:step "model/step")
                                                  (:iges "model/iges")))
                       (setf (net.aserve:reply-header-slot-value req :content-disposition)
                             (format nil "attachment; filename=~s" filename))
                       (net.aserve:with-http-body (req ent)
                         (with-open-file (in temp-path :element-type '(unsigned-byte 8))
                           (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
                                 (out (net.aserve:request-reply-stream req)))
                             (loop for count = (read-sequence buffer in)
                                   while (plusp count)
                                   do (write-sequence buffer out :end count)))))))
                (ignore-errors (delete-file temp-path)))))))))
