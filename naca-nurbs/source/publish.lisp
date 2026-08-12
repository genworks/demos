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
