;;;; -*- Mode: Lisp; Package: picnic-table -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :picnic-table)

;; The shared demos stylesheet lives at <demos>/css/ and serves at /demo/css/.
(defparameter *demos-dir*
  (let ((base (glisp:source-pathname)))
    (make-pathname :name nil :type nil
                   :directory (butlast (pathname-directory base) 2)
                   :defaults base)))

(defparameter *drawing-formats*
  '(("pdf" :pdf "application/pdf")
    ("svg" :svg "image/svg+xml")
    ("dxf" :dxf "application/dxf"))
  "Each drawing format as (extension format content-type).")

(defun respond-with-drawing (req ent)
  "The drawing door, /demo/picnic-table/drawing.{pdf,svg,dxf}: builds
the table the query names (parse-drawing-query), draws its sheet into
a temporary file and sends it, inline or, with download=1, as an
attachment.  Nothing is kept between requests."
  (let* ((path (net.uri:uri-path (net.aserve:request-uri req)))
         (format (find (pathname-type path) *drawing-formats* :key #'first :test #'string-equal))
         (query (net.aserve:request-query req)))
    (multiple-value-bind (inputs hidden-lines) (parse-drawing-query query)
      (if (or (null format) (null inputs))
          (net.aserve:with-http-response (req ent :response net.aserve:*response-bad-request*
                                                  :content-type "text/plain")
            (net.aserve:with-http-body (req ent)
              (format (net.aserve:request-reply-stream req) "~a~%"
                      (if format hidden-lines "Ask for drawing.pdf, drawing.svg or drawing.dxf"))))
          (let ((temp-path (namestring (glisp:temporary-file)))
                (model (apply #'make-object 'picnic-table :colored? nil inputs)))
            (unwind-protect
                 (progn
                   (write-drawing model :format (second format) :hidden-lines hidden-lines
                                        :file temp-path)
                   (net.aserve:with-http-response (req ent :content-type (third format))
                     (setf (net.aserve:reply-header-slot-value req :content-disposition)
                           (format nil "~:[inline~;attachment~]; filename=\"picnic-table-~ax~ax~a.~a\""
                                   (equal (cdr (assoc "download" query :test #'string-equal)) "1")
                                   (round (the-object model table-length))
                                   (round (the-object model overall-depth))
                                   (round (the-object model top-height))
                                   (first format)))
                     (net.aserve:with-http-body (req ent)
                       (with-open-file (in temp-path :element-type '(unsigned-byte 8))
                         (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
                               (out (net.aserve:request-reply-stream req)))
                           (loop for count = (read-sequence buffer in)
                                 while (plusp count)
                                 do (write-sequence buffer out :end count)))))))
              (ignore-errors (delete-file temp-path))))))))

;; Individual (non-shared) sessions: each visitor's first hit on
;; /demo/picnic-table mints a fresh instance; session-control-mixin on
;; picnic-table-ui keeps it mortal for the standard GWL reaper.
(defun publish-picnic-table! (&key host)
  (gwl:with-all-servers (server)
    (gwl:publish-gwl-app "/demo/picnic-table" 'picnic-table-ui
                         :server server
                         :host host)
    (dolist (format *drawing-formats*)
      (net.aserve:publish :path (format nil "/demo/picnic-table/drawing.~a" (first format))
                          :server server :host host
                          :function 'respond-with-drawing))
    (publish-directory :prefix "/demo/css/"
                       :server server
                       :host host
                       :destination (namestring
                                     (merge-pathnames "css/" *demos-dir*)))))
