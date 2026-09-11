;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; The stateless export: /demo/gear/cad
;;;;
;;;;   ?module=2&teeth=20             the gear (module in mm)
;;;;   &pressure_angle=20             14.5, 20 or 25 (any value 10..30)
;;;;   &shift=0                       profile shift coefficient x
;;;;   &backlash=0                    tooth thickness reduction, mm
;;;;   &mate=40[&mate_shift=0]        a PAIR: both gears, meshed at the
;;;;                                  working center distance
;;;;   &format=step|iges|json         json = the drawing numbers only
;;;;   /cad.stp, /cad.igs, /cad.json  fix the format by name
;;;;
;;;; Every request builds its gear-profile objects, writes them through
;;;; the format lens, streams the file, and lets them go: no session,
;;;; nothing kept.  A refused parameter is a 400 with the reason --
;;;; including the undercut refusal, which names the profile shift
;;;; that fixes it.

(in-package :gear)

(defparameter *demos-dir*
  (let ((base (glisp:source-pathname)))
    (make-pathname :name nil :type nil
                   :directory (butlast (pathname-directory base) 2)
                   :defaults base)))

(defparameter *cad-export-path* "/demo/gear/cad")

(defparameter *teeth-range* '(6 . 200))
(defparameter *module-range* '(0.1 . 100))
(defparameter *pressure-angle-range* '(10 . 30))
(defparameter *shift-range* '(-1 . 1.5))

(defun publish-gear! (&key host)
  (with-all-servers (server)
    (dolist (suffix '("" ".stp" ".igs" ".json"))
      (net.aserve:publish :path (concatenate 'string *cad-export-path* suffix)
                          :server server
                          :host host
                          :function 'respond-with-gear-export))))

(defun %query-value (query name)
  (let ((value (cdr (assoc name query :test #'string-equal))))
    (and value (plusp (length (string-trim " " value)))
         (string-trim " " value))))

(defun %parse-number (string name)
  (let ((cleaned (string-trim " " string)))
    (unless (and (plusp (length cleaned))
                 (every #'(lambda (c) (or (digit-char-p c) (find c "+-.eE"))) cleaned))
      (error "~a must be a number, got ~s" name cleaned))
    (let ((value (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
                   (ignore-errors (read-from-string cleaned)))))
      (unless (realp value) (error "~a must be a number, got ~s" name cleaned))
      (float value 1d0))))

(defun %check-range (value range name)
  (unless (<= (car range) value (cdr range))
    (error "~a must be between ~a and ~a, got ~a" name (car range) (cdr range) value))
  value)

(defun %number-or (query name default range &key integer?)
  (let ((raw (%query-value query name)))
    (if (null raw)
        default
        (let ((value (%parse-number raw name)))
          (%check-range value range name)
          (if integer? (round value) value)))))

(defun parse-gear-request (query path)
  "The request as a plist (:format :module :teeth ...), or an error
naming the parameter."
  (let* ((format-param (%query-value query "format"))
         (format (cond ((null format-param)
                        (cond ((glisp:match-regexp "\\.igs$" path) :iges)
                              ((glisp:match-regexp "\\.json$" path) :json)
                              (t :step)))
                       ((string-equal format-param "step") :step)
                       ((string-equal format-param "iges") :iges)
                       ((string-equal format-param "json") :json)
                       (t (error "format must be step, iges or json, got ~s" format-param))))
         (teeth (%number-or query "teeth" nil *teeth-range* :integer? t)))
    (unless teeth (error "teeth is required, e.g. teeth=20"))
    (list :format format
          :teeth teeth
          :module (%number-or query "module" 1d0 *module-range*)
          :pressure-angle (%number-or query "pressure_angle" 20d0 *pressure-angle-range*)
          :shift (%number-or query "shift" 0d0 *shift-range*)
          :backlash (%number-or query "backlash" 0d0 '(0 . 10))
          :mate (%number-or query "mate" nil *teeth-range* :integer? t)
          :mate-shift (%number-or query "mate_shift" 0d0 *shift-range*))))

(defun gear-family (spec)
  "The gear-profile objects a request describes: the gear, and its
mate meshed at the working center distance when :mate is given."
  (let* ((gear (make-object 'gear-profile
                            :module (getf spec :module) :teeth (getf spec :teeth)
                            :pressure-angle (getf spec :pressure-angle)
                            :shift (getf spec :shift) :backlash (getf spec :backlash)
                            :mate-teeth (getf spec :mate) :mate-shift (getf spec :mate-shift)))
         (mate (when (getf spec :mate)
                 (make-object 'meshed-mate :driver gear))))
    (remove nil (list gear mate))))

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
   (outline (the placed)))
  :objects
  ((mate :type 'gear-profile
         :module (the driver module) :teeth (the driver mate-teeth)
         :pressure-angle (the driver pressure-angle)
         :shift (the driver mate-shift) :backlash (the driver backlash))
   (placed :type 'boxed-curve
           :curve-in (the mate outline)
           :orientation (the rotation)
           :orientation-center (make-point 0 0 0)
           :center (make-point (the center-distance) 0 0))))

(defun write-gear-cad-file (path format objects)
  (ecase format
    (:step (with-format (step path)
             (dolist (object objects) (write-the-object (the-object object outline) cad-output))))
    (:iges (with-format (iges path)
             (dolist (object objects) (write-the-object (the-object object outline) cad-output))))))

(defun %respond-error (req ent message)
  (net.aserve:with-http-response (req ent :response net.aserve:*response-bad-request*
                                          :content-type "text/plain")
    (net.aserve:with-http-body (req ent)
      (format (net.aserve:request-reply-stream req)
              "~a~%~%usage: ~a?module=2&teeth=20[&pressure_angle=20&shift=0&backlash=0&mate=40&mate_shift=0]&format=step|iges|json~%"
              message *cad-export-path*))))

(defun %json-report-string (objects spec)
  "The drawing numbers as JSON: the gear's report, and the mate's
under \"mate\" when there is one."
  (let ((gear (first objects)) (mate (second objects)))
    (with-output-to-string (s)
      (json:with-object (s)
        (json:encode-object-member "resource" "gear" s)
        (dolist (pair (the-object gear report))
          (json:encode-object-member (car pair) (cdr pair) s))
        (when mate
          (json:as-object-member ("mate" s)
            (json:with-object (s)
              (dolist (pair (the-object mate report))
                (json:encode-object-member (car pair) (cdr pair) s)))))
        (json:encode-object-member "units" (if (getf spec :module) "mm" "mm") s)))))

(defun respond-with-gear-export (req ent)
  "GET handler: parse and check, build the family, stream the file or
the report; a bad parameter is a 400 with the reason."
  (let* ((query (net.aserve:request-query req))
         (path (net.uri:uri-path (net.aserve:request-uri req))))
    (multiple-value-bind (spec problem)
        (ignore-errors (parse-gear-request query path))
      (if (null spec)
          (%respond-error req ent (princ-to-string problem))
          (multiple-value-bind (objects build-problem)
              (ignore-errors (let ((objects (gear-family spec)))
                               ;; force the numbers now so an undercut
                               ;; refusal is a 400, not a broken file
                               (dolist (o objects) (the-object o numbers))
                               objects))
            (if (null objects)
                (%respond-error req ent (princ-to-string build-problem))
                (let* ((format (getf spec :format))
                       (stem (format nil "gear-m~a-z~a~@[-z~a~]"
                                     (getf spec :module) (getf spec :teeth) (getf spec :mate))))
                  (if (eq format :json)
                      (net.aserve:with-http-response (req ent :content-type "application/json")
                        (net.aserve:with-http-body (req ent)
                          (write-string (%json-report-string objects spec)
                                        (net.aserve:request-reply-stream req))))
                      (let* ((extension (ecase format (:step "stp") (:iges "igs")))
                             (filename (format nil "~a.~a" stem extension))
                             (temp-path (namestring (glisp:temporary-file))))
                        (unwind-protect
                             (progn
                               (write-gear-cad-file temp-path format objects)
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
                          (ignore-errors (delete-file temp-path))))))))))))
