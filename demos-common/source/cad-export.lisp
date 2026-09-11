;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; The export declaration: one registration per demo says what its
;;;; stateless CAD endpoint takes and builds, and everything else --
;;;; the aserve handlers for /demo/<slug>/cad and its .stp/.igs/.json
;;;; twins, the query parsing with ranges and defaults, the 400 with
;;;; the reason and the usage line, the temp-file streaming through
;;;; the STEP/IGES lens, the JSON report -- is here once.  The same
;;;; declaration answers for the cyclops :x402 rule's :discovery block
;;;; (cad-export-discovery), so what the catalog advertises is what the
;;;; handler checks.
;;;;
;;;;   (register-cad-export! :gear
;;;;     :path "/demo/gear/cad"
;;;;     :description "..."
;;;;     :parameters '(("teeth" :type :integer :required t :range (6 200)
;;;;                    :example 20 :description "...")
;;;;                   ("module" :type :number :default 1 :range (0.1 100) ...))
;;;;     :build (lambda (spec) ...objects...)      ; spec: plist by keyword
;;;;     :leaves (lambda (objects) ...)            ; what write-the-object writes
;;;;     :report (lambda (objects spec) alist)     ; enables format=json
;;;;     :filename (lambda (spec) "gear-m2-z20"))
;;;;
;;;; Parameter query names use underscores ("pressure_angle"); the spec
;;;; plist keys are the same names as keywords with hyphens
;;;; (:pressure-angle).

(in-package :demos-common)

(defvar *cad-exports* nil
  "name -> plist, one per registered export.")

(defparameter *cad-export-suffixes*
  '((".stp" . :step) (".igs" . :iges) (".json" . :json))
  "The path twins that fix the format by name.")

(defun find-cad-export (name)
  (cdr (assoc name *cad-exports*)))

(defun register-cad-export! (name &key path description (mime-type "model/step")
                                     (formats '(:step :iges)) parameters build
                                     (leaves #'identity) report filename)
  "Declare (or redeclare) the export NAME.  See the file header."
  (let ((plist (list :name name :path path :description description
                     :mime-type mime-type
                     :formats (if (and report (not (member :json formats)))
                                  (append formats (list :json))
                                  formats)
                     :parameters parameters :build build :leaves leaves
                     :report report :filename filename)))
    (setf *cad-exports* (cons (cons name plist)
                              (remove name *cad-exports* :key #'car)))
    name))

(defun publish-cad-export! (name &key host)
  "Publish NAME's endpoint and its format twins on every server."
  (let ((export (find-cad-export name)))
    (unless export (error "No CAD export registered as ~s" name))
    (with-all-servers (server)
      (net.aserve:publish :path (getf export :path)
                          :server server :host host
                          :function 'respond-with-cad-export)
      (dolist (suffix *cad-export-suffixes*)
        (when (member (cdr suffix) (getf export :formats))
          (net.aserve:publish :path (concatenate 'string (getf export :path) (car suffix))
                              :server server :host host
                              :function 'respond-with-cad-export))))
    name))

;;; --- parsing -----------------------------------------------------------

(defun %param-keyword (query-name)
  "\"pressure_angle\" -> the keyword source code spells :pressure-angle.
Read, not interned: the readtable case decides the symbol's name, so
the same declaration works on a modern-mode (case-preserving) Allegro
and on an upcasing Lisp alike."
  (let ((*package* (find-package :keyword))
        (*read-eval* nil))
    (read-from-string (concatenate 'string ":" (substitute #\- #\_ query-name)))))

(defun %export-query-value (query name)
  (let ((value (cdr (assoc name query :test #'string-equal))))
    (and value (plusp (length (string-trim " " value)))
         (string-trim " " value))))

(defun %parse-export-number (string name)
  (let ((cleaned (string-trim " " string)))
    (unless (and (plusp (length cleaned))
                 (every #'(lambda (c) (or (digit-char-p c) (find c "+-.eE"))) cleaned))
      (error "~a must be a number, got ~s" name cleaned))
    (let ((value (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
                   (ignore-errors (read-from-string cleaned)))))
      (unless (realp value) (error "~a must be a number, got ~s" name cleaned))
      value)))

(defun parse-cad-export-parameter (definition query)
  "One parameter's value from QUERY per its DEFINITION (name &key
type default required range parse choices), or an error naming it."
  (destructuring-bind (name &key (type :string) default required range parse choices
                       &allow-other-keys)
      definition
    (let ((raw (%export-query-value query name)))
      (cond
        ((null raw)
         (when required (error "~a is required" name))
         default)
        (parse (funcall parse raw))
        (t
         (let ((value
                 (ecase type
                   (:integer (round (%parse-export-number raw name)))
                   (:number (float (%parse-export-number raw name) 1d0))
                   (:string raw))))
           (when (and range (realp value)
                      (not (<= (first range) value (second range))))
             (error "~a must be between ~a and ~a, got ~a" name (first range) (second range) value))
           (when (and choices (not (member value choices :test #'string-equal)))
             (error "~a must be one of ~{~a~^, ~}, got ~a" name choices value))
           value))))))

(defun parse-cad-export-request (export query path)
  "(values spec nil) -- the request as a plist, :format included --
or (values nil message)."
  (handler-case
      (let* ((format-param (%export-query-value query "format"))
             (format (cond ((null format-param)
                            (or (loop for (suffix . fmt) in *cad-export-suffixes*
                                      when (glisp:match-regexp (format nil "\\~a$" suffix) path)
                                        return fmt)
                                (first (getf export :formats))))
                           (t (or (find format-param '(:step :iges :json)
                                        :test #'string-equal :key #'symbol-name)
                                  (error "format must be one of ~{~(~a~)~^, ~}, got ~s"
                                         (getf export :formats) format-param))))))
        (unless (member format (getf export :formats))
          (error "format ~(~a~) is not offered here; use ~{~(~a~)~^, ~}" format (getf export :formats)))
        (values
         (append (list :format format)
                 (loop for definition in (getf export :parameters)
                       append (list (%param-keyword (first definition))
                                    (parse-cad-export-parameter definition query))))
         nil))
    (error (e) (values nil (princ-to-string e)))))

(defun cad-export-usage (export)
  (format nil "usage: ~a?~{~a~^&~}&format=~{~(~a~)~^|~}"
          (getf export :path)
          (loop for definition in (getf export :parameters)
                collect (destructuring-bind (name &key example required default &allow-other-keys) definition
                          (format nil "~:[[~;~]~a=~a~:[]~;~]" required name (or example default "...") required)))
          (getf export :formats)))

;;; --- the handler -------------------------------------------------------

(defun %cad-export-for-path (path)
  "The registered export whose path, or one of its twins, is PATH."
  (loop for (nil . export) in *cad-exports*
        for base = (getf export :path)
        when (or (string= path base)
                 (loop for (suffix) in *cad-export-suffixes*
                       thereis (string= path (concatenate 'string base suffix))))
          return export))

(defun %encode-report (value stream)
  "An alist of string keys -> object; a list -> array; else scalar."
  (cond ((and (consp value) (consp (car value)) (stringp (caar value)))
         (json:with-object (stream)
           (dolist (pair value)
             (json:as-object-member ((car pair) stream)
               (%encode-report (cdr pair) stream)))))
        ((and (consp value) (listp (cdr value)))
         (json:with-array (stream)
           (dolist (item value)
             (json:as-array-member (stream) (%encode-report item stream)))))
        (t (json:encode-json value stream))))

(defun %respond-cad-export-error (req ent export message)
  (net.aserve:with-http-response (req ent :response net.aserve:*response-bad-request*
                                          :content-type "text/plain")
    (net.aserve:with-http-body (req ent)
      (format (net.aserve:request-reply-stream req) "~a~%~%~a~%" message (cad-export-usage export)))))

(defun %stream-file (req ent path content-type filename)
  (net.aserve:with-http-response (req ent :content-type content-type)
    (setf (net.aserve:reply-header-slot-value req :content-disposition)
          (format nil "attachment; filename=~s" filename))
    (net.aserve:with-http-body (req ent)
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
              (out (net.aserve:request-reply-stream req)))
          (loop for count = (read-sequence buffer in)
                while (plusp count)
                do (write-sequence buffer out :end count)))))))

(defun write-cad-export-file (path format leaves)
  "LEAVES through the STEP or IGES lens into PATH, one file."
  (ecase format
    (:step (with-format (step path) (dolist (leaf leaves) (write-the-object leaf cad-output))))
    (:iges (with-format (iges path) (dolist (leaf leaves) (write-the-object leaf cad-output))))))

(defun respond-with-cad-export (req ent)
  "The one GET handler behind every registered export."
  (let* ((path (net.uri:uri-path (net.aserve:request-uri req)))
         (export (%cad-export-for-path path))
         (query (net.aserve:request-query req)))
    (if (null export)
        (net.aserve:with-http-response (req ent :response net.aserve:*response-not-found*)
          (net.aserve:with-http-body (req ent)))
        (multiple-value-bind (spec problem) (parse-cad-export-request export query path)
          (if (null spec)
              (%respond-cad-export-error req ent export problem)
              (multiple-value-bind (objects build-problem)
                  (handler-case (values (funcall (getf export :build) spec) nil)
                    (error (e) (values nil (princ-to-string e))))
                (if (null objects)
                    (%respond-cad-export-error req ent export build-problem)
                    (let ((format (getf spec :format))
                          (stem (if (getf export :filename)
                                    (funcall (getf export :filename) spec)
                                    (string-downcase (symbol-name (getf export :name))))))
                      (if (eq format :json)
                          (let ((report (funcall (getf export :report) objects spec)))
                            (net.aserve:with-http-response (req ent :content-type "application/json")
                              (net.aserve:with-http-body (req ent)
                                (%encode-report report (net.aserve:request-reply-stream req)))))
                          (let ((temp-path (namestring (glisp:temporary-file))))
                            (unwind-protect
                                 (progn
                                   (write-cad-export-file temp-path format
                                                          (funcall (getf export :leaves) objects))
                                   (%stream-file req ent temp-path
                                                 (ecase format (:step "model/step") (:iges "model/iges"))
                                                 (format nil "~a.~a" stem (ecase format (:step "stp") (:iges "igs")))))
                              (ignore-errors (delete-file temp-path)))))))))))))

;;; --- the catalog side ----------------------------------------------------

(defun cad-export-discovery (name)
  "The cyclops :discovery plist for NAME's rule -- the x402 bazaar
block's inputs, from the same parameter definitions the handler
checks.  Print it into the stack's cyclops config."
  (let ((export (find-cad-export name)))
    (list :method "GET"
          :query (loop for definition in (getf export :parameters)
                       collect (destructuring-bind (query-name &key (type :string) example default description &allow-other-keys)
                                   definition
                                 (list query-name
                                       :type (ecase type (:integer "integer") (:number "number") (:string "string"))
                                       :example (princ-to-string (or example default ""))
                                       :description (or description ""))))
          :required (loop for definition in (getf export :parameters)
                          when (getf (rest definition) :required)
                            collect (first definition)))))
