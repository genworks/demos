;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The agent's tools.  Each works on one session: its package and its
;; model file.  run-tool returns two values: a list of content blocks in
;; the Claude Messages API's tool_result shape (text and image blocks as
;; alists, ready for cl-json), and true when the call failed.
;;
;; The model is always the object named MODEL in the session package,
;; built with (make-object 'model) and no arguments.
;;

(defmacro with-time-limit ((seconds what) &body body)
  "Run BODY, stopping it after SECONDS with an ordinary error naming WHAT,
so the tool's own error handling reports it like any other failure."
  `(handler-case (bt:with-timeout (,seconds) ,@body)
     (bt:timeout ()
       (error "The ~a took longer than ~a seconds and was stopped." ,what ,seconds))))

(defun clip (string)
  (if (> (length string) *result-limit*)
      (format nil "~a~%[... ~a more characters cut]"
              (subseq string 0 *result-limit*) (- (length string) *result-limit*))
      string))

(defun text-result (control &rest args)
  `(("type" . "text") ("text" . ,(clip (apply #'format nil control args)))))

(defun image-block (base64)
  `(("type" . "image")
    ("source" . (("type" . "base64") ("media_type" . "image/png") ("data" . ,base64)))))

(defun image-case (string) (gendl-lisply::lisply-image-case string))

(defun model-symbol (session)
  (find-symbol (image-case "MODEL") (session-package session)))

(defun make-model (session)
  (let ((symbol (model-symbol session)))
    (unless (and symbol (find-class symbol nil))
      (error "No MODEL is defined yet -- write one with write_model."))
    (make-object symbol)))


;;
;; write_model and read_model: the model file is the source of truth, so
;; the visitor's editor and the agent see the same code.
;;

(defun write-model (session source)
  (when (search "(in-package" source :test #'char-equal)
    (return-from write-model
      (values (list (text-result "Refused: do not put an in-package form in the source. ~
The file's header already sets the session package.")) t)))
  (let ((file (session-model-file session))
        (package-name (session-package-name session)))
    (with-open-file (out file :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (format out ";; Prompt lab session ~a -- the model file.~%~
;; Written by the modeling agent; edit it freely.  The object named MODEL~%~
;; is what (make-object 'model) builds and the viewer shows.~%~%~
(in-package ~s)~%~%~a~%"
              (session-id session) package-name source))
    (load-model-file session)))

(defun load-model-file (session)
  "Compile and load the session's model file.  Values: content blocks and
an error flag.  The file as it is now goes to the archive first, so a
version that fails to compile is kept too."
  (let ((file (session-model-file session))
        (warnings nil))
    (archive-model! session)
    ;; a compile costs credits by the volume of the file (meter.lisp)
    (multiple-value-bind (ok? reason)
        (meter! session :compile
                (or (ignore-errors (symbol-volume (uiop:read-file-string file :external-format :utf-8)
                                                  (session-package session)))
                    0))
      (unless ok? (return-from load-model-file (meter-refusal reason))))
    (handler-case
        (with-time-limit (*load-seconds* "compile and load")
          (let ((fasl (handler-bind ((warning #'(lambda (w)
                                                  (push (princ-to-string w) warnings)
                                                  (muffle-warning w))))
                        (let ((*package* (session-package session)))
                          (compile-file file :output-file (make-pathname :type "fasl"
                                                                         :defaults file))))))
            (unless fasl (error "The compiler produced no output file."))
            (let ((*package* (session-package session)))
              (load fasl))
            (let ((defined? (let ((symbol (model-symbol session)))
                              (and symbol (find-class symbol nil) t))))
              (values (list (text-result "Compiled and loaded.~:[  No object named MODEL is defined!~;~]~@[~%Warnings:~%~{- ~a~%~}~]"
                                        defined? (reverse warnings)))
                      (not defined?)))))
      (error (condition)
        (values (list (text-result "Error compiling or loading the model file: ~a~@[~%Warnings first:~%~{- ~a~%~}~]"
                                  condition (reverse warnings)))
                t)))))

(defun read-model (session)
  (let ((file (session-model-file session)))
    (if (probe-file file)
        (values (list (text-result "~a" (uiop:read-file-string file :external-format :utf-8))) nil)
        (values (list (text-result "There is no model file yet.")) nil))))


;;
;; evaluate: one expression, as at a REPL.
;;

;; (Named evaluate-expression, and the tool's text blocks text-result:
;; the symbols evaluate and text-block are exported by gendl and
;; geom-base, which Allegro locks -- a defun on either fails to load on
;; the workshop, and silently redefines them on CCL.)
(defun evaluate-expression (session expression)
  (let ((*package* (session-package session)))
    (multiple-value-bind (form end)
        (handler-case (read-from-string expression)
          (error (condition)
            (return-from evaluate-expression
              (values (list (text-result "Could not read the expression: ~a" condition)) t))))
      (when (let ((rest (string-trim '(#\space #\tab #\newline #\return)
                                     (subseq expression end))))
              (plusp (length rest)))
        (return-from evaluate-expression
          (values (list (text-result "Refused: one expression per call, as at a REPL. ~
Wrap several forms in progn, or put definitions in the model with write_model.")) t)))
      ;; a run costs credits by the volume of what runs (meter.lisp)
      (multiple-value-bind (ok? reason) (meter! session :run (form-volume form))
        (unless ok? (return-from evaluate-expression (meter-refusal reason))))
      (let (result)
        (handler-case
            (let ((output (with-output-to-string (*standard-output*)
                            (with-time-limit (*eval-seconds* "evaluation")
                              (setq result (eval form))))))
              (values (list (let ((*print-length* 50) (*print-level* 6))
                              (text-result "~s~@[~%Output:~%~a~]"
                                          result (when (plusp (length output)) output))))
                      nil))
          (error (condition)
            (values (list (text-result "Error: ~a" condition)) t)))))))


;;
;; check_model: numbers the harness computes itself, so the agent's own
;; opinion of its work is not the only check.
;;

(defun leaf-box (leaf)
  (ignore-errors (the-object leaf bounding-box)))

(defun leaf-center (leaf)
  "The midpoint of LEAF's bounding box -- not its center: leaves made by
projection or profile (a gear's teeth) inherit their parent's center,
and a center-based pile test cried pile over sound models (the eval
of 2026-09-25)."
  (let ((box (leaf-box leaf)))
    (when box
      (make-point (/ (+ (get-x (first box)) (get-x (second box))) 2)
                  (/ (+ (get-y (first box)) (get-y (second box))) 2)
                  (/ (+ (get-z (first box)) (get-z (second box))) 2)))))

(defun union-box (boxes)
  (let ((boxes (remove nil boxes)))
    (when boxes
      (flet ((corner (fn key)
               (destructuring-bind (x y z)
                   (loop for axis below 3
                         collect (reduce fn boxes :key #'(lambda (b) (aref (funcall key b) axis))))
                 (make-point x y z))))
        (list (corner #'min #'first) (corner #'max #'second))))))

(defun check-model (session &key expected-size)
  ;; building the model runs the whole file (meter.lisp)
  (multiple-value-bind (ok? reason) (meter! session :run (model-volume session))
    (unless ok? (return-from check-model (meter-refusal reason))))
  (handler-case
      (with-time-limit (*eval-seconds* "build and check")
      (let* ((model (make-model session))
             (leaves (the-object model leaves))
             (box (or (ignore-errors (the-object model bounding-box))
                      (union-box (mapcar #'leaf-box leaves))))
             (size (when box (loop for axis below 3
                                   collect (- (aref (second box) axis) (aref (first box) axis)))))
             (centers (let ((table (make-hash-table :test #'equal)))
                        (dolist (leaf leaves table)
                          (let ((center (leaf-center leaf)))
                            (when center
                              (incf (gethash (map 'list #'(lambda (c) (/ (round (* c 1000)) 1000)) center)
                                             table 0)))))))
             (crowd (let ((most 0)) (maphash #'(lambda (k v) (declare (ignore k)) (setq most (max most v)))
                                             centers)
                          most))
             (pile? (and (>= crowd 3) (> (/ crowd (max 1 (length leaves))) *pile-threshold*)))
             (broken (remove-if #'leaf-box leaves))
             (volumes (remove nil (mapcar #'(lambda (leaf) (ignore-errors (the-object leaf volume)))
                                          leaves)))
             (mismatches (when (and expected-size size)
                           (loop for want in expected-size
                                 for have in size
                                 for axis in '("X" "Y" "Z")
                                 when (and want (> (abs (- want have))
                                                   (max 1 (* 0.05 (abs want)))))
                                   collect (format nil "~a: expected ~,1f, got ~,1f" axis want have)))))
        (values
         (list (text-result "~{~a~%~}"
                           (remove nil
                                   (list "MODEL builds with default inputs."
                                         (format nil "Leaves: ~a (types: ~{~(~a~)~^, ~})."
                                                 (length leaves)
                                                 (remove-duplicates (mapcar #'type-of leaves)))
                                         (if size
                                             (format nil "Overall size X Y Z: ~{~,1f~^ x ~}." size)
                                             "Overall size: unknown.")
                                         (when broken
                                           (format nil "ERROR: ~a of ~a leaves cannot compute their geometry (first: ~a): ~a"
                                                   (length broken) (length leaves)
                                                   (the-object (first broken) root-path)
                                                   (handler-case (progn (the-object (first broken) bounding-box) "")
                                                     (error (condition) (princ-to-string condition)))))
                                         (when volumes
                                           (format nil "Volume of leaves: ~,1f." (reduce #'+ volumes)))
                                         (when pile?
                                           (format nil "WARNING: ~a of ~a leaves share one center -- parts look unplaced (a pile)."
                                                   crowd (length leaves)))
                                         (cond (mismatches
                                                (format nil "SIZE MISMATCH: ~{~a~^; ~}." mismatches))
                                               (expected-size "Expected size matched."))))))
         (and (or pile? broken mismatches) t))))
    (error (condition)
      (values (list (text-result "MODEL does not build with default inputs: ~a" condition)) t))))


;;
;; render: a picture of (make-object 'model), through the render_png tool's
;; own handler.
;;

(defun render-keyword (string)
  "A projection, layout or hidden-lines name from the API as the drawing
system's keyword (the image's case)."
  (and (stringp string) (plusp (length string))
       (every #'(lambda (c) (or (alphanumericp c) (char= c #\-))) string)
       ;; through the reader, so the keyword wears the image's case
       (let ((*read-eval* nil)) (read-from-string (format nil ":~a" string)))))

(defun render (session &key projection layout hidden-lines)
  "A wireframe of (make-object 'model), drawn by the drawing system's svg
lens and rasterised in process (raster.lisp): no Ghostscript."
  ;; building the model runs the whole file (meter.lisp)
  (multiple-value-bind (ok? reason) (meter! session :run (model-volume session))
    (unless ok? (return-from render (meter-refusal reason))))
  (handler-case
      (with-time-limit (*render-seconds* "render")
        (let* ((*package* (session-package session))
               (model (eval (read-from-string "(make-object 'model)")))
               (file (merge-pathnames "render.svg" (session-directory session))))
          (unwind-protect
               (progn
                 (apply (if layout 'gdl-user::generate-multi-view-drawing 'gdl-user::generate-single-view-drawing)
                        :format :svg
                        :output-file file
                        :object-roots (list model)
                        (append (if layout
                                    (list :views-config (render-keyword layout))
                                    (list :projection-vector (or (render-keyword projection) :trimetric)))
                                (when (render-keyword hidden-lines)
                                  (list :hidden-lines (render-keyword hidden-lines)))))
                 (let ((png (render-svg-to-png (uiop:read-file-string file :external-format :utf-8))))
                   (values (list (image-block (png-base64 png))) nil)))
            (when (probe-file file) (ignore-errors (delete-file file))))))
    (error (condition)
      (values (list (text-result "Render failed: ~a" condition)) t))))


;;
;; Dispatch and the tool definitions the Messages API is given.
;;

(defun input (name input) (cdr (assoc name input :test #'string-equal)))

(defun run-tool (session name input)
  "Run tool NAME with INPUT (an alist, keys as strings or keywords) on
SESSION.  Values: content blocks, and true on failure."
  (touch session)
  (cond ((string= name "write_model") (write-model session (input "source" input)))
        ((string= name "read_model") (read-model session))
        ((string= name "evaluate") (evaluate-expression session (input "expression" input)))
        ((string= name "check_model")
         (let ((size (input "expected_size" input)))
           (check-model session :expected-size (when size (coerce size 'list)))))
        ((and (string= name "render") (not (render-offered?)))
         (values (list (text-result "There is no render on this host; judge the model by check_model's numbers.")) t))
        ((string= name "render")
         (render session :projection (input "projection" input)
                         :layout (input "layout" input)
                         :hidden-lines (input "hidden_lines" input)))
        (t (values (list (text-result "Unknown tool ~a." name)) t))))

(defun schema (properties &optional required)
  `(("type" . "object")
    ("properties" . ,(or properties (make-hash-table)))
    ("required" . ,(coerce required 'vector))))

(defun render-offered? ()
  "Whether the agent gets the render tool: allowed, and the PNG writer here."
  (and *render-tool?* (raster-available?)))

(defun tool-definitions ()
  "The tools, as the Messages API's tools array (a vector of alists);
render only where it is offered."
  (let ((tools (%tool-definitions)))
    (if (render-offered?)
        tools
        (remove "render" tools :key #'(lambda (tool) (cdr (assoc "name" tool :test #'string=)))
                               :test #'string=))))

(defun %tool-definitions ()
  (vector
   `(("name" . "write_model")
     ("description" . "Replace the session's model file with SOURCE and compile and load it. The source holds every definition the model needs: one define-object named MODEL, whose input-slot defaults build the requested design, plus any helper define-objects or defuns. Never include an in-package form; the file header sets the package. Returns compiler warnings and errors. The visitor sees and may edit this same file.")
     ("input_schema" . ,(schema `(("source" . (("type" . "string")
                                               ("description" . "Complete Lisp source for the model file."))))
                                '("source"))))
   `(("name" . "read_model")
     ("description" . "Return the current contents of the model file. Read it before changing a model the visitor may have edited by hand.")
     ("input_schema" . ,(schema nil)))
   `(("name" . "evaluate")
     ("description" . "Evaluate ONE Lisp expression in the session package, as at a REPL, and return its printed value and output. For queries such as (the-object (make-object 'model) bounding-box). Several forms: wrap them in progn. Definitions belong in the model file (write_model).")
     ("input_schema" . ,(schema `(("expression" . (("type" . "string")
                                                   ("description" . "One Lisp expression."))))
                                '("expression"))))
   `(("name" . "check_model")
     ("description" . "Build (make-object 'model) with default inputs and report numbers computed by the harness: leaf count and types, overall size X Y Z, total volume of solid leaves, a warning when parts pile up at one center, and a comparison with EXPECTED_SIZE when given. Give the overall envelope the request implies, in mm, before building.")
     ("input_schema" . ,(schema `(("expected_size" . (("type" . "array")
                                                      ("items" . (("type" . "number")))
                                                      ("description" . "Expected overall size [x, y, z] in mm (optional)."))))
                                nil)))
   ;; the enums as ,(vector ...), not #(...) literals: inside a nested
   ;; backquote Allegro's reader turns a vector literal into an
   ;; (excl::bq-vector ...) form, which reached the encoder as a list
   ;; with a symbol at its head (the workshop, 2026-09-28)
   `(("name" . "render")
     ("description" . "Render (make-object 'model) as a wireframe drawing and return the image, to see what you built.")
     ("input_schema" . ,(schema `(("projection" . (("type" . "string")
                                                   ("enum" . ,(vector "trimetric" "top" "bottom" "left" "right" "front" "rear"))))
                                  ("layout" . (("type" . "string")
                                               ("enum" . ,(vector "isometric-plus-ortho" "orthographic-3view" "standard-4view"))
                                               ("description" . "A multi-view layout; overrides projection.")))
                                  ("hidden_lines" . (("type" . "string")
                                                     ("enum" . ,(vector "draw" "remove" "dashed"))
                                                     ("description" . "remove or dashed suits solids."))))
                                nil)))))


;;
;; The primer, from the Gendl guide.
;;

(defun primer-text ()
  "The 'Building models through the lisply tools' section of the Gendl
guide, from the first primer file found."
  (let ((file (find-if #'probe-file *primer-files*)))
    (when file
      (let* ((text (uiop:read-file-string file :external-format :utf-8))
             (start (search *primer-start* text))
             (end (when start (search (format nil "~%## ") text :start2 (1+ start)))))
        (when start (subseq text start end))))))
