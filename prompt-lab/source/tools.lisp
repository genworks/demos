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

;;
;; A timer of the lab's own.  bordeaux-threads' with-timeout starts a
;; thread that sleeps and then interrupts the caller, and stops that
;; thread only when its body ends NORMALLY: a body left by an error or a
;; throw leaves the sleeper alive, and it interrupts the calling thread
;; when its time is up, whatever that thread is doing by then.  The
;; caller is often one of the web server's workers; on Clozure CL a
;; worker interrupted while it waits for its next request never takes
;; another, and the server stops answering when its turn comes round.
;; Here the sleeper is stopped however the body ends, and it interrupts
;; nothing once the body has ended.
;;
;; The timer is Gendl's, glisp:call-with-deadline, where the image has it.
;; On an image built before Gendl carried it the copy below runs; drop
;; the copy once every image the lab is served from has Gendl's.
;;

(define-condition time-up () ((mark :initarg :mark :reader time-up-mark)))

(defun call-with-deadline (seconds function)
  "Call FUNCTION, signalling bt2:timeout in this thread if it has not
returned within SECONDS."
  (let ((gendls (find-symbol (symbol-name '#:call-with-deadline) :glisp)))
    (if (and gendls (fboundp gendls))
        (funcall gendls seconds function)
        (own-call-with-deadline seconds function))))

(defun own-call-with-deadline (seconds function)
  (let* ((caller (bt2:current-thread))
         (lock (bt2:make-lock :name "prompt-lab deadline"))
         (state :running)               ; then :fired or :done, under the lock
         (delivered? nil)
         (mark (list seconds))
         (sleeper
           (bt2:make-thread
            #'(lambda ()
                (sleep seconds)
                (bt2:with-lock-held (lock)
                  (when (eq state :running)
                    (setq state :fired)
                    (bt2:interrupt-thread caller
                                          #'(lambda ()
                                              (setq delivered? t)
                                              (signal 'time-up :mark mark))))))
            :name "prompt-lab deadline")))
    (unwind-protect
         (handler-bind ((time-up #'(lambda (condition)
                                     (when (eq (time-up-mark condition) mark)
                                       (error 'bt2:timeout :length seconds)))))
           (funcall function))
      (let ((fired? (bt2:with-lock-held (lock)
                      (prog1 (eq state :fired)
                        (when (eq state :running) (setq state :done))))))
        (if fired?
            ;; the interrupt is on its way: let it land here, where it
            ;; finds no handler, and not on whatever this thread does next
            (loop repeat 200 until delivered? do (sleep 0.01))
            (when (bt2:thread-alive-p sleeper)
              (ignore-errors (bt2:destroy-thread sleeper))))))))

(defmacro with-deadline ((seconds) &body body)
  "Run BODY, signalling bt2:timeout if it takes longer than SECONDS."
  `(call-with-deadline ,seconds #'(lambda () ,@body)))

(defmacro with-time-limit ((seconds what) &body body)
  "Run BODY, stopping it after SECONDS with an ordinary error naming WHAT,
so the tool's own error handling reports it like any other failure."
  `(handler-case (with-deadline (,seconds) ,@body)
     (bt2:timeout ()
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

(defun in-package-line? (source)
  "True when a line of SOURCE begins with an in-package form.  Only a form
that opens a line counts: the words in a comment or a string do not."
  (with-input-from-string (in source)
    (loop for line = (read-line in nil) while line
          thereis (let ((text (string-left-trim '(#\space #\tab) line)))
                    (and (>= (length text) 11)
                         (string-equal "(in-package" text :end2 11))))))

(defun copyright-holder (session)
  "Whose name goes on SESSION's copyright line: the name its author goes
by, typed beside the author key and kept with it at the gate (the user's
ruling, 2026-11-12), else the author of the session by its id."
  (or (ignore-errors (author-name (session-wallet session)))
      (format nil "the author of prompt-lab session ~a" (session-id session))))

(defun license-header (session)
  "The copyright and license lines of SESSION's model file.  The author's
name is on the copyright line.  Code made in the lab is AGPL-3.0 (or
later, as Gendl itself is) unless its session is closed; a closed file
reserves its rights and names no license until its author decides."
  (let ((year (nth-value 5 (decode-universal-time (get-universal-time) 0)))
        (holder (copyright-holder session)))
    (if (session-closed? session)
        (format nil ";;~%;; Copyright (C) ~d ~a.  All rights reserved.~%" year holder)
        (format nil ";;~%;; Copyright (C) ~d ~a.~%;;~%~
;; SPDX-License-Identifier: AGPL-3.0-or-later~%~
;; Free software under the GNU Affero General Public License, version 3 or~%~
;; (at your option) any later version: <https://www.gnu.org/licenses/agpl-3.0.html>.~%"
                year holder))))

(defun write-model (session source)
  (when (in-package-line? source)
    (return-from write-model
      (values (list (text-result "Refused: do not put an in-package form in the source. ~
The file's header already sets the session package.")) t)))
  ;; a web app's package sees the names of GWL and the web server: a
  ;; definition that lands on one is stopped here (kinds.lisp)
  (when (eq (session-kind session) :app)
    (let ((foreign (foreign-definitions session source)))
      (when foreign
        (return-from write-model
          (values (list (text-result "Refused: the source defines ~{~a~^, ~}, which ~:[is a name~;are names~] Lisp, Gendl or GWL already ~:*~:[has~;have~].  Give your own objects and functions names of their own."
                                     foreign (rest foreign)))
                  t)))))
  (watch-source! session source)
  (let ((file (session-model-file session))
        (package-name (session-package-name session)))
    (with-open-file (out file :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (format out ";; Prompt lab session ~a -- the model file.~%~
;; Written by the modeling agent; edit it freely.  The object named MODEL~%~
;; is what (make-object 'model) builds and the viewer shows~:[.~;;~%~
;; the object named APP is the web app, a GWL page the lab serves.~]~%~
~@[~a~]~%~
(in-package ~s)~%~%~a~%"
              (session-id session) (eq (session-kind session) :app)
              (license-header session) package-name source))
    (load-model-file session)))

(defun edit-model (session old new)
  "Replace OLD, which must occur exactly once in the model file's body
(the file as read_model shows it), with NEW, and compile and load the
result as write-model does.  The agent's way to change a long file
without writing it whole again (2026-10-20: one write_model of a 23 KB
app file with a bigger table in it outgrew the reply twice, and nothing
of a cut-off tool call arrives).  Values: content blocks and an error
flag."
  (let ((file (session-model-file session)))
    (cond ((not (probe-file file))
           (values (list (text-result "There is no model file yet; write one with write_model.")) t))
          ((or (not (stringp old)) (zerop (length old)))
           (values (list (text-result "Refused: OLD is empty.  Give the passage to replace, exactly as read_model shows it.")) t))
          (t
           (let* ((body (file-model-body file))
                  (new (if (stringp new) new ""))
                  (count (loop with start = 0
                               for pos = (search old body :start2 start)
                               while pos
                               count t
                               do (setq start (+ pos (length old))))))
             (if (/= count 1)
                 (values (list (text-result "Refused: OLD occurs ~a times in the model file, not once.  ~:[Give enough surrounding text to match one passage.~;Read the file with read_model and give the passage exactly as it is there.~]"
                                            count (zerop count)))
                         t)
                 ;; the body's trailing newline is write-model's to add
                 ;; back, or every edit would leave one more
                 (let ((pos (search old body)))
                   (write-model session (string-right-trim '(#\Newline #\Return)
                                                           (concatenate 'string
                                                                        (subseq body 0 pos)
                                                                        new
                                                                        (subseq body (+ pos (length old)))))))))))))

(defun load-model-file (session)
  "Compile and load the session's model file.  Values: content blocks and
an error flag.  The file as it is now goes to the archive first, so a
version that fails to compile is kept too."
  (let ((file (session-model-file session))
        (warnings nil))
    (archive-model! session)
    ;; what the old file charged for is no longer what this one does (deploy.lisp)
    (forget-built-tolls! session)
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
            ;; MODEL, or for a session that builds a web app, APP (kinds.lisp)
            (let ((defined? (let ((symbol (built-symbol session)))
                              (and symbol (find-class symbol nil) t))))
              (values (list (text-result "Compiled and loaded.~:[  No object named ~a is defined!~;~*~]~@[~%Warnings:~%~{- ~a~%~}~]"
                                        defined? (built-name session) (reverse warnings)))
                      (not defined?)))))
      ;; a stack overflow too (see evaluate-expression)
      (serious-condition (condition)
        (values (list (text-result "Error compiling or loading the model file: ~a~@[~%Warnings first:~%~{- ~a~%~}~]~@[~%~a~]"
                                  condition (reverse warnings)
                                  ;; a house model loses count of its parentheses
                                  ;; (agent.lisp): say where, which the reader's
                                  ;; 'unexpected end of file' does not
                                  (and (house-model? session)
                                       (ignore-errors (paren-report (file-model-body file))))))
                t)))))

(defun paren-report (text)
  "Where TEXT's parentheses fail to balance, in words for the agent, or nil
when they balance.  Strings, comments and character literals are skipped."
  (let ((depth 0) (line 1) (form-line nil) (form-head nil) (i 0) (n (length text)))
    (flet ((head (start)
             (let ((end (or (position #\Newline text :start start) n)))
               (string-trim '(#\Space #\Tab) (subseq text start (min end (+ start 50)))))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((char= c #\Newline) (incf line))
                       ((char= c #\;)
                        (setq i (or (position #\Newline text :start i) n))
                        (when (< i n) (incf line)))
                       ((char= c #\")
                        (incf i)
                        (loop while (and (< i n) (char/= (char text i) #\"))
                              do (when (char= (char text i) #\\) (incf i))
                                 (when (and (< i n) (char= (char text i) #\Newline)) (incf line))
                                 (incf i)))
                       ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\\))
                        (incf i 2))
                       ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                        (let ((end (or (search "|#" text :start2 (+ i 2)) n)))
                          (incf line (count #\Newline text :start i :end end))
                          (setq i (1+ end))))
                       ((char= c #\()
                        (when (zerop depth) (setq form-line line form-head (head i)))
                        (incf depth))
                       ((char= c #\))
                        (decf depth)
                        (when (minusp depth)
                          (return-from paren-report
                            (format nil "The parentheses do not balance: an extra ')' on line ~d closes more than was opened.  Write the whole file again with write_model." line))))))
               (incf i))
      (when (plusp depth)
        (format nil "The parentheses do not balance: ~d '(' left open at the end of the file.  The top-level form starting on line ~d, ~s, is not closed.  Write the whole file again with write_model, closing each child form on its own last line, then ')' for its section and ')' for the define-object."
                depth form-line form-head)))))

(defun read-model (session)
  "The model's source as write-model takes it: the file after its header
(the lab's comment and the in-package line), which is also what the
page's editor shows.  Handed the whole file, an agent wrote the header's
comment back into the source, and the next write put a second one on top.
All of it, never cut at *result-limit*: an agent asked to change a model
it did not write in this conversation (a visitor's edit, a session opened
on a file) has nothing else to work from, and one shown the first 4000
characters of a longer file spent its rounds looking for the rest."
  (let ((file (session-model-file session)))
    (if (probe-file file)
        (values (list `(("type" . "text") ("text" . ,(file-model-body file)))) nil)
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
        ;; #. would run at read time, outside the time limit below
        (handler-case (let ((*read-eval* nil)) (read-from-string expression))
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
          ;; serious-condition, not error: a model that recurses into
          ;; itself overflows the stack, a storage-condition, which took
          ;; the whole Lisp down when only errors were caught (2026-11-14)
          (serious-condition (condition)
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
                                               (expected-size "Expected size matched."))
                                         ;; what a deployed copy would charge for (deploy.lisp)
                                         (let ((prices (ignore-errors
                                                        (file-prices (remove-if-not #'valid-toll? (the-object model tolls))
                                                                     (the-object model file-tolls)))))
                                           (when prices
                                             (format nil "Charges when deployed: ~{~a~^, ~}."
                                                     (loop for (format . cents) in prices
                                                           collect (format nil "~a $~,2f" format (/ cents 100))))))))))
         (and (or pile? broken mismatches) t))))
    ;; a stack overflow too (see evaluate-expression)
    (serious-condition (condition)
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
    ;; a stack overflow too (see evaluate-expression)
    (serious-condition (condition)
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
        ((string= name "edit_model") (edit-model session (input "old" input) (input "new" input)))
        ((string= name "read_model") (read-model session))
        ((string= name "evaluate") (evaluate-expression session (input "expression" input)))
        ((string= name "check_model")
         (let ((size (input "expected_size" input)))
           (check-model session :expected-size (when size (coerce size 'list)))))
        ;; the web app's page (kinds.lisp)
        ((string= name "check_app") (check-app session))
        ((and (string= name "render") (not (render-offered?)))
         (values (list (text-result "There is no render on this host; judge the model by check_model's numbers.")) t))
        ((string= name "render")
         (render session :projection (input "projection" input)
                         :layout (input "layout" input)
                         :hidden-lines (input "hidden_lines" input)))
        ;; the reference (docs.lisp)
        ((string= name "search_docs") (search-docs (input "query" input) :hits (input "hits" input)))
        ((string= name "describe_object") (describe-type session (input "type" input)))
        ;; the visitor's uploaded files (uploads.lisp)
        ((string= name "list_files") (list-files-tool session))
        ((string= name "read_file")
         (read-file-tool session (input "name" input)
                         :offset (input "offset" input) :limit (input "limit" input)))
        ;; the visitor's own project (project.lisp)
        ((string= name "list_project") (list-project-tool session))
        ((string= name "read_project_file")
         (read-project-file-tool session (input "path" input)
                                 :offset (input "offset" input) :limit (input "limit" input)))
        ((string= name "write_project_file")
         (write-project-file-tool session (input "path" input) (input "content" input)))
        ((string= name "check_profile") (check-profile-tool session))
        ;; structure and sight for a house model (below)
        ((string= name "set_slot") (set-slot-tool session (input "object" input) (input "slot" input) (input "value" input)))
        ((string= name "add_slot") (add-slot-tool session (input "object" input) (input "section" input)
                                                  (input "slot" input) (input "value" input)))
        ((string= name "remove_slot") (remove-slot-tool session (input "object" input) (input "slot" input)))
        ((string= name "replace_form") (replace-form-tool session (input "name" input) (input "source" input)))
        ((string= name "view_model") (view-model session))
        ((string= name "look_at_model") (look-at-model session))
        (t (values (list (text-result "Unknown tool ~a." name)) t))))

(defun schema (properties &optional required)
  `(("type" . "object")
    ("properties" . ,(or properties (make-hash-table)))
    ("required" . ,(coerce required 'vector))))

(defun render-offered? ()
  "Whether the agent gets the render tool: allowed, and the PNG writer here."
  (and *render-tool?* (raster-available?)))

(defparameter *house-vision-models* nil
  "List of house model ids served with a vision projector: such a session
is offered render, and looks at its own drawing.  Nil: every house model
reads text only (view_model is its sight).")

(defparameter *house-viewer-model* nil
  "String or nil.  A vision model on the gate's menu that looks at a
text-only house model's render for it and describes it (look_at_model): a
coder and a separate viewer.  Nil: no viewer.")

(defparameter *house-structure?* t
  "Boolean.  Whether a house session edits by structure and has view_model
(set_slot, add_slot, remove_slot, replace_form; edit_model withdrawn).")

(defun tool-definitions (&optional session)
  "The tools, as the Messages API's tools array (a vector of alists);
render, search_docs and the two file tools only where they are offered,
and check_app only for a SESSION that builds a web app (kinds.lisp)."
  (let ((tools (%tool-definitions)))
    (flet ((without (name tools)
             (remove name tools :key #'(lambda (tool) (cdr (assoc "name" tool :test #'string=)))
                                :test #'string=)))
      (unless (and session (eq (session-kind session) :app))
        (setq tools (without "check_app" tools)))
      ;; a project session works on its files and builds nothing here;
      ;; any other has no project (project.lisp)
      (if (and session (eq (session-kind session) :project))
          (dolist (name '("write_model" "edit_model" "read_model" "evaluate" "check_model" "render"))
            (setq tools (without name tools)))
          (dolist (name '("list_project" "read_project_file" "write_project_file" "check_profile"))
            (setq tools (without name tools))))
      ;; a house model reads text only (agent.lisp) unless it is one that
      ;; also sees (*house-vision-models*): then it renders and looks too
      (unless (and (render-offered?)
                   (or (not (house-model? session))
                       (member (session-model session) *house-vision-models* :test #'equal)))
        (setq tools (without "render" tools)))
      (unless (search-offered?) (setq tools (without "search_docs" tools)))
      (unless (uploads-offered?)
        (setq tools (without "read_file" (without "list_files" tools))))
      ;; a house model edits by structure and sees in words (below): the
      ;; raw-text edit goes, which its unbalanced patches came through
      (when (and (house-model? session) *house-structure?*)
        (setq tools (concatenate 'vector (without "edit_model" tools) (house-tool-definitions))))
      ;; a house model that cannot see may have a viewer look for it
      (when (and (house-model? session) *house-viewer-model* (render-offered?)
                 (not (member (session-model session) *house-vision-models* :test #'equal)))
        (setq tools (concatenate 'vector tools (list (look-tool-definition)))))
      tools)))

(defun %tool-definitions ()
  (vector
   `(("name" . "write_model")
     ("description" . "Replace the session's model file with SOURCE and compile and load it. The source holds every definition the model needs: one define-object named MODEL, whose input-slot defaults build the requested design, plus any helper define-objects or defuns. Never include an in-package form; the file header sets the package. Returns compiler warnings and errors. The visitor sees and may edit this same file.")
     ("input_schema" . ,(schema `(("source" . (("type" . "string")
                                               ("description" . "Complete Lisp source for the model file."))))
                                '("source"))))
   `(("name" . "edit_model")
     ("description" . "Change part of the model file: replace OLD, a passage that occurs exactly once in the file as read_model shows it, with NEW, then compile and load as write_model does. Use it to change a file that is already long -- rows added to a table, one definition rewritten -- instead of writing the whole file again: a reply holds about 30,000 tokens in all, thinking included, and one that runs out is lost entirely, nothing written. Give OLD enough surrounding text to be unique; refused when it occurs no times or more than once. Returns compiler warnings and errors.")
     ("input_schema" . ,(schema `(("old" . (("type" . "string")
                                            ("description" . "The passage to replace, exactly as it is in the file, occurring once.")))
                                  ("new" . (("type" . "string")
                                            ("description" . "What takes its place; empty to delete the passage."))))
                                '("old" "new"))))
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
   ;; a session that builds a web app (kinds.lisp)
   `(("name" . "check_app")
     ("description" . "Build (make-object 'app) and render its page, and report what the harness sees: the title, each form control with its value, the sections, whether the body embeds them all, the viewport's objects and leaves, and the page's text. Run it after every write_model of a web app.")
     ("input_schema" . ,(schema nil)))
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
                                nil)))
   ;; the reference: what a type takes, and where a thing is defined (docs.lisp)
   `(("name" . "describe_object")
     ("description" . "Describe a Gendl object type: its description and mixins, its input slots (required and optional) with their documentation, and its documented computed slots, children and functions. Use it to learn what a type takes -- subtracted-solid, extruded-solid, global-polygon-projection -- before writing it into the model. Cheap: nothing is evaluated.")
     ("input_schema" . ,(schema `(("type" . (("type" . "string")
                                             ("description" . "The type's name, as written in a define-object (e.g. cylinder-solid, surf:subtracted-solid)."))))
                                '("type"))))
   `(("name" . "search_docs")
     ("description" . "Search the Gendl sources and guides: object definitions with their documentation, functions, the guide's sections, the demos. Returns the best matches with file, line and a passage. Use it to find the right type or input name, or an example, instead of guessing one.")
     ("input_schema" . ,(schema `(("query" . (("type" . "string")
                                              ("description" . "Words to search for, e.g. extruded-solid profile, or keyway.")))
                                  ("hits" . (("type" . "integer")
                                             ("description" . "How many matches to return (default 5, at most 20)."))))
                                '("query"))))
   ;; the visitor's uploaded files (uploads.lisp)
   `(("name" . "list_files")
     ("description" . "List the files the visitor has uploaded to this session: each one's name, kind, size and path on the host. A PDF or an image also arrives in the conversation with the visitor's message; a text file (DXF, SVG, CSV, STEP...) is read with read_file, or by its path from the model.")
     ("input_schema" . ,(schema nil)))
   `(("name" . "read_file")
     ("description" . "Read an uploaded file. A text file comes back as text, at most 20000 characters a call: give OFFSET to read further into a long one. An image comes back as the image. A PDF is already in the conversation, attached to the visitor's message.")
     ("input_schema" . ,(schema `(("name" . (("type" . "string")
                                             ("description" . "The file's name, as list_files gives it.")))
                                  ("offset" . (("type" . "integer")
                                               ("description" . "Character to start from (default 0).")))
                                  ("limit" . (("type" . "integer")
                                              ("description" . "Characters to return (default and most: 20000)."))))
                                '("name"))))
   ;; the visitor's own project (project.lisp)
   `(("name" . "list_project")
     ("description" . "List the project's files with their sizes, marking those changed or new since the project was read from GitLab, and what the reading left out.")
     ("input_schema" . ,(schema nil)))
   `(("name" . "read_project_file")
     ("description" . "Read one of the project's files, at most 20000 characters a call: give OFFSET to read further into a long one.")
     ("input_schema" . ,(schema `(("path" . (("type" . "string")
                                             ("description" . "The file's path in the project, as list_project gives it.")))
                                  ("offset" . (("type" . "integer")
                                               ("description" . "Character to start from (default 0).")))
                                  ("limit" . (("type" . "integer")
                                              ("description" . "Characters to return (default and most: 20000)."))))
                                '("path"))))
   `(("name" . "write_project_file")
     ("description" . "Write one whole file of the project: a new file, or a replacement for one that is there. The change stays in this session until the visitor pushes it, as a merge request they approve on GitLab.")
     ("input_schema" . ,(schema `(("path" . (("type" . "string")
                                             ("description" . "The file's path in the project, e.g. monetize.sexp or src/server.lisp.")))
                                  ("content" . (("type" . "string")
                                                ("description" . "The file's whole new text."))))
                                '("path" "content"))))
   `(("name" . "check_profile")
     ("description" . "Check the project against the Monocle profile as far as reading can tell: image.sexp, PORT, the health path, DATA_DIR, monetize.sexp. Each rule comes back ok or TO DO with the reason.")
     ("input_schema" . ,(schema nil)))))


;;
;; STRUCTURE AND SIGHT FOR HOUSE MODELS (2026-11-14; the user, on the
;; overnight report: 'the house models have trouble writing balanced
;; parentheses.  Could we give them tools ... to help with that?' and 'the
;; main thing is these prompt-lab sessions need to be able to
;; render-view-iterate').  A house session edits its file by STRUCTURE --
;; a whole top-level form by name, or one slot of a define-object by name,
;; the tool writing every parenthesis around what the model gives -- and
;; every fragment it gives is balance-checked before anything is written;
;; and it SEES the built model as text (view_model): each part's size and
;; place, what touches and overlaps what, what floats, the symmetry.  A
;; session on the lab's own model keeps its tools as they were.
;;

(defun blank-end (text i)
  "Past whitespace, ; comments and #| |# blocks of TEXT from I."
  (let ((n (length text)))
    (loop
      (cond ((>= i n) (return n))
            ((member (char text i) '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
            ((char= (char text i) #\;) (setq i (or (position #\Newline text :start i) n)))
            ((and (char= (char text i) #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
             (setq i (let ((e (search "|#" text :start2 (+ i 2)))) (if e (+ e 2) n))))
            (t (return i))))))

(defun form-end (text i)
  "The position just past the form that starts at I in TEXT, or nil when it
does not end (a list left open, a string left unclosed, a stray ')')."
  (let ((n (length text)))
    (when (< i n)
      (let ((c (char text i)))
        (cond ((char= c #\()
               (let ((j (1+ i)))
                 (loop
                   (setq j (blank-end text j))
                   (cond ((>= j n) (return nil))
                         ((char= (char text j) #\)) (return (1+ j)))
                         (t (let ((e (form-end text j)))
                              (if e (setq j e) (return nil))))))))
              ((char= c #\)) nil)
              ((char= c #\")
               (let ((j (1+ i)))
                 (loop
                   (cond ((>= j n) (return nil))
                         ((char= (char text j) #\\) (incf j 2))
                         ((char= (char text j) #\") (return (1+ j)))
                         (t (incf j))))))
              ((member c '(#\' #\`))
               (form-end text (blank-end text (1+ i))))
              ((char= c #\,)
               (form-end text (blank-end text (if (and (< (1+ i) n) (char= (char text (1+ i)) #\@)) (+ i 2) (1+ i)))))
              ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\'))
               (form-end text (+ i 2)))
              ((and (char= c #\#) (< (+ i 2) n) (char= (char text (1+ i)) #\\))
               ;; a character: the one after the backslash, and the rest of
               ;; a name such as Space
               (let ((j (+ i 3)))
                 (when (alpha-char-p (char text (+ i 2)))
                   (loop while (and (< j n) (alphanumericp (char text j))) do (incf j)))
                 (min j n)))
              (t
               (loop for j from i below n
                     when (member (char text j) '(#\Space #\Tab #\Newline #\Return #\Page #\( #\) #\" #\;))
                       return j
                     finally (return n))))))))

(defun form-elements (text open)
  "The elements of the list whose '(' is at OPEN in TEXT, as (start . end)
spans; a second value, the position of its ')', nil when it does not close."
  (let ((n (length text)) (j (1+ open)) (elements nil))
    (loop
      (setq j (blank-end text j))
      (cond ((>= j n) (return (values (nreverse elements) nil)))
            ((char= (char text j) #\)) (return (values (nreverse elements) j)))
            (t (let ((e (form-end text j)))
                 (unless e (return (values (nreverse elements) nil)))
                 (push (cons j e) elements)
                 (setq j e)))))))

(defun top-forms (text)
  "TEXT's top-level forms as (start . end) spans; a second value, true when
the whole text reads to its end."
  (let ((n (length text)) (j 0) (forms nil))
    (loop
      (setq j (blank-end text j))
      (when (>= j n) (return (values (nreverse forms) t)))
      (let ((e (form-end text j)))
        (unless e (return (values (nreverse forms) nil)))
        (push (cons j e) forms)
        (setq j e)))))

(defun span-text (text span) (subseq text (car span) (cdr span)))

(defun fragment-problem (fragment &key one-form?)
  "Why FRAGMENT, a piece of Lisp an agent gave a structural tool, cannot be
written as it is, or nil.  ONE-FORM? asks for exactly one form."
  (cond ((not (and (stringp fragment) (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) fragment)))))
         "It is empty.")
        ((paren-report fragment))
        (t (multiple-value-bind (forms complete?) (top-forms fragment)
             (cond ((not complete?) "It does not read to its end: a string or a list is left open.")
                   ((and one-form? (/= (length forms) 1))
                    (format nil "It holds ~d forms; give exactly one." (length forms))))))))

(defparameter *definer-names* '("define-object" "defun" "defparameter" "defvar" "defmacro" "defmethod" "defgeneric")
  "The heads of the top-level forms replace_form finds by name.")

(defparameter *slot-sections* '(":input-slots" ":computed-slots" ":objects" ":hidden-objects" ":functions")
  "The sections of a define-object whose entries the slot tools edit.")

(defun model-body-text (session)
  (let ((file (session-model-file session)))
    (if (probe-file file) (file-model-body file) "")))

(defun definitions-named (text name &optional (heads *definer-names*))
  "TEXT's top-level forms that define NAME: a list of (span elements)."
  (loop for span in (top-forms text)
        for elements = (and (char= (char text (car span)) #\() (form-elements text (car span)))
        when (and (>= (length elements) 2)
                  (member (span-text text (first elements)) heads :test #'string-equal)
                  (string-equal (span-text text (second elements)) name))
          collect (list span elements)))

(defun slot-entry-name (text span)
  "The name of a define-object section's entry: its first element that is
not a documentation string."
  (when (char= (char text (car span)) #\()
    (let ((first (find-if-not #'(lambda (e) (char= (char text (car e)) #\")) (form-elements text (car span)))))
      (and first (string-downcase (span-text text first))))))

(defun object-sections (text elements)
  "The sections of a define-object whose ELEMENTS are given: an alist from
the keyword's text, downcased, to the span of its list."
  (loop for (key value) on (nthcdr 3 elements) by #'cddr
        when (and key value (char= (char text (car key)) #\:) (char= (char text (car value)) #\())
          collect (cons (string-downcase (span-text text key)) value)))

(defun find-slot (text elements slot)
  "Where SLOT is in the define-object of ELEMENTS: (values section span)."
  (loop for (section . list-span) in (object-sections text elements)
        when (member section *slot-sections* :test #'string=)
          do (dolist (entry (form-elements text (car list-span)))
               (when (equal (slot-entry-name text entry) (string-downcase slot))
                 (return-from find-slot (values section entry))))))

(defun structural-refusal (control &rest args)
  (values (list (apply #'text-result control args)) t))

(defun with-object (session object function)
  "Call FUNCTION with the file's text and the define-object OBJECT's span
and elements, or refuse when it is not there just once, or the file itself
does not read."
  (let* ((text (model-body-text session)))
    (multiple-value-bind (forms complete?) (top-forms text)
      (declare (ignore forms))
      (cond ((not complete?)
             (structural-refusal "The model file does not read to its end now, so it cannot be edited by structure.  ~a  Write it whole again with write_model."
                                 (or (paren-report text) "")))
            (t (let ((found (definitions-named text object '("define-object"))))
                 (case (length found)
                   (0 (structural-refusal "There is no define-object named ~a in the model file.  replace_form with a whole (define-object ~a ...) adds one." object object))
                   (1 (funcall function text (first (first found)) (second (first found))))
                   (t (structural-refusal "The model file defines ~a ~d times; keep one, with replace_form." object (length found))))))))))

(defun splice (text start end new)
  (concatenate 'string (subseq text 0 start) new (subseq text end)))

(defun replace-form-tool (session name source)
  "Replace the top-level form defining NAME with SOURCE, one whole form that
defines NAME; with no such form, add SOURCE at the end of the file."
  (let ((problem (fragment-problem source :one-form? t)))
    (when problem (return-from replace-form-tool (structural-refusal "Nothing written: ~a" problem))))
  (let* ((source (string-trim '(#\Space #\Tab #\Newline #\Return) source))
         (own (form-elements source 0)))
    (unless (and (char= (char source 0) #\() (>= (length own) 2)
                 (string-equal (span-text source (second own)) name))
      (return-from replace-form-tool
        (structural-refusal "Nothing written: SOURCE must be one form that defines ~a -- (define-object ~a ...), (defun ~a ...) or the like." name name name)))
    (let ((text (model-body-text session)))
      (multiple-value-bind (forms complete?) (top-forms text)
        (declare (ignore forms))
        (unless complete?
          (return-from replace-form-tool
            (structural-refusal "The model file does not read to its end now.  ~a  Write it whole again with write_model."
                                (or (paren-report text) ""))))
        (let ((found (definitions-named text name)))
          (case (length found)
            (0 (write-model session (if (plusp (length (string-trim '(#\Space #\Newline) text)))
                                        (format nil "~a~%~%~a" (string-right-trim '(#\Space #\Newline) text) source)
                                        source)))
            (1 (let ((span (first (first found))))
                 (write-model session (splice text (car span) (cdr span) source))))
            (t (structural-refusal "The model file defines ~a ~d times; write it whole with write_model." name (length found)))))))))

(defun slot-entry-text (slot value)
  (format nil "(~a ~a)" slot (string-trim '(#\Space #\Tab #\Newline #\Return) value)))

(defparameter *child-recipe*
  "A child is ONE entry of the objects section, named for itself: add_slot with object \"model\", section \"objects\", slot \"shelf\" and value \":type 'box :width (the width) :length 300 :height 18 :center (translate (the center) :up 100)\" adds it; set_slot with slot \"shelf\" and a new value changes it; remove_slot with slot \"shelf\" removes it.  One child per call, and VALUE starts at :type -- no parentheses around it, no list of children."
  "How a child object is added, changed and removed: said by every refusal
that catches a section or a list of children where one child belongs.")

(defun section-name? (slot)
  "True when SLOT names a define-object section (objects, :input-slots ...)
rather than an entry of one."
  (and (stringp slot)
       (member (concatenate 'string ":" (string-downcase (string-left-trim ":" (string-trim " " slot))))
               *slot-sections* :test #'string=)))

(defun section-refusal (slot)
  (structural-refusal "Nothing written: ~a is a section of the define-object, not a slot in it; these tools take ONE entry by its own name.  ~a"
                      slot *child-recipe*))

(defun child-value-problem (section value)
  "For an entry of SECTION (objects or hidden-objects), a refusal text when
VALUE is not one child's inputs -- a list of children, or the child in its
own parentheses; else nil."
  (when (and (member section '(":objects" ":hidden-objects") :test #'string=)
             (stringp value)
             (let ((v (string-left-trim '(#\Space #\Tab #\Newline #\Return) value)))
               (and (plusp (length v)) (char= (char v 0) #\())))
    (format nil "Nothing written: VALUE begins with a parenthesis, but a child's VALUE is its inputs alone.  ~a" *child-recipe*)))

(defun set-slot-tool (session object slot value)
  "Replace SLOT of the define-object OBJECT -- an input, a computed slot, a
child, a function -- with (SLOT VALUE)."
  (when (section-name? slot)
    (return-from set-slot-tool (section-refusal slot)))
  (let ((problem (fragment-problem value)))
    (when problem (return-from set-slot-tool (structural-refusal "Nothing written: VALUE ~a" problem))))
  (with-object session object
    #'(lambda (text span elements)
        (declare (ignore span))
        (multiple-value-bind (section entry) (find-slot text elements slot)
          (cond ((null section)
                 (structural-refusal "~a has no slot named ~a; add_slot adds one." object slot))
                ((child-value-problem section value)
                 (structural-refusal "~a" (child-value-problem section value)))
                (t (write-model session (splice text (car entry) (cdr entry) (slot-entry-text slot value)))))))))

(defun add-slot-tool (session object section slot value)
  "Add (SLOT VALUE) to SECTION of the define-object OBJECT, making the
section when it has none."
  (let ((problem (fragment-problem value))
        (section (string-downcase (if (and (stringp section) (plusp (length section)) (char/= (char section 0) #\:))
                                      (concatenate 'string ":" section)
                                      (or section "")))))
    (cond ((section-name? slot) (section-refusal slot))
          (problem (structural-refusal "Nothing written: VALUE ~a" problem))
          ((child-value-problem section value)
           (structural-refusal "~a" (child-value-problem section value)))
          ((not (member section *slot-sections* :test #'string=))
           (structural-refusal "SECTION is one of ~{~a~^, ~}." (mapcar #'(lambda (s) (subseq s 1)) *slot-sections*)))
          (t (with-object session object
               #'(lambda (text span elements)
                   (cond ((find-slot text elements slot)
                          (structural-refusal "~a already has a slot named ~a: set_slot changes it.  A name is used once in a define-object." object slot))
                         (t (let ((list-span (cdr (assoc section (object-sections text elements) :test #'string=)))
                                  (entry (slot-entry-text slot value)))
                              (write-model session
                                           (if list-span
                                               (let ((close (1- (cdr list-span))))
                                                 (splice text close close (format nil "~%   ~a" entry)))
                                               (let ((close (1- (cdr span))))
                                                 (splice text close close (format nil "~%~%  ~a~%  (~a)" section entry))))))))))))))

(defun remove-slot-tool (session object slot)
  "Remove SLOT from the define-object OBJECT."
  (when (section-name? slot)
    (return-from remove-slot-tool (section-refusal slot)))
  (with-object session object
    #'(lambda (text span elements)
        (declare (ignore span))
        (multiple-value-bind (section entry) (find-slot text elements slot)
          (if (null section)
              (structural-refusal "~a has no slot named ~a." object slot)
              ;; the entry and the blank before it
              (let ((start (or (position-if-not #'(lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                                text :end (car entry) :from-end t)
                               (car entry))))
                (write-model session (splice text (1+ start) (cdr entry) ""))))))))

(defun leaf-name (leaf)
  (let ((path (ignore-errors (the-object leaf root-path))))
    (if path (format nil "~{~(~a~)~^ ~}" path) "model")))

(defun axis-word (vector)
  "X, Y or Z with its sign when VECTOR lies along an axis, else the vector."
  (let ((axis (position (reduce #'max (map 'list #'abs vector)) (map 'list #'abs vector))))
    (if (> (abs (aref vector axis)) 0.999)
        (format nil "~:[-~;+~]~a" (plusp (aref vector axis)) (char "XYZ" axis))
        (format nil "(~{~,2f~^ ~})" (coerce vector 'list)))))

(defparameter *view-max-parts* 40
  "Parts view_model describes one by one; a bigger model is summed up by type.")

(defun view-model (session)
  "The built model in words: every part's type, size and place, what
touches and overlaps what, what floats, and its symmetry."
  (handler-case
      (with-time-limit (*eval-seconds* "view")
        (let* ((model (make-model session))
               (leaves (the-object model leaves))
               (boxes (mapcar #'leaf-box leaves))
               (env (or (ignore-errors (the-object model bounding-box)) (union-box boxes)))
               (n (length leaves))
               (lines nil))
          (flet ((lo (box axis) (aref (first box) axis))
                 (hi (box axis) (aref (second box) axis))
                 (say (control &rest args) (push (apply #'format nil control args) lines)))
            (when env
              (say "MODEL's envelope: X ~,1f..~,1f  Y ~,1f..~,1f  Z ~,1f..~,1f  (~,1f x ~,1f x ~,1f)."
                   (lo env 0) (hi env 0) (lo env 1) (hi env 1) (lo env 2) (hi env 2)
                   (- (hi env 0) (lo env 0)) (- (hi env 1) (lo env 1)) (- (hi env 2) (lo env 2))))
            (cond
              ((zerop n) (say "No parts: MODEL has no leaves to draw."))
              ((> n *view-max-parts*)
               (say "~d parts, too many to list; by type: ~{~a~^, ~}." n
                    (let ((counts nil))
                      (dolist (leaf leaves) (incf (getf counts (type-of leaf) 0)))
                      (loop for (type count) on counts by #'cddr collect (format nil "~d ~(~a~)" count type)))))
              (t
               (let ((names (mapcar #'leaf-name leaves))
                     (floor-z (and env (lo env 2))))
                 (say "~d part~:p:" n)
                 (loop for leaf in leaves for box in boxes for name in names for i from 0
                       do (if (null box)
                              (say "- ~a (~(~a~)): its geometry does not compute." name (type-of leaf))
                              (let ((touching nil) (overlapping nil))
                                (loop for other in boxes for other-name in names for k from 0
                                      when (and other (/= i k))
                                        do (let ((gap (loop for axis below 3
                                                            maximize (max (- (lo box axis) (hi other axis))
                                                                          (- (lo other axis) (hi box axis)))))
                                                 (depth (loop for axis below 3
                                                              minimize (- (min (hi box axis) (hi other axis))
                                                                          (max (lo box axis) (lo other axis))))))
                                             (cond ((> depth 1) (push other-name overlapping))
                                                   ((<= gap 0.5) (push other-name touching)))))
                                (say "- ~a (~(~a~)): ~,1f x ~,1f x ~,1f, X ~,1f..~,1f Y ~,1f..~,1f Z ~,1f..~,1f~@[, axis ~a~]~:[~;, on the floor~]~@[; touches ~{~a~^, ~}~]~@[; OVERLAPS ~{~a~^, ~}~]~:[~;; FLOATS: touches nothing, off the floor~]"
                                     name (type-of leaf)
                                     (- (hi box 0) (lo box 0)) (- (hi box 1) (lo box 1)) (- (hi box 2) (lo box 2))
                                     (lo box 0) (hi box 0) (lo box 1) (hi box 1) (lo box 2) (hi box 2)
                                     (when (member (type-of leaf) '(cylinder cone) :test #'(lambda (a b) (string-equal (symbol-name a) (symbol-name b))))
                                       (ignore-errors (axis-word (the-object leaf (face-normal-vector :rear)))))
                                     (and floor-z (<= (abs (- (lo box 2) floor-z)) 0.5))
                                     (reverse touching) (reverse overlapping)
                                     (and floor-z (null touching) (null overlapping) (> n 1)
                                          (> (- (lo box 2) floor-z) 0.5))))))
                 ;; symmetry: a part with a twin of its type and size, mirrored
                 ;; across the envelope's middle
                 (when env
                   (flet ((twins (axis)
                            (let ((mid (/ (+ (lo env axis) (hi env axis)) 2)))
                              (loop for leaf in leaves for box in boxes
                                    count (and box
                                               (find-if #'(lambda (pair)
                                                            (let ((other (cdr pair)))
                                                              (and other (eq (type-of (car pair)) (type-of leaf))
                                                                   (loop for a below 3
                                                                         always (< (abs (- (- (hi box a) (lo box a)) (- (hi other a) (lo other a)))) 1))
                                                                   (< (abs (- (+ (lo box axis) (hi box axis) (lo other axis) (hi other axis))
                                                                              (* 4 mid)))
                                                                      2)
                                                                   (loop for a below 3
                                                                         always (or (= a axis)
                                                                                    (< (abs (- (+ (lo box a) (hi box a)) (+ (lo other a) (hi other a)))) 2))))))
                                                        (mapcar #'cons leaves boxes)))))))
                     (say "Symmetry: ~d of ~d parts have a mirror twin across X (left-right), ~d across Y (front-back)."
                          (twins 0) n (twins 1)))))))
            (values (list (text-result "~a" (clip (format nil "~{~a~^~%~}" (reverse lines))))) nil))))
    (serious-condition (condition)
      (values (list (text-result "MODEL does not build with default inputs: ~a" condition)) t))))

(defparameter *house-view?* t
  "Boolean.  Whether a house session is offered view_model, its text view.")

(defun house-tool-definitions ()
  "The structural edit tools and view_model, for a house session."
  (remove-if
   #'(lambda (tool) (and (not *house-view?*) (equal (cdr (assoc "name" tool :test #'string=)) "view_model")))
   (list
   `(("name" . "set_slot")
     ("description" . "Change ONE slot of a define-object by name -- an input, a computed slot, a child under :objects, or a function: the tool writes (SLOT VALUE) in place of the old entry and compiles, so you write only VALUE and never the parentheses around it. For a child, VALUE is its keyword inputs, e.g. \":type 'box :width (the width) :center (translate (the center) :up 10)\". For a function, VALUE is \"(args) body\". VALUE must balance; it is checked before anything is written. SLOT is never a section's name (objects, input-slots ...): to change a child named shelf, SLOT is \"shelf\".")
     ("input_schema" . ,(schema `(("object" . (("type" . "string") ("description" . "The define-object's name, e.g. model.")))
                                  ("slot" . (("type" . "string") ("description" . "The slot's, child's or function's name.")))
                                  ("value" . (("type" . "string") ("description" . "What follows the name inside the entry."))))
                                '("object" "slot" "value"))))
   `(("name" . "add_slot")
     ("description" . "Add ONE entry to a section of a define-object -- input-slots, computed-slots, objects, hidden-objects or functions -- making the section when it has none. You give the name and VALUE (as for set_slot); the tool writes the parentheses. A name already used in the object is refused. A CHILD OBJECT is one call per child: section \"objects\", slot the child's own name, value its type and inputs, e.g. {\"object\": \"model\", \"section\": \"objects\", \"slot\": \"shelf\", \"value\": \":type 'box :width (the width) :length 300 :height 18 :center (translate (the center) :up 100)\"} -- never a list of children, never the child in parentheses.")
     ("input_schema" . ,(schema `(("object" . (("type" . "string")))
                                  ("section" . (("type" . "string") ("description" . "input-slots, computed-slots, objects, hidden-objects or functions.")))
                                  ("slot" . (("type" . "string")))
                                  ("value" . (("type" . "string"))))
                                '("object" "section" "slot" "value"))))
   `(("name" . "remove_slot")
     ("description" . "Remove ONE entry -- input, computed slot, child or function -- from a define-object, by name.")
     ("input_schema" . ,(schema `(("object" . (("type" . "string"))) ("slot" . (("type" . "string"))))
                                '("object" "slot"))))
   `(("name" . "replace_form")
     ("description" . "Replace a whole top-level form -- (define-object NAME ...), (defun NAME ...) -- by its NAME with SOURCE, one form that defines NAME; with no such form, SOURCE is added to the file. SOURCE must balance and is checked before anything is written. For a change inside a define-object, set_slot is smaller and safer.")
     ("input_schema" . ,(schema `(("name" . (("type" . "string")))
                                  ("source" . (("type" . "string") ("description" . "One whole form defining NAME."))))
                                '("name" "source"))))
   `(("name" . "view_model")
     ("description" . "See the built model, in words: MODEL's envelope; every part's type, size and place; what touches and what OVERLAPS what; which parts FLOAT (touch nothing, off the floor); cylinders' axes; left-right and front-back symmetry. Use it after check_model to judge whether the parts are where the request wants them, then fix what it shows.")
     ("input_schema" . ,(schema nil))))))

(defparameter *look-question*
  "This is a wireframe line drawing of a 3D model built for this design request:~%~%~a~%~%The drawing shows an isometric view and front, top and side views.  In under 200 words, say what you see: the parts, where each sits relative to the others, and anything that floats, passes through another part, is turned the wrong way, is missing, or is out of proportion for the request.  Be concrete; do not praise."
  "What the viewer is asked of a drawing (look_at_model); ~a is the request.")

(defun latest-request (session)
  "The visitor's latest prompt: the last text block of the last user turn
that has one."
  (dolist (message (reverse (session-messages session)) "")
    (when (and (hash-table-p message) (equal (gethash "role" message) "user"))
      (let ((content (gethash "content" message)))
        (cond ((stringp content) (return content))
              (t (let ((text (find-if #'(lambda (block) (and (hash-table-p block) (equal (gethash "type" block) "text")))
                                      (reverse (coerce content 'list)))))
                   (when text (return (gethash "text" text))))))))))

(defun look-at-model (session)
  "Render SESSION's model and have *house-viewer-model* describe the
drawing against the request; answer its words."
  (multiple-value-bind (blocks error?) (render session :layout "isometric-plus-ortho")
    (if error?
        (values blocks t)
        (handler-case
            (let ((body (encode (h "model" *house-viewer-model* "max_tokens" 1024
                                   "messages" (list (h "role" "user"
                                                       "content" (list (->json (first blocks))
                                                                       (h "type" "text"
                                                                          "text" (format nil *look-question* (latest-request session))))))))))
              (multiple-value-bind (status text)
                  ;; read at run time: *messages-url* is agent.lisp's, which loads after this file
                  (post-json (symbol-value '*messages-url*) body
                             :headers (list (cons "anthropic-version" "2023-06-01")
                                            (cons *session-header* (session-id session)))
                             :seconds 180)
                (let ((json (ignore-errors (yason:parse text))))
                  (if (and (eql status 200) (hash-table-p json))
                      (values (list (text-result "What a viewer sees in a drawing of your model:~%~{~a~}"
                                                 (loop for block in (coerce (gethash "content" json) 'list)
                                                       when (and (hash-table-p block) (equal (gethash "type" block) "text"))
                                                         collect (gethash "text" block))))
                              nil)
                      (values (list (text-result "The viewer did not answer (~a); judge by check_model and view_model." status)) t)))))
          (serious-condition (condition)
            (values (list (text-result "The viewer could not be asked: ~a" condition)) t))))))

(defun look-tool-definition ()
  `(("name" . "look_at_model")
    ("description" . "Have a vision model look at a drawing of your built model (isometric, front, top and side) and tell you in words what it sees against the request: what is where, what floats, passes through another part, is turned the wrong way or is missing. Use it after check_model and view_model, then fix what it reports.")
    ("input_schema" . ,(schema nil))))

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
