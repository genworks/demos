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
    (multiple-value-bind (blocks error?) (load-model-file session)
      ;; an agent's file that iterates with loop, or names vanilla-mixin, is
      ;; told so, with the lines (the iteration and mixin rules, agent.lisp);
      ;; a person's own edit is not
      (values (if (member *model-writer* '(:agent :own-agent))
                  (append blocks (house-notes source))
                  blocks)
              error?))))

(defparameter *loop-note*
  "NOTE: this file uses `loop` (line~p ~{~a~^, ~}).  The house writes no `loop` (the iteration rule in your instructions): rewrite each one in the plain forms, keeping its time and space of the same order."
  "String, a format control taking the count and the line numbers.  What write_model and
edit_model add to their result when an agent's file uses loop.")

(defparameter *vanilla-note*
  "NOTE: this file names `vanilla-mixin` (line~p ~{~a~^, ~}).  Every object has it already (the mixin rule in your instructions): drop it from the mixin list; a plain object is (define-object name () ...)."
  "String, a format control taking the count and the line numbers.  What
write_model and edit_model add to their result when an agent's file names
vanilla-mixin.")

(defun house-notes (source)
  "The notes on SOURCE for the agent that wrote it: one text block per
house rule it breaks (*loop-note*, *vanilla-note*), naming the lines."
  (mapcan #'(lambda (note)
              (let ((lines (code-lines source (rest note))))
                (when lines
                  (list (text-result (first note) (length lines) lines)))))
          (list (cons *loop-note* #'loop-form-p)
                (cons *vanilla-note* #'vanilla-mixin-p))))

(defun code-lines (source test)
  "The lines of SOURCE (1 the first) whose code -- the part before any `;`
-- satisfies TEST: a hint for the agent, not a parse (a match in a string
counts)."
  (do* ((n 1 (1+ n))
        (start 0 (1+ end))
        (end (position #\Newline source :start start)
             (position #\Newline source :start start))
        (hits nil))
       (nil)
    (let* ((line (subseq source start (or end (length source))))
           (code (subseq line 0 (or (position #\; line) (length line)))))
      (when (funcall test code) (push n hits)))
    (unless end (return (reverse hits)))))

(defun loop-form-p (code)
  "True when CODE holds a (loop form: (loop then a space, a tab, ) or the
end."
  (let ((pos (search "(loop" code)))
    (and pos
         (let ((after (+ pos 5)))
           (or (>= after (length code))
               (member (char code after) '(#\Space #\Tab #\) #\Return)))))))

(defun vanilla-mixin-p (code)
  (search "vanilla-mixin" code :test #'char-equal))

(defun loop-lines (source)
  "The lines of SOURCE holding a (loop form outside a line comment."
  (code-lines source #'loop-form-p))

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
        (warnings nil)
        ;; code in the file may name its own uploaded files as it loads (own-file)
        (*loading-session* session))
    (archive-model! session)
    ;; what the old file charged for is no longer what this one does (deploy.lisp)
    (forget-built-tolls! session)
    ;; a compile costs credits by the volume of the file (meter.lisp)
    (multiple-value-bind (ok? reason)
        (meter! session :compile
                (or (ignore-errors (symbol-volume (uiop:read-file-string file :external-format :utf-8)
                                                  (session-package session)))
                    0))
      (unless ok?
        ;; written and not compiled: the file is not what is loaded (the
        ;; completion check reads this, model-load-failure)
        (note-model-load! session "the compile was refused, so the file was not loaded")
        (return-from load-model-file (meter-refusal reason))))
    ;; no instance made while the definition goes in (gate.lisp): the
    ;; compile as well as the load, since define-object defines its class
    ;; at compile time too.  The gate is taken OUTSIDE the time limit, so
    ;; the limit can never fire while the gate is being waited for
    (with-model-exclusive
    (handler-case
        (with-time-limit (*load-seconds* "compile and load")
          (let ((fasl (with-model-exclusive
                        (handler-bind ((warning #'(lambda (w)
                                                    (push (princ-to-string w) warnings)
                                                    (muffle-warning w))))
                          (let ((*package* (session-package session)))
                            (compile-file file :output-file (make-pathname :type "fasl"
                                                                           :defaults file)))))))
            (unless fasl (error "The compiler produced no output file."))
            (with-model-exclusive
              (let ((*package* (session-package session)))
                (load fasl))
              ;; and the new class's first dispatches made here, one thread,
              ;; before the gate lets readers in: several threads filling a
              ;; class's dispatch caches at once is the same fault's other
              ;; door ("Generic-function dispatch bug!", 1 in 2,000 without)
              (when *load-gate?*
                (let ((symbol (built-symbol session)))
                  (when (and symbol (find-class symbol nil))
                    (ignore-errors (the-object (make-object symbol) children))))))
            ;; the file as it stands is what is loaded
            (note-model-load! session nil)
            ;; MODEL, or for a session that builds a web app, APP (kinds.lisp)
            (let ((defined? (let ((symbol (built-symbol session)))
                              (and symbol (find-class symbol nil) t))))
              (values (list (text-result "Compiled and loaded.~:[  No object named ~a is defined!~;~*~]~@[~%Warnings:~%~{- ~a~%~}~]"
                                        defined? (built-name session) (reverse warnings)))
                      (not defined?)))))
      ;; a stack overflow too (see evaluate-expression)
      (serious-condition (condition)
        ;; the file stays as written; the image keeps the last class that loaded
        (note-model-load! session (condition-text condition))
        (values (list (text-result "Error compiling or loading the model file: ~a~@[~%Warnings first:~%~{- ~a~%~}~]~@[~%~a~]"
                                  condition (reverse warnings)
                                  ;; a house model loses count of its parentheses
                                  ;; (agent.lisp): say where, which the reader's
                                  ;; 'unexpected end of file' does not
                                  (and (house-model? session)
                                       (ignore-errors (paren-report (file-model-body file))))))
                t))))))

(defvar *rewrite-advice* "Write the whole file again with write_model"
  "What a refusal for unbalanced or unreadable text tells the model to do,
bound by run-tool for the session at hand (rewrite-advice): a structure-only
session has no write_model to send it to.")

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
                            (format nil "The parentheses do not balance: an extra ')' on line ~d closes more than was opened.  ~a." line *rewrite-advice*))))))
               (incf i))
      (when (plusp depth)
        (format nil "The parentheses do not balance: ~d '(' left open at the end of the file.  The top-level form starting on line ~d, ~s, is not closed.  ~a.  Close each child form on its own last line, then ')' for its section and ')' for the define-object."
                depth form-line form-head *rewrite-advice*)))))

(defun rewrite-advice (session)
  (if (structure-only? session)
      "Send it again balanced: entry by entry with add_slot and set_slot, or one whole define-object with replace_form once two structural edits of it have failed"
      "Write the whole file again with write_model"))

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
      (flet ((try (form)
               ;; values: content blocks, an error flag, and the condition
               (let (result)
                 (handler-case
                     (let ((output (with-output-to-string (*standard-output*)
                                     ;; an expression may define a class too (gate.lisp);
                                     ;; the gate outside the time limit, as for a load
                                     (with-model-exclusive
                                       (with-time-limit (*eval-seconds* "evaluation")
                                         (setq result (eval form)))))))
                       (values (list (let ((*print-length* 50) (*print-level* 6))
                                       (text-result "~s~@[~%Output:~%~a~]"
                                                   result (when (plusp (length output)) output))))
                               nil))
                   ;; serious-condition, not error: a model that recurses into
                   ;; itself overflows the stack, a storage-condition, which took
                   ;; the whole Lisp down when only errors were caught (2026-11-14)
                   (serious-condition (condition)
                     (values (list (text-result "Error: ~a" condition)) t condition))))))
        (multiple-value-bind (blocks error? condition) (try form)
          ;; the :evaluate-self rail: (the ...) outside an object fails for
          ;; want of SELF -- the commonest evaluate error of a house model.
          ;; It is run again with SELF a fresh MODEL, and the result says so.
          (if (and error? (rail? :evaluate-self)
                   (typep condition 'unbound-variable)
                   (string-equal (symbol-name (cell-error-name condition)) "SELF")
                   (model-defined? session))
              (let ((model (ignore-errors (make-model session))))
                (cond (model
                       (rail-note "NO SELF HERE: evaluate runs outside any object, so the expression was run again with SELF a fresh MODEL: (the x) read MODEL's x.")
                       (multiple-value-bind (blocks error?)
                           (try `(let ((self ,model)) (declare (ignorable self)) ,form))
                         (values blocks error?)))
                      (t (values blocks error?))))
              (values blocks error?)))))))


;;
;; check_model: numbers the harness computes itself, so the agent's own
;; opinion of its work is not the only check.
;;

(defun leaf-box (leaf)
  (ignore-errors (the-object leaf bounding-box)))

(defparameter *completion-check?* t
  "Boolean.  Whether a reply that ends a prompt is checked before it counts
as done (Ruling 144, 2026-11-16): MODEL builds, every part computes, and
every computed slot of MODEL computes.  A reply that fails it is logged as
not done, with what the check found.  Nil: the agent's word, as before.")

;; THE FILE AND THE CLASS.  A model file that fails to compile or load stays
;; on disk as written while the image keeps the last class that loaded, so a
;; check of the class alone called a session done whose file -- what the
;; visitor has, the archive keeps and a deployment compiles -- does not
;; build.  load-model-file notes each outcome here, beside the session (no
;; slot in the struct: sessions outlive a reload), and the check reads it.

(defvar *model-load-failures* (make-hash-table :test #'equal)
  "session package name -> why the model file as it now stands is not what
is loaded: the text of its failed compile or load.")

(defvar *model-load-lock* (bt:make-lock "prompt-lab model loads"))

(defun note-model-load! (session failure)
  "Keep whether SESSION's model file as last written is what is loaded:
FAILURE is nil when it compiled and loaded, else why it did not."
  (bt:with-lock-held (*model-load-lock*)
    (if failure
        (setf (gethash (session-package-name session) *model-load-failures*) failure)
        (remhash (session-package-name session) *model-load-failures*))))

(defun model-load-failure (session)
  "Why SESSION's model file as it now stands is not what is loaded, or nil."
  (bt:with-lock-held (*model-load-lock*)
    (values (gethash (session-package-name session) *model-load-failures*))))

;;
;; NAMED RESULTS (2026-11-17).  The completion check cannot read which
;; results a request names: it checks that what MODEL declares computes.  A
;; trial's unsound builds were, more often than anything else, models the
;; check passed that lacked results the request had asked for.  So for a
;; house session the lab may ask first which results the request names
;; (agent.lisp, extract-results) and declare each in MODEL as a computed
;; slot that signals until it is written.  The check, the findings sent
;; with every edit and the done gate then see them like any slot that does
;; not compute.
;;

(defparameter *house-named-results* nil
  "Boolean.  Whether a house session's model build has the results its
request names declared first: listed by one call to the session's model
and written into MODEL as slots that signal until the agent writes them.")

(defvar *session-results* (make-hash-table :test #'equal)
  "Session id -> the results declared for it, a list of (NAME . LABEL):
the slot's name and what the request calls it.")

(defvar *session-results-lock* (bt:make-lock "prompt-lab declared results"))

(defun session-results (session)
  "The results declared for SESSION, a list of (NAME . LABEL)."
  (bt:with-lock-held (*session-results-lock*)
    (values (gethash (session-id session) *session-results*))))

(defun set-session-results! (session results)
  "Keep RESULTS, a list of (NAME . LABEL), as SESSION's declared results;
nil forgets them."
  (bt:with-lock-held (*session-results-lock*)
    (if results
        (setf (gethash (session-id session) *session-results*) results)
        (remhash (session-id session) *session-results*)))
  results)

(defparameter *result-stub-words* "Not written yet"
  "How the message of a declared result's stub begins.")

(defun result-stub-value (label)
  "The value a declared result has until the agent writes it: an error
that says so."
  (format nil "(error ~s)"
          (format nil "~a: ~a.  Give this slot its expression with set_slot." *result-stub-words* label)))

(defun unwritten-results (session model)
  "The names declared for SESSION that MODEL, an instance, has no message
for: a stub replaced along with the whole define-object, or removed."
  (let ((declared (and (not (session-replay? session)) (session-results session))))
    (when declared
      (let ((have (ignore-errors (the-object model (message-list :message-type :local)))))
        (loop for (name . nil) in declared
              unless (member name have :test #'string-equal)
                collect name)))))

(defun geometric? (object)
  "Whether OBJECT has geometry to judge: a child whose type has no
base-object in it (a holder of numbers) has no bounding box and is no
'part whose geometry does not compute'."
  (typep object 'base-object))

(defun empty-model? (model leaves slots)
  "Whether MODEL builds nothing yet: no child, no computed slot of its own
and no size -- the skeleton a structure-only session starts from, or a
define-object left bare.  LEAVES and SLOTS are MODEL's, already asked."
  (and (null slots) (partless-model? model leaves)))

(defun partless-model? (model leaves)
  "Whether MODEL has nothing to draw: no child, and no size of its own (a
MODEL that is itself a box, with no children, has a part).  A geometry
request answered with numbers alone is not done."
  (and (null (rest leaves))
       (or (null leaves) (eq (first leaves) model))
       (let ((box (leaf-box model)))
         (or (null box)
             (every #'(lambda (low high) (< (abs (- high low)) 1e-9)) (first box) (second box))))))

(defun completion-findings (session)
  "What the completion check (*completion-check?*) finds, by name: a plist
(:broken PART-NAMES :failing SLOT-NAMES :error TEXT :empty BOOLEAN
:unjudged TEXT), or nil when there is nothing to judge -- no MODEL
defined, or an app or project session.  Everything empty means complete.
:error also when the model FILE as it now stands failed to compile or load
(model-load-failure): the class in the image is then an older version, and
judging it would call a file done that does not build.  :empty for a MODEL
with nothing in it.  :unjudged when the check itself ran out of time: that
says nothing of the model either way."
  (when (and *completion-check?*
             (eq (session-kind session) :model)
             (model-defined? session))
    (let ((stale (model-load-failure session)))
      (if stale
          (list :error (format nil "the model file as it now stands did not compile or load, so what is loaded is an older version (~a)"
                               (subseq stale 0 (min 300 (length stale)))))
          (handler-case
              (with-deadline (*eval-seconds*)
                (let* ((model (make-model session))
                       (leaves (the-object model leaves))
                       (slots (ignore-errors
                               (the-object model (message-list :category :computed-slots
                                                               :message-type :local)))))
                  ;; each failing slot with why it fails, for the :findings rail
                  (let ((why (loop for key in slots
                                   for reason = (handler-case (progn (the-object model (evaluate key)) nil)
                                                  (error (condition)
                                                    (or (ignore-errors (condition-text condition)) "an error")))
                                   when reason collect (cons (string-downcase (symbol-name key)) reason))))
                    (list :broken (loop for leaf in leaves
                                        unless (or (not (geometric? leaf)) (leaf-box leaf))
                                          collect (leaf-name leaf))
                          :failing (mapcar #'car why)
                          :why why
                          :empty (empty-model? model leaves slots)
                          ;; slots, perhaps, but nothing to draw
                          :no-parts (partless-model? model leaves)
                          ;; results declared for the request that MODEL has lost
                          :unwritten (unwritten-results session model)))))
            ;; the check's own limit, not the model's fault: a sound model
            ;; with one slow slot was reported as one that does not build
            (bt2:timeout ()
              (list :unjudged (format nil "the completion check took longer than ~a seconds and was stopped"
                                      *eval-seconds*)))
            (serious-condition (condition)
              (list :error (condition-text condition))))))))

(defun completion-problems (session)
  "What keeps SESSION's model from being done, as short texts; nil when it
is complete by the check (*completion-check?*): the model file as it stands
is what is loaded, MODEL builds and is not empty, every part computes, every
computed slot of MODEL computes.  Nil too when nothing was built -- no MODEL
defined -- or the session builds an app or a project, which this check does
not judge; and nil, with a note in the session's log, when the check ran out
of time and judged nothing (the agent's word then stands).  It cannot read
which results a request names: it checks that the results MODEL declares
compute -- and, where the lab declared the request's results first
(*house-named-results*), that MODEL still has each of them."
  (let ((findings (completion-findings session)))
    (when (getf findings :unjudged)
      (log-event session :note "The lab's check of the model judged nothing: ~a.  The reply stands as the agent's word."
                 (getf findings :unjudged)))
    (flet ((some-of (names)
             (format nil "~{~a~^, ~}~@[ and ~d more~]"
                     (subseq names 0 (min 8 (length names)))
                     (when (> (length names) 8) (- (length names) 8)))))
      (append (when (getf findings :error)
                (list (format nil "MODEL does not build: ~a" (getf findings :error))))
              (cond ((getf findings :empty)
                     (list "MODEL is empty: nothing has been built yet"))
                    ;; a geometry request answered with numbers alone
                    ((getf findings :no-parts)
                     (list "MODEL has no parts: there is nothing to draw yet (its slots are not parts)")))
              (when (getf findings :broken)
                (list (format nil "parts whose geometry does not compute: ~a" (some-of (getf findings :broken)))))
              (when (getf findings :failing)
                (list (format nil "computed slots of MODEL that do not compute -- results and helpers alike, since a broken helper is a broken model: ~a" (some-of (getf findings :failing)))))
              (when (getf findings :unwritten)
                (list (format nil "results the request names that MODEL does not have -- add each as a computed slot of MODEL under this name: ~a" (some-of (getf findings :unwritten)))))))))

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

;;
;; RAILS FOR A HOUSE MODEL (2026-11-16).  A count of 609 tool errors in the
;; structural edits of thirteen trial arms found most of them of four kinds a
;; tool can take care of itself, each a round spent for nothing: a value
;; whose only fault is its closing count (187), a name that is already there
;; (82), a common Gendl mistake the error text does not explain, and a build
;; that ended 'clean' with slots that do not compute.  Only a house session
;; rides them, and only those *house-rails* names (T, the default: all):
;;
;;   :paren-repair  a VALUE or SOURCE left open, or closed too often, at its
;;                  END is balanced by the tool and used; the result says so
;;   :upsert        add_slot on a slot that is there changes it; new_object_def
;;                  on an object that is there leaves it; the result says so
;;   :hints         a tool error gains a HINT for the mistake behind it
;;   :findings      every edit that compiles is answered with what the
;;                  completion check finds now
;;
;; and a second pack, from what the first trial's errors still were:
;;
;;   :evaluate-self an evaluate that fails for want of SELF -- (the x)
;;                  outside any object -- is run again with SELF a fresh MODEL
;;   :set-adds      set_slot on a slot that is not there adds it, unless a
;;                  name near it is: then it is still refused, as a slip
;;   :unquote       a VALUE that is one string holding code has its own
;;                  pair of quotes taken off
;;   :show-entry    an edit is answered with each changed entry as the file
;;                  now has it
;;

(defparameter *rail-names* '(:paren-repair :upsert :hints :findings
                             :evaluate-self :set-adds :unquote :show-entry)
  "Every rail *house-rails* can name.")

(defparameter *house-rails* t
  "Nil, T or a list of rails (*rail-names*) a house session rides; T is all
of them, the default since a trial of four runs found the rails doing their
own work at no cost.  Nil: the tools as they were.")

(defvar *rails* nil
  "The rails in force for the tool call being run; run-tool binds it.")

(defvar *rail-notes* nil
  "What the rails did during the tool call being run, newest first: texts
run-tool appends to the call's result.")

(defparameter *later-rail-names* '(:refusal-run :glime)
  "Rails built since *rail-names* and not yet measured in a trial.  T does
not turn them on; a list that names one does, with :all in it for every
rail of *rail-names* beside.
  :refusal-run   a whole define-object sent to replace_form and refused
                 several times in a row (*refusal-run-length*) is answered
                 plainly: it will be refused again, and what to call
                 instead (*refusal-run-text*)
  :glime         an edit that compiles is answered with Gendl's reference
                 check over the model file: each (the ...) or (the-child ...)
                 whose first message the object and its mixins do not
                 answer (glime-findings-blocks)")

(defparameter *structural-tool-names* '("set_slot" "add_slot" "remove_slot" "new_object_def" "replace_form")
  "The tools that edit the model file by structure.")

(defun house-rails ()
  "The rails *house-rails* names, a list: T is *rail-names*; in a list,
:all stands for those."
  (cond ((eq *house-rails* t) *rail-names*)
        ((listp *house-rails*)
         (if (member :all *house-rails*)
             (union *rail-names* (remove :all *house-rails*))
             *house-rails*))))

(defun session-rails (session)
  "The rails SESSION rides: *house-rails* for a house session, else none."
  (and (house-model? session) (house-rails)))

(defun rail? (rail) (and (member rail *rails*) t))

(defun rail-note (control &rest args)
  (push (apply #'format nil control args) *rail-notes*))

(defvar *touched-entries* nil
  "The entries the tool call being run has written to the file, newest
first, each (object slot): what the :show-entry rail shows.")

(defun touch-entry (object slot)
  (push (list object slot) *touched-entries*))

(defun run-tool (session name input)
  "Run tool NAME with INPUT (an alist, keys as strings or keywords) on
SESSION, its refusals advising what this session can do.  Values: content
blocks, and true on failure.  For a house session on rails the result ends
with what the rails did (*rail-notes*) and, for a failure, any hints."
  (let ((*rewrite-advice* (rewrite-advice session))
        (*rails* (session-rails session))
        (*rail-notes* nil)
        (*touched-entries* nil))
    (multiple-value-bind (blocks error?) (%run-tool session name input)
      ;; an edit that went through ends a run of refused rewrites (below)
      (when (and (not error?) (member name *structural-tool-names* :test #'string=))
        (note-whole-refusal! session nil))
      (values (append blocks
                      (mapcar #'(lambda (note) (text-result "~a" note)) (reverse *rail-notes*))
                      (and (rail? :show-entry) *touched-entries*
                           (ignore-errors (touched-blocks session (reverse *touched-entries*))))
                      (and error? (rail? :hints) (ignore-errors (hint-blocks session blocks))))
              error?))))

(defun %run-tool (session name input)
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
        ((string= name "set_slot")
         (multiple-value-call #'note-structural-call session (input "object" input)
           (set-slot-tool session (input "object" input) (input "slot" input) (input "value" input))))
        ((string= name "add_slot")
         (multiple-value-call #'note-structural-call session (input "object" input)
           (add-slot-tool session (input "object" input) (input "section" input)
                          (input "slot" input) (input "value" input) (input "entries" input))))
        ((string= name "remove_slot")
         (multiple-value-call #'note-structural-call session (input "object" input)
           (remove-slot-tool session (input "object" input) (input "slot" input))))
        ((string= name "new_object_def")
         (new-object-def-tool session (input "name" input) (input "mixins" input) (input "description" input)
                              (input "entries" input)))
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
  "T, nil or :only.  T: a house session edits by structure and has view_model
(new_object_def, set_slot, add_slot, remove_slot, replace_form; edit_model
withdrawn).  :only: the same with write_model withdrawn too -- MODEL starts
as an empty skeleton, and a whole define-object is replaced only after two
structural edits of it have failed.  Nil: write_model and edit_model, as for
any model.")

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
      ;; building by structure alone: no whole-file write either
      (when (structure-only? session)
        (setq tools (without "write_model" tools)))
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
     ("description" . "List the files the visitor has uploaded to this session: each one's name, kind, size and path on the host. A PDF or an image also arrives in the conversation with the visitor's message; a text file (DXF, SVG, CSV, STEP...) is read with read_file, or by its path from the model. In the model, never write the path itself: write (prompt-lab:own-file \"<name>\") for a file's path and (prompt-lab:own-files-directory) for the directory, which find the files wherever the model or web app runs, deployed included.")
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

;;
;; The :paren-repair rail.
;;

(defparameter *paren-repair-most* 8
  "Integer. The most parentheses the :paren-repair rail adds to or drops
from the end of a fragment; a fragment further out than that is refused as
before.")

(defun paren-balance (text)
  "How TEXT's parentheses stand: the depth it ends at, the lowest depth it
reaches (negative when a ')' closes more than was opened), and true when it
ends inside a string.  Strings, comments and character literals are skipped,
as paren-report skips them."
  (let ((depth 0) (low 0) (i 0) (n (length text)) (in-string? nil))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((char= c #\;) (setq i (or (position #\Newline text :start i) n)))
                     ((char= c #\")
                      (incf i)
                      (loop while (and (< i n) (char/= (char text i) #\"))
                            do (when (char= (char text i) #\\) (incf i))
                               (incf i))
                      (when (>= i n) (setq in-string? t)))
                     ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\\)) (incf i 2))
                     ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                      (setq i (1+ (or (search "|#" text :start2 (+ i 2)) n))))
                     ((char= c #\() (incf depth))
                     ((char= c #\)) (decf depth) (setq low (min low depth)))))
             (incf i))
    (values depth low in-string?)))

(defun balanced-fragment (text)
  "TEXT with its closing count put right, when that is its only fault and it
is at the END: left open, the missing ')' are added; closed too often, the
extra ')' are dropped.  Values: the mended text and what was done, in words;
nil when TEXT balances already or cannot be mended this way (a ')' too many
in its middle, a string left open, more than *paren-repair-most* out)."
  (when (and (stringp text) (paren-report text))
    (multiple-value-bind (end low in-string?) (paren-balance text)
      (let ((trimmed (string-right-trim '(#\Space #\Tab #\Newline #\Return) text)))
        (flet ((good? (candidate)
                 (and (null (paren-report candidate)) (nth-value 1 (top-forms candidate)))))
          (cond (in-string? nil)
                ((and (plusp end) (>= low 0) (<= end *paren-repair-most*))
                 (let ((candidate (concatenate 'string trimmed (make-string end :initial-element #\)))))
                   (and (good? candidate)
                        (values candidate
                                (format nil "it left ~d '(' open, so ~d ')' ~:[was~;were~] added at its end"
                                        end end (> end 1))))))
                ((minusp low)
                 (loop for drop from 1 to *paren-repair-most*
                       for cut = (- (length trimmed) drop)
                       while (and (>= cut 0) (char= (char trimmed cut) #\)))
                       do (let ((candidate (subseq trimmed 0 cut)))
                            (when (good? candidate)
                              (return (values candidate
                                              (format nil "it closed ~d ')' more than it opened, so ~:*~d ~:[was~;were~] dropped from its end"
                                                      drop (> drop 1))))))))))))))

(defun mended (fragment what)
  "FRAGMENT as a structural tool will take it: under the :paren-repair rail,
with a wrong closing count at its end put right and a note of it kept for
the result; else as given.  WHAT names it in the note (\"VALUE of width\")."
  (if (and (rail? :paren-repair) (stringp fragment))
      (multiple-value-bind (fixed how) (balanced-fragment fragment)
        (cond (fixed
               (rail-note "REPAIRED: ~a did not balance: ~a.  It was taken as ending ~s.  If a parenthesis belonged somewhere else, send it again."
                          what how (subseq fixed (max 0 (- (length fixed) 50))))
               fixed)
              (t fragment)))
      fragment))

;;
;; The :unquote rail.  A house model now and then sends a VALUE wrapped in
;; its own pair of double quotes: the entry then holds one string where code
;; belongs, and the compile fails on its type some rounds later.
;;

(defun string-form-contents (text)
  "When TEXT is exactly one Lisp string literal, what it holds, its escapes
undone; else nil."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (when (and (> (length trimmed) 1) (char= (char trimmed 0) #\")
               (eql (form-end trimmed 0) (length trimmed)))
      (let ((last (1- (length trimmed))))
        (with-output-to-string (out)
          (loop with i = 1
                while (< i last)
                do (when (and (char= (char trimmed i) #\\) (< (1+ i) last)) (incf i))
                   (write-char (char trimmed i) out)
                   (incf i)))))))

(defun unquoted (value section what)
  "VALUE as a structural tool will take it: under the :unquote rail, with
its own pair of double quotes taken off when the whole of it is one string
that holds code -- always for a child or a function (SECTION; a string is
never one), for an input or a computed slot only when what it holds begins
like code, with '(' or ':'.  A note of it is kept for the result."
  (if (and (rail? :unquote) (stringp value))
      (let* ((inside (string-form-contents value))
             (start (and inside (string-left-trim '(#\Space #\Tab #\Newline #\Return) inside))))
        (cond ((and start (plusp (length start))
                    (or (member section '(":objects" ":hidden-objects" ":functions") :test #'equal)
                        (find (char start 0) "(:")))
               (rail-note "UNWRAPPED: ~a came wrapped in its own double quotes, which would have made it one string and not code; the quotes were taken off."
                          what)
               inside)
              (t value)))
      value))

;;
;; The :show-entry rail.
;;

(defparameter *show-entries-most* 8
  "Integer. The most changed entries one edit's result shows.")

(defun object-slot-names (text elements)
  "The names of every entry of the define-object whose ELEMENTS are given,
lower case."
  (declare (special *slot-sections*))   ; defined with the structural tools, below
  (loop for (section . list-span) in (object-sections text elements)
        when (member section *slot-sections* :test #'string=)
          append (loop for entry in (form-elements text (car list-span))
                       for name = (if (char= (char text (car entry)) #\()
                                      (slot-entry-name text entry)
                                      (and (char/= (char text (car entry)) #\")
                                           (string-downcase (span-text text entry))))
                       when name collect name)))

(defun touched-blocks (session touched)
  "The entries of TOUCHED -- lists (object slot), the oldest first -- as the
model file now has them, one content block; nil when none is to be found."
  (let* ((text (model-body-text session))
         (pairs (remove-duplicates touched :test #'equalp :from-end t))
         (lines (loop for (object slot) in pairs
                      for found = (and (stringp object) (stringp slot)
                                       (definitions-named text object '("define-object")))
                      for entry = (and (= (length found) 1)
                                       (nth-value 1 (find-slot text (second (first found)) slot)))
                      when entry
                        collect (let ((source (span-text text entry)))
                                  (format nil "in ~a: ~a" object
                                          (if (> (length source) 600)
                                              (concatenate 'string (subseq source 0 600) " ...")
                                              source))))))
    (when lines
      (list (text-result "AS WRITTEN, ~d entr~:@p in the file now~@[ (the first ~d shown)~]:~%~{~a~^~%~}"
                         (length lines)
                         (and (> (length lines) *show-entries-most*) *show-entries-most*)
                         (subseq lines 0 (min *show-entries-most* (length lines))))))))

;;
;; The :hints rail: what the common mistakes behind a tool error are, said
;; with the error.  Each test reads the error's text; nothing is guessed
;; from the request.
;;

(defun edit-distance (a b)
  "How many single-character changes turn string A into B."
  (let ((row (make-array (1+ (length b)))))
    (dotimes (j (1+ (length b))) (setf (aref row j) j))
    (dotimes (i (length a))
      (let ((diagonal (aref row 0)))
        (setf (aref row 0) (1+ i))
        (dotimes (j (length b))
          (let ((above (aref row (1+ j))))
            (setf (aref row (1+ j))
                  (min (1+ above) (1+ (aref row j))
                       (+ diagonal (if (char= (char a i) (char b j)) 0 1))))
            (setq diagonal above)))))
    (aref row (length b))))

(defun nearest-names (name candidates &key (most 3))
  "The CANDIDATES (strings) nearest NAME, at most MOST, the nearest first:
within a third of NAME's length in changes, or one holding the other."
  (let* ((name (string-downcase name))
         (limit (max 2 (floor (length name) 3)))
         (scored (loop for candidate in (remove-duplicates candidates :test #'string=)
                       for distance = (if (and (>= (min (length name) (length candidate)) 4)
                                               (or (search name candidate) (search candidate name)))
                                          1
                                          (edit-distance name candidate))
                       when (and (<= distance limit) (string/= candidate name))
                         collect (cons distance candidate))))
    (mapcar #'cdr (subseq (sort scored #'< :key #'car) 0 (min most (length scored))))))

(defun file-slot-names (text)
  "The names of every entry -- input, computed slot, child, function -- of
every define-object in TEXT, lower case."
  (declare (special *slot-sections*))   ; defined with the structural tools, below
  (loop for span in (top-forms text)
        for elements = (and (char= (char text (car span)) #\() (form-elements text (car span)))
        when (and (>= (length elements) 2)
                  (string-equal (span-text text (first elements)) "define-object"))
          append (loop for (section . list-span) in (object-sections text elements)
                       when (member section *slot-sections* :test #'string=)
                         append (loop for entry in (form-elements text (car list-span))
                                      for name = (if (char= (char text (car entry)) #\()
                                                     (slot-entry-name text entry)
                                                     (and (char/= (char text (car entry)) #\")
                                                          (string-downcase (span-text text entry))))
                                      when name collect name))))

(defvar *known-function-names* nil
  "The functions Gendl and its geometry export, lower case, once asked for.")

(defun known-function-names ()
  (or *known-function-names*
      (setq *known-function-names*
            (let ((names nil))
              (dolist (package '(:gendl :geom-base) names)
                (let ((found (find-package package)))
                  (when found
                    (do-external-symbols (symbol found)
                      (when (fboundp symbol) (push (string-downcase (symbol-name symbol)) names))))))))))

(defparameter *function-misnomers*
  '(("radians" . "degrees-to-radians") ("deg->rad" . "degrees-to-radians") ("deg-to-rad" . "degrees-to-radians")
    ("degrees->radians" . "degrees-to-radians") ("to-radians" . "degrees-to-radians") ("deg2rad" . "degrees-to-radians")
    ("degrees" . "radians-to-degrees") ("rad->deg" . "radians-to-degrees") ("rad-to-deg" . "radians-to-degrees")
    ("radians->degrees" . "radians-to-degrees") ("rad2deg" . "radians-to-degrees")
    ("vector-length" . "length-vector") ("normalize-vector" . "unitize-vector") ("normalize" . "unitize-vector")
    ("cross-product" . "cross-vectors") ("dot-product" . "dot-vectors") ("distance" . "3d-distance")
    ("vector-add" . "add-vectors") ("vector-subtract" . "subtract-vectors") ("scale-vector" . "scalar*vector"))
  "Names a model reaches for that Gendl spells otherwise: (wrong . right).")

(defun word-after (text marker)
  "The run of non-blank characters after MARKER in TEXT, trimmed of the
punctuation an error message puts around a name; nil without MARKER."
  (let ((at (search marker text)))
    (when at
      (let* ((blank '(#\Space #\Newline #\Tab #\Return))
             (start (or (position-if-not #'(lambda (c) (member c blank)) text :start (+ at (length marker)))
                        (length text)))
             (end (or (position-if #'(lambda (c) (member c blank)) text :start start)
                      (length text)))
             (word (string-trim ".,;:()'\"" (subseq text start end))))
        (and (plusp (length word)) word)))))

(defun rewrite-quoted (text prefix suffix make)
  "TEXT with every PREFIX<name>SUFFIX replaced by what MAKE answers for
<name>, the run up to SUFFIX."
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search prefix text :start2 start)
          for name-start = (and at (+ at (length prefix)))
          for end = (and at (search suffix text :start2 name-start))
          while end
          do (write-string text out :start start :end at)
             (write-string (funcall make (subseq text name-start end)) out)
             (setq start (+ end (length suffix)))
          finally (write-string text out :start start))))

(defun ccl-wording (text)
  "TEXT, an error's words, with Allegro's phrasings said as CCL says them,
so that one table of hints reads both: an unbound variable, an undefined
function and a value of the wrong type, Allegro's lower-case names between
` and ' given CCL's upper case (a quoted string or a #<...> object as it
is)."
  (flet ((up (name) (if (or (zerop (length name)) (member (char name 0) '(#\" #\#))) name (string-upcase name))))
    (let* ((text (rewrite-quoted text "Attempt to take the value of the unbound variable `" "'"
                                 #'(lambda (name) (format nil "Unbound variable: ~a" (up name)))))
           (text (rewrite-quoted text "attempt to call `" "' which is an undefined function"
                                 #'(lambda (name) (format nil "Undefined function ~a" (up name))))))
      ;; `value' is not of the expected type `type'
      (with-output-to-string (out)
        (loop with start = 0
              with marker = "' is not of the expected type `"
              for at = (search marker text :start2 start)
              for open = (and at (position #\` text :end at :from-end t))
              for type-end = (and at (position #\' text :start (+ at (length marker))))
              while (and open (>= open start) type-end)
              do (write-string text out :start start :end open)
                 (format out "The value ~a is not of the expected type ~a"
                         (up (subseq text (1+ open) at))
                         (string-upcase (subseq text (+ at (length marker)) type-end)))
                 (setq start (1+ type-end))
              finally (write-string text out :start start))))))

(defun error-hints (session text)
  "Hints for the mistake behind a tool error whose words are TEXT, a list of
strings (at most two): the common Gendl slips, each named by what the error
itself says."
  (let ((hints nil)
        (text (ccl-wording text))
        (names (ignore-errors (file-slot-names (model-body-text session)))))
    (flet ((hint (control &rest args) (push (apply #'format nil control args) hints))
           (near (name) (nearest-names name names)))
      (cond
        ((search "Unbound variable: SELF" text)
         (hint "There is no SELF outside a define-object, so (the ...) does not work in evaluate.  Read a built slot with (the-object (make-object 'model) slot-name), a child's with (the-object (make-object 'model) child-name slot-name)."))
        ((search "Unbound variable: " text)
         (let ((name (string-downcase (or (word-after text "Unbound variable: ") ""))))
           (hint "~a is read bare.  A slot of this object is (the ~a), a child's slot (the child-name ~a); only a function's arguments and let variables are bare.~:[~;  ~a is a slot in the file.~]"
                 name name name (member name names :test #'string=) name))))
      (cond
        ((search "reserved word" text)
         (let ((name (word-after text "to nil..")))
           (hint "~:[That name~;~:*~a~] is one of Gendl's own names for every object, so it cannot be a slot or a child's input here.  Give yours another name (row-index, part-type, ...).  An element of a :sequence already has its number: (the-child index) among the child's inputs, (the index) inside its own define-object."
                 (and name (string-downcase name)))))
        ((search "translate needs direction/offset pairs" text)
         (hint "translate takes an origin, then PAIRS of a direction keyword (:up :down :left :right :front :rear) and one number: (translate (the center) :right 10 :up 5).  A signed or computed offset is ONE number after ONE keyword: :right (* sign distance)."))
        ((and (search "is not of the expected type" text) (search "The value \"" text))
         (hint "VALUE reached the file wrapped in its own double quotes, so the entry holds one string instead of code.  Send the same VALUE again with no quotes around the whole of it."))
        ((and (search "is not of the expected type" text)
              (some #'(lambda (type) (search type text)) '("SIMPLE-VECTOR" "ARRAY" "VECTOR")))
         (cond ((search "The value NIL" text)
                (hint "NIL reached a place that wants a point or a vector: the slot it came from has no value.  Check that slot first."))
               ((search "The value #<" text)
                (hint "An object was given where a point belongs: ask it for one, (the child-name center) or a vertex."))
               (t (hint "A point or a vector is not a list.  Make one with (make-point x y z) or (make-vector x y z); a direction of this object is (the (face-normal-vector :top)), likewise :bottom :left :right :front :rear."))))
        ((and (search "is not of the expected type" text) (search "The value NIL" text))
         (hint "NIL reached arithmetic: a slot it reads has no value.  Check that every slot named there exists in THIS object as (the name), and that each input has a default.")))
      (cond
        ((search "STANDARD-SEQUENCE" text :test #'char-equal)
         (hint "A child with :sequence is many parts, not one: ask one element, (the (child-name 0) ...), or every one with (list-elements (the child-name))."))
        ((or (search "has no slot named " text) (search "could not handle the :" text))
         (let* ((name (string-downcase (or (word-after text "has no slot named ")
                                           (word-after text "could not handle the :") "")))
                (near (near name)))
           (hint "Nothing here is named ~a.~@[  The nearest names in the file: ~{~a~^, ~}.~]  read_model shows every name as it is written."
                 name near))))
      (cond
        ((search "Undefined function (SETF" text)
         (hint "A slot is not set with setf.  Change it in the file with set_slot; in evaluate, (the-object object (set-slot! :name value))."))
        ((search "Undefined function " text)
         (let* ((raw (or (word-after text "Undefined function ") ""))
                (name (string-downcase (subseq raw (let ((colon (position #\: raw :from-end t))) (if colon (1+ colon) 0)))))
                (right (cdr (assoc name *function-misnomers* :test #'string=))))
           (cond ((search "gdl-slots::" raw :test #'char-equal)
                  ;; a message an object was asked for, (the-object x name):
                  ;; a slot's name gone wrong, not a function's
                  (hint "Nothing here is named ~a.~@[  The nearest names in the file: ~{~a~^, ~}.~]  read_model shows every name as it is written."
                        name (near name)))
                 ((member name names :test #'string=)
                  (hint "~a is a slot, not a function: read it as (the ~a), with no parentheses of its own.  A function of the object is called as (the (~a argument))." name name name))
                 (right (hint "There is no ~a in Gendl: the function is ~a." name right))
                 (t (let ((near (nearest-names name (known-function-names))))
                      (hint "There is no function ~a.~@[  Near it in Gendl: ~{~a~^, ~}.~]  search_docs finds the real name." name near)))))))
      (when (search "Stack overflow" text)
        (hint "Something reads itself: a slot whose value reads that same slot (straight or through another), or a function that calls itself without an end.  Follow what the newest slot reads."))
      (when (search "There is no package named" text)
        (hint "A colon inside a name makes Lisp look for a package.  Write names with no ':' in the middle; a keyword starts with one colon and stands alone."))
      (subseq (nreverse hints) 0 (min 2 (length hints))))))

(defun hint-blocks (session blocks)
  "The hints for a failed tool call whose result is BLOCKS, as content
blocks; nil when the error is none the table knows."
  (let ((text (format nil "~{~a~^ ~}"
                      (loop for block in blocks
                            for words = (and (consp block) (cdr (assoc "text" block :test #'string=)))
                            when (stringp words) collect words))))
    (mapcar #'(lambda (hint) (text-result "HINT: ~a" hint))
            (error-hints session text))))

;;
;; The :findings rail: an edit that compiles is answered with what the
;; completion check finds now, so a slot that does not compute is seen the
;; round it is written and not at the end.
;;

(defparameter *edit-findings-mark* "CHECK, what the lab's completion check finds now:"
  "The words that open the findings appended to an edit, which the eval's
tally looks for.")

(defun edit-findings-text (session)
  "What the completion check finds in SESSION's model now, in a few lines;
nil when there is nothing to judge or the check ran out of time."
  (let ((findings (let ((*completion-check?* t)) (completion-findings session))))
    (flet ((clipped (text most) (if (> (length text) most) (concatenate 'string (subseq text 0 most) "...") text)))
      (cond ((or (null findings) (getf findings :unjudged)) nil)
            ((getf findings :error) (format nil "MODEL does not build: ~a" (clipped (getf findings :error) 300)))
            ((getf findings :empty) "MODEL has nothing in it yet: no part and no computed slot.")
            ((not (or (getf findings :failing) (getf findings :broken) (getf findings :no-parts)
                      (getf findings :unwritten)))
             "nothing wrong: every part and every computed slot of MODEL computes.")
            (t (let ((why (getf findings :why))
                     (broken (getf findings :broken))
                     (unwritten (getf findings :unwritten)))
                 (with-output-to-string (out)
                   (when (getf findings :no-parts)
                     (format out "~%MODEL has no parts yet: nothing to draw."))
                   (when unwritten
                     (format out "~%Results the request names that MODEL does not have (add each as a computed slot of MODEL under this name): ~{~a~^, ~}~@[ and ~d more~]"
                             (subseq unwritten 0 (min 8 (length unwritten)))
                             (and (> (length unwritten) 8) (- (length unwritten) 8))))
                   (when why
                     (format out "~%Computed slots of MODEL that do not compute:")
                     (loop for (name . reason) in (subseq why 0 (min 6 (length why)))
                           do (format out "~%- ~a: ~a" name (clipped (substitute #\Space #\Newline reason) 160)))
                     (when (> (length why) 6) (format out "~%- and ~d more" (- (length why) 6))))
                   (when broken
                     (format out "~%Parts whose geometry does not compute: ~{~a~^, ~}~@[ and ~d more~]"
                             (subseq broken 0 (min 8 (length broken)))
                             (and (> (length broken) 8) (- (length broken) 8)))))))))))

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
             (structural-refusal "The model file does not read to its end now, so it cannot be edited by structure.  ~a"
                                 (or (paren-report text) *rewrite-advice*)))
            (t (let ((found (definitions-named text object '("define-object"))))
                 (case (length found)
                   (0 (structural-refusal "There is no define-object named ~a in the model file.  replace_form with a whole (define-object ~a ...) adds one." object object))
                   (1 (funcall function text (first (first found)) (second (first found))))
                   (t (structural-refusal "The model file defines ~a ~d times; keep one, with replace_form." object (length found))))))))))

(defun splice (text start end new)
  (concatenate 'string (subseq text 0 start) new (subseq text end)))

(defun structure-only? (session)
  "True when SESSION is a house session that builds by structure alone
(*house-structure?* :only): no whole-file write, no text edit."
  (and (house-model? session) (eq *house-structure?* :only)))

(defun definer-name? (string)
  "True for a plausible name to define: letters, digits and - _ * only."
  (and (stringp string) (plusp (length string))
       (every #'(lambda (c) (or (alphanumericp c) (find c "-_*"))) string)))

(defun object-skeleton (name mixins &optional description)
  "A define-object of NAME on MIXINS (a list of names) with every section
empty: balanced by construction, filled by add_slot."
  (format nil "(define-object ~a (~{~a~^ ~})~@[~%  :documentation (:description ~s)~]~%  :input-slots ()~%  :computed-slots ()~%  :objects ()~%  :functions ())"
          name mixins description))

(defun ensure-model-skeleton! (session)
  "In a structure-only session whose model file is empty, write MODEL's
skeleton, so the first edit is an add_slot and no header or define-object is
ever written by hand."
  (when (and (structure-only? session)
             (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) (model-body-text session)))))
    (write-model session (object-skeleton "model" '("base-object")))))

(defun new-object-def-tool (session name mixins description &optional entries)
  "Add an empty define-object NAME on MIXINS to the model file, every
section there to be filled by add_slot -- or filled at once from ENTRIES, as
add_slot takes them (the object defaults to NAME), all or nothing."
  (let ((mixins (remove "" (uiop:split-string (or mixins "") :separator '(#\Space #\, #\Tab #\Newline))
                        :test #'string=))
        (description (and (stringp description) (plusp (length (string-trim " " description)))
                          (string-trim " " description))))
    (cond ((not (definer-name? name))
           (structural-refusal "Nothing written: NAME is one Lisp name, letters, digits and hyphens, e.g. \"shelf-bracket\"."))
          ((notevery #'definer-name? mixins)
           (structural-refusal "Nothing written: MIXINS are type names separated by spaces, e.g. \"base-object\" or \"box\"."))
          (t
           (let ((text (model-body-text session))
                 (entries (mapcar #'(lambda (e)
                                      (list (or (entry-field e "object") name)
                                            (entry-field e "section")
                                            (or (entry-field e "slot") (entry-field e "name"))
                                            (entry-field e "value")))
                                  (coerce (or entries '()) 'list))))
             (multiple-value-bind (forms complete?) (top-forms text)
               (declare (ignore forms))
               (cond ((not complete?)
                      (structural-refusal "The model file does not read to its end now.  ~a" (or (paren-report text) *rewrite-advice*)))
                     ;; the :upsert rail: an object that is there stands, and
                     ;; its ENTRIES, if any, are added to it
                     ((and (rail? :upsert) (definitions-named text name '("define-object")))
                      (rail-note "ALREADY THERE: ~a is defined already, so no new define-object was started~:[~;; its ENTRIES were added to the one that stands~]."
                                 name entries)
                      (if entries
                          (multiple-value-bind (new reason) (add-entries text entries)
                            (cond (reason (structural-refusal "~a" reason))
                                  (t (loop for (entry-object nil entry-slot) in entries
                                           do (touch-entry entry-object entry-slot))
                                     (write-model session new))))
                          (values (list (text-result "~a stands as it was; nothing written.  add_slot adds to it, set_slot changes it." name))
                                  nil)))
                     ((definitions-named text name)
                      (structural-refusal "~a is already defined in the model file: add_slot adds to it and set_slot changes it." name))
                     (t (multiple-value-bind (new reason)
                            (add-entries (format nil "~@[~a~%~%~]~a"
                                                 (let ((kept (string-right-trim '(#\Space #\Newline #\Tab #\Return) text)))
                                                   (and (plusp (length kept)) kept))
                                                 (object-skeleton name (or mixins '("base-object")) description))
                                         entries)
                          (cond (reason
                                 (structural-refusal "~a  (~a was not started either.)" reason name))
                                (t (loop for (entry-object nil entry-slot) in entries
                                         do (touch-entry entry-object entry-slot))
                                   (write-model session new))))))))))))

;; the escape hatch: a whole define-object may be replaced in a
;; structure-only session once two structural edits of it have failed
(defvar *structural-failures* (make-hash-table :test #'equal)
  "(session-id object) -> how many structural edits of that object failed.")
(defvar *structural-failures-lock* (bt2:make-lock :name "prompt-lab structural failures"))

(defun structural-failures (session object)
  (bt2:with-lock-held (*structural-failures-lock*)
    (gethash (list (session-id session) (string-downcase (or object ""))) *structural-failures* 0)))

(defun note-structural-call (session object blocks error?)
  "Count a failed structural edit of OBJECT; values BLOCKS and ERROR? as given."
  (when error?
    (bt2:with-lock-held (*structural-failures-lock*)
      (incf (gethash (list (session-id session) (string-downcase (or object ""))) *structural-failures* 0))))
  (values blocks error?))

;;
;; A RUN OF REFUSED REWRITES (the :refusal-run rail).  A house session
;; building by structure was seen sending a whole define-object to
;; replace_form thirty times in a row, each refused in the same words,
;; until its context was full: a third of a run's tokens, and no model.
;; The refusal that says how to work did not turn it.  On this rail a
;; refusal of that kind that is the third in a row or later says instead,
;; plainly, that the call will be refused every time, and what to call.  A
;; run is refusals of a whole define-object with no structural edit that
;; went through between them; reading or viewing does not end one.
;;

(defparameter *refusal-run-length* 3
  "Integer.  From this many refusals of a whole define-object in a row, the
:refusal-run rail answers with *refusal-run-text*.")

(defparameter *refusal-run-text*
  "REFUSED AGAIN: nothing written, and nothing will be.  This is the ~:r time in a row you sent a whole define-object to replace_form; it is refused every time here, and sending it again only uses up your rounds.  Stop rewriting ~a whole.  What the file holds of it now is what read_model shows.  Add what is missing with add_slot (its ENTRIES takes many slots in one call) and change what is wrong with set_slot, one entry at a time."
  "What a refused whole define-object is answered with once it is a run:
~:r the count, ~a the object's name.")

(defvar *whole-refusals* (make-hash-table :test #'equal)
  "Session id -> how many whole define-objects in a row replace_form has
refused it.")

(defun note-whole-refusal! (session refused?)
  "Count one more refused whole define-object for SESSION and answer the
run's length so far -- or, with REFUSED? nil, end the run."
  (bt2:with-lock-held (*structural-failures-lock*)
    (if refused?
        (incf (gethash (session-id session) *whole-refusals* 0))
        (progn (remhash (session-id session) *whole-refusals*) 0))))

(defun replace-form-tool (session name source)
  "Replace the top-level form defining NAME with SOURCE, one whole form that
defines NAME; with no such form, add SOURCE at the end of the file."
  ;; a closing count wrong at the end is put right (the :paren-repair rail)
  (setq source (mended source "SOURCE"))
  (let ((problem (fragment-problem source :one-form? t)))
    (when problem (return-from replace-form-tool (structural-refusal "Nothing written: ~a" problem))))
  (let* ((source (string-trim '(#\Space #\Tab #\Newline #\Return) source))
         (own (form-elements source 0)))
    (unless (and (char= (char source 0) #\() (>= (length own) 2)
                 (string-equal (span-text source (second own)) name))
      (return-from replace-form-tool
        (structural-refusal "Nothing written: SOURCE must be one form that defines ~a -- (define-object ~a ...), (defun ~a ...) or the like." name name name)))
    ;; building by structure: a define-object is made with new_object_def and
    ;; filled with add_slot; whole, it is replaced only once two structural
    ;; edits of it have failed (the escape hatch)
    (when (and (structure-only? session)
               (string-equal (span-text source (first own)) "define-object")
               (< (structural-failures session name) 2))
      (return-from replace-form-tool
        (let ((run (note-whole-refusal! session t)))
          (if (and (rail? :refusal-run) (>= run *refusal-run-length*))
              (structural-refusal *refusal-run-text* run name)
              (structural-refusal "Nothing written: here a define-object is not written whole.  Change ~a one entry at a time -- add_slot, set_slot, remove_slot -- or make a new object with new_object_def.  (A whole replacement of ~a is allowed once two structural edits of it have failed.)" name name)))))
    (let ((text (model-body-text session)))
      (multiple-value-bind (forms complete?) (top-forms text)
        (declare (ignore forms))
        (unless complete?
          (return-from replace-form-tool
            (structural-refusal "The model file does not read to its end now.  ~a"
                                (or (paren-report text) *rewrite-advice*))))
        (let ((found (definitions-named text name)))
          (case (length found)
            (0 (write-model session (if (plusp (length (string-trim '(#\Space #\Newline) text)))
                                        (format nil "~a~%~%~a" (string-right-trim '(#\Space #\Newline) text) source)
                                        source)))
            (1 (let ((span (first (first found))))
                 (write-model session (splice text (car span) (cdr span) source))))
            (t (structural-refusal "The model file defines ~a ~d times.  ~a." name (length found) *rewrite-advice*))))))))

(defun slot-entry-text (slot value)
  (format nil "(~a ~a)" slot (string-trim '(#\Space #\Tab #\Newline #\Return) value)))

(defparameter *child-recipe*
  "A child is ONE entry of the objects section, named for itself: add_slot with object \"model\", section \"objects\", slot \"shelf\" and value \":type 'box :width (the width) :length 300 :height 18 :center (translate (the center) :up 100)\" adds it; set_slot with slot \"shelf\" and a new value changes it; remove_slot with slot \"shelf\" removes it.  One child per entry -- several children go as several ENTRIES of one add_slot -- and VALUE starts at :type: no parentheses around it, no list of children."
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
  (let ((problem (and (not (stringp value)) (fragment-problem value))))
    (when problem (return-from set-slot-tool (structural-refusal "Nothing written: VALUE ~a" problem))))
  (with-object session object
    #'(lambda (text span elements)
        (declare (ignore span))
        (multiple-value-bind (section entry) (find-slot text elements slot)
          (let* ((what (format nil "VALUE of ~a" slot))
                 ;; where a slot that is not there would go (the :set-adds
                 ;; rail): a child when its value starts at :type, else a
                 ;; computed slot
                 (plain (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                          (or (string-form-contents value) value)))
                 (home (or section
                           (if (and (>= (length plain) 5) (string-equal ":type" plain :end2 5))
                               ":objects"
                               ":computed-slots")))
                 ;; its own quotes off, then a closing count wrong at the end
                 ;; put right (the :unquote and :paren-repair rails)
                 (value (mended (unquoted value home what) what))
                 (problem (fragment-problem value)))
            (cond (problem (structural-refusal "Nothing written: VALUE ~a" problem))
                  ;; not there, and nothing near it is: added, and said so.  A
                  ;; near name is a slip of the pen, refused as ever
                  ((and (null section) (rail? :set-adds)
                        (null (nearest-names slot (object-slot-names text elements))))
                   (multiple-value-bind (new reason) (add-entry text object home slot value)
                     (cond (reason (structural-refusal "~a" reason))
                           (t (rail-note "ADDED, NOT CHANGED: ~a had no slot named ~a, so set_slot added it under ~a.  If it belongs in another section, remove_slot it and add_slot it there."
                                         object slot (subseq home 1))
                              (touch-entry object slot)
                              (write-model session new)))))
                  ((null section)
                   (structural-refusal "~a has no slot named ~a; add_slot adds one." object slot))
                  ((child-value-problem section value)
                   (structural-refusal "~a" (child-value-problem section value)))
                  (t (touch-entry object slot)
                     (write-model session (splice text (car entry) (cdr entry) (slot-entry-text slot value))))))))))

(defun entry-field (entry key)
  "KEY of one ENTRY of a batch: a JSON object as parsed (a hash table) or an
alist."
  (cond ((hash-table-p entry) (gethash key entry))
        ((consp entry) (cdr (assoc key entry :test #'string-equal)))))

(defun add-entry (text object section slot value)
  "TEXT with (SLOT VALUE) added to SECTION of the define-object OBJECT.
Values: the new text, or nil and the reason it is refused.  On the :upsert
rail a SLOT that is there already is changed instead -- where it stands, or
moved to SECTION when it stood in another -- and the result says so."
  (let* ((section (string-downcase (if (and (stringp section) (plusp (length section)) (char/= (char section 0) #\:))
                                       (concatenate 'string ":" section)
                                       (or section ""))))
         ;; its own quotes off, then a closing count wrong at the end put
         ;; right (the :unquote and :paren-repair rails)
         (value (let ((what (format nil "VALUE of ~a" slot)))
                  (mended (unquoted value section what) what)))
         (problem (fragment-problem value)))
    (flet ((no (control &rest args) (values nil (apply #'format nil control args))))
      (cond ((not (and (stringp slot) (plusp (length slot))))
             (no "Nothing written: every entry needs a SLOT, its own name."))
            ((section-name? slot)
             (no "Nothing written: ~a is a section of the define-object, not a slot in it; these tools take ONE entry by its own name.  ~a"
                 slot *child-recipe*))
            ((not (stringp value)) (no "Nothing written: every entry needs a VALUE."))
            (problem (no "Nothing written: VALUE ~a" problem))
            ((child-value-problem section value) (no "~a" (child-value-problem section value)))
            ((not (member section *slot-sections* :test #'string=))
             (no "SECTION is one of ~{~a~^, ~}." (mapcar #'(lambda (s) (subseq s 1)) *slot-sections*)))
            (t
             (multiple-value-bind (forms complete?) (top-forms text)
               (declare (ignore forms))
               (if (not complete?)
                   (no "The model file does not read to its end now, so it cannot be edited by structure.  ~a"
                       (or (paren-report text) *rewrite-advice*))
                   (let ((found (definitions-named text object '("define-object"))))
                     (case (length found)
                       (0 (no "There is no define-object named ~a in the model file.  new_object_def starts one." object))
                       (1 (destructuring-bind (span elements) (first found)
                            (multiple-value-bind (old-section old-entry) (find-slot text elements slot)
                              (cond
                                ((and old-section (not (rail? :upsert)))
                                 (no "~a already has a slot named ~a: set_slot changes it.  A name is used once in a define-object." object slot))
                                ;; the :upsert rail: changed where it stands ...
                                ((and old-section (string= old-section section))
                                 (rail-note "CHANGED, NOT ADDED: ~a already had a slot named ~a, so add_slot changed it in place, as set_slot does."
                                            object slot)
                                 (values (splice text (car old-entry) (cdr old-entry) (slot-entry-text slot value)) nil))
                                ;; ... or taken out of the section it stood in and added where asked
                                (old-section
                                 (rail-note "CHANGED, NOT ADDED: ~a already had a slot named ~a under ~a; it was moved to ~a with the new value.  A name is used once in a define-object."
                                            object slot (subseq old-section 1) (subseq section 1))
                                 (add-entry (without-entry text old-entry) object section slot value))
                                (t
                                 (let ((list-span (cdr (assoc section (object-sections text elements) :test #'string=)))
                                       (entry (slot-entry-text slot value)))
                                   (values (if list-span
                                               (let ((close (1- (cdr list-span))))
                                                 (splice text close close (format nil "~%   ~a" entry)))
                                               (let ((close (1- (cdr span))))
                                                 (splice text close close (format nil "~%~%  ~a~%  (~a)" section entry))))
                                           nil)))))))
                       (t (no "The model file defines ~a ~d times; keep one, with replace_form." object (length found))))))))))))

(defun add-entries (text entries)
  "TEXT with every entry of ENTRIES -- lists (object section slot value) --
added in turn.  Values: the new text, or nil and the reason the first
refused entry was refused, naming it when there are several."
  (let ((count (length entries)))
    (loop for (object section slot value) in entries
          for i from 1
          do (multiple-value-bind (next reason) (add-entry text object section slot value)
               (if reason
                   (return-from add-entries
                     (values nil (if (= count 1)
                                     reason
                                     (format nil "Nothing written -- entry ~d of ~d (slot ~s of ~a) was refused, so none of the ~d was: ~a"
                                             i count slot object count reason))))
                   (setq text next))))
    (values text nil)))

(defun declare-results-in-model! (session results)
  "Write a stub (result-stub-value) into MODEL's computed slots for each of
RESULTS, a list of (NAME . LABEL), whose name MODEL has no slot for yet,
in one write of the file.  Answers the names written; none, and nothing
written, when the file has no one define-object named MODEL to take them."
  (let* ((text (model-body-text session))
         (found (and (nth-value 1 (top-forms text))
                     (definitions-named text "model" '("define-object")))))
    (when (= (length found) 1)
      (let ((elements (second (first found)))
            (*rails* nil)
            (written nil))
        (loop for (name . label) in results
              unless (find-slot text elements name)
                do (multiple-value-bind (next refusal)
                       (add-entry text "model" ":computed-slots" name (result-stub-value label))
                     (unless refusal
                       (setq text next)
                       (push name written)
                       ;; the spans moved: read the form again
                       (setq elements (second (first (definitions-named text "model" '("define-object"))))))))
        (when written
          ;; the lab's own lines, in the record of who wrote what (archive.lisp)
          (let ((*model-writer* :lab))
            (write-model session text)))
        (reverse written)))))

(defun model-takes-results? (session)
  "Whether SESSION's model file has the one define-object named MODEL that
declared results are written into."
  (let ((text (model-body-text session)))
    (and (nth-value 1 (top-forms text))
         (= 1 (length (definitions-named text "model" '("define-object")))))))

(defun add-slot-tool (session object section slot value &optional entries)
  "Add (SLOT VALUE) to SECTION of the define-object OBJECT, making the
section when it has none -- or, given ENTRIES (a list of objects with
section, slot or name, value and optionally object), every one of them in
one write.  All or nothing: each entry is checked against the file as the
entries before it leave it, and one refused entry writes none."
  (let ((entries (if entries
                     (mapcar #'(lambda (e)
                                 (list (or (entry-field e "object") object)
                                       (entry-field e "section")
                                       (or (entry-field e "slot") (entry-field e "name"))
                                       (entry-field e "value")))
                             (coerce entries 'list))
                     (list (list object section slot value)))))
    (multiple-value-bind (text refusal) (add-entries (model-body-text session) entries)
      (cond (refusal (structural-refusal "~a" refusal))
            (t (loop for (entry-object nil entry-slot) in entries
                     do (touch-entry entry-object entry-slot))
               (write-model session text))))))

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
              (write-model session (without-entry text entry)))))))

(defun without-entry (text entry)
  "TEXT without the section entry whose span is ENTRY, and without the blank
before it."
  (let ((start (or (position-if-not #'(lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                    text :end (car entry) :from-end t)
                   (car entry))))
    (splice text (1+ start) (cdr entry) "")))

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

(defun view-model (session &key brief?)
  "The built model in words: every part's type, size and place, what
touches and overlaps what, what floats, and its symmetry.  BRIEF? says only
the envelope, the count, what floats, overlaps or does not compute, and the
symmetry -- the account appended to an edit by *house-auto-view*."
  (handler-case
      (with-time-limit (*eval-seconds* "view")
        (let* ((model (make-model session))
               (leaves (the-object model leaves))
               (boxes (mapcar #'leaf-box leaves))
               (env (or (ignore-errors (the-object model bounding-box)) (union-box boxes)))
               (n (length leaves))
               (lines nil)
               ;; for BRIEF?: what is worth a look, by name
               (broken nil) (floating nil) (pairs nil))
          (flet ((lo (box axis) (aref (first box) axis))
                 (hi (box axis) (aref (second box) axis))
                 (say (control &rest args) (push (apply #'format nil control args) lines)))
            (when env
              (say "MODEL's envelope: X ~,1f..~,1f  Y ~,1f..~,1f  Z ~,1f..~,1f  (~,1f x ~,1f x ~,1f)."
                   (lo env 0) (hi env 0) (lo env 1) (hi env 1) (lo env 2) (hi env 2)
                   (- (hi env 0) (lo env 0)) (- (hi env 1) (lo env 1)) (- (hi env 2) (lo env 2))))
            (cond
              ((zerop n) (say "No parts: MODEL has no leaves to draw."))
              ((and (not brief?) (> n *view-max-parts*))
               (say "~d parts, too many to list; by type: ~{~a~^, ~}." n
                    (let ((counts nil))
                      (dolist (leaf leaves) (incf (getf counts (type-of leaf) 0)))
                      (loop for (type count) on counts by #'cddr collect (format nil "~d ~(~a~)" count type)))))
              (t
               (let ((names (mapcar #'leaf-name leaves))
                     (floor-z (and env (lo env 2))))
                 (say "~d part~:p~:[:~;.~]" n brief?)
                 (loop for leaf in leaves for box in boxes for name in names for i from 0
                       do (if (null box)
                              (if brief?
                                  (push name broken)
                                  (say "- ~a (~(~a~)): its geometry does not compute." name (type-of leaf)))
                              (let ((touching nil) (overlapping nil))
                                (loop for other in boxes for other-name in names for k from 0
                                      when (and other (/= i k))
                                        do (let ((gap (loop for axis below 3
                                                            maximize (max (- (lo box axis) (hi other axis))
                                                                          (- (lo other axis) (hi box axis)))))
                                                 (depth (loop for axis below 3
                                                              minimize (- (min (hi box axis) (hi other axis))
                                                                          (max (lo box axis) (lo other axis))))))
                                             (cond ((> depth 1)
                                                    (push other-name overlapping)
                                                    (when (> k i) (push (cons name other-name) pairs)))
                                                   ((<= gap 0.5) (push other-name touching)))))
                                (when (and brief? floor-z (null touching) (null overlapping) (> n 1)
                                           (> (- (lo box 2) floor-z) 0.5))
                                  (push name floating))
                                (unless brief?
                                 (say "- ~a (~(~a~)): ~,1f x ~,1f x ~,1f, X ~,1f..~,1f Y ~,1f..~,1f Z ~,1f..~,1f~@[, axis ~a~]~:[~;, on the floor~]~@[; touches ~{~a~^, ~}~]~@[; OVERLAPS ~{~a~^, ~}~]~:[~;; LOOSE: touches no other part and not the floor~]"
                                     name (type-of leaf)
                                     (- (hi box 0) (lo box 0)) (- (hi box 1) (lo box 1)) (- (hi box 2) (lo box 2))
                                     (lo box 0) (hi box 0) (lo box 1) (hi box 1) (lo box 2) (hi box 2)
                                     (when (member (type-of leaf) '(cylinder cone) :test #'(lambda (a b) (string-equal (symbol-name a) (symbol-name b))))
                                       (ignore-errors (axis-word (the-object leaf (face-normal-vector :rear)))))
                                     (and floor-z (<= (abs (- (lo box 2) floor-z)) 0.5))
                                     (reverse touching) (reverse overlapping)
                                     (and floor-z (null touching) (null overlapping) (> n 1)
                                          (> (- (lo box 2) floor-z) 0.5)))))))
                 (when brief?
                   (flet ((some-of (items)
                            (format nil "~{~a~^, ~}~@[ and ~d more~]"
                                    (subseq items 0 (min 8 (length items)))
                                    (when (> (length items) 8) (- (length items) 8)))))
                     (if (or broken floating pairs)
                         (progn
                           (when broken (say "Geometry does not compute: ~a." (some-of (reverse broken))))
                           (when floating (say "LOOSE (touches no other part and not the floor; joined to nothing): ~a." (some-of (reverse floating))))
                           (when pairs (say "OVERLAPS: ~a."
                                            (some-of (mapcar #'(lambda (p) (format nil "~a with ~a" (car p) (cdr p)))
                                                             (reverse pairs))))))
                         (say "Nothing is loose, overlaps or fails to compute."))))
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

(defparameter *house-auto-view* nil
  "Nil, :text, :render or :look.  The author-view-edit loop: after every edit
of a house session that compiles, the harness itself shows the house model
what it built, appended to that edit's result -- the brief view_model
account (:text), the drawing as an image for a model that sees (:render), or
the viewer's words on it (:look, look_at_model).  Nothing is appended when
the model does not build: the edit's own result says why.  Nil: the model
asks for a view when it chooses, as before.")

(defparameter *auto-view-tools*
  '("write_model" "edit_model" "set_slot" "add_slot" "remove_slot" "replace_form" "new_object_def")
  "The edits after which *house-auto-view* shows the model.")

(defparameter *auto-view-mark* "AUTO-VIEW, the model as built now:"
  "The words that open an appended view, which the eval's tally looks for.")

(defun auto-view-blocks (session name)
  "The view to append to the result of tool NAME, an edit that compiled, in
a house SESSION under *house-auto-view*; nil when there is none or the model
does not build."
  (when (and *house-auto-view* (house-model? session)
             (member name *auto-view-tools* :test #'string=))
    (multiple-value-bind (blocks error?)
        (ignore-errors
         (case *house-auto-view*
           (:text (view-model session :brief? t))
           (:render (if (render-offered?)
                        (render session :layout "isometric-plus-ortho")
                        (values nil t)))
           (:look (look-at-model session))))
      (unless (or error? (null blocks))
        (cons (text-result "~a" *auto-view-mark*) blocks)))))

(defun edit-findings-blocks (session name)
  "The completion check's findings to append to the result of tool NAME, an
edit that compiled, for a SESSION on the :findings rail; else nil."
  (when (and (member :findings (session-rails session))
             (member name *auto-view-tools* :test #'string=))
    (let ((text (ignore-errors (edit-findings-text session))))
      (and text (list (text-result "~a ~a" *edit-findings-mark* text))))))

;;
;; The :glime rail (a later rail: off unless a list names it).  An edit that
;; compiles is answered with Gendl's reference check over the model file,
;; gendl-lisply:gendl-check -- Glime's checker without SLIME, in Gendl's
;; Lisply backend since 2026-11-18; an image without it skips the rail.
;; What it marks is a hint, never an error: a slot that reaches an object by
;; trickling down from its parent cannot be seen from a definition.
;;

(defparameter *glime-findings-mark*
  "REFERENCES, Gendl's reference check (hints: a slot trickling down from a parent is not seen):"
  "The words that open the reference check's findings appended to an edit.")

(defparameter *glime-findings-most* 8
  "Most findings of the reference check shown after one edit.")

(defun gendl-checks? ()
  "Whether this image's Lisply backend carries gendl-check.  Named through
the reader so the symbol wears the image's case."
  (let ((symbol (and (find-package :gendl-lisply)
                     (find-symbol (symbol-name :gendl-check) :gendl-lisply))))
    (and symbol (fboundp symbol) t)))

(defun glime-findings-blocks (session name)
  "The reference check's findings to append to the result of tool NAME, an
edit that compiled, for a SESSION on the :glime rail; else nil."
  (when (and (member :glime (session-rails session))
             (member name *auto-view-tools* :test #'string=)
             (gendl-checks?))
    (let ((findings (ignore-errors
                     (with-time-limit (*eval-seconds* "reference check")
                       (uiop:symbol-call :gendl-lisply :gendl-check
                                         (uiop:read-file-string (session-model-file session))
                                         :package (session-package session))))))
      (when findings
        (list (text-result "~a~{~%- line ~a: ~a~}~@[~%- ... and ~a more~]"
                           *glime-findings-mark*
                           (loop for f in findings
                                 for n from 1 to *glime-findings-most*
                                 append (list (getf f :line) (getf f :note)))
                           (and (> (length findings) *glime-findings-most*)
                                (- (length findings) *glime-findings-most*))))))))

(defun entries-schema ()
  "The schema of a batch of entries for add_slot and new_object_def."
  `(("type" . "array")
    ("description" . "Entries to add in one write, all or nothing; each names its section, slot and value (and object, when not the call's).")
    ("items" . ,(schema `(("object" . (("type" . "string")))
                           ("section" . (("type" . "string") ("description" . "input-slots, computed-slots, objects, hidden-objects or functions.")))
                           ("slot" . (("type" . "string") ("description" . "The entry's own name.")))
                           ("value" . (("type" . "string") ("description" . "What follows the name inside the entry, as for set_slot."))))
                         '("section" "slot" "value")))))

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
     ("description" . ,(concatenate 'string "ENTRIES adds many at once -- a list of {section, slot, value} (and object, when it is another define-object), checked first and written together, all or nothing: one refused entry writes none and the error names it.  Or add ONE entry to a section of a define-object -- input-slots, computed-slots, objects, hidden-objects or functions -- making the section when it has none. You give the name and VALUE (as for set_slot); the tool writes the parentheses. "
                                    ;; what a name already there meets depends on the :upsert rail
                                    (if (member :upsert (house-rails))
                                        "A name already used in the object is changed, as set_slot would, and the result says so."
                                        "A name already used in the object is refused.")
                                    " A CHILD OBJECT is one call per child: section \"objects\", slot the child's own name, value its type and inputs, e.g. {\"object\": \"model\", \"section\": \"objects\", \"slot\": \"shelf\", \"value\": \":type 'box :width (the width) :length 300 :height 18 :center (translate (the center) :up 100)\"} -- never a list of children, never the child in parentheses."))
     ("input_schema" . ,(schema `(("object" . (("type" . "string")))
                                  ("section" . (("type" . "string") ("description" . "input-slots, computed-slots, objects, hidden-objects or functions.")))
                                  ("slot" . (("type" . "string")))
                                  ("value" . (("type" . "string")))
                                  ("entries" . ,(entries-schema)))
                                '("object"))))
   `(("name" . "new_object_def")
     ("description" . ,(concatenate 'string "Start a new define-object: the tool writes (define-object NAME (MIXINS) :input-slots () :computed-slots () :objects () :functions ()) at the end of the file, balanced, and compiles it; then fill it with add_slot -- or give ENTRIES here, as add_slot takes them, to fill it in the same call, all or nothing. MIXINS are type names separated by spaces, base-object when not given. "
                                    (if (member :upsert (house-rails))
                                        "A name already defined is left as it stands (its ENTRIES, if any, are added to it), and the result says so."
                                        "A name already defined is refused: add_slot and set_slot change that one.")))
     ("input_schema" . ,(schema `(("name" . (("type" . "string") ("description" . "The new object's name, e.g. shelf-bracket.")))
                                  ("mixins" . (("type" . "string") ("description" . "Type names it builds on, separated by spaces; base-object when empty.")))
                                  ("description" . (("type" . "string") ("description" . "Optional: one sentence on what it is.")))
                                  ("entries" . ,(entries-schema)))
                                '("name"))))
   `(("name" . "remove_slot")
     ("description" . "Remove ONE entry -- input, computed slot, child or function -- from a define-object, by name.")
     ("input_schema" . ,(schema `(("object" . (("type" . "string"))) ("slot" . (("type" . "string"))))
                                '("object" "slot"))))
   `(("name" . "replace_form")
     ("description" . "Replace a whole top-level form -- (define-object NAME ...), (defun NAME ...) -- by its NAME with SOURCE, one form that defines NAME; with no such form, SOURCE is added to the file. SOURCE must balance and is checked before anything is written. For a change inside a define-object, set_slot is smaller and safer; where whole-file writing is withdrawn, a whole define-object is replaced only after two structural edits of it have failed.")
     ("input_schema" . ,(schema `(("name" . (("type" . "string")))
                                  ("source" . (("type" . "string") ("description" . "One whole form defining NAME."))))
                                '("name" "source"))))
   `(("name" . "view_model")
     ("description" . "See the built model, in words: MODEL's envelope; every part's type, size and place; what touches and what OVERLAPS what; which parts are LOOSE (touch no other part and not the floor: joined to nothing); cylinders' axes; left-right and front-back symmetry. Use it after check_model to judge whether the parts are where the request wants them, then fix what it shows.")
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
drawing against the request; answer its words.  The call is the session's:
it names the session's wallet as the builder's calls do, its tokens join
the session's usage and what the gate says of money is kept (it was outside
all three: a hosted viewer's tokens read as nothing, and its calls fell to
whoever pays for a session with no wallet)."
  (multiple-value-bind (blocks error?) (render session :layout "isometric-plus-ortho")
    (if error?
        (values blocks t)
        (handler-case
            (let ((body (encode (h "model" *house-viewer-model* "max_tokens" 1024
                                   "messages" (list (h "role" "user"
                                                       "content" (list (->json (first blocks))
                                                                       (h "type" "text"
                                                                          "text" (format nil *look-question* (latest-request session))))))))))
              (multiple-value-bind (status text headers)
                  ;; read at run time: *messages-url* is agent.lisp's, which loads after this file
                  (post-json (symbol-value '*messages-url*) body
                             :headers (append (list (cons "anthropic-version" "2023-06-01")
                                                    (cons *session-header* (session-id session)))
                                              (when (session-wallet session)
                                                (list (cons *wallet-header* (session-wallet session)))))
                             :seconds 180)
                (let ((json (ignore-errors (yason:parse text))))
                  ;; the session's accounts, as for a builder's call (agent.lisp)
                  (note-gate-answer session headers)
                  (when (hash-table-p json)
                    (let ((usage (gethash "usage" json)))
                      (when (hash-table-p usage) (add-usage session usage))))
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
