;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The load gate.  On CCL a class redefined while another thread makes an
;; instance of it can be left broken: every later instance signals (a
;; stack overflow in its parameters, or no applicable method) until the
;; class is defined again.  The gate keeps the two apart: every request
;; holds it SHARED while it runs, and loading a model file holds it
;; EXCLUSIVE, so no instance is made while a definition is going in.
;;
;; A one-shot lab (worker.lisp) turns it on: there the whole image is one
;; session's, so "every request" is exactly the requests that may touch
;; that session's model.  In a shared lab it stays off (*load-gate?* nil),
;; where one visitor's load would hold every other visitor's page.
;;
;; Re-entrant: a thread already holding the gate in either mode may take
;; it again; a thread holding it shared that asks for it exclusive (a
;; request that saves a model) lets its share go for the exclusive hold
;; and takes it back after.  A waiting writer keeps new readers out, so a
;; load is never starved by a busy page.
;;

(defparameter *load-gate?* nil
  "Boolean. Hold every request off while a model file loads (gate.lisp).
Set by a one-shot lab's init; nil elsewhere.")

(defstruct (model-gate (:constructor make-model-gate ()))
  (lock (bt2:make-lock :name "prompt-lab load gate"))
  (cv (bt2:make-condition-variable :name "prompt-lab load gate"))
  (readers 0)
  (writer nil)
  (writers-waiting 0))

(defvar *model-gate* (make-model-gate))

(defvar *gate-hold* nil
  "This thread's hold on the gate: nil, :shared or :exclusive.")

(defun gate-acquire-shared (gate)
  (bt2:with-lock-held ((model-gate-lock gate))
    (loop while (or (model-gate-writer gate) (plusp (model-gate-writers-waiting gate)))
          do (bt2:condition-wait (model-gate-cv gate) (model-gate-lock gate)))
    (incf (model-gate-readers gate))))

(defun gate-release-shared (gate)
  (bt2:with-lock-held ((model-gate-lock gate))
    (decf (model-gate-readers gate))
    (when (zerop (model-gate-readers gate))
      (bt2:condition-broadcast (model-gate-cv gate)))))

(defun gate-acquire-exclusive (gate)
  (bt2:with-lock-held ((model-gate-lock gate))
    (incf (model-gate-writers-waiting gate))
    (loop while (or (model-gate-writer gate) (plusp (model-gate-readers gate)))
          do (bt2:condition-wait (model-gate-cv gate) (model-gate-lock gate)))
    (decf (model-gate-writers-waiting gate))
    (setf (model-gate-writer gate) (bt2:current-thread))))

(defun gate-release-exclusive (gate)
  (bt2:with-lock-held ((model-gate-lock gate))
    (setf (model-gate-writer gate) nil)
    (bt2:condition-broadcast (model-gate-cv gate))))

(defun call-with-model-shared (thunk &optional (gate *model-gate*))
  (if (or (not *load-gate?*) *gate-hold*)
      (funcall thunk)
      (progn
        (gate-acquire-shared gate)
        (unwind-protect (let ((*gate-hold* :shared)) (funcall thunk))
          (gate-release-shared gate)))))

(defun call-with-model-exclusive (thunk &optional (gate *model-gate*))
  (cond ((or (not *load-gate?*) (eq *gate-hold* :exclusive))
         (funcall thunk))
        ((eq *gate-hold* :shared)
         ;; give up this thread's share for the exclusive hold, then take
         ;; it back: holding it would wait on itself
         (gate-release-shared gate)
         (unwind-protect
              (progn
                (gate-acquire-exclusive gate)
                (unwind-protect (let ((*gate-hold* :exclusive)) (funcall thunk))
                  (gate-release-exclusive gate)))
           (gate-acquire-shared gate)))
        (t
         (gate-acquire-exclusive gate)
         (unwind-protect (let ((*gate-hold* :exclusive)) (funcall thunk))
           (gate-release-exclusive gate)))))

(defmacro with-model-shared (&body body)
  `(call-with-model-shared #'(lambda () ,@body)))

(defmacro with-model-exclusive (&body body)
  `(call-with-model-exclusive #'(lambda () ,@body)))
