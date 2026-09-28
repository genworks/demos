;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The meter: compiles and runs cost modeling credits too, not only the
;; agent's tokens.  Each act is measured by the SYMBOL VOLUME of the
;; code involved -- what the reader counts, a rough figure to start
;; with, priced at the gate -- and reported to the gate's /meter door,
;; which books it against the session's free allowance and then the
;; visitor's wallet, the one ledger the token charges use.  A COMPILE is
;; metered by the symbols of the file compiled (the agent's write_model,
;; the visitor's edit or reload); a RUN by the symbols of what runs: the
;; expression an evaluate reads, the whole model file when the model is
;; built (check_model, render, the viewer drawing it).  Each report names
;; the ENGINE behind the room (*engine*, parameters.lisp), and the gate
;; multiplies a solids room's symbols by its factor for that engine
;; (:meter-engine-factors beside the rates in the cyclops config: the
;; knobs are there, never here).  A gate without the door, or no gate
;; at all, meters nothing and the lab works on.
;;

(defparameter *meter?* t
  "Boolean. Whether compiles and runs are reported to the gate's meter.")

(defun form-volume (form)
  "How many symbols FORM holds; NIL and the ends of lists are not counted."
  (cond ((null form) 0)
        ((symbolp form) 1)
        ((consp form) (+ (form-volume (car form)) (form-volume (cdr form))))
        ((stringp form) 0)
        ((vectorp form) (reduce #'+ form :key #'form-volume :initial-value 0))
        (t 0)))

(defun token-volume (text)
  "The tokens of TEXT, roughly: runs of characters that are neither
whitespace nor parentheses.  The fallback when the reader cannot finish
TEXT (an unbalanced edit still costs its size)."
  (let ((count 0) (in-token nil))
    (loop for c across text
          do (if (member c '(#\space #\tab #\newline #\return #\( #\) #\' #\`))
                 (setq in-token nil)
                 (unless in-token (setq in-token t) (incf count))))
    count))

(defun symbol-volume (text &optional (package *package*))
  "How many symbols TEXT holds when read in PACKAGE, nothing evaluated.
A text the reader cannot finish is counted by its tokens instead."
  (let ((count 0))
    (handler-case
        (with-input-from-string (in text)
          (let ((*package* package) (*read-eval* nil))
            (loop for form = (read in nil :eof)
                  until (eq form :eof)
                  do (incf count (form-volume form)))))
      (error () (setq count (max count (token-volume text)))))
    count))

(defun model-volume (session)
  "The symbol volume of SESSION's model file; 0 without one."
  (let ((file (session-model-file session)))
    (or (and (probe-file file)
             (ignore-errors (symbol-volume (uiop:read-file-string file :external-format :utf-8)
                                           (session-package session))))
        0)))

(defun meter! (session kind symbols)
  "Report to the gate that SESSION did KIND (:compile or :run) over
SYMBOLS symbols, and keep what the gate answers about the balance.
Values: true when booked, or when there is nothing to book or no gate to
book it (the lab works without one); nil and the gate's reason when it
refused -- the credits are spent -- so the caller can refuse the act."
  (if (or (not *meter?*) (not (integerp symbols)) (zerop symbols))
      t
      (handler-case
          (multiple-value-bind (json status)
              (gate-post "meter" (h "session" (session-id session)
                                    "wallet" (session-wallet session)
                                    "kind" (string-downcase (symbol-name kind))
                                    "symbols" symbols
                                    "engine" (engine-name)))
            (cond ((and (eql status 200) (hash-table-p json))
                   (note-balance session json)
                   (let ((meter (session-meter session))
                         ;; what the gate booked for this act, in credits
                         ;; (the symbols times its rate and engine factor)
                         (credits (gethash "credits" json)))
                     (setf (getf meter kind) (+ symbols (or (getf meter kind) 0)))
                     (when (realp credits)
                       (setf (getf meter :credits) (+ credits (or (getf meter :credits) 0))))
                     (setf (session-meter session) meter))
                   t)
                  ((member status '(402 429))
                   (values nil
                           (or (and (hash-table-p json)
                                    (let ((e (gethash "error" json)))
                                      (and (hash-table-p e) (gethash "message" e))))
                               "Modeling credits are spent -- top up to continue.")))
                  ;; no such door (an older gate), a gate that is down:
                  ;; unmetered, not refused
                  (t t)))
        (error () t))))

(defun meter-refusal (reason)
  "A tool's answer when the meter refused: content blocks and the error flag."
  (values (list (text-result "~a" reason)) t))
