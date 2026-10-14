;;;; -*- Mode: Lisp; Package: picnic-table -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;;
;;;; The table's parameters, said once: the page builds its form from
;;;; this list, writes the drawing links' query strings with it, and
;;;; the drawing door reads those query strings back with it.

(in-package :picnic-table)

(defparameter *parameters*
  '((:table-length "length" "Length (in)" 72 48 120 6)
    (:top-height "top_height" "Table height (in)" 30 26 34 0.5)
    (:seat-height "seat_height" "Seat height (in)" 17.5 15 20 0.5)
    (:overall-depth "depth" "Overall depth (in)" 60 52 72 1)
    (:foot-spread "foot_spread" "Foot spread (in)" 48 36 60 1)
    (:end-overhang "overhang" "End overhang (in)" 12 4 18 1)
    (:top-boards-count "top_boards" "Top boards" 5 4 7 1 :integer)
    (:seat-boards-count "seat_boards" "Boards per seat" 2 1 3 1 :integer))
  "Each parameter as (input query-name label default min max step
[:integer]): the picnic-table input it sets, its name in a drawing
link, the form's label, and the bounds the form and the door hold it to.")

(defparameter *hidden-line-modes* '(("remove" . :remove) ("dashed" . :dashed) ("draw" . :draw))
  "The hidden-line treatments a drawing link may name, by query value.")

(defun drawing-query-string (values hidden-lines)
  "The query string naming VALUES (one per *parameters* entry, in
order) and the HIDDEN-LINES keyword."
  (format nil "~{~a=~a~^&~}&hidden=~a"
          (loop for spec in *parameters*
                for value in values
                append (list (second spec)
                             (if (integerp value) value (format nil "~,4f" value))))
          (car (rassoc hidden-lines *hidden-line-modes*))))

(defun parse-drawing-query (query)
  "(values inputs hidden-lines) from QUERY, an alist of (name . value)
strings: INPUTS a plist of picnic-table inputs, every one in bounds,
missing ones at their defaults.  (values nil message) when a value is
not a number or is out of bounds."
  (let (inputs)
    (dolist (spec *parameters*)
      (destructuring-bind (key name label default min max step &optional integer?) spec
        (declare (ignore label step))
        (let* ((text (cdr (assoc name query :test #'string-equal)))
               (value (if text
                          (ignore-errors (with-standard-io-syntax
                                           (let ((*read-eval* nil)) (read-from-string text))))
                          default)))
          (unless (and (realp value) (<= min value max))
            (return-from parse-drawing-query
              (values nil (format nil "~a must be a number from ~a to ~a" name min max))))
          (setq inputs (list* key (if integer? (round value) value) inputs)))))
    (let ((hidden (cdr (assoc (or (cdr (assoc "hidden" query :test #'string-equal)) "remove")
                              *hidden-line-modes* :test #'string-equal))))
      (if hidden
          (values inputs hidden)
          (values nil "hidden must be remove, dashed or draw")))))
