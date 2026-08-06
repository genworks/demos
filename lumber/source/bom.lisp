;;;; -*- Mode: Lisp; Package: lumber -*-
;;;;
;;;; Bill-of-materials aggregation: walk an assembly's leaves,
;;;; collect every lumber-mixin piece, and group identical cuts.
;;;; Because the walk visits the same objects that produce the
;;;; geometry, the cut list can never drift out of sync with the
;;;; model.

(in-package :lumber)

;; round-to-nearest is inherited from the :gendl package.

(defun bill-of-materials (object &key (round-to 1/16))
  "List of (:stock ... :cut-length ... :buy-length ... :count ...)
plists aggregating every lumber-mixin leaf in OBJECT's tree.  Cut
lengths are rounded to ROUND-TO inch before grouping.  Lines sort by
stock name then cut length, longest first."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (leaf (the-object object leaves))
      (when (typep leaf 'lumber-mixin)
        (let* ((plist (the-object leaf bom-plist))
               (key (list (getf plist :stock)
                          (round-to-nearest (getf plist :cut-length) round-to)
                          (getf plist :buy-length))))
          (incf (gethash key table 0)))))
    (let (lines)
      (maphash (lambda (key count)
                 (push (list :stock (first key)
                             :cut-length (second key)
                             :buy-length (third key)
                             :count count)
                       lines))
               table)
      (sort lines
            (lambda (a b)
              (let ((stock-a (getf a :stock)) (stock-b (getf b :stock)))
                (if (string-equal stock-a stock-b)
                    (> (getf a :cut-length) (getf b :cut-length))
                    (string-lessp stock-a stock-b))))))))

(defun bom-text (object &key title)
  "Human-readable cut list for OBJECT, one line per distinct stock and
cut length.  Buy column shows the store length in feet, or 'special'
when the cut outruns the longest stock."
  (let ((lines (bill-of-materials object)))
    (with-output-to-string (out)
      (when title (format out "~a~%" title))
      (format out "~4a  ~10a  ~9a  ~8a~%" "Qty" "Stock" "Cut (in)" "Buy (ft)")
      (dolist (line lines)
        (format out "~4d  ~10a  ~9,2f  ~8a~%"
                (getf line :count)
                (getf line :stock)
                (float (getf line :cut-length))
                (let ((buy (getf line :buy-length)))
                  (if buy (floor buy 12) "special")))))))
