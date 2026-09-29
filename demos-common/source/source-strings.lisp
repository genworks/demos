;;;; -*- Mode: Lisp; Package: demos-common -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;;
;;;; PATCH -- candidate for elevation into gendl proper, alongside
;;;; gdl:definition-source-string (which handles define-objects only).
;;;; Once elevated, delete this file and call the gendl version.
;;;;
;;;; Verbatim defun source via the Lisp's own source recording -- no
;;;; wrapper macros, no advice.  CCL records the definition text
;;;; itself (source notes, on with *save-source-locations*); Allegro
;;;; records the source file (*record-source-file-info*), from which
;;;; we extract the form's text.  Other implementations: nil for now
;;;; (SBCL via sb-introspect is future work).

(in-package :demos-common)

#+allegro
(defun %extract-toplevel-form-text (file marker)
  "Verbatim text of the top-level form starting with MARKER in FILE."
  (with-open-file (in file :direction :input :external-format :utf-8)
    (let* ((buf (make-string (file-length in)))
           (len (read-sequence buf in))
           (text (subseq buf 0 len))
           (mlen (length marker))
           (start (do ((pos (search marker text)
                            (search marker text :start2 (1+ pos))))
                      ((or (null pos)
                           (and (or (zerop pos)
                                    (eql (char text (1- pos)) #\Newline))
                                (or (>= (+ pos mlen) (length text))
                                    (member (char text (+ pos mlen))
                                            '(#\Space #\Newline #\Tab #\()))))
                       pos))))
      (when start
        (let ((depth 0) (i start) (n (length text)) (state :normal))
          (loop while (< i n) do
            (let ((ch (char text i)))
              (ecase state
                (:normal
                 (case ch
                   (#\( (incf depth))
                   (#\) (decf depth)
                        (when (zerop depth)
                          (return-from %extract-toplevel-form-text
                            (subseq text start (1+ i)))))
                   (#\" (setq state :string))
                   (#\; (setq state :line-comment))
                   (#\# (when (< (1+ i) n)
                          (case (char text (1+ i))
                            (#\\ (incf i 2))
                            (#\| (setq state :block-comment) (incf i)))))))
                (:string
                 (case ch
                   (#\\ (incf i))
                   (#\" (setq state :normal))))
                (:line-comment
                 (when (eql ch #\Newline) (setq state :normal)))
                (:block-comment
                 (when (and (eql ch #\|) (< (1+ i) n)
                            (eql (char text (1+ i)) #\#))
                   (setq state :normal) (incf i)))))
            (incf i))
          nil)))))

(defun function-source-string (symbol)
  "Verbatim source text of SYMBOL's defun, from the implementation's
built-in source recording.  Nil when nothing is recorded."
  (ignore-errors
    #+ccl
    (let ((note (ccl:function-source-note (fdefinition symbol))))
      (and note (ccl:source-note-text note)))
    #+allegro
    (let ((file (excl:source-file symbol :operator)))
      (and file
           (%extract-toplevel-form-text
            file (format nil "(defun ~a"
                         (string-downcase (symbol-name symbol))))))
    #-(or ccl allegro) nil))
