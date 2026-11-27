;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;
;; The agent's tools, against a level's allowlist: for each kind of session,
;; the names the lab would offer its agent, and any not on the list.  The
;; ship's levels (basilisk levels/) name what a lab room may offer; this is
;; the lab's half of that check.  Load into a Lisp with prompt-lab loaded
;; and call
;;
;;   (prompt-lab::level-tools-run-checks)              ; the default allowlists
;;   (prompt-lab::level-tools-run-checks :allowlists '(:model (...) :app (...)))
;;
;; It returns (:passed N :failed M :failures (...)).  Nothing is made: the
;; probe session is a struct of its own, its kind set in the kinds table
;; under a package name no session has, and taken out again.
;;

(in-package :prompt-lab)

(defparameter *level-tool-allowlists*
  '(:model ("write_model" "edit_model" "read_model" "evaluate" "check_model" "render"
            "describe_object" "search_docs" "list_files" "read_file")
    :app ("write_model" "edit_model" "read_model" "evaluate" "check_model" "render"
          "describe_object" "search_docs" "list_files" "read_file" "check_app" "look_at_page"))
  "Plist, session kind -> the tool names a lab may offer for it.")

(defun level-tools (kind)
  "The tool names the lab offers a session of KIND, by a probe session."
  (let* ((name (format nil "PL-LEVEL-TOOLS-PROBE-~(~a~)" kind))
         (probe (make-session-internal :id (format nil "level-tools-probe-~(~a~)" kind) :package-name name)))
    (bt:with-lock-held (*kinds-lock*) (setf (gethash name *session-kinds*) kind))
    (unwind-protect
         (map 'list #'(lambda (tool) (cdr (assoc "name" tool :test #'string=))) (tool-definitions probe))
      (bt:with-lock-held (*kinds-lock*) (remhash name *session-kinds*)))))

(defun level-tools-run-checks (&key (allowlists *level-tool-allowlists*))
  (let ((results nil))
    (loop for (kind allowed) on allowlists by #'cddr
          do (let* ((offered (handler-case (level-tools kind) (error (e) (list :error (format nil "~a" e)))))
                    (extra (if (eq (first offered) :error) offered
                               (set-difference offered allowed :test #'string=))))
               (push (cons (format nil "~(~a~): every tool offered is on the list" kind)
                           (or (null extra) (format nil "also offered: ~{~a~^, ~}" extra)))
                     results)))
    (let ((failures (remove t results :key #'cdr)))
      (list :passed (count t results :key #'cdr) :failed (length failures) :failures (reverse failures)))))
