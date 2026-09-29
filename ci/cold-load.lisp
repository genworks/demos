;;;; Copyright © 2026 Gornskew Enterprises
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as
;;;; published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.  Distributed WITHOUT
;;;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; ci/cold-load.lisp -- load every demo system on a COLD Gendl image.
;;;;
;;;; Run by .gitlab-ci.yml inside genworks/gendl:devo-ccl, an image
;;;; that has never seen this repo.  It does what a stack host's
;;;; services-init does: starts the web server, then loads
;;;; demos-common and every demo system from this checkout.  Any
;;;; error (a package that does not exist yet because a file sorted
;;;; ahead of package.lisp, a missing dependency, a compile failure)
;;;; and any WARNING signalled while a system loads fails the job.  A
;;;; warm development image, where the packages already exist, cannot
;;;; catch these; this is the check the 2026-09-11 demos-common outage
;;;; wanted: a system without source/file-ordering.isc loads its files
;;;; alphabetically, and one that sorts ahead of package.lisp breaks.
;;;;
;;;; Locally, with DEMOS_DIR naming a checkout:
;;;;   cd /opt/gendl && DEMOS_DIR=/path/to/demos \
;;;;     ./gdl/program/gdl-ccl -n -b --load /path/to/demos/ci/cold-load.lisp

(in-package :gdl-user)

;;; First, before any form below is READ: the QL package must exist.
(load-quicklisp)

(defparameter *demos-dir*
  (let ((dir (or (uiop:getenv "DEMOS_DIR") (uiop:getenv "CI_PROJECT_DIR")
                 (error "Set DEMOS_DIR or CI_PROJECT_DIR to the demos checkout."))))
    (uiop:ensure-directory-pathname dir)))

;;; Systems this job does not load, each with its reason.  The first
;;; two are enterprise demos: the gdl image publishes them, and this
;;; image has no surf.  The rest are the older applications the repo
;;; still carries (projects.org, "Reorganize & modernize demos"); each
;;; either cannot load on today's Gendl or warns as it loads, and a
;;; warning fails this job.  Delete a line the day its reason is gone.
(defparameter *not-loaded*
  '((:naca-nurbs . "needs surf/SMLib (enterprise GDL); the gdl image publishes it")
    (:gear . "profile.lisp's arc-curve is surf's (enterprise GDL); the gdl image publishes it")
    (:bench . "planking.gdl reads an undeclared *model-a*, and lumber.gdl defines its own package :lumber over the lumber system's")
    (:pui . "initialize.lisp warns as it loads that its images directory is missing")
    (:deck . "depends on pui and bench")
    (:house . "depends on deck, pui and bench, and its own initialize.lisp warns like pui's")))

(defun all-system-names ()
  "Every system named by an .asd file one level below the checkout,
demos-common first since everything else depends on it."
  (let ((names (mapcar (lambda (p) (intern (string-upcase (pathname-name p)) :keyword))
                       (directory (merge-pathnames "*/*.asd" *demos-dir*)))))
    (cons :demos-common
          (sort (remove :demos-common (remove-duplicates names)) #'string<))))

(defvar *failures* nil "((system message ...) ...) in load order.")

(defun load-one (system)
  "Load SYSTEM, recording every warning and any error against it.
Through ASDF, not ql:quickload: quickload muffles every warning
unless it is verbose, and it would fetch a missing dependency from
the Quicklisp dist over the network, which is exactly the kind of
thing this check should report instead."
  (let ((problems nil))
    (handler-case
        (handler-bind ((warning
                         (lambda (w)
                           (push (format nil "~a: ~a" (type-of w) w) problems)
                           (muffle-warning w))))
          (let ((*compile-verbose* nil) (*load-verbose* nil))
            (asdf:load-system system)))
      (error (e)
        (push (format nil "ERROR ~a: ~a" (type-of e) e) problems)))
    (setq problems (nreverse problems))
    (format t "~&~a ~(~a~)~%" (if problems "FAIL" "ok  ") system)
    (dolist (p problems) (format t "      ~a~%" p))
    (when problems (push (cons system problems) *failures*))
    (null problems)))

(format t "~&Cold load of the demos in ~a~%" *demos-dir*)
(format t "~&Image: ~a ~a, Gendl ~a~%"
        (lisp-implementation-type) (lisp-implementation-version) *gendl-version*)

;;; A deployment's services-init runs after the web server is up, and
;;; the older applications here publish as they load.
(gendl:start-gendl!)

(pushnew (namestring *demos-dir*) ql:*local-project-directories* :test #'equalp)
(ql:register-local-projects)

(let* ((all (all-system-names))
       (systems (remove-if (lambda (s) (assoc s *not-loaded*)) all)))
  (format t "~&~a system~:p found; loading ~a: ~{~(~a~)~^ ~}~%"
          (length all) (length systems) systems)
  (dolist (s all)
    (let ((why (cdr (assoc s *not-loaded*))))
      (when why (format t "~&skip ~(~a~) -- ~a~%" s why))))
  (dolist (s systems) (load-one s))
  (setq *failures* (nreverse *failures*))
  (format t "~&~%~a of ~a systems loaded clean.~%"
          (- (length systems) (length *failures*)) (length systems))
  (when *failures*
    (format t "~&FAILED: ~{~(~a~)~^ ~}~%" (mapcar #'car *failures*)))
  (finish-output)
  (uiop:quit (if *failures* 1 0)))
