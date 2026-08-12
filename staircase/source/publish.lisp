;;;; -*- Mode: Lisp; Package: staircase-demo -*-

;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.


(in-package :staircase-demo)

;; The shared demos stylesheet, compiled by the build:demos entry in
;; /projects/apps/tailwind (scans everything under demos/), lives at
;; <demos>/css/ and serves at /demo/css/.
(defparameter *demos-dir*
  (let ((base (glisp:source-pathname)))
    (make-pathname :name nil :type nil
                   :directory (butlast (pathname-directory base) 2)
                   :defaults base)))

;; Individual (non-shared) sessions: each visitor's first hit on
;; /demo/staircase mints a fresh instance and redirects to its
;; /sessions/... URL.  session-control-mixin on staircase-ui keeps
;; those instances mortal for the standard GWL reaper.  Deployments
;; call this with :host to scope the publish to particular virtual
;; hosts; dev.lisp calls it with no host for load-and-go use.
(defun publish-staircase! (&key host)
  (gwl:with-all-servers (server)
    (gwl:publish-gwl-app "/demo/staircase" 'staircase-ui
                         :server server
                         :host host)
    (publish-directory :prefix "/demo/css/"
                       :server server
                       :host host
                       :destination (namestring
                                     (merge-pathnames "css/" *demos-dir*)))))
