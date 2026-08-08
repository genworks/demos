;;;; -*- Mode: Lisp; Package: staircase-demo -*-

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
