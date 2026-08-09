;;;; -*- Mode: Lisp; Package: brick-wall-demo -*-

(in-package :brick-wall-demo)

;; The shared demos stylesheet, compiled by the build:demos entry in
;; /projects/apps/tailwind (scans everything under demos/), lives at
;; <demos>/css/ and serves at /demo/css/.
(defparameter *demos-dir*
  (let ((base (glisp:source-pathname)))
    (make-pathname :name nil :type nil
                   :directory (butlast (pathname-directory base) 2)
                   :defaults base)))

;; Individual (non-shared) sessions: each visitor's first hit on
;; /demo/brick-wall mints a fresh instance and redirects to its
;; /sessions/... URL.  session-control-mixin on brick-wall-ui keeps
;; those instances mortal for the standard GWL reaper.  Deployments
;; call this with :host to scope the publish to particular virtual
;; hosts; dev use calls it with no host for load-and-go.
(defun publish-brick-wall! (&key host)
  (gwl:with-all-servers (server)
    (gwl:publish-gwl-app "/demo/brick-wall" 'brick-wall-ui
                         :server server
                         :host host)
    (publish-directory :prefix "/demo/css/"
                       :server server
                       :host host
                       :destination (namestring
                                     (merge-pathnames "css/" *demos-dir*)))))
