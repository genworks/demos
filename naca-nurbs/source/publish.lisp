;;;; -*- Mode: Lisp; Package: naca-nurbs -*-

(in-package :naca-nurbs)

;; The shared demos stylesheet (compiled by build:demos in
;; /projects/apps/tailwind) lives at <demos>/css/ and serves at
;; /demo/css/.  Every demo publishes it (idempotent) so no demo
;; depends on another being loaded.
(defparameter *demos-dir*
  (let ((base (glisp:source-pathname)))
    (make-pathname :name nil :type nil
                   :directory (butlast (pathname-directory base) 2)
                   :defaults base)))

;; Individual (non-shared) sessions: each visitor's first hit on
;; /demo/naca-nurbs mints a fresh instance and redirects to its
;; /sessions/... URL.  session-control-mixin on ui keeps those
;; instances mortal for the standard GWL reaper.  Deployments call
;; this with :host to scope the publish to particular virtual hosts;
;; dev.lisp calls it with no host for load-and-go use.
(defun publish-ui! (&key host)
  (with-all-servers (server)
    (publish-gwl-app "/demo/naca-nurbs" 'ui
                     :server server
                     :host host)
    (publish-directory :prefix "/demo/css/"
                       :server server
                       :host host
                       :destination (namestring
                                     (merge-pathnames "css/" *demos-dir*)))))
