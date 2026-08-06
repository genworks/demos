;;;; -*- Mode: Lisp; Package: staircase-demo -*-

(in-package :staircase-demo)

;; Individual (non-shared) sessions: each visitor's first hit on
;; /staircase mints a fresh instance and redirects to its
;; /sessions/... URL.  session-control-mixin on staircase-ui keeps
;; those instances mortal for the standard GWL reaper.  Deployments
;; call this with :host to scope the publish to particular virtual
;; hosts; dev.lisp calls it with no host for load-and-go use.
(defun publish-staircase! (&key host)
  (gwl:with-all-servers (server)
    (gwl:publish-gwl-app "/staircase" 'staircase-ui
                         :server server
                         :host host)))
