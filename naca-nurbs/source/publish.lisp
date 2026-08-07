;;;; -*- Mode: Lisp; Package: naca-nurbs -*-

(in-package :naca-nurbs)

;; Individual (non-shared) sessions: each visitor's first hit on
;; /demo/naca-nurbs mints a fresh instance and redirects to its
;; /sessions/... URL.  session-control-mixin on ui keeps those
;; instances mortal for the standard GWL reaper.  Deployments call
;; this with :host to scope the publish to particular virtual hosts;
;; dev.lisp calls it with no host for load-and-go use.
;;
;; The compiled Tailwind stylesheet referenced by ui's
;; additional-header-content is served from <project>/css/ at
;; /demo/naca-nurbs-css/, scoped the same way.
(defun publish-ui! (&key host)
  (with-all-servers (server)
    (publish-gwl-app "/demo/naca-nurbs" 'ui
                     :server server
                     :host host)
    (publish-directory :prefix "/demo/naca-nurbs-css/"
                       :server server
                       :host host
                       :destination (namestring
                                     (merge-pathnames "css/" *project-dir*)))))
