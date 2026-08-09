;;
;; Copyright 2002, 2009, 2026 Genworks International and Genworks BV
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;;
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;;

(in-package :genworks.demos.bus)

;; The shared demos stylesheet, compiled by the build:demos entry in
;; /projects/apps/tailwind (scans everything under demos/), lives at
;; <demos>/css/ and serves at /demo/css/.
(defparameter *demos-dir*
  (let ((base (glisp:source-pathname)))
    (make-pathname :name nil :type nil
                   :directory (butlast (pathname-directory base) 2)
                   :defaults base)))

;; Individual (non-shared) sessions: each visitor's first hit on
;; /demo/bus mints a fresh instance and redirects to its
;; /sessions/... URL.  session-control-mixin on bus-ui keeps those
;; instances mortal for the standard GWL reaper.  Deployments call
;; this with :host to scope the publish to particular virtual hosts;
;; dev use calls it with no host for load-and-go.
(defun publish-bus! (&key host)
  (gwl:with-all-servers (server)
    (gwl:publish-gwl-app "/demo/bus" 'bus-ui
                         :server server
                         :host host)
    (publish-directory :prefix "/demo/css/"
                       :server server
                       :host host
                       :destination (namestring
                                     (merge-pathnames "css/" *demos-dir*)))))
