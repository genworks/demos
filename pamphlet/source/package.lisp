;; Copyright (C) 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :user)


(gwl:define-package :pamphlet (:export #:side-1 #:side-2 #:assembly :save!) (:use :pdf) 
  (:shadowing-import-from :geom-base #:translate #:circle #:ellipse #:polyline #:arc #:pie-chart #:bbox)   )

