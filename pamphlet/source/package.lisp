(in-package :user)


(gwl:define-package :pamphlet (:export #:side-1 #:side-2 #:assembly :save!) (:use :pdf) 
  (:shadowing-import-from :geom-base #:translate #:circle #:ellipse #:polyline #:arc #:pie-chart #:bbox)   )

