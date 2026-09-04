;; Copyright © 2026 Gornskew Enterprises
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(gwl:define-package :demos-common
  (:documentation "Shared UI chrome and source-pane support for the
public Gendl demos under /demo/ on genworks.com.")
  (:export #:demo-ui-mixin #:function-source-string #:*hack-base* #:*url-prefix*))
