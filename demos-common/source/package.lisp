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
  (:export #:demo-ui-mixin #:function-source-string #:*hack-base* #:*url-prefix*
           ;; the workshop portal (portal.gdl)
           #:*console-base* #:*portal-demos* #:register-portal-demo! #:publish-portal!
           ;; the export declaration (cad-export.lisp)
           #:register-cad-export! #:publish-cad-export! #:find-cad-export
           #:respond-with-cad-export #:cad-export-discovery #:cad-export-usage
           #:parse-cad-export-request #:write-cad-export-file #:*cad-exports*))
