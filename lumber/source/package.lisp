;;
;; Copyright 2026 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (Gendl).
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

;;
;; Successor to the :lumber package embedded in the bench demo; that
;; copy stays untouched until bench and deck are moved onto this
;; system.  Load one or the other, not both.
;;

(gdl:define-package :lumber
  (:export #:lumber-mixin #:lumber #:angle-cut-mixin #:angle-cut-lumber
           #:lumber-solid #:angle-cut-lumber-solid
           #:lumber-type #:bill-of-materials #:bom-text
           #:nominal-section #:stock-lengths-for
           #:1x2 #:1x4 #:1x6 #:1x8 #:1x10 #:1x12
           #:2x2 #:2x4 #:2x6 #:2x8 #:2x10 #:2x12
           #:4x4 #:4x6 #:6x6))
