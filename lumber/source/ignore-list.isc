;; solids.lisp is loaded by solids-loader.lisp at load time, NOT by ASDF.
;;
;; It references surf:extruded-solid at READ time, so on a free image
;; (no SMLib / no surf solids) the file cannot even be read.  The loader
;; checks the running image first and only then loads it, which is why
;; lumber works on Enterprise and on free images alike.
;;
;; Listing it here keeps cl-lite from adding it as an ASDF component.
;; Without this, a regenerated lumber.asd compiles solids.lisp
;; unconditionally and the system fails to build on every free image --
;; and lumber is a dependency of staircase, with deck and other
;; lumber-consuming demos to follow.
("solids")
