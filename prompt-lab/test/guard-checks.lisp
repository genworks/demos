;;
;; Checks for the web-app name guard (kinds.lisp foreign-definitions): which
;; definitions in a web app's source are refused because they would land on
;; a name Lisp, Gendl or GWL already has.  Load into a Lisp that has
;; prompt-lab loaded -- a throwaway lab, never a public one -- and call
;;
;;   (prompt-lab::guard-run-checks)
;;
;; Nothing is compiled: each source is only read, as the guard reads it.
;; Returns (:passed N :failed M :failures (...)).
;;

(in-package :prompt-lab)

(defparameter *guard-check-cases*
  ;; (label source names-refused): nil when the source is allowed
  '(("a lens for the app's own type"
     "(define-lens (x3d bp-piece) () :output-functions ((cad-output () nil)))" nil)
    ("a lens for Gendl's box"
     "(define-lens (x3d box) () :output-functions ((cad-output () nil)))" ("box"))
    ("a format named x3d"
     "(define-format x3d (base-format))" ("x3d"))
    ("a format of the app's own on x3d"
     "(define-format bp-format (x3d))" nil)
    ("print-object for the app's own class"
     "(defmethod print-object ((p bp-piece) stream) (format stream \"piece\"))" nil)
    ("an :around method for the app's own class"
     "(defmethod print-object :around ((p bp-piece) stream) (call-next-method))" nil)
    ("the app's own class in the second argument"
     "(defmethod print-object ((p t) (s bp-piece)) nil)" nil)
    ("describe-object for the app's own class"
     "(defmethod describe-object ((s bp-piece) stream) nil)" nil)
    ("print-object for every object"
     "(defmethod print-object ((p standard-object) stream) nil)" ("print-object"))
    ("a method with no specializer"
     "(defmethod print-object (p stream) nil)" ("print-object"))
    ("an eql specializer"
     "(defmethod print-object ((p (eql 3)) stream) nil)" ("print-object"))
    ("a method on one of the lab's functions"
     "(defmethod prompt-lab::app-public-files ((s bp-piece)) nil)" ("app-public-files"))
    ("a method on a name that is no function"
     "(defmethod prompt-lab::no-such-generic ((s bp-piece)) nil)" ("no-such-generic"))
    ("a setf method of the app's own"
     "(defmethod (setf bp-colour) (v (p bp-piece)) v)" nil)
    ("an amendment of the app's own object"
     "(define-object-amendment bp-piece () :input-slots ((colour :white)))" nil)
    ("an amendment of base-object"
     "(define-object-amendment base-object () :input-slots ((colour :white)))" ("base-object"))
    ("an amendment of the lab's web-app"
     "(define-object-amendment prompt-lab:web-app () :computed-slots ((x 1)))" ("web-app"))
    ("a function named start"
     "(defun start () nil)" ("start"))
    ("a function of the app's own"
     "(defun bp-move () nil)" nil)))

(defun guard-run-checks ()
  (let ((session (make-session :address "127.0.0.1"))
        (passed 0) (failures nil))
    (set-session-kind! session :app)
    (unwind-protect
         (dolist (c *guard-check-cases*)
           (destructuring-bind (label source want) c
             (let ((got (ignore-errors (foreign-definitions session source))))
               (if (equal got want)
                   (incf passed)
                   (push (cons label (format nil "refused ~s, expected ~s" got want)) failures)))))
      (ignore-errors (delete-session session)))
    (list :passed passed :failed (length failures) :failures (nreverse failures))))
