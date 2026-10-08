;;;; Copyright © 2026 Genworks International
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as
;;;; published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.  Distributed WITHOUT
;;;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; ci/cold-load.lisp -- load every demo system on a COLD Gendl image.
;;;;
;;;; Run by .gitlab-ci.yml inside genworks/gendl:devo-ccl, an image
;;;; that has never seen this repo.  It does what a stack host's
;;;; services-init does: starts the web server, then loads
;;;; demos-common and every demo system from this checkout.  Any
;;;; error (a package that does not exist yet because a file sorted
;;;; ahead of package.lisp, a missing dependency, a compile failure)
;;;; and any WARNING signalled while a system loads fails the job.  A
;;;; warm development image, where the packages already exist, cannot
;;;; catch these; this is the check the 2026-09-11 demos-common outage
;;;; wanted: a system without source/file-ordering.sexp loads its files
;;;; alphabetically, and one that sorts ahead of package.lisp breaks.
;;;;
;;;; Then a SMOKE RUN through what loaded: each demo is published on
;;;; the image's own web server and its page fetched (a gwl app mints
;;;; a session and redirects; a page whose model or markup fails
;;;; ends the connection early, which fails here too); the shared
;;;; stylesheet and the portal likewise; x3dom-page writes a page for
;;;; a box; and the prompt lab's page (the sheet) is fetched and the lab
;;;; driven through its doors with no agent and no network -- config, a session, a hand-written model
;;;; compiled and loaded through the model door, the state door
;;;; reading it back, and the viewer (the sluice) drawing it.  Its gate
;;;; is pointed at a closed local port, so the balance and meter calls
;;;; fail at once, which the lab treats as "no gate".
;;;;
;;;; Locally, with DEMOS_DIR naming a checkout:
;;;;   cd /opt/gendl && DEMOS_DIR=/path/to/demos \
;;;;     ./gdl/program/gdl-ccl -n -b --load /path/to/demos/ci/cold-load.lisp

(in-package :gdl-user)

;;; First the web server: a deployment's services-init runs after it is
;;; up, the older applications here publish as they load, and on
;;; Genworks GDL it is start-gendl! that initializes the engine and
;;; sets the home load-quicklisp looks in.
(gendl:start-gendl!)

;;; Then, before any form below is READ: the QL package must exist.
(load-quicklisp)

(defparameter *demos-dir*
  (let ((dir (or (uiop:getenv "DEMOS_DIR") (uiop:getenv "CI_PROJECT_DIR")
                 (error "Set DEMOS_DIR or CI_PROJECT_DIR to the demos checkout."))))
    (uiop:ensure-directory-pathname dir)))

;;; Systems this job does not load, each with its reason.  The first
;;; two need surf and SMLib: skipped on the free image, loaded on
;;; Genworks GDL, which carries them.  The test is the SMLIB package,
;;; aboard only on Genworks GDL (the free image has a SURF package of
;;; its own, so that one decides nothing).  The rest are the older
;;; applications the repo still carries; each either cannot load on
;;; today's engine or warns as it loads, and a warning fails this job.
;;; Delete a line the day its reason is gone.
;;;
;;; The same file runs on CCL (Gendl) and on Allegro in modern mode
;;; (Genworks GDL), where symbol names are lower case: every package
;;; and symbol below is named by the reader (a keyword or #:name), never
;;; by an upper-case string.
(defparameter *not-loaded*
  (append
   (unless (find-package :smlib)
     '((:naca-nurbs . "needs surf/SMLib, which this image does not carry (the Genworks GDL job loads it)")
       (:gear . "profile.lisp's arc-curve is surf's, which this image does not carry (the Genworks GDL job loads it)")))
   '((:bench . "planking.gdl reads an undeclared *model-a*, and lumber.gdl defines its own package :lumber over the lumber system's")
     (:pui . "initialize.lisp warns as it loads that its images directory is missing")
     (:deck . "depends on pui and bench")
     (:house . "depends on deck, pui and bench, and its own initialize.lisp warns like pui's"))))

(defun system-keyword (name)
  "NAME, an .asd file's name, as the keyword the reader makes of it."
  (let ((*package* (find-package :keyword)))
    (read-from-string name)))

(defun all-system-names ()
  "Every system named by an .asd file one or two levels below the
checkout (prompt-lab/prompt-lab-sheet), demos-common first since
everything else depends on it."
  (let ((names (mapcar (lambda (p) (system-keyword (pathname-name p)))
                       (append (directory (merge-pathnames "*/*.asd" *demos-dir*))
                               (directory (merge-pathnames "*/*/*.asd" *demos-dir*))))))
    (cons :demos-common
          (sort (remove :demos-common (remove-duplicates names)) #'string<))))

(defvar *failures* nil "((system message ...) ...) in load order.")

(defun outside-dependencies (system)
  "The systems SYSTEM's .asd depends on that this checkout does not
define, as ASDF names them."
  (let ((ours (mapcar (lambda (s) (string-downcase (symbol-name s))) (all-system-names))))
    (remove-if (lambda (name) (member name ours :test #'string-equal))
               (remove-if-not #'stringp
                              (mapcar (lambda (d) (if (symbolp d) (string-downcase (symbol-name d)) d))
                                      (asdf:system-depends-on (asdf:find-system system)))))))

(defun load-one (system)
  "Load SYSTEM, recording every warning and any error against it.
Through ASDF, not ql:quickload: quickload muffles every warning
unless it is verbose, and it would fetch a missing dependency from
the Quicklisp dist over the network, which is exactly the kind of
thing this check should report instead."
  (let ((problems nil))
    (handler-case
        (let ((*compile-verbose* nil) (*load-verbose* nil))
          ;; Third-party libraries first, their warnings muffled: an
          ;; image may compile them cold here (Allegro does), and a
          ;; library's style warning is not this repository's to fix.
          ;; Their errors still fail the system.
          (handler-bind ((warning #'muffle-warning))
            (dolist (dependency (outside-dependencies system))
              (asdf:load-system dependency)))
          (handler-bind ((warning
                           (lambda (w)
                             (push (format nil "~a: ~a" (type-of w) w) problems)
                             (muffle-warning w))))
            (asdf:load-system system)))
      (error (e)
        (push (format nil "ERROR ~a: ~a" (type-of e) e) problems)))
    (setq problems (nreverse problems))
    (format t "~&~a ~(~a~)~%" (if problems "FAIL" "ok  ") system)
    (dolist (p problems) (format t "      ~a~%" p))
    (when problems (push (cons system problems) *failures*))
    (null problems)))

(format t "~&Cold load of the demos in ~a~%" *demos-dir*)
(format t "~&Image: ~a ~a, Gendl ~a~%"
        (lisp-implementation-type) (lisp-implementation-version) *gendl-version*)

(pushnew (namestring *demos-dir*) ql:*local-project-directories* :test #'equalp)
;;; The prompt lab depends on Monocle, a repository of its own: a stack
;;; host registers its checkout beside the demos', and so does this job
;;; (MONOCLE_DIR), unless the image's own Quicklisp tree carries it.
(let ((monocle (uiop:getenv "MONOCLE_DIR")))
  (when (and monocle (plusp (length monocle)))
    (pushnew (namestring (uiop:ensure-directory-pathname monocle))
             ql:*local-project-directories* :test #'equalp)))
(ql:register-local-projects)

;;;
;;; The smoke run.
;;;

;;; (system package publish-function path): each demo page, published
;;; with no host so it answers on localhost.
(defparameter *smoke-pages*
  '((:brick-wall :brick-wall-demo :publish-brick-wall! "/demo/brick-wall")
    (:bus :genworks.demos.bus :publish-bus! "/demo/bus")
    (:pod-line :pod-line-demo :publish-pod-line! "/demo/pod-line")
    (:robot :robot-demo :publish-robot! "/demo/robot")
    (:staircase :staircase-demo :publish-staircase! "/demo/staircase")
    ;; surf's two, on Genworks GDL only (see *not-loaded*)
    (:gear :gear :publish-gear! "/demo/gear")
    (:naca-nurbs :naca-nurbs :publish-ui! "/demo/naca-nurbs")))

;;; (system query): the stateless CAD export each of those publishes
;;; beside its page -- a STEP file for the query, and the free trace
;;; record at <path>/trace.
(defparameter *smoke-cad*
  '((:gear "/demo/gear/cad" "teeth=20")
    (:naca-nurbs "/demo/naca-nurbs/cad" "digits=2412")))

;;; What the prompt lab's model door is given: a small model in the
;;; shape its agent writes -- the object named MODEL, inputs for the
;;; key dimensions, a coloured child.
(defparameter *smoke-model* "(define-object model (base-object)
  :input-slots ((length 120) (width 60) (thickness 8))
  :objects ((plate :type 'box :length (the length) :width (the width)
                   :height (the thickness)
                   :display-controls (list :color :steelblue))
            (post :type 'cylinder :radius 6 :length 40
                  :center (translate (the center) :up (+ (half (the thickness)) 20))
                  :orientation (alignment :rear (the (face-normal-vector :top)))
                  :display-controls (list :color :firebrick))))
")

(defvar *smoke-failures* nil "((what message) ...) in run order.")

(defun smoke (what thunk)
  "Run THUNK, which returns true or signals; record WHAT as a failure on
nil or any error, and print a line either way."
  (let ((start (get-internal-real-time))
        (problem nil))
    (handler-case (unless (funcall thunk) (setq problem "check failed"))
      (error (e) (setq problem (format nil "~a: ~a" (type-of e) e))))
    (format t "~&~a ~a (~,1f s)~%" (if problem "FAIL" "ok  ") what
            (/ (- (get-internal-real-time) start) internal-time-units-per-second))
    (when problem
      (format t "      ~a~%" problem)
      (push (list what problem) *smoke-failures*))
    (null problem)))

(defun http-port ()
  (or (and (boundp 'gwl:*http-port*) (integerp gwl:*http-port*) gwl:*http-port*)
      (socket:local-port (net.aserve:wserver-socket gwl::*http-server*))))

(defun http (method path &key json headers)
  "One request to this image's own server, redirects followed.  Values:
the body, the status, the final path."
  (multiple-value-bind (body status response-headers uri)
      (net.aserve.client:do-http-request
          (format nil "http://127.0.0.1:~a~a" (http-port) path)
        :method method :redirect 5 :timeout 180 :headers headers
        :content-type (and json "application/json")
        ;; yason arrives with the prompt lab: named, not read, since this
        ;; file is read before anything is loaded
        :content (and json (with-output-to-string (s) (uiop:symbol-call :yason :encode json s))))
    (declare (ignore response-headers))
    (values body status (if uri (net.uri:uri-path uri) path))))

(defun page-ok? (path)
  "PATH answers 200 with a whole HTML document."
  (multiple-value-bind (body status) (http :get path)
    (unless (eql status 200) (error "~a answered ~a" path status))
    (unless (search "</html>" body :test #'char-equal)
      (error "~a answered 200 but no whole page (~a characters)" path (length body)))
    t))

(defun json-of (method path &key json headers)
  "Request PATH and parse its JSON answer; any status but 200 is an error."
  (multiple-value-bind (body status) (http method path :json json :headers headers)
    (unless (eql status 200)
      (error "~a answered ~a: ~a" path status (subseq body 0 (min 200 (length body)))))
    (uiop:symbol-call :yason :parse body)))

(defun table (&rest plist)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr do (setf (gethash key table) value))
    table))

(defun smoke-pages (loaded)
  (dolist (entry *smoke-pages*)
    (destructuring-bind (system package function path) entry
      (when (member system loaded)
        (smoke (format nil "~(~a~) page ~a" system path)
               (lambda ()
                 (uiop:symbol-call package function)
                 (page-ok? path))))))
  (dolist (entry *smoke-cad*)
    (destructuring-bind (system path query) entry
      (when (member system loaded)
        (smoke (format nil "~(~a~) STEP export ~a.stp?~a" system path query)
               (lambda ()
                 (multiple-value-bind (body status) (http :get (format nil "~a.stp?~a" path query))
                   (or (and (eql status 200) (search "ISO-10303-21" body :end2 (min 200 (length body))))
                       (error "answered ~a: ~a" status (subseq body 0 (min 200 (length body))))))))
        (smoke (format nil "~(~a~) trace record ~a/trace" system path)
               (lambda ()
                 (multiple-value-bind (body status) (http :get (format nil "~a/trace" path))
                   (and (eql status 200) (plusp (length body)))))))))
  (when (intersection loaded (mapcar #'first *smoke-pages*))
    (smoke "shared stylesheet /demo/css/demos-style.css"
           (lambda ()
             (multiple-value-bind (body status) (http :get "/demo/css/demos-style.css")
               (and (eql status 200) (plusp (length body)))))))
  (when (member :demos-common loaded)
    (smoke "portal /"
           (lambda ()
             (uiop:symbol-call :demos-common :publish-portal!)
             (page-ok? "/")))))

(defun smoke-x3dom-page ()
  (smoke "x3dom-page writes a page for a box"
         (lambda ()
           (let ((file (merge-pathnames "demos-smoke/box.html" (uiop:temporary-directory))))
             (uiop:symbol-call :x3dom-page :write-x3dom-page
                               (make-object 'box :length 10 :width 20 :height 30) file)
             (let ((text (uiop:read-file-string file)))
               (and (search "<Scene>" text) (search "</html>" text)))))))

(defun smoke-prompt-lab (sheet?)
  "Drive the prompt lab through its doors; with SHEET? (prompt-lab-sheet
loaded) its page too."
  (flet ((lab (name) (find-symbol (symbol-name name) :prompt-lab)))
    ;; no agent, no gate, no network: the gate's doors fail at once
    (setf (symbol-value (lab '#:*messages-url*)) "http://127.0.0.1:9/llm/messages"
          (symbol-value (lab '#:*meter?*)) nil
          (symbol-value (lab '#:*render-tool?*)) nil)
    (let ((prefix (symbol-value (lab '#:*url-prefix*)))
          (session nil) (owner nil))
      (flet ((door (name) (format nil "~a/api/~a" prefix name))
             (owner-headers () (list (cons "X-Prompt-Lab-Owner" owner))))
        (smoke "prompt-lab publish" (lambda () (uiop:symbol-call :prompt-lab :publish-prompt-lab!) t))
        ;; the page is the sheet (prompt-lab-sheet), published over the prefix
        (when sheet?
          (smoke "prompt-lab sheet publish" (lambda () (uiop:symbol-call :prompt-lab :publish-lab-sheet!) t))
          (smoke (format nil "prompt-lab page ~a, the sheet" prefix)
                 (lambda ()
                   (page-ok? prefix)
                   (let ((body (http :get prefix)))
                     (dolist (address (list (format nil "~a/manifest.webmanifest" prefix)
                                            (format nil "~a/static/editor.js" prefix))
                                      t)
                       (unless (search address body)
                         (error "the page does not name ~a" address))))))
          (smoke "prompt-lab's retired classic page sends its links to the sheet"
                 ;; the redirect itself, not followed: zacl's client stumbles
                 ;; on a second redirect (the sheet's minted session)
                 (lambda ()
                   (multiple-value-bind (body status headers)
                       (net.aserve.client:do-http-request
                           (format nil "http://127.0.0.1:~a~a/classic?session=abc" (http-port) prefix)
                         :redirect nil)
                     (declare (ignore body))
                     (let ((location (cdr (assoc :location headers))))
                       (or (and (eql status 301) (equal location (format nil "~a?session=abc" prefix)))
                           (error "answered ~a, to ~a" status location)))))))
        ;; The lab wears the sluice's skins where the image's sluice has
        ;; them, and its own copies where it is older; the checks below
        ;; hold either way, and say which they met.
        (format t "~&     the sluice of this image ~:[is older than its skins: the lab's own copies~;wears skins: the lab asks it~]~%"
                (uiop:symbol-call :prompt-lab :sluice-skins?))
        (smoke "prompt-lab stylesheets, editor and skins"
               (lambda ()
                 (let ((skins (mapcar (lambda (skin) (getf skin :href))
                                      (uiop:symbol-call :prompt-lab :skins)))
                       (split (uiop:symbol-call :prompt-lab :split-url)))
                   (unless skins (error "no skin was found"))
                   (dolist (address (append (list (uiop:symbol-call :prompt-lab :tokens-url))
                                            (mapcar (lambda (name) (format nil "~a/static/~a" prefix name))
                                                    '("prompt-lab.css"
                                                      "prompt-lab-viewer.css" "prompt-lab-phone.css"
                                                      "editor.js"))
                                            (and split (list split))
                                            skins)
                                    t)
                     (multiple-value-bind (body status) (http :get address)
                       (unless (and (eql status 200) (plusp (length body)))
                         (error "~a answered ~a" address status)))))))
        (when (uiop:symbol-call :prompt-lab :sluice-skins?)
          (smoke "prompt-lab's copy of the tokens says what the sluice's tokens say"
                 (lambda ()
                   (flet ((tokens (address)
                            (let* ((body (http :get address))
                                   ;; the block, at the head of a line; the
                                   ;; word is in the sheet's comment too
                                   (start (search (format nil "~%:root {") body))
                                   (end (and start (position #\} body :start start))))
                              (and end (subseq body start end)))))
                     (let ((theirs (tokens (uiop:symbol-call :prompt-lab :tokens-url)))
                           (ours (tokens (format nil "~a/static/prompt-lab.css" prefix))))
                       (or (and theirs (equal theirs ours))
                           (error "static/prompt-lab.css has fallen behind the sluice's tokens.css")))))))
        (smoke "prompt-lab manifest, and every icon it names"
               (lambda ()
                 (let* ((manifest (json-of :get (format nil "~a/manifest.webmanifest" prefix)))
                        (icons (gethash "icons" manifest)))
                   (unless (search prefix (gethash "start_url" manifest))
                     (error "the manifest starts somewhere else: ~a" (gethash "start_url" manifest)))
                   ;; the sheet lives at /sessions/<id>/, outside the prefix
                   (unless (equal (gethash "scope" manifest) "/")
                     (error "the manifest's scope is ~a" (gethash "scope" manifest)))
                   (when (zerop (length icons)) (error "the manifest names no icon"))
                   ;; HEAD: a picture is no text for the client to decode
                   (map nil (lambda (icon)
                              (let ((status (nth-value 1 (http :head (gethash "src" icon)))))
                                (unless (eql status 200)
                                  (error "~a answered ~a" (gethash "src" icon) status))))
                        icons)
                   t)))
        (smoke "prompt-lab service worker retires the one the classic page kept"
               (lambda ()
                 (multiple-value-bind (body status) (http :get (format nil "~a/worker" prefix))
                   (unless (eql status 200) (error "the worker answered ~a" status))
                   (unless (search "unregister()" body)
                     (error "the worker does not unregister itself"))
                   (unless (search (format nil "'prompt-lab:' + ~s" prefix) body)
                     (error "the worker does not name the lab's caches"))
                   t)))
        (smoke "prompt-lab config door"
               (lambda () (gethash "engine" (json-of :get (door "config")))))
        (when (smoke "prompt-lab session door opens a session"
                     (lambda ()
                       (let ((answer (json-of :post (door "session") :json (table))))
                         (setq session (gethash "session" answer)
                               owner (gethash "owner" answer))
                         (and (stringp session) (stringp owner)))))
          (smoke "prompt-lab model door compiles and loads a model"
                 (lambda ()
                   (let ((answer (json-of :post (door "model") :headers (owner-headers)
                                          :json (table "session" session "source" *smoke-model*))))
                     (or (eq (gethash "ok" answer) t)
                         (error "the model door refused it: ~a" (gethash "text" answer))))))
          (smoke "prompt-lab state door reads the model back"
                 (lambda ()
                   (let ((state (json-of :get (format nil "~a?session=~a" (door "state") session)
                                         :headers (owner-headers))))
                     (and (eq (gethash "model_defined" state) t)
                          (eq (gethash "editable" state) t)
                          (search "define-object model" (gethash "model_source" state))))))
          ;; a visitor's files (uploads.lisp): refused without the
          ;; declaration of rights and when the lab cannot read the kind,
          ;; kept and listed otherwise, fetched back byte for byte as a
          ;; download, a name that climbs out of the directory not found,
          ;; and brought into the conversation by the next prompt
          (when (gethash "uploads" (json-of :get (door "config")))
            (smoke "prompt-lab upload door keeps a file, and the file door hands it back"
                   (lambda ()
                     (let* ((text (format nil "name,mm~%width,120~%"))
                            (data (uiop:symbol-call :cl-base64 :string-to-base64-string text))
                            (file (format nil "http://127.0.0.1:~a~a?session=~a&name=" (http-port) (door "file") session)))
                       (flet ((upload (name data &optional (rights t))
                                (http :post (door "upload") :headers (owner-headers)
                                      :json (if rights
                                                (table "session" session "name" name "data" data "rights" t)
                                                (table "session" session "name" name "data" data)))))
                         (unless (eql 400 (nth-value 1 (upload "dims.csv" data nil)))
                           (error "a file came in without the declaration of rights"))
                         (unless (eql 400 (nth-value 1 (upload "tool.exe" data)))
                           (error "a kind the lab does not read came in"))
                         (unless (eql 200 (nth-value 1 (upload "dims.csv" data)))
                           (error "the upload was refused"))
                         (let ((files (gethash "files" (json-of :get (format nil "~a?session=~a" (door "state") session)))))
                           (unless (and (= (length files) 1) (equal (gethash "name" (elt files 0)) "dims.csv"))
                             (error "the state door does not list the file")))
                         (multiple-value-bind (body status)
                             (net.aserve.client:do-http-request (format nil "~adims.csv" file) :timeout 30)
                           (unless (and (eql status 200) (equal body text))
                             (error "the file door answered ~a" status)))
                         (unless (eql 404 (nth-value 1 (net.aserve.client:do-http-request
                                                           (format nil "~a../session.json" file) :timeout 30)))
                           (error "the file door reached outside the session's files"))
                         (let ((found (uiop:symbol-call :prompt-lab :find-session session)))
                           (unless (equal (mapcar (lab :file-reference-name)
                                                  (uiop:symbol-call :prompt-lab :pending-attachments found))
                                          '("dims.csv"))
                             (error "the file does not wait for the next prompt")))
                         t)))))
          ;; which lab a first prompt belongs in (routing.lisp): with a
          ;; sibling lab beside this one, a prompt that names the other
          ;; engine in so many words is answered 409 with where to take
          ;; it, and is built here all the same when it says to stay (the
          ;; start is refused further on for want of an agent: any answer
          ;; but the 409 will do).  The model's own verdict needs the
          ;; network and is not asked here.
          (smoke "prompt-lab sends a first prompt that names the other engine to the sibling lab"
                 (lambda ()
                   (let* ((sibling (lab '#:*sibling-lab*))
                          (kept (symbol-value sibling))
                          (other (if (eq (symbol-value (lab '#:*engine*))
                                         (uiop:symbol-call :prompt-lab :prompt-engine "use solids"))
                                     "no solids" "use solids"))
                          ;; a session of its own, made here: the session
                          ;; door's cap per address is for the other tests
                          (fresh (uiop:symbol-call :prompt-lab :make-session :address "127.0.0.1"))
                          (id (funcall (lab '#:session-id) fresh))
                          (headers (list (cons "X-Prompt-Lab-Owner" (funcall (lab '#:session-owner) fresh)))))
                     (setf (symbol-value sibling) (cons "/other-lab" "other lab"))
                     (unwind-protect
                          (flet ((prompt (&rest more)
                                   (http :post (door "prompt") :headers headers
                                         :json (apply #'table "session" id
                                                      "prompt" (format nil "a shelf, ~a" other) more))))
                            (multiple-value-bind (body status) (prompt)
                              (unless (eql status 409) (error "the prompt door answered ~a" status))
                              (unless (equal (gethash "url" (gethash "route" (uiop:symbol-call :yason :parse body)))
                                             "/other-lab?routed=1")
                                (error "the answer does not say where the prompt belongs: ~a" body)))
                            (when (eql 409 (nth-value 1 (prompt "stay" t)))
                              (error "a prompt that says to stay was sent away"))
                            t)
                       (setf (symbol-value sibling) kept)))))
          ;; the model as files (export.lisp): every format the config
          ;; door offers answers a file that starts as its kind does --
          ;; STEP, IGES and STL on the solids engine, where the box and
          ;; the cylinder are written as the solids of their shapes
          (smoke "prompt-lab download door answers every format it offers"
                 (lambda ()
                   (let ((magic '(("pdf" . "%PDF") ("svg" . "<") ("png" . "PNG")
                                  ("step" . "ISO-10303") ("iges" . "S      1")
                                  ("stl" . "facet normal"))))
                     (dolist (offer (coerce (gethash "downloads" (json-of :get (door "config"))) 'list) t)
                       (let ((kind (gethash "format" offer)))
                         (multiple-value-bind (body status)
                             (net.aserve.client:do-http-request
                                 (format nil "http://127.0.0.1:~a~a?session=~a&format=~a"
                                         (http-port) (door "download") session kind)
                               :format :binary :timeout 180 :headers (owner-headers))
                           (unless (eql status 200) (error "~a answered ~a" kind status))
                           (let ((head (map 'string #'code-char (subseq body 0 (min 80 (length body)))))
                                 (expected (cdr (assoc kind magic :test #'string=))))
                             (unless (and expected (search expected head))
                               (error "~a does not start as one: ~s" kind (subseq head 0 (min 20 (length head))))))))))))
          (smoke "prompt-lab download door refuses a format it does not offer"
                 (lambda ()
                   (eql 400 (nth-value 1 (http :get (format nil "~a?session=~a&format=dwg" (door "download") session))))))
          ;; an agent that runs elsewhere (external.lisp): its doors are
          ;; shut until the switch is thrown, then the session's tools
          ;; answer its owner over MCP and the agent door keeps the log
          (smoke "prompt-lab external doors are shut unless switched on"
                 (lambda ()
                   (and (eql 404 (nth-value 1 (http :post (door "agent") :headers (owner-headers)
                                                    :json (table "event" "text" "session" session "text" "x"))))
                        (eql 404 (nth-value 1 (http :post (format nil "~a/mcp?session=~a" prefix session)
                                                    :headers (owner-headers)
                                                    :json (table "jsonrpc" "2.0" "id" 1 "method" "ping")))))))
          (setf (symbol-value (lab '#:*external-agent?*)) t)
          (smoke "prompt-lab MCP door lists the tools and runs one, for the session's owner alone"
                 (lambda ()
                   (let ((mcp (format nil "~a/mcp?session=~a" prefix session)))
                     (flet ((rpc (id method &optional (params (table)))
                              (let ((answer (json-of :post mcp :headers (owner-headers)
                                                     :json (table "jsonrpc" "2.0" "id" id
                                                                  "method" method "params" params))))
                                (or (gethash "result" answer)
                                    (error "~a answered ~a" method
                                           (gethash "message" (gethash "error" answer)))))))
                       (unless (stringp (gethash "protocolVersion" (rpc 1 "initialize")))
                         (error "initialize named no protocol version"))
                       (unless (find "write_model" (gethash "tools" (rpc 2 "tools/list"))
                                     :key (lambda (tool) (gethash "name" tool)) :test #'equal)
                         (error "tools/list lacks write_model"))
                       (let ((answer (rpc 3 "tools/call" (table "name" "read_model" "arguments" (table)))))
                         (unless (search "define-object model"
                                         (gethash "text" (first (gethash "content" answer))))
                           (error "read_model did not answer the model file")))
                       (eql 403 (nth-value 1 (http :post mcp
                                                   :json (table "jsonrpc" "2.0" "id" 4 "method" "tools/list"))))))))
          (smoke "prompt-lab agent door takes a prompt, hands out the brief, and logs the reply"
                 (lambda ()
                   (let ((brief (json-of :post (door "agent") :headers (owner-headers)
                                         :json (table "event" "prompt" "session" session "text" "A plate."))))
                     (unless (and (stringp (gethash "system" brief)) (search session (gethash "mcp" brief)))
                       (error "the brief lacks the system prompt or the tools' address"))
                     (json-of :post (door "agent") :headers (owner-headers)
                              :json (table "event" "done" "session" session "text" "A plate, built."))
                     (let ((state (json-of :get (format nil "~a?session=~a" (door "state") session)
                                           :headers (owner-headers))))
                       (and (not (eq (gethash "busy" state) t))
                            (find "done" (gethash "log" state)
                                  :key (lambda (entry) (gethash "kind" entry)) :test #'equal))))))
          (smoke "prompt-lab agent door opens a session on a model it is handed"
                 (lambda ()
                   (let* ((brief (json-of :post (door "agent")
                                          :json (table "event" "prompt" "text" "Make the plate thicker."
                                                       "model" *smoke-model*)))
                          (headers (list (cons "X-Prompt-Lab-Owner" (gethash "owner" brief))))
                          (seeded (gethash "session" brief)))
                     (unless (eq (gethash "seeded" brief) t)
                       (error "the brief does not say the model was taken"))
                     (json-of :post (door "agent") :headers headers
                              :json (table "event" "stopped" "session" seeded "text" "Only a test."))
                     (let ((state (json-of :get (format nil "~a?session=~a" (door "state") seeded)
                                           :headers headers)))
                       (and (eq (gethash "model_defined" state) t)
                            (search "define-object model" (gethash "model_source" state)))))))
          ;; the other kind of session (kinds.lisp): one that builds a web
          ;; app, opened here on the recipe the agent's brief carries --
          ;; so the recipe itself is compiled, checked and served on
          ;; every engine at every push
          (smoke "prompt-lab builds a web app: the brief, check_app, and the app's own page"
                 (lambda ()
                   (let* ((brief (json-of :post (door "agent")
                                          :json (table "event" "prompt" "text" "A plate sizer." "kind" "app"
                                                       "model" (symbol-value (lab '#:*app-recipe*)))))
                          (headers (list (cons "X-Prompt-Lab-Owner" (gethash "owner" brief))))
                          (app (gethash "session" brief)))
                     (unless (and (equal (gethash "kind" brief) "app")
                                  (find "check_app" (gethash "tools" brief) :test #'equal))
                       (error "the brief is not a web app's"))
                     (let ((checked (gethash "result"
                                             (json-of :post (format nil "~a/mcp?session=~a" prefix app)
                                                      :headers headers
                                                      :json (table "jsonrpc" "2.0" "id" 1 "method" "tools/call"
                                                                   "params" (table "name" "check_app"
                                                                                   "arguments" (table)))))))
                       (when (or (null checked) (eq (gethash "isError" checked) t))
                         (error "check_app faults the lab's own recipe: ~a"
                                (and checked (gethash "text" (first (gethash "content" checked)))))))
                     (json-of :post (door "agent") :headers headers
                              :json (table "event" "stopped" "session" app "text" "Only a test."))
                     (let ((state (json-of :get (format nil "~a?session=~a" (door "state") app)
                                           :headers headers)))
                       (and (equal (gethash "kind" state) "app")
                            (eq (gethash "app_defined" state) t)
                            (page-ok? (gethash "app_url" state)))))))
          ;; Monetize (deploy.lisp): the session's model deployed at an
          ;; address of its own, open source and free, then priced by the
          ;; download, then taken down
          (smoke "prompt-lab deploys a model: its page, its source, its files; a priced download; taken down"
                 (lambda ()
                   (let* ((name "ci-smoke-plate")
                          (address (format nil "~a/d/~a" prefix name))
                          (drawing (format nil "~a?deployed=~a&format=svg" (door "download") name))
                          ;; a session of its own, on a model that says what it charges for
                          (opened (json-of :post (door "session") :json (table)))
                          (tolled (gethash "session" opened))
                          (owner (gethash "owner" opened))
                          (headers (list (cons "X-Prompt-Lab-Owner" owner))))
                     (flet ((deploy (&rest terms)
                              (json-of :post (door "deploy") :headers headers
                                       :json (apply #'table "session" tolled "name" name
                                                    "payee" "owner@example.com" terms)))
                            (status (path) (nth-value 1 (http :get path))))
                       (unwind-protect
                            (progn
                              ;; nothing is deployed that charges for nothing
                              (json-of :post (door "model") :headers headers
                                       :json (table "session" tolled "source" *smoke-model*))
                              (unless (eql 400 (nth-value 1 (http :post (door "deploy") :headers headers
                                                                  :json (table "session" tolled "name" name
                                                                               "payee" "owner@example.com"))))
                                (error "a model with no tolls was deployed"))
                              (json-of :post (door "model") :headers headers
                                       :json (table "session" tolled
                                                    "source" "(define-object model (box)
  :input-slots ((length 30) (width 20) (height 10))
  :computed-slots
  ((tolls (list (list :key :drawing :label \"SVG drawing\" :rivets 500)))
   (file-tolls (list :svg :drawing))))"))
                              (unless (eq (gethash "monetizable"
                                                   (json-of :get (format nil "~a?session=~a" (door "state") tolled)
                                                            :headers headers))
                                          t)
                                (error "a model with a toll on a download does not read as monetizable"))
                              (let ((deployment (deploy "title" "A plate")))
                                (unless (equal (gethash "url" deployment) address)
                                  (error "the deploy door did not answer the deployment's address"))
                                (unless (eql (gethash "fee_percent" deployment)
                                             (uiop:symbol-call :prompt-lab :house-fee-percent nil))
                                  (error "an open deployment does not carry the open fee")))
                              ;; the address sends to the viewer, asked here itself: this
                              ;; client does not follow a second redirect
                              (unless (page-ok? (format nil "~a/viewer?deployed=~a" prefix name))
                                (error "the deployed model's page did not open"))
                              (unless (search "define-object model" (http :get (format nil "~a/source" address)))
                                (error "an open deployment did not serve its source"))
                              ;; the download it priced is its owner's alone for now;
                              ;; the one it did not is anyone's
                              (unless (eql 402 (status drawing))
                                (error "a priced download opened to a stranger"))
                              (unless (eql 200 (status (format nil "~a?deployed=~a&format=pdf" (door "download") name)))
                                (error "a deployed model's free drawing was not to be had"))
                              (unless (eql 403 (nth-value 1 (http :post (door "deploy")
                                                                  :json (table "session" tolled "name" name))))
                                (error "a stranger deployed someone's session"))
                              (eql 200 (status (format nil "~a&owner=~a" drawing owner))))
                         (http :post (door "undeploy") :headers headers :json (table "name" name))
                         (unless (eql 404 (status address))
                           (error "the deployment is still there after it was taken down")))))))
          ;; closed source is chosen as a session opens: out of sight, its
          ;; deployment serving no source, at the closed fee
          (smoke "prompt-lab opens a session closed-source: unseen by others, deployed without its source"
                 (lambda ()
                   (let* ((opened (json-of :post (door "session") :json (table "closed" t)))
                          (closed (gethash "session" opened))
                          (headers (list (cons "X-Prompt-Lab-Owner" (gethash "owner" opened))))
                          (name "ci-smoke-closed"))
                     (json-of :post (door "model") :headers headers
                              :json (table "session" closed
                                           "source" "(define-object model (box)
  :input-slots ((length 30) (width 20) (height 10))
  :computed-slots
  ((tolls (list (list :key :drawing :label \"SVG drawing\" :rivets 500)))
   (file-tolls (list :svg :drawing))))"))
                     (unless (eql 403 (nth-value 1 (http :get (format nil "~a?session=~a" (door "state") closed))))
                       (error "a closed-source session showed itself to a stranger"))
                     (unwind-protect
                          (let ((deployment (json-of :post (door "deploy") :headers headers
                                                     :json (table "session" closed "name" name
                                                                  "payee" "owner@example.com"))))
                            (and (eq (gethash "closed" deployment) t)
                                 (eql (gethash "fee_percent" deployment)
                                      (uiop:symbol-call :prompt-lab :house-fee-percent t))
                                 (eql 404 (nth-value 1 (http :get (format nil "~a/d/~a/source" prefix name))))))
                       (http :post (door "undeploy") :headers headers :json (table "name" name))))))
          ;; a web app's tollbooth (kinds.lisp): declared by the app, paid
          ;; by its visitor -- here as a test payment -- and booked
          (smoke "prompt-lab web app takes a toll: unpaid, paid, and a test line in the books"
                 (lambda ()
                   (let* ((recipe (symbol-value (lab '#:*app-recipe*)))
                          (mark "(objects (list (the model))))")
                          (at (or (search mark recipe) (error "the recipe has lost the line the test puts a toll after")))
                          (tolled (concatenate 'string (subseq recipe 0 at)
                                               "(objects (list (the model))) (tolls (list (list :key :pass :label \"A pass\" :rivets 200))))"
                                               (subseq recipe (+ at (length mark)))))
                          (brief (json-of :post (door "agent")
                                          :json (table "event" "prompt" "text" "A tolled plate sizer." "kind" "app"
                                                       "model" tolled)))
                          (headers (list (cons "X-Prompt-Lab-Owner" (gethash "owner" brief))))
                          (app (gethash "session" brief))
                          (name "ci-smoke-toll"))
                     (json-of :post (door "agent") :headers headers
                              :json (table "event" "stopped" "session" app "text" "Only a test."))
                     (unwind-protect
                          (progn
                            ;; the author's slider: 20 points for the community pot
                            (let ((deployment (json-of :post (door "deploy") :headers headers
                                                       :json (table "session" app "name" name "payee" "owner@example.com"
                                                                    "pot_percent" 20))))
                              (unless (eql (gethash "pot_percent" deployment) 20)
                                (error "the author's share for the community pot was not kept with the deployment")))
                            (let* ((deployed (uiop:symbol-call :prompt-lab :ensure-deployed name))
                                   (page (gdl:make-object (uiop:symbol-call :prompt-lab :app-symbol deployed)
                                                          :deployment-name name))
                                   (before (length (uiop:symbol-call :prompt-lab :revenue-lines))))
                              (when (gdl:the-object page (toll-paid? :pass)) (error "the toll was paid before it was paid"))
                              (gdl:the-object page (pay-toll! :pass))
                              (unless (gdl:the-object page (toll-paid? :pass)) (error "the toll was not paid after it was"))
                              ;; the card cost comes off the top, then the fee (open
                              ;; source: a tenth) and the pot's 20 points, in rivets
                              (let ((line (uiop:symbol-call :prompt-lab :book-revenue! name 1000
                                                            :card-cents 60 :test? t)))
                                (unless (and (= (gethash "fee_rivets" line) 94)
                                             (= (gethash "pot_rivets" line) 188)
                                             (= (gethash "payee_rivets" line) 658)
                                             (= (gethash "fee_before_card_rivets" line) 100))
                                  (error "the card cost, the fee and the pot's share did not split as they should")))
                              (unless (eql 403 (nth-value 1 (http :get (format nil "~a?name=~a" (door "earnings") name))))
                                (error "a stranger read a deployment's earnings"))
                              (let* ((earned (json-of :get (format nil "~a?name=~a" (door "earnings") name)
                                                      :headers headers))
                                     (quarter (first (gethash "quarters_with_tests" earned)))
                                     (line (first (last (uiop:symbol-call :prompt-lab :revenue-lines) 2))))
                                (and (= (length (uiop:symbol-call :prompt-lab :revenue-lines)) (+ 2 before))
                                     (eql (gethash "gross_rivets" line) 200)
                                     (eql (gethash "pot_rivets" line) 40)
                                     (eq (gethash "test" line) t)
                                     (eql (gethash "payments" quarter) 2)
                                     (= (gethash "yours_rivets" quarter) (+ 140 658))
                                     (equal (gethash "unit" earned) "rivets")
                                     (zerop (length (gethash "quarters" earned)))
                                     (null (uiop:symbol-call :prompt-lab :revenue-report))))))
                       (http :post (door "undeploy") :headers headers :json (table "name" name))))))
          (setf (symbol-value (lab '#:*external-agent?*)) nil)
          ;; the community pot (page.lisp): what the lab has heard from a
          ;; gate that keeps one is everyone's to see, and an empty pot
          ;; builds for nobody.  There is no gate here, so the word is put
          ;; in by hand, and the lab kept from asking for a fresher one.
          (setf (symbol-value (lab '#:*pot-asked*)) (+ (get-universal-time) 3600)
                (symbol-value (lab '#:*pot*)) (list :credits 0 :max 9900 :room 9900 :topup? t
                                                    :amounts (list 1000 2000) :key ""))
          (smoke "prompt-lab shows a community pot to everyone, and an empty one refuses the prompt"
                 (lambda ()
                   (let ((config (json-of :get (door "config")))
                         (watched (json-of :get (format nil "~a?session=~a" (door "state") session))))
                     (unless (eql (gethash "max" (gethash "pot" config)) 9900)
                       (error "the config door does not show the pot"))
                     (unless (eql (gethash "credits" (gethash "pot" watched)) 0)
                       (error "a watcher's state does not show the pot"))
                     (multiple-value-bind (body status)
                         (http :post (door "prompt") :headers (owner-headers)
                                                     :json (table "session" session "prompt" "A plate."))
                       (or (and (eql status 429) (search "pot" body))
                           (error "the prompt door answered ~a: ~a" status
                                  (subseq body 0 (min 200 (length body)))))))))
          ;; behind a human check the pot is everybody's and the prompt
          ;; caps stand aside; and a prompt that carries no check token
          ;; rides the automated lane's allowance, a token is verified
          ;; (here: at a gate that is not there, so it fails), and the
          ;; allowance runs out.  The site key is pretended.
          (setf (symbol-value (lab '#:*pot*)) (list :credits 500 :max 9900 :room 9400 :topup? nil :amounts nil :key "")
                (symbol-value (lab '#:*turnstile-site-key*)) "smoke-site-key"
                (symbol-value (lab '#:*max-automated-prompts-per-day*)) 1)
          (smoke "prompt-lab lifts the prompt caps at a pot behind a human check, and takes automated prompts on an allowance"
                 (lambda ()
                   (flet ((state () (json-of :get (format nil "~a?session=~a" (door "state") session)
                                             :headers (owner-headers)))
                          (prompt (&rest more)
                            (http :post (door "prompt") :headers (owner-headers)
                                                        :json (apply #'table "session" session "prompt" "A plate." more))))
                     (unless (eq (gethash "prompts_unlimited" (state)) t)
                       (error "the caps still bind at a pot behind a human check"))
                     (multiple-value-bind (body status) (prompt "turnstile" "a-token")
                       (unless (eql status 403) (error "a token no gate can verify was answered ~a: ~a" status body)))
                     (multiple-value-bind (body status) (prompt)
                       (unless (and (eql status 202) (search "automated" body))
                         (error "a prompt with no token was answered ~a: ~a" status body)))
                     ;; the agent stops at once: there is no gate to call
                     (loop repeat 40 while (eq (gethash "busy" (state)) t) do (sleep 0.25))
                     (multiple-value-bind (body status) (prompt)
                       (or (and (eql status 429) (search "without the human check" body))
                           (error "the second prompt with no token was answered ~a: ~a" status body))))))
          (setf (symbol-value (lab '#:*pot*)) nil
                (symbol-value (lab '#:*pot-asked*)) 0
                (symbol-value (lab '#:*turnstile-site-key*)) nil)
          (smoke "prompt-lab viewer draws the model"
                 (lambda ()
                   (page-ok? (format nil "~a/viewer?session=~a&owner=~a" prefix session owner))))
          (smoke "prompt-lab viewer wears a skin, and the phone's sheet when asked"
                 (lambda ()
                   (let* ((skin (getf (first (uiop:symbol-call :prompt-lab :skins)) :name))
                          (body (http :get (format nil "~a/viewer?session=~a&skin=~a&mode=phone"
                                                   prefix session skin))))
                     (dolist (name (if (uiop:symbol-call :prompt-lab :sluice-skins?)
                                       (list "/sluice-static/tokens.css" "/sluice-static/skinned.css"
                                             (format nil "/sluice-static/skin-~a.css" skin)
                                             (format nil "~a/static/prompt-lab-phone.css" prefix))
                                       (list (format nil "~a/static/prompt-lab.css" prefix)
                                             (format nil "~a/static/prompt-lab-viewer.css" prefix)
                                             (format nil "~a/static/prompt-lab-phone.css" prefix)
                                             (format nil "~a/static/prompt-lab-~a.css" prefix skin)))
                                   t)
                       (unless (search name body)
                         (error "the viewer does not link ~a" name))))))
          (smoke "prompt-lab viewer asked for no skin's name wears the house look, and links nothing it should not"
                 (lambda ()
                   (let ((body (http :get (format nil "~a/viewer?session=~a&skin=..%2Fpage" prefix session))))
                     (and (or (search "/sluice-static/skinned.css" body)
                              (search "prompt-lab-viewer.css" body))
                          (not (search "/sluice-static/skin-" body))
                          (not (search "prompt-lab-phone.css" body))
                          (not (search "prompt-lab-page.css" body)))))))))))

(let* ((all (all-system-names))
       (systems (remove-if (lambda (s) (assoc s *not-loaded*)) all)))
  (format t "~&~a system~:p found; loading ~a: ~{~(~a~)~^ ~}~%"
          (length all) (length systems) systems)
  (dolist (s all)
    (let ((why (cdr (assoc s *not-loaded*))))
      (when why (format t "~&skip ~(~a~) -- ~a~%" s why))))
  (dolist (s systems) (load-one s))
  (setq *failures* (nreverse *failures*))
  (format t "~&~%~a of ~a systems loaded clean.~%"
          (- (length systems) (length *failures*)) (length systems))
  (when *failures*
    (format t "~&FAILED: ~{~(~a~)~^ ~}~%" (mapcar #'car *failures*)))
  (let ((loaded (remove-if (lambda (s) (assoc s *failures*)) systems)))
    (format t "~&~%Smoke run through what loaded, on port ~a~%" (http-port))
    (smoke-pages loaded)
    (when (member :x3dom-page loaded) (smoke-x3dom-page))
    (when (member :prompt-lab loaded)
      (smoke-prompt-lab (and (member :prompt-lab-sheet loaded) t))))
  (setq *smoke-failures* (nreverse *smoke-failures*))
  (format t "~&~%Smoke run: ~a failure~:p.~%" (length *smoke-failures*))
  (dolist (f *smoke-failures*) (format t "~&FAILED: ~a~%" (first f)))
  (finish-output)
  (uiop:quit (if (or *failures* *smoke-failures*) 1 0)))
