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
;;;; wanted: a system without source/file-ordering.isc loads its files
;;;; alphabetically, and one that sorts ahead of package.lisp breaks.
;;;;
;;;; Then a SMOKE RUN through what loaded: each demo is published on
;;;; the image's own web server and its page fetched (a gwl app mints
;;;; a session and redirects; a page whose model or markup fails
;;;; ends the connection early, which fails here too); the shared
;;;; stylesheet and the portal likewise; x3dom-page writes a page for
;;;; a box; and the prompt lab is driven through its doors with no
;;;; agent and no network -- config, a session, a hand-written model
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
  "Every system named by an .asd file one level below the checkout,
demos-common first since everything else depends on it."
  (let ((names (mapcar (lambda (p) (system-keyword (pathname-name p)))
                       (directory (merge-pathnames "*/*.asd" *demos-dir*)))))
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
(ql:register-local-projects)

;;;
;;; The smoke run.
;;;

;;; (system package publish-function path): each demo page, published
;;; with no host so it answers on localhost.
(defparameter *smoke-pages*
  '((:brick-wall :brick-wall-demo :publish-brick-wall! "/demo/brick-wall")
    (:bus :genworks.demos.bus :publish-bus! "/demo/bus")
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

(defun smoke-prompt-lab ()
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
        (smoke (format nil "prompt-lab page ~a" prefix) (lambda () (page-ok? prefix)))
        ;; The lab wears the sluice's skins where the image's sluice has
        ;; them, and its own copies where it is older; the checks below
        ;; hold either way, and say which they met.
        (format t "~&     the sluice of this image ~:[is older than its skins: the lab's own copies~;wears skins: the lab asks it~]~%"
                (uiop:symbol-call :prompt-lab :sluice-skins?))
        (smoke "prompt-lab page names its stylesheets, every place filled in"
               (lambda ()
                 (let ((body (http :get prefix)))
                   (when (search "{{" body) (error "a place in the page was left unfilled"))
                   (dolist (address (list (uiop:symbol-call :prompt-lab :tokens-url)
                                          (format nil "~a/static/prompt-lab-page.css" prefix)
                                          (format nil "~a/static/prompt-lab.js" prefix))
                                    t)
                     (unless (search address body)
                       (error "the page does not name ~a" address))))))
        (smoke "prompt-lab stylesheets, script and skins"
               (lambda ()
                 (let ((skins (mapcar (lambda (skin) (getf skin :href))
                                      (uiop:symbol-call :prompt-lab :skins)))
                       (split (uiop:symbol-call :prompt-lab :split-url)))
                   (unless skins (error "no skin was found"))
                   (dolist (address (append (list (uiop:symbol-call :prompt-lab :tokens-url))
                                            (mapcar (lambda (name) (format nil "~a/static/~a" prefix name))
                                                    '("prompt-lab.css" "prompt-lab-page.css"
                                                      "prompt-lab-viewer.css" "prompt-lab-phone.css"
                                                      "prompt-lab.js"))
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
                   (unless (equal (gethash "scope" manifest) prefix)
                     (error "the manifest's scope is ~a" (gethash "scope" manifest)))
                   (when (zerop (length icons)) (error "the manifest names no icon"))
                   ;; HEAD: a picture is no text for the client to decode
                   (map nil (lambda (icon)
                              (let ((status (nth-value 1 (http :head (gethash "src" icon)))))
                                (unless (eql status 200)
                                  (error "~a answered ~a" (gethash "src" icon) status))))
                        icons)
                   t)))
        (smoke "prompt-lab service worker, filled in, keeping the page and nothing of the doors"
               (lambda ()
                 (multiple-value-bind (body status) (http :get (format nil "~a/worker" prefix))
                   (unless (eql status 200) (error "the worker answered ~a" status))
                   (when (search "{{" body) (error "a place in the worker was left unfilled"))
                   (unless (search (format nil "var PREFIX = ~s;" prefix) body)
                     (error "the worker does not name the prefix"))
                   (let* ((start (search "var SHELL = " body))
                          (shell (subseq body start (position #\Newline body :start start))))
                     (unless (search (format nil "~a/static/prompt-lab.js" prefix) shell)
                       (error "the worker's shell lacks the page's script"))
                     (when (or (search "/api/" shell) (search "/viewer" shell))
                       (error "the worker's shell names a door or the viewer")))
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
    (when (member :prompt-lab loaded) (smoke-prompt-lab)))
  (setq *smoke-failures* (nreverse *smoke-failures*))
  (format t "~&~%Smoke run: ~a failure~:p.~%" (length *smoke-failures*))
  (dolist (f *smoke-failures*) (format t "~&FAILED: ~a~%" (first f)))
  (finish-output)
  (uiop:quit (if (or *failures* *smoke-failures*) 1 0)))
