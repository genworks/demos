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
          ;; code made in the lab is AGPL unless its session is closed: the
          ;; file says so; the body the page shows does not carry it
          (smoke "prompt-lab model file of an open session carries the AGPL header"
                 (lambda ()
                   (let* ((found (uiop:symbol-call :prompt-lab :find-session session))
                          (text (uiop:read-file-string (uiop:symbol-call :prompt-lab :session-model-file found))))
                     (and (search "SPDX-License-Identifier: AGPL-3.0-or-later" text)
                          ;; no name kept (no gate here): the line names the session
                          (search (format nil "the author of prompt-lab session ~a." session) text)
                          (not (search "SPDX" (uiop:symbol-call :prompt-lab :model-body found)))))))
          ;; the author's name, kept with the key at the gate, goes on the
          ;; line; here the gate's word is put in by hand
          (smoke "prompt-lab copyright line carries the author's kept name"
                 (lambda ()
                   (let* ((found (uiop:symbol-call :prompt-lab :find-session session))
                          (key "cccccccccccccccccccccc01")
                          (old (uiop:symbol-call :prompt-lab :session-wallet found)))
                     (unwind-protect
                          (progn
                            (eval `(setf (,(lab '#:session-wallet) ',found) ,key))
                            (uiop:symbol-call :prompt-lab :note-author-answer key
                                              (table "standing" "author" "name" "Grace Hopper"))
                            (let ((header (uiop:symbol-call :prompt-lab :license-header found)))
                              (and (search "Copyright (C) " header)
                                   (search " Grace Hopper." header)
                                   (search "AGPL-3.0-or-later" header))))
                       (eval `(setf (,(lab '#:session-wallet) ',found) ',old))))))
          ;; no unit is named to a developer (ruling 59): the lab's page says
          ;; no rivets, the free lab's words included
          (smoke "prompt-lab names no rivets to a developer, the free lab's page included"
                 (lambda ()
                   (flet ((page () (http :get prefix)))
                     (let ((plain (page))
                           (saved (symbol-value (lab '#:*free-lab?*))))
                       (setf (symbol-value (lab '#:*free-lab?*)) t)
                       (unwind-protect
                            (let ((free (page)))
                              (and (stringp plain) (plusp (length plain)) (not (search "rivet" plain))
                                   (stringp free) (search "community pot" free) (not (search "rivet" free))))
                         (setf (symbol-value (lab '#:*free-lab?*)) saved))))))
          ;; the author's AI allowance (rulings 46, 46b): the gate's word on the
          ;; key, kept with its standing, shown as a bar and about-tokens; past
          ;; it, the way on is the author's own agent
          (smoke "prompt-lab shows an author key's AI allowance: percent, tokens, and the way on past it"
                 (lambda ()
                   (let ((key "cccccccccccccccccccccc02"))
                     (uiop:symbol-call :prompt-lab :note-author-answer key
                                       (table "standing" "author" "ai_month" "2026-11"
                                              "ai_month_percent" 62 "ai_month_tokens" 6200 "ai_month_cents" 62))
                     (let ((html (uiop:symbol-call :prompt-lab :ai-allowance-html
                                                   (uiop:symbol-call :prompt-lab :author-ai key))))
                       (unless (and (search "62% used" html) (search "about 6.2k tokens" html) (search "width:62%" html)
                                    (not (search "renews" html)))
                         (error "the allowance under its cap reads wrong: ~a" html)))
                     (uiop:symbol-call :prompt-lab :note-author-answer key
                                       (table "standing" "author" "ai_month" "2026-11"
                                              "ai_month_percent" 100 "ai_month_tokens" 1400000))
                     (let ((html (uiop:symbol-call :prompt-lab :ai-allowance-html
                                                   (uiop:symbol-call :prompt-lab :author-ai key))))
                       (unless (and (search "100% used" html) (search "1.4M" html) (search "your own agent" html))
                         (error "the allowance at its cap reads wrong: ~a" html)))
                     (let ((html (uiop:symbol-call :prompt-lab :ai-allowance-html
                                                   (table "ai_month" "2026-11" "ai_month_tokens" 900))))
                       (unless (and (search "about 900 tokens" html) (not (search "%" html)))
                         (error "with no cap the allowance should show tokens alone: ~a" html)))
                     (when (symbol-value (lab '#:*meter?*))
                       (error "development is metered"))
                     t)))
          ;; the model switch: the gate's menu (here a stand-in for the gate's
          ;; models door), offered on the sheet, checked at the prompt, kept
          ;; on the session and sent with its calls
          (smoke "prompt-lab model switch: the gate's menu on the sheet, a pick checked, kept and sent"
                 (lambda ()
                   (let* ((menu-fn (lab '#:gate-menu))
                          (saved (fdefinition menu-fn))
                          (found (uiop:symbol-call :prompt-lab :find-session session))
                          (default (symbol-value (lab '#:*model*)))
                          (messages (uiop:symbol-call :prompt-lab :session-messages found)))
                     (flet ((set-messages (value)
                              ;; a structure's slot, set by name: no symbol of the lab's is read here
                              (eval (list 'setf (list (lab '#:session-messages) found) (list 'quote value)))))
                     (unless (null (uiop:symbol-call :prompt-lab :model-menu))
                       (error "a lab with no gate menu offers a switch"))
                     (when (search "data-bind:model" (http :get prefix))
                       (error "the sheet shows a switch with no menu"))
                     (setf (fdefinition menu-fn)
                           (lambda (&optional wallet)
                             (declare (ignore wallet))
                             (list (list :id default :label "Opus 5.5" :available t :allowed t)
                                   (list :id "house-1" :label "House model" :house t :standings '("trusted" "monetizing")
                                         :available t :allowed nil)
                                   (list :id "house-2" :label "House small" :house t :available t :allowed t)
                                   (list :id "house-dark" :label "House dark" :house t :available nil :allowed t))))
                     (unwind-protect
                          (let ((page (http :get prefix)))
                            (unless (and (search "data-bind:model" page) (search "House model (trusted and monetizing authors)" page)
                                         (search "House dark (not answering)" page) (search "disabled" page))
                              (error "the sheet's switch reads wrong"))
                            (unless (= 4 (length (gethash "models" (json-of :get (door "config")))))
                              (error "the config door does not list the menu"))
                            (flet ((refusal (model) (uiop:symbol-call :prompt-lab :model-refusal found model)))
                              (unless (and (null (refusal nil)) (null (refusal default)) (null (refusal "house-2"))
                                           (search "offered to trusted and monetizing" (refusal "house-1"))
                                           (search "not answering" (refusal "house-dark"))
                                           (search "not offered" (refusal "claude-nope")))
                                (error "a pick is checked wrong")))
                            (set-messages
                                  (list (table "role" "user" "content" "a plate")
                                        (table "role" "assistant"
                                               "content" (list (table "type" "thinking" "thinking" "..." "signature" "sig")
                                                               (table "type" "text" "text" "done")))))
                            (uiop:symbol-call :prompt-lab :set-session-model! found "house-2")
                            (unless (and (equal (uiop:symbol-call :prompt-lab :session-model found) "house-2")
                                         (search "house-2" (uiop:symbol-call :prompt-lab :request-body found)))
                              (error "the pick is not kept or not sent"))
                            (when (search "signature" (uiop:symbol-call :prompt-lab :request-body found))
                              (error "another model's thinking blocks went with the call"))
                            (unless (search "done" (uiop:symbol-call :prompt-lab :request-body found))
                              (error "the conversation's text was lost"))
                            (uiop:symbol-call :prompt-lab :set-session-model! found default)
                            (equal (uiop:symbol-call :prompt-lab :session-model found) default))
                       (setf (fdefinition menu-fn) saved)
                       (uiop:symbol-call :prompt-lab :set-session-model! found nil)
                       (set-messages messages))))))
          ;; MODEL A (2026-11-13): each model's rate on the menu, the fallback to
          ;; the model the gate serves past the AI allowance, a lab's default
          (smoke "prompt-lab Model A: rates on the menu, the fallback past the allowance, a lab's default model"
                 (lambda ()
                   (let* ((menu-fn (lab '#:gate-menu))
                          (api-fn (lab '#:call-messages-api))
                          (saved-menu (fdefinition menu-fn))
                          (saved-api (fdefinition api-fn))
                          (found (uiop:symbol-call :prompt-lab :find-session session))
                          (default (symbol-value (lab '#:*model*)))
                          (house-up t)
                          (calls nil)
                          (messages (uiop:symbol-call :prompt-lab :session-messages found)))
                     (flet ((set-messages (value)
                              (eval (list 'setf (list (lab '#:session-messages) found) (list 'quote value)))))
                       (setf (fdefinition menu-fn)
                             (lambda (&optional wallet)
                               (declare (ignore wallet))
                               (list (list :id default :label "Opus 5.5" :available t :allowed t
                                           :prices '(:input 4 :output 20 :cache-read 0.2 :cache-write 5))
                                     (list :id "claude-sonnet-5-5" :label "Sonnet 5.5" :available t :allowed t
                                           :prices '(:input 2 :output 10 :cache-read 0.1 :cache-write 2.5))
                                     (list :id "house-1" :label "House" :house t :available house-up :allowed t
                                           :past-allowance t :prices '(:input 0.08 :output 0.4 :cache-read 0.004 :cache-write 0.1)))))
                       (unwind-protect
                            (let ((page (http :get prefix))
                                  (used "this author key has used this month's AI allowance -- it renews on the 1st (UTC); House keeps working past it"))
                              (unless (and (search "Opus 5.5 -- 50x" page) (search "Sonnet 5.5 -- 25x" page)
                                           (search "House -- 1x -- keeps working past the allowance" page)
                                           (search "$model != $modelnow" page))
                                (error "the menu's rates or the switch warning read wrong"))
                              (unless (equal "50x" (gethash "rate" (first (gethash "models" (json-of :get (door "config"))))))
                                (error "the config door gives no rate"))
                              ;; the fallback: once, to the model that keeps working
                              (unless (equal "house-1" (getf (uiop:symbol-call :prompt-lab :allowance-fallback found used) :id))
                                (error "no fallback past the allowance"))
                              (when (uiop:symbol-call :prompt-lab :allowance-fallback found "some other refusal")
                                (error "a fallback for another refusal"))
                              ;; end to end: the gate refuses the month, the session goes on with the house
                              (setf (fdefinition api-fn)
                                    (lambda (s)
                                      (push (uiop:symbol-call :prompt-lab :session-model s) calls)
                                      (if (equal (first calls) "house-1")
                                          (values (table "type" "message" "role" "assistant" "stop_reason" "end_turn"
                                                         "content" (list (table "type" "text" "text" "built on the house"))
                                                         "usage" (table "input_tokens" 10 "output_tokens" 5))
                                                  200)
                                          (values (table "type" "error" "error" (table "type" "permission_error" "message" used))
                                                  403))))
                              (uiop:symbol-call :prompt-lab :set-session-model! found default)
                              (uiop:symbol-call :prompt-lab :run-prompt found "a washer")
                              ;; the prompt counted in the funnel's tally, given back: a later smoke counts it exactly
                              (uiop:symbol-call :prompt-lab :funnel-count! "prompts" -1)
                              (unless (and (equal calls (list "house-1" default))
                                           (equal (uiop:symbol-call :prompt-lab :session-model found) "house-1"))
                                (error "the session did not go on with the house: calls ~s" calls))
                              (when (uiop:symbol-call :prompt-lab :allowance-fallback found used)
                                (error "a second fallback from the house itself"))
                              ;; a lab's default model: the house when offered and answering
                              (uiop:symbol-call :prompt-lab :set-session-model! found nil)
                              (setf (symbol-value (lab '#:*session-default-model*)) "house-1")
                              (unless (equal (uiop:symbol-call :prompt-lab :default-session-model) "house-1")
                                (error "the lab's default model is not used"))
                              (setq house-up nil)
                              (unless (equal (uiop:symbol-call :prompt-lab :default-session-model) default)
                                (error "a dark default model is still used"))
                              t)
                         (setf (symbol-value (lab '#:*session-default-model*)) nil)
                         (setf (fdefinition menu-fn) saved-menu
                               (fdefinition api-fn) saved-api)
                         (uiop:symbol-call :prompt-lab :set-session-model! found nil)
                         (set-messages messages))))))
          ;; BRING YOUR OWN AGENT as a first choice (2026-11-13): a session
          ;; marked for the author's own agent refuses the lab's, its MCP door
          ;; hands over the lab's system prompt, the mark is kept in the
          ;; record, and the page offers the choice where the lab takes one
          (smoke "prompt-lab own agent: chosen, the lab's agent refused, the MCP instructions, kept"
                 (lambda ()
                   (let ((saved (symbol-value (lab '#:*external-agent?*)))
                         (own (uiop:symbol-call :prompt-lab :open-session! "10.9.8.7" nil)))
                     (setf (symbol-value (lab '#:*external-agent?*)) t)
                     (unwind-protect
                          (progn
                            (unless (search "pl-agentmode" (http :get prefix))
                              (error "the page offers no choice of agent"))
                            (uiop:symbol-call :prompt-lab :set-own-agent! own t)
                            (unless (uiop:symbol-call :prompt-lab :own-agent? own) (error "the mark did not take"))
                            (multiple-value-bind (started reason)
                                (uiop:symbol-call :prompt-lab :begin-prompt! own "a box" "10.9.8.7" nil :route? nil)
                              (when (or started (not (search "your own agent" (or reason ""))))
                                (error "the lab's agent was not refused: ~a" reason)))
                            (let* ((answer (uiop:symbol-call :prompt-lab :mcp-answer own
                                                             (table "jsonrpc" "2.0" "id" 1 "method" "initialize"
                                                                    "params" (table "protocolVersion" "2025-06-18"))))
                                   (instructions (gethash "instructions" (gethash "result" answer))))
                              (unless (and (stringp instructions)
                                           (search (subseq (uiop:symbol-call :prompt-lab :system-text own) 0 60) instructions))
                                (error "the MCP door does not hand over the lab's system prompt")))
                            (uiop:symbol-call :prompt-lab :save-session! own)
                            (let ((json (uiop:symbol-call :prompt-lab :read-record
                                                          (merge-pathnames "session.json"
                                                                           (uiop:symbol-call :prompt-lab :session-directory own))))
                                  (token (uiop:symbol-call :prompt-lab :mcp-token own)))
                              (unless (and json (equal (gethash "agent" json) "own"))
                                (error "the record does not keep whose agent builds"))
                              ;; the session's own MCP token: in a header, scoped, replaceable,
                              ;; never in the record (which the archive copies whole)
                              (unless (stringp token) (error "no MCP token was minted"))
                              (when (search token (uiop:read-file-string
                                                   (merge-pathnames "session.json" (uiop:symbol-call :prompt-lab :session-directory own))))
                                (error "the MCP token is in session.json"))
                              (flet ((mcp (headers)
                                       (nth-value 1 (http :post (format nil "~a/mcp?session=~a" prefix (uiop:symbol-call :prompt-lab :session-id own))
                                                          :headers headers
                                                          :json (table "jsonrpc" "2.0" "id" 1 "method" "ping")))))
                                (unless (eql 200 (mcp (list (cons "Authorization" (format nil "Bearer ~a" token)))))
                                  (error "the MCP token in a header was refused"))
                                (unless (eql 403 (mcp (list (cons "Authorization" "Bearer plmcp_not-the-token"))))
                                  (error "a wrong MCP token was taken"))
                                (unless (eql 200 (mcp (list (cons "X-Prompt-Lab-Owner" (uiop:symbol-call :prompt-lab :session-owner own)))))
                                  (error "the owner key no longer opens the door"))
                                (uiop:symbol-call :prompt-lab :mint-mcp-token! own)
                                (unless (eql 403 (mcp (list (cons "Authorization" (format nil "Bearer ~a" token)))))
                                  (error "a replaced MCP token still opens the door"))))
                            (uiop:symbol-call :prompt-lab :set-own-agent! own nil)
                            (multiple-value-bind (started reason)
                                (uiop:symbol-call :prompt-lab :begin-prompt! own "" "10.9.8.7" nil :route? nil)
                              (declare (ignore started))
                              (when (search "your own agent" (or reason ""))
                                (error "the lab's agent is still refused after switching back")))
                            t)
                       (setf (symbol-value (lab '#:*external-agent?*)) saved)
                       (ignore-errors (uiop:symbol-call :prompt-lab :delete-session own))))))
          ;; THE AUTHORSHIP RECORD (ruling 96): a hand edit is kept as the
          ;; author's in versions.jsonl, and the owner's download says so
          (smoke "prompt-lab authorship record: a hand edit marked, the owner's alone"
                 (lambda ()
                   (let* ((found (uiop:symbol-call :prompt-lab :find-session session))
                          (source (uiop:symbol-call :prompt-lab :model-body found))
                          (edited (format nil "~a~%;; a hand edit for the authorship record~%" source)))
                     (unless (eql 200 (nth-value 1 (http :post (door "model") :headers (owner-headers)
                                                         :json (table "session" session "source" edited))))
                       (error "the hand edit was refused"))
                     (let* ((url (format nil "~a?session=~a&format=authorship" (door "download") session))
                            (record (http :get url :headers (owner-headers))))
                       (unless (and (stringp record)
                                    (search "# Authorship record" record)
                                    (search "HAND EDIT: the author, by hand" record)
                                    (search "a hand edit for the authorship record" record)
                                    (search "SHA-256" record))
                         (error "the authorship record does not mark the hand edit"))
                       (unless (eql 404 (nth-value 1 (http :get url)))
                         (error "a stranger got the authorship record")))
                     ;; the file put back as it was, for the smokes after this one
                     (http :post (door "model") :headers (owner-headers) :json (table "session" session "source" source))
                     t)))
          ;; the author takes their own source at any time; nobody else does
          (smoke "prompt-lab source download: the owner's file, a stranger's 404"
                 (lambda ()
                   (let ((url (format nil "~a?session=~a&format=source" (door "download") session)))
                     (multiple-value-bind (body status) (http :get url :headers (owner-headers))
                       (unless (and (eql status 200) (search "define-object model" body) (search "SPDX" body))
                         (error "the owner's source download answered ~a" status)))
                     (multiple-value-bind (body status) (http :get url)
                       (declare (ignore body))
                       (unless (eql status 404) (error "a stranger's source download answered ~a" status)))
                     t)))
          ;; THE SOURCE PAGE's image record (ruling 95, deploy.lisp): of the
          ;; repositories an image lists in its COMMITS file, the bundled
          ;; libraries' tree joins the components; nothing else an image
          ;; records is named; only a public host's https origin is linked
          (smoke "prompt-lab source page: the image's COMMITS read, only the libraries named, only public hosts linked"
                 (lambda ()
                   (let ((file (merge-pathnames "ci-COMMITS" (uiop:temporary-directory)))
                         (saved (symbol-value (lab '#:*image-commits-files*))))
                     (with-open-file (out file :direction :output :if-exists :supersede)
                       (format out "gendl 1111111111111111111111111111111111111111 devo https://gitlab.common-lisp.net/gendl/gendl.git~%~
                                    quicklisp 2222222222222222222222222222222222222222 devo https://private.example.com/genworks/quicklisp.git~%~
                                    build-scripts 3333333333333333333333333333333333333333 main https://github.com/example/build-scripts.git~%"))
                     (setf (symbol-value (lab '#:*image-commits-files*)) (list (namestring file)))
                     (unwind-protect
                          (let ((rows (uiop:symbol-call :prompt-lab :image-commits)))
                            ;; quicklisp alone, its private origin not linked
                            (unless (and (= (length rows) 1)
                                         (equal (getf (first rows) :label) "quicklisp")
                                         (equal (getf (first rows) :commit) "2222222222222222222222222222222222222222")
                                         (null (getf (first rows) :repository)))
                              (error "the image's COMMITS were read as ~s" rows))
                            ;; a repository the page shows, on a public host, is linked
                            (progv (list (lab '#:*image-commits-shown*)) (list (list "quicklisp" "build-scripts"))
                              (let ((public (find "build-scripts" (uiop:symbol-call :prompt-lab :image-commits)
                                                  :key (lambda (row) (getf row :label)) :test #'equal)))
                                (unless (and public (search "https://github.com/" (getf public :repository)))
                                  (error "a public host's origin was not linked: ~s" public))))
                            ;; a private origin with a public mirror links the mirror
                            (progv (list (lab '#:*origin-mirrors*))
                                (list (list (cons "https://private.example.com/genworks/quicklisp"
                                                  "https://gitlab.common-lisp.net/example/quicklisp")))
                              (let ((row (first (uiop:symbol-call :prompt-lab :image-commits))))
                                (unless (equal (getf row :repository) "https://gitlab.common-lisp.net/example/quicklisp")
                                  (error "a mirrored origin was not linked to its mirror: ~s" row))))
                            (when (symbol-value (lab '#:*origin-mirrors*))
                              (error "the lab ships a mirror table before a mirror exists"))
                            t)
                       (setf (symbol-value (lab '#:*image-commits-files*)) saved)))))
          ;; THE HOUSE PRIMER (ruling 106, agent.lisp, primers/house-gendl.md):
          ;; every worked example compiles and builds through write_model and
          ;; check_model, as the agent's would; a house model's session gets
          ;; the primer, no render tool and the longer per-call limit
          (smoke "prompt-lab house primer: every worked example builds; the house session's text, tools and limit"
                 (lambda ()
                   (flet ((pl (name &rest args) (apply #'uiop:symbol-call :prompt-lab name args)))
                     (let* ((primer (pl :house-primer-text))
                            (examples (let ((from (search "Worked examples" primer)) (found nil))
                                        (loop for start = (and from (search (format nil "```lisp~%") primer :start2 from))
                                              while start
                                              do (let ((end (search (format nil "~%```") primer :start2 (+ start 8))))
                                                   (push (subseq primer (+ start 8) end) found)
                                                   (setq from (+ end 4))))
                                        (nreverse found)))
                            (sizes '((1200 700 750) (300 90 90) (800 300 1800) (200 200 40))))
                       (unless (= (length examples) 4) (error "the primer has ~d worked examples, not 4" (length examples)))
                       ;; the grammar's own first example compiles too (a reserved
                       ;; slot name in it once taught the very mistake it warns of)
                       (let ((first-block (let ((start (search (format nil "```lisp~%") primer)))
                                            (subseq primer (+ start 8) (search (format nil "~%```") primer :start2 (+ start 8)))))
                             ;; an address of its own: the examples above use 10.9.8.5's day of sessions
                             (s (pl :open-session! "10.9.8.3" nil)))
                         (unwind-protect
                              (when (nth-value 1 (pl :write-model s first-block))
                                (error "the primer's first example does not compile"))
                           (ignore-errors (pl :delete-session s))
                           (pl :funnel-count! "sessions" -1)))
                       (loop for source in examples
                             for size in sizes
                             for s = (pl :open-session! "10.9.8.5" nil)
                             do (unwind-protect
                                     (multiple-value-bind (blocks error?) (pl :write-model s source)
                                       (when error? (error "a worked example does not compile: ~s" blocks))
                                       (let ((check (format nil "~{~a~}" (mapcar (lambda (b) (or (cdr (assoc "text" b :test #'string=)) ""))
                                                                                  (pl :check-model s :expected-size size)))))
                                         (format t "~&HOUSE PRIMER EXAMPLE ~s:~%~a~%" size check)
                                         (when (or (search "rror" check) (not (search "Expected size matched" check)))
                                           (error "a worked example does not build to its size: ~a" check))))
                                  (ignore-errors (pl :delete-session s))
                                  (pl :funnel-count! "sessions" -1)))
                       ;; the house session: its primer, no render, the longer limit
                       (let ((s (pl :open-session! "10.9.8.5" nil)))
                         (unwind-protect
                              (progn
                                (pl :set-session-model! s "house-1")
                                (unless (and (search "Worked examples" (pl :system-text s))
                                             (search "after EVERY change that compiles, then view_model" (pl :system-text s)))
                                  (error "a house session's system text lacks the house primer"))
                                (when (find "render" (coerce (pl :tool-definitions s) 'list)
                                            :key (lambda (tool) (cdr (assoc "name" tool :test #'string=))) :test #'equal)
                                  (error "a house session is offered render"))
                                (unless (eql (pl :call-seconds s) (symbol-value (lab '#:*house-call-seconds*)))
                                  (error "a house session's calls keep the short limit"))
                                (unless (eql (pl :max-rounds s) (symbol-value (lab '#:*house-max-rounds*)))
                                  (error "a house session keeps the lab model's round cap"))
                                ;; the parenthesis report a house model gets on a file that does not read
                                (unless (and (search "line 1" (or (pl :paren-report (format nil "(define-object model (base-object)~% :objects ((a :type 'box))")) ""))
                                             (search "extra ')' on line 2" (or (pl :paren-report (format nil "(a)~%(b))")) ""))
                                             (null (pl :paren-report (format nil "(a \"(\" #\\( ; (~%)"))))
                                  (error "the parenthesis report is wrong"))
                                (pl :set-session-model! s nil)
                                (when (search "Worked examples" (pl :system-text s))
                                  (error "a session on the lab's own model got the house primer"))
                                t)
                           (ignore-errors (pl :delete-session s))
                           (pl :funnel-count! "sessions" -1)))))))
          ;; STRUCTURE AND SIGHT for a house model (tools.lisp): slots edited by
          ;; name with the parentheses written for it, an unbalanced value
          ;; refused before anything is written, and the model seen in words
          (smoke "prompt-lab house structure and sight: slot edits, refusals, view_model"
                 (lambda ()
                   (flet ((pl (name &rest args) (apply #'uiop:symbol-call :prompt-lab name args)))
                     (let ((s (pl :open-session! "10.9.8.2" nil)))
                       (unwind-protect
                            (flet ((tool (name &rest pairs)
                                     (multiple-value-bind (blocks error?)
                                         (pl :run-tool s name (loop for (k v) on pairs by #'cddr collect (cons k v)))
                                       (values (format nil "~{~a~}" (mapcar (lambda (b) (or (cdr (assoc "text" b :test #'string=)) "")) blocks))
                                               error?)))
                                   (body () (pl :model-body-text s))
                                   (names (session) (mapcar (lambda (tool) (cdr (assoc "name" tool :test #'string=)))
                                                            (coerce (pl :tool-definitions session) 'list))))
                              (pl :set-session-model! s "house-1")
                              (unless (and (member "set_slot" (names s) :test #'equal) (member "view_model" (names s) :test #'equal)
                                           (not (member "edit_model" (names s) :test #'equal)))
                                (error "a house session's tools are not the structural ones: ~s" (names s)))
                              ;; and its rules name those tools and the table's colour keys
                              (let ((text (pl :model-system-text s)))
                                (when (or (search "edit_model" text) (not (search "set_slot" text))
                                          (search "saddle" text))
                                  (error "a house session's rules still name edit_model or a colour the table lacks")))
                              (pl :write-model s "(define-object model (base-object)
  :input-slots ((width 600) (length 400) (height 300))
  :objects ((top :type 'box :width (the width) :length (the length) :height 20
                 :center (translate (the center) :up (- (half (the height)) 10)))
            (leg :type 'box :width 20 :length 20 :height 280
                 :center (translate (the center) :down 10))))")
                              (multiple-value-bind (text error?) (tool "set_slot" "object" "model" "slot" "width" "value" "800")
                                (when (or error? (not (search "(width 800)" (body)))) (error "set_slot did not set: ~a" text)))
                              (let ((before (body)))
                                (multiple-value-bind (text error?) (tool "set_slot" "object" "model" "slot" "width" "value" "(+ 1 2")
                                  (unless (and error? (search "Nothing written" text) (equal before (body)))
                                    (error "an unbalanced value was not refused: ~a" text))))
                              (tool "add_slot" "object" "model" "section" "computed-slots" "slot" "gap" "value" "50")
                              (unless (search "(gap 50)" (body)) (error "add_slot made no section entry: ~a" (body)))
                              (unless (nth-value 1 (tool "add_slot" "object" "model" "section" "input-slots" "slot" "width" "value" "1"))
                                (error "add_slot took a name already used"))
                              (tool "remove_slot" "object" "model" "slot" "gap")
                              (when (search "gap" (body)) (error "remove_slot left the entry"))
                              (unless (nth-value 1 (tool "replace_form" "name" "model" "source" "(defun other () 1)"))
                                (error "replace_form took a form that defines something else"))
                              ;; a CHILD goes by its own name, one a call: a section's
                              ;; name or a list of children is refused with the recipe
                              (let ((before (body)))
                                (dolist (call '(("set_slot" "object" "model" "slot" "objects" "value" "((shelf :type 'box))")
                                                ("add_slot" "object" "model" "section" "objects" "slot" "objects" "value" ":type 'box")
                                                ("add_slot" "object" "model" "section" "objects" "slot" "shelf"
                                                 "value" "(shelf :type 'box :width 10 :length 10 :height 10)")
                                                ("set_slot" "object" "model" "slot" "leg" "value" "((leg :type 'box))")
                                                ("remove_slot" "object" "model" "slot" ":objects")))
                                  (multiple-value-bind (text error?) (apply #'tool call)
                                    (unless (and error? (search "add_slot with object" text) (equal before (body)))
                                      (error "~a was not refused with the child recipe: ~a" (first call) text)))))
                              (multiple-value-bind (text error?)
                                  (tool "add_slot" "object" "model" "section" "objects" "slot" "shelf"
                                        "value" ":type 'box :width 100 :length 100 :height 10 :center (translate (the center) :up 50)")
                                (when (or error? (not (search "(shelf :type 'box" (body))))
                                  (error "add_slot did not add a child: ~a" text)))
                              (tool "remove_slot" "object" "model" "slot" "shelf")
                              (when (search "shelf" (body)) (error "remove_slot left the child"))
                              (let ((view (tool "view_model")))
                                (unless (and (search "on the floor" view) (search "touches" view) (search "Symmetry" view)
                                             (not (search "LOOSE" view)))
                                  (error "view_model's account is wrong: ~a" view)))
                              ;; lift the top off its leg: it floats
                              (tool "set_slot" "object" "model" "slot" "top"
                                    "value" ":type 'box :width (the width) :length (the length) :height 20 :center (translate (the center) :up 400)")
                              (let ((view (tool "view_model")))
                                (unless (search "LOOSE" view) (error "view_model did not see a floating part: ~a" view)))
                              ;; the author-view-edit loop: under *house-auto-view* an edit
                              ;; carries the brief view, opened by the mark the tally reads
                              (flet ((auto (name) (format nil "~{~a~}" (mapcar (lambda (b) (or (cdr (assoc "text" b :test #'string=)) ""))
                                                                               (pl :auto-view-blocks s name)))))
                                (when (plusp (length (auto "set_slot"))) (error "an edit was shown a view with the loop off"))
                                (progv (list (lab '#:*house-auto-view*)) (list :text)
                                  (let ((seen (auto "set_slot")))
                                    (unless (and (search (symbol-value (lab '#:*auto-view-mark*)) seen)
                                                 (search "LOOSE" seen) (search "Symmetry" seen)
                                                 (not (search "- top (" seen)))
                                      (error "the brief auto-view is wrong: ~a" seen)))
                                  (when (plusp (length (auto "check_model"))) (error "a check was shown a view as if it were an edit"))
                                  (unless (search "AUTO-VIEW" (pl :model-system-text s))
                                    (error "the house rules do not say the view comes with each change"))))
                              ;; building by structure ALONE: no whole-file write; objects begin
                              ;; as skeletons from new_object_def; a whole define-object is
                              ;; replaced only after two structural edits of it have failed
                              (progv (list (lab '#:*house-structure?*)) (list :only)
                                (when (or (member "write_model" (names s) :test #'equal)
                                          (member "edit_model" (names s) :test #'equal)
                                          (not (member "new_object_def" (names s) :test #'equal)))
                                  (error "a structure-only session's tools are wrong: ~s" (names s)))
                                (multiple-value-bind (text error?) (tool "new_object_def" "name" "bracket" "mixins" "box")
                                  (when (or error? (not (search "(define-object bracket (box)" (body))))
                                    (error "new_object_def did not write a skeleton: ~a" text)))
                                (unless (nth-value 1 (tool "new_object_def" "name" "bracket"))
                                  (error "new_object_def took a name already defined"))
                                (unless (nth-value 1 (tool "new_object_def" "name" "bad name!"))
                                  (error "new_object_def took a name that is not one"))
                                (multiple-value-bind (text error?) (tool "add_slot" "object" "bracket" "section" "input-slots" "slot" "thickness" "value" "5")
                                  (when (or error? (not (search "(thickness 5)" (body))))
                                    (error "add_slot did not fill the skeleton: ~a" text)))
                                (multiple-value-bind (text error?) (tool "replace_form" "name" "bracket" "source" "(define-object bracket (box))")
                                  (unless (and error? (search "not written whole" text))
                                    (error "a whole define-object was taken before any structural edit failed: ~a" text)))
                                (tool "set_slot" "object" "bracket" "slot" "nosuch" "value" "1")
                                (tool "set_slot" "object" "bracket" "slot" "nosuch" "value" "2")
                                (multiple-value-bind (text error?) (tool "replace_form" "name" "bracket" "source" "(define-object bracket (box) :input-slots ((thickness 6)))")
                                  (when (or error? (not (search "(thickness 6)" (body))))
                                    (error "the escape hatch did not open after two failed edits: ~a" text)))
                                ;; many entries in one call, all or nothing
                                (multiple-value-bind (text error?)
                                    (tool "add_slot" "object" "bracket"
                                          "entries" (list (list (cons "section" "input-slots") (cons "slot" "hole") (cons "value" "8"))
                                                          (list (cons "section" "objects") (cons "slot" "nut")
                                                                (cons "value" ":type 'box :width 10 :length 10 :height 5"))))
                                  (when (or error? (not (search "(hole 8)" (body))) (not (search "(nut :type 'box" (body))))
                                    (error "add_slot's entries were not all written: ~a" text)))
                                (let ((before (body)))
                                  (multiple-value-bind (text error?)
                                      (tool "add_slot" "object" "bracket"
                                            "entries" (list (list (cons "section" "input-slots") (cons "slot" "slot-a") (cons "value" "1"))
                                                            (list (cons "section" "objects") (cons "slot" "objects") (cons "value" ":type 'box"))))
                                    (unless (and error? (search "entry 2 of 2" text) (equal before (body)))
                                      (error "a batch with a bad entry was not refused whole: ~a" text))))
                                (multiple-value-bind (text error?)
                                    (tool "new_object_def" "name" "plate" "mixins" "base-object"
                                          "entries" (list (list (cons "section" "input-slots") (cons "slot" "thick") (cons "value" "3"))))
                                  (when (or error? (not (search "(define-object plate (base-object)" (body))) (not (search "(thick 3)" (body))))
                                    (error "new_object_def's entries were not written: ~a" text)))
                                (unless (search "new_object_def" (pl :model-system-text s))
                                  (error "the structure-only rules do not name new_object_def"))
                                ;; a refusal there never sends the model to the write_model it lacks
                                (multiple-value-bind (text error?) (tool "set_slot" "object" "bracket" "slot" "thickness" "value" "(+ 1 2")
                                  (when (or (not error?) (search "write_model" text))
                                    (error "a structure-only refusal named write_model, or none came: ~a" text)))
                                (unless (search "ONE OR TWO add_slot CALLS" (pl :model-system-text s))
                                  (error "the structure-only rules do not ask for the first version in batches"))
                                ;; a new structure-only session starts with MODEL's skeleton
                                (let ((s2 (pl :open-session! "10.9.8.3" nil)))
                                  (unwind-protect
                                       (progn
                                         (pl :set-session-model! s2 "house-1")
                                         (pl :ensure-model-skeleton! s2)
                                         (unless (search "(define-object model (base-object)" (pl :model-body-text s2))
                                           (error "a structure-only session did not start with MODEL's skeleton: ~a" (pl :model-body-text s2))))
                                    (ignore-errors (pl :delete-session s2))
                                    (pl :funnel-count! "sessions" -1))))
                              ;; a house model that sees gets render back (*house-vision-models*)
                              (when (pl :render-offered?)
                                (when (member "render" (names s) :test #'equal)
                                  (error "a text-only house model was offered render"))
                                (progv (list (lab '#:*house-vision-models*)) (list (list "house-1"))
                                  (unless (member "render" (names s) :test #'equal)
                                    (error "a house model that sees was not offered render")))
                                ;; a text-only house model with a viewer named gets look_at_model
                                (progv (list (lab '#:*house-viewer-model*)) (list "viewer-1")
                                  (unless (member "look_at_model" (names s) :test #'equal)
                                    (error "a text-only house model with a viewer was not offered look_at_model"))
                                  (progv (list (lab '#:*house-vision-models*)) (list (list "house-1"))
                                    (when (member "look_at_model" (names s) :test #'equal)
                                      (error "a house model that sees was offered a viewer too")))))
                              ;; a house model's thinking budget goes into its request alone
                              (unless (search "\"budget_tokens\":4096" (pl :request-body s))
                                (error "a house session does not ask the 4096 default budget"))
                              (progv (list (lab '#:*house-thinking-budget*)) (list nil)
                                (when (search "budget_tokens" (pl :request-body s)) (error "a thinking budget was sent with none set")))
                              (progv (list (lab '#:*house-thinking-budget*)) (list 4096)
                                (unless (search "\"budget_tokens\":4096" (pl :request-body s))
                                  (error "the house thinking budget was not sent: ~a" (subseq (pl :request-body s) 0 300)))
                                (pl :set-session-model! s nil)
                                (when (search "budget_tokens" (pl :request-body s))
                                  (error "a thinking budget was sent for a model that is not the house's"))
                                (pl :set-session-model! s "house-1"))
                              ;; the lab's own model keeps its tools
                              (pl :set-session-model! s nil)
                              (when (member "set_slot" (names s) :test #'equal) (error "a session on the lab's model got the house tools"))
                              t)
                         (ignore-errors (pl :delete-session s))
                         (pl :funnel-count! "sessions" -1))))))
          ;; a streamed tool call whose arguments are not JSON (a house model
          ;; that closed its string early, 2026-11-14) comes back to the model
          ;; as a tool error it can retry, not as the prompt's end: the stream
          ;; is scripted by standing in for post-sse
          (smoke "prompt-lab a tool call whose arguments are not JSON is a tool error, not the prompt's end"
                 (lambda ()
                   (flet ((pl (name &rest args) (apply #'uiop:symbol-call :prompt-lab name args)))
                     (let* ((sse (lab '#:post-sse))
                            (saved (fdefinition sse))
                            (calls 0)
                            (seen nil)
                            (s (pl :open-session! "10.9.8.3" nil)))
                       (unwind-protect
                            (progv (list (lab '#:*stream-replies?*)) (list t)
                              (setf (fdefinition sse)
                                    (lambda (url body &key headers seconds on-event)
                                      (declare (ignore url headers seconds))
                                      (push body seen)
                                      (flet ((event (name json) (funcall on-event name json)))
                                        (event "message_start" "{\"message\": {\"role\": \"assistant\", \"content\": [], \"usage\": {\"input_tokens\": 10}}}")
                                        (cond ((= (incf calls) 1)
                                               (event "content_block_start" "{\"index\": 0, \"content_block\": {\"type\": \"tool_use\", \"id\": \"toolu_x1\", \"name\": \"write_model\", \"input\": {}}}")
                                               (event "content_block_delta" "{\"index\": 0, \"delta\": {\"type\": \"input_json_delta\", \"partial_json\": \"{\\\"source\\\": \\\"(define-object model (base-object)\"}}")
                                               (event "content_block_stop" "{\"index\": 0}")
                                               (event "message_delta" "{\"delta\": {\"stop_reason\": \"tool_use\"}, \"usage\": {\"output_tokens\": 5}}"))
                                              (t
                                               (event "content_block_start" "{\"index\": 0, \"content_block\": {\"type\": \"text\", \"text\": \"\"}}")
                                               (event "content_block_delta" "{\"index\": 0, \"delta\": {\"type\": \"text_delta\", \"text\": \"done\"}}")
                                               (event "content_block_stop" "{\"index\": 0}")
                                               (event "message_delta" "{\"delta\": {\"stop_reason\": \"end_turn\"}, \"usage\": {\"output_tokens\": 1}}"))))
                                      (values 200 nil nil)))
                              (pl :run-prompt s "a box")
                              (let ((log (pl :session-log s)))
                                (unless (and (= calls 2)
                                             (find-if (lambda (e) (and (eq (second e) :tool-error) (search "did not arrive as valid JSON" (third e)))) log)
                                             (find :done log :key #'second))
                                  (error "the call with arguments that are not JSON was not answered as a tool error: ~a calls, ~s"
                                         calls (mapcar #'second log))))
                              ;; the second request carried the error back as the call's result
                              (unless (search "\"is_error\":true" (remove #\Space (first seen)))
                                (error "the tool error did not reach the model"))
                              t)
                         (setf (fdefinition sse) saved)
                         ;; the prompt, its tool error and its done counted in the funnel's
                         ;; tally, given back: a later smoke counts it exactly
                         (pl :funnel-count! "prompts" -1)
                         (pl :funnel-count! "tool-errors" -1)
                         (pl :funnel-count! "builds-done" -1)
                         (ignore-errors (pl :delete-session s))
                         (pl :funnel-count! "sessions" -1))))))
          ;; THE COMPLETION CHECK (Ruling 144): a reply that ends a prompt counts
          ;; as done only when MODEL builds, every part computes and every
          ;; computed slot of MODEL computes; otherwise it is logged not done
          (smoke "prompt-lab the completion check: a broken model's 'done' is not done"
                 (lambda ()
                   (flet ((pl (name &rest args) (apply #'uiop:symbol-call :prompt-lab name args)))
                     (let* ((sse (lab '#:post-sse))
                            (saved (fdefinition sse))
                            (s (pl :open-session! "10.9.8.4" nil)))
                       (flet ((last-kind () (second (car (last (pl :session-log s)))))
                              (last-text () (third (car (last (pl :session-log s))))))
                         (unwind-protect
                              (progv (list (lab '#:*stream-replies?*)) (list t)
                                (setf (fdefinition sse)
                                      (lambda (url body &key headers seconds on-event)
                                        (declare (ignore url body headers seconds))
                                        (flet ((event (name json) (funcall on-event name json)))
                                          (event "message_start" "{\"message\": {\"role\": \"assistant\", \"content\": [], \"usage\": {\"input_tokens\": 10}}}")
                                          (event "content_block_start" "{\"index\": 0, \"content_block\": {\"type\": \"text\", \"text\": \"\"}}")
                                          (event "content_block_delta" "{\"index\": 0, \"delta\": {\"type\": \"text_delta\", \"text\": \"All done.\"}}")
                                          (event "content_block_stop" "{\"index\": 0}")
                                          (event "message_delta" "{\"delta\": {\"stop_reason\": \"end_turn\"}, \"usage\": {\"output_tokens\": 3}}"))
                                        (values 200 nil nil)))
                                ;; a part that does not compute, and a result that does not
                                (pl :write-model s "(define-object model (base-object)
  :computed-slots ((ratio (/ 1 0)))
  :objects ((plate :type 'box :width 100 :length 50 :height 10)
            (bracket :type 'box :width (the nosuch) :length 5 :height 5)))")
                                (pl :run-prompt s "a plate and a bracket")
                                (unless (and (eq (last-kind) :stopped) (search "Not done" (last-text))
                                             (search "bracket" (last-text)) (search "ratio" (last-text)))
                                  (error "a broken model's 'done' was not caught: ~s ~a" (last-kind) (last-text)))
                                ;; mended, the same reply is done
                                (pl :write-model s "(define-object model (base-object)
  :computed-slots ((ratio (/ 1 2)))
  :objects ((plate :type 'box :width 100 :length 50 :height 10)))")
                                (pl :run-prompt s "mend it")
                                (unless (eq (last-kind) :done)
                                  (error "a complete model's 'done' was refused: ~s ~a" (last-kind) (last-text)))
                                t)
                           (setf (fdefinition sse) saved)
                           (pl :funnel-count! "prompts" -2)
                           (pl :funnel-count! "builds-stopped" -1)
                           (pl :funnel-count! "builds-done" -1)
                           (ignore-errors (pl :delete-session s))
                           (pl :funnel-count! "sessions" -1)))))))
          ;; THE LAB'S LEFTOVERS (Ruling 188), each a case that went wrong before:
          ;; a session that did nothing is not done; the check judges the FILE,
          ;; not an older class still loaded; a child with no geometry is no
          ;; broken part; a check that runs out of time judges nothing; a house
          ;; model is one the gate's menu marks, whatever its id; the thinking
          ;; budget is sent only when it is a sane one; the last round says it
          ;; is the last, so the reply after it is words; a reply cut on the
          ;; way still counts its tokens; the viewer's call is the session's
          (smoke "prompt-lab leftovers: an empty model, a stale class, the house menu, the budget, the last round, cut calls"
                 (lambda ()
                   (flet ((pl (name &rest args) (apply #'uiop:symbol-call :prompt-lab name args)))
                     (let* ((sse (lab '#:post-sse))
                            (saved (fdefinition sse))
                            (post (lab '#:post-json))
                            (saved-post (fdefinition post))
                            (script nil)
                            (failed nil)
                            (menu (find-symbol (string '#:*house-menu*) :prompt-lab))
                            (s (pl :open-session! "10.9.8.7" nil))
                            (plate "(define-object model (base-object)
  :computed-slots ((ratio (/ 1 2)))
  :objects ((plate :type 'box :width 100 :length 50 :height 10)))"))
                       (flet ((last-kind () (second (car (last (pl :session-log s)))))
                              (last-text () (third (car (last (pl :session-log s)))))
                              (usage (key) (or (getf (pl :session-usage s) key) 0))
                              ;; each case on its own: one that fails does not hide the rest
                              (part (label thunk)
                                (handler-case (funcall thunk)
                                  (error (condition) (push (format nil "~a: ~a" label condition) failed))))
                              (words (on-event text)
                                (funcall on-event "content_block_start" "{\"index\": 0, \"content_block\": {\"type\": \"text\", \"text\": \"\"}}")
                                (funcall on-event "content_block_delta"
                                         (format nil "{\"index\": 0, \"delta\": {\"type\": \"text_delta\", \"text\": \"~a\"}}" text))
                                (funcall on-event "content_block_stop" "{\"index\": 0}")))
                         (unwind-protect
                              (progv (cons (lab '#:*stream-replies?*) (and menu (list menu)))
                                  ;; the gate's menu as last read: one house model, under an id of its own
                                  (list t (cons (get-universal-time) (list "gate-house-9")))
                                (setf (fdefinition sse)
                                      (lambda (url body &key headers seconds on-event)
                                        (declare (ignore url headers seconds))
                                        (flet ((event (name json) (funcall on-event name json)))
                                          (event "message_start" "{\"message\": {\"role\": \"assistant\", \"content\": [], \"usage\": {\"input_tokens\": 1000}}}")
                                          (ecase script
                                            (:words
                                             (words on-event "All done.")
                                             (event "message_delta" "{\"delta\": {\"stop_reason\": \"end_turn\"}, \"usage\": {\"output_tokens\": 3}}"))
                                            ;; tools for ever, until told the round was the last
                                            (:tools
                                             (cond ((search "LAST ROUND" body)
                                                    (words on-event "Closing words.")
                                                    (event "message_delta" "{\"delta\": {\"stop_reason\": \"end_turn\"}, \"usage\": {\"output_tokens\": 3}}"))
                                                   (t
                                                    (event "content_block_start" "{\"index\": 0, \"content_block\": {\"type\": \"tool_use\", \"id\": \"t1\", \"name\": \"read_model\", \"input\": {}}}")
                                                    (event "content_block_stop" "{\"index\": 0}")
                                                    (event "message_delta" "{\"delta\": {\"stop_reason\": \"tool_use\"}, \"usage\": {\"output_tokens\": 3}}"))))
                                            ;; 300 characters of reply, then the stream ends with no stop
                                            (:cut (words on-event (make-string 300 :initial-element #\x)))
                                            ;; the same, and the call never comes back
                                            (:lost (words on-event (make-string 300 :initial-element #\x))
                                             (error "The call did not answer."))))
                                        (values 200 nil nil)))
                                ;; a house model by the gate's word alone
                                (part "the gate's house models" (lambda ()
                                (pl :set-session-model! s "gate-house-9")
                                (unless (pl :house-model? s)
                                  (error "a model the gate's menu marks as the house's is not treated as one"))
                                (pl :set-session-model! s "some-other-model")
                                (when (pl :house-model? s) (error "a model no list and no menu names is treated as a house model"))))
                                (pl :set-session-model! s "house-1")
                                (part "the thinking budget" (lambda ()
                                ;; the thinking budget: none for 0, a negative number or
                                ;; a fraction; held under the reply's own limit
                                (dolist (bad (list 0 -5 0.5))
                                  (progv (list (lab '#:*house-thinking-budget*)) (list bad)
                                    (when (search "budget_tokens" (pl :request-body s))
                                      (error "a thinking budget of ~a was sent" bad))))
                                (progv (list (lab '#:*house-thinking-budget*)) (list 999999)
                                  (let ((most (- (symbol-value (lab '#:*max-tokens*)) (symbol-value (lab '#:*house-reply-room*)))))
                                    (unless (search (format nil "\"budget_tokens\":~d" most) (pl :request-body s))
                                      (error "a thinking budget over the reply's limit was not held under it"))))))
                                (part "a session that built nothing" (lambda ()
                                ;; a structure-only session that does nothing: MODEL is the
                                ;; empty skeleton, and a reply of words alone is not done
                                (progv (list (lab '#:*house-structure?*)) (list :only)
                                  (setq script :words)
                                  (pl :run-prompt s "a plate")
                                  (unless (and (eq (last-kind) :stopped) (search "empty" (last-text)))
                                    (error "a session that built nothing was logged ~s: ~a" (last-kind) (last-text))))))
                                (pl :set-session-model! s nil)
                                (part "the file and the class" (lambda ()
                                ;; the file and the class: a good version, then one that
                                ;; does not compile -- the old class stays loaded, and the
                                ;; check must not call the session complete
                                (pl :write-model s plate)
                                (when (pl :completion-problems s) (error "a sound model has problems: ~a" (pl :completion-problems s)))
                                (pl :write-model s "(define-object model (base-object)
  :computed-slots ((ratio (no-such-package-zz:f 1))))")
                                (unless (search "did not compile or load" (format nil "~{~a~}" (pl :completion-problems s)))
                                  (error "a model file that does not compile was judged by the class still loaded"))
                                (pl :write-model s plate)
                                (when (pl :completion-problems s) (error "mended, the file is still called stale"))))
                                (part "a child with no geometry" (lambda ()
                                ;; a child with no geometry is not a broken part
                                (pl :write-model s "(define-object numbers () :computed-slots ((n 1)))
(define-object model (base-object)
  :objects ((figures :type 'numbers)
            (plate :type 'box :width 100 :length 50 :height 10)))")
                                (when (pl :completion-problems s)
                                  (error "a child with no geometry was called a broken part: ~a" (pl :completion-problems s)))))
                                (part "a check out of time" (lambda ()
                                ;; a check that runs out of time judges nothing
                                (pl :write-model s "(define-object model (base-object)
  :computed-slots ((slow (progn (sleep 3) 1)))
  :objects ((plate :type 'box :width 100 :length 50 :height 10)))")
                                (progv (list (lab '#:*eval-seconds*)) (list 1)
                                  (unless (getf (pl :completion-findings s) :unjudged)
                                    (error "a check that ran out of time did not say so: ~s" (pl :completion-findings s)))
                                  (when (pl :completion-problems s)
                                    (error "a check that ran out of time was read as a model that does not build")))))
                                (part "the last round" (lambda ()
                                ;; the last round allowed says it is the last, and the
                                ;; reply after it is words, not a call thrown away
                                (pl :write-model s plate)
                                (progv (list (lab '#:*max-rounds*)) (list 2)
                                  (setq script :tools)
                                  (pl :run-prompt s "look twice")
                                  (unless (and (eq (last-kind) :done) (search "Closing words" (last-text)))
                                    (error "the round cap still ends on a reply thrown away: ~s ~a" (last-kind) (last-text))))))
                                (part "cut calls" (lambda ()
                                ;; a reply cut on the way counts what the stream reported
                                ;; and an estimate of what arrived
                                (let ((input (usage :input)) (output (usage :output)))
                                  (setq script :cut)
                                  (pl :run-prompt s "cut")
                                  (unless (and (eq (last-kind) :stopped) (= (- (usage :input) input) 1000)
                                               (= (- (usage :output) output) 100))
                                    (error "a cut reply added ~a input and ~a output tokens"
                                           (- (usage :input) input) (- (usage :output) output)))
                                  (setq script :lost)
                                  (pl :run-prompt s "lost")
                                  (unless (and (eq (last-kind) :stopped) (= (- (usage :input) input) 2000))
                                    (error "a call that never came back added no usage")))))
                                (part "the viewer's call" (lambda ()
                                ;; the viewer's call names the session's wallet and its
                                ;; tokens join the session's.  The drawing is stood in
                                ;; for, so the case runs where the image has no renderer.
                                (let* ((render (lab '#:render))
                                       (saved-render (fdefinition render))
                                       (sent nil) (input (usage :input)))
                                  (unwind-protect
                                       (progn
                                         (setf (fdefinition render)
                                               (lambda (session &key layout)
                                                 (declare (ignore session layout))
                                                 (values (list (pl :text-result "a drawing")) nil))
                                               (fdefinition post)
                                               (lambda (url body &key headers seconds)
                                                 (declare (ignore url body seconds))
                                                 (setq sent headers)
                                                 (values 200 "{\"content\": [{\"type\": \"text\", \"text\": \"A plate.\"}], \"usage\": {\"input_tokens\": 700, \"output_tokens\": 40}}" nil)))
                                         (eval `(setf (,(lab '#:session-wallet) ,s) "0123456789abcdef01234567"))
                                         (progv (list (lab '#:*house-viewer-model*)) (list "viewer-1")
                                           (pl :look-at-model s)))
                                    (setf (fdefinition render) saved-render))
                                  (unless (equal (cdr (assoc (symbol-value (lab '#:*wallet-header*)) sent :test #'equal))
                                                 "0123456789abcdef01234567")
                                    (error "the viewer's call did not name the session's wallet"))
                                  (unless (= (- (usage :input) input) 700)
                                    (error "the viewer's tokens did not join the session's usage")))))
                                (when failed (error "~{~a~^ | ~}" (reverse failed)))
                                t)
                           (setf (fdefinition sse) saved
                                 (fdefinition post) saved-post)
                           (pl :funnel-count! "prompts" -4)
                           (pl :funnel-count! "builds-stopped" -3)
                           (pl :funnel-count! "builds-done" -1)
                           (ignore-errors (pl :delete-session s))
                           (pl :funnel-count! "sessions" -1)))))))
          ;; AUTOMATIC STAGING (Ruling 153): a long request, *auto-stage* on, goes
          ;; as four stages in one session; only the last is judged complete
          (smoke "prompt-lab automatic staging: a long request in four stages, the check on the last"
                 (lambda ()
                   (flet ((pl (name &rest args) (apply #'uiop:symbol-call :prompt-lab name args)))
                     (let* ((sse (lab '#:post-sse))
                            (saved (fdefinition sse))
                            (sent nil)
                            (refuse? nil)
                            (s (pl :open-session! "10.9.8.6" nil))
                            (long (format nil "A test request that is long enough to stage. ~a" (make-string 620 :initial-element #\x))))
                       (unwind-protect
                            (progv (list (lab '#:*stream-replies?*) (lab '#:*auto-stage*)) (list t t)
                              (setf (fdefinition sse)
                                    (lambda (url body &key headers seconds on-event)
                                      (declare (ignore url headers seconds))
                                      (push body sent)
                                      (flet ((event (name json) (funcall on-event name json)))
                                        (if refuse?
                                            ;; the API refusing: an error event, as the stream gives it
                                            (event "error" "{\"type\": \"error\", \"error\": {\"type\": \"overloaded_error\", \"message\": \"busy\"}}")
                                            (progn
                                              (event "message_start" "{\"message\": {\"role\": \"assistant\", \"content\": [], \"usage\": {\"input_tokens\": 10}}}")
                                              (event "content_block_start" "{\"index\": 0, \"content_block\": {\"type\": \"text\", \"text\": \"\"}}")
                                              (event "content_block_delta" "{\"index\": 0, \"delta\": {\"type\": \"text_delta\", \"text\": \"Stage done.\"}}")
                                              (event "content_block_stop" "{\"index\": 0}")
                                              (event "message_delta" "{\"delta\": {\"stop_reason\": \"end_turn\"}, \"usage\": {\"output_tokens\": 3}}"))))
                                      (values 200 nil nil)))
                              ;; a model with a broken part: stages 1-3 are not judged, the last is
                              (pl :write-model s "(define-object model (base-object)
  :objects ((plate :type 'box :width 100 :length 50 :height 10)
            (bracket :type 'box :width (the nosuch) :length 5 :height 5)))")
                              ;; house models only (Ruling 167): the lab's own model is not staged
                              (pl :run-prompt s long)
                              (unless (= (length sent) 1) (error "a request on the lab's own model was staged: ~d calls" (length sent)))
                              (pl :set-session-model! s "house-1")
                              (setq sent nil)
                              (let ((before (length (pl :session-log s))))
                                (pl :run-prompt s long)
                                (let* ((log (nthcdr before (pl :session-log s)))
                                       (prompts (remove :prompt log :key #'second :test-not #'eq))
                                       (stages (remove :stage log :key #'second :test-not #'eq))
                                       (ends (remove-if-not (lambda (e) (member (second e) '(:done :stopped))) log)))
                                  ;; one request is ONE prompt (Ruling 167), its stages logged as stages
                                  (unless (and (= (length prompts) 1) (equal (third (first prompts)) long)
                                               (= (length stages) 4) (= (length sent) 4)
                                               (search "Stage 1 of 4" (third (first stages))) (search "xxxx" (third (first stages)))
                                               (search "Stage 4 of 4" (third (fourth stages))) (search "xxxx" (third (fourth stages)))
                                               (equal (mapcar #'second ends) '(:done :done :done :stopped))
                                               (search "Not done" (third (car (last ends))))
                                               (not (pl :session-busy? s)))
                                    (error "staging went wrong: ~d prompts, ~d stages, ~d calls, ends ~s"
                                           (length prompts) (length stages) (length sent) (mapcar #'second ends)))))
                              ;; a stage refused: the rest are not sent (Ruling 167)
                              (setq sent nil refuse? t)
                              (pl :run-prompt s long)
                              (unless (and (= (length sent) 1) (not (pl :session-busy? s))
                                           (find-if (lambda (e) (and (eq (second e) :note) (search "stages stopped" (third e))))
                                                    (pl :session-log s)))
                                (error "the stages went on after a refusal: ~d calls" (length sent)))
                              (setq refuse? nil)
                              ;; a short request is not staged
                              (setq sent nil)
                              (pl :run-prompt s "a short one")
                              (unless (= (length sent) 1) (error "a short request was staged: ~d calls" (length sent)))
                              t)
                         (setf (fdefinition sse) saved)
                         (pl :funnel-count! "prompts" -4)
                         (pl :funnel-count! "builds-done" -3)
                         (pl :funnel-count! "builds-stopped" -4)
                         (ignore-errors (pl :delete-session s))
                         (pl :funnel-count! "sessions" -1))))))
          ;; a house model's reply cut off at max_tokens goes back into the
          ;; conversation as one line, not its thinking (the staged arm of
          ;; 2026-11-15 filled a 128K context with three of them)
          (smoke "prompt-lab a house model's cut-off reply is dropped from the conversation"
                 (lambda ()
                   (flet ((pl (name &rest args) (apply #'uiop:symbol-call :prompt-lab name args)))
                     (let* ((sse (lab '#:post-sse))
                            (saved (fdefinition sse))
                            (s (pl :open-session! "10.9.8.3" nil)))
                       (unwind-protect
                            (progv (list (lab '#:*stream-replies?*)) (list t)
                              (setf (fdefinition sse)
                                    (lambda (url body &key headers seconds on-event)
                                      (declare (ignore url body headers seconds))
                                      (flet ((event (name json) (funcall on-event name json)))
                                        (event "message_start" "{\"message\": {\"role\": \"assistant\", \"content\": [], \"usage\": {\"input_tokens\": 10}}}")
                                        (event "content_block_start" "{\"index\": 0, \"content_block\": {\"type\": \"thinking\", \"thinking\": \"\"}}")
                                        (event "content_block_delta" "{\"index\": 0, \"delta\": {\"type\": \"thinking_delta\", \"thinking\": \"long thoughts that never end\"}}")
                                        (event "content_block_stop" "{\"index\": 0}")
                                        (event "message_delta" "{\"delta\": {\"stop_reason\": \"max_tokens\"}, \"usage\": {\"output_tokens\": 32000}}"))
                                      (values 200 nil nil)))
                              (pl :set-session-model! s "house-1")
                              (pl :run-prompt s "a box")
                              (let* ((last (car (last (pl :session-messages s))))
                                     (content (and (hash-table-p last) (gethash "content" last))))
                                (unless (and (equal (gethash "role" last) "assistant")
                                             (= (length content) 1)
                                             (equal (gethash "type" (first content)) "text")
                                             (search "cut off" (gethash "text" (first content))))
                                  (error "the cut-off reply was kept: ~s" content)))
                              t)
                         (setf (fdefinition sse) saved)
                         (pl :funnel-count! "prompts" -1)
                         (pl :funnel-count! "builds-stopped" -1)
                         (ignore-errors (pl :delete-session s))
                         (pl :funnel-count! "sessions" -1))))))
          ;; a model or an expression that recurses without end overflows the
          ;; stack -- a storage-condition, not an error -- which took a whole
          ;; Lisp down on 2026-11-14; the tools answer it as an error instead
          (smoke "prompt-lab tools survive a stack overflow"
                 (lambda ()
                   (flet ((pl (name &rest args) (apply #'uiop:symbol-call :prompt-lab name args)))
                     (let ((s (pl :open-session! "10.9.8.4" nil)))
                       (unwind-protect
                            (multiple-value-bind (blocks error?)
                                (pl :evaluate-expression s "(labels ((deeper (n) (+ 1 (deeper n)))) (deeper 1))")
                              (unless (and error? (search "rror" (cdr (assoc "text" (first blocks) :test #'string=))))
                                (error "an endless recursion was not answered as an error: ~s" blocks))
                              (pl :write-model s "(defun deeper (n) (+ 1 (deeper n)))
(define-object model (base-object)
  :computed-slots ((depth (deeper 1)))
  :objects ((plate :type 'box :width (the depth) :length 10 :height 10)))")
                              (multiple-value-bind (blocks error?) (pl :check-model s)
                                (unless error? (error "check_model of an endless recursion did not answer an error: ~s" blocks)))
                              ;; outside the tools: the Monetize read and the download door
                              (when (pl :built-tolls s) (error "built-tolls read tolls from a model that cannot build"))
                              (let ((status (nth-value 1 (http :get (format nil "~a?session=~a&format=pdf" (door "download") (pl :session-id s))
                                                               :headers (list (cons "X-Prompt-Lab-Owner" (pl :session-owner s)))))))
                                (unless (eql status 422)
                                  (error "the download door answered ~a for a model that overflows the stack" status)))
                              (unless (page-ok? prefix) (error "the lab stopped answering after the overflows"))
                              t)
                         (ignore-errors (pl :delete-session s))
                         (pl :funnel-count! "sessions" -1))))))
          ;; THE CLICKWRAP (ruling 94, guards.lisp): versioned texts with their
          ;; hashes; Build, the own agent, an external agent and Deploy each wait
          ;; for the tick, Declare for none (ruling 99); the acceptance log is chained, names no
          ;; owner key and is kept out of the archive
          (let* ((root (merge-pathnames "ci-terms/" (uiop:temporary-directory)))
                 (log-root (merge-pathnames "ci-acceptances/" (uiop:temporary-directory)))
                 (saved (mapcar #'(lambda (name) (cons name (symbol-value (lab name))))
                                '(#:*terms-root* #:*terms-log-root* #:*external-agent?* #:*closed-source?*
                                  #:*deployments?* #:*free-lab?*)))
                 (fresh (lambda ()
                          (setf (symbol-value (lab '#:*terms-log-head*)) nil
                                (symbol-value (lab '#:*key-acceptances*)) nil)
                          (clrhash (symbol-value (lab '#:*terms-cache*))))))
            (ensure-directories-exist root)
            (uiop:delete-directory-tree log-root :validate t :if-does-not-exist :ignore)
            (with-open-file (out (merge-pathnames "terms.md" root) :direction :output :if-exists :supersede)
              (format out "Version: ci-1~%~%The terms, for the smoke run.~%"))
            (with-open-file (out (merge-pathnames "author-agreement.md" root) :direction :output :if-exists :supersede)
              (format out "Version: ci-1~%~%The author agreement, for the smoke run.~%"))
            (setf (symbol-value (lab '#:*terms-root*)) (namestring root)
                  (symbol-value (lab '#:*terms-log-root*)) (namestring log-root)
                  (symbol-value (lab '#:*external-agent?*)) t
                  (symbol-value (lab '#:*closed-source?*)) t
                  (symbol-value (lab '#:*deployments?*)) t
                  (symbol-value (lab '#:*free-lab?*)) nil)
            (funcall fresh)
            (unwind-protect
                 (smoke "prompt-lab clickwrap: versioned texts, the tick at Build, agent and Deploy, none at Declare, the log"
                        (lambda ()
                          (flet ((pl (name &rest args) (apply #'uiop:symbol-call :prompt-lab name args))
                                 (refused? (reason) (search "First accept" (or reason ""))))
                            (let* ((build (pl :terms-set :build))
                                   (terms (find "terms" build :key (lambda (d) (getf d :id)) :test #'equal)))
                              ;; the texts: the root's, and the lab's own notice beside them
                              (unless (and terms (find "open-source" build :key (lambda (d) (getf d :id)) :test #'equal)
                                           (equal (getf terms :version) "ci-1")
                                           (equal (getf terms :sha256)
                                                  (pl :watch-sha256-hex (uiop:read-file-string (merge-pathnames "terms.md" root)))))
                                (error "the build texts are not versioned and hashed: ~s" build))
                              (unless (search "The terms, for the smoke run" (http :get (format nil "~a/terms/terms" prefix)))
                                (error "the terms door does not answer the text"))
                              (unless (eql 404 (nth-value 1 (http :get (format nil "~a/terms/nothing" prefix))))
                                (error "an unknown text was answered"))
                              (unless (gethash "terms" (json-of :get (door "config")))
                                (error "the config door does not name the texts"))
                              (unless (search "pl-tagree" (http :get prefix))
                                (error "the page asks for no tick before a build")))
                            (let* ((s (pl :open-session! "10.9.8.6" nil))
                                   (closed (pl :open-session! "10.9.8.6" nil :closed? t))
                                   (owner (list (cons "X-Prompt-Lab-Owner" (pl :session-owner s))))
                                   (id (pl :session-id s)))
                              (unwind-protect
                                   (progn
                                     ;; BUILD: refused without the tick, before anything costs
                                     (unless (refused? (nth-value 1 (pl :begin-prompt! s "a box" "10.9.8.6" nil :route? nil)))
                                       (error "a first build was let through without the tick"))
                                     (unless (eql 403 (nth-value 1 (http :post (door "prompt") :headers owner
                                                                         :json (table "session" id "prompt" "a box"))))
                                       (error "the prompt door let a first build through without the tick"))
                                     ;; THE OWN AGENT'S MCP DOOR: refused until accepted
                                     (pl :set-own-agent! s t)
                                     (flet ((ping () (nth-value 1 (http :post (format nil "~a/mcp?session=~a" prefix id) :headers owner
                                                                        :json (table "jsonrpc" "2.0" "id" 1 "method" "ping")))))
                                       (unless (eql 403 (ping)) (error "the MCP door served a session whose terms were not accepted"))
                                       ;; AN EXTERNAL AGENT: accept_terms with the prompt
                                       (pl :set-own-agent! s nil)
                                       (unless (eql 403 (nth-value 1 (http :post (door "agent") :headers owner
                                                                           :json (table "event" "prompt" "session" id "text" "A plate."))))
                                         (error "the agent door took a first prompt without accept_terms"))
                                       (json-of :post (door "agent") :headers owner
                                                :json (table "event" "prompt" "session" id "text" "A plate." "accept_terms" t))
                                       (json-of :post (door "agent") :headers owner
                                                :json (table "event" "stopped" "session" id "text" "Only a test."))
                                       (unless (pl :session-accepted? s :build) (error "the agent door's acceptance was not noted"))
                                       (unless (eql 200 (ping)) (error "the MCP door refused an accepted session")))
                                     ;; accepted, the build goes on to the next check (busy here)
                                     (funcall (fdefinition (list 'setf (find-symbol "SESSION-BUSY?" :prompt-lab))) t s)
                                     (when (refused? (nth-value 1 (pl :begin-prompt! s "a box" "10.9.8.6" nil :route? nil)))
                                       (error "an accepted session was still asked to accept"))
                                     (funcall (fdefinition (list 'setf (find-symbol "SESSION-BUSY?" :prompt-lab))) nil s)
                                     ;; a new version of a text asks again
                                     (with-open-file (out (merge-pathnames "terms.md" root) :direction :output :if-exists :supersede)
                                       (format out "Version: ci-2~%~%The terms, changed.~%"))
                                     (clrhash (symbol-value (lab '#:*terms-cache*)))
                                     (when (pl :session-accepted? s :build) (error "a changed text was taken as accepted"))
                                     ;; DEPLOY: every deployment waits for the tick
                                     (unless (refused? (nth-value 1 (pl :deploy-session! s :name "ci-terms")))
                                       (error "a deployment went ahead without the tick"))
                                     (when (refused? (nth-value 1 (pl :deploy-session! s :name "ci-terms" :accepted t)))
                                       (error "a ticked deployment was still refused for the terms"))
                                     ;; DECLARE: nothing to tick (ruling 99)
                                     (unless (pl :decide-closed-work! (pl :session-id closed) (pl :session-owner closed) "declare")
                                       (error "a declaration was refused with nothing to tick"))
                                     ;; THE LOG: chained, the texts' versions and hashes, no owner key, not in the archive
                                     (let* ((file (pl :terms-log-file))
                                            (text (uiop:read-file-string file)))
                                       (unless (pl :terms-log-verify file) (error "the acceptance log's chain does not verify"))
                                       (unless (and (search "\"scope\":\"build\"" text) (search "ci-1" text)
                                                    (search "agent door" text))
                                         (error "the log does not record the acceptances"))
                                       (when (search "\"scope\":\"declare\"" text)
                                         (error "a declaration was logged as an acceptance"))
                                       (when (or (search (pl :session-owner s) text) (search (pl :session-owner closed) text))
                                         (error "the log holds an owner key"))
                                       (let ((archive (symbol-value (lab '#:*archive-root*))))
                                         (when (and archive (search (namestring archive) (namestring file)))
                                           (error "the log is in the archive")))
                                       ;; a line changed afterwards breaks the chain
                                       (let ((copy (merge-pathnames "tampered.jsonl" log-root)))
                                         (with-open-file (out copy :direction :output :if-exists :supersede)
                                           (write-string (uiop:frob-substrings text '("agent door") "agent doer") out))
                                         (when (pl :terms-log-verify copy) (error "a tampered log verified"))))
                                     t)
                                (ignore-errors (pl :delete-session s))
                                (ignore-errors (pl :decide-closed-work! (pl :session-id closed) (pl :session-owner closed) "delete"))
                                (ignore-errors (pl :delete-session closed))
                                ;; the two sessions, the prompt and the stop joined the funnel's tally: given back
                                (pl :funnel-count! "sessions" -2)
                                (pl :funnel-count! "prompts" -1)
                                (pl :funnel-count! "builds-stopped" -1))))))
              (dolist (pair saved) (setf (symbol-value (lab (car pair))) (cdr pair)))
              (funcall fresh)))
          ;; CLOSED WORK in a free lab (closed.lisp): an author key may close
          ;; a work, a keyless visitor may not; thirty days from its opening;
          ;; a declaration keeps it closed; an undecided one is opened by the
          ;; sweep once its time is out, its files given the AGPL lines; delete
          ;; leaves nothing; the house's door wants its token.
          (let ((saved (mapcar #'(lambda (name) (cons name (symbol-value (lab name))))
                               '(#:*free-lab?* #:*deployments?* #:*closed-source?* #:*house-token-file*)))
                (token-file (merge-pathnames "ci-house-token" (uiop:temporary-directory))))
            (setf (symbol-value (lab '#:*free-lab?*)) t
                  (symbol-value (lab '#:*deployments?*)) nil
                  (symbol-value (lab '#:*closed-source?*)) nil)
            (unwind-protect
                 (smoke "prompt-lab closed work: an author key's, thirty days, declared with nothing ticked, swept open, deleted, extended by the house"
                        (lambda ()
                          (flet ((call (name &rest args) (apply #'uiop:symbol-call :prompt-lab name args))
                                 (open-closed (address key)
                                   (let ((session (uiop:symbol-call :prompt-lab :open-session! address key :closed? t)))
                                     (or session (error "no closed session for ~a" address))))
                                 (record (id)
                                   (uiop:symbol-call :prompt-lab :read-record
                                                     (merge-pathnames "session.json" (uiop:symbol-call :prompt-lab :archived-directory id))))
                                 (archived-model (id)
                                   (uiop:read-file-string (merge-pathnames "model.lisp" (uiop:symbol-call :prompt-lab :archived-directory id)))))
                            (let ((key "dddddddddddddddddddddd01"))
                              (unless (and (null (call :closed-allowed? nil)) (call :closed-allowed? key))
                                (error "a free lab's closed work is not keyed to an author key"))
                              ;; a keyless visitor asks to close and gets an open session
                              (let ((open (call :open-session! "10.9.9.1" nil :closed? t)))
                                (when (call :session-closed? open) (error "a keyless visitor's session was closed"))
                                (call :delete-session open))
                              ;; 1: declared
                              (let* ((one (open-closed "10.9.9.2" key))
                                     (id (call :session-id one))
                                     (owner (call :session-owner one)))
                                (unless (call :session-closed? one) (error "an author key's closed session is open"))
                                (unless (eql (call :closed-deadline id) (+ (call :session-created one) (* 30 86400)))
                                  (error "the clock is not thirty days from the opening"))
                                (call :write-model one *smoke-model*)
                                (let ((text (uiop:read-file-string (call :session-model-file one))))
                                  (unless (and (search "All rights reserved." text) (not (search "SPDX" text)))
                                    (error "a closed work's file does not reserve its rights")))
                                ;; no box to tick and no address needed (rulings, 2026-11-13);
                                ;; no request for more time on the page
                                (when (call :decide-closed-work! id owner "more-time")
                                  (error "the page's request for more time is still taken"))
                                (when (call :decide-closed-work! id owner "declare" :email "not an address")
                                  (error "a malformed address was taken"))
                                (let ((notice (call :decide-closed-work! id owner "declare")))
                                  (unless (and notice (search "genworks.com/royalties/" notice))
                                    (error "the declaration without an address was refused, or gave no reminder")))
                                (when (member id (call :sweep-closed-works! :now (+ (get-universal-time) (* 40 86400)) :force? t)
                                              :test #'equal)
                                  (error "a declared work was opened by the sweep"))
                                (unless (call :closed-state id owner) (error "the declared work is no longer closed")))
                              ;; 2: undecided, its session ended, opened by the sweep
                              (let* ((two (open-closed "10.9.9.3" key))
                                     (id (call :session-id two)))
                                (call :write-model two *smoke-model*)
                                (call :delete-session two)
                                (let ((json (record id)))
                                  (unless (and json (eq (gethash "closed" json) 'yason:true) (eq (gethash "private" json) 'yason:true))
                                    (error "an ended closed work did not stay closed in the archive")))
                                (when (member id (call :sweep-closed-works! :force? t) :test #'equal)
                                  (error "a work inside its thirty days was opened"))
                                (unless (member id (call :sweep-closed-works! :now (+ (get-universal-time) (* 31 86400)) :force? t)
                                                :test #'equal)
                                  (error "the sweep did not open a work past its time"))
                                (let ((json (record id)))
                                  (unless (and (eq (gethash "private" json) 'yason:false) (eq (gethash "closed" json) 'yason:false))
                                    (error "the opened work's record is still closed")))
                                (let ((text (archived-model id)))
                                  (unless (and (search "SPDX-License-Identifier: AGPL-3.0-or-later" text)
                                               (not (search "All rights reserved." text)))
                                    (error "the opened work's file was not given the AGPL lines"))))
                              ;; 3: deleted
                              (let* ((three (open-closed "10.9.9.4" key))
                                     (id (call :session-id three)))
                                (call :write-model three *smoke-model*)
                                (call :save-session! three)
                                (unless (call :decide-closed-work! id (call :session-owner three) "delete")
                                  (error "delete was refused"))
                                (when (or (call :live-session id) (call :archived-directory id))
                                  (error "a deleted work left something behind")))
                              ;; 4: more time from the house, by its door
                              (let* ((four (open-closed "10.9.9.5" key))
                                     (id (call :session-id four))
                                     (url (format nil "~a?work=~a&days=10" (door "closed-extend") id))
                                     (before (call :closed-deadline id)))
                                (unless (eql (nth-value 1 (http :post url)) 404)
                                  (error "the house's door answered without a token file"))
                                (with-open-file (out token-file :direction :output :if-exists :supersede)
                                  (write-line "ci-house-token-0123456789" out))
                                (setf (symbol-value (lab '#:*house-token-file*)) token-file)
                                (unless (eql (nth-value 1 (http :post url :headers '(("X-Prompt-Lab-House" . "wrong-token-0123456789")))) 403)
                                  (error "the house's door took a wrong token"))
                                (unless (eql (nth-value 1 (http :post url :headers '(("X-Prompt-Lab-House" . "ci-house-token-0123456789")))) 200)
                                  (error "the house's door refused its token"))
                                (unless (eql (call :closed-deadline id) (+ before (* 10 86400)))
                                  (error "the house's grant did not move the date"))
                                (call :delete-closed-work! id :session four)))
                            t)))
              (ignore-errors (delete-file token-file))
              (dolist (pair saved) (setf (symbol-value (lab (car pair))) (cdr pair)))))
          ;; BRING YOUR OWN AGENT (external.lisp, :authors): an author key's
          ;; sessions alone, opened with the session door's caps, a day's calls
          ;; per key, a frozen key refused
          (let ((saved (mapcar #'(lambda (name) (cons name (symbol-value (lab name))))
                               '(#:*external-agent?* #:*external-calls-per-key*))))
            (setf (symbol-value (lab '#:*external-agent?*)) :authors)
            (unwind-protect
                 (smoke "prompt-lab bring your own agent: an author key's sessions only, a day's calls per key, frozen refused"
                        (lambda ()
                          (let ((key "ffffffffffffffffffffff01"))
                            (unless (eql 403 (nth-value 1 (http :post (door "agent")
                                                                :json (table "event" "prompt" "text" "A plate."))))
                              (error "an agent with no author key was let in"))
                            (let* ((brief (json-of :post (door "agent")
                                                   :json (table "event" "prompt" "text" "A plate." "wallet" key)))
                                   (id (gethash "session" brief))
                                   (owner (gethash "owner" brief))
                                   (headers (list (cons "X-Prompt-Lab-Owner" owner)))
                                   (mcp (format nil "~a/mcp?session=~a" prefix id)))
                              (unless (and (stringp id) (stringp owner)) (error "no session for an author key's agent: ~s" brief))
                              (unless (equal (uiop:symbol-call :prompt-lab :session-wallet
                                                               (uiop:symbol-call :prompt-lab :find-session id))
                                             key)
                                (error "the agent's session is not under its key"))
                              (unless (eql 200 (nth-value 1 (http :post mcp :headers headers
                                                                  :json (table "jsonrpc" "2.0" "id" 1 "method" "tools/list"))))
                                (error "the MCP door refused an author key's session"))
                              (setf (symbol-value (lab '#:*external-calls-per-key*)) 1)
                              (unless (eql 429 (nth-value 1 (http :post mcp :headers headers
                                                                  :json (table "jsonrpc" "2.0" "id" 2 "method" "tools/list"))))
                                (error "the day's calls per key did not bind"))
                              (setf (symbol-value (lab '#:*external-calls-per-key*)) nil)
                              (json-of :post (door "agent") :headers headers
                                       :json (table "event" "stopped" "session" id "text" "Only a test."))
                              (uiop:symbol-call :prompt-lab :note-author-answer key (table "standing" "frozen"))
                              (unless (eql 403 (nth-value 1 (http :post mcp :headers headers
                                                                  :json (table "jsonrpc" "2.0" "id" 3 "method" "tools/list"))))
                                (error "a frozen key's agent was let in"))
                              t))))
              (dolist (pair saved) (setf (symbol-value (lab (car pair))) (cdr pair)))
              ;; the session it opened came from this address and joined the
              ;; funnel's tally: given back, as the smokes after it count both
              (let ((record (gethash (uiop:symbol-call :prompt-lab :address-key "127.0.0.1")
                                     (symbol-value (lab '#:*address-counts*)))))
                (when (and record (plusp (or (getf record :sessions) 0)))
                  (decf (getf record :sessions))))
              (uiop:symbol-call :prompt-lab :funnel-count! "sessions" -1)
              (uiop:symbol-call :prompt-lab :funnel-count! "prompts" -1)
              (uiop:symbol-call :prompt-lab :funnel-count! "builds-stopped" -1)))
          ;; the private front's request (private.lisp): a monetizing author's
          ;; session sells, a trusted author's deploys free open work (ruling 50)
          (smoke "prompt-lab private front: a request says monetize true, or free, or nothing without a deployed root"
                 (lambda ()
                   (let* ((spool (merge-pathnames "ci-spool/" (uiop:temporary-directory)))
                          (saved (mapcar #'(lambda (name) (cons name (symbol-value (lab name))))
                                         '(#:*private-spool* #:*private-deployed-root*))))
                     (ensure-directories-exist (merge-pathnames "requests/" spool))
                     (unwind-protect
                          (flet ((written (id monetize)
                                   (let ((file (uiop:symbol-call :prompt-lab :write-private-request! id "eeeeeeeeeeeeeeeeeeeeee01"
                                                                 "0123456789abcdef0123456789abcdef" :monetize monetize)))
                                     (gethash "monetize" (with-open-file (in file) (yason:parse in))))))
                            (setf (symbol-value (lab '#:*private-spool*)) (namestring spool)
                                  (symbol-value (lab '#:*private-deployed-root*)) (namestring spool))
                            (unless (and (eq (written "pl-0000000001" t) t)
                                         (equal (written "pl-0000000002" :free) "free"))
                              (error "the request does not carry what the author may deploy"))
                            (setf (symbol-value (lab '#:*private-deployed-root*)) nil)
                            (when (written "pl-0000000003" :free)
                              (error "a front with no deployed root offered deploys"))
                            t)
                       (dolist (pair saved) (setf (symbol-value (lab (car pair))) (cdr pair)))
                       (ignore-errors (uiop:delete-directory-tree spool :validate t))))))
          ;; the reviewer pass over the archive's authors (author-review.lisp):
          ;; a dry run reads and calls nothing; each recommendation's rung
          (smoke "prompt-lab reviewer pass: a dry run reads the archive, and the ladder's moves"
                 (lambda ()
                   (progv (list (lab '#:*author-review-root*))
                       (list (namestring (merge-pathnames "author-review-smoke/" (uiop:temporary-directory))))
                     (and (listp (uiop:symbol-call :prompt-lab :review-authors! :dry-run? t))
                          (equal (uiop:symbol-call :prompt-lab :author-review-target "author" "promote" "pass") "trusted")
                          (equal (uiop:symbol-call :prompt-lab :author-review-target "trusted" "promote" "pass") "monetizing")
                          (equal (uiop:symbol-call :prompt-lab :author-review-target "trusted" "demote" "fail") "frozen")
                          (null (uiop:symbol-call :prompt-lab :author-review-target "monetizing" "promote" "pass"))))))
          ;; the funnel (funnel.lisp): the tally counts, the digest reads
          ;; it with the archive and says the day, kept on its shelf and
          ;; sent nowhere here
          (smoke "prompt-lab funnel: the tally counts and the day's digest reads"
                 (lambda ()
                   (progv (list (lab '#:*funnel-root*))
                       (list (namestring (merge-pathnames "funnel-smoke/" (uiop:temporary-directory))))
                     (uiop:symbol-call :prompt-lab :funnel-count! "sessions")
                     (uiop:symbol-call :prompt-lab :funnel-count! "prompts" 2)
                     (uiop:symbol-call :prompt-lab :funnel-write-tally!)
                     (let* ((day (uiop:symbol-call :prompt-lab :day-key))
                            (text (nth-value 1 (uiop:symbol-call :prompt-lab :funnel-report! day :send? nil))))
                       (and (stringp text)
                            (search day text)
                            (search "prompts 2" text)
                            (probe-file (uiop:symbol-call :prompt-lab :funnel-tally-file))
                            (probe-file (merge-pathnames (format nil "funnel-~a.json" day)
                                                         (uiop:symbol-call :prompt-lab :funnel-directory))))))))
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
                              ;; open source that charges nothing is hosted free, with
                              ;; no payee asked (rulings 44 and 50); the house may
                              ;; withdraw it, through a door that wants its token
                              (json-of :post (door "model") :headers headers
                                       :json (table "session" tolled "source" *smoke-model*))
                              (let ((free (json-of :post (door "deploy") :headers headers
                                                   :json (table "session" tolled "name" name))))
                                (unless (equal (gethash "url" free) address)
                                  (error "an open model that charges nothing was not deployed free"))
                                (unless (uiop:symbol-call :prompt-lab :truthy? (gethash "free" (uiop:symbol-call :prompt-lab :deployment-record name)))
                                  (error "the free deployment's record does not say free")))
                              (let ((withdraw (format nil "~a?name=~a" (door "house/withdraw") name))
                                    (token-file (merge-pathnames "ci-house-token-2" (uiop:temporary-directory)))
                                    (saved (symbol-value (lab '#:*house-token-file*))))
                                (unless (eql 404 (nth-value 1 (http :post withdraw)))
                                  (error "the house's withdrawal answered without a token file"))
                                (with-open-file (out token-file :direction :output :if-exists :supersede)
                                  (write-line "ci-house-token-abcdefghijk" out))
                                (setf (symbol-value (lab '#:*house-token-file*)) token-file)
                                (unwind-protect
                                     (progn
                                       (unless (eql 403 (nth-value 1 (http :post withdraw :headers '(("X-Prompt-Lab-House" . "not-the-house-token-xyz")))))
                                         (error "the house's withdrawal took a wrong token"))
                                       (unless (eql 200 (nth-value 1 (http :post withdraw :headers '(("X-Prompt-Lab-House" . "ci-house-token-abcdefghijk")))))
                                         (error "the house's withdrawal refused its token"))
                                       (unless (eql 404 (status address))
                                         (error "a withdrawn free deployment is still served")))
                                  (setf (symbol-value (lab '#:*house-token-file*)) saved)
                                  (ignore-errors (delete-file token-file))))
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
                              (let ((source (http :get (format nil "~a/source" address))))
                                (unless (search "define-object model" source)
                                  (error "an open deployment did not serve its source"))
                                ;; the Corresponding Source names every component and
                                ;; its version, and the model file as a download
                                (dolist (component '("The prompt lab" "demos-common" "Monocle" "Gendl" "source?download=model"))
                                  (unless (search component source)
                                    (error "the source page does not name ~a" component)))
                                (unless (or (search "commit " source) (search "build " source))
                                  (error "the source page names no version of any component")))
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
                     ;; a closed deployment always charges: one that charges nothing is refused
                     (let ((plain (json-of :post (door "session") :json (table "closed" t))))
                       (json-of :post (door "model") :headers (list (cons "X-Prompt-Lab-Owner" (gethash "owner" plain)))
                                :json (table "session" (gethash "session" plain) "source" *smoke-model*))
                       (unless (eql 400 (nth-value 1 (http :post (door "deploy")
                                                           :headers (list (cons "X-Prompt-Lab-Owner" (gethash "owner" plain)))
                                                           :json (table "session" (gethash "session" plain) "name" "ci-smoke-closed-free"
                                                                        "payee" "owner@example.com"))))
                         (error "a closed deployment that charges nothing was taken")))
                     (unwind-protect
                          (let ((deployment (json-of :post (door "deploy") :headers headers
                                                     :json (table "session" closed "name" name
                                                                  "payee" "owner@example.com"))))
                            (and (eq (gethash "closed" deployment) t)
                                 (eql (gethash "fee_percent" deployment)
                                      (uiop:symbol-call :prompt-lab :house-fee-percent t))
                                 (eql 404 (nth-value 1 (http :get (format nil "~a/d/~a/source" prefix name))))))
                       (http :post (door "undeploy") :headers headers :json (table "name" name))))))
          ;; the authors' payouts (ruling 79): the lab asks the gate once a
          ;; month, only where it takes tolls by card, and asks again after a
          ;; gate that did not answer
          (smoke "prompt-lab asks the gate for the authors' monthly payouts, once a month"
                 (lambda ()
                   (let* ((post-fn (lab '#:gate-post))
                          (saved (fdefinition post-fn))
                          (asked 0)
                          (answer 200)
                          (root (merge-pathnames "payouts-smoke/" (uiop:temporary-directory))))
                     (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
                     (progv (list (lab '#:*deployments?*) (lab '#:*books?*) (lab '#:*toll-provider*) (lab '#:*deployed-root*))
                         (list t t :test root)
                       (setf (fdefinition post-fn)
                             (lambda (name object &key seconds)
                               (declare (ignore object seconds))
                               (unless (equal name "connect-payouts") (error "asked the gate's ~a door" name))
                               (incf asked)
                               (values (table "month" "2026-11") answer)))
                       (unwind-protect
                            (progn
                              (when (uiop:symbol-call :prompt-lab :ask-gate-payouts!)
                                (error "a lab taking no card payments asked for payouts"))
                              (setf (symbol-value (lab '#:*toll-provider*)) :stripe)
                              (setq answer 503)
                              (uiop:symbol-call :prompt-lab :ask-gate-payouts!)
                              (setq answer 200)
                              (unless (uiop:symbol-call :prompt-lab :ask-gate-payouts!)
                                (error "a gate that did not answer was not asked again"))
                              (when (uiop:symbol-call :prompt-lab :ask-gate-payouts!)
                                (error "asked twice in a month"))
                              (uiop:symbol-call :prompt-lab :ask-gate-payouts! :now (+ (get-universal-time) (* 32 86400)))
                              (unless (= asked 3) (error "the gate was asked ~d times, not 3" asked))
                              t)
                         (setf (fdefinition post-fn) saved))))))
          ;; a closed deployment's monthly minimum (ruling 49): reckoned from
          ;; the books for the month just ended, the shortfall told to the
          ;; gate (caught here: there is no gate); an open one owes none; the
          ;; house sets a deployment's own, $0 included, behind its token.
          ;; monocle is loaded after this file is read: its names by symbol-call
          (smoke "prompt-lab closed deployment's monthly minimum: reckoned from the books, told to the gate, set by the house"
                 (lambda ()
                   (let* ((opened (json-of :post (door "session") :json (table "closed" t)))
                          (id (gethash "session" opened))
                          (headers (list (cons "X-Prompt-Lab-Owner" (gethash "owner" opened))))
                          (name "ci-smoke-minimum")
                          (asked nil)
                          (real-ask (fdefinition (lab '#:hosting-ask)))
                          (saved-minimum (symbol-value (lab '#:*closed-minimum-cents*)))
                          (saved-token (symbol-value (lab '#:*house-token-file*)))
                          (token-file (merge-pathnames "ci-house-token-3" (uiop:temporary-directory))))
                     (flet ((call (fn &rest args) (apply #'uiop:symbol-call :prompt-lab fn args)))
                       (json-of :post (door "model") :headers headers
                                :json (table "session" id
                                             "source" "(define-object model (box)
  :input-slots ((length 30) (width 20) (height 10))
  :computed-slots
  ((tolls (list (list :key :drawing :label \"SVG drawing\" :rivets 500)))
   (file-tolls (list :svg :drawing))))"))
                       (json-of :post (door "deploy") :headers headers
                                :json (table "session" id "name" name "payee" "owner@example.com"))
                       ;; the books as they were, put back after: this test books a real
                       ;; line, and the smokes after it count the books' lines
                       (let* ((house (call :lab-house))
                              (books (uiop:symbol-call :monocle :revenue-file house))
                              (books-before (and (probe-file books) (uiop:read-file-string books))))
                         (unwind-protect
                              (let ((record (call :deployment-record name))
                                    (now (get-universal-time)))
                                (unless record (error "the closed deployment was not made"))
                                ;; made long before last month, so last month is reckoned
                                (setf (gethash "deployed" record) (- now (* 70 86400)))
                                (uiop:symbol-call :monocle :save-record! house record)
                                (setf (symbol-value (lab '#:*closed-minimum-cents*)) 1000)
                                (unless (eql (call :deployment-minimum record) 1000)
                                  (error "a closed deployment does not owe the default minimum"))
                                (when (call :deployment-minimum (let ((open (make-hash-table :test #'equal)))
                                                                  (setf (gethash "closed" open) nil) open))
                                  (error "an open deployment owes a minimum"))
                                ;; a real payment, moved into last month
                                (multiple-value-bind (s m h d month year) (decode-universal-time now 0)
                                  (declare (ignore s m h d))
                                  (let* ((prev-month (if (= month 1) 12 (1- month)))
                                         (prev-year (if (= month 1) (1- year) year))
                                         (mid (encode-universal-time 0 0 12 15 prev-month prev-year 0)))
                                    ;; $4 of revenue, its fee at the closed rate, against $10 of fees
                                    (uiop:symbol-call :monocle :book-revenue! house name 400 :toll "ci" :reference "ci-minimum")
                                    (unless (= (call :deployment-fees-cents name (- now 60) (+ now 60))
                                               (* 400 (call :house-fee-percent t) 1/100))
                                      (error "the booked payment's fee does not read the closed rate's share of 400"))
                                    (let ((moved (with-output-to-string (out)
                                                   (with-input-from-string (in (uiop:read-file-string books))
                                                     (loop for l = (read-line in nil) while l
                                                           do (write-line (if (search "ci-minimum" l)
                                                                              (let ((j (yason:parse l)))
                                                                                (setf (gethash "time" j) mid)
                                                                                (with-output-to-string (s) (yason:encode j s)))
                                                                              l)
                                                                          out))))))
                                      (with-open-file (out books :direction :output :if-exists :supersede)
                                        (write-string moved out)))))
                                ;; the gate is stubbed: what would be told, caught; its balance
                                ;; says the app has been paused 91 days for 940 cents of dues, and
                                ;; its settle door names a page of this lab to go to
                                (setf (fdefinition (lab '#:hosting-ask))
                                      (lambda (door dname &rest plist)
                                        (push (list* door dname plist) asked)
                                        (let ((answer (make-hash-table :test #'equal)))
                                          (cond ((equal door "host-balance")
                                                 (setf (gethash "minimum_due" answer) 940
                                                       (gethash "stopped_since" answer) (- (get-universal-time) (* 91 86400))))
                                                ((equal door "host-settle")
                                                 (setf (gethash "url" answer) (format nil "http://127.0.0.1:~a~a/api/config" (http-port) prefix))))
                                          (values answer 200))))
                                (let* ((reported (call :reckon-closed-minimums! :force? t))
                                       (mine (assoc name reported :test #'equal)))
                                  ;; what the fees fell short of $10 by
                                  (unless (and mine (eql (cdr mine) (ceiling (- 1000 (* 400 (call :house-fee-percent t) 1/100)))))
                                    (error "the shortfall was not reckoned from the fees: ~s" reported))
                                  (unless (find-if (lambda (a) (and (equal (first a) "host-minimum") (equal (second a) name))) asked)
                                    (error "the gate was not told")))
                                (when (assoc name (call :reckon-closed-minimums!) :test #'equal)
                                  (error "the same month was reckoned twice"))
                                ;; the paused page's 'Pay what is due' goes to the gate's checkout:
                                ;; a 302 (this client follows no redirect after a POST), the gate asked
                                (multiple-value-bind (body status)
                                    (http :post (format nil "~a/host-settle?deployed=~a" prefix name))
                                  (declare (ignore body))
                                  (unless (and (member status '(302 303))
                                               (find-if (lambda (a) (and (equal (first a) "host-settle") (equal (second a) name))) asked))
                                    (error "the settle door did not send the payer to the gate's checkout (~a)" status)))
                                ;; the house's own minimum: $0 waives it
                                (let ((url (format nil "~a?name=~a&cents=0" (door "house/minimum") name)))
                                  (unless (eql 404 (nth-value 1 (http :post url)))
                                    (error "the house's minimum door answered without a token file"))
                                  (with-open-file (out token-file :direction :output :if-exists :supersede)
                                    (write-line "ci-house-token-minimum-123" out))
                                  (setf (symbol-value (lab '#:*house-token-file*)) token-file)
                                  (unless (eql 200 (nth-value 1 (http :post url :headers '(("X-Prompt-Lab-House" . "ci-house-token-minimum-123")))))
                                    (error "the house's minimum door refused its token"))
                                  (unless (eql 0 (call :deployment-minimum (call :deployment-record name)))
                                    (error "the house's $0 minimum was not kept")))
                                (when (assoc name (call :reckon-closed-minimums! :force? t) :test #'equal)
                                  (error "a deployment the house waived was reckoned"))
                                ;; paused 91 days for dues: taken down.  (The house's $0 above
                                ;; leaves it owing nothing new; the gate's word on old dues stands.)
                                (setf (symbol-value (lab '#:*closed-minimum-cents*)) 1000)
                                (call :set-deployment-minimum! name nil)
                                (unless (member name (call :sweep-unpaid-dues! :force? t) :test #'equal)
                                  (error "a deployment paused 91 days for its dues was not taken down"))
                                (when (call :deployment-record name)
                                  (error "the deployment taken down is still on the books' list"))
                                t)
                           (setf (fdefinition (lab '#:hosting-ask)) real-ask
                                 (symbol-value (lab '#:*closed-minimum-cents*)) saved-minimum
                                 (symbol-value (lab '#:*house-token-file*)) saved-token)
                           (ignore-errors (delete-file token-file))
                           (if books-before
                               (with-open-file (out books :direction :output :if-exists :supersede)
                                 (write-string books-before out))
                               (ignore-errors (delete-file books)))
                           (http :post (door "undeploy") :headers headers :json (table "name" name))))))))
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
                     ;; the slider is offered only where the gate pours the share
                     ;; (parameters.lisp *pot-percent-least*, nil by default): offer
                     ;; it for the length of this smoke, which checks the books' split
                     (setf (symbol-value (lab '#:*pot-percent-least*)) 0)
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
                                   (before (length (uiop:symbol-call :prompt-lab :revenue-lines)))
                                   ;; the open fee rate on this engine: a tenth on the free
                                   ;; one, three twentieths on solids
                                   (percent (uiop:symbol-call :prompt-lab :house-fee-percent nil)))
                              (when (gdl:the-object page (toll-paid? :pass)) (error "the toll was paid before it was paid"))
                              (gdl:the-object page (pay-toll! :pass))
                              (unless (gdl:the-object page (toll-paid? :pass)) (error "the toll was not paid after it was"))
                              ;; the card cost comes off the top, then the fee and
                              ;; the pot's 20 points, in rivets
                              (let* ((net 940)
                                     (fee (/ (* net percent) 100))
                                     (pot (/ (* net 20) 100))
                                     (line (uiop:symbol-call :prompt-lab :book-revenue! name 1000
                                                             :card-cents 60 :test? t)))
                                (unless (and (= (gethash "fee_rivets" line) fee)
                                             (= (gethash "pot_rivets" line) pot)
                                             (= (gethash "payee_rivets" line) (- net fee pot))
                                             (= (gethash "fee_before_card_rivets" line) (/ (* 1000 percent) 100)))
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
                                     ;; the author's take: the 200-rivet pass and the
                                     ;; 1000-rivet line, each less its fee and pot
                                     (= (gethash "yours_rivets" quarter)
                                        (+ (- 200 (/ (* 200 percent) 100) 40)
                                           (- 940 (/ (* 940 percent) 100) 188)))
                                     (equal (gethash "unit" earned) "rivets")
                                     (zerop (length (gethash "quarters" earned)))
                                     (null (uiop:symbol-call :prompt-lab :revenue-report))))))
                       (http :post (door "undeploy") :headers headers :json (table "name" name))
                       (setf (symbol-value (lab '#:*pot-percent-least*)) nil)))))
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
          ;; development is free (2026-11-12): nothing sold to a builder,
          ;; and a visitor with no author key keeps the address's daily
          ;; prompts even where the pot lifts the free caps
          (let ((most (symbol-value (lab '#:*max-prompts-per-address*))))
            (setf (symbol-value (lab '#:*max-prompts-per-address*)) 0
                  ;; a gate that sells: the lab still offers nothing
                  (getf (symbol-value (lab '#:*pot*)) :topup?) t)
            (unwind-protect
                 (smoke "prompt-lab sells builders nothing, and holds a keyless visitor to the address's daily prompts"
                        (lambda ()
                          (multiple-value-bind (body status)
                              (http :post (door "topup") :headers (owner-headers)
                                                         :json (table "session" session "amount_cents" 1000))
                            (unless (and (eql status 403) (search "sells no" body))
                              (error "the topup door answered ~a: ~a" status body)))
                          (when (eq (gethash "topup" (gethash "pot" (json-of :get (door "config")))) t)
                            (error "the config door still offers top-ups"))
                          (multiple-value-bind (body status)
                              (http :post (door "prompt") :headers (owner-headers)
                                                          :json (table "session" session "prompt" "A plate." "turnstile" "a-token"))
                            (unless (and (eql status 429) (search "without an author key" body))
                              (error "a keyless prompt past the address's prompts was answered ~a: ~a" status body)))
                          t))
              (setf (symbol-value (lab '#:*max-prompts-per-address*)) most)))
          ;; uploads from the page go to the upload door, one request a
          ;; file, on ONE human check: where a check stands the door wants
          ;; a token, a check the session passed lately covers the next
          ;; requests, and an old one covers nothing.  And the page's
          ;; script sends no file through a sheet action, whose body the
          ;; room caps under a file's size.  (The site key is pretended.)
          (when (gethash "uploads" (json-of :get (door "config")))
            (smoke "prompt-lab upload door takes several files on one human check"
                   (lambda ()
                     (let ((data (uiop:symbol-call :cl-base64 :string-to-base64-string (format nil "a,b~%1,2~%")))
                           (checked (symbol-value (lab '#:*topup-checked*)))
                           (script (let ((symbol (find-symbol (string '#:*sheet-ui-script*) :prompt-lab)))
                                     (and symbol (boundp symbol) (symbol-value symbol)))))
                       (flet ((upload (name)
                                (http :post (door "upload") :headers (owner-headers)
                                      :json (table "session" session "name" name "data" data "rights" t))))
                         (remhash session checked)
                         (multiple-value-bind (body status) (upload "one.csv")
                           (unless (and (eql status 400) (search "human check" body))
                             (error "with no check passed, the door answered ~a: ~a" status body)))
                         (setf (gethash session checked) (get-universal-time))
                         (dolist (name '("one.csv" "two.csv"))
                           (multiple-value-bind (body status) (upload name)
                             (unless (eql status 200)
                               (error "on a check passed lately, ~a was answered ~a: ~a" name status body))))
                         (setf (gethash session checked)
                               (- (get-universal-time) (symbol-value (lab '#:*topup-check-seconds*)) 5))
                         (unless (eql 400 (nth-value 1 (upload "late.csv")))
                           (error "a check long past still covered an upload"))
                         (remhash session checked)
                         (when script
                           (unless (search "plSendFiles" script)
                             (error "the sheet's script does not send files to the upload door"))
                           (when (or (search "plAction('upload',{name" script)
                                     (search "plAction('adopt',{files" script))
                             (error "the sheet's script still sends file bytes through a sheet action")))
                         t)))))
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
