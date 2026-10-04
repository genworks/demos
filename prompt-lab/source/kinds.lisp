;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; What a session builds: a geometry model (the object named MODEL, shown
;; in the viewer -- the lab as it always was, and the default), or a web
;; app (the object named APP, a GWL page served at an address of its own,
;; which may hold a MODEL and show it).  The visitor's switch beside the
;; prompt says which; the kind goes with each prompt and the session keeps
;; the last one.  It picks the agent's brief (agent.lisp) and its tools
;; (tools.lisp), and opens the session's package to GWL's symbols.
;;

(defparameter *kinds* '(:model :app)
  "List of keywords. What this lab offers to build, the default first:
:model, a geometry model, and :app, a GWL web app.  With one kind the
page shows no switch.")

(defparameter *kind-labels*
  '(:model "Geometry model"
    :app "Web app")
  "Plist, kind -> what the page's switch calls it.")

(defun default-kind () (or (first *kinds*) :model))

(defun kind-name (kind) (string-downcase (symbol-name kind)))

(defun kind-label (kind) (or (getf *kind-labels* kind) (kind-name kind)))

(defun parse-kind (name)
  "The kind this lab offers that NAME (a string or a keyword) names, or
nil.  Compared, never interned: the name comes from a request."
  (and (or (stringp name) (keywordp name))
       (find name *kinds* :test #'string-equal)))

(defun kinds-state ()
  "The kinds as the config door reports them."
  (map 'vector #'(lambda (kind) (h "kind" (kind-name kind) "label" (kind-label kind))) *kinds*))

;; Kept beside the session struct rather than in it, like its lock: a
;; reload that adds a slot strands the sessions already in the table.
;; Keyed by the package's name, which a session, a replay of it and a
;; thumbnail's scratch build each have their own.
(defvar *session-kinds* (make-hash-table :test #'equal)
  "Package name -> the kind of the session, replay or scratch build there.")

(defvar *kinds-lock* (bt:make-lock "prompt-lab kinds"))

(defun session-kind (session)
  (or (bt:with-lock-held (*kinds-lock*)
        (gethash (session-package-name session) *session-kinds*))
      (default-kind)))

(defun set-session-kind! (session kind)
  "Make SESSION one that builds KIND.  A web app's source names GWL's
symbols bare, so its package is opened to them first."
  (when (eq kind :app)
    (open-package-to-gwl! (session-package session)))
  (bt:with-lock-held (*kinds-lock*)
    (setf (gethash (session-package-name session) *session-kinds*) kind))
  kind)

(defun forget-session-kind! (session)
  (bt:with-lock-held (*kinds-lock*)
    (remhash (session-package-name session) *session-kinds*)))


;;
;; A session's package is made like gdl-user (session.lisp).  A web app
;; wants what a GWL package has besides: GWL itself, the HTML generators
;; and the web server's names.  The package made by gwl:define-package
;; is the pattern; the session's own package is brought up to it in
;; place, so the model already written there stays.
;;

(defun gwl-package-pattern ()
  (or (find-package :pl-app-pattern)
      (progn (eval `(gwl:define-package :pl-app-pattern))
             (find-package :pl-app-pattern))))

(defun open-package-to-gwl! (package)
  "Have PACKAGE use every package a GWL package uses.  A symbol of its own
that would clash with one coming in gives way: the source that named it is
compiled again with the new meaning, and GDL's messages are keywords."
  (let* ((pattern (gwl-package-pattern))
         (missing (set-difference (package-use-list pattern) (package-use-list package))))
    (when missing
      (dolist (used missing)
        (do-external-symbols (symbol used)
          (multiple-value-bind (own status) (find-symbol (symbol-name symbol) package)
            (when (and (member status '(:internal :external))
                       (not (eq own symbol))
                       (not (member own (package-shadowing-symbols package))))
              (unintern own package)))))
      ;; what the pattern shadows with a symbol of its own (define-package)
      (shadow (loop for symbol in (package-shadowing-symbols pattern)
                    when (eq (symbol-package symbol) pattern)
                      collect (symbol-name symbol))
              package)
      (use-package missing package))
    package))


;;
;; The web app: the object named APP in the session package.
;;

(defun app-symbol (session)
  (find-symbol (image-case "APP") (session-package session)))

(defun app-defined? (session)
  (let ((symbol (ignore-errors (app-symbol session))))
    (and symbol (find-class symbol nil) t)))

(defun built-symbol (session)
  "The symbol of what SESSION is building: APP for a web app, else MODEL."
  (if (eq (session-kind session) :app) (app-symbol session) (model-symbol session)))

(defun built-name (session)
  (if (eq (session-kind session) :app) "APP" "MODEL"))

(define-object web-app (demos-common:demo-ui-mixin session-control-mixin base-html-page)

  :documentation
  (:description "What a prompt-lab web app mixes in: a GWL page with the
demos' shared chrome (the stylesheet, the card and param-field helpers,
one standard viewport on the page's objects) that expires when its
visitor leaves.  The app supplies title, body, its form controls and
sections, and objects when it shows geometry."
   :author "Genworks International")

  :input-slots
  (("List of GDL objects the viewport (viewport-area) draws; nil for a
page without geometry."
    objects nil)
   ("List of plists, the app's tollbooths: what a visitor pays for.  Each
is (:key <keyword> :label <string> <unit-key> <integer>) with :uses <n>
(the payment covers n uses, spent with use-toll!) or :seconds <n> (it
covers that long), or neither (it covers this visit).  The price is in
the lab's own unit, under its key (:rivets, :mites: unit-key; an older
:cents is read the same), and may be computed from the model."
    tolls nil)
   ("Plist, download format keyword (:pdf :svg :png :step :iges :stl) to
the key of the toll its file-link asks for; a format not named is free."
    file-tolls nil)
   ("String or nil. The host the page is served on: the apps' own
(*app-origin*), so a page a visitor's prompts wrote never runs as the
lab; nil, any host, where none is set."
    host (app-origin-host))
   ("String or nil. The deployment this instance serves (deploy.lisp), set
by its door; nil for the session's own preview, where a toll is paid
without money and nothing is booked."
    deployment-name nil))

  :computed-slots
  (;; what this visitor has paid: toll key -> (:at <time> [:uses n] [:until time])
   (payments nil :settable)

   ;; the demos' stylesheet from under the lab's own prefix (published
   ;; there by publish-prompt-lab!): the address the mixin would name is
   ;; published for the demos' own virtual hosts
   (additional-header-content
    (let ((demos-common:*url-prefix* (format nil "~a/app-static" *url-prefix*)))
      (call-next-method))))

  :functions
  (;; Tollbooths.  The app says what is paid for (tolls) and where the
   ;; booths stand (toll-button, toll-paid?, use-toll!, file-tolls); the
   ;; lab takes the payment (pay-toll!, below) and keeps the books.
   (toll (key) (find key (the tolls) :key #'(lambda (toll) (getf toll :key))))

   ("Boolean. Whether this visitor holds a payment for toll KEY that still
stands: uses left, time left, or made on this visit."
    toll-paid?
    (key)
    (let ((payment (getf (the payments) key)))
      (and payment
           (let ((uses (getf payment :uses)) (until (getf payment :until)))
             (and (or (null uses) (plusp uses))
                  (or (null until) (< (get-universal-time) until)))))))

   ("Void. Spend one use of the payment for toll KEY, where it counts uses."
    use-toll!
    (key)
    (let ((payment (getf (the payments) key)))
      (when (and payment (getf payment :uses))
        (let ((payments (copy-list (the payments))))
          (setf (getf payments key)
                (list* :uses (1- (getf payment :uses)) (alexandria:remove-from-plist payment :uses)))
          (the (set-slot! :payments payments))))))

   ;; called by the toll's button through gdlAjax: the price is the
   ;; toll's own, never the browser's
   (pay-toll!
    (key)
    (let ((toll (the (toll key))))
      (when (and toll (not (the (toll-paid? key))))
        (when (charge-toll! self toll)
          (let ((payments (copy-list (the payments))))
            (setf (getf payments key)
                  (append (list :at (get-universal-time))
                          (when (getf toll :uses) (list :uses (getf toll :uses)))
                          (when (getf toll :seconds)
                            (list :until (+ (get-universal-time) (getf toll :seconds))))))
            (the (set-slot! :payments payments)))))))

   ("String of html. The booth for toll KEY: a button that pays it, or a
line saying it is paid.  Put it in a section, so it redraws when paid."
    toll-button
    (key)
    (let ((toll (the (toll key))))
      (with-lhtml-string ()
        (cond ((null toll) (htm (:span "(no such toll)")))
              ((the (toll-paid? key))
               (htm (:span :class "text-sm font-medium text-gray-700"
                           (fmt "Paid: ~a" (getf toll :label))
                           (let ((uses (getf (getf (the payments) key) :uses)))
                             (when uses (fmt " (~d left)" uses))))))
              (t (htm (:button :type "button"
                               :class "inline-flex items-center rounded-md bg-indigo-600 px-3 py-1.5 text-sm font-semibold text-white shadow-sm hover:bg-indigo-700"
                               :onclick (the (gdl-ajax-call :function-key :pay-toll! :arguments (list key)))
                               (fmt "~:d ~a -- ~a~a" (monocle:toll-price toll) (units (monocle:toll-price toll))
                                    (getf toll :label)
                                    (toll-mode-note self)))))))))

   ("String of html. A link that downloads the first of objects as FORMAT
(:pdf :svg :png, and :step :iges :stl on a solids engine) as it stands
now, inputs and all.  A format named in file-tolls wants its toll paid."
    file-link
    (format &key label)
    (with-lhtml-string ()
      (:a :class "text-sm font-medium text-indigo-600 hover:text-indigo-500"
          :href (format nil "~a/app-file?iid=~a&format=~(~a~)" *url-prefix* (the instance-id) format)
          (str (or label (format nil "~:@(~a~) file" format))))))))

;;
;; Taking a toll.  *toll-provider* (parameters.lisp) says how: :test
;; grants the payment on the spot and books it marked as a test, for a
;; lab with no payment gateway behind it; nil takes none.  A session's
;; own preview of its app (no deployment-name) is always granted and
;; never booked.  The taking is Monocle's (monocle:charge-toll!, whose
;; take-toll is where a payment provider is written).
;;

(defun toll-mode-note (app)
  (cond ((null (the-object app deployment-name)) " (preview: no charge)")
        ((eq *toll-provider* :test) " (test payment: no charge)")
        (t "")))

(defun charge-toll! (app toll)
  "Take TOLL from the visitor of APP.  True when it is paid."
  (monocle:charge-toll! (lab-house) (the-object app deployment-name) toll
                        :reference (the-object app instance-id)))

(defun app-staged? (session)
  "Whether SESSION's web app takes the lab page's stage, where a model's
drawing would be: the session builds a web app and has defined APP."
  (and session (eq (session-kind session) :app) (app-defined? session) t))

;;
;; The apps' own origin (*app-origin*): a page a visitor's prompts wrote is
;; served there and nowhere else, so its script never runs as the lab.
;;

(defun app-origin-host ()
  "The host of *app-origin*, in lower case, without scheme or port; nil
when apps are served on the lab's own address."
  (when (and (stringp *app-origin*) (plusp (length *app-origin*)))
    (let* ((start (let ((at (search "://" *app-origin*))) (if at (+ at 3) 0)))
           (end (or (position-if #'(lambda (c) (member c '(#\: #\/))) *app-origin* :start start)
                    (length *app-origin*))))
      (string-downcase (subseq *app-origin* start end)))))

(defun app-origin-base ()
  "*app-origin* without a trailing slash, or the empty string."
  (if (app-origin-host) (string-right-trim "/" *app-origin*) ""))

(defun request-host (req)
  "The host the visitor's browser asked for, as the proxies forwarded it,
in lower case and without its port."
  (flet ((header (value) (and (stringp value) (plusp (length value)) value)))
    (let ((host (or (header (net.aserve:header-slot-value req :x-cyclops-forwarded-host))
                    (header (net.aserve:header-slot-value req :host))
                    "")))
      (string-downcase (subseq host 0 (or (position #\: host) (length host)))))))

(defun on-app-origin? (req)
  "True when REQ may be answered with a web app: it came to the apps' own
host, or none is set."
  (let ((host (app-origin-host)))
    (or (null host) (string= (request-host req) host))))

(defun to-app-origin (req ent)
  "Answer REQ with a redirect to the same path and query on *app-origin*."
  (let ((query (net.aserve:request-query req)))
    (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
      (setf (net.aserve:reply-header-slot-value req :location)
            (format nil "~a~a~@[?~a~]" (app-origin-base)
                    (net.uri:uri-path (net.aserve:request-uri req))
                    (and query (net.aserve:query-to-form-urlencoded query))))
      (net.aserve:with-http-body (req ent)))))

(defun app-doors-host (host)
  "The host the app doors are published for, given the lab's HOST: the
lab's and the apps' own, when both are named; else HOST."
  (let ((app (app-origin-host)))
    (if (and host app)
        (remove-duplicates (cons app (if (listp host) host (list host))) :test #'string-equal)
        host)))

(defun app-url (session &key owner-key)
  "Where SESSION's web app opens, on the apps' own origin when there is
one.  A private session's needs its owner's key on the address, as the
viewer's does."
  (format nil "~a~a/app?~:[session~;replay~]=~a~@[&owner=~a~]"
          (app-origin-base) *url-prefix* (session-replay? session) (session-id session) owner-key))


;;
;; A web app's package sees every exported name of Gendl, GWL and the web
;; server, so a definition in its source can land on one of them and
;; redefine it for the whole image.  Refused before the compiler sees it.
;;

(defparameter *defining-forms*
  '("defun" "defmacro" "defgeneric" "defmethod" "defvar" "defparameter" "defconstant"
    "defstruct" "defclass" "deftype" "define-object" "define-object-amendment"
    "define-format" "define-lens" "define-condition" "define-compiler-macro" "defsetf")
  "List of strings. The operators whose second element names what is defined.")

(defun foreign-definitions (session source)
  "The names SOURCE would define that are not the session package's own: a
list of strings, nil when there are none or SOURCE cannot be read (the
compiler says why)."
  (let ((*package* (session-package session))
        (*read-eval* nil)
        (found nil))
    (ignore-errors
     (with-input-from-string (in source)
       (loop for form = (read in nil in)
             until (eq form in)
             do (when (and (consp form) (symbolp (first form)) (consp (rest form))
                           (member (symbol-name (first form)) *defining-forms* :test #'string-equal))
                  (let* ((name (second form))
                         (symbol (cond ((symbolp name) name)
                                       ;; (setf name), and defstruct's (name options...)
                                       ((and (consp name) (eq (first name) 'setf) (symbolp (second name)))
                                        (second name))
                                       ((and (consp name) (symbolp (first name))) (first name)))))
                    (when (and symbol (not (keywordp symbol))
                               (not (eq (symbol-package symbol) *package*)))
                      (pushnew (format nil "~(~a~)" symbol) found :test #'string=)))))))
    (nreverse found)))


;;
;; check_app: what the harness sees of the page, so the agent's opinion of
;; its work is not the only check.
;;

(defun html-text (html &optional (limit 1200))
  "HTML without its tags, scripts and styles, its runs of space folded: what
the page says, cut at LIMIT characters."
  (let ((text (with-output-to-string (out)
                (let ((depth 0) (skip nil) (space? nil))
                  (loop for i from 0 below (length html)
                        for char = (char html i)
                        do (cond ((char= char #\<)
                                  (incf depth)
                                  (flet ((tag? (name) (let ((end (+ i 1 (length name))))
                                                        (and (<= end (length html))
                                                             (string-equal name html :start2 (1+ i) :end2 end)))))
                                    (cond ((or (tag? "script") (tag? "style")) (setq skip t))
                                          ((or (tag? "/script") (tag? "/style")) (setq skip nil))))
                                  (setq space? t))
                                 ((char= char #\>) (setq depth (max 0 (1- depth))))
                                 ((or (plusp depth) skip))
                                 ((member char '(#\space #\tab #\newline #\return)) (setq space? t))
                                 (t (when space? (write-char #\space out) (setq space? nil))
                                    (write-char char out))))))))
    (string-trim " " (if (> (length text) limit)
                         (format nil "~a ..." (subseq text 0 limit))
                         text))))

(defun check-app (session)
  ;; building the app runs the whole file (meter.lisp)
  (multiple-value-bind (ok? reason) (meter! session :run (model-volume session))
    (unless ok? (return-from check-app (meter-refusal reason))))
  (handler-case
      (with-time-limit (*eval-seconds* "build and check")
        (let ((symbol (app-symbol session)))
          (unless (and symbol (find-class symbol nil))
            (error "No APP is defined yet -- write one with write_model."))
          (let* ((app (make-object symbol))
                 (mixed? (typep app 'web-app))
                 (body (the-object app body))
                 (head (when mixed? (the-object app additional-header-content)))
                 (children (the-object app children))
                 (controls (remove-if-not #'(lambda (child) (typep child 'base-form-control)) children))
                 ;; the mixin's own View menu and viewport are not the app's to embed
                 (own-controls (remove :view-control controls
                                       :key #'(lambda (control) (first (the-object control root-path)))))
                 (sections (remove :viewport-area
                                   (remove-if-not #'(lambda (child) (typep child 'sheet-section)) children)
                                   :key #'(lambda (section) (first (the-object section root-path)))))
                 (name #'(lambda (object) (format nil "~(~a~)" (first (the-object object root-path)))))
                 (unplaced-controls (remove-if #'(lambda (control)
                                                   (search (format nil "~a" (the-object control id)) body))
                                               own-controls))
                 (unplaced-sections (remove-if #'(lambda (section) (search (the-object section dom-id) body))
                                               sections))
                 (quiet-controls (remove-if #'(lambda (control)
                                                (ignore-errors (the-object control ajax-submit-on-change?)))
                                            own-controls))
                 (objects (when mixed? (the-object app objects)))
                 (viewport? (and mixed? (search (the-object app viewport-area dom-id) body) t))
                 (leaves (loop for object in objects append (the-object object leaves)))
                 (broken (remove-if #'leaf-box leaves))
                 ;; the tollbooths: each a key, a label and a price, and
                 ;; every toll a file is put behind one of them
                 (tolls (when mixed? (the-object app tolls)))
                 (bad-tolls (remove-if-not #'monocle:toll-fault tolls))
                 (lost-tolls (when mixed?
                               (loop for (nil key) on (the-object app file-tolls) by #'cddr
                                     unless (find key tolls :key #'(lambda (toll) (and (listp toll) (getf toll :key))))
                                       collect key)))
                 (problem? (or (not mixed?) unplaced-controls unplaced-sections quiet-controls broken
                               bad-tolls lost-tolls
                               (and objects (not viewport?)) (and viewport? (null objects)))))
            (declare (ignore head))
            (values
             (list (text-result "~{~a~%~}"
                                (remove nil
                                        (list "APP builds and its page renders."
                                              (unless mixed?
                                                "ERROR: APP does not mix in prompt-lab:web-app; the lab serves only a page that does.")
                                              (format nil "Title: ~a." (ignore-errors (the-object app title)))
                                              (format nil "Form controls: ~:[none~;~:*~{~a~^, ~}~]."
                                                      (mapcar #'(lambda (control)
                                                                  (format nil "~a = ~s" (funcall name control)
                                                                          (ignore-errors (the-object control value))))
                                                              own-controls))
                                              (format nil "Sections (redrawn when what they read changes): ~:[none~;~:*~{~a~^, ~}~]."
                                                      (mapcar name sections))
                                              (when unplaced-controls
                                                (format nil "ERROR: not on the page (the body does not embed them): ~{~a~^, ~}."
                                                        (mapcar name unplaced-controls)))
                                              (when unplaced-sections
                                                (format nil "ERROR: sections the body does not embed with (str (the <section> div)): ~{~a~^, ~}."
                                                        (mapcar name unplaced-sections)))
                                              (when quiet-controls
                                                (format nil "ERROR: controls without :ajax-submit-on-change? t (a change would do nothing): ~{~a~^, ~}."
                                                        (mapcar name quiet-controls)))
                                              (cond ((and objects viewport?)
                                                     (format nil "Viewport: ~a object~:p, ~a leaves." (length objects) (length leaves)))
                                                    (objects "ERROR: objects is given but the body does not embed (the viewport-area div).")
                                                    (viewport? "ERROR: the body embeds the viewport but objects is empty.")
                                                    (t "No viewport (a page without geometry)."))
                                              (when (and tolls (not bad-tolls))
                                                (format nil "Tollbooths: ~{~a~^; ~}."
                                                        (mapcar #'(lambda (toll)
                                                                    (format nil "~(~a~) \"~a\" ~:d ~a~@[, ~a uses~]~@[, ~a s~]"
                                                                            (getf toll :key) (getf toll :label)
                                                                            (monocle:toll-price toll)
                                                                            (units (monocle:toll-price toll))
                                                                            (getf toll :uses) (getf toll :seconds)))
                                                                tolls)))
                                              (when bad-tolls
                                                (format nil "ERROR: every toll is (:key <keyword> :label <string> ~(~s~) <integer>), with :uses or :seconds if wanted: ~a."
                                                        (unit-key) (monocle:toll-fault (first bad-tolls))))
                                              (when lost-tolls
                                                (format nil "ERROR: file-tolls names tolls that tolls does not declare: ~{~(~a~)~^, ~}." lost-tolls))
                                              (when broken
                                                (format nil "ERROR: ~a of ~a leaves cannot compute their geometry (first: ~a)."
                                                        (length broken) (length leaves) (the-object (first broken) root-path)))
                                              (format nil "What the page says: ~a" (html-text body))))))
             (and problem? t)))))
    (error (condition)
      (values (list (text-result "APP does not build or render with default inputs: ~a" condition)) t))))


;;
;; The agent's brief for a web app (agent.lisp, system-text): how to work,
;; the rules, and a recipe proven by hand.
;;

(defparameter *app-recipe*
  "(define-object model (base-object)
  :input-slots ((plate-length 120) (plate-width 80) (plate-height 25))
  :computed-slots ((volume (* (the plate-length) (the plate-width) (the plate-height))))
  :objects
  ((plate :type 'box
          :length (the plate-length) :width (the plate-width) :height (the plate-height)
          :display-controls (list :color :steel-blue))))

(define-object app (prompt-lab:web-app)
  :input-slots
  ((title \"Plate sizer\")
   (objects (list (the model))))          ; what the viewport draws
  :computed-slots
  ((body
    (with-lhtml-string ()
      (:div :class \"max-w-5xl mx-auto p-8\"
        (:h1 :class \"text-3xl font-bold text-gray-900 mb-6\" \"Plate sizer\")
        (str (the (card :title \"Parameters\"
                        :content (with-lhtml-string ()
                                   (:div :class \"p-6\" (str (the control-form)))))))
        (str (the (card :title \"3D view\"
                        :content (with-lhtml-string ()
                                   (:div :class \"p-6 relative\" :style \"height: 500px;\"
                                     (str (the viewport-area div)))))))
        (str (the numbers-section div)))))
   (control-form
    (with-form-string (:class \"grid grid-cols-2 md:grid-cols-4 gap-6\")
      (str (the (param-field \"Length (mm)\" (the length-control))))
      (str (the (param-field \"Width (mm)\" (the width-control))))
      (str (the (param-field \"Finish\" (the finish-control))))))
   (numbers-card
    (the (card :title \"Numbers\"
               :content (with-lhtml-string ()
                          (:div :class \"p-6\"
                            (:p (fmt \"Volume: ~:d mm3, finish ~(~a~)\"
                                     (round (the model volume)) (the finish-control value)))))))))
  :objects
  ((length-control :type 'number-form-control
                   :default 120 :ajax-submit-on-change? t
                   :domain :number :min 20 :max 400 :step 10)
   (width-control :type 'number-form-control
                  :default 80 :ajax-submit-on-change? t
                  :domain :number :min 20 :max 400 :step 10)
   (finish-control :type 'menu-form-control
                   :default :plain :size 1 :ajax-submit-on-change? t
                   :choice-plist '(:plain \"Plain\" :painted \"Painted\"))
   (model :type 'model
          :plate-length (the length-control value)
          :plate-width (the width-control value))
   (numbers-section :type 'base-html-div :inner-html (the numbers-card))))"
  "String. A whole web app as the agent is to write one: a model, a page
with three controls, the viewport and a section.  Proven by hand before
it went into the brief; prove a change to it the same way.")

(defun app-brief ()
  "The opening of the system prompt for a session that builds a web app."
  (format nil "You are the agent of the ~a prompt lab, and this session builds a WEB APP.  A visitor describes a small web application in plain words; you build it as a working page in GWL, Gendl's web layer, in their session.  It is served live at an address of its own, which the lab's page links as 'Open the app', and the visitor sees the same source file in an editor.

How to work:
1. Decide the page: what the visitor enters (form controls), what is computed from it, what is shown, and whether it shows geometry.
2. write_model: the file holds one define-object named APP that mixes in prompt-lab:web-app, written after the recipe below, with whatever helper objects and functions it needs.  When the app shows geometry, that is a define-object named MODEL in the same file (millimetres, a colour on every part, as in the primer), built by (make-object 'model) with no arguments, and APP holds it as a child fed from its form controls and names it in objects.  An app without geometry (a calculator, a table, a report) has no MODEL and no viewport.
3. check_app: it builds APP, renders the page and reports its controls, its sections and what the page says~:[~;; with a MODEL, check_model too~].  Fix what is wrong.  Few, deliberate calls.
4. Finish with a short reply to the visitor: what the app does and how to use it, and that 'Open the app' opens it.  No code in the reply; the code is in their editor.

The recipe, a whole app (it has been run):

~a

Rules for the page:
- A change must show at once: every form control carries :ajax-submit-on-change? t, and there is no submit button.  Read a control with (the <control> value).
- What changes with the inputs lives in a section: a child of type 'base-html-div whose :inner-html is the html, put on the page with (str (the <section> div)).  Html written straight into body is rendered once and never again.
- The control types: number-form-control (:domain :number, :min :max :step), text-form-control, menu-form-control (:choice-plist or :choice-list, :size 1 for a drop-down), checkbox-form-control, radio-form-control.  describe_object names their inputs.
- The mixin gives (the (card :title .. :subtitle .. :content <html string>)), (the (param-field <label> <control>)) and, for geometry, (the viewport-area div) on the list in objects, with its own Shaded / Wireframe / Hidden line switch and View menu.  Do not call page-intro.
- Html is cl-who: (str <string>) puts a string in, (fmt ..) a formatted one, (esc <string>) one that came from the visitor, and htm goes back to html inside a Lisp form.  A table of results is :table with :thead and :tbody.
- Style with the classes the recipe uses (they are in the page's stylesheet; other utility classes may not be) and a :style attribute for anything else.
- The page loads nothing from another site and carries no script of yours unless the request cannot be met without one.
- Charging, when the visitor asks for it, is done with tollbooths, placed wherever they say.  Declare them in APP's input-slots: (tolls (list (list :key :cad :label \"STEP download\" ~(~s~) 300 :uses 1))) -- with :uses n the payment covers n uses, with :seconds n it covers that long, with neither it covers this visit.  Prices are in ~a, the lab's own unit, which visitors buy in packets, never in money; at today's rate a ~a is a cent, so a visitor who asks for $3 means 300 ~a.  A price may be computed, from the model's size say.  (the (toll-button :cad)) is the booth, a pay button that turns into a paid line: put it in a section.  (the (toll-paid? :cad)) says whether to show what the toll guards, and (the (use-toll! :cad)) spends one use.  (the (file-link :step :label \"STEP file\")) is a link that downloads the first of objects as it stands, inputs and all (:pdf :svg :png anywhere; :step :iges :stl on a solids engine); name the format in (file-tolls (list :step :cad)) and the link wants that toll paid and spends a use of it.  The tollbooths are the app's payment points: the lab takes the payment at them.  Write no payment code of your own.  An app with at least one priced toll can be deployed with the Monetize button; say in your reply where the booths are, what they charge, and that Monetize now opens.
- Name your own objects and functions with names of your own: a definition named like something Lisp, Gendl or GWL already has (start, publish, header, title ...) is refused.
- Never include an in-package form."
          *brand*
          (and (member :model *kinds*) t)
          *app-recipe*
          ;; the lab's unit, in the charging rule
          (unit-key) (units) (units 1) (units)))
