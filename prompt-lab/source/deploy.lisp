;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; Monetize: a session's owner deploys what the session built -- its
;; model, or its web app (kinds.lisp) -- at an address of its own,
;; <prefix>/d/<name>, for others to use.  A deployment is a COPY: the
;; model file as it was when deployed and a record of the terms, kept
;; where the reaper never looks, compiled on demand into a package of
;; its own as a replay is (browse.lisp).  The session goes on changing,
;; or goes away; the deployment stays until its owner deploys again or
;; takes it down.
;;
;; The source is open (served beside the deployment) or closed, as the
;; session chose when it opened.  Where money is taken is the author's
;; to decide: a web app's tollbooths (kinds.lisp), a deployed model's
;; price for a download.  Of what users pay the house keeps its hosting
;; and licence fee (*house-fee-percents*, by the source terms) and the
;; author is owed the rest.  The lab keeps the terms and the books
;; (book-revenue!, revenue-report, payables).  No money moves yet: a
;; toll is a test payment (*toll-provider*), and a priced download opens
;; to its owner alone until a gateway stands behind it
;; (*deployment-payments?*).
;;
;; Every deployment and every line of the books names the engine and the
;; Lisp it ran on, so that what hosted applications earn can be reported
;; by runtime.
;;
;; The records, the tolls' declaration, the provider's seam and the books
;; are Monocle's (the monocle system); the lab is one house of it
;; (lab-house, parameters.lisp).  What is here is the lab's own: what it
;; deploys (a session's model file), how that runs (compiled into this
;; image), closed sessions, and the doors.
;;


;;
;; The record.
;;

(defun deployment-directory (name) (monocle:deployment-directory (lab-house) name))

(defun deployment-record (name)
  "The record of deployment NAME on this engine, a hash table, or nil."
  (monocle:deployment-record (lab-house) name))

(defun deployment-records ()
  "Every deployment of this engine, the newest first."
  (monocle:deployment-records (lab-house)))

(defun session-deployment (session)
  "The deployment made from SESSION, its record, or nil."
  (find (session-id session) (deployment-records)
        :key #'(lambda (record) (gethash "session" record)) :test #'equal))

(defun deployment-url (name) (format nil "~a/d/~a" *url-prefix* name))

(defun deployment-priced? (record) (monocle:record-priced? record))

(defun deployment-state (record &key owner?)
  "RECORD as the doors answer it: the terms everyone may read, and for its
owner what is owed."
  (let ((name (gethash "name" record)))
    (h "name" name
       "url" (deployment-url name)
       "title" (gethash "title" record)
       "blurb" (gethash "blurb" record)
       "kind" (gethash "kind" record)
       "engine" (gethash "engine" record)
       "closed" (if (truthy? (gethash "closed" record)) t 'yason:false)
       "source_url" (unless (truthy? (gethash "closed" record))
                      (format nil "~a/source" (deployment-url name)))
       ;; the most any of its files costs, in the lab's unit, named for it
       (monocle:amount-field (lab-house) "price") (or (monocle:record-price record) 0)
       "fee_percent" (gethash "fee_percent" record)
       ;; what its author adds for the community pot, on top of the fee
       "pot_percent" (or (gethash "pot_percent" record) 0)
       ;; a priced deployment opens to others once the gate takes payment
       "open" (if (or (not (deployment-priced? record)) *deployment-payments?*) t 'yason:false)
       "deployed" (gethash "deployed" record)
       "payee" (and owner? (gethash "payee" record)))))

(defvar *deploy-lock* (bt:make-lock "prompt-lab deployments"))

;;
;; Closed source is a session's choice, made as it opens and before its
;; first prompt: the session is then closed to watchers and kept out of
;; the listings, as a private one is, and what it deploys does not serve
;; its source.  The choice stands while the session completes the
;; Monetize flow -- a deployment.  A closed session that ends without one
;; REVERTS: it is opened again and goes into the public archive under the
;; GNU Affero General Public License, like any other.
;;

(defvar *closed-sessions* (make-hash-table :test #'equal)
  "Session id -> true, for the sessions opened closed-source.")

(defun session-closed? (session)
  (and (bt:with-lock-held (*kinds-lock*) (gethash (session-id session) *closed-sessions*))
       (session-private? session)))

(defun close-session-source! (session)
  "Mark SESSION, just opened, as closed-source: out of sight until it is
deployed or reverts."
  (bt:with-lock-held (*kinds-lock*) (setf (gethash (session-id session) *closed-sessions*) t))
  (setf (session-private? session) t)
  session)

(defun closed-deployment? (id)
  "Whether session ID has completed the Monetize flow: a closed deployment
made from it stands."
  (find-if #'(lambda (record) (and (equal (gethash "session" record) id)
                                   (truthy? (gethash "closed" record))))
           (deployment-records)))

(defun revert-closed-session! (session)
  "SESSION is ending.  If it was opened closed-source and never deployed,
it is opened again, so the record that goes to the archive is public."
  (when (session-closed? session)
    (unless (ignore-errors (closed-deployment? (session-id session)))
      (setf (session-private? session) nil)
      (ignore-errors
       (log-event session :note "This session was opened closed-source and ended without a deployment: its source is public again, under the GNU Affero General Public License."))
      ;; the record on disk is what the archive copies
      (save-session! session)))
  (bt:with-lock-held (*kinds-lock*) (remhash (session-id session) *closed-sessions*)))

(defun revert-closed-record! (directory)
  "The same for a session directory no live session owns (a restarted
Lisp left it): its archived record is opened when it was closed and
never deployed.  Never signals."
  (ignore-errors
   (let* ((json (read-record (merge-pathnames "session.json" directory)))
          (id (and json (gethash "id" json)))
          (created (and json (gethash "created" json))))
     (when (and (stringp id) (integerp created) (truthy? (gethash "closed" json))
                (not (closed-deployment? id)))
       (let* ((target (archive-directory-for id created))
              (file (and target (merge-pathnames "session.json" target)))
              (record (and file (read-record file))))
         (when record
           (setf (gethash "private" record) 'yason:false
                 (gethash "closed" record) 'yason:false)
           (write-text-file (with-output-to-string (out) (yason:encode record out)) file)))))))

;;
;; The monetization story.  Nothing is deployed until what was built says
;; what it charges for: its TOLLS, which the visitor asks the agent for
;; in plain words and the agent writes into the source -- on a web app,
;; with the booths placed on its page (kinds.lisp); on a model, with
;; FILE-TOLLS saying which of its downloads cost what.  The tolls are
;; where a payment gateway plugs in (charge-toll!).
;;

(defvar *built-tolls* (make-hash-table :test #'equal)
  "Package name -> (stamp tolls file-tolls): what the object built there
declares, as of the compile the stamp dates.")

(defun built-tolls (session)
  "The tolls of what SESSION built -- its APP's, or its MODEL's -- and, a
second value, its file-tolls (a plist, download format to toll key).  Nil
when it declares none or does not build.  Read once per compile."
  (let* ((key (session-package-name session))
         (stamp (ignore-errors (file-write-date (make-pathname :type "fasl"
                                                               :defaults (session-model-file session)))))
         (kept (bt:with-lock-held (*kinds-lock*) (gethash key *built-tolls*))))
    (unless (and kept stamp (eql (first kept) stamp))
      (let* ((object (ignore-errors
                      (with-time-limit (*eval-seconds* "build")
                        (let ((symbol (built-symbol session)))
                          (and symbol (find-class symbol nil) (make-object symbol))))))
             (tolls (and object (ignore-errors (remove-if-not #'valid-toll? (the-object object tolls)))))
             (file-tolls (and object tolls (ignore-errors (the-object object file-tolls)))))
        (setq kept (list stamp tolls (and (listp file-tolls) file-tolls)))
        (bt:with-lock-held (*kinds-lock*) (setf (gethash key *built-tolls*) kept))))
    (values (second kept) (third kept))))

(defun forget-built-tolls! (session)
  "The model file is being compiled again: what was read of the old one
goes.  (The stamp alone would not do: a file's date is in whole seconds,
and two compiles can share one.)"
  (bt:with-lock-held (*kinds-lock*) (remhash (session-package-name session) *built-tolls*)))

(defun monetizable? (session)
  "Whether what SESSION built has a monetization story: at least one toll
with a price, and for a model a download that toll stands on."
  (multiple-value-bind (tolls file-tolls) (ignore-errors (built-tolls session))
    (and (some #'(lambda (toll) (plusp (monocle:toll-price toll))) tolls)
         (or (eq (session-kind session) :app) (file-prices tolls file-tolls))
         t)))

(defun deployment-file-price (record format)
  "What deployed model RECORD asks for a download in FORMAT, in the lab's unit;
nil when that one is free."
  (monocle:record-file-price record format))

(defun deploy-session! (session &key name title blurb payee pot-percent)
  "Deploy what SESSION built as NAME.  Values: the record, or nil and the
reason.  It must have a monetization story (monetizable?).  Its source is
closed when the session was opened closed-source, else open.  POT-PERCENT
is the author's slider: points of every payment, on top of the fee, for
the lab's community pot.  With the :stripe provider PAYEE must have a
Stripe account of its own, onboarded through the gate (author-account):
its id is kept in the record as connect_account.  Deploying again from
the same session under the same name replaces the copy and the terms."
  (bt:with-lock-held (*deploy-lock*)
    (let* ((closed? (session-closed? session))
           (name (and (stringp name) (string-downcase (string-trim " " name))))
           (existing (and (deployment-name? name) (deployment-record name)))
           (mine (session-deployment session))
           (app? (eq (session-kind session) :app))
           ;; a deployed MODEL's prices, by download; a web app keeps
           ;; tollbooths of its own on its page (kinds.lisp)
           (prices (unless app?
                     (multiple-value-bind (tolls file-tolls) (built-tolls session)
                       (file-prices tolls file-tolls))))
           (payee (clip-line payee 200))
           (symbol (ignore-errors (built-symbol session)))
           (account nil) (account-problem nil))
      ;; the author's account, where the gate takes the money
      (when (and (eq *toll-provider* :stripe) (find #\@ payee))
        (multiple-value-bind (id url ready? why) (author-account payee)
          (declare (ignore url))
          (setq account (and ready? id)
                account-problem why)))
      (cond
        ((not *deployments?*) (values nil "This lab deploys nothing."))
        ((not (and symbol (find-class symbol nil)))
         (values nil (format nil "There is nothing to deploy yet: build a ~:[model~;web app~] first." app?)))
        ((not (monetizable? session))
         (values nil (if app?
                         (unit-text "This app charges for nothing yet.  Tell the agent what should cost {units} -- 'charge 300 {units} for each STEP download', 'a day pass for 500 {units} unlocks the results' -- and Monetize opens when it has written the tollbooths.")
                         (unit-text "This model charges for nothing yet.  Tell the agent what should cost {units} -- 'charge 300 {units} for each STEP download' -- and Monetize opens when it has written that in."))))
        ((not (deployment-name? name))
         (values nil "Give it a name for its address: 3 to 40 lower-case letters, digits and hyphens, a letter first."))
        ((and existing (not (equal (gethash "owner" existing) (session-owner session))))
         (values nil (format nil "The name ~a is taken; choose another." name)))
        ((and mine (not (equal (gethash "name" mine) name)))
         (values nil (format nil "This session is deployed as ~a: deploy under that name again, or take it down first."
                             (gethash "name" mine))))
        ((and (null existing) (>= (length (deployment-records)) *max-deployments*))
         (values nil "This lab holds all the deployments it takes."))
        ((not (and (find #\@ payee) (> (length payee) 5)))
         (values nil "Say where your share is to be paid: an email address we can reach you at."))
        ((and (eq *toll-provider* :stripe) (null account))
         (values nil (or account-problem "Set up payouts with Stripe first: the button beside your email.")))
        (t
         (let* ((directory (deployment-directory name))
                ;; the terms as they stand today, what it runs on (revenue
                ;; is reported by runtime), a model's downloads that cost;
                ;; and the lab's own: the session it came from, its kind,
                ;; and the author's account at the gate
                (record (monocle:make-record (lab-house)
                                             :name name :title title :blurb blurb
                                             :owner (session-owner session) :payee payee
                                             :closed? closed? :file-prices prices :existing existing
                                             :pot-percent (and (realp pot-percent) pot-percent)
                                             :fields (append (list "session" (session-id session)
                                                                   "kind" (kind-name (session-kind session)))
                                                             (when account (list "connect_account" account))))))
           (ensure-directories-exist directory)
           (write-text-file (model-body session) (merge-pathnames "model.lisp" directory))
           (monocle:save-record! (lab-house) record)
           (drop-deployed! name)
           (let ((saved (deployment-record name)))
             (log-event session :note "Deployed as ~a: ~a, ~:[open~;closed~] source; of what its users pay, ~a% is the monetization fee~@[ and ~a% goes to the community pot~]~:[~;; paid by card through Stripe to your account~]."
                        name (deployment-url name) closed? (house-fee-percent closed?)
                        (let ((pot (gethash "pot_percent" saved))) (and (realp pot) (plusp pot) pot))
                        account)
             saved)))))))

(defun undeploy! (name owner-key)
  "Take deployment NAME down for the holder of OWNER-KEY.  Values: true, or
nil and the reason.  The books keep what it earned."
  (bt:with-lock-held (*deploy-lock*)
    (let ((record (deployment-record name)))
      (cond ((null record) (values nil "There is no such deployment."))
            ((not (and (stringp owner-key) (equal (gethash "owner" record) owner-key)))
             (values nil "Only its owner may take a deployment down."))
            (t (drop-deployed! name)
               (monocle:delete-deployment! (lab-house) name)
               t)))))


;;
;; A deployment running: its model file compiled into a package of its
;; own, as a replay's is.  Never in *sessions*, never archived or metered.
;;

(defvar *deployed* (make-hash-table :test #'equal)
  "Deployment name -> the session struct its copy is compiled in.")

(defvar *deployed-lock* (bt:make-lock "prompt-lab deployed"))

(defvar *deployed-build-lock* (bt:make-lock "prompt-lab deployed builds"))

(defun drop-deployed! (name)
  (let ((deployed (bt:with-lock-held (*deployed-lock*)
                    (prog1 (gethash name *deployed*) (remhash name *deployed*)))))
    (when deployed
      (forget-session-kind! deployed)
      (let ((package (find-package (session-package-name deployed))))
        (when package (ignore-errors (delete-package package))))
      (ignore-errors (uiop:delete-directory-tree (pathname (session-directory deployed))
                                                 :validate t :if-does-not-exist :ignore)))))

(defun ensure-deployed (name)
  "Deployment NAME ready to run: built when it is not, nil when there is
none or its file no longer compiles."
  (or (bt:with-lock-held (*deployed-lock*) (gethash name *deployed*))
      (bt:with-lock-held (*deployed-build-lock*)
        (or (bt:with-lock-held (*deployed-lock*) (gethash name *deployed*))
            (let* ((record (and *deployments?* (deployment-record name)))
                   (file (and record (merge-pathnames "model.lisp" (deployment-directory name))))
                   (source (and file (probe-file file)
                                (uiop:read-file-string file :external-format :utf-8))))
              (when (and record (plusp (length source)))
                (let* ((keyword (intern (string-upcase (format nil "pl-deployed-~a" name)) :keyword))
                       (package (progn (let ((old (find-package keyword))) (when old (delete-package old)))
                                       (eval `(gdl:define-package ,keyword))
                                       (find-package keyword)))
                       (deployed (make-session-internal
                                  :id name :package-name (package-name package) :replay? t
                                  :owner (gethash "owner" record)
                                  :directory (merge-pathnames (format nil "deployed-~a/" name) *replay-root*))))
                  (ensure-directories-exist (session-directory deployed))
                  (set-session-kind! deployed (if (equal (gethash "kind" record) "app") :app :model))
                  (multiple-value-bind (blocks error?) (write-model deployed source)
                    (declare (ignore blocks))
                    (unless error?
                      (bt:with-lock-held (*deployed-lock*)
                        (setf (gethash name *deployed*) deployed)))))))))))

(defun deployment-admits? (record format owner-key)
  "Whether a deployed model's download in FORMAT opens to a visitor holding
OWNER-KEY (or none): a free format to everyone, a priced one to its owner
-- and to a visitor who has paid, once a gateway takes payment
(*deployment-payments?*; that side is not written yet, so no one else is
let in)."
  (or (not (deployment-file-price record format))
      (and (stringp owner-key) (equal (gethash "owner" record) owner-key))))

(defun deployed-for-viewer (name)
  "The running deployment NAME for the viewer: a deployed model opens to
everyone."
  (and *deployments?* (deployment-record name) (ensure-deployed name)))

(defun deployed-for-download (name format owner-key)
  "The running deployment NAME for the download door, when the visitor may
have its file in FORMAT."
  (let ((record (and *deployments?* (deployment-record name))))
    (and record (deployment-admits? record format owner-key) (ensure-deployed name))))


;;
;; The books.  One line per payment, appended: what was paid, the house's
;; fee and the owner's share at the deployment's own terms, and the
;; engine and Lisp it ran on.  The gate calls for a line when it has
;; settled a payment; nothing here takes money.
;;

(defun revenue-file () (monocle:revenue-file (lab-house)))

(defun book-revenue! (name gross-cents &rest keys &key reference toll test? (card-cents 0))
  "Book a payment of GROSS-CENTS to deployment NAME in the lab's books
(monocle:book-revenue!, which says what each argument is).  Returns the
line booked."
  (declare (ignore reference toll test? card-cents))
  (apply #'monocle:book-revenue! (lab-house) name gross-cents keys))

(defun revenue-lines () (monocle:revenue-lines (lab-house)))

(defun payables (&key tests?)
  "What the books say each author is owed, all time: a list of plists
(:payee :name :payments :payee-cents), one per deployment."
  (monocle:payables (lab-house) :tests? tests?))

(defun earnings (name &key tests?)
  "What deployment NAME has taken, quarter by quarter, the latest first: a
list of plists (:year :quarter :payments :gross-cents :card-cents
:fee-cents :payee-cents)."
  (monocle:earnings (lab-house) name :tests? tests?))

(defun revenue-report (&key year quarter tests?)
  "The books summed by runtime -- the Lisp and the engine (\"gendl\", or
\"solid\" for one with the solids kernel) -- for YEAR and QUARTER (1-4,
UTC) when given: a list of plists (:lisp :engine :payments :gross-cents
:card-cents :fee-cents :fee-before-card-cents :payee-cents), one per
runtime.  Test payments are left out unless TESTS?."
  (monocle:revenue-report (lab-house) :year year :quarter quarter :tests? tests?))


;;
;; The doors.
;;

(defun deployments-off (req ent)
  (refuse req ent net.aserve:*response-not-found* "This lab deploys nothing."))

(defun deploy-door (req ent)
  "POST <prefix>/api/deploy {session, name, payee, title?, blurb?,
pot_percent?, turnstile?}: the session's owner deploys what it built, once it charges
for something (monetizable?); answers the deployment.  Its source is
closed when the session was opened closed-source.  Where a human check
stands it wants its token."
  (let* ((json (request-json req))
         (session (requested-session req json)))
    (cond ((not *deployments?*) (deployments-off req ent))
          ((null session) (no-such-session req ent))
          ((not (owner-request? req session json)) (not-yours req ent))
          (t (multiple-value-bind (ok? reason)
                 (topup-check! session (gethash "turnstile" json) (client-address req))
               (if (not ok?)
                   (refuse req ent net.aserve:*response-forbidden* "~a" reason)
                   (multiple-value-bind (record reason)
                       (deploy-session! session
                                        :name (gethash "name" json) :title (gethash "title" json)
                                        :blurb (gethash "blurb" json)
                                        :payee (gethash "payee" json)
                                        :pot-percent (gethash "pot_percent" json))
                     (if record
                         (respond-json req ent (deployment-state record :owner? t))
                         (refuse req ent "~a" reason)))))))))

(defun undeploy-door (req ent)
  "POST <prefix>/api/undeploy {name}, the owner's key in its header: take
the deployment down."
  (let ((json (request-json req)))
    (if (not *deployments?*)
        (deployments-off req ent)
        (multiple-value-bind (ok? reason)
            (undeploy! (and json (gethash "name" json)) (request-owner-key req json))
          (if ok?
              (respond-json req ent (h "ok" t))
              (refuse req ent "~a" reason))))))

(defun deployments-door (req ent)
  "GET <prefix>/api/deployments: what is deployed on this engine, the
newest first."
  (if (not *deployments?*)
      (deployments-off req ent)
      (respond-json req ent (h "deployments" (map 'vector #'deployment-state (deployment-records))))))

(defun earnings-door (req ent)
  "GET <prefix>/api/earnings?name=<name>, with the owner's key (its header,
or owner=): what the deployment has taken, quarter by quarter -- the
payments, what they cost to take by card, the house's fee and the
author's share, which accumulates through a quarter and is paid out after
it -- and the same for its test payments, apart."
  (let* ((name (query-value req "name"))
         (key (or (request-owner-key req) (query-value req "owner")))
         (record (and *deployments?* (stringp name) (deployment-record name))))
    (cond ((null record) (refuse req ent net.aserve:*response-not-found* "There is no such deployment."))
          ((not (and (stringp key) (equal (gethash "owner" record) key))) (not-yours req ent))
          ;; the amounts are in the lab's unit and named for it: gross_rivets, gross_mites ...
          (t (flet ((quarters (sums)
                      (let ((house (lab-house)))
                        (flet ((amount (sum stem) (or (getf sum (monocle:amount-key house stem)) 0))
                               (field (stem) (monocle:amount-field house stem)))
                          (map 'vector #'(lambda (sum)
                                           (h "year" (getf sum :year) "quarter" (getf sum :quarter)
                                              "payments" (getf sum :payments)
                                              (field "gross") (amount sum :gross)
                                              (field "card") (amount sum :card)
                                              (field "fee") (amount sum :fee)
                                              (field "pot") (amount sum :pot)
                                              (field "yours") (amount sum :payee)))
                               sums)))))
               (let ((real (earnings name))
                     (all (earnings name :tests? t)))
                 (respond-json req ent
                               (h "name" name
                                  "unit" (units)
                                  "fee_percent" (gethash "fee_percent" record)
                                  "pot_percent" (or (gethash "pot_percent" record) 0)
                                  "payee" (gethash "payee" record)
                                  "quarters" (quarters real)
                                  ;; test payments included: no money moved for those
                                  "quarters_with_tests" (quarters all)
                                  "paid_out" (unit-text "Held in {units}, and paid out after a quarter once the share owed has reached the house's minimum, at that day's rate for a {unit}; a smaller sum carries to the next.")))))))))

(defun app-file-door (req ent)
  "GET <prefix>/app-file?iid=<instance>&format=<name>: a web app's model as
a file, as it stands in that visitor's instance of the app -- the first
of its objects, inputs and all (web-app's file-link, kinds.lisp).  A
format the app put a toll on (file-tolls) answers 402 until this visitor
has paid it, and spends a use of the payment."
  (let* ((iid (query-value req "iid"))
         (app (and (stringp iid)
                   (first (gethash (make-keyword-sensitive iid) gwl::*instance-hash-table*))))
         (kind (string-downcase (or (query-value req "format") "")))
         (entry (assoc kind (download-formats) :test #'string=))
         (key (and app entry (typep app 'web-app)
                   (loop for (format toll) on (the-object app file-tolls) by #'cddr
                         when (string-equal format kind) return toll)))
         (model (and app (typep app 'web-app) (first (the-object app objects)))))
    (cond ((or (null app) (not (typep app 'web-app)))
           (refuse req ent net.aserve:*response-not-found* "This page has expired; open the app again."))
          ((null entry) (refuse req ent "No such download here: ~a." kind))
          ((null model) (refuse req ent "This app shows no model to download."))
          ((and key (not (the-object app (toll-paid? key))))
           (refuse req ent (net.aserve::make-resp 402 "Payment Required")
                   "This file is paid for on the app's page first."))
          (t
           (let ((file (merge-pathnames (format nil "download-~a.~a" (random 1000000000) (third entry))
                                        (uiop:temporary-directory))))
             (unwind-protect
                  (multiple-value-bind (ok? note)
                      (handler-case
                          (with-time-limit (*render-seconds* "download")
                            (let ((*package* (symbol-package (the-object app type))))
                              (write-download kind model file)))
                        (error (condition) (values nil (format nil "The model did not write: ~a" condition))))
                    (cond ((not ok?)
                           (refuse req ent (net.aserve::make-resp 422 "Unprocessable Entity") "~a" note))
                          (t
                           (when key (the-object app (use-toll! key)))
                           (net.aserve:with-http-response (req ent :content-type (second entry) :format :binary)
                             (setf (net.aserve:reply-header-slot-value req :content-disposition)
                                   (format nil "attachment; filename=\"~a.~a\""
                                           (or (the-object app deployment-name) "prompt-lab-app") (third entry)))
                             (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
                             (net.aserve:with-http-body (req ent)
                               (with-open-file (in file :element-type '(unsigned-byte 8))
                                 (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
                                       (out (net.aserve:request-reply-stream req)))
                                   (loop for count = (read-sequence buffer in)
                                         while (plusp count)
                                         do (write-sequence buffer out :end count)))))))))
               (when (probe-file file) (ignore-errors (delete-file file)))))))))

(defun respond-deployment (req ent text &key (response net.aserve:*response-ok*) (type "text/plain; charset=utf-8")
                                    attachment)
  (net.aserve:with-http-response (req ent :response response :content-type type)
    (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
    (when attachment
      (setf (net.aserve:reply-header-slot-value req :content-disposition)
            (format nil "attachment; filename=\"~a\"" attachment)))
    (net.aserve:with-http-body (req ent :external-format :utf-8)
      (write-string text net.html.generator:*html-stream*))))

(defun deployment-page (title &rest paragraphs)
  "A small page of the lab's own, for a deployment that cannot open."
  (with-lhtml-string ()
    (:html (:head (:meta :charset "utf-8")
                  (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                  (:title (esc title)))
           (:body :style "font-family: system-ui, sans-serif; max-width: 36rem; margin: 3rem auto; padding: 0 1rem; line-height: 1.5"
                  (:h1 :style "font-size: 1.4rem" (esc title))
                  (dolist (paragraph paragraphs) (htm (:p (esc paragraph))))
                  (:p (:a :href *url-prefix* (esc (lab-title))))))))

(defun deployed-door (req ent)
  "GET <prefix>/d/<name>: the deployment, for whoever may use it -- a web
app as its own page, a model in the viewer.  <prefix>/d/<name>/source: an
open deployment's model file."
  (let* ((path (net.uri:uri-path (net.aserve:request-uri req)))
         (prefix (format nil "~a/d/" *url-prefix*))
         (rest (if (and (> (length path) (length prefix)) (string= prefix path :end2 (length prefix)))
                   (subseq path (length prefix))
                   ""))
         (slash (position #\/ rest))
         (name (subseq rest 0 slash))
         (what (and slash (string-right-trim "/" (subseq rest (1+ slash)))))
         (owner-key (query-value req "owner"))
         (record (and *deployments?* (deployment-record name))))
    (cond ((null record)
           (respond-deployment req ent (deployment-page "Nothing is deployed here" "There is no deployment of that name at this lab.")
                         :response net.aserve:*response-not-found* :type "text/html; charset=utf-8"))
          ((equal what "source")
           (if (truthy? (gethash "closed" record))
               (respond-deployment req ent "This deployment's source is closed." :response net.aserve:*response-not-found*)
               (respond-deployment req ent
                             (format nil ";; ~a -- deployed from the prompt lab at ~a.~%;; GNU Affero General Public License, version 3 or later.~%~%~a"
                                     (gethash "title" record) (deployment-url name)
                                     (file-model-body (merge-pathnames "model.lisp" (deployment-directory name))))
                             :attachment (format nil "~a.lisp" name))))
          ((and what (plusp (length what)))
           (respond-deployment req ent "Not found." :response net.aserve:*response-not-found*))
          ;; a deployed web app is served on the apps' own origin only
          ((and (equal (gethash "kind" record) "app") (not (on-app-origin? req)))
           (to-app-origin req ent))
          (t
           (let ((deployed (ensure-deployed name)))
             (cond ((null deployed)
                    (respond-deployment req ent (deployment-page (gethash "title" record) "This deployment no longer builds on this host.  Its owner can deploy it again.")
                                  :response net.aserve:*response-internal-server-error* :type "text/html; charset=utf-8"))
                   ;; the page instance's own address carries the request's
                   ;; query, and d=<name> in it is how an instance that has
                   ;; expired finds its way back here (lab-session-recovery)
                   ((and (eq (session-kind deployed) :app) (app-defined? deployed)
                         (not (equal (query-value req "d") name)))
                    (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
                      (setf (net.aserve:reply-header-slot-value req :location)
                            (format nil "~a?d=~a~@[&owner=~a~]" (deployment-url name) name owner-key))
                      (net.aserve:with-http-body (req ent))))
                   ((and (eq (session-kind deployed) :app) (app-defined? deployed))
                    (touch deployed)
                    ;; the instance knows which deployment it serves: its
                    ;; tollbooths book to it (kinds.lisp)
                    (gwl-make-object req ent (format nil "~s" (app-symbol deployed))
                                     :make-object-args (list :deployment-name name)))
                   (t
                    (touch deployed)
                    (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
                      (setf (net.aserve:reply-header-slot-value req :location)
                            (format nil "~a/viewer?deployed=~a~@[&owner=~a~]" *url-prefix* name
                                    (and (deployment-priced? record) owner-key)))
                      (net.aserve:with-http-body (req ent))))))))))

;;
;; A toll paid through the gate (*toll-provider* :stripe).  The booth is
;; a form (kinds.lisp, toll-button) that POSTs to <prefix>/toll-pay: the
;; door has Monocle open the payment at the gate's Connect door -- a
;; Checkout Session on the platform, the author's account the
;; destination and the name on the customer's statement -- and sends the
;; visitor to Stripe's page.  Back at <prefix>/toll-return the grant is
;; given only when the gate says the payment settled, which it knows from
;; Stripe's webhook, never from the visitor's return; the line is booked
;; then, with the card cost the charge's balance transaction reports, the
;; engine and the fee at the deployment's terms.  A visitor who paid and
;; never came back is booked by the sweep (sweep-toll-payments!, on the
;; reaper's thread), which asks the gate what is paid and unbooked for
;; this lab.
;;
;; The author's side: at deploy the payee email must have a Stripe
;; account of the author's own, made and onboarded through the gate
;; (author-account, the connect door); its id goes into the deployment's
;; record as connect_account, which the lab never serves.
;;

(defun gate-ask (door plist)
  "Ask the gate's side DOOR with PLIST (string keys and values): Monocle's
way in (lab-house's gate).  Values: the answer, a hash table, and the
status."
  (gate-post door (apply #'h plist)))

(defun request-origin (req)
  "The scheme and host the visitor's browser used, as the proxies
forwarded them: \"https://host\"."
  (flet ((header (value) (and (stringp value) (plusp (length value)) value)))
    (format nil "~a://~a"
            (or (header (net.aserve:header-slot-value req :x-forwarded-proto)) "http")
            (or (header (net.aserve:header-slot-value req :x-cyclops-forwarded-host))
                (header (net.aserve:header-slot-value req :host))
                "localhost"))))

(defun redirect-to (req ent url)
  (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
    (setf (net.aserve:reply-header-slot-value req :location) url)
    (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
    (net.aserve:with-http-body (req ent))))

(defun toll-app (req)
  "The web app instance the request's iid= names, with the toll its key=
names: values (app key toll); or nil and the reason."
  (let* ((iid (query-value req "iid"))
         (app (and (stringp iid)
                   (first (gethash (make-keyword-sensitive iid) gwl::*instance-hash-table*))))
         (key-name (query-value req "key"))
         (toll (and app (typep app 'web-app) (stringp key-name)
                    (find key-name (the-object app tolls)
                          :key #'(lambda (toll) (and (listp toll) (string (getf toll :key))))
                          :test #'string-equal))))
    (cond ((or (null app) (not (typep app 'web-app)))
           (values nil "This page has expired; open the app again."))
          ((null toll) (values nil "No such toll here."))
          ((null (the-object app deployment-name))
           (values nil "This is the app's own preview: nothing is charged here."))
          (t (values app (getf toll :key) toll)))))

(defun toll-pay-door (req ent)
  "POST <prefix>/toll-pay (form: iid, key): open the toll's payment at
the gate and send the visitor to Stripe's page.  With another provider
the toll is taken here, as the booth's own button does."
  (multiple-value-bind (app key toll) (toll-app req)
    (cond ((null app) (refuse req ent net.aserve:*response-not-found* "~a" key))
          ((not (eq *toll-provider* :stripe))
           (the-object app (pay-toll! key))
           (redirect-to req ent (the-object app url)))
          (t
           (let* ((name (the-object app deployment-name))
                  (origin (request-origin req))
                  (back (format nil "~a~a" origin (the-object app url)))
                  (return-url (format nil "~a~a/toll-return?iid=~a&key=~(~a~)"
                                      origin *url-prefix* (the-object app instance-id) key)))
             (multiple-value-bind (url reason)
                 (monocle:open-toll :stripe (lab-house) name toll (the-object app instance-id)
                                    :return-url return-url :cancel-url back)
               (if url
                   (redirect-to req ent url)
                   (respond-deployment req ent
                                       (deployment-page "The payment could not be opened" reason)
                                       :response net.aserve:*response-service-unavailable*
                                       :type "text/html; charset=utf-8"))))))))

(defun toll-return-door (req ent)
  "GET <prefix>/toll-return?iid=&key=&payment=: the visitor is back from
Stripe.  The grant is given when the gate says the payment settled (its
webhook) and the line is booked; until then a page that asks again every
few seconds."
  (multiple-value-bind (app key toll) (toll-app req)
    (let ((payment (query-value req "payment")))
      (cond ((null app) (refuse req ent net.aserve:*response-not-found* "~a" key))
            ((not (and (stringp payment) (< 3 (length payment) 200)
                       (every #'(lambda (c) (or (alphanumericp c) (char= c #\_))) payment)))
             (refuse req ent "No payment named."))
            ((the-object app (toll-paid? key)) (redirect-to req ent (the-object app url)))
            (t
             (multiple-value-bind (paid? word)
                 (monocle:settle-toll! (lab-house) (the-object app deployment-name) payment)
               (cond (paid?
                      (the-object app (grant-toll! key))
                      (redirect-to req ent (the-object app url)))
                     (t
                      (respond-deployment
                       req ent
                       (with-lhtml-string ()
                         (:html (:head (:meta :charset "utf-8")
                                       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                                       (:meta :http-equiv "refresh" :content "4")
                                       (:title "Confirming your payment"))
                                (:body :style "font-family: system-ui, sans-serif; max-width: 36rem; margin: 3rem auto; padding: 0 1rem; line-height: 1.5"
                                       (:h1 :style "font-size: 1.4rem" "Confirming your payment")
                                       (:p "Stripe is telling us the payment went through.  This page asks again every few seconds; there is nothing more to do.")
                                       (:p :style "color: #666" (esc (format nil "Status: ~a." (or word "pending"))))
                                       (:p (:a :href (the-object app url) "Back to the app")))))
                       :type "text/html; charset=utf-8")))))))))

(defun sweep-toll-payments! ()
  "Book the toll payments the gate has settled for this lab that the books
lack (a visitor who paid and never came back), and tell the gate.
Nothing without the :stripe provider."
  (when (eq *toll-provider* :stripe)
    (monocle:settle-unbooked! (lab-house))))

(defun author-account (email &key create? return-url refresh-url)
  "The author's Stripe account for EMAIL, from the gate's connect door
(made when CREATE? and there is none).  Values: the account's id or nil,
an onboarding URL when the gate gave one (RETURN-URL and REFRESH-URL
asked for it), whether it is ready to be paid (details submitted and
charges enabled), and the reason when it is not."
  (multiple-value-bind (answer status)
      (gate-post "connect" (h "email" email
                              "create" (if create? t 'yason:false)
                              "return_url" (or return-url "")
                              "refresh_url" (or refresh-url "")
                              "lab" (monocle:house-lab-name (lab-house))))
    (let* ((account (and answer (gethash "account" answer)))
           (url (and answer (let ((url (gethash "url" answer)))
                              (and (stringp url) (plusp (length url)) url))))
           (ready? (and answer (eq (gethash "details_submitted" answer) t)
                        (eq (gethash "charges_enabled" answer) t))))
      (cond ((eql status 404)
             (values nil nil nil "No Stripe account for that address yet: set up payouts first."))
            ((not (and (eql status 200) (stringp account)))
             (values nil nil nil (or (ignore-errors (gethash "message" (gethash "error" answer)))
                                     "The gate did not answer about payouts; try again in a moment.")))
            (ready? (values account url t nil))
            (t (values account url nil "Payouts are not set up yet: finish Stripe's onboarding first."))))))

(defun connect-door (req ent)
  "POST <prefix>/api/connect {session, email, turnstile?}: the session's
owner sets up payouts -- the gate finds or makes their Stripe account for
EMAIL and answers an onboarding link.  Answers {account, ready, url,
email, reason}.  A lab without the :stripe provider answers 404."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (email (and json (gethash "email" json))))
    (cond ((not (eq *toll-provider* :stripe))
           (refuse req ent net.aserve:*response-not-found* "This lab takes no card payments."))
          ((null session) (no-such-session req ent))
          ((not (owner-request? req session json)) (not-yours req ent))
          ((not (and (stringp email) (find #\@ email) (> (length email) 5)))
           (refuse req ent "Say your email address."))
          (t (multiple-value-bind (ok? reason)
                 (topup-check! session (gethash "turnstile" json) (client-address req))
               (if (not ok?)
                   (refuse req ent net.aserve:*response-forbidden* "~a" reason)
                   (let ((back (page-url req session)))
                     (multiple-value-bind (account url ready? why)
                         (author-account email :create? t
                                               :return-url (concatenate 'string back "&connect=1")
                                               :refresh-url (concatenate 'string back "&connect=refresh"))
                       (respond-json req ent (h "account" (or account "")
                                                "ready" (if ready? t 'yason:false)
                                                "url" (or url "")
                                                "email" email
                                                "reason" (or why "")))))))))))
