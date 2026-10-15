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
       ;; whether its author offers a prepaid balance for its tolls (the purse)
       "balance" (if (balance-offered? record) t 'yason:false)
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

(defun deploy-session! (session &key name title blurb payee pot-percent balance?)
  "Deploy what SESSION built as NAME.  Values: the record, or nil and the
reason.  It must have a monetization story (monetizable?).  Its source is
closed when the session was opened closed-source, else open.  POT-PERCENT
is the author's slider: points of every payment, on top of the fee, for
the lab's community pot.  BALANCE? switches on a prepaid balance per
visitor for the app's tolls (the purse).  With the :stripe provider PAYEE must have a
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
                         "This app charges for nothing yet.  Tell the agent what should cost money -- 'charge $3 for each STEP download', 'a day pass for $5 unlocks the results' -- and Monetize opens when it has written the tollbooths."
                         "This model charges for nothing yet.  Tell the agent what should cost money -- 'charge $3 for each STEP download' -- and Monetize opens when it has written that in.")))
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
                                                             (when account (list "connect_account" account))
                                                             ;; the author's switch: a prepaid balance for the app's tolls
                                                             (list "balance" (if balance? t 'yason:false))))))
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
                        (setf (gethash name *deployed*) deployed))
                      ;; the compile is the author's compute (meter-deployment!, below):
                      ;; a refusal marks the stop, which the first run then honours
                      (meter-deployment! name :compile (model-volume deployed))
                      deployed)))))))))

;;
;; A deployed MODEL's priced download is a toll keyed by its format
;; ("step"), paid on Stripe's page from the viewer's tile and answered by
;; a GRANT: an id the lab mints when the gate says the payment settled,
;; good for one download of that format of that deployment, kept in
;; *grants* and in <deployed-root>/grants.json (a restart keeps it),
;; handed to the visitor as grant=<id> on the viewer's address and in a
;; cookie, and spent by the download door when the file is served.  The
;; same doors as a web app's booth, told apart by deployed=<name>.
;;

(defvar *grants* nil
  "Hash table, grant id -> plist (:deployment :format :uses :made :payment);
read from the grants file on first use.")

(defvar *grants-lock* (bt:make-lock "prompt-lab grants"))

(defun grants-file () (merge-pathnames "grants.json" *deployed-root*))

(defun grants ()
  "The grants table, read from its file the first time."
  (bt:with-lock-held (*grants-lock*)
    (or *grants*
        (setf *grants*
              (let ((table (make-hash-table :test #'equal))
                    (json (ignore-errors (read-record (grants-file)))))
                (when (hash-table-p json)
                  (maphash #'(lambda (id g)
                               (when (hash-table-p g)
                                 (setf (gethash id table)
                                       (list :deployment (gethash "deployment" g) :format (gethash "format" g)
                                             :uses (gethash "uses" g) :made (gethash "made" g)
                                             :payment (gethash "payment" g)))))
                           json))
                table)))))

(defun save-grants! ()
  (bt:with-lock-held (*grants-lock*)
    (let ((table (make-hash-table :test #'equal)))
      (maphash #'(lambda (id g)
                   (setf (gethash id table)
                         (h "deployment" (getf g :deployment) "format" (getf g :format)
                            "uses" (getf g :uses) "made" (getf g :made) "payment" (getf g :payment))))
               *grants*)
      (write-text-file (with-output-to-string (out) (yason:encode table out)) (grants-file)))))

(defun grant-for-payment (payment)
  "The grant already minted for PAYMENT (the gate's id), or nil: one grant a
payment, however many times its return page is loaded."
  (grants)
  (bt:with-lock-held (*grants-lock*)
    (loop for id being the hash-keys of *grants* using (hash-value g)
          when (equal (getf g :payment) payment) return id)))

(defun mint-grant! (name format payment &key (uses 1))
  "A grant for USES downloads of FORMAT of deployment NAME, paid by PAYMENT
(the gate's id); its id -- the one minted before for that payment, if any."
  (or (grant-for-payment payment)
      (let ((id (new-owner-key)))
        (grants)
        (bt:with-lock-held (*grants-lock*)
          (setf (gethash id *grants*)
                (list :deployment name :format format :uses uses :made (get-universal-time) :payment payment)))
        (save-grants!)
        id)))

(defun grant-admits? (id name format)
  "Whether grant ID is live for FORMAT of deployment NAME."
  (and (stringp id) (stringp name)
       (let ((g (gethash id (grants))))
         (and g (equal (getf g :deployment) name) (string-equal (getf g :format) format)
              (let ((uses (getf g :uses))) (or (null uses) (plusp uses)))))))

(defun spend-grant! (id)
  "One download served on grant ID."
  (grants)
  (bt:with-lock-held (*grants-lock*)
    (let ((g (gethash id *grants*)))
      (when (and g (integerp (getf g :uses)) (plusp (getf g :uses)))
        (setf (getf g :uses) (1- (getf g :uses)))
        (setf (gethash id *grants*) g))))
  (save-grants!))

(defun grant-cookie (name) (format nil "pl-grant-~a" name))

(defun request-grant (req name)
  "The grant REQ names for deployment NAME: grant= on the query, else the
deployment's cookie."
  (or (query-value req "grant")
      (cdr (assoc (grant-cookie name) (net.aserve:get-cookie-values req) :test #'string=))))

(defun model-toll (record format)
  "The toll a deployed model's priced FORMAT stands behind: keyed by the
format, labelled by its name, priced as RECORD says, one use; nil for a
free format or one not offered."
  (let ((price (deployment-file-price record format))
        (entry (assoc format (download-formats) :test #'string=)))
    (when (and price entry)
      (list :key (intern (string-upcase format) :keyword) :label (fourth entry) (unit-key) price :uses 1))))

(defun model-toll-request (req)
  "For deployed=<name>&format=<f>: values (record format toll); or nil and
the reason."
  (let* ((name (query-value req "deployed"))
         (record (and (stringp name) *deployments?* (deployment-record name)))
         (format (string-downcase (or (query-value req "format") "")))
         (toll (and record (model-toll record format))))
    (cond ((null record) (values nil "There is no such deployment."))
          ((null toll) (values nil "That file is free here, or not offered."))
          (t (values record format toll)))))

(defun payment-id? (payment)
  (and (stringp payment) (< 3 (length payment) 200)
       (every #'(lambda (c) (or (alphanumericp c) (char= c #\_))) payment)))

(defun grant-and-go (req ent name format payment)
  "Mint the grant for FORMAT of deployment NAME paid by PAYMENT, hand it to
the visitor -- grant= on the model's page address, and the deployment's
cookie for a year -- and send them there."
  (let ((id (mint-grant! name format payment)))
    (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
      (setf (net.aserve:reply-header-slot-value req :location)
            (format nil "~a/viewer?deployed=~a&grant=~a" *url-prefix* name id))
      (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
      (net.aserve:set-cookie-header req :name (grant-cookie name) :value id :path "/" :http-only t
                                    :expires (+ (get-universal-time) (* 365 86400)))
      (net.aserve:with-http-body (req ent)))))

(defun confirming-page (req ent back word)
  "The page shown while the gate waits for Stripe's word on a payment:
it asks again every few seconds."
  (respond-deployment
   req ent
   (with-lhtml-string ()
     (:html (:head (:meta :charset "utf-8")
                   (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                   (:meta :http-equiv "refresh" :content "4")
                   (:link :rel "icon" :href "data:,")
                   (:title "Confirming your payment"))
            (:body :style "font-family: system-ui, sans-serif; max-width: 36rem; margin: 3rem auto; padding: 0 1rem; line-height: 1.5"
                   (:h1 :style "font-size: 1.4rem" "Confirming your payment")
                   (:p "Stripe is telling us the payment went through.  This page asks again every few seconds; there is nothing more to do.")
                   (:p :style "color: #666" (esc (format nil "Status: ~a." (or word "pending"))))
                   (:p (:a :href back "Back")))))
   :type "text/html; charset=utf-8"))

(defun model-toll-return (req ent)
  "GET <prefix>/toll-return?deployed=&format=&payment=: the visitor is back
from Stripe; the grant is minted when the gate says the payment settled."
  (multiple-value-bind (record format) (model-toll-request req)
    (let ((payment (query-value req "payment")))
      (cond ((null record) (refuse req ent net.aserve:*response-not-found* "~a" format))
            ((not (payment-id? payment)) (refuse req ent "No payment named."))
            (t
             (let ((name (gethash "name" record)))
               (multiple-value-bind (paid? word) (monocle:settle-toll! (lab-house) name payment)
                 (if paid?
                     (grant-and-go req ent name format payment)
                     (confirming-page req ent (format nil "~a/viewer?deployed=~a" *url-prefix* name) word)))))))))

;;
;; THE PURSE: a prepaid balance per app (the sub-dollar option, step 7
;; of the Monetize build; the mites design of 2026-10-03/04 reconciled
;; with Connect).  An author switches it on at Monetize (the record's
;; "balance"); a visitor then buys a pack (*balance-packs*: $5, $10,
;; $20) in ONE Checkout -- a destination charge to the author like any
;; toll, booked then, the card cost off the top -- and the app's tolls
;; draw on the purse with no card and no Stripe, so a toll under
;; Stripe's 50 c minimum can be taken.  The purse is the visitor's claim
;; on that app alone (named for it, nothing of the house), held here --
;; *purses*, persisted in <deployed-root>/purses.json -- and handed over
;; as the cookie pl-purse-<name> and purse=<id> on the page's address.
;; A draw is a line of its own in the books (kind draw: no money moves),
;; for the author's record of what each toll takes.
;;

(defvar *purses* nil
  "Hash table, purse id -> plist (:deployment :cents :made :topped :spent
:payments); read from the purses file on first use.")

(defvar *purses-lock* (bt:make-lock "prompt-lab purses"))

(defun purses-file () (merge-pathnames "purses.json" *deployed-root*))

(defun purses ()
  "The purses table, read from its file the first time."
  (bt:with-lock-held (*purses-lock*)
    (or *purses*
        (setf *purses*
              (let ((table (make-hash-table :test #'equal))
                    (json (ignore-errors (read-record (purses-file)))))
                (when (hash-table-p json)
                  (maphash #'(lambda (id p)
                               (when (hash-table-p p)
                                 (setf (gethash id table)
                                       (list :deployment (gethash "deployment" p) :cents (or (gethash "cents" p) 0)
                                             :made (gethash "made" p) :topped (or (gethash "topped" p) 0)
                                             :spent (or (gethash "spent" p) 0)
                                             :payments (coerce (or (gethash "payments" p) #()) 'list)))))
                           json))
                table)))))

(defun save-purses! ()
  (bt:with-lock-held (*purses-lock*)
    (let ((table (make-hash-table :test #'equal)))
      (maphash #'(lambda (id p)
                   (setf (gethash id table)
                         (h "deployment" (getf p :deployment) "cents" (getf p :cents) "made" (getf p :made)
                            "topped" (getf p :topped) "spent" (getf p :spent)
                            "payments" (coerce (getf p :payments) 'vector))))
               *purses*)
      (write-text-file (with-output-to-string (out) (yason:encode table out)) (purses-file)))))

(defun purse-cookie (name) (format nil "pl-purse-~a" name))

(defun request-purse (req name)
  "The purse REQ names for deployment NAME: purse= on the query, else the
deployment's cookie."
  (or (query-value req "purse")
      (cdr (assoc (purse-cookie name) (net.aserve:get-cookie-values req) :test #'string=))))

(defun find-purse (id name)
  "Purse ID's plist when it is deployment NAME's; nil otherwise."
  (and (stringp id) (stringp name)
       (let ((p (gethash id (purses))))
         (and p (equal (getf p :deployment) name) p))))

(defun purse-cents (id name)
  "What purse ID holds for deployment NAME, in cents; 0 for none."
  (let ((p (find-purse id name))) (if p (getf p :cents) 0)))

(defun credit-purse! (id name cents payment)
  "Put CENTS into purse ID of deployment NAME, paid by PAYMENT (the gate's
id), minting the purse when ID is not one of NAME's; nothing twice for
the same payment.  Returns the purse's id."
  (let ((id (if (find-purse id name) id (new-owner-key))))
    (purses)
    (bt:with-lock-held (*purses-lock*)
      (let ((p (or (gethash id *purses*)
                   (list :deployment name :cents 0 :made (get-universal-time) :topped 0 :spent 0 :payments nil))))
        (unless (member payment (getf p :payments) :test #'equal)
          (incf (getf p :cents) cents)
          (incf (getf p :topped) cents)
          (push payment (getf p :payments)))
        (setf (gethash id *purses*) p)))
    (save-purses!)
    id))

(defun draw-purse! (id name cents)
  "Take CENTS from purse ID of deployment NAME; true when it held enough."
  (purses)
  (let ((drawn (bt:with-lock-held (*purses-lock*)
                 (let ((p (gethash id *purses*)))
                   (when (and p (equal (getf p :deployment) name) (>= (getf p :cents) cents))
                     (decf (getf p :cents) cents)
                     (incf (getf p :spent) cents)
                     (setf (gethash id *purses*) p)
                     t)))))
    (when drawn (save-purses!))
    drawn))

(defun balance-offered? (record)
  "Whether RECORD's author offers a prepaid balance."
  (and record (truthy? (gethash "balance" record))))

(defun pack? (cents) (and (integerp cents) (member cents *balance-packs*) t))

(defun pack-toll (record pack)
  "The toll a PACK (cents) of RECORD's balance is bought at: keyed
:balance, labelled for the app, priced in the lab's unit."
  (list :key :balance
        :label (format nil "~a balance: ~a" (or (gethash "title" record) (gethash "name" record)) (money-text pack))
        (unit-key) (/ pack *unit-cents*)))

(defun hand-purse (req ent name id back)
  "Hand purse ID of deployment NAME to the visitor -- the cookie for a year,
purse= on the address -- and send them BACK."
  (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
    (setf (net.aserve:reply-header-slot-value req :location)
          (format nil "~a~apurse=~a" back (if (find #\? back) "&" "?") id))
    (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
    (net.aserve:set-cookie-header req :name (purse-cookie name) :value id :path "/" :http-only t
                                  :expires (+ (get-universal-time) (* 365 86400)))
    (net.aserve:with-http-body (req ent))))

(defun purse-request (req)
  "What a purse top-up names: values (record back-url app) -- for a web
app's page (iid) or a deployed model's (deployed) -- or nil and the
reason.  BACK is where the visitor returns to, on this host."
  (let ((iid (query-value req "iid")) (deployed (query-value req "deployed")))
    (cond ((stringp deployed)
           (let ((record (and *deployments?* (deployment-record deployed))))
             (if record
                 (values record (format nil "~a/viewer?deployed=~a" *url-prefix* deployed) nil)
                 (values nil "There is no such deployment."))))
          (t
           (let* ((app (and (stringp iid) (first (gethash (make-keyword-sensitive iid) gwl::*instance-hash-table*))))
                  (name (and app (typep app 'web-app) (the-object app deployment-name)))
                  (record (and name (deployment-record name))))
             (if record
                 (values record (the-object app url) app)
                 (values nil "This page has expired; open the app again.")))))))

(defun purse-topup (req ent)
  "POST <prefix>/toll-pay with pack=<cents> (and iid= or deployed=): buy a
pack of the app's balance -- a destination charge to the author, like a
toll, through the gate; with another provider granted on the spot."
  (multiple-value-bind (record back app) (purse-request req)
    (let* ((pack (let ((p (query-value req "pack"))) (and p (ignore-errors (parse-integer p :junk-allowed t)))))
           (name (and record (gethash "name" record))))
      (cond ((null record) (refuse req ent net.aserve:*response-not-found* "~a" back))
            ((not (balance-offered? record)) (refuse req ent "This app offers no prepaid balance."))
            ((not (pack? pack)) (refuse req ent "No such pack: the packs are ~{~a~^, ~}." (mapcar #'money-text *balance-packs*)))
            ((not (eq *toll-provider* :stripe))
             ;; a test payment, booked as one: the pack is credited at once
             (if (monocle:charge-toll! (lab-house) name (pack-toll record pack) :reference (format nil "purse:~a" name))
                 (let ((id (credit-purse! (request-purse req name) name pack
                                          (format nil "test:purse:~a:~a" name (get-universal-time)))))
                   (when app (the-object app (set-slot! :purse-id id)))
                   (hand-purse req ent name id back))
                 (refuse req ent net.aserve:*response-service-unavailable* "No payment can be taken here.")))
            (t
             (let* ((origin (request-origin req))
                    (return-url (format nil "~a~a/toll-return?~a&pack=~a" origin *url-prefix*
                                        (if app (format nil "iid=~a" (query-value req "iid")) (format nil "deployed=~a" name))
                                        pack)))
               (multiple-value-bind (url reason)
                   (monocle:open-toll :stripe (lab-house) name (pack-toll record pack) (format nil "purse:~a" name)
                                      :return-url return-url :cancel-url (concatenate 'string origin back))
                 (if url
                     (redirect-to req ent url)
                     (respond-deployment req ent
                                         (deployment-page "The payment could not be opened" (list reason)
                                                          :back back :back-label (format nil "Back to ~a" (or (gethash "title" record) name)))
                                         :response net.aserve:*response-service-unavailable*
                                         :type "text/html; charset=utf-8")))))))))

(defun purse-return (req ent)
  "GET <prefix>/toll-return with pack=<cents> and payment=: back from
Stripe after buying a pack; credited to the visitor's purse when the
gate says the payment settled, and the purse handed over."
  (multiple-value-bind (record back app) (purse-request req)
    (let* ((pack (let ((p (query-value req "pack"))) (and p (ignore-errors (parse-integer p :junk-allowed t)))))
           (payment (query-value req "payment"))
           (name (and record (gethash "name" record))))
      (cond ((null record) (refuse req ent net.aserve:*response-not-found* "~a" back))
            ((not (pack? pack)) (refuse req ent "No such pack."))
            ((not (payment-id? payment)) (refuse req ent "No payment named."))
            (t (multiple-value-bind (paid? word) (monocle:settle-toll! (lab-house) name payment)
                 (if paid?
                     (let ((id (credit-purse! (request-purse req name) name pack payment)))
                       (when app (the-object app (set-slot! :purse-id id)))
                       (hand-purse req ent name id back))
                     (confirming-page req ent back word))))))))

(defun draw-for-toll! (req name toll)
  "Pay TOLL at deployment NAME from the visitor's purse when it holds
enough: the draw booked; the purse's id, or nil."
  (let* ((id (request-purse req name))
         (cents (* (monocle:toll-price toll) *unit-cents*)))
    (when (and (find-purse id name) (draw-purse! id name cents))
      (ignore-errors
       (monocle:book-draw! (lab-house) name (monocle:toll-price toll)
                           :toll (string-downcase (symbol-name (getf toll :key))) :purse id
                           :reference (format nil "purse:~a:~a" id (get-universal-time))))
      id)))

(defun deployment-admits? (record format owner-key &optional grant)
  "Whether a deployed model's download in FORMAT opens to a visitor holding
OWNER-KEY (or none) and GRANT (or none): a free format to everyone, a
priced one to its owner, and to a visitor whose GRANT is live for it --
bought on the model's page (model-toll-pay)."
  (or (not (deployment-file-price record format))
      (and (stringp owner-key) (equal (gethash "owner" record) owner-key))
      (grant-admits? grant (gethash "name" record) format)))

(defun toll-pay-door (req ent)
  "POST <prefix>/toll-pay: a web app's booth (form: iid, key), a deployed
model's priced file (deployed, format), or a pack of an app's prepaid
balance (iid or deployed, pack=<cents>).  A toll the visitor's purse
covers is drawn from it here; otherwise the payment is opened at the
gate and the visitor sent to Stripe's page.  With another provider the
toll is taken here, as the booth's own button does."
  (cond ((query-value req "pack") (purse-topup req ent))
        ((query-value req "deployed") (model-toll-pay req ent))
        (t
         (multiple-value-bind (app key toll) (toll-app req)
           (cond ((null app) (refuse req ent net.aserve:*response-not-found* "~a" key))
                 ;; from the visitor's purse, when it holds enough
                 ((draw-for-toll! req (the-object app deployment-name) toll)
                  (the-object app (grant-toll! key))
                  (redirect-to req ent (the-object app url)))
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
                                              (deployment-page "The payment could not be opened" (list reason)
                                                               :back (the-object app url)
                                                               :back-label (format nil "Back to ~a"
                                                                                   (or (gethash "title" (deployment-record name)) name)))
                                              :response net.aserve:*response-service-unavailable*
                                              :type "text/html; charset=utf-8"))))))))))

(defun toll-return-door (req ent)
  "GET <prefix>/toll-return: the visitor is back from Stripe -- a web app's
booth (iid, key, payment), a deployed model's file (deployed, format,
payment), or a pack of the balance (iid or deployed, pack, payment).
The grant or the credit is given when the gate says the payment settled
(its webhook) and the line is booked; until then a page that asks again
every few seconds."
  (cond ((query-value req "pack") (purse-return req ent))
        ((query-value req "deployed") (model-toll-return req ent))
        (t
         (multiple-value-bind (app key toll) (toll-app req)
           (let ((payment (query-value req "payment")))
             (cond ((null app) (refuse req ent net.aserve:*response-not-found* "~a" key))
                   ((not (payment-id? payment)) (refuse req ent "No payment named."))
                   ((the-object app (toll-paid? key)) (redirect-to req ent (the-object app url)))
                   (t
                    (multiple-value-bind (paid? word)
                        (monocle:settle-toll! (lab-house) (the-object app deployment-name) payment)
                      (cond (paid?
                             (the-object app (grant-toll! key))
                             (redirect-to req ent (the-object app url)))
                            (t (confirming-page req ent (the-object app url) word)))))))))))

(defun model-toll-pay (req ent)
  "POST <prefix>/toll-pay (deployed, format): a deployed model's priced
file -- drawn from the visitor's purse when it holds enough, else opened
at the gate and the visitor sent to Stripe's page; with another provider
the toll is taken on the spot (a test payment, booked as one) and the
grant handed over."
  (multiple-value-bind (record format toll) (model-toll-request req)
    (let ((name (and record (gethash "name" record))))
      (cond ((null record) (refuse req ent net.aserve:*response-not-found* "~a" format))
            ((let ((purse (draw-for-toll! req name toll)))
               (and purse (grant-and-go req ent name format (format nil "purse:~a:~a" purse (get-universal-time))))))
            ((not (eq *toll-provider* :stripe))
             (if (monocle:charge-toll! (lab-house) name toll :reference (format nil "model:~a:~a" name format))
                 (grant-and-go req ent name format (format nil "test:~a:~a:~a" name format (get-universal-time)))
                 (refuse req ent net.aserve:*response-service-unavailable* "No payment can be taken here.")))
            (t
             (let* ((origin (request-origin req))
                    (viewer (format nil "~a~a/viewer?deployed=~a" origin *url-prefix* name))
                    (return-url (format nil "~a~a/toll-return?deployed=~a&format=~a" origin *url-prefix* name format)))
               (multiple-value-bind (url reason)
                   (monocle:open-toll :stripe (lab-house) name toll (format nil "model:~a:~a" name format)
                                      :return-url return-url :cancel-url viewer)
                 (if url
                     (redirect-to req ent url)
                     (respond-deployment req ent
                                         (deployment-page "The payment could not be opened" (list reason)
                                                          :back viewer
                                                          :back-label (format nil "Back to ~a" (or (gethash "title" record) name)))
                                         :response net.aserve:*response-service-unavailable*
                                         :type "text/html; charset=utf-8")))))))))

(defun deployed-for-viewer (name)
  "The running deployment NAME for the viewer: a deployed model opens to
everyone."
  ;; the deployment's door metered the run and sends the visitor here;
  ;; a deployment whose hosting is spent (deployment-stopped?) shows nothing
  (and *deployments?* (deployment-record name) (not (deployment-stopped? name)) (ensure-deployed name)))

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
                                        :pot-percent (gethash "pot_percent" json)
                                        :balance? (eq (gethash "balance" json) t))
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
          ;; writing the file runs the model: the author's compute (the hosting meter, below)
          ((and (the-object app deployment-name)
                (not (meter-deployment! (the-object app deployment-name) :run
                                        (deployment-volume (the-object app deployment-name)))))
           (refuse req ent net.aserve:*response-service-unavailable* "This app is paused: its hosting is spent."))
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

(defun deployment-page (title paragraphs &key back (back-label "Back"))
  "A small page for a deployment that cannot open, or a payment that could
not: TITLE, the PARAGRAPHS, and a way BACK (to the app itself) when
there is one.  Nothing of the house on it: the deployment is its
author's (the user's ruling, 2026-10-14)."
  (with-lhtml-string ()
    (:html (:head (:meta :charset "utf-8")
                  (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                  (:link :rel "icon" :href "data:,")
                  (:title (esc title)))
           (:body :style "font-family: system-ui, sans-serif; max-width: 36rem; margin: 3rem auto; padding: 0 1rem; line-height: 1.5"
                  (:h1 :style "font-size: 1.4rem" (esc title))
                  (dolist (paragraph paragraphs) (htm (:p (esc paragraph))))
                  (when back (htm (:p (:a :href back (esc back-label)))))))))

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
           (respond-deployment req ent (deployment-page "Nothing is deployed here" (list "There is no deployment of that name here."))
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
                    (respond-deployment req ent (deployment-page (gethash "title" record) (list "This deployment no longer builds on this host.  Its owner can deploy it again."))
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
                   ;; the run is the author's compute, metered against the
                   ;; deployment's hosting; spent, the deployment pauses (below)
                   ((not (meter-deployment! name :run (model-volume deployed)))
                    (paused-page req ent record (deployment-stopped? name)))
                   ((and (eq (session-kind deployed) :app) (app-defined? deployed))
                    (touch deployed)
                    ;; the instance knows which deployment it serves: its
                    ;; tollbooths book to it (kinds.lisp)
                    (gwl-make-object req ent (format nil "~s" (app-symbol deployed))
                                     ;; and the visitor's purse, from its cookie, so the booths can say the balance
                                     :make-object-args (list :deployment-name name
                                                             :purse-id (request-purse req name))))
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

;;
;; The upstream licensor's view of what accrues to it from the hosted
;; applications (step 8 of the Monetize build): the lab's books through
;; monocle:upstream-report, under RATES the house keeps in a private
;; file -- nothing of the arithmetic is in this repository -- behind a
;; token of its own, so the licensor's reader needs no lab password.
;; `<prefix>/api/royalty?token=` answers JSON, `<prefix>/royalty?token=`
;; a page; without the files both answer 404.  The books are this
;; host's; a house with several hosts reads each.
;;

(defvar *royalty-rates-file* nil
  "String or nil. The file holding the licensor's rates as monocle:upstream-report
reads them, a plist (:percent :lisp-match :attribution :minimum); set by
the stack's init, read at every request.  Nil: no royalty view here.")

(defvar *royalty-token-file* nil
  "String or nil. The file whose first line is the token that opens the
royalty view (token= on the query, or X-Prompt-Lab-Royalty).  Nil: no
royalty view here.")

(defun royalty-rates ()
  "The rates plist, read from *royalty-rates-file*; nil without one."
  (and (stringp *royalty-rates-file*)
       (ignore-errors
        (with-open-file (in *royalty-rates-file* :if-does-not-exist nil)
          (and in (let ((*read-eval* nil))
                    (let ((plist (read in nil nil)))
                      (and (consp plist) (evenp (length plist)) plist))))))))

(defun royalty-token ()
  (and (stringp *royalty-token-file*)
       (ignore-errors
        (with-open-file (in *royalty-token-file* :if-does-not-exist nil)
          (and in (let ((line (read-line in nil nil)))
                    (and (stringp line) (plusp (length (string-trim " " line))) (string-trim " " line))))))))

(defun royalty-admits? (req)
  (let ((token (royalty-token))
        (given (or (query-value req "token") (net.aserve:header-slot-value req :x-prompt-lab-royalty))))
    (and token (stringp given) (string= token given))))

(defun royalty-report (&key tests?)
  "The licensor's report from this host's books under the private rates;
nil without rates."
  (let ((rates (royalty-rates)))
    (and rates (monocle:upstream-report (lab-house) rates :tests? tests?))))

(defun dollars-of (units)
  "UNITS of the lab's unit as money, two places."
  (money-text (* units *unit-cents*)))

(defun royalty-door (req ent)
  "GET <prefix>/api/royalty?token=<token>[&tests=1]: the licensor's share
quarter by quarter, as JSON; 404 without the rates or the token."
  (cond ((not (and (royalty-rates) (royalty-token)))
         (refuse req ent net.aserve:*response-not-found* "No royalty view here."))
        ((not (royalty-admits? req))
         (refuse req ent net.aserve:*response-forbidden* "This view opens only with its token."))
        (t (let ((report (royalty-report :tests? (equal (query-value req "tests") "1"))))
             (respond-json req ent
                           (h "unit" (units) "cents_per_unit" *unit-cents*
                              "percent" (getf report :percent)
                              "other_half" (getf report :other-half)
                              "quarters" (map 'vector
                                              #'(lambda (q)
                                                  (h "year" (getf q :year) "quarter" (getf q :quarter)
                                                     "share" (getf q :share) "rounded" (getf q :rounded)
                                                     "carried_in" (getf q :carried-in) "payable" (getf q :payable)
                                                     "carried_out" (getf q :carried-out)
                                                     "rows" (map 'vector
                                                                 #'(lambda (row)
                                                                     (h "lisp" (getf row :lisp) "engine" (getf row :engine)
                                                                        "licensed" (if (getf row :licensed?) t 'yason:false)
                                                                        "payments" (getf row :payments) "refunds" (getf row :refunds)
                                                                        "base" (getf row :base) "attributable" (getf row :attributable)
                                                                        "share" (getf row :share)))
                                                                 (getf q :rows))))
                                              (getf report :quarters))))))))

(defun royalty-page (req ent)
  "GET <prefix>/royalty?token=<token>[&tests=1]: the same, as a page for
the licensor's reader."
  (cond ((not (and (royalty-rates) (royalty-token)))
         (respond-deployment req ent (deployment-page "Nothing here" (list "There is no royalty view on this host."))
                             :response net.aserve:*response-not-found* :type "text/html; charset=utf-8"))
        ((not (royalty-admits? req))
         (respond-deployment req ent (deployment-page "Not yours" (list "This page opens only with its token."))
                             :response net.aserve:*response-forbidden* :type "text/html; charset=utf-8"))
        (t
         (let* ((tests? (equal (query-value req "tests") "1"))
                (report (royalty-report :tests? tests?))
                (minimum (or (getf (royalty-rates) :minimum) 0)))
           (respond-deployment
            req ent
            (with-lhtml-string ()
              (:html (:head (:meta :charset "utf-8")
                            (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                            (:link :rel "icon" :href "data:,")
                            (:title "Hosted applications: the royalty accruing")
                            (:style "body{font-family:system-ui,sans-serif;max-width:60rem;margin:2rem auto;padding:0 1rem;line-height:1.45}table{border-collapse:collapse;margin:.5rem 0 1rem}th,td{border-bottom:1px solid #ddd;padding:.3rem .6rem;text-align:right}th:first-child,td:first-child,th:nth-child(2),td:nth-child(2){text-align:left}.muted{color:#666}"))
                     (:body
                      (:h1 :style "font-size:1.4rem" "Hosted applications: the royalty accruing")
                      (:p :class "muted"
                          (fmt "This host's books, read ~a.  The base is the house's fee before its share of the card cost, net of refunds and disputes; the share is ~a% of what is attributable, summed over each quarter and rounded once; a quarter under ~a is carried into the next.  Amounts in ~a (~a)."
                               (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time) 0)
                                 (declare (ignore s)) (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d UTC" y mo d h m))
                               (getf report :percent) (dollars-of minimum) (units) (dollars-of 1))
                          (when tests? (htm " " (:b "Test payments are included on this page."))))
                      (dolist (q (getf report :quarters))
                        (htm (:h2 :style "font-size:1.1rem;margin:1.2rem 0 .3rem" (fmt "~a Q~a" (getf q :year) (getf q :quarter)))
                             (:table
                              (:thead (:tr (:th "Runtime") (:th "Engine") (:th "Payments") (:th "Refunds") (:th "Base") (:th "Attributable") (:th "Share")))
                              (:tbody
                               (dolist (row (getf q :rows))
                                 (htm (:tr (:td (esc (getf row :lisp)))
                                           (:td (esc (getf row :engine)))
                                           (:td (fmt "~d" (getf row :payments)))
                                           (:td (fmt "~d" (getf row :refunds)))
                                           (:td (str (dollars-of (getf row :base))))
                                           (:td (if (getf row :licensed?) (str (dollars-of (getf row :attributable))) (htm (:span :class "muted" "no part"))))
                                           (:td (if (getf row :licensed?) (str (dollars-of (getf row :share))) (htm (:span :class "muted" "none")))))))))
                             (:p (fmt "Share for the quarter ~a, rounded ~a; carried in ~a; "
                                      (dollars-of (getf q :share)) (dollars-of (getf q :rounded)) (dollars-of (getf q :carried-in)))
                                 (if (plusp (getf q :payable))
                                     (htm (:b (fmt "payable ~a" (dollars-of (getf q :payable)))))
                                     (htm (fmt "carried out ~a" (dollars-of (getf q :carried-out))))))))
                      (:p :class "muted" (fmt "Not attributed of the licensed runtimes' fees, all time (the other half): ~a." (dollars-of (getf report :other-half))))
                      (:p :class "muted" "Runtimes the licensor has no part in are summed the same way and shown with no share."))))
            :type "text/html; charset=utf-8")))))

;;
;; HOSTING METERED AGAINST THE AUTHOR (step 9 of the Monetize build;
;; the user's point of 2026-10-14: a busy app with a free tier of its
;; own is what neither the revenue share nor an idle-stop covers).  A
;; deployment's compile (ensure-deployed) and every run -- a visit to an
;; app or a model, a download -- are measured by symbol volume as a
;; session's are (meter.lisp) and reported to the gate's host-meter
;; door, which books them against the deployment's hosting: the month's
;; free allowance first, then the credits bought for it.  When the gate
;; refuses, the deployment PAUSES: its page is the begging page, which
;; offers hosting -- bought from the house in one Checkout (the gate's
;; host-topup, the house's own sale; host-confirm credits it) by its
;; author, or anyone -- and the stop is lifted.  The gate's
;; stopped-since date is what the un-hosting rule counts from; nothing
;; here un-hosts.  A gate without the door, or none, meters nothing and
;; the deployment runs on.
;;

(defvar *stopped* (make-hash-table :test #'equal)
  "Deployment name -> the gate's hosting answer when it last refused, so
the page can say what is left; cleared when it serves again.")

(defun deployment-lab-name () (monocle:house-lab-name (lab-house)))

(defun meter-deployment! (name kind symbols)
  "Report to the gate that deployment NAME did KIND (:compile or :run)
over SYMBOLS symbols, against its author's hosting.  Values: true when
served -- or when there is nothing to meter, or no gate to meter it --
and nil with the gate's answer (a hash table) when the hosting is spent,
so the caller pauses the deployment."
  (if (or (not *hosting-meter?*) (not (integerp symbols)) (zerop symbols))
      t
      (handler-case
          (multiple-value-bind (json status)
              (gate-post "host-meter" (h "lab" (deployment-lab-name) "deployment" name
                                         "kind" (string-downcase (symbol-name kind))
                                         "symbols" symbols "engine" (engine-name)))
            (cond ((eql status 429)
                   (setf (gethash name *stopped*) (or json (h)))
                   (values nil json))
                  (t (remhash name *stopped*) t)))
        (error () t))))

(defun deployment-stopped? (name) (gethash name *stopped*))

(defun deployment-volume (name)
  "The symbol volume of the running deployment NAME's model file: what a
run of it is metered at; 0 when it is not running."
  (let ((deployed (bt:with-lock-held (*deployed-lock*) (gethash name *deployed*))))
    (if deployed (model-volume deployed) 0)))

(defun hosting-balance (name)
  "The deployment's hosting as the gate says it, a hash table, or nil."
  (ignore-errors
   (multiple-value-bind (json status) (gate-post "host-balance" (h "lab" (deployment-lab-name) "deployment" name))
     (and (eql status 200) (hash-table-p json) json))))

(defun hosting-offers (balance)
  "The packs of hosting the gate sells, (cents . credits) pairs, from a
balance answer; nil when it names none."
  (let ((amounts (and balance (coerce (or (gethash "topup_amounts" balance) #()) 'list)))
        (credits (and balance (coerce (or (gethash "topup_credits" balance) #()) 'list))))
    (loop for cents in amounts for n in (or credits amounts) collect (cons cents n))))

(defun paused-page (req ent record &optional balance)
  "The page a paused deployment answers: what is spent, and how to add
hosting for it."
  (let* ((name (gethash "name" record))
         (title (or (gethash "title" record) name))
         (balance (or balance (hosting-balance name)))
         (offers (hosting-offers balance)))
    (respond-deployment
     req ent
     (with-lhtml-string ()
       (:html (:head (:meta :charset "utf-8")
                     (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                     (:link :rel "icon" :href "data:,")
                     (:title (esc (format nil "~a is paused" title))))
              (:body :style "font-family: system-ui, sans-serif; max-width: 36rem; margin: 3rem auto; padding: 0 1rem; line-height: 1.5"
                     (:h1 :style "font-size: 1.4rem" (esc (format nil "~a is paused" title)))
                     (:p "Its hosting allowance for the month, and what was added for it, are spent: it runs again when hosting is added.")
                     (when balance
                       (htm (:p :style "color: #666"
                                (esc (format nil "This month: ~a of ~a free credits used; ~a bought credits left."
                                             (round (or (gethash "free_used" balance) 0)) (or (gethash "free_credits" balance) 0)
                                             (round (or (gethash "credits" balance) 0)))))))
                     (when offers
                       (htm (:p "Add hosting for it:")
                            (:p (dolist (offer offers)
                                  (htm (:form :method "post" :action (format nil "~a/host-topup" *url-prefix*) :style "display:inline;margin-right:.6rem"
                                              (:input :type "hidden" :name "deployed" :value name)
                                              (:input :type "hidden" :name "amount_cents" :value (format nil "~d" (car offer)))
                                              (:button :type "submit" :style "font:inherit;cursor:pointer;padding:.3rem .8rem"
                                                       (esc (format nil "~:d credits for ~a" (cdr offer) (money-text (car offer)))))))))))
                     (:p :style "color: #666" "Hosting credits pay for the compute its visitors use, and keep until they are used."))))
     :response net.aserve:*response-service-unavailable*
     :type "text/html; charset=utf-8")))

(defun host-topup-door (req ent)
  "POST <prefix>/host-topup (form: deployed, amount_cents): buy hosting
for a deployment at the gate and go to Stripe's page."
  (let* ((name (query-value req "deployed"))
         (record (and (stringp name) *deployments?* (deployment-record name)))
         (amount (let ((a (query-value req "amount_cents"))) (and a (ignore-errors (parse-integer a :junk-allowed t)))))
         (origin (request-origin req)))
    (cond ((null record) (refuse req ent net.aserve:*response-not-found* "There is no such deployment."))
          ((not (integerp amount)) (refuse req ent "Say how much."))
          (t
           (multiple-value-bind (answer status)
               (gate-post "host-topup" (h "lab" (deployment-lab-name) "deployment" name
                                          "title" (or (gethash "title" record) name)
                                          "amount_cents" amount
                                          "success_url" (format nil "~a~a/host-return?deployed=~a" origin *url-prefix* name)
                                          "cancel_url" (format nil "~a~a" origin (deployment-url name))))
             (let ((url (and answer (gethash "url" answer))))
               (if (and (eql status 200) (stringp url))
                   (redirect-to req ent url)
                   (respond-deployment req ent
                                       (deployment-page "Hosting could not be bought"
                                                        (list (or (ignore-errors (gethash "message" (gethash "error" answer)))
                                                                  "The gate did not answer; try again in a moment."))
                                                        :back (deployment-url name) :back-label (format nil "Back to ~a" (or (gethash "title" record) name)))
                                       :response net.aserve:*response-service-unavailable*
                                       :type "text/html; charset=utf-8"))))))))

(defun host-return-door (req ent)
  "GET <prefix>/host-return?deployed=&checkout=: back from Stripe; the
gate credits the deployment's hosting once and the deployment runs again."
  (let* ((name (query-value req "deployed"))
         (record (and (stringp name) *deployments?* (deployment-record name)))
         (checkout (query-value req "checkout")))
    (cond ((null record) (refuse req ent net.aserve:*response-not-found* "There is no such deployment."))
          ((not (payment-id? checkout)) (refuse req ent "No checkout named."))
          (t
           (multiple-value-bind (answer status)
               (gate-post "host-confirm" (h "lab" (deployment-lab-name) "deployment" name "checkout" checkout))
             (let ((outcome (and answer (gethash "outcome" answer))))
               (when (member outcome '("credited" "already") :test #'equal)
                 (remhash name *stopped*))
               (respond-deployment
                req ent
                (deployment-page (cond ((equal outcome "credited") "Hosting added")
                                       ((equal outcome "already") "Hosting was already added")
                                       ((equal outcome "unpaid") "The payment has not completed yet")
                                       (t "Hosting could not be confirmed"))
                                 (list (cond ((member outcome '("credited" "already") :test #'equal)
                                              (format nil "~a has ~a hosting credits now and runs again."
                                                      (or (gethash "title" record) name) (round (or (gethash "credits" answer) 0))))
                                             (t (or (and answer (gethash "text" answer)) (format nil "The gate answered ~a." status)))))
                                 :back (deployment-url name) :back-label (format nil "Open ~a" (or (gethash "title" record) name)))
                :type "text/html; charset=utf-8")))))))
