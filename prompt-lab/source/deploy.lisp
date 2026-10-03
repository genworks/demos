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

(defun lisp-name ()
  "The Lisp this room runs, as the books name it."
  (format nil "~a ~a" (lisp-implementation-type) (lisp-implementation-version)))

(defun truthy? (value) (or (eq value t) (eq value 'yason:true)))


;;
;; The record.
;;

(defun deployment-name? (name)
  "A deployment's name, the last part of its address: 3 to 40 of a-z, 0-9
and hyphen, a letter first."
  (and (stringp name) (<= 3 (length name) 40)
       (lower-case-p (char name 0)) (char<= #\a (char name 0) #\z)
       (every #'(lambda (c) (or (char<= #\a c #\z) (digit-char-p c) (char= c #\-))) name)))

(defun deployment-directory (name)
  (merge-pathnames (format nil "~a/~a/" (engine-name) name) *deployed-root*))

(defun deployment-record (name)
  "The record of deployment NAME on this engine, a hash table, or nil."
  (and (deployment-name? name)
       (read-record (merge-pathnames "deployment.json" (deployment-directory name)))))

(defun deployment-records ()
  "Every deployment of this engine, the newest first."
  (sort (remove-duplicates
         (remove nil (mapcar #'(lambda (directory)
                                 (read-record (merge-pathnames "deployment.json" directory)))
                             (directory (merge-pathnames (format nil "~a/*/" (engine-name)) *deployed-root*))))
         :key #'(lambda (record) (gethash "name" record)) :test #'equal)
        #'> :key #'(lambda (record) (let ((at (gethash "deployed" record))) (if (realp at) at 0)))))

(defun session-deployment (session)
  "The deployment made from SESSION, its record, or nil."
  (find (session-id session) (deployment-records)
        :key #'(lambda (record) (gethash "session" record)) :test #'equal))

(defun deployment-url (name) (format nil "~a/d/~a" *url-prefix* name))

(defun deployment-priced? (record)
  (let ((price (gethash "price_cents" record))) (and (integerp price) (plusp price))))

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
       "price_cents" (or (gethash "price_cents" record) 0)
       "fee_percent" (gethash "fee_percent" record)
       ;; a priced deployment opens to others once the gate takes payment
       "open" (if (or (not (deployment-priced? record)) *deployment-payments?*) t 'yason:false)
       "deployed" (gethash "deployed" record)
       "payee" (and owner? (gethash "payee" record)))))

(defun clip-line (text limit)
  "TEXT on one line, its ends trimmed, LIMIT characters at most."
  (let ((line (string-trim " " (substitute-if #\space #'(lambda (c) (member c '(#\newline #\return #\tab)))
                                              (if (stringp text) text "")))))
    (subseq line 0 (min limit (length line)))))

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

(defun deploy-session! (session &key name title blurb price-cents payee)
  "Deploy what SESSION built as NAME on the given terms.  Values: the
record, or nil and the reason.  Its source is closed when the session
was opened closed-source, else open.  Deploying again from the same
session under the same name replaces the copy and the terms."
  (bt:with-lock-held (*deploy-lock*)
    (let* ((closed? (session-closed? session))
           (name (and (stringp name) (string-downcase (string-trim " " name))))
           (existing (and (deployment-name? name) (deployment-record name)))
           (mine (session-deployment session))
           ;; a price is a deployed MODEL's, for a download; a web app
           ;; keeps tollbooths of its own (kinds.lisp)
           (price (if (and (integerp price-cents) (plusp price-cents)
                           (not (eq (session-kind session) :app)))
                      price-cents
                      0))
           (payee (clip-line payee 200))
           (symbol (ignore-errors (built-symbol session))))
      (cond
        ((not *deployments?*) (values nil "This lab deploys nothing."))
        ((not (and symbol (find-class symbol nil)))
         (values nil (format nil "There is nothing to deploy yet: build a ~:[model~;web app~] first."
                             (eq (session-kind session) :app))))
        ((not (deployment-name? name))
         (values nil "Give it a name for its address: 3 to 40 lower-case letters, digits and hyphens, a letter first."))
        ((and existing (not (equal (gethash "owner" existing) (session-owner session))))
         (values nil (format nil "The name ~a is taken; choose another." name)))
        ((and mine (not (equal (gethash "name" mine) name)))
         (values nil (format nil "This session is deployed as ~a: deploy under that name again, or take it down first."
                             (gethash "name" mine))))
        ((and (null existing) (>= (length (deployment-records)) *max-deployments*))
         (values nil "This lab holds all the deployments it takes."))
        ((and (plusp price) (not (<= (car *deployment-price-range*) price (cdr *deployment-price-range*))))
         (values nil (format nil "A price is between $~,2f and $~,2f a download."
                             (/ (car *deployment-price-range*) 100) (/ (cdr *deployment-price-range*) 100))))
        ((and (or (plusp price) (eq (session-kind session) :app) (plusp (length payee)))
              (plusp (length payee))
              (not (and (find #\@ payee) (> (length payee) 5))))
         (values nil "That does not read as an email address."))
        ((and (plusp price) (zerop (length payee)))
         (values nil "Say where your share is to be paid: an email address we can reach you at."))
        (t
         (let* ((directory (deployment-directory name))
                (record (h "version" 1
                           "name" name
                           "title" (let ((title (clip-line title 80))) (if (plusp (length title)) title name))
                           "blurb" (clip-line blurb 400)
                           "session" (session-id session)
                           "owner" (session-owner session)
                           "kind" (kind-name (session-kind session))
                           ;; what it runs on: revenue is reported by runtime
                           "engine" (engine-name)
                           "lisp" (lisp-name)
                           "closed" (if closed? t 'yason:false)
                           "price_cents" price
                           "fee_percent" (house-fee-percent closed?)
                           "payee" payee
                           "created" (or (and existing (gethash "created" existing)) (get-universal-time))
                           "deployed" (get-universal-time))))
           (ensure-directories-exist directory)
           (write-text-file (model-body session) (merge-pathnames "model.lisp" directory))
           (write-text-file (with-output-to-string (out) (yason:encode record out))
                            (merge-pathnames "deployment.json" directory))
           (drop-deployed! name)
           (log-event session :note "Deployed as ~a: ~a, ~:[open~;closed~] source; of what its users pay, ~a% is the hosting and licence fee~@[; $~,2f a download~]."
                      name (deployment-url name) closed? (house-fee-percent closed?)
                      (and (plusp price) (/ price 100)))
           (deployment-record name)))))))

(defun undeploy! (name owner-key)
  "Take deployment NAME down for the holder of OWNER-KEY.  Values: true, or
nil and the reason.  The books keep what it earned."
  (bt:with-lock-held (*deploy-lock*)
    (let ((record (deployment-record name)))
      (cond ((null record) (values nil "There is no such deployment."))
            ((not (and (stringp owner-key) (equal (gethash "owner" record) owner-key)))
             (values nil "Only its owner may take a deployment down."))
            (t (drop-deployed! name)
               (uiop:delete-directory-tree (pathname (deployment-directory name))
                                           :validate t :if-does-not-exist :ignore)
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

(defun deployment-admits? (record owner-key)
  "Whether a deployed model's downloads open to a visitor holding OWNER-KEY
(or none): a free one's to everyone, a priced one's to its owner -- and to
a visitor who has paid, once the gate takes payment
(*deployment-payments?*; the gate's side of that is not written yet, so
no one else is let in)."
  (or (not (deployment-priced? record))
      (and (stringp owner-key) (equal (gethash "owner" record) owner-key))))

(defun deployed-for-viewer (name)
  "The running deployment NAME for the viewer: a deployed model opens to
everyone."
  (and *deployments?* (deployment-record name) (ensure-deployed name)))

(defun deployed-for-download (name owner-key)
  "The running deployment NAME for the download door, when the visitor may
have its files."
  (let ((record (and *deployments?* (deployment-record name))))
    (and record (deployment-admits? record owner-key) (ensure-deployed name))))


;;
;; The books.  One line per payment, appended: what was paid, the house's
;; fee and the owner's share at the deployment's own terms, and the
;; engine and Lisp it ran on.  The gate calls for a line when it has
;; settled a payment; nothing here takes money.
;;

(defun revenue-file () (merge-pathnames "revenue.jsonl" *deployed-root*))

(defun exact-cents (amount)
  "AMOUNT as the books write it: exact, never rounded to the cent on a
line -- a small toll's fee is a fraction of one, and the sums are what
get rounded."
  (if (integerp amount) amount (float amount)))

(defun book-revenue! (name gross-cents &key reference toll test? (card-cents 0))
  "Book a payment of GROSS-CENTS to deployment NAME.  CARD-CENTS is what
it cost to take the payment by card -- for a toll paid from a prepaid
balance, the payment's share of what the balance's purchase cost -- and
comes OFF THE TOP: the house's fee and the author's share are split from
what is left, so each bears the card cost in its own proportion.
REFERENCE is the payment's id at whoever settled it, TOLL the name of the
tollbooth it was paid at, TEST? true for a payment no money moved for
(the reports leave those out).  Returns the line booked."
  (let* ((record (or (deployment-record name) (error "There is no deployment ~a." name)))
         (closed? (truthy? (gethash "closed" record)))
         (percent (or (gethash "fee_percent" record) (house-fee-percent closed?)))
         (net (- gross-cents card-cents))
         (fee (/ (* net percent) 100))
         (line (h "time" (get-universal-time)
                  "name" name
                  "engine" (gethash "engine" record)
                  "lisp" (gethash "lisp" record)
                  "closed" (if closed? t 'yason:false)
                  "test" (if test? t 'yason:false)
                  "toll" toll
                  "gross_cents" gross-cents
                  "card_cents" (exact-cents card-cents)
                  "fee_percent" percent
                  ;; the house's fee as kept, after its share of the card
                  ;; cost, and as it stood before that share
                  "fee_cents" (exact-cents fee)
                  "fee_before_card_cents" (exact-cents (/ (* gross-cents percent) 100))
                  "payee_cents" (exact-cents (- net fee))
                  "payee" (gethash "payee" record)
                  "reference" reference)))
    (bt:with-lock-held (*deploy-lock*)
      (ensure-directories-exist (revenue-file))
      (with-open-file (out (revenue-file) :direction :output :if-exists :append
                                          :if-does-not-exist :create :external-format :utf-8)
        (yason:encode line out)
        (terpri out)))
    line))

(defun revenue-lines ()
  (let ((file (revenue-file)))
    (when (probe-file file)
      (with-open-file (in file :external-format :utf-8)
        (loop for text = (read-line in nil) while text
              for line = (ignore-errors (yason:parse text))
              when (hash-table-p line) collect line)))))

(defun payables (&key tests?)
  "What the books say each author is owed, all time: a list of plists
(:payee :name :payments :payee-cents), one per deployment.  What has been
paid out is not kept here: whoever disburses keeps that."
  (let ((sums nil))
    (dolist (line (revenue-lines) (sort sums #'string< :key #'(lambda (sum) (getf sum :name))))
      (when (or tests? (not (gethash "test" line)))
        (let* ((name (gethash "name" line))
               (sum (or (find name sums :key #'(lambda (sum) (getf sum :name)) :test #'equal)
                        (first (push (list :payee (gethash "payee" line) :name name
                                           :payments 0 :payee-cents 0)
                                     sums)))))
          (incf (getf sum :payments))
          (incf (getf sum :payee-cents) (gethash "payee_cents" line)))))))

(defun line-quarter (line)
  "The year and quarter (1-4, UTC) of a line of the books, as a list."
  (multiple-value-bind (s m hour d month year) (decode-universal-time (gethash "time" line) 0)
    (declare (ignore s m hour d))
    (list year (1+ (floor (1- month) 3)))))

(defun earnings (name &key tests?)
  "What deployment NAME has taken, quarter by quarter, the latest first: a
list of plists (:year :quarter :payments :gross-cents :card-cents
:fee-cents :payee-cents).  The author's share accumulates through a
quarter and is paid out after it."
  (let ((sums nil))
    (dolist (line (revenue-lines)
                  (sort sums #'> :key #'(lambda (sum) (+ (* 4 (getf sum :year)) (getf sum :quarter)))))
      (when (and (equal (gethash "name" line) name) (or tests? (not (gethash "test" line))))
        (destructuring-bind (year quarter) (line-quarter line)
          (let ((sum (or (find-if #'(lambda (sum) (and (eql (getf sum :year) year) (eql (getf sum :quarter) quarter)))
                                  sums)
                         (first (push (list :year year :quarter quarter :payments 0 :gross-cents 0
                                            :card-cents 0 :fee-cents 0 :payee-cents 0)
                                      sums)))))
            (incf (getf sum :payments))
            (incf (getf sum :gross-cents) (gethash "gross_cents" line))
            (incf (getf sum :card-cents) (or (gethash "card_cents" line) 0))
            (incf (getf sum :fee-cents) (gethash "fee_cents" line))
            (incf (getf sum :payee-cents) (gethash "payee_cents" line))))))))

(defun revenue-report (&key year quarter tests?)
  "The books summed by runtime -- the Lisp and the engine (\"gendl\", or
\"solid\" for one with the solids kernel) -- for YEAR and QUARTER (1-4,
UTC) when given: a list of plists (:lisp :engine :payments :gross-cents
:card-cents :fee-cents :fee-before-card-cents :payee-cents), one per
runtime.  :fee-cents is the house's revenue from that runtime as kept,
:fee-before-card-cents the same before its share of the card cost.  Test
payments are left out unless TESTS?."
  (let ((sums nil))
    (dolist (line (revenue-lines) (sort sums #'string< :key #'(lambda (sum) (getf sum :lisp))))
      (multiple-value-bind (s m hour d month line-year) (decode-universal-time (gethash "time" line) 0)
        (declare (ignore s m hour d))
        (when (and (or tests? (not (gethash "test" line)))
                   (or (null year) (eql year line-year))
                   (or (null quarter) (eql quarter (1+ (floor (1- month) 3)))))
          (let* ((lisp (or (gethash "lisp" line) "unknown"))
                 (engine (or (gethash "engine" line) "unknown"))
                 (sum (or (find-if #'(lambda (sum) (and (equal (getf sum :lisp) lisp)
                                                        (equal (getf sum :engine) engine)))
                                   sums)
                          (first (push (list :lisp lisp :engine engine :payments 0 :gross-cents 0
                                             :card-cents 0 :fee-cents 0 :fee-before-card-cents 0
                                             :payee-cents 0)
                                       sums)))))
            (incf (getf sum :payments))
            (incf (getf sum :gross-cents) (gethash "gross_cents" line))
            (incf (getf sum :card-cents) (or (gethash "card_cents" line) 0))
            (incf (getf sum :fee-cents) (gethash "fee_cents" line))
            ;; a line older than the field: no card cost was booked
            (incf (getf sum :fee-before-card-cents)
                  (or (gethash "fee_before_card_cents" line) (gethash "fee_cents" line)))
            (incf (getf sum :payee-cents) (gethash "payee_cents" line))))))))


;;
;; The doors.
;;

(defun deployments-off (req ent)
  (refuse req ent net.aserve:*response-not-found* "This lab deploys nothing."))

(defun deploy-door (req ent)
  "POST <prefix>/api/deploy {session, name, title?, blurb?, price_cents?,
payee?, turnstile?}: the session's owner deploys what it built; answers
the deployment.  Its source is closed when the session was opened
closed-source.  Where a human check stands it wants its token."
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
                                        :price-cents (gethash "price_cents" json)
                                        :payee (gethash "payee" json))
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
          (t (flet ((quarters (sums)
                      (map 'vector #'(lambda (sum)
                                       (h "year" (getf sum :year) "quarter" (getf sum :quarter)
                                          "payments" (getf sum :payments)
                                          "gross_cents" (getf sum :gross-cents)
                                          "card_cents" (getf sum :card-cents)
                                          "fee_cents" (getf sum :fee-cents)
                                          "yours_cents" (getf sum :payee-cents)))
                           sums)))
               (let ((real (earnings name))
                     (all (earnings name :tests? t)))
                 (respond-json req ent
                               (h "name" name
                                  "fee_percent" (gethash "fee_percent" record)
                                  "payee" (gethash "payee" record)
                                  "quarters" (quarters real)
                                  ;; test payments included: no money moved for those
                                  "quarters_with_tests" (quarters all)
                                  "paid_out" "After each quarter, once the share owed has reached the house's minimum; a smaller sum carries to the next."))))))))

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
          (t
           (let ((deployed (ensure-deployed name)))
             (cond ((null deployed)
                    (respond-deployment req ent (deployment-page (gethash "title" record) "This deployment no longer builds on this host.  Its owner can deploy it again.")
                                  :response net.aserve:*response-internal-server-error* :type "text/html; charset=utf-8"))
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
