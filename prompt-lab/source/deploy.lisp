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
;; The terms are the owner's: the source open (served beside the
;; deployment) or closed, and use free or priced.  Of what users pay the
;; house keeps *house-fee-percent* as its hosting and licence fee and
;; the owner is owed the rest.  The lab keeps the terms and the books
;; (book-revenue!, revenue-report); taking the money is the gate's, as
;; every payment here is, and until an instance says its gate does
;; (*deployment-payments?*) a priced deployment opens to its owner alone.
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

(defun deploy-session! (session &key name title blurb closed? price-cents payee)
  "Deploy what SESSION built as NAME on the given terms.  Values: the
record, or nil and the reason.  Deploying again from the same session
under the same name replaces the copy and the terms."
  (bt:with-lock-held (*deploy-lock*)
    (let* ((name (and (stringp name) (string-downcase (string-trim " " name))))
           (existing (and (deployment-name? name) (deployment-record name)))
           (mine (session-deployment session))
           (price (if (and (integerp price-cents) (plusp price-cents)) price-cents 0))
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
         (values nil (format nil "A price is between $~,2f and $~,2f a use."
                             (/ (car *deployment-price-range*) 100) (/ (cdr *deployment-price-range*) 100))))
        ((and (plusp price) (not (and (find #\@ payee) (> (length payee) 5))))
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
                           "fee_percent" *house-fee-percent*
                           "payee" payee
                           "created" (or (and existing (gethash "created" existing)) (get-universal-time))
                           "deployed" (get-universal-time))))
           (ensure-directories-exist directory)
           (write-text-file (model-body session) (merge-pathnames "model.lisp" directory))
           (write-text-file (with-output-to-string (out) (yason:encode record out))
                            (merge-pathnames "deployment.json" directory))
           (drop-deployed! name)
           (log-event session :note "Deployed as ~a: ~a, ~:[open~;closed~] source, ~:[free to use~;~:*$~,2f a use, of which ~a% is the hosting and licence fee~]."
                      name (deployment-url name) closed?
                      (and (plusp price) (/ price 100)) *house-fee-percent*)
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
  "Whether deployment RECORD opens to a visitor holding OWNER-KEY (or none):
a free one to everyone, a priced one to its owner -- and to a visitor who
has paid, once the gate takes payment (*deployment-payments?*; the gate's
side of that is not written yet, so no one else is let in)."
  (or (not (deployment-priced? record))
      (and (stringp owner-key) (equal (gethash "owner" record) owner-key))))

(defun deployed-for-viewer (name owner-key)
  "The running deployment NAME for the viewer, when it is a model's and
the visitor may use it."
  (let ((record (deployment-record name)))
    (and record (deployment-admits? record owner-key) (ensure-deployed name))))


;;
;; The books.  One line per payment, appended: what was paid, the house's
;; fee and the owner's share at the deployment's own terms, and the
;; engine and Lisp it ran on.  The gate calls for a line when it has
;; settled a payment; nothing here takes money.
;;

(defun revenue-file () (merge-pathnames "revenue.jsonl" *deployed-root*))

(defun book-revenue! (name gross-cents &key reference)
  "Book a payment of GROSS-CENTS for a use of deployment NAME.  REFERENCE
is the payment's id at whoever settled it.  Returns the line booked."
  (let* ((record (or (deployment-record name) (error "There is no deployment ~a." name)))
         (percent (or (gethash "fee_percent" record) *house-fee-percent*))
         (fee (round (* gross-cents percent) 100))
         (line (h "time" (get-universal-time)
                  "name" name
                  "engine" (gethash "engine" record)
                  "lisp" (gethash "lisp" record)
                  "closed" (if (truthy? (gethash "closed" record)) t 'yason:false)
                  "gross_cents" gross-cents
                  "fee_percent" percent
                  "fee_cents" fee
                  "payee_cents" (- gross-cents fee)
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

(defun revenue-report (&key year quarter)
  "The books summed by runtime -- the Lisp and the engine (\"gendl\", or
\"solid\" for one with the solids kernel) -- for YEAR and QUARTER (1-4,
UTC) when given: a list of plists (:lisp :engine :payments :gross-cents
:fee-cents :payee-cents), one per runtime.  :fee-cents is the house's
revenue from that runtime."
  (let ((sums nil))
    (dolist (line (revenue-lines) (sort sums #'string< :key #'(lambda (sum) (getf sum :lisp))))
      (multiple-value-bind (s m hour d month line-year) (decode-universal-time (gethash "time" line) 0)
        (declare (ignore s m hour d))
        (when (and (or (null year) (eql year line-year))
                   (or (null quarter) (eql quarter (1+ (floor (1- month) 3)))))
          (let* ((lisp (or (gethash "lisp" line) "unknown"))
                 (engine (or (gethash "engine" line) "unknown"))
                 (sum (or (find-if #'(lambda (sum) (and (equal (getf sum :lisp) lisp)
                                                        (equal (getf sum :engine) engine)))
                                   sums)
                          (first (push (list :lisp lisp :engine engine :payments 0 :gross-cents 0
                                             :fee-cents 0 :payee-cents 0)
                                       sums)))))
            (incf (getf sum :payments))
            (incf (getf sum :gross-cents) (gethash "gross_cents" line))
            (incf (getf sum :fee-cents) (gethash "fee_cents" line))
            (incf (getf sum :payee-cents) (gethash "payee_cents" line))))))))


;;
;; The doors.
;;

(defun deployments-off (req ent)
  (refuse req ent net.aserve:*response-not-found* "This lab deploys nothing."))

(defun deploy-door (req ent)
  "POST <prefix>/api/deploy {session, name, title?, blurb?, closed?,
price_cents?, payee?, turnstile?}: the session's owner deploys what it
built; answers the deployment.  Where a human check stands it wants its
token."
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
                                        :closed? (truthy? (gethash "closed" json))
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
          ((not (deployment-admits? record owner-key))
           (respond-deployment req ent
                         (deployment-page (gethash "title" record)
                                          (format nil "This is a priced application: $~,2f a use." (/ (gethash "price_cents" record) 100))
                                          "Payment for deployed applications is not switched on at this lab yet, so for now it opens only to its owner.")
                         :response (net.aserve::make-resp 402 "Payment Required") :type "text/html; charset=utf-8"))
          (t
           (let ((deployed (ensure-deployed name)))
             (cond ((null deployed)
                    (respond-deployment req ent (deployment-page (gethash "title" record) "This deployment no longer builds on this host.  Its owner can deploy it again.")
                                  :response net.aserve:*response-internal-server-error* :type "text/html; charset=utf-8"))
                   ((and (eq (session-kind deployed) :app) (app-defined? deployed))
                    (touch deployed)
                    (gwl-make-object req ent (format nil "~s" (app-symbol deployed))))
                   (t
                    (touch deployed)
                    (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
                      (setf (net.aserve:reply-header-slot-value req :location)
                            (format nil "~a/viewer?deployed=~a~@[&owner=~a~]" *url-prefix* name
                                    (and (deployment-priced? record) owner-key)))
                      (net.aserve:with-http-body (req ent))))))))))
