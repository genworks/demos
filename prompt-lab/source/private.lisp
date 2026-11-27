(in-package :prompt-lab)

;;
;; THE PRIVATE FRONT (the trust ladder's rung 2).  An author whose key the
;; gate holds at the trusted rung (or above) opens a PRIVATE SESSION here:
;; the whole lab in a microVM of its own on the hangar's host, its work on a
;; disk of the author's, nobody else's eyes on it.  The front asks the gate
;; for the key's standing (<messages-url>/author), files a request in the
;; hangar's spool (*private-spool*/requests/<id>.json, which the hangar's
;; loop raises a VM for), reads the hangar's answer (spool/ready/<id>.json)
;; and, once the VM is up, sends the author's browser into it with the
;; session's token -- the VM's lab password, made here and handed to this
;; browser alone.  The front runs no visitor's code: it may share a room
;; with anything; the hangar is what must have KVM.
;;
;;   <prefix>                the page: an author key in, a private lab out
;;   <prefix>/api/open       POST {wallet}: open (or find) the author's session
;;   <prefix>/api/opened     GET ?id=&wallet=: the hangar's word on it
;;   <prefix>/api/close      POST {id, wallet}: end it
;;
;; THE MONETIZING RUNG.  A session whose author the gate holds at the
;; monetizing standing when it opens is told so (the request's "monetize",
;; which the hangar carries into the VM) and offers Monetize; its deploy and
;; undeploy come back here, to the SESSION DOORS, which take the session's
;; id and token as credentials -- the token is what the front handed the
;; author's browser and the hangar the VM, and the request must be live:
;;
;;   <prefix>/api/vm/deploy       POST {id, token, name, title, blurb, payee,
;;                                closed, pot_percent, balance, kind, session,
;;                                owner, model}: a web app deployed
;;   <prefix>/api/vm/undeploy     POST {id, token, name}: taken down
;;   <prefix>/api/vm/deployments  POST {id, token}: this author's deployments
;;
;; The front asks the gate for the standing again at every deploy, writes
;; the terms itself (nothing the VM says of fees or accounts is taken), and
;; keeps the deployment in the PRIVATE DEPLOYED ROOT (*private-deployed-root*,
;; on the hangar's volume), which the hangar flies at <name>.<domain> under
;; the private lab's name (*private-lab-name*).  The books of that root are
;; kept here too: a sweeper books the tolls and hosting the gate settled
;; for the private lab's deployments, whose VMs book nothing.
;;

(defparameter *private-spool* nil
  "String or nil.  The hangar's spool directory as this room sees it (a
mount of the hangar's $FC_DIR/spool), with requests/ and ready/ in it.  Nil:
no private front.")

(defparameter *private-prefix* "/own-lab"
  "The front's path; the proxy sends its host's root here.  Not /private...:
the fleet's scanner lists drop every path under /private before any rule.")

(defparameter *private-session-domain* "gen.works"
  "The domain a session's host is under: https://<id>.<domain>/.")

(defparameter *private-session-scheme* "https"
  "The scheme of a session's address (http on a dev ship with no certificate).")

(defparameter *private-session-prefix* "/prompt-lab"
  "Where the lab answers inside a session's VM.")

(defparameter *private-deployed-root* nil
  "String or nil.  The private deployed root as this room sees it (a mount
of the hangar's $FC_DIR/private-deployed): <root>/<engine>/<name>/ for each
deployment made from a private session, and that root's books.  Nil: no
private session deploys.")

(defparameter *private-lab-name* "prompt-lab-private"
  "String.  The private lab's name at the gate -- the hangar's
HANGAR_SESSION_LAB, the name its sessions and their deployments report
under -- and so the name this front keeps their books under.")

(defparameter *private-sweep-seconds* 60
  "Integer.  How often the front books what the gate settled for the
private lab's deployments.")

(defun private-file (kind id)
  (format nil "~a/~a/~a.json" (string-right-trim "/" *private-spool*) kind id))

(defun private-id? (id)
  (and (stringp id) (= (length id) 13) (string= "pl-" id :end2 3)
       (every (lambda (c) (digit-char-p c 16)) (subseq id 3))))

(defun private-requests ()
  "Every request in the spool, as hash tables."
  (loop for file in (directory (format nil "~a/requests/*.json" (string-right-trim "/" *private-spool*)))
        for json = (ignore-errors (read-record file))
        when (hash-table-p json) collect json))

(defun private-answer (id)
  "The hangar's word on request ID (a hash table), or nil while it has said nothing."
  (let ((json (ignore-errors (read-record (private-file "ready" id)))))
    (and (hash-table-p json) json)))

(defun private-live-request (wallet)
  "The request of WALLET's that is not over yet (no answer, or up), or nil."
  (find-if (lambda (r)
             (and (equal (gethash "wallet" r) wallet)
                  (let ((a (private-answer (gethash "id" r))))
                    (or (null a) (equal (gethash "state" a) "up")))))
           (private-requests)))

(defun private-standing (wallet)
  "Values: true when the gate's standing for WALLET opens private sessions,
else nil and the reason in words."
  (let ((answer (ignore-errors (gate-post "author" (h "wallet" wallet)))))
    (cond ((not (hash-table-p answer))
           (values nil "The gate did not answer; try again in a moment."))
          ((not (eq (gethash "known" answer) t))
           (values nil "That is not an author key this lab knows."))
          ((equal (gethash "standing" answer) "frozen")
           (values nil "This author key is frozen."))
          ;; the third value: the gate's answer, whose "monetize" says the rung above
          ((eq (gethash "private" answer) t) (values t nil answer))
          (t (values nil (format nil "Your author key is at the ~a rung.  Private labs open at the trusted rung, granted on review of the work you make in the public lab."
                                 (gethash "standing" answer)))))))

(defun write-private-request! (id wallet token &key monetize standing)
  "File request ID in the spool.  MONETIZE t (the author's standing is the
monetizing rung) has the hangar open the session with Monetize; :FREE (a
trusted author, the user's ruling 50) with deploys of open work that
charges nothing only.  Either only where this front keeps a private
deployed root.  STANDING is the author's standing as the gate gave it
here; the hangar hands it to the session's VM with the key (Ruling 449),
since the gate answers no session machine."
  (let ((final (private-file "requests" id))
        (tmp (format nil "~a/requests/.~a.json" (string-right-trim "/" *private-spool*) id)))
    (with-open-file (out tmp :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (encode (h "id" id "wallet" wallet "token" token "created" (get-universal-time)
                               "standing" (if (member standing '("trusted" "monetizing") :test #'equal) standing nil)
                               "monetize" (cond ((not *private-deployed-root*) 'yason:false)
                                                 ((eq monetize t) t)
                                                 ((eq monetize :free) "free")
                                                 (t 'yason:false))))
                    out))
    (rename-file tmp final)
    final))

(defun private-session-url (id)
  (format nil "~a://~a.~a~a" *private-session-scheme* id *private-session-domain* *private-session-prefix*))

(defun private-open-door (req ent)
  "POST <prefix>/api/open {wallet}: the author's private session -- the one
open already, or a new request to the hangar once the gate's standing says
yes.  Answers {id, token, unlock, next} or {error}."
  (let* ((json (request-json req))
         (wallet (and json (gethash "wallet" json)))
         (address (client-address req)))
    (cond ((null *private-spool*) (refuse req ent net.aserve:*response-not-found* "No private labs here."))
          ((not (wallet-id? wallet)) (refuse req ent "Paste your author key: 24 letters and digits."))
          ((address-over-limit? address :private-opens)
           (refuse req ent net.aserve:*response-forbidden* "That is enough requests from this address for today."))
          (t
           (multiple-value-bind (ok? reason answer) (private-standing wallet)
             (if (not ok?)
                 (refuse req ent net.aserve:*response-forbidden* "~a" reason)
                 (let* ((live (private-live-request wallet))
                        (id (if live (gethash "id" live) (format nil "pl-~a" (subseq (new-owner-key) 0 10))))
                        (token (if live (gethash "token" live) (new-owner-key))))
                   (unless live
                     (count-address! address :private-opens)
                     ;; a monetizing author's session sells; a trusted author's may
                     ;; deploy open work that charges nothing (ruling 50)
                     (write-private-request! id wallet token :monetize (if (eq (gethash "monetize" answer) t) t :free)
                                             :standing (gethash "standing" answer)))
                   (respond-json req ent (h "id" id "token" token
                                            "unlock" (format nil "~a/api/unlock" (private-session-url id))
                                            "next" (format nil "~a?wallet=~a" *private-session-prefix* wallet))))))))))

(defun private-opened-door (req ent)
  "GET <prefix>/api/opened?id=<id>&wallet=<wallet>: the hangar's word on the
request -- {state waiting|up|failed|ended, host, reason}."
  (let* ((id (query-value req "id"))
         (wallet (query-value req "wallet"))
         (request (and (private-id? id) (ignore-errors (read-record (private-file "requests" id)))))
         (answer (and (private-id? id) (private-answer id))))
    (cond ((null *private-spool*) (refuse req ent net.aserve:*response-not-found* "No private labs here."))
          ((not (private-id? id)) (refuse req ent "No such session."))
          ;; the request's own author only (an ended one has no request left: its answer is harmless)
          ((and (hash-table-p request) (not (equal (gethash "wallet" request) wallet)))
           (refuse req ent net.aserve:*response-not-found* "No such session."))
          (answer (respond-json req ent answer))
          ((hash-table-p request) (respond-json req ent (h "id" id "state" "waiting")))
          (t (refuse req ent net.aserve:*response-not-found* "No such session.")))))

(defun private-close-door (req ent)
  "POST <prefix>/api/close {id, wallet}: withdraw the request; the hangar ends
the session on its next pass (the author's disk stays)."
  (let* ((json (request-json req))
         (id (and json (gethash "id" json)))
         (wallet (and json (gethash "wallet" json)))
         (request (and (private-id? id) (ignore-errors (read-record (private-file "requests" id))))))
    (cond ((null *private-spool*) (refuse req ent net.aserve:*response-not-found* "No private labs here."))
          ((not (and (hash-table-p request) (equal (gethash "wallet" request) wallet)))
           (refuse req ent net.aserve:*response-not-found* "No such session."))
          (t (ignore-errors (delete-file (private-file "requests" id)))
             (respond-json req ent (h "ok" t))))))

;;
;; The session doors: a monetizing author's deployments.
;;

(defmacro with-private-house (&body body)
  "BODY with the lab's house standing for the private deployed root: its
deployments and books there, under the private lab's name, tolls taken by
card through the gate."
  `(let ((*deployed-root* *private-deployed-root*)
         (*lab-name* *private-lab-name*)
         (*toll-provider* :stripe)
         (*deployment-payments?* t)
         (*books?* t)
         (*hosting-meter?* t)
         (*deploy-front* nil))
     ,@body))

(defun private-session-request (json)
  "The spool request of the LIVE private session that JSON names by its id
and token (the hangar has said it is up), or nil."
  (let* ((id (and json (gethash "id" json)))
         (token (and json (gethash "token" json)))
         (request (and (private-id? id) (stringp token) (= (length token) 32)
                       (ignore-errors (read-record (private-file "requests" id)))))
         (answer (and (hash-table-p request) (private-answer id))))
    (and answer (equal (gethash "state" answer) "up")
         (equal (gethash "token" request) token)
         request)))

(defun private-deployment-url (name)
  (format nil "~a://~a.~a/" *private-session-scheme* name *private-session-domain*))

(defun private-author-deployments (wallet)
  "WALLET's deployments in the private root (hash tables, the newest first)."
  (remove-if-not #'(lambda (record) (equal (gethash "author" record) wallet)) (deployment-records)))

(defun private-deployment-row (record)
  "RECORD as the author's own session reads it: the whole record and its
address, less the deployment's key at the gate (the machine serving it
holds that, never the session that made it)."
  (let ((row (alexandria:copy-hash-table record)))
    (remhash "gate_key" row)
    (setf (gethash "url" row) (private-deployment-url (gethash "name" record)))
    ;; its command-line token, when one is kept: when made and last used
    (setf (gethash "cli_token" row) (ignore-errors (cli-token-state (gethash "name" record) (gethash "author" record))))
    row))

(defun private-feed-names ()
  "The names the hangar flies from a lab's feed (spool/feed-names.json, which
it writes every pass): taken, since every deployment's host is <name>.<domain>."
  (let ((names (ignore-errors (yason:parse (uiop:read-file-string
                                            (format nil "~a/feed-names.json" (string-right-trim "/" *private-spool*)))))))
    ;; yason reads a JSON array as a list unless told otherwise
    (remove-if-not #'stringp (cond ((listp names) names)
                                   ((vectorp names) (coerce names 'list))))))

(defun private-deploys? ()
  "Whether this front takes deployments from private sessions."
  (and *private-spool* *private-deployed-root* t))

(defun private-vm-deploy-door (req ent)
  "POST <prefix>/api/vm/deploy: a monetizing author's web app, from their
private session, into the private deployed root.  Answers {deployment} or
{error}."
  (let* ((json (request-json req))
         (request (private-session-request json))
         (wallet (and request (gethash "wallet" request))))
    (cond
          ((not (private-deploys?)) (refuse req ent net.aserve:*response-not-found* "No private deployments here."))
          ((null request) (refuse req ent net.aserve:*response-forbidden* "No such session."))
          ;; the clickwrap: the texts the author ticked in their session, sent
          ;; with the deployment, logged here where the books are kept
          ((and (terms-required? :deploy)
                (not (plusp (length (gethash "accepted_terms" json)))))
           (refuse req ent net.aserve:*response-forbidden* "~a" (terms-refusal :deploy)))
          ;; a monetizing author deploys anything; a trusted one, open work
          ;; that charges nothing, hosted free (the user's ruling 50)
          ((let ((standing (or (ignore-errors (gate-post "author" (h "wallet" wallet))) (h))))
             (not (or (eq (gethash "monetize" standing) t)
                      (and (eq (gethash "private" standing) t)
                           (not (eq (gethash "closed" json) t))
                           (eq (gethash "free" json) t)))))
           (refuse req ent net.aserve:*response-forbidden*
                   "From a trusted author's private lab, open work that charges nothing deploys free; deploying what charges its users opens at the monetizing rung, with payouts set up, on review."))
          (t
           (with-private-house
             (bt:with-lock-held (*deploy-lock*)
               (let* ((name (let ((name (gethash "name" json))) (and (stringp name) (string-downcase (string-trim " " name)))))
                      (existing (and (deployment-name? name) (deployment-record name)))
                      (session-id (gethash "session" json))
                      (mine (find session-id (private-author-deployments wallet)
                                  :key #'(lambda (record) (gethash "session" record)) :test #'equal))
                      (model (gethash "model" json))
                      ;; the session's uploaded files, which the app reads (own-file):
                      ;; (files . why-not)
                      (files (multiple-value-list (decode-deploy-files (gethash "files" json))))
                      (payee (clip-line (gethash "payee" json) 200))
                      (closed? (eq (gethash "closed" json) t))
                      ;; open and charging nothing: hosted free at the house's
                      ;; discretion (rulings 44, 50); a closed one always charges
                      (free? (and (not closed?) (eq (gethash "free" json) t))))
                 (multiple-value-bind (account url ready? why) (and (not free?) (find #\@ payee) (author-account payee))
                   (declare (ignore url))
                   (cond
                     ((not (equal (gethash "kind" json) "app"))
                      (refuse req ent "From a private lab, web apps deploy for now."))
                     ((not (deployment-name? name))
                      (refuse req ent "Give it a name for its address: 3 to 40 lower-case letters, digits and hyphens, a letter first."))
                     ((or (member name *reserved-deployment-names* :test #'string=)
                          (string= "pl-" name :end2 3))
                      (refuse req ent "The name ~a is reserved; choose another." name))
                     ((or (member name (private-feed-names) :test #'string=)
                          (and existing (not (equal (gethash "author" existing) wallet))))
                      (refuse req ent "The name ~a is taken; choose another." name))
                     ((and mine (not (equal (gethash "name" mine) name)))
                      (refuse req ent "This session is deployed as ~a: deploy under that name again, or take it down first."
                              (gethash "name" mine)))
                     ((and (null existing) (>= (length (deployment-records)) *max-deployments*))
                      (refuse req ent "The private labs hold all the deployments they take."))
                     ((not (and (stringp model) (plusp (length model)) (< (length model) 1000000)))
                      (refuse req ent "There is no model file to deploy."))
                     ((second files) (refuse req ent "~a" (second files)))
                     ((and (not free?) (not (and (find #\@ payee) (> (length payee) 5))))
                      (refuse req ent "Say where your share is to be paid: an email address we can reach you at."))
                     ((and (not free?) (not (and account ready?)))
                      (refuse req ent "~a" (or why "Set up payouts with Stripe first: the button beside your email.")))
                     (t
                      (let ((record (monocle:make-record
                                     (lab-house)
                                     :name name :title (gethash "title" json) :blurb (gethash "blurb" json)
                                     :owner (let ((owner (gethash "owner" json))) (and (stringp owner) (clip-line owner 64)))
                                     :payee payee :closed? closed? :existing existing
                                     :pot-percent (let ((pot (gethash "pot_percent" json))) (and (realp pot) pot))
                                     :fields (list "session" (and (stringp session-id) (clip-line session-id 64))
                                                   "kind" "app"
                                                   "author" wallet
                                                   "private" t
                                                   "connect_account" account
                                                   "free" (if free? t 'yason:false)
                                                   "balance" (if (eq (gethash "balance" json) t) t 'yason:false)))))
                        (ensure-directories-exist (deployment-directory name))
                        (write-text-file model (merge-pathnames "model.lisp" (deployment-directory name)))
                        ;; the files, unless the deploy asks the deployment to keep its own
                        (unless (eq (gethash "keep_files" json) t)
                          (write-deploy-files! (first files) (merge-pathnames "files/" (deployment-directory name))))
                        (monocle:save-record! (lab-house) record)
                        ;; the terms just written, at the gate: the VM that will serve it cannot steer them
                        (register-deployment! name)
                        (let ((accepted (coerce (gethash "accepted_terms" json) 'list)))
                          (when (every #'hash-table-p accepted)
                            (record-acceptance! :deploy accepted
                                                :session (and (stringp session-id) (clip-line session-id 64))
                                                :wallet wallet :owner (gethash "owner" json)
                                                :address (client-address req) :via "private front")))
                        (respond-json req ent (h "deployment" (private-deployment-row (deployment-record name)))))))))))))))

(defun private-vm-undeploy-door (req ent)
  "POST <prefix>/api/vm/undeploy {id, token, name}: the author's deployment
taken down; the books keep what it earned, and the hangar retires its disk."
  (let* ((json (request-json req))
         (request (private-session-request json))
         (wallet (and request (gethash "wallet" request)))
         (name (and json (gethash "name" json))))
    (cond
      ((not (private-deploys?)) (refuse req ent net.aserve:*response-not-found* "No private deployments here."))
      ((null request) (refuse req ent net.aserve:*response-forbidden* "No such session."))
      (t
       (with-private-house
         (bt:with-lock-held (*deploy-lock*)
           (let ((record (and (deployment-name? name) (deployment-record name))))
             (if (not (and record (equal (gethash "author" record) wallet)))
                 (refuse req ent net.aserve:*response-not-found* "There is no such deployment of yours.")
                 (progn (monocle:delete-deployment! (lab-house) name)
                        (deregister-deployment! name)
                        ;; its command-line tokens go with it
                        (ignore-errors (drop-cli-tokens! name wallet))
                        (respond-json req ent (h "ok" t)))))))))))

(defun private-vm-deployments-door (req ent)
  "POST <prefix>/api/vm/deployments {id, token}: the author's deployments."
  (let* ((json (request-json req))
         (request (private-session-request json)))
    (cond
      ((not (private-deploys?)) (refuse req ent net.aserve:*response-not-found* "No private deployments here."))
      ((null request) (refuse req ent net.aserve:*response-forbidden* "No such session."))
      (t (with-private-house
           (respond-json req ent (h "deployments" (coerce (mapcar #'private-deployment-row
                                                                  (private-author-deployments (gethash "wallet" request)))
                                                          'vector))))))))

(defun private-vm-connect-door (req ent)
  "POST <prefix>/api/vm/connect {id, token, email, create?, return_url?,
refresh_url?}: the author of a live monetizing session sets up payouts.
The gate's connect door answers its registrar alone -- a session's machine
runs its author's code, and could otherwise look up any address's account
or ask its onboarding link -- so the front asks it and relays the answer
and its status."
  (let* ((json (request-json req))
         (request (private-session-request json))
         (wallet (and request (gethash "wallet" request))))
    (cond
      ((not (private-deploys?)) (refuse req ent net.aserve:*response-not-found* "No private deployments here."))
      ((null request) (refuse req ent net.aserve:*response-forbidden* "No such session."))
      ((not (eq (gethash "monetize" (or (ignore-errors (gate-post "author" (h "wallet" wallet))) (h))) t))
       (refuse req ent net.aserve:*response-forbidden*
               "Payouts are set up from a private lab at the monetizing rung."))
      (t
       (flet ((field (name) (let ((value (gethash name json))) (if (stringp value) value ""))))
         (multiple-value-bind (answer status)
             (gate-post "connect" (h "email" (field "email")
                                     "create" (if (eq (gethash "create" json) t) t 'yason:false)
                                     "return_url" (field "return_url")
                                     "refresh_url" (field "refresh_url")
                                     "lab" *private-lab-name*))
           (respond-json req ent (or answer (h "error" (h "message" "The gate did not answer about payouts.")))
                         (case status
                           (200 net.aserve:*response-ok*)
                           (400 net.aserve:*response-bad-request*)
                           (403 net.aserve:*response-forbidden*)
                           (404 net.aserve:*response-not-found*)
                           (t net.aserve:*response-service-unavailable*)))))))))

;;
;; COMMAND-LINE TOKENS.  A token lets a program on the author's own machine
;; (prompt-lab/cli/pl-deploy) save a model file and deploy it as ONE named
;; deployment of the author's, through the author's live private session --
;; and do nothing else: it does not unlock the session, read its work, deploy
;; another name, take anything down or touch payouts.  The session's sheet
;; makes one (vm/cli-token, under the session's own credentials), shows it
;; once, and may revoke it; taking the deployment down drops it.  Only its
;; SHA-256 is kept here, one line each, in the spool beside the requests
;; (*private-spool*/cli-tokens.json), so this room adds no file or directory
;; of its own to the host.
;;
;;   <prefix>/api/vm/cli-token    POST {id, token, name, action}: make | revoke
;;   <prefix>/api/vm/cli-check    POST {id, token, cli_token, name}: may it?
;;   <prefix>/api/cli/where       GET, Authorization: Bearer: the author's
;;                                live session's address
;;   <prefix>/api/cli/open        POST, Bearer: the same, opening one first
;;

(defparameter *cli-token-prefix* "pltok_"
  "String.  What every command-line token begins with, so a person can tell
one from the other keys at a glance.")

(defvar *cli-tokens-lock* (bt:make-lock "prompt-lab cli tokens"))

(defun cli-tokens-file ()
  (and *private-spool* (format nil "~a/cli-tokens.json" (string-right-trim "/" *private-spool*))))

(defun cli-tokens ()
  "The kept tokens: hash tables {hash, name, author, made, used}."
  (let* ((file (cli-tokens-file))
         (json (and file (probe-file file)
                    (ignore-errors (with-open-file (in file :external-format :utf-8) (yason:parse in))))))
    ;; yason reads a JSON array as a list unless told otherwise
    (remove-if-not #'hash-table-p (cond ((listp json) json) ((vectorp json) (coerce json 'list))))))

(defun save-cli-tokens! (tokens)
  "Write TOKENS whole, to a file beside and then in place of the old one."
  (let* ((file (cli-tokens-file))
         (tmp (format nil "~a.new" file)))
    (with-open-file (out tmp :direction :output :if-exists :supersede :if-does-not-exist :create
                             :external-format :utf-8)
      (write-string (encode (coerce tokens 'vector)) out))
    (uiop:rename-file-overwriting-target tmp file)
    tokens))

(defun cli-token-hash (token)
  (and (stringp token) (watch-sha256-hex token)))

(defun cli-token-entry (token)
  "The kept entry of TOKEN, or nil."
  (let ((hash (cli-token-hash token)))
    (and hash (find hash (cli-tokens) :key #'(lambda (e) (gethash "hash" e)) :test #'equal))))

(defun make-cli-token! (name author)
  "A new token for deployment NAME of AUTHOR, in place of any it had.  Value:
the token itself, which nothing keeps."
  (let ((token (format nil "~a~a" *cli-token-prefix* (new-owner-key))))
    (bt:with-lock-held (*cli-tokens-lock*)
      (save-cli-tokens! (cons (h "hash" (cli-token-hash token) "name" name "author" author
                                 "made" (get-universal-time) "used" nil)
                              (remove-if #'(lambda (e) (and (equal (gethash "name" e) name)
                                                            (equal (gethash "author" e) author)))
                                         (cli-tokens)))))
    token))

(defun drop-cli-tokens! (name &optional author)
  "Forget every token for deployment NAME (of AUTHOR, when given)."
  (bt:with-lock-held (*cli-tokens-lock*)
    (let ((tokens (cli-tokens)))
      (when (find-if #'(lambda (e) (equal (gethash "name" e) name)) tokens)
        (save-cli-tokens! (remove-if #'(lambda (e) (and (equal (gethash "name" e) name)
                                                        (or (null author) (equal (gethash "author" e) author))))
                                     tokens))))))

(defun note-cli-token-used! (hash)
  (bt:with-lock-held (*cli-tokens-lock*)
    (let ((tokens (cli-tokens)))
      (dolist (e tokens) (when (equal (gethash "hash" e) hash) (setf (gethash "used" e) (get-universal-time))))
      (save-cli-tokens! tokens))))

(defun cli-token-state (name author)
  "What the sheet shows of deployment NAME's token: {made, used}, or nil."
  (let ((e (find-if #'(lambda (e) (and (equal (gethash "name" e) name) (equal (gethash "author" e) author)))
                    (cli-tokens))))
    (and e (h "made" (gethash "made" e) "used" (gethash "used" e)))))

(defun request-bearer (req)
  "The token of an Authorization: Bearer header, never one in the address."
  (let ((auth (net.aserve:header-slot-value req :authorization)))
    (and (stringp auth) (> (length auth) 7) (string-equal "Bearer " auth :end2 7)
         (let ((token (string-trim " " (subseq auth 7)))) (and (plusp (length token)) token)))))

(defun cli-refusals-over? (req)
  "True when this address has been refused tokens too often today."
  (address-over-limit? (client-address req) :cli-refusals))

(defun refuse-cli-token (req ent)
  (count-address! (client-address req) :cli-refusals)
  (refuse req ent net.aserve:*response-forbidden*
          "That token is not valid here: it may have been revoked, or its deployment taken down.  Make a new one on the sheet."))

(defun private-vm-cli-token-door (req ent)
  "POST <prefix>/api/vm/cli-token {id, token, name, action}: the live
session's author makes (action make) or revokes (action revoke) the
command-line token of their deployment NAME."
  (let* ((json (request-json req))
         (request (private-session-request json))
         (wallet (and request (gethash "wallet" request)))
         (name (and json (gethash "name" json)))
         (action (and json (gethash "action" json))))
    (cond
      ((not (private-deploys?)) (refuse req ent net.aserve:*response-not-found* "No private deployments here."))
      ((null request) (refuse req ent net.aserve:*response-forbidden* "No such session."))
      ((not (and (deployment-name? name)
                 (with-private-house
                   (let ((record (deployment-record name)))
                     (and record (equal (gethash "author" record) wallet))))))
       (refuse req ent net.aserve:*response-not-found* "There is no deployment of yours by that name."))
      ((equal action "make")
       (respond-json req ent (h "token" (make-cli-token! name wallet) "name" name)))
      ((equal action "revoke")
       (drop-cli-tokens! name wallet)
       (respond-json req ent (h "ok" t "name" name)))
      (t (refuse req ent "Say make or revoke.")))))

(defun private-vm-cli-check-door (req ent)
  "POST <prefix>/api/vm/cli-check {id, token, cli_token, name}: whether
CLI_TOKEN may deploy NAME from this live session -- its author's token for
that name, and the deployment still the author's."
  (let* ((json (request-json req))
         (request (private-session-request json))
         (wallet (and request (gethash "wallet" request)))
         (name (and json (gethash "name" json)))
         (entry (and json (cli-token-entry (gethash "cli_token" json)))))
    (cond
      ((not (private-deploys?)) (refuse req ent net.aserve:*response-not-found* "No private deployments here."))
      ((null request) (refuse req ent net.aserve:*response-forbidden* "No such session."))
      ((not (and entry (equal (gethash "name" entry) name) (equal (gethash "author" entry) wallet)
                 (with-private-house
                   (let ((record (deployment-record name)))
                     (and record (equal (gethash "author" record) wallet))))))
       (refuse req ent net.aserve:*response-forbidden*
               "That token is not valid for ~a: it may have been revoked, or the deployment taken down.  Make a new one on the sheet." name))
      (t (note-cli-token-used! (gethash "hash" entry))
         (respond-json req ent (h "ok" t "name" name))))))

(defun cli-session-answer (request)
  "What the command line is told of a private session REQUEST: its address
and state, never its token."
  (let* ((id (gethash "id" request))
         (answer (private-answer id)))
    (h "id" id
       "url" (private-session-url id)
       "state" (if answer (gethash "state" answer) "waiting"))))

(defun private-cli-where-door (req ent &key open?)
  "GET <prefix>/api/cli/where (Bearer): the live private session of the
token's author.  POST <prefix>/api/cli/open: the same, a new one filed when
there is none and the author may open one."
  (let ((entry (and (not (cli-refusals-over? req)) (cli-token-entry (request-bearer req)))))
    (cond
      ((not (private-deploys?)) (refuse req ent net.aserve:*response-not-found* "No private deployments here."))
      ((null entry) (refuse-cli-token req ent))
      (t
       (let* ((wallet (gethash "author" entry))
              (live (private-live-request wallet)))
         (cond
           (live (respond-json req ent (cli-session-answer live)))
           ((not open?)
            (refuse req ent net.aserve:*response-not-found*
                    "No private session of yours is open.  Open one on the lab's page, or ask again with --open."))
           ((address-over-limit? (client-address req) :private-opens)
            (refuse req ent net.aserve:*response-forbidden* "That is enough requests from this address for today."))
           (t
            (multiple-value-bind (ok? reason answer) (private-standing wallet)
              (if (not ok?)
                  (refuse req ent net.aserve:*response-forbidden* "~a" reason)
                  (let ((id (format nil "pl-~a" (subseq (new-owner-key) 0 10))))
                    (count-address! (client-address req) :private-opens)
                    (write-private-request! id wallet (new-owner-key)
                                            :monetize (if (eq (gethash "monetize" answer) t) t :free)
                                            :standing (gethash "standing" answer))
                    (respond-json req ent (h "id" id "url" (private-session-url id) "state" "waiting"))))))))))))

(defvar *private-sweeper* nil "The thread that keeps the private root's books.")

(defun private-sweep! ()
  "Book what the gate settled for the private lab's deployments -- tolls,
and hosting bought -- that the books lack: the VMs that serve them book
nothing (*books?* nil there)."
  (with-private-house
    (ignore-errors (sweep-toll-payments!))
    (ignore-errors (sweep-hosting!))
    ;; closed deployments' monthly minimums, once a month (deploy.lisp)
    (ignore-errors (reckon-closed-minimums!))
    (ignore-errors (sweep-unpaid-dues!))
    ;; the authors paid their month, from $25 (deploy.lisp)
    (ignore-errors (ask-gate-payouts!))
    ;; and the root's terms at the gate: at the front's start, then every *register-sweep-seconds*
    (ignore-errors (register-deployments!))))

(defun start-private-sweeper! ()
  (unless (and *private-sweeper* (bt:thread-alive-p *private-sweeper*))
    (setq *private-sweeper*
          (bt:make-thread #'(lambda ()
                              (do () (nil)
                                (when *private-deployed-root* (private-sweep!))
                                (sleep *private-sweep-seconds*)))
                          :name "prompt-lab private books"))))

(defun private-page (req ent)
  "The front's page: an author key in, a private lab out."
  (net.aserve:with-http-response (req ent :content-type "text/html; charset=utf-8")
    (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
    (net.aserve:with-http-body (req ent)
      (format net.html.generator:*html-stream* "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><meta name=\"robots\" content=\"noindex\">
<title>A private lab</title>
<style>
:root{--bg:#f4f4f2;--card:#fff;--ink:#222;--dim:#555;--accent:#2a5db0;--bad:#b02a2a}
@media (prefers-color-scheme: dark){:root{--bg:#16181c;--card:#202329;--ink:#e6e6e6;--dim:#a8a8a8;--accent:#6f9be8;--bad:#e07070}}
body{font-family:system-ui,sans-serif;background:var(--bg);color:var(--ink);display:flex;min-height:100vh;margin:0;align-items:center;justify-content:center}
main{background:var(--card);padding:2rem 2.5rem;border-radius:12px;box-shadow:0 2px 12px rgba(0,0,0,.12);max-width:30rem;width:90vw}
h1{font-size:1.3rem;margin:0 0 .6rem}p{margin:.5rem 0;color:var(--dim);line-height:1.45}
input{width:100%;box-sizing:border-box;font:inherit;font-size:1rem;padding:.55rem .7rem;border:1px solid #999;border-radius:8px;background:transparent;color:inherit}
button{margin-top:1rem;width:100%;font:inherit;font-size:1rem;padding:.6rem;border:0;border-radius:8px;background:var(--accent);color:#fff;cursor:pointer}
button:disabled{opacity:.5;cursor:default}#say{min-height:1.4em;color:var(--ink)}#say.bad{color:var(--bad)}
a{color:var(--accent)}
</style></head><body><main>
<h1>A private lab</h1>
<p>Your own prompt lab, in a machine of its own: the work you make here is yours alone, kept from one session to the next.  It opens for authors whose public work has earned it.</p>
<label for=\"key\"><p>Your author key</p></label>
<input id=\"key\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"24 letters and digits\">
<button id=\"go\" type=\"button\">Open my private lab</button>
<p id=\"say\" role=\"status\"></p>
<form id=\"enter\" method=\"post\" style=\"display:none\"><input type=\"hidden\" name=\"password\"><input type=\"hidden\" name=\"next\"></form>
<script>
(function(){
  var api = ~s, key = document.getElementById('key'), go = document.getElementById('go'), say = document.getElementById('say');
  var q = new URLSearchParams(location.search).get('wallet');
  try { key.value = q || localStorage.getItem('prompt-lab-wallet') || ''; if (q) localStorage.setItem('prompt-lab-wallet', q); } catch (e) { key.value = q || ''; }
  if (q) history.replaceState(null, '', location.pathname);
  function tell(text, bad) { say.textContent = text; say.className = bad ? 'bad' : ''; }
  function enter(open) {
    var f = document.getElementById('enter');
    f.action = open.unlock; f.elements.password.value = open.token; f.elements.next.value = open.next; f.submit();
  }
  function wait(open, wallet, tries) {
    fetch(api + '/opened?id=' + encodeURIComponent(open.id) + '&wallet=' + encodeURIComponent(wallet))
      .then(function (r) { return r.json(); })
      .then(function (a) {
        if (a.state === 'up') { tell('Your lab is up.  Going in.'); enter(open); }
        else if (a.state === 'failed' || a.state === 'ended') { tell(a.reason || a.error || 'It did not open.', true); go.disabled = false; }
        else if (tries > 90) { tell('It is taking longer than it should.  Try again in a minute.', true); go.disabled = false; }
        else { tell(a.state === 'waiting' ? 'Preparing your lab (a few seconds; longer when the hangar is busy)...' : (a.error || '...')); setTimeout(function () { wait(open, wallet, tries + 1); }, 2000); }
      })
      .catch(function () { setTimeout(function () { wait(open, wallet, tries + 1); }, 3000); });
  }
  go.onclick = function () {
    var wallet = key.value.trim().toLowerCase();
    go.disabled = true; tell('Asking...');
    try { localStorage.setItem('prompt-lab-wallet', wallet); } catch (e) {}
    fetch(api + '/open', {method: 'POST', headers: {'Content-Type': 'application/json'}, body: JSON.stringify({wallet: wallet})})
      .then(function (r) { return r.json(); })
      .then(function (open) { if (open.error) { tell(open.error, true); go.disabled = false; } else wait(open, wallet, 0); })
      .catch(function () { tell('The front did not answer; try again.', true); go.disabled = false; });
  };
})();
</script></main></body></html>"
              (format nil "~a/api" *private-prefix*)))))

(defun publish-private-front! (&key host)
  "Publish the private front at *private-prefix* on every server (HOST, when
given, for that host alone).  Needs *private-spool* and the gate (*messages-url*)."
  (gwl:with-all-servers (server)
    (net.aserve:publish :path *private-prefix* :server server :host host :function #'private-page)
    (net.aserve:publish :path (format nil "~a/" *private-prefix*) :server server :host host :function #'private-page)
    (net.aserve:publish :path (format nil "~a/api/open" *private-prefix*) :server server :host host :function #'private-open-door)
    (net.aserve:publish :path (format nil "~a/api/opened" *private-prefix*) :server server :host host :function #'private-opened-door)
    (net.aserve:publish :path (format nil "~a/api/close" *private-prefix*) :server server :host host :function #'private-close-door)
    ;; the session doors: not host-scoped, since a session's VM reaches them
    ;; through the ship's proxy at whatever address it has for it
    (net.aserve:publish :path (format nil "~a/api/vm/deploy" *private-prefix*) :server server :function #'private-vm-deploy-door)
    (net.aserve:publish :path (format nil "~a/api/vm/undeploy" *private-prefix*) :server server :function #'private-vm-undeploy-door)
    (net.aserve:publish :path (format nil "~a/api/vm/deployments" *private-prefix*) :server server :function #'private-vm-deployments-door)
    (net.aserve:publish :path (format nil "~a/api/vm/connect" *private-prefix*) :server server :function #'private-vm-connect-door)
    ;; command-line tokens: made and checked by a session, used from the author's machine
    (net.aserve:publish :path (format nil "~a/api/vm/cli-token" *private-prefix*) :server server :function #'private-vm-cli-token-door)
    (net.aserve:publish :path (format nil "~a/api/vm/cli-check" *private-prefix*) :server server :function #'private-vm-cli-check-door)
    (net.aserve:publish :path (format nil "~a/api/cli/where" *private-prefix*) :server server :host host
                        :function #'(lambda (req ent) (private-cli-where-door req ent)))
    (net.aserve:publish :path (format nil "~a/api/cli/open" *private-prefix*) :server server :host host
                        :function #'(lambda (req ent) (private-cli-where-door req ent :open? t)))
    ;; the house's withdrawal of a private lab's free deployment (deploy.lisp),
    ;; behind the house's token (*house-token-file*)
    (net.aserve:publish :path (format nil "~a/api/house/withdraw" *private-prefix*) :server server :host host
                        :function #'(lambda (req ent) (with-private-house (house-withdraw-door req ent))))
    (net.aserve:publish :path (format nil "~a/api/house/minimum" *private-prefix*) :server server :host host
                        :function #'(lambda (req ent) (with-private-house (house-minimum-door req ent)))))
  (when *private-deployed-root* (start-private-sweeper!))
  *private-prefix*)
