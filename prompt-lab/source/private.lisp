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

(defun write-private-request! (id wallet token &key monetize?)
  "File request ID in the spool.  MONETIZE? (the author's standing is the
monetizing rung, and this front keeps a private deployed root) has the
hangar open the session with Monetize."
  (let ((final (private-file "requests" id))
        (tmp (format nil "~a/requests/.~a.json" (string-right-trim "/" *private-spool*) id)))
    (with-open-file (out tmp :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (encode (h "id" id "wallet" wallet "token" token "created" (get-universal-time)
                               "monetize" (if (and monetize? *private-deployed-root*) t 'yason:false)))
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
                     (write-private-request! id wallet token :monetize? (eq (gethash "monetize" answer) t)))
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
  "RECORD as the author's own session reads it: the whole record and its address."
  (let ((row (alexandria:copy-hash-table record)))
    (setf (gethash "url" row) (private-deployment-url (gethash "name" record)))
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
          ((not (eq (gethash "monetize" (or (ignore-errors (gate-post "author" (h "wallet" wallet))) (h))) t))
           (refuse req ent net.aserve:*response-forbidden*
                   "Deploying from a private lab opens at the monetizing rung: a trusted author with payouts set up, on review."))
          (t
           (with-private-house
             (bt:with-lock-held (*deploy-lock*)
               (let* ((name (let ((name (gethash "name" json))) (and (stringp name) (string-downcase (string-trim " " name)))))
                      (existing (and (deployment-name? name) (deployment-record name)))
                      (session-id (gethash "session" json))
                      (mine (find session-id (private-author-deployments wallet)
                                  :key #'(lambda (record) (gethash "session" record)) :test #'equal))
                      (model (gethash "model" json))
                      (payee (clip-line (gethash "payee" json) 200))
                      (closed? (eq (gethash "closed" json) t)))
                 (multiple-value-bind (account url ready? why) (and (find #\@ payee) (author-account payee))
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
                     ((not (and (find #\@ payee) (> (length payee) 5)))
                      (refuse req ent "Say where your share is to be paid: an email address we can reach you at."))
                     ((not (and account ready?))
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
                                                   "balance" (if (eq (gethash "balance" json) t) t 'yason:false)))))
                        (ensure-directories-exist (deployment-directory name))
                        (write-text-file model (merge-pathnames "model.lisp" (deployment-directory name)))
                        (monocle:save-record! (lab-house) record)
                        ;; the terms just written, at the gate: the VM that will serve it cannot steer them
                        (register-deployment! name)
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

(defvar *private-sweeper* nil "The thread that keeps the private root's books.")

(defun private-sweep! ()
  "Book what the gate settled for the private lab's deployments -- tolls,
and hosting bought -- that the books lack: the VMs that serve them book
nothing (*books?* nil there)."
  (with-private-house
    (ignore-errors (sweep-toll-payments!))
    (ignore-errors (sweep-hosting!))
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
    (net.aserve:publish :path (format nil "~a/api/vm/deployments" *private-prefix*) :server server :function #'private-vm-deployments-door))
  (when *private-deployed-root* (start-private-sweeper!))
  *private-prefix*)
