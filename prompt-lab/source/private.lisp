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
          ((eq (gethash "private" answer) t) t)
          (t (values nil (format nil "Your author key is at the ~a rung.  Private labs open at the trusted rung, granted on review of the work you make in the public lab."
                                 (gethash "standing" answer)))))))

(defun write-private-request! (id wallet token)
  (let ((final (private-file "requests" id))
        (tmp (format nil "~a/requests/.~a.json" (string-right-trim "/" *private-spool*) id)))
    (with-open-file (out tmp :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (encode (h "id" id "wallet" wallet "token" token "created" (get-universal-time))) out))
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
           (multiple-value-bind (ok? reason) (private-standing wallet)
             (if (not ok?)
                 (refuse req ent net.aserve:*response-forbidden* "~a" reason)
                 (let* ((live (private-live-request wallet))
                        (id (if live (gethash "id" live) (format nil "pl-~a" (subseq (new-owner-key) 0 10))))
                        (token (if live (gethash "token" live) (new-owner-key))))
                   (unless live
                     (count-address! address :private-opens)
                     (write-private-request! id wallet token))
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
    (net.aserve:publish :path (format nil "~a/api/close" *private-prefix*) :server server :host host :function #'private-close-door))
  *private-prefix*)
