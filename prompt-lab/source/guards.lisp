;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The guards on the doors: who is asking (the visitor's address as it
;; reaches this room behind the proxies), how often one address may open
;; sessions and run prompts in a day, and Cloudflare Turnstile on the
;; prompt door.  None of this is about the visitor at the console next
;; door, who can call the agent directly; the budgets on the gate are
;; the cap there.  This is about the anonymous spray at the HTTP doors,
;; which is where the money would otherwise go.
;;


;;
;; Who is asking.
;;

(defun client-address (req)
  "The visitor's address: CF-Connecting-IP when a Cloudflare edge fronts
the ship, else the first X-Forwarded-For entry (every proxy on the way
appends itself AFTER the client), else the socket's own peer."
  (flet ((header (value) (and (stringp value) (plusp (length value)) value)))
    (or (header (net.aserve:header-slot-value req :cf-connecting-ip))
        (let ((forwarded (header (net.aserve:header-slot-value req :x-forwarded-for))))
          (and forwarded
               (header (string-trim " " (subseq forwarded 0 (or (position #\, forwarded)
                                                                (length forwarded)))))))
        (ignore-errors
         (socket:ipaddr-to-dotted (socket:remote-host (net.aserve:request-socket req))))
        "unknown")))

(defun address-key (address)
  "The address as counted: an IPv4 address whole; an IPv6 address by its
first four groups (a /64, one household), so a visitor cannot rotate
through the addresses of one allocation."
  (if (find #\: address)
      (let ((groups (uiop:split-string address :separator ":")))
        (format nil "~{~a~^:~}::" (subseq groups 0 (min 4 (length groups)))))
      address))


;;
;; How often.  One record per address key, today's counts only: the
;; record is replaced when the day changes, and the reaper drops the
;; stale ones.
;;

(defvar *address-counts* (make-hash-table :test #'equal)
  "address key -> (:day \"YYYY-MM-DD\" :sessions n :prompts n)")

(defvar *address-lock* (bt:make-lock "prompt-lab addresses"))

(defun day-key (&optional (now (get-universal-time)))
  (multiple-value-bind (s m h day month year) (decode-universal-time now 0)
    (declare (ignore s m h))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun address-limit (kind)
  (ecase kind
    (:sessions *max-sessions-per-address*)
    (:prompts *max-prompts-per-address*)
    (:automated *max-automated-prompts-per-address*)))

(defun address-record (key)
  "Today's record for KEY (fresh when there is none or it is yesterday's)."
  (let ((record (gethash key *address-counts*))
        (today (day-key)))
    (if (and record (equal (getf record :day) today))
        record
        (setf (gethash key *address-counts*)
              (list :day today :sessions 0 :prompts 0 :automated 0)))))

(defun address-count (address kind)
  "How many KIND (:sessions, :prompts or :automated) ADDRESS has had today."
  (bt:with-lock-held (*address-lock*)
    (or (getf (address-record (address-key address)) kind) 0)))

(defun address-over-limit? (address kind)
  "True when a limit is set for KIND and ADDRESS has reached it today."
  (let ((limit (address-limit kind)))
    (and limit (>= (address-count address kind) limit))))

(defun count-address! (address kind)
  (bt:with-lock-held (*address-lock*)
    (let* ((key (address-key address))
           (record (address-record key)))
      ;; a record from before a kind existed has no place for it yet
      (setf (getf record kind) (1+ (or (getf record kind) 0)))
      (setf (gethash key *address-counts*) record)
      (getf record kind))))


;;
;; The automated lane.  Where a human check stands at the prompt door, a
;; script or an agent that comes to build has no way through it; some of
;; what such callers ask for is worth having, so a prompt that carries NO
;; check token is let in on an allowance of its own: so many a day from
;; one address, and so many a day from all addresses together.  Those
;; prompts spend the same credits as everyone's, and the two figures are
;; what bound what a day of abuse can take -- the second holds against a
;; caller that changes its address.  A prompt that carries a token is
;; checked as ever and counts against neither.
;;

(defvar *automated-day* (cons "" 0)
  "Cons of a UTC day and the automated prompts taken on it, all addresses.")

(defun automated-count ()
  (bt:with-lock-held (*address-lock*)
    (if (equal (car *automated-day*) (day-key)) (cdr *automated-day*) 0)))

(defun automated-lane? ()
  "True when the lab takes prompts without the human check at all."
  (and (integerp *max-automated-prompts-per-day*) (plusp *max-automated-prompts-per-day*)))

(defun automated-room? (address)
  "Values: true when ADDRESS may run one more prompt without the human
check today; else nil and the reason."
  (cond ((not (automated-lane?))
         (values nil "Complete the human check first."))
        ((>= (automated-count) *max-automated-prompts-per-day*)
         (values nil (format nil "This lab takes ~a prompts a day without the human check, and today's are taken.  Come back tomorrow, or build from the page in a browser."
                             *max-automated-prompts-per-day*)))
        ((address-over-limit? address :automated)
         (values nil (format nil "This address has run its ~a prompts without the human check for today.  Come back tomorrow, or build from the page in a browser."
                             *max-automated-prompts-per-address*)))
        (t (values t nil))))

(defun count-automated! (address)
  (count-address! address :automated)
  (bt:with-lock-held (*address-lock*)
    (let ((today (day-key)))
      (if (equal (car *automated-day*) today)
          (incf (cdr *automated-day*))
          (setq *automated-day* (cons today 1))))))

(defun admit-prompt (token address)
  "Whether a prompt from ADDRESS carrying check TOKEN (or none) may run.
Values: true or nil; the reason when not; and :automated when it is let
in without the human check, on the automated lane's allowance -- the
caller counts it (count-automated!) once the prompt has started."
  (cond ((not (turnstile-required?)) (values t nil nil))
        ((and (stringp token) (plusp (length token)))
         (multiple-value-bind (ok? reason) (verify-turnstile token address)
           (values ok? reason nil)))
        (t (multiple-value-bind (room? reason) (automated-room? address)
             (values room? reason (and room? :automated))))))

(defun prune-addresses! ()
  "Forget every address whose record is not today's.  Returns how many."
  (let ((today (day-key)) (pruned 0))
    (bt:with-lock-held (*address-lock*)
      (maphash #'(lambda (key record)
                   (unless (equal (getf record :day) today)
                     (remhash key *address-counts*)
                     (incf pruned)))
               *address-counts*))
    pruned))


;;
;; Turnstile.  The page renders Cloudflare's widget when a site key is
;; set and sends the token it yields with each prompt; the door has the
;; token verified before the agent runs.  Verifying takes the widget's
;; SECRET, which may not live on a host whose visitors hold a console:
;; the default verifier is the gate's Turnstile door beside the
;; Messages door (<*messages-url*>/turnstile), which adds the secret at
;; the gate and relays Cloudflare's answer.  A dev ship with a secret file
;; of its own may name Cloudflare's siteverify directly instead.
;;

(defun turnstile-required? ()
  (and *turnstile-site-key* t))

(defun turnstile-verify-url ()
  (or *turnstile-verify-url*
      (concatenate 'string (string-right-trim "/" *messages-url*) "/turnstile")))

(defun turnstile-secret ()
  "The secret from *turnstile-secret-file*, or nil (the gate adds it)."
  (when *turnstile-secret-file*
    (let ((path (probe-file *turnstile-secret-file*)))
      (when path
        (let ((line (ignore-errors
                     (string-trim '(#\space #\tab #\newline #\return)
                                  (uiop:read-file-string path)))))
          (and line (plusp (length line)) line))))))

(defun verify-turnstile (token address)
  "Have TOKEN verified for the visitor at ADDRESS.  Returns (values ok?
reason): without a site key every prompt passes; otherwise the token
must be present and the verifier must answer {\"success\": true}."
  (cond
    ((not (turnstile-required?)) (values t nil))
    ((not (and (stringp token) (plusp (length token))))
     (values nil "Complete the human check first."))
    ((> (length token) 2048)
     (values nil "That check token is not valid.  Reload the page and try again."))
    (t
     (handler-case
         (let* ((secret (turnstile-secret))
                (body (encode (apply #'h "response" token
                                     (append (when (and address (not (equal address "unknown")))
                                               (list "remoteip" address))
                                             (when secret (list "secret" secret))))))
                (answer (nth-value 1 (post-json (turnstile-verify-url) body :seconds *turnstile-seconds*)))
                (json (ignore-errors (yason:parse answer))))
           (cond ((not (hash-table-p json))
                  (values nil "The human check could not be completed; try again in a moment."))
                 ((eq (gethash "success" json) t) (values t nil))
                 ((gethash "error" json)
                  ;; the gate's own refusal, in the API's error shape
                  (values nil (format nil "The human check could not be completed: ~a"
                                      (or (ignore-errors (gethash "message" (gethash "error" json)))
                                          "the gate refused"))))
                 (t (values nil (format nil "The human check did not pass~@[ (~{~a~^, ~})~].  Try again."
                                        (let ((codes (gethash "error-codes" json)))
                                          (and codes (coerce codes 'list))))))))
       (error (condition)
         (values nil (format nil "The human check could not be completed: ~a" condition)))))))

;;
;; THE LOCK (the user, 2026-10-12): the lab stays private for a while --
;; one password, no username, nothing without it.  *password* comes
;; from the room's init or *password-file* (parameters.lisp); nil leaves
;; the lab open.  A visitor who posts it to <prefix>/api/unlock gets the
;; cookie prompt-lab-key, whose value is a token kept beside the password
;; file, so the password itself never rides in a cookie and both labs on
;; a ship admit the same visitor.  Page paths (the sheet, its listings, the
;; viewer) show the unlock form to anyone without it; the doors under
;; <prefix>/api/ answer 403.  The deployed apps' doors (kinds.lisp) and
;; the hosting worker's (hosting.lisp, its own secret) are not behind it.
;;

(defvar *unlock-token* nil
  "The cookie value that admits a visitor; minted by load-password!.")

(defparameter *unlock-cookie* "prompt-lab-key")

(defun unlock-token-file ()
  "Where the token lives: beside *password-file*, so every room reading
that password (the free lab and the solids lab on one ship) admits the
same cookie, and the cookie outlives a restart."
  (and *password-file*
       (namestring (make-pathname :name "prompt-lab-unlock-token" :type nil
                                  :defaults (pathname *password-file*)))))

(defun load-password! ()
  "Read *password-file*'s first line into *password* when the file is
there and no password is set yet, and load or mint the token (kept in
unlock-token-file; delete it with the password to turn every cookie
away).  Returns whether the lab is locked."
  (flet ((first-line (file)
           (with-open-file (in file :if-does-not-exist nil)
             (let ((line (and in (read-line in nil))))
               (and line
                    (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
                      (and (plusp (length trimmed)) trimmed)))))))
    (when (and (null *password*) *password-file*)
      (setf *password* (first-line *password-file*)))
    (when (and *password* (null *unlock-token*))
      (let ((file (unlock-token-file)))
        (setf *unlock-token*
              (or (and file (first-line file))
                  (let ((token (new-owner-key)))
                    (when file
                      (ignore-errors
                       (with-open-file (out file :direction :output :if-exists :supersede
                                                 :if-does-not-exist :create)
                         (write-line token out))))
                    token))))))
  (locked?))

(defun locked? ()
  (and *password* t))

(defun admitted? (req)
  "Whether REQ carries the cookie that unlocks the lab, or the lab is open."
  (or (not (locked?))
      (let ((cookie (cdr (assoc *unlock-cookie* (net.aserve:get-cookie-values req) :test #'string=))))
        (and cookie *unlock-token* (string= cookie *unlock-token*)))))

(defun gated (function)
  "FUNCTION as a door only an admitted visitor opens; anyone else gets a 403."
  (lambda (req ent)
    (if (admitted? req)
        (funcall function req ent)
        (refuse req ent net.aserve:*response-forbidden*
                "This lab is private; unlock it at ~a." *url-prefix*))))

(defun gated-page (function)
  "FUNCTION as a page only an admitted visitor sees; anyone else gets the unlock form."
  (lambda (req ent)
    (if (admitted? req)
        (funcall function req ent)
        (unlock-page req ent))))

(defun publish-gated-gwl-app (path class &key host)
  "gwl:publish-gwl-app with the lock on it: CLASS answers at PATH to an
admitted visitor; anyone else gets the unlock form."
  (apply #'net.aserve:publish
         :path path
         :server gwl:*http-server*
         :function (gated-page (lambda (req ent)
                                 (gwl:gwl-make-object req ent (format nil "~s" class))))
         (and host (list :host host))))

(defun unlock-escape (string)
  (with-output-to-string (out)
    (loop for char across string
          do (case char
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char char out))))))

(defun safe-next (next)
  "NEXT when it is a path on this host (one leading slash), else the prefix."
  (if (and (stringp next)
           (> (length next) 0)
           (char= (char next 0) #\/)
           (not (and (> (length next) 1) (member (char next 1) '(#\/ #\\)))))
      next
      *url-prefix*))

(defun request-path-and-query (req)
  (let ((uri (net.aserve:request-uri req)))
    (format nil "~a~@[?~a~]" (net.uri:uri-path uri) (net.uri:uri-query uri))))

(defun unlock-page (req ent &key wrong (next (request-path-and-query req)))
  "The unlock form: one password field, posted to <prefix>/api/unlock
with the address to go on to afterwards."
  (net.aserve:with-http-response (req ent :content-type "text/html; charset=utf-8")
    (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
    (net.aserve:with-http-body (req ent)
      (format net.html.generator:*html-stream*
              "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<meta name=\"robots\" content=\"noindex\"><title>~a</title>
<style>body{font-family:system-ui,sans-serif;background:#f4f4f2;color:#222;display:flex;min-height:100vh;margin:0;align-items:center;justify-content:center}
form{background:#fff;padding:2rem 2.5rem;border-radius:12px;box-shadow:0 2px 12px rgba(0,0,0,.08);max-width:22rem;width:90vw}
h1{font-size:1.2rem;margin:0 0 .4rem}p{margin:.4rem 0 1rem;color:#555;font-size:.95rem}
input[type=password]{width:100%;box-sizing:border-box;font-size:1.1rem;padding:.55rem .7rem;border:1px solid #bbb;border-radius:8px}
button{margin-top:1rem;width:100%;font-size:1rem;padding:.6rem;border:0;border-radius:8px;background:#2a5db0;color:#fff;cursor:pointer}
.wrong{color:#b02a2a}</style></head><body>
<form method=\"post\" action=\"~a/api/unlock\">
<h1>~a</h1>
<p~:[~; class=\"wrong\"~]>~:[This lab is private.  Enter the password to come in.~;That is not it.  Try again.~]</p>
<input type=\"password\" name=\"password\" autofocus autocomplete=\"current-password\" aria-label=\"Password\">
<input type=\"hidden\" name=\"next\" value=\"~a\">
<button type=\"submit\">Enter</button>
</form></body></html>"
              (unlock-escape (lab-title)) *url-prefix* (unlock-escape (lab-title))
              wrong wrong (unlock-escape (safe-next next))))))

(defun unlock-door (req ent)
  "POST <prefix>/api/unlock: the password (form field password) sets the
cookie and sends the visitor on to next (the prefix by default); a wrong
one shows the form again.  An open lab just sends the visitor on."
  (let* ((query (net.aserve:request-query req))
         (password (cdr (assoc "password" query :test #'string=)))
         (next (safe-next (cdr (assoc "next" query :test #'string=)))))
    (cond ((or (not (locked?))
               (and (stringp password) (string= password *password*)))
           (net.aserve:with-http-response (req ent :response net.aserve:*response-found*)
             (when (locked?)
               (net.aserve:set-cookie-header req :name *unlock-cookie* :value *unlock-token*
                                                 :path "/" :http-only t
                                                 :expires (+ (get-universal-time) (* 30 24 3600))))
             (setf (net.aserve:reply-header-slot-value req :location) next)
             (net.aserve:with-http-body (req ent))))
          (t (unlock-page req ent :wrong t :next next)))))
