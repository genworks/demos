;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The page and its doors.  The page (static/page.html) is one
;; document, the same for every visitor but for the addresses of its
;; stylesheet and script and the list of skins (skins.lisp).  Its script
;; (static/prompt-lab.js) opens or resumes a session, posts prompts,
;; polls the session's state (the log, the token totals, the model
;; file) and reloads the viewer beside it after every build.  The doors
;; under <prefix>/api/ are plain aserve publishes with no gwl session
;; behind them, cheap and nothing to reap.  The viewer is a gwl app: a
;; sluice opened on the session's MODEL, dressed in the page's skin.
;;

(defparameter *page-file*
  ;; the SOURCE file's place, read at compile time: at load time the
  ;; truename is the fasl's, off in a cache directory
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (make-pathname :name "page" :type "html"
                   :directory (append (butlast (pathname-directory here)) (list "static"))
                   :defaults here))
  "Pathname. The page, beside the source in static/.")

(defun page-door (req ent)
  "GET <prefix>: the page, filled in (page-text, skins.lisp).  Never kept by
a cache: it names its stylesheets by their dates."
  (net.aserve:with-http-response (req ent :content-type "text/html; charset=utf-8")
    (setf (net.aserve:reply-header-slot-value req :cache-control) "no-cache")
    (net.aserve:with-http-body (req ent)
      (write-string (page-text) net.html.generator:*html-stream*))))


;;
;; JSON in and out.
;;

(defun respond-json (req ent object &optional (response net.aserve:*response-ok*))
  (net.aserve:with-http-response (req ent :content-type "application/json; charset=utf-8"
                                          :response response)
    (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
    (net.aserve:with-http-body (req ent)
      (yason:encode object net.html.generator:*html-stream*))))

(defun request-text (req)
  "The request body as text.  This aserve hands the body back with each
byte as one character (Latin-1); a page sends UTF-8, so the bytes are
decoded again as UTF-8 when they can be -- a prompt with a dash or a
multiplication sign in it arrived as mojibake before this."
  (let ((body (ignore-errors (net.aserve:get-request-body req))))
    (when (and body (plusp (length body)))
      (if (every #'(lambda (char) (< (char-code char) 256)) body)
          (or (ignore-errors
               (babel:octets-to-string
                (map '(vector (unsigned-byte 8)) #'char-code body)
                :encoding :utf-8))
              body)
          body))))

(defun request-json (req)
  "The request body parsed as JSON, when it is an object; nil otherwise."
  (let ((body (request-text req)))
    (when body
      (let ((parsed (ignore-errors (yason:parse body))))
        (when (hash-table-p parsed) parsed)))))

(defun query-value (req name)
  (cdr (assoc name (net.aserve:request-query req) :test #'string-equal)))

(defun requested-session (req &optional json)
  "The session named by the JSON body's \"session\" or the query's session=."
  (let ((id (or (and json (gethash "session" json)) (query-value req "session"))))
    (and (stringp id) (find-session id))))

(defun no-such-session (req ent)
  (respond-json req ent (h "error" "No such session.") net.aserve:*response-not-found*))


;;
;; Whose session it is.  The session door hands the opening browser the
;; session's owner key; the page sends it back as X-Prompt-Lab-Owner
;; (the viewer's iframe, which cannot send a header, as owner=).  Only
;; the owner prompts, edits, reloads or pays; anyone else who has the
;; URL watches, read-only, when *browsing?* allows it.
;;

(defun request-owner-key (req &optional json)
  (flet ((key (value) (and (stringp value) (plusp (length value)) value)))
    (or (key (net.aserve:header-slot-value req :x-prompt-lab-owner))
        (and json (key (gethash "owner" json)))
        (key (query-value req "owner")))))

(defun owner? (session key &optional address)
  "True when KEY is SESSION's owner key.  A session opened before owners
were minted has none; the address that opened it stands in, by the same
/64 the guards count."
  (let ((owner (session-owner session)))
    (cond ((session-replay? session) nil)
          (owner (and (stringp key) (string= key owner)))
          (t (let ((opened (session-address session)))
               (and (stringp address) (stringp opened)
                    (string= (address-key address) (address-key opened))))))))

(defun owner-request? (req session &optional json)
  (owner? session (request-owner-key req json) (client-address req)))

(defun visible-to? (session key)
  "Whether a request naming owner KEY may see SESSION: any session that is
not private, and a private one only with its key.  (A private session
always has a key: going private mints one -- see privacy-door.)"
  (or (not (session-private? session))
      (let ((owner (session-owner session)))
        (and owner (stringp key) (string= key owner)))))

(defun not-yours (req ent)
  (refuse req ent net.aserve:*response-forbidden*
          "This session belongs to the visitor who opened it; you can watch it, not change it.  Start a session of your own to build."))

(defvar *response-too-many-requests* (net.aserve::make-resp 429 "Too Many Requests")
  "AllegroServe ships no 429; this is ours.")

(defun refuse (req ent control &rest args)
  "A 400 with the reason.  A leading response object in ARGS sets the
status instead: (refuse req ent *response-forbidden* \"No.\")."
  (let ((response net.aserve:*response-bad-request*))
    (when (typep control 'net.aserve::response)
      (setf response control control (pop args)))
    (respond-json req ent (h "error" (apply #'format nil control args)) response)))

(defun epoch-seconds (universal-time)
  (- universal-time #.(encode-universal-time 0 0 0 1 1 1970 0)))


;;
;; What the page knows about a session.
;;

(defun prompts-used (session)
  (count :prompt (session-log session) :key #'second))

(defun balance-number (session name)
  "The number the gate's last balance answer for SESSION gave under NAME, or nil."
  (let* ((balance (session-balance session))
         (value (and balance (gethash name balance))))
    (and (realp value) value)))

(defun paying? (session)
  "True when the session's wallet holds credit: the free-use caps (prompts
a session, prompts and sessions an address a day) step aside, and the
gate's budgets are what bind.  Where the gate keeps a community pot
nobody holds credit of their own; there it is true while the wallet has
put more into the pot than its sessions have drawn from it."
  (let ((credits (session-credits session))
        (left (balance-number session "contribution_left_cents")))
    (or (and (realp credits) (plusp credits))
        (and left (plusp left)))))

(defun paid? (session)
  "True once the session's wallet has bought credits -- credit on it now,
credits already drawn from it here, or a top-up of the community pot:
what makes a session eligible to be made private."
  (or (paying? session)
      (let ((charged (session-charged session))) (and (realp charged) (plusp charged)))
      (let ((contributed (balance-number session "contributed_cents")))
        (and contributed (plusp contributed)))))


;;
;; The community pot.  A gate may keep ONE balance of modeling credits
;; for everybody in place of a free allowance a session and a wallet a
;; payer: every build, whoever's, draws on it; anyone may add to it, up
;; to a cap; at zero the gate refuses, and the page asks for a top-up.
;; The gate says so in every balance it answers ("pot": true, what the
;; pot holds, its cap, the room left), and with each relayed call.  The
;; lab keeps the latest word, asks again when that is *pot-refresh-seconds*
;; old -- other labs draw on the same pot -- and shows it to everyone,
;; owner and watcher alike: it is nobody's secret.
;;

(defvar *pot* nil
  "Plist or nil. What the gate last said of its pot: :credits :max :room
:topup? :amounts :key.  Nil until a gate has said it keeps one, and again
once it says it does not.")

(defvar *pot-asked* 0 "Universal time the gate was last asked about the pot.")

(defun note-pot! (json)
  "Keep what a balance answer (a hash table) says of the gate's pot.  An
answer that is no balance -- a refusal, an error -- says nothing of it."
  (when (and (hash-table-p json) (nth-value 1 (gethash "markup" json)))
    (setq *pot* (and (eq (gethash "pot" json) t)
                     (list :credits (gethash "pot_credits" json)
                           :max (gethash "pot_max" json)
                           :room (gethash "pot_room" json)
                           :topup? (eq (gethash "topup" json) t)
                           :amounts (gethash "topup_amounts" json)
                           :key (gethash "publishable_key" json))))))

(defun note-pot-credits! (credits)
  "Keep what a relayed call says the pot holds now (agent.lisp)."
  (let ((pot *pot*))
    (when (and pot (realp credits))
      (let ((spent (- (or (getf pot :credits) credits) credits)))
        (setq *pot* (list* :credits credits
                           ;; what was drawn is room again, up to the cap
                           :room (+ (or (getf pot :room) 0) (max 0 spent))
                           (loop for (key value) on pot by #'cddr
                                 unless (member key '(:credits :room))
                                   append (list key value))))))))

(defun pot ()
  "The gate's pot as last heard of (a plist), or nil when the gate keeps
none or has not answered.  Asks the gate when the word is old; a gate
that does not answer leaves the old word standing."
  (let ((now (get-universal-time)))
    (when (>= (- now *pot-asked*) *pot-refresh-seconds*)
      ;; whoever asks within the interval takes what is here
      (setq *pot-asked* now)
      (ignore-errors
       (let ((json (gate-post "balance" (h) :seconds *pot-seconds*)))
         (when json (note-pot! json))))))
  *pot*)

(defun pot-empty? ()
  "True when the gate keeps a pot and it is spent: nothing builds."
  (let ((pot (pot)))
    (and pot (not (plusp (or (getf pot :credits) 0))))))

(defun pot-state ()
  "What the page shows of the pot, or nil: what it holds, its cap, the
room left, whether and at which amounts it can be topped up, and where
to go for a lab of one's own when it is full."
  (let ((pot (pot)))
    (and pot
         (h "credits" (max 0 (floor (or (getf pot :credits) 0)))
            "max" (getf pot :max)
            "room" (max 0 (floor (or (getf pot :room) 0)))
            "topup" (if (getf pot :topup?) t 'yason:false)
            "topup_amounts" (or (getf pot :amounts) #())
            "publishable_key" (or (getf pot :key) "")
            "own_lab_url" (car *own-lab*)
            "own_lab_label" (cdr *own-lab*)))))

(defun free-caps? (session)
  "True when the free-use caps bind SESSION: prompts a session, prompts an
address a day.  Not for a session that is paying.  And not where the gate
keeps a community pot and the human check stands at the prompt door:
there the credits are everybody's, whoever put them in, the pot is what
runs out, and the check on every prompt is what keeps a bot from draining
it.  A pot with no human check in front of it keeps the caps."
  (not (or (paying? session)
           (and (pot) (turnstile-required?)))))

(defun model-defined? (session)
  (let ((symbol (model-symbol session)))
    (and symbol (find-class symbol nil) t)))

(defun model-body (session)
  "The model file's source after its header (the in-package line): what
the page's editor shows, and what write-model takes back."
  (file-model-body (session-model-file session)))

(defun file-model-body (file)
  "A model FILE's source after its header; the empty string without one."
  (if (probe-file file)
      (let* ((text (uiop:read-file-string file :external-format :utf-8))
             (start (search "(in-package" text))
             (eol (and start (position #\Newline text :start start))))
        (if eol
            (string-left-trim '(#\Newline #\Return) (subseq text (1+ eol)))
            text))
      ""))

(defun viewer-url (session)
  (format nil "~a/viewer?session=~a" *url-prefix* (session-id session)))

(defun console-url (session)
  "The terminal opened on the session's model file, or nil without a terminal."
  (when *console-base*
    (format nil "~a/?arg=~a" *console-base* (namestring (session-model-file session)))))

;;
;; The gate's side doors -- balance, top-up, confirm -- beside the
;; Messages door (guards.lisp knows the Turnstile one).  The gate holds
;; the Stripe key and the wallet ledger; this side only relays for the
;; page, and remembers what the gate said.
;;

(defun gate-door-url (name)
  (format nil "~a/~a" (string-right-trim "/" *messages-url*) name))

(defun gate-post (name object &key (seconds *gate-seconds*))
  "POST OBJECT (a hash table) as JSON to the gate's side door NAME.
Values: the parsed answer (a hash table, or nil) and the status."
  (multiple-value-bind (status text) (post-json (gate-door-url name) (encode object) :seconds seconds)
    (let ((json (ignore-errors (yason:parse text))))
      (values (and (hash-table-p json) json) status))))

(defun note-balance (session json)
  "Keep a balance answer from the gate on SESSION."
  (let ((wallet (gethash "wallet" json))
        (credits (gethash "credits_cents" json))
        (cents (gethash "session_cents" json))
        (allowance (gethash "allowance_cents" json)))
    (when (and (stringp wallet) (plusp (length wallet))) (setf (session-wallet session) wallet))
    (when (realp credits) (setf (session-credits session) credits))
    (when (and (realp cents) (> cents (session-cents session))) (setf (session-cents session) cents))
    (when (and (realp allowance) (plusp allowance)) (setf (session-allowance session) allowance))
    (setf (session-balance session) json)
    ;; and what it says of the community pot, if the gate keeps one
    (note-pot! json)
    json))

(defun refresh-balance! (session)
  "Ask the gate about the session's spend and wallet; nothing on failure
(a lab without a gate still works, it just shows no balance)."
  (ignore-errors
   (let ((json (gate-post "balance" (h "wallet" (session-wallet session) "session" (session-id session)))))
     (when json (note-balance session json)))))

(defun wallet-id? (string)
  (and (stringp string) (= (length string) 24) (every #'(lambda (c) (digit-char-p c 16)) string)))

(defun spend-state (session)
  "What the page shows about money, in MODELING CREDITS -- the one unit
the visitor ever sees.  A credit is a cent of the wallet's balance; the
session's spend and its free allowance, which the gate keeps in the
API's cents, are converted at the gate's rate so free and paid read on
one scale.  Tokens never appear."
  (let* ((balance (session-balance session))
         (rate (or (and balance (let ((m (gethash "markup" balance))) (and (realp m) m))) 1))
         (allowance (or (session-allowance session) *free-allowance-cents*)))
    (h "credits_used" (round (* rate (session-cents session)))
       "credits_free" (round (* rate allowance))
       "credits_from_wallet" (round (session-charged session))
       "credits_balance" (and (session-credits session) (max 0 (round (session-credits session))))
       ;; where the gate keeps a community pot: what this wallet has put
       ;; into it, and what of that its sessions have not drawn yet
       "contributed" (round (or (balance-number session "contributed_cents") 0))
       "contribution_left" (round (or (balance-number session "contribution_left_cents") 0))
       "wallet" (session-wallet session)
       "topup" (if (and balance (eq (gethash "topup" balance) t)) t 'yason:false)
       "topup_amounts" (or (and balance (gethash "topup_amounts" balance)) #())
       "publishable_key" (or (and balance (gethash "publishable_key" balance)) ""))))

(defun page-url (req session)
  "The page's own public URL for SESSION, as the visitor's browser has
it: the scheme and host the proxies forwarded."
  (flet ((header (value) (and (stringp value) (plusp (length value)) value)))
    (let ((host (or (header (net.aserve:header-slot-value req :x-cyclops-forwarded-host))
                    (header (net.aserve:header-slot-value req :host))
                    "localhost"))
          (proto (or (header (net.aserve:header-slot-value req :x-forwarded-proto)) "http")))
      (format nil "~a://~a~a?session=~a" proto host *url-prefix* (session-id session)))))

(defun log-vector (log)
  "A session's log, (time kind text) entries, as the page reads it."
  (map 'vector #'(lambda (entry)
                   (destructuring-bind (time kind text) entry
                     (h "time" (epoch-seconds time)
                        "kind" (string-downcase kind)
                        "text" text)))
       log))

(defun session-state (session &key (owner? t))
  "Everything the page shows about SESSION.  For a watcher (OWNER? nil)
the owner's own things are left out: the wallet and the spend, the model
file's place and the terminal opened on it."
  (let ((usage (session-usage session)))
    (h "session" (session-id session)
       "editable" (if owner? t 'yason:false)
       "private" (if (session-private? session) t 'yason:false)
       ;; the owner may close the session once it has bought credits
       "may_be_private" (if (and owner? (paid? session)) t 'yason:false)
       "busy" (if (session-busy? session) t 'yason:false)
       "prompts_used" (prompts-used session)
       "prompts_allowed" *max-prompts-per-session*
       ;; with credit on the wallet the prompt cap does not apply; nor
       ;; at a community pot behind a human check (free-caps?)
       "prompts_unlimited" (if (free-caps? session) 'yason:false t)
       "usage" (h "input" (getf usage :input) "output" (getf usage :output)
                  "cache_read" (getf usage :cache-read) "cache_write" (getf usage :cache-write))
       ;; symbols compiled and run, the meter's side of the spend, and
       ;; the credits the gate booked for them
       "meter" (h "compile" (or (getf (session-meter session) :compile) 0)
                  "run" (or (getf (session-meter session) :run) 0)
                  "credits" (round (or (getf (session-meter session) :credits) 0)))
       "engine" (engine-name)
       "created" (epoch-seconds (session-created session))
       "spend" (and owner? (spend-state session))
       ;; the community pot, where the gate keeps one: everyone's to see
       "pot" (pot-state)
       "log" (log-vector (session-log session))
       "model_defined" (if (model-defined? session) t 'yason:false)
       "model_file" (and owner? (namestring (session-model-file session)))
       "model_source" (model-body session)
       "viewer_url" (viewer-url session)
       ;; nil encodes as null; an empty vector would be the empty array
       "console_url" (and owner? (console-url session)))))


;;
;; The doors.
;;

(defun config-door (req ent)
  "GET <prefix>/api/config: what the page needs before it has a session --
the Turnstile site key (nil: no widget), the limits it shows, the engine
behind this room and the sibling lab on the other engine, if any."
  (respond-json req ent (h "turnstile_site_key" *turnstile-site-key*
                           "prompts_allowed" *max-prompts-per-session*
                           "prompt_max_length" *max-prompt-length*
                           "engine" (engine-name)
                           "engine_label" (engine-label)
                           "sibling_url" (car *sibling-lab*)
                           "sibling_label" (cdr *sibling-lab*)
                           "browsing" (if *browsing?* t 'yason:false)
                           ;; the files the model may be had as (export.lisp)
                           "downloads" (coerce (downloads-state) 'vector)
                           ;; the community pot, where the gate keeps one
                           "pot" (pot-state))))

(defun session-door (req ent)
  "POST <prefix>/api/session {wallet?}: open a session; answers its id.
One address opens at most *max-sessions-per-address* a day."
  (let* ((address (client-address req))
         (json (request-json req))
         (wallet (and json (gethash "wallet" json))))
    (if (and (address-over-limit? address :sessions)
             ;; a wallet with credit -- or, at a community pot, one that
             ;; has put in more than it has drawn -- opens sessions past
             ;; the free cap
             (not (and (wallet-id? wallet)
                       (let ((balance (ignore-errors (gate-post "balance" (h "wallet" wallet)))))
                         (and balance
                              (flet ((credit? (name)
                                       (let ((value (gethash name balance)))
                                         (and (realp value) (plusp value)))))
                                (or (credit? "credits_cents") (credit? "contribution_left_cents"))))))))
        (refuse req ent *response-too-many-requests*
                "This address has opened its ~a free sessions for today.  Come back tomorrow, or bring your own agent."
                *max-sessions-per-address*)
        (let ((session (make-session :address address :wallet (and (wallet-id? wallet) wallet))))
          (count-address! address :sessions)
          (log-event session :note "Session ~a opened.  Describe what to build." (session-id session))
          (refresh-balance! session)
          (save-session! session)
          ;; the owner key goes to this browser once, here, and nowhere else
          (respond-json req ent (h "session" (session-id session)
                                   "owner" (session-owner session)
                                   "spend" (spend-state session)
                                   "pot" (pot-state)))))))

(defun topup-door (req ent)
  "POST <prefix>/api/topup {session, amount_cents, embedded?}: a Stripe
Checkout through the gate.  Answers {url, wallet} for Stripe's hosted
page, or {client_secret, publishable_key, wallet} for the in-page form."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (amount (and json (gethash "amount_cents" json)))
         (embedded? (and json (eq (gethash "embedded" json) t)))
         (address (client-address req)))
    (let ((verdict :unchecked))
      (flet ((verified? ()
               (when (eq verdict :unchecked)
                 (setf verdict (multiple-value-list
                                (verify-turnstile (and json (gethash "turnstile" json)) address))))
               (first verdict))
             (reason () (or (second verdict) "Complete the human check first.")))
    (cond ((null session) (no-such-session req ent))
          ((not (owner-request? req session json)) (not-yours req ent))
          ((not (integerp amount)) (refuse req ent "Say how much."))
          ;; a public Checkout for small amounts draws card testers: a
          ;; fresh Turnstile token before every purchase, as before every
          ;; prompt (checked once -- a token is single-use)
          ((not (verified?))
           (refuse req ent net.aserve:*response-forbidden* "~a" (reason)))
          (t
           (let ((url (page-url req session)))
             (multiple-value-bind (answer status)
                 (gate-post "topup" (h "wallet" (session-wallet session)
                                       "amount_cents" amount
                                       "success_url" url
                                       "cancel_url" (concatenate 'string url "&topup=cancelled")
                                       "embedded" (if embedded? t 'yason:false)))
               (let ((checkout-url (and answer (gethash "url" answer)))
                     (client-secret (and answer (gethash "client_secret" answer)))
                     (wallet (and answer (gethash "wallet" answer))))
                 (cond ((and (eql status 200) (or (stringp checkout-url) (stringp client-secret)))
                        (when (wallet-id? wallet) (setf (session-wallet session) wallet))
                        (log-event session :note (if (pot)
                                                     "Adding ~:d modeling credits to the community pot.  They arrive when the payment completes."
                                                     "Buying ~:d modeling credits.  They arrive when the payment completes.")
                                   amount)
                        (respond-json req ent (h "url" checkout-url
                                                 "client_secret" client-secret
                                                 "publishable_key" (gethash "publishable_key" (spend-state session))
                                                 "wallet" (session-wallet session))))
                       (t (refuse req ent net.aserve:*response-service-unavailable* "~a"
                                  (or (ignore-errors (gethash "message" (gethash "error" answer)))
                                      "The top-up could not be started; try again in a moment.")))))))))))))

(defun format-cents (cents)
  (if (and (realp cents) (>= cents 100))
      (format nil "$~,2f" (/ cents 100))
      (format nil "~,1f cents" (or cents 0))))

(defun confirm-door (req ent)
  "POST <prefix>/api/confirm {session, wallet, checkout}: the visitor is
back from Stripe; have the gate credit the checkout once and answer the
spend state with the outcome."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (wallet (and json (gethash "wallet" json)))
         (checkout (and json (gethash "checkout" json))))
    (cond ((null session) (no-such-session req ent))
          ((not (owner-request? req session json)) (not-yours req ent))
          ((not (wallet-id? wallet)) (refuse req ent "No wallet named."))
          (t
           (setf (session-wallet session) wallet)
           (multiple-value-bind (answer status)
               (gate-post "confirm" (h "wallet" wallet "checkout" checkout "session" (session-id session)))
             (declare (ignore status))
             (when answer (note-balance session answer))
             (let ((outcome (and answer (gethash "outcome" answer))))
               (when (equal outcome "credited")
                 (if *pot*
                     (log-event session :note "Thank you: the community pot holds ~:d modeling credits now, for everyone's builds."
                                (max 0 (floor (or (getf *pot* :credits) 0))))
                     (log-event session :note "Credits added: ~:d on your balance.  Builds beyond the free credits draw on it."
                                (round (or (session-credits session) 0)))))
               (save-session! session)
               (respond-json req ent (h "outcome" (or outcome "failed")
                                        "text" (or (and answer (gethash "text" answer)) "The gate did not answer.")
                                        "spend" (spend-state session)
                                        "pot" (pot-state)))))))))

(defun state-door (req ent)
  "GET <prefix>/api/state?session=<id>: everything the page shows.  The
owner's request keeps the session alive; a watcher's does not, and a
watcher is refused when *browsing?* is off."
  (let ((session (requested-session req)))
    (cond ((null session) (no-such-session req ent))
          ((owner-request? req session) (respond-json req ent (session-state (touch session))))
          ((and *browsing?* (not (session-private? session)))
           (respond-json req ent (session-state session :owner? nil)))
          (t (refuse req ent net.aserve:*response-forbidden* "This session is private.")))))

(defun privacy-door (req ent)
  "POST <prefix>/api/privacy {session, private}: the owner closes the
session to watchers -- out of the listings, live and archived, its URL
answering no one else -- or opens it again.  Closing takes a session
that has bought credits.  A session opened before owner keys gets one
here, answered as \"owner\", since the address no longer suffices once
nobody else may look."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (private? (and json (eq (gethash "private" json) t))))
    (cond ((null session) (no-such-session req ent))
          ((not (owner-request? req session json)) (not-yours req ent))
          ((and private? (not (paid? session)))
           (refuse req ent "A session becomes private once it has bought modeling credits."))
          (t (unless (session-owner session) (setf (session-owner session) (new-owner-key)))
             (unless (eq private? (session-private? session))
               (setf (session-private? session) private?)
               ;; a replay built from the archive while it was open would
               ;; still show it (browse.lisp); the next is built afresh
               (let ((replay (find-replay (session-id session))))
                 (when replay (drop-replay replay)))
               (log-event session :note (if private?
                                            "This session is private now: out of the listings, and closed to anyone else with its link."
                                            "This session is open to view again.")))
             (save-session! session)
             (respond-json req ent (h "private" (if private? t 'yason:false)
                                      "owner" (session-owner session)))))))

(defun prompt-door (req ent)
  "POST <prefix>/api/prompt {session, prompt}: start the agent on the
prompt in a thread of its own; the page follows along through the state door."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (prompt (and json (gethash "prompt" json)))
         (address (client-address req)))
    (cond ((null session) (no-such-session req ent))
          ((not (owner? session (request-owner-key req json) address)) (not-yours req ent))
          ((not (and (stringp prompt)
                     (plusp (length (string-trim '(#\space #\tab #\newline #\return) prompt)))))
           (refuse req ent "Say what to build."))
          ((> (length prompt) *max-prompt-length*)
           (refuse req ent "A prompt may have ~a characters at most." *max-prompt-length*))
          ;; a community pot with nothing in it builds for nobody
          ((pot-empty?)
           (refuse req ent *response-too-many-requests*
                   "The community pot of modeling credits is empty.  Top it up and the lab builds again, for everyone."))
          ((and (free-caps? session) (>= (prompts-used session) *max-prompts-per-session*))
           (refuse req ent "This session has used its ~a ~:[free ~;~]prompts.  ~a to keep going here, take a copy of the model file, or start a new session."
                   *max-prompts-per-session* (pot)
                   (if (pot) "Add modeling credits to the pot" "Buy modeling credits")))
          ((and (free-caps? session) (address-over-limit? address :prompts))
           (refuse req ent *response-too-many-requests*
                   "This address has run its ~a ~:[free ~;~]prompts for today.  ~a to keep going, come back tomorrow, or bring your own agent."
                   *max-prompts-per-address* (pot)
                   (if (pot) "Add modeling credits to the pot" "Buy modeling credits")))
          ((session-busy? session)
           (refuse req ent "Still working on the previous request."))
          (t
           ;; the token is single-use and the check is a network call:
           ;; last, after every cheap refusal.  A prompt with no token at
           ;; all is a script's or an agent's, and rides the automated
           ;; lane's allowance when there is one (guards.lisp).
           (multiple-value-bind (ok? reason lane) (admit-prompt (gethash "turnstile" json) address)
             (cond ((not ok?)
                    (refuse req ent (if (and (automated-lane?) (null (gethash "turnstile" json)))
                                        *response-too-many-requests*
                                        net.aserve:*response-forbidden*)
                            "~a" reason))
                   ((not (start-prompt! session (string-trim '(#\space #\tab #\newline #\return) prompt)))
                    (refuse req ent "Still working on the previous request."))
                   (t (count-address! address :prompts)
                      (when (eq lane :automated)
                        (count-automated! address)
                        (log-event session :note "This prompt came without the human check: an automated one, on the lab's daily allowance for those."))
                      (respond-json req ent (h "started" t "automated" (if (eq lane :automated) t 'yason:false))
                                    net.aserve:*response-accepted*))))))))

(defun model-door (req ent)
  "POST <prefix>/api/model {session, source}: the visitor's own edit of the
model file, written, compiled and loaded like the agent's."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (source (and json (gethash "source" json))))
    (cond ((null session) (no-such-session req ent))
          ((not (owner-request? req session json)) (not-yours req ent))
          ((not (stringp source)) (refuse req ent "No source given."))
          ((session-busy? session) (refuse req ent "Wait for the agent to finish first."))
          (t (multiple-value-bind (blocks error?) (write-model (touch session) source)
               (let ((text (or (cdr (assoc "text" (first blocks) :test #'string=)) "")))
                 (log-event session :reload "Your edit: ~a" text)
                 (save-session! session)
                 (respond-json req ent (h "ok" (if error? 'yason:false t) "text" text))))))))

(defun reload-door (req ent)
  "POST <prefix>/api/reload {session}: compile and load the model file as
it is on disk, after an edit made in the terminal."
  (let* ((json (request-json req))
         (session (requested-session req json)))
    (cond ((null session) (no-such-session req ent))
          ((not (owner-request? req session json)) (not-yours req ent))
          ((session-busy? session) (refuse req ent "Wait for the agent to finish first."))
          ((not (probe-file (session-model-file session))) (refuse req ent "There is no model file yet."))
          (t (multiple-value-bind (blocks error?) (load-model-file (touch session))
               (let ((text (or (cdr (assoc "text" (first blocks) :test #'string=)) "")))
                 (log-event session :reload "Reloaded from disk: ~a" text)
                 (save-session! session)
                 (respond-json req ent (h "ok" (if error? 'yason:false t) "text" text))))))))


;;
;; The viewer: a sluice opened on the session's MODEL, its leaves drawn
;; as the page opens.
;;

(define-object viewer (sluice:assembly)

  :documentation
  (:description "A sluice opened on one prompt-lab session's MODEL, reached
at <prefix>/viewer?session=<id>.  The model's leaves are drawn as the page
opens; the tree, the menus and the headset button are the sluice's own.
It wears the page's skin (skin=<name>), and on a phone (mode=phone) it is
the model with one panel under it, the inputs or the tree."
   :author "Genworks International")

  :computed-slots
  ((title "Prompt lab viewer")

   ;; the skin the page wears, named on the frame's address; nil is the
   ;; house look
   (page-skin (find-skin (cdr (assoc "skin" (the query-toplevel) :test #'string-equal))))

   ;; what the sluice is told to wear: the same, when the skin is one of
   ;; its own (skins.lisp)
   (skin (viewer-skin (the page-skin)))

   ;; the page is laid out for a phone: the frame shows the model and,
   ;; under it, whichever panel the page's tabs ask for
   (phone? (equal (cdr (assoc "mode" (the query-toplevel) :test #'string-equal)) "phone"))

   ;; after the sluice's own sheets and its skin: the phone's sheet
   ;; when the page is one, and a skin of the lab's own (an older
   ;; sluice is handed the tokens and the skin here too).  Only INPUTS
   ;; of the sluice are overridden here: where its package is locked
   ;; (a Genworks GDL workshop) its computed slots, body-class among
   ;; them, are reserved words.
   (additional-css-links (viewer-css-links (the page-skin) :phone? (the phone?)))

   ;; on a phone the inspector holds the inputs alone
   (user-mode?-default (the phone?))

   (session-id (cdr (assoc "session" (the query-toplevel) :test #'string-equal)))

   ;; ?replay=<id> instead: an archived session's model, compiled again
   ;; by the replay door (browse.lisp)
   (replay-id (cdr (assoc "replay" (the query-toplevel) :test #'string-equal)))

   ;; the owner's page names its key (owner=) on the iframe
   (owner-key (cdr (assoc "owner" (the query-toplevel) :test #'string-equal)))

   ;; a private session shows only to its owner's key
   (session (let* ((id (the session-id)) (replay (the replay-id))
                   (session (cond ((stringp id) (find-session id))
                                  ((stringp replay) (find-replay replay)))))
              (and session (visible-to? session (the owner-key)) session)))

   ;; only the owner's draws are metered -- a watcher's cost the owner nothing
   (owner-viewing? (let ((session (the session)))
                     (and session (owner? session (the owner-key)))))

   ;; root-object-type is not overridden here but SET at instantiation
   ;; (below): it is the sluice's own settable input, and on a Genworks
   ;; GDL workshop, where the sluice package is locked, redefining it
   ;; is a reserved-word error at load (2026-09-28).

   (empty-display-list-greeting
    (with-lhtml-string ()
      (:h2 :class "mb-4 text-center text-xl font-semibold" "Prompt lab viewer")
      (:p :class "mb-3"
          (str (cond ((null (the session)) "This session does not exist any more.")
                     ((null (the root-object-type)) "No model has been built in this session yet.")
                     (t "The model's tree is at the left; click a node to draw it here.")))))))

  :functions
  ((set-instantiation-time!
    ()
    (call-next-method)
    ;; the sluice opens on the session's MODEL, a symbol (a string
    ;; passes root-object-type-valid? and then fails make-object)
    (let ((session (the session)))
      (when (and session (model-defined? session))
        (the (set-slot! :root-object-type (model-symbol session)))))
    (the (draw-model!)))

   (draw-model!
    ()
    ;; drawing the model runs it: metered like a check (meter.lisp); a
    ;; refusal is not enforced here -- the page's doors already are
    (let ((session (the session)))
      (when (and session (the owner-viewing?))
        (ignore-errors (meter! session :run (model-volume session)))))
    ;; hidden lines removed by default: the wireframe reads as a solid
    ;; object rather than a cage -- up to *hidden-lines-max-leaves*;
    ;; removal is quadratic in the edges, so a larger model opens as the
    ;; plain wireframe and View > Hidden Lines turns removal back on
    (when (and (the root-object)
               (<= (or (ignore-errors (length (the root-object leaves))) 0)
                   *hidden-lines-max-leaves*))
      (ignore-errors (the viewport (set-slot! :hidden-lines :remove))))
    (when (the root-object)
      (ignore-errors (the viewport (draw-leaves! (the root-object))))))))


;;
;; Publishing.
;;

(defun door-path (name)
  (format nil "~a/api/~a" *url-prefix* name))

(defun publish-prompt-lab! (&key host)
  "Publish the page at *url-prefix*, its stylesheets and script under
<prefix>/static/, its manifest and service worker beside it, its doors
under <prefix>/api/ (config, session, state,
prompt, model, reload, topup, confirm, privacy; sessions, archive,
archived, replay; agent), the tools' door for an external agent at
<prefix>/mcp, and the viewer at <prefix>/viewer, on every server."
  (gwl:with-all-servers (server)
    (net.aserve:publish :path *url-prefix* :server server :host host :function #'page-door
                        :content-type "text/html; charset=utf-8")
    (net.aserve:publish-directory :prefix (format nil "~a/static/" *url-prefix*)
                                  :server server :host host
                                  :destination (namestring *static-directory*))
    ;; the sluice's tokens, skins and dividers, for the page (skins.lisp)
    (when (sluice-skins?)
      (net.aserve:publish-directory :prefix (format nil "~a/sluice-static/" *url-prefix*)
                                    :server server :host host
                                    :destination (format nil "~a/" sluice:*static*)))
    ;; the lab as an installable app (app.lisp)
    (net.aserve:publish :path (format nil "~a/manifest.webmanifest" *url-prefix*)
                        :server server :host host :function #'manifest-door)
    (net.aserve:publish :path (format nil "~a/worker" *url-prefix*)
                        :server server :host host :function #'worker-door)
    (net.aserve:publish :path (door-path "config") :server server :host host :function #'config-door)
    (net.aserve:publish :path (door-path "session") :server server :host host :function #'session-door)
    (net.aserve:publish :path (door-path "state") :server server :host host :function #'state-door)
    (net.aserve:publish :path (door-path "prompt") :server server :host host :function #'prompt-door)
    (net.aserve:publish :path (door-path "model") :server server :host host :function #'model-door)
    (net.aserve:publish :path (door-path "reload") :server server :host host :function #'reload-door)
    (net.aserve:publish :path (door-path "topup") :server server :host host :function #'topup-door)
    (net.aserve:publish :path (door-path "confirm") :server server :host host :function #'confirm-door)
    (net.aserve:publish :path (door-path "privacy") :server server :host host :function #'privacy-door)
    ;; browsing other sessions (browse.lisp)
    (net.aserve:publish :path (door-path "sessions") :server server :host host :function #'sessions-door)
    (net.aserve:publish :path (door-path "archive") :server server :host host :function #'archive-door)
    (net.aserve:publish :path (door-path "archived") :server server :host host :function #'archived-door)
    (net.aserve:publish :path (door-path "replay") :server server :host host :function #'replay-door)
    ;; the model as files (export.lisp)
    (net.aserve:publish :path (door-path "download") :server server :host host :function #'download-door)
    ;; an agent that runs elsewhere (external.lisp); shut unless *external-agent?*
    (net.aserve:publish :path (door-path "agent") :server server :host host :function #'agent-door)
    (net.aserve:publish :path (format nil "~a/mcp" *url-prefix*)
                        :server server :host host :function #'mcp-door)
    (publish-gwl-app (format nil "~a/viewer" *url-prefix*) 'viewer :server server :host host))
  (start-reaper!)
  *url-prefix*)
