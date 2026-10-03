;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The lab's doors.  Those under <prefix>/api/ open or resume a
;; session, post prompts and answer the session's state (the log, the
;; token totals, the model file); they are plain aserve publishes with no
;; gwl session behind them, cheap and nothing to reap, and serve scripts
;; and agents (README.md).  The page is the sheet (prompt-lab-sheet),
;; published over the prefix by publish-lab-sheet!.  The viewer is a gwl
;; app: a sluice opened on the session's MODEL, dressed in the lab's skin.
;;


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
                           ;; what each amount buys; an older gate sends
                           ;; none, and a cent buys a credit
                           :credits-sold (gethash "topup_credits" json)
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
            "topup_credits" (or (getf pot :credits-sold) (getf pot :amounts) #())
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
       ;; what each amount buys (an older gate sends none: a cent a credit)
       "topup_credits" (or (and balance (or (gethash "topup_credits" balance)
                                            (gethash "topup_amounts" balance)))
                           #())
       "publishable_key" (or (and balance (gethash "publishable_key" balance)) ""))))

(defun page-url (req session &key (path ""))
  "The page's own public URL for SESSION, as the visitor's browser has
it: the scheme and host the proxies forwarded.  PATH follows the prefix
(\"/sheet\" for the sheet)."
  (flet ((header (value) (and (stringp value) (plusp (length value)) value)))
    (let ((host (or (header (net.aserve:header-slot-value req :x-cyclops-forwarded-host))
                    (header (net.aserve:header-slot-value req :host))
                    "localhost"))
          (proto (or (header (net.aserve:header-slot-value req :x-forwarded-proto)) "http")))
      (format nil "~a://~a~a~a?session=~a" proto host *url-prefix* path (session-id session)))))

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
       ;; what the session builds, and its web app's address once there
       ;; is one (kinds.lisp); a private session's opens with the key
       "kind" (kind-name (session-kind session))
       ;; opened closed-source (deploy.lisp)
       "closed" (if (session-closed? session) t 'yason:false)
       ;; whether what it built charges for anything yet: Monetize wants that
       "monetizable" (if (and (not (session-busy? session)) (ignore-errors (monetizable? session)))
                         t 'yason:false)
       "app_defined" (if (app-defined? session) t 'yason:false)
       "app_url" (and (app-defined? session)
                      (app-url session :owner-key (and owner? (session-private? session)
                                                       (session-owner session))))
       ;; what the session built, deployed for others to use (deploy.lisp)
       "deployment" (let ((record (and *deployments?* (session-deployment session))))
                      (and record (deployment-state record :owner? owner?)))
       "created" (epoch-seconds (session-created session))
       "spend" (and owner? (spend-state session))
       ;; the community pot, where the gate keeps one: everyone's to see
       "pot" (pot-state)
       "log" (log-vector (session-log session))
       "model_defined" (if (model-defined? session) t 'yason:false)
       "model_file" (and owner? (namestring (session-model-file session)))
       "model_source" (model-body session)
       ;; the visitor's uploaded files, as public as the session (uploads.lisp)
       "files" (files-state (session-files session) (session-id session))
       "uploads" (and owner? (uploads-state session))
       ;; whether the uploaded drawing wants the solids lab (routing.lisp)
       "routing" (routing-state session)
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
                           ;; what a prompt may ask to have built, the default first (kinds.lisp)
                           "kinds" (kinds-state)
                           "sibling_url" (car *sibling-lab*)
                           "sibling_label" (cdr *sibling-lab*)
                           "browsing" (if *browsing?* t 'yason:false)
                           ;; the files the model may be had as (export.lisp)
                           "downloads" (coerce (downloads-state) 'vector)
                           ;; the caps on uploaded files, nil where none are taken (uploads.lisp)
                           "uploads" (uploads-state)
                           ;; the community pot, where the gate keeps one
                           "pot" (pot-state))))

(defun open-session! (address &optional wallet &key closed?)
  "Open a session for a visitor at ADDRESS, naming WALLET if any; CLOSED?
opens it closed-source (deploy.lisp), where the lab allows.  Values:
the session, or nil, the reason and the response to refuse with.  One
address opens at most *max-sessions-per-address* a day, unless its wallet
has credit or has put more into the pot than it has drawn."
  (if (and (address-over-limit? address :sessions)
           (not (and (wallet-id? wallet)
                     (let ((balance (ignore-errors (gate-post "balance" (h "wallet" wallet)))))
                       (and balance
                            (flet ((credit? (name)
                                     (let ((value (gethash name balance)))
                                       (and (realp value) (plusp value)))))
                              (or (credit? "credits_cents") (credit? "contribution_left_cents"))))))))
      (values nil
              (format nil "This address has opened its ~a free sessions for today.  Come back tomorrow, or bring your own agent."
                      *max-sessions-per-address*)
              *response-too-many-requests*)
      (let ((session (make-session :address address :wallet (and (wallet-id? wallet) wallet))))
        (count-address! address :sessions)
        (log-event session :note "Session ~a opened.  Describe what to build." (session-id session))
        (when (and closed? *closed-source?* *deployments?*)
          (close-session-source! session)
          (log-event session :note "This session is closed-source: nobody else sees it, and what you deploy from it (Monetize) does not serve its source.  If it ends without a deployment its source becomes public, under the GNU Affero General Public License, like any other session's."))
        (refresh-balance! session)
        (save-session! session)
        session)))

(defun begin-prompt! (session prompt address token &key (route? t) kind)
  "Start the agent on PROMPT in SESSION for a visitor at ADDRESS, after
every check the prompt door makes (the owner's excepted: the caller's).
TOKEN is the human check's, nil for a script's prompt.  KIND, when it
names one this lab offers, is what the session builds from this prompt
on (kinds.lisp); without it the session keeps its kind.  Values: :started
and the lane, or nil, the reason and the response to refuse with.  With
ROUTE?, a session's first prompt that wants the sibling lab's engine is
not started here (routing.lisp): nil, the reason, a 409 and, a fourth
value, where it belongs -- a plist of :engine, :reason and :url."
  (cond ((not (and (stringp prompt)
                   (plusp (length (string-trim '(#\space #\tab #\newline #\return) prompt)))))
         (values nil "Say what to build." net.aserve:*response-bad-request*))
        ((> (length prompt) *max-prompt-length*)
         (values nil (format nil "A prompt may have ~a characters at most." *max-prompt-length*)
                 net.aserve:*response-bad-request*))
        ;; a community pot with nothing in it builds for nobody
        ((pot-empty?)
         (values nil (unit-text "The community pot of {units} is empty.  Top it up and the lab builds again, for everyone.")
                 *response-too-many-requests*))
        ((and (free-caps? session) (>= (prompts-used session) *max-prompts-per-session*))
         (values nil (format nil "This session has used its ~a ~:[free ~;~]prompts.  ~a to keep going here, take a copy of the model file, or start a new session."
                             *max-prompts-per-session* (pot)
                             (unit-text (if (pot) "Add {units} to the pot" "Buy {units}")))
                 net.aserve:*response-bad-request*))
        ((and (free-caps? session) (address-over-limit? address :prompts))
         (values nil (format nil "This address has run its ~a ~:[free ~;~]prompts for today.  ~a to keep going, come back tomorrow, or bring your own agent."
                             *max-prompts-per-address* (pot)
                             (unit-text (if (pot) "Add {units} to the pot" "Buy {units}")))
                 *response-too-many-requests*))
        ((session-busy? session)
         (values nil "Still working on the previous request." net.aserve:*response-bad-request*))
        (t
         ;; the token is single-use and the check is a network call:
         ;; last, after every cheap refusal.  A prompt with no token at
         ;; all is a script's or an agent's, and rides the automated
         ;; lane's allowance when there is one (guards.lisp).
         (multiple-value-bind (ok? reason lane) (admit-prompt token address)
           (cond ((not ok?)
                  (values nil reason (if (and (automated-lane?) (null token))
                                         *response-too-many-requests*
                                         net.aserve:*response-forbidden*)))
                 ;; first of all, which engine the request wants: one that
                 ;; wants the sibling lab's is sent there (routing.lisp)
                 ((let ((route (and route? (route-prompt session prompt))))
                    (when route
                      (return-from begin-prompt!
                        (values nil (getf route :reason) *response-conflict* route)))))
                 ((not (start-prompt! session (string-trim '(#\space #\tab #\newline #\return) prompt)
                                      :kind (parse-kind kind)))
                  (values nil "Still working on the previous request." net.aserve:*response-bad-request*))
                 (t (count-address! address :prompts)
                    (when (eq lane :automated)
                      (count-automated! address)
                      (log-event session :note "This prompt came without the human check: an automated one, on the lab's daily allowance for those."))
                    (values :started lane)))))))

(defun session-door (req ent)
  "POST <prefix>/api/session {wallet?}: open a session; answers its id.
One address opens at most *max-sessions-per-address* a day."
  (let* ((address (client-address req))
         (json (request-json req))
         (wallet (and json (gethash "wallet" json))))
    (multiple-value-bind (session reason response)
        (open-session! address wallet :closed? (and json (eq (gethash "closed" json) t)))
      (if (null session)
          (refuse req ent response "~a" reason)
          ;; the owner key goes to this browser once, here, and nowhere else
          (respond-json req ent (h "session" (session-id session)
                                   "owner" (session-owner session)
                                   "spend" (spend-state session)
                                   "pot" (pot-state)))))))

(defvar *topup-checked* (make-hash-table :test #'equal)
  "session id -> universal time its last top-up passed the human check.")

(defparameter *topup-check-seconds* 300
  "Integer. How long a passed human check covers the session's next top-up
request: the card line first, then 'more payment options' if the payer
wants them, is one purchase and one check.")

(defun topup-check! (session token address)
  "Whether a purchase in SESSION passes the human check: a check passed for
the session in the last *topup-check-seconds* covers it (the card line,
then 'more payment options', is one purchase), else TOKEN is verified --
once, a token is single-use.  Values: true, or nil and the reason."
  (let ((checked (gethash (session-id session) *topup-checked*)))
    (if (and checked (< (- (get-universal-time) checked) *topup-check-seconds*))
        t
        (multiple-value-bind (ok? reason) (verify-turnstile token address)
          (cond (ok?
                 (let ((now (get-universal-time)))
                   ;; the old ones go as a new one comes: the table stays small
                   (maphash #'(lambda (id at) (when (> (- now at) *topup-check-seconds*)
                                                (remhash id *topup-checked*)))
                            *topup-checked*)
                   (setf (gethash (session-id session) *topup-checked*) now))
                 t)
                (t (values nil (or reason "Complete the human check first."))))))))

(defun begin-topup! (session amount return-url &key embedded? flow)
  "Ask the gate for a payment of AMOUNT cents in SESSION, coming back to
RETURN-URL.  Values: the gate's answer (a hash table: url for Stripe's
hosted page, or client_secret for a form in the page, and the wallet), or
nil and the reason."
  (multiple-value-bind (answer status)
      (gate-post "topup" (h "wallet" (session-wallet session)
                            "amount_cents" amount
                            "success_url" return-url
                            "cancel_url" (concatenate 'string return-url "&topup=cancelled")
                            "embedded" (if embedded? t 'yason:false)
                            "flow" flow))
    (let ((checkout-url (and answer (gethash "url" answer)))
          (client-secret (and answer (gethash "client_secret" answer)))
          (wallet (and answer (gethash "wallet" answer))))
      (cond ((and (eql status 200) (or (stringp checkout-url) (stringp client-secret)))
             (when (wallet-id? wallet) (setf (session-wallet session) wallet))
             (log-event session :note (if (pot)
                                          (unit-text "Adding ~:d {units} to the community pot.  They arrive when the payment completes.")
                                          (unit-text "Buying ~:d {units}.  They arrive when the payment completes."))
                        (credits-for-amount amount))
             answer)
            (t (values nil (or (ignore-errors (gethash "message" (gethash "error" answer)))
                               "The top-up could not be started; try again in a moment.")))))))

(defun topup-door (req ent)
  "POST <prefix>/api/topup {session, amount_cents, embedded?, flow?}: a
payment through the gate.  Answers {url, wallet} for Stripe's hosted
page, {client_secret, publishable_key, wallet} for the in-page form, and
with flow \"card\" (a gate that knows it) {flow \"card\", client_secret,
checkout, publishable_key, wallet} for Stripe's one-line card control."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (amount (and json (gethash "amount_cents" json)))
         (embedded? (and json (eq (gethash "embedded" json) t)))
         (flow (and json (equal (gethash "flow" json) "card") "card")))
    (cond ((null session) (no-such-session req ent))
          ((not (owner-request? req session json)) (not-yours req ent))
          ((not (integerp amount)) (refuse req ent "Say how much."))
          (t
           ;; a public Checkout for small amounts draws card testers: a
           ;; fresh Turnstile token before every purchase, as before every
           ;; prompt
           (multiple-value-bind (ok? reason)
               (topup-check! session (gethash "turnstile" json) (client-address req))
             (if (not ok?)
                 (refuse req ent net.aserve:*response-forbidden* "~a" reason)
                 (multiple-value-bind (answer reason)
                     (begin-topup! session amount (page-url req session) :embedded? embedded? :flow flow)
                   (if (null answer)
                       (refuse req ent net.aserve:*response-service-unavailable* "~a" reason)
                       (respond-json req ent (h "url" (gethash "url" answer)
                                                "client_secret" (gethash "client_secret" answer)
                                                ;; a gate that made a PaymentIntent says so
                                                "flow" (gethash "flow" answer)
                                                "checkout" (gethash "checkout" answer)
                                                "publishable_key" (gethash "publishable_key" (spend-state session))
                                                "wallet" (session-wallet session)))))))))))

(defun credits-for-amount (cents)
  "The credits CENTS buys, as the gate last said (its offers may sell more
credits than cents); CENTS itself from a gate that said nothing."
  (let* ((pot (pot))
         (amounts (coerce (or (getf pot :amounts) #()) 'list))
         (sold (coerce (or (getf pot :credits-sold) #()) 'list))
         (at (position cents amounts)))
    (or (and at (realp (nth at sold)) (nth at sold)) cents)))

(defun format-cents (cents)
  (if (and (realp cents) (>= cents 100))
      (format nil "$~,2f" (/ cents 100))
      (format nil "~,1f cents" (or cents 0))))

(defun confirm-topup! (session wallet checkout)
  "The visitor is back from paying: have the gate credit CHECKOUT on WALLET
once.  Values: the outcome (\"credited\", \"already\", ...) and the gate's text."
  (setf (session-wallet session) wallet)
  (multiple-value-bind (answer status)
      (gate-post "confirm" (h "wallet" wallet "checkout" checkout "session" (session-id session)))
    (declare (ignore status))
    (when answer (note-balance session answer))
    (let ((outcome (and answer (gethash "outcome" answer))))
      (when (equal outcome "credited")
        (if *pot*
            (log-event session :note (unit-text "Thank you: the community pot holds ~:d {units} now, for everyone's builds.")
                       (max 0 (floor (or (getf *pot* :credits) 0))))
            (log-event session :note (unit-text "{Units} added: ~:d on your balance.  Builds beyond the free {units} draw on it.")
                       (round (or (session-credits session) 0)))))
      (save-session! session)
      (values (or outcome "failed")
              (or (and answer (gethash "text" answer)) "The gate did not answer.")))))

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
          (t (multiple-value-bind (outcome text) (confirm-topup! session wallet checkout)
               (respond-json req ent (h "outcome" outcome
                                        "text" text
                                        "spend" (spend-state session)
                                        "pot" (pot-state))))))))

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

(defun set-privacy! (session private?)
  "The owner closes SESSION to watchers (PRIVATE? true) -- out of the
listings, live and archived, its URL answering no one else -- or opens it
again.  Closing takes a session that has bought credits.  A session opened
before owner keys gets one, since the address no longer suffices once
nobody else may look.  Values: true, or nil and the reason."
  (cond ((and private? (not (paid? session)))
         (values nil (unit-text "A session becomes private once it has bought {units}.")))
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
           t)))

(defun privacy-door (req ent)
  "POST <prefix>/api/privacy {session, private}: the owner closes the
session to watchers or opens it again (set-privacy!).  Answers the owner
key too, which a session older than keys gets here."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (private? (and json (eq (gethash "private" json) t))))
    (cond ((null session) (no-such-session req ent))
          ((not (owner-request? req session json)) (not-yours req ent))
          (t (multiple-value-bind (ok? reason) (set-privacy! session private?)
               (if ok?
                   (respond-json req ent (h "private" (if private? t 'yason:false)
                                            "owner" (session-owner session)))
                   (refuse req ent "~a" reason)))))))

(defun prompt-door (req ent)
  "POST <prefix>/api/prompt {session, prompt}: start the agent on the
prompt in a thread of its own; the page follows along through the state door."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (address (client-address req)))
    (cond ((null session) (no-such-session req ent))
          ((not (owner? session (request-owner-key req json) address)) (not-yours req ent))
          (t (multiple-value-bind (started reason response route)
                 (begin-prompt! session (gethash "prompt" json) address (gethash "turnstile" json)
                                ;; "stay": true builds here whatever engine the prompt wants
                                :route? (not (eq (gethash "stay" json) t))
                                ;; "kind": "app" builds a web app, "model" a model
                                :kind (gethash "kind" json))
               (cond (started
                      (respond-json req ent (h "started" t "automated" (if (eq reason :automated) t 'yason:false))
                                    net.aserve:*response-accepted*))
                     ;; the prompt belongs in the sibling lab (routing.lisp)
                     (route
                      (respond-json req ent (h "error" reason
                                               "route" (h "engine" (string-downcase (getf route :engine))
                                                          "url" (getf route :url)))
                                    response))
                     (t (refuse req ent response "~a" reason))))))))

(defun model-door (req ent)
  "POST <prefix>/api/model {session, source}: the visitor's own edit of the
model file, written, compiled and loaded like the agent's."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (source (and json (gethash "source" json))))
    (cond ((null session) (no-such-session req ent))
          ((not (owner-request? req session json)) (not-yours req ent))
          ((not (stringp source)) (refuse req ent "No source given."))
          ;; claimed, not merely checked: a prompt starting in the gap
          ;; would compile the same model file at the same time
          ((not (claim! session)) (refuse req ent "Wait for the agent to finish first."))
          (t (multiple-value-bind (blocks error?)
                 (unwind-protect (write-model (touch session) source)
                   (setf (session-busy? session) nil))
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
          ((not (probe-file (session-model-file session))) (refuse req ent "There is no model file yet."))
          ((not (claim! session)) (refuse req ent "Wait for the agent to finish first."))
          (t (multiple-value-bind (blocks error?)
                 (unwind-protect (load-model-file (touch session))
                   (setf (session-busy? session) nil))
               (let ((text (or (cdr (assoc "text" (first blocks) :test #'string=)) "")))
                 (log-event session :reload "Reloaded from disk: ~a" text)
                 (save-session! session)
                 (respond-json req ent (h "ok" (if error? 'yason:false t) "text" text))))))))


;;
;; The viewer: a sluice opened on the session's MODEL, its leaves drawn
;; as the page opens.
;;

;; session-control-mixin: the page opens a new viewer at every build and
;; every page load, each holding the model's whole tree, and without the
;; mixin none of them ever expired.  A sluice that carries the mixin
;; itself (gendl, 2026-10-01) gets it from there; named here too, after
;; the sluice, for an image whose sluice does not.
(define-object viewer (sluice:assembly session-control-mixin)

  :documentation
  (:description "A sluice opened on one prompt-lab session's MODEL, reached
at <prefix>/viewer?session=<id>.  The model's leaves are drawn as the page
opens; the tree, the menus and the headset button are the sluice's own.
It wears the page's skin (skin=<name>), and on a phone (mode=phone) it is
the model with one panel under it, the inputs or the tree."
   :author "Genworks International")

  :computed-slots
  ((title "Prompt lab viewer")

   ;; the sluice as the public gets it: no File or Develop menu, and
   ;; gdlAjax answers only what its menus, tree and panes call (a sluice
   ;; that knows the audience makes File > Open refuse too)
   (audience :public)

   ;; File > Open evaluates what is typed, in the image every visitor
   ;; shares: never offered here (said again for a sluice older than the
   ;; audience)
   (open-from-expression? nil)

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
   ;; of the sluice are overridden here: its computed slots, body-class
   ;; among them, are reserved words to a subclass in another package
   ;; (the sluice is in *packages-to-lock*, GDL's reserved-word check).
   (additional-css-links (viewer-css-links (the page-skin) :phone? (the phone?)))

   ;; on a phone, and for a deployed model, the inspector holds the inputs alone
   (user-mode?-default (or (the phone?) (stringp (the deployed-name))))

   (session-id (cdr (assoc "session" (the query-toplevel) :test #'string-equal)))

   ;; ?replay=<id> instead: an archived session's model, compiled again
   ;; by the replay door (browse.lisp)
   (replay-id (cdr (assoc "replay" (the query-toplevel) :test #'string-equal)))

   ;; the owner's page names its key (owner=) on the iframe
   (owner-key (cdr (assoc "owner" (the query-toplevel) :test #'string-equal)))

   ;; ?deployed=<name> instead: a deployed model (deploy.lisp)
   (deployed-name (cdr (assoc "deployed" (the query-toplevel) :test #'string-equal)))

   ;; A deployed model's wrapper: what it is, and its files to download,
   ;; in a tile at the left; the inspector holds its inputs alone.  (An
   ;; INPUT of the sluice; nil for a session's or a replay's viewer.)
   (tiles (when (and (stringp (the deployed-name)) (the session))
            (list (list :object (the deployed-tile) :place :left :tab "About"))))

   ;; a private session shows only to its owner's key
   (session (let* ((id (the session-id)) (replay (the replay-id)) (deployed (the deployed-name))
                   (session (cond ((stringp id) (find-session id))
                                  ((stringp replay) (find-replay replay))
                                  ((stringp deployed) (deployed-for-viewer deployed)))))
              (and session (or (stringp deployed) (visible-to? session (the owner-key))) session)))

   ;; only the owner's draws are metered -- a watcher's cost the owner nothing
   (owner-viewing? (let ((session (the session)))
                     (and session (owner? session (the owner-key)))))

   ;; root-object-type is not overridden here but SET at instantiation
   ;; (below): it is a settable computed slot of the sluice's, and
   ;; naming it here is a reserved-word error at load (2026-09-28).

   (empty-display-list-greeting
    (with-lhtml-string ()
      (:h2 :class "mb-4 text-center text-xl font-semibold" "Prompt lab viewer")
      (:p :class "mb-3"
          (str (cond ((null (the session)) "This session does not exist any more.")
                     ((null (the root-object-type)) "No model has been built in this session yet.")
                     (t "The model's tree is at the left; click a node to draw it here.")))))))

  :objects
  ((deployed-tile
    :type 'base-html-div
    :inner-html (let* ((name (the deployed-name))
                       (record (and (stringp name) (deployment-record name)))
                       (key (and record (deployment-priced? record) (the owner-key))))
                  (with-lhtml-string ()
                    (when record
                      (htm (:div :style "padding:.7rem;font-size:.9rem;line-height:1.45"
                                 (:h2 :style "font-size:1.05rem;margin:0 0 .4rem" (esc (gethash "title" record)))
                                 (let ((blurb (gethash "blurb" record)))
                                   (when (plusp (length blurb)) (htm (:p :style "margin:0 0 .6rem" (esc blurb)))))
                                 (:p :style "margin:0 0 .6rem"
                                     "Change the inputs in the inspector and the model follows.")
                                 (:p :style "margin:0 0 .3rem;font-weight:600" "Download")
                                 (:p :style "margin:0 0 .6rem"
                                     (dolist (entry (download-formats))
                                       (htm (:a :style "display:inline-block;margin:0 .5rem .3rem 0"
                                                :href (format nil "~a?deployed=~a&format=~a~@[&owner=~a~]"
                                                              (door-path "download") name (first entry) key)
                                                (esc (fourth entry))
                                                ;; what the author charges for this one
                                                (let ((cents (deployment-file-price record (first entry))))
                                                  (when cents (fmt " ($~,2f)" (/ cents 100))))))))
                                 (unless (truthy? (gethash "closed" record))
                                   (htm (:p :style "margin:0 0 .6rem"
                                            (:a :href (format nil "~a/source" (deployment-url name)) "Its source")
                                            ", under the GNU Affero General Public License.")))
                                 (:p :style "margin:0;opacity:.7"
                                     "Built in the " (:a :href *url-prefix* (esc (lab-title))) "."))))))))

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
    ;; plain wireframe and a pane's View > Hidden lines turns removal back on
    (when (and (the root-object)
               (<= (or (ignore-errors (length (the root-object leaves))) 0)
                   *hidden-lines-max-leaves*))
      (ignore-errors (the viewport (set-slot! :hidden-lines :remove))))
    (when (the root-object)
      (ignore-errors (the viewport (draw-leaves! (the root-object))))))))


;;
;; The web app (kinds.lisp): the session's APP, a page of its own.
;;

(defun app-door (req ent)
  "GET <prefix>/app?session=<id> (or replay=<id>, an archived session's):
an instance of the session's APP, made for this visitor and answered as
gwl answers any of its pages, with a redirect to the instance.  Whoever
may see the session may open its app; a private session's wants owner=."
  (let* ((id (query-value req "session"))
         (replay (query-value req "replay"))
         (session (cond ((stringp id) (find-session id))
                        ((stringp replay) (find-replay replay)))))
    (cond ((or (null session) (not (visible-to? session (query-value req "owner"))))
           (no-such-session req ent))
          ((not (app-defined? session))
           (refuse req ent "No web app has been built in this session yet."))
          (t (gwl-make-object req ent (format nil "~s" (app-symbol session)))))))

(defparameter *demos-css-directory*
  ;; beside the lab in the demos tree: <demos>/css/, the demos' shared stylesheet
  (make-pathname :name nil :type nil :version nil
                 :directory (append (butlast (pathname-directory *static-directory*) 2) (list "css"))
                 :defaults *static-directory*)
  "Pathname. The directory of the demos' compiled stylesheet, which a web
app's page wears (web-app, kinds.lisp).")


;;
;; A page instance that is gone.  The lab's page, its viewer and a web
;; app are gwl page instances at /sessions/<instance>/..., one per visit,
;; cleared when idle and lost at a restart; what they show -- a session,
;; an archived one, a deployment -- outlives them and is named in the
;; address's query.  gwl asks, for a /sessions/ address whose instance is
;; gone, where the visitor should go instead: here, to the address that
;; opens a fresh instance on the same thing.
;;

(defun lab-session-recovery (req)
  "Where a dead /sessions/ address belongs when its query names something
of this lab's: a deployment, a session (live, or by now in the archive) or
an archived session.  Nil when it names nothing the lab knows -- another
application's address."
  (let ((query (net.aserve:request-query req)))
    (flet ((named (name)
             (let ((value (cdr (assoc name query :test #'string-equal))))
               (and (stringp value) (plusp (length value)) value))))
      (let ((session (named "session"))
            (archive (or (named "archive") (named "replay")))
            (deployed (or (named "d") (named "deployed"))))
        (cond ((and deployed *deployments?* (deployment-record deployed))
               (deployment-url deployed))
              ((and session (session-id? session) (find-session session))
               (format nil "~a?session=~a" *url-prefix* session))
              ;; a session that has ended since: its archive entry
              ((and (or session archive) (session-id? (or session archive)) *browsing?*
                    (archived-directory (or session archive)))
               (format nil "~a?archive=~a" *url-prefix* (or session archive))))))))

(defun register-session-recovery! ()
  "Have gwl ask the lab about dead /sessions/ addresses, where this image's
gwl asks anyone (an older one sends them all to the site's front page)."
  (let ((hooks (find-symbol (string '#:*unknown-session-recoveries*) :gwl)))
    (when (and hooks (boundp hooks))
      (pushnew 'lab-session-recovery (symbol-value hooks)))))


;;
;; Publishing.
;;

(defun door-path (name)
  (format nil "~a/api/~a" *url-prefix* name))

(defun publish-prompt-lab! (&key host)
  "Publish the lab's stylesheets, editor and icons under <prefix>/static/,
its manifest and retiring service worker beside the prefix, its doors
under <prefix>/api/ (config, session, state,
prompt, model, reload, topup, confirm, privacy; sessions, archive,
archived, replay; upload, file; agent), the tools' door for an external agent at
<prefix>/mcp, a session's web app at <prefix>/app, and the viewer at
<prefix>/viewer, on every server.  The
page itself is the sheet's (publish-lab-sheet!, prompt-lab-sheet)."
  (gwl:with-all-servers (server)
    ;; THE PAGE IS THE SHEET (prompt-lab-sheet, publish-lab-sheet!): it
    ;; answers at the prefix.  The classic page was retired on 2026-10-02;
    ;; its doors stay, the API scripts and agents speak.
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
    ;; the archive's thumbnails (thumbs.lisp)
    (net.aserve:publish :path (door-path "thumb") :server server :host host :function #'thumb-door)
    ;; the model as files (export.lisp)
    (net.aserve:publish :path (door-path "download") :server server :host host :function #'download-door)
    ;; the visitor's files (uploads.lisp)
    (net.aserve:publish :path (door-path "upload") :server server :host host :function #'upload-door)
    (net.aserve:publish :path (door-path "file") :server server :host host :function #'file-door)
    ;; an agent that runs elsewhere (external.lisp); shut unless *external-agent?*
    (net.aserve:publish :path (door-path "agent") :server server :host host :function #'agent-door)
    (net.aserve:publish :path (format nil "~a/mcp" *url-prefix*)
                        :server server :host host :function #'mcp-door)
    ;; Monetize: what a session built, deployed for others to use (deploy.lisp)
    (net.aserve:publish :path (door-path "deploy") :server server :host host :function #'deploy-door)
    (net.aserve:publish :path (door-path "undeploy") :server server :host host :function #'undeploy-door)
    (net.aserve:publish :path (door-path "deployments") :server server :host host :function #'deployments-door)
    (net.aserve:publish :path (door-path "earnings") :server server :host host :function #'earnings-door)
    (net.aserve:publish-prefix :prefix (format nil "~a/d/" *url-prefix*)
                               :server server :host host :function #'deployed-door)
    (net.aserve:publish :path (format nil "~a/app-file" *url-prefix*)
                        :server server :host host :function #'app-file-door)
    ;; a session's web app and the stylesheet it wears (kinds.lisp)
    (net.aserve:publish :path (format nil "~a/app" *url-prefix*)
                        :server server :host host :function #'app-door)
    (net.aserve:publish-directory :prefix (format nil "~a/app-static/demo/css/" *url-prefix*)
                                  :server server :host host
                                  :destination (namestring *demos-css-directory*))
    (publish-gwl-app (format nil "~a/viewer" *url-prefix*) 'viewer :server server :host host))
  (register-session-recovery!)
  (start-reaper!)
  (start-thumbnailer!)
  *url-prefix*)
