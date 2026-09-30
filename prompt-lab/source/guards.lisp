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
