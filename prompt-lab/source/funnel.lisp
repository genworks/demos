;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; THE FUNNEL: what the lab can count of its own life, once a day, for
;; the operator -- author keys minted, sessions opened, prompts, builds
;; that finished and that stopped, archive visits and replays, uploads,
;; deployments, the authors by standing, the reviewer's recommendations,
;; the day's payments and the first one ever -- so a week of the lab
;; reads as one line a day instead of a listing to walk.
;;
;; The day's tally is kept in memory as events happen (funnel-count!,
;; archive.lisp: a lock and an increment, nothing more on a visitor's
;; request) and written to the shelf by this file's thread every minute.
;; The rest is read when the digest is made -- after *funnel-hour* UTC,
;; for the day before -- from the archive, the gate's author door, the
;; reviews, the deployment records and the books.  The digest goes out
;; through the gate's alert door as the reviewer's does and is kept as
;; <*funnel-root*>/funnel-<day>.json.  Off where *funnel-root* is nil.
;; Nothing here signals into a request, and nothing here holds a secret.
;;

(defparameter *funnel-root* nil
  "String or nil.  Where the tally and the daily digests are kept; nil: no
funnel.  One per room: each engine counts its own.")

(defparameter *funnel-hour* 5
  "Integer.  The UTC hour after which the day before's digest goes out.")

(defparameter *funnel-days* 90
  "Integer.  Days of the tally kept on the shelf.")

(defparameter *funnel-standing-keys* 60
  "Integer.  The most author keys asked of the gate's author door in one
digest: the keys seen in the archive's last 30 days, the newest first.")

(defvar *funnel-thread* nil)
(defvar *funnel-loaded?* nil "Whether the shelf's tally has been brought into memory.")

(defun funnel-on? () (and (stringp *funnel-root*) t))
(defun funnel-directory () (uiop:ensure-directory-pathname *funnel-root*))
(defun funnel-tally-file () (merge-pathnames "tally.json" (funnel-directory)))
(defun funnel-state-file () (merge-pathnames "state.json" (funnel-directory)))

;;
;; The tally, kept.
;;

(defun funnel-load-tally! ()
  "Bring the shelf's tally into memory once, at the start: the rows kept
there are added to what memory has counted since the room came up.
Never signals."
  (when (and (funnel-on?) (not *funnel-loaded?*))
    (setq *funnel-loaded?* t)
    (ignore-errors
     (let ((saved (author-review-read (funnel-tally-file))))
       (when (hash-table-p saved)
         (bt:with-lock-held (*funnel-tally-lock*)
           (maphash #'(lambda (day row)
                        (when (hash-table-p row)
                          (let ((mine (or (gethash day *funnel-tally*)
                                          (setf (gethash day *funnel-tally*) (make-hash-table :test #'equal)))))
                            (maphash #'(lambda (kind n) (when (realp n) (incf (gethash kind mine 0) n))) row))))
                    saved)))))))

(defun funnel-write-tally! ()
  "Write the tally to the shelf when it has changed, rows older than
*funnel-days* dropped.  Never signals."
  (when (and (funnel-on?) *funnel-tally-dirty?*)
    (ignore-errors
     (let ((copy (h))
           (oldest (day-key (- (get-universal-time) (* *funnel-days* 86400)))))
       (bt:with-lock-held (*funnel-tally-lock*)
         (setq *funnel-tally-dirty?* nil)
         (maphash #'(lambda (day row)
                      (if (string< day oldest)
                          (remhash day *funnel-tally*)
                          (setf (gethash day copy) (alexandria:copy-hash-table row))))
                  *funnel-tally*))
       (author-review-write copy (funnel-tally-file))))))

(defun funnel-tally-row (day)
  "The counts of DAY: a fresh hash table, kind -> count."
  (bt:with-lock-held (*funnel-tally-lock*)
    (let ((row (gethash day *funnel-tally*)))
      (if row (alexandria:copy-hash-table row) (make-hash-table :test #'equal)))))

;;
;; What the archive, the gate, the reviews, the deployments and the books
;; say of a day.
;;

(defun day-universal (day)
  "The universal time at which the UTC day DAY (YYYY-MM-DD) began, or nil."
  (ignore-errors
   (let ((y (parse-integer day :start 0 :end 4))
         (m (parse-integer day :start 5 :end 7))
         (d (parse-integer day :start 8 :end 10)))
     (encode-universal-time 0 0 0 d m y 0))))

(defun record-day (record)
  (day-key (or (gethash "created" record) 0)))

(defun record-prompts (record)
  (count-if #'(lambda (entry) (equal (second entry) "prompt")) (gethash "log" record)))

(defun funnel-archive-facts (day)
  "What the archive shows of DAY for this engine, and of the week and the
month ending with it: a hash table, with the author keys seen in the
month under \"keys_30d\" (the newest first)."
  (let* ((records (remove-if-not #'(lambda (r) (equal (gethash "engine" (cdr r)) (engine-name)))
                                 (archive-records-within 31)))
         (start (or (day-universal day) 0))
         (week-start (day-key (- start (* 6 86400))))
         (today (remove-if-not #'(lambda (r) (equal (record-day (cdr r)) day)) records))
         (week (remove-if-not #'(lambda (r) (let ((d (record-day (cdr r))))
                                              (and (string>= d week-start) (string<= d day))))
                              records))
         (keyed-week (remove-if-not #'(lambda (r) (wallet-id? (gethash "wallet" (cdr r)))) week))
         (days-by-key (make-hash-table :test #'equal))
         (keys-30d nil))
    (dolist (r keyed-week)
      (pushnew (record-day (cdr r)) (gethash (gethash "wallet" (cdr r)) days-by-key) :test #'string=))
    ;; the month's keys, the most recently used first
    (dolist (r (sort (remove-if-not #'(lambda (r) (wallet-id? (gethash "wallet" (cdr r)))) records)
                     #'> :key #'(lambda (r) (record-last-used (cdr r)))))
      (pushnew (gethash "wallet" (cdr r)) keys-30d :test #'string=))
    (setq keys-30d (nreverse keys-30d))
    (h "sessions" (length today)
       "with_model" (count-if #'(lambda (r) (probe-file (merge-pathnames "model.lisp" (car r)))) today)
       "apps" (count-if #'(lambda (r) (equal (gethash "kind" (cdr r)) "app")) today)
       "iterated" (count-if #'(lambda (r) (>= (record-prompts (cdr r)) 2)) today)
       "authored" (count-if #'(lambda (r) (wallet-id? (gethash "wallet" (cdr r)))) today)
       "authors_today" (length (remove-duplicates
                                (mapcar #'(lambda (r) (gethash "wallet" (cdr r)))
                                        (remove-if-not #'(lambda (r) (wallet-id? (gethash "wallet" (cdr r)))) today))
                                :test #'string=))
       "sessions_7d" (length week)
       "authors_7d" (hash-table-count days-by-key)
       "returning_7d" (let ((n 0)) (maphash #'(lambda (k days) (declare (ignore k)) (when (cdr days) (incf n))) days-by-key) n)
       "keys_30d" keys-30d)))

(defun funnel-standings (keys)
  "The gate's standing of each of KEYS (at most *funnel-standing-keys*),
tallied: standing -> count; \"unknown\" where the gate did not answer."
  (let ((tally (h)))
    (dolist (key (subseq keys 0 (min (length keys) *funnel-standing-keys*)) tally)
      (let* ((answer (ignore-errors (gate-post "author" (h "wallet" key))))
             (standing (or (and (hash-table-p answer) (gethash "standing" answer)) "unknown")))
        (incf (gethash (format nil "~(~a~)" standing) tally 0))))))

(defun funnel-reviews (day)
  "The reviewer's recommendations written on DAY (author-review.lisp):
recommendation -> count, \"held\" counted apart."
  (let ((tally (h)))
    (when *author-review-root*
      (dolist (file (ignore-errors (directory (merge-pathnames (format nil "*/~a.json" day) (author-review-directory)))))
        (let ((review (author-review-read file)))
          (when (hash-table-p review)
            (incf (gethash (if (gethash "held" review) "held" (or (gethash "recommendation" review) "stay")) tally 0))))))
    tally))

(defun funnel-payment-facts (lines day)
  "Of the book LINES, the real payments of DAY and the first ever: a plist
(:count :gross :first-day)."
  (let* ((payments (remove-if-not #'(lambda (line) (and (monocle:payment-line? line)
                                                         (not (truthy? (gethash "test" line)))))
                                  lines))
         (today (remove-if-not #'(lambda (line) (equal (day-key (or (gethash "time" line) 0)) day)) payments))
         (first (and payments (reduce #'min (mapcar #'(lambda (line) (or (gethash "time" line) 0)) payments)))))
    (list :count (length today)
          :gross (reduce #'+ (mapcar #'(lambda (line) (or (gethash "gross_rivets" line) (gethash "gross_cents" line) 0)) today))
          :first-day (and first (plusp first) (day-key first)))))

(defun funnel-money-facts (day)
  "Deployments live, the day's payments and the first ever: this lab's
own books and, where this room keeps the private front, the private
lab's."
  (let ((own (and *deployments?* (ignore-errors (deployment-records))))
        (own-lines (ignore-errors (monocle:revenue-lines (lab-house))))
        (private-count nil) (private-lines nil))
    (when *private-deployed-root*
      (ignore-errors
       (with-private-house
         (setq private-count (length (deployment-records))
               private-lines (monocle:revenue-lines (lab-house))))))
    (let ((own-pay (funnel-payment-facts own-lines day))
          (private-pay (funnel-payment-facts private-lines day)))
      (h "deployments" (length own)
         "private_deployments" private-count
         "payments" (+ (getf own-pay :count) (getf private-pay :count))
         "gross_rivets" (+ (getf own-pay :gross) (getf private-pay :gross))
         "first_payment_day" (let ((days (remove nil (list (getf own-pay :first-day) (getf private-pay :first-day)))))
                               (and days (reduce #'(lambda (a b) (if (string< a b) a b)) days)))))))

(defun funnel-private-facts (day)
  "The private front's sessions (private.lisp): requests filed on DAY, and
those up now.  Nil where this room keeps no front."
  (when *private-spool*
    (ignore-errors
     (let* ((requests (private-requests))
            (today (count-if #'(lambda (r) (equal (day-key (or (gethash "created" r) 0)) day)) requests))
            (up (count-if #'(lambda (r) (let ((a (private-answer (gethash "id" r))))
                                          (and a (equal (gethash "state" a) "up"))))
                          requests)))
       (h "private_sessions" today "private_up" up)))))

;;
;; The digest.
;;

(defun funnel-digest (day)
  "Everything the funnel knows of DAY, a hash table."
  (let* ((tally (funnel-tally-row day))
         (archive (funnel-archive-facts day))
         (standings (funnel-standings (gethash "keys_30d" archive)))
         (money (funnel-money-facts day))
         (private (funnel-private-facts day))
         (digest (h "day" day "lab" (lab-title) "engine" (engine-name) "time" (get-universal-time)
                    "tally" tally "archive" archive "standings" standings
                    "reviews" (funnel-reviews day) "money" money
                    "pot_credits" (let ((pot (ignore-errors (pot)))) (and pot (floor (or (getf pot :credits) 0)))))))
    (when private (setf (gethash "private" digest) private))
    (remhash "keys_30d" archive)
    digest))

(defun funnel-text (digest)
  "DIGEST as the operator reads it, within what the gate's alert door takes."
  (let ((tally (gethash "tally" digest)) (archive (gethash "archive" digest))
        (money (gethash "money" digest)) (private (gethash "private" digest)))
    (flet ((n (table key) (or (gethash key table) 0))
           (tally-text (table) (let (parts) (maphash #'(lambda (k v) (push (format nil "~a ~a" k v) parts)) table)
                                 (if parts (format nil "~{~a~^, ~}" (sort parts #'string<)) "none"))))
      (clip-line
       (format nil "~a, ~a (~a).~%keys minted ~d; sessions ~d (~d with a model, ~d apps, ~d iterated, ~d under a key by ~d authors); prompts ~d, builds done ~d, stopped ~d, tool errors ~d; archive visits ~d, replays ~d, uploads ~d.~%week: ~d sessions, ~d authors, ~d returning.  standings of ~d keys seen this month: ~a.~%reviews written: ~a.~%deployments ~d~@[ (private ~d)~]; payments ~d (~d rivets); first payment: ~a.~@[  private sessions ~d, up now ~d.~]~@[  pot ~:d.~]"
               (gethash "lab" digest) (gethash "day" digest) (gethash "engine" digest)
               (n tally "author-keys") (n archive "sessions") (n archive "with_model") (n archive "apps")
               (n archive "iterated") (n archive "authored") (n archive "authors_today")
               (n tally "prompts") (n tally "builds-done") (n tally "builds-stopped") (n tally "tool-errors")
               (n tally "archive-visits") (n tally "replays") (n tally "uploads")
               (n archive "sessions_7d") (n archive "authors_7d") (n archive "returning_7d")
               (let ((sum 0)) (maphash #'(lambda (k v) (declare (ignore k)) (incf sum v)) (gethash "standings" digest)) sum)
               (tally-text (gethash "standings" digest))
               (tally-text (gethash "reviews" digest))
               (n money "deployments") (gethash "private_deployments" money)
               (n money "payments") (round (n money "gross_rivets"))
               (or (gethash "first_payment_day" money) "none yet")
               (and private (n private "private_sessions")) (and private (n private "private_up"))
               (gethash "pot_credits" digest))
       1900))))

(defun funnel-report! (day &key (send? t))
  "The digest of DAY: made, kept on the shelf, and sent through the gate's
alert door unless SEND? is nil.  Values: the digest and its text."
  (let* ((digest (funnel-digest day))
         (text (funnel-text digest)))
    (setf (gethash "text" digest) text)
    (when (funnel-on?)
      (ignore-errors (author-review-write digest (merge-pathnames (format nil "funnel-~a.json" day) (funnel-directory)))))
    (when (and send? *messages-url*)
      (ignore-errors
       (gate-post "alert" (h "title" (format nil "public lab: funnel ~a" day)
                             "text" text "priority" "default" "tags" "bar_chart"))))
    (values digest text)))

(defun start-funnel! ()
  "The funnel's thread: every minute the tally is written when it changed,
and once a day, after *funnel-hour* UTC, the day before's digest goes out.
Nothing without *funnel-root*."
  (when (and (funnel-on?)
             (not (and *funnel-thread* (bt:thread-alive-p *funnel-thread*))))
    (funnel-load-tally!)
    (setq *funnel-thread*
          (bt:make-thread
           #'(lambda ()
               (do () (nil)
                 (ignore-errors
                  (funnel-write-tally!)
                  (multiple-value-bind (s m hour) (decode-universal-time (get-universal-time) 0)
                    (declare (ignore s m))
                    (let ((state (or (author-review-read (funnel-state-file)) (h))))
                      (when (and (>= hour *funnel-hour*)
                                 (not (equal (gethash "last_day" state) (day-key))))
                        (funnel-report! (day-key (- (get-universal-time) 86400)))
                        (setf (gethash "last_day" state) (day-key)
                              (gethash "last_time" state) (get-universal-time))
                        (author-review-write state (funnel-state-file))))))
                 (sleep 60)))
           :name "prompt-lab funnel"))))
