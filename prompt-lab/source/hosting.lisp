;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; Hosting a project: from a GitLab project to <name>.<*hosting-domain*>,
;; served by an apps host (Monocle's house: its kit, its gate, its
;; books).  What goes out is the project's default branch at a pinned
;; commit, never a session's working files, so what runs is what anyone
;; can read.
;;
;; A REQUEST is one try at hosting one commit, kept as a JSON file under
;; *hosting-root*.  Its states:
;;
;;   queued      waiting for the apps host's worker
;;   checking    the worker builds and asks it (Monocle's house/check)
;;   reviewing   the check passed; the model reads it (review.lisp)
;;   approved    the review passed, or a person released it
;;   held        the review flagged it; a person releases or rejects it
;;   deploying   the worker puts it on the apps host
;;   live        it answers at its address
;;   failed      the check or the review failed it
;;   rejected    a person rejected it after a flag
;;   error       the worker could not put it up
;;
;; THE APPS HOST ASKS; the lab never calls it.  Its worker polls
;; <prefix>/api/hosting/next with the shared secret (*hosting-secret*)
;; and reports at <prefix>/api/hosting/report.  Without a secret both
;; doors answer 404.
;;
;; A NAME is first come: the first project to pass the check under the
;; name its monetize.sexp gives keeps it, and only that project may be
;; hosted under it again.
;;
;; What a deployment sets (the secret, the root, the domain) is DEFVAR,
;; so that loading this file again into a running lab keeps it; the
;; tuning (*hosting-claim-seconds*) is DEFPARAMETER, which a reload
;; brings back to the file's value.
;;

(defvar *hosting-secret* nil
  "String or nil. The secret the apps host's worker sends in
X-Hosting-Secret.  Nil shuts the worker's doors.")

(defvar *hosting-root*
  (namestring (merge-pathnames "prompt-lab-hosting/"
                               (uiop:pathname-parent-directory-pathname (pathname *workspace-root*))))
  "String. Where the requests (requests/<id>.json), the pages the check
kept (pages/<id>/) and the names (names.json) are kept.")

(defvar *hosting-domain* "common-lisp.app"
  "String. The domain an application is hosted under, as <name>.<domain>.")

(defparameter *hosting-claim-seconds* 1800
  "Integer. A request the worker took and has not reported on for this
long is offered again.")

(defvar *hosting-lock* (bt:make-lock "prompt-lab hosting"))

;;
;; The records.
;;

(defun hosting-path (control &rest arguments)
  (merge-pathnames (apply #'format nil control arguments) (uiop:ensure-directory-pathname *hosting-root*)))

(defun read-json-file (file)
  (ignore-errors
   (with-open-file (in file :external-format :utf-8)
     (yason:parse in))))

(defun write-json-file (file object)
  "Write OBJECT to FILE whole: to a file beside it, then renamed over it."
  (ensure-directories-exist file)
  (let ((temporary (make-pathname :type "tmp" :defaults file)))
    (with-open-file (out temporary :direction :output :if-exists :supersede :external-format :utf-8)
      (yason:encode object out))
    (uiop:rename-file-overwriting-target temporary file)))

(defun hosting-id? (id)
  (and (stringp id) (= (length id) 12) (every #'(lambda (c) (digit-char-p c 16)) id)))

(defun hosting-request (id)
  (and (hosting-id? id) (read-json-file (hosting-path "requests/~a.json" id))))

(defun save-hosting-request! (request)
  (write-json-file (hosting-path "requests/~a.json" (gethash "id" request)) request)
  request)

(defun hosting-requests ()
  "Every request, the oldest first."
  (sort (remove nil (mapcar #'read-json-file (directory (hosting-path "requests/*.json"))))
        #'< :key #'(lambda (request) (or (gethash "made" request) 0))))

(defun session-hosting-request (session-id)
  "The latest request asked from the lab session SESSION-ID, or nil."
  (find session-id (reverse (hosting-requests)) :key #'(lambda (request) (gethash "session" request)) :test #'equal))

(defparameter *hosting-state-texts*
  '(("queued" . "waiting for the apps host")
    ("checking" . "being built and checked")
    ("reviewing" . "being read")
    ("approved" . "passed, going up")
    ("held" . "held for a person to look at")
    ("deploying" . "going up")
    ("live" . "live")
    ("failed" . "not hosted")
    ("rejected" . "not hosted: rejected by a person")
    ("error" . "could not be put up"))
  "What the page calls each state of a request.")

(defun hosting-state-text (state)
  (or (cdr (assoc state *hosting-state-texts* :test #'equal)) state))

(defun hosting-request-reasons (request)
  "The lines that say why REQUEST did not go up, or is held: the check's
FAIL lines, the review's summary and findings, the apps host's last
words.  Nil for a request that is on its way or up."
  (let ((state (gethash "state" request))
        (check (gethash "check" request))
        (review (gethash "review" request)))
    (when (member state '("failed" "held" "rejected" "error") :test #'equal)
      (append
       (when (hash-table-p check)
         (remove-if-not #'(lambda (line) (and (stringp line) (> (length line) 4) (string= "FAIL" line :end2 4)))
                        (coerce (or (gethash "lines" check) #()) 'list)))
       (when (hash-table-p review)
         (cons (gethash "summary" review)
               (map 'list #'(lambda (finding)
                              (format nil "~a: ~a" (gethash "where" finding) (gethash "detail" finding)))
                    (or (gethash "findings" review) #()))))
       (let ((last (car (last (gethash "history" request)))))
         (when (and (equal state "error") (hash-table-p last))
           (list (gethash "note" last))))))))

(defun hosting-state! (request state &optional control &rest arguments)
  "Move REQUEST to STATE, noting why in its history, and save it.  The
lab session it was asked from hears of it in its log, so its page shows
the step as it happens."
  (let ((note (and control (apply #'format nil control arguments))))
    (setf (gethash "state" request) state
          (gethash "changed" request) (epoch-seconds (get-universal-time))
          (gethash "history" request)
          (append (gethash "history" request)
                  (list (h "time" (epoch-seconds (get-universal-time)) "state" state "note" note))))
    (save-hosting-request! request)
    (let* ((id (gethash "session" request))
           (session (and (stringp id) (ignore-errors (find-session id)))))
      (when session
        (ignore-errors
         (log-event session :note "Hosting ~a: ~a~@[ -- ~a~]" (gethash "project" request) state note))))
    request))

(defun new-hosting-request! (&key project-id project sha web-url session requester)
  "Queue PROJECT (its path on the forge, its id there) at commit SHA for
hosting.  REQUESTER is who asked (their name on the forge); SESSION the
lab session it was asked from, if any.  Answers the request."
  (bt:with-lock-held (*hosting-lock*)
    (let ((request (h "id" (subseq (new-owner-key) 0 12)
                      "project" project "project_id" project-id "sha" sha
                      "web_url" web-url
                      "clone_url" (format nil "~a/~a.git" (string-right-trim "/" *gitlab-url*) project)
                      "session" session "requester" requester
                      "made" (epoch-seconds (get-universal-time)))))
      (hosting-state! request "queued" "Asked by ~a." (or requester "the lab")))))

;;
;; Names.
;;

(defun hosting-names ()
  (or (read-json-file (hosting-path "names.json")) (make-hash-table :test #'equal)))

(defun claim-name! (name project-id)
  "Give NAME to the project PROJECT-ID if nobody has it.  True when the
project has it now; nil when another project does."
  (bt:with-lock-held (*hosting-lock*)
    (let* ((names (hosting-names))
           (owner (gethash name names)))
      (cond ((null owner)
             (setf (gethash name names) project-id)
             (write-json-file (hosting-path "names.json") names)
             t)
            ((eql owner project-id) t)))))

(defun release-name! (name)
  "An admin's: let NAME be claimed again."
  (bt:with-lock-held (*hosting-lock*)
    (let ((names (hosting-names)))
      (remhash name names)
      (write-json-file (hosting-path "names.json") names))))

;;
;; The worker's side.
;;

(defun next-for-worker ()
  "Take the oldest request with work for the apps host: one to check, or
one approved to deploy (or one taken too long ago and not reported on).
Answers it with the action for the worker, or nil."
  (bt:with-lock-held (*hosting-lock*)
    (let ((now (epoch-seconds (get-universal-time))))
      (dolist (request (hosting-requests))
        (let* ((state (gethash "state" request))
               (stale? (> (- now (or (gethash "changed" request) now)) *hosting-claim-seconds*))
               (action (cond ((or (equal state "queued") (and stale? (equal state "checking"))) "check")
                             ((or (equal state "approved") (and stale? (equal state "deploying"))) "deploy"))))
          (when action
            (hosting-state! request (if (equal action "check") "checking" "deploying")
                            "Taken by the apps host to ~a." action)
            (return (h "id" (gethash "id" request) "action" action
                       "project" (gethash "project" request)
                       "sha" (gethash "sha" request)
                       "clone_url" (gethash "clone_url" request)
                       "web_url" (gethash "web_url" request)
                       "name" (gethash "name" request)
                       "requester" (gethash "requester" request)
                       ;; the image the check built and tagged
                       "image" (let ((check (gethash "check" request)))
                                 (and (hash-table-p check) (gethash "image" check)))))))))))

(defun keep-pages! (id pages)
  "Keep the pages the check fetched (a list of {path, body}) for the
review.  Answers them as (path . body)."
  (let ((kept (loop for page in pages
                    for path = (and (hash-table-p page) (gethash "path" page))
                    for body = (and (hash-table-p page) (gethash "body" page))
                    when (and (stringp path) (stringp body))
                      collect (cons path body))))
    (write-json-file (hosting-path "pages/~a.json" id)
                     (map 'vector #'(lambda (page) (h "path" (car page) "body" (cdr page))) kept))
    kept))

(defun hosting-report! (id json)
  "The worker's report on request ID.  Answers nil, or why it is refused."
  (let ((request (hosting-request id))
        (action (gethash "action" json))
        (ok? (eq (gethash "ok" json) t))
        (lines (gethash "lines" json)))
    (cond
      ((null request) "No such request.")
      ((equal action "check")
       (unless (equal (gethash "state" request) "checking")
         (return-from hosting-report! "Not being checked."))
       (setf (gethash "check" request) (h "ok" (if ok? t 'yason:false) "lines" lines
                                          "image" (gethash "image" json)))
       (let ((name (gethash "name" json)))
         (cond
           ((not ok?) (hosting-state! request "failed" "The house's check failed."))
           ((not (monocle:deployment-name? name))
            (hosting-state! request "failed" "The check named no application."))
           ((not (claim-name! name (gethash "project_id" request)))
            (setf (gethash "name" request) name)
            (hosting-state! request "failed" "~a.~a belongs to another project." name *hosting-domain*))
           (t
            (setf (gethash "name" request) name)
            (hosting-state! request "reviewing" "The check passed.")
            (let ((pages (keep-pages! id (gethash "pages" json))))
              (bt:make-thread #'(lambda () (review-hosting-request! id pages))
                              :name (format nil "prompt-lab review ~a" id))))))
       nil)
      ((equal action "deploy")
       (unless (equal (gethash "state" request) "deploying")
         (return-from hosting-report! "Not being deployed."))
       (if ok?
           (progn (setf (gethash "url" request)
                        (format nil "https://~a.~a/" (gethash "name" request) *hosting-domain*))
                  (hosting-state! request "live" "Up at ~a." (gethash "url" request)))
           (progn (setf (gethash "deploy" request) (h "lines" lines))
                  (hosting-state! request "error" "The apps host could not put it up.")))
       nil)
      (t "Report a check or a deploy."))))

;;
;; The review, between the check and the deploy: the files at the
;; commit, read from the forge, and the pages the check kept.
;;

(defun project-texts-at (project-id sha)
  "The project's text files at commit SHA that fit *project-limits*, as
(path . text), read from the forge."
  (let ((files nil) (bytes 0))
    (loop for (path . blob) in (project-tree project-id sha)
          while (< (length files) (project-limit :files))
          do (when (safe-project-path-p path)
               (multiple-value-bind (octets status)
                   (ignore-errors (get-forge (format nil "/api/v4/projects/~d/repository/blobs/~a/raw" project-id blob)))
                 (let ((text (and (eql status 200)
                                  (<= (length octets) (project-limit :file-bytes))
                                  (<= (+ bytes (length octets)) (project-limit :bytes))
                                  (text-from-octets octets))))
                   (when text
                     (incf bytes (length octets))
                     (push (cons path text) files))))))
    (nreverse files)))

(defun review-hosting-request! (id pages)
  "Review request ID's commit with the check's PAGES and move it on:
approved, held for a person, or failed."
  (let ((request (hosting-request id)))
    (when (and request (equal (gethash "state" request) "reviewing"))
      (let* ((project (gethash "project" request))
             (session (let ((lab-session (gethash "session" request)))
                        (and (stringp lab-session) (ignore-errors (find-session lab-session)))))
             (review (cond ((not (take-review! project))
                            (held-for-a-person (format nil "~a has had its ~d reviews today" project *reviews-per-day*)))
                           (t (handler-case
                                  (review-files (project-texts-at (gethash "project_id" request) (gethash "sha" request))
                                                pages :session session :session-id (format nil "hosting-~a" id))
                                (error (condition) (held-for-a-person (condition-text condition))))))))
        (setf (gethash "project" review) project
              (gethash "review" request) review)
        (let ((verdict (gethash "verdict" review)))
          (cond ((equal verdict "pass") (hosting-state! request "approved" "The review passed."))
                ((equal verdict "fail") (hosting-state! request "failed" "The review failed it: ~a" (gethash "summary" review)))
                ;; a held review's summary says so already
                (t (hosting-state! request "held" "~a" (gethash "summary" review)))))
        (notify-review! review (format nil "deploy request ~a of ~a at ~a" id project (gethash "sha" request)))))))

(defun decide-held-request! (id release? &optional note)
  "A person's word on a held request: RELEASE? approves it, else it is
rejected.  NOTE, a string, is kept in its history and shown to whoever
asked.  Nil, or why not."
  (let* ((request (hosting-request id))
         (note (and (stringp note) (string-right-trim ". " (string-trim '(#\Space #\Tab #\Newline #\Return) note))))
         (note (and note (plusp (length note)) (subseq note 0 (min 500 (length note))))))
    (cond ((null request) "No such request.")
          ((not (equal (gethash "state" request) "held")) "It is not held.")
          (release? (hosting-state! request "approved" "Released by a person~@[: ~a~]." note) nil)
          (t (hosting-state! request "rejected" "Rejected by a person~@[: ~a~]." note) nil))))

;;
;; The doors.
;;

(defun worker-allowed? (req)
  (let ((given (net.aserve:header-slot-value req :x-hosting-secret)))
    (and (stringp *hosting-secret*) (plusp (length *hosting-secret*)) (stringp given)
         (= (length given) (length *hosting-secret*))
         ;; every character compared, whatever the first difference
         (zerop (loop for a across given for b across *hosting-secret*
                      sum (logxor (char-code a) (char-code b)))))))

(defun hosting-next-door (req ent)
  "GET <prefix>/api/hosting/next, the apps host's worker: the next piece
of work, or 204."
  (if (not (worker-allowed? req))
      (respond-json req ent (h "error" "Not found.") net.aserve:*response-not-found*)
      (let ((work (next-for-worker)))
        (if work
            (respond-json req ent work)
            (net.aserve:with-http-response (req ent :response net.aserve:*response-no-content*)
              (net.aserve:with-http-body (req ent)))))))

(defun hosting-report-door (req ent)
  "POST <prefix>/api/hosting/report {id, action, ok, lines, name, image,
pages}, the apps host's worker."
  (if (not (worker-allowed? req))
      (respond-json req ent (h "error" "Not found.") net.aserve:*response-not-found*)
      (let* ((json (request-json req))
             (why (if json (hosting-report! (gethash "id" json) json) "Send a JSON object.")))
        (if why
            (respond-json req ent (h "error" why) net.aserve:*response-bad-request*)
            (respond-json req ent (h "ok" t))))))

(defun hosting-state-door (req ent)
  "GET <prefix>/api/hosting/state?id=<id>: where a request stands, for
whoever has its id -- the history, the check's lines, the review's
verdict, the address once it is up."
  (let ((request (hosting-request (query-value req "id"))))
    (if (null request)
        (respond-json req ent (h "error" "No such request.") net.aserve:*response-not-found*)
        (respond-json req ent (h "id" (gethash "id" request) "state" (gethash "state" request)
                                 "project" (gethash "project" request) "sha" (gethash "sha" request)
                                 "name" (gethash "name" request) "url" (gethash "url" request)
                                 "history" (gethash "history" request)
                                 "check" (gethash "check" request)
                                 "review" (gethash "review" request))))))

(defvar *session-hostings* (make-hash-table :test #'equal)
  "Session id -> (approve-url . time) of its last staged hosting.")

(defun stage-deploy! (session)
  "Ask the gate to host SESSION's project: the gate keeps the request for
the visitor's consent, signed in with the forge, and on it hands this lab
the request at the default branch's commit (hosting/request).  Values:
the address of the gate's page where the visitor approves, and nil; or
nil and the reason."
  (let ((record (and session (project-record session))))
    (cond ((not (pushes-offered?)) (values nil "This lab hosts nothing."))
          ((null record) (values nil "This session holds no project."))
          ((project-changes session)
           (values nil "The project has changes not on GitLab yet: push them and merge the request first; what is hosted is the default branch."))
          (t
           (multiple-value-bind (status text)
               (handler-case (post-json (format nil "~a/stage-deploy" (string-right-trim "/" *git-gate-url*))
                                        (encode (h "session" (session-id session) "project_id" (gethash "id" record))))
                 (error (e) (return-from stage-deploy! (values nil (condition-text e)))))
             (let* ((answer (ignore-errors (yason:parse text)))
                    (staged (and (eql status 200) (hash-table-p answer) (gethash "push" answer))))
               (if (not (stringp staged))
                   (values nil (format nil "The gate would not take the request (~a)~@[: ~a~]."
                                       status (and (hash-table-p answer) (gethash "message" answer))))
                   (let ((url (format nil "~a/approve?push=~a" *git-approve-base* staged)))
                     (setf (gethash (session-id session) *session-hostings*) (cons url (get-universal-time)))
                     (log-event session :note "Asked to host ~a: approve it on GitLab." (gethash "path" record))
                     (values url nil)))))))))

(defun hosting-request-door (req ent)
  "POST <prefix>/api/hosting/request {project_id, project, sha, web_url,
session, requester}, from the gate once a Maintainer of the project has
approved it: the request queued.  Answers {id}."
  (if (not (worker-allowed? req))
      (respond-json req ent (h "error" "Not found.") net.aserve:*response-not-found*)
      (let* ((json (request-json req))
             (project-id (and json (gethash "project_id" json)))
             (project (and json (gethash "project" json)))
             (sha (and json (gethash "sha" json))))
        (if (not (and (integerp project-id) (stringp project) (project-path-from project)
                      (stringp sha) (= (length sha) 40) (every #'(lambda (c) (digit-char-p c 16)) sha)))
            (respond-json req ent (h "error" "project_id, project and a commit's sha.") net.aserve:*response-bad-request*)
            (let ((request (new-hosting-request! :project-id project-id :project project :sha sha
                                                 :web-url (gethash "web_url" json)
                                                 :session (gethash "session" json)
                                                 :requester (gethash "requester" json))))
              (let ((session (let ((id (gethash "session" json))) (and (stringp id) (ignore-errors (find-session id))))))
                (when session
                  (log-event session :note "~a asked to host ~a at ~a: request ~a."
                             (or (gethash "requester" json) "A maintainer") project (subseq sha 0 8) (gethash "id" request))))
              (respond-json req ent (h "id" (gethash "id" request))))))))

(defvar *hosting-admin-secret* nil
  "String or nil. The secret an admin of the lab sends in
X-Hosting-Admin-Secret to see and decide held requests.  Nil shuts
those doors.")

(defun admin-allowed? (req)
  (let ((given (net.aserve:header-slot-value req :x-hosting-admin-secret)))
    (and (stringp *hosting-admin-secret*) (plusp (length *hosting-admin-secret*)) (stringp given)
         (= (length given) (length *hosting-admin-secret*))
         (zerop (loop for a across given for b across *hosting-admin-secret*
                      sum (logxor (char-code a) (char-code b)))))))

(defun hosting-held-door (req ent)
  "GET <prefix>/api/hosting/held, an admin: the requests held for a
person, each with the review's verdict and findings."
  (if (not (admin-allowed? req))
      (respond-json req ent (h "error" "Not found.") net.aserve:*response-not-found*)
      (respond-json req ent
                    (h "held" (map 'vector #'(lambda (request)
                                                 (h "id" (gethash "id" request) "project" (gethash "project" request)
                                                    "sha" (gethash "sha" request) "name" (gethash "name" request)
                                                    "requester" (gethash "requester" request)
                                                    "review" (gethash "review" request)))
                                     (remove "held" (hosting-requests) :key #'(lambda (r) (gethash "state" r))
                                                                       :test-not #'equal))))))

(defun hosting-decide-door (req ent)
  "POST <prefix>/api/hosting/decide {id, release, note}, an admin: a held
request released (approved, for the worker to deploy) or rejected, with
an optional note for its history."
  (if (not (admin-allowed? req))
      (respond-json req ent (h "error" "Not found.") net.aserve:*response-not-found*)
      (let* ((json (request-json req))
             (why (if (hash-table-p json)
                      (decide-held-request! (gethash "id" json) (eq (gethash "release" json) t)
                                            (gethash "note" json))
                      "Send {id, release}.")))
        (if why
            (respond-json req ent (h "error" why) net.aserve:*response-bad-request*)
            (respond-json req ent (h "ok" t))))))

;;
;; The admins' page: what is held, what is named, what happened lately,
;; with release and reject for a held request and release for a name.
;; The page holds no data; it asks the admin doors with the admin secret,
;; which the admin types into it.  Where the doors are refused from
;; outside (a front's rules), the page is reached the same way.
;;

(defparameter *hosting-admin-recent* 40
  "Integer. How many of the latest requests the admins' page lists.")

(defun check-fail-lines (request)
  (let ((check (gethash "check" request)))
    (when (hash-table-p check)
      (remove-if-not #'(lambda (line) (and (stringp line) (> (length line) 4) (string= "FAIL" line :end2 4)))
                     (coerce (or (gethash "lines" check) #()) 'list)))))

(defun hosting-admin-entry (request &key full?)
  "REQUEST as the admins' page shows it; FULL? adds the review, the
check's lines and the history."
  (let ((entry (h "id" (gethash "id" request) "state" (gethash "state" request)
                  "state_text" (hosting-state-text (gethash "state" request))
                  "project" (gethash "project" request) "project_id" (gethash "project_id" request)
                  "sha" (gethash "sha" request) "web_url" (gethash "web_url" request)
                  "name" (gethash "name" request) "url" (gethash "url" request)
                  "requester" (gethash "requester" request)
                  "made" (gethash "made" request) "changed" (gethash "changed" request)
                  "reasons" (coerce (remove nil (hosting-request-reasons request)) 'vector))))
    (when full?
      (let ((check (gethash "check" request)))
        (setf (gethash "review" entry) (gethash "review" request)
              (gethash "fails" entry) (coerce (check-fail-lines request) 'vector)
              (gethash "check_lines" entry) (or (and (hash-table-p check) (gethash "lines" check)) #())
              (gethash "history" entry) (coerce (gethash "history" request) 'vector))))
    entry))

(defun hosting-admin-names (requests)
  "The names given out, each with its project and, when one of its
requests is live, the address."
  (let ((names (hosting-names)) (out nil))
    (maphash #'(lambda (name project-id)
                 (let* ((mine (remove-if-not #'(lambda (r) (eql (gethash "project_id" r) project-id)) requests))
                        (live (find-if #'(lambda (r) (and (equal (gethash "state" r) "live")
                                                          (equal (gethash "name" r) name)))
                                       mine :from-end t)))
                   (push (h "name" name "project_id" project-id
                            "project" (and mine (gethash "project" (car (last mine))))
                            "url" (and live (gethash "url" live)))
                         out)))
             names)
    (coerce (sort out #'string< :key #'(lambda (entry) (gethash "name" entry))) 'vector)))

(defun hosting-admin-door (req ent)
  "GET <prefix>/api/hosting/admin, an admin: {domain, held, recent,
names} -- the held requests in full, the latest *hosting-admin-recent*
newest first, and the names given out."
  (if (not (admin-allowed? req))
      (respond-json req ent (h "error" "Not found.") net.aserve:*response-not-found*)
      (let ((requests (hosting-requests)))
        (respond-json req ent
                      (h "domain" *hosting-domain*
                         "held" (map 'vector #'(lambda (r) (hosting-admin-entry r :full? t))
                                     (remove "held" requests :key #'(lambda (r) (gethash "state" r)) :test-not #'equal))
                         "recent" (map 'vector #'hosting-admin-entry
                                       (subseq (reverse requests) 0 (min *hosting-admin-recent* (length requests))))
                         "names" (hosting-admin-names requests))))))

(defun hosting-release-name-door (req ent)
  "POST <prefix>/api/hosting/release-name {name}, an admin: NAME may be
claimed again, by any project.  What runs under it stays up."
  (if (not (admin-allowed? req))
      (respond-json req ent (h "error" "Not found.") net.aserve:*response-not-found*)
      (let* ((json (request-json req))
             (name (and (hash-table-p json) (gethash "name" json))))
        (cond ((not (and (stringp name) (nth-value 1 (gethash name (hosting-names)))))
               (respond-json req ent (h "error" "No such name is given out.") net.aserve:*response-bad-request*))
              (t (release-name! name)
                 (respond-json req ent (h "ok" t)))))))

(defun hosting-admin-page-door (req ent)
  "GET <prefix>/hosting-admin: the admins' page (static/hosting-admin.html).
It holds no data and works only with the admin secret."
  (let ((file (merge-pathnames "hosting-admin.html" *static-directory*)))
    (net.aserve:with-http-response (req ent :content-type "text/html; charset=utf-8")
      (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
      (net.aserve:with-http-body (req ent :external-format :utf-8)
        (write-string (uiop:read-file-string file :external-format :utf-8) net.html.generator:*html-stream*)))))

(defun publish-hosting! (&key host)
  (gwl:with-all-servers (server)
    (net.aserve:publish :path (door-path "hosting/next") :server server :host host :function #'hosting-next-door)
    (net.aserve:publish :path (door-path "hosting/report") :server server :host host :function #'hosting-report-door)
    (net.aserve:publish :path (door-path "hosting/state") :server server :host host :function #'hosting-state-door)
    (net.aserve:publish :path (door-path "hosting/request") :server server :host host :function #'hosting-request-door)
    (net.aserve:publish :path (door-path "hosting/held") :server server :host host :function #'hosting-held-door)
    (net.aserve:publish :path (door-path "hosting/decide") :server server :host host :function #'hosting-decide-door)
    (net.aserve:publish :path (door-path "hosting/admin") :server server :host host :function #'hosting-admin-door)
    (net.aserve:publish :path (door-path "hosting/release-name") :server server :host host :function #'hosting-release-name-door)
    (net.aserve:publish :path (format nil "~a/hosting-admin" *url-prefix*) :server server :host host
                        :function #'hosting-admin-page-door)))
