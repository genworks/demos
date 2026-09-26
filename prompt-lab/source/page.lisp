;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The page and its doors.  The page (static/page.html) is one static
;; document: its script opens or resumes a session, posts prompts,
;; polls the session's state (the log, the token totals, the model
;; file) and reloads the viewer beside it after every build.  The doors
;; under <prefix>/api/ are plain aserve publishes with no gwl session
;; behind them, cheap and nothing to reap.  The viewer is a gwl app: a
;; sluice opened on the session's MODEL.
;;

(defparameter *page-file*
  ;; the SOURCE file's place, read at compile time: at load time the
  ;; truename is the fasl's, off in a cache directory
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (make-pathname :name "page" :type "html"
                   :directory (append (butlast (pathname-directory here)) (list "static"))
                   :defaults here))
  "Pathname. The page, beside the source in static/.")


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

(defun refuse (req ent control &rest args)
  (respond-json req ent (h "error" (apply #'format nil control args))
                net.aserve:*response-bad-request*))

(defun epoch-seconds (universal-time)
  (- universal-time #.(encode-universal-time 0 0 0 1 1 1970 0)))


;;
;; What the page knows about a session.
;;

(defun prompts-used (session)
  (count :prompt (session-log session) :key #'second))

(defun model-defined? (session)
  (let ((symbol (model-symbol session)))
    (and symbol (find-class symbol nil) t)))

(defun model-body (session)
  "The model file's source after its header (the in-package line): what
the page's editor shows, and what write-model takes back."
  (let ((file (session-model-file session)))
    (if (probe-file file)
        (let* ((text (uiop:read-file-string file :external-format :utf-8))
               (start (search "(in-package" text))
               (eol (and start (position #\Newline text :start start))))
          (if eol
              (string-left-trim '(#\Newline #\Return) (subseq text (1+ eol)))
              text))
        "")))

(defun viewer-url (session)
  (format nil "~a/viewer?session=~a" *url-prefix* (session-id session)))

(defun console-url (session)
  "The terminal opened on the session's model file, or nil without a terminal."
  (when *console-base*
    (format nil "~a/?arg=~a" *console-base* (namestring (session-model-file session)))))

(defun session-state (session)
  (let ((usage (session-usage session)))
    (h "session" (session-id session)
       "busy" (if (session-busy? session) t 'yason:false)
       "prompts_used" (prompts-used session)
       "prompts_allowed" *max-prompts-per-session*
       "usage" (h "input" (getf usage :input) "output" (getf usage :output)
                  "cache_read" (getf usage :cache-read) "cache_write" (getf usage :cache-write))
       "log" (map 'vector #'(lambda (entry)
                              (destructuring-bind (time kind text) entry
                                (h "time" (epoch-seconds time)
                                   "kind" (string-downcase kind)
                                   "text" text)))
                  (session-log session))
       "model_defined" (if (model-defined? session) t 'yason:false)
       "model_file" (namestring (session-model-file session))
       "model_source" (model-body session)
       "viewer_url" (viewer-url session)
       ;; nil encodes as null; an empty vector would be the empty array
       "console_url" (console-url session))))


;;
;; The doors.
;;

(defun session-door (req ent)
  "POST <prefix>/api/session: open a session; answers its id."
  (let ((session (make-session)))
    (log-event session :note "Session ~a opened.  Describe what to build." (session-id session))
    (respond-json req ent (h "session" (session-id session)))))

(defun state-door (req ent)
  "GET <prefix>/api/state?session=<id>: everything the page shows."
  (let ((session (requested-session req)))
    (if session
        (respond-json req ent (session-state (touch session)))
        (no-such-session req ent))))

(defun prompt-door (req ent)
  "POST <prefix>/api/prompt {session, prompt}: start the agent on the
prompt in a thread of its own; the page follows along through the state door."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (prompt (and json (gethash "prompt" json))))
    (cond ((null session) (no-such-session req ent))
          ((not (and (stringp prompt)
                     (plusp (length (string-trim '(#\space #\tab #\newline #\return) prompt)))))
           (refuse req ent "Say what to build."))
          ((> (length prompt) *max-prompt-length*)
           (refuse req ent "A prompt may have ~a characters at most." *max-prompt-length*))
          ((>= (prompts-used session) *max-prompts-per-session*)
           (refuse req ent "This session has used its ~a prompts.  Take a copy of the model file, or start a new session."
                   *max-prompts-per-session*))
          ((not (start-prompt! session (string-trim '(#\space #\tab #\newline #\return) prompt)))
           (refuse req ent "Still working on the previous request."))
          (t (respond-json req ent (h "started" t) net.aserve:*response-accepted*)))))

(defun model-door (req ent)
  "POST <prefix>/api/model {session, source}: the visitor's own edit of the
model file, written, compiled and loaded like the agent's."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (source (and json (gethash "source" json))))
    (cond ((null session) (no-such-session req ent))
          ((not (stringp source)) (refuse req ent "No source given."))
          ((session-busy? session) (refuse req ent "Wait for the agent to finish first."))
          (t (multiple-value-bind (blocks error?) (write-model (touch session) source)
               (let ((text (or (cdr (assoc "text" (first blocks) :test #'string=)) "")))
                 (log-event session :reload "Your edit: ~a" text)
                 (respond-json req ent (h "ok" (if error? 'yason:false t) "text" text))))))))

(defun reload-door (req ent)
  "POST <prefix>/api/reload {session}: compile and load the model file as
it is on disk, after an edit made in the terminal."
  (let* ((json (request-json req))
         (session (requested-session req json)))
    (cond ((null session) (no-such-session req ent))
          ((session-busy? session) (refuse req ent "Wait for the agent to finish first."))
          ((not (probe-file (session-model-file session))) (refuse req ent "There is no model file yet."))
          (t (multiple-value-bind (blocks error?) (load-model-file (touch session))
               (let ((text (or (cdr (assoc "text" (first blocks) :test #'string=)) "")))
                 (log-event session :reload "Reloaded from disk: ~a" text)
                 (respond-json req ent (h "ok" (if error? 'yason:false t) "text" text))))))))


;;
;; The viewer: a sluice opened on the session's MODEL, its leaves drawn
;; as the page opens.
;;

(define-object viewer (sluice:assembly)

  :documentation
  (:description "A sluice opened on one prompt-lab session's MODEL, reached
at <prefix>/viewer?session=<id>.  The model's leaves are drawn as the page
opens; the tree, the menus and the headset button are the sluice's own."
   :author "Genworks International")

  :computed-slots
  ((title "Prompt lab viewer")

   (session-id (cdr (assoc "session" (the query-toplevel) :test #'string-equal)))

   (session (let ((id (the session-id))) (and (stringp id) (find-session id))))

   (root-object-type (let ((session (the session)))
                       (and session (model-defined? session) (model-symbol session)))
                     :settable)

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
    (the (draw-model!)))

   (draw-model!
    ()
    (when (the root-object)
      (ignore-errors (the viewport (draw-leaves! (the root-object))))))))


;;
;; Publishing.
;;

(defun door-path (name)
  (format nil "~a/api/~a" *url-prefix* name))

(defun publish-prompt-lab! (&key host)
  "Publish the page at *url-prefix*, its doors under <prefix>/api/ and the
viewer at <prefix>/viewer, on every server."
  (gwl:with-all-servers (server)
    (net.aserve:publish-file :path *url-prefix* :server server :host host
                             :file (namestring *page-file*) :content-type "text/html; charset=utf-8")
    (net.aserve:publish :path (door-path "session") :server server :host host :function #'session-door)
    (net.aserve:publish :path (door-path "state") :server server :host host :function #'state-door)
    (net.aserve:publish :path (door-path "prompt") :server server :host host :function #'prompt-door)
    (net.aserve:publish :path (door-path "model") :server server :host host :function #'model-door)
    (net.aserve:publish :path (door-path "reload") :server server :host host :function #'reload-door)
    (publish-gwl-app (format nil "~a/viewer" *url-prefix*) 'viewer :server server :host host))
  (start-reaper!)
  *url-prefix*)
