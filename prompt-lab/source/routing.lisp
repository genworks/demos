;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; Which lab a request belongs in.  A lab on the open-source engine
;; draws a hole but cannot cut one; a lab on a solids engine cuts it,
;; and costs more to run.  Where two labs stand side by side
;; (*sibling-lab*), the first thing done with a session's first prompt
;; is to decide which engine it wants:
;;
;;   - the prompt may say so itself: "no solids", "use solids";
;;   - else one small call asks the model, of the prompt and of any
;;     drawing uploaded with it.
;;
;; A request that wants the other engine is not built here: the visitor
;; is sent to the sibling lab with the prompt, and the sibling takes the
;; session's files over (the page's adopt action).  An uploaded drawing
;; is asked the same question as it arrives, on the open-source lab, so
;; the page can say so before any prompt; that verdict is kept beside
;; the session's record as routing.json and the first prompt goes by it.
;;

(defparameter *classify-uploads?* t
  "Boolean. Whether an uploaded drawing is looked at once, by a small call
to the model, to say whether it needs a solids engine.  Only a lab on
the open-source engine with a sibling lab asks.")

(defparameter *route-prompts?* t
  "Boolean. Whether a session's first prompt is sent to the sibling lab
when it wants the other engine.  Only a lab with a sibling decides.")

(defparameter *classify-max-tokens* 2000
  "Integer. max_tokens of the classifying call (its thinking included).")

(defparameter *classify-seconds* 60
  "Integer. Seconds the classifying call may take.")

(defparameter *classify-system*
  "You sort requests for a CAD modelling service: a request in words, a drawing, or both.  Answer with one word on the first line, SOLIDS or PLAIN, and one short sentence of reason on the second line, written for the person who asked.
SOLIDS: building the part faithfully needs material removed or merged: holes, bores, slots, pockets, cutouts, chamfers, fillets, counterbores, threads, hollow bodies, or bodies joined into one.
PLAIN: the design is an arrangement of separate simple shapes (boxes, cylinders, cones, spheres, extruded outlines) with nothing cut from them: furniture, frames, structures, layouts, assemblies of plain parts.
If it is not a request for a physical design, answer PLAIN."
  "String. The system prompt of the classifying call.")

(defparameter *solids-phrases*
  '(:gendl ("no solids" "without solids" "no solid model" "don't use solids" "do not use solids")
    :solid ("use solids" "with solids" "using solids" "as solids" "as a solid" "use the solids"))
  "Plist, engine -> phrases.  A prompt holding one of them has said which
engine it wants, and nothing is asked of the model.")

(defvar *response-conflict* (net.aserve::make-resp 409 "Conflict")
  "The prompt door's answer to a prompt that belongs in the sibling lab.")

(defun other-engine ()
  (if (eq *engine* :solid) :gendl :solid))

(defun classifying-uploads? ()
  "Whether this lab asks of a drawing as it arrives: allowed, on the
open-source engine, and a sibling lab to send it to."
  (and *classify-uploads?* (eq *engine* :gendl) (car *sibling-lab*) t))

(defun prompt-engine (prompt)
  "The engine PROMPT asks for in so many words, or nil."
  (let ((text (string-downcase prompt)))
    (loop for (engine phrases) on *solids-phrases* by #'cddr
          when (some #'(lambda (phrase) (search phrase text)) phrases)
            return engine)))

(define-object engine-classifier ()

  :documentation
  (:description "One request asked of the model: does building it need a
solids engine?  The request is a prompt, drawings (uploaded PDFs or
images), or both.  The call is made when the verdict is first asked for."
   :author "Genworks International")

  :input-slots
  ("The session (session.lisp) the request belongs to."
   session
   ("String or nil. The visitor's prompt."
    prompt nil)
   ("List of uploaded-file objects (uploads.lisp). The drawings."
    drawings nil))

  :computed-slots
  (("String. The request, as the Messages API takes it."
    request
    (encode (h "model" *model*
               "max_tokens" *classify-max-tokens*
               "output_config" (h "effort" "low")
               "system" *classify-system*
               "messages"
               (list (h "role" "user"
                        "content"
                        (append (loop for drawing in (the drawings)
                                      append (the-object drawing api-blocks))
                                (list (h "type" "text"
                                         "text" (format nil "~@[The request: ~a~%~%~]Does building this need SOLIDS, or is it PLAIN?"
                                                        (the prompt))))))))))

   ("Hash table or nil. The model's answer; nil when the call failed.  The
gate's word on money and the tokens used are booked on the session as
for any call."
    response
    (let ((session (the session))
          (key (api-key)))
      (ignore-errors
       (multiple-value-bind (status text headers)
           (post-json *messages-url* (the request)
                      :headers (append (list (cons "anthropic-version" "2023-06-01"))
                                       (when key (list (cons "x-api-key" key)))
                                       (list (cons *session-header* (session-id session)))
                                       (when (session-wallet session)
                                         (list (cons *wallet-header* (session-wallet session)))))
                      :seconds *classify-seconds*)
         (note-gate-answer session headers)
         (let ((json (yason:parse text)))
           (when (and (eql status 200) (hash-table-p json) (listp (gethash "content" json)))
             (add-usage session (gethash "usage" json))
             json))))))

   ("List of strings. The answer's lines that say anything."
    answer-lines
    (let ((response (the response)))
      (when response
        (remove "" (mapcar #'(lambda (line) (string-trim '(#\space #\tab #\return) line))
                           (uiop:split-string
                            (format nil "~{~a~^~%~}"
                                    (loop for block in (gethash "content" response)
                                          when (equal (gethash "type" block) "text")
                                            collect (gethash "text" block)))
                            :separator '(#\newline)))
                :test #'string=))))

   ("Boolean. Whether there is a verdict at all."
    answered? (and (the answer-lines) t))

   ("Boolean. The verdict: the request needs material removed or joined."
    needs-solids? (let ((first (first (the answer-lines))))
                    (and first (search "SOLIDS" (string-upcase first)) t)))

   ("Keyword. The engine the request wants, :solid or :gendl."
    engine (if (the needs-solids?) :solid :gendl))

   ("String. The model's sentence of reason, for the visitor."
    reason (or (second (the answer-lines)) ""))

   ("Hash table. The verdict as it is kept and as the page is told."
    verdict (h "file" (let ((drawing (first (the drawings))))
                        (and drawing (the-object drawing file-name)))
               "solids" (if (the needs-solids?) t 'yason:false)
               "reason" (the reason)))))


;;
;; The verdict on an uploaded drawing, kept beside the session's record.
;;

(defun routing-file (session)
  (merge-pathnames "routing.json" (session-directory session)))

(defun session-routing (session)
  "The verdict on SESSION's drawing, a hash table (file, solids, reason),
or nil when none was asked for or given."
  (ignore-errors
   (let ((file (routing-file session)))
     (when (probe-file file)
       (with-open-file (in file :external-format :utf-8)
         (let ((json (yason:parse in)))
           (and (hash-table-p json) json)))))))

(defun route-url (session)
  "The sibling lab's address for a visitor sent there from SESSION: it
offers to take the session's files over when there are any."
  (if (session-files session)
      (format nil "~a?adopt=~a&from=~a&routed=1" (car *sibling-lab*) (session-id session) *url-prefix*)
      (format nil "~a?routed=1" (car *sibling-lab*))))

(defun routing-state (session)
  "What the page is told of an uploaded drawing: the verdict, with the
sibling lab's address when the drawing wants the other engine."
  (let ((routing (and (not (session-replay? session)) (session-routing session))))
    (when routing
      (let ((elsewhere? (and (car *sibling-lab*)
                             (not (eq *engine* (if (gethash "solids" routing) :solid :gendl))))))
        (h "file" (gethash "file" routing)
           "solids" (if (gethash "solids" routing) t 'yason:false)
           "reason" (gethash "reason" routing)
           "sibling_url" (and elsewhere? (route-url session)))))))

(defun classify-upload! (session file)
  "Ask the model whether FILE, a drawing uploaded to SESSION, needs a
solids engine; keep the verdict and say it in the session's log.  Never
signals."
  (ignore-errors
   (let ((classifier (make-object 'engine-classifier :session session :drawings (list file))))
     (when (the-object classifier answered?)
       (write-text-file (encode (the-object classifier verdict)) (routing-file session))
       (if (the-object classifier needs-solids?)
           (log-event session :note "~a looks like a part with material cut away or joined.  ~a  This lab draws holes without cutting them; the ~a cuts them as real solids, and can take this session's files over."
                      (the-object file file-name) (the-object classifier reason) (cdr *sibling-lab*))
           (log-event session :note "~a looks like a part this lab builds as drawn.  ~a"
                      (the-object file file-name) (the-object classifier reason)))
       (save-session! session)))))

(defun maybe-classify! (session file)
  "Start the classifying call for FILE in a thread of its own, when this
lab asks and FILE is a drawing: the upload's answer does not wait for it."
  (when (and (classifying-uploads?)
             (member (the-object file kind) '(:pdf :image))
             (not (pot-empty?)))
    (bt:make-thread #'(lambda () (classify-upload! session file))
                    :name (format nil "prompt-lab classify ~a" (session-id session)))))


;;
;; The first prompt, sent where it belongs.
;;

(defun route-prompt (session prompt)
  "Decide which engine SESSION's first PROMPT wants.  Nil when it is to
be built here: this lab has no sibling, the session has prompts or a
model already, or the verdict is this engine (or none came).  Else a
plist -- :engine the one wanted, :reason for the visitor, :url the
sibling lab's address for them -- and the session's log says so."
  (when (and *route-prompts?* (car *sibling-lab*)
             (zerop (prompts-used session))
             (not (model-defined? session)))
    (let* ((said (prompt-engine prompt))
           (kept (and (null said) (session-routing session)))
           (classifier (and (null said) (null kept)
                            (make-object 'engine-classifier
                                         :session session :prompt prompt
                                         :drawings (remove-if-not
                                                    #'(lambda (file)
                                                        (member (the-object file kind) '(:pdf :image)))
                                                    (session-files session)))))
           (engine (cond (said said)
                         (kept (if (gethash "solids" kept) :solid :gendl))
                         ((the-object classifier answered?) (the-object classifier engine))))
           (reason (cond (said "The prompt says so.")
                         (kept (or (gethash "reason" kept) ""))
                         (classifier (the-object classifier reason)))))
      (when (and engine (not (eq engine *engine*)))
        (let ((text (format nil "This request ~:[builds without a solids engine, which costs less~;wants a solids engine: holes and cuts made for real~].  ~a  It belongs in the ~a."
                            (eq engine :solid) (or reason "") (cdr *sibling-lab*))))
          (log-event session :note "~a" text)
          (save-session! session)
          (list :engine engine :reason text :url (route-url session)))))))
