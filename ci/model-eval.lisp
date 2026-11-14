;;;; Copyright © 2026 Genworks International
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as
;;;; published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.  Distributed WITHOUT
;;;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;; ci/model-eval.lisp -- compare models on the prompt lab's own prompts.
;;;;
;;;; Not a CI check: a tool, loaded into a Lisp that has the prompt lab
;;;; loaded and its *messages-url* pointed at a gate whose menu offers the
;;;; models to compare.  It replays the FIRST prompt of archived public
;;;; sessions, one fresh session each, on one model at a time, and records
;;;; for each: whether the agent finished (its last line :done) and left a
;;;; model built, the seconds it took, the rounds of tool calls and the
;;;; tokens; the cost is the tokens at the prices given here, so it does not
;;;; depend on the gate's balance answer.  A run stops before the next prompt
;;;; once its cost reaches BUDGET-CENTS.  Results go to a JSON-lines file,
;;;; one line a prompt.
;;;;
;;;;   (load "ci/model-eval.lisp")
;;;;   (defvar *sample* (eval-sample "/path/to/prompt-lab-archive/sessions/" :n 50))
;;;;   (eval-estimate *sample*)                 ; what each arm would cost: no call made
;;;;   (eval-run *sample* "house-1" :budget-cents 0 :out "/tmp/eval-house.jsonl")
;;;;   (eval-run *sample* "claude-sonnet-5-5" :budget-cents <ceiling> :out "/tmp/eval-sonnet.jsonl")
;;;;   (eval-summary "/tmp/eval-house.jsonl")
;;;;
;;;; BUDGET-CENTS nil means no ceiling: give one for any model that costs
;;;; money.  A prompt whose session had a file uploaded before it is left
;;;; out (the file is not replayed), and so is a prompt seen before.

(in-package :gdl-user)

(defparameter *eval-prices*
  '(("claude-opus-5-5" :input 4 :output 20 :cache-read 0.2 :cache-write 5)
    ("claude-sonnet-5-5" :input 2 :output 10 :cache-read 0.1 :cache-write 2.5)
    ("claude-haiku-5-5" :input 0.1 :output 0.5 :cache-read 0.01 :cache-write 0.125))
  "USD per million tokens, by model, for the cost of a run.  A model not
listed costs nothing here (a house model: power, not API money).")

(defparameter *eval-mean-prompt-cents* nil
  "Number or nil.  What an archived prompt is taken to have cost on Opus
when its session does not say; set it before an estimate.  Nil: such a
prompt is left out of the estimate and counted apart.")

(defun eval-lab (name)
  (or (find-symbol (symbol-name name) :prompt-lab)
      (error "The prompt lab has no ~a: load it first." name)))

(defun eval-call (name &rest args) (apply (eval-lab name) args))

(defun eval-get (object key)
  (and (hash-table-p object) (gethash key object)))

(defun eval-first-prompt (json)
  "The first prompt of an archived session's log and what followed it.
Values: the prompt's text or nil (none, or a file uploaded before it) and
the outcome, :done, :stopped or nil."
  (let ((prompt nil) (outcome nil))
    (dolist (entry (coerce (or (eval-get json "log") '()) 'list))
      (let ((kind (and (consp entry) (second entry)))
            (text (and (consp entry) (third entry))))
        (cond ((and (null prompt) (equal kind "note") (stringp text) (search "Uploaded" text))
               (return-from eval-first-prompt nil))
              ((and (null prompt) (equal kind "prompt") (stringp text))
               (setq prompt text))
              ((and prompt (member kind '("done" "stopped") :test #'equal))
               (setq outcome (if (equal kind "done") :done :stopped))
               (return)))))
    (values prompt outcome)))

(defun eval-sample (root &key (n 50))
  "N first prompts from the archived sessions under ROOT (its
<day>/<id>/session.json), spread evenly over the archive in date order.
Each is a plist (:id :prompt :archived-outcome :archived-cents)."
  (let ((seen (make-hash-table :test #'equal))
        (candidates nil))
    (dolist (file (sort (directory (merge-pathnames "*/*/session.json" (uiop:ensure-directory-pathname root)))
                        #'string< :key #'namestring))
      (let ((json (ignore-errors (with-open-file (in file :external-format :utf-8) (yason:parse in)))))
        (multiple-value-bind (prompt outcome) (and json (eval-first-prompt json))
          (when (and prompt (not (gethash prompt seen)))
            (setf (gethash prompt seen) t)
            (let ((prompts (count "prompt" (coerce (or (eval-get json "log") '()) 'list)
                                  :key (lambda (e) (and (consp e) (second e))) :test #'equal))
                  (cents (eval-get json "cents")))
              (push (list :id (eval-get json "id") :prompt prompt :archived-outcome outcome
                          :archived-cents (if (and (realp cents) (plusp cents) (plusp prompts))
                                              (/ cents prompts)
                                              *eval-mean-prompt-cents*))
                    candidates))))))
    (setq candidates (nreverse candidates))
    (let ((total (length candidates)))
      (if (<= total n)
          candidates
          (loop for i below n collect (nth (floor (* i total) n) candidates))))))

(defun eval-model-ratio (model)
  "MODEL's cost against Opus 5.5's for the same tokens."
  (let ((prices (cdr (assoc model *eval-prices* :test #'equal)))
        (opus (cdr (assoc "claude-opus-5-5" *eval-prices* :test #'equal))))
    (if prices
        (/ (+ (getf prices :input) (getf prices :output)) (+ (getf opus :input) (getf opus :output)))
        0)))

(defun eval-estimate (sample &key (models (append (mapcar #'car *eval-prices*) '("house"))))
  "What one run of SAMPLE would cost on each of MODELS, in cents, from the
archive's Opus cost: no call is made.  Values: an alist (model . cents),
and how many prompts had no known cost and were left out."
  (let ((opus (reduce #'+ sample :key (lambda (s) (or (getf s :archived-cents) 0))))
        (unknown (count-if-not #'realp sample :key (lambda (s) (getf s :archived-cents)))))
    (values (mapcar (lambda (model) (cons model (float (* opus (eval-model-ratio model)) 1.0))) models)
            unknown)))

(defun eval-cents (model usage)
  "What USAGE (the lab's plist: :input :output :cache-read :cache-write)
costs on MODEL at *eval-prices*, in cents."
  (let ((prices (cdr (assoc model *eval-prices* :test #'equal))))
    (if prices
        (/ (+ (* (or (getf usage :input) 0) (getf prices :input))
              (* (or (getf usage :output) 0) (getf prices :output))
              (* (or (getf usage :cache-read) 0) (getf prices :cache-read))
              (* (or (getf usage :cache-write) 0) (getf prices :cache-write)))
           10000)
        0)))

(defun eval-json-object (plist)
  "PLIST as a hash table for JSON: keys in lower case with underscores,
keyword values as their lower-case names."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash (substitute #\_ #\- (string-downcase (symbol-name key))) table)
                   (cond ((eq value t) t)
                         ((null value) 'yason:false)
                         ((keywordp value) (string-downcase (symbol-name value)))
                         (t value))))
    table))

(defun eval-run (sample model &key budget-cents out (net 250))
  "Replay SAMPLE's prompts on MODEL, one fresh session each, until the cost
reaches BUDGET-CENTS (nil: no ceiling).  Each session opens from an address
of its own in 10.NET.x.y, so the lab's sessions-per-address cap never stops
a run.  Each result is written to OUT (a JSON-lines file, appended) when
given.  Answers the results, plists."
  (let ((spent 0) (results nil) (index 0))
    (dolist (item sample)
      (when (and budget-cents (>= spent budget-cents)) (return))
      (let* ((address (format nil "10.~d.~d.~d" net (floor index 250) (1+ (mod index 250))))
             (session (progn (incf index) (eval-call '#:open-session! address nil))))
        (unless session (error "The lab would not open a session for ~a." address))
        (unwind-protect
             (let ((start (get-internal-real-time))
                   (started (eval-call '#:utc-time (get-universal-time))))
               (eval-call '#:set-session-model! session model)
               (eval-call '#:run-prompt session (getf item :prompt))
               (let* ((log (eval-call '#:session-log session))
                      (last-kind (let ((entry (find-if (lambda (e) (member (eval-log-kind e) '(:done :stopped)))
                                                       (reverse (coerce log 'list)))))
                                   (and entry (eval-log-kind entry))))
                      (usage (eval-call '#:session-usage session))
                      (cents (eval-cents model usage))
                      (result (list :id (getf item :id) :model model
                                    ;; when and as which session, to pair with the server's own timings
                                    :session (eval-call '#:session-id session)
                                    :started started :ended (eval-call '#:utc-time (get-universal-time))
                                    :finished (eq last-kind :done)
                                    :built (and (ignore-errors (eval-call '#:model-defined? session)) t)
                                    :seconds (float (/ (- (get-internal-real-time) start) internal-time-units-per-second) 1.0)
                                    :rounds (count :tool (coerce log 'list) :key #'eval-log-kind)
                                    :input (getf usage :input) :output (getf usage :output)
                                    :cache-read (getf usage :cache-read) :cache-write (getf usage :cache-write)
                                    :cents (float cents 1.0)
                                    :archived-outcome (getf item :archived-outcome))))
                 (incf spent cents)
                 (push result results)
                 (when out
                   (with-open-file (stream out :direction :output :if-exists :append :if-does-not-exist :create
                                               :external-format :utf-8)
                     (yason:encode (eval-json-object result) stream)
                     (terpri stream)))))
          (ignore-errors (eval-call '#:delete-session session)))))
    (nreverse results)))

(defun eval-log-kind (entry)
  "The kind of a session log entry, a keyword, whatever shape the lab keeps it in."
  (let ((kind (cond ((consp entry) (or (getf entry :kind) (second entry)))
                    ((hash-table-p entry) (gethash "kind" entry))
                    ((and (vectorp entry) (> (length entry) 1)) (aref entry 1)))))
    (if (stringp kind) (intern (string-upcase kind) :keyword) kind)))

(defun eval-summary (results)
  "RESULTS (a list of plists, or a JSON-lines file eval-run wrote) summed up:
how many, the share finished and built, the median seconds, the rounds,
the cents in all.  A plist."
  (let* ((rows (if (or (stringp results) (pathnamep results))
                   (with-open-file (in results :external-format :utf-8)
                     (loop for line = (read-line in nil nil) while line
                           collect (let ((h (yason:parse line)))
                                     (list :finished (gethash "finished" h) :built (gethash "built" h)
                                           :seconds (gethash "seconds" h) :rounds (gethash "rounds" h)
                                           :cents (gethash "cents" h)))))
                   results))
         (n (length rows))
         (seconds (sort (mapcar (lambda (r) (getf r :seconds)) rows) #'<)))
    (if (zerop n)
        (list :n 0)
        (list :n n
              :finished-percent (round (* 100 (count-if (lambda (r) (getf r :finished)) rows)) n)
              :built-percent (round (* 100 (count-if (lambda (r) (getf r :built)) rows)) n)
              :median-seconds (nth (floor n 2) seconds)
              :mean-rounds (float (/ (reduce #'+ rows :key (lambda (r) (or (getf r :rounds) 0))) n) 1.0)
              :cents (float (reduce #'+ rows :key (lambda (r) (or (getf r :cents) 0))) 1.0)))))

;;;; THE HYBRID ARMS: a strong model and a house model on one prompt.
;;;;
;;;;   (eval-run-planned  *sample* "house-1" :budget-cents 250 :out "/tmp/eval-planned.jsonl")
;;;;   (eval-run-reviewed *sample* "house-1" :budget-cents 250 :out "/tmp/eval-reviewed.jsonl")
;;;;
;;;; FALLBACK: the house model builds; a prompt it leaves unfinished is
;;;; built afresh by the strong model in a session of its own.
;;;;
;;;; ESCALATE: the house model builds; a prompt it leaves unfinished (not
;;;; done, or no model) goes to the strong model in the lab's own loop, on a
;;;; fresh conversation over the same model file; only that costs.
;;;;
;;;; PLANNED: the strong model writes a short spec and a define-object
;;;; skeleton (one call, no tools); the house model builds from the request
;;;; and the plan, in the lab's own loop.  REVIEWED: the house model builds;
;;;; then, up to *eval-max-reviews* times, the free checks first (a model
;;;; that does not build is sent back with check_model's own words, at no
;;;; cost), else the strong model sees the request, the model file,
;;;; check_model's numbers and a render, and answers PASS or corrections,
;;;; which the house model gets as its next prompt.  Only the strong
;;;; model's tokens cost money; BUDGET-CENTS stops a run before the next
;;;; prompt once they reach it.  A reviewed prompt counts as finished only
;;;; when the reviewer said PASS.

(defparameter *eval-strong-model* "claude-opus-5-5"
  "The model that plans or reviews in the hybrid arms.")

(defparameter *eval-max-reviews* 3
  "Review rounds at most for one prompt in the reviewed arm.")

(defparameter *eval-engine-grammar*
  "Gendl grammar: (define-object model (base-object) :input-slots ((name default) ...) :computed-slots ((name expression) ...) :objects ((part :type 'box :width ... :length ... :height ... :center (translate (the center) :up ...)) ...)), the keyword pairs never wrapped in one more list; slots are read with (the name), a sequence member's index with (the-child index) under :sequence (:size n).  MODEL's own width, length and height are its envelope, a box centered on (the center): every part goes inside it, placed by its height above the floor -- a part of height h whose bottom is z up: :center (translate (the center) :down (half (the height)) :up (+ z (half h))).  This engine has box (width X, length Y, height Z), cylinder and cone (axis along their own Y; upright with :orientation (alignment :rear (the (face-normal-vector :top)))), sphere and global-polygon-projection; no booleans."
  "What the strong model is told of Gendl when it plans or reviews.")

(defparameter *eval-plan-system*
  (format nil "You plan a parametric Gendl model for a less experienced modeling agent, which builds it in the prompt lab with tools that compile and check it.  Given the design request, write (1) a short spec: the overall envelope in mm (x y z), each part with its size and where it sits relative to the model's center, and the few inputs a person would want to change; and (2) a skeleton of MODEL as one define-object that compiles: input-slots with defaults, computed-slots for derived values, and every part under :objects with its :type, sizes and :center.  The builder completes and corrects it.  ~a  Keep the skeleton under 40 lines and close every form -- the builder copies it, and an unclosed skeleton is its most common failure; no prose beyond the spec."
          *eval-engine-grammar*))

(defparameter *eval-review-system*
  (format nil "You review a parametric Gendl model that a less experienced modeling agent built for a design request.  You get the request, the model file, the lab's check numbers and a wireframe render (isometric and three orthographic views).  If the model builds what was asked -- the right parts, in the right places, at plausible sizes -- answer PASS on the first line and nothing else.  Otherwise answer CORRECTIONS on the first line, then at most five numbered corrections the builder can act on: which part, what is wrong, and the fix in Gendl terms.  No praise.  ~a"
          *eval-engine-grammar*))

(defun eval-ask (model system content &key (max-tokens 4000) (label "eval-ask"))
  "One Messages call to MODEL through the lab's gate, no tools, low effort:
SYSTEM, and CONTENT (a list of content blocks, hash tables) as the user's
turn.  Values: the reply's text, and its usage as the lab's plist
(:input :output :cache-read :cache-write)."
  (let ((body (with-output-to-string (s)
                (yason:encode (eval-call '#:h "model" model "max_tokens" max-tokens
                                         "output_config" (eval-call '#:h "effort" "low")
                                         "system" system
                                         "messages" (list (eval-call '#:h "role" "user" "content" content)))
                              s))))
    (multiple-value-bind (status text)
        (eval-call '#:post-json (symbol-value (eval-lab '#:*messages-url*)) body
                   :headers (list (cons "anthropic-version" "2023-06-01")
                                  (cons (symbol-value (eval-lab '#:*session-header*)) label))
                   :seconds 300)
      (let ((json (ignore-errors (yason:parse text))))
        (unless (and (eql status 200) (hash-table-p json))
          (error "~a answered ~a: ~a" model status (subseq text 0 (min 300 (length text)))))
        (let ((usage (gethash "usage" json)))
          (values (format nil "~{~a~}" (loop for block in (coerce (gethash "content" json) 'list)
                                             when (equal (eval-get block "type") "text")
                                               collect (eval-get block "text")))
                  (list :input (or (eval-get usage "input_tokens") 0)
                        :output (or (eval-get usage "output_tokens") 0)
                        :cache-read (or (eval-get usage "cache_read_input_tokens") 0)
                        :cache-write (or (eval-get usage "cache_creation_input_tokens") 0))))))))

(defun eval-text-block (text) (eval-call '#:h "type" "text" "text" text))

(defun eval-check-text (session)
  "check_model's own words for SESSION's model."
  (format nil "~{~a~}" (mapcar (lambda (b) (or (cdr (assoc "text" b :test #'string=)) ""))
                               (eval-call '#:check-model session))))

(defun eval-render-block (session)
  "A render of SESSION's model for the reviewer, as a content block, or nil."
  (let ((block (first (ignore-errors (eval-call '#:render session :layout "isometric-plus-ortho")))))
    (and block (equal (cdr (assoc "type" block :test #'string=)) "image")
         (eval-call '#:->json block))))

(defun eval-hybrid-run (sample builder mode &key budget-cents out (net 230) (strong *eval-strong-model*))
  "The hybrid arm MODE, :planned, :reviewed, :escalate or :fallback, on SAMPLE with BUILDER as the
house model and STRONG planning or reviewing.  Results as eval-run's, with
:mode, :strong-cents, :reviews and :verdict."
  (let ((spent 0) (results nil) (index 0))
    (dolist (item sample)
      (when (and budget-cents (>= spent budget-cents)) (return))
      (let* ((address (format nil "10.~d.~d.~d" net (floor index 250) (1+ (mod index 250))))
             (session (progn (incf index) (eval-call '#:open-session! address nil)))
             (strong-usage nil) (reviews 0) (verdict nil)
             (prompt (getf item :prompt)))
        (unless session (error "The lab would not open a session for ~a." address))
        (flet ((ask (system content max-tokens label)
                 (multiple-value-bind (text usage) (eval-ask strong system content :max-tokens max-tokens :label label)
                   (setq strong-usage (loop for (key value) on usage by #'cddr
                                            append (list key (+ value (or (getf strong-usage key) 0)))))
                   text))
               (built? () (and (ignore-errors (eval-call '#:model-defined? session)) t)))
          (unwind-protect
               (let ((start (get-internal-real-time))
                     (started (eval-call '#:utc-time (get-universal-time)))
                     (id (eval-call '#:session-id session)))
                 (eval-call '#:set-session-model! session builder)
                 (ecase mode
                   (:planned
                    (let ((plan (ask *eval-plan-system* (list (eval-text-block prompt)) 8000 (format nil "plan-~a" id))))
                      (eval-call '#:run-prompt session
                                 (format nil "~a~%~%An engineer's plan for this request -- a spec and a skeleton of MODEL that compiles as a start; follow it, complete it and check it:~%~%~a" prompt plan))))
                   (:reviewed
                    (eval-call '#:run-prompt session prompt)
                    (loop while (< reviews *eval-max-reviews*)
                          do (incf reviews)
                             (let ((correction
                                     (if (not (built?))
                                         ;; the free check first: no strong model for a model that does not build
                                         (format nil "The model does not build yet.  check_model says:~%~a~%Fix it." (eval-check-text session))
                                         (let ((answer (ask *eval-review-system*
                                                            (remove nil (list (eval-text-block (format nil "The request:~%~a~%~%The model file:~%~a~%~%check_model:~%~a"
                                                                                                       prompt (eval-call '#:model-body session) (eval-check-text session)))
                                                                              (eval-render-block session)))
                                                            2000 (format nil "review-~a-~d" id reviews))))
                                           (if (eql 0 (search "PASS" (string-left-trim '(#\Space #\Newline) answer)))
                                               (progn (setq verdict :pass) nil)
                                               (format nil "A reviewer looked at your model and the render.  Make these corrections, then check_model:~%~a" answer))))))
                               (if correction
                                   (eval-call '#:run-prompt session correction)
                                   (return))))
                    (unless verdict (setq verdict :not-passed)))
                   (:fallback
                    ;; the house builds; what it leaves unfinished the strong model
                    ;; builds AFRESH, in a session of its own -- the house's file
                    ;; costs it nothing to read or repair
                    (eval-call '#:run-prompt session prompt)
                    (let ((last (find-if (lambda (e) (member (eval-log-kind e) '(:done :stopped)))
                                         (reverse (coerce (eval-call '#:session-log session) 'list)))))
                      (if (and last (eq (eval-log-kind last) :done) (built?))
                          (setq verdict :house)
                          (let ((fresh (eval-call '#:open-session! (format nil "10.~d.250.~d" net (mod index 250)) nil)))
                            (setq verdict :fallback)
                            (incf reviews)
                            (unwind-protect
                                 (progn
                                   (eval-call '#:set-session-model! fresh strong)
                                   (eval-call '#:run-prompt fresh prompt)
                                   (setq strong-usage (eval-call '#:session-usage fresh))
                                   ;; the result is the fresh session's
                                   (let ((last (find-if (lambda (e) (member (eval-log-kind e) '(:done :stopped)))
                                                        (reverse (coerce (eval-call '#:session-log fresh) 'list)))))
                                     (setq verdict (if (and last (eq (eval-log-kind last) :done)
                                                            (ignore-errors (eval-call '#:model-defined? fresh)))
                                                       :fallback-done :fallback-failed))))
                              (ignore-errors (eval-call '#:delete-session fresh)))))))
                   (:escalate
                    ;; the house builds; only what it leaves unfinished goes to the
                    ;; strong model, in the lab's own loop, on a fresh conversation
                    ;; over the same model file (so it does not read the house's
                    ;; whole transcript)
                    (eval-call '#:run-prompt session prompt)
                    (let ((last (find-if (lambda (e) (member (eval-log-kind e) '(:done :stopped)))
                                         (reverse (coerce (eval-call '#:session-log session) 'list)))))
                      (if (and last (eq (eval-log-kind last) :done) (built?))
                          (setq verdict :house)
                          ;; a copy: the lab adds to the usage plist in place (agent.lisp add-usage),
                          ;; so the list itself would read as 'after' too and the difference as 0
                          (let ((before (copy-list (eval-call '#:session-usage session))))
                            (setq verdict :escalated)
                            (incf reviews)
                            (eval-call '#:set-session-model! session strong)
                            (funcall (fdefinition (list 'setf (eval-lab '#:session-messages))) nil session)
                            (eval-call '#:run-prompt session
                                       (format nil "The visitor asked: ~a~%~%Another agent started this and left it unfinished.  read_model, then fix and finish the model." prompt))
                            (let ((after (eval-call '#:session-usage session)))
                              (setq strong-usage
                                    (loop for key in '(:input :output :cache-read :cache-write)
                                          append (list key (- (or (getf after key) 0) (or (getf before key) 0)))))))))))
                 (let* ((log (coerce (eval-call '#:session-log session) 'list))
                        (last-kind (let ((entry (find-if (lambda (e) (member (eval-log-kind e) '(:done :stopped))) (reverse log))))
                                     (and entry (eval-log-kind entry))))
                        (usage (eval-call '#:session-usage session))
                        (strong-cents (eval-cents strong strong-usage))
                        (result (list :id (getf item :id) :model builder :mode mode :strong strong
                                      :session id :started started :ended (eval-call '#:utc-time (get-universal-time))
                                      :finished (if (eq mode :fallback)
                                                    (and (member verdict '(:house :fallback-done)) t)
                                                    (and (eq last-kind :done) (built?) (not (eq verdict :not-passed))))
                                      :built (if (eq mode :fallback)
                                                 (or (built?) (eq verdict :fallback-done))
                                                 (built?))
                                      :verdict verdict :reviews reviews
                                      :seconds (float (/ (- (get-internal-real-time) start) internal-time-units-per-second) 1.0)
                                      :rounds (count :tool log :key #'eval-log-kind)
                                      :input (getf usage :input) :output (getf usage :output)
                                      :strong-input (getf strong-usage :input) :strong-output (getf strong-usage :output)
                                      :cents (float strong-cents 1.0)
                                      :archived-outcome (getf item :archived-outcome))))
                   (incf spent strong-cents)
                   (push result results)
                   (when out
                     (with-open-file (stream out :direction :output :if-exists :append :if-does-not-exist :create
                                                 :external-format :utf-8)
                       (yason:encode (eval-json-object result) stream)
                       (terpri stream)))))
            (ignore-errors (eval-call '#:delete-session session))))))
    (nreverse results)))

(defun eval-run-planned (sample builder &rest keys)
  (apply #'eval-hybrid-run sample builder :planned keys))

(defun eval-run-reviewed (sample builder &rest keys)
  (apply #'eval-hybrid-run sample builder :reviewed keys))
