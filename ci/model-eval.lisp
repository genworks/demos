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

(defparameter *eval-results* nil
  "The results each trial prompt names, for the eval only (Ruling 145): an
alist from a sample id to a list of (LABEL ALTERNATIVE ...), where a result
is present when one of MODEL's own computed slots has an ALTERNATIVE in its
name.  Loose by design -- a slot named spring-rate or rate-n-per-mm both
answer (\"spring rate\" \"rate\") -- since the agent names its slots.
Set with eval-load-results; visitors' sessions are never judged by it.")

(defun eval-load-results (file)
  "Read FILE, a list of (:id ID :results ((LABEL ALTERNATIVE ...) ...)), into
*eval-results*."
  (setq *eval-results*
        (with-open-file (in file :external-format :utf-8)
          (with-standard-io-syntax
            (let ((*read-eval* nil))
              (mapcar (lambda (entry) (cons (getf entry :id) (getf entry :results)))
                      (read in)))))))

(defun eval-results-for (id) (cdr (assoc id *eval-results* :test #'equal)))

(defun eval-model-slot-names (session)
  "The names of MODEL's own computed slots, lower case; nil when it does
not build."
  (ignore-errors
   (let ((model (eval-call '#:make-model session)))
     (mapcar (lambda (key) (string-downcase (symbol-name key)))
             (the-object model (message-list :category :computed-slots :message-type :local))))))

(defun eval-results-missing (session results)
  "The labels of RESULTS (as in *eval-results*) that MODEL has no computed
slot for."
  (let ((names (eval-model-slot-names session)))
    (loop for (label . alternatives) in results
          unless (some (lambda (alt) (some (lambda (name) (search alt name)) names)) alternatives)
            collect label)))

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

(defparameter *eval-followup-names* 6
  "Integer.  At most this many findings are named in one scripted
follow-up line (eval-followup-text).")

(defun eval-last-kind (session)
  "The kind, :done or :stopped, of SESSION's latest ending; nil if none."
  (let ((entry (find-if (lambda (e) (member (eval-log-kind e) '(:done :stopped)))
                        (reverse (coerce (eval-call '#:session-log session) 'list)))))
    (and entry (eval-log-kind entry))))

(defun eval-model-state (session named)
  "What SESSION's model is now, judged by the MODEL ALONE: no closing reply
is asked for, so a sound model cut off at a round cap is sound.  A plist:
:sound true when MODEL is defined, has at least one part, the lab's
completion check finds nothing (it is asked whatever *completion-check?*
says) and no result the prompt names (NAMED) is missing; else what is
wrong, by name -- :no-model, :no-parts, :error, :broken, :failing,
:missing."
  (if (not (ignore-errors (eval-call '#:model-defined? session)))
      (list :sound nil :no-model t)
      (let* ((findings (progv (list (eval-lab '#:*completion-check?*)) (list t)
                         (ignore-errors (eval-call '#:completion-findings session))))
             (missing (and named (eval-results-missing session named)))
             ;; an empty MODEL is its own one leaf: it has no parts when it has
             ;; no children and no size of its own (a MODEL that is itself a
             ;; box, with no children, has parts)
             (no-parts (and (not (getf findings :error))
                            (ignore-errors
                             (let* ((model (eval-call '#:make-model session))
                                    (box (the-object model bounding-box)))
                               (and (null (the-object model children))
                                    (or (null box)
                                        (every (lambda (low high) (< (abs (- high low)) 1e-9))
                                               (first box) (second box)))))))))
        (list :sound (not (or (getf findings :error) (getf findings :broken) (getf findings :failing)
                              (getf findings :unjudged) missing no-parts))
              :error (getf findings :error) :broken (getf findings :broken)
              :failing (getf findings :failing) :missing missing :no-parts no-parts
              ;; the check ran out of time: not known to be sound, and nothing
              ;; found wrong either
              :unjudged (getf findings :unjudged)))))

(defun eval-built? (session)
  "True when SESSION has built something: MODEL is defined and is not the
empty skeleton a structure-only session starts from (the lab's own test,
completion-findings' :empty, asked whatever *completion-check?* says).  A
session that did nothing used to count as built in such arms, so the
figure was 100 whatever happened."
  (and (ignore-errors (eval-call '#:model-defined? session))
       (not (getf (progv (list (eval-lab '#:*completion-check?*)) (list t)
                    (ignore-errors (eval-call '#:completion-findings session)))
                  :empty))
       t))

(defun eval-verdict-pass? (answer)
  "Whether a reviewer's ANSWER says PASS: the word PASS, upper case, first
on its first line once markup and a 'Verdict:' label are set aside, and
not the start of a longer word.  A bare prefix test read **PASS** and
'Verdict: PASS' as corrections, and PASSABLE as a pass."
  (let* ((line (string-trim '(#\Space #\Tab #\Return)
                            (let ((text (string-left-trim '(#\Space #\Tab #\Newline #\Return) (or answer ""))))
                              (subseq text 0 (or (position #\Newline text) (length text))))))
         (line (string-left-trim "*_#>`\"' " line))
         (line (if (and (>= (length line) 8) (string-equal "verdict:" line :end2 8))
                   (string-left-trim "*_`\"' " (subseq line 8))
                   line)))
    (and (>= (length line) 4)
         (string= "PASS" line :end2 4)
         (or (= (length line) 4) (not (alphanumericp (char line 4)))))))

(defun eval-state-text (state)
  "STATE (eval-model-state) in a few words, for the run's record."
  (cond ((getf state :sound) "sound")
        ((getf state :no-model) "no model")
        (t (format nil "~{~a~^; ~}"
                   (remove nil
                           (list (and (getf state :error) "MODEL does not build")
                                 (and (getf state :no-parts) "no parts")
                                 (and (getf state :broken)
                                      (format nil "~d part~:p not computing" (length (getf state :broken))))
                                 (and (getf state :failing)
                                      (format nil "~d slot~:p failing" (length (getf state :failing))))
                                 (and (getf state :missing)
                                      (format nil "~d named result~:p missing" (length (getf state :missing))))
                                 (and (getf state :unjudged) "check timed out")))))))

(defun eval-complete? (session named)
  "True when SESSION's model is sound (eval-model-state): judged by the
model's state, not by whether the agent closed with a reply."
  (and (getf (eval-model-state session named) :sound) t))

(defun eval-followup-text (state)
  "A visitor's next line for a model that is not sound, scripted from STATE
(eval-model-state): the lab's findings and the results the prompt names.
No model writes it; at most *eval-followup-names* findings are named.  NIL
for a sound model: nothing is asked of a model with nothing wrong -- a line
telling it to carry on had it edit sound models into broken ones."
  (cond
    ((getf state :sound) nil)
    ((getf state :no-model)
     "Not done yet: there is no MODEL yet.  Build what the request asks, and reply.")
    (t
     (let ((budget *eval-followup-names*)
           (parts nil))
       (flet ((take (names)
                (let ((some (subseq names 0 (min budget (length names)))))
                  (decf budget (length some))
                  some)))
         (when (getf state :error)
           (decf budget)
           ;; clipped: an untrimmed error text made a line long enough to stage
           (push (format nil "MODEL does not build (~a)"
                         (let ((text (princ-to-string (getf state :error))))
                           (subseq text 0 (min 200 (length text)))))
                 parts))
         (when (getf state :no-parts)
           (push "MODEL has no parts yet" parts))
         (let ((broken (take (getf state :broken))))
           (when broken (push (format nil "these parts' geometry does not compute: ~{~a~^, ~}" broken) parts)))
         (let ((failing (take (getf state :failing))))
           (when failing (push (format nil "these computed slots of MODEL fail: ~{~a~^, ~}" failing) parts)))
         (let ((absent (take (getf state :missing))))
           (when absent
             (push (format nil "these results are missing -- add each as a computed slot of MODEL with its unit: ~{~a~^, ~}" absent)
                   parts))))
       ;; nothing found wrong -- the check only ran out of time: no line to send
       (and parts
            (format nil "Not done yet: ~{~a~^; ~}.  Fix those and reply." (nreverse parts)))))))

(defun eval-run (sample model &key budget-cents out (net 250) (followups 0) followup-rounds)
  "Replay SAMPLE's prompts on MODEL, one fresh session each, until the cost
reaches BUDGET-CENTS (nil: no ceiling).  Each session opens from an address
of its own in 10.NET.x.y, so the lab's sessions-per-address cap never stops
a run.  Each result is written to OUT (a JSON-lines file, appended) when
given.  Answers the results, plists.  FOLLOWUPS (Ruling 153): up to that
many scripted visitor lines (eval-followup-text) while the build is not
complete; the result says whether it was complete after the build and
after each line sent.  FOLLOWUP-ROUNDS caps each follow-up's rounds apart
from the build's."
  (let ((spent 0) (results nil) (index 0))
    (dolist (item sample)
      (when (and budget-cents (>= spent budget-cents)) (return))
      (let* ((address (format nil "10.~d.~d.~d" net (floor index 250) (1+ (mod index 250))))
             (session (progn (incf index) (eval-call '#:open-session! address nil))))
        (unless session (error "The lab would not open a session for ~a." address))
        (unwind-protect
             (let ((start (get-internal-real-time))
                   (started (eval-call '#:utc-time (get-universal-time)))
                   ;; the model's state after the build, then after each follow-up sent
                   (states nil))
               (eval-call '#:set-session-model! session model)
               (eval-call '#:run-prompt session (getf item :prompt))
               ;; scripted follow-ups while the model is not sound (Rulings 153, 166):
               ;; judged by the model's state, and never sent to a sound model
               (let ((named (eval-results-for (getf item :id)))
                     (staging (find-symbol (symbol-name '#:*auto-stage*) :prompt-lab)))
                 (push (eval-model-state session named) states)
                 (loop repeat followups
                       for line = (eval-followup-text (first states))
                       until (getf (first states) :sound)
                       ;; nothing to ask (the check only timed out)
                       while line
                       ;; the ceiling holds between follow-ups too, not only
                       ;; between prompts: this session's cost so far counts
                       until (and budget-cents
                                  (>= (+ spent (eval-cents model (eval-call '#:session-usage session)))
                                      budget-cents))
                       do (progn
                            ;; a follow-up is one prompt: never staged, whatever its length
                            ;; FOLLOWUP-ROUNDS: a follow-up's own round cap, where the
                            ;; build was given a larger one (an equal-budget arm)
                            (progv (append (and staging (list staging))
                                           (and followup-rounds
                                                (list (eval-lab '#:*house-max-rounds*) (eval-lab '#:*max-rounds*))))
                                (append (and staging (list nil))
                                        (and followup-rounds (list followup-rounds followup-rounds)))
                              (eval-call '#:run-prompt session line)))
                          (push (eval-model-state session named) states))
                 (setq states (reverse states)))
               (let* ((log (eval-call '#:session-log session))
                      (last-kind (let ((entry (find-if (lambda (e) (member (eval-log-kind e) '(:done :stopped)))
                                                       (reverse (coerce log 'list)))))
                                   (and entry (eval-log-kind entry))))
                      (usage (eval-call '#:session-usage session))
                      (cents (eval-cents model usage))
                      ;; the results the prompt names, where the eval knows them (Ruling 145)
                      (named (eval-results-for (getf item :id)))
                      (missing (and named (eval-results-missing session named)))
                      (result (list :id (getf item :id) :model model
                                    :results-named (length named)
                                    :results-missing (format nil "~{~a~^; ~}" missing)
                                    ;; by the model's state at the end (eval-model-state);
                                    ;; :finished below is the agent's closing reply
                                    :complete (and (getf (car (last states)) :sound) t)
                                    ;; after the build, then after each follow-up sent
                                    :complete-after (format nil "~{~:[no~;yes~]~^,~}"
                                                            (mapcar (lambda (s) (getf s :sound)) states))
                                    ;; what the check found at each of those steps
                                    :step-findings (format nil "~{~a~^ | ~}" (mapcar #'eval-state-text states))
                                    :followups-sent (max 0 (1- (length states)))
                                    ;; when and as which session, to pair with the server's own timings
                                    :session (eval-call '#:session-id session)
                                    :started started :ended (eval-call '#:utc-time (get-universal-time))
                                    :finished (eq last-kind :done)
                                    :built (eval-built? session)
                                    :seconds (float (/ (- (get-internal-real-time) start) internal-time-units-per-second) 1.0)
                                    ;; TOOL CALLS, not rounds: parallel calls in one
                                    ;; round count each (the name is kept for old files)
                                    :rounds (count :tool (coerce log 'list) :key #'eval-log-kind)
                                    :tool-calls (count :tool (coerce log 'list) :key #'eval-log-kind)
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
how many, the share finished (the agent closed with a reply), built (a MODEL
is defined) and complete (the model is sound), the median seconds (the upper
of the two middle ones for an even count), the mean tool calls a prompt
(:mean-rounds, its old name), the cents in all.  A plist."
  (let* ((rows (if (or (stringp results) (pathnamep results))
                   (with-open-file (in results :external-format :utf-8)
                     (loop for line = (read-line in nil nil) while line
                           collect (let ((h (yason:parse line)))
                                     (list :finished (gethash "finished" h) :built (gethash "built" h)
                                           :complete (eq (gethash "complete" h) t)
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
              ;; by the model's state (eval-model-state), where the rows carry it
              :complete-percent (round (* 100 (count-if (lambda (r) (getf r :complete)) rows)) n)
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
               (built? () (eval-built? session)))
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
                                           (if (eval-verdict-pass? answer)
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
                        ;; the strong model's tokens, and the builder's when it costs
                        ;; money too (a hosted model as the first try, not a house one);
                        ;; under :escalate the builder's session also holds the strong
                        ;; model's usage, so its own share is the difference
                        (strong-cents (+ (eval-cents strong strong-usage)
                                         (eval-cents builder
                                                     (loop for key in '(:input :output :cache-read :cache-write)
                                                           append (list key (max 0 (- (or (getf usage key) 0)
                                                                                      (if (eq mode :escalate)
                                                                                          (or (getf strong-usage key) 0)
                                                                                          0))))))))
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
