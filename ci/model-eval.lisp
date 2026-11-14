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
             (let ((start (get-internal-real-time)))
               (eval-call '#:set-session-model! session model)
               (eval-call '#:run-prompt session (getf item :prompt))
               (let* ((log (eval-call '#:session-log session))
                      (last-kind (let ((entry (find-if (lambda (e) (member (eval-log-kind e) '(:done :stopped)))
                                                       (reverse (coerce log 'list)))))
                                   (and entry (eval-log-kind entry))))
                      (usage (eval-call '#:session-usage session))
                      (cents (eval-cents model usage))
                      (result (list :id (getf item :id) :model model
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
