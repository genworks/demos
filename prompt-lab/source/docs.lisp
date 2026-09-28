;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The agent's two doors to the reference (2026-09-27/28, the user: the
;; agent guesses at what a type takes; give it what an Emacs user has).
;;
;; search_docs asks the ship's READY ROOM: the Emacs next door carries
;; the lisply_search corpus (the Gendl sources with their documentation
;; plists, the guides, the demos), and its lisply door takes an
;; expression over plain HTTP on the ship network.  The lab POSTs
;; (lisply-search ...) there through the same in-process client the
;; gate calls use and hands the hits back as text: file, line, score,
;; snippet.  A room without a ready room (*search-url* nil) withholds
;; the tool, as the render tool is withheld without zpng.  One thing to
;; know about that Emacs: it is single-threaded and it is the humans'
;; console too, so a search costs the console a fraction of a second
;; each; *search-seconds* bounds the wait.
;;
;; describe_object asks the ROOM ITSELF: a prototype of the type is made
;; and its message list read by category, each message with the
;; documentation string its defining class gave it -- the walk glime's
;; SLIME completion does (gendl/emacs/glime.lisp), done here with the
;; core messages it rests on (message-list, message-documentation) so
;; nothing of swank is needed at request time.  No network, no meter:
;; a prototype is made, nothing is evaluated.
;;

(defparameter *search-url* nil
  "String or nil. The lisply door of a ready room on the ship, e.g.
\"http://ready-room:7080/lisply/lisp-eval\": where search_docs runs
lisply-search.  Nil withholds the tool.")

(defparameter *search-seconds* 20
  "Integer. Seconds one search may take.")

(defparameter *search-hits* 5
  "Integer. Hits a search returns unless the agent asks for another number (at most 20).")

(defparameter *search-snippet-tokens* 120
  "Integer. Length of each hit's snippet, in the corpus's tokens.")

(defparameter *search-query-length* 300
  "Integer. Characters a query may have.")

(defparameter *describe-base-types*
  '((:surf :brep
     "As a brep solid it also takes brep-tolerance and tessellation-parameters, answers volume, and takes and answers everything a geometric object does: center, orientation, width, length, height, display-controls (e.g. (list :color :steelblue)), bounding-box, leaves.")
    (:gendl :base-object
     "Like every geometric object it also takes center, orientation, width, length, height and display-controls (e.g. (list :color :steelblue)), and answers bounding-box and leaves."))
  "List of (package name sentence): the types at which describe_object
stops walking a type's mixins, the first one the type inherits from
winning, and the sentence that stands for everything above it.  A brep
carries a dozen tessellation and layer inputs that every solid shares
and no model sets; base-object's inputs are the same for every
geometric object.  Named as keywords, so the names wear the image's
case (a modern-mode workshop is lower case).")

(defparameter *describe-quiet-messages* '("POLYGONS-FOR-IFS")
  "List of strings. Messages describe_object leaves out although a
category lists them: internals a type computes for itself but declares
as inputs on a mixin.")

(defparameter *describe-per-category* 40
  "Integer. Messages listed per category at most.")

(defparameter *describe-doc-length* 160
  "Integer. Characters of a message's documentation quoted at most.")


;;
;; search_docs
;;

(defun search-offered? ()
  "Whether the agent gets the search_docs tool: a ready room named."
  (and *search-url* t))

(defun search-form (query hits)
  "The expression the ready room evaluates: lisply-search, its answer
encoded as JSON there so it comes back as data.  QUERY travels as a
string literal (~s escapes the quote and the backslash the way Emacs
reads them), so a query cannot become code."
  (format nil "(json-encode (let ((r (lisply-search '(:query ~s :k ~a :max-snippet-tokens ~a)))) (list :hits (plist-get r :hits) :warning (plist-get r :warning))))"
          query hits *search-snippet-tokens*))

(defun collapse-whitespace (text)
  "TEXT with runs of whitespace as one space, trimmed."
  (let ((out (make-string-output-stream)) (space? nil))
    (loop for c across text
          do (if (member c '(#\space #\tab #\newline #\return))
                 (setq space? t)
                 (progn (when space? (write-char #\space out) (setq space? nil))
                        (write-char c out))))
    (string-trim " " (get-output-stream-string out))))

(defun search-hit-text (n hit)
  (flet ((field (name) (gethash name hit)))
    (format nil "~a. ~a:~a (~a~@[, score ~,2f~])~%~a"
            n (field "path") (field "start-line") (field "source")
            (let ((s (field "score"))) (and (realp s) s))
            (or (field "snippet") (field "preview") ""))))

(defun search-docs (query &key hits)
  "Search the corpus for QUERY through the ready room.  Values: content
blocks and an error flag (a refusal, a door that does not answer)."
  (let ((query (and (stringp query) (string-trim '(#\space #\tab #\newline #\return) query)))
        (hits (if (and (integerp hits) (< 0 hits 21)) hits *search-hits*)))
    (cond
      ((not (search-offered?))
       (values (list (text-result "There is no documentation search on this host.")) t))
      ((or (null query) (zerop (length query)))
       (values (list (text-result "Say what to search for.")) t))
      ((> (length query) *search-query-length*)
       (values (list (text-result "A query may have ~a characters at most." *search-query-length*)) t))
      (t
       (handler-case
           (multiple-value-bind (status text)
               (post-json *search-url* (encode (h "code" (search-form query hits))) :seconds *search-seconds*)
             (let* ((answer (and (eql status 200) (ignore-errors (yason:parse text))))
                    (result (and (hash-table-p answer) (gethash "result" answer)))
                    (found (and (stringp result) (ignore-errors (yason:parse result))))
                    (all (and (hash-table-p found) (gethash "hits" found)))
                    (warning (and (hash-table-p found) (gethash "warning" found))))
               (cond
                 ((not (hash-table-p answer))
                  (values (list (text-result "The documentation search did not answer (status ~a)." status)) t))
                 ((not (hash-table-p found))
                  (values (list (text-result "The documentation search failed: ~a"
                                             (or (gethash "error" answer) (gethash "result" answer) "no result")))
                          t))
                 (t
                  ;; the corpus holds some files twice; one hit per place
                  (let ((seen (make-hash-table :test #'equal)) (unique nil))
                    (dolist (hit (coerce all 'list))
                      (when (hash-table-p hit)
                        (let ((key (list (gethash "path" hit) (gethash "start-line" hit))))
                          (unless (gethash key seen)
                            (setf (gethash key seen) t)
                            (push hit unique)))))
                    (setq unique (nreverse unique))
                    (values (list (text-result "~:[No matches for ~s.~;~:*~*Matches for ~s (file:line, then the passage):~]~%~%~{~a~^~%~%~}~@[~%Note: ~a~]"
                                               unique query
                                               (loop for hit in unique for n from 1 collect (search-hit-text n hit))
                                               (and (stringp warning) warning)))
                            nil))))))
         (error (condition)
           (values (list (text-result "The documentation search could not be reached: ~a" condition)) t)))))))


;;
;; describe_object
;;

(defun find-object-type (name package)
  "The class named by NAME (a string, qualified or not) read in PACKAGE,
else looked up by name in the geometry packages; nil when none."
  (let ((symbol (ignore-errors
                 (let ((*package* package) (*read-eval* nil))
                   (read-from-string name)))))
    (when (and symbol (symbolp symbol))
      (or (find-class symbol nil)
          (loop for p in '(:surf :geom-base :gwl :gdl-user :gendl)
                for found = (and (find-package p) (find-symbol (symbol-name symbol) p))
                when (and found (find-class found nil)) return (find-class found nil))))))

(defun message-doc (object message)
  "The documentation of MESSAGE on OBJECT: values the text (nil when
none) and the name of the class that documented it."
  (let ((doc (ignore-errors (the-object object (message-documentation message)))))
    (when (consp doc)
      (values (and (stringp (second doc)) (collapse-whitespace (second doc)))
              (and (first doc) (symbol-name (first doc)))))))

(defun quiet-message? (message)
  "An internal: a %name% or one of the quiet messages."
  (let ((name (symbol-name message)))
    (or (and (plusp (length name)) (char= (char name 0) #\%))
        (member name *describe-quiet-messages* :test #'string-equal))))

(defun describe-base (type)
  "Values: the base type below which TYPE's own messages are listed (nil
for a type that inherits from none of *describe-base-types*), and the
sentence standing for the rest."
  (loop for (package name sentence) in *describe-base-types*
        for base = (loop for p in (list package :gdl-user :geom-base :gendl :surf)
                         for symbol = (and (find-package p) (find-symbol (symbol-name name) p))
                         when (and symbol (find-class symbol nil)) return symbol)
        when (and base (not (eq base type)) (subtypep type base))
          return (values base sentence)))

(defun messages-in (object category base)
  "The messages of CATEGORY on OBJECT below BASE (all of them without one), by name."
  (ignore-errors
   (let ((list (the-object object (message-list :category category :message-type :global
                                                :base-part-type base))))
     (sort (remove-if #'quiet-message? (remove-if-not #'symbolp list)) #'string< :key #'symbol-name))))

(defun describe-category (out object base title categories &key documented-only)
  "Write TITLE and the messages of CATEGORIES on OBJECT below BASE: the
documented ones each with their documentation, the rest named on one
line (or left out when DOCUMENTED-ONLY)."
  (let ((documented nil) (bare nil))
    (dolist (category categories)
      (dolist (message (messages-in object category base))
        (let ((doc (message-doc object message)))
          (cond (doc (pushnew (list message doc) documented :key #'first))
                ((not documented-only) (pushnew message bare))))))
    (setq documented (nreverse documented) bare (nreverse bare))
    (when (or documented bare)
      (format out "~%~a:~%" title)
      (loop for (message doc) in documented
            for n from 1
            while (<= n *describe-per-category*)
            do (format out "- ~(~a~) -- ~a~%" message
                       (if (> (length doc) *describe-doc-length*)
                           (format nil "~a..." (subseq doc 0 *describe-doc-length*))
                           doc)))
      (when (> (length documented) *describe-per-category*)
        (format out "- ... and ~a more~%" (- (length documented) *describe-per-category*)))
      (when bare
        (format out "- undocumented: ~{~(~a~)~^, ~}~%" bare)))))

(defun describe-type (session name)
  "Describe object type NAME for the agent.  Values: content blocks and
an error flag."
  (let ((name (and (stringp name) (string-trim '(#\space #\tab #\newline #\return) name))))
    (cond
      ((or (null name) (zerop (length name)))
       (values (list (text-result "Name the object type to describe.")) t))
      (t
       (handler-case
           (with-time-limit (*eval-seconds* "description")
             (let ((class (find-object-type name (session-package session))))
               (unless class
                 (return-from describe-type
                   (values (list (text-result "No object type named ~a is defined.  search_docs may find the name you mean." name)) t)))
               (let* ((type (class-name class))
                      (object (make-object type))
                      (description (ignore-errors (getf (the-object object documentation) :description)))
                      (mixins (ignore-errors (the-object object mixins))))
                 (multiple-value-bind (base sentence) (describe-base type)
                   (values
                    (list (text-result "~a"
                                       (with-output-to-string (out)
                                         (format out "~(~s~)~@[ (mixins: ~{~(~a~)~^, ~})~]~%~@[~a~%~]"
                                                 type (and (listp mixins) mixins)
                                                 (and (stringp description) (collapse-whitespace description)))
                                         (describe-category out object base "Required inputs" '(:required-input-slots))
                                         (describe-category out object base "Optional inputs"
                                                            '(:optional-input-slots :settable-optional-input-slots
                                                              :defaulted-input-slots :settable-defaulted-input-slots))
                                         (describe-category out object base "Computed slots (documented ones)"
                                                            '(:computed-slots :settable-computed-slots) :documented-only t)
                                         (describe-category out object base "Children" '(:objects :quantified-objects))
                                         (describe-category out object base "Functions (documented ones)"
                                                            '(:functions :methods) :documented-only t)
                                         (when sentence (format out "~%~a~%" sentence)))))
                    nil)))))
         (error (condition)
           (values (list (text-result "Could not describe ~a: ~a" name condition)) t)))))))
