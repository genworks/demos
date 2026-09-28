;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The archive: what every session produced, kept where the reaper never
;; looks.  A session's directory under *workspace-root* is the visitor's
;; workspace and goes when the session does; the archive is the
;; operator's record -- every prompt, every version of the model file as
;; it was written (compiling or not), the agent's replies and its tool
;; calls -- for reviewing what visitors ask for, what the agent builds,
;; and the style of the code it writes.
;;
;; One directory per session, <*archive-root*>/<YYYY-MM-DD>/<id>/ (the
;; UTC day the session opened), holding:
;;   session.json    the record, as save-session! writes it
;;   transcript.md   the conversation, readable: each prompt, the agent's
;;                   text and thinking, each tool call with its source,
;;                   each result
;;   model.lisp      the model file as it last was
;;   model-NNN.lisp  every distinct version of it, in the order written
;;
;; Written after every prompt and every edit (save-session!) and once
;; more before a session is deleted; a version of the model file is kept
;; the moment it is loaded (load-model-file), so one that failed to
;; compile is kept too.  Nothing here signals: the archive must never
;; cost a visitor anything.
;;

(defun utc-day (universal-time)
  (multiple-value-bind (s m h day month year) (decode-universal-time universal-time 0)
    (declare (ignore s m h))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun utc-time (universal-time)
  (multiple-value-bind (s m h day month year) (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d UTC" year month day h m s)))

(defun archive-directory-for (id created)
  "The archive directory of session ID opened at CREATED, or nil when
there is no archive."
  (when (and *archive-root* (stringp id) (integerp created))
    (merge-pathnames (format nil "~a/~a/" (utc-day created) id) *archive-root*)))

(defun archive-directory (session)
  (archive-directory-for (session-id session) (session-created session)))

(defun same-text-file? (a b)
  (and (probe-file a) (probe-file b)
       (string= (uiop:read-file-string a :external-format :utf-8)
                (uiop:read-file-string b :external-format :utf-8))))

(defun write-text-file (text file)
  "Write TEXT to FILE atomically, UTF-8."
  (let ((tmp (make-pathname :type "tmp" :defaults file)))
    (ensure-directories-exist file)
    (with-open-file (out tmp :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text out))
    (when (probe-file file) (delete-file file))
    (rename-file tmp file)
    file))

(defun archive-model-file! (file directory)
  "Keep the model FILE in DIRECTORY: as model.lisp, and as the next
model-NNN.lisp when it differs from the newest version kept there.
Never signals."
  (ignore-errors
   (when (and directory (probe-file file))
     (ensure-directories-exist directory)
     (let* ((versions (sort (mapcar #'namestring (directory (merge-pathnames "model-*.lisp" directory)))
                            #'string<))
            (newest (car (last versions))))
       (unless (and newest (same-text-file? file newest))
         (uiop:copy-file file (merge-pathnames (format nil "model-~3,'0d.lisp" (1+ (length versions)))
                                               directory))))
     (uiop:copy-file file (merge-pathnames "model.lisp" directory))
     directory)))

(defun archive-model! (session)
  "Keep SESSION's model file as it is now (see archive-model-file!)."
  (archive-model-file! (session-model-file session) (archive-directory session)))


;;
;; The transcript.
;;

(defun json-true? (value)
  (and value (not (eq value 'yason:false)) (not (eq value 'yason:null))))

(defun write-tool-use (out block)
  (let* ((name (gethash "name" block))
         (input (gethash "input" block))
         (source (and (hash-table-p input) (gethash "source" input)))
         (expression (and (hash-table-p input) (gethash "expression" input))))
    (cond ((and (equal name "write_model") (stringp source))
           (format out "### Tool: write_model~%~%```lisp~%~a~%```~%~%" source))
          ((and (equal name "evaluate") (stringp expression))
           (format out "### Tool: evaluate~%~%```lisp~%~a~%```~%~%" expression))
          (t (format out "### Tool: ~a~%~%~a~%~%" name
                     (if (and (hash-table-p input) (plusp (hash-table-count input)))
                         (encode input)
                         ""))))))

(defun write-tool-result (out block)
  (let ((content (gethash "content" block)))
    (format out "#### Result~:[~; (error)~]~%~%" (json-true? (gethash "is_error" block)))
    (cond ((stringp content) (format out "```~%~a~%```~%~%" content))
          ((listp content)
           (dolist (b content)
             (when (hash-table-p b)
               (let ((type (gethash "type" b)))
                 (cond ((equal type "text") (format out "```~%~a~%```~%~%" (gethash "text" b)))
                       ((equal type "image") (format out "[image]~%~%"))
                       (t (format out "[~a]~%~%" type))))))))))

(defun write-content-block (out block)
  (when (hash-table-p block)
    (let ((type (gethash "type" block)))
      (cond ((equal type "text") (format out "### Agent~%~%~a~%~%" (gethash "text" block)))
            ((equal type "thinking")
             ;; an empty thinking block (the API returns those) says nothing
             (let ((thinking (gethash "thinking" block)))
               (when (and (stringp thinking) (plusp (length (string-trim '(#\space #\newline) thinking))))
                 (format out "### Agent (thinking)~%~%~a~%~%" thinking))))
            ((equal type "tool_use") (write-tool-use out block))
            ((equal type "tool_result") (write-tool-result out block))
            ((equal type "image") (format out "[image]~%~%"))
            (t (format out "[~a]~%~%" type))))))

(defun transcript-text (session)
  "SESSION's conversation as Markdown: a header of the numbers, then
every message in order."
  (let ((usage (session-usage session)))
    (with-output-to-string (out)
      (format out "# Prompt lab session ~a~%~%" (session-id session))
      (format out "- opened: ~a~%- last used: ~a~%- visitor: ~a~%- prompts: ~a~%~
- tokens: input ~:d, output ~:d, cache read ~:d, cache write ~:d~%~
- spend: ~,2f cents~@[, allowance ~a~]~@[, wallet ~a~]~%~%"
              (utc-time (session-created session))
              (utc-time (session-last-used session))
              (or (session-address session) "unknown")
              (count :prompt (session-log session) :key #'second)
              (or (getf usage :input) 0) (or (getf usage :output) 0)
              (or (getf usage :cache-read) 0) (or (getf usage :cache-write) 0)
              (or (session-cents session) 0)
              (session-allowance session)
              (session-wallet session))
      (let ((n 0))
        (dolist (message (session-messages session))
          (when (hash-table-p message)
            (let ((content (gethash "content" message)))
              (cond ((stringp content)
                     (format out "## Prompt ~a~%~%~a~%~%" (incf n) content))
                    ((listp content)
                     (dolist (block content) (write-content-block out block))))))))
      (let ((stopped (remove-if-not #'(lambda (entry) (member (second entry) '(:stopped :tool-error)))
                                    (session-log session))))
        (when stopped
          (format out "## Stops and tool errors~%~%")
          (dolist (entry stopped)
            (format out "- ~a ~(~a~): ~a~%" (utc-time (first entry)) (second entry) (third entry)))
          (terpri out))))))

(defun archive-session! (session)
  "Write SESSION's record, transcript and model file to the archive.
Never signals; nil when there is no archive."
  (ignore-errors
   (let ((directory (archive-directory session)))
     (when directory
       (ensure-directories-exist directory)
       (let ((record (session-state-file session)))
         (when (probe-file record)
           (uiop:copy-file record (merge-pathnames "session.json" directory))))
       (write-text-file (transcript-text session) (merge-pathnames "transcript.md" directory))
       (archive-model! session)
       directory))))

(defun archive-stale-directory! (directory)
  "Keep what a session directory no live session owns still holds (a
visitor's edit made in the terminal after the last save): its model
file, in the archive directory its record names.  Never signals."
  (ignore-errors
   (let* ((record (merge-pathnames "session.json" directory))
          (json (and (probe-file record)
                     (with-open-file (in record :external-format :utf-8)
                       (let ((yason:*parse-json-booleans-as-symbols* t)) (yason:parse in)))))
          (id (and (hash-table-p json) (gethash "id" json)))
          (created (and (hash-table-p json) (gethash "created" json))))
     (when (and (stringp id) (integerp created))
       (let ((target (archive-directory-for id created)))
         (when target
           (unless (probe-file (merge-pathnames "session.json" target))
             (ensure-directories-exist target)
             (uiop:copy-file record (merge-pathnames "session.json" target)))
           (archive-model-file! (merge-pathnames "model.lisp" directory) target)))))))
