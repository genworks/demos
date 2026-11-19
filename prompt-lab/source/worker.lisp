;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; One-shot labs.  A visitor's model is code, and code run in an image
;; every visitor shares can change that image for all of them.  A
;; ONE-SHOT LAB (*one-session?*) is the whole lab -- the sheet, the doors,
;; the agent's loop, every compile, check, render and download -- serving
;; exactly one session in a process of its own, typically a locked-down
;; container: a second session is refused while the first lives, and once
;; the reaper has archived and dropped it the process quits, so the
;; container starts again from its image and the next visitor meets a
;; fresh Lisp.
;;
;; Its archive is its own directory.  The lab that shows the public
;; archive copies those sessions in (*archive-imports*,
;; import-archives!), each marked with where it came from, so that no
;; one-shot lab can write over a session that is not its own.
;;

(defvar *one-session-opened* nil
  "The id of the session a one-shot lab has served, once it has opened one.")

(defun one-session-taken? ()
  "True once a one-shot lab has opened its session: it opens no other."
  (and *one-session-opened* t))

(defun note-one-session! (session)
  (setf *one-session-opened* (session-id session)))

(defun one-session-check! ()
  "In a one-shot lab whose session has been reaped (and so archived),
quit: the container's restart brings a fresh image."
  (when (and *one-session?* *one-session-opened*
             (zerop (bt:with-lock-held (*sessions-lock*) (hash-table-count *sessions*))))
    (format *error-output* "~&prompt-lab: session ~a is done; quitting so a fresh lab takes its place~%"
            *one-session-opened*)
    (finish-output *error-output*)
    (uiop:quit 0)))

;;
;; The load gate's request side (gate.lisp), for a one-shot lab on CCL:
;; every request the web server handles holds the gate shared -- except
;; the sheet's event stream, which stays open for hours and would hold
;; every load off; its renders (gwl's %collect-stale!, once a pass) hold
;; it instead.  Installed at run time by install-load-gate!, from a
;; one-shot lab's init, so a shared lab's image never carries either.
;;

(defun stream-request? (req)
  (search "/gdlStream" (princ-to-string (net.aserve:request-uri req))))

(defun install-load-gate! ()
  "Turn the load gate on (*load-gate?*) and put its request side in:
a method around the web server's handle-request (specialized on T, so
it runs inside the server's own around methods and replaces none of
them), and advice around gwl's stream render.  CCL only; elsewhere the
gate holds loads apart from the agent's own tools alone.  Returns what
it installed."
  (setf *load-gate?* t)
  #+ccl
  (progn
    (eval `(defmethod net.aserve::handle-request :around ((req t))
             (if (and *load-gate?* (not (stream-request? req)))
                 (call-with-model-shared #'(lambda () (call-next-method)))
                 (call-next-method))))
    (let ((render (find-symbol "%COLLECT-STALE!" :gwl)))
      (when (and render (fboundp render))
        ;; barging: requests in hand wait on this render (gate.lisp)
        (eval `(ccl:advise ,render (call-with-model-shared #'(lambda () (:do-it)) *model-gate* t)
                           :when :around :name load-gate))))
    (list :handle-request :stream-render))
  #-ccl
  (list :tools-only))

;;
;; Importing one-shot labs' archives.
;;

(defparameter *archive-import-max-bytes* 30000000
  "Integer. The most one imported session directory may hold, in bytes;
a larger one is left where it is.")

(defparameter *archive-import-mark* ".imported-from"
  "String. The file, in an imported session's directory, naming the
directory it was imported from.")

(defun day-name? (name)
  (and (stringp name) (= (length name) 10)
       (every #'(lambda (i) (digit-char-p (char name i))) '(0 1 2 3 5 6 8 9))
       (char= (char name 4) #\-) (char= (char name 7) #\-)))

(defun import-files (directory)
  "The regular files of a session DIRECTORY and of its files/ subdirectory,
as (pathname . relative name)."
  (append (mapcar #'(lambda (f) (cons f (file-namestring f)))
                  (uiop:directory-files directory))
          (let ((sub (merge-pathnames "files/" directory)))
            (and (uiop:directory-exists-p sub)
                 (mapcar #'(lambda (f) (cons f (concatenate 'string "files/" (file-namestring f))))
                         (uiop:directory-files sub))))))

(defun import-session-directory (source root)
  "Copy SOURCE, a session directory under the import ROOT, into this lab's
archive, when it is well formed, not too big, and either new here or
imported from ROOT before.  Values: the target directory or nil, and why."
  (let* ((parts (pathname-directory source))
         (id (car (last parts)))
         (day (car (last parts 2)))
         (target (merge-pathnames (format nil "~a/~a/" day id) (uiop:ensure-directory-pathname *archive-root*)))
         (mark (merge-pathnames *archive-import-mark* target))
         (root-name (namestring (uiop:ensure-directory-pathname root)))
         (files (import-files source))
         (record (cdr (find "session.json" files :key #'cdr :test #'string=))))
    (declare (ignore record))
    (cond ((not (and (session-id? id) (day-name? day))) (values nil "not a session directory"))
          ((> (reduce #'+ files :key #'(lambda (f) (or (ignore-errors (with-open-file (in (car f)) (file-length in))) 0)))
              *archive-import-max-bytes*)
           (values nil "too big"))
          ((not (let ((json (ignore-errors (yason:parse (uiop:read-file-string (merge-pathnames "session.json" source)
                                                                               :external-format :utf-8)))))
                  (and (hash-table-p json) (equal (gethash "id" json) id))))
           (values nil "its record does not parse or names another session"))
          ((and (uiop:directory-exists-p target)
                (not (equal (ignore-errors (string-trim '(#\Newline) (uiop:read-file-string mark))) root-name)))
           (values nil "a session of that id is already here, from elsewhere"))
          ((and (probe-file mark)
                (let ((here (ignore-errors (file-write-date (merge-pathnames "session.json" target))))
                      (there (ignore-errors (file-write-date (merge-pathnames "session.json" source)))))
                  (and here there (>= here there))))
           (values nil "up to date"))
          (t
           (dolist (f files)
             (let ((to (merge-pathnames (cdr f) target)))
               (ensure-directories-exist to)
               (uiop:copy-file (car f) to)))
           (with-open-file (out mark :direction :output :if-exists :supersede)
             (write-line root-name out))
           (values target "imported")))))

(defun import-archives! ()
  "Copy every session of each *archive-imports* directory into this lab's
archive (import-session-directory).  Returns the directories copied."
  (when (and *archive-root* *archive-imports*)
    (let ((copied nil))
      (dolist (root *archive-imports*)
        (dolist (record (ignore-errors
                         (directory (merge-pathnames "*/*/session.json" (uiop:ensure-directory-pathname root)))))
          (let ((target (ignore-errors (import-session-directory (uiop:pathname-directory-pathname record) root))))
            (when target (push target copied)))))
      (nreverse copied))))
