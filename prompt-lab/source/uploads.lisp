;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; Uploads: files a visitor hands the agent to build from -- a 2D
;; drawing as a PDF or an image, a DXF, a table of dimensions, a STEP
;; file.  A file lives in the session's directory under files/, goes to
;; the archive with the session, and is as public as the session is:
;; anyone who may see the session may fetch its files, so the visitor
;; declares the right to use and share each one, and only a private
;; session (one that has bought credits) keeps them to its owner.
;;
;; The agent meets a file two ways.  A PDF or an image rides with the
;; visitor's next prompt as a document or image block of the Messages
;; API, which reads a PDF page as its text and its picture.  The
;; conversation keeps only a REFERENCE to the file (a block whose source
;; says lab_file and the name); request-body puts the bytes in when a
;; call is made, so the record on disk stays small and the same bytes
;; go out every time, which the prompt cache needs.  Text files are
;; named to the agent, which reads them with read_file; list_files
;; gives every file's path, for a reader that takes one (a STEP import
;; on a solids engine).
;;

(defparameter *uploads?* t
  "Boolean. Whether visitors may upload files for the agent to build from.")

(defparameter *upload-caps*
  '(:free (:bytes 2000000 :files 3 :total 6000000 :pages 10)
    :paid (:bytes 5000000 :files 10 :total 8000000 :pages 40))
  "Plist of plists. What one session may upload: the most bytes a file may
have, the most files, the most bytes in all and the most pages a PDF may
have -- for a session on the free caps, and for one that has bought
credits.  Every attached PDF and image travels with every call of the
session, base64, so :total times 4/3 must stay under what the gate (or
the API: 32 MB) takes in one request.")

(defparameter *upload-text-types*
  '("dxf" "svg" "csv" "txt" "md" "json" "step" "stp" "iges" "igs")
  "List of strings. File types taken as text, by the name's ending; the
agent reads them with read_file.")

(defparameter *read-file-limit* 20000
  "Integer. The most characters of a text file one read_file call answers.")

(defun uploads-offered? ()
  "Whether this lab takes uploads: allowed, and base64 aboard."
  (and *uploads?* (find-package :cl-base64) t))

(defun upload-caps (session)
  (getf *upload-caps* (if (paid? session) :paid :free)))


;;
;; A file's name and kind.
;;

(defun safe-file-name (name)
  "NAME as a file name the lab will keep: its last component, letters,
digits, dot, hyphen and underscore only (anything else a hyphen), 80
characters at most, not opening with a dot.  Nil when nothing is left."
  (when (stringp name)
    (let* ((cut (position-if #'(lambda (c) (member c '(#\/ #\\))) name :from-end t))
           (base (if cut (subseq name (1+ cut)) name))
           (clean (map 'string #'(lambda (c)
                                   (if (and (< (char-code c) 128)
                                            (or (alphanumericp c) (member c '(#\. #\- #\_))))
                                       c
                                       #\-))
                       base))
           (clean (string-left-trim ".-" clean))
           (clean (if (> (length clean) 80) (subseq clean (- (length clean) 80)) clean))
           (clean (string-left-trim ".-" clean)))
      (and (plusp (length clean)) (some #'alphanumericp clean) clean))))

(defun file-name-type (name)
  (let ((dot (position #\. name :from-end t)))
    (and dot (string-downcase (subseq name (1+ dot))))))

(defun octets-start? (octets &rest codes)
  (and (>= (length octets) (length codes))
       (loop for code in codes for i from 0 always (= code (aref octets i)))))

(defun text-octets? (octets)
  "True when OCTETS read as UTF-8 text with no NUL in it."
  (and (not (find 0 octets))
       (ignore-errors (babel:octets-to-string octets :encoding :utf-8 :errorp t) t)))

(defun sniff-kind (name octets &key (text-check? t))
  "What a file is, by its first bytes, or by its name for a text file.
Values: :pdf, :image or :text and the media type; nil when the lab does
not take it.  OCTETS may be only the file's head when TEXT-CHECK? is nil."
  (cond ((octets-start? octets 37 80 68 70 45) (values :pdf "application/pdf")) ; %PDF-
        ((octets-start? octets 137 80 78 71) (values :image "image/png"))
        ((octets-start? octets 255 216 255) (values :image "image/jpeg"))
        ((octets-start? octets 71 73 70 56) (values :image "image/gif"))
        ((and (octets-start? octets 82 73 70 70) (>= (length octets) 12) ; RIFF....WEBP
              (equalp (subseq octets 8 12) #(87 69 66 80)))
         (values :image "image/webp"))
        ((and (member (file-name-type name) *upload-text-types* :test #'equal)
              (or (not text-check?) (text-octets? octets)))
         (values :text "text/plain"))))

(defun pdf-pages (octets)
  "The pages of a PDF, counted by its /Type /Page objects.  A file that
keeps those in compressed object streams shows none (0): its count is
left to the API, which refuses a PDF beyond its own limit."
  (let ((needle (map 'vector #'char-code "/Type"))
        (page (map 'vector #'char-code "/Page"))
        (n (length octets))
        (count 0))
    (loop with start = 0
          for at = (search needle octets :start2 start)
          while at
          do (let ((i (+ at 5)))
               (loop while (and (< i n) (member (aref octets i) '(32 10 13 9))) do (incf i))
               (when (and (<= (+ i 5) n)
                          (equalp (subseq octets i (+ i 5)) page)
                          (or (= (+ i 5) n)
                              (not (alpha-char-p (code-char (aref octets (+ i 5)))))))
                 (incf count))
               (setq start (1+ at))))
    count))


;;
;; An uploaded file, as an object: what it is and every form it takes --
;; for the page, for the agent's listing, for the kept conversation and
;; for the API.  Made afresh from the file on disk whenever one is asked
;; for (a shelf, below), so nothing here outlives the file.
;;

(define-object uploaded-file ()

  :documentation
  (:description "One file a visitor uploaded to a session, read from where it
is kept on disk."
   :author "Genworks International")

  :input-slots
  ("Pathname or string. Where the file is kept."
   file-path)

  :computed-slots
  (("String. The file's name."
    file-name (file-namestring (the file-path)))

   (head-and-bytes (with-open-file (in (the file-path) :element-type '(unsigned-byte 8))
                     (let* ((head (make-array 16 :element-type '(unsigned-byte 8)))
                            (count (read-sequence head in)))
                       (list (subseq head 0 count) (file-length in)))))

   ("Integer. The file's size."
    bytes (second (the head-and-bytes)))

   (date (or (file-write-date (the file-path)) 0))

   (kind-and-type (multiple-value-list
                   (sniff-kind (the file-name) (first (the head-and-bytes)) :text-check? nil)))

   ("Keyword. :pdf, :image or :text; nil for a file the lab does not take."
    kind (first (the kind-and-type)))

   (media-type (second (the kind-and-type)))

   ("String. What the page and the agent are told the file is."
    kind-label (ecase (the kind)
                 (:pdf "PDF")
                 (:image (format nil "~:@(~a~) image" (subseq (the media-type) 6)))
                 (:text (format nil "~a text" (or (file-name-type (the file-name)) "plain")))))

   (size-label (size-label (the bytes)))

   (octets (alexandria:read-file-into-byte-vector (the file-path)))

   (base64 (uiop:symbol-call :cl-base64 :usb8-array-to-base64-string (the octets)))

   ("String. The file as text, for a text file."
    text (uiop:read-file-string (the file-path) :external-format :utf-8))

   ("Hash table. The block that stands for the file in the kept conversation."
    reference (h "type" (ecase (the kind) (:pdf "document") (:image "image") (:text "text"))
                 "source" (h "type" "lab_file" "name" (the file-name))))

   ("List of hash tables. The Messages API blocks the reference stands for:
a PDF as a document, an image as an image under its name, a text file as
a line naming it and its path."
    api-blocks (ecase (the kind)
                 (:pdf (list (h "type" "document" "title" (the file-name)
                                "source" (h "type" "base64" "media_type" "application/pdf"
                                            "data" (the base64)))))
                 (:image (list (h "type" "text" "text" (format nil "Uploaded image ~a:" (the file-name)))
                               (h "type" "image"
                                  "source" (h "type" "base64" "media_type" (the media-type)
                                              "data" (the base64)))))
                 (:text (list (h "type" "text"
                                 "text" (format nil "Uploaded file ~a (~a, ~a) at ~a: read it with read_file."
                                                (the file-name) (the kind-label) (the size-label)
                                                (namestring (the file-path))))))))

   ("String. The file's line in the agent's list_files."
    listing (format nil "~a -- ~a, ~a, at ~a"
                    (the file-name) (the kind-label) (the size-label) (namestring (the file-path)))))

  :functions
  (("Content blocks for the agent's read_file: a text file's characters
from OFFSET, at most LIMIT and *read-file-limit* of them; an image; for a
PDF, where to find it."
    read-blocks
    (&key offset limit)
    (ecase (the kind)
      (:pdf (list (text-result "~a is a PDF: it is in the conversation, attached to the visitor's message, pages and text.  Read it there."
                               (the file-name))))
      (:image (list `(("type" . "image")
                      ("source" . (("type" . "base64") ("media_type" . ,(the media-type))
                                   ("data" . ,(the base64)))))))
      (:text
       (let* ((text (the text))
              (total (length text))
              (start (min total (if (and (integerp offset) (plusp offset)) offset 0)))
              (count (min *read-file-limit*
                          (if (and (integerp limit) (plusp limit)) limit *read-file-limit*)))
              (end (min total (+ start count))))
         ;; not through text-result: its clip is for printed values
         (list `(("type" . "text")
                 ("text" . ,(format nil "~a~:[~;~%[characters ~d to ~d of ~d; give offset ~d for more]~]"
                                    (subseq text start end)
                                    (or (plusp start) (< end total))
                                    start end total end))))))))

   ("Hash table. The file as the page lists it, fetched at URL."
    state
    (url)
    (h "name" (the file-name) "bytes" (the bytes) "kind" (the kind-label) "url" url))))


;;
;; A shelf: the files kept under one directory, a session's or its
;; archive's.
;;

(define-object file-shelf ()

  :documentation
  (:description "The uploaded files under one directory (a session's, or its
archive's), the oldest first."
   :author "Genworks International")

  :input-slots
  ("Pathname or string. The session's directory, or its archive's."
   folder)

  :computed-slots
  ((files-folder (merge-pathnames "files/" (the folder)))

   (paths (ignore-errors (directory (merge-pathnames "*.*" (the files-folder)))))

   ("List of uploaded-file objects. Those the lab takes, the oldest first."
    files (sort (remove-if-not #'(lambda (file) (ignore-errors (the-object file kind)))
                               (list-elements (the entries)))
                #'(lambda (a b)
                    (if (= (the-object a date) (the-object b date))
                        (string< (the-object a file-name) (the-object b file-name))
                        (< (the-object a date) (the-object b date))))))

   (total-bytes (reduce #'+ (the files) :key #'(lambda (file) (the-object file bytes)))))

  :objects
  ((entries :type 'uploaded-file
            :sequence (:size (length (the paths)))
            :file-path (nth (the-child index) (the paths))))

  :functions
  (("The file named FILE-NAME, or nil."
    file-named
    (file-name)
    (find file-name (the files) :key #'(lambda (file) (the-object file file-name)) :test #'equal))))

(defun directory-files (directory)
  "The uploaded files kept under DIRECTORY, a session's or its archive's."
  (the-object (make-object 'file-shelf :folder directory) files))

(defun session-shelf (session)
  (make-object 'file-shelf :folder (session-directory session)))

(defun session-files (session)
  (the-object (session-shelf session) files))

(defun find-file (session name)
  (the-object (session-shelf session) (file-named name)))


;;
;; A candidate: a file as it arrives, before it is kept.  Whether the
;; lab takes it is one slot, the reason it does not.
;;

(define-object upload-candidate ()

  :documentation
  (:description "A file a visitor is uploading to a session: its name as
the lab will keep it, what it is, and the reason the session may not have
it, if any."
   :author "Genworks International")

  :input-slots
  ("The session (session.lisp) the file is for."
   session
   "String. The name the browser gave."
   given-name
   "Vector of octets. The file."
   octets)

  :computed-slots
  ((file-name (safe-file-name (the given-name)))
   (bytes (length (the octets)))
   (paid? (paid? (the session)))
   (caps (upload-caps (the session)))
   (shelf (session-shelf (the session)))
   (kind (and (the file-name) (sniff-kind (the file-name) (the octets))))
   (pages (if (eq (the kind) :pdf) (pdf-pages (the octets)) 0))
   (file-path (merge-pathnames (the file-name) (the shelf files-folder)))

   ("String or nil. Why the session may not have this file."
    refusal
    (let ((caps (the caps)) (name (the file-name)))
      (cond ((not (uploads-offered?)) "This lab takes no uploads.")
            ((session-replay? (the session)) "An archived session takes no uploads.")
            ((null name) "That file has no usable name.")
            ((zerop (the bytes)) "That file is empty.")
            ((> (the bytes) (getf caps :bytes))
             (format nil (unit-text "~a has ~a; a file may have ~a here~:[~;, more for a session that has added {units}~].")
                     name (size-label (the bytes)) (size-label (getf caps :bytes)) (not (the paid?))))
            ((the shelf (file-named name))
             (format nil "This session already has a file named ~a." name))
            ((>= (length (the shelf files)) (getf caps :files))
             (format nil (unit-text "This session has its ~a files~:[~;; a session that has added {units} may have more~].")
                     (getf caps :files) (not (the paid?))))
            ((> (+ (the bytes) (the shelf total-bytes)) (getf caps :total))
             (format nil "With ~a this session's files would pass ~a in all." name (size-label (getf caps :total))))
            ((null (the kind))
             (format nil "~a is not a kind of file the lab reads: a PDF, an image (PNG, JPEG, GIF, WebP) or a text file (~{~a~^, ~})."
                     name *upload-text-types*))
            ((> (the pages) (getf caps :pages))
             (format nil "~a has ~a pages; a PDF may have ~a here." name (the pages) (getf caps :pages)))))))

  :functions
  (("Write the file where the session keeps its files; returns it as an
uploaded-file."
    keep!
    ()
    (let* ((path (the file-path))
           (tmp (make-pathname :type "tmp" :defaults path)))
      (ensure-directories-exist path)
      (with-open-file (out tmp :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
        (write-sequence (the octets) out))
      (rename-file tmp path)
      (make-object 'uploaded-file :file-path path)))))

(defun size-label (bytes)
  (cond ((< bytes 1000) (format nil "~d bytes" bytes))
        ((< bytes 1000000) (format nil "~d KB" (round bytes 1000)))
        (t (format nil "~,1f MB" (/ bytes 1000000.0)))))

(defun add-upload! (session name octets)
  "Keep OCTETS as the file NAME of SESSION, within the session's caps.
Values: the file (an uploaded-file), or nil and the reason."
  (with-session-lock (session)
    (let* ((candidate (make-object 'upload-candidate :session session :given-name name :octets octets))
           (refusal (the-object candidate refusal)))
      (if refusal
          (values nil refusal)
          (the-object candidate keep!)))))

(defun accept-upload! (session name data &key rights? token address checked?)
  "A visitor's upload to SESSION: NAME and DATA, the file in base64.
RIGHTS? is the visitor's declaration that they may use and share the
file; TOKEN the human check's, where one stands (CHECKED? true when the
caller has had it verified already: several files, one check).  Values:
the file (an uploaded-file), or nil and the reason.  The owner's check
is the caller's."
  (cond ((not (uploads-offered?)) (values nil "This lab takes no uploads."))
        ((not rights?)
         (values nil "Acknowledge first that this file will be shared: it is public with the session."))
        ((not (and (stringp data) (plusp (length data))))
         (values nil "No file was sent."))
        ;; before the bytes are decoded: four characters carry three bytes
        ((> (length data) (* 3/2 (getf (upload-caps session) :bytes)))
         (values nil (format nil "That file is too large; a file may have ~a here."
                             (size-label (getf (upload-caps session) :bytes)))))
        (t
         (multiple-value-bind (ok? reason) (if checked? t (verify-turnstile token address))
           (if (not ok?)
               (values nil reason)
               (let ((octets (ignore-errors
                              (uiop:symbol-call :cl-base64 :base64-string-to-usb8-array data))))
                 (if (null octets)
                     (values nil "The file did not arrive whole (it is not base64).")
                     (multiple-value-bind (file reason) (add-upload! session name octets)
                       (when file
                         (touch session)
                         (log-event session :note "Uploaded ~a (~a, ~a).  The visitor declared the right to use and share it; it is ~:[public with the session~;private with the session~].  It goes to the agent with the next prompt."
                                    (the-object file file-name) (the-object file kind-label)
                                    (the-object file size-label) (session-private? session))
                         (save-session! session)
                         ;; a drawing is asked which lab it belongs in (routing.lisp)
                         (maybe-classify! session file))
                       (values file reason)))))))))


;;
;; The files in the conversation: references, and the bytes at call time.
;;

(defun file-reference-name (block)
  "The name of the file BLOCK refers to, when it is a file reference."
  (and (hash-table-p block)
       (let ((source (gethash "source" block)))
         (and (hash-table-p source)
              (equal (gethash "type" source) "lab_file")
              (gethash "name" source)))))

(defun attached-names (session)
  (loop for message in (session-messages session)
        for content = (and (hash-table-p message) (gethash "content" message))
        when (listp content)
          append (loop for block in content
                       for name = (file-reference-name block)
                       when name collect name)))

(defun pending-attachments (session)
  "References to SESSION's files that no turn of the conversation carries
yet: what the visitor's next prompt brings along."
  (unless (session-replay? session)
    (let ((attached (attached-names session)))
      (loop for file in (session-files session)
            unless (member (the-object file file-name) attached :test #'string=)
              collect (the-object file reference)))))

(defun expand-messages (session messages)
  "MESSAGES as the API takes them: every file reference replaced by the
file itself (an uploaded-file's api-blocks).  The kept messages are not
touched."
  (let ((shelf nil))
    (flet ((blocks (name)
             (let ((file (the-object (or shelf (setq shelf (session-shelf session))) (file-named name))))
               (if file
                   (the-object file api-blocks)
                   (list (h "type" "text"
                            "text" (format nil "[The uploaded file ~a is no longer on the host.]" name)))))))
      (mapcar #'(lambda (message)
                  (let ((content (and (hash-table-p message) (gethash "content" message))))
                    (if (and (listp content) (some #'file-reference-name content))
                        (h "role" (gethash "role" message)
                           "content" (loop for block in content
                                           for name = (file-reference-name block)
                                           if name append (blocks name)
                                           else collect block))
                        message)))
              messages))))


;;
;; The agent's tools.
;;

(defun list-files-tool (session)
  (let ((files (session-files session)))
    (values
     (list (if files
               (text-result "~{~a~%~}" (mapcar #'(lambda (file) (the-object file listing)) files))
               (text-result "The visitor has uploaded no files.")))
     nil)))

(defun read-file-tool (session name &key offset limit)
  (let ((file (and (stringp name) (find-file session (safe-file-name name)))))
    (if file
        (values (the-object file (read-blocks :offset offset :limit limit)) nil)
        (values (list (text-result "There is no uploaded file named ~a; list_files names them." name)) t))))


;;
;; The archive keeps a session's files with it.
;;

(defun archive-files! (session)
  "Copy SESSION's files into its archive directory, those not there yet.
Never signals."
  (ignore-errors
   (let ((directory (archive-directory session)))
     (when directory
       (let ((target-folder (the-object (make-object 'file-shelf :folder directory) files-folder)))
         (dolist (file (session-files session))
           (let ((target (merge-pathnames (the-object file file-name) target-folder)))
             (unless (probe-file target)
               (ensure-directories-exist target)
               (uiop:copy-file (the-object file file-path) target)))))))))


;;
;; What the page is told, and the two doors.
;;

(defun file-url (id name &key archive?)
  (format nil "~a?~:[session~;archive~]=~a&name=~a" (door-path "file") archive? id name))

(defun files-state (files id &key archive?)
  "FILES (uploaded-file objects) as the page lists them: name, size, kind
and where to fetch each."
  (map 'vector #'(lambda (file)
                   (the-object file (state (file-url id (the-object file file-name) :archive? archive?))))
       files))

(defun uploads-state (&optional session)
  "The caps the page shows, or nil where the lab takes no uploads."
  (when (uploads-offered?)
    (let ((caps (if session (upload-caps session) (getf *upload-caps* :free))))
      (h "max_bytes" (getf caps :bytes)
         "max_files" (getf caps :files)
         "max_pages" (getf caps :pages)
         "text_types" (coerce *upload-text-types* 'vector)))))

(defun upload-door (req ent)
  "POST <prefix>/api/upload {session, name, data, rights, turnstile}: the
owner hands the session a file, DATA its bytes in base64, RIGHTS true for
the declaration that they may use and share it.  Answers the session's
files."
  (let* ((json (request-json req))
         (session (requested-session req json))
         (address (client-address req)))
    (cond ((null json) (refuse req ent "Send the file as JSON: session, name, data (base64), rights."))
          ((null session) (no-such-session req ent))
          ((not (owner? session (request-owner-key req json) address)) (not-yours req ent))
          (t (multiple-value-bind (file reason)
                 (accept-upload! session (gethash "name" json) (gethash "data" json)
                                 ;; the acknowledgement that the file is shared is
                                 ;; not asked of a session that is not public:
                                 ;; closed-source, or one that has topped up
                                 :rights? (or (eq (gethash "rights" json) t)
                                              (session-closed? session) (paid? session))
                                 :token (gethash "turnstile" json) :address address)
               (if file
                   (respond-json req ent (h "name" (the-object file file-name)
                                            "bytes" (the-object file bytes)
                                            "kind" (the-object file kind-label)
                                            "files" (files-state (session-files session) (session-id session))))
                   (refuse req ent "~a" reason)))))))

(defun file-door (req ent)
  "GET <prefix>/api/file?session=<id>&name=<name> (or archive=<id>): an
uploaded file, to whoever may see the session -- always as a download,
never as a page of this site."
  (let* ((name (safe-file-name (query-value req "name")))
         (key (request-owner-key req))
         (archive (query-value req "archive"))
         (session (and (null archive) (requested-session req)))
         (folder (cond (session
                        (and (or (owner-request? req session)
                                 (and *browsing?* (visible-to? session key)))
                             (session-directory session)))
                       ((and archive *browsing?*)
                        (let* ((directory (archived-directory archive))
                               (json (and directory
                                          (read-record (merge-pathnames "session.json" directory)))))
                          (and json (record-visible? json key) directory)))))
         (file (and folder name
                    (the-object (make-object 'file-shelf :folder folder) (file-named name)))))
    (if (null file)
        (refuse req ent net.aserve:*response-not-found* "No such file.")
        (net.aserve:with-http-response (req ent :content-type (if (eq (the-object file kind) :text)
                                                                  "text/plain; charset=utf-8"
                                                                  (the-object file media-type))
                                                :format :binary)
          (setf (net.aserve:reply-header-slot-value req :content-disposition)
                (format nil "attachment; filename=\"~a\"" (the-object file file-name)))
          (setf (net.aserve:reply-header-slot-value req :x-content-type-options) "nosniff")
          (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
          (net.aserve:with-http-body (req ent)
            (write-sequence (the-object file octets) (net.aserve:request-reply-stream req)))))))
