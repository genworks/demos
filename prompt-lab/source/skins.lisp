;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; Skins.  The lab's look is a set of TOKENS, CSS custom properties, each
;; with its default (the house look), and a SKIN is one stylesheet that
;; redefines tokens.  The tokens, the skins and their contract
;; (SKIN-API.md) are the SLUICE's, Gendl's object browser, which is the
;; lab's viewer: the lab is two documents, the page and the viewer in
;; its frame, and both wear the one skin.
;;
;;   the page     the sluice's tokens, prompt-lab-page.css (the page laid
;;                out in them), the skin
;;   the viewer   a sluice told which skin to wear (its skin input)
;;
;; The lab finds the sluice's skins by asking it, and adds any of its own
;; (static/prompt-lab-<name>.css: a skin for the lab alone, which the
;; viewer is handed as an additional stylesheet).
;;
;; A SLUICE OLDER THAN ITS SKINS has neither tokens nor skins nor a skin
;; input.  The lab then falls back on copies it carries of the tokens
;; (static/prompt-lab.css), of the sheet that says the sluice in them
;; (static/prompt-lab-viewer.css) and of the two skins, and hands all of
;; them to the viewer as additional stylesheets.  The copies go when
;; every image that serves the lab has the newer sluice; until then the
;; smoke run holds the tokens' copy to the sluice's, where there is one.
;;

(defparameter *static-directory*
  ;; from the SOURCE file's place, like *page-file* (page.lisp)
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (make-pathname :name nil :type nil :version nil
                   :directory (append (butlast (pathname-directory here)) (list "static"))
                   :defaults here))
  "Pathname. The directory of the page, its stylesheets and its script.")

(defparameter *house-skin* "workstation"
  "String. The name of the look the base stylesheet gives on its own.")

(defparameter *default-skin* nil
  "String or nil. The skin a visitor sees before choosing one; nil is the
house look.")

(defparameter *skin-aliases* '(("default" . "workstation") ("genera" . "workstation"))
  "Association list of strings. Names that lead to another skin: retired
names, so that a saved choice or a bookmark never falls back unexplained.")

(defparameter *reserved-skin-names* '("page" "viewer" "phone")
  "List of strings. Names a skin may not take: prompt-lab-page.css,
prompt-lab-viewer.css and prompt-lab-phone.css are the documents' own
sheets.")

(defun static-file (name)
  "Pathname or nil. The file NAME in the static directory, when it is there."
  (probe-file (merge-pathnames name *static-directory*)))

(defun static-url (name)
  "String. The address of the static file NAME, carrying the file's write
date: a stylesheet is kept for hours by every cache between the server
and the visitor, so a changed file needs an address of its own."
  (let* ((file (static-file name))
         (date (and file (ignore-errors (file-write-date file)))))
    (format nil "~a/static/~a~@[?v=~d~]" *url-prefix* name date)))

;;
;; The sluice's side, asked for by name at run time: an image whose
;; sluice is older has no such symbols, and a file that named them
;; would not be read there.
;;

(defun sluice-function (name)
  "Symbol or nil. The sluice's function called NAME (a string designator),
when this image's sluice has it."
  (let ((symbol (find-symbol (string name) :sluice)))
    (and symbol (fboundp symbol) symbol)))

(defun sluice-skins? ()
  "Boolean. True when this image's sluice wears skins itself: it has the
functions, and its static directory the tokens."
  (and (sluice-function '#:skin-links)
       (sluice-function '#:skins)
       sluice:*static*
       (probe-file (format nil "~a/tokens.css" sluice:*static*))
       t))

(defun sluice-static-url (name)
  "String. The address, for the PAGE, of the file NAME of the sluice's static
directory, with its date.  It is under the lab's own prefix
(<prefix>/sluice-static/, published with the page): behind a proxy that
sends each prefix to its own server, whatever answers for the lab answers
for its tokens, from the same image.  The viewer's own links are the
sluice's business."
  (let* ((file (probe-file (format nil "~a/~a" sluice:*static* name)))
         (date (and file (ignore-errors (file-write-date file)))))
    (format nil "~a/sluice-static/~a~@[?v=~d~]" *url-prefix* name date)))

(defun tokens-url ()
  "String. The address of the tokens: the sluice's, or the lab's copy of
them where the sluice is older than its skins."
  (if (sluice-skins?)
      (sluice-static-url "tokens.css")
      (static-url "prompt-lab.css")))

(defun split-url ()
  "String or nil. The address of the script that makes the dividers
between panes draggable, which is the sluice's; nil where it has none."
  (and (sluice-skins?)
       (probe-file (format nil "~a/split.js" sluice:*static*))
       (sluice-static-url "split.js")))

(defun skin-name? (string)
  "True when STRING can name a skin: lower-case letters, digits, hyphen,
underscore and full stop (prompt-lab-<vendor>.<name>.css), nothing else."
  (and (stringp string)
       (plusp (length string))
       (every #'(lambda (char)
                  (or (char<= #\a char #\z) (digit-char-p char) (find char "-_.")))
              string)
       (not (member string *reserved-skin-names* :test #'string=))))

(defun skin-file-name (name)
  (format nil "prompt-lab-~a.css" name))

(defun token-value (file token)
  "String or nil. What the stylesheet FILE declares TOKEN to be, as written
there (the first declaration; a quoted string without its quotes), or nil
when it does not declare it or cannot be read."
  (ignore-errors
   (let* ((text (uiop:read-file-string file :external-format :utf-8))
          (start (search (concatenate 'string token ":") text))
          (from (and start (+ start (length token) 1)))
          (end (and from (position #\; text :start from))))
     (when end
       (let ((value (string-trim '(#\space #\tab #\newline #\return) (subseq text from end))))
         (if (and (> (length value) 1)
                  (char= (char value 0) #\")
                  (char= (char value (1- (length value))) #\"))
             (subseq value 1 (1- (length value)))
             value))))))

(defun skin-label (file name)
  "String. What the skin calls itself -- the string of its --pl-skin-label
token -- or NAME with a capital when it declares none."
  (let ((label (token-value file "--pl-skin-label")))
    (if (and label (plusp (length label)))
        label
        (string-capitalize (substitute #\space #\- name)))))

(defun skins ()
  "List of plists (:name :label :href :file :sluice?), the skins there are,
by name: the sluice's, which it wears by name (:sluice? true), and the
lab's own.  Where both have a skin of one name it is the sluice's.  The
house look is not among them: it is the tokens' own defaults."
  (let ((theirs (and (sluice-skins?)
                     (mapcar #'(lambda (skin)
                                 (let ((name (getf skin :name)))
                                   (list :name name
                                         :label (getf skin :label)
                                         :href (sluice-static-url (format nil "skin-~a.css" name))
                                         :file (probe-file (format nil "~a/skin-~a.css"
                                                                   sluice:*static* name))
                                         :sluice? t)))
                             (funcall (sluice-function '#:skins))))))
    (sort (append theirs
                  (remove-if #'(lambda (own)
                                 (find (getf own :name) theirs
                                       :key #'(lambda (skin) (getf skin :name)) :test #'string=))
                             (own-skins)))
          #'string< :key #'(lambda (skin) (getf skin :name)))))

(defun own-skins ()
  "List of plists (:name :label :href :file), the skins in the lab's own
static directory, by name."
  (let ((found nil))
    (dolist (file (directory (merge-pathnames "prompt-lab-*.css" *static-directory*)))
      ;; the pathname's name, past "prompt-lab-": a namestring would
      ;; carry an escape before a vendor's full stop on some Lisps
      (let* ((base (pathname-name file))
             (name (and (> (length base) (length "prompt-lab-"))
                        (subseq base (length "prompt-lab-")))))
        (when (skin-name? name)
          (push (list :name name
                      :label (skin-label file name)
                      :href (static-url (skin-file-name name))
                      :file file)
                found))))
    (sort found #'string< :key #'(lambda (skin) (getf skin :name)))))

(defun find-skin (name)
  "Plist or nil. The skin NAME, through the aliases; nil for the house look,
for a name that is no skin's, and for anything that is not a name."
  (when (stringp name)
    (let ((name (or (cdr (assoc name *skin-aliases* :test #'string=)) name)))
      (and (skin-name? name)
           (not (string= name *house-skin*))
           (find name (skins) :key #'(lambda (skin) (getf skin :name)) :test #'string=)))))

(defun viewer-skin (skin)
  "String or nil. What the viewer's sluice is told to wear for SKIN (a plist
from find-skin, or nil for the house look): a skin of the sluice's by its
name, the house look for anything else -- a skin of the lab's own comes
after it, as a stylesheet.  Nil where the sluice is older than its skins,
and is told nothing."
  (when (sluice-skins?)
    (if (and skin (getf skin :sluice?))
        (getf skin :name)
        *house-skin*)))

(defun viewer-css-links (skin &key phone?)
  "List of strings. The stylesheets the viewer adds to the sluice's own for
SKIN (a plist from find-skin, or nil for the house look).  A sluice that
wears skins links the tokens and the skin itself (viewer-skin), and is
handed only the phone's sheet when PHONE? and a skin of the lab's own.  An
older one is handed everything: the lab's copy of the tokens, the sheet
that lays them onto the sluice, the phone's, the skin."
  (if (sluice-skins?)
      (append (and phone? (list (static-url "prompt-lab-phone.css")))
              (and skin (not (getf skin :sluice?)) (list (getf skin :href))))
      (append (list (static-url "prompt-lab.css")
                    (static-url "prompt-lab-viewer.css"))
              (and phone? (list (static-url "prompt-lab-phone.css")))
              (and skin (list (getf skin :href))))))


;;
;; The page.  static/page.html is the document, with places in double
;; braces for this side to fill in: the addresses of its two sheets, its
;; script, its manifest and icons (app.lisp), and what the script needs
;; to know before it asks any door.
;;

(defun ascii-json (json)
  "String. JSON, a string of JSON, fit to stand inside a script element
and to be written in any encoding: nothing in it closes the element, and a
character beyond ASCII (a skin's label) goes as its escape."
  (with-output-to-string (out)
    (loop for char across json
          for code = (char-code char)
          do (cond ((char= char #\<) (write-string "\\u003c" out))
                   ((< code 128) (write-char char out))
                   ((< code #x10000) (format out "\\u~4,'0x" code))
                   (t (let ((rest (- code #x10000)))
                        (format out "\\u~4,'0x\\u~4,'0x"
                                (+ #xD800 (ash rest -10))
                                (+ #xDC00 (logand rest #x3FF)))))))))

(defun page-boot ()
  "String. JSON for the page's script: where the static files are, and the
skins there are."
  (ascii-json
   (encode (h "prefix" *url-prefix*
              ;; the viewer's phone sheet, for a frame that was opened
              ;; on a desk and finds itself on a phone
              "phone_css" (static-url "prompt-lab-phone.css")
              ;; the service worker, when the lab keeps one (app.lisp)
              "worker" (and (app?) (format nil "~a/worker" *url-prefix*))
              "house" *house-skin*
              "default_skin" (let ((skin (find-skin *default-skin*)))
                               (if skin (getf skin :name) *house-skin*))
              "aliases" (let ((table (make-hash-table :test #'equal)))
                          (loop for (from . to) in *skin-aliases*
                                do (setf (gethash from table) to))
                          table)
              "skins" (map 'vector #'(lambda (skin)
                                       (h "name" (getf skin :name)
                                          "label" (getf skin :label)
                                          "href" (getf skin :href)))
                           (skins))))))

(defun ascii-only (text)
  "String. TEXT with every character beyond ASCII as an HTML character
reference, so that the page reads the same whatever encoding a server
writes it in.  The page's script is a file of its own for that reason: a
reference means nothing inside a script."
  (if (every #'(lambda (char) (< (char-code char) 128)) text)
      text
      (with-output-to-string (out)
        (loop for char across text
              do (if (< (char-code char) 128)
                     (write-char char out)
                     (format out "&#~d;" (char-code char)))))))

(defun static-signature ()
  "List. The static files with their write dates: the page is filled in
again when any of them changes, and the worker keeps a new cache."
  (mapcar #'(lambda (file) (list (pathname-name file) (pathname-type file)
                                 (ignore-errors (file-write-date file))))
          (append (directory (merge-pathnames "*.css" *static-directory*))
                  (directory (merge-pathnames "*.js" *static-directory*))
                  (directory (merge-pathnames "*.html" *static-directory*))
                  (directory (merge-pathnames "icons/*.png" *static-directory*)))))

(defvar *page-cache* nil
  "Cons of the static signature and the page filled in under it, or nil.")

(defun fill-page (text)
  (ascii-only
   (reduce #'(lambda (text place)
               (replace-substring text (car place) (cdr place)))
           (list (cons "{{tokens-css}}" (tokens-url))
                 ;; the dividers' script, where the sluice has one; the
                 ;; panes keep their places without it
                 (cons "{{split-script}}"
                       (let ((url (split-url)))
                         (if url (format nil "<script src=\"~a\" defer></script>" url) "")))
                 ;; the model file's editor (built from ../editor); the
                 ;; page's plain textarea serves without it
                 (cons "{{editor-script}}"
                       (if (static-file "editor.js")
                           (format nil "<script src=\"~a\" defer></script>" (static-url "editor.js"))
                           ""))
                 (cons "{{page-css}}" (static-url "prompt-lab-page.css"))
                 (cons "{{script}}" (static-url "prompt-lab.js"))
                 (cons "{{manifest}}" (format nil "~a/manifest.webmanifest" *url-prefix*))
                 (cons "{{icon}}" (static-url "icons/icon-192.png"))
                 (cons "{{touch-icon}}" (static-url "icons/apple-touch-icon.png"))
                 (cons "{{app-name}}" (app-short-name))
                 ;; which lab: the free one or the solids one (parameters.lisp)
                 (cons "{{title}}" (lab-title))
                 (cons "{{boot}}" (page-boot)))
           :initial-value text)))

(defun page-text ()
  "String. The page, filled in; read again when a static file has changed."
  (let ((signature (list *url-prefix* *default-skin* (app?) (app-short-name) (lab-title) (static-signature)
                         ;; the sluice's side: its tokens, its skins, its script
                         (tokens-url) (split-url) (mapcar #'(lambda (skin) (getf skin :href)) (skins))))
        (cache *page-cache*))
    (if (and cache (equal (car cache) signature))
        (cdr cache)
        (let ((text (fill-page (uiop:read-file-string (merge-pathnames "page.html" *static-directory*)
                                                      :external-format :utf-8))))
          (setq *page-cache* (cons signature text))
          text))))
