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
;; lab's viewer: the lab is two documents, the page (prompt-lab-sheet: a
;; sluice with the lab's tiles) and the viewer, and both wear the one
;; skin, each a sluice told which skin to wear (its skin input).
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
  ;; the SOURCE file's place, read at compile time: at load time the
  ;; truename is the fasl's, off in a cache directory
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (make-pathname :name nil :type nil :version nil
                   :directory (append (butlast (pathname-directory here)) (list "static"))
                   :defaults here))
  "Pathname. The directory of the lab's stylesheets, editor and icons.")

(defparameter *house-skin* "workstation"
  "String. The name of the look the base stylesheet gives on its own.")

(defparameter *default-skin* nil
  "String or nil. The skin a visitor sees before choosing one; nil is the
house look.")

(defparameter *skin-aliases* '(("default" . "workstation") ("genera" . "workstation"))
  "Association list of strings. Names that lead to another skin: retired
names, so that a saved choice or a bookmark never falls back unexplained.")

(defparameter *reserved-skin-names* '("page" "viewer" "phone")
  "List of strings. Names a skin may not take: prompt-lab-viewer.css and
prompt-lab-phone.css are the viewer's own sheets, and prompt-lab-page.css
was the retired classic page's.")

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

