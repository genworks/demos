;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The lab as an installable app: its manifest, at
;; <prefix>/manifest.webmanifest, says what the app is called, its icons
;; and where it starts.  The page is the sheet (prompt-lab-sheet), which
;; lives at the address gwl mints for each visit (/sessions/<id>/), so the
;; manifest's scope is the whole site, and it keeps no service worker: a
;; minted session page cannot be kept for offline use, and a browser
;; installs an app without one.
;;
;; <prefix>/worker answers only for browsers that still hold the worker
;; the lab's first page kept: a worker that drops the lab's caches and
;; unregisters itself.  A worker once installed stays until it is
;; replaced, so that door stays.
;;

(defparameter *app-name* "Genworks Prompt Lab"
  "String. The app's name, under its icon where there is room.")

(defparameter *app-short-name* "Prompt Lab"
  "String. The app's name where there is little room.")

(defparameter *app-description*
  "Describe a part; a modeling agent builds it as a parametric Gendl model you can inspect, adjust and edit."
  "String. What the app is, for an installer that says.")

(defparameter *app-icons*
  '(("icons/icon-192.png" "192x192" "any")
    ("icons/icon-512.png" "512x512" "any")
    ("icons/icon-maskable-512.png" "512x512" "maskable"))
  "List of (file sizes purpose). The app's icons, files of the static directory.")

(defun app-name ()
  "String. The app's name; a lab on another engine than the plain one says which."
  (if (eq *engine* :gendl)
      *app-name*
      (format nil "~a (~(~a~))" *app-name* (engine-name))))

(defun app-short-name ()
  (if (eq *engine* :gendl)
      *app-short-name*
      (format nil "~a ~(~a~)" *app-short-name* (engine-name))))

(defun app-color (token fallback)
  "String. The colour of TOKEN in the look a visitor meets first -- the
default skin's, else the house look's -- when the sheet gives it as a
plain colour; FALLBACK otherwise.  An installer paints with it before any
stylesheet is read."
  (flet ((colour (file)
           (let ((value (and file (token-value file token))))
             (and value (plusp (length value)) (char= (char value 0) #\#) value))))
    (or (let ((skin (find-skin *default-skin*)))
          (and skin (colour (getf skin :file))))
        (and (sluice-skins?)
             (colour (probe-file (format nil "~a/tokens.css" sluice:*static*))))
        (colour (static-file "prompt-lab.css"))
        fallback)))

(defun manifest-icons ()
  "Vector. The icons of *app-icons* the static directory holds, as a
manifest lists them."
  (map 'vector #'(lambda (icon)
                   (destructuring-bind (file sizes purpose) icon
                     (h "src" (static-url file)
                        "sizes" sizes
                        "type" "image/png"
                        "purpose" purpose)))
       (remove-if-not #'(lambda (icon) (static-file (first icon))) *app-icons*)))

(defun manifest ()
  "Hash table. The web app manifest of this lab."
  (h "id" *url-prefix*
     "name" (app-name)
     "short_name" (app-short-name)
     "description" *app-description*
     ;; app=1: the page opened from its icon takes up the session this
     ;; browser was last in
     "start_url" (format nil "~a?app=1" *url-prefix*)
     ;; the sheet answers at /sessions/<id>/, outside the prefix, and an
     ;; installed app keeps in its window only what is in scope
     "scope" "/"
     "display" "standalone"
     "theme_color" (app-color "--pl-label-bg" "#101010")
     "background_color" (app-color "--pl-bg" "#f7f6f2")
     "categories" (vector "productivity" "utilities")
     "icons" (manifest-icons)
     "shortcuts" (coerce
                  (append (list (h "name" "New session" "url" *url-prefix*))
                          (when *browsing?*
                            (list (h "name" "Live sessions"
                                     "url" (format nil "~a/sheet-list?browse=live" *url-prefix*))
                                  (h "name" "Archive"
                                     "url" (format nil "~a/sheet-list?browse=archive" *url-prefix*)))))
                  'vector)))

(defun manifest-door (req ent)
  "GET <prefix>/manifest.webmanifest."
  (respond-text req ent (ascii-json (encode (manifest))) "application/manifest+json"))


;;
;; A deployed web app as an installable app, every one alike and nothing
;; in its own file: its page (web-app, kinds.lisp) links a manifest of its
;; own, <prefix>/d/<name>/manifest.webmanifest (deployed-door), named from
;; the Deploy fields and starting at the app's own address, with the
;; house's icons.  No service worker, as the lab keeps none: the page is
;; live (gdlAjax and the stream) and must never be answered from a cache,
;; and a browser installs an app without one.  Links and a manifest only,
;; all from the app's own origin, so a strict content policy leaves them be.
;;

(defparameter *deployed-theme-color* "#4f46e5"
  "String. The colour an installed deployment's window bar wears: the
demos' primary colour, which a web app's page is drawn in.")

(defparameter *deployed-background-color* "#f9fafb"
  "String. The colour of an installed deployment's splash: the demos'
page background.")

(defparameter *short-name-length* 12
  "Integer. The most characters a home screen shows under an icon.")

(defparameter *short-name-trailers*
  '("a" "an" "the" "and" "or" "of" "for" "to" "in" "on" "with" "by" "at" "&" "-")
  "List of strings. Words a shortened name does not end on.")

(defun short-app-name (title)
  "TITLE as a home screen can show it: whole, when it fits
*short-name-length*; else as many of its first words as fit, less any of
*short-name-trailers* at the end; else its first characters."
  (let ((title (string-trim " " title)))
    (if (<= (length title) *short-name-length*)
        title
        (let ((words (loop with kept = nil
                           for word in (remove "" (uiop:split-string title :separator " ") :test #'string=)
                           for length = (length word) then (+ length 1 (length word))
                           while (<= length *short-name-length*)
                           do (push word kept)
                           finally (return kept))))
          (loop while (and (rest words) (member (first words) *short-name-trailers* :test #'string-equal))
                do (pop words))
          (if words
              (format nil "~{~a~^ ~}" (reverse words))
              (subseq title 0 *short-name-length*))))))

(defun deployment-own-host? (record)
  "True when RECORD's deployment is a web app on a host of its own
(deployment-url), whose root is the app."
  (and (app-origin-pattern?) (equal (gethash "kind" record) "app")))

(defun deployment-manifest (name record)
  "Hash table. The web app manifest of deployment NAME (RECORD).  Its scope
is the whole host: the page lives at the address gwl mints for each visit
(/sessions/<id>/), outside the deployment's path, and on a host of its own
the host is the app's alone."
  (let ((title (let ((title (gethash "title" record)))
                 (if (and (stringp title) (plusp (length (string-trim " " title)))) title name)))
        (blurb (gethash "blurb" record)))
    (h "id" (deployment-path name)
       "name" title
       "short_name" (short-app-name title)
       "description" (if (stringp blurb) blurb "")
       "start_url" (if (deployment-own-host? record) "/" (deployment-path name))
       "scope" "/"
       "display" "standalone"
       "theme_color" *deployed-theme-color*
       "background_color" *deployed-background-color*
       "icons" (manifest-icons))))

(defun deployment-manifest-door (req ent name record)
  "GET <prefix>/d/<name>/manifest.webmanifest."
  (respond-text req ent (ascii-json (encode (deployment-manifest name record))) "application/manifest+json"))

(defun deployment-head-links (name)
  "String. What deployment NAME's page carries in its head to install: its
manifest, its window colour, and the icon and name an iPhone's home screen
takes; nil for no such deployment."
  (let ((record (deployment-record name)))
    (when record
      (let ((title (gethash "name" (deployment-manifest name record))))
        (with-lhtml-string ()
          (:link :rel "manifest" :href (format nil "~a/manifest.webmanifest" (deployment-path name)))
          (:meta :name "theme-color" :content *deployed-theme-color*)
          (:meta :name "apple-mobile-web-app-title" :content (escape-string (short-app-name title)))
          (when (static-file "icons/apple-touch-icon.png")
            (htm (:link :rel "apple-touch-icon" :href (static-url "icons/apple-touch-icon.png")))))))))


(defun worker-text ()
  "String. The worker that retires the one the lab's first page kept."
  (format nil "// The prompt lab keeps no worker any more: this one drops the lab's~%~
               // caches and unregisters itself.~%~
               var MINE = 'prompt-lab:' + ~a + ':';~%~
               self.addEventListener('install', function () { self.skipWaiting(); });~%~
               self.addEventListener('activate', function (event) {~%  ~
               event.waitUntil(caches.keys().then(function (names) {~%    ~
               return Promise.all(names.filter(function (name) { return name.indexOf(MINE) === 0; })~%      ~
               .map(function (name) { return caches.delete(name); }));~%  ~
               }).then(function () { return self.registration.unregister(); }));~%~
               });~%"
          (ascii-json (encode *url-prefix*))))

(defun worker-door (req ent)
  "GET <prefix>/worker: the retiring worker, allowed the reach the old
one had: the prefix itself."
  (respond-text req ent (worker-text) "text/javascript; charset=utf-8"
                :headers (list (cons :service-worker-allowed *url-prefix*))))

(defun respond-text (req ent text content-type &key headers)
  "Answer TEXT, which is ASCII, as CONTENT-TYPE; never kept by a cache."
  (net.aserve:with-http-response (req ent :content-type content-type)
    (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
    (net.aserve:with-http-body (req ent :headers headers)
      (write-string text net.html.generator:*html-stream*))))
