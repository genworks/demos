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
     "icons" (map 'vector #'(lambda (icon)
                              (destructuring-bind (file sizes purpose) icon
                                (h "src" (static-url file)
                                   "sizes" sizes
                                   "type" "image/png"
                                   "purpose" purpose)))
                  (remove-if-not #'(lambda (icon) (static-file (first icon))) *app-icons*))
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
