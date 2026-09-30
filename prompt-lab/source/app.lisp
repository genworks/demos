;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; The lab as an installable app.  Two doors make the page one:
;;
;;   <prefix>/manifest.webmanifest   what the app is called, its icons,
;;                                   where it starts
;;   <prefix>/worker                 a service worker that keeps the
;;                                   page's SHELL -- the page, its
;;                                   stylesheets, script, skins, icons
;;
;; and nothing else changes: installed or not it is the same page at the
;; same address.  What the worker keeps is the shell ALONE.  The doors
;; under <prefix>/api/ and the viewer are the engine's and are never
;; kept: a model is built, drawn and saved on the server, and an app
;; that showed yesterday's answer as today's would be worse than one
;; that says it cannot reach the lab.  Without a connection the page
;; opens, shows the last session as this browser last saw it, and says
;; so.
;;
;; Both doors are written here rather than kept as files because each
;; names the lab's prefix, and one system serves labs at several.
;;

(defparameter *app?* t
  "Boolean. Whether the page registers its service worker.  A worker once
installed stays in a visitor's browser until it is replaced, so turning
this off does two things: the page stops registering one, and the worker's
door answers a worker that drops this lab's caches and unregisters itself,
which a browser that holds the old one fetches in its place.")

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

(defun app? () *app?*)

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
     ;; no slash after it: the page IS the prefix
     "scope" *url-prefix*
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
                                     "url" (format nil "~a?browse=live" *url-prefix*))
                                  (h "name" "Archive"
                                     "url" (format nil "~a?browse=archive" *url-prefix*)))))
                  'vector)))

(defun manifest-door (req ent)
  "GET <prefix>/manifest.webmanifest."
  (respond-text req ent (ascii-json (encode (manifest))) "application/manifest+json"))


;;
;; The worker.  static/worker.js is its text, with three places for this
;; side to fill in: the lab's prefix, the addresses of the shell, and a
;; name for the cache that changes when any file of the shell does -- a
;; changed name is a changed worker, which a browser installs in place
;; of the old one, and the new one drops the old cache.
;;

(defun shell-addresses ()
  "List of strings. What the worker keeps: the page and every static file
it may ask for."
  (append (list *url-prefix*
                (tokens-url)
                (static-url "prompt-lab-page.css")
                (static-url "prompt-lab.js"))
          (let ((split (split-url))) (and split (list split)))
          (mapcar #'(lambda (skin) (getf skin :href)) (skins))
          (mapcar #'(lambda (icon) (static-url (first icon)))
                  (remove-if-not #'(lambda (icon) (static-file (first icon))) *app-icons*))
          (and (static-file "icons/apple-touch-icon.png")
               (list (static-url "icons/apple-touch-icon.png")))))

(defun worker-text ()
  "String. The service worker's script, filled in; with *app?* off, the
worker that retires the one before it."
  (if *app?*
      (let ((signature (list *url-prefix* *default-skin* (static-signature) (shell-addresses))))
        (reduce #'(lambda (text place)
                    (replace-substring text (car place) (cdr place)))
                (list (cons "{{cache}}"
                            (ascii-json (encode (format nil "prompt-lab:~a:~36r"
                                                        *url-prefix*
                                                        (sxhash (prin1-to-string signature))))))
                      (cons "{{prefix}}" (ascii-json (encode *url-prefix*)))
                      ;; where the shell's files are: the lab's own, and
                      ;; the sluice's, whose tokens and skins the page wears
                      (cons "{{kept}}" (ascii-json
                                        (encode (vector (format nil "~a/static/" *url-prefix*)
                                                        (format nil "~a/sluice-static/" *url-prefix*)))))
                      (cons "{{shell}}" (ascii-json (encode (coerce (shell-addresses) 'vector)))))
                :initial-value (uiop:read-file-string (merge-pathnames "worker.js" *static-directory*)
                                                      :external-format :utf-8)))
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
              (ascii-json (encode *url-prefix*)))))

(defun worker-door (req ent)
  "GET <prefix>/worker: the service worker.  It sits beside the page, not
above it, so the answer says how far it may reach: the prefix itself."
  (respond-text req ent (worker-text) "text/javascript; charset=utf-8"
                :headers (list (cons :service-worker-allowed *url-prefix*))))

(defun respond-text (req ent text content-type &key headers)
  "Answer TEXT, which is ASCII, as CONTENT-TYPE; never kept by a cache."
  (net.aserve:with-http-response (req ent :content-type content-type)
    (setf (net.aserve:reply-header-slot-value req :cache-control) "no-store")
    (net.aserve:with-http-body (req ent :headers headers)
      (write-string text net.html.generator:*html-stream*))))
