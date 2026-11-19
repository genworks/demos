;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; THE FRONT OF A FLEET OF SESSION SHIPS.  Each visitor's session can run
;; in a ship of its own: a one-shot lab (worker.lisp) and a console beside
;; it, raised and struck by a launcher on the host (basilisk's
;; ships/session/ship-launcher), routed by the reverse proxy at
;; <prefix>/s/<ship>.  The lab that greets visitors is then the FRONT: it
;; shows the page, the archive and the listings as ever, but opens no
;; session of its own.  A visitor's first Build there claims a ship from
;; the launcher's warm pool and sends the browser to it, the prompt
;; carried along (plRoute, as for the sibling lab); with the pool spent,
;; the page waits in line, first come first served, and moves on by
;; itself when a ship comes free.
;;
;; The front and the launcher share one directory, *ships-home*: ready/
;; holds the booted, unclaimed ships, one empty file each, and live/ the
;; claimed ones (the reverse proxy routes a ship while its name is there).
;; A claim is one rename, ready/<ship> to live/<ship>: of two claims on
;; one ship the second finds no file and tries the next.  The front needs
;; write access to both directories, nothing else of the launcher's.
;;
;; The daily cap on sessions per address (*max-sessions-per-address*) is
;; kept here, where every ship's session starts; a ship counts only its
;; own one.
;;

(defparameter *ships-home* nil
  "String or nil.  The ship launcher's directory, as this lab sees it:
set, this lab is the FRONT of a fleet of session ships and opens no
session itself.")

(defparameter *ships-wait-limit* 1200
  "Seconds a visitor waits in line for a ship before the place is given up.")

(defparameter *ships-dispatch-interval* 2
  "Seconds between looks at the pool while visitors wait in line.")

(defun ships-front? ()
  (and *ships-home* t))

(defun ships-dir (name)
  (merge-pathnames (format nil "~a/" name) (uiop:ensure-directory-pathname *ships-home*)))

(defun ship-name? (name)
  (and (stringp name) (< 0 (length name) 64)
       (every #'(lambda (c) (or (char<= #\a c #\z) (char<= #\0 c #\9) (char= c #\-))) name)))

(defun ship-url (ship)
  "The page of SHIP, as the reverse proxy routes it."
  (format nil "~a/s/~a" *url-prefix* ship))

(defun ships-ready ()
  "The names of the ships in the warm pool, oldest name first."
  (sort (remove-if-not #'ship-name? (mapcar #'file-namestring (uiop:directory-files (ships-dir "ready"))))
        #'string<))

(defun claim-ship! ()
  "Take a ship from the pool for a visitor: its name, or nil when the pool
is spent."
  (dolist (name (ships-ready))
    (when (ignore-errors (rename-file (merge-pathnames name (ships-dir "ready"))
                                      (merge-pathnames name (ships-dir "live")))
                         t)
      (return name))))

;;
;; The line.  A waiting visitor is three functions of the page's: DELIVER
;; sends the browser to a ship, ALIVE says whether the page is still open,
;; TELL shows it a line of text.
;;

(defstruct ship-wait deliver alive tell address (since (get-universal-time)) (told 0))

(defvar *ships-line* nil "The visitors waiting for a ship, first come first.")
(defvar *ships-line-lock* (bt:make-lock "prompt-lab ships line"))
(defvar *ships-dispatcher* nil)

(defun ships-line-text (place)
  (format nil "Every lab is in use just now: you are number ~d in line.  Keep this page open; it takes you to your lab by itself."
          place))

(defparameter *ship-ready-text*
  "Your lab is ready: a session of its own, with its own Lisp and console.  Taking your request there now.")

(defun ship-deliver! (wait ship)
  "Send WAIT's visitor to SHIP and count the session against the address."
  (count-address! (ship-wait-address wait) :sessions)
  (funnel-count! "ships")
  (ignore-errors (funcall (ship-wait-deliver wait) ship)))

(defun request-ship! (address deliver alive tell)
  "A visitor at ADDRESS asks for a ship.  Values: :sent and the ship, sent
there through DELIVER; :queued and the place in line; or nil and the
reason."
  (let (wait ship place)
    (bt:with-lock-held (*ships-line-lock*)
      (cond ((find address *ships-line* :key #'ship-wait-address :test #'equal)
             (return-from request-ship!
               (values nil "This address is already in line for a lab: keep that page open.")))
            (t (setf wait (make-ship-wait :deliver deliver :alive alive :tell tell :address address))
               ;; nobody ahead: the pool, if it has a ship
               (unless *ships-line* (setf ship (claim-ship!)))
               (unless ship
                 (setf *ships-line* (append *ships-line* (list wait))
                       place (length *ships-line*)
                       (ship-wait-told wait) place)))))
    (cond (ship (ship-deliver! wait ship) (values :sent ship))
          (t (ensure-ships-dispatcher!) (values :queued place)))))

(defun dispatch-ships! ()
  "One look at the line: the gone and the too long dropped, the first in
line given what ships the pool has, the rest told their new places."
  (let ((now (get-universal-time)) given expired told)
    (bt:with-lock-held (*ships-line-lock*)
      (setf *ships-line*
            (remove-if #'(lambda (wait)
                           (cond ((not (ignore-errors (funcall (ship-wait-alive wait)))) t)
                                 ((> (- now (ship-wait-since wait)) *ships-wait-limit*) (push wait expired) t)))
                       *ships-line*))
      (loop while *ships-line*
            for ship = (claim-ship!)
            while ship
            do (push (cons (pop *ships-line*) ship) given))
      (loop for wait in *ships-line*
            for place from 1
            unless (= place (ship-wait-told wait))
              do (setf (ship-wait-told wait) place)
                 (push (cons wait place) told)))
    (dolist (pair (nreverse given)) (ship-deliver! (car pair) (cdr pair)))
    (dolist (wait expired)
      (ignore-errors (funcall (ship-wait-tell wait) "No lab came free in time.  Try again in a few minutes.")))
    (dolist (pair told)
      (ignore-errors (funcall (ship-wait-tell (car pair)) (ships-line-text (cdr pair)))))))

(defun ensure-ships-dispatcher! ()
  "The thread that serves the line, started with the first visitor in it."
  (bt:with-lock-held (*ships-line-lock*)
    (unless (and *ships-dispatcher* (bt:thread-alive-p *ships-dispatcher*))
      (setf *ships-dispatcher*
            (bt:make-thread #'(lambda ()
                                (loop (sleep *ships-dispatch-interval*)
                                      (ignore-errors (dispatch-ships!))))
                            :name "prompt-lab ships line")))))

(defun ships-line-length ()
  (bt:with-lock-held (*ships-line-lock*) (length *ships-line*)))
