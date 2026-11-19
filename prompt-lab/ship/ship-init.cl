;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;; ship-init.cl -- the prompt lab as a SESSION SHIP's lab: the whole lab
;;; serving exactly one session (source/worker.lisp, *one-session?*), in a
;;; locked-down room of its own beside that session's own console.  When
;;; its one session has been reaped the process quits, and the ship is
;;; struck and raised fresh, so the next visitor meets a fresh Lisp.
;;; Mounted read-only as the room's services init (for a Gendl image,
;;; /home/gendl-user/gdl-services-init.cl).
;;;
;;; The ship's template is basilisk's ships/session/.  The environment says
;;; what differs between deployments:
;;;   WORKER_NAME             the ship's name (letters, digits, hyphens)
;;;   LAB_PREFIX              the lab's prefix, e.g. /prompt-lab/s/<ship>
;;;   LAB_CONSOLE             set: the ship carries a console, linked at
;;;                           <prefix>/console
;;;   LAB_SESSION_LIFETIME    idle seconds before the session ends (1800)
;;;   LAB_MESSAGES_URL        the gate's Messages door
;;;                           (default http://gate-relay/llm/messages)
;;;   LAB_TURNSTILE_SITE_KEY  the human check's public site key (none: off)
;;;   LAB_NAME                the lab's name at the gate
;;;
;;; Because nothing here is shared with another visitor, hand edits are on.
;;; Not opened: a visitor's own MCP agent (its doors want more thought) and
;;; search_docs (its door is an Emacs this room cannot reach).

(in-package :gdl-user)

(defun ship-env (name &optional default)
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value))) value default)))

(defparameter *ship-name*
  (let ((name (ship-env "WORKER_NAME")))
    (if (and name (every #'(lambda (c) (or (alphanumericp c) (char= c #\-))) name))
        name "ship")))

(load-quicklisp)

;; the web server: a basilisk room starts it before its init runs; a room
;; started otherwise does not
(unless (ignore-errors (funcall (read-from-string "gwl::server-running?")))
  (ignore-errors (gendl:start-gendl!)))

(handler-case
    (progn
      ;; the lab, demos-common and monocle from the read-only checkouts, on
      ;; ASDF's own registry (Quicklisp's local projects would write an
      ;; index into a read-only directory); the fasls land in ~/.cache, a
      ;; tmpfs
      (dolist (dir '("/demos-pristine/prompt-lab/" "/demos-pristine/prompt-lab/prompt-lab-sheet/"
                     "/demos-pristine/demos-common/" "/monocle/"))
        (when (probe-file dir) (pushnew (pathname dir) asdf:*central-registry* :test #'equal)))
      (ql:quickload :prompt-lab)
      (ql:quickload :prompt-lab-sheet)
      (flet ((set! (name value) (setf (symbol-value (read-from-string name)) value)))
        (let ((prefix (ship-env "LAB_PREFIX" (format nil "/prompt-lab/~a" *ship-name*))))
          (set! "prompt-lab::*url-prefix*" prefix)
          ;; the ship's own console, beside the lab
          (set! "prompt-lab:*console-base*"
                (and (ship-env "LAB_CONSOLE") (format nil "~a/console" prefix))))
        ;; ONE SESSION, then quit
        (set! "prompt-lab::*one-session?*" t)
        (set! "prompt-lab::*session-lifetime*"
              (or (ignore-errors (parse-integer (ship-env "LAB_SESSION_LIFETIME"))) 1800))
        (set! "prompt-lab::*reaper-interval*" 60)
        (set! "prompt-lab::*messages-url*" (ship-env "LAB_MESSAGES_URL" "http://gate-relay/llm/messages"))
        (set! "prompt-lab:*turnstile-site-key*" (ship-env "LAB_TURNSTILE_SITE_KEY"))
        (set! "prompt-lab::*engine*" :gendl)
        (set! "prompt-lab::*sibling-lab*" nil)
        (set! "prompt-lab::*search-url*" nil)
        (set! "prompt-lab::*free-lab?*" t)
        (set! "prompt-lab::*author-keys?*" t)
        (set! "prompt-lab::*kinds*" (list :model))
        (set! "prompt-lab::*external-agent?*" nil)
        ;; walled off, so the hand edits are on
        (set! "prompt-lab::*visitor-edits?*" t)
        (set! "prompt-lab::*password-file*" nil)
        ;; its archive is its own directory; the public front imports it
        (set! "prompt-lab::*archive-root*" "/worker-archive/")
        (when (ship-env "LAB_NAME")
          (set! "prompt-lab::*lab-name*" (ship-env "LAB_NAME")))
        ;; no thumbnailer: its loads would go in beside the session's; the
        ;; front draws the thumbnails of what it imports
        (set! "prompt-lab::*thumbnails?*" nil))
      ;; THE LOAD GATE (source/gate.lisp): no request runs while a model file
      ;; loads.  One line to drop once Gendl closes CCL's redefinition race.
      (when (find-symbol "INSTALL-LOAD-GATE!" :prompt-lab)
        (format t "~&[ship ~a] load gate: ~s~%" *ship-name*
                (funcall (read-from-string "prompt-lab::install-load-gate!"))))
      (funcall (read-from-string "prompt-lab:publish-prompt-lab!"))
      (funcall (read-from-string "prompt-lab::publish-lab-sheet!"))
      (format t "~&[ship ~a] one-session lab at ~a~%" *ship-name*
              (symbol-value (read-from-string "prompt-lab::*url-prefix*"))))
  (error (e)
    (let ((*print-readably* nil))
      (format t "~&[ship ~a] lab FAILED to start: ~a~%" *ship-name* e))
    ;; a lab that cannot serve quits, and its room tries again
    (uiop:quit 1)))

(gwl:start-session-reaper :debug? nil)
