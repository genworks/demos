;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;
;; look_at_page: a web app's page as a visitor's browser shows it.
;; check_app reads the page's structure on the Lisp side; this loads the
;; page in a real browser, at desktop and phone widths, and reports what
;; only a browser knows -- whether the page settled, the console's
;; errors, the requests that failed or were refused, a page wider than
;; its screen, a 3D scene that drew nothing or drew black -- with the
;; screenshots, so the agent sees the layout itself.  One optional click,
;; with a second picture once the page has settled again.
;;
;; The browser is not here.  It is weblook-server, in a room of its own
;; (the page is a visitor's code, and must not run beside the lab), asked
;; over HTTP at the session's glass endpoint: the ship's glass room today,
;; a session's own room tomorrow -- the same protocol either way.  What a
;; look guarantees travels with weblook, not with the room: a fresh
;; browser for every width, and every request off the lab's own address
;; refused.  The lab adds the LOOK TOKEN: a private session's app wants
;; its owner's key on the address, and the key never leaves the lab; the
;; browser gets a token good for this look's few loads, for a minute.
;;

(defun glass-endpoint (session)
  "Where SESSION's pages are looked at: the glass URL and the lab's base as
that browser reaches it (two values), or nil when there is no browser."
  (if *glass-endpoint-function*
      (funcall *glass-endpoint-function* session)
      (and *glass-url* *glass-lab-base* (values *glass-url* *glass-lab-base*))))

(defun look-offered? (session)
  "Whether SESSION's agent gets look_at_page: a web app, and a browser to
look with."
  (and session (eq (session-kind session) :app) (not (session-replay? session))
       (glass-endpoint session) t))

;;
;; The look token.
;;

(defvar *look-tokens* (make-hash-table :test #'equal)
  "token -> (session-id uses expires), the loads a look may make.")

(defvar *look-lock* (bt:make-lock "prompt-lab looks"))

(defparameter *look-token-seconds* 90
  "Integer. How long a look token opens its session's app.")

(defun mint-look-token! (session uses)
  "A token that opens SESSION's app USES times, for *look-token-seconds*."
  (let ((token (new-owner-key))
        (now (get-universal-time)))
    (bt:with-lock-held (*look-lock*)
      ;; the spent and the stale go as new ones come
      (loop for key being the hash-keys of *look-tokens* using (hash-value entry)
            when (or (<= (second entry) 0) (< (third entry) now))
              collect key into dead
            finally (dolist (key dead) (remhash key *look-tokens*)))
      (setf (gethash token *look-tokens*)
            (list (session-id session) uses (+ now *look-token-seconds*))))
    token))

(defun look-token-key (session token)
  "SESSION's owner key when TOKEN is a live look token for it (using one
of its loads up), else nil.  The app door's stand-in for owner=."
  (when (and (stringp token) (plusp (length token)))
    (bt:with-lock-held (*look-lock*)
      (let ((entry (gethash token *look-tokens*)))
        (when (and entry (equal (first entry) (session-id session))
                   (plusp (second entry)) (>= (third entry) (get-universal-time)))
          (decf (second entry))
          (session-owner session))))))

;;
;; How often a session may look.
;;

(defvar *look-times* (make-hash-table :test #'equal)
  "session id -> the universal times of its looks within the hour.")

(defun look-allowed! (session)
  "True, counting the look, when SESSION has looks left this hour."
  (let ((now (get-universal-time))
        (id (session-id session)))
    (bt:with-lock-held (*look-lock*)
      (let ((recent (remove-if #'(lambda (time) (< time (- now 3600))) (gethash id *look-times*))))
        (cond ((>= (length recent) *max-looks-per-hour*)
               (setf (gethash id *look-times*) recent)
               nil)
              (t (setf (gethash id *look-times*) (cons now recent))
                 t))))))

;;
;; The look.
;;

(defun look-viewports (width)
  "The widths to look at: WIDTH alone (a phone's when narrow), or the
server's default -- a desktop and a phone."
  (when (and (integerp width) (<= 240 width 2560))
    (list (h "name" (format nil "~d px" width) "width" width
             "height" (if (< width 600) 844 800) "mobile" (if (< width 600) t 'yason:false)))))

(defun look-path (session token)
  "The app's address below the lab's base, with the look token."
  (format nil "~a/app?session=~a&look=~a" *url-prefix* (session-id session) token))

(defun look-text (report verdict)
  "The words of a look: REPORT, weblook's (a hash table), for the agent."
  (flet ((g (table &rest keys)
           (loop for key in keys
                 while (hash-table-p table)
                 do (setq table (gethash key table))
                 finally (return table))))
    (with-output-to-string (out)
      (format out "The app in a browser, as a visitor sees it.~%~a~%" verdict)
      (when (g report "error") (format out "The look did not finish: ~a~%" (g report "error")))
      (dolist (vp (g report "viewports"))
        (let ((settled (g vp "settled")) (layout (g vp "layout")) (requests (g vp "requests"))
              (scene (g vp "scene")) (click (g vp "click")))
          (format out "~%~:@(~a~) (~ax~a~:[~;, a phone~]):~%" (g vp "name") (g vp "width") (g vp "height") (g vp "mobile"))
          (cond ((g vp "load" "error") (format out "- The page did not load: ~a~%" (g vp "load" "error")))
                ((not (g vp "load" "fired")) (format out "- The page's load event never fired.~%")))
          (if (g settled "ok")
              (format out "- Settled in ~,1f s.~%" (/ (or (g settled "ms") 0) 1000.0))
              (format out "- Did NOT settle in ~,1f s~@[: requests still open: ~{~a~^, ~}~]~:[~;; requests kept coming~]~:[~;; the DOM kept changing~]~@[; the busy overlay is ~a~].~%"
                      (/ (or (g settled "ms") 0) 1000.0) (g settled "open_requests") (g settled "requests_kept_coming") (g settled "dom_changing")
                      (let ((o (g settled "overlay"))) (and o (not (equal o "hidden")) o))))
          (let ((over (g layout "overflows_x")))
            (if (and (realp over) (plusp over))
                (format out "- The page is ~d px wider than the screen~@[ (the browser zoomed out to ~a px)~]~@[; reaching past it: ~{~a~^, ~}~].~%"
                        over (g layout "zoomed_out_to") (g layout "too_wide"))
                (format out "- It fits the screen's width; the page is ~a px tall.~%" (g layout "page_height"))))
          (let ((console (g vp "console")))
            (if console
                (dolist (entry console)
                  (format out "- Console ~a: ~a~@[ (~a)~]~@[ [x~a]~]~%" (g entry "level") (g entry "text") (g entry "at") (g entry "count")))
                (format out "- Console: no errors or warnings.~%")))
          (dolist (entry (g requests "failed"))
            (format out "- Request failed: ~a: ~a~@[ (blocked: ~a)~]~%" (g entry "url") (g entry "error") (g entry "blocked")))
          (dolist (entry (g requests "refused_by_server"))
            (format out "- Refused by the server: ~a ~a~@[ -- ~a~]~%" (g entry "status") (g entry "url") (g entry "body")))
          (dolist (entry (g requests "refused_off_origin"))
            (format out "- Refused because it leaves the app's own address (an app loads nothing from elsewhere): ~a (~a)~%"
                    (g entry "url") (g entry "type")))
          (when scene
            (if (g scene "drew")
                (format out "- 3D scene (~a): ~:d triangles a frame" (g scene "kind") (or (g scene "triangles_per_frame") 0))
                (format out "- 3D scene (~a): NOTHING DREW in ~,1f s" (g scene "kind") (/ (or (g scene "waited_ms") 0) 1000.0)))
            (format out "~@[, ~a Shape elements~]~@[, canvas ~a~].~%"
                    (let ((n (g scene "shape_elements"))) (and (realp n) (plusp n) n))
                    (g scene "canvas"))
            (let ((black (g scene "canvas_black_share")))
              (cond ((g scene "canvas_one_colour")
                     (format out "  The canvas is ONE FLAT COLOUR~:[~; (black)~]: nothing visible was drawn.~%" (and (realp black) (> black 0.9))))
                    ((and (realp black) (> black 0.6))
                     (format out "  ~d% of the canvas is black.~%" (round (* 100 black))))))
            (let ((gaps (g scene "frame_gaps_over_100ms")))
              (when gaps (format out "  Frames over 100 ms apart in 2 s (software rendering, so relative only): ~{~a~^, ~} ms.~%" gaps)))
            (when (and (equal (g scene "kind") "X_ITE") (not (g scene "x_ite_scene_loaded")))
              (format out "  X_ITE has no scene loaded.~%")))
          (when click
            (cond ((not (g click "found"))
                   (format out "- Click: nothing matches ~a.~%" (g click "selector")))
                  (t
                   (format out "- Clicked ~a~@[ (\"~a\")~], ~a~@[; but ~a covers it, and took the click~].~%"
                           (g click "selector") (let ((s (g click "text"))) (and (plusp (length s)) s))
                           (g click "size") (g click "covered_by"))
                   (let ((events (g click "events")))
                     (if events
                         (format out "  After the click:~{ ~a;~}~%" events)
                         (format out "  After the click: no busy overlay, nothing replaced.~%")))
                   (let ((after (g click "settled")))
                     (if (g after "ok")
                         (format out "  It settled again in ~,1f s.~%" (/ (or (g after "ms") 0) 1000.0))
                         (format out "  It did NOT settle again in ~,1f s~@[: open ~{~a~^, ~}~]~@[; busy overlay ~a~].~%"
                                 (/ (or (g after "ms") 0) 1000.0) (g after "open_requests") (g after "overlay"))))))))))))

(defun look-pictures (report)
  "The screenshots of REPORT as (label . base64) pairs."
  (loop for vp in (gethash "viewports" report)
        append (loop for shot in (gethash "shots" vp)
                     collect (cons (format nil "~a (~ax~a)" (gethash "name" shot) (gethash "width" shot) (gethash "height" shot))
                                   (gethash "png" shot)))))

(defparameter *look-page-ask*
  "Look at the pictures for what the numbers cannot say: parts overlapping, text cut off or clipped, empty regions, an overlay or spinner left up, text too faint against its background, a phone layout broken where the desktop one is not.  Fix what you find, then look again."
  "What the agent is asked of the pictures (look_at_page).")

(defun sees-pictures? (session)
  "Whether SESSION's model reads images: not a text-only house model."
  (or (not (house-model? session))
      (member (session-model session) *house-vision-models* :test #'equal)))

(defun look-at-page (session &key width click)
  "Have weblook-server look at SESSION's web app; answer its report and the
screenshots (or, for a model that reads text only, a viewer's words on
them)."
  (multiple-value-bind (glass base) (glass-endpoint session)
    (cond
      ((not glass)
       (values (list (text-result "There is no browser for look_at_page here; judge the page by check_app.")) t))
      ((not (app-defined? session))
       (values (list (text-result "No APP is defined yet -- write one with write_model.")) t))
      ((not (look-allowed! session))
       (values (list (text-result "This session has looked ~d times in the last hour; that is the most.  Fix what the last look showed, and check with check_app."
                                  *max-looks-per-hour*))
               t))
      (t
       ;; the browser builds the app as any visitor does: a run (meter.lisp)
       (multiple-value-bind (ok? reason) (meter! session :run (model-volume session))
         (unless ok? (return-from look-at-page (meter-refusal reason))))
       (let* ((verdict (multiple-value-bind (blocks problem?) (let ((*meter?* nil)) (check-app session))
                         (declare (ignore blocks))
                         (if problem?
                             "check_app reports problems with this page: run check_app and fix them as well."
                             "check_app: the page's structure is sound.")))
              (viewports (look-viewports width))
              (token (mint-look-token! session (max 1 (length (or viewports '(1 2))))))
              (spec (h "url" (format nil "~a~a" base (look-path session token)))))
         (when viewports (setf (gethash "viewports" spec) viewports))
         (when (and (stringp click) (plusp (length click)))
           (setf (gethash "click" spec) (h "selector" click)))
         (handler-case
             (multiple-value-bind (status text) (post-json glass (encode spec) :seconds *look-seconds*)
               (let ((report (ignore-errors (yason:parse text))))
                 (cond
                   ((not (and (hash-table-p report) (gethash "viewports" report)))
                    (values (list (text-result "The browser could not look (~a~@[: ~a~])."
                                               status (and (hash-table-p report) (gethash "error" report))))
                            t))
                   (t
                    (let ((words (look-text report verdict))
                          (pictures (look-pictures report)))
                      (cond
                        ((sees-pictures? session)
                         (values (append (list (text-result "~a~%~a" words *look-page-ask*))
                                         (loop for (label . base64) in pictures
                                               append (list (text-result "~a:" label) (image-block base64))))
                                 nil))
                        (*house-viewer-model*
                         (multiple-value-bind (said error?)
                             (viewer-words session (mapcar #'(lambda (picture) (image-block (cdr picture))) pictures)
                                           (format nil "These are screenshots of a web page built for this request:~%~%~a~%~%~a  In under 200 words, say what you see.  Be concrete; do not praise."
                                                   (latest-request session) *look-page-ask*))
                           (values (list (text-result "~a~%What a viewer sees in the screenshots: ~a" words
                                                      (if error? "(the viewer did not answer)" said)))
                                   nil)))
                        (t (values (list (text-result "~a" words)) nil))))))))
           (serious-condition (condition)
             (values (list (text-result "The browser could not be asked: ~a" condition)) t))))))))

(defun look-page-tool-definition ()
  `(("name" . "look_at_page")
    ("description" . "Open the web app in a real browser, as a visitor would, and see it: a screenshot at desktop and phone widths, the browser console's errors and warnings, requests that failed or were refused, whether the page settled, whether it is wider than the screen, and -- when it shows a 3D scene -- whether the scene drew. Optionally click one element (a CSS selector) and get a second screenshot once the page has settled again, to see an update land. Use it after check_app is clean, and fix what it shows.")
    ("input_schema" . ,(schema `(("width" . (("type" . "integer")
                                             ("description" . "Look at this one width (px) instead of a desktop and a phone; below 600 is a phone.")))
                                 ("click" . (("type" . "string")
                                             ("description" . "A CSS selector to click once the page has settled, e.g. \"#add\" or \"button.go\"."))))
                               nil))))
