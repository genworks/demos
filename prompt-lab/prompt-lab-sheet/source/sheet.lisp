;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;;;
;;;; The lab as one gwl sheet, at <prefix>/sheet.
;;;;
;;;; The same sessions, agent, guards and archive as the page at
;;;; <prefix>; what differs is the page.  There the browser polls a JSON
;;;; door every two seconds and redraws by hand; here each part of the
;;;; page is a section of a gwl sheet, and the sheet hears every change
;;;; to its session (*session-change-hooks*: each log entry, each save)
;;;; and pushes the sections that changed over its stream (gwl's
;;;; datastar-mixin).  The model is drawn by a viewport of the same sheet.
;;;;
;;;; Ownership is the page's: the owner key is kept in the browser
;;;; (localStorage prompt-lab-owners, id -> key), so a session opened
;;;; here carries on at <prefix> and the other way round.  A sheet opened
;;;; on ?session=<id> shows it to anyone it is visible to, and the claim
;;;; action makes it the owner's when the browser holds the key.
;;;;
;;;; Not yet here (the page at <prefix> has them): the editor and Save,
;;;; the sluice's tree and inputs (the viewer link opens them), credits
;;;; and top-ups, downloads, privacy, the browsing listings, the phone
;;;; layout, skins.
;;;;

;;
;; Which sheets show which session.
;;

(defvar *session-sheets* (make-hash-table :test #'equal)
  "Session id -> the sheets showing it.")

(defvar *session-sheets-lock* (bt:make-lock "prompt-lab sheets"))

(defun sheet-alive? (sheet)
  (eq (first (gethash (make-keyword-sensitive (the-object sheet instance-id))
                      gwl::*instance-hash-table*))
      sheet))

(defun watch-session! (sheet session)
  (bt:with-lock-held (*session-sheets-lock*)
    (pushnew sheet (gethash (session-id session) *session-sheets*))))

(defun session-sheets (session)
  "The live sheets showing SESSION; the cleared ones are forgotten here."
  (bt:with-lock-held (*session-sheets-lock*)
    (let ((sheets (remove-if-not #'sheet-alive? (gethash (session-id session) *session-sheets*))))
      (if sheets
          (setf (gethash (session-id session) *session-sheets*) sheets)
          (remhash (session-id session) *session-sheets*))
      sheets)))

(defun model-stamp (session)
  "When SESSION's model was last compiled, or nil when it has none."
  (and (model-defined? session)
       (ignore-errors (file-write-date (make-pathname :type "fasl"
                                                      :defaults (session-model-file session))))))

(defun json-boolean (value) (if value "true" "false"))

(defun sheets-hear (session)
  "A change to SESSION: every sheet showing it takes a new revision (its
log, status and source sections depend on it), a new model stamp when the
model was compiled again (its viewport depends on that), and the busy
signal; then its stream sends what went stale."
  (dolist (sheet (session-sheets session))
    (the-object sheet (set-slot! :revision (1+ (the-object sheet revision))))
    (let ((stamp (model-stamp session)))
      (unless (eql stamp (the-object sheet model-stamp))
        (the-object sheet (set-slot! :model-stamp stamp))))
    (sheet-send! sheet (datastar-signals-event
                        (format nil "{\"busy\": ~a}" (json-boolean (session-busy? session)))))
    (sheet-changed! sheet)))

(pushnew 'sheets-hear *session-change-hooks*)


;;
;; What the browser runs: the owner's key kept where the page keeps it.
;;

(defun owners-script (session)
  "JavaScript: SESSION's owner key into the browser's keys, and the address
bar to the sheet's address for the session, so a reload finds it again."
  (format nil "(function(){var o={};try{o=JSON.parse(localStorage.getItem('prompt-lab-owners')||'{}')||{}}catch(e){}
o[~a]=~a;try{localStorage.setItem('prompt-lab-owners',JSON.stringify(o))}catch(e){}
history.replaceState(null,'',~a)})()"
          (js-string-literal (session-id session))
          (js-string-literal (session-owner session))
          (js-string-literal (format nil "~a/sheet?session=~a" *url-prefix* (session-id session)))))

(defun owner-signal-expression (session-id)
  "A JavaScript expression: this browser's key for SESSION-ID, or ''."
  (format nil "(function(){try{return (JSON.parse(localStorage.getItem('prompt-lab-owners')||'{}')||{})[~a]||''}catch(e){return ''}})()"
          (js-string-literal session-id)))

(defparameter *log-shown* 200
  "Integer. The most log entries the sheet shows, the newest.")


;;
;; The sheet.
;;

(defparameter *sheet-css*
  "body{margin:0;font:14px/1.45 system-ui,sans-serif;background:#f4f4f2;color:#1d1d1b}
.pl-head{display:flex;gap:1rem;align-items:baseline;padding:.6rem 1rem;border-bottom:1px solid #ccc;background:#fff}
.pl-head h1{font-size:1.1rem;margin:0}.pl-engine{color:#666}.pl-head nav{margin-left:auto}.pl-head a{margin-left:.8rem}
.pl-grid{display:grid;grid-template-columns:minmax(18rem,2fr) 3fr;gap:1rem;padding:1rem}
@media (max-width:760px){.pl-grid{grid-template-columns:1fr}}
.pl-card{background:#fff;border:1px solid #d6d6d2;border-radius:4px;padding:.7rem;margin-bottom:.8rem}
.pl-card h2{font-size:.9rem;margin:0 0 .4rem}
#pl-prompt{width:100%;box-sizing:border-box;font:inherit;padding:.4rem;border:1px solid #bbb;border-radius:3px}
.pl-row{display:flex;gap:.8rem;align-items:center;margin-top:.5rem}
.pl-build{padding:.35rem 1.2rem;font:inherit;background:#2b4c7e;color:#fff;border:0;border-radius:3px;cursor:pointer}
.pl-build:disabled{background:#999;cursor:default}.pl-busy{color:#2b4c7e}
.pl-error{color:#a12;margin:0 0 .8rem}
.pl-status{display:flex;flex-wrap:wrap;gap:.3rem 1rem;color:#444}
.pl-log{max-height:55vh;overflow:auto}
.pl-entry{display:grid;grid-template-columns:5.5rem 1fr;gap:.5rem;padding:.2rem 0;border-bottom:1px solid #eee}
.pl-kind{color:#888;font-size:.8rem}.pl-text{white-space:pre-wrap;word-break:break-word}
.pl-prompt .pl-text{font-weight:600}.pl-done .pl-text{color:#1d4d1d}.pl-stopped .pl-text,.pl-tool-error .pl-text{color:#a12}
.pl-tool .pl-text{color:#777;font-family:monospace;font-size:.8rem}
.pl-viewport{position:relative;height:60vh;background:#fff;border:1px solid #d6d6d2;border-radius:4px;margin-bottom:.8rem;overflow:hidden}
.pl-source pre{margin:0;max-height:40vh;overflow:auto;font-size:.8rem}"
  "String. The sheet's own look, until it wears the lab's skins.")




(define-object lab-sheet (datastar-mixin session-control-mixin base-html-page)

  :input-slots
  (("The session shown, a struct (session.lisp); nil until a new visitor's
first build opens one."
    session nil :settable)
   ("The key this browser proved it holds (the claim action), or the one a
build minted here."
    owner-key nil :settable)
   ("Integer. Bumped at every change to the session (sheets-hear): what the
log, status and source sections are recomputed from."
    revision 0 :settable)
   ("When the model was last compiled: what the viewport is recomputed from."
    model-stamp nil :settable))

  :computed-slots
  ((title (lab-title))
   (datastar-actions (list :build :claim))

   (owner? (let ((session (the session)))
             (and session (the owner-key) (owner? session (the owner-key)) t)))

   ;; a new visitor builds into a session of their own; a session's
   ;; page is the owner's to build in
   (editable? (or (null (the session)) (the owner?)))

   (model-object (progn (the model-stamp)
                        (let ((session (the session)))
                          (and session (model-defined? session) (ignore-errors (make-model session))))))

   (leaf-count (let ((model (the model-object)))
                 (or (and model (ignore-errors (length (the-object model leaves)))) 0)))

   (additional-header-content
    (with-lhtml-string ()
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (str (the datastar-head-content))
      (when *turnstile-site-key*
        (htm (:script :src "https://challenges.cloudflare.com/turnstile/v0/api.js" :async "async" :defer "defer")
             (:script "window.plTurnstile=function(t){var e=document.getElementById('pl-turnstile');if(e){e.value=t;e.dispatchEvent(new Event('input',{bubbles:true}))}};")))
      (:style (str *sheet-css*))))

   (initial-signals
    (format nil "{prompt: '', turnstile: '', error: '', sending: false, busy: ~a, editable: ~a, owner: ~a}"
            (json-boolean (and (the session) (session-busy? (the session))))
            (json-boolean (the editable?))
            (if (the session) (owner-signal-expression (session-id (the session))) "''")))

   (body
    (with-lhtml-string ()
      (:div :id "pl-sheet"
            :|data-signals| (escape-string-minimal-plus-quotes (the initial-signals))
            :|data-init| (the datastar-stream-attribute)
            ;; a browser holding this session's key makes the page its owner's
            (when (and (the session) (not (the owner?)))
              (htm (:span :|data-init| (format nil "$owner && ~a" (the (datastar-action :claim))))))
            (:header :class "pl-head"
                     (:h1 (esc (lab-title)))
                     (:span :class "pl-engine" (esc (engine-label)))
                     (:nav (:a :href *url-prefix* "the page") " "
                           (when *sibling-lab*
                             (htm (:a :href (format nil "~a/sheet" (car *sibling-lab*)) (esc (cdr *sibling-lab*)))))))
            (:main :class "pl-grid"
                   (:section :class "pl-left"
                             (str (the prompt-form))
                             (str (the status-section div))
                             (str (the log-section div)))
                   (:section :class "pl-right"
                             (:div :class "pl-viewport" (str (the viewport div)))
                             (str (the source-section div)))))))

   ;; Not a section: never redrawn, so what is being typed stays.  What
   ;; it shows changes through signals.
   (prompt-form
    (with-lhtml-string ()
      (:div :class "pl-card" :|data-show| "$editable"
            (:textarea :id "pl-prompt" :rows "4" :|data-bind:prompt| ""
                       :placeholder "Describe what to build, e.g. a picnic table with two benches")
            (when *turnstile-site-key*
              (htm (:div :class "cf-turnstile" :data-sitekey *turnstile-site-key* :data-callback "plTurnstile")))
            (:input :type "hidden" :id "pl-turnstile" :|data-bind:turnstile| "")
            (:div :class "pl-row"
                  (:button :class "pl-build"
                           :|data-on:click| (the (datastar-action :build))
                           :|data-indicator:sending| ""
                           ;; with a human check, a build waits for its token
                           :|data-attr:disabled| (format nil "$busy || $sending || !$prompt.trim()~@[ || !$turnstile~]"
                                                         *turnstile-site-key*)
                           "Build")
                  (:span :class "pl-busy" :|data-show| "$busy" "the agent is working...")))
      (:p :class "pl-error" :|data-show| "$error" :|data-text| "$error")
      (:div :class "pl-card pl-watch" :|data-show| "!$editable"
            "You are watching this session as it is built.  "
            (:a :href *url-prefix* "Start your own") ".")))

   (classic-url (if (the session)
                    (format nil "~a?session=~a" *url-prefix* (session-id (the session)))
                    *url-prefix*)))

  :objects
  ((status-section
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let ((session (the session)))
                    (with-lhtml-string ()
                      (:div :class "pl-card pl-status"
                            (if (null session)
                                (htm (:span "No session yet: your first build opens one."))
                                (let ((usage (session-usage session)) (pot (pot)))
                                  (htm (:span (fmt "Session ~a" (session-id session)))
                                       (:span (str (if (session-busy? session) "working" "ready")))
                                       (:span (fmt "~a of ~a prompts" (prompts-used session) *max-prompts-per-session*))
                                       (:span (fmt "tokens ~:d in, ~:d out" (getf usage :input) (getf usage :output)))
                                       (when pot
                                         (htm (:span (fmt "pot ~:d credits" (max 0 (floor (or (getf pot :credits) 0)))))))
                                       (when (model-defined? session)
                                         (htm (:a :href (viewer-url session) :target "_blank" "tree and inputs")))
                                       (:a :href (the classic-url) "editor and credits")))))))))

   (log-section
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let* ((session (the session))
                         (log (and session (session-log session)))
                         (shown (last log *log-shown*)))
                    (with-lhtml-string ()
                      (:div :class "pl-card pl-log"
                            (when (> (length log) (length shown))
                              (htm (:p :class "pl-entry pl-note"
                                       (fmt "~d earlier entries not shown." (- (length log) (length shown))))))
                            (dolist (entry (reverse shown))
                              (destructuring-bind (time kind text) entry
                                (declare (ignore time))
                                (htm (:div :class (format nil "pl-entry pl-~(~a~)" kind)
                                           (:span :class "pl-kind" (fmt "~(~a~)" kind))
                                           (:span :class "pl-text" (esc text)))))))))))

   (source-section
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let ((session (the session)))
                    (with-lhtml-string ()
                      (:div :class "pl-card pl-source"
                            (:h2 "Model file")
                            (if (and session (model-defined? session))
                                (htm (:pre (esc (or (model-body session) ""))))
                                (htm (:p "No model yet."))))))))

   (viewport :type 'viewport-html-div
             :image-format :svg
             :projection-key :trimetric
             :hidden-lines (if (<= 1 (the leaf-count) *hidden-lines-max-leaves*) :remove :draw)
             :display-list-object-roots (let ((model (the model-object))) (when model (list model)))))

  :functions
  ((set-instantiation-time!
    ()
    (call-next-method)
    ;; ?session=<id>: the sheet shows that session to whoever may see it
    (let* ((id (cdr (assoc "session" (the query-toplevel) :test #'string-equal)))
           (session (and (stringp id) (find-session id))))
      (when (and session (visible-to? session nil))
        (the (set-slot! :session session))
        (the (set-slot! :model-stamp (model-stamp session)))
        (watch-session! self session))))

   (claim
    (signals)
    (let ((session (the session))
          (key (gethash "owner" signals)))
      (when (and session (stringp key) (owner? session key))
        (the (set-slot! :owner-key key))
        (touch session)
        (sheet-send! self (datastar-signals-event "{\"editable\": true}")))))

   (build
    (signals)
    (let* ((address (client-address *datastar-request*))
           (prompt (gethash "prompt" signals))
           (token (let ((token (gethash "turnstile" signals))) (and (stringp token) (plusp (length token)) token))))
      (flet ((refuse! (reason)
               (sheet-send! self (datastar-signals-event
                                  (with-output-to-string (s) (yason:encode (h "error" reason) s))))))
        (cond
          ((and (the session) (not (the owner?)))
           (refuse! "This session is someone else's; start your own to build."))
          (t
           ;; a new visitor's first build opens the session
           (unless (the session)
             (multiple-value-bind (session reason) (open-session! address)
               (if session
                   (progn (the (set-slot! :session session))
                          (the (set-slot! :owner-key (session-owner session)))
                          (watch-session! self session)
                          (sheet-send! self (datastar-script-event (owners-script session))))
                   (refuse! reason))))
           (when (the session)
             (multiple-value-bind (started reason) (begin-prompt! (the session) prompt address token)
               (if started
                   (sheet-send! self (datastar-signals-event "{\"error\": \"\", \"prompt\": \"\"}"))
                   (refuse! reason))
               ;; a Turnstile token is single-use
               (when *turnstile-site-key*
                 (sheet-send! self
                              (datastar-signals-event "{\"turnstile\": \"\"}")
                              (datastar-script-event "if(window.turnstile)turnstile.reset()"))))))))))))


(defun publish-lab-sheet! (&key host)
  "Publish the sheet at <prefix>/sheet, beside the page."
  (gwl::publish-gwl-app (format nil "~a/sheet" *url-prefix*) 'lab-sheet :host host))
