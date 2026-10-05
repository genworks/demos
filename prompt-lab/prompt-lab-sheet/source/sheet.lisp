;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;;;
;;;; The lab's page: one gwl sheet, at <prefix> (and <prefix>/sheet, its
;;;; address before it took the prefix): THE SLUICE, opened on the
;;;; session's model, with the lab's own sections as its tiles.  It
;;;; replaced the classic page (a static page polling the JSON doors under
;;;; <prefix>/api/, which stay for scripts and agents) on 2026-10-02.
;;;;
;;;; Each part of the page is a section: the sheet hears every change to
;;;; its session (*session-change-hooks*: each log entry, each save) and pushes the
;;;; sections that changed over its stream (gwl's datastar-mixin, which
;;;; the sluice carries).  The tree, the inspector with the model's
;;;; inputs, the panes (wireframe, shaded, several of them) and the
;;;; menus are the sluice's; the lab adds the prompt, status, credits,
;;;; downloads and log at the left and the model file under the panes.
;;;;
;;;; Ownership is the page's: the owner key is kept in the browser
;;;; (localStorage prompt-lab-owners, id -> key), so a session opened
;;;; in one tab carries on in another.  A sheet opened
;;;; on ?session=<id> shows it to anyone it is visible to, and the claim
;;;; action makes it the owner's when the browser holds the key.
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
  "When SESSION's model (or web app) was last compiled, or nil when it has
none."
  (and (or (model-defined? session) (app-defined? session))
       (ignore-errors (file-write-date (make-pathname :type "fasl"
                                                      :defaults (session-model-file session))))))

(defun json-boolean (value) (if value "true" "false"))

(defun sheets-hear (session)
  "A change to SESSION: every sheet showing it takes a new revision (its
log, status and source sections depend on it), a new model stamp when the
model was compiled again (its viewport depends on that), the model file
for its editor, and the busy signal; then its stream sends what went
stale.  A session gone private closes on every sheet but its owner's."
  (dolist (sheet (session-sheets session))
    (cond
      ((and (session-private? session) (not (the-object sheet owner?)))
       ;; as the page's state door refuses a watcher: the sheet closes on
       ;; it, its editor emptied, and it hears no more
       (bt:with-lock-held (*session-sheets-lock*)
         (setf (gethash (session-id session) *session-sheets*)
               (remove sheet (gethash (session-id session) *session-sheets*))))
       (the-object sheet (set-slot! :session nil))
       (the-object sheet (set-slot! :private-id (session-id session)))
       (sheet-send! sheet (datastar-script-event "plSetSource('')")
                    (datastar-signals-event "{\"editable\": false}"))
       (sheet-changed! sheet))
      (t
       (the-object sheet (set-slot! :revision (1+ (the-object sheet revision))))
       (let ((stamp (model-stamp session)))
         (unless (eql stamp (the-object sheet model-stamp))
           (the-object sheet (set-slot! :model-stamp stamp))
           ;; the session's first model: the sluice opens on it.  A
           ;; rebuild after that redefines the model's class, and the
           ;; sluice redraws itself (its refresh-redefined!)
           (the-object sheet show-model!)))
       ;; the editor is no section: a new version of the file (as it is,
       ;; compiled or not) goes to it as a call, which an edit in
       ;; progress does not take
       (let ((body (or (ignore-errors (model-body session)) "")))
         (unless (equal body (the-object sheet shown-source))
           (the-object sheet (set-slot! :shown-source body))
           (sheet-send! sheet (datastar-script-event (format nil "plSetSource(~a)" (js-string-literal body))))))
       (sheet-send! sheet (datastar-signals-event
                           (format nil "{\"busy\": ~a, \"built\": ~a~@[, \"story\": ~a~]}" (json-boolean (session-busy? session))
                                   ;; something to deploy (the Monetize tile)
                                   (json-boolean (and (not (session-replay? session))
                                                      (or (model-defined? session) (app-defined? session))))
                                   ;; and whether it charges for anything yet, which
                                   ;; opens Monetize: asked once a build has settled
                                   ;; (it builds the thing, once per compile)
                                   (unless (or (session-busy? session) (session-replay? session))
                                     (json-boolean (ignore-errors (monetizable? session)))))))
       (sheet-changed! sheet)))))

(pushnew 'sheets-hear *session-change-hooks*)

;;
;; The reply as it is written (agent.lisp, *session-text-hooks*): every
;; sheet showing the session gets the text so far as the `live' signal,
;; which the log shows as a line of its own until the block is done.  At
;; most five times a second per session; the end of a block always goes.
;;
(defvar *live-sent* (make-hash-table :test #'equal)
  "Session id -> when its live text was last sent, internal real time.")

(defvar *live-sent-lock* (bt:make-lock "prompt-lab live text"))

(defparameter *live-interval* (floor internal-time-units-per-second 5))

(defun sheets-hear-text (session text)
  (let ((now (get-internal-real-time))
        (id (session-id session)))
    (when (or (null text)
              (bt:with-lock-held (*live-sent-lock*)
                (let ((last (gethash id *live-sent*)))
                  (when (or (null last) (>= (- now last) *live-interval*))
                    (setf (gethash id *live-sent*) now)
                    t))))
      (when (null text)
        (bt:with-lock-held (*live-sent-lock*) (remhash id *live-sent*)))
      (let ((event (datastar-signals-event
                    (with-output-to-string (s) (yason:encode (h "live" (if text (coerce text 'simple-string) "")) s)))))
        (dolist (sheet (session-sheets session))
          (sheet-send! sheet event))))))

(pushnew 'sheets-hear-text *session-text-hooks*)


;;
;; What the browser runs: the owner's key kept where the page keeps it.
;;

(defun owners-script (session)
  "JavaScript: SESSION's owner key into the browser's keys, and the address
bar to the sheet's address for the session, so a reload finds it again."
  (format nil "(function(){var o={};try{o=JSON.parse(localStorage.getItem('prompt-lab-owners')||'{}')||{}}catch(e){}
o[~a]=~a;try{localStorage.setItem('prompt-lab-owners',JSON.stringify(o))}catch(e){}
history.replaceState(null,'',~a)})();~a"
          (js-string-literal (session-id session))
          (js-string-literal (session-owner session))
          (js-string-literal (format nil "~a?session=~a" *url-prefix* (session-id session)))
          (keep-last-script (session-id session))))

(defun keep-last-script (session-id)
  "JavaScript: SESSION-ID as the session this browser was last in
(localStorage prompt-lab-last), for an
installed lab opened from its icon (?app=1)."
  (format nil "(function(){var l={};try{l=JSON.parse(localStorage.getItem('prompt-lab-last')||'{}')||{}}catch(e){}
if(l.session!==~a){l={session:~a,log:[]}}try{localStorage.setItem('prompt-lab-last',JSON.stringify(l))}catch(e){}})()"
          (js-string-literal session-id) (js-string-literal session-id)))

(defun owner-signal-expression (session-id)
  "A JavaScript expression: this browser's key for SESSION-ID, or ''."
  (format nil "(function(){try{return (JSON.parse(localStorage.getItem('prompt-lab-owners')||'{}')||{})[~a]||''}catch(e){return ''}})()"
          (js-string-literal session-id)))

(defparameter *log-shown* 200
  "Integer. The most log entries the sheet shows, the newest.")

(defun dollars (cents)
  (if (zerop (mod cents 100))
      (format nil "$~d" (floor cents 100))
      (format nil "$~,2f" (/ cents 100))))

(defun offers (amounts sold)
  "List of (cents credits): what each amount on sale buys; a cent a credit
from a gate that names no credits."
  (let ((amounts (coerce (or amounts #()) 'list))
        (sold (coerce (or sold #()) 'list)))
    (loop for cents in amounts
          for i from 0
          when (integerp cents)
            collect (list cents (let ((c (nth i sold))) (if (realp c) (round c) cents))))))

(defun credits-html (sheet pot spend)
  "String of HTML: the credits pane's body for SHEET, from the gate's POT
(a plist, or nil) and the owner's SPEND (spend-state, or nil)."
  (flet ((buttons (offers label)
           (with-output-to-string (out)
             (when (and offers (the-object sheet editable?))
               (format out "<div class=\"pl-topup\"><span>~a</span>" label)
               (dolist (offer offers)
                 (destructuring-bind (cents credits) offer
                   ;; every offer is always on offer: a pot too full for
                   ;; one says so when it is asked for (the topup action)
                   (format out "<button type=\"button\" class=\"pl-buy\" data-indicator:paying=\"\" data-attr:disabled=\"~a\" data-on:click=\"~a\" title=\"Pay by card\">~:d ~a for ~a</button>"
                           (format nil "$paying~@[ || !$turnstile~]" *turnstile-site-key*)
                           (escape-string-minimal-plus-quotes
                            (format nil "$amount = ~d; ~a" cents
                                    (the-object sheet (datastar-action :topup :options "{filterSignals: {include: /^(amount|turnstile|owner|wallet)$/}}"))))
                           credits (units credits) (dollars cents))))
               (format out "</div>")))))
    (cond
      (pot
       (let* ((held (max 0 (floor (or (getf pot :credits) 0))))
              (most (or (getf pot :max) 0))
              (offers (and (getf pot :topup?) (offers (getf pot :amounts) (getf pot :credits-sold)))))
         (with-output-to-string (out)
           (format out "<div class=\"pl-figure~:[~; pl-out~]\"><b>~:d</b> ~a left in the community pot</div>"
                   (zerop held) held (units held))
           (when (plusp most)
             (format out "<div class=\"pl-meter\"><span style=\"width:~,1f%\"></span></div>"
                     (* 100 (/ (min held most) most))))
           ;; what the pot can hold is not said here: a top-up that
           ;; would overfill it is told so when it is tried
           (let ((drawn (let ((n (and spend (gethash "credits_used" spend)))) (and n (plusp n) n)))
                 (added (let ((n (and spend (gethash "contributed" spend)))) (and n (plusp n) n))))
             (when (or drawn added)
               (format out "<p class=\"pl-line\">~@[this session has drawn ~:d~]~:[~; &middot; ~]~@[you have added ~:d~]</p>"
                       drawn (and drawn added) added)))
           (when (zerop held)
             (format out "<p class=\"pl-begging\">The pot is empty: nothing builds until someone tops it up.</p>"))
           (write-string (buttons offers "Add to the pot:") out))))
      (spend
       (let* ((used (or (gethash "credits_used" spend) 0))
              (free (or (gethash "credits_free" spend) 0))
              (balance (gethash "credits_balance" spend))
              (offers (and (eq (gethash "topup" spend) t)
                           (offers (gethash "topup_amounts" spend) (gethash "topup_credits" spend)))))
         (with-output-to-string (out)
           (format out "<div class=\"pl-figure\"><b>~:d</b> ~a left</div>"
                   (+ (max 0 (- free used)) (or balance 0)) (units))
           (format out "<p class=\"pl-line\">this session: ~:d of ~:d free ~a~@[ &middot; balance ~:d~]</p>"
                   used free (units) balance)
           (write-string (buttons offers "Buy:") out))))
      (t (format nil "<p class=\"pl-line\">~a show here once you build.</p>" (units-title))))))

(defun publishable-key (session)
  "The Stripe publishable key the gate reports -- with the pot, else with
SESSION's balance -- or nil."
  (let ((key (or (getf (pot) :key)
                 (and session (ignore-errors (gethash "publishable_key" (spend-state session)))))))
    (and (stringp key) (plusp (length key)) key)))

(defun listing-entry (summary archive?)
  "String of HTML: one session of a listing (a summary from browse.lisp)."
  (let* ((id (gethash "id" summary))
         (engine (gethash "engine" summary))
         (here? (equal engine (engine-name)))
         (live? (if archive? (live? id) t))
         ;; a live session to its sheet, an archived one to the sheet's
         ;; archive view -- here, or on the other engine's lab
         (href (cond ((and here? live?) (format nil "~a?session=~a" *url-prefix* id))
                     (here? (format nil "~a?archive=~a" *url-prefix* id))
                     ((car *sibling-lab*) (format nil "~a?~:[archive~;session~]=~a"
                                                  (car *sibling-lab*) live? id))
                     (t nil)))
         (thumb (and archive? (gethash "thumb" summary)))
         (seconds (or (gethash "last_used" summary) (gethash "created" summary))))
    (with-output-to-string (out)
      (format out "<a class=\"pl-entry-card\"~@[ href=\"~a\"~]>" (and href (escape-string-minimal-plus-quotes href)))
      (when thumb
        (format out "<img class=\"pl-thumb\" alt=\"\" loading=\"lazy\" src=\"~a?id=~a&amp;v=~a\">"
                (door-path "thumb") id thumb))
      (format out "<span class=\"pl-entry-title\">~a</span>"
              (escape-string-minimal-plus-quotes (or (gethash "title" summary) "(no prompt yet)")))
      (format out "<span class=\"pl-line\">~@[~a &middot; ~]~d prompt~:p~:[~; &middot; a model~]~:[~; &middot; working~]~:[~; &middot; live~]~@[ &middot; ~a~]</span>"
              (and seconds (multiple-value-bind (s m h d mo y) (decode-universal-time (+ seconds (encode-universal-time 0 0 0 1 1 1970 0)) 0)
                          (declare (ignore s))
                          (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d UTC" y mo d h m)))
              (gethash "prompts" summary)
              (eq (gethash "model" summary) t)
              (eq (gethash "busy" summary) t)
              (and archive? live?)
              (unless here? engine))
      (format out "</a>"))))

;;
;; The sheet.
;;

(defparameter *sheet-css*
  ".pl-head{display:flex;gap:1rem;align-items:center;padding:.3rem 1rem;background:var(--pl-label-bg,#26262b);color:var(--pl-label-ink,#fff);flex:none}
.pl-head h1{font-size:1rem;margin:0;font-family:var(--pl-font-label);font-weight:var(--pl-label-weight,700);text-transform:var(--pl-label-case);letter-spacing:var(--pl-label-tracking)}
.pl-head a,.pl-engine{color:var(--pl-label-ink,#fff);opacity:.85}
.pl-head nav{margin-left:auto;display:flex;gap:.8rem;align-items:center;font-size:.9em}
.pl-card{background:var(--pl-panel,#fff);color:var(--pl-ink,#111);border:var(--pl-rule,1px) solid var(--pl-line,#ccc);border-radius:var(--pl-radius,4px);padding:.6rem}
.pl-card h2{font-size:.85rem;margin:0 0 .4rem;font-family:var(--pl-font-label);font-weight:var(--pl-label-weight,700);text-transform:var(--pl-label-case);letter-spacing:var(--pl-label-tracking)}
.sluice-tile .pl-card{border:0;border-radius:0}
.pl-app-host{display:contents}
#pl-app-stage .pl-project-stage{flex:1 1 auto;overflow:auto;padding:1rem 1.5rem;color:#111;font-size:.9rem}
#pl-app-stage .pl-project-stage pre{white-space:pre-wrap;background:#f6f6f4;padding:.6rem;border:1px solid #ddd}
#pl-app-stage .pl-project-stage ul{padding-left:1.2rem;columns:2}
.pl-dim{color:var(--pl-ink-dimmer,#888)}
#sluice-panes:has(#pl-app-stage:not([style*=none])){display:flex!important}
#sluice-panes:has(#pl-app-stage:not([style*=none]))>div:not(.pl-app-host),#sluice-panes:has(#pl-app-stage:not([style*=none]))>.sluice-split{display:none!important}
#pl-app-stage .pl-app-empty{flex:1 1 auto;display:flex;flex-direction:column;align-items:center;justify-content:center;padding:2rem;color:#555;text-align:center;border:2px dashed #ddd;margin:1rem}
#pl-app-stage .pl-app-empty h2{font-size:1.1rem;margin:0 0 .5rem;color:#111}
#pl-app-stage{flex:1 1 auto;display:flex;min-width:0;min-height:0;background:#fff}
#pl-app-stage iframe{flex:1 1 auto;width:100%;height:100%;border:0;background:#fff}
#pl-prompt{width:100%;box-sizing:border-box;font:inherit;padding:.4rem;background:var(--pl-panel,#fff);color:var(--pl-ink,#111);border:var(--pl-rule,1px) solid var(--pl-line-soft,#ddd);border-radius:var(--pl-radius,4px)}
.pl-row{display:flex;gap:.8rem;align-items:center;margin-top:.5rem}
.pl-build{padding:.35rem 1.2rem;font:inherit;background:var(--pl-accent,#366fc5);color:var(--pl-accent-ink,#fff);border:0;border-radius:var(--pl-radius,4px);cursor:pointer;font-weight:var(--pl-label-weight,700);text-transform:var(--pl-label-case)}
.pl-build:disabled{opacity:.4;cursor:default}.pl-busy{color:var(--pl-status-busy,#a60)}
.pl-busy::before{content:'';display:inline-block;width:.75em;height:.75em;margin-right:.45em;vertical-align:-.08em;border:2px solid currentColor;border-right-color:transparent;border-radius:50%;animation:pl-spin .8s linear infinite}
@keyframes pl-spin{to{transform:rotate(360deg)}}
.pl-error{color:var(--pl-status-fail,#b00);margin:.4rem 0 0}.pl-notice{color:var(--pl-status-pass,#060);margin:.4rem 0 0}
.pl-status{display:flex;flex-wrap:wrap;gap:.3rem 1rem;color:var(--pl-ink-dim,#555);font-size:.9em}
.pl-entry{display:grid;grid-template-columns:5rem 1fr;gap:.5rem;padding:.2rem 0;border-bottom:var(--pl-rule,1px) solid var(--pl-line-soft,#ddd)}
.pl-kind{color:var(--pl-ink-dimmer,#888);font-size:.8rem}.pl-text{white-space:pre-wrap;word-break:break-word;font-size:.9em}
.pl-prompt .pl-text{font-weight:600}.pl-live .pl-text{color:var(--pl-ink-dim,#555);font-style:italic}.pl-done .pl-text{color:var(--pl-status-pass,#060)}
.pl-stopped .pl-text,.pl-tool-error .pl-text{color:var(--pl-status-fail,#b00)}
.pl-tool .pl-text{color:var(--pl-ink-dim,#555);font-family:var(--pl-font-mono,monospace);font-size:.8rem}
.pl-source-head{display:flex;gap:.6rem;align-items:baseline;margin-bottom:.4rem}.pl-source-head h2{margin:0}
.pl-source-state{color:var(--pl-ink-dimmer,#888);font-size:.8rem}.pl-source-state.pl-warn{color:var(--pl-status-fail,#b00)}
.pl-source-head .pl-save{margin-left:auto}
#pl-source{width:100%;box-sizing:border-box;min-height:12rem;font:12px/1.4 var(--pl-font-mono,monospace)}
.pl-source .cm-editor{font-size:12px;font-family:var(--pl-font-mono,monospace);background:var(--pl-panel,#fff);color:var(--pl-ink,#111);border:var(--pl-rule,1px) solid var(--pl-line-soft,#ddd)}
.pl-source .cm-editor.cm-focused{outline:none;border-color:var(--pl-focus,#36c)}
.pl-source .cm-gutters{background:var(--pl-panel-alt,#f3f3f3);color:var(--pl-ink-dimmer,#888);border-right:var(--pl-rule,1px) solid var(--pl-line-soft,#ddd)}
.pl-source .cm-cursor{border-left-color:var(--pl-ink,#111)}
.code-editor .tok-comment{color:var(--pl-code-comment,var(--pl-ink-dimmer));font-style:italic}
.code-editor .tok-keyword{color:#6a1b9a;color:var(--pl-code-keyword,color-mix(in srgb,#8e24aa 45%,var(--pl-ink)));font-weight:700}
.code-editor .tok-heading{color:#00695c;color:var(--pl-code-section,color-mix(in srgb,#00897b 50%,var(--pl-ink)));font-weight:700}
.code-editor .tok-atom,.code-editor .tok-meta{color:#1550a8;color:var(--pl-code-atom,color-mix(in srgb,#1e6fd9 50%,var(--pl-ink)))}
.code-editor .tok-number{color:#9a4a00;color:var(--pl-code-number,color-mix(in srgb,#d2691e 50%,var(--pl-ink)))}
.code-editor .tok-string,.code-editor .tok-string2{color:#2a6e2f;color:var(--pl-code-string,color-mix(in srgb,#2e9d38 50%,var(--pl-ink)))}
.code-editor .tok-punctuation{color:var(--pl-code-paren,var(--pl-ink-dimmer))}.code-editor .tok-invalid{color:var(--pl-status-fail)}
.pl-buy,.pl-download,.pl-source-head button{font:inherit;font-size:.85em;padding:.2rem .6rem;border:var(--pl-rule,1px) solid var(--pl-line,#ccc);border-radius:var(--pl-radius,4px);background:var(--pl-panel,#fff);color:var(--pl-link,#1550a8);cursor:pointer}
.pl-buy:disabled{opacity:.4;cursor:default}
.pl-downloads .pl-download{margin:0 .3rem .3rem 0}
.pl-figure{font-size:.9em;color:var(--pl-ink-dim,#555)}.pl-figure b{font-size:1.8rem;color:var(--pl-ink,#111);margin-right:.3rem;font-variant-numeric:tabular-nums}
.pl-figure.pl-out b{color:var(--pl-status-fail,#b00)}
.pl-meter{height:.45rem;background:var(--pl-panel-alt,#eee);border:var(--pl-rule,1px) solid var(--pl-line-soft,#ddd);border-radius:var(--pl-radius-pill,9px);overflow:hidden;margin:.3rem 0}
.pl-meter span{display:block;height:100%;background:var(--pl-accent,#366fc5)}
.pl-line{color:var(--pl-ink-dim,#555);font-size:.9em;margin:.3rem 0}.pl-begging{color:var(--pl-status-fail,#b00);font-weight:600;margin:.3rem 0}
.pl-doc{flex:1;min-width:0;overflow:hidden;white-space:nowrap;text-overflow:ellipsis;opacity:.75;font-size:.85em}
.pl-toast{position:fixed;left:50%;bottom:1.2rem;transform:translateX(-50%);z-index:60;max-width:min(36rem,92vw);padding:.55rem .9rem;border-radius:var(--pl-radius,4px);background:var(--pl-label-bg,#26262b);color:var(--pl-label-ink,#fff);box-shadow:0 4px 18px rgba(0,0,0,.25);font-size:.95em}
.pl-toast-error{background:var(--pl-status-fail,#b00018);color:#fff}
.pl-pay{margin-top:.6rem}.pl-card-line{padding:.6rem;border:var(--pl-rule,1px) solid var(--pl-line,#ccc);border-radius:var(--pl-radius,4px);background:var(--pl-panel,#fff);margin:.3rem 0}
.pl-topup{display:flex;flex-wrap:wrap;gap:.4rem;align-items:center;margin-top:.4rem;font-size:.9em}
.pl-private{display:inline-flex;gap:.3rem;align-items:center;cursor:pointer}
.pl-private input{accent-color:var(--pl-accent,#366fc5)}
.pl-upload{margin-top:.6rem;padding-top:.5rem;border-top:var(--pl-rule,1px) solid var(--pl-line-soft,#ddd);font-size:.9em}
.pl-rights{display:block;color:var(--pl-ink-dim,#555)}.pl-rights input{accent-color:var(--pl-accent,#366fc5)}
#pl-file{position:absolute;width:1px;height:1px;opacity:0;overflow:hidden}
.pl-file-link{display:inline-flex;gap:.4rem;align-items:center;cursor:pointer;color:var(--pl-link,#1550a8);text-decoration:underline;text-underline-offset:2px}
.pl-file-link:hover{text-decoration-thickness:2px}#pl-file:focus-visible+.pl-file-link,.pl-file-link:focus-within{outline:2px solid var(--pl-focus,#36c);outline-offset:2px}
.pl-fee b{color:var(--pl-ink,#111)}
.pl-kinds{margin:0 0 .45rem;gap:.7rem;flex-wrap:wrap;font-size:.9em}.pl-kinds .pl-line{margin:0}
.pl-pick{display:inline-flex;gap:.3rem;align-items:center;cursor:pointer}.pl-pick input{accent-color:var(--pl-accent,#366fc5);margin:0}
.pl-open-app{font-weight:600;color:var(--pl-link,#1550a8)}
.pl-project a{color:var(--pl-link,#1550a8);text-decoration:underline}
.pl-monetize .pl-open-app{font-weight:400;font-size:.85em;overflow-wrap:anywhere}
.pl-deploy{margin-top:.5rem;font-size:.9em}.pl-deploy label{display:block;margin:.45rem 0 0;color:var(--pl-ink-dim,#555)}
.pl-deploy label.pl-pick{display:flex;color:var(--pl-ink,#111)}
.pl-deploy input[type=text],.pl-deploy input[type=email],.pl-deploy input[type=number],.pl-deploy textarea{display:block;width:100%;box-sizing:border-box;font:inherit;padding:.3rem;margin-top:.15rem;background:var(--pl-panel,#fff);color:var(--pl-ink,#111);border:var(--pl-rule,1px) solid var(--pl-line-soft,#ddd);border-radius:var(--pl-radius,4px)}
.pl-list{max-width:60rem;margin:0 auto;padding:1rem}
.pl-list h2{font-family:var(--pl-font-label);font-weight:var(--pl-label-weight);text-transform:var(--pl-label-case);letter-spacing:var(--pl-label-tracking)}
.pl-entry-card{display:grid;grid-template-columns:auto 1fr;grid-template-rows:auto auto;column-gap:.8rem;align-items:center;padding:.6rem .7rem;margin-bottom:.5rem;background:var(--pl-panel);border:var(--pl-rule) solid var(--pl-line);border-radius:var(--pl-radius);color:var(--pl-ink);text-decoration:none}
.pl-entry-card:hover{background:var(--pl-hover-bg);outline:var(--pl-rule) solid var(--pl-hover-line)}
.pl-thumb{grid-row:1/3;width:96px;height:72px;object-fit:contain;background:#fff;border:var(--pl-rule) solid var(--pl-line-soft)}
.pl-entry-title{font-weight:600;overflow-wrap:anywhere;font-size:.95em}
@media (max-width:47.99rem){.pl-head{padding:.3rem .6rem;gap:.5rem;flex-wrap:wrap}.pl-head .pl-engine,.pl-doc{display:none}.pl-head nav{gap:.5rem;font-size:.8em}
 .pl-thumb{width:72px;height:54px}.pl-list{padding:.5rem}}"
  "String. The lab's own cards on the sheet (its tiles), in the skin tokens
(the sluice's tokens.css, SKIN-API.md), with fallbacks for the classic
look, which links no tokens.  The frame, the panes, the tree and the
inspector are the sluice's.")

(defparameter *sheet-editor-script*
  "(function(){
var ed=null,dirty=false,base=null,last=null;
function area(){return document.getElementById('pl-source')}
function state(text,warn){var s=document.getElementById('pl-source-state');if(s){s.textContent=text;s.classList.toggle('pl-warn',!!warn)}}
function mount(){var a=area();if(!a)return;last=a.value;
 if(window.PromptLabEditor&&!ed){try{ed=PromptLabEditor.mount(a,{onEdit:edited})}catch(e){ed=null}}
 if(!ed)a.addEventListener('input',edited);
 window.plSourceEditor=ed}
function edited(){if(!dirty){dirty=true;base=last}state(last!==base?'the model changed since your edit began; Save replaces it':'edited',last!==base)}
window.plSourceText=function(){return ed?ed.value:(area()?area().value:'')};
window.plSetSource=function(text){last=text;if(!dirty){if(ed)ed.value=text;else if(area())area().value=text;state('')}
 else if(text!==base)state('the model changed since your edit began; Save replaces it',true)};
window.plConfirmSave=function(){return !(dirty&&last!==base)||confirm('The model file changed since your edit began (the agent wrote a new version). Save your edit over it?')};
window.plSaved=function(ok,text){if(ok){dirty=false;base=null}state(ok?'saved and loaded':'saved; see the log',!ok)};
window.plLock=function(flag){if(ed)ed.setReadOnly(!!flag);else if(area())area().readOnly=!!flag};
window.plFold=function(all){if(ed){if(all)ed.foldAll();else ed.unfoldAll()}};
window.plDownload=function(url,owner){var n=document.getElementById('pl-download-note');function say(t,bad){if(n)n.textContent=t||'';if(bad&&window.plToast)plToast(t,'error')}
 say('Making the file...');
 fetch(url,{headers:owner?{'X-Prompt-Lab-Owner':owner}:{}}).then(function(r){
  if(!r.ok)return r.json().then(function(j){say(j.error||('The file could not be made ('+r.status+').'),true)},function(){say('The file could not be made ('+r.status+').',true)});
  var cd=r.headers.get('Content-Disposition')||'',m=/filename=\"?([^\";]+)/.exec(cd),left=r.headers.get('X-Prompt-Lab-Note');
  return r.blob().then(function(b){var a=document.createElement('a');a.href=URL.createObjectURL(b);a.download=m?m[1]:'model';
   document.body.appendChild(a);a.click();setTimeout(function(){URL.revokeObjectURL(a.href);a.remove()},1000);say(left||'')})
 }).catch(function(e){say('The file could not be fetched: '+e,true)})};
if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',mount);else mount();
})();"
  "String. The model file's editor on the sheet: the lab's CodeMirror bundle
(static/editor.js) mounted over a textarea that no section contains, so
nothing the stream sends redraws it.  New versions come as plSetSource
calls; an edit in progress keeps its text and says the file changed.")




(defparameter *sheet-ui-script*
  "(function(){
// the web app's stage (app-stage): its section moves into the pane grid,
// where the stream still finds it by its id
function plStageApp(){var h=document.querySelector('.pl-app-host'),p=document.getElementById('sluice-panes');
 if(h&&p&&h.parentNode!==p)p.appendChild(h)}
if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',plStageApp);else plStageApp();
// a toast: a line at the foot of the screen for a few seconds, wherever
// the page stands (a phone's other tab included)
var timer=null;
window.plToast=function(text,kind){if(!text)return;var t=document.getElementById('pl-toast');if(!t)return;
 t.textContent=text;t.className='pl-toast pl-toast-'+(kind||'note');t.style.display='block';
 if(timer)clearTimeout(timer);timer=setTimeout(function(){t.style.display='none'},kind==='error'?7000:4500)};
// the documentation line: what the thing under the pointer is or does,
// from its data-doc or its title (the sluice's controls carry titles)
document.addEventListener('mouseover',function(e){var d=document.getElementById('pl-doc');if(!d||!e.target.closest)return;
 var el=e.target.closest('[data-doc],[title]');d.textContent=el?(el.getAttribute('data-doc')||el.getAttribute('title')||''):''});
// an upload: the chosen file read here and sent, base64, through the
// sheet's upload action; what the lab answers comes as a notice or an error
window.plUpload=function(el,owner,wallet,token){var f=el.files&&el.files[0],n=document.getElementById('pl-upload-note'),r=document.getElementById('pl-rights');
 if(n&&!n.hint)n.hint=n.textContent;
 function say(t){if(n)n.textContent=t||n.hint||''}
 if(!f)return;
 if(r&&r.offsetParent!==null&&!r.checked){say('Tick the box first: the file will be shared with this session.');el.value='';return}
 say('Uploading '+f.name+'...');var fr=new FileReader();
 fr.onload=function(){var s=String(fr.result),d=s.slice(s.indexOf(',')+1);
  plAction('upload',{name:f.name,data:d,rights:!!(r&&r.checked),owner:owner||'',wallet:wallet||'',turnstile:token||'',closedPick:!!(document.getElementById('pl-closed')||{}).checked})
   .then(function(x){say(x.ok?'':'The upload did not go through ('+x.status+').');el.value=''},function(e){say('The upload did not go through: '+e);el.value=''})};
 fr.onerror=function(){say('The file could not be read.');el.value=''};
 fr.readAsDataURL(f)};
// a request that wants the other lab's engine: the browser goes there,
// carrying the prompt and the reason, which the page there takes up
window.plRoute=function(url,prompt,reason,kind){try{sessionStorage.setItem('prompt-lab-carry',JSON.stringify({prompt:prompt,reason:reason,kind:kind||''}))}catch(e){}
 if(window.plToast)plToast(reason,'note');setTimeout(function(){location.href=url},2500)};
function carried(){var c=null;try{c=JSON.parse(sessionStorage.getItem('prompt-lab-carry')||'null');sessionStorage.removeItem('prompt-lab-carry')}catch(e){}
 if(!c||!/[?&]routed=1/.test(location.search))return;var p=document.getElementById('pl-prompt');
 if(p&&c.prompt){p.value=c.prompt;p.dispatchEvent(new Event('input',{bubbles:true}))}
 var k=c.kind&&document.querySelector('input[name=\"pl-kind\"][value=\"'+c.kind+'\"]');
 if(k){k.checked=true;k.dispatchEvent(new Event('change',{bubbles:true}))}
 if(c.reason)setTimeout(function(){if(window.plToast)plToast(c.reason+'  Your prompt came along: press Build.','note')},1500)}
// (after Datastar has bound the prompt box, which sets it from its signal)
window.addEventListener('load',function(){setTimeout(carried,1200)});
// the files of a session in the sibling lab, brought here: fetched from
// that lab's doors with this browser's key for the session, and sent,
// all in one, through the sheet's adopt action
window.plAdopt=function(from,id,wallet,token){var n=document.getElementById('pl-upload-note'),r=document.getElementById('pl-rights');
 if(n&&!n.hint)n.hint=n.textContent;
 function say(t){if(n)n.textContent=t||n.hint||''}
 if(!r||!r.checked){say('Tick the box first: you declare the right to use and share the files.');return}
 var key='';try{key=(JSON.parse(localStorage.getItem('prompt-lab-owners')||'{}')||{})[id]||''}catch(e){}
 var hs=key?{'X-Prompt-Lab-Owner':key}:{};
 function b64(blob){return new Promise(function(ok,no){var fr=new FileReader();fr.onload=function(){var s=String(fr.result);ok(s.slice(s.indexOf(',')+1))};fr.onerror=no;fr.readAsDataURL(blob)})}
 say('Fetching the files...');
 fetch(from+'/api/state?session='+encodeURIComponent(id),{headers:hs}).then(function(x){if(!x.ok)throw new Error('that session is not to be had ('+x.status+')');return x.json()})
  .then(function(st){var fs=st.files||[];if(!fs.length)throw new Error('that session has no files');
   return Promise.all(fs.map(function(f){return fetch(f.url,{headers:hs}).then(function(x){if(!x.ok)throw new Error(f.name+' is not to be had');return x.blob()}).then(b64).then(function(d){return {name:f.name,data:d}})}))})
  .then(function(files){say('Bringing '+files.length+' over...');return plAction('adopt',{files:files,rights:true,wallet:wallet||'',turnstile:token||''})})
  .then(function(x){say(x.ok?'':'The files did not come over ('+x.status+').')})
  .catch(function(e){say('The files did not come over: '+(e.message||e))})};
})();"
  "String. The sheet's toast (plToast), its documentation line and the
upload control's reader (plUpload).")

(defparameter *sheet-pay-script*
  "(function(){
var stripe=null,card=null,embedded=null,last=null;
function $(id){return document.getElementById(id)}
function show(id,on){var e=$(id);if(e)e.style.display=on?'':'none'}
function loadStripe(){if(window.Stripe)return Promise.resolve();return new Promise(function(ok,no){var s=document.createElement('script');s.src='https://js.stripe.com/v3/';s.async=true;s.onload=ok;s.onerror=function(){no(new Error('Stripe.js did not load'))};document.head.appendChild(s)})}
function tokenHex(name,fallback){try{var v=getComputedStyle(document.documentElement).getPropertyValue(name).trim();var c=document.createElement('canvas');c.width=c.height=1;var g=c.getContext('2d');g.fillStyle='#010203';g.fillStyle=v;if(!v||g.fillStyle==='#010203')return fallback;g.fillRect(0,0,1,1);var p=g.getImageData(0,0,1,1).data;return '#'+[p[0],p[1],p[2]].map(function(n){return('0'+n.toString(16)).slice(-2)}).join('')}catch(e){return fallback}}
window.plAction=function(fn,signals){return fetch('/gdlAction?iid='+encodeURIComponent(window.plIid)+'&fn='+fn,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(signals||{})})};
window.plClosePayment=function(){if(embedded){try{embedded.destroy()}catch(e){}embedded=null}if(card){try{card.destroy()}catch(e){}card=null}show('pl-pay',false);show('pl-card-pay',false);$('pl-card-error').textContent='';$('pl-checkout').innerHTML='';$('pl-card-line').innerHTML=''};
window.plMoreOptions=function(){if(last)plAction('topup-full',{amount:last.amount})};
window.plOpenPayment=function(r){last=r;plClosePayment();
 try{localStorage.setItem('prompt-lab-wallet',r.wallet)}catch(e){}
 loadStripe().then(function(){stripe=window.Stripe(r.publishable_key);
  show('pl-pay',true);$('pl-card-amount').textContent=r.label||'';
  if(r.flow==='card'){
   card=stripe.elements().create('card',{style:{base:{color:tokenHex('--pl-ink','#101010'),fontFamily:getComputedStyle(document.body).fontFamily||'system-ui, sans-serif',fontSize:'16px','::placeholder':{color:tokenHex('--pl-ink-dimmer','#6f6e68')}},invalid:{color:tokenHex('--pl-status-fail','#b00018')}}});
   show('pl-card-line',true);card.mount('#pl-card-line');
   card.on('change',function(e){$('pl-card-error').textContent=e.error?e.error.message:''});
   var pay=$('pl-card-pay'),label='Pay $'+(r.amount/100).toFixed(r.amount%100?2:0);show('pl-card-pay',true);pay.disabled=false;pay.textContent=label;
   pay.onclick=function(){pay.disabled=true;pay.textContent='Paying...';$('pl-card-error').textContent='';
    stripe.confirmCardPayment(r.client_secret,{payment_method:{card:card}}).then(function(res){
     if(res.error){$('pl-card-error').textContent=res.error.message||'The card was not accepted. Nothing was charged.';pay.disabled=false;pay.textContent=label}
     else if(res.paymentIntent&&res.paymentIntent.status==='succeeded'){var co=r.checkout;plClosePayment();plAction('confirm',{checkout:co,wallet:r.wallet})}
     else{$('pl-card-error').textContent='The payment did not go through. Nothing was charged.';pay.disabled=false;pay.textContent=label}})}}
  else{show('pl-card-line',false);return stripe.initEmbeddedCheckout({clientSecret:r.client_secret}).then(function(co){embedded=co;co.mount('#pl-checkout')})}
 }).catch(function(e){$('pl-card-error').textContent='The payment form could not open: '+e;show('pl-pay',true)})};
})();"
  "String. The payment in the sheet: Stripe's one-line card control and a
Pay button for a gate that made a PaymentIntent, or Stripe's full form in
the page, opened by plOpenPayment from start-payment!.  A card paid here
is confirmed through the confirm action (plAction posts to /gdlAction as
Datastar does).")

(define-object lab-sheet (sluice:assembly)

  :documentation
  (:description "The lab as one page: the sluice, opened on the session's
model, with the lab's own sections as its tiles -- the prompt, the
status, the credits, the downloads and the log in a column at the left,
the model file's editor under the panes.  A phone shows them as four
tabs: Prompt, Model, Parts, Code.  Only the sluice's INPUTS are
overridden here: its other messages are reserved words to a subclass in
another package (the sluice is in *packages-to-lock*); the lab's own
slots and children are its own.")

  :input-slots
  (("The session shown, a struct (session.lisp); nil until a new visitor's
first build opens one."
    session nil :settable)
   ("The key this browser proved it holds (the claim action), or the one a
build minted here."
    owner-key nil :settable)
   ("Integer. Bumped at every change to the session (sheets-hear): what the
log, status, credits and downloads sections are recomputed from."
    revision 0 :settable)
   ("When the model was last compiled (sheets-hear)."
    model-stamp nil :settable)
   ("The model file's text as the editor was last sent it (sheets-hear)."
    shown-source "" :settable)
   ("String or nil. The id of a private session named on the address, which
the sheet shows only once its owner's key is proven (claim)."
    private-id nil :settable)
   ("String or nil. The archived session shown (?archive=<id>), read-only."
    archive-id nil :settable)
   ("Integer or nil. Which saved version of the archived model file the
editor shows (?version=<n>); nil for the last."
    archive-version nil :settable)
   ("String or nil. A private archived session named on the address, shown
once its owner's key is proven (claim)."
    archive-pending nil :settable)
   ("String or nil. An archive id the address named that the archive does
not hold."
    archive-missing nil :settable))

  :computed-slots
  (;; the sluice's inputs
   (title (lab-title))
   (audience :public)
   (tiles (list (list :object (the prompt-tile) :place :left :tab "Prompt")
                ;; a visitor's own project, from GitLab (project.lisp)
                (list :object (the project-tile) :place :left :tab "Prompt")
                (list :object (the app-section) :place :left :tab "Prompt")
                (list :object (the monetize-tile) :place :left :tab "Prompt")
                (list :object (the files-section) :place :left :tab "Prompt")
                (list :object (the status-section) :place :left :tab "Prompt")
                (list :object (the credits-section) :place :left :tab "Prompt")
                (list :object (the downloads-section) :place :left :tab "Prompt")
                (list :object (the log-section) :place :left :tab "Prompt")
                ;; its section moves into the panes (plStageApp), so it
                ;; shows with the Model tab whatever this says; "Code" keeps
                ;; the region under the panes off the phone's Model tab
                (list :object (the app-stage) :place :under-panes :tab "Code")
                (list :object (the editor-tile) :place :under-panes :tab "Code")))
   (tabs (list "Prompt" "Model" "Parts" "Code"))
   (head-html (the lab-head-html))
   (body-html (the lab-body-html))
   ;; on a phone the inspector holds the inputs alone
   (user-mode?-default nil)

   ;; the lab's own Datastar actions (gwl's allowlist for /gdlAction)
   (datastar-actions (list :build :claim :save :topup :topup-full :confirm :privacy :wear-skin :upload :adopt
                           :deploy :undeploy :open-project :push-project :deploy-project))

   ;; ?adopt=<id>&from=<prefix>: the session of the lab at <prefix> (the
   ;; sibling, on this site) whose files this lab is offered (routing.lisp)
   (adopt-id (let ((id (cdr (assoc "adopt" (the query-toplevel) :test #'string-equal))))
               (and (session-id? id) id)))
   ;; ?routed=1: the sibling lab sent this visitor here, having decided
   ;; the request wants this engine; it is not asked again
   (arrived? (equal (cdr (assoc "routed" (the query-toplevel) :test #'string-equal)) "1"))
   (adopt-from (let ((from (cdr (assoc "from" (the query-toplevel) :test #'string-equal))))
                 (and (stringp from) (equal from (car *sibling-lab*)) from)))

   ;; the files the visitor uploaded (uploads.lisp): the archived session's
   ;; from its archive directory, a live one's from its own
   (shown-files (progn
                  (the revision)
                  (cond ((the archive-id)
                         (let ((directory (the archive-directory)))
                           (and directory (directory-files directory))))
                        ((the session) (session-files (the session))))))

   ;; the archived session's record and directory (browse.lisp)
   (archive-directory (and (the archive-id) (archived-directory (the archive-id))))
   (archive-record (let ((directory (the archive-directory)))
                     (and directory (read-record (merge-pathnames "session.json" directory)))))
   (archive-versions (let ((directory (the archive-directory)))
                       (and directory (model-versions directory))))
   ;; the model file as the editor shows it: the chosen version, else the last
   (archive-source (let ((directory (the archive-directory)))
                     (when directory
                       (let* ((versions (the archive-versions))
                              (version (the archive-version))
                              (file (if (and version (<= 1 version (length versions)))
                                        (nth (1- version) versions)
                                        (merge-pathnames "model.lisp" directory))))
                         (file-model-body file)))))

   (owner? (let ((session (the session)))
             (and session (the owner-key) (owner? session (the owner-key)) t)))

   ;; a new visitor builds into a session of their own; a session's
   ;; page is the owner's to build in
   ;; (an archived session never is)
   (editable? (and (null (the archive-id)) (null (the archive-pending)) (null (the archive-missing))
                   (or (and (null (the session)) (null (the private-id))) (the owner?))))

   ;; the session the address names: the one shown, or a private one
   ;; waiting for its owner's key
   (shown-id (or (and (the session) (session-id (the session))) (the private-id)))

   (lab-head-html
    (with-lhtml-string ()
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (when *turnstile-site-key*
        (htm (:script :src "https://challenges.cloudflare.com/turnstile/v0/api.js" :async "async" :defer "defer")
             (:script "window.plTurnstile=function(t){var e=document.getElementById('pl-turnstile');if(e){e.value=t;e.dispatchEvent(new Event('input',{bubbles:true}))}};")))
      ;; the lab as an installable app (app.lisp)
      (:link :rel "manifest" :href (format nil "~a/manifest.webmanifest" *url-prefix*))
      (:meta :name "theme-color" :content (app-color "--pl-label-bg" "#101010"))
      (let ((icon (find-if #'(lambda (i) (search "192" (second i))) *app-icons*)))
        (when (and icon (static-file (first icon)))
          (htm (:link :rel "apple-touch-icon" :href (static-url (first icon))))))
      (:script :defer "defer" :src (static-url "editor.js"))
      (:script (str *sheet-editor-script*))
      (:script (str *sheet-pay-script*))
      (:script (str *sheet-ui-script*))
      (:style (str *sheet-css*))))

   ;; Written with the page, never redrawn: the lab's header, the page's
   ;; signals, the claim, and the phone's turn to the model when a build
   ;; is done.  What changes is in the tiles.
   (lab-body-html
    (with-lhtml-string ()
      (:div :id "pl-sheet" :style "display:contents"
            :|data-signals| (escape-string-minimal-plus-quotes (the initial-signals))
            ;; a browser holding this session's key makes the page its owner's
            (when (and (or (the shown-id) (the archive-pending)) (not (the owner?)))
              (htm (:span :|data-init| (format nil "$owner && ~a" (the (datastar-action :claim))))))
            (:span :|data-effect| "var b=$busy; if(window.plWasBusy && !b && window.sluiceTab && matchMedia('(max-width: 47.99rem)').matches){sluiceTab('model')} window.plWasBusy=b")
            ;; ?browse=live|archive, the lab's old listing addresses:
            ;; the listings are a page of their own
            (let ((browse (cdr (assoc "browse" (the query-toplevel) :test #'string-equal))))
              (when (member browse '("live" "archive") :test #'equal)
                (htm (:script (str (format nil "location.replace(~a);"
                                           (js-string-literal (format nil "~a/sheet-list?browse=~a" *url-prefix* browse))))))))
            ;; opened from the installed app's icon (?app=1) with no session
            ;; named: the session this browser was last in, if it owns it
            (when (and (equal (cdr (assoc "app" (the query-toplevel) :test #'string-equal)) "1")
                       (null (the shown-id)) (null (the archive-id)) (null (the archive-pending)))
              (htm (:script (str (format nil "(function(){try{var l=JSON.parse(localStorage.getItem('prompt-lab-last')||'null');var o=JSON.parse(localStorage.getItem('prompt-lab-owners')||'{}')||{};
if(l&&l.session&&o[l.session])location.replace(~a+encodeURIComponent(l.session))}catch(e){}})();"
                                         (js-string-literal (format nil "~a?session=" *url-prefix*)))))))
            ;; the skin this browser chose (localStorage prompt-lab-skin): worn when it is not the one
            ;; showing, and kept when Page > Skin picks another
            (:script (str (format nil "window.plIid=~a;" (js-string-literal (the instance-id)))))
            (:script "window.sluiceSkinChosen=function(n){try{localStorage.setItem('prompt-lab-skin',n)}catch(e){}};")
            (:span :|data-init|
                   ;; cl-who writes attribute values raw, between quotes
                   (escape-string-minimal-plus-quotes
                    (format nil "$skinPref = (function(){try{return (localStorage.getItem('prompt-lab-skin')||'').toLowerCase()}catch(e){return ''}})(); $skinPref && $skinPref !== ~a && ~a"
                            (js-string-literal (sluice:skin-name (the skin)))
                            (the (datastar-action :wear-skin :options "{filterSignals: {include: /^skinPref$/}}")))))
            ;; errors and notices as a toast too, seen from any tab
            (:div :id "pl-toast" :class "pl-toast" :style "display:none" :role "status")
            (:span :|data-effect| "$error && window.plToast && plToast($error, 'error')")
            (:span :|data-effect| "$notice && window.plToast && plToast($notice, 'note')")
            (:header :class "pl-head"
                     (:h1 (esc (lab-title)))
                     (:span :class "pl-engine" (esc (engine-label)))
                     ;; the documentation line (*sheet-ui-script*)
                     (:span :id "pl-doc" :class "pl-doc")
                     (:nav (str (browse-links))
                           (when *sibling-lab*
                             (htm (:a :href (car *sibling-lab*) (esc (cdr *sibling-lab*))))))))))

   ;; back from Stripe the address carries the checkout and the wallet;
   ;; the wallet otherwise comes from where the page keeps it
   (query-checkout (let ((c (cdr (assoc "checkout" (the query-toplevel) :test #'string-equal)))) (and (stringp c) c)))
   (query-wallet (let ((w (cdr (assoc "wallet" (the query-toplevel) :test #'string-equal)))) (and (wallet-id? w) w)))
   (cancelled? (equal (cdr (assoc "topup" (the query-toplevel) :test #'string-equal)) "cancelled"))

   (initial-signals
    (format nil "{gitproject: '', pushurl: ~a, hosturl: ~a, story: ~a, paidUp: ~a, built: ~a, deployed: ~a, opened: ~a, closed: ~a, closedPick: false, monetize: false, dname: '', dtitle: '', dblurb: '', dpayee: '', dpot: 0, kind: ~a, archived: ~a, live: '', skinPref: '', private: false, prompt: '', turnstile: '', source: '', amount: 0, error: '', notice: ~a, sending: false, saving: false, paying: false, busy: ~a, editable: ~a, owner: ~a, checkout: ~a, wallet: ~a}"
            ;; the gate's page for the session's last staged push (project.lisp)
            (js-string-literal (or (and (the session) (session-push-url (the session))) ""))
            ;; and of its last staged hosting (hosting.lisp)
            (js-string-literal (or (and (the session) (car (gethash (session-id (the session)) *session-hostings*))) ""))
            ;; whether what the session built charges for anything yet: the
            ;; Monetize button is greyed until it does (deploy.lisp)
            (json-boolean (let ((session (the session)))
                            (and session (not (session-replay? session)) (not (session-busy? session))
                                 (ignore-errors (monetizable? session)))))
            ;; a session that has topped up is not public unless it says
            ;; so: its uploads want no acknowledgement
            (json-boolean (and (the session) (ignore-errors (paid? (the session)))))
            ;; Monetize (deploy.lisp): whether there is something to deploy,
            ;; and where this session is deployed already
            (json-boolean (let ((session (the session)))
                            (and session (not (session-replay? session))
                                 (or (model-defined? session) (app-defined? session)))))
            (js-string-literal (let ((record (and (the session) *deployments?*
                                                  (ignore-errors (session-deployment (the session))))))
                                 (if record (deployment-url (gethash "name" record)) "")))
            ;; whether there is a session yet, and whether it was opened
            ;; closed-source: that choice is made before the first build
            (json-boolean (the shown-id))
            (json-boolean (and (the session) (session-closed? (the session))))
            ;; what the next prompt builds (kinds.lisp): the session's kind,
            ;; where this lab still offers it, else the lab's default
            (js-string-literal (kind-name (or (and (the session) (parse-kind (session-kind (the session))))
                                              (default-kind))))
            (json-boolean (or (the archive-id) (the archive-pending)))
            (if (the cancelled?) "'The payment was cancelled; nothing was charged.'" "''")
            (json-boolean (and (the session) (session-busy? (the session))))
            ;; a browser holding the session's key shows the owner's page
            ;; from the start (the claim confirms it; every action checks
            ;; the key again), rather than a watcher's for the first second
            (if (and (the shown-id) (not (the editable?)))
                (format nil "(~a !== '')" (owner-signal-expression (the shown-id)))
                (json-boolean (the editable?)))
            (let ((id (or (the shown-id) (the archive-pending))))
              (if id (owner-signal-expression id) "''"))
            (js-string-literal (or (the query-checkout) ""))
            (if (the query-wallet)
                (js-string-literal (the query-wallet))
                "(function(){try{return localStorage.getItem('prompt-lab-wallet')||''}catch(e){return ''}})()"))))

  :objects
  (;; The prompt, as a tile that never goes stale (it reads nothing that
   ;; changes): what is typed stays.  What it shows changes through
   ;; signals.
   (prompt-tile
    :type 'base-html-div
    :inner-html (with-lhtml-string ()
                  (:div :class "pl-card"
                        (:div :|data-show| "$editable"
                              ;; the switch: what the prompt builds (kinds.lisp),
                              ;; shown where the lab offers more than one kind
                              (when (rest *kinds*)
                                (htm (:div :class "pl-row pl-kinds" :role "radiogroup"
                                           :data-doc "What the agent builds from your prompt: a geometry model shown here, or a web app with a page of its own"
                                           (:span :class "pl-line" "Build a")
                                           (dolist (kind *kinds*)
                                             (htm (:label :class "pl-pick"
                                                          (:input :type "radio" :name "pl-kind" :value (kind-name kind)
                                                                  :|data-bind:kind| "")
                                                          " " (esc (string-downcase (kind-label kind)))))))))
                              ;; closed source is chosen here, before the session
                              ;; opens with the first build or upload (deploy.lisp)
                              (when (and *closed-source?* *deployments?*)
                                (htm (:label :class "pl-pick pl-closed" :style "display:none" :|data-show| "!$opened"
                                             :data-doc "Closed source: nobody else sees the session, and what you deploy from it does not serve its source, under a closed-source licence for the Gendl it runs on, which comes with the higher fee.  If you do not deploy it, its source becomes public when the session ends."
                                             ;; closed-pick: an attribute's name reaches Datastar
                                             ;; in lower case, and it reads the hyphen as closedPick
                                             (:input :type "checkbox" :id "pl-closed" :|data-bind:closed-pick| "")
                                             " Closed source")
                                     ;; the hosting fee follows the box as it is ticked
                                     (:p :class "pl-line pl-fee" :style "display:none" :|data-show| "!$opened"
                                         "Monetization fee: "
                                         (:b :|data-text| (format nil "$closedPick ? '~d%' : '~d%'"
                                                                  (house-fee-percent t) (house-fee-percent nil)))
                                         (:span :|data-show| "$closedPick"
                                                " -- nobody else sees the session; its source becomes public if you never deploy it.")
                                         (:span :|data-show| "!$closedPick" " -- open source, public as you build."))
                                     (:p :class "pl-line" :style "display:none" :|data-show| "$opened && $closed"
                                         (fmt "Closed source, ~d% monetization fee: deploy it with Monetize, or its source becomes public when the session ends."
                                              (house-fee-percent t)))))
                              (:textarea :id "pl-prompt" :rows "4" :|data-bind:prompt| ""
                                         :|data-attr:placeholder| "$kind == 'app' ? 'Describe the web app, e.g. a page that sizes a shelf bracket from its load and shows it' : 'Describe what to build, e.g. a picnic table with two benches'"
                                         :placeholder "Describe what to build, e.g. a picnic table with two benches")
                              (when *turnstile-site-key*
                                (htm (:div :class "cf-turnstile" :data-sitekey *turnstile-site-key* :data-callback "plTurnstile")))
                              (:input :type "hidden" :id "pl-turnstile" :|data-bind:turnstile| "")
                              (:div :class "pl-row"
                                    (:button :class "pl-build"
                                             :|data-on:click| (the (datastar-action :build :options "{filterSignals: {include: /^(prompt|turnstile|owner|wallet|kind|closedPick)$/}}"))
                                             :|data-indicator:sending| ""
                                             ;; with a human check, a build waits for its token
                                             :|data-attr:disabled| (format nil "$busy || $sending || !$prompt.trim()~@[ || !$turnstile~]"
                                                                           *turnstile-site-key*)
                                             "Build")
                                    (:span :class "pl-busy" :|data-show| "$busy" "the agent is working..."))
                              ;; a file for the agent to build from (uploads.lisp):
                              ;; read here, sent through the upload action
                              (when (uploads-offered?)
                                (let ((caps (getf *upload-caps* :free)))
                                  (htm (:div :class "pl-upload"
                                             ;; a file is public with its session: said and
                                             ;; acknowledged, except where the session is not
                                             ;; public -- closed source, or one that has topped up
                                             (:label :class "pl-rights" :|data-show| "!($closed || $closedPick || $paidUp)"
                                                     :title "A file you upload is public with this session, here and in the archive, and you have the right to share it"
                                                     (:input :type "checkbox" :id "pl-rights")
                                                     " I acknowledge this file will be shared")
                                             ;; the files of a session in the sibling lab,
                                             ;; offered here (routing.lisp)
                                             (when (and (the adopt-id) (the adopt-from))
                                               (htm (:div :class "pl-row pl-adopt"
                                                          (:button :type "button" :class "pl-download"
                                                                   :data-doc "Bring the files you uploaded in the other lab into a session here"
                                                                   :|data-on:click|
                                                                   (escape-string-minimal-plus-quotes
                                                                    (format nil "plAdopt(~a, ~a, $wallet, $turnstile)"
                                                                            (js-string-literal (the adopt-from))
                                                                            (js-string-literal (the adopt-id))))
                                                                   "Bring the files over")
                                                          (:span :class "pl-line" "from your session in the other lab: tick the box, then press."))))
                                             ;; the browser's own file button is hidden: its
                                             ;; label is the control, a link with an upload mark
                                             (:div :class "pl-row"
                                                   (:label :class "pl-file-link" :for "pl-file"
                                                           :data-doc "A drawing or other file for the agent to build from: a PDF, an image, or a text file such as DXF"
                                                           (:svg :viewBox "0 0 16 16" :width "16" :height "16" :aria-hidden "true"
                                                                 (:path :d "M8 11V2M4.5 5.5 8 2l3.5 3.5M2.5 10.5v3h11v-3"
                                                                        :fill "none" :stroke "currentColor" :stroke-width "1.6"
                                                                        :stroke-linecap "round" :stroke-linejoin "round"))
                                                           "Upload a drawing or file")
                                                   (:input :type "file" :id "pl-file"
                                                           :accept (format nil ".pdf,.png,.jpg,.jpeg,.gif,.webp~{,.~a~}" *upload-text-types*)
                                                           :|data-on:change| "plUpload(el, $owner, $wallet, $turnstile)"))
                                             (:p :id "pl-upload-note" :class "pl-line"
                                                 (fmt "Build from a drawing: a PDF of up to ~d pages, an image or a text file (DXF, SVG, CSV, STEP), ~a at most."
                                                      (getf caps :pages) (size-label (getf caps :bytes)))))))))
                        ;; hidden until Datastar has read the signals: no
                        ;; banner flashes at an owner while the script loads
                        (:div :class "pl-watch" :style "display:none" :|data-show| "!$editable && !$archived"
                              "You are watching this session as it is built.  "
                              (:a :href *url-prefix* "Start your own") ".")
                        (:div :class "pl-watch" :style "display:none" :|data-show| "$archived"
                              "An archived session: its log, its model file and its model, read-only.  "
                              (:a :href *url-prefix* "Start your own") ".")
                        (:p :class "pl-notice" :|data-show| "$notice" :|data-text| "$notice")
                        (:p :class "pl-error" :|data-show| "$error" :|data-text| "$error"))))

   ;; A visitor's own project, from GitLab (project.lisp): the form that
   ;; opens one in a fresh session, and, in a project session, what has
   ;; changed, how it stands against the profile, and the push -- staged at
   ;; the git gate and approved by the visitor on the gate's own page
   ;; ($pushurl), signed in with GitLab there.
   (project-tile
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let ((session (the session)))
                    (with-lhtml-string ()
                      (when (projects-offered?)
                        (cond
                          ((project-session? session)
                           (let ((record (project-record session))
                                 (changes (project-changes session))
                                 (findings (profile-findings session)))
                             (htm (:div :class "pl-card pl-project"
                                        (:h2 "GitLab project")
                                        (:p :class "pl-line"
                                            (:a :href (gethash "web_url" record) :target "_blank" :rel "noopener"
                                                (esc (gethash "path" record)))
                                            (fmt " -- ~d file~:p" (length (project-paths (project-directory session)))))
                                        (:p :class "pl-line"
                                            (fmt "The profile: ~:[~d thing~:p to do~;met, as far as reading can tell~*~]."
                                                 (every #'first findings) (count nil findings :key #'first)))
                                        (if changes
                                            (htm (:p :class "pl-line" (fmt "Changed: ~{~a~^, ~}" changes)))
                                            (htm (:p :class "pl-line pl-dim" "Nothing changed yet.")))
                                        (when (and (pushes-offered?) changes)
                                          (htm (:div :class "pl-row" :style "display:none" :|data-show| "$editable"
                                                     (:button :type "button" :class "pl-build"
                                                              :|data-attr:disabled| "$busy"
                                                              :|data-on:click| (the (datastar-action :push-project
                                                                                                     :options "{filterSignals: {include: /^(owner)$/}}"))
                                                              "Push changes"))))
                                        (:p :class "pl-line" :style "display:none" :|data-show| "$pushurl"
                                            (:a :target "_blank" :rel "noopener" :|data-attr:href| "$pushurl"
                                                "Approve the push on GitLab")
                                            " -- you sign in with GitLab there, and nothing is pushed until you press its button.")
                                        ;; hosting (hosting.lisp): the default branch as it is on
                                        ;; GitLab, so only a project with nothing left to push
                                        (when (and (pushes-offered?) (not changes) (notevery #'first findings))
                                          (htm (:p :class "pl-line pl-dim"
                                                   (fmt "Deploy to ~a opens once the profile is met on GitLab: have the agent bring it in, push, and merge." *hosting-domain*))))
                                        (when (and (pushes-offered?) (not changes) (every #'first findings))
                                          (htm (:div :class "pl-row" :style "display:none" :|data-show| "$editable"
                                                     (:button :type "button" :class "pl-build"
                                                              :data-doc (format nil "Host the project's default branch, as it is on GitLab, at its own name under ~a: the house builds and checks it, reads it, and puts it up if both pass.  A Maintainer of the project approves it on GitLab." *hosting-domain*)
                                                              :|data-attr:disabled| "$busy"
                                                              :|data-on:click| (the (datastar-action :deploy-project
                                                                                                     :options "{filterSignals: {include: /^(owner)$/}}"))
                                                              (fmt "Deploy to ~a" *hosting-domain*)))))
                                        (:p :class "pl-line" :style "display:none" :|data-show| "$hosturl"
                                            (:a :target "_blank" :rel "noopener" :|data-attr:href| "$hosturl"
                                                "Approve hosting on GitLab")
                                            " -- a Maintainer of the project signs in with GitLab there; nothing is sent until the button is pressed.")
                                        (let ((request (session-hosting-request (session-id session))))
                                          (when request
                                            (htm (:div :class "pl-hosting"
                                                       (:p :class "pl-line"
                                                           (fmt "Hosting ~a at ~a: " (gethash "project" request)
                                                                (subseq (gethash "sha" request) 0 8))
                                                           (:b (esc (hosting-state-text (gethash "state" request)))))
                                                       (when (gethash "url" request)
                                                         (htm (:p :class "pl-line"
                                                                  (:a :href (gethash "url" request) :target "_blank" :rel "noopener"
                                                                      (esc (gethash "url" request))))))
                                                       (dolist (line (hosting-request-reasons request))
                                                         (htm (:p :class "pl-line pl-dim" (esc line))))))))))))
                          ((not (and session (or (model-defined? session) (app-defined? session))))
                           (htm (:div :class "pl-card pl-project" :style "display:none" :|data-show| "!$opened || $editable"
                                      (:h2 "Your GitLab project")
                                      (:p :class "pl-line"
                                          (fmt "Open your own project from ~a.  Ready as it is?  Deploy it to ~a once it passes the house's checks.  If not, have the agent bring it into the profile first; the changes go back as a merge request you approve." *gitlab-url* *hosting-domain*))
                                      (:div :class "pl-row"
                                            (:input :type "text" :placeholder "group/project" :|data-bind:gitproject| ""
                                                    :style "flex:1;min-width:0")
                                            (:button :type "button" :class "pl-build"
                                                     :|data-attr:disabled| "$busy || !$gitproject"
                                                     :|data-on:click| (the (datastar-action :open-project
                                                                                            :options "{filterSignals: {include: /^(gitproject|owner|turnstile|wallet)$/}}"))
                                                     "Open")))))))))))

   ;; The session's web app (kinds.lisp), once there is one: a page of
   ;; its own, opened beside the lab by whoever may see the session.
   ;; Nothing shows until then.
   (app-section
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let ((session (the session)))
                    (with-lhtml-string ()
                      (when (and session (app-defined? session))
                        (htm (:div :class "pl-card pl-app"
                                   (:h2 "Web app")
                                   (:p :class "pl-line"
                                       (:a :class "pl-open-app" :target "_blank" :rel "noopener"
                                           :href (app-url session
                                                          :owner-key (and (session-private? session) (the owner?)
                                                                          (the owner-key)))
                                           :data-doc "Open the web app this session built, a page of its own, in a new tab"
                                           "Open the app")
                                       (str (if (the owner?)
                                                "  -- the page your prompts built.  It opens afresh from the model file each time."
                                                (format nil "  -- a page built by this session's visitor, not by ~a." *brand*)))))))))))

   ;; THE STAGE: a session that builds a web app shows the app itself,
   ;; live in a frame, where a model's drawing would be -- the tree and
   ;; inspector stay beside it.  A tile under the panes whose section
   ;; (pl-app-host) the page's script moves into the pane grid
   ;; (plStageApp); while it holds the frame, *sheet-css* hides the
   ;; panes' cells and gives it their room, and the divider to the
   ;; model file under them works as it does for a drawing.  The stream
   ;; patches a section by its id wherever it stands.  Keyed to the
   ;; model's stamp, not the log's revision, so the app reloads when it
   ;; is rebuilt and not at every line the agent writes.  The frame is
   ;; the app door's own instance of APP: the inspector beside it looks
   ;; at the sluice's instance, so an edit there does not reach it.
   (app-stage
    :type 'base-html-div
    :div-class "pl-app-host"
    :inner-html (progn
                  (the model-stamp)
                  (let ((session (the session)))
                    (with-lhtml-string ()
                      ;; a project session: the project on the stage, as it
                      ;; stands against the profile and file by file
                      (when (project-session? session)
                        (the revision)
                        (let ((changes (project-changes session)))
                          (htm (:div :id "pl-app-stage"
                                     (:div :class "pl-project-stage"
                                           (:h2 (esc (gethash "path" (project-record session))))
                                           (:pre (esc (profile-text session)))
                                           (:ul (dolist (file (project-files session))
                                                  (htm (:li (:code (esc (car file)))
                                                            (fmt " (~:d)" (cdr file))
                                                            (when (member (car file) changes :test #'string=)
                                                              (htm (:b " changed"))))))))))))
                      ;; a web app not built yet: an empty frame where it
                      ;; will be, not the sluice's welcome.  Before the
                      ;; session opens, the switch decides (Datastar hides
                      ;; it with an inline display:none, which *sheet-css*
                      ;; reads to give the panes back for a model).
                      (when (and (not (app-staged? session))
                                 (not (and session (project-session? session)))
                                 (or (null session) (eq (session-kind session) :app)))
                        (htm (:div :id "pl-app-stage"
                                   :|data-show| (if session "true" "$kind == 'app'")
                                   (:div :class "pl-app-empty"
                                         (:h2 "Your web app")
                                         (:p "It shows here, live, once the agent has built it.  Describe it on the left and press Build.")))))
                      (when (app-staged? session)
                        (htm (:div :id "pl-app-stage"
                                   (:iframe :title "The web app this session built"
                                            :src (format nil "~a&v=~a"
                                                         (app-url session
                                                                  :owner-key (and (session-private? session) (the owner?)
                                                                                  (the owner-key)))
                                                         (or (the model-stamp) 0))))))))))

   ;; Monetize (deploy.lisp): the owner deploys what the session built
   ;; at an address of its own, on terms of their choosing.  Like the
   ;; prompt, a tile that reads nothing that changes, so what is typed
   ;; stays; what it shows changes through signals.
   (monetize-tile
    :type 'base-html-div
    :inner-html (with-lhtml-string ()
                  (when *deployments?*
                    (htm (:div :class "pl-card pl-monetize" :style "display:none" :|data-show| "$editable && $built"
                               (:div :class "pl-row" :style "margin-top:0"
                                     ;; greyed until what was built charges for something:
                                     ;; the visitor says what, the agent writes the tollbooths
                                     (:button :type "button" :class "pl-build pl-monetize-button"
                                              :data-doc "Deploy what this session built at an address of its own, for others to pay to use.  It opens once the agent has written what it charges for."
                                              :|data-attr:disabled| "!$story"
                                              :|data-on:click| "$monetize = !$monetize"
                                              "Monetize")
                                     (:a :class "pl-open-app" :target "_blank" :rel "noopener"
                                         :|data-show| "$deployed" :|data-attr:href| "$deployed" :|data-text| "$deployed"))
                               (:p :class "pl-line" :|data-show| "!$story"
                                   (unit-text "Nothing here charges yet.  Tell the agent what should cost {units} -- 'charge 300 {units} for each STEP download', 'a pass for 500 {units} unlocks the results' -- and Monetize opens when it has written that in."))
                               (:div :class "pl-deploy" :|data-show| "$monetize && $story"
                                     (:p :class "pl-line"
                                         "Deploy what you built at an address of its own.  A model gets a page where others change its inputs and download its files; a web app is served as it is.  It is a copy: deploy again to update it.")
                                     (:label "Name in the address"
                                             (:input :type "text" :maxlength "40" :placeholder "shelf-bracket" :|data-bind:dname| ""))
                                     (:label "Title"
                                             (:input :type "text" :maxlength "80" :|data-bind:dtitle| ""))
                                     (:label "What it is, in a line or two"
                                             (:textarea :rows "2" :maxlength "400" :|data-bind:dblurb| ""))
                                     ;; the source terms were the session's choice as it opened
                                     (:p :class "pl-line" :|data-show| "!$closed"
                                         (fmt "Open source, under the GNU Affero General Public License: it runs on Gendl, which is under that licence, so the deployment serves its source to the people who use it.  Of what its users pay, in ~a, ~d% is the monetization fee."
                                              (units) (house-fee-percent nil)))
                                     (:p :class "pl-line" :|data-show| "$closed"
                                         (fmt "Closed source, as you chose when the session opened: the house licenses the Gendl it runs on for closed use, so the deployment does not serve its source.  Of what its users pay, in ~a, ~d% is the monetization fee."
                                              (units) (house-fee-percent t)))
                                     ;; the author's slider: points of every payment, on top of
                                     ;; the fee, for the community pot (deploy.lisp)
                                     (when *pot-percent-least*
                                       (htm (:label :data-doc (unit-text "Give more of every payment to the community pot of {units} that pays for everyone's builds.  It is taken on top of the monetization fee, and kept with the deployment's terms.")
                                                    (:span :|data-text| (format nil "'To the community pot, on top of the fee: ' + $dpot + '%'"))
                                                    (:input :type "range" :step "1"
                                                            :min (format nil "~d" *pot-percent-least*)
                                                            :max "50" :|data-bind:dpot| ""))
                                            (:p :class "pl-line"
                                                (:span :|data-text|
                                                       (format nil "'You receive ' + (($closed ? ~d : ~d) - Number($dpot)) + '% of what its users pay.'"
                                                               (- 100 (house-fee-percent t)) (- 100 (house-fee-percent nil)))))))
                                     ;; the hosting terms, in short, by the source terms
                                     (:p :class "pl-line" :|data-show| "!$closed"
                                         "Hosting: an open-source deployment with no tollbooth, or with no revenue for an extended period, may be un-hosted -- or kept, if we and its visitors find it interesting.")
                                     (:p :class "pl-line" :|data-show| "$closed"
                                         "Hosting: a closed-source deployment that shows no revenue, or too little, for some time may be taken down.  Its code then stays yours, still closed, and is wiped from our systems.")
                                     ;; what it charges for is in the source, written by
                                     ;; the agent at the visitor's word (deploy.lisp)
                                     (:p :class "pl-line"
                                         "It charges what its tollbooths say, as you had the agent write them: ask for a change in a prompt and deploy again.  No money moves yet: a web app's tolls are taken as test payments, booked and marked as tests, and a model's priced downloads open only to you.")
                                     (:label "Where to reach you about your share (email)"
                                             (:input :type "email" :maxlength "200" :|data-bind:dpayee| ""))
                                     (:p :class "pl-line"
                                         (unit-text "Your share is held in {units} through each quarter and paid out after it, at that day's rate.  ")
                                         (:a :target "_blank" :rel "noopener" :|data-show| "$deployed"
                                             :|data-attr:href| (format nil "'~a?name=' + $deployed.split('/').pop() + '&owner=' + $owner"
                                                                       (door-path "earnings"))
                                             "What it has taken so far"))
                                     (:p :class "pl-line"
                                         "Keep a copy of the model file for your own records: the lab holds your code as a file, and the editor shows all of it.")
                                     (:div :class "pl-row"
                                           (:button :type "button" :class "pl-download"
                                                    :|data-attr:disabled| "!$dname.trim()"
                                                    :|data-on:click| (the (datastar-action :deploy :options "{filterSignals: {include: /^(d[a-z]+|owner|turnstile)$/}}"))
                                                    "Deploy")
                                           (:button :type "button" :class "pl-download" :|data-show| "$deployed"
                                                    :|data-on:click| (format nil "confirm('Take the deployment down?') && ~a"
                                                                             (the (datastar-action :undeploy :options "{filterSignals: {include: /^(owner)$/}}")))
                                                    "Take it down"))))))))

   ;; The model file's editor, under the panes.  Its card is never
   ;; morphed (data-ignore-morph): CodeMirror builds its own DOM beside
   ;; the textarea, which a patch would strip.  New versions of the file
   ;; arrive as plSetSource calls (sheets-hear); Save sends the text as
   ;; the `source' signal and nothing else.
   (editor-tile
    :type 'base-html-div
    :inner-html (let ((session (the session)))
                  (with-lhtml-string ()
                    (:div :class "pl-card pl-source" :|data-ignore-morph| ""
                          :|data-effect| "window.plLock && plLock(!$editable || $busy)"
                          (:div :class "pl-source-head"
                                (:h2 "Model file")
                                (:span :id "pl-source-state" :class "pl-source-state")
                                (:button :type "button" :|data-on:click| "plFold(true)" "fold all")
                                (:button :type "button" :|data-on:click| "plFold(false)" "unfold all")
                                (:button :type "button" :class "pl-save"
                                         :|data-show| "$editable"
                                         :|data-indicator:saving| ""
                                         :|data-attr:disabled| "$busy || $saving"
                                         :|data-on:click|
                                         (format nil "plConfirmSave() && ($source = plSourceText(), ~a)"
                                                 (the (datastar-action :save :options "{filterSignals: {include: /^(source|owner)$/}}")))
                                         "Save"))
                          (:textarea :id "pl-source" :spellcheck "false"
                                     (esc (or (the archive-source)
                                              (and session (ignore-errors (model-body session)))
                                              "")))))))

   (status-section
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let ((session (the session)))
                    (with-lhtml-string ()
                      (:div :class "pl-card pl-status"
                            (cond
                              ((the archive-missing)
                               (htm (:span "The archive holds no session of that name.")))
                              ((the archive-pending)
                               (htm (:span "This archived session is private: it opens only in the browser that owned it.")))
                              ((the archive-id)
                               (let* ((record (the archive-record))
                                      (id (the archive-id))
                                      (engine (or (and record (gethash "engine" record)) "gendl"))
                                      (created (and record (gethash "created" record)))
                                      (versions (length (the archive-versions)))
                                      (base (format nil "~a?archive=~a" *url-prefix* id)))
                                 (htm (:span (fmt "Archived session ~a" id))
                                      (when (integerp created)
                                        (htm (:span (str (multiple-value-bind (s m h d mo y) (decode-universal-time created 0)
                                                           (declare (ignore s))
                                                           (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d UTC" y mo d h m))))))
                                      (unless (equal engine (engine-name))
                                        (htm (:span (fmt "built on ~a: " engine)
                                                    (if (car *sibling-lab*)
                                                        (htm (:a :href (format nil "~a?archive=~a" (car *sibling-lab*) id) "open it there"))
                                                        (str "not drawn here")))))
                                      (when (live? id)
                                        (htm (:a :href (format nil "~a?session=~a" *url-prefix* id) "live now")))
                                      ;; the model file's saved versions, for the editor
                                      (when (> versions 1)
                                        (htm (:span "versions:"
                                                    (loop for v from 1 to versions
                                                          do (htm " " (if (eql v (the archive-version))
                                                                          (htm (:b (fmt "~d" v)))
                                                                          (htm (:a :href (format nil "~a&version=~d" base v) (fmt "~d" v))))))
                                                    " "
                                                    (if (the archive-version) (htm (:a :href base "last")) (htm (:b "last")))))))))
                              ((the private-id)
                               (htm (:span "This session is private: it opens only in the browser that owns it.")))
                              ((null session)
                               (htm (:span "No session yet: your first build opens one.")))
                              (t
                                (let ((usage (session-usage session)) (pot (pot)))
                                  (htm (:span (fmt "Session ~a" (session-id session)))
                                       (:span (str (if (session-busy? session) "working" "ready")))
                                       (:span (fmt "~a of ~a prompts" (prompts-used session) *max-prompts-per-session*))
                                       (:span (fmt "tokens ~:d in, ~:d out" (getf usage :input) (getf usage :output)))
                                       (when pot
                                         (htm (:span (fmt "pot ~:d ~a" (max 0 (floor (or (getf pot :credits) 0))) (units)))))
                                       ;; closing the session to watchers, once it has bought credits
                                       (when (and (the owner?) (or (paid? session) (session-private? session)))
                                         (htm (:label :class "pl-private"
                                                      :title "A private session is out of the listings and opens only in the browser that owns it"
                                                      (:input :type "checkbox"
                                                              :checked (when (session-private? session) "checked")
                                                              :|data-on:change|
                                                              (format nil "$private = el.checked; ~a"
                                                                      (the (datastar-action :privacy :options "{filterSignals: {include: /^(private|owner)$/}}"))))
                                                      " private"))))))))))))

   ;; The visitor's uploaded files, each a download for whoever may see
   ;; the session (the file door); nothing shows until there is one.
   (files-section
    :type 'base-html-div
    :inner-html (let ((files (the shown-files))
                      (session (the session)))
                  (with-lhtml-string ()
                    (when files
                      (htm (:div :class "pl-card pl-files"
                                 (:h2 "Uploaded files")
                                 (dolist (file files)
                                   (htm (:p :class "pl-line"
                                            (:a :href "#"
                                                :|data-on:click__prevent|
                                                (escape-string-minimal-plus-quotes
                                                 (format nil "plDownload(~a, $owner)"
                                                         (js-string-literal
                                                          (if (the archive-id)
                                                              (file-url (the archive-id) (the-object file file-name) :archive? t)
                                                              (file-url (session-id session) (the-object file file-name))))))
                                                (esc (the-object file file-name)))
                                            (fmt " (~a, ~a)" (the-object file kind-label) (the-object file size-label)))))
                                 (:p :class "pl-line"
                                     (str (if (and session (session-private? session))
                                              "Private, with the session."
                                              "Public, with the session.")))
                                 ;; a drawing that wants solids is offered the
                                 ;; sibling lab, which takes the files over
                                 ;; (routing.lisp)
                                 (let ((routing (and session (null (the archive-id)) (routing-state session))))
                                   (when (and routing (gethash "sibling_url" routing))
                                     (htm (:p :class "pl-line pl-route"
                                              (esc (gethash "reason" routing))
                                              "  This lab draws holes without cutting them. "
                                              (when (the owner?)
                                                (htm (:a :href (escape-string-minimal-plus-quotes (gethash "sibling_url" routing))
                                                         (fmt "Build it in the ~a" (cdr *sibling-lab*)))
                                                     "."))))))))))))

   (log-section
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let* ((session (the session))
                         ;; an archived session's log is its record's
                         (log (if (the archive-id)
                                  (let ((record (the archive-record)))
                                    (loop for entry in (and record (gethash "log" record))
                                          when (and (listp entry) (= (length entry) 3) (integerp (first entry)))
                                            collect entry))
                                  (and session (session-log session))))
                         (shown (last log *log-shown*)))
                    (with-lhtml-string ()
                      (:div :class "pl-card pl-log"
                            (:h2 "Log")
                            ;; the reply as it is written (sheets-hear-text)
                            (:div :class "pl-entry pl-live" :style "display:none" :|data-show| "$live"
                                  (:span :class "pl-kind" "writing")
                                  (:span :class "pl-text" :|data-text| "$live"))
                            (when (> (length log) (length shown))
                              (htm (:p :class "pl-entry pl-note"
                                       (fmt "~d earlier entries not shown." (- (length log) (length shown))))))
                            (dolist (entry (reverse shown))
                              (destructuring-bind (time kind text) entry
                                (declare (ignore time))
                                (htm (:div :class (format nil "pl-entry pl-~(~a~)" kind)
                                           (:span :class "pl-kind" (fmt "~(~a~)" kind))
                                           (:span :class "pl-text" (esc text)))))))))))

   ;; Modeling credits, the one unit a visitor sees: the community pot
   ;; where the gate keeps one (what it holds, its cap, what this session
   ;; drew and this browser added, the offers that fit its room), else the
   ;; session's free credits and the wallet's balance.  Recomputed at every
   ;; change to the session; the pot itself is asked of the gate at most
   ;; every *pot-refresh-seconds* (pot).
   (credits-section
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let* ((session (the session))
                         (spend (and session (the owner?) (spend-state session))))
                    (with-lhtml-string ()
                      (:div :class "pl-card pl-credits"
                            (:h2 (esc (units-title)))
                            (str (credits-html self (pot) spend))
                            ;; the payment in the page (start-payment!): Stripe's
                            ;; card control and form live in frames the script
                            ;; makes, which a patch must not strip
                            (:div :id "pl-pay" :class "pl-pay" :|data-ignore-morph| "" :style "display:none"
                                  (:div :id "pl-card-amount" :class "pl-line")
                                  (:div :id "pl-card-line" :class "pl-card-line")
                                  (:div :id "pl-card-error" :class "pl-error" :role "alert")
                                  (:div :id "pl-checkout")
                                  (:div :class "pl-row"
                                        (:button :type "button" :id "pl-card-pay" :class "pl-build" :style "display:none" "Pay")
                                        (:button :type "button" :class "pl-download" :onclick "plClosePayment()" "Close")
                                        (:a :href "#" :id "pl-pay-more" :onclick "plMoreOptions(); return false" "more payment options"))))))))

   ;; The model's files, from the download door (export.lisp): fetched, so
   ;; a refusal is a line on the page and not a saved file, and with the
   ;; owner's key, which the door needs for a private session and to
   ;; meter the run on the owner.
   (downloads-section
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let ((session (the session)))
                    (with-lhtml-string ()
                      (:div :class "pl-card pl-downloads"
                            (:h2 "Download")
                            (if (and session (model-defined? session))
                                (dolist (entry (download-formats))
                                  (htm (:button :type "button" :class "pl-download"
                                                ;; cl-who writes attribute values raw
                                                :|data-on:click|
                                                (escape-string-minimal-plus-quotes
                                                 (format nil "plDownload(~a, $owner)"
                                                         (js-string-literal
                                                          (format nil "~a?~:[session~;replay~]=~a&format=~a"
                                                                  (door-path "download") (session-replay? session)
                                                                  (session-id session) (first entry)))))
                                                (esc (fourth entry)))))
                                (htm (:p :class "pl-line" "The model's drawings and files, once it is built.")))
                            ;; the model file in the host's terminal, for its owner
                            (let ((console (and session (the owner?) (not (session-replay? session))
                                                (ignore-errors (console-url session)))))
                              (when console
                                (htm (:p :class "pl-line"
                                         (:a :href console :target "_blank" :rel "noopener"
                                             :data-doc "Open the model file in a terminal on the host, beside the page"
                                             "Open the file in a console")))))
                            (:p :id "pl-download-note" :class "pl-line")))))))

  :functions
  ((set-instantiation-time!
    ()
    (call-next-method)
    ;; ?session=<id>: the sheet shows that session to whoever may see it
    ;; (a private one only to its owner: the claim opens it, once the
    ;; browser has shown the key)
    (let* ((id (cdr (assoc "session" (the query-toplevel) :test #'string-equal)))
           (session (and (stringp id) (find-session id)))
           (archive (cdr (assoc "archive" (the query-toplevel) :test #'string-equal))))
      (cond ((and (null session) (stringp archive))
             ;; ?archive=<id>[&version=<n>]: an archived session, read-only
             (the (show-archive! archive nil)))
            ((null session))
            ((visible-to? session nil) (the (show-session! session)))
            (t (the (set-slot! :private-id (session-id session)))))))

   (show-archive!
    (id key)
    ;; An archived session: its record's log and numbers, the model file
    ;; as it last was (or its &version=), and its model drawn by a REPLAY
    ;; -- the last version compiled again in a package of its own
    ;; (browse.lisp), which is the sheet's session from then on, so the
    ;; page is read-only and the sluice draws it.  A private record waits
    ;; for its owner's key (claim); a record from the other engine's room
    ;; draws nothing here and links to that lab.
    (let* ((directory (and *browsing?* (archived-directory id)))
           (record (and directory (read-record (merge-pathnames "session.json" directory)))))
      (cond ((null record)
             (the (set-slot! :archive-missing id)))
            ((not (record-visible? record key))
             (the (set-slot! :archive-pending id)))
            (t
             (the (set-slot! :archive-pending nil))
             (the (set-slot! :archive-id id))
             (let ((version (ignore-errors (parse-integer (cdr (assoc "version" (the query-toplevel) :test #'string-equal))))))
               (when version (the (set-slot! :archive-version version))))
             (the (set-slot! :shown-source (or (the archive-source) "")))
             (when (equal (or (gethash "engine" record) "gendl") (engine-name))
               (let ((replay (ignore-errors (ensure-replay id))))
                 (when replay
                   (the (set-slot! :session replay))
                   (the (set-slot! :model-stamp (model-stamp replay)))
                   (the show-model!))))))))

   (show-session!
    (session)
    (the (set-slot! :session session))
    (the (set-slot! :private-id nil))
    (the (set-slot! :model-stamp (model-stamp session)))
    (the (set-slot! :shown-source (or (ignore-errors (model-body session)) "")))
    (watch-session! self session)
    (the show-model!))

   (show-model!
    ()
    ;; the sluice opens on the session's MODEL, a symbol, once there is
    ;; one; a rebuild after that redefines the model's class, and the
    ;; sluice hears that itself (refresh-redefined!) and redraws
    ;; A web app without a MODEL (kinds.lisp) opens the sluice on APP
    ;; itself: the page's own tree, its controls and sections.
    ;; A session that builds a web app opens it on APP, the page, even
    ;; when it holds a MODEL: the app is on the stage (app-stage), and
    ;; the tree shows what the page is made of.
    (let* ((session (the session))
           (symbol (and session (cond ((app-staged? session) (app-symbol session))
                                      ((model-defined? session) (model-symbol session))
                                      ((app-defined? session) (app-symbol session))))))
      (when (and symbol (not (eq (the root-object-type) symbol)))
        (the (set-slot! :root-object-type symbol))
        ;; hidden lines removed up to *hidden-lines-max-leaves* leaves
        ;; (quadratic in the edges; a pane's View > Hidden lines turns it on for
        ;; a larger model), and the model's leaves drawn
        (unless (app-staged? session)
          (when (and (the root-object)
                     (<= (or (ignore-errors (length (the root-object leaves))) 0)
                         *hidden-lines-max-leaves*))
            (ignore-errors (the viewport (set-slot! :hidden-lines :remove))))
          (when (the root-object)
            (ignore-errors (the viewport (draw-leaves! (the root-object)))))))))

   (wear-skin
    (signals)
    ;; (not `skin': that is the sluice's input of the name, and GDL keys
    ;; messages by name) the skin this browser chose, worn in
    ;; place (the sluice's set-skin!) -- only a name the sluice knows, so
    ;; a skin of the page's own is left alone, not taken for the house look
    (let ((name (gethash "skinPref" signals)))
      (when (and (stringp name)
                 (or (assoc name (sluice:skin-choices) :test #'string-equal)
                     (assoc name sluice:*skin-aliases* :test #'string-equal)))
        (the (set-skin! (string-downcase name))))))

   (tell-error!
    (reason)
    (sheet-send! self (datastar-signals-event
                       (with-output-to-string (s) (yason:encode (h "error" reason) s)))))

   (save
    (signals)
    ;; the visitor's own edit: written, compiled and loaded like the
    ;; agent's, the session claimed meanwhile (as the model door does)
    (let ((session (the session))
          (source (gethash "source" signals)))
      (flet ((saved (ok? text)
               (sheet-send! self (datastar-script-event
                                  (format nil "plSaved(~a,~a)" (json-boolean ok?) (js-string-literal text))))))
        (cond ((not (the owner?)) (the (tell-error! "Only the session's owner may save the model file.")))
              ((not (stringp source)) (the (tell-error! "No source given.")))
              ((not (claim! session)) (the (tell-error! "Wait for the agent to finish first.")))
              (t (multiple-value-bind (blocks error?)
                     (unwind-protect (write-model (touch session) source)
                       (setf (session-busy? session) nil))
                   (let ((text (or (cdr (assoc "text" (first blocks) :test #'string=)) "")))
                     (log-event session :reload "Your edit: ~a" text)
                     (save-session! session)
                     (the (tell-error! ""))
                     (saved (not error?) text))))))))

   (claim
    (signals)
    ;; the owner's browser holds the key; and back from paying, the
    ;; checkout it brings is credited once (as the page's confirm door)
    (when (and (the archive-pending) (stringp (gethash "owner" signals)))
      ;; a private archived session, shown now to the browser that owned it
      (the (show-archive! (the archive-pending) (gethash "owner" signals)))
      (sheet-send! self (datastar-script-event
                         (format nil "plSetSource(~a)" (js-string-literal (the shown-source))))))
    (let* ((key (gethash "owner" signals))
           (checkout (gethash "checkout" signals))
           (wallet (gethash "wallet" signals))
           (session (or (the session)
                        ;; a private session, shown now to its owner
                        (let ((private (and (the private-id) (find-session (the private-id)))))
                          (when (and private (stringp key) (owner? private key))
                            (the (show-session! private))
                            (sheet-send! self (datastar-script-event
                                               (format nil "plSetSource(~a)"
                                                       (js-string-literal (the shown-source)))))
                            private)))))
      (when (and session (stringp key) (owner? session key))
        (the (set-slot! :owner-key key))
        (touch session)
        (sheet-send! self (datastar-signals-event "{\"editable\": true}")
                     (datastar-script-event (keep-last-script (session-id session))))
        (when (and (stringp checkout) (plusp (length checkout)) (wallet-id? wallet))
          (multiple-value-bind (outcome text) (confirm-topup! session wallet checkout)
            (sheet-send! self
                         (datastar-signals-event (with-output-to-string (s)
                                                   (yason:encode (h "checkout" "" "notice" text) s)))
                         (datastar-script-event
                          (format nil "try{localStorage.setItem('prompt-lab-wallet',~a)}catch(e){};history.replaceState(null,'',~a)"
                                  (js-string-literal wallet)
                                  (js-string-literal (format nil "~a?session=~a" *url-prefix* (session-id session))))))
            (unless (member outcome '("credited" "already") :test #'equal)
              (the (tell-error! text))))))))

   (privacy
    (signals)
    ;; the owner closes the session to watchers, or opens it again
    (let ((session (the session))
          (private? (eq (gethash "private" signals) t)))
      (if (not (the owner?))
          (the (tell-error! "Only the session's owner may change that."))
          (multiple-value-bind (ok? reason) (set-privacy! session private?)
            (cond (ok?
                   ;; a session older than owner keys has one now
                   (the (set-slot! :owner-key (session-owner session)))
                   (sheet-send! self
                                (datastar-script-event (owners-script session))
                                (datastar-signals-event
                                 (with-output-to-string (s)
                                   (yason:encode (h "notice" (if private?
                                                                 "Private: out of the listings, and open only in this browser."
                                                                 "Open to view again.")
                                                    "error" "")
                                                 s)))))
                  (t (the (tell-error! reason))))))))

   (own-session!
    (address wallet &optional closed?)
    ;; the session of a visitor who has none yet, opened for this sheet
    ;; (closed-source when they chose that: deploy.lisp); nil with the
    ;; refusal shown when the address is over its cap
    (or (the session)
        (multiple-value-bind (session reason)
            (open-session! address (and (wallet-id? wallet) wallet) :closed? closed?)
          (cond (session
                 (the (set-slot! :session session))
                 (the (set-slot! :owner-key (session-owner session)))
                 (watch-session! self session)
                 (sheet-send! self (datastar-script-event (owners-script session))
                              (datastar-signals-event
                               ;; the owner signal too: it was read from the
                               ;; browser's keys when the page opened, before
                               ;; there was a session, and what the page sends
                               ;; with a download or an upload is that signal --
                               ;; without it a private (closed-source) session's
                               ;; own downloads answered 'No such session'
                               (format nil "{\"opened\": true, \"closed\": ~a, \"owner\": \"~a\"}"
                                       (json-boolean (session-closed? session))
                                       (session-owner session))))
                 session)
                (t (the (tell-error! reason)) nil)))))

   (spent-token!
    ()
    ;; a Turnstile token is single-use
    (when *turnstile-site-key*
      (sheet-send! self
                   (datastar-signals-event "{\"turnstile\": \"\"}")
                   (datastar-script-event "if(window.turnstile)turnstile.reset()"))))

   (build
    (signals)
    (let ((address (client-address *datastar-request*))
          (prompt (gethash "prompt" signals))
          (token (let ((token (gethash "turnstile" signals))) (and (stringp token) (plusp (length token)) token))))
      (cond ((and (the session) (not (the owner?)))
             (the (tell-error! "This session is someone else's; start your own to build.")))
            ((the (own-session! address (gethash "wallet" signals) (eq (gethash "closedPick" signals) t)))
             (multiple-value-bind (started reason response route)
                 (begin-prompt! (the session) prompt address token
                                ;; a visitor the sibling lab sent here stays here
                                :route? (not (the arrived?))
                                ;; the switch: a model or a web app (kinds.lisp)
                                :kind (gethash "kind" signals))
               (declare (ignore response))
               (cond (started
                      (sheet-send! self (datastar-signals-event "{\"error\": \"\", \"prompt\": \"\"}")))
                     ;; the request wants the sibling lab's engine
                     ;; (routing.lisp): the browser goes there, the prompt
                     ;; and the reason with it
                     (route
                      (sheet-send! self (datastar-script-event
                                         (format nil "plRoute(~a,~a,~a,~a)"
                                                 (js-string-literal (getf route :url))
                                                 (js-string-literal (string-trim '(#\space #\tab #\newline #\return) prompt))
                                                 (js-string-literal reason)
                                                 (js-string-literal (let ((kind (parse-kind (gethash "kind" signals))))
                                                                      (if kind (kind-name kind) "")))))))
                     (t (the (tell-error! reason))))
               (the spent-token!))))))

   (deploy
    (signals)
    ;; Monetize (deploy.lisp): the owner deploys what the session built,
    ;; on the terms the tile's signals carry
    (let ((session (the session))
          (address (client-address *datastar-request*))
          (token (let ((token (gethash "turnstile" signals))) (and (stringp token) (plusp (length token)) token))))
      (if (not (the owner?))
          (the (tell-error! "Only the session's owner may deploy it."))
          (multiple-value-bind (ok? reason) (topup-check! session token address)
            (if (not ok?)
                (the (tell-error! reason))
                (multiple-value-bind (record reason)
                    (deploy-session! session
                                     :name (gethash "dname" signals) :title (gethash "dtitle" signals)
                                     :blurb (gethash "dblurb" signals)
                                     :payee (gethash "dpayee" signals)
                                     :pot-percent (let ((pot (gethash "dpot" signals)))
                                                    (cond ((realp pot) pot)
                                                          ((stringp pot) (ignore-errors (parse-integer pot :junk-allowed t))))))
                  (if record
                      (sheet-send! self (datastar-signals-event
                                         (with-output-to-string (s)
                                           (yason:encode (h "error" "" "monetize" 'yason:false
                                                            "deployed" (deployment-url (gethash "name" record))
                                                            "notice" (format nil "Deployed at ~a." (deployment-url (gethash "name" record))))
                                                         s))))
                      (the (tell-error! reason)))))
            (the spent-token!)))))

   (open-project
    (signals)
    ;; a visitor's own project, read from GitLab into a fresh session (or
    ;; read again into the project session it is): project.lisp
    (let ((address (client-address *datastar-request*))
          (token (let ((token (gethash "turnstile" signals))) (and (stringp token) (plusp (length token)) token)))
          (session (the session)))
      (cond ((not (projects-offered?)) (the (tell-error! "This lab opens no projects.")))
            ((the archive-id) (the (tell-error! "An archived session opens no project.")))
            ((and session (not (the owner?)))
             (the (tell-error! "This session is someone else's; start your own to open a project.")))
            ((and session (not (project-session? session))
                  (or (model-defined? session) (app-defined? session)))
             (the (tell-error! "This session has built something already: open the project in a new session.")))
            ((and session (session-busy? session))
             (the (tell-error! "The agent is at work; open the project when it is done.")))
            ((not (verify-turnstile token address))
             (the (tell-error! "Complete the human check first.")))
            ((the (own-session! address (gethash "wallet" signals)))
             (multiple-value-bind (opened reason) (import-project! (the session) (gethash "gitproject" signals))
               (if opened
                   (sheet-send! self (datastar-signals-event
                                      (with-output-to-string (s)
                                        (yason:encode (h "error" "" "gitproject" "" "pushurl" ""
                                                         "notice" (format nil "Opened ~a: ~d file~:p.  Ask the agent for what it needs."
                                                                          (getf opened :path) (getf opened :files)))
                                                      s))))
                   (the (tell-error! reason))))
             (the spent-token!)))))

   (push-project
    (signals)
    ;; the project's changes staged at the git gate; the visitor approves
    ;; the push on the gate's page ($pushurl), signed in with GitLab there
    (let ((session (and signals (the session))))
      (cond ((not (and session (the owner?)))
             (the (tell-error! "Only the session's owner may push its changes.")))
            ((session-busy? session)
             (the (tell-error! "The agent is at work; push when it is done.")))
            (t
             (multiple-value-bind (url reason) (stage-push! session)
               (if url
                   (sheet-send! self (datastar-signals-event
                                      (with-output-to-string (s)
                                        (yason:encode (h "error" "" "pushurl" url
                                                         "notice" "The push is ready: approve it on GitLab, through the link under Push.")
                                                      s))))
                   (the (tell-error! reason))))))))

   (deploy-project
    (signals)
    ;; hosting the project's default branch (hosting.lisp): staged at the
    ;; git gate, approved there by a Maintainer of the project ($hosturl)
    (let ((session (and signals (the session))))
      (cond ((not (and session (the owner?)))
             (the (tell-error! "Only the session's owner may ask to host its project.")))
            ((session-busy? session)
             (the (tell-error! "The agent is at work; ask when it is done.")))
            (t
             (multiple-value-bind (url reason) (stage-deploy! session)
               (if url
                   (sheet-send! self (datastar-signals-event
                                      (with-output-to-string (s)
                                        (yason:encode (h "error" "" "hosturl" url
                                                         "notice" "Ready: approve hosting on GitLab, through the link under Deploy.")
                                                      s))))
                   (the (tell-error! reason))))))))

   (undeploy
    (signals)
    (let* ((session (and signals (the session)))
           (record (and session (the owner?) (session-deployment session))))
      (if (null record)
          (the (tell-error! "This session has no deployment of yours to take down."))
          (multiple-value-bind (ok? reason) (undeploy! (gethash "name" record) (session-owner session))
            (if ok?
                (progn (log-event session :note "The deployment ~a was taken down." (gethash "name" record))
                       (sheet-send! self (datastar-signals-event
                                          "{\"deployed\": \"\", \"error\": \"\", \"notice\": \"The deployment is taken down.\"}")))
                (the (tell-error! reason)))))))

   (upload
    (signals)
    ;; a file for the agent to build from (uploads.lisp), the visitor's
    ;; first act here opening the session as a first build does
    (let ((address (client-address *datastar-request*))
          (token (let ((token (gethash "turnstile" signals))) (and (stringp token) (plusp (length token)) token))))
      (cond ((the archive-id) (the (tell-error! "An archived session takes no uploads.")))
            ((and (the session) (not (the owner?)))
             (the (tell-error! "This session is someone else's; start your own to upload.")))
            ;; a file is public with its session, and that is acknowledged
            ;; first -- unless the session is closed-source or has topped up
            ((not (or (eq (gethash "rights" signals) t)
                      (let ((session (the session)))
                        (if session
                            (or (session-closed? session) (paid? session))
                            (and *closed-source?* *deployments?* (eq (gethash "closedPick" signals) t))))))
             (the (tell-error! "Tick the box first: the file will be shared with this session.")))
            ((and *turnstile-site-key* (null token))
             (the (tell-error! "Complete the human check first.")))
            ((the (own-session! address (gethash "wallet" signals) (eq (gethash "closedPick" signals) t)))
             (multiple-value-bind (file reason)
                 (accept-upload! (the session) (gethash "name" signals) (gethash "data" signals)
                                 :rights? t :token token :address address)
               (if file
                   (sheet-send! self (datastar-signals-event
                                      (with-output-to-string (s)
                                        (yason:encode (h "error" ""
                                                         "notice" (format nil "~a is uploaded; it goes to the agent with your next prompt."
                                                                          (the-object file file-name)))
                                                      s))))
                   (the (tell-error! reason)))
               (the spent-token!))))))

   (adopt
    (signals)
    ;; the files of a session in the sibling lab, fetched by the browser
    ;; and handed over in one (routing.lisp): one human check for all
    (let ((address (client-address *datastar-request*))
          (token (let ((token (gethash "turnstile" signals))) (and (stringp token) (plusp (length token)) token)))
          (files (gethash "files" signals)))
      (cond ((the archive-id) (the (tell-error! "An archived session takes no uploads.")))
            ((and (the session) (not (the owner?)))
             (the (tell-error! "This session is someone else's; start your own to upload.")))
            ((not (eq (gethash "rights" signals) t))
             (the (tell-error! "Tick the box first: you declare the right to use and share the files.")))
            ((not (and (listp files) files (every #'hash-table-p files)))
             (the (tell-error! "No files were sent.")))
            ((not (verify-turnstile token address))
             (the (tell-error! "Complete the human check first.")))
            ((the (own-session! address (gethash "wallet" signals) (eq (gethash "closedPick" signals) t)))
             (let ((kept nil) (refused nil))
               (dolist (entry files)
                 (multiple-value-bind (file reason)
                     (accept-upload! (the session) (gethash "name" entry) (gethash "data" entry)
                                     :rights? t :checked? t)
                   (if file (push (the-object file file-name) kept) (push reason refused))))
               (sheet-send! self (datastar-signals-event
                                  (with-output-to-string (s)
                                    (yason:encode (h "error" (format nil "~{~a~^  ~}" (reverse refused))
                                                     "notice" (if kept
                                                                  (format nil "~{~a~^, ~} came over; ~:[it goes~;they go~] to the agent with your next prompt."
                                                                          (reverse kept) (rest kept))
                                                                  ""))
                                                  s))))
               (the spent-token!))))))

   (start-payment!
    (session amount &key card?)
    ;; Ask the gate for a payment and open it here: Stripe's one-line card
    ;; control (CARD? and a gate that makes a PaymentIntent, flow "card"),
    ;; else Stripe's full form in the page (a client secret), else Stripe's
    ;; hosted page (a url), which comes back with the checkout on the
    ;; address for claim to confirm.  Without a publishable key there is
    ;; no form in the page, only the hosted page.
    (let ((key (publishable-key session)))
      (multiple-value-bind (answer reason)
          (begin-topup! session amount (page-url *datastar-request* session)
                        :embedded? (and key t) :flow (and key card? "card"))
        (let ((secret (and answer (gethash "client_secret" answer)))
              (url (and answer (gethash "url" answer))))
          (cond
            ((and key (stringp secret))
             (sheet-send! self (datastar-script-event
                                (format nil "plOpenPayment(~a)"
                                        (with-output-to-string (s)
                                          (yason:encode (h "flow" (or (gethash "flow" answer) "checkout")
                                                           "client_secret" secret
                                                           "checkout" (or (gethash "checkout" answer) "")
                                                           "publishable_key" key
                                                           "wallet" (or (session-wallet session) "")
                                                           "amount" amount
                                                           "label" (format nil (unit-text "~:d {units} for ~a~:[~;, into the community pot~]")
                                                                           (credits-for-amount amount) (dollars amount) (pot)))
                                                        s))))))
            ((stringp url)
             (sheet-send! self (datastar-script-event
                                (format nil "try{localStorage.setItem('prompt-lab-wallet',~a)}catch(e){};location.href=~a"
                                        (js-string-literal (or (session-wallet session) ""))
                                        (js-string-literal url)))))
            (t (the (tell-error! (or reason "The top-up could not be started.")))))))))

   (topup-full
    (signals)
    ;; "more payment options": Stripe's full form for the amount the card
    ;; line was opened for; the human check that opened it covers this
    (let ((session (the session))
          (amount (gethash "amount" signals)))
      (cond ((not (the owner?)) (the (tell-error! (unit-text "Only the session's owner may add {units} here."))))
            ((not (integerp amount)) (the (tell-error! "Say how much.")))
            (t (multiple-value-bind (ok? reason)
                   (topup-check! session nil (client-address *datastar-request*))
                 (if ok?
                     (the (start-payment! session amount))
                     (the (tell-error! reason))))))))

   (confirm
    (signals)
    ;; a card paid in the page: the gate credits the checkout once
    (let ((session (the session))
          (checkout (gethash "checkout" signals))
          (wallet (gethash "wallet" signals)))
      (cond ((not (the owner?)) (the (tell-error! "Only the session's owner may confirm a payment.")))
            ((not (and (stringp checkout) (plusp (length checkout)) (wallet-id? wallet)))
             (the (tell-error! "The payment could not be confirmed: no checkout named.")))
            (t (multiple-value-bind (outcome text) (confirm-topup! session wallet checkout)
                 (sheet-send! self (datastar-signals-event
                                    (with-output-to-string (s)
                                      (yason:encode (h "notice" (cond ((equal outcome "credited")
                                                                       (if (pot)
                                                                           (unit-text "Thank you: your {units} are in the pot, for everyone's builds.")
                                                                           (unit-text "Thank you: your {units} are in.")))
                                                                      ((equal outcome "already") "That payment was already credited.")
                                                                      ((equal outcome "unpaid") "The payment has not completed yet; it is credited when it does.")
                                                                      (t (format nil "The payment could not be confirmed: ~a" text))))
                                                    s))))
                 (the (set-slot! :revision (1+ (the revision)))))))))

   (topup
    (signals)
    ;; modeling credits by card: the human check, then the payment in the
    ;; page (start-payment!)
    (let ((address (client-address *datastar-request*))
          (amount (gethash "amount" signals))
          (token (gethash "turnstile" signals)))
      (cond ((and (the session) (not (the owner?)))
             (the (tell-error! (unit-text "This session is someone else's; start your own to add {units}."))))
            ((not (integerp amount)) (the (tell-error! "Say how much.")))
            ;; what the pot can hold is said only here, to someone whose
            ;; top-up would overfill it
            ((let* ((pot (pot))
                    (room (and pot (getf pot :room)))
                    (bought (and room (ignore-errors (credits-for-amount amount)))))
               (when (and (realp room) (realp bought) (> bought room))
                 (the (tell-error! (format nil "The community pot has room for ~:d more ~a just now, and that would add ~:d.  Try a smaller amount, or come back when some have been spent~@[; to build without sharing a pot, ~a~]."
                                           (max 0 (floor room)) (units) (round bought)
                                           (and *own-lab* (cdr *own-lab*)))))
                 t)))
            ((the (own-session! address (gethash "wallet" signals) (eq (gethash "closedPick" signals) t)))
             (let ((session (the session)))
               (multiple-value-bind (ok? reason) (topup-check! session token address)
                 (the spent-token!)
                 (if (not ok?)
                     (the (tell-error! reason))
                     ;; the card line where the gate offers it
                     (the (start-payment! session amount :card? t)))))))))))


(defun browse-links ()
  "String of HTML: the links to the listings, where sessions are listed."
  (with-lhtml-string ()
    (when *browsing?*
      (htm (:a :href (format nil "~a/sheet-list?browse=live" *url-prefix*) "sessions")
           (:a :href (format nil "~a/sheet-list?browse=archive" *url-prefix*) "archive")))))

(defun skin-script ()
  "JavaScript: the listing page wears the skin this browser chose
(localStorage prompt-lab-skin, which the page keeps too), else ?skin=,
else the house look."
  (format nil "(function(){var skins=~a,house=~a,aliases=~a;
var l=document.getElementById('pl-skin-link');
function name(n){return aliases[n]||n}
var q=null;try{q=new URLSearchParams(location.search).get('skin')}catch(e){}
var s=null;try{s=localStorage.getItem('prompt-lab-skin')}catch(e){}
var n=name(q||s||house);if(skins[n])l.setAttribute('href',skins[n])})();"
          (with-output-to-string (s)
            (yason:encode (alexandria:alist-hash-table
                           (mapcar #'(lambda (skin) (cons (getf skin :name) (getf skin :href))) (skins))
                           :test #'equal)
                          s))
          (js-string-literal *house-skin*)
          (with-output-to-string (s)
            (yason:encode (alexandria:alist-hash-table *skin-aliases* :test #'equal) s))))

;;
;; The listings (browse.lisp's own summaries), a page of their own beside
;; the sheet: the live sessions, or every archived one with its
;; thumbnail.  A session goes to its sheet when it is live here, to the
;; page's archive view when it is not, and to the other lab when that one
;; built it.
;;
(define-object lab-listing (session-control-mixin base-html-page)
  :computed-slots
  ((title (format nil "~a: sessions" (lab-title)))
   (use-x3dom? nil)
   (browse (let ((b (cdr (assoc "browse" (the query-toplevel) :test #'string-equal))))
             (if (equal b "archive") "archive" "live")))
   (additional-header-content
    (with-lhtml-string ()
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:link :rel "stylesheet" :href (tokens-url))
      (:style (str *sheet-css*))
      (:link :id "pl-skin-link" :rel "stylesheet")
      (:script (str (skin-script)))
      (:style "body{margin:0;font-family:var(--pl-font);background:var(--pl-bg);color:var(--pl-ink)}")))
   (body
    (let* ((archive? (equal (the browse) "archive"))
           (summaries (and *browsing?*
                           (newest-first (if archive? (archive-summaries) (live-summaries))))))
      (with-lhtml-string ()
        (:div :id "pl-sheet" :class "pl-listing"
              (:header :class "pl-head"
                       (:h1 (esc (lab-title)))
                       (:span :class "pl-engine" (esc (engine-label)))
                       (:nav (str (browse-links))
                             (:a :href *url-prefix* "start your own")))
              (:main :class "pl-list"
                     (:h2 (str (if archive? "Archived sessions" "Live sessions")))
                     (cond ((not *browsing?*)
                            (htm (:p :class "pl-line" "Sessions are not listed on this lab.")))
                           ((null summaries)
                            (htm (:p :class "pl-line" "Nothing to list yet.")))
                           (t
                            (dolist (s summaries)
                              (str (listing-entry s archive?))))))))))))

(defun classic-door (req ent)
  "GET <prefix>/classic: the classic page was retired on 2026-10-02; an old
link to it (a session or an archive entry in its query) comes to the sheet."
  (let ((query (net.aserve:request-query req :post nil)))
    (net.aserve:with-http-response (req ent :response net.aserve:*response-moved-permanently*)
      (setf (net.aserve:reply-header-slot-value req :location)
            (if query
                (format nil "~a?~a" *url-prefix* (net.aserve:query-to-form-urlencoded query))
                *url-prefix*))
      (net.aserve:with-http-body (req ent)))))

(defun publish-lab-sheet! (&key host)
  "THE PAGE: publish the sheet at <prefix>, and at <prefix>/sheet, its
address before it took the prefix; its listings at <prefix>/sheet-list;
the manifest (app.lisp) also at <prefix>/sheet-manifest.webmanifest, which
installed apps may still name; and <prefix>/classic, the retired classic
page's address, sending to the sheet.  Call after publish-prompt-lab!."
  (gwl::publish-gwl-app *url-prefix* 'lab-sheet :host host)
  (gwl::publish-gwl-app (concatenate 'string *url-prefix* "/sheet") 'lab-sheet :host host)
  (gwl::publish-gwl-app (format nil "~a/sheet-list" *url-prefix*) 'lab-listing :host host)
  (gwl:with-all-servers (server)
    (net.aserve:publish :path (format nil "~a/classic" *url-prefix*) :server server :host host
                        :function #'classic-door)
    (net.aserve:publish :path (format nil "~a/sheet-manifest.webmanifest" *url-prefix*)
                        :server server :host host :function #'manifest-door)))
