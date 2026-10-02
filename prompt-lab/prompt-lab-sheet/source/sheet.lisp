;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

;;;;
;;;; The lab as one gwl sheet, at <prefix>/sheet: THE SLUICE, opened on
;;;; the session's model, with the lab's own sections as its tiles.
;;;;
;;;; The same sessions, agent, guards and archive as the page at
;;;; <prefix>; what differs is the page.  There the browser polls a JSON
;;;; door every two seconds and redraws by hand; here each part of the
;;;; page is a section, the sheet hears every change to its session
;;;; (*session-change-hooks*: each log entry, each save) and pushes the
;;;; sections that changed over its stream (gwl's datastar-mixin, which
;;;; the sluice carries).  The tree, the inspector with the model's
;;;; inputs, the panes (wireframe, shaded, several of them) and the
;;;; menus are the sluice's; the lab adds the prompt, status, credits,
;;;; downloads and log at the left and the model file under the panes.
;;;;
;;;; Ownership is the page's: the owner key is kept in the browser
;;;; (localStorage prompt-lab-owners, id -> key), so a session opened
;;;; here carries on at <prefix> and the other way round.  A sheet opened
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
  "When SESSION's model was last compiled, or nil when it has none."
  (and (model-defined? session)
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
                           (format nil "{\"busy\": ~a}" (json-boolean (session-busy? session)))))
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
  (flet ((buttons (offers room label)
           (with-output-to-string (out)
             (when (and offers (the-object sheet editable?))
               (format out "<div class=\"pl-topup\"><span>~a</span>" label)
               (dolist (offer offers)
                 (destructuring-bind (cents credits) offer
                   (let ((fits? (or (null room) (<= credits room))))
                     (format out "<button type=\"button\" class=\"pl-buy\"~:[ disabled~;~] data-indicator:paying=\"\" data-attr:disabled=\"~a\" data-on:click=\"~a\" title=\"~a\">~:d credits for ~a</button>"
                             fits?
                             (if fits?
                                 (format nil "$paying~@[ || !$turnstile~]" *turnstile-site-key*)
                                 "true")
                             (escape-string-minimal-plus-quotes
                              (format nil "$amount = ~d; ~a" cents
                                      (the-object sheet (datastar-action :topup :options "{filterSignals: {include: /^(amount|turnstile|owner|wallet)$/}}"))))
                             (if fits? "Pay by card on Stripe's page" "The pot has no room for that many")
                             credits (dollars cents)))))
               (format out "</div>")))))
    (cond
      (pot
       (let* ((held (max 0 (floor (or (getf pot :credits) 0))))
              (most (or (getf pot :max) 0))
              (room (max 0 (floor (or (getf pot :room) 0))))
              (offers (and (getf pot :topup?) (offers (getf pot :amounts) (getf pot :credits-sold))))
              (fit (remove-if-not #'(lambda (o) (<= (second o) room)) offers)))
         (with-output-to-string (out)
           (format out "<div class=\"pl-figure~:[~; pl-out~]\"><b>~:d</b> modeling credits left in the community pot</div>"
                   (zerop held) held)
           (when (plusp most)
             (format out "<div class=\"pl-meter\"><span style=\"width:~,1f%\"></span></div>"
                     (* 100 (/ (min held most) most))))
           (format out "<p class=\"pl-line\">it holds at most ~:d~@[ &middot; this session has drawn ~:d~]~@[ &middot; you have added ~:d~]</p>"
                   most
                   (let ((n (and spend (gethash "credits_used" spend)))) (and n (plusp n) n))
                   (let ((n (and spend (gethash "contributed" spend)))) (and n (plusp n) n)))
           (when (zerop held)
             (format out "<p class=\"pl-begging\">The pot is empty: nothing builds until someone tops it up.</p>"))
           (write-string (buttons offers room "Add to the pot:") out)
           (when (and offers (null fit))
             (format out "<p class=\"pl-line\">The pot is as full as it gets.~@[ To build without sharing one, <a href=\"~a\">~a</a>.~]</p>"
                     (car *own-lab*) (or (cdr *own-lab*) "run a lab of your own"))))))
      (spend
       (let* ((used (or (gethash "credits_used" spend) 0))
              (free (or (gethash "credits_free" spend) 0))
              (balance (gethash "credits_balance" spend))
              (offers (and (eq (gethash "topup" spend) t)
                           (offers (gethash "topup_amounts" spend) (gethash "topup_credits" spend)))))
         (with-output-to-string (out)
           (format out "<div class=\"pl-figure\"><b>~:d</b> modeling credits left</div>"
                   (+ (max 0 (- free used)) (or balance 0)))
           (format out "<p class=\"pl-line\">this session: ~:d of ~:d free credits~@[ &middot; balance ~:d~]</p>"
                   used free balance)
           (write-string (buttons offers nil "Buy:") out))))
      (t "<p class=\"pl-line\">Credits show here once you build.</p>"))))

(defun listing-entry (summary archive?)
  "String of HTML: one session of a listing (a summary from browse.lisp)."
  (let* ((id (gethash "id" summary))
         (engine (gethash "engine" summary))
         (here? (equal engine (engine-name)))
         (live? (if archive? (live? id) t))
         (href (cond ((and here? live?) (format nil "~a/sheet?session=~a" *url-prefix* id))
                     (here? (format nil "~a?archive=~a" *url-prefix* id))
                     ((car *sibling-lab*) (format nil "~a~:[?archive=~a~;/sheet?session=~a~]"
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
#pl-prompt{width:100%;box-sizing:border-box;font:inherit;padding:.4rem;background:var(--pl-panel,#fff);color:var(--pl-ink,#111);border:var(--pl-rule,1px) solid var(--pl-line-soft,#ddd);border-radius:var(--pl-radius,4px)}
.pl-row{display:flex;gap:.8rem;align-items:center;margin-top:.5rem}
.pl-build{padding:.35rem 1.2rem;font:inherit;background:var(--pl-accent,#366fc5);color:var(--pl-accent-ink,#fff);border:0;border-radius:var(--pl-radius,4px);cursor:pointer;font-weight:var(--pl-label-weight,700);text-transform:var(--pl-label-case)}
.pl-build:disabled{opacity:.4;cursor:default}.pl-busy{color:var(--pl-status-busy,#a60)}
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
.code-editor .tok-keyword{color:var(--pl-code-keyword,#6a1b9a);font-weight:700}
.code-editor .tok-heading{color:var(--pl-code-section,#00695c);font-weight:700}
.code-editor .tok-atom,.code-editor .tok-meta{color:var(--pl-code-atom,#1550a8)}
.code-editor .tok-number{color:var(--pl-code-number,#9a4a00)}
.code-editor .tok-string,.code-editor .tok-string2{color:var(--pl-code-string,#2a6e2f)}
.code-editor .tok-punctuation{color:var(--pl-code-paren,var(--pl-ink-dimmer))}.code-editor .tok-invalid{color:var(--pl-status-fail)}
.pl-buy,.pl-download,.pl-source-head button{font:inherit;font-size:.85em;padding:.2rem .6rem;border:var(--pl-rule,1px) solid var(--pl-line,#ccc);border-radius:var(--pl-radius,4px);background:var(--pl-panel,#fff);color:var(--pl-link,#1550a8);cursor:pointer}
.pl-buy:disabled{opacity:.4;cursor:default}
.pl-downloads .pl-download{margin:0 .3rem .3rem 0}
.pl-figure{font-size:.9em;color:var(--pl-ink-dim,#555)}.pl-figure b{font-size:1.8rem;color:var(--pl-ink,#111);margin-right:.3rem;font-variant-numeric:tabular-nums}
.pl-figure.pl-out b{color:var(--pl-status-fail,#b00)}
.pl-meter{height:.45rem;background:var(--pl-panel-alt,#eee);border:var(--pl-rule,1px) solid var(--pl-line-soft,#ddd);border-radius:var(--pl-radius-pill,9px);overflow:hidden;margin:.3rem 0}
.pl-meter span{display:block;height:100%;background:var(--pl-accent,#366fc5)}
.pl-line{color:var(--pl-ink-dim,#555);font-size:.9em;margin:.3rem 0}.pl-begging{color:var(--pl-status-fail,#b00);font-weight:600;margin:.3rem 0}
.pl-topup{display:flex;flex-wrap:wrap;gap:.4rem;align-items:center;margin-top:.4rem;font-size:.9em}
.pl-private{display:inline-flex;gap:.3rem;align-items:center;cursor:pointer}
.pl-private input{accent-color:var(--pl-accent,#366fc5)}
.pl-list{max-width:60rem;margin:0 auto;padding:1rem}
.pl-list h2{font-family:var(--pl-font-label);font-weight:var(--pl-label-weight);text-transform:var(--pl-label-case);letter-spacing:var(--pl-label-tracking)}
.pl-entry-card{display:grid;grid-template-columns:auto 1fr;grid-template-rows:auto auto;column-gap:.8rem;align-items:center;padding:.6rem .7rem;margin-bottom:.5rem;background:var(--pl-panel);border:var(--pl-rule) solid var(--pl-line);border-radius:var(--pl-radius);color:var(--pl-ink);text-decoration:none}
.pl-entry-card:hover{background:var(--pl-hover-bg);outline:var(--pl-rule) solid var(--pl-hover-line)}
.pl-thumb{grid-row:1/3;width:96px;height:72px;object-fit:contain;background:#fff;border:var(--pl-rule) solid var(--pl-line-soft)}
.pl-entry-title{font-weight:600;overflow-wrap:anywhere;font-size:.95em}
@media (max-width:47.99rem){.pl-head{padding:.3rem .6rem;gap:.5rem;flex-wrap:wrap}.pl-head .pl-engine{display:none}.pl-head nav{gap:.5rem;font-size:.8em}
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
window.plDownload=function(url,owner){var n=document.getElementById('pl-download-note');function say(t){if(n)n.textContent=t||''}
 say('Making the file...');
 fetch(url,{headers:owner?{'X-Prompt-Lab-Owner':owner}:{}}).then(function(r){
  if(!r.ok)return r.json().then(function(j){say(j.error||('The file could not be made ('+r.status+').'))},function(){say('The file could not be made ('+r.status+').')});
  var cd=r.headers.get('Content-Disposition')||'',m=/filename=\"?([^\";]+)/.exec(cd),left=r.headers.get('X-Prompt-Lab-Note');
  return r.blob().then(function(b){var a=document.createElement('a');a.href=URL.createObjectURL(b);a.download=m?m[1]:'model';
   document.body.appendChild(a);a.click();setTimeout(function(){URL.revokeObjectURL(a.href);a.remove()},1000);say(left||'')})
 }).catch(function(e){say('The file could not be fetched: '+e)})};
if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',mount);else mount();
})();"
  "String. The model file's editor on the sheet: the lab's CodeMirror bundle
(static/editor.js) mounted over a textarea that no section contains, so
nothing the stream sends redraws it.  New versions come as plSetSource
calls; an edit in progress keeps its text and says the file changed.")




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
    private-id nil :settable))

  :computed-slots
  (;; the sluice's inputs
   (title (lab-title))
   (audience :public)
   (tiles (list (list :object (the prompt-tile) :place :left :tab "Prompt")
                (list :object (the status-section) :place :left :tab "Prompt")
                (list :object (the credits-section) :place :left :tab "Prompt")
                (list :object (the downloads-section) :place :left :tab "Prompt")
                (list :object (the log-section) :place :left :tab "Prompt")
                (list :object (the editor-tile) :place :under-panes :tab "Code")))
   (tabs (list "Prompt" "Model" "Parts" "Code"))
   (head-html (the lab-head-html))
   (body-html (the lab-body-html))
   ;; on a phone the inspector holds the inputs alone
   (user-mode?-default nil)

   ;; the lab's own Datastar actions (gwl's allowlist for /gdlAction)
   (datastar-actions (list :build :claim :save :topup :privacy :wear-skin))

   (owner? (let ((session (the session)))
             (and session (the owner-key) (owner? session (the owner-key)) t)))

   ;; a new visitor builds into a session of their own; a session's
   ;; page is the owner's to build in
   (editable? (or (and (null (the session)) (null (the private-id))) (the owner?)))

   ;; the session the address names: the one shown, or a private one
   ;; waiting for its owner's key
   (shown-id (or (and (the session) (session-id (the session))) (the private-id)))

   (lab-head-html
    (with-lhtml-string ()
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (when *turnstile-site-key*
        (htm (:script :src "https://challenges.cloudflare.com/turnstile/v0/api.js" :async "async" :defer "defer")
             (:script "window.plTurnstile=function(t){var e=document.getElementById('pl-turnstile');if(e){e.value=t;e.dispatchEvent(new Event('input',{bubbles:true}))}};")))
      (:script :defer "defer" :src (static-url "editor.js"))
      (:script (str *sheet-editor-script*))
      (:style (str *sheet-css*))))

   ;; Written with the page, never redrawn: the lab's header, the page's
   ;; signals, the claim, and the phone's turn to the model when a build
   ;; is done.  What changes is in the tiles.
   (lab-body-html
    (with-lhtml-string ()
      (:div :id "pl-sheet" :style "display:contents"
            :|data-signals| (escape-string-minimal-plus-quotes (the initial-signals))
            ;; a browser holding this session's key makes the page its owner's
            (when (and (the shown-id) (not (the owner?)))
              (htm (:span :|data-init| (format nil "$owner && ~a" (the (datastar-action :claim))))))
            (:span :|data-effect| "var b=$busy; if(window.plWasBusy && !b && window.sluiceTab && matchMedia('(max-width: 47.99rem)').matches){sluiceTab('model')} window.plWasBusy=b")
            ;; the skin this browser chose, shared with the classic page
            ;; (localStorage prompt-lab-skin): worn when it is not the one
            ;; showing, and kept when View > Skin picks another
            (:script "window.sluiceSkinChosen=function(n){try{localStorage.setItem('prompt-lab-skin',n)}catch(e){}};")
            (:span :|data-init|
                   ;; cl-who writes attribute values raw, between quotes
                   (escape-string-minimal-plus-quotes
                    (format nil "$skinPref = (function(){try{return (localStorage.getItem('prompt-lab-skin')||'').toLowerCase()}catch(e){return ''}})(); $skinPref && $skinPref !== ~a && ~a"
                            (js-string-literal (sluice:skin-name (the skin)))
                            (the (datastar-action :wear-skin :options "{filterSignals: {include: /^skinPref$/}}")))))
            (:header :class "pl-head"
                     (:h1 (esc (lab-title)))
                     (:span :class "pl-engine" (esc (engine-label)))
                     (:nav (str (browse-links))
                           (:a :href (the classic-url) "the page")
                           (when *sibling-lab*
                             (htm (:a :href (format nil "~a/sheet" (car *sibling-lab*)) (esc (cdr *sibling-lab*))))))))))

   ;; back from Stripe the address carries the checkout and the wallet;
   ;; the wallet otherwise comes from where the page keeps it
   (query-checkout (let ((c (cdr (assoc "checkout" (the query-toplevel) :test #'string-equal)))) (and (stringp c) c)))
   (query-wallet (let ((w (cdr (assoc "wallet" (the query-toplevel) :test #'string-equal)))) (and (wallet-id? w) w)))
   (cancelled? (equal (cdr (assoc "topup" (the query-toplevel) :test #'string-equal)) "cancelled"))

   (initial-signals
    (format nil "{live: '', skinPref: '', private: false, prompt: '', turnstile: '', source: '', amount: 0, error: '', notice: ~a, sending: false, saving: false, paying: false, busy: ~a, editable: ~a, owner: ~a, checkout: ~a, wallet: ~a}"
            (if (the cancelled?) "'The payment was cancelled; nothing was charged.'" "''")
            (json-boolean (and (the session) (session-busy? (the session))))
            ;; a browser holding the session's key shows the owner's page
            ;; from the start (the claim confirms it; every action checks
            ;; the key again), rather than a watcher's for the first second
            (if (and (the shown-id) (not (the editable?)))
                (format nil "(~a !== '')" (owner-signal-expression (the shown-id)))
                (json-boolean (the editable?)))
            (if (the shown-id) (owner-signal-expression (the shown-id)) "''")
            (js-string-literal (or (the query-checkout) ""))
            (if (the query-wallet)
                (js-string-literal (the query-wallet))
                "(function(){try{return localStorage.getItem('prompt-lab-wallet')||''}catch(e){return ''}})()")))

   (classic-url (if (the session)
                    (format nil "~a?session=~a" *url-prefix* (session-id (the session)))
                    *url-prefix*)))

  :objects
  (;; The prompt, as a tile that never goes stale (it reads nothing that
   ;; changes): what is typed stays.  What it shows changes through
   ;; signals.
   (prompt-tile
    :type 'base-html-div
    :inner-html (with-lhtml-string ()
                  (:div :class "pl-card"
                        (:div :|data-show| "$editable"
                              (:textarea :id "pl-prompt" :rows "4" :|data-bind:prompt| ""
                                         :placeholder "Describe what to build, e.g. a picnic table with two benches")
                              (when *turnstile-site-key*
                                (htm (:div :class "cf-turnstile" :data-sitekey *turnstile-site-key* :data-callback "plTurnstile")))
                              (:input :type "hidden" :id "pl-turnstile" :|data-bind:turnstile| "")
                              (:div :class "pl-row"
                                    (:button :class "pl-build"
                                             :|data-on:click| (the (datastar-action :build :options "{filterSignals: {include: /^(prompt|turnstile|owner|wallet)$/}}"))
                                             :|data-indicator:sending| ""
                                             ;; with a human check, a build waits for its token
                                             :|data-attr:disabled| (format nil "$busy || $sending || !$prompt.trim()~@[ || !$turnstile~]"
                                                                           *turnstile-site-key*)
                                             "Build")
                                    (:span :class "pl-busy" :|data-show| "$busy" "the agent is working...")))
                        ;; hidden until Datastar has read the signals: no
                        ;; banner flashes at an owner while the script loads
                        (:div :class "pl-watch" :style "display:none" :|data-show| "!$editable"
                              "You are watching this session as it is built.  "
                              (:a :href (format nil "~a/sheet" *url-prefix*) "Start your own") ".")
                        (:p :class "pl-notice" :|data-show| "$notice" :|data-text| "$notice")
                        (:p :class "pl-error" :|data-show| "$error" :|data-text| "$error"))))

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
                                     (esc (or (and session (ignore-errors (model-body session))) "")))))))

   (status-section
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let ((session (the session)))
                    (with-lhtml-string ()
                      (:div :class "pl-card pl-status"
                            (cond
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
                                         (htm (:span (fmt "pot ~:d credits" (max 0 (floor (or (getf pot :credits) 0)))))))
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

   (log-section
    :type 'base-html-div
    :inner-html (progn
                  (the revision)
                  (let* ((session (the session))
                         (log (and session (session-log session)))
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
                            (:h2 "Modeling credits")
                            (str (credits-html self (pot) spend)))))))

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
                                                          (format nil "~a?session=~a&format=~a"
                                                                  (door-path "download") (session-id session) (first entry)))))
                                                (esc (fourth entry)))))
                                (htm (:p :class "pl-line" "The model's drawings and files, once it is built.")))
                            (:p :id "pl-download-note" :class "pl-line")))))))

  :functions
  ((set-instantiation-time!
    ()
    (call-next-method)
    ;; ?session=<id>: the sheet shows that session to whoever may see it
    ;; (a private one only to its owner: the claim opens it, once the
    ;; browser has shown the key)
    (let* ((id (cdr (assoc "session" (the query-toplevel) :test #'string-equal)))
           (session (and (stringp id) (find-session id))))
      (cond ((null session))
            ((visible-to? session nil) (the (show-session! session)))
            (t (the (set-slot! :private-id (session-id session)))))))

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
    (let ((session (the session)))
      (when (and session (model-defined? session)
                 (not (eq (the root-object-type) (model-symbol session))))
        (the (set-slot! :root-object-type (model-symbol session)))
        ;; hidden lines removed up to *hidden-lines-max-leaves* leaves
        ;; (quadratic in the edges; View > Hidden Lines turns it on for
        ;; a larger model), and the model's leaves drawn
        (when (and (the root-object)
                   (<= (or (ignore-errors (length (the root-object leaves))) 0)
                       *hidden-lines-max-leaves*))
          (ignore-errors (the viewport (set-slot! :hidden-lines :remove))))
        (when (the root-object)
          (ignore-errors (the viewport (draw-leaves! (the root-object))))))))

   (wear-skin
    (signals)
    ;; (not `skin': that is the sluice's input of the name, and GDL keys
    ;; messages by name) the skin this browser chose on the classic page or here, worn in
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
        (sheet-send! self (datastar-signals-event "{\"editable\": true}"))
        (when (and (stringp checkout) (plusp (length checkout)) (wallet-id? wallet))
          (multiple-value-bind (outcome text) (confirm-topup! session wallet checkout)
            (sheet-send! self
                         (datastar-signals-event (with-output-to-string (s)
                                                   (yason:encode (h "checkout" "" "notice" text) s)))
                         (datastar-script-event
                          (format nil "try{localStorage.setItem('prompt-lab-wallet',~a)}catch(e){};history.replaceState(null,'',~a)"
                                  (js-string-literal wallet)
                                  (js-string-literal (format nil "~a/sheet?session=~a" *url-prefix* (session-id session))))))
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
    (address wallet)
    ;; the session of a visitor who has none yet, opened for this sheet;
    ;; nil with the refusal shown when the address is over its cap
    (or (the session)
        (multiple-value-bind (session reason) (open-session! address (and (wallet-id? wallet) wallet))
          (cond (session
                 (the (set-slot! :session session))
                 (the (set-slot! :owner-key (session-owner session)))
                 (watch-session! self session)
                 (sheet-send! self (datastar-script-event (owners-script session)))
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
            ((the (own-session! address (gethash "wallet" signals)))
             (multiple-value-bind (started reason) (begin-prompt! (the session) prompt address token)
               (if started
                   (sheet-send! self (datastar-signals-event "{\"error\": \"\", \"prompt\": \"\"}"))
                   (the (tell-error! reason)))
               (the spent-token!))))))

   (topup
    (signals)
    ;; modeling credits by card: the human check, then Stripe's hosted
    ;; Checkout, which comes back to this sheet with the checkout and the
    ;; wallet on the address (claim confirms it)
    (let ((address (client-address *datastar-request*))
          (amount (gethash "amount" signals))
          (token (gethash "turnstile" signals)))
      (cond ((and (the session) (not (the owner?)))
             (the (tell-error! "This session is someone else's; start your own to add credits.")))
            ((not (integerp amount)) (the (tell-error! "Say how much.")))
            ((the (own-session! address (gethash "wallet" signals)))
             (let ((session (the session)))
               (multiple-value-bind (ok? reason) (topup-check! session token address)
                 (the spent-token!)
                 (if (not ok?)
                     (the (tell-error! reason))
                     (multiple-value-bind (answer reason)
                         (begin-topup! session amount (page-url *datastar-request* session :path "/sheet"))
                       (if (and answer (stringp (gethash "url" answer)))
                           (sheet-send! self (datastar-script-event
                                              (format nil "try{localStorage.setItem('prompt-lab-wallet',~a)}catch(e){};location.href=~a"
                                                      (js-string-literal (or (session-wallet session) ""))
                                                      (js-string-literal (gethash "url" answer)))))
                           (the (tell-error! (or reason "The top-up could not be started."))))))))))))))


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
                             (:a :href (format nil "~a/sheet" *url-prefix*) "start your own")))
              (:main :class "pl-list"
                     (:h2 (str (if archive? "Archived sessions" "Live sessions")))
                     (cond ((not *browsing?*)
                            (htm (:p :class "pl-line" "Sessions are not listed on this lab.")))
                           ((null summaries)
                            (htm (:p :class "pl-line" "Nothing to list yet.")))
                           (t
                            (dolist (s summaries)
                              (str (listing-entry s archive?))))))))))))

(defun publish-lab-sheet! (&key host)
  "Publish the sheet at <prefix>/sheet, and its listings at
<prefix>/sheet-list, beside the page."
  (gwl::publish-gwl-app (format nil "~a/sheet" *url-prefix*) 'lab-sheet :host host)
  (gwl::publish-gwl-app (format nil "~a/sheet-list" *url-prefix*) 'lab-listing :host host))
