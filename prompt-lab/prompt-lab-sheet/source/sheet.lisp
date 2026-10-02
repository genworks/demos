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
           (the-object sheet (set-slot! :model-stamp stamp))))
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

(defparameter *tree-max-nodes* 400
  "Integer. The most parts the sheet's tree lists; a larger model shows
the first ones and says how many are left out.")

(defun tree-label (object)
  "String. What the tree calls OBJECT: its name in its parent, with its
index in a sequence; `model' for the root."
  (let ((step (first (ignore-errors (the-object object root-path)))))
    (cond ((null step) "model")
          ((consp step) (format nil "~(~a~)[~{~a~^,~}]" (first step) (rest step)))
          (t (format nil "~(~a~)" step)))))

(defun tree-more-html (out sheet path depth left)
  "Writes to OUT the row standing for the LEFT children of the part at PATH
that the tree does not list yet: a click lists *tree-condense-limit* more,
a shift-click all of them."
  (format out "<div class=\"tree-more\" style=\"padding-left:~,1frem\" title=\"~a\" data-on:click=\"~a\">&hellip; ~:d more</div>"
          (* 0.9 depth)
          (format nil "Click for ~d more, shift-click for all of them"
                  (min left sluice::*tree-condense-limit*))
          (escape-string-minimal-plus-quotes
           (format nil "$more = ~a; $moreAll = evt.shiftKey; ~a"
                   (js-string-literal (let ((*package* (find-package :keyword)))
                                        (format nil "~s" path)))
                   (the-object sheet (datastar-action :more :options "{filterSignals: {include: /^more/}}"))))
          left))

(defun tree-html (sheet model inspected &optional shown)
  "String of HTML: MODEL's parts as an indented outline, each a node that
carries its data-root-path (as the drawing's paths do, for the hover)
and inspects itself when clicked.  Each part lists its first
sluice::*tree-condense-limit* children, or as many as SHOWN (an alist
from a part's root-path to a count) says, and a \"...\" row for the rest."
  (let ((count 0) (left-out 0))
    (with-output-to-string (out)
      (labels ((walk (object depth)
                 (if (>= count *tree-max-nodes*)
                     (incf left-out)
                     (let ((path (ignore-errors (the-object object root-path))))
                       (incf count)
                       (format out "<div class=\"tree-node~:[~; pl-inspected~]\" style=\"padding-left:~,1frem\" data-root-path=\"~a\" data-on:click=\"~a\">~a<span class=\"tree-type\">~(~a~)</span></div>"
                               (eq object inspected)
                               (* 0.9 depth)
                               (escape-string-minimal-plus-quotes
                                (or (ignore-errors (gwl:root-path-reference object)) ""))
                               (escape-string-minimal-plus-quotes
                                (format nil "$inspect = ~a; ~a"
                                        (js-string-literal (let ((*package* (find-package :keyword)))
                                                             (format nil "~s" path)))
                                        (the-object sheet (datastar-action :inspect :options "{filterSignals: {include: /^inspect$/}}"))))
                               (escape-string-minimal-plus-quotes (tree-label object))
                               (type-of object))
                       (let* ((children (ignore-errors (the-object object children)))
                              (total (length children))
                              (limit (min total (or (cdr (assoc path shown :test #'equal))
                                                    sluice::*tree-condense-limit*))))
                         (loop for child in children
                               repeat limit
                               do (walk child (1+ depth)))
                         (when (< limit total)
                           (tree-more-html out sheet path (1+ depth) (- total limit))))))))
        (walk model 0))
      (when (plusp left-out)
        (format out "<p class=\"tree-type\">~:d more parts not listed.</p>" left-out)))))


;;
;; The sheet.
;;

(defparameter *sheet-css*
  "body{margin:0;font-family:var(--pl-font);font-size:var(--pl-size);line-height:1.45;background:var(--pl-bg);color:var(--pl-ink)}
a{color:var(--pl-link)}
.pl-head{display:flex;gap:1rem;align-items:center;padding:.4rem 1rem;background:var(--pl-label-bg);color:var(--pl-label-ink)}
.pl-head h1{font-size:1.05rem;margin:0;font-family:var(--pl-font-label);font-weight:var(--pl-label-weight);text-transform:var(--pl-label-case);letter-spacing:var(--pl-label-tracking)}
.pl-head a,.pl-engine{color:var(--pl-label-ink);opacity:.85}
.pl-head nav{margin-left:auto;display:flex;gap:.8rem;align-items:center}
.pl-head select{font:inherit;font-size:.9em;background:var(--pl-panel);color:var(--pl-ink);border:var(--pl-rule) solid var(--pl-line-soft);border-radius:var(--pl-radius)}
.pl-grid{display:grid;grid-template-columns:minmax(18rem,2fr) minmax(0,3fr);gap:.8rem;padding:.8rem}
.pl-left,.pl-right{min-width:0}
.pl-inputs .bg-white,.pl-inputs .bg-gray-50,.pl-viewport .bg-white\\/80{background-color:var(--pl-panel)}
.pl-inputs .bg-gray-100{background-color:var(--pl-bg)}.pl-inputs .bg-gray-200{background-color:var(--pl-panel-alt)}
.pl-inputs .text-gray-900,.pl-inputs .text-gray-700{color:var(--pl-ink)}
.pl-inputs .text-gray-600,.pl-inputs .text-gray-500{color:var(--pl-ink-dim)}.pl-inputs .text-gray-400{color:var(--pl-ink-dimmer)}
.pl-inputs .text-red-700{color:var(--pl-status-fail)}
.pl-inputs .text-blue-700,.pl-inputs .text-blue-800{color:var(--pl-link);text-decoration:underline;text-underline-offset:2px}
.pl-inputs .border-gray-300,.pl-viewport .border-gray-300{border-color:var(--pl-line)}.pl-inputs .border-gray-200{border-color:var(--pl-line-soft)}
.pl-inputs .rounded,.pl-viewport .rounded-md{border-radius:var(--pl-radius)}
.pl-inputs input:not([type=checkbox]):not([type=range]),.pl-inputs select{border:var(--pl-rule) solid var(--pl-line);border-radius:var(--pl-radius);background:var(--pl-panel);color:var(--pl-ink);font-family:var(--pl-font-mono)}
.pl-inputs input:focus,.pl-inputs select:focus{border-color:var(--pl-focus);outline:var(--pl-rule) solid var(--pl-focus)}
.pl-inputs input[type=checkbox]{accent-color:var(--pl-accent)}
.pl-viewport button[id$=-reset-size]{background:var(--pl-panel);color:var(--pl-ink);border:var(--pl-rule) solid var(--pl-line);box-shadow:none}
.pl-card{background:var(--pl-panel);border:var(--pl-rule) solid var(--pl-line);border-radius:var(--pl-radius);box-shadow:var(--pl-shadow);padding:.7rem;margin-bottom:.8rem}
.pl-card h2{font-size:.9rem;margin:0 0 .4rem;font-family:var(--pl-font-label);font-weight:var(--pl-label-weight);text-transform:var(--pl-label-case);letter-spacing:var(--pl-label-tracking)}
#pl-prompt{width:100%;box-sizing:border-box;font:inherit;padding:.4rem;background:var(--pl-panel);color:var(--pl-ink);border:var(--pl-rule) solid var(--pl-line-soft);border-radius:var(--pl-radius)}
.pl-row{display:flex;gap:.8rem;align-items:center;margin-top:.5rem}
.pl-build{padding:.35rem 1.2rem;font:inherit;background:var(--pl-accent);color:var(--pl-accent-ink);border:0;border-radius:var(--pl-radius);cursor:pointer;font-weight:var(--pl-label-weight);text-transform:var(--pl-label-case)}
.pl-build:disabled{opacity:.4;cursor:default}.pl-busy{color:var(--pl-status-busy)}
.pl-error{color:var(--pl-status-fail);margin:0 0 .8rem}.pl-notice{color:var(--pl-status-pass);margin:0 0 .8rem}
.pl-status{display:flex;flex-wrap:wrap;gap:.3rem 1rem;color:var(--pl-ink-dim)}
.pl-log{max-height:55vh;overflow:auto}
.pl-entry{display:grid;grid-template-columns:5.5rem 1fr;gap:.5rem;padding:.2rem 0;border-bottom:var(--pl-rule) solid var(--pl-line-soft)}
.pl-kind{color:var(--pl-ink-dimmer);font-size:.8rem}.pl-text{white-space:pre-wrap;word-break:break-word}
.pl-prompt .pl-text{font-weight:600}.pl-done .pl-text{color:var(--pl-status-pass)}
.pl-stopped .pl-text,.pl-tool-error .pl-text{color:var(--pl-status-fail)}
.pl-tool .pl-text{color:var(--pl-ink-dim);font-family:var(--pl-font-mono);font-size:.8rem}
.pl-viewport{position:relative;height:60vh;background-color:var(--pl-viewport-bg);background-image:var(--pl-viewport-image);background-size:var(--pl-viewport-image-size);border:var(--pl-rule) solid var(--pl-line);border-radius:var(--pl-radius);margin-bottom:.8rem;overflow:hidden}
.pl-viewport svg{filter:var(--pl-viewport-filter)}
.pl-source-head{display:flex;gap:.6rem;align-items:baseline;margin-bottom:.4rem}.pl-source-head h2{margin:0}
.pl-source-state{color:var(--pl-ink-dimmer);font-size:.8rem}.pl-source-state.pl-warn{color:var(--pl-status-fail)}
.pl-source-head .pl-save{margin-left:auto}
#pl-source{width:100%;box-sizing:border-box;min-height:16rem;font:12px/1.4 var(--pl-font-mono)}
.pl-source .cm-editor{max-height:45vh;font-size:12px;font-family:var(--pl-font-mono);background:var(--pl-panel);color:var(--pl-ink);border:var(--pl-rule) solid var(--pl-line-soft)}
.pl-source .cm-editor.cm-focused{outline:none;border-color:var(--pl-focus)}
.pl-source .cm-gutters{background:var(--pl-panel-alt);color:var(--pl-ink-dimmer);border-right:var(--pl-rule) solid var(--pl-line-soft)}
.pl-source .cm-cursor{border-left-color:var(--pl-ink)}
.code-editor .tok-comment{color:var(--pl-code-comment,var(--pl-ink-dimmer));font-style:italic}
.code-editor .tok-keyword{color:var(--pl-code-keyword,#6a1b9a);font-weight:700}
.code-editor .tok-heading{color:var(--pl-code-section,#00695c);font-weight:700}
.code-editor .tok-atom,.code-editor .tok-meta{color:var(--pl-code-atom,#1550a8)}
.code-editor .tok-number{color:var(--pl-code-number,#9a4a00)}
.code-editor .tok-string,.code-editor .tok-string2{color:var(--pl-code-string,#2a6e2f)}
.code-editor .tok-punctuation{color:var(--pl-code-paren,var(--pl-ink-dimmer))}.code-editor .tok-invalid{color:var(--pl-status-fail)}
.pl-parts{display:grid;grid-template-columns:minmax(10rem,1fr) minmax(14rem,1.4fr);gap:.8rem}
@media (max-width:1100px){.pl-parts{grid-template-columns:1fr}}
#tree{max-height:40vh;overflow:auto;font-size:.9em}
#tree .tree-node{cursor:pointer;padding:.05rem .3rem;border:var(--pl-rule) solid transparent;border-radius:var(--pl-radius);white-space:nowrap}
#tree .tree-node:hover,#tree .tree-node.sluice-lit{background:var(--pl-hover-bg);border-color:var(--pl-hover-line)}
#tree .tree-node.pl-inspected{background:var(--pl-accent);color:var(--pl-accent-ink)}
#tree .tree-more{cursor:pointer;color:var(--pl-ink-dimmer);font-style:italic;padding:.05rem .3rem;white-space:nowrap}
#tree .tree-more:hover{color:var(--pl-ink)}
#tree .tree-type{color:var(--pl-ink-dimmer);margin-left:.4rem;font-size:.85em}
#tree .tree-node.pl-inspected .tree-type{color:var(--pl-accent-ink);opacity:.75}
#sluice-panes svg path.sluice-lit{stroke:var(--pl-focus)!important;stroke-width:3!important}
.pl-inputs{font-size:.9em;max-height:40vh;overflow:auto}
.pl-buy,.pl-download,.sluice-apply,.pl-source-head button{font:inherit;font-size:.9em;padding:.2rem .6rem;border:var(--pl-rule) solid var(--pl-line);border-radius:var(--pl-radius);background:var(--pl-panel);color:var(--pl-link);cursor:pointer}
.sluice-apply.sluice-pending{background:var(--pl-accent);color:var(--pl-accent-ink)}
.pl-buy:disabled{opacity:.4;cursor:default}
.pl-downloads .pl-download{margin:0 .3rem .3rem 0}
.pl-figure{font-size:.9em;color:var(--pl-ink-dim)}.pl-figure b{font-size:2rem;color:var(--pl-ink);margin-right:.3rem;font-variant-numeric:tabular-nums}
.pl-figure.pl-out b{color:var(--pl-status-fail)}
.pl-meter{height:.45rem;background:var(--pl-panel-alt);border:var(--pl-rule) solid var(--pl-line-soft);border-radius:var(--pl-radius-pill);overflow:hidden;margin:.3rem 0}
.pl-meter span{display:block;height:100%;background:var(--pl-accent)}
.pl-line{color:var(--pl-ink-dim);font-size:.9em;margin:.3rem 0}.pl-begging{color:var(--pl-status-fail);font-weight:600;margin:.3rem 0}
.pl-topup{display:flex;flex-wrap:wrap;gap:.4rem;align-items:center;margin-top:.4rem;font-size:.9em}
.pl-private{display:inline-flex;gap:.3rem;align-items:center;cursor:pointer}
.pl-private input{accent-color:var(--pl-accent)}
.pl-list{max-width:60rem;margin:0 auto;padding:1rem}
.pl-list h2{font-family:var(--pl-font-label);font-weight:var(--pl-label-weight);text-transform:var(--pl-label-case);letter-spacing:var(--pl-label-tracking)}
.pl-entry-card{display:grid;grid-template-columns:auto 1fr;grid-template-rows:auto auto;column-gap:.8rem;align-items:center;padding:.6rem .7rem;margin-bottom:.5rem;background:var(--pl-panel);border:var(--pl-rule) solid var(--pl-line);border-radius:var(--pl-radius);color:var(--pl-ink);text-decoration:none}
.pl-entry-card:hover{background:var(--pl-hover-bg);outline:var(--pl-rule) solid var(--pl-hover-line)}
.pl-thumb{grid-row:1/3;width:96px;height:72px;object-fit:contain;background:#fff;border:var(--pl-rule) solid var(--pl-line-soft)}
.pl-entry-title{font-weight:600;overflow-wrap:anywhere;font-size:.95em}
@media (max-width:760px){.pl-thumb{width:72px;height:54px}.pl-list{padding:.5rem}}
.pl-tabs{display:none}
@media (max-width:760px){
 body{font-size:15px}
 .pl-head{padding:.4rem .6rem;gap:.5rem;flex-wrap:wrap}.pl-head .pl-engine{display:none}
 .pl-head nav{gap:.5rem;font-size:.85em}
 .pl-tabs{display:flex;position:sticky;top:0;z-index:30;background:var(--pl-panel);border-bottom:var(--pl-rule) solid var(--pl-line)}
 .pl-tabs button{flex:1;padding:.65rem 0;font:inherit;background:none;border:0;color:var(--pl-ink-dim);font-weight:var(--pl-label-weight);text-transform:var(--pl-label-case)}
 .pl-tabs button.pl-tab-on{background:var(--pl-accent);color:var(--pl-accent-ink)}
 .pl-grid{display:block;padding:.5rem}
 #pl-sheet .pl-g{display:none}
 #pl-sheet[data-tab=prompt] .pl-g-prompt,#pl-sheet[data-tab=model] .pl-g-model,
 #pl-sheet[data-tab=parts] .pl-g-parts,#pl-sheet[data-tab=code] .pl-g-code{display:block}
 .pl-viewport{height:calc(100dvh - 9rem)}
 #pl-sheet:not([data-tab=model]) .pl-viewport{position:absolute;left:-300vw;top:0;width:calc(100vw - 1rem)}
 .pl-parts{grid-template-columns:1fr}
 #tree,.pl-inputs,.pl-log{max-height:none}
 .pl-source .cm-editor{max-height:calc(100dvh - 12rem)}
}"
  "String. The sheet's look, in the skin tokens (the sluice's tokens.css,
SKIN-API.md): a skin restyles the sheet as it does the page and the
sluice.  At 760px and under the sheet is an app of four tabs.")

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
    model-stamp nil :settable)
   ("The model file's text as the editor was last sent it (sheets-hear)."
    shown-source "" :settable)
   ("String or nil. The id of a private session named on the address, which
the sheet shows only once its owner's key is proven (claim)."
    private-id nil :settable)
   ("List. The root-path, from the model, of the node whose inputs the
panel shows; nil for the model itself."
    inspected-path nil :settable)
   ("Boolean. The sluice inputs panel's auto-apply switch, which the
panel sets on the page that holds it."
    inputs-auto-apply? t :settable)
   ("List. How many children the tree lists of each part whose \"...\" row
has been clicked: an alist from the part's root-path to a count."
    tree-shown nil :settable)
   (use-fontawesome? t)
   ;; the sheet draws in svg: base-html-page's x3dom (834 KB) is not wanted
   (use-x3dom? nil))

  :computed-slots
  ((title (lab-title))
   (datastar-actions (list :build :claim :save :inspect :more :topup :privacy))

   ;; the gdlAjax calls the page makes: the viewport's, and the inputs
   ;; panel's (a call with no function only sets its fields); any other
   ;; is refused, whatever a crafted request names
   (ajax-callable-functions (append '(:reset! :reset-all! (:set-slot! :open? :inputs-auto-apply?))
                                    gwl:*viewport-ajax-functions*))

   ;; the node the inputs panel is on: the inspected path followed from
   ;; the model, or the model when that path no longer leads anywhere (a
   ;; rebuild renamed or dropped the node)
   (inspected-node (let ((model (the model-object)))
                     (and model
                          (or (and (the inspected-path)
                                   (ignore-errors (the-object model (follow-root-path (the inspected-path)))))
                              model))))

   (owner? (let ((session (the session)))
             (and session (the owner-key) (owner? session (the owner-key)) t)))

   ;; a new visitor builds into a session of their own; a session's
   ;; page is the owner's to build in
   (editable? (or (and (null (the session)) (null (the private-id))) (the owner?)))

   ;; the session the address names: the one shown, or a private one
   ;; waiting for its owner's key
   (shown-id (or (and (the session) (session-id (the session))) (the private-id)))

   ;; ?browse=live or ?browse=archive: the listings instead of the lab
   (browse (let ((b (cdr (assoc "browse" (the query-toplevel) :test #'string-equal))))
             (and (member b '("live" "archive") :test #'equal) b)))

   ;; The model is a root of its own, as in the sluice (so the inputs
   ;; panel offers all its inputs), made afresh at every compile.
   ;; Dependencies are recorded only within one tree or between trees a
   ;; god-parent link joins (gendl same-tree?): without the link, an
   ;; input set in the model changed nothing on the page.  The model
   ;; names the sheet -- the direction every image honours.
   (model-object (progn (the model-stamp)
                        (let* ((session (the session))
                               (model (and session (model-defined? session)
                                           (ignore-errors (make-model session)))))
                          (when model (add-godparent model self))
                          model)))

   (leaf-count (let ((model (the model-object)))
                 (or (and model (ignore-errors (length (the-object model leaves)))) 0)))

   (additional-header-content
    (with-lhtml-string ()
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (str (the datastar-head-content))
      (when *turnstile-site-key*
        (htm (:script :src "https://challenges.cloudflare.com/turnstile/v0/api.js" :async "async" :defer "defer")
             (:script "window.plTurnstile=function(t){var e=document.getElementById('pl-turnstile');if(e){e.value=t;e.dispatchEvent(new Event('input',{bubbles:true}))}};")))
      (:script :defer "defer" :src (static-url "editor.js"))
      ;; the sluice's inputs panel wears the sluice's utility classes,
      ;; and its hover script lights a drawn part and its tree node together
      (:link :rel "stylesheet" :href "/static/sluice/css/sluice.css")
      (:script (str (or (ignore-errors (symbol-value (find-symbol "*HOVER-SCRIPT*" :sluice))) "")))
      (:script (str *sheet-editor-script*))
      ;; the look: the tokens, the sheet's own (which says the sluice's
      ;; utility classes in them, inside the panes that carry them: the
      ;; sluice's skinned.css is written for the sluice's own frame), then
      ;; the skin (skin-script picks it before the page is painted)
      (:link :rel "stylesheet" :href (tokens-url))
      (:style (str *sheet-css*))
      (:link :id "pl-skin-link" :rel "stylesheet")
      (:script (str (the skin-script)))))

   ;; The skin: ?skin= on the address, else the one this browser chose
   ;; (localStorage prompt-lab-skin, which the page keeps too), else the
   ;; house look.  Chosen in the header's menu; nothing reloads.
   (skin-choices (mapcar #'(lambda (skin) (list (getf skin :name) (getf skin :label) (getf skin :href)))
                         (skins)))

   (skin-script
    (format nil "(function(){var skins=~a,house=~a,aliases=~a;
var l=document.getElementById('pl-skin-link');
function name(n){return aliases[n]||n}
function wear(n){n=name(n);if(skins[n])l.setAttribute('href',skins[n]);else l.removeAttribute('href');window.plSkin=skins[n]?n:house}
var q=null;try{q=new URLSearchParams(location.search).get('skin')}catch(e){}
var s=null;try{s=localStorage.getItem('prompt-lab-skin')}catch(e){}
wear(q||s||house);
window.plSetSkin=function(n){try{localStorage.setItem('prompt-lab-skin',n)}catch(e){}wear(n)};
document.addEventListener('DOMContentLoaded',function(){var m=document.getElementById('pl-skin');if(m)m.value=window.plSkin})})();"
            (with-output-to-string (s)
              (yason:encode (alexandria:alist-hash-table
                             (mapcar #'(lambda (c) (cons (first c) (third c))) (the skin-choices))
                             :test #'equal)
                            s))
            (js-string-literal *house-skin*)
            (with-output-to-string (s)
              (yason:encode (alexandria:alist-hash-table *skin-aliases* :test #'equal) s))))

   ;; back from Stripe the address carries the checkout and the wallet;
   ;; the wallet otherwise comes from where the page keeps it
   (query-checkout (let ((c (cdr (assoc "checkout" (the query-toplevel) :test #'string-equal)))) (and (stringp c) c)))
   (query-wallet (let ((w (cdr (assoc "wallet" (the query-toplevel) :test #'string-equal)))) (and (wallet-id? w) w)))
   (cancelled? (equal (cdr (assoc "topup" (the query-toplevel) :test #'string-equal)) "cancelled"))

   (initial-signals
    (format nil "{tab: 'prompt', private: false, prompt: '', turnstile: '', source: '', inspect: '', more: '', moreAll: false, amount: 0, error: '', notice: ~a, sending: false, saving: false, paying: false, busy: ~a, editable: ~a, owner: ~a, checkout: ~a, wallet: ~a}"
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

   (body (if (the browse) (the listing-body) (the lab-body)))

   ;; The listings (browse.lisp's own summaries): the live sessions, or
   ;; every archived one with its thumbnail.  A session goes to its sheet
   ;; when it is live here, to the page's archive view when it is not (the
   ;; sheet has none yet), and to the other lab when that one built it.
   (listing-body
    (let* ((archive? (equal (the browse) "archive"))
           (summaries (and *browsing?*
                           (newest-first (if archive? (archive-summaries) (live-summaries))))))
      (with-lhtml-string ()
        (:div :id "pl-sheet" :class "pl-listing"
              (:header :class "pl-head"
                       (:h1 (esc (lab-title)))
                       (:span :class "pl-engine" (esc (engine-label)))
                       (:nav (str (the browse-links))
                             (:a :href (format nil "~a/sheet" *url-prefix*) "start your own")))
              (:main :class "pl-list"
                     (:h2 (str (if archive? "Archived sessions" "Live sessions")))
                     (cond ((not *browsing?*)
                            (htm (:p :class "pl-line" "Sessions are not listed on this lab.")))
                           ((null summaries)
                            (htm (:p :class "pl-line" "Nothing to list yet.")))
                           (t
                            (dolist (s summaries)
                              (str (listing-entry s archive?))))))))))

   (browse-links
    (with-lhtml-string ()
      (when *browsing?*
        (htm (:a :href (format nil "~a/sheet?browse=live" *url-prefix*) "sessions")
             (:a :href (format nil "~a/sheet?browse=archive" *url-prefix*) "archive")))))

   (lab-body
    (with-lhtml-string ()
      (:div :id "pl-sheet"
            :|data-signals| (escape-string-minimal-plus-quotes (the initial-signals))
            :|data-init| (the datastar-stream-attribute)
            ;; the phone's tab, for the stylesheet
            :|data-attr:data-tab| "$tab"
            ;; a browser holding this session's key makes the page its owner's
            (when (and (the shown-id) (not (the owner?)))
              (htm (:span :|data-init| (format nil "$owner && ~a" (the (datastar-action :claim))))))
            ;; on a phone, a build that finishes takes the screen to the model
            (:span :|data-effect| "var b=$busy; if(window.plWasBusy && !b && matchMedia('(max-width: 760px)').matches){$tab='model'} window.plWasBusy=b")
            (:header :class "pl-head"
                     (:h1 (esc (lab-title)))
                     (:span :class "pl-engine" (esc (engine-label)))
                     (:nav (str (the browse-links))
                           (:a :href (the classic-url) "the page")
                           (when *sibling-lab*
                             (htm (:a :href (format nil "~a/sheet" (car *sibling-lab*)) (esc (cdr *sibling-lab*)))))
                           (:select :id "pl-skin" :title "How the lab looks" :onchange "plSetSkin(this.value)"
                                    (:option :value *house-skin* (esc (or (ignore-errors (token-value (format nil "~a/tokens.css" sluice:*static*) "--pl-skin-label")) "Workstation")))
                                    (dolist (choice (the skin-choices))
                                      (htm (:option :value (first choice) (esc (second choice))))))))
            ;; the phone's four screens; on a desk everything shows at once
            (:nav :class "pl-tabs"
                  (dolist (tab '(("prompt" "Prompt") ("model" "Model") ("parts" "Parts") ("code" "Code")))
                    (htm (:button :type "button"
                                  :|data-on:click| (format nil "$tab = '~a'" (first tab))
                                  :|data-class:pl-tab-on| (format nil "$tab === '~a'" (first tab))
                                  (str (second tab))))))
            (:main :class "pl-grid"
                   (:section :class "pl-left"
                             (:div :class "pl-g pl-g-prompt"
                                   (str (the prompt-form))
                                   (:p :class "pl-notice" :|data-show| "$notice" :|data-text| "$notice")
                                   (str (the status-section div))
                                   (str (the credits-section div))
                                   (str (the log-section div))))
                   (:section :class "pl-right"
                             ;; never hidden: off the screen on a phone's other tabs, so
                             ;; the drawing keeps its size
                             (:div :id "sluice-panes" :class "pl-viewport" (str (the viewport div)))
                             (:div :class "pl-g pl-g-model" (str (the downloads-section div)))
                             (:div :class "pl-g pl-g-parts"
                                   (:div :class "pl-parts"
                                         (str (the tree-section div))
                                         (str (the inputs-section div))))
                             (:div :class "pl-g pl-g-code" (str (the editor-card))))))))

   ;; Not a section either: the editor keeps its text, folds and caret
   ;; through every push.  New versions of the file arrive as
   ;; plSetSource calls (sheets-hear); Save sends the text as the
   ;; `source' signal and nothing else.
   (editor-card
    (with-lhtml-string ()
      (:div :class "pl-card pl-source"
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
                       (esc (the shown-source))))))

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
                           :|data-on:click| (the (datastar-action :build :options "{filterSignals: {include: /^(prompt|turnstile|owner|wallet)$/}}"))
                           :|data-indicator:sending| ""
                           ;; with a human check, a build waits for its token
                           :|data-attr:disabled| (format nil "$busy || $sending || !$prompt.trim()~@[ || !$turnstile~]"
                                                         *turnstile-site-key*)
                           "Build")
                  (:span :class "pl-busy" :|data-show| "$busy" "the agent is working...")))
      (:p :class "pl-error" :|data-show| "$error" :|data-text| "$error")
      ;; hidden until Datastar has read the signals: no banner flashes at
      ;; an owner while the script loads
      (:div :class "pl-card pl-watch" :style "display:none" :|data-show| "!$editable"
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
                                       (when (model-defined? session)
                                         (htm (:a :href (viewer-url session) :target "_blank" "full sluice")))
                                       (:a :href (the classic-url) "the page")
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
                                (htm (:p "The model's drawings and files, once it is built.")))
                            (:p :id "pl-download-note" :class "pl-line"))))))

   (tree-section
    :type 'base-html-div
    :inner-html (let ((model (the model-object)))
                  (with-lhtml-string ()
                    (:div :class "pl-card"
                          (:h2 "Parts")
                          (:div :id "tree"
                                (if model
                                    (str (tree-html self model (the inspected-node) (the tree-shown)))
                                    (htm (:p "No model yet."))))))))

   (inputs-section
    :type 'base-html-div
    :inner-html (let ((node (the inspected-node)))
                  (with-lhtml-string ()
                    (:div :class "pl-card pl-inputs"
                          (:h2 (if node
                                   (fmt "Inputs of ~a" (tree-label node))
                                   (str "Inputs")))
                          (if node
                              (str (the input-panel html))
                              (htm (:p "The model's inputs appear here once it is built.")))))))

   (viewport :type 'viewport-html-div
             :image-format :svg
             :projection-key :trimetric
             :hidden-lines (if (<= 1 (the leaf-count) *hidden-lines-max-leaves*) :remove :draw)
             :display-list-object-roots (let ((model (the model-object))) (when model (list model)))))

  :hidden-objects
  (;; the sluice's own inputs panel: a live control for every input of
   ;; the node (every :settable slot below the root), set in this
   ;; sheet's instance of the model with no agent and no compile; its
   ;; calls are gdlAjax's, whose replies hand the redraw to the stream
   (input-panel :type 'sluice::input-panel
                :node (the inspected-node)
                :sluice self))

  :functions
  ((input-changed!
    (&key node slot)
    ;; what the sluice calls after every set and reset: nothing to do
    ;; yet (the place for a meter report on edits, as on the viewer)
    (declare (ignore node slot))
    nil)

   (more
    (signals)
    ;; a "..." row: that part's children listed further, or all of them
    (let* ((text (gethash "more" signals))
           (path (and (stringp text) (ignore-errors (let ((*package* (find-package :keyword)))
                                                      (read-safe-string text)))))
           (node (and (listp path) (the model-object)
                      (ignore-errors (the-object (the model-object) (follow-root-path path))))))
      (when node
        (let* ((total (length (ignore-errors (the-object node children))))
               (shown (or (cdr (assoc path (the tree-shown) :test #'equal)) sluice::*tree-condense-limit*)))
          (the (set-slot! :tree-shown
                          (acons path (if (eq (gethash "moreAll" signals) t)
                                          total
                                          (min total (+ shown sluice::*tree-condense-limit*)))
                                 (remove path (the tree-shown) :key #'car :test #'equal))))))))

   (inspect
    (signals)
    (let* ((text (gethash "inspect" signals))
           (path (and (stringp text) (ignore-errors (let ((*package* (find-package :keyword)))
                                                      (read-safe-string text))))))
      (when (listp path)
        (the (set-slot! :inspected-path path)))))

   (set-instantiation-time!
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
    (watch-session! self session))

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

(defun publish-lab-sheet! (&key host)
  "Publish the sheet at <prefix>/sheet, beside the page."
  (gwl::publish-gwl-app (format nil "~a/sheet" *url-prefix*) 'lab-sheet :host host))
