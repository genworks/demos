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
    ;; the editor is no section: a new version of the file goes to it as
    ;; a call, which an edit in progress does not take
    ;; the file as it is, compiled or not
    (let ((body (or (ignore-errors (model-body session)) "")))
      (unless (equal body (the-object sheet shown-source))
        (the-object sheet (set-slot! :shown-source body))
        (sheet-send! sheet (datastar-script-event (format nil "plSetSource(~a)" (js-string-literal body))))))
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

(defun tree-html (sheet model inspected)
  "String of HTML: MODEL's parts as an indented outline, each a node that
carries its data-root-path (as the drawing's paths do, for the hover)
and inspects itself when clicked."
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
                       (dolist (child (ignore-errors (the-object object children)))
                         (walk child (1+ depth)))))))
        (walk model 0))
      (when (plusp left-out)
        (format out "<p class=\"tree-type\">~:d more parts not listed.</p>" left-out)))))


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
.pl-source pre{margin:0;max-height:40vh;overflow:auto;font-size:.8rem}
.pl-source-head{display:flex;gap:.6rem;align-items:baseline;margin-bottom:.4rem}.pl-source-head h2{margin:0}
.pl-source-state{color:#777;font-size:.8rem}.pl-source-state.pl-warn{color:#a12}
.pl-source-head button{font:inherit;font-size:.8rem;padding:.1rem .5rem}.pl-source-head .pl-save{margin-left:auto}
#pl-source{width:100%;box-sizing:border-box;min-height:16rem;font:12px/1.4 monospace}
.pl-source .cm-editor{max-height:45vh;font-size:12px;border:1px solid #ddd}
.pl-source .cm-editor.cm-focused{outline:none;border-color:#2b4c7e}
.code-editor .tok-comment{color:#777;font-style:italic}.code-editor .tok-keyword{color:#6a1b9a;font-weight:700}
.code-editor .tok-heading{color:#00695c;font-weight:700}.code-editor .tok-atom{color:#1550a8}
.code-editor .tok-number{color:#9a4a00}.code-editor .tok-string,.code-editor .tok-string2{color:#2a6e2f}
.code-editor .tok-punctuation{color:#999}.code-editor .tok-invalid{color:#a12}
.pl-parts{display:grid;grid-template-columns:minmax(10rem,1fr) minmax(14rem,1.4fr);gap:.8rem}
@media (max-width:1100px){.pl-parts{grid-template-columns:1fr}}
#tree{max-height:40vh;overflow:auto;font-size:.85rem}
#tree .tree-node{cursor:pointer;padding:.05rem .3rem;border-radius:2px;white-space:nowrap}
#tree .tree-node:hover,#tree .tree-node.sluice-lit{background:#dfe8f5}
#tree .tree-node.pl-inspected{background:#2b4c7e;color:#fff}
#tree .tree-type{color:#888;margin-left:.4rem;font-size:.75rem}
#tree .tree-node.pl-inspected .tree-type{color:#cdd8ea}
#sluice-panes svg path.sluice-lit{stroke:#2b4c7e!important;stroke-width:3!important}
.pl-inputs{font-size:.85rem;max-height:40vh;overflow:auto}
.sluice-apply{margin-left:.3rem;padding:0 .5rem;border:1px solid #2b4c7e;border-radius:3px;background:#fff;color:#2b4c7e;cursor:pointer}
.sluice-apply.sluice-pending{background:#2b4c7e;color:#fff}"
  "String. The sheet's own look, until it wears the lab's skins.")

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
   ("List. The root-path, from the model, of the node whose inputs the
panel shows; nil for the model itself."
    inspected-path nil :settable)
   ("Boolean. The sluice inputs panel's auto-apply switch, which the
panel sets on the page that holds it."
    inputs-auto-apply? t :settable)
   (use-fontawesome? t))

  :computed-slots
  ((title (lab-title))
   (datastar-actions (list :build :claim :save :inspect))

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
   (editable? (or (null (the session)) (the owner?)))

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
      (:style (str *sheet-css*))))

   (initial-signals
    (format nil "{prompt: '', turnstile: '', source: '', inspect: '', error: '', sending: false, saving: false, busy: ~a, editable: ~a, owner: ~a}"
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
                             (:div :id "sluice-panes" :class "pl-viewport" (str (the viewport div)))
                             (:div :class "pl-parts"
                                   (str (the tree-section div))
                                   (str (the inputs-section div)))
                             (str (the editor-card)))))))

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
                           :|data-on:click| (the (datastar-action :build :options "{filterSignals: {include: /^(prompt|turnstile|owner)$/}}"))
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
                                         (htm (:a :href (viewer-url session) :target "_blank" "full sluice")))
                                       (:a :href (the classic-url) "credits, downloads and more")))))))))

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

   (tree-section
    :type 'base-html-div
    :inner-html (let ((model (the model-object)))
                  (with-lhtml-string ()
                    (:div :class "pl-card"
                          (:h2 "Parts")
                          (:div :id "tree"
                                (if model
                                    (str (tree-html self model (the inspected-node)))
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
    (let* ((id (cdr (assoc "session" (the query-toplevel) :test #'string-equal)))
           (session (and (stringp id) (find-session id))))
      (when (and session (visible-to? session nil))
        (the (set-slot! :session session))
        (the (set-slot! :model-stamp (model-stamp session)))
        (the (set-slot! :shown-source (or (ignore-errors (model-body session)) "")))
        (watch-session! self session))))

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
