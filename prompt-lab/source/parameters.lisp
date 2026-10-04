;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :prompt-lab)

(defun default-workspace-root ()
  "A prompt-lab/ directory under the first of these that exists: the ship's
state shelf /state/ (the public workshop, shared with the terminal next
door so it can open a session's model file), the shared state directory
/projects/.state/ (a dev ship), or /tmp/."
  (let ((shelf (or (find-if #'probe-file (list "/state/" "/projects/.state/")) "/tmp/")))
    (namestring (merge-pathnames "prompt-lab/" shelf))))

(defparameter *workspace-root* (default-workspace-root)
  "String. Directory under which each session keeps its own model file.")

(defun default-archive-root ()
  "A prompt-lab-archive/ directory beside the workspace root:
/state/prompt-lab-archive/ on the public workshop, /projects/.state/prompt-lab-archive/
on a dev ship."
  (namestring (merge-pathnames "prompt-lab-archive/"
                               (uiop:pathname-parent-directory-pathname (pathname *workspace-root*)))))

(defparameter *archive-root* (default-archive-root)
  "String or nil. Directory under which every session's record, its
transcript and each version of its model file are kept for review
(archive.lisp), out of the reaper's reach.  Nil keeps no archive.")

(defparameter *browsing?* t
  "Boolean. Whether visitors may browse sessions that are not their own
(browse.lisp): the live ones and the archived ones listed, each opened
read-only at its URL, an archived model drawn in a replay.  Nil keeps
every session private to the browser that opened it.")

(defparameter *browse-limit* 200
  "Integer. Sessions one listing shows, newest first.")

(defparameter *replay-root*
  (namestring (merge-pathnames "prompt-lab-replays/" (uiop:temporary-directory)))
  "String. Directory under which an archived session's model is compiled
again to be drawn (a replay, browse.lisp): scratch, rebuilt on demand.")

(defparameter *hidden-lines-max-leaves* 60
  "Integer. The viewer opens a model of at most this many leaves with its
hidden lines removed; a larger one opens as the plain wireframe, since
removal is quadratic in the edges (the 213-leaf Eiffel tower took 132 s).
A pane's View > Hidden lines turns removal back on.")

(defparameter *max-replays* 12
  "Integer. Replays held at once; the least recently used goes first.")

(defparameter *result-limit* 4000
  "Integer. Characters of printed result or output handed back to the agent
from one tool call; longer text is cut and marked.")

(defparameter *primer-files*
  (list "/src/gendl/CLAUDE.md" "/projects/gw/gendl/CLAUDE.md")
  "List of pathnames. The first one that exists supplies the modeling primer.")

(defparameter *primer-start* "## Building models through the lisply tools"
  "String. Heading that opens the primer section of the Gendl guide.")

(defparameter *pile-threshold* 0.5
  "Number. A model where more than this fraction of its leaves (and at least
three of them) share one center is reported as a pile -- parts left
unplaced at a common origin.")

(defparameter *url-prefix* "/prompt-lab"
  "String. Where the page lives: the page at the prefix itself, its doors
under <prefix>/api/, the viewer at <prefix>/viewer.")

(defparameter *console-base* nil
  "String or nil. URL prefix of the host's browser terminal (ttyd behind the
reverse proxy): \"/console\" on the public workshop, \"/ttyd\" on a dev
ship.  The page's editor link is <*console-base*>/?arg=<model file>, which
the terminal's door script hands to emacsclient.  Nil: no link.")

(defparameter *external-agent?* nil
  "Boolean. True opens the two doors of external.lisp: an agent that runs
somewhere else (a developer's headless Claude Code, say) is handed the
lab's system prompt and its tools over MCP, and works on a session in
place of the lab's own loop, with no call to the gate.  Those doors run
the tools for whoever holds a session's key, without the caps or the
human check of the public doors: for a development host, not for an
instance that serves visitors.")

(defparameter *max-prompts-per-session* 6
  "Integer. Prompts one session may run: a first build and its follow-ups.")

(defparameter *max-prompt-length* 2000
  "Integer. Characters a prompt may have.")

(defparameter *max-sessions-per-address* 5
  "Integer or nil. Sessions one visitor address may open in a UTC day
(an IPv6 address counts by its /64); nil for no limit.  One more than
four: a first prompt sent to the sibling lab (routing.lisp) has opened
a session in each.")

(defparameter *max-prompts-per-address* 12
  "Integer or nil. Prompts one visitor address may run in a UTC day, across
its sessions; nil for no limit.  Two full sessions' worth.")

(defparameter *max-automated-prompts-per-day* 24
  "Integer or nil. Prompts a UTC day the lab takes WITHOUT the human check,
from all addresses together, where a human check stands at the prompt
door (guards.lisp): the lane for a script or an agent that comes to
build.  They spend the same credits as everyone's prompts; this figure
and the one below bound what a day of abuse can take.  Nil or 0: no such
lane, and every prompt passes the human check.")

(defparameter *max-automated-prompts-per-address* 6
  "Integer or nil. Of those, the prompts one address may run in a UTC day
(an IPv6 address counts by its /64); nil for no limit of its own.")

(defparameter *turnstile-site-key* nil
  "String or nil. The Cloudflare Turnstile site key (public) the page
renders its widget with; nil renders no widget and the prompt door asks
for no token.  Cloudflare's test key 1x00000000000000000000AA always
passes, for a dev ship.")

(defparameter *turnstile-verify-url* nil
  "String or nil. Where a Turnstile token is verified.  Nil means the gate's
Turnstile door beside the Messages door (<*messages-url*>/turnstile), which
adds the secret; Cloudflare's siteverify
(https://challenges.cloudflare.com/turnstile/v0/siteverify) together with
*turnstile-secret-file* on a trusted dev ship.")

(defparameter *turnstile-secret-file* nil
  "Pathname or nil. A one-line file holding the widget's secret, for direct
verification on a trusted ship only; nil when the gate adds it.")

(defparameter *turnstile-seconds* 15
  "Integer. Seconds one token verification may take.")

(defparameter *free-allowance-cents* 77
  "Integer. The free allowance a session has at the gate, in cents of API
cost, as the page shows it before the gate has reported (the gate's
:session-budget-cents is the one that counts; it reports it back with
every answer, and that value replaces this one).  77 c reads as a round
100 modeling credits at the gate's 1.3 markup; the gate itself takes
:session-budget-credits and derives the cents from whatever the markup
is, and the balance door reports both before the first build.")

(defparameter *unit* '("rivet" . "rivets")
  "Cons of strings, singular and plural. What the page calls the unit
builds, and a deployed app's tollbooths, are paid in -- bought in packets,
given free, or drawn from a community pot.  A rivet is Genworks' own
currency: what a packet buys, and what a rivet is worth when an author is
paid, may change (monocle:payout-amount).  One name, said in one place:
the doors' field names (credits, credits_used ...) and the gate's are
program names and stay as they are.")

(defun units (&optional (count 2))
  "The unit's name for COUNT of them: rivets, or one rivet."
  (if (eql count 1) (car *unit*) (cdr *unit*)))

(defun units-title ()
  "The unit's name as a heading: Rivets."
  (string-capitalize (units)))

(defun unit-text (text)
  "TEXT with {units} read as the unit's name and {Units} as the same,
capitalised: how a message or a format control names the unit."
  (flet ((swap (text mark word)
           (loop for at = (search mark text)
                 while at
                 do (setq text (concatenate 'string (subseq text 0 at) word
                                            (subseq text (+ at (length mark)))))
                 finally (return text))))
    (swap (swap text "{units}" (units)) "{Units}" (units-title))))

(defparameter *pot-refresh-seconds* 10
  "Integer. How old what the lab knows of the gate's community pot may get
before the gate is asked again.  Other labs behind the same gate draw on
the same pot, so the page's figure is this stale at most.")

(defparameter *pot-seconds* 5
  "Integer. Seconds that question may take; the page waits behind it.")

(defparameter *own-lab*
  (cons "https://github.com/genworks/demos/tree/devo/prompt-lab" "run a prompt lab of your own")
  "Cons of a URL and a label, or nil.  Where the page sends someone who
would add to a community pot that has no room left: a lab of their own.")

(defparameter *wallet-header* "X-Prompt-Lab-Wallet"
  "String. The header naming the visitor's wallet to the gate on each
call; must match the gate's :wallet-header.")

(defparameter *gate-seconds* 20
  "Integer. Seconds a gate side door (balance, top-up, confirm) may take.")

(defparameter *session-lifetime* 7200
  "Integer. Seconds a session may sit unused before the reaper deletes it
(its package and its directory).  A busy session is never reaped.")

(defparameter *reaper-interval* 300
  "Integer. Seconds between the reaper's rounds.")

(defparameter *eval-seconds* 30
  "Integer. Time an evaluation, a model build or a check may take before
it is stopped: a runaway loop in a visitor's or the agent's model must not
hold the session's thread forever.")

(defparameter *load-seconds* 60
  "Integer. Time a compile-and-load of the model file may take.")

(defparameter *render-seconds* 60
  "Integer. Time a render may take.")

;;
;; The engine behind the session: which kernel the room runs.  The agent
;; is told (agent.lisp), the page shows it, the archive records it and
;; the gate's meter prices by it -- a solids room's symbols cost a
;; multiple, the factor a knob on the gate (:meter-engine-factors in the
;; cyclops config, beside the rates).  A room's services-init sets it;
;; the default reads the image, since the SMLib package is aboard only
;; on a Genworks GDL workshop.
;;

(defparameter *engine* (if (find-package :smlib) :solid :gendl)
  "Keyword. The engine behind this lab's sessions: :gendl (open-source
Gendl -- wireframe and surface primitives, no booleans) or :solid
(Genworks GDL with the SMLib kernel -- breps, booleans, volumes).  Sent
as \"engine\" with every meter report, so the gate can price a solids
room at its factor.")

(defparameter *engine-labels*
  '(:gendl "open-source Gendl"
    :solid "GDL + SMLib")
  "Plist, engine -> what the page calls it.")

(defparameter *engine-titles*
  '(:gendl "Prompt Lab"
    :solid "Prompt Lab · Solids")
  "Plist, engine -> the page's title, upper left and in the browser's tab:
which lab this is, at a glance.")

(defparameter *engine-notes*
  '(:gendl "The engine is open-source Gendl (no solid modelling kernel): there are no boolean operations, so holes can be drawn but not cut. Say so plainly when a request needs them."
    :solid "The engine is Genworks GDL with the SMLib solid modelling kernel. Whenever a request needs holes, cuts or joins, build it from the brep solids (box-solid, cylinder-solid, cone-solid, torus-solid, extruded-solid) and the booleans (subtracted-solid, united-solid, intersected-solid), so the visible result is one real solid with a volume; keep the tool solids as hidden children. Plain box, cylinder and the other wireframe primitives are fine for parts that need no boolean.")
  "Plist, engine -> the sentence the system prompt says about it.")

(defparameter *sibling-lab* nil
  "Nil, or (url . label): a lab on the same site running the other
engine, offered as a link on the page -- (\"/prompt-lab/solid\" . \"solid
modelling lab\") on a free room, the reverse on a solids room.")

(defun engine-name ()
  "The engine as the meter names it to the gate and the page reads it."
  (string-downcase (symbol-name *engine*)))

(defun engine-label ()
  (or (getf *engine-labels* *engine*) (engine-name)))

(defun lab-title ()
  (or (getf *engine-titles* *engine*) "Prompt Lab"))

(defun engine-note ()
  (or (getf *engine-notes* *engine*) ""))

(defparameter *render-tool?* t
  "Boolean. Whether the agent is offered the render tool.  Rendering
rasterises through Ghostscript, a subprocess; on a host where
run-program stalls (the public workshop, 2026-09-27: every render sat
150 s behind CCL's spinning process monitor, past the time limit, which
cannot interrupt that wait) the tool is withheld and the agent works
from check_model's numbers -- the visitor has the live viewer anyway.")

;;
;; Monetize: what a session built, deployed for others to use (deploy.lisp).
;;

(defparameter *deployments?* t
  "Boolean. Whether a session's owner may deploy what it built (the
page's Monetize button, the deploy door).  Nil shuts the doors; what is
already deployed stays on disk and is not served.")

(defparameter *deployed-root*
  (namestring (merge-pathnames "prompt-lab-deployed/"
                               (uiop:pathname-parent-directory-pathname (pathname *workspace-root*))))
  "String. Directory under which every deployment is kept, by engine and
name: <root>/<engine>/<name>/ holds deployment.json and model.lisp; the
books are <root>/revenue.jsonl.")

(defparameter *house-fee-percents* '(:open 10 :closed 15)
  "Plist. The house's share of what a deployment's users pay, its
monetization fee, in percent, by the deployment's source terms: open (the
GNU Affero General Public License) or closed.  Written into each
deployment's record when it is deployed: a later change here does not
reach back.")

(defparameter *toll-provider* :test
  "Keyword or nil. How a deployed app's tolls are taken: :test (granted at
once, booked as a test payment: no money moves), or nil (no toll can be
paid).  A real provider is the gate's to add.")

(defparameter *pot-percent-least* 0
  "Number or nil. The least an author adds of every payment to the lab's
community pot of rivets, in percentage points on top of the monetization
fee; the Monetize tile's slider starts here and goes up.  Nil: no slider,
and nothing goes to the pot.  The fee itself is the house's.")

(defun lab-house ()
  "This lab as Monocle knows it: where its deployments and books are kept,
the engine and the Lisp they run on, its unit (rivets), the fees, the
community pot's least share and the toll provider, as the parameters
above stand now."
  (monocle:make-house :root *deployed-root* :runtime (engine-name) :unit :rivets
                      :fee-percents *house-fee-percents* :pot-percent *pot-percent-least*
                      :provider *toll-provider*))

(defun house-fee-percent (closed?) (monocle:fee-percent (lab-house) closed?))

(defparameter *closed-source?* t
  "Boolean. Whether a visitor may open a session whose source is closed
(deploy.lisp): chosen as the session opens, before its first prompt.")

(defparameter *deployment-price-range* '(100 . 10000)
  "Cons of integers. The least and the most a deployed model may ask for
a download, in cents.")

(defparameter *max-deployments* 200
  "Integer. Deployments this lab keeps for its engine, all owners together.")

(defparameter *deployment-payments?* nil
  "Boolean. Whether the gate behind this lab takes payment for a deployed
model's priced downloads.  Nil: the model opens to everyone and its
downloads only to its owner until payments are on.  (A deployed web app
keeps tollbooths of its own: kinds.lisp, *toll-provider*.)")
