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

(defparameter *max-prompts-per-session* 6
  "Integer. Prompts one session may run: a first build and its follow-ups.")

(defparameter *max-prompt-length* 2000
  "Integer. Characters a prompt may have.")

(defparameter *max-sessions-per-address* 4
  "Integer or nil. Sessions one visitor address may open in a UTC day
(an IPv6 address counts by its /64); nil for no limit.")

(defparameter *max-prompts-per-address* 12
  "Integer or nil. Prompts one visitor address may run in a UTC day, across
its sessions; nil for no limit.  Two full sessions' worth.")

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
  "Integer. curl --max-time for one token verification.")

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
