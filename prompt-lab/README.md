# Prompt Lab

A visitor describes a part in plain words; a modeling agent builds it
as a parametric [Gendl](https://gitlab.common-lisp.net/gendl/gendl)
model in a session of the visitor's own, who sees it live in a viewer
beside the model's source, and may keep prompting, adjust the inputs,
or edit the file by hand and reload it.

## The reference instance

Genworks hosts the lab at
**<https://hack.genworks.com/prompt-lab>**.  It is an experiment,
offered as is: each session allows a handful of prompts and starts
with free rivets to spend, prompts and generated code are logged,
and generated code is the visitor's under the GNU Affero General
Public License, like Gendl itself and like this code.

This directory of the [genworks/demos](https://github.com/genworks/demos)
repository is that instance's source.  Issues with the lab, a model it
built wrong, or a page that misbehaves are welcome at
<https://github.com/genworks/demos/issues>.

## What is here

- `source/` -- the ASDF system `prompt-lab`, a Gendl web application:
  its doors (`page.lisp`), the sessions and their
  archive, the agent loop over the Messages API (`agent.lisp`), the
  tools the agent is given (write, read, evaluate, check, render, the
  docs, the visitor's files), the uploaded files themselves
  (`uploads.lisp`), the metering of compiles and runs, and the guards
  at the door.  `parameters.lisp` holds what an instance sets.
- `prompt-lab-sheet/` -- the ASDF system `prompt-lab-sheet`, the
  page: the sluice, Gendl's object browser, opened on the session's
  model, with the lab's own parts (the prompt, the status and credits,
  the downloads, the log, the model file) as its tiles.  It is one gwl
  sheet that hears every change to its session and pushes what changed
  to the browser over a stream; on a phone it shows one tab at a time.
- `editor/` -- the model file's editor: CodeMirror taught Lisp, with
  colours, folding by s-expression and Emacs's keys for moving by
  one; built into `static/editor.js`
  ([editor/README.md](editor/README.md)).
- `external/` -- a script that stands in for the agent loop during
  development (below).
- `static/` -- the editor, the icons and the stylesheets.  How the lab
  looks is a **skin**, one CSS file of tokens dressing the page and the
  viewer alike; the skins are the viewer's, Gendl's object browser (the
  sluice), and [SKIN-API.md](SKIN-API.md) says what the lab adds.
- The viewer, at `<prefix>/viewer`, is a sluice opened on a session's
  model alone, dressed in the page's skin.

The agent's calls to the language model go through a gate on the
deploying stack's reverse proxy, which holds the API key: no key
lives with this code, and none is read from it.  Payments, when an
instance offers them, are settled by the same gate.

Builds are paid in **rivets**, the lab's unit (`*unit*` in
`parameters.lisp`: an instance may call it what it likes; the doors'
field names still say `credits`), and the gate keeps the count.  It
may give every session a free allowance and every payer a balance of
their own, or keep one **community pot**: a single balance that every
build, by anyone, draws on, that anyone may add to, and that stops the
lab at zero until someone does.  The page shows whichever the gate
says it keeps; the reference instance keeps a pot.

## Two kinds of build: a geometry model, or a web app

A switch beside the prompt says what the agent builds
(`source/kinds.lisp`):

- a **geometry model**, the default: the object named `MODEL`, shown in
  the viewer with a control for each of its inputs;
- a **web app**: the object named `APP`, a page written in GWL, Gendl's
  web layer, with form controls of its own, sections that redraw as the
  inputs change, and, when it shows geometry, a `MODEL` in the same
  file drawn in a viewport on the page.  The lab serves the app at
  `<prefix>/app?session=<id>` and its page links it as **Open the app**.

The kind goes with each prompt and the session keeps the last one.  A
web app mixes in `prompt-lab:web-app` (the demos' shared page chrome,
and an instance that expires when its visitor leaves); the agent is
given a whole worked app as its recipe (`*app-recipe*`, which the CI
compiles, checks and serves on every engine) and a `check_app` tool
that builds the page and reports its controls, its sections and what
it says.  From a script, `"kind": "app"` or `"kind": "model"` rides
with the prompt, and the state door answers `kind`, `app_defined` and
`app_url`.  `*kinds*` is what an instance offers, the default first:
`'(:model)` offers no switch.

A web app is a page written by a visitor's prompts, or by the visitor,
and it is served from the lab's own origin.

## Monetize: deploying what a session built

A session's owner may deploy what it built at an address of its own,
`<prefix>/d/<name>`, for others to pay to use (`source/deploy.lisp`;
the page's **Monetize** button).  The button is greyed until what was
built has a monetization story: the owner tells the agent, in a prompt,
what should cost money, and the agent writes the tollbooths into the
source.  Then:

- a **model** is deployed in a plain wrapper: the viewer, with a
  control for each of the model's inputs and its drawings and files to
  download;
- a **web app** is served as it is.

A deployment is a copy -- the model file as it was, and a record of its
terms -- kept apart from the session, which may go on changing or go
away; deploying again under the same name replaces it, and its owner
may take it down.

**Open or closed source** is chosen as the session opens, before its
first prompt (a box beside the prompt; `"closed": true` to the session
door).  An open session's deployment serves its source at
`<prefix>/d/<name>/source` under the GNU Affero General Public
License.  A closed-source session is seen by nobody else and its
deployment serves no source -- for as long as it completes the
Monetize flow: a closed session that ends without a deployment
**reverts**, and goes into the public archive under the AGPL like any
other.  `*closed-source?*` nil offers no such choice.

**Where the money is taken is the author's to decide.**

- A web app has **tollbooths**, which the author asks the agent to put
  wherever they like -- a price for each CAD download, a pass that
  unlocks a results table for a day, a price computed from the model.
  The app declares them (`tolls`: a key, a label, the price in rivets, and
  `:uses` or `:seconds` when the payment covers so many uses or so
  long) and places them (`toll-button`, `toll-paid?`, `use-toll!`);
  `file-link` gives a download of the model as it stands in the
  visitor's page, and `file-tolls` puts a format behind a toll.  The
  price is always the app's own on the server, never the browser's.
- A model says what its downloads cost in two computed slots of
  `MODEL`: `tolls`, as above, and `file-tolls`, a plist from download
  format to toll.  The page of the deployed model shows the prices; a
  format not named is free.

Nothing is deployed that charges for nothing (`monetizable?`: a priced
toll, and for a model a download it stands on); the state door answers
`monetizable`.

Everything is priced and paid in **rivets**, the lab's own unit, which
visitors buy in packets.  Of what users pay, the house keeps its
**monetization fee**: `*house-fee-percents*`, 10% of an open-source
deployment's takings and 15% of a closed-source one's.  The author may
give more, to the lab's community pot of rivets that pays for
everyone's builds: a slider on the Monetize form, from
`*pot-percent-least*` upwards, in points on top of the fee.  The author
is owed the rest, held in rivets and paid out after each quarter at
that day's rate.  The terms are written into each deployment's record
on the day.  The records and the books are kept by
[Monocle](https://gitlab.common-lisp.net/clo/monocle).

Hosting is at the house's discretion.  A closed-source deployment that
shows no revenue, or too little, for some time may be taken down: its
code stays the author's, still closed, and is wiped from the house's
systems.  An open-source deployment with no tollbooth, or with no
revenue for an extended period, may be un-hosted -- or kept, where the
house and its visitors find it interesting.

A toll has no minimum, and the fee on a line of the books is exact,
never rounded to the cent: it is the sums that are rounded.  What a
payment cost to take by card comes **off the top**: the house's fee
and the author's share are split from what is left, each bearing the
card cost in its own proportion.  The author's share accumulates
through a quarter and is paid out after it; a deployment's owner reads
what it has taken, quarter by quarter, at
`<prefix>/api/earnings?name=<name>`.

The lab keeps a deployment's code as a file, and nothing more: its
author keeps their own copy.

The lab keeps the terms and the books: `book-revenue!` appends a line
for each payment, with the fee and the author's share at the
deployment's own terms, the tollbooth, and the engine and Lisp it ran
on; `revenue-report` sums them by runtime for a year or a quarter and
`payables` by author.  **No money moves yet.**  `*toll-provider*` is
`:test`: a toll is granted on the spot and booked as a test payment,
which the reports leave out, so that the whole flow can be built and
tried; a real payment gateway is still to come, and until
`*deployment-payments?*` a priced model's downloads open only to its
owner.  Nothing a deployment does is metered or limited.

From a script: `POST <prefix>/api/session` with `{"closed": true}` for
a closed-source session; `POST <prefix>/api/deploy` with `{"session":
..., "name": ..., "payee": ..., "title": ..., "blurb": ...}` and the
owner's key (and the human check's token where
one stands); `POST <prefix>/api/undeploy` with `{"name": ...}`; `GET
<prefix>/api/deployments` lists them.  `*deployments?*` nil shuts all
of it.

## Building from a drawing: uploaded files

A visitor may hand the agent files to build from: a 2D drawing as a
PDF or an image (PNG, JPEG, GIF, WebP), or a text file (DXF, SVG, CSV,
STEP, IGES).  A PDF or an image goes to the model with the visitor's
next prompt, pages read as text and as pictures, and the agent builds
the part as drawn, the drawing's dimensions as the model's inputs,
saying which it could not read.  Text files it reads with its
`read_file` tool; `list_files` gives it every file's path, so on a
solids engine a STEP or IGES file can be imported outright.

**An uploaded file is public with its session**, live and in the
archive, for anyone to download, unless the session is private (a
closed-source session is, and one that has added rivets may be).  A
visitor whose session is public acknowledges that the file will be
shared before it is taken.  Files are only ever served as downloads.

The caps are `*upload-caps*` in `source/uploads.lisp` (by default 2 MB
a file, three files and a PDF of ten pages for a session; more for one
that has added rivets), and `*uploads?*` nil takes none.  Attached
files travel with every call the session makes, so the gate must take
a request that large.

From a script: `POST <prefix>/api/upload` with `{"session": ...,
"name": ..., "data": <the file in base64>, "rights": true}` and the
owner's key; `GET <prefix>/api/file?session=...&name=...` fetches one.
Where a human check stands, an upload needs its token (`"turnstile"`).

## Two labs, and which one a request belongs in

A lab on the open-source engine draws a hole but cannot cut one; a lab
on a solids engine cuts it, and costs more to run.  Where an instance
runs both side by side (`*sibling-lab*`), the first thing done with a
session's first prompt is to decide which engine it wants
(`source/routing.lisp`):

- the prompt may say so itself -- "no solids", "use solids";
- else one small call asks the model, of the prompt and of any drawing
  uploaded with it.  A drawing uploaded to the open-source lab is asked
  as it arrives, so the page can say so before any prompt.

A request that wants the other engine is not built where it was typed:
the page goes to the sibling lab with the prompt, and the sibling
offers to bring the session's files over.  From a script the prompt
door answers 409 with `{"route": {"engine": ..., "url": ...}}`;
`"stay": true` with the prompt builds it where it is.
`*route-prompts?*` and `*classify-uploads?*` nil ask nothing.

## Prompts from a script or an agent

The page is one client of the lab's doors, and a script or an agent
may be another.  `POST <prefix>/api/session` with `{}` opens a session
and answers its id and its owner key; `POST <prefix>/api/prompt` with
`{"session": ..., "prompt": ...}` (and `"kind": "app"` for a web app)
and the key in an `X-Prompt-Lab-Owner` header starts a build; `GET
<prefix>/api/state?session=...` follows it: the log, the model's
source, the viewer's address.

Where an instance puts a human check in front of the prompt door, a
prompt that carries no check token is taken on a small daily
allowance -- so many from one address, so many from all together
(`*max-automated-prompts-per-day*`, `*max-automated-prompts-per-address*`
in `parameters.lisp`; none at all when the first is nil) -- and spends
the same rivets as anyone's.  An interesting model built
that way is as welcome as any other; the allowance is there so that
a day of abuse stays small.

## Running it

Load the system into a Gendl image that has the `sluice` application
and call `(prompt-lab:publish-prompt-lab!)`; the lab answers at
`/prompt-lab` on every server the image runs.  What an instance needs
beyond that -- the gate's address, a Turnstile site key if visitors
are to be challenged, the engine name, the sibling lab on the other
engine -- is set in `parameters.lisp` or by the image's own
initialization.  A `prompt-lab` directory under the first of
`/state/`, `/projects/.state/` and `/tmp/` that exists holds the
sessions' files.

## Developing without the gate: your own Claude Code as the agent

A lab under development does not need the gate or an API key.  With
`prompt-lab:*external-agent?*` set to `t` the image opens two more
doors (`source/external.lisp`), and an agent that runs somewhere else
works on a session in place of the lab's own loop:

- `<prefix>/api/agent` takes the prompt (and, for a new session, a
  model file to start from) and answers with the system prompt the
  lab's own agent gets, the model and effort it would use, and the
  address of the session's tools; later it takes the agent's progress
  and its reply, for the log the page shows.
- `<prefix>/mcp?session=<id>` is the session's tools -- `write_model`,
  `read_model`, `evaluate`, `check_model`, `render`, `describe_object`,
  `search_docs` -- as a [Model Context Protocol](https://modelcontextprotocol.io)
  server over HTTP.  Any MCP client that holds the session's key can
  call them.

[`external/claude-code.mjs`](external/claude-code.mjs) is such an
agent: [Claude Code](https://code.claude.com/docs/en/headless) run
headless, with the lab's system prompt in place of its own, none of
its built-in tools, and the session's tools allowed.

```lisp
(setq prompt-lab:*external-agent?* t)     ; in the image that serves the lab
```

```bash
claude auth login                         # once, wherever the script will run
node external/claude-code.mjs --lab http://localhost:9080/prompt-lab \
  "A bracket 120 by 80 mm, 6 mm thick, with four 8 mm mounting holes"
node external/claude-code.mjs --lab http://localhost:9080/prompt-lab \
  --continue "Make the holes 10 mm and add a 3 mm fillet"
```

It prints each tool call as it happens, the reply, the token counts,
where the lab keeps the model file, and the session's address: open
that in a browser to see the model in the viewer beside its source
(as a watcher -- the session's key stays with the script).  With
`--out FILE` it also writes the model's source to a file of your own,
under an `(in-package :gdl-user)` header, so the file loads into any
Gendl by itself.  With `--seed FILE` the session opens on a model
that already exists -- a file `--out` wrote, or any Gendl source whose
object is named MODEL: it is compiled and loaded there before the
agent starts, and the agent, asked to change it, reads it and works
from it (a file built on one engine can be taken to a lab on the
other this way).  It exits 0 when the build finished and 1 when it
did not, so a script can run a batch of prompts and collect the
models.  The script needs node 18 or later and the `claude` command;
`--help` lists its options.

What this exercises is everything but the lab's own loop and the
gate: the system prompt and the primer, the tools, the compiles and
runs and their metering, the page, the viewer, the archive.  The
model runs inside Claude Code's harness rather than behind a bare
Messages API call, so what it builds is close to, not identical with,
what a visitor of the same lab would get.

The external doors run a session's tools for whoever holds its key,
with none of the public doors' caps and no human check: they are for
a development host, and stay shut (404) wherever the switch is left
off.  The script drives your own Claude Code, signed in as you signed
it in, for your own development and testing; a lab that serves
visitors calls the language model through its gate, with an API key.

## License

AGPL-3.0-or-later, © 2026 Genworks International.
