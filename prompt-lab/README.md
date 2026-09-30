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
with free modeling credits, prompts and generated code are logged,
and generated code is the visitor's under the GNU Affero General
Public License, like Gendl itself and like this code.

This directory of the [genworks/demos](https://github.com/genworks/demos)
repository is that instance's source.  Issues with the lab, a model it
built wrong, or a page that misbehaves are welcome at
<https://github.com/genworks/demos/issues>.

## What is here

- `source/` -- the ASDF system `prompt-lab`, a Gendl web application:
  the page and its doors (`page.lisp`), the sessions and their
  archive, the agent loop over the Messages API (`agent.lisp`), the
  tools the agent is given (write, read, evaluate, check, render, the
  docs), the metering of compiles and runs, and the guards at the
  door.  `parameters.lisp` holds what an instance sets.
- `external/` -- a script that stands in for the agent loop during
  development (below).
- `static/` -- one HTML document laid out twice (a workstation's tiled
  frame on a desk, an app on a phone), its stylesheets, the script,
  the web app manifest and the service worker.  How the lab looks is
  a **skin**, one CSS file of tokens dressing the page and the viewer
  alike; the skins are the viewer's, Gendl's object browser (the
  sluice), and [SKIN-API.md](SKIN-API.md) says what the lab adds.
- The viewer is a sluice opened on the session's model, dressed in
  the page's skin.

The agent's calls to the language model go through a gate on the
deploying stack's reverse proxy, which holds the API key: no key
lives with this code, and none is read from it.  Payments, when an
instance offers them, are settled by the same gate.

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

- `<prefix>/api/agent` takes the prompt and answers with the system
  prompt the lab's own agent gets, the model and effort it would use,
  and the address of the session's tools; later it takes the agent's
  progress and its reply, for the log the page shows.
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
Gendl by itself.  It exits 0 when the build finished and 1 when it
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
