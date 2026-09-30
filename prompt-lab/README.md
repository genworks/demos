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

## License

AGPL-3.0-or-later, © 2026 Genworks International.
