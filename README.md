
## A hodgepodge of demo applications mostly half-done. 

## The prompt lab

`prompt-lab/` is not a demo page but a door: a visitor describes a
part in plain words, a modeling agent builds it as a parametric Gendl
model in a session of its own, and the visitor sees it in a viewer
beside the model's source, which they may edit and reload.  The
agent's calls to the language model go through a gate on the
deploying stack's reverse proxy, which holds the API key; no key
lives with this code.

## Issues

Problems with the prompt lab or any demo -- a model it built wrong, a
page that misbehaves, a suggestion -- are welcome as issues at
<https://github.com/genworks/demos/issues>.  That repository is a live
mirror of this one.

## License

AGPL-3.0-or-later, © 2026 Gornskew Enterprises — full text in
[LICENSE](LICENSE), which is the license verbatim and nothing else.

The demos are Gornskew's; the engine they run on is Genworks'. Gendl
and Genworks GDL proper are "Copyright Genworks International", and
that distinction is why the headers in this repo say Gornskew without
contradicting anything.


