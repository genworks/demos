
## A hodgepodge of demo applications mostly half-done. 

## The prompt lab

`prompt-lab/` is not a demo page but a door: a visitor describes a
part in plain words, a modeling agent builds it as a parametric Gendl
model in a session of its own, and the visitor sees it in a viewer
beside the model's source, which they may edit and reload.  The
agent's calls to the language model go through a gate on the
deploying stack's reverse proxy, which holds the API key; no key
lives with this code.

How the lab looks is a **skin**: one CSS file of tokens, dropped into
`prompt-lab/static/`, dresses the page and the viewer alike, on a desk
and on a phone.  [prompt-lab/SKIN-API.md](prompt-lab/SKIN-API.md) is
the contract.

## Issues

Problems with the prompt lab or any demo -- a model it built wrong, a
page that misbehaves, a suggestion -- are welcome as issues at
<https://github.com/genworks/demos/issues>.  That repository is a live
mirror of this one.

## License

AGPL-3.0-or-later, © 2026 Genworks International — full text in
[LICENSE](LICENSE), which is the license verbatim and nothing else.

Most of the demos run on open-source Gendl; `gear` and `naca-nurbs`
need Genworks GDL's surface and solid modeling.

