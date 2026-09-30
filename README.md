
## A hodgepodge of demo applications mostly half-done. 

## The prompt lab

`prompt-lab/` is not a demo page but a door: a visitor describes a
part in plain words, a modeling agent builds it as a parametric Gendl
model in a session of its own, and the visitor sees it in a viewer
beside the model's source, which they may edit and reload.  The
agent's calls to the language model go through a gate on the
deploying stack's reverse proxy, which holds the API key; no key
lives with this code.

How the lab looks is a **skin**: one CSS file of tokens dresses the
page and the viewer alike, on a desk and on a phone.  The skins are
the viewer's -- the sluice's, Gendl's object browser -- and so is
their contract; [prompt-lab/SKIN-API.md](prompt-lab/SKIN-API.md) says
what the lab adds.  On a desk the seams between the panes can be
dragged.

The lab can be installed as an app (a web app manifest, and a service
worker that keeps the page itself on the device).  It needs its
connection all the same: models are built, drawn and saved by the
lab's engine, and nothing of that is ever answered from a cache.

The lab's own [README](prompt-lab/README.md) names the reference
instance Genworks hosts, <https://hack.genworks.com/prompt-lab>; the
page links to this repository's `prompt-lab/` tree under About.

## The pod line

`pod-line/` began as a model built in the prompt lab: personal pods
on a rail run like a utility line past homes, each lowered by its
winch to a pad at its owner's door.  It grew from the question whether
the line could go the distance on cables.  The rail is a slotted box
girder and the long runs are twin track cables at the gauge of its
running heads, so one bogie serves both; the towers are sized by the
sag; and the model stands a pod at every pole, tower and anchor and
checks that it passes.  It is a sketch of the method, not a design:
the rope's weight and strength are rules of thumb, and wind is not
considered.

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

