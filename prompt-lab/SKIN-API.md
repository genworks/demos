# Prompt Lab -- Skin API 1

How the prompt lab looks is a skin's business, and a skin is one CSS
file.  This document is the contract: what a skin may rely on, and what
the lab promises not to break without a new version number.

## 1. What a skin is

A skin is one stylesheet, `static/prompt-lab-<name>.css`.  It is CSS
only: no JavaScript, no markup, nothing on the server.  Drop the file
into `static/` and it is on the page's Skin menu at the next request;
nothing else is edited.  `static/prompt-lab-blueprint.css` and
`static/prompt-lab-plain.css` are two to read and copy.

A skin's whole job is to redefine **tokens**, CSS custom properties
named `--pl-...`:

```css
:root {
  --pl-skin-api: 1;
  --pl-skin-label: "Amber";
  color-scheme: dark;

  --pl-bg: #1a1200;
  --pl-panel: #1f1600;
  --pl-ink: #ffb000;
  --pl-line: #ffb000;
  --pl-label-bg: #ffb000;
  --pl-label-ink: #1a1200;
}
```

That is a complete skin.  Every token has a default, so a skin defines
as few as it likes and inherits the rest.

The lab is two documents: the **page**, and the **viewer** in the
page's frame (a sluice, Gendl's object browser).  Each loads the same
sheets in the same order:

| order | sheet | what it is |
|---|---|---|
| 1 | `prompt-lab.css` | the tokens, each with its default |
| 2 | `prompt-lab-page.css` or `prompt-lab-viewer.css` | that document laid out in the tokens |
| 3 | `prompt-lab-<name>.css` | the skin |

So one file dresses both, and a skin never needs to know which
document it is in.  Nor which layout: on a desk the page is a frame of
tiled panes, on a phone it is an app with a bar, screens and tabs, and
both are drawn from the same tokens.

The look with no skin at all is the **house look**, called
`workstation`: the defaults in `prompt-lab.css`.  It is drawn from the
Lisp workstations of the 1980s -- black ink on a near-white ground,
panes tiled edge to edge and ruled off in single pixels, labels in
reverse video, the thing under the pointer boxed, a documentation line
at the foot of the screen.

## 2. Versioning

A skin declares the contract it was written against:

```css
:root { --pl-skin-api: 1; }
```

- **Additive within a version.**  New tokens may appear; the ones
  listed here will not be renamed, removed or change their meaning.
- **Renames and removals only at a new version**, with the old names
  kept working for one version wherever that can be done mechanically.
- A skin that declares no version, or one the lab does not know, is
  loaded anyway and read as version 1.

## 3. The tokens

`static/prompt-lab.css` is the list, with each default; this section
says what each one promises.

### Surface

| token | meaning |
|---|---|
| `--pl-bg` | the ground behind everything |
| `--pl-panel` | a pane's interior |
| `--pl-panel-alt` | a second surface inside a pane: wells, headings, the visitor's own words in the log |
| `--pl-line` | rules between and around panes; borders of fields and buttons |
| `--pl-line-soft` | rules inside a pane, between rows |
| `--pl-ink` | text |
| `--pl-ink-dim` | secondary text |
| `--pl-ink-dimmer` | a step dimmer: times, counts, placeholders |

### Labels

| token | meaning |
|---|---|
| `--pl-label-bg` | the ground of a label strip: pane titles, the title block, the documentation line, the phone's bar |
| `--pl-label-ink` | text on it |

### Action

| token | meaning |
|---|---|
| `--pl-accent` | the ground of the primary button, and of what is selected (the current tab, the current listing) |
| `--pl-accent-ink` | text on it |
| `--pl-link` | a link, and in the viewer a value that can be followed |
| `--pl-hover-bg` | the fill under the pointer; `transparent` for none |
| `--pl-hover-line` | the box drawn around what is under the pointer; `transparent` for none |
| `--pl-focus` | the focus ring, and the second rule of the pane that has the keyboard |

### Status

These carry meaning, not appearance: a skin says what "stopped" looks
like here, it does not pick a red.

| token | meaning |
|---|---|
| `--pl-status-pass` | built |
| `--pl-status-warn` | a tool's complaint; the agent carried on |
| `--pl-status-fail` | stopped; an error |
| `--pl-status-busy` | working |

### Type

| token | meaning |
|---|---|
| `--pl-font` | the text face |
| `--pl-font-mono` | the face of code, of values, of the status line |
| `--pl-font-label` | the face of labels, commands and tabs |
| `--pl-size` | the text size on a desk (the phone sets its own) |
| `--pl-label-weight` | the weight of labels |
| `--pl-label-case` | their `text-transform`: `none`, `uppercase` |
| `--pl-label-tracking` | their `letter-spacing` |

### Shape

| token | meaning |
|---|---|
| `--pl-rule` | the weight of a rule |
| `--pl-gap` | the distance between panes |
| `--pl-seam` | what shows in that distance |
| `--pl-radius` | corners of panes, fields and buttons |
| `--pl-radius-pill` | corners of tags and pills |
| `--pl-shadow` | under a pane; `none` for none |
| `--pl-shadow-raised` | under what floats: a menu, a dialog |

`--pl-gap` and `--pl-seam` between them decide whether panes **tile**
or **float**.  Tiled (the house look): the gap is one rule wide and the
seam is the rule's colour, so neighbours share a single line.  Floating
(`plain`): the gap is wider and the seam is the ground, so each pane
stands alone, and the frame keeps the same distance from the edge of
the screen.

### The viewer

| token | meaning |
|---|---|
| `--pl-viewport-bg` | the ground the model is drawn on |
| `--pl-viewport-filter` | a CSS `filter` over the line drawing |

The drawing's lines come from the model, black unless the model says
otherwise, so a dark skin turns them with a filter rather than a
colour: `invert(1) hue-rotate(180deg)` makes black lines white and
leaves hues about where they were.

### What a skin says about itself

| token | meaning |
|---|---|
| `--pl-skin-api` | the version of this contract the skin was written for |
| `--pl-skin-label` | its name on the Skin menu, a quoted string; without one the menu shows the file's name |

A skin also sets the standard `color-scheme` property (`light` or
`dark`), which is what gives the browser's own controls -- scroll
bars, the fields' insides -- the right ground.

## 4. Rules beyond tokens

A skin *may* also write ordinary rules.  They name the lab's classes
and ids, and **those carry no promise**: they change as the page
changes.  A skin made of tokens alone outlives every such change; a
skin that reaches into `.pane .label` may not.  `blueprint` reaches in
once, for the grid on its drawing sheet, and says so where it does.

## 5. Choosing a skin

- The **Skin menu** among the page's commands.  The choice is kept in
  the visitor's browser (`localStorage`, `prompt-lab-skin`) and applies
  at once, to the page and the viewer.
- **`?skin=<name>`** on the page's address pins a skin for that visit
  and rides along on the page's own links; choosing from the menu
  drops the pin.
- **`*default-skin*`** (a Lisp parameter of the lab, `nil` for the
  house look) is what a visitor sees who has chosen nothing.

A name that is no skin's gives the house look, never an error.
`*skin-aliases*` leads retired names to their successors, so that a
saved choice or a bookmark does not fall back unexplained.

## 6. Rules for a skin

1. **Names.**  Lower-case letters, digits, hyphen and underscore.  A
   skin from outside this repository is
   `prompt-lab-<vendor>.<name>.css`; names without a vendor are kept
   for the skins that ship here.  `page`, `viewer` and `phone` are
   taken by the lab's own sheets.
2. **Tokens are the contract; class names are not.**  See section 4.
3. **No `@import`, no remote addresses.**  Fonts and images a skin
   needs go into the file as `data:` URIs.  The page is served with
   its stylesheets and should draw with them alone.
4. **ASCII.**  Write characters beyond it as CSS escapes
   (`content: "\2026"`).
5. **What has gone wrong must show.**  `--pl-status-fail` may not be
   set to a value that cannot be told from `--pl-status-pass` or from
   the ground it is drawn on.
6. **The names of other people's products stay out** of a skin's name
   and label.  A skin may be drawn from a period or a genre; it is
   named for what it is.

## 7. What a skin cannot reach (yet)

Candidates for a later version, left out of this one because each
means changing what is drawn rather than naming a colour:

- **The shaded views.**  The X3D scenes behind *View > Graphics
  Format > Shaded* draw their own ground inside the canvas.
- **Colours a model gives its own parts**, in the drawing and in the
  tree.  The filter turns them; no token sets them.
- **The human check and the card form**, which are other people's
  frames and follow `color-scheme` at most.
