# Prompt Lab -- skins

How the prompt lab looks is a skin's business, and a skin is one CSS
file of **tokens**.  The tokens, the skins and the contract between
them are the sluice's, Gendl's object browser, which is the lab's
viewer:

> **`apps/sluice/SKIN-API.md` in the Gendl repository is the contract.**

This file says only what the lab adds to it.

## Two documents, one skin

The lab is two documents: the **page**, and the **viewer** in the
page's frame, which is a sluice.  Both wear the one skin.

| document | what it loads |
|---|---|
| the page | the sluice's `tokens.css`; `static/prompt-lab-page.css`, the page laid out in the tokens; the skin |
| the viewer | a sluice told which skin to wear |

On a desk the page is a frame of tiled panes; on a phone it is an app
with a bar, screens and tabs.  Both are drawn from the same tokens, so
a skin never needs to know which document it is in, nor which layout.

## Where skins are found

- **The sluice's**, `skin-<name>.css` in its static directory.  They
  dress every sluice and the lab alike; this is where a skin belongs.
- **The lab's own**, `static/prompt-lab-<name>.css` here: a skin for
  the lab alone.  The viewer is handed it as one more stylesheet.
  `page`, `viewer` and `phone` are names the lab's own sheets have
  taken.

Where both have a skin of one name, it is the sluice's.

## Choosing a skin

- The **Skin menu** among the page's commands.  The choice is kept in
  the visitor's browser (`localStorage`, `prompt-lab-skin`) and applies
  at once, to the page and the viewer.
- **`?skin=<name>`** on the page's address pins a skin for that visit
  and rides along on the page's own links; choosing from the menu
  drops the pin.
- **`prompt-lab:*default-skin*`** (`nil` for the house look) is what a
  visitor sees who has chosen nothing.

## What the lab draws with the tokens that a sluice does not

| token | in the lab |
|---|---|
| `--pl-label-bg`, `--pl-label-ink` | also the title block, the documentation line at the foot, and the bar across the top of the phone's screen |
| `--pl-accent`, `--pl-accent-ink` | also the Build button, the current tab, the visitor's own words in the phone's conversation |
| `--pl-status-pass`, `-warn`, `-fail` | the edge of a log entry: built, a tool's complaint, stopped |
| `--pl-status-busy` | the run bars while the agent works |
| `--pl-size` | the text size on a desk; the phone sets its own |

The human check and the card form are other people's frames, and
follow `color-scheme` at most.

## The model file's colours

The model file is shown in an editor that colours Lisp (`editor/`,
built into `static/editor.js`).  Its ground, gutter and rules are the
tokens above.  The colours of the code are tokens of the lab's own,
which a skin MAY say and none has to: unsaid, each is a fixed hue
leaned toward `--pl-ink`, so it reads on a light ground and on a dark
one.

| token | colours |
|---|---|
| `--pl-code-keyword` | `define-object`, `the`, `let`, `defun` and their kin, at the head of a form |
| `--pl-code-section` | a define-object's sections: `:input-slots`, `:computed-slots`, `:objects` ... |
| `--pl-code-atom` | keywords, `nil` and `t` |
| `--pl-code-number` | numbers |
| `--pl-code-string` | strings |
| `--pl-code-comment` | comments (default `--pl-ink-dimmer`) |
| `--pl-code-paren` | parentheses (default `--pl-ink-dimmer`) |

## A sluice older than its skins

The lab runs on whatever Gendl image serves it, and an image built
before the sluice had skins has neither tokens nor skins to give.  The
lab then falls back on copies it carries: `static/prompt-lab.css` (the
tokens), `static/prompt-lab-viewer.css` (the sluice said in them) and
the two skins.  They follow the sluice's files and are not the place
to change anything; they go when no image needs them.
