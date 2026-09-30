# The model file's editor

The page shows a session's model file in a small structured editor:
[CodeMirror 6](https://codemirror.net) taught enough Lisp for the job.

- **Colours.**  Common Lisp, with Gendl's words laid over it: the
  sections of a `define-object` stand out, `the` and its kin read as
  keywords.  The colours are the skin's (`../SKIN-API.md`).
- **Folding by s-expression.**  Every form that runs over several
  lines folds to its first line and its closing parenthesis: click the
  arrow in the gutter, or the `...` to open it again.  *Fold* in the
  pane's head folds every top-level form, the file as an outline;
  *Unfold* opens them all.  Folded or not, the text stays editable.
- **Parentheses.**  The partner of the one at the cursor is marked; an
  opening one brings its closing one; Tab indents the line, or the
  selection, as Lisp is indented.
- **Moving by s-expression**, with the keys Emacs uses:

  | key | does |
  |---|---|
  | Ctrl-Alt-F, Ctrl-Alt-B | forward and backward over one expression (with Shift: selecting) |
  | Ctrl-Alt-U | up, to the start of the list the cursor is in |
  | Ctrl-Alt-D | down, into the next list |
  | Ctrl-Alt-K | kill the expression after the cursor |
  | Ctrl-Alt-Space | select the list the cursor is in; again, the one around it |
  | Ctrl-Shift-[, Ctrl-Shift-] | fold and unfold at the cursor |

## What is here

- `src/sexp.js` -- Lisp source as s-expressions: where each starts
  and ends, which list encloses a place, which lines open a form that
  closes further down.  It reads as the Lisp reader does (a parenthesis
  in a string, a comment, a `#\(` or a `|symbol|` opens nothing), and
  it is plain functions over a string: `npm test` runs them with no
  browser.
- `src/editor.js` -- the editor: CodeMirror's pieces, its Common Lisp
  mode with Gendl's vocabulary, the folding and the keys above.  It
  exports `mount(textarea, {onEdit})`, which puts the editor over the
  page's textarea and keeps the two in step.
- `build.mjs` -- bundles both and CodeMirror into `../static/editor.js`,
  one ASCII script.

## Building

```bash
npm install     # once: CodeMirror and esbuild, into node_modules/
npm test
npm run build   # writes ../static/editor.js
```

The bundle is committed, so a host that serves the lab needs neither
node nor the network; a lab without the file, or a browser in which
it did not load, shows the model file in a plain textarea and works
the same.

## Licenses

The editor's own code is the lab's, AGPL-3.0-or-later.  CodeMirror and
the packages bundled with it are MIT, copyright Marijn Haverbeke and
others; the bundle's first lines name each with its version.
