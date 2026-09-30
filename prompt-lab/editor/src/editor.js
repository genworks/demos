// Copyright (c) 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
//
// editor.js -- the prompt lab's model-file editor: CodeMirror 6 taught
// enough Lisp to be a small structured editor.  The source is coloured
// (Common Lisp, with Gendl's own words), every form that runs over
// several lines folds to its first line from the gutter, parentheses
// are matched and closed as they are typed, Tab indents as Lisp is
// indented, and the cursor moves, selects and kills by s-expression
// with the keys Emacs uses for it.
//
// Built into ../static/editor.js (see README.md here); the page mounts
// it over its plain textarea, which stays as the fallback where this
// script did not load.  How it LOOKS is the page's stylesheet: tokens
// carry the classes of @lezer/highlight's classHighlighter (tok-keyword,
// tok-string, tok-comment ...), so a skin colours code with CSS alone.

import { Annotation, Compartment, EditorSelection, EditorState } from '@codemirror/state';
import { EditorView, keymap, lineNumbers, placeholder } from '@codemirror/view';
import {
  StreamLanguage, bracketMatching, foldAll, foldGutter, foldKeymap, foldService,
  indentOnInput, indentUnit, syntaxHighlighting, unfoldAll,
} from '@codemirror/language';
import { defaultKeymap, history, historyKeymap, indentSelection } from '@codemirror/commands';
import { closeBrackets, closeBracketsKeymap } from '@codemirror/autocomplete';
import { classHighlighter } from '@lezer/highlight';
import { commonLisp } from '@codemirror/legacy-modes/mode/commonlisp';
import { backward, down, enclosing, folds, forward, lineStarts, tokens } from './sexp.js';

// --- the language ------------------------------------------------------------
//
// CodeMirror's Common Lisp mode, with Gendl's vocabulary laid over it:
// the section keywords of a define-object stand out as headings, and
// the referencing macros read as the keywords they are.

const SECTIONS = new Set([
  ':input-slots', ':computed-slots', ':objects', ':hidden-objects', ':functions', ':methods',
  ':trickle-down-slots', ':documentation',
]);
const WORDS = new Set([
  'the', 'the-child', 'the-object', 'the-element', 'define-object', 'define-format', 'define-lens',
  'make-object', 'evaluate',
]);

const gendlLisp = {
  ...commonLisp,
  name: 'gendl',
  token(stream, state) {
    const opened = state.lastType === 'open';
    const style = commonLisp.token(stream, state);
    if (style === 'atom' || style === 'variableName' || style === 'keyword') {
      const word = stream.current().toLowerCase();
      if (SECTIONS.has(word)) return 'heading';
      if (opened && WORDS.has(word)) return 'keyword';
    }
    return style;
  },
};

// --- s-expressions -----------------------------------------------------------

const read = new WeakMap();     // a document -> what sexp.js makes of it

function reading(doc) {
  let r = read.get(doc);
  if (!r) {
    const text = doc.toString(), list = tokens(text);
    r = { text, list, folds: null };
    read.set(doc, r);
  }
  return r;
}

const lispFolds = foldService.of((state, lineStart) => {
  const r = reading(state.doc);
  if (!r.folds) r.folds = folds(r.text, r.list, lineStarts(r.text));
  return r.folds.get(lineStart) || null;
});

function go(view, pos, extend) {
  if (pos != null) {
    const main = view.state.selection.main;
    view.dispatch({
      selection: extend ? EditorSelection.range(main.anchor, pos) : EditorSelection.cursor(pos),
      scrollIntoView: true, userEvent: 'select',
    });
  }
  return true;                  // the key is ours either way
}

const head = (view) => view.state.selection.main.head;
const list = (view) => reading(view.state.doc).list;

const sexpKeys = [
  { key: 'Ctrl-Alt-f', run: (v) => go(v, forward(list(v), head(v))), shift: (v) => go(v, forward(list(v), head(v)), true) },
  { key: 'Ctrl-Alt-b', run: (v) => go(v, backward(list(v), head(v))), shift: (v) => go(v, backward(list(v), head(v)), true) },
  { key: 'Ctrl-Alt-u', run: (v) => { const e = enclosing(list(v), head(v), head(v), v.state.doc.length); return go(v, e && e.from); } },
  { key: 'Ctrl-Alt-d', run: (v) => go(v, down(list(v), head(v))) },
  { key: 'Ctrl-Alt-k',
    run(v) {
      const from = head(v), to = forward(list(v), from);
      if (to != null && to > from && !v.state.readOnly) v.dispatch({ changes: { from, to }, userEvent: 'delete.cut' });
      return true;
    } },
  // select the list the selection is in; again, the one around that
  { key: 'Ctrl-Alt-Space',
    run(v) {
      const main = v.state.selection.main;
      let e = enclosing(list(v), main.from, main.to, v.state.doc.length);
      if (e && e.from === main.from && e.to === main.to) e = enclosing(list(v), e.from, e.to, v.state.doc.length);
      if (e) v.dispatch({ selection: EditorSelection.range(e.from, e.to), scrollIntoView: true, userEvent: 'select' });
      return true;
    } },
];

// --- the editor --------------------------------------------------------------

const programmatic = Annotation.define();   // a change the page made, not the person

// Mount an editor over TEXTAREA, which is hidden and kept in step.
// options.onEdit() is called when the person changes the text.
// Returns the editor: value (get and set), setReadOnly(flag),
// setPlaceholder(text), foldAll(), unfoldAll(), focus(), view.
export function mount(textarea, options = {}) {
  const readOnly = new Compartment(), hint = new Compartment();
  const holder = document.createElement('div');
  holder.className = 'code-editor';
  // what the page's documentation line says of it
  const doc = textarea.getAttribute('data-editor-doc') || textarea.getAttribute('data-doc');
  if (doc) holder.setAttribute('data-doc', doc);
  textarea.parentNode.insertBefore(holder, textarea);

  const locked = (flag) => [EditorState.readOnly.of(flag), EditorView.editable.of(!flag)];
  const view = new EditorView({
    parent: holder,
    state: EditorState.create({
      doc: textarea.value,
      extensions: [
        lineNumbers(),
        foldGutter(),
        history(),
        StreamLanguage.define(gendlLisp),
        syntaxHighlighting(classHighlighter),
        lispFolds,
        indentUnit.of('  '),
        indentOnInput(),
        bracketMatching(),
        closeBrackets(),
        EditorState.tabSize.of(8),
        keymap.of([
          ...sexpKeys,
          ...closeBracketsKeymap,
          { key: 'Tab', run: indentSelection },
          ...defaultKeymap,
          ...historyKeymap,
          ...foldKeymap,
        ]),
        readOnly.of(locked(textarea.readOnly)),
        hint.of(placeholder(textarea.placeholder || '')),
        EditorView.contentAttributes.of({
          spellcheck: 'false', autocapitalize: 'off', autocorrect: 'off',
          'aria-label': textarea.getAttribute('aria-label') || 'Model file',
        }),
        EditorView.updateListener.of((update) => {
          if (!update.docChanged) return;
          textarea.value = update.state.doc.toString();
          if (options.onEdit && !update.transactions.every((t) => t.annotation(programmatic))) options.onEdit();
        }),
      ],
    }),
  });
  textarea.hidden = true;

  return {
    view,
    get value() { return view.state.doc.toString(); },
    set value(text) {
      text = text == null ? '' : String(text);
      if (text === view.state.doc.toString()) return;
      view.dispatch({
        changes: { from: 0, to: view.state.doc.length, insert: text },
        annotations: programmatic.of(true),
      });
    },
    setReadOnly(flag) { view.dispatch({ effects: readOnly.reconfigure(locked(!!flag)) }); },
    setPlaceholder(text) { view.dispatch({ effects: hint.reconfigure(placeholder(text || '')) }); },
    foldAll() { foldAll(view); },
    unfoldAll() { unfoldAll(view); },
    focus() { view.focus(); },
  };
}
