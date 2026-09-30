// Copyright (c) 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
//
// sexp.js -- Lisp source as s-expressions, for an editor: where each one
// starts and ends, which list encloses a place, which lines open a form
// that closes further down.  Plain functions over a string; no editor
// and no browser is needed to run or test them.
//
// The reader's view of the text is what counts: a parenthesis inside a
// string, a comment, a #\( or a |symbol| opens and closes nothing.

// True for a character (or the end of the text) that ends an atom.
function ends(c) {
  return c === undefined || c === ' ' || c === '\n' || c === '\t' || c === '\r' || c === '\f'
    || c === '(' || c === ')' || c === '"' || c === ';' || c === "'" || c === '`' || c === ',';
}

// The tokens of TEXT, in order: { type, from, to } with type one of
// open, close, atom, string, comment, prefix (the quote, backquote,
// comma, ,@, #' and the # of #( that belong to the expression after
// them).  An open and its close name each other in `match`; one without
// a partner has match null.
export function tokens(text) {
  const out = [], stack = [], n = text.length;
  let i = 0;
  while (i < n) {
    const c = text[i], d = text[i + 1];
    if (c === ' ' || c === '\n' || c === '\t' || c === '\r' || c === '\f') { i++; continue; }
    let j;
    if (c === ';') {
      j = text.indexOf('\n', i);
      if (j < 0) j = n;
      out.push({ type: 'comment', from: i, to: j });
    } else if (c === '#' && d === '|') {
      // block comments nest
      let depth = 1;
      j = i + 2;
      while (j < n && depth > 0) {
        if (text[j] === '|' && text[j + 1] === '#') { depth--; j += 2; }
        else if (text[j] === '#' && text[j + 1] === '|') { depth++; j += 2; }
        else j++;
      }
      out.push({ type: 'comment', from: i, to: Math.min(j, n) });
    } else if (c === '"') {
      j = i + 1;
      while (j < n && text[j] !== '"') j += text[j] === '\\' ? 2 : 1;
      j = Math.min(j + 1, n);
      out.push({ type: 'string', from: i, to: j });
    } else if (c === '(') {
      const open = { type: 'open', from: i, to: i + 1, match: null };
      out.push(open);
      stack.push(open);
      j = i + 1;
    } else if (c === ')') {
      const close = { type: 'close', from: i, to: i + 1, match: null };
      const open = stack.pop();
      if (open) { open.match = close; close.match = open; }
      out.push(close);
      j = i + 1;
    } else if (c === "'" || c === '`') {
      j = i + 1;
      out.push({ type: 'prefix', from: i, to: j });
    } else if (c === ',') {
      j = d === '@' ? i + 2 : i + 1;
      out.push({ type: 'prefix', from: i, to: j });
    } else if (c === '#' && d === "'") {
      j = i + 2;
      out.push({ type: 'prefix', from: i, to: j });
    } else if (c === '#' && d === '(') {
      j = i + 1;
      out.push({ type: 'prefix', from: i, to: j });
    } else if (c === '#' && d === '\\') {
      // a character: the one after the backslash, whatever it is, and
      // the rest of its name
      j = Math.min(i + 3, n);
      while (j < n && !ends(text[j])) j++;
      out.push({ type: 'atom', from: i, to: j });
    } else {
      j = i;
      while (j < n) {
        const e = text[j];
        if (e === '\\') { j += 2; continue; }
        if (e === '|') {
          j++;
          while (j < n && text[j] !== '|') j += text[j] === '\\' ? 2 : 1;
          j++;
          continue;
        }
        if (ends(e)) break;
        j++;
      }
      j = Math.min(Math.max(j, i + 1), n);
      out.push({ type: 'atom', from: i, to: j });
    }
    i = j;
  }
  return out;
}

// Where the expression that starts at token K ends, or null when it
// does not (a close, an open with no partner, a prefix with nothing
// after it).
function endOf(list, k) {
  while (k < list.length && (list[k].type === 'prefix' || list[k].type === 'comment')) k++;
  const t = list[k];
  if (!t || t.type === 'close') return null;
  if (t.type === 'open') return t.match ? t.match.to : null;
  return t.to;
}

// With the prefixes that stand right before token K counted in: where
// its expression starts.
function startOf(list, k) {
  let from = list[k].from;
  while (k > 0 && list[k - 1].type === 'prefix' && list[k - 1].to === from) from = list[--k].from;
  return from;
}

// The end of the expression after POS (the rest of the one POS is in,
// when it is inside an atom or a string); null at the end of a list.
export function forward(list, pos) {
  for (let k = 0; k < list.length; k++) {
    const t = list[k];
    if (t.to <= pos || t.type === 'comment') continue;
    if ((t.type === 'atom' || t.type === 'string') && t.from < pos) return t.to;
    return endOf(list, k);
  }
  return null;
}

// The start of the expression before POS; null at the start of a list.
export function backward(list, pos) {
  for (let k = list.length - 1; k >= 0; k--) {
    const t = list[k];
    if (t.from >= pos || t.type === 'comment') continue;
    if (t.type === 'open') return null;
    if (t.type === 'close') return t.match ? startOf(list, list.indexOf(t.match)) : null;
    if (t.type === 'prefix') return t.from;
    return startOf(list, k);
  }
  return null;
}

// The innermost list that holds everything from FROM to TO: { from, to }
// with its parentheses (and prefixes), to at the text's end when the
// list is never closed; null at top level.  A list does not hold its
// own parentheses.
export function enclosing(list, from, to, length) {
  let best = null;
  for (let k = 0; k < list.length; k++) {
    const t = list[k];
    if (t.type !== 'open' || t.from >= from) continue;
    const close = t.match ? t.match.from : length;
    if (close < to) continue;
    // a later open that still holds the range is further in
    best = { from: startOf(list, k), open: t.from, to: t.match ? t.match.to : length };
  }
  return best;
}

// The first place inside a list after POS: just past its open
// parenthesis; null when no list opens before the enclosing one closes.
export function down(list, pos) {
  for (let k = 0; k < list.length; k++) {
    const t = list[k];
    if (t.from < pos) continue;
    if (t.type === 'close') return null;
    if (t.type === 'open') return t.to;
  }
  return null;
}

// What folds: for each line that opens a list closed on a later line,
// the range from that line's end to the closing parenthesis -- so a
// folded form keeps its first line and its last parenthesis.  Of
// several such lists opened on one line the outermost folds.  LINES is
// the offsets at which each line starts, ascending.  Returns a Map from
// a line's start to { from, to }.
export function folds(text, list, lines) {
  const found = new Map();
  const lineAt = (pos) => {
    let low = 0, high = lines.length - 1;
    while (low < high) {
      const mid = (low + high + 1) >> 1;
      if (lines[mid] <= pos) low = mid; else high = mid - 1;
    }
    return low;
  };
  for (const t of list) {
    if (t.type !== 'open' || !t.match) continue;
    const line = lineAt(t.from);
    if (lineAt(t.match.from) === line || found.has(lines[line])) continue;
    const next = line + 1 < lines.length ? lines[line + 1] : text.length + 1;
    let end = next - 1;
    if (end > 0 && text[end - 1] === '\r') end--;
    if (end < t.match.from) found.set(lines[line], { from: end, to: t.match.from });
  }
  return found;
}

export function lineStarts(text) {
  const starts = [0];
  for (let i = text.indexOf('\n'); i >= 0; i = text.indexOf('\n', i + 1)) starts.push(i + 1);
  return starts;
}
