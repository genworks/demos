// test.mjs -- src/sexp.js against the Lisp it has to read.  `npm test`.
import assert from 'node:assert/strict';
import { backward, down, enclosing, folds, forward, lineStarts, tokens } from './src/sexp.js';

const model = `;; a (comment with parens
(define-object model (base-object)
  :input-slots
  ((length 100) (label "a ) in a string")
   (open #\\( ) (odd |sym (bol|))

  #| a block (comment
     #| nested ( |# still ( |#
  :objects
  ((plate :type 'box
          :length (the length))))
`;
const list = tokens(model);
const at = (s, n = 0) => { let i = -1; do { i = model.indexOf(s, i + 1); } while (n-- > 0); return i; };
const types = (type) => list.filter((t) => t.type === type).map((t) => model.slice(t.from, t.to));

// what the reader sees, and what it does not
assert.equal(types('open').length, types('close').length, 'every parenthesis has its partner');
assert.ok(list.every((t) => t.type !== 'open' || t.match), 'no list is left open');
assert.deepEqual(types('string'), ['"a ) in a string"']);
assert.ok(types('atom').includes('#\\('), 'a character is an atom');
assert.ok(types('atom').includes('|sym (bol|'), 'a |symbol| is an atom');
assert.equal(types('comment').length, 2);
assert.ok(types('comment')[1].endsWith('still ( |#'), 'block comments nest');
assert.deepEqual(types('prefix'), ["'"]);

// moving by expression
const define = at('(define-object');
assert.equal(forward(list, define), model.lastIndexOf(')') + 1, 'forward over the whole form');
assert.equal(forward(list, at('(length 100)')), at('(length 100)') + '(length 100)'.length);
assert.equal(forward(list, at('length 100') + 2), at('length 100') + 'length'.length, 'from inside an atom to its end');
assert.equal(forward(list, at('100)') + 3), null, 'nothing forward at the end of a list');
assert.equal(backward(list, at('(label')), at('(length 100)'), 'backward over a list');
assert.equal(backward(list, at('length 100')), null, 'nothing backward at the start of a list');
assert.equal(backward(list, at("'box") + 4), at("'box"), 'a quote goes with what it quotes');
assert.equal(forward(list, at("'box")), at("'box") + 4);

// lists within lists
const slots = at('((length');
assert.deepEqual(enclosing(list, at('100'), at('100'), model.length),
                 { from: at('(length 100)'), open: at('(length 100)'), to: at('(length 100)') + 12 });
assert.equal(enclosing(list, at('(length 100)'), at('(length 100)') + 12, model.length).from, slots,
             'a list is not inside itself');
assert.equal(enclosing(list, 0, 0, model.length), null, 'top level has no list around it');
assert.equal(down(list, define + 1), at('(base-object)') + 1);
assert.equal(enclosing(tokens('(a (b'), 5, 5, 5).from, 3, 'an unclosed list still encloses');

// folding: a line that opens a form closed further down
const starts = lineStarts(model), found = folds(model, list, starts);
const lineOf = (s) => starts[model.slice(0, at(s)).split('\n').length - 1];
const fold = found.get(lineOf('(define-object'));
assert.equal(model.slice(fold.from, fold.to + 1), model.slice(at('(base-object)') + 13, model.lastIndexOf(')') + 1),
             'the form folds from the end of its first line to its last parenthesis');
assert.ok(found.has(lineOf('((length')), 'the slots fold');
assert.ok(found.has(lineOf('((plate')), 'the objects fold');
assert.ok(!found.has(lineOf('(open #')), 'a line whose lists all close on it does not fold');
assert.ok(!found.has(0), 'a comment does not fold');
assert.equal(folds('(a\r\n b)', tokens('(a\r\n b)'), lineStarts('(a\r\n b)')).get(0).from, 2, 'a CR stays on its line');

// nothing here may loop or throw on broken text
for (const broken of ['(((', ')))', '"never closed', '#| never closed', '|never', '#\\', '\\', '(a . b', "'", ',@']) {
  const t = tokens(broken);
  forward(t, 0); backward(t, broken.length); enclosing(t, 1, 1, broken.length); down(t, 0);
  folds(broken, t, lineStarts(broken));
}
console.log('sexp.js: all assertions hold');
