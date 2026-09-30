// build.mjs -- bundle src/editor.js and CodeMirror into ../static/editor.js,
// one ASCII script the page loads as it loads its own.  Run `npm install`
// once, then `npm run build`; the bundle is committed, so a host that
// serves the lab needs neither node nor the network.
import { build } from 'esbuild';
import { readFileSync } from 'node:fs';

const bundled = ['@codemirror/state', '@codemirror/view', '@codemirror/language', '@codemirror/commands',
                 '@codemirror/autocomplete', '@codemirror/legacy-modes', '@lezer/highlight', '@lezer/common',
                 'style-mod', 'w3c-keyname', 'crelt', '@marijn/find-cluster-break'];
const versions = bundled.map((name) => {
  try { return `${name} ${JSON.parse(readFileSync(`node_modules/${name}/package.json`, 'utf8')).version}`; }
  catch { return null; }
}).filter(Boolean);

await build({
  entryPoints: ['src/editor.js'],
  outfile: '../static/editor.js',
  bundle: true,
  minify: true,
  format: 'iife',
  globalName: 'PromptLabEditor',
  target: ['es2019'],
  charset: 'ascii',
  legalComments: 'none',
  banner: { js: `/*
 * The prompt lab's model-file editor.  BUILT from ../editor (src/editor.js,
 * src/sexp.js) by its build.mjs: change those and rebuild, not this file.
 *
 * The editor's own code: Copyright (c) 2026 Genworks International, GNU
 * Affero General Public License, version 3 or later;
 * see https://www.gnu.org/licenses/agpl-3.0.html
 *
 * Bundled with it, under the MIT license, copyright (c) Marijn Haverbeke
 * and others (https://codemirror.net, https://lezer.codemirror.net):
 * ${versions.join(',\n * ')}
 */` },
});
console.log(`built ../static/editor.js with ${versions.length} packages`);
