#!/usr/bin/env node
// Copyright (c) 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
//
// claude-code.mjs -- Claude Code, run headless, as the prompt lab's
// modeling agent: for developing and testing a lab of your own without
// its gate and without an API key.
//
//   node claude-code.mjs [options] "A bracket 120 by 80 mm, 6 mm thick..."
//
//   --lab URL        the lab (default $PROMPT_LAB_URL, else
//                    http://localhost:9080/prompt-lab)
//   --page URL       the lab as your browser reaches it, when that is
//                    another address (default $PROMPT_LAB_PAGE_URL, else
//                    the same)
//   --out FILE       when the build finishes, write the model's source
//                    there, under a header that puts it in gdl-user, so
//                    the file loads into any Gendl by itself
//   --continue       go on in the session this script last worked in
//   --session ID     go on in that session (one this script opened)
//   --model NAME     instead of the model the lab itself uses
//   --effort LEVEL   instead of the lab's
//   --claude PATH    the claude executable (default $CLAUDE, else claude)
//   --raw            print Claude Code's own event stream as it comes
//
// The lab must have its external doors open (prompt-lab:*external-agent?*
// true; see ../README.md).  The script tells the lab of the prompt,
// which answers with the system prompt its own agent gets and the
// address of the session's tools, an MCP server; runs `claude -p` with
// that prompt in place of Claude Code's own, every built-in tool off and
// the lab's tools allowed; passes the agent's progress and its reply to
// the lab's log; and prints where to watch.  Follow-ups in a session
// resume the same Claude Code conversation.
//
// Claude Code is signed in however you signed it in (`claude auth
// login`, or an API key in its environment); nothing here reads or
// handles its credentials.  This drives your own Claude Code for your
// own development.  A lab that serves visitors uses the gate.
//
// Needs node 18 or later and nothing else.

import { spawn } from 'node:child_process';
import { chmodSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { homedir, tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { randomUUID } from 'node:crypto';
import { createInterface } from 'node:readline';

const SERVER = 'prompt-lab';            // the MCP server's name: tools are mcp__prompt-lab__<name>

function usage(code) {
  const text = readFileSync(new URL(import.meta.url), 'utf8');
  const lines = text.split('\n');
  const from = lines.findIndex((l) => l.startsWith('// claude-code.mjs'));
  const to = lines.findIndex((l, i) => i > from && !l.startsWith('//'));
  console.log(lines.slice(from, to).map((l) => l.replace(/^\/\/ ?/, '')).join('\n'));
  process.exit(code);
}

// --- the command line ------------------------------------------------------

const opt = { lab: process.env.PROMPT_LAB_URL || 'http://localhost:9080/prompt-lab',
              claude: process.env.CLAUDE || 'claude' };
const words = [];
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  const value = () => { if (++i >= process.argv.length) usage(2); return process.argv[i]; };
  if (a === '--help' || a === '-h') usage(0);
  else if (a === '--lab') opt.lab = value();
  else if (a === '--page') opt.page = value();
  else if (a === '--out') opt.out = value();
  else if (a === '--session') opt.session = value();
  else if (a === '--continue') opt.continue = true;
  else if (a === '--model') opt.model = value();
  else if (a === '--effort') opt.effort = value();
  else if (a === '--claude') opt.claude = value();
  else if (a === '--raw') opt.raw = true;
  else if (a.startsWith('--')) { console.error(`Unknown option ${a}`); usage(2); }
  else words.push(a);
}
const prompt = words.join(' ').trim();
if (!prompt) usage(2);
const lab = opt.lab.replace(/\/+$/, '');
const origin = new URL(lab).origin;

// --- what this script remembers: per lab, the sessions it opened -----------
// (their owner keys, and the Claude Code conversation each one is)

const stateFile = process.env.PROMPT_LAB_STATE
  || join(process.env.XDG_STATE_HOME || join(homedir(), '.local', 'state'), 'prompt-lab', 'external.json');
function readState() { try { return JSON.parse(readFileSync(stateFile, 'utf8')); } catch { return {}; } }
function writeState(state) {
  mkdirSync(dirname(stateFile), { recursive: true });
  writeFileSync(stateFile, JSON.stringify(state, null, 1), { mode: 0o600 });
  chmodSync(stateFile, 0o600);          // it holds the sessions' owner keys
}
const state = readState();
const mine = state[lab] || (state[lab] = { last: null, sessions: {} });
let sessionId = opt.session || (opt.continue ? mine.last : null);
if (opt.continue && !sessionId) { console.error('No session to continue at this lab yet.'); process.exit(1); }
let known = sessionId ? mine.sessions[sessionId] : null;
if (sessionId && !known) {
  console.error(`Session ${sessionId} was not opened by this script: its owner key is not here.`);
  process.exit(1);
}

// --- the lab's side --------------------------------------------------------

async function tell(body) {
  const headers = { 'Content-Type': 'application/json' };
  if (known) headers['X-Prompt-Lab-Owner'] = known.owner;
  const response = await fetch(`${lab}/api/agent`, { method: 'POST', headers, body: JSON.stringify(body) });
  let json = null;
  try { json = await response.json(); } catch { /* not JSON: an error page */ }
  return { status: response.status, json: json || {} };
}

let brief;
try {
  brief = await tell({ event: 'prompt', text: prompt, session: sessionId || undefined });
} catch (e) {
  console.error(`The lab at ${lab} cannot be reached: ${e.cause ? e.cause.message : e.message}`);
  process.exit(1);
}
if (brief.status === 404 && !sessionId) {
  console.error(`${lab} takes no external agent (${brief.json.error || 'not found'}).\n`
    + 'Set prompt-lab:*external-agent?* to t in the image that serves it.');
  process.exit(1);
}
if (brief.status !== 200) {
  console.error(`The lab refused the prompt (${brief.status}): ${brief.json.error || 'no reason given'}`);
  process.exit(1);
}
brief = brief.json;
const resumed = !!known;
if (!known) {
  sessionId = brief.session;
  known = mine.sessions[sessionId] = { owner: brief.owner, claude: randomUUID() };
}
mine.last = sessionId;
writeState(state);

const pageUrl = `${(opt.page || process.env.PROMPT_LAB_PAGE_URL || lab).replace(/\/+$/, '')}?session=${sessionId}`;
console.log(`session  ${sessionId}${resumed ? ' (continued)' : ''}`);
console.log(`watch    ${pageUrl}`);

// everything said to the lab from here on goes in order, and a failure
// to say it never stops the agent; the promise answers whether the lab
// took it
let told = Promise.resolve();
const report = (body) => {
  told = told.then(() => tell({ ...body, session: sessionId }))
    .then((r) => {
      if (r.status !== 200) console.error(`(the lab answered ${r.status} to "${body.event}")`);
      return r.status === 200;
    })
    .catch((e) => { console.error(`(the lab did not hear "${body.event}": ${e.message})`); return false; });
  return told;
};

// --- Claude Code -----------------------------------------------------------

const work = mkdtempSync(join(tmpdir(), 'prompt-lab-agent-'));   // an empty directory to run in
chmodSync(work, 0o700);
const systemFile = join(work, 'system.txt');
const mcpFile = join(work, 'mcp.json');
const tools = brief.tools.map((name) => `mcp__${SERVER}__${name}`);
writeFileSync(systemFile, `${brief.system}\n\nYour tools are served over MCP: `
  + `${brief.tools.join(', ')} reach you as ${tools.join(', ')}.  There are no others.\n`);
writeFileSync(mcpFile, JSON.stringify({ mcpServers: { [SERVER]: {
  type: 'http', url: origin + brief.mcp, headers: { 'X-Prompt-Lab-Owner': known.owner } } } }), { mode: 0o600 });

const model = opt.model || brief.model;
const effort = opt.effort || brief.effort;
const args = ['-p', prompt,
  '--system-prompt-file', systemFile,
  '--mcp-config', mcpFile, '--strict-mcp-config',
  '--tools', '',                                    // none of Claude Code's own
  '--allowedTools', tools.join(','),
  '--model', model, '--effort', effort,
  '--max-turns', String(brief.max_rounds + 1),
  '--output-format', 'stream-json', '--verbose',
  ...(resumed ? ['--resume', known.claude] : ['--session-id', known.claude])];
console.log(`agent    ${opt.claude} -p, ${model}, effort ${effort}, ${brief.tools.length} tools\n`);

const child = spawn(opt.claude, args, { cwd: work, stdio: ['ignore', 'pipe', 'pipe'] });
let stderr = '';
child.stderr.on('data', (chunk) => { stderr += chunk; });

let result = null;
let pending = [];                 // text said before a tool call: progress
let interrupted = false;
const flush = () => {
  const text = pending.join('\n').trim();
  pending = [];
  if (text) report({ event: 'text', text });
};
const clip = (text, n) => { const s = String(text).replace(/\s+/g, ' ').trim(); return s.length > n ? s.slice(0, n) + '...' : s; };

createInterface({ input: child.stdout }).on('line', (line) => {
  if (opt.raw) console.log(line);
  let event;
  try { event = JSON.parse(line); } catch { return; }
  if (event.type === 'system' && event.subtype === 'init') {
    const server = (event.mcp_servers || []).find((s) => s.name === SERVER);
    if (!server || server.status !== 'connected') {
      console.error(`Claude Code did not connect to the lab's tools (${server ? server.status : 'server missing'}).`);
    }
    const others = (event.tools || []).filter((t) => !t.startsWith(`mcp__${SERVER}__`));
    if (others.length) console.error(`(Claude Code also has: ${others.join(', ')})`);
  } else if (event.type === 'assistant' && event.message && !event.is_api_error_message) {
    for (const block of event.message.content || []) {
      if (block.type === 'text' && block.text.trim()) {
        pending.push(block.text);
        console.log(block.text.trim() + '\n');
      } else if (block.type === 'tool_use') {
        flush();
        const name = block.name.replace(`mcp__${SERVER}__`, '');
        const input = block.input || {};
        const gist = input.source ? `${input.source.split('\n').length} lines`
          : clip(Object.values(input).map((v) => typeof v === 'string' ? v : JSON.stringify(v)).join(' '), 70);
        console.log(`  -> ${name}${gist ? '  ' + gist : ''}`);
      }
    }
  } else if (event.type === 'user' && event.message && Array.isArray(event.message.content)) {
    for (const block of event.message.content) {
      if (block.type !== 'tool_result') continue;
      const parts = Array.isArray(block.content) ? block.content : [{ type: 'text', text: String(block.content || '') }];
      const said = parts.map((p) => p.type === 'text' ? p.text : `[${p.type}]`).join(' ');
      console.log(`     ${block.is_error ? 'ERROR ' : ''}${clip(said, 160)}\n`);
    }
  } else if (event.type === 'result') {
    result = event;
  }
});

process.on('SIGINT', () => { interrupted = true; child.kill('SIGINT'); });

const exit = await new Promise((resolve) => {
  child.on('error', (e) => { stderr += `${opt.claude}: ${e.message}\n`; resolve(127); });
  child.on('close', (code) => resolve(code));
});
rmSync(work, { recursive: true, force: true });

// --- the end of the prompt -------------------------------------------------

let ok = result && result.subtype === 'success' && !result.is_error && !interrupted;
if (ok) {
  pending = [];                   // the last text is the reply itself
  // Claude Code finishing is not the build finishing: a lab that went
  // away under it (its host restarted, the session with it) takes no
  // reply, and whatever the agent thinks it built is not there
  if (!await report({ event: 'done', text: result.result || '', usage: result.usage })) {
    ok = false;
    console.error('The lab did not take the agent\'s reply: the session is gone (did its host restart?). '
      + 'Nothing of this build is kept there.');
  }
} else {
  flush();
  const why = interrupted ? 'Interrupted.'
    : result ? `Claude Code stopped (${result.terminal_reason || result.subtype})`
               + (result.result ? `: ${clip(result.result, 300)}` : '.')
    : `Claude Code exited ${exit} with no result${stderr ? ': ' + clip(stderr, 300) : '.'}`;
  await report({ event: 'stopped', text: why });
  console.error(why);
  if (result && /log(ged)? ?in|api key|authenticat/i.test(result.result || '')) {
    console.error(`Claude Code is not signed in here.  Sign it in with: ${opt.claude} auth login`);
  }
}
if (result) {
  const u = result.usage || {};
  const n = (x) => Number(x || 0).toLocaleString('en-US');
  console.log(`turns    ${result.num_turns ?? '?'}, ${Math.round((result.duration_ms || 0) / 1000)} s`);
  console.log(`tokens   in ${n(u.input_tokens)}, out ${n(u.output_tokens)}, `
    + `cache read ${n(u.cache_read_input_tokens)}, cache write ${n(u.cache_creation_input_tokens)}`);
  if (typeof result.total_cost_usd === 'number') {
    console.log(`cost     $${result.total_cost_usd.toFixed(4)} at API prices, Claude Code's own estimate `
      + '(a subscription sign-in is not billed by the token)');
  }
}

// where the model is: the file the lab keeps (as the lab's own host names
// it), and a copy of its source where --out asked for one
let kept = null;
try {
  const response = await fetch(`${lab}/api/state?session=${sessionId}`,
                               { headers: { 'X-Prompt-Lab-Owner': known.owner } });
  if (response.ok) kept = await response.json();
} catch { /* the lab went away: nothing to add */ }
const source = kept && typeof kept.model_source === 'string' ? kept.model_source.trim() : '';
if (source && kept.model_file) console.log(`model    ${kept.model_file}`);
if (opt.out) {
  if (ok && source) {
    writeFileSync(opt.out, `;; Built by the prompt lab's modeling agent: session ${sessionId} at ${lab}.\n`
      + ";; (make-object 'model) builds it.\n\n(in-package :gdl-user)\n\n" + source + '\n');
    console.log(`wrote    ${opt.out}`);
  } else {
    console.error(`--out: ${opt.out} was not written (${ok ? 'the session has no model' : 'the build did not finish'}).`);
    ok = false;                   // asked for a file, and there is none
  }
}
console.log(`watch    ${pageUrl}`);
process.exit(ok ? 0 : 1);
