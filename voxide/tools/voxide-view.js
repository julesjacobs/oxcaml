#!/usr/bin/env node
"use strict";

// voxide-view -- a headless, textual mirror of the vox2 browser IDE.
//
// It loads the REAL frontend (index.html for its element set, the vendored
// CodeMirror core + mllike + vox-mode.js for the REAL tokenizer, and app.js
// verbatim) and runs it against a REAL running server (server.py).  It then
// drives that frontend the way a user would -- open a file from the explorer,
// move the cursor, edit the buffer, or select a backend and wait for the
// automatic round -- then prints, as deterministic text, exactly what the user
// would see:
// the editor with its error/obligation underlines shown inline, the proof
// pane, the verdict legend, the diagnostics list, the type-at-cursor and
// signature panes, the verification banner, the active toggles, and any
// confirm() guard the frontend raised.
//
// The one thing modelled rather than executed is CodeMirror's on-screen
// editor WIDGET: there is no browser or jsdom on this box, so fromTextArea is
// replaced by a faithful document/marker model.  app.js runs unmodified
// against it, so every user-visible decision app.js makes -- which span gets
// which severity underline, which obligation the pane shows for the caret,
// the status text, the adapter normalisation -- is the real code.  The real
// tokenizer still runs, so the pane/goal text is what the browser renders
// (colour is the only thing text cannot show; see tools/README.md).
//
// Usage: see tools/README.md  (or `voxide-view.js --help`).

const fs = require("fs");
const path = require("path");
const vm = require("vm");
const cp = require("child_process");

// Captured before we shadow the globals inside the sandbox.
const realSetImmediate = setImmediate;
const realFetch = typeof fetch === "function" ? fetch : null;

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

function parseArgs(argv) {
  const opts = {
    server: "http://127.0.0.1:8000",
    compiler: null,
    replay: null,
    frontend: path.resolve(__dirname, ".."),
    theme: "dark",
    compact: "on",
    sidebar: "shown",
    confirm: "yes",
    tokenize: true,
    section: null,
    redact: false,
    commands: [],
  };
  const positional = [];
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    const next = () => argv[++i];
    switch (a) {
      case "--help":
      case "-h":
        opts.help = true;
        break;
      case "--server": opts.server = next(); break;
      case "--compiler": opts.compiler = path.resolve(next()); break;
      case "--replay": opts.replay = path.resolve(next()); break;
      case "--frontend": opts.frontend = path.resolve(next()); break;
      case "--theme": opts.theme = next(); break;
      case "--compact": opts.compact = next(); break;
      case "--sidebar": opts.sidebar = next(); break;
      case "--confirm": opts.confirm = next(); break;
      case "--section": opts.section = next(); break;
      case "--no-tokenize": opts.tokenize = false; break;
      case "--redact": opts.redact = true; break;
      case "-e": opts.commands.push(next()); break;
      case "--script": {
        const f = next();
        opts.commands.push(
          f === "-" ? fs.readFileSync(0, "utf8") : fs.readFileSync(f, "utf8")
        );
        break;
      }
      default:
        positional.push(a);
    }
  }
  if (positional.length) opts.commands.push(positional.join(" "));
  return opts;
}

// Split the gathered command text into individual commands.  Commands are
// separated by newlines or ';'.  '#' starts a line comment.
function splitCommands(chunks) {
  const out = [];
  for (const chunk of chunks) {
    for (const raw of chunk.split(/[\n;]+/)) {
      const line = raw.replace(/\s+#.*$/, "").trim();
      if (line && !line.startsWith("#")) out.push(line);
    }
  }
  return out;
}

const HELP = `voxide-view -- headless textual view of the vox2 IDE

  voxide-view.js [options] [command ...]

Options
  --server URL        running server.py base URL (default http://127.0.0.1:8000)
  --compiler PATH     socket-free server.py bridge to this compiler
  --replay FILE       replay compiler-wrapper responses from a JSON artifact
  --frontend DIR      voxide dir holding index.html/app.js (default: parent of tools/)
  --theme dark|light  initial persisted theme (applied before first paint)
  --compact on|off    initial "compact" proof-pane toggle
  --sidebar shown|hidden   initial explorer visibility
  --confirm yes|no    default answer to a discard-edits confirm() guard
  --no-tokenize       skip the real CodeMirror tokenizer (text is identical)
  --redact            print "<server>"/"<frontend>" in the header (stable goldens)
  --section NAME      render only one section (editor, proof, diagnostics, ...)
  -e "CMD"            a command (repeatable); ';' or newline separates commands
  --script FILE       read commands from FILE ('-' for stdin)

Commands (each settles the frontend, then the final view is printed)
  open <path>         open a file from the explorer (e.g. examples/abs.ml)
  open! <path>        open it even if the buffer has unsaved edits (skip guard)
  openfile <path>     open any allowlisted path directly (bypasses the tree)
  workspace [unit]    enter the multi-file demo (optionally with <unit> active)
  tab <unit>          switch the active unit within the workspace
  source-file <file>  load a LOCAL file as a scratch buffer (no explorer path)
  source <text>       set the buffer to one line of text (scratch buffer)
  cursor L:C          move the caret to line L, column C (1-based, as shown)
  backend lean|z3|oxsmt|cross   select the verification backend
  toggle theme|compact|sidebar    flip a toggle the way a click would
  confirm yes|no      answer the NEXT discard-edits guard this way
  render [section]     print the view now (also printed once at the end)

Exit status is non-zero if the frontend raised an error or failed to settle.`;

// ---------------------------------------------------------------------------
// A minimal DOM the real app.js drives.
// ---------------------------------------------------------------------------

class ClassList {
  constructor(el) { this.el = el; }
  _tokens() { return this.el.className.split(/\s+/).filter(Boolean); }
  add(c) { const t = this._tokens(); if (!t.includes(c)) { t.push(c); this.el.className = t.join(" "); } }
  remove(c) { this.el.className = this._tokens().filter((x) => x !== c).join(" "); }
  contains(c) { return this._tokens().includes(c); }
  toggle(c, force) {
    const has = this.contains(c);
    const on = force === undefined ? !has : !!force;
    if (on) this.add(c); else this.remove(c);
    return on;
  }
}

class El {
  constructor(tag) {
    this.tagName = String(tag || "div").toUpperCase();
    this.children = [];
    this._text = null;
    this._html = null;
    this.id = "";
    this.title = "";
    this.type = "";
    this.value = "";
    this.checked = false;
    this.hidden = false;
    this.className = "";
    this.dataset = {};
    this.style = {};
    this.attributes = {};
    this.parentNode = null;
    this.nodeType = 1;
    this._listeners = {};
    this.classList = new ClassList(this);
  }
  get childNodes() { return this.children; }
  get textContent() {
    if (this._text !== null) return this._text;
    if (this._html !== null) return stripTags(this._html);
    return this.children.map((c) => (c.nodeType === 3 ? c._text : c.textContent)).join("");
  }
  set textContent(v) { this._text = String(v); this._html = null; this.children = []; }
  get innerHTML() { return this._html !== null ? this._html : ""; }
  set innerHTML(v) { this._html = String(v); this._text = null; this.children = []; }
  appendChild(child) { child.parentNode = this; this.children.push(child); this._text = null; this._html = null; return child; }
  append(...nodes) { nodes.forEach((n) => this.appendChild(typeof n === "string" ? { nodeType: 3, _text: n } : n)); }
  replaceChildren(...nodes) { this.children = []; this._text = null; this._html = null; nodes.forEach((n) => this.appendChild(n)); }
  removeChild(child) { this.children = this.children.filter((c) => c !== child); return child; }
  setAttribute(name, val) { this.attributes[name] = String(val); if (name === "id") this.id = String(val); }
  getAttribute(name) { return this.attributes[name] != null ? this.attributes[name] : null; }
  addEventListener(type, fn) { (this._listeners[type] || (this._listeners[type] = [])).push(fn); }
  removeEventListener(type, fn) { this._listeners[type] = (this._listeners[type] || []).filter((f) => f !== fn); }
  dispatch(type, event) { (this._listeners[type] || []).forEach((fn) => fn(event || { type })); }
  querySelectorAll(sel) {
    const classes = sel.split(".").filter(Boolean);
    const out = [];
    const walk = (el) => {
      for (const c of el.children || []) {
        if (c.nodeType === 1) {
          if (classes.every((cl) => c.classList.contains(cl))) out.push(c);
          walk(c);
        }
      }
    };
    walk(this);
    return out;
  }
  focus() {}
}

function stripTags(html) {
  return html
    .replace(/<[^>]+>/g, "")
    .replace(/&gt;/g, ">").replace(/&lt;/g, "<").replace(/&amp;/g, "&");
}

// Build the element registry from the REAL index.html: every id-bearing tag
// becomes an El with its tag, class, type, checked/hidden, and initial text,
// so adding an element to index.html surfaces here without editing the tool.
function buildDomFromIndexHtml(html) {
  const byId = {};
  const re = /<([a-zA-Z0-9]+)\b([^>]*?)\bid="([^"]+)"([^>]*)>/g;
  let m;
  while ((m = re.exec(html)) !== null) {
    const tag = m[1];
    const attrs = m[2] + " " + m[4];
    const id = m[3];
    const el = new El(tag);
    el.id = id;
    const cls = /\bclass="([^"]*)"/.exec(attrs);
    if (cls) el.className = cls[1];
    const typ = /\btype="([^"]*)"/.exec(attrs);
    if (typ) el.type = typ[1];
    const val = /\bvalue="([^"]*)"/.exec(attrs);
    if (val) el.value = val[1];
    if (/\bchecked\b/.test(attrs)) el.checked = true;
    if (/\bhidden\b/.test(attrs)) el.hidden = true;
    // Initial text content: the run of non-tag text right after this tag.
    const after = html.slice(re.lastIndex);
    const text = /^([^<]*)</.exec(after);
    if (text && text[1].trim()) el.textContent = text[1].trim();
    byId[id] = el;
  }
  return byId;
}

// ---------------------------------------------------------------------------
// The CodeMirror document/marker model (replaces fromTextArea only).
// ---------------------------------------------------------------------------

function makeCm() {
  let lines = [""];
  let cursor = { line: 0, ch: 0 };
  const options = { readOnly: false };
  const handlers = {};
  const marks = new Set();
  const clamp = (n, lo, hi) => Math.max(lo, Math.min(hi, n));
  function emit(ev) { (handlers[ev] || []).forEach((fn) => fn()); }
  function clampCursor() {
    const line = clamp(cursor.line, 0, lines.length - 1);
    const ch = clamp(cursor.ch, 0, lines[line].length);
    cursor = { line, ch };
  }
  const cm = {
    _marks: marks,
    getValue() { return lines.join("\n"); },
    setValue(v) {
      lines = String(v).split("\n");
      if (!lines.length) lines = [""];
      clampCursor();
      emit("change");
    },
    getLine(n) { return lines[n] !== undefined ? lines[n] : ""; },
    lineCount() { return lines.length; },
    getCursor() { return { line: cursor.line, ch: cursor.ch }; },
    setCursor(pos) {
      cursor = { line: Number(pos.line) || 0, ch: Number(pos.ch) || 0 };
      clampCursor();
      emit("cursorActivity");
    },
    markText(from, to, o) {
      const mark = {
        type: "range", from, to,
        className: (o && o.className) || "",
        title: (o && o.title) || "",
        cleared: false,
        clear() { this.cleared = true; marks.delete(mark); },
      };
      marks.add(mark);
      return mark;
    },
    setBookmark(pos, o) {
      const w = o && o.widget;
      const mark = {
        type: "bookmark", from: pos,
        className: (w && w.className) || "",
        title: (w && w.title) || "",
        widgetText: (w && w.textContent) || "^",
        cleared: false,
        clear() { this.cleared = true; marks.delete(mark); },
      };
      marks.add(mark);
      return mark;
    },
    on(ev, fn) { (handlers[ev] || (handlers[ev] = [])).push(fn); },
    addKeyMap(map) { cm._keymap = map; },
    focus() { cm._focused = true; },
    getOption(n) { return options[n]; },
    setOption(n, v) { options[n] = v; },
    getWrapperElement() { return new El("div"); },
    // Viewport/widget operations a textual view does not model: exitDocMode
    // re-measures the editor with refresh() when leaving a doc, and the
    // hypothesis-jump scrolls the caret into view.  Both are no-ops here (the
    // buffer/cursor state they affect is already tracked above).
    refresh() {},
    scrollIntoView() {},
  };
  return cm;
}

// ---------------------------------------------------------------------------
// Sandbox globals: fake timers, tracked fetch, localStorage, confirm.
// ---------------------------------------------------------------------------

function directFetch(target, request, opts) {
  return new Promise((resolve, reject) => {
    const method = String(request.method || "GET").toUpperCase();
    const args = [
      path.join(opts.frontend, "server.py"),
      "--ocamlc",
      opts.compiler,
      "--one-shot",
      method,
      target,
    ];
    try {
      let stdout;
      try {
        stdout = cp.execFileSync("python3", args, {
          input: request.body || "",
          encoding: "utf8",
          maxBuffer: 16 * 1024 * 1024,
        });
      } catch (error) {
        // See the pane tests: the managed sandbox can return EPERM despite a
        // completed status-0 child and fully captured stdout.
        if (error && error.status === 0 && error.stdout != null) stdout = error.stdout;
        else throw error;
      }
      const envelope = JSON.parse(String(stdout));
      const status = Number(envelope.status) || 500;
      const body = envelope.text != null
        ? String(envelope.text)
        : JSON.stringify(envelope.json || {});
      resolve({
        ok: status >= 200 && status < 300,
        status,
        statusText: String(status),
        json: () => Promise.resolve(
          envelope.json != null ? envelope.json : JSON.parse(body)
        ),
        text: () => Promise.resolve(body),
      });
    } catch (error) {
      reject(error);
    }
  });
}

function replayFetch(target, request, replay) {
  const method = String(request.method || "GET").toUpperCase();
  const defaultPath = replay.default_path || "examples/capture.ml";
  const defaultName = replay.default_name || path.basename(defaultPath, path.extname(defaultPath));
  const response = (status, value, isText) => Promise.resolve({
    ok: status >= 200 && status < 300,
    status,
    statusText: String(status),
    json: () => Promise.resolve(isText ? JSON.parse(value) : value),
    text: () => Promise.resolve(isText ? String(value) : JSON.stringify(value)),
  });
  if (method === "GET" && target === "/ls") {
    return response(200, replay.tree || {
      roots: [{
        id: "examples",
        name: "examples",
        kind: "dir",
        children: [{ id: defaultPath, name: path.basename(defaultPath), kind: "ml" }],
      }],
    });
  }
  if (method === "GET" && target === "/examples") {
    return response(200, replay.examples || {
      examples: [{ name: defaultName, title: defaultName, default: true, cursor: replay.cursor || 1 }],
    });
  }
  if (method === "GET" && target === "/config") {
    if (replay.config) return response(200, replay.config);
    return response(200, {
      backend_options: ["lean"],
      backend_solver_configuration: { z3: false, oxsmt: false },
      default_backend: "lean",
    });
  }
  if (method === "GET" && target.startsWith("/file?path=")) {
    const requestedPath = decodeURIComponent(target.slice("/file?path=".length));
    const files = replay.files || {};
    const source = Object.prototype.hasOwnProperty.call(files, requestedPath)
      ? files[requestedPath]
      : replay.source || "";
    return response(200, source, true);
  }
  if (method === "POST") {
    const body = request.body ? JSON.parse(request.body) : {};
    const backend = body.backend || "lean";
    const bucket = target === "/workspace-check"
      ? replay.workspace
      : target === "/vcs"
      ? replay.vcs
      : replay.check;
    const workspaceKey = Array.isArray(body.files)
      ? body.files.map((file) => file && file.name).join(",")
      : "";
    const layerBucket = target === "/workspace-check" &&
      replay.workspace_layers && replay.workspace_layers[backend];
    const value = layerBucket && layerBucket[workspaceKey]
      ? layerBucket[workspaceKey]
      : bucket && bucket[backend];
    if (!value) return response(404, { error: "missing replay for " + target + ":" + backend });
    const copied = JSON.parse(JSON.stringify(value));
    copied.revision = body.revision || 0;
    const completed = response(200, copied);
    if (target === "/check" && Number(replay.latency_ms) > 0) {
      return new Promise((resolve) =>
        setTimeout(() => resolve(completed), Number(replay.latency_ms))
      );
    }
    return completed;
  }
  return response(404, { error: "not found" });
}

function makeSandbox(opts, dom, cm) {
  const timers = new Map();
  let timerSeq = 1;
  const inflight = new Set();
  const store = new Map();
  if (opts.theme === "light") store.set("voxide-theme", "light");
  if (opts.compact === "off") store.set("voxide-compact", "off");
  if (opts.sidebar === "hidden") store.set("voxide-sidebar", "hidden");

  const dialogs = [];
  let confirmAnswer = null; // one-shot override; else default from opts.
  const errors = [];

  const documentElement = new El("html");
  const body = new El("body");

  const document = {
    documentElement,
    body,
    getElementById: (id) => dom[id] || null,
    createElement: (tag) => new El(tag),
    createTextNode: (t) => ({ nodeType: 3, _text: String(t), get textContent() { return this._text; } }),
    createDocumentFragment: () => new El("fragment"),
    createRange: () => ({ setStart() {}, setEnd() {}, getBoundingClientRect: () => ({}), getClientRects: () => [] }),
    addEventListener() {},
    removeEventListener() {},
    querySelectorAll: (sel) => body.querySelectorAll(sel),
  };

  const localStorage = {
    getItem: (k) => (store.has(k) ? store.get(k) : null),
    setItem: (k, v) => store.set(k, String(v)),
    removeItem: (k) => store.delete(k),
    clear: () => store.clear(),
  };

  const sandbox = {
    navigator: { userAgent: "voxide-view", platform: "node", vendor: "", maxTouchPoints: 0 },
    document,
    localStorage,
    console,
    setTimeout: (fn) => { const id = timerSeq++; timers.set(id, fn); return id; },
    clearTimeout: (id) => { timers.delete(id); },
    setInterval: () => 0,
    clearInterval: () => {},
    confirm: (msg) => {
      const dflt = opts.confirm !== "no";
      const answer = confirmAnswer === null ? dflt : confirmAnswer;
      confirmAnswer = null;
      dialogs.push({ kind: "confirm", message: String(msg), answer });
      return answer;
    },
    alert: (msg) => { dialogs.push({ kind: "alert", message: String(msg) }); },
    fetch: (url, o) => {
      const full = /^https?:/.test(url) ? url : opts.server + url;
      const request = opts.replayData
        ? replayFetch(String(url), o || {}, opts.replayData)
        : opts.compiler
        ? directFetch(String(url), o || {}, opts)
        : realFetch(full, o);
      const tracked = request.finally(() => inflight.delete(tracked));
      inflight.add(tracked);
      return tracked;
    },
  };
  sandbox.window = sandbox;
  sandbox.self = sandbox;
  sandbox.globalThis = sandbox;

  // `errors` is the per-render display buffer (drained when shown, like
  // dialogs). `errorTotal` is a monotonic count for the exit decision, so
  // displaying an error never clears the signal a test gates on.
  let errorTotal = 0;
  const state = {
    timers, inflight, dialogs, errors,
    recordError: (e) => { errors.push(e); errorTotal += 1; },
    getErrorTotal: () => errorTotal,
    getStored: (key) => (store.has(key) ? store.get(key) : null),
    setConfirm: (v) => { confirmAnswer = v; },
  };
  return { sandbox, state };
}

// Fire all due timers and let real I/O / promises drain until quiescent.
async function settle(state) {
  const { timers, inflight } = state;
  const tick = () => new Promise((r) => realSetImmediate(r));
  for (let i = 0; i < 2000; i++) {
    if (timers.size) {
      const entries = [...timers.entries()];
      timers.clear();
      for (const [, fn] of entries) {
        try { fn(); } catch (e) { state.recordError(e); }
      }
    }
    // Block on outstanding network requests rather than busy-spinning: a
    // real /check can take seconds (Lean discharge) and must not time out.
    if (inflight.size) {
      await Promise.allSettled([...inflight]);
      await tick(); // let awaiting continuations (response.json, applyCheck) run
      continue;
    }
    await tick();
    if (timers.size === 0 && inflight.size === 0) {
      await tick(); // drain trailing microtasks
      if (timers.size === 0 && inflight.size === 0) return;
    }
  }
  throw new Error("frontend did not settle (possible network hang or loop)");
}

// ---------------------------------------------------------------------------
// Load the real frontend into the sandbox.
// ---------------------------------------------------------------------------

function loadFrontend(opts, sandbox, cm) {
  const dir = opts.frontend;
  const read = (p) => fs.readFileSync(path.join(dir, p), "utf8");
  vm.createContext(sandbox);
  const run = (code, file) => vm.runInContext(code, sandbox, { filename: file });

  let tokenizer = "none";
  if (opts.tokenize) {
    try {
      run(read("vendor/codemirror/codemirror.js"), "codemirror.js");
      run(read("vendor/codemirror/mode/mllike/mllike.js"), "mllike.js");
      run(read("vox-mode.js"), "vox-mode.js");
      tokenizer = typeof sandbox.CodeMirror.voxTokenize === "function" ? "real" : "loaded";
    } catch (e) {
      tokenizer = "none";
    }
  }
  if (!sandbox.CodeMirror) {
    sandbox.CodeMirror = {
      defineMode() {}, defineMIME() {}, getMode() { return {}; },
      startState() { return {}; }, copyState() { return {}; },
    };
  }
  // Model the editor widget; keep the real tokenizer/getMode intact.
  sandbox.CodeMirror.fromTextArea = () => cm;

  // The proof pane renders from the shared pane_model.js (adapter + view-model
  // as globals); load it into the same context before app.js.
  run(read("pane_model.js"), "pane_model.js");
  run(read("app.js"), "app.js");
  return tokenizer;
}

// ---------------------------------------------------------------------------
// Commands
// ---------------------------------------------------------------------------

async function runCommand(line, ctx) {
  const { sandbox, state, cm, dom, opts } = ctx;
  const parts = line.split(/\s+/);
  const cmd = parts[0];
  const rest = line.slice(cmd.length).trim();
  const vox = sandbox.__voxide;

  switch (cmd) {
    case "open":
    case "open!": {
      const before = state.dialogs.length;
      const force = cmd === "open!";
      // Bare names use the manifest-aware path, which also opens curated
      // multi-file examples as a workspace. Explicit paths keep bypassing the
      // manifest and open through the allowlisted file endpoint.
      const ok = rest.includes("/")
        ? await vox.openFile(
            {
              path: rest,
              kind: rest.endsWith(".md")
                ? "doc"
                : rest.endsWith(".mli")
                ? "mli"
                : "ml",
            },
            force
          )
        : await vox.loadExample(rest, force);
      if (ok === false) {
        // A declined discard-edits guard is expected behaviour, not an error.
        const declined = state.dialogs.slice(before).some((d) => d.kind === "confirm" && !d.answer);
        if (!declined) state.recordError(new Error("open failed for '" + rest + "' (not an allowlisted file?)"));
      }
      break;
    }
    case "openfile": {
      const p = rest;
      const ok = await vox.openFile({ path: p, kind: p.endsWith(".md") ? "doc" : "ml" }, true);
      // force=true means no guard, so a false return is always a load failure.
      if (ok === false) state.recordError(new Error("openfile failed for '" + p + "' (not an allowlisted file?)"));
      break;
    }
    case "workspace": {
      // Enter the multi-file workspace demo, optionally with a named unit
      // active (defaults to the first unit).
      if (!vox.openWorkspace) throw new Error("frontend has no multi-file workspace");
      vox.openWorkspace(rest || undefined);
      break;
    }
    case "tab": {
      // Switch the active unit within the workspace.
      if (!rest) throw new Error("tab needs a unit name (e.g. tab Demo.ml)");
      if (!vox.switchTab) throw new Error("frontend has no multi-file workspace");
      vox.switchTab(rest);
      break;
    }
    case "source": {
      cm.setValue(rest);
      break;
    }
    case "source-file": {
      const p = path.isAbsolute(rest) ? rest : path.resolve(process.cwd(), rest);
      cm.setValue(fs.readFileSync(p, "utf8"));
      break;
    }
    case "cursor": {
      const mm = /^(\d+):(\d+)$/.exec(rest);
      if (!mm) throw new Error(`bad cursor '${rest}' (want L:C, 1-based)`);
      cm.setCursor({ line: Number(mm[1]) - 1, ch: Number(mm[2]) - 1 });
      break;
    }
    case "backend": {
      if (!["lean", "z3", "oxsmt", "cross"].includes(rest))
        throw new Error(`bad backend '${rest}'`);
      const select = dom["backend-select"];
      const offered = (select.children || []).some((o) => o.value === rest);
      if (!offered) throw new Error(`backend '${rest}' is not offered by this compiler`);
      select.value = rest;
      select.dispatch("change");
      break;
    }
    case "confirm": {
      state.setConfirm(rest !== "no");
      return; // no settle needed
    }
    case "toggle": {
      if (rest === "theme") dom["theme-button"].dispatch("click");
      else if (rest === "sidebar") dom["sidebar-button"].dispatch("click");
      else if (rest === "compact") {
        const box = dom["compact-box"];
        box.checked = !box.checked;
        box.dispatch("change");
      }
      else throw new Error(`unknown toggle '${rest}'`);
      break;
    }
    case "render": {
      await settle(state);
      process.stdout.write(renderView(ctx, rest || opts.section));
      return;
    }
    case "settle":
      break;
    default:
      throw new Error(`unknown command '${cmd}'`);
  }
  await settle(state);
}

// ---------------------------------------------------------------------------
// Rendering the textual view
// ---------------------------------------------------------------------------

function htmlToText(html) {
  if (!html) return "";
  let s = html;
  s = s.replace(/<span class="badge[^"]*"[^>]*>([^<]*)<\/span>/g, (m, t) => "[" + t + "]");
  // The grey CONTEXT token (off-obligation, full view) sits in its own block;
  // put it on its own line.  (The obligation verdict now rides the goal line's
  // colour + leading glyph -- there is no separate token div or swatch there,
  // and the goal's `⊢` is literal text, so it needs no special handling.)
  s = s.replace(/<div class="verdict-token[^"]*">/g, "\n");
  // A hypothesis the proof did not use is always dimmed on screen; surface
  // that in text. The
  // prover-style pane uses a "hyprow" row (with an optional hyp-unused class);
  // match either that or the older "hyp" row.
  s = s.replace(/<div class="[^"]*hyp-unused[^"]*"[^>]*>/g, "\n(unused) ");
  // The prover-style pane demotes the raw predicate and generated Lean to
  // <details> disclosures; put each disclosure, its summary, and its <pre>
  // body on their own lines so they do not run into the turnstile goal.
  s = s.replace(/<summary>/g, "\n");
  s = s.replace(/<(h3|div|p|details|pre)\b[^>]*>/g, "\n");
  s = s.replace(/<[^>]+>/g, "");
  s = s.replace(/&gt;/g, ">").replace(/&lt;/g, "<").replace(/&amp;/g, "&");
  s = s.replace(/[ \t]+\n/g, "\n").replace(/\n{3,}/g, "\n\n").replace(/^\n+/, "").replace(/\n+$/, "");
  return s;
}

// Serialize the read-only doc viewer (a tree of createElement nodes, so there
// is no innerHTML to scrape) to readable text: block elements start a new
// line, list items get a bullet.  This is what the user reads in the doc pane.
function docNodeText(node) {
  if (!node) return "";
  if (node.nodeType === 3) return node._text || "";
  const tag = node.tagName || "";
  const inner = (node.children || []).map(docNodeText).join("");
  if (tag === "LI") return "\n- " + inner;
  if (tag === "BR") return "\n";
  if (/^(H[1-6]|P|DIV|UL|OL|PRE|BLOCKQUOTE|SECTION|ARTICLE|DETAILS)$/.test(tag)) {
    return "\n" + inner + "\n";
  }
  return inner;
}

function renderDoc(el) {
  const text = docNodeText(el)
    .replace(/[ \t]+\n/g, "\n")
    .replace(/\n{3,}/g, "\n\n")
    .replace(/^\n+/, "")
    .replace(/\n+$/, "");
  return text || "(empty document)";
}

function markKind(className) {
  if (!className) return "mark";
  if (className.includes("diagnostic-squiggle-verify")) return "verify";
  if (className.includes("diagnostic-squiggle")) return "type";
  if (className.includes("diagnostic-point-verify")) return "verify";
  if (className.includes("diagnostic-point")) return "type";
  // Status may be hyphenated once the real dump lands (not-proved,
  // solver-error), so capture the whole token, not just the first word.
  const m = /vc-([\w-]+)/.exec(className);
  if (m) return "vc:" + m[1];
  return className;
}

function renderEditor(ctx) {
  const { cm, dom } = ctx;
  // In doc mode the on-screen editor is hidden and the rendered Markdown doc
  // is shown instead; mirror that so the transcript reflects what the user
  // actually sees (not the stale editor buffer behind the doc view).
  const docView = dom["doc-view"];
  if (docView && !docView.hidden) return renderDoc(docView);
  const lines = cm.getValue().split("\n");
  const cur = cm.getCursor();
  const marks = [...cm._marks].filter((m) => !m.cleared);
  const width = String(lines.length).length;
  const gut = (n, star) => `${star ? "*" : " "} ${String(n).padStart(width)} | `;
  const pad = " ".repeat(width + 5);
  const out = [];
  for (let i = 0; i < lines.length; i++) {
    out.push(gut(i + 1, i === cur.line) + lines[i]);
    // Marks covering line i. A bookmark anchors on its own line; a range
    // spans from.line..to.line, so a multi-line mark draws a segment on each
    // line it covers -- from.ch on the first, the whole line in between, up to
    // to.ch on the last -- exactly as the browser paints it.
    const segStart = (m) =>
      m.type === "bookmark" || i === m.from.line ? m.from.ch : 0;
    const here = marks
      .filter((m) =>
        m.type === "bookmark"
          ? m.from.line === i
          : m.from.line <= i && i <= m.to.line
      )
      .sort(
        (a, b) =>
          segStart(a) - segStart(b) ||
          markKind(a.className).localeCompare(markKind(b.className))
      );
    for (const m of here) {
      let row;
      let label = "[" + markKind(m.className) + "]";
      if (m.type === "bookmark") {
        row = " ".repeat(m.from.ch) + (m.widgetText || "^");
        if (m.title) label += " " + m.title;
      } else {
        const single = m.from.line === m.to.line;
        const startCh = i === m.from.line ? m.from.ch : 0;
        const endCh = i === m.to.line ? m.to.ch : lines[i].length;
        row = " ".repeat(startCh) + "~".repeat(Math.max(1, endCh - startCh));
        if (single || i === m.from.line) {
          if (m.title) label += " " + m.title;
          if (!single) label += ` (to ${m.to.line + 1}:${m.to.ch + 1})`;
        } else {
          label += " (cont.)";
        }
      }
      out.push(pad + row + "  " + label);
    }
    if (i === cur.line) out.push(pad + " ".repeat(cur.ch) + "^ (cursor)");
  }
  return out.join("\n");
}

function renderTree(ctx) {
  const { dom, sandbox } = ctx;
  const tree = dom["tree"];
  const out = [];
  const active = (sandbox.__voxide && sandbox.__voxide.getCurrentPath()) || null;
  const walk = (el, depth) => {
    for (const c of el.children) {
      if (c.nodeType !== 1) continue;
      if (c.classList.contains("tree-dir")) {
        const label = c.children.find((x) => x.classList && x.classList.contains("tree-dir-label"));
        const nameEl = label && label.children.find((x) => x.classList && x.classList.contains("tree-name"));
        const identity = label && label.children.find(
          (x) => x.classList && x.classList.contains("workspace-identity")
        );
        const identityTitle = identity && identity.children.find(
          (x) => x.classList && x.classList.contains("tree-file-title")
        );
        const identityFilename = identity && identity.children.find(
          (x) => x.classList && x.classList.contains("tree-file-name")
        );
        const labelText = nameEl
          ? nameEl.textContent
          : identityTitle
          ? identityTitle.textContent +
            (identityFilename ? " (" + identityFilename.textContent + ")" : "")
          : "?";
        const collapsed = c.classList.contains("collapsed");
        out.push("  ".repeat(depth) + (collapsed ? "▸ " : "▾ ") + labelText);
        const kids = c.children.find((x) => x.classList && x.classList.contains("tree-children"));
        if (kids) walk(kids, depth + 1);
      } else if (c.classList.contains("tree-file")) {
        const isActive = c.dataset.path === active || c.classList.contains("active");
        const title = c.children.find(
          (x) => x.classList && x.classList.contains("tree-file-title")
        );
        const filename = c.children.find(
          (x) => x.classList && x.classList.contains("tree-file-name")
        );
        let row = "  ".repeat(depth) + (title ? title.textContent : c.textContent);
        if (filename) row += " (" + filename.textContent + ")";
        if (isActive) row += "   (active)";
        if (c.title && c.title !== c.dataset.path) row += "   -- " + c.title;
        out.push(row);
      }
    }
  };
  walk(tree, 0);
  return out.length ? out.join("\n") : "(empty)";
}

function renderDiagnostics(ctx) {
  const { dom } = ctx;
  const el = dom["diagnostics"];
  if (!el.children.length) return el.textContent || "(none)";
  const out = [];
  for (const c of el.children) {
    if (c.nodeType !== 1) continue;
    if (c.classList.contains("diagnostic-ok")) { out.push(c.textContent); continue; }
    const badge = c.children.find((x) => x.classList && x.classList.contains("diagnostic-badge"));
    const badgeText = badge ? badge.textContent : "";
    const rest = c.children
      .filter((x) => x !== badge)
      .map((x) => (x.nodeType === 3 ? x._text : x.textContent))
      .join("");
    out.push((badgeText ? "[" + badgeText + "] " : "") + rest);
  }
  return out.join("\n");
}

function renderLegend(ctx) {
  const { dom } = ctx;
  const el = dom["legend"];
  if (el.hidden) return "(hidden -- shown only when an obligation fails)";
  // The swatches carry the shared verdict labels; the enclosing <details> /
  // <summary> ("verdict key") is browser-only collapse chrome, so read the
  // swatches directly (they are the surface the terminal mirror also lists).
  const swatches = el.querySelectorAll(".leg").map((c) => c.textContent);
  return swatches.length ? swatches.join("   ") : "(empty)";
}

function renderBackendResults(ctx) {
  const { dom } = ctx;
  const select = dom["backend-select"];
  const control = dom["backend-control"];
  const results = dom["backend-results"];
  const options = (select.children || []).map((o) => o.value).filter(Boolean);
  const lines = [
    "selected: " + (select.value || "lean"),
    "available: " + options.join(", "),
    "selector: " + (control.hidden ? "hidden (legacy Lean only)" : "shown"),
  ];
  if (!results.hidden) {
    for (const row of results.children || []) {
      const parts = (row.children || []).map((child) => child.textContent);
      if (parts.length) lines.push(parts.join(" | "));
    }
  }
  return lines.join("\n");
}

// The multi-file tab strip (workspace mode only): each open unit with its
// per-file taxonomy glyph and the active marker, mirroring what the user sees.
function renderWorkspaceTabs(ctx) {
  const { dom, sandbox } = ctx;
  const vox = sandbox.__voxide;
  if (!vox || !vox.isWorkspace || !vox.isWorkspace()) return null;
  const tabsEl = dom["tabs"];
  if (!tabsEl) return null;
  const out = [];
  for (const t of tabsEl.children) {
    if (t.nodeType !== 1) continue;
    const glyph = t.children.find(
      (x) => x.classList && x.classList.contains("tab-status")
    );
    const verdict = glyph
      ? (glyph.className.match(/tab-status-([\w-]+)/) || [])[1]
      : "?";
    const name = t.dataset.file || t.textContent.trim();
    const activeMark = t.classList.contains("tab-active") ? "  (active)" : "";
    out.push("[" + (verdict || "?") + "] " + name + activeMark);
  }
  return out.length ? out.join("\n") : "(no tabs)";
}

// The cross-unit obligations list (workspace mode): obligations anchored in a
// unit other than the active one, shown as jump links in the browser.
function renderCrossUnitSection(ctx) {
  const { dom } = ctx;
  if (dom["compact-box"] && dom["compact-box"].checked) return null;
  const el = dom["cross-unit"];
  if (!el || el.hidden) return null;
  const out = [];
  for (const c of el.children) {
    if (c.nodeType !== 1) continue;
    out.push(c.textContent);
  }
  return out.length ? out.join("\n") : null;
}

function renderProof(ctx) {
  const { dom } = ctx;
  const mode = dom["pane-mode"].textContent;
  const body = htmlToText(dom["pane-body"].innerHTML || dom["pane-body"].textContent);
  const lines = ["Proof pane"];
  if (mode) lines.push("mode: " + mode);
  lines.push(body || "(empty)");
  const details = dom["proof-details"]
    ? htmlToText(dom["proof-details"].innerHTML || "")
    : "";
  if (details) {
    lines.push("");
    lines.push("Local details");
    lines.push(details);
  }
  const cross = renderCrossUnitSection(ctx);
  if (cross) {
    lines.push("");
    lines.push(cross);
  }
  return lines.join("\n");
}

function renderView(ctx, only) {
  const { dom, sandbox, cm, opts, tokenizer, state } = ctx;
  const cur = cm.getCursor();
  const theme = sandbox.document.documentElement.dataset.theme === "light" ? "light" : "dark";
  const sidebarHidden = sandbox.document.body.classList.contains("sidebar-hidden");
  const vox = sandbox.__voxide;
  const inWorkspace = !!(vox && vox.isWorkspace && vox.isWorkspace());
  const inDoc = !!(vox && vox.isDocOpen && vox.isDocOpen());
  const active = inWorkspace
    ? vox.getActiveFile() + " (workspace)"
    : (vox && vox.getCurrentPath()) || "(scratch buffer)";
  const status = dom["status"];
  const verify = dom["verify-output"];
  const savedCompact = state.getStored("voxide-compact");

  const sections = {};
  sections.header = [
    opts.redact
      ? "server: <server>    frontend: <frontend>"
      : "server: " + opts.server + "    frontend: " + opts.frontend,
    "toggles: theme=" + theme +
      "  compact=" + (dom["compact-box"].checked ? "on" : "off") +
      " (saved=" + (savedCompact || "default") + ")" +
      "  sidebar=" + (sidebarHidden ? "hidden" : "shown") +
      "  tokenizer=" + tokenizer,
    inDoc
      ? "controls: document mode (backend/status/output hidden)"
      : "controls: backend=" + (dom["backend-select"].value || "(loading)") +
        "  |  " + status.textContent,
    "backend options: " +
      renderBackendResults(ctx).split("\n").slice(1, 3).join("; "),
    "actions: automatic (no Check or Verify buttons)",
    "status class: " + (status.className || ""),
  ].join("\n");
  const wsTabs = renderWorkspaceTabs(ctx);
  if (wsTabs) sections.tabs = wsTabs;
  sections.explorer = renderTree(ctx);
  sections.editor = renderEditor(ctx);
  sections.proof = inDoc
    ? "Proof pane\n(hidden in document mode)"
    : renderProof(ctx);
  sections.legend = renderLegend(ctx);
  sections.backend = inDoc
    ? "(hidden in document mode)"
    : renderBackendResults(ctx);
  // The STATUS zone's fail-closed roll-up token (depth 0).
  const statusVerdict = dom["status-verdict"];
  sections.status = statusVerdict ? statusVerdict.textContent : "";
  sections.diagnostics = renderDiagnostics(ctx);
  sections["type"] = dom["cursor-type"].textContent;
  sections.signature = dom["signature"].textContent;
  sections.verification =
    "[" + (verify.className || "") + "]" +
    (verify.textContent ? " " + verify.textContent : "");

  const dlg = state.dialogs.splice(0);
  sections.dialogs = dlg.length
    ? dlg.map((d) => d.kind + "(" + JSON.stringify(d.message) + ")" + (d.kind === "confirm" ? " -> " + (d.answer ? "yes" : "no") : "")).join("\n")
    : "(none)";

  const docViewEl = dom["doc-view"];
  const inDocMode = !!(docViewEl && !docViewEl.hidden);
  const errs = state.errors.splice(0);
  const titles = {
    header: null,
    tabs: "Open units (multi-file)",
    explorer: "File explorer (active: " + active + ")",
    editor: inDocMode
      ? "Document (" + active + ", read-only)"
      : "Editor (" + active + ", cursor " + (cur.line + 1) + ":" + (cur.ch + 1) + ")",
    proof: "Proof pane",
    legend: "Verdict legend",
    status: "Status (roll-up)",
    backend: "Verification backend",
    diagnostics: "Diagnostics",
    type: "Type at cursor",
    signature: "Inferred signature",
    verification: "Verification",
    dialogs: "Dialogs / guards",
  };

  const baseOrder = ["header"];
  if ("tabs" in sections) baseOrder.push("tabs");
  baseOrder.push(
    "explorer",
    "editor",
    "status",
    "backend",
    "proof",
    "legend",
    "diagnostics",
    "type",
    "signature",
    "verification",
    "dialogs"
  );
  const order = only ? [only] : baseOrder;
  const parts = ["================= VOXIDE VIEW ================="];
  for (const key of order) {
    if (!(key in sections)) { parts.push("(no such section: " + key + ")"); continue; }
    if (key === "header") { parts.push(sections.header); continue; }
    parts.push("");
    parts.push("--- " + titles[key] + " ---");
    parts.push(sections[key]);
  }
  if (errs.length) {
    parts.push("");
    parts.push("--- Frontend errors ---");
    parts.push(errs.map((e) => String(e && e.stack ? e.stack.split("\n")[0] : e)).join("\n"));
  }
  parts.push("===============================================");
  return parts.join("\n") + "\n";
}

// ---------------------------------------------------------------------------

async function main() {
  const opts = parseArgs(process.argv.slice(2));
  if (opts.help) { process.stdout.write(HELP + "\n"); return; }
  if (!realFetch) throw new Error("this node has no global fetch (need node >= 18)");
  if (opts.replay) {
    opts.replayData = JSON.parse(fs.readFileSync(opts.replay, "utf8"));
  }

  const html = fs.readFileSync(path.join(opts.frontend, "index.html"), "utf8");
  const dom = buildDomFromIndexHtml(html);
  const cm = makeCm();
  const { sandbox, state } = makeSandbox(opts, dom, cm);
  const tokenizer = loadFrontend(opts, sandbox, cm);

  const ctx = { opts, sandbox, state, cm, dom, tokenizer };

  // Let init() (loadTree -> open default example -> check) settle.
  await settle(state);

  const commands = splitCommands(opts.commands);
  let rendered = false;
  for (const line of commands) {
    if (line.startsWith("render")) rendered = true;
    await runCommand(line, ctx);
  }
  if (!rendered) process.stdout.write(renderView(ctx, opts.section));

  // Non-draining: displaying errors in a render must not clear the exit signal.
  if (state.getErrorTotal()) process.exitCode = 1;
}

main().catch((e) => {
  process.stderr.write("voxide-view: " + (e && e.stack ? e.stack : e) + "\n");
  process.exitCode = 2;
});
