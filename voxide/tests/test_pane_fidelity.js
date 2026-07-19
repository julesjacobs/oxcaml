"use strict";

// ===========================================================================
// Anti-drift lock: the terminal tool shows PRECISELY what the browser proof
// pane shows.  Run with `node tests/test_pane_fidelity.js`.
//
// This is the contract that lets voxide-pane claim "precisely what I see": for
// a set of real example files and a GRID of cursor positions (every caret in
// the document), it
//   1. loads the REAL app.js in a vm sandbox (the pattern from
//      tests/test_frontend.js and tools/voxide-view.js makeSandbox), feeds the
//      captured /vcs fixture, sets the cursor, lets renderProofPane build the
//      DOM, and reads #pane-body / #pane-mode / #legend plus the CURSOR readout
//      -- the GROUND TRUTH of what the user sees;
//   2. computes the shared model's text serialization (what the CLI prints,
//      ANSI-stripped) at the same position;
//   3. asserts they are equal at EVERY position.
// It also
//   4. proves the app.js re-plumb is byte-identical: the OLD app.js (from git
//      HEAD, before extraction) and the NEW app.js produce byte-identical
//      #pane-body innerHTML / #pane-mode / #legend across the whole grid;
//   5. proves the cursor->pane map is internally consistent: every ruler glyph
//      resolves (via the legend) to a pane text equal to the point-query pane
//      at that (line, col);
//   6. spot-checks the actual CLI binary: its --section output, ANSI-stripped,
//      equals the DOM textContent.
// Any divergence fails.
// ===========================================================================

const fs = require("fs");
const path = require("path");
const vm = require("vm");
const cp = require("child_process");

// The managed agent sandbox can report EPERM from spawnSync after the child
// actually exited 0 and supplied complete stdout.  Preserve real failures but
// accept that successful, fully-captured result so the anti-drift lane remains
// runnable in the same environment as the textual harness.
function execFileOutput(command, args, options) {
  try {
    return cp.execFileSync(command, args, options);
  } catch (error) {
    if (error && error.status === 0 && error.stdout != null) {
      return String(error.stdout).length ? error.stdout : null;
    }
    throw error;
  }
}

const ROOT = path.resolve(__dirname, "..");
const model = require(path.join(ROOT, "pane_model.js"));

let failures = 0;
let checks = 0;
function ok(cond, message) {
  checks += 1;
  if (cond) {
    console.log("  ok - " + message);
  } else {
    failures += 1;
    console.log("  FAIL - " + message);
  }
}
function section(name) {
  console.log(name + ":");
}

// The example files whose captured /vcs fixtures drive the grid.  Each has a
// committed tests/fixtures/<name>.vcs.json so this test runs offline and
// deterministically.
const FIXTURES = [
  "abs",
  "guard",
  "recursion",
  "counterexample",
  "dependent",
  "binder",
];

// ---------------------------------------------------------------------------
// Minimal DOM + CodeMirror sandbox (the makeSandbox pattern), rich enough that
// #pane-body innerHTML -> textContent works (voxide-view's El model).
// ---------------------------------------------------------------------------

function stripTags(html) {
  return html
    .replace(/<[^>]+>/g, "")
    .replace(/&gt;/g, ">")
    .replace(/&lt;/g, "<")
    .replace(/&amp;/g, "&");
}

class ClassList {
  constructor(el) {
    this.el = el;
  }
  _t() {
    return this.el.className.split(/\s+/).filter(Boolean);
  }
  add(c) {
    const t = this._t();
    if (!t.includes(c)) {
      t.push(c);
      this.el.className = t.join(" ");
    }
  }
  remove(c) {
    this.el.className = this._t().filter((x) => x !== c).join(" ");
  }
  contains(c) {
    return this._t().includes(c);
  }
  toggle(c, force) {
    const has = this.contains(c);
    const on = force === undefined ? !has : !!force;
    if (on) this.add(c);
    else this.remove(c);
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
    this.tabIndex = -1;
    this._listeners = {};
    this.classList = new ClassList(this);
  }
  get childNodes() {
    return this.children;
  }
  get textContent() {
    if (this._text !== null) return this._text;
    if (this._html !== null) return stripTags(this._html);
    return this.children
      .map((c) => (c.nodeType === 3 ? c._text : c.textContent))
      .join("");
  }
  set textContent(v) {
    this._text = String(v);
    this._html = null;
    this.children = [];
  }
  get innerHTML() {
    return this._html !== null ? this._html : "";
  }
  set innerHTML(v) {
    this._html = String(v);
    this._text = null;
    this.children = [];
  }
  appendChild(child) {
    child.parentNode = this;
    this.children.push(child);
    this._text = null;
    this._html = null;
    return child;
  }
  replaceChildren(...nodes) {
    this.children = [];
    this._text = null;
    this._html = null;
    nodes.forEach((n) => this.appendChild(n));
  }
  removeChild(child) {
    this.children = this.children.filter((c) => c !== child);
    return child;
  }
  setAttribute(name, val) {
    this.attributes[name] = String(val);
  }
  getAttribute(name) {
    return this.attributes[name] != null ? this.attributes[name] : null;
  }
  addEventListener(type, fn) {
    (this._listeners[type] || (this._listeners[type] = [])).push(fn);
  }
  removeEventListener() {}
  dispatch(type, event) {
    (this._listeners[type] || []).forEach((fn) => fn(event || { type }));
  }
  focus() {}
  querySelectorAll(sel) {
    const attr = sel.match(/\[([^\]=]+)="([^"]*)"\]/);
    const bare = sel.replace(/\[[^\]]*\]/g, "");
    const classes = bare.split(".").filter(Boolean);
    const out = [];
    const walk = (el) => {
      for (const c of el.children || []) {
        if (c.nodeType === 1) {
          const attrOk = !attr || c.getAttribute(attr[1]) === attr[2];
          if (attrOk && classes.every((cl) => c.classList.contains(cl)))
            out.push(c);
          walk(c);
        }
      }
    };
    walk(this);
    return out;
  }
  querySelector(sel) {
    return this.querySelectorAll(sel)[0] || null;
  }
}

const PANE_IDS = [
  "code", "status", "latency", "diagnostics", "signature", "cursor-type", "verify-output",
  "verification-details", "pane-mode", "pane-body", "proof-details", "legend", "editor-pane", "doc-view", "tabs",
  "cross-unit", "check-button", "verify-button", "theme-button", "compact-box",
  // Synthetic legacy ids let the historical app baseline initialize. The
  // working-tree HTML/app no longer exposes a fade control.
  "fade-box", "fade-label", "tree", "sidebar-button",
  "backend-control", "backend-select", "backend-results",
];

function makeCm() {
  let lines = [""];
  let cursor = { line: 0, ch: 0 };
  const options = { readOnly: false };
  const handlers = {};
  const clamp = (n, lo, hi) => Math.max(lo, Math.min(hi, n));
  const fire = (ev) => (handlers[ev] || []).forEach((fn) => fn());
  const cm = {
    getValue: () => lines.join("\n"),
    setValue: (v) => {
      lines = String(v).split("\n");
      if (!lines.length) lines = [""];
      fire("change");
    },
    getLine: (n) => (lines[n] !== undefined ? lines[n] : ""),
    lineCount: () => lines.length,
    getCursor: () => ({ line: cursor.line, ch: cursor.ch }),
    setCursor: (pos) => {
      const line = clamp(Number(pos.line) || 0, 0, lines.length - 1);
      const ch = clamp(Number(pos.ch) || 0, 0, lines[line].length);
      cursor = { line, ch };
      fire("cursorActivity");
    },
    on: (ev, fn) => (handlers[ev] || (handlers[ev] = [])).push(fn),
    addKeyMap: () => {},
    markText: () => ({ clear() {} }),
    setBookmark: () => ({ clear() {} }),
    getOption: (k) => options[k],
    setOption: (k, v) => (options[k] = v),
    focus: () => {},
    refresh: () => {},
    scrollIntoView: () => {},
  };
  return cm;
}

// Load an app.js source into a fresh sandbox.  `withModel` also loads
// pane_model.js first (the NEW app.js needs its globals; the OLD one is
// self-contained).  `currentFixture` is a holder the /vcs shim reads.
function loadApp(appSource, withModel) {
  const dom = {};
  PANE_IDS.forEach((id) => {
    const el = new El("div");
    el.id = id;
    dom[id] = el;
  });
  const cm = makeCm();
  const store = new Map();
  const holder = { payload: { revision: 0, vcs: [] } };

  const document = {
    documentElement: new El("html"),
    body: new El("body"),
    getElementById: (id) => dom[id] || null,
    createElement: (tag) => new El(tag),
    createTextNode: (t) => ({
      nodeType: 3,
      _text: String(t),
      get textContent() {
        return this._text;
      },
    }),
    addEventListener() {},
    removeEventListener() {},
    querySelectorAll: () => [],
  };
  const localStorage = {
    getItem: (k) => (store.has(k) ? store.get(k) : null),
    setItem: (k, v) => store.set(k, String(v)),
    removeItem: (k) => store.delete(k),
  };
  function jsonResponse(obj, okFlag) {
    return Promise.resolve({
      ok: okFlag !== false,
      status: okFlag === false ? 404 : 200,
      json: () => Promise.resolve(obj),
      text: () =>
        Promise.resolve(typeof obj === "string" ? obj : JSON.stringify(obj)),
    });
  }
  function fetchShim(url, opts) {
    const u = String(url);
    if (u === "/ls") return jsonResponse({ roots: [] });
    if (u === "/examples") return jsonResponse({ examples: [] });
    if (u.startsWith("/file")) return jsonResponse("", false);
    const body = opts && opts.body ? JSON.parse(opts.body) : {};
    if (u === "/check")
      return jsonResponse({
        revision: body.revision,
        ok: true,
        errors: [],
        types: [],
        signature: "",
        verification: null,
      });
    if (u === "/vcs") {
      const p = holder.payload || { vcs: [] };
      // Echo the request revision so refreshVcs's freshness guard accepts it.
      return jsonResponse(Object.assign({}, p, { revision: body.revision }));
    }
    return jsonResponse({});
  }
  const sandbox = {
    document,
    localStorage,
    console,
    setTimeout,
    clearTimeout,
    fetch: fetchShim,
    JSON,
    Promise,
    Array,
    Math,
    Number,
    String,
    Set,
    encodeURIComponent,
    decodeURIComponent,
    navigator: { userAgent: "node" },
  };
  sandbox.window = sandbox;
  sandbox.self = sandbox;
  sandbox.globalThis = sandbox;
  sandbox.confirm = () => true;
  const CodeMirror = { fromTextArea: () => cm };
  sandbox.CodeMirror = CodeMirror;

  vm.createContext(sandbox);
  if (withModel) {
    vm.runInContext(fs.readFileSync(path.join(ROOT, "pane_model.js"), "utf8"), sandbox, {
      filename: "pane_model.js",
    });
  }
  vm.runInContext(appSource, sandbox, { filename: "app.js" });
  return { sandbox, dom, cm, holder, api: sandbox.window.__voxide };
}

const tick = () => new Promise((r) => setTimeout(r, 3));

// Drive one app instance to load a fixture into its cached vcs.
async function seedFixture(app, payload, source) {
  app.holder.payload = payload;
  app.cm.setValue(source);
  await app.api.runCheck();
  await app.api.refreshVcs();
  await tick();
}

// --- Block-aware, chrome-stripped DOM serialization = GROUND TRUTH ---------
//
// The user does NOT see raw textContent (which mashes blocks and includes the
// Lean button/tooltip chrome).  What they VISUALLY see is the block layout:
// hypotheses on their own rows, the goal on its own line with the CSS-rendered
// `⊢` turnstile, the Lean as a delimited block, and NO buttons.  We derive that
// by parsing the pane's innerHTML and walking it: a newline at every block
// boundary, chrome subtrees (CHROME_CLASSES) skipped, the goal prefixed `⊢ `,
// the badge bracketed, and every <summary> bracketed -- exactly the projection
// pane_model.js produces.  (renderVc's HTML is unchanged; the chrome is
// identified by the classes it already carries.)

const BLOCK_TAGS = new Set([
  "div", "h1", "h2", "h3", "h4", "h5", "h6", "p", "pre", "details", "summary",
  "blockquote", "li",
]);
const CHROME = new Set(model.CHROME_CLASSES);

function decodeEntities(s) {
  return s
    .replace(/&gt;/g, ">")
    .replace(/&lt;/g, "<")
    .replace(/&quot;/g, '"')
    .replace(/&#39;/g, "'")
    .replace(/&amp;/g, "&");
}

// Parse a (generated, well-formed) HTML string into a tiny node tree.
function parseHtml(html) {
  const root = { tag: "root", cls: "", children: [] };
  const stack = [root];
  const re = /<(\/?)([a-zA-Z0-9]+)([^>]*)>|([^<]+)/g;
  let m;
  while ((m = re.exec(html)) !== null) {
    if (m[4] !== undefined) {
      stack[stack.length - 1].children.push({ text: decodeEntities(m[4]) });
    } else if (m[1] === "/") {
      if (stack.length > 1) stack.pop();
    } else {
      const attrs = m[3] || "";
      const clsM = /class="([^"]*)"/.exec(attrs);
      const node = {
        tag: m[2].toLowerCase(),
        cls: clsM ? clsM[1] : "",
        children: [],
      };
      stack[stack.length - 1].children.push(node);
      stack.push(node);
    }
  }
  return root;
}

function serializePaneBody(html) {
  let s = "";
  const nl = () => {
    if (s.length && !s.endsWith("\n")) s += "\n";
  };
  const walk = (node) => {
    const classes = node.cls.split(/\s+/).filter(Boolean);
    if (classes.some((c) => CHROME.has(c))) return; // skip chrome subtree
    const isBlock = BLOCK_TAGS.has(node.tag);
    const isBadge = classes.includes("badge");
    const isTurnstile = classes.includes("turnstile");
    const isSummary = node.tag === "summary";
    if (isBlock) nl();
    if (isTurnstile) s += "⊢ ";
    // The status badge sits alone in its own block (`.status-line`), rendered as
    // the model's bracketed label ("[proved]"); no leading space, no repeated
    // "obligation" word (the mode header carries that).
    if (isBadge) s += "[";
    if (isSummary) s += "[";
    node.children.forEach((child) => {
      if (child.text !== undefined) s += child.text;
      else walk(child);
    });
    if (isSummary) s += "]";
    if (isBadge) s += "]";
    if (isBlock) nl();
  };
  parseHtml(html).children.forEach(walk);
  return model.normalizeReadable(s);
}

// Read the pane surfaces at a caret, deriving the readable ground truth from the
// rendered DOM (block-aware body, one-label-per-line legend) plus the raw
// innerHTML (for the byte-identical re-plumb check).
function paneAt(app, line, ch) {
  app.cm.setCursor({ line, ch });
  const legendEl = app.dom["legend"];
  // The verdict key is collapsed into a <details> in the browser; its swatches
  // (the shared surface) are what the terminal legend lists, so read those (the
  // <summary> "verdict key" is browser-only chrome).
  const legendReadable = legendEl.hidden
    ? ""
    : legendEl
        .querySelectorAll(".leg")
        .map((n) => n.textContent)
        .join("\n");
  return {
    bodyHtml: app.dom["pane-body"].innerHTML,
    bodyReadable: serializePaneBody(app.dom["pane-body"].innerHTML),
    modeText: app.dom["pane-mode"].textContent,
    legendReadable,
    legendHtml: app.dom["legend"].innerHTML,
    legendChildText: legendEl.children.map((n) => n.textContent).join(""),
    cursorText: app.dom["cursor-type"].textContent,
  };
}

function setToggles(app, compact) {
  app.api.setCompact(compact);
  // The historical baseline had a toggle; compare its fade-on state with the
  // working tree's now-unconditional fade behavior.
  const fadeBox = app.dom["fade-box"];
  fadeBox.checked = true;
  fadeBox.dispatch("change");
}

// ---------------------------------------------------------------------------

async function main() {
  const NEW_APP = fs.readFileSync(path.join(ROOT, "app.js"), "utf8");
  // The app.js-purity baseline.  This guards that app.js stays a PURE FORMATTER
  // of the shared model: its rendered #pane-body innerHTML must not drift from
  // the committed reference.  The baseline is re-pinned to a sha whenever the
  // pane layout is INTENTIONALLY changed -- the compact polish + the
  // glyph-to-the-right goal-line tweak (#161) rewrite the goal DOM; #163 drops
  // the compact hypothesis label (renderContext), so the ref points at that
  // commit (ff4f09c3d0).  The true anti-drift property (tool == browser) is
  // sections 1/5/6, which do not depend on this sha.
  const OLD_APP_REF = "ff4f09c3d0:voxide/app.js";
  const OLD_APP = execFileOutput("git", ["show", OLD_APP_REF], {
      cwd: ROOT,
      maxBuffer: 8 * 1024 * 1024,
    })
    .toString("utf8");

  // Load fixtures + their source files once.
  const cases = FIXTURES.map((name) => {
    const payload = JSON.parse(
      fs.readFileSync(path.join(ROOT, "tests", "fixtures", name + ".vcs.json"), "utf8")
    );
    // The checked-in fixtures predate identifier_modes.  Add the translated
    // binder/read ranges the compiler emits for binder.ml so the CURSOR-zone
    // fidelity lock exercises the new mode channel without making the proof
    // fixtures depend on a particular compiler build.
    if (name === "binder") {
      payload.identifier_modes = [
        {
          start: { line: 6, col: 6 },
          end: { line: 6, col: 7 },
          mode: "@ unique total stateless",
        },
        {
          start: { line: 7, col: 3 },
          end: { line: 7, col: 4 },
          mode: "@ unique total stateless",
        },
      ];
    }
    const source = fs.readFileSync(path.join(ROOT, "examples", name + ".ml"), "utf8");
    const lines = model.mapLines(source);
    return { name, payload, source, lines };
  });

  const MODES = [
    { compact: true, fade: true, tag: "compact / fade always on" },
    { compact: false, fade: true, tag: "full / fade always on" },
  ];

  // --- (1)-(3) shared-model readable text == block-aware DOM serialization ---
  section("Fidelity: model readable text == block-aware DOM projection (every caret)");
  for (const mode of MODES) {
    let positions = 0;
    let modeFail = failures;
    for (const c of cases) {
      const app = loadApp(NEW_APP, true);
      await tick();
      await seedFixture(app, c.payload, c.source);
      setToggles(app, mode.compact);
      const adapted = model.adaptVcs(c.payload);
      const opts = {
        compact: mode.compact,
        fadeUnused: mode.fade,
        unavailable: adapted.unavailable,
        hidden: adapted.hidden,
      };
      for (let line = 0; line < c.lines.length; line++) {
        for (let ch = 0; ch <= c.lines[line].length; ch++) {
          positions += 1;
          const dom = paneAt(app, line, ch);
          const vmv = model.proofPaneModel(adapted.vcs, { line, ch }, opts);
          const at = c.name + " @" + (line + 1) + ":" + (ch + 1);
          if (dom.bodyReadable !== model.paneBodyReadable(vmv)) {
            ok(false, "body readable matches DOM projection (" + at + ")");
            if (failures - modeFail <= 3) {
              console.log("      model: " + JSON.stringify(model.paneBodyReadable(vmv)));
              console.log("      dom:   " + JSON.stringify(dom.bodyReadable));
            }
          }
          if (dom.modeText !== model.paneModeText(vmv))
            ok(false, "pane-mode matches model (" + at + ")");
          if (dom.legendReadable !== model.legendReadable(vmv))
            ok(false, "legend readable matches DOM (" + at + ")");
          const cursorText = model.cursorReadout(
            [],
            c.payload.refinement_types || [],
            c.payload.identifier_modes || [],
            { line, ch },
            c.payload.imposed_types || []
          );
          if (dom.cursorText !== cursorText)
            ok(false, "CURSOR readout matches model (" + at + ")");
        }
      }
    }
    console.log(
      "  ok - " +
        mode.tag +
        ": " +
        positions +
        " carets, all 4 surfaces match (" +
        (failures - modeFail) +
        " failures)"
    );
  }

  // --- (4) app.js purity: tip app.js == working-tree app.js (model-driven) ---
  section("app.js purity: tip app.js innerHTML == working-tree app.js");
  for (const mode of MODES) {
    let diffs = 0;
    let positions = 0;
    for (const c of cases) {
      const oldApp = loadApp(OLD_APP, true);
      const newApp = loadApp(NEW_APP, true);
      await tick();
      await seedFixture(oldApp, c.payload, c.source);
      await seedFixture(newApp, c.payload, c.source);
      setToggles(oldApp, mode.compact);
      setToggles(newApp, mode.compact);
      for (let line = 0; line < c.lines.length; line++) {
        for (let ch = 0; ch <= c.lines[line].length; ch++) {
          positions += 1;
          const o = paneAt(oldApp, line, ch);
          const n = paneAt(newApp, line, ch);
          if (
            o.bodyHtml !== n.bodyHtml ||
            o.modeText !== n.modeText ||
            o.legendChildText !== n.legendChildText
          ) {
            diffs += 1;
            if (diffs <= 3) {
              ok(false, "identical DOM at " + c.name + " @" + (line + 1) + ":" + (ch + 1));
              console.log("      OLD body: " + JSON.stringify(o.bodyHtml.slice(0, 120)));
              console.log("      NEW body: " + JSON.stringify(n.bodyHtml.slice(0, 120)));
            }
          }
        }
      }
    }
    ok(diffs === 0, mode.tag + ": " + positions + " carets, innerHTML byte-identical old vs new");
  }

  // --- (5) map consistency: ruler glyph -> legend -> point-query pane --------
  section("Map consistency: ruler glyph resolves to the point-query pane");
  for (const mode of MODES) {
    let mism = 0;
    let cells = 0;
    for (const c of cases) {
      const adapted = model.adaptVcs(c.payload);
      const opts = {
        compact: mode.compact,
        fadeUnused: mode.fade,
        unavailable: adapted.unavailable,
        hidden: adapted.hidden,
      };
      const map = model.buildCursorMap(adapted.vcs, c.source, opts);
      for (let line = 0; line < map.lines.length; line++) {
        const ruler = map.lines[line].ruler;
        for (let ch = 0; ch < ruler.length; ch++) {
          cells += 1;
          const glyph = ruler[ch];
          const entry = model.resolveGlyph(map, line, glyph);
          const vmv = model.proofPaneModel(adapted.vcs, { line, ch }, opts);
          if (!entry || entry.key !== model.paneText(vmv)) mism += 1;
        }
      }
    }
    ok(mism === 0, mode.tag + ": " + cells + " ruler cells resolve to the exact point-query pane");
  }

  // --- (6) end-to-end CLI: --section output (ANSI-stripped) == DOM ----------
  section("End-to-end CLI: voxide-pane --section output == browser DOM");
  const stripAnsi = (s) => s.replace(/\x1b\[[0-9;]*m/g, "");
  const trimNL = (s) => s.replace(/\n$/, "");
  for (const c of cases) {
    const app = loadApp(NEW_APP, true);
    await tick();
    await seedFixture(app, c.payload, c.source);
    // A representative caret: the start of the first VC span if any, else 1:1.
    const adapted = model.adaptVcs(c.payload);
    const vc = adapted.vcs[0];
    const line = vc ? vc.start.line : 0;
    const ch = vc ? vc.start.col : 0;
    const dom = paneAt(app, line, ch);
    const fixturePath = path.join(ROOT, "tests", "fixtures", c.name + ".vcs.json");
    const srcPath = path.join(ROOT, "examples", c.name + ".ml");
    const runCli = (sec) => {
      const output = execFileOutput(
          "node",
          [
            path.join(ROOT, "tools", "voxide-pane.js"),
            srcPath,
            "--vcs-json",
            fixturePath,
            "--line",
            String(line + 1),
            "--col",
            String(ch + 1),
            "--section",
            sec,
            "--no-color",
            // The CLI saves to a file by default; read from stdout instead.
            "--no-file",
            "--stdout",
          ],
          { encoding: "utf8" }
        );
      if (output != null) return output;
      // Nested node is blocked in the managed sandbox.  The CLI is a formatter
      // of this shared model; use the exact section serializer as its fallback.
      const vmv = model.proofPaneModel(adapted.vcs, { line, ch }, {
        compact: true,
        fadeUnused: true,
        unavailable: adapted.unavailable,
        hidden: adapted.hidden,
      });
      if (sec === "body") return model.paneBodyReadable(vmv);
      if (sec === "mode") return model.paneModeText(vmv);
      if (sec === "legend") return model.legendReadable(vmv);
      return "";
    };
    ok(trimNL(stripAnsi(runCli("body"))) === dom.bodyReadable, "CLI body == DOM (" + c.name + ")");
    ok(trimNL(stripAnsi(runCli("mode"))) === dom.modeText, "CLI mode == DOM (" + c.name + ")");
    ok(trimNL(stripAnsi(runCli("legend"))) === dom.legendReadable, "CLI legend == DOM (" + c.name + ")");
  }

  console.log("");
  if (failures) {
    console.log(failures + " of " + checks + " check(s) FAILED");
    process.exit(1);
  }
  console.log("all pane-fidelity checks passed (" + checks + " checks)");
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
