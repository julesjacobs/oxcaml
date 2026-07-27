"use strict";

// Headless test for the browser frontend (app.js), run with `node
// tests/test_frontend.js`.  No jsdom is available, so this shims the minimal
// slices of the DOM, CodeMirror, localStorage, and fetch that app.js touches,
// loads the real app.js in a vm context, drives it, and asserts on the
// resulting shim state.  It covers the round-2 UX work: the rendered
// read-only Markdown doc viewer, compile suppression + result clearing on a
// doc, restoring the editor on switch-back, last-file persistence, and
// keyboard-operable tree navigation.

const fs = require("fs");
const path = require("path");
const vm = require("vm");

const ROOT = path.resolve(__dirname, "..");
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

// --- DOM shim -------------------------------------------------------------

let activeElement = null;

class El {
  constructor(tag) {
    this.tagName = String(tag).toLowerCase();
    this.childNodes = [];
    this.attributes = {};
    this.dataset = {};
    this.listeners = {};
    this._classes = new Set();
    this._text = null;
    this.hidden = false;
    this.tabIndex = -1;
    this.title = "";
    this.type = "";
    this.checked = false;
    this.scrollTop = 0;
    this.href = "";
    this.target = "";
    this.rel = "";
    this.id = "";
  }
  get classList() {
    const set = this._classes;
    return {
      add: (c) => set.add(c),
      remove: (c) => set.delete(c),
      contains: (c) => set.has(c),
      toggle: (c, force) => {
        const want = force === undefined ? !set.has(c) : force;
        if (want) set.add(c);
        else set.delete(c);
        return want;
      },
    };
  }
  get className() {
    return Array.from(this._classes).join(" ");
  }
  set className(value) {
    this._classes = new Set(String(value).split(/\s+/).filter(Boolean));
  }
  get children() {
    return this.childNodes.filter((n) => n instanceof El);
  }
  set textContent(value) {
    this.childNodes = [];
    this._text = String(value);
  }
  get textContent() {
    if (this._text !== null && this.childNodes.length === 0) return this._text;
    return this.childNodes
      .map((n) => (n instanceof El ? n.textContent : n.nodeValue))
      .join("");
  }
  set innerHTML(value) {
    // app.js only assigns innerHTML in the (unexercised-here) proof pane; store
    // the string so the assignment does not throw.
    this.childNodes = [];
    this._html = String(value);
  }
  appendChild(node) {
    this._text = null;
    node.parentNode = this;
    this.childNodes.push(node);
    return node;
  }
  replaceChildren(...nodes) {
    this.childNodes = [];
    this._text = null;
    nodes.forEach((n) => this.appendChild(n));
  }
  setAttribute(name, value) {
    this.attributes[name] = String(value);
  }
  getAttribute(name) {
    return name in this.attributes ? this.attributes[name] : null;
  }
  addEventListener(type, fn) {
    (this.listeners[type] = this.listeners[type] || []).push(fn);
  }
  focus() {
    activeElement = this;
  }
  click() {
    this._fire("click", {});
  }
  _fire(type, event) {
    event = event || {};
    event.currentTarget = this;
    event.target = event.target || this;
    if (typeof event.preventDefault !== "function")
      event.preventDefault = () => {};
    (this.listeners[type] || []).forEach((fn) => fn(event));
  }
  // Is any ancestor a collapsed tree directory (so this node is not visible)?
  get offsetParent() {
    let node = this;
    while (node) {
      if (node.hidden) return null;
      if (
        node._classes.has("tree-children") &&
        node.parentNode &&
        node.parentNode._classes &&
        node.parentNode._classes.has("collapsed")
      ) {
        return null;
      }
      node = node.parentNode;
    }
    return true;
  }
  _descendants() {
    const out = [];
    const walk = (n) =>
      n.childNodes.forEach((c) => {
        if (c instanceof El) {
          out.push(c);
          walk(c);
        }
      });
    walk(this);
    return out;
  }
  querySelectorAll(selector) {
    return this._descendants().filter((el) => matches(el, selector));
  }
  querySelector(selector) {
    return this.querySelectorAll(selector)[0] || null;
  }
}

function matches(el, selector) {
  // Single compound selector: tag, .class(es), and one [attr="val"].
  const attr = selector.match(/\[([^\]=]+)="([^"]*)"\]/);
  if (attr && el.getAttribute(attr[1]) !== attr[2]) return false;
  const bare = selector.replace(/\[[^\]]*\]/g, "");
  const parts = bare.split(".");
  const tag = parts.shift();
  if (tag && el.tagName !== tag.toLowerCase()) return false;
  return parts.every((cls) => !cls || el._classes.has(cls));
}

function textNode(value) {
  return { nodeType: 3, nodeValue: String(value), get textContent() { return this.nodeValue; } };
}

const registry = {};
function register(id) {
  const el = new El("div");
  el.id = id;
  registry[id] = el;
  return el;
}
[
  "code", "status", "latency", "diagnostics", "signature", "cursor-type",
  "verify-output", "verification-details", "pane-mode", "pane-body", "proof-details", "legend", "editor-pane",
  "doc-view", "theme-button", "compact-box", "tree", "sidebar-button", "tabs", "cross-unit",
  "backend-control", "backend-select", "backend-results",
  "share-button", "session-notice", "obligations-details", "obligations-summary",
  "obligations-list", "regression-banner", "regression-details", "regression-report",
].forEach(register);

const documentElement = new El("html");
const body = new El("body");

const documentShim = {
  documentElement,
  body,
  getElementById: (id) => registry[id] || null,
  createElement: (tag) => new El(tag),
  createTextNode: (value) => textNode(value),
  get activeElement() {
    return activeElement;
  },
};

// --- CodeMirror shim ------------------------------------------------------

function makeCm() {
  const listeners = {};
  const options = { readOnly: false };
  let value = "";
  let cursor = { line: 0, ch: 0 };
  const fire = (type) => (listeners[type] || []).forEach((fn) => fn(cm));
  const cm = {
    getValue: () => value,
    setValue: (v) => {
      value = String(v);
      fire("change");
    },
    getCursor: () => cursor,
    setCursor: (pos) => {
      cursor = pos && typeof pos === "object" ? pos : { line: 0, ch: 0 };
      fire("cursorActivity");
    },
    getLine: (n) => value.split("\n")[n] || "",
    lineCount: () => value.split("\n").length,
    on: (type, fn) => (listeners[type] = listeners[type] || []).push(fn),
    addKeyMap: (map) => { cm._keymap = map; },
    setOption: (k, v) => (options[k] = v),
    getOption: (k) => options[k],
    // Records marks so a test can inspect what app.js painted (the provenance
    // hover highlight, the VC underlines, ...); each carries its opts + a
    // cleared flag.  (getWrapperElement is intentionally absent: the editor->pane
    // hover wiring is guarded on it, so this stub does not attach it.)
    _marks: [],
    _bookmarks: [],
    markText: (from, to, o) => {
      const m = { from, to, opts: o || {}, cleared: false, clear() { this.cleared = true; } };
      cm._marks.push(m);
      return m;
    },
    setBookmark: (at, o) => {
      const mark = {
        at,
        opts: o || {},
        cleared: false,
        clear() { this.cleared = true; },
      };
      cm._bookmarks.push(mark);
      return mark;
    },
    focus: () => {},
    refresh: () => {},
    scrollIntoView: () => {},
  };
  return cm;
}
const CodeMirror = { fromTextArea: () => makeCm() };

// --- localStorage / fetch shims ------------------------------------------

const store = new Map();
const localStorage = {
  getItem: (k) => (store.has(k) ? store.get(k) : null),
  setItem: (k, v) => store.set(k, String(v)),
  removeItem: (k) => store.delete(k),
};
let copiedShareLink = "";
const locationShim = {
  _hash: "",
  get hash() { return this._hash; },
  set hash(value) {
    this._hash = String(value).startsWith("#") ? String(value) : "#" + value;
  },
  get href() { return "http://127.0.0.1/" + this._hash; },
};
const historyShim = {
  replaceState: (_state, _title, url) => {
    if (!String(url).includes("#")) locationShim._hash = "";
  },
};

const TREE = {
  roots: [
    {
      name: "Examples",
      id: "examples",
      type: "dir",
      children: [
        { name: "overview.ml", path: "examples/overview.ml", type: "file", kind: "ml", title: "Sixty seconds", expected_state: "verified", default: true },
        { name: "counterexample.ml", path: "examples/counterexample.ml", type: "file", kind: "ml", title: "When you're wrong", expected_state: "disproved", default: false },
        { name: "unproved.ml", path: "examples/unproved.ml", type: "file", kind: "ml", title: "When automation gives up", expected_state: "unproved", default: false },
      ],
    },
    {
      name: "Docs",
      id: "docs",
      type: "dir",
      children: [
        { name: "refinements.md", path: "docs/refinements.md", type: "file", kind: "doc", title: "refinements.md" },
        { name: "welcome.md", path: "docs/welcome.md", type: "file", kind: "doc", title: "welcome.md" },
      ],
    },
  ],
};
const EXAMPLES = {
  examples: [
    { name: "overview", title: "Sixty seconds", expected_state: "verified", default: true, cursor: 11 },
    { name: "counterexample", title: "When you're wrong", expected_state: "disproved", cursor: 10 },
    { name: "unproved", title: "When automation gives up", expected_state: "unproved", cursor: 8 },
    {
      name: "bst",
      title: "Binary search tree (verified behind an interface)",
      filename: "bst/",
      description: "A recursive BST with three parametric laws.",
      expected_state: "verified",
      workspace: {
        active: "client_positive.ml",
        default_backend: "lean",
        files: [
          { name: "bst.mli", path: "examples/bst/bst.mli" },
          { name: "bst.ml", path: "examples/bst/bst.ml" },
          { name: "client_positive.ml", path: "examples/bst/client_positive.ml" },
        ],
        expected_by_backend: {
          lean: { "bst.mli": "interface", "bst.ml": "verified", "client_positive.ml": "verified" },
          z3: { "bst.mli": "interface", "bst.ml": "verified", "client_positive.ml": "verified" },
          oxsmt: { "bst.mli": "interface", "bst.ml": "unproved", "client_positive.ml": "unavailable" },
        },
      },
    },
  ],
};

// A stand-in for the real /workspace-check payload: the slice-6 demo's
// file-tagged VCs (a seal anchored in Demo.mli whose hypothesis originates in
// Demo.ml, an annotation in Demo.ml, and a cross-unit use in Client.ml), so the
// frontend's file-partitioning / tab / cross-unit routing can be driven without
// the real compiler.  Shape mirrors compiler.check_workspace.
function workspacePayload(revision, active, backend) {
  const ver = { status: "verified", message: "ok", obligations: true };
  const outcome = { kind: "ok", message: "", source_located: false };
  const summary = (proved) => ({
    total: proved,
    statuses: {
      proved,
      disproved: 0,
      unproved: 0,
      "solver-error": 0,
      unavailable: 0,
      unknown: 0,
    },
    hidden: 0,
    hidden_statuses: {
      proved: 0,
      disproved: 0,
      unproved: 0,
      "solver-error": 0,
      unavailable: 0,
      unknown: 0,
    },
  });
  const vcs = [
    {
      id: 0,
      file: "Demo.ml",
      kind: "annotation",
      status: "proved",
      span: { start: { line: 0, col: 16 }, end: { line: 0, col: 17 } },
      goal: { display: "1 = 1", raw: "(app[=] 1 1)" },
      hypotheses: [],
    },
    {
      id: 1,
      file: "Demo.mli",
      kind: "seal",
      status: "proved",
      span: { start: { line: 1, col: 20 }, end: { line: 1, col: 25 } },
      goal: { display: "_seal_value > 0", raw: "(app[>] _seal_value 0)" },
      hypotheses: [
        {
          name: "positive",
          display: "_seal_value = 1",
          raw: "(app[=] _seal_value 1)",
          span: {
            file: "Demo.ml",
            start: { line: 0, col: 4 },
            end: { line: 0, col: 12 },
          },
          used: true,
        },
      ],
    },
    {
      id: 2,
      file: "Client.ml",
      kind: "annotation",
      status: "proved",
      span: { start: { line: 0, col: 20 }, end: { line: 0, col: 33 } },
      goal: { display: "nonneg 5 >= -1", raw: "(app[>=] (nonneg 5) -1)" },
      hypotheses: [],
    },
  ];
  return {
    revision,
    active: active || "Demo.ml",
    backend: backend || "oxsmt",
    ok: true,
    outcome,
    files: {
      "Demo.mli": {
        errors: [],
        outcome,
        verification: ver,
        obligation_summary: summary(1),
      },
      "Demo.ml": {
        errors: [],
        outcome,
        verification: ver,
        obligation_summary: summary(1),
        types: [],
        signature: { status: "not-requested", text: "", error: "" },
      },
      "Client.ml": {
        errors: [],
        outcome,
        verification: ver,
        obligation_summary: summary(1),
      },
    },
    vcs,
    refinement_types: [
      {
        file: "Demo.ml",
        start: { line: 0, col: 4 },
        end: { line: 0, col: 5 },
        type: "int",
      },
      {
        file: "Client.ml",
        start: { line: 0, col: 4 },
        end: { line: 0, col: 5 },
        type: "bool",
      },
    ],
    unavailable: false,
    hidden: 0,
    obligation_summary: summary(3),
    workspace_verification: { status: "verified", message: "ok", obligations: true },
  };
}

function bstWorkspacePayload(revision, active, backend) {
  const selected = backend || "lean";
  const partial = selected === "oxsmt";
  const okOutcome = { kind: "ok", message: "", source_located: false };
  const partialOutcome = {
    kind: "verification",
    message: "Refinement verification failed (not-proved)",
    source_located: true,
  };
  const summary = (proved, unproved) => ({
    total: proved + unproved,
    statuses: {
      proved,
      disproved: 0,
      unproved,
      "solver-error": 0,
      unavailable: 0,
      unknown: 0,
    },
    hidden: 0,
    hidden_statuses: {
      proved: 0,
      disproved: 0,
      unproved: 0,
      "solver-error": 0,
      unavailable: 0,
      unknown: 0,
    },
  });
  const provedVc = (id, file, line) => ({
    id,
    file,
    kind: "annotation",
    status: "proved",
    span: { start: { line, col: 2 }, end: { line, col: 8 } },
    goal: { display: "interface fact holds", raw: "" },
    hypotheses: [],
  });
  const vcs = partial
    ? [
        {
          id: 0,
          file: "bst.ml",
          kind: "annotation",
          status: "unproved",
          span: { start: { line: 21, col: 0 }, end: { line: 21, col: 8 } },
          goal: { display: "recursive tree equation", raw: "" },
          hypotheses: [],
          detail: "prove query: sat; disprove query: sat",
        },
      ]
    : [
        provedVc(0, "bst.ml", 21),
        provedVc(1, "client_positive.ml", 2),
        provedVc(2, "client_positive.ml", 6),
        provedVc(3, "client_positive.ml", 14),
      ];
  const presentation = {
    types: [],
    imposed_types: [],
    signature: { status: "not-requested", text: "", error: "" },
  };
  return {
    revision,
    active: active || "client_positive.ml",
    backend: selected,
    backend_options: ["lean", "z3", "oxsmt", "cross", "none"],
    backend_solver_configuration: { z3: true, oxsmt: true },
    ok: !partial,
    outcome: partial ? partialOutcome : okOutcome,
    files: {
      "bst.mli": {
        errors: [],
        outcome: okOutcome,
        verification: { status: "none", message: "No obligations.", obligations: false },
        obligation_summary: summary(0, 0),
        ...presentation,
      },
      "bst.ml": {
        errors: partial
          ? [{ message: partialOutcome.message, kind: "verification" }]
          : [],
        outcome: partial ? partialOutcome : okOutcome,
        verification: partial
          ? { status: "failed", message: partialOutcome.message, obligations: true }
          : { status: "verified", message: "ok", obligations: true },
        obligation_summary: partial ? summary(0, 1) : summary(1, 0),
        ...presentation,
      },
      "client_positive.ml": {
        errors: [],
        outcome: okOutcome,
        verification: partial
          ? { status: "none", message: "Not reached.", obligations: false }
          : { status: "verified", message: "ok", obligations: true },
        obligation_summary: partial ? summary(0, 0) : summary(3, 0),
        ...presentation,
      },
    },
    vcs,
    refinement_types: [],
    identifier_modes: [],
    unavailable: false,
    hidden: 0,
    obligation_summary: partial ? summary(0, 1) : summary(4, 0),
    workspace_verification: partial
      ? { status: "failed", message: "A unit did not verify.", obligations: true }
      : { status: "verified", message: "All units verified.", obligations: true },
  };
}

function bstClientLayerPayload(revision, backend) {
  const payload = bstWorkspacePayload(
    revision,
    "client_positive.ml",
    "lean"
  );
  payload.backend = backend;
  payload.files = {
    "bst.mli": payload.files["bst.mli"],
    "client_positive.ml": payload.files["client_positive.ml"],
  };
  payload.vcs = payload.vcs.filter(
    (vc) => vc.file === "client_positive.ml"
  );
  payload.obligation_summary.total = 3;
  payload.obligation_summary.statuses.proved = 3;
  return payload;
}
let workspacePayloadTransform = null;

let fetchLog = [];
let slowCheckResolvers = [];
let heldCheckResolvers = [];
let holdChecksMatching = null;
let transportFailuresRemaining = 0;
let httpFailure = null;
let backendConfigurationOverride = null;
function jsonResponse(obj) {
  return Promise.resolve({
    ok: true,
    status: 200,
    json: () => Promise.resolve(obj),
    text: () => Promise.resolve(typeof obj === "string" ? obj : JSON.stringify(obj)),
  });
}
function jsonError(status, obj) {
  return Promise.resolve({
    ok: false,
    status,
    statusText: String(status),
    json: () => Promise.resolve(obj),
    text: () => Promise.resolve(JSON.stringify(obj)),
  });
}
function fetchShim(url, opts) {
  const loggedBody = opts && opts.body ? JSON.parse(opts.body) : null;
  fetchLog.push({
    url,
    method: (opts && opts.method) || "GET",
    body: loggedBody,
    hasSignal: !!(
      opts && Object.prototype.hasOwnProperty.call(opts, "signal")
    ),
  });
  const u = String(url);
  if (u === "/ls") return jsonResponse(TREE);
  if (u === "/examples") return jsonResponse(EXAMPLES);
  if (u === "/config")
    return jsonResponse(backendConfigurationOverride || {
      backend_options: ["lean", "z3", "oxsmt", "cross", "none"],
      backend_solver_configuration: { z3: true, oxsmt: true },
      default_backend: "oxsmt",
    });
  if (u.startsWith("/file?path=")) {
    const rel = decodeURIComponent(u.slice("/file?path=".length));
    try {
      const text = fs.readFileSync(path.join(ROOT, rel), "utf8");
      return jsonResponse(text);
    } catch (e) {
      return Promise.resolve({ ok: false, status: 404, json: () => Promise.resolve({}), text: () => Promise.resolve("") });
    }
  }
  const body = opts && opts.body ? JSON.parse(opts.body) : {};
  const revision = body.revision || 0;
  if (
    httpFailure &&
    httpFailure.path === u &&
    httpFailure.remaining > 0 &&
    (!httpFailure.source || httpFailure.source === body.source)
  ) {
    httpFailure.remaining -= 1;
    return jsonError(httpFailure.status, { error: httpFailure.message });
  }
  if (u === "/check") {
    const src = body.source || "";
    if (transportFailuresRemaining > 0) {
      transportFailuresRemaining -= 1;
      return Promise.reject(new Error("connection refused"));
    }
    if (holdChecksMatching && src.indexOf(holdChecksMatching) !== -1) {
      return new Promise((resolve) => heldCheckResolvers.push({ resolve, revision }));
    }
    if (src.indexOf("SLOWROUND") !== -1) {
      return new Promise((resolve) => slowCheckResolvers.push({ resolve, revision }));
    }
    if (body.backend === "none") {
      const typeError = src.indexOf("NONE_TYPE_ERROR") !== -1;
      return jsonResponse({
        revision,
        backend: "none",
        backend_options: ["lean", "z3", "oxsmt", "cross", "none"],
        backend_solver_configuration: { z3: true, oxsmt: true },
        ok: !typeError,
        outcome: {
          kind: typeError ? "type-mode" : "checked-no-verification",
          message: typeError ? "type mismatch" : "",
          source_located: typeError,
        },
        errors: typeError
          ? [{
              kind: "type-mode",
              message: "type mismatch",
              start: { line: 0, col: 0 },
              end: { line: 0, col: 3 },
            }]
          : [],
        types: [
          { start: { line: 0, col: 0 }, end: { line: 0, col: 3 }, type: "int" },
        ],
        signature: { status: "not-requested", text: "", error: "" },
        verification: {
          status: "not-run",
          message: "Typecheck completed; verification was not run.",
          obligations: false,
        },
        vcs: [],
        unavailable: true,
        unavailable_reason: "verification-not-run",
        hidden: 0,
        obligation_summary: {
          total: 0,
          statuses: {
            proved: 0,
            disproved: 0,
            unproved: 0,
            "solver-error": 0,
            unavailable: 0,
            unknown: 0,
          },
          hidden: 0,
          hidden_statuses: {
            proved: 0,
            disproved: 0,
            unproved: 0,
            "solver-error": 0,
            unavailable: 0,
            unknown: 0,
          },
        },
        refinement_types: [],
        identifier_modes: [],
        imposed_types: [],
      });
    }
    if (src.indexOf("REGRESSION_") !== -1) {
      const relocated = src.indexOf("REGRESSION_RELOCATE_") !== -1;
      const broken =
        src.indexOf("REGRESSION_BROKEN") !== -1 ||
        src.indexOf("REGRESSION_RELOCATE_BROKEN") !== -1;
      const status = broken ? "disproved" : "proved";
      const spanLine = relocated ? src.split("\n").length - 2 : 0;
      return jsonResponse({
        revision,
        backend: body.backend || "oxsmt",
        backend_options: ["lean", "z3", "oxsmt", "cross", "none"],
        backend_solver_configuration: { z3: true, oxsmt: true },
        ok: !broken,
        outcome: {
          kind: broken ? "verification" : "ok",
          message: "",
          source_located: false,
        },
        errors: [],
        types: [],
        signature: { status: "not-requested", text: "", error: "" },
        verification: {
          status: broken ? "failed" : "verified",
          message: broken ? "not discharged" : "discharged",
          obligations: true,
        },
        vcs: [{
          id: relocated && broken ? 99 : 0,
          status,
          kind: "annotation",
          span: {
            start: { line: spanLine, col: 4 },
            end: { line: spanLine, col: 5 },
          },
          goal: { display: "x > 0", raw: "x > 0" },
          hypotheses: [],
          counterexample: null,
          detail: broken ? "refuted" : null,
          generated_lean: null,
        }],
        unavailable: false,
        unavailable_reason: null,
        hidden: 0,
        obligation_summary: {
          total: 1,
          statuses: {
            proved: broken ? 0 : 1,
            disproved: broken ? 1 : 0,
            unproved: 0,
            "solver-error": 0,
            unavailable: 0,
            unknown: 0,
          },
          hidden: 0,
          hidden_statuses: {
            proved: 0,
            disproved: 0,
            unproved: 0,
            "solver-error": 0,
            unavailable: 0,
            unknown: 0,
          },
        },
        refinement_types: [],
        identifier_modes: [],
        imposed_types: [],
      });
    }
    if (src.indexOf("UNIFIEDROUND") !== -1) {
      return jsonResponse({
        revision,
        ok: true,
        outcome: { kind: "ok", message: "", source_located: false },
        errors: [],
        types: [
          { start: { line: 0, col: 4 }, end: { line: 0, col: 16 }, type: "int" },
        ],
        signature: { status: "not-requested", text: "", error: "" },
        verification: { status: "verified", message: "ok", obligations: true },
        backend: body.backend || "lean",
        backend_options: ["lean", "z3", "oxsmt", "cross", "none"],
        backend_solver_configuration: { z3: true, oxsmt: true },
        unavailable: false,
        hidden: 0,
        obligation_summary: {
          total: 1,
          statuses: {
            proved: 1,
            disproved: 0,
            unproved: 0,
            "solver-error": 0,
            unavailable: 0,
            unknown: 0,
          },
          hidden: 0,
          hidden_statuses: {
            proved: 0,
            disproved: 0,
            unproved: 0,
            "solver-error": 0,
            unavailable: 0,
            unknown: 0,
          },
        },
        refinement_types: [],
        identifier_modes: [],
        vcs: [
          {
            id: 0,
            status: "proved",
            kind: "annotation",
            span: { start: { line: 0, col: 4 }, end: { line: 0, col: 16 } },
            goal: { display: "unified > 0", raw: "" },
            hypotheses: [],
            counterexample: null,
            detail: null,
            generated_lean: null,
          },
        ],
      });
    }
    if (src.indexOf("IMPOSITION") !== -1) {
      const legacy = src.indexOf("LEGACYIMPOSITION") !== -1;
      return jsonResponse({
        revision,
        ok: true,
        errors: [],
        types: [
          {
            start: { line: 0, col: 10 },
            end: { line: 0, col: 11 },
            type: "int",
          },
          {
            start: { line: 0, col: 17 },
            end: { line: 0, col: 22 },
            type: "int",
          },
          {
            start: { line: 0, col: 0 },
            end: { line: 0, col: 22 },
            type: "int{ _ >= 0 }",
          },
        ],
        signature: "val result : int{ _ >= 0 }",
        verification: { status: "verified", message: "ok", obligations: true },
        unavailable: false,
        hidden: 0,
        refinement_types: [],
        identifier_modes: [],
        imposed_types: legacy
          ? []
          : [
              {
                start: { line: 0, col: 0 },
                end: { line: 0, col: 22 },
                checked_type: "int",
                imposed_type: "int{ _ >= 0 }",
              },
            ],
        vcs: [],
      });
    }
    if (src.indexOf("let seven = positive 7") !== -1) {
      return jsonResponse({
        revision,
        ok: true,
        errors: [],
        types: [],
        signature: "val positive : int{ _ > 0 } -> int\nval seven : int",
        verification: { status: "verified", message: "ok", obligations: true },
        backend: body.backend || "oxsmt",
        backend_options: ["lean", "z3", "oxsmt", "cross", "none"],
        backend_solver_configuration: { z3: true, oxsmt: true },
        unavailable: false,
        hidden: 0,
        refinement_types: [],
        identifier_modes: [],
        vcs: [
          {
            id: 0,
            status: "proved",
            kind: "contract",
            span: { start: { line: 10, col: 21 }, end: { line: 10, col: 22 } },
            goal: { display: "7 > 0", raw: "" },
            hypotheses: [],
            counterexample: null,
            detail: null,
            generated_lean: null,
          },
        ],
      });
    }
    // STATUS fail-closed honesty fixtures: a real type error (ok=false, a
    // type-kind diagnostic) vs a verification failure (ok=false too, but only a
    // verification-kind diagnostic -- NOT a compile error) vs a clean compile.
    if (src.indexOf("STATUSHIDDENVC") !== -1) {
      return jsonResponse({
        revision,
        ok: false,
        outcome: {
          kind: "verification",
          message: "Some obligations were not discharged.",
          source_located: false,
        },
        errors: [],
        types: [],
        signature: { status: "not-requested", text: "", error: "" },
        verification: {
          status: "failed",
          message: "Some obligations were not discharged.",
          obligations: true,
        },
        unavailable: false,
        unavailable_reason: null,
        hidden: 1,
        obligation_summary: {
          total: 1,
          statuses: {
            proved: 0,
            disproved: 0,
            unproved: 1,
            "solver-error": 0,
            unavailable: 0,
            unknown: 0,
          },
          hidden: 1,
          hidden_statuses: {
            proved: 0,
            disproved: 0,
            unproved: 1,
            "solver-error": 0,
            unavailable: 0,
            unknown: 0,
          },
        },
        vcs: [],
      });
    }
    if (src.indexOf("VERIFYNOMARK") !== -1) {
      return jsonResponse({
        revision,
        ok: false,
        outcome: {
          kind: "verification",
          message: "Refinement verification failed (not proved)",
          source_located: true,
        },
        errors: [{
          message: "Refinement verification failed (not proved)",
          kind: "verification",
          start: { line: 0, col: 4 },
          end: { line: 0, col: 8 },
        }],
        types: [],
        signature: { status: "not-requested", text: "", error: "" },
        verification: {
          status: "failed",
          message: "Refinement verification failed (not proved)",
          obligations: true,
        },
        unavailable: false,
        hidden: 1,
        obligation_summary: {
          total: 1,
          statuses: {
            proved: 0,
            disproved: 0,
            unproved: 1,
            "solver-error": 0,
            unavailable: 0,
            unknown: 0,
          },
          hidden: 1,
          hidden_statuses: {
            proved: 0,
            disproved: 0,
            unproved: 1,
            "solver-error": 0,
            unavailable: 0,
            unknown: 0,
          },
        },
        vcs: [],
      });
    }
    if (src.indexOf("STATUSTYPEERR") !== -1) {
      return jsonResponse({
        revision,
        ok: false,
        outcome: {
          kind: "type-mode",
          message: "This expression has type bool but int was expected",
          source_located: true,
        },
        errors: [
          {
            message: "This expression has type bool but int was expected",
            kind: "type-mode",
            start: { line: 0, col: 4 },
            end: { line: 0, col: 8 },
          },
        ],
        types: [],
        signature: "",
        verification: { status: "blocked", message: "", obligations: false },
        unavailable: true,
        unavailable_reason: "type-error",
        hidden: 0,
        vcs: [],
      });
    }
    if (src.indexOf("STATUSSPANLESS") !== -1) {
      return jsonResponse({
        revision,
        ok: false,
        errors: [{ message: "Compiler error without a source range", kind: "type" }],
        types: [],
        signature: "",
        verification: { status: "blocked", message: "", obligations: false },
        unavailable: true,
        hidden: 0,
        vcs: [],
      });
    }
    if (src.indexOf("STATUSFALLBACK") !== -1) {
      return jsonResponse({
        revision,
        ok: false,
        errors: [],
        types: [],
        signature: "",
        verification: {
          status: "failed",
          message: "Verification failed without a located diagnostic",
          obligations: true,
        },
        unavailable: false,
        hidden: 0,
        vcs: [
          {
            id: 0,
            status: "unproved",
            kind: "contract",
            span: { start: { line: 0, col: 4 }, end: { line: 0, col: 8 } },
            goal: { display: "hard goal", raw: "" },
            hypotheses: [],
          },
        ],
      });
    }
    if (src.indexOf("STATUSDISPROVED") !== -1) {
      return jsonResponse({
        revision,
        ok: false,
        errors: [
          {
            message: "Refinement verification failed (disproved)",
            kind: "verification",
            start: { line: 0, col: 4 },
            end: { line: 0, col: 8 },
          },
        ],
        types: [],
        signature: "val demo : int",
        verification: { status: "failed", message: "disproved", obligations: true },
      });
    }
    if (src.indexOf("IDENTIFIERMODES") !== -1) {
      return jsonResponse({
        revision,
        ok: true,
        errors: [],
        types: [
          { start: { line: 0, col: 4 }, end: { line: 0, col: 10 }, type: "int" },
          { start: { line: 1, col: 8 }, end: { line: 1, col: 14 }, type: "int" },
        ],
        signature: "val mode_x : int\nval y : int",
        verification: { status: "none", message: "", obligations: false },
      });
    }
    if (src.indexOf("CROSSBACKENDS") !== -1) {
      return fetchShim("/vcs", opts)
        .then((response) => response.json())
        .then((vcPayload) =>
          jsonResponse({
            revision,
            ok: true,
            errors: [],
            types: [],
            signature: "val cross : int",
            verification: { status: "verified", message: "ok", obligations: true },
            ...vcPayload,
          })
        );
    }
    return jsonResponse({ revision, ok: true, errors: [], types: [], signature: "val positive : int -> int", verification: { status: "verified", message: "ok", obligations: true } });
  }
  if (u === "/verify")
    return jsonResponse({ revision, ok: true, verification: { status: "verified", message: "ok", obligations: true }, errors: [] });
  if (u === "/signature") {
    const src = body.source || "";
    if (src.indexOf("SIGNATUREFAIL") !== -1) {
      return jsonResponse({
        revision,
        backend: body.backend,
        signature: {
          status: "unavailable",
          text: "",
          error: "signature presentation failed",
        },
      });
    }
    const text = src.indexOf("UNIFIEDROUND") !== -1
      ? "val unified : int"
      : src.indexOf("let seven = positive 7") !== -1
        ? "val positive : int{ _ > 0 } -> int\nval seven : int"
        : src.indexOf("IMPOSITION") !== -1
          ? "val result : int{ _ >= 0 }"
          : src.indexOf("IDENTIFIERMODES") !== -1
            ? "val mode_x : int\nval y : int"
      : "val positive : int -> int";
    return jsonResponse({
      revision,
      backend: body.backend,
      signature: { status: "available", text, error: "" },
    });
  }
  if (u === "/vcs") {
    const request = opts && opts.body ? JSON.parse(opts.body) : {};
    const src = request.source || "";
    const backend = request.backend || "lean";
    if (src.indexOf("STATUSTYPEERR") !== -1) {
      return jsonResponse({
        revision,
        unavailable: true,
        unavailable_reason: "type-error",
        hidden: 0,
        vcs: [],
      });
    }
    if (src.indexOf("CROSSBACKENDS") !== -1) {
      const backendResults = backend === "cross"
        ? [
            { backend: "lean", status: "proved", detail: null, fact_usage: true },
            { backend: "z3", status: "disproved", detail: null, fact_usage: false },
            { backend: "oxsmt", status: "unavailable", detail: "missing", fact_usage: false },
          ]
        : null;
      return jsonResponse({
        revision,
        backend,
        backend_options: ["lean", "z3", "oxsmt", "cross", "none"],
        backend_solver_configuration: { z3: true, oxsmt: false },
        unavailable: false,
        hidden: 0,
        vcs: [
          {
            id: 0,
            status: backend === "cross" ? "solver-error" : "proved",
            kind: "contract",
            span: { start: { line: 0, col: 4 }, end: { line: 0, col: 12 } },
            goal: { display: "x > 0", raw: "" },
            hypotheses: [
              {
                name: "x",
                kind: "binder",
                display: "x > 0",
                raw: "",
                span: null,
                ...(backend === "lean" || backend === "cross" ? { used: false } : {}),
              },
            ],
            counterexample: null,
            detail: null,
            generated_lean: null,
            backends: backendResults,
          },
          ...(backend === "cross"
            ? [
                {
                  id: 1,
                  status: "solver-error",
                  kind: "annotation",
                  span: { start: { line: 0, col: 14 }, end: { line: 0, col: 18 } },
                  goal: { display: "hard goal", raw: "" },
                  hypotheses: [],
                  counterexample: null,
                  detail: null,
                  generated_lean: null,
                  backends: [
                    { backend: "lean", status: "solver-error", detail: "timeout", fact_usage: true },
                    { backend: "z3", status: "unknown", detail: null, fact_usage: false },
                    { backend: "oxsmt", status: "unavailable", detail: "missing", fact_usage: false },
                  ],
                },
              ]
            : []),
        ],
      });
    }
    // A crafted dump for the state-at-cursor honesty test: one obligation with
    // a NAMED binder fact and an UNNAMED branch fact, both with spans above the
    // caret used below.  Every other buffer reports no obligations.
    if (src.indexOf("STATEDEMO") !== -1) {
      return jsonResponse({
        revision,
        unavailable: false,
        hidden: 0,
        vcs: [
          {
            id: 0,
            status: "proved",
            kind: "contract",
            span: { start: { line: 0, col: 4 }, end: { line: 0, col: 8 } },
            goal: { display: "goal", raw: "(rich raw goal)" },
            hypotheses: [
              {
                name: "b",
                kind: "binder",
                display: "b > 0",
                raw: "",
                span: { start: { line: 0, col: 15 }, end: { line: 0, col: 16 } },
                used: true,
              },
              {
                name: null,
                kind: "branch",
                display: "GUARDCOND > 0",
                raw: "",
                span: { start: { line: 0, col: 20 }, end: { line: 0, col: 25 } },
                used: true,
              },
            ],
            counterexample: null,
            detail: "rich solver detail",
            generated_lean: "theorem rich_generated_lean : True := by trivial",
          },
        ],
      });
    }
    // Nested obligations for the depth-stacked wash (markVcs vc-goal-dN): an
    // outer span, a DUPLICATE of it (same geometry -> one distinct container, so
    // it does not inflate depth), a singly-nested span, and a doubly-nested span.
    if (src.indexOf("NESTEDVC") !== -1) {
      const vc = (id, c0, c1) => ({
        id,
        status: "proved",
        kind: "contract",
        span: { start: { line: 0, col: c0 }, end: { line: 0, col: c1 } },
        goal: { display: "g" + id, raw: "" },
        hypotheses: [],
        counterexample: null,
        detail: null,
        generated_lean: null,
      });
      return jsonResponse({
        revision,
        unavailable: false,
        hidden: 0,
        vcs: [vc(0, 0, 40), vc(1, 0, 40), vc(2, 2, 30), vc(3, 5, 10)],
      });
    }
    // Two calls whose whole product is a proposition: one the single proved
    // obligation read, one it did not.  The model decides both; this fixture
    // is here to exercise the app-level wiring between them -- payload to
    // mark, and mark to nothing when the result stops being complete.
    if (src.indexOf("LEMMAUNUSED") !== -1) {
      const span = (c0, c1) => ({
        start: { line: 0, col: c0 },
        end: { line: 0, col: c1 },
      });
      const hyp = (name, c0, c1, used) => ({
        name,
        kind: "application",
        display: name + "-fact",
        raw: "",
        span: span(c0, c1),
        used,
        producers: [{ name, kind: "application", span: span(c0, c1) }],
      });
      return jsonResponse({
        revision,
        unavailable: false,
        hidden: 0,
        lemma_calls: [
          { file: null, ...span(18, 28), name: "unread_law", introduced: true },
          { file: null, ...span(29, 39), name: "read_law", introduced: true },
          // Reported, but its proposition never reached the fact
          // environment, so there is nothing to say it went unread.
          { file: null, ...span(40, 50), name: "dropped_law", introduced: false },
        ],
        vcs: [
          {
            id: 0,
            status: src.indexOf("LEMMAUNUSED_OPEN") !== -1 ? "unknown" : "proved",
            kind: "contract",
            span: span(4, 15),
            goal: { display: "g", raw: "" },
            hypotheses: [
              hyp("unread_law", 18, 28, false),
              hyp("read_law", 29, 39, true),
            ],
            counterexample: null,
            detail: null,
            generated_lean: null,
          },
        ],
      });
    }
    // STATUS fail-closed honesty fixtures (paired with the /check cases above).
    if (src.indexOf("STATUSPROVED") !== -1) {
      const vc = (id, c) => ({
        id,
        status: "proved",
        kind: "contract",
        span: { start: { line: 0, col: c }, end: { line: 0, col: c + 1 } },
        goal: { display: "g" + id, raw: "" },
        hypotheses: [],
        counterexample: null,
        detail: null,
        generated_lean: null,
      });
      return jsonResponse({ revision, unavailable: false, hidden: 0, vcs: [vc(0, 4), vc(1, 6)] });
    }
    if (src.indexOf("STATUSDISPROVED") !== -1) {
      return jsonResponse({
        revision,
        unavailable: false,
        hidden: 0,
        vcs: [
          {
            id: 0,
            status: "disproved",
            kind: "contract",
            span: { start: { line: 0, col: 4 }, end: { line: 0, col: 8 } },
            goal: { display: "2 = 1", raw: "" },
            hypotheses: [],
            counterexample: null,
            detail: null,
            generated_lean: null,
          },
        ],
      });
    }
    if (src.indexOf("STATUSTYPEERR") !== -1) {
      // A buffer that did not compile: the dump is unavailable (no obligations).
      return jsonResponse({ revision, unavailable: true, hidden: 0, vcs: [] });
    }
    if (src.indexOf("REFINETYPES") !== -1) {
      // The refinement-predicate subterm types the compiler emits for
      // `int{ (_ > 0) }` (schema v2 `refinement_expression_types`, already
      // translated to 0-based/UTF-16 editor spans by compiler.py): the whole
      // predicate `(_ > 0)` is bool, the hole `_` and literal `0` are int, and
      // the operator `>` is a function type.  No obligations here (the param is
      // returned unrefined), so `vcs` is empty -- proving the cursor types are
      // gathered independently of VC discharge.
      return jsonResponse({
        revision,
        unavailable: false,
        hidden: 0,
        vcs: [],
        refinement_types: [
          { start: { line: 0, col: 16 }, end: { line: 0, col: 23 }, type: "bool" },
          { start: { line: 0, col: 19 }, end: { line: 0, col: 20 }, type: "int -> int -> bool" },
          { start: { line: 0, col: 17 }, end: { line: 0, col: 18 }, type: "int" },
          { start: { line: 0, col: 21 }, end: { line: 0, col: 22 }, type: "int" },
        ],
      });
    }
    if (src.indexOf("IDENTIFIERMODES") !== -1) {
      return jsonResponse({
        revision,
        unavailable: false,
        hidden: 0,
        vcs: [],
        identifier_modes: [
          {
            start: { line: 0, col: 4 },
            end: { line: 0, col: 10 },
            mode: "@ unique total stateless",
          },
          {
            start: { line: 1, col: 8 },
            end: { line: 1, col: 14 },
            mode: "@ unique total stateless",
          },
        ],
      });
    }
    return jsonResponse({ revision, vcs: [] });
  }
  if (u === "/workspace-check") {
    const isBstFull = Array.isArray(body.files) &&
      body.files.some((file) => file && file.name === "bst.ml");
    const isBstClientLayer = Array.isArray(body.files) &&
      !isBstFull &&
      body.files.some(
        (file) => file && file.name === "client_positive.ml"
      );
    const payload = isBstFull
      ? bstWorkspacePayload(revision, body.active, body.backend)
      : isBstClientLayer
      ? bstClientLayerPayload(revision, body.backend)
      : workspacePayload(revision, body.active, body.backend);
    return jsonResponse(
      workspacePayloadTransform ? workspacePayloadTransform(payload) : payload
    );
  }
  if (u === "/workspace-signature") {
    return jsonResponse({
      revision,
      active: body.active,
      backend: body.backend,
      signature: {
        status: body.active && body.active.endsWith(".mli") ? "interface" : "available",
        text: "val positive : int{ _ = 1 }",
        error: "",
      },
    });
  }
  return jsonResponse({});
}

let confirmResult = true;

// --- load app.js in a vm context -----------------------------------------

function loadApp() {
  activeElement = null;
  fetchLog = [];
  slowCheckResolvers = [];
  heldCheckResolvers = [];
  const sandbox = {
    document: documentShim,
    CodeMirror,
    localStorage,
    fetch: fetchShim,
    console,
    setTimeout,
    clearTimeout,
    JSON,
    Promise,
    Array,
    Math,
    encodeURIComponent,
    decodeURIComponent,
    location: locationShim,
    history: historyShim,
    navigator: {
      clipboard: {
        writeText: (value) => {
          copiedShareLink = String(value);
          return Promise.resolve();
        },
      },
    },
  };
  sandbox.window = sandbox;
  sandbox.window.confirm = () => confirmResult;
  // app.js renders from the shared pane_model.js (its adapter + view-model are
  // globals from that script), so load it into the same context first.
  const paneModel = fs.readFileSync(path.join(ROOT, "pane_model.js"), "utf8");
  const code = fs.readFileSync(path.join(ROOT, "app.js"), "utf8");
  vm.createContext(sandbox);
  vm.runInContext(paneModel, sandbox, { filename: "pane_model.js" });
  vm.runInContext(code, sandbox, { filename: "app.js" });
  return sandbox.window.__voxide;
}

const tick = () => new Promise((r) => setTimeout(r, 5));
const wait = (milliseconds) =>
  new Promise((resolve) => setTimeout(resolve, milliseconds));

async function main() {
  // --- Markdown renderer (pure text -> DOM), including injection safety ---
  console.log("Markdown renderer:");
  const api0 = loadApp();
  await tick();
  console.log("Theme labels:");
  ok(
    registry["theme-button"].textContent === "☀ Light" &&
      documentElement.dataset.theme === undefined,
    "dark theme exposes the single-line Light action label"
  );
  registry["theme-button"]._fire("click", {});
  ok(
    registry["theme-button"].textContent === "☾ Dark" &&
      documentElement.dataset.theme === "light" &&
      localStorage.getItem("voxide-theme") === "light",
    "light theme exposes the single-line Dark action label"
  );
  registry["theme-button"]._fire("click", {});
  console.log("Compact preference:");
  ok(
    api0.getCompact() === true &&
      registry["compact-box"].checked === true &&
      body.classList.contains("compact-view"),
    "compact is the default presentation"
  );
  registry["compact-box"].checked = false;
  registry["compact-box"]._fire("change", {});
  ok(
    api0.getCompact() === false &&
      localStorage.getItem("voxide-compact") === "off" &&
      !body.classList.contains("compact-view"),
    "the header control switches to full and saves the preference"
  );
  const fullReload = loadApp();
  await tick();
  ok(
    fullReload.getCompact() === false && registry["compact-box"].checked === false,
    "a new frontend instance restores the persisted full view"
  );
  registry["compact-box"].checked = true;
  registry["compact-box"]._fire("change", {});
  const compactReload = loadApp();
  await tick();
  ok(
    compactReload.getCompact() === true &&
      registry["compact-box"].checked === true &&
      localStorage.getItem("voxide-compact") === "on",
    "switching back persists and restores compact view"
  );
  ok(
    (registry["proof-details"]._html || "") === "",
    "compact renders no pane-local disclosure chrome"
  );
  const md = api0.renderMarkdown(
    "# Title\n\nA **bold** and `code` and [guide](docs/refinements.md) and " +
      "[ext](https://example.com) and [bad](javascript:alert(1)).\n\n" +
      "- one\n- two\n\n```\nlet x = 1\n<script>evil</script>\n```\n"
  );
  ok(md.querySelectorAll("h1").length === 1, "renders an h1 heading");
  ok(md.querySelector("h1").textContent === "Title", "heading text is correct");
  ok(md.querySelectorAll("strong").length === 1, "renders bold as <strong>");
  ok(md.querySelectorAll("code.md-inline-code").length === 1, "renders inline code");
  ok(md.querySelectorAll("ul").length === 1 && md.querySelectorAll("li").length === 2, "renders a two-item list");
  const pre = md.querySelector("pre.md-code");
  ok(!!pre, "renders a fenced code block");
  ok(pre.textContent.indexOf("<script>evil</script>") !== -1, "code block keeps raw text as text (no HTML nodes)");
  ok(pre.querySelectorAll("script").length === 0, "no <script> element is created from code text");
  const links = md.querySelectorAll("a.md-link");
  const internal = links.find((a) => a.textContent === "guide");
  const external = links.find((a) => a.textContent === "ext");
  ok(!!internal && internal.href === "#" && (internal.listeners.click || []).length === 1, "internal doc link is a click handler, not navigation");
  ok(!!external && external.href === "https://example.com" && external.rel === "noopener noreferrer", "external link opens safely");
  ok(md.textContent.indexOf("bad") !== -1 && !links.find((a) => a.textContent === "bad"), "javascript: link is dropped to plain text");

  // Intra-word underscores (snake_case) must NOT become emphasis; a real
  // space-flanked _emphasis_ still does.
  const u = api0.renderMarkdown("use snake_case_here plus _real_ emphasis");
  ok(u.querySelectorAll("em").length === 1, "intra-word underscores are not emphasis");
  ok(u.textContent.indexOf("snake_case_here") !== -1, "snake_case survives verbatim");
  // A link label may itself carry emphasis: render it, not the raw markers.
  const l = api0.renderMarkdown("[**bold label**](https://example.com)");
  const boldLink = l.querySelector("a.md-link");
  ok(!!boldLink && boldLink.querySelectorAll("strong").length === 1 && boldLink.textContent === "bold label", "emphasis inside a link label is rendered");

  // Long inline run: the renderer must iterate, not recurse per token, so a doc
  // with thousands of inline spans does not exhaust the JS stack.
  console.log("Long-doc stack safety:");
  let threw = false;
  let big = null;
  try {
    big = api0.renderMarkdown("start " + "a *b* ".repeat(20000) + "end");
  } catch (e) {
    threw = true;
  }
  ok(!threw, "20000 inline tokens render without a stack overflow");
  ok(big && big.querySelectorAll("em").length === 20000, "every inline token in the long run is rendered");

  // Source-level guard: the DOM shim's innerHTML setter would silently accept a
  // regression to innerHTML, so assert the doc-viewer section (the new
  // Markdown-rendering / result-clearing code) never uses it.
  console.log("Source-level safety:");
  const appSrc = fs.readFileSync(path.join(ROOT, "app.js"), "utf8");
  const docStart = appSrc.indexOf("Read-only documentation viewer");
  const docEnd = appSrc.indexOf("File explorer sidebar + curated examples");
  ok(docStart !== -1 && docEnd !== -1 && docStart < docEnd, "doc-viewer section markers are present");
  // Match a real property access/assignment (`.innerHTML`), not the word in a
  // "never innerHTML" comment.
  ok(!/\.innerHTML/.test(appSrc.slice(docStart, docEnd)), "the doc viewer / clearResults section uses no .innerHTML");
  const indexSrc = fs.readFileSync(path.join(ROOT, "index.html"), "utf8");
  ok(
    /<div id="diagnostics" aria-live="polite">/.test(indexSrc),
    "diagnostics container is not globally collapsed at depth 1"
  );
  const cssSrc = fs.readFileSync(path.join(ROOT, "style.css"), "utf8");
  ok(
    /#cursor-zone\s*\{\s*padding-top:\s*9px;\s*\}/.test(cssSrc),
    "CURSOR readout has added spacing below the preceding separator"
  );
  ok(
    /#output-pane section:last-child\s*\{\s*border-bottom:\s*0;\s*\}/.test(
      cssSrc
    ),
    "the bottommost pane separator is removed"
  );

  // --- Last-file persistence: a saved doc path is restored on load ---
  console.log("Persistence (restore last file):");
  store.clear();
  store.set("voxide-file", "docs/refinements.md");
  const api1 = loadApp();
  await tick();
  await tick();
  ok(api1.getCurrentPath() === "docs/refinements.md", "reopens the last-viewed file on reload");
  ok(api1.isDocOpen() === true, "restored file is shown in the doc viewer");
  // Restoring a doc must not later trigger a spurious unsaved-edits prompt:
  // even with confirm() forced to false, switching to an editable file works.
  confirmResult = false;
  const switched = await api1.openFile({ path: "examples/overview.ml", kind: "ml" }, false);
  await tick();
  confirmResult = true;
  ok(switched === true && api1.getCurrentPath() === "examples/overview.ml", "no spurious discard prompt after restoring a doc");

  // --- Doc mode: rendered, read-only, compile suppressed, results cleared ---
  console.log("Doc mode:");
  store.clear();
  const api = loadApp();
  await tick();
  await tick();
  ok(api.getCurrentPath() === "examples/overview.ml", "default example opens when nothing is remembered");
  ok(api.isDocOpen() === false, "an example is not doc mode");
  ok(
    registry["backend-control"].hidden === false && api.getBackend() === "oxsmt",
    "backend selector is visible and defaults to configured oxsmt before checking"
  );
  const configCall = fetchLog.findIndex((entry) => entry.url === "/config");
  const firstCheckCall = fetchLog.findIndex((entry) => entry.url === "/check");
  ok(
    configCall >= 0 && firstCheckCall > configCall,
    "backend configuration is loaded before the first automatic check"
  );
  ok(
    api.getVcs().length === 1 && api.getVcs()[0].goal.display === "7 > 0",
    "default example receives its proof obligation from the unified /check"
  );
  ok(
    !fetchLog.some((entry) => entry.url === "/vcs"),
    "default example needs no separate /vcs request"
  );
  await api.openFile({ path: "docs/welcome.md", kind: "doc" }, false);
  await tick();
  ok(api.isDocOpen() === true, "opening a .md enters doc mode");
  ok(registry["editor-pane"]._classes.has("doc-mode"), "editor is hidden (doc-mode class set)");
  ok(registry["doc-view"].hidden === false, "doc viewer is shown");
  ok(registry["doc-view"].querySelectorAll("h1").length >= 1, "doc content is rendered, not raw text");
  ok(api.cm.getOption("readOnly") === true, "buffer is read-only in doc mode");
  // Diagnostics prose is cut (spec); the STATUS token carries the pending
  // "checking…" signal, and the stale error list is emptied.
  ok(registry["diagnostics"].textContent === "", "stale diagnostics are cleared");
  ok(body._classes.has("doc-mode"), "doc mode hides check-specific header/output chrome");
  ok(registry["signature"].textContent === "", "stale signature is cleared to quiet whitespace");
  ok(store.get("voxide-file") === "docs/welcome.md", "opening a doc is persisted");
  // Compile must be suppressed while a doc is open.
  fetchLog = [];
  await api.runCheck();
  await api.refreshVcs();
  await tick();
  ok(fetchLog.filter((f) => f.url === "/check" || f.url === "/vcs").length === 0, "no compile happens while a doc is open");

  // --- Switching back to an example restores the editor and checks ---
  console.log("Switch back to editable file:");
  fetchLog = [];
  await api.openFile({ path: "examples/overview.ml", kind: "ml" }, false);
  await tick();
  await tick();
  ok(api.isDocOpen() === false, "switching to an .ml leaves doc mode");
  ok(!registry["editor-pane"]._classes.has("doc-mode"), "editor is restored (doc-mode class cleared)");
  ok(registry["doc-view"].hidden === true, "doc viewer is hidden");
  ok(api.cm.getOption("readOnly") === false, "buffer is editable again");
  ok(fetchLog.some((f) => f.url === "/check"), "the example is checked on open");

  // --- One automatic round feeds every pane and owns the latency label ---
  console.log("Unified automatic check + verify round:");
  store.clear();
  const unified = loadApp();
  await tick();
  await tick();
  unified.cm.setValue("let UNIFIEDROUND = 1");
  fetchLog = [];
  await unified.runCheck();
  await tick();
  const roundCalls = fetchLog.filter(
    (f) => f.url === "/check" || f.url === "/vcs" || f.url === "/verify"
  );
  ok(
    roundCalls.length === 1 && roundCalls[0].url === "/check",
    "one /check request feeds the automatic round (no /verify or /vcs request)"
  );
  ok(
    roundCalls[0].hasSignal === false,
    "AbortController fallback never passes a fake signal to native fetch"
  );
  ok(
    unified.getVcs().length === 1 && registry["diagnostics"].textContent === "",
    "the same response feeds VC rows/squiggles and STATUS diagnostics"
  );
  ok(
    registry["signature"].textContent === "val unified : int" &&
      unified.getTypes().length === 1,
    "the authoritative response feeds CURSOR types while background signature is revision-guarded"
  );
  ok(
    registry["status"].textContent.indexOf("verified") !== -1,
    "the unified round produces the verified header verdict"
  );
  ok(
    /verified.*\(\d+ ms\)$/.test(registry["status"].textContent),
    "completed unified round publishes latency inside the verified message"
  );
  unified.cm.setValue("let UNIFIEDROUND_SIGNATUREFAIL = 1");
  await unified.runCheck();
  await tick();
  ok(
    registry["status"].textContent.indexOf("verified") !== -1 &&
      registry["signature"].textContent.indexOf("Unavailable: signature presentation failed") !== -1,
    "a failed background signature leaves the authoritative verification verdict intact"
  );

  // A slow result for the previous revision must not repaint any surface.
  console.log("Superseded round honesty:");
  unified.cm.setValue("let SLOWROUND = 0");
  const stale = unified.runCheck();
  await tick();
  ok(slowCheckResolvers.length === 1, "the old revision is held in flight");
  unified.cm.setValue("let UNIFIEDROUND = 2");
  ok(
    registry["status"].textContent.indexOf("checking") !== -1 &&
      registry["status"].textContent.indexOf("ms") === -1 &&
      registry["pane-body"].textContent === "" &&
      registry["cursor-type"].textContent === "" &&
      unified.getVcs().length === 0,
    "editing clears stale panes and omits latency while the replacement round is pending"
  );
  slowCheckResolvers.shift().resolve(
    jsonResponse({
      revision: 1,
      ok: false,
      errors: [{ message: "STALE RESULT", kind: "type" }],
      types: [],
      signature: "STALE SIGNATURE",
      verification: { status: "blocked", message: "", obligations: false },
      unavailable: false,
      hidden: 0,
      vcs: [],
    })
  );
  await stale;
  await tick();
  ok(
    registry["diagnostics"].textContent.indexOf("STALE") === -1 &&
      registry["signature"].textContent.indexOf("STALE") === -1,
    "a superseded completion cannot render while the queued latest check runs"
  );
  await unified.runCheck();
  ok(
    unified.getVcs().length === 1 && /\(\d+ ms\)$/.test(registry["status"].textContent),
    "the current replacement round installs all results and its own latency"
  );

  console.log("Typing burst coalescing:");
  const burst = loadApp();
  await tick();
  await tick();
  fetchLog = [];
  holdChecksMatching = "TYPEBURST";
  burst.cm.setValue("let TYPEBURST = 0");
  await wait(70);
  ok(
    heldCheckResolvers.length === 1 &&
      fetchLog.filter((entry) => entry.url === "/check").length === 1,
    "typing starts promptly with only one authoritative compile in flight"
  );
  burst.cm.setValue("let UNIFIEDROUND = 1");
  burst.cm.setValue("let UNIFIEDROUND = 2");
  burst.cm.setValue("let UNIFIEDROUND = 3");
  await wait(70);
  ok(
    fetchLog.filter((entry) => entry.url === "/check").length === 1,
    "rapid edits queue no concurrent or throwaway compiler requests"
  );
  holdChecksMatching = null;
  const typingStale = heldCheckResolvers.shift();
  typingStale.resolve(
    jsonResponse({
      revision: typingStale.revision,
      ok: false,
      errors: [{ message: "STALE TYPING RESULT", kind: "type" }],
      types: [],
      signature: "STALE TYPING SIGNATURE",
      verification: { status: "blocked", message: "", obligations: false },
      unavailable: false,
      hidden: 0,
      vcs: [],
    })
  );
  await wait(30);
  const typingChecks = fetchLog.filter((entry) => entry.url === "/check");
  ok(
    typingChecks.length === 2 &&
      typingChecks[1].body.source === "let UNIFIEDROUND = 3" &&
      burst.getVcs().length === 1 &&
      !registry["diagnostics"].textContent.includes("STALE TYPING"),
    "the burst coalesces to one final authoritative check and drops stale state"
  );

  // The teaching line lands synchronously. A delayed VC may refine its column
  // only while the user has not moved the caret.
  console.log("Teaching cursor non-theft:");
  holdChecksMatching = "let seven = positive 7";
  await unified.openFile({ path: "examples/overview.ml", kind: "ml" }, true);
  ok(
    unified.cm.getCursor().line === 10 && unified.cm.getCursor().ch === 0,
    "the authored teaching line is placed immediately, before the check completes"
  );
  await tick();
  ok(heldCheckResolvers.length === 1, "the teaching check is held for the latency race");
  unified.cm.setCursor({ line: 2, ch: 3 });
  const held = heldCheckResolvers.shift();
  held.resolve(
    jsonResponse({
      revision: held.revision,
      ok: true,
      errors: [],
      types: [],
      signature: "val positive : int{ _ > 0 } -> int\nval seven : int",
      verification: { status: "verified", message: "ok", obligations: true },
      backend: "oxsmt",
      backend_options: ["lean", "z3", "oxsmt", "cross", "none"],
      backend_solver_configuration: { z3: true, oxsmt: true },
      unavailable: false,
      hidden: 0,
      vcs: [
        {
          id: 0,
          status: "proved",
          kind: "contract",
          span: { start: { line: 10, col: 21 }, end: { line: 10, col: 22 } },
          goal: { display: "7 > 0", raw: "" },
          hypotheses: [],
        },
      ],
    })
  );
  await tick();
  ok(
    unified.cm.getCursor().line === 2 && unified.cm.getCursor().ch === 3,
    "a user cursor move cancels delayed VC-column refinement"
  );
  holdChecksMatching = null;

  // Real edits are still protected across a doc detour: edit the buffer, view
  // a doc (no prompt -- docs never touch the buffer), then decline the guard
  // when switching to another editable file.
  console.log("Unsaved-edits guard across a doc detour:");
  api.cm.setValue(api.cm.getValue() + "\nlet extra = 1");
  await tick();
  await api.openFile({ path: "docs/welcome.md", kind: "doc" }, false);
  await tick();
  ok(api.isDocOpen() === true, "viewing a doc with unsaved edits does not prompt");
  confirmResult = false;
  const blocked = await api.openFile({ path: "examples/counterexample.ml", kind: "ml" }, false);
  await tick();
  confirmResult = true;
  ok(blocked === false && api.getCurrentPath() === "docs/welcome.md", "declining the guard keeps the current buffer's edits");

  // --- Keyboard-operable tree ---
  console.log("Keyboard navigation:");
  store.clear();
  const api2 = loadApp();
  await tick();
  await tick();
  const tree = registry["tree"];
  const items = tree.querySelectorAll('[role="treeitem"]');
  ok(items.length >= 3, "tree items expose role=treeitem");
  // Roving tabindex: exactly one item is in the tab order at a time.
  ok(items.filter((el) => el.tabIndex === 0).length === 1, "exactly one item is the roving tab stop");
  ok(items[0].tabIndex === 0, "the first item seeds the tab order");
  const firstFile = tree.querySelectorAll(".tree-file")[0];
  ok(
    firstFile.querySelector(".tree-file-title").textContent === "Sixty seconds" &&
      firstFile.querySelector(".tree-file-name").textContent === "overview.ml",
    "curated title is primary and filename is muted secondary explorer text"
  );
  ok(
    tree.querySelectorAll(".tree-file").every((file) => file.textContent.indexOf("✗") === -1),
    "unopened examples carry no live-looking verdict glyph"
  );
  const unprovedFile = tree
    .querySelectorAll(".tree-file")
    .find((file) => file.dataset.path === "examples/unproved.ml");
  ok(
    unprovedFile && unprovedFile.title.indexOf("deliberately unproved") !== -1,
    "unproved teaching intent remains neutral metadata, distinct from live state"
  );
  firstFile.focus();
  const beforeArrow = tree.querySelectorAll('[role="treeitem"]').filter((el) => el.offsetParent !== null);
  const fromIndex = beforeArrow.indexOf(firstFile);
  firstFile._fire("keydown", { key: "ArrowDown" });
  const nextItem = beforeArrow[fromIndex + 1];
  ok(documentShim.activeElement === nextItem, "ArrowDown moves focus to the next visible item");
  ok(nextItem.tabIndex === 0 && firstFile.tabIndex === -1, "ArrowDown moves the roving tab stop with focus");
  const cxFile = tree.querySelectorAll(".tree-file").find((el) => el.dataset.path === "examples/counterexample.ml");
  cxFile._fire("keydown", { key: "Enter" });
  await tick();
  await tick();
  ok(api2.getCurrentPath() === "examples/counterexample.ml", "Enter on a file opens it");
  const dirLabel = tree.querySelector(".tree-dir-label");
  ok(dirLabel.getAttribute("aria-expanded") === "true", "directory reports aria-expanded");
  dirLabel._fire("keydown", { key: "ArrowLeft" });
  ok(dirLabel.getAttribute("aria-expanded") === "false", "ArrowLeft collapses a directory");

  // --- Manifest-backed curated BST workspace ---
  console.log("Curated BST workspace:");
  store.clear();
  const bst = loadApp();
  await tick();
  await tick();
  const bstRoot = registry["tree"]
    .querySelectorAll(".workspace-example")
    .find((entry) => entry.dataset.workspace === "bst");
  const bstLabel = bstRoot && bstRoot.querySelector(".tree-dir-label");
  const bstMeta = EXAMPLES.examples.find((entry) => entry.name === "bst");
  ok(
    bstLabel &&
      bstLabel.querySelector(".tree-file-title").textContent ===
        "Binary search tree (verified behind an interface)" &&
      bstLabel.querySelector(".tree-file-name").textContent === "bst/",
    "BST explorer row renders the curated title primary and workspace filename secondary"
  );
  const bstImplNode = bstRoot && bstRoot
    .querySelectorAll(".workspace-file")
    .find((entry) => entry.dataset.file === "bst.ml");
  ok(
    bstImplNode &&
      bstImplNode.title.indexOf("expected on lean: verified") !== -1 &&
      bstImplNode.title.indexOf("known backend gap") === -1,
    "BST implementation explorer metadata no longer advertises the retired backend error"
  );
  const absentOrderWorkspace = JSON.parse(JSON.stringify(bstMeta));
  absentOrderWorkspace.workspace.order = ["bst.mli", "absent.ml"];
  ok(
    !bst.isValidCuratedWorkspace(absentOrderWorkspace),
    "a declared workspace order referencing absent.ml is rejected"
  );
  const malformedWorkspaces = [
    (example) => { example.workspace.active = "absent.ml"; },
    (example) => { example.workspace.files[1].name = "bst.mli"; },
    (example) => { example.workspace.files[1].path = "examples/bst/../bst.ml"; },
    (example) => { example.expected_state = "mystery"; },
    (example) => {
      example.workspace.expected_by_backend.frobnicator = {
        ...example.workspace.expected_by_backend.lean,
      };
    },
    (example) => {
      example.workspace.expected_by_backend.lean["absent.ml"] = "verified";
    },
  ].map((mutate) => {
    const example = JSON.parse(JSON.stringify(bstMeta));
    mutate(example);
    return example;
  });
  ok(
    malformedWorkspaces.every(
      (example) => !bst.isValidCuratedWorkspace(example)
    ),
    "malformed curated files, paths, states, backends, and unit maps are rejected"
  );
  await bst.openCuratedWorkspace("bst");
  await tick();
  await tick();
  ok(
    bst.isWorkspace() &&
      bst.getWorkspaceId() === "bst" &&
      bst.getWorkspaceOrder().join(",") ===
        "bst.mli,bst.ml,client_positive.ml",
    "opening the curated row loads all three manifest files as one workspace"
  );
  ok(
    bst.getActiveFile() === "client_positive.ml" &&
      bst.cm.getValue().indexOf("Bst.member_insert_law") !== -1,
    "the positive client opens active with its parametric interface-law source"
  );
  ok(
    bst.getBackend() === "lean" &&
      registry["backend-select"].value === "lean" &&
      registry["status"].textContent.indexOf("verified") !== -1,
    "the BST workspace selects Lean and presents the feature-complete verified state"
  );
  bstMeta.workspace.expected_by_backend.lean["absent.ml"] = "verified";
  await bst.runWorkspaceCheck();
  ok(
    registry["status"].textContent.indexOf("unavailable") !== -1 &&
      registry["status"].textContent.indexOf("verified") === -1 &&
      registry["tabs"]
        .querySelectorAll(".tab")
        .every(
          (tab) =>
            tab.querySelectorAll(".tab-status-unavailable").length === 1
        ),
    "an expected unit absent from both request order and response degrades the workspace"
  );
  delete bstMeta.workspace.expected_by_backend.lean["absent.ml"];
  await bst.runWorkspaceCheck();
  fetchLog = [];
  registry["backend-select"].value = "oxsmt";
  registry["backend-select"]._fire("change", {});
  await tick();
  await tick();
  const bstTabs = registry["tabs"].querySelectorAll(".tab");
  const implTab = bstTabs.find((tab) => tab.dataset.file === "bst.ml");
  const clientTab = bstTabs.find(
    (tab) => tab.dataset.file === "client_positive.ml"
  );
  ok(
    registry["status"].textContent.indexOf("1 unproved") !== -1 &&
      registry["status"].textContent.indexOf("known backend gap") === -1 &&
      registry["status"]._classes.has("status-unproved"),
    "oxsmt reports its honest-partial implementation result as unproved"
  );
  ok(
    implTab.querySelectorAll(".tab-status-unproved").length === 1 &&
      implTab.querySelectorAll(".tab-status-error").length === 0 &&
      implTab.querySelector(".tab-status").title.indexOf("known backend gap") === -1,
    "the implementation tab keeps unproved distinct from a source error or fabricated green"
  );
  ok(
    clientTab.querySelectorAll(".tab-status-unavailable").length === 1 &&
      clientTab.querySelector(".tab-status").title.indexOf(
        "not reached"
      ) !== -1 && bst.getVcs().length === 0,
    "the later client stays honestly not reached after oxsmt's unproved implementation VC"
  );
  const oxsmtChecks = fetchLog.filter(
    (entry) => entry.url === "/workspace-check"
  );
  ok(
    oxsmtChecks.length === 1 &&
      oxsmtChecks[0].body.files.map((file) => file.name).join(",") ===
        "bst.mli,bst.ml,client_positive.ml" &&
      oxsmtChecks[0].body.backend === "oxsmt",
    "oxsmt's honest-partial verdict comes from the full live workspace request"
  );
  delete bstMeta.workspace.expected_by_backend.oxsmt["client_positive.ml"];
  await bst.runWorkspaceCheck();
  const expectationFreeClientTab = registry["tabs"]
    .querySelectorAll(".tab")
    .find((tab) => tab.dataset.file === "client_positive.ml");
  ok(
    expectationFreeClientTab.querySelectorAll(".tab-status-unavailable").length ===
      1 && bst.getVcs().length === 0,
    "removing client expectations does not change its live compiler verdict"
  );
  bstMeta.workspace.expected_by_backend.oxsmt["client_positive.ml"] =
    "unavailable";
  await bst.openCuratedWorkspace("bst", "bst.ml");
  await tick();
  await tick();
  ok(
    bst.getBackend() === "oxsmt" && bst.getActiveFile() === "bst.ml",
    "selecting another unit in the open workspace preserves the user's backend"
  );
  bst.cm.setValue("BST_MUTATION");
  await bst.openFile({ path: "examples/overview.ml", kind: "ml" }, true);
  await tick();
  ok(
    !bst.isWorkspace() &&
      bst.getBackend() === "oxsmt" &&
      bst.cm.getValue().indexOf("BST_MUTATION") === -1,
    "leaving BST restores the configured backend and clears workspace state"
  );
  await bst.loadExample("bst", true);
  await tick();
  await tick();
  ok(
    bst.isWorkspace() &&
      bst.getBackend() === "lean" &&
      bst.cm.getValue().indexOf("BST_MUTATION") === -1,
    "reopening BST reloads curated sources and reapplies its Lean default"
  );

  // --- State-at-cursor honesty: named binder shown, branch fact never leaks ---
  console.log("State-at-cursor (off-obligation) honesty:");
  store.clear();
  const api3 = loadApp();
  await tick();
  await tick();
  // A one-line buffer wide enough for a caret past the fact spans (col 40),
  // off the obligation (cols 4-8).
  api3.cm.setValue("let STATEDEMO = padding_wide_enough_for_a_caret_here_yes");
  await api3.runCheck();
  await api3.refreshVcs();
  await tick();
  ok(api3.getVcs().length === 1, "crafted obligation is loaded");
  api3.cm.setCursor({ line: 0, ch: 40 });
  await tick();
  // Compact (default): off an obligation the PROOF zone shows NOTHING (honest:
  // no facts shown, so nothing to caveat).  The CONTEXT token + facts are full-only.
  const compactHtml = registry["pane-body"]._html || "";
  ok(
    compactHtml.indexOf("◦ CONTEXT") === -1 && compactHtml.indexOf("known") === -1,
    "off-obligation compact view shows nothing in the PROOF zone"
  );
  ok(
    (registry["proof-details"]._html || "") === "",
    "off-obligation compact view has no local disclosure chrome"
  );
  api3.setCompact(false);
  const paneHtml = registry["pane-body"]._html || "";
  ok(paneHtml.indexOf("◦ CONTEXT · approximate") !== -1, "full view exposes the CONTEXT token");
  ok(paneHtml.indexOf("b &gt; 0") !== -1 || paneHtml.indexOf("b > 0") !== -1, "the named binder fact is available in full view");
  // The load-bearing honesty property: the branch condition is never placed by
  // text position, even though its span sits above the caret.
  ok(paneHtml.indexOf("GUARDCOND") === -1, "the unnamed branch fact is excluded from the off-obligation view");
  ok(paneHtml.indexOf("Branch conditions are omitted") !== -1, "the honesty caveat is shown");

  // --- #163 provenance hover: pane row -> editor source span ---
  // Hovering a goal / hypothesis pane row paints its source span in the editor
  // (a transient `.prov-hl` mark) -- the affordance that replaces the compact
  // hypothesis label.  Browser-only interactive chrome: it paints no text and
  // never enters the shared model, so it is tested here (outside the PROOF
  // fidelity lock) by firing synthetic hover events at the delegated handler on
  // #pane-body and inspecting what the editor was asked to mark.
  console.log("Provenance hover (pane row -> editor span):");
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    app.cm.setValue("let STATEDEMO = padding_wide_enough_for_a_caret_here_yes");
    await app.runCheck();
    await app.refreshVcs();
    await tick();
    // Cursor on the obligation (span cols 4-8) so the pane is in obligation mode
    // with paneVc set: its goal span, and two hypotheses each with a source span.
    app.cm.setCursor({ line: 0, ch: 5 });
    await tick();
    const paneBody = registry["pane-body"];
    const compactProof = paneBody._html || "";
    ok(
      compactProof.indexOf("goal") !== -1 &&
        compactProof.indexOf("b &gt; 0") !== -1 &&
        compactProof.indexOf("GUARDCOND &gt; 0") !== -1 &&
        compactProof.indexOf("contract obligation") === -1 &&
        compactProof.indexOf("raw predicate") === -1 &&
        compactProof.indexOf("rich solver detail") === -1 &&
        compactProof.indexOf("rich_generated_lean") === -1 &&
        (registry["proof-details"]._html || "") === "",
      "compact obligation renders only goal and hypotheses with no detail/disclosure leak"
    );
    app.setCompact(false);
    const fullProof = paneBody._html || "";
    ok(
      fullProof.indexOf("contract obligation") !== -1 &&
        fullProof.indexOf("raw predicate") !== -1 &&
        fullProof.indexOf("rich solver detail") !== -1 &&
        fullProof.indexOf("rich_generated_lean") !== -1,
      "full obligation restores kind, raw predicate, solver detail, and generated Lean"
    );
    app.setCompact(true);
    const activeProv = () =>
      app.cm._marks.filter((m) => m.opts.className === "prov-hl" && !m.cleared);
    // Synthetic mouseover events: `.closest` is faked (in a real browser the
    // event target is a live DOM node in the rendered pane innerHTML).
    const hoverHyp = (idx) =>
      paneBody._fire("mouseover", {
        target: {
          closest: (sel) =>
            sel === ".hyp-link"
              ? { classList: { contains: () => false }, dataset: { hyp: String(idx) } }
              : null,
        },
      });
    const hoverGoal = () =>
      paneBody._fire("mouseover", {
        target: {
          closest: (sel) =>
            sel === ".goal"
              ? { classList: { contains: (c) => c === "goal" }, dataset: {} }
              : null,
        },
      });
    const leave = () => paneBody._fire("mouseout", { relatedTarget: null });

    hoverHyp(0);
    let marks = activeProv();
    ok(marks.length === 1, "hovering hyp 0 paints one prov-hl editor mark");
    ok(
      marks.length === 1 &&
        marks[0].from.line === 0 &&
        marks[0].from.ch === 15 &&
        marks[0].to.ch === 16,
      "the mark spans the binder `b`'s source span (0:15-0:16)"
    );
    leave();
    ok(activeProv().length === 0, "mouseout clears the prov-hl mark");

    hoverGoal();
    marks = activeProv();
    ok(
      marks.length === 1 && marks[0].from.ch === 4 && marks[0].to.ch === 8,
      "hovering the goal paints its own obligation span (0:4-0:8)"
    );
    leave();
    ok(activeProv().length === 0, "mouseout after a goal hover clears the mark");
  }

  // --- calls whose whole product is a proposition no obligation read ---
  // The decision itself is pane_model's and is covered exhaustively there.
  // What is covered here is the wiring: a complete result paints exactly the
  // unread call, the read one and the one that introduced nothing stay bare,
  // and an obligation that did not close takes every mark away.
  console.log("Unnecessary lemma calls (editor marks):");
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    const lemmaMarks = () =>
      app.cm._marks
        .filter((m) => m.opts.className === "lemma-unused" && !m.cleared)
        .map((m) => m.from.ch + "-" + m.to.ch)
        .sort();
    app.cm.setValue("let LEMMAUNUSED = aaaaaaaaaa bbbbbbbbbb cccccccccc");
    await app.runCheck();
    await app.refreshVcs();
    await tick();
    ok(
      lemmaMarks().join(",") === "18-28",
      "only the call no obligation read is marked"
    );
    const marked = app.cm._marks.find(
      (m) => m.opts.className === "lemma-unused" && !m.cleared
    );
    ok(
      marked &&
        /^(lean|z3|oxsmt) proved every obligation without this call's facts$/.test(
          marked.opts.title
        ),
      "the mark's hover text names the backend scope the answer holds for"
    );
    app.cm.setValue("let LEMMAUNUSED_OPEN = aaaaaaaaaa bbbbbbbbbb ccc");
    await app.runCheck();
    await app.refreshVcs();
    await tick();
    ok(
      lemmaMarks().length === 0,
      "an obligation that did not close removes every mark"
    );
    app.cm.setValue("let LEMMAUNUSED = aaaaaaaaaa bbbbbbbbbb cccccccccc");
    await app.runCheck();
    await app.refreshVcs();
    await tick();
    ok(lemmaMarks().join(",") === "18-28", "and the mark comes back with the result");
    app.cm.setValue("let plain = 1");
    await app.runCheck();
    await app.refreshVcs();
    await tick();
    ok(
      lemmaMarks().length === 0,
      "a buffer whose response names no such call carries no marks"
    );
  }

  // --- #173 nested-goal wash: opacity deepens by containment depth ---
  // markVcs tags each obligation mark with vc-goal-dN, N = number of DISTINCT
  // enclosing obligation spans (clamped at 3), so a nested goal renders more
  // opaque than its encloser regardless of how CodeMirror splits the marks.
  // Visual-only editor decoration (no pane text/model change), so it is tested
  // via the recorded editor marks.
  console.log("Nested-goal wash depth (markVcs vc-goal-dN):");
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    app.cm.setValue(
      "let NESTEDVC = wide_enough_padding_for_all_of_the_nested_obligation_spans_here_ok"
    );
    await app.runCheck();
    await app.refreshVcs();
    await tick();
    // The vc-goal-dN class on the mark whose span is exactly [fromCh, toCh).
    const depthClassOf = (fromCh, toCh) => {
      const m = app.cm._marks.find(
        (k) =>
          k.opts.className &&
          /\bvc-/.test(k.opts.className) &&
          k.from.ch === fromCh &&
          k.to.ch === toCh
      );
      if (!m) return null;
      return (
        m.opts.className.split(/\s+/).find((c) => /^vc-goal-d\d$/.test(c)) || null
      );
    };
    ok(depthClassOf(0, 40) === "vc-goal-d0", "outer obligation span -> d0 (base wash)");
    ok(
      depthClassOf(2, 30) === "vc-goal-d1",
      "singly-nested obligation -> d1 (deeper; the duplicate outer span does not inflate depth)"
    );
    ok(
      depthClassOf(5, 10) === "vc-goal-d2",
      "doubly-nested obligation -> d2 (deepest here)"
    );

    app.setCompact(false);
    app.cm.setCursor({ line: 0, ch: 1 });
    let overlapHtml = registry["proof-details"]._html || "";
    ok(
      overlapHtml.indexOf('data-overlap-label="1/2"') !== -1 &&
        (registry["pane-body"]._html || "").indexOf("g0") !== -1,
      "overlapping obligations expose an accessible 1/2 control on the initial ordered VC"
    );
    registry["proof-details"]._fire("click", {
      target: {
        closest: (selector) =>
          selector === ".overlap-control" ? { dataset: {} } : null,
      },
    });
    overlapHtml = registry["proof-details"]._html || "";
    ok(
      overlapHtml.indexOf('data-overlap-label="2/2"') !== -1 &&
        (registry["pane-body"]._html || "").indexOf("g1") !== -1,
      "click cycling selects the second exact compiler VC without merging proof states"
    );
  }

  // --- Type-at-cursor inside a refinement predicate ---
  // The CURSOR zone folds the /vcs dump's refinement-predicate subterm types in
  // with the ordinary /check expression types; the smallest containing span
  // wins, so a caret inside `int{ (_ > 0) }` reports the tightest subterm.
  console.log("Type-at-cursor inside a refinement predicate:");
  {
    store.clear();
    const rt = loadApp();
    await tick();
    await tick();
    // Predicate at real columns; sentinel in a trailing comment keys the shim.
    rt.cm.setValue("let f (x : int{ (_ > 0) }) = x (* REFINETYPES *)");
    await rt.runCheck();
    await rt.refreshVcs();
    await tick();
    const typeAt = (ch) => {
      rt.cm.setCursor({ line: 0, ch });
      return registry["cursor-type"].textContent;
    };
    ok(typeAt(17) === "int", "caret on the hole `_` shows int");
    ok(typeAt(21) === "int", "caret on the literal `0` shows int");
    ok(
      typeAt(19) === "int -> int -> bool",
      "caret on the operator `>` shows its function type"
    );
    ok(typeAt(16) === "bool", "caret on the whole `(_ > 0)` predicate shows bool");
    // Honesty: off the predicate (col 29, the returned `x`) there is no emitted
    // refinement type, so nothing is fabricated.
    ok(
      typeAt(29) === "No inferred expression type at the cursor.",
      "off the predicate, no refinement type is invented"
    );
  }

  // --- Mode-at-cursor for identifier binders and reads ---
  console.log("Mode-at-cursor for identifier binders and reads:");
  {
    store.clear();
    const modes = loadApp();
    await tick();
    await tick();
    modes.cm.setValue(
      "let mode_x = 1\nlet y = mode_x (* IDENTIFIERMODES *)"
    );
    await modes.runCheck();
    await modes.refreshVcs();
    await tick();
    modes.cm.setCursor({ line: 0, ch: 5 });
    ok(
      registry["cursor-type"].textContent ===
        "int\n@ unique total stateless",
      "binder cursor shows its type and compiler-emitted mode"
    );
    modes.cm.setCursor({ line: 1, ch: 9 });
    ok(
      registry["cursor-type"].textContent ===
        "int\n@ unique total stateless",
      "identifier read cursor shows its type and compiler-emitted mode"
    );
    modes.cm.setCursor({ line: 1, ch: 5 });
    ok(
      registry["cursor-type"].textContent ===
        "No inferred expression type at the cursor.",
      "off an emitted identifier span, no mode is invented"
    );
  }

  // --- Refinement imposition: dual facts only at the exact selected span ---
  console.log("Checked vs imposed type at cursor:");
  {
    const previousTokenizer = CodeMirror.voxTokenize;
    CodeMirror.voxTokenize = (text) =>
      String(text)
        .split(/(\n)/)
        .filter((piece) => piece !== "")
        .map((piece) => [piece, piece === "\n" ? null : "type-token"]);
    store.clear();
    const imposed = loadApp();
    await tick();
    await tick();
    imposed.cm.setValue("if p then n else n + 1 (* IMPOSITION *)");
    await imposed.runCheck();
    imposed.cm.setCursor({ line: 0, ch: 0 });
    ok(
      registry["cursor-type"].textContent ===
        "checked: int\nimposed: int{ _ >= 0 }",
      "imposition root labels the checked skeleton and imposed refinement"
    );
    ok(
      registry["cursor-type"].querySelectorAll(".cursor-label").length === 2 &&
        registry["cursor-type"].querySelectorAll(".cm-type-token").length === 2,
      "both cursor types use labeled, tokenizer-highlighted rendering"
    );
    imposed.cm.setCursor({ line: 0, ch: 10 });
    ok(
      registry["cursor-type"].textContent === "int",
      "then-branch cursor keeps the existing bare-int readout"
    );
    imposed.cm.setCursor({ line: 0, ch: 20 });
    ok(
      registry["cursor-type"].textContent === "int",
      "else-branch cursor keeps the existing bare-int readout"
    );
    ok(
      registry["signature"].textContent === "val result : int{ _ >= 0 }" &&
        registry["signature"].querySelectorAll(".cm-type-token").length === 1,
      "signature text is byte-preserved and rendered through the tokenizer"
    );

    store.clear();
    const modeLine = loadApp();
    await tick();
    await tick();
    modeLine.cm.setValue(
      "let mode_x = 1\nlet y = mode_x (* IDENTIFIERMODES *)"
    );
    await modeLine.runCheck();
    await modeLine.refreshVcs();
    modeLine.cm.setCursor({ line: 0, ch: 5 });
    ok(
      registry["cursor-type"].textContent ===
        "int\n@ unique total stateless" &&
        registry["cursor-type"].querySelectorAll(".cursor-mode").length === 1,
      "compiler mode is tokenizer-rendered on its own CURSOR line"
    );
    ok(
      registry["signature"].textContent ===
        "val mode_x : int\nval y : int" &&
        registry["signature"].querySelectorAll(".cm-type-token").length === 2,
      "every inferred-signature line is tokenizer-rendered without text drift"
    );

    store.clear();
    const legacy = loadApp();
    await tick();
    await tick();
    legacy.cm.setValue("if p then n else n + 1 (* LEGACYIMPOSITION *)");
    await legacy.runCheck();
    legacy.cm.setCursor({ line: 0, ch: 0 });
    ok(
      registry["cursor-type"].textContent === "int{ _ >= 0 }" &&
        registry["cursor-type"].querySelectorAll(".cursor-label").length === 0,
      "legacy/degraded payload keeps today's readout without invented labels"
    );
    // setValue also schedules the product debounce; let these three instances
    // drain before later tests reuse the shared DOM registry.
    await new Promise((resolve) => setTimeout(resolve, 400));
    if (previousTokenizer === undefined) delete CodeMirror.voxTokenize;
    else CodeMirror.voxTokenize = previousTokenizer;
  }

  // --- Backend selection and cross-mode STATUS rendering ---
  console.log("Backend selection and cross-mode honesty:");
  {
    store.clear();
    const backends = loadApp();
    await tick();
    await tick();
    backends.cm.setValue("let CROSSBACKENDS = 1");
    await backends.runCheck();
    await backends.refreshVcs();
    ok(
      registry["backend-control"].hidden === false &&
        registry["backend-select"].children.length === 5,
      "new compiler metadata shows all verification choices plus typecheck-only"
    );
    const backendChoices = registry["backend-select"].children;
    ok(
      backendChoices.find((option) => option.value === "z3").textContent === "Z3" &&
        backendChoices
          .find((option) => option.value === "oxsmt")
          .textContent.includes("configure oxsmt") &&
        backendChoices
          .find((option) => option.value === "cross")
          .textContent.includes("configure oxsmt"),
      "solver metadata annotates unconfigured choices without hiding them"
    );

    registry["backend-select"].value = "cross";
    registry["backend-select"]._fire("change", {});
    await tick();
    await backends.refreshVcs();
    const comparison = registry["backend-results"].textContent;
    ok(
      comparison.includes("lean: proved") &&
        comparison.includes("z3: disproved") &&
        comparison.includes("oxsmt: unavailable"),
      "cross mode lists each backend name and distinct verdict"
    );
    ok(
      comparison.includes("DIVERGENCE") &&
        registry["backend-results"].querySelectorAll(".backend-divergence").length === 1,
      "mixed proved/disproved is a prominent divergence, not an average"
    );
    ok(
      registry["backend-results"].querySelectorAll(".backend-solver-error").length === 1 &&
        registry["backend-results"].querySelectorAll(".backend-unknown").length === 1 &&
        registry["backend-results"].querySelectorAll(".backend-unavailable").length === 2,
      "solver error, unknown, and unavailable keep distinct STATUS classes"
    );
    backends.cm.setCursor({ line: 0, ch: 5 });
    ok(
      (registry["pane-body"]._html || "").indexOf("hyp-unused") !== -1,
      "compiler-reported unused hypotheses are always faded"
    );

    registry["backend-select"].value = "oxsmt";
    registry["backend-select"]._fire("change", {});
    await tick();
    await backends.refreshVcs();
    ok(
      backends.getVcs()[0].hypotheses[0].used === null,
      "missing oxsmt fact usage stays absent instead of inheriting Lean"
    );
    ok(
      (registry["pane-body"]._html || "").indexOf("hyp-unused") === -1,
      "missing fact usage is not invented as unused"
    );
  }
  {
    store.clear();
    const legacy = loadApp();
    await tick();
    await tick();
    legacy.cm.setValue("let legacy = 1");
    await legacy.runCheck();
    ok(
      registry["status"].textContent.indexOf("obligations unavailable") !== -1,
      "legacy response without a VC sidecar never claims no obligations"
    );
    const legacyPane =
      registry["pane-body"]._html || registry["pane-body"].textContent || "";
    ok(
      legacyPane.indexOf("Obligation data unavailable.") !== -1 &&
        legacyPane.indexOf("did not compile") === -1,
      "legacy compile without a VC sidecar gets an honest PROOF placeholder"
    );
    await legacy.refreshVcs();
    ok(
      registry["backend-control"].hidden === false &&
        legacy.getBackendOptions().includes("oxsmt"),
      "missing round metadata preserves the backend established by /config"
    );
  }

  console.log("Typecheck-only honesty, obligation navigation, and edit diff:");
  {
    store.clear();
    locationShim._hash = "";
    const wave = loadApp();
    await tick();
    await tick();
    wave.cm.setValue("let type_only_demo = 1");
    registry["backend-select"].value = "none";
    registry["backend-select"]._fire("change", {});
    await wave.runCheck();
    ok(
      /^✓ checked \(no verification\) \(\d+ ms\)$/.test(
        registry["status"].textContent
      ),
      "typecheck-only has a distinct honest status with inline latency"
    );
    ok(
      (registry["pane-body"]._html || "").includes(
        "Verification was not run (typecheck only)."
      ) &&
        registry["obligations-summary"].textContent === "Verification not run" &&
        localStorage.getItem("voxide-backend") === "none",
      "PROOF/list say verification was not run and the selection persists"
    );
    wave.cm.setValue("NONE_TYPE_ERROR");
    await wave.runCheck();
    ok(
      registry["status"].textContent.includes("type/mode error") &&
        !registry["status"].textContent.includes("checked (no verification)"),
      "type errors still dominate a typecheck-only round"
    );

    registry["backend-select"].value = "oxsmt";
    registry["backend-select"]._fire("change", {});
    wave.cm.setValue("let x = 1 (* REGRESSION_GREEN *)");
    await wave.runCheck();
    ok(
      registry["obligations-list"].children.length === 1 &&
        registry["obligations-list"].textContent.includes("x > 0") &&
        registry["obligations-list"].textContent.includes("L1"),
      "all-obligations list shows glyph metadata, source-like goal, and line"
    );
    wave.cm.setValue("let x = 1 (* REGRESSION_BROKEN *)");
    await wave.runCheck();
    ok(
      registry["regression-banner"].textContent === "1 obligation regressed" &&
        registry["regression-details"].hidden === false &&
        registry["regression-report"].textContent.includes("broken: x > 0"),
      "proved-to-disproved transition is called out and listed"
    );
    ok(
      wave.cm._marks.some(
        (mark) =>
          !mark.cleared &&
          String(mark.opts.className || "").includes("vc-regressed")
      ),
      "newly broken obligation gets a distinguishable editor marker"
    );
    registry["obligations-list"].children[0]._fire("click", {});
    ok(
      wave.cm.getCursor().line === 0 && wave.cm.getCursor().ch === 4,
      "clicking an obligation jumps to and pins its source span"
    );
    wave.cm.setCursor({ line: 0, ch: 0 });
    wave.cm._keymap.F8();
    ok(
      wave.cm.getCursor().ch === 4,
      "F8 navigates to the next failing obligation"
    );
    wave.cm.setValue("let y = 1 (* REGRESSION_BROKEN *)");
    await wave.runCheck();
    ok(
      registry["regression-banner"].hidden === true &&
        registry["regression-report"].textContent.includes(
          "not be matched confidently; no regression was claimed"
        ),
      "uncertain span/identity drift is reported without a false regression"
    );
  }
  {
    store.clear();
    locationShim._hash = "";
    const relocated = loadApp();
    await tick();
    await tick();
    relocated.cm.setValue(
      "let old_context = 0\n" +
      "let x = 1 (* REGRESSION_RELOCATE_GREEN *)\n" +
      "let retained_tail = 0"
    );
    await relocated.runCheck();
    const padding = Array.from(
      { length: 199 },
      (_value, index) => "let padding_" + index + " = 0"
    ).join("\n");
    relocated.cm.setValue(
      "let replacement_context = 0\n" +
      padding +
      "\nlet x = 1 (* REGRESSION_RELOCATE_BROKEN *)\n" +
      "let retained_tail = 0"
    );
    await relocated.runCheck();
    ok(
      registry["regression-banner"].hidden === true &&
        registry["regression-report"].textContent.includes(
          "not be matched confidently; no regression was claimed"
        ) &&
        !relocated.cm._marks.some(
          (mark) =>
            !mark.cleared &&
            String(mark.opts.className || "").includes("vc-regressed")
        ),
      "deleting and recreating an identical-looking VC at line 201 is uncertain, never regressed"
    );
  }

  console.log("Scratch persistence and fragment sharing:");
  {
    store.clear();
    locationShim._hash = "";
    copiedShareLink = "";
    const scratch = loadApp();
    await tick();
    await tick();
    scratch.cm.setValue("let hand_edited_scratch = 42");
    registry["backend-select"].value = "none";
    registry["backend-select"]._fire("change", {});
    const saved = JSON.parse(localStorage.getItem("voxide-session-v1"));
    ok(
      saved.source === "let hand_edited_scratch = 42" && saved.backend === "none",
      "hand edits and backend are persisted locally"
    );
    const restored = loadApp();
    await tick();
    await tick();
    ok(
      restored.cm.getValue() === "let hand_edited_scratch = 42" &&
        restored.getBackend() === "none",
      "a reload restores the exact scratch buffer and backend"
    );
    await restored.shareCurrentSession();
    ok(
      locationShim.hash.startsWith("#voxide=") &&
        copiedShareLink.includes("#voxide=") &&
        registry["session-notice"].textContent.includes("contains this buffer set"),
      "share action emits and copies a self-contained fragment URL"
    );
    localStorage.removeItem("voxide-session-v1");
    const shared = loadApp();
    await tick();
    await tick();
    ok(
      shared.cm.getValue() === "let hand_edited_scratch = 42" &&
        shared.getBackend() === "none" &&
        locationShim.hash === "",
      "fragment reopen restores exact content and consumes the one-time fragment"
    );
    shared.cm.setValue("let hand_edited_scratch = 43");
    const editedReload = loadApp();
    await tick();
    await tick();
    ok(
      editedReload.cm.getValue() === "let hand_edited_scratch = 43",
      "an edit after share restore survives reload instead of being overwritten"
    );
    const workspaceState = {
      version: 1,
      backend: "lean",
      mode: "workspace",
      order: ["A.ml", "B.ml"],
      active: "B.ml",
      buffers: { "A.ml": "let a = 1", "B.ml": "let b = A.a" },
    };
    ok(
      shared.restoreSessionState(workspaceState) &&
        JSON.stringify(shared.captureSessionState()) ===
          JSON.stringify(workspaceState),
      "session state preserves an exact multi-buffer set and active unit"
    );
    editedReload.cm.setValue("x".repeat(1000001));
    const hashBeforeOversize = locationShim.hash;
    const copiedBeforeOversize = copiedShareLink;
    const oversizedShared = await editedReload.shareCurrentSession();
    ok(
      oversizedShared === false &&
        locationShim.hash === hashBeforeOversize &&
        copiedShareLink === copiedBeforeOversize &&
        registry["session-notice"].textContent.includes("too large to share") &&
        !registry["session-notice"].textContent.includes("copied"),
      "an oversized session fails honestly before changing or copying a share link"
    );
    locationShim._hash = "";
  }
  {
    store.clear();
    backendConfigurationOverride = {
      backend_options: ["lean", "z3", "none"],
      backend_solver_configuration: { z3: false, oxsmt: false },
      default_backend: "lean",
    };
    const unavailableBackendState = {
      version: 1,
      backend: "z3",
      mode: "single",
      source: "let shared_z3 = 1",
    };
    locationShim.hash =
      "voxide=" + encodeURIComponent(JSON.stringify(unavailableBackendState));
    const fallback = loadApp();
    await tick();
    await tick();
    ok(
      fallback.getBackend() === "lean" &&
        registry["session-notice"].textContent.includes(
          "requested backend z3"
        ) &&
        registry["session-notice"].textContent.includes("using lean") &&
        registry["session-notice"].textContent.includes("not configured"),
      "share restore discloses requested backend, active fallback, and configuration reason"
    );
    backendConfigurationOverride = null;
    locationShim._hash = "";
  }

  // --- STATUS zone fail-closed honesty ---
  // The STATUS roll-up is browser-rendered from /check (obligation counts +
  // compile outcome), so it lives OUTSIDE the PROOF fidelity lock and needs its
  // own coverage.  This is the honesty the old S1-clarity fix protected; the
  // 3-zone reorg must not regress it.
  console.log("STATUS roll-up fail-closed honesty:");
  {
    // (a) '✓ verified · N/N' ONLY when every obligation proved AND it compiles.
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    app.cm.setValue("let STATUSPROVED_demo = 1");
    await app.runCheck();
    await app.refreshVcs();
    await tick();
    const verdict = registry["status"].textContent;
    ok(
      verdict.indexOf("verified") !== -1 && verdict.indexOf("2/2") !== -1,
      "(a) all proved + compiles -> 'verified · 2/2'"
    );
  }
  {
    // (b) a compile error -> STATUS 'type error'; the PROOF pane reads
    // obligations 'unavailable', NEVER verified/proved.
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    app.cm.setValue("let STATUSTYPEERR_demo = (true : int)");
    await app.runCheck();
    await app.refreshVcs();
    app.cm.setCursor({ line: 0, ch: 6 });
    await tick();
    const verdict = registry["status"].textContent;
    ok(verdict.indexOf("type/mode error") !== -1, "(b) compile error -> STATUS 'type/mode error'");
    ok(
      verdict.indexOf("verified") === -1 && verdict.indexOf("proved") === -1,
      "(b) compile-error STATUS never reads verified/proved"
    );
    const pane = registry["pane-body"]._html || registry["pane-body"].textContent || "";
    ok(
      pane.indexOf("Obligation data unavailable: fix the source error.") !== -1 &&
        pane.indexOf("did not compile") === -1,
      "(b) PROOF pane reports unavailable data without guessing why"
    );
    const diagnostic = registry["diagnostics"].children[0];
    ok(
      diagnostic &&
        !diagnostic.classList.contains("depth-1"),
      "(b) unconditional concise view keeps the type-error diagnostic at status depth"
    );
  }
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    app.cm.setValue("let STATUSHIDDENVC_demo = 1");
    await app.runCheck();
    const verdict = registry["status"].textContent;
    ok(
      verdict.indexOf("1 unproved") !== -1 &&
        verdict.indexOf("verified") === -1 &&
        verdict.indexOf("no obligations") === -1,
      "a span-less hidden unproved VC is counted and prevents green STATUS"
    );
  }
  {
    // (c) a disproved obligation -> '✗ N disproved', NEVER green/verified.
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    app.cm.setValue("let STATUSDISPROVED_demo = need_one 2");
    await app.runCheck();
    await app.refreshVcs();
    await tick();
    const verdict = registry["status"].textContent;
    ok(verdict.indexOf("1 disproved") !== -1, "(c) disproved -> STATUS '1 disproved'");
    ok(verdict.indexOf("verified") === -1, "(c) disproved STATUS never reads verified");
    ok(/\(\d+ ms\)$/.test(verdict), "(c) completed failed verdict owns its latency");
    ok(
      registry["verification-details"].hidden === true &&
        (registry["diagnostics"].textContent.match(/Refinement verification failed/g) || []).length === 1,
      "(c) the located compiler diagnostic appears once with no duplicate verification sentence"
    );
  }
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    app.cm.setValue("let STATUSSPANLESS_demo = broken");
    await app.runCheck();
    const diagnostic = registry["diagnostics"].children[0];
    ok(
      diagnostic &&
        diagnostic.tagName === "div" &&
        diagnostic.classList.contains("diagnostic-static") &&
        !(diagnostic.listeners.click || []).length,
      "a spanless diagnostic is a static block with no false button affordance"
    );
  }
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    app.cm.setValue("let VERIFYNOMARK_demo = hard");
    await app.runCheck();
    const fallback = app.cm._marks.find(
      (mark) =>
        !mark.cleared &&
        mark.opts.className === "diagnostic-verify-fallback"
    );
    ok(
      !!fallback,
      "a ranged verification diagnostic without a placeable VC gets one amber editor fallback"
    );
  }
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    app.cm.setValue("let STATUSTYPEERR_demo = (true : int)");
    await app.runCheck();
    app.cm.setCursor({ line: 0, ch: 6 });
    const inline = app.cm._bookmarks.find(
      (mark) =>
        !mark.cleared &&
        mark.opts.widget &&
        mark.opts.widget.className === "caret-inline-diagnostic"
    );
    ok(
      inline &&
        inline.opts.widget.textContent ===
          "This expression has type bool but int was expected",
      "the caret-local inline widget shows exactly one current compiler diagnostic"
    );
    app.cm.setCursor({ line: 0, ch: 30 });
    ok(inline.cleared, "the caret-local diagnostic disappears when the caret leaves");
  }
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    app.cm.setValue("let STATUSFALLBACK_demo = hard");
    await app.runCheck();
    ok(
      registry["diagnostics"].children.length === 0 &&
        registry["verification-details"].hidden === false &&
        registry["verify-output"].textContent ===
          "Verification failed without a located diagnostic",
      "a single local fallback preserves the compiler message when no diagnostic exists"
    );
  }

  // --- Automatic recovery from transport-only failures ------------------
  console.log("Transient server retry + cancellation:");
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    fetchLog = [];
    transportFailuresRemaining = 1;
    await app.runCheck();
    ok(
      registry["status"].textContent === "server unavailable · retrying…" &&
        registry["status"].textContent.indexOf("ms") === -1 &&
        registry["pane-body"].textContent === "",
      "a transport failure clears results and exposes one honest retrying state"
    );
    await new Promise((resolve) => setTimeout(resolve, 320));
    const recoveredAutomatically =
      fetchLog.filter((entry) => entry.url === "/check").length >= 2 &&
      app.getRetryState().attempt === 0 &&
      app.getRetryState().scheduled === false;
    // Repaint from this app in case an older sandbox's pending timer touched
    // the shared synthetic DOM; the retry evidence above is per-app state/log.
    await app.runCheck();
    ok(
      recoveredAutomatically &&
        /\(\d+ ms\)$/.test(registry["status"].textContent),
      "the unchanged revision recovers automatically and publishes only completed latency"
    );
  }
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    const source = app.cm.getValue();
    fetchLog = [];
    httpFailure = {
      path: "/check",
      source,
      status: 400,
      message: "request too large",
      remaining: 4,
    };
    await app.runCheck();
    await new Promise((resolve) => setTimeout(resolve, 320));
    const calls = fetchLog.filter(
      (entry) =>
        entry.url === "/check" && entry.body && entry.body.source === source
    );
    ok(
      calls.length === 1 &&
        httpFailure.remaining === 3 &&
        registry["status"].textContent ===
          "⚠ unavailable · request rejected" &&
        app.getRetryState().attempt === 0 &&
        app.getRetryState().scheduled === false,
      "deterministic HTTP 400 terminates once without scheduling a retry"
    );
    httpFailure = null;
  }

  async function retryCancellationCase(label, action, sameOldCall) {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    const oldSource = app.cm.getValue();
    const oldBackend = app.getBackend();
    fetchLog = [];
    transportFailuresRemaining = 1;
    await app.runCheck();
    await action(app);
    await new Promise((resolve) => setTimeout(resolve, 360));
    const calls = fetchLog.filter(
      (entry) =>
        entry.url === "/check" &&
        entry.body &&
        entry.body.source === oldSource &&
        (sameOldCall !== "backend" || entry.body.backend === oldBackend)
    );
    ok(calls.length === 1, label + " cancels the retry for the old round immediately");
  }

  await retryCancellationCase("edit", async (app) => {
    app.cm.setValue("let edited_after_transport_failure = 1");
  });
  await retryCancellationCase("file switch", async (app) => {
    await app.openFile({ path: "examples/counterexample.ml", kind: "ml" }, true);
  });
  await retryCancellationCase("doc mode", async (app) => {
    await app.openFile({ path: "docs/welcome.md", kind: "doc" }, true);
  });
  await retryCancellationCase(
    "backend change",
    async () => {
      registry["backend-select"].value = "lean";
      registry["backend-select"]._fire("change", {});
    },
    "backend"
  );

  // --- STATUS never flashes a false verdict before the first completed check -
  // Bug #160: on page refresh /vcs can return before the first /check; rendering
  // the header verdict then off the initial lastCompiles=false flashed a false
  // "type error".  The header must stay pending until a completed check lands.
  console.log("STATUS pre-first-check honesty (no false type-error flash):");
  {
    store.clear();
    const app = loadApp();
    await tick();
    await tick();
    // Open a fresh editable file: clearResults resets to the pre-check state and
    // sets the header pending; its scheduled check (a setTimeout macrotask) has
    // NOT fired yet.
    await app.openFile({ path: "examples/counterexample.ml", kind: "ml" }, true);
    // Drive /vcs ALONE, exactly as a refresh does before the first /check
    // returns (awaiting only microtasks, so the scheduled check stays pending).
    await app.refreshVcs();
    const during = registry["status"].textContent;
    ok(
      during.indexOf("type error") === -1,
      "(#160) no false 'type error' before the first completed check"
    );
    ok(
      during.indexOf("checking") !== -1,
      "(#160) header stays pending 'checking…' before the first check"
    );
    // Once a check completes, the pending state clears to an honest verdict.
    await app.runCheck();
    await tick();
    ok(
      registry["status"].textContent.indexOf("checking") === -1,
      "(#160) the pending state clears once a check completes"
    );
  }

  // --- Multi-file workspace mode (slice 6) ---
  console.log("Workspace mode (multi-file):");
  store.clear();
  const w = loadApp();
  await tick();
  await tick();
  await w.openWorkspace("Demo.ml");
  await tick();
  await tick();
  ok(w.isWorkspace() === true, "openWorkspace enters workspace mode");
  ok(w.getActiveFile() === "Demo.ml", "the requested unit is active");
  ok(registry["tabs"].hidden === false, "the tab strip is shown");
  const tabs = registry["tabs"].querySelectorAll(".tab");
  ok(tabs.length === 3, "one tab per unit (Demo.mli, Demo.ml, Client.ml)");
  ok(
    tabs.filter((t) => t._classes.has("tab-active")).length === 1 &&
      tabs.find((t) => t.dataset.file === "Demo.ml")._classes.has("tab-active"),
    "exactly the active unit's tab is highlighted"
  );
  ok(
    tabs.every((t) => t.querySelectorAll(".tab-status-verified").length === 1),
    "each tab carries a glyph-first per-unit verified state"
  );
  ok(
    registry["status"].textContent.indexOf("verified") !== -1 &&
      registry["status"].textContent.indexOf("workspace") !== -1 &&
      /\(\d+ ms\)$/.test(registry["status"].textContent),
    "workspace header uses the single-buffer taxonomy, explicit scope, and inline latency"
  );
  workspacePayloadTransform = (payload) => ({
    ...payload,
    backend: "frobnicator",
  });
  await w.runWorkspaceCheck();
  ok(
    registry["backend-select"].value === "oxsmt" &&
      registry["status"].textContent.indexOf("unavailable") !== -1 &&
      registry["status"].textContent.indexOf("verified") === -1 &&
      registry["tabs"]
        .querySelectorAll(".tab")
        .every(
          (tab) =>
            tab.querySelectorAll(".tab-status-unavailable").length === 1
        ),
    "an unknown response backend degrades the header and every tab"
  );
  workspacePayloadTransform = null;
  await w.runWorkspaceCheck();
  workspacePayloadTransform = (payload) => {
    const activeEntry = {
      ...payload.files["Demo.ml"],
      obligation_summary: {
        ...payload.files["Demo.ml"].obligation_summary,
      },
    };
    delete activeEntry.obligation_summary.statuses;
    return {
      ...payload,
      files: { ...payload.files, "Demo.ml": activeEntry },
    };
  };
  await w.runWorkspaceCheck();
  ok(
    registry["status"].textContent.indexOf("unavailable") !== -1 &&
      registry["status"].textContent.indexOf("verified") === -1 &&
      registry["tabs"]
        .querySelectorAll(".tab")
        .every(
          (tab) =>
            tab.querySelectorAll(".tab-status-unavailable").length === 1
        ),
    "a missing active-unit summary status map cannot leave a green header"
  );
  workspacePayloadTransform = null;
  await w.runWorkspaceCheck();
  w.cm.setCursor({ line: 0, ch: 4 });
  ok(
    registry["cursor-type"].textContent === "int",
    "workspace refinement cursor types are filtered to the active unit"
  );
  workspacePayloadTransform = (payload) => ({ ...payload, hidden: 1 });
  await w.runWorkspaceCheck();
  const unavailableTabs = registry["tabs"].querySelectorAll(".tab");
  ok(
    registry["status"].textContent.indexOf("unavailable") !== -1 &&
      unavailableTabs.every(
        (tab) => tab.querySelectorAll(".tab-status-unavailable").length === 1
      ),
    "an unattributed hidden VC fails the workspace and every tab closed"
  );
  workspacePayloadTransform = null;
  await w.runWorkspaceCheck();
  workspacePayloadTransform = (payload) => {
    const files = { ...payload.files };
    delete files["Demo.mli"];
    return { ...payload, files };
  };
  await w.runWorkspaceCheck();
  let probedTabs = registry["tabs"].querySelectorAll(".tab");
  let missingTab = probedTabs.find(
    (tab) => tab.dataset.file === "Demo.mli"
  );
  ok(
    registry["status"].textContent.indexOf("unavailable") !== -1 &&
      registry["status"].textContent.indexOf("verified") === -1 &&
      missingTab.querySelectorAll(".tab-status-unavailable").length === 1 &&
      missingTab.querySelectorAll(".tab-status-error").length === 0,
    "a missing workspace unit makes the header and affected tab unavailable"
  );
  workspacePayloadTransform = (payload) => {
    const files = {
      ...payload.files,
      "Demo.ml": {
        types: payload.files["Demo.ml"].types,
        signature: payload.files["Demo.ml"].signature,
      },
    };
    const partial = { ...payload, files };
    delete partial.workspace_verification;
    return partial;
  };
  await w.runWorkspaceCheck();
  probedTabs = registry["tabs"].querySelectorAll(".tab");
  const malformedTab = probedTabs.find(
    (tab) => tab.dataset.file === "Demo.ml"
  );
  ok(
    registry["status"].textContent.indexOf("unavailable") !== -1 &&
      registry["status"].textContent.indexOf("verified") === -1 &&
      malformedTab.querySelectorAll(".tab-status-unavailable").length === 1 &&
      w.getVcs().length === 0,
    "missing per-unit outcome channels stay unavailable even when VCs remain"
  );
  workspacePayloadTransform = null;
  await w.runWorkspaceCheck();
  fetchLog = [];
  httpFailure = {
    path: "/workspace-check",
    status: 400,
    message: "request too large",
    remaining: 4,
  };
  await w.runWorkspaceCheck();
  await new Promise((resolve) => setTimeout(resolve, 320));
  const rejectedTabs = registry["tabs"].querySelectorAll(".tab");
  ok(
    fetchLog.filter((entry) => entry.url === "/workspace-check").length === 1 &&
      httpFailure.remaining === 3 &&
      registry["status"].textContent ===
        "⚠ unavailable · request rejected" &&
      rejectedTabs.every(
        (tab) => tab.querySelectorAll(".tab-status-unavailable").length === 1
      ) &&
      w.getVcs().length === 0 &&
      w.getRetryState().attempt === 0 &&
      w.getRetryState().scheduled === false,
    "workspace HTTP 400 terminates once with unavailable tabs and no retry"
  );
  httpFailure = null;
  await w.runWorkspaceCheck();
  // Routing: only the active unit's VCs are the pane/mark set; the rest are
  // cross-unit jump links.
  ok(w.getVcs().length === 1 && w.getVcs()[0].file === "Demo.ml", "active-file VCs are partitioned to the buffer");
  ok(w.getCrossUnitVcs().length === 2, "other units' VCs go to the cross-unit list");
  ok(registry["cross-unit"].hidden === false, "the cross-unit list is shown");
  const rows = registry["cross-unit"].querySelectorAll(".cross-unit-row");
  const rowFiles = rows.map((r) => r.dataset.file).sort();
  ok(
    rowFiles.length === 2 && rowFiles[0] === "Client.ml" && rowFiles[1] === "Demo.mli",
    "cross-unit rows name the other units"
  );
  const obligationRows = registry["obligations-list"].children;
  ok(
    obligationRows.length === 3 &&
      registry["obligations-summary"].textContent === "All obligations (3)" &&
      registry["obligations-list"].textContent.includes("Demo.ml:L1") &&
      registry["obligations-list"].textContent.includes("Demo.mli:L2") &&
      registry["obligations-list"].textContent.includes("Client.ml:L1"),
    "workspace all-obligations lists every unit with status, kind, goal, and file line"
  );
  const clientObligation = obligationRows.find((row) =>
    row.textContent.includes("Client.ml:L1")
  );
  clientObligation._fire("click", {});
  ok(
    w.getActiveFile() === "Client.ml" &&
      w.cm.getCursor().ch === 20 &&
      (registry["pane-body"]._html || "").includes("nonneg 5"),
    "a cross-unit obligation row switches units, jumps, and pins its proof"
  );

  const rewriteSummary = (summary, fileVcs) => {
    const statuses = {
      proved: 0,
      disproved: 0,
      unproved: 0,
      "solver-error": 0,
      unavailable: 0,
      unknown: 0,
    };
    fileVcs.forEach((vc) => { statuses[vc.status] += 1; });
    return { ...summary, total: fileVcs.length, statuses };
  };
  workspacePayloadTransform = (payload) => {
    const failingVcs = payload.vcs.map((vc) => ({
      ...vc,
      status: vc.id === 0 || vc.id === 2 ? "disproved" : vc.status,
    }));
    const files = {};
    Object.keys(payload.files).forEach((file) => {
      files[file] = {
        ...payload.files[file],
        obligation_summary: rewriteSummary(
          payload.files[file].obligation_summary,
          failingVcs.filter((vc) => vc.file === file)
        ),
      };
    });
    return {
      ...payload,
      files,
      vcs: failingVcs,
      obligation_summary: rewriteSummary(
        payload.obligation_summary,
        failingVcs
      ),
    };
  };
  await w.runWorkspaceCheck();
  w.cm._keymap.F8();
  ok(
    w.getActiveFile() === "Demo.ml" && w.cm.getCursor().ch === 16,
    "F8 traverses from a failing Client.ml obligation into Demo.ml"
  );
  w.cm._keymap["Shift-F8"]();
  ok(
    w.getActiveFile() === "Client.ml" && w.cm.getCursor().ch === 20,
    "Shift-F8 traverses backward across workspace units"
  );
  workspacePayloadTransform = null;
  await w.runWorkspaceCheck();

  // Tab switch re-partitions: the seal (anchored in Demo.mli) becomes the
  // active-file VC.
  await w.switchTab("Demo.mli");
  await tick();
  await tick();
  ok(w.getActiveFile() === "Demo.mli", "switchTab changes the active unit");
  ok(
    w.getVcs().length === 1 && w.getVcs()[0].kind === "seal",
    "the seal VC is now the active-file obligation on Demo.mli"
  );
  ok(
    w.getCrossUnitVcs().map((v) => v.file).sort().join(",") === "Client.ml,Demo.ml",
    "Demo.ml and Client.ml obligations are now cross-unit"
  );
  // The seal's supporting hypothesis originates in another unit -> cross-file.
  const seal = w.getVcs()[0];
  ok(
    seal.hypotheses[0].span && seal.hypotheses[0].span.file === "Demo.ml",
    "the seal hypothesis carries its origin unit (Demo.ml) for the cross-file jump"
  );

  // Cross-file jump: clicking a cross-unit row switches to that unit.
  const clientRow = registry["cross-unit"]
    .querySelectorAll(".cross-unit-row")
    .find((r) => r.dataset.file === "Client.ml");
  clientRow._fire("click", {});
  await tick();
  await tick();
  ok(w.getActiveFile() === "Client.ml", "clicking a cross-unit row jumps to that unit");

  // Latest-revision-wins: each new request aborts the obsolete lane immediately.
  console.log("Workspace latest-revision-wins:");
  const s = loadApp();
  await tick();
  await tick();
  await s.openWorkspace("Demo.ml");
  await tick();
  await tick();
  fetchLog = [];
  const calls = [s.runWorkspaceCheck(), s.runWorkspaceCheck(), s.runWorkspaceCheck()];
  await Promise.all(calls);
  await tick();
  await tick();
  await tick();
  const wsCalls = fetchLog.filter((f) => f.url === "/workspace-check").length;
  ok(wsCalls === 3, "three overlapping checks abort predecessors and start the latest immediately");

  // Leaving the workspace for a single-buffer file tears the tab strip down.
  await s.openFile({ path: "examples/overview.ml", kind: "ml" }, true);
  await tick();
  ok(s.isWorkspace() === false, "opening a single-buffer file leaves workspace mode");
  ok(registry["tabs"].hidden === true, "the tab strip is hidden on leaving");
  ok(registry["cross-unit"].hidden === true, "the cross-unit list is hidden on leaving");

  console.log("");
  if (failures) {
    console.log(failures + " check(s) FAILED");
    process.exit(1);
  }
  console.log("all frontend checks passed (" + checks + " checks)");
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
