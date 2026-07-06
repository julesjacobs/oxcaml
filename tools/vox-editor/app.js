// Layer 5: the thin UI layer. All non-trivial logic lives in
// selection.js (tested under node); this file wires CodeMirror to the
// /check and /goal endpoints and renders the proof pane. Kept small on
// purpose.

const SAMPLE = `let rec total_ dbl n =
  if n <= 0 then 0 else 2 + dbl (n - 1)
[@@vox.decreases n]

[%%vox.lean {lean|
theorem dbl_nonneg (n : Int) (h : 0 <= n) : dbl n >= 0 := by
  fun_induction dbl n <;> omega
|lean}]

let use () =
  let refine_ ok = (dbl 0 : int{ _ = 0 }) in
  ok
`;

const cm = CodeMirror.fromTextArea(document.getElementById("code"), {
  mode: "text/x-vox",
  lineNumbers: true,
  value: SAMPLE,
});
cm.setValue(SAMPLE);

let regions = [];
let errors = [];
// Expression types from -annot (0-based coords), for type-at-cursor.
let exprTypes = [];
// Program-point states (facts + scope at each expression's entry).
let pointStates = [];
// Compact pane (default): goal then hypotheses, nothing else -- the
// original display. Unchecked = the full proof state (cursor type,
// context, hypotheses, turnstile goal, program-point view).
let compact = true;
let revision = 0;
let applied = -1;
let marks = [];
// A 0-based line the next successful check should move the cursor to (an
// example's suggested first-cursor / best-teaching line), consumed once so
// the pane opens on that obligation instead of line 1. Null when idle.
let pendingCursor = null;
// The source of the example currently loaded (or the initial SAMPLE), so
// we can warn before discarding hand edits when a new one is picked.
let lastLoaded = SAMPLE;

const statusEl = document.getElementById("status");
const modeEl = document.getElementById("pane-mode");
const bodyEl = document.getElementById("pane-body");

function esc(s) {
  // Internal names never reach the user: fresh unknowns display as ?N,
  // wildcard synthetics as _ (compiler task tracks the real fix).
  return String(s)
    .replace(/\*unknown(\d+)\*/g, "?$1")
    .replace(/\*vox-wild\*(#\d+)?/g, "_")
    .replace(/[&<>]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" }[c]));
}

function setStatus(cls, text) {
  statusEl.className = cls;
  statusEl.textContent = text;
}

function clearMarks() {
  marks.forEach((m) => m.clear());
  marks = [];
}

// Provenance hover: at most one transient CodeMirror mark painted over the
// source span a hovered goal/hypothesis came from. Cleared on mouse-out (and
// whenever the pane re-renders). `hoverSpans` collects the current pane's
// spans (compiler convention) so a row's data-prov-key indexes back to one.
let hoverMark = null;
let hoverSpans = [];

function clearHoverMark() {
  if (hoverMark) {
    hoverMark.clear();
    hoverMark = null;
  }
}

function paintSpan(span) {
  clearHoverMark();
  const range = Selection.markFromSpan(span);
  if (!range) return;
  hoverMark = cm.markText(range.from, range.to, { className: "vox-prov-hl" });
}

// A goal/hypothesis row. With a span it becomes hover-sensitive (the `prov`
// class + a key into hoverSpans); without one it renders exactly as before,
// with no affordance.
function provRow(cls, text, span) {
  if (!span) return '<div class="' + cls + '">' + esc(text) + "</div>";
  const key = hoverSpans.push(span) - 1;
  return (
    '<div class="' + cls + ' prov" data-prov-key="' + key + '">' +
    esc(text) +
    "</div>"
  );
}

// Wire mouseenter/mouseleave on the rows provRow marked hoverable.
function wireProvenanceHover() {
  clearHoverMark();
  bodyEl.querySelectorAll("[data-prov-key]").forEach((el) => {
    const span = hoverSpans[+el.dataset.provKey];
    el.addEventListener("mouseenter", () => paintSpan(span));
    el.addEventListener("mouseleave", clearHoverMark);
  });
}

function markRegions() {
  clearMarks();
  regions.forEach((r) => {
    let cls = null;
    if (r.kind === "vc") cls = "vc-" + (r.status || "unknown");
    else if (r.kind === "block") cls = "vc-block";
    if (!cls) return;
    marks.push(
      cm.markText(
        { line: r.start.line, ch: r.start.col },
        { line: r.end.line, ch: r.end.col },
        { className: cls }
      )
    );
  });
}

// One checking pipeline, two speeds. fast=true is the as-you-type pass:
// the server skips the Lean solve (~20ms dry-run instead of seconds), so
// the pane's goals/hypotheses/spans and any elaboration errors track the
// buffer almost live; verdicts of content-unchanged VCs are carried over
// (Selection.carryVerdicts), everything else shows "unknown" until the
// slower full check lands its authoritative statuses. The shared
// revision counter keeps late fast responses from clobbering a newer
// full result.
let lastCheckFast = null; // which pass painted last (browser-test probe)

async function check(fast) {
  const source = cm.getValue();
  revision += 1;
  const rev = revision;
  if (!fast) setStatus("status-checking", "checking…");
  try {
    const resp = await postJSON("/check", {
      source,
      revision: rev,
      fast: !!fast,
    });
    if (resp.revision < applied) return; // stale
    applied = resp.revision;
    const fresh = resp.regions || [];
    regions = resp.fast ? Selection.carryVerdicts(fresh, regions) : fresh;
    errors = resp.errors || [];
    exprTypes = resp.types || [];
    pointStates = resp.states || [];
    markRegions();
    if (resp.fast) {
      // Elaboration errors need no Lean, so a failing fast pass is
      // authoritative; a PASSING one is not a verdict -- leave the
      // "checking…" status for the full check to resolve.
      if (!resp.ok) setStatus("status-fail", "errors ✗");
    } else {
      setStatus(
        resp.ok ? "status-ok" : "status-fail",
        resp.ok ? "verified ✓" : "errors ✗"
      );
    }
    lastCheckFast = !!resp.fast;
    renderPane();
    applyPendingCursor();
  } catch (e) {
    if (!fast) setStatus("status-fail", "server error");
  }
}

// Move the cursor to a freshly-loaded example's suggested line, once the
// regions for it are in. Snap to the region that STARTS on that line so
// line-addressed VCs and span-addressed theorems both land precisely (a
// theorem is "inside" only when the cursor is within its columns); fall
// back to column 0 if nothing starts there.
function applyPendingCursor() {
  if (pendingCursor === null) return;
  const line = pendingCursor;
  pendingCursor = null;
  const here = regions
    .filter((r) => r.start.line === line)
    .sort(
      (a, b) => Selection.kindRank(b) - Selection.kindRank(a) || Selection.spanCmp(a, b)
    );
  const target = here[0];
  cm.setCursor(target ? { line: target.start.line, ch: target.start.col } : { line, ch: 0 });
  cm.focus();
}

const REGION_NOUN = {
  vc: "obligation",
  theorem: "theorem",
  block: "[%%vox.lean] block",
};

function renderPane() {
  const c = cm.getCursor();
  const sel = Selection.selectRegion(regions, { line: c.line, col: c.ch });
  const r = sel.region;
  hoverSpans = [];
  let html = "";
  // Type of the expression under the cursor (from -annot), shown above
  // whatever else the pane displays (full mode only).
  const at = compact
    ? null
    : Selection.typeAtPos(exprTypes, { line: c.line, col: c.ch });
  if (at) {
    const rng = { from: { line: at.start.line, ch: at.start.col },
                  to: { line: at.end.line, ch: at.end.col } };
    let snippet = cm.getRange(rng.from, rng.to);
    const om = /^\(?\s*Obj\.magic\s+([^)]*)\)?$/.exec(snippet || "");
    if (om) snippet = om[1].trim();
    // Skip when it just repeats a context row (x : int over x : int).
    const dup =
      r && r.scope &&
      r.scope.some((v) => v.name === snippet && v.ocaml === at.type);
    if (dup) {
      // nothing
    } else if (snippet && snippet.length <= 40 && snippet.indexOf("\n") < 0) {
      html +=
        '<div class="cursor-type"><span class="ctx-name">' + esc(snippet) +
        "</span> : " + esc(at.type) + "</div>";
    } else {
      html += '<div class="cursor-type">cursor : ' + esc(at.type) + "</div>";
    }
  }
  if (sel.relation === "inside" && r) {
    // The cursor is AT this region: show it. Header: a plain noun in
    // full mode; compact keeps the header empty (the body's own h3
    // already says what it is).
    modeEl.textContent = compact ? "" : (REGION_NOUN[r.kind] || r.kind);
    if (r.kind === "vc") {
      html += renderVc(r);
    } else if (r.kind === "theorem") {
      html += renderTheorem(r) + liveButton();
    } else if (r.kind === "block") {
      html += '<p>Inside a <code>[%%vox.lean]</code> block.</p>' + liveButton();
    }
  } else if (sel.relation === "nearest" && r) {
    // Not at any obligation: show the PROGRAM-POINT state -- the
    // context and facts that hold right here -- plus a muted jump to
    // the nearest obligation. (No goal: nothing to prove here.)
    const arrow = sel.mode === "below" ? "↓" : "↑";
    const stHtml = renderState({ line: c.line, col: c.ch });
    modeEl.textContent = stHtml ? "program point" : "";
    html += stHtml || '<p class="placeholder">No obligation at the cursor.</p>';
    html +=
      '<div class="nearest"><button id="jump-btn" class="jump">nearest ' +
      esc(REGION_NOUN[r.kind] || r.kind) +
      " " + arrow + " line " +
      (r.start.line + 1) +
      "</button></div>";
  } else {
    const stHtml = renderState({ line: c.line, col: c.ch });
    modeEl.textContent = stHtml ? "program point" : "";
    html += stHtml || '<p class="placeholder">No obligation at the cursor.</p>';
  }
  html += renderErrors();
  bodyEl.innerHTML = html;
  wireProvenanceHover();
  const btn = document.getElementById("live-btn");
  if (btn) btn.addEventListener("click", liveGoal);
  const jump = document.getElementById("jump-btn");
  if (jump && r) {
    jump.addEventListener("click", () => {
      cm.setCursor({ line: r.start.line, ch: r.start.col });
      cm.focus();
    });
  }
}

const BADGE_HINT = {
  proved: "Lean proved this obligation",
  failed: "Lean rejected this obligation (see counterexample)",
  unknown: "not yet checked",
  trusted: "assumed by construction (borrow/slice framing), not proved",
};

function badge(status) {
  const s = status || "unknown";
  return (
    '<span class="badge badge-' + s + '" title="' +
    esc(BADGE_HINT[s] || "") + '">' + s + "</span>"
  );
}

// A VC renders as a Lean/Rocq-style proof state: the context (each
// variable with its OxCaml type, solver sort dimmed), the hypotheses,
// then the goal behind a turnstile. Hover-provenance stays on the
// hypothesis/goal rows.
// The raw Lean sort names are solver spellings; show readable labels.
function leanLabel(sort) {
  if (sort === "VoxU") return "opaque";
  const m = /^Vox_[A-Za-z0-9]+_(.+)$/.exec(sort);
  if (m) return m[1];
  if (sort === "Vox_unit") return "unit";
  return sort;
}

function renderCtx(scope) {
  if (!scope || !scope.length) return "";
  return (
    "<h3>context</h3>" +
    scope
      .map((v) => {
        const inner =
          '<span class="ctx-name">' + esc(v.name) + "</span> : " +
          esc(v.ocaml) +
          '<span class="ctx-lean">' + esc(leanLabel(v.lean)) + "</span>";
        // A row that knows its binder's span gets the same hover
        // affordance as hypotheses: hovering highlights the binding.
        if (!v.span) return '<div class="ctx">' + inner + "</div>";
        const key = hoverSpans.push(v.span) - 1;
        return (
          '<div class="ctx prov" data-prov-key="' + key + '">' + inner +
          "</div>"
        );
      })
      .join("")
  );
}

function renderModuleFacts(hyps, spans) {
  if (compact || !hyps || !hyps.length) return "";
  let rows = "";
  hyps.forEach((x, idx) => {
    const t = Selection.splitSpanSuffix(x);
    rows += provRow("hyp", t.text, (spans || [])[idx] || t.span);
  });
  return (
    '<details class="modfacts"><summary>module facts (' + hyps.length +
    ")</summary>" + rows + "</details>"
  );
}

function renderVc(r) {
  let h = "";
  const g = Selection.splitSpanSuffix(r.goal);
  if (compact) {
    // The original display: goal first, hypotheses after, nothing else.
    h += "<h3>goal" + badge(r.status) + "</h3>";
    h += provRow("goal", g.text, r.goal_span || g.span);
    h += renderHyps(r.hypotheses, r.hyp_spans);
  } else {
    h += renderCtx(r.scope);
    h += renderHyps(r.hypotheses, r.hyp_spans);
    h += renderModuleFacts(r.module_hypotheses, r.module_hyp_spans);
    h += "<h3>goal" + badge(r.status) + "</h3>";
    h += provRow("goal turnstile", g.text, r.goal_span || g.span);
  }
  if (r.counterexample && r.counterexample.length) {
    h += "<h3>counterexample</h3>";
    h +=
      '<div class="cex">goal is false when:\n' +
      esc(r.counterexample.join("\n")) + "</div>";
  }
  return h;
}

// The proof state of "here" when the cursor is on no obligation: the
// innermost program-point state's context + facts. Same sections and
// hover behavior as a VC, no goal.
function renderState(pos) {
  if (compact) return null; // compact pane: obligations only
  const st = Selection.stateAtPos(pointStates, pos);
  if (!st) return null;
  let h = renderCtx(st.scope);
  if (st.hypotheses && st.hypotheses.length) {
    h += renderHyps(st.hypotheses, st.hyp_spans);
  } else if (h) {
    h += "<h3>hypotheses</h3><div class='hyp'>—</div>";
  }
  h += renderModuleFacts(st.module_hypotheses, st.module_hyp_spans);
  return h || null;
}

function renderTheorem(r) {
  // Static block theorems come from the Lean bridge, not the VC dumper, so
  // they carry no provenance spans -- rendered plain, no hover. Same
  // order as a VC: hypotheses above, the goal behind a turnstile.
  let h = "<h3>theorem " + esc(r.name) + "</h3>";
  h += renderHyps(r.hypotheses);
  h += "<h3>goal</h3>";
  h += '<div class="goal turnstile">' + esc(r.goal) + "</div>";
  return h;
}

// `spans` (optional) is parallel to `hyps`: a per-hypothesis provenance span
// or null. A hypothesis with a span becomes hover-sensitive; one without
// renders exactly as today.
function renderHyps(hyps, spans) {
  if (!hyps || !hyps.length) return "<h3>hypotheses</h3><div class='hyp'>—</div>";
  spans = spans || [];
  return (
    "<h3>hypotheses</h3>" +
    hyps
      .map((x, idx) => {
        const s = Selection.splitSpanSuffix(x);
        return provRow("hyp", s.text, spans[idx] || s.span);
      })
      .join("")
  );
}

function liveButton() {
  return '<p><button id="live-btn">Get live Lean goal at cursor</button></p>';
}

function renderErrors() {
  // A failed VC already shows its red badge + counterexample; the
  // generic "verification failed" error on top is pure duplication.
  const anyFailed = regions.some(
    (r) => r.kind === "vc" && r.status === "failed"
  );
  const shown = errors.filter(
    (e) => !(anyFailed && /vox: verification failed/.test(e.message || ""))
  );
  if (!shown.length) return "";
  return (
    '<div id="errors"><h3>errors</h3>' +
    shown
      .map((e) => '<div class="err">' + esc(e.message || "") + "</div>")
      .join("") +
    "</div>"
  );
}

async function liveGoal() {
  const c = cm.getCursor();
  const btn = document.getElementById("live-btn");
  if (btn) btn.textContent = "querying Lean…";
  try {
    const resp = await postJSON("/goal", {
      source: cm.getValue(),
      line: c.line,
      col: c.ch,
      revision: applied,
    });
    let h;
    if (resp.status === "ok" && resp.goals.length) {
      h =
        "<h3>live Lean goal</h3>" +
        resp.goals
          .map((g) => '<div class="goal live-goals">' + esc(g) + "</div>")
          .join("");
    } else {
      h = '<p class="placeholder">' + esc(resp.detail || resp.status) + "</p>";
    }
    bodyEl.insertAdjacentHTML("afterbegin", h);
  } catch (e) {
    if (btn) btn.textContent = "Lean query failed";
  }
}

async function postJSON(path, body) {
  const resp = await fetch(path, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  return resp.json();
}

// Cursor moves: pane only (client-side, no network).
cm.on("cursorActivity", renderPane);

// Light-speed scheduling: every change fires BOTH passes immediately --
// no debounce delay -- with single-flight coalescing as the only brake.
// Each channel keeps at most one request in flight; keystrokes landing
// meanwhile just mark the channel dirty, and the moment the in-flight
// response returns, one new request fires with the LATEST buffer
// (check() reads cm.getValue() at send time). So the backlog is bounded
// at one per channel no matter how fast you type, while freshness is
// bounded only by the round-trip: ~100ms for the fast (no-Lean) pass,
// a couple of seconds for the full Lean verdicts running continuously
// behind it.
// (coalescing logic lives in selection.js, node-tested)
const fireFast = Selection.singleFlight(() => check(true));
const fireFull = Selection.singleFlight(() => check(false));

// Marks are NOT cleared on change: CodeMirror shifts them with the edit,
// and the fast pass re-lays them almost immediately -- clearing first
// just made the underlines flicker on every keystroke.
cm.on("change", () => {
  clearHoverMark();
  setStatus("status-checking", "checking…");
  fireFast();
  fireFull();
});
document.getElementById("check-btn").addEventListener("click", fireFull);
cm.addKeyMap({
  "Ctrl-Enter": fireFull,
  "Cmd-Enter": fireFull,
});

// Theme: dark by default (no OS sniffing); the toolbar toggle flips
// [data-theme="light"] on <html> and persists the choice. The <head>
// applies the saved theme pre-paint; here we sync the button label and
// wire the toggle.
const THEME_KEY = "vox-editor-theme";
const themeBtn = document.getElementById("theme-btn");

function currentTheme() {
  return document.documentElement.dataset.theme === "light" ? "light" : "dark";
}

function applyTheme(theme) {
  if (theme === "light") document.documentElement.dataset.theme = "light";
  else delete document.documentElement.dataset.theme; // dark = no attribute
  themeBtn.textContent = theme === "light" ? "☾ Dark" : "☀ Light";
}

function initTheme() {
  let saved = "dark";
  try {
    saved = localStorage.getItem(THEME_KEY) || "dark";
  } catch (e) {}
  applyTheme(saved);
}

themeBtn.addEventListener("click", () => {
  const next = currentTheme() === "light" ? "dark" : "light";
  try {
    localStorage.setItem(THEME_KEY, next);
  } catch (e) {}
  applyTheme(next);
});

initTheme();

// Compact pane toggle (persisted; default checked = compact).
const COMPACT_KEY = "vox-editor-compact";
const compactBox = document.getElementById("compact-box");

function initCompact() {
  try {
    compact = localStorage.getItem(COMPACT_KEY) !== "off";
  } catch (e) {}
  compactBox.checked = compact;
}

compactBox.addEventListener("change", () => {
  compact = compactBox.checked;
  try {
    localStorage.setItem(COMPACT_KEY, compact ? "on" : "off");
  } catch (e) {}
  renderPane();
});

initCompact();

// Examples dropdown: populated from /examples, each choice loads the
// source and re-checks. A confirm() guards unsaved edits.
const examplesEl = document.getElementById("examples");
let examplesList = [];

async function loadExamples() {
  try {
    const resp = await fetch("/examples");
    examplesList = (await resp.json()).examples || [];
  } catch (e) {
    examplesList = []; // no examples served; leave the dropdown as-is
  }
  examplesList.forEach((ex) => {
    const opt = document.createElement("option");
    opt.value = ex.name;
    opt.textContent = (ex.verifies ? "" : "✗ ") + ex.title;
    opt.title = ex.description;
    examplesEl.appendChild(opt);
  });
  return examplesList;
}

// Load an example into the buffer and re-check. `force` skips the
// unsaved-buffer guard (used on startup, where there is nothing to lose).
async function loadExample(name, force) {
  if (!force && cm.getValue() !== lastLoaded &&
      !confirm("Discard your edits and load this example?")) {
    return false;
  }
  try {
    const resp = await fetch("/examples/" + encodeURIComponent(name));
    if (!resp.ok) return false;
    const source = await resp.text();
    lastLoaded = source;
    cm.setValue(source);
    examplesEl.value = name; // reflect the loaded example in the dropdown
    // Open on the example's suggested teaching line (1-based in the
    // manifest), applied by the check we kick off below.
    const meta = examplesList.find((e) => e.name === name);
    pendingCursor =
      meta && typeof meta.cursor === "number" ? meta.cursor - 1 : null;
    fireFull();
    return true;
  } catch (e) {
    setStatus("status-fail", "could not load example");
    return false;
  }
}

// Back-compat alias for the guarded, user-initiated path.
const pickExample = (name) => loadExample(name, false);

examplesEl.addEventListener("change", () => {
  const name = examplesEl.value;
  if (name) loadExample(name, false);
});

// Startup: open with the default example (auto-checked) rather than a
// bare sample. Falls back to checking whatever is in the buffer.
async function init() {
  const list = await loadExamples();
  const def = list.find((e) => e.default) || list[0];
  if (def && (await loadExample(def.name, true))) return;
  fireFull();
}

// Testability hook for the headless browser smoke test.
window.__vox = {
  cm,
  check,
  renderPane,
  loadExample,
  pickExample,
  loadExamples,
  getRegions: () => regions,
  getLastCheckFast: () => lastCheckFast,
  getTypes: () => exprTypes,
  getStates: () => pointStates,
  setCompact: (v) => {
    compact = !!v;
    document.getElementById("compact-box").checked = compact;
    renderPane();
  },
};

init();
