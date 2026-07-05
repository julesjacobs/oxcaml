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
  mode: "text/x-ocaml",
  lineNumbers: true,
  value: SAMPLE,
});
cm.setValue(SAMPLE);

let regions = [];
let errors = [];
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
  return String(s).replace(/[&<>]/g, (c) =>
    ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" }[c])
  );
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
  if (sel.relation === "inside" && r) {
    // The cursor is AT this region: show it.
    modeEl.textContent = sel.mode + " · " + r.kind;
    if (r.kind === "vc") {
      html = renderVc(r);
    } else if (r.kind === "theorem") {
      html = renderTheorem(r) + liveButton();
    } else if (r.kind === "block") {
      html = '<p>Inside a <code>[%%vox.lean]</code> block.</p>' + liveButton();
    }
  } else if (sel.relation === "nearest" && r) {
    // Not at any region — do NOT present the nearest one as if it were
    // here. Empty state plus a muted, clickable secondary that jumps.
    modeEl.textContent = "no obligation at cursor";
    // Arrow points the way to the nearest region (it may sit below the
    // cursor, not just above).
    const arrow = sel.mode === "below" ? "↓" : "↑";
    html =
      '<p class="placeholder">No obligation at the cursor.</p>' +
      '<div class="nearest"><button id="jump-btn" class="jump">nearest ' +
      esc(REGION_NOUN[r.kind] || r.kind) +
      " " + arrow + " line " +
      (r.start.line + 1) +
      "</button></div>";
  } else {
    modeEl.textContent = "no obligation at cursor";
    html = '<p class="placeholder">No obligation at the cursor.</p>';
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

function badge(status) {
  const s = status || "unknown";
  return '<span class="badge badge-' + s + '">' + s + "</span>";
}

function renderVc(r) {
  let h = "<h3>goal" + badge(r.status) + "</h3>";
  const g = Selection.splitSpanSuffix(r.goal);
  h += provRow("goal", g.text, r.goal_span || g.span);
  h += renderHyps(r.hypotheses, r.hyp_spans);
  if (r.counterexample && r.counterexample.length) {
    h += "<h3>counterexample</h3>";
    h += '<div class="cex">' + esc(r.counterexample.join("\n")) + "</div>";
  }
  return h;
}

function renderTheorem(r) {
  // Static block theorems come from the Lean bridge, not the VC dumper, so
  // they carry no provenance spans -- rendered plain, no hover.
  let h = "<h3>theorem " + esc(r.name) + " (static)</h3>";
  h += '<div class="goal">' + esc(r.goal) + "</div>";
  return h + renderHyps(r.hypotheses);
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
  if (!errors.length) return "";
  return (
    '<div id="errors"><h3>errors</h3>' +
    errors
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

// Two-tier idle debounce: a fast (no-Lean) pass keeps the pane's
// hypotheses tracking the buffer as you type; the full check follows
// once typing pauses. Explicit triggers (button, Ctrl-Enter) go
// straight to a full check -- wrapped, since event handlers pass a
// truthy first argument that must not read as `fast`.
let timer = null;
let fastTimer = null;
cm.on("change", () => {
  clearMarks();
  clearHoverMark();
  setStatus("status-checking", "checking…");
  if (fastTimer) clearTimeout(fastTimer);
  fastTimer = setTimeout(() => check(true), 250);
  if (timer) clearTimeout(timer);
  timer = setTimeout(() => check(false), 900);
});
document.getElementById("check-btn").addEventListener("click", () => check(false));
cm.addKeyMap({
  "Ctrl-Enter": () => check(false),
  "Cmd-Enter": () => check(false),
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
    check();
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
  check();
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
};

init();
