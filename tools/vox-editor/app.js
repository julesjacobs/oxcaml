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
// Fade hypotheses the found proof did not reference (-vox-explain-proofs,
// r.hyp_used). On by default; the toolbar checkbox turns it off.
let fadeUnused = true;
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
// The file-explorer path id of the buffer's current file ("stdlib/vmap.ml",
// "examples/fib.ml"), or null for the built-in sample. Sent with every
// /check so the server stages a stdlib unit's interface artifacts; harmless
// for anything that is not a stdlib unit.
let currentPath = null;

const statusEl = document.getElementById("status");
const modeEl = document.getElementById("pane-mode");
const bodyEl = document.getElementById("pane-body");

function esc(s) {
  // Internal names never reach the user: a fresh unknown (a value the pass
  // could not reflect -- an opaque call result, a tuple/record component)
  // displays as anonN, wildcard synthetics as _.  NOT ?N: a leading `?`
  // reads as a Lean metavariable and alarms users into thinking the proof
  // state is broken.  The number correlates repeated uses (e.g. the three
  // projections of one anonymous triple).  Meaningful, source-derived
  // names are the compiler-side readable-names task (backlog #8).
  return String(s)
    .replace(/\*unknown(\d+)\*/g, "anon$1")
    .replace(/\*vox-wild\*(#\d+)?/g, "_")
    .replace(/[&<>]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" }[c]));
}

// Colorize a fragment of pane text with the SAME tokenizer the buffer
// uses (CodeMirror.voxTokenize from vox-mode.js), so a hypothesis / goal /
// type row reads exactly like the corresponding source. Each token's text
// is escaped individually and concatenated, so the row's textContent is
// byte-for-byte the input -- provenance hover keys and layout are
// untouched. `refine` true starts the tokenizer inside a refinement, for
// predicate text (hypotheses, goals, counterexamples) that matches the
// italic `type{ ... }` interior in the buffer; false is for plain program
// text such as an OCaml type expression in the context rows.
function tok(text, refine) {
  // Mask internal names on the WHOLE string before tokenizing: the
  // tokenizer splits *unknown1* into pieces the per-token mask in
  // esc() can never reassemble (found by review round 4).
  const masked = String(text)
    .replace(/\*unknown(\d+)\*/g, "anon$1")
    .replace(/\*vox-wild\*(#\d+)?/g, "_");
  const pairs = CodeMirror.voxTokenize(masked, refine ? { refine: 1 } : null);
  return pairs
    .map(([t, cls]) =>
      cls
        ? '<span class="' +
          cls.split(" ").map((c) => "cm-" + c).join(" ") +
          '">' +
          esc(t) +
          "</span>"
        : esc(t)
    )
    .join("");
}

function setStatus(cls, text) {
  statusEl.className = cls;
  statusEl.textContent = text;
}

// The failing-check status text and the verdict taxonomy live in
// selection.js (node-tested); see Selection.failSummary / verdictFamily.

// The verdict legend in the toolbar: a self-documenting key that appears
// only while the last full check is FAILING (exactly when the red lines
// need explaining) and disappears once everything is proved.  Each swatch
// reuses the vc-* marker classes, so the legend and the source underlines
// can never drift apart.  Toolbar-level, so compact mode (a pane concern)
// is untouched.
function renderLegend(regs, ok) {
  const el = document.getElementById("legend");
  if (!el) return;
  const anyFail =
    !ok &&
    (regs || []).some(
      (r) => r.kind === "vc" && FAILED_STATUSES.includes(r.status)
    );
  if (!anyFail) {
    el.hidden = true;
    el.innerHTML = "";
    return;
  }
  el.innerHTML =
    '<span class="leg vc-proved">proved</span>' +
    '<span class="leg vc-disproved">disproved (counterexample)</span>' +
    '<span class="leg vc-unproved">unproved (no witness)</span>';
  el.hidden = false;
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
function provRow(cls, text, span, title) {
  const body = tok(text, true); // predicate text: refinement-interior styling
  const t = title ? ' title="' + esc(title) + '"' : "";
  if (!span) return '<div class="' + cls + '"' + t + ">" + body + "</div>";
  const key = hoverSpans.push(span) - 1;
  return (
    '<div class="' + cls + ' prov" data-prov-key="' + key + '"' + t + ">" +
    body +
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
      path: currentPath,
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
        resp.ok ? "verified ✓" : Selection.failSummary(regions)
      );
      // The legend explains the red lines; only the authoritative full
      // pass carries verdicts, so drive it from here (not the fast pass).
      renderLegend(regions, resp.ok);
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
  // Full mode tracks the cursor COLUMN-precisely: an obligation only
  // claims the cursor inside its span, so elsewhere on the line the
  // program-point state (with the correct branch facts) shows instead.
  const sel = Selection.selectRegion(
    regions,
    { line: c.line, col: c.ch },
    { strictVc: !compact }
  );
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
    // Peel any chain of Obj.magic* coercion heads; if one still
    // remains (unparseable nesting), suppress the line entirely.
    for (let guard = 0; guard < 4; guard += 1) {
      const om = /^\(?\s*Obj\.magic\w*\s+(.*?)\)?\s*$/.exec(snippet || "");
      if (!om) break;
      snippet = om[1].trim();
    }
    if (/Obj\.magic/.test(snippet || "")) snippet = "";
    // Skip when it just repeats a context row (x : int over x : int).
    const dup =
      r && r.scope &&
      r.scope.some((v) => v.name === snippet && v.ocaml === at.type);
    if (dup) {
      // nothing
    } else if (snippet && snippet.length <= 40 && snippet.indexOf("\n") < 0) {
      html +=
        '<div class="cursor-type"><span class="ctx-name">' + esc(snippet) +
        "</span> : " + tok(at.type, false) + "</div>";
    } else {
      html += '<div class="cursor-type">cursor : ' + tok(at.type, false) + "</div>";
    }
  }
  if (sel.relation === "inside" && r) {
    // The cursor is AT this region: show it. Header: a plain noun in
    // full mode; compact keeps the header empty (the body's own h3
    // already says what it is).
    modeEl.textContent = compact ? "" : (REGION_NOUN[r.kind] || r.kind);
    if (r.kind === "vc") {
      // A loop [@vox.invariant] emits its establishment and preservation
      // obligations at the SAME span (the attribute's), so the cursor can
      // only ever select one of the pair.  Render EVERY co-located VC, each
      // under its role sublabel, so neither half is hidden behind the other.
      const group = coLocatedVcs(r);
      if (group.length > 1) {
        if (!compact) modeEl.textContent = "obligations";
        group.forEach((vc, i) => {
          html += vcGroupLabel(vc, i, group.length);
          html += renderVc(vc);
        });
      } else {
        html += renderVc(r);
      }
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
  failed: "Lean rejected this obligation; no counterexample was validated",
  disproved: "Lean rejected this obligation (a counterexample was validated)",
  unproved: "automation gave up; no counterexample found (may still hold)",
  unknown: "not yet checked",
  trusted: "assumed by construction (borrow/slice framing), not proved",
};

// The statuses a failing solve attaches to an obligation (shared with
// selection.js, which owns the verdict taxonomy).
const FAILED_STATUSES = Selection.FAIL_STATUSES;

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
function leanLabel(sort, ocaml) {
  let label = sort;
  if (sort === "VoxU") label = "opaque";
  else if (sort === "Vox_unit") label = "unit";
  else {
    const m = /^Vox_[A-Za-z0-9]+_(.+)$/.exec(sort);
    if (m) label = m[1];
  }
  // A label that repeats the OxCaml type, or a compound solver
  // spelling (VoxT2 VoxU Vox_unit), is noise -- show nothing.
  if (
    (ocaml && label.toLowerCase() === String(ocaml).toLowerCase()) ||
    /\s/.test(label) ||
    /^Vox/.test(label)
  ) {
    return "";
  }
  return label;
}

function renderCtx(scope) {
  if (!scope || !scope.length) return "";
  return (
    "<h3>context</h3>" +
    scope
      .map((v) => {
        const inner =
          '<span class="ctx-name">' + esc(v.name) + "</span> : " +
          tok(v.ocaml, false) +
          (leanLabel(v.lean, v.ocaml)
            ? '<span class="ctx-lean">' + esc(leanLabel(v.lean, v.ocaml)) +
              "</span>"
            : "");
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

// The lemmas grind used to close this VC (-vox-explain-proofs).  Full mode
// only, and only when the compiler reported it (r.used is a list of names;
// null means no report -- old compiler or no solver).  An empty list is an
// arithmetic/logic-only proof.  A name that matches a [%%vox.lean] theorem
// region gets the provenance hover affordance (data-prov-key into
// hoverSpans), so hovering it highlights that theorem's source span; plain
// names (an [@@vox.lemma] whose source is a function, a prelude fact) render
// without one.
function renderUsed(used) {
  if (compact || used == null) return "";
  if (!used.length) {
    return (
      '<div class="used">used lemmas: ' +
      '<span class="used-none">arithmetic only</span></div>'
    );
  }
  const parts = used.map((name) => {
    const region = regions.find(
      (r) => r.name === name && (r.kind === "theorem" || r.kind === "block")
    );
    if (!region) return '<span class="used-name">' + esc(name) + "</span>";
    // regions are 0-based; hoverSpans/markFromSpan want the compiler's
    // 1-based line (it subtracts 1), so shift the line back up by one.
    const span = {
      start: { line: region.start.line + 1, col: region.start.col },
      end: { line: region.end.line + 1, col: region.end.col },
    };
    const key = hoverSpans.push(span) - 1;
    return (
      '<span class="used-name prov" data-prov-key="' + key + '">' +
      esc(name) + "</span>"
    );
  });
  return '<div class="used">used lemmas: ' + parts.join(", ") + "</div>";
}

// Every VC region co-extensive with `r` (same start AND end).  A loop
// invariant's establishment and preservation obligations share the
// attribute's span, so selecting one must surface the whole group.
function coLocatedVcs(r) {
  return regions.filter(
    (x) =>
      x.kind === "vc" &&
      x.start.line === r.start.line && x.start.col === r.start.col &&
      x.end.line === r.end.line && x.end.col === r.end.col
  );
}

// Sublabel over one obligation of a co-located group.  The server tags a
// loop invariant's pair with roles (establishment / preservation); any other
// co-located group falls back to positional numbering.
function vcGroupLabel(vc, i, n) {
  const label = vc.role || "obligation " + (i + 1) + " of " + n;
  return '<h4 class="vc-role">' + esc(label) + "</h4>";
}

function renderVc(r) {
  let h = "";
  const g = Selection.splitSpanSuffix(r.goal);
  if (compact) {
    // The original display: goal first, hypotheses after, nothing else
    // -- and nothing means nothing: no empty-section headers either.
    h += "<h3>goal" + badge(r.status) + "</h3>";
    h += provRow("goal", g.text, r.goal_span || g.span);
    if (r.hypotheses && r.hypotheses.length) {
      h += renderHyps(r.hypotheses, r.hyp_spans, r.hyp_used);
    }
  } else {
    h += renderCtx(r.scope);
    h += renderHyps(r.hypotheses, r.hyp_spans, r.hyp_used);
    h += renderModuleFacts(r.module_hypotheses, r.module_hyp_spans);
    h += "<h3>goal" + badge(r.status) + "</h3>";
    h += provRow("goal turnstile", g.text, r.goal_span || g.span);
    h += renderUsed(r.used);
  }
  if (r.counterexample && r.counterexample.length) {
    // A "disproved" VC carries a VALIDATED counterexample (the solver
    // re-checked the assignment); label it so.
    const cexHead =
      r.status === "disproved" ? "counterexample (validated)" : "counterexample";
    h += "<h3>" + cexHead + "</h3>";
    h +=
      '<div class="cex">goal is false when:\n' +
      tok(r.counterexample.join("\n"), true) + "</div>";
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

// Static theorem text carries solver sort spellings verbatim; keep the
// "no raw internal names" invariant at the display boundary.
function leanText(t) {
  return String(t)
    .replace(/\bVoxU\b/g, "opaque")
    .replace(/\bVox_[A-Za-z0-9]+_([A-Za-z0-9_]+)\b/g, "$1")
    .replace(/\bVox_unit\b/g, "unit");
}

function renderTheorem(r) {
  // Static block theorems come from the Lean bridge, not the VC dumper, so
  // they carry no provenance spans -- rendered plain, no hover. Each mode
  // matches its VC layout: compact = goal first, no turnstile; full =
  // hypotheses above, the goal behind a turnstile.
  let h = "<h3>theorem " + esc(r.name) + "</h3>";
  const hyps = (r.hypotheses || []).map(leanText);
  if (compact) {
    h += "<h3>goal</h3>";
    h += '<div class="goal">' + tok(leanText(r.goal), true) + "</div>";
    if (hyps.length) h += renderHyps(hyps);
  } else {
    h += renderHyps(hyps);
    h += "<h3>goal</h3>";
    h += '<div class="goal turnstile">' + tok(leanText(r.goal), true) + "</div>";
  }
  return h;
}

// `spans` (optional) is parallel to `hyps`: a per-hypothesis provenance span
// or null. A hypothesis with a span becomes hover-sensitive; one without
// renders exactly as today.
function renderHyps(hyps, spans, used) {
  if (!hyps || !hyps.length) return "<h3>hypotheses</h3><div class='hyp'>—</div>";
  spans = spans || [];
  used = used || [];
  return (
    "<h3>hypotheses</h3>" +
    hyps
      .map((x, idx) => {
        const s = Selection.splitSpanSuffix(x);
        // used[idx] === false means the linter flagged this hypothesis as
        // absent from the proof grind found; fade it (unless the toggle is
        // off). The predicate text is untouched so provenance hover stays
        // byte-exact.
        const faded = fadeUnused && used[idx] === false;
        const cls = faded ? "hyp hyp-unused" : "hyp";
        const title = faded ? "unused in this proof" : undefined;
        return provRow(cls, s.text, spans[idx] || s.span, title);
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
    (r) => r.kind === "vc" && FAILED_STATUSES.includes(r.status)
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
  // A read-only .md doc is loaded for reading, not verifying; its
  // setValue still emits a change, so skip the check entirely.
  if (cm.getOption("readOnly")) return;
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

// Fade-unused toggle (persisted; default checked = fade).
const FADE_KEY = "vox-editor-fade";
const fadeBox = document.getElementById("fade-box");

function initFade() {
  try {
    fadeUnused = localStorage.getItem(FADE_KEY) !== "off";
  } catch (e) {}
  fadeBox.checked = fadeUnused;
}

fadeBox.addEventListener("change", () => {
  fadeUnused = fadeBox.checked;
  try {
    localStorage.setItem(FADE_KEY, fadeUnused ? "on" : "off");
  } catch (e) {}
  renderPane();
});

initFade();

// File explorer (task #76): a collapsible left-sidebar tree of the two
// allowlisted read-only roots (curated examples + vox_stdlib). Selecting an
// .ml/.mli loads it into the buffer and checks it; a stdlib unit checks
// with its interface artifacts staged server-side (we send currentPath with
// /check). A notes/*.md doc opens read-only, with no verification.
const treeEl = document.getElementById("tree");
// The curated examples' metadata (title / cursor) still comes from
// /examples; the tree itself comes from /ls. Kept so the pickExample /
// loadExample compatibility API (and the startup default) can open on an
// example's suggested teaching line.
let examplesList = [];
let treeData = null;
// path id -> file node, for cursor lookup and active-row highlighting.
const fileNodes = {};

async function loadExamples() {
  try {
    const resp = await fetch("/examples");
    examplesList = (await resp.json()).examples || [];
  } catch (e) {
    examplesList = [];
  }
  return examplesList;
}

function renderPlaceholder(text) {
  modeEl.textContent = "";
  bodyEl.innerHTML = '<p class="placeholder">' + esc(text) + "</p>";
}

function highlightActive(path) {
  treeEl.querySelectorAll(".tree-file.active").forEach((e) =>
    e.classList.remove("active")
  );
  treeEl.querySelectorAll(".tree-file").forEach((e) => {
    if (e.dataset.path === path) e.classList.add("active");
  });
}

// Load any allowlisted file into the buffer. `node` is a tree file node
// ({path, kind, ...}); `force` skips the unsaved-edits guard (startup).
async function openFile(node, force) {
  if (
    !force &&
    cm.getValue() !== lastLoaded &&
    !confirm("Discard your edits and load this file?")
  ) {
    return false;
  }
  try {
    const resp = await fetch("/file?path=" + encodeURIComponent(node.path));
    if (!resp.ok) return false;
    const source = await resp.text();
    const doc = node.kind === "doc";
    // Set read-only BEFORE setValue: setValue emits a change, and the
    // change handler skips the check when the buffer is read-only.
    cm.setOption("readOnly", doc);
    lastLoaded = source;
    currentPath = node.path;
    cm.setValue(source);
    highlightActive(node.path);
    if (doc) {
      setStatus("status-idle", "read-only doc");
      renderPlaceholder(
        "Documentation (read-only). Open an .ml or .mli to verify."
      );
      return true;
    }
    // Open an example on its suggested teaching line (1-based in the
    // manifest); other files open at the top.
    const meta = examplesList.find(
      (e) => "examples/" + e.name + ".ml" === node.path
    );
    pendingCursor =
      meta && typeof meta.cursor === "number" ? meta.cursor - 1 : null;
    fireFull();
    return true;
  } catch (e) {
    setStatus("status-fail", "could not load file");
    return false;
  }
}

// Compatibility API for the headless tests and startup: load a curated
// example by bare name (e.g. "fib").
async function loadExample(name, force) {
  return openFile({ path: "examples/" + name + ".ml", kind: "ml" }, force);
}
const pickExample = (name) => loadExample(name, false);

function renderFileNode(node) {
  const el = document.createElement("div");
  el.className = "tree-file kind-" + (node.kind || "file");
  el.setAttribute("role", "treeitem");
  el.dataset.path = node.path;
  el.title = node.title ? node.title : node.path;
  el.textContent = (node.verifies === false ? "✗ " : "") + node.name;
  el.addEventListener("click", () => openFile(node, false));
  fileNodes[node.path] = node;
  return el;
}

function renderDirNode(node, isRoot) {
  const wrap = document.createElement("div");
  wrap.className = "tree-dir" + (isRoot ? " tree-root" : "");
  const label = document.createElement("div");
  label.className = "tree-dir-label";
  label.setAttribute("role", "treeitem");
  const chevron = document.createElement("span");
  chevron.className = "tree-chevron";
  chevron.textContent = "▾";
  label.appendChild(chevron);
  const name = document.createElement("span");
  name.className = "tree-name";
  name.textContent = node.name;
  label.appendChild(name);
  const kids = document.createElement("div");
  kids.className = "tree-children";
  (node.children || []).forEach((c) =>
    kids.appendChild(c.type === "dir" ? renderDirNode(c, false) : renderFileNode(c))
  );
  label.addEventListener("click", () => {
    const collapsed = wrap.classList.toggle("collapsed");
    chevron.textContent = collapsed ? "▸" : "▾";
  });
  wrap.appendChild(label);
  wrap.appendChild(kids);
  return wrap;
}

function renderTree(data) {
  treeEl.innerHTML = "";
  (data.roots || []).forEach((root) =>
    treeEl.appendChild(renderDirNode(root, true))
  );
  if (currentPath) highlightActive(currentPath);
}

async function loadTree() {
  await loadExamples();
  try {
    const resp = await fetch("/ls");
    treeData = await resp.json();
  } catch (e) {
    treeData = { roots: [] };
  }
  renderTree(treeData);
  return treeData;
}

// Sidebar show/hide (persisted). Collapsing the whole explorer gives the
// editor the full width when the tree is not needed.
const sidebarBtn = document.getElementById("sidebar-btn");
const SIDEBAR_KEY = "vox-editor-sidebar";
function applySidebar(hidden) {
  document.body.classList.toggle("sidebar-hidden", hidden);
}
sidebarBtn.addEventListener("click", () => {
  const hidden = !document.body.classList.contains("sidebar-hidden");
  try {
    localStorage.setItem(SIDEBAR_KEY, hidden ? "hidden" : "shown");
  } catch (e) {}
  applySidebar(hidden);
});
try {
  applySidebar(localStorage.getItem(SIDEBAR_KEY) === "hidden");
} catch (e) {}

// Startup: render the tree, then open the default example (auto-checked).
// Falls back to checking whatever is in the buffer.
async function init() {
  await loadTree();
  const def = examplesList.find((e) => e.default) || examplesList[0];
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
  loadTree,
  openFile,
  getTree: () => treeData,
  getCurrentPath: () => currentPath,
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
