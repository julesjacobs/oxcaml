// Layer 4: selection logic (pure, no DOM, no network).
//
// Given the REGIONS from /check and a 0-based cursor {line, col}, decide
// which region the proof pane should show, and — crucially — WHETHER the
// cursor is actually at it. The result carries a `relation`:
//   "inside"  the cursor is AT a region (see atCursor): show it as now;
//   "nearest" the cursor is at no region, but one is nearby: the pane
//             shows an empty state and offers this as a secondary;
//   "none"    nothing to show.
// Among "inside" regions the innermost (smallest span, most specific
// kind) wins; the "nearest" region is the closest preceding on the line,
// else the closest by line distance in either direction (mode "above" or
// "below"), ties going to the one below.
//
// A region of kind "block" or "theorem" routes to the Lean path. Those
// regions count as "inside" ONLY when the cursor is within their span
// (contains) — the same closed interval lean_bridge.block_at uses — so
// the pane never claims in-block membership the live-goal query denies.
// A "vc" region is short, so being anywhere on its line(s) counts.
//
// A region is {kind, start:{line,col}, end:{line,col}, ...payload}.
// Runs in node (module.exports) and the browser (window.Selection).

function cmp(a, b) {
  return a.line - b.line || a.col - b.col;
}

function contains(region, pos) {
  return cmp(region.start, pos) <= 0 && cmp(pos, region.end) <= 0;
}

// Is the cursor AT this region (as opposed to merely near it)?
// strictVc: a VC claims the cursor only within its exact span --
// used by the FULL proof-state view, where an off-span cursor should
// fall through to the flow-precise program-point state instead of a
// same-line obligation with the WRONG branch facts (cursor on the
// then-branch must not show the else-branch's hypotheses).  Compact
// keeps the forgiving whole-line match.
function atCursor(region, pos, strictVc) {
  if (region.kind === "vc") {
    if (strictVc) return contains(region, pos);
    return region.start.line <= pos.line && pos.line <= region.end.line;
  }
  // Lean-routing regions must actually contain the cursor.
  return contains(region, pos);
}

// Lexicographic (lines, cols) span; smaller means more specific.
function spanCmp(a, b) {
  const al = a.end.line - a.start.line;
  const bl = b.end.line - b.start.line;
  if (al !== bl) return al - bl;
  return (a.end.col - a.start.col) - (b.end.col - b.start.col);
}

// Among equally-sized enclosing regions, prefer the more specific kind
// (a theorem/vc over a whole-block outline).
function kindRank(region) {
  if (region.kind === "vc") return 2;
  if (region.kind === "theorem") return 2;
  if (region.kind === "block") return 1;
  return 0;
}

// Column distance from the cursor to a region's span on the cursor's
// line: 0 when the span contains the cursor, else the gap to the nearer
// edge.  Only used to break ties among same-line regions the cursor is
// not inside; a region off the cursor's line sorts last.
function colDist(region, pos) {
  if (contains(region, pos)) return 0;
  if (pos.line < region.start.line || pos.line > region.end.line) {
    return Number.MAX_SAFE_INTEGER;
  }
  if (pos.col < region.start.col) return region.start.col - pos.col;
  if (pos.col > region.end.col) return pos.col - region.end.col;
  return 0;
}

function selectRegion(regions, pos, opts) {
  const strictVc = !!(opts && opts.strictVc);
  const inside = regions.filter((r) => atCursor(r, pos, strictVc));
  if (inside.length) {
    // Column-aware, so several obligations on ONE line each claim the
    // cursor over their own argument (e.g. `f ... k (k + 1) ...`, whose
    // preconditions carry the narrow spans of `k` and `(k + 1)`).  A
    // region that actually CONTAINS the cursor beats one that only shares
    // its line; among on-line-but-not-containing VCs the NEAREST by column
    // wins -- never an arbitrary fixed pick.  Ties fall back to the
    // innermost span, then the more specific kind.  (In strictVc / full
    // mode every `inside` region already contains the cursor, so the first
    // two keys are no-ops and this reduces to the old ordering.)
    inside.sort(
      (a, b) =>
        (contains(a, pos) ? 0 : 1) - (contains(b, pos) ? 0 : 1) ||
        colDist(a, pos) - colDist(b, pos) ||
        spanCmp(a, b) ||
        kindRank(b) - kindRank(a)
    );
    const region = inside[0];
    const mode = contains(region, pos) ? "enclosing" : "on-line";
    return { region, relation: "inside", mode };
  }
  // Not at any region: offer the nearest as a secondary. A region ending
  // earlier on THIS line wins outright; otherwise take the closest by line
  // distance in EITHER direction (above OR below -- a cursor in the file
  // header should still find the first obligation just below it), breaking
  // a tie toward the one below since reading flows downward.
  const sameLine = regions.filter(
    (r) => r.end.line === pos.line && r.end.col <= pos.col
  );
  if (sameLine.length) {
    sameLine.sort((a, b) => b.end.col - a.end.col || kindRank(b) - kindRank(a));
    return { region: sameLine[0], relation: "nearest", mode: "preceding" };
  }
  const nearestAbove = regions
    .filter((r) => r.end.line < pos.line)
    .sort((a, b) => b.end.line - a.end.line || b.end.col - a.end.col)[0];
  const nearestBelow = regions
    .filter(
      (r) =>
        r.start.line > pos.line ||
        (r.start.line === pos.line && r.start.col > pos.col)
    )
    .sort((a, b) => a.start.line - b.start.line || a.start.col - b.start.col)[0];
  if (nearestAbove && nearestBelow) {
    const distAbove = pos.line - nearestAbove.end.line;
    const distBelow = nearestBelow.start.line - pos.line;
    return distBelow <= distAbove
      ? { region: nearestBelow, relation: "nearest", mode: "below" }
      : { region: nearestAbove, relation: "nearest", mode: "above" };
  }
  if (nearestBelow) return { region: nearestBelow, relation: "nearest", mode: "below" };
  if (nearestAbove) return { region: nearestAbove, relation: "nearest", mode: "above" };
  return { region: null, relation: "none", mode: "none" };
}

function routesToLean(region) {
  return !!region && (region.kind === "block" || region.kind === "theorem");
}

// --- nested obligations ---------------------------------------------------
//
// A nested expression (`shrink (bump (bump n))`) emits several VCs at
// STRICTLY-CONTAINED, non-identical spans -- the outer call's obligation
// plus one per nested argument.  selectRegion picks a single primary (the
// innermost span containing the cursor); these two helpers let the pane
// surface the REST of the chain the cursor sits inside, so a sibling or
// parent obligation -- especially one whose goal text reads identically,
// like two `*arg* >= 0` -- is discoverable rather than hidden.

// Every kind:"vc" region whose span CONTAINS pos, innermost (smallest
// span, most specific kind) first.  Spatial containment only -- never the
// forgiving whole-line match -- so on a line with two disjoint nests the
// cursor in one nest never lists the other's obligations.
function containingVcs(regions, pos) {
  return (regions || [])
    .filter((r) => r.kind === "vc" && contains(r, pos))
    .sort((a, b) => spanCmp(a, b) || kindRank(b) - kindRank(a));
}

// A column on `line` at which selectRegion (under `opts`) selects exactly
// `target` -- used to make a nested obligation the primary selection when
// its pane row is clicked.  Prefers the target's start column (each nested
// shell's start is normally exclusive to it), then scans its span; falls
// back to the start column if nothing selects it (should not happen for a
// reachable obligation).
function columnFor(regions, target, line, opts) {
  const tryCol = (c) => selectRegion(regions, { line, col: c }, opts).region === target;
  if (tryCol(target.start.col)) return target.start.col;
  for (let c = target.start.col; c <= target.end.col; c += 1) {
    if (tryCol(c)) return c;
  }
  return target.start.col;
}

// --- provenance spans -----------------------------------------------------
//
// A provenance span is the source location a goal / hypothesis came from,
// in the COMPILER's convention: {start:{line,col}, end:{line,col}} with a
// 1-based line and a 0-based column. The server passes these through from
// -vox-dump-vc-provenance untouched; the two helpers below are the single
// point where that convention meets the pane and CodeMirror.

// The provenance suffix the compiler appends to a predicate: exactly two
// spaces, "@ ", then "line.col-line.col". A predicate can itself contain
// '@' (SSA names like x@1), so we split on the LAST such run anchored at
// the end of the string, never on a bare '@'.
const SPAN_SUFFIX = /^(.*)  @ (\d+)\.(\d+)-(\d+)\.(\d+)$/;

// Split "pred  @ L.C-L.C" into { text, span }. The server already strips
// the suffix and sends a structured span alongside, so on normal input the
// text arrives clean and this returns span:null -- it is a tolerant fallback
// that also copes with a raw suffix-bearing predicate. Never splits on a
// bare '@'.
function splitSpanSuffix(text) {
  const m = SPAN_SUFFIX.exec(text);
  if (!m) return { text, span: null };
  return {
    text: m[1],
    span: {
      start: { line: +m[2], col: +m[3] },
      end: { line: +m[4], col: +m[5] },
    },
  };
}

// Convert a provenance span (1-based line, 0-based col) to a CodeMirror
// mark range {from:{line,ch}, to:{line,ch}} (0-based line). Returns null
// for a null/undefined span so callers can `if (!range) return`.
function markFromSpan(span) {
  if (!span) return null;
  return {
    from: { line: span.start.line - 1, ch: span.start.col },
    to: { line: span.end.line - 1, ch: span.end.col },
  };
}

// A VC's identity for verdict carryover: its goal plus hypotheses,
// verbatim. The Lean theorem is generated from exactly this content
// (plus the module's fixed prelude), so two VCs with the same key have
// the same verdict -- which makes it sound to keep showing a proved /
// failed badge through a fast (no-Lean) re-check as long as the
// obligation's content didn't change.
function vcKey(region) {
  return region.goal + " " + (region.hypotheses || []).join(" ");
}

// Carry Lean verdicts (and their counterexamples) from `prev` regions
// onto `fresh` ones whose content matches, leaving fresh non-unknown
// statuses (e.g. an assume-VC's "trusted") untouched. Mutates and
// returns `fresh`. Content that changed -- or is new -- stays
// "unknown" until the next full check delivers its verdict.
function carryVerdicts(fresh, prev) {
  const byKey = new Map();
  (prev || []).forEach((r) => {
    if (r.kind === "vc" && r.status && r.status !== "unknown") {
      byKey.set(vcKey(r), r);
    }
  });
  (fresh || []).forEach((r) => {
    if (r.kind !== "vc" || (r.status && r.status !== "unknown")) return;
    const old = byKey.get(vcKey(r));
    if (!old) return;
    r.status = old.status;
    if (old.counterexample) r.counterexample = old.counterexample;
    if (old.lean_msg) r.lean_msg = old.lean_msg;
  });
  return fresh;
}

// Single-flight coalescing: at most ONE invocation of `task` in
// flight; calls landing meanwhile mark the flight dirty, and exactly
// one trailing invocation fires when it returns (with whatever state
// the task reads THEN -- callers read the live buffer at send time).
// This is the editor's only brake on as-you-type requests: zero added
// latency, backlog bounded at one, no matter how fast input arrives.
function singleFlight(task) {
  let inflight = false;
  let dirty = false;
  return async function fire() {
    if (inflight) {
      dirty = true;
      return;
    }
    inflight = true;
    try {
      do {
        dirty = false;
        await task();
      } while (dirty);
    } finally {
      inflight = false;
    }
  };
}

// Type-at-cursor: among the -annot ranges containing pos (both 0-based
// lines and cols here -- the server already normalised), pick the
// INNERMOST (smallest span) -- the expression the cursor is on, not the
// enclosing let. Returns {start, end, type} or null.
function typeAtPos(types, pos) {
  let best = null;
  (types || []).forEach((t) => {
    if (!contains(t, pos)) return;
    if (best === null || spanCmp(t, best) < 0) best = t;
  });
  return best;
}

// Program-point state at the cursor: the INNERMOST state span
// containing pos (states nest like expressions). null off all spans.
function stateAtPos(states, pos) {
  let best = null;
  (states || []).forEach((st) => {
    if (!contains(st, pos)) return;
    if (best === null || spanCmp(st, best) < 0) best = st;
  });
  return best;
}

// --- verdict taxonomy (shared by the source markers, the pane badge,
//     and the status bar) ------------------------------------------------
//
// A failing solve leaves each obligation in one of three DISPLAY
// families, deliberately distinct so a user never mistakes "grind gave
// up" for "your spec is false":
//   proved     still holds -> green (unchanged even when a sibling fails)
//   disproved  a Lean-VALIDATED counterexample -> solid red
//   unproved   failed with no validated witness -> dashed red.  This
//              folds in the legacy "failed" status and the
//              rest-of-the-failures class that skips the classifier.
// (trusted / unknown are their own families, styled separately.)
const FAIL_STATUSES = ["failed", "disproved", "unproved"];

function verdictFamily(status) {
  if (status === "disproved") return "disproved";
  if (status === "unproved" || status === "failed") return "unproved";
  if (status === "proved") return "proved";
  if (status === "trusted") return "trusted";
  return "unknown";
}

// Status-bar text for a FAILING full check.  Count the display families
// so a genuinely-false goal (disproved) is never hidden behind the
// milder "unproved": "1 disproved / 2 unproved / 13 proved ✗" when any
// disproof exists, else "2 unproved / 13 proved ✗".  Falls back to
// "errors ✗" when NO VC carries a verdict (a plain compile error, or an
// old compiler reporting only the first failure).
function failSummary(regions) {
  let disproved = 0;
  let unproved = 0;
  let proved = 0;
  (regions || []).forEach((r) => {
    if (r.kind !== "vc") return;
    const f = verdictFamily(r.status);
    if (f === "disproved") disproved += 1;
    else if (f === "unproved") unproved += 1;
    else if (f === "proved") proved += 1;
  });
  if (disproved + unproved + proved === 0) return "errors ✗";
  const parts = [];
  if (disproved) parts.push(disproved + " disproved");
  if (unproved) parts.push(unproved + " unproved");
  parts.push(proved + " proved");
  return parts.join(" / ") + " ✗";
}

const Selection = {
  cmp,
  contains,
  atCursor,
  spanCmp,
  kindRank,
  colDist,
  selectRegion,
  containingVcs,
  columnFor,
  routesToLean,
  splitSpanSuffix,
  markFromSpan,
  vcKey,
  carryVerdicts,
  singleFlight,
  typeAtPos,
  stateAtPos,
  FAIL_STATUSES,
  verdictFamily,
  failSummary,
};

if (typeof module !== "undefined" && module.exports) module.exports = Selection;
if (typeof window !== "undefined") window.Selection = Selection;
