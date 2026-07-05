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
function atCursor(region, pos) {
  if (region.kind === "vc") {
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

function selectRegion(regions, pos) {
  const inside = regions.filter((r) => atCursor(r, pos));
  if (inside.length) {
    inside.sort((a, b) => spanCmp(a, b) || kindRank(b) - kindRank(a));
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
    .filter((r) => r.start.line > pos.line)
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

const Selection = {
  cmp,
  contains,
  atCursor,
  spanCmp,
  kindRank,
  selectRegion,
  routesToLean,
};

if (typeof module !== "undefined" && module.exports) module.exports = Selection;
if (typeof window !== "undefined") window.Selection = Selection;
