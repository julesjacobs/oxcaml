// Layer 4: selection logic (pure, no DOM, no network).
//
// Given the REGIONS from /check and a 0-based cursor {line, col}, decide
// which region's goal/hypotheses the proof pane should show. Priority,
// per the design:
//   1. the innermost region that ENCLOSES the cursor (smallest span);
//   2. else the nearest region PRECEDING the cursor on the same line;
//   3. else the nearest region ABOVE the cursor;
//   4. else nothing.
// A region of kind "block" or "theorem" routes to the Lean path (the
// pane can offer a live goal there); "vc" regions show their own
// goal/hypotheses directly.
//
// A region is {kind, start:{line,col}, end:{line,col}, ...payload}.
// Runs in node (module.exports) and the browser (window.Selection).

function cmp(a, b) {
  return a.line - b.line || a.col - b.col;
}

function contains(region, pos) {
  return cmp(region.start, pos) <= 0 && cmp(pos, region.end) <= 0;
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
  const enclosing = regions.filter((r) => contains(r, pos));
  if (enclosing.length) {
    enclosing.sort((a, b) => spanCmp(a, b) || kindRank(b) - kindRank(a));
    return { region: enclosing[0], mode: "enclosing" };
  }
  const sameLine = regions.filter(
    (r) => r.end.line === pos.line && r.end.col <= pos.col
  );
  if (sameLine.length) {
    // closest end column, then more specific kind
    sameLine.sort((a, b) => b.end.col - a.end.col || kindRank(b) - kindRank(a));
    return { region: sameLine[0], mode: "preceding" };
  }
  const above = regions.filter((r) => r.end.line < pos.line);
  if (above.length) {
    above.sort(
      (a, b) => b.end.line - a.end.line || b.end.col - a.end.col
    );
    return { region: above[0], mode: "above" };
  }
  return { region: null, mode: "none" };
}

function routesToLean(region) {
  return !!region && (region.kind === "block" || region.kind === "theorem");
}

const Selection = { cmp, contains, spanCmp, kindRank, selectRegion, routesToLean };

if (typeof module !== "undefined" && module.exports) module.exports = Selection;
if (typeof window !== "undefined") window.Selection = Selection;
