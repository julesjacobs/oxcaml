// Tests for selection.js. Run: node test_selection.js
// Exits non-zero on failure. No dependencies beyond node's assert.

const assert = require("assert");
const S = require("./selection.js");

function loc(line, col) {
  return { line, col };
}

// A sample /check region set, with realistic coordinates:
//   a VC on line 1, cols 20-21
//   a whole [%%vox.lean] block: its CONTENT spans from just after the
//     `{lean|` on line 4 (col 18) to the start of the `|lean}]` line (7,0),
//     exactly as the server reports it (offset_to_linecol of the content
//     bounds) and exactly the closed interval lean_bridge.block_at uses.
//   a theorem inside the block, lines 5-7.
const VC = { kind: "vc", start: loc(1, 20), end: loc(1, 21), goal: "x >= 0" };
const BLOCK = { kind: "block", start: loc(4, 18), end: loc(7, 0) };
const THM = {
  kind: "theorem",
  start: loc(5, 0),
  end: loc(7, 0),
  name: "helper",
  goal: "n + 1 >= 1",
};
const REGIONS = [VC, BLOCK, THM];

let passed = 0;
function check(name, fn) {
  fn();
  passed += 1;
  console.log("ok - " + name);
}

check("enclosing VC when cursor on it", () => {
  const r = S.selectRegion(REGIONS, loc(1, 20));
  assert.strictEqual(r.relation, "inside");
  assert.strictEqual(r.mode, "enclosing");
  assert.strictEqual(r.region.kind, "vc");
});

check("innermost picks theorem over block", () => {
  const r = S.selectRegion(REGIONS, loc(5, 4));
  assert.strictEqual(r.relation, "inside");
  assert.strictEqual(r.region.kind, "theorem");
  assert.ok(S.routesToLean(r.region));
});

check("inside block but outside theorem picks block", () => {
  // (4,20) is past `{lean|` (col 18) but above the theorem (line 5).
  const r = S.selectRegion(REGIONS, loc(4, 20));
  assert.strictEqual(r.relation, "inside");
  assert.strictEqual(r.region.kind, "block");
  assert.ok(S.routesToLean(r.region));
});

check("anywhere on a VC's line counts as inside", () => {
  // col 40 is past the VC's end col (21) but on its line; the user
  // clicking anywhere on that line should see the obligation.
  const r = S.selectRegion(REGIONS, loc(1, 40));
  assert.strictEqual(r.relation, "inside");
  assert.strictEqual(r.mode, "on-line");
  assert.strictEqual(r.region.kind, "vc");
});

check("off-region -> nearest by distance (closer below beats farther above)", () => {
  // Line 3 is 2 lines below the VC (ends line 1) but only 1 above the
  // block (starts line 4): the closer region, below, wins.
  const r = S.selectRegion(REGIONS, loc(3, 0));
  assert.strictEqual(r.relation, "nearest");
  assert.strictEqual(r.mode, "below");
  assert.strictEqual(r.region.kind, "block");
});

check("nearest above prefers the closest", () => {
  const r = S.selectRegion(REGIONS, loc(8, 0));
  assert.strictEqual(r.relation, "nearest");
  assert.strictEqual(r.mode, "above");
  assert.strictEqual(r.region.kind, "block");
});

check("above all regions -> nearest BELOW (the first obligation)", () => {
  // The bug: a cursor in the file header (line 0), above every region,
  // used to get a bare empty state. Now it finds the first VC just below.
  const r = S.selectRegion(REGIONS, loc(0, 0));
  assert.strictEqual(r.relation, "nearest");
  assert.strictEqual(r.mode, "below");
  assert.strictEqual(r.region.kind, "vc");
});

check("equidistant above and below -> tie goes below", () => {
  const a = { kind: "vc", start: loc(2, 0), end: loc(2, 5), goal: "a" };
  const b = { kind: "vc", start: loc(6, 0), end: loc(6, 5), goal: "b" };
  const r = S.selectRegion([a, b], loc(4, 0)); // 2 above, 2 below
  assert.strictEqual(r.relation, "nearest");
  assert.strictEqual(r.mode, "below");
  assert.strictEqual(r.region, b);
});

check("none only when there are no regions at all", () => {
  const r = S.selectRegion([], loc(3, 0));
  assert.strictEqual(r.relation, "none");
  assert.strictEqual(r.region, null);
  assert.strictEqual(S.routesToLean(r.region), false);
});

check("contains is inclusive at both ends", () => {
  assert.ok(S.contains(VC, loc(1, 20)));
  assert.ok(S.contains(VC, loc(1, 21)));
  assert.ok(!S.contains(VC, loc(1, 22)));
});

// --- BUG 1 regressions: a block is "inside" only when the cursor is
// genuinely within its content span (matching lean_bridge.block_at), so
// the pane never claims in-block for a position the live-goal denies.

check("cursor on the |lean}] delimiter line is NOT inside the block", () => {
  // (7,2) is on the closing delimiter, col past the block end (7,0):
  // block_at reports not-in-block, so selection must not either.
  const r = S.selectRegion(REGIONS, loc(7, 2));
  assert.notStrictEqual(r.relation, "inside");
  assert.strictEqual(r.relation, "nearest");
});

check("cursor on the blank line below a block is NOT inside it", () => {
  const r = S.selectRegion(REGIONS, loc(8, 0));
  assert.notStrictEqual(r.relation, "inside");
});

check("cursor genuinely inside block content IS inside", () => {
  const r = S.selectRegion(REGIONS, loc(6, 4));
  assert.strictEqual(r.relation, "inside");
  assert.ok(S.routesToLean(r.region));
});

// --- provenance: markFromSpan coordinate conversion ----------------------
// A span is {start:{line,col}, end:{line,col}} in the compiler convention:
// 1-based line, 0-based col. markFromSpan shifts the line to CodeMirror's
// 0-based {line: L-1, ch: C}.

check("markFromSpan shifts 1-based line to 0-based, col unchanged", () => {
  const m = S.markFromSpan({ start: { line: 1, col: 58 }, end: { line: 1, col: 59 } });
  assert.deepStrictEqual(m.from, { line: 0, ch: 58 });
  assert.deepStrictEqual(m.to, { line: 0, ch: 59 });
});

check("markFromSpan handles a multi-line span", () => {
  const m = S.markFromSpan({ start: { line: 3, col: 2 }, end: { line: 5, col: 8 } });
  assert.deepStrictEqual(m.from, { line: 2, ch: 2 });
  assert.deepStrictEqual(m.to, { line: 4, ch: 8 });
});

check("markFromSpan of a zero-width / col-0 span", () => {
  const m = S.markFromSpan({ start: { line: 2, col: 0 }, end: { line: 2, col: 0 } });
  assert.deepStrictEqual(m.from, { line: 1, ch: 0 });
  assert.deepStrictEqual(m.to, { line: 1, ch: 0 });
});

check("markFromSpan of null/undefined is null", () => {
  assert.strictEqual(S.markFromSpan(null), null);
  assert.strictEqual(S.markFromSpan(undefined), null);
});

// --- provenance: splitSpanSuffix edge cases ------------------------------

check("splitSpanSuffix peels a trailing span", () => {
  const r = S.splitSpanSuffix("x > 0  @ 1.58-1.59");
  assert.strictEqual(r.text, "x > 0");
  assert.deepStrictEqual(r.span, {
    start: { line: 1, col: 58 },
    end: { line: 1, col: 59 },
  });
});

check("splitSpanSuffix leaves a span-less predicate untouched", () => {
  const r = S.splitSpanSuffix("x@2 = (x@1 + 1)");
  assert.strictEqual(r.text, "x@2 = (x@1 + 1)");
  assert.strictEqual(r.span, null);
});

check("splitSpanSuffix keeps an @ in the predicate (SSA name)", () => {
  const r = S.splitSpanSuffix("x@1 = x + 1  @ 1.15-1.16");
  assert.strictEqual(r.text, "x@1 = x + 1");
  assert.deepStrictEqual(r.span, {
    start: { line: 1, col: 15 },
    end: { line: 1, col: 16 },
  });
});

check("splitSpanSuffix splits on the LAST coordinate suffix only", () => {
  const r = S.splitSpanSuffix("a  @ 1.2-3.4  @ 9.0-9.7");
  assert.strictEqual(r.text, "a  @ 1.2-3.4");
  assert.deepStrictEqual(r.span, {
    start: { line: 9, col: 0 },
    end: { line: 9, col: 7 },
  });
});

check("splitSpanSuffix does not split a bare trailing @", () => {
  const r = S.splitSpanSuffix("f @ g");
  assert.strictEqual(r.text, "f @ g");
  assert.strictEqual(r.span, null);
});

// -- carryVerdicts: fast-pass verdict carryover by VC content ------------

const vc = (goal, hyps, status, extra) =>
  Object.assign(
    { kind: "vc", goal, hypotheses: hyps, status },
    extra || {}
  );

check("carryVerdicts carries a proved verdict when content matches", () => {
  const prev = [vc("x >= 0", ["x = 1"], "proved")];
  const fresh = [vc("x >= 0", ["x = 1"], "unknown")];
  S.carryVerdicts(fresh, prev);
  assert.strictEqual(fresh[0].status, "proved");
});

check("carryVerdicts carries failed + counterexample + lean_msg", () => {
  const prev = [
    vc("x = 2", ["x = 1"], "failed", {
      counterexample: ["x = 1"],
      lean_msg: "grind failed",
    }),
  ];
  const fresh = [vc("x = 2", ["x = 1"], "unknown")];
  S.carryVerdicts(fresh, prev);
  assert.strictEqual(fresh[0].status, "failed");
  assert.deepStrictEqual(fresh[0].counterexample, ["x = 1"]);
  assert.strictEqual(fresh[0].lean_msg, "grind failed");
});

check("carryVerdicts leaves changed content unknown", () => {
  const prev = [vc("x >= 0", ["x = 1"], "proved")];
  const fresh = [vc("x >= 1", ["x = 1"], "unknown")];
  S.carryVerdicts(fresh, prev);
  assert.strictEqual(fresh[0].status, "unknown");
});

check("carryVerdicts never overwrites a fresh non-unknown status", () => {
  const prev = [vc("x >= 0", [], "proved")];
  const fresh = [vc("x >= 0", [], "trusted")];
  S.carryVerdicts(fresh, prev);
  assert.strictEqual(fresh[0].status, "trusted");
});

check("carryVerdicts ignores non-vc regions", () => {
  const prev = [vc("g", [], "proved")];
  const fresh = [{ kind: "theorem", goal: "g", hypotheses: [] }];
  S.carryVerdicts(fresh, prev);
  assert.strictEqual(fresh[0].status, undefined);
});

console.log("\n" + passed + " tests passed");
