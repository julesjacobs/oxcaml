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

check("no region on the line -> nearest above (not at cursor)", () => {
  const r = S.selectRegion(REGIONS, loc(3, 0));
  assert.strictEqual(r.relation, "nearest");
  assert.strictEqual(r.mode, "above");
  assert.strictEqual(r.region.kind, "vc");
});

check("nearest above prefers the closest", () => {
  const r = S.selectRegion(REGIONS, loc(8, 0));
  assert.strictEqual(r.relation, "nearest");
  assert.strictEqual(r.region.kind, "block");
});

check("none when cursor is above all regions", () => {
  const r = S.selectRegion(REGIONS, loc(0, 0));
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

console.log("\n" + passed + " tests passed");
