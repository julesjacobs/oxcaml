// Tests for selection.js. Run: node test_selection.js
// Exits non-zero on failure. No dependencies beyond node's assert.

const assert = require("assert");
const S = require("./selection.js");

function loc(line, col) {
  return { line, col };
}

// A sample /check region set:
//   a VC on line 1, cols 20-21
//   a whole block spanning lines 4-7
//   a theorem inside the block spanning lines 5-6
const VC = { kind: "vc", start: loc(1, 20), end: loc(1, 21), goal: "x >= 0" };
const BLOCK = { kind: "block", start: loc(4, 0), end: loc(7, 0) };
const THM = {
  kind: "theorem",
  start: loc(5, 0),
  end: loc(6, 10),
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
  assert.strictEqual(r.mode, "enclosing");
  assert.strictEqual(r.region.kind, "vc");
});

check("innermost picks theorem over block", () => {
  const r = S.selectRegion(REGIONS, loc(5, 4));
  assert.strictEqual(r.mode, "enclosing");
  assert.strictEqual(r.region.kind, "theorem");
  assert.ok(S.routesToLean(r.region));
});

check("inside block but outside theorem picks block", () => {
  const r = S.selectRegion(REGIONS, loc(4, 2));
  assert.strictEqual(r.region.kind, "block");
  assert.ok(S.routesToLean(r.region));
});

check("nearest preceding on same line", () => {
  // line 1, col 40 is past the VC's end col (21); the VC precedes it.
  const r = S.selectRegion(REGIONS, loc(1, 40));
  assert.strictEqual(r.mode, "preceding");
  assert.strictEqual(r.region.kind, "vc");
});

check("nearest above when nothing on line", () => {
  // line 3 has no region; the VC (ends line 1) is the nearest above.
  const r = S.selectRegion(REGIONS, loc(3, 0));
  assert.strictEqual(r.mode, "above");
  assert.strictEqual(r.region.kind, "vc");
});

check("nearest above prefers the closest", () => {
  // line 8 is below everything; the block (ends line 7) is closest above.
  const r = S.selectRegion(REGIONS, loc(8, 0));
  assert.strictEqual(r.mode, "above");
  assert.strictEqual(r.region.kind, "block");
});

check("none when cursor is above all regions", () => {
  const r = S.selectRegion(REGIONS, loc(0, 0));
  assert.strictEqual(r.mode, "none");
  assert.strictEqual(r.region, null);
  assert.strictEqual(S.routesToLean(r.region), false);
});

check("contains is inclusive at both ends", () => {
  assert.ok(S.contains(VC, loc(1, 20)));
  assert.ok(S.contains(VC, loc(1, 21)));
  assert.ok(!S.contains(VC, loc(1, 22)));
});

console.log("\n" + passed + " tests passed");
