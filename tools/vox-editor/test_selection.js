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
// Sync tests report inline; a test returning a promise is awaited and
// reported when it settles (a rejection fails the run). The summary
// line waits for all of them.
const pending = [];
function check(name, fn) {
  const r = fn();
  if (r && typeof r.then === "function") {
    pending.push(
      r.then(
        () => {
          passed += 1;
          console.log("ok - " + name);
        },
        (e) => {
          console.error("FAIL - " + name + ": " + e.message);
          process.exitCode = 1;
        }
      )
    );
    return;
  }
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

// -- singleFlight: bounded as-you-type request scheduling ----------------

// check() is async in the app; these tests drive a controllable task.
async function flightScenario(fires, resolveAll) {
  let runs = 0;
  const resolvers = [];
  const fire = S.singleFlight(
    () => new Promise((res) => { runs += 1; resolvers.push(res); })
  );
  const done = [];
  for (let i = 0; i < fires; i += 1) done.push(fire());
  await resolveAll(resolvers, () => runs);
  await Promise.all(done);
  return runs;
}

check("singleFlight coalesces a burst into flight + one trailing", async () => {
  const runs = await flightScenario(10, async (resolvers, getRuns) => {
    assert.strictEqual(getRuns(), 1); // 9 calls landed mid-flight
    resolvers[0]();                   // finish flight -> ONE trailing run
    await new Promise((r) => setTimeout(r, 0));
    assert.strictEqual(getRuns(), 2);
    resolvers[1]();
    await new Promise((r) => setTimeout(r, 0));
  });
  assert.strictEqual(runs, 2, "10 fires -> exactly 2 task runs");
});

check("singleFlight runs sequential fires individually", async () => {
  let runs = 0;
  const fire = S.singleFlight(async () => { runs += 1; });
  await fire();
  await fire();
  await fire();
  assert.strictEqual(runs, 3);
});

// -- strictVc: full mode is column-precise for obligations ---------------

check("strictVc: off-span same-line cursor is NOT inside the VC", () => {
  const vcs = [
    { kind: "vc", start: { line: 5, col: 40 }, end: { line: 5, col: 53 } },
  ];
  const loose = S.selectRegion(vcs, { line: 5, col: 20 });
  assert.strictEqual(loose.relation, "inside"); // compact behavior
  const strict = S.selectRegion(vcs, { line: 5, col: 20 }, { strictVc: true });
  assert.strictEqual(strict.relation, "nearest");
  const on = S.selectRegion(vcs, { line: 5, col: 45 }, { strictVc: true });
  assert.strictEqual(on.relation, "inside");
});

// -- typeAtPos: innermost -annot range at the cursor ---------------------

const TYPES = [
  { start: { line: 2, col: 0 }, end: { line: 2, col: 20 }, type: "int" },
  { start: { line: 2, col: 4 }, end: { line: 2, col: 9 }, type: "ilist" },
  { start: { line: 2, col: 4 }, end: { line: 2, col: 6 }, type: "int -> ilist" },
];

check("typeAtPos picks the innermost containing range", () => {
  const t = S.typeAtPos(TYPES, { line: 2, col: 5 });
  assert.strictEqual(t.type, "int -> ilist");
});

check("typeAtPos falls back to a wider range off the inner one", () => {
  const t = S.typeAtPos(TYPES, { line: 2, col: 15 });
  assert.strictEqual(t.type, "int");
});

check("typeAtPos returns null outside all ranges", () => {
  assert.strictEqual(S.typeAtPos(TYPES, { line: 9, col: 0 }), null);
  assert.strictEqual(S.typeAtPos(null, { line: 2, col: 5 }), null);
});

// -- stateAtPos: innermost program-point state --------------------------

const STATES = [
  { start: { line: 1, col: 0 }, end: { line: 9, col: 5 },
    hypotheses: ["outer"], scope: [] },
  { start: { line: 3, col: 2 }, end: { line: 5, col: 10 },
    hypotheses: ["inner"], scope: [] },
];

check("stateAtPos picks the innermost enclosing state", () => {
  assert.strictEqual(
    S.stateAtPos(STATES, { line: 4, col: 0 }).hypotheses[0], "inner");
  assert.strictEqual(
    S.stateAtPos(STATES, { line: 8, col: 0 }).hypotheses[0], "outer");
  assert.strictEqual(S.stateAtPos(STATES, { line: 20, col: 0 }), null);
});

// -- verdict taxonomy: the three failure DISPLAY families ---------------

check("verdictFamily maps each status to its display family", () => {
  assert.strictEqual(S.verdictFamily("proved"), "proved");
  assert.strictEqual(S.verdictFamily("disproved"), "disproved");
  assert.strictEqual(S.verdictFamily("unproved"), "unproved");
  // legacy "failed" folds into the dashed-red unproved family
  assert.strictEqual(S.verdictFamily("failed"), "unproved");
  assert.strictEqual(S.verdictFamily("trusted"), "trusted");
  assert.strictEqual(S.verdictFamily("unknown"), "unknown");
  assert.strictEqual(S.verdictFamily(undefined), "unknown");
});

const vcr = (status) => ({ kind: "vc", status, goal: "g" });

check("failSummary names disproved separately from unproved", () => {
  const s = S.failSummary([
    vcr("disproved"),
    vcr("proved"),
    vcr("unproved"),
    vcr("proved"),
    { kind: "block" }, // ignored
  ]);
  assert.strictEqual(s, "1 disproved / 1 unproved / 2 proved ✗");
});

check("failSummary omits the disproved clause when there is none", () => {
  // failed groups into unproved -> 2 unproved
  const s = S.failSummary([vcr("unproved"), vcr("proved"), vcr("failed")]);
  assert.strictEqual(s, "2 unproved / 1 proved ✗");
});

check("failSummary falls back to 'errors' when no VC carries a verdict", () => {
  assert.strictEqual(
    S.failSummary([vcr("unknown"), { kind: "block" }]),
    "errors ✗"
  );
  assert.strictEqual(S.failSummary([]), "errors ✗");
});

// -- column-precise selection among MULTIPLE VCs on one line ------------
// The qsort `split3 ... k (k + 1) ...` line: two dependent-argument
// precondition VCs, whose narrow spans are `k` (cols 25-26) and `(k + 1)`
// (cols 27-34).  Compact mode (strictVc:false) must still track the
// cursor COLUMN here, not show the same goal across the whole line.
const VC_K = {
  kind: "vc",
  start: { line: 881, col: 25 },
  end: { line: 881, col: 26 },
  goal: "0 <= k",
};
const VC_K1 = {
  kind: "vc",
  start: { line: 881, col: 27 },
  end: { line: 881, col: 34 },
  goal: "k <= k + 1 && k + 1 <= len (now m2)",
};
const SPLIT3 = [VC_K1, VC_K]; // order-independent: put K second on purpose
const pickGoal = (col) =>
  S.selectRegion(SPLIT3, { line: 881, col }, { strictVc: false }).region.goal;

check("compact: cursor inside k's span shows k's goal", () => {
  assert.strictEqual(pickGoal(25), "0 <= k");
});

check("compact: cursor inside (k+1)'s span shows (k+1)'s goal", () => {
  assert.strictEqual(pickGoal(30), "k <= k + 1 && k + 1 <= len (now m2)");
});

check("compact: outside both spans -> the column-NEAREST VC, not a fixed pick", () => {
  // far right (on the lambda) is nearer (k+1)
  assert.strictEqual(pickGoal(40), "k <= k + 1 && k + 1 <= len (now m2)");
  // before both is nearer k
  assert.strictEqual(pickGoal(22), "0 <= k");
});

check("compact: a lone VC still claims its whole line (forgiving preserved)", () => {
  const one = [VC_K];
  // cursor far from the tiny span still selects the only VC on the line
  const sel = S.selectRegion(one, { line: 881, col: 60 }, { strictVc: false });
  assert.strictEqual(sel.relation, "inside");
  assert.strictEqual(sel.region.goal, "0 <= k");
});

check("colDist is 0 inside the span, else the gap to the nearer edge", () => {
  const p = (col) => ({ line: 881, col });
  assert.strictEqual(S.colDist(VC_K1, p(30)), 0);
  assert.strictEqual(S.colDist(VC_K1, p(40)), 6); // 40 - 34
  assert.strictEqual(S.colDist(VC_K1, p(22)), 5); // 27 - 22
  assert.strictEqual(S.colDist(VC_K1, { line: 880, col: 30 }) > 1e6, true);
});

Promise.all(pending).then(() => {
  console.log("\n" + passed + " tests passed");
});
