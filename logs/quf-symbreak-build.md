# QF_UF online symmetry breaking — build + freeze report (task #25)

Branch: `task/quf-symbreak` off trunk `c37fee56d7`. Env-gated dark: `OXSMT_SYMBREAK`
(default OFF, byte-identical to trunk). Builds on the offline verdict in
`logs/quf-symmetry-experiment.md` (§6) and its three ruled conditions.

## What shipped (dark)

`Presolve.symmetry_break : Env.reserved_cap -> Env.t -> Context.t -> Term.t list ->
Term.t list` (smt/interface/presolve.ml/.mli), wired into `Session.assert_presolved`
(smt/interface/session.ml) as extra top-level constraints — internalized alongside the
assertions, NOT recorded in `t.asserted` (the R1 self-check set), exactly like Pass A.

- **Detector (general/structural, no family logic).** Harvest same-sort constants
  (non-Bool) and application cells from the term DAG; refine constants by a cheap
  occurrence signature (parent-symbol + arg position); within each bucket build the
  confirmed-transposition graph and take connected components as interchangeable classes.
  A transposition is CONFIRMED EXACTLY: rebuild every conjunct under the swap through
  `ctx` (whose smart constructors AC-normalize `and`/`or` and tag-order `eq`, so the
  hash-cons tag is the canonical form) and compare the tag multiset to the original. The
  exact check is the soundness guarantee; the signature refinement is only a cost filter
  (condition c: cheap detector + exact check retained as the oracle).
- **Emission = full-action generator-based LEX-LEADER only (condition a).** For each class
  and each adjacent transposition g, require `A ⪯ g(A)` over the equality atoms
  `(= cell c_v)`, encoded with an **O(n) prefix-equal chain of fresh reserved
  `.oxsmt.sym.*` Bool aux vars**. Value precedence is NOT implemented anywhere in the
  product (it is proven unsound for this index+value symmetry). Sound: `A ⪯ g(A)` keeps
  ≥1 representative per orbit ⇒ SAT-preserving; adding constraints ⇒ UNSAT-preserving.
- **Size cap (sat-safe, condition b).** Emission skipped for classes of size ≥ 6
  (`symbreak_emit_max`), where the offline A/B showed size-6/7 classes regress the
  satisfiable instances they touch.
- **cert-OFF gating.** `symbreak_enabled t = flag && not t.cert_active`. A lex-leader
  clause is not resolution-derivable, so the pass SELF-DISABLES whenever a certificate
  trace is installed (same discipline as Pass A / projection). Cert corpus runs are a
  soundness gate, not a solve-rate target, so this is free. Certificate behavior:
  **self-disable under cert** (stated + implemented).
- Neutral-abort (returns `[]`) on any hard budget; pure and deterministic (tag order).

### Encoding note (the load-bearing fix)

A first cut built the prefix-equal chain as shared hash-consed `and`-terms. Because
`Context.and_` AC-FLATTENS, each prefix became a fresh flat n-ary `And` and Tseitin
re-encoded all of them → O(n²) clauses. Sampled A/B was a disaster (qg5-sat 20/40
regressed, qg5-unsat 0/37 converted — the clause blowup killed wall-time everywhere).
Switching to explicit reserved Bool aux vars (opaque, so `and(aux, atom)` is a 2-input
node that does not re-flatten) restored the O(n) encoding: same sample went to qg5-sat
0/40 regressed, qg5-unsat 33/35 converted, 0 mismatches.

## Gates (by EXIT CODE)

| gate | result |
|---|---|
| `make test` (OFF, bit-identical) | **exit 0** |
| `make symbreak-test` (new, OXSMT_SYMBREAK=1) | **exit 0** (5 checks, 0 failures) |
| `make check-frozen` | **exit 0** (14 interfaces match; no frozen .mli touched) |
| `dune build @fmt` on touched files | clean (array_defs.ml is pre-existing trunk drift, reverted, not mine) |

### Tests (RED-verified — tests/solver/symbreak_test.ml, `make symbreak-test`)

1. **detector fires** on a symmetric quasigroup (non-empty break).
2. **detector rejects** a broken (cyclic-only) symmetry → `[]`. RED-verified: forcing the
   exact check `is_symmetry := true` makes this FAIL (emits 30 constraints on the
   asymmetric input).
3. **SAT-preservation** on a satisfiable symmetric quasigroup (product path → Sat).
   RED-verified: an unsound emit (`false` when a class fires) makes this FAIL
   ("SAT-preservation VIOLATED").
4. **value-precedence MUTANT discrimination**: a test-only value-precedence function flips
   the SAME instance SAT→UNSAT (solver-checked). Proves test 3 would go RED if the product
   ever regressed to value precedence.
5. **UNSAT golden**: an order-2 anti-idempotent Latin square (genuinely UNSAT, e0/e1
   interchangeable) stays UNSAT under the break.

## Decisive OFF/ON A/B — full QF_UF (7,503 files, 2 s wall)

Same freshly-built `--profile release` binary, OFF then ON back-to-back per file.
**Load conditions: shared 64-core box, P=32, load avg ~24-26** (not a quiesced lock-box
run — so the MAGNITUDE is a load-affected estimate; a quiesced tandcperf sweep is the
promotable follow-up. The SIGN and the soundness result are load-independent.)

- **Soundness: 0 OFF/ON definite disagreements. 0 ON-verdict-vs-`:status` contradictions
  across all 7,503 (every file is labeled). 0 OFF-vs-label. z3 4.8.5 spot-check of 8
  conversions: 8/8 agree.**
- **Net: OFF solved 5,903 → ON solved 6,872 = +969 (78.7% → 91.6% at 2 s).**
- 986 conversions (978 QG-classification, 3 NEQ, 2 SEQ, 2 PEQ, 1 Goel-hwbench);
  17 regressions (10 Goel-hwbench sat, 7 near-2s-boundary timeouts across families —
  consistent with load amplification).

This substantially exceeds the offline lower-bound estimate (~+400): the online
tag-ordered atom sequence + tight aux-var encoding is a stronger sound break than the
offline python lower bound. z3 QF_UF is ~99.2% for context; this closes ~⅔ of the gap
with a sound, dark, size-capped lever.

## Status

Built, gated, A/B-clean, dark. **NOT landed** (per charter). Frozen at the **tip of
`task/quf-symbreak`** (off trunk `c37fee56d7`); awaiting dual review. Follow-up: quiesced
lock-box A/B for a promotable magnitude before any default flip.
