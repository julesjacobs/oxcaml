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

---

## Fix round (post-review) + rebase

Both review legs (fable APPROVE-dark; codex SAFE-dark / three verified defects) drove a
fix stack, now rebased onto trunk `a2423ec5fd`. Tip of `task/quf-symbreak` after rebase.

- **F1 (wrong-unsat, non-monotonic incremental).** Symmetry breaking is not monotonic: an
  assertion AFTER the emission can break the detected symmetry while the (permanent) lex
  clauses remain → a SAT model wrongly refuted. Fix: the lex clauses are guarded by a fresh
  **activation selector** (`assert_clausified ~sel`); `check_sat` assumes it POSITIVE while
  `sym_sel = Some _`, and every post-emission assertion entry point (`assert_term`,
  a further `assert_presolved`, `push`, `assert_lemma`) clears it to `None`. The selector
  occurs only negatively (a pure literal), so once unassumed the clauses are trivially
  satisfiable — sound retraction without touching the permanent clause DB. RED-verified:
  `(= (op e0 e1) e0)` is base-SAT (z3) but the stale clauses refute it with the retraction
  disabled.
- **F2 (wrong-unsat, aux-var name reuse).** The `.oxsmt.sym.*` counter was per-call; a
  second emission reused `.oxsmt.sym.0` with a conflicting definition (idempotent
  `declare_reserved`). Fix: a per-session monotone `~counter` ref. RED-verified: two calls
  sharing a counter now emit disjoint names (else overlap=50/50).
- **F3 (crash).** Sort grouping used `Sort.hash` without `Sort.equal` → a hash collision
  pairs cross-sort constants → `Context.eq` raises `Term.Sort_error` escaping the firewall.
  Fix: group by `Sort.equal`; the `sym_extra` firewall is now fail-closed on ANY exception
  (→ no breaking, never a crash). Test: a two-sort input is handled with no cross-sort pair.
- **F4.** Corrected the `.mli`/impl comments that still described the abandoned
  shared-`and`-term encoding; they now match the shipped O(n) reserved-aux-var chain.

### Gates after rebase (by EXIT CODE)

| gate | result |
|---|---|
| `make test` (OFF, bit-identical) | exit 0 |
| `make symbreak-test` (now 8 checks: adds F1 incremental, F2 counter, F3 two-sort) | exit 0 |
| `make check-frozen` | exit 0 |
| touched-file fmt | clean |

Label recheck (stratified 46-file QG sample, OFF vs ON): 0 ON-verdict-vs-`:status`
mismatches; 6 conversions — the batch-path win is preserved (F1 retraction only fires on
post-emission assertions, which the batch CLI never makes).

The full-7503 ON/OFF A/B numbers above (net +969, 0 disagreements) stand for the batch
path, which the fix round does not alter; a fresh quiesced lock-box A/B remains the
promotable-magnitude follow-up. NOT landed; narrow dual confirm pending.

### Narrow confirm bounce (fable)

One required one-line fix + one non-blocking defensive guard, committed on top of the
fix-round tip:
- **Required (F3 firewall):** the `sym_extra` catch-all swallowed `Out_of_memory` /
  `Stack_overflow`; `Stack_overflow` is REACHABLE (`symmetry_break`'s DAG rebuild is
  non-tail-recursive, so a deep term can overflow before the step budget). Added
  `| exception ((Out_of_memory | Stack_overflow) as e) -> raise e` before the catch-all,
  matching `raw_solve`'s posture — those propagate, never a silent no-op.
- **Non-blocking defensive:** the F1 pop-soundness relied on the implicit batch-once
  base-frame contract (lex clauses guarded by `sym_sel`, not a frame selector). Added
  `deactivate_symbreak` to `pop` (clears `sym_sel` after any pop) + a CONTRACT comment at
  the emission site, so F1 soundness no longer depends on that contract. No separate RED
  test: the offending scenario (emission inside a pushed frame) is contract-unreachable in
  the shipped path, so a test would not discriminate.

Re-gated by exit code: `make test` (OFF) 0, `make symbreak-test` 0 (8 checks), `check-frozen`
0. New tip below.

### Second confirm bounce (codex) — R2 structural restriction + riders

Codex found two more F1 wrong-unsat paths beyond the incremental one:
- **B1:** emission INSIDE a pushed frame (`push → assert_presolved → pop`) — the lex clauses
  are guarded by `sym_sel`, not the frame selector, so they survive the `pop` that retracts
  the assertions making the batch symmetric.
- **B2:** during-solve lemma instances (`assert_instance_at_frame`) extend the formula, and
  `check_sat` builds its assumption list once, so an emission could not be retracted mid-solve.

**Fix (codex's simpler-restriction route):** `symmetry_break` EMITS ONLY when there are no
open frames AND no lemmas registered — an explicit `at_base_no_lemmas` check at the emission
site (`(match t.frames with [_] -> true | _ -> false) && not t.lemmas_registered`). B1 becomes
impossible (no frame to pop under an emission); B2 impossible (no lemmas at emission, and
`assert_lemma` sets `lemmas_registered` + deactivates any prior emission). The existing
per-assertion `deactivate_symbreak` still handles the base-frame post-emission incremental
case. Defensive belt kept: `deactivate_symbreak` on `pop` and `assert_instance_at_frame`, with
comments that the restriction makes them unreachable.

Three riders in the same commit:
1. `failed_assumptions` filters out the internal `sym_sel` selector — a caller's core never
   sees the private aux var.
2. **F3 firewall FINAL FORM** (subsumes fable's re-raise ask): the `sym_extra` catch now
   matches ONLY `(Term.Sort_error _ | Term.Overflow | Term.Unsupported _) -> []`; everything
   else — `Out_of_memory`, `Stack_overflow`, `Sys.Break`, any unexpected soundness raise —
   PROPAGATES.
3. F3 test now exercises a REAL `Sort.hash` collision: an uninterpreted sort whose symbol
   hashes to `h ≡ 1 (mod 7), h ≥ 8` collides with `BitVec ((3h-3)/7)` (e.g. `h=8 ↔ BitVec 3`,
   both hash 26); with empty occurrence signatures the collided constants share one bucket and
   a cross-sort pair drives `Context.eq` to `Sort_error` under the buggy grouping.

RED-verified (make symbreak-test, now 11 checks): B1 (emits under frame with the restriction
off), B2 (emits with a lemma registered when the restriction is off), F3-collision (raises
`Sort_error` with `Sort.hash` grouping restored). Gates by exit code: `make test` (OFF) /
`make symbreak-test` / `check-frozen` all 0. A test-only accessor `symbreak_active_for_test`
was added to `session.mli` to check the restriction directly. New tip below.

### Post-accept addition (test-only)

Deviation on the B1 test (accessor-based RED vs verdict-level repro) ACCEPTED. Added the
cheap verdict-level property guard the lead asked for: `test_b1_verdict_guard` runs codex's
exact intermediate-frame sequence (push; symmetric `assert_presolved`; pop; `check_sat`) on
the REAL code and asserts the verdict is not a wrong-UNSAT. Not a RED test — it catches a
future change that regresses BOTH the restriction and the pop-deactivation at once (the only
dangerous case, which no single-mutation RED test can express). `make symbreak-test` now 12
checks, 0 failures. Test-only commit on top of the R2 tip.

### Third confirm bounce (codex) — free-constants-only + formula-is-exactly-the-batch

Codex found two more wrong-unsat paths (guard-completion, not redesign):
- **B3 [shipped path]:** the detector harvested EVERY nullary non-Bool App, including
  DATATYPE CONSTRUCTORS — theory-interpreted (distinctness/exhaustiveness) invisibly to the
  syntactic tag-multiset check. Trigger: `D=A|B, x:D, {A≠x, f(A,x)≠B, f(x,A)≠B}` is
  syntactically swap(A,x)-symmetric, OFF sat / ON unsat, on any datatype query.
- **B4 [public API]:** detection sees only the batch `terms`, but `assert_presolved` can
  follow a prior `assert_term` — a symmetry of the batch need not be one of `prior ∧ batch`.

**Fixes:**
- B3: candidates are now genuinely FREE constants only — a POSITIVE predicate
  `is_free_const` = nullary App of a NON-reserved symbol whose sort is `Sort.Uninterpreted`.
  Excludes datatype constructors (Datatype sort), numerals (`Int_const`), BitVec/Int/Array
  constants, reserved skolems; future interpreted sorts excluded by default. The QG wins are
  uninterpreted-sort constants, untouched.
- B4: the emission guard is now `formula_is_exactly_this_batch` = base frame ∧ no lemmas ∧
  NO prior assertions (`t.asserted` empty before this batch). When it holds, a symmetry of
  `terms` is a symmetry of the whole formula — the entire soundness story in one predicate.
  This also discharges fable's standing batch-once-contract note.
- Minor: `failed_assumptions` now filters by `sym_sel_in_core` (the selector CAPTURED at
  solve time in `check_sat`), not the live `sym_sel` — a later assertion clears `sym_sel`
  while the SAT core still holds the selector, so a read-time filter on the live value would
  leak it. Captured, not read-time-keyed on mutable state.

RED-verified (make symbreak-test, now 14 checks): B3 (detector emits over datatype
constructors when the free-constant restriction is reverted to "any non-Bool"), B4
(wrong-UNSAT when the no-prior-assertions guard is dropped). Gates by exit code: `make test`
(OFF) / `make symbreak-test` (14) / `check-frozen` all 0. Batch-path win intact (label recheck
42 QG files, 0 mismatch, 6 conversions). New tip below.

### Fourth confirm (codex) — set_datatypes install-door validator (pre-existing gap)

Codex confirmed B3/B4/minor sound on every well-formed and shipped path; the remaining
bounce is a PRE-EXISTING gap this lane surfaced: `set_datatypes` (session.ml) installed the
datatype registry with ZERO validation, unlike `set_arrays` (which runs
`Array_defs.validate_ranks` as the install-door defense — third instance of that pattern
after the arrays arity-guard incident). A hand-built registry marking an uninterpreted-sort
constant as a constructor slips `is_free_const` AND can drive other DT wrong-verdicts
independent of symmetry.

**Fix (a), the general one:** added `Datatype_defs.validate_ranks` (mirrors
`Array_defs.validate_ranks`) and call it in `set_datatypes`. Every registered constructor /
selector / tester must carry, in the env, its role's canonical datatype rank (constructor
returns the datatype sort with the field sorts as domain; tester `(dt) -> Bool`; selector
`(dt) -> field`); raises `Invalid_argument` fail-closed on a disagreeing or missing rank.
This keeps `is_free_const`'s soundness BY CONSTRUCTION and closes the forged-registry class
for ALL DT consumers, not just symmetry. (Belt (b) — `is_free_const` also checking registry
non-membership — was NOT taken: it is not one line for `symmetry_break` (would need the
registry threaded in), and the install door already closes the class.)

RED-verified (make symbreak-test, now 15 checks): a forged registry marking an
uninterpreted-sort constant `A` as a constructor is REJECTED at `set_datatypes` (RED —
accepted — with the validator removed). Well-formed registries install cleanly: `dt-sat-gate`
(18 checks) and the DT goldens in `make test` all pass. Gates by exit code: `make test`
(OFF) / `make symbreak-test` (15) / `check-frozen` (14 intact — `datatype_defs.mli` is
deliberately unfrozen) / `dt-sat-gate` all 0. New tip below.
