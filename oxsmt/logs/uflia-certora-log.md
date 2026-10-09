# A13 — Model.value integer widened to Bigint (uflia-certora lane)

Worktree: `worktrees/uflia-certora`, branched off trunk `8e056625e6`.
Change: authorized unfreeze of `smt/core/model.mli` — `Model.value` variant `Int of int`
→ `Int of Bigint.t` + full downstream ripple + `FROZEN.sha256` regenerated to the new
`model.mli` hash + DESIGN.md A13 addendum + RED golden
`tests/certora-red/bigint_model_2p256.smt2`. See DESIGN.md §A13 for the design/rationale.

## Build (finisher-reconstructed; builder log was not present at finish time)

Frozen-surface diff touches 19 tracked files (261 ins / 129 del):
`smt/core/model.{ml,mli}`, `smt/theories/lia/{lia.ml,lia.mli,rational.ml,rational.mli,
lia_adapter.ml}`, `smt/combine/combine.ml`, `smt/interface/{cdclt.ml,array_model_check.ml,
dt_model_check.ml}`, `smt/theories/{dt/dt.ml,arr/arr.ml}`, plus tests
(`combine_test.ml`, `lia_adapter_test.ml`, `prelude_test.ml`, `wiring_test.ml`) and
`DESIGN.md`, `FROZEN.sha256`. Untracked: `tests/certora-red/`.

Key ripple: `Lia.model_bigint` (arb-precision extraction via `Rational.num_bigint`),
`Lia.suggest_branch` branches via `Rational.floor_bigint` + `Context.int_const_big`,
`Combine` folds/compares in `Bigint` (removed `add_guard`/`mul_guard` overflow guards,
no longer degrades a >int63 constant to `None`), `Cdclt.value_of` passes the `Bigint`
through without re-`of_int`. `model_check` (R1 TCB checker) already consumed `Cdclt.VInt`
(Bigint) — unchanged.

## Acceptance battery (a13-finisher, box-local, shared/loaded 64-core box)

Two release-profile classifiers built (assertions=off, euf_self_check=off), stamped by sha256:
- branch (A13):  `f7cd102b4e3ebff27fa7f3bc29ae55db70fc403f3603431927711995d7ca6246`
- trunk (8e056625e6, A13 stashed): `039209f40f2bd99a92a09c460e27f47ee9a21d58ee7a8511e6a252c363e16348`

### 1. 0-flip A/B — QF_UFLIA 659 + corpus-weighted QF_LIA sample 297 = 956 files
Wall 15s/file, `unbounded` effort, background+kill (no `timeout` cmd), branch vs trunk,
per-file verdict compared. Artifacts: `../logs/a13/ab-main.raw`, `ab-main.out`.

    trunk:  511 solved-sat  278 solved-unsat  156 timeout  11 unknown
    branch: 512 solved-sat  278 solved-unsat  166 timeout   0 unknown

- FORBIDDEN sat<->unsat flips: **0** (the required guarantee).
- Regressions (branch lost a definite verdict trunk had): **0**.
- Branch net +1 definite verdict at this wall/load = the Certora conversion
  `65782_..._6_QF_UFLIA.smt2` (unknown -> solved-sat).
- 10 `unknown -> timeout`: the pre-A13 overflow FAST-degrades to unknown; A13 removes the
  overflow exit so those files now genuinely search (arb-precision B&B) and hit the 15s
  wall. Non-definite -> non-definite: neither a flip nor a regression (expected per DESIGN).

QF_UFLIA Certora family (76 files) focused A/B (wall 30s): 0 flips; trunk 2 solved-unsat /
13 unknown / 61 timeout, branch 1 solved-sat / 2 solved-unsat / 73 timeout.

Certora conversion confirmation (13 trunk-`unknown` files, branch UNLOADED, wall 60s):
exactly **2 convert** to R1-checked `solved-sat`:
  - `65782_..._6_QF_UFLIA.smt2` (effort 12296)
  - `65782_..._7_QF_UFLIA.smt2` (effort 327835 — needs the unloaded 60s budget)
The other 11 are the arb-precision B&B search frontier (timeout60; NO overflow, NO flip).
Matches DESIGN "2 convert to sat, the rest a separate perf frontier".
Artifact: `../logs/a13/certora_branch_isolated.txt`.

### 2. Named perf risk — SMPT build_model Int-of-Bigint boxing
8 sat BART-PT exemplars (BART-PT-005/010 RF-*, all solved-sat both binaries), trunk vs
branch INTERLEAVED, 5 reps, per-file median wall + `allocated_words` (OCAMLRUNPARAM
v=0x400, deterministic/load-independent). Artifact: `../logs/a13/perf.out`.

    wall delta (branch vs trunk): -0.1% .. +0.2%  (median ~1.005s both)
    alloc delta (branch vs trunk): -0.2% .. -0.3%  (branch allocates slightly LESS)

Verdict: **NOT a hold-and-tier finding** (threshold was >2-3% median wall regression).
Why boxing did not cost: `Bigint.t` is a boxed record (no immediate small-int form), so it
allocates — but trunk already paid that: `Cdclt.value_of`/`Combine` re-`Bigint.of_int`'d
each native `Model.Int` at every sink. A13 allocates the `Bigint` ONCE (at
`Rational.num_bigint`) and threads it through, and drops the `add_guard`/`mul_guard`
closures — net allocation-neutral-to-favorable.

### 3. Gates (by exit code, at the tip)
- `check-frozen`: **14/14 interfaces match FROZEN.sha256** at the NEW model.mli hash, EXIT 0.
- `make test` (full suite: harness, combine, chrono, wiring-test x2, rational-word, stage0,
  smtlib, lemma, cert-test, checker-test, cert-corpus-gate, driver-equiv, bv gates, dt-sat-gate,
  array-sat-gate, weq-graph, regress, sat-test, satpre): **EXIT 0**
  (`../logs/a13/make-test.exit` = MAKE_TEST_EXIT=0).
- RED golden `tests/certora-red/bigint_model_2p256.smt2`: trunk `unknown` -> branch
  `solved-sat` (forced 2^256 model value; discrimination confirmed).
- `dune build @fmt`: my touched files clean (`wiring_test.ml` promoted). `array_defs.ml`
  carries a PRE-EXISTING (base-tree) fmt divergence, untouched by A13 — left as-is.

## Status: FROZEN for BLOCKING dual review (frozen-surface + TCB-adjacent: codex + fable).
