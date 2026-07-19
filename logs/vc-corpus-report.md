# VC corpus wire-up (task #49, bugreport 05) — report

Wires the live consumer's (vox2 refinement-type verifier) verification-condition
corpus into the regression suite, and reports the per-VC core-optimality deltas.

Base trunk: `40e8d7392f1e71d98a0b498b8a399cc87a4cc7ad` (oxsmt).
Manifest (attribution): `compiler.revision` =
`845c60e68eef31445ca85bc8da931ab9606095d5`, `corpus_revision` =
`845c60e68eef31445ca85bc8da931ab9606095d5` (schema v1, reference solver Z3 4.8.5,
reference_core search bound: max 12 facts / 4096 checks).

## What ships

- `tests/solver/vc_corpus_test.ml` — the corpus driver.
- `tests/solver/dune` — new `vc_corpus_test` executable.
- `Makefile` — `vc-corpus-test` target + `VC_CORPUS` knob, wired into `make test`
  and `.PHONY`.
- `tests/vc-corpus/` — tracked fixture copy of the current drop (5 `.smt2` + 5
  `.json` + `manifest.json`).

## Why a dedicated driver (not the CLI)

The shipped `oxsmt_cli` and its test-only SMT-LIB parser have **no
`check-sat-assuming` command**: it falls through to the parser's generic
"unsupported command" arm, so every corpus VC degrades to `unknown`
(`cli-parse-unsupported`). Confirmed by probe: `(set-option
:produce-unsat-cores ...)` and `(get-unsat-core)` parse fine; only
`check-sat-assuming` is rejected. The CLI also emits no core at all, so core
comparison needs the in-process API regardless.

The driver therefore, per VC:
1. `Sexp.parse_many`, strip the `check-sat-assuming` command and capture its
   selector list;
2. parse the remaining decls + assertions into a fresh `Session` and load them
   via the **shared** `Oxsmt_query_loader.assert_all` (the exact path `oxsmt_cli`
   uses — the corpus cannot diverge from the CLI on how a document is asserted);
3. rebuild the selector terms by name (`Env.declare_fun` re-declare is
   idempotent, so the rebuilt Bool const is hash-cons-identical to the one in the
   parsed assertions) and call `Session.check_sat_assuming` — the consumer's
   actual path;
4. **verdict gate**: fail loudly (nonzero exit) on any verdict != the sidecar's
   `expected_verdict`;
5. **core comparison**: deletion-probe the returned core for subset-minimality
   (protocol class (a)) and compare cardinality/set to `reference_core` (class
   (b)); a parse/load failure on an expected-decidable VC is a loud missed
   obligation (class (c)).

Stdlib-only throughout (I3), including a small recursive-descent JSON reader for
the sidecars/manifest (the `smt/` subproject links no JSON library).

## Fixture strategy: tracked copy + glob

Chosen: **commit a tracked copy under `tests/vc-corpus/` and glob it**
(`VC_CORPUS ?= tests/vc-corpus`, overridable to the live `bugreports/corpus`).

Rationale from repo convention: every small, curated, in-`make test` regression
set here is **tracked** (`tests/cases`, `tests/dt-goldens`, `tests/arr-goldens`,
…); only the **large external public corpora** (SMTLIB/SAT) are untracked and
globbed, and those live *outside* `make test` (`smtlib-corpus`, `sat-bench`). The
consumer's `bugreports/` tree is untracked working-tree, so a fresh checkout
lacks it — globbing only the live dir would make the CI gate a silent no-op.
Committing the small VC set keeps `make test` a real gate everywhere.

The target still **globs** the dir (never a hardcoded file list), so append-mostly
future drops are picked up with zero code/Makefile change — refresh by copying new
files into `tests/vc-corpus/` (or run `make vc-corpus-test
VC_CORPUS=bugreports/corpus` against the live dir). An absent dir or a dir with no
`.smt2` is a clean skip (exit 0). Tradeoff: the tracked copy can go stale vs the
live drop; mitigated by the stable-id append-only contract and the override knob.

## Delta table (our core vs reference_core)

All 5 VCs are expected `unsat` and returned `unsat`. Every returned core is
subset-minimal (deletion-probe confirmed) **and set-equal to the Z3
`minimum_cardinality` reference core** — zero cardinality gaps, zero defects.

| id              | verdict | ref class            | our core  | ref core  | delta |
|-----------------|---------|----------------------|-----------|-----------|-------|
| ref-vc-dump-001 | unsat   | minimum_cardinality  | {}        | {}        | = reference (minimality confirmed) |
| ref-vc-dump-002 | unsat   | minimum_cardinality  | {}        | {}        | = reference (minimality confirmed) |
| ref-vc-dump-003 | unsat   | minimum_cardinality  | {sel_5}   | {sel_5}   | = reference (minimality confirmed) |
| ref-vc-seal-001 | unsat   | minimum_cardinality  | {}        | {}        | = reference (minimality confirmed) |
| ref-vc-seal-002 | unsat   | minimum_cardinality  | {sel_1}   | {sel_1}   | = reference (minimality confirmed) |

Summary: 5 VC(s); 0 verdict failures; 0 core defects. The three empty cores
(dump-001/002, seal-001) are the "goal already unsatisfiable without any fact"
shape (the negated goal is self-contradictory: `3>=3`, `1>0`, `1=1` all hold, so
their negations are unsat with no selector needed) — vox2's computed core and the
Z3 reference both agree the fact set is unused, and so do we. dump-003 and
seal-002 each need exactly one guarding selector, matching both references.

## Gates

- `make check-frozen`: 14/14 unchanged (no frozen `.mli` touched).
- `make test`: exit 0 including `vc-corpus-test`.
- Loudness proven: corrupting `ref-vc-dump-003.json`'s `expected_verdict` to
  `sat` made the target exit nonzero (`1 verdict failure(s)`, make Error 1);
  restored byte-identical.
- Absent dir / empty dir: clean skip, exit 0. Live-dir override
  (`VC_CORPUS=…/bugreports/corpus`) runs and passes.

Note on the DEFECT path: `Session.check_sat_assuming` guarantees a subset-minimal
core by contract, so the deletion-probe cannot be made to fire against the current
sound minimizer — but it is a live, reference-independent regression tripwire (a
future minimizer regression that returned a redundant member would fail the suite
loudly). The verdict-flip RED already demonstrates the harness fails loudly.
