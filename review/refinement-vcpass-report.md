# Refinement verification-condition pass

Canonical specification read from
`/j/office/user/jujacobs/pub/vox2/plan.html` (2026-07-16 revision).

## Implementation

- `typing/vox_verify.ml:22-532` adds the finished-typedtree verification pass.
  It maintains a `Vox_vc.Fact_env`, enters immutable pattern and parameter
  binders, instantiates their top-level refinements as facts, consumes retained
  `Trefine` constraints as obligation marks, checks refined call arguments, and
  adds refined call results as facts.  `if` result obligations are checked in
  branch-local environments containing the condition or its negation.
- `typing/vox_verify.ml:90-104,414-473` retains parameter metadata for local
  definitions.  This permits dependent argument substitution and uses a
  recursive definition's final refined arrow type as the induction hypothesis;
  the recursive identifier typedtree itself still carries the typechecker's
  initial approximation.
- `typing/vox_verify.ml:257-275` snapshots the scoped facts into a `Vox_vc.t`,
  rejects an escaped goal, discharges through `Vox_lean`, and raises a located
  `Refinement verification failed` error for `Not_proved`, `Disproved`, or
  `Solver_error`.  Facts that cease to be in scope are filtered by
  `Vox_vc.Fact_env`.
- `typing/typemod.ml:4151,4470-4471` runs verification after typing both
  toplevel phrases and batch implementations.  Toplevel fact and definition
  state is preserved across phrases; batch state is fresh per structure.
- `typing/typecore.ml:6823-6868,9990-10042` routes function return constraints
  through the same refined-annotation marking path as expression constraints.
  `typing/typecore.ml:9797-9891,11986-12056` retains refined function results on
  arrows and binding patterns while keeping ordinary non-refinement typing
  unchanged.  The existing `Tvar`/`Tunivar` escape now fail-closes by retaining
  an obligation mark.
- `typing/typecore.ml:14169-14179` lowers value references in a signature to
  rename-immune `Rsibling` heads.  The positive and negative signature-boundary
  regressions show that equal sibling predicates survive copying while bare or
  unequal predicates remain rigidly rejected.
- `dune:247-250` and `typing/vox_verify.mli:1-11` add the verification module to
  compiler-libs with a deliberately small interface.

## Lean identity hardening

- `typing/vox_lean.ml:260-286` recognizes arithmetic, comparison, and boolean
  builtins only after resolving the path in the VC's typing environment and
  observing the exact `Val_prim.prim_name`.  Source spellings and `Path.last`
  are never primitive identity.  Unknown and user-defined functions remain
  opaque.
- `typing/vox_lean.ml:288-333,532-542` compares free-reference heads by variant
  and uses `Path.same` for path heads.  Distinct same-printed paths receive
  deterministic per-VC names (`VoxRef_0`, `VoxRef_1`, ...), avoiding the former
  stamp-dropping `Path.name` key.
- `typing/vox_lean.ml:296-313` rejects `forall_` and `exists_` with a clear
  fail-closed emission error.  This follows the current plan's quantifier-free
  ruling; no quantifier semantics were added.
- `typing/vox_lean.ml:846-860` prefers the pinned Lean toolchain over an
  unrelated PATH wrapper, while preserving an explicit `VOX_LEAN` override.

The F1 machine-`int` question remains deliberately untouched: emission still
uses the inherited unbounded Lean `Int` operators, pending the separate user
ruling.  The wiring does not add any new integer model.  Division and modulus
(F6) are **opaque**, not rejected: they receive no builtin semantics and hence
normally fail to prove.  `Disproved` retains the review's F5 limitation: the
negated theorem can be vacuous when the facts are inconsistent, so it is a
solver classification rather than a counterexample witness.

## Tests and corpus

- `testsuite/tests/refinement-lean/identity_guards.ml:1-23` proves that a user
  function named `add` receives no builtin meaning while real Stdlib addition
  still discharges.  `standalone_lean.ml:31-33,198-246` now builds a normal
  compiler environment and tests primitive identity, two same-printed but
  distinct paths, deterministic emission, and quantifier rejection.
- `testsuite/tests/refinement-acceptance/{annotation_obligations,binder_facts,
  contract_obligations,recursion_fib,scope_mutation}.ml` is promoted to final VC
  behavior.  Provable obligations accept; false or unavailable-fact obligations
  report verification errors; `scope_fact_dropped` rejects; recursive examples
  use the induction hypothesis.  The rigid-unification anchors are unchanged.
- `testsuite/tests/refinement/{sibling_boundary,sibling_reject}.ml` covers
  positive and negative sibling-reference boundaries.  The persistence
  string-head test remains green.

Verification performed with
`TMPDIR=/usr/local/home/jujacobs/tmp`, the OCaml 5.4 opam tools, and the pinned
Lean 4.31.0:

- `make -s boot-compiler -j8`: passed (including the final clean incremental
  run).
- `refinement`: 11 passed.
- `refinement-lean`: 2 passed (native and bytecode standalone solver paths).
- `refinement-acceptance`: 8 passed.
- `typing-misc`: 57 passed, 1 configuration skip.
- `typing-gadts`: 71 passed.
- `typing-local`: 42 passed.
- `typing-modules`: 51 passed; only the three documented base failures
  `aliases.ml`, `pr7726.ml`, and `pr7787.ml` remain.  Their differences are the
  pre-existing `total`-printing drift and their sources are byte-identical to
  base `91a0306e3e`.
- `parsetree`: 6 passed; only documented base failure
  `source_jane_street.ml` remains, with the same `total`-printing drift.
- Mode-stub markers: 4 before and after this stage.  Mode machinery and marker
  sites are unchanged.

No seal implication, mode integration, or cross-module persistence work was
added in this stage.

## Time accounting

Rough, reconstructed wall-clock accounting for the full stage: about 2h20m
editing and code inspection; about 35m building (`boot-compiler`, approximately
12 incremental cycles, plus one final checkpoint); about 45m in focused and
full affected suites (including final-compiler setup and Lean subprocesses);
about 10m investigating the standalone compiler-libs environment and baseline
test drift; no intentional idle time.
