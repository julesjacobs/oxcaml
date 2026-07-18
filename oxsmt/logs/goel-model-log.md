# Goel sat-model reconstruction (task #23, lever L3)

Branch `task/goel-model`, based on trunk `c37fee56d7`.

## Summary

The ~53 structurally-unknown QF_UF files (Goel-hwbench + CLEARSY) do NOT degrade at
`commit_sat` / model reconstruction, as the census framing suggested. They degrade DURING
the solve via `Combine.Incomplete` — specifically the H2 guard `require_bool_args_bound`
(smt/combine/combine.ml). The census's line-number pointer had drifted; the mechanism is
`raw_solve` catching `Combine.Incomplete`, not a withheld model at commit.

Root cause is a genuine (sound) completeness guard, not an over-broad one. A **bare nullary
Bool variable** used only as an uninterpreted-function argument (e.g. `Concat(y$8, y$10)`
with `y$10 : Bool`) is a propositional variable, NOT a theory atom, so EUF never binds it to
true/false — it stays an opaque third Boolean class. If `n>=3` such classes are forced
pairwise-distinct by congruence the instance is pigeonhole-impossible, but the engine can't
see it. The guard refuses to certify Sat in that state (`h(b)≠h(true) ∧ h(b)≠h(false)`
would otherwise be a wrong-Sat), degrading to a sound `unknown`.

`Session.register_bool_terms` already closed the completeness half for APPLIED predicates
`p(x…)` (arity ≥ 1, interned as their own theory atom). It skipped bare (arity 0) Bool
variables — exactly the failing case.

## Diagnosis (source-confirmed, instrumented)

- Env-gated `eprintf` at the three `Cdclt.model` `Degrade` sites: never fired → not a model
  reconstruction problem.
- Env-gated print at `commit_sat` entry: never reached → verdict was Unknown BEFORE commit.
- Env-gated print in `raw_solve`'s `Combine.Incomplete` arm: fired with
  `"Bool leaf / predicate under an uninterpreted function is unbound (buried, no true/false
  binding in EUF)"` on all 3 repros.
- Env-gated print in `require_bool_args_bound`: the unbound term was a nullary `App`
  (`y$10/0`, `y$error/0`, `y$WriteOp/0`) — a bare Bool variable, arity 0, confirming the
  gap. All debug instrumentation removed before the final build.

The 4 CLEARSY residuals (00293/00304/00314/00324) hit a DIFFERENT, harder Incomplete —
`"structured Bool compound as an uninterpreted-function argument"` (they apply a UF to
`(= ...)`, i.e. `(bool (= x y))`). Out of scope for this fix (the leaf bridge names a
nullary leaf; a compound arg would need abstraction). They remain sound-unknown.

## Fix (sound; drives the model checker)

The sat direction is unchanged in its soundness bar: the ONLY Sat exit still goes through
`Model_check.check` (QF_UF) / the theory self-checkers. The fix makes the search DECIDE the
buried Bool variable rather than reconstruct anything after the fact.

- `smt/interface/cdclt.ml` / `.mli`: new `bind_bool_var_atom t term v` — registers `term`
  (a bare Bool variable) as an EUF `K_bool` theory atom bound to the **already-allocated**
  propositional SAT var `v` (from `prop_to_var`/`bool_consts`). Reusing `v` (rather than
  minting a fresh var via `intern`) keeps ONE SAT variable per term: the model reads its
  value from `bool_consts`, EUF binds it from the same var, so the propositional skeleton and
  EUF can never disagree. `Sat.new_var` inserts every var on the decision heap, so it is
  decided even when it occurs in no clause; `on_assign` then asserts it to EUF. Idempotent.
- `smt/interface/session.ml`: extracted `prop_var_of` (the propositional-var path, shared
  with `register_bool_terms`); rewrote `register_bool_terms` to track UF-argument position
  (`~under_uf`, set when descending into an applied `App`'s arguments) and, for a bare
  nullary Bool App in that position, bind it via `bind_bool_var_atom (prop_var_of …)`.

Soundness notes:
- One SAT var per term (by construction); no propositional/EUF divergence.
- Binding only ADDS the always-true fact "b is true or false" to the search → cannot create
  a false unsat and cannot fabricate a sat the guard was preventing (a genuinely-unbindable
  pigeonhole now DECIDES b and congruence refutes it → correct unsat).
- Structured Bool compounds under a UF arg still degrade to sound unknown (unchanged).

## Tests (RED-verified against trunk c37fee56d7)

- `tests/cases/uf_buried_bool_arg_sat.smt2` (+ `.expected`, `.model`): a bare Bool var used
  only as a UF arg, trivially SAT. Trunk: `unknown`; fixed: `sat` with an eval-self-checked
  model (`p false`). z3: sat.
- `tests/cases/uf_buried_bool_arg_pigeonhole_unsat.smt2` (+ `.expected`): the discriminating
  soundness case `(distinct (h p) (h true) (h false))`. Trunk: `unknown`; fixed: `unsat`
  (the case the H2 guard protected — now correctly DECIDED, not degraded). z3: unsat.
- Existing goldens that PINNED the old gap, updated to the correct verdicts (comments
  rewritten to note the gap is closed):
  - `tests/harness/fixtures/uf_buried_bool_unknown.smt2` → renamed `..._unsat.smt2`
    (`unknown` → `unsat`).
  - `tests/dt-goldens/dt_box_bool_pigeonhole_unsat.smt2` (`unknown` → `unsat`; DT
    constructor-arg variant of the same pigeonhole — bonus fix).
  - `tests/solver/wiring_test.ml`: 3 buried-Bool cases (`¬b ∧ h(b)≠h(false)` → unsat,
    `b ∧ h(b)≠h(false)` → sat, buried-H2 → unsat), comment block rewritten. Structured
    compound case unchanged (still unknown).

## Measurement (before/after over the 53 census structural unknowns)

Baseline trunk c37fee56d7: all 53 return `unknown` (structural, sub-second, budget left).

After fix (2s wall, `timeout -s KILL`; z3 4.8.5 cross-check):

| verdict | count |
|---|---|
| sat (was unknown) | 39 |
| unsat (was unknown) | 10 |
| still unknown (CLEARSY structured-compound) | 4 |
| **resolved** | **49 / 53** |

- **0 label mismatches** vs `:status`.
- **0 disagreements** vs z3 4.8.5 (every resolved file matches z3's sat/unsat; the 4
  residuals stay unknown, never a wrong verdict).
- The 49 resolved exactly matches the census taxonomy (49 Goel-hwbench).

## Gates (by exit code)

- `make check-frozen`: exit 0 (14 frozen interfaces match; none of the modified files are
  frozen).
- `make test`: exit 0 (harness 69/69, combine-test, euf-test, wiring-test 209/209,
  chrono, satpre).
- `make dt-sat-gate`: exit 0.
- `make array-sat-gate`: exit 0.
- `dune build @fmt`: my 4 source files are clean (a pre-existing `smt/core/array_defs.ml`
  fmt drift, owned by another lane, was left untouched).

## Files changed

- smt/interface/cdclt.ml, smt/interface/cdclt.mli  (new `bind_bool_var_atom`)
- smt/interface/session.ml  (`prop_var_of`, `register_bool_terms` UF-arg binding)
- tests/cases/uf_buried_bool_arg_sat.{smt2,smt2.expected,model}  (new)
- tests/cases/uf_buried_bool_arg_pigeonhole_unsat.{smt2,smt2.expected}  (new)
- tests/harness/fixtures/uf_buried_bool_unknown.* → uf_buried_bool_unsat.*  (renamed, golden updated)
- tests/dt-goldens/dt_box_bool_pigeonhole_unsat.{smt2,smt2.expected}  (golden updated)
- tests/solver/wiring_test.ml  (3 buried-Bool expectations updated)
