# Architecture

Module DAG (DESIGN.md §3). Edges point from user to dependency; nothing under
`smt/` depends on anything above stdlib (INVARIANTS.md I3).

```
                     interface/            <- clients (refinement checker)
                    /     |      \
             solver/   euf/     lia/       <- theories are plugins vs THEORY
                    \     |      /
                        core/              <- terms/sorts, depends only on stdlib
        smtlib/  (printer ships; parser test-only; sits alongside, over core)

  tests/ (harness runner, Lean gate encoder, .smt2 cases) lives OUTSIDE smt/,
  consuming the SMT-LIB2 dumps the printer produces.
```

All modules are **skeleton** until their owning task lands.

## smt/core (`oxsmt_core`)
Sorts, hash-consed terms, smart constructors, symbol environments. Owns the
`Term.t`/`Sort.t` representation; smart constructors are the sole construction
path so well-sortedness and hash-consing hold by construction (I1, I2).
Representation frozen by ADR-0003. **Status: implemented** (was skeleton). Public
modules `Iarr`, `Symbol`, `Sort`, `Rank`, `Env`, `Term` (with `Term.Debug.check`),
`Context` (the smart-constructor surface), `Theory_view`; the private type's
construction machinery lives in the library-private `Node` module (dune
`private_modules node iarr_unsafe`) so `Context` is the sole build path. Frozen
`.mli`s: `iarr`, `symbol`, `sort`, `rank`, `env`, `term`, `context`, `theory_view`.
Unit + property tests under `smt/core/test/` (`make core-test`). Owner: TASKS.md
M0-core.

## smt/solver (`oxsmt_solver`)
CDCL(T) engine (MiniSat design, novelty-free): trail, two-watched-literal
propagation, 1UIP conflict analysis with clause learning + local minimization,
VSIDS activity branching, phase saving, Luby restarts, activity-based learned-
clause deletion. Online theory integration via the THEORY callback arrives with
M2+; **the M1 SAT core is propositional only and sees no terms**. Public surface:
`Sat` (int vars/lits, `add_clause`, `solve ?assumptions`, model + failed-
assumption core, stats trio, a zero-cost proof-readiness `trace` hook per learned
clause — I4/§7). Every derived fact is justified (I4); deterministic (I6): no
wall-clock/randomness, count-based schedules. **Status: implemented** (~855 lines
shipped, stdlib-only via `Dynarray`; well under the 1.5k budget). `sat.mli`
freezes at the M1 THEORY freeze. Test-only DIMACS parser + DPLL oracle + bench
runner under `smt/solver/test/` (`make sat-test`, `make sat-bench`). Owner:
TASKS.md M1-cdcl (SAT core); the clausifier is the separate M1 preprocess task.

## smt/theories/euf (`oxsmt_euf`)
EUF congruence closure over a proof-producing union-find (Nieuwenhuis-Oliveras):
e-graph, congruence table, merge queue; every merge explainable (I4). Implements
THEORY. Owner: M2-euf.

## smt/theories/lia (`oxsmt_lia`)
LIA via incremental simplex over rationals + branch-and-bound for integrality
(Dutertre-de Moura); conflicts as infeasible bound sets with Farkas coefficients
(I4). Implements THEORY. Owner: M3-lia.

## smt/interface (`oxsmt_interface`)
Session API: declare, assert, check, push/pop, unsat cores, reasons. Sole client
entry point and the SMT-LIB2 serialization seam; solver never exposes internals
(DESIGN.md §3 boundary 1). Owner: M4-interface.

## smt/smtlib (`oxsmt_smtlib`)
SMT-LIB2 printer (shipped) and parser (test-only, never linked into the
compiler). The interchange format for the oracle and public benchmarks. Owner:
M0-smtlib.

## tests/ (outside smt/)
`tests/harness` runner (.smt2 golden/expect + promote), `tests/gate` Lean encoder
+ certification + content-addressed cache, `tests/cases/*.smt2` corpus. Gate
paths are master-only (AGENTS.md). See `tests/README.md`.
Status: **landed** — harness (M0-harness) and gate (M0-gate) both merged;
`tests/cases/` seeded with 11 cases (harness goldens are `unknown` under the stub
solver until the real solver lands). Not skeleton; the rest of the DAG still is.
