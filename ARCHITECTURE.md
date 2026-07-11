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
Representation frozen by ADR-0003. Owner: TASKS.md M0-core (blocked on ADR-0003).

## smt/solver (`oxsmt_solver`)
CDCL(T) engine: trail, two-watched-literal propagation, 1UIP conflict analysis,
clause learning, restarts; online theory integration via the THEORY callback.
Propositional only — sees theories through the callback interface. Owner: M1-cdcl.

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
