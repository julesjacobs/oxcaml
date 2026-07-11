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

## smt/preprocess (`oxsmt_preprocess`)
Desugaring passes (ADR-0003 §5 pipeline invariants) + Tseitin clausifier, over
`core` only (stdlib-only, I3). `Preprocess`: `ite_removal` (lift non-Bool `Ite`
to a fresh constant + guarded equalities), `div_mod_elimination` (euclidean
`q`/`r` for nonzero-constant divisors), a minimal `simplify`, and `run`
(`div_mod` then `ite`, whose output satisfies `Term.Debug.check ~mode:Pipeline`);
all thread the session `Context`, declaring fresh symbols in a reserved
`.oxsmt.` namespace. `Cnf`: plain-Tseitin clausification of the boolean skeleton
into **abstract** CNF (its own `Lit`/`Clause` over its own var ids + an
atom↔var map), deterministic by term-tag order (I6). Deliberately does **not**
depend on `smt/solver`; the abstract CNF is mapped to the SAT core's literals at
M1-end wiring. Unit + property tests (`make preprocess-test`): brute-force
equivalence-by-evaluation for the passes, brute-force original⇔CNF for the
clausifier. Owner: TASKS.md M1-preprocess.

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
Session API (`Session`): the sole client entry point — declare sorts/funs/consts,
`assert_term`, `check_sat`, `push`/`pop`, `get_model`, `stats`; solver never
exposes internals (DESIGN.md §3 boundary 1). **Status: M1-wiring landed** (was
skeleton). Shipped, stdlib-only over `oxsmt_core` + `oxsmt_preprocess` +
`oxsmt_solver` (I3) — it never links the test-only SMT-LIB parser. Bundles one
Env+Context, threads every asserted term through preprocessing → clausification →
the CDCL core, sharing one SAT variable per hash-consed atom; push/pop is
selector-literal retraction (frame clauses guarded by a selector `check_sat`
assumes). **THE SOUNDNESS RULE** (documented in `session.mli`, a code comment, and
`wiring_test`): with any theory atom present (`Le`, non-Bool `Eq`, applied
predicate) a propositional `Sat` downgrades to `Unknown` (the SAT core cannot see
theory inconsistency); propositional `Unsat` stays sound; pure-Boolean formulas get
real sat/unsat; `Overflow`/`Unsupported` → `Unknown` (I8). Unsat cores / reasons and
the SMT-LIB serialization seam arrive with M4. Owner: M1-wiring (was M4-interface).

## smt/smtlib (`oxsmt_smtlib` printer; `oxsmt_smtlib_parser` test-only)
SMT-LIB2 interchange, the format for the oracle and public benchmarks. **Status:
implemented** (was skeleton). Split into two libraries so the parser can never be
linked into the compiler (DESIGN.md §3):
- `oxsmt_smtlib` (`smt/smtlib/`, SHIPS) — the printer over `Oxsmt_core`
  (stdlib-only, I3). `Printer.print_session` renders an `Env` + ordered assertions
  (+ optional `:status`) as a complete `QF_UFLIA` script: declarations in first-use
  order (all sorts before all funs), one `(assert …)` per assertion, `(check-sat)`.
  Deterministic (I6). Rendering choices + SMT-LIB symbol quoting (`|…|`, refusing
  names with `|`/`\`) are in `printer.mli`.
- `oxsmt_smtlib_parser` (`smt/smtlib/parser/`, TEST-ONLY) — a SEPARATE library
  reading the subset back into frozen-API terms through a `Context`; distinguishes
  `Malformed` from `Unsupported`. `define-fun` macros are expanded by
  capture-avoiding substitution at use sites (recursion rejected). Shipped code
  depends on `oxsmt_smtlib`, never on this library — the mechanical boundary
  DESIGN.md §3 mandates.
Tests (`smt/smtlib/test/`, `make smtlib-test` / `make smtlib-corpus`): print↔parse
round-trips + a parse-only corpus smoke. Owner: M0-smtlib.

## tests/ (outside smt/)
`tests/harness` runner (.smt2 golden/expect + promote), `tests/gate` Lean encoder
+ certification + content-addressed cache, `tests/cases/*.smt2` corpus, and
`tests/solver` — the real-solver CLI (`oxsmt_cli`) + wiring unit tests
(`wiring_test`, `make wiring-test`). The CLI drives `Session` from a `.smt2` file
via the test-only parser (so it lives here, not in shipped `smt/`); it is the
default harness `SOLVER` (M1-wiring). Gate paths are master-only (AGENTS.md). See
`tests/README.md`.
Status: **landed** — harness (M0-harness) and gate (M0-gate) merged; the real
solver is wired (M1-wiring), so pure-Boolean goldens are now real sat/unsat and
cases with theory atoms are `unknown` under THE SOUNDNESS RULE. `tests/cases/`
holds 21 cases, all gate-certified — including `degrade_*.smt2` degradation
honeypots (propositionally-sat but theory-unsat, `:status unsat`, golden
`unknown`) that turn a regression to `sat` into a red label-check failure.
