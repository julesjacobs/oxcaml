# Architecture

Module DAG (DESIGN.md §3). Edges point from user to dependency; nothing under
`smt/` depends on anything above stdlib (INVARIANTS.md I3).

```
                     interface/            <- clients (refinement checker)
                    /     |      \
             solver/   euf/     lia/       <- theories are plugins vs THEORY
                    \     |      /
                        core/              <- terms/sorts, depends only on stdlib
        smtlib/  (printer ships; parser test-only; over core + lexical)
        lexical/ (the one SMT-LIB lexer; stdlib-only; printer + parser + gate link it)

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
`Context` (the smart-constructor surface), `Theory_view`; plus the ADR-0005 THEORY
vocabulary `Atom`, `Lit` (packed literal currency), `Explanation` (premise-set +
`Rule_tag`), `Theory` (the `THEORY` module type + `effort`/`check_result`), and
`Model` (candidate assignment). The private type's construction machinery lives in
the library-private `Node` module (dune `private_modules node iarr_unsafe`;
`explanation`/`theory` are `modules_without_implementation` — pure signatures) so
`Context` is the sole build path. Frozen `.mli`s (12, `FROZEN.sha256`): ADR-0003's
`iarr`/`symbol`/`sort`/`term`/`context` (5) plus ADR-0005 Tranche-A's `env`, `rank`,
`theory_view`, `atom`, `lit`, `explanation`, `theory` (7). `model.mli` (Tranche B, M2)
and `smt/solver/sat.mli` (Tranche C, M4) freeze later — see
`decisions/adr-0005-freeze-plan.md`. Unit + property tests under `smt/core/test/`
(`make core-test`). Owner: TASKS.md M0-core, M1-brand-checkpoint (ADR-0005).

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
Proof-producing congruence closure (Nieuwenhuis-Oliveras): union-find over e-node
ids (union-by-size, no path compression on the proof forest) + a separate
explanation forest (original union edges, never rewritten) + a flat congruence
table keyed on (symbol, argument-class ids) + a pending merge queue; every merge is
explainable as a premise subset (I4), self-checked in debug/test by replay into a
fresh independent naive closure (DESIGN §7). Only `App` is congruence-closed; every
non-`App` subterm is an opaque leaf (the LIA/Nelson-Oppen sharing seam, ADR-0003
dispatch split). Backtracking is a trail with level-granular push/pop (registration
truncated on pop). The public `euf.mli` (`module Euf`) is the ENGINE, parametric
over an opaque premise token `'p`; it is adapter-facing but freeze-agnostic (depends
on `core` only, NOT on `solver`). The ADR-0005 THEORY adapter (mapping `Atom`/`Lit`
→ `'p`) is a thin later layer, not yet built. Tests: `make euf-test`. Owner: M2-euf.

## smt/theories/lia (`oxsmt_lia`)
LIA via incremental simplex over rationals + branch-and-bound for integrality
(Dutertre-de Moura); conflicts as infeasible bound sets with Farkas coefficients
(I4). **Status: implemented (algorithm-first, M3-lia).** Stdlib-only over
`oxsmt_core`. Public submodules: `Rational` (exact overflow-guarded ℚ, raises
`Rational.Overflow` before wrapping — the LIA analogue of I8; DdM coefficient
growth makes this a known native-int incompleteness ceiling that degrades to
`unknown`, counted via `Lia.overflow_count`, until the post-M4 core-bignum row),
`Delta`
(δ-rationals `a + b·δ` for strict bounds), `Simplex` (the DdM two-layer tableau:
`new_problem_var`/`new_slack`, `assert_upper`/`assert_lower`, Bland-rule `check`,
backtrackable `push`/`pop` restoring bounds only, and Farkas conflicts whose
certificate is **self-checked at production** — `Farkas_error` if the
multiplier-weighted half-planes don't cancel to a strictly positive constant),
and `Lia` (the adapter-facing decision procedure over frozen-core `Le`/`Eq`
atoms, parameterized by an opaque `'tok` premise token: `assert_atom` with the
exact ℤ complement of a negated `Le`, `check` for rational feasibility,
`solve_integer` branch-and-bound with a split budget → `Int_unknown`,
`suggest_branch` mirroring the ADR-0005 Split, `register_atom`/`propagate` for
bound propagation, and integer model extraction). `Lia_adapter` (M4) binds `Lia`
to the frozen `Theory.THEORY`: `'tok = Lit.t`; `check Propagate` → rational
`Conflict` (`Lia_farkas`) / bound `Propagations` (`Lia_bound`); `check Final` →
`Sat` (integral) / `Split [x≤⌊v⌋; x≥⌊v⌋+1]` (delegating integer branching to
CDCL(T), CONTRACT-SPLIT) / `Conflict`; lazy `explain` from a push/pop-frame-scoped
premise cache (precedence-valid, CONTRACT-EX); `model` reads `Lia.model` as
`Model.Int` bindings. CONTRACT-POISON: an engine `Rational.Overflow` propagates out
of the THEORY op (engine degrades to `unknown`, I8), counted by
`overflows_to_unknown`; reuse of a bricked instance raises `Lia.Poisoned` — never a
verdict. Determinism (I6): variables numbered in atom-arrival order, Bland's rule,
branch by lowest `Term` tag. An escaped overflow leaves the tableau mid-pivot, so the
instance is bricked (`is_poisoned`): every later public entry raises
`Lia.Poisoned` rather than return a verdict from corrupt state (review item 10) —
discard and rebuild. All atom/equality/branch integer arithmetic routes through
the guarded `Rational` ops (raise → poison → unknown, never a silent wrap —
codex L2/L4/L5); `new_slack` sums duplicate-var coefficients (codex L1); `check`
detects an empty bound interval (asserted lower > upper) structurally rather than
via a cached conflict, so an earlier-scope contradiction can't be lost to a later
assert + pop (codex L3/R1). Unit + property tests under `smt/theories/lia/test/`
(`make lia-test`, `make lia-adapter-test`); the codex L1/L3-R1/poison faults are
guarded by registry mutants. Owner: TASKS.md M3-lia / M4-adapters.

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

## smt/ematch (`oxsmt_ematch`)
Stage-2 quantifier instantiation for the lemma tier (ADR-0012), stdlib-only over
`oxsmt_core` plus a read-only EUF query view. **Tranche 1 (#101):** `Qvar` placeholder
(mint/gate walk under the reserved `.oxsmt.qvar.*` namespace), `Instance` capture-free
substitution rebuild, `Lemma` record (incl. `frame : Sat.var`), `Manager` store with
frame-scoped liveness + active-clause dedup + a manual seed queue + `on_pop`. **Tranche 2
(#135):** `Egraph_view` — a genuinely non-registering read-only e-graph query surface
(`app_terms_by_symbol`/`find_class_opt`/`equal_if_registered`/`class_members`, all
deterministic tag-ordered, never mutating; ADR-0012 §5 L2) — and `Matcher`, a
deterministic backtracking E-matcher (uninterpreted-symbol triggers, per-step budget
debit; §5 L3) driving `Manager.round`'s transactional (dedup + seed) rollback. Matching is
**structural** against the persistent registered-term e-graph end-to-end at the current
outer-loop locus; congruence-modulo matching is coded and unit-covered but dormant until
the in-search O2 locus (ADR-0012 §5 erratum). Consumed by `Session` (`assert_lemma`/
`instantiate`); exercised by `make lemma-test` (lemma_honeypot + crit_repro + matcher). No
frozen `.mli`. Owner: lemma-tranche builders.

## smt/certificate (`oxsmt_certificate`)
Certificate event recorder (ADR-0013, M5 step 1): consumes the frozen `sat.mli` Tranche-C
emission hooks to record the Unsat-proof event stream (the four Unsat exits + E3
`Theory_prop` materialization + ordered-RUP antecedents). Emission is **trace-gated** — an
untraced solve is bit-identical (verdict/model/counters) to the pre-wiring core — so the
recorder is off the shipping hot path. **Step 2 (#153, IMPLEMENTED):** `checker.{ml,mli}` is
a native RUP replay checker over the recorder's event stream — it turns the gate from
*searching* to *checking* (replay the emitted antecedent chains against the axiom DB rather
than re-solving). Its axiom-DB admission surface is fix-the-class audited — all five paths
have a definite terminal: empty `Query` legit (E1) / empty `Theory_lemma` Invalid / empty
`Reason` Invalid / empty `Conflict` Unsupported / `learned` must RUP-derive (self/forward
citation gated on already-verified ids). Exercised by `make cert-test` (`cert_emit_test`,
discriminating against the pre-wiring core), `make checker-test` (`checker_test`, replay
admission surface), and `make cert-corpus-gate` (end-to-end §4.1 checker-side acceptance over
`tests/cases`). No frozen `.mli` (session.mli wiring is non-frozen). Owner: cert-tranche builders.

## smt/lexical (`oxsmt_lexical`)
The one SMT-LIB 2.6 §3.1 lexer (ADR-0008), stdlib-only, zero deps. Emits a `token`
type whose **headline invariant is "token kind is never lost"**: a quoted `|0|` is a
`Symbol {quoted=true}`, never the numeral `0`; `|let|` is a `Symbol`, never the
`Reserved` word `let`. Both the shipped printer (its quoting decision) and the
test-only parser now tokenize through it, so they cannot disagree on a token
boundary — the fix for the `|0|`/cache-collision bug family. The gate reader
migrates onto it after task/gate3 (a deliberate break of the gate's
zero-`smt/`-deps posture, argued in ADR-0008). Exercised by `make fuzz-lex`.

## smt/smtlib (`oxsmt_smtlib` printer; `oxsmt_smtlib_parser` test-only)
SMT-LIB2 interchange, the format for the oracle and public benchmarks. **Status:
implemented** (was skeleton). Split into two libraries so the parser can never be
linked into the compiler (DESIGN.md §3):
- `oxsmt_smtlib` (`smt/smtlib/`, SHIPS) — the printer over `Oxsmt_core` +
  `Oxsmt_lexical` (both stdlib-only, I3). `Printer.print_session` renders an `Env` +
  ordered assertions (+ optional `:status`) as a complete `QF_UFLIA` script:
  declarations in first-use order (all sorts before all funs), one `(assert …)` per
  assertion, `(check-sat)`. Deterministic (I6). Its symbol-quoting decision is
  grounded in the shared lexer (emit bare iff the name re-lexes as that one symbol),
  plus the predefined-operator/empty refusals; see `printer.mli`.
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
