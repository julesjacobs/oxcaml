# Implementation Plan — OxCaml Refinement Extension

Companion to DESIGN.md. Execute steps strictly in order (exceptions noted
in the dependency notes). ONE step per session/PR.

## Protocol for the implementing model

1. Read DESIGN.md §6 and §9 before every step. They are non-negotiable.
2. For each step: (a) write the step's tests FIRST and show they fail,
   (b) implement, (c) show the step's tests pass, (d) run the regression
   gate, (e) stop. Do not begin the next step.
3. **Regression gate** (every step): the upstream OxCaml testsuite passes,
   plus all tests from prior steps. Report the exact commands run.
4. Never weaken, delete, or skip an existing test to make a step pass.
   Never stub the solver except where a step explicitly says "mock".
5. If a step is blocked by a settled design decision, STOP and produce a
   short blockage report (what was attempted, why it fails) instead of
   improvising a different design.
6. Keep a running NOTES.md: per step, files touched, surprises, and any
   OPEN item resolved (e.g. the C1 lexer decision).
7. Expect-test format for verification tests: each source file declares
   PASS or FAIL and the expected VC COUNT. VC-count drift without a
   plan-level reason is a regression (it usually means obligations were
   silently lost).

Likely relevant upstream files (orientation, not gospel):
`parsing/lexer.mll`, `parsing/parser.mly`, `parsing/parsetree.mli`,
`typing/types.ml`, `typing/ctype.ml`, `typing/typecore.ml`,
`typing/jkind.ml` (and neighbors), `typing/ident.ml`, `typing/env.ml`,
`typing/includecore.ml`, `typing/subst.ml`, `file_formats/cmi_format.ml`,
`typing/printtyp.ml`.

## Phase A — Logic layer (standalone library; no compiler changes)

- **A1. Predicate AST + printer.**
  New library dir (e.g. `verification/`). Constructors per DESIGN §4;
  `v` as Bound index; free vars as stamps (represent as int + name for
  printing at this stage).
  Test: golden-print ~10 hand-built trees.
  Done: goldens stable; no compiler files touched.

- **A2. Sort AST + well-sortedness checker.**
  Sorts per DESIGN §4/§5. `sort_of_pred : sort_env -> pred -> (sort, error) result`
  against hand-built envs.
  Test: unit suite — well-sorted and ill-sorted trees, including
  arithmetic on an uninterpreted sort rejected.
  Done: suite green.

- **A3. SMT-LIB2 serialization.**
  Sorts, declarations, predicates → SMT-LIB text. No solver.
  Test: golden text output for A1's trees.
  Done: goldens stable; output loads in Z3 manually (spot check).

- **A4. Solver harness.**
  Spawn Z3, send trivial script, parse sat/unsat/unknown/timeout;
  configurable timeout; clean process teardown.
  Test: mocked-process unit tests for parsing + one real-Z3 smoke test
  (skippable via env var if Z3 absent).
  Done: both pass; killing the harness leaks no processes.

- **A5. VC module.**
  `check : hyps:pred list -> goal:pred -> Valid|Invalid|Unknown`.
  Test: unit suite incl. `[x>0] ⊢ x≥0` Valid, `[] ⊢ x>0` Invalid,
  timeout ⇒ Unknown.
  Done: suite green.

## Phase B — Kinds (compiler; independent of A, may be done in parallel)

- **B1. Verifier-sort field in jkinds, always default, ignored.**
  Test: full upstream testsuite green. (That IS the test.)
- **B2. Builtin sorts populated** (`int`→Int, `bool`→Bool) in initial env.
  Test: a `-ddump`-style flag or unit hook dumps kinds; assert values.
- **B3. Fresh uninterpreted sort per type declaration.**
  Test: two `type t` decls dump distinct sort ids; redeclaration in a new
  scope gets a new id.
- **B4. Parse `type t : value with verifier Int`; store only.**
  Test: parse + reprint round-trip; testsuite green.
- **B5. Kind inclusion checks sort equality when annotated.**
  Test: sig `with verifier Int` over `type t = int` OK; over `type t =
  bool` errors with a located message.
- **B6. Sealing direction.**
  Unannotated sig kind over `type t = int` ⇒ client-visible sort is a
  fresh uninterpreted sort.
  Test: dump the sealed kind through a functor/module boundary.
- **B7. .cmi round-trip for sorted kinds.**
  Test: compile mli, reload, dump equals pre-save dump.

## Phase C — Refinement syntax (needs B for C4; A1/A2 for the AST)

- **C1. Lexer decision + implementation for `x#` in refinements.**
  Resolve DESIGN's OPEN item: lexer state inside `{... | ...}` or an
  alternative sigil. Record the decision in NOTES.md and DESIGN.md.
  Test: token-stream goldens for refinement snippets AND for
  `float#`/`#(1,2)`/unboxed literals; full testsuite green.
- **C2. Parse `{v:int | p}` → A1 AST; attach payload; checker ignores.**
  Payload per DESIGN §6.
  Test: parse+reprint round-trip of annotated files; testsuite green.
- **C3. Payload survives .cmi.**
  Test: compile annotated mli, reload, print — refinement intact.
- **C4. Well-sortedness of parsed refinements** (A2 wired to B sorts).
  Test: `{v:t | v+1>0}` rejected for abstract `t` with a good location;
  `{v:int | v+1>0}` accepted.

## Phase D — Base checking (needs A5, C)

- **D1. Ascription VC on closed terms.** `(3 : {v:int|v>0})` ⇒ VC with
  empty hyps.
  Test: expect-file: passing and failing ascriptions; failure location
  on the ascription; VC count = number of ascriptions. `--dump-vc`
  implemented HERE.
- **D2. Stamped logic vars minted for let-bound reflectable vars.**
  No hypotheses yet. Reflectability per DESIGN §6 (skip mutable/ref).
  Test: dump logical env; `ref` binding absent from it.
- **D3. Bound refinements become hypotheses.**
  Test: `let x : {v|v>0} = 3 in (x : {v|v≥0})` PASS; `≥1` variant PASS;
  `>1` variant FAIL.
- **D4. Shadowing via stamps.**
  Test: `let x : {v|v>0} = 1 in let x = 0 in (x : {v|v>0})` FAIL, and a
  dual that must PASS only if stamps (not names) key hypotheses.
- **D5. Selfification.**
  Test: `let y = x in (y : {v|v = x#})` PASS — designed to pass ONLY
  with selfification.
- **D6. Refined arrow: argument check only** (result refinement dropped).
  Test: `f : {v|v>0} -> int` applied to `3` PASS, to `0` FAIL.
- **D7. Result refinement propagates into synthesis.**
  Test: `((f 3) : {v|...})` using f's postcondition; also an application
  where the head's type is a type variable ⇒ plain-arrow default, no
  crash.

## Phase E — Path sensitivity (needs D)

- **E1. `if` on a bare boolean variable:** `x#` / `¬x#` per branch.
- **E2. Expression→predicate reflection for conditions**
  (vars, literals, comparisons, `&&`/`||`/`not`; anything else ⇒ no path
  fact, not an error).
  Test: `if x <> 0 then 100 / x else 0` with div's divisor refined
  nonzero — the flagship test. Also: unreflectable condition still
  typechecks with no fact.
- **E3. `match` on `option`: tag facts.**
- **E4. Pattern-binder equations** (`Some x ->` links `x#` to scrutinee).
  Test: payload refinement flows through the pattern.
- **E5. `list` + hardcoded `len` axioms** (`len [] = 0`,
  `len (x::xs) = 1 + len xs`).
  Test: `[]`-branch proves `len = 0`; cons-branch proves `len ≥ 1`.

## Phase F — Pi (needs D; E only for realistic tests)

- **F1. Parse dependent arrow `(x:τ) -> σ`; payload only; testsuite green.**
- **F2. Binder Ident freshened in `Ctype.copy`.**
  Test: instantiate a pi-typed polymorphic value twice; dumped stamps
  differ; sharing of the rest of the graph preserved.
- **F3. Lambda checked against pi.**
  Test: `(fun x -> x : (x:int) -> {v|v = x#})` PASS; broken variant FAIL.
- **F4. Dependent application, variable arg** — stamp renaming.
  Test: `abs y` result type mentions `y#`; usable in a later ascription.
- **F5. Non-variable arg to pi ⇒ clean located error** (no ANF yet).
- **F6. Escape check at generalization.**
  Test: refinement mentioning an out-of-scope binder ⇒ error, not crash,
  not silent `true`. Include the argument-position case.
- **F7. ANF elaboration.**
  Test: F5's example now PASSES; FULL upstream testsuite rerun is the
  gate (elaboration touches every application).
- **F8. Pi ≤ arrow subsumption.** Test: pi-typed `f` into `List.map`.
- **F9. Arrow → trivial-pi lifting.** Test: plain function where a pi is
  expected.

## Phase G — Boundaries (needs B, C3, F)

- **G1. `assume e`** — ascription with VC skipped; flagged in `--dump-vc`
  output as ASSUMED.
- **G2. Unannotated-module boundary** — trivial refinements assumed.
  Test: two-module project, verified↔unverified calls both ways.
- **G3. `measure` declarations in signatures** as uninterpreted logical
  functions (axiom-free beyond congruence, except E5's builtin `len`).
- **G4. Lemma-function pattern end-to-end.**
  Test: a fact unprovable without the lemma call, provable with it.
- **G5. Abstraction-leak test.**
  Sealed `type t = int`: client CANNOT prove `x# + 1 > x#` at `t`.
  This is a required negative test, not optional.

## Dependency notes

- A and B are independent; C4 needs both. D onward is linear.
- E can float after D (F does not depend on it), but E2's flagship test
  is the best early smoke test of the whole pipeline — do not defer it
  past F.
- The three historically-backward dependencies are already fixed here:
  .cmi at B7/C3 (not the end), refined arrows D6 before the div test E2,
  escape F6 immediately after F4.
