# OxCaml Refinement-Type SMT Solver — Design

Status: draft for review · 2026-07-10 · jujacobs + AIDE

**How to read this document**: everything here is a means to one end — a
sound, fast, pure-OxCaml SMT solver for refinement-type VCs. It mixes
*decisions* (architectural principles, staging, correctness strategy, argued
with some care) with *sketches* (code shapes, file names, formats,
thresholds — illustrative only). Treat decisions as strong defaults and
sketches as starting points for your own design pass; neither is law. When
following the document conflicts with the goal, the goal wins — amend the
document rather than comply with it.

## 1. Goals and constraints

Build an SMT solver, in pure OxCaml with no external dependencies, living in the
compiler tree, to discharge verification conditions (VCs) for the OxCaml
refinement type system.

Hard constraints:

- **Pure OxCaml, stdlib-only.** No Base/Core, no ppx, no external solvers at
  compile time.
- **A separate project.** It lives on a branch of the compiler repo purely
  as a convenient host — we are reusing the repo, not joining its build.
  `smt/` is never wired into the compiler's build graph, and nothing in the
  dev loop or CI builds or runs the compiler. The project builds and tests
  standalone (its own make/dune entry points, its own test format), so the
  dev loop is seconds. What keeps eventual integration cheap is the **CI
  dependency firewall**: stdlib-only, no compiler-libs, no opam packages.
- **Correctness gate from day one.** Every query the solver answers can be
  serialized as SMT-LIB2 and certified by an external oracle in CI. The
  primary oracle is **Lean 4 (`grind`)** — Z3/cvc5 are not available in this
  environment — which is slower but stronger: verdicts become kernel-checked
  proofs rather than solver agreement (§8). The gate exists before the solver
  does (milestone M0).
- **Zero human review capacity.** The project is developed by agents. Trust
  comes from agent review, self-checking, redundancy, and external
  certification — with the external gate holding final authority on
  soundness — see §10.
- **Staged.** v1 is a quantifier-free (QF) validity checker for EUF + LIA.
  Lemma instantiation (quantifiers via E-matching) comes next, then a CHC/kvar
  inference layer. Datatypes and bitvectors arrive later as theory plugins.

**Explicit non-goals for now** (to preempt eager integration): do not wire
the solver into the compiler or its build graph, do not modify compiler
sources, and do not build the compiler or run its testsuite as part of any
task — the compiler repo is a host, not a participant. Integration is a
future milestone gated on the refinement checker existing (§9); until then
the stdlib-only dependency firewall is what keeps it cheap.

Key semantic decisions:

- **Integers are mathematical (ℤ) in v1**, as in Liquid Haskell — technically
  unsound under 63-bit wraparound, accepted for v1 since index/length reasoning
  dominates. Sorts carry a theory tag so `Int32`/`Int64`/bitvector semantics
  and an opt-in overflow-side-condition mode can be added later.
- **Annotation-only checking in v1**: the user writes refinement signatures;
  the solver checks QF validity. No refinement inference until stage 3.
- **`sat` handling**: v1 error messages report the failed VC plus relevant
  hypotheses (unsat-core machinery run on the failing direction). Theory
  procedures keep enough state to extract candidate models, but polished
  counterexamples are deferred.

## 2. Staging

1. **Stage 1 (this doc's focus): ground QF solver.** EUF + LIA, CDCL(T),
   incremental (push/pop and assert-after-check), reasons on every inference.
2. **Stage 2: lemma instantiation.** User-stated lemmas / function specs
   instantiated by E-matching against trigger patterns; each round asserts
   ground instances and re-runs the ground core. Requires only the
   incrementality that stage 1 builds in.
3. **Stage 3: CHC / fixpoint layer** for refinement inference (kvars at join
   points and recursive functions), following the liquid-fixpoint recipe:
   exact elimination for acyclic kvars, Houdini-style qualifier weakening for
   cyclic ones. Sits entirely above the SMT interface.

Parallel-anytime after stage 1: datatypes, bitvectors, and other theories as
plugins; certificate production (§7) — a strong candidate to pull early, since
it also converts the slow Lean oracle into a fast checker (§8); counterexample
polish.

## 3. Architecture and layout

```
smt/
  core/        Sorts, hash-consed terms, smart constructors, symbol envs
  solver/      CDCL(T) engine: trail, propagation, conflict analysis
  theories/    euf/, lia/ — plugins implementing the THEORY signature
  interface/   Session API: declare, assert, check, push/pop, cores, reasons
  smtlib/      SMT-LIB2 printer (shipped); parser (test-only, never linked
               into the compiler)
```

(The Lean encoder for the oracle lives in the test harness, outside `smt/`,
as a consumer of the SMT-LIB2 dumps.)

`smt/` is a standalone subproject: its own build entry points (`make build`,
`make test`, `make bench`, `make gate`), its own `.smt2`-file test format
(assertions + expected verdict, the same shape as the public benchmarks), and
its own CI pipeline. Nothing in the dev loop builds or runs the compiler.
Integration later consists of adding the library to the compiler's build
graph and calling the session API from the refinement checker; the
`-dump-smt` flag arrives with that integration, not before.

Hard boundaries:

1. `smt/` depends on nothing above stdlib and nothing from the compiler. The
   refinement checker translates its VC language into `smt/core` terms through
   `interface/`; the solver never sees typechecker internals.
2. Theories are plugins against a fixed `THEORY` signature (assertions in;
   propagations/conflicts + explanations out; trail-synchronized push/pop).
   This is the parallelization seam for agent workstreams.
3. Every session serializes to SMT-LIB2 (`set-logic QF_UFLIA`, declarations,
   assertions, `check-sat`). A `-dump-smt` compiler flag writes one `.smt2`
   file per VC for offline replay. SMT-LIB2 is kept as the interchange format
   regardless of oracle: it ingests the public benchmark corpora and lets
   Z3/cvc5 join the gate in any environment that has them.

## 4. Terms and sorts

**Decided principles**:

- Dynamically sorted, construction-restricted, match-friendly: the classic
  hash-consing pattern (Filliâtre–Conchon) exposed as a **`private` type** —
  `private` permits deep pattern matching and field access while forbidding
  construction, so smart constructors (which sort-check, normalize, and
  hash-cons) remain the sole way to build terms.
- Every term knows its sort in O(1); the invariant is *any `Term.t` in
  existence is well-sorted and hash-consed* — ill-formed terms are
  unconstructible, not merely detectable.
- The node constructors are part of the frozen core API, so the constructor
  set stays minimal and canonical, and normalization invariants ("no nullary
  sums", "comparison args share a sort") are load-bearing for client
  matchers: documented next to the type, enforced by `Term.Debug.check`.
- GADT-indexed terms are rejected (sorts are partly dynamic; solver internals
  want homogeneous terms); a thin GADT facade for the VC-generation client
  can be layered on later without core changes.

**Illustrative sketch only — NOT the frozen shape:**

```ocaml
type t = private { node : node; sort : Sort.t; tag : int (* hash-cons id *) }
and node = Var of Symbol.t | App of Symbol.t * t list | (* ... *)
```

The exact field layout and constructor set are an **explicit M0 design task**
(ADR + adversarial review before the freeze), with open questions this
document deliberately does not settle:

- Arithmetic representation: naive nodes (`Add`, `Le`) vs a normalized linear
  form (`Sum of (coeff * t) list * const`, canonicalized comparisons), and
  whether that normal form lives in the term type or inside the LIA plugin.
- Boolean structure: `And`/`Or`/`Not`/`Ite` as term nodes vs a separate
  formula layer feeding the clausifier, terms staying atom-only.
- Negation/polarity: a node, or normalized into signed atoms.
- Mechanics: symbol interning, tag-vs-hash fields, weak tables vs explicit
  interning, reliance on physical equality, and whether OxCaml features
  (unboxed types, immutable arrays) pay their way in the representation.

These interact with theory-plugin ergonomics and the load-bearing
normalization invariants, which is why they get a considered design
pass rather than inheriting a sketch from the planning phase.

Supporting machinery:

- **Hash-consing** in the constructors: perfect sharing, O(1) equality,
  term-id-keyed sets/maps; sort checks paid once per distinct term. Also
  yields the canonical forms the oracle cache is keyed on (§8).
- `Term.Debug.check`: deep validator for internal invariants (flattened n-ary
  ops, normalized literals), run in debug builds and the testsuite.
- Backstop: ill-sorted output fails loudly in the oracle pipeline (the Lean
  encoder type-checks, and Lean elaboration rejects ill-sorted goals).

## 5. Solver engine

Standard **CDCL(T)** (DPLL(T) à la Z3/cvc5/MiniSat), kept minimal and
deliberately novelty-free:

1. **Preprocessing**: simplification at construction (constant folding,
   `x = x → true`), purification of mixed terms (§6), then Tseitin-style
   clausification of the boolean skeleton with theory atoms as opaque boolean
   variables.
2. **CDCL SAT core** (~1–2k lines): two-watched literals, 1UIP conflict
   analysis with clause learning, activity-based branching, restarts.
   Propositional only; sees theories only through a callback interface.
3. **Online theory integration**: asserted atoms stream to theory solvers as
   the trail extends. Theories report conflicts with explanations (a subset of
   asserted atoms, becoming learned clauses) and propagate implied atoms with
   lazy explanations. Backtracking pops theory state via trail-synchronized
   undo.

Incrementality (push/pop, assert-after-check) falls out of the trail and
assertion-level machinery; it is a day-one requirement because both E-matching
rounds (stage 2) and fixpoint weakening loops (stage 3) depend on it.

## 6. Theories (v1) and combination

**EUF — congruence closure.** E-graph: union-find over term ids, congruence
table, merge queue. Uses the **proof-producing union-find**
(Nieuwenhuis–Oliveras) so every merge is explainable as a subset of asserted
atoms — explanations are native, not bolted on.

**LIA — simplex + integrality.** The Dutertre–de Moura two-layer design:
incremental general simplex over rationals for bound reasoning
(backtracking-friendly), **branch-and-bound** on top for integrality; Gomory
cuts later if workloads demand them. Conflicts fall out as the infeasible
bound set of a row, justified by Farkas coefficients (§7).

**Combination — Nelson-Oppen, lazy.** Preprocessing purifies mixed terms
(`f (x + 1)` becomes `f t1` + `t1 = x + 1`); theories then need only agree on
equalities between shared variables. Since LIA over ℤ is non-convex, use
**model-based combination** (as in Z3): split on a shared equality via the SAT
core only when the theories' candidate models disagree on it.

All three components are literature-standard with reference implementations to
crib from — a deliberate choice for reviewability-by-oracle and for agents
implementing from well-known papers.

## 7. Reasons, cores, proof-readiness

**Uniform currency**: every derived fact — theory propagation, theory
conflict, learned clause — is justified by a **premise set + rule tag**.
Explanations are lazy (computed when conflict analysis asks) but always
available.

**Unsat cores via assumption literals**: each top-level assertion gets a
selector literal; the final conflict over selectors is the core. Powers error
messages ("couldn't prove `i < len`; relevant facts: …") and stage-3
core-guided weakening.

**Certificate-shaped rule tags**, so proof output later is serialization, not
redesign:

- SAT core: resolution chains for learned clauses (DRAT-style logging is
  near-free to add).
- EUF: proof-forest equality/congruence chains — the textbook certificate.
- LIA: **Farkas coefficients** recorded at conflict time; branch-and-bound
  steps as case splits.

**Self-checking before certificates exist** (debug/testsuite builds): every
theory explanation is re-verified when produced (Farkas sums cancel, equality
chains connect), and any learned clause can be re-checked via the oracle.

**Certificates are doubly strategic here.** With zero human review, a tiny
certificate checker eventually removes per-compile trust from all search code.
And with Lean as the oracle, certificates can be **replayed as Lean proof
scripts** (Farkas combinations, congruence chains, resolution steps), so Lean
stops *searching* (`grind`) and merely *checks* — fast and immune
to grind's incompleteness. Pull this work earlier than originally staged.

## 8. Testing and the correctness gate

Four layers, cheapest first:

1. **Internal self-checks** (no external oracle): explanation verification
   (§7); once models exist, every `sat` answer is checked by *evaluating*
   assertions under the candidate model with a trivial independent
   interpreter — `sat` is self-certifying long before `unsat` certificates
   exist.
2. **Golden/expect tests as the backbone**, in the project's own format:
   each test is a `.smt2` file; the runner emits a committed golden block per
   goal — verdict, core size, canonicalized model if sat, and **bucketed
   counters** (log-scale: `<10`, `<100`, `<1k` conflicts/decisions/
   propagations) — accepted via a promote workflow. Bucketing means a golden
   diff appears only on order-of-magnitude behavior change, so perf outliers
   surface as information in the same diff the author must review, rather
   than as a hard gate. Wall-clock never appears in golden files
   (nondeterministic); exact counters and timings go to an uncommitted stats
   sidecar aggregated nightly. Property/fuzz tests are invariant-checked (not
   golden); public benchmarks are label-checked; everything deterministic
   flows through the same promote workflow.
3. **External oracle: Lean 4 certification** in a dedicated CI job (the
   compiler build never needs it). Slower than a solver diff but stronger —
   the Lean kernel checks proofs, so agreement is certification:
   - *Unsat direction*: each `unsat` query is translated to a Lean theorem
     (the VC's validity) discharged `by grind`. Success is a kernel-checked
     proof our verdict is right. Failure is **inconclusive** (grind
     incompleteness or timeout, not necessarily our bug): the oracle is
     asymmetric. Hard failure = Lean proves the *negation* of our claim —
     ship-stopping. Soft failure = grind gives up — triage queue.
   - *Sat direction*: our candidate model is emitted as a concrete ground
     Lean goal closed by `decide`/`native_decide` — fully certified
     counterexamples, strictly better than solver agreement.
   - *Real corpus*: `-dump-smt` collects every VC from compiling
     refinement-annotated code; new queries are certified and cached (below).
   - *SMT-LIB benchmark library*: public QF_UF / QF_LIA / QF_UFLIA sets come
     **pre-labeled**, so they need no local oracle at all — they are the main
     fast completeness *and* soundness regression suite (this is why
     `smtlib/` grows a test-only parser early). SAT-competition benchmarks
     exercise the CDCL core in isolation.
   - *Random formulas*: generator biased toward small, VC-shaped formulas
     (deep conjunctions, shallow disjunctions, mixed EUF/LIA atoms). The
     `sat` side is checked by free model evaluation; only `unsat` claims
     spend Lean budget. The delta-debugging shrinker runs against our own
     self-checks, confirming only the final minimal case in Lean.
   - *Round-trip*: parse our own dumps, re-solve, require verdict equality —
     tests the dump path itself.

   **Oracle caching (mandatory — Lean is orders of magnitude slower than a
   solver diff):**
   - Queries are canonicalized (normalization, sorted assertions, canonical
     variable numbering — nearly free given hash-consing) and content-hashed;
     the cache maps hash → verdict + oracle outcome, keyed additionally by
     encoding version, Lean toolchain, and grind configuration.
   - PR CI certifies only cache misses within a fixed time budget; timeouts
     go to a nightly queue with a larger budget. The corpus grows
     monotonically; nothing is silently re-certified or silently dropped.
   - Once certificate replay (§7) lands, Lean checking becomes cheap and the
     triage queue shrinks to genuine disagreements.
4. **Performance and latency visibility — alerts and outliers, not gates.**
   Hard CI failures are reserved for correctness (verdict/label mismatches,
   honeypots not firing, models failing evaluation). Performance is surfaced,
   not gated:
   - Per-goal bucketed counters in golden output (layer 2) make
     order-of-magnitude changes visible in the promotion diff itself.
   - Nightly aggregation: counter and wall-clock
     distributions, top-k slowest goals, trend alerts on drift. Refinement
     VCs are expected to solve in low milliseconds; outliers are flagged for
     the master, who decides whether they warrant a task.
   - An **adversarial perf corpus** grown deliberately (EUF diamond chains,
     dense simplex instances, large `ite` trees) so cliffs surface in CI
     rather than in the first real codebase.
   - **The dev loop itself is monitored the same way**: PR-suite wall-clock,
     cache hit rate, slowest tests, triage/nightly queue depths, and
     time-from-dispatch-to-merge are aggregated. Latency rot has no
     natural victim in an all-agent project — no agent gets annoyed waiting —
     so the master must watch these numbers and spawn re-curation/
     parallelization tasks when the loop silts up.

**Verdict asymmetry**: a soundness signal (Lean refutes our `unsat`; a
pre-labeled `sat` benchmark we call `unsat`; a model that fails evaluation)
blocks merge. A completeness signal (we return `unknown`/`sat` where the label
says `unsat`, or grind can't confirm) files an issue and enters triage.

## 9. Roadmap

- **M0 — the gate**: sorts/terms/hash-consing, smart constructors, SMT-LIB
  printer, `.smt2` regression harness, Lean encoder + certification CI job
  with the content-addressed cache. *The oracle exists before the solver.*
- **M1 — ground core**: clausifier + CDCL SAT core, validated on public SAT
  benchmarks. `THEORY` interface frozen at the end of M1 (after a
  sanity-check against stage-2 E-matching needs).
- **M2 — EUF** (QF_UF passes) ∥ **M3 — LIA** (QF_LIA passes): parallel
  workstreams against the frozen interface.
- **M4 — combination** (QF_UFLIA) and unsat cores: the solver is
  feature-complete for stage 1 as a standalone library. (Compiler
  integration — build-graph wiring, `-dump-smt`, first real refinement VCs —
  is a separate milestone gated on the refinement checker existing, not on
  this roadmap; the stdlib-only dependency firewall keeps it cheap in the
  meantime.)
- **M5 — certificates + Lean replay** (pulled forward; see §7): removes
  trust from search code and makes the oracle fast.
- **M6+**: E-matching / lemma instantiation. **M7+**: CHC/kvar layer.
  Datatypes, bitvectors: parallel theory plugins any time after M4.

## 10. Trust model (zero human review)

Trust comes from agent review, oracles, redundancy, and external
certification; the Lean gate is the only fully uncorrelated judge and holds
final authority on soundness. The mechanisms:

- **Oracle-first.** No module starts until its independent oracle exists.
  Agent-written tests never count as the gate.
- **The gate is write-protected.** Oracle code, the Lean encoder, corpora,
  frozen interfaces, and CI config are master-only; a child may not edit
  tests/oracles in the same change as code — a test it believes wrong goes to
  the master, not into the diff. Primary defense against test-gaming.
- **Mutation testing.** Seeded faults must be caught by the suite; a
  surviving mutant halts feature work on that module.
- **Honeypots.** Nightly known-wrong verdicts and seeded mutants must turn
  the gate red.
- **Two-model review (codex + fable) on every land**; a zero-finding codex
  exit counts only after transcript validation.
- **N-version checkers** (model evaluator, explanation verifiers, Lean
  encoder) written from spec by separate sessions, no solver-internals access.

Residual TCB, stated honestly: term smart constructors, check-sat glue, the
SMT-LIB printer, the Lean encoder, the certificate replay checker, and VC
generation in the refinement checker.

## 11. Orchestration

A master agent plans, adjudicates, and approves; child agents build in their
own worktrees and never push; a dedicated integrator executes merges on the
master's sha-pinned approval. Dispatches are short and high-level: outcome,
hard constraints, acceptance evidence — the builder owns the design.

**Rebase → test → fast-forward only.** The integrator rebases onto trunk,
runs the full suite on the rebased head, lands with `--ff-only`: trunk never
contains an untested state, history stays linear, bisect stays mechanical.
Reviews and suites run speculatively against pinned shas, in parallel across
lanes; a rebase that shifts reviewed hunks triggers a scoped re-verify.
Corpus sweeps attribute, they do not gate — they run off the land path, but
MISMATCH>0 or a wrong-direction surprise stops trunk until explained.

Topology: `main/` (integration trunk, branch `oxsmt`), `worktrees/` (one per
task, removed at merge or abandonment), `cache/`, `corpora/`, `logs/` (full
tool output; agents see digests) — siblings, never in git. Nothing lives
loose in a worktree. Tools print digests and write detail to `logs/`;
flooding an agent's context is a defect.

## 12. Risks

1. **Silent unsoundness** — the design's center of gravity; mitigated by the
   gate, self-checks, mutation testing, certificate-shaped reasons, and
   certificate replay in Lean (M5).
2. **Weak oracles that pass everything** — the specific zero-human-review
   failure mode; mitigated by mutation testing, N-version checkers, and
   reviewer calibration via honeypots.
3. **Oracle asymmetry and slowness** — grind failures are inconclusive and
   Lean is slow; mitigated by the content-addressed cache, pre-labeled
   benchmark suites carrying the fast both-direction regression load, model
   evaluation covering the `sat` side for free, and certificate replay
   eliminating grind search from the loop at M5.
4. **Performance cliffs** on adversarial-but-real VCs (large `ite` trees, wide
   bounds) — mitigated by the benchmark corpus and perf tracking from M0.
5. **Interface churn from stages 2–3** — mitigated by incrementality-first
   and validating the `THEORY` interface against E-matching needs before
   freezing.
6. **Stdlib-only friction** (no ppx, no expect tests) — accepted cost of
   compiler-integrability; `.smt2`-file-based testing compensates.
7. **Integration drift** ("we'll integrate it later" rot) — consciously
   accepted more fully now that nothing ever compiles against the compiler
   tree; the mitigation is the stdlib-only dependency firewall, which keeps
   the eventual integration surface small by construction rather than
   continuously tested.
8. **Integer-semantics gap** (ℤ vs 63-bit wraparound) — accepted for v1,
   flagged for an opt-in overflow-side-condition mode or bitvector theory
   later.

## Addenda

### A2 — Combination by internalization (2026-07-11, design author)

Amends §6 ("Combination — Nelson-Oppen, lazy"). The preprocessing
purification sketch is superseded by **internalization** (Z3-style, per
de Moura & Bjørner): no fresh proxy variables, no defining equations —
**each term node is its own proxy**. Theory ownership is by head symbol;
the interface set is the **boundary-crossing nodes** (nodes whose owner
differs from their parent's: an arith-headed node under `f`; an
EUF-headed node inside a sum, which LIA treats as an opaque variable),
computed structurally at assertion time — sharedness is total by
construction, never the output of a relevance filter. This deletes the
combinator's per-case gates and the wrong-SAT bug family they
approximated away, with less machinery than explicit purification.

The CDCL(T) **seam is kept** — no e-graph hub. Equality exchange between
theories stays at the seam as SAT-visible atoms over **original terms**,
with splits requested as decisions (not tautology clauses); EUF remains
an ordinary plugin behind the frozen interface. Rationale: decoupling
(the parallel-workstream seam), reasons-purity, and certificates —
splits over original terms need no definitional layer in Lean replay.

Spec-by-citation: each boundary node is its own proxy; this implements
Nelson–Oppen-with-purification, per de Moura & Bjørner's observation
that internalization makes explicit proxies unnecessary. Reviewers check
conformance against the textbook account through that stated
correspondence.

Lemma-readiness invariants (stage-2-proof): (i) instantiated instances
and all their interface bookkeeping are asserted at the scope of their
quantifier, never at the current decision level — all interface state is
grow-only and retraction-free (registry mutant: an instance asserted at
decision level must be caught by a test where backtracking strands
interface state); (ii) interface registration is a pure function of the
hash-consed node — idempotent, deduplicated; (iii) the seam walks the
current interface set in canonical term-id order, bucketing by candidate
value; termination is scoped per ground check (finite current set, each
pair split once per branch) — not claimed globally across instantiation
rounds; (iv) triggers follow the standard discipline (uninterpreted
symbols only); arithmetic lives in lemma bodies, which the assert-time
pipeline handles like any other assertion.

Acceptance: the dual-review repro corpus (including
`x=0 ∧ f(x+1)<f(1)` and `x=y ∧ f(x+1)<f(y+1)`) is the acceptance suite;
UNSAT through the real stack gates the close.

**A2 erratum — splits ruling (design author, same day).** The
"splits as decisions" clause above is overruled by the evidence: the
frozen `THEORY` seam has no decision-request channel (verified against
source in review), and both original arguments for decisions dissolve.
The discardability hazard applied to the two-literal `A ∨ ¬A` form;
the trichotomy `x=y ∨ x<y ∨ x>y` is three distinct atoms —
propositionally not a tautology, genuinely constraining, and no
cleanup pass may discard it. Reasons-purity is preserved: the
trichotomy is a valid LIA lemma (one uniform "theory-valid clause"
step in the certificate ledger), and branch choice remains an ordinary
SAT decision — fact and guess stay in their lanes, which was the actual
principle. Conditions: clauses created lazily, only for pairs the seam
actually questions (bounded by the interface set), always in the
multi-literal non-tautological form; for non-arithmetic sorts, use the
equality atom with disequality handled by EUF — the clause exists only
where a real third literal does. Watch item (bought on measurement
only, never preemptively): split-exploration promptness rides the
final-check re-fail loop; if the perf corpus shows combination-heavy
lag, a decision-request/phase-hint hook is a Tranche-C candidate.
*Lesson (at the design author's expense): the directive specified a
mechanism where it meant a principle; the frozen interface plus a
reviewer reading actual code caught it. Directives should state the
invariant — "guesses must never carry fake justifications; splits must
not be discardable" — and leave mechanism to the people holding the
code.*

**A2 erratum — interface-membership invariant.** "Registration is a
pure function of the hash-consed node" is corrected: boundary status is
a property of *occurrences*, not terms (`x` in `x ≤ 0` is purely
arithmetic; a later `f(x)` creates an ownership-crossing occurrence and
promotes `x`). Blessed form: shared-status grows monotonically as new
uses appear and is never retracted; each processing step is idempotent
and deterministic. Riders: (1) the safe direction of error is
over-approximation — wrongly-in costs harmless extra questions,
wrongly-out is the wrong-SAT family; caches and push/pop resolve doubt
toward *more* shared (conservatively retain membership until the
creating scope pops). (2) The debug check adapts rather than dies:
membership must be constant between assertions — snapshot-identical
across branching rounds, cut generation, and seam passes — and may only
grow at an assertion. The original sentence stays in the ADR with the
correction labeled beneath it, as permanent teaching material.

**Triviality exception (design author, same day):** the exact-sha rule is
not absolute. For **trivial rebases** — formatting-only deltas, or hunks
fully disjoint from everything that landed since the reviewed base — the
integrator may carry review verdicts forward and land with the fast suite
(or, for whitespace-only deltas, a build check) instead of the full PR
suite. The integrator classifies triviality and records the
classification in the merge report; anything with semantic overlap, a
touched reviewed/TCB hunk, or a conflict resolution gets the full
re-test, and a moved reviewed hunk still triggers the scoped re-review.
The point of the exact-sha discipline is semantic-conflict defense, not
ceremony — spend it where that risk exists.

### A4 — Whole-theory deactivation vs relevance filtering (2026-07-12, design author)

The UF-free EUF skip is sound, but the reason must be stated precisely or
it will either be rejected by over-applying the internalization lesson, or
worse, slowly generalize back into per-term gating. The distinction: the
banned thing was per-occurrence relevance filtering inside mixed problems —
deciding which terms "matter"; every heuristic had counterexamples. A
whole-theory deactivation by absent signature — "this problem contains zero
uninterpreted function/predicate symbols, so congruence can contribute
nothing LIA doesn't know, and the boundary set is empty" — is a total,
syntactic, assert-time condition, not a relevance guess. One subtlety
inherited from our own invariants: activation must be monotone and
assert-triggered — the first UF symbol arriving (input or lemma instance,
later) flips EUF on, exactly like boundary-status growth. Worded that way,
it is the same grow-only pattern, not a new gate. Enforcement: a mutant in
the registry — a mixed problem sneaking past a stale "UF-free" flag must be
caught.

**A4 erratum — from switch to membership rule (2026-07-12, design
author).** The A4 "UF-free skip" is superseded: a binary switch makes the
cost function discontinuous — one `f` buys congruence-grinding over ten
thousand arithmetic terms — which is a design smell, not a fix. The cliff
is an implementation conflation, not inherent to internalization. The
correct rule: **EUF's cost must be proportional to the uninterpreted
structure, not the term count.** The e-graph needs exactly: uninterpreted
applications (`f(…)`, `p(…)`); their argument subterms — precisely the
boundary nodes already computed; the Bool machinery (⊤/⊥, predicate
atoms); and terms in equality atoms between the above. A pure-arithmetic
term that never sits under an uninterpreted symbol needs no e-node at all
— there is nothing above it for congruence to conclude about, and
equalities among such terms are LIA's native food. This is not a
relevance heuristic of the banned kind: it is definitional (congruence
provably cannot involve terms outside this set), total, syntactic,
computed at assert time, and monotone — a term enters the e-graph when
its first under-`f` occurrence arrives and never leaves. Same invariant
family as boundary status, same safe direction: over-inclusion is merely
slow; under-inclusion is the wrong-SAT direction and gets the mutant (a
term under `f` missing its e-node must be caught). Consequences: a
pure-LIA file yields an empty e-graph — the "UF-free skip" is the empty
instance of the general rule, with no activation logic and no stale-flag
hazard; a QF_UFLIA file with 3 applications and 5,000 arithmetic terms
pays for 3, not 5,000. The seam is untouched (it always operated on
boundary nodes, which are all present); `x = y ⟹ x+1 ~ y+1` was never
congruence's job (`+` is interpreted) — it flows through the seam's value
comparison. Acceptance evidence: an e-graph-size counter in the goldens
tracking #UF-applications + #boundary nodes (proportional by
construction), the pure-LIA-implies-empty-e-graph property test, and the
under-inclusion mutant. *Lesson (second occurrence of this shape): when a fix arrives as an on/off switch, look for the
proportionality rule it's approximating. Switches have cliffs and
stale-state hazards; structural cost-proportionality has neither.*

### A7 — Constitution reset (2026-07-12, master)

AGENTS.md replaced with a short constitution; all other .md files deleted.
Their full text is in git history (`git show <sha>:<path>`). Invariants I1-I8,
the frozen-interface mechanics (FROZEN.sha256, `make check-frozen`), and all
ratified ADR decisions remain in force unchanged — deleted as files, not as
rules. Land bar: `make test` + `make gate` + two-model review (codex + fable).
Any process step over 5 minutes gets questioned: eliminate it or speed it up.

### A8 — Tranche-C unfreeze: SAT decision branch-filter hook (2026-07-13, master-approved)

`sat.mli` grows one optional value, `set_branch_filter : t -> (var -> bool)
option -> unit`, and `FROZEN.sha256` is regenerated to match. This is the
"decision-request hook, a Tranche-C candidate bought on measurement" that the A2
erratum (above) explicitly reserved — now bought: QF_UF loses to z3 by
decision-heuristic thrashing (measured 6-33x more decisions/conflicts on the
QG/iso tail), whose root cause is full-biconditional Tseitin leaving a satisfied
disjunction's other atoms free for VSIDS to decide and spuriously over-constrain.
The hook lets a relevancy driver (z3 `smt_relevancy`) tell the brancher not to
DECIDE currently-irrelevant atoms. None of the 14 frozen declarations change; this
is an addition, and with the hook unset (`None`, the default) `pick_branch` and
therefore every verdict/model/counter is bit-identical to before — so the
certificate-emission surface is untouched in practice, and untouched in principle
because certificate replay validates learned clauses and the input/unit closure,
never the decision ORDER. Soundness distinction from the A4 relevance-filtering
ban: that ban is on THEORY-level per-term gating, where under-inclusion silently
drops a term the theory needed and yields wrong-SAT. This hook only restricts SAT
*branching*; it asserts nothing, so it cannot create a conflict (no wrong-UNSAT),
and handing a branchable-only partial assignment to the theory's `Final`
check/model-check (fail-closed) means a driver that wrongly marks a needed atom
irrelevant degrades the query to `unknown`, never a wrong verdict. The relevancy
driver ships env-gated, default OFF; the ON decision rides a corpus A/B (zero
verdict disagreements + net-non-negative at 2s). Amended when the client landed (the
seam+fix pair): the review found the raw core under a filter can report `sat` on a
partial assignment that falsifies a clause (safe only because every consumer
model-checks), so `pick_branch` was made exception-safe (it restores every popped
variable on any exit, including a raising filter) and the `sat.mli` soundness
paragraph was rewritten to hold unconditionally — no wrong-`Unsat` ever; a filtered
`sat` is not self-certifying and the client must model-check it — plus a
filter-totality contract line.

### A9 — Tranche-C unfreeze: read-only VSIDS activity accessor (2026-07-13, master-approved)

`sat.mli` grows one read-only value, `var_activity : t -> var -> float`, and
`FROZEN.sha256` is regenerated to match. Same shape as the A8 hook: a pure
addition, none of the frozen declarations change, and it never mutates the solver
or affects search — a client that does not call it (every shipped caller) is
bit-identical. The relevancy driver (A8) uses it so that, when a satisfied
disjunction has no justifying child yet, the branch candidate it keeps relevant is
the solver's own highest-activity unassigned child rather than an arbitrary
lowest-var pick; this measurably tames the decision-count variance the first-cut
lowest-var driver showed on the QF_UF sat sample. Read-only and side-effect-free,
so it carries none of the soundness weight of the branch filter itself.

### A10 — Tranche-C unfreeze: eliminable-variable marking for CNF pre/inprocessing (2026-07-14)

`sat.mli` grows one value, `set_eliminable : t -> var -> unit`, and `FROZEN.sha256`
is regenerated to match. Same additive shape as A8/A9: none of the 14 frozen
declarations change, and a client that never calls it (the default) leaves the
whole feature inert — SAT preprocessing (bounded clause elimination, Jacobs 2021)
eliminates nothing, and verdicts/models/counters are bit-identical. The value is
the seam by which the clausifier tells the core which variables are pure Tseitin
auxiliaries (invisible outside the SAT core) and therefore safe to eliminate. The
core DEFAULTS every variable frozen, so this is the sole opt-in and "when in doubt,
freeze" is structural: a forgotten marking costs only effectiveness, never
soundness. Preprocessing itself is env-gated (`OXSMT_SATPRE`, default OFF) and, like
the presolve passes, disabled while a certificate trace is installed (the added
resolvents / deleted clauses are not yet routed through cert emission — a follow-up).
Soundness rests on the note's Lemma 1 reconstruction, which the core performs inside
the single model-snapshot choke point (`save_model`) before any consumer reads a
value — so a reported model is correct over every variable including eliminated ones,
with no downstream check required (the raw-SAT-API contract); a marked variable that
later reappears in an added clause is restored (its deleted clauses re-added) to keep
the elimination sound under incremental additions.

### A11 — Tranche-C unfreeze: clarify the A10 restoration promise for ELS (2026-07-14)

TEXT-ONLY amendment of the `sat.mli` `set_eliminable` doc-comment; no declaration
changes (the 14 frozen signatures are byte-identical to A10) — `FROZEN.sha256` is
regenerated only because the comment bytes moved. The A10 text promised, without
qualification, that "a variable already eliminated that later appears in a newly
`add_clause`d clause is restored." That holds for the bounded-variable-elimination
form (a `restore_map` replay re-adds the deleted clauses), but the newer
equivalent-literal-substitution (ELS) form instead RAISES `Invalid_argument` on such
a re-reference: ELS rewrote the variable's equivalence-establishing clauses away, so
sound reactivation would need the incremental-ELS machinery (Fazekas–Biere–Scholl,
SAT 2019) that is not built, and failing loud is preferred to a silent wrong result.
Both branches are contractually UNREACHABLE for a conforming client, since an
eliminable variable is by contract one that no re-added clause can name; the amend
just makes the frozen text match the fail-loud reality rather than contradict it (the
"frozen doc-comments are contracts" discipline). No behavior change. Surfaced by the
codex/fable review of the ELS+FLP stack (finding F3); adjudged option (a) — amend the
contract text — over building incremental-ELS reactivation for a dead path.

### A12 — QF_AX weak-equivalence decision procedure (Christ-Hoenicke) (2026-07-14)

RATIFIED-WITH-OBLIGATIONS (Rev 2; both adversarial legs). Full ADR body:
`../logs/adr-weakeq-draft.md` (the ADR home — ADRs live in the untracked sibling
`../logs/`, e.g. ADR-0014 = `../logs/adr-egraph-fabric-draft.md`; DESIGN.md carries the
decision record, not the body). **Decision:** replace the blind index-split
read-over-write (`row_split`) in the arrays theory with a maintained
weak-equivalence graph (store edges + equality edges) and graph-guided read
propagation — the residual QF_AX walls (23 swap store-permutation refutations) blow up
`row_split` because no index diseqs are asserted, and every prior lemma-on-demand
variant netted <=0 on the deterministic counted-effort measure (family 1/2, parked). CH
confines the index-coincidence search to the actual store-chain path indices.

Staged, env-gated `OXSMT_ARR_WEQ` (default OFF, byte-identical when unset), reject-capable
at each stage on the **counted-effort** measure (wall A/B is inadmissible — QF_AX losses
sit at the 2s wall):

- **W0 (dark, this stage):** the maintained weak-equivalence graph (`Weq_graph`) off the
  Euf merge stream with Trail undo, an abstract term-carrying e-graph view (O6, so the W3
  fabric migration is a re-binding), the O9 index-sort-stability gate (rules fire only over
  stably-infinite index sorts — a finite index sort breaks the model validator's
  finite-default assumption, a wrong-SAT vector), and the O8 arity/sort forge tests.
  Emits nothing; verdict-identical OFF and ON.
- **W0.5:** a dark store-chain analyzer that arbitrates whether the storecomm losses are
  CH-addressable (O4' established only that the CH-immune level-0 closure is <1%; the
  split-induced share is OPEN).
- **W1:** the O1'-guarded L1 read-over-weak-equivalence rule, ADDING to `row_split`. The
  emitted clause is an unconditional array-theory tautology over TERMS with a negated
  equality guard at EVERY class-collapse point (the two reads' own indices, path endpoints
  including the zero-length branch-local-merge case, mid-path congruences, term-identity-
  deduped store indices) — this is what makes the permanent lemma memo sound across
  backtracking. Deterministic fuel -> `unknown`, cert self-disable.
- **W2:** retire `row_split`; L1 + L2 + ROW1 is the decision procedure. The SAT authority
  stays the `Array_model_check` materialized-diff-witness validator (O3/O10), so a bad
  lemma set can only fail to refute (-> `unknown`), never wrong-`sat`.

Soundness frame: every emitted clause is theory-valid (UNSAT direction can only prune);
the sat direction is unchanged and validator-gated. The `row_split` arity/sort guards are
LOAD-BEARING and re-inherited by L1/L2 through W2, with the `Array_defs.validate_ranks`
install-door as the unchanged backstop (`[[arr-arity-guard-load-bearing]]`).

**W0/W0.5 landed (dark) + review addenda (2026-07-14).** W0 (graph substrate) LANDED dark;
both review legs ratified the builder's design deviation — the graph is maintained as a
permanent-store + trailed-equality adjacency with deterministic BFS, NOT a re-rooting
union-find forest, because mixing permanent and trailed unions in one forest dangles a union
when an equality edge pops (codex verified), and the term-node/explicit-edge representation
makes every class-collapse point a guardable edge, which RESOLVES the O1 term-guard gap
outright (fable). W0.5 (dark store-chain analyzer) measured all 50 storecomm losses: OPEN = 0
on every one (closed = exactly the 32 z3-unsat) -> storecomm IS CH-addressable (deterministic
~60-way L2 closure, zero open off-diagonal tests), GREEN-lighting the W2-storecomm expectation
and resolving OQ3/O4'. Obligation-4 check (fable rider): the LEGACY `row_split` path does NOT
wrong-SAT on finite-index arrays today — they degrade to `unknown` via the fail-closed
`Array_model_check` posture (sound, incomplete); no separate lane needed.

**W1 measurement status (2026-07-14, checkpoint 2dafbcef91; SOUND — 0 mismatches — and dark
throughout).** Two L1 forms have been measured and are BOTH net-negative WITH EXPLANATIONS,
not yet a verdict on the idea:
- CLAUSE form (O1'-guarded wide avoidance disjunction added to `row_split`): counted-effort
  QF_AX A/B = OFF 518 -> 410 @50k (NET -108; 107/108 regressions swap; ZERO gains). The wide
  disjunct set forces the SAT solver to re-derive propositionally what the theory already
  knows deductively (W0.5's entailed closure), inflating like the blind splits it replaces.
- NAIVE PROPAGATION form (ruling (b) mechanism, no incremental trigger): collapses to 41
  solved @50k — an implementation artifact (mass-assert of all entailed reachable pairs +
  O(reads^2) per-Final rescan), NOT a fair test of the propagation idea.
FAIR-PROPAGATION PENDING (handed off, bounded charter + pre-committed kill rule): the honest
(b) test needs (1) an incremental merge-cursor-driven trigger, (2) diseq-endpoint-targeted
propagation, (3) a path-confined one-index-at-a-time split for the fresh witness `k`, plus
premise-completeness RED tests. If fair-(b) also nets <=0 with zero conversions, CH-as-W1
joins the refuted ledger (option (c)) with W0 (landed) + W0.5 (banked) retained; no fourth
form. Full spec + shas in `../logs/ax-bar-log.md`.

**FINAL STATE (2026-07-14): fair-(b) LANDED dark, W2 PARKED per the pre-committed kill rule.**
The CH program concludes here.
- **fair-(b) LANDED (dark, `OXSMT_ARR_WEQ` default OFF).** The honest ruling-(b) propagation
  form — CONFLICT-ONLY read-equality propagation (fire `assert_eq` only when the read pair is
  already entailed-DISTINCT, so a merge always refutes and never survives into a SAT model to
  corrupt its reconstruction), trailed `an_diseqs` (so `an_distinct` reflects only live
  disequalities — the basis of the conflict-only gate and of `weq_read_premise`'s off-diagonal
  premises), an incremental merge-cursor/diseq trigger, diseq-endpoint-targeted propagation,
  and a path-confined witness split. Counted-effort QF_AX A/B = NET +1 at 20k/50k/200k, 0
  disagreements / 0 mismatches at every budget, storecomm/storeinv/cvc completely FLAT, with
  genuine swap-unsat WALL conversions. First weakeq form with zero SAT-family collateral (vs
  the clause -108 / naive -477). Both review legs clean (fable APPROVE, codex SAFE). RED tests
  (mutant-patch): drop off-diagonal `i≠jₗ` premise -> 11 wrong-unsat; drop `an_diseqs`
  pop-restore -> 111 unknown-regr; fuel=0 -> safe degrade.
- **W2 (retire `row_split`) PARKED.** Built the O11 weak-congruence-modulo-i read closure
  (`Weq_graph.find_path_avoiding` i-avoiding-subpath BFS + a modulo-i propagation) and
  broadened the narrow split into a genuine path-confined `row_split` replacement. NOROW
  counted-effort A/B @50k: witness-only -215 -> broadened -61 -> +O11 -61 (O11 changed
  NOTHING). W2 cannot reach net-≥0 for TWO reasons, both measured: (1) **SAT-model
  completeness** — retiring `row_split` loses the global read materialization that constructs
  valid swap models, so several `swap_invalid` (SAT) files reject at `Array_model_check` and go
  `unknown` at tiny effort; `row_split` IS the SAT-model backstop. (2) **Split-ordering
  efficiency** — path-confined telescoping is slower than blind `row_split`'s global tag-least
  ordering (the regressed swap-unsat files solve only at ~500k). O11's propagation is
  conflict-only (a refutation accelerator that needs decided index diseqs swap lacks) and
  addresses NEITHER. A genuine W2 needs the full **O10** semantically-complete model-value
  validator (materialize weakeq-determined read values for the SAT direction) PLUS complete,
  non-path-confined case-split coverage — the "large/uncertain/multi-session" scope this ADR
  always flagged. Parked scaffolding: `../logs/weq-w2-o11-parked.patch` (SOUND, 0 disagreements).
- **Upstream statement:** the QF_AX 537 bar needs either that O10 + full-case-split investment
  or a different lever; W0 graph + W0.5 analyzer + fair-(b) are banked, storecomm/storeinv
  close under NOROW, and 2-3 swap-unsat walls convert in the landed default form.

**Consolidated W1 obligations (from the W0 dual review; binding, tested):**
1. **Count bound is NOT acyclicity (LOAD-BEARING, codex).** The combined store+equality graph
   cycles (s1—base—s2 with s1=s2) even though the equality subgraph is a forest. W1's lemma
   count bound rests on SELECTED-PATH (one BFS path per trigger) + TRIGGER DEDUP (permanent
   per-read-pair memo) + bounded re-emission across backtrack (the memo is not trailed, and a
   term-level-tautology clause stays valid after any backtrack, so re-emission is suppressed
   forever). Tested on a cycle-heavy instance under repeated backtracking (bounded emission).
2. **Query-side O9 (LOAD-BEARING, codex).** `find_path a a` returned `Some []` before the O9
   admissibility check, so a rule could fire over a finite-index array via a zero-length path;
   `find_path` now rejects an inadmissible sort up front (before the reflexive shortcut).
3. **eq_key overflow (codex).** The equality-edge dedup key switched from a Cantor pack (int
   overflow -> silent collision -> dropped edge -> incompleteness) to a structural `(int*int)`
   pair key.
4. **Finite-index legacy path (fable rider):** checked, not a live bug (see above).
5. **Freeze reports are box-local:** gate claims (make test EXIT, gate counts) go in the
   commit message so they are tracked-verifiable.

### A13 — Model.value integer widened to Bigint (2026-07-15, master-approved)

**Unfreeze of `smt/core/model.mli`** (one of the 14 hash-frozen core interfaces).
Change: the `Model.value` variant `Int of int` becomes `Int of Bigint.t`. `FROZEN.sha256`
is regenerated to match; the other 13 frozen signatures are byte-identical. This is a
type-shape change, not additive, so — unlike A8–A11 — it is NOT dark/byte-identical: the
acceptance battery (below) replaces byte-identity.

**Rationale (master).** The term layer already carries arbitrary-precision integers
(`Term.Int_const of Bigint.t`, killing the 2^64 coefficient cap), and the downstream model
vocabulary is already Bigint (`Cdclt.value = VInt of Bigint.t`, rendered via
`Bigint.to_string`). `Model.value`'s `Int of int` was the lagging int63 inconsistency
between them: a satisfiable query whose model assigns a variable a value exceeding int63 —
e.g. a uint256 mask (2^256) in the Certora/Solidity QF_UFLIA family — could not be
represented at the LIA↔combinator model boundary, so `Lia.extract_model`'s native-int
projection (`Rational.num`) raised `Rational.Overflow`, which escaped `Sat.solve` and
degraded the query to `unknown`. 2^256 model values are core to the OxCaml refinement-type
target. A parallel Bigint model channel (leaving `Model.value` native) was rejected as the
two-event-path hazard class the certificate lane flagged.

**Exact scope (this addendum's working name in code comments is "ADR-0018"):**
- `smt/core/model.mli` / `.ml`: `Int of int` → `Int of Bigint.t` (the frozen change).
- `smt/theories/lia/rational.{ml,mli}`: add `num_bigint` / `floor_bigint` — the numerator
  and floor as `Bigint.t` WITHOUT the int63 output projection (never raise `Overflow`).
- `smt/theories/lia/lia.{ml,mli}`: add `model_bigint` (arbitrary-precision model
  extraction via `num_bigint`); `suggest_branch` branches via `floor_bigint` +
  `Context.int_const_big` so B&B on a >int63 value no longer overflows. The int-tier
  drivers (`model`, `cube_model`, `Int_sat`) are unchanged.
- `smt/theories/lia/lia_adapter.ml`: `model` reads `model_bigint`.
- `smt/combine/combine.ml`: `value_equal`/`model_eval`/`class_int` fold and compare in
  `Bigint`; the `model_eval` Arith fold no longer needs an overflow guard (removed
  `add_guard`/`mul_guard`) and no longer degrades a >int63 constant to `None`.
- `smt/interface/cdclt.ml`: `value_of` `Model.Int n -> VInt n` (no `Bigint.of_int`); the
  ζ-realization `int_used` pool records only int63-fitting used values (a >int63 value
  cannot collide with the small non-negative witnesses `fresh` mints).
- `smt/interface/{array,dt}_model_check.ml`, `smt/theories/{dt,arr}.ml`: `Model.Int`
  construction/compare in `Bigint`; the prior >int63 → `Uninterp`/`Bad` degrades collapse
  to keeping the exact value.
- Tests updated to construct/compare `Model.Int` in `Bigint`.

Nothing else on the frozen surface. `model_check` (the R1 UF checker) was ALREADY Bigint
(it consumes `Cdclt.VInt`), so the TCB evaluator is unchanged.

**Acceptance battery (replaces byte-identity; status at freeze):** make test green
(check-frozen at the NEW hash); the 6 Certora `Rational.Overflow` unknowns no longer
overflow — 2 convert to R1-checked sat, the other 4 no longer degrade but now require
arbitrary-precision B&B SEARCH on 2^256-range values (a separate perf frontier, NOT a
representation bug); a reduced RED golden carrying a forced 2^256 model value; a 0-flip A/B
over QF_UFLIA 659 + a LIA regression sample; an SMPT wall/alloc check for the boxing cost
of `Int of Bigint` (named perf risk — build_model is 83–96% of SMPT's 2s budget).

**BLOCKING dual review on the frozen-surface diff** (model_check consumes `Model.value`,
so this is TCB): codex leg per ADR-0007 + fable leg.

### A14 — Tranche-C unfreeze: cert emitter knob for base-l0 level-0-unit declarations (2026-07-15)

`sat.mli` grows one optional argument on `create`,
`?base_l0_cert_mode:bool -> unit -> t` (default `false`), and `FROZEN.sha256` is
regenerated to match. Same additive shape as A8–A11: none of the 14 frozen DECLARATIONS'
bodies change — `create` gains a defaulted label, so every existing `Sat.create ()` caller
and the raw-SAT test fixtures are source- and behaviour-identical. The value is a PURE
CERTIFICATE-EMITTER mode bit. It is never read by search — verdicts, models, and the
conflicts/decisions/propagations counters are bit-identical in both states. Default `false`
keeps every emitter behaviour byte-identical to the pre-#53 build.

ONE FLAG, TWO COUPLED CERT BEHAVIOURS (codex #53 bounce fix). Passing `true`
(session-side, under `OXSMT_BASE_L0`) drives both together, so the OFF path is
trunk-identical BY CONSTRUCTION rather than by a reachability argument:
1. `add_clause` SUPPRESSES the redundant `on_unit` level-0-unit DECLARATION. Those
   declarations are VERIFIED-not-trusted by the checker (checker.ml (b) requires each
   declared level-0 unit to be BCP-entailed by the inputs) and redundant (the checker
   re-derives every level-0 unit from the raw `Input` clause).
2. a level-0 THEORY conflict concludes via an empty-core E3 `Failed_assumption
   { antecedents = [] }` rather than E2 `Level0_conflict`. (A theory conflict clause
   self-propagates when added to the checker closure, so E2's `falsified` test cannot see
   it; E3's `refutes_under` over the whole DB derives ⊥ by construction — the same way the
   pre-base-l0 build's base ASSUMPTION made these E3.)

Both address the same hazard: under base-frame-at-level-0 the base frame's clauses are
unguarded level-0 inputs, so a base-frame input unit that a level-0 theory conflict
RETRACTS in the checker's (legitimately contradictory) closure would spuriously fail check
(b) even though the whole-DB E3 refutation is valid.

The E3-route change is gated on the SAME `base_l0_cert_mode` bit (codex #53 bounce
remedy): strict OFF keeps a level-0 theory conflict on the pre-existing E2 route, so raw
SAT is byte-identical to trunk in the OFF state (verified by `cert_emit_test`
`test_base_l0_e3_gate`: OFF ⇒ E2, ON ⇒ E3 at a directly-constructed level-0 theory
conflict). Boolean level-0 conflicts stay E2 in both states, so the non-base certs are
untouched. Soundness of the E3 route is gated by the checker's existing `refutes_under`
(unchanged) — a bogus theory conflict over a non-refuting DB is still rejected
(checker_test `bogus_theory_conflict_empty_core_e3`).

**BLOCKING dual review on the frozen-surface diff** (`create` is a TCB constructor): codex
leg per ADR-0007 + fable leg. Acceptance: cert-corpus-gate 33/33 VALID with `OXSMT_BASE_L0=1`
AND 33/33 with it off; full gate suite green in BOTH flag states; the bogus-theory-conflict
RED rejected; the raw-Sat gate-pin (OFF=E2 / ON=E3) green.
### A15 — Reset-per-query theory invalidation (task #54, contract-A) (2026-07-15)

(A14 is reserved by the in-flight #53 lane — the `sat.mli` `emit_level0_unit_decls`
unfreeze; this addendum takes A15 to avoid the collision. If #53 does not land first, the
master renumbers whichever lands second.)

**Not a frozen-interface change** (`smt/interface/{session,cdclt}.mli` are theory-facing,
not among the 14 hash-frozen core signatures; `check-frozen` stays 14/14). Additive to the
session lifecycle; the single-query / corpus path is byte-identical (verified: full 8700
QF_DT 0-flip + 62/62 cross-logic).

**Problem.** The combined/standalone theory is chosen lazily at the first theory-atom
intern (`Cdclt.ensure_theory`) and cached in `Cdclt.t.theory` for the session's lifetime;
it is never reset when the datatype/array registry is REPLACED for a later query. A batched
refinement-type VC workload reuses ONE Session (each VC declares its own datatypes, checked
under push/pop). Three degrade patterns resulted: (1) loader overwrite — each VC's
`set_datatypes` replaces the registry; (2) none→DT — an early pure-logic VC caches the
EUF+LIA stack, a later VC declares a datatype it cannot serve; (3) DT→arrays. The #51
interim guard fail-CLOSED all three to `unknown` (and, before it, the by-ref read produced a
wrong `unsat` when a re-used symbol changed datatype role — the session-lifetime
`ctor_terms`/`seen_cat` of the cached `Dt.t` met a differently-populated registry).

**Fix.** On a registry mutation (`set_datatypes` / `set_arrays` / `declare_datatype`) after
a theory is already instantiated, and only when the mutation actually involves datatypes /
arrays (a pure-logic `set_datatypes empty` is a no-op — the batched pure-logic path stays
byte-identical), INVALIDATE the cached theory: `Cdclt.reset_for_new_query` drops the theory
instance and clears the SAT-var↔theory-atom bijection (`t2v`/`v2a`/`v2term`/`a2v`/`is_split`/
`subterms`); `Session` clears its per-query term→var maps (`prop_to_var`, `bool_consts`) and
last-verdict/model/poison state. The next intern rebuilds the theory fresh from the new
registry and re-interns every (possibly re-used) term against it — so no stale
classification can survive, and the discarded `Dt.t`'s `ctor_terms` dissolve the #51
wrong-`unsat` landmine.

**What survives a reset, and why it is sound.** The `Env` (symbol declarations), `Context`
(hash-consing), the shared registry refs, the `Sat.t` core, the atom allocator, and the
effort budget survive. The prior (already-popped) query's SAT vars/clauses stay allocated
but INERT: their frame selector is free to be false (trivially satisfiable), and they are
absent from the cleared bijection so `on_assign` ignores them; re-interned terms mint fresh
vars that never collide. `sat.mli` is frozen and offers no clause-drop primitive, so the
core is not recreated — the inert-clause accumulation is identical to the pre-existing
selector-based push/pop frame model (no new leak).

**Fail-LOUD above base (the contract).** Resetting is sound only BETWEEN self-contained
queries: with live assertions active (`asserted <> []`, i.e. no `pop` since the last
`check_sat`) the cached theory holds in-flight atoms bound to the bijection that would be
dropped — resetting would strand them (the #51 wrong-answer path). So a registry replacement
attempted with live assertions raises a documented `Invalid_argument` rather than silently
resetting under live state or silently rebuilding. The self-contained-VC pattern (declare →
assert → check → pop) always reaches the reset with `asserted = []`.

**Removes** the #51 interim non-monotonicity guard (`session.ml` `set_datatypes`/`set_arrays`
`if Cdclt.theory_instantiated → degraded`), replacing fail-closed-to-`unknown` with the
correct verdict at base + fail-loud above base.

**Acceptance.** `tests/solver/dt_multi_query_gate.ml`: none→DT, loader overwrite-rerank (the
codex/fable CRITICAL, kept spec'd "must-not-be-unsat" as a world-independent standing gate),
DT-guard-isolated overwrite, and disjoint overwrite all now REQUIRED-green (sat); a new
fail-loud RED asserts the live-assertion replacement raises. Discrimination: neutering the
reset reproduces all five failures (two wrong-`unsat`, two `unknown`, one missing raise).
`make test` / `check-frozen` (14/14) / `dt-sat-gate` / `dt_test` EXIT 0.
