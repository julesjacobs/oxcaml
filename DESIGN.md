# OxCaml Refinement-Type SMT Solver — Design

Status: draft for review · 2026-07-10 · jujacobs + AIDE

**How to read this document**: everything here is a means to one end — a
sound, fast, pure-OxCaml SMT solver for refinement-type VCs. It mixes
*decisions* (architectural principles, staging, correctness strategy, argued
with some care) with *sketches* (code shapes, file names, formats,
thresholds — illustrative only). Treat decisions as strong defaults and
sketches as starting points for your own design pass; neither is law. When
following the document conflicts with the goal, the goal wins — amend the
document (a short ADR is enough) rather than comply with it.

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

**Decided principles** (these are commitments):

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
(ADR + adversarial review before the freeze), with real open questions this
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
normalization invariants, which is precisely why they get a considered design
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
stops *searching* (`grind`) and merely *checks* — fast, complete, and immune
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
   - Nightly aggregation into `STATUS.md`: counter and wall-clock
     distributions, top-k slowest goals, trend alerts on drift. Refinement
     VCs are expected to solve in low milliseconds; outliers are flagged for
     the master, who decides whether they warrant a task.
   - An **adversarial perf corpus** grown deliberately (EUF diamond chains,
     dense simplex instances, large `ite` trees) so cliffs surface in CI
     rather than in the first real codebase.
   - **The dev loop itself is monitored the same way**: PR-suite wall-clock,
     cache hit rate, slowest tests, triage/nightly queue depths, and
     time-from-dispatch-to-merge all land in `STATUS.md`. Latency rot has no
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

## 10. Agent workflow (zero-human-review operating model)

Organizing principle: **trust comes from agent review, oracles, redundancy,
and external certification — what is absent is *human* review, not review.**
Agent code review is genuinely strong and is the first line of defense; it
catches what oracles cannot: design quality, spec conformance, invariant
reasoning, weaknesses on rarely-exercised paths, and suspicious test
changes. Its one structural limitation is correlation — reviewer and author
share a base model and can share blind spots — which is why review verdicts
are calibrated (honeypot PRs, below) and why the external gate, the only
fully uncorrelated judge, retains final authority on soundness. The project
is unusually suited to that backstop: it is maximally self-checkable and
differential-testable.

**Against incorrectness:**

- **Oracle-first ordering**: no module starts until its independent oracle
  exists. Agent-written tests never count as the gate — code and tests from
  the same session share blind spots. The gate is external: Lean kernel
  certification, pre-labeled public benchmarks, model evaluation, explanation
  checkers.
- **The gate is write-protected from child agents.** Oracle code, the Lean
  encoder, test corpora, frozen interfaces, and CI config live in paths child
  agents cannot modify — enforced mechanically (diff checks in CI,
  master-only merges for those paths), not by convention. Children may not
  edit tests/oracles in the same PR as code; a test a child believes is wrong
  becomes an issue for master adjudication. This is the primary defense
  against test-gaming, the top behavioral risk under "make CI green"
  pressure.
- **Mutation testing is the review-substitute for oracle quality.** Routinely
  inject seeded faults (flip a simplex comparison, drop a congruence merge,
  omit an explanation premise) and require the tiered suite catches them. A
  surviving mutant halts feature work on that module until the oracle is
  strengthened.
- **Honeypot the pipeline continuously.** Nightly, feed the gate known-wrong
  verdicts and seeded-mutant code and require it goes red — a green gate that
  hasn't recently proven it can go red is unaudited. This is also the answer
  to "nothing gates the gate" during M0: the encoder is validated by claims
  that must fail. The same trick calibrates reviewers: occasionally hand a
  review agent a PR with a known injected bug; approval means the review
  process is broken and gets fixed before further approvals are trusted.
- **N-version checkers**: self-checkers (model evaluator, explanation
  verifiers) are written by separate agent sessions from spec only, with no
  access to solver internals, to reduce common-mode blind spots. The Lean
  encoder deserves the same treatment: it is trusted translation code.

**Against slowness and design ping-pong:**

- **Tiered CI budget, fixed upfront**: unit suite (seconds, every build) →
  curated corpus + fixed-seed fuzz + Lean-certification of cache misses
  within a fixed budget (minutes, every PR) → full benchmarks, continuous
  fuzzing, and the Lean triage/timeout queue (nightly, off the merge path).
- **Spec-by-citation**: each module implements a named paper/algorithm
  (Dutertre–de Moura simplex, Nieuwenhuis–Oliveras union-find, MiniSat core)
  — one decision each, made once. Agents implement from spec + acceptance
  criteria (benchmark sets that must pass, perf envelope, size budget); they
  do not co-design.
- **Mechanical freezes**: frozen interface files are hash-checked in CI;
  changes require an explicit unfreeze marker plus an adversarial review pass
  by a fresh agent with an attack brief (grill-me style), since no human will
  review them.

**Against design rot:**

- **Modules stay small enough to rewrite.** With real oracles, a rotten module
  is rewritten from its spec rather than patched — the payoff of oracle-heavy
  design.
- Tripwires: per-module line budgets (a CDCL core at 6k lines is a smell), no
  new abstractions without a written case, periodic consolidation passes.
- An `smt/AGENTS.md` carries conventions, invariants, and the frozen-interface
  list, so every agent session starts with the same constitution.

**Residual trusted computing base (stated honestly):** term smart
constructors, the check-sat glue, the SMT-LIB printer, the Lean encoder, and —
outside this project — VC generation in the refinement checker. These are
validated only by round-trip and end-to-end behavior until certificate replay
arrives; the encoder additionally by N-version implementation.

## 11. Orchestration model

A master agent (long-horizon planner) orchestrates child agents (task
executors) with git as the integration fabric. The master's context window is
a scarce resource and is treated as such.

**Means, not ends.** The goal is a sound, fast solver — every rule, map, and
metric below is instrumental, and the constitution says so in its opening
lines. Goal displacement (a healthy-looking process wrapped around a stagnant
product) is a named failure mode: `STATUS.md` leads with **outcome metrics**
(milestone, pass rates per logic, corpus solved-rate, days since last
outcome improvement) before any process metrics, and the master's loop
includes a periodic ball-check — does the task queue trace back to milestone
progress, or has recent work been mostly process grooming? The master has
explicit authority to amend process rules via ADR when they stop serving the
goal; rules are subject to the same consolidation-beats-accretion discipline
as everything else.

**Tools are context-frugal by default.** Every internal tool (test runner,
bench runner, gate harness) writes full detail to files and prints a digest —
counts, top-k outliers, first few failures with paths to full logs —
verbosity strictly opt-in. A tool that floods an agent's context with
thousands of lines is a defect of the same severity as a slow test suite,
and for the same reason: it silently degrades every agent that touches it.

**The master reads maps, not territory.** As a default the master does not
read large files, run commands with large output, or write code — its context
is reserved for planning, spec adjudication, merge decisions, and
escalations. Questions are answered by dispatching **scout agents** (concise
answers with `file:line` and invariant citations); commands run via **runner
agents** returning structured summaries. These are defaults grounded in
context economics, not rules requiring enforcement — children are as capable
as the master and exercise judgment about what detail matters.

**The master's working set** — a small, fixed set of files loaded at session
start. Compactness of these files is what makes the scheme work, so it is a
real requirement: growth is treated as a defect, consolidation beats
accretion, and the nightly auditor flags bloat alongside drift:

1. `ARCHITECTURE.md`: module DAG, one paragraph per module (responsibility,
   owning task, status), one data-flow diagram.
2. **The frozen `.mli`s** (or a generated `SPINE.md` concatenating them):
   `Sort`, `Term` constructors, `THEORY`, the session API. The master's view
   of the core data types *is* the interface files — compact, compiler-checked
   against the code (cannot drift), and stable across sessions because they
   are hash-frozen (§10).
3. `INVARIANTS.md`: numbered, citable invariants ("I3: any `Term.t` in
   existence is well-sorted"). Specs and child reports cite them by number.
4. **Decision log** (ADRs): append-only with a one-line index; the master
   reads full entries only on demand. Re-opening a logged decision requires
   the adversarial-review ritual — this prevents the master re-litigating
   against its own past self across sessions.
5. `TASKS.md`: the board — status, owner, acceptance criteria, attempt count.
6. `STATUS.md`: **generated by CI, never by an agent** — benchmark pass rates
   per logic, corpus/cache stats, triage-queue depth, per-module line counts
   vs budget. The empirical state of the world cannot be stale or gamed by an
   optimistic child report.

**Keeping maps honest**: a PR changing any `.mli` must include the
corresponding map delta or it does not merge; a nightly **auditor agent**
diffs maps against reality and files drift issues; children must flag
map-mismatches explicitly (**escalation over silence**) rather than silently
adapting — mismatch reports are the master's drift detector.

**Repository topology and custodianship.** Development lives in `~/oxsmt`,
deliberately separate from `~/oxcamls` (which is for real compiler work):

```
oxsmt/
  DESIGN.md      # until it migrates into the branch at M0
  main/          # clone of the personal oxcaml GitHub fork; branch `oxsmt`
  worktrees/     # one git worktree per child task, branched off `oxsmt`
  cache/         # content-addressed Lean oracle cache   (never in git)
  corpora/       # public benchmark sets, fetched once   (never in git)
  logs/          # full tool output; agents see digests, detail lands here
```

- The long-lived `oxsmt` branch on the personal fork is the integration
  trunk; task branches (`oxsmt/task/<name>`) are short-lived, local-only,
  bound 1:1 to a worktree.
- **Authorship and integration are separated.** Task children commit in
  their worktrees and never push. Merges are executed by a dedicated
  **integrator agent** acting on a recorded master approval: rebase/merge
  onto `oxsmt`, re-run the fast suite, push, clean up the worktree and
  branch, report one line back. Trivial conflicts the integrator resolves
  and re-verifies; non-trivial ones bounce to the task owner. The master
  *decides* merges but never executes them — merge mechanics (rebase noise,
  CI logs, conflict diffs) are exactly the context-heavy work it must not
  absorb.
- **Worktree lifecycle = task lifecycle**: created at dispatch, removed (and
  branch deleted) at merge or abandonment; no orphans. Worktrees share the
  object store, so they are cheap.
- **Nothing lives loose in a worktree**: bench logs, build outputs, one-off
  scripts go to `logs/` or a gitignored scratch dir — never untracked files
  scattered through checkouts. This is the default outcome of unsupervised
  agent work; preventing it has an owner, and the owner is the master.
- **Custodial work is delegated too**: worktree pruning, upstream syncs
  (periodically merging upstream oxcaml `main` into `oxsmt` so the branch
  stays mergeable, not just the code compilable), and hygiene sweeps are
  scheduled janitor-agent tasks. The master is responsible for it all not
  becoming a mess, but discharges that responsibility by scheduling and
  monitoring, never by doing.
- Hygiene is monitored, not intended: `STATUS.md` includes live worktrees vs
  active tasks, stale branches, and dirty/orphaned worktree counts; the
  nightly auditor flags them like any other drift.

**Git workflow**: trunk-based; one short-lived branch per task; merge requires
green CI plus a recorded review-agent pass with an attack brief (reviewers
must exhibit evidence — a failing input or an invariant argument — not
opinions); the master approves all merges, the integrator executes them.

**Rebase → test → fast-forward only.** The integrator rebases the task branch
onto the current `oxsmt` tip, runs the full PR suite on the *rebased* head,
and lands it with `--ff-only`: the commit on trunk is bit-identical to the
commit CI tested, so trunk can never contain an untested state — this is the
defense against semantic conflicts, where two independently-green branches
combine into a broken trunk. If trunk moves meanwhile, re-rebase and re-test;
integration serializes through the integrator's queue. Corollary: history is
linear and every commit was green, which makes `git bisect` a perfectly
mechanical debugging tool — the kind agents wield best. The one exception is
the upstream sync (necessarily a merge commit): there the underlying
principle still applies — the merge is built and fully tested locally and
pushed only if green.

Branches older than ~a day of agent work are re-scoped, not endlessly
rebased. Module ownership keeps parallel children out of each other's files;
shared-interface changes serialize through the master's unfreeze ritual.

**Child task protocol**: each task ships with a spec file (spec-by-citation,
acceptance criteria, non-goals) — children are pointed at spec files, never
at prose the master paraphrases from memory. Completion reports are concise
and structured: what changed, interfaces touched (normally "none"),
invariants affected, map deltas, test evidence, open questions. Repeated
failure on a task is a signal for the master to re-scope it (smaller task,
better spec, different decomposition) rather than re-prompt the same task.

**Predicted failure modes this design targets** (from simulating the plan):

- *Test-gaming under green-CI pressure* (M2–M3, the LIA long tail) → gate
  write-protection, honeypots, reviewer calibration.
- *A quietly-broken gate during M0*, when the oracle infrastructure is being
  built and nothing gates it → honeypots from day one; M0 built slowly with
  N-version encoders.
- *Local workarounds for other modules' bugs* (M4 combination debugging) →
  workaround-requires-issue rule, consolidation passes.
- *Heisenbugs outlasting agent attention spans* → hard determinism
  requirement (fixed seeds, no wall-clock heuristics), decision-trail
  logging with replay, first-class shrinker: convert the debugging style
  agents are worst at into the one they are best at (small deterministic
  repros).
- *Master context exhaustion and cross-session amnesia* → the working-set
  files are the master's externalized memory; the repo, not any
  conversation, is the source of truth.
- *Correlated blind spots* (all agents share one base model, so N-versioning
  is weaker than with humans) → the only truly uncorrelated judges are
  Lean's kernel, pre-labeled benchmarks, and evaluation-based model checking;
  this is why the external gate outranks every other mechanism.

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

### A1 — Async review pipelining (2026-07-11, design author)

Amends the §11 git workflow. Agents produce **"PR" branches**; reviews and
test runs happen **independently and speculatively** against pinned shas —
multiple review rounds and suite runs per branch proceed in parallel with
each other and with other lanes, and the integrator **pre-rebases and
pre-tests** queued branches before final verdicts land, so landing is
instant on approval. **Rebasing triggers a re-run.**

The load-bearing invariant is unchanged: trunk stays linear, and every
landed commit was fully reviewed *and* fully tested at its exact rebased
sha (rebase → test → ff-only). Async here means *decoupled and
speculative*, not post-merge: blocking gates (TCB codex passes, soundness
verdicts) still gate landing; only the §10 trailing cross-model reviewer
reviews post-merge. Rationale: (a) more parallelism and speculation for
the same serial merge discipline; (b) more review rounds per branch per
unit wall-clock. Sha-pinned dispatch and frozen tips remain the
coordination primitives that make speculation safe. (In-repo ADR to
follow in `decisions/`.)

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

### A3 — Agent context economics and rotation (2026-07-12, design author)

Agents run on a 1M-token context window, but context is not free: near the
limit a single tool call costs on the order of a dollar (the full window is
re-read on every cache miss), and reasoning quality degrades as the window
fills. Auto-compaction rescues a stuck agent but is lossy and uncontrolled.
The fleet therefore treats context as a budget to be spent deliberately:

- **Default: a fresh agent per task.** Spawn new agents for new tasks
  unless the incumbent's accumulated context is *definitely* useful for
  the specific dispatch — e.g. a builder mid-way through the very arc in
  question, or a specialist whose load-bearing knowledge is not yet
  written down. "Vaguely familiar with the area" does not qualify;
  on-disk artifacts (ADRs, logs/ memos, probe plans, design notes,
  runbooks) are the durable memory, and the habit of writing everything
  down is precisely what makes fresh spawns cheap.
- **Rotation thresholds by role.** General ceiling: do not dispatch new
  work to an agent past ~50% of the window; rotate at the next task
  boundary. Roles whose history carries little forward value get much
  tighter budgets — e.g. a codex-driver (whose job is mediating scoped
  external-review sessions) should not run past ~20%; spawn a fresh
  driver per review arc. Long-lived coordination roles (integrator)
  rotate via an explicit handoff runbook committed to logs/.
- **Never interrupt mid-task to rotate** — mid-task compaction is
  survivable; rotate at boundaries. Never resume a dormant high-fill
  agent for new work.
- **Monitoring:** `check-agent-context.py` (repo root, outside the
  reviewed tree) reads each subagent transcript's latest usage entry and
  flags ROTATE ≥50% / CRITICAL ≥80%; the orchestrator runs it
  periodically and plans rotations from it.
- **Sizing dispatches:** a task should fit in the agent's remaining
  headroom without crossing the ceiling; if it can't, split the task or
  start fresh.

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
under-inclusion mutant. *Lesson (second occurrence of this shape, for
lessons.md): when a fix arrives as an on/off switch, look for the
proportionality rule it's approximating. Switches have cliffs and
stale-state hazards; structural cost-proportionality has neither.*

### A5 — Freezing is not an end in itself (2026-07-12, design author)

An interface freeze exists to stabilize load-bearing surfaces against
churn and drive-by edits — it is a tool, not a goal. A Term unfreeze (or
any §10 unfreeze) is fine when it leads to a better design; run the
ritual and take the better design. Concretely: when a design contorts
itself to stay off a frozen surface (encodings, side-channels, reserved
namespaces standing in for what a type should express), the comparison
must be argued on design merits — correctness-by-construction,
enforceability, extensibility — with freeze-avoidance carrying no weight
of its own. If the frozen-surface version wins on merits, unfreeze.

### A6 — Gates gate; sweeps attribute (2026-07-12, design author)

Long corpus sweeps must not sit on the land critical path. The merge
gates are the fast, binary checks: build, test suites, mutants,
frozen-interface check, formatter, defensive gate. The full-corpus sweep
is *attribution* — it tells us what a land bought, not whether it may
land — so it runs as a follow-up job, concurrent with whatever the box
is doing, and its results arrive as a STATUS amendment rather than
inside the land chore. This became safe the moment counted-effort became
the primary measurement term: counted effort is load-immune by
construction, so sweeps no longer need the box to themselves and no
longer need to serialize against builds or each other (wall numbers stay
secondary/informational). The same reasoning extends downstream: a
successor task may branch speculatively off an approved frozen tip
before the fast-forward completes, since FF-only guarantees trunk equals
that sha; a bounced land costs one rebase, which the velocity directive
prices as acceptable.

Two hard edges survive the decoupling. First, the mismatch tripwire is
not measurement: if a post-land sweep reports MISMATCH>0 or a
wrong-direction surprise, trunk stops advancing until it is explained —
the sweep is off the critical path, the soundness signal it carries is
not. Second, provenance discipline is unchanged: sweeps still run
release-config binaries with stamped build_commit/dirty checks, and
baseline promotion stays fail-closed. *Lesson: when a measurement
protocol becomes load-immune, re-examine every place it was being
serialized — the serialization was compensating for a fragility that no
longer exists.*
