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
