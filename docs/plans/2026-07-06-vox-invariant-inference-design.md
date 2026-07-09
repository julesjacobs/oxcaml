# vox: invariant and annotation inference — design options

Status: design study (no implementation). Author: invariant-inference quest,
2026-07-06. Reference tree read: `vox-editor` @ `vox-proof-pane`
(tip `c60ad6e2f`). Clone for local commits: `vox-infer`.

This is the "CHC invariant inference" item on vox's future-work list. Today
every `while`/`for` loop needs a hand-stated `[@vox.invariant]`, every
recursive helper needs its loop invariant written by hand as parameter/result
refinements, and array/heap loops additionally need one hand-proved step lemma
each. The prize: infer them. The vox twist that shapes every option: whatever
proposes an invariant, the invariant is *re-proved through the normal Lean
pipeline*, so inference adds **zero TCB** — a buggy proposer can only fail to
verify, never verify a falsehood.

---

## 1. Where invariants live today (ground truth from the tree)

### 1a. Two annotation surfaces, one problem

**`while`/`for` loops** carry `[@vox.invariant p]` — a *formula over program
variables*, not a refinement type (`typing/vox_verify.ml:1159`). It elaborates
in the logical environment (`elab_loop_invariant`, `:1201`), is never compared
or propagated, and at each loop boundary is instantiated by closing every
mutable mention over that variable's current SSA version
(`close_over_versions`, `:1191`). Example (`demo/lean_imp_binsearch.ml`,
`search2`):

```
[@vox.invariant
   -1 <= lo && lo < hi && hi <= Iarray.length a
   && (lo = -1 || Iarray.get a lo < x)
   && (hi = Iarray.length a || Iarray.get a hi >= x)]
```

**Recursive helpers** carry the invariant as *contracts on their parameters*
(refinement types), assumed in the body and discharged at each call. This is
the more common demo style. Example (`demo/lean_isqrt.ml`):

```
let rec go (lo : int{ 0 <= _ && sq _ <= x })
           (hi : int{ lo < _ && x < sq _ })
  : {r:int | 0 <= r && sq r <= x && x < sq (r + 1)} = ...
```

Both are the same inference target: find the inductive relation that holds at
every loop head / recursive entry and is strong enough to prove the exit
obligation. The recursive-contract surface is *easier to target first* because
it reuses the ordinary contract-VC path (call sites prove the callee
precondition; the body proves the result) with no loop-specific machinery.

### 1b. The VC discipline is a Hoare quadruple (the CHC skeleton)

For `Texp_while` (`vox_verify.ml:3096`) with invariant `I`:

| edge | what the compiler emits | Horn clause (with `I` an unknown predicate) |
|---|---|---|
| entry | `emit_vc goal=I` over entry versions | `pre(x) ⇒ I(x)` |
| head | havoc written mutables; **assume** `I` + declared refinements | (defines the head state) |
| body | walk under reflected guard fact | |
| in-body obligations | `emit_vc goal=safety` under `I ∧ guard` | `I(x) ∧ guard(x) ∧ step ⇒ safe` |
| back-edge | `emit_vc goal=I` over body-exit versions | `I(x) ∧ guard(x) ∧ step(x,x') ⇒ I(x')` |
| post | continuation sees head state **+ negated guard** | `I(x) ∧ ¬guard(x) ⇒ post(x)` |

`Texp_for` (`:3150`) is the same with an index-aware instantiation (`at_index`
`First`/`Next`/`Past`, `:3184`). **This is a CHC system a rename away**: promote
`I` from a concrete `Refinement.pred` to an uninterpreted predicate symbol over
the loop's live variables, and the entry / inductive / safety / exit VCs *are*
the Horn clauses. The inference problem is: solve for `I`.

### 1c. The McCarthy lesson — the deep tension

Finding the invariant *predicate* is only half the job. Discharging its
inductiveness (`back-edge`) is the other half, and for array/heap loops grind
cannot do it alone even when `I` is exactly right. `demo/lean_reverse.ml`
carries a correct invariant `revinv` but needs a hand-proved
`revinv_step` theorem "all variables bound by its conclusion" so grind's
automatic pattern applies and discharge becomes ground congruence. Quantified
per-call frame conditions do **not** scale — grind cannot instantiate
forall-facts at goal indices. So the target splits:

- **linear-arith / measure loops** (isqrt, summation, binsearch, index
  bounds): once `I` is right, grind + omega close inductiveness automatically.
  Inference of the *predicate* is the whole job.
- **array / heap / permutation loops** (reverse, qsort partition, array-fill
  with reordering): even the right `I` needs a *structured proof*. Inference
  must produce both a predicate **and** a proof obligation someone (grind, an
  auto-emitted `[@@vox.lemma]`, or a human) closes.

Every option below is honest about which half it solves.

**Empirical anchor (probed 2026-07-06).** Compiling `demo/lean_reverse.ml`
directly with the correct `revinv` invariant *present* but `revinv_step`
*deleted* fails at the back-edge:

```
Error: vox: verification failed -- NOT PROVED (automation gave up; no
counterexample was found ...).  Goal: revinv b b5 i
(lean: error: `grind` failed)
```

Two facts this pins down: (1) even the exactly-right Tier-2 predicate is *not*
grind-dischargeable — the step lemma is load-bearing; (2) the disproof oracle
found **no counterexample** because the array sort is opaque `VoxU` (§2.2's
`sort_evaluable` returns `[]`), confirming the oracle's blind spot on heap
loops. The unmodified file verifies (exit 0). This is the whole Tier-1 /
Tier-2 split, measured.

---

## 2. Reusable assets an inference loop can stand on

1. **The VC stream** (`emit_vc` `:1822`; `vc = { vc_facts : pred list;
   vc_goal : pred; vc_kind }` `:26`). Each VC is literally `facts ⊢ goal` — a
   Horn clause body ⊢ head. `emit_vc` already gathers scoped facts, needed
   definitional equations, and by-need global (`.cmi`) facts. An inference pass
   consumes exactly this stream.

2. **The disproof / counterexample validator** (`:5429`–`:5850`). Given a
   failed VC it enumerates ground values for the binders (`enum_sort` `:5557`:
   an int pool seeded by grind's model + a small symmetric spread; datatypes to
   depth 2), builds `wc_N` theorems asserting `hyps ∧ ¬goal`, and asks Lean
   (`decide`/`grind`) to *validate* the assignment (`validate_witness`
   `:5792`). A validated assignment is a genuine counterexample. **Crucial
   limit**: it only fires on *evaluable* sorts — `Int`, `bool`, and datatypes
   built from them (`sort_evaluable` `:5460`); opaque `VoxU`, arrays, poly, and
   ghost sorts return `[]`. This is the ready-made **counterexample oracle** for
   ICE / CEGAR and the **fast filter** for Houdini — but only over evaluable
   sorts (so it filters isqrt/summation candidates, *not* array-frame ones).

3. **`[@@vox.lemma]`** (`:4233`). An ordinary recursive function whose refined
   result is a Prop over its parameters is emitted as a Lean theorem
   `forall params, contracts -> Q`, **re-proved** by structural / functional
   induction + grind, and registered as an ambient `@[grind]` fact. Soundness
   is Lean's (the `*_fail` companions demonstrate a false lemma is rejected).
   **This is the propose/dispose vehicle**: an inferred fact — a step lemma or a
   whole invariant lemma — can be emitted through this path and re-proved, with
   the proposer entirely outside the TCB.

4. **`grind?` used-lemmas** (`-vox-explain-proofs`, `:5136`, `:5861`,
   `used_lemmas` `:239`). A second pass swaps `grind` for `grind?` and harvests
   which facts closed each goal. This is a **relevance signal**: which
   candidate qualifiers actually mattered — usable to rank/seed the candidate
   set (property-directed flavour).

5. **The editor as-you-type pipeline**. There is already a *fast no-Lean
   `/check` pass* with verdict carryover and a proof-state pane showing scope
   variables and facts at any cursor (tasks #73, #74; `vc_index.py`,
   `server.py`, `app.js`). This is the natural **surface** for inferred
   annotations: ghost text ("try invariant: …") in the pane, confirmed by the
   full Lean pass.

---

## 3. Solver-on-the-box probe (decisive for option 3)

Swept PATH, the nix store, and the opam switch:

- **Present**: Lean `4.31.0` at
  `/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean` (the
  pinned copy `has-lean.sh` finds). `lake`, `leanc`, `leanchecker` alongside.
- **Absent**: `z3`, `cvc5`, `cvc4`, `yices`, `boolector`, `eldarica`, `hoice`
  — nothing on PATH, nothing in `/nix/store/*`, nothing in the opam bin.

**Consequence**: any option that *ships* invariants to an external SMT/CHC
solver (Spacer, Eldarica, HoIce) is **design-only** on today's box — it would
require adding a solver to the nix closure first. Every *deployable* option must
use the tools already present: Lean/grind as the checker, and the disproof
evaluator (also Lean) as the oracle. This is not a small constraint; it is the
single biggest discriminator among the options and it pushes the recommendation
firmly toward Lean-native engines.

---

## 4. The candidate language (qualifier alphabet)

The vox-native move: **the qualifier alphabet is the spec vocabulary the module
already wrote.** Mined from three places — (i) the loop's live variables and
their sorts; (ii) every in-scope reflected/`total_` measure and imported spec
function; (iii) the target obligation's own atoms (the declared result
refinement, decomposed — the most relevant candidates, and what `grind?`
relevance and property-directed reasoning both point at). Measures actually
present in the tree today: `len` (×17), `rev`/`rev_append`, `append`/`app`,
`mem`, `sorted`, `ord`, `depth`, `dmin`, `sq`, `fib`, plus the McCarthy array
theory `len`/`elem`/`upd`.

Three tiers, increasing cost and decreasing grind-dischargeability:

- **Tier 0 — arithmetic / octagon.** For int-sorted live variables and bounds
  `x, y` and constants `c ∈ {-1,0,1}`: `0 ≤ x`, `x ≤ y`, `x < y`, `x = y`,
  `x ± y ≤ c`, and bracket relations `lo ≤ x ≤ hi`. Covers isqrt bounds,
  summation index, binsearch bracket, array-fill index. Grind/omega closes
  inductiveness once the conjunction is right.
- **Tier 1 — measures over program data.** Instantiate each in-scope measure
  `m` at in-scope variables and compare to int variables or to each other:
  `m(a) = x`, `x ≤ m(a)`, `m(a) = m(b)`, `sorted(a)`, `mem(v,a)`. The
  vox-native core; `sq lo ≤ x` for isqrt lives here. Usually
  grind-dischargeable (the measure is reflected, so it computes).
- **Tier 2 — array / McCarthy frame.** Quantified templates
  `∀k. lo ≤ k < hi → elem a k = e(k)`, with `e` from a tiny set of index
  expressions (`elem b k`, `elem b (len b − 1 − k)`, a constant). These are the
  reverse/array-fill invariants. **Not grind-dischargeable without a step
  lemma** — Tier 2 candidates come paired with an auto-emitted `[@@vox.lemma]`
  proof obligation.

---

## 5. The TCB=0 checking loop (identical across all engines)

This is the load-bearing invariant of the whole design and must be spelled out
because it is what makes even an LLM proposer sound:

1. The engine proposes a candidate `I` (a conjunction, or DNF, of qualifiers).
2. The compiler **installs `I` exactly where the hand-written annotation would
   go** — as the `[@vox.invariant]` template, or as the recursive helper's
   parameter/result refinement — and runs the *unchanged* `emit_vc` path. No
   new trusted code: `I` flows through the same entry / back-edge / safety /
   exit VC generation as a human annotation.
3. Those VCs go through the **normal Lean/grind pipeline**. If all pass, `I` is
   both inductive and sufficient, and it is exactly as trustworthy as a
   hand-written invariant — Lean checked it. **The engine is outside the TCB.**
4. Fast pre-filter (optional, before step 3): run the *disproof evaluator* on
   the candidate's entry/back-edge/exit VCs over evaluable sorts. A validated
   counterexample kills the candidate with no Lean proof call; survivors go to
   Lean. (Only fires for Tier 0/1 over evaluable sorts.)
5. Tier-2 fallback: if the back-edge VC fails but `I` is genuinely inductive
   (grind too weak — the McCarthy case), emit a candidate step lemma as a
   `[@@vox.lemma]` and let the lemma pipeline's structural/functional induction
   attempt the re-proof. If that also fails, report "invariant found; proof
   obligation X remains" and surface it in the editor for a human to finish.
   Still zero TCB.

The differentiator among options is therefore **convergence and automation
cost, never soundness.**

---

## 6. Design options

### 6a. The fit question: Liquid inference vs vox's eager-VC / HM architecture (the crux)

Liquid Haskell presupposes a **constraint-based** checker: subtyping is not
decided on the spot; it is *deferred* into a global Horn system over `κ`
unknowns that a fixpoint solver closes afterward. vox does the opposite —
refinements ride HM unification with binder pairing, and each obligation is
discharged **eagerly** as a per-site VC (`facts ⊢ goal`) handed to Lean/grind.
There is no accumulated constraint graph and no notion of an unknown refinement.
So "does Liquid inference fit?" splits cleanly:

**(a) Lightweight Liquid WITHOUT the constraint solver — YES, HM-compatible
today.** The key realization: vox's per-site VC stream *already is* the Horn
constraint set, just discharged instead of accumulated. Introduce a `κ`
placeholder at each site where vox currently demands a known refinement
(unannotated `let` binders, loop targets, branch joins). Then:

- every VC where `κ(x̄)` appears among the **hypotheses** is the Horn clause
  `env ∧ facts ∧ κ(x̄) ⇒ goal` — "`κ` strong enough";
- every VC whose **goal** is `κ(x̄)` is the clause `env ∧ facts ⇒ κ(x̄)` — "`κ`
  weak enough."

This is exactly the entry/back-edge/use structure the loop machinery (§1b)
already builds, so a loop invariant is just the `κ` at the loop-head join —
lightweight Liquid is the *generalization* of machinery vox already has. Solve
it by the propose-then-re-prove loop (§5) with Liquid's qualifier lattice as the
candidate generator: seed each `κ` with the full qualifier conjunction, run the
normal VC/grind pipeline, and Houdini-weaken on each refuted weak-enough clause.
No dedicated fixpoint engine, no Horn-clause solver — the "solver" is grind, the
"constraint set" is the VC stream, the "abstract domain" is the qualifier
conjunction. The cost of doing it this way: each weakening step re-runs Lean
rather than propagating in a cheap in-memory lattice, so it is O(qualifiers ×
κs) Lean rounds — fine for a handful of `κ`s (one loop, a few binders; seconds),
expensive for a whole module of dozens of join points. The *scope limit*: this
handles acyclic `κ` dependencies and the single-loop cycle (which the loop
encoding closes by construction); **mutually-recursive `κ`s** (a join feeding a
loop feeding the join, several unknowns referring to each other) need all their
clauses held and solved *simultaneously*, which the eager pipeline does not do.

**(b) Full liquid-fixpoint — subtyping-gated (Option 6).** A real
liquid-fixpoint integration — global Horn system over mutually-dependent `κ`s,
one-shot predicate-abstraction solve, sharing work across `κ`s — presupposes
that refinement checking is recast from "unify + eagerly discharge" to "emit
subtyping edges into a constraint set." That recast **is** the subtyping
migration a sibling quest is designing (`vox-subty`,
`docs/plans/2026-07-06-vox-subtyping-design.md` when it lands). This study does
not design that migration; it states the **interface the inference engine needs
from it** (below). Until that migration exists, full liquid-fixpoint is not
deployable and lightweight Liquid (Option 1) is the path.

**Interface required from the subtyping quest** (a requirement on their
elaboration, not a design of it):
1. For each inference site, a constraint of shape
   `(environment, guard-facts, lhs) <: rhs` where `lhs`/`rhs` may be `κ(x̄)`
   applications — i.e. a Horn clause `env ∧ facts ∧ lhs ⇒ rhs`.
2. The constraints delivered as a **batch** (a traversable set closed over the
   function/module), keyed so the engine can find all clauses where a given `κ`
   is in the antecedent vs the consequent.
3. Each `κ` tagged with its variable environment (its sort / in-scope binders)
   so the qualifier alphabet can be instantiated at it.
4. Acceptance stays proof-carrying: once the engine assigns each `κ`, their
   checker re-emits the now-ground VCs through grind, so the TCB stays 0 (§5) —
   the engine's assignment is a *candidate* they re-check, never a trusted
   result.

**HM-compatible today vs subtyping-gated (summary).** Options 1 (lightweight
Liquid/Houdini), 2 (ICE), 4 (Daikon), 5 (LLM) all run on the *current*
architecture — they only need the `κ` placeholder and the propose-then-re-prove
loop, never a constraint solver. Options 3 (external CHC export) and 6 (full
liquid-fixpoint) are architecture-gated: 3 on a solver in the nix closure, 6 on
the subtyping migration.

### Option 1 — Lightweight Liquid inference (Houdini over a qualifier lattice) on the existing VC stream (RECOMMENDED engine)

This is the Liquid-Haskell recipe *minus* its constraint solver, run on vox's
current eager-VC/HM architecture. Because Liquid inference and Houdini converge
(Liquid inference **is** Houdini over a per-`κ` qualifier assignment), they are
one option; §6a below dissects the architectural fit the mandate calls the crux
and separates what is HM-compatible today from what is subtyping-gated.

**(a) Idea + literature.** Houdini (Flanagan & Leino, 2001): from a fixed
candidate conjunction, iteratively delete each conjunct refuted by a check until
a mutually-consistent fixpoint remains — the strongest conjunctive inductive
invariant expressible in the candidate set; monotone and terminating. Liquid
Types / Liquid Haskell (Rondon–Kawaguchi–Jhala, 2008; Vazou et al.) is exactly
this specialized to refinement types: unannotated positions become unknown
refinement variables (`κ`), type checking emits subtyping edges
`Γ ⊢ {v:b|p} <: {v:b|κ}` that are Horn clauses over the `κ`s, and
*liquid-fixpoint* solves them by predicate abstraction — each `κ` starts as the
conjunction of every in-scope qualifier and Houdini-weakening drops the ones a
clause refutes until fixpoint, yielding the strongest qualifier conjunction per
`κ`. Annotations are needed only where inference is intentionally lost:
top-level signatures and termination metrics. vox *is* a refinement-type system,
so this is the closest fit in the literature; the only question is the plumbing.

**(b) Architecture.** A compile-time pass (behind `-vox-infer`) that runs after
VC generation but before solving. Unannotated inference sites — `let` binders
without a written refinement, loop targets, branch/`match` joins — get a fresh
`κ` placeholder in the refinement AST; the *existing* VC stream (§2.1) is the
constraint set (§6a explains why this works without a separate solver): every
VC where a `κ` sits in the hypotheses is a "`κ` must be strong enough" clause,
every VC whose goal is a `κ` is a "`κ` must be weak enough" clause. The pass
seeds each `κ` with the full qualifier conjunction (§4) instantiated over that
site's in-scope variables, then runs Houdini: batch all VCs under the current
assignment into one Lean file, and for each refuted "weak-enough" clause drop
the offending conjuncts, until fixpoint. Emits each accepted `κ` as a *source
suggestion* (write the refinement back into the buffer / editor ghost text)
and/or an invisible accepted fact that lets compilation proceed. The loop-head
`κ` is the loop invariant, so the existing loop machinery (§1b) is the working
prototype of the general mechanism — generalizing it to let-binders and joins
is the bulk of the work.

**(c) Candidate language.** Tier 0 + Tier 1 (§4); Tier 2 only with the
step-lemma fallback.

**(d) TCB=0 checking.** §5 verbatim. Houdini's per-round refutation check *is*
the back-edge VC (or the disproof evaluator as a pre-filter).

**(e) Automation + convergence.** At most |candidates| rounds (each deletes
≥1); strictly terminating; no divergence. With the disproof pre-filter most
rounds avoid Lean entirely. Cost ≈ (rounds) × (batched Lean call). Incremental
fit: the fast no-Lean editor pass runs the disproof-filter Houdini and shows a
*provisional* invariant instantly; the full pass confirms with Lean — matches
the existing two-pass editor.

**(f) Unlocks (see §7 benchmark).** summation, isqrt, binsearch bracket,
array-fill *index bounds* — everything whose invariant is a conjunction of
Tier-0/1 qualifiers. Plus the Liquid-Haskell usability win: *local refinement
inference* for intermediate `let` binders, so a user annotates only top-level
signatures and the body's refinements are filled in silently. Misses genuinely
disjunctive invariants (needs Option 2) and Tier-2 frames (needs the step-lemma
fallback).

**Liquid-Haskell specifics worth adopting.**
- *User-extensible qualifier file.* LH lets users list domain qualifiers that
  seed every `κ`. vox already has the alphabet for free — the in-scope
  `total_`/reflected measures (§4) — and should also accept an explicit
  `[%%vox.qualifiers {| ... |}]` block for domain qualifiers the measures don't
  cover. This is the concrete home for the "specs-as-qualifiers" idea.
- *Inference invisible until it fails.* When a `κ` resolves, say nothing; when
  it does not, the error must not mention the synthetic `κ` — it degrades to
  "could not infer a refinement for this binder; add an annotation," pointing at
  the site and (via the editor) offering the strongest conjunction that *did*
  survive as a starting suggestion. This UX rule is what keeps lightweight
  inference from producing confusing internal errors.
- *Termination-metric inference* (LH case study). The decreasing metric of a
  recursive function is itself a small inference problem: the metric is almost
  always a measure of a shrinking argument (`len l`, or a numeric parameter that
  decreases on every recursive call). Infer it by trying each in-scope measure /
  numeric argument as the metric and checking the decrease VC through the same
  pipeline. Bounded, high-value, and shared with the totality/termination
  sibling quest (cross-ref §9) — its `has_decreases` handling in the lemma
  translator is the hook.

**(g) Effort + compatibility.** **HM-compatible today** (see §6a): week-scale
for recursive-helper contract inference over Tier 0/1; the only new compiler
surface is the `κ` placeholder and generalizing the loop machinery to
binders/joins. Does *not* require the subtyping migration.

### Option 2 — ICE learning with the disproof enumerator as oracle

**(a) Idea + literature.** ICE (Garg et al., 2014/2016): a learner proposes a
candidate from positive, negative, and *implication* examples; a teacher
(the verifier) checks and returns new examples; iterate. Implication examples
(`if x is in the invariant then x' must be too`) are exactly what a pure
pos/neg learner (Houdini) cannot use, and they are what non-conjunctive
invariants need. HoIce is the ICE-based CHC solver; Sorcar is a
property-directed ICE variant that adaptively restricts the qualifier set to
bound blowup. A decision-tree/DNF learner over the qualifier alphabet is the
natural learner here.

**(b) Architecture.** Same compile-time pass shell as Option 1, but the inner
loop is learner↔teacher. **Teacher = vox's own machinery**: the back-edge/exit
VCs give implication/negative examples; the disproof evaluator (§2.2) *validates
concrete counterexamples* over evaluable sorts, which is precisely the ICE
teacher's job.

**(c) Candidate language.** Same alphabet; the learner assembles *disjunctions*
(decision trees) rather than a single conjunction.

**(d) TCB=0 checking.** §5. The learned DNF is installed as `I` and re-proved;
zero TCB regardless of learner bugs.

**(e) Automation + convergence.** More powerful than Houdini (disjunctive
invariants) but *not* guaranteed to terminate — the learner can oscillate;
bound with a max-round cap and a template-size cap. The disproof oracle only
produces counterexamples over evaluable sorts, so ICE is strong for
Int/bool/datatype loops and *blind* for opaque-array loops (no ground array
witnesses).

**(f) Unlocks.** Everything Option 1 does, plus disjunctive invariants (e.g. a
loop with a sentinel case, or the `lo = -1 || a.(lo) < x` shape in
binsearch that Houdini can only get if the disjunction is a single qualifier).

**(g) Effort.** Quarter-scale (learner + example management + termination
control).

### Option 3 — CHC export to an external solver (Spacer / Eldarica) — DESIGN-ONLY

**(a) Idea + literature.** Translate the VC stream to CHC-Lisp (SMT-LIB Horn)
and hand it to Spacer/Z3-CHC (PDR/IC3 with interpolation) or Eldarica
(CEGAR + predicate abstraction + interpolation). These are the state of the art
for automatic invariant synthesis and would need *no* qualifier alphabet — they
synthesize interpolants directly.

**(b) Architecture.** An offline tool: `emit_vc` → CHC-Lisp → solver → parse the
model (the synthesized `I`) → re-install as a vox annotation → re-prove via §5.

**(c) Candidate language.** None needed (interpolation-based synthesis).

**(d) TCB=0 checking.** Still §5 — the solver's model is *proposed*, then
re-proved by Lean, so even a buggy solver adds no TCB. This is the one place
where importing a heavyweight external solver is *safe*: it never joins the
trust base.

**(e) Automation + convergence.** Best-in-class synthesis power, but Spacer can
diverge (bounded by timeout); theory support for the McCarthy array fragment is
partial.

**(f) Unlocks.** Potentially the widest class, including some Tier-2 array
invariants Spacer's array theory can reach.

**(g) Effort + deployability.** **Not deployable today**: §3 found no SMT/CHC
solver on the box. This option requires first adding z3/Eldarica to the nix
closure. Given that, it is a promising *later* engine but cannot be the first
milestone. Recorded honestly as design-only.

### Option 4 — Daikon-style dynamic candidate mining, machine-checked

**(a) Idea + literature.** Daikon (Ernst et al.): run the program on a test
suite, report properties that held over *all* observed states as likely
invariants. Purely dynamic — cheap, unsound on its own, but here it only
*proposes*.

**(b) Architecture.** vox programs compile and *run* (they are OxCaml). Instrument
loop heads / recursive entries to log the measure alphabet's values on the
existing test inputs, mine the properties that always held (the alphabet gives
the property templates), then feed the survivors as candidates into the §5
checker. An offline suggestion tool, not a compile-time pass.

**(c) Candidate language.** Same alphabet, but only instances the tests
*exercised* are proposed.

**(d) TCB=0 checking.** §5 — mined candidates are re-proved; a coincidental
property that happened to hold on the tests but isn't inductive simply fails.

**(e) Automation + convergence.** Single mining pass + one re-proof batch —
cheapest of all; no convergence concern. Incomplete: proposes nothing the tests
didn't exercise, and quality tracks test coverage.

**(f) Unlocks.** A fast *seed* for Options 1/2 (dynamic values seed the int
pool and the likely conjuncts). Weak as a standalone engine.

**(g) Effort.** Week-scale as a seeder; its real value is *composed* with
Houdini (dynamic seeding of the candidate set — a well-known Daikon+Houdini
pairing).

### Option 5 — LLM/heuristic proposer, machine-checked

**(a) Idea + literature.** Recent work (Lemur, Loopy, and related 2023–24
LLM-invariant systems) pairs an LLM proposer with an SMT/verifier checker and
iterates on counterexample feedback. The propose/dispose split makes the loop
sound regardless of the proposer.

**(b) Architecture.** Editor-server side-channel: on request, send the function
+ obligation + in-scope measures to the proposer, get candidate annotations,
run §5, feed back Lean's counterexample (the disproof witness renders a concrete
one) on failure.

**(c) Candidate language.** Unconstrained (free-form refinement predicates) —
its strength and its risk (may propose ill-sorted or non-inductive garbage,
caught at re-proof).

**(d) TCB=0 checking.** §5 — this is the whole point: an LLM proposing
invariants is *sound* because Lean re-proves. The vox counterexample oracle
gives high-quality feedback for the iterate step.

**(e) Automation + convergence.** No termination guarantee; latency and
determinism worse than the algorithmic options; excellent at *guessing the
shape* of an invariant a human would write, especially Tier-2 frames the
deterministic engines struggle with.

**(f) Unlocks.** Complements the deterministic core: best on the invariants
that need creativity (permutation/frame reasoning), worst on the routine
arithmetic ones Houdini nails deterministically and cheaply.

**(g) Effort.** Week-scale to wire (the checker and oracle already exist); value
is as a *fallback* when Options 1/2 return empty, surfaced in the editor.

### Surface option (orthogonal to engine) — editor-guided suggestion

Regardless of engine, the right *product* shape is likely an interactive
suggestion in the proof pane ("try invariant: …", accept to write it into the
source) rather than silent compile-time magic — the invariant becomes visible,
reviewable source, and the human stays in the loop for the Tier-2 obligations
the engine can't finish. This reuses the existing two-pass editor and
proof-state pane. Engine and surface should be chosen separately: the same
Houdini core serves both a `-vox-infer` batch CLI and the editor ghost-text.

---

## 7. Benchmark and ranking

Ranked by invariant class; "grind-closes" = inductiveness discharged
automatically once the predicate is right.

| # | benchmark | invariant class | grind-closes? | Opt 1 (Houdini) | Opt 2 (ICE) | Opt 3 (CHC) | Opt 4 (Daikon) | Opt 5 (LLM) |
|---|---|---|---|---|---|---|---|---|
| 1 | summation loop `s = i*(i-1)/2` | Tier 0/1 conj | yes | ✅ | ✅ | ✅* | seed | ✅ |
| 2 | isqrt bound (delete `go`'s refinements) | Tier 1 conj (nonlinear `sq`) | yes | ✅ | ✅ | ✅* | seed | ✅ |
| 3 | binsearch bracket (sentinel disjunction) | Tier 0/1 **disj** | yes | ⚠️ (only if disj is one qualifier) | ✅ | ✅* | seed | ✅ |
| 4 | array-fill frame `∀k<i. elem a k = v` | Tier 2, McCarthy | **needs step lemma** | ⚠️ + fallback | ⚠️ + fallback | maybe | seed | ✅ |
| 5 | reverse loop (delete `revinv_step`) | Tier 2 + structured proof | **no** | ❌ predicate only | ❌ predicate only | maybe | no | ⚠️ |
| 5b | qsort partition bounds | Tier 0 bounds ✅ / permutation ❌ | bounds yes | ✅ bounds | ✅ bounds | ✅* bounds | seed | bounds ✅ |

`*` = design-only (no solver on box). The table's honest headline: **Options 1
and 2 fully unlock #1–#3 and the *bounds* of #5b**; #4 needs the step-lemma
fallback; **#5 (the lead's suggested milestone) is the north star, not the first
milestone** — deleting `revinv_step` requires Tier-2 predicate inference *and*
structured-proof synthesis, which is quarter-plus work.

---

## 8. Recommendation

**Engine:** Houdini / Liquid-fixpoint (Option 1) over the Tier-0+1 measure
alphabet, mined from the function's own postcondition and in-scope measures,
pre-filtered by the disproof evaluator, survivors re-proved by grind through the
unchanged pipeline (zero TCB). Add ICE (Option 2) later for disjunctive
invariants; keep CHC-export (Option 3) as a design-only future engine pending a
solver in the nix closure; use Daikon (Option 4) as a *seeder* for the candidate
set and LLM (Option 5) as an *editor fallback* when the deterministic core
returns empty.

**Surface:** editor ghost-text suggestion in the proof pane plus a `-vox-infer`
CLI that writes the discovered annotation back as a source suggestion. Choose
engine and surface independently; the Houdini core drives both.

**Staging:**
- *Week:* Houdini over Tier 0/1 for **recursive-helper contracts only** (reuse
  the contract-VC path — no new loop machinery), batched single-Lean-file check,
  `-vox-infer` prints the discovered contract.
- *Month:* `[@vox.invariant]` loop inference; disproof pre-filter; editor
  ghost-text.
- *Quarter:* ICE for disjunctive invariants; Tier-2 array frames with automatic
  step-lemma emission via `[@@vox.lemma]`; Daikon dynamic seeding from the test
  runs; then attempt the reverse north star.

**First milestone (honest choice):** `demo/lean_isqrt.ml` verifies with `go`'s
hand-written `lo`/`hi` parameter refinements **deleted** — the engine
rediscovers `0 <= lo`, `sq lo <= x`, `lo < hi`, `x < sq hi` from the alphabet
and the postcondition, and grind closes inductiveness. This is achievable at
week scale because isqrt is linear+measure (grind-dischargeable), whereas the
lead's suggested `reverse.ml`-step-lemma-deletion milestone is Tier-2 +
structured proof (the north star, staged for the quarter). A summation loop is a
good second milestone on the `[@vox.invariant]` surface.

---

## 9. Cross-references to sibling design quests (interfaces only)

- **Borrowing (now/fin).** A borrow's now/fin two-state facts are extra program
  variables; a loop over a borrowed slice needs an invariant *relating* now/fin
  across iterations. Interface: the qualifier alphabet must admit now/fin-indexed
  measures, and the live-variable set must include the borrow's ghost witnesses.
  Design owned by the borrowing quest.
- **Shared mutation.** The mutable-set in-place-borrow ghosts (image at the ISet)
  extend the alphabet with ghost-image measures; inference over such loops needs
  those measures in scope. Owned by the shared-mutation quest.
- **Exceptions.** Exceptional exit edges add VCs; an inferred invariant must hold
  on the exceptional back-edge too. Interface: the CHC system gains clauses for
  exception paths, and the exit obligation splits over normal/exceptional
  continuations. Owned by the exceptions quest.
- **Stdlib specs.** Inference quality scales directly with the exported measure
  surface: the richer the stdlib `total_`/reflected spec vocabulary, the larger
  the qualifier alphabet, the more loops become inferrable. Owned by the stdlib
  quest.
