# Vox design-fleet synthesis (2026-07-06)

Nine concurrent design studies covered the future-work list: shared mutation,
concurrency/atomics, borrowing/uniqueness, exceptions, termination/totality,
subtyping-vs-HM, invariant inference, a verified stdlib, and the capsule API.
Each produced a probe-grounded options doc (same directory, same date).  This
note records what converged, what the fleet recommends building next, and the
dependency order.

## The three convergences

1. **Modes pay for everything.**  Independently reached by five studies: the
   OxCaml mode checker (uniqueness, locality, linearity, contention,
   visibility, portability) statically answers "what else can touch this?",
   which is the expensive question in every verification story.  Consuming
   mode results is type-safety-adjacent — near-zero TCB — where today's
   borrow layer *assumes* the same facts per-library.  Concretely: framing
   (shared-mutation), data-race freedom + mutual exclusion (concurrency,
   via the OCaml 5 LDRF-SC theorem: race-free programs get sequential
   consistency, so vox's sequential Lean model stays sound under one
   memory-model axiom), borrow no-interference (borrowing), and capsule
   access ("type-safety-free, not TCB").  Strategic direction: convert the
   trusted `assume_unchecked_` layer into mode-consumed guarantees.

2. **Invariant cells, three ways, one mechanism.**  Shared-mutation M1
   (`[@@vox.cell_invariant]`), concurrency Option A (atomic invariant cell),
   and capsule Option 1 (capsule invariants) are the same design: a cell
   carries a declared invariant; reads assume it, writes re-establish it; no
   aliasing reasoning at all; sound under arbitrary aliasing *and*
   interleaving; grind-trivial.  The capsule study built its variant as a
   probe with **zero compiler changes** (a `capsule_spec` shim in the
   borrow_lib style) and exhibited the VC.

3. **Zero-added-TCB is achievable everywhere it was asked.**  Inference:
   propose-then-re-prove (a wrong proposer can only fail to verify).
   Termination: decrease conditions are ordinary refinement VCs.
   Capsules: the mode theorem replaces the trust.  Stdlib: per-module TCB
   is literally "none" for most of the graduating artifacts.

## Probe-established facts that constrain everything

- Cross-unit `[@@vox.lemma]` export **fails** (stdlib probe A); the `.mli`
  public-theorem path **works** (probe A2).  House rule: client-facing
  lemmas are interface-block public theorems.
- No SMT solver exists on this box — CHC-style inference is design-only;
  deployable inference runs through Lean/grind with the disproof evaluator
  as counterexample oracle.
- vox already runs verified **parallel** code (qsort's `psort` forks over
  disjoint slice borrows, zero extra obligations).
- Mid-body `raise` is not vacuous today: vox demands the continuation's VCs
  after a raise, so dead code after a mid-function raise spuriously fails —
  a live precision bug (exceptions Milestone 0).
- `[@@vox.decreases]` on an *ordinary* `let rec` is silently ignored; loops
  have no variant clause.  Termination is Lean-checked exactly where code
  becomes a Lean `def` (total_, lemmas) and absent where users write
  programs.
- vox is already half-bidirectional: expected refined types check by
  entailment VC; what is missing is subsumption under constructors and at
  synthesis joins.  Two warts hard-reject *sound* code (higher-order
  erasure; covariant result strengthening).
- The generic ordered functor (`Vset.Make(Ord)`) is the single BLOCKING gap
  for a general stdlib; v1 ships int-keyed containers around it.

## Recommended build waves

**Wave B0 — bug-fix tier (days each, no design risk):**
- Exceptions Milestone 0: raise/failwith recognized as never-returning
  (false into the continuation) — fixes the live precision bug.
- Failure-state UI + provenance honesty (in flight: fail-ui agent).
- Deferred-bug queue as capacity allows: #31 via value binding (dominates
  recursive via code per the stdlib PoC), #32 refined-bool, #72
  self-capture.

**Wave B1 — small features, each independently shippable (1–2 weeks each):**
- Invariant cells `[@@vox.cell_invariant]` (the three-way convergence; the
  capsule single-key counter is the acceptance demo, already prototyped).
- Termination measure VCs: `[@@vox.decreases]` honored on ordinary `let rec`
  and loop back-edges (opt-in `[@@vox.total]`); the `unit{1=2}` exhibit
  rejected under total mode.
- Subtyping stage 1: covariant erasure + arrow-codomain subsumption at
  directional edges only (fixes the two sound-code rejections; validates
  the "subsumption emits into the existing VC stream" thesis).
- Stdlib v0: Vlist (built), Voption/Vresult, Vbits — zero compiler work.
- Exceptions Option 3: handler arms learn the negation of normal-return
  conditions (zero annotation, ~1 week).

**Wave B2 — medium (a quarter, order by appetite):**
- Borrowing milestone: built-in prophecy-resolution rules for slice_lib
  (delete its assumed laws; qsort still verifies; trusted surface ~10 fns →
  3–4 raw stores).  North star: mode-consumed native borrows against the
  host `&x`/`exclusive` RFC.
- `raises` clauses v1 (per-exception postconditions, closed-world default).
- Inference v1: Houdini over the Tier-0/1 qualifier alphabet + editor
  ghost-text surface; milestone = isqrt's inner refinements deleted.
- Capsule now/fin through the key (Option 2) → via-modeled capsule
  libraries (Option 3): the concrete concurrent-verification story.
- Stdlib v1: Vset/Vhashmap/Viarray (wants #31/#32 fixed).

**End-state tier (deliberate commitments, not backed into):**
- Bidirectional refinement layer over HM (subtyping Option 2) — prerequisite
  for κ-style refinement inference.
- Native `&mut` (shared-mutation M3 / borrowing Option 1).
- Total-by-default modules; the total arrow.
- Linearizability / SL-in-Lean: explicitly declined for now; the trigger is
  a deliberate commitment to lock-free-structure linearizability, nothing
  in the atomics/locks roadmap forces it.

## Cross-study seams (reconciled)

- Borrow region exceptional exit = exceptions' join point (resolution fires
  or soundly leaks on the exceptional edge).
- Inference's qualifier alphabet must admit now/fin-indexed and ghost-image
  measures (borrowing, shared-mutation).
- κ-placement/constraint form belongs to the subtyping doc; the fixpoint
  engine to inference; subtyping stage 2 precedes κ-inference.
- Capsule work is the concrete instantiation of concurrency Option B and
  strictly out-trusts the borrow layer; align on the shared heap-store
  interface (borrowing ↔ shared-mutation).
- "Does a function that always raises terminate?" — joint
  exceptions/termination decision, deferred to raises-clauses v1.
