# Mutable inference PR sequence

Each stage supplies explicit existential witnesses and total proof functions for
universal facts. Keep SMT refinements quantifier-free. Preserve the existing Lean
algorithm and theorem as a reference throughout this separate Vox development.

- [x] [Pref handle identity](https://github.com/julesjacobs/oxcaml/pull/137):
  runtime physical equality with a logical identity contract.
- [x] [Total recursion through closures](https://github.com/julesjacobs/oxcaml/pull/139):
  direct recursive proof calls inside total model callbacks.
- [x] [Proof ergonomics](https://github.com/julesjacobs/oxcaml/pull/140):
  dependent arguments, checked function adaptation, recursive annotations and
  direct unboxed ghost fields.
- [x] Mutable first-order unifier: shared cells, representative lookup,
  occurs checking, returned ownership, exact model transformation and correct
  rejection. See `mutable-unifier.md` for the precise guarantee.
- [x] Finite readback and acyclicity: construct finite unfolding witnesses from
  allocation and preserve them through unification, including failure. Prove
  cycle exclusion and finite-model construction; see `unifier-finite-readback.md`.
- [x] MGU factorization: canonical handle-indexed readback, solution and
  factorization functions, idempotence and identity outside the original heap.
  See `unifier-mgu.md`.
- [x] Principal closed-term STLC inference: allocation, shared environments,
  declarative typing derivations, model-extension completeness, principality and
  rejection. See `stlc-inference.md`; the first runtime layer generates then
  solves an equation tree. Open-term principal pairs remain a separate API.
- [x] Monomorphic recursive lambdas: shared argument/result/self allocation,
  explicit body/result equality, and extended soundness, model-extension,
  principality and rejection proofs. See `stlc-inference.md`.
- [x] Generic templates and copy instantiation: exact scheme-instance witnesses
  with fresh copies, a shared nongeneric boundary and in-node memoization using
  fresh session handles. Separate richer-node subsystem with finite/generic
  levels; see `scheme-instantiation.md`. Unifier integration is checked below.
- [x] Richer-node unification and lowering: exact success/rejection, finite
  scope, level order, generic-node and memo preservation.
- [x] Pool generalization primitive: explicit coverage and finite forests,
  environmental path exclusion and two-sided exact instantiation; see
  `level-generalization.md`.
- [x] Automatic pool registration for allocation and copying, including session
  cells; finite-unfolding transport through richer-node unification,
  generalization and copying. See `level-generalization.md`.
- [x] Nested pool transfer and draining: retain lowered finite nodes in the
  parent pool, discard completed generic entries, and prove coverage transport.
- [x] Generalization provenance: track allocations and environment reachability
  sufficiently to prove principal generalization, with explicit witnesses.
  Causal lowering, saved-root origins, two-sided classification and relative
  principal generalization with actual copy realization and the full
  declarative bridge are checked.
- [x] Richer-node MGU: substitution factorization, idempotence and identity
  outside the original heap; see `unifier-mgu.md`.
- [x] Copy depth discipline: require shared finite boundaries to be no deeper
  than the instance depth, and prove copied heap level order.
- [x] Let-polymorphic HM: connect environments, let bindings, generalization and
  copying; prove soundness, completeness, principality and rejection against
  independent declarative typing rules.
- [x] In-node occurs-check marks with cleanup on every return path: Boolean
  fields cache completed negative searches, and a temporary trail restores full
  node contents pointwise. Success, failure, MGU and provenance proofs compose.
- [x] Structure linking without a unification pair cache.
- [x] Path compression with explicit representative and generic-cell framing.
- [x] Copy-memo cleanup: a temporary duplicate-free list of touched source
  cells clears all written memos before return. Descriptor, level and exact
  model preservation are checked; lookup remains in-node.
- [x] Vox sort-encoding crash: minimize, fix and add a compiler regression.
- [x] Vox dependent-callback adaptation: refine expressions and projected proof
  callbacks through one evaluated local binding, preserving checked contracts,
  totality and curried application timing. Frontend and Merlin checks pass.

For every item: implement the runtime change, check the semantic proofs, run
positive and rejection tests, inspect ghost erasure, and review before publishing
its stacked PR. Mark an item complete only when its stated guarantee is checked.
Runtime termination remains outside scope.

## HM proof ladder

- [x] Independent indexed declarative typing, scheme opening and context weakening.
  Checked opening/evaluation, weakening/meaning, well-formedness and scoping;
  positive `id id` and recursive-call derivations plus rejection probes.
- [x] Total type proofs: evaluation, opening, weakening, embedding, abstraction
  and free-variable substitution. The generalization lemma uses an explicit
  context-freshness predicate; its graph bridge is recorded below.
- [x] Environment templates with protected boundary transport and constructed
  empty, monomorphic, weakened and model-transported instance translators.
  The let translator is constructed in the one-let bridge below.
- [x] Interleaved execution witnesses for actual richer-node operations, with
  heap extension and successful-result ownership. A concrete let trace checks
  allocation, close/transfer and clean copying; the driver is checked below.
- [x] Actual richer-node let-free driver, including monomorphic recursive
  lambdas: checked execution witnesses, pool/level/ownership preservation,
  cleanup invariants and explicit failure prefixes.
- [x] Finite forests and model restriction through every execution constructor,
  including nested lets and failure prefixes.
- [x] Richer-node let-free soundness: construct independent declarative typing
  witnesses for finite readback, including monomorphic recursive lambdas.
- [x] Richer-node let-free model extension, completeness, principality and
  rejection against independent declarative typing.
- [x] Actual let-polymorphic driver: nested RHS levels, child-pool closing and
  transfer, preserved parent coverage, protected saved nodes, and safety on
  failure. Let-free theorem contracts remain available.
- [x] Canonical environment freshness for graph-selected generalization names:
  low boundary readbacks and existing generic templates avoid the selected names;
  abstraction establishes the premise of `generalize_typing` before substitution.
- [x] One-let bridge: construct baseline and alternative RHS models, apply
  relative generalization, and consume the resulting factorization for `id id`.
  The baseline comes from finite readback; each alternative comes from the same
  RHS execution’s checked completeness proof. Both copies use their current heap.
- [x] Nested execution transport for forests, levels, pools and active RHS origins.
  `run_origin` constructs saved-root paths for low residual variables across
  every execution constructor. `rhs_interpret` derives relative interpretation
  from two RHS models agreeing on the saved low boundary. The full completeness
  proof constructs these models from declarative RHS typings.
- [x] Public closed-term HM completeness, rejection and factorization: constructed
  baseline and alternative models for arbitrary nested lets; actual nested-alias
  and mixed-boundary runtime fixtures pass in bytecode and native builds.
- [x] Declarative template instances: construct scheme arguments from copy
  choices, preserve shared parameters, and protect low-boundary names from
  capture. Connect direct scheme reification to canonical abstraction.
- [x] Public closed-term HM soundness, combined with factorization for principality:
  full nested-let induction and actual runtime proof consumers pass.
- [x] Nested-let, mixed-boundary and saved-garbage regression cases.

Completed steps construct their witnesses rather than accept the desired
semantic theorem as an input callback. Structure linking and path compression
are integrated into recursive unification with the proofs listed below.

The Pro design consultation informed the order above. For each
stage, require checked semantic proof functions, positive runtime cases,
rejected incorrect implementations, and inspection that ghost evidence erases.
Do not promote a soundness-only milestone to principality. OCaml runtime
termination is outside this sequence; ghost mathematics must remain total.

## Heap rewrites

- [x] Separate relative interpretation from directed saved-root paths: accept
  explicit agreement of the two models on current low-level nodes.
- [x] Check the compression counterexample for directed saved-root paths.
- [x] Prove compression preserves exactly the models, finite readback, levels,
  generic cells, marks, memos, pool coverage and semantic low-node agreement.
- [x] Implement and run the standalone compressing representative with a ghost
  edit trace; positive bytecode/native fixtures check rewritten links and generic
  framing.
- [x] Complete compression rejection probes and ghost-erasure inspection.
- [x] Restrict saved-root provenance to low residual variables; derive compound
  model agreement by finite unfolding, then recheck execution provenance. Full
  HM principal fixtures pass with the weaker provenance interface.
- [x] Prove post-success structure linking preserves models, finite forests,
  level order, scratch metadata and semantic determination.
- [x] Link structure roots inside recursive unification, re-resolving roots
  after child calls. Keep the unification pair cache absent.
- [x] Integrate compressing representative calls into recursive unification.
- [x] Recheck full HM soundness, completeness, principality and rejection for
  the integrated runtime, including failure prefixes and saved garbage.
- [x] Review and publish the remaining stacked changes.

The inference driver now calls the optimized recursive unifier. Its ghost
forest comes from allocation, execution and pool-closing proofs. Full principal
fixtures and the let, recursion, polymorphic, shared-graph and compression
regressions pass in bytecode and native code. Structure, execution, runtime and
model rejection probes pass. Ghost erasure was inspected, and the final
`codex review --uncommitted` reported no actionable defects.

## Runtime efficiency follow-up

Keep `Pref` as the semantic model of mutable fields. No Vox language change is
required for this sequence. Preserve full HM proofs at every runtime change.

- [x] Prune level lowering using level order; check a deeply shared DAG.
- [x] Avoid copy-session allocation for nongeneric instances.
- [x] Reuse compressed representatives and avoid already-direct link writes.
- [x] Fuse pool closing/draining and skip unchanged writes.
- [ ] Discard obsolete pool links and route retained nodes to the appropriate
  enclosing pool.
- [x] Prove all memos empty at inference boundaries and restore this invariant
  after copying, including untouched and newly allocated nodes.
- [x] Remove copy epochs using the clean entry/exit memo invariant.
- [x] Package runtime proof arguments in all-ghost records with void layout;
  inspect native calling conventions.
- [x] Replace unary-index linear environment lookup with an efficient verified
  representation.
- [x] Replace depth-sensitive runtime recursion with explicit worklists.
- [x] Reduce update allocation by skipping unchanged writes. Retain the accepted
  `Pref` representation; splitting every node field is not required for the
  algorithmic model and has not been justified by a measured benefit.
- [ ] Run integrated positive/rejection tests, inspect erasure, review and
  publish the stacked improvements.

All helpers on the HM execution path now use void-layout ghost wrappers.
Native Cmm confirms their runtime argument lists. Clean instantiation uses
in-node forwarding pointers, restores empty memos, and allocates no epoch cell.
The earlier stamped copier remains a separate proof-ladder implementation.

Representative lookup and compression compile to loops with erased path
reconstruction. The occurs check, pruned lowering, optimized unification,
clean copying and HM driver use explicit continuation closures for pending
work. Their full proof chain checks; native stress cases at depth 200,000 pass.
All nine deep HM, shared lowering, compression, copy cleanup, shared
unification, marked occurs and rejection suites pass in bytecode and native
code. Pool closing and
copy cleanup already compile to loops. Representative-only pool tracking and
level-directed transfer remain.

The first efficiency batch passes nine focused positive/rejection suites in
bytecode and native code, including full HM principality and the shared-DAG
regression. The remaining allocation and HM callers also pass direct proof
checking. Native Cmm was inspected for the converted helpers, and
`codex review --uncommitted` reported no actionable defects. The unchecked
items above remain work for subsequent batches.

Pool consultation (2026-09-16): https://chatgpt.com/c/6aab0a94-5f58-83eb-8540-4ad3abdef13c

Objective: derive representative-only pool coverage without weakening full HM
principality. Full current development attached. Proposed results must be
checked against the existing low-boundary and finite-scope invariants.

The environment implementation uses skew binary random-access lists and machine
integer variable indices. `Fast_environment.lookup_encoded` connects the
executable lookup to the existing declarative environment. `Hm_infer.closed_hm`
converts the original syntax once; `closed_compiled` accepts `Fast_term.term`
directly. `Fast_term.bound` constructs a variable from an integer while its
unary source index is ghost. The native worker receives only depth, pool,
runtime environment, runtime term and continuation. The full HM proof chain,
200,000 lookups and direct compiled inference pass in bytecode/native code.
The full principality regression also passes in bytecode/native code with
the new environment, including the deep cases. The open-environment
compatibility fixture also passes in bytecode/native code.

Pool migration experiments are isolated in `/tmp/vox-representative-pool-check`.
Checked steps: finite forests construct unique representative witnesses;
representative-level observations are independent of the witness; pool closing
preserves resolution paths and closes covered representative levels; an alias
can keep a finite stored level while its representative becomes generic; and a
level-independent representative loop checks. A closing loop that skips links also checks. These are local migration
lemmas and an isolated primitive, not yet the full HM integration.

The effective-level experiment now also proves representative-level closing,
physical preservation of saved low boundaries, and whole-scan level ordering.
These predicates use explicit total representative witnesses, with no SMT
quantifiers or new assumptions. The active HM implementation has not switched
to these predicates yet.

The epoch-free copying, void-layout arguments, stack-safe traversals and efficient
environment batch is published as PR #176, stacked on #175:
https://github.com/julesjacobs/oxcaml/pull/176.

Representative-pool migration:

- [x] Construct unique representative witnesses from finite forests.
- [x] Check effective closing, physical low-boundary preservation and whole-scan
  effective ordering; derive effective finite scope from effective ordering.
- [x] Prove links never become representatives again under the current optimized
  unifier, including failure prefixes, compression and structure linking.
- [x] Prove terminal redirection transports representative paths and low bounds.
- [x] Implement representative-only fused closing/transfer as a separate checked
  primitive; prove coverage and bounds for its retained entries.
- [ ] Migrate copy classification, templates, protected boundaries and the
  inference sharing shortcut to effective levels.
- [ ] Migrate lowering and the full HM runtime/semantic ladder, then switch the
  inference driver to representative-only pools.
- [ ] Route retained representatives directly to their enclosing level pools.
- [ ] Complete regression, erasure, review and publication of the pool migration.

The new primitive remains separate from the active inferencer until copying and
saved-boundary transport support stale Link levels. These local proofs do not
claim the full HM theorem for the new pool implementation.

The representative-pool fixture passes in bytecode/native code, including the
two-stage alias/generalized-target case. Native Cmm shows fused closing as a loop
with only cut, child pool and parent pool arguments. The effective-template
uniqueness, transport and closing-protection lemmas also check. The final
`codex review --uncommitted` found no actionable defects.

Effective copy-history framing, allocation, mapping freshness (including aliases)
and pointer-equality classification check in the isolated experiment. Wiring them
into an erased runtime interface exposed the ghost function-field limitation
recorded in `unifier-vox-findings.md`; those experimental copy modules are not
part of the active inferencer.

Effective-level copying now has a separate checked runtime and proof ladder:

- [x] Classify links through their representative and share finite sources.
- [x] Use in-node forwarding for generic sources and clear all touched memos.
- [x] Preserve saved representatives and construct representatives for fresh nodes.
- [x] Prove soundness and model-extension completeness for effective templates,
  including the final heap after cleanup.
- [x] Preserve finite forests, effective ordering, bounds, leaf provenance,
  pool scope and representative-only pool coverage.
- [ ] Switch the active HM driver and its sharing shortcut to this copier.

The stale-link fixture exercises finite sharing, generic copying, repeated
instantiation, shared arrows, stale stored levels and memo cleanup. Its initial
bytecode/native runs pass. Native Cmm shows only depth, pool and root as
instantiation arguments; representative callbacks and heap witnesses erase.
The depth-200,000 shared-graph stress case passes in bytecode and native code.
`codex review --uncommitted` completed without actionable findings. The active HM driver still uses the previous copier until lowering and
the remaining semantic interfaces migrate.

The effective-level copier is published as PR #179, stacked on #178:
https://github.com/julesjacobs/oxcaml/pull/179.

Descriptor-first lowering migration:

- [x] Follow links before testing stored levels; never write link records.
- [x] Preserve representative witnesses, effective scope and ordering.
- [x] Preserve physical heap frames, models, finite forests and graph paths.
- [x] Retain confined write traces and construct effective bounded trees.
- [x] Check the CPS runtime with erased heap and representative witnesses.
- [x] Test stale finite and generic link levels against finite targets in
  bytecode and native code.
- [x] Pass depth-200,000 shared-graph regression in bytecode/native code;
  inspect erased native calling conventions.
- [x] Complete review and publish the lowering batch.
- [ ] Connect this lowering operation to the effective unifier and HM driver.

The semantic lowering certificate is independent of representative functions.
It contains the terminal-write trace and a concrete bounded tree: links carry
only graph edges, while terminal nodes carry the level bound. The runtime
constructs this certificate from its effective-level proof. The certificate
alone proves heap framing, exact level updates, preservation of link records
and low boundaries, and completed-traversal effective ordering. This allows
the next unifier trace to remain ordinary inductive data.

The complete lowering batch passes bytecode/native tests, including the
terminal-certificate proofs and depth-200,000 shared graph. Native lowering
takes only bound and pointer arguments. `codex review --uncommitted` exited
zero with no actionable findings.

Effective unifier migration (in progress):

- [x] Remove raw alias-level requirements from representative lookup and the
  in-node marked occurs check.
- [x] Check path compression against finite representative levels, preserving
  effective levels, ordering, finite graphs, models and generic nodes.
- [x] Add a semantic trace with terminal-only lowering and graph-only traversal.
- [x] Reprove soundness, completeness, rejection, finite-graph preservation and
  the most-general-unifier property for that trace.
- [x] Connect occurs checking and descriptor-first lowering in variable binding;
  check successful binding and occurs rejection through stale aliases in
  bytecode/native code, including mark cleanup and unchanged link levels.
- [x] Transport representative paths, effective level decreases and ordering
  through every trace constructor, including failure prefixes.
- [x] Connect the recursive runtime, compression and structure linking to the
  effective trace and metadata proofs.
- [x] Prove physical protection of effectively generic nodes and preservation of
  representative-only pool coverage.
- [ ] Integrate the effective unifier into the full HM driver and reclose its
  public conclusions.
- [ ] Switch the HM driver to representative-only pools and direct enclosing
  pool routing; complete integrated regression, erasure checks and review.

The active HM driver still uses the previous unifier and pool implementation.
The new trace theorems certify the separate effective unifier runtime. Its
stale-alias, occurs-rejection and recursive-arrow fixtures pass in bytecode
and native code. Native Cmm shows only the two node pointers as public unifier
arguments, and only those pointers plus a continuation in the recursive worker.

The shared-graph fixture also passes in bytecode/native code. It applies the MGU
theorem on success and checks retained child linking when a later comparison
rejects. `codex review --uncommitted` exited zero with no actionable findings;
its nested test attempt could not identify its process, so the successful
parent-run suites are the test evidence.
