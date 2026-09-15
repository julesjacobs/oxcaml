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
- [x] Mutable first-order unifier (this PR): shared cells, representative lookup,
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
  levels; see `scheme-instantiation.md`. Unifier integration remains below.
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
- [ ] Generalization provenance: track allocations and environment reachability
  sufficiently to prove principal generalization, with explicit witnesses.
  Causal lowering, saved-root origins, two-sided classification and relative
  principal generalization with actual copy realization are checked;
  the bridge to the declarative environment remains.
- [x] Richer-node MGU: substitution factorization, idempotence and identity
  outside the original heap; see `unifier-mgu.md`.
- [x] Copy depth discipline: require shared finite boundaries to be no deeper
  than the instance depth, and prove copied heap level order.
- [ ] Let-polymorphic HM: connect environments, let bindings, generalization and
  copying; prove soundness, completeness, principality and rejection against
  independent declarative typing rules.
- [x] In-node occurs-check marks with cleanup on every return path: Boolean
  fields cache completed negative searches, and a temporary trail restores full
  node contents pointwise. Success, failure, MGU and provenance proofs compose.
- [ ] Structure linking without a unification pair cache.
- [ ] Path compression with explicit representative and generic-cell framing.
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

## Remaining HM proof ladder

- [x] Independent indexed declarative typing, scheme opening and context weakening.
  Checked opening/evaluation, weakening/meaning, well-formedness and scoping;
  positive `id id` and recursive-call derivations plus rejection probes.
- [x] Total type proofs: evaluation, opening, weakening, embedding, abstraction
  and free-variable substitution. The generalization lemma uses an explicit
  context-freshness predicate; proving it for graph-selected names remains below.
- [x] Environment templates with protected boundary transport and constructed
  empty, monomorphic, weakened and model-transported instance translators.
  The let translator remains in the one-let bridge below.
- [x] Interleaved execution witnesses for actual richer-node operations, with
  heap extension and successful-result ownership. A concrete let trace checks
  allocation, close/transfer and clean copying; the driver remains below.
- [x] Actual richer-node let-free driver, including monomorphic recursive
  lambdas: checked execution witnesses, pool/level/ownership preservation,
  cleanup invariants and explicit failure prefixes.
- [ ] Richer-node let-free semantics: model extension, model restriction,
  soundness and rejection against independent declarative typing.
- [ ] One-let bridge: construct baseline and alternative RHS models, apply
  relative generalization, and consume the resulting factorization for `id id`.
- [ ] Nested execution transport for forests, levels, pools and active RHS origins.
- [ ] Public closed-term HM soundness, completeness, principality and rejection;
  nested-let, mixed-boundary and saved-garbage regression cases.

These are proposed steps from the completed Pro consultation, not checked
results. Each step must construct its witnesses rather than accept the desired
semantic theorem as an input callback.

The Pro design consultation supports the remaining order above. For each
stage, require checked semantic proof functions, positive runtime cases,
rejected incorrect implementations, and inspection that ghost evidence erases.
Do not promote a soundness-only milestone to principality. OCaml runtime
termination is outside this sequence; ghost mathematics must remain total.
