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
- [ ] Principal STLC inference: allocation, shared environments, explicit typing
  derivations, soundness, completeness and rejection.
- [ ] Monomorphic recursive functions.
- [ ] Levels and scope: lowering, pool membership witnesses and saved-heap
  provenance, preserving the unifier's semantic contract.
- [ ] Generic templates and copy instantiation: exact scheme-instance witnesses
  with fresh copies and a shared nongeneric boundary.
- [ ] Let-polymorphic HM: level-based generalization, context framing and
  principal typing.
- [ ] In-node traversal marks with all-exit cleanup.
- [ ] In-node copy memoization with epoch-scoped correspondence witnesses.
- [ ] Structure linking without a unification pair cache.
- [ ] Path compression with explicit representative and generic-cell framing.

Later boundaries remain subject to the ongoing Pro design consultation. For each
stage, require checked semantic proof functions, positive runtime cases,
rejected incorrect implementations, and inspection that ghost evidence erases.
Do not promote a soundness-only milestone to principality. OCaml runtime
termination is outside this sequence; ghost mathematics must remain total.
