# Historical structure demo review boundaries

The starting sources for lists, trees, rings, expressions, definitions, clamp
and integer lists match the frozen `pro-review-fixes` catalogue files byte for
byte. This branch starts at PR #204 (`ddeab29cf8`), preserving its tested
compiler lineage. No catalogue file or other owner's worktree is changed.

## Ordered review files: mutable lists and trees

1. `stdlib/stdlib.mli`: machine integers, options, lists and equality.
2. `verification/library/pref.mli`: abstract physical handles, affine ownership,
   finite heap operations/equations, disjoint splitting/joining, fresh allocation
   and owned reads/writes. These are trusted primitive contracts.
3. `testsuite/tests/vox/pref_list.mli`: complete root, link, heap, shape-validity,
   reversal and node-observation equations; constructors, actual in-place
   reversal and unchanged disjoint-frame postcondition.
4. `testsuite/tests/vox/pref_tree.mli`: complete root, links, heap, shape-validity
   and flipped-tree equations; constructors and actual mirroring with its
   unchanged disjoint-frame postcondition.

The models are specification-relevant finite shapes over physical nodes. Their
visible equations express exactly which cells are owned and how each link must
point. The validity predicates exclude shared link cells and cycles within the
modeled list/tree. They are fully defined public input domains, not opaque
implementation invariants. Constructors return the ghost model and ownership
needed by operations; the independent clients construct no internal proofs.
The algorithms' token splitting, recursive invariants and helper lemmas remain
in the implementations. `reverse_into`, `set_links` and analogous helpers are
not exported. The list reversal preserves the same physical nodes, not merely
their values. Tree mirroring retains each node and exchanges its two subtrees.

The tree `observe` function is a diagnostic traversal with an ownership
precondition, not a theorem identifying its returned `shape`. The actual
mirroring theorem identifies the resulting heap with `heap (flipped model)`.
The separate runtime client additionally checks the diagnostic shape. Likewise,
`of_list` and `leaf` retain their established well-formed-construction contracts;
no new input-content theorem is claimed for those helpers.

## Ordered review files: rings

1. `stdlib/stdlib.mli` and `verification/library/pref.mli`, as above.
2. `testsuite/tests/vox/pref_ring.mli`: complete `present`, `owns`, directional
   path, linked-ring, pointer-update and flip equations; exact executable
   allocation, insertion, removal, six-write splice, reversal, traversal and
   ownership-detachment contracts.
3. `testsuite/tests/vox/pref_ring_splice.mli`: explicit initial singleton-cell,
   sentinel and distinctness premises; checked resulting empty source ring and
   two-node destination ring, including both traversal orders.
4. `testsuite/tests/vox/pref_ring_reverse.mli`: explicit initial configuration
   premises; checked resulting three-node reversed ring and both traversal orders.

The generic splice contract specifies six ordered boundary writes. It does not
claim that arbitrary ranges or destinations preserve a globally valid ring.
The six cells can alias unless the caller supplies additional distinctness;
the ordered `put` expression still specifies the exact effect. `reverse_nodes`
processes its ordinary runtime node list in order; repetitions mean repeated
flips. Its worklist is executable input, not a proof certificate. The concrete
reversal routine builds that worklist by traversal; its expected shape is ghost.
Whole-ring postconditions are exported for the original concrete configurations
only. No general abstract-sequence ring implementation has been substituted.

The concrete routines now return ownership with their proved ring properties.
They perform no final traversal checker. Their helper lemmas are in
`pref_ring_proofs.ml`, the setup/model modules and `pref_ring_checks.ml`, outside
the semantic interface. The older insertion/removal diagnostic fixture retains
its separate runtime traversal oracle; correctness of the ordinary generic
operations never depends on that oracle.

## Ordered review files: folding and supporting pure examples

1. `stdlib/stdlib.mli`: wrapping machine-integer arithmetic and equality.
2. `testsuite/tests/vox/expression_folding.mli`: the entire expression language
   (`Lit`, `Input`, `Add`), complete evaluation equation, actual `fold`, its
   preservation theorem for arbitrary input, and `eval_folded`'s direct result
   contract. Smart construction and inductive proof bodies are private.
3. `testsuite/tests/vox/clamp_api.mli`: complete clamp equation, ordered-bound
   domain, actual bounded-result operation, identity and idempotence claims.
4. `testsuite/tests/vox/int_list_laws.mli`: complete inductive-list append,
   length and sum equations, identity/associativity and append length/sum laws.

Folding preserves evaluation under wrapping arithmetic, including overflow;
there is no theorem about mathematical-integer arithmetic, normalization,
code size or a larger compiler language. `eval_folded` calls the proof in ghost
code. The public theorem may likewise be used through an erased ghost call.
Clamp's range and idempotence claims require ordered bounds; its equation still
specifies the function on unordered bounds. List `length` and `sum` use wrapping
machine integers, so length is not an unbounded cardinality or a positivity
claim. The implementation adds no axioms.

`testsuite/tests/vox/definitions.ml` remains a compiler-language regression,
not a separate algorithm theorem: explicit definition elimination, aliases,
shadowing, closures, partial/stateful-call distinctions and wrapping-overflow
rejection. It is unchanged, and all successful and rejected phrases are checked.
Its semantics are those of the complete public equations above and Vox's
trusted definition/refinement mechanism; it adds no hidden assumption to the
folding, clamp or list-law contracts.

## Trust, execution and evidence

All results assume the Vox type/refinement checker, SMT translation/solver and
listed primitive contracts. Physical handles identify mutable cells; heap
observations are ghost finite maps. Primitive allocation failure, stack
exhaustion and exception-safe ownership recovery are not covered. Mutation and
traversal contracts are normal-return contracts; this refactor adds no general
termination, memory-reclamation, concurrency or cost theorem. Pure functions
explicitly marked total retain their checked structural totality.

Run `python3 verification/review/check_structures_boundaries.py COMPILER_PREFIX`.
It reads the compiler installation and writes only this checkout's
`_build/structures-boundary-check`. It recompiles interfaces/implementations,
compiles list/tree/ring clients with only their public cmi files, runs bytecode
and native fixtures, checks the original successful/rejected pure-example
phrases and structural rejections, runs all three concrete ring fixtures, and
checks private-helper rejection and emitted-operation erasure. The concrete
ring client derives first-node observations from the exported whole-ring
postconditions without importing setup/model/proof modules. Isolated clients
can emit harmless missing-cmx optimization warnings; implementation objects
are supplied only when linking.

## Recorded result, 2026-09-25

With the read-only `worktrees/time-credits/_install` compiler (`5.4.0+ox`),
all checked interfaces and implementations compiled in bytecode and native mode.
The independent list/tree/generic-ring clients executed in both modes; the
concrete-ring client compiled with only the public interfaces. Folding, clamp,
integer-list and definition regression successes executed, and all 24 original
negative cases rejected in both modes. All three concrete ring fixtures ran;
four private helper lookups rejected; all ten emitted-operation audits passed.
The emitted reversal contains its ordinary traversal/worklist processing but no
runtime shape certificate or final traversal checker.

This preserves the frozen source/compiler lineage. As with PR #204, the newer
remote typed-heap restack is a different compiler baseline; migration there
requires its compatible compiler and a regression rerun.

`make -s fmt` could not start because this intentionally unconfigured checkout
has no `Makefile.build_config`. `git diff --check` passes. No configuration or
full compiler build was started under the shared disk-space constraint.
