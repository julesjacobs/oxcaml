# Mutable levels and pool generalization

The richer-node unifier, level lowering, generalization and copier now use the
same `Copy_spec.node` and unique `Pref.token`. No graph conversion or separate
runtime memo table is involved. The earlier minimal unifier and STLC development
remain as a reference; the STLC entry point still uses that minimal unifier.

## Runtime algorithms

`level_unifier.ml` follows representatives, checks occurrence, lowers the target
of a variable binding to the variable's level, and then installs a link. Only
finite, nonnegative nodes can be unification operands. Generic sources cannot be
bound. Arrow unification retains successful left-child writes if the right
child fails. There is no rollback, pair cache or path compression.

`level_lower.ml` traverses links and both arrow children, then updates a node's
level to the minimum of its current level and the requested bound. It preserves
descriptors and memos. It deliberately does not prune at equal or smaller levels:
there is no assumed descendant invariant hidden in that traversal. A shared DAG
may be traversed repeatedly. Visited marks and occurs-check pruning remain later
optimizations. Cycles can diverge; runtime termination is not proved.

`generalize.ml` consumes a supplied pool traversal and marks finite nodes
strictly above the cutoff generic. It preserves descriptors and memos, accepts
duplicate entries, and writes each visited cell. The pool is an immutable list
of shared handles. It is a generalization worklist, not a copying memo table.
The function returns ownership of the updated heap. The complete level classification
is established after processing the whole worklist; the proof does not assume
that classification midway through processing it.

## Checked boundaries

- `level_spec.ml` separates heap-write evidence from a completed subtree bound.
  A valid level write requires its immediate children already below the bound.
  `level_proofs.ml` proves model equivalence, descriptor/memo preservation,
  scope preservation and preservation of level order. A node already below the
  requested bound, or generic, keeps its exact contents.
- `level_unifier_spec.ml` records both lowering and link writes. Its binding rule
  requires the target at or below the variable's level. The ported model proofs
  retain success in both directions and refutation on failure. The metadata
  proofs preserve nonnegative finite reachability, level order, all memo fields
  and the entire contents of generic nodes. `copy_equation` identifies the
  copier's model predicate with the unifier's descriptor model.
- `level_copy_proofs.ml` transports the copier's returned history into the finite
  scope required by unification. It covers finite boundaries, shared copies,
  generic links, session allocation and later memo writes.
- `generalize_spec.ml` states pool membership, ownership, coverage and the exact
  level-based scheme independently of the traversal. Coverage is a total
  function: every owned finite node above the cutoff occurs in the supplied
  pool. `closed_observe` proves the exact pointwise effect; `closed_level` uses
  coverage to establish complete classification. Lowering and unification
  preserve coverage because they allocate nothing and never raise levels.
- `environment_preserved` takes an explicit path from an environment root at or
  below the cutoff. Every cell along that path retains its exact contents after
  generalization. This uses the checked parent-to-child level order.
- `with_generalized_instance` constructs a final copied-heap model for arbitrary
  parameter choices, preserving every assignment in the pre-generalization
  heap. `with_generalized_choices` derives parameter choices from any final
  copied-heap model. Both interpret the scheme selected by the original levels;
  they compose the actual generalization heap with the exact copier theorem.

All universal facts are total proof functions. Finite unfoldings, pool membership
and paths are explicit witnesses. Ghost recursion is checked total; no new
axioms or SMT quantifiers were added.

## Registered allocation and finite forests

`pooled_allocator.ml` registers each allocation immediately in the returned
pool. `pooled_copy.ml` threads that pool through the copier, registering the
session cell and every fresh copy exactly once. Memo hits, finite boundaries
and generic links add no entry. Its checked history equation specifies the
exact returned pool; total proof functions preserve coverage and ownership.
The earlier `copy_algorithm.ml` remains the standalone reference interface.

`level_finite_spec.ml` and `level_finite_proofs.ml` port finite readback to the
richer nodes. They construct finite forests from allocation and transport them
through lowering and unification, including retained writes on failure.
`forest_transport.ml` preserves forests through generalization and actual copy
histories and derives the finite unfoldings needed by generalization. Finite
model construction and cycle exclusion are checked on this representation.

`pooled_demo.ml` starts with an empty heap, allocates and registers a shared
arrow graph, unifies, generalizes and copies it, then generalizes and copies
again. It constructs the forests and coverage callbacks through the checked
transport functions. Both successful unification and occurs rejection are
exercised; exact pool counts check session allocation and sharing. Rejection
fixtures rule out missing registrations, invented copy allocations, stale
finite witnesses, cycles and stale coverage after allocation.

## Causal provenance

`lower_locality_spec.ml` states that every level-write target occurs in the
returned bounded traversal. `Level_lower.lower` now proves that confinement.
The unifier's `Lowering` evidence also identifies the target traversal and the
source variable's original level, and requires the ensuing variable binding.
Previously, its weaker evidence allowed unrelated level decreases; those are
model-preserving but unsuitable for principal generalization.

`provenance_spec.ml` defines an origin as a saved finite root at or below the
cutoff plus an explicit path in the current graph. `provenance_proofs.ml`
constructs origins initially and transports them through fresh allocation above
the cutoff, causal unification, copying and generalization. A newly lowered
node obtains a path through the binding source and the target traversal.
Successful and failed unification both preserve provenance.

With the saved roots still below the cutoff and current level order,
`generic_excludes_origin` rules out an origin for every newly generic node.
`nongeneric_origin` constructs an origin for each active node that remains
nongeneric after generalization. Pool coverage supplies complete classification.
These statements are relative to saved heap roots; the correspondence to the
declarative typing environment is still part of the HM proof.

`provenance_demo.ml` allocates an outer variable and two fresh variables, unifies
one fresh variable with the outer variable, and generalizes. The shared variable
stays finite and the independent variable becomes generic. The fixture constructs
its origin callbacks from allocations and the actual unification evidence.

## Remaining integration

These are verified primitives, not a principal let-polymorphic inferencer.
Callers still compose explicit total forest, coverage and level-order callbacks.
`nested_pool.ml` closes a child pool and transfers its remaining finite entries
to the parent pool. Generic child entries are discarded; existing parent entries
are preserved. `transfer_listed` specifies exact membership and
`close_transfer_coverage` preserves coverage for any outer cutoff. This is a
list-based scope interface; callers retain the parent pool while filling the
child pool. The original accumulating interface remains available. No separate runtime
memo table was added. Runtime termination is not proved.

The saved-heap provenance theorem still needs connecting to the declarative
typing environment. It does not itself establish that every nongeneric variable
is reachable from an environment scheme. The richer unifier has
exact model, finite-readback and MGU factorization guarantees; see
`unifier-mgu.md`. The HM layer must solve each RHS
before generalizing it and prove principal typing against independent
declarative rules. The minimal STLC entry point remains unchanged.
