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

## Remaining integration

These are verified primitives, not a principal let-polymorphic inferencer.
Callers currently construct the pool, coverage function and finite-unfolding
forest explicitly; the positive fixtures do this from allocations. Allocation
and copying do not yet register every newly allocated node in a runtime pool.
The copier's session cell must also be accounted for when adding that allocator
interface. Automatic pool registration, nested pool transfer and saved-heap
provenance sufficient for principal generalization remain next. The current
path bound proves environmental exclusion, not that every nongeneric variable
is reachable from the environment.

The richer unifier has exact model and level-order guarantees. The earlier
minimal unifier's finite-readback/MGU API has not yet been ported to this node
representation. Generalization accepts a supplied finite forest; it does not
construct that forest from the richer unifier's derivation. The next HM layer
must supply that transport, solve each RHS before generalizing it, and prove
principal typing against the independent declarative rules.

The runtime pipeline fixture generalizes a source, instantiates it, unifies a
copied variable with a Boolean boundary and checks that the original generic
variable remains unchanged. It covers shared parameters and partial
instantiation. Other fixtures check level lowering, occurs rejection, retained
writes on failure, and rejected invalid level, memo and pool witnesses.
