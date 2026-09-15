# Exact scheme instantiation

`copy_algorithm.ml` copies a shared mutable type graph owned by one unique
`Pref.token`. Nodes contain a descriptor, a finite or generic level, and an
in-node memo. Generic variables, constants and arrows allocate new nodes at the
requested level. Generic links return the copied target. Finite nodes, including
finite links, retain physical identity. Repeated occurrences of a generic node
reuse its completed copy within a call.

Each call allocates one finite Boolean node as its fresh session identity.
Memo entries contain this handle and the copied handle. Physical equality
recognizes current entries without integer epochs or wraparound assumptions.
Old entries remain in source nodes until overwritten by a later copy. They can
retain earlier copies; cleanup and memory-retention improvements are deferred.
There is no separate runtime memo table. `finish` rereads the source after copying
children and checks its memo before updating it.

## Checked interfaces

`copy_spec.ml` defines the graph, its model equations, scheme templates and
interpretation independently of the traversal. The ghost `history` records
actual allocation and memo writes; its interpretation is the exact returned
heap. `instantiate` returns a valid history and a target corresponding to the
source root. The history is erased execution evidence, not a runtime log.

`copy_heap_proofs.ml` proves preservation of old descriptors and levels,
completed memo lookup, allocation and freshness. `copied_fresh` proves that the
copy of a generic nonlink node is outside the old heap and remains at the
requested level with an empty memo throughout the session. Generic links may
share a finite target, so this freshness statement excludes links.

`copy_template_proofs.ml` supplies both semantic directions:

- `with_scheme_instance` takes an old model, an explicit finite template forest
  and arbitrary total choices for generic parameters. It constructs a final
  model preserving every old assignment and assigning the copied root exactly
  the interpreted instance.
- `with_instance_choices` takes any final model and constructs total parameter
  choices whose interpreted instance equals the copied root's value.

The underlying model construction is in `copy_complete_proofs.ml`; restriction,
heap scope and allocation models are in `copy_model_proofs.ml`; the converse
interpretation proof is in `copy_sound_proofs.ml`. Universal facts are explicit
total functions. Finite templates and histories are explicit inductive
witnesses. No SMT quantifiers or new proof assumptions are introduced.

## Scope of this stage

This is a separate richer-node subsystem. The existing minimal mutable unifier
and STLC inferencer do not yet operate on these nodes. Finite levels currently
control the copying boundary; lowering, level ordering, generalization and
let-polymorphic inference belong to subsequent stages.

Clients supply a heap-scope function. The completeness interface additionally
requires a finite template for every owned node and an old model. The demo
constructs these from allocations. `history_scope` supports subsequent calls;
a reusable template-forest transport API is not provided yet.

The runtime traversal has partial-correctness contracts. It has no termination
proof or cyclic-template rejection: a generic cycle can recurse indefinitely.
Finite boundaries stop template expansion. Memoization is installed after
children finish. The ghost proof functions themselves are checked total.

The positive fixture exercises shared parameters, finite boundaries, generic
links, repeated sessions, constants and finite-root reuse, and calls both
semantic interfaces. The negative fixture rejects fabricated mappings, copied
finite boundaries, reused old allocation targets, dropped arrow children,
inconsistent duplicate-parameter instances and stale-session equality.
