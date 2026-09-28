# Finite readback and acyclicity

This stage adds checked ghost proofs above the mutable unifier. The executable
unifier and its execution contract are unchanged. Runtime termination remains
outside the contract.

## Review surfaces

- `testsuite/tests/vox/unifier_finite_spec.ml`: finite unfolding trees, heap
  correspondence, directed walks, mathematical size and type readback.
- `testsuite/tests/vox/unifier_finite_proofs.ml`: allocation and unification
  preservation, uniqueness, cycle exclusion and the connection to type models.
- `testsuite/tests/vox/unifier_finite_demo.ml`: construct the invariant from an
  empty token, extend it through allocation, and transport it through actual
  unification, including retained writes on failure.
- `testsuite/tests/vox/unifier_finite_rejected.ml`: reject a fabricated self-link
  unfolding, a stale variable witness, an omitted arrow child and unchecked
  binding of a variable to an arrow containing that variable.

## Invariant

A `tree` is a finite inductive unfolding. Every node records its physical handle;
`Branch` contains both child unfoldings and `Alias` contains the target unfolding.
`finite h t` checks allocation membership and exact saved-heap contents at every
node. Sharing can occur in the graph; its ghost unfolding may duplicate subtrees.

The invariant is an explicit total ghost function:

```ocaml
(x : node Pref.t) @ immutable ->
{t : tree | root t === x &&
  (if H.mem h x then finite h t else H.at h x === None)} @ immutable
```

The returned tree is the existential witness for the requested handle. The
function supplies the universal fact. No SMT quantifier or assumed invariant
constructor is introduced. The absence clause records coherence between heap
membership and lookup, which the current Pref encoding does not infer from
membership alone. The empty-heap fixture proves it and allocation preserves it.

## Preservation

`allocation_finite_at` extends the invariant when allocating a fresh node whose
children already belong to the old heap. `allocation_frame` preserves every old
unfolding. `finite_scope_at` derives the unifier's safe-access scope contract.

For binding `p` to `q`, `search_finite` turns the successful negative occurs
search into a finite unfolding of `q` in the successor heap. Each visited handle
is distinct from `p`. `replace_free` replaces occurrences of the old variable
leaf by an alias to that unfolding. Its recursion descends on the old tree.

`unified_finite_at` follows the existing execution witness and constructs the
successor invariant for any requested handle. Arrow-child execution composes
these transformations through its intermediate heap. The theorem holds for
both success and failure: writes retained before a later failure also preserve
finite unfoldings.

## Acyclicity and finite models

`finite_unique` proves that two valid unfoldings of the same handle are equal.
`edge_smaller` proves that an edge strictly decreases unfolding size, including
link edges. `walk_bound` extends this to explicit walks. `no_cycle` derives false
from any nonempty walk from an allocated handle back to itself. Thus even pure
link cycles, which the old model equations permit, are excluded.

`readback` interprets variable leaves as `TVar` of their physical handle, reads
Boolean and arrow nodes, and follows aliases. The result is the finite inductive
`ty`. `readback_model_at` establishes the existing heap equation for any
requested handle, given the pointwise definition of the valuation.
`with_finite_model` constructs that valuation with a local `[@def]` function and
supplies its defining lemma. It passes the valuation and its total model
witness to a total continuation. This is explicit existential elimination; the
model witness is constructed, not assumed.

The MGU layer makes this readback canonical by preserving physical variable
identities; the earlier label-function parameter is removed. Its factorization
contract is described in `unifier-mgu.md`.

## Boundary and checks

All mathematical recursion is checked total. The new unfolding, transformation
and model construction run in ghost code. They need not be efficient runtime
algorithms, and no unfolding cache is added to the executable unifier.

Run `vox/unifier_finite_demo.ml` and `vox/unifier_finite_rejected.ml` through the
test runner; both exercise bytecode and native compilation. The demo covers
shared arrows, alias chains, variable binding, identity, occurs failures,
constructor clashes and partial mutations. Lambda inspection checks that the
new proof computations erase. The MGU layer adds factorization; principal STLC remains a later PR.
