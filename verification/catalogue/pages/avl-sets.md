title: AVL sets
blurb: Persistent AVL trees of integers proved to act as sets: membership after `add` and `union` is exact, and `equal` holds exactly when two sets have the same members.
status: review-pending
date: 27 September 2026
sources:
  - testsuite/tests/vox/int_set_intf.mli — Public signatures `Operations` and `Extensional`
  - testsuite/tests/vox/avl_sets.mli — The module's interface, `Extensional`
  - testsuite/tests/vox/avl_sets.ml — Implementation and proofs: sorted-list model, tree invariant, rotations, set operations
  - testsuite/tests/vox/collections_boundary_client.ml — Client: the order of insertions does not matter
  - testsuite/tests/vox/avl_set_client.ml — Client with runtime checks against lists
  - testsuite/tests/vox/avl_stdlib_set.ml — Membership compared with `Set.MakeTotal`
  - testsuite/tests/vox/collections_boundary.ml — Public-only compile of the collection clients, rejected clients and erasure checks
---
`Avl_sets` is a persistent set of `int`s stored as an AVL tree. Its interface is stated in terms of membership alone: `lookup x empty` is false, `lookup x (add y s)` is `x = y || lookup x s`, and `lookup x (union s t)` is `lookup x s || lookup x t`. `equal s t` is true exactly when `s` and `t` have the same members, which can hold for trees of different shape. Inside the implementation, every operation is proved to keep the tree ordered and balanced with correct cached heights, and to agree with a sorted-list model. The abstract type `t` hides both the tree and these invariants.

`size` is specified only at zero: `size s = 0Z` exactly when `s` is empty. An implementation returning `1Z` or `-1Z` for every nonempty set would satisfy the interface. The implementation counts nodes, and proves internally that the count is the length of the element list, but that theorem is not exported. There is no `remove`, iteration or conversion to a list, and no bound on running time or tree height is stated.

## Client example

From a client that sees only the public interface. `@ total` marks a function that terminates without raising or touching mutable state; only such functions can appear in refinements. `{u : unit | p}` is `unit` refined by the predicate `p`, so a total function returning it is a lemma proving `p`. `===` is logical equality. `extensional` takes a function proving that two sets agree on every `query` and concludes that they are `equal`. The client calls `set_commutes` inside `ghost_ (...)`, which marks proof code: it is checked and then erased.

@code testsuite/tests/vox/collections_boundary_client.ml 38-54

## A rejected program

`equal` is not structural equality. This client assumes `equal a b` and claims `a === b`. It is compiled against the public interface only.

@code testsuite/tests/vox/collections_boundary.ml "(* avl_structural_equality *)" "|}]"

## Interface

@code testsuite/tests/vox/int_set_intf.mli "module type Operations = sig" "end"

@code testsuite/tests/vox/int_set_intf.mli "module type Extensional = sig" "end"

`avl_sets.mli` is the single line `include Int_set_intf.Extensional`. `(x : a) -> b` names the argument so that `b` can mention it, and `@@ total` declares a value total. The premise of `extensional` must itself be a total function, marked `@ total`. `Bigint.t` is the type of unbounded integers and `0Z` is a `Bigint` literal. The same file defines a third signature, `Canonical`, in which sets with the same members are logically equal; `Avl_sets` does not implement it.

## Trusted base

- The AVL proof trusts nothing beyond the shared base. `avl_sets.ml`, `int_set_intf.mli` and the two clients contain no `assume_`, `Obj.magic`, `external` or suppressed warning.
- `avl_stdlib_set.ml`, compiled by the same test, proves for one query at a time that membership in `Avl_sets` and in the standard library's `Set.MakeTotal` still agree after `add` and `union` if they agreed before (for `add`, given a proof that the set's comparison agrees with `=` at that key), and checks keys 1–3 at run time. That functor wraps the unverified `Set.Make` and marks its functions `total` through `%identity` externals (`stdlib/set.ml`), and the checker supplies the meaning of its membership. The comparison depends on this; the AVL proof does not.

## Scope

- Operations: `empty`, `lookup`, `add`, `union`, `size` and `equal`, with the laws `lookup_empty`, `lookup_add`, `lookup_union`, `equal_lookup`, `extensional` and `size_zero`. There is no `remove`, `fold`, iteration, minimum or conversion to a list.
- Elements are `int`; there is no functor over an ordered type.
- `size` is specified only at zero (see above).
- `union s t` inserts the elements of `s` into `t` one at a time; it does not split and join trees. `equal` builds the in-order element lists of both trees and compares them.
- Cached heights and `size` are `Bigint.t` values, so rebuilding an internal node calls the C runtime (`Bigint.add`) to compute its height; a new leaf gets the literal `1Z`. With unbounded integers the proof needs no overflow argument.
- Every function in the interface is `total`. No running-time, height or allocation bound is exported.

## Reproduce

```
./dev test vox/avl_sets.ml vox/collections_boundary_client.ml vox/collections_boundary.ml
```

`avl_sets.ml` compiles the interface, the implementation, `avl_set_client.ml` and `avl_stdlib_set.ml` as bytecode, runs them and compares the output with `avl_sets.reference`. `collections_boundary_client.ml` compiles the client above, with the quicksort demo, and runs it as bytecode and native code. `collections_boundary.ml` compiles the collection clients against the public interfaces only, with and without `-principal`, links and runs them with both compilers, requires the rejected programs (this one among them) to fail with their exact errors, and checks in the emitted Lambda that `add`, `union`, `lookup` and the tree functions call no validity or model proof.
