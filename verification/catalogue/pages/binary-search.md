title: Binary search and sorted updates
blurb: A persistent sorted array of integers whose searches, insertion and removal are proved against its elements.
status: owner-review
date: 4 October 2026
sources:
  - testsuite/tests/vox/sorted_array_model.ml — Pure sequence insertion and removal
  - testsuite/tests/vox/sorted_array.mli — Public interface
  - testsuite/tests/vox/sorted_array.ml — The abstract type and the proofs of the public contracts
  - testsuite/tests/vox/sorted_array_proofs.ml — Midpoint search, equal range, insertion and removal on plain arrays
  - testsuite/tests/vox/sorted_array_client.ml — Client, with runtime checks against lists
  - testsuite/tests/vox/sorted_array_rejected.ml — Rejected clients and their expected errors
---
`Sorted_array` is a persistent sorted array of `int`s. A client starts from `empty` and changes arrays only with `insert` and the two removals, so every array it holds is sorted: `ordered` gives `at a i <= at a j` for `0 <= i <= j < length a`. The searches are specified by the elements. `mem a v` is true exactly when some index holds `v`. `equal_range a v` returns `(first, past)` such that the elements before `first` are less than `v`, those from `first` to `past - 1` equal `v`, and the rest are greater. `find_first` and `find_last` return the first and last index holding `v`, or `None` when there is none. `insert a v` returns a position `p` and an array one longer in which the elements before `p` are unchanged, `v` is at `p`, and the rest are shifted up by one. `remove_at a p` removes the element at `p`, and `remove_one a v` removes the first occurrence of `v` or returns `None`. The search is written once, over a predicate on indices; it evaluates the predicate only strictly inside the current interval and takes the midpoint as `left + (right - left) / 2`, so it neither reads out of bounds nor overflows.

`insert` has no precondition, so a client can insert any number of times into an array whose length it does not know. It raises `Invalid_argument` if the result would be longer than an array can be. The runtime's own limit is reached first, inside `Iarray.append`, but the checker does not know that limit, so `insert` also checks at run time that `length a + 2` does not wrap around, which the hidden invariant needs. No bound on the number of comparisons is proved, only that the search terminates. The elements are `int`, compared with the built-in order; there is no comparator.

## Interface

`contents` is the complete immutable sequence observation. The pure model
uses a prefix and suffix to define each edit once: insertion puts the new
value between them; removal drops one position from the suffix.

@code testsuite/tests/vox/sorted_array_model.ml

@code testsuite/tests/vox/sorted_array.mli "val contents :" "(** Derived observation laws for client proofs. *)"

The interface gives the length and indexing observations, sortedness, and
operation contracts. `contents_length` and `contents_at` tie run-time access
to the sequence. `inserted` and `removed` require valid positions, the length
change and exact equality to the model's sequence. No Boolean traversal
or private array-copy invariant defines an edit. `inserted_at` and
`removed_at` are derived pointwise consequences.

The search predicates retain their defining equations over `length` and
`at`: membership, membership in an interval, and the equal-value range.
`at` returns 0 outside the array. Consequently an interval extending past
the end can observe 0; operation contracts only use intervals in bounds.
`length_bounds` requires a nonnegative length whose addition of 1 does not
wrap. `@@ total` declares a function that terminates without raising.
Functions used in refinements must also be stateless.

## Trusted base

- `sorted_array_proofs.ml` declares integer division locally as `external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"`. The refined type and `total` are not checked; the checker's integer encoding gives the quotient its meaning.
- Insertion and removal build their results with `Iarray.sub` and `Iarray.append`. The checker has built-in models of both (`verification/vox_encoding.ml`, `verification/vox_vc.ml`); on normal return it assumes that `Iarray.sub`'s bounds were valid and that `Iarray.append`'s result length did not overflow. They are OCaml functions in `stdlib/iarray.ml` over `caml_array_sub` and `caml_array_append` in `runtime/array.c`: `Iarray.sub` raises `Invalid_argument` on bad bounds, `caml_array_append` raises it when the result would be too long, and `Iarray.append` returns an operand unchanged when the other is empty.
- The checker's built-in meaning of `Iarray.length` and of the bounds-checked reads `Iarray.Refined.get`, `Vox_iarray.get` and `Vox_sequence.iarray_get`; the last two are `external "%array_safe_get"` declarations in `verification/library` whose refined types and `total` are not checked.
- None of the demo's files uses `assume_`.

## Scope

- Operations: `empty`, `mem`, `equal_range`, `find_first`, `find_last`, `insert`, `remove_at` and `remove_one`, with the observers `length`, `at` and `contents`. There is no constructor from an arbitrary array, no merge and no iteration.
- `insert` raises `Invalid_argument` if the result would be longer than an array can be; this is not part of its contract, which describes normal return. `remove_at` requires an index in bounds.
- `insert` does not say where among equal elements the new one goes.
- `insert`, `remove_at` and a successful `remove_one` return a new array and leave their argument unchanged (removing the only element returns the shared empty array); the three are not declared `total`, and their contracts describe normal return. The searches are `total`.
- `at` is 0 outside the array, and `occurs_between` reads `at`, so for an interval that runs past the end it can report an occurrence of 0; the contracts of the operations use only intervals in bounds. `inserted` and `removed` include valid positions and exact sequence equality; the private element traversal appears only in the proof of their observation bridge.
- Elements are `int`. No bound on the number of comparisons is proved.

## Client example

The public-only `round_trip` client inserts `value` and removes the returned
position, proving equality of the entire observed sequence with the source.
It unfolds the two pure model functions and uses the checked sequence laws
for splitting an append and composing drops. `inserted_at` and `removed_at`
also recover the original pointwise guarantee. The source can contain
arbitrary duplicates; insertion may choose any compatible position.

@code testsuite/tests/vox/sorted_array_client.ml "let round_trip :" "  result"

`insert_twice` inserts two values into an array of any length and proves the length of the result. Neither call to `insert` needs a proof.

@code testsuite/tests/vox/sorted_array_client.ml "let insert_twice :" "  result"

The same client checks every operation against a list model on all 121 sequences of length at most four over three values. It builds each array by folding `insert` over a list.

@code testsuite/tests/vox/sorted_array_client.ml "let check values =" "initial values in"

## A rejected program

A failed search cannot be used to claim membership. `(() : {u : unit | p})` asks the checker to prove `p` at that point. When `find_first` returns `None`, the checker knows that `value` does not occur, so the claim that it does is rejected.

@code testsuite/tests/vox/sorted_array_rejected.ml "let invalid_search_result" "| Some _ -> ();;"

@text testsuite/tests/vox/sorted_array_rejected.ml "Line 3, characters 13-15:" "The refinement is stated here."

The same test also rejects removing index 0 from `empty`, passing a plain `int iarray` as a `Sorted_array.t`, and claiming that `insert` puts the new element at position 0.

## Reproduce

```
./dev test vox/sorted_array_client.ml vox/sorted_array_rejected.ml
```

`sorted_array_client.ml` compiles `Vox_sequence`, `Vox_int_sequence`, `Vox_iarray`, the proofs, the module and the client, and runs the client as bytecode and native code. `sorted_array_rejected.ml` is an expect test that compiles the two interfaces and checks the four rejections.
