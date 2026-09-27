title: Binary search and sorted updates
blurb: A persistent sorted array of integers whose searches, insertion and removal are proved against its elements.
status: owner-review
date: 27 September 2026
sources:
  - testsuite/tests/vox/sorted_array.mli — Public interface
  - testsuite/tests/vox/sorted_array.ml — The abstract type and the proofs of the public contracts
  - testsuite/tests/vox/sorted_array_proofs.ml — Midpoint search, equal range, insertion and removal on plain arrays
  - testsuite/tests/vox/sorted_array_client.ml — Client, with runtime checks against lists
  - testsuite/tests/vox/sorted_array_rejected.ml — Rejected clients and their expected errors
---
`Sorted_array` is a persistent sorted array of `int`s. A client starts from `empty` and changes arrays only with `insert` and the two removals, so every array it holds is sorted: `ordered` gives `at a i <= at a j` for `0 <= i <= j < length a`. The searches are specified by the elements. `mem a v` is true exactly when some index holds `v`. `equal_range a v` returns `(first, past)` such that the elements before `first` are less than `v`, those from `first` to `past - 1` equal `v`, and the rest are greater. `find_first` and `find_last` return the first and last index holding `v`, or `None` when there is none. `insert a v` returns a position `p` and an array one longer in which the elements before `p` are unchanged, `v` is at `p`, and the rest are shifted up by one. `remove_at a p` removes the element at `p`, and `remove_one a v` removes the first occurrence of `v` or returns `None`. The search is written once, over a predicate on indices; it evaluates the predicate only strictly inside the current interval and takes the midpoint as `left + (right - left) / 2`, so it neither reads out of bounds nor overflows.

`insert` has no precondition, so a client can insert any number of times into an array whose length it does not know. It raises `Invalid_argument` if the result would be longer than an array can be. The runtime's own limit is reached first, inside `Iarray.append`, but the checker does not know that limit, so `insert` also checks at run time that `length a + 2` does not wrap around, which the hidden invariant needs. No bound on the number of comparisons is proved, only that the search terminates. The elements are `int`, compared with the built-in order; there is no comparator.

## Client example

From the client. `round_trip` inserts `value` and removes it again, and proves that the element at `index` is unchanged. `(x : a) -> b` names the argument so that `b` can mention it. `{u : unit | p}` is `unit` refined by the predicate `p`: an argument of this type is a proof of `p`, and `@ ghost` marks it as erased at runtime. Passing `()` for it asks the checker to prove `p`. `ghost_ (...)` is proof code, checked and then erased; here it applies `edited_at` to the removal and to the insertion. `length_bounds source` gives `0 < length source + 1`, so that `position <= length source` is a valid index of the array returned by `insert`, which is one longer.

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

## Interface

@code testsuite/tests/vox/sorted_array.mli

`length`, `at`, `occurs`, `occurs_between`, `range_spec`, `edited` and `edit_suffix` are total functions used in the contracts; `length` and `at` are primitive, and the `_equation` lemmas define the other five in terms of them. `length` and `at` are also the run-time accessors, which the client uses to read arrays back. `length_bounds` says that `length a` is nonnegative and that `length a + 1` does not wrap around. `at` returns 0 outside the array (`at_outside`) instead of raising, and `occurs_between a v i j` asks whether `v` occurs at an index from `i` to `j - 1`; it is false when `i` is negative. `contents` is an erased view of the array as a `Vox_sequence.t`, a list, tied to `length` and `at` by `contents_length` and `contents_at`; no operation contract uses it. `@@ total` declares a function total: it terminates without raising or touching mutable state, which lets it appear in refinements.

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
- `at` is 0 outside the array, and `occurs_between` reads `at`, so for an interval that runs past the end it can report an occurrence of 0; the contracts of the operations use only intervals in bounds. Likewise `edited` alone does not require the lengths or the position to be valid; the contracts of `insert` and the removals state those separately.
- Elements are `int`. No bound on the number of comparisons is proved.

## Reproduce

```
./dev test vox/sorted_array_client.ml vox/sorted_array_rejected.ml
```

`sorted_array_client.ml` compiles `Vox_sequence`, `Vox_int_sequence`, `Vox_iarray`, the proofs, the module and the client, and runs the client as bytecode and native code. `sorted_array_rejected.ml` is an expect test that compiles the two interfaces and checks the four rejections.
