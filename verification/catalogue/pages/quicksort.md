title: Quicksort
blurb: An in-place quicksort on mutable `int` arrays, sequential and parallel, proved to leave the array sorted and a permutation of its input.
status: owner-review
date: 28 September 2026
sources:
  - testsuite/tests/vox/quicksort.mli — Public interface
  - testsuite/tests/vox/quicksort.ml — Implementation: partition, the sequential recursion on slices and the parallel recursion on owned arrays
  - testsuite/tests/vox/quicksort_model.ml — Lemmas about one partition step
  - verification/library/vox_int_sequence.mli — `sorted`, `permutation`, `count` and their laws
  - verification/library/borrow.mli — Borrowed slices and owned arrays
  - verification/library/borrow.ml — Slice operations, owned-array splitting and appending, and the internal `Raw` primitives
  - verification/library/vox_parallel.mli — `fork_join`, with an ordinary polymorphic type
  - verification/library/vox_parallel.ml — `fork_join` over `Domain.Safe.spawn` (trusted)
  - design-docs/borrows-and-slices.md — Design of borrows and slices, and what the checker assumes about them
  - testsuite/tests/vox/quicksort_client.ml — Client: sorts immutable arrays sequentially and in parallel
  - testsuite/tests/vox/quicksort_frame_client.ml — Client: sorts a subrange and leaves the rest unchanged
  - testsuite/tests/vox/quicksort_rejected.ml — Rejected programs
  - testsuite/tests/vox/borrow_partial.ml — Rejected programs: loans in total and erased code
---
`Quicksort` sorts a mutable array of `int` in place. Each of its three operations is proved to leave the array sorted by `<=` and a permutation of its contents at the call, where `permutation` means that every integer occurs the same number of times. `sort` and `sort_array` run on one domain; `parallel_sort_array` sorts the two sides of a partition in separate OCaml domains. None of the three is declared `total`, and their contracts hold on normal return only. They lend the array (`Owned_array.with_mut`, `Slice.split3`) and end those loans (`Slice.finish`). In the checker's model creating a loan chooses its final contents (`Slice.final`) and ending it assumes that they equal its current contents; these are effects, not functions of the arguments, so they are not `total`. The recursion of `sort` still has a checked decreasing measure, and the partition, which only reads and swaps, is `total`. Partitioning is Lomuto's scheme with the middle element as pivot; an element equal to the pivot goes left or right depending on the parity of its position.

The array is reached through `Borrow`, a library of exclusively borrowed array slices. Its storage primitives and the equations the checker uses for them are assumed, not proved. Nothing is proved about running time or recursion depth. The parallel operation may raise (for example if a domain cannot be started), and then the array is lost to the caller.

## Client example

From the client test. `{v : t | p}` is the type `t` refined by the predicate `p`. The checker proves the refined result type from the contracts of the three calls. `Owned_array.t` is a uniquely owned mutable array: `of_iarray` copies an immutable array into one, and `into_iarray` freezes it again. `Model.of_iarray` is the list of an array's elements; here it is used only in the specification.

@code testsuite/tests/vox/quicksort_client.ml "let verified_sort" "  Owned_array.into_iarray sorted"

The rest of the test runs this on fixed and random inputs of up to 8,192 elements and compares the results with `List.sort` at run time.

## Parallelism through a polymorphic type

The parallel sort uses a fork/join whose type mentions no refinement and nothing about arrays or sorting:

@code verification/library/vox_parallel.mli "val fork_join" "'a * 'b @ unique"

`parallel_sort_array` partitions the owned array in place, splits it into three owned pieces (the left side, the pivot and the right side) with `Owned_array.split_at`, which does not copy, and sorts the two sides in separate domains. Each side's thunk states its result as a refined type, and `fork_join` instantiates `'a` and `'b` at those types, so the proofs come back through the join as ordinary values:

@code testsuite/tests/vox/quicksort.ml "    let sort_left () :" "        l, sort_right ()) in"

`left_before` and `right_before` are erased copies of the two sides' contents before sorting. After the join, `Owned_array.append` puts the pieces back together (in place, since they are adjacent pieces of one array), and `Quicksort_model.glue_partition` proves that the whole is sorted and a permutation of the input. No specification of `fork_join` beyond its OCaml type is needed. An array of at most `2 * cutoff` elements, and every array once the domain budget is used up, is sorted by the sequential `sort_array`. When one side of a partition is smaller than `cutoff`, the two sides are sorted one after the other, each with the whole budget.

A borrowed slice cannot be sorted this way: a slice is `local` to its borrow, and a thunk passed to another domain must be global, so `fork_join` cannot capture one. That is why the parallel operation takes an owned array.

## A rejected program

The parallel operation is not declared `total`, so a function declared `total` (`@ total`) cannot call it, even with one domain:

@code testsuite/tests/vox/quicksort_rejected.ml "let (blocking_sort @ total)" "();;"

@text testsuite/tests/vox/quicksort_rejected.ml "Line 2, characters 16-45:" "which is expected to be"

The same test rejects `sort` and `sort_array` inside a `total` function, and accepts `Owned_array.split_at` there. It rejects a write to a slice inside `ghost_ (...)`, which is erased code (inside `ghost_` the slice is not unique, so the uniqueness check rejects the write). `borrow_partial.ml` rejects `Slice.finish`, `Owned_array.with_mut` and `sort` in erased code, and `finish` in a lemma and in a `total` function. It also rejects two false contracts: a function that leaves its slice unchanged and claims that the result is sorted (it can prove the permutation half), and one that overwrites the first element before calling `sort` and claims that the result is a permutation of the original contents (it can prove the sorted half).

## Interface

@code testsuite/tests/vox/quicksort.mli

`(s : t) @ local unique` means that the caller passes its only reference to `s` and that `s` does not escape the call; `@@ total` marks a function declared total. For a slice `s`, `Borrow.Slice.current s` is its contents when the function is called and `Borrow.Slice.final s` is its contents when the borrow ends; both are erased values that exist only for the checker. `Borrow.Owned_array.contents a` is the contents of an owned array. `Spec` is `Vox_int_sequence`, whose laws characterize `sorted` and `permutation` completely:

@code verification/library/vox_int_sequence.mli "val sorted_adjacent" "@@ total"

@code verification/library/vox_int_sequence.mli "val count_def" "total"

@code verification/library/vox_int_sequence.mli "val permutation_count" "else true} @@ total"

@code verification/library/vox_int_sequence.mli "val count_extensional" "{u : unit | permutation left right} @@ total"

`===` is logical equality, `Bigint.t` is unbounded integers and `0Z` and `1Z` are its literals.

## Trusted base

- Beyond the `borrow.mli` operations on the shared page, the slice library relies on internal primitives declared `external` in the `Raw` module of `verification/library/borrow.ml` (`open_`, `restore`, `split`, `recombine`, `transfer`, `finish`, `length`), implemented in `runtime/borrow.c`. `open_`, `split` and `finish` are not declared `total`. Their meaning comes partly from refinements on the `external` declarations (splitting at `k` gives slices whose contents are the first `k` and the remaining elements of the parent; recombining gives a slice whose current contents are the children's final contents, concatenated) and partly from fixed equations in `verification/vox_vc.ml` (`normal_borrow_transition`): splitting a slice of length `n` at `k` gives slices of lengths `k` and `n - k`, final contents are carried across opening, splitting, recombining and restoring, and finishing a slice makes its final contents equal its current contents.
- `quicksort.ml` redeclares integer division as `external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"`. The nonzero-divisor refinement on the primitive is written by hand.
- `Vox_parallel.fork_join` is trusted OCaml: it starts a domain with `Domain.Safe.spawn` under `[@alert "-do_not_spawn_domains"]`, runs the right thunk on the calling domain, and joins the domain before it returns or raises. `Domain.Safe.spawn` itself is OxCaml's mode-checked API (the thunk must be `portable once`). The one unchecked step is `Obj.magic_unique` on the joined result: `Domain.join` returns its result `aliased`, but the domain handle is private to `fork_join`, is joined once, and the domain has finished.
- `Owned_array.split_at` and `Owned_array.append` are `external` declarations in `verification/library/borrow.ml`, implemented in `runtime/borrow.c` (`caml_borrow_owned_split`, `caml_borrow_owned_append`); their refinements (the pieces' contents are `Model.take` and `Model.drop` of the whole; an append's contents are `Model.append` of the two) are assumed. Splitting gives two owners of disjoint ranges of one backing array. Appending two adjacent pieces of one array joins them without copying; any other pair is copied into a new array. `Owned_array.into_iarray` freezes an owner that covers its whole backing array without copying and copies a piece out. This is sound only because owners are unique: the live owners of one backing array cover disjoint ranges, so an owner covering the whole array is the only one.

## Scope

- Operations: `sort` on a borrowed slice, `sort_array` and `parallel_sort_array` on an owned array. Elements are `int`, ordered by `<=`.
- None of the operations is `total`; their contracts hold on normal return. If `parallel_sort_array` raises, the caller's array is consumed.
- Parallel scheduling: `max_domains` defaults to `Domain.recommended_domain_count ()` and is clamped to between 1 and that count; `cutoff` defaults to 512 and is at least 2. A partition step starts a new domain only when more than one domain is left and both sides have at least `cutoff` elements. These parameters affect only scheduling, not the contract.
- No bound on running time, recursion depth or stack use is stated. The recursion is not tail-recursive.
- Every `Slice.get` and `Slice.set` is a call to C (`caml_borrow_get`, `caml_borrow_set`) that checks the index again at run time. The tests check results only and report no running times.
- `quicksort_iarray.ml` is a near copy that specifies slices by `int iarray` instead of lists (`Borrow_iarray`), and still sorts in parallel through `Borrow_iarray.Slice.parallel`. This page does not cover it.

## Reproduce

After `./configure --prefix=$PWD/_install`, `make install` and `./dev init`:

```
./dev test vox/quicksort_client.ml vox/quicksort_frame_client.ml vox/quicksort_rejected.ml vox/borrow_partial.ml
```

The two client tests check `Vox_sequence`, `Borrow`, `Vox_parallel`, `Vox_int_sequence`, `Quicksort_model` and `Quicksort` while compiling them, then run the clients as bytecode and native code. With this configuration the runtime has a single domain: `Domain.recommended_domain_count ()` is 1, so `parallel_sort_array` never starts a domain and runs sequentially. To run the parallel path, configure with `--enable-poll-insertion --enable-multidomain` as well.
