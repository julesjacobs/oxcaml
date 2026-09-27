title: Quicksort
blurb: An in-place quicksort on mutable `int` arrays, sequential and parallel, proved to leave the array sorted and a permutation of its input.
status: review-pending
date: 27 September 2026
sources:
  - testsuite/tests/vox/quicksort.mli — Public interface
  - testsuite/tests/vox/quicksort.ml — Implementation: partition, recursion and the sequential and parallel runners
  - testsuite/tests/vox/quicksort_model.ml — Lemmas about one partition step
  - verification/library/vox_int_sequence.mli — `sorted`, `permutation`, `count` and their laws
  - verification/library/borrow.mli — Borrowed slices and owned arrays
  - verification/library/borrow.ml — Slice operations, the internal `Raw` primitives and `Slice.parallel`
  - design-docs/borrows-and-slices.md — Design of borrows and slices, and what the checker assumes about them
  - testsuite/tests/vox/quicksort_client.ml — Client: sorts immutable arrays sequentially and in parallel
  - testsuite/tests/vox/quicksort_frame_client.ml — Client: sorts a subrange and leaves the rest unchanged
  - testsuite/tests/vox/quicksort_rejected.ml — Rejected programs
---
`Quicksort` sorts a mutable array of `int` in place. Each of its four operations is proved to leave the array sorted by `<=` and a permutation of its contents at the call, where `permutation` means that every integer occurs the same number of times. `sort` and `sort_array` run on one domain and are declared `total`, so the checker also proves that they terminate. `parallel_sort` and `parallel_sort_array` sort the two sides of a partition in separate OCaml domains; their contract holds on normal return only. Partitioning is Lomuto's scheme with the middle element as pivot; an element equal to the pivot goes left or right depending on the parity of its position.

The array is reached through `Borrow`, a library of exclusively borrowed array slices. Its storage primitives and the equations the checker uses for them are assumed, not proved. Nothing is proved about running time or recursion depth. The parallel operations may raise (for example if a domain cannot be started), and then the array is lost to the caller.

## Client example

From the client test. `{v : t | p}` is the type `t` refined by the predicate `p`. `let refine_ x = e in` binds `x` to the value of `e` and keeps the refinement of `e`'s type as a known fact; `refine_ result` at the end checks that `result` has the refined result type. `Owned_array.t` is a uniquely owned mutable array: `of_iarray` copies an immutable array into one, and `into_iarray` freezes it again. `Model.of_iarray` is the list of an array's elements, used only in specifications.

@code testsuite/tests/vox/quicksort_client.ml "let verified_sort" "  refine_ result"

The rest of the test runs this on fixed and random inputs of up to 8,192 elements and compares the results with `List.sort` at run time.

## A rejected program

The parallel operations are not declared `total`, so a function declared `total` (`@ total`) cannot call them, even with one domain:

@code testsuite/tests/vox/quicksort_rejected.ml "let (blocking_sort @ total)" "();;"

@text testsuite/tests/vox/quicksort_rejected.ml "Line 2, characters 23-52:" "which is expected to be"

The same test rejects a non-terminating callback passed to `Owned_array.with_mut` inside a `total` function, and a write to a slice inside `ghost_ (...)`, which is erased code. `quicksort_rejected.ml` contains no false sortedness or permutation claim.

## Interface

@code testsuite/tests/vox/quicksort.mli

`(s : t) @ local unique` means that the caller passes its only reference to `s` and that `s` does not escape the call; `@@ total` marks a function declared total. For a slice `s`, `Borrow.Slice.current s` is its contents when the function is called and `Borrow.Slice.final s` is its contents when the borrow ends; both are erased values that exist only for the checker. `Borrow.Owned_array.contents a` is the contents of an owned array. `Spec` is `Vox_int_sequence`, whose laws characterize `sorted` and `permutation` completely:

@code verification/library/vox_int_sequence.mli "val sorted_adjacent" "@@ total"

@code verification/library/vox_int_sequence.mli "val count_def" "total"

@code verification/library/vox_int_sequence.mli "val permutation_count" "else true} @@ total"

@code verification/library/vox_int_sequence.mli "val count_extensional" "{u : unit | permutation left right} @@ total"

`===` is logical equality, `Bigint.t` is unbounded integers and `0Z` and `1Z` are its literals.

## Trusted base

- Beyond the `borrow.mli` operations on the shared page, the slice library relies on internal primitives declared `external` in the `Raw` module of `verification/library/borrow.ml` (`open_`, `restore`, `split`, `recombine`, `transfer`, `finish`, `length`), implemented in `runtime/borrow.c`. The checker gives them fixed equations in `verification/vox_vc.ml` (`normal_borrow_transition`): for example, splitting a slice of length `n` at `k` gives slices of lengths `k` and `n - k`, recombining them gives back the parent with the children's final contents as its current contents, and finishing a slice makes its final contents equal its current contents.
- `quicksort.ml` redeclares integer division as `external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"`. The nonzero-divisor refinement on the primitive is written by hand.
- `parallel_sort` runs through `Borrow.Slice.parallel`, which starts a domain with `Domain.Safe.spawn` under `[@alert "-do_not_spawn_domains"]` and hands the left slice to it with `Raw.transfer`, which turns a `local` slice into a global one by returning the same handle. This is safe only because slice handles are heap-allocated and `Slice.parallel` joins the domain before it returns or raises. The right half runs on the calling domain; the left half's result comes back through `Domain.join`.

## Scope

- Operations: `sort` and `parallel_sort` on a borrowed slice, `sort_array` and `parallel_sort_array` on an owned array. Elements are `int`, ordered by `<=`.
- `sort` and `sort_array` are `total`. The parallel operations are not; if one raises, the caller's array or slice is consumed.
- Parallel scheduling: `max_domains` defaults to `Domain.recommended_domain_count ()` and is clamped to between 1 and that count; `cutoff` defaults to 512 and is at least 2. A partition step starts a new domain only when more than one domain is left and both sides have at least `cutoff` elements. These parameters affect only scheduling, not the contract.
- No bound on running time, recursion depth or stack use is stated. The recursion is not tail-recursive.
- Every `Slice.get` and `Slice.set` is a call to C (`caml_borrow_get`, `caml_borrow_set`) that checks the index again at run time. The tests check results only and report no running times.
- `quicksort_iarray.ml` is a near copy that specifies slices by `int iarray` instead of lists (`Borrow_iarray`). This page does not cover it.

## Reproduce

After `./configure --prefix=$PWD/_install`, `make install` and `./dev init`:

```
./dev test vox/quicksort_client.ml vox/quicksort_frame_client.ml vox/quicksort_rejected.ml
```

The two client tests check `Vox_sequence`, `Borrow`, `Vox_int_sequence`, `Quicksort_model` and `Quicksort` while compiling them, then run the clients as bytecode and native code. The frame client needs a runtime with multiple domains.
