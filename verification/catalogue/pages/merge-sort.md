title: Merge sort
blurb: A generic merge sort on immutable lists, proved to return a sorted permutation of its input while spending at most n⌈log₂ n⌉ erased credits, one per comparator call.
status: review-pending
date: 27 September 2026
sources:
  - verification/library/vox_merge_sort.mli — Public interface
  - verification/library/vox_merge_sort.ml — Implementation: split, merge and the recursive sort
  - verification/library/vox_ordered_sequence.ml — `count`, `permutation` and `sorted` for a total preorder, with their lemmas
  - verification/library/vox_merge_proofs.ml — Lemmas for one split or merge step
  - verification/library/vox_credits.mli — Credit tokens
  - verification/library/vox_credits.ml — Credit tokens: implementation
  - verification/library/vox_sort_cost.ml — `height`, `budget` and the proof that `height n` is ⌈log₂ n⌉
  - verification/library/README.md — The credit discipline and the cost model (section "Comparison credits and merge sort")
  - testsuite/tests/vox/merge_sort.ml — Client: integer and ranked-record sorts
  - testsuite/tests/vox/merge_sort_rejected.ml — Rejected programs
---
`Vox_merge_sort.Make` is a merge sort on immutable lists, generic in the element type and its order. Its `sort` is declared `total`, so the checker proves that it terminates, and it returns a list that is sorted by the order `O.le`, has the input's length, and is a permutation of the input. Permutation compares complete values, not keys: two records with equal keys and different payloads count as different elements.

The comparator is a functor argument, `Compare.compare`, which must return `O.le left right` and consume one credit from an erased token. `sort` requires a token holding at least `budget n = n * height n` credits, where `n` is the length of the list, and returns a token that has lost at most `budget n` credits. `Vox_sort_cost` proves that `height n` is ⌈log₂ n⌉ for `n ≥ 1` (it is 0 for `n = 0`). Because the functor can split, merge and spend credits but cannot create them, and `Compare.compare` is its only run-time access to the order, the sort makes at most n⌈log₂ n⌉ calls to `Compare.compare`. That last step is an argument from the signatures, not a statement the checker proves; the checked statement is the credit bound.

Not claimed: stability (splitting alternates elements, so equal-keyed elements can change order), any cost other than comparator calls, and stack depth (`split` and `merge` are not tail-recursive). The credit operations are not all erased: `C.split` and `C.merge` are called outside `ghost_`, so erasure removes their token arguments but not the calls.

## Client example

From the client test, with integers ordered by `<=`. `O.le` is defined with `let[@def]`, which also generates the equation `le_def` used in the proofs; `ghost_ (...)` is proof code, checked and then erased. `Vox_credits.Make ()` creates a fresh token type. Each comparison requires a token with `C.credits t > 0` and spends one credit with `C.tick`. `#{...}` is an unboxed record, and `@ unique total ghost` on the token means it is erased and used at most once.

@code testsuite/tests/vox/merge_sort.ml "module O = struct" "module Sort = Vox_merge_sort.Make (O) (C) (Compare)"

A caller with enough credits gets the full contract, here specialized to one element's count. `{v : t | p}` is the type `t` refined by the predicate `p`, and `===` is logical equality.

@code testsuite/tests/vox/merge_sort.ml "let (funded_sort @ total)" "  #{Sort.values = output; state}"

The test funds the token with `C.Budget.create`, which is not available inside the functor. It justifies the amount with `assume_`, which checks the stated predicate at run time, and sorts lists of up to 128 elements with exactly `budget n`, one more, and `max_int` credits, comparing the results with `List.sort`.

## A rejected program

`two` (in the client test) makes two comparisons and returns a token with two fewer credits. After it has spent both credits of a two-credit token, a third comparison is rejected:

@code testsuite/tests/vox/merge_sort_rejected.ml "let third () =" "Compare.compare 1 0 (state);;"

Compiled as a file after `open Merge_sort`, the installed compiler prints:

```
File "third.ml", line 9, characters 22-29:
9 |   Compare.compare 1 0 (state)
                          ^^^^^^^
Error: Refinement could not be proved (counterexample)
File "merge_sort.ml", line 26, characters 30-45:
  The refinement is stated here.
```

The test also rejects claims that `[]` is a permutation of `[1]`, that `[1; 1]` is a permutation of `[1]`, and that `[a; b]` is a permutation of `[a; a]` when the records `a` and `b` have equal ranks and different payloads. It checks that `false` cannot be proved after splitting and merging credits, after the two `height` lemmas, or after a funded sort. No test rejects a call to `sort` with too few credits.

## Interface

@code verification/library/vox_merge_sort.mli

`O` must be a total preorder: `le` is a ghost function (`@ ghost`, usable only in specifications) with proofs of reflexivity, totality and transitivity. The `P` laws characterize `count`, `permutation` and `sorted` completely. `S.length` is the length of a list as a `Bigint.t`, an unbounded integer. The credit tokens have this signature, which has no operation that adds credits:

@code verification/library/vox_credits.mli "module type S = sig" "end"

`Vox_credits.Make ()` also has `Budget.create`, which makes a token with any nonnegative number of credits; the sort receives only `Vox_credits.S`. The budget is defined in `Vox_sort_cost`, which has no `.mli`:

@code verification/library/vox_sort_cost.ml "let[@def] rec height" "let[@def] budget"

`height_bound` and `height_minimal` in the same file prove `n ≤ power (height n)` and, for `n > 1`, `power (height n - 1) < n`.

## Trusted base

- The link between credits and comparator calls, as argued above: `Compare.compare` is the only run-time access to `O.le`, each call spends exactly one credit, and `Vox_credits.S` has no operation that increases the credits of the live tokens (`empty` makes zero, `split` and `merge` conserve, `tick` spends). Tokens are unique, so they cannot be copied. Each `Vox_credits.Make ()` has its own token type, so the sort cannot use credits from another instance. None of this is a checked statement.
- Otherwise nothing beyond the shared base: the merge-sort, ordered-sequence, credit and cost modules contain no `external`, `assume_` or suppressed warning. The two `assume_` calls in the client test are checked at run time.

## Scope

- One operation, `sort`, and the laws in `P`. There is no merge of two sorted lists, no sorting of arrays and no deduplication.
- Elements have kind `immutable_data mod total`, so mutable records and closures cannot be sorted. `Compare.compare` must be `total`.
- Credits are machine `int`s. `budget n` is compared as a `Bigint.t`, so a list whose budget exceeds `max_int` cannot be funded.
- Only calls to `Compare.compare` are counted, at one credit each. Work inside the comparator and the cost of splitting and merging lists are not.
- Not stable. `split` and `merge` recurse to a depth proportional to the list length; the tests stop at 128 elements.
- The client test runs as bytecode only.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/merge_sort.ml vox/merge_sort_rejected.ml
```

Both tests check `Vox_sequence`, `Vox_ordered_sequence`, `Vox_credits`, `Vox_merge_proofs`, `Vox_sort_cost` and `Vox_merge_sort` while compiling them. `verification/library/build.sh _install` also checks these modules as part of the installed library.
