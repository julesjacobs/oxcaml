# Verified borrow library

From a configured Vox checkout with Z3 on `PATH`:

```sh
make vox-library
```

This builds and installs the final compiler, verifies the library with
`-principal` in bytecode and native modes, and installs `Vox_sequence`,
`Vox_int_sequence`, `Vox_iarray`, `Borrow`, `Borrow_iarray`, and `vox_borrow`
under the configured prefix's `lib/ocaml/vox`.

With the worktree-local prefix from the agent guide, compile a client with:

```sh
_install/bin/ocamlc -extension refinement_types \
  -I _install/lib/ocaml/vox vox_borrow.cma client.ml -o client.byte
_install/bin/ocamlopt -extension refinement_types \
  -I _install/lib/ocaml/vox vox_borrow.cmxa client.ml -o client.exe
```

The library needs this compiler's storage primitives. Configure the compiler
with `--enable-poll-insertion --enable-multidomain` for actual parallel domains.
`Slice.parallel false` is also available with the single-domain runtime.

[The design](../../design-docs/borrows-and-slices.md) explains ownership,
current/final models, callback postconditions, the storage boundary, and the
normal-return correctness guarantee. Complete verified clients live in
`testsuite/tests/vox`: start with `borrow_demo.ml`, then `borrow_ranges.ml`,
`borrow_validation.ml`, and `quicksort_client.ml`.

Proof-only observations and lemma calls must be enclosed in `ghost_`. A
`Slice.snapshot` is a real copy suitable for executable `assume_` checks.

`Vox_sequence` contains polymorphic list operations and decomposition laws.
`Vox_int_sequence` adds integer bounds, sortedness, and permutation laws. Its
abstract `multiset` model exposes `bag`, `multiplicity`, and list `count`.
`permutation_count` derives equal counts from permutation; `count_extensional`
proves permutation from a total proof function establishing equal counts at
any integer. Clients do not depend on canonical insertion or its ordering.

`collection_theory.ml` uses those laws to verify rotation, while quicksort
uses them for slice swaps and recombination. Only the three partition lemmas
remain in `quicksort_model.ml`. Integer ordering and counting remain specialized
to integers; the list decomposition laws apply to any `immutable_data`.

`Vox_sequence` specifies lists with `Bigint.t` indices; `Vox_iarray` specifies
ordinary iarrays with `int` indices. Both offer predicate introduction,
lookup, splitting, and update laws through a `For_all` functor:

```ocaml
module Nonnegative = struct
  type element = int
  let[@def] test (value : int) = value >= 0
end
module Lists = Vox_sequence.For_all (Nonnegative)
module Arrays = Vox_iarray.For_all (Nonnegative)
```

`Lists.holds values` and `Arrays.holds values` state that every element satisfies
`Nonnegative.test`. Their `intro` lemmas accept total pointwise proofs, and
`get` specializes the property to one index. Put proof calls in `ghost_`.
`Lists.filter` has predicate, length, concatenation, and idempotence laws.
`Vox_sequence.Map` provides length, lookup, and concatenation laws;
`Vox_sequence.Fold` provides the right-fold concatenation law. Bind their
predicate, mapping, or folding argument to a named module. Functors keep these
operations usable in refinements without encoding functions as SMT data.

`Vox_int_sequence` and `Vox_iarray.Int` provide `ordered` for arbitrary ordered
indices, sorted slices, and updates whose value lies between its neighbors.
`Vox_int_sequence.insert_sorted` exposes the existing checked insertion proof.
`Vox_iarray.to_list` delegates to `Vox_sequence.of_iarray`; its length, lookup,
update-lookup, and slice-lookup laws provide an explicit conversion boundary.

`collection_surface.ml` exercises map/filter and predicate-preserving slice
updates. `collection_functions.ml` and the `hof_*` fixtures compare explicit
function APIs for partial-callback map/fold contracts.
`sorted_array_client.ml` uses the shared ordering library for binary search,
insertion, and removal with exact content contracts. `queue_client.ml` uses
shared append laws; `quicksort_iarray_model.ml` uses `Int.sorted_glue` to
recombine sorted partitions. These changes add checked library definitions and
lemmas; they add no trusted collection axiom or SMT encoding rule.

The [proof inventory](../../design-docs/vox-v1.md#proof-inventory-and-higher-order-contracts)
distinguishes total model functions, possibly partial callbacks specified by
total relations, and fold invariants. The relational prototypes are not yet a
public library API.

The [HOF idiom study](../../design-docs/hof-idioms.md) compares relational,
IH, total-model, preservation-proof, and ghost-evidence APIs.
`vox_traversal.ml` and its interface are experimental checked implementations;
they are not yet included in the installed library.
