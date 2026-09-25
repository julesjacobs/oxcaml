# Verified Vox library

From a configured Vox checkout with Z3 on `PATH`:

```sh
make vox-library
```

This builds and installs the final compiler and verifies the library in
bytecode and native modes. Checking uses `-principal` except for
`vox_cdcl_total`, `vox_table_*`, and `vox_verified_flat_hashtbl`: an upstream
`immutable_data` inference issue
prevents those modules from compiling in principal mode. All modules undergo
refinement and termination checking. The build installs `Vox_sequence`,
`Vox_int_sequence`, `Vox_iarray`, `Vox_sat`, `Vox_cdcl`, `Vox_cdcl_total`, `Borrow`,
`Borrow_iarray`, the permission, raw-memory, atomic and verified-table
modules, and the `vox_borrow` archive
under the configured prefix's `lib/ocaml/vox`. The archive name is historical;
its modules include the ownership primitives and derived collections.

[`Vox_sat`](vox_sat.md) is a bounded DPLL solver for in-memory CNFs. Its SAT
and UNSAT guarantees are checked in Vox.

[`Vox_cdcl`](vox_cdcl.md) adds first-UIP clause learning and backjumping. Its
learned clauses carry ghost derivations; UNSAT needs no runtime trace check.
[`Vox_cdcl_total`](vox_cdcl_total.md) provides the same sound answers with
Vox-checked termination and a recursion budget that returns `Unknown` when
exhausted.

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

## Comparison credits and merge sort

`Vox_credits.Make ()` creates a fresh abstract token type. Its private balance
is a nonnegative machine integer in a ghost record. `credits (borrow_ token)`
observes that balance without consuming the token. `tick` consumes a positive
balance and returns one fewer credit. `split` partitions a balance; `merge`
consumes two tokens and returns their sum. The merge precondition requires
both operands and their machine sum to be nonnegative: with nonnegative
operands, a wrapping addition would be negative. `split` and `merge` preserve
the sum of live credit; `empty` adds zero and `tick` consumes one credit.
Discarding a token discards its credit. Balances remain nonnegative.

The driver can issue positive credit with `Budget.create`. Algorithms receive
only `Vox_credits.S`, which omits that constructor and permits only zero-credit
creation. Each application of `Make` has a distinct token type, so an algorithm
cannot replenish its input from a new instance. Uniqueness prevents reuse;
discarding credit is allowed. The accounting boundary must exclude any other
source of unaccounted tokens of the same type.

`Vox_merge_sort.Make (Order) (Credits) (Compare)` sorts immutable lists. `Order`
supplies a ghost total preorder and checked reflexivity, totality and
transitivity laws. Every call to `Compare.compare` requires positive credit
and returns exactly one fewer credit. The sort establishes termination,
sortedness, preservation of length and the multiplicity of every complete
element, and a comparison bound of `n * height n`, independently of surplus
initial credit. No positive credit is issued inside the functor.

`Vox_sort_cost.height` is zero for sizes zero and one. Its checked bounds
establish `2^(height n - 1) < n <= 2^(height n)` for `n > 1`, so the bound is
`n * ceil(log2 n)` for positive sizes; the empty-list budget is zero.
Arithmetic for this theorem uses `Bigint`; funding and
splitting require the relevant amounts to fit the supplied machine-int
balance. Inside the sorting implementation, budget calculations, scalar
termination measures and functional proofs are erased with `ghost_`.
Executable fixtures separately validate initial budgets with `assume_`;
those computations remain at runtime.

`Vox_ordered_sequence.Make` defines permutation by equal counts over the
finite union of both lists' supports. Its checked elimination lemma gives
equal counts at any element. Equality here is full logical equality, not
comparison equivalence. The ranked-record fixture preserves payloads and
repeated records even when their ranks compare equal. Alternating splitting
does not promise stable sorting.

This counts invocations of `Compare.compare` made by the sorting implementation,
with unit cost. Work or further comparisons inside the comparator and other
sorting operations are not charged by this cost model;
a bound on total work would require instrumentation for those operations.
Ghost representation erasure alone does not promise elimination of every
out-of-line token call. The comparison and sort results use unboxed records.

The implementation adds no trusted credit or sorting primitive. See
`time_credits.ml`, `time_credits_rejected.ml`, `merge_sort.ml`, and
`merge_sort_rejected.ml` in the Vox fixtures for borrowed observations,
splitting and merging, overflow and reuse
rejections, and integer and ranked-record clients.
