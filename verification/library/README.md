# Verified Vox library

From a configured Vox checkout with Z3 on `PATH`:

```sh
make vox-library
```

This builds and installs the final compiler and verifies the library in
bytecode and native modes. Checking uses `-principal` except for
`vox_http`, `vox_cdcl_total`, `vox_cdcl_total_proof`, `vox_table_*`, and
`vox_verified_flat_hashtbl`: an upstream `immutable_data` inference issue
prevents those modules from compiling in principal mode. All modules undergo
refinement and termination checking. The build installs `Vox_sequence`,
`Vox_int_sequence`, `Vox_iarray`, `Vox_http_spec`, `Vox_http`, `Vox_sat_spec`,
`Vox_sat`, `Vox_cdcl`, `Vox_cdcl_total`, `Borrow`,
`Borrow_iarray`, the permission, raw-memory, atomic and verified-table
modules, and the `vox_borrow` archive
under the configured prefix's `lib/ocaml/vox`. The archive name is historical;
its modules include the ownership primitives and derived collections.

[`Vox_http`](vox_http.md) is an incremental HTTP/1.1 request parser with direct
accepted-input soundness, roundtrip, chunking, accounting, and request-separation
proofs. Its semantic module and sealed API define the human review surface.
Run its streaming demo with `verification/demos/http_stream.sh`.

[`Vox_sat`](vox_sat.md) is a bounded DPLL solver for in-memory CNFs. Its SAT
and UNSAT guarantees are checked in Vox.

[`Vox_cdcl`](vox_cdcl.md) adds first-UIP clause learning and backjumping. Its
learned clauses carry ghost derivations; UNSAT needs no runtime trace check.
[`Vox_cdcl_total`](vox_cdcl_total.md) provides the same sound answers with
Vox-checked termination and a recursion budget that returns `Unknown` when
exhausted or when an internal CDCL check fails. Its separate
`solve_with_fallback` entry point guarantees a decision on every accepted input
when the fallback depth budget is at least `n + 1`.
The [SAT review boundary](vox_sat_boundary.md) lists its complete semantic
surface. The three SAT `*_proof` interfaces are private and are not installed.

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

## Flat hash table

`Vox_verified_flat_hashtbl.Make` exposes an abstract finite map with its laws, abstract snapshots
and normal-return ownership contracts. Start with the
[ordered review surface](vox_flat_hashtbl_review.md); the public-only client
check is `verification/clients/check_flat_hashtbl_public.sh`.

## Union-find time credits

`Vox_union_find_online.Make (Credits)` provides the growing interface.
`create` takes one credit and no capacity argument. `make_set` takes eleven
credits; `find` and `union` take exactly `find_fee state` and `union_fee state`.
Operations consume the unique state and payment, returning only the result
and updated state. All surplus stays private. The fee observations are ghost
functions; the public interface exposes neither capacity nor credit tokens
owned by the state. Insertions require `size state < Bigint.of_int max_int`.

`Vox_union_find_simple.Make (Credits)` provides the same interface for a fixed
capacity supplied to `create`, with a three-credit insertion fee. Both wrappers
require exact payments, so callers split their external budget before a call.
They export membership, representative semantics, and accounting observations
for verification. Their abstract state hides the underlying resource and saved
credits. `account_bounds` bounds ticks by the account, and each operation
increases the account by exactly its advertised fee.

The online implementation doubles a ghost epoch `E` when full and uses analysis
capacity `min(E,max_int)`. No runtime nodes move. Its saved credits cover the
reserve `8n - 4(E-1)`, plus retained surplus. Each insertion deposits eight
credits. At growth it unlocks `4E`, transfers the required amount into the
potential bank, and retains the remainder. Cap coherence preserves the old
nodes' levels and indices. The checked reparameterization equation is
`Phi(new) - Phi(old) = (new_alpha - old_alpha) * sum_ranks`.
The checked doubling lemma bounds the alpha increase by one; `sum_ranks <= n`
therefore bounds the transfer by `4E`. This establishes funding before growth
at every prefix, independently of intervening finds and unions.

`Vox_union_find_online_cost.sequence` telescopes accounts with constructor-derived
operation counts. `fee_bounds` bounds each state's fees using any final population
at least its current size and the canonical inverse for that population. Together
they give, for every finite prefix with `n` insertions, `f` top-level finds and
`u` unions, with `a = alpha(max(1,n))`:

```
ticks <= 1 + 11n + (4a+12)f + (12a+36)u
```

Thus the online API supports `O(n + (f+u) alpha(n))` without an upfront population
bound, within the implementation's machine-size limit. `union_find_online.ml`
checks both sealed APIs, interleaves insertion and union across alpha-changing
and same-alpha capacity growth, and derives the final bounds from actual returned
accounts. The rejection fixture checks exact fees, state reuse, hidden savings,
nonmember queries, and the machine-size limit.

The lower-level refund API remains available:

`Vox_union_find.Make (Credits)` implements union by rank and full path
compression. Supply `Vox_big_credits.S`; issuance is available only to the
caller through `Vox_big_credits.Make ().Budget`. Bigint balances avoid an
artificial machine-integer limit on accumulated credit. `tick`, `split`, and
`merge` are checked implementations with the same uniqueness discipline as
`Vox_credits`.

`create` fixes a positive ghost capacity `N <= max_int`. `make_set` requires
room within that capacity. `find` and `union` require membership in the
owned forest. Operations consume the unique state and a unique fee token,
then return the state and surplus credit. The private state representation
and abstract heap resource prevent callers from constructing a forest or
extracting its bank. Model observations borrow the state.

Write `J_b(k,t,x) = Vox_ackermann.iter b k t x`. Its admissible domain is
`b >= 1`, `k >= 0`, and `0 <= t,x <= b`; outside this domain it returns zero.
The checked coherence lemma proves `J_b(k,t,x) = min(b,J_c(k,t,x))`
for `b <= c` on the smaller cap's domain. `alpha_bounds` exposes that the
implemented inverse is the least `a >= 1` with `J_N(a,1,1) = N`.

For comparison, define the conventional uncapped hierarchy by
`A_0(x) = x+1`, `I(k,0,x) = x`,
`I(k,t+1,x) = A_k(I(k,t,x))`, and `A_(k+1)(x) = I(k,x+1,x)`.
Induction on level and iteration count identifies `J_b(k,t,x)` with
`min(b,I(k,t,x))` on the admissible domain. This identification is an external
mathematical argument, not a separate Vox theorem against an independently
defined uncapped iterator. It identifies the implemented inverse with the
least `a >= 1` such that `A_a(1) >= N`; thresholds are 3, 7, and 2047 for
levels 1, 2, and 3.

All hierarchy, potential, and registry computations in the operations are
ghost computations. Native inspection confirms these computations disappear
from the public operations and recursive worker. Abstract token calls remain;
with the concrete `Vox_big_credits.Make` implementation, they have constant
overhead per event.

The caller supplies these fees:

| Operation | Credits |
| --- | ---: |
| `create` | 1 |
| `make_set` | 3 |
| `find` | `4a + 8` |
| `union` | `12a + 24` |

The cost model charges one tick per worker entry, owned-cell read or write,
constant-size node construction, and cell-allocation request. Entry covers
the bounded scalar work in that body. A find with `d` parent edges spends
exactly `4d + 2` ticks. Root linking
precharges seven ticks, including a link of a root to itself. Union also
charges one entry tick. The bank holds
exactly four times the forest potential. A positive-rank nonroot of rank `r`
uses level `l = max { k < a | A_k(r) <= parent_rank }` and index
`i = max { t >= 1 | I(l,t,r) <= parent_rank }`, with potential
`(a-l)r-i`. Rank-zero nonroots have zero potential; roots have potential
`ar`. The bank consists of actual credit tokens, never newly issued credit.
The returned surplus can exceed the incoming fee when compression releases
stored credit.

The proof connects this potential to the mutable heap. It proves strict rank
increase along paths, preservation of ranks and representatives under
compression, the representative change caused by union, and the exact
forest-potential decrease during compression. Counting last occurrences of
Ackermann levels bounds a path by its potential release plus `a+1`. Linking
increases potential by at most `a`. The rank-sum invariant
`sum ranks <= elements - components` bounds all ranks below capacity and
rules out rank overflow. Nonroot nodes retain their historical integer rank.

Every operation preserves an exact accounting equation: cumulative ticks
plus the current bank and the returned credit equal the previous account
plus the supplied credit. `account_bounds` establishes that cumulative ticks
are at most this account. `Vox_union_find_complexity.sequence` telescopes
a finite trace of these account increments. The executable client builds
such a trace from actual operation results through the sealed interface.
For `n` calls to `make_set`, `f` top-level finds, and `u` unions, the checked
bound is
`1 + 3n + (4a+8)f + (12a+24)u`, at most
`1 + 3n + 36a(f+u)`. Internal finds are included in the union fee.
Choosing capacity `N=n` for `n >= 1` gives
`O(n + (f+u) alpha(n))`; an empty history can use `N=1`. Increasing capacity requires a new accounting
argument; this API keeps capacity fixed.

Tick placement and coverage are part of the trusted cost model. The claim
counts instrumented unit-cost events, not CPU instructions or wall time.
Reference operations and rank arithmetic are assumed constant-time.
Heap ownership primitives, compiler refinement checking, SMT, and ghost
erasure retain their existing trust boundaries. The development adds no
axioms, `external` declarations, or `assume_` to the algorithm or proofs.
Effectful `find` uses checked recursive decreases on its finite ghost path;
termination also assumes the audited primitive bodies terminate. It does
not claim that effectful operations inhabit Vox's pure `total` mode.

### Connectivity clients

`Vox_connectivity.Make (Credits)` seals the online implementation behind
abstract elements and persistent ghost snapshots. `snapshot` observes the
partition; `contains`, `root`, and `connected` describe it. After an operation,
`added_law`, `found_law`, and `joined_law` instantiate its membership and
representative guarantees for any chosen element. New elements form fresh
singleton components; a union chooses one of the two previous representatives
and preserves every other component. Saved snapshots remain
usable after the live unique state has been consumed; they grant no mutation
permission.

The operations retain exact payments and return no refunds. The
`connectivity.ml` client keeps unspent caller credits in its own wallet and
proves conservation using `account` and `account_bounds`. The implementation's
surplus and growth reserve remain private. The signature exposes no path,
heap, rank, or reserve model.

Connectivity states are valid by construction, so its contracts state
membership through `contains (snapshot s) x` and results through `root`.
`Vox_union_find_events` records completed operations newest first: `Initialize`
(1), `Allocate` (3), `Find depth` (`4 * depth + 2`, depth nonnegative), `Link`
(7) and `Union` (1). `event_cost` proves that the ticks equal the total weight
of `events state`; together with `account_bounds` this bounds the event cost by
the account.
