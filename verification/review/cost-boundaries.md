# Cost-demo review boundaries

Baseline: committed union-find PR #194, `9d5d8fca7a`. The changes retain the
online potential proof and the final Pro review fixes. Existing lower-level
forest APIs remain available for diagnostics and proofs; ordinary connectivity
clients use `Vox_connectivity`.

## Merge sort: ordered transitive review surface

1. `stdlib/bigint.mli`: mathematical integer operations; `stdlib/stdlib.mli`:
   machine integers, lists, logical equality and allocation conventions.
2. `verification/library/vox_sequence.mli`: `length` and `length_def`.
3. `verification/library/vox_credits.mli`: affine balance, tick, splitting,
   merging, machine bounds, generative tokens and separate `Budget.create`.
4. `verification/library/vox_sort_cost.ml`: definitions of `height`, `power`,
   and `budget`, and statements of `height_bound` and `height_minimal`.
   Their proof bodies and `bounded_int` are proof-internal.
5. `verification/library/vox_merge_sort.mli`: complete preorder premises,
   recursive multiplicity/permutation/sortedness equations, comparator charge,
   actual `sort` contract. It exposes no auxiliary merge or permutation proofs.

These files completely specify the logical result. `vox_ordered_sequence.ml`,
`vox_merge_proofs.ml`, and `vox_merge_sort.ml` supply checked proofs and execution.
The instrumentation audit additionally reads the comparator call sites in
`vox_merge_sort.ml`: every invocation spends exactly one credit; no positive
credit is minted there. This source audit is necessary to identify the measured
operations. The credit theorem does not itself attach a time cost to a call.

The order is a total preorder. Permutation uses full logical equality, including
payloads that compare equally; no stability theorem is claimed. For `n > 0`,
`height n = ceil(log2 n)`; empty lists have zero budget. Sorting preserves length
and multiplicity and uses at most `n * height n` comparator calls. This is
independent of surplus funding. The comparator's internal work, list work,
allocation and wall-clock time are outside the count.

Only the caller retains `Budget.create`. The algorithm receives `Vox_credits.S`,
which cannot issue positive credit. The accounting boundary excludes additional
unaccounted tokens of the same type. Initial credit and split amounts must fit
machine integers; the budget formula uses mathematical integers. Existing test
clients check their input budget at runtime with `assume_`; that check is outside
the sorting theorem. Totality is in the Vox model; allocation failure and stack
exhaustion are not separately modeled or bounded.

## Online union-find: ordered transitive review surface

1. `stdlib/bigint.mli` and `stdlib/stdlib.mli`: mathematical integers, bounded
   machine integers, logical equality and allocation conventions.
2. `verification/library/vox_big_credits.mli`: affine unbounded balances,
   consuming operations, generative tokens and caller-only positive issuance.
3. `verification/library/vox_ackermann.ml`: definitions of `minimum`, `iter`
   and `below`, and the contract of `inverse`. Other functions are proof
   machinery. The recursive equations and invalid-input cases are part of the
   specification; the search implementation and coherence proofs are not.
4. `verification/library/vox_union_find_online_cost.ml`: `find_fee`,
   `union_fee`, `budget`, `operation`, `step`, `same`, `fee`, `trace`,
   `final_account`, `count`, `total_fee`, and the `sequence` contract.
   Telescoping/counting proof bodies are proof-internal.
5. `verification/library/vox_connectivity.mli`: abstract handles and snapshots,
   complete pointwise membership/representative transition laws, initial
   emptiness, connectivity equation, fee bounds, accounting and executable
   operation contracts.
6. Instrumentation/primitive boundary: `verification/library/pref.mli` and
   `ghost_pref.mli` (allocation, physical handles, owned read/write and heap
   contracts); the charged executable bodies of `vox_union_find_worker.ml`,
   `vox_union_find.ml` and the forwarding operations in
   `vox_union_find_online.ml` and `vox_connectivity.ml`. Read their tick/charge
   placement and `spent` updates; forest invariants, potential lemmas and ghost
   representations are proof-internal. The concrete constant-overhead token
   implementation is `vox_big_credits.ml`.

`valid` and `added`/`found`/`joined` are abstract proof evidence, not definitions
of connectivity. Operations establish and preserve this evidence themselves.
Clients never manufacture it: the exported elimination laws give the complete
observable effects for an arbitrary element. Initial emptiness follows from
`create` and `empty_law`. Allocation adds a fresh singleton, find preserves all
membership and representative observations and returns the old representative,
and union changes exactly the two old classes to one of their representatives.
Other classes and membership are unchanged. `connected_def` means membership
of both handles and equality of representatives. Handles are abstract, and
queries require membership in the uniquely owned state. Insertions require
size strictly below `max_int`. Proof snapshots and traces erase.

For any finite prefix with `n` allocations, `f` top-level finds and `u` unions,
let `N=max(1,n)` and `a=inverse N`. Operation postconditions supply the account
trace; `fee_bounds` and `sequence` give

```
ticks <= 1 + 11*n + (4*a+12)*f + (12*a+36)*u.
```

An internal find is included in its union's fee, not counted again in `f`.
There is no upfront population bound; the machine-size limit still applies.
Credit issuance is confined to the caller, and state-owned savings cannot be
extracted. Balances are mathematical integers. The trace is ghost client proof
data, never an execution certificate or a final checker.

The exact charged model is one creation tick, three ticks per allocation,
`4*d+2` per find on a path with `d` parent edges, seven ticks per root link
(including self-links), and one union entry tick plus its two finds and link.
Worker entry covers bounded scalar work; owned reads/writes, node construction
and allocation requests are charged as described in the existing README.
Allocation implementation time, garbage collection, exceptions and stack
exhaustion are not covered. The algorithm's resource/cost theorem does not
prove that the trusted primitives have constant wall-clock latency.

`iter b k t x` is zero outside `b>=1`, `k>=0`, `0<=t,x<=b`; otherwise its visible
recurrence is capped at `b`. `inverse N` is the least `a>=1` reaching `N` from
`iter N a 1 1`. Checked cap coherence and minimality are preserved. Identifying
this capped recurrence with the conventional uncapped Ackermann hierarchy is
the external mathematical argument documented in README, not a new Vox theorem
about an independent uncapped definition. The resulting asymptotic claim uses
that identification and constant overhead for the concrete token operations.

## Reproduction

Run `python3 verification/review/check_cost_boundaries.py COMPILER_PREFIX`.
The script reads the installed compiler and writes only `_build/cost-boundary-check`
in this checkout. It freshly verifies all relevant modules, compiles independent
clients in directories containing only public interfaces, runs bytecode/native
clients, checks focused negative fixtures, and records emitted-operation erasure
checks. It does not install into or modify the compiler prefix.

The merge-sort client proves multiplicity at an arbitrary element and tests
ranked records with distinct payloads. The connectivity client proves initial
emptiness, connectivity and separation after growing/merging, representative
agreement, a paid-prefix bound and a constructor-counted trace bound. Rejections
exercise fees, state/token reuse, nonmembers, machine bounds, hidden state,
forged transitions, positive issuance, ghost escape, false claims and private
sorting proof helpers.

## Recorded result, 2026-09-25

Using the read-only installed `worktrees/time-credits/_install` compiler
(`5.4.0+ox`) with this baseline's library sources, all 28 relevant modules
verified in bytecode and native mode. Both public-only clients and the existing
online growth/credit regression programs compiled and ran in both modes.
All 37 negative cases rejected in both modes; the additional hidden sorting
proof lookup rejected. The 15 emitted-operation audits passed. Isolated clients
produce harmless missing-cmx optimization warnings because only public cmi
files are present while compiling; implementations are supplied at link time.

The tests rebuild library interfaces from source and do not reuse installed
library cmi files. No compiler build or installation was performed. The
follow-up preserves the local committed lineage: remote #194 has meanwhile
been restacked onto the typed-heap lineage, so applying this patch there requires
that lineage's compatible compiler and regression rerun.
