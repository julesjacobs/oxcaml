# Collection demo review boundary

Compiler baseline: `d143961f173da258a7c4220a8f140c81871a2c60` (#193).
Source dependency: `78b8b9e2ac`, the isolated #207 library migration
cherry-picked onto that baseline, preserving typed Pref/Ghost_pref tokens.
The six scopes below use checked public interfaces. The implementation modules,
representations, rotation/height invariants and auxiliary proofs are private.
No client includes their `.cmi` files. No execution returns or accumulates a
certificate or invokes a final correctness checker.

## Code-only catalogue inventory

`collections-specs.json` is the authoritative declaration inventory for the
six specification pages. Each page references an ordered list of exact code
fragments, with source file, enclosing signature, line span, source hash and
fragment hash. Regenerate it with `python3 verification/review/collections_specs.py`.
Every fragment comes from a checked `.mli`; no proof bodies or intermediate
invariants are included. Group fragments by their source module and enclosing
signature when displaying them. The fragment collection is a source view,
not a synthetic compilation unit. Its explicit limitation metadata is separate
from the displayed code. The English notes below are not specification pages.

## Ordered semantic review surface

Paths are repository-relative. Each list is ordered, transitive, and identifies
the declarations relevant to that claim; other declarations in a shared
interface need not be reviewed. In particular, an import of a shared sequence
library does not make all its auxiliary lemmas part of a sorting specification.
The implementation files are checked against these interfaces, not assumed.

All six scopes use the language's ordinary booleans, algebraic data, structural
logical equality (`===`), and signed machine integers. Integer arithmetic wraps;
explicit bounds below prevent wrap where an index/count argument requires it.
`stdlib/stdlib.mli` supplies the integer comparison/arithmetic primitives.
`stdlib/ghost.mli` documents ghost erasure. These are the first two files in
every list below. Resource exhaustion is outside the totality model: `total`
does not promise successful allocation on a finite machine. No scope claims
exception-safe reclamation, asymptotic complexity, or exact allocation counts.

**Sequential quicksort** (append to the two common files):

1. `stdlib/bigint.mli`: signed unbounded integers, `of_int`, arithmetic/comparison.
2. `stdlib/iarray.mli`: immutable arrays, `length`, bounded `Refined.get`.
3. `verification/library/vox_sequence.mli`: `t`, `length_def`, `append_def`,
   `at_def`, `set_def`, `take_def`, `drop_def`, `sub_def`, `swap_def`,
   `from_iarray_unfold`, `of_iarray_def`, `iarray_at_get`, `of_iarray_length`,
   `of_iarray_at`, `at_outside`, and `extensional`.
4. `verification/library/vox_int_sequence.mli`: `element_def`, `accepts_def`,
   `all_def`, `sorted_def`, `permutation_def`, `count_def`,
   `permutation_count`, and `count_extensional`. The last two directions
   completely characterize permutation by equality of every multiplicity;
   equality of an opaque `bag` alone is not the specification.
5. `verification/library/borrow.mli`: `Slice.current/final`, `length/get/set`,
   `swap`, `split_at/split3/with_range`, `finish`, and `Owned_array`.
   These state how sequence observations correspond to owned storage,
   disjoint subranges, and recombination. `with_range` gives the exact unchanged
   prefix/suffix equation. Runtime borrowing primitives and their compiler
   models are a trusted boundary; this does not verify their C implementation.
6. `testsuite/tests/vox/quicksort.mli`: `sort` and `sort_array`.

The domain is all owned integer arrays/slices admitted by Borrow. The result is
sorted and preserves every integer's multiplicity. Both sequential operations
are total in the model. `quicksort_frame_client.ml` derives exact framing for
sorting a selected subrange using these public operations.

**Parallel quicksort**: the same ordered files, additionally reading
`Borrow.Slice.parallel` and `Quicksort.parallel_sort/parallel_sort_array`.
Callbacks consume disjoint unique loans, are portable/once, and establish their
own postconditions on normal return. Parallel quicksort supplies those
callbacks internally; callers supply no callback correctness assumptions.
The primitive combines their postconditions on normal return. Optional domain
and cutoff integers affect scheduling, not the semantic domain. No parallel
termination, liveness, speedup, or exceptional-result claim is exported.
`Borrow.await_both` joins the worker before propagating a right-side exception;
`borrow_parallel.ml` tests spawning, sequential fallback, and that behavior.
This operational test is not a proved exception-recovery theorem.

**AVL integer sets**:

1. `stdlib/bigint.mli`.
2. `testsuite/tests/vox/int_set_intf.mli`: `Operations`, then `Extensional`.
3. `testsuite/tests/vox/avl_sets.mli`.

`lookup_empty/add/union` give complete membership equations. `equal_lookup`
and `extensional` characterize `equal` by pointwise membership. They imply
semantic equality, not structural equality of trees. `Canonical` is not part
of the AVL interface. `size_zero` characterizes emptiness only: the exported
interface does not prove a general cardinality formula or a running-time bound.
Rotation, ordering, balance and cached-height invariants remain in
`avl_sets.ml`. Import-time literal-tree diagnostic checks were removed; public
client tests still exercise the four rotation patterns and larger trees.

**Binary search and persistent sorted updates**:

1. `stdlib/bigint.mli`.
2. `verification/library/vox_sequence.mli`: `t`, `length_def`, `at_def`,
   `at_outside`, and `extensional`.
3. `testsuite/tests/vox/sorted_array.mli` (all declarations).

`contents_length/contents_at` connect the abstract array to the sequence model.
`ordered` gives pairwise ordering. `at_outside`, `occurs_equation`,
`occurs_between_equation`, `range_equation`, `edit_suffix_equation`, and
`edited_equation` now completely define every semantic predicate in the API.
`at` is zero outside the array; `occurs_between` follows its stated recurrence
even for out-of-bounds intervals, so a query for zero can observe that default.
Public searches use bounded intervals. Lower/upper equal-range endpoints,
first/last occurrence, insertion position, lengths, removed position, and every
shifted/unchanged index are explicit. There is no arbitrary-array validator:
clients start with `empty` and preserve the invariant through updates.
Lengths satisfy `0 <= n` and `0 < n + 1`; insertion additionally requires
`0 < n + 2`. Removal requires an existing index. Allocating updates do not
export totality. Interval descent proves search termination, not a logarithmic
comparison bound. `sorted_array_proofs.ml` is not a semantic dependency.

**Functional queue**:

1. `verification/library/vox_sequence.mli`: `t`, `append`, `append_def`.
2. `testsuite/tests/vox/functional_queue.mli` (all declarations).

Empty/enqueue/dequeue exactly characterize the FIFO sequence. Dequeue requires
a nonempty sequence. Values have `immutable_data` kind. The two-list
representation, normalization and reverse lemmas are private. There is no
amortized-cost theorem. `contents` is an explicitly callable runtime observer;
ordinary queue operations do not reconstruct it for proof checking.

**Sparse array overlays**:

1. `stdlib/iarray.mli`: immutable arrays, `length`, bounded `Refined.get`.
2. `verification/library/vox_iarray.mli`: `get`, `at`, `at_get`, `at_outside`.
   The two observation laws completely define optional array reads.
3. `testsuite/tests/vox/sparse_overlay.mli` (all declarations).

`empty_base/set_base/clear_base` fix the immutable base. `length_equation`,
`get_lookup`, `lookup_outside`, `empty_lookup`, `set_lookup`, and `clear_lookup`
give exact observations for every integer index. Out-of-bounds lookup is None,
including after an out-of-bounds write. Clearing an index restores its base
value; the default is the corresponding base element, not a fixed zero.
The Laws functor quantifies over arbitrary immutable values. Generic executable
operations retain total stored values and mutable payload access; no claim freezes or verifies
subsequent payload mutation. Array length is fixed; bounded `get` requires a
valid index. No client supplies representation invariants or proof objects.

The new implementation uses a private integer association map. Set removes old
bindings for that index before adding one, so repeated writes do not accumulate
history. Lookup/update/clear are linear in the number of overrides; the
complexity statement is an implementation description, not a proved cost bound.
This choice permits numeric-index laws without additional trusted axioms.
The historical `sparse_iarrays.ml` fixture remains a separate balanced-map
example: its `Map.MakeTotal` SMT model uses abstract comparison classes, and its
commutation premise explicitly distinguishes those classes. This new proof
does not verify that historical implementation or claim its balanced-map cost.

## Reproduction and evidence

Run `python3 verification/review/collections.py COMPILER_PREFIX` with a stable
installed compiler from this baseline, configured for multidomain execution
and poll insertion. It writes only `_build/collections-review` in this checkout;
it does not install, mutate the supplied prefix, or invoke Make/Dune.

The harness builds all actual implementations in bytecode/native. Quicksort,
AVL, sorted arrays and queue also pass principal mode. Sparse overlays use
ordinary mode: this compiler's principal mode rejects passing immutable
polymorphic values to the generic writable list operations in `find_remove`.
This is a type-checker limitation, not an assumed verification condition;
all sparse contracts, implementations and clients undergo normal verification.
The principal sparse compile failure is recorded in the delivery notes. Clients are compiled from a separate directory with
only public interfaces, then linked against the verified implementations.
They cover all six scopes, including arbitrary-query/count laws; finite runtime
oracles additionally cover duplicates, extrema, sorted/reverse inputs,
rotations, persistent edit effects, FIFO values, fallback, mutable access,
subrange framing, spawning and exceptional worker joins.

Negative clients must reject empty dequeue/removal, invalid sparse bounds,
false multiplicity/update claims, deriving structural AVL equality, and access
to private representations or proof modules. Rejections check the diagnostic
reason, not only a nonzero exit status.

Emitted bytecode/native Lambda is saved. The harness extracts actual operation
bodies and rejects calls to proof/model machinery in quicksort, queue and sparse
execution. Ghost snapshots and callbacks lower to inert placeholders, not
runtime sequences, proof closures or accumulated certificates. Sorted-array
and AVL execution paths are also inspected at closeout. Unused proof-function
definitions may remain in a compilation unit; that is distinct from calls on
ordinary execution paths. Explicit semantic observers remain executable.

`report.json` lists each successful client run, rejection and erasure check;
per-command logs retain the compiler prefix, commands, generated code and output.

## Delivery results (2026-09-25)

- 30 isolated public-client executions passed, 21 intended rejections passed,
  and 138 emitted-operation checks passed. Four additional runs match the
  original AVL test's updated reference output exactly.
- Bytecode and native compilation used the stable installed compiler at
  `../older-concurrency-boundary-20260925/_install`, built from `d143961f17`,
  with multidomain and poll insertion enabled. No compiler source changed;
  `verification/library/build.sh` is unchanged.
  The shared-library migration is a separate dependency commit.
- `make -s fmt` and `git diff --check` passed. This worktree was configured
  with its own `_install` prefix for formatting; no local install was built.
- Baseline quicksort's `refine_ lower` VC at line 73 was independently replayed
  and timed out at the default 5-second limit. Its VC/log is retained under
  `_build/collections-boundary/baseline/quicksort.log`. A checked comparison
  helper now isolates the unchanged parity tie-break; the final implementation
  passes at the default limit in both modes/backends.
- The excluded principal sparse failure is reproducible with `-principal` at
  `find_remove`'s call to `remove`: “This value is immutable but is expected to
  be read_write.” No solver obligation is bypassed in the supported mode.

The boundary-owned collection files also use implicit refinement introduction
and ordinary `let`: 536 obsolete keywords were removed from implementation,
interface and client sources, plus 14 from generated negative clients. Expected
tuple/component type annotations remain where inference needs them. The two
quicksort callback-signature binders now match the migrated Borrow interface.
The full client/rejection/erasure matrix passed after this cleanup.

The integrated shared-library dependency also passes
`check_implicit_library.py` on both backends with the same read-only compiler.
The complete collection matrix was rerun after integration. All 134 displayed
specification fragments match current source hashes and contain no obsolete
refinement keywords.
