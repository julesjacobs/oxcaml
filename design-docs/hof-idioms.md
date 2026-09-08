# Higher-order specification idioms for Vox

The strongest current recommendation is to use a pointwise relation for map,
provide an invariant-based fold for general callbacks, and retain total model
functions for separate mathematical lemmas. A fold that exports ghost evidence
is a useful alternative when clients need to derive new properties after the
call. Its measured wrapper allocation supports keeping it optional.

This recommendation is scoped to the checked prototypes below. It does not
establish a universal winner: full frequency-map specifications remain blocked
by the collection proof surface, and full indexed array traversal has not been
compared across all candidates. The prototypes are experiments, not a decision
to publish every variant as a permanent library API.

## Requirements and terminology

A **callback contract** describes one invocation. Its precondition and relation
are total specifications; the callback can still raise or diverge. A
**traversal invariant** relates the processed input to an accumulator. A
**total model** computes a mathematical result independently of the callback.
**Ghost evidence** records values witnessing successful callback steps and is
unavailable to ordinary computation.

These distinguish five separate questions:

1. Can the contract express the desired result without selecting a more
   deterministic callback than the client requires?
2. Can the HOF establish each callback precondition at the point of invocation?
3. Can clients prove further properties using a separate lemma after the call?
4. Can the same specification handle captured immutable parameters and partial
   callbacks?
5. What proof, implementation, and runtime costs does the representation add?

Normal-return correctness and termination are separate. In particular, a
returned value marked `@ total` does not make evaluation of the callback total.
The partial combinators in this experiment do not acquire a termination
contract merely because one particular caller supplies a total callback.

## Candidate contracts

The following signatures are schematic: mode annotations are abbreviated.
The executable prototypes contain the complete annotations and checked bodies.

### Total model

```ocaml
map_model model
  (f : (x : a) -> {y : b | y === model x})
  (xs : a list)
  : {ys : b list | ys === model_map model xs}

fold_model model
  (f : (x : a) -> (acc : b) -> {out : b | out === model x acc})
  (xs : a list) (initial : b)
  : {out : b | out === model_fold model xs initial}
```

`model` and the model traversals are total. `f` need not be total. A separate
`model_invariant` lemma proves an invariant of `model_fold`; equality transports
that result to the returned value. The HOF does not need to receive the
invariant or its preservation proof.

This exports a reusable description of the entire computation. Its direct
form requires the concrete and model results to have the same type and usable
logical equality. An abstraction function can generalize it to differing
representations, but that is another contract to design, not something the
current equality signature already provides.

A partial decoder illustrates another distinction: equality with a total
Boolean decoder model alone need not establish that all input tags were
valid. The step relation can include both tag validity and the output meaning.
A model returning an option, connected through `Some output`, can express that
stronger fact, but requires a more general simulation contract than direct
result equality.

### Step relation

```ocaml
map r (f : (x : a) -> {y : b | r x y}) (xs : a list)
  : {ys : b list | map_rel r xs ys}
```

`map_rel` checks equal shapes and relates corresponding elements. Both endpoint
lists are available, so its definition needs no intermediate witnesses.
Length and sortedness are derived by separate lemmas, without changing map.

The analogous fold contract would return `fold_rel r xs initial out`.
Mathematically, its cons case existentially quantifies the recursive
accumulator. For an arbitrary relation over an arbitrary accumulator type,
that existential is not generally an executable Boolean search. The study
therefore does not add an axiom or pretend that this predicate is implemented.
It compares explicit evidence and supplied invariants instead.

### IH relation

```ocaml
fold_right_ih inv
  (f : (x : a) -> (tail : a list) @ ghost -> (acc : b) ->
       {u : unit | inv tail acc} @ ghost ->
       {out : b | inv (x :: tail) out})
  xs initial
  ({u : unit | inv [] initial} @ ghost)
  : {out : b | inv xs out}
```

The callback gets the induction hypothesis. This works without specifying a
separate step relation. It is particularly direct when the operation is
naturally verified under the traversal invariant.

The corresponding IH map gets the input and output tails as ghost arguments.
It computes the tail before invoking the callback. The relational map prototype
invokes callbacks from the head first. This evaluation-order difference is part
of the API: neither implementation can silently replace the other for partial
or effectful callbacks.

### Separate preservation proof

```text
f x acc returns out satisfying r x acc out
preserve proves inv tail acc ∧ r x acc out ⇒ inv (x :: tail) out
fold_right r inv f preserve xs initial base returns out satisfying inv xs out
```

The ordinary callback retains its reusable step contract. `preserve` is a
separate, erased proof function, but it is supplied to fold. Naming this proof
separately does not turn it into a theorem that can be applied to an otherwise
unspecified fold result after the call.

This form is useful when a callback already has a good modular contract.
Compared with the IH form, it introduces an explicit intermediate relation and
an adapter between that relation and the traversal invariant. Both can express
underdetermined steps such as returning any upper bound.

### Ghost evidence and a separate lemma

```text
fold_trace r f xs initial returns { value; trace @@ ghost }
  satisfying valid_trace r xs initial value trace

trace_invariant r inv preserve xs initial value trace base
  proves inv xs value
```

The trace contains the recursive accumulators. `valid_trace` checks every step
and the initial state. Its base case uses logical equality, so this prototype
is a ghost predicate rather than an ordinary runtime validator. A separately
checked structural induction consumes the trace to prove any compatible
invariant. The callback is not re-executed in the proof.

This preserves evidence that can support multiple later lemmas. Ghost fields
erase their payload, but the enclosing result record is an independent
representation choice. Erasure alone does not establish zero allocation
cost for the record wrappers.

## Evidence from existing systems

**F\*.** The current total-list properties library has separate
`fold_left_invar`, fold/map compatibility, and append lemmas whose callback
has a `Tot` computation type. Its tutorial distinguishes total list mapping
from mapping an effectful callback. This supports the total-model approach;
those pure fold lemmas do not by themselves describe an arbitrary diverging
callback. [F\* list properties](https://github.com/FStarLang/FStar/blob/master/ulib/FStar.List.Tot.Properties.fst),
[F\* total computations](https://fstar-lang.org/tutorial/book/part4/part4_computation_types_and_tot.html),
[older tutorial, higher-order lists](https://fstar-lang.org/tutorial/old/tutorial.html).

**Lean.** `Array.foldl_induction` accepts a motive indexed by the number of
processed elements and the accumulator. Its conclusion is a proposition about
the ordinary total fold result. Monadic folds have separate equations exposing
the sequence of monadic steps. The transferable pattern is a separate theorem
about an already-denoted computation; representing effects explicitly is what
makes the monadic result available to logic. [Array fold induction](https://lean-lang.org/doc/api/Init/Data/Array/Lemmas.html#Array.foldl_induction),
[monadic list folds](https://lean-lang.org/doc/api/Init/Data/List/Control.html).

**Abstract refinements and Synquid.** Abstract refinement types parameterize
contracts by uninterpreted predicates. Synquid's fold example explicitly gives
the callback a ghost tail and an accumulator satisfying a relation between
that tail and the accumulator; the result extends the relation to the cons
list. This is a direct precedent for the IH candidate. Predicate inference and
program synthesis are additional mechanisms, not requirements for Vox to
check an explicitly supplied relation. [Vazou, Rondon, Jhala, ESOP 2013](https://goto.ucsd.edu/~rjhala/papers/abstract_refinement_types.html),
[Polikarpova, Kuraj, Solar-Lezama, PLDI 2016, §2.3](https://cseweb.ucsd.edu/~npolikarpova/publications/pldi16.pdf).

**Iris and CFML.** Iris's fold case study specifies the callback with a nested
triple involving an invariant over a suffix and accumulator, then derives sum
and filter clients. CFML's hash-table work parameterizes a generic fold by an
invariant, permitted traversal prefixes, completion, and ownership assertions.
It gives distinct contracts for callbacks that can read the table and those
that cannot access it. This supports an invariant-based specification for
effectful traversal, while showing why mutable ownership needs more than a
relation between ordinary input/output values. [Iris foldr case study](https://iris-project.org/tutorial-pdfs/lecture6-foldr.pdf),
[Pottier, Verifying a Hash Table and Its Iterators, §§4.5–4.6](https://gallium.inria.fr/~fpottier/publis/fpottier-hashtable.pdf).

**Dafny.** Its reference distinguishes heap-dependent function arrows,
heap-independent partial arrows, and total arrows. Function values expose
preconditions and read sets. Here “partial” concerns a function's defined
input domain; it should not be identified with Vox's possibly diverging or
raising implementation callback. The relevant lesson is to keep domain,
heap dependence, and termination separate in the contract. [Dafny arrow types](https://dafny.org/dafny/DafnyRef/DafnyRef#sec-arrow-types).

**Why3.** WhyML distinguishes pure functions usable as logical symbols,
logical predicates, lemma functions, and partial program functions. Partial
program functions cannot be used in ghost code. This reinforces the need for
a total specification separate from a partial implementation, rather than
trying to call that implementation in a proof. [WhyML declarations](https://why3.org/doc/syntaxref.html#function-declarations).

**Verus.** Closures can have preconditions and postconditions; clients connect
those contracts through `call_requires` and `call_ensures`. The guide separately
discusses closure captures and limitations around mutable captures. The
transferable idea is a callback contract independent of its body, with capture
and ownership rules treated explicitly. It does not establish that a generic
input/output relation is sufficient for arbitrary mutable closures.
[Verus closures](https://verus-lang.github.io/verus/guide/exec_closures.html).

These are source-level comparisons, not cross-verifier benchmarks. Library
and guide pages were consulted on 2026-09-08; paper dates above are publication
dates, not search-engine crawl dates.

## Vox experiments

The comparison keeps the externally stated property fixed where candidates
are compared directly. A failure to express the property is recorded rather
than replacing it with a weaker property.

| Client | Checked property and comparison |
|---|---|
| Count | The same length contract via IH, separate preservation, total model plus a separate lemma, and ghost evidence plus a separate lemma; partial callbacks also raise on a selected input |
| Affine map | Exact total model with two captured Bigint parameters and an explicitly total implementation callback |
| Partial decoder | Pointwise decoding relation and equal lengths; invalid tags raise |
| Sortedness-preserving map | An independent generic lemma consumes `map_rel`, source sortedness, and a supplied monotonicity proof; map's signature is unchanged |
| Checked sum | On normal return the machine result equals the Bigint sum; overflow raises; the callback relation is connected to a fold invariant |
| Upper-bound fold | Arbitrary callbacks satisfying an upper-bound relation, through IH, separate preservation, and trace APIs; no exact callback model is assumed |
| Early termination | A total transition model describes stop/continue; a partial callback raises only if the offending element is visited |
| Map with accumulator | Exact model connects the final state and every produced output; the callback can raise |
| Preconditioned map | A total list predicate supplies each callback's nonzero-input proof; the callback may additionally raise |
| Frequency map | The direct whole-map model-equality contract is rejected by the current finite-map encoding; no full frequency-map client is claimed verified |

The frequency-map probe establishes a limitation of this particular contract,
not an impossibility theorem about all encodings of a frequency table. A useful
follow-up is to compare pointwise observations, an explicit extensional map
model, and ghost bundles of observation lemmas. A single chosen-key property
would not meet the original all-keys challenge.

The sorted-map theorem checks the relationship between two callback results.
Monotonicity for non-strictly ordered inputs includes equal inputs: the theorem
must account for duplicates, rather than assuming any arbitrary per-element
upper-bound relation preserves sortedness.

The checked-sum callback uses a Bigint range check before machine addition.
The prototype does not establish a performance result for a more optimized
machine-only overflow check. The division client reuses the existing
nonzero-divisor `%divint` contract pattern from the quicksort demo.

## Generalization probes

Tree mapping uses the same exact-model contract and structural recursion over
an explicitly inductive tree. An indexed iarray update uses a total model of
the changed element and exports equality with `Vox_iarray.updated`; the
unchanged elements remain described by the native iarray specification.
This is an indexed update probe, not a verified full `mapi` implementation.

`iterate_down` requires a total callback that returns a strictly smaller
nonnegative machine integer and proves a zero result using a numeric decreases
annotation. It establishes the need to express callback termination and
progress separately from ordinary normal-return postconditions.

For mutable ownership, the existing `Borrow_iarray.Owned_array.with_mut` and
`Slice.split_at` APIs already receive postcondition functions and callbacks
whose contracts refer to final slice models. The `iarray_borrow` clients use
this form to restore ownership and prove updates. This supports retaining a
separate ownership-aware contract family; it does not validate passing a
mutable slice through the immutable list combinators in this experiment.

A nested tuple output in the first `map_accum` client encountered a kind
compatibility rejection. The passing client uses scalar outputs while still
relating them to the threaded index. Rich accumulator/output kinds therefore
need additional coverage before calling the generic surface complete.

## Implementation and trust boundary

The prototype accepts function arguments in logical applications by
specializing the logical symbol for the supplied function. Captured scalar
arguments of partial applications become ordinary SMT arguments. Specialization
keys distinguish function position, domain/result sorts, and capture sorts.
This permits repeated partial applications with equal captures to participate
in ordinary congruence reasoning. It does not introduce a general SMT function
sort, function extensionality, quantified axioms, or a CHC solver.

Partial applications can remain function values while evaluating a predicate.
Their eventual applications are connected to the original function and captured
arguments. This is deliberately narrower than a complete higher-order logic;
it should not be advertised as unrestricted higher-order specification support.
The capture probes use named functions and scalar captures; primitive partial
applications and function-valued captures need additional coverage.

The ghost-record experiment also required the existing immutable-record
encoding to accept mixed-layout records. Their logical fields include the
ghost fields even though those fields occupy no runtime slot. Ordinary
structural equality on compound data is not thereby given a new SMT meaning.

The model traversals, map relation, fold trace checker, induction lemmas, and
HOF implementations are checked Vox code. The compiler changes belong to the
trusted verification implementation and need review separately from those
checked proofs. The study adds no existential axiom for `fold_rel`.

## Ergonomics and maintenance

Several costs are independent of the chosen semantics:

- A ghost lambda that constructs an iarray can lose its length/read equations
  when its argument is substituted. For example, the result of
  `ghost_ (fun x -> Iarray.length [: x; 20 :])` cannot currently be proved to
  equal two after application. This is an encoding limitation to fix in a
  follow-up, not a reason to avoid the specification idiom.
- Curried total functions need precise intermediate mode annotations.
- Dependent arguments generally need plain local variables; fields and compound
  expressions need local bindings before calls.
- Equivalent captured-predicate contracts sometimes need explicit eta adapters
  because their refinement syntax differs.
- Empty polymorphic data sometimes needs a type annotation to keep the logical
  instance aligned with its intended concrete or generic type.
- Total definition lemmas support tuple matches where tuple let-patterns in a
  reflected predicate are currently rejected.

The capture tests check that separately constructed equal captures can be
related, while different captures and different function-argument positions
are not silently identified. Other rejection tests cover a false callback
postcondition, a partial function used as a total model, a missing callback
precondition, and incorrect ghost evidence.

These tests are more informative than raw line counts alone. The total-model
count client can change its later invariant proof without changing fold's
contract. The upper-bound clients can change the implementation within its
relation without choosing a new exact model. The same count and upper-bound
properties are checked through multiple APIs. This is limited maintenance
coverage, not a longitudinal claim about a large application.

## Reproduction and measurements

Run the focused correctness tests with `./dev test vox/collection_functions.ml`
and `./dev test vox/hof_rejected.ml`. Candidate implementations are in
`testsuite/tests/vox/hof_candidates.ml`; the relational and IH combinators are
in `verification/library/vox_traversal.ml` and its explicit interface. The
challenge clients are in `collection_functions.ml`, `hof_challenges.ml`, and
`hof_array_clients.ml` in the same tests directory.

The measurement script is `verification/benchmarks/hof_idioms.py`. It uses the
compiler under the supplied installation prefix, recompiles the experimental
units in both backends, repeats selected compilations, runs the clients, and
measures allocation for the same count contract over 1,000 elements. It writes
raw JSON under `_build/hof-study/results.json`. Compilation times include
verification, normal compilation, and process startup; they are not isolated
SMT timings or a comparison between other verifiers.

The installed compiler was rebuilt with `make -s install`. On this local
arm64 Mac, the following are medians of three compile runs and allocated minor
words averaged over 100 executions of a 1,000-element count. The source counts
include each client's required predicate, callback, and proof definitions,
imports, and blank lines; shared traversal implementations are compiled
separately.

| Count API | Client lines | Bytecode compile ms | Native compile ms | Bytecode words | Native words |
|---|---:|---:|---:|---:|---:|
| IH | 39 | 28.6 | 65.9 | 8,003.24 | 8,003.24 |
| Separate preservation | 52 | 28.9 | 72.0 | 8,003.24 | 8,003.24 |
| Total model plus lemma | 78 | 33.7 | 79.0 | 8,003.24 | 8,003.24 |
| Ghost evidence plus lemma | 54 | 29.5 | 73.4 | 11,006.24 | 10,005.24 |

The total-model client reuses the relation-based callback through an adapter;
its source count includes that adapter and its proof dependencies. These are
counts of the checked implementations, not minimal possible implementations
of each design. The generated `client_*.ml` files make the comparison inspectable.

IH, separate preservation, and total model allocate the same amount in this
client. The trace variant allocates about 3,003 additional words in bytecode
and 2,002 in native code. This is consistent with surviving result wrappers;
the measurement does not imply the ghost trace list is allocated. The common
baseline includes Bigint arithmetic. It is not a general runtime benchmark of
all HOF clients.

Compilation takes tens of milliseconds for these small clients. Three samples
and differing helper sets do not establish a meaningful compiler-speed ranking
between the idioms. Representation cost and proof reuse provide stronger
reasons to choose here. The complete challenge module compiles in median
160 ms with `ocamlc` and 221 ms with `ocamlopt` in this run.

Final checks: all 72 tests in `./dev test vox/` passed, including both new test
harnesses. Z3-enabled `@verification/tests/runtest` passed. The measurement
script compiled and ran the challenge clients with both installed backends.
`make -s fmt` and `git diff --check` were also checked. These results describe the baseline before the follow-up compiler cleanups.


## Decision criteria

Use relational map as the main map contract: both endpoint collections are
available, its relation is directly definable, and separate lemmas can recover
length and ordering. Exact model graphs should be supported as a common
specialization rather than requiring a separate functor per model.

For fold, keep an invariant-based core. Use the IH form when the callback's
natural contract uses the recursive invariant. Use a separate preservation
proof when an independently specified callback already exposes a step
relation. The two forms are alternative placements of the same induction
obligation, not different termination guarantees.

Offer total model folds where the result has a usable mathematical model and
logical equality. Their separate lemmas provide particularly good reuse.
Retain the ghost-evidence candidate for clients that need multiple later
properties, when that reuse justifies the measured wrapper allocation and API complexity.

Do not add logical existentials solely to give arbitrary folds an endpoint-only
relation. First resolve finite-map observations, test full indexed traversal,
and improve the captured-predicate adapters. Those gaps currently limit the
clients more directly than the absence of an existential `fold_rel`.
