# Vox v1: capability contract and readiness review

Draft for agreement, reviewed against stack tip `764064cc5718` on 2026-09-07.
This is a source and demo review, not a fresh full-suite or CI result.

## Capability contract

Vox v1 should let an OCaml programmer verify functional algorithms over
immutable data, expose their guarantees through abstract module interfaces,
and use those guarantees in separately compiled clients. Total functions and
proofs must additionally terminate. Runtime validation may establish the
preconditions from which static verification proceeds.

There are two guarantees to distinguish:

- A result refinement describes what holds when an operation returns normally.
  The operation may be partial, including because it performs runtime checks.
- A total function additionally satisfies the language's totality requirements.
  Totality of a value does not imply that evaluating the expression producing
  that value was effect-free.

`assume_` checks a predicate at runtime. On success it returns the refined
value; on failure it raises `Assert_failure`. Checks remain enabled under
`-noassert`. It is part of the supported verification idiom, and is not a
trusted logical assumption. A function performing such a check is partial.
The predicate must be executable under the runtime-checking rules; it cannot
read erased ghost data. Checking a total Boolean validator and using a proved
law of that validator is a suitable way to establish a richer specification.

### Required capabilities

| Capability | v1 acceptance criterion |
| --- | --- |
| Scalar reasoning | Prove bounds, branch consequences, and arithmetic contracts with machine-integer wrapping semantics; support mathematical integer proofs with bigints. |
| Recursive data | Define supported inductive variants and verify traversals using structural induction. |
| Termination | Check structural descent and nonnegative decreasing numerical measures for recursive functions and lemmas. |
| Collections | Verify sequence, membership, lookup, and pointwise update properties over lists, sets, maps, and immutable arrays. State comparator requirements explicitly. |
| Abstract modules | Hide representations and invariants behind `.mli` files; separately compiled clients prove useful results solely from exported contracts. |
| Generic APIs | Support polymorphic immutable-data algorithms and proof interfaces with explicit mode requirements where necessary. |
| Algorithm refinement | Prove an implementation agrees with an independently stated mathematical specification. |
| Runtime validation | Turn executable checks on untrusted-by-the-type-system input into refined values, then combine those facts with static proofs. |
| Proof programming | Use total refined-unit lemmas, explicit definition lemmas, and erased `ghost_` computations without changing runtime results. |

The normative idiom is [Verification programming in Vox](verification-programming.md).
Users may supply lemmas and explicit unfolding; v1 does not promise automatic
induction or that every true formula in these domains is discharged by SMT.

### Acceptance workflows

Each workflow needs an executable example, checked contracts, a separately
compiled client where abstraction matters, and nearby rejected incorrect uses.

1. **Checked sorted array:** validate an incoming immutable array, construct an
   abstract sorted array, then search, insert, and remove. Establish order,
   exact contents, first/last occurrence, and absence. Demonstrate failed input
   validation and successful validation feeding client proofs.
2. **Persistent collections:** an abstract generic FIFO queue with a sequence
   model, and a balanced search tree with membership and balance invariants.
   Exercise standard set/map contracts and their comparator semantics too.
3. **Compiler transformation:** prove constant folding preserves a separately
   defined expression evaluator's result, including machine-integer behavior.
4. **Regex matching:** state membership independently of derivatives; prove
   derivative matching and total finite DFA construction sound and complete.
   The acceptance criterion is correctness and termination, not DFA minimality
   or efficient construction for large expressions.
5. **Hybrid verification:** perform runtime checks, use their facts in static
   proofs, erase correctness-only computations, and retain the checks in both
   bytecode and native code, including under `-noassert`.

### Scope and trust

V1 does not require proofs about mutable heap updates, aliasing invariants,
concurrency, general effects, or asymptotic resource bounds. Runtime clients may
allocate or perform effects; their contracts must express only supported facts.
Immutable containers holding mutable elements do not imply immutable element
contents. Proof APIs must state the access requirements they actually need.

The release must document which primitive contracts, external declarations,
collection models, and imported interfaces are trusted, and which library
implementations are themselves checked. A compiler-accepted signature alone is
not evidence that its implementation has been verified. The verifier and SMT
solver also belong to the trusted implementation. Resource exhaustion needs an
explicit convention in the meaning of totality.

## Readiness review

### Existing evidence

The [demo inventory](../testsuite/tests/vox/README.md) covers all capability
categories above. In particular:

- `checked_windows.ml` already combines `assume_` checks with static arithmetic
  proofs. `assume_runtime.ml` exercises runtime failures and `-noassert` across
  bytecode and native execution.
- `sorted_array.mli` exports abstract search and update contracts, with a
  separate client and rejected uses. Construction currently starts at `empty`;
  it does not yet demonstrate validating an arbitrary incoming array.
- The abstract queue, list set, and AVL set demonstrate model-based collection
  APIs. Sparse arrays and standard collections cover polymorphic operations.
- `expressions.ml` proves constant-folding correctness. `regex.ml` supplies
  independent membership derivations and soundness/completeness proofs for
  both matchers. Its DFA uses finite partial-derivative support and a powerset
  construction; finiteness of ACI-normalized derivatives is unnecessary for
  that construction.
- Ghost demos cover erased computations and stored proof data. Interface
  roundtrip tests cover refinements, dependent arguments, definitions, and
  runtime checks across compilation boundaries.

These are acceptance evidence, not a proof that every interaction in the
language is correct. In particular, `dependent.ml` only supplies a proposed
recursion-combinator signature; it does not verify its implementation.

### Work required before calling this v1

**1. Resolve the totality boundary.** The demo README explicitly records a
totality loophole through ordinary negative datatypes. The implementation's
`check_inductive_decl` restricts declarations marked `[@@inductive]`; that
restriction alone does not establish a totality guarantee for ordinary types.
This review has not independently reproduced the documented loophole. First
recover a compile-only regression and determine its current status. If it
remains, either fix totality checking or enforce a precisely defined fragment
at the proof boundary. A documented request to avoid such programs is
insufficient for the proposed totality contract. This is the primary potential
semantic blocker; it should be settled before adding more capabilities.

**2. Complete the checked-construction workflow.** Extend the existing sorted
array demo with a total executable sortedness validator, the laws connecting
its result to the array invariant, and a checked constructor. Include capacity
conditions required by the representation. Use `assume_` at the constructor
boundary, then prove search/update properties in the separate client. Failed
validation should exercise the runtime failure path. This is a focused
acceptance gap, not evidence that runtime validation is missing from Vox.

**3. Make the release claim reproducible.** Specify the supported target and
solver configuration, run the complete acceptance workflows and relevant
regressions at the release tip, and require solver-dependent tests to execute.
The current verifier requires a 63-bit integer target; the default Linux CI
job installs pinned Z3 4.16.0. Record verification times and set explicit CI
budgets from measurements. No new timing or CI claim is made by this review.
Finish the trust inventory described above as part of this release gate.

### Reasonable v1 limitations

The four remaining items in [problems.md](../problems.md) do not currently
justify expanding the language to ship these workflows:

- Observer laws may require `immutable_data` or explicit mode-crossing
  information. Do not claim that the same laws observe mutable contents.
- Use a nullary constructor such as `M.Refined.empty ()` where a polymorphic
  constant cannot supply the required access mode.
- Annotate retained earlier curried arguments `@ total` where required.
- Bind expressions to local variables and supply an expected type before
  introducing a refinement with `refine_`.

Keep these rules documented and represented in generic client tests. Revisit
them if the acceptance workflows require pervasive wrappers or duplicated
proofs; their mere existence is not a v1 blocker.

The solver already distinguishes counterexamples, unknown results, timeouts,
and failures, and reports unsupported predicates. Unsupported premises can be
omitted conservatively, with explanatory diagnostics on proof failure. The
compiler currently discards counterexample model details when reporting an
invalid result. Source-level counterexamples would improve usability, but are
not required before the workflows above can be useful.

Efficient reachable-state DFA construction, additional collection demos, and
broader automation can follow v1. The next milestone should resolve the
totality boundary, complete checked construction, and establish the release
gate as one coherent package.

## Mutable-borrow development

The earlier Vox roadmap included prophecy-based mutable borrows and in-place
quicksort. The [current borrow and slice proposal](borrows-and-slices.md)
recovers that direction with current refinements and ghost code. Its milestones
are stable model observations, scoped loans and resolution, disjoint reborrows,
and verified sequential quicksort. Effect-aware termination and parallel
sorting are separate follow-ups. This is a proposed extension of the scope
above; it does not silently make mutable-state verification a completed v1
capability.

## Proof inventory and higher-order contracts

This section records the 2026-09-08 proof-surface review on the separate
list/iarray prototype, updated with the [HOF idiom study](hof-idioms.md). “Checked” below means there is a checked implementation
or example on that branch. “Proposed” is a capability or API to develop, not a
claim about the release. The earlier readiness review remains dated as above.

### Distinctions the API must preserve

A **model function** is a total function usable in a specification. A
**callback contract** consists of a total precondition and total postcondition
relation; it constrains an executable callback on normal return. The callback
itself may be partial. A **fold invariant** relates the input already processed
to the current accumulator. It is total even when the fold step is partial.

These distinctions are independent of the container representation. Keep list
specifications structural and iarray specifications indexed. Also distinguish
totality of a returned value from totality of the call that produced it.
Annotating a callback's result `@ total` does not make the callback total.

### Inventory

| Proof pattern | Example guarantee | Current evidence / remaining work |
| --- | --- | --- |
| Total model map | `ys = map_model f xs`; preserve length and pointwise application; distribute over append | Checked `Vox_sequence.Map`; total model supplied by a named module. Fusion/composition laws remain to add when exercised. |
| Total model fold | Result equals a total mathematical fold; fold an append in two stages | Checked `Vox_sequence.Fold`, a right fold. Add left-fold equivalence and useful accumulator invariants. |
| Partial callback map | Every input satisfies `P`; every returned output satisfies `R(input, output)`; preserve length and order | Checked explicit-relation `Vox_traversal.map` and preconditioned `map_pre`, with callbacks that can raise. See the HOF idiom study before promoting an API. |
| Partial callback fold | A step preserves `I(processed_input, accumulator)`; the returned accumulator satisfies `I(all_input, result)` | Checked right-fold prototype with an erased suffix argument. Add a left fold using an erased processed prefix. |
| Partial callback with exact model | On normal return, callback output equals `f_model input`; map/fold agrees with the model | Use the graph relation `R(x,y) = (y = f_model x)`. Checked map/fold clients and separate model-invariant lemmas; do not mention the partial callback itself in logical equations. |
| Callback preconditions | Checked division, decoding, or lookup is called only on admissible inputs | Map prototype supplies a proof of `P(input)` at each call. Fold invariants must imply the next step's precondition; add a nontrivial precondition example. |
| Context-dependent contracts | Map depends on a bound, immutable environment, or configuration supplied at runtime | Checked captured relation parameters and total models through function specialization. Captured predicates sometimes require explicit adapters. |
| Filter and partition | Output is the selected subsequence; partition preserves all elements and establishes opposite predicates | Checked total-predicate filter laws and quicksort partition proofs. Partial predicates require a relation between the input and returned Boolean; a canonical selected subsequence needs a deterministic predicate model. |
| Indexed traversals | `mapi`, scans, and indexed folds preserve position-dependent facts | Iarray lookup, range, update, and slice laws are checked. Generic indexed traversal contracts and demos remain to build. |
| Finite set transformation | Every output comes from an input; every input has a related output; cardinality may decrease when outputs coincide | Existing finite-set surface needs a relational-map client and membership/cardinality lemmas. Do not reuse the list length-preservation contract. |
| Finite map transformation | Mapping values preserves the key domain and establishes a key-dependent value relation | Existing finite-map surface needs a `map_values` client with `R(key, old_value, new_value)`. State collision behavior separately for key transformation. |
| Set/map folds | A fold preserves an invariant despite the container's traversal order | Proposed. Fix comparator/traversal semantics, or require algebraic laws establishing order independence. |
| Iarray construction | `init`/map preserve length and establish a relation at every valid index | Proposed checked executable construction API. Separate allocation/capacity failure from a total indexed mathematical model. |
| Recursive container traversal | Mapping a tree preserves shape, or rebuilding preserves search and balance invariants | Existing balanced-tree demo provides structural evidence. Generic callback traversal and refined child preconditions remain to exercise. |
| Collection equality | Same length and pointwise reads imply equality; set/map equality and bag equality support abstract specs | Derived list extensionality; existing explicit iarray extensionality; checked multiset/count proofs. Inventory set/map lemma gaps separately before adding axioms. |
| Sortedness and order | Binary search returns boundaries; insertion/removal preserve exact contents and order | Checked sorted-array client now uses shared iarray ordering. Sorted slices, neighbor-bounded updates, and pivot recombination are library lemmas. |
| Generic comparator proofs | Sorting respects the supplied order, including duplicates | Integer order is checked. Generic comparators need explicit order laws and an equality convention; totality of comparison alone is insufficient. |
| Abstract representation invariants | A queue exposes a list model; clients reason solely from `.mli` contracts | Checked queue client; shared list append replaces the queue's separate append API. Maintain separately compiled positive and rejected clients. |
| Recursive algorithm refinement | Constant folding preserves evaluation; derivative/DFA matching agrees with regex membership | Existing checked demos. Keep independent specifications where they are the point of the example. |
| Termination and total combinators | A structurally recursive traversal is total when its callback is total and admissible | Checked total model functors. A relation contract alone does not establish callback termination. A general total relational map/fold entry point remains to validate. |
| Runtime validation | `assume_` establishes executable preconditions before verified calls | Checked, including the relational prototypes' input premises. Executable checks cannot depend on erased ghost data. |
| Mutable-state callbacks | A step transforms a current model into a final model while preserving unaffected regions | Borrow/iarray prototype demonstrates such reasoning for sorting. Generic effectful traversal contracts still need explicit state, ownership, and frame requirements. |
| Parallel callbacks | Disjoint operations compose and preserve a whole-container postcondition | Existing parallel-quicksort prototype. Do not infer general parallel map/fold support from that example. |
| Existential intermediate states | A fold result is reachable through a sequence of relation-respecting steps | Checked ghost accumulator traces and a separate invariant lemma. An arbitrary existential relation is not automatically an executable total Boolean definition. |
| Exceptional outcomes | Specify which exception is raised and how state changes on failure | Current callback contracts cover normal return; the prototypes check propagation at runtime. General exceptional postconditions remain outside the demonstrated proof surface. |

The recommended v1 acceptance scope is total model map/fold, normal-return
relation-based map/fold, callback preconditions, explicit immutable context,
and useful indexed/list/set/map observations. Include both fold directions.
Keep the existing independently specified algorithm and abstract-module
workflows. First-class predicate syntax is an ergonomic decision, not a reason
to postpone demonstrating those proof patterns with the current encoding.
General exceptional postconditions, invariant synthesis, resource bounds, and
generic stateful traversal contracts are separate follow-ups. This is a
recommendation for agreeing the inventory, not a claim that every row is done.

### Relation-based map

Ignoring mode syntax, the desired contract is:

```text
P : context -> input -> bool                     total
R : context -> input -> output -> bool           total
f : (x : input) -> proof(P c x) -> {y | R c x y}   possibly partial

map c f : (xs : input list) -> proof(all (P c) xs)
       -> {ys : output list | map_relation c xs ys}
```

The list relation has a structural definition:

```text
map_relation c [] [] = true
map_relation c (x :: xs) (y :: ys) =
  R c x y && map_relation c xs ys
map_relation c _ _ = false                        mismatched shapes
```

This fixes length, position, and the relation at each position. It permits
several valid outputs for one input. It neither invents a model value for a
partial call nor asserts that the callback returns. The callback may have
stronger guarantees than `R`; an explicit adapter can expose the contract the
combinator requires. The current checker may require such adapters when
logically equivalent dependent function types are not syntactically identical.

The current experiment supplies predicates and callbacks as explicit function
arguments, including captured immutable parameters. It proves the relation,
length, and sortedness consequences and checks propagated callback exceptions.
See the [HOF idiom study](hof-idioms.md) for contracts, encoding changes, and
remaining gaps. Add pointwise lookup and relation weakening/composition before
promoting this into the public library.

### Relation-based fold

A right fold processes a suffix first. Supply a total invariant `I(xs,a)` and:

```text
base : I([], initial)
step : (x : input) -> (suffix : input list) ghost -> (a : accumulator)
    -> proof(I(suffix, a)) -> {b | I(x :: suffix, b)}
fold_right step xs initial base : {result | I(xs, result)}
```

The checked prototype passes the suffix only as ghost data. The executable
step receives the element and accumulator. A left fold should use the same
idea with a processed prefix; its order and invariant are different and should
be stated explicitly.

For a total model fold, choose `I(xs,a) = (a = fold_model xs initial)`.
For a less precise result, choose an invariant such as a length bound, sum
bound, membership property, or representation invariant. With a separate step
relation `S(x,a,b)`, prove that `I(suffix,a) && S(x,a,b)` implies
`I(x :: suffix,b)` and use that proof in an adapter around the callback.
This keeps the operational callback contract separate from a particular fold
invariant.

Specifying only the endpoint relation of an arbitrary fold can require hidden
intermediate accumulators. Do not silently replace the invariant with an
existential Boolean predicate that Vox cannot execute or prove total. Use a
supplied invariant, a total model, or explicit ghost derivation witnesses.

### What “CHC-style” means here

The useful connection is relational specification plus inductive closure:
base facts establish the invariant, and each callback contract establishes a
step. For map, `R(x,y)` and the tail relation establish the whole-list relation.
For fold, the invariant and step relation establish the next invariant.

The current implementation checks user-supplied relations and induction proofs.
It does not synthesize an unknown relation or introduce a CHC solver. Keep
invariant synthesis as a separate possible feature; these contracts should
remain useful without it.

### API decisions and next acceptance examples

1. Retain total model map/fold and introduce relation-based combinators as a
   separate surface. Model equality is an important specialization of a relation.
2. Validate explicit immutable context parameters and a total relational
   entry point before choosing final signatures. Preserve the distinction
   between total callbacks and possibly partial callbacks.
3. Add a checked division/decoder map, a left fold with an accumulator
   precondition, and a callback specified against an independent total model.
   Each needs a client proving something about its result and rejected
   incorrect callback contracts, not just successful library compilation.
4. Review the experimental function specialization used by the explicit
   predicate APIs. It keeps SMT arguments first-order and lifts scalar captures;
   it does not implement unrestricted higher-order logic. The HOF idiom study
   records checked clients and current limitations.
5. Handle stateful callbacks, exceptional postconditions, generic comparators,
   and existential fold derivations as explicit follow-ups. Do not enlarge the
   trusted theory to disguise a missing checked library argument.
