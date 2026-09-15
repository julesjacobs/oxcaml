# Vox findings from the mutable unifier

The unifier now builds on #137, #139 and #140. Rejected examples distinguish
language boundaries from compiler defects; rejection alone is not a defect.

## Improvements applied

- #137 adds `Pref.equal`: physical handle equality with the contract
  `{b : bool | b = (p === q)}`. This is a trusted primitive contract backed by
  `%eq`; structural comparison of cell contents would be the wrong operation.
- #139 permits direct, fully applied recursive calls inside named and returned
  closures. The existing structural and numerical descent obligations still
  apply. Intermediate model callbacks now call recursive proofs directly.
- #140 preserves recursive totality at the interactive top level and supports
  dependent result annotations on explicitly typed, unlabelled variable
  parameters. Full function signatures remain useful for reviewing contracts.
- #140 adds checked `refine_` adaptation of function results. Equal saved heap
  observations can transport a model callback through pointwise obligations.
  Wrappers preserve effects at each curried application; ghost wrappers erase.
- #140 accepts stable literals and immutable projections in dependent calls.
  Mutable reads and effectful arguments still require an explicit `let`.
- #140 supports direct `@@ ghost` fields in unboxed records. The unifier now
  uses these fields for resolution, search and execution witnesses. Real reads,
  real pattern inspection and block indices into ghost fields remain forbidden.
  Ownership checks prevent erased fields from recovering real tokens or
  duplicating unique ownership.

The earlier negative-datatype warning was stale: total elimination already
requires the checked datatype guarantee. Compile-only regressions cover that
boundary. This is an audit of concrete cases, not a soundness proof of Vox.

## Remaining language boundaries

Function adaptation currently requires a stable local function and unlabelled
parameters. Labelled or optional arguments need an explicit wrapper. Scalar
refinements still use `let refine_` to open evidence and `refine_` to repackage it.
The callback's `@ total` annotation applies to the function value; placing it
after an arrow constrains the returned value. #140 adds a targeted diagnostic.

Do not infer heap membership merely from `Heap.at h p === Some value` in the
current encoding. Operation certificates retain `Heap.mem h p` explicitly.
Connecting these observations is a possible specification/automation improvement;
it must not become an assumed allocation invariant.

## Representation and proof boundary

Native code removes void witness slots. Bytecode can retain void placeholders.
Lambda inspection checks that executable operations contain no proof calls or
resolution, search, derivation, or edit-witness allocations. This is an erasure
check, not a native cost proof.

The unifier proves partial correctness and exact model transformation. The finite
readback layer additionally constructs and preserves finite unfoldings and
excludes cycles. The MGU layer adds canonical substitutions and explicit
factorization functions. No invariant constructor or correctness theorem is
assumed.

## Findings from finite readback

A proof over a tuple match did not expose structural descent to the totality
checker; nested matches do. This is a checking limitation, not evidence of a
soundness defect. `finite_unique` uses nested matches.

Directly constructing all unfolding evidence after eight allocations failed to
recover an early cell's contents through the entire update chain. The emitted
SMT obligations did not propagate that lookup through every intermediate heap.
The checked `allocation_frame` and `allocation_finite_at` lemmas establish the
invariant incrementally. Heap-observation saturation is worth investigating;
this development does not change the compiler's automation or assume lookup
facts to bypass it.

Local total closures do not automatically expose their bodies to later proofs.
A local `[@def]` valuation supplies an explicit defining lemma for the model
witness. Equal saved-heap names are transported using checked `refine_` function
adaptation from #140.

## Build tooling

Running Merlin's bootstrap Dune workspace can remove the main workspace's
compiler-library installation links. `dev test` then refuses to run before
rebuilding those links and requests full initialization. This is an incremental
workflow issue: the installed compiler can still be used by `ocamltest` directly.
The test helper could distinguish missing workspace links from stale runtime or
standard-library artifacts and rebuild the required links automatically.

## MGU factorization

Physical handles can serve directly as symbolic variable identities in `ty`.
This removes the finite-readback label function and avoids an additional
numbering table and injectivity invariant. The same existing model theorems
then support MGU factorization without polymorphizing or duplicating them.

Totality on a curried callback must constrain the callback itself. An annotation
at the end of an unparenthesized arrow chain can instead constrain its final
result, causing higher-order mode mismatches. The MGU continuation and factor
callback use parenthesized function types. This remains an annotation and
diagnostic usability issue; no new Vox soundness defect was identified.

## STLC inference

The runtime result combines a unique ownership token with shared graph handles
and an immutable equation tree. The shared result fields need explicit
`@@ aliased` modalities so returning the unique token does not demand unique
ownership of the shared handles or equations.

A larger branching generator again exposed incomplete heap-observation
propagation: direct allocation did not establish membership of the new handle
at the later proof site. The checked `allocation_mem` lemma supplies exactly
that consequence of `H.put`. This reinforces the earlier heap-saturation
observation; no membership fact is assumed and no compiler change is made here.

A proof block that introduces local names must return a refinement over names
visible to the caller. Returning plain unit discards needed evidence, while
returning a refinement mentioning a block-local name produces an escaping-scope
diagnostic. The generator returns explicit scope refinements across these
blocks. Non-variable dependent arguments such as `S Z` must still be named;
the existing literal/projection elaboration does not cover constructor calls.

## Recursive lambdas

The STLC extension checked direct recursive calls through three allocation-model
continuations and a body-model continuation using the existing totality support.
No new compiler capability was required. The current heap-fact propagation issue
also affected preservation of an argument's membership across the result and
self-arrow allocations. A checked `allocation_keeps_mem` lemma exposes that
consequence of `H.put`; the implementation does not assume membership.

## Scheme copying

A node containing an optional memo payload caused heap-observation premises to
be omitted as unsupported in the template-uniqueness proof. Replacing the memo's
`option` with the explicit `Empty_memo | Memo` datatype allowed the proofs to
check. Factoring the descriptor into a polymorphic shape alone did not fix it.
The suspected interaction is between the outer `Heap.at` option and a nested
option in the node; this needs a minimized encoding regression before assigning
an exact cause. Vox should explain which premise it cannot encode and why.

Matching a pair of inductive template witnesses lost the structural descent
needed for a recursive total proof call. Nested matches exposed that descent.
Supporting equivalent tuple matches would simplify these proofs.

Heap updates again required a checked `put_frame` lemma to expose membership
and unchanged observations at a selected handle. Adapting dependent callbacks
between a local heap name and its equal history interpretation required explicit
`refine_` annotations. The expression-refinement extension removes the separate local-binding step:
`refine_ evidence.proof` adapts a projected callback directly. Other expressions
are bound once before ordinary refinement checking or function adaptation.
Dependent heap-alias transport still uses the checked expected function contract;
this extension does not infer a missing invariant or add an assumption.

The copier uses fresh node handles as session identities. This avoids an
unproved machine-integer epoch bound, at the cost of one Boolean cell per call.
The public clean-copy wrapper clears written in-node memos through a temporary
list of touched source handles before returning. The raw copier remains available
for proof composition. Neither choice requires a compiler change.

## Levels and generalization

The same richer-node model supports the original unifier's exact success and
rejection proofs through an explicit descriptor observation. Metadata framing
then handles levels and memos separately. Larger allocation fixtures made heap
membership/observation propagation harder to expose; small fixtures and selected
heap-update lemmas remained reliable. A minimized compiler regression is still
needed before classifying the larger-fixture failure as an encoding defect.

The environment exclusion proof uses explicit paths and a total pointwise level
order function. Pool completeness is another total function; no quantifier was
added to the SMT fragment. Exact generalization composes existing CPS model
construction through total local callbacks. Equal heap aliases required checked
callback adaptation, and one higher-order scheme alias required an explicit
forwarding callback rather than direct `refine_` adaptation. Diagnostics and
alias adaptation remain useful ergonomics targets.

## Registered allocation and forest transport

An unannotated `let desc1 = Var` could trigger an uncaught sort mismatch after
using it in a refinement and then pattern matching. The minimized regression
requires no heap operations: the scrutinee and pattern carry different
instantiations of the polymorphic datatype. The verifier now reinstantiates a
known nullary constructor at the pattern's type after checking nominal type
identity. Other sort mismatches lose outgoing pattern facts conservatively.
The positive fixture proves the constructor branch; the rejection fixture
rejects its negation. The pooled allocation fixture no longer needs the explicit
`desc` annotation.

Forest and pool-coverage transport work with explicit total callbacks. Heap
aliases still require repeated dependent callback annotations at composition
sites. Reducing that adaptation overhead would simplify clients without
changing their invariants or adding SMT quantifiers.
