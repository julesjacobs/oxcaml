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
