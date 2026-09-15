# Vox findings from the mutable unifier

Tested on the Pref stack at `edc50aeab356`, with the unifier branch's changes.
These findings distinguish limitations from defects; a rejected proof is not by
itself evidence of a compiler bug.

## Handle identity: missing primitive contract

Neither generic `=` nor `==` lets the verifier infer logical equality of two
`Pref.t` handles. `vox_encoding.ml` only translates these comparisons for its
supported numeric/Boolean payload classes. This blocks representative identity
and the occurs check's base case. Use physical equality for handles: structural
comparison of the contents would be the wrong algorithm.

Add `Pref.equal`, implemented by `%eq`, with the contract
`{b : bool | b = (p === q)}`. This is a small addition to the trusted Pref primitive
interface, not a lemma proved from generic equality. Test aliases, independent
allocations holding identical payloads, and preservation of identity after writes.
A future compiler improvement could recognize physical equality for Pref types
without requiring the wrapper, while retaining conservative behavior for other
types.

## Total recursive proofs and closures

The first implementation encountered "the recursive function occurs in a
delayed body" when constructing intermediate model callbacks. This restriction
was syntactic: Vox already checked equivalent inline callbacks, and both descent
checkers already traversed ordinary function bodies.

[PR #139](https://github.com/julesjacobs/oxcaml/pull/139) permits direct recursive
calls inside named and returned closures while retaining every structural or
numerical descent obligation. The unifier now uses recursive model callbacks
directly. This removes the separate semantic edit traversal; the erased edit
witness remains only to describe the exact physical writes.

`closure_termination.ml` covers accepted and rejected recursive closures.
`unifier_vox_limits.ml` retains the original proof callback as a positive
regression. The same pointwise recursive definition at the interactive top level
is still rejected as partial. `pat_modes` uses legacy modes for that context;
fixing the discrepancy requires a separate mode-policy change. Module-scoped
proofs remain the workaround.

## Unboxed records and ghost fields

An unboxed record with `@@ ghost` on a field is rejected with "Unrecognized
modality ghost". Boxed records accept it, but then a result with one runtime
field can retain a wrapper allocation after proof erasure. Use a field of type
`witness Ghost.t` inside the unboxed record instead: its void representation
removes the field and the unboxed result avoids the wrapper. The unifier uses
this accepted form. The limits test records both forms; accepting the direct
syntax or giving a targeted diagnostic would improve the language.

The `-dlambda` output for the final implementation contains no calls into the
proof/specification modules and no constructors for resolution, search,
derivation, or edit witnesses. The remaining block constructions are the actual
`Link` payload, the unreachable assertion's exception payload, and the module's
function exports. This checks erasure at Lambda; it is not a native cost proof.

## Dependent arguments and refinement evidence

Dependent calls require plain local variables: literals and projections such as
`r.value` must first be named. A refined argument must be explicitly opened with
`let refine_`, then repackaged for a different refinement. Local callback result
dependencies require explicit function signatures. These are current language
rules, not identified soundness defects, but they cause substantial annotation
noise in this example.

Some callbacks need totality annotations on the function, not its returned
value; misplaced annotations produce superficially similar printed types.
Examples should show the distinction consistently.

## Heap membership and contents

Do not assume a standalone `Heap.at h p = Some value` automatically supplies a
membership fact in the current encoding. Operation certificates explicitly
retain `Heap.mem h p` for safe access. This is a specification/automation issue to
investigate, rather than an excuse to assume an allocation invariant.

## Recursive result annotations

The initial `weight_positive` lemma used a result annotation after its arguments.
Recursive calls did not expose that dependent result refinement. Writing the
whole dependent function signature before `= fun ...` resolved this. The existing
proof files use that explicit style. A smaller dedicated reproducer is still
needed before classifying this as a compiler defect.

## Scope of this report

No soundness defect was identified. The only added primitive is `Pref.equal`,
with a concrete `%eq` implementation. No invariant or correctness theorem was
added as an assumed external.
