# Predicate typing consolidation report

Status: complete. The consolidated implementation is on
`jujacobs/vox/predicate-typing` as the required RED/GREEN pair. All VERIFIED
review findings are fixed in the surviving implementation except the explicitly
documented bootstrap-placeholder diagnostic limitation; every JUDGEMENT finding
is adopted, partially adopted, or rebutted below.

## Consolidation-review fix round

The follow-up consolidation reviews are
`reviews/consolidation-by-codex.md` and
`reviews/consolidation-by-fable.md`. Their findings have these final
dispositions:

| Review finding | Final disposition |
|---|---|
| Codex 1, HIGH VERIFIED: application arguments keyed by location | **Fixed.** Correspondence consumes same-label, same-anchor matches in source occurrence order. A unique anchor is the fallback when label representations differ, and no match is reusable. One compiler-libs regression erases every location to `Location.none`, reorders `~y` before `~x`, and follows them with two unlabelled arguments. It fails both anchor-only cross-wiring and non-consuming same-label lookup. |
| Codex 2, HIGH VERIFIED: locals in stored node types | **Fixed.** Every stored `rexp_type` is scanned under the lexical mirror-binder set during mirror construction, promoting nested-refinement mentions to `Rexp_var` before persistence. Copying preallocates all local binder renames before mapping stored types, so a full outer arrow annotation also sees nested function parameters. Raw-CMI/functor checks cover the stored-only shape, and a direct `Subst` regression requires a new binder stamp, its occurrence in the copied stored predicate, and absence of the old stamp. |
| Codex 3, MED VERIFIED: object and unboxed-tuple frame payloads | **Fixed structurally.** Frame views copy every non-variable descriptor while preserving the special arrow-commutation and refinement-cell contracts. Source-spelling tests cover the reported object and unboxed-tuple payloads, the special polymorphic-variant row-copy path, and the generic first-class-package path. |
| Codex 4, MED VERIFIED: no fixed-point or whole-batch rollback regression | **Fixed.** A synthetic two-job batch takes eight predicate-typer calls, covering bootstrap, an unequal strict pass, stabilization, and replay. A second regression fails call four after an earlier job pins a shared variable, then proves exact restoration of the target predicate cells and type graph and reuse as `string`. |
| Codex 5, LOW JUDGEMENT: defensive queue failures are ICEs | **Adopted.** Fuel exhaustion and warning-replay divergence now raise located `Typetexp` errors. Both paths are forced by a test hook, and each test checks that the exception materializes as a located diagnostic, followed by rollback and reuse of the identical payload node. |
| Codex 6, LOW JUDGEMENT: three rejection cases are GREEN-only | **Adopted.** `drx`, `dvx`, and `ap7b` now enter in RED with unchecked-acceptance expectations and flip to their GREEN rejections. The committed-type/rolled-back-mode fixture follows the same RED-to-GREEN form. |
| Codex 7, LOW VERIFIED: stale documentation comments | **Fixed.** The test inventory names `string{ String.length _ }`; the roundtrip claim is limited to printed written structure and identities; the type-formers header reflects later stack pieces; and the gate text records its annotation behavior. |
| Fable 1, MED JUDGEMENT: defensive queue failures are ICEs | **Adopted** by the located-error change and rollback regressions described for Codex 5. |
| Fable 2, MED-LOW JUDGEMENT: committed types versus rolled-back modes | **Adopted as a boundary fixture; no defect reproduced.** Predicate typing commits a weak variable's arrow shape, and its otherwise unconstrained arrow modes default conservatively, so a later local-argument demand rejects. This documents the intended rollback/defaulting boundary but does not independently isolate `Mode.with_rollback`. |
| Fable 3, LOW VERIFIED: fabricated `int{ _ }` spelling in batch errors | **Documented limitation.** At bootstrap failure there is not yet an authoritative typed mirror to print. Preserving the written predicate would require retaining unresolved source syntax in `Types` or restoring a second parsetree mirror walker. That representation/duplication cost is disproportionate to this bootstrap-only cosmetic diagnostic; successful mirrors are unaffected. |
| Fable 4, LOW JUDGEMENT: queued-batch cost | **Adopted.** The design records one bootstrap typing, one or more stabilization typings, and one warning replay per predicate, with a defensive quadratic worst case per binder-carrying domain. |
| Fable 5, LOW JUDGEMENT: eager-versus-queued rule drift | **Adopted.** The design now states the implemented depth rule: every predicate nested while a binder-carrying domain is translated queues, whether or not it mentions the binder. |
| Fable 6, LOW VERIFIED: fixture and message drift | **Fixed.** The inventory uses `string{ String.length _ }`, and sibling unsupported-form diagnostics consistently say “in a refinement predicate.” |

## Base-lane choice

Lane A (`impl-fable`) is the base. It most closely follows the specified
ownership boundary (Typetexp gates and queues; Typecore builds the authoritative
typed mirror), preserves annotated predicate-`let` constraints and
signature-local value pairing, has no generic dev-loop tooling, has the broader
RED matrix, and has the smaller semantic GREEN diff. Its reviews identified
fewer architectural changes needed to reach the design than Lane B's reviews.

Lane A is not retained unchanged. The consolidation ports Lane B's complete
failure transaction, mode rollback, contextual variable nodes, stored-annotation
closure, and direct compiler-libs inspection tests. It then repairs defects
shared by or found in both lanes: atomic whole-batch queue barriers, semantic
versus persistence traversal, copied refinement frame nodes, durable
non-semantic refinement identity, ghost-context reentry, exact CMI inspection,
and artifact-magic advancement.

This choice and the mechanism-level decisions are also recorded in
`design-docs/predicate-typing.md` under “Decisions taken during consolidation.”

## Review-finding dispositions

IDs use `CC` = codex lane reviewed by Codex, `CF` = codex lane reviewed by
Fable, `FC` = fable lane reviewed by Codex, and `FF` = fable lane reviewed by
Fable.

| ID | Classification | Final disposition |
|---|---|---|
| CC-1 | HIGH VERIFIED: queued provisional identity | Fixed. A payload-only bootstrap collects every job before one install barrier; strict whole-batch passes stabilize before an authoritative warning replay. Both source orders are fixtured. |
| CC-2 | MED VERIFIED: ghost read rejected | Fixed. Reentry enters a ghost context inside a reversible mode frame; the mode-isolation fixture accepts. |
| CC-3 | MED JUDGEMENT: direct Typecore-failure rollback test | Adopted. Compiler-libs tests reuse the identical fresh payload after both early Typecore failure and late mirror-construction failure. |
| CC-4 | MED JUDGEMENT: weak imported-annotation test | Fixed after follow-up strengthening. The original raw-CMI fixture checked exact node presence, closure, binder identity, selected field/constructor/application identities, dependent-hole structure, and functor substitutions, but missed a binder reachable only through a stored `Rexp_fun` annotation. The fix round adds that producer, direct/functor-import checks, and an explicit old-stamp-versus-fresh-stamp substitution assertion. |
| CC-5 | LOW VERIFIED: wrong non-bool application fixture | Fixed in RED with `string{ String.length _ }`; the expected result is a boolean-type error. |
| CC-6 | LOW JUDGEMENT: generic dev-loop tooling and unused helper | Adopted. The tooling change and unused helper are absent from the branch. |
| CF-1 | HIGH VERIFIED: metadata occur-check cycle | Fixed. Semantic occurrence traversal excludes derived node annotations; multi-predicate and own-domain/codomain composition cases are fixtured. |
| CF-2 | MED VERIFIED: rollback degrades diagnostics | Fixed. Exceptions are converted to materialized `Location.error` values before type-graph backtracking. |
| CF-3 | MED VERIFIED mechanism / JUDGEMENT severity: frame relinks written spelling | Fixed completely after follow-up. The initial descriptor whitelist fixed the refined-string case but left other payload spines mutable. Frame views now structurally copy every non-variable descriptor while retaining ambient-variable, arrow-commutation, and refinement-cell sharing where required. |
| CF-4 | MED JUDGEMENT: missing CMI/CMT magic bump | Adopted. The common artifact version advances from 583 to 584, producing `Caml1999I584` and `Caml1999T584`. |
| CF-5 | LOW VERIFIED: annotated predicate `let` flip | Fixed by retaining Lane A's correspondence treatment; the discriminating fixture remains. |
| CF-6 | LOW JUDGEMENT: unrelated full-suite churn | Adopted. Lane A's inherited promotion commit is omitted; reproduced unrelated reference changes remain unpromoted and are itemized under validation. |
| CF-7 | LOW JUDGEMENT: overbuild and fragility | Partially adopted. No Ident watermark was added; written/stored walks share one parameterized fold; constraint-extra popping is one helper. A wholesale TyVarEnv rewrite is rebutted because the reentrant scope is needed to retain ambient named variables while rejecting newly introduced names, now covered by a discriminating pinning fixture. |
| FC-1 | HIGH VERIFIED: failed predicate leaks unification | Fixed. Every failure through mirror completion restores the Btype snapshot after freezing diagnostics; early and late shared-payload regressions pass. |
| FC-2 | HIGH VERIFIED: stored annotations force invariance | Fixed. Variance follows payloads and written constraint types only; the contravariant declaration is fixtured. |
| FC-3 | MED JUDGEMENT: missing magic bump | Adopted through the same shared 584 version change. |
| FC-4 | MED JUDGEMENT: cross-module test cannot see node annotations | Adopted and strengthened in the fix round. Raw CMI graph inspection observes stored annotations and substitutions directly rather than inferring them from printing, including the stored-only `Rexp_fun` binder shape missed by the first fixture; a direct copy additionally proves binder freshening. |
| FC-5 | MED VERIFIED: fixture gaps | Fixed. The suite contains both real queue orders, true `%revapply`, a nested-head consumer, corrected written-type substitution, recursive-signature and functor field-owner cases; misleading `ap7` was removed while the discriminating omitted-label cases remain. |
| FF-1 | VERIFIED DEFECT: dependent-arrow-hole self-inclusion | Fixed. Ordinary copies use fresh refinement cells, frame views use distinct refinement nodes, and toplevel, direct MLI, and functor-copy forms pass. |
| FF-2 | VERIFIED DEFECT: variance/injectivity | Fixed by the written-only semantic traversal described for FC-2. |
| FF-3 | VERIFIED cosmetic: refined payload relinking | Fixed rather than accepted as cosmetic; the source spelling fixture observes it. |
| FF-4 | JUDGEMENT: principal warning is harness merge behavior | Adopted. No semantic change was made; principal-only warning 18 is retained in the promoted combined expectation. |
| FF-5 | VERIFIED CONFORMANCE inventory | Adopted only for mechanisms independently retained and revalidated. Its original rollback/copy claims are superseded by the counterexamples and fixes above. |
| FF-6 | JUDGEMENT: expectations are discriminating | Partially rebutted and strengthened. The other reviews found weak cases, which were repaired across RED and GREEN. A final line-by-line audit found and fixed the stale roundtrip golden and ambient-TyVar fixture; all remaining expectations are coherent. |
| FF-7 | JUDGEMENT: inherited churn is credible | Not relied upon. The consolidation independently ran and triaged the full suite; inherited reference churn is not promoted here. |
| FF-8 | SIMPLICITY | Adopted where useful: the binder-order documentation is corrected, the fold and constraint-pop duplications are consolidated, and unrelated refactors are omitted. The gate/hole-rewrite/mirror walks remain separate because they run in different phases. |

## Mechanism map

| Mechanism | Implementation anchors |
|---|---|
| Syntactic gate, refinement creation, immediate/queued dispatch | `typing/typetexp.ml:1138`, `typing/typetexp.ml:1836` |
| Atomic queue collection, bootstrap barrier, fixed-point passes, authoritative warning replay, all-failure restore | `typing/typetexp.ml:1032`, `typing/typetexp.ml:1069` |
| Persistent non-semantic identity and frame-view sharing contract | `typing/types.mli:298`, `typing/ctype.ml:343`, `typing/ctype.ml:388` |
| Ordinary copy/substitution of mirrors with fresh cells and retained identity | `typing/btype.ml:632`, `typing/subst.ml:782` |
| Semantic versus persistence traversal | `typing/btype.ml:389`, `typing/vox_rexp.ml:17`, `typing/typedecl_variance.ml:127` |
| Contextual `Rexp_hole`/`Rexp_var`, correspondence mirror, nested-local promotion and freshening | `typing/typecore.ml:13003`, `typing/typecore.ml:13021`, `typing/vox_rexp.ml:70` |
| Stored-node annotation closure | `typing/typecore.ml:13418` |
| Reentry transaction: warnings/CMT/delayed checks/allocations, TyVarEnv, modes, ghost and locks, frozen-error rollback | `typing/typecore.ml:13449`, `typing/mode.ml:5751` |
| CMI/CMT shared magic version | `build-aux/ocaml_version.m4:100` |
| Early/late rollback, consuming correspondence, fixed-point/whole-batch rollback, defensive located errors, and direct imported-CMI contract | `testsuite/tests/vox/predicate_typing_internals.ml:17`, `testsuite/tests/vox/predicate_typing_internals.ml:81`, `testsuite/tests/vox/predicate_typing_internals.ml:189`, `testsuite/tests/vox/predicate_typing_internals.ml:269`, `testsuite/tests/vox/predicate_typing_internals.ml:373` |
| Dependent-hole, variance, structural spelling, both queue orders, and mode interaction | `testsuite/tests/vox/predicate_typing.ml:176`, `testsuite/tests/vox/predicate_typing.ml:225`, `testsuite/tests/vox/predicate_typing.ml:232`, `testsuite/tests/vox/predicate_typing.ml:240`, `testsuite/tests/vox/predicate_typing.ml:390`, `testsuite/tests/vox/predicate_typing.ml:832` |
| Cross-module producer shapes and roundtrip consumer | `testsuite/tests/vox/predicate_typing_defs.mli:1`, `testsuite/tests/vox/roundtrip/test.ml:1` |

## Validation

### Consolidation-review fix round

- `make dev NOWATCH=1` passed with the required toolchain path and
  `TMPDIR=$HOME/tmp`.
- `make dev-test DIR=vox NOWATCH=1` considered 14 tests: 12 passed, the two
  expected bytecode-compiler actions were skipped by the fast harness, and 0
  failed. No corrected expectation was produced.
- Every changed expectation was reread against its source fixture, including
  the object/unboxed/variant/package spelling, eight-call fixed point,
  whole-batch rollback, defensive located-error rollback, stored-only binder
  import, and conservative mode-defaulting results.

### Original consolidation validation

- Setup and final build: `autoconf27`, configure with
  `--prefix="$PWD/_install"`, and final `make dev NOWATCH=1` all passed.
- Artifact configuration: generated values are `Caml1999I584` and
  `Caml1999T584`; full-suite magic checks passed.
- Final `make install_for_test` passed after the Types changes.
- Final `make dev-test DIR=vox NOWATCH=1`: 12 passed, 2 expected fast-harness
  bytecode skips, 0 failed. This includes the compiler-libs rollback and raw-CMI
  inspection fixture.
- The skipped roundtrip bytecode path was promoted with `make dev-promote` and
  then rerun after the final installation with the installed harness: 1 passed,
  0 skipped, 0 failed.
- Every new or changed predicate expectation was read line by line. The final
  audit corrected the ambient named-variable pinning case, removed a redundant
  source-level rollback overclaim, and regenerated the complete cross-module
  `#show` output, including its principal-only warning.

### Full suite

`make dev-test-all NOWATCH=1` considered 2,670 tests: 2,436 passed, 208
skipped, and 26 failed. The failures were completely triaged:

- 1 branch-local failure: the roundtrip golden omitted six newly exported
  declarations and warning 18. It was promoted and its installed-harness rerun
  is green.
- 11 deterministic inherited reference changes, left out of this branch:
  `atomic-locs/record_fields.ml`, `flambda2/array_element_kind_meet.ml`,
  `formatting/test_locations.ml`,
  `layout_poly/cross_module_static/use_cross_module_static.ml`,
  `parsetree/test_ppx.ml`, `templates/basic/test.ml`,
  `tool-ocamlc-stop-after/stop_after_typing_impl.ml`,
  `typedtree/module_presence.ml`,
  `typing-layouts-or-null/non_float_array.ml`,
  `typing-modes/yielding_lambda.ml`, and `typing-zero-alloc/cmi_test.ml`.
  Their diffs are only inherited mode-axis printing, source-line, or uniform
  identifier-stamp shifts.
- 14 sandbox/environment failures: seven socket/bind tests failed with `EPERM`
  (`lib-threads/{pr4466,pr5325,sockets}.ml`,
  `lib-unix/common/{channel_of,cloexec}.ml`, and
  `lib-unix/unix-socket/{recvfrom_linux,recvfrom_unix}.ml`); native CFI timed
  out under restricted GDB; six `tool-debugger` tests failed because debugger
  binding is denied.
- 0 failures were classified as predicate-typing or compiler regressions after
  the branch-local golden was corrected.

The end-to-end suite was not rerun after that golden-only correction; the
corrected bytecode test, final compiler build, final install, and complete Vox
directory were rerun individually.

## Open concerns, ranked

1. The queue stabilizer uses `number of jobs + 1` defensive fuel. This prevents
   nontermination and rolls the entire batch back on exhaustion, but it is not a
   proof that every pathological convergent batch settles within the bound.
   Historical mirror batches are live mutable type graphs and cannot serve as
   sound cycle keys without a substantially purer/frozen transition design.
2. A bootstrap-time batch diagnostic can print the internal `int{ _ }`
   placeholder described in the fix-round table instead of the written
   predicate. Fixing that cosmetic spelling would require retaining source
   syntax in the persisted representation or duplicating the mirror walk.
3. Fourteen environment-sensitive full-suite tests need an unrestricted host
   for an all-green infrastructure result; all failed specifically at denied
   socket/debugger operations after successful compilation.
4. Eleven inherited golden changes remain intentionally unpromoted. They are
   independent of this piece but keep the aggregate suite red until the owning
   stack changes normalize those references.

No open functional defect is known in the consolidated predicate-typing
implementation.
