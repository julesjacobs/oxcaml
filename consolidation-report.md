# Predicate typing consolidation report

Status: complete through predicate-typing round 4 and its review amendments.
The consolidated implementation is on `jujacobs/vox/predicate-typing` as the
required RED/GREEN and RED2/GREEN2 pairs. All VERIFIED review findings are
fixed in the surviving implementation except the explicitly documented
bootstrap-placeholder diagnostic limitation; every JUDGEMENT finding is
adopted, partially adopted, or rebutted below.

## Round 4 (GREEN2)

GREEN2 completes the rulings pinned by RED2 and the design at `f5f7ad12f2`.
The round-4 review fixes amend those two commits in place: new semantic flips
are pinned in RED2 before their GREEN2 implementation, preserving the
RED2/GREEN2 spine.

### What landed where

| Mechanism | Implementation and coverage |
|---|---|
| Total Typecore judgment over Logical spec entities | Predicate reentry enters the ordinary Total closure frame and additionally requires every applied callee to be Total. Hole and dependent-binder entries retain their declared modes, defaulting to legacy Partial, with Logicality forced to Logical. The reentrant `TyVarEnv`, transient CMT/delayed-state framing, frozen-diagnostic rollback, removed refinement cutoff, and ordinary `Btype` mode trail remain unchanged. |
| Predicate-scoped comparison admission | `typing/typecore.ml:723-738,9585-9785` recognizes the six comparison primitives only during predicate reentry, gives each instance one shared immediate operand type, Logical argument modes, and a Total value mode. The global allowlist remains unchanged, so strings, `/`, and `mod` reject. `typing/btype.ml:2387-2392` and `typing/jkind.mli:559-563` expose the fresh immediate inference jkind. |
| Persistent total mirror translation | The mirror preserves `%apply`/`%revapply`, represents optional/defaulted/call-position/omitted application completion, format expansion, genuine layout wrappers, immutable instance variables, and GADT/existential constructor patterns. The Typecore judgment rejects arity-changing constructor wildcards and ambiguous omitted-label PPX applications before construction; defensive correspondence fallbacks for other PPX-shaped mismatches are located errors. |
| Mirror representation and graph operations | `typing/types.mli:339-392` defines application completion and `Rexp_format`. `typing/vox_rexp.ml:17-68,71-244,279-377,381-568` extends stored-type traversal, copying/substitution support, equality, source printing, search, and local promotion. `Subst` reaches the forms through `Vox_rexp.map`. |
| Mode diagnostics for instance variables | `typing/typecore.ml:7337-7365`, `typing/mode_hint.mli:54-58`, and `typing/mode.ml:5181-5187` make a mutable instance-variable read check its implicit self through the normal locks while retaining immutable instance-variable mirroring. |
| Regression coverage | Source fixtures cover direct, aliased, indirect, and curried callee totality; declared binder/payload modes; the diverging-function counterexample; every application-completion descriptor including a dedicated omitted-position flip; a genuine `Texp_apply_layout`; located nullary and multi-arity wildcard rejection; and located explicit-polymorphic-annotation rejection. Compiler-libs and roundtrip fixtures retain exact raw-CMI, equality, copying, substitution, printing, and import checks. |

### Deviations and decisions

- Inferred polymorphic predicate `let` remains supported. An explicit
  polymorphic binding annotation has no faithful expression-constraint mirror
  node and is rejected by the syntactic gate with a located error.
- Typecore pattern typing rejects a wildcard constructor argument when the
  selected constructor's arity is not one, because elaboration would erase or
  replicate its source node. The design-authorized existential-persistence
  check separately rejects only when a stored annotation retains an arm-local
  existential identity. Ordinary GADT patterns, unary wildcard existentials,
  and unrelated value binders are persisted.
- Application typing rejects omitted-label applications when colliding source
  anchors make correspondence ambiguous. Application and pattern
  correspondence fallbacks are also located errors, so malformed PPX-shaped
  source metadata cannot turn a mismatch into a compiler abort.
- The earlier mode-isolation helper and refinement lock cutoff are removed,
  including the now-unused helper API. Failed type and mode constraints share
  the `Btype` transaction; successful constraints commit.
- The branch-wide unreleased artifact magic remains 584. Round-4 mirror
  variants are part of the same RED/GREEN stack and do not consume another
  version.

### Round-4 delta-review dispositions

The final round-4 reviews are `reviews/round4-by-codex.md` and
`reviews/round4-by-fable.md`.

| Review finding | Final disposition |
|---|---|
| Codex HIGH VERIFIED: Partial hole/binder calls evade Total and declared modes are lost | **Fixed.** Every predicate application requires a Total callee, including aliases, indirect callback parameters, overapplication of a Total primitive that returns a Partial function, and each consumed curried stage. Payload and dependent-binder modes survive reentry, default to legacy Partial, and retain every declared axis except the required Logical override. Direct/operator Total-binder controls and the diverging-function rejection pin both sides. |
| Codex LOW VERIFIED: `ap13` misses `Texp_apply_layout` | **Fixed.** The primitive external is replaced by an ordinary layout-polymorphic value reached through a static module parameter. Its nonempty layout arguments force the transparent `Texp_apply_layout` correspondence arm. |
| Codex JUDGEMENT: omitted-position completion lacks a RED2 flip | **Adopted.** A dedicated partial-position application rejects in RED2 and is represented in GREEN2; the existing raw-CMI table and imported roundtrip continue to assert `Rarg_omitted_position` exactly. |
| Fable HIGH VERIFIED: multi-arity constructor wildcard reaches a fatal correspondence path | **Fixed by judgment rejection.** Once the constructor is resolved, Typecore rejects wildcard arguments whose arity-changing elaboration would erase or replicate the source node. Unary constructor wildcards remain represented. |
| Fable HIGH VERIFIED: explicit polymorphic binding annotation reaches a fatal correspondence path | **Fixed by judgment rejection.** Inferred polymorphic `let` remains accepted, while an explicit polymorphic binding annotation is rejected by the syntactic gate with a located error before Typecore reentry. |
| Fable MED JUDGEMENT: PPX-shaped correspondence fallbacks can abort | **Adopted.** Typecore rejects the demonstrated ambiguous all-ghost omitted-label application during the judgment, and the regression matches that exact diagnostic. Every residual application, expression, case, and pattern correspondence fallback is a located error rather than a fatal assertion. Parser-produced admitted input still has total translation. |
| Fable MED VERIFIED: stale compiler-libs in the review worktree | **Environmental; resolved by required validation.** No source fix is attributed to the branch. The authoritative validation runs `make install_for_test NOWATCH=1` before the Vox directory run, refreshing the installed compiler-libs and superseding the stale review tree. |
| Fable LOW VERIFIED: weakened or GREEN-only fixture pins | **Fixed.** RED2 now carries the malformed-primitive and omitted-position baselines; the shadowing fixture uses its local binder; the original Partial-call and occurrence source shapes remain alongside Total controls; and the layout-wrapper comment and source now describe a genuine wrapper. |
| Fable LOW JUDGEMENT: comparison primitives need a canonical type-shape guard | **Rebutted.** Comparison recognition relies on trusted compiler primitive identity and arity, as other primitive typing does. Valid declarations already have the canonical contract; forged externals are outside it. Unlike `%apply`/`%revapply`, this path does not structurally reconstruct a callback arrow, so a separate guard would add malformed-external policy without protecting a valid program or a fatal destructuring path. |

### Round-4 validation

- `make dev NOWATCH=1` passed after the required
  `make dev-refresh-stdlib NOWATCH=1` refresh for the `Types` change.
- `make install_for_test NOWATCH=1` passed and refreshed the compiler-libs and
  complete test installation used by the subsequent runs.
- `make dev-test DIR=vox NOWATCH=1`: 13 passed, 1 expected bytecode skip, 0
  failed. No corrected expectation remained. Every changed inline expectation
  and the complete roundtrip output was reread line by line. This final run
  used the refreshed compiler-libs tree; stale artifacts observed in a review
  worktree are not part of the result.
- `make dev-test-all NOWATCH=1`, with
  `TMPDIR=/usr/local/home/jujacobs/tmp`, considered 2,670 tests: 2,451 passed,
  208 skipped, and exactly the 11 pinned inherited reference tests failed.
  Every Vox test, including the installed bytecode and cross-CMI paths, passed.
  The authoritative full run was outside the restricted sandbox so Unix socket
  tests were not spuriously denied.
- The 11 inherited failures remain unpromoted:
  `atomic-locs/record_fields.ml`, `flambda2/array_element_kind_meet.ml`,
  `formatting/test_locations.ml`,
  `layout_poly/cross_module_static/use_cross_module_static.ml`,
  `parsetree/test_ppx.ml`, `templates/basic/test.ml`,
  `tool-ocamlc-stop-after/stop_after_typing_impl.ml`,
  `typedtree/module_presence.ml`,
  `typing-layouts-or-null/non_float_array.ml`,
  `typing-modes/yielding_lambda.ml`, and `typing-zero-alloc/cmi_test.ml`.
  Their fresh triage found only inherited identifier-stamp, source-location,
  and mode-axis printing deltas; none touches predicate typing.

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
| Codex 6, LOW JUDGEMENT: three rejection cases are GREEN-only | **Adopted for the first GREEN.** Round 4 supersedes the application restriction: `ap7b` is now accepted and its omitted required label is represented explicitly. |
| Codex 7, LOW VERIFIED: stale documentation comments | **Fixed.** The result-type fixture now uses a Total `total_length` helper so the boolean mismatch is isolated from mode checking; the roundtrip claim is limited to printed written structure and identities; the type-formers header reflects later stack pieces; and the gate text records its annotation behavior. |
| Fable 1, MED JUDGEMENT: defensive queue failures are ICEs | **Adopted** by the located-error change and rollback regressions described for Codex 5. |
| Fable 2, MED-LOW JUDGEMENT: committed types versus rolled-back modes | **Superseded by round 4.** Predicate typing now uses the ordinary `Btype` transaction: successful type and mode constraints commit, while either kind rolls back on failure. |
| Fable 3, LOW VERIFIED: fabricated `int{ _ }` spelling in batch errors | **Documented limitation.** At bootstrap failure there is not yet an authoritative typed mirror to print. Preserving the written predicate would require retaining unresolved source syntax in `Types` or restoring a second parsetree mirror walker. That representation/duplication cost is disproportionate to this bootstrap-only cosmetic diagnostic; successful mirrors are unaffected. |
| Fable 4, LOW JUDGEMENT: queued-batch cost | **Adopted.** The design records one bootstrap typing, one or more stabilization typings, and one warning replay per predicate, with a defensive quadratic worst case per binder-carrying domain. |
| Fable 5, LOW JUDGEMENT: eager-versus-queued rule drift | **Adopted.** The design now states the implemented depth rule: every predicate nested while a binder-carrying domain is translated queues, whether or not it mentions the binder. |
| Fable 6, LOW VERIFIED: fixture and message drift | **Fixed.** The inventory uses the Total `total_length` result-type fixture, and sibling unsupported-form diagnostics consistently say “in a refinement predicate.” |

## Base-lane choice

Lane A (`impl-fable`) is the base. It most closely follows the specified
ownership boundary (Typetexp gates and queues; Typecore builds the authoritative
typed mirror), preserves annotated predicate-`let` constraints and
signature-local value pairing, has no generic dev-loop tooling, has the broader
RED matrix, and has the smaller semantic GREEN diff. Its reviews identified
fewer architectural changes needed to reach the design than Lane B's reviews.

Lane A is not retained unchanged. The first consolidation ports Lane B's
complete failure transaction, contextual variable nodes, stored-annotation
closure, and direct compiler-libs inspection tests. Round 4 replaces its
success-isolating mode rollback with the ordinary `Btype` transaction. It also
repairs defects shared by or found in both lanes: atomic whole-batch queue
barriers, semantic versus persistence traversal, copied refinement frame nodes,
durable non-semantic refinement identity, ghost-context reentry, exact CMI
inspection, and artifact-magic advancement.

This choice and the mechanism-level decisions are also recorded in
`design-docs/predicate-typing.md` under “Decisions taken during consolidation.”

## Review-finding dispositions

IDs use `CC` = codex lane reviewed by Codex, `CF` = codex lane reviewed by
Fable, `FC` = fable lane reviewed by Codex, and `FF` = fable lane reviewed by
Fable.

| ID | Classification | Final disposition |
|---|---|---|
| CC-1 | HIGH VERIFIED: queued provisional identity | Fixed. A payload-only bootstrap collects every job before one install barrier; strict whole-batch passes stabilize before an authoritative warning replay. Both source orders are fixtured. |
| CC-2 | MED VERIFIED: ghost read rejected | Fixed. Reentry enters a ghost context; round 4 otherwise uses the ordinary Total/Logical locks and snapshot transaction. |
| CC-3 | MED JUDGEMENT: direct Typecore-failure rollback test | Adopted. Compiler-libs tests reuse the identical fresh payload after early and late judgment failures; round-4 mirror correspondence is total for parser-produced admitted programs. |
| CC-4 | MED JUDGEMENT: weak imported-annotation test | Fixed after follow-up strengthening. The original raw-CMI fixture checked exact node presence, closure, binder identity, selected field/constructor/application identities, dependent-hole structure, and functor substitutions, but missed a binder reachable only through a stored `Rexp_fun` annotation. The fix round adds that producer, direct/functor-import checks, and an explicit old-stamp-versus-fresh-stamp substitution assertion. |
| CC-5 | LOW VERIFIED: wrong non-bool application fixture | Fixed with a Total `total_length` helper; the expected result is a boolean-type error rather than a mode error. |
| CC-6 | LOW JUDGEMENT: generic dev-loop tooling and unused helper | Adopted. The tooling change and unused helper are absent from the branch. |
| CF-1 | HIGH VERIFIED: metadata occur-check cycle | Fixed. Semantic occurrence traversal excludes derived node annotations; multi-predicate and own-domain/codomain composition cases are fixtured. |
| CF-2 | MED VERIFIED: rollback degrades diagnostics | Fixed. Exceptions are converted to materialized `Location.error` values before type-graph backtracking. |
| CF-3 | MED VERIFIED mechanism / JUDGEMENT severity: frame relinks written spelling | Fixed completely after follow-up. The initial descriptor whitelist fixed the refined-string case but left other payload spines mutable. Frame views now structurally copy every non-variable descriptor while retaining ambient-variable, arrow-commutation, and refinement-cell sharing where required. |
| CF-4 | MED JUDGEMENT: missing CMI/CMT magic bump | Adopted. The common artifact version advances from 583 to 584, producing `Caml1999I584` and `Caml1999T584`. |
| CF-5 | LOW VERIFIED: annotated predicate `let` flip | Fixed by retaining the simple monomorphic constraint correspondence; the discriminating fixture remains. Explicit polymorphic binding annotations are separately rejected by the syntactic gate. |
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
| Reentry transaction: warnings/CMT/delayed checks/allocations, TyVarEnv, modes, ghost and locks, frozen-error rollback | `typing/typecore.ml:13799`, `typing/types.ml:1219`, `typing/types.ml:1732` |
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

### Full suite (historical first-GREEN run)

This pre-round-4 run is retained for audit history and is superseded by the
round-4 validation above.

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
3. Eleven inherited golden changes remain intentionally unpromoted. They are
   independent of this piece but keep the aggregate suite red until the owning
   stack changes normalize those references.

No open functional defect is known in the consolidated predicate-typing
implementation.
