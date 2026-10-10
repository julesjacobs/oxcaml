# HM simplification inventory

This inventory separates source cleanup, branch integration, library contracts,
and proposed Vox changes. It preserves the imperative level-based algorithm,
its proof ladder, explicit witnesses/functions instead of SMT quantifiers,
and the existing soundness, completeness, rejection and principality statements.
Checked boxes record implemented work; open boxes retain deferred or partial
proposals. Final validation status is recorded separately below.

## Baseline and evidence

HM baseline: `19827b4c33`, branch
`jujacobs/vox/hm-direct-pool-routing-20260917` in `worktrees/agent-1`.
The implicit-refinement implementation was inspected in the **working tree** of
`worktrees/agent-3`, branch
`jujacobs/vox/implicit-refinement-context-20260910`. Its HEAD is `57d46c8c0f`,
but substantial relevant changes are uncommitted; HEAD alone is not the tested
implementation. Do not overwrite that work or treat these branches as merged.

The module dependency closure of `Hm_routed_infer`, `Hm_effective_sound` and
`Hm_effective_complete` contains 156 HM modules and 21,094 physical source lines.
This excludes shared Vox libraries and test modules. Module dependencies include
unused definitions within imported modules; this is not a runtime reachability
count. Counts below are lexical matches in that same closure, including any
matches in comments. Categories overlap and are not estimates of lines saved.

| Pattern | Occurrences | Files | Meaning |
| --- | ---: | ---: | --- |
| `refine_` token | 7,945 | 127 | Explicit checking and unpacking sites |
| `let refine_` | 1,977 | 127 | Subset of the preceding row |
| `let refine_ x = x in` | 908 | 125 | Candidate redundant unpacking |
| `let u = () in` | 1,499 | 125 | Often supplies a proof-premise argument |
| `Ghost.t` | 411 | 23 | Includes annotations and repeated contracts |
| Identifiers ending `_def` | 4,513 | 126 | Definition-lemma mentions |
| `put_frame` | 238 | 59 | Heap-frame lemma mentions |
| Generated adapter/witness bindings | 130 | 9 | See measurement recipe below |

Two experiments provide limited positive evidence:

- Isolated `Pref.read` and `Pref.write` contracts and forwarding clients check
  without explicit refinement unpacking. Missing ownership and a false result
  claim are still rejected.
- A simplified allocator checks against the same logical postcondition using
  the existing allocation/frame lemma interfaces. Wrong-pool and wrong-level
  mutations are rejected. This is a **modular check**, not a recheck of the heap
  lemmas or the complete HM development. The experiment used a boxed result
  because the newer branch rejects the unboxed result predicates used by HM.

These experiments establish that syntax cleanup is promising. They do not
establish that all wrappers are removable or that a combined compiler works.

## Checking-only baseline (2026-09-19)

The installed native compiler at `_install/bin/ocamlopt.opt` checked the complete
closure in **29.463 seconds wall time** with `-stop-after typing -c` and
`-extension refinement_types`. This includes parsing, ordinary type/mode and
termination checks, refinement VC generation/SMT, and interface writing. It
excludes code generation, linking, and runtime tests; it is not SMT-only time.
All 166 source/interface files (156 HM modules plus 10 library files) were copied
to an isolated directory with no prebuilt dependency interfaces. A deliberately
false refinement was rejected under the same flags before timing the run.
This is one warm-machine measurement, not a statistical benchmark.
The earlier 357-second suite used a different compiler execution path and ran
both backends, so the ratio does not isolate code-generation cost.

Logs: `/tmp/hm-check-baseline-20260919/checking.log` and `result.json`.
Reproduction script: `scripts/measure-hm-check.py`; rerun with the worktree and a
fresh output directory. Use an installed compiler for subsequent measurements.

## Current status (2026-09-19)

- P0: the original 166-file closure checks in 32.908 seconds with the integrated
  installed native compiler. The original bytecode/native routed HM fixture
  passes. Stable immutable field projections retain their structural logical
  identities; computed arguments receive fresh names. Positive and negative
  projection/callback regressions pass. `Borrow_iarray.parallel` states the
  callback results explicitly across opaque higher-order calls.
- S1–S3: a checked pass over 127 modules removed 6,944 `refine_` occurrences,
  old unit-witness locals, and 35 redundant token-membership annotations.
  Necessary function adapters and public refinement syntax are retained.
- S4: lowering and clean-copy result types live in specification modules.
  This closure has 154 HM modules and checked in 31.905 seconds before later
  factoring. Earlier algorithms remain available as proof-ladder examples.
- S5–S6: repeated goal contracts and copy/unifier proof helpers are implemented.
  The full closure and runtime artifact generation passed after extraction
  (32.897 seconds checking). Copy-history composition and terminal-child
  facts now have named checked helper contracts.
- V3: discarded ghost statements emit no runtime placeholder. A million-step
  recursion and a real trailing-effect control pass in bytecode and native code.
- V6: direct tuple/immutable-record matches preserve structural descent.
  Same-argument and increasing tuple recursion are rejected. `finite_unique`
  now uses a tuple match and checks with the updated compiler.
- V9: successful calls to the standard `Iarray.init` expose their length.
  Recognition uses the standard value's UID; shadowed functions do not gain
  the law. This extends the trusted library model, like `Iarray.append` and
  `Iarray.sub`; it does not verify the standard-library implementation or assert
  termination. Callback order, aliases, false lengths and shadowing are tested.
  The redundant HM length guard is removed.
- V12: `scripts/measure-hm-check.py --runtime-view` generated raw Lambda and
  assembly successfully for the complete closure (159 implementation files). Its output is compiler IR and assembly, not
  regex-erased OCaml or a claim about reachable runtime LOC.

All times are single warm-machine runs, not statistical performance claims.
The adapter inspection restores its type-checker snapshot, avoiding cached
pattern-local GADT equations; the standard library rebuild exercises this.
The final bytecode/native HM fixture passes (121 seconds including compilation
and execution), as do 42 typing-refinement tests and the focused ownership,
closure, callback, array, termination and ghost-erasure regressions. The final
checking-only run takes **31.632 seconds** for 164 inputs (154 HM modules plus
10 library inputs), with no code generation, linking or runtime execution in
the timed interval. The false-refinement control is rejected. Runtime artifact
generation also succeeds; all 159 source snapshots, raw Lambda dumps and
assembly files listed in its index exist.

Final logs: `/tmp/hm-final-check-20260919/`, `/tmp/hm-final-runtime.log`,
`/tmp/hm-typing-refinement-suite-final.log`. The source closure now contains
20,186 physical lines and 881 lexical `refine_` occurrences. This is not a
runtime LOC count. Source passes removed 6,944 occurrences; the closure also
lost two older algorithm-module dependencies.

Merlin import is complete with three typecore conflicts resolved, retaining
Merlin's error recovery. Final `codex review --uncommitted` reports no
actionable findings (`/tmp/hm-simplification-codex-review-merlin.log`). Merlin
tests pass (`/tmp/hm-merlin-test-final.log`); the final browsing/inlay-hint
metadata cases preserve implicit annotations and have been formatted.

## Integration prerequisite

- [x] **P0 — Establish a compiler containing both sets of features.** Integrate
  implicit refinements with HM's `Pref` heap theory, unboxed result projections,
  unboxed ghost fields and function witnesses. The current implicit-refinement
  compiler rejects `r.#value` in predicates, and the complete allocator proofs
  need the HM heap model. Rebuild interfaces from source with the combined
  compiler; copying compiled interfaces across the divergent compilers is not
  an integration strategy. First recheck the original allocator and supporting
  proofs, then the original full HM suite, before any bulk cleanup. Keep the
  original unboxed representation; the boxed experiment is not a production fix.

## Source changes enabled by newer Vox

- [x] **S1 — Remove redundant refinement introduction and unpacking.** Start with
  `effective_allocator.ml`, `effective_lower_write.ml` and `copy_cleanup.ml`,
  then proof modules and the driver. Replace redundant `let refine_` bindings
  with ordinary bindings and let expected contracts discharge result/argument
  refinements. Remove `let refine_ t = t` inside postconditions too. Preserve
  declared contracts, ghost/total modes and ownership requirements. An explicit
  refinement check can establish a fact at a particular evaluation point;
  remove each only when the implicit check still proves the intended obligation.
  **Evidence:** both experiments above; 908 self-unpacking sites are candidates.

- [x] **S2 — Remove locals needed only by old dependent-argument restrictions.**
  Replace `let u = () in lemma ... (refine_ u)` with `lemma ... ()` where checked.
  Inline constructor/projection arguments and final `let out = ... in out`
  bindings when their names carry no needed dependency. Preserve snapshots of
  mutable observations, evaluation order, borrow scopes and identities reused
  by later contracts. The newer implementation supports logical identities for
  non-stable arguments; this does not make repeated effectful reads equivalent.
  **Evidence:** allocator proof calls check with `()` directly; 1,499 unit locals.

- [x] **S3 — Remove intermediate token annotations when call checking suffices.**
  `effective_lower_write.ml` reannotates `state` with membership before reading
  and again before writing. With the facts available implicitly, pass the token
  or its borrow directly and let the primitive precondition check membership.
  Do not weaken `read` to permit unowned reads as part of this cleanup.
  **Check:** missing membership and reuse of consumed ownership must still fail.

- [x] **S4 — Move shared result types out of older algorithm modules.**
  `effective_copy_runtime.ml` uses `Clean_copy.instance` and
  `effective_lower_write.ml` uses `Level_lower.written`. Put shared carrier
  types in a small existing specification module where practical, preserving
  layouts and modalities. Check all users before removing dependencies: a
  module may also supply useful proof lemmas. Retain the earlier algorithms as
  proof-ladder examples. **Benefit:** clearer dependencies and more meaningful
  LOC counts; no runtime improvement is claimed from moving types alone.

- [x] **S5 — Name the repeated CPS result predicates.** The 713-line
  `hm_routed_infer.ml` repeatedly spells out `ran`, source/result agreement,
  routing origin, physical-pool equality, bucket equality and depth conditions
  in continuation types. Define small reflected predicates and total helpers
  for these repeated conjunctions. Keep input and output heap observations
  explicit. Ordinary library factoring should precede new language syntax.
  **Check:** each helper must expand to the same contract, including failure
  cases where the stored depth need not equal the entry depth.

- [x] **S6 — Extract proof blocks behind checked helper contracts.** Move
  mathematical bookkeeping out of executable cases in `hm_routed_infer.ml`,
  `effective_copy_runtime.ml` and `effective_unifier_runtime.ml`. Keep reads,
  writes, allocations, branch decisions, cleanup and continuation calls visible
  in runtime modules. Use existing ghost context records rather than packaging
  a new universal invariant indiscriminately. **Check:** helpers derive their
  conclusions; they must not accept the desired result as an assumed witness.
  Source movement improves reviewability but does not itself reduce total LOC.

## Vox and library improvements with broad potential

- [x] **V1 — Checked adaptation of dependent callback arguments.** The HM
  branch's adapter handles some result refinements but not equivalent refined
  callback inputs or nested callback heap aliases. This causes `call_facts_*`,
  `call_pool_*` and explicit forwarding closures in the driver and completeness
  proofs. First rerun minimized cases on the combined compiler: some may already
  be handled by implicit refinements. For remaining cases, elaborate checked
  wrappers that prove the original input contract and the expected output
  contract, respecting variance, totality, ghostness and unique captures.
  Do not insert unchecked conversions under arbitrary arrows or containers.
  **Evidence:** continuation and nested-callback findings in
  [unifier-vox-findings.md](unifier-vox-findings.md).

- [ ] **V2 — Make ghost parameters erase as completely as ghost record fields.**
  The HM code uses `Ghost.t` to avoid dummy argument slots. A compiler change
  could make direct ghost binders equally efficient and remove wrapper plumbing.
  This must retain logical identity, function-witness support and mode checking.
  Do not remove wrappers solely because their contents are ghost.
  **Check:** native calling conventions and bytecode behavior, including
  higher-order arguments and partial application. There are 411 wrapper-type
  mentions, not 411 known removable allocations.

- [x] **V3 — Remove erased proof barriers to tail calls.** Ghost statements
  after recursion previously left opaque expressions that prevented a loop.
  Current code moves proofs before tail calls. Preserve the compiler's required
  typing evidence while removing runtime optimization barriers.
  **Check:** a minimal recursive function with a trailing ghost lemma compiles
  to a loop; its proof still checks and any real surrounding effects remain.
  Until then, retain the current ordering workarounds.

- [x] **V4 — Improve heap observation propagation.** Repeated `put_frame`,
  allocation-membership and selected-read transport calls expose facts about
  `Heap.put` chains. Prefer pointwise checked library lemmas first; consider
  bounded, demand-driven instantiation at handles already present in a VC.
  Investigate the relationship between `Heap.mem h p` and `Heap.at h p`, and
  no-op writes, against the actual heap encoding. Exact heap equality is stronger
  than equality of one observation. Do not assume heap extensionality or add
  quantified SMT invariants. Any extension to trusted primitive laws needs an
  explicit semantic justification. **Evidence:** 238 `put_frame` mentions;
  allocation and no-op-write findings. **Check:** positive and false frame
  claims, aliasing, unrelated cells and solver-time measurements.

- [ ] **V5 — Add controlled definition unfolding.** Many proofs repeatedly call
  `_def` lemmas for the same shallow predicates. Try library lemmas that bundle
  common unfoldings; then evaluate opt-in, bounded unfolding for reflected
  definitions at concrete arguments. Recursive unfolding needs a depth/budget
  limit and an explanation of which facts were used. This is local automation,
  not a substitute for induction or witness construction.
  **Evidence:** 4,513 `_def` identifier mentions, not necessarily distinct calls.
  **Check:** compare verification time as well as source size; SMT expansion can
  cost more than the source syntax it saves.

- [x] **V6 — Preserve structural descent through tuple/record matches.**
  Equivalent nested matches currently work where tuple matches lose the
  descendant relation. Simplify `finite_unique` and template/environment proof
  cases after preserving the actual constructor-field descent information.
  **Check:** accepted tuple matches plus rejected same-argument and increasing
  recursion. Runtime termination remains outside this project; total proof
  recursion must still be justified.

- [ ] **V7 — Support relevant total closures in reflected definitions.**
  Historical failures with local lambdas led to finite parameter vectors and
  duplicated evaluators in the declarative semantics. First retest against the
  newer higher-order work. Where still unsupported, preserve captures and
  function identities outside SMT and generate checked defining equations or
  pointwise proof obligations. Do not identify functions by extensional equality
  or reflect partial calls. Keep first-order certificate data where it makes
  the specification clearer; closures are not automatically a better design.
  **Check:** the actual HM evaluator examples and rejection cases for partial,
  mutable or ghost-to-runtime captures.

- [x] **V8 — Improve scoped proof-fact interfaces.** Proof blocks currently need
  explicit contracts to export facts about names visible outside the block.
  New implicit refinements may remove some unit-witness plumbing, but do not
  establish an arbitrary lemma's missing postcondition. Prefer small named
  postconditions and library helpers. Investigate optional elaboration support
  only after minimizing cases in `hm_effective_complete.ml` and model transport.
  **Check:** branch-local assumptions, fresh identities and inaccessible local
  witnesses must not escape. Keep existential witnesses explicit and universal
  facts as total functions, as required by Vox's quantifier-free design.

- [x] **V9 — Supply a verified array-initialization contract.**
  `Hm_pool_capacity.create` checks that `Iarray.init limit ...` returned an array
  of length `limit`. A library contract for successful initialization can expose
  that length and remove the redundant guard. Start with the length property;
  element properties need explicit witnesses/functions as usual. Check whether
  the newer array library already provides the needed operation before adding
  another one. Preserve callback effects, evaluation order and allocation/error
  behavior. This needs a real checked implementation or an explicitly audited
  primitive contract, not an assumed length certificate.

- [x] **V10 — Support descriptive exceptional exits at unboxed layouts.**
  `failwith`/`raise` at value layouts cannot directly inhabit the unboxed HM
  result. The code factors some checks into boxed-return helpers and uses
  `assert false` elsewhere. A suitable layout-polymorphic exceptional operation
  would simplify these branches. It must remain partial; an exception does not
  supply a terminating proof of false. This is an OxCaml API/layout issue.

- [ ] **V11 — Make unsupported premises and mode failures actionable.**
  Report the omitted expression, relevant heap/callback type and source location
  when reflection fails, and explain whether `@ total` constrains a callback or
  its result. Minimize historical nested-option observation failures before
  treating them as current bugs. Better diagnostics reduce blind annotations
  and unnecessary representation changes; they do not alone reduce proof size.

- [x] **V12 — Provide a trustworthy view of runtime code after erasure.**
  A source-correlated erased IR view would make the imperative algorithm easier
  to review and permit a defensible runtime/proof size split. Start with tooling
  around existing compiler dumps; do not claim a regex-deleted source file is
  the executable semantics. Include remaining allocations, continuation calls,
  array operations and placeholders. This addresses reviewability rather than
  changing the theorem or the runtime algorithm.

## Recommended sequence and completion criteria

1. P0: integrate compiler features and recheck the unmodified development.
2. S1–S3: clean the allocator, lowering write and cleanup modules first; extend
   to the driver and theorem modules only after the small cases check.
3. S4–S6: separate shared types and factor contracts/proofs, preserving the
   proof ladder and keeping actual mutation visible to reviewers.
4. V1–V3 and V9: target callback plumbing, erasure and the concrete array guard.
   Re-measure generated code after any wrapper or ghost-parameter change.
5. V4–V8: use minimized cases to decide which proof-automation features pay for
   their complexity. Do not promise wholesale removal of the mathematical
   proofs. V10–V12 can proceed independently once there is a stable baseline.

For each implemented batch: retain the same public theorem statements, recheck
its affected proof dependencies, include a corresponding false-contract test
where appropriate, and inspect erasure if runtime calling conventions could
change. Finish a migration with the integrated bytecode/native HM suite,
including deep CPS and sharing fixtures. Record verified source reductions and
verification time separately from runtime performance; benchmark only with the
installed optimized compiler. No LOC-savings or speedup target is asserted here.

Keep level pools, direct routing, copy memo cleanup, occurs checking and the
current ownership preconditions. Replacing them merely to shorten the proof
would change the implementation objective. Keep the declarative typing rules
and exported theorem statements readily reviewable.

## Measurement recipe

Use the module names in `hm_routed_infer_demo.ml`'s `all_modules` to locate
sources, then `_install/bin/ocamldep -modules` to compute the closure rooted at
`Hm_routed_infer`, `Hm_effective_sound`, and `Hm_effective_complete`. Count only
files under `testsuite/tests/vox`. Physical LOC includes blanks. The patterns
above are lexical: `\brefine_\b`, `\blet\s+refine_\b`,
`\blet\s+refine_\s+([A-Za-z_]\w*)\s*=\s*\1\s+in\b`,
`\blet\s+u\s*=\s*\(\)\s+in\b`, `\bGhost\.t\b`,
`\b[A-Za-z_]\w*_def\b`, and `\bput_frame\b`.
Generated adapter bindings match `let`, optional `refine_`, then one of
`call_facts_\d+`, `call_pool_\d+`, `\w*_witness\d+`, or
`state_argument\d+`. These counts include inherited helper definitions and
cannot establish a runtime/proof LOC ratio.

## Decisions from the implementation pass

- V1: identifier callbacks with different dependent input/result contracts are
  elaborated through checked eta wrappers. Equivalent aliases pass; unjustified
  input or result conversions fail. This does not coerce arbitrary containers.
- V4: the existing pointwise heap model suffices to remove 143 `put_frame`
  calls. Necessary calls remain. No heap extensionality or SMT quantifiers were
  added; this is a source simplification using existing heap laws.
- V5: use the S5/S6 checked helpers to bundle recurring unfoldings. Defer a new
  automatic unfolding mechanism: no evidence yet that its solver expansion
  improves on these explicit, bounded helper calls.
- V7: reflected local closures now support scalar logical parameters/results,
  including captures and curried arguments. Positive and false-result tests
  exercise their defining equations; partial and mutable reads are rejected.
  Keep HM's finite argument vectors: they also represent scheme arities and
  explicit existential instantiations. Closure support alone does not justify
  replacing that declarative representation.
- V8: retain explicit named postconditions on proof helpers; implicit refinement
  introduction removes their unit-witness plumbing. Do not export arbitrary
  branch facts or inaccessible local witnesses. Existing branch/deferred tests
  check rejection as well as successful propagation.
- V10: a layout-polymorphic `%raise` declaration supports the driver's descriptive
  overflow exit. The operation remains partial. Both compiler backends are tested.
- V11: unsupported reflected closures now suggest an explicit total function
  witness and a pointwise lemma. Broader diagnostic redesign is deferred until
  a concrete failing example warrants it.
- V2 is deferred: direct ghost-parameter erasure changes function calling
  conventions, including partial application and cross-module interfaces.
  `Ghost.t` already provides void-layout erasure for HM. Removing it before
  implementing and testing the complete calling-convention change would be a
  regression. Keep these wrappers in this simplification pass.

These dispositions do not claim that every proposed compiler feature is
implemented. In particular V2 and automatic unfolding remain future work.

The V7 HM probe (`/tmp/hm-closure-probe.ml`) defines
`eval (fun i -> prefix vs xi i) t` successfully, but its attempted pointwise
proof using a separately constructed closure is rejected. The closures do not
share a logical identity; Vox does not assume function extensionality. This
confirms that local closure reflection alone is insufficient to replace the
existing `eval_prefixed` proof interface. V7 remains partial rather than claiming
that the duplicated evaluator can now be deleted.

User disposition (2026-09-19): keep direct ghost-parameter erasure, automatic
unfolding and broader closure support deferred; do not implement those larger
projects in this pass. The existing proof interfaces and `Ghost.t` wrappers
remain.

## Checked interface boundary

The production path has 23 explicit `.mli` interfaces. They contain public
contracts and required representations; recursive workers and local proof
helpers remain in `.ml`. Module abbreviations in interfaces use destructive
substitution (`module D := ...`), so an abbreviation does not accidentally
re-export an entire proof module. Earlier ladder examples remain available.

Start with `testsuite/tests/vox/verified_hm.mli`. The public operations are
`infer` and `principal`. Results contain `root`, `ownership`, and ghost
`inferred_type` and `evidence` fields. On success the return contract directly
states that the owned graph represents the inferred type and that the input has
that type. `principal` supplies a substitution for any alternative valid typing;
on rejection it derives a contradiction from that typing instead.

Review the three `Spec` predicate definitions in `verified_hm.ml` as well.
`inferred` connects the input and inferred type to a certified completed run.
`represents` checks finite unfolding, root equality and readback equality;
`has_type` checks the declarative typing judgement. Their explicit `evidence`
argument carries the witnesses required by Vox's quantifier-free logic. Finite
trees, typing derivations, traces and pools stay private. Construction of these
witnesses is ghost code, and `infer` invokes the routed inferencer once.
Inference remains partial. The graph relation refers to the heap at return;
later mutations require a new proof that the relation holds.

The facade's full dependency check passes for 188 inputs in 33.545 seconds.
Native assembly confirms that `infer` calls `closed_hm` once, without runtime
evidence construction or proof traversal
(`/tmp/verified-hm-small-check-20260919/`).
The facade review found no issues. The broader uncommitted review reproduced an
unrelated deferred-argument limitation in `verification/vox_vc.ml`: overridden
unboxed-record fields use `eval` where boxed fields use `result`, rejecting some
valid dependent applications. Keep that compiler correction as separate work;
reproductions are `/tmp/review_unboxed.ml` and `/tmp/review_boxed.ml`, with review
output in `/tmp/verified-hm-review.log`.
The public-interface runtime cases and existing routed stress cases pass on
bytecode and native backends. The negative fixture rejects forged evidence,
unjustified rejection claims and principality for an unrelated input.
Final logs: `/tmp/verified-hm-small-runtime.log` and
`/tmp/verified-hm-small-rejected-final.log`. The final interface review has no
actionable findings (`/tmp/verified-hm-small-review.log`).

For the lower-level proof boundary, review these three interfaces:

- `testsuite/tests/vox/hm_routed_infer.mli`: source and compiled entry points,
  returned mutable graph roots, ownership tokens, and execution correspondence.
- `testsuite/tests/vox/hm_effective_sound.mli`: closed-program soundness and
  principality, including the finite readback witness and substitution witness.
- `testsuite/tests/vox/hm_effective_complete.mli`: completeness, rejection and
  factorization. The implementation's model-construction helpers are private.

Predicate meanings are also part of the review boundary. A contract mentioning
an opaque predicate is insufficient to explain a claim. Read its definition in
`hm_declarative.ml` (syntax, embedding and declarative typing),
`hm_effective_execution_spec.ml` (execution/source/result and `ran`),
`level_finite_spec.ml` (the heap/tree relation and readback), and
`level_mgu_spec.ml` (substitution). Follow the execution relation's referenced
specifications for copy, unification, level lowering and generalization. The
`embed` definition now lives in `hm_declarative.ml`, rather than inside
`hm_type_proofs.ml`; its mathematical definition is unchanged.

For the imperative-algorithm review, read the corresponding `.ml` implementations:
`hm_routed_infer`, `effective_unifier_runtime`, `effective_copy_runtime`,
`graph_occurs`, `effective_lower_runtime`, `effective_lower_write`,
`graph_representative`, `effective_compressed_representative`, `effective_bind`,
`effective_link`, `structure_link`, `effective_allocator`, `certified_copy`,
`copy_cleanup`, `representative_pool`, `effective_hm_unify`, `level_pool_routing`,
`hm_pool_capacity`, `fast_environment` and `fast_term`. Their interfaces expose
all input witnesses, heap/level assumptions and result guarantees. Runtime
entry points remain partial; total ghost theorem functions describe completed
runs. Pool routing, copying, cleanup and mutation remain visible in those
implementations.

Other proof bodies can be checked mechanically once those meanings and contracts
are accepted. The trusted base still includes the Vox compiler, SMT encoding and
primitive heap/library laws; `.mli` files do not turn that checker into Lean's
kernel. Interfaces are compiled alongside their implementations, not accepted as
standalone theorem axioms.

All fixture compilation lists now include each interface before its implementation.
The checking script includes interfaces in the dependency closure and counts HM
implementation modules separately. A fresh full check of 186 inputs (154 HM
implementations, 22 HM interfaces, 10 library inputs) passes in 32.935 seconds.
A negative check rejects access to five private workers/proof helpers and rejects
an implementation against an interface with a forged `false` return contract.
Logs: `/tmp/hm-interface-check5-20260919/` and
`/tmp/hm-interface-boundary-negative/`. The routed runtime fixture and runtime,
abstraction and template-instance negative fixtures pass. Interface review found
no actionable issues (`/tmp/hm-mli-codex-review.log`). The older principality
fixture exposed an existing unchecked level-overflow assertion in `hm_infer`;
that branch now raises the same capacity error as the routed inferencer.
The principality fixture passes after that correction. All five selected
runtime/negative fixtures pass; logs are `/tmp/hm-interface-hm_*.log`.

## Interpreter safety extension

Pro consultation: https://chatgpt.com/c/6aae7538-d54c-83ea-b463-dc5e651d7f9e
Objective: prove that a successful HM inference rules out untyped interpreter
errors, covering polymorphic let and recursive closures with explicit Vox
witnesses. The selected runtime design is now partial and fuel-free, requires
an erased typing derivation, and uses `unreachable_ ()` at impossible branches.
There is no execution-trace requirement. The finite value/environment evidence, erasure from the original typing
judgement, capture-avoiding parameter substitution, and direct interpreter
refinement all check. `Verified_hm.Spec.typing` supplies the erased derivation
needed by `Hm_interpreter.run`; no inference result is rechecked. The runtime
fixture exercises this composition. Runtime Lambda contains no certificate
construction or proof/substitution calls. The consultation synopsis is in
`hm-interpreter-pro.txt`.

The proof uses a typing judgement with type-parameter well-formedness premises
removed, while retaining scheme arity and the original typing rules. A checked
structural lemma derives it from `Hm_declarative.typed`. This simplifies ghost
substitution; its safety is proved directly rather than assumed.

The implementation also exposed two existing Vox datatype restrictions:
`[@@inductive]` does not support mutually declared datatypes or recursive fields
nested under `list`. Values and environments therefore share one inductive
sum, with the checked evidence distinguishing their uses.

The closing review also reproduced an existing automatic-adaptation limitation:
`let (id @ total) ~x = x` cannot be reused as
`x:{x : int | x >= 0} -> {x : int | x >= 0}`. The adapter tests structural type
equality before allowing polymorphic instantiation and then rejects labelled
parameters. This predates the interpreter/unreachable change and belongs with
the deferred function-adaptation work. Reproducer: `/tmp/Labelled_adapter.ml`.
