# Progress

Last updated: 2026-05-28.

## Current state

Branch: `jujacobs/llvm-fast-path-roots-integration`

PR: https://github.com/julesjacobs/oxcaml/pull/18

Current focus: AArch64 LLVM `try`/trap performance. The fast-path-roots
integration work and earlier compiler-benchmark work are background context.
The active implementation now uses optimizer-visible trap recovery edges instead
of the hot `_wrap_try` helper.

Note: despite the agent and branch name, the active goal has shifted from the
fast-path-root-slot optimization to native-like AArch64 trap-region handling for
the OxCaml LLVM backend. The updated `GOAL.md` is the source of truth.

## 2026-05-28 Update

The current `GOAL.md` is good enough as a guiding goal. It specifies the
native-like trap model and validation expectations without forcing one exact
LLVM spelling.

Goal audit:

- The goal should stay broad. It correctly asks for a native-like model and
  explicit LLVM control-flow facts, rather than requiring today's exact
  `invoke`/`landingpad token` spelling forever.
- The current implementation now satisfies the main shape of that goal:
  OxCaml emits real unwind edges for protected calls, LLVM classifies the
  recovery entry as `runtime-entered`, AArch64 owns the recovery ABI live-in
  rule, and the hot path no longer calls `_wrap_try`.
- The implementation also covers the important hardening found during the
  spike: multiple recovery entries, split unwind trampolines, nondominating
  publish rejection, extra runtime-entered live-in rejection, PHI rejection in
  recovery entries, and BranchFolding placement/tail-merge hazards.
- The remaining question is no longer "is the goal bad?" but "is this
  implementation reviewable and fast enough to land?" The main residual risk is
  performance, not a known correctness hole.

Implemented and validated since the previous note:

- AArch64 `Pushtrap`/`Poptrap`/recovery no longer update
  `Caml_state(exn_handler)` on the hot trap path. The intended model is that
  x26 is authoritative while executing OCaml code; runtime transitions already
  synchronize the domain-state field when needed.
- Updated `testsuite/tests/llvm-codegen/fast_path_roots.ml` expected output for
  the removed x28/#48 stores.
- Extended `testsuite/tests/llvm-codegen/trap_recovery_runtime.sh` with a hot
  try/call source case that rejects `[x28, #48]` stores in the generated
  AArch64 assembly.
- Rebuilt the installed compiler and refreshed `_runtest` before focused
  expect testing. The `no-rebuild` one-test target was stale until `_runtest`
  was refreshed.

Validation:

- `make llvm-test-one TEST=llvm-codegen/fast_path_roots.ml LLVM_PATH="$LLVM_PATH"`
  passed.
- `make llvm-test-one-no-rebuild TEST=llvm-codegen/trap_recovery_runtime.ml
  LLVM_PATH="$LLVM_PATH"` passed after the install rebuild.
- `make llvm-test-one-no-rebuild DIR=llvm-codegen LLVM_PATH="$LLVM_PATH"`
  passed before the final expect promotion: 92 passed, 2 skipped.
- `make llvm-test-no-rebuild LLVM_PATH="$LLVM_PATH"` passed before the final
  expect promotion: 6743 passed, 295 skipped.
- Fresh `make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` passed:
  6717 passed, 282 skipped, 0 failed, 6999 considered. Wrapper activity was
  nonzero: 6701 wrapper lines and 3339 fresh IR lines.

Performance:

- Representative try/trap microbenchmarks:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_final_no_domain_exn_store_20260528_020706.json`.
  The biggest try/call cases improved materially:
  `direct_call_in_try_hit` 1.378 -> 1.174, `closure_call_in_try_hit` 1.414 ->
  1.050, `closure_call_in_nested_try_hit` 1.401 -> 1.235, and
  `try_with_string_compare_hit` 1.470 -> 1.185.
- Fresh compiler binary benchmark against `_llvm_self_stage2_install`:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_final_no_domain_exn_store_20260528_030435.json`.
  Geomean LLVM-built/native-built ratio is 1.0478, improved from the previous
  validated 1.0539 and the old 1.0897 baseline.

Remaining performance risk:

- The native-like trap model has removed the helper call and the extra hot
  domain-state exception-handler stores, but some try/call cases are still slow
  (`closure_call_in_nested_try_hit` is 1.235 and
  `try_with_string_compare_hit` is 1.185).
- Compiler binary speed is much better than the old baseline but still around
  4.8% slower geomean. The largest current compiler-benchmark module slowdown
  is `llvmize.ml` at 1.069.

Recommended next step:

- Do a focused review pass over the current diff, treating the native-like trap
  model as the chosen design unless a concrete correctness issue appears.
- Keep the `GOAL.md` as-is unless new evidence changes the target model.
- Before landing, either explain the remaining `closure_call_in_nested_try_hit`
  and `try_with_string_compare_hit` slowdowns, or reduce them to explicit
  follow-up work with assembly evidence.
- Commit the code/test/docs state only after that review pass has checked that
  stale spike-only comments do not describe current production behavior.

Follow-up production cleanup:

- Removed the dead AArch64 cases from the old non-AArch64 `wrap_try` /
  returns-twice `Pushtrap` branch in `backend/llvm/llvmize.ml`. The active
  AArch64 path is `emit_aarch64_pushtrap`; the old branch now fails if it is
  accidentally reached on AArch64 instead of silently emitting the retired
  model.
- Removed the now-unused `read_link_register` helper, which only served that
  retired AArch64 `wrap_try` path.
- The first focused rebuild failed because login shells inherited
  `OPAMSWITCH=modal-kinds-rocq` from the parent app environment. Set the global
  switch to `oxcaml-5.4.0+oxcaml`, updated `~/.zprofile` to clear that stale
  inherited switch for login shells, and confirmed a fresh shell now reports
  `OPAMSWITCH=oxcaml-5.4.0+oxcaml`.
- Validation after the cleanup:
  `make llvm-test-one TEST=llvm-codegen/trap_recovery_runtime.ml
  LLVM_PATH="$LLVM_PATH"` passed, then
  `make llvm-test-one-no-rebuild DIR=llvm-codegen LLVM_PATH="$LLVM_PATH"`
  passed with 92 passed, 2 skipped, 0 failed.
- Tightened the LLVM codegen boundary so `FunctionLoweringInfo::set` receives
  the existing `DominatorTree` before it decides whether a token landingpad
  should be marked as an ordinary EH pad. This now uses the same
  publish-dominates-invoke rule as SelectionDAG lowering instead of the weaker
  old "a publish exists somewhere" predicate.
- Removed the now-unused public `hasOxCamlTrapPublishForRecoveryPad` helper, so
  the only exported publish predicate is the dominance-backed one.
- Rebuilt the agent LLVM tools after that interface change:
  `ninja -C /tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build llc
  opt FileCheck clang`.
- Reran focused LLVM trap recovery RUN coverage manually with the rebuilt
  `llc`, `opt`, and `FileCheck`: positive, malformed-shape negative, runtime
  live-in negative, branch-folder, and Linux triple smoke checks passed.
- Reran `make llvm-test-one-no-rebuild DIR=llvm-codegen LLVM_PATH="$LLVM_PATH"`
  with the rebuilt wrapper tools: 92 passed, 2 skipped, 0 failed.

## Current decision

Use a target-owned AArch64 trap-region design for Patch 2.

Do not use inline-asm `callbr` as the final owner of x26 publication:

- with `~{x26}`, LLVM warns about clobbering reserved X26;
- without the x26 clobber, final assembly writes x26 but MIR does not know.

The target-owned design should have:

- one trap-entry/publish operation that stores the trap block fields, stores or
  materializes the recovery label from the modeled recovery block, publishes
  x26, and exposes normal/recovery successors to Machine CFG through an explicit
  edge owner;
- one recovery operation that snapshots x0/x26/x27/x28 as true recovery ABI
  outputs at the dedicated recovery block, after any required stack/frame-pointer
  repair and before ordinary handler code consumes the values.

## Latest evidence

Experiment directory:
`agent-state/llvm-fast-path-roots-integration/try_trap_spikes/callbr_truth_test`

Current plan:
`agent-state/llvm-fast-path-roots-integration/TRY_TRAP_PERF_PLAN.md`

Key result file:
`agent-state/llvm-fast-path-roots-integration/try_trap_spikes/callbr_truth_test/RESULTS.md`

LLVM-modifying recovery spike:

- Added experimental `llvm.aarch64.oxcaml.trap.recover`.
- Lowered it in AArch64 DAG instruction selection to ordered copies from x0,
  x26, x27, and x28.
- Added those physical registers as live-ins on the recovery block.
- Initially used a broad temporary hook that marked `gc "ocaml"` `callbr`
  indirect targets as landing-pad-like blocks so MachineVerifier permits
  allocatable physical live-ins.

The stronger proof case is
`callbr_target_recover_intrinsic_abi_only.ll`. It fixes the earlier review
critique where the tiny test used x1 in the recovery block. The recovery block
now uses only recovery ABI values.

Validated commands with the branch-local LLVM tools:

- `opt -passes=verify callbr_target_recover_intrinsic_abi_only.ll`
- `opt -O2 callbr_target_recover_intrinsic_abi_only.ll`
- `llc -verify-machineinstrs -stop-after=finalize-isel`
- `llc -verify-machineinstrs -stop-after=greedy`
- full `llc -verify-machineinstrs`

The recovery MIR shape is:

```mir
bb.3.exn_entry (machine-block-address-taken, landing-pad, inlineasm-br-indirect-target):
  liveins: $x0, $x26, $x27, $x28
  %14:gpr64 = COPY $x0
  %15:gpr64 = COPY $x26
  %16:gpr64all = COPY $x27
  %17:gpr64all = COPY $x28
```

The final recovery assembly is:

```asm
LBB0_2:
  add x0, x0, x26
  ret
```

This proves the recovery-copy mechanism, not the whole hidden edge.

LLVM-modifying publish spike:

- Added experimental `llvm.aarch64.oxcaml.trap.publish`.
- Lowered it to `OXCAML_TRAP_PUBLISH`, with explicit x26 def and memory side
  effect.
- Kept `OXCAML_TRAP_PUBLISH` as one pseudo through post-RA scheduling and
  emitted the ordered `str previous`, `str recovery target`, `mov x26` sequence
  in `AArch64AsmPrinter`.
- Narrowed the landing-pad-like classification hook to only a `gc "ocaml"`
  `callbr` target whose immediately preceding instruction is a matching
  trap-publish intrinsic with a `blockaddress` naming that target.

Validated commands with the branch-local LLVM tools:

- `opt -passes=verify callbr_publish_pseudo_abi_only.ll`
- `llc -verify-machineinstrs -stop-after=finalize-isel`
- `llc -verify-machineinstrs -stop-after=greedy`
- `llc -verify-machineinstrs -stop-after=aarch64-expand-pseudo`
- full `llc -verify-machineinstrs` on macOS and Linux triples
- negative `gc "ocaml"` `callbr` test without trap-publish

Key result: expanding the publish pseudo before final scheduling is not enough.
When the pseudo was expanded to separate `str`, `str`, and `mov` instructions in
`AArch64ExpandPseudoInsts.cpp`, final scheduling could move `mov x26` before the
stores. Keeping one pseudo until asm emission preserves the required
store-then-publish order.

Runtime-entered block classification spike:

- Added a generic `runtime-entered` MachineBasicBlock bit and MIR attribute.
- Taught MachineVerifier and LiveIntervals that `runtime-entered` is an
  ABI-entry block where target-defined physical live-ins are legal.
- Taught PHI copy placement and critical-edge splitting to treat
  `runtime-entered` successors like hidden-transfer ABI successors.
- Changed the OxCaml trap-publish match to set `runtime-entered`, not
  `landing-pad`, on the matching recovery target.

Validated commands with the branch-local LLVM tools:

- rebuilt `llc` and `opt`
- `opt -passes=verify callbr_publish_pseudo_abi_only.ll`
- `llc -verify-machineinstrs` through instruction selection, greedy register
  allocation, pseudo expansion, and final assembly on macOS
- `llc -verify-machineinstrs` through greedy and final assembly on Linux
- `llc -run-pass=none -verify-machineinstrs` on generated MIR to check
  `runtime-entered` MIR parsing
- negative MIR test with `runtime-entered` removed rejects x0/x27/x28 live-ins
- negative IR test with recovery but no matching trap-publish fails because the
  recovery block is not an ABI-entry block
- BTI variant still emits `bti j` at the address-taken recovery block

Key result: the generic `runtime-entered` block concept works for the narrow
combined publish/recover spike and avoids EH-pad preserved-mask behavior.

Devil's advocate experiments:

- Good failures: mismatched publish target, intervening instruction between
  publish and `callbr`, recovery in an ordinary block, and call-before-recovery
  are rejected by MachineVerifier.
- Bad passes: an extra `$x1` live-in on a `runtime-entered` block is accepted;
  an empty instruction before recovery is accepted; protected-path SSA can be
  used in recovery through a PHI and becomes an x1 use in final assembly; two
  publishes before one `callbr` are accepted.
- Biggest design issue found: a call inside the protected `try` body has no
  Machine CFG recovery edge to the recovery block. The current `callbr` carrier
  models a hidden edge only at publication time, not from every runtime-entering
  operation while the trap is active.

Fix prototypes:

- Added spike validation so malformed recovery blocks are not classified as
  `runtime-entered` unless the publish/recover shape is exact.
- Added a MachineVerifier spike that rejects non-x0/x26/x27/x28 live-ins on
  `runtime-entered` blocks and rejects PHIs there.
- Positive publish/recover still verifies and emits the ordered publish
  sequence.
- Former bad passes now fail: not-first recovery, protected-path SSA in
  recovery, extra `$x1` live-in, and two publishes before one `callbr`.
- Added a MIR-only prototype recovery successor from a protected-body call block
  to the `runtime-entered` recovery block. It verifies with
  `llc -run-pass=none -verify-machineinstrs` and
  `llc -run-pass=greedy -verify-machineinstrs`.
- Conclusion: do not abandon the approach, but the final design must model an
  active trap region, not just one hidden edge at publication time.

## Open issues

- The new `runtime-entered` hook is still a spike. Its generic-code uses need a
  fuller audit before treating it as production-ready.
- The recovery operation is now constrained to blocks that were classified as
  `runtime-entered`, but the diagnostic is still late and generic.
- `runtime-entered` currently has an AArch64/OxCaml-specific verifier check in
  generic MachineVerifier. That is fine for the spike but should become a
  target-owned ABI live-in rule before this is production code.
- The current spike rejects protected-path SSA values in recovery. Real lowering
  must keep that invariant and use explicit reloads for any required state.
- The experiment does not prove that real runtime transfers always have current
  x27/x28 state. The target-owned trap-entry/publish operation must prove that.
- The plan must replace `_wrap_try`'s other effects too: stack repair, optional
  x29 restore, runtime-register rethreading, and the rule for
  `Caml_state->exn_handler`.

## Next step

IR-level edge experiment:

- Confirmed a plain `invoke` to a non-EH recovery block is invalid IR.
- Confirmed normal LLVM EH (`landingpad { ptr, i32 }`) preserves the edge but
  pulls in the wrong ABI/codegen: x0/x1 EH live-ins, EH labels, personality/LSDA,
  and x0 is consumed before OxCaml recovery can read it.
- Prototyped a hybrid shape: IR uses `invoke` to a `landingpad token` block
  followed by `trap.recover`; SelectionDAG recognizes that as an OxCaml recovery
  edge, skips normal EH codegen, and marks the destination `runtime-entered`.
- This verifies through `opt -passes=verify`, `opt -O2`, `llc
  -stop-after=finalize-isel -verify-machineinstrs`, and final `llc
  -verify-machineinstrs`.
- Resulting MIR has the call block successor edge to a `runtime-entered`
  recovery block with exactly x0/x26/x27/x28 live-ins. Resulting assembly has no
  personality/LSDA for the OxCaml token landingpad path.

Next step: turn the hybrid prototype into a proper design boundary. Factor the
OxCaml recovery-block recognizer, make the active trap-region validation explicit
instead of ad hoc local scans, and decide whether the source IR spelling should
remain `landingpad token` or become a dedicated OxCaml marker with the same
optimizer-visible edge discipline.

Follow-up completed:

- Factored the current IR spelling into `SelectionDAG/OxCamlTrapUtils.{h,cpp}`.
- Added in-tree AArch64 CodeGen tests for the positive construct and five
  malformed shapes.

Compiler-side integration completed:

- AArch64 `Pushtrap` now emits a trap-publish intrinsic, a token landingpad
  recovery entry, and a direct `invoke` unwind edge to that entry.
- AArch64 raise calls inside an active trap region are emitted as `invoke`s to
  the active recovery entry followed by `unreachable`.
- AArch64 trap regions are keyed by the static `Pushtrap` instruction id, not
  by handler label, so multiple static trap regions can share one OCaml handler
  block.
- Shared handler blocks receive the recovered exception bucket through a
  handler-local entry alloca. The recovery entry remains responsible for
  restoring SP/x29, recovered domain state, recovered allocation pointer, and
  the previous trap chain before branching to ordinary handler code.
- LLVM recovery classification now accepts `gc "oxcaml"`, recovery-local
  values, constants/basic blocks/metadata, inline asm operands, and entry-block
  allocas. It still rejects protected-path SSA values and PHIs in the recovery
  block.

Focused validation:

- Rebuilt local LLVM `clang`, `llc`, `opt`, and `FileCheck`.
- Rebuilt the installed native compiler with
  `make install LLVM_BOOT_BACKEND=0 LLVM_BACKEND=0 OCAMLPARAM= BUILD_OCAMLPARAM=`.
- Compiled and ran a minimal LLVM-backend `try` executable: normal path prints
  `2`, exceptional path prints `42`.
- Added and ran `testsuite/tests/llvm-codegen/trap_recovery_runtime.ml`.
- Ran nearby `llvm-codegen` script tests `poll_statepoint.ml` and
  `long_frame.ml`.
- Manually checked LLVM positive lowering for `oxcaml-trap-recovery-invoke.ll`,
  `oxcaml-trap-recovery-invoke-multiple.ll`, and the new
  `oxcaml-trap-recovery-invoke-alloca-operands.ll`, including `opt -O2 | llc
  -verify-machineinstrs`.
- Rechecked the protected-path SSA negative test still rejects with
  `trap recovery intrinsic must be in a runtime-entered ABI block`.

Remaining risk:

- The recovery-block classifier is intentionally narrow, but the allowed operand
  set should still get reviewer attention. The important invariant is that
  recovery code may use recovered ABI values and entry allocas, but not
  protected normal-path SSA values.
- Full self-stage validation has not been rerun after this integration patch.
- Added a branched protected-body positive case with two invokes unwinding to
  one recovery block. This found a real missing integration point:
  `runtime-entered` successors also need EH-pad-like treatment in
  `MachineBlockPlacement` and `BranchFolding`.
- Updated placement/folding enough that final `llc -verify-machineinstrs` now
  passes for the branched case on Darwin and Linux.

Current LLVM-side construct:

1. Emit one `trap.publish` naming the recovery block.
2. Emit ordinary protected calls as `invoke ... unwind label %recovery`.
3. Start `%recovery` with `landingpad token cleanup`.
4. Immediately call `trap.recover`.
5. Recovery code may only use constants or values defined in the recovery block.

This is strong enough to start planning OCaml codegen changes, but one design
question remains: the recognizer currently accepts exactly one publish naming a
recovery block anywhere in the function. That is enough for the prototype and
tests, but a production design should encode the active trap region explicitly,
or prove the publish dominates each protected `invoke` and is not stale.

Follow-up hardening:

- Removed the old `callbr` runtime-entered classification path. The `callbr`
  spike shape is now a negative test; only the `invoke`/`landingpad token`
  construct may classify a recovery block as `runtime-entered`.
- Added `IntrNoMerge` to both `trap.publish` and `trap.recover`. This was
  needed because CodeGen `simplifycfg` merged identical token landingpad
  recovery blocks, rewrote one blockaddress to `inttoptr (i32 1 to ptr)`, and
  would have made a publish store the wrong recovery target.
- Tightened the recovery-pad recognizer to require `landingpad token cleanup`
  and to only count blockaddresses belonging to the same function.
- Added a positive LLVM CodeGen test with two independent recovery entries and
  a nested-trap shape where two recovery entries branch to the same handler
  block. Both recovery entries survive through `simplifycfg`, become
  `runtime-entered`, and verify on Darwin and Linux.
- Added a negative LLVM CodeGen test proving the retired `callbr` shape is not
  classified.

Current remaining LLVM-side issue: the recognizer still proves only the local
shape and publish target, not the active trap region. The OCaml emitter can
generate the intended shape now, but the final LLVM design should either encode
the active trap-region identity in the IR construct or add a dominance/region
check that proves each protected `invoke` unwinds to the recovery entry named by
the active publish.

2026-05-27 full validation update:

- `make llvm-test` passed after updating the stale `fast_path_roots.ml`
  expectation for the new trap recovery emission shape:
  6743 passed, 295 skipped, 0 failed, 7038 considered.
- `make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` initially failed during
  stage2 main linking with `No space left on device` under the agent temp dir.
  This was an environment/storage failure, not a compiler failure.
- Removed generated build/temp artifacts only, freeing disk from about 643M to
  about 100G available, removed the partial failed stage2 main/install dirs, and
  reran the self-stage2 validation.
- The retry built and smoke-tested stage1 and stage2 with fresh wrapper IR
  activity, then the stage2 testsuite passed:
  6717 passed, 282 skipped, 0 failed, 6999 considered.
- `git diff --check` passed after the validation run.

2026-05-27 post-validation benchmark update:

- Ran representative generated-code microbenchmarks with `PAIRS=3`:
  `agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/summary_post_full_validation_20260527_233923.json`.
  Aggregate over 36 cases: geomean LLVM/native `0.9075`, median `1.0143`,
  min `0.2958`, max `1.5118`.
- Top remaining generated-code slowdowns in that run:
  `env_find_same_layered_hit` `1.512x`,
  `closure_call_in_try_hit` `1.397x`,
  `closure_call_in_nested_try_hit` `1.394x`,
  `direct_call_in_try_hit` `1.350x`,
  `try_with_string_compare_hit` `1.318x`.
  All reported `wrap_try_refs=0`.
- Ran compiler-binary benchmark with clean `_native_install` versus validated
  `_llvm_self_stage2_install`, `--pairs 7`:
  `agent-state/llvm-fast-path-roots-integration/compiler_binary_bench/summary_post_full_validation_stage2_20260527_233923.json`.
  Aggregate: geomean LLVM-built/native-built `1.0539`, median `1.0502`,
  min `1.0444`, max `1.0722`.
- Compiler-binary representative files:
  `env.ml` `1.044x`, `ctype.ml` `1.054x`, `typecore.ml` `1.047x`,
  `translcore.ml` `1.054x`, `typemod.ml` `1.050x`,
  `cfg_to_linear.ml` `1.070x`, `cfg_selectgen.ml` `1.047x`,
  `llvmize.ml` `1.072x`, `regalloc_irc.ml` `1.047x`.

2026-05-28 LLVM trap-region hardening update:

- Reproduced a real verifier failure in
  `oxcaml-trap-recovery-invoke-multiple.ll` after `branch-folder`:
  `exn_a` and `exn_b` had become identical after recovery values were dead, so
  BranchFolding tail-merged `exn_a` into the layout-successor `exn_b`.
  That created a normal machine CFG edge into a `runtime-entered` ABI block and
  dropped the required `$x26` live-in from `exn_b`.
- Fixed BranchFolding to treat `runtime-entered` blocks as ABI entry labels:
  they are not profitable tail-merge inputs, not whole-block common-tail
  targets, not no-successor merge candidates, not predecessor merge candidates,
  and not candidates for adjacent inlining / branch-only forwarding / placement
  rewrites that would create ordinary control flow into the ABI entry.
- While expanding focused LLVM tests, found the `callbr` negative test was
  actually being accepted because the continuation recognizer could classify a
  `callbr` indirect target as a runtime recovery entry. Fixed that by rejecting
  recovery continuations with a `callbr` predecessor.
- Rebuilt the agent LLVM tools:
  `ninja -C /tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build llc opt FileCheck clang`.
- Hand-ran the `oxcaml-trap-recovery*.ll` RUN coverage with the agent LLVM
  tools, including the new `-stop-after=branch-folder` check for the multiple
  recovery-entry case: all passed.
- Reran the compiler-facing focused tests with the agent wrapper:
  `make llvm-test-one-no-rebuild DIR=llvm-codegen` passed with 92 passed,
  2 skipped, 0 failed, 94 considered.
- Added the `-stop-after=branch-folder` RUN line to
  `oxcaml-trap-recovery-invoke-multiple.ll` so the tail-merge regression is
  covered by the checked-in LLVM test, not only by a manual command.
- Reran the focused regression checks after adding the RUN line:
  `finalize-isel` FileCheck passed, `branch-folder` FileCheck passed, and the
  `callbr` negative test still rejected with the expected diagnostic.
- Reran `make llvm-test-one-no-rebuild DIR=llvm-codegen` with the agent wrapper
  after the test update: 92 passed, 2 skipped, 0 failed, 94 considered.

2026-05-28 active-publish hardening update:

- Tightened LLVM recovery classification so a recovery entry is no longer
  accepted just because a matching `trap.publish` exists somewhere in the
  function. The unique publish naming a recovery entry must dominate at least
  one invoke edge to that entry, and each invoke only classifies as a trap
  recovery invoke when that publish dominates the invoke.
- Added `oxcaml-trap-recovery-invoke-nondominating-publish.ll`, where one path
  reaches the protected invoke without executing the publish. This now rejects
  with `trap recovery intrinsic must be in a runtime-entered ABI block`.
- This does not add a full IR-level pop marker. It closes the stale
  publish-anywhere hole and proves the publish is on all paths to a classified
  invoke; the complete active trap stack is still represented by OxCaml's
  emitted invoke unwind targets plus the runtime trap-chain stores.
- Rebuilt the agent LLVM tools:
  `ninja -C /tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build llc opt FileCheck clang`.
- Reran the hand-expanded `oxcaml-trap-recovery*.ll` RUN coverage, including
  the new nondominating-publish negative test: all passed.
- Reran `make llvm-test-one-no-rebuild DIR=llvm-codegen` with the agent wrapper:
  92 passed, 2 skipped, 0 failed, 94 considered.
- `git diff --check` passed.
- Follow-up cleanup: replaced the first dominance implementation, which
  hand-walked CFG reachability, with LLVM `DominatorTree`. This keeps the
  active-publish proof aligned with LLVM's own dominance semantics instead of
  maintaining a second local dominance algorithm.
- Rebuilt LLVM tools after the cleanup and reran the same focused validation:
  hand-expanded `oxcaml-trap-recovery*.ll` coverage passed, and
  `make llvm-test-one-no-rebuild DIR=llvm-codegen` again passed with 92 passed,
  2 skipped, 0 failed, 94 considered.
- Follow-up production cleanup: threaded SelectionDAG's existing
  `DominatorTreeWrapperPass` into `FunctionLoweringInfo` and passed that
  analysis into the OxCaml trap recognizer. The publish-dominates-invoke rule
  no longer rebuilds a fresh dominator tree inside helper queries.
- Rebuilt LLVM tools after threading the analysis:
  `ninja -C /tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build llc opt FileCheck clang`.
- Reran the hand-expanded `oxcaml-trap-recovery*.ll` RUN coverage with the
  agent LLVM tools, including positive MIR/asm checks, branch-folder coverage,
  Linux triple smoke checks, and the negative malformed-shape tests: all
  passed.
- Reran `make llvm-test-one-no-rebuild DIR=llvm-codegen` with the agent
  wrapper: 92 passed, 2 skipped, 0 failed, 94 considered.

2026-05-28 runtime-entered verifier boundary cleanup:

- Moved the `runtime-entered` live-in rule out of hard-coded generic
  MachineVerifier AArch64 register-name checks. Generic verifier now asks
  `TargetRegisterInfo` whether each runtime-entered live-in is valid and which
  live-ins are required.
- Added AArch64 `TargetRegisterInfo` hooks for OxCaml runtime-entered recovery
  blocks: exactly `x0`, `x26`, `x27`, and `x28` are valid/required for
  `gc "ocaml"` / `gc "oxcaml"` functions.
- Updated the AArch64 trap intrinsic comments to remove stale callbr/spike
  wording and describe the current invoke-edge model.
- Added `oxcaml-runtime-entry-invalid-liveins.mir` so an extra live-in such as
  `x1` on a runtime-entered block is rejected through the target hook.
- Rebuilt LLVM tools after the public codegen interface change:
  `ninja -C /tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build llc opt FileCheck clang`.
- Validated the new MIR negative test and the existing
  `oxcaml-runtime-entry-multiple-calls.mir` runtime-entry pass test.
- Reran the hand-expanded `oxcaml-trap-recovery*.ll` RUN coverage with the
  rebuilt LLVM tools: all passed.
- Reran `make llvm-test-one-no-rebuild DIR=llvm-codegen` with the agent
  wrapper: 92 passed, 2 skipped, 0 failed, 94 considered.

2026-05-28 AArch64 wrap_try cleanup:

- Found that real AArch64 OxCaml lowering no longer calls `wrap_try`, but
  `define_auxiliary_functions` still emitted the private helper into AArch64 IR
  and assembly. This was stale model leakage rather than a hot-path call, but
  the native-like path should not emit the unused helper at all.
- Changed `backend/llvm/llvmize.ml` so `define_wrap_try` is skipped on
  AArch64 and still emitted for the non-AArch64 path that uses the old
  returns-twice model.
- Added a source-level regression check to
  `testsuite/tests/llvm-codegen/trap_recovery_runtime.sh` that AArch64 trap
  recovery emits `trap.publish` / `trap.recover` but not the `wrap_try` symbol.
- Rebuilt the compiler with `make install`.
- Manual source probe with a real raising call in a `try` showed:
  `IR_wrap_try_symbol=0`, `ASM_wrap_try_label=0`, `IR_trap_publish=2`,
  `IR_trap_recover=2`.
- Reran `make llvm-test-one-no-rebuild TEST=llvm-codegen/trap_recovery_runtime.ml`
  with the agent wrapper: 4 passed, 0 failed.
- Reran `make llvm-test-one-no-rebuild DIR=llvm-codegen` with the agent
  wrapper: 92 passed, 2 skipped, 0 failed, 94 considered.

2026-05-28 split unwind recovery fix:

- A broad `make llvm-test-no-rebuild` run exposed backend crashes in
  `basic-io/wc.ml`, `lib-scanf/tscanf.ml`, and `lib-seq/test.ml`:
  `trap recovery intrinsic must be in a runtime-entered ABI block`.
- Reduced the first failure to generated `wc.ll`. The source lowering was
  valid: `trap.publish` dominated the protected invokes and named the final
  `trap.recover` block. The problem was LLVM `-O3` splitting unwind edges into
  token `landingpad` trampoline blocks plus branch-only join blocks before the
  final recover block.
- Fixed the LLVM recognizer to follow a branch-only chain from a token
  landingpad trampoline to the published `trap.recover` continuation, instead
  of only accepting direct landingpad+recover or one-hop landingpad shapes.
- Added `oxcaml-trap-recovery-split-unwind.ll` to block this multi-hop split
  unwind shape.
- Rebuilt LLVM tools:
  `ninja -C /tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build llc opt FileCheck clang`.
- Validated:
  - New split-unwind LLVM test passes.
  - The exact crashing `wc.ll` now compiles with the same `clang -O3` flags
    used by the wrapper.
  - Targeted source tests pass:
    `basic-io/wc.ml`, `lib-scanf/tscanf.ml`, and `lib-seq/test.ml`.
  - Hand-expanded positive and negative `oxcaml-trap-recovery*.ll` coverage
    passes.
  - `make llvm-test-one-no-rebuild DIR=llvm-codegen`: 92 passed, 2 skipped,
    0 failed, 94 considered.
  - `make llvm-test-no-rebuild`: 6743 passed, 295 skipped, 0 failed,
    7038 considered.

2026-05-28 full self-stage2 validation:

- Ran:
  `make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` after setting the
  `oxcaml-5.4.0+oxcaml` opam switch and the agent-local LLVM temp
  environment.
- Stage1 compiler build completed with fresh wrapper activity:
  boot wrapper lines 1678 / fresh IR 834, runtime wrapper lines 148 /
  fresh IR 74, main wrapper lines 2224 / fresh IR 1098, self-stage smoke
  fresh IR 2.
- Stage2 compiler build from `_llvm_self_stage_install` completed with fresh
  wrapper activity: boot wrapper lines 1678 / fresh IR 831, runtime wrapper
  lines 148 / fresh IR 74, main wrapper lines 2224 / fresh IR 1099, stage2
  self-stage smoke fresh IR 2.
- Stage2 testsuite passed:
  6717 tests passed, 282 skipped, 0 failed, 0 not started, 0 unexpected
  errors, 6999 tests considered.
- Stage2 testsuite wrapper activity was nonzero:
  wrapper lines 6699, fresh IR 3341.

2026-05-28 post-dominance-cleanup broad validation:

- Rebuilt LLVM tools after changing trap recovery EH-pad detection to use the
  dominance-backed publish/recover predicate.
- Reran focused LLVM trap recovery coverage and `llvm-codegen`; all passed.
- First broad `make llvm-test-no-rebuild` run had one failure in
  `tests/ast-invariants/test.ml`: the test recursively walked stale generated
  `_ocamltest` directories under `_runtest` and hit a partially removed
  `tests/codegen/allocation` path. This was not a compiler failure.
- Removed generated `_ocamltest` work directories and reran the same broad
  suite:
  6743 tests passed, 295 skipped, 0 failed, 0 not started, 0 unexpected errors,
  7038 tests considered.
- Ran a fresh `make llvm-self-stage2-test LLVM_PATH="$LLVM_PATH"` after the
  dominance cleanup.
- Stage1 LLVM self-stage wrapper activity: boot fresh IR 830, smoke fresh IR 2,
  runtime fresh IR 73, main fresh IR 1102, self-stage smoke fresh IR 2.
- Stage2 compiler build wrapper activity: boot fresh IR 835, smoke fresh IR 2,
  runtime fresh IR 74, main fresh IR 1105, self-stage smoke fresh IR 2.
- Stage2 testsuite passed:
  6717 tests passed, 282 skipped, 0 failed, 0 not started, 0 unexpected errors,
  6999 tests considered.
- Stage2 testsuite wrapper activity was nonzero:
  wrapper lines 6701, fresh IR 3339.
