# Progress

Last updated: 2026-05-27.

## Current state

Branch: `jujacobs/llvm-fast-path-roots-integration`

PR: https://github.com/julesjacobs/oxcaml/pull/18

Current focus: AArch64 LLVM `try`/trap performance. The fast-path-roots
integration work and earlier compiler-benchmark work are background context; the
active design question is how to remove the hot `_wrap_try` call without hiding
the trap recovery edge from LLVM.

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
