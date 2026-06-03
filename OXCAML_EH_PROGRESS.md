# OxCaml EH / GC Relocate Progress

Goal: make OxCaml LLVM lowering correctly model GC-live values across
`pushtrap` exceptional control transfer, including shared handlers.

## Current Model

For OxCaml traps, a landingpad-token `gc.relocate` in a shared handler should
mean an edge-dependent relocate over the predecessor statepoints:

```llvm
handler:
  %lp = landingpad token cleanup
  %r = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 N, i32 M)
```

Expected meaning:

```text
%r = merge [ relocate(%token.a, N, M), %call_a ],
           [ relocate(%token.b, N, M), %call_b ]
```

Each predecessor statepoint must have a compatible `gc-live` bundle layout for
the relocate indices used in the shared handler.

Non-GC values assigned before each invoke already fit existing LLVM/MachineIR:
use an ordinary PHI in the landingpad block. GC values need the same PHI-like
merge, but through `gc.relocate`.

## LLVM IR Tests Added

Passing/control tests:

- `vendor/llvm-project/llvm/test/CodeGen/AArch64/oxcaml-landingpad-gc-relocate-one-invoke.ll`
  - One invoke with a unique landingpad.
  - Covers handler-live, normal-live, and normal+handler-live GC values.
  - Tightened to use `oxcaml_nofpcc` for the test functions, callee, and
    statepoint invokes.
- `vendor/llvm-project/llvm/test/CodeGen/AArch64/oxcaml-shared-landingpad-gc-relocate-normal-only.ll`
  - Multiple invokes share a handler.
  - GC values are live only on normal continuations.
  - Shows shared landingpads themselves are not the problem.
  - Tightened to use `oxcaml_nofpcc`; branch selector arguments use `i64` plus
    `icmp` rather than `i1`, matching the OxCaml ABI shape.

Shared-handler Machine PHI test:

- `vendor/llvm-project/llvm/test/CodeGen/AArch64/oxcaml-shared-landingpad-gc-relocate.ll`
  - Multiple invokes share a landingpad with handler-side landingpad-token
    `gc.relocate`.
  - Tightened to use `oxcaml_nofpcc` for the test functions, callees, and
    statepoint invokes; alternative-branch selector arguments use `i64` plus
    `icmp`.
  - Covers:
    - alternative invokes, handler-live GC value;
    - alternative invokes, normal+handler-live GC value;
    - sequential invokes, handler-live GC value;
    - sequential invokes, normal+handler-live GC value;
    - two handler-live GC values;
    - distinct normal-live and handler-live GC values;
    - GC value plus ordinary `i64` live in normal and handler paths;
    - sequential versions of the multi-value cases.
  - Current result: `llc -verify-machineinstrs` passes for non-derived shared
    handler relocates, and the `-stop-after=finalize-isel` check requires a
    Machine PHI in the shared handler for representative alternative and
    sequential invoke shapes.

Current guardrail/future-slice test:

- `vendor/llvm-project/llvm/test/CodeGen/AArch64/oxcaml-shared-landingpad-gc-relocate-derived.ll`
  - Multiple invokes share a landingpad with a handler-side landingpad-token
    `gc.relocate` whose derived index differs from its base index.
  - Current result: verifier accepts the IR, but codegen still rejects it with
    `OxCaml statepoint contains a derived GC pointer`. This preserves the
    existing OxCaml stackmap/RS4GC constraint until derived pointers are
    rematerialized before stackmap lowering.

## Manual IR Evidence

- `agent-state/test-suite-29e4cd/rs4gc_inspect_try_shared.no_exnroot.ll`
  - One-call try-body rewrite that replaces the exception-root alloca/load with
    a landingpad-token `gc.relocate`.
  - Custom clang accepts and compiles it.
- `agent-state/test-suite-29e4cd/rs4gc_inspect_try_two_calls.no_exnroot.ll`
  - Two-call try-body experiment.
  - Splitting landingpads fights OxCaml trap recovery rules:
    `trap.recover` must be in a runtime-entered ABI block and first in its
    recovery block.
  - This points back to shared-handler landingpad-token `gc.relocate` as the
    intended source IR shape.

## MachineIR Prototype

Prototype files:

- `agent-state/test-suite-29e4cd/mir-prototypes/baseline_shared_landingpad.ll`
- `agent-state/test-suite-29e4cd/mir-prototypes/landingpad_phi_prototype.mir`
- `agent-state/test-suite-29e4cd/mir-prototypes/landingpad_phi_prototype.after-phi.mir`
- `agent-state/test-suite-29e4cd/mir-prototypes/landingpad_phi_prototype.s`
- `agent-state/test-suite-29e4cd/mir-prototypes/landingpad_phi_prototype.O0.s`

Result:

- A landingpad-block MachineSSA PHI is accepted by LLVM MIR parsing and
  verification.
- PHI elimination lowers it to one merged virtual register, with copies in each
  predecessor before the call:

```text
bb.call_a:
  %merged = COPY %value.a
  BL @callee_a

bb.call_b:
  %merged = COPY %value.b
  BL @callee_b

bb.handler:
  %handler_value = COPY %merged
```

- The default post-isel pipeline assigns the merged value to one callee-saved
  register (`x19` in the prototype), which gives the handler a consistent
  location independent of which call unwound.
- Caveat: the `-O0` / fast-regalloc result for the generic EH prototype is not
  directly the OxCaml statepoint path. For OxCaml, allocatable value registers
  are caller-saved across runtime entry. Values live over a statepoint are
  already forced through spill slots, and `FixupStatepointCallerSaved` has logic
  to reload spilled statepoint registers in the EHPad. That makes the desired
  handler merge naturally stack-slot based rather than callee-saved-register
  based. The generic EH prototype still demonstrates that a landingpad Machine
  PHI is accepted and PHI elimination can create one merged value.

## LLVM Assumptions Changed

- `lib/IR/Verifier.cpp`
  - The verifier used to require safepoint invokes to have unique landingpads.
    It now permits the shared landingpad-token `gc.relocate` shape for OxCaml
    when each relevant predecessor statepoint has compatible relocate indices
    and pointer types.
- `lib/IR/IntrinsicInst.cpp`
  - `GCProjectionInst::getStatepoint()` recovers one statepoint from the
    landingpad's unique predecessor. Shared landingpad lowering must avoid that
    helper when it needs predecessor-specific base/derived values.
- `lib/CodeGen/SelectionDAG/StatepointLowering.cpp`
  - Statepoint lowering still records relocates per predecessor statepoint, but
    shared landingpad relocates are interpreted relative to the statepoint
    currently being lowered.
  - `visitGCRelocate` now materializes non-derived shared landingpad relocates
    through a Machine PHI instead of asking the relocate for one owning
    statepoint.

## Design Direction

Implement OxCaml-specific support for shared landingpad-token `gc.relocate`:

- verifier: allow the shared landingpad shape for OxCaml when every predecessor
  relevant to a handler relocate is a compatible statepoint invoke;
- lowering: do not ask a shared landingpad relocate for a single statepoint;
- MachineIR: lower each shared handler relocate to a PHI-like machine value over
  the predecessor statepoint relocations;
- codegen: ensure the merged value has a consistent handler-visible location
  across all exceptional entries, while letting normal register allocation and
  spill-slot placement choose the concrete storage;
- validation: first make the red LLVM IR tests pass, then add/update OxCaml
  source expect tests, then run broader `-llvm-backend` and self-stage2 tests.

## Required LLVM Changes

Implemented slices:

- Fixed verifier-only milestone: `opt -passes=verify` now accepts the red
  OxCaml shared-handler IR.
- Non-OxCaml shared landingpad relocates remain rejected with
  `safepoints should have unique landingpads`.
- New verifier-negative test:
  `vendor/llvm-project/llvm/test/Verifier/oxcaml-shared-landingpad-gc-relocate.ll`.
  It checks non-OxCaml rejection, per-predecessor index bounds, address-space
  compatibility, and pointer type compatibility.
- SelectionDAG statepoint lowering now interprets a shared landingpad relocate
  relative to the predecessor statepoint currently being lowered.
- SelectionDAG now materializes each non-derived shared landingpad
  `gc.relocate` as a Machine PHI in the landingpad block. Incoming values are
  recorded through the existing `PHINodesToUpdate` edge-update path, so the
  later PHI-elimination and register-allocation pipeline decides where the
  value lives.
- `AsmWriter` now prints shared landingpad `gc.relocate` comments by indices,
  avoiding the unique-predecessor query that is invalid for this shape.

Remaining next slice:

- derived-pointer shared handler relocates still fail at stackmap lowering.
  The current guardrail test pins that limitation. Fixing it likely needs the
  same edge-dependent Machine PHI idea plus an RS4GC/stackmap path that can
  rematerialize derived pointers before final stackmap emission.

This keeps the optimization target intact: the shared handler value remains a
machine value until register allocation, rather than being forced into a
preselected statepoint spill slot that would recreate the old alloca-root shape.
