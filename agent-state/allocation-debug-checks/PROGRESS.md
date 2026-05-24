# Progress

Last updated: 2026-05-24.

## Current Claim

Found and fixed one high-leverage `-llvm-backend` assembly inefficiency:
AArch64 LLVM prologue stack checks were requested for every function instead of
only functions where the CFG stack-check pass inserted `Stack_check`.

## Current Scope

- OxCaml branch: `jujacobs/allocation-debug-checks`
- Agent state path: `agent-state/allocation-debug-checks`
- Active goal recorded in `agent-state/allocation-debug-checks/GOAL.md`
- Compare normal native backend assembly with normal `-llvm-backend` assembly.
- Start with compact source snippets and static assembly inspection.
- Use focused timing or counting only after finding a concrete candidate
  inefficiency.

## Completed History

- Previous cleanup PR: https://github.com/julesjacobs/oxcaml/pull/7
- The previous goal removed temporary allocation debug-helper calls from normal
  LLVM backend codegen and removed the dead runtime helper declarations and
  definitions.
- That previous PR was merged.

## Assembly Comparison

- Toolchain:
  - Ran `eval "$(../../../scripts/agent-tmp-env)"`.
  - Used the installed compiler from `make install`:
    `./_install/bin/ocamlopt.opt`.
  - Used the agent-local patched clang wrapper pointing at
    `/tmp/oxcaml-agent-allocation-debug-checks/llvm-build-1fc/bin/clang`.
- Compact snippets covered:
  - small heap allocation;
  - integer arithmetic;
  - tail-recursive loop;
  - indirect non-tail call;
  - closure allocation.
- Strongest fixed finding:
  - Before the fix, allocation-only LLVM functions such as `alloc_pair` had
    `"oxcaml-stack-check"="true"` and emitted the AArch64
    `_caml_plat_pagesize` / `_caml_llvm_prologue_realloc_stack` prologue
    sequence.
  - Native backend assembly for the same allocation-only shape did not emit a
    stack check.
  - Cause: `backend/llvm/llvmize.ml` attached `Oxcaml_stack_check` to every
    AArch64 function, while the native backend only emits stack checks when the
    stack-check pass inserted `Stack_check`.
  - Fix: attach `Oxcaml_stack_check` only when the CFG contains a
    `Stack_check` instruction.
- Verification evidence for the fixed finding:
  - After `make install`, the installed-compiler comparison showed the
    allocation-only `alloc_pair` LLVM IR has no `oxcaml-stack-check` attribute.
  - The corresponding LLVM assembly has no `_caml_plat_pagesize` load and no
    `_caml_llvm_prologue_realloc_stack` call.
  - The non-tail indirect-call snippet still has the attribute and prologue
    stack-check sequence, matching the native backend's need for a stack check
    around non-tail calls.
- Remaining high-leverage candidate:
  - Allocation fast paths still spill live allocation inputs to stack slots
    because preserved GC-root slots are volatile across allocation safepoints.
  - Example shape: LLVM `alloc_pair` stores arguments to `[sp]` before the
    heap-limit check and reloads them on the fast path; native keeps the same
    values in registers and only calls GC on the slow path.
  - Likely fix area: reduce or split preserved root-slot use around allocation
    safepoints so the fast path does not pay for slow-path root materialization.
    This is larger than the stack-check attribute fix.

## Implementation

- Updated `backend/llvm/llvmize.ml`.
- Added `testsuite/tests/llvm-codegen/stack_check_attr.ml` and
  `testsuite/tests/llvm-codegen/stack_check_attr.sh`.
- The focused test proves both sides:
  - allocation-only function: no `oxcaml-stack-check` attribute and no LLVM
    prologue stack-check assembly;
  - non-tail-call function: still has `oxcaml-stack-check` and the LLVM
    prologue stack-check assembly.

## Verification

- `make -s boot-compiler` passed.
- `make -s install` passed.
- `OCAMLPARAM="_,llvm-path=$LLVM_PATH" make promote-one-no-rebuild TEST=llvm-codegen/allocation.ml`
  passed.
- `OCAMLPARAM="_,llvm-path=$LLVM_PATH" make promote-one-no-rebuild TEST=llvm-codegen/stack_check_attr.ml`
  passed.

## Next Step

Review the diff, then commit and push the focused stack-check attribute fix and
test. Continue with the preserved-root allocation fast-path spill candidate
after this patch is reviewed or merged.
