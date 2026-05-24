# Goal

Audit the LLVM backend allocation debug checks and make the normal LLVM backend
allocation path match production expectations.

The current LLVM backend appears to emit calls such as
`caml_debug_check_minor_heap` and `caml_debug_check_minor_heap_head` on heap
allocation paths. Determine whether these checks are intentionally supported
debugging functionality or leftover temporary instrumentation.

Normal `-llvm-backend` generated code should not call debug heap-check helpers
unless an explicit debug option requests it.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Expected output:
  - Find every place the LLVM backend emits allocation debug helper calls.
  - Compare the LLVM backend behavior against native backend allocation
    lowering and runtime debug-checking conventions.
  - Decide whether the right fix is to remove the calls from normal codegen or
    gate them behind an explicit compiler/debug flag.
  - If a flag is kept, document the flag and add focused tests proving both
    default and enabled behavior.
  - Update existing LLVM codegen expected-output tests accordingly.

## Edit Policy

- May edit OxCaml compiler sources and tests needed for the LLVM backend
  allocation lowering.
- Do not edit vendored LLVM sources unless the investigation shows the helper
  emission is caused by vendored LLVM code. If that happens, record the reason
  in `agent-state/allocation-debug-checks/PROGRESS.md` before changing vendored
  LLVM.

## First Steps

1. Run `eval "$(../../../scripts/agent-tmp-env)"` from the agent checkout before
   LLVM-backend work.
2. Search for every LLVM backend emission of `caml_debug_check_minor_heap` and
   `caml_debug_check_minor_heap_head`, including expected-output tests.
3. Compare the found LLVM allocation path with native backend allocation
   lowering and runtime debug-checking conventions.
4. Write down the intended debug story in `PROGRESS.md` before making the code
   change.

## Non-goals

- Do not start with broad benchmarking.
- Do not use historical `stage4` or `stage5` script names as conceptual
  validation stages.

## Branches

- OxCaml: `jujacobs/allocation-debug-checks`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/7
