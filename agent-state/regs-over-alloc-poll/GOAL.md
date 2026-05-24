# Goal

Allow the LLVM backend to keep selected OCaml values in registers across
allocations and poll points, matching the native backend model where practical.

Full callee-saves support is not part of this goal.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- This agent may edit both OxCaml compiler sources and vendored LLVM sources
  when needed for this backend feature.
- Expected output: a focused implementation and tests or reproducers showing
  LLVM-backend values can remain in registers across allocation and poll-point
  safepoints without relying on full callee-saves.

## Non-goals

- Do not implement general full callee-saves for the LLVM backend.
- Do not make broad native-backend changes except for comparison or tests.
- Do not update the vendored LLVM baseline unless the feature requires it and
  the baseline metadata is kept accurate.

## Branches

- OxCaml: `jujacobs/regs-over-alloc-poll`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/2
