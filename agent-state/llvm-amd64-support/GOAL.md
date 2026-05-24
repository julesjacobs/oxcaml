# Goal

Implement AMD64/x86-64 support for the OxCaml LLVM backend.

The target outcome is that the LLVM backend can produce correct AMD64 code for
normal explicit `-llvm-backend` use, with the same runtime, GC, frame-table,
calling-convention, stack-check, and debugging contracts that the current
integration branch relies on for ARM64.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Editable areas: OxCaml compiler/runtime/test sources and vendored LLVM
  sources when needed for AMD64 backend correctness.
- Expected output: focused AMD64 implementation patches, minimal regression
  tests or reproducers, and recorded validation evidence.
- Non-goal: broad refactors that are not required for AMD64 support.

## Initial Questions

- Identify where the current LLVM backend is target-specific and ARM64-biased.
- Determine the AMD64 ABI/runtime contracts for domain state, allocation,
  stack checks, poll/safepoint lowering, frame tables, exception handling,
  and DWARF/debug metadata.
- Establish the smallest AMD64 `-llvm-backend` smoke test that should pass
  first, then expand toward testsuite coverage.

## Validation Expectations

- Prefer focused standard-compiler tests with `-llvm-backend` during
  development.
- Use self-stage2 only for full validation or for failures that cannot be
  reproduced with the standard installed compiler.
- Record exact commands, results, and relevant log paths in `PROGRESS.md`.

## Branches

- OxCaml: `jujacobs/llvm-amd64-support`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/10
