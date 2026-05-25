# Goal

Make AArch64 `-llvm-backend` stack checks follow the native-style slack model:
avoid the special LLVM machine-prologue stack check when the LLVM prologue is
small enough and CFG stack checks cover the function.

Use `agent-state/llvm-stack-check-slack/DESIGN.md` as a design guide, not as a
rigid script. Preserve safety first, keep changes staged and testable, and
adjust the design if the code shows a cleaner or safer route.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Expected output: focused OxCaml and vendored LLVM changes with tests proving
  small functions avoid needless prologue checks, CFG-checked functions use real
  ordinary checks before prologue elision, large prologue frames remain safe,
  `-no-cfg-stack-checks` stays conservative, and `-no-stack-checks` stays exact.

## Branches

- OxCaml: `jujacobs/llvm-stack-check-slack`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/15

## Baseline

This workspace includes the `jujacobs/stack-check-size-contract` work because
the design guide assumes the `"oxcaml-stack-check-bytes"` contract exists.
