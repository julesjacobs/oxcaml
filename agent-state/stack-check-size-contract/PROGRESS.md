# Progress

Last updated: 2026-05-24.

## Current Claim

Goal defined. The first implementation task is to add and test an explicit
CFG-required stack-check byte-count contract from `llvmize.ml` to AArch64 LLVM
frame lowering.

## Evidence

- OxCaml branch: `jujacobs/stack-check-size-contract`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/11
- Initial state commit: `106d76c3c37f`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/stack-check-size-contract`

## Current Blocker

None.

## Next Step

Run `eval "$(../../../scripts/agent-tmp-env)"`, inspect the current LLVM stack
check attributes in `backend/llvm/llvmize.ml` and
`vendor/llvm-project/llvm/lib/Target/AArch64/AArch64FrameLowering.cpp`, then
add the numeric byte-count attribute with focused tests.
