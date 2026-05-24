# Progress

Last updated: 2026-05-24.

## Current Claim

Goal defined. The first implementation task is to add and test an explicit
CFG-required stack-check byte-count contract from `llvmize.ml` to AArch64 LLVM
frame lowering. The goal now defines the source of truth precisely: the value is
the maximum `Cfg.Stack_check.max_frame_size_bytes` already present in the CFG,
or `0` if there is no `Cfg.Stack_check`. When stack checks are enabled, the
attribute should be emitted explicitly even for `0`; absence is reserved for
disabled stack checks.

## Evidence

- OxCaml branch: `jujacobs/stack-check-size-contract`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/11
- Initial state commit: `106d76c3c37f`
- Goal clarification commit: `afb853a6361b`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/stack-check-size-contract`

## Current Blocker

None.

## Next Step

Run `eval "$(../../../scripts/agent-tmp-env)"`, inspect the current LLVM stack
check attributes in `backend/llvm/llvmize.ml`, then add the numeric byte-count
attribute with focused tests. Do not invent stack sizes from source shape or
final LLVM assembly; compare the attribute against the CFG `Stack_check` size.
Do not change AArch64 prologue sizing or omission policy in this first PR.
