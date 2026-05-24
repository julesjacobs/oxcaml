# Progress

Last updated: 2026-05-24.

## Current Claim

Fresh agent workspace created for LLVM-backend register liveness across
allocations and poll points. Goal scope is defined.

## Evidence

- OxCaml branch: `jujacobs/regs-over-alloc-poll`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/2
- Initial PR commit: `137cd04341ef`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Local checkout: `agents/regs-over-alloc-poll/oxcaml`
- Agent state path: `agent-state/regs-over-alloc-poll`

## Current Blocker

No blocker recorded.

## Next Step

Identify the current LLVM-backend safepoint/register handling path, then build a
small reproducer or test that distinguishes stack-spilled values from values kept
in registers across allocation and poll-point safepoints.
