# Progress

Last updated: 2026-05-25.

## Current Claim

Agent workspace is initialized with the stack-check size contract baseline and
a design guide for native-style slack stack checks.

## Evidence

- OxCaml branch: `jujacobs/llvm-stack-check-slack`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/15
- Initial state commit: `2d58d583e085`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-stack-check-slack`
- Design guide: `agent-state/llvm-stack-check-slack/DESIGN.md`
- Baseline merged: `jujacobs/stack-check-size-contract`

## Current Blocker

None.

## Next Step

Start from `DESIGN.md`, extend the focused stack-check contract tests, then
make the smallest safe prologue-check predicate split.
