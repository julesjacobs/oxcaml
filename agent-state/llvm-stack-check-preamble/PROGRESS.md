# Progress

Last updated: 2026-05-24.

## Current Claim

Fresh agent workspace created for improving the LLVM backend stack-check
preamble sequence. The goal is limited to the local assembly sequence; CFG
sinking is out of scope.

## Evidence

- OxCaml branch: `jujacobs/llvm-stack-check-preamble`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/3
- Initial PR commit: `87e729dc7ac2`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-stack-check-preamble`
- Goal recorded in `agent-state/llvm-stack-check-preamble/GOAL.md`.

## Current Blocker

None.

## Next Step

Find the current LLVM-backend stack-check sequence and compare it against the
native-backend sequence using a small assembly-level reproducer.
