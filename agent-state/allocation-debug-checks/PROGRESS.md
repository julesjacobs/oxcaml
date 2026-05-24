# Progress

Last updated: 2026-05-24.

## Current Claim

Fresh agent workspace created for auditing LLVM backend allocation debug checks.
No source investigation has started yet.

## Evidence

- OxCaml branch: `jujacobs/allocation-debug-checks`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/7
- Initial state commit: `a3bf6d3f68ce`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/allocation-debug-checks`
- Goal recorded in `agent-state/allocation-debug-checks/GOAL.md`

## Current Blocker

None.

## Next Step

Run `eval "$(../../../scripts/agent-tmp-env)"`, then find every LLVM backend
emission of `caml_debug_check_minor_heap` and
`caml_debug_check_minor_heap_head` and compare with native allocation lowering.
