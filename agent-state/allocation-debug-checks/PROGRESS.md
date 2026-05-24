# Progress

Last updated: 2026-05-24.

## Current Claim

Active goal revised: find high-leverage inefficiencies in `-llvm-backend`
assembly compared with native backend assembly.

## Current Scope

- OxCaml branch: `jujacobs/allocation-debug-checks`
- Agent state path: `agent-state/allocation-debug-checks`
- Active goal recorded in `agent-state/allocation-debug-checks/GOAL.md`
- Compare normal native backend assembly with normal `-llvm-backend` assembly.
- Start with compact source snippets and static assembly inspection.
- Use focused timing or counting only after finding a concrete candidate
  inefficiency.

## Completed History

- Previous cleanup PR: https://github.com/julesjacobs/oxcaml/pull/7
- The previous goal removed temporary allocation debug-helper calls from normal
  LLVM backend codegen and removed the dead runtime helper declarations and
  definitions.
- That previous PR was merged.

## Next Step

Run `eval "$(../../../scripts/agent-tmp-env)"`, confirm the installed compiler
and patched clang wrapper, then build the first compact native-vs-LLVM assembly
comparison set.
