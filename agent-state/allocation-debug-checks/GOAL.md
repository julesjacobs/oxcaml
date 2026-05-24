# Goal

Find high-leverage inefficiencies in `-llvm-backend` assembly compared with
the native backend.

The LLVM backend is now past the obvious allocation debug-helper cleanup. The
next goal is to compare generated assembly from normal `-llvm-backend` builds
against native backend assembly and identify inefficiencies that are worth
fixing because they affect common code shapes, allocation fast paths, call
sequences, register use, or other hot generated-code patterns.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Baseline: native backend assembly from the same checkout and compiler
  configuration.
- Comparison target: `-llvm-backend` assembly produced with the installed
  compiler and the agent-local patched clang wrapper.

Expected output:

- Build a compact set of representative comparison cases instead of starting
  with broad benchmarks.
- Compare native and `-llvm-backend` assembly for those cases.
- Identify recurring inefficiency patterns, not one-off formatting or register
  naming differences.
- Prioritize candidates by expected payoff and likely fix location.
- Record the strongest findings in `PROGRESS.md`, with source snippets,
  native assembly shape, LLVM assembly shape, and a proposed fix direction.
- If one finding has a small, clear fix, implement it and add a focused
  expected-output test. Otherwise, leave a ranked investigation plan.

## Edit Policy

- May edit OxCaml compiler sources and tests when a focused LLVM backend fix is
  clear.
- May edit vendored LLVM sources only when the investigation shows the
  inefficiency is caused by LLVM lowering or codegen rather than OxCaml LLVM IR.
  Record the reason in `agent-state/allocation-debug-checks/PROGRESS.md` before
  changing vendored LLVM.
- Keep comparison helpers, temporary snippets, and assembly dumps out of
  committed source unless they become focused tests.

## First Steps

1. Run `eval "$(../../../scripts/agent-tmp-env)"` from the agent checkout before
   LLVM-backend work.
2. Confirm the installed compiler and patched clang wrapper being used for
   `-llvm-backend` assembly generation.
3. Pick a small set of high-signal snippets covering common generated-code
   shapes:
   - small allocations and allocation checks;
   - direct calls, tail calls, and indirect calls;
   - integer arithmetic and comparisons;
   - closure allocation/application;
   - exception raise paths;
   - loops with simple memory access.
4. For each snippet, compile both native and `-llvm-backend` assembly from the
   same source and optimization level.
5. Classify differences into likely causes:
   - OxCaml LLVM IR shape;
   - LLVM backend lowering;
   - calling convention or frame layout;
   - register allocation;
   - unavoidable backend difference.
6. Start with static assembly differences. Only run focused timing or counting
   checks after there is a concrete candidate inefficiency to validate.

## Non-goals

- Do not start with broad benchmarking.
- Do not treat every native/LLVM assembly difference as a bug.
- Do not optimize for cosmetic assembly differences.
- Do not use historical `stage4` or `stage5` script names as conceptual
  validation stages.

## Branches

- OxCaml: `jujacobs/allocation-debug-checks`

## Pull Requests

- Previous completed cleanup PR: https://github.com/julesjacobs/oxcaml/pull/7
