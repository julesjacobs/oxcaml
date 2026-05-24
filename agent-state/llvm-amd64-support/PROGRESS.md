# Progress

Last updated: 2026-05-24.

## Current Claim

Fresh agent workspace created for AMD64/x86-64 LLVM backend support. No
implementation claim yet.

## Evidence

- OxCaml branch: `jujacobs/llvm-amd64-support`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/10
- Initial state commit: `7965927f6af4`
- Agent PR-record commit: `25e06ddb58`
- Base integration branch: `jujacobs/llvm-backend-integration`
- Base integration commit at worktree creation: `95443659432c`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-amd64-support`
- Makefile workflow audit recorded in `GOAL.md`. Main finding:
  `Makefile.common-ox` provides `llvm-*` targets that set `LLVM_BACKEND=1`,
  use `LLVM_PATH`, require frame pointers, and support explicit `ARCH=amd64`.
- Full-validation scripts:
  `tools/build-llvm-self-stage-install.sh`,
  `tools/build-llvm-stage5-install.sh`, and
  `tools/run-llvm-stage5-ocamltest.sh`. They support `DUNE_BUILD_FLAGS`,
  `LLVM_TESTSUITE_PARALLEL`, `LLVM_TESTSUITE_JOBS`, `LIST`, and
  `EXCLUDE_REGEX`.

## Current Blocker

No blocker. The next agent should start by mapping existing LLVM backend
target assumptions and creating the smallest AMD64 `-llvm-backend` smoke test
or reproducer.

## Next Step

Run per-agent environment setup with `eval "$(../../../scripts/agent-tmp-env)"`
from `agents/llvm-amd64-support/oxcaml`, inspect the target-specific LLVM
backend/runtime paths, and identify the first AMD64 failure to make concrete.
A likely first command shape is:
`DUNE_BUILD_FLAGS=-j1 ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-install`,
followed by a focused `llvm-test-one`, for example
`ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-test-one TEST=llvm-codegen/arithmetic`.
