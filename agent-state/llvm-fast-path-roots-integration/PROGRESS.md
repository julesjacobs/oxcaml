# Progress

Last updated: 2026-05-25.

## Current Claim

The fast-path root-slot optimization has been ported onto
`jujacobs/llvm-backend-integration` while preserving the integration branch's
stack-check slack machinery.

For eligible heap allocation and poll safepoints, the LLVM backend now keeps
roots in ordinary SSA/register values on the fast path and writes static GC root
slots only in the cold `caml_call_gc` slow path. Safepoints with unwind edges,
terminator calls, or active trap handlers stay on the existing conservative
root handling.

## Evidence

- OxCaml branch: `jujacobs/llvm-fast-path-roots-integration`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/18
- Initial state commit: `af33d12129d5`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-fast-path-roots-integration`
- Built a branch-local LLVM clang from the checked-out vendored LLVM sources:
  `/tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build/bin/clang`
- Built and installed OxCaml with:
  `OCAMLPARAM="_,llvm-path=$LLVM_PATH" make install`
- Focused tests run with the branch-local clang wrapper:
  - `make test-one-no-rebuild TEST=llvm-codegen/fast_path_roots.ml` passed
  - `make test-one-no-rebuild TEST=llvm-codegen/allocation.ml` passed after
    promoting the intended fast-path-root code shape
  - `make test-one-no-rebuild TEST=llvm-codegen/poll_statepoint.ml` passed
  - `make test-one-no-rebuild TEST=llvm-codegen/allocation_frametable.ml`
    passed
  - `make test-one-no-rebuild TEST=llvm-codegen/effect_preemption.ml` skipped
    the poll-insertion action in this local predicate set
- Added an `expectnat` anchor for `_caml_llvm_eh_personality`; without it,
  native toplevel fragments using LLVM EH failed at `dlopen` with a missing
  personality symbol.

## Current Blocker

No current blocker.

## Next Step

Review the diff, then commit and push the integration branch.
