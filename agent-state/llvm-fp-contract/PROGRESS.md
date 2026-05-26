# Progress

Last updated: 2026-05-26.

## Current Claim

The FP contraction goal has been incorporated into
`jujacobs/llvm-fast-path-roots-integration`.

LLVM IR now marks only the native-selected scalar floating-point
multiply-add/subtract family as `contract`:

- `Imuladdf`
- `Imulsubf`
- `Inegmuladdf`
- `Inegmulsubf`

Ordinary floating-point `Iaddf`, `Isubf`, and `Imulf` remain strict. This keeps
`Sys.opaque_identity` barriers effective, because the contract flag is attached
only after native arm64 selection has already recognized one of the fused
specific operations.

## Evidence

- OxCaml branch: `jujacobs/llvm-fp-contract`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/17
- Initial state commit: `9805def1dbff`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-fp-contract`
- Goal file includes the motivating `float_ref_loop` example and the narrow semantics requirements.
- Merged into integration branch:
  `jujacobs/llvm-fast-path-roots-integration`
- Implementation:
  - `backend/llvm/llvm_ir.ml`
  - `backend/llvm/llvm_ir.mli`
  - `backend/llvm/llvmize.ml`
- Focused test:
  - `testsuite/tests/llvm-codegen/fp_contract.ml`
  - `testsuite/tests/llvm-codegen/fp_contract.sh`
- Validation:
  - `make llvm-test-one TEST=llvm-codegen/fp_contract.ml LLVM_PATH="$LLVM_PATH"`
    passed: 4 passed, 0 failed.
  - `make llvm-test-one-no-rebuild DIR=llvm-codegen LLVM_PATH="$LLVM_PATH"`
    passed: 70 passed, 2 skipped, 0 failed.

## Current Blocker

None.

## Next Step

Optional: rerun the full global `make llvm-test` after this integration branch
has accumulated all intended follow-up changes.
