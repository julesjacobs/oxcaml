# Goal

Implement AMD64/x86-64 support for the OxCaml LLVM backend.

The target outcome is that the LLVM backend can produce correct AMD64 code for
normal explicit `-llvm-backend` use, with the same runtime, GC, frame-table,
calling-convention, stack-check, and debugging contracts that the current
integration branch relies on for ARM64.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Editable areas: OxCaml compiler/runtime/test sources and vendored LLVM
  sources when needed for AMD64 backend correctness.
- Expected output: focused AMD64 implementation patches, minimal regression
  tests or reproducers, and recorded validation evidence.
- Non-goal: broad refactors that are not required for AMD64 support.

## Initial Questions

- Identify where the current LLVM backend is target-specific and ARM64-biased.
- Determine the AMD64 ABI/runtime contracts for domain state, allocation,
  stack checks, poll/safepoint lowering, frame tables, exception handling,
  and DWARF/debug metadata.
- Establish the smallest AMD64 `-llvm-backend` smoke test that should pass
  first, then expand toward testsuite coverage.

## Validation Expectations

- Prefer focused standard-compiler tests with `-llvm-backend` during
  development.
- Use self-stage2 only for full validation or for failures that cannot be
  reproduced with the standard installed compiler.
- Record exact commands, results, and relevant log paths in `PROGRESS.md`.

## LLVM Makefile Workflow Notes

- Before LLVM-backend work, run from this checkout:
  `eval "$(../../../scripts/agent-tmp-env)"`.
- If clang is needed through a per-agent wrapper, run:
  `eval "$(../../../scripts/write-agent-clang-wrapper /path/to/clang)"`.
- The LLVM Makefile targets live in `Makefile.common-ox`:
  `llvm-compiler`, `llvm-install`, `llvm-test`, `llvm-test-one`,
  `llvm-self-stage-install`, `llvm-self-stage2-install`,
  `llvm-self-stage-test`, and `llvm-self-stage2-test`.
- The first four targets just rerun normal compiler/install/test targets with
  `LLVM_BACKEND=1`. This sets `OCAMLPARAM` and `BUILD_OCAMLPARAM` to include
  `llvm-backend=1,llvm-path=$(LLVM_PATH)`.
- A tree built with LLVM backend enabled requires frame pointers. If configure
  was not run with frame pointers, `Makefile.common-ox` errors out.
- For AMD64 work, pass `ARCH=amd64` explicitly when validating target-specific
  paths, even though the scripts infer `amd64` from `x86_64` hosts.
- Useful focused iteration shape:
  `ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-install`, then
  `ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-test-one TEST=llvm-codegen/arithmetic`.
  Use normal build parallelism; avoid only concurrent top-level `make` or
  `dune` commands in this checkout because they share lockfiles.
- `llvm-self-stage-install` builds `_install`, then builds a compiler using the
  LLVM backend and runs a small smoke test.
- `llvm-self-stage2-install` repeats self-staging from `_llvm_self_stage_install`
  into `_llvm_self_stage2_install`.
- `llvm-self-stage-test` and `llvm-self-stage2-test` run
  `tools/run-llvm-stage5-ocamltest.sh` with `SELF_STAGE=1` or `SELF_STAGE=2`.
  The script defaults to excluding `tests/asmgen` and `tests/asmcomp`.
- Useful full-testsuite knobs: `LLVM_TESTSUITE_PARALLEL=0|1|auto`,
  `LLVM_TESTSUITE_JOBS=<n>`, `LIST=<path>`, `EXCLUDE_REGEX=<regex>`,
  `STAGE_INSTALL=<path>`, and `STAGE_BUILD=<path>`.
- Check `$LLVM_WRAPPER_LOG` for `-x ir` entries to confirm tests are actually
  exercising the LLVM backend.

## Branches

- OxCaml: `jujacobs/llvm-amd64-support`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/10
