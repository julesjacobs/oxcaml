# Progress

Last updated: 2026-05-25.

## Current Claim

Independent AMD64 LLVM-backend implementation is in progress. The compiler
builds on AMD64, and simple scalar programs compile and run with
`-llvm-backend` through this agent's own LLVM wrapper.

## Evidence

- OxCaml branch: `jujacobs/llvm-amd64-support-2`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/12
- Initial state commit: `c206e2f15041`
- PR-record commit: `b1cde046ea`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-amd64-support-2`
- Goal: build AMD64 support for `-llvm-backend` until simple programs run,
  full `llvm-test` passes, and `llvm-self-stage2-test` passes.
- Per-agent temp setup:
  - `eval "$(../../../scripts/agent-tmp-env)"`
  - `OXCAML_AGENT_TMP=/tmp/oxcaml-agent-llvm-amd64-support-2`
  - `LLVM_PATH=/tmp/oxcaml-agent-llvm-amd64-support-2/clang-wrapper`
  - `LLVM_WRAPPER_LOG=/tmp/oxcaml-agent-llvm-amd64-support-2/clang-wrapper.log`
- Built vendored LLVM `llc` and `opt` for this agent checkout under
  `$OXCAML_AGENT_TMP/llvm-build` with `LLVM_TARGETS_TO_BUILD=X86`.
- Implemented AMD64 lowering in `backend/llvm/llvmize.ml` for the AMD64
  `Arch.specific_operation` constructors needed by non-SIMD code:
  `Ilea`, `Istore_int`, `Ioffset_loc`, `Ifloatarithmem`, `Ibswap`,
  `Isextend32`, `Izextend32`, `Irdtsc`, `Irdpmc`, fences, and
  `Illvm_intrinsic`.
- Added AMD64 LLVM-backend selection/lowering for target-only `Cpackf32` and
  `Cprefetch` so they no longer fall into the generic selector fatal path.
  `Icldemote` is selected but lowered as a no-op because it is only a cache hint
  and `llvm.x86.cldemote` aborts in this LLVM X86 configuration without the
  matching selectable target feature.
- SIMD AMD64 specifics are intentionally still reported as not implemented;
  this keeps the first implementation scalar and avoids importing ARM64-specific
  lowering into AMD64.
- Fixed a normal AMD64 compiler build break in
  `backend/amd64/simd_selection.ml` by selecting the generated
  `vpmulhrsw_Y_Y_Ym256` AVX2 instruction helper.
- `opam exec -- ./configure --enable-frame-pointers` completed.
- `opam exec -- make compiler` completed after the source changes.
- Manual scalar smoke after the rebuild:
  - compiled with `_build/install/main/bin/ocamlopt.opt -nostdlib -I
    _build/runtime_stdlib_install/lib/ocaml_runtime_stdlib -llvm-backend
    -llvm-path "$LLVM_PATH"`
  - program used recursion, allocation, exceptions, `Printf`, and `sqrt`
  - output: `42 24 3.0`
  - wrapper log contained 2 `-x ir` invocations.
- Manual prefetch/cldemote smoke:
  - used builtin externals for `caml_prefetch_read_high`,
    `caml_prefetch_write_low`, and `caml_cldemote`, plus temporary C stubs for
    primitive table symbols
  - compiled with the same `-llvm-backend -llvm-path "$LLVM_PATH"` setup
  - output: `7`
  - wrapper log contained 2 `-x ir` invocations.
- Local testsuite setup works when run under opam with a make-time local prefix:
  `opam exec -- make prefix="$PWD/_local-test-install" install_for_test`.
- Direct testsuite run of `tests/llvm-codegen/arithmetic.ml` completed through
  the harness but skipped because the test is currently gated by `macos` and
  `arch_arm64`.
- Added `testsuite/tests/llvm-codegen/amd64_smoke.ml` plus
  `amd64_smoke.sh`, an AMD64-gated testsuite smoke that builds and runs three
  `-llvm-backend` programs:
  - scalar recursion/allocation/exception/`Printf`/`sqrt` smoke, expected
    output `42 24 3.0`
  - float32 conversion smoke, expected output `1.5 2.5`
  - prefetch/cldemote builtin smoke with local C primitive-table stubs,
    expected output `7`
- The smoke script passes `_runtest/stdlib` explicitly with `-nostdlib -I`
  because the local `ocamlopt.opt` can otherwise look at its configured install
  prefix and fail with `Unbound module "Stdlib"` in this harness setup.
- Focused testsuite run passed:
  `env -u DIR -u LIST opam exec -- make --trace one TEST=tests/llvm-codegen/amd64_smoke.ml`
  from `_runtest/testsuite`, with `OCAMLSRCDIR` and
  `CAML_LD_LIBRARY_PATH` pointed at `_runtest`.

## Current Blocker

No immediate source blocker. The first AMD64-specific `llvm-codegen` smoke now
runs through the official harness, but full `llvm-test` and
`llvm-self-stage2-test` have not been attempted yet.

## Next Step

Run broader AMD64 `-llvm-backend` test targets and use the first focused failure
to decide whether the next work is additional scalar lowering, SIMD lowering, or
runtime/linkage support. After that, move toward full `llvm-test` and
`llvm-self-stage2-test`.
