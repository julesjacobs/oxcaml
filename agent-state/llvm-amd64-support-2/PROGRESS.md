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
- Implemented scalar AMD64 lowering in `backend/llvm/llvmize.ml` for the AMD64
  `Arch.specific_operation` constructors needed by non-SIMD code:
  `Ilea`, `Istore_int`, `Ioffset_loc`, `Ifloatarithmem`, `Ibswap`,
  `Isextend32`, `Izextend32`, `Irdtsc`, `Irdpmc`, fences, and
  `Illvm_intrinsic`.
- SIMD/prefetch/cache-demote AMD64 specifics are intentionally still reported
  as not implemented; this keeps the first implementation scalar and avoids
  importing ARM64-specific lowering into AMD64.
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
- Tried `ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH" make
  llvm-test-one TEST=llvm-codegen/arithmetic`.
  - With the default `/usr/local` prefix it reached `install` and failed
    because the sandbox cannot write `/usr/local`.
  - Reconfiguring to a local prefix avoided that but forced a main compiler
    rebuild on this host; GCC 15 defaults to PIE for executable links and Dune
    then failed to link non-PIE OCaml objects. Global `LDFLAGS=-no-pie` fixed
    executable linking but broke shared runtime library linking, so it is not a
    valid workaround.
  - The official target has not yet reached a real AMD64 LLVM codegen failure.

## Current Blocker

No source blocker. The official `llvm-test-one` path currently needs a clean
local install/test setup that avoids both `/usr/local` writes and global
`-no-pie` on shared-library links.

## Next Step

Find or add a test invocation path that reuses the built compiler/runtime
without installing to `/usr/local`, then run focused scalar LLVM tests and fix
the first real AMD64 `-llvm-backend` lowering/runtime failure. After that,
expand toward full `llvm-test` and `llvm-self-stage2-test`.
