# LLVM Backend Progress

Last updated: 2026-05-22.

Goal: make the LLVM backend able to replace the native backend: build the
compiler and required libraries with LLVM, then pass the compiler test suite
using that LLVM-built toolchain.

## Current Status

- Progress is steady. The normal-built compiler with `-llvm-backend` passes the
  relevant LLVM smoke suite and broad tests on arm64, and the LLVM-built
  stage-5 compiler can now run most of the compiler testsuite with
  `-llvm-backend` forced.
- This is not production self-hosting yet. The stage-5 compiler can compile and
  run many real programs and tests, but the workflow still uses a staged fake
  test root plus normal build infrastructure. We have not yet made the normal
  bootstrap process use LLVM everywhere and then pass the full suite.
- The latest broad stage-5 ocamltest sweep excluded only `tests/asmgen` and
  `tests/asmcomp`. It passed with forced LLVM: `6573` passed, `274` skipped,
  `0` failed, `0` unexpected errors. The wrapper log recorded `5474` clang
  calls and `2737` fresh `-x ir` compilations.
- `tools/run-llvm-stage5-ocamltest.sh` now wraps the stage-5 fake-root setup,
  list generation, forced-LLVM ocamltest run, and wrapper counts. A smoke run
  with `GENERATE_LIST=0 LIST=/tmp/oxcaml-stage5-smoke-list.txt` containing
  `tests/basic` passed: `82` passed, `0` failed, with `78` fresh `-x ir`
  compilations.
- `tools/build-llvm-stage5-install.sh` now wraps the staged LLVM runtime/main
  rebuild and `_llvm_stage5_install` refresh. The cached validation path
  succeeds and reports wrapper counts cleanly; use a clean build directory when
  fresh `-x ir` counts are needed.
- The current copied-stack relocation fix is conservative and still needs
  design review before treating it as production-ready. Hard problems should be
  handled with reductions and design experiments, not broad self-host retries.

The useful current capability is: a stage-5 LLVM-built compiler can compile and
run a large fraction of ordinary native programs, including effects,
exceptions, callbacks, dynlink, Unix/Str/systhreads slices, statmemprof,
unboxed products, layouts, local allocation, C stubs, weak/finalizer tests, and
many compiler/tool tests, when `-llvm-backend` is forced.

Always verify real LLVM use by checking `/tmp/oxcaml-clang-wrapper.log` for
`-x ir` plus the fixed-register flags:

```sh
/tmp/oxcaml-clang-wrapper ... -x ir ... \
  -ffixed-x15 -ffixed-x26 -ffixed-x27 -ffixed-x28
```

## Known Good Commands

Build the normal compiler with LLVM enabled:

```sh
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make compiler \
  BUILD_OCAMLPARAM='_,llvm-backend=1,llvm-path=/tmp/oxcaml-clang-wrapper'
```

Run one installed-compiler testsuite directory with forced LLVM:

```sh
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make test-one DIR=typing-layouts-products \
  OCAMLPARAM='_,llvm-backend=1,llvm-path=/tmp/oxcaml-clang-wrapper'
```

Run the broad stage-5 forced-LLVM ocamltest sweep:

```sh
tools/run-llvm-stage5-ocamltest.sh
```

Rebuild and refresh the staged LLVM install:

```sh
tools/build-llvm-stage5-install.sh
```

If switching LLVM on/off, remove stale `duneconf/runtime_stdlib.ws` and
`duneconf/main.ws` first. Put the OxCaml opam switch first in `PATH`.

## Verified Fixes

- LLVM module metadata must use the assembler-escaped module name. Otherwise
  names that require escaping, such as `0001-test`, make the LLVM-generated
  `code_begin`/`data_begin`/`frametable` labels disagree with startup
  references.
- Generic intermediate curry wrappers allocate and return a closure, so their
  `fun_ret_type` must be `typ_val` even when the final function result has a
  non-value return convention.
- Unknown `[@@builtin]` externals must fall back to the normal builtin-check
  path unless the name is one of the LLVM intrinsics implemented by
  `Llvmize.intrinsic`.
- On AArch64 Darwin, the LLVM OxCaml calling convention uses `x8`-`x15` for
  OxCaml integer values like non-Darwin. This avoids LLVM lowering large
  aggregate returns through a hidden return buffer in `x28`, which conflicts
  with the OxCaml domain-state register.
- On AArch64, copied-stack growth now relocates old-stack addresses that LLVM
  kept in saved GPR slots or copied stack words. The raw stack-word rewrite must
  run after the typed exception-chain and frame-pointer rewrites.
- `RewriteStatepointsForGC` must compute caller live roots before the
  statepoint call itself; otherwise call arguments can stay live across calls.

## Test Harness Notes

- `_llvm_stage5_install` is assembled from the stage runtime stdlib and stage
  main install tree. It reports its standard library as
  `_llvm_stage5_install/lib/ocaml`.
- `tools/build-llvm-stage5-install.sh` generates the staged runtime/main dune
  workspaces, supplies the configured asm preprocessor environment, and refreshes
  `_llvm_stage5_install`.
- `tools/setup-llvm-stage4-ocamltest.sh` mirrors enough of `install_for_test`
  for stage ocamltest runs: staged tool binaries, runtime/stdlib files, compiler
  library source layout, `Makefile.config`, `Makefile.build_config`, debugger,
  toplevel, `fexprc.exe`, `codegen`, and source trees needed by source-walking
  tests.
- The helper removes stale mirrored `*.ml`, `*.mli`, and `*.corrected` files
  before relinking sources. Without this, ignored generated correction files can
  be rediscovered as tests.

## Known Gaps

- `tests/asmcomp` under stage-5 forced LLVM reports `27` passed, `10` skipped,
  `2` failed. The two failures, `optargs.ml` and `staticalloc.ml`, also fail in
  non-LLVM configurations checked so far, so treat them as unrelated until
  proven otherwise.
- `tests/asmgen` still fails before LLVM is involved: the existing
  `_runtest/testsuite/tools/codegen` reports a Cmm lexical error on
  `tests/asmgen/fib.cmm`.
- The stage fake root is a useful test harness, not the final bootstrap story.
  The next major milestone is to make the ordinary bootstrap/testing pipeline
  build and test with LLVM everywhere.
- Main remaining design risks: exception/effect control flow, runtime stack
  switching, multidomain interactions, SIMD coverage, and the exact
  statepoint-to-frametable contract.

## Next Checks

1. Audit copied-stack relocation for false positives and decide whether
   conservative runtime scanning is acceptable or needs stack-address metadata.
2. Run `tools/build-llvm-stage5-install.sh` from a clean build directory and
   record fresh wrapper counts.
3. Move from the fake-root sweep to the normal bootstrap process with LLVM
   enabled everywhere, then run the full test suite.
