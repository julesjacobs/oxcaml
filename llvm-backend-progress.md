# LLVM Backend Progress

Last updated: 2026-05-22.

Goal: make the LLVM backend able to replace the native backend: build the
compiler and required libraries with LLVM, then pass the compiler test suite
using that LLVM-built toolchain.

## Current Status

- Progress is steady. The normal-built compiler with `-llvm-backend` passes the
  broad tests on arm64, and an LLVM-built boot compiler can now rebuild a staged
  runtime/main compiler install with LLVM forced.
- This is not production self-hosting yet. The staged compiler can compile and
  run many real programs and tests, and the normal Make path now has an
  opt-in LLVM boot context that can build boot/runtime/main and pass the full
  testsuite with LLVM enabled.
- `LLVM_BOOT_BACKEND=1` makes `duneconf/boot.ws` use an LLVM-capable stage-0
  install, defaulting to `_install`, plus `llvm-backend=1`. After removing stale
  `_build/default` artifacts from a previous non-LLVM boot context, `make
  boot-compiler LLVM_BOOT_BACKEND=1` succeeded with `839` fresh `-x ir`
  compilations.
- `make compiler LLVM_BOOT_BACKEND=1 LLVM_BACKEND=1` then succeeded through the
  normal Make/Dune dependency chain on arm64, with `1186` fresh `-x ir`
  compilations after clearing the wrapper log.
- `make test-one DIR=basic LLVM_BOOT_BACKEND=1 LLVM_BACKEND=1` passed through
  the normal `install_for_test` path: `82` passed, `0` failed, with `78` fresh
  `-x ir` compilations during the test run.
- A standalone `make install LLVM_BOOT_BACKEND=1 LLVM_BACKEND=1` passed on
  arm64 and refreshed `_install`. The wrapper log recorded `3616` clang calls
  and `1808` fresh `-x ir` compilations, all with the fixed register flags.
- A clean top-level `make test LLVM_BOOT_BACKEND=1 LLVM_BACKEND=1` passed on
  arm64: boot/runtime/main install plus testsuite, `6599` passed, `287`
  skipped, `0` failed, `0` unexpected errors. The wrapper log recorded `9540`
  clang calls and `4770` fresh `-x ir` compilations, all with the fixed
  register flags.
- The latest broad stage-5 ocamltest sweep excluded only `tests/asmgen` and
  `tests/asmcomp`. It passed with forced LLVM: `6573` passed, `274` skipped,
  `0` failed, `0` unexpected errors. The wrapper log recorded `5474` clang
  calls and `2737` fresh `-x ir` compilations.
- `tools/run-llvm-stage5-ocamltest.sh` now wraps the stage-5 fake-root setup,
  self-stage fake-root setup with `SELF_STAGE=1`, list generation, forced-LLVM
  ocamltest run, and wrapper counts.
- `tools/build-llvm-self-stage-install.sh` is the current repeatable
  self-stage path. It builds an LLVM boot compiler from `_install`, packages it
  as `_llvm_boot_install`, then uses that LLVM-built boot compiler to rebuild
  runtime/main into `_llvm_self_stage_install`. The latest script run succeeded:
  boot `839` fresh `-x ir`, runtime `74` fresh `-x ir`, main `1112` fresh
  `-x ir`; the resulting compiler compiled and ran `fib 10` with forced LLVM,
  output `55`, `2` fresh `-x ir`.
- The broad self-stage ocamltest sweep used `_llvm_self_stage_install` and
  `_llvm_self_stage_main_build`, excluded only `tests/asmgen` and
  `tests/asmcomp`, and passed with forced LLVM: `6573` passed, `274` skipped,
  `0` failed, `0` unexpected errors. The wrapper log recorded `5996` clang
  calls and `2998` fresh `-x ir` compilations.
- `tools/build-llvm-stage5-install.sh` still wraps the lower-level staged
  LLVM runtime/main rebuild and `_llvm_stage5_install` refresh.
- `LLVM_BACKEND=1` is now the normal Make entry point for this mode. It sets the
  build contexts and testsuite environment consistently, and regenerates the
  Dune workspaces when the generated contents change.
- Normal `test-one-no-rebuild` also passed selected runtime/control-flow slices
  with forced LLVM: `effects`, `exception-extra-args`, `match-exception`,
  `runtime-C-exceptions`, `statmemprof`, and `weak-ephe-final`; combined wrapper
  count was `130` fresh `-x ir`.
- After refreshing `_install`, `_install/bin/ocamlopt.opt` compiled and ran a
  recursive `fib` program with forced LLVM: output `55`, `2` fresh `-x ir`.
- The boot Dune context still clears `OCAMLPARAM` by default. The old opam
  compiler rejects `llvm-backend` and `llvm-path`, but
  `LLVM_BOOT_BACKEND=1` makes the boot context use the LLVM-capable `_install`
  compiler as stage 0 and force LLVM for boot compilation.
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
  LLVM_BACKEND=1 LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Build through the normal Make path with LLVM also enabled for the boot context:

```sh
rm -rf _build/default  # needed when switching from a non-LLVM boot context
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make compiler \
  LLVM_BOOT_BACKEND=1 LLVM_BACKEND=1 LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Install through the same explicit LLVM-enabled path:

```sh
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make install \
  LLVM_BOOT_BACKEND=1 LLVM_BACKEND=1 LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Run the full installed testsuite through the same path:

```sh
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make test \
  LLVM_BOOT_BACKEND=1 LLVM_BACKEND=1 LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Run one installed-compiler testsuite directory with forced LLVM:

```sh
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make test-one DIR=typing-layouts-products \
  LLVM_BACKEND=1 LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Run the broad stage-5 forced-LLVM ocamltest sweep:

```sh
tools/run-llvm-stage5-ocamltest.sh
```

Run the broad self-stage forced-LLVM ocamltest sweep:

```sh
SELF_STAGE=1 tools/run-llvm-stage5-ocamltest.sh
```

Rebuild and refresh the staged LLVM install:

```sh
tools/build-llvm-stage5-install.sh
```

Build the LLVM boot compiler, then rebuild a staged compiler from it:

```sh
tools/build-llvm-self-stage-install.sh
```

Build the boot compiler with the LLVM-capable installed compiler as stage 0:

```sh
tools/build-llvm-boot-with-installed.sh
```

Put the OxCaml opam switch first in `PATH`.

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
- `_llvm_self_stage_install` is assembled the same way, but its runtime/main
  build uses `_llvm_boot_install` instead of `_install` as the boot compiler
  install.
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
- Normal `install_for_test`, `test`, and `test-one-no-rebuild` now delete
  mirrored `*.corrected` files before running ocamltest. Without this, generated
  correction files in the source tree or from a previous `_runtest` run can be
  treated as tests with extension `corrected`.
- Changing `duneconf/boot.ws` now clears `_build/default` and
  `_build/_bootinstall`, so switching an existing checkout between normal boot
  and `LLVM_BOOT_BACKEND=1` no longer needs a manual boot-context cleanup.
- `install_for_test` now removes replaced backend-specific testsuite
  directories recursively, so stale `_ocamltest` directories do not break
  repeated normal `make test-one` runs.

## Known Gaps

- The staged fake-root `asmgen`/`asmcomp` issues are harness-specific. The
  clean top-level Make path runs the normal `asmgen` tests successfully and
  skips the configured `asmcomp` cases for this arm64/LLVM configuration.
- The stage fake root is a useful test harness, not the final bootstrap story.
  The ordinary Make path now builds boot/runtime/main with LLVM explicitly
  enabled and passes the installed testsuite under forced LLVM. The next major
  milestone is making the explicit LLVM-enabled `make` / `make install` /
  `make test` workflow repeatable enough for regular use.
- Main remaining design risks: exception/effect control flow, runtime stack
  switching, multidomain interactions, SIMD coverage, and the exact
  statepoint-to-frametable contract.

## Next Checks

1. Audit copied-stack relocation for false positives and decide whether
   conservative runtime scanning is acceptable or needs stack-address metadata.
2. Decide whether the explicit LLVM-enabled Make workflow needs a single
   documented wrapper target, or whether `LLVM_BOOT_BACKEND=1 LLVM_BACKEND=1`
   is enough.
