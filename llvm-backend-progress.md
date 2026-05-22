# LLVM Backend Progress

Last updated: 2026-05-22.

Goal: make the LLVM backend able to replace the native backend: build the
compiler and required libraries with LLVM, then pass the compiler test suite
using that LLVM-built toolchain.

## Current Status

- Progress is steady. `LLVM_BACKEND=1` is the supported opt-in Make entry point
  for LLVM builds. It implies the LLVM boot-context setup, puts the configured
  Dune binary directory on `PATH` for Dune actions, and leaves default Make
  targets on the normal backend.
- `make llvm-test LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed the normal
  boot/runtime/main install plus full testsuite on arm64: `6599` passed, `287`
  skipped, `0` failed, `0` unexpected errors. The wrapper log recorded `4770`
  fresh `-x ir` compilations with the fixed-register flags.
- `make llvm-self-stage2-test LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed the
  repeat self-stage flow on arm64. It built `_llvm_self_stage_install`, rebuilt
  `_llvm_self_stage2_install` from that LLVM-built compiler, then ran the broad
  testsuite through the stage2 compiler with `-llvm-backend` forced. Results:
  `6573` passed, `274` skipped, `0` failed, `0` unexpected errors; the test
  phase recorded `2998` fresh `-x ir` compilations.
- The self-stage scripts now avoid known false failures: boot workspaces are
  generated with the same Makefile LLVM boot-context rules instead of patched
  with `sed`, and staged tool wrappers are copied via their `.real` executable
  when present. Stage install and ocamltest helper paths are canonicalized so
  the Make targets work when they pass relative paths.
- This is a strong functional milestone, not a production sign-off. The backend
  still needs design review around GC/stack-map correctness, DWARF/debugging,
  unsupported asmgen/asmcomp tests, performance, and platform coverage.

The useful current capability is: an LLVM-built compiler can rebuild the
compiler again and pass the broad non-asmgen/non-asmcomp testsuite on arm64 when
`-llvm-backend` is forced. That includes effects, exceptions, callbacks,
dynlink, Unix/Str/systhreads slices, statmemprof, unboxed products, layouts,
local allocation, C stubs, weak/finalizer tests, and many compiler/tool tests.

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
  make llvm-compiler LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Install through the same explicit LLVM-enabled workflow:

```sh
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make llvm-install LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Run the full installed testsuite through the same explicit workflow:

```sh
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make llvm-test LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Run one installed-compiler testsuite directory through the same explicit
workflow:

```sh
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make llvm-test-one DIR=typing-layouts-products LLVM_PATH=/tmp/oxcaml-clang-wrapper
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
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make llvm-self-stage-install LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Repeat that once more, using the LLVM-built staged compiler as stage 0:

```sh
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make llvm-self-stage2-install LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Run the broad self-stage forced-LLVM ocamltest sweep through Make:

```sh
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make llvm-self-stage-test LLVM_PATH=/tmp/oxcaml-clang-wrapper
```

Run the broad repeat-self-stage forced-LLVM ocamltest sweep through Make:

```sh
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make llvm-self-stage2-test LLVM_PATH=/tmp/oxcaml-clang-wrapper
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

- Existing `[%%expect_asm]` tests are driven by `expect.opt`, not by parsing a
  `.s` file after `ocamlopt -S`: `expectnat` registers a one-shot callback with
  `Emit`, amd64 `Emit.fundecl` records and normalizes the emitted function body,
  and the expect runner compares that callback output against the inline
  architecture-specific payload.
- LLVM expect-test support is partially implemented. `expect.opt` accepts
  `[%%expect_llvm_ir]` and `[%%expect_llvm_asm]`, `expectnat` registers
  callbacks with `Llvmize`, and `Llvmize.end_assembly` captures normalized `.ll`
  and LLVM-generated `.s` output per phrase. `expectnat` now reads `OCAMLPARAM`
  before and after command-line parsing so `llvm-test-one` can pass
  `llvm-path=/tmp/oxcaml-clang-wrapper`.
- Validated: `make llvm-test-one DIR=llvm-codegen
  LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed on arm64 with one
  `testsuite/tests/llvm-codegen/arithmetic.ml` test. After restricting that
  initial test to `macos; arch_arm64`, `make test-one-no-rebuild
  DIR=llvm-codegen LLVM_BACKEND=1 LLVM_PATH=/tmp/oxcaml-clang-wrapper` also
  passed; the wrapper log included `-x ir` for the expect phrase.
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
- Staged installs now wrap compiler tools to set `OCAMLLIB` relative to their
  own `bin` directory. This makes the staged compiler find its staged stdlib by
  default, even though the baked `standard_library_default` still points at the
  top-level `_install`.
- Main remaining design risks: exception/effect control flow, runtime stack
  switching, multidomain interactions, SIMD coverage, and the exact
  statepoint-to-frametable contract.

## Next Checks

1. Harden LLVM expect output normalization: target/OS tags, unstable toplevel
   names, and intentional pruning of boilerplate.
2. Add a small set of LLVM expect tests for calls/safepoints, allocation, and
   exception/control-flow shape.
3. Audit copied-stack relocation for false positives and decide whether
   conservative runtime scanning is acceptable or needs stack-address metadata.
