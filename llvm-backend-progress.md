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
- AArch64 non-initializing word stores now preserve the native backend's
  memory-ordering barrier in LLVM. A small `mutable int` setter emits
  `fence acquire` in LLVM IR and `dmb ishld; str ...` in final assembly, while
  pointer field assignment still goes through `caml_modify`.

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
- AArch64 non-initializing `Word_int`/`Word_val` stores in LLVM now emit
  `fence acquire`, matching the native backend's `dmb ishld` before the store.
  Coverage is in `testsuite/tests/llvm-codegen/store_modify.ml`.
- AArch64 atomic field loads in LLVM now emit `fence acquire` plus a `seq_cst`
  LLVM atomic load, matching the native backend's `dmb ishld; ldar` final
  assembly. Coverage is in `testsuite/tests/llvm-codegen/atomic_load.ml`.
- Exception control-flow edges were audited for the current arm64 LLVM path.
  Potentially-raising calls under a trap lower to `invoke` plus `landingpad`,
  `wrap_try` is marked `returns_twice`, and a focused smoke test compiled with
  `-llvm-backend` ran correctly.
- Non-preemption effect-handler tests passed under real LLVM use:
  `make llvm-test-one DIR=effects LLVM_PATH=/tmp/oxcaml-clang-wrapper` reported
  `127` passed, `28` skipped, `0` failed, and the wrapper log contained `2073`
  `-x ir` invocations. The skipped preemption tests require a poll-insertion
  configured build; a manual `-enable-poll-insertion` attempt also timed out on
  the native backend, so it was not LLVM-specific evidence.
- Allocating C calls were audited. Focused IR inspection showed allocating
  externals lowering to `caml_c_call`/`caml_c_call_stack_args` with statepoints
  and live roots. LLVM test runs passed for `c-api` (`11` passed), `callback`
  (`38` passed, `1` skipped), and `statmemprof` (`48` passed, `3` skipped).
- Non-allocating C-call wrappers were audited. Focused IR inspection showed the
  wrapper switching to `Domain_c_stack`, making the direct C call, switching
  back, and returning unchanged runtime registers. A noalloc C stub that raises
  is not a supported comparison point: native does not catch it either.
- Local allocation was audited. `Begin_region`/`End_region` match the native
  local-sp protocol, the local slow path calls `caml_call_local_realloc` as a GC
  leaf, and focused IR showed local values live across a later `Gc.minor`
  statepoint. `make llvm-test-one DIR=typing-local
  LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed (`87` passed, `6` skipped).
- Copied-stack growth now has focused LLVM runtime coverage:
  `testsuite/tests/llvm-codegen/stack_growth.ml`. The test forces stack growth
  in an effect-handled non-tail-recursive computation and checks the result.
  `make llvm-test-one DIR=llvm-codegen LLVM_PATH=/tmp/oxcaml-clang-wrapper`
  passed (`27` passed), with `2051` real `-x ir` wrapper invocations. The
  remaining copied-stack question is design-level: the arm64 runtime rewrites
  raw stack words conservatively, and we still need stronger evidence or a
  metadata design to rule out false-positive rewrites of non-pointer words.
- SIMD/vector lowering was audited for current arm64 support. Unaligned vector
  memory operations lower with `align 1`, arm64 splits 256-bit vectors before
  Cmm, and a focused SIMD smoke test plus `make llvm-test-one
  DIR=typing-layouts-vec128 LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed under
  real LLVM use.
- DWARF/frame-table audit found a real compatibility gap: the custom LLVM
  frametable printer only emits short frame descriptors. Large static frames
  currently abort in LLVM codegen with
  `[OxCamlGCPrinter] frame size requires long frames`; coverage is in
  `testsuite/tests/llvm-codegen/long_frame.ml`. `make llvm-test-one
  DIR=llvm-codegen LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed with the
  regression included (`31` passed, `2052` real `-x ir` wrapper invocations).

## Test Harness Notes

- Existing `[%%expect_asm]` tests are driven by `expect.opt`, not by parsing a
  `.s` file after `ocamlopt -S`: `expectnat` registers a one-shot callback with
  `Emit`, amd64 `Emit.fundecl` records and normalizes the emitted function body,
  and the expect runner compares that callback output against the inline
  architecture-specific payload.
- LLVM expect-test support is implemented for focused macOS/arm64 tests.
  `expect.opt` accepts
  `[%%expect_llvm_ir]` and `[%%expect_llvm_asm]`, `expectnat` registers
  callbacks with `Llvmize`, and `Llvmize.end_assembly` captures normalized LLVM
  code functions and LLVM-generated assembly functions per phrase. If a chunk
  has multiple phrases before one expectation, the expect harness concatenates
  those per-phrase LLVM outputs. `expectnat` now reads `OCAMLPARAM` before and
  after command-line parsing so `llvm-test-one` can pass
  `llvm-path=/tmp/oxcaml-clang-wrapper`.
- Validated: `make test-one DIR=llvm-codegen LLVM_BACKEND=1
  LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed on arm64. The current
  `llvm-codegen` directory has arithmetic, allocation/GC slow path,
  indirect-call, control-flow, exception-handler shape, and multi-phrase chunk
  tests. The wrapper log included `-x ir` plus the fixed-register flags for the
  expect phrases.
- Validated: `make test-one DIR=tool-expect-test` without `LLVM_BACKEND=1`
  passed on arm64, so the focused existing expect-tool tests still work through
  the non-LLVM path.
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
- Main remaining design risks: effect/preemption control flow, runtime stack
  switching, multidomain interactions, and the exact statepoint-to-frametable
  contract.

## Next Checks

1. Add target coverage beyond macOS/arm64. Final assembly expectations need
   target-specific handling because Mach-O and ELF sections differ.
2. Add an effect-control-flow LLVM expect test once there is a small useful
   example.
3. Audit copied-stack relocation for false positives and decide whether
   conservative runtime scanning is acceptable or needs stack-address metadata.
