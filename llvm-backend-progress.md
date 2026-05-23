# LLVM Backend Progress

Last updated: 2026-05-23.

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
- AArch64 copied-stack growth uses the same typed runtime rewrites as the native
  backend for exception/trap links, C stack links, and frame-pointer chains. An
  imprecise fallback that rewrote every copied stack word or saved GPR that
  looked like an old stack address was removed; it could corrupt raw unboxed
  integers. Coverage is in `testsuite/tests/llvm-codegen/raw_stack_word.ml`.
- `RewriteStatepointsForGC` must compute caller live roots before the
  statepoint call itself; otherwise call arguments can stay live across calls.
- AArch64 non-initializing `Word_int`/`Word_val` stores in LLVM now emit
  `fence acquire`, matching the native backend's `dmb ishld` before the store.
  Coverage is in `testsuite/tests/llvm-codegen/store_modify.ml`.
- AArch64 atomic field loads in LLVM now emit `fence acquire` plus a `seq_cst`
  LLVM atomic load, matching the native backend's `dmb ishld; ldar` final
  assembly. Coverage is in `testsuite/tests/llvm-codegen/atomic_load.ml`.
- LLVM frame-table emission now supports the runtime long-frame descriptor
  format. Large static frames emit `FRAME_LONG_MARKER`, 32-bit frame data,
  32-bit live count, and 32-bit live offsets instead of aborting in the custom
  LLVM GC printer. Coverage is in
  `testsuite/tests/llvm-codegen/long_frame.ml`; the LLVM fork change is
  `.toolchains/llvm-project-source` commit `012c49512`.
- LLVM frametable allocation metadata now has final-assembly coverage in
  `testsuite/tests/llvm-codegen/allocation_frametable.ml`: a small
  `-g -S -llvm-backend` program checks allocation count, per-allocation size
  bytes, and source-file debug strings in the emitted frametable.
- LLVM poll statepoint metadata now has final-assembly coverage in
  `testsuite/tests/llvm-codegen/poll_statepoint.ml`: the test checks both the
  LLVM IR statepoint ID and the final poll frametable record encoded with zero
  allocation entries.
- `-g -llvm-backend` now emits minimal standard DWARF source debug metadata in
  addition to OCaml frame-table debug metadata. The LLVM IR contains a compile
  unit, per-function subprogram metadata, and instruction/call locations when
  `Debuginfo` is available, so clang emits `.loc` and `.debug_*` in assembly.
  Coverage is in `testsuite/tests/llvm-codegen/dwarf_debug_info.ml`.
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
- Effect preemption now has focused LLVM coverage in
  `testsuite/tests/llvm-codegen/effect_preemption.ml`: signal-driven
  preemption, allocation across the preemption, major GC in the handler,
  explicit `%poll` in a non-allocating loop, and continuation resume. Explicit
  `%poll` and compiler-inserted polls both become `Operation.Poll`, and
  `testsuite/tests/llvm-codegen/poll_statepoint.ml` verifies that `Poll` reaches
  LLVM as a `caml_call_gc` poll statepoint. `make llvm-test-one DIR=llvm-codegen
  LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed with this included (`48` passed,
  `2061` real `-x ir` wrapper invocations).
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
  remaining copied-stack question is design-level: temporary logging showed
  real stack-growth rewrites in both copied stack slots and saved GPR slots, but
  `testsuite/tests/llvm-codegen/raw_stack_word.ml` also shows that the same
  conservative rewrite can change a raw `nativeint#` that only looks like an
  old stack address. The likely fix needs precise stack-address metadata or a
  design that avoids preserving stack addresses in unreported raw locations.
  Existing LLVM statepoint stack maps describe GC roots, and patchpoint
  liveness describes live-out registers, but neither currently identifies
  stack-address-bearing locations at the prologue stack-growth check.
- SIMD/vector lowering was audited for current arm64 support. Unaligned vector
  memory operations lower with `align 1`, arm64 splits 256-bit vectors before
  Cmm, and a focused SIMD smoke test plus `make llvm-test-one
  DIR=typing-layouts-vec128 LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed under
  real LLVM use.
- Copied-stack growth has a concrete open correctness issue. The arm64 LLVM
  fallback rewrites any copied stack word whose bits fall in the old stack
  range; `testsuite/tests/llvm-codegen/raw_stack_word.ml` shows that an
  unboxed `nativeint#` with those bits is rewritten even though it is not a
  pointer. `make llvm-test-one DIR=llvm-codegen
  LLVM_PATH=/tmp/oxcaml-clang-wrapper` passed with the current-behavior
  regression included (`40` passed, `2056` real `-x ir` wrapper invocations).
  Removing the broad rewrite is not enough: disabling copied-stack word
  rewriting still leaves saved-GPR false positives, while disabling both
  rewrites crashes the compiler build in Flambda2 simplification. This needs
  precise metadata or another design that only rewrites real stack addresses.

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
- Main remaining design risks: effect/preemption control flow, precise
  copied-stack relocation, multidomain interactions, and the exact
  statepoint-to-frametable contract.

## Next Checks

1. Add target coverage beyond macOS/arm64. Final assembly expectations need
   target-specific handling because Mach-O and ELF sections differ.
2. Add an effect-control-flow LLVM expect test once there is a small useful
   example.
3. Design precise copied-stack relocation metadata. The current conservative
   runtime scan is needed for real runs, but it can rewrite non-pointer raw
   words that happen to look like old stack addresses.
