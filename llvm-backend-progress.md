# LLVM Backend Progress

Last updated: 2026-05-22.

Goal: make the LLVM backend able to replace the native backend: build the
compiler and required libraries with LLVM, then pass the compiler test suite
using that LLVM-built toolchain.

## Current Status

- Progress is steady on normal-built compiler plus `-llvm-backend`, targeted
  test-suite slices, and required-library rebuilds.
- This is not production self-hosting yet. Stage-3/stage-4 LLVM-built compiler
  probes have worked, but the normal workspace still uses normal-built boot
  tools in parts of the pipeline.
- The normal Make pipeline can build the compiler with LLVM enabled and can pass
  `make runtest-llvmize` on arm64 with real LLVM use.
- A broad `make runtest` with LLVM enabled passes on arm64 with real LLVM use.
  Latest run recorded 195 fresh `-x ir` clang invocations with fixed-register
  flags.
- The LLVM-built compiler can run many tests with `-llvm-backend`, but a full
  installed-compiler sweep still has failing clusters.
- Hard problems should be handled with reductions and design experiments, not
  broad self-host retries. The main hard areas remain exception/effect control
  flow, runtime stack switching, multidomain interactions, SIMD coverage, and
  the exact statepoint-to-frametable contract.

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

Run the focused LLVM tests:

```sh
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make runtest-llvmize \
  BUILD_OCAMLPARAM='_,llvm-backend=1,llvm-path=/tmp/oxcaml-clang-wrapper'
```

If switching LLVM on/off, remove stale `duneconf/runtime_stdlib.ws` and
`duneconf/main.ws` first. Put the OxCaml opam switch first in `PATH`.

## Latest Verification

- `make compiler` with LLVM enabled passes on arm64.
- A broad `make runtest` with LLVM enabled recorded fresh `-x ir` invocations
  and fails later in the suite, not during the earlier unsupported SIMD-op
  compile step.
- `make runtest-llvmize` with LLVM enabled passes on arm64. The latest run was
  mostly cached but still recorded 2 fresh `-x ir` clang invocations.
- The reduced SIMD file `_build/main/oxcaml/tests/simd/builtins_u.ml` now
  compiles successfully with `-llvm-backend`; the wrapper log recorded
  `-x ir`.
- Additional reduced SIMD files now compile through LLVM: `utils_u.ml`,
  `basic_u.ml`, `ops_int64x2.ml`, `arrays256.ml`, `ops_float32x4.ml`, and
  `ops_float64x2.ml`. These focused checks recorded fresh `-x ir` clang
  invocations.

Latest fixes behind that SIMD progress:

- Added LLVM IR support for vector machine types and vector compare/select.
- Added tentative LLVM lowering for ARM64 NEON integer arithmetic/bitwise/shift,
  lane operations, widening/narrowing, vector int/float conversions, float
  comparisons, vector rounding, widening multiply, and integer comparisons.
- Fixed overloaded LLVM intrinsic names for vector types, e.g. vector rounding
  now uses suffixes like `v2f64` instead of printed IR types like
  `< 2 x double >`.
- Fixed the local LLVM AArch64 OxCaml calling convention to assign 128-bit
  vector arguments/results to `Q0..Q15` and spill extra vectors to 16-byte
  aligned stack slots. Without this, clang crashed in AArch64 argument lowering
  on SIMD functions.
- Added vector memory/external-argument lowering, vector static data as raw
  64-bit words, vector `Opaque` lowering without scalar-register inline asm,
  vector reinterpret casts, scalar/vector casts, ZIP, EXT, pairwise add, vector
  lane copy, reciprocal estimate, reciprocal-square-root estimate, vector sqrt,
  ARM64 vector float32/float64 low-lane conversion, ARM64 vector float
  min/max/arithmetic lowering, and more ARM64 integer SIMD lowering.
- Fixed scalar `caml_simd_float*_min/max` lowering to match SSE semantics with
  ordered compare plus select. After rebuilding the focused SIMD slice through
  LLVM, `oxcaml/tests/simd/consts.out` is empty with 10 fresh `-x ir` wrapper
  calls.
- Fixed ARM64 SIMD float-to-int conversions: `cvtt` lowers to `FCVTZS` and
  `cvt` lowers to `FCVTNS`, matching the native ARM64 backend instead of using
  LLVM `fptosi` for both. `ops_float32x4.out` is now empty with fresh LLVM IR
  calls.
- Fixed `float64x2 -> float32x4` lowering to zero the high half. After the fix,
  `ops_float64x2.out` is empty with fresh LLVM IR calls.
- `basic_u.out` passes with `OCAMLPARAM=_,llvm-backend=1,...` and fresh
  wrapper logs showing 32 `-x ir` calls. This is a real LLVM run; the generated
  `camlBasic_u__entry` has a valid x29 setup before the epilogue restores
  `sp` from x29.
- `test_callee_save_neon_regs_nodynlink.out` passes with
  `OCAMLPARAM=_,llvm-backend=1,...` and a clean rebuilt runtime-stdlib. The
  wrapper log recorded fresh `-x ir` plus fixed-register flags and the output
  file is empty. The previous `retaddr=0x4` crash reproduced only while the
  linked runtime-stdlib archive was stale; after rebuilding that archive from a
  clean `runtime/fiber.c`, the focused test exits 0.
- A conservative experiment routing noalloc C calls through `caml_c_call` did
  not change the `retaddr=0x4` failure, so the private C-call wrapper is not the
  only cause.
- A 28-line CSV parser reduced from `tools/simdgen` compiles and runs with the
  LLVM backend, printing `3601` on `tools/simdgen/amd64/amd64.csv` with fresh
  `-x ir` wrapper calls. This reducer previously crashed after
  `caml_create_bytes`: its IR had `"gc-live"` allocas on `caml_c_call`, but the
  emitted frame descriptor had zero roots for those call sites.
- The local LLVM stackmap printer now records GC alloca operands in
  `GCLocations`, so OxCaml frame tables scan `"gc-live"` alloca roots on
  statepoints. Calls with deopt plus `"gc-live"` are also accepted in
  SelectionDAG lowering. This fixed the CSV reducer without adding OCaml-side
  spills.
- The local LLVM AArch64 frame lowering now places an LR-only OxCaml no-FP
  frame's saved return address at `SP+8`, the slot expected by the OCaml stack
  walker. Before that fix, the same reducer could fail with
  `caml_scan_stack: missing frame descriptor retaddr=0x0`.
- Fixed LLVM data emission for static closures with captured payloads. The last
  closure function slot now owns the remaining payload instead of splitting it
  into a separate temporary global. This fixed `basic256_u` and
  `basic256_u_nodynlink`, where a captured `int64x4#` high half was read from
  the next static object and printed `8 <> 10` / `9 <> 12`.
- A forced LLVM-enabled compiler build on arm64 succeeded and recorded 1112
  fresh `-x ir` wrapper calls. The copied normal-built and LLVM-built compiler
  binaries were benchmarked on `typecore.ml`, `typedecl.ml`, and
  `translcore.ml`; results are in `llvm-backend-compile-benchmarks.md`.
- Clean LLVM-enabled compiler rebuild after the static-closure fix passed with
  1186 fresh `-x ir` clang invocations. Focused `basic256_u` and
  `basic256_u_nodynlink` targets then passed with empty output, and broad
  `make runtest` passed with 195 fresh `-x ir` clang invocations.
- Direct Dune probes must also be checked against
  `/tmp/oxcaml-clang-wrapper.log`; use `OCAMLPARAM`, not just
  `BUILD_OCAMLPARAM`, for these focused tests.
- Installed LLVM-built compiler plus forced `-llvm-backend` passed
  `@runtest-llvmize` with 60 fresh IR compilations.
- Installed LLVM-built compiler plus forced `-llvm-backend` passed focused
  ocamltest slices: `tests/basic`, `tests/effects`, `tests/callback`,
  `tests/gc-roots`, `tests/lib-unix`, `tests/lib-str`, and
  `tests/lib-systhreads`.
- A full installed-compiler flambda2 testsuite sweep used real LLVM
  (`2719` fresh IR compilations) and reported `6531` passed, `287` skipped,
  and `63` failed. The first fixed cluster was `tests/quotation`.
- `tests/quotation` now passes under the installed LLVM-built compiler with
  forced `-llvm-backend`: `40` passed, `45` fresh IR compilations. Fixes:
  bundled CM generation now uses LLVM begin/end hooks, headered data can be
  followed by additional C data labels, and `Name_for_debugger` is ignored like
  native emitters do.

## Previously Verified

- Normal-built compiler plus `-llvm-backend`: allocation in `try`, external
  calls in `try`, stack growth, caught stack overflow, callbacks, effects,
  continuation probes, small multi-module programs, weak/ephemeron/finalizer
  tests, and several stdlib/runtime slices passed.
- Stage-3 and stage-4 LLVM-built compiler probes built
  `oxcaml_main_native.exe` with forced `-llvm-backend`; each compiler compiled a
  Fibonacci smoke test through LLVM and printed `55`.
- Stage-3 and stage-4 compilers passed `@runtest-llvmize` on arm64 with
  `OXCAML_LLVM_TEST_OCAMLOPT` pointing at the LLVM-built compiler.
- Stage-4 ocamltest slices for stdlib/runtime, callbacks/effects/gc-roots, and
  `lib-str`/`lib-unix`/`lib-systhreads` passed with `-llvm-backend` and fresh
  wrapper logs containing `-x ir`.

## Design Findings

- AArch64 LLVM code must not use the platform frame-pointer prologue for normal
  OxCaml frames, because the OCaml stack walker finds saved return addresses
  from `sp` and frame descriptors. However, forcing no-FP unconditionally is
  also wrong: LLVM still needs x29 for var-sized objects, stack realignment, or
  frame-address-taking functions. The local LLVM patch now avoids no-FP only in
  those cases and removes FP from the no-FP callee-save list.
- Vector `Opaque` lowering should not create a dynamic alloca on arm64. It now
  uses an empty SIMD-register inline asm (`=w,0`), which helped `basic_u.out`
  pass under real LLVM.
- Protected OCaml calls, protected external calls, `raise_notrace`,
  allocation/poll safepoints, and stack checks need explicit unwind successors
  when a trap is active.
- AArch64 `Pushtrap` uses preallocated entry-frame trap blocks. Trap recovery
  must store a trap-block-relative restore-SP delta because stack growth rewrites
  the trap-block chain.
- The `reperform_consumed_cont` crash was not missing LLVM `gc-live` metadata.
  `last_fiber` was freed by `caml_runstack`; the runtime fix checks
  `Cont_fiber = Val_ptr(NULL)` before touching `last_fiber`.
- Direct `asmcomp/optargs` probes are not useful yet because the no-allocation
  assertion can fail even with the normal backend unless the exact testsuite
  setup is used.
- Stale runtime-stdlib archives can produce misleading runtime crashes. When
  changing runtime or local LLVM frame lowering, rebuild `runtime-stdlib` before
  reducing stack-walk failures.
- Stale stdlib/compiler-tool objects can also produce misleading crashes after
  local LLVM changes. Dune does not track `/tmp/oxcaml-clang-wrapper` or the
  nested LLVM build as dependencies; force a clean rebuild of the relevant
  libraries/tools before drawing conclusions from generated-tool failures.

## Next Checks

1. Reduce the next installed-compiler failures, starting with statmemprof and
   unboxed layout crash clusters from the full sweep.
2. Re-run the full installed-compiler flambda2 testsuite after each cluster fix.
3. Confirm a normal bootstrap using the LLVM-built installed compiler, not just
   the boot compiler plus LLVM-enabled final build.
