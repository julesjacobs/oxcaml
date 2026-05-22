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
- The LLVM-built compiler can compile and run small programs with
  `-llvm-backend`. A full test-suite run with an LLVM-built compiler is still
  pending.
- Hard problems should be handled with reductions and design experiments, not
  broad self-host retries. The main hard areas remain exception/effect control
  flow, runtime stack switching, multidomain interactions, and full SIMD
  coverage.

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
- `make runtest-llvmize` with LLVM enabled passes on arm64. The latest run was
  mostly cached but still recorded 2 fresh `-x ir` clang invocations.
- The reduced SIMD file `_build/main/oxcaml/tests/simd/builtins_u.ml` now
  compiles successfully with `-llvm-backend`; the wrapper log recorded
  `-x ir`.
- Additional reduced SIMD files now compile through LLVM: `utils_u.ml`,
  `basic_u.ml`, `ops_int64x2.ml`, `arrays256.ml`, and `ops_float32x4.ml`.
  These focused checks recorded fresh `-x ir` clang invocations.

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
  vector reinterpret casts, scalar/vector casts, ZIP, pairwise add, reciprocal
  estimate, reciprocal-square-root estimate, vector sqrt, ARM64 vector
  float32/float64 low-lane conversion, and ARM64 vector float
  min/max/arithmetic lowering.
- Current broad `make runtest` blockers still need a fresh run after the latest
  SIMD lowering fixes. Earlier blockers included scalar SIMD min/max NaN
  mismatches and `small_numbers` executable failures.

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

- AArch64 LLVM code must not use `x29` as a frame pointer unless the compiler
  and all OCaml callees preserve it. Current local LLVM support avoids
  frame-pointer codegen for LLVM arm64 stackmap functions.
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

## Next Checks

1. Run broader normal `make runtest` with LLVM enabled and reduce failures.
2. Finish SIMD coverage or add a principled fallback for unsupported NEON ops.
3. Confirm a normal bootstrap using the LLVM-built installed compiler, not just
   the boot compiler plus LLVM-enabled final build.
4. Run broader test-suite slices with an LLVM-built compiler.
5. Add targeted LLVM-side tests for the AArch64 OxCaml vector calling convention.
