# LLVM Backend Progress

Last updated: 2026-05-22.

Goal: make the LLVM backend able to replace the native backend: build the
compiler and required libraries with LLVM, then pass the compiler test suite
using that LLVM-built toolchain.

## Current Status

- Progress is steady on normal-built compiler plus `-llvm-backend`,
  LLVM-built installed compiler probes, and targeted test-suite reductions.
- This is not production self-hosting yet. Stage-3/stage-4 LLVM-built compiler
  probes have worked, but the normal workspace still uses normal-built boot
  tools in parts of the pipeline.
- The normal Make pipeline can build the compiler with LLVM enabled and can pass
  `make runtest-llvmize` and a broad `make runtest` on arm64 with real LLVM use.
- The LLVM-built compiler can compile and run many ordinary, stdlib/runtime,
  callback/effect/GC-root, SIMD, quotation, statmemprof, and unboxed-product
  programs with `-llvm-backend`; a full installed-compiler sweep still has
  failing clusters.
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

Run one installed-compiler testsuite directory with forced LLVM:

```sh
: > /tmp/oxcaml-clang-wrapper.log
PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH \
  make test-one DIR=typing-layouts-products \
  OCAMLPARAM='_,llvm-backend=1,llvm-path=/tmp/oxcaml-clang-wrapper'
```

If switching LLVM on/off, remove stale `duneconf/runtime_stdlib.ws` and
`duneconf/main.ws` first. Put the OxCaml opam switch first in `PATH`.

## Latest Verification

- `make install` from the patched source passed.
- Reduced unboxed-product repro passed under `_install/bin/ocamlopt.opt` with
  forced LLVM. The wrapper log recorded `4` clang calls, `2` fresh `-x ir`
  compilations, and fixed-register flags.
- `repro.opt.ll` now shows `caml_curryVF_V_RF` returning `ptr addrspace(1)`;
  the final wrapper `caml_curryVF_V_RF_1` still returns `double`.
- `make test-one DIR=typing-layouts-products` with forced LLVM passed:
  `105` passed, `0` skipped, `0` failed. The wrapper log recorded `100` clang
  calls, `50` fresh `-x ir` compilations, and fixed-register flags.

Fix behind that progress:

- Generic intermediate curry wrappers allocate and return a closure, but their
  Cmm `fun_ret_type` used the final function result. Native codegen still
  returned the allocated closure in `x0`; LLVM trusted `fun_ret_type` and
  emitted wrappers such as `caml_curryVF_V_RF` as returning `double` in `d0`.
  Setting only the intermediate wrappers' `fun_ret_type` to `typ_val` fixes
  partial applications whose final result has a non-value return convention.

## Previously Verified

- Normal-built compiler plus `-llvm-backend`: allocation in `try`, external
  calls in `try`, stack growth, caught stack overflow, callbacks, effects,
  continuation probes, small multi-module programs, weak/ephemeron/finalizer
  tests, and several stdlib/runtime slices passed.
- Normal LLVM-enabled compiler builds on arm64 recorded fresh `-x ir` wrapper
  calls and passed `make runtest-llvmize`; a broad LLVM-enabled `make runtest`
  also passed with fresh IR calls.
- Stage-3 and stage-4 LLVM-built compiler probes built
  `oxcaml_main_native.exe` with forced `-llvm-backend`; each compiler compiled a
  Fibonacci smoke test through LLVM and printed `55`.
- Stage-3 and stage-4 compilers passed `@runtest-llvmize` on arm64 with
  `OXCAML_LLVM_TEST_OCAMLOPT` pointing at the LLVM-built compiler.
- Installed LLVM-built compiler plus forced `-llvm-backend` passed focused
  ocamltest slices: `tests/basic`, `tests/effects`, `tests/callback`,
  `tests/gc-roots`, `tests/lib-unix`, `tests/lib-str`, `tests/lib-systhreads`,
  `tests/quotation`, `tests/statmemprof`, and `tests/typing-layouts-products`.
- A full installed-compiler flambda2 testsuite sweep used real LLVM
  (`2719` fresh IR compilations) and reported `6531` passed, `287` skipped,
  and `63` failed before the quotation, statmemprof, and unboxed-product fixes.
- A forced LLVM-enabled compiler build on arm64 succeeded and recorded `1112`
  fresh `-x ir` wrapper calls. The copied normal-built and LLVM-built compiler
  binaries were benchmarked on `typecore.ml`, `typedecl.ml`, and
  `translcore.ml`; results are in `llvm-backend-compile-benchmarks.md`.

## Design Findings

- AArch64 LLVM code must not use the platform frame-pointer prologue for normal
  OxCaml frames, because the OCaml stack walker finds saved return addresses
  from `sp` and frame descriptors. LLVM still needs x29 for var-sized objects,
  stack realignment, or frame-address-taking functions.
- Protected OCaml calls, protected external calls, `raise_notrace`,
  allocation/poll safepoints, and stack checks need explicit unwind successors
  when a trap is active.
- AArch64 `Pushtrap` uses preallocated entry-frame trap blocks. Trap recovery
  must store a trap-block-relative restore-SP delta because stack growth rewrites
  the trap-block chain.
- Stale runtime-stdlib archives, stdlib objects, compiler-tool objects, or
  nested LLVM builds can produce misleading crashes. Dune does not track
  `/tmp/oxcaml-clang-wrapper` or the nested LLVM build as dependencies; force a
  clean rebuild of the relevant libraries/tools before drawing conclusions.
- `tests/statmemprof/bigarray.ml` exposed an LLVM statepoint liveness bug:
  `RewriteStatepointsForGC` included the statepoint instruction itself when
  computing caller live roots, which kept call arguments live across calls. The
  local LLVM fix computes liveness before the statepoint call.

## Next Checks

1. Re-run the full installed-compiler flambda2 testsuite after the current
   cluster fixes to get the new failure list.
2. Reduce the next remaining installed-compiler failure cluster.
3. Confirm a normal bootstrap using the LLVM-built installed compiler, not just
   the boot compiler plus LLVM-enabled final build.
