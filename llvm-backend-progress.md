# LLVM Backend Progress

Last updated: 2026-05-21.

Goal: first make useful test-suite slices pass with a normal-built compiler plus
`-llvm-backend`; then run the same slices with an LLVM-built compiler; only
after that target full self-hosting.

## Current State

- Progress is steady on single-domain programs and test-suite slices.
- Hard problems should still be handled by reductions and design experiments,
  not by broad self-host retries. The main hard areas are exception/effect
  control flow, runtime stack switching, and multidomain interactions.
- The normal-built driver plus `-llvm-backend` passes targeted arm64 llvmize
  tests and many manual programs: allocation in `try`, external calls in `try`,
  stack growth, caught stack overflow, small multi-module programs, callbacks,
  and effect/continuation probes.
- LLVM-built compilers have self-compiled far enough to build stage-3 and
  stage-4 `oxcaml_main_native.exe` on arm64 when Dune is forced to use
  `-llvm-backend`. The produced compilers report `5.2.0+ox`/`arm64` and can
  compile/run small programs with `-llvm-backend`.
- This is not full production self-hosting yet: the workspace still uses
  normal-built bytecode tools/runtime stdlib, and only selected test-suite
  slices have been run on later-stage compilers.

Always verify real LLVM use by checking `/tmp/oxcaml-clang-wrapper.log` for:

```sh
/tmp/oxcaml-clang-wrapper ... -x ir ... \
  -ffixed-x15 -ffixed-x26 -ffixed-x27 -ffixed-x28
```

## Known Setup

- Normal runtime stdlib:
  `_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib`.
- Normal stage-1 compiler:
  `_normal_stage1_fpfix_build/install/main/bin/ocamlopt.opt`.
- Stage-2 compiler: `_llvm_stage2_fpfix_build/main/oxcaml_main_native.exe`.
- Stage-3 compiler: `_llvm_stage3_probe_build/main/oxcaml_main_native.exe`.
- Stage-4 compiler: `_llvm_stage4_probe_build/main/oxcaml_main_native.exe`.
- Stage builds used real local LLVM; stage-3/stage-4 build wrapper logs each
  had 1596 clang calls, 798 with `-x ir`.
- Fake stage-4 ocamltest root: `/tmp/oxcaml-stage4-ocamltest-src`.
  Keep `ocamlopt.opt` pointing at the stage-4 native compiler, but keep root
  `ocamlopt`/`ocamlc` pointing at bytecode tools because some ocamltest actions
  run them through `runtime/ocamlrun`.
- Do not point stage-4 ocamltest at stale `_install/lib/ocaml`; use
  `_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib`.
- Use `tools/setup-llvm-stage4-ocamltest.sh` to create a fresh fake root. It
  was verified with `FAKE_ROOT=/tmp/oxcaml-stage4-ocamltest-src-script` on the
  `lib-array`/`lib-str`/`lib-systhreads` subset: 55 passed, 5 skipped, 0
  failed, with 22 fresh `-x ir` calls.
- Extra fake-root paths currently needed for broader ocamltest slices:
  `CAML_LD_LIBRARY_PATH=/tmp/oxcaml-stage4-ocamltest-src/stublibs`,
  `otherlibs/{str,stdlib_stable}` symlinked to the normal stage-1 install,
  thread stub archives symlinked into the fake `threads` library directory, and
  `runtime/{threads.h,caml/threads.h}` available for C-stub tests.
- `make runtime-stdlib` currently depends on the ambient opam switch and can
  fail if the switch is incompatible. For the latest runtime-only check, direct
  Dune rebuilt `_build/runtime_stdlib/runtime/libasmrun*`; those archives were
  copied into both `_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib` and
  `_normal_stage1_fpfix_build/main/runtime` for verification.

## Verified Tests

- Normal-built compiler plus `-llvm-backend`: `@runtest-llvmize` passes on
  arm64. The alias includes 20 output checks; IR checks remain amd64-only
  because their expected output is target-specific.
- Normal-built compiler plus `-llvm-backend`: reduced repros in
  `/tmp/oxcaml-six-args-repro` and `/tmp/oxcaml-llvm-refill-repro` pass.
- LLVM-built stage-2 compiler plus `-llvm-backend`: direct single-domain
  runtime/reference tests, callbacks through C, exceptions through C, effects,
  GC roots, weak/ephemeron/finalizer tests, `lib-obj`, deterministic
  `lib-random`, `lib-bigarray`, and non-C-stub `asmcomp` probes pass with fresh
  wrapper logs containing `-x ir`.
- LLVM-built compiler-source probes: 45 generated compiler modules compiled into
  `/tmp`, including `parser.pp`, `typecore`, `typemod`, `translcore`,
  `translmod`, `llvm_ir`, `llvmize`, `asmgen`, `asmlink`, `compile`, and
  `oxcaml_main_native`.
- Stage-3 and stage-4 Dune probes both built `oxcaml_main_native.exe` with
  forced `-llvm-backend`. Each produced compiler compiled a Fibonacci smoke test
  through LLVM; the executable printed `55`.
- Stage-3 and stage-4 compilers pass `@runtest-llvmize` on arm64 with
  `OXCAML_LLVM_TEST_OCAMLOPT` pointing at the LLVM-built compiler.
- Stage-4 ocamltest stdlib/runtime slices pass with `-llvm-backend`:
  `lib-list`, `lib-string`, `lib-buffer`, `lib-queue`, `lib-stack`, `lib-int`,
  `lib-int64`, `lib-float`, `lib-bytes`, `lib-printf`, `lib-format`,
  `lib-hashtbl`, `lib-set`, `lib-marshal`, `lib-filename`, `lib-obj`,
  `lib-random`, `lib-bigarray`, `lib-array`, `lib-arg`, `lib-bool`,
  `lib-option`, `lib-result`, `lib-seq`, `lib-lazy`, `lib-digest`,
  `lib-channels`, `lib-sys`, `lib-str`, `lib-unix`, and `lib-systhreads`.
- Stage-4 callback/effects/gc-roots ocamltest slice passes with
  `-llvm-backend`: 175 passed, 29 skipped, 0 failed, with 89 fresh `-x ir`
  calls. This includes `effects/reperform_consumed_cont.ml` and
  `effects/dynamic.ml`.
- Stage-4 broader stdlib/unix/systhreads slice passes with `-llvm-backend`:
  229 passed, 17 skipped, 0 failed, with 123 fresh `-x ir` calls.
- Reduced repro `/tmp/oxcaml-reperform-consumed/test.ml` now passes with the
  normal-built compiler plus `-llvm-backend` and patched runtime:
  `first reperform raised Unhandled: true` and
  `second reperform raised Continuation_already_resumed: true`; wrapper log had
  2 fresh `-x ir` calls.

## Key Findings

- AArch64 LLVM code must not use `x29` as a frame pointer unless the compiler
  and all OCaml callees preserve it. The current OxCaml/LLVM direction is to
  avoid frame-pointer codegen for LLVM arm64 stackmap functions; LLVM source
  commit `0fa649d3b` contains the local AArch64 OxCaml support.
- Protected OCaml calls, protected external calls, `raise_notrace`,
  allocation/poll safepoints, and stack checks need explicit unwind successors
  when a trap is active.
- AArch64 `Pushtrap` uses preallocated entry-frame trap blocks. Trap recovery
  must store a trap-block-relative restore-SP delta because stack growth
  rewrites the trap-block chain.
- The latest `reperform_consumed_cont` crash was not missing LLVM `gc-live`
  metadata. `last_fiber` was freed by `caml_runstack`; `caml_reperform`
  dereferenced that stale stack before checking whether the continuation had
  already been consumed. The normal backend passed accidentally because the
  freed handler memory was not zeroed before the second `reperform` in that
  build. The runtime fix checks `Cont_fiber = Val_ptr(NULL)` before touching
  `last_fiber`.
- The direct `asmcomp/optargs` probe is not useful yet: it fails its
  no-allocation assertion even with the normal-built compiler and normal
  backend, so it needs the real testsuite invocation or exact optimization
  setup before classifying LLVM behavior.

## Next Checks

1. Keep expanding stage-4 ocamltest slices before broad self-hosting.
2. Convert the reduced runtime/effect issue into a committed source change and
   rely on the existing `effects/reperform_consumed_cont.ml` test as coverage.
3. Build enough LLVM tools for `llvm-lit`, or keep using direct `llc |
   FileCheck` for targeted LLVM tests until the toolchain build is expanded.
4. If an LLVM-built compiler test fails, reduce from that test-suite case rather
   than returning directly to broad self-hosting.
