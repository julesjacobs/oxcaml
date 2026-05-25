# Progress

Last updated: 2026-05-24.

## Current Claim

First AMD64 implementation patch is in progress. The vendored LLVM X86
prologue now recognizes OxCaml stack-check functions and emits an AMD64
prologue stack-growth slow path, and the AMD64 runtime now provides the
matching no-OCaml-stack helper.

The X86 prologue patch now also builds with this vendored LLVM snapshot and
passes a direct `llc` prologue smoke test for a stack-frame-using
`oxcaml_fpcc` function.

## Evidence

- OxCaml branch: `jujacobs/llvm-amd64-support`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/10
- Initial state commit: `7965927f6af4`
- Agent PR-record commit: `25e06ddb58`
- Base integration branch: `jujacobs/llvm-backend-integration`
- Base integration commit at worktree creation: `95443659432c`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-amd64-support`
- Makefile workflow audit recorded in `GOAL.md`. Main finding:
  `Makefile.common-ox` provides `llvm-*` targets that set `LLVM_BACKEND=1`,
  use `LLVM_PATH`, require frame pointers, and support explicit `ARCH=amd64`.
- Full-validation scripts:
  `tools/build-llvm-self-stage-install.sh`,
  `tools/build-llvm-stage5-install.sh`, and
  `tools/run-llvm-stage5-ocamltest.sh`. They support `DUNE_BUILD_FLAGS`,
  `LLVM_TESTSUITE_PARALLEL`, `LLVM_TESTSUITE_JOBS`, `LIST`, and
  `EXCLUDE_REGEX`.
- Implemented source changes:
  - `vendor/llvm-project/llvm/lib/Target/X86/X86FrameLowering.cpp` disables
    red-zone use for `oxcaml-stack-check` functions and emits an inline
    AMD64 stack check that jumps to `caml_llvm_prologue_realloc_stack` without
    pushing on the OCaml stack.
  - `runtime/amd64.S` defines `caml_llvm_prologue_realloc_stack`, preserving
    OCaml registers in a `gc_regs` bucket, calling `caml_try_realloc_stack`
    from the C stack, restoring registers on success, and raising
    `Stack_overflow` via the no-push internal raise path on failure.
  - `backend/amd64/simd_selection.ml` uses the generated AVX2
    `vpmulhrsw_Y_Y_Ym256` selector for `caml_avx2_int16x16_mul_round`.
  - `backend/llvm/llvm_ir.{ml,mli}` can now print
    `llvm.experimental.stackmap` calls with optional live-root operands.
  - `backend/llvm/llvmize.ml` now permits AMD64 stack checks, attaches
    `oxcaml-stack-check` on AMD64 LLVM functions, emits an incremental
    AMD64-specific `specific` lowering subset, and emits explicit stackmap
    descriptors for non-tail indirect OCaml calls. This gets AMD64 buildability
    further but is not a final multi-arch cleanup: the old ARM64-specific
    `specific` body is temporarily commented out.
  - `backend/llvm/llvmize.ml` passes `-fPIC` for AMD64 LLVM IR-to-assembly
    compilation and emits AMD64 recover-rbp variable loads through
    `@GOTPCREL`, clearing PIE and shared-object relocation failures.
  - `backend/amd64/{cfg_selection,arch}.ml` and `backend/llvm/llvmize.ml`
    recognize and lower the scalar float `sqrt`/`sqrtf` and SIMD float32/float64
    min/max/round/cast builtins needed by the LLVM backend path.
  - `vendor/llvm-project/llvm/lib/Target/X86/{X86AsmPrinter.cpp,
    X86AsmPrinter.h,X86MCInstLower.cpp}` records a temporary return-address
    label after emitted X86 call instructions and lets the following
    standalone stackmap consume that label. This fixes call stackmaps whose
    MachineInstr position is after LLVM's return-value copies, where the
    runtime needs the actual post-call return address.
  - `backend/llvm/llvmize.ml` now lowers X86_64 `Pushtrap` without routing the
    normal `wrap_try` return through an unmodelled `%rax` write/read pair. The
    normal path branches on the SSA `wrap_try` result, while exception recovery
    reads the bucket with a fixed `={rax}` inline-asm constraint.
- Validation done this turn:
  - `git diff --check` passed.
  - Added local opam repository `tools/ci/local-opam` as `oxcaml-local`.
  - Created switch `oxcaml-5.4.0+oxcaml` with
    `ocaml-base-compiler.5.4.0+oxcaml`; installed `dune.3.20.2` and
    `menhir.20231231`.
  - `./configure --enable-frame-pointers` now succeeds in that switch.
  - Stock `/usr/bin/llc` is not usable for this validation because it rejects
    `oxcaml_fpcc` IR.
  - Built patched vendored LLVM tools in
    `/tmp/oxcaml-agent-llvm-amd64-support/llvm-build` with:
    `cmake -S vendor/llvm-project/llvm -B /tmp/oxcaml-agent-llvm-amd64-support/llvm-build -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_ENABLE_PROJECTS= -DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF -DLLVM_INCLUDE_EXAMPLES=OFF -DCMAKE_CXX_FLAGS='-include cstdint'`
    then
    `cmake --build /tmp/oxcaml-agent-llvm-amd64-support/llvm-build --target llc opt -- -j8`.
  - Fixed two X86 prologue patch issues found by the vendored LLVM build and a
    direct `llc` test:
    - `getOxCamlRuntimeSymbol` now uses `MachineFunction::getDataLayout()`
      because this vendored LLVM snapshot has no `Function::getDataLayout()`.
    - The inline asm immediate uses `$$` so LLVM's inline asm printer emits a
      literal `$`.
  - Runtime assembly was preprocessed and assembled with temporary configured
    `m.h`/`s.h` shims before the OCaml 5.4 switch was available:
    `gcc -E -x assembler-with-cpp -I /tmp/oxcaml-agent-llvm-amd64-support/include -I /tmp/oxcaml-agent-llvm-amd64-support/include/caml -I runtime -DSYS_linux runtime/amd64.S >/tmp/oxcaml-agent-llvm-amd64-support/amd64.S.pp`
    then
    `gcc -c -x assembler /tmp/oxcaml-agent-llvm-amd64-support/amd64.S.pp -o /tmp/oxcaml-agent-llvm-amd64-support/amd64.o`.
  - `nm -g /tmp/oxcaml-agent-llvm-amd64-support/amd64.o` shows
    `caml_llvm_prologue_realloc_stack`.
  - A standalone AMD64 assembly snippet using the LLVM inline stack-check
    sequence assembled and produced the expected `R_X86_64_PLT32`
    relocation to `caml_llvm_prologue_realloc_stack`.
  - Temporary wrapper:
    `/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper`, log at
    `/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper.log`. It dispatches
    `-x ir -emit-llvm -S` to patched `opt`, `-x ir -S` to patched `llc`, and
    assembly/object work to `/usr/bin/gcc`.
  - Direct prologue smoke test:
    `/tmp/oxcaml-agent-llvm-amd64-support/llvm-build/bin/llc -O3 -filetype=asm /tmp/oxcaml-agent-llvm-amd64-support/prologue.ll -o /tmp/oxcaml-agent-llvm-amd64-support/prologue.s`
    emitted the stack check before `subq $72, %rsp`, with `%r10` carrying the
    required words, `%r11` carrying the continuation label, and a relocation to
    `caml_llvm_prologue_realloc_stack`. `/usr/bin/gcc -c` assembled it
    successfully.
  - Per workspace guidance and user clarification, validation now uses normal
    Dune parallelism and avoids only concurrent top-level `make`/`dune`
    commands in this checkout; no `-j1` is used for OxCaml builds.
  - A stale previous run with `ccopt=-no-pie` left non-PIC options in `.cmxa`
    metadata. `make clean` plus rerunning without `ccopt=-no-pie` confirmed the
    remaining link failures were LLVM AMD64 PIC/codegen issues, not a global
    link flag fix.
  - `ARCH=amd64 OCAMLPARAM='_,keep-llvmir=1'
    BUILD_OCAMLPARAM='_,keep-llvmir=1' LLVM_PATH="$LLVM_PATH"
    LLVM_BOOT_BACKEND=0 make llvm-install` now gets past the previous
    generated-SIMD typechecking mismatch, PIE executable link failures,
    shared-object `recover_rbp_var` relocation failures, scalar builtin
    recognition failures, and the LLVM-built `tools/simdgen/simdgen.exe`
    startup stack-scan crash.
  - The wrapper log contains many `-x ir` invocations, confirming the patched
    LLVM tools are being exercised.
  - Manual reproduction of the former `simdgen.exe` blocker now succeeds:
    `cd _build/main/tools/simdgen && ./simdgen.exe amd64` exits 0, writes
    9,987 lines, and emits no stderr.
  - Diagnosed the next stack-scan crash in
    `_build/main/tools/objinfo.exe ocamloptcomp.cma`: the missing descriptor was
    for the first non-tail indirect call in `caml_apply3`. The return address
    was `0x...4664`, but the standalone stackmap descriptor had been emitted
    after LLVM's `mov %rax,%rbx` return-value copy at `0x...4667`.
  - A statepoint-based attempt fixed that descriptor but corrupted the
    `merge_archives.exe` path, so it was backed out. The committed direction is
    the narrower X86 asm-printer label fix described above.
  - After rebuilding agent-local `llc` and forcing `_build/main` regeneration,
    `_build/main/tools/objinfo.exe ocamloptcomp.cma` now exits 0 and writes
    34,474 lines, confirming the `caml_apply3` missing-frame-descriptor
    reproducer is fixed.
  - Diagnosed the next `tools/merge_archives.exe` crash as X86_64 exception
    lowering relying on `%rax` without telling LLVM. In
    `Bytelink.check_consistency`, the old lowering wrote the zero `wrap_try`
    result to `%rax`, branched to the shared exception-entry block, and then
    read `%rax`; LLVM was free to reuse `%rax` for the
    `camlBytelink__check_consistency_138 + 0x18` closure address before the
    read, causing `caml_reraise_exn` to propagate that closure as a bogus
    exception bucket.
  - After rebuilding the boot compiler under the
    `oxcaml-5.4.0+oxcaml` opam switch, regenerating `duneconf/main.ws` with
    `LLVM_BACKEND=1`, and forcing `_build/main` regeneration, the regenerated
    `bytelink.ll` contains `asm sideeffect "", "={rax}"()` for exception
    bucket reads and no longer contains the old `movq $0, %rax` / `mov %rax,
    $0` pair.
  - Focused validation now passes:
    - `_build/main/tools/merge_archives.exe /tmp/merge_repro.cma
      ocamloptcomp.cma` exits 0.
    - `_build/main/tools/objinfo.exe ocamloptcomp.cma` exits 0 and writes
      34,474 lines.
  - `make llvm-install` with
    `ARCH=amd64 OCAMLPARAM='_,keep-llvmir=1'
    BUILD_OCAMLPARAM='_,keep-llvmir=1' LLVM_PATH="$LLVM_PATH"
    LLVM_BOOT_BACKEND=0` now reaches the final install rsync. The default
    `/usr/local` prefix fails with permission denied, but rerunning as
    `make llvm-install prefix=/tmp/oxcaml-agent-llvm-amd64-support/install`
    succeeds.

## Current Blocker

No focused blocker remains for `llvm-install` with a writable prefix. The
default `/usr/local` install prefix is not writable in this environment, so use
an explicit agent-local `prefix=...` for install validation.

## Next Step

Run the first focused LLVM tests with the installed compiler:
`ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-test-one TEST=llvm-codegen/arithmetic`.
If that exposes an AMD64 lowering/runtime failure, reduce it before expanding
the implementation.
