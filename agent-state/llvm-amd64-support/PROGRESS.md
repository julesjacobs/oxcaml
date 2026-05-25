# Progress

Last updated: 2026-05-25.

## Current Claim

AMD64 LLVM backend bring-up now passes focused install, enabled Linux/AMD64
`llvm-codegen`, and installed-compiler boot-smoke validation with the
agent-local LLVM tools and writable install prefix. Focused AMD64 SIMD
lowering now also covers the i64x2 add/sub, vec128 interleave, and vec256
join/split operations needed by several layout/block-index tests, including
the generated mixed-blocks native test on AMD64.

The latest fixes add focused AMD64 SIMD LLVM lowering for i64x2 arithmetic,
vec128 interleaves, and vec256 join/split support. Earlier fixes reserve the
AMD64 OxCaml runtime registers in LLVM's X86 register allocator and make
exception-recovery blocks treat runtime blockaddress entry as a
register-clobbering edge.

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
  - `backend/amd64/cfg_selection.ml` and `backend/llvm/llvmize.ml` recognize
    and lower a focused AMD64 SIMD subset for the LLVM backend:
    `caml_simd_int64x2_{add,sub}`,
    `caml_simd_vec128_interleave_{low,high}_64`,
    `caml_avx_vec256_{insert,extract}_128`, their SSE aliases where present,
    and the low-half vec128/vec256 reinterpret casts used by `%join_vec256` and
    `%split_vec256`.
  - `backend/llvm/llvm_ir.{ml,mli}` and `backend/llvm/llvmize.ml` can emit an
    explicit alignment on `alloca`. Wide vector register spill slots (`Vec256`
    and `Vec512`) are now capped at `align 16`, avoiding X86 stack realignment
    and base-pointer stackmap operands for generated mixed-block SIMD code.
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
  - `backend/llvm/llvmize.ml` now emits explicit standalone stackmaps for all
    non-tail X86_64 OCaml calls, including direct calls that have live GC roots.
    This covers frame descriptors for direct-call return addresses when the
    callee triggers a GC and scans the caller.
  - `testsuite/tests/llvm-codegen/amd64_direct_call_stackmap.ml` is an
    AMD64/Linux LLVM-backend regression test for a direct call with a live heap
    root across an allocating callee.
  - `testsuite/tests/llvm-codegen/amd64_exceptions.ml` is an AMD64/Linux
    LLVM-backend regression test for exception handler setup and exception
    bucket recovery.
  - `testsuite/tests/llvm-codegen/amd64_stack_growth.ml` is an AMD64/Linux
    LLVM-backend regression test for stack growth through an effect
    continuation.
  - `testsuite/tests/llvm-codegen/amd64_core_ops.ml` is an AMD64/Linux
    LLVM-backend regression test covering allocation, indirect calls, control
    flow, atomic get/set, integer mutable fields, `caml_modify` for pointer
    fields, and repeated allocation across GC.
  - `tools/build-llvm-boot-with-installed.sh` and
    `tools/build-llvm-stage5-install.sh` no longer default `OPAM_SWITCH_BIN` to
    a hard-coded macOS path. They derive it from the active `dune` on `PATH`
    unless `OPAM_SWITCH_BIN` is explicitly supplied.
  - `tools/build-llvm-boot-with-installed.sh` creates a temporary `ocaml`
    wrapper for Dune feature detection during self-stage boot builds. This
    wrapper runs the stage0 install's bytecode toplevel with the matching
    `ocamlrun`, `OCAMLLIB`, and `CAML_LD_LIBRARY_PATH`, and the copied boot
    workspace puts the wrapper directory before the stage0 `bin` path. This
    prevents the `camlinternalquote_if_missing_from_stdlib` rule from
    mistakenly compiling a second `camlinternalQuote` against the installed
    stdlib.
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
  - A subsequent startup crash in the LLVM-built native compiler mapped to a
    missing frame descriptor at the return address of the direct call from
    `camlIdentifiable__Make_map_6_65_code` to
    `camlStdlib__Map__Make_0_142_code`. That direct call had live roots, so
    the old AMD64 logic skipped the standalone stackmap and relied on metadata
    that did not produce a runtime frame descriptor.
  - After changing X86_64 call lowering to emit explicit stackmaps for all
    non-tail OCaml calls and rebuilding with normal Dune parallelism:
    - `_build/main/oxcaml_main_native.exe -version` exits 0 and prints
      `5.2.0+ox`.
    - `_install/bin/ocamlopt.opt -version` exits 0 and prints `5.2.0+ox`.
    - `/tmp/oxcaml-agent-llvm-amd64-support/install/bin/ocamlopt.opt -version`
      exits 0 and prints `5.2.0+ox`.
    - Manual direct-call/live-root reproducer compiled with
      `_install/bin/ocamlopt.opt -O3 -llvm-backend` exits 0 and prints
      `200007`.
    - `make llvm-install prefix=/tmp/oxcaml-agent-llvm-amd64-support/install`
      exits 0.
    - `make llvm-test-one TEST=llvm-codegen/amd64_direct_call_stackmap.ml
      LIST= DIR= prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0:
      4 tests passed, 0 skipped, 0 failed.
  - The older command
    `make llvm-test-one TEST=llvm-codegen/arithmetic prefix=...` is not a useful
    AMD64 validation: the existing `arithmetic.ml` test is ARM64/macOS-only and
    skips on this Linux AMD64 checkout. The new
    `amd64_direct_call_stackmap.ml` test covers the current AMD64 failure.
  - Added and validated two more focused AMD64/Linux LLVM-backend native tests
    using normal Make/Dune parallelism, with `LIST=` and `DIR=` cleared because
    `agent-tmp-env` sets a broad `LIST`:
    - `make llvm-test-one TEST=llvm-codegen/amd64_exceptions.ml LIST= DIR=
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 4 passed,
      0 skipped, 0 failed.
    - `make llvm-test-one TEST=llvm-codegen/amd64_stack_growth.ml LIST= DIR=
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 4 passed,
      0 skipped, 0 failed.
  - Added and validated
    `testsuite/tests/llvm-codegen/amd64_core_ops.ml`:
    `make llvm-test-one TEST=llvm-codegen/amd64_core_ops.ml LIST= DIR=
    prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 4 passed,
    0 skipped, 0 failed.
  - Directory-level focused validation now passes for enabled LLVM-codegen
    tests on Linux/AMD64:
    `make llvm-test-one DIR=llvm-codegen LIST= TEST=
    prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 16 passed,
    15 skipped, 0 failed.
  - Started `llvm-self-stage-install` validation. Without extra stack settings,
    the LLVM-built `_install/bin/ocamlc.opt` reliably overflows while inferring
    the interface of `parser__mock.ml.mock`:
    `OCAMLLIB=$(pwd)/_install/lib/ocaml OCAMLRUNPARAM=b
    _install/bin/ocamlc.opt ... -short-paths -i -impl
    _build/main/parser__mock.ml.mock` exits 2 with `Stack overflow`.
  - A smaller stack-usage reproducer shows that a normal opam native compiler
    handles 50,000 non-tail recursive calls with the default stack, while an
    LLVM-built executable raises `Stack_overflow` unless run with
    `OCAMLRUNPARAM=b,Xmain_stack_size=64M`. This is recorded as an AMD64 LLVM
    stack-usage/stack-growth follow-up, not treated as fixed.
  - With `OCAMLRUNPARAM=b,Xmain_stack_size=64M`, the self-stage boot-context
    build gets past the parser mock overflow and the previous
    `CamlinternalQuote` duplicate-interface failure. The boot build prints:
    `boot wrapper lines: 1678` and `boot fresh ir: 831`, confirming the
    LLVM-backed boot compiler build is exercising the wrapper.
  - The next self-stage blocker was the boot compiler smoke:
    `_llvm_boot_context_build/default/boot_ocamlopt.exe` segfaulted compiling a
    tiny recursive program. Focused repro:
    `OCAMLLIB=$(pwd)/_install/lib/ocaml
    _llvm_boot_context_build/default/boot_ocamlopt.exe -c /tmp/rec_simple.ml`
    where `/tmp/rec_simple.ml` contains
    `let rec f n = if n = 0 then 0 else f (n - 1)`.
    Empty, constant-only, and `Printf.printf`-only files compiled; recursive
    files (`rec_simple.ml` and the self-stage `fib` smoke) segfaulted.
  - GDB on the `fib` smoke segfault shows a bad indirect call in `caml_apply2`.
    The backtrace reaches
    `camlFlambda2_algorithms__Table_by_int_id__add_7_19_code` and the closure
    argument to `caml_apply2` is the code symbol `caml_curry2` itself rather
    than a heap closure whose first word is `caml_curry2`.
  - Diagnosed that boot compiler segfault as two AMD64 LLVM issues:
    - X86 LLVM did not reserve `%r14`/`%r15` for OxCaml calling conventions, so
      LLVM could allocate ordinary values into `%r15`, which is the AMD64
      allocation-pointer runtime register.
    - The X86_64 `Pushtrap` exception recovery block can be entered directly by
      the runtime through a stored `blockaddress`, bypassing the normal LLVM CFG
      predecessor. Values kept only in registers set up on the normal edge were
      therefore stale on real exception entry.
  - Implemented fixes:
    - `vendor/llvm-project/llvm/lib/Target/X86/X86RegisterInfo.cpp` reserves
      all aliases of `%r14` and `%r15` for OxCaml calling conventions, matching
      the existing AArch64 reservation pattern for runtime registers.
    - `backend/llvm/llvmize.ml` emits a side-effect inline-asm clobber barrier
      on X86_64 `Pushtrap` exception entry after reading the exception bucket
      from `%rax`. The barrier clobbers non-runtime OCaml GPRs, XMM registers,
      and memory, so LLVM cannot preserve handler values only in registers that
      the runtime direct edge did not establish.
  - Rebuilt agent-local LLVM after the X86 register-reservation patch:
    `cmake --build /tmp/oxcaml-agent-llvm-amd64-support/llvm-build --target
    llc opt -- -j8` exits 0.
  - Rebuilt the installed compiler with the patched `llvmize.ml` using the
    opam boot compiler and writable prefix:
    `OCAMLRUNPARAM=b,Xmain_stack_size=64M make llvm-install
    LLVM_BOOT_BACKEND=0 ARCH=amd64 LLVM_PATH="$LLVM_PATH"
    prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0.
  - The previous focused boot-compiler segfault repros now pass:
    `OCAMLLIB=$(pwd)/_install/lib/ocaml
    _llvm_boot_context_build/default/boot_ocamlopt.exe -c /tmp/const_add.ml`
    and the same command for `/tmp/rec_simple.ml` both exit 0.
  - Installed-compiler boot-context validation now passes with normal Dune
    parallelism and no `-j1`:
    `ARCH=amd64 LLVM_WRAPPER="$LLVM_PATH"
    LLVM_WRAPPER_LOG="$LLVM_WRAPPER_LOG"
    OCAMLRUNPARAM=b,Xmain_stack_size=64M RUN_SMOKE=1
    tools/build-llvm-boot-with-installed.sh` exits 0. The final run prints
    `boot wrapper lines: 1678`, `boot fresh ir: 836`, smoke output `55`,
    `smoke wrapper lines: 4`, and `smoke fresh ir: 2`.
  - `make llvm-test-one DIR=llvm-codegen LIST= TEST= ARCH=amd64
    LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
    prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 16 passed,
    15 skipped, 0 failed. `LLVM_BOOT_BACKEND=0` avoids the Makefile boot path's
    stale `CamlinternalQuote` detection issue; the test compiler itself remains
    LLVM-backed through `LLVM_BACKEND=1`.
  - Direct self-stage install validation now passes with the patched AMD64 LLVM
    backend:
    `ARCH=amd64 LLVM_WRAPPER="$LLVM_PATH"
    LLVM_WRAPPER_LOG="$LLVM_WRAPPER_LOG"
    OCAMLRUNPARAM=b,Xmain_stack_size=64M
    tools/build-llvm-self-stage-install.sh` exits 0. The run prints boot counts
    `boot wrapper lines: 1678`, `boot fresh ir: 829`; runtime counts
    `runtime wrapper lines: 148`, `runtime fresh ir: 74`; main counts
    `main wrapper lines: 2224`, `main fresh ir: 1105`; and the self-stage smoke
    output `55` with `self-stage-smoke wrapper lines: 4` and
    `self-stage-smoke fresh ir: 2`.
  - Fixed `tools/setup-llvm-stage-install.sh` so wrapped bytecode tools in
    `_llvm_self_stage_install/bin` execute with the staged `ocamlrun` instead
    of their original `/usr/local/bin/ocamlrun` shebang. The wrappers also add
    staged `stublibs` directories to `CAML_LD_LIBRARY_PATH`. After regenerating
    the stage install layout, `_llvm_self_stage_install/bin/ocamlc.byte
    -version`, `ocamlopt.byte -version`, and `ocamlopt.opt -version` all print
    `5.2.0+ox`.
  - Full self-stage ocamltest now runs to completion rather than failing during
    setup:
    `SELF_STAGE=1 ARCH=amd64 LLVM_WRAPPER="$LLVM_PATH"
    LLVM_WRAPPER_LOG="$LLVM_WRAPPER_LOG"
    OCAMLRUNPARAM=b,Xmain_stack_size=64M
    tools/run-llvm-stage5-ocamltest.sh` exits 2 with 6594 passed, 269 skipped,
    61 failed, 0 unexpected errors, `wrapper lines: 5942`, and
    `fresh ir: 2971`. GNU parallel was unavailable, so the testsuite fallback
    was serial; no build was forced to `-j1`.
  - Main failure families from that full self-stage run:
    - Native async exception, backtrace, frame-pointer, and CFI stepping output
      mismatches.
    - Native atomic/cmpxchg and or-null atomic assertion failures, plus native
      segfaults in `lib-domain/cpu_relax.ml` and `statmemprof/bigarray.ml`.
    - Missing AMD64 SIMD builtin recognition for operations such as
      `caml_simd_int64x2_sub` and
      `caml_simd_vec128_interleave_high_64`, affecting mixed blocks, records and
      block indices, product arrays, and vector-array tests.
    - `Llvmize: unimplemented instruction: probe` in
      `typing-layouts-or-null/probe.ml`.
    - `misc/gctweaks.ml` fails in both bytecode and native under this
      self-stage test environment, so it is not clearly LLVM-native-specific.
  - Implemented the first AMD64 SIMD LLVM-backend lowering tranche and rebuilt
    with normal Make/Dune parallelism:
    `OCAMLRUNPARAM=b,Xmain_stack_size=64M make llvm-install
    LLVM_BOOT_BACKEND=0 ARCH=amd64 LLVM_PATH="$LLVM_PATH"
    prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0.
  - Focused SIMD/layout validation now passes with `LIST=` and `DIR=` cleared:
    - `make llvm-test-one
      TEST=typing-layouts-block-indices/block_indices_native.ml LIST= DIR=
      ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 2 passed,
      0 skipped, 0 failed.
    - `make llvm-test-one
      TEST=records-and-block-indices/generated_record_access_native_test.ml
      LIST= DIR= ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 3 passed,
      0 skipped, 0 failed.
    - `make llvm-test-one
      TEST=typing-layouts-arrays/test_vec128_u_array.ml LIST= DIR= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 3 passed,
      0 skipped, 0 failed.
  - Diagnosed the remaining
    `mixed-blocks/generated_native_test.ml` failure. The target function
    `camlGenerated_native_test__create_int64x4_12_54_code` has wide vector
    stack slots; LLVM's default `<4 x i64>` alloca alignment triggered X86
    stack realignment, which then selected the X86 base pointer for stackmap
    frame-index operands and hit
    `X86RegisterInfo::eliminateFrameIndex`'s `BasePtr == FramePtr` assertion.
    A direct saved-IR experiment adding `align 16` to wide vector allocas made
    the agent-local patched `llc` compile the file successfully.
  - After emitting `align 16` for `Vec256`/`Vec512` register spill allocas,
    focused validation now passes with normal Make/Dune parallelism:
    - `OCAMLRUNPARAM=b,Xmain_stack_size=64M make llvm-install
      LLVM_BOOT_BACKEND=0 ARCH=amd64 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0.
    - `make llvm-test-one TEST=mixed-blocks/generated_native_test.ml LIST= DIR=
      ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 3 passed,
      1 skipped, 0 failed.
    - `make llvm-test-one
      TEST=typing-layouts-arrays/test_vec128_u_array.ml LIST= DIR= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 3 passed,
      0 skipped, 0 failed.
    - `make llvm-test-one DIR=llvm-codegen LIST= TEST= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 16 passed,
      15 skipped, 0 failed.

## Current Blocker

No focused blocker remains for `llvm-install` with a writable prefix, the
enabled Linux/AMD64 `llvm-codegen` tests, or self-stage install and smoke. The
default `/usr/local` install prefix is not writable in this environment, so use
an explicit agent-local `prefix=...` for install validation. Also clear
`LIST=`/`DIR=` when running a single `TEST=...` after
`eval "$(../../../scripts/agent-tmp-env)"`, because the agent env may set
`LIST` for broader test runs.

Full self-stage ocamltest is now the broad validation blocker: it completed with
61 failures before the focused SIMD and wide-vector alloca fixes, so it should
be rerun to refresh the failure list. The known stack-usage issue remains: some
LLVM-built compiler paths need
`OCAMLRUNPARAM=b,Xmain_stack_size=64M` where the normal opam compiler does not.

## Next Step

Rerun broad self-stage ocamltest or a representative subset to refresh the
remaining failure families after the SIMD and wide-vector alloca fixes. Likely
next targets from the stale full run are native atomic/cmpxchg failures, native
async/backtrace/CFI output mismatches, and
`Llvmize: unimplemented instruction: probe` in
`typing-layouts-or-null/probe.ml`. Keep using normal build parallelism; avoid
only concurrent top-level `make`/`dune` commands in this checkout because of the
shared lockfile.
