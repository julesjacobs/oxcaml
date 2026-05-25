# Progress

Last updated: 2026-05-25.

## Current Claim

AMD64 LLVM backend bring-up now passes focused install, enabled Linux/AMD64
`llvm-codegen`, installed-compiler boot-smoke validation, and a full AMD64
LLVM self-stage ocamltest refresh with the agent-local LLVM tools and writable
install prefix. Focused AMD64 SIMD lowering now also covers the i64x2 add/sub,
vec128 interleave, and vec256 join/split operations needed by several
layout/block-index tests, including the generated mixed-blocks native test on
AMD64. The stale
`typing-layouts-or-null/probe.ml` LLVM lowering failure is also fixed for the
default disabled-probe path. The standard-compiler LLVM-backend
`lib-atomic/test_atomic_cmpxchg.ml` failure is fixed too. The native
`async-exns/async_exns_1.ml` output mismatch is now fixed as well.

The latest fixes publish the current allocation pointer to `%r15` before the
X86_64 `Raise_notrace` inline exception jump, correct LLVM `Compare_exchange`
lowering, and add focused AMD64 probe terminator lowering plus focused AMD64
SIMD LLVM lowering for i64x2 arithmetic, vec128 interleaves, and vec256
join/split support. The poll slow-path frame descriptor gap in
`lib-domain/cpu_relax.ml` is also fixed for AMD64 LLVM output. Plain
debug-less X86_64 raise/reraise calls now emit a standalone frame stackmap,
fixing the standard-compiler LLVM-backend backtrace propagation gap in
`tests/backtrace/backtrace.ml`. AMD64 LLVM frame-pointer builds now request
frame-pointer frames for normal LLVM functions and generated noalloc C-call
wrappers, fixing focused `tests/frame-pointers` stack walks. The generated
AMD64 recover-rbp exception shims now carry CFI, fixing focused native CFI
single-stepping through exception recovery. The AMD64 LLVM path now also
selects and lowers `Cpackf32`, fixing unboxed float32 array native
compilation. `tests/misc/gctweaks.ml` now tolerates pre-existing active GC
tweaks from validation `OCAMLRUNPARAM`, clearing the last known focused
failure from the previous self-stage ocamltest run. Earlier fixes reserve the
AMD64 OxCaml runtime registers in LLVM's X86 register allocator and make
exception-recovery blocks treat runtime blockaddress entry as a
register-clobbering edge. The newest X86_64 `Pushtrap` fix now refreshes the
LLVM runtime-register spill slots after the direct exception-runtime entry
returns to the exception edge, preventing optimized IR from reloading stale
domain-state/allocation-pointer values on handler entry.

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
    descriptors for non-tail indirect OCaml calls.
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
  - `backend/llvm/llvmize.ml` lowers `Probe` terminators enough for AMD64 LLVM
    bring-up: default disabled probes fall through, while `enabled_at_init`
    probes are emitted as ordinary direct OCaml calls to their generated
    handler before branching to the continuation.
  - `backend/llvm/llvmize.ml` now stores LLVM `cmpxchg`'s loaded value for
    `Compare_exchange`. LLVM returns the old memory value in both success and
    failure cases, which is exactly the OCaml `Atomic.compare_exchange` result;
    the previous lowering incorrectly selected the destination register's old
    contents on success.
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
  - `backend/llvm/llvmize.ml` now writes the current LLVM allocation pointer
    slot back to the AMD64 allocation-pointer register `%r15` before the
    X86_64 `Raise_notrace` inline jump to an exception handler. This prevents
    a handler-allocated exception bucket from being overwritten by later
    allocations in the catching handler.
  - `backend/llvm/llvmize.ml` now lets raise/reraise calls request an explicit
    standalone X86_64 stackmap when the call has no deopt or GC-live operand
    bundle. This records the otherwise-missing frame descriptor for debug-less
    unmatched-handler propagation paths, while leaving existing debug-bearing
    raise descriptors to the statepoint/deopt path.
  - `backend/llvm/llvmize.ml` now adds LLVM `frame-pointer="all"` to AMD64
    functions when OxCaml is configured with frame pointers. This keeps the
    `%rbp` chain visible for normal LLVM-emitted OCaml frames.
  - `backend/llvm/llvmize.ml` also adds LLVM `frame-pointer="all"` to
    generated noalloc C-call wrappers on AMD64 frame-pointer builds. Those
    wrappers switch stacks before calling C, so they need an explicit wrapper
    frame for C-side `%rbp` stack walkers to report the OCaml caller frame.
  - `backend/llvm/llvmize.ml` now emits `.cfi_startproc`/`.cfi_endproc` and
    CFA rules for generated X86_64 `recover_rbp_asm` labels. The entry rule
    computes the CFA from the saved `%rbp` still on the trap block; after
    `pop %rbp`, the rule switches to the normal `%rbp + 16` frame-pointer
    CFA. This lets GDB unwind through each instruction in the transient
    exception-recovery shim.
  - `backend/amd64/cfg_selection.ml` now selects `Cpackf32` to `Ipackf32` even
    on the LLVM-backend path, and `backend/llvm/llvmize.ml` lowers `Ipackf32`
    by combining the low 32 bits of two float32 bit-pattern carriers into one
    64-bit payload word. This fixes the previous
    `Selection.select_oper` failure for unboxed float32 array payloads.
  - `testsuite/tests/misc/gctweaks.ml` now records the initially active GC
    tweaks and verifies that `custom_work_max_multiplier` is added and removed
    relative to that baseline. This keeps the test valid when AMD64 validation
    runs with `OCAMLRUNPARAM=b,Xmain_stack_size=64M`, which makes
    `main_stack_size` appear in `Gc.Tweak.list_active ()`.
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
  - `testsuite/tests/llvm-codegen/amd64_raise_notrace_alloc.ml` is an
    AMD64/Linux LLVM-backend regression test for the `Raise_notrace` path that
    allocates an exception bucket in one handler and allocates again before the
    outer handler reads that bucket.
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
  - Updated `GOAL.md` so the focused iteration example no longer recommends
    `DUNE_BUILD_FLAGS=-j1`; normal internal build parallelism is expected.
  - `ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
    make llvm-test-one TEST=backtrace/backtrace.ml` now passes with the
    standard compiler LLVM backend: 2 tests passed, 0 failed. Before the
    debug-less raise/reraise stackmap fix, the native run omitted the
    `Called from Backtrace in file "backtrace.ml", line 21, characters 9-25`
    frame for the uncaught `Error "d"` propagation case.
  - Follow-up standard-compiler LLVM-backend backtrace validations also pass:
    `make test-one-no-rebuild TEST=backtrace/backtrace_deprecated.ml`,
    `make test-one-no-rebuild TEST=backtrace/backtrace_slots.ml`, and
    `make test-one-no-rebuild TEST=backtrace/raw_backtrace.ml` each report
    2 tests passed, 0 failed.
  - After the frame-pointer attribute fixes, broader focused backtrace
    validation also passes:
    `make test-one-no-rebuild DIR=backtrace LLVM_BACKEND=1 ARCH=amd64
    LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
    prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 67 passed,
    6 skipped, 0 failed.
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
  - Focused validation of the stale probe lowering failure now passes with
    normal Make/Dune parallelism:
    `make llvm-test-one TEST=typing-layouts-or-null/probe.ml LIST= DIR=
    ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
    prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 5 passed,
    0 skipped, 0 failed.
  - Focused validation of the stale atomic compare-exchange failure now passes
    with normal Make/Dune parallelism:
    - `make llvm-test-one TEST=lib-atomic/test_atomic_cmpxchg.ml LIST= DIR=
      ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 2 passed,
      0 skipped, 0 failed.
    - `make llvm-test-one DIR=lib-atomic LIST= TEST= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 5 passed,
      1 skipped, 0 failed.
  - Diagnosed the current `lib-domain/cpu_relax.ml` native segfault as a
    missing frame descriptor for the AMD64 poll slow-path return address after
    `call caml_call_gc`. The ordinary poll call carried a poll statepoint ID,
    but did not produce a standalone X86 frame-table descriptor at the return
    address used by `current_frame_alloc_wosize`.
  - `backend/llvm/llvmize.ml` now emits an explicit poll stackmap after X86_64
    `caml_call_gc` poll calls, using the same poll statepoint ID and live GC
    root bundle as the call. This gives the X86 asm-printer label handoff a
    concrete stackmap to record at the call return address.
  - Focused poll validation now passes with normal Make/Dune parallelism:
    - `make llvm-install ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0.
    - `make llvm-test-one TEST=lib-domain/cpu_relax.ml LIST= DIR= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 3 passed,
      0 skipped, 0 failed.
    - A direct retained-IR compile of `testsuite/tests/lib-domain/cpu_relax.ml`
      with `/tmp/oxcaml-agent-llvm-amd64-support/install/bin/ocamlopt.opt
      -llvm-backend -llvm-path "$LLVM_PATH" -nostdlib ... unix.cmxa
      threads.cmxa` exits 0, and the generated assembly contains
      `camlCpu_relax__frametable: .quad 9` with poll entries for the
      `caml_call_gc` return labels.
    - Forced-poll gdb validation of that direct binary sets the saved domain
      young limit equal to the saved allocation pointer at
      `camlCpu_relax__entry+0xf0`; the program exits normally and gdb reports
      `No stack` instead of the previous `frame_end_of_live_ofs(d=0x0)` crash.
    - `make llvm-test-one DIR=lib-domain LIST= TEST= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 21 passed,
      4 skipped, 0 failed.
  - The stale native `statmemprof/bigarray.ml` segfault no longer reproduces
    after the current fixes:
    `make llvm-test-one TEST=statmemprof/bigarray.ml LIST= DIR= ARCH=amd64
    LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
    prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 3 passed,
    0 skipped, 0 failed.
  - Diagnosed the native `async_exns_1.ml` output mismatch as stale `%r15` on
    the X86_64 `Raise_notrace` path. GDB showed the inner handler built a
    correct `Ok "OK"` exception bucket, but the outer handler entered with
    `%r15` still pointing above that freshly allocated bucket. The next
    allocation in `Printf` reused the bucket storage and changed the payload to
    `"1. "`, producing `1. 1.`.
  - `backend/llvm/llvmize.ml` now reloads the current LLVM allocation pointer
    slot and writes it to `%r15` before the X86_64 `Raise_notrace` inline jump.
  - Added `testsuite/tests/llvm-codegen/amd64_raise_notrace_alloc.ml` as a
    focused AMD64/Linux LLVM-backend regression for that stale-allocation-
    pointer exception path.
  - Focused async and exception validation now passes with normal Make/Dune
    parallelism:
    - `make llvm-install ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0.
    - `make llvm-test-one DIR=async-exns LIST= TEST= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 5 passed,
      0 skipped, 0 failed.
    - `make llvm-test-one DIR=exception-extra-args LIST= TEST= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 6 passed,
      0 skipped, 0 failed.
    - `make llvm-test-one
      TEST=llvm-codegen/amd64_raise_notrace_alloc.ml LIST= DIR= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 4 passed,
      0 skipped, 0 failed.
    - `make llvm-test-one DIR=llvm-codegen LIST= TEST= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 20 passed,
      15 skipped, 0 failed.
  - Rebuilt the current AMD64 self-stage install after the async exception fix:
    `tools/build-llvm-self-stage-install.sh` exits 0 with
    `OCAMLRUNPARAM='b,Xmain_stack_size=64M'`. It reported boot wrapper/fresh IR
    counts of 1678/832, runtime counts of 148/73, main counts of 2224/1106,
    and both the stage0 and self-stage smoke tests printed `55`.
  - Full self-stage ocamltest refresh completed with normal build parallelism
    and no concurrent top-level Make/Dune command:
    `SELF_STAGE=1 ARCH=amd64 LLVM_WRAPPER="$LLVM_PATH"
    LLVM_WRAPPER_LOG="$LLVM_WRAPPER_LOG" OCAMLRUNPARAM='b,Xmain_stack_size=64M'
    tools/run-llvm-stage5-ocamltest.sh` exited 2 after 6331 passed, 269
    skipped, and 15 failed. GNU parallel was unavailable, so the script ran the
    testsuite serially. Wrapper counts: 6190 lines, 3095 fresh IR files.
  - The full self-stage refresh confirmed the focused async fix:
    `tests/async-exns` passed, and
    `tests/llvm-codegen/amd64_raise_notrace_alloc.ml` passed in the broad run.
  - Focused frame-pointer validation now passes with the standard compiler
    LLVM backend:
    - `make llvm-test-one TEST=frame-pointers/c_call.ml LIST= DIR= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 3 passed,
      0 skipped, 0 failed.
    - `make llvm-test-one TEST=frame-pointers/stack_realloc2.ml LIST= DIR=
      ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 3 passed,
      0 skipped, 0 failed.
    - `make test-one-no-rebuild DIR=frame-pointers LLVM_BACKEND=1 ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 21 passed,
      0 skipped, 0 failed.
  - Focused native CFI stepping validation now passes with the standard
    compiler LLVM backend. Before the recover-rbp CFI fix, GDB reported
    "Backtrace failed" while single-stepping through `caml_raise_exn` and the
    generated `recover_rbp_asm` block.
    - `make llvm-test-one TEST=native-cfi-stepping/test_cfi.ml LIST= DIR=
      ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 6 passed,
      0 skipped, 0 failed.
    - `make test-one-no-rebuild DIR=native-cfi-stepping LLVM_BACKEND=1
      ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 6 passed,
      0 skipped, 0 failed.
  - Focused unboxed-float32-array validation now passes with the standard
    compiler LLVM backend:
    - `make llvm-test-one
      TEST=typing-layouts-arrays/test_float32_u_array.ml LIST= DIR= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 5 passed,
      0 skipped, 0 failed. Before the `Cpackf32`/`Ipackf32` fix, both native
      variants failed during compilation with `Fatal error:
      Selection.select_oper`.
    - `make test-one-no-rebuild DIR=typing-layouts-arrays LLVM_BACKEND=1
      ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 134
      passed, 0 skipped, 0 failed.
  - Focused `gctweaks.ml` validation now passes:
    - With AMD64 validation stack settings:
      `OCAMLRUNPARAM='b,Xmain_stack_size=64M' make llvm-test-one
      TEST=misc/gctweaks.ml LIST= DIR= ARCH=amd64 LLVM_BOOT_BACKEND=0
      LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 3 passed,
      0 skipped, 0 failed. Before the test fix, both bytecode and native
      failed after printing `100` because `Gc.Tweak.list_active ()` also
      contained `main_stack_size`.
    - Without `OCAMLRUNPARAM`:
      `make test-one-no-rebuild TEST=misc/gctweaks.ml LLVM_BACKEND=1
      ARCH=amd64 LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 3 passed,
      0 skipped, 0 failed.
  - Rebuilt the current AMD64 self-stage install after the frame-pointer,
    native CFI, float32 array, and gctweaks fixes:
    `tools/build-llvm-self-stage-install.sh` exits 0 with
    `OCAMLRUNPARAM='b,Xmain_stack_size=64M'`. It reported boot wrapper/fresh IR
    counts of 1678/827, runtime counts of 148/74, main counts of 2224/1105,
    and both the stage0 and self-stage smoke tests printed `55`.
  - Full self-stage ocamltest refresh now passes with normal build
    parallelism and no concurrent top-level Make/Dune command:
    `SELF_STAGE=1 ARCH=amd64 LLVM_WRAPPER="$LLVM_PATH"
    LLVM_WRAPPER_LOG="$LLVM_WRAPPER_LOG" OCAMLRUNPARAM='b,Xmain_stack_size=64M'
    tools/run-llvm-stage5-ocamltest.sh` exited 0 after 6346 passed, 269
    skipped, 0 failed, 0 not started, and 0 unexpected errors. GNU parallel was
    unavailable, so the script ran the testsuite serially. Wrapper counts:
    6198 lines, 3099 fresh IR files.
  - The broad self-stage refresh confirmed the previously failing families now
    pass in context: `tests/backtrace`, `tests/frame-pointers`,
    `tests/native-cfi-stepping`, `tests/typing-layouts-arrays` including
    `test_float32_u_array.ml`, `tests/misc/gctweaks.ml`, and the AMD64
    `llvm-codegen` regressions all passed.
  - PR-readiness cleanup: removed the large commented-out ARM64
    `specific`-lowering reference block from `backend/llvm/llvmize.ml`. This
    does not change AMD64 generated code, but it removes review noise from the
    AMD64 branch.
  - Replaced the compatibility-only ARM64 compile fix with an arch-specific
    `llvmize_specific` classifier copied through `backend/%{env:ARCH}`:
    - ARM64 `Arch.specific_operation` is back to its native constructor set;
      shared `backend/llvm/llvmize.ml` no longer depends on AMD64 constructors
      being present in the ARM64 `Arch` module.
    - AMD64-specific SIMD decoding now happens in `backend/amd64`, so
      `llvmize.ml` consumes an `Amd64_simd_instrs.id` plus immediate instead of
      inspecting the AMD64 `Simd.operation` record directly.
    - `ARCH=arm64 RUNTIME_DIR=runtime dune build ocamloptcomp.cma` exits 0.
    - `ARCH=amd64 RUNTIME_DIR=runtime dune build ocamloptcomp.cma` exits 0.
    - A direct AMD64 `-llvm-backend -llvm-path "$LLVM_PATH"` compile/link/run
      smoke with the freshly built compiler prints `42`.
    - A direct AMD64 `-llvm-backend -S -c` compile of
      `testsuite/tests/typing-layouts-block-indices/block_indices_native.ml`
      exits 0, exercising the SIMD lowering path that uses the AMD64
      `Isimd` record layout.
    - Follow-up wrapped the new classifier operation type declarations to stay
      under 80 columns; `git diff --check` is clean and both `ARCH=arm64` and
      `ARCH=amd64` `dune build ocamloptcomp.cma` checks still exit 0.
  - Installed `ocamlformat.0.29.0` into the agent opam switch and formatted the
    branch-touched OCaml files. This cleaned the PR-specific `@fmt` output for
    `backend/amd64/cfg_selection.ml` and `backend/llvm/llvmize.ml`.
    `dune build @fmt` still reports pre-existing integration-branch diffs in
    files this branch did not touch, such as `asmcomp/asmgen.ml` and
    `oxcaml/tests/backend/llvmize/*`; those were intentionally left alone.
  - Rechecked after formatting:
    - `git diff --check` exits 0.
    - `ARCH=arm64 RUNTIME_DIR=runtime dune build ocamloptcomp.cma` exits 0.
    - `ARCH=amd64 RUNTIME_DIR=runtime dune build ocamloptcomp.cma` exits 0.
    - With a local `main` branch tracking `oxcaml/main`, `scripts/80ch.sh`
      still reports inherited vendored LLVM/expectcommon long lines when run
      locally against upstream `main`, but no longer reports the branch-touched
      `backend/llvm/llvmize.ml` lines after running through the opam switch.
  - Restored the integration branch's explicit ARM64 LLVM `specific` lowering
    behind the `Llvmize_specific` classifier instead of matching directly on
    ARM64 `Arch`/`Simd` constructors in shared `llvmize.ml`:
    - `backend/arm64/llvmize_specific.ml` now classifies the ARM64 SIMD subset
      that the integration branch lowered into architecture-neutral
      `arm64_simd_operation` constructors.
    - `backend/amd64/llvmize_specific.ml` defines the same neutral operation
      type so shared `backend/llvm/llvmize.ml` compiles under `ARCH=amd64`;
      AMD64 classification still never constructs those ARM64 cases.
    - Shared `backend/llvm/llvmize.ml` now lowers the ARM64 shift/add,
      multiply-add/subtract, sign/zero-extension, float fused-operation,
      `sqrt`, and restored SIMD operation subset through those neutral
      classifier constructors. Unsupported ARM64 SIMD operations still flow to
      the existing `not_implemented_basic` fallback, matching the old lowering
      coverage rather than adding new ARM64 SIMD support.
  - Rechecked after restoring ARM64 lowering:
    - `ARCH=arm64 RUNTIME_DIR=runtime dune build ocamloptcomp.cma` exits 0.
    - `ARCH=amd64 RUNTIME_DIR=runtime dune build ocamloptcomp.cma` exits 0.
    - `git diff --check` exits 0.
    - `ocamlformat --check` exits 0 for
      `backend/llvm/llvmize.ml`,
      `backend/arm64/llvmize_specific.ml`, and
      `backend/amd64/llvmize_specific.ml`.
    - `make llvm-install ARCH=amd64 LLVM_BOOT_BACKEND=0
      LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0 with normal
      build parallelism.
    - The freshly rebuilt installed compiler with
      `OCAMLLIB=/tmp/oxcaml-agent-llvm-amd64-support/install/lib/ocaml`
      compiles and runs a direct AMD64 `-llvm-backend -llvm-path "$LLVM_PATH"`
      smoke that prints `42`.
    - The freshly rebuilt installed compiler compiles
      `testsuite/tests/typing-layouts-block-indices/block_indices_native.ml`
      with `-llvm-backend -llvm-path "$LLVM_PATH" -S -c`, exercising the AMD64
      SIMD classifier/lowering path.
    - `make llvm-test-one DIR=llvm-codegen LIST= TEST= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0: 20 passed,
      15 skipped, 0 failed.
  - Follow-up cleanup moved the neutral ARM64 SIMD operation type family from
    duplicated definitions in `backend/{amd64,arm64}/llvmize_specific.ml` to
    `backend/llvm/llvmize_specific_types.ml`. Both arch-specific classifiers
    now open that shared type module, and `backend/llvm/llvmize.ml` lowers
    those constructors directly. This keeps the architecture boundary explicit
    while avoiding two large copies of the same neutral type.
    - `ARCH=arm64 RUNTIME_DIR=runtime dune build ocamloptcomp.cma` exits 0.
    - `ARCH=amd64 RUNTIME_DIR=runtime dune build ocamloptcomp.cma` exits 0.
    - `git diff --check` exits 0.
    - `ocamlformat --check` exits 0 for
      `backend/llvm/llvmize.ml`,
      `backend/llvm/llvmize_specific_types.ml`,
      `backend/arm64/llvmize_specific.ml`, and
      `backend/amd64/llvmize_specific.ml`.
    - `make llvm-test-one DIR=llvm-codegen LIST= TEST= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` exits 0 with normal
      build parallelism after the shared-type cleanup: 20 passed, 15 skipped,
      0 failed.
  - Local validation wrapper audit:
    - The agent-local `/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper`
      was a manual `opt`/`llc` substitute for clang because the local patched
      LLVM build contains `opt` and `llc`, not clang. Its `-x ir -S` path had
      been feeding raw LLVM IR directly to `llc`, unlike `clang -O3`, while
      only the `-emit-llvm` path ran `opt -O3`.
    - Updated that local wrapper so `-x ir -S` first runs `opt -O3 -S` to a
      temporary IR file, then runs `llc -O3 -filetype=asm
      -mtriple=x86_64-unknown-linux-gnu` on the optimized IR. The explicit
      target triple is needed because optimized stackmap operands can require
      OxCaml GC-printer DWARF register mapping.
    - With the corrected wrapper, the small non-tail recursion stack-usage
      check is no longer worse than native in the tested range: the explicit
      standard-compiler AMD64 `-llvm-backend` executable runs 100,000,
      200,000, and 400,000 recursive calls with default runtime settings, and
      overflows at 800,000 like the native-backend executable. The generated
      recursive function no longer has the old raw-IR `subq $80, %rsp` frame;
      direct `llc` on optimized IR and the corrected wrapper both emit just
      the frame-pointer push path for that function.
    - Rechecked `make llvm-test-one DIR=llvm-codegen LIST= TEST= ARCH=amd64
      LLVM_BOOT_BACKEND=0 LLVM_PATH="$LLVM_PATH"
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install` with the corrected
      wrapper: 20 passed, 15 skipped, 0 failed.
    - A fresh default-runtime-stack self-stage attempt in
      `/tmp/oxcaml-agent-llvm-amd64-support/corrected_*` got past the old
      parser-mock overflow only far enough to expose the missing-target-triple
      wrapper issue above. After adding `-mtriple`, the `llc` failure
      disappeared, but the boot build still reported `Stack overflow`, likely
      because the stage0 `_install` compiler being used was itself built with
      the old unoptimized wrapper path. A forced stage0 rebuild using a new
      wrapper path was attempted, but stopped before LLVM codegen on a missing
      `_build/default/duneconf/camlinternalquote_if_missing_from_stdlib`
      generated include in the existing `_build`; retesting self-stage without
      `OCAMLRUNPARAM` needs a refreshed stage0 install.
  - Fixed the optimized-wrapper X86_64 exception regression:
    - A stage0 compiler rebuilt with the corrected optimizing wrapper
      initially segfaulted in generated build tools (`make_opcodes.exe` and
      `simdgen.exe`). GDB showed `caml_c_call` using a stale `%r14` domain
      state register on the `Pushtrap` exception edge, with `%r14 == %r15`.
    - `backend/llvm/llvmize.ml` now mirrors the AArch64 path by reading the
      current X86_64 domain-state and allocation-pointer registers after the
      direct exception-runtime entry returns, then storing them back to
      `domainstate_ptr` and `allocation_ptr` before branching to the handler.
    - `make llvm-install ARCH=amd64 LLVM_BOOT_BACKEND=0
      LLVM_PATH=/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper-opt
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install-opt` exits 0 with
      normal build parallelism.
    - The previously crashing generated tools now run:
      `_build/main/tools/make_opcodes.exe -opcodes < runtime/caml/instruct.h`
      writes 156 lines, and
      `tools/simdgen`-relative
      `../../_build/main/tools/simdgen/simdgen.exe amd64` writes 9,987 lines.
    - Direct installed-compiler smoke with
      `OCAMLLIB=/tmp/oxcaml-agent-llvm-amd64-support/install-opt/lib/ocaml`
      and `-llvm-backend -llvm-path
      /tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper-opt` prints `11`.
    - `make llvm-test-one DIR=llvm-codegen LIST= TEST= ARCH=amd64
      LLVM_BOOT_BACKEND=0
      LLVM_PATH=/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper-opt
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install-opt` exits 0 after
      a clean `_build` refresh: 20 passed, 15 skipped, 0 failed.
    - A default-stack self-stage attempt with that refreshed stage0 still
      fails in the boot-context build compiling `amd64_simd_instrs.ml`:
      `/tmp/oxcaml-agent-llvm-amd64-support/install-opt/bin/ocamlopt.opt`
      reports `Fatal error: exception Stack overflow`. The single compile
      passes with `OCAMLRUNPARAM=b,Xmain_stack_size=64M`.
    - GDB on that single compile shows the overflow in deep Flambda2
      closure-conversion recursion over the generated SIMD table:
      repeated
      `Flambda2_from_lambda__Closure_conversion__cont` frames at
      `closure_conversion.ml:1442`, with the top frame in
      `Lambda_to_flambda_primitives.convert_lprim` at
      `lambda_to_flambda_primitives.ml:1790`. This is the remaining
      default-stack self-stage caveat, not a focused standard `-llvm-backend`
      failure.
  - Investigated but did not commit a GC-root slot pooling experiment:
    - Hypothesis: the default-stack self-stage overflow might be caused by
      function-wide volatile root allocas for every register live across any
      safepoint, inflating frames in LLVM-emitted compiler code.
    - Tried replacing most function-wide preserved root slots with a reusable
      pool sized to the maximum number of roots live at one safepoint, while
      keeping trap-edge-visible roots in their original slots.
    - The experiment built with `make llvm-install ARCH=amd64
      LLVM_BOOT_BACKEND=0
      LLVM_PATH=/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper-opt
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install-opt`, the direct
      installed-compiler smoke printed `11`, and the focused
      `llvm-codegen` suite still passed: 20 passed, 15 skipped, 0 failed.
    - However, the default-stack boot-context build then failed
      deterministically while building
      `middle_end/flambda2/types/traversals.mli` with
      `/tmp/oxcaml-agent-llvm-amd64-support/install-opt/bin/ocamlc.opt`:
      `Fatal error: caml_scan_stack: missing frame descriptor`.
    - The failing action reproduced under Dune but 50 isolated runs of the
      same `ocamlc.opt` command passed. The bad reported return addresses
      mapped into the middle of generated instructions, for example around
      `camlPprintast__signature_item_134_918_code`, which points to corrupted
      stack scanning or root/frame-slot metadata rather than one missing call
      descriptor.
    - Restoring an explicit stackmap after AMD64 raise calls did not fix the
      failure. The root-slot pooling edits were backed out before commit; the
      current worktree is back to the pushed backend code plus this progress
      note.
  - Revalidated the current pushed backend after backing out the root-slot
    pooling experiment:
    - A clean `make llvm-install ARCH=amd64 LLVM_BOOT_BACKEND=0
      LLVM_PATH=/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper-opt
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install-opt` passed with
      normal build parallelism after `make clean`.
    - The refreshed installed compiler smoke with `-llvm-backend` and the
      agent-local wrapper printed `11`.
    - A fresh default-stack boot-context build in
      `/tmp/oxcaml-agent-llvm-amd64-support/current_boot_context_build`
      failed in two actions: `ocamlc.opt` compiling `simd.ml` aborted with
      `caml_scan_stack: missing frame descriptor`, and `ocamlopt.opt`
      compiling `amd64_simd_instrs.ml` reported `Stack overflow`.
    - The missing-frame retaddr mapped to the LLVM-built `ocamlc.opt`
      function `camlMode__zap_to_floor_279_1375_code` at `typing/mode.ml`,
      near ordinary allocation code rather than an obvious call return site.
      A direct rerun of the `simd.ml` byte target then succeeded, so this
      frame-scan abort is recorded as a schedule/retry-sensitive symptom, not
      yet a stable isolated reproducer.
    - A direct rerun of the native `amd64_simd_instrs.ml` target reproduced
      the default-stack overflow, and the same target passed with
      `OCAMLRUNPARAM=b,Xmain_stack_size=64M`.
    - A fresh boot-context build with
      `OCAMLRUNPARAM=b,Xmain_stack_size=64M` and normal build parallelism
      passed with `RUN_SMOKE=0`: 1,678 wrapper invocations and 827 fresh IR
      compilations.

## Current Blocker

No focused blocker remains for `llvm-install` with a writable prefix, the
enabled Linux/AMD64 `llvm-codegen` tests, self-stage install and smoke, or the
full AMD64 LLVM self-stage ocamltest refresh. The default `/usr/local` install
prefix is not writable in this environment, so use an explicit agent-local
`prefix=...` for install validation. Also clear `LIST=`/`DIR=` when running a
single `TEST=...` after `eval "$(../../../scripts/agent-tmp-env)"`, because the
agent env may set `LIST` for broader test runs.

The previous stack-usage caveat is now narrowed. With a corrected local
optimizing wrapper, the standard explicit AMD64 `-llvm-backend` path matches
the native backend on the small non-tail recursion threshold check, and the
focused Linux/AMD64 `llvm-codegen` suite passes with the refreshed stage0
install. Self-stage without `OCAMLRUNPARAM` still overflows while compiling the
generated `amd64_simd_instrs.ml`; the single failing compile passes with
`OCAMLRUNPARAM=b,Xmain_stack_size=64M`, and GDB attributes the default-stack
overflow to deep Flambda2 closure-conversion recursion over that generated
file. A fresh boot-context build with `Xmain_stack_size=64M` now passes. The
latest default-stack boot-context attempt also produced one non-reproduced
`ocamlc.opt` `simd.ml` missing-frame-descriptor abort that should stay on the
watch list while investigating stack pressure and frame metadata.
The OxCaml PR is still draft; as of the latest check after the shared-type
cleanup, GitHub reported `built with flambda-backend, flambda2` passed and the
rest of CI still pending.

## Next Step

Continue investigating the remaining default-stack self-stage overflow without
committing the unsafe reusable-root-slot experiment, and watch for recurrence
of the `simd.ml` byte-compiler frame-scan abort. Keep using normal build
parallelism; avoid only concurrent top-level `make`/`dune` commands in this
checkout because of the shared lockfile.
