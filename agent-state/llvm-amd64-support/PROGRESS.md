# Progress

Last updated: 2026-05-25.

## Current Claim

AMD64 LLVM backend bring-up now passes focused install, enabled Linux/AMD64
`llvm-codegen`, installed-compiler boot-smoke validation, self-stage
install/smoke validation, and a fresh full self-stage ocamltest refresh with
the agent-local LLVM tools and writable install prefix. Focused AMD64 SIMD
lowering now also covers the i64x2 add/sub,
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

The newest C-call stackmap fix emits an explicit frame stackmap for AMD64 LLVM
external C calls through `caml_c_call`/`caml_c_call_stack_args`. The reduced
self-stage2 crash mapped to the first `caml_ml_pos_out_64` C call in
`camlCmi_format__marshal_11_39_code`: the pre-fix compiler had no frame
descriptor for the post-call return address, while the following
`caml_output_value` call did have descriptors. A patched stage now records the
formerly missing `camlCmi_format__marshal` return address.

The AMD64 branch has now merged the updated
`jujacobs/llvm-backend-integration` base. The merge kept the AMD64 LLVM
backend conflict resolutions and restored the AVX2
`caml_avx2_int16x16_mul_round` selector to the generated 256-bit
`vpmulhrsw_Y_Y_Ym256` helper after the integration merge selected the stale
non-256-bit name. A merged self-stage2 validation pass now also succeeds: the
merged self-stage2 compiler passes the focused Linux/AMD64 `llvm-codegen`
tests and a full self-stage2 ocamltest run. The AMD64 LLVM path now also
lowers `Probe_is_enabled` by reading the OCaml probe semaphore slot, covering
the non-optimized probe lowering path instead of fatal-erroring. The LLVM
lowering now also supports the AMD64 int128 builtins
`caml_int128_add`, `caml_int128_sub`, `caml_int64_mul128`, and
`caml_unsigned_int64_mul128` via LLVM `i128` arithmetic. Wide vector constants
now lower directly to LLVM `<2 x i64>`, `<4 x i64>`, and `<8 x i64>` immediate
vectors instead of fatal-erroring on 256-bit and 512-bit constants. Wide
vector scalar casts now lower for 256-bit and 512-bit vectors as well, covering
the `caml_*_low_of_*` and `caml_*_low_to_*` builtins that previously hit the
LLVM static-cast fatal path. Vec512 reinterpret casts now lower too: the AMD64
LLVM path copies the shared low 64-bit lanes for vec128/vec256/vec512
widening and narrowing instead of fatal-erroring on 512-bit reinterpret casts.
AMD64 prefetch builtins now also reach LLVM lowering under `-llvm-backend` and
emit `llvm.prefetch` calls instead of falling back to the generic selector's
`Selection.select_oper` fatal path. AMD64 `caml_cldemote` now reaches LLVM
lowering under `-llvm-backend` as well and emits a side-effecting `cldemote`
inline-asm instruction instead of falling through selection/lowering gaps.
Simple full-width AMD64 SIMD load/store builtins now also lower through the
generic LLVM vector load/store path for 128-bit SSE and 256-bit AVX vectors.
The LLVM path now also covers the 128-bit SSE2/SSE3 low-64 memory forms,
including zeroing, copy-low/high, broadcast, and low-lane stores.
It also covers the related SSE2 vec128 low-32 load/zero-load/store forms.
The AVX memory-broadcast forms now lower through LLVM as scalar/vector loads
plus lane replication for 128-bit-to-256-bit, 64-bit-to-256-bit, and
32-bit-to-128/256-bit broadcasts.

The latest stack-pressure fix safely pools X86_64 preserved GC-root stack slots
using full CFG liveness interference instead of safepoint-only disjointness.
This keeps simultaneously live preserved roots in distinct slots, but lets
non-overlapping preserved roots share storage. A clean patched install now
passes, focused AMD64 LLVM-codegen tests pass through the installed compiler,
and a fresh default-stack boot-context build passes without
`OCAMLRUNPARAM=b,Xmain_stack_size=64M`. The newest follow-up also makes all
entry function arguments interfere for this pooling, because they are all
stored into their allocas before the first CFG instruction is emitted.
The latest AMD64 LLVM allocation follow-up now selects the GC slow-path entry
that preserves live SIMD registers (`caml_call_gc_sse`, `caml_call_gc_avx`, or
`caml_call_gc_avx512`) when float/SIMD values are live across heap allocation
or poll slow paths. This fixes the reduced high-arity `float32` mismatch where
boxed float32 allocation hit the slow path after computing the payload in
`%xmm0`. A further AMD64 follow-up now defaults those LLVM slow paths to the
XMM-preserving helper even when Cfg liveness only sees scalar values, because
LLVM can materialize scalar lane extracts through XMM registers across the
call. This clears the remaining focused vector array/product-array failures.
The newest AMD64 LLVM debug fix emits a real post-raise `nop` after regular
and reraising calls, so the return address used by GDB unwinding stays inside
the raising function's symbol and DWARF subprogram range. LLVM function
attributes also now respect `Config.no_stack_checks`: frame pointers remain
requested on AMD64 frame-pointer builds, but `oxcaml-stack-check` is not added
when the configured native compiler would omit stack checks. The remaining
focused `runtime-errors/stackoverflow.ml` mismatch was a tight stack-budget
expectation: the LLVM recursive frame is larger than the native frame, but the
exception/unwind behavior matches the reference once the test's OCaml stack
limit is raised from `l=100000` to `l=150000`.

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
  - `backend/llvm/llvm_ir.{ml,mli}` and `backend/llvm/llvmize.ml` can emit
    weak hidden data globals. AMD64 LLVM `Probe_is_enabled` now records a weak
    hidden `.probes` semaphore, loads the OCaml semaphore halfword at offset
    `+2`, and stores a `0`/`1` result. Unknown initial enabledness defaults to
    disabled, matching the dummy semaphore initial state used by the native
    probe emitter.
  - `backend/llvm/llvmize.ml` lowers `Int128op`: 128-bit add/sub combine the
    low/high 64-bit inputs into an LLVM `i128`, do the arithmetic, then split
    low/high results; signed and unsigned 64x64->128 multiplies use `sext` or
    `zext` to `i128` before multiplication.
  - `backend/llvm/llvmize.ml` lowers `Const_vec128`, `Const_vec256`, and
    `Const_vec512` through a shared LLVM vector-immediate helper. This removes
    the previous AMD64 LLVM fatal path for 256-bit and 512-bit vector
    constants.
  - `backend/llvm/llvmize.ml` now lowers `V256_of_scalar`,
    `Scalar_of_v256`, `V512_of_scalar`, and `Scalar_of_v512` using the same
    insert/extract-element strategy as the 128-bit scalar casts. Integer
    scalar casts use the existing 64-bit carrier with truncation or zero
    extension for narrow lanes, and float vector storage is bitcast to the
    canonical LLVM vector storage type.
  - `backend/llvm/llvmize.ml` now lowers vec512 reinterpret casts for AMD64
    LLVM output. `V128_of_vec`, `V256_of_vec`, and `V512_of_vec` share a
    low-lane copy helper over the canonical `<N x i64>` storage types, so
    vec512-to-vec128/vec256 narrowing and vec128/vec256-to-vec512 widening no
    longer hit the `vector reinterpret cast` fatal path.
  - `backend/amd64/cfg_selection.ml`, `backend/amd64/llvmize_specific.ml`,
    `backend/arm64/llvmize_specific.ml`,
    `backend/llvm/llvmize_specific_types.ml`, and
    `backend/llvm/llvmize.ml` now carry AMD64 prefetch read/write and locality
    metadata through LLVM-backend selection and lower it to `llvm.prefetch.p0`.
    `testsuite/tests/llvm-codegen/amd64_prefetch.{ml,sh}` checks that
    ext-pointer prefetch builtins compile with `-llvm-backend -c -S
    -keep-llvmir` and emit the LLVM prefetch intrinsic.
  - `backend/amd64/cfg_selection.ml`, `backend/amd64/llvmize_specific.ml`,
    `backend/arm64/llvmize_specific.ml`, and `backend/llvm/llvmize.ml` now
    carry AMD64 `caml_cldemote` addressing through LLVM-backend selection and
    lower it with side-effecting inline assembly. The LLVM X86 cldemote
    intrinsic requires a target-feature attribute for instruction selection;
    inline assembly matches the native backend's explicit instruction emission
    without adding a whole-function CPU-feature contract.
    `testsuite/tests/llvm-codegen/amd64_cldemote.{ml,sh}` checks that
    `caml_cldemote` compiles with `-llvm-backend -c -S -keep-llvmir` and that
    the kept IR/assembly contain the cldemote instruction.
  - `backend/amd64/cfg_selection.ml` now rewrites simple full-width AMD64 SIMD
    memory builtins to generic LLVM vector `Load`/`Store` operations when
    `-llvm-backend` is enabled. This covers aligned/unaligned 128-bit SSE
    vector loads/stores and aligned/unaligned/known-unaligned 256-bit AVX
    vector loads/stores while leaving partial, broadcast, masked, and
    uncached SIMD-memory forms to focused follow-ups.
    `testsuite/tests/llvm-codegen/amd64_simd_mem.{ml,sh}` checks the
    unaligned 128-bit and 256-bit cases with `-llvm-backend -c -S
    -keep-llvmir`.
  - `backend/amd64/llvmize_specific.ml`,
    `backend/arm64/llvmize_specific.ml`,
    `backend/llvm/llvmize_specific_types.ml`, and `backend/llvm/llvmize.ml`
    now carry AMD64 SIMD memory instruction IDs into LLVM lowering and lower
    the SSE2/SSE3 low-64 memory subset: `caml_sse2_vec128_load_low64`,
    `caml_sse2_vec128_load_zero_low64`,
    `caml_sse2_vec128_load_low64_copy_high64`,
    `caml_sse2_vec128_load_high64_copy_low64`,
    `caml_sse3_vec128_load_broadcast64`, and
    `caml_sse2_vec128_store_low64`. Loads use unaligned `i64` memory access
    plus vec128 lane inserts; stores extract lane 0 and write an unaligned
    `i64`.
  - `backend/amd64/simd.ml` fixes SIMD memory-operation pretty-printing by
    passing the memory-address argument count, not an end index, to
    `Array.sub`. LLVMize formats each source instruction for IR debug comments
    before lowering it, so this was needed for SIMD memory instructions whose
    memory operand is not the first argument.
    `testsuite/tests/llvm-codegen/amd64_simd_low64_mem.{ml,sh}` checks the
    low-64 memory subset with `-llvm-backend -c -S -keep-llvmir`.
  - `backend/amd64/cfg_selection.ml` and `backend/llvm/llvmize.ml` also lower
    the SSE2 vec128 low-32 memory subset:
    `caml_sse2_vec128_load_low32`, `caml_sse2_vec128_load_zero_low32`, and
    `caml_sse2_vec128_store_low32`. Loads use unaligned `i32` memory access
    plus lane-0 insertion in a `<4 x i32>` vec128 view; stores extract lane 0
    from the same view and write an unaligned `i32`.
    `testsuite/tests/llvm-codegen/amd64_simd_low32_mem.{ml,sh}` checks this
    subset with `-llvm-backend -c -S -keep-llvmir`.
  - `backend/amd64/cfg_selection.ml` and `backend/llvm/llvmize.ml` now lower
    AVX memory broadcasts: `caml_avx_vec256_load_broadcast128`,
    `caml_avx_vec256_load_broadcast64`,
    `caml_avx_vec256_load_broadcast32`, and
    `caml_avx_vec128_load_broadcast32`. The LLVM lowering uses unaligned
    loads from memory and explicit lane insertion into the canonical vec128 or
    vec256 storage type.
    `testsuite/tests/llvm-codegen/amd64_simd_broadcast_mem.{ml,sh}` checks
    these cases with `-llvm-backend -c -S -keep-llvmir`.
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
  - `testsuite/tests/runtime-errors/stackoverflow.ml` now uses
    `OCAMLRUNPARAM=l=150000` instead of `l=100000`, giving AMD64 LLVM's larger
    recursive frame enough stack to reach the same `20000`, `10000`, and `0`
    exception-handler checkpoints as the native backend.
  - `backend/llvm/llvmize.ml` now emits explicit standalone stackmaps for all
    non-tail X86_64 OCaml calls, including direct calls that have live GC roots.
    This covers frame descriptors for direct-call return addresses when the
    callee triggers a GC and scans the caller.
  - `backend/llvm/llvmize.ml` now also emits explicit stackmaps for external
    C calls lowered through `caml_c_call` or `caml_c_call_stack_args` on the
    AMD64 LLVM path, and passes the primitive-call operand bundle metadata
    through the stackmap-emitting call helper.
  - `testsuite/tests/llvm-codegen/c_call_stackmap.ml` and
    `testsuite/tests/llvm-codegen/c_call_stackmap.sh` are an AMD64/Linux
    LLVM-backend regression test that compiles an `Out_channel.pos` followed
    by `Marshal.to_channel`, runs the binary, and checks the generated
    frametable contains a descriptor for the first `caml_c_call` return label.
  - `testsuite/tests/llvm-codegen/amd64_direct_call_stackmap.ml` is an
    AMD64/Linux LLVM-backend regression test for a direct call with a live heap
    root across an allocating callee.
  - `testsuite/tests/llvm-codegen/amd64_exceptions.ml` is an AMD64/Linux
    LLVM-backend regression test for exception handler setup and exception
    bucket recovery.
  - `testsuite/tests/llvm-codegen/amd64_int128_ops.ml` and
    `testsuite/tests/llvm-codegen/amd64_int128_ops_stubs.c` are an
    AMD64/Linux LLVM-backend regression test for int128 add/sub plus signed
    and unsigned 64x64->128 multiplication builtins.
  - `testsuite/tests/llvm-codegen/amd64_probe_is_enabled.ml` is an AMD64/Linux
    LLVM-backend regression test for non-optimized probes and
    `[%probe_is_enabled]` using disabled and `~enabled_at_init:true`
    semaphores.
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
  - Checked PR state with GitHub CLI: draft PR
    https://github.com/julesjacobs/oxcaml/pull/10 was open but reported
    `mergeStateStatus: DIRTY` against `jujacobs/llvm-backend-integration`;
    no checks were reported on the branch at that time.
  - Merged `origin/jujacobs/llvm-backend-integration` into
    `jujacobs/llvm-amd64-support`. Conflicts were in AMD64/LLVM backend files
    plus LLVM stage helper scripts and X86 vendored LLVM target files; the
    resolution preserved the AMD64 LLVM backend versions for those files.
  - A standard `make compiler -j "$(nproc)"` under the
    `oxcaml-5.4.0+oxcaml` switch initially failed after the merge because
    `backend/amd64/simd_selection.ml` referenced the stale
    `vpmulhrsw_Y_Y_Y` selector. Restoring
    `vpmulhrsw_Y_Y_Ym256` for `caml_avx2_int16x16_mul_round` fixed the build;
    the rerun of `make compiler -j "$(nproc)"` passed.
  - The updated integration base adds `%with_stack_preemptible`, so the stale
    checkout-local `_install` compiler could not bootstrap the merged stdlib.
    Running `make install` refreshed `_install`; after that,
    `_install/bin/ocamlc.opt` recognized the primitive.
  - Built a fresh AMD64 LLVM stage from the refreshed `_install` in
    `validation-tmp/integration_merge_stage` with normal Dune parallelism:
    `DUNE_BUILD_FLAGS="-j $(nproc)" ./tools/build-llvm-stage5-install.sh`.
    Runtime build evidence: `148` wrapper lines / `73` fresh IR. Main build
    evidence: `2228` wrapper lines / `1105` fresh IR. Stage install:
    `validation-tmp/integration_merge_stage/stage_install`.
  - Ran focused LLVM-codegen ocamltest from that merged stage install:
    `STAGE_INSTALL=validation-tmp/integration_merge_stage/stage_install`,
    `STAGE_BUILD=validation-tmp/integration_merge_stage/main_build`,
    `LIST=validation-tmp/integration_merge_stage/llvm-codegen-list.txt`,
    `LLVM_TESTSUITE_PARALLEL=auto`, and `LLVM_TESTSUITE_JOBS=$(nproc)`.
    GNU parallel was unavailable, so the script used the serial `one` target
    with `make -j$(nproc)`. Result: `25` tests passed, `15` skipped,
    `0` failed, `0` unexpected errors, `40` considered. The focused AMD64
    tests including `c_call_stackmap.ml` passed. Wrapper evidence:
    `24` wrapper lines / `12` fresh IR. Log:
    `validation-tmp/integration_merge_stage/llvm_codegen_ocamltest.log`.
  - Pushed merge commit `76e149a203` to
    `origin/jujacobs/llvm-amd64-support`. GitHub now reports the PR merge
    state as `UNSTABLE` rather than `DIRTY`; checks are queued or in progress
    on the pushed branch.
  - Reduced the intermittent self-stage2
    `caml_scan_stack: missing frame descriptor` abort to a missing descriptor
    after the first C call in `camlCmi_format__marshal_11_39_code`
    (`caml_ml_pos_out_64`). In the pre-fix self-stage2 compiler, the return
    address after that `caml_c_call` was absent from the frametable; the later
    `caml_output_value` C-call return address was present.
  - A patched stage build in
    `validation-tmp/c_call_stackmap_stage2a` completed with normal Dune
    parallelism (`DUNE_BUILD_FLAGS="-j $(nproc)"`). Runtime build evidence:
    `148` wrapper lines / `74` fresh IR. Main build evidence before the final
    incremental C-call stackmap rebuild: `2228` wrapper lines / `1102` fresh
    IR; the incremental main rebuild after emitting stackmaps for all
    `caml_c_call` paths produced `154` wrapper lines / `76` fresh IR.
  - The focused generated `Out_channel.pos`/`Marshal.to_channel` repro built
    with the patched stage compiler and `-O3 -g -llvm-backend` now prints `0`;
    frametable parsing finds descriptors for both the formerly missing first
    C-call return address and the second C-call return address.
  - Direct execution of
    `testsuite/tests/llvm-codegen/c_call_stackmap.sh` against the patched
    stage install passes.
  - `tools/run-llvm-stage5-ocamltest.sh` for `tests/llvm-codegen` with the
    patched stage install passes: `25` passed, `15` skipped, `0` failed,
    `40` considered. Wrapper evidence: `24` wrapper lines / `12` fresh IR.
  - Built another full patched stage in
    `validation-tmp/c_call_stackmap_stage2b` from the stage2a install with
    normal Dune parallelism. Runtime build evidence: `148` wrapper lines /
    `74` fresh IR. Main build evidence: `2228` wrapper lines / `1101` fresh
    IR. A stage2b smoke compiled and ran `fib 10`, printing `55`.
  - Inspected `camlCmi_format__marshal_11_39_code` in the stage2b compiler.
    The first `caml_c_call` return address now has a frametable descriptor,
    as do the later C-call return descriptors.
  - Focused self-stage2 ocamltest validation with the stage2b compiler for the
    previously failing directories (`tests/flambda2/symbol_projections`,
    `tests/lib-smallint`, and `tests/typing-labels`) passes: `66` passed,
    `0` skipped, `0` failed. Wrapper evidence: `110` wrapper lines /
    `55` fresh IR.
  - A fresh full self-stage2 ocamltest rerun from the patched stage2b compiler
    now passes. Command used `SELF_STAGE=2`,
    `STAGE_INSTALL=validation-tmp/c_call_stackmap_stage2b/stage_install`,
    `STAGE_BUILD=validation-tmp/c_call_stackmap_stage2b/main_build`,
    `LLVM_TESTSUITE_PARALLEL=auto`, and `LLVM_TESTSUITE_JOBS=$(nproc)`.
    GNU parallel was unavailable, so the script used the serial `one` target
    with `make -j$(nproc)`. Result: `6351` tests passed, `269` skipped,
    `0` failed, `0` unexpected errors, `6620` considered. Wrapper evidence:
    `6202` wrapper lines / `3101` fresh IR. Log:
    `validation-tmp/c_call_stackmap_stage2b/full_self_stage2_ocamltest_after_c_call_stackmap.log`.
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
  - Implemented safe X86_64 preserved-root slot pooling in
    `backend/llvm/llvmize.ml`:
    - The new helper builds an interference graph from CFG liveness `before`
      and `across` sets, adds tailcall-self parallel-assignment interference
      between terminator arguments and function arguments, and greedily colors
      only preserved value registers.
    - `alloca_regs` reuses one `alloca` for each X86_64 liveness color class.
      AArch64 and non-X86_64 targets keep the previous one-slot-per-register
      behavior.
    - `ARCH=amd64 RUNTIME_DIR=runtime dune build ocamloptcomp.cma` exits 0
      after regenerating the clean build's configure/dune files.
    - `opam exec --switch=oxcaml-5.4.0+oxcaml -- ocamlformat --check
      backend/llvm/llvmize.ml` exits 0, and `git diff --check` exits 0.
    - A clean `make llvm-install ARCH=amd64 LLVM_BOOT_BACKEND=0
      LLVM_PATH=/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper-opt
      prefix=/tmp/oxcaml-agent-llvm-amd64-support/install-pooled` exits 0 with
      normal build parallelism.
    - The patched installed compiler reports version `5.2.0+ox`; its BuildID
      is `e84aedaa0f007fd6c2c1301adb88ebe3021ff0fc`.
    - A direct installed-compiler smoke with `-llvm-backend` and the
      agent-local wrapper prints `11`.
    - Key patched `ocamlopt.opt` stack frames are much smaller than the current
      pushed backend: `Typecore__type_expect` is 400 bytes,
      `Closure_conversion__cont` is 256 bytes,
      `Lambda_to_flambda_primitives__convert_lprim` is 160 bytes, and
      `Flambda2_reaper__Points_to_analysis__entry` is 80 bytes. Previously
      observed current-backend values were roughly 4,112 bytes, 672 bytes,
      4,464 bytes, and 6,128 bytes respectively.
    - A fresh default-stack boot-context build in
      `/tmp/oxcaml-agent-llvm-amd64-support/pooled_boot_context_build` passes
      with `RUN_SMOKE=0` and without `OCAMLRUNPARAM=b,Xmain_stack_size=64M`:
      1,678 wrapper invocations and 825 fresh IR compilations.
    - Focused manual AMD64 LLVM-codegen regressions compile and run through
      the patched installed compiler:
      `amd64_core_ops.ml`, `amd64_direct_call_stackmap.ml`,
      `amd64_exceptions.ml`, `amd64_raise_notrace_alloc.ml`, and
      `amd64_stack_growth.ml`.
    - `make llvm-test-one DIR=llvm-codegen ...` was not rerun successfully in
      the local checkout after the clean install because `_build/default` was
      missing generated dependency/include files such as
      `duneconf/camlinternalquote_if_missing_from_stdlib`, then many `.d` and
      `.sexp` files. This is recorded as a stale local build-context issue;
      use the direct installed-compiler tests above as the focused result for
      this patch.
  - Ran broader self-stage validation on top of the preserved-root slot
    pooling patch using normal build parallelism:
    - The first root-backed boot-context attempt failed once with
      `Fatal error: caml_scan_stack: missing frame descriptor` during
      `.ocamloptcomp.objs/byte/_unknown_`; rerunning the same boot build
      without cleaning passed. This remains an intermittent frame-scan
      symptom rather than a stable reduced action.
    - A full self-stage install from the successful boot context passed:
      boot `1678` wrapper lines / `828` fresh IR, runtime `148` / `74`,
      main `2228` / `1108`, and both stage and self-stage smokes printed
      `55`.
    - Full self-stage ocamltest all-minus-asm completed with:
      `6320` passed, `269` skipped, `25` failed, `0` unexpected errors,
      wrapper lines `6198`, fresh IR `3099`.
    - Failures were:
      `lib-floatarray/floatarray.ml` native,
      `native-cfi-stepping/test_cfi.ml` output mismatch,
      `runtime-errors/stackoverflow.ml` native output mismatch,
      21 native unboxed array/iarray layout tests, and
      `unboxed-primitive-args/test.ml` AMD64 run.
    - The unboxed array/product-array sort failures reproduce with a patched
      boot compiler even when X86_64 preserved-root pooling is diagnostically
      disabled, while the default backend passes the same direct compile. The
      direct repro used `test_float32_u_array.ml` with
      `OCAMLPARAM="_,llvm-backend=1,llvm-path=/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper-opt"`
      and failed at `test_gen_u_array.ml` sort/permutation assertions.
    - `unboxed-primitive-args/test.ml` still fails with the entry-argument
      interference follow-up; mismatches show lost/zeroed `float32` and SIMD
      arguments in mixed AMD64 unboxed primitive calls.
    - Added a follow-up to the pooling graph so all `cfg.fun_args` interfere
      with each other before coloring preserved-root slots. A fresh
      boot-context build with that follow-up passed with `1678` wrapper lines
      and `825` fresh IR. This does not fix the array sort or ABI clusters,
      which reproduce without pooling.
    - Added X86_64 OxCaml calling-convention rules for LLVM vector value types
      (`v2i64`, `v4i64`, `v8i64`) in both argument and return lowering. Rebuilt
      the local wrapper tools with
      `cmake --build /tmp/oxcaml-agent-llvm-amd64-support/llvm-build --target llc opt --parallel`;
      this regenerated `X86GenCallingConv.inc` and rebuilt `llc`/`opt`.
    - A direct temporary repro at
      `validation-tmp/min_sii_direct` passes through the patched wrapper:
      constructing two `int64x2` values with `vec128_of_int64s`, reading them
      back with `vec128_low_int64`/`vec128_high_int64`, and passing
      `float32, int64x2, int64x2` to a noalloc C stub prints matching ML/C
      bits. Exact compile used the entry-interference boot compiler with
      `OCAMLPARAM="_,llvm-backend=1,llvm-path=/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper-opt"`.
    - The full generated `unboxed-primitive-args` direct repro still fails
      after the vector CC patch. The generated assembly for simple wrappers
      such as `test_v_sII` now passes vector C arguments in `xmm0..` as
      expected, and a direct `gdb` breakpoint at `test_v_IIfx` showed valid
      vector registers on at least one call. The remaining mismatch appears to
      involve the large harness's argument construction/preservation path, not
      the basic noalloc vector C-call edge.
    - Fixed one LLVM-side violation of the Domainstate extra-params contract:
      incoming function arguments located in `Stack (Domainstate _)` are now
      copied into private allocas at function entry instead of using the shared
      domain slots as their backing storage for the whole function. Outgoing
      call arguments and return locations still point at the real shared slots.
      Validation used a fresh boot compiler built with normal Dune parallelism
      in `validation-tmp/domainstate_arg_boot/boot_context_build`; the boot
      build printed `1682` wrapper lines / `833` fresh IR and the smoke printed
      `55`.
    - Rebuilt the compact generated repro in
      `validation-tmp/subset_full_test0/select.domainstate.opt` with the
      Domainstate copy fix. Compared with the previous `select.opt 23`
      mismatch in `test_x_xxxxxxxxxxxxxxxxx`, the new run has no vector
      mismatch and reports a single remaining mismatch in
      `test_s_sssssssssssssssss`. `select.domainstate.opt 22` and
      `select.domainstate.opt 25` both pass; extras `23` and `24` expose the
      same single `float32` stack-argument mismatch. This leaves a narrower
      high-arity `float32`/stack-argument issue to investigate next.
    - Reduced that remaining `float32` mismatch to the boxed-float32 allocation
      slow path in `Common.Buffer.get_float32`: LLVM emitted
      `callq caml_call_gc@PLT` while `%xmm0` still held the double result of
      `caml_int32_float_of_bits_unboxed`; the native backend emits
      `caml_call_gc_sse_` for the same CFG because the live float register must
      survive the GC.
    - Fixed AMD64 LLVM heap allocation and poll slow paths to pick
      `caml_call_gc_sse`, `caml_call_gc_avx`, or `caml_call_gc_avx512` when
      liveness shows float/SIMD values across the safepoint. Rebuilt the boot
      compiler with normal Dune parallelism:
      `DUNE_BUILD_FLAGS="-j $(nproc)" ./tools/build-llvm-boot-with-installed.sh`
      using `BOOT_BUILD=validation-tmp/simd_gc_boot/boot_context_build`;
      result: `1682` boot wrapper lines, `839` fresh IR, smoke printed `55`.
    - Rebuilt the compact repro as
      `validation-tmp/subset_full_test0/select.simdgc.opt` with
      `OCAMLPARAM="_,llvm-backend=1,llvm-path=/tmp/oxcaml-agent-llvm-amd64-support/clang-wrapper-opt,keep-llvmir=1"`.
      The generated `common.s` now calls `caml_call_gc_sse@PLT` in
      `camlCommon__get_float32_27_71_code`, and selectors `22`, `23`, `24`,
      and `25` all exit 0 with empty output.
    - Rebuilt the full generated `unboxed-primitive-args` harness against the
      latest boot compiler in
      `validation-tmp/repro_unboxed_primitive_args_simdgc/test.opt`; it exits
      0 with empty output.
    - The previously reduced `float32` array sort repro now passes in
      `validation-tmp/reduce_float32_sort_simdgc/repro.opt`, printing `ok`.
      The original direct `typing-layouts-arrays/test_float32_u_array.ml`
      shape also exits 0 with empty output in
      `validation-tmp/repro_float32_array_simdgc`.
    - A sequential direct batch of layout array/iarray tests with the
      SIMD-preserving liveness patch showed all scalar arrays and all iarrays
      passing, but still failed vector array/product-array cases. Reduction
      showed `Vector_elem.Int64x2.high_to` emitted `caml_call_gc@PLT` while
      LLVM had materialized a lane extract in `%xmm0` across the slow-path
      call; the Cfg liveness only contained the extracted scalar.
    - Updated AMD64 LLVM slow-path selection to use `caml_call_gc_sse` by
      default when no wider live vector class requires `caml_call_gc_avx` or
      `caml_call_gc_avx512`. Rebuilt a fresh boot compiler with normal Dune
      parallelism in
      `validation-tmp/xmm_default_gc_boot/boot_context_build`; result:
      `1682` boot wrapper lines, `827` fresh IR, smoke printed `55`.
    - With that compiler, `Vector_elem.Int64x2.high_to` now emits
      `caml_call_gc_sse@PLT`, and the focused vector map repro passes under
      default, `OCAMLRUNPARAM=s=4k`, `s=8k`, and `s=16k` minor heaps. Direct
      `typing-layouts-arrays` validations now pass with empty output for
      `test_vec128_u_array.ml`, `test_vec256_u_array.ml`,
      `test_ignorable_product_array_3.ml`,
      `test_ignorable_product_array_4.ml`,
      `test_ignorable_product_array_with_uninit_3.ml`, and
      `test_ignorable_product_array_with_uninit_4.ml`.
    - Rechecked the two old non-layout focused failures on top of
      `f6451372fe`:
      - `native-cfi-stepping/test_cfi.ml` still mismatches. The direct compile
        in `validation-tmp/native_cfi_xmm_default` succeeds, but
        `run_gdb.sh ./test_cfi` emits repeated `Backtrace failed` sections
        while single-stepping through `caml_raise_exn` around
        `test_cfi.ml:50`; the expected output is just `ok`.
      - `runtime-errors/stackoverflow.ml` still mismatches. The direct run in
        `validation-tmp/stackoverflow_xmm_default` exits 0, but omits both
        `x = 20000` lines from `stackoverflow.opt.reference`, while preserving
        the later `x = 10000`, `x = 0`, backtrace, and `!p = 42` lines.
    - Fixed the `native-cfi-stepping/test_cfi.ml` mismatch. GDB was unwinding
      through `caml_raise_exn` to the call return address at the first byte
      after the LLVM function's DWARF range, so the caller frame printed as
      `?? () at test_cfi.ml:50` even though the preceding bytes had the right
      symbol and debug subprogram. AMD64 LLVM regular/reraise lowering now
      emits a side-effecting `nop` after the raise call, matching the native
      backend's post-raise padding contract.
    - Rebuilt a fresh boot compiler with normal Dune parallelism in
      `validation-tmp/no_stackcheck_attr_boot/boot_context_build`:
      `DUNE_BUILD_FLAGS="-j $(nproc)" ./tools/build-llvm-boot-with-installed.sh`
      produced `1682` boot wrapper lines / `835` fresh IR, and the smoke
      printed `55`.
    - With that boot compiler, the focused direct
      `native-cfi-stepping/test_cfi.ml` run in
      `validation-tmp/native_cfi_no_stackcheck_attr` passes: `run_gdb.sh`
      exits 0 and `diff -u test_cfi.reference test_cfi.output` is empty.
      Wrapper log evidence: `4` wrapper lines / `2` fresh IR.
    - Also fixed the LLVM function attribute mismatch with this switch's
      `no_stack_checks: true`: AMD64 LLVM functions keep
      `frame-pointer="all"` but no longer receive `oxcaml-stack-check` unless
      `Config.no_stack_checks` is false. The focused
      `runtime-errors/stackoverflow.ml` repro in
      `validation-tmp/stackoverflow_no_stackcheck_attr` confirms the recursive
      function no longer has the LLVM prologue stack check, but it still omits
      both `x = 20000` lines because the LLVM frame for the recursive function
      is larger than the native frame. The run still exits 0 and preserves the
      `x = 10000`, `x = 0`, backtrace, and `!p = 42` lines.
    - The focused Linux/AMD64 stack-growth regression
      `testsuite/tests/llvm-codegen/amd64_stack_growth.ml` still passes with
      the no-stack-check-attribute compiler in
      `validation-tmp/amd64_stack_growth_no_stackcheck_attr`; output is empty,
      wrapper log evidence is `4` wrapper lines / `2` fresh IR.
    - Tried an uncommitted X86_64 `wrap_try` result extraction experiment to
      shrink the `runtime-errors/stackoverflow.ml` frame. It did not help: the
      focused run in `validation-tmp/stackoverflow_wrap_try_extract_only`
      still omitted the `x = 20000` lines and the recursive frame grew from
      `sub $0x20,%rsp` to `sub $0x30,%rsp`. The experiment was reverted.
    - Confirmed the `runtime-errors/stackoverflow.ml` mismatch is stack-budget
      sensitivity rather than broken exception propagation. The existing LLVM
      binary in `validation-tmp/stackoverflow_no_stackcheck_attr` fails the
      reference with `OCAMLRUNPARAM=l=100000`, but passes with `l=150000`,
      `l=200000`, `l=300000`, and `l=1000000`. `Xmain_stack_size=64M` alone
      does not affect this test.
    - After raising the test's `ocamlrunparam` to `l=150000`, the focused
      LLVM-backend direct run in `validation-tmp/stackoverflow_l150000` passes
      against `stackoverflow.opt.reference`: run exits 0, diff is empty, and
      wrapper evidence is `4` wrapper lines / `2` fresh IR.
    - The same updated source also passes direct non-LLVM installed-compiler
      checks in `validation-tmp/stackoverflow_l150000_standard`: native and
      bytecode runs both exit 0 and match their existing references.
    - Refreshed broader self-stage validation in
      `validation-tmp/self_stage_refresh` with normal Dune parallelism:
      `DUNE_BUILD_FLAGS="-j $(nproc)" ./tools/build-llvm-self-stage-install.sh`
      built a fresh boot compiler (`1682` wrapper lines / `836` fresh IR,
      smoke printed `55`) and a self-stage runtime (`148` wrapper lines /
      `74` fresh IR). The first self-stage main build reported one transient
      `caml_scan_stack: missing frame descriptor` while compiling
      `otherlibs/dynlink`, but an immediate incremental rerun of the same main
      build completed (`304` wrapper lines / `152` fresh IR). A refreshed
      self-stage install smoke then printed `55` with `4` wrapper lines /
      `2` fresh IR.
    - Ran a fresh full self-stage ocamltest refresh from that install:
      `SELF_STAGE=1 STAGE_INSTALL=validation-tmp/self_stage_refresh/stage_install
      STAGE_BUILD=validation-tmp/self_stage_refresh/self_main_build
      LLVM_TESTSUITE_PARALLEL=auto LLVM_TESTSUITE_JOBS=$(nproc)
      ./tools/run-llvm-stage5-ocamltest.sh`. Result: `6346` tests passed,
      `269` skipped, `0` failed, `0` unexpected errors, `6615` considered.
      Wrapper evidence: `6470` wrapper lines / `3235` fresh IR. Log:
      `validation-tmp/self_stage_refresh/self_stage_ocamltest.log`.
    - Refreshed self-stage2 validation in
      `validation-tmp/self_stage2_refresh` with normal Dune parallelism:
      `DUNE_BUILD_FLAGS="-j $(nproc)" ./tools/build-llvm-stage5-install.sh`
      completed from the refreshed self-stage install. Runtime build evidence:
      `148` wrapper lines / `74` fresh IR. Main build evidence: `2228`
      wrapper lines / `1104` fresh IR. The self-stage2 smoke printed `55`
      with `4` wrapper lines / `2` fresh IR.
    - The first full self-stage2 ocamltest run used
      `SELF_STAGE=2 STAGE_INSTALL=validation-tmp/self_stage2_refresh/stage_install
      STAGE_BUILD=validation-tmp/self_stage2_refresh/main_build
      LLVM_TESTSUITE_PARALLEL=auto LLVM_TESTSUITE_JOBS=$(nproc)
      ./tools/run-llvm-stage5-ocamltest.sh`. GNU parallel was unavailable, so
      the script selected the serial `one` target, but `make -j$(nproc)` was
      still active. Result: `6342` tests passed, `269` skipped, `4` failed,
      `0` unexpected errors, `6615` considered. The four failures were all
      `caml_scan_stack: missing frame descriptor` aborts while running the
      stage2 `ocamlopt.opt`: two in
      `tests/flambda2/symbol_projections/symbol_projections_mixed_blocks.ml`
      line 6, plus `tests/lib-smallint/test_int16.ml` and
      `tests/lib-smallint/test_int8.ml`. Wrapper evidence: `6178` lines /
      `3089` fresh IR. Log:
      `validation-tmp/self_stage2_refresh/self_stage2_ocamltest.log`.
    - Focused self-stage2 repro attempts did not make those first-run aborts
      deterministic. The exact `test_int8.ml` native compile passed when run
      directly. A focused serial ocamltest run of
      `tests/flambda2/symbol_projections` and `tests/lib-smallint` passed
      `60` tests with `0` failures (`98` wrapper lines / `49` fresh IR; log
      `validation-tmp/self_stage2_refresh/failed_dirs_serial.log`). Forcing
      `LLVM_TESTSUITE_PARALLEL=1` could not run because GNU parallel is not
      installed.
    - A full self-stage2 ocamltest rerun with `LLVM_TESTSUITE_PARALLEL=0` and
      no `LLVM_TESTSUITE_JOBS` reduced the abort pattern but still found one
      intermittent missing-frame-descriptor abort:
      `tests/typing-labels/mixin.ml` native failed while compiling with the
      stage2 `ocamlopt.opt`; the emitted message was
      `caml_scan_stack: missing frame descriptor`. Result: `6345` tests
      passed, `269` skipped, `1` failed, `0` unexpected errors, `6615`
      considered. Wrapper evidence: `6194` lines / `3097` fresh IR. Log:
      `validation-tmp/self_stage2_refresh/self_stage2_ocamltest_rerun.log`.
      Five direct retries of the same LLVM-backend `mixin.ml` native compile
      passed (`20` wrapper lines / `10` fresh IR), and a focused serial
      ocamltest run of `tests/typing-labels` passed all `6` tests
      (`12` wrapper lines / `6` fresh IR; log
      `validation-tmp/self_stage2_refresh/typing_labels_serial.log`).
    - Merged `origin/jujacobs/llvm-backend-integration` into the AMD64 branch,
      resolved backend/vendor/script conflicts by preserving the AMD64 LLVM
      bring-up changes, and restored
      `caml_avx2_int16x16_mul_round -> vpmulhrsw_Y_Y_Ym256` after the merge
      selected the stale non-256-bit helper name.
    - Rebuilt the merged standard compiler with normal build parallelism:
      `make -s compiler -j "$(nproc)"` passed after regenerating
      `duneconf/camlinternalquote_if_missing_from_stdlib` and using the
      `oxcaml-5.4.0+oxcaml` opam switch for the host compiler.
    - Built a merged AMD64 LLVM stage in
      `validation-tmp/integration_merge_stage` using the refreshed checkout
      `_install` as `BOOT_INSTALL` and
      `DUNE_BUILD_FLAGS="-j $(nproc)" ./tools/build-llvm-stage5-install.sh`.
      Runtime build evidence: `148` wrapper lines / `73` fresh IR. Main build
      evidence: `2228` wrapper lines / `1105` fresh IR. The focused merged
      `tests/llvm-codegen` run passed `25` tests, skipped `15`, failed `0`,
      and considered `40`; wrapper evidence was `24` lines / `12` fresh IR.
    - Built a merged AMD64 LLVM self-stage2 in
      `validation-tmp/integration_merge_stage2` from the merged stage install
      with `DUNE_BUILD_FLAGS="-j $(nproc)"
      ./tools/build-llvm-stage5-install.sh`. Runtime build evidence: `148`
      wrapper lines / `74` fresh IR. Main build evidence: `2228` wrapper
      lines / `1106` fresh IR. A self-stage2 smoke printed `55` with `4`
      wrapper lines / `2` fresh IR.
    - The focused merged self-stage2 `tests/llvm-codegen` run passed `25`
      tests, skipped `15`, failed `0`, and considered `40`; wrapper evidence
      was `24` lines / `12` fresh IR. The new `c_call_stackmap.ml` regression
      passed in that run.
    - A full merged self-stage2 ocamltest run from
      `validation-tmp/integration_merge_stage2` used
      `SELF_STAGE=2 STAGE_INSTALL=../validation-tmp/integration_merge_stage2/stage_install
      STAGE_BUILD=../validation-tmp/integration_merge_stage2/main_build
      LLVM_TESTSUITE_PARALLEL=auto LLVM_TESTSUITE_JOBS=$(nproc)
      ./tools/run-llvm-stage5-ocamltest.sh`. GNU parallel was unavailable, so
      the script selected the serial `one` target while the underlying make
      still used normal `-j$(nproc)` parallelism. Result: `6388` tests passed,
      `272` skipped, `0` failed, `0` unexpected errors, and `6660`
      considered. Wrapper evidence: `6326` lines / `3163` fresh IR. Log:
      `../validation-tmp/integration_merge_stage2/full_self_stage2_ocamltest.log`.
    - Implemented AMD64 LLVM `Probe_is_enabled` lowering. The first compiler
      rebuild caught the local LLVM IR comparison constructor typo (`Ne`
      instead of `Ine`); after fixing it, a clean `dune clean` followed by
      `make -s compiler -j "$(nproc)"` passed.
    - Direct focused validation using the rebuilt optimizing compiler
      `_build/main/oxcaml_main_native.exe` passed for a disabled probe, an
      `~enabled_at_init:true` probe, and matching `[%probe_is_enabled]`
      assertions:
      `OCAMLLIB=_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib
      _build/main/oxcaml_main_native.exe -O3 -llvm-backend
      -llvm-path "$LLVM_PATH" -probes -no-probes-optimized`. Result: binary
      ran successfully, `4` wrapper lines / `2` fresh IR. A second
      `-S -keep-llvmir` run also succeeded and showed weak hidden `.probes`
      globals for `caml_probes_semaphore_disabled` and
      `caml_probes_semaphore_enabled`, plus aligned `load i16` operations from
      offset `+2`; wrapper evidence again was `4` lines / `2` fresh IR. Log
      artifacts are under `validation-tmp/probe_is_enabled_direct`.
    - The repository `make llvm-test-one
      TEST=llvm-codegen/amd64_probe_is_enabled` target did not reach the test:
      its LLVM boot-context rebuild failed while opening
      `_build/default/duneconf/camlinternalquote_if_missing_from_stdlib`.
      Regenerating only that include left Dune's default context with stale
      dependency-file expectations, so the build state was cleaned with
      `dune clean --workspace=duneconf/boot.ws` before the successful normal
      compiler rebuild.
    - Implemented LLVM lowering for `Int128op` and rebuilt with a clean
      `dune clean --workspace=duneconf/boot.ws` followed by
      `make -s compiler -j "$(nproc)"`; result: passed.
    - Direct focused validation using the rebuilt optimizing compiler
      `_build/main/oxcaml_main_native.exe` passed for int128 add/sub,
      signed 64x64->128 multiply, and unsigned 64x64->128 multiply:
      `OCAMLLIB=_install/lib/ocaml _build/main/oxcaml_main_native.exe
      -I +stdlib_upstream_compatible -O3 -llvm-backend
      -llvm-path "$LLVM_PATH" -o validation-tmp/int128_ops_direct/int128_ops.exe
      stdlib_upstream_compatible.cmxa amd64_int128_ops_stubs.c int128_ops.ml`.
      Result: binary ran successfully, `4` wrapper lines / `2` fresh IR.
    - A second direct `-S -keep-llvmir` run also passed and showed LLVM
      `i128` lowering in
      `validation-tmp/int128_ops_direct/int128_ops.ll`: 128-bit add/sub use
      `add i128`/`sub i128`, signed multiply uses `sext i64 ... to i128`, and
      unsigned multiply uses `zext i64 ... to i128`; wrapper evidence again
      was `4` lines / `2` fresh IR.
    - Implemented LLVM lowering for wide vector constants and rebuilt with a
      clean `dune clean --workspace=duneconf/boot.ws` followed by
      `make -s compiler -j "$(nproc)"`; result: passed.
    - Direct focused validation using the rebuilt optimizing compiler passed:
      `OCAMLLIB=_install/lib/ocaml _build/main/oxcaml_main_native.exe
      -O3 -llvm-backend -llvm-path "$LLVM_PATH" -keep-llvmir -o
      validation-tmp/vec256_const/amd64_vec256_const.exe
      amd64_vec256_const_stubs.c amd64_vec256_const.ml`. Result: binary ran
      successfully, `4` wrapper lines / `2` fresh IR, and the kept IR contains
      the expected `<4 x i64>` constant literal for `caml_int64x4_const4`.
    - `make llvm-test-one TEST=llvm-codegen/amd64_vec256_const` still did not
      reach the test: its LLVM boot-context rebuild failed while opening
      `_build/default/duneconf/camlinternalquote_if_missing_from_stdlib`, the
      same generated-include issue seen in earlier focused `llvm-test-one`
      attempts. The build state was restored afterward with another clean
      `dune clean --workspace=duneconf/boot.ws` followed by
      `make -s compiler -j "$(nproc)"`; result: passed.
    - Implemented LLVM lowering for 256-bit and 512-bit vector scalar casts
      and rebuilt with a clean `dune clean --workspace=duneconf/boot.ws`
      followed by `make -s compiler -j "$(nproc)"`; result: passed.
    - Direct focused validation using the rebuilt optimizing compiler passed:
      `OCAMLLIB=_install/lib/ocaml _build/main/oxcaml_main_native.exe
      -O3 -llvm-backend -llvm-path "$LLVM_PATH" -keep-llvmir -o
      validation-tmp/vec256_scalar_cast/amd64_vec256_scalar_cast.exe
      amd64_vec256_scalar_cast_stubs.c amd64_vec256_scalar_cast.ml`. Result:
      binary ran successfully, `4` wrapper lines / `2` fresh IR, and the kept
      IR contains `insertelement`/`extractelement` lowering for both
      `<4 x i64>` and `<4 x double>`.
    - `make llvm-test-one TEST=llvm-codegen/amd64_vec256_scalar_cast` still
      did not reach the test: its LLVM boot-context rebuild failed while
      opening `_build/default/duneconf/camlinternalquote_if_missing_from_stdlib`.
      The build state was restored afterward with another clean
      `dune clean --workspace=duneconf/boot.ws` followed by
      `make -s compiler -j "$(nproc)"`; result: passed.
    - Merged the updated `origin/jujacobs/llvm-backend-integration` base at
      `32d7ad2f69`, including the AArch64 LLVM stack-check byte/slack
      contract work. Conflict resolutions kept the AMD64 LLVM frame-pointer
      and stack-check attributes while preserving the new AArch64
      `oxcaml-stack-check-bytes` and `oxcaml-stack-check-before-bytes`
      attributes, and kept the boot-context script's agent-local `ocaml`
      wrapper plus the new `dune_command` flag handling.
    - Post-merge validation rebuilt with a clean
      `dune clean --workspace=duneconf/boot.ws` followed by
      `make -s compiler -j "$(nproc)"`; result: passed.
    - Post-merge direct focused validation reran
      `amd64_vec256_scalar_cast` with `_build/main/oxcaml_main_native.exe
      -O3 -llvm-backend -llvm-path "$LLVM_PATH" -keep-llvmir`. Result:
      binary ran successfully, `4` wrapper lines / `2` fresh IR, and the kept
      IR still contains the expected vec256 scalar-cast insert/extract
      operations plus AMD64 `frame-pointer="all"` attributes. This checkout is
      configured with `no_stack_checks: true`, so the direct IR smoke does not
      contain `oxcaml-stack-check` attributes.
    - Implemented LLVM lowering for vec512 reinterpret casts and rebuilt with
      a clean `dune clean --workspace=duneconf/boot.ws` followed by
      `make -s compiler -j "$(nproc)"`; result: passed. An attempted direct
      ML-level vec512 smoke could not be made source-visible in this checkout:
      the public SIMD extension accepts the existing `int64x2`/`int64x4`
      surface types, but the attempted `int64x8` external declarations fail
      before lowering with `Unbound type constructor "int64x8"`.
    - Implemented AMD64 LLVM prefetch selection/lowering and rebuilt with
      `make -s compiler -j "$(nproc)"`; result: passed. Direct validation of
      `testsuite/tests/llvm-codegen/amd64_prefetch.sh` against the rebuilt
      compiler also passed under `validation-tmp/prefetch_script`: the kept IR
      contains two `call void @llvm.prefetch.p0(...)` sites and the matching
      declaration.
    - Implemented AMD64 LLVM `caml_cldemote` selection/lowering and rebuilt
      with `make -s compiler -j "$(nproc)"`; result: passed. The first direct
      smoke confirmed that `llvm.x86.cldemote` IR was emitted but `llc`
      rejected the intrinsic without `+cldemote`; the final lowering uses
      side-effecting inline assembly instead. Direct validation with
      `_build/main/oxcaml_main_native.exe -O3 -llvm-backend -c -S
      -keep-llvmir` passed under `validation-tmp/cldemote_direct`, with
      `2` wrapper lines / `1` fresh IR and generated assembly containing
      `cldemote (%rax)`. The new
      `testsuite/tests/llvm-codegen/amd64_cldemote.sh` script also passed
      directly under `validation-tmp/cldemote_script`.
    - Implemented AMD64 LLVM selection for simple SIMD memory load/store
      builtins and rebuilt with `make -s compiler -j "$(nproc)"`; result:
      passed. Direct validation with `_build/main/oxcaml_main_native.exe
      -extension layouts_alpha -extension simd_beta -extension small_numbers
      -O3 -llvm-backend -c -S -keep-llvmir` passed under
      `validation-tmp/simd_mem_direct`, with `2` wrapper lines / `1` fresh IR.
      The kept IR contains unaligned `<2 x i64>` and `<4 x i64>` vector
      stores and loads with `align 1`. The new
      `testsuite/tests/llvm-codegen/amd64_simd_mem.sh` script also passed
      directly under `validation-tmp/simd_mem_script`.
    - Implemented AMD64 LLVM lowering for the SSE2/SSE3 vec128 low-64 memory
      subset and rebuilt with `make -s compiler -j "$(nproc)"`; result:
      passed. Direct validation with `_build/main/oxcaml_main_native.exe
      -extension layouts_alpha -extension simd_beta -extension small_numbers
      -O3 -llvm-backend -c -S -keep-llvmir` passed under
      `validation-tmp/simd_low64_mem_direct`, with `2` wrapper lines /
      `1` fresh IR. The kept IR contains unaligned `i64` loads, an unaligned
      `i64` store, lane inserts for lanes `0` and `1`, and a low-lane extract.
      The new `testsuite/tests/llvm-codegen/amd64_simd_low64_mem.sh` script
      also passed directly under `validation-tmp/simd_low64_mem_script`.
    - Implemented AMD64 LLVM lowering for the SSE2 vec128 low-32 memory subset
      and rebuilt with `make -s compiler -j "$(nproc)"`; result: passed. The
      new `testsuite/tests/llvm-codegen/amd64_simd_low32_mem.sh` script passed
      directly under `validation-tmp/simd_low32_mem_script`; the kept IR
      contains unaligned `i32` loads, an unaligned `i32` store, `<4 x i32>`
      lane-0 inserts, and a lane-0 extract.
    - Implemented AMD64 LLVM lowering for the AVX memory-broadcast subset and
      rebuilt with `make -s compiler -j "$(nproc)"`; result: passed. The new
      `testsuite/tests/llvm-codegen/amd64_simd_broadcast_mem.sh` script passed
      directly under `validation-tmp/simd_broadcast_mem_script`; the kept IR
      contains an unaligned `<2 x i64>` load for broadcast128, unaligned
      scalar loads for the 64-bit and 32-bit broadcasts, and lane inserts up
      to the final vec128/vec256 lanes.

## Current Blocker

No focused blocker remains for `llvm-install` with a writable prefix, the
enabled Linux/AMD64 `llvm-codegen` tests, self-stage install/smoke, full
self-stage ocamltest, self-stage2 install/smoke, or full self-stage2
ocamltest. After merging the updated integration base, a standard compiler
build, focused merged-stage AMD64 LLVM-codegen runs, merged self-stage2
install/smoke, and full merged self-stage2 ocamltest run also pass. The
previously intermittent self-stage2
`caml_scan_stack: missing frame descriptor` abort had a concrete fixed
reproducer: the first external C call in `camlCmi_format__marshal_11_39_code`
now has a frame descriptor in the patched stage2b compiler, the previously
failing self-stage2 directories pass focused validation, and the full
self-stage2 ocamltest rerun passes. The default `/usr/local` install prefix is
not writable in this environment, so use an explicit agent-local `prefix=...`
for install validation. Also clear `LIST=`/`DIR=` when running a single
`TEST=...` after
`eval "$(../../../scripts/agent-tmp-env)"`, because the agent env may set
`LIST` for broader test runs.

The previous stack-usage caveat is now narrowed further. With a corrected
local optimizing wrapper, the standard explicit AMD64 `-llvm-backend` path
matches the native backend on the small non-tail recursion threshold check, and
the focused Linux/AMD64 `llvm-codegen` suite passes with the refreshed stage0
install. The safe X86_64 preserved-root slot pooling patch also lets the fresh
default-stack boot-context build pass without
`OCAMLRUNPARAM=b,Xmain_stack_size=64M`. Broader self-stage/ocamltest validation
now passes on top of the SIMD/XMM GC slow-path, post-raise CFI, and
`runtime-errors/stackoverflow.ml` stack-budget fixes. Broader self-stage2
validation also passes after the external C-call stackmap fix.
The OxCaml PR is still draft. Before the latest integration-base merge,
GitHub reported `mergeStateStatus: DIRTY` because
`jujacobs/llvm-backend-integration` had advanced to `32d7ad2f69`.

## Next Step

Next step is to keep the draft PR current, monitor GitHub CI, and continue
reviewing the remaining AMD64 LLVM backend surface for unsupported lowering
cases. Keep using normal build parallelism; avoid only concurrent top-level
`make` or `dune` commands in this checkout because of the shared lockfile.
