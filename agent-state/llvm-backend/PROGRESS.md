# Progress

Branch `jujacobs/llvm-backend` is working toward the no-frontend-roots model: the frontend should emit OCaml values as `ptr addrspace(1)`, custom RS4GC should materialize GC data late, and derived addrspace(1) values must be rematerialized from relocated bases because the OxCaml runtime cannot scan interior pointers. The current implementation has substantial RS4GC support for base/derived rematerialization, C-call argument roots, invoke/rejoin cases, base-equivalent PHIs, and fail-closed stackmap lowering for remaining derived relocates; it also includes a heuristic report for statepoint-crossing integers that are later cast to `ptr addrspace(1)`, explicitly as a debugging aid rather than a correctness invariant because immediates can have that shape safely. The important open issue is ordinary live heap SSA values in the no-frontend-roots model: reductions such as `weak_argv2.ml` showed a live string/closure value can cross a callback allocation without being listed in `gc-live`, so deleting frontend alloca roots is still unsafe until the late-root pass materializes all normal live OCaml values across statepoints, not just allocation slow paths or derived addresses. Recent source progress was committed as `52579c829d` (`Advance no-frontend GC root model`); the cleanup pass then deleted build outputs and most accumulated notes/artifacts, keeping only `BACKGROUND.md`, this file, and `NUMBERS.md` as handoff state.

2026-05-31 setup note: the correct local configure for LLVM-agent work is
`../../../scripts/configure-agent-oxcaml --clean` from `agents/llvm-backend/oxcaml`
or `scripts/configure-agent-oxcaml --clean llvm-backend` from the workspace
root. This runs `./configure --enable-frame-pointers --prefix=$PWD/_install`.
Using the default `/usr/local` prefix caused `_install/bin/ocamlc.opt -config`
to report `/usr/local/lib/ocaml`, which made Dune's `CamlinternalQuote` probe
compile a second incompatible `camlinternalQuote.cmi` in the boot build. The
symptom was `stdlib.cmxa and ocamlbytecomp.cmxa make inconsistent assumptions
over interface "CamlinternalQuote"` before reaching the SIGBUS repro. The
workspace now has `scripts/configure-agent-oxcaml` and stronger
`scripts/agent-doctor` checks for local prefix, frame pointers, and installed
stdlib path.

2026-05-31 self-stage repro after fixing setup: from this checkout, with
`../../../scripts/agent-tmp-env` evaluated and the wrapper created from
`../llvm-build/bin/clang`, running
`STAGE0_INSTALL="$PWD/_install" LLVM_WRAPPER="$PWD/../clang-wrapper" tools/build-llvm-self-stage-install.sh`
successfully built the LLVM boot compiler, then failed during the self-stage
runtime stdlib build with `Command got signal BUS`. The first reduced standalone
reproducer is the `stdlib__Map.cmx` compile from
`_llvm_self_stage_runtime_build/log`, replayed with the original
`stdlib__Map.cmx` basename. Under lldb it stops at
`camlSelect_utils__join_list_map_38_125_code` (`backend/select_utils.ml:406`):
the call sequence loads `x9` from the closure argument and branches to `x9`,
but `x9` points at an OCaml heap closure block (`Closure_tag`, first field
`0x0000000100327f34`) rather than executable code. This maps to the open root
tracking concern: an ordinary live heap value appears to survive incorrectly
across allocation/GC in the LLVM-built boot compiler.

2026-05-31 GC-root stress suite: added `testsuite/tests/llvm-gc-roots` with
focused LLVM-backend runtime tests for ordinary live heap values across forced
collections. The current cases cover allocation slow paths, closure calls
shaped like `Select_utils.join_list_map`, live records/arrays/closures across
minor/full/compact collections, and exception-handler paths with captured
closures. Validation run from this checkout:
`eval "$(../../../scripts/agent-tmp-env)" && make llvm-test-one DIR=testsuite/tests/llvm-gc-roots LLVM_PATH="$PWD/../clang-wrapper"`.
Result: 12 tests passed, 0 failed.

2026-06-01 self-stage2 status on `jujacobs/llvm-backend-stage2`: full LLVM
self-stage2 tests now pass with comballoc disabled: 6748 passed, 284 skipped,
0 failed. The last failure was `tests/statmemprof/bigarray.ml`, which was not
corruption. The reduced run showed `Gc.full_major` retained a mapped bigarray
through a tailcall because RS4GC rooted ordinary managed-call arguments in the
caller frame for the whole callee dynamic extent. The fix is to remove
call-operand-only liveness for OxCaml managed calls unless the value is really
used after the call, while still adding callee-duration roots for C-wrapper
call arguments (`oxcaml_ccc` / `oxcaml_c_stackcc`). Focused validation:
`llvm-lit` for `oxcaml-call-arg-root.ll` and
`oxcaml-statepoint-call-arg-root.ll`, standalone statmemprof bigarray repro,
`SELF_STAGE=2 ... tests/statmemprof`, and the full `SELF_STAGE=2
tools/run-llvm-stage5-ocamltest.sh`.

2026-06-01 regalloc call-clobber performance note: OxCaml has no callee-saved
GPRs, so values live across calls need stack homes at call regmasks but should
still be register-allocated in call-free regions between calls. PR #30 showed
the performance opportunity by leaving regmask-crossing region-split remainders
at `RS_New`, improving `direct_call_in_try_hit` from 1.7968x to 1.0143x
LLVM/native, but that violated Greedy RA's termination model: generated
`misc.ll` timed out after 60s in LLVM codegen. A bounded local experiment marks
those remainders `RS_Split2` instead, so they skip another general region split
and can only reach block/local splitting. That preserved the direct-call speedup
(`direct_call_in_try_hit` 1.0142x), avoided the `misc.ll` blowup (1.136s), and
improved the compiler-binary benchmark geomean from 1.0251x to 0.9829x. See
`agent-state/llvm-backend/NUMBERS.md` for the full table. The next cleanup is to
make this explicit as an OxCaml call-split stage rather than overloading
`RS_Split2`.

Follow-up in the same session: implemented explicit `RS_CallSplit` in vendored
LLVM and made it the default for OCaml/OxCaml GC functions whose live interval
crosses a call regmask. `RS_CallSplit` sits below `RS_Spill` but above
`RS_Split2`, so affected remainders skip normal region splitting and still
reach the bounded block/local split path. The final representative micro run
had geomean 0.8265x and kept `direct_call_in_try_hit` near parity at 1.0285x.
A fresh LLVM-built compiler using `_llvm_self_stage2_callstage_install`
benchmarked at 0.9782x geomean
LLVM/native, with max slowdown 1.0154x. Full numbers are in `NUMBERS.md`.

2026-06-01 regmask child-classification experiment: tested the extra idea of
classifying split children by the number of call regmask crossings. The strict
version, where a region-split remainder only gets the bounded call-split path
if it crosses fewer regmasks than its parent, regressed
`direct_call_in_try_hit` to 1.6607x LLVM/native. That shows ordinary region
splitting often has not made regmask-count progress yet; the useful split still
happens later in block/local splitting. A weaker version that sends call-free
remainders back to `RS_New` and call-crossing remainders to `RS_CallSplit`
slightly improved the worst nested closure case but worsened aggregate micro
performance (0.8307x geomean versus 0.8265x for plain `RS_CallSplit`). Do not
keep this refinement in the current hook. If we want a principled next step,
add a dedicated regmask-aware splitter that creates call-free islands directly;
then the regmask-count progress rule can be applied to that splitter's own
children.

TODO: investigate and fix duplicate explicit exception-root slots in RS4GC. The
remaining large micro slowdowns are `closure_call_in_try_hit` and
`closure_call_in_nested_try_hit`. The closure call itself is not the issue: the
slow path is active-trap root preservation around invokes. In the nested case,
the pre-RS4GC IR has one closure pointer live at the invoke, but custom OxCaml
exception-root handling creates eight volatile `.exnroot` slots for the same
logical value. Four come from one root slot per recovery boundary edge
(`L175`, `L181`, `L187`, `L193` -> `L203`), and four more come from the late
exception-root materialization pass after normal statepoint relocation. Some
of the resulting selectors are unused, but the slots are appended to `gc-live`,
so the volatile stores remain in the hot loop. A principled fix should intern
explicit exception-root slots by equivalent store-site value/statepoint rather
than by recovery incoming edge, and should prune unused explicit root slots
before appending them to `gc-live`.

2026-06-27 AMD64 LLVM validation on branch `jujacobs/llvm-x86-plan`: rebuilt
the compiler/runtime with the existing LLVM tools after clearing stale dune
boot-context state. The first `llvm-stack-checks` failure in
`compile_challenges_amd64.ml` was not a source regression; the checkout had an
inconsistent `_build/default` context.
After clearing `_build/default`, `_build/_bootinstall`, and dune's `_build/.db`
metadata, `make -s llvm-compiler` and `make -s llvm-install` completed. Focused
validation then passed:

- `make -s llvm-test-one LLVM_PATH="$PWD/llvm-tool-wrapper.sh" DIR=llvm-stack-checks`
  -> 8 passed, 2 skipped, 0 failed.
- `make -s llvm-test-one LLVM_PATH="$PWD/llvm-tool-wrapper.sh" DIR=llvm-gc-roots`
  -> 12 passed, 6 skipped, 0 failed.
- `make -s llvm-test-one LLVM_PATH="$PWD/llvm-tool-wrapper.sh" DIR=llvm-codegen`
  -> 60 passed, 30 skipped, 0 failed.
- `STAGE0_INSTALL="$PWD/_install" LLVM_WRAPPER="$PWD/llvm-tool-wrapper.sh" \
  tools/build-llvm-self-stage-install.sh` completed and produced
  `_llvm_self_stage_install`. Wrapper counts: boot 829 fresh IR inputs, runtime
  73, main 1107, final self-stage smoke 2. Both boot and final smoke programs
  printed `55`.
- Re-running `tools/build-llvm-self-stage-install.sh` with
  `STAGE0_INSTALL="$PWD/_llvm_self_stage_install"` and separate
  `_llvm_self_stage2_*` output directories completed and produced
  `_llvm_self_stage2_install`. Wrapper counts: boot 835 fresh IR inputs,
  runtime 74, main 1106, final self-stage2 smoke 2. Both boot and final smoke
  programs printed `55`.

Rejected experiment: tried to make AMD64 LLVM stack-growth helpers terminate
their `%rbp` chain like AArch64 terminates `x29`, but this is incorrect for
AMD64 with frame pointers. A clean rebuild produced a reproducible segfault in
`_build/main/tools/merge_archives.exe` during `ocamloptcomp_with_flambda2.cmxa`
creation. The reason is that AMD64 frame-pointer stacks need `%rbp` rewritten
by `caml_try_realloc_stack`; restoring an old-stack `%rbp` after stack growth
breaks execution. The experiment was fully reverted before the passing build
and tests above. Any future AMD64 frame-chain cleanup must preserve native
AMD64 frame-pointer rewriting semantics, not copy the AArch64 clobbering
approach literally.

2026-06-27 self-stage2 full test-suite status for AMD64 LLVM:
`SELF_STAGE=2 LLVM_TESTSUITE_JOBS=8 tools/run-llvm-stage5-ocamltest.sh`
completed serially because GNU parallel was unavailable. Final result:
6730 passed, 301 skipped, 38 failed, 0 not started, 0 unexpected errors.
Wrapper totals: 6500 wrapper lines, 3250 fresh IR inputs.

Failure clusters to reduce and fix:

- Atomic compare-exchange semantics:
  `tests/lib-atomic/test_atomic_cmpxchg.ml` native fails with
  `Assert_failure("test_atomic_cmpxchg.ml", 11, 9)`, and
  `tests/typing-layouts-or-null/atomics.ml` fails in native variants at
  `Assert_failure("atomics.ml", 79, 14)`. Treat these as one AMD64 LLVM
  primitive-lowering bug and compare against native AMD64 lowering.
- Stack-check / stack-overflow quality:
  `tests/llvm-stack-checks/compile_challenges_amd64.ml` fails with compiler
  stack overflow under self-stage2; `tests/misc/pr7168.ml` fails in both
  bytecode and native compiler modes with stack overflow; and
  `tests/runtime-errors/stackoverflow.ml` native catches overflow too early
  compared with the reference output. Standard installed `-llvm-backend`
  `llvm-stack-checks` passed earlier, so reduce which failures only reproduce
  with the self-stage2 compiler.
- GC/frame descriptor correctness:
  `tests/mixed-blocks/generated_native_test.ml` compiles but aborts during GC
  with `caml_scan_stack: missing frame descriptor retaddr=(nil)`. The older
  generated mixed-block native test passed. Reduce this against the standard
  installed `-llvm-backend` compiler first; if it only reproduces under
  self-stage2, keep the smallest self-stage2 reproducer.
- LLVM test harness/setup issues:
  `tests/llvm-codegen/raw_stack_word_amd64.ml` failed with inconsistent CMI
  assumptions involving `Stdlib_upstream_compatible`; and
  `tests/llvm-codegen/stack_check_size_contract_amd64.ml` failed because
  `/tmp/oxcaml-clang-wrapper` was missing. These look like self-stage2
  harness/env issues, not source backend failures.
- Missing LLVM lowering coverage:
  `tests/templates/basic/probe.ml` and
  `tests/typing-layouts-or-null/probe.ml` fail with
  `Llvmize: unimplemented instruction: probe`. Many
  `tests/typing-layouts-arrays` native failures are the same AMD64 SIMD gap,
  typically `Llvmize: unimplemented instruction ... vinsertf128`; the
  `test_float32_u_array.ml` variants instead report `Selection.select_oper`.
  Treat the repeated generated product/vector array failures as one SIMD
  lowering cluster; related scalar unboxed arrays and product iarrays passed.

2026-06-27 atomic compare-exchange fix in progress:
`backend/llvm/llvmize.ml` now returns LLVM `cmpxchg`'s loaded old value for
`Cmm.Compare_exchange` instead of selecting the previous destination register
on success. This matches native AMD64 `lock cmpxchg` result semantics: the old
loaded value is in `rax` on both success and failure. Added AMD64 LLVM codegen
coverage in `tests/llvm-codegen/amd64_core_ops.ml` for int and ref
`Atomic.compare_exchange` success/failure.

Validation after clearing stale dune boot-context state
(`_build/default`, `_build/_bootinstall`, `_build/.db`,
`_build/.filesystem-clock`):

- `make -s llvm-compiler LLVM_PATH="$PWD/llvm-tool-wrapper.sh"` passed.
- `make -s llvm-install LLVM_PATH="$PWD/llvm-tool-wrapper.sh"` passed.
- Direct installed-compiler repro for
  `testsuite/tests/lib-atomic/test_atomic_cmpxchg.ml` with
  `-llvm-backend -llvm-path "$PWD/llvm-tool-wrapper.sh"` passed.
- Direct installed-compiler repro for
  `testsuite/tests/typing-layouts-or-null/atomics.ml` with
  `-llvm-backend -llvm-path "$PWD/llvm-tool-wrapper.sh"` passed.
- `make -s llvm-test-one-no-rebuild LLVM_PATH="$PWD/llvm-tool-wrapper.sh" \
  TEST=testsuite/tests/llvm-codegen/amd64_core_ops.ml` passed: 5 passed.
- `make -s llvm-test-one-no-rebuild LLVM_PATH="$PWD/llvm-tool-wrapper.sh" \
  TEST=testsuite/tests/lib-atomic/test_atomic_cmpxchg.ml` passed: 2 passed.
- `make -s llvm-test-one-no-rebuild LLVM_PATH="$PWD/llvm-tool-wrapper.sh" \
  TEST=testsuite/tests/typing-layouts-or-null/atomics.ml` passed: 5 passed.

2026-06-27 optimized AMD64 GC-root fix: the clean
`LLVM_WRAPPER_LLC_OPT_LEVEL=3 make -s llvm-compiler` build exposed a stale-root
bug in the shared post-regalloc root listing. X86 byte stack-slot copies
matched LLVM's generic `isLoadFromStackSlot` / `isStoreToStackSlot` predicates,
so the pass treated a one-byte spill slot as value-preserving and listed it as
a GC root. The reduced failing shape was in
`camlFlambda2_simplify__Join_points__compute_handler_env_20_48_code`: a byte
copy
`MOV8rm %stack.0` / `MOV8mr %stack.9` preceded a call to `caml_apply2`, and the
old root list included `%stack.9`. At runtime the corresponding frame offset
held raw byte data such as `0x40` / `0x98`, so `caml_scan_stack` could scan a
garbage root under stress GC.

The fix is intentionally shared rather than AMD64-special-cased:
`OxCamlStatepointGCValueness.h` now defines the value-copy predicates used by
both `OxCamlStatepointSpillRoots` and `OxCamlGCRootVerifier`. A stack-slot
load/store is value-preserving only when the actual memory access is 8 bytes.
Targets that report the width through the `TargetInstrInfo` hook use that; for
targets such as AArch64 where the hook returns size `0`, the helper falls back
to the instruction's single `MachineMemOperand` size. Non-pointer-width stores
to LiveStacks slots are recorded as clobbers, so old value stores do not
incorrectly reach past raw subword writes.

Validation after the fix:

- `make -C _build/llvm-tools -j8 llc opt` passed.
- Regenerated the reduced `Join_points` MIR with `llc -O3
  -stop-after=oxcaml-statepoint-spill-roots`; the byte copy to `%stack.9`
  remains, but the following `caml_apply2` statepoint lists only `%stack.0` and
  `%stack.1` as roots, not `%stack.9`.
- Clean build with
  `LLVM_WRAPPER_LLC_OPT_LEVEL=3 LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" make -s llvm-compiler`
  passed after clearing dune build state.
- `make -s llvm-test-one ... DIR=llvm-gc-roots` did not reach the test after
  that clean build because the default boot/test Dune context was missing
  generated include/flag files (`_build/default/ocamlopt_flags.sexp`,
  `duneconf/camlinternalquote_if_missing_from_stdlib`, etc.). As a direct
  backend check instead, compiled and ran
  `testsuite/tests/llvm-gc-roots/allocation_slow_path_roots.ml` with
  `_build/install/main/bin/ocamlopt.opt -llvm-backend -llvm-path
  "$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` and
  `LLVM_WRAPPER_LLC_OPT_LEVEL=3 OCAMLRUNPARAM='s=64k,o=1,O=1'`; it printed
  `ok`.

2026-06-27 AMD64 optimized-llc backend progress:
added target-side active trap-depth tracking for X86, mirroring the AArch64
machine-CFG trap stack analysis.  X86 now records active OCaml trap bytes before
frame-index replacement and adjusts real `%rsp`-relative frame-index accesses,
which fixed the earlier optimized-wrapper runtime-stdlib crash where a runtime
exception landing block stored R14/R15 spill slots 16 bytes away from the later
recovered-RBP reloads.  Statepoint/stackmap/patchpoint metadata operands are
excluded from that generic X86 offset adjustment because the OxCaml frametable
printer already applies active trap bytes from the encoded statepoint id.
Continue this line of work by generalizing the ARM64 LLVM backend mechanism to
AMD64 target details.  Do not preserve old x86 LLVM behavior when it conflicts
with the current ARM64 mechanism or native AMD64 backend/runtime contracts.

Also corrected the AMD64 OxCaml frametable gc_regs map in
`OxCamlGCPrinter.cpp`: the previous table declared 16 entries but initialized
15, causing DWARF RAX to be remapped to gc_regs index 15.  Runtime
`amd64.S:SAVE_ALL_REGS` saves only 13 integer root registers; R14 and R15 are
runtime domain/allocation registers and are now rejected as scannable gc_regs
roots, matching the native AMD64 register map.

Validation:

- `make -C _build/llvm-tools -j8 llc opt` passed after the LLVM target changes.
- Clean default-wrapper build passed:
  `make -s llvm-compiler` with `LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`
  and default `LLVM_WRAPPER_LLC_OPT_LEVEL` (`llc -O0`).
- Clean optimized-wrapper build still fails:
  `LLVM_WRAPPER_LLC_OPT_LEVEL=3 make -s llvm-compiler` reaches the main
  compiler workspace and then reports `.ocamlcommon.objs/native/_unknown_`
  segfaults.  The earlier runtime stdlib crash is gone, but the remaining
  failure is not fixed yet.  Direct stress loops of the reported `mode.ml`,
  `tast_mapper.ml`, and dynlink parser commands did not reproduce reliably
  outside dune; continue by reducing the remaining GC-sensitive main-workspace
  crash and inspecting generated frame metadata/roots.
- Requested `gpt-5.5` high code-review agent could not be spawned because the
  multi-agent thread limit was reached.  Local review checked the changed X86
  trap-depth path and AMD64 gc_regs map against `runtime/amd64.S`, native
  `backend/amd64/proc.ml`, and AArch64 active-trap handling; no additional
  obvious issue was found beyond the known incomplete optimized build.

2026-06-27 optimized LLVM pipeline blocker and buildable RS4GC wrapper:
with the clang-like local wrapper (`opt -O3` then `llc -O3`), a clean
`make -s llvm-compiler` failed in the boot compiler while lowering optimized
`utils/file_sections.ml`:

```
LLVM ERROR: unrematerialized OxCaml derived pointer across statepoint
```

The reduced failing function was extracted with:

```
_build/llvm-tools/bin/llvm-extract \
  --func=$'\001camlOxcaml_utils__File_sections__unsafe_blit_to_array_8_24_code' \
  -S /tmp/oxcaml-rs4gc-fails/optimized.333780.ll \
  -o /tmp/file_sections.unsafe_blit.ll
```

`llc -O3` creates an LSR pointer induction PHI for an addrspace(1) array slot:
`%lsr.iv1 = phi ptr addrspace(1) [ %uglygep2.remat, %L623 ],
[ %uglygep.remat, %L540 ]`, base `%3`.  The current RS4GC single-base PHI
rematerializer correctly refuses to relocate this interior pointer
independently, but it also cannot yet rematerialize the LSR form.  The same
reducer passes if RS4GC runs after IR optimization and before `llc`:

```
_build/llvm-tools/bin/opt -S \
  -passes='default<O3>,rewrite-statepoints-for-gc,verify' \
  /tmp/file_sections.unsafe_blit.ll -o /tmp/file_sections.rs4gc.ll
_build/llvm-tools/bin/llc -O3 --relocation-model=pic --frame-pointer=all \
  -mattr=+avx,+avx2 -mattr=+avx \
  /tmp/file_sections.rs4gc.ll -o /tmp/file_sections.rs4gc.s
```

However, a clean boot compiler built with `opt
default<O3>,rewrite-statepoints-for-gc,verify` followed by `llc -O3` was
miscompiled: `_build/_bootinstall/bin/ocamlc.opt` deterministically segfaulted
while compiling `stdlib/camlinternalOO.ml`, with gdb showing the crash at
`typing/types.ml:1328` in `Types.get_level`, called from
`Ctype.nondep_type_rec_inner`.  The crash happened after several minor/major
collections, so post-RS4GC codegen optimization remains suspect for stack-map
or root metadata.

Added `tools/llvm-rs4gc-llc-wrapper.sh` as a checked-in build wrapper.  It runs
`opt -passes='default<O3>,rewrite-statepoints-for-gc,verify'` and defaults to
`llc -O0` after RS4GC; `LLVM_WRAPPER_LLC_OPT_LEVEL=3` reproduces the optimized
post-RS4GC path for follow-up debugging.  Validation with the conservative
default after clearing `_build/default`, `_build/_bootinstall`,
`_build/runtime_stdlib`, `_build/install`, `_build/.db`, and
`_build/.filesystem-clock`:

```
PATH="$PWD/_build/llvm-tools/bin:$PATH" \
LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" make -s llvm-compiler
```

passed with the checked-in wrapper.  Next step: reduce and fix the
`LLVM_WRAPPER_LLC_OPT_LEVEL=3` miscompile so the final AMD64 path can regain
optimized post-RS4GC codegen and performance measurements.

`llvm-codegen` no-rebuild directory run got through the new
`amd64_core_ops.ml` checks and still has the known `raw_stack_word_amd64.ml`
CMI-mismatch harness failure. Attempted to spawn the requested `gpt-5.5-high`
code-review agent for this commit, but the multi-agent tool reported the
thread limit was reached; local review with `git diff --check`, `git diff`,
and `git status --short` found no issue.

2026-06-27 optimized LLVM tool path status:
the local `llvm-tool-wrapper.sh` had been sending real IR-to-assembly builds
straight to `llc -O3`, skipping the `opt -O3` step implied by the compiler's
clang command line. After changing the untracked local wrapper to run
`opt -O3` before `llc`, a reduced `runtime-errors/stackoverflow.ml` compile
initially crashed in `RewriteStatepointsForGC::exposeGCPointersInAggregates`.
The source fix in `vendor/llvm-project/llvm/lib/Transforms/Scalar/RewriteStatepointsForGC.cpp`
changes that pass's `MaybeDead` queue from raw `Instruction *` to
`WeakTrackingVH`, so recursive deletion can null out instructions that were
queued but already deleted as operands of another dead aggregate instruction.

Validation:

- Rebuilt local LLVM `opt` and `llc` with
  `cmake --build _build/llvm-tools --target opt llc -- -j8`.
- `_build/llvm-tools/bin/llc -O3 ... /tmp/stackoverflow.optimized.ll` no
  longer crashes.
- `_build/llvm-tools/bin/llc -O3 ... /tmp/stackoverflow.stackoverflow-fns.optimized.ll`
  no longer crashes.
- Direct `runtime-errors/stackoverflow.ml` compile/run with the optimized
  wrapper now matches the expected nested overflow output.
- `make -s llvm-test-one-no-rebuild LLVM_PATH="$PWD/llvm-tool-wrapper.sh" \
  TEST=testsuite/tests/runtime-errors/stackoverflow.ml` passed: 4 passed.
- `_build/llvm-tools/bin/opt -S -passes=rewrite-statepoints-for-gc,verify \
  vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-gc-aggregate-explosion.ll`
  passed.
- The same `opt` command with `-rs4gc-fail-on-unhandled-gc-aggregate` passed.

Remaining optimized-pipeline blocker: a clean `make -s llvm-compiler` with the
optimized wrapper now reaches real RS4GC derived-pointer guard failures in
`tools/simdgen/simdgen.ml`, `utils/file_sections.ml`, and
`middle_end/flambda2/types/grammar/type_grammar.ml`:
`LLVM ERROR: unrematerialized OxCaml derived pointer across statepoint`.
This confirms the old wrapper was hiding optimized-pipeline backend gaps. Next
work should reduce and fix the derived-pointer rematerialization failures
rather than weakening the guard or returning to the no-opt validation path.

2026-06-27 AMD64 stack realignment investigation:
`testsuite/tests/mixed-blocks/generated_native_test.ml` still exposes an
LLVM-built compiler stack-scan failure.  The failing compile aborts in
`caml_scan_stack` while scanning the compiler process, not the generated test
program.  A gdb walk found a realigned AMD64 frame in
`Flambda2_from_lambda__Closure_conversion_aux.create`; LLVM emits
`push %rbp; mov %rsp,%rbp; and $-32,%rsp; sub ...,%rsp`, but the OxCaml frame
descriptor is static and does not describe the dynamic padding correctly.

Rejected source experiments:

- Disabling X86 stack realignment for `gc "oxcaml"`/`gc "ocaml"` functions
  made frames static and fixed the immediate mixed-block compile, but it grew
  compiler frames enough that `llvm-stack-checks/compile_challenges_amd64.ml`
  failed the large-stack compile with `Fatal error: exception Stack overflow`.
  That is not acceptable for stack-check quality.
- Carrying `HasStackRealignment`/`MaxStackAlignment` through `StackMaps` and
  compensating in `OxCamlGCPrinter` also failed.  Adding max alignment
  overshot the scan (the next plausible code return addresses were below the
  scanner SP); changing the compensation to only a saved-frame-pointer slot
  still failed the mixed-block compile with a missing frame descriptor.

The likely next fix needs a principled AMD64 frame-table model for dynamically
realigned OCaml frames, or a way to make only OCaml-callable frames statically
aligned without the frame-size explosion above.  Do not commit either rejected
experiment.

2026-06-27 AMD64 statepoint frame-index cleanup:
changed x86 frame-index elimination so OxCaml `STACKMAP` / `PATCHPOINT` /
`STATEPOINT` operands follow the AArch64 contract.  Stackmap locations for
OxCaml calling conventions must resolve relative to `RSP`, include the
statepoint operand immediate, `SPAdj`, and active trap bytes, and fail closed
if LLVM tries to describe an FP/base-pointer location.  This generalizes the
ARM mechanism to AMD64 rather than adding a frontend-root fallback or preserving
old x86 behavior.

Validation:

- `make -C _build/llvm-tools -j8 llc opt` passed.
- Clean optimized-wrapper rebuild passed:
  `LLVM_WRAPPER_LLC_OPT_LEVEL=3 make -s llvm-compiler
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`.
- `make -s llvm-install LLVM_WRAPPER_LLC_OPT_LEVEL=3
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed, with only
  same-file `cp` warnings.
- `make -s llvm-test-one-no-rebuild LLVM_WRAPPER_LLC_OPT_LEVEL=3
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" DIR=llvm-stack-checks`
  passed: 8 passed, 2 skipped, 0 failed.
- `make -s llvm-test-one-no-rebuild LLVM_WRAPPER_LLC_OPT_LEVEL=3
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" DIR=llvm-gc-roots`
  passed: 12 passed, 6 skipped, 0 failed.

Build-state note: `llvm-test-one-no-rebuild` uses `_runtest/ocamlopt.opt`,
which points at `_install/bin/ocamlopt.opt`.  Rebuilding only
`make llvm-compiler` leaves `_install` stale; that stale compiler reproduced
`compile_challenges_amd64.ml` stack overflow even though direct replay with
`_build/install/main/bin/ocamlopt.opt` passed.  Running `make llvm-install`
fixed the focused stack-check result.

Remaining validation issues not fixed by this patch:

- `make -s llvm-test-one-no-rebuild ... DIR=llvm-codegen` reached 58 passed,
  30 skipped, 2 failed.  `raw_stack_word_amd64.ml` failed from stale
  `_runtest/otherlibs/stdlib_upstream_compatible` CMIs disagreeing with the
  rebuilt runtime stdlib; refreshing `_runtest` through `llvm-test-one`
  currently needs default Dune build-context repair after the clean LLVM build.
- `stack_check_size_contract_amd64.ml` failed with
  `noalloc_outgoing_stack_args: expected LLVM prologue stack check`; inspect
  this next against the native AMD64 stack-check contract before changing the
  test or frame lowering.

2026-06-27 AMD64 stack-check contract cleanup:
inspected `stack_check_size_contract_amd64.ml` after the statepoint
SP-relative change.  The failing `noalloc_outgoing_stack_args` case emits an
ordinary CFG stack check for 352 bytes before the late x86 outgoing
stack-argument subtraction.  Forcing `X86FrameLowering` to add
`getMaxCallFrameSize()` to the prologue-check prefix made the next clean LLVM
boot fail with widespread `caml_scan_stack: missing frame descriptor` crashes,
so that experiment was rejected.  The correct normal-mode contract is that the
ordinary CFG stack check covers outgoing C stack arguments; prologue checks
only cover stack use before an ordinary CFG check can run.  The
`no_cfg_stack_checks` variant still verifies the old/no-CFG mode gets the
prologue check.

Build-state note: after a failed LLVM self-boot, break the cycle with a native
boot compiler while still building the main compiler with LLVM:
`make -s llvm-compiler LLVM_BOOT_BACKEND=0
LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`, followed by
`make -s llvm-install LLVM_BOOT_BACKEND=0
LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`.  Keep
`$PWD/_build/llvm-tools/bin` on `PATH`; otherwise the wrapper may find a
system `opt` that rejects `oxcaml_fpcc` IR.

Validation after the cleanup, with `LLVM_WRAPPER_LLC_OPT_LEVEL=3`,
`LLVM_BOOT_BACKEND=0`, and `_build/llvm-tools/bin` first on `PATH`:

- `make -C _build/llvm-tools -j8 llc opt` passed.
- `make -s llvm-compiler ...` passed.
- `make -s llvm-install ...` passed, with only same-file `cp` warnings.
- `make -s llvm-test-one-no-rebuild ... DIR=llvm-stack-checks` passed:
  8 passed, 2 skipped, 0 failed.
- `make -s llvm-test-one-no-rebuild ... DIR=llvm-gc-roots` passed:
  12 passed, 6 skipped, 0 failed.
- `make -s llvm-test-one-no-rebuild ... DIR=llvm-codegen` passed:
  60 passed, 30 skipped, 0 failed.

2026-06-27 AMD64 clean self-stage build-state cleanup:
clean `llvm-self-stage-install` runs can remove `_build/default` while the
boot workspace still expects generated default-context include files.  Added a
`bootstrap-default-dune-includes` make target, called before `boot-compiler`,
to materialize the generated default-context files that the boot workspace
references: the CamlinternalQuote probe result, project-root flag sexps, and
the local Flambda2 algorithms `ocamlopt_flags.sexp`.

Validation from a clean boot/default context:

- Removed `_build/default`, `_build/_bootinstall`, `_build/.db`, and
  `_build/.filesystem-clock`.
- Ran `make -s llvm-self-stage-install
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` with
  `LLVM_WRAPPER_LLC_OPT_LEVEL=3` and `_build/llvm-tools/bin` first on `PATH`.
- The build no longer failed on missing generated Dune include files.  It
  reached the real AMD64 self-boot failure:
  `caml_scan_stack: missing frame descriptor ...`, ending in
  `Makefile.common-ox:116: boot-compiler`.

Next work should reduce that self-boot frame-descriptor failure with a
single-worker boot-context build and fix the AMD64 frame/frametable mechanism
by generalizing the current ARM LLVM backend design, not by adding frontend
roots or preserving the old x86 LLVM behavior.
