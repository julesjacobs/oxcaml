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

2026-06-27 AMD64 CFI/exception unwinding fix in progress on
`jujacobs/llvm-x86-plan`:

- Fixed AMD64 OxCaml trap push/pop expansion to emit DWARF CFA offset
  adjustments for the two trap words, matching the real `%rsp` movement during
  active exception-handler chains.
- Switched generated AMD64 OxCaml frame CFI to the native AMD64-style
  `%rsp`-based CFA model instead of exposing `%rbp` as the canonical frame
  register. This preserves native unwinding behavior around generated OCaml
  frames while leaving non-OxCaml x86 code on LLVM's existing frame-pointer CFI
  path.
- Added post-statepoint `nop` padding after OxCaml `caml_raise_exn` and
  `caml_reraise_exn` calls. Native AMD64 keeps terminal raises inside the
  function FDE range this way; without it, GDB sees the call return address at
  the half-open function end and falls back to bad frame-pointer heuristics.
- Addressed gpt-5.5-high review feedback that the new `%rsp`-based CFA mode
  also has to follow the no-frame-pointer callee-saved push/pop CFA update
  paths. The fix accounts for the extra saved `%rbp` slot in generated OxCaml
  frames and keeps the existing non-OxCaml x86 frame-pointer CFI behavior.
- Addressed follow-up review feedback by restoring the saved `%rbp`
  `.cfi_offset` rule while keeping `%rsp` as the CFA register in normal OxCaml
  frames, and by falling back to LLVM's frame-pointer CFI path for stack
  realignment or variable-sized-object frames where `%rsp + constant` is not a
  valid caller-CFA description.
- Addressed final review feedback by gating trap push/pop CFA adjustments on
  the same `%rsp`-CFA eligibility. Realigned or dynamic OxCaml frames now keep
  the frame-pointer CFI fallback clean while normal generated OxCaml frames
  still track the two active-trap stack words precisely.
- Addressed another review finding by applying the same `%rsp`-CFA eligibility
  to call-frame pseudo lowering. OxCaml frames with outgoing stack arguments
  now emit CFA updates for temporary call-argument stack adjustments, matching
  the existing no-frame-pointer x86 path.

Validation:

- `cmake --build _build/llvm-tools --target llc -- -j8` passed.
- Manual `tests/native-cfi-stepping/test_cfi.ml` LLVM-backend GDB reproducer
  passed (`ok`) before and after the callee-saved CFI review fix.
- First `make -s llvm-install LLVM_PATH=...` failed because `_build/llvm-tools/bin`
  was not first on `PATH`; system `opt` could not parse `oxcaml_fpcc`. Rerunning
  with `PATH="$PWD/_build/llvm-tools/bin:$PATH"` then exposed stale Dune
  interface assumptions from the bad invocation.
- After the documented stale-state cleanup
  (`rm -rf _build/main _build/default _build/boot _build/_bootinstall _build/.db
  _build/.filesystem-clock _runtest`), `PATH="$PWD/_build/llvm-tools/bin:$PATH"
  make -s llvm-install LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed.
- After clearing `_build/default`, `_build/_bootinstall`, Dune metadata, and
  `_runtest`, `PATH="$PWD/_build/llvm-tools/bin:$PATH" make -s install_for_test
  LLVM_BACKEND=1 LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed.
- Post-clean focused tests passed:
  `native-cfi-stepping` 6/0/0, `llvm-codegen` 67/30/0,
  `llvm-gc-roots` 12/6/0, `llvm-stack-checks` 8/2/0,
  `syntactic-arity` 24/0/0, and `async-exns` 5/0/0.
- After the callee-saved CFI review fix, `cmake --build _build/llvm-tools
  --target llc -- -j8` passed, the manual CFI reproducer still passed, and the
  focused tests still passed: `native-cfi-stepping` 6/0/0,
  `llvm-codegen` 67/30/0, `llvm-gc-roots` 12/6/0, `llvm-stack-checks` 8/2/0,
  `syntactic-arity` 24/0/0, and `async-exns` 5/0/0.
- A later `llvm-install` without cleanup hit the known stale
  `CamlinternalQuote` interface state again; after the same documented cleanup,
  `PATH="$PWD/_build/llvm-tools/bin:$PATH" make -s llvm-install
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed, and the final manual
  CFI reproducer with the freshly installed compiler passed (`ok`).
- After the follow-up review fixes, `cmake --build _build/llvm-tools --target
  llc -- -j8` passed. Small `llc` probes confirmed that a normal `oxcaml_fpcc`
  frame emits `%rsp`-CFA rows plus `.cfi_offset %rbp, -16`, while an
  `alignstack(32)` `oxcaml_fpcc` frame falls back to `.cfi_def_cfa_register
  %rbp` across the stack realignment.
- After refreshing `install_for_test`, focused tests passed again:
  `native-cfi-stepping` 6/0/0, `llvm-codegen` 67/30/0,
  `llvm-gc-roots` 12/6/0, `llvm-stack-checks` 8/2/0,
  `syntactic-arity` 24/0/0, and `async-exns` 5/0/0.
- A final documented-cleanup
  `PATH="$PWD/_build/llvm-tools/bin:$PATH" make -s llvm-install
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed after the last source
  edits. The final manual CFI reproducer with the freshly installed compiler
  passed (`ok`).
- After the trap-CFI gate, `cmake --build _build/llvm-tools --target llc -- -j8`
  passed, the manual CFI reproducer passed, and the small normal/re-aligned
  `llc` CFI probes still showed the expected `%rsp`-CFA and frame-pointer-CFA
  split. After refreshing `install_for_test`, focused tests passed again:
  `native-cfi-stepping` 6/0/0, `llvm-codegen` 67/30/0,
  `llvm-gc-roots` 12/6/0, `llvm-stack-checks` 8/2/0,
  `syntactic-arity` 24/0/0, and `async-exns` 5/0/0.
- Final documented-cleanup
  `PATH="$PWD/_build/llvm-tools/bin:$PATH" make -s llvm-install
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed after the trap-CFI
  gate. The final manual CFI reproducer with the freshly installed compiler
  passed (`ok`).
- After the call-frame CFI fix, `cmake --build _build/llvm-tools --target llc
  -- -j8` passed. A direct `llc` probe for an `oxcaml_fpcc` caller making an
  `oxcaml_ccc` call with outgoing stack arguments emitted CFA rows around the
  stack-argument adjustment (`.cfi_def_cfa_offset 32` after `subq $16, %rsp`
  and back to 16 after `addq $16, %rsp`).
- After refreshing `install_for_test`, focused tests passed again:
  `native-cfi-stepping` 6/0/0, `llvm-codegen` 67/30/0,
  `llvm-gc-roots` 12/6/0, `llvm-stack-checks` 8/2/0,
  `syntactic-arity` 24/0/0, and `async-exns` 5/0/0.
- Final documented-cleanup
  `PATH="$PWD/_build/llvm-tools/bin:$PATH" make -s llvm-install
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed after the call-frame
  CFI fix. The final manual CFI reproducer with the freshly installed compiler
  passed (`ok`).

2026-06-27 AMD64 native trap-depth fix:
high-arity calls inside active exception handlers were overwriting AMD64 LLVM
native trap records because outgoing stack-argument stores used `%rsp` offsets
that did not account for the two trap words pushed by `OXCAML_PUSH_TRAP`.
Rejected the fixed-trap-frame-object experiment because it created a separate
mechanism from AArch64 and broke clean runtime stdlib builds. The committed
direction is the AArch64 mechanism generalized to AMD64: keep native trap
push/pop records, keep active-trap-depth analysis, and adjust SP-relative
stack memory operands carrying stack pseudo memory operands by the active trap
byte count before frame indices are replaced.

Validation:

- `cmake --build _build/llvm-tools --target llc -- -j8` passed.
- Direct installed-compiler repro for
  `testsuite/tests/syntactic-arity/max_arity.ml` with `-llvm-backend` now
  prints `f () (): Exception.` instead of segfaulting.
- After clearing stale dune build state, `make -s llvm-install
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed.
- `make -s llvm-test-one-no-rebuild DIR=syntactic-arity
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed: 24 passed.
- `make -s llvm-test-one-no-rebuild DIR=async-exns
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed: 5 passed.
- `make -s llvm-test-one-no-rebuild DIR=ast-invariants
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed: 2 passed.

2026-06-27 AMD64 LLVM SIMD-preserving GC slow paths:
reduced the `tests/unboxed-primitive-args` failures to heap allocation slow
paths after C calls returning `float32`/SIMD values. The direct C-call ABI was
already correct; the bug was that AMD64 LLVM called plain `caml_call_gc` before
boxing the returned XMM/YMM value, clobbering the return register. The fix makes
AMD64 LLVM allocation/poll slow paths call the existing native AMD64
SIMD-preserving helper family (`caml_call_gc_sse`, `_avx`, `_avx512`) while
leaving AArch64 on `caml_call_gc`. AMD64 local realloc slow paths now use the
matching `caml_call_local_realloc*` helper family. These LLVM runtime calls use
a conservative target-feature save width, widened by live SIMD registers, so
they preserve just-returned SIMD values that are not present in the allocation
instruction's liveness. Stack realloc helper selection was refactored to use the
same AMD64 save-class helper instead of string suffix construction, but remains
live-set based like the native AMD64 stack-check path.
The X86 LLVM OxCaml calling convention also now bit-converts `iPTR` to `i64` for
arguments/results, matching native AMD64's `Val`/`Addr`/`Int` GPR class and the
existing AArch64 OxCaml pointer convention.

Validation for the SIMD-preserving GC patch, with
`PATH="$PWD/_build/llvm-tools/bin:$PATH"` and
`LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`:

- `make -s llvm-install LLVM_PATH=...` passed after clearing stale dune build
  contexts.
- `make -s install_for_test LLVM_BACKEND=1 LLVM_PATH=...` passed.
- `make -s llvm-test-one-no-rebuild LLVM_PATH=... DIR=unboxed-primitive-args`
  passed: 8 passed, 1 skipped.
- `make -s llvm-test-one-no-rebuild LLVM_PATH=... DIR=llvm-gc-roots` passed:
  12 passed, 6 skipped.
- `make -s llvm-test-one-no-rebuild LLVM_PATH=... DIR=llvm-stack-checks`
  passed: 8 passed, 2 skipped.
- `make -s llvm-test-one-no-rebuild LLVM_PATH=... DIR=llvm-codegen` passed:
  67 passed, 30 skipped.
- `make -s llvm-test-one-no-rebuild LLVM_PATH=... DIR=typing-layouts-arrays`
  passed: 134 passed.
- `make -s llvm-test-one-no-rebuild LLVM_PATH=... DIR=typing-layouts-iarrays`
  passed: 81 passed.

Next full-suite run should confirm the previous 30-failure standard
`llvm-test-no-rebuild` result is reduced. Expected remaining real clusters are
native CFI stepping, syntactic max-arity segfault, and any residual harness
issues around internal assembler tests.

2026-06-27 AMD64 LLVM `Cpackf32` selection fix:
`tests/typing-layouts-arrays/test_float32_u_array.ml` failed in the standard
LLVM backend with `Fatal error: Selection.select_oper`. The failing operation
was `Cpackf32`: ARM64 keeps selecting its normal semantic pack operation in
LLVM mode (`Zip1_f32`), but AMD64 LLVM mode bypassed the normal `Ipackf32`
rewrite and fell through to the generic fatal case even though Llvmize already
lowers `Amd64_packf32`. The fix is to keep selecting `Ipackf32` for AMD64 LLVM
mode, matching the ARM pattern of preserving the backend semantic operation and
letting Llvmize lower it.

Validation:

- `PATH="$PWD/_build/llvm-tools/bin:$PATH" make -s llvm-compiler \
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed after clearing stale
  `_build/default`, `_build/_bootinstall`, `_build/.db`, and
  `_build/.filesystem-clock`.
- Manual fresh-compiler compile and link/run of
  `gen_u_array.ml`, `test_gen_u_array.ml`, and `test_float32_u_array.ml` with
  `-extension layouts_beta -llvm-backend` passed using
  `_build/install/main/bin/ocamlopt.opt` and the existing `_runtest` stdlib.
- After `llvm-install`, manually refreshed `_runtest` from `_install` without
  invoking the plain non-LLVM `install_for_test` prerequisite path, then
  `PATH="$PWD/_build/llvm-tools/bin:$PATH" make -s llvm-test-one-no-rebuild \
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" \
  TEST=testsuite/tests/typing-layouts-arrays/test_float32_u_array.ml` passed:
  5 passed, 0 failed.
- `make -C _build/llvm-tools -j8 llc opt` passed.

Build-state note: `make -s llvm-test-one ... test_float32_u_array.ml` still
routes through the plain `install_for_test` prerequisite, which attempts a
non-LLVM boot rebuild in this checkout and fails before running the test. The
usable path after a fresh LLVM install is the no-rebuild harness with `_runtest`
synced from `_install`.

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

2026-06-27 clean generated-include overwrite fix:
the bootstrap include materialization target also has to handle the case where
Dune already generated the files read-only.  It now removes the generated probe
and flag sexps before rewriting/copying them.

Validation:

- After the `rm -f` cleanup, `make -s llvm-install LLVM_BOOT_BACKEND=0
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed, with only same-file
  `cp` warnings.

2026-06-27 AMD64 no-dynamic-realignment fix:
the clean self-stage frame-descriptor failure reduced to LLVM-generated AMD64
functions using dynamically realigned frames (`push %rbp; mov %rsp,%rbp; and
$-32,%rsp; sub ...,%rsp`).  The OxCaml frametable records a static frame size,
so that dynamic padding cannot be described correctly during stack scanning.
The fix is to give all x86-64 LLVM-generated OxCaml functions the LLVM
`"no-realign-stack"` function attribute, alongside the existing AMD64 red-zone
handling.  This prevents MachineFrameInfo from creating a dynamically realigned
frame in the first place, matching the native AMD64 backend's static stack
model rather than adding a separate x86 LLVM mechanism.

Validation with `LLVM_WRAPPER_LLC_OPT_LEVEL=3`,
`LLVM_BOOT_BACKEND=0`, and `_build/llvm-tools/bin` first on `PATH`:

- Clean `make -s llvm-compiler ...` passed after removing `_build/default`,
  `_build/_bootinstall`, `_build/.db`, and `_build/.filesystem-clock`.
- `make -s llvm-install ...` passed, with only same-file `cp` warnings.
- `make -s llvm-test-one ... TEST=llvm-codegen/no_realign_stack_attr.ml`
  passed: 4 passed, 0 failed.
- `make -s llvm-test-one-no-rebuild ... DIR=llvm-stack-checks` passed:
  8 passed, 2 skipped, 0 failed.
- `make -s llvm-test-one-no-rebuild ... DIR=llvm-gc-roots` passed:
  12 passed, 6 skipped, 0 failed.
- `make -s llvm-test-one-no-rebuild ... DIR=llvm-codegen` passed:
  64 passed, 30 skipped, 0 failed.
- Clean `make -s llvm-self-stage-install
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed and produced
  `_llvm_self_stage_install/bin/ocamlopt.opt`.

Caveat: the self-stage wrapper diagnostics still printed zero wrapper/fresh-IR
counts.  The target completed successfully and got past the previous missing
frame-descriptor blocker, but the wrapper-count instrumentation should be
understood before treating the self-stage result as full LLVM coverage.

Post-commit validation at branch head: `make -C _build/llvm-tools -j8 llc opt`
passed.

2026-06-27 wrapper-count validation fix:
the self-stage scripts were counting `$wrapper.log` by default but not exporting
that path as `LLVM_WRAPPER_LOG`, while the checked-in RS4GC wrapper only logs
when that environment variable is set.  Exporting the resolved log path in the
boot, stage5, self-stage, and stage5 ocamltest helpers makes the diagnostics
measure real LLVM backend use instead of silently reporting zero.

Validation:

- `bash -n tools/build-llvm-boot-with-installed.sh
  tools/build-llvm-stage5-install.sh tools/build-llvm-self-stage-install.sh
  tools/run-llvm-stage5-ocamltest.sh` passed.
- A first `make -s llvm-self-stage-install ...` rerun exposed stale build
  state before self-stage: `_install/lib/ocaml/stdlib.cmxa` disagreed with
  boot artifacts over `CamlinternalQuote`.
- Repaired the build state by removing `_build/default`, `_build/_bootinstall`,
  `_build/.db`, and `_build/.filesystem-clock`, then running
  `make -s llvm-install LLVM_BOOT_BACKEND=0
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`; it passed with only
  same-file `cp` warnings.
- `LLVM_WRAPPER="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"
  tools/build-llvm-self-stage-install.sh` passed with real wrapper counts:
  boot 1678 wrapper lines / 834 fresh IR inputs, smoke 4 / 2, runtime 148 / 74,
  main 2228 / 1097, self-stage smoke 4 / 2.

Next validation step: run the LLVM testsuite using the self-stage compiler now
that wrapper coverage diagnostics are trustworthy.

2026-06-27 self-stage testsuite harness cleanup:
a full self-stage testsuite run was started with
`SELF_STAGE=1 LLVM_WRAPPER="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"
tools/run-llvm-stage5-ocamltest.sh`.  It was stopped after reaching
`typing-layouts-*` because the generated list included test output directories
named `_ocamltest`, causing duplicate nested test runs and noisy failures from
generated roots.  The useful failures seen before stopping include missing
AMD64 SIMD lowering (`vinsertf128`), `Selection.select_oper` in float32 array
native tests, native CFI stepping output mismatches, `templates/basic/probe.ml`
missing LLVM lowering for `probe`, and a few likely self-stage-only runtime
failures that still need focused reduction against the standard
`-llvm-backend` compiler.

Harness fix: `tools/run-llvm-stage5-ocamltest.sh` now exports the
same wrapper as `LLVM_PATH` for shell tests that do not read `OCAMLPARAM`, and
its generated test list prunes `_ocamltest` directories.  This is intended to
make reruns measure the AMD64 LLVM backend rather than stale generated output.
The focused rerun then showed `stack_check_size_contract_amd64.ml` passing and
left only the known `raw_stack_word_amd64.ml` inconsistent-CMI failure in
`llvm-codegen`; `raw_stack_word.sh` now prefers the active `OCAMLLIB` for both
the stdlib and `stdlib_upstream_compatible` so self-stage tests do not mix a
normal-build runtime stdlib with self-stage libraries.

Validation:

- `bash -n tools/run-llvm-stage5-ocamltest.sh
  testsuite/tests/llvm-codegen/raw_stack_word.sh` passed.
- The generated self-stage test list has 400 entries and 0 `_ocamltest`
  entries.
- Focused self-stage testsuite rerun with `tests/llvm-stack-checks` and
  `tests/llvm-codegen` passed: 72 passed, 32 skipped, 0 failed, with 109
  wrapper lines and 56 fresh IR inputs.

2026-06-27 AMD64 SIMD `vinsertf128`/`vextractf128` fix:
added LLVM lowering for AVX 128-bit lane extraction and insertion on 256-bit
vectors.  The lowering treats AMD64 `Vec256` as the existing LLVM `<4 x i64>`
representation and AMD64 `Vec128` as `<2 x i64>`, matching native AMD64
selection semantics: `vextractf128 imm, src` returns 128-bit lane `imm & 1`,
and `vinsertf128 imm, base, inserted` replaces the corresponding 128-bit lane
of `base`.

Validation:

- `bash -n testsuite/tests/llvm-codegen/amd64_simd_smoke.sh` passed.
- After restoring build state with a fresh native `make -s install`,
  `make -s llvm-install LLVM_BOOT_BACKEND=0
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed with only same-file
  copy warnings.
- `make -s llvm-test-one-no-rebuild ... TEST=testsuite/tests/llvm-codegen/amd64_simd_smoke.ml`
  passed: 3 passed, 0 failed.
- `make -s llvm-test-one-no-rebuild ... TEST=testsuite/tests/typing-layouts-arrays/test_int_or_null_array.ml`
  passed: 4 passed, 0 failed.
- `make -C _build/llvm-tools -j8 llc opt` passed.

2026-06-27 AMD64 C-stack-args root test cleanup:
the `amd64_c_stack_args_roots` generated program and IR contract were correct:
the program printed `ok`, the IR contained the `caml_c_call_stack_args`
statepoint, and the generated assembly restored `%rsp` after the helper call.
The test failed because its awk check required the saved `%rsp` value to remain
in the original register.  Current LLVM can spill the saved stack pointer and
reload it before `stackrestore`, which is valid.  Relaxed the assembly check to
accept direct register restore, spill/reload restore, or direct spill-slot
restore while still requiring the restore after `caml_c_call_stack_args`.
Code review caught the direct spill-slot restore case before commit.

Build-state repair: a pre-commit `make -s llvm-compiler` initially failed with
stale `_install`/boot artifacts disagreeing over `CamlinternalQuote`.  Clearing
`_build/default`, `_build/_bootinstall`, `_build/.db`, and
`_build/.filesystem-clock` fixed the stale state, and the rerun passed.

Validation:

- `make -s llvm-compiler LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`
  passed after clearing stale build state.
- `make -s llvm-test-one-no-rebuild LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" \
  TEST=testsuite/tests/llvm-codegen/amd64_c_stack_args_roots.ml` passed:
  3 passed, 0 failed.
- `make -s llvm-test-one-no-rebuild LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" \
  DIR=llvm-codegen` passed: 64 passed, 30 skipped, 0 failed.
- `make -C _build/llvm-tools -j8 llc opt` passed.

2026-06-27 mixed-blocks frame-descriptor investigation:
`testsuite/tests/mixed-blocks/generated_native_test.ml` now passes with the
current standard installed compiler under `-llvm-backend`; the failure only
reproduced with the existing `_llvm_self_stage2_install`, which was stale
relative to later AMD64 stack-frame commits. A mechanical reducer for the stale
self-stage2 failure narrowed the abort
(`caml_scan_stack: missing frame descriptor retaddr=(nil)`) to the first live
`int64x4#` mixed-block record after a sequence of flattened-float records, with
all values kept live across `Gc.full_major`. The failing stale assembly used
dynamic stack realignment in the OCaml frame:
`push %rbp; mov %rsp,%rbp; and $-32,%rsp; sub ...,%rsp`. Recompiling the same
reduced prefix with the current standard compiler emits `no-realign-stack`, has
no dynamic `%rsp` realignment in the OCaml frame, and runs successfully.

Follow-up validation after deleting the stale self-stage build/install
directories and rebuilding from the current branch: both stage1 and stage2
LLVM self-stage installs completed. Stage1 wrapper counts were boot 826 fresh
IR inputs, runtime 73, main 1099, final smoke 2; stage2 wrapper counts were
boot 833, runtime 73, main 1098, final smoke 2. All smoke programs printed
`55`. The focused self-stage2 mixed-blocks run then passed:
`SELF_STAGE=2 GENERATE_LIST=0 LIST=/tmp/llvm-self-stage2-mixed-blocks-list.txt
LLVM_TESTSUITE_PARALLEL=0 LLVM_WRAPPER="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"
tools/run-llvm-stage5-ocamltest.sh` with `tests/mixed-blocks` in the list
reported 43 passed, 2 skipped, 0 failed. This clears the prior
`generated_native_test.ml` frame-descriptor abort as stale build state, not a
current AMD64 LLVM backend failure.

2026-06-27 wrapper `llc` optimization level fix for AMD64 stack checks:
the fresh self-stage2 stack-check run exposed a real toolchain problem in
`tests/llvm-stack-checks/compile_challenges_amd64.ml`: compiling the generated
deep-let stress file with `OCAMLRUNPARAM=b,l=2000000` raised `Stack overflow`.
The reduced reproducer was normal compiler compilation of a generated file, not
frontend roots and not a second AMD64-specific stack-check mechanism.

The failing compiler object had a huge `type_expect_` frame:
`lea -0x9120(%rsp), %r10` and `sub $0x8f90, %rsp`.  Keeping IR showed the
wrapper's `opt` pipeline reduced the pre-opt IR from 5550 allocas to 12, but
then the wrapper ran `llc -O0` by default even when the compiler passed `-O3`.
Manual codegen of the same optimized IR produced the bad ~36KB frame at
`llc -O0`, while `llc -O3` produced an ordinary sub-KB frame.  The wrapper now
defaults `llc` to `-O3`, maps recognized clang-style `-O*` arguments to the
corresponding `llc` optimization level, and still lets
`LLVM_WRAPPER_LLC_OPT_LEVEL` override the choice for diagnostics.

Validation:

- `bash -n tools/llvm-rs4gc-llc-wrapper.sh` passed.
- Direct shim testing of wrapper argument parsing confirmed the default
  `llc -O3` behavior, bare `-O`/`-O1` mapping to `llc -O1`,
  `-O2`/`-Os`/`-Oz` mapping to `llc -O2`, `-O3`/`-O4`/`-Ofast` mapping to
  `llc -O3`, and `LLVM_WRAPPER_LLC_OPT_LEVEL` overriding a later `-O3`.
- Direct wrapper codegen of the saved `typecore.ll` defaulted to optimized
  codegen and emitted `type_expect_` with `leaq -688(%rsp), %r10` rather than
  the previous `-37152` check.
- After clearing stale main build state, `make -s llvm-compiler
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed, and direct
  compilation of the reduced deep-let file with
  `_build/install/main/bin/ocamlopt.opt` passed under
  `OCAMLRUNPARAM=b,l=2000000`.
- After clearing stale install state, `make -s llvm-install
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` passed; the installed
  `type_expect_` prologue used `lea -0x2b0(%rsp), %r10`, and the same direct
  deep-let compilation passed with `_install/bin/ocamlopt.opt`.
- `make -s llvm-test-one-no-rebuild
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" DIR=llvm-stack-checks`
  passed: 8 passed, 2 skipped, 0 failed.
- `make -s llvm-test-one-no-rebuild
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" DIR=llvm-codegen` passed:
  64 passed, 30 skipped, 0 failed.
- A fresh stage1 LLVM self-stage install and fresh stage2 LLVM self-stage
  install both passed with real wrapper coverage.  Stage1 wrapper counts were
  boot 826 fresh IR inputs, runtime 73, main 1103, final smoke 2; stage2 counts
  were boot 829, runtime 73, main 1096, final smoke 2.  All smoke programs
  printed `55`.
- Focused self-stage2 `tests/llvm-stack-checks` passed with
  `SELF_STAGE=2 GENERATE_LIST=0 LLVM_TESTSUITE_PARALLEL=0
  LLVM_WRAPPER="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`: 8 passed, 2 skipped,
  0 failed, with 14 wrapper lines and 7 fresh IR inputs.
- `make -C _build/llvm-tools -j8 llc opt` passed.
- After the review-requested `-O`/`-Ofast` mapping cleanup,
  `PATH="$PWD/_build/llvm-tools/bin:$PATH" make -s
  llvm-test-one-no-rebuild LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"
  DIR=llvm-stack-checks` passed again: 8 passed, 2 skipped, 0 failed.  Running
  without the repo LLVM tools on `PATH` fails with stock `opt` rejecting
  `oxcaml_fpcc`, which is an invocation/environment error rather than a backend
  regression.

2026-06-27 AMD64 LLVM probe lowering in progress:
the remaining `probe` failure from the self-stage2 suite was reproduced against
the current standard installed compiler with
`testsuite/tests/templates/basic/probe.ml`, which failed in LLVMize with
`unimplemented instruction: probe`.  The implementation now follows the native
AMD64 semaphore naming and layout (`caml_probes_semaphore_<name>` containing
two `i16`s, with the OCaml enable flag at byte offset 2), and lowers
`Probe_is_enabled` to a volatile aligned load of that flag.  `Probe`
terminators are lowered as a branch on the same flag to an ordinary OxCaml
handler call, so handler calls use the existing LLVM call/statepoint,
live-root, and trap/unwind machinery rather than adding a second
AMD64-specific frontend-root mechanism.

Current limitations to keep in mind: this is not yet the full native optimized
probe/USDT patch-site implementation.  LLVM output defines weak hidden
semaphore globals in `.probes` and preserves OCaml `enabled_at_init` semantics,
but it does not yet emit native probe notes or byte-patchable call sites.
That is acceptable as incremental backend progress, but full probe parity still
requires note emission if external dynamic probe enabling is in scope.

Validation:

- Restored a usable stage0 `_install` from the fresh `_llvm_self_stage2_install`
  after accidentally deleting the stale `_install`; `_install/bin/ocamlopt.opt
  -config` reports `standard_library: $PWD/_install/lib/ocaml`.
- After clearing `_build/default`, `_build/_bootinstall`, `_build/.db`, and
  `_build/.filesystem-clock`, `PATH="$PWD/_build/llvm-tools/bin:$PATH"
  make -s llvm-install LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`
  passed with only same-file copy warnings.
- Direct installed-compiler repro of `testsuite/tests/templates/basic/probe.ml`
  passed under `-llvm-backend -llvm-path
  "$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`.
- Direct installed-compiler repro of
  `testsuite/tests/typing-layouts-or-null/probe.ml` passed under the same LLVM
  backend flags.
- Inspecting `probe.ll`/`probe.s` for the template repro showed a load from
  `caml_probes_semaphore_probe + 2`, a conditional branch, an
  `oxcaml_fpcc` statepoint call to the generated probe handler, and a weak
  hidden `.probes` semaphore definition.  A code review caught that the
  semaphore load must be volatile because the flag can be changed outside
  LLVM-visible IR; after adding an aligned volatile load helper, the emitted IR
  contains `load volatile i16, ptr ..., align 2`.
- Direct runtime smokes passed for `~enabled_at_init:true`, `false`,
  `probe_is_enabled`, a heap root live across `Gc.full_major ()` in an enabled
  probe handler, and exception propagation from an enabled handler.
- Added `testsuite/tests/llvm-codegen/amd64_probes.ml`; direct installed-compiler
  compile and run of that exact test passed with `-O3 -llvm-backend`.
- `make -C _build/llvm-tools -j8 llc opt` passed, confirming the vendored LLVM
  tools still build after the wrapper/build-state cleanup.
- The ordinary `make llvm-test-one` path currently needs `_runtest` repair in
  this checkout after the build-state cleanup; invoking it tried to rebuild the
  default boot compiler and failed on generated-source/fallback files before
  reaching the test.  Use direct installed-compiler repros or the stage harness
  with a coherent stage build until `_runtest` is refreshed.

2026-06-27 AMD64 slow-path SIMD preservation validation:
after commit `43056b8f7e` (`Preserve AMD64 SIMD regs in LLVM GC slow paths`),
the standard installed LLVM-backend test suite was rebuilt with the repo LLVM
tools on `PATH` and run with
`make -s llvm-test-no-rebuild
LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`.

The full standard run improved from the earlier 30-failure baseline to:

- 6806 passed
- 312 skipped
- 4 failed

Resolved failure clusters:

- `tests/typing-layouts-arrays` passed in the full run, including vector,
  product, scannable product, nullable product, and unboxed scalar array cases.
- `tests/typing-layouts-iarrays` passed in the full run, including moving-GC,
  product, scannable product, nullable product, and unboxed scalar iarray cases.
- `tests/unboxed-primitive-args` passed in the full run on AMD64.
- Additional relevant GC/local/stack sanity passed in the same run, including
  `tests/statmemprof`, `tests/runtime-errors/stackoverflow.ml`,
  `tests/typing-local/localgcbug.ml`, `tests/typing-local`, weak/ephemeron
  tests, vector array tests, and AMD64 layout `caml_modify` cases.

Remaining standard LLVM failures:

- `tests/asmcomp/movsx_small_ints.ml` line 8: native assembler-output harness
  expects `movsx_small_ints.s`; LLVM backend does not leave the expected `.s`.
- `tests/asmcomp/shift_mem_cl.ml` line 4: same assembler-output harness shape.
- `tests/native-cfi-stepping/test_cfi.ml` line 12: `gdb` output differs from
  reference, with repeated backtrace failures around `caml_raise_exn`.
- `tests/syntactic-arity/max_arity.ml` line 166: generated executable prints
  `f (): No exception.` then segfaults.  This is the next substantive AMD64
  backend target and likely belongs in calling-convention / stack-argument
  lowering; compare the ARM LLVM backend generalization and the native AMD64
  backend rather than preserving old LLVM AMD64 behavior.

Next implementation priority:

1. Fix `syntactic-arity/max_arity.ml` by auditing AMD64 OxCaml calling
   convention lowering for high arity, stack arguments, tail calls, and partial
   application paths against native AMD64.
2. Fix CFI/exception stepping so LLVM AMD64 has native-quality unwind metadata
   around exception paths, especially `caml_raise_exn`.
3. Treat the two `asmcomp` failures as test-harness/codegen-output integration
   unless reduced evidence shows a semantic backend issue.

2026-06-27 AMD64 high-arity/trap-frame investigation:

- Reduced `tests/syntactic-arity/max_arity.ml` with the installed compiler:
  LLVM AMD64 printed `f (): No exception.` and then jumped to address `0x1`
  during `f () ()`; native AMD64 printed `f () (): Exception.` and exited
  normally.
- The generated partial closure layout was correct and matched native:
  `caml_curry7`, arity info, direct code pointer, then captured values.  The
  direct function entered with the expected closure in `r9` and took the same
  missing-optional-argument reraise path as native.
- The immediate corruption was in the caller's active trap frame.  LLVM emitted
  `push handler; push Caml_state->exn_handler; mov rsp, Caml_state->exn_handler`
  like native, but then wrote high-arity outgoing stack arguments at `0(%rsp)`,
  overwriting the trap frame.  `caml_reraise_exn` restored `rsp` from
  `Caml_state->exn_handler`, popped the previous handler, and returned through
  an overwritten word containing OCaml unit (`1`).
- A rejected LLVM experiment disabled reserved call frames for every machine
  function containing OxCaml native trap pseudos.  After rebuilding `llc`
  directly with `cmake --build _build/llvm-tools --target llc -- -j8`, this
  changed the high-arity call shape to materialize `subq $416` / `addq $416`
  around the stack-argument calls inside the trap.  The direct repro and
  `make -s llvm-test-one-no-rebuild
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" DIR=syntactic-arity` then
  passed (`24 passed, 0 failed`).
- That experiment is not viable as-is.  A partial full LLVM suite run showed
  new segfaults in `tests/ast-invariants/test.ml` and
  `tests/async-exns/async_exns_1.ml`.  The async-exns crash reduced to GC stack
  scanning during minor collection (`runtime/fiber.c:478`, `Hd_val(vblock)`),
  which means the broad call-frame change invalidates frame-table / stack-root
  assumptions around GC.  The source change was backed out before committing.
- Next fix should be narrower than "disable reserved call frames for any trap".
  It needs to preserve native AMD64's invariant that active trap frames are not
  overwritten by outgoing stack arguments while keeping LLVM's GC/frame-table
  model consistent.  Candidate directions: model only stack-argument calls
  inside active traps, or make the X86 trap pseudo/call-frame lowering reserve a
  trap-frame band without changing unrelated calls.  Re-run at least
  `syntactic-arity`, `async-exns`, and `ast-invariants` before any commit.

2026-06-28 normal LLVM suite after AMD64 trap/CFI fixes:

- Full standard installed-compiler LLVM run:
  `PATH="$PWD/_build/llvm-tools/bin:$PATH" make -s llvm-test-no-rebuild
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`.
- Result: 6809 passed, 312 skipped, 2 failed.  The previous substantive
  `native-cfi-stepping` and `syntactic-arity/max_arity.ml` failures passed in
  the full run, along with the AMD64 LLVM codegen, GC roots, stack checks,
  statmemprof callback, dynlink, local allocation, stack-allocation, layout,
  C-API, and weak/ephemeron native sections.
- Remaining failures were both native backend internal-assembler regression
  tests copied from `oxcaml/testsuite/tests/asmcomp`:
  `movsx_small_ints.ml` and `shift_mem_cl.ml`.  Both pass
  `-internal-assembler` and are specifically about `x86_binary_emitter.ml`;
  under `-llvm-backend`, the harness tried to assemble a native `.s` file that
  the LLVM backend path does not leave behind.
- Added `not-llvm-backend;` to both tests so they continue to run in native
  internal-assembler configurations but are skipped under the LLVM backend.
  Focused LLVM checks after refreshing the `_runtest` copies:
  `llvm-test-one-no-rebuild TEST=testsuite/tests/asmcomp/movsx_small_ints.ml`
  and `llvm-test-one-no-rebuild TEST=testsuite/tests/asmcomp/shift_mem_cl.ml`
  both passed with the tests skipped by the `not-llvm-backend` predicate.
  Non-LLVM focused `test-one-no-rebuild` runs for both files also passed and
  executed the native tests, preserving internal-assembler coverage.
- Next gate after committing this cleanup is a clean full normal LLVM suite
  rerun if needed, then self-stage2 build/test under LLVM.  After self-stage2
  passes, collect performance measurements against the native backend.

2026-06-28 committed-state normal LLVM suite rerun:

- Reran the full standard installed-compiler LLVM suite after committing the
  `not-llvm-backend` predicates for the two native internal-assembler tests:
  `PATH="$PWD/_build/llvm-tools/bin:$PATH" make -s llvm-test-no-rebuild
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`.
- Result: 6809 passed, 314 skipped, 0 failed, 0 unexpected errors.  The two
  `asmcomp` internal-assembler tests now skip only under LLVM, and the full
  AMD64 LLVM coverage still passes, including CFI stepping, exception paths, GC
  roots, stack checks, statmemprof callbacks, layout/C-API tests, local
  allocation, and weak/ephemeron sections.
- Next gate is LLVM self-stage2 build/test.  If a self-stage2-only failure
  appears, reduce it as far as possible and record why the standard
  `-llvm-backend` compiler does not cover it.

2026-06-28 LLVM self-stage2 build/test gate passed on
`jujacobs/llvm-x86-plan`.

Build-state recovery notes:

- Use the OxCaml switch explicitly:
  `eval "$(opam env --switch=oxcaml-5.4.0+oxcaml --set-switch)"`.
- Disable the shared Dune cache while recovering this checkout:
  `DUNE_CACHE=disabled`.
- Keep the local LLVM tools first on `PATH`:
  `PATH="$PWD/_build/llvm-tools/bin:$PATH"`.
- The phony `llvm-install` / `install_for_test` paths can rebuild stale or wrong
  Dune contexts after cleanup.  The successful recovery was: rebuild `_install`
  with `make -s install LLVM_BOOT_BACKEND=0`; rebuild the LLVM install with
  `make -s llvm-install`; run `tools/build-llvm-self-stage-install.sh` directly
  for stage1, then run it again for stage2 with explicit output directories:

`LLVM_WRAPPER="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" \
tools/build-llvm-self-stage-install.sh`

`STAGE0_INSTALL="$PWD/_llvm_self_stage_install" \
BOOT_BUILD="$PWD/_llvm_self_stage2_boot_context_build" \
BOOT_INSTALL="$PWD/_llvm_self_stage2_boot_install" \
SELF_RUNTIME_BUILD="$PWD/_llvm_self_stage2_runtime_build" \
SELF_MAIN_BUILD="$PWD/_llvm_self_stage2_main_build" \
SELF_STAGE_INSTALL="$PWD/_llvm_self_stage2_install" \
LLVM_WRAPPER="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" \
tools/build-llvm-self-stage-install.sh`

Stage1 self-build produced `_llvm_self_stage_install`. Wrapper counts:
boot 820 fresh IR inputs, runtime 73, main 1100, final smoke 2. Both smoke
programs printed `55`.

Stage2 self-build produced `_llvm_self_stage2_install`. Wrapper counts:
boot 834 fresh IR inputs, runtime 74, main 1097, final smoke 2. Both smoke
programs printed `55`.

Self-stage2 suite command:

`DUNE_CACHE=disabled PATH="$PWD/_build/llvm-tools/bin:$PATH" SELF_STAGE=2 \
LLVM_WRAPPER="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" \
tools/run-llvm-stage5-ocamltest.sh`

The runner's default `SELF_STAGE=2` list is
`_llvm_self_stage2_all_minus_asm_list.txt`, excluding `tests/asmgen` and
`tests/asmcomp`. GNU parallel is not installed in this environment, so the
script fell back to the serial `one` target with that generated list. Final
result: 6778 passed, 301 skipped, 0 failed, 0 not started, 0 unexpected errors,
7079 considered. Wrapper totals: 6447 wrapper lines, 3225 fresh IR inputs.

Important pass signals include AMD64 LLVM codegen probes, GC roots, stack
checks, native CFI stepping, frame pointers, statmemprof callback roots, native
dynlink, runtime events, stack allocation/local allocation, layout arrays and
iarrays with moving-GC coverage, C API layout tests, weak/ephemeron/finalizer
tests, and unboxed primitive arguments. This clears the requested normal-suite
and self-stage2 correctness gate for AMD64 LLVM. The next gate is performance
measurement against the native AMD64 backend.

2026-06-28 focused AMD64 performance gate:

- The historical representative/minibench/compiler-binary benchmark harness
  scripts referenced by old `agent-state` notes are not present in this
  checkout.  To still get a current performance signal, reran the nine archived
  raw slow-case inputs under
  `agent-state/test-suite-29e4cd/slowdown_vs_native_stage_artifacts_20260608_current/cases`.
- Comparison was generated-code performance from the same compiler:
  `_install/bin/ocamlopt.opt` native backend versus the same compiler with
  `-llvm-backend -llvm-path "$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`.
- Environment: `eval "$(opam env --switch=oxcaml-5.4.0+oxcaml --set-switch)"`,
  `DUNE_CACHE=disabled`, `PATH="$PWD/_build/llvm-tools/bin:$PATH"`,
  `OCAMLLIB="$PWD/_install/lib/ocaml"`.
- Method: output-checked each pair on a reduced run, then one warmup plus five
  measured executions per backend.  Runtime args were `100000 100`; `boyer`
  ignores those args.  Raw JSON is at
  `/tmp/oxcaml_llvm_x86_perf_scaled_20260628.json`.
- Result: geomean 1.0248x LLVM/native, median 1.0073x, summed median runtime
  ratio 1.0147x, min 0.9456x, max 1.1498x.  Slowest cases were
  `try_raise_cross_function_caught` at 1.1498x,
  `layered_try_raise_hit_only` at 1.0965x, `hash_lookup_string_equal` at
  1.0547x, and `string_map_equal_content` at 1.0531x.  Fastest cases were
  `boyer` at 0.9456x, `array_binary_search_string` at 0.9661x, and
  `try_find_miss_rare` at 0.9661x.
- This completes the requested local gate order on AMD64 in this checkout:
  normal installed-compiler LLVM suite, LLVM self-stage2 build/test, then a
  current native-vs-LLVM performance measurement.  A broader perf sweep should
  restore or recreate the missing historical harnesses if more exhaustive
  compiler-binary/minibench coverage is needed.

2026-06-28 compiler-binary performance benchmark:

- Ran the requested native-built compiler versus LLVM-built compiler comparison.
  Native side was `_install/bin/ocamlopt.opt`; LLVM side was
  `_llvm_self_stage2_install/bin/ocamlopt.opt`.
- Both compilers compiled representative compiler source files in normal native
  mode.  No `-llvm-backend` flag was passed.
- Method: direct `ocamlopt.opt -c` of these `_build/main` modules:
  `env.ml`, `ctype.ml`, `typecore.ml`, `translcore.ml`, `typemod.ml`,
  `cfg_to_linear.ml`, `cfg_selectgen.ml`, `llvmize.ml`, and
  `regalloc_irc.ml`.  Used the generated `_build/main` `.cmi` object
  directories as include context.  `OCAMLLIB` was unset so each installed
  compiler used its own installed stdlib path.  Each file used one warmup plus
  five measured samples, alternating measured compiler order.
- Result: geomean 1.0105x LLVM-built/native-built, median 1.0125x, summed
  median runtime ratio 1.0108x, min 0.9792x, max 1.0577x.  Slowest file was
  `regalloc_irc.ml` at 1.0577x; fastest was `cfg_selectgen.ml` at 0.9792x.
  Raw JSON is at
  `/tmp/oxcaml_compiler_binary_native_vs_llvm_stage2_20260628.json`.

2026-06-28 benchmark harness import and run:

- Fetched `origin/jujacobs/llvm-backend` and applied commit `bd34dfbf89`
  (`Add LLVM backend benchmark harnesses`) onto `jujacobs/llvm-x86-plan`.
  Did not merge the branch wholesale because it is based behind the AMD64 work.
- Ran the new runtime harnesses with
  `OCAMLOPT="$PWD/_install/bin/ocamlopt.opt"`,
  `OCAMLLIB="$PWD/_install/lib/ocaml"`,
  `LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`, `SAMPLES=3`, and
  `WARMUPS=1`.
- `exception_microprobe/run.py`: 21 cases completed.  From printed medians:
  geomean LLVM/native 0.9595x, median 1.0009x, min 0.4574x, max 1.3652x.
  Largest slowdowns were `closure_call_many_handler_live_roots_raise` 1.3652x,
  `raise_payload_caught_cross_function` 1.3466x,
  `raise_caught_cross_function` 1.2579x, and
  `many_handler_live_roots_raise` 1.2250x.
- `loop_invariant_microbench/run.py`: 2 cases completed.  Geomean 1.1842x,
  median 1.2395x, min 0.8734x, max 1.6057x.  The slowdown was
  `loop_invariant_gc_across_call` at 1.6057x; the int case was 0.8734x.
- `minibench_suite/run.py`: 16 default cases completed.  Runtime geomean
  0.9675x, median 0.9933x, total runtime ratio 0.9251x, min 0.7275x, max
  1.1408x.  LLVM compile-time geomean was 3.2620x native.  Results JSON:
  `agent-state/test-suite-29e4cd/minibench_suite/results.json`.
- `benchmarksgame_ocaml/run.py`: 11 selected cases completed.  Runtime geomean
  0.9635x, median 0.9884x, total runtime ratio 0.9377x, min 0.7924x, max
  1.1674x.  Results JSON:
  `agent-state/test-suite-29e4cd/benchmarksgame_ocaml/results.json`.
- The imported `run_compiler_bench.py` expected `_native_install` and
  `_llvm_self_stage_install` build logs.  `_native_install` was created with
  `tools/build-clean-native-install.sh`, but the required logs were unavailable
  in this checkout, so the harness was extended with `COMPILER_BENCH_MODE=direct`
  and explicit compiler/OCAMLLIB environment overrides.  Ran it with
  `_native_install/bin/ocamlopt.opt` versus
  `_llvm_self_stage2_install/bin/ocamlopt.opt`, normal native compilation mode,
  `REPETITIONS=3`.  Sum-of-module-medians ratio was 1.0095x; median
  round-total ratio was 1.0117x.  Generated JSON is ignored by
  `agent-state/test-suite-29e4cd/.gitignore`.

2026-06-28 full minibench default run:

- Changed `minibench_suite/run.py` so the default `CASES` selection is
  `CASES + LOCAL_CASES`, not only the 16 standard js_of_ocaml cases.  This
  makes all 52 known minibench cases run when `CASES` is unset, including
  `matmul` and `matmul_transposed`.
- Reran the full minibench suite with
  `OCAMLOPT="$PWD/_install/bin/ocamlopt.opt"`,
  `OCAMLLIB="$PWD/_install/lib/ocaml"`,
  `LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"`, `SAMPLES=3`, and
  `WARMUPS=1`.
- Result: 52 cases completed.  Runtime geomean 0.8598x LLVM/native, median
  0.9090x, total runtime ratio 0.8493x, min 0.5793x, max 1.1785x.  LLVM
  compile-time geomean was 3.2975x native and total compile-time ratio was
  3.2738x.
- Largest slowdowns: `soli` 1.1785x, `hash_batch_murmur_mix` 1.1586x,
  `finance_greeks_pnl` 1.1199x, `splay` 1.0582x, and `binary_trees` 1.0552x.
  Largest speedups: `numeric_float_dot_hof` 0.5793x,
  `hash_stdlib_string_vecadd_param_int64u` 0.5828x,
  `hash_stdlib_string_vecxor_param_int64u` 0.6099x, and
  `hash_stdlib_string_ocaml_mix_param2_int64u` 0.6331x.
- Matrix cases: `matmul` was essentially tied at 1.0085x; `matmul_transposed`
  was faster under LLVM at 0.8322x.  Results JSON:
  `agent-state/test-suite-29e4cd/minibench_suite/results.json` (ignored).

2026-06-28 AMD64 small-root in-place statepoint step:

- Investigated the largest current slowdown from the focused loop benchmark,
  `loop_invariant_gc_across_call` at about 1.6x LLVM/native.  MIR showed the
  AMD64 path still used the spill-slot statepoint lowering for ordinary calls,
  while the AArch64 path uses in-place statepoint lowering.
- Enabled in-place lowering for AMD64 ordinary OxCaml calls only when the
  statepoint has at most two unique GC roots.  Larger AMD64 statepoints and all
  32-bit x86 statepoints keep the existing spill-slot lowering to avoid the
  known AMD64 register-pressure cliff.
- Added a fixup-pass guard for AMD64 `OxCaml_WithFP` ordinary calls so any
  statepoint register metadata operands are rewritten to stack slots before
  frametable emission; this avoids listing `%rbp` as a caller root at ordinary
  call frames, which the runtime cannot update.
- Added `CodeGen/X86/oxcaml-small-root-inplace-statepoint.ll` covering:
  small-root AMD64 default in-place lowering after fixup, forced budget-zero
  fallback, default 3-root AMD64 fallback, and the 32-bit x86 non-in-place path.
- Code review: spawned `gpt-5.5` high-reasoning reviewer.  First pass caught
  an over-broad `isX86()` gate and missing budget coverage; both were fixed.
  Second pass found no remaining correctness bugs and requested the i386 RUN,
  which was added.
- Validation passed:
  `cmake --build _build/llvm-tools --target llc FileCheck -j 8`;
  manual FileCheck runs for the X86 default, budget-zero, and i386 RUN lines
  with `-verify-machineinstrs`; and both existing AArch64 alloc-statepoint
  FileCheck paths.
- Focused loop benchmark after the safe fixup change was still a modest perf
  move, not a complete fix: `loop_invariant_gc_across_call` was about 1.58x
  LLVM/native versus about 1.61x before.  This is mainly a parity/foundation
  step; the next perf work should inspect remaining runtime-register and root
  spill/reload traffic in the hot loop.
- High-level compiler/testsuite validation is currently blocked before the
  LLVM backend by a Dune/boot-compiler build-state failure: `make boot-compiler`
  and `make llvm-test-one DIR=llvm-gc-roots` both fail with broad
  `ocamldep returned unexpected output` and ocamllex missing-input errors such
  as `tools/make_opcodes.mll`.  The source files exist in the checkout, so this
  should be treated as a build-state issue to clear before the next suite run.

2026-06-28 boot build-state fix:

- Root cause of the immediate boot failure was Dune 3.23 user-rule sandboxing
  interacting with generated lexer rules.  The old `ocamllex (mode fallback)`
  rules for `parsing/lexer.ml` and `tools/make_opcodes.ml`, plus the Flambda
  parser lexer rule, could run in a sandbox where the source `.mll` files were
  not materialized.
- Replaced the two `ocamllex` stanzas with their explicit-rule equivalent,
  preserving `(mode fallback)`, and added `(sandbox always)` dependencies to
  all three lexer rules so Dune materializes the `.mll` inputs in the action
  sandbox.  This keeps Dune's dependency path rewriting instead of relying on
  checkout-relative `_build` paths.
- Cleaned only `_build/default` with `dune clean --root=. --workspace=duneconf/boot.ws _build/default`;
  `_build/llvm-tools` was left intact.
- Validation passed:
  `make -s boot-compiler` from the clean `_build/default`, then
  `make -s boot-compiler` again without cleaning.
- The previously blocked focused LLVM test now passes:
  `make -s llvm-test-one-no-rebuild DIR=llvm-gc-roots` with
  `LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"` and
  `PATH="$PWD/_build/llvm-tools/bin:$PATH"` reported 12 passed, 6 skipped,
  0 failed.  Full `make -s test` was not run for this build-rule fix, so this
  is a focused validation step, not a full-suite success claim.

2026-06-28 current-head installed LLVM suite:

- Reran the full standard installed-compiler LLVM suite on
  `jujacobs/llvm-x86-plan` at `69289499fd` after the AMD64 small-root
  statepoint step and boot lexer rule fix:
  ```sh
  PATH="$PWD/_build/llvm-tools/bin:$PATH" \
  DUNE_CACHE=disabled \
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" \
    make -s llvm-test-no-rebuild
  ```
- Result: 6809 passed, 314 skipped, 0 failed, 0 not started, 0 unexpected
  errors, 7123 considered.
- Important pass signals in the streamed output included AMD64 LLVM codegen,
  GC-root, stack-check, stack-growth, trap-recovery, native CFI stepping,
  statmemprof callback, dynlink, runtime-events, local/stack allocation,
  layout/C-API, SIMD-like vector array, weak/ephemeron/finalizer, and unboxed
  primitive argument coverage.
- This re-clears the normal installed-compiler `-llvm-backend` gate for the
  current commit.  The next gate is a fresh LLVM self-stage2 build/test on the
  same head because the older self-stage2 pass predates the latest commits.

2026-06-28 current-head LLVM self-stage2:

- Revalidated LLVM self-stage on `jujacobs/llvm-x86-plan` at `69289499fd`
  using isolated current-head stage directories and the rs4gc wrapper:
  ```sh
  PATH="$PWD/_build/llvm-tools/bin:$PATH" \
  DUNE_CACHE=disabled \
  LLVM_WRAPPER="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"
  export DUNE_CACHE PATH LLVM_WRAPPER
  STAGE0_INSTALL="$PWD/_install" \
  BOOT_BUILD="$PWD/_llvm_current_stage1_boot_context_build" \
  BOOT_INSTALL="$PWD/_llvm_current_stage1_boot_install" \
  SELF_RUNTIME_BUILD="$PWD/_llvm_current_stage1_runtime_build" \
  SELF_MAIN_BUILD="$PWD/_llvm_current_stage1_main_build" \
  SELF_STAGE_INSTALL="$PWD/_llvm_current_stage1_install" \
    tools/build-llvm-self-stage-install.sh
  ```
  then produced `_llvm_current_stage2_install` with:
  ```sh
  PATH="$PWD/_build/llvm-tools/bin:$PATH" \
  DUNE_CACHE=disabled \
  LLVM_WRAPPER="$PWD/tools/llvm-rs4gc-llc-wrapper.sh"
  export DUNE_CACHE PATH LLVM_WRAPPER
  STAGE0_INSTALL="$PWD/_llvm_current_stage1_install" \
  BOOT_BUILD="$PWD/_llvm_current_stage2_boot_context_build" \
  BOOT_INSTALL="$PWD/_llvm_current_stage2_boot_install" \
  SELF_RUNTIME_BUILD="$PWD/_llvm_current_stage2_runtime_build" \
  SELF_MAIN_BUILD="$PWD/_llvm_current_stage2_main_build" \
  SELF_STAGE_INSTALL="$PWD/_llvm_current_stage2_install" \
    tools/build-llvm-self-stage-install.sh
  ```
- Stage1 and stage2 both built successfully.  Wrapper summaries:
  stage1 boot 1678 wrapper lines / 825 fresh IR, runtime 148 / 74, main
  2228 / 1104; stage2 boot 1678 / 828, runtime 148 / 74, main 2228 / 1103.
  The script's final smoke test printed `55` for both stages.
- Ran the self-stage2 suite with:
  ```sh
  SELF_STAGE=2 \
  STAGE_INSTALL="$PWD/_llvm_current_stage2_install" \
  STAGE_BUILD="$PWD/_llvm_current_stage2_main_build" \
  NORMAL_RUNTIME_DIR="$PWD/_build/runtime_stdlib/runtime" \
  FAKE_ROOT="$PWD/_llvm_current_stage2_ocamltest_src" \
  LIST="$PWD/_llvm_current_stage2_all_minus_asm_list.txt" \
  LLVM_WRAPPER="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" \
    tools/run-llvm-stage5-ocamltest.sh
  ```
- Result: 6778 passed, 301 skipped, 0 failed, 0 not started, 0 unexpected
  errors, 7079 considered.  The self-stage2 test wrapper reported 6719
  wrapper lines and 3361 fresh IR inputs.
- Important pass signals in the streamed self-stage2 output included the AMD64
  LLVM codegen, GC-root, stack-check, stack-growth, trap-recovery, native CFI
  stepping, dynlink, runtime-events, statmemprof callback, weak/ephemeron/
  finalizer, local/stack allocation, layout/C-API, SIMD vector-array,
  unboxed return, and unboxed primitive argument tests.
- This re-clears the self-stage2 build and self-stage2 all-minus-asm suite
  gates for the current commit.  The runner excluded `tests/asmgen` and
  `tests/asmcomp`, matching the `all_minus_asm` list used for this LLVM
  validation pass.  Remaining project work is performance: rerun/report the
  benchmark suite against native and continue investigating the largest LLVM
  slowdowns from generated code evidence.

2026-06-28 current-head AMD64 LLVM performance pass:

- Reran the imported minibench suite with the current installed compiler:
  ```sh
  PATH="$PWD/_build/llvm-tools/bin:$PATH" \
  OCAMLOPT="$PWD/_install/bin/ocamlopt.opt" \
  OCAMLLIB="$PWD/_install/lib/ocaml" \
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" \
  SAMPLES=3 WARMUPS=1 \
    agent-state/test-suite-29e4cd/minibench_suite/run.py
  ```
- The suite ran 52 cases.  Aggregate runtime was LLVM/native 0.8433x by summed
  medians, 0.8524x geomean, and 0.9034x median case ratio.  Aggregate compile
  time was LLVM/native 3.2317x by summed times and 3.2486x geomean.  The largest
  minibench runtime slowdowns were `soli` 1.2280x, `boyer` 1.0981x, `hamming`
  1.0864x, `numeric_float_dot` 1.0562x, and `finance_greeks_pnl` 1.0547x.
  Matrix cases were not slow in this run: `matmul` 0.9150x and
  `matmul_transposed` 0.8466x.
- Reran the loop-invariant microbenchmark with 9 samples:
  ```sh
  PATH="$PWD/_build/llvm-tools/bin:$PATH" \
  OCAMLOPT="$PWD/_install/bin/ocamlopt.opt" \
  OCAMLLIB="$PWD/_install/lib/ocaml" \
  LLVM_PATH="$PWD/tools/llvm-rs4gc-llc-wrapper.sh" \
  SAMPLES=9 N=12000000 REPS=5 \
    python3 agent-state/test-suite-29e4cd/loop_invariant_microbench/run.py
  ```
- Results: `loop_invariant_int_across_call` native 0.0682s, LLVM 0.0596s,
  ratio 0.8743x; `loop_invariant_gc_across_call` native 0.0676s, LLVM 0.1073s,
  ratio 1.5873x.  The LLVM absolute time for the GC-root case is consistent
  with the earlier 0.1081s run; the ratio moved mostly because the native
  median was faster in this run.
- Reran a corrected compiler-binary benchmark that uses matching build trees
  for each compiler instead of mixing both compilers with `_build/main` CMIs.
  Raw results are recorded in
  `agent-state/test-suite-29e4cd/compiler_bench_current_matching_builds_20260628_124645.json`.
  Modules: `cfg_selectgen`, `llvmize`, `translcore`, `ctype`, `env`,
  `typecore`, and `typemod`; 5 repetitions; normal native compilation mode
  without `-llvm-backend`.  Sum of module medians was LLVM/native 1.0145x
  (native 15.9805s, LLVM-built compiler 16.2122s).  Round-total median was
  1.0173x (native 15.9783s, LLVM-built compiler 16.2546s).  Largest module
  ratios: `cfg_selectgen` 1.0380x, `translcore` 1.0322x, `llvmize` 1.0296x,
  `typemod` 1.0256x, `typecore` 1.0098x; `env` and `ctype` were slightly
  faster under the LLVM-built compiler.
- Current largest slowdown remains `loop_invariant_gc_across_call`.  The hot
  native inner loop keeps the string root in a stack slot and reloads it after
  the call, then computes `String.length` with `salq $8; shrq $18; leaq
  -1(,%rbx,8)`.  The hot LLVM inner loop additionally spills/reloads the live
  accumulator, loop index, and string root around every statepoint call, shuffles
  the OxCaml runtime registers through `%r14/%r15`, and computes the string
  offset with `movabsq $562949953421304; shrq $7; andq`.
- A backend-only experiment changed `emit_ocaml_string_length` to emit the
  native-shaped `shl reserved_header_bits; lshr
  (reserved_header_bits + header_wosize_shift); shl 3` sequence.  The generated
  LLVM IR changed as intended, and direct focused checks passed
  (`string_compare_correctness` printed `OK`, `live_values_roots` printed `ok`
  under stress GC), but X86 instruction selection canonicalized the sequence
  back to the same `shr $7` plus large-mask machine code.  The experiment was
  reverted rather than committed as a cosmetic IR-only change.
- Next performance fix should be target-side, not frontend-side: either teach
  X86 lowering to prefer native-style shifts/LEA for this OCaml header mask
  shape when the mask needs a `movabsq`, or attack the larger statepoint
  quality issue by reducing call-boundary root spills/reloads and runtime
  register shuffles while preserving the no-frontend-roots model.

2026-06-28 X86 target-side OCaml header-mask address fold:

- Implemented the target-side part of the string-header code-shape cleanup in
  `llvm/lib/Target/X86/X86ISelDAGToDAG.cpp`.  When X86 sees a shifted
  contiguous mask such as `((x >> 7) & 0x1fffffffffff8)` and cannot prove the
  masked high bits are already zero, it can now preserve those semantics with a
  left shift followed by a right shift, then use the addressing-mode scale
  instead of materializing a large 64-bit mask immediate.  The motivating OCaml
  string-header case now selects as:
  ```asm
  shlq $8, %rsi
  shrq $18, %rsi
  movzbl -1(%rdi,%rsi,8), %eax
  ```
  rather than `shrq $7; movabsq $562949953421304; andq`.
- Added `llvm/test/CodeGen/X86/oxcaml-header-mask-addressing.ll` to lock this
  addressing form down, including a case where the masked value is also used
  outside the address computation so normal DAG uses remain scaled while the
  addressing-mode index uses the unscaled value.
- Code review found that the first version preempted the existing BEXTR fold
  on targets where BEXTR is fast.  The final version preserves that behavior
  for the single-use case, and the test now checks `-mcpu=znver1` still selects
  `bextrq` while the multi-use case still takes the shift/address-scale path.
- Validation run:
  ```sh
  cmake --build _build/llvm-tools --target llc -- -j8
  _build/llvm-tools/bin/llc < vendor/llvm-project/llvm/test/CodeGen/X86/oxcaml-header-mask-addressing.ll -mtriple=x86_64-unknown-linux-gnu | _build/llvm-tools/bin/FileCheck vendor/llvm-project/llvm/test/CodeGen/X86/oxcaml-header-mask-addressing.ll
  _build/llvm-tools/bin/llc < vendor/llvm-project/llvm/test/CodeGen/X86/oxcaml-header-mask-addressing.ll -mtriple=x86_64-unknown-linux-gnu -mcpu=znver1 | _build/llvm-tools/bin/FileCheck vendor/llvm-project/llvm/test/CodeGen/X86/oxcaml-header-mask-addressing.ll --check-prefix=BEXTR
  _build/llvm-tools/bin/llc < vendor/llvm-project/llvm/test/CodeGen/X86/fold-and-shift.ll -mtriple=i686-- | _build/llvm-tools/bin/FileCheck vendor/llvm-project/llvm/test/CodeGen/X86/fold-and-shift.ll
  _build/llvm-tools/bin/llc < vendor/llvm-project/llvm/test/CodeGen/X86/shift-mask.ll -mtriple=x86_64-pc-linux | _build/llvm-tools/bin/FileCheck vendor/llvm-project/llvm/test/CodeGen/X86/shift-mask.ll --check-prefixes=X64,X64-MASK
  _build/llvm-tools/bin/llc < vendor/llvm-project/llvm/test/CodeGen/X86/shift-mask.ll -mtriple=x86_64-pc-linux -mattr=+fast-scalar-shift-masks | _build/llvm-tools/bin/FileCheck vendor/llvm-project/llvm/test/CodeGen/X86/shift-mask.ll --check-prefixes=X64,X64-SHIFT,X64-SHIFT2
  ```
- Also recompiled and ran focused normal-path OxCaml checks with the rebuilt
  `llc`: `testsuite/tests/llvm-codegen/string_compare_correctness.ml` printed
  `OK`, and `testsuite/tests/llvm-gc-roots/live_values_roots.ml` printed `ok`
  with `OCAMLRUNPARAM='s=64k,o=1,O=1'`.
- Rerunning the loop-invariant microbench after the semantic fix showed:
  `loop_invariant_int_across_call` native 0.068184s, LLVM 0.058621s,
  0.8597x; `loop_invariant_gc_across_call` native 0.069402s, LLVM 0.111809s,
  1.6110x.  This confirms the header-mask code shape is fixed, but the largest
  slowdown remains the statepoint call-boundary quality issue: extra live-value
  spills/reloads and runtime-register shuffles around calls.
