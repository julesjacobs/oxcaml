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
LLVM. The hidden flag is now `-mllvm -oxcaml-regalloc-call-split-remainders`.
`RS_CallSplit` sits below `RS_Spill` but above `RS_Split2`, so affected
remainders skip normal region splitting and still reach the bounded block/local
split path. The final representative micro run had geomean 0.8265x and kept
`direct_call_in_try_hit` near parity at 1.0285x. A fresh LLVM-built compiler
using `_llvm_self_stage2_callstage_install` benchmarked at 0.9782x geomean
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

2026-06-02 EH-root statepoint lowering phase 2: implemented the Stage 1
statepoint-level EH-root representation for shared pushtrap handlers. RS4GC now
materializes scalar handler-live GC values as `oxcaml-eh-live` statepoint
operands plus token-independent `llvm.oxcaml.gc.eh.recover` reloads when all
store-site invokes are rewritten statepoints; unsupported vector and mixed
non-statepoint cases keep the older volatile explicit-slot path. SelectionDAG
lowering now supports multiple logical EH roots that lower to the same SDValue
by assigning distinct EH root frame indices and adding the extra homes as
stackmap self-pairs. Added an AArch64 codegen regression for the duplicate
logical-root stackmap case and updated focused RS4GC tests. Validation:
`ninja -C ../llvm-build opt llc` was up to date, focused
`opt -passes=rewrite-statepoints-for-gc,verify` checks passed, focused
`llvm-lit` passed for the EH-root/RS4GC tests, `git diff --check` passed, and
`codex-review --mode local --full-access` reported no actionable findings.

2026-06-02 follow-up focused RS4GC correctness chunk: fixed two issues found
while reducing the next self-stage crash shape. First, apply/send helper calls
now keep their GC pointer arguments as call-duration roots, like C-wrapper
calls, because `caml_apply*`/`caml_send*` can allocate before the helper has
saved ordinary managed arguments in a managed frame. A saved optimized
`ctype` replay now shows the `caml_apply3` statepoint carrying the closure and
argument values in `gc-live`. Second, late EH-root materialization now treats a
raw root and the retry-boundary selector for that root as the same logical
`oxcaml-eh-live` value, prefers the dominating selector when needed, and reuses
the existing recovery/root id instead of creating a duplicate root. Focused
validation passed:
`ninja -C ../llvm-build opt FileCheck`,
focused `llvm-lit` for the call-arg, EH-root, derived-invoke, explicit-root,
and AArch64 EH-root stackmap tests, `opt
-passes=rewrite-statepoints-for-gc,verify /tmp/ctype-opt-o3.ll`, and
`git diff --check`. A final `codex-review` attempt was interrupted by local
Codex session permission errors, so the follow-up review for this small patch
was done manually; earlier review rounds on the same chunk had reported no
actionable findings before the final compatibility cleanup.

2026-06-02 active-trap statepoint root offset chunk: found and fixed a
separate frametable bug while investigating the remaining product-array
crashes. AArch64 PEI was rewriting STATEPOINT frame-index operands as offsets
from the stack pointer before active native traps, while ordinary stack loads
and stores under the same trap had already been adjusted to the post-trap SP.
Added a target hook for STATEPOINT frame-index references and used it on
AArch64 to add the active-trap byte count when the statepoint location is
SP-relative. The OxCaml frametable printer then had to avoid adding the same
active-trap bytes a second time for already-SP-relative statepoint
`GCLocations`, while preserving the existing adjustment for non-SP statepoint
locations and CSR root maps. Added
`oxcaml-eh-root-active-trap-statepoint.ll`, which checks both the PEI
STATEPOINT operands and the final OxCaml frametable root offsets.
Validation: the test failed red with final offsets `40,32` instead of `24,16`,
then passed after the printer fix; `ninja -C ../llvm-build llc`,
`ninja -C ../llvm-build clang`, focused `llvm-lit`, and `git diff --check`
passed; a final `codex-review --mode local` reported no actionable findings.
The broader `make llvm-test-one DIR=testsuite/tests/typing-layouts-arrays
LLVM_PATH=$PWD/../clang-wrapper` run still has the same 7 ignorable
product-array native SIGBUS/SIGSEGV failures, so this fixes a real active-trap
frametable offset bug but is not the root cause of those remaining crashes.

2026-06-02 EH-live root liveness chunk: found the root cause for the remaining
focused product-array crash shape. `oxcaml-eh-live` operands were being appended
after RS4GC liveness, so a value used only by a later exception handler was not
visible to earlier safepoints. In the reduced
`test_ignorable_product_array_2.ml` IR, two preceding safepoints had only two
frame-table roots before this fix; after the fix they carry the additional
handler-live root. The patch inserts temporary `__tmp_use` holders immediately
before each invoke for early EH-live roots. This makes the roots live for
preceding safepoints, but the reverse-iterator liveness boundary excludes the
holder at the recording invoke, so the root is not forced into ordinary
`gc-live` on the invoke's normal return path. Added
`oxcaml-eh-live-root-keeps-normal-liveness.ll` and updated obsolete volatile
exception-root expectations. Validation passed:
focused EH/RS4GC `llvm-lit` tests, all `RewriteStatepointsForGC/oxcaml-*.ll`
tests (28 passed, 1 XFAIL), `git diff --check`, and
`make llvm-test-one DIR=testsuite/tests/typing-layouts-arrays
LLVM_PATH=$PWD/../clang-wrapper` (134 passed). The direct `codex-review` helper
could not start because nested Codex could not write its local state database,
so the review loop used read-only reviewer agents with the actual diff and
liveness snippets; they reported no actionable findings. One invariant to keep
in mind for future work: late EH roots are safe only because they are expected
to be compatible remaps of roots already handled by the early liveness pass, not
brand-new liveness sources.
