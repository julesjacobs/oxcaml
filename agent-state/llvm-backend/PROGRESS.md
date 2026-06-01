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
