# Progress

Last updated: 2026-05-25.

## Current Claim

Independent AMD64 LLVM-backend implementation is in progress. The compiler
builds on AMD64, and simple scalar programs compile and run with
`-llvm-backend` through this agent's own LLVM wrapper.

## Evidence

- OxCaml branch: `jujacobs/llvm-amd64-support-2`
- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/12
- Initial state commit: `c206e2f15041`
- PR-record commit: `b1cde046ea`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Agent state path: `agent-state/llvm-amd64-support-2`
- Goal: build AMD64 support for `-llvm-backend` until simple programs run,
  full `llvm-test` passes, and `llvm-self-stage2-test` passes.
- Per-agent temp setup:
  - `eval "$(../../../scripts/agent-tmp-env)"`
  - `OXCAML_AGENT_TMP=/tmp/oxcaml-agent-llvm-amd64-support-2`
  - `LLVM_PATH=/tmp/oxcaml-agent-llvm-amd64-support-2/clang-wrapper`
  - `LLVM_WRAPPER_LOG=/tmp/oxcaml-agent-llvm-amd64-support-2/clang-wrapper.log`
- Built vendored LLVM `llc` and `opt` for this agent checkout under
  `$OXCAML_AGENT_TMP/llvm-build` with `LLVM_TARGETS_TO_BUILD=X86`.
- Implemented AMD64 lowering in `backend/llvm/llvmize.ml` for the AMD64
  `Arch.specific_operation` constructors needed by non-SIMD code:
  `Ilea`, `Istore_int`, `Ioffset_loc`, `Ifloatarithmem`, `Ibswap`,
  `Isextend32`, `Izextend32`, `Irdtsc`, `Irdpmc`, fences, and
  `Illvm_intrinsic`.
- Added AMD64 LLVM-backend selection/lowering for target-only `Cpackf32` and
  `Cprefetch` so they no longer fall into the generic selector fatal path.
  `Icldemote` is selected but lowered as a no-op because it is only a cache hint
  and `llvm.x86.cldemote` aborts in this LLVM X86 configuration without the
  matching selectable target feature.
- SIMD AMD64 specifics are intentionally still reported as not implemented;
  this keeps the first implementation scalar and avoids importing ARM64-specific
  lowering into AMD64.
- Fixed a normal AMD64 compiler build break in
  `backend/amd64/simd_selection.ml` by selecting the generated
  `vpmulhrsw_Y_Y_Ym256` AVX2 instruction helper.
- `opam exec -- ./configure --enable-frame-pointers` completed.
- `opam exec -- make compiler` completed after the source changes.
- Manual scalar smoke after the rebuild:
  - compiled with `_build/install/main/bin/ocamlopt.opt -nostdlib -I
    _build/runtime_stdlib_install/lib/ocaml_runtime_stdlib -llvm-backend
    -llvm-path "$LLVM_PATH"`
  - program used recursion, allocation, exceptions, `Printf`, and `sqrt`
  - output: `42 24 3.0`
  - wrapper log contained 2 `-x ir` invocations.
- Manual prefetch/cldemote smoke:
  - used builtin externals for `caml_prefetch_read_high`,
    `caml_prefetch_write_low`, and `caml_cldemote`, plus temporary C stubs for
    primitive table symbols
  - compiled with the same `-llvm-backend -llvm-path "$LLVM_PATH"` setup
  - output: `7`
  - wrapper log contained 2 `-x ir` invocations.
- Local testsuite setup works when run under opam with a make-time local prefix:
  `opam exec -- make prefix="$PWD/_local-test-install" install_for_test`.
- Direct testsuite run of `tests/llvm-codegen/arithmetic.ml` completed through
  the harness but skipped because the test is currently gated by `macos` and
  `arch_arm64`.
- Added `testsuite/tests/llvm-codegen/amd64_smoke.ml` plus
  `amd64_smoke.sh`, an AMD64-gated testsuite smoke that builds and runs three
  `-llvm-backend` programs:
  - scalar recursion/allocation/exception/`Printf`/`sqrt` smoke, expected
    output `42 24 3.0`
  - float32 conversion smoke, expected output `1.5 2.5`
  - prefetch/cldemote builtin smoke with local C primitive-table stubs,
    expected output `7`
- The smoke script passes `_runtest/stdlib` explicitly with `-nostdlib -I`
  because the local `ocamlopt.opt` can otherwise look at its configured install
  prefix and fail with `Unbound module "Stdlib"` in this harness setup.
- Focused testsuite run passed:
  `env -u DIR -u LIST opam exec -- make --trace one TEST=tests/llvm-codegen/amd64_smoke.ml`
  from `_runtest/testsuite`, with `OCAMLSRCDIR` and
  `CAML_LD_LIBRARY_PATH` pointed at `_runtest`.
- Started broader `llvm-test` validation with a local prefix and
  `LLVM_BOOT_BACKEND=0` to keep normal backend testing separate from self-stage:
  `opam exec -- make prefix="$PWD/_local-llvm-test-install" llvm-test LLVM_BOOT_BACKEND=0`.
  The run reached the testsuite and was stopped after the first repeated
  failures were reduced.
- Fixed AMD64 LLVM object files for Linux PIE/shared linking by passing `-fPIC`
  to clang when `Clflags.pic_code` or `Clflags.dlcode` is set. This fixed the
  repeated `R_X86_64_32S against .text` link failures seen in native tests.
- Fixed `-internal-assembler` interactions with `-llvm-backend` by forcing
  `Asmgen.compile_unit` to create an assembly file for LLVM even when
  `Emitaux.binary_backend_available` has been enabled by the internal
  assembler flag. This fixed the missing `.s` failure in AMD64 asmcomp tests.
- Verified after both fixes:
  - `opam exec -- make compiler`
  - local `-shared` smoke using `_runtest/ocamlopt.opt` with
    `OCAMLPARAM=_,llvm-backend=1,llvm-path=$LLVM_PATH`
  - `tests/array-functions/test.ml` through `_runtest/testsuite`
  - `tests/asmcomp/movsx_small_ints.ml` through `_runtest/testsuite`
  - `tests/llvm-codegen/amd64_smoke.ml` through `_runtest/testsuite`
- Added independent AMD64 exception/runtime fixes in
  `backend/llvm/llvmize.ml`:
  - empty `deopt` operand bundles are emitted for non-leaf calls, while
    `Gc_leaf_function` calls skip operand bundles;
  - `Compare_exchange` now returns the old loaded value from LLVM `cmpxchg`;
  - AMD64 runtime exception recovery uses PIC-safe GOTPCREL addressing and
    moves the exception bucket through `%r11`;
  - frame-pointer attributes are attached on AMD64 when configured, including
    generated C-call wrappers and `wrap_try`.
- Added AMD64 LLVM lowering for the subset of SIMD builtins needed by layout
  tests: `caml_int64x2_*`, `caml_simd_int64x2_{add,sub}`,
  64-bit vec128 interleaves, low vec256 extraction, and AVX vec256
  extract/insert 128 operations. Added the corresponding AMD64 selection and
  purity table entries.
- Added limited LLVM lowering for vector reinterpret casts between vec128 and
  vec256 low lanes.
- Added minimal probe lowering: `Probe` terminators branch to their continuation
  and `Probe_is_enabled` returns the static `enabled_at_init` value when known
  (otherwise false). This is enough for the focused probe tests but does not yet
  emit real USDT probe metadata.
- Focused tests that now pass:
  - `tests/exception-extra-args/exception_extra_args.ml`
  - `tests/match-exception/nested_handlers.ml`
  - `tests/lib-atomic/test_atomic_cmpxchg.ml`
  - `tests/callback/test3.ml`
  - `tests/frame-pointers/stack_realloc.ml`
  - `tests/frame-pointers/stack_realloc2.ml`
  - `tests/typing-layouts-block-indices/block_indices_native.ml`
  - `tests/typing-layouts-or-null/probe.ml`
  - `tests/templates/basic/probe.ml`
- Rebuilt and refreshed the local testsuite install after the latest source
  changes:
  - `opam exec -- make compiler`
  - `opam exec -- make prefix="$PWD/_local-llvm-test-install" install_for_test`
- Full `llvm-test` with `LLVM_BOOT_BACKEND=0` completed:
  - `6632 passed`, `280 skipped`, `43 failed`, `6955 considered`
  - This improves from the previous full run's `6605 passed`, `280 skipped`,
    `67 failed`, `6952 considered`.
  - Fixed failures include `exception_extra_args`, `nested_handlers`,
    `test_atomic_cmpxchg`, block-index SIMD compilation, and probe tests.
  - Remaining notable failures include native segfaults in
    `tests/async-exns/async_exns_2.ml`, `tests/basic/patmatch.ml`,
    small-int tests, `tests/misc-kb/kbmain.ml`, and several layout array/iarray
    semantic assertions; quotation linking, native CFI stepping, and unboxed
    primitive argument ABI failures remain.
- Attempted a simpler AMD64 exception recovery path that avoided moving `%r14`
  and `%r15` into LLVM's landing registers; it immediately regressed
  `exception_extra_args`, so that experiment was backed out before this handoff.

## Current Blocker

No immediate source blocker. Full `llvm-test` now completes but still has 43
failures. The next focused runtime/codegen blocker is the native segfault in
`tests/async-exns/async_exns_2.ml`; it reproduces after the current exception
fixes, while `exception_extra_args` and `nested_handlers` pass. Several layout
array/iarray tests now compile but fail semantic assertions, so the remaining
SIMD/product-array work is beyond just adding missing builtins.

## Next Step

Reduce `tests/async-exns/async_exns_2.ml` under the standard compiler with
`-llvm-backend`, then fix the AMD64 runtime/exception state issue without
regressing `exception_extra_args` or `nested_handlers`. After that, rerun the
full `llvm-test` and continue into layout array semantics and unboxed primitive
argument ABI failures before attempting `llvm-self-stage2-test`.
