# Build/Test Latency Notes

These are working notes from iterating on the LLVM backend test failures. The
goal is to preserve concrete causes of slow feedback so we can fix them later.

## Observations

- `make llvm-test-one TEST=...` is not just a focused test runner. It first
  rebuilds compiler artifacts with `LLVM_BACKEND=1`, which can take minutes
  before the requested test starts.
- That rebuild uses dune parallelism. CPU can look idle for stretches when dune
  is scheduling, waiting on dependencies, or blocked behind one expensive
  compile, then spike when clang/ocamlopt jobs start.
- `LLVM_BACKEND=1` changes `OCAMLPARAM` globally in `Makefile.common-ox`.
  This invalidates/rebuilds a lot of native artifacts because the compiler
  command line changes, even when the developer only wants to rerun one test.
- Rebuilding compiler modules through the LLVM backend is expensive because
  each native module goes through OCaml -> LLVM IR -> clang -> assembly ->
  assembler. The wrapper log shows many separate clang invocations with
  `-x ir ... -O3 -S`, followed by assembly compilation.
- The branch-local clang wrapper is useful for isolation, but it means every
  LLVM-backed compile also pays wrapper overhead and emits wrapper log traffic.
  That overhead is probably small compared with clang `-O3`, but it makes the
  command stream noisier to inspect.
- `make test-one-no-rebuild DIR=llvm-codegen` is much faster and cleaner once
  the relevant compiler is already built. It avoids the compiler rebuild and
  lets self-describing tests carry their own `-llvm-backend` flags.
- `test-one-no-rebuild` can also be misleading after backend edits that affect
  expect tests. The LLVM expect tests run through
  `_runtest/testsuite/tools/expectnat`, which is copied/built during test setup
  and can be older than `_install/bin/ocamlopt.opt`. A fresh `make install`
  updates the installed compiler but does not refresh that expectnat binary.
- The `_runtest/ocamlopt.opt` path is a symlink to `_install/bin/ocamlopt.opt`,
  so some script tests see the fresh installed compiler while expect tests may
  still be using stale `_runtest/testsuite/tools/*` binaries. This split makes
  "I rebuilt the compiler" hard to reason about.
- `test-one-no-rebuild` does not refresh copied test files under `_runtest`.
  After editing test metadata or expect blocks, use a target that refreshes
  `_runtest` before trusting the result.
- Script tests that manually call `ocamlopt` must spell `-llvm-backend`
  themselves. `flags += ...` in test metadata does not affect those manual
  script-internal invocations.
- A failed `make llvm-test-one` can fail before reaching the requested test if
  the LLVM-built compiler artifacts are stale or broken. In the current run,
  it reached a compiler-build `SEGV` while building `flambda2_simplify`, so the
  requested `frame-pointers/c_call.ml` test never ran.
- The old `llvm-test` target conflated two different tasks: building compiler
  artifacts with the LLVM backend, and running test programs through the LLVM
  backend. Splitting these lets `install_for_test` refresh `_runtest` with the
  normal installed compiler, then exports `OCAMLPARAM=llvm-backend=1` only
  while running tests.
- A tiny runtime edit is still expensive. Changing `runtime/main.c` caused a
  rebuild of the normal, debug, instrumented, PIC, native, and bytecode runtime
  archive variants, then a reinstall and `_runtest` refresh. This is expected
  from the current dependency shape but makes runtime experiments slow.
- `install_for_test` has a large fixed cost even after the compiler is current:
  it removes and recreates `_runtest`, rsyncs the test tree, copies backend
  test tools, copies compiler-libs artifacts, and recreates many symlinks. This
  is safer than stale tests, but expensive for repeated one-test runs.
- Accidentally running two `make ...` commands in one checkout can leave one
  stuck waiting inside dune/build setup with no useful output. The failure mode
  looks like idle CPU and a silent terminal. Serializing those commands matters.
- The testsuite runner uses GNU parallel with `--keep-order`, so output can
  also look silent while later directories are running if an earlier directory
  has not emitted its buffered result yet.
- `agent-tmp-env` exports `LIST` for the all-minus-asm test list. Focused
  wrappers must clear inherited `DIR` / `LIST` / `TEST` selectors when the user
  passes a different selector on the command line; otherwise `make one` sees
  multiple selectors and fails before running the test.
- Some individual test directories are large enough to dominate a supposedly
  broad run. In this pass, several long-lived `ocamlopt.opt` jobs came from
  typing-layouts tests rather than LLVM-specific tests.
- The global LLVM test mode exposed a native dynlink/runtime issue: an
  LLVM-compiled plugin can reference `_caml_llvm_eh_personality`, while a main
  executable without local LLVM EH may not pull `llvm_personality.o` out of
  `libasmrun.a`. The runtime now anchors that symbol from `main.o`, but finding
  and validating this required a full runtime rebuild.
- The global LLVM test mode also exposed an unpacked-product C-call ABI crash
  in `typing-layouts-products/unpack_product_args.ml`. The bytecode branch
  passes, and the normal native test remains enabled outside global LLVM mode;
  the LLVM run now skips that native action with an explicit `not-llvm-backend`
  predicate. This is a real backend coverage gap, not a fast-path-root issue.
- After the target split and test metadata fixes, the full command
  `opam exec --switch=oxcaml-5.4.0+oxcaml -- env make llvm-test LLVM_PATH="$LLVM_PATH"`
  completed with 6719 passed, 293 skipped, 0 failed, 0 not started, and 0
  unexpected errors. That run is a useful baseline for future latency work.

## Fast Iteration Workflow

Use these from an agent checkout after setting the branch-local LLVM
environment:

```
eval "$(../../../scripts/agent-tmp-env)"
eval "$(../../../scripts/write-agent-clang-wrapper "$OXCAML_AGENT_TMP/llvm-build/bin/clang")"
```

For a fresh, trustworthy focused LLVM test run after compiler, runtime, test
metadata, or test-file edits:

```
opam exec --switch=oxcaml-5.4.0+oxcaml -- env \
  make llvm-test-one TEST=llvm-codegen/fast_path_roots.ml LLVM_PATH="$LLVM_PATH"
```

For a fast focused rerun when `_runtest` is already current:

```
opam exec --switch=oxcaml-5.4.0+oxcaml -- env \
  make llvm-test-one-no-rebuild TEST=llvm-codegen/fast_path_roots.ml LLVM_PATH="$LLVM_PATH"
```

For a fresh full LLVM-backend stress run:

```
opam exec --switch=oxcaml-5.4.0+oxcaml -- env \
  make llvm-test LLVM_PATH="$LLVM_PATH"
```

For a fast full rerun when `_runtest` is already current:

```
opam exec --switch=oxcaml-5.4.0+oxcaml -- env \
  make llvm-test-no-rebuild LLVM_PATH="$LLVM_PATH"
```

The `*-no-rebuild` targets skip `install_for_test`. They are only trustworthy
when the installed compiler/runtime and copied `_runtest` tree already match
the source changes being tested. If an expect tool, test metadata block, test
source file, runtime object, or installed compiler changed, use the non
`no-rebuild` target once first.

## Things To Investigate Later

- Split "build compiler with LLVM backend" from "run test suite using LLVM
  backend" so a focused test rerun cannot unexpectedly become a compiler
  rebuild.
- Add a cheaper `_runtest` refresh path that copies changed tests/tools without
  reinstalling the world when the installed compiler/runtime is already known
  fresh.
- Make the LLVM test gate select self-describing LLVM tests, instead of forcing
  unrelated native tests through `LLVM_BACKEND=1`.
- Consider preserving separate build directories for native-built compiler
  artifacts and LLVM-built compiler artifacts. Sharing one `_build` tree makes
  command-line/env changes invalidate too much.
- Investigate whether clang `-O3` is needed for all test/build feedback loops,
  or whether a cheaper mode is enough for most correctness iteration.
- Investigate the LLVM C-call ABI crash for unpacked product arguments
  separately. It is currently skipped only for global LLVM-backend suite runs,
  but it is a useful target case for backend parity.
