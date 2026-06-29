# Full BOLT plan for the LLVM-built compiler

## Goal

Get full BOLT working on the LLVM-built OxCaml compiler, then measure whether it
closes the remaining native-built-vs-LLVM-built compiler runtime gap.

The primary benchmark is:

- native-built `ocamlopt.opt` versus LLVM-built `ocamlopt.opt`;
- both compilers running in normal native mode, not `-llvm-backend`;
- both compiling the same representative compiler modules with matching build
  trees, CMIs, and stdlib paths.

Full BOLT means BOLT can rewrite OCaml-managed `caml*` functions in the compiler
binary, not only C/runtime/support functions, and the rewritten compiler still
has correct GC, exception, stack-check, backtrace, and frame-table behavior.

## Current state

- BOLT tools build locally in `_build/llvm-bolt/bin`.
- The LLVM-built stage2 compiler can be relinked with `-Wl,--emit-relocs`.
- The relocation-enabled compiler contains the needed relocation sections:
  `.rela.text`, `.rela.rodata`, `.rela.eh_frame`, `.rela.data*`, and `.symtab`.
- A BOLT rewrite that skips all `caml*` functions runs, but has no useful win.
- A no-profile BOLT rewrite of all functions segfaults on `-version`.
- BOLT instrumentation also segfaults immediately in GC entry, so the first
  supported profiling route should be external `perf` sampling, not BOLT's
  inserted instrumentation calls.
- `perf` was previously blocked by `kernel.perf_event_paranoid=4`.

## Non-goals

- Do not use frontend roots or older GC-root lowering as a BOLT workaround.
- Do not special-case AMD64 away from the ARM-quality statepoint/frame-table
  model.
- Do not treat a skip-`caml*` BOLT binary as full BOLT.
- Do not optimize only BOLT's C/runtime subset unless it is needed as a
  diagnostic baseline.

## Phase 1: Enable profiling cleanly

1. Set host profiling permissions for this session:
   `sudo sysctl kernel.perf_event_paranoid=1`.
2. Confirm that ordinary software and hardware events work:
   `perf record -e cycles:u -- true`,
   `perf record -e instructions:u -- true`,
   and, if supported, branch/LBR sampling.
3. Record the exact kernel setting and supported event set in
   `agent-state/test-suite-29e4cd/PROGRESS.md`.

Acceptance criteria:

- `perf record` succeeds without BOLT instrumentation.
- We can produce a non-empty perf profile for the unmodified relocation-enabled
  LLVM-built compiler while it compiles at least one compiler module.

## Phase 2: Make frame metadata BOLT-safe

The suspected blocker for rewriting OCaml-managed code is not ordinary LLVM
module frame-table emission. LLVM's OxCaml GC printer already emits module
frametables as data with symbolic label differences. The weak spot is the
remaining AMD64/native-style startup/global frametable shape, especially
`caml_startup__frametable` being emitted in `.text` as a text symbol.

Work items:

1. Audit all final-executable frametable symbols:
   `caml*__frametable`, `caml_startup__frametable`, `code_begin`,
   `code_end`, `data_begin`, and `data_end`.
2. Move AMD64 startup/global frametable data out of executable text into data
   or a dedicated alloc data section, following ARM's shape.
3. Make frametable symbols object/data symbols where practical, so BOLT does
   not try to disassemble metadata.
4. Ensure return-address entries are relocation-bearing symbolic expressions in
   relocation-enabled builds, so BOLT can update them when it moves code.
5. If BOLT still classifies OCaml frametables incorrectly, add the smallest
   BOLT-side classifier needed to treat `caml*__frametable` metadata as data.

Acceptance criteria:

- `llvm-bolt <compiler> -o <rewritten> --print-only` or equivalent inspection no
  longer tries to disassemble frametable data.
- A no-profile BOLT rewrite of all functions runs `ocamlopt.opt -version`.
- The rewritten compiler can compile a hello-world native executable.

## Phase 3: Correctness tests for a rewritten compiler

Run tests in increasing scope after every BOLT/frame-table change:

1. Smoke:
   `ocamlopt.opt -version`, compile and run hello-world, compile and run a
   small allocation loop.
2. GC/frame tests:
   allocation across calls, major/minor GC stress, live roots across calls,
   live roots across allocation slow paths, and mixed-block cases.
3. Exceptions:
   ordinary `try`/`raise`, `raise_notrace`, nested handlers, backtraces, and
   exceptions with live roots.
4. Stack behavior:
   prologue stack checks, stack overflow, stack growth under active traps, and
   stack growth with live roots.
5. Compiler validation:
   compile representative compiler modules in native mode with the rewritten
   compiler.
6. Full validation:
   run the relevant OxCaml test suite with the rewritten compiler, then attempt
   self-stage2 if the smaller gates pass.

Acceptance criteria:

- No crashes in GC, exceptions, backtraces, or stack checks.
- The rewritten compiler can compile the selected compiler modules repeatedly.
- Failures are reduced to focused reproducers before moving to benchmark work.

## Phase 4: Profile-guided BOLT pipeline

Use external profiles first:

1. Relink or rebuild the LLVM-built compiler with relocation information.
2. Run representative compiler workloads under `perf record`, preferably using
   user-space branch samples when available.
3. Convert profiles with `perf2bolt`.
4. Run `llvm-bolt` with conservative first-pass options:
   relocation mode, no PLT rewriting if it causes issues, and only standard
   block/function reordering.
5. Add one optimization at a time:
   function reordering, basic-block reordering, splitting, ICF, and other BOLT
   transforms supported by this binary.

Acceptance criteria:

- A profile-guided BOLT binary passes the Phase 3 smoke and focused tests.
- The exact BOLT command line is recorded.
- Each added BOLT transform has an A/B timing result and a correctness result.

## Phase 5: Benchmarking

Benchmark four compiler binaries:

1. native-built baseline;
2. LLVM-built baseline;
3. LLVM-built relocation-enabled baseline;
4. LLVM-built relocation-enabled plus full profile-guided BOLT.

Use the existing compiler-module benchmark harness and keep the setup strict:

- same source tree;
- same build tree shape for the compiler being measured;
- same stdlib/CMI path per compiler;
- no `-llvm-backend` in the benchmarked compile commands;
- one warmup and at least seven measured repetitions initially;
- increase repetitions for sub-1% effects.

Report:

- sum of per-module medians;
- round-total median;
- geomean module ratio;
- per-module ratios;
- perf counter deltas if stable: cycles, instructions, branches, branch misses,
  iTLB misses, and icache misses.

Acceptance criteria:

- The BOLT-vs-LLVM-built result is statistically larger than noise before we
  claim a win.
- If BOLT helps only selected modules, inspect profiles and assembly before
  assuming the improvement generalizes.
- If BOLT does not help, keep the result: that means the remaining gap is
  probably not primarily layout/profile-ordering.

## Phase 6: Upstreamable cleanup

Before committing a BOLT-enabling change:

1. Separate OxCaml frame-table correctness changes from local benchmarking
   scripts and host setup.
2. Keep any BOLT runtime/tool patch separate from OxCaml backend changes.
3. Add focused tests for the frame-table section/symbol/relocation contract.
4. Run code review on each commit.
5. Record commands, artifacts, and results in
   `agent-state/test-suite-29e4cd/PROGRESS.md`.

## Expected outcome

The best outcome is:

- full BOLT can rewrite OCaml-managed compiler code;
- the rewritten compiler passes smoke, focused runtime tests, and compiler
  module compilation;
- profile-guided BOLT measurably improves the LLVM-built compiler runtime.

The useful negative outcome is:

- full BOLT is made correct and measurable;
- it produces little or no speedup;
- we can then deprioritize BOLT and focus on code generation differences that
  BOLT cannot fix.

## 2026-06-29 status update

What now works:

- AMD64 native frametables can be emitted in `.data`, matching the arm-style
  section shape needed for BOLT-visible relocations.
- A corrected LLVM self-stage2 build passed with reduced dune parallelism
  (`DUNE_BUILD_FLAGS=-j2`), and its installed compiler passes a native-mode
  allocation/exception smoke test.
- The stage2 compiler was relinked with `-ccopt -Wl,--emit-relocs` as
  `agent-state/test-suite-29e4cd/bolt_compiler_20260629/ocamlopt.boltfix2.reloc`.
- BOLT `--enable-bat` plus the local
  `patch_ocaml_frametables.py` post-pass can rewrite all OCaml frame descriptor
  return addresses by matching old/new direct call return sites. This avoids the
  imprecision of BAT alone when BOLT shortens instructions inside a basic block.
- No-profile BOLT of OCaml-managed code now works:
  `ocamlopt.boltfix2.noprofile.bat.callpatched` starts and compiles native-mode
  smoke tests.
- Profile-guided BOLT without block splitting or block reordering works when it
  only reorders functions:
  `ocamlopt.boltfix2.profile-funconly.bat.callpatched` starts and passes the
  reduced compiler-module workload.

Full BOLT status:

- Profile-guided BOLT with basic-block reordering now works on the reduced
  compiler-module workload. The frame-table patcher matches by the call
  instruction address, not the return address, because reordered blocks can put
  return PCs at BAT block boundaries.
- Profile-guided BOLT with function splitting now works on the same workload.
  The patcher parses BOLT's cold-to-hot BAT entries and matches hot plus cold
  fragments together against the parent input function. The successful split
  patch rewrote all 224,538 frame descriptors by call-site mapping with zero
  BAT fallback mappings.
- The profile workload currently excludes `typing/typecore.ml` and
  `typing/typemod.ml` because the standalone compile harness does not supply the
  exact interface setup those modules need. This is a harness limitation, not a
  BOLT success criterion.

Measured quick result on the reduced workload
(`cfg_selectgen`, `llvmize`, `translcore`, `ctype`, `env`; one compile
repetition per sample; three samples):

- `reloc_emit_relocs`: median `9.634s`, baseline.
- `noprofile_bolt`: median `9.514s`, `0.988x` vs relocation baseline.
- `profile_noreorder`: median `9.576s`, `0.994x`.
- `profile_funconly`: median `9.600s`, `0.996x`.

Benchmark artifact:
`agent-state/test-suite-29e4cd/bolt_compiler_20260629/boltfix2-stable-benchmark.json`.

Full BOLT benchmark on the same five-module workload, three timing samples per
compiler and three compile repetitions per sample:

- `reloc_emit_relocs`: median `28.849s`, baseline.
- `noprofile_bolt`: median `28.344s`, `0.983x` vs relocation baseline.
- `profile_funconly`: median `28.729s`, `0.996x`.
- `fullbolt_nosplit_block_function`: median `27.807s`, `0.964x`.
- `fullbolt_split_block_function`: median `28.230s`, `0.979x`.

Full BOLT artifacts:

- no-split block/function reordered:
  `ocamlopt.boltfix2.fullbolt-nosplit.bat.calladdrpatched`;
- split block/function reordered:
  `ocamlopt.boltfix2.fullbolt.bat.coldpatched`;
- benchmark JSON:
  `agent-state/test-suite-29e4cd/bolt_compiler_20260629/boltfix2-fullbolt-benchmark.json`.

Next steps:

1. Promote the patcher logic into a maintained tool or integrate equivalent
   frame-table rewriting with the BOLT flow.
2. Expand validation beyond the reduced standalone module workload, ideally by
   using the BOLTed compiler for a larger native compiler build/test slice.
3. Re-run compiler-binary benchmarking against the native-built compiler once
   the larger validation slice is green.
