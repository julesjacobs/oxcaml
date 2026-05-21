# OxCaml LLVM backend plan

This plan is grounded in the current OxCaml LLVM backend and the custom LLVM
branch used by it. The key boundary is:

```text
OxCaml CFG -> backend/llvm/llvmize.ml -> custom LLVM statepoints/stackmaps
           -> OxCaml frametable/runtime
```

The goal is to make the LLVM backend production-usable without losing the
runtime guarantees that the normal backend currently provides.

## Current source contracts

- `asmcomp/asmgen.ml` enters the LLVM path after CFG selection. It currently
  runs polling instrumentation, zero-allocation checking, and comballoc, then
  calls `Llvmize.cfg`. It explicitly says stack checks are missing.
- `backend/llvm/llvmize.ml` emits LLVM IR with `gc "oxcaml"`, custom OxCaml
  calling conventions, threaded runtime registers, and `statepoint-id`
  attributes.
- `statepoint-id` currently encodes allocation size, stack offset, and whether
  the call is to `caml_call_gc`.
- `Poll` is currently dropped in `llvmize.ml`.
- `Stack_check _` currently fails in `llvmize.ml`.
- The exception/trap handling in `llvmize.ml` is amd64-specific and emits x86
  inline/module assembly.
- `backend/arm64/proc.ml` explicitly rejects the LLVM backend in
  `trap_size_in_bytes`.
- The custom LLVM branch has:
  - `OxCamlGC` in `llvm/lib/IR/BuiltinGCs.cpp`.
  - custom OxCaml calling convention names in LLVM IR parsing/printing.
  - x86 custom calling convention lowering in
    `llvm/lib/Target/X86/X86CallingConv.td`.
  - `OxCamlGCPrinter.cpp`, which emits OxCaml frametables from stackmaps and
    currently contains amd64 register mapping.
  - OxCaml-specific handling in `PlaceSafepoints.cpp` and
    `RewriteStatepointsForGC.cpp`.

## Plan

### 1. Lock down current amd64 behavior

Before changing abstractions, add tests that pin down the current contract:

- heap allocation fast path and slow path;
- live roots across allocation;
- trap/stack-offset handling;
- C calls that can reach safepoints;
- local allocation;
- generated frametable contents where possible.

These tests should inspect the generated assembly or frametable when the
program output alone would not prove the property.

Keep `make runtest-llvmize` green throughout.

### 2. Centralize safepoint metadata without changing behavior

Add a small OCaml-side abstraction around the current `statepoint-id` encoding.
For example:

```ocaml
type safepoint_kind =
  | Allocation
  | Poll
  | C_call
  | Local_realloc

type safepoint_info =
  { kind : safepoint_kind;
    alloc_words : int;
    stack_offset : int;
    can_call_gc : bool
  }
```

Initially this should encode to the exact current integer format. The point is
to make the hidden ABI explicit in OxCaml before changing LLVM.

### 3. Implement poll lowering

`compile_via_llvm` already runs `Cfg_polling.instrument_fundecl`, but
`Llvmize.basic_op` currently handles `Poll` as `()`.

Implement `Poll` by mirroring the normal backend's semantics:

- compare the allocation pointer against `Domain_young_limit`;
- branch around the slow path when no poll is needed;
- call the correct runtime path when polling is needed;
- emit a proper safepoint/frame entry.

Use `backend/amd64/emit.ml`, `backend/arm64/emit.ml`, `runtime/amd64.S`, and
`runtime/arm64.S` as ground truth. After this works, remove
`-disable-poll-insertion` from the llvmize tests.

### 4. Implement stack checks

The LLVM path currently skips stack checks and `Llvmize.emit_basic` rejects
`Stack_check _`.

Prefer a CFG-level stack-check pass if the existing stack-check logic can be
reused before `Llvmize.cfg`. If that is not practical, lower `Stack_check`
directly in `llvmize.ml`, but make it match the normal backend.

This is required before production use.

### 5. Split target-specific LLVM lowering inside OxCaml

`llvmize.ml` is currently amd64-specific in several places. Introduce a target
interface, for example:

- `backend/llvm/llvm_target_intf.ml`
- `backend/llvm/llvm_target_amd64.ml`
- later `backend/llvm/llvm_target_arm64.ml`

Move target-specific logic behind this interface:

- trap frame layout;
- restore-frame assembly;
- stack pointer/frame pointer handling;
- pause instruction;
- special runtime calls;
- target-specific calling convention assumptions.

Keep amd64 behavior unchanged while doing this.

### 6. Add AArch64 support to the custom LLVM branch

Changing only OxCaml is not enough. The LLVM side currently has x86-specific
lowering and amd64-specific frametable register mapping.

Add:

- AArch64 lowering for `oxcaml_fpcc`, `oxcaml_nofpcc`, `oxcaml_ccc`,
  `oxcaml_c_stackcc`, and `oxcaml_alloccc`;
- AArch64 register mapping in `OxCamlGCPrinter.cpp`;
- AArch64 frame-size/root-location handling that matches OxCaml runtime
  expectations;
- LLVM tests for both x86_64 and aarch64.

The calling convention enum values and LLVM IR parser/printer support are
already target-independent.

### 7. Add OxCaml arm64 LLVM support

Use the existing arm64 backend and runtime as the source of truth:

- `runtime/arm64.S` for `caml_call_gc`, frame pointer, link register, and trap
  behavior;
- `backend/arm64/emit.ml` for poll shape;
- `backend/arm64/proc.ml` for register and trap-size expectations.

Start with Linux AArch64. Darwin arm64 should come later because Mach-O
sections, symbol naming, and assembler behavior add separate failure modes.

### 8. Replace encoded `statepoint-id` only after behavior is tested

The current integer encoding is not a clean long-term interface, but custom
LLVM already consumes it.

After amd64 is locked down and AArch64 support exists, replace it with explicit
metadata such as:

- safepoint kind;
- allocation words;
- stack offset;
- frame adjustment reason.

This requires coordinated changes in `llvmize.ml` and
`OxCamlGCPrinter.cpp`. It should not be the first step.

## Correct order

1. Preserve and test current amd64 behavior.
2. Wrap the current metadata encoding.
3. Implement polls.
4. Implement stack checks.
5. Split target-specific OxCaml LLVM code.
6. Add AArch64 support in custom LLVM.
7. Add AArch64 support in OxCaml.
8. Later replace encoded `statepoint-id` with explicit metadata.

## Non-goals for the first milestone

- Do not start with Darwin arm64.
- Do not remove `statepoint-id` before tests pin down the current behavior.
- Do not try to enable arm64 by only removing the fatal error in
  `backend/arm64/proc.ml`.
- Do not switch to a shadow stack design except as a throwaway experiment.

## First PR-sized steps

### PR 1: Document and test the current amd64 metadata contract

Purpose: make the current behavior observable before cleanup.

Files to inspect or change:

- `backend/llvm/llvmize.ml`
- `oxcaml/tests/backend/llvmize/gen/gen_dune.ml`
- `oxcaml/tests/backend/llvmize/*.ml`
- `oxcaml/tests/backend/llvmize/*_ir.output`
- `oxcaml/tests/backend/llvmize/*_output`
- custom LLVM: `llvm/lib/CodeGen/AsmPrinter/OxCamlGCPrinter.cpp`

Tests to add or strengthen:

- allocation slow path emits `caml_call_gc` with a nonzero `statepoint-id`;
- allocation size in `statepoint-id` matches the allocation site;
- trap/stack-offset code emits the expected low bits in `statepoint-id`;
- live roots appear in the generated frametable/stackmap output.

Expected result:

- no behavior change;
- tests clearly describe the current ABI between `llvmize.ml` and
  `OxCamlGCPrinter.cpp`.

Validation:

```sh
OXCAML_CLANG=/work/.toolchains/oxcaml-llvm/bin/clang \
  make runtest-llvmize DUNE=/work/.toolchains/dune-j1
```

Use the Docker command from the setup notes when running this on an arm64 Mac,
because the current llvmize tests are amd64-oriented.

### PR 2: Wrap `statepoint-id` encoding in OxCaml

Purpose: make the current hidden metadata ABI explicit without changing output.

Files to change:

- `backend/llvm/llvmize.ml`
- maybe a new file under `backend/llvm/` if the helper is large enough;
  otherwise keep it local to `llvmize.ml` for the first cleanup.

Implementation shape:

- introduce a small type for safepoint metadata;
- add one encoder for the current integer format;
- replace direct `statepoint_id_attr` construction with the helper;
- keep generated LLVM IR byte-for-byte identical if practical.

Do not change `OxCamlGCPrinter.cpp` in this PR.

Expected result:

- no behavior change;
- the source makes it clear which calls are allocation safepoints, poll
  safepoints, C-call safepoints, or leaf calls.

Validation:

- `make runtest-llvmize`;
- compare relevant `*_ir.output` changes manually. Ideally there are none.

### PR 3: Implement LLVM poll lowering

Purpose: stop silently dropping CFG polls.

Files to inspect:

- `backend/llvm/llvmize.ml`
- `backend/amd64/emit.ml`
- `backend/arm64/emit.ml`
- `runtime/amd64.S`
- `runtime/arm64.S`
- `oxcaml/tests/backend/llvmize/gen/gen_dune.ml`

Implementation shape:

- lower `Poll` in `Llvmize.basic_op`;
- compare allocation pointer against `Domain_young_limit`;
- branch around the slow path when no poll is needed;
- call the same runtime path that normal backend polling uses;
- attach safepoint metadata through the new helper;
- remove `-disable-poll-insertion` only after the new lowering is covered.

Test shape:

- add a loop that is known to get poll instrumentation;
- check the generated IR contains the poll slow path;
- check the generated assembly/frametable has the corresponding frame entry;
- run the test binary if the existing llvmize harness supports it.

Expected result:

- llvmize tests no longer need `-disable-poll-insertion`;
- the poll path is parseable by the runtime.

## Source map

Use these files as the main ground truth while implementing:

- `asmcomp/asmgen.ml`: decides whether the LLVM or normal backend is used.
- `backend/llvm/llvmize.ml`: emits LLVM IR and currently owns most of the
  backend-specific behavior.
- `backend/llvm/llvm_ir.ml`: prints OxCaml calling conventions.
- `backend/amd64/emit.ml`: normal amd64 allocation, poll, stack-check, and
  frame emission behavior.
- `backend/arm64/emit.ml`: normal arm64 allocation, poll, stack-check, and
  frame emission behavior.
- `backend/amd64/proc.ml`: amd64 register, trap, and destroyed-register rules.
- `backend/arm64/proc.ml`: arm64 register, trap, and destroyed-register rules.
- `runtime/caml/frame_descriptors.h`: runtime frametable format.
- `runtime/amd64.S`: amd64 GC/runtime entrypoints.
- `runtime/arm64.S`: arm64 GC/runtime entrypoints.
- custom LLVM `llvm/lib/IR/BuiltinGCs.cpp`: `OxCamlGC` strategy.
- custom LLVM `llvm/include/llvm/IR/CallingConv.h`: OxCaml calling convention
  enum values.
- custom LLVM `llvm/lib/Target/X86/X86CallingConv.td`: current x86 lowering.
- custom LLVM `llvm/lib/CodeGen/AsmPrinter/OxCamlGCPrinter.cpp`: frametable
  printer and current amd64 register mapping.
- custom LLVM `llvm/lib/Transforms/Scalar/PlaceSafepoints.cpp`: safepoint
  insertion policy.
- custom LLVM `llvm/lib/Transforms/Scalar/RewriteStatepointsForGC.cpp`:
  statepoint rewrite policy.

## Validation ladder

Use this order instead of jumping straight to a full compiler test suite:

1. Build the specific changed compiler libraries or binary.
2. Run the smallest llvmize test that covers the change.
3. Run `make runtest-llvmize`.
4. For poll/stack-check changes, run one hand-written program that allocates
   and loops long enough to exercise the runtime path.
5. Only after that, run broader backend/runtime tests.

On this arm64 machine, the known working amd64 validation command is:

```sh
docker run --rm --platform linux/amd64 \
  -v "$PWD:/work" \
  -v oxcaml_llvm_opam_root:/opamroot \
  -w /work \
  -e OPAMROOT=/opamroot \
  -e OPAMYES=1 \
  oxcaml-llvm-opam-env:24.04 bash -lc '
    set -euo pipefail
    eval "$(opam env --switch=llvm-backend)"
    export PATH="/work/.toolchains/oxcaml-llvm/bin:$PATH"
    OXCAML_CLANG=/work/.toolchains/oxcaml-llvm/bin/clang \
      make runtest-llvmize DUNE=/work/.toolchains/dune-j1
  '
```

Do not run multiple `make` commands concurrently in the same worktree.
