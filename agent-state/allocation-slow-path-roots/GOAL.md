# Goal

Make the normal LLVM backend heap-allocation fast path avoid spilling live GC
roots before the heap-limit check.

The current LLVM backend materializes values live across allocation safepoints
in volatile alloca slots before the allocation check. This keeps the GC root
story simple, but it makes the common fast path worse than native: simple pair
allocation stores live arguments to the stack before checking the young limit,
then reloads them to initialize fields even when no GC happens.

Implement a native-like allocation shape:

- the fast path keeps allocation inputs in registers/SSA values;
- root materialization happens only on the slow path before `caml_call_gc`;
- after `caml_call_gc`, any relocated roots used after the allocation are
  reloaded/refreshed before joining the normal continuation;
- frame-table metadata still exposes all live OCaml roots at every GC
  safepoint.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Primary implementation area: `backend/llvm/llvmize.ml`
- Tests: focused LLVM backend codegen/assembly expected-output tests
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
  - Inspect as needed.
  - Edit only if the existing statepoint/stackmap lowering cannot support the
    required root-materialization shape from OxCaml IR alone.
- Expected output: a focused implementation, tests proving the default
  allocation fast path does not spill roots before the heap-limit branch, and
  updated expected-output files.

## Design Direction

Prefer splitting two concepts that are currently conflated:

- trap-preserved slots: slots that must remain materialized because hidden
  exception edges may read them;
- GC root materialization slots: temporary root slots needed only at a GC
  safepoint.

The intended first implementation path is:

1. Stop treating values live across heap allocation/poll safepoints as globally
   volatile preserved register slots.
2. In the allocation slow block, store the currently live root values into
   dedicated root allocas immediately before `caml_call_gc`.
3. Pass those root allocas in the `"gc-live"` operand bundle.
4. After `caml_call_gc`, reload relocated root values from those allocas and
   refresh the corresponding compiler registers before branching to the join
   block.
5. Keep hidden-exception/trap preservation behavior intact.

Do not start by broad benchmarking. First make the generated allocation
sequence sane and prove correctness with focused codegen tests.

## Invariants

- At every GC safepoint, every live OCaml `Val` root is visible to the runtime
  through the frame table.
- Non-`Val` values are not reported as GC roots.
- If GC relocates a live value, every later use observes the relocated value.
- The normal fast path for heap allocation does not materialize root slots.
- The slow path materializes roots before `caml_call_gc`.
- Frame-table root locations match the physical locations the runtime can scan
  and update.
- Trap-preserved values remain available to hidden exception edges.
- Existing allocation debug metadata, statepoint IDs, and stack-offset encoding
  remain compatible with `OxCamlGCPrinter`.

## Non-goals

- Do not claim full parity with native allocation codegen.
- Do not rewrite the full statepoint strategy unless the smaller slow-path
  materialization approach cannot be made correct.
- Do not do broad benchmarking before the focused allocation sequence is fixed.

## Branches

- OxCaml: `jujacobs/allocation-slow-path-roots`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/9
