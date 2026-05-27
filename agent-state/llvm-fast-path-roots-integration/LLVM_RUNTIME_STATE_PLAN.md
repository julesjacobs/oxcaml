# Plan: LLVM runtime-state representation

This note records the current design view for representing OxCaml runtime state
in the LLVM backend.

There are two serious designs:

1. Keep `ds` and `alloc` as LLVM SSA values and let register allocation place
   them in `x28` and `x27` through the calling conventions.
2. Reserve `x28` and `x27` and manipulate runtime state through explicit
   target operations.

The current implementation is an awkward halfway design: it models `ds` and
`alloc` as LLVM values, but also reserves exactly the physical registers where
the calling convention wants those values to live.

## Current halfway design

The LLVM backend currently does this:

- `ds` and `alloc` are ordinary LLVM values stored through allocas and usually
  simplified by mem2reg.
- OxCaml function types prepend `ds` and `alloc` as arguments.
- OxCaml return types include returned `ds` and `alloc`.
- The AArch64 OxCaml calling convention assigns those first two integer
  arguments/results to `x28` and `x27`.
- `AArch64RegisterInfo` reserves `x27` and `x28` for OxCaml functions, so LLVM
  cannot allocate normal virtual registers to them.

That last point is the contradiction. The IR says "`ds` and `alloc` are values".
The calling convention says "put those values in `x28` and `x27`". But the
register allocator is forbidden from choosing `x28` and `x27` as the homes for
those values.

The result is the pattern we keep seeing:

```asm
mov x28, <some allocatable register holding ds>
mov x27, <some allocatable register holding alloc>
bl  callee
mov <some allocatable register>, x28
mov <some allocatable register>, x27
```

The register-register moves themselves are not the whole cost. The real cost is
that the call boundary creates extra fixed-register constraints, extra values,
and extra spills/reloads/rematerialization around hot calls.

## Design 1: SSA state, unreserved runtime registers

This design keeps the good part of the current approach: `ds` and `alloc` are
LLVM values.

The change is to stop reserving `x27` and `x28` for OxCaml functions.

Contract:

- `ds` and `alloc` are normal LLVM SSA values.
- Relevant OxCaml calling conventions pass `ds` in `x28` and `alloc` in `x27`.
- Relevant OxCaml calling conventions return updated `ds` in `x28` and updated
  `alloc` in `x27`.
- External/runtime boundaries use calling conventions or explicit calls that
  force the values into the runtime ABI registers.
- The register allocator is allowed to allocate the `ds` and `alloc` virtual
  registers to `x28` and `x27`.

Expected outcome:

- Since `ds` and `alloc` are live across most of the function and are fixed to
  `x28`/`x27` at call boundaries, LLVM should usually coalesce them into
  `x28`/`x27`.
- The explicit runtime-register round trips should disappear without adding a
  new special-case call path.
- Fast allocation still works naturally as SSA: `alloc_new = alloc_old - n`.
  If `alloc` is allocated to `x27`, this becomes the native-like physical update.
- If register pressure makes LLVM choose another register or spill temporarily,
  that is LLVM making a normal allocation decision. At the next relevant
  boundary, the calling convention moves the value back to `x27`.

This is the LLVM-friendly design. It gives the allocator freedom while making
the ABI preference strong.

### Main risks for Design 1

1. **Runtime observation away from boundaries.**

   This design is correct only if the runtime requires `x28`/`x27` to be current
   at well-defined boundaries, not at every arbitrary instruction.

   Boundaries include:

   - calls using the OxCaml ABI;
   - allocation slow paths;
   - poll slow paths;
   - stack growth;
   - C/runtime transitions;
   - raise/unwind paths;
   - statepoint/stackmap record points;
   - entry/return to mixed native/LLVM code.

   If any runtime mechanism can observe `x28` or `x27` at an arbitrary machine
   instruction, then Design 1 needs either stronger pinning or explicit syncs at
   those observation points.

2. **Inline asm or helper code that directly reads physical `x28`/`x27`.**

   Existing lowering has places that read physical runtime registers directly.
   Under Design 1, such reads are only valid at a point where the value has been
   forced into the physical register. Otherwise they must be rewritten to use the
   SSA value.

3. **Exceptional paths.**

   A caller-side pre-call sync is not enough if the callee can allocate and then
   raise. The callee's raise path must itself cross a boundary that forces the
   current `alloc` into `x27`, or the exceptional edge must carry the updated
   state explicitly.

4. **Ordinary values using `x27`/`x28`.**

   If `ds`/`alloc` are not live, LLVM might allocate unrelated values to
   `x27`/`x28`. That is fine only if no runtime code observes those registers at
   that point. Again, the key question is whether observation is boundary-based.

### Design 1 experiments

This should be the first serious experiment.

1. Stop reserving `x27` and `x28` for OxCaml functions, but keep reserving
   registers that really must not be allocated, such as `x26` if trap handling
   still requires it and `x16`/`x17` for linker/runtime temporaries.
2. Keep the existing runtime-state threading in IR.
3. Keep the current OxCaml calling conventions that place the first two
   integer args/results in `x28`/`x27`.
4. Remove or disable ad hoc physical runtime-register reads that bypass the SSA
   state, except where the boundary contract proves them valid.
5. Inspect generated assembly for:
   - `variant_dispatch_with_int_payload`;
   - `recursive_fib_small`;
   - allocation fast-path loops;
   - C/runtime helper calls.
6. Run focused correctness tests:
   - allocation;
   - poll;
   - stack growth;
   - exception handling;
   - effects;
   - C calls;
   - mixed native/LLVM calls if available.
7. Benchmark the known slow cases and the compiler-built-with-LLVM benchmark.

Success criteria:

- `ds` mostly lives in `x28` and `alloc` mostly lives in `x27` without explicit
  round trips.
- Tiny direct-call slowdowns improve without special call paths.
- Allocation fast paths remain at parity.
- Focused runtime/exception/stack tests pass.

## Design 2: fixed runtime registers with explicit operations

This design goes the other way: `x28` and `x27` are fixed runtime registers,
and LLVM code manipulates them through explicit target-aware operations.

Contract:

- `x28` is always the domain-state pointer in generated OCaml code.
- `x27` is always the current allocation pointer in generated OCaml code.
- Fast allocation explicitly updates `x27`.
- Domain-state loads explicitly use `x28`.
- Calls and runtime boundaries assume the physical registers are already in the
  runtime ABI state.

This is closer to the native backend.

### Main risks for Design 2

1. **Less freedom for LLVM.**

   LLVM cannot choose better homes for `ds` and `alloc`; those registers are
   fixed even when pressure is high.

2. **Need real target operations, not opaque inline asm.**

   Inline asm is useful for experiments, but it is a poor long-term
   representation. It hides semantics from LLVM and can block scheduling.

   The production shape should be target intrinsics or MachineInstr pseudos
   with exact effects:

   ```text
   read_ds_from_x28
   write_ds_to_x28
   read_alloc_from_x27
   write_alloc_to_x27
   alloc_fast_path_sub_from_x27
   ```

   These operations should model register uses/defs precisely and should not
   claim to touch memory unless they really do.

3. **Statepoint and stackmap integration.**

   If `x27`/`x28` are always physical state, statepoint emission and stackmap
   consumers need to know that. This is manageable, but it must be explicit.

4. **Optimization quality around explicit operations.**

   If the operations are too barrier-like, LLVM will schedule poorly. If they
   are too weak, LLVM may move them across boundaries where the runtime needs
   current state.

### Design 2 experiments

1. Prototype with inline asm only to test the performance ceiling.
2. If promising, replace inline asm with target pseudos/intrinsics.
3. Check whether allocation loops, tiny direct calls, and C/runtime calls get
   native-like assembly.
4. Check that scheduling around the explicit operations is not worse than the
   current state-threading design.

## How to choose

Design 1 should be tried first because it is simpler and more LLVM-native. It
keeps the existing SSA state-threading model and fixes the main self-inflicted
problem: reserving the exact registers that the calling convention wants.

Design 2 is the fallback if Design 1 fails because the runtime really needs
`x27`/`x28` to be current outside well-defined boundaries, or because LLVM does
not reliably coalesce `ds`/`alloc` into those registers even after they are made
allocatable.

## Concrete next step

Run Design 1 as an experiment:

1. Make `x27` and `x28` allocatable for OxCaml functions.
2. Keep `ds`/`alloc` threaded as SSA values through the existing calling
   conventions.
3. Audit direct physical reads/writes of `x27`/`x28` and either remove them or
   restrict them to boundaries where the calling convention has made the
   physical register value valid.
4. Rebuild the custom LLVM and compiler.
5. Compare assembly and timings against the current branch on the known slow
   cases.
6. Run the focused runtime correctness tests before considering broader
   testsuite/self-stage validation.

## Current Design 1 prototype results

Implemented prototype shape:

- Stop reserving `x27` and `x28` in `AArch64RegisterInfo.cpp` for OxCaml
  calling conventions.
- Stop passing `-ffixed-x27` and `-ffixed-x28` to clang.
- Remove the old special `caml_send*` path that read physical `x27/x28` after
  proving the CFG had no heap allocation. That path was only valid when those
  registers were globally reserved.
- Keep `alloc` and `ds` as SSA values and let the OxCaml calling convention
  place them in `x27/x28` at calls.
- For AArch64 exception entries, snapshot the physical exception-boundary state
  into an entry-block stack slot:

  ```asm
  str x0,  [slot]       ; exception bucket/value
  str x28, [slot, #8]   ; domain state at the boundary
  str x27, [slot, #16]  ; allocation pointer at the boundary
  str x26, [slot, #24]  ; previous trap pointer
  ```

  This is not on the normal hot path. It runs only on the exception-return
  entry from `wrap_try`.

Important finding:

- The exception entry is a hidden control-flow edge. LLVM only sees the normal
  branch after `wrap_try`, but the runtime can arrive at the exception-entry
  label directly.
- Therefore values used by handler code must be materialized in or after the
  exception-entry block, not in the visible predecessor.
- A boundary inline-asm clobber of `~{x30}` is not enough on AArch64 LLVM.
  Standalone LLVM IR experiments showed that `~{x30}` does not make LLVM treat
  the link register as clobbered, while `~{lr}` does.
- Using `~{lr}` fixed the observed `Stdlib__Arg.add_help` failure where
  `Not_found` was compared against a value left in `x30/lr` from code that the
  real exception edge skipped.

Validation so far:

- `make llvm-compiler` succeeds.
- `_build/main/tools/make_opcodes.exe -opnames < runtime/caml/instruct.h`
  succeeds; this previously reproduced the bad exception entry.
- `make llvm-test-one-no-rebuild TEST=testsuite/tests/llvm-codegen/exceptions.ml`
  passes under `OCAMLPARAM=_,llvm-backend=1`.
- `make llvm-test-one-no-rebuild TEST=testsuite/tests/match-exception/allocation.ml`
  passes under `OCAMLPARAM=_,llvm-backend=1`.
- A small allocation-loop probe compiles and runs with the LLVM backend, and the
  clang wrapper log no longer contains `-ffixed-x27` or `-ffixed-x28`.
