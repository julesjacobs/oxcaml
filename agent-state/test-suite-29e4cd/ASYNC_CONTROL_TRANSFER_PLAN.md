# LLVM async control-transfer plan

## Problem statement

The LLVM backend currently mixes two different runtime control-transfer
mechanisms:

- Ordinary exceptions transfer through `Caml_state->exn_handler` / `TRAP_PTR`.
  These are caught by the active ordinary `try`.
- Async exceptions transfer through `Caml_state->async_exn_handler`. The runtime
  routes them by setting `exn_handler = async_exn_handler` before raising, so
  they skip intervening ordinary `try` handlers and arrive at the current
  `Sys.with_async_exns` callback boundary.

This distinction matters for LLVM because an `invoke` unwind edge to the active
ordinary trap is valid only for operations that may transfer through the
ordinary exception handler. "Can raise" is too coarse: the lowering decision is
whether the operation may unwind to the active ordinary trap.

## Current evidence

Runtime inspection shows:

- `caml_raise_async` in `runtime/fail_nat.c` restores local allocation state,
  sets `Caml_state->exn_handler = Caml_state->async_exn_handler`, marks
  `raising_async_exn`, and then calls `caml_raise_exception`.
- `caml_call_gc` calls `caml_garbage_collection`. For polls, that processes
  pending actions. For heap allocations, `caml_alloc_small_dispatch` processes
  pending actions and raises callback/finalizer exceptions through
  `caml_raise_async_if_exception`.
- Native AArch64 stack reallocation failure switches from OCaml to C and
  branches to `caml_raise_async`.
- LLVM AArch64 stack reallocation failure currently branches to
  `caml_raise_exn` while still on the OCaml stack.

Focused experiments show:

- An async finalizer exception raised during heap allocation inside an inner
  ordinary `try` is caught by the outer `Sys.with_async_exns` handler for both
  native and LLVM. This is a characterization test for the runtime rule: async
  allocation exceptions skip ordinary handlers.
- A `Stack_overflow` raised by stack reallocation inside an inner ordinary
  `try` is caught by the outer `Sys.with_async_exns` handler for native, but by
  the inner ordinary handler for LLVM. This is a real LLVM behavioral bug.

## Lowering rule

Centralize the LLVM lowering decision around one question:

> May this operation unwind to the active ordinary trap?

Use that question to decide whether to pass an ordinary `unwind_label` to the
call emission path.

### Ordinary-trap operations

These may transfer through `Caml_state->exn_handler` / `TRAP_PTR`.

Examples:

- ordinary OCaml calls that can raise
- C calls that may transfer through `caml_raise`
- explicit raise/reraise paths

LLVM should model these with an ordinary unwind edge to the active trap when
there is an active trap. Liveness for such edges must include both normal live
roots across the call and roots live at the ordinary exception target.

### Not-ordinary-trap operations

These do not transfer to the active ordinary trap. They may return normally,
transfer through `Caml_state->async_exn_handler`, perform an effect/preemption,
or terminate fatally. Those differences matter to the runtime, but they do not
justify an LLVM unwind edge to the active ordinary trap.

Examples currently in scope:

- poll slow paths
- heap-allocation `caml_call_gc` slow paths, after auditing ordinary OOM paths
- AArch64 LLVM stack-check failure after the runtime stub is fixed to match
  native

LLVM should emit these as plain calls/statepoints with no ordinary
`unwind_label`. This must preserve all non-exception metadata:

- `statepoint-id`
- active trap depth encoded in the statepoint ID
- stack offset encoded in the statepoint ID
- heap allocation size encoded in the allocation statepoint ID
- normal `gc-live` roots
- slow-path root storage required for normal roots

Only the ordinary exception successor and ordinary handler live-in roots should
be removed.

### No-exception-transfer operations

These do not ordinary-unwind and do not have an async/effect transfer path.

Examples:

- no-effect/no-raise externals
- local allocation stack reallocation, if the audit confirms it only returns or
  fatal-errors on unrecoverable failure

Do not include local allocation in the first implementation diff unless the
audit shows it needs a change.

## Red and characterization tests

Split the tests into characterization tests and red tests.

Characterization tests should pass before and after the fix:

- `allocation_async_exception_skips_ordinary_try`: an async finalizer exception
  raised during heap allocation inside an inner ordinary `try` is caught by the
  outer `Sys.with_async_exns` handler for both native and LLVM.
- A focused poll/preemption/effect validation, if there is an existing small
  harness, to show poll-driven transfers still work after removing the ordinary
  trap edge.

Red tests should fail before the fix and pass after it:

- `llvm_stack_overflow_uses_async_handler`: on AArch64, force the LLVM
  stack-check path to call `caml_llvm_call_realloc_stack` under an inner
  ordinary `try`. The current result is `inner`; the fixed result is `outer`.
  Make the test deterministic with a constrained stack or other reliable
  trigger, avoid tail recursion hiding the stack check, and verify the generated
  output reaches `caml_llvm_call_realloc_stack`.
- `basic_safepoints_do_not_unwind_to_ordinary_trap`: generated-output checks
  for Poll, heap Alloc, and fixed Stack_check inside an active ordinary `try`
  should show no `invoke ... unwind` edge to the ordinary trap.
- `ordinary_calls_still_unwind_to_ordinary_trap`: a matching generated-output
  check should show that an ordinary-raising call in the same shape still uses
  the active ordinary trap.

The current untracked `llvm-gc-roots/active_trap_basic_roots` test asserts the
wrong model for Poll and heap Alloc. Replace it rather than promote it:

- handler-only roots should not appear at Poll/heap Alloc/Stack_check
  statepoints once they have no ordinary trap edge
- normal values live across those safepoints must still appear as roots,
  including when the safepoint is inside an active ordinary `try`
- ordinary-trap call tests should cover handler live-in roots

Avoid large golden snapshots. Prefer small scripts that compile focused source
and check only the relevant IR/asm lines.

## Green implementation

1. Fix the LLVM AArch64 stack reallocation stub.

   In `runtime/arm64.S`, change the failure path for
   `caml_llvm_call_realloc_stack` to match the native stack reallocation
   failure path:

   - restore the `gc_regs` bucket as it does today
   - pop the saved frame
   - switch from OCaml to C before calling the C runtime
   - load `caml_exn_Stack_overflow`
   - branch to `caml_raise_async`

   Do not make a direct `caml_raise_exn` -> `caml_raise_async` substitution
   while still on the OCaml stack.

2. Centralize the ordinary-trap decision in `backend/llvm/llvmize.ml`.

   Add a small helper or equivalent local abstraction whose contract is:
   "does this operation need an active ordinary trap unwind edge?" Keep the
   helper focused on the lowering decision, not a broad runtime taxonomy unless
   the broader type is actually used.

3. For Poll, heap Alloc, and fixed AArch64 Stack_check, emit the runtime call
   with no ordinary `unwind_label`.

   The likely minimal mechanism is to pass no `unwind_label` to the existing
   call emission path, so `call_simple` emits a `call` rather than an `invoke`.
   Preserve `gc_attr`, `call_gc_for_basic_safepoint`,
   `call_runtime_for_basic_safepoint`, statepoint IDs, allocation metadata, and
   normal root bundles.

4. Define root calculation for not-ordinary-trap basic safepoints.

   The root set should be the normal safepoint roots live across the runtime
   call. It should not include roots live only in the active ordinary exception
   target. Tests should prove both halves:

   - handler-only roots disappear from not-ordinary-trap safepoints
   - normal live-across roots remain present

5. Keep ordinary active-trap unwind edges for operations that really may use the
   ordinary exception handler.

   Do not change ordinary OCaml calls, C calls that may transfer through
   `caml_raise`, explicit raise/reraise paths, or their handler-root behavior
   except as needed to use the centralized classification.

## Scope

The stack-check runtime fix is AArch64-only because the LLVM stack-check
lowering in this checkout is AArch64-only. Do not generalize the runtime-stub
part to other architectures without a separate audit.

The Poll/heap Alloc lowering change applies to the LLVM backend's AArch64
active-trap modeling. Before changing heap Alloc, audit the exact
`caml_call_gc` heap-allocation path for ordinary `Out_of_memory` or any other
ordinary exception path. If an ordinary exception path exists, keep or split the
ordinary edge for that path instead of treating all heap allocation slow paths
as not-ordinary-trap.

## Validation

Run validation in this order:

1. Focused behavioral tests:
   - allocation async finalizer skips ordinary `try`
   - LLVM stack-check `Stack_overflow` lands at `Sys.with_async_exns`
2. Focused generated-output checks:
   - Poll/heap Alloc/fixed Stack_check do not lower to `invoke ... unwind` to
     the ordinary trap
   - ordinary-raising calls still do
   - normal roots remain present at not-ordinary-trap safepoints
   - handler-only roots remain present for ordinary-trap calls and absent for
     not-ordinary-trap safepoints
3. Existing async-exn tests.
4. Relevant LLVM GC-root tests after replacing the stale
   `active_trap_basic_roots` direction.
5. A focused poll/preemption/effect test if available.
6. Full self-stage2 validation after the focused tests pass.

## Open audit points

- Confirm whether local allocation stack reallocation can ever ordinary-raise.
  Current runtime inspection suggests it fatal-errors on unrecoverable local
  stack allocation failure rather than raising through `exn_handler`.
- Confirm whether the LLVM heap-allocation `caml_call_gc` path can ordinary
  raise `Out_of_memory` or any other ordinary exception.
- Confirm whether LLVM lowering ever needs `invoke` for affected safepoints for
  reasons unrelated to ordinary exception transfer. If so, the unwind
  destination must not be the active ordinary trap unless the runtime transfer
  channel is ordinary.
- Confirm that removing ordinary unwind edges from Poll/heap Alloc/Stack_check
  does not regress frame tables, stack maps, or GC root recording. The intended
  change is only the ordinary-trap edge and ordinary-handler live-in roots.

## Review-loop changes

The human-like review loop changed the plan in these ways:

- Replaced overlapping transfer categories with the explicit lowering question:
  "may this operation unwind to the active ordinary trap?"
- Made the replacement IR shape explicit: plain call/statepoint with existing
  safepoint metadata and normal root bundles preserved.
- Scoped the stack-check runtime fix to AArch64 and documented the required
  OCaml-to-C switch before `caml_raise_async`.
- Split characterization tests from red tests.
- Added an OOM audit precondition for heap allocation.
- Added tests for root preservation, handler-only root removal, ordinary-call
  behavior, and the stale `active_trap_basic_roots` direction.
