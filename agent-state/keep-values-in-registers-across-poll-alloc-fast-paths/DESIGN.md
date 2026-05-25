# Design

## Problem

The LLVM backend currently treats poll and allocation checks as points where
live values may need to be made available through memory or other conservative
locations. The target improvement is to keep values in registers across the
fast path of these checks when doing so is correct.

This design doc uses these terms:

- `check`: a poll check or allocation check.
- `fast path`: the fallthrough path when the check succeeds.
- `slow path`: the path that calls into runtime support when the check fails.
- `live value`: a value needed after the check.
- `register-resident value`: a live value that remains in a machine register
  across the fast path.

## Goal

Keep live values register-resident across check fast paths without weakening the
slow path's ability to reconstruct the runtime-visible state it needs.

Expected benefits:

- Reduce spills around frequent poll and allocation checks.
- Reduce fast-path instruction count.
- Preserve existing slow-path correctness and debugging behavior.

## Non-goals

- Do not change the language-level semantics of polls or allocations.
- Do not change the runtime calling convention unless the goal is updated to
  require it.
- Do not optimize uncommon slow-path code at the cost of fast-path clarity.

## Constraints

- Fast-path code must preserve all values required after the check.
- Slow-path code must have access to any values required by the runtime, GC, and
  exception or debugging metadata.
- The implementation should fit the existing backend structure instead of
  introducing a separate register allocation model.
- Validation should start with a focused reproducer that shows unnecessary
  spilling or reloading around a check.

## Approach Sketch

1. Identify the current lowering points for poll checks and allocation checks.
2. Find where live values are forced out of registers around these checks.
3. Classify each forced location by reason:
   - required by slow-path runtime state,
   - required by metadata,
   - required by an existing conservative compiler invariant,
   - incidental artifact of the current lowering.
4. Preserve only the requirements that are needed on the slow path.
5. Keep values register-resident on the fast path when the value is not needed
   by the slow path or can be materialized only on the slow path.
6. Add a focused test that distinguishes fast-path register preservation from
   slow-path state reconstruction.

## Candidate Implementation Shapes

### Split Fast Path and Slow Path State

Represent the fast path and slow path requirements separately in the lowering.
The fast path keeps normal liveness and register allocation behavior. The slow
path receives explicit state operands only for values it needs.

This is the preferred shape if the current backend already has a clean place to
attach slow-path operands.

### Late Slow-path Materialization

Leave fast-path values in registers and insert materialization only on the slow
path edge. This can be useful when the slow path needs a stack location or a
runtime-specific representation that the fast path does not need.

This is appropriate only if the materialization is legal after the branch and
does not hide a GC or metadata requirement.

### Narrow Existing Conservative Barriers

If the current implementation uses a broad barrier around checks, narrow that
barrier so it only covers the exact values needed by the slow path.

This is the smallest change if the existing conservative behavior is localized.

## Validation Plan

Start with assembly or compiler IR evidence from one small function containing a
poll check or allocation check with live values after the check. The first useful
test should show that a value remains in a register on the fast path.

Then validate:

- Existing focused backend tests for poll checks.
- Existing focused backend tests for allocation checks.
- One reduced program that exercises the slow path, if practical.

Do not start a broad build or long test run until the focused evidence is in
place.

## Open Questions

- Which exact lowering pass introduces the current spill or reload behavior?
- Are poll checks and allocation checks handled by the same mechanism or by two
  independent mechanisms?
- Which values are required by the slow path for GC safety, runtime calls, and
  metadata?
- Can slow-path-only materialization be represented before register allocation,
  or does it need to happen later?
