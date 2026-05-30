# GC Aggregate Explosion Plan

Goal: make first-class SSA aggregates that contain GC pointers visible to
RewriteStatepointsForGC, without forcing physical root slots early.

## Problem

RS4GC tracks scalar GC pointer SSA values. It does not look inside a first-class
aggregate value. This is unsafe when an aggregate containing `ptr addrspace(1)`
crosses a safepoint and the pointer field is extracted only after the
safepoint.

Bad shape:

```llvm
%agg = call { i64, ptr addrspace(1) } @make_pair(...)
call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
%p = extractvalue { i64, ptr addrspace(1) } %agg, 1
ret ptr addrspace(1) %p
```

RS4GC sees `%agg` live across the safepoint but does not see `%p`.

## Proposed Transform

Add a late RS4GC prepass that scalarizes internal first-class SSA aggregates
whose type recursively contains a GC pointer.

For each aggregate value, build a stable leaf mapping:

```text
%agg -> [leaf path -> scalar SSA value]
```

Then rewrite aggregate-manipulation operations:

- `extractvalue %agg, path`: replace with the mapped leaf, or with a rebuilt
  sub-aggregate if the extracted result is itself aggregate.
- `insertvalue %agg, %field, path`: create a new leaf mapping equal to the base
  mapping with the inserted field leaves overwritten.
- aggregate `phi`: create one scalar PHI per leaf.
- aggregate `select`: create one scalar select per leaf.
- call result: extract all leaves immediately after the call.
- invoke result: extract all leaves at the start of the normal destination.
- function aggregate argument: extract all leaves in the entry block if the
  function body uses it internally.

Rebuild the aggregate only at real boundaries that still require aggregate
shape:

- `ret` returning aggregate,
- call argument to a non-statepoint callee,
- store of aggregate to memory,
- other non-understood uses.

For the initial implementation, support only the internal SSA shapes needed by
the current failure and tests. Keep the existing fail-closed diagnostic for any
remaining aggregate containing GC pointers that reaches RS4GC liveness or
statepoint operands.

## Invariants

After the prepass:

1. No first-class aggregate containing `ptr addrspace(1)` should be live across
   a safepoint through ordinary internal SSA uses.
2. Any scalar GC pointer field that crosses a safepoint should be a direct
   `ptr addrspace(1)` value, so RS4GC can relocate it.
3. Boundary cases not handled by the prepass must fail closed under
   `-rs4gc-fail-on-unhandled-gc-aggregate`.

## Tests

Add LLVM IR tests for:

- call result aggregate crossing one safepoint,
- call result aggregate crossing two safepoints,
- two GC pointer fields,
- nested aggregate with GC pointer field,
- aggregate PHI,
- aggregate select,
- aggregate invoke normal result,
- insertvalue chain followed by extract after a safepoint,
- aggregate returned from the function,
- unsupported aggregate statepoint argument, expected diagnostic when the
  fail-closed flag is enabled.

## Self-Review 1

Concern: global aggregate scalarization can accidentally change ABI or memory
semantics.

Revision: do not erase or rewrite boundary operations in the first version
unless they are simple `extractvalue` consumers. The first version should
scalarize enough to expose GC fields to RS4GC and leave ABI/memory values alone
unless all uses are internal. Boundary aggregate uses remain visible to the
tripwire.

## Self-Review 2

Concern: PHI cycles make a recursive "get leaves" implementation fragile.

Revision: handle PHIs in two phases. First create placeholder scalar PHIs for
each aggregate PHI leaf. Then fill their incoming values after mappings for
incoming aggregate values are available.

## Self-Review 3

Concern: `invoke` result extraction cannot be inserted immediately after the
`invoke`, because an invoke is a terminator.

Revision: create result leaf extracts at the first insertion point of the
normal destination. This matches LLVM's rule that the invoke result is only
available on the normal edge.

## Self-Review 4

Concern: aggregate values can be rebuilt after one safepoint and then cross a
later safepoint again.

Revision: the pass should not rely on manual local split/rebuild sites. It
should map every supported aggregate-producing instruction, so later
`extractvalue` users use scalar fields from the mapping. This makes repeated
safepoints safe as long as the aggregate does not escape through an unsupported
boundary.

## Self-Review 5

Concern: trying to rebuild sub-aggregates for extracted aggregate values may
reintroduce hidden GC fields.

Revision: for the first implementation, only replace `extractvalue` when the
extracted type is scalar. Aggregate-valued extracts are kept unless a later
scalar extract causes the containing mapping to be used. The diagnostic remains
the guard for unsupported remaining aggregate values.

## Decision

This is viable as an incremental LLVM-side prototype if scoped narrowly:

- implement scalar leaf exposure for internal SSA aggregate values,
- cover call/invoke results, insertvalue, PHI, select, and scalar
  extractvalue,
- keep unsupported aggregate boundaries fail-closed,
- validate on focused LLVM IR tests before trying to remove frontend roots.

This does not by itself solve hidden memory roots or exception-root slots. It
solves the distinct "GC pointer hidden inside first-class aggregate SSA" part
of the late-root design.

## Implementation Status

Implemented a first RS4GC prepass in
`vendor/llvm-project/llvm/lib/Transforms/Scalar/RewriteStatepointsForGC.cpp`.

Implemented cases:

- aggregate call results,
- aggregate invoke normal results,
- aggregate `insertvalue` chains,
- aggregate `extractvalue` chains,
- aggregate PHIs,
- aggregate selects,
- aggregate function arguments inside the function body.

Musttail aggregate call results are deliberately left alone. In real OxCaml IR
they are immediately returned, so they are not live across a later safepoint.
Exploding them would insert instructions between the `musttail` call and `ret`,
which breaks LLVM's musttail invariant.

The pass is OxCaml-only and is gated by hidden option
`-rs4gc-explode-gc-aggregates`, which defaults to enabled. The existing hidden
diagnostic option `-rs4gc-fail-on-unhandled-gc-aggregate` now also checks for
remaining unsupported aggregate boundary uses after the explosion pass.

Added LLVM IR tests:

- `vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-gc-aggregate-explosion.ll`
- `vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-gc-aggregate-explosion-negative.ll`

Validation run:

```sh
cmake --build /tmp/oxcaml-agent-llvm-fast-path-roots-integration/llvm-build \
  --target opt llc -- -j8

opt -S -passes=rewrite-statepoints-for-gc,verify \
  -rs4gc-fail-on-unhandled-gc-aggregate \
  vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-gc-aggregate-explosion.ll \
  -o - | FileCheck \
  vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-gc-aggregate-explosion.ll

opt -S -passes=rewrite-statepoints-for-gc,verify \
  vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-gc-aggregate-explosion.ll \
  -o - | FileCheck \
  vendor/llvm-project/llvm/test/Transforms/RewriteStatepointsForGC/oxcaml-gc-aggregate-explosion.ll
```

Manual negative test also passed by checking the expected diagnostic for an
unsupported aggregate argument to a statepoint call.

Codegen sanity:

- `llc -O3` on the rewritten prototype IR produced one stack slot for one
  live GC field across repeated safepoints.
- Two live GC fields used two adjacent slots while both fields were live.
- PHI/select/invoke-normal-result cases did not show a duplicate-slot
  explosion in the prototype assembly.

Focused OxCaml test:

```sh
make llvm-test-one TEST=typing-layouts-iarrays/test_scannable_product_iarray_4.ml \
  LLVM_PATH="$LLVM_PATH"
```

Result: 5 passed, 0 failed.

Real compiler unsafe-mode test:

After rebuilding `clang`, the previously failing slow-path-root diagnostic mode
now passes the focused reproducer:

```sh
make test-one-no-rebuild \
  TEST=typing-layouts-iarrays/test_scannable_product_iarray_4.ml \
  TEST_RUN_OCAMLPARAM="_,llvm-backend=1,llvm-path=$LLVM_PATH,llvm-unsafe-no-slow-path-root-slots=1"
```

Result: 5 passed, 0 failed.

The whole nearby directory also passes in the same unsafe mode:

```sh
make test-one-no-rebuild \
  DIR=typing-layouts-iarrays \
  TEST_RUN_OCAMLPARAM="_,llvm-backend=1,llvm-path=$LLVM_PATH,llvm-unsafe-no-slow-path-root-slots=1"
```

Result: 81 passed, 0 failed.

## Post-Implementation Review

What looks good:

- The original hidden-field problem is addressed directly: scalar GC fields are
  extracted before safepoints and appear in `gc-live`.
- The transform runs late, inside RS4GC, so it does not expose this physical
  shape to the normal scalar optimizer.
- Unsupported aggregate boundaries still have a diagnostic path instead of
  being silently guessed.

Known limits:

- This is not the full late-root implementation. Hidden memory roots and
  explicit exception-root slots remain separate work.
- The negative diagnostic is intentionally a development check. Normal builds
  do not reject every remaining aggregate use unless the hidden fail option is
  enabled.
- Aggregate stores, aggregate returns, and aggregate arguments to statepoint
  calls are not lowered yet. They should be added only with tests for the exact
  required semantics.
