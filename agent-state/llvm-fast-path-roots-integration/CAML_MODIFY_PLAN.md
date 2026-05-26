# Plan: `caml_modify` Fast Path

Date: 2026-05-26.

This plan mirrors the string-compare plan style: start with the runtime facts,
define candidate generated shapes, run small experiments before committing to a
large implementation, then benchmark the compiler binary.

Scope note: this is a follow-up optimization plan, not part of the fast-path
root-slot PR #18. PR #18 should stay focused on root-slot traffic around
allocations and poll points. `caml_modify` lowering should either get its own
follow-up PR or be explicitly added to a retargeted goal before implementation.

## Goal

Reduce LLVM-built compiler time by removing noalloc C-call wrapper overhead from
hot `caml_modify` writes while preserving:

- OCaml's write-barrier semantics;
- the multicore memory model;
- TSAN/debug/runtime modes;
- GC correctness for remembered sets and major marking.

The motivating `typecore.ml` profile:

```text
caml_modify total calls:                 88,484,839
old value old while marking active:       5,974,421
new value young, destination old:         3,094,363
```

Those two slow categories are not a joint histogram, so the exact "did real GC
barrier work" count is between `6.8%` and `10.2%`. The hot case is still the
vast majority.

## Runtime Facts

Current `runtime/memory.c`:

```c
write_barrier((value)fp, 0, *fp, val);
atomic_thread_fence(memory_order_acquire);
atomic_store_release(&Op_atomic_val((value)fp)[0], val);
```

`write_barrier` does no GC work when:

- `fp` points inside the minor heap reservation;
- or the old field value is a young block;
- or the old field value is an immediate/non-block and the new value is not a
  young block;
- or the old field value is an old block but major marking is not active and
  the new value is not a young block.

`write_barrier` does GC work when:

- new value is a young block and destination is old: add `fp` to the major-ref
  remembered set;
- old value is an old block and major marking is active: darken the old value.

The acquire fence and release store happen for every write. They are not part
of the cold GC-barrier work.

Value predicates must match the runtime exactly:

```text
Is_block(x) = low bit is 0 and x != 0
Is_young(x) = caml_minor_heaps_start < x < caml_minor_heaps_end
Is_block_and_young(x) = Is_block(x) && Is_young(x)
```

`Is_young` is a reserved-minor-heaps range check, not a current-domain active
minor-heap check.

The existing LLVM store path for `Cfg.Store (_, _, is_modify = true)` already
emits an acquire fence on AArch64 word/value stores. It currently lowers the
store itself through the generic LLVM `Store`, whose IR representation has no
atomic ordering field. That is a known memory-model detail this plan must not
hide.

## Design Constraint

Do not start by inlining all of `caml_darken`.

`caml_darken` can:

- adjust infix pointers;
- mark headers;
- handle continuations via `caml_darken_cont`;
- update marking counters;
- push scan ranges on the mark stack;
- grow or compress the mark stack through `mark_stack_push_block`;
- interact with concurrent marking state.

That is too much GC policy to duplicate in `llvmize.ml` in the first PR.

`Ref_table_add` is different. Its fast path is mechanically small:

```c
if (ref->ptr >= ref->limit) caml_realloc_ref_table(ref);
*ref->ptr++ = fp;
```

So the remembered-set append fast path is a plausible inline target, with only
table growth kept as a cold C helper.

## Candidate Shapes

### Candidate 0: Baseline

Current lowering:

```text
call c_call_wrapper.caml_modify(fp, val)
```

This pays:

- OCaml-to-C stack switch wrapper;
- wrapper call/return;
- real helper call/return;
- caller register pressure and live-root machinery around the wrapper.

### Candidate 1: Inline Classification, Combined Slow Helper

Inline only cheap classification and the required fence/store:

```text
old = load fp

need_slow =
  fp is old
  &&
  old is not young block
  &&
  (
    newval is young block
    ||
    (old is old block && marking active)
  )

if unlikely(need_slow):
  cold call caml_modify_slow_barrier(fp, old, newval)

fence acquire
store release newval -> fp
```

The slow helper repeats the exact runtime barrier logic. Candidate 1 should be
boring on purpose: the prefilter may over-call the helper, but it must not
change the helper's API or semantics. A later candidate can specialize the
helper only after the exact no-work/work split is locked down by tests.

Pros:

- Minimal runtime surface.
- Easy correctness story.
- Good first benchmark for "does removing the wrapper help?"

Cons:

- Old-to-young writes still call a helper even though `Ref_table_add` fast path
  is tiny.
- The combined helper may duplicate some checks already done in generated code.

This is the first correctness prototype.

### Candidate 2: Inline Remembered-Set Append, Cold Marking Helper

Inline classification, remembered-set append fast path, fence/store:

```text
old = load fp

if fp is old:
  old_is_block = Is_block(old)
  old_is_young = old_is_block && Is_young(old)

  if not old_is_young:
    if old_is_block && marking active:
      cold call caml_modify_darken_old(old)

    if newval is young block:
      ref = &Caml_state->minor_tables->major_ref
      if likely(ref->ptr < ref->limit):
        *ref->ptr = fp
        ref->ptr = ref->ptr + 1
      else:
        cold call caml_realloc_ref_table(ref)
        *ref->ptr = fp
        ref->ptr = ref->ptr + 1

fence acquire
store release newval -> fp
```

Pros:

- Removes a helper call for the most common real barrier case.
- Keeps `caml_darken` policy in C.
- The inlined remembered-set path is close to existing `Ref_table_add`.
- Preserves the runtime's `old_val`-young early return, so an already
  remembered field is not appended again just because `newval` is young.

Cons:

- Larger generated code.
- Needs careful access to `Caml_state->minor_tables->major_ref`.
- Must ensure table-growth path is correct and cold.

This is likely the best first performance candidate if Candidate 1 is correct.

### Candidate 3: Inline `caml_darken` Prefilter Only

This is out of scope for the first implementation PR. Keep it here only as a
later experiment to run if Candidate 2 proves that marking helper calls still
dominate.

Inline a stricter marking prefilter:

```text
if old is old block && marking active:
  hd = load header
  if old is infix:
    cold call caml_modify_darken_old(old)
  else if header status is UNMARKED:
    cold call caml_modify_darken_old_known_markable(old, hd)
```

The helper still performs the mark. The prefilter only avoids calls when the old
value is already marked/not relevant.

Pros:

- Avoids some marking helper calls.
- Does not duplicate mark-stack policy.

Cons:

- Needs header status constants and infix handling in LLVM lowering.
- More risk for less likely benefit.

Do not implement until Candidate 2 is measured and a profile shows how often
the old-old-while-marking case is already marked, not markable, an infix
pointer, or a continuation.

### Candidate 4: Full `caml_darken` Inline

Inline header marking and simple mark-stack push.

Reject as an early plan. It may be a later research direction, but it is too
invasive for the first optimization.

## Coldness Contract For LLVM

LLVM should see the generated shape as a hot write with cold barrier side exits,
not as a generic helper call.

Required information:

- Branch probability on `fp is old`: profile-dependent, not always rare.
- Branch probability on `newval is young block`: cold enough to side-exit.
- Branch probability on `old is old block && marking active`: cold enough to
  side-exit.
- Branch probability on `ref->ptr >= ref->limit`: very cold.
- Slow helper calls marked `cold` where the IR/function attribute layer supports
  it.

Implementation options:

1. Start with `llvm.expect.i1` because the local IR layer already has intrinsic
   support patterns.
2. If layout is not good enough, add branch-weight metadata support to
   `backend/llvm/llvm_ir.ml`.
3. Keep probabilities centralized in `llvmize.ml`, not scattered literals.

Initial conservative probabilities:

```text
need remembered-set append: about 3.5% in typecore
need marking work: about 6.8% in typecore
ref-table growth: much colder than remembered-set append
```

The exact overlap between remembered-set and marking work is unknown. Do not
overfit codegen to `10.2%` as a single branch probability.

## Runtime Helper Surface

Add only helpers that are hard to express safely in generated LLVM.

Candidate 1 helper:

```c
void caml_modify_slow_barrier(volatile value *fp, value old_val, value new_val);
```

This helper should share the exact runtime barrier logic. It may be implemented
as a small C wrapper around the existing `write_barrier` code, but it should not
encode a subtly different "slow barrier" contract.

Candidate 2 helpers:

```c
void caml_darken(void *domain_state, value old_val, volatile value *ignored);
void caml_modify_realloc_ref_table(struct caml_ref_table *tbl);
```

Prefer the existing `caml_darken(Caml_state, old_val, NULL)` call if the
generated call boundary can pass `Caml_state` cleanly. Add a tiny
`caml_modify_darken_old(old_val)` wrapper only if it materially simplifies the
ABI and that reason is documented in the implementation.

`caml_modify_realloc_ref_table` may simply be the existing
`caml_realloc_ref_table` if that symbol is already suitable to call from
generated code.

Do not expose mark-stack internals to generated code in the first PR.

## Runtime Layout Contract

Candidate 2 must not hard-code C struct offsets casually in `llvmize.ml`.
Inlining `Caml_state->minor_tables->major_ref.{ptr,limit}` requires a stable
layout source, for example:

- generated runtime-layout constants consumed by the LLVM backend;
- compile-time C assertions that check any backend constants against
  `offsetof`;
- or a centralized LLVM runtime-layout module used for all domain-state and
  runtime-table accesses.

Until that exists, Candidate 2 can still be tested with a C helper for
remembered-set append, but the real inline append should be blocked. The first
PR should not spread `minor_tables`, `major_ref`, `ptr`, and `limit` offsets as
ad hoc literals.

## Helper Calling Convention

Cold helper calls still need a valid OCaml-to-C boundary. The first
implementation should use the existing noalloc C-call wrapper path for cold
helpers, or a mechanically equivalent cold stack-switch sequence, so the hot
path avoids the wrapper while the slow path keeps today's calling convention.

Per helper contract:

- `caml_modify_slow_barrier(fp, old_val, new_val)`: no OCaml allocation; may
  touch remembered-set and major-marking state; model with the same clobbers as
  today's `caml_modify` noalloc call.
- `caml_darken(Caml_state, old_val, NULL)` or `caml_modify_darken_old(old_val)`:
  no OCaml allocation; may grow/compress
  major-GC mark-stack structures; keep the full mark policy in C.
- `caml_realloc_ref_table(ref)`: no OCaml allocation; may reallocate C-side
  remembered-set storage; after the call, generated code must reload/use the
  updated `ref->ptr`.

Do not introduce a direct plain C ABI call from generated OCaml code unless the
runtime-register, stack-switch, clobber, and live-root contracts are written down
and tested separately.

## Build-Mode Contract

The inline path must make an explicit choice for each mode:

| mode | first implementation |
| --- | --- |
| normal native runtime | enable only after fence/store and helper-call contracts are implemented |
| TSAN native runtime | fall back to `caml_modify` until generated code replicates `__tsan_func_entry`, `__tsan_write8`, and `__tsan_func_exit` semantics |
| helper profiling enabled | fall back to `caml_modify`, or emit the same guarded `caml_llvm_helper_profile_record_modify` call before the barrier |
| DEBUG/runtime assertions | either fall back or document which `CAMLassert` checks remain only in cold C helper paths |

For the profiling mode, fallback is the right first choice: this keeps the
instrumentation used to justify the optimization trustworthy.

## LLVM Lowering

Current source of the hot store shape:

- normal OCaml field assignment reaches `Cfg.Store (memory_chunk,
  addressing_mode, is_modify)`;
- `backend/llvm/llvmize.ml` handles that in `store`;
- when `is_modify` is true on AArch64 word/value stores, the current lowering
  emits `I.fence Acquire` before the generic store.

The first implementation should hook into this store-specific path, not only
look for an arbitrary `Cextcall "caml_modify"` shape. If a separate Cmm/Cfg
shape still reaches a `caml_modify` external call, treat that as a fallback
case until it is reduced and documented.

For any remaining external-call shape, recognize `Cextcall` to `caml_modify`
only as a fallback candidate when:

- `alloc = false`;
- `stack_ofs = 0`;
- two arguments;
- result type is void;
- target is AArch64 64-bit first;
- TSAN is disabled, unless TSAN behavior is replicated.

Generated operations needed:

- cast `fp` to integer for minor-heap range checks;
- load old field value before the store. Use volatile/atomic/other ordering
  only after checking what is required to match the volatile `*fp` load in
  `caml_modify`;
- immediate/block tests via low bit and nonzero;
- minor-heap reserved range checks using `caml_minor_heaps_start` and
  `caml_minor_heaps_end`;
- load `caml_gc_phase` and compare against `Phase_sweep_main`. The plan must
  decide the exact LLVM representation before implementation: enum width,
  volatile vs ordinary vs atomic load, and whether optimization is allowed to
  CSE/hoist the load;
- for Candidate 2, compute/load `Caml_state->minor_tables->major_ref.{ptr,limit}`;
- emit cold helper calls;
- emit `fence acquire`;
- emit an atomic release store. If the local LLVM IR layer cannot express this
  yet, add explicit support instead of silently lowering to a plain store.

Open implementation detail:

- `Caml_state` access should reuse existing domain-state/runtime-register
  helpers if possible. Do not invent a second way to address domain state if
  the LLVM backend already has one.
- Release-store support can be split into a prerequisite cleanup if that keeps
  the `caml_modify` PR smaller. What is not acceptable is an inline
  `caml_modify` path that is less precise than either the C runtime helper or
  the existing LLVM `is_modify` store convention.

## Evidence To Collect Before Choosing Candidate 2

The current counts are typecore-shaped and not joint enough. Before choosing
between Candidates 1, 2, and 3, collect a joint histogram across the same
representative compiler files used in `NUMBERS.md`:

- destination young vs old;
- old value immediate vs young block vs old block;
- new value immediate vs young block vs old block;
- marking active vs inactive;
- remembered-set append actually needed after the old-young early return;
- darken actually needed;
- overlap between remembered-set append and darken;
- ref-table growth count.

This matters because the old profile's `3.5%` remembered-set number is an upper
bound unless it already excludes old-young fields, and Candidate 3 only makes
sense if marking-helper calls remain a measured cost after Candidate 2.

## Tests

### Codegen Expect Tests

Add `testsuite/tests/llvm-codegen/caml_modify.ml`.

Cases:

- field update lowering calls no `c_call_wrapper.caml_modify` on the hot path;
- IR contains `fence acquire` and release/atomic store shape;
- IR/assembly has cold side exit for old-to-young remembered-set work;
- old field already contains a young block, then new value is young: generated
  code skips remembered-set append;
- if Candidate 2 is implemented, assembly includes inline `ref->ptr` append
  and cold `caml_realloc_ref_table` path;
- after the `caml_realloc_ref_table` call, code uses the updated `ref->ptr`;
- marking path remains a cold helper call.
- unsupported target, TSAN, helper-profiling, and unsupported extcall/store
  shapes fall back to the current helper path.

Review promoted output critically. It should lock down the semantic shape, not
hundreds of incidental temporaries.

### Runtime Correctness Tests

Add focused tests for:

- old object updated to point to young object, then minor collection; young
  object must survive;
- repeated old-to-young writes enough to grow the ref table;
- mutation while major marking is active, if a deterministic hook exists;
- fallback to current `caml_modify` under TSAN/debug modes if inline lowering is
  disabled there.

Also run existing GC and multicore memory-model tests that cover write barriers.

### Benchmark Tests

Small benchmarks:

- old object to immediate;
- old object to old object outside marking;
- old object to young object with no table growth;
- old object to young object with forced table growth;
- young object writes.

Compiler binary benchmark:

```sh
python3 _compiler_binary_perf_current/bench_compiler_binary.py
```

Compare against `NUMBERS.md` current baseline:

```text
LLVM-built/native-built geomean after string compare lowering: 1.0766
```

## Experiment Matrix

Run these before choosing the final PR shape.

Prerequisite experiments:

1. Lower one representative assignment and one explicit `caml_modify` external
   call to confirm the exact Cfg shapes that must be optimized or left alone.
2. Add a tiny IR/codegen experiment for `store atomic release`, or decide to
   preserve the existing `is_modify` acquire-fence-plus-store convention and
   file release-store IR support as a separate prerequisite.
3. Add a runtime-layout experiment that proves `major_ref.ptr` and
   `major_ref.limit` offsets come from a single checked source.
4. Collect the joint helper histogram described above across the representative
   compiler files.

| experiment | remembered-set | marking | cold info | purpose |
| --- | --- | --- | --- | --- |
| A | helper | helper | none | correctness baseline |
| B | helper | helper | `llvm.expect` + cold helper | measure cold-layout value |
| C | inline `Ref_table_add` fast path | helper | `llvm.expect` + cold growth | expected best first win |
| D | inline `Ref_table_add` fast path | no extra prefilter | branch weights if needed | test whether branch-weight metadata beats `llvm.expect` |

Stop after C if compiler-binary benchmark improves enough. Treat darken
prefiltering as a separate later design, not experiment D.

## Safety Checklist

Do not land until these are answered.

- Does every path execute the acquire fence plus release store?
- Does generated youngness testing exactly match `Is_young`: address inside
  reserved minor heaps, not just the current domain's active minor heap?
- Is `old_val` read before the store?
- Does an old field value that is a young block skip both marking and
  remembered-set append work?
- Does remembered-set append store the exact `fp` address?
- Does table growth preserve `ref->ptr` semantics after reallocation?
- Is `caml_gc_phase` read safely enough for the same semantics as
  `caml_marking_started()`?
- Is `caml_darken` called only when the old value is a block and not young?
- Is `Infix_tag` left to the C helper unless the generated prefilter handles it
  exactly?
- Is `Cont_tag` left to the C helper?
- Is TSAN disabled or replicated?
- Are debug assertions either preserved in C helper paths or explicitly
  accepted as not present in generated code?
- Are slow helper calls modeled as runtime calls with correct clobbers?

## Human Review Notes

Five `gpt-5.5` high-reasoning review agents checked the draft. Accepted
critiques:

- This must be a follow-up PR unless the agent goal is retargeted. The current
  fast-path-roots PR should not silently grow a write-barrier optimization.
- Candidate 1 and Candidate 2 must preserve the `old_val`-young early return.
  This is now part of the pseudo-code, codegen tests, and safety checklist.
- Candidate 1's helper contract should be exact and boring. The helper should
  share the runtime barrier logic rather than rely on a new ad hoc prefilter
  ABI.
- Candidate 2 needs a real runtime-layout contract before inlining
  `major_ref.ptr`/`major_ref.limit`.
- The plan needs an explicit helper calling convention. Slow helper calls
  should use the existing noalloc wrapper path or an equivalent documented
  stack-switch sequence at first.
- Store ordering cannot be handwaved. The plan now calls out the existing LLVM
  `is_modify` convention and the missing atomic-store ordering field.
- TSAN, helper profiling, and debug modes need explicit fallback or replicated
  behavior. The first plan is fallback for TSAN and helper profiling.
- The evidence needs a joint histogram across representative compiler files,
  not only `typecore.ml` marginal counts.
- Darken prefiltering is too speculative for the first implementation; revisit
  only if measurements show it matters.

Rejected or deferred critiques:

- Do not fully inline `caml_darken` or duplicate mark-stack policy in
  `llvmize.ml`; the benefit is unproven and the runtime-policy split is too
  risky.
- Do not require Candidate 2 to introduce a new `caml_modify_darken_old`
  helper if the existing `caml_darken(Caml_state, old, NULL)` call can be made
  cleanly. A wrapper is allowed only as an ABI simplification.
- Do not make branch-weight metadata the first dependency. Start with
  `llvm.expect`; add branch weights only if layout remains poor.

## First Milestone

Prototype Candidate 1 after the prerequisite shape/store/helper-mode questions
are settled, then prototype Candidate 2 only after the runtime-layout source is
available.

Success criteria:

- Focused correctness tests pass.
- Codegen expect test shows no `c_call_wrapper.caml_modify` on the hot path.
- TSAN/helper-profiling unsupported modes fall back to current `caml_modify`.
- Candidate 2 assembly shows inline remembered-set append fast path and cold
  table-growth call.
- Compiler-binary benchmark improves over the current `1.0766` geomean ratio in
  `NUMBERS.md`.
