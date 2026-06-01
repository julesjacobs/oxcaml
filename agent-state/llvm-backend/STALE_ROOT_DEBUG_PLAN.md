# Stale-root debug plan

## Goal

Make missed frame-table roots easier to reproduce and easier to diagnose.

The suspected failure mode is:

1. An OCaml value in a stack slot or saved register is live across a moving
   collection.
2. The frame table does not report that storage location.
3. Compaction moves the object and updates all reported roots.
4. The missed storage location still contains the old object address.
5. Later, that stale value is used directly or flows into a heap field, global
   root, stack root, or register root that the runtime does scan.

The debug path should help with two separate goals:

- make failures less intermittent by making moving collections and stale old
  addresses more hostile;
- when a stale value is found later, identify the collection that made it stale
  and the raw stack/register slots that still held it immediately after that
  collection.

It should also be possible to turn the diagnostic off with virtually no
performance loss and without recompilation. The normal runtime should not pay
for stack raw-scanning, range lookups, quarantine, or logging. When the debug
tweaks are all zero, the only acceptable overhead is a small number of
predictable checks on already-slow GC/compaction paths.

This should be implemented in the runtime. Do not add compiler-emitted calls at
extra safepoints for this diagnostic: those calls would perturb LLVM liveness,
RS4GC behavior, register allocation, and spilling.

## Existing knobs to use first

Start reproductions with existing runtime options:

```sh
OCAMLRUNPARAM='s=32k,o=1,O=1,V=1,Xcompaction=52,Xcompact_unmap=1'
```

Relevant implementation points:

- `runtime/startup_aux.c` parses `OCAMLRUNPARAM`.
- `runtime/gc_ctrl.c` registers GC tweaks in `gc_tweaks`.
- `Xcompaction=52` selects the old compaction algorithm in
  `runtime/shared_heap.c`.
- `Xcompact_unmap=1` is honored by the old compaction algorithm: evacuated
  pools are unmapped after `compact_fix`.
- `V=1` enables heap verification in `runtime/major_gc.c` and
  `runtime/shared_heap.c`.

These options increase collection pressure and make stale old-space pointers
more likely to crash. They do not explain where a missed root came from, so they
are a baseline, not the final diagnostic.

## Proposed runtime debug knobs

Add opt-in GC tweaks, not compiler flags:

```text
Xdebug_compact_every_major=<0|1>
Xdebug_stale_roots=<0|1>
Xdebug_stale_roots_abort=<0|1>
Xdebug_stale_roots_max_candidates=<N>
Xdebug_stale_roots_target_epoch=<N>
Xdebug_stale_roots_target_addr=<addr>
```

`Xdebug_compact_every_major=1` makes failures less intermittent by forcing a
moving compaction at existing major-cycle boundaries. This should hook into the
major-GC compaction decision near `should_compact_from_stw_single` in
`runtime/major_gc.c`.

`Xdebug_stale_roots=1` enables the diagnostics described below.

`Xdebug_stale_roots_abort=1` turns a diagnostic into a fatal error. Without it,
the runtime should print candidates and continue when that is safe.

`Xdebug_stale_roots_max_candidates` bounds raw-scan output per compaction. The
default should be small.

`Xdebug_stale_roots_target_epoch` and `Xdebug_stale_roots_target_addr` are
optional filters for the second run. A good workflow is to first let the later
visible-stale check identify the evacuated epoch and old address, then rerun
with those filters to print only the relevant raw candidates from that epoch.

All debug state should be guarded by these tweak values. In particular, do not
record ranges or scan raw stack/register storage unless `Xdebug_stale_roots` is
nonzero.

The off state should require no recompilation:

```sh
OCAMLRUNPARAM='Xdebug_stale_roots=0,Xdebug_compact_every_major=0'
```

This means the implementation should prefer runtime branches over `#ifdef
DEBUG` for the main feature. `#ifdef DEBUG` can add extra assertions, but the
diagnostic itself should be available in an ordinary runtime build and disabled
by default.

## Part 1: record evacuated ranges

During compaction, record every evacuated source pool range in two logically
separate registries:

- current in-flight ranges for the compaction that is currently running;
- completed stale ranges from earlier compactions.

The distinction is essential. During `compact_fix`, roots and heap fields are
expected to point into the current in-flight evacuated ranges; that is how the
compactor finds values to update. Those current pointers are not stale. They
become suspicious only after normal `compact_fix` has finished.

Each range record should contain:

```text
debug compaction epoch
pool base address
first block address
pool end address
size class
domain id / heap owner if available
```

The collection epoch should be the compaction count that this compaction will
become. `caml_compactions_count` is incremented after the compaction driver
returns, so during the driver use:

```text
debug_epoch = atomic_load(&caml_compactions_count) + 1
```

This registry must cover all participating domains, not just the current
domain's local evacuation list. A stale value on one domain's stack can point
into a pool evacuated by another domain. If the first implementation cannot
cleanly build a cross-domain registry, it should say so explicitly in the
diagnostic and restrict itself to single-domain reproductions.

Because compaction is stop-the-world but runs with multiple participating
domains, range recording should either:

- append to a synchronized global debug buffer; or
- record into per-domain buffers and merge them at an existing compaction
  barrier before raw scanning.

Do not let each domain raw-scan against only its own evacuation list.

Implementation locations:

- old algorithm: `runtime/shared_heap.c`, around the `evacuated_pools` list
  that is passed through `compact_fix` and then released;
- new algorithm: `runtime/shared_heap.c`, around `domain_evac_pools` in
  `compact_run_phase`.

The registry should be bounded. A ring buffer is enough for the initial version.
The record should survive after compaction so a later scanned stale value can be
attributed to the compaction that evacuated that address range.

If the ring overwrites old entries, diagnostics must say so. Do not silently
report "not stale" merely because the responsible compaction aged out of the
ring. Keep counters for:

- total ranges recorded;
- total ranges overwritten;
- oldest retained epoch;
- newest retained epoch.

When a stale-looking address is not found, include the retained epoch window if
debug logging is enabled.

Lifecycle:

1. While evacuating objects, add source ranges to the current in-flight table.
2. During `compact_fix`, pre-header stale checks must ignore the current
   in-flight table and only check completed stale ranges from previous
   compactions.
3. After every participating domain has completed `compact_fix`, raw-scan
   stack/register storage against the current in-flight table. Hits here are
   post-fix stale candidates for this compaction.
4. After raw scanning and before release/unmap/quarantine, move the current
   in-flight table into the completed stale-range ring.
5. Future GCs use the completed stale-range ring for visible-stale checks.

Barrier placement:

```text
evacuate objects and fill current in-flight ranges
barrier
compact_fix(...)
barrier
raw scan current in-flight ranges for missed slots
barrier
promote current ranges to completed stale-range ring
release / unmap / quarantine evacuated pools
```

The second barrier is already present in both compaction algorithms after
`compact_fix`. The debug path needs an additional barrier after raw scanning so
no domain releases or poisons evacuated pools while another domain is still
reading forwarding pointers for diagnostics.

The `pool` type is private to `runtime/shared_heap.c`, but the pre-header
helper must be callable from `runtime/fiber.c`, `runtime/major_gc.c`, and
`runtime/shared_heap.c`. Put only raw debug metadata in a small internal API,
for example in `runtime/caml/shared_heap.h`:

```text
struct caml_debug_evacuated_range {
  uintnat epoch;
  char *pool_base;
  char *first_block;
  char *pool_end;
  sizeclass_t size_class;
};
```

The helper should not expose or require the private `pool` definition outside
`shared_heap.c`.

The completed stale-range ring is read from GC paths that may run outside the
exact compaction release section, including major marking and write-barrier
darkening. Do not put a mutex acquisition in the pre-header helper. Use a
read-mostly structure:

- fixed-capacity array;
- write entries during stop-the-world compaction or under the existing pool
  allocator lock when invalidating reused ranges;
- publish entry count / generation with release semantics;
- read count / generation with acquire semantics in the helper;
- never free the backing array while domains are running.

## Part 2: raw post-fix scan

Immediately after normal `compact_fix`, and before evacuated pools are released
or unmapped, raw-scan stack/register storage for old addresses that normal root
scanning failed to update.

Normal root scanning enters through:

- `runtime/shared_heap.c:compact_fix`;
- `runtime/roots.c:caml_do_roots`;
- `runtime/fiber.c:caml_scan_stack`;
- `runtime/fiber.c:scan_stack_frames`.

`scan_stack_frames` only visits offsets listed in the frame descriptor. The raw
scan should instead inspect physical storage.

Scan:

- active OCaml stack words from `stack->sp` to `Stack_high(stack)`;
- `Caml_state->gc_regs`, if non-null;
- saved `gc_regs` buckets at OCaml stack chunk boundaries;
- stack handler fields that are already normal roots;
- later, continuation stacks if needed.

For each raw word:

1. Treat it only as a candidate value.
2. If it has OCaml block shape, check whether it falls inside any evacuated
   range recorded for the current compaction.
3. If it does, report a stale candidate.

Report:

```text
stale candidate after compaction #N
  value: old address
  evacuated range: [lo, hi)
  offset in range
  storage kind: current stack / saved gc_regs / current gc_regs / handler
  storage address or register index
  nearest frame return address if available
  frame descriptor address if available
```

If the value points at the start of an evacuated object and the object still
has a forwarding pointer in field 0, also report:

```text
  old object base
  new object base from forwarding pointer
```

If the value is interior to an evacuated object, report it as an interior stale
candidate. This is especially useful for validating that derived `addrspace(1)`
values were rematerialized from relocated bases and were not emitted as direct
frame-table roots.

For small-pool objects, the evacuated range record includes the size class, so
the debug code can compute the candidate object's old header address without a
heap walk:

```text
first = range.first_block
whsize = whsize_sizeclass[size_class]
object_index = ((header_t *)candidate_address - first) / whsize
old_header = first + object_index * whsize
offset_in_object = (header_t *)candidate_address - old_header
```

Only read `old_header` and `Field(Val_hp(old_header), 0)` while the evacuated
pool is still mapped and before optional poisoning.

This raw scan deliberately may produce false positives from dead stack slots.
Those should be called `candidate`, not `missed root`.

## Part 3: later visible stale-value check

Add a stronger check before any code reads `Hd_val(v)` or `Tag_val(v)` from a
candidate root/field value.

If `v` has block shape and points into any recently evacuated range, report:

```text
visible stale value during compaction #M
  value: old address
  evacuated by compaction #N
  current scanned slot: p
  offset in evacuated range
```

This is stronger than the raw post-fix scan. At this point the stale value is
being presented to the GC as a real root or heap field. That means it has
escaped dead-stack noise and become semantically visible again.

This cannot live only in `compact_update_value`. In native-code root scanning,
`runtime/fiber.c:visit` classifies roots before calling the scanning action,
and that classification reads `Hd_val(vblock)`. A stale stack or local-root
value can therefore SIGBUS in `visit` before `compact_update_value` gets a
chance to run.

The visible stale-value check needs hooks at every GC path that reads a header
from a value that came from roots or heap fields:

- in `runtime/fiber.c:visit`, immediately after `Is_block(v)` and before the
  first `Hd_val(vblock)` access;
- in `runtime/shared_heap.c:compact_update_value`, before its first
  `Tag_val(v)` / `Hd_val(v)` access, for heap fields, globals, dynamic roots,
  memprof roots, and other paths that call the scanning action directly;
- in `runtime/major_gc.c:caml_darken`, before `Hd_val(v)`, for global roots
  and other direct major-marking scanning-action calls;
- in `runtime/major_gc.c:mark_slice_darken` and the duplicated marking loop in
  `do_some_marking`, before `Hd_val(child)` / `Hd_val(block)`, for stale values
  found in heap fields during major marking. In the debug marking loop, check
  `child` before `prefetch_block(child)` as well as before the later pop-side
  `Hd_val(block)`;
- in `runtime/shared_heap.c:verify_object`, before `Hd_val(v)` / `Tag_val(v)`,
  because the recommended `V=1` heap verifier first records roots and then
  dereferences them while traversing the reachable graph.

The hook should be a tiny helper, for example:

```text
caml_debug_check_stale_before_header(
  value v,
  volatile value *slot_or_null,
  enum caml_debug_stale_source source)
```

`slot_or_null` is optional. Some paths, such as the major-GC prefetch buffer,
may only have the value by the time they are about to read the header. When a
field address is available, pass it; when it is not, report the source as
`mark queue` / `prefetch buffer` rather than pretending it is a root slot.

When `Xdebug_stale_roots=0`, the helper must return immediately. When it is
enabled, it should do pure address-range membership tests before interpreting
the value as an object.

This avoids a poor failure mode: with `Xcompact_unmap=1`, reading the header of
an old unmapped address may SIGBUS before we can print a useful message. The
range check has to happen before every relevant GC header access, not only
inside the compaction update action.

## Part 4: optional quarantine / poisoning

For the first implementation, prefer range tracking plus `Xcompact_unmap=1`.

If address reuse makes diagnostics noisy, add a debug quarantine:

- after `compact_fix`, do not immediately return evacuated pools to the normal
  free list;
- keep them in a bounded debug quarantine for a few compactions;
- optionally fill payload/header words with a recognizable pattern after the
  forwarding information is no longer needed.

Poisoning must happen after `compact_fix`, not before. During `compact_fix`,
old evacuated objects still need their forwarding pointer in field 0.

If quarantine is not implemented, then completed stale-range records must be
invalidated when the allocator reuses the same virtual address for a new pool or
chunk. Otherwise a valid new object at a reused address could be misreported as
a stale pointer from an older compaction. Pairing the first implementation with
`Xcompact_unmap=1` reduces this risk, but mmap can still theoretically return
the same address later.

## Expected workflow

1. Run a known intermittent failure with:

   ```sh
   OCAMLRUNPARAM='s=32k,o=1,O=1,V=1,Xcompaction=52,Xcompact_unmap=1'
   ```

2. If it still only gives a late SIGBUS, rebuild the runtime with
   `Xdebug_stale_roots`.
3. Rerun with:

   ```sh
   OCAMLRUNPARAM='s=32k,o=1,O=1,V=1,Xcompaction=52,Xcompact_unmap=1,Xdebug_compact_every_major=1,Xdebug_stale_roots=1'
   ```

4. Use the later visible stale-value check to identify the compaction epoch
   that evacuated the stale address.
5. Look at the raw post-fix candidate list from that same epoch.
6. Match the stale address to stack/register storage that still held the old
   value immediately after `compact_fix`.
7. Use the reported frame return address and frame descriptor to inspect the
   emitted frame table and RS4GC lowering for that safepoint.

## Self-review pass 1

Problem: The first draft was too close to "scan the current domain's
evacuated pools." That misses cross-domain cases. The revised plan requires a
per-compaction registry that covers all participating domains, or an explicit
single-domain limitation.

Problem: The first draft treated raw stack findings too strongly. Raw stack
words include dead slots, stale non-values, saved metadata, and values whose
low bit happens to have block shape. The revised plan consistently calls these
`stale candidates`. Only the later `compact_update_value` range hit is a
strong visible-stale diagnostic.

Problem: The first draft did not account for `caml_compactions_count` being
incremented after the compaction driver. The revised plan uses
`atomic_load(&caml_compactions_count) + 1` as the in-flight debug epoch.

Problem: The first draft mixed poisoning with forwarding-pointer use. The
revised plan keeps poisoning optional and explicitly delays it until after
`compact_fix`.

## Self-review pass 2

Remaining risk: Raw stack scanning from `stack->sp` to `Stack_high(stack)` will
find stale values in caller-save spill slots and dead frame slots. That is
acceptable for candidate generation, but the diagnostic should include enough
context to let us correlate with frame-table data:

- the stack address containing the word;
- the nearest frame return address while walking the stack;
- whether the address is within a frame's listed live offsets.

Improvement: while raw-scanning stack frames, reuse the frame-walking logic from
`scan_stack_frames` only for boundaries and metadata. For each frame, scan all
words in that frame, then annotate whether a candidate word's offset appears in
the frame descriptor live-offset table. If it does not, that is exactly the
interesting missed-root shape.

Remaining risk: checking whether an arbitrary stack word is inside an
evacuated range should not dereference the stack word. The range check must be
pure address comparison. Only after range membership is established should the
debug code try to interpret object headers or forwarding fields, and even then
only before evacuated pools are released/unmapped.

Improvement: store range bounds as raw `char *`/`uintnat` addresses and perform
membership tests before any `Hd_val`, `Tag_val`, or `Field` access.

Remaining risk: later visible stale-value checks can false-positive if an old
range is returned to the allocator and reused for a new object. The first
version should either:

- pair `Xdebug_stale_roots` with `Xcompact_unmap=1`; or
- keep a debug quarantine for recorded ranges.

Do not trust long-lived range records without preventing reuse.

## Self-review pass 3

Problem: The plan needs to distinguish two uses of the evacuated-range table.
During the post-`compact_fix` raw scan, the evacuated pools are still mapped and
still contain forwarding pointers. At that point it is valid to compute object
bases and read forwarding fields after range membership is proven. During later
GCs, old ranges may be unmapped or reused, so the later visible-stale check
must treat the range table as address metadata only unless quarantine prevents
reuse.

Problem: The raw scan cannot recover the true current machine register state
unless the runtime already saved it into `Caml_state->gc_regs`. That is fine
for compaction, because the GC path is already using the saved register bucket
for precise roots, but the diagnostic should say `saved gc_regs`, not imply it
is asynchronously reading registers.

Problem: `Xdebug_compact_every_major` should not override an explicit
no-compaction mode unless we intentionally document that it does. The least
surprising behavior is:

- if the major cycle requests `Compaction_forced`, compact;
- if it requests `Compaction_none`, do not compact;
- if it requests `Compaction_auto` and `Xdebug_compact_every_major=1`, compact.

Problem: adding `Xdebug_compact_every_major` before attribution works can make
logs too noisy. The revised implementation order keeps attribution first.

Problem: The first versions did not state the off-performance contract. The
debug path must not add work to ordinary allocation fast paths, LLVM-generated
code, or normal safepoints. It should sit on GC/compaction paths only, behind
runtime tweak checks. `compact_update_value` is already a compaction-only
scanner, so a guarded stale-range check there is acceptable. The expensive raw
scan and range registry allocation must not happen unless `Xdebug_stale_roots`
is set.

Problem: `Xdebug_compact_every_major` changes GC behavior even if
`Xdebug_stale_roots` is off. That is intentional when explicitly enabled, but
it must default to zero and should be documented as a stress knob rather than a
diagnostic-only knob.

## Self-review pass 4

Problem: The previous plan incorrectly assumed that adding the later
visible-stale check to `compact_update_value` was enough. In native-code stack
and local-root scanning, `runtime/fiber.c:visit` reads `Hd_val` before invoking
the scanning action. That means stale stack roots can fault before
`compact_update_value` runs. The plan now requires the pre-header check in both
`visit` and `compact_update_value`.

Problem: The pre-header helper sits on root-scanning paths, not allocation fast
paths, but `visit` is used by ordinary major/minor root scans. To preserve the
off-performance contract, the helper must be a single cheap runtime check when
disabled, and it must not allocate, lock, format strings, or walk the range
table unless `Xdebug_stale_roots` is nonzero.

Problem: The check in `visit` will also run for local and external-looking
values. That is acceptable: range membership is purely address-based, and the
evacuated range table only records old major-heap compaction source ranges.
The helper must not infer that every block-shaped value is a major heap object
until after it has ruled out the stale-range case.

## Self-review pass 5

Problem: The pre-header hooks in `visit` and `compact_update_value` still do
not cover all ways a stale value can become visible. During major marking,
global roots call `caml_darken` directly, and heap fields are later processed by
`mark_slice_darken` or by the duplicated prefetch-buffer loop in
`do_some_marking`. Those paths call `Hd_val` after `Is_markable` without going
through `compact_update_value`. The plan now requires the same pre-header
helper in those major-marking paths.

Problem: `Is_markable` does not read the heap header in this runtime; it only
checks block shape and young-heap membership, plus a debug assertion against
`Debug_free_major`. So the helper should sit after `Is_markable` succeeds and
before the first `Hd_val`, not before every `Is_markable` call. That keeps the
off-state overhead lower and avoids checking obvious immediates.

Problem: The diagnostic should distinguish the storage source:

- root slot from `visit` / `compact_update_value`;
- global/final/memprof root from a direct scanning action;
- heap field from major marking.

For heap fields, reporting the field address is more useful than trying to
force it into "root" terminology.

## Self-review pass 6

Problem: The range record previously said "old pool start", but that is
ambiguous. For object-base reconstruction the debug code needs both the pool
base, for attribution, and the first block address, for size-class indexing.
The record now explicitly stores `pool_base`, `first_block`, and `pool_end`.

Problem: The helper will be used from files that cannot see the private
`struct pool` in `runtime/shared_heap.c`. The revised plan requires a raw
metadata API in an internal header, not passing `pool *` across module
boundaries.

Problem: Object-base reconstruction for interior candidates must operate on
the candidate address as an address inside the pool, not by pretending every
candidate is an object-start value. The revised formula computes the containing
size-class slot and an offset within that slot.

## Self-review pass 7

Problem: The recommended reproduction settings include `V=1`, but heap
verification can itself dereference stale values. `caml_verify_heap_from_stw`
pushes values through `verify_push`, then `verify_object` reads `Hd_val` and
`Tag_val` while traversing. If a stale value reaches verification through a
global root or heap field, it can fault before the major-marking or compaction
hooks. The plan now requires the pre-header helper in `verify_object` too.

Problem: This reinforces the implementation rule: the pre-header helper should
be a general GC debug helper, not something named only for compaction update.
The helper name should mention stale evacuated ranges, not a specific GC phase.

## Self-review pass 8

Problem: Calling the pre-header helper directly inside major marking's hottest
field loop would add a disabled-flag branch per markable heap field. That is
not compatible with the off-performance contract.

For cold or already bounded paths, a local guard is acceptable:

- `runtime/fiber.c:visit`, once per reported root;
- `runtime/shared_heap.c:compact_update_value`, during compaction only;
- `runtime/shared_heap.c:verify_object`, only with `V=1`;
- `runtime/major_gc.c:caml_darken`, once per direct root/action call.

For hot major-marking loops, use outer dispatch instead:

```text
if (caml_debug_stale_roots) {
  run debug marking loop with pre-header checks;
} else {
  run the existing marking loop unchanged;
}
```

Concretely, avoid adding a disabled branch to every iteration of
`do_some_marking`'s heap-field loop. Either factor the duplicated marking code
behind a debug/normal helper selected once per marking slice, or keep a debug
variant of the duplicated code. The normal `Xdebug_stale_roots=0` path should
compile to the existing inner loop plus at most one outer phase/slice branch.

This is a case where a little duplication is acceptable: it preserves the
performance contract and keeps the debug path opt-in without recompilation.

## Self-review pass 9

Problem: A raw post-`compact_fix` stack/register scan can produce many
candidates from dead slots, especially in large compiler processes. Unbounded
logging would make the diagnostic hard to use and could perturb timing enough
to hide the issue.

The plan now includes candidate caps and filters. The first run should use
small bounded output and rely on the later visible-stale diagnostic to identify
`epoch` and `old address`. The second run can set `target_epoch` and
`target_addr` so the raw scan reports only storage locations that held the
specific stale value after the responsible compaction.

Problem: Parsing an address in `OCAMLRUNPARAM` is slightly awkward because
existing `scanmult` is numeric. That is fine: accept decimal or `0x...` through
the GC tweak parser if needed, or initially expose only `target_epoch` and
candidate cap. The address filter is useful but not required for the first
implementation.

## Self-review pass 10

Problem: The previous plan would have reported every legitimate current
compaction root as stale. During `compact_fix`, a pointer into the current
evacuated source pool is exactly the normal case: `compact_update_value` should
read the forwarding pointer and update the slot. It is only stale if it remains
after `compact_fix`, or if it points into a completed range from an earlier
compaction.

The plan now separates current in-flight ranges from completed stale ranges.
The pre-header helper used during normal GC scanning checks only completed
ranges. The raw post-`compact_fix` scan checks current ranges to find candidates
missed by the just-finished frame-table scan. Only after that raw scan should
the current ranges be promoted into the completed stale-range ring.

Problem: Raw scanning reads forwarding information from evacuated pools. The
existing post-`compact_fix` barrier is not enough by itself; without another
barrier after raw scanning, one domain could release, unmap, quarantine, or
poison evacuated pools while another domain is still diagnosing. The plan now
requires a raw-scan barrier before release.

## Self-review pass 11

Problem: Completed stale-range records are address metadata, but virtual
addresses can be reused. If a released pool is later reused for a valid new
object, a stale-range check could produce a false visible-stale report. The
debug mode should choose one of these policies:

- quarantine completed ranges so reuse cannot happen while the record is live;
- force unmap and accept the much lower mmap-same-address risk for the first
  implementation;
- invalidate completed range records whenever the allocator maps or reuses a
  pool whose address overlaps a recorded range.

Do not leave long-lived address records active while the normal pool allocator
is free to reuse the same addresses.

## Self-review pass 12

Problem: The completed range table is not only read during compaction. Hooks in
major marking and `caml_darken` can run from GC/write-barrier paths outside the
specific compaction release section. A lock in the pre-header helper would hurt
performance and could be unsafe in low-level runtime paths. The plan now
requires a fixed read-mostly table with atomic publication and no allocation or
locking in the helper.

Problem: Range invalidation on address reuse must follow the same rule. It can
run under allocator/pool locks or during stop-the-world setup, but readers must
see either the old complete entry or the new invalidated state, never a
partially written range.

## Self-review pass 13

Problem: A bounded ring can age out the responsible compaction before the stale
value becomes visible. That is acceptable for a bounded diagnostic, but silent
loss would be misleading. The plan now requires retained-epoch and overwrite
counters. If attribution is unavailable because history was overwritten, the
runtime should say that rather than implying there was no stale range.

## Self-review pass 14

Problem: The helper signature assumed every visible stale value has a slot
address. Major marking can lose that context after pushing a value through the
prefetch buffer or compressed mark stack. The helper now takes an optional slot
and an explicit source enum so diagnostics can say `heap field`, `mark queue`,
`prefetch buffer`, `root slot`, and so on.

Problem: The major-GC debug variant must check before `prefetch_block(child)`,
not only before the later `Hd_val(block)`. `prefetch_block` computes
`Hp_val(v)` and `Field(v, 3)` addresses for prefetching. Even if the hardware
prefetch is normally non-faulting, the diagnostic should not intentionally touch
an address known to be in a completed evacuated range before reporting it.

## Revised implementation order

1. Add `Xdebug_stale_roots` and `Xdebug_stale_roots_abort` GC tweaks.
2. Add a per-compaction evacuated-range registry with an in-flight epoch.
3. Wire range recording into the old `Xcompaction=52` path first, because
   `Xcompact_unmap=1` already makes that path hostile to stale pointers.
4. Add a synchronized cross-domain range merge before any raw scan. For a
   single-domain first patch, make the limitation explicit in the diagnostic and
   test harness.
5. Add the later visible stale-value pre-header helper, guarded by
   `Xdebug_stale_roots`.
6. Call that helper from `runtime/fiber.c:visit` before `Hd_val`, and from
   `runtime/shared_heap.c:compact_update_value` before `Tag_val` / `Hd_val`.
7. Call that helper from `runtime/shared_heap.c:verify_object` before
   `Hd_val` / `Tag_val`, because the recommended debug workflow uses `V=1`.
8. Call that helper from `runtime/major_gc.c:caml_darken`. For
   `mark_slice_darken` and the duplicated marking loop in `do_some_marking`,
   use an outer debug dispatch or debug variant so the normal marking inner
   loop stays unchanged when `Xdebug_stale_roots=0`; in the debug variant,
   check before `prefetch_block` as well as before header reads.
9. Add raw post-`compact_fix` stack/register candidate scanning for the current
   domain.
10. Annotate stack candidates with frame bounds and whether the slot appears in
   the frame descriptor live-offset table.
11. Extend range recording and raw scanning to the default new compactor.
12. Add `Xdebug_compact_every_major` only after the attribution machinery works.
13. Add quarantine/poisoning only if address reuse makes the first diagnostic
   ambiguous.

This order gets the highest-value attribution first with the least runtime
surface area. It also avoids starting with aggressive forced compaction, which
could create too much output before the diagnostic format is useful.
