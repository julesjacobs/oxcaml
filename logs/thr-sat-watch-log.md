# SAT throughput lever 1 — watch-record cost (thr-sat-watch)

Worktree `worktrees/thr-sat-watch`, branch `task/thr-sat-watch` off trunk `8e056625e6`
(sat.ml/sat.mli byte-identical to my brainstorm read `eac129d9b7`; the two intervening
lands touched dt.ml + parser.ml only). Lever context: `logs/brainstorm-thr-sat-fable.md`.

## Status
- **Commit 1 (mutable-blocker watch reuse) — DONE, measured, gated, review-ready.**
  `f8ab815ca4`. Clears the ≥5% bar counted-identically.
- **Commit 2 (full int-array unboxing) — NOT BUILT: scoped form is not viable; the only
  form that pays off is a full flat clause arena, a major separate lever.** Analysis below.

---

## Commit 1 — mutable-blocker watch reuse (`f8ab815ca4`)

### Mechanism
`type watch = { cl : clause; mutable blocker : lit }` (was immutable). In `propagate`'s
non-fast-path, instead of allocating a fresh `w' = { cl = c; blocker = first }` on every
visit, refresh `w.blocker <- first` and reuse `w` for the keep / relocate / conflict cases.
The census (`OXSMT_THR_CENSUS` instrumented probe) showed this allocation fired on **~40%
of watch visits** (676k of 1.68M on qg5/dead_dnd003).

`old_blocker` is captured before the mutation so the `first <> old_blocker` partner-satisfied
branch condition is byte-for-byte the pre-change condition.

### Counted-identity argument (the acceptance bar)
Watch lists are an internal acceleration structure; no count (verdict / conflicts /
decisions / propagations) depends on whether a kept entry is a fresh record or the reused
one. The reuse is sound because **a watch record lives in exactly one watch list at a time**:
`attach` creates the clause's two watches as two DISTINCT records (one per list), and the
relocate case moves the SAME record to another list (removing it from this one in the same
step). So mutating `w.blocker` is local to the single list being swept; no aliased record
observes a stale/!changed blocker. Watch-list traversal ORDER is preserved exactly (same
in-place compaction, same `add_last` relocation target, same tail-copy on conflict) — so
propagation order, hence decision/conflict order, is identical.

Verified: **byte-identical** verdict + conflicts + decisions + propagations vs the trunk
reference binary on 21 files across QF_UF (search-heavy qg5/qg6/loops6/Goel), QF_LIA,
QF_AX, QF_DT (`/tmp/golden.txt` == `/tmp/variant1.txt`, `diff` clean).

### 2WL / exception-safety / CB invariants
- Two-watched-literal invariants unchanged: slots `lits.(0)/(1)` are still the watched
  pair, the watch still caches the partner as its blocker, relocation still moves the watch
  off `neg false_lit` onto `neg lk`. Only the record's identity/allocation changed.
- Exception-safety (restorable by `cancel_until 0`): the watch lists are not touched by
  `cancel_until`; the change adds no new failure mode and no new allocation to unwind.
- OXSMT_CHRONO (dark): `propagate` is shared by both chrono configs. `chrono-test` green
  (see gates). CB's `cancel_until` scatter-scan and `enqueue_level` read `reason`/`level`,
  not watch records; unaffected.

### Measurement (perf, release, qg5/dead_dnd003, 5-run avg, low variance)
| metric | trunk | commit 1 | reduction |
|---|---|---|---|
| wall (time elapsed) | 0.5796 s | 0.5478 s | **5.5%** |
| cycles | 2.097 B | 1.960 B | **6.5%** |
| task-clock | 575.4 ms | 543.4 ms | 5.6% |
| cache-misses | 18.55 M | 16.98 M | **8.5%** |
| instructions (qg5/003) | 4.202 B | 4.005 B | 4.6% |

Instruction-count reduction across UF heavy files (contention-independent): qg5/dead_dnd003
4.6%, qg5/dead_dnd010 3.8%, loops6/dead_dnd003 3.0%. Cycles/wall beat instructions because
the removed allocation disproportionately saves GC marking + cache pressure (profile:
`caml_shared_try_alloc` self 3.2→2.0%, `do_some_marking` 4.5→3.4%).

### Gates (by exit code)
- `make test` — EXIT 0
- `sat-test` — EXIT 0; `chrono-test` (OXSMT_CHRONO=1) — EXIT 0
- `check-frozen` — EXIT 0 (14/14 interfaces match; sat.mli untouched)
- `wiring-test` — EXIT 0 (211 checks, 0 failures)
- Note: `OXSMT_CHRONO=1 <full sat_test.exe>` exits 2 on BOTH trunk and this branch — the
  `test_branch_filter_firing` case deliberately trips the pre-existing OXSMT_CHRONO-vs-
  branch-filter mutual-exclusion guard (`sat.ml`). Pre-existing, not caused by this change
  (confirmed by running the trunk-base sat_test.exe under CHRONO). The sanctioned chrono
  gate is `chrono-test`, which is green.
- Diff is exactly the intended 19 lines (canonicalized with the repo's ocamlformat 0.29.0;
  the edit-hook formatter diverges from 0.29.0 and injected file-wide comment reindent
  churn, stripped — `dune build @fmt` reports 0 sat.ml diffs).

---

## Commit 2 — full watch-entry unboxing: why the scoped form does NOT pay off

Goal was to remove the `caml_modify` write barrier (6.7% self-time) that fires when a watch
entry is stored during the compacting sweep. Attribution: the barrier is dominated by
**watch-entry pointer stores** (~1.5M `Dynarray.set ws j w` / relocate `add_last` on qg5)
over reason-array stores (~0.5M `Implied_by c`), ~3:1.

The barrier fires because the stored value is a **pointer**. Two candidate representations:

1. **Parallel arrays `clause Dynarray.t` + `int Dynarray.t` (clause POINTER + blocker int).**
   Removes the `watch` record header + the record (alloc already gone in commit 1), and the
   blocker store becomes barrier-free. BUT the clause slot still stores a **pointer**
   (`Dynarray.set cls j c`) — the SAME number of barrier'd pointer stores as commit 1's
   `Dynarray.set ws j w`. Net `caml_modify` removed ≈ 0. Not worth the lockstep-two-array
   complexity in the compaction loop.

2. **Parallel arrays with clause-INDEX int (fully immediate entry).** This is the form that
   would remove the barrier. NOT VIABLE as a drop-in:
   - `reduce_db` does `Dynarray.clear t.learnts; re-add survivors` (`sat.ml:1294`) — it
     COMPACTS the learnt DB, so any stored index into `t.learnts` is invalidated (points at
     a different clause post-compaction → wrong reason clause / OOB → correctness break).
   - A stable side-table (index-by-`c.id`, or an append-only all-clauses array) never
     reclaims deleted learnt slots — `c.id` grows monotonically via `next_id` — reintroducing
     exactly the unbounded memory growth `reduce_db` exists to prevent. A `Hashtbl id→clause`
     restores O(1)-ish decode but adds a hash probe per watch visit (the profile already has
     `Hashtbl.find_opt` at ~5% from the theory side; we would be adding to BCP).

The only representation that removes the watch-store barrier without breaking `reduce_db` or
leaking memory is a **flat clause arena**: clauses identified by an integer OFFSET into one
resizable int array (lits + metadata inline), with a periodic relocation/GC pass that
compacts the arena and rewrites all watch offsets (z3 `sat_allocator.h` + the relocation
loop). That makes `c.lits` / `c.lbd` / `c.activity` / `c.deleted` / `locked` / `analyze` /
the ADR-0013 cert seam (`c.id`) / the CB `cancel_until` paths all arena reads, and adds a
relocation pass under the exception-safety invariant. That is a **major, higher-risk,
separate lever** (multi-day, real soundness surface), not a commit-2 stacked on commit 1.

### Recommendation
Freeze **commit 1** for dual review as the lever-1 deliverable — it is counted-identical,
clears the ≥5% bar on the target UF population, and all gates are green. Scope the flat
clause arena as a distinct lever with its own design round (the `reduce_db` relocation +
arena reclamation is the crux) rather than build a broken clause-index variant or a
zero-value pointer-array variant under this task.
