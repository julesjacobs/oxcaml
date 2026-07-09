# vox proving-latency investigation (constants + scaling + lever matrix)

2026-07-08. Author: perf investigation (agent). Status: analysis + two
low-risk prototypes measured; nothing landed. All commits local on branch
`vox-perf` in `vox-editor2`.

## TL;DR

- **The verdict itself is cheap; the machinery around it is not.** On a
  trivial file, `grind` proving the goal is ~0.05s. The editor's `/check`
  still takes **~1.75s** because it runs the whole proof pipeline **three
  times** (solve, explain-harvest, generated-Lean capture) in a fresh
  scratch dir, and each Lean invocation pays a ~0.35s process startup plus
  a ~0.47s cold `VoxCore.olean` rebuild.
- **Top 3 time sinks (editor `/check`, shares of total):**
  1. Redundant full re-proofs — the explain pass + the generated-Lean
     capture re-run the entire `grind` proof two extra times:
     **46% (tiny) / 56% (fib)**.
  2. `VoxCore.olean` rebuilt per fresh scratch: **~27% (tiny) / ~16% (fib)**.
  3. Lean process startup baked into the above: 3 × 0.35s ≈ 1.05s.
- **Scaling verdict:** cost ≈ `startup_constant + k·#VCs`, linear at
  ~4ms/trivial VC (elaboration is negligible). The two non-linear terms
  are (a) hypothesis-context size per goal — **super-linear**, grind's
  context handling degrades from 4.5ms/fact at 25 facts to 13ms/fact at
  100; (b) genuinely-stuck goals — ~6s each of unbounded search, additive
  per VC. Constants dominate today; those two are the future scaling
  cliffs.
- **Prototyped (low-risk, zero-soundness-risk):** shared `VoxCore.olean`
  (lever A) + lazy explain pass (lever B). Measured on the real editor
  code path: tiny `/check`-solve **1.32s → 0.43s (3.1×)**; end-to-end
  harness **tiny 1.75→0.87s, fib 3.20→1.82s**. Adding lever C (dump
  generated Lean during solve instead of a third re-proof) reaches
  **tiny 0.44s (4.0×), fib 0.92s (3.5×)**.
- **Recommend against (on risk):** a persistent long-lived Lean worker
  that processes successive solves in one environment (state-leakage =
  a catastrophic soundness bug for a verifier). A short-lived
  process-per-solve pool is fine; a shared mutable env is not.

## Measurement discipline

Shared 64-core box, ~5 other agents building. Every headline number is
median of ≥5 repeats; `uptime` load recorded alongside; I prefer relative
breakdowns. Load during the runs below ranged 4–11 (1-min avg); I re-ran
headline numbers in a quiet window (load ~4–7) and they were stable to
±0.02s. Compiler: `_install/bin/ocamlc.opt` (5.4.0+ox, `_install` newer
than `typing/`, spot-compiles green). Lean: pinned
`.../lean4-4.31.0/bin/lean`.

Battery: **tiny** = `clamp.ml` (3 VCs, no proof content); **mid** =
`demo/lean_fib.ml` (reflected `total_ fib` + a `%%vox.lean` lemma block +
~5 VCs, real inductive proofs); **synthetic** modules for scaling.

## Phase 1 — where the time goes (the constants)

### Component measurements (median of 5, warm unless noted)

| component | how measured | time |
|---|---|---|
| Lean binary cold startup | `lean` on `module\n` (empty) | **0.35s** |
| + import VoxCore | `lean` on `module\nimport VoxCore` (LEAN_PATH set) | 0.36s (**+0.01s**) |
| VoxCore.olean build | cold-minus-warm compile delta (see below); isolation ~0.37s | **~0.47s** |
| elaboration + VC-gen | `ocamlc -vox-dry-run -c` (tiny / fib) | **0.01 / 0.02s** |
| solve proper (1 Lean, no explain) | warm compile − dry-run (tiny / fib) | 0.42 / 0.89s |
| — of which grind (tiny) | solve − dry-run − startup − import | **~0.05s** |
| explain pass (#75) | warm solve+explain − warm solve (tiny / fib) | **+0.39 / +0.89s** |
| capture_generated (3rd process) | warm full re-solve (tiny / fib) | ~0.42 / ~0.89s |

Compile-time anchors (tiny `clamp`, median of 5):
`dry-run 0.01` · `warm solve 0.42` · `cold solve (rebuild VoxCore) 0.89`
→ VoxCore build = 0.89 − 0.42 = **0.47s**, grind proper ≈ 0.05s, the rest
is Lean startup.

### The editor `/check` runs the proof up to three times

A full `/check` (from `server.build_check_response`) spawns **3
`ocamlc.opt` processes in one shared scratch dir**:

1. **dump** — `-vox-dump-vc-provenance -vox-dry-run` (no solver for `.ml`):
   VC shapes for the gutter. ~0.01s.
2. **solve** — `-vox-solver-path <lean> -vox-dump-vc-provenance
   -vox-explain-proofs`: the load-bearing verdict. Internally
   (`vox_verify.ml run_lean`) this does `ensure_core` (builds
   `VoxCore.olean` — **cold** in a fresh scratch), then the main `grind`
   Lean run, then **because `-vox-explain-proofs` is on, a SECOND full
   Lean run with `grind?`** purely to harvest the used-lemma /
   unused-hypothesis report.
3. **capture_generated** (`lean_bridge.capture_generated`) — wraps `lean`
   with a shell shim that copies the input then `exec`s lean normally, so
   it **re-runs the entire proof a third time** just to snapshot the
   generated Lean text for the pane (no explain flag here).

Because the 3 processes share the scratch, `VoxCore.olean` is cold only in
process 2 and warm for process 3.

### End-to-end `/check` (3-process simulation, fresh scratch, median of 3)

| file | total `/check` | dump | solve (cold VoxCore + solve + explain) | capture (warm) |
|---|---|---|---|---|
| tiny (clamp) | **~1.75s** | 0.01 | 1.29 (= VoxCore 0.47 + solve 0.42 + explain 0.39) | 0.42 |
| fib (mid) | **~3.20s** | 0.02 | 2.28 (= VoxCore 0.50 + solve 0.89 + explain 0.89) | 0.89 |

**Top-3 sinks, editor path:**
1. Redundant re-proofs (explain + capture): tiny 0.81s (**46%**), fib
   1.78s (**56%**). Only the first grind decides the verdict.
2. VoxCore rebuild: 0.47–0.50s (**~27% tiny / ~16% fib**), paid every
   check because each check gets a fresh scratch.
3. Lean startup: 3 × 0.35s ≈ 1.05s, unavoidable per process but
   multiplied by the redundant passes.

The direct compiler CLI (not the editor) pays only #2 + one startup: a
warm-VoxCore solve is 0.42s (tiny) / 0.89s (fib); AGENTS.md's "~1s honest
module" is exactly this path.

## Phase 2 — scaling

### #VCs (warm solve, single Lean process, median of 5)

| #VCs | dry-run | warm solve |
|---|---|---|
| 10 | 0.02 | 0.45 |
| 50 | 0.02 | 0.61 |
| 150 | 0.04 | 1.02 |

Fit: `warm_solve ≈ 0.41 + 0.0041·#VCs`. **Linear**, ~4ms per trivial VC,
with a ~0.41s fixed floor (Lean startup + import). Elaboration/VC-gen
(dry-run) is essentially flat — **not** a scaling concern. Batching all
VCs as theorems in one Lean file (which vox already does) amortizes
startup across the whole module; splitting per-VC would multiply the
0.35s startup by #VCs (see lever "parallel solve").

### Hypothesis-context size (one goal under K threaded facts, median of 5)

| K facts | warm solve | marginal |
|---|---|---|
| 5 | 0.41 | — |
| 25 | 0.50 | 4.5ms/fact |
| 100 | 1.47 | **13ms/fact** |

**Super-linear.** grind's per-fact cost grows with context size (context
handling looks worse than linear, ~quadratic-ish). A goal buried under a
big threaded fact set is far more expensive than the same goal in a small
context — this is the real algorithmic scaling risk as modules grow
invariant/hypothesis chains.

### Failing goals

- Quickly-refutable false goal (e.g. `result = 1` returning `2`, or a
  false ground postcondition): **~0.79–0.85s**, a counterexample is
  validated fast (DISPROVED).
- Genuinely-stuck quantified goal (grind can neither prove nor quickly
  refute): searches to `maxHeartbeats 400000` — **~6s** per the AGENTS.md
  cost model (heartbeat-bound, not confirmed to the second here because it
  needs a hand-crafted stuck goal).
- **Additive:** each VC is an independent theorem grind attacks in the
  batched file, so a module under active development with several stuck
  goals costs ~6s × (stuck goals). This is the worst scaling case and the
  strongest argument for a heartbeat budget / fail-fast ordering (below).

### Does `public import VoxCore` scale with artifact size?

No. `VoxCore` is compiler-owned and **fixed-size** (VoxU + iarray theory +
tuple products) — the stdlib campaign does not inflate it. Importing it
costs ~10ms (measured: empty 0.35s vs import-VoxCore 0.36s). Client
verification also imports each dependency's `VoxSig_<M>.olean`; those are
individually small and import cost is ~10ms each, sub-dominant to the
0.35s startup. Import cost is roughly linear in #imported-sigs at ~10ms
each — only a concern for a client transitively importing dozens of sigs,
still <<< one Lean startup.

## Phase 3 — lever matrix

Each lever: expected win (from the measurements above), sketch, risks
(soundness first), verdict.

### A. Shared precompiled VoxCore.olean — **DO (prototyped)**

- **Win:** removes the ~0.47–0.50s cold rebuild from every check. ~27%
  (tiny) / 16% (fib) of `/check`.
- **Sketch:** build `VoxCore.olean` once per `(ocamlc, lean)` in a cache
  dir; copy the artifact (`.olean`/`.private`/`.server`/`.src`) into each
  scratch before compiling. The compiler's own `ensure_core` already
  hash-checks a `VoxCore.olean.src` digest sidecar against its base-theory
  text; a staged-fresh artifact makes that check pass and skips the
  rebuild.
- **Risks + mitigations:**
  - *Staleness* — if the compiler's base theory changes, a staged artifact
    is stale. **Self-correcting:** the `.src` digest won't match, so the
    compiler rebuilds it in the private scratch exactly as today. A wrong
    artifact is never trusted.
  - *Lean-version skew* — `.olean` is a lean-version-specific format; a
    stale artifact would make `import VoxCore` **fail loudly** (a hard
    error, never a false "proved"). Mitigate by keying the cache dir on
    the lean path (done in the prototype).
  - *Write race* — the reason BUILD.md mandates private scratches is a
    shared-dir `VoxCore.olean` **write** race. This lever writes the shared
    artifact **once** (under a lock) and only ever **reads/copies** it into
    private scratches, so it dodges the race rather than reintroducing it.
- **Verdict: adopt.** Pure constant kill, zero soundness risk, no UX
  change.

### B. Lazy / skip the explain pass — **DO (prototyped)**

- **Win:** removes one full Lean re-proof: −0.39s (tiny) / −0.89s (fib).
- **Sketch:** the explain pass (`grind?` harvest of used-lemmas /
  unused-hypotheses) is a SECOND Lean invocation triggered by
  `-vox-explain-proofs`. It **never decides the verdict** (grind already
  did; `grind?` is explicitly unsound as a verifier). Skip it on the fast
  `/check`; run it lazily only when the pane is focused (or cache it).
- **Risks:** none to soundness — the verdict already stands. Only cost:
  the pane's used/unused-hypothesis fade is absent until the lazy pass
  runs; the client already treats those fields as optional (`None`).
- **Verdict: adopt**, as an opt-in the pane requests on focus.

### C. Dump generated Lean during solve (kill capture_generated re-solve) — **DO (recommend; prototyped as harness)**

- **Win:** removes the third full re-proof: −0.42s (tiny) / −0.89s (fib).
- **Sketch:** `capture_generated` re-runs the whole proof solely to grab
  the Lean text `run_lean` already writes to its temp file. Add a compiler
  flag (e.g. `-vox-dump-lean <file>`) that writes that text during the
  solve pass; the editor reads the file instead of spawning a third
  process. More invasive than A/B (a small compiler change) but the same
  zero-risk category (emitting text, not deciding verdicts).
- **Verdict: adopt after A/B**; it is the other half of the "stop
  re-proving three times" win.

### D. Proof/solve result caching — **DO (conditional), key must be complete**

- **Win:** a warm re-check of an unchanged file → ~0. Big for
  save-without-edit and revisiting files.
- **Sketch:** key = hash of the **exact generated Lean solver input**
  (which already inlines/imports everything: VoxCore text, all sig-module
  contents, the VC theorems, flags) + lean binary identity + maxHeartbeats.
  Value = per-VC verdict table. On a hit, skip Lean entirely.
- **Risk — key completeness (a miss = an unsound cached "proved"):** the
  honest safe design keys on the **byte-identical generated solver input**
  rather than on the OCaml source. If the generated Lean bytes match and
  the lean binary + flags match, the proof is deterministic and the cached
  verdict is exactly what re-running would produce — arguably zero risk.
  Everything that could change the result is already *in* those bytes
  (that is the whole point of the module-mode self-contained input) **plus**
  the lean binary hash and heartbeat budget, which must be folded into the
  key explicitly. The trap is keying on anything less complete (OCaml
  source hash, mtimes): a compiler change that alters codegen without a
  source change would serve a stale verdict. **Verdict: adopt with
  generated-bytes + lean-hash + flags keying only.**

### E. Keeping Lean alive (persistent worker) — **RECOMMEND AGAINST (as a shared-env worker)**

- **Win:** would remove the 0.35s startup per invocation.
- **Risk — state leakage across solves is a catastrophic soundness bug.**
  A verifier must guarantee that declarations/axioms/environment from file
  N are invisible to file N+1; otherwise a stray axiom or `def` from one
  check silently discharges a later goal (verifies falsehood). Lean's
  server/worker model isolates *documents*, but a naive "feed successive
  files to one lean process" reuses the elaboration environment and does
  **not** give that guarantee for free. The saving (0.35s) is exactly the
  cost of the isolation (fresh process = fresh env).
- **Mitigation that keeps the win without the risk:** a **pool of
  short-lived processes**, one solve per process, recycled — you amortize
  fork/exec but never share an environment across solves. Or a worker that
  hard-resets its environment between solves and is audited for it. Given
  a verifier's stakes, the shared-mutable-env variant is **not worth the
  risk**; the process-pool variant is acceptable but its win (~0.35s) is
  smaller than A+B+C and it carries ongoing "prove the isolation" burden.
- **Verdict: against** the shared-env worker; **defer** the process-pool
  variant until A+B+C+D are in and startup is the remaining bottleneck.

### F. Per-VC incremental re-solve — **DO (conditional), narrower than it looks**

- **Win:** on edit, only re-solve VCs whose theorem text changed.
- **Risk — inter-VC coupling via the shared prelude/blocks.** VCs are
  independent *theorems*, but they all sit over the same prelude (VoxCore,
  imported sigs, `%%vox.lean` blocks, reflected defs). Editing a block or
  a reflected definition can change every VC's meaning; editing one
  function body changes only its own VCs. **Sound reuse rule:** reuse a
  VC's cached verdict only when (a) the shared prelude is byte-identical
  and (b) that VC's own generated theorem text is byte-identical. This is
  really lever D applied per-theorem rather than per-file. **Verdict:
  adopt as the per-theorem refinement of D.**

### G. Parallel solve (split VCs across N processes) — **MARGINAL**

- **Win:** wall-time ≈ max instead of sum of per-VC grind — helps big
  modules with expensive goals.
- **Cost/risk:** each process re-pays the 0.35s startup + prelude import,
  and (pre-lever-A) the VoxCore rebuild + the VoxCore **write race** if
  they share a dir. With lever A (shared read-only VoxCore) the race is
  gone and prelude import is cheap, but you still multiply the 0.35s
  startup by N. Net win only when per-file grind time >> N × 0.35s — i.e.
  large modules with heavy goals, not the common small file.
- **Verdict: defer**; revisit for the stdlib's heaviest modules after A/C
  (which make the per-process fixed cost small).

### H. Cheap config knobs — **DO**

- **maxHeartbeats budget flag:** expose the 400000 budget so the editor
  can cap stuck-goal search (e.g. 50000 for interactive checks, full
  budget for CI). Directly bounds the ~6s worst case. Soundness-neutral
  (a lower budget only turns some "proved" into "unknown/timeout", never
  the reverse). **Adopt.**
- **Fail-fast / cheap-VC-first ordering:** solve ground/cheap VCs first so
  the pane gets partial verdicts fast; expensive quantified goals last.
  Presentation-only, zero risk. **Adopt.**

## Phase 4 — prototypes (levers A + B), measured

Implemented A + B as real edits on branch `vox-perf` (editor Python only —
no compiler rebuild needed), marked `PROTOTYPE`:

- `tools/vox-editor/workspace.py`: `_shared_voxcore_dir` / `stage_voxcore`
  build VoxCore once per `(ocamlc, lean)` and copy it into each scratch;
  called from `stage_for_check`.
- `tools/vox-editor/vc_index.py`: `solve_capture` / `build_index` take
  `explain: bool` (default `True`); `explain=False` runs the
  provenance-only solve (verdicts, no second `grind?` pass).
- `tools/vox-editor/server.py`: stages VoxCore for standalone buffers too;
  `/check` reads an optional `explain` request field (default `True`, so
  behaviour is unchanged unless the client opts in).

### Before/after (median of 5, same session)

**End-to-end 3-process `/check` harness (fresh scratch):**

| file | BEFORE | A+B (shared VoxCore + lazy explain) | A+B+C (also fold capture) |
|---|---|---|---|
| tiny (clamp) | 1.75s | **0.87s (2.0×)** | **0.44s (4.0×)** |
| fib (mid) | 3.20s | **1.82s (1.76×)** | **0.92s (3.5×)** |

**Real editor code path (`vc_index.build_index`, dump+solve; excludes
capture_generated), median of 4, verdicts identical `ok=True` / all VCs
`proved`:**

| variant | tiny |
|---|---|
| BEFORE (no stage, explain) | 1.32s |
| A only (stage VoxCore) | 0.83s (1.6×) |
| A+B (stage + no explain) | 0.43s (3.1×) |

Correctness preserved: the prototyped path returns the same verdicts, and
the editor unit suites pass (`test_workspace` 17/17 OK; `test_vc_index`,
`test_server` run with the built compiler + pinned lean).

### Recommended landing order

1. **A** (shared VoxCore) — transparent, always-on, biggest constant kill,
   zero UX change.
2. **B** (lazy explain) — pane requests explain on focus.
3. **C** (`-vox-dump-lean` to kill the capture re-solve) — small compiler
   change, completes the "stop re-proving 3×" win.
4. **D/F** (byte-keyed verdict cache, per-theorem) — for warm re-checks.
5. **H** (heartbeat budget + cheap-first ordering) — bounds the stuck-goal
   worst case.
6. **Against:** shared-env persistent Lean worker (soundness). Defer the
   process-pool variant and parallel solve until 1–5 land.

## Appendix — reproduction

- Compiler: `_install/bin/ocamlc.opt`; Lean: pinned 4.31.0.
- Direct solve: `cd <dir-with-VoxCore.olean> && ocamlc.opt
  -vox-solver-path <lean> -c file.ml`.
- Component knobs: `-vox-dry-run` (elaboration only),
  `-vox-dump-vc-provenance`, `-vox-explain-proofs` (the second-pass
  harvest). VoxCore build cost = cold-minus-warm compile delta (remove
  `VoxCore.olean*` between runs).
- Editor path: `server.build_check_response` → `vc_index.build_index`
  (dump + solve) + `lean_bridge.capture_generated` (third process).
- Prototype timing script: direct `vc_index.build_index` with/without
  `workspace.stage_voxcore` and `explain=False`.
