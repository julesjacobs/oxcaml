# oxsmt — agent constitution

Every session loads this file. Keep it compact: growth is a defect (DESIGN.md §11).

## Goal

A sound, fast, pure-OCaml SMT solver (stdlib-only, no external deps) that
discharges quantifier-free EUF+LIA verification conditions for the OxCaml
refinement type system, developed entirely by agents with an external Lean-4
correctness gate holding final authority on soundness. Everything below is a
means to that end; when a rule conflicts with the goal, amend the rule via an
ADR rather than comply. Read DESIGN.md for the full plan (start with §1, §3, §11).

## Toolchain (this environment)

- OCaml 5.4.0 / dune 3.20.2 / ocamlformat 0.29.0 / menhir at
  `/usr/local/home/jujacobs/.opam/5.4.0/bin`. The bare `dune` on PATH is a Jane
  Street dispatch wrapper that FAILS outside jane workspaces — always use the
  full opam path, or `export PATH=/usr/local/home/jujacobs/.opam/5.4.0/bin:$PATH`
  first. The Makefile already pins it.
- Lean 4.31.0 oracle: `~/.dispatch/bin/lean` (nix via `dispatch add-nix`;
  `by grind` verified working). Alternative: `sysdep-run -attr-path lean4 -- lean`.
- OxCaml compiler (`5.4.0+ox`, flambda2): present in the local nix store, resolved
  with no network via the boot sysdep `oxcaml.r5`
  (`sysdep-run -source boot -attr-path oxcaml.r5 -print-bin-dir`). `make build-oxcaml`
  uses it to compile the shipped `smt/` libraries (keeps "pure OxCaml" true by test).
  It is NOT dispatch-registered for `-source latest` (no `oxcaml.v1.sexp`), so
  `sysdep-run -source latest -attr-path oxcaml` / `dispatch add-nix oxcaml` 404 — use
  `-source boot`. Caveat: the nix OxCaml runtime targets a newer glibc than this el8
  box, so linking a native `.exe` fails (`__isoc23_strtol`/`dlopen` undefined refs);
  `build-oxcaml` therefore compiles libraries + type-checks only, never links exes.
- No Z3/cvc5. No network in the dev loop.
- Siblings of `main/`, never in git: `../cache` (Lean oracle cache),
  `../corpora` (benchmark sets), `../logs` (full tool output), `../worktrees`
  (child task worktrees).

## Hard rules

- **Dependency firewall.** Everything under `smt/` is stdlib-only: no opam
  packages, no Base/Core, no ppx, no `compiler-libs`, no `Unix`. This is what
  keeps eventual compiler integration cheap (DESIGN.md §1). Test code under
  `tests/` is exempt but still avoids gratuitous deps.
- **The SMT-LIB parser is test-only** (`smt/smtlib`): it ingests benchmarks and
  round-trips our dumps; it is never linked into the compiler. The printer ships.
- **The gate is master-only.** Oracle code, the Lean encoder, test corpora,
  frozen interfaces, and CI config are off-limits to child agents. A child may
  not edit tests/oracles in the same change as code; a test you believe is
  wrong becomes an issue for master adjudication, not an edit.
- **Escalation over silence.** If a map (ARCHITECTURE/INVARIANTS/SPINE) mismatches
  reality, or a spec conflicts with the goal, or an interface needs to change —
  report it; do not silently adapt.
- **Digest-first tool output.** Every tool writes full detail to `../logs` and
  prints a digest: counts, top-k outliers, first few failures with log paths.
  Flooding an agent's context is a defect of the same severity as a slow suite.
- **Determinism.** Fixed seeds; no wall-clock in search heuristics or goldens.

## Merge requirements

Every merge to `oxsmt` (rebase → test → ff-only, DESIGN §11) requires: green
suites + a recorded same-model soundness review + a recorded simplicity review
(`logs/simplicity-review.md`) + — for any diff touching a **TCB path** — a
recorded cross-model (codex) review (ADR-0007). TCB paths: `tests/gate/**`,
`smt/smtlib/printer*`, `tools/check_frozen*` / `FROZEN.sha256` mechanics, and any
canonicalization / cache-key code. "No findings" is a valid recorded verdict; a
content-filter refusal that exits 0 with zero findings is not — validate per
`logs/codex-review-runbook.md`.

Integrator merges only on an explicit master "APPROVED FOR MERGE: task/X at
<sha>" message; board rows, worktrees, and review-round status are tracking, not
authorization.

**Scoped re-review after rebase.** A review of pre-rebase code does not transfer
to semantically-shifted code: if a rebase produces conflicts touching hunks the
reviews examined, or moves ANY TCB-path hunk, the integrator bounces for a scoped
re-verify before landing. Trivial / no-conflict rebases need only the suite re-run.

**Speculative pipelining (queue depth ≥2).** By the ff-only invariant, item N's
post-merge trunk is bit-identical to N's tested rebased head, so while N's suite
runs the integrator may rebase item N+1 onto N's candidate head in a scratch
worktree and pre-run N+1's suite. If N lands, N+1 is already green → immediate ff;
if N fails or is reversed, the speculative run is discarded and redone against real
trunk. Speculation never touches trunk; ff-only pushes stay strictly in order, each
only after its own item's reviews + suite are green. Scratch worktrees are cleaned
like any other (no orphans).

## Acceptance thresholds (milestone-done, author policy)

Milestone-done is defined by `STATUS.md` numbers, two tiers: the VC-shaped corpus
solves ~100% at low-ms (the **user bar**); SMT-LIB solves **≥95%** within the
per-goal timeout with **zero soundness mismatches**, remainder triaged with named
causes (the **insurance bar**, may lag).

## Maps (the master's working set — cite, don't paraphrase)

- `ARCHITECTURE.md` — module DAG, one paragraph each.
- `INVARIANTS.md` — numbered, citable (e.g. "I3").
- `decisions/` — ADRs, append-only; `adr-0000-index.md` is the one-line index.
- `TASKS.md` — the board.
- `STATUS.md` — generated by CI, never edited by hand.
- `reports/` — dated master reports (externalized memory; e.g. milestone summaries).

## Frozen interfaces

Hash-checked in CI; changing one requires an explicit unfreeze + adversarial
review by a fresh agent (DESIGN.md §10).

**Twelve interfaces are frozen** by content hash in `FROZEN.sha256` (repo root):

- M0-core (ADR-0003): `sort.mli`, `symbol.mli`, `term.mli`, `context.mli`, `iarr.mli`
- M1 THEORY freeze (ADR-0005 Tranche A): `env.mli`, `rank.mli`, `theory_view.mli`,
  `atom.mli`, `lit.mli`, `explanation.mli`, `theory.mli` (all under `smt/core/`)

`make check-frozen` (run first inside `make test`, via `tools/check_frozen.sh`)
recomputes their sha256 and diffs the manifest, going **red** on any drift.
`SPINE.md` (regenerate with `make spine`) is the master's concatenated view of the
frozen set.

**Changing a frozen `.mli` requires all of:** (1) an updated `FROZEN.sha256`
(`tools/check_frozen.sh generate`), (2) an unfreeze ADR in `decisions/`, and
(3) an adversarial review by a fresh agent. Otherwise CI (`make check-frozen`)
goes red.

**Remaining ADR-0005 freeze tranches** (schedule in
`decisions/adr-0005-freeze-plan.md`):
- Tranche B (M2, with the EUF adapter): `smt/core/model.mli` — its `value` variant's
  `Uninterp` encoding is pinned by EUF (open q3); `model.mli`/`model.ml` exist now
  but are deliberately **not** frozen until M2.
- Tranche C (M4, with CDCL(T) integration): `smt/solver/sat.mli`, frozen with the
  theory-callback seam (trail-extension notify, theory-literal enqueue w/ lazy
  reason, conflict injection, backtrack notify) + ADR-0006's `trace.on_input`/
  `on_unit` DRAT hooks, in one combined event.

**`CONTRACT-POISON` is an engine obligation (M4):** any exception escaping a THEORY
op bricks that theory instance — the engine discards it and degrades the query to
`unknown` (I8), uniform across theories (EUF needs no mechanism; LIA's `poisoned`
flag is defense-in-depth). It is ADR-level discipline, deliberately **not** baked
into the frozen `theory.mli`.
