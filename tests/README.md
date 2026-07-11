# tests/

Test harness and correctness gate. Lives **outside** `smt/` and consumes the
SMT-LIB2 dumps the printer produces (DESIGN.md §3, §8). Test code is exempt from
the `smt/` stdlib-only firewall (I3) but stays lean.

**Gate paths here are master-only** (AGENTS.md): child agents may not edit the
oracle, the Lean encoder, or the corpora, and may not change tests in the same
change as solver code. A test believed wrong becomes an issue for master
adjudication.

## Layout

- `harness/` — the `.smt2` runner (`make test` / `make promote`). Globs
  `tests/cases/*.smt2` plus its own `tests/harness/fixtures/*.smt2`, runs the
  solver on each (sorted, deterministic), and diffs produced golden text against
  a committed sidecar. See the harness contract below. Full detail lands under
  `../logs/harness/<run>/`; only a digest prints to stdout (context-frugal,
  §11). Exact counters + wall-clock go to the uncommitted stats sidecar.
- `gate/` — the Lean 4 encoder + certification driver + content-addressed cache.
  Unsat queries become Lean theorems discharged `by grind`; sat models become
  ground goals closed by `decide`/`native_decide`. Cache maps canonical query
  hash → verdict + oracle outcome, keyed also by encoding version, Lean
  toolchain, and grind config. Cache lives in `../cache` (never in git).
- `cases/` — `*.smt2` regression cases: assertions + expected verdict, the same
  shape as the public benchmarks. Larger fetched corpora live in `../corpora`
  (never in git).

Status: harness landed (M0-harness). Gate and the `cases/` corpus arrive with
M0-gate / their own tasks; the harness tolerates an empty `cases/`.

## The harness (M0-harness)

### Solver CLI contract

The harness never links the solver — it invokes it as a subprocess and reads
stdout, so the same contract serves the stub today and the real solver later:

```
SOLVER <file.smt2>      # path passed as argv[1]; output on stdout; exit 0
```

The solver prints **exactly one `(result …)` block per `(check-sat)`**, in
order (so push/pop / multi-check-sat files yield one block per check), each:

```
(result
  (verdict sat|unsat|unknown)
  (core-size N)          ; optional — present when a core is available (unsat)
  (model ((x 0) (y 1)))  ; optional — present iff sat; bindings (name value)
  (counters (conflicts N) (decisions N) (propagations N)))
```

`verdict` and `counters` (all three keys) are required; field order is not
significant; extra fields are ignored. A non-zero exit, unparseable output, or a
block count that disagrees with the number of `check-sat`s in the file is a
harness failure. Until the real solver exists, `SOLVER` defaults to the built
`stub_solver` (`_build/default/tests/harness/stub_solver.exe`), which reports
`unknown` with zero counters for every check-sat.

### Golden format

Golden output is a committed sidecar next to each case: `foo.smt2` →
`foo.smt2.expected`. One `(goal N …)` block per check-sat, in order:

```
(goal 1
  (verdict unsat)
  (core-size 3)                                             ; when available
  (model ((x 0) (y 1)))                                     ; when sat, sorted by name
  (counters (conflicts <100) (decisions <10) (propagations <1k)))
```

Counters are **log-scale buckets** — `<10`, `<100`, `<1k`, `<10k`, `>=10k`
(DESIGN.md §8) — so a golden diff appears only on an order-of-magnitude change,
not on every ±1 wobble. Models are canonicalized (bindings sorted by name).
**No wall-clock or other nondeterministic value ever appears in a golden (I5,
I6).**

### Label check (soundness)

If a `.smt2` carries `(set-info :status sat|unsat)` and the solver's verdict for
that check-sat *contradicts* it (declared `unsat`, solver `sat`, or vice-versa),
that is a **failure regardless of whether the golden matches** — a soundness
signal. `unknown` against a definite label is only a completeness gap and does
not fail. Status is tracked per check-sat (the most recent `set-info :status`
in effect).

### Workflow

- `make test` — runs the pure harness self-test (which proves red-detection
  works), then the golden regression over `cases/` + `fixtures/`. Prints a
  digest (`PASS`/`FAIL` counts, first failures with paths to full diffs under
  `../logs/harness/<run>/`). Exits non-zero on any diff, missing golden, label
  mismatch, or solver error.
- `make promote` — accepts current solver output as the new golden, rewriting
  the `.expected` sidecars for missing/mismatched goldens and printing a
  per-file diffstat so the promoting agent sees what it accepts. **Label
  mismatches and solver errors are never masked** — promote refuses them and
  they stay red.
- Override the solver or paths: `make test SOLVER=path/to/real`, or
  `LOGS=`, `STATS=`, `CASES=`, `FIXTURES=`.

### Stats sidecar (uncommitted)

Exact per-goal counters and per-file wall-clock go to one JSONL file per run
under `$(STATS)` (default `../logs/stats/`, i.e.
`/usr/local/home/jujacobs/oxsmt/logs/stats/run-<time>-<pid>.jsonl`) — **never
committed** (I5). One object per goal: `file`, `goal`, `verdict`, exact
`conflicts`/`decisions`/`propagations`, `wall_ms`. This is the feed for nightly
aggregation into `STATUS.md` (aggregation itself is CI/nightly, not the
harness — DESIGN.md §11).

