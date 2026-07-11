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

Status: harness landed (M0-harness) and the gate (M0-gate) is implemented — both
documented below; the harness tolerates an empty `cases/`.

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

## Gate (`tests/gate/`, `make gate`)

The gate certifies each `.smt2` case that carries `(set-info :status ...)` against
the Lean 4 oracle, running the day-one honeypots first. It is a stdlib+Unix OCaml
executable (`tests/gate/gate.exe`), deliberately independent of everything under
`smt/` — this is the N-version / trust-isolation point (DESIGN.md §10). Encoder
design decisions and grind findings are recorded in `tests/gate/NOTES.md`; read
that before touching `encoder.ml`.

### Running

- `make gate` — build, run `gate selftest` (sha256 FIPS vectors + sexp), then
  `gate run`: honeypots first (abort red if any is CERTIFIED), then the
  `tests/cases` corpus, using the cache in `../cache`. Digest to stdout; full log
  (and every generated `.lean` / Lean output) under `../logs/gate-<timestamp>/`.
- `gate certify FILE.smt2 [--no-cache] [--timeout SECS]` — certify one file. Exit
  codes: 0 CERTIFIED, 1 REFUTED/ENCODE_ERROR, 2 INCONCLUSIVE, 3 MALFORMED,
  4 UNSUPPORTED, 5 NO_STATUS.
- `gate run [--cases DIR] [--honeypots DIR] [--cache DIR] [--logs DIR] [--no-cache]
  [--timeout SECS]`. Env overrides: `OXSMT_LEAN`, `OXSMT_CACHE`, `OXSMT_LOGS`.

### Outcome semantics (DESIGN.md §8)

- **CERTIFIED** — Lean kernel-checked our claim (unsat: `⋀ assertions → False` by
  `grind`; sat: the model satisfies `⋀ assertions` by `decide`/`native_decide`).
- **REFUTED** — Lean kernel-checked the *opposite* claim (a satisfying witness for
  a claimed-unsat query, or a `grind` proof of unsat for a claimed-sat query).
  Ship-stopping: any REFUTED case, or any honeypot that gets CERTIFIED, turns the
  gate red.
- **INCONCLUSIVE** — grind gave up or timed out and nothing refuted (soft; a
  completeness signal, not a soundness one).
- **ENCODE_ERROR** — the encoder produced Lean that failed to elaborate (a bug in
  the trusted encoder; loud).
- **MALFORMED / UNSUPPORTED** — the reader rejected the file (bad syntax/sort vs.
  a well-formed construct outside the QF_UFLIA subset).

REFUTED is never inferred from parsing grind diagnostics — it is always a second,
kernel-checked Lean proof of the opposite (NOTES.md).

### SAT models and refutation witnesses

A sat claim (and an unsat-refutation witness) needs a sidecar model file:
`foo.smt2` → `foo.model`, an s-expression `(model (sort S 2) (const x 3)
(const a 0) (fun f (default 0) (case (0) 0) ...))`. Values are interpreted per the
symbol's declared sort (Int literal, `Fin n` index for an uninterpreted sort, or
`true`/`false`). Every function needs a `(default …)`. Format details in
`tests/gate/model.ml`.

### Cache format (`../cache`, never in git)

One s-expression file per entry, named `<key>.sexp`. The key is
`SHA-256(canonical-query ‖ claim ‖ model ‖ encoding-version ‖ lean-version ‖
grind-config)`; folding the toolchain identifiers into the key keeps the cache
monotonic (a new encoder or Lean version yields new keys; nothing is overwritten
or silently re-certified). Canonicalization (`canonical.ml`) sorts assertions and
commutative operands and canonically prints terms; it does **not** rename symbols
in v1 (see NOTES.md). Timeouts and honeypots are never cached.

### Encoding-version bump rule

`Encoder.encoding_version` (currently `enc-v1`) MUST be bumped on any change to the
emitted Lean — preamble, tactic, or term mapping. The cache is keyed on it, so a
bump cleanly invalidates every prior certification rather than silently trusting a
stale one.
