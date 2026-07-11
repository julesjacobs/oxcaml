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

## In-tree module self-tests (outside this dir)

Some modules also carry a stdlib-only unit/property self-test living *beside the
code* under `smt/<module>/test/`, not here — they are the module's own TCB check,
not the external gate. Run individually:

- `make core-test` — `smt/core` term layer (ADR-0003 invariants).
- `make preprocess-test` — `smt/preprocess` desugaring passes + Tseitin
  clausifier. Its oracle is brute-force **equivalence by evaluation** (a
  deliberately independent, test-only evaluator over `Term.t`): original vs.
  preprocessed formula on every enumerated assignment (fresh symbols witnessed
  via the passes' definitions), and original⇔CNF over enumerated atom
  assignments for the clausifier. Also checks `Term.Debug.check ~mode:Pipeline`
  on outputs, determinism, and the `Unsupported` boundary. This evaluator is
  *not* the project's N-version model evaluator (that is a separate, later
  agent's work); it shares no code with the gate.

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

- `make gate` — build, run `gate selftest` (sha256 FIPS vectors + sexp + cache-key
  injectivity), then `gate run`: honeypots first (abort red unless the floor is
  met and every honeypot matches its expected outcome), then the `tests/cases`
  corpus, using the cache in `../cache`. Digest to stdout; full log (and every
  generated `.lean` / Lean output) under `../logs/gate-<timestamp>/`.
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

### Honeypots (`tests/gate/honeypots/`)

Known-wrong inputs the gate must catch — a green gate that hasn't proven it can
go red is unaudited (DESIGN.md §10). They run first, with the cache disabled, and
are never cached. The phase is not vacuously satisfiable:

- a hard floor (`min_honeypots`, currently 4) — fewer present ⇒ RED "gate
  unaudited" (so an empty/missing glob cannot pass);
- each honeypot declares its expected outcome in a sidecar `foo.expect`, one tag
  from the allowlist `REFUTED` / `MALFORMED` / `UNSUPPORTED` / `INCONCLUSIVE`.
  `CERTIFIED` (or any other/typo'd value) is rejected as an invalid expectation
  — a honeypot may never be expected to certify. The gate asserts the actual
  outcome equals it, so a honeypot degrading from REFUTED to INCONCLUSIVE turns
  the gate RED rather than passing silently; a missing `.expect` is a breach.

A honeypot that gets CERTIFIED is always a breach. Current set: two sat-claimed-
unsat (LIA + EUF, each REFUTED via a kernel-checked witness model), one unsat-
claimed-sat with a wrong model (REFUTED via grind), one malformed (rejected).

The stdout digest always prints a one-line attestation that the audit ran, green
or red, e.g. `honeypots: 4/4 matched, floor 4, none certified`.

### Cache format (`../cache`, never in git)

One s-expression file per entry, named `<key>.sexp`. The key is
`SHA-256(canonical-query ‖ claim ‖ model ‖ encoding-version ‖ lean-version ‖
grind-config)`; folding the toolchain identifiers into the key keeps the cache
monotonic (a new encoder or Lean version yields new keys; nothing is overwritten
or silently re-certified). Canonicalization (`canonical.ml`) sorts assertions and
commutative operands and serialises the query with an **injective netstring
encoding** (each atom `A<len>:<bytes>`, each list `L<count>:<subnodes>`) so that
bytes inside a `|quoted symbol|` cannot forge token boundaries — two different
queries can never collide onto one key (the injectivity argument is in the file
header; the qA/qB exploit that motivated it lives in `tests/gate/collision/` and
is asserted distinct by `gate selftest`). It does **not** rename symbols in v1
(see NOTES.md). Entries are published by atomic temp-file + `rename`. Timeouts and
honeypots are never cached.

### Encoding-version bump rule

`Encoder.encoding_version` (currently `enc-v1`) MUST be bumped on any change to the
emitted Lean — preamble, tactic, or term mapping. The cache is keyed on it, so a
bump cleanly invalidates every prior certification rather than silently trusting a
stale one.

## STATUS.md generation (`make status`, `tools/status_gen/`)

`STATUS.md` is the master's empirical view of the world (DESIGN.md §8.4, §11):
**outcome metrics first** (goal-displacement defense), process metrics after. It
is **generated, never hand-edited** — the banner says so. There is no CI/nightly
scheduler yet; the make targets ARE the deliverable, and a scheduler wires them
later.

Two targets, deliberately split:

- **`make status`** — pure aggregation of artifacts already on disk. Runs
  **nothing** (no harness, no Lean), writes no stats. This is what keeps the
  committed `STATUS.md` **byte-stable**: back-to-back `make status` produce
  identical output given the same `(repo, logs)`. Run this from `main/` (paths
  like `../logs`, `../cache` resolve as siblings of `main/`, per
  M0-harness-hygiene — from a worktree they'd point at `worktrees/`).
- **`make status-fresh`** — the nightly path. Runs the fast harness once (writing
  a new stats JSONL and capturing its pass/fail digest to
  `../logs/harness/last-digest.txt`), then calls `make status`. Kept separate
  precisely because it *mutates* inputs; folding it into `make status` would make
  the committed file's diff perpetually dirty and train readers to ignore it.

The generator (`tools/status_gen/status_gen.ml`, stdlib+Unix, standalone — it
lives under `tools/` alongside `check_frozen.sh` rather than in `tests/`, so it
does not couple to the harness build) **only aggregates existing artifacts**;
it runs nothing and re-derives no product state. Inputs, each optional (a missing
one degrades to `n/a`, never a crash):

- **TASKS.md** → per-milestone done/total and the current milestone (first
  `M<n>-` row group with any non-`done` row);
- **git** → the `generated at <HEAD>` line (git HEAD short hash, **never
  wall-clock**, so the committed file stays reproducible), worktree/branch
  hygiene, and days-since-last-outcome-improvement (commits touching `smt/` or
  `tests/cases/`, measured to HEAD's commit timestamp — a documented heuristic);
- **the last captured harness digest** (`../logs/harness/last-digest.txt`, written
  by `make status-fresh`) → live pass/fail;
- **the latest full `../logs/gate-*/gate.log`** → gate outcome counts, honeypot
  floor, cache hit-rate, Lean/encoding versions (prefers a full `gate run` over an
  honeypot-only `gate selftest`; honeypot health = none `CERTIFIED` and count ≥
  floor);
- **the most recent stats JSONL** (the sidecar above) → search-counter **bucket**
  distributions (log-scale, deterministic) and the corpus solved-rate over
  `tests/cases` (fraction of goals with a definite `sat`/`unsat` verdict — **0%
  while the solver is a stub; this is the number that must move**). Per-goal
  `wall_ms` is deliberately **not** emitted into `STATUS.md` — it is
  nondeterministic and stays only in the uncommitted stats sidecar/logs;
- **`tools/line_budgets.txt`** (committed, master-owned) → per-module `.ml`+`.mli`
  line counts vs budget, flagged `OVER` past budget (a tripwire, not a gate —
  DESIGN.md §10).

Determinism: given the same `(repo, logs)` the output is byte-identical except
the `generated at <HEAD>` line. Digest-first: the target prints ~5 summary lines
and writes the full document to `STATUS.md`.

## SAT core self-test + bench (`smt/solver/test/`, `make sat-test` / `make sat-bench`)

The propositional CDCL SAT core (M1, `smt/solver`) carries its own test suite,
co-located with the module (like `smt/core/test/`, not under `tests/`) and
stdlib-only. It is independent of the `.smt2` harness and the Lean gate: the SAT
core sees no terms, so its oracle is a naive **DPLL reference solver** written
from scratch in the same test lib, plus model self-evaluation.

- **`make sat-test`** — `smt/solver/test/sat_test.ml`. Deterministic (fixed-seed
  xorshift), nonzero exit on any failed check. Covers: exact learned clause +
  backjump level + antecedent chain on textbook conflicts (observed through the
  proof-readiness `trace` hook), assumption semantics + failed-assumption core,
  incremental add-after-solve, pigeonhole unsat, determinism (same formula twice
  → identical stats + model), and ~20k random small CNFs (≤12 vars) whose
  verdicts must all agree with the DPLL oracle. Every `sat` verdict is
  self-checked by evaluating all clauses under the model (cheap, always on).

- **`make sat-bench`** — `smt/solver/test/sat_bench.ml`. Runs the core over a
  DIMACS corpus (`SAT_CORPUS`, default the uf50/uuf50 families under
  `../corpora/SAT`). GLOBs `**/*.cnf` at runtime and tolerates an absent corpus
  with a clear message (exit 0). Label-checks families whose name encodes the
  verdict (SATLIB `uf*` = sat, `uuf*` = unsat), self-checks every sat model, and
  fails on any mismatch. Digest to stdout; full per-file log under `../logs`.
  Deterministic: the "slowest" ranking is by conflict count, never wall-clock.

- **`smt/solver/test/dimacs.ml`** — the DIMACS parser is a **test-only** dune
  library (`oxsmt_dimacs`), never linked into shipped solver code, the same split
  discipline the SMT-LIB parser follows (DESIGN.md §3).

## SMT-LIB printer + parser tests (`smt/smtlib/test/`, M0-smtlib)

These live under `smt/` (not here) because they exercise the `oxsmt_smtlib` printer
and the test-only `oxsmt_smtlib_parser`, but they follow the same digest-first,
deterministic discipline. Two entry points:

- `make smtlib-test` — the committed, corpus-independent round-trip suite (96
  checks).
  - **Round-trip A (print → parse):** ~30 programmatic sessions cover every one of
    the 9 term nodes, symbol-quoting edge cases (a symbol named `a b(c)`, a function
    named `Int`, digit-leading names, and simple symbols that must *not* be quoted),
    negative constants, `div`/`mod`, `distinct`/`abs` desugaring, and deep nesting.
    Each session is built in a `Context`, printed, and parsed back **into the same
    `Context`**, so equality is `Term.equal` (hash-cons tag identity) — the strongest
    check that print;parse is the identity on our subset (ADR-0003's single-`Context`
    contract makes the strong check the simple one).
  - **Naming classes (R1):** class-driven tables assert that reserved *words*
    (`let`, `as`, `forall`, `_`, `!`, …) round-trip via `|quoting|`, while names
    equal to predefined *operators* (`+ - * = <= and or not => ite …`), predefined
    *sorts* (`Int`/`Bool`, in sort position), and the empty name are **refused** by
    the printer (`Printer.Unsupported`) — quoting is lexical, so `|+|` is still the
    operator `+` and cannot disambiguate a user symbol. The tables are the spec:
    adding a name to a class is a one-line change, so a regression cannot slip past
    (as the original operator-name gap did past 46 green checks).
  - **Round-trip B (parse → print → parse):** over `tests/cases/*.smt2`, the harness
    fixtures, and the gate honeypots — parse, print, re-parse into the same
    `Context`, assert the assertion lists are `Term.equal` and the `:status` label
    survives. Files outside our subset are reported as *skipped* (expected: the
    `push`/`pop` multi-check fixture; the deliberately malformed honeypot), not
    failed.
- `make smtlib-corpus` — a parse-only smoke over a public corpus (default
  `../corpora/QF_UFLIA`, never in git, hence separate from `smtlib-test`). Reports
  ok / unsupported / malformed / skipped-large / crashed; a crash fails the run,
  unsupported constructs are expected diversity. Full per-file results to
  `../logs/smtlib-corpus-smoke.log`, digest to stdout. A 20 MB per-file size cap
  (`--max-bytes`) guards the box from pathological multi-MB instances the eager
  parser would otherwise blow up on.

### Printer rendering choices (kept parseable by standard tools)

Full detail in `smt/smtlib/printer.mli`. In brief: integer constants print as the
numeral, negatives as `(- N)` (so `min_int` needs no negation); `Arith` linear
forms print as a sum of `(* c t)` products (coefficient 1 → bare term, nonzero
constant last, a lone product without a unary `+`); order atoms as `(<= arg 0)`;
`Eq`/`Not`/`And`/`Or`/`Ite` directly; reserved `div`/`mod` as `(div a b)`/`(mod a
b)` and never declared; `distinct`/`abs` never appear (they desugar at construction
to `Not`/`Eq` and `Ite`).

**Symbol quoting is a TCB concern** (the printer feeds the Lean oracle). Because
quoting is purely lexical (`|s|` and `s` are the same symbol), the printer *refuses*
(`Printer.Unsupported`) names it cannot render faithfully: names containing `|`/`\`,
the empty name, names equal to a predefined operator (function position) or
predefined sort (sort position). Reserved *words* are representable and are
`|quoted|`. This closes a gap where a function literally named `+` would have
printed `(+ a b)` — read by any standard tool (and our own parser) as integer
addition, silently certifying a different formula. See `printer.mli`.

### Parser subset

Commands: `set-logic` (QF_UF/QF_LIA/QF_UFLIA/QF_IDL/QF_RDL), `set-info :status`,
`declare-sort` (arity 0), `declare-fun`, `declare-const`, `assert`, `check-sat`,
`exit`. Terms: `true`/`false`, numerals, `and`/`or`/`not`/`=>`, `ite`,
`=`/`distinct`, chainable `<=`/`<`/`>=`/`>`/`=`, `+`/`-`/`*` (multiplication must be
linear — ≥2 non-constant factors is `Unsupported`), `div`/`mod`/`abs`, `let`
(parallel binding), `(! t …)` annotations (attributes dropped), `|quoted symbols|`,
`;` comments, and declared symbols. `define-fun` macros, quantifiers, `push`/`pop`,
compound sorts, and arithmetic exceeding native `int` are `Unsupported`;
ill-sorted / undeclared / wrong-arity input is `Malformed`.

### Bool-`=` / gate interaction (tracked M0-gate-iff)

A Bool-sorted `Eq` is an iff. The printer emits it faithfully as `(= a b)` and the
parser reads it back. The Lean gate's *reader* separately tracks whether it accepts
Bool-sorted `=` (the M0-gate-iff item); that is the gate's concern, not the
printer's — the printer must render the frozen term type completely, and a dump
containing an iff is valid SMT-LIB regardless.
