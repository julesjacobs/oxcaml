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
- `make euf-test` — `smt/theories/euf` proof-producing congruence closure. Its
  oracle is an independent **naive quadratic congruence closure** (a from-scratch
  union-find + brute-force O(n²) congruence rule over a fixed term universe),
  sharing no code with the engine's union-find / explanation forest / congruence
  table. See the dedicated section below.
- `make lia-test` — `smt/theories/lia` LIA decision procedure (Dutertre-de Moura
  simplex + branch-and-bound). Two independent oracles, both test-only and
  written from definitions (not solver internals): (1) **brute-force
  cross-check** — 3000 random small *bounded* systems (≤3 vars, coeffs ≤5,
  each variable box-bounded so the feasible set is finite), whose sat/unsat
  verdict and returned integer model are checked against exhaustive integer
  enumeration over the box; (2) an **independent Farkas verifier** run on every
  conflict, recomputing `Σ multiplierᵢ · half-planeᵢ` from the premise atoms and
  confirming it cancels all variables to a strictly positive constant (a tampered
  certificate — dropped premise, zeroed/negated multiplier — is asserted to be
  rejected, the mutant demonstration). Also: exact-multiplier hand cases,
  gcd-tightening, strict-vs-nonstrict via δ, unbounded, near-max_int overflow, and
  run-twice determinism. The solver additionally self-checks every Farkas
  certificate at production (`Simplex.Farkas_error`), independent of these tests.

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
harness failure.

Since **M1-wiring**, `SOLVER` defaults to the real solver CLI
(`_build/default/tests/solver/oxsmt_cli.exe`): it parses `.smt2` via the test-only
SMT-LIB parser and drives the shipped `Session` (`smt/interface`). A batch file
(one check-sat, no push/pop) is solved for real; an incremental file (push/pop or
multiple check-sats) degrades to one `unknown` block per check-sat (always sound —
see THE SOUNDNESS RULE in `session.mli`). Pure-Boolean sat prints a Boolean model
(`(model ((p true) (q false)))`). The `stub_solver`
(`_build/default/tests/harness/stub_solver.exe`, `unknown`/zero for every
check-sat) stays buildable as a fallback and for harness-plumbing tests — override
`SOLVER=` to use it. The session's own semantics (push/pop, model, guards) are unit
tested by `make wiring-test` (`tests/solver/wiring_test`).

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

#### Degradation honeypots (`tests/cases/degrade_*.smt2`)

These exist to weaponize the label check against a specific regression. Each is a
formula whose **boolean skeleton is satisfiable but whose theory is unsat** — two
independent theory atoms that the propositional core cannot relate (e.g. `x<0 ∧
x>0`; `x=y ∧ f(x)≠f(y)`; the mixed `x=y ∧ f(x)<f(y)`). They carry `:status unsat`,
and the wired v1 solver's correct answer is `unknown` (THE SOUNDNESS RULE in
`session.mli`: theory atoms present ⇒ a propositional sat is downgraded), which is a
tolerated completeness gap. The trap: if the wiring ever regresses to reporting
`sat` here, the label check turns that into a **soundness failure (red)** rather
than a silent golden diff. The same three are asserted directly in
`tests/solver/wiring_test.ml`. When EUF/LIA/combination land (M2–M4) these become
real `unsat` and the goldens flip.

The `degrade_*` files are the dedicated, minimal traps, but they are not the only
ones: **several pre-existing theory cases already have this exact shape** — labeled
`unsat`, yet the wired v1 solver returns `unknown` because their skeleton is
propositionally satisfiable — so their label check equally guards against a
degradation-to-`sat` regression. As of M1-wiring those are `euf_congruence`,
`euf_transitivity`, `lia_bounds`, `lia_farkas`, `lia_mul_const`, `mixed_euf_lia`,
`ite_case`, and `let_case`. (By contrast an `unsat`-labeled case whose skeleton is
*propositionally* unsat — e.g. `distinct_case`, `iff_chain_unsat` — verdicts a real
`unsat` and does not exercise the degradation path.)

### Eval self-check (layer 1 — every sat model)

Before accepting **any `sat` verdict**, the harness runs the independent N-version
evaluator CLI (`tests/eval`, `--eval PATH`, default the built `eval_cli.exe`) on
that goal's model (DESIGN.md §8 layer 1). It renders the solver's inline model
into the evaluator's sidecar grammar (one `(const NAME VALUE)` per binding, into a
temp file) and maps the CLI's exit code:

- **`MODEL-SATISFIES` (exit 0)** → the model checks out; the goal may pass.
- **`MODEL-FAILS` (exit 1)** → a **soundness failure** (`model unsound`): our own
  `sat` model does not satisfy the assertions. Red regardless of the golden, and
  **never promotable** — the same class as a label mismatch.
- **`MALFORMED`/`UNSUPPORTED` (exit 2), or eval could not run** → a **harness
  failure** (`eval unusable`) with a distinct message: a model our own evaluator
  cannot read is a contract bug, not a pass. Also never promotable.

A `sat` verdict the harness *cannot* self-check (no `--eval` configured, or a
`sat` with no model) is likewise a failure — we never accept an unchecked `sat`.
Only single-check-sat batch files currently produce `sat` (incremental files
degrade to `unknown`), so evaluating the whole file's assertion set matches the
goal; a future multi-check-sat `sat` would need per-goal assertion slicing.
The harness self-test drives this end-to-end: a lying-model solver (emits `sat`
with a wrong model) is confirmed to go red via eval.

### Workflow

- `make test` — runs the harness self-test (proves red-detection works, incl. the
  lying-model eval path), then the golden regression over `cases/` + `fixtures/`.
  Prints a digest (`PASS`/`FAIL` counts, first failures with paths to full diffs
  under `../logs/harness/<run>/`). Exits non-zero on any diff, missing golden,
  label mismatch, **unsound sat model, unusable eval,** or solver error.
- `make promote` — accepts current solver output as the new golden, rewriting
  the `.expected` sidecars for missing/mismatched goldens and printing a
  per-file diffstat so the promoting agent sees what it accepts. **Soundness
  signals (label mismatch, unsound sat model) and errors (solver, or eval unable
  to read our model) are never masked** — promote refuses them and they stay red.
- Override the solver, evaluator, or paths: `make test SOLVER=path/to/real`,
  `EVAL=path/to/eval`, or `LOGS=`, `STATS=`, `CASES=`, `FIXTURES=`.

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

- a hard floor (`min_honeypots`, currently 11) — fewer present ⇒ RED "gate
  unaudited" (so an empty/missing glob cannot pass);
- each honeypot declares its expected outcome in a sidecar `foo.expect`, one tag
  from the allowlist `REFUTED` / `MALFORMED` / `UNSUPPORTED` / `INCONCLUSIVE`.
  `CERTIFIED` (or any other/typo'd value) is rejected as an invalid expectation
  — a honeypot may never be expected to certify. The gate asserts the actual
  outcome equals it, so a honeypot degrading from REFUTED to INCONCLUSIVE turns
  the gate RED rather than passing silently; a missing `.expect` is a breach.

A honeypot that gets CERTIFIED is always a breach. Current set (11): two sat-
claimed-unsat (LIA + EUF, each REFUTED via a kernel-checked witness model), one
unsat-claimed-sat with a wrong model (REFUTED via grind), one malformed
(rejected); plus one per codex reader-hardening finding (G1–G4): a `|0|`-vs-`0`
quoted-numeral trap (MALFORMED — the quoted symbol is ill-sorted against the
numeral), a `:source "(assert false)"` string-injection trap (REFUTED — the
string is inert, the real theorem is refuted), a multi-`check-sat` trap
(UNSUPPORTED — asserts after check-sat rejected), and a `div`/`mod` trap
(UNSUPPORTED — loud, not a silent MALFORMED-green bypass); plus an `abs` trap
(UNSUPPORTED — same recognised-but-unsupported LIA class as div/mod); plus two
reader-vs-execution divergence traps (codex round-3, both MALFORMED): a
`(check-sat X)` junk-arg trap and a `(exit) (assert false) (check-sat)` post-exit
trap — each CERTIFIES a false unsat on the pre-fix reader (empirically verified)
and is rejected by the fixed reader that assembles exactly the query a conformant
solver would execute. See `../logs/gate3-recertification.md`.

The stdout digest always prints a one-line attestation that the audit ran, green
or red, e.g. `honeypots: 11/11 matched, floor 11, none certified`.

**Accounting invariant.** The case digest prints a sum identity — `accounting: N
inputs = C certified + I inconclusive + Q quarantined + R refuted + E
encode_error` — and the gate is RED if it does not close (a query silently
dropped). Every input lands in exactly one class; MALFORMED / UNSUPPORTED /
NO_STATUS are *quarantine* reasons, listed per-file with their reason (never
dropped). Quarantine is a coverage gap, not RED; only refuted / encode_error /
honeypot-breach / accounting-mismatch are RED. This makes silent oracle bypass
(e.g. the old div/mod MALFORMED-green) structurally impossible, not patched
per-bug.

**Ruling — quarantine-is-green is an accepted asymmetry** (master, recorded as a
decision): it is the same asymmetry as INCONCLUSIVE — nothing can be CERTIFIED
through quarantine, so no unsound verdict is laundered; the only cost is oracle
coverage, kept LOUD via the per-reason counts + QUARANTINED listing. The real
close of the div/mod (+abs) gap is the `gate-divmod-elim` follow-up (encoder-side
euclidean/ite elimination, before M4 LIA). Quarantine counts should also flow to
STATUS.md as a coverage metric (a `status_gen` concern, task #133).

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

- **TASKS.md** → per-milestone done/total (milestones sorted numerically, so `M10`
  follows `M2`) and the current milestone (first `M<n>-` row group with any
  non-`done` row). If no milestone rows parse at all, the current milestone reads
  **`unknown`** — never "all complete", which would falsely imply the project is done;
- **git** → the `generated at <HEAD>` line (git HEAD short hash, **never
  wall-clock**, so the committed file stays reproducible), worktree/branch
  hygiene, and days-since-last-outcome-improvement (commits touching `smt/` or
  `tests/cases/`, measured to HEAD's commit timestamp — a documented heuristic);
- **the last captured harness digest** (`../logs/harness/last-digest.txt`, written
  by `make status-fresh`) → live pass/fail;
- **the latest full `../logs/gate-*/gate.log`** → gate outcome counts, honeypot
  floor, cache hit-rate, Lean/encoding versions (prefers a full `gate run` over an
  honeypot-only `gate selftest`; honeypot health = none `CERTIFIED` and count ≥
  floor). A **REFUTED** case (Lean proved our verdict wrong) or a honeypot breach
  emits a **loud leading `‼ GATE RED — SOUNDNESS BREACH`** line so a soundness
  failure screams from the outcome metrics rather than hiding in a count;
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
  `--parse-only` parses + strict-validates every file without solving — a
  corpus-hygiene sweep that surfaces truncated/corrupt DIMACS over families too
  hard to solve (e.g. pigeon-hole).

- **`smt/solver/test/dimacs.ml`** — the DIMACS parser is a **test-only** dune
  library (`oxsmt_dimacs`), never linked into shipped solver code, the same split
  discipline the SMT-LIB parser follows (DESIGN.md §3). It is **strict**: with a
  `p cnf V C` header present it rejects (loud `Parse_error`) a parsed-clause-count
  ≠ C and any nonempty unterminated trailing clause, so a truncated file is a
  reject rather than a silently-shorter formula that can flip unsat→sat
  undetected (sat-review item 11, the dubois100 corruption shape). The SATLIB
  `%`-footer early-stop is preserved.

## EUF self-test (`smt/theories/euf/test/`, `make euf-test`)

The proof-producing congruence closure (M2, `smt/theories/euf`) carries its own
stdlib-only suite, co-located with the code. Its oracle is a **naive quadratic
congruence closure** written from the EUF spec alone in the same test lib (a
from-scratch union-find plus a brute-force O(n²) congruence rule iterated to a
fixpoint over a fixed, subterm-closed term universe). It shares no code with the
engine's union-find / explanation forest / congruence table, so agreement is a real
cross-check. This is separate from the `.smt2` harness and the Lean gate: EUF is
engine-independent and the ADR-0005 adapter does not exist yet.

- **`make euf-test`** — `smt/theories/euf/test/euf_test.ml`. Deterministic
  (fixed-seed xorshift), nonzero exit on any failed check. Covers:
  - the textbook refutation `(f x)=a, x=y, (f y)≠a` with the **exact** conflict
    premise set traced;
  - deep congruence chains — `a=f(a)` collapsing every `fᵏ(a)`, and
    `f³(x)=x ∧ f⁵(x)=x ⇒ f(x)=x` (pure congruence + transitivity, cross-checked);
  - (dis)equality **propagation** of watched equality atoms with lazy explanations
    (positive, congruence-positive, and disequality-negative cases);
  - **randomized cross-check** (~4.7k assert-cases): random eq/neq sequences over a
    10-node universe, comparing the full equivalence relation and the consistency
    verdict against the naive oracle;
  - **explanation soundness**: for implied equalities, the returned premises replayed
    into a fresh naive closure suffice to re-derive the equality (the engine also
    self-checks this internally on every explanation, DESIGN §7; here it is re-checked
    from the test side);
  - **push/pop**: randomized interleaved assert/push/pop/check vs a
    recomputation-from-scratch oracle, including registration inside a frame being
    undone on pop (e-node truncation + use-list restore);
  - **determinism**: identical input sequence twice → identical propagation stream and
    explanation.

  Explanation *minimality* is deliberately **not** asserted: Nieuwenhuis-Oliveras gives
  small (path-based) explanations, not minimal ones. A throwaway congruence-drop mutant
  in `merge` is caught by the textbook, chain, and cross-check tests.

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
  parser would otherwise blow up on. Current QF_UFLIA (659 files): 583 ok, 76
  unsupported, 0 malformed, 0 crashed. With `define-fun` now expanded, the entire
  remaining 76 are the *same* files, blocked only by integer literals beyond native
  `int` (Certora uint256 constants ≈ 2^256) — the tracked native-`int`/bignum
  limitation (ADR-0003), not a parser-coverage gap. 659/659 needs bignum in the
  core, out of this task's scope.

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

### Shared lexer + `make fuzz-lex` (ADR-0008)

Both the printer's quoting and the parser's tokenizing go through one lexer,
`smt/lexical` (`oxsmt_lexical`, SMT-LIB 2.6 §3.1), whose token type never loses
kind: a quoted `|0|` is a symbol, never the numeral `0`; `|let|` is a symbol, never
the reserved word. This is the fix for the token-boundary bug family (the gate's
`|0|` confusion + the cache-collision exploit). `make fuzz-lex` is a standing,
deterministic adversarial fuzzer (fixed seeds) over that boundary: it checks
printer↔lexer kind-preservation, print→parse round-trip, and lexer idempotence on
inputs built from numeral-lookalikes, reserved-word/operator symbols, whitespace/
paren/newline-bearing quoted symbols, and high bytes. A small subset runs inside
`make smtlib-test`; the full stream (`make fuzz-lex`, 160k cases) runs on demand.
Because a shared-lexer bug hits printer and reader symmetrically (round-trips can't
catch it), the truly uncorrelated check is the cross-implementation differential
against the gate's independent reader — that lands with the gate migration over
`task/gate3` (ADR-0008).

### Parser subset

Commands: `set-logic` (QF_UF/QF_LIA/QF_UFLIA/QF_IDL/QF_RDL), `set-info :status`,
`declare-sort` (arity 0), `declare-fun`, `declare-const`, `assert`, `check-sat`,
`exit`, `define-fun`. Terms: `true`/`false`, numerals, `and`/`or`/`not`/`=>`,
`ite`, `=`/`distinct`, chainable `<=`/`<`/`>=`/`>`/`=`, `+`/`-`/`*` (multiplication
must be linear — ≥2 non-constant factors is `Unsupported`), `div`/`mod`/`abs`,
`let` (parallel binding), `(! t …)` annotations (attributes dropped),
`|quoted symbols|`, `;` comments, and declared symbols. Quantifiers, `push`/`pop`,
compound sorts, recursive `define-fun`(-rec), and arithmetic exceeding native `int`
are `Unsupported`; ill-sorted / undeclared / wrong-arity input is `Malformed`.

**`define-fun` macros** are expanded by capture-avoiding substitution at each use
site (they are macros, not recursive functions). The argument s-expressions are
read in the *caller's* scope; the body is then read in a fresh scope containing
*only* the parameters, so the caller's local `let`-bindings never leak into the
body and a nested `let` in the body binds tighter than a parameter (both fall out
of the innermost-first scope lookup). Argument values are already-built `Term.t`s,
so substituting them can never capture. Zero-arg `define-fun` is a named constant.
Recursion (direct or mutual) is caught by a cycle guard and rejected `Unsupported`;
`define-fun-rec`/`define-funs-rec` are rejected outright. The expansion is
sort-checked through `Context` as usual — an ill-sorted argument or a body whose
sort disagrees with the declared result sort fails `Malformed`, never silently.
**define-fun is erased by expansion**, so round-tripping prints the *expanded*
terms (no `define-fun` in the output) and re-parsing still fixpoints — the
`define_fun_cases` tests cover both the expansion equalities and this round-trip.
Expansion is **memoized** on `(define name, argument-term tags)`, so a nested
doubling chain (`f_{i+1}(x) = f_i(x) + f_i(x)`) expands in linear rather than
exponential time — a `define_fun_perf` test parses a depth-40 chain (2^40 body
reads unmemoized) and asserts it both completes fast and equals `2^40 * a`.

### Bool-`=` / gate interaction (tracked M0-gate-iff)

A Bool-sorted `Eq` is an iff. The printer emits it faithfully as `(= a b)` and the
parser reads it back. The Lean gate's *reader* separately tracks whether it accepts
Bool-sorted `=` (the M0-gate-iff item); that is the gate's concern, not the
printer's — the printer must render the frozen term type completely, and a dump
containing an iff is valid SMT-LIB regardless.

## Mutation testing (`tools/mutants/`, `make mutants`)

The suites above are only as good as their ability to go red. Mutation testing
audits that (DESIGN.md §10: "routinely inject seeded faults and require the tiered
suite catches them; a surviving mutant halts feature work on that module").

`tools/mutants/registry/` holds seeded faults (one `<name>.patch` + `<name>.meta`
each), drawn from the project's review history and spanning core / sat /
preprocess / gate / smtlib. `make mutants` (or `make mutants MODULE=core`) applies
each patch in a throwaway git worktree off HEAD, runs the mutant's declared suite,
and requires a red exit:

- **KILLED** — the suite caught it (the system working).
- **SURVIVED** — the suite stayed green: a real oracle gap. Exits 1 and halts
  feature work on that module. Strengthen the suite; never weaken the mutant.
- **PATCH-FAILED / LINT-REJECT** — the patch drifted, or targets a frozen `.mli`
  (never a legal target). Exits 2.

Full detail and how to add/refresh a mutant: `tools/mutants/registry/README.md`.
The runner never mutates `main/` or the task worktree — every fault is applied in
a scratch worktree under `../worktrees/scratch-mutant-*`, always cleaned up on exit.
This is the on-demand / nightly counterpart to the per-PR suites; it is not on the
`make test` path.

## N-version model evaluator (`tests/eval/`, `make eval-test`)

Layer-1 self-check (DESIGN.md §8): *every `sat` answer is checked by evaluating the
assertions under the candidate model with a trivial independent interpreter.* Written
**spec-only** by a separate agent session with no access to solver internals (§10
N-version checkers), so it shares no common-mode blind spot with the search code. The
library `oxsmt_eval` depends on **`oxsmt_core` only** — the frozen `Term`/`Sort`/
`Context` vocabulary — and nothing else in the tree (not the shipped parser, not the
gate reader, not the preprocess test evaluator).

Modules: `sexp` (a fresh ~120-line s-expression reader), `reader` (an independent
QF_UFLIA `.smt2` reader that builds frozen-API terms through `Context`'s smart
constructors, so its output is well-sorted/hash-consed by construction; reject-don't-
guess on anything outside the subset), `model` (the sidecar reader), `value` +
`eval` (the total denotational evaluator). The `eval` CLI answers *does MODEL satisfy
ASSERTIONS?*

```
eval <file.smt2> <file.model>
  exit 0  MODEL-SATISFIES
  exit 1  MODEL-FAILS <index>       # 0-based first falsified assertion; failing-path
                                    # subterm values to stderr
  exit 2  MALFORMED | UNSUPPORTED   # bad syntax/sort/undefined-symbol vs out-of-subset
```

Digest to stdout, all detail to stderr (§11 context-frugal).

### Evaluation semantics (from ADR-0003, independently derived)

Values are `Bool b | Int n | Uninterp (sort, id)` (`id` = element index of an
uninterpreted sort). Per node: `Bool_const`/`Int_const` self-evaluate; `Arith` =
`Σ cᵢ·⟦tᵢ⟧ + const` over ℤ (subterms evaluated recursively — they may be `App`s);
`Le arg` = `⟦arg⟧ ≤ 0`; `Eq(a,b)` = structural value equality (Bool operands ⇒ iff);
`Not`/`And`/`Or` standard (`And`/`Or` force every operand so a model error stays loud
past a false one); `Ite` picks the taken branch. `App` on the reserved `div`/`mod`
symbols is **euclidean** (`x = d·q + r ∧ 0 ≤ r < |d|`); any other `App` is resolved in
the model. Integer ops are **overflow-guarded — they raise, never wrap** (I8 spirit).
An undefined symbol or a type mismatch is a loud failure, never a silent default.

### Model sidecar format (the documented contract, as this evaluator reads it)

A single s-expression. Tokens are typed against each symbol's *declared* sort/rank
(taken from the `.smt2`), which is why a bare numeral can mean an `Int` or an
uninterpreted element index depending on the declaration:

```
(model
  (sort S 2)                 ; optional: cardinality of an uninterpreted sort (for range checks)
  (const x 3)                ; Int-sorted constant
  (const p true)             ; Bool-sorted constant
  (const a 0)                ; uninterpreted element index (0-based, < the sort's card)
  (fun f (default 0)         ; every function needs a (default …)
         (case (0) 0)        ; (case (arg…) result); first matching case wins, else default
         (case (1) 0)))
```

Reading choices where the shape is thin (documented so they are the contract): negative
integers may be written bare (`-3`) or as `(- 3)`; a function's cases are matched by
structural value equality on the argument tuple, first match wins, falling back to the
mandatory `default`; a nullary symbol appears as `(const …)`, an arity-≥1 symbol as
`(fun …)`; a symbol declared in the `.smt2` but absent from the model is a loud
`MALFORMED` (an incomplete model), distinct from a model that defines a *wrong* value
(a clean `MODEL-FAILS`).

### `make eval-test` (53 checks, deterministic, nonzero exit on any failure)

One satisfying + one falsifying model per `Term` node kind (through the full reader →
model → eval pipeline); the gate's real `sat` cases + their `.model` sidecars, all of
which must `MODEL-SATISFIES` (auto-discovered from `tests/cases/*.model`); deliberately-
corrupted models that must `MODEL-FAIL`; the euclidean div/mod sign matrix (4 combos,
hand-computed `q`/`r` plus the `x = d·q + r` identity); an integer-overflow case that
must raise; and reject-don't-guess probes (unsupported logic, quantifier, nonlinear
`*`, undeclared symbol, ill-typed model value).

## Adversarial perf corpus (`tests/perf/`, `make perf-bench`)

DESIGN.md §8.4's deliberately-grown adversarial perf corpus, "so cliffs surface in
CI rather than in the first real codebase." A small committed OCaml generator
(`tests/perf/gen_perf.ml`, `make perf-gen`) emits deterministic `.smt2` families into
`tests/perf/cases/` (committed, so the corpus is fixed and reviewable). `make
perf-bench` runs the solver CLI over them and prints a per-case `{ verdict, counters,
wall_ms }` table, full log under `../logs/perf/`.

**This is a visibility tool, not a gate** (DESIGN §8: performance is surfaced, not
gated). Its stdout is *not* a committed golden — wall-clock is allowed here, unlike
the §I5/§I6 regression goldens — and a clean run always exits 0. It is not part of
`make test`.

**Do NOT move these cases into `tests/cases/`.** They are deliberately large and mostly
`unknown` under the current solver; `tests/cases/` is globbed by both the harness
(`make test`) and the gate (`make gate`), which would try to run and Lean-certify each
one — slow, and not what those suites are for. Perf cases stay under `tests/perf/cases/`.

Families: (a) `euf_diamond_d*` — equality diamonds; (b) `dense_simplex_c*` —
overlapping linear bounds; (c) `ite_tree_d*` — balanced Int ite trees (depth 8/10/12);
(d) `wide_sum_flat_n*` and `wide_sum_nested_n*` — flat vs left-nested sums; (e)
`pushpop_n*` — deep push/pop stacks; (f) `pigeonhole_n*` — PHP(n+1,n) pure-Boolean.

**What bites today vs. later.** The theory families (EUF, LIA — diamonds, dense
simplex, wide sums, Int ite trees) currently answer `unknown` *fast*: v1 degrades
theory atoms to `unknown` under the soundness rule, so their solve cost is near-zero
and their real perf value activates when combination lands (M4). What bites **today**
is (1) **term-construction / parse cost** — most sharply the `wide_sum_nested_*`
family, which reproduces the O(n²) construction cliff (#49): re-expanding the growing
`Arith` node (`terms_of`) at each nesting level. The `wide_sum_flat_*` family is the
mitigated contrast — the parser routes flat n-ary `+` through a single
`linear_combination` pass (O(n log n)); and (2) the **pure-Boolean pigeonhole** search,
a real `unsat` verdict from the SAT core. Sizes are chosen so the worst case today is
sub-second, not minutes.
