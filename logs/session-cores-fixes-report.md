# session-cores #106 — codex-review fixes

Lane: `task/session-cores`. Base `c0f37a744a`. Reviewed tip `bd21406975` (report:
`logs/codex-review/session-cores-bd21406975.md`). This report covers the fixes applied on
top of `bd21406975`; the new PIN sha for re-review is stated at the end.

The API is the observational `Session.last_unsat_core` / `Session.last_farkas`
(consumer: CHC interpolation #88). All four codex findings are addressed. No frozen
interface changed (`make check-frozen` 14/14); no solving-path behavior changed (byte-id
29/0 vs base).

## Finding 1 (HIGH) — equality-premise Farkas orientation

**Bug.** An Int equality `x = k` is lowered into BOTH an upper and a lower bound on the
same var (`Lia.equality_reading`), both attributed to the same premise token. A Farkas
multiplier paired with that token has no single half-plane orientation — the proof used
`x >= k` or `x <= k`, but the surfaced atom `x = k` cannot say which — so
`Σ coeffᵢ·half-plane(atomᵢ)` cannot be reconstructed. Poisons interpolation.

**Fix (fail-closed, Option B).** `Lia_adapter.last_conflict_core`: when any premise atom is
an Int equality, drop `farkas` to `None`. The `atoms` core itself stays valid and is still
surfaced (an equality is a sound core member) — only the ambiguous Farkas certificate is
withheld. `None` is always honest for an observational API. Chosen over "split the equality
into the oriented side actually used" because rendering the oriented inequality would have
to intern a fresh `Le` term — reintroducing Finding 3's mutation.
`smt/theories/lia/lia_adapter.ml` (`last_conflict_core`), doc in `.mli` and `session.mli`.

**Test.** `eq-premise` in `session_cores_test.ml`: `x = y ∧ x <= 0 ∧ y >= 1` (two vars so
the equality survives as a genuine conflict premise). Asserts: core present + contains an
equality premise + re-checks `Unsat` on a fresh Session; `last_farkas = None`.
RED verified: disabling the equality guard makes `farkas absent` fail (the unoriented
`x = y` coefficient is surfaced) — 1 failure / 47.

## Finding 2 (HIGH) — stale LIA evidence via the pure-BV fast path

**Bug.** `check_sat` dispatches pure QF_BV BEFORE `Cdclt.begin_check`; its `Unsat` arm sets
`last_verdict <- Unsat` without clearing the LIA stash (only `begin_check` cleared it). So
after `LIA unsat; pop; pure-BV unsat`, `last_unsat_core` returned the OLD LIA core.

**Fix.** Moved the stash clear out of `begin_check` into a standalone
`Cdclt.clear_last_conflict`, called at the single mandatory TOP of `check_sat` (beside the
`last_verdict <- Unknown` reset), so it dominates EVERY dispatch path: the degraded
early-`Unknown`, the pure-BV fast path (`Unsat`/`Sat`/`Unknown`, which bypasses
`begin_check`), and the combinator path. One dominating clear rather than one-per-verdict
is why no path can leak (the original per-path claim missed exactly the pure-BV `Unsat`
arm). `smt/interface/cdclt.ml{,i}`, `smt/interface/session.ml`.

**Test.** `bv-stale` in `session_cores_test.ml`: `push; LIA unsat; pop;` then a pure-BV
unsat (`b0 = b1 ∧ b0 <> b1`, width-8 BV vars, no Int term → `is_pure_bv` holds → fast
path). Asserts `last_unsat_core = None` and `last_farkas = None` after check-2.
RED verified: restoring the clear to `begin_check`-only makes both check-2 leak checks fail
(stale core AND stale farkas surface) — 2 failures / 46 — while the pre-existing Bool-only
staleness test still passes (Bool path calls `begin_check`), which is why the original test
missed the leak.

## Finding 3 (MEDIUM) — reading the API mutated future solving state

**Bug.** `signed_atom_term` rendered a negative premise via `Context.not_ ctx atom`. On a
cache miss (a negative theory assignment reached through Boolean structure with no interned
`Not` node — e.g. `a` forced false via `a iff false`), `not_` interns a fresh term and
bumps the context tag counter, changing later term tags / CNF ordering. So a read of this
"never perturbs solving" API perturbed solving.

**Fix.** Carry polarity OUT OF BAND: the accessors return `(Term.t * bool)` pairs (the atom
+ its asserted polarity) instead of a pre-negated term. No `not_`, no interning, no
mutation. This also respects the freeze — a lookup-only `find_not` would have needed a new
`context.mli` entry, and `context.mli` is frozen. API shape change (no trunk consumer yet;
chc/ #88 not landed):
- `last_unsat_core : (Term.t * bool) list option`
- `last_farkas : (Rational.t * (Term.t * bool)) list option`
`smt/interface/session.ml{,i}`. The internal `conflict_core.atoms` was already
`(Term.t * bool) list`, so this just stops collapsing it.

**Test.** `read-purity` in `session_cores_test.ml`: `q <-> (x<=1); ¬q; x<=0` (forces
`x<=1` false with no interned `Not(x<=1)`); after `Unsat`, records `Context.term_count`,
reads both accessors, asserts `term_count` unchanged; also checks the core carries a
negative-polarity premise (proving the out-of-band channel works).
Load-bearing verified with a probe: this query yields a core with exactly 1 negative
premise whose negation is un-interned, and simulating the old rendering
(`Context.not_ ctx atom` on it) bumps `term_count` 11 → 12 (delta 1) — the old accessor
would have mutated; the fix leaves it at 11.

## Finding 4 (LOW) — over-stated purity claims

Tightened comments/report. The change is NOT allocation-free (the stash allocates a
`Some`/pair/lists on every conflict; the reset adds control flow), so an adversarial
tight-memory run could differ. What is true and load-bearing: the stash is never read
during solving, so no verdict / search counter / CNF-ordering feedback exists — the
byte-identity gate (verdict + all counters) holds, and the read accessors now intern
nothing. Corrected in `smt/interface/session.ml` (accessor header comment) and
`logs/session-cores-report.md` (Soundness bullet, staleness bullet).

## Gates

- `make test` → exit 0 (includes `check-frozen` 14/14 and `session-cores-test`).
- `session-cores-test`: 48/48 checks pass (37 original + 11 new across the 3 regression
  tests).
- `make check-frozen` → `frozen: 14 interface(s) match FROZEN.sha256`, exit 0.
- Byte-identity: fixed tip CLI vs base `c0f37a744a` CLI over the lane's 29-file spot set
  (`tmp-scratch/sc-files.txt`), `--max-effort 5000`, full stdout: **compared=29
  mismatches=0**. The fixes touch only the observational stash (clear timing, adapter
  farkas→None) and the read accessors (out-of-band), none of which is on the solve path.
- `lia-adapter-test` 49/0, `lia-test` 620/0, `cdclt-lemma-test` 14/0.
- RED direction for each new test verified by temporary revert (see per-finding notes).

## Files changed (on top of `bd21406975`)

- `smt/theories/lia/lia_adapter.ml{,i}` — Finding 1 equality→`None`; doc.
- `smt/interface/cdclt.ml{,i}` — Finding 2 `clear_last_conflict` extracted + exported.
- `smt/interface/session.ml{,i}` — Finding 2 top-of-check_sat clear; Finding 3 out-of-band
  polarity (API shape); Finding 4 comment.
- `smt/interface/test/session_cores_test.ml` — pair-shape update + 3 new regression tests.
- `logs/session-cores-report.md` — Finding 4 corrections.

## PIN for re-review

New tip sha: see `git log` on `task/session-cores` after the fixes commit (stated in the
handoff message). Not landed; trunk not moved.
