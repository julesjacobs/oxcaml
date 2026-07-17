# Task #106 — Session API: theory unsat core / Farkas dual coefficients

Branch `task/session-cores`, off trunk `c0f37a744a`.
Commits: `eab92720f3` (API layer), `bb8ee4fe2a` (consumer-proof test), + report/wiring.

## What was built

An additive, **observational** public Session surface exposing the most recent LIA theory
conflict's evidence after an UNSAT `check_sat`:

```
Session.last_unsat_core : t -> (Term.t * bool) list option
Session.last_farkas     : t -> (Oxsmt_lia.Rational.t * (Term.t * bool)) list option
```

- `last_unsat_core` — the premise literals of the refuting LIA conflict, each rendered as
  the Bool `Term.t` asserted true (the atom, or its negation for a negative premise). Their
  conjunction is theory-unsatisfiable.
- `last_farkas` — `(coeffᵢ, litᵢ)` pairs: `coeffᵢ ≥ 0` is the dual multiplier for the
  asserted half-plane `litᵢ`, and `Σ coeffᵢ · half-plane(litᵢ)` is a variable-free false
  constant (the rational-infeasibility proof), index-aligned with the core.

### Plumbing (all additive; no frozen `.mli` touched — `check-frozen` 14/14)

The Farkas vector never rides the frozen, payload-free `Explanation` (ADR-0006). Instead a
separate read-only channel is populated at conflict-production time:

1. `smt/theories/lia/lia_adapter.ml` — new `conflict_core` type + `mutable last_conflict`
   field; `fabric_conflict_explanation` stashes the raw `(farkas, premises)` of every
   solve-path conflict (the `Lia.check` Farkas conflict AND the `diophantine_conflict`),
   after the empty-premise tripwire. `last_conflict_core` maps premise tokens → `(term,
   polarity)`; `clear_last_conflict` resets it.
2. `smt/combine/combine.ml{,i}` — `arith_state : t -> B.t`, symmetric to the existing
   `congruence_state`, to reach the LIA child inside the combinator.
3. `smt/interface/cdclt.ml{,i}` — `last_conflict_core` passthrough (only the EUF+LIA stack
   carries it; DT/arrays → `None`); the stash is cleared unconditionally at the top of
   `check_sat` (via `clear_last_conflict`, so the pure-BV fast path that bypasses
   `begin_check` cannot leak a stale core).
4. `smt/interface/session.ml{,i}` — the two public functions, gated on `last_verdict =
   Unsat`.

## Soundness

- **Observational / non-perturbing.** The always-on changes are (a) a write to a new
  mutable field at conflict production and (b) a `None` reset per check-sat. This is NOT
  allocation-free — the stash allocates a `Some`/pair/lists on every conflict, and the
  reset adds a small amount of control flow (so an adversarial tight-memory run could
  differ). What IS true and is the load-bearing guarantee: the stash is never read during
  solving, so no verdict, search counter, or CNF ordering feeds back — the byte-identity
  gate below (verdict + all counters) holds. The read accessors intern nothing (polarity
  carried out of band; see fixes report).
- **The core is a genuine theory-unsat core** by construction (a conflict's premises are
  T-infeasible). Consumers must still re-check on a fresh Session (the test does); a wrong
  core cannot arise but the discipline guards against consumer misuse.
- **Wrong-evidence firewall.** Unmappable premises (an EUF-congruence fabric-edge handle,
  or an atom absent from the term map) → `None` rather than a partial/misleading core. A
  Farkas coefficient vector whose length disagrees with the premises → coefficients
  suppressed (never misaligned).

## Consumer proof — `smt/interface/test/session_cores_test.ml` (37/37 checks pass)

Run: `make session-cores-test` (wired into `make test`).

1. **Core soundness** (positive + negative-premise conflicts): returned atoms re-checked
   `Unsat` on a fresh Session.
2. **Farkas certificate arithmetic**: `Σ coeffᵢ · half-plane(litᵢ)` recombines to a
   variable-free **strictly-positive constant** (every variable coefficient cancels) — the
   `0 < c` contradiction. The negative-premise case exercises the ℤ-complement half-plane
   (`¬(t≤0) ≡ -t+1 ≤ 0`).
3. **Farkas interpolation (the decisive CHC use).** On a counterexample-to-induction-shaped
   query — A = `{xa ≤ 0, s ≤ xa}` (frame+transition, A-local `xa`, shared `s`), B = `{s ≥
   1}` — the A-side of the certificate is summed into a single inequality `I`, and verified
   on fresh Sessions: **A ⊨ I** (`A ∧ ¬I` Unsat), **`I ∧ B` Unsat**, and **`I` mentions
   only the shared variable `s`** (the A-local `xa` cancels). This is a real McMillan LA(ℚ)
   interpolant, produced from the API alone — exactly what `chc/` needs to replace its
   template-proxy interpolants (see `logs/chc-solver-report.md` interpolation section).
4. **Diophantine** (`2x = 1`, ℚ-feasible / ℤ-infeasible): core present, `last_farkas` =
   `None` (certified by the GCD/divisibility argument, no rational multiplier vector).
5. **Gating**: `None` before any check, on `Sat`, and on a purely propositional `Unsat`.
6. **Staleness**: a check-1 theory conflict does NOT leak into a later check-2 propositional
   `Unsat` (the stash is cleared at the top of `check_sat`) — the decisive test that
   discriminates the per-check reset. (Post-review: also covered for the pure-BV fast path;
   see the fixes report.)

## Byte-identity measurement (observational neutrality)

Built the solver CLI at base `c0f37a744a` (isolated worktree `session-cores-base`) and at
my tip, ran both over **50 small QF_LIA / QF_UFLIA files** (the LIA path is where the stash
writes happen) with `--max-effort 5000`, comparing full CLI output (verdict + conflicts +
decisions + propagations):

```
RESULT compared=50 mismatches=0
```

Zero divergence in verdict or any counter — the API is byte-identical to base when unused,
as expected from a write-only observational field.

Gates: `check-frozen` 14/14; whole project builds clean (native + the new test).

## Coverage / honest limits

- **Farkas coefficients are surfaced for rational-infeasible conflicts** (the `Lia.check`
  Farkas path) — the interpolating case, and the shape a CHC CTI query produces. A
  Diophantine conflict surfaces the core but no coefficients (`last_farkas = None`); a
  consumer wanting a divisibility interpolant reads `last_unsat_core` and reasons about the
  GCD separately.
- **The evidence is the MOST RECENT LIA conflict.** For a conjunctive query refuted at
  decision level 0 (the CTI shape) that is the whole-query refutation, so the core is the
  clashing asserted-formula subset. For a query with Boolean structure it is the last theory
  lemma the search derived — still a sound theory core, but not necessarily the minimal
  whole-formula core. Documented in `session.mli`.
- **Combination premises.** A premise that is a fabric-edge handle (an EUF-entailed equality
  crossing into LIA) has no single atom term, so a conflict citing one yields `None`. Pure
  QF_LIA and the CTI conjunctive queries never hit this; a mixed UFLIA conflict can. Extending
  coverage (expanding fabric handles to their underlying trail literals via the combinator's
  `expand_justifications`) is the natural next increment if a consumer needs it.

## Handoff to `chc/` (task #77/#88)

`Session.last_farkas` + `last_unsat_core` deliver the Farkas dual coefficients the CHC
interpolation lane priced as its ceiling. The interpolant-construction recipe is proven in
`session_cores_test.ml` §3 (partition the certificate by asserted origin; sum the A-side;
clear denominators; the result is an interpolant over shared variables). `chc/` is not yet
on trunk, so the consumer proof is self-contained on the Session API rather than wired into
`chc/test`; when `chc/` lands, its relational decisive tests can call these directly.
