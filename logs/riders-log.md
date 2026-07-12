# riders lane log (task/riders off 167a305f2a)

Batch of four small independent board rows, one commit each.

## Item 1 — Board #118: LIA empty-premise conflict tripwire (parity with EUF AP4)

- EUF AP4 (euf_adapter.ml:163-201) is an UNCONDITIONAL `if premises = [] then failwith
  "...[codex AP4 tripwire]"` in both the propagation-reason and conflict paths — survives
  release `-noassert`, degrades to unknown via CONTRACT-POISON. Matched exactly (failwith,
  not assert).
- LIA parity: added `checked_premises what premises` in lia_adapter.ml, wired into the two
  premise-carrying paths: `conflict_explanation` (Lia_farkas) and new `propagation_reason`
  (Lia_bound, factored out of `propagations`).
- Exposed `conflict_explanation` + `propagation_reason` in the (non-frozen) lia_adapter.mli
  so the test can drive the tripwire's own path directly. FROZEN.sha256 has only env.mli.
- Test: `test_empty_premise_tripwire` in lia_adapter_test.ml — happy path (non-empty builds
  with right rule tag) + two discriminating `check_raises` (empty conflict / empty prop).
- Discrimination VERIFIED: with `checked_premises` neutered to `ignore what; premises`,
  exactly the 2 tripwire checks go RED (49 checks, 2 failures); happy path stays green.
  Restored → 49 checks, 0 failures.

## Item 2 — Board #122: widen term_has_reserved to sort-carried symbols

- `term_has_reserved` (session.ml) walked only App-head symbols. But a Sort is
  `Uninterpreted of Symbol.t`, and `Env.declare_fun`/`Session.declare_const` check only the
  NAME, not the rank's sorts; `Symbol.intern`/`Sort.uninterpreted` are public. So a client
  can declare a const of a reserved `.oxsmt.*` uninterpreted sort with an innocuous head —
  a term whose ONLY reserved symbol is sort-carried — and slip the gate.
- Fix: added `bad_sort` + `bad_sort t.sort ||` at the top of the recursive walk. Every
  subterm's own sort is checked, so a reserved sort in result OR argument position is caught.
  No `allowed` whitelist for sorts (a reserved uninterpreted sort is never a legit qvar, and
  nothing internal mints one — verified: no `Sort.uninterpreted` with a reserved name in
  interface/preprocess/combine/theories).
- env.mli is the ONLY frozen .mli; this change is entirely in session.ml — no .mli touched.
- Test: `sort_carried_gate` in lemma_honeypot_test.ml. Matched pair over the model-free
  UNSAT shape `c1<>c2 /\ c1=c2` (no model → M2 filtering can't mask): user-sort control →
  Unsat; reserved-sort → Unknown. Only sort reservedness differs.
- Discrimination VERIFIED: disable `bad_sort t.sort ||` → sort-carried case returns Unsat
  (RED, 16/17), control stays green. Restored → 17/17.

## Item 3 — Board #161: predicate late-binding pop recurrence

- Bug: the register-time re-arm (euf_adapter, existing test 3d) is TRAILED (euf.mli:131), so
  a pop BELOW the binding frame restores a bound predicate watch's stale w_reported while the
  atom binding survives (t.watched is monotone, NOT trailed) — the propagation is lost again.
  Eq atoms are immune (bound at register, before any report; w_reported restoration tracks
  the entailing merge). Entailing facts at base survive the pop; only the binding + its
  re-arm were at the popped frame.
- Fix (check-time idempotent re-arm, pop-proof):
  - euf.ml/euf.mli: new `rearm_watches_if : 'p t -> (Term.t -> bool) -> unit` — one
    O(#watches) pass re-arming every matching watch (vs O(#predicates x #watches) per-term).
  - euf_adapter.ml: `predicates_maybe_stale` bool set by `pop`; `check` (before Euf.check)
    does one re-arm pass over BOUND predicate watches whose atom has NO live cached
    propagation (`stale_bound_predicate`), then clears the flag. A check with no intervening
    pop does zero work (single-bool gate). Re-arm is idempotent: propagate recomputes the
    truth (delivers if still entailed, nothing if the entailing merge was itself popped).
  - euf.mli/euf_adapter.mli NOT frozen (only core/*.mli + sat.mli). euf.mli edit is additive.
- Test: `test_predicate_latebind_pop_recurrence` — 3 cases: (i) codex repro true-valued,
  (ii) false-valued, (iii) two-level push with binding after the pushes + pop of both. Each
  asserts propagation at the binding frame (register-time re-arm, unchanged) AND re-propagation
  after pop-below-binding + a sound recovered explanation.
- Discrimination VERIFIED: disable the check-time re-arm (keep the flag read) → "RE-propagated
  after pop-below-binding" fails and the recovered-explain crashes on the missing cache; the
  "propagated at binding frame" check stays green (isolates the NEW recurrence). Restored →
  1493/0 (adapter), 6412/0 (engine).

## Item 4 — Board #152(c): rewrite u_dedup_rollback via the seed path (was vacuous)

- Vacuousness confirmed: the old test used a TRIGGER-based lemma over 100 candidates with
  gen_budget 3. Matcher.substitutions debits the budget INSIDE its enumeration
  (matcher.ml:92) and raises Budget_exhausted BEFORE returning, so manager `process` never
  runs → `added=[]` → the dedup-rollback branch (manager.ml:156) was never exercised.
- Rewrite (empty-trigger lemma + seeds, matching u_seed_rollback): empty triggers make the
  matcher a no-op, so budget is spent INSIDE `process` as the seed queue drains. gen_budget
  3, seeds s0..s4: round 1 processes s0,s1,s2 INTO dedup then aborts on s3 (non-vacuous),
  rolling back their dedup entries + restoring seeds. Round 2 (budget reset, restored set)
  must re-attempt the rolled-back instances and re-hit the budget.
- Discrimination VERIFIED: disable ONLY the dedup rollback (manager.ml:156) → round 2 skips
  the still-suppressed s0,s1,s2 and drains s3,s4 within budget → does NOT abort; both round-2
  checks flip RED (24/3). With the rollback: 27/0. (Test-only change; no source touched.)

## Item 5 — Board #152 item 5 / issue5144: reset/reset-assertions fail-closed (was silent no-op)

- Bug: parser.ml:422 treated `set-option | reset | reset-assertions` as a SILENT no-op.
  reset/reset-assertions clear the assertion set mid-script, but this batch reader folds
  every `assert` into ONE set for a single check-sat, so ignoring them leaves the pre-reset
  assertions live → verdict flip. Confirmed end-to-end: `(assert (= 0 1)) (reset-assertions)
  (check-sat)` yields `unsat` pre-fix (WRONG; should be sat/unknown), `unknown` post-fix.
- Fix: split the arm — `reset`/`reset-assertions` now `unsupportedf` (fail-closed →
  Parser.Unsupported → CLI solve_batch catches at oxsmt_cli.ml:169 → unknown_block, I8).
  Audit of the same match arm: `set-option` and `get-model|get-value|get-unsat-core` are
  output-only/non-stateful (can't change the assertion set) → kept as no-ops; `push`/`pop`
  already unsupported. So reset* were the only verdict-flipping silent no-ops.
- Test: `command_gate_cases` in roundtrip_test.ml — the exact issue5144 shape via
  `check_unsupported` (reset-assertions + reset) + a positive control `check_parses_ok`
  proving set-option/get-* still parse (not spuriously degraded).
- Discrimination VERIFIED: neuter the reset arm back to a no-op (`ignore c`) → both reset
  check_unsupported checks go RED ("parsed OK"), positive control stays green (163→2 fail);
  and the CLI yields `unsat` (concrete verdict flip). Restored → 163/0.
