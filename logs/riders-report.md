# riders lane — FREEZE report (re-frozen: item 5 added)

Branch `task/riders` off trunk `oxsmt` @ 167a305f2a. Five small independent board rows,
one commit each (reviewable individually). No frozen `.mli` touched (`make check-frozen`:
14 interfaces match). All discrimination verified by neutering the fix and observing the
test go RED.

## Commits (in order)

1. `f24617a58d` — #118 LIA empty-premise tripwire
2. `bb7e52fe85` — #122 widen term_has_reserved to sort-carried symbols
3. `7c6b8b6b6f` — #161 predicate late-binding pop recurrence (check-time re-arm)
4. `9b7a2b6f76` — #152(c) rewrite u_dedup_rollback via the seed path
5. `30b52d32c9` — #152(item5)/issue5144 reset/reset-assertions fail-closed

---

## Item 1 — Board #118: LIA empty-premise conflict tripwire (parity with EUF AP4)

- **What.** EUF's codex AP4 tripwire (euf_adapter.ml) is an UNCONDITIONAL
  `if premises = [] then failwith "...[codex AP4 tripwire]"` on both the propagation-reason
  and conflict paths — survives release `-noassert`, degrades to unknown via
  CONTRACT-POISON. LIA had no equivalent.
- **Why.** A premise-free LIA conflict fed to 1UIP learns the empty clause → spurious
  `unsat`; a premise-free propagation is an unconditional entailment. Both are soundness
  bugs and must fail-close, matching AP4 (failwith, not a compiled-out `assert`).
- **How.** `checked_premises what premises` in lia_adapter.ml, wired into the two
  premise-carrying paths: `conflict_explanation` (Lia_farkas) and the new
  `propagation_reason` (Lia_bound, factored out of `propagations`). Both builders exposed in
  the (non-frozen) `lia_adapter.mli` so the test drives the tripwire's own path.
- **How tested / discrimination.** `test_empty_premise_tripwire` (lia_adapter_test.ml):
  happy path (non-empty builds with the right rule tag) + two `check_raises` on empty
  conflict / empty propagation. Neutering `checked_premises` to `ignore what; premises`
  fails EXACTLY the 2 tripwire checks (49 checks, 2 failures); happy path stays green →
  the test drives the defect's own path, not just a property.

## Item 2 — Board #122: widen term_has_reserved to sort-carried symbols

- **What.** `term_has_reserved` (session.ml) walked only `App`-head symbols; a `Sort` is
  `Uninterpreted of Symbol.t` and never inspected.
- **Why.** `Env.declare_fun`/`Session.declare_const` check only the NAME, not the rank's
  sorts; `Symbol.intern`/`Sort.uninterpreted` are public. A client can declare a const of a
  reserved `.oxsmt.*` uninterpreted sort with an innocuous head — a term whose ONLY reserved
  symbol is sort-carried — and slip the reserved-name gate (the R1/C1 capture door).
- **How.** Added a `bad_sort` check and `bad_sort t.sort ||` at the top of the recursive
  walk; every subterm's sort is checked, so a reserved sort in result OR argument position
  is caught. No `allowed` whitelist for sorts (a reserved uninterpreted sort is never a
  legit qvar, and nothing internal mints one — verified: no `Sort.uninterpreted` with a
  reserved name across interface/preprocess/combine/theories). Entirely in session.ml;
  `env.mli` (the only frozen core `.mli` here) untouched.
- **How tested / discrimination.** `sort_carried_gate` (lemma_honeypot_test.ml): a matched
  pair over the model-free UNSAT shape `c1<>c2 /\ c1=c2` (no model → the M2 `.oxsmt.*`
  model-filter can't independently mask the difference). User-sort control → Unsat;
  reserved-sort → Unknown; only sort reservedness differs. Disabling `bad_sort t.sort ||`
  flips the reserved case to a wrong `unsat` (16/17); control stays green.

## Item 3 — Board #161: predicate late-binding pop recurrence

- **What.** A late-bound predicate's propagation is lost AGAIN after a pop below the binding
  frame.
- **Why.** The register-time watch re-arm (existing test 3d) is TRAILED (euf.mli), so a pop
  below the binding restores the bound predicate watch's stale `w_reported`, while the atom
  binding survives (`t.watched` is monotone, not trailed) → the engine won't re-report the
  still-entailed truth. Entailing facts at base survive the pop; only the binding + its
  re-arm were popped. Eq atoms are immune (bound before any report; `w_reported` restoration
  tracks the entailing merge).
- **How.** Check-time idempotent, pop-proof re-arm:
  - euf.ml/euf.mli: new `rearm_watches_if : 'p t -> (Term.t -> bool) -> unit` — one
    O(#watches) pass (vs O(#predicates × #watches) per-term).
  - euf_adapter.ml: `predicates_maybe_stale` bool set by `pop`; `check` (before Euf.check)
    does one re-arm pass over BOUND predicate watches whose atom has NO live cached
    propagation (`stale_bound_predicate`), then clears the flag. A check with no intervening
    pop does zero work. Re-arm is idempotent (propagate recomputes: delivers if still
    entailed, nothing if the entailing merge was itself popped). euf.mli/euf_adapter.mli are
    NOT frozen; the euf.mli edit is additive.
- **How tested / discrimination.** `test_predicate_latebind_pop_recurrence` — 3 cases:
  (i) codex repro true-valued, (ii) false-valued, (iii) two-level push with binding after
  the pushes + pop of both. Each asserts propagation at the binding frame (register-time
  re-arm, unchanged) AND re-propagation after pop-below-binding + a sound recovered
  explanation. Disabling the check-time re-arm (keeping the flag read) fails "RE-propagated
  after pop-below-binding" and crashes the recovered-explain on the missing cache; the
  "propagated at binding frame" check stays green → isolates the NEW recurrence, not the
  existing 3d behavior. euf-adapter 1493/0, euf 6412/0 restored.

## Item 4 — Board #152(c): rewrite u_dedup_rollback via the seed path (was vacuous)

- **What.** The existing `u_dedup_rollback` never exercised the dedup-rollback branch.
- **Why.** It used a TRIGGER-based lemma over 100 candidates with gen_budget 3.
  `Matcher.substitutions` debits the budget INSIDE its enumeration (matcher.ml:92) and
  raises `Budget_exhausted` BEFORE returning, so manager `process` never runs → `added=[]` →
  the dedup rollback (manager.ml:156) is never reached.
- **How.** Rewrote through the empty-trigger seed path (like `u_seed_rollback`): empty
  triggers make the matcher a no-op, so the budget is spent INSIDE `process` as the seed
  queue drains. gen_budget 3, seeds s0..s4: round 1 processes s0,s1,s2 INTO dedup then
  aborts on s3 (non-vacuous), rolling their dedup entries back + restoring seeds; round 2
  (budget reset, restored set) must re-attempt the rolled-back instances and re-hit the
  budget. Test-only change; no source touched.
- **How tested / discrimination.** 4 checks (round-1 abort + empty batch; round-2 re-abort +
  empty batch). Disabling ONLY the dedup rollback (manager.ml:156) leaves s0,s1,s2
  suppressed, so round 2 skips them and drains s3,s4 within budget → does NOT abort; both
  round-2 checks flip RED (24/3). With the rollback: 27/0. The discrimination itself proves
  non-vacuousness (process ran and polluted dedup in round 1).

## Item 5 — Board #152 item 5 / issue5144: reset/reset-assertions fail-closed

- **What.** parser.ml treated `set-option | reset | reset-assertions` as a SILENT no-op.
- **Why.** `reset`/`reset-assertions` clear the assertion set mid-script; this batch reader
  folds every `assert` into ONE set for a single `check-sat`, so silently ignoring them
  leaves the pre-reset assertions live → wrong verdict. Confirmed end-to-end:
  `(assert (= 0 1)) (reset-assertions) (check-sat)` returns `unsat` pre-fix (WRONG — the
  contradiction is reset away; should be sat/unknown), `unknown` post-fix.
- **Audit evidence.** Matches the regress-harness reviewer's independent finding
  (`logs/regress-harness-review.md`, regress lane) that reset/reset-assertions is the ONLY
  verdict-flipping silent no-op in the dispatch. This parser fix is the ROOT layer; the
  regress lane separately added a DRIVER-side backstop (corpus_classify degrades reset* to
  unknown-incremental) plus a fixture exercising both layers.
- **How.** Split the match arm: `reset`/`reset-assertions` now `unsupportedf` → the CLI's
  `solve_batch` catches `Parser.Unsupported` (oxsmt_cli.ml:169) and returns `unknown_block`
  (I8 fail-closed). Audit of the same arm: `set-option` and `get-model|get-value|
  get-unsat-core` are output-only / non-stateful (cannot change the assertion set) → kept as
  no-ops; `push`/`pop` were already `unsupportedf`. reset* were the only verdict-flipping
  silent no-ops.
- **How tested / discrimination.** `command_gate_cases` (roundtrip_test.ml): the exact
  issue5144 shape via `check_unsupported` (reset-assertions + reset) + a `check_parses_ok`
  positive control proving set-option/get-* still parse. Neutering the reset arm back to a
  no-op reddens both `check_unsupported` checks (163→2 fail) and flips the CLI to `unsat`;
  positive control stays green. Restored → 163/0.

---

## Gate (all components of `make test`, run individually — `make test` itself invokes a
sandbox-denied `timeout`)

- `make check-frozen` — 14 interfaces match FROZEN.sha256
- harness_test — all checks passed; run_harness — 47/47 PASS
- combine-test 87/0; smtlib-test (roundtrip 163/0 + fuzz-lex 4000/0); lemma-test
  (honeypot 17/0 + crit_repro + matcher 27/0); cert-test 51/0; driver-equiv-test 48 files,
  0 divergence
- affected suites: lia-adapter 49/0, euf-adapter 1493/0, euf 6412/0
- `dune build @fmt` (non-promoting) exit 0; `dune build` exit 0

FROZEN (re-frozen after item 5).
