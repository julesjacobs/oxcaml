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
