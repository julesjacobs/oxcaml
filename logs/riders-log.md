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
