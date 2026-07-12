# Cert step 2 — replay CHECKER — FREEZE report

**Branch** `task/cert-step2` · **RE-FROZEN after fix round** (off trunk `oxsmt` @ 167a305f2a) ·
awaiting delta re-verify (same-model + codex) + Lean gate. Fix-round sha in the SendMessage.

## Fix round (reviewer REJECT — CRIT-1 accept-invalid + MED-1), append-only on b34096472bb

- **CRIT-1 (the demonstrated accept-invalid) — FIXED.** Learned-clause ordered RUP resolved
  antecedent hint ids against the GLOBAL event index, so a learned clause could cite ITSELF
  or a LATER/mutually-referential learned clause and "verify" out of nothing — certifying a
  satisfiable query as unsat. Fix: `check` threads a **growing verified-learned-id set**;
  `ordered_rup` gets `~learned_verified` and a `Klearned` hint resolves **only if already
  verified** (a strictly-earlier emission index). Inputs / theory leaves are unaffected.
  The LRAT id-monotonicity the code comment claimed is now **enforced, not asserted**.
- **CRIT-2 — both reviewer streams committed as discrimination tests**, each reproduced RED
  against the pre-fix tip first (both returned VALID — confirmed) then INVALID after the fix:
  self-citing learned clauses (empty / trivially-SAT query) → INVALID; mutually-referential
  learned clauses (SAT query `[a]`) → INVALID.
- **MED-1 (RULED) — DONE.** The verdict type now distinguishes
  `Valid_modulo_theory_leaves` (what today's skeleton checker returns — theory leaves
  trusted as axioms this tranche) from a **reserved `Valid`** (full pass incl. verified leaf
  witnesses, not returned until the leaf-checker tranche). `.mli` doc, `string_of_verdict`,
  the corpus gate, and the test harness all updated so a gate cannot silently book a
  skeleton-only pass as fully certified.
- **MED-2** — subsumed by CRIT-1's sound-DB restoration (the verified-id gate keeps every
  cited learned clause genuinely entailed before it can back a downstream chain or terminal).

Post-fix: `checker_test` 36/36, `cert_emit_test` 51/51, corpus gate 24/24 VALID
(now `Valid_modulo_theory_leaves`), frozen guard 14/14, `dune build @fmt` clean.

---

## Original freeze (superseded sha b34096472bb5a4d5b3e5dc7ca7dc2ecc550f0dff)

## What landed

The replay checker that turns the certified-`unsat` gate from *searching* to *checking*.

- **`smt/certificate/checker.{ml,mli}`** — an independent, stdlib-only resolution-skeleton
  checker consuming the step-1 `Recorder` event stream. Verdict = `Valid | Invalid reason |
  Unsupported feature`, fail-closed. Validates:
  - **Input well-formedness / kind-keyed citation resolution** — every cited id resolves to
    exactly one content event *of the kind the site requires*. This is the **#153a debt
    fix**: a `Root_empty` citing a learned event's id now FAILS (the wrong-kind false-clean
    codex found); dangling and ambiguous ids (cross-solver HIGH-4) fail closed.
  - **Level-0 unit closure (§1.3)** — re-derives the closure by confluent BCP over the axiom
    clauses (query/lemma inputs + theory leaves), and every declared `on_unit` must fall
    inside it.
  - **Learned-clause ordered RUP (§1.4)** — each learned clause replays by ordered,
    hint-restricted RUP `[rₙ..r₁; conflict]`; a satisfied / ≥2-free hint breaks the chain
    (reject, never search). Verified clauses fold into an incremental closure (linear in
    practice) so a downstream chain can cite an earlier level-0 learned unit.
  - **Theory leaves = accepted axioms this stage** — `Reason`/`Conflict` leaves and
    `Theory_lemma` inputs are taken as valid T-axioms (their EUF/LIA witness is a later leaf
    tranche); their premises still resolve kind-keyed. An **empty `Conflict` clause**
    (unconditional `T_conflict []`, Rev 6) has no v1 leaf witness → `Unsupported`.
  - **Terminal conclusion (§4.0 E1–E4)** — `Root_empty`/`Level0_conflict` check the cited
    clause is falsified by the closure; `Failed_assumption` (E3) resolves the antecedent
    chain kind-keyed then refutes by seeding the solve's assumption literals true and BCP
    over the verified DB — the **OCaml-side equivalent of the §1.0 selector strip** (an
    assumed-true selector's `¬sel` is false throughout). No separate strip pass is needed to
    *check*; the explicit strip-to-`[||]` is a Lean-bridge concern (step 3).
  - **UNSUPPORTED extension point (marked, not implemented)** for the ADR-0014 Rev-4
    fabric-edge / `Shared_eq` leaf: it is not representable in today's frozen Sat trace
    (roles are exactly `{Reason;Conflict}`), so when the cert format grows that leaf it must
    route to the guarded `Unsupported` branch and land as its own reviewed tranche.

- **`smt/certificate/test/checker_test.ml`** (34 checks, 0 failures) — every honest
  E1–E4 / theory-reason / ordered-RUP / crit1–3 / high4 stream is VALID; **one
  discrimination test per corruption class** (dropped hint, permuted hints where order
  matters, wrong antecedent set, forged citation KIND, ambiguous cross-solver id, truncated
  stream, dangling id), each proven to FLIP a VALID baseline to INVALID; empty-`Conflict`
  → UNSUPPORTED; **exact antecedent-SET (and order) assertions** on a real chain (the
  **#153b debt fix**, superseding cert_emit_test's length-only checks).

- **`tests/certificate/cert_corpus_gate.ml`** (`make cert-corpus-gate`) — end-to-end gate
  driving real `.smt2` through the shipped `Session` with a recorder on the inner SAT core.

- **`smt/interface/session.{ml,mli}`** (non-frozen) — additive `install_cert_trace` /
  `cert_assumptions` / `failed_assumptions` hooks. **`sat.mli` UNTOUCHED** — the frozen
  guard passes (14/14). Zero frozen-surface change, as required.

## Corpus numbers (honest)

| sample | unsat solves | VALID | INVALID | UNSUPPORTED | repeat re-emit VALID |
|---|---|---|---|---|---|
| `tests/cases` (42 files, in-repo) | 24 | **24** | 0 | 0 | 24/24 |
| QF_UF `eq_diamond` 1–20 (smallest) | 17 | **17** | 0 | 0 | 17/17 |

**41 real session `unsat` certs check VALID, 0 INVALID, 0 UNSUPPORTED**, across pure-Bool,
EUF, LIA, mixed UFLIA, and QF_UF eq_diamond — repeat-solve re-emission VALID on all. No
emission bugs surfaced by the checker. Non-unsat files (sat/unknown) are skipped; a handful
of larger eq_diamond (≥18) hit the effort cap and book as Unknown/skip (see finding below).

## Findings / scope notes for review

1. **Every session `Unsat` exits via E3** (the base frame is selector-guarded,
   `session.ml`). E1/E2/E4 are unreachable through the session and are exercised only by the
   raw-Sat fixture suite. The corpus gate therefore stresses E3 + theory-reason
   materialization; the four-exit terminal coverage lives in `checker_test`.
2. **Traced solve is a weaker solver** (minimization bypassed, ADR §1.4(b)). It has a search
   cliff — e.g. `eq_diamond18`+ blow up under the traced config. The gate caps effort
   (`max_effort`) so a cliff instance becomes a clean `Unknown`/skip, never a hang; this is
   exactly the "generous CI budget / traced-config timeout booked uncertified" resolution
   the ADR anticipated, and is not a soundness signal.
3. **Theory-leaf witness is deferred by design** (task item d): the checker trusts theory
   `Reason`/`Conflict` leaves as axioms at this stage. Its guarantee is the resolution
   skeleton: *if the theory leaves are valid, then `[||]` follows*. Leaf-witness checking
   (EUF proof trees, Farkas multipliers) is the next tranche (ADR §4.1 step 2).
4. **E3 derivation is BCP-refutation, not ordered-RUP over the antecedent chain.** The
   recorded antecedents are resolvability/kind-checked, but the *derivation* is confirmed by
   confluent BCP over the verified DB seeded with the assumptions. This is robust to the
   common level-0-failure case where `analyze_final` backjumped to level 0 and emitted `[]`
   antecedents (the forcing lives in the verified learned clauses). The explicit
   ordered-`[||]` chain is what the Lean cert consumes at step 3.

## Gate wiring

`make checker-test` (deterministic fixtures) and `make cert-corpus-gate`
(`CORPUS_GATE_DIRS=tests/cases` by default; pass a dir to widen) are both added to
`make test`.

## TCB path

Treat every checker rule as soundness-critical. Full dual review + Lean gate at land.
