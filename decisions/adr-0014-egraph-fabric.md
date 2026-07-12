# ADR-0014 — Theory–e-graph fabric: theories interact through a shared congruence hub, not propositional round-trips

Status: **RATIFIED (Rev 6.1) 2026-07-12** — design-only; the implementation is the staged fabric dispatch (Stage 0 landed on trunk at 7dd331ac74; Stage 1a+ tracked). Promoted verbatim from `logs/adr-egraph-fabric-draft.md` at Rev 6.1; the original DRAFT Rev-6.1 status block below is preserved as-ratified for the record. Dual adversarial review complete (same-model + codex final-confirm legs across Rev 5 → 6 → 6.1); master-ratified.

- **Rev-6.1 pin (reviewer-dictated, no round — the qg-intkey-rider class; codex FINAL-CONFIRM
  Item 3).** `propagated_by` entries for fabric-reason literals are **origin-frame TRAILED** — removed
  on `pop` of their origin frame, exactly like the child `explain_cache`s they mirror
  (`euf_adapter.cache_reason` push `euf_adapter.ml:184-186` / uncache-on-pop `:262`). Today
  `propagated_by` is grow-only and UNTRAILED (`combine.ml:89`, `record_props` `:399-400`; `Combine.pop`
  `:632-649` unwinds only `pin_frames`), so first-wins (Rev6-3) on an untrailed map would leave a
  **stale post-pop owner** that blocks the correct re-record and makes `Combine.explain` route a
  post-pop re-propagation to the wrong child — wrong-owner 1UIP input on the #102 surface. Trailing it
  closes that. (§B.2, §B.8.)
- **Status:** DRAFT Rev 6.1 — the FINAL revision round (team-lead convergence rule: after Rev 6,
  ratification is the master's decision on the full two-leg record; the ADR may carry named
  forward-obligations gating later stages, the cert-ADR precedent; no Rev 7 unless a reviewer finds
  a NEW soundness contradiction in the Rev-6 deltas themselves). Rev 5 closed B3 (witness math,
  codex-confirmed CLOSED); the codex ratification leg held the other five PARTIAL — two were
  driver-confirmed internal contradictions from incomplete Rev-5 edit propagation, three were
  under-specifications on the Rev5-B1 machinery. Rev 6 folds four now-items (contradictions +
  cache-at-propagation + acyclicity enforcement) and converts three genuinely-Stage-4 residuals to
  named forward-obligations. All of Rev 2 (R1–R10 + MED-5), Rev 3 (C1–C6/H1–H5/M1–M4), Rev 4 (DELTA
  residuals), Rev 4.1 (chokepoint completeness), Rev 5 (typed child-explanation currency + 6
  blockers), and Rev 6 (below) are in this single file. Design-only; no implementation dispatches
  until ratified. Repo-external draft; the integrator promotes the ratified text to
  `decisions/adr-0014-egraph-fabric.md` at land.

## Rev-6 changelog (final round — codex RATIFICATION residuals; source-verified; per-item)

- **Rev6-1 INTERNAL CONTRADICTION fixed — §C Stage 0 item 4 no longer says "replay on pop"
  (codex Item 3, driver-confirmed).** Item 5 (Rev5-B4) removed the hub's replay-of-the-child-undo-log
  on `pop` (the double-rewind authority), but item 4's co-location paragraph still described exactly
  that ("`pop` of the merge replays the theory's undo log to that watermark"). Rev 6 rewrites item 4
  to match item 5: a merge-combine's theory-local mutations ride the child's own trail in the merge's
  frame and are reversed by the **child's own frame `pop`** (single owner); the hub records no
  watermark for pop and does not replay on pop. Added the missing rule codex named: **callback-driven
  mutations land in the merge's own frame — no child-local temporary frame during callback drain** (so
  a mutation cannot land in a different frame than the merge that caused it). (§C Stage 0 item 4)
- **Rev6-2 RESIDUAL DISEQ PROMISES swept — §0.1a, F1, §A.7 diagram (codex Item 5, driver-confirmed).**
  Rev5-NEW-HIGH scoped the fabric to equality-only through Stage 3 in §A.2/§B.7/F7 but left three
  normative statements still advertising disequality injection: §0.1a "(dis)equality," F1 "equality
  or disequality a theory injects," and the §A.7 diagram (`assert_neq ~premise:Γ` + "inject
  eq/diseq"). All three now say equality-only through Stage 3 with the diseq-injection delta named.
  The A.7 arrow label is corrected too (same-model cosmetic note). (§0.1a, §B.1 F1, §A.7)
- **Rev6-3 CACHE DISCIPLINE — combined-reason cache is at-PROPAGATION, not at-explanation
  (codex NEW-HIGH-3, the #102-adjacent one).** §B.2's new `Combine`-layer combined-reason cache
  keyed "on first expansion" — but the #102 discipline (§B.8) requires snapshot-**at propagation
  time**. A child can re-propagate the same literal before its first explanation and (with last-wins
  `propagated_by`) a later, precedence-invalid reason would be cached. Rev 6: the `Combine` cache
  entry for a fabric-derived reason is recorded when the propagation is **RETURNED** (at
  `check`/propagate time — the same instant the child snapshots its own `explain_cache`), FIRST-wins,
  origin-frame trailed; `propagated_by` is also first-wins for a fabric-reason literal. §B.2 and §B.8
  aligned; the existing #102-consequence mutant covers it (stated). (§B.2, §B.8)
- **Rev6-4 ACYCLICITY ENFORCED, not asserted (codex NEW-HIGH-2).** Recursive handle expansion was
  bounded by "the acyclic edge DAG," but the F1(b) ledger orders `Lit.t` arrivals, not `edge_id`s.
  Rev 6 adds the mechanism: **`edge_id`s are allocated by a monotone counter, and an injected edge's
  `Γ` may reference only STRICTLY-SMALLER, still-live `edge_id`s — checked at injection (one
  comparison per referenced handle)**; expansion uses a visited-set and fails closed (→ `unknown`)
  on a missing/larger handle. Invariant + mutant (inject an edge whose `Γ` cites a not-smaller
  handle → must be rejected at injection, not loop at expansion). (§B.2)
- **Rev6-FWD forward-obligations named (do NOT design now; the cert-ADR/#153 precedent).** Three
  Rev-5 residuals are genuinely Stage-4 work and are recorded as numbered gate obligations, not
  designed here: **FWD-1** N-child fabric composition (binary `Combine (R)(A)(B)` cannot nest a
  Stage-4 datatype/array child as a `FABRIC_CHILD` on the shared hub — a Stage-4 packaging
  unfreeze/redesign, §C Stage 4 gate); **FWD-2** value-validator SEMANTIC completeness (Rev5-B5b's
  validator checks shape; canonical/structural DT equality + extensional array equality with
  canonical `select`/`store` evaluation are a Stage-4 obligation on the validator spec, §C Stage 4
  gate); **FWD-3** A4 size-metric accounting (a `Both`-routed pure-arith Int equality registers its
  full closure into EUF via `A.register_atom`, `combine.ml:345-347`, so the size metric must count
  EVERY congruence-child registration root, not only maximal UF applications — a metrics correction,
  §B.5a). (§C Stage 4, §B.5a, §E)

## Rev-5 changelog (the four codex CONFIRM blockers + diseq-replay HIGH + A4-metric MED; source-verified)

- **Rev5-B1 THE HEADLINE — the fabric currency is not typeable through the frozen `THEORY`
  boundary; a NON-FROZEN internal child-explanation interface (`FABRIC_CHILD`) carries it (§B.2).**
  Rev 4/4.1 said "both children instantiate the engine's opaque token to `Real of Lit.t | Fabric
  of edge_id`, and `Combine` expands." Source-checked, that names only the *engine↔adapter* layer
  and cannot be typed at the *adapter→Combine* layer: a child is driven as a **frozen**
  `Theory.THEORY` (LIA is literally `B : Theory.THEORY`, `combine.ml:23`), whose `check` returns
  `Conflict of Explanation.t` / `Propagations of Lit.t list` (`theory.mli:30,33`) and whose
  `explain` returns `Explanation.t` with `premises : Lit.t list` (`explanation.mli:31-33`) — **a
  `Fabric edge_id` cannot cross any of those.** Rev 5 designs the widening the `CONGRUENCE_CHILD`
  precedent (`combine.mli:175-183`) already establishes: a non-frozen `FABRIC_CHILD` sig that
  `include Theory.THEORY` and adds fabric-aware `check`/`explain` returning a richer
  `Fabric_explanation.t` (premises `justification list`); `Combine` drives BOTH children through
  it (`Combine (R) (A : FABRIC_CONGRUENCE_CHILD) (B : FABRIC_CHILD)`), maintains the `edge_id → Γ`
  registry + a **first-wins combined-reason cache**, expands **recursively** (a `Fabric` handle's
  `Γ` may itself hold `Fabric` handles — bounded by the acyclic edge-dependency DAG the
  assertion-order ledger enforces), at **both** seam-return points (`Combine.check`,
  `Combine.explain`). Every type is verified spellable against the current `.mli`/functor
  structure. (§B.2, §B.4)
- **Rev5-B3 F1-SEM witness math corrected — TWO oriented Farkas derivations, not a four-bound sum
  (§B.1).** The four unit-multiplier bounds `{s≥v, s≤v, w≥v, w≤v}` summed are the tautology
  `0 ≤ 0`, not a witness for `s=w` (codex, mathematically correct). `s=w` needs two *oriented*
  derivations: `s≤w` from `{s≤v, w≥v}` and `s≥w` from `{s≥v, w≤v}`. `Lia.fixed_bounds` must return
  **ACTIVE EXACT** bounds (both `s≥v` and `s≤v` asserted and tight to `v`, not merely a term whose
  rational value is `v`), the two oriented premise tokens, precedence-valid; the F1-SEM verifier
  and the F7 Farkas-check extension validate the two **oriented implications**, not a variable-free
  contradiction. Accessor shape unchanged from Rev4-3 (`(value, lower≥token, upper≤token)`) — so no
  divergence from builder-stage1a's in-flight `fixed_bounds`. (§B.1)
- **Rev5-B4 rollback_to/watermark ordering pinned vs ordinary child pop (§C Stage 0 item 5).** Rev4-4
  said "on `pop` the hub replays the theory's undo log to the captured watermark" — a second rewind
  authority racing the child's own `pop` (codex: double-rewind / stale-watermark). Rev 5 pins TWO
  temporally-disjoint rewind mechanisms with a single owner each: **decision-level `pop`** reverses a
  merge-combine's theory-local mutations via the **child's own frame pop** (they ride the child's own
  trail in the current frame — the hub does NOT replay on pop); **intra-check `rollback_to
  watermark`** (fuel exhaustion / mid-cascade exception) reverses a partial cascade *before `check`
  returns*, and it **truncates** (removes the entries it reverses). Invariant DR (no double-rewind):
  `rollback_to` removes what it reverses, so a later `pop` never re-reverses it. Invariant SW (no
  stale watermark): a watermark is check-local — captured and consumed inside one `check`, never
  persisted across a `check`/`push`/`pop` boundary. The two events never interleave (a `rollback_to`
  is inside `check`; a `pop` is between checks), so there is no hub-rewind-vs-child-pop ordering
  question. (§C Stage 0 item 5, §B.3, §B.5(b))
- **Rev5-B5b Stage-4 out-scoping no longer rests on R1 (§C Stage 4).** R1 (`model_check.mli:1-12`)
  EVALUATES every asserted term in the candidate model and fail-closes; it does **not** validate that
  DT/array VALUES are well-formed — an opaque/cyclic datatype value or a non-extensional array
  representation can make every asserted equality evaluate consistently and pass R1. So "R1 gates the
  omitted rules → unknown" was unsound. Rev 5 picks option (c)+(b): Stage-4 `sat` promotion is
  restricted to **constructor-form / finite-function models** and enforced by a **model-value
  well-formedness validator** (a Stage-4 gate, landing WITH Stage 4, folded into the M4 cert
  prerequisite), NOT by R1's formula evaluation. The omitted search rules stay named follow-ups; the
  validator — not R1 — is what makes their omission `unknown`-not-wrong-`sat`. (§C Stage 4)
- **Rev5-NEW-HIGH fabric scoped to EQUALITIES-ONLY through Stage 3; diseq injection + its replay
  deferred (§A.2, §B.7, F7).** §B.7's `v_st` rule is equality-only, but F7 claimed "(dis)equality."
  Rev 5 makes the honest v1 explicit: the fabric injects only **equalities** (`assert_eq ~premise:Γ`)
  through Stage 3. A *justified disequality fabric edge* (`assert_neq ~premise:Γ` into the hub) is
  the Stage-4 datatype constructor-distinctness need and is DEFERRED to a Stage-4 diseq-injection ADR
  delta that carries the `v_neq`-analogue replay rule (assumed-disequality virtual proposition,
  disequality endpoint semantics, discharge). `new_diseq` NOTIFICATION *out* of the hub (A.3) is
  unaffected — it stays notification-only (H3), never a fabric edge. Existing hub disequalities with a
  real `Lit.t` premise (the `true≠false` axiom, a negated equality atom routed to EUF) are unchanged
  and already certifiable. (§A.2, §A.3, §B.7, F7)
- **Rev5-NEW-MED A4 proportionality metric corrected (§B.5a).** `Euf.register_term` internalizes a
  term's FULL subterm closure (`euf.mli:47-58`), so registering a maximal UF application `f(e)` pulls
  in the non-boundary arithmetic descendants of `e` too. The e-graph SIZE is therefore NOT
  proportional to `#UF-applications + #boundary nodes`; it is proportional to the **subterm closure of
  the uninterpreted-application structure** (every maximal UF app + its full argument closure) — still
  excluding a pure-arith term that never occurs under any UF. The **callback/`theory_var` domain**
  stays boundary-nodes-only (the soundness claim, correct). A boundary-marker pruning that would drop
  the under-UF arith descendants from the e-graph is named **future work**, not claimed as delivered.
  (§B.5a)

## Rev-4 changelog (codex DELTA residuals, driver-confirmed against source; team-lead scoped)

- **Rev4-1 THE THROUGH-LINE (C3-residual + NEW-CRITICAL, both source-confirmed).**
  (a) `lia_adapter` instantiates its token to `Lit.t` (`lia_adapter.ml:23`) with its own
  `explain_cache`, and `conflict_explanation` builds `{premises = c.premises; …}` straight from
  the LIA conflict's `Lit.t` premises (`lia_adapter.ml:80-85`) — so a LIA conflict **never
  traverses the fabric-handle expansion chokepoint** and cannot hold a fabric handle. Rev 4
  MECHANIZES the receiving-side currency: **both children instantiate the engine's opaque
  premise token to a shared `justification` sum (`Real of Lit.t | Fabric of edge_id`), and the
  COMBINATOR is the single expansion chokepoint** — `Combine.check` expands every `Fabric`
  handle in a child's returned `Conflict`/`Propagations` explanation to its `Γ` before the
  explanation crosses to the SAT core. This RELOCATES Rev-3's "chokepoint inside
  `euf_adapter.explain`" (which missed LIA) to `Combine`, covering both children uniformly.
  (§B.2, §B.4). (b) The injection-time forward-ref assertion cited `trail_pos`, which
  `theory.mli`'s `check`/`assert_lit` do not expose — replaced with a **combinator-side monotone
  assertion-order ledger** over the `assert_lit` stream; `trail_pos` is no longer cited as a
  theory-layer-readable quantity anywhere (the SAT core's own ask-time guard uses the trail it
  owns). (§B.1)
- **Rev4-2 C4 checker rule spelled out (same-model carry-forward note 1).** §B.7 now states the
  actual replay rule the #153 checker implements — a **virtual proposition** `v_st` for the
  fabric edge, **polarity** (`v_st` assumed true), **endpoint binding** (`v_st` connects the
  classes of `s` and `t` in the EUF proof), and **assumption discharge** (final resolution
  eliminates `v_st` against the witnessed `¬Γ ∨ v_st` LIA-implication clause). Joining by
  `edge_id` alone is insufficient; the rule is ADR content, not a gesture.
- **Rev4-3 C1 witness availability RESOLVED (source-checked).** `lia.mli` exposes **no**
  fixed-value bound-witness (only `rational_value` = value, and `propagate` premises; Farkas
  multipliers live only in `conflict`, `lia.mli:46-51`). Rev 4 specs the **additive non-frozen
  accessor** `Lia.fixed_bounds : 'tok t -> Term.t -> (Rational.t * 'tok * 'tok) option`
  (value + lower-bound premise + upper-bound premise); the four bounds of `s`,`w` with unit
  multipliers are the `s=w` Farkas witness F1-SEM/F7 consume. (§B.1)
- **Rev4-4 C5/C6 reversible state — THEORY-LOCAL UNDO LOG (source-confirmed insufficiency).**
  Snapshot-both-payloads is insufficient: LIA's destructive row unification pivots the tableau,
  touching state **beyond the two payloads**. Rev 4 requires a **theory-local undo-log
  contract**: before invoking a theory's merge-combine, the hub captures the theory's own
  trail watermark; the combine logs every mutation on the theory's trail; `pop` replays the
  theory's undo log back to that watermark. (Persistent/immutable payloads are the special case
  where a payload snapshot suffices.) Plus the **fuel-exhaustion rollback rule** (parity with
  H5): a partially-drained cascade has mutated state — on fuel exhaustion the fabric **rolls
  back to the check-entry watermark (or poisons)**, then returns `unknown`; it NEVER returns
  `unknown` over half-mutated inconsistent state. (§C Stage 0 item 5, §B.5(b))
- **Rev4-5 NEW-HIGH-2 Stage-4 completeness-rule inventory.** Enumerated and scoped: arrays
  (extensionality + difference), datatypes (exhaustiveness, acyclicity; no-confusion =
  injectivity+distinctness, in-scope). The completeness rules beyond the core are **out-of-scope
  for the first cut**, WITH the soundness argument: each omission is a wrong-`sat` risk, gated by
  **R1 model-check** — a candidate model violating an omitted rule (a cyclic DT value; two arrays
  equal in-model but differing at an index) fails R1 → `unknown`, never wrong-`sat`. Named
  follow-ups. (§C Stage 4)
- **Rev4-6 NEW-HIGH-3 §E.2 vs §C grade contradiction fixed.** §E.2 now agrees with §C: Stage-0
  scope-consolidation (items 1–3) is refactor-grade; the cross-module machinery (items 4–6) is
  **TCB-grade** (TCB wins).
- **Rev4-7 H4 driver caveat — verified, no correction to either side needed (precision added).**
  `euf.ml:369-399` `register` DOES do full subterm closure (codex's premise is accurate); the
  A4-erratum proportionality (§B.5a) is a **combinator-level** property — which terms the
  combinator calls `register_term` on (boundary/UF only) — NOT a `register_term`-level
  selectivity. The ADR and the finding operate at different levels and are consistent; §B.5a
  reworded to say so.
- **Rev4-8 same-model carry-forward note 2 — intra-theory disjointness wording.** §C Stage 0
  item 4 clarified: the disjointness invariant is **between theories** (cross-module); a hub
  merge's theory-side mutation (LIA tableau pivot) lives on that theory's own trail and is
  reversed by its theory-local undo log (Rev4-4), so it composes with the other theories'
  trails without a global order.

## Rev-2 changelog (same-model review `logs/adr-egraph-fabric-review.md`, R1–R10)

- **R1 (HIGH-1) — F1 re-grounded.** Precedence-safety for an injected merge's *consequences*
  comes from routing them through `Euf.propagate` → `euf_adapter.cache_reason`
  (snapshot-at-propagation, the actual #102 fix, `euf_adapter.ml:152-171,229-244`), NOT the
  forest edge-order (`euf.mli:84-91`), which #102 proved insufficient for the *walk*. The #102
  precedence mutant is re-armed to drive a *consequence* of an injected edge. (§B.1)
- **R2 (HIGH-1b) — F2 expansion chokepoint pinned.** Handle→Γ expansion happens inside
  `euf_adapter.explain`/`reason_of_implied`, before the premise list crosses
  `Sat.theory_explain_checked`; it is the single chokepoint all consumers route through; the
  cache stores the *expanded* Γ (registry lives only to propagation time); the repealed
  `euf_adapter.mli:22-24` "adds no new reasoning" invariant is named. F4 restated as "same
  expansion chokepoint," not "same walk." (§B.2, §B.4)
- **R3 (HIGH-2) — F5 termination conditioned.** Proven for the no-new-terms case (Stages 1–3,
  where callbacks may NOT register terms — a stated restriction); Stage 4's term-creating
  callbacks get a fuel bound with a *hard-stop* enforcement (degrade to `unknown`), not a
  goldens counter. (§B.5, §C Stage 4)
- **R4 (CRITICAL-1 + MED-2) — named backtracking-substrate section rewritten.** §C Stage 0
  now gives the *mechanism* for cross-module undo order (not an assertion): a disjointness
  invariant under which independent per-module trails compose over shared frames, plus a
  co-location rule putting the one genuinely-coupled state (per-class data) on its merge's
  trail; merge-undo specified as snapshot-both-inputs with the owning theory's `unmerge`; the
  data-merge mutant must drive a pop restoring two distinct payloads; global-epoch protocol
  named as the fallback if disjointness fails; euf hot-path carve-out under an equivalence
  gate. (§C Stage 0)
- **R5 (AP5) — staging honesty.** Stages 1b–3 reframed plainly as Stage-4-enabling
  infrastructure (acceptance = soundness + equivalence + determinism, NOT a solve-rate delta
  under A7-1a); the solve-rate justification for the 1b→4 arc rests on Stage 4; 1a is the
  severable, self-justifying off-ramp. The real decision is binary. (§C, §E.3)
- **R6 (MED-3) — Stage-2/3 gate corrected.** MISMATCH=0 is soundness-equivalence, NOT
  verdict-equivalence (#142 MED-2 standard); the fallback is not "free"; a model-reconstruction
  check added to Stage-2 acceptance. (§C Stage 2)
- **R7 (LOW-2) — §0 now quotes the repealed combine.mli invariant** ("NEVER fabricates an
  assertion … provenance by construction") and states the F1–F7 replacement in the same
  breath. (§0.2a)
- **R8 (MED-1) — Stage 4 split** into 4a (datatypes, lands on the primitives) and 4b (arrays),
  with array read-over-write's actual requirements stated (term-creation-during-cascade + i=j
  `Theory.Split`) and marked as needing the Stage-4 fuel primitive, not delivered by 1–3. (§C)
- **R9 (AP7) — A4-erratum contract stated:** callbacks fire only on boundary-node classes;
  `theory_var` attaches only to boundary classes; the fabric registers the A4-erratum set and
  no more; under-inclusion mutant carried. (§B.5a, §D)
- **R10 (MED-4, LOW-1/3/4) — overclaims fixed:** F7 scoped emission-only until the replay
  checker (#153); §0.3.1 "more decoupled" → "concentrates coupling in `euf.mli` by design";
  §A.1 "existing asymmetry" → only Stage 1 is wiring, Stages 2–3 are substantial new `euf.mli`
  surface; Stage 1a's dependency on #142/#154 landing noted.
- **MED-5 — F1(b) injection-time assertion** added (`max trail_pos(Γ) < current trail length`)
  so a forward-referencing premise fails loud at injection, not as a silent corpus `unknown`.

## Rev-3 changelog (codex batch `logs/adr-egraph-fabric-codex.md`, 6C/5H/4M + team-lead rulings; theme: "no end-to-end provenance path from injection, through callbacks and conflicts, into a checkable certificate")

- **C4 (the standout, code-verified; OQ4 CLOSED to "specify now") — §B.7 rewritten with a
  concrete end-to-end certificate path.** `Shared_eq` had no complete cert path:
  `recorder.theory_event = {id;clause;role}` carries no tag/witness (`recorder.mli:36-40`),
  `Rule_tag.Shared_eq` is payload-free (`explanation.mli:23`), a hub edge with no Boolean atom
  has no `lit` to cite, and E3 walks only SAT-trail literals so a purely-internal fabric edge is
  invisible. §B.7 now specifies the additive ADR-0013 delta: a **parallel fabric-event stream**
  (combinator-emitted, trace-gated, no `sat.mli`/frozen change) carrying `{edge_id; s; t; Γ;
  Farkas witness}`, and a **certificate sub-DAG** (structurally-denoted equality → witnessed LIA
  implication checked by the existing Farkas checker → EUF proof using the assumed edge →
  resolution eliminating it), co-designed with the #153 step-2 checker so emittable ⇒ checkable.
- **C1 (promote, NUANCE) — §B.1 enforcement (ii) promoted** from a one-clause mention to a
  numbered obligation (F1-SEM): a cheap always-on injecting-theory Farkas-witness verifier
  (re-sum the multipliers, O(|Γ|)) distinct from the env-gated EUF replay; mutant is now a
  **weak-Γ** (earlier-but-semantically-insufficient), not just a late-Γ.
- **C2 (CONFIRMED) — §B.2 handle representation mandated:** an abstract edge-id in a disjoint
  namespace (not a `Lit.t`), immutable, FIRST-successful-merge-WINS, same-frame removal, no
  entry for skipped redundant merges; duplicate-injection-with-different-Γ-across-nested-frames
  mutant.
- **C3 (CONFIRMED) — notify-OUT justification currency added (F1c/§A.3):** `new_eq`/`new_diseq`
  now carry a snapshotted justification handle + source; the receiving theory attributes its
  derived facts to that handle; self-source suppression + idempotence + acyclic order +
  mandatory expansion to real trail lits before any seam result.
- **C5 (PLAUSIBLE; labels are codex's) — §C Stage 0 re-entrancy + TCB review:** the cross-module
  ordering machinery gets a **queued, non-reentrant closure** (callbacks enqueue, not recurse)
  so nested hub→LIA→hub cascades have a single drain order; the cross-module aspects of Stage 0
  are **TCB-grade** review (not the refactor-grade of the pure scope-consolidation); nested
  cross-injection + nonchronological-backjump + `cancel_until 0` test added.
- **C6 (PLAUSIBLE) — merge-undo witness strengthened (§C Stage 0 item 5 / Stage 3):** the undo
  witness records both roots, both pre-merge payloads, presence bits, and all callback
  mutations; the test inspects BOTH restored classes; stale-data mutant with a verdict effect.
- **H1 (CONFIRMED) — F5 termination hardened:** queued non-reentrant closure + **novelty keys**
  (skip already-known eq/diseq/events, so diseq-echo terminates) + a well-founded measure that
  includes attached-data state; diseq-echo + redundant-injection mutants; the fuel budget is the
  backstop.
- **H2 (CONFIRMED) — β-hash CUT to a non-goal (lead default).** It was orphaned (never staged)
  and one-directional. Model-based combination is RETAINED via the existing **bidirectional**
  `find_disagreement` Final split (unchanged, both "LIA-equal/hub-distinct" and
  "hub-equal/LIA-different" directions); β-hash candidate generation is a §D non-goal (future
  optimization if the interface-set scan is a measured bottleneck). Resolves the §A.6-vs-§C
  orphan.
- **H3 (CONFIRMED) — `new_diseq` under non-convex ℤ specified (§A.3):** for LIA-over-ℤ a diseq
  notification is **notification-only, idempotent** (LIA cannot assert `x≠y` as a bound); the
  SAT-visible ℤ-trichotomy remains the mechanism that forces the ordering. Per-theory otherwise
  (datatypes: constructor distinctness).
- **H4 (CONFIRMED) — OQ5 resolved normatively:** the monotone combinator-side interface set
  survives unchanged as `find_disagreement`'s domain and the A4-erratum e-node set; the
  under-inclusion mutant is imported into the fabric stage gates (§B.5a).
- **H5 (CONFIRMED) — F6 atomicity (§B.6):** injection is transactional — all fallible work (term
  construction, Γ extraction) happens BEFORE any hub mutation, so a caught exception leaves zero
  partial state ("catch only before first mutation" boundary); L1 mutant + mid-cascade-exception
  mutant added.
- **M1 (PLAUSIBLE) — empty-Γ tripwire refined (§B.2):** empty Γ permitted ONLY with a checkable
  theory-axiom witness (Stage-4 unconditional theorems); for Stage 1 (always has bounds) it
  stays a hard tripwire.
- **M2 (PLAUSIBLE) — §C gate definitions split:** (G-sound) label/oracle-soundness (repo
  `mismatch_count` vs `:status`, ignores `unknown`) vs (G-equiv) paired branch-vs-trunk outcome
  diff (includes `unknown`, models, counters, certs); per-stage pinned manifests + allowed
  transitions. "MISMATCH=0" was conflating the two.
- **M3 (CONFIRMED) — §C fallback reversibility costed honestly:** each off-switch is reversible
  only until a *later* stage consumes it; explicit rollback boundaries; §E.4 risk 3 corrected
  past Stage 1b.
- **M4 (CONFIRMED, resolves with C4) — Stage 4 cert delta is a formal prerequisite,** not a
  deferred OQ; §C Stage 4 gate now depends on the §B.7 fabric-event stream + a datatype/array
  leaf grammar scoped as its own ADR-0013 delta.

## Rev-4.1 changelog (same-model scoped-confirm RATIFY-after-one-fix + two Stage-0 builder-reality notes)

- **Rev 4.1: chokepoint completeness (`Combine.explain`), per same-model confirm.** Rev 4's
  "`Combine.check` is the single chokepoint" was incomplete: lazy propagation reasons carry no
  explanation at check time (`theory.mli:31-32`) and are served via `Combine.explain`
  (`combine.ml:617-625`), which returns the child `Explanation.t` verbatim — so a `Fabric`
  handle on a lazy consequence reason bypassed expansion. Fixed: the expansion chokepoint is
  **BOTH** `Combine.check` (eager conflicts) AND `Combine.explain` (lazy reasons), same registry;
  the "lazy reasons route through check" claim is deleted; a **handle-leak-VIA-EXPLAIN mutant**
  (distinct from the check-path one) is added to the acceptance table. (§B.2, §B.4, acceptance)
- **Stage-0 scope (builder reality):** the e-match manager is **EXCLUDED** from Trail migration
  by **mechanism-mismatch** (selector-keyed `on_pop` = SAT-assumption lifecycle, FIFO
  seed-restore — not LIFO decision-level scoping), not deferred; Stage 0 migrates **three** sites
  (simplex, combinator pins, EUF scope carve-out) + ematch excluded-with-rationale. (§C Stage 0)
- **Trail representation (builder reality):** the implemented `Trail` is a **polymorphic
  `('e,'f)` record**, not a literal functor — meets zero-hot-path-allocation (settles OQ6) and
  carries entry + all aux watermarks in one frame, restored atomically (strengthens the desync
  story). Resolves RRR-F0(1)'s (a)/(b) open. (§C Stage 0)
- **Not addressed here (Rev-5 scope, fresh author):** the codex NOT-RATIFIABLE blockers
  B1/B3/B4/B5b + the new diseq-replay HIGH + the A4-metric MED — these are genuinely NEW design
  work (the typed child-explanation currency), tracked as #169; see "Successor pointer" at the
  end of `logs/egraph-fabric-log.md`.

- **Directive (design author, verbatim):** "I think we might need to build z3 style
  support for all theories able to interact with the e-graph, rather than going via
  propositional assertions."
- **Reopens:** ADR-0010 §2 / §5 — *"Keep the seam; do NOT adopt an e-graph hub."* This
  ADR is the adversarial-review ritual DESIGN §10/§11 requires to re-open a logged
  decision. It is argued on design merits per A5 (freeze-avoidance carries no weight of
  its own); §0.3 rebuts ADR-0010's three rationale points one at a time.
- **Owning tasks:** #156 (Fabric Stage 1: LIA fixed-eq propagation — gated on this ADR);
  new rows per stage in §C.
- **Spec-by-citation:** z3's `theory_lra`/`smt_context` mechanisms as documented in
  `logs/z3-combination-study.md` (source-cited against `Z3Prover/z3`), de Moura & Bjørner
  2007 (model-based theory combination), Nieuwenhuis–Oliveras 2005/2007 (proof-producing
  congruence closure — already our EUF engine's spec). Reviewers check conformance through
  those correspondences.

---

## 0. Context — what we have, what z3 has, and why reopen ADR-0010

### 0.1 The mechanism we ship today (propositional-assertion routing)

`Combine (Uflia_router) (Euf_adapter) (Lia_adapter)` (`smt/combine/combine.ml`,
`combine.mli`) presents ONE `Theory.THEORY` to the CDCL(T) engine (`smt/interface/
cdclt.ml`). EUF and LIA are separate child plugins. They exchange cross-theory
information through exactly two paths (`combine.mli:39-52`):

- **Path 1 — entailed-equality forwarding.** A shared equality a child *entails* (EUF
  congruence deriving `f a = f b` after `a = b`; LIA a bound-implied equality) is returned
  by that child's `check` as a `Propagations` element; `Combine` forwards it, the SAT core
  puts the equality literal on the trail, and it is re-asserted to its owner(s). A shared
  equality thereby reaches both children **through the SAT trail** — a propositional
  round-trip.
- **Path 2 — model-disagreement split.** When neither child *entails* the equality but
  their candidate models pick incompatible arrangements, `Combine` emits a
  `Theory.Split [x=y; x<y; x>y]` (the ℤ-trichotomy, `uflia_router.equality_split`,
  `uflia_router.ml:63-71`) at `Final`, forcing the SAT core to *decide* the shared equality
  atom. `find_disagreement` (`combine.ml:499-529`) selects the tag-least disagreeing
  Int-sorted interface pair; the loop terminates because every split atom is decided at the
  next `Final` and a decided pair never re-disagrees (`combine.mli:47-52`).

Both paths route cross-theory equalities **as propositional atoms over original terms
through the SAT core**. This is a deliberate ADR-0010 decision, not an accident.

### 0.1a The specific ADR-0010 invariant this ADR repeals, and its replacement (R7)

A decision-reversing amendment must name what it overturns. The load-bearing ADR-0010 / A2
invariant, verbatim (`combine.mli:65-76`):

> "The combinator NEVER fabricates an assertion: it only ever forwards literals the engine
> placed on the trail. A shared equality becomes asserted to both theories exactly when the
> SAT core decides one of the [Split] disjuncts, so every conflict a child later derives from
> that equality lists the split literal in its premises — provenance is by construction,
> never 'from thin air'."

This ADR **repeals the "provenance by construction" clause** for fabric-injected equalities
(the combinator now *does* introduce a cross-theory equality into the hub without the SAT
core deciding a split literal). The replacement contract is exact and is the whole of §B:
**every fabric-injected equality carries an explicit premise set `Γ`, recorded at
injection time, trail-precedence-valid, expanded at ask time, never re-derived (F1–F2), and
its consequences' reasons are snapshotted at propagation time via the #102 cache path (F1/F4);
provenance becomes by-record, not by-construction.** (The fabric injects only *equalities* through
Stage 3; a justified *disequality* fabric edge is deferred to the Stage-4 diseq-injection delta,
§A.2/§B.7, and carries the same by-record contract when it lands.) The rest of the ADR-0010 invariant —
exchange over *original terms*, no proxies — is retained unchanged (§0.3.3). What we give up
(the free provenance) is precisely what F1–F2 pay for; the review's concession that the fabric
*loses by-construction provenance* is owned here, not hidden.

### 0.2 What z3 does instead (source-cited; `logs/z3-combination-study.md`)

z3's QF_UFLIA path uses no static syntactic split. It has two model/entailment-driven
mechanisms:

- **Mechanism 1 — eager fixed-equality propagation (the load-bearing one).** When
  `theory_lra` makes a shared term *fixed* (lower = upper = `k`), it looks up a
  `value → term` map (`m_value2var`); if another term is already fixed to `k`, it
  **propagates the equality `v = w` directly into the e-graph as a theory implication**,
  with a Farkas-style justification built from the four bound constraints
  (`fixed_var_eh` → `assign_eq` → `ctx().assign_eq(x, y, eq_justification(js))`,
  `theory_lra.cpp:3494-3539`). It runs in `propagate`, not at final check; it needs no
  complete Boolean model and adds no permanent clause. This is exactly what cracks the
  mathsat/Wisa family (`fmt0=0`, `fmt1=fmt0+2` ⇒ `fmt1=2` ⇒ congruence fires
  `select_format(fmt1) ~ select_format(2)` during propagation).
- **Mechanism 2 — model-based theory combination (de Moura–Bjørner 2007).** At final
  check, shared terms equal in the current simplex model but not entailed-equal are
  proposed as a **retractable true-first case split** (`assume_eq` + `set_true_first_flag`,
  `smt_context.cpp:4697-4753`). Candidate pairs come from hashing shared terms by their
  current β value (`m_model_eqs`, `theory_lra.cpp:186-208`).

The structural difference: z3 lets the **arithmetic state** choose the pairs (entailment
for mech 1, model value for mech 2) and, for mech 1, propagates the equality *into the
shared congruence structure* rather than round-tripping through the SAT core. Our Path 1
is the moral analogue of mech 2's *forwarding*, and our Path 2 is a heavier version of
mech 2's *split* — but **we have no analogue of mechanism 1 at all**, and neither of our
paths writes to a shared e-graph. The directive asks for both: mechanism 1, and a shared
e-graph theories write to directly.

### 0.3 Reopening ADR-0010's "no e-graph hub" — the three rationale points, rebutted

ADR-0010 §2 kept the seam and rejected an e-graph hub for three reasons. A5 says the
comparison must be on design merits, with freeze-avoidance carrying no weight. Taken one at
a time:

1. **"Decoupling — the seam is the parallel-workstream boundary."** *Rebuttal:* the
   parallel-workstream boundary that matters is the **frozen CDCL(T)↔`Combine` seam**
   (`theory.mli`), and this ADR keeps it byte-for-byte (§A.1, §E.1). The fabric changes
   only how `Combine`'s *children* talk to each other — a boundary that is already
   combinator-internal and already asymmetric (EUF is a `CONGRUENCE_CHILD` with
   `internalize_term`, LIA is a plain `THEORY`; `combine.mli:169-183`). *Honest form (the
   draft's earlier "more decoupled" was spin):* the fabric does **reduce** plugin-from-plugin
   decoupling — LIA/datatypes/arrays gain a hard dependency on the (unfrozen) `euf.mli`
   callback + `theory_var` surface — and it does so **deliberately**: it concentrates coupling
   in `euf.mli` because datatype/array procedures ARE e-graph rules (injectivity on merge,
   read-over-write) and the trichotomy-split idiom fits neither. That is a real cost, justified
   by Stage 4, not a net decouple. A5 is satisfied because the concentration is argued on
   merits (Stage-4 fit, correctness-by-construction for the merge rules), not to avoid a
   freeze.
2. **"Reasons-purity."** *Rebuttal:* purity is preserved **iff every equality injected
   into the e-graph carries a premise set recorded at injection time and valid by trail
   precedence** (§B). That is a real, hard obligation — it is the #102 lesson generalized —
   but it is *dischargeable*, and the machinery to discharge it already exists (the EUF
   engine's proof-producing union-find + explanation forest, and the euf_adapter
   explain-cache the #102 fix installed). Reasons-purity is not lost by the hub; it is
   relocated from "the SAT trail carries the premise by construction" to "the fabric edge
   carries the premise explicitly" — a stronger, checkable contract.
3. **"Certificates — exchange over original terms needs no definitional layer."**
   *Rebuttal:* the fabric still exchanges over **original terms** — e-graph classes are
   classes of original hash-consed `Term.t`s; there are no proxies (ADR-0010's
   internalization property is retained). The certificate grammar already reserves the
   exact slot: `Shared_eq {gamma; eq}` = the explicit N-O clause `¬Γ ∨ (s=t)`
   (`explanation.mli`, cert `theory_leaf`), today **defined but dormant** (never emitted;
   the trichotomy Split is used instead). Activating it via the existing E3 `Theory_prop`
   materialization path (§B.7) is an additive ADR-0013 delta, not a definitional layer.
   *Scope honesty (MED-4):* this makes a fabric equality **emittable**, not yet **checkable** —
   cert is at step-1 emission only (#134); the replay checker is #153 (future). So ADR-0010's
   worry is answered at the level it was raised (no definitional layer is needed for the
   *emission* it lacked the hook for), but until #153 the uncorrelated soundness backstop for a
   fabric UNSAT remains the whole-VC Lean/grind path, exactly as it is for every UNSAT today.
   This is now the leverage ADR-0010 did not have: it predates ratified ADR-0013, so it cited
   "no definitional layer" without the `Shared_eq` hook that now exists to carry the exchange.

**Net.** ADR-0010's rationale was sound *for the QF_UFLIA-only, EUF+LIA-only world at the
time*. The forcing function to reopen is (a) a measured corpus gap the seam cannot close
cheaply (mathsat/Wisa — mechanism 1 has no propositional-split equivalent that isn't the
rejected HIGH-3/HIGH-4 static injection, §0.4) and (b) the impending datatypes/arrays
theories, whose procedures are e-graph rules. This ADR does not discard ADR-0010; it
*retains its internalization result* (no proxies, boundary-node interface set, original-term
exchange) and *supersedes its transport decision* (propositional round-trip → direct
justified merge), staged so the seam stays frozen through Stage 3.

### 0.4 Why the seam alone cannot close the gap cheaply (the #142 evidence)

The one attempt to get mechanism-1-like behavior through the seam without a fabric — eager
static-syntactic trichotomy injection (`task/uflia-search`, mechanism B) — was **rejected**
(`logs/uflia-search-review.md:7`):

- **HIGH-3:** a useless `(x+2, x)` bridge (symbolic×symbolic, LIA trivially separates it,
  zero congruence gain) defeats #117's model reconstruction, flipping a genuinely-SAT
  file to `unknown` (`uflia-search-review.md:91-100`).
- **HIGH-4:** 30 UFs over 24 mutually-distinct shared vars inject 276 trichotomies (under
  the 600 cap), forcing a 24-variable arrangement search the base problem never had —
  **~50× slowdown** (1.58s vs 0.03s), measured (`uflia-search-review.md:102-119`).

The root cause is syntactic pair selection: static rules must over-approximate (blowup) or
guess a filter (misses), because syntax cannot know which pairs *arithmetic* will make
equal (`z3-combination-study.md:178-207`). z3's mechanisms don't have this problem because
the arithmetic *state* selects the pair. The fabric is how we get state-driven selection.
The interim `v1b` (fix-triggered trichotomy Split, §C Stage 1) already escapes HIGH-3/HIGH-4
by construction (a term fixed to value `v` is in at most one live bridge per value) — but it
is still a propositional split; the fabric's `v1a` is the direct-merge realization.

---

## A. TARGET ARCHITECTURE — the theory–e-graph fabric

### A.1 Where the fabric lives (and what stays frozen)

**The fabric is internal to `Combine`.** The CDCL(T) engine continues to drive one
`Theory.THEORY` (the combined theory) through the frozen seam
(`create`/`register_atom`/`assert_lit`/`check`/`explain`/`push`/`pop`/`model`); nothing
about that contract changes for Stages 1–3. What the fabric reorganizes is the *internal
cross-talk between `Combine`'s children*, which today is the propositional round-trip
(§0.1) and becomes direct interaction with a shared congruence hub.

**The hub is the EUF engine's congruence structure, promoted.** `smt/theories/euf/euf.ml`
already is a proof-producing union-find + explanation forest + congruence table + pending
merge queue (Nieuwenhuis–Oliveras), with level-granular push/pop
(`ARCHITECTURE.md`, `euf.mli`). It already exposes the exact primitive the fabric needs:

```
val assert_eq  : 'p t -> premise:'p -> Term.t -> Term.t -> unit   (* euf.mli:60 *)
val assert_neq : 'p t -> premise:'p -> Term.t -> Term.t -> unit   (* euf.mli:64 *)
val explain    : 'p t -> Term.t -> Term.t -> 'p list              (* euf.mli:92 *)
val explain_implied : 'p t -> implied -> 'p list                  (* euf.mli:113 *)
```

We do **not** build a new hub. The fabric = this engine, generalized along three axes
(A.2–A.4). EUF thereby stops being "just one plugin among equals" and becomes "the
congruence substrate the other theories attach to" — the honest statement of the ADR-0010
reversal. *Scope honesty (LOW-4):* only **Stage 1** is genuinely "mostly wiring" (the
`assert_eq ~premise` primitive above already exists, verified `euf.mli:60,64,92,113`). The
callback direction (A.3, Stage 2) and per-class `theory_var` data (A.4, Stage 3) are
**substantial NEW `euf.mli` surface**, not a mere evolution of the existing
`CONGRUENCE_CHILD`/`internalize_term` asymmetry — they are new engine APIs with their own
soundness obligations (F3, F5). The existing asymmetry is the *precedent* that EUF already
plays a distinguished role; it is not the *whole* of what Stages 2–3 add.

### A.2 Mechanism (1): theories propagate EQUALITIES INTO the hub, with justifications

Generalizing z3 mechanism 1. Any theory `T` may, mid-search, call the hub's
`assert_eq ~premise:Γ a b` where `a`, `b` are original terms and `Γ` is a
**theory-local premise set** justifying `a = b` under `T`'s current state. The hub merges
the classes, runs congruence closure, and fires any consequent
merges — all in the shared structure, with no SAT round-trip. Concretely for Stage 1: when
LIA fixes a shared Int term `s` to a value `w` also holds (β lower = β upper = `w`'s value),
`Combine` calls `assert_eq ~premise:(the fixing bound literals for s and w) s w`.

**Scope: EQUALITIES ONLY through Stage 3 (Rev5-NEW-HIGH).** A *justified disequality* fabric edge
(`assert_neq ~premise:Γ` INTO the hub) is deliberately OUT of scope through Stage 3. Stage 1's
fix trigger only ever equates two terms; LIA over ℤ cannot assert `x ≠ y` as a bound anyway (the
non-convex case, H3); and injecting a diseq as a justified fabric edge only becomes a need at
Stage 4 (datatype constructor-distinctness). That need — and its certificate replay rule (a
`v_neq`-analogue: an assumed-disequality virtual proposition, disequality endpoint semantics,
discharge; §B.7) — is DEFERRED to the Stage-4 diseq-injection ADR delta, stated consistently with
F7 (which Rev 5 corrects from "(dis)equality" to "equality"). This does not touch existing hub
disequalities that carry a **real `Lit.t` premise** — the standing `true ≠ false` axiom and a
negated equality atom routed to EUF (`euf_adapter.ml:55,131`) — which are asserted through the
normal seam, not as fabric edges, and are already certifiable. The reverse direction, `new_diseq`
NOTIFICATION *out* of the hub (A.3), is also unaffected: it stays notification-only (H3).

The premise set `Γ` is a set of `Lit.t` (the global literal currency, ADR-0005 D2) — the
bound/Farkas literals the arithmetic child used to fix `s` and `w`. This is the crux of the
soundness frame (§B): `Γ` must be recorded *now*, at injection time, and every member must
be trail-precedence-valid (asserted strictly before this merge). It is **never re-derived
at ask time** — the #102 rule, elevated to a fabric-wide contract.

### A.3 Mechanism (2): hub merge/diseq events NOTIFY registered theories (callbacks)

The reverse direction (z3's theory internalization hooks). Theories register `new_eq` /
`new_diseq` callbacks with the hub. When the hub merges classes `c1`, `c2` (from any
source — congruence, a user assertion, or another theory's injected equality), it notifies
each registered theory. A theory uses this to update its own state (LIA: assert the
corresponding bound-equality into the tableau; datatypes: check constructor compatibility,
propagate injectivity/selectors). This is what replaces Path-1 forwarding: instead of an
entailed EUF equality round-tripping through the SAT trail to reach LIA, the hub notifies LIA
directly. (Arrays' read-over-write is deliberately NOT in this list — it needs
term-creation-during-cascade + an `i=j` Split, primitives the callback alone does not provide;
see §C Stage 4b.)

**Callbacks carry a justification, not just terms (C3 — the notify-OUT half of the provenance
path).** Rev 1 described the callback as delivering "the terms carried by `c1` and `c2` are now
equal" — terms only. That is a provenance hole: when LIA records a hub equality as a bound, the
new bound needs a **premise token**, or LIA's later conflict cannot name why the two terms are
equal. So `new_eq` delivers `(s, t, justification_handle, source)` where `justification_handle`
is the hub edge's F2 handle (§B.2) — the receiving theory attributes any fact it derives to that
handle, and at ask time the handle expands (via the single §B.2 chokepoint) to the real trail
premises `Γ`. F1c (§B.1) makes this a numbered obligation. **`source` suppresses
self-notification** (a theory that injected the merge does not re-consume its own callback —
idempotence), and the cascade is drained in a **queued, non-reentrant** order (F5, C5/H1) so the
dependency graph of callback-derived facts is acyclic and expansion terminates.

**`new_diseq` under non-convex ℤ (H3).** LIA cannot assert `x ≠ y` as a single simplex bound —
disequality is exactly the non-convex case the trichotomy machinery exists for. So for
LIA-over-ℤ, a `new_diseq` notification is **notification-only and idempotent**: LIA may record it
(for its Final model-disagreement check) but does NOT convert it to a bound, and the
**SAT-visible ℤ-trichotomy remains the mechanism that forces the ordering** (§0.1 Path 2, §A.6
row 2). Treating a diseq notification as a convex constraint would be unsound; silently ignoring
it after weakening the fallback would be incomplete — notification-only + retained trichotomy is
the sound-and-complete rule. For other theories `new_diseq` means what that theory defines
(datatypes: constructor distinctness → a conflict if the classes carry incompatible
constructors).

Callbacks fire during hub closure, so they can *cascade* (a `new_eq` triggers a theory to inject
another equality, merging more classes, notifying again). The cascade is a **queued fixpoint,
not recursion** (F5): callbacks enqueue work; the hub drains the queue to a fixpoint in one
deterministic order. Termination (novelty keys so a diseq echo cannot loop) and the trailing of
every merge are §B obligations (F5, F6).

### A.4 Mechanism (3): per-class theory data (`theory_var`), surviving merges

Each theory may attach opaque per-class data to an e-class, and the hub maintains it across
merges (z3's `theory_var`). On `merge(c1, c2)`, the hub hands each registered theory the
data from both classes so the theory can combine them (LIA: unify the two bound rows,
detecting an immediate bound conflict; datatypes: the constructor tag of the merged class,
raising injectivity/distinctness conflicts; arrays: the union of read indices). This is the
substrate that makes Stage 4 (datatypes/arrays) *native* rather than bolted on: their
decision procedures become "what to do when two classes carrying my data merge."

Attached data is trailed with the class (undone on `pop`), like all hub state (F6).

### A.5 Model-based combination is RETAINED unchanged; β-hash is a NON-GOAL (H2)

z3's mechanism 2 (model-based combination) stays — but as the **existing bidirectional
`find_disagreement` Final split**, unchanged, NOT as a β-hash rewrite. Rev 1 proposed replacing
`find_disagreement` with β-value bucketing (`lia.rational_value`) as "mechanism 4"; the codex
leg (H2) showed this was **orphaned** (never assigned a migration stage) and **one-directional**
(β-bucketing finds "LIA-equal, hub-distinct" pairs but NOT the "hub-equal, LIA-different"
direction that today's `find_disagreement` also detects — dropping it opens a wrong-`sat`). So
Rev 2 **cuts β-hash to a non-goal** (§D): the model-coincident-but-not-entailed case is handled
by the existing bidirectional `find_disagreement` → ℤ-trichotomy `Theory.Split` at `Final`,
exactly as today, sound in both directions. β-hash candidate *generation* (choosing pairs by
β-value instead of scanning the interface set) is a **future optimization** to revisit only if
the interface-set scan is a measured bottleneck — not part of this ADR.

### A.6 What each existing mechanism BECOMES

| Today (§0.1) | Under the fabric | Disposition |
|---|---|---|
| Path 1: entailed-equality forwarding via SAT trail | Hub merge + `new_eq` callback (§A.3): an entailed equality merges the class directly and notifies the other theory; no SAT round-trip | **Subsumed** by A.3. The equality still becomes SAT-visible *iff* a clause references its atom (registered atoms still propagate as `Propagations`); pure congruence drivers stay internal to the hub. |
| Path 2: Final model-disagreement ℤ-trichotomy Split | **UNCHANGED** — the bidirectional `find_disagreement` → `Theory.Split` stays as-is | **Retained, unchanged.** For non-convex ℤ, a model coincidence not entailed must still be *decided* by the SAT core (the fabric may not assert a non-entailed equality as a fact). Both disagreement directions preserved (H2). |
| `uflia_router.equality_split` ℤ-trichotomy | Unchanged construction; called from the Final `find_disagreement` path and (interim, §C Stage 1 v1b) the fix-trigger path | **Retained.** (Its H1/L1 carry-over bugs are fixed at the construction site, F6.) |
| `combine.ml find_disagreement` (syntactic-free, over interface set) | **Unchanged** — still scans the interface set for pairwise model disagreement, both directions (H2 cut β-hash) | **Retained, unchanged.** |
| `EUF.internalize_term` (makes EUF see a term only in a LIA atom) | The hub's registration; every interface term is a hub node by construction (A.2 needs both sides valued) | **Subsumed** by hub registration; the interface set (its domain) survives unchanged (OQ5 resolved, §B.5a). |

The Stage-1 fix-trigger (mechanism 1) is **new**; it has no predecessor to subsume.

### A.7 The picture

```
                 CDCL(T) engine  (Sat.theory callback; FROZEN theory.mli seam — UNCHANGED)
                        |  register_atom / assert_lit / check(effort) / explain / push/pop / model
                        v
   +-------------------------------- Combine (one THEORY) --------------------------------+
   |                                                                                       |
   |   Router (ownership + split material)                                                 |
   |                                                                                       |
   |          +----------------------- FABRIC HUB (EUF congruence engine) -------------+   |
   |          |  proof-producing union-find + explanation forest + congruence table    |   |
   |          |  assert_eq ~premise:Γ  (A.2, into hub; eq-only thru St.3, neq=St.4 delta)|   |
   |          |  new_eq / new_diseq callbacks                   (A.3, out of hub)       |   |
   |          |  per-class theory data, merge-combined          (A.4)                   |   |
   |          |  trailed; cancel_until-0 restorable             (B, F6)                 |   |
   |          +-----------^-------------------------------^-----------------------------+   |
   |                      | inject eq (A.2)               | notify new_eq/new_diseq (A.3)   |
   |                      | attach/merge data (A.4)       |                                 |
   |            +---------+---------+           +---------+---------+        +-----------+  |
   |            |  LIA (simplex+B&B) |           |  (EUF congruence  |        | datatypes |  |
   |            |  fixed-value eq    |           |   is the hub      |        |  arrays   |  |
   |            |  (mech 1, Stage 1) |           |   itself)         |        | (Stage 4) |  |
   |            +-------------------+            +-------------------+        +-----------+  |
   |                                                                                       |
   |   Final fallback: find_disagreement (bidirectional) → Theory.Split (ℤ-trichotomy)     |
   |                    UNCHANGED from today (A.5/A.6 Path 2; β-hash is a non-goal, §D)     |
   +---------------------------------------------------------------------------------------+
```

---

## B. SOUNDNESS FRAME — the fabric as TCB design

This is the hard part and is treated as trusted-computing-base design. The fabric introduces
one genuinely new hazard the seam did not have: **a theory-justified equality entering the
shared congruence structure directly, not via the SAT trail.** Today, cross-theory
equalities reach EUF only by the SAT core deciding a split literal, so the split literal is
in every downstream conflict's premises "by construction" (`combine.mli:65-76`). Removing
the round-trip removes that free provenance; the fabric must supply it explicitly. The
following obligations are numbered `RRR-F*` (fabric review obligations) and the invariants
`F1..F7`; each is stated as a checkable property with its enforcement and acceptance
evidence.

### B.1 F1 — Justification-at-injection (the #102 lesson, fabric-wide)

**Invariant F1.** Every equality a theory injects into the hub (A.2) carries
a premise set `Γ` that is (a) **recorded at injection time**, (b) **trail-precedence-valid**
— every member of `Γ` was assigned strictly before the merge is performed — and (c)
**never re-derived at ask time**. This is CONTRACT-EX (`euf.mli:84-91`,
`explanation.mli:27-30`) generalized from "equalities EUF propagates *out*" to "equalities
any theory injects *in*."

*Why this is the whole ballgame.* The #102 bug (`logs/euf-explain-bug.md`) was precisely an
ask-time re-derivation of an EUF-propagated reason over the *current* proof forest, which
could route through an edge asserted *after* the explained literal — violating precedence,
caught by `Sat.theory_explain_checked`, degrading to `unknown` (a completeness loss that was
sound only because the guard fired). The fix snapshotted the reason at propagation time
(`euf_adapter.ml` `explain_cache`, first-wins, pop-scoped). The fabric multiplies the number
of such edges (every cross-theory injected merge is one), so the same discipline must hold
for all of them, uniformly.

**RRR-F1 (obligation) — TWO distinct precedence surfaces, only one of which the forest
secures (R1, the sharpest correction).** The injected merge produces reasons on two paths,
and Rev 1 mis-located the guarantee for the second:

1. **The injected edge's OWN reason** (`s = w`, justified by `Γ`): stored at injection,
   expanded verbatim, never walked. Precedence holds by F1(b) (`Γ`'s members are all earlier,
   see the injection-time assertion below). This path the forest edge-order (`euf.mli:84-91`)
   is adequate for — it is a stored constant.
2. **The CONSEQUENCES of the injected merge** — the congruence merges the injection *fires*
   (inject `s = w` ⇒ `select(s) ~ select(w)` propagates as a watched-atom flip). **Their
   reasons are produced by the forest walk (`Euf.explain_implied`), which is EXACTLY the #102
   defect:** the walk can route through an edge asserted after the propagated literal. The #102
   fix did **not** make the walk precedence-safe — it installed
   `euf_adapter.explain_cache`, which snapshots the walk's result **at propagation time**
   (before the seam assigns the literal on the trail, so every premise is already strictly
   earlier), first-wins, pop-scoped (`euf_adapter.ml:152-171` `reason_of_implied` +
   `cache_reason`, and `:229-244` `explain` serves the cache; comment `:209-211` verbatim:
   *"snapshotted now … so ask-time `explain` serves the cache instead of re-deriving against a
   later forest"*).

Therefore F1's real obligation: **the fabric's injected merges must fire their consequent
watched-atom flips through the same `Euf.propagate` → `euf_adapter.cache_reason` path the #102
fix installed**, so each consequence's reason is snapshotted at propagation time, not walked at
ask time. Concretely, an injected merge is performed *inside* the `check` cycle so that the
very next `Euf.propagate` reports its consequent flips and the adapter caches their reasons
before `check` returns — identical to how EUF's own congruence propagations are already
handled. Rev 1's attribution of consequence-precedence to `euf.mli:84-91` (the forest) was
wrong; the guarantee is the cache.

**F1(b) injection-time assertion — a combinator assertion-order ledger, NOT `trail_pos`
(MED-5, corrected by Rev4-1b).** F1(b) must not be enforced *only* late by the ask-time guard —
a forward-referencing `Γ` would pass it (entailment still holds) and surface as a silent
`unknown`, not a loud bug at its source. Rev 3 phrased the injection-time check as
`max trail_pos(Γ) < current trail length`, but **`theory.mli`'s `assert_lit`/`check` do not
expose `trail_pos`** (the theory layer cannot read the SAT trail). Codex's own fix, adopted: the
**combinator maintains a monotone assertion-order ledger** — a counter incremented on each
`assert_lit`, recording each asserted `Lit`'s arrival ordinal. The injection-time assertion is
then **`max assertion_order(Γ) < current assertion_order counter`** (every `Γ` member arrived
before this injection), checkable entirely combinator-side from the `assert_lit` stream. No
`trail_pos` is read by any theory-layer code. (The SAT core's ask-time guard still uses the
trail *it* owns — that is the SAT core reading its own trail, not the theory layer.)

**Enforcement.** (i) `Sat.theory_explain_checked` rejects any returned premise the SAT core's
own trail order places at-or-after the explained literal → CONTRACT-POISON → `unknown`; it
guards the consequences too (they surface as ordinary `Propagations`). (ii) The combinator
assertion-order-ledger injection-time assertion above (MED-5, Rev4-1b). (iii) The
`OXSMT_EUF_SELF_CHECK` replay (`euf.mli:170-178`) extended to replay an injected edge's `Γ` into
a fresh independent closure and confirm `Γ` alone entails `s = w` under the injecting theory's
semantics (LIA: the Farkas combination of `Γ`'s bounds, via the F1-SEM witness accessor below).
(iv) **The re-armed #102 precedence mutant drives a CONSEQUENCE of an injected edge, not the
edge itself** (R1): inject `s = w`, let it propagate `f(s) ~ f(w)`, add a *later* merge that
shortens the path, and assert the *consequence's* cached reason excludes the later-asserted
premise. This exercises the actual multiplied #102 surface — the injected edge itself never
walks, so a mutant on it would be vacuous.

**F1-SEM — the SEMANTIC verifier, promoted to a numbered obligation (C1).** Precedence
(F1(b)/`theory_explain_checked`) checks *when* `Γ`'s members were assigned; it does NOT check
that `Γ` **entails** `s = w`. A `Γ` that is precedence-valid but *semantically insufficient*
(a dropped bound, so `Γ` no longer forces `s = w`) passes both the precedence guard
and — because the EUF replay *assumes* each given edge while replaying congruence — the EUF
self-check. That unjustified merge in a conflict is a wrong `unsat`. So F1 requires an
**independent injecting-theory semantic verifier** that consumes the actual witness, always-on at
injection, O(|Γ|) (distinct from the heavyweight env-gated EUF replay). The
mutant is a **weak-Γ** mutant: drop one bound so `Γ` is earlier-and-precedence-valid but
no longer entails `s = w`, and require an end-to-end **false `unsat`** unless the verifier
catches it (a *different* mutant from the late-Γ precedence mutant).

**F1-SEM witness math — TWO oriented derivations, not a four-bound sum (Rev5-B3, codex CONFIRM
Blocker 3, mathematically correct).** Rev 4 said "the four bounds `{s≥v, s≤v, w≥v, w≤v}` with unit
multipliers are the `s=w` Farkas witness — re-sum `Σ multiplierᵢ · boundᵢ`." That is **wrong**:
summing the four half-planes `s−v≤0`, `v−s≤0`, `w−v≤0`, `v−w≤0` gives `0 ≤ 0`, a tautology, not a
witness for `s = w`. The existing `Lia_farkas` contract checks a variable-free **contradiction**
(`Σ farkasᵢ · half-planeᵢ` is a positive constant, `lia.mli:46-51`); an equality is not a
contradiction, so that check does not apply as written. `s = w` decomposes into two **oriented
implications**, each a unit-multiplier Farkas derivation over a DIFFERENT two-bound subset:
- `s ≤ w` from `{s ≤ v, w ≥ v}` (i.e. `s ≤ v` and `v ≤ w`, chained);
- `s ≥ w` from `{s ≥ v, w ≤ v}` (i.e. `w ≤ v` and `v ≤ s`, chained).
The verifier constructs both oriented certificates and confirms **each implication** (`Γ_≤ ⊢_LIA
s≤w` and `Γ_≥ ⊢_LIA s≥w`), not a single variable-free contradiction; `s = w` follows from the two.
The weak-Γ mutant drops one of the four oriented bounds (e.g. `w≥v`), breaking the `s≤w` direction
→ false `unsat` unless the verifier's per-direction check catches it.

**C1 witness availability — `Lia.fixed_bounds` returning ACTIVE EXACT oriented bounds (Rev4-3
shape, Rev5-B3 contract).** The verifier needs, for a currently-fixed term, its value and its two
active bound tokens; `lia.mli` exposes no such accessor (`rational_value` gives the value only,
`propagate` gives premises but not the bound structure, `conflict` carries Farkas multipliers only
for a *conflict*). Rev 4 specs the **additive, non-frozen** accessor
`Lia.fixed_bounds : 'tok t -> Term.t -> (Rational.t * 'tok * 'tok) option` returning
`(value, lower≥premise, upper≤premise)` — shape unchanged. Rev 5 pins its CONTRACT so the two
oriented derivations are sound: the two returned tokens must be the **ACTIVE EXACT** bounds `s≥v`
and `s≤v` (both currently asserted AND tight to `v` — not a looser bound whose implied rational
value merely happens to be `v`), each precedence-valid, each carrying its real premise token
(recursively flattened to `Real` if it was itself fabric-derived). For `s`,`w` both fixed to `v`,
`fixed_bounds s = (v, s_lo, s_hi)` and `fixed_bounds w = (v, w_lo, w_hi)` supply exactly the
oriented subsets: `{s_hi, w_lo}` for `s≤w`, `{s_lo, w_hi}` for `s≥w`. The same two oriented
certificates are what the F7 sub-DAG's `¬Γ ∨ v_st` clause is checked against (§B.7 — the
`Lia_farkas` checker is extended to validate an oriented implication, not only a contradiction).
`lia.mli` is an engine interface (not frozen); this is additive, a Stage-1b prerequisite. **No
divergence from builder-stage1a's in-flight `fixed_bounds`** (its "value + oriented ≥ and ≤ tokens"
matches; the ACTIVE-EXACT guarantee is a contract on the bounds it returns, not a shape change).

**F1c — notify-OUT justification currency (C3), the reverse-direction obligation.** F1/F1-SEM
govern the inject-INTO-hub direction (A.2). The notify-OUT direction (A.3 callbacks) has its own
obligation: a `new_eq`/`new_diseq` callback delivers `(s, t, justification_handle, source)`, and
the receiving theory MUST attribute every fact it derives from that notification to
`justification_handle` (the hub edge's F2 handle). Without it, the receiving theory's later
conflict cannot name the premises behind the merge — the exact "provenance path" hole codex's
verdict centers on. Rules: `source` suppresses self-notification (idempotence); the callback
dependency graph is acyclic (queued non-reentrant drain, F5); and any handle a receiving theory
carries into a seam result MUST expand (via the §B.2 chokepoint) to real trail literals before
crossing the seam. Enforcement: a "callback-derived conflict names the merge premises" oracle
(assert a LIA conflict routed through a hub-notified equality lists `Γ`'s members) + a
self-notification-suppression test.

**Acceptance.** The consequence-driving precedence mutant is KILLED; the **weak-Γ mutant
produces a false `unsat` only when the F1-SEM verifier is disabled** (KILLED when enabled); the
extended self-check passes on the Stage-1 mathsat/Wisa acceptance set; the injection-time
assertion fires on a seeded forward-referencing `Γ`; the callback-provenance oracle is green;
MISMATCH=0 on the full corpus.

### B.2 F2 — The injected-edge token expands to a premise SET (the `Shared_eq` structure)

The EUF engine is parametric over an opaque per-edge premise token `'p` (`euf.mli:14-18`),
instantiated to `Lit.t` today (one literal per asserted equality). A fabric-injected
equality's justification is a **set** `Γ`, not a single literal. F2 fixes how the set is
carried.

**Invariant F2.** The injected edge's token resolves, at ask time, to the premise set `Γ`
recorded at injection time; a conflict or propagation whose explanation routes through the
edge lists `Γ`'s members (not the edge itself) in its premises. The materialized form is the
Nelson–Oppen clause `¬Γ ∨ (s = t)` — the existing (dormant) `Shared_eq {gamma; eq}` cert
leaf and `Rule_tag.Shared_eq` (`explanation.mli`).

**RRR-F2 (obligation) — the handle representation lives at TWO layers; only the second is the
through-line fix (C2 + B1, source-verified).** Rev 4/4.1 said "both children instantiate the
engine's opaque token to a shared `Real of Lit.t | Fabric of edge_id` sum, and `Combine`
expands." That is right for **one** of two distinct boundaries and silent about the other — the
gap codex's Blocker 1 named and the driver confirmed against `theory.mli`. Name both layers:

- **Layer 1 — the ENGINE↔adapter token (`'p`/`'tok`).** `Euf`'s `'p` (`euf.mli:14-18,40`) and
  `Lia`'s `'tok` (`lia.mli:44`) are fully polymorphic — "the engine never inspects a token; it
  only stores and returns it." Instantiating BOTH to a shared `justification = Real of Lit.t |
  Fabric of edge_id` (EUF's adapter today has `type prem = P_lit of Lit.t | P_axiom`,
  `euf_adapter.ml:12-14`, so it becomes `P_lit | P_axiom | P_fabric of edge_id`; LIA's adapter
  today has `lia : Lit.t Lia.t`, `lia_adapter.ml:23`, widened to `justification Lia.t`) needs **no
  `euf.mli`/`lia.mli` change** — the engines store/return the sum opaquely. This part of Rev 4 was
  correct. It lets `Euf.explain_implied` / `Lia.conflict.premises` HOLD a `Fabric` handle.

- **Layer 2 — the adapter→`Combine` explanation currency (THE through-line fix).** But holding the
  handle in the engine's token is useless if the **child→`Combine` return type cannot carry it**,
  and it cannot: `Combine` drives its children as **frozen `Theory.THEORY`** (LIA is literally
  `B : Theory.THEORY`, `combine.ml:23`; A is `CONGRUENCE_CHILD = Theory.THEORY + internalize_term`,
  `combine.mli:175-183`), and the frozen seam types are `check : … -> Conflict of Explanation.t |
  Propagations of Lit.t list | …` (`theory.mli:30,33`) and `explain : … -> Explanation.t`
  (`theory.mli:67`), with `Explanation.t = { premises : Lit.t list; rule : Rule_tag.t }`
  (`explanation.mli:31-33`). **A `Fabric edge_id` is not a `Lit.t` and cannot cross any of those.**
  Today `euf_adapter.reason_of_implied` / `lia_adapter.conflict_explanation` build an
  `Explanation.t` by `lits_of_prems` / straight from `c.premises` — a `Fabric` handle would be
  dropped (EUF's `lits_of_prems`, `euf_adapter.ml:144-150`, filters non-`P_lit`) or fail to type
  (LIA's `{ premises = c.premises; … }`, `:80-85`, needs `Lit.t list`). So Rev 4's "`Combine`
  expands what the child returned" has nothing to expand: the frozen return type already erased the
  handle. **This is the non-typeability Blocker 1 flagged.**

**The fix — a NON-FROZEN internal child-explanation interface, the `CONGRUENCE_CHILD` widening
generalized (B1).** `combine.mli:175-183` already sets the precedent: a child is a
`Theory.THEORY` PLUS an extra combinator-internal method (`internalize_term`), and `Combine` is
`Combine (R) (A : CONGRUENCE_CHILD) (B : Theory.THEORY)`. Rev 5 widens BOTH children the same
way — a non-frozen `FABRIC_CHILD` (and `FABRIC_CONGRUENCE_CHILD` for `A`) — so `Combine` can drive
them through a richer-than-`THEORY` signature that carries the fabric currency, while the ENGINE
still drives `Combine` (which IS a frozen `THEORY`) unchanged. Every type below is spellable in the
combinator/adapter layer (all non-frozen); none touches `theory.mli`/`explanation.mli`/`euf.mli`/
`lia.mli`:

```
(* combinator-internal, non-frozen *)
type edge_id                                        (* abstract, disjoint namespace by construction *)
type justification = Real of Lit.t | Fabric of edge_id
module Fabric_explanation : sig
  type t = { premises : justification list ; rule : Explanation.Rule_tag.t }
end
type fabric_check_result =
  | Sat
  | Propagations of Lit.t list                      (* propagated LITERALS are always real atoms;
                                                       only their REASONS carry Fabric handles *)
  | Conflict of Fabric_explanation.t                (* premises may contain Fabric handles *)
  | Split of Term.t list
module type FABRIC_CHILD = sig
  include Theory.THEORY                             (* create/register_atom/assert_lit/push/pop/model,
                                                       and check/explain for direct-drive tests *)
  val check_fabric   : t -> Theory.effort -> fabric_check_result
  val explain_fabric : t -> Lit.t -> Fabric_explanation.t
end
module type FABRIC_CONGRUENCE_CHILD = sig
  include FABRIC_CHILD
  val internalize_term : t -> Term.t -> unit        (* as CONGRUENCE_CHILD *)
end
(* Combine (R) (A : FABRIC_CONGRUENCE_CHILD) (B : FABRIC_CHILD) *)
```

`Combine` drives its children via `check_fabric`/`explain_fabric` (NOT the frozen `check`/`explain`,
which stay only for the ROUTER-tested hand-rolled children and direct-drive unit tests). The one new
premise-set behavior — expanding `Fabric` handles to real `Lit.t` — lives entirely in `Combine`,
which is where B1 wants it. Note `Propagations` stays `Lit.t list`: a propagated literal is always a
real atom, so only its lazily-fetched REASON (`explain_fabric`) and an eager `Conflict` can carry a
handle.

**The `edge_id → Γ` registry (C2), each invariant with its guard.** `Combine` owns a registry
mapping each injected `edge_id` to the premise set `Γ` recorded at injection, `Γ : justification
list` (it may itself contain `Fabric` handles — see recursive expansion below). Invariants:
**FIRST-successful-merge-WINS** (an `edge_id` records `Γ` at the first merge that connects the
classes; a later redundant injection of an already-`are_equal` pair is skipped and records NOTHING,
so it cannot overwrite — generalizing the #102 first-wins to the injected case); **same-frame
removal** (the `edge_id → Γ` entry is trailed on the `Combine` trail in the merge's frame, dropped
on its `pop`, F3); **bounded/idempotent** (one entry per live injected edge). Mutant: inject the
same pair with *different* `Γ` across nested frames and require first-wins (a last-wins overwrite
substitutes wrong premises → wrong `unsat`, the #102 recurrence).

**RRR-F2 — expansion at BOTH `Combine` seam-return points, recursive, first-wins-cached
(B1 + Rev4.1).** `Combine` expands every `Fabric` handle to real `Lit.t`s before an explanation
crosses to the SAT core, at the TWO points a fabric reason surfaces:

1. **Eager `Conflict` — `Combine.check`.** A child's `check_fabric` may return `Conflict of
   Fabric_explanation.t` whose `premises : justification list` hold `Fabric` handles; `Combine.check`
   expands them and returns the frozen `Conflict of Explanation.t` (all-`Lit.t` premises) to the
   engine.
2. **Lazy propagation reason — `Combine.explain`.** `theory.mli`'s `Propagations` carries NO
   explanation at check time (`theory.mli:30-32`: reason fetched later via `THEORY.explain`); the
   SAT core asks via `Combine.explain` (`combine.ml:617-625`), which today delegates to
   `A.explain`/`B.explain` and returns the child `Explanation.t` **verbatim**. Under the fabric
   `Combine.explain` calls the child's `explain_fabric`, gets a `Fabric_explanation.t`, and expands
   it to a frozen `Explanation.t`. Rev 4's "`Combine.check` is the single chokepoint" was
   mechanically wrong here — a lazy reason does not exist at `check` time — and it is the exact
   `combine.ml:617` bypass the same-model confirm named: an injected `s=w`'s congruence consequence
   `f(s)~f(w)`, whose cached reason holds a `Fabric` handle, would otherwise reach
   `Sat.theory_explain_checked` unexpanded.

**Recursive expansion, acyclicity ENFORCED not asserted (B1, corrected Rev6-4).** A `Fabric`
handle's `Γ` may itself contain `Fabric` handles (a callback-derived injection whose premise
references an earlier fabric edge). Expansion is recursive: replace each `Fabric e` by `registry[e]`,
repeat until every premise is `Real`. Rev 5 bounded this by "the acyclic edge-dependency DAG …
because F1(b)'s assertion-order ledger makes an edge reference only strictly-earlier edges" — but
the ledger orders `Lit.t` arrivals, NOT `edge_id`s, so that DAG claim was asserted, not mechanized.
Rev 6 enforces it directly: **`edge_id`s are allocated by a monotone counter, and an injected edge's
`Γ` may reference only STRICTLY-SMALLER, still-live `edge_id`s — checked at injection, one integer
comparison per referenced handle (reject → `Combination_unsound` → `unknown`).** Expansion then
walks a strictly-decreasing `edge_id` chain with a visited-set, and **fails closed** (→ `unknown`)
on a missing or not-smaller handle. Termination and acyclicity are now by construction (a
strictly-decreasing bounded-below integer chain cannot cycle), and first-wins keeps each
`edge_id → Γ` immutable. The bound is the number of live fabric edges. Mutant: inject an edge whose
`Γ` cites a handle with an equal-or-larger `edge_id` — must be REJECTED at injection (a verdict
effect via `unknown`), never reach expansion and loop.

**The combined-reason cache is snapshot-at-PROPAGATION, first-wins (B1, corrected Rev6-3 — the
#102 discipline).** So that all consumers — solve-time 1UIP, the lazy `explain` path, cert-time
materialization — cite BYTE-IDENTICAL `Γ` for the same propagated literal, `Combine` records the
fully-expanded `Lit.t list` keyed by the literal **at the moment the propagation is RETURNED** (in
`Combine.check`/propagate, the same instant the child adapter snapshots its own `explain_cache`
reason), FIRST-wins, origin-frame trailed on the `Combine` trail. Rev 5's "cache on first
*expansion*" was wrong for exactly the #102 reason (§B.8): a child can re-propagate the same literal
before its first `explain`, and with a last-wins `propagated_by` (`combine.ml:89`) a later,
precedence-INVALID reason would be the one cached and served. Rev 6 fixes both: the combined-reason
cache and the `propagated_by` entry for a fabric-derived reason are **first-wins at propagation
time** — the first propagation's reason is the precedence-valid one (its `Γ` was all asserted before
the literal hit the trail), exactly as `euf_adapter.cache_reason`/`lia_adapter.cache_reason` already
do at the child level (which stay snapshot-at-propagation, the #102 fix intact — the residual was
only the NEW `Combine`-layer cache). **Rev 6.1 pin — the pop lifecycle:** both the combined-reason
cache AND the `propagated_by` entry for a fabric-reason literal are **origin-frame TRAILED and removed
on `pop` of that frame**, exactly like the child `explain_cache`s they mirror
(`euf_adapter.cache_reason` push `euf_adapter.ml:184-186`, uncache-on-pop `:262`). Today
`propagated_by` is grow-only and untrailed (`combine.ml:89`, `record_props` `:399-400`; `Combine.pop`
`:632-649` unwinds only `pin_frames`); leaving it so, first-wins would strand a **stale post-pop
owner** that blocks the correct re-record after backtrack, and `Combine.explain` would route a
post-pop re-propagation of the same literal to the wrong child — a wrong-owner 1UIP input on the very
#102 surface. Trailing removes the stale owner on `pop`, so a re-propagation records the fresh one. Eager conflicts are returned immediately (not literal-keyed)
but read the same registry, so they agree by construction. This is the "same expansion chokepoint,
not same walk" F4 requires. Enforcement: the existing #102-consequence precedence mutant (§B.1
enforcement (iv)) drives a consequence of an injected edge and re-propagates it under a later
merge — a cache-at-explanation (last-wins) implementation would serve the later reason and the
mutant KILLS it; the mutant is stated to cover the `Combine`-layer cache, not only the child cache.

**The per-adapter caches are unchanged in shape; the ONE new behavior is `Combine`'s (R2).** The
child `explain_cache`s keep storing the child's own reason (now a `Fabric_explanation.t`, first-wins,
pop-scoped). `euf_adapter.mli:22-24`'s "adds no new reasoning; it only relabels premise tokens"
stays TRUE — the adapters still only relabel (they carry the opaque `Fabric` handle through; they do
not expand it). The premise-set **expansion** and the combined-reason cache are `Combine`'s new
reasoning, and it is `Combine` (non-frozen) that takes the Stage-1b TCB review line, not the adapters.

**Empty-`Γ`: tripwire for Stage 1, theory-axiom witness for Stage 4 (M1).** Rev 1 declared
empty `Γ` unconditionally unsound → raise. That is right for a LIA fixed-value equality (a fix
*always* rests on ≥1 bound), so the empty-`Γ` tripwire stays a hard raise for Stage 1
(analogue of the AP4 tripwire, `euf_adapter.ml:163-169`, survives `-noassert`). But codex is
right that the *generic* fabric contract Stage 4 inherits has legitimate empty-premise facts:
array read-over-write yields an unconditional equality, datatype distinctness is a theorem, not
a hypothesis. So the refined rule: **empty `Γ` is permitted ONLY with a checkable theory-axiom
witness** (a `Rule_tag` marking the fact as an unconditional theory theorem, carrying the
axiom-rule identity the cert checker can re-derive), and rejected otherwise. "Unconditional with
a checkable witness" and "missing justification" are distinguished by the presence of that
witness. Stage 1 registers no axiom witnesses, so its empty-`Γ` tripwire is the empty instance
of the general rule; Stage 4 theories supply witnesses. Tests: a positive (empty `Γ` + valid
array-axiom witness accepted) and a forged-empty (empty `Γ`, no witness → raise).

**Enforcement.** A "no bare handle escapes" test: every premise in any `Explanation.t`
crossing the seam is a real `Lit.t` (a `P_fabric` handle expanded to `Γ`, never surfaced) —
mirrors the euf_adapter's existing rule that the `true ≠ false` axiom token is filtered out
of explanations (`euf_adapter.mli:13-16`, `lits_of_prems` at `euf_adapter.ml:142-150`). The
empty-`Γ` tripwire (Stage 1) / theory-axiom-witness rule (Stage 4) above.

**Acceptance.** Handle-leak test green; Stage-1 empty-`Γ` tripwire fires on a seeded fault;
Stage-4 empty-`Γ`+witness positive accepted and forged-empty rejected; a chokepoint test
asserts solve-time and cert-time consumers cite byte-identical `Γ` for the same injected edge;
the first-wins duplicate-`Γ`-across-frames mutant KILLED.

### B.3 F3 — Backtracking: all fabric state trailed, `cancel_until 0` restorable, undone in order

**Invariant F3 (SAT exception-safety, extended).** Every fabric mutation — an injected
merge, a `new_eq`/`new_diseq` callback's downstream state change, an attached/merged
`theory_var` datum, a justification-registry entry — is trailed and fully restored by
`pop`/`cancel_until 0`, undone in reverse order. "Recovery" means *restorable to the
`cancel_until 0` state*, which (per the SAT exception-safety invariant, memory
`sat-exception-safety-invariant`) means every piece of state is either trailed or provably
reconstructible — not merely "the trail is consistent."

**RRR-F3.** The hub already trails union-find, explanation forest, congruence table,
disequalities, and registered e-nodes with level-granular push/pop (`euf.mli:153-160`). F3
extends the trail to: (i) injected-edge justification records (pop drops the frame's
entries, exactly as the euf_adapter `frames` list does today, `euf_adapter.ml:246-254`);
(ii) per-class theory data across a **destructive** merge — the trail entry must **snapshot
BOTH pre-merge payloads** (see §C Stage 0 merge-undo, R4/MED-2), not "the class's data";
(iii) any theory-side state a callback mutated. The ordering requirement is discharged by
the Stage-0 mechanism below, not asserted here.

**How the cross-module ordering is MECHANIZED (not asserted) — see §C Stage 0.** Rev 1
reduced cross-module pop-ordering to a bare "pop as a unit, newest-first" assertion across
four private trails. Rev 2 replaces that with a stated mechanism (§C Stage 0 RRR-F0): the
per-module trails own **disjoint** state and share frame boundaries, so they compose under a
proven disjointness invariant; the ONE genuinely-coupled piece (per-class data, whose undo
order is tied to the merge that combined it) is **co-located on the merge's own trail** via a
snapshot-both-inputs entry + the owning theory's `unmerge`. So F3 is not "one global trail
owns everything"; it is "disjoint trails compose + coupled state rides its coupling event's
trail." The global-epoch protocol is the named fallback if a future theory violates
disjointness. Stage 0 is a prerequisite of F3 at the cross-module stages.

**The specific hazard to test.** The ADR-0010 grow-only combinator interface set can outlive
the child e-node that backed a member (EUF truncates e-nodes on pop; `combine.ml` §3.3
both-valued skip guards it). The fabric adds the symmetric hazard: an injected edge whose
`Γ` references a bound literal that a `pop` retracted. F3 requires the edge and its `Γ`
record to be trailed in the SAME frame as the merge, so the pop that retracts the bound also
drops the edge — they cannot desynchronize.

**Enforcement.** A push/pop-reassert oracle (analogue of `test_explain_cache_pushpop`,
`euf_adapter_test.ml:925`, and the ADR-0010 §6 push/pop-reassert fixture): inject a
cross-theory equality in a frame, `pop` over it, re-assert the fixing bounds, and require an
*identical* verdict and model — plus a mutant that strands the injected edge past its frame
(the ADR-0010 §6 "backjump OVER the instance-creation branch, require a VERDICT effect"
discipline: the mutant must produce a *wrong* verdict, not merely a stranded record).

**Acceptance.** Push/pop-reassert oracle green; strand-the-edge mutant KILLED with a verdict
effect.

### B.4 F4 — Conflict analysis consumes fabric justifications (explanation substitution)

This is where #102-class bugs breed. When the SAT core's 1UIP analysis or a theory conflict
needs the reason for a literal whose derivation crossed a fabric-injected edge, the
explanation must **substitute** the edge's `Γ` in place of the edge, transitively, until
every premise is a real trail literal.

**Invariant F4.** For any conflict/propagation the fabric surfaces at the seam
(`Conflict of Explanation.t` or a `Propagations` element's lazy `explain`), the returned
premise set contains only literals assigned strictly before the explained fact (CONTRACT-EX,
F1) and alone entails it (soundness). Substitution across injected edges is **precedence-
preserving**: because each `Γ` was precedence-valid at injection (F1) and the injection
preceded any consequence, the substituted premises are all earlier than the consequence.

**RRR-F4 (restated per R2 — "same expansion chokepoint," not "same walk").** The E3
`Theory_prop` materialization walks the forcing chain, materializing each crossed
`Theory_prop` literal's reason clause via `theory_reason_clause`
(`adr-certificates-draft.md:520-576`). Rev 1 said the fabric "uses the same walk as E3" — that
conflates two different code paths. The correct requirement (R2/HIGH-1b): solve-time conflict
analysis and cert-time materialization are **different code that reads the SAME F2 expansion
chokepoint** (§B.2): solve-time serves the cached expanded `Γ`; cert-time materializes the
`Shared_eq` clause via `theory_reason_clause` from the same registry. They agree because they
cite the same recorded `Γ`, not because they share a traversal. This certificate-vs-solver
`Γ`-equivalence is what the acceptance pins.

**Enforcement.** (i) The self-check replay (F1) already re-verifies each produced
explanation. (ii) An explanation-substitution mutant: drop one `Γ`-member during expansion
(the analogue of the euf `explain` premise-drop mutants) — must be caught by
`theory_explain_checked` or the self-check. (iii) A soundness oracle over the mathsat/Wisa
UNSATs through the real stack: each must reach UNSAT (drives the injected-edge path), and the
Lean gate certifies the original VC (MISMATCH=0).

**Acceptance.** Substitution mutant KILLED; mathsat/Wisa UNSATs certified through the gate.

### B.5 F5 — Callback cascade termination and determinism

**Invariant F5 (CONDITIONED per R3 — no longer asserted fabric-wide).** The `new_eq`/
`new_diseq` cascade (A.3) terminates in one hub-closure step, and its observable output (merge
order, injected equalities, chosen conflict) is a deterministic function of the register/
assert/check/push/pop sequence (I6). **The termination argument holds only for the
no-new-terms case (Stages 1–3), which the ADR enforces by restriction; Stage 4's term-creating
callbacks get a separate fuel-bounded argument.**

**RRR-F5 (a) — no-new-terms case (Stages 1–3), proven with a queued closure + novelty keys
(H1).** In Stages 1–3 **callbacks may NOT register new terms** (a stated, enforced restriction),
and the cascade is a **queued, non-reentrant closure** (§A.3, §C Stage 0 item 6): callbacks
enqueue events, the hub drains the queue to a fixpoint, no callback recurses inside another.
Termination then holds under a well-founded measure — but the measure is NOT "distinct classes
strictly decrease" (Rev 1's error: that covers merges but **not disequality insertion**, which
does not merge classes, and **not redundant echoes**). The correct measure is
**lexicographic (distinct-class count ↓, then novel-event budget ↓)** enforced by **novelty
keys**: each eq / diseq / notification event has a key, and a repeat is a **no-op** (already in
the class structure / already-recorded diseq). So a `new_diseq`-echo cannot re-enqueue forever
(the second identical diseq is dropped), and a redundant equality injection is skipped by
`Euf.assert_eq`'s already-connected check. With the term set fixed (restriction) and every
event either strictly reducing classes or being a novel-but-finite diseq/notification, the queue
drains in finitely many steps. The measure **includes attached-data transitions** (a data-merge
is a class merge, already counted; a data update without a merge must carry a novelty key too).

**RRR-F5 (b) — term-creating callbacks (Stage 4), fuel-bounded.** Stage-4 procedures DO
register terms mid-cascade (datatype selector propagation mints `sel(c)`; array
read-over-write mints `select(a,j)`), which can trigger fresh congruence merges → fresh
callbacks → more terms — the "distinct classes decrease" argument does not bound a cascade
that also *increases* the node count. Stage 4 therefore carries a **fuel bound with a
hard-stop enforcement**, not merely the goldens observability counter Rev 1 offered: a
per-`check` cascade-fuel budget (analogue of the existing split budget, `cdclt.mli:42-43`
`Split_budget_exceeded`); on exhaustion the query **degrades to `unknown`** (sound incomplete,
I8), never loops. The fuel is a hard cap that terminates the cascade, distinct from the
goldens counter that merely *observes* depth. This is scoped to Stage 4; Stages 1–3 cannot
reach it (restriction (a)).

**Fuel exhaustion must not return `unknown` over INCONSISTENT state (Rev4-4, parity with H5).**
A cascade cut off mid-drain has **half-applied mutations** — some injected merges done, others
queued-but-not-run — so the hub/theory state is not a consistent snapshot of any complete
round. Returning `unknown` from *that* state is unsafe if the state is reused (a later `check`
would reason from a half-merged e-graph). So on fuel exhaustion the fabric **first `rollback_to`
the check-entry watermark** (the intra-check, truncating rewind of §C Stage 0 item 5 — reverses
AND removes every hub- and child-trail entry the cascade added, on every affected trail; invariant
DR then guarantees the subsequent decision `pop` never re-reverses them) so state is consistent,
**OR poisons the instance** — and only then returns `unknown`. This is exactly the H5
mid-cascade-exception rule (never continue over half-applied state), applied to the fuel-cutoff
path. Enforcement: a fuel-cutoff fixture that, after the `unknown`, re-runs a `check` and asserts
the state is the pre-cascade one (or the instance is poisoned) — a mutant that skips the
`rollback_to` shows a wrong verdict on the follow-up `check`.

**Determinism (both cases).** Callbacks fire in a fixed order (registration order of theories;
within a theory, e-node id / `Term` tag order, never `Hashtbl` traversal — the existing EUF
determinism discipline, `euf.mli:26-31`).

**Enforcement.** A determinism golden (byte-identical decisions/verdict/counters across two
runs on a pinned mathsat file, per I6 and `z3-combination-study.md:304-308`: a nondeterministic
β would silently reorder injections). For Stages 1–3, a **no-new-terms assertion** in the
callback path (a callback that registers a term fails loud); a **diseq-echo mutant** (disable
the diseq novelty key → the cascade must be shown to loop / hit a bound, not silently
diverge); a **redundant-injection mutant** (disable the already-connected skip → the injection
count must be caught growing). For Stage 4, a **fuel-exhaustion fixture** (a constructed
read-over-write cascade that hits the cap degrades to `unknown`, does not hang). The
cascade-depth counter stays in the goldens as observability (bucketed, I5).

**Acceptance.** Determinism golden byte-identical; no-new-terms assertion fires on a seeded
term-registering callback (Stages 1–3); diseq-echo and redundant-injection mutants KILLED;
Stage-4 fuel-exhaustion fixture degrades to `unknown`.

### B.5a A4-erratum contract for the fabric (R9)

**Invariant.** The fabric registers exactly the A4-erratum set and no more: callbacks fire
**only on boundary-node classes**, and `theory_var` data attaches **only to boundary classes**.
A pure-arithmetic term that never sits under an uninterpreted symbol gets no e-node (A4-erratum,
DESIGN.md), so it is neither a callback target nor a `theory_var` carrier.

**Where the proportionality lives, and the CORRECTED size metric (Rev4-7 level distinction +
Rev5-NEW-MED metric fix).** `euf.ml:369-399` / `euf.mli:47-58` `register_term` DOES internalize a
term's **full subterm closure** (the codex premise is accurate, not stale post-#65). Two distinct
quantities must not be conflated (Rev 4 conflated them, codex CONFIRM A4-metric MED):
- **The callback / `theory_var` DOMAIN is boundary-nodes-only** — a combinator-level property: the
  combinator fires callbacks and attaches `theory_var` data on **boundary classes** only. This is
  the SOUNDNESS claim (a non-boundary merge cannot mislead a theory), and it is correct.
- **The e-graph SIZE is NOT `#UF-applications + #boundary nodes` (corrected again, Rev6-FWD-3).**
  Registering a maximal UF application `f(e)` pulls in `e`'s **full subterm closure**, including the
  **non-boundary arithmetic descendants** of a large argument (`f(x + big_sum)` internalizes the
  whole `x+big_sum` DAG). AND — the case Rev 5 still under-counted — a shared Int equality routed
  `Both` is `A.register_atom`'d, so `Euf.register_term` internalizes the equality **and its full
  arithmetic closure EVEN WITH NO UF APPLICATION PRESENT** (`combine.ml:345-347`:
  `R.Both -> A.register_atom t.a atom term; …`). So the honest size metric is: **the e-graph is
  proportional to the subterm closure of EVERY congruence-child registration root** — every maximal
  UF application AND every `Both`-routed atom (equalities over shared terms) plus their full
  argument/side closures — NOT to `#UF-applications + #boundary nodes`, and NOT only to maximal UF
  applications. A pure-arithmetic term that is NEVER a registration root (never under a UF and never
  a side of a shared equality) still gets NO e-node (the "UF-free skip," `internalize_uf_subterms`,
  `combine.ml:321-328`), so the 5,000-pure-arith-terms cliff A4-erratum evicted stays evicted; but a
  pure-arith equality's operands ARE registration roots and are in the e-graph, and the metric must
  say so.
A **boundary-marker mechanism** that would internalize only the boundary nodes of a registration
root and prune the interior arith descendants is **named future work**, not claimed as delivered —
it would tighten the size metric back toward `#registration-roots + #boundary nodes`, at the cost of
a marker-tracking pass. Rev 5/6 do not claim it. This note records the Rev4-7 level distinction (a
reader must not read §B.5a as "`register_term` is selective") AND the corrected size metric that
counts every congruence-child registration root (Rev6-FWD-3), a cost-accounting correction with no
soundness/wrong-verdict impact.

**Why this is load-bearing (the unaddressed edge the review named).** Stage 2's callback fires
when "two classes carrying Int terms shared with LIA" merge. If making LIA *hear about* merges
required every LIA variable to carry an e-node (so any two becoming equal could notify LIA),
that reintroduces the 5,000-arith-terms → 5,000-e-nodes cliff A4-erratum evicted. The fabric
must NOT do that: only boundary Int terms (already e-nodes, because they appear under a UF or in
an equality atom) are callback targets. If a future Stage-2 throughput need genuinely required
hearing about non-boundary merges, that is an **A4-erratum conflict to surface, not bury** — it
would be raised as an explicit erratum, not silently added.

**Enforcement.** The A4-erratum under-inclusion mutant (a boundary term missing its e-node)
carries over from the euf-perf lane; plus a "no non-boundary class carries `theory_var` data /
receives a callback" property test (the boundary-only DOMAIN claim). **Acceptance.** Both
KILLED/green; the e-graph-size counter stays proportional to the **subterm closure of every
congruence-child registration root** (maximal UF apps AND `Both`-routed shared-equality atoms + their
closures), NOT to #boundary-nodes and NOT only to maximal UF apps (Rev5-NEW-MED + Rev6-FWD-3) — a
pure-arith term that is never a registration root contributes zero e-nodes (the A4-erratum goldens
metric, corrected; a cost-accounting metric, no soundness impact).

### B.6 F6 — Poison/exception safety (CONTRACT-POISON preserved)

**Invariant F6.** Any exception escaping a fabric operation (a `Rational.Overflow` from LIA
during a fixed-value computation, a `Term.Overflow` from split construction, a
`Combination_unsound` from an unroutable state) bricks the instance and degrades the query to
`unknown` (I8, CONTRACT-POISON) — never a crash, never a verdict from corrupt state.

**RRR-F6.** Two carry-over hazards from #142 that any equality-minting realization inherits
(`logs/uflia-search-codex.md`):
- **H1 — `Term.Overflow` escaping split construction.** `equality_split` →
  `Context.lt/gt` subtracts operand constants and can raise `Term.Overflow` on `x+max_int`
  (`node.ml:284-292`); today the injection site can sit *outside* the `raw_solve`
  CONTRACT-POISON firewall (`session.ml:661-666`) and crash on satisfiable input. **The
  fabric must make every equality/disequality construction total: catch `Term.Overflow` /
  `Combination_unsound` per-pair at the construction site and skip that pair (completeness-
  only), OR ensure the construction runs inside the poison firewall.** Since fabric
  injections happen in `check` (inside `raw_solve`), they are inside the firewall — but the
  ADR mandates the per-pair catch regardless, so a skipped pair is a clean completeness loss
  rather than a whole-query degrade.
- **L1 — non-atomic state flags.** Set any "injected"/"bridged" bookkeeping flag only
  *after* a successful, side-effect-complete injection, or make it per-pair, so a raised
  construction leaves no half-applied state a retry would skip.
- **H5 — injection is TRANSACTIONAL: construct-then-commit, "catch only before first
  mutation."** The per-pair catch (H1) is only safe if the exception can fire **before any hub
  mutation**, never after a partial merge. Rev 1 did not state this boundary; codex (H5) is right
  that a caught-and-skipped pair that *already mutated the hub* is not a "clean completeness
  loss." The design makes it clean by construction: **all fallible work — `Context.lt/gt` term
  construction, `Γ` extraction, the Farkas re-sum (F1-SEM) — happens BEFORE `Euf.assert_eq`
  touches the union-find.** Once the merge begins it is pure trailed union-find + congruence
  (no user-code, no overflow), so no exception can fire mid-mutation. A skipped pair therefore
  leaves ZERO partial state. If a future theory's combine *can* raise mid-mutation, that
  injection must be wrapped as a transaction that rolls back to the pre-merge trail watermark
  (or poisons); it may NOT be locally skipped.

**Enforcement.** The H1 overflow fixture (`x:Int`, `p:Int→Bool`, `hi=x+max_int`,
`lo=x+min_int`, assert `p(hi)`, `p(lo)`; satisfiable) must return `sat`/`unknown`, never
crash — through the fabric injection path. LIA `Rational.Overflow` → `unknown` counted via
`overflows_to_unknown`. Poison-reuse mutant: a bricked instance raises on the next public
entry, never returns a verdict. **L1 mutant** (set the flag before the injection completes → a
retry skips re-injection → a verdict effect). **Mid-cascade-exception mutant** (inject a fault
*after* the hub merge but before callbacks/registry complete → the instance must poison to
`unknown`, NOT continue with half-applied state).

**Acceptance.** H1 fixture no-crash; poison-reuse, L1, and mid-cascade-exception mutants all
KILLED with a verdict effect.

### B.7 F7 — Certificate emission for fabric propagations (ADR-0013 delta)

**Invariant F7 (EMISSION-ONLY until #153, per R10/MED-4; EQUALITY-ONLY through Stage 3, Rev5-NEW-HIGH).**
Every fabric-injected **equality** is **EMITTABLE** as a certificate step (I4, ADR-0013): the
`Shared_eq {gamma; eq}` theory leaf = the clause `¬Γ ∨ (s = t)`, emitted as a `Theory` intro and
consumed by the E3 `Theory_prop` materialization on the Unsat path. It is **emittable, not yet
*checkable***: cert is at step-1 emission only (#134); the replay checker is #153 (future). **Until
#153, the uncorrelated soundness backstop for a fabric UNSAT is the whole-VC Lean/grind path**,
exactly as for every UNSAT the solver produces today — the `Shared_eq` emission does not yet
independently check a fabric UNSAT. F7's obligation is that the emission is *well-formed and
future-checkable* (the recorded `Γ` composes with the existing `Lia_farkas` checker once #153
lands), not that it is checked now.

**Disequality replay is DEFERRED (Rev5-NEW-HIGH).** The `v_st` replay rule below (Rev4-2) covers an
assumed-**equality** edge only. Because the fabric injects only equalities through Stage 3 (§A.2), a
fabric-derived disequality edge does not exist yet, so its replay rule is out of scope here. When a
Stage-4 datatype needs to inject a justified disequality (`assert_neq ~premise:Γ`), the Stage-4
diseq-injection ADR delta must add the **`v_neq`-analogue**: a fresh virtual proposition meaning
`s ≠ t`, assumed true on the edge, used in the EUF proof as an assumed **disequality** binding
(a violated-disequality conflict cites it), discharged by resolution against the witnessed
`¬Γ ∨ v_neq` (whatever theory-implication witness the injecting theory supplies — constructor
distinctness for datatypes). That delta is named here so the equality-only v1 is honest, not silent.

**The gap Rev 1 missed (C4, code-verified — OQ4 CLOSED to "specify now").** Rev 1 claimed the
fabric edge is recorded "through the existing `on_theory_clause` hook." Codex verified against
source that this does not work, on three counts: (i) `recorder.theory_event = {id; clause;
role}` and `Sat.on_theory_clause` carry **no tag and no witness field** (`recorder.mli:36-40`,
`sat.mli:159`, role ∈ {Reason,Conflict}) — there is nowhere to record the `Shared_eq` tag or the
Farkas witness; (ii) `Rule_tag.Shared_eq` is **payload-free** (`explanation.mli:23`) and
witnesses live off-core, so `{gamma; eq}` is a cert-grammar construct the *solve path* does not
carry; (iii) a hub edge with **no Boolean atom** has no `lit` to be `Shared_eq.eq`, and E3 walks
only **SAT-trail literals**, so a purely-internal fabric edge is invisible to it. Net: a
fabric-derived `unsat` is currently **uncertifiable** — it would silently exit the Lean gate's
coverage. This is the concrete core of codex's verdict, and the ADR must close it now, not defer
to OQ4.

**RRR-F7 — the end-to-end certificate path (concrete additive ADR-0013 delta, co-designed with
the #153 step-2 checker).**

1. **A parallel fabric-event stream (the additive delta; NO frozen change).** A fabric-injected
   edge that never becomes a SAT literal cannot be recorded via `sat.mli`'s trace (sat never sees
   it, and `sat.mli` is frozen). Instead the **combinator** (non-frozen) emits a fabric event to
   the recorder over a new trace-style hook it owns: `on_fabric_eq { edge_id; s; t; gamma :
   Lit.t list; witness }`, where `witness` is the off-core Farkas multiplier vector (lives in
   `smt/certificate/`, never on the frozen core — same discipline as every other cert witness,
   `explanation.mli:10-15`). Trace-gated exactly like `sat`'s (`set_trace`): unset ⇒ bit-identical,
   zero cost. This adds a recorder event kind + a combinator trace hook — both non-frozen — and
   touches neither `sat.mli` nor `explanation.mli` payloads.
2. **The certificate sub-DAG + the concrete checker rule the #153 checker implements (Rev4-2 —
   spelled out, not gestured; joining by `edge_id` alone is insufficient).** For an injected
   `s = t` on an Unsat forcing chain, the replay rule is:
   - **Virtual proposition.** Because a fabric edge may have no Boolean atom, the checker
     introduces a **fresh virtual proposition `v_st`** for the equality `s = t`, keyed by
     `edge_id` (this is how the fabric-event stream joins the resolution skeleton — the `edge_id`
     tells the checker *which* `v_st` to mint and where it is used, which a bare `edge_id` match
     could not).
   - **Polarity.** `v_st` is introduced with the meaning "`s = t`" and is **assumed true** on the
     edge (the injection asserts the equality). The witnessed LIA implication is the clause
     **`¬Γ ∨ v_st`** — checked by validating the **two oriented Farkas implications** the witness
     carries (`Γ_≤ ⊢_LIA s≤w` from `{s≤v, w≥v}` and `Γ_≥ ⊢_LIA s≥w` from `{s≥v, w≤v}`, the
     `Lia.fixed_bounds` ACTIVE-EXACT oriented bounds of Rev5-B3), which together give `Γ ⊢_LIA
     (s = t)`. This requires the `Lia_farkas` checker to be **extended to validate an oriented
     implication**, not only a variable-free contradiction (Rev5-B3) — the same extension the
     always-on F1-SEM verifier uses; a four-bound sum `0≤0` is NOT accepted.
   - **Endpoint binding.** The EUF proof step treats `v_st` as an **assumed equality edge binding
     the class of `s` to the class of `t`**; the congruence chain that derives the consequence
     `c` cites `v_st` among its antecedents, producing an EUF leaf clause **`¬v_st ∨ … ∨ c`**
     (the standard congruence-chain leaf, with `v_st` as one hypothesis).
   - **Assumption discharge.** The final **resolution eliminates `v_st`**: resolve the EUF leaf
     `¬v_st ∨ … ∨ c` against the LIA-implication clause `¬Γ ∨ v_st`, yielding `¬Γ ∨ … ∨ c` —
     `v_st` discharged, leaving only real premises `Γ`. No un-discharged virtual proposition may
     survive into the final `[||]`; the checker rejects a proof where a `v_st` is used but never
     discharged against its `¬Γ ∨ v_st`.
   This is a leaf *composition* (LIA-implication clause + EUF-use leaf + discharge resolution),
   which a single `Theory` intro cannot express — hence the explicit sub-DAG. It is ADR content
   the #153 checker implements verbatim, not a deferred design.
3. **Emittable ⇒ checkable is a co-design obligation with #153.** The #153 step-2 replay checker
   MUST consume this fabric-event stream and validate the sub-DAG (OCaml native checker and Lean
   replay both). Until #153 the emission is well-formed and future-checkable (F7 emission-only,
   above), and the whole-VC Lean gate is the backstop. This is now a **hard prerequisite
   coupling**: the fabric-event stream schema and the #153 checker are designed together, and a
   Stage-1b builder that emits an event the #153 checker cannot join has not met F7.
4. **Honeypots.** Wrong-`Γ` and wrong-multiplier honeypots: a recorded event whose `witness` does
   not re-sum to force `s = t` must be REJECTED by the checker (the cert analogue of the F1-SEM
   weak-`Γ` solve-time mutant).

**Enforcement.** A `cert_emit_test` case (analogue of `smt/certificate/test/cert_emit_test.ml`)
drives a mathsat-shaped UNSAT through the fabric and asserts an `on_fabric_eq` event is recorded
with the correct `{edge_id; s; t; Γ; witness}`, and that the recorded stream resolves all
citations by `edge_id` (`recorder.unresolved_citations = 0`, extended to fabric events). The
wrong-multiplier honeypot is REJECTED by the (native, #153) checker.

**Acceptance.** Cert emit test green for a fabric UNSAT; fabric-event citations all resolve;
wrong-multiplier honeypot rejected; trace-off solve bit-identical to pre-fabric (recorder off
the hot path). Full checkability is gated on #153 landing (M4).

### B.8 Summary of the soundness contract

The fabric is sound iff **F1–F7 hold simultaneously**, and the load-bearing one is F1
(justification-at-injection) because F2/F4/F7 all consume the premise sets F1 records, and
F3/F6 bound the damage of any failure to `unknown`. The single sentence a Stage-1 builder
must internalize, corrected per R1: *any equality entering the shared congruence structure
carries a premise set recorded at the moment of the merge, trail-precedence-valid, never
re-derived — AND its CONSEQUENCES' reasons are snapshotted at propagation time via the same
`euf_adapter.cache_reason` path the #102 fix installed, never walked from a later forest. The
injected edge is a stored constant; the multiplied #102 surface is its consequences, and the
cache — not the forest edge-order — is what secures them.*

The `Combine`-layer caches that mirror this discipline — the combined-reason cache AND the
`propagated_by` owner map for fabric-reason literals — are therefore **first-wins, snapshotted at
propagation time, and origin-frame trailed** (removed on `pop` of their frame), exactly like the
child `explain_cache`s (Rev6-3 + Rev6.1). A grow-only untrailed owner map would strand a stale
post-pop owner and route a re-propagation to the wrong child — the same #102 surface, one layer up.

---

## C. MIGRATION PLAN — staged, headline-protected

Each stage names what unfreezes, what gates it, what it can break, and its fallback. Per A8
(build is the evidence) the measured 2s-protocol sweep on the branch IS the experiment; a
negative result parks the stage.

### C.0 Two distinct gates — do not conflate them (M2)

Rev 1 wrote "MISMATCH=0" as if it were one gate; codex (M2) is right it was doing two
different jobs. Every stage below is gated on BOTH, named separately:

- **(G-sound) label/oracle soundness.** The repo's `mismatch_count`: our *definite* verdicts
  vs the benchmark `:status` / Lean gate. It **ignores `unknown`** and does **not** pair a
  branch result against trunk. It answers "did we ever contradict a known-true label?" — the
  soundness tripwire (A6). **G-sound must be 0** at every stage, always.
- **(G-equiv) branch-vs-trunk outcome equivalence.** A *paired* diff of the stage branch
  against its pinned trunk baseline over the same corpus, including `unknown`, models, counters,
  and (where emitted) certificates. It answers "did this stage change any outcome, and are the
  changes the intended ones?" G-equiv is NOT expected to be empty — Stage 1's whole point is
  `unknown → unsat` transitions — so each stage ships a **pinned manifest of permitted
  transitions** (e.g. Stage 1: only `unknown → {sat,unsat}` on the mathsat/Wisa family, no
  `X → unknown` regressions; Stages 1b/2/3 as infrastructure: the permitted set is *empty*
  modulo the throughput counters, since they are verdict-preserving refinements). A transition
  outside the manifest fails G-equiv.

So "verdict-equivalence" (G-equiv, a paired diff vs trunk) and "label-soundness" (G-sound, vs
oracle) are different comparisons against different baselines. Each stage states which
transitions its G-equiv manifest permits. (Caveat: the specific claim that `mismatch_count`
"ignores unknown" is worth a 2-minute source confirm before a builder relies on the exact
mechanism; the two-gate distinction stands regardless.)

### C.1 Fallback reversibility is bounded by the dependency chain (M3)

The per-stage "fallback" claims below are honest **only until a later stage consumes the
mechanism**. Codex (M3) is right that the off-switches are not permanent: Stage 2's Path-1 flag
is a real fallback until Stage 3's per-class data and Stage 4's theories depend on callbacks;
Stage 3's "infrastructure-only" mode is not a rollback once Stage 4 consumes it; Stage 4's
"don't register the theory" does not reverse the frozen-core constructor changes; Stage 1a's
β-hash fallback (OQ3) is itself unstaged. So each fallback carries an explicit **reversibility
horizon** — the last stage at which turning it off still isolates a regression — and past that
horizon the rollback boundary is a *commit revert of the dependent stage*, not a flag. E.4 risk
3's "designed-in off-ramp" language holds cleanly only through Stage 1b (the 1b→1a retreat);
beyond that, isolation is by staged revert, stated per stage.

### Stage 0 — the backtracking substrate (a fabric prerequisite; its own implementation lane)

**Goal.** Own scope/frame/pop-ordering logic ONCE, in a core `Trail` module behind a
`Backtrackable` signature, so the fabric-wide `cancel_until 0` invariant (F3) is enforceable
in one place instead of re-proven in four. This is a prerequisite of Stages 2–3, not a
nice-to-have (see "Why the fabric needs it," below).

**The problem it fixes (verified against source).** The frame *contract* is coherent — the
frozen `Theory.THEORY` `push`/`pop n` (`theory.mli:71-75`), `cdclt`'s one-frame-per-decision-
level discipline, and the `cancel_until 0` doctrine (the SAT exception-safety invariant) all
agree. But the frame *mechanism* is hand-rolled at least four times, each re-implementing
scope tracking, pop-ordering, and truncation bookkeeping:

| Site | Mechanism | Cite |
|---|---|---|
| EUF engine | typed `'p undo` sum (`U_parent`/`U_size`/`U_uses`/`U_fedge`/`U_reported`/`U_sig_*`) + `trail`/`levels` `Dynarray`s + `apply_undo`; e-node ids int-packed; the "watermark trap" for prop-mark restore | `euf.ml:94,126-136,204-262` |
| LIA simplex | typed `'a undo` sum (`Undo_lower`/`Undo_upper`) + `trail` `Dynarray` + `scopes` (trail length per open frame) + hand `push`/`pop` | `simplex.ml:50-58,248-249,531-539` |
| Combinator | `pin_frames : pin list list` — per-frame pinned shared-eq literals, list-of-lists push/pop | `combine.ml:90-92,362-370,628-648` |
| E-match manager (EXCLUDED — mechanism mismatch, see below) | transactional round rollback: `on_pop` filters the store by frame SELECTOR, `(frame, tag)`-keyed dedup dropped per frame, popped-seed restoration on abort | `manager.ml:114-146,173-188` |

This duplicated bookkeeping is **empirically where this wave's bugs lived** — the dedup/seed
rollback correctness, `w_reported` trailing (the EUF prop-mark watermark), and the
`l_watched` reasoning were all scope/pop-ordering defects in these hand-rolled trails.

**Stage 0 migrates THREE of the four; the e-match manager is EXCLUDED by mechanism-mismatch,
not deferred (Rev4.1, from Stage-0 impl reality).** Three sites — LIA simplex, the combinator
pins, and the EUF scope carve-out — share genuine LIFO decision-level scoping and migrate to the
substrate. The **e-match manager does NOT**: its `on_pop` is **frame-SELECTOR-keyed filtering**
(a SAT-assumption lifecycle ordering, not decision-level LIFO), and its **FIFO seed-restore**
does not map to newest-first rewind. Forcing it onto the LIFO substrate would be a mismatch, so
it **keeps its own audited transactional mechanism** — an explicit exclusion with rationale, not
a TODO. So the `cancel_until 0` invariant is consolidated for the three LIFO sites, and the
e-match lifecycle stays a separately-audited mechanism.

**RRR-F0 (specification).**
1. **A core `Trail` module + `Backtrackable` signature owning scope/frame logic once.** The
   signature carries `push : t -> unit`, `pop : t -> int -> unit`, and an undo-recording
   primitive; the module owns the scope stack (trail-length watermark per open frame) and the
   newest-first pop-ordering. The two entry representations Rev 3 posed as an open (a) typed-undo
   functor vs (b) closure trail are **resolved by the implementation (Rev4.1, from Stage-0 impl
   reality): a polymorphic `('e,'f)` `Trail` record — not a literal functor.** It meets the
   zero-hot-path-allocation obligation (settling the OQ6 worry: no per-entry closure allocation,
   EUF keeps its int-packed entries as the `'e` parameter), and it carries **the entry watermark
   AND all auxiliary watermarks in one frame, restored atomically** by a single `pop` — which
   *strengthens* the desync story (all watermarks rewind in lockstep, no cross-array skew). The
   old (a)/(b) framing is retained below only as the reasoning that led here:
   - **(a) Typed-undo functor** — zero per-entry allocation but a functor per site; the
     polymorphic-record form achieves the same allocation profile without the functor boilerplate.
   - **(b) Closure-entry trail** — a closure allocation per mutation, rejected for the hot path.
2. **Migration under equivalence gates.** Each of the **three migrating** sites (simplex,
   combinator pins, EUF scope carve-out; the e-match manager is excluded, above) migrates behind
   a **byte-identical verdict test** (its existing `push`/`pop` oracle is the acceptance): EUF's
   `test_propagate_pushpop_vs_full` / `test_explain_cache_pushpop` (`euf_adapter_test.ml`),
   simplex's bound push/pop tests, and the combinator's push/pop-reassert fixture (ADR-0010 §6).
   MISMATCH=0 on the full corpus per migrated module. No behavior change is permitted — a
   mechanism consolidation, gated as a refactor, not a feature. (The e-match manager's own
   transactional-round tests continue to guard its unmigrated mechanism.)
3. **The honest performance carve-out (per-module justification).** EUF's trail is
   perf-tuned: int-packed undo entries on the hottest path in the solver (`euf.ml:94-136`,
   and the A4-erratum cost-proportionality regime makes this path load-bearing). The
   substrate may share **only the scope bookkeeping** there — the frame stack, the pop
   watermark loop, the truncation-on-pop discipline — **not the entry representation**. EUF
   keeps its int-packed typed entries (realization (a)); the win at EUF is that the
   *scope/pop-ordering logic* (where the watermark-trap bug lived) is no longer private. The
   cold sites (combinator, e-match) may take the closure realization (b) for simplicity. Each
   module's choice is justified in its migration PR against a per-entry allocation
   measurement, not assumed.
4. **Cross-module undo ordering — the MECHANISM, not an assertion (R4/CRITICAL-1).** Rev 1
   left this as "a merge and its consequent mutations pop as a unit, newest-first" across four
   private trails, with no account of how four independently-`pop`-ed trails preserve one
   global order. The review is right that this is the load-bearing question. The answer is
   **not** "one global trail owns everything"; it is a stated invariant plus a co-location
   rule, and it is *why the four private trails are sound today* and stay sound under the
   fabric:

   - **Disjointness invariant (why independent trails compose).** Each module's trail owns
     **disjoint mutable state** (EUF: union-find/forest/congruence table; LIA: simplex
     tableau/bounds; combinator: pins; e-match: lemma store). Two mutations on different trails
     whose undo order could matter must touch overlapping state — and disjoint state means it
     never does. So independent per-module trails compose correctly **provided they share the
     same frame (decision-level) boundaries**, which they already do (`cdclt` forwards one
     `push`/`pop` to the combined theory, which fans out to each child). The substrate's job is
     to consolidate that shared *scope* bookkeeping (the watermark stack + pop loop, where the
     bugs lived), NOT to serialize disjoint mutations into one trail.
   - **Intra- vs cross-theory (Rev4-8, same-model note 2).** Disjointness is a statement
     *between* theories: theory X's trail never records a mutation of theory Y's state. It is NOT
     a claim about a theory's own internal aliasing — LIA's row unification aliases within LIA's
     tableau, and that is fine, because those mutations are all on LIA's own trail and reversed by
     LIA's own undo log (item 5). The cross-module property that matters is only: no undo entry
     of one theory touches another's state (the disjointness property test).
   - **Co-location rule for the ONE genuinely-coupled case (per-class data) — reversed by the
     CHILD's own frame `pop`, NOT a hub replay (item 5, Rev6-1 propagating Rev5-B4).** Per-class
     `theory_var` data's undo order is coupled to the merge that combined it. Rev 6 resolves the
     coupling the way item 5 does: the merge-combine records every mutation on the **triggered
     theory's own trail, in the merge's frame** (both children push/pop in lockstep via
     `Combine.push`/`pop`, so a merge-driven mutation lands in the current decision-level frame),
     and an ordinary `pop` reverses it via the **child's own frame pop** — the SAME mechanism that
     reverses the child's ordinary assertions. The hub records NO pop-watermark and does NOT replay
     a child undo log on `pop`; the child's frame pop is the single owner (the double-rewind
     authority that an earlier draft described here is removed — see item 5, invariants DR/SW). The
     one hub-driven rewind, `rollback_to watermark`, is intra-check only (fuel/exception aborts,
     truncating), never on the pop path. **No child-local temporary frame during callback drain:** a
     callback-driven mutation MUST land in the merge's own frame, so it cannot land in a different
     frame than the merge that caused it (which would desynchronize the single-owner pop).
   - **Injected merges (Stage 1b) go through `Euf.assert_eq` → the hub trail** (EUF-internal),
     so they raise no cross-trail question at all.
   - **Callback-driven theory mutations (Stage 2)** touch only the reacting theory's own
     disjoint state, on that theory's trail, in the merge's frame (sound by disjointness); an
     ordinary `pop` reverses them via that theory's own frame pop (item 5, single owner).
   - **Fallback if disjointness ever fails** (a future theory whose callback mutates another
     theory's state): the named protocol is a **global epoch counter** stamped on every
     trailed mutation, with `pop` draining all trails in descending-epoch order (a k-way merge
     over the ~4 trails). This mechanizes a single global order unconditionally, at the cost of
     one epoch int per entry. It is the fallback, not the default, because the disjointness
     invariant makes it unnecessary for the theories in scope (EUF/LIA/datatypes/arrays all own
     disjoint state; only per-class data couples, and co-location handles it).

5. **Merge-undo = a THEORY-LOCAL UNDO LOG on the CHILD's own trail, reversed by the child's own
   frame `pop` — with a SEPARATE intra-check `rollback_to watermark` for aborts (R4/MED-2 + C6,
   sharpened Rev4-4, ordering pinned Rev5-B4).** A destructive combine touches more than the two
   payloads: LIA's destructive row unification **pivots the tableau**, mutating state *beyond* the
   two payloads (other rows, basic/non-basic assignments), so snapshotting two payloads would not
   restore those. The contract: **the merge-combine records every mutation it makes on the
   *triggered theory's own trail*** (a theory-local undo log). This gives arbitrary mutable/aliased
   reversibility. (Persistent/immutable payloads are the special case where a payload snapshot
   alone suffices; the general contract is the undo log.)

   **Rev4-4 said "on `pop` the hub replays the theory's undo log to a captured watermark" — that
   is a SECOND rewind authority racing the child's own `pop`, and codex's Blocker 4 (double-rewind
   / stale-watermark) is right to reject it.** Rev 5 pins **two temporally-disjoint rewind
   mechanisms, single-owner each:**
   - **Decision-level `pop` (between checks).** A merge-combine's mutations ride the **child's own
     trail in the current frame** (the children push/pop in lockstep via `Combine.push`/`pop`,
     `combine.ml:628-649`, so a callback-driven mutation during `Combine.check` lands in the child's
     current decision-level frame). An ordinary `pop n` reverses them **via the child's own frame
     pop** — the SAME mechanism that reverses the child's ordinary assertions. The hub does **NOT**
     replay on pop; the child's frame pop is the single owner. This removes the cross-trail
     replay-on-pop entirely (and with it the hub-rewind-vs-child-pop ordering question).
   - **Intra-check `rollback_to watermark` (inside a `check`).** Fuel exhaustion / a mid-cascade
     exception (§B.5(b), F6) must reverse a *partially-applied* cascade **before `check` returns**.
     This uses `Trail.watermark`/`Trail.rollback_to` (a natural addition to the `Backtrackable`
     signature): capture a watermark at check-entry, and on abort reverse **and truncate** every
     entry back to it — on every affected trail (the hub's and each triggered child's), driven by
     the hub because it owns the cascade.
   - **Invariant DR (no double-rewind).** `rollback_to` **truncates** (removes the entries it
     reverses), so a subsequent decision `pop` sees only what actually survived the abort and never
     re-reverses a rolled-back entry. Decision `pop` and `rollback_to` are the only two reversal
     paths, and truncation makes them disjoint on every entry.
   - **Invariant SW (no stale watermark).** A watermark is **check-local**: captured and consumed
     inside a single `Combine.check` invocation, never stored on a trailed structure or reused
     across a `check`/`push`/`pop` boundary. It is a stack local of the cascade driver, so it can
     never point past a trail position a later `pop` invalidated.
   - **Temporal disjointness.** A `rollback_to` happens strictly inside `check` (before it returns);
     a decision `pop` happens strictly between checks (after `check` returned). They never
     interleave, so "when does the hub rewind run relative to the child's own pop processing" has a
     definite answer: never at the same time, and on the pop path the hub does not rewind at all.

   Mutants: (a) **stale-data mutant** — truncate the theory undo log so a pivoted row is NOT
   restored on a decision `pop`, require a **verdict difference** (spurious post-pop conflict →
   wrong `unsat`); the test **inspects BOTH restored classes AND tableau feasibility**. (b)
   **double-rewind mutant** — make `rollback_to` reverse-without-truncate so a following `pop`
   re-reverses the same entries (trail underflow / restore-past-the-frame), require a verdict
   difference. (c) **stale-watermark mutant** — persist a watermark across a `pop` and reuse it,
   require a verdict difference. Cost: the undo log is one entry per boundary-class merge that
   carries theory data (bounded by boundary-class merges, §B.5a, not by term count); watermarks are
   O(1) per check.

6. **Re-entrant cascades need a queued, non-reentrant closure (C5/H1).** A nested
   hub→LIA→hub→… cascade (a `new_eq` makes LIA inject an equality, which merges classes, which
   notifies again) has no well-defined global reverse order if callbacks *recurse*. The
   mechanism: **callbacks ENQUEUE work; the hub drains one work queue to a fixpoint** in a single
   deterministic order (registration order of theories, then e-node id / tag). No callback runs
   inside another callback's stack frame, so the do-order is a single flat sequence and its
   reverse is unambiguous. Combined with the disjointness invariant (item 4) and co-located
   merge-undo (item 5), this gives one total undo order per closure without a global trail.
   **Novelty keys** (F5/H1) make the queue finite: an already-known eq/diseq/event is a no-op, so
   a diseq echo cannot re-enqueue forever. Test: nested cross-injections + a **nonchronological
   backjump** + `cancel_until 0`, asserting full state restoration (the C5 re-entrancy test).

**Review grade (C5).** The pure scope-consolidation (items 1–3) is a **refactor** gated
byte-identical. But the **cross-module ordering machinery (items 4–6) — disjointness
enforcement, co-located reversible merge-undo, the queued non-reentrant closure — is TCB-grade**
and gets full dual review (correcting Rev 1's §E.2 "refactor-grade" for the whole of Stage 0):
it is where a backtracking-desync wrong-`unsat` would breed.

**Unfreezes:** nothing frozen. `Trail`/`Backtrackable` are new `core` modules (stdlib-only,
I3); the four sites are engine/combinator-internal (not frozen). `theory.mli` push/pop
contract is unchanged (the substrate implements it, does not redefine it). **Gates:**
MISMATCH=0 per migrated module; each site's existing push/pop oracle green byte-for-byte; the
**euf hot-path carve-out under an equivalence gate on the euf-perf goldens** (EUF keeps its
int-packed entry representation; only scope bookkeeping is shared — any perf regression on the
euf-perf goldens fails the gate); determinism golden (I6). **Enforcement (oracle shapes):**
(i) a **disjointness property test** — no undo entry recorded by theory X mutates theory Y's
state (asserted structurally: each trail's `apply_undo` touches only its module's fields);
(ii) the **stale-data mutant restores only ONE payload** and must produce a verdict difference,
with the test inspecting BOTH restored classes (item 5); (iii) the **re-entrancy test** —
nested cross-injections + nonchronological backjump + `cancel_until 0`, full state restoration
(item 6); (iv) the byte-identical scope-consolidation gate per migrated site. **Can break:** a
subtle pop-ordering or truncation regression during migration (caught by the byte-identical
gate); a re-entrant cascade with a stranded callback fact surviving its merge (caught by (iii)). **Fallback:** migrate sites one at a time; a
site that resists consolidation (EUF, if scope-sharing itself regresses the hot path — OQ6)
stays on its private trail; the substrate requires only that (a) all migrated sites share the
scope machinery and (b) per-class coupled data is co-located on its merge's trail (item 5).

**Sequencing.** Stage 0 lands before Stage 2 (the first cross-module trailed state). Stage
1a/1b do not strictly require it (Stage 1b's injected edge is EUF-hub-local, hostable on
EUF's existing trail), so Stage 0 can proceed **in parallel** with Stage 1 on its own lane and
must be complete before Stage 2 opens. It is a prerequisite of the fabric's cross-module
stages, not of the first corpus win.

### Stage 1 — LIA fixed-value equality propagation (the mathsat/Wisa gap; #156)

**Goal.** Close the mathsat/Wisa family natively — the largest untouched sub-family
(`z3-combination-study.md:315-329`) — by realizing z3 mechanism 1: when LIA fixes a shared
Int term to a value another shared term holds, connect them.

**Two sub-steps, shipped in order (velocity + headline protection):**

- **Stage 1a — `v1b` (fix-triggered trichotomy Split; interim, ZERO new soundness
  surface).** Trigger on the *fix event* (LIA reports a shared Int term newly fixed to a
  value another holds — surfaced via `lia.rational_value` / an incremental fix event) but
  discharge with the **existing** ℤ-trichotomy Split (`s = w ∨ s < w ∨ s > w`), never
  asserting `s = w`. LIA refutes the `<`/`>` disjuncts with its own Farkas explanations;
  `s = w` reaches the trail as an ordinary SAT literal with the standard explanation path
  (`z3-combination-study.md:251-261`). This is z3-mechanism-1's *trigger* with #142's
  *audited machinery*. It structurally cannot reproduce HIGH-3/HIGH-4 (a fixed term is in at
  most one live bridge per value). **Unfreezes:** nothing. **Gates:** MISMATCH=0, R1, the
  mathsat/Wisa before/after sweep, H1/L1 fixed (F6). **Can break:** nothing on the frozen
  surface; a bad fix-event feed is a completeness miss, not unsoundness. **Fallback:** if the
  incremental fix-event surface from LIA is more plumbing than expected, fall back to the
  β-hash trigger (`v2`, `z3-combination-study.md:263-291`), which needs only β read-out
  (already proven by the phase-hints lane). **This is the immediate corpus win and it is
  landable before any fabric machinery exists.** **Dependency (LOW-3):** Stage 1a is NOT
  free-standing — it reuses the ℤ-trichotomy split machinery and the H1/L1 construction-site
  fixes whose fix round is #142/#154 (parked at `parked/uflia-bridge-static`, superseded by
  this stage). Stage 1a is gated on that machinery being available (either landed or carried
  into this lane), not independent of it.

- **Stage 1b — `v1a` (the first true fabric primitive: justified merge into the hub).**
  Replace the trichotomy Split with a **direct `assert_eq ~premise:Γ s w`** into the hub
  (A.2), where `Γ` is the LIA fixing bounds. This is the first exercise of F1/F2/F4/F7 on
  the simplest possible theory pair, and it is the point of the whole ADR — it removes the
  propositional round-trip. **Unfreezes:** nothing frozen — the `FABRIC_CHILD` interface (§B.2,
  the `CONGRUENCE_CHILD` widening generalized to both children), the `edge_id → Γ` registry, and
  the combined-reason cache all live in the (non-frozen) combinator/adapter layer; the engine
  tokens widen to `justification` with NO `euf.mli`/`lia.mli` change; `Rule_tag.Shared_eq` is the
  additive enum unfreeze already permitted. **Gates:** F1–F7 acceptance (§B), MISMATCH=0, R1, cert
  emit test, TCB-grade dual review. **Can break:** a wrong `Γ` or an ask-time re-derivation
  is a #102-class soundness bug — this is why Stage 1b is TCB-reviewed and Stage 1a ships
  first as the headline-protected baseline. **Fallback:** Stage 1a remains the shipped
  mechanism; 1b can be reverted to 1a with no corpus regression (both close the same gap;
  1b only removes the round-trip's search overhead).

**Why split 1a/1b.** 1a captures the corpus points now with zero soundness risk; its measured
win justifies building the F1–F7 machinery for 1b. If 1b's dual review finds the justification
discipline too costly to get right, we keep 1a and the corpus win is banked — the fabric
investment is then re-evaluated against Stage 4's needs alone. This is the headline-protection
the directive asks for.

**Honest framing of 1b–3 (R5, A7-1a).** Only Stage 1a clears the product bar on its own (a
solve-rate delta on the mathsat/Wisa family). By this ADR's own measured expectations, 1b's
gain over 1a is "removes the round-trip's search overhead" (a throughput refinement on the
*same solved set*), Stage 2 is "solved-rate-neutral, latency-positive," and Stage 3 is "modest,
its value is as Stage 4's enabler." Under DESIGN A7-1a (*"small effects are not worth landing as
perf work … spend it where the return is measured in points, not tenths"*), **1b/2/3 do NOT
clear the product bar as perf work** — they are **Stage-4-enabling infrastructure**, and their
acceptance is **soundness + equivalence + determinism**, NOT a solve-rate delta. So the real
decision this ADR asks the master to make is **binary**: *do we want datatypes/arrays on a
fabric (the 1b→4 arc) or not?* — with Stage 1a severable in front of it as a self-justifying
corpus win regardless of the answer. The Rev-1 phrasing "1b … is the point of the whole ADR" was
a purpose statement standing in for a corpus justification 1b cannot give alone; the corpus
justification for the whole 1b→4 arc rests on **Stage 4**.

**Corpus gain (to measure, not assume):** Wisa is the target; expect a partial not total
sweep (some Wisa files are Boolean-search-bound, not combination-bound —
`z3-combination-study.md:324-329`). Stage 1 also must not regress #117 (HIGH-3's victim) —
the fix-trigger never bridges `(x+2, x)` because they are never fixed to the same value.

### Stage 2 — merge-notification callbacks (EUF→LIA; A.3)

**Goal.** Replace Path-1 forwarding (§0.1) with direct `new_eq`/`new_diseq` notification:
when the hub merges two classes carrying Int terms shared with LIA, notify LIA so it asserts
the bound-equality directly, instead of the equality round-tripping through the SAT trail.

**Unfreezes:** the `FABRIC_CHILD`/`FABRIC_CONGRUENCE_CHILD` signatures (§B.2, the combinator-internal
widening of `CONGRUENCE_CHILD` `combine.mli:175-183`, NOT frozen) gain callback registration; the
engine-level `euf.mli`/`lia.mli` (NOT frozen) gain the callback hook and LIA's notification handler.
`theory.mli` stays frozen. **Gates (R6 — soundness-equivalence, NOT verdict-equivalence).** MISMATCH=0 is the
**soundness-equivalence** gate: it guarantees *no wrong verdict*, it does **not** guarantee the
*same* verdict as Path-1 forwarding. This is the #142 MED-2 standard: removing the SAT
round-trip changes which literals hit the trail, which can change model reconstruction and
which *sound* verdict (sat vs unknown) the incomplete layer reaches (the HIGH-3 class,
`uflia-search-review.md:91-100`). So Stage 2 additionally gates on: R1; cert (a callback-driven
merge emits the same `Shared_eq`/congruence cert as the forwarded equality did); the
determinism golden (F5); and a **model-reconstruction check** — because the callback keeps the
entailed equality hub-internal (no longer SAT-visible), verify the UF-models extractor (#110)
still sees the class merge for model output (it reads the hub, so it should — but this must be
*verified*, not assumed). **Can break:** a callback that mutates theory state not on the trail
(F3 violation) → backtracking bug; a callback cascade that does not terminate (F5); a
model-reconstruction that silently loses a hub-internal merge → wrong `sat` model (caught by
the extractor check + R1). **Fallback:** Path-1 forwarding is retained behind a flag; if the
callback path shows a regression, forward as today. The fallback is **not "free"**: Path 1 and
the callback are only *soundness*-equivalent, so a switch can shift which files land as `sat`
vs `unknown` — the fallback is a safe *soundness* retreat, measured against the corpus, not a
transparent no-op.

**Corpus gain:** primarily a *throughput* win (removes SAT round-trips for entailed
equalities — the EUF-transitivity-throughput family, #143, is a candidate beneficiary), not
a new solved-rate family. Measure against the firehose.

### Stage 3 — per-class theory data (`theory_var`; A.4)

**Goal.** Let theories attach per-class data the hub combines on merge — the substrate Stage
4 needs. For LIA this is the bound row; attaching it to the class means a merge that unifies
two rows detects an immediate bound conflict without a `Final` round.

**Unfreezes:** `euf.mli` (NOT frozen) gains the attach/merge-data API; the child signatures
gain the merge-combine callback. `theory.mli` stays frozen. **Gates:** MISMATCH=0, R1, cert,
determinism; a data-merge mutant (drop the merge-combine on one class) must be KILLED with a
verdict effect. **Can break:** stale attached data after a `pop` (F3) — the data must be
trailed with the class; a non-deterministic merge-combine order (F5). **Fallback:** if the
attached-row optimization does not pay off for LIA, Stage 3 can ship as *infrastructure only*
(the attach API exists, LIA uses it minimally) to unblock Stage 4 without a LIA behavior
change. Stage 3's value is mostly as Stage 4's enabler.

**Corpus gain:** modest for QF_UFLIA (a throughput refinement); its real payoff is Stage 4.

### Stage 4 — datatypes and arrays native on the fabric (split 4a/4b per R8)

Rev 1 sold "datatypes **and** arrays land natively on the fabric" as one step; for arrays that
was a gesture (§A.3 literally trailed off at "arrays: propagate read-over-write)"). R8 splits
Stage 4 by what the Stages 1–3 primitives actually deliver.

**Stage 4a — datatypes (lands on the primitives).** Injectivity/distinctness on merge is a
`new_eq` callback (Stage 2) reading a per-class **constructor tag** (Stage 3 `theory_var`): when
two classes with different constructors merge → conflict (distinctness); when two `C(a)`,`C(b)`
merge → propagate `a = b` (injectivity, an inject-eq back into the hub, A.2). Selector
propagation (`sel_i(C(x_1..x_n)) = x_i`) is a `new_eq`-driven inject-eq. **These are exactly
the Stages 1b–3 primitives** — datatypes genuinely land on the fabric. Term creation is bounded
(selectors of existing constructor applications), so the F5(b) fuel bound applies but is rarely
hit.

**Stage 4b — arrays (needs a primitive Stages 1–3 do NOT deliver).** Read-over-write
`select(store(a,i,v), j) = ite(i=j, v, select(a,j))` requires (a) **creating a new term**
`select(a,j)` mid-cascade and (b) a **case split on `i=j`**. Neither is an inject-eq, a
callback, or per-class data: (a) is *term-creation-during-cascade* (which is exactly the F5
termination hazard, RRR-F5(b), so 4b is gated on the **Stage-4 cascade-fuel bound** with
hard-stop degrade-to-`unknown`), and (b) is a `Theory.Split` back through the seam (the existing
Split channel — arrays reuse the trichotomy/Split machinery for the `i=j` decision). So 4b is
**not** delivered by Stages 1–3; it needs the fuel primitive (F5(b)) and reuses the seam Split.
The ADR states this honestly rather than gesturing: **arrays are a Stage-4b lane with its own
primitive (fueled term-creating cascade + `i=j` Split), sequenced after datatypes (4a).**

**Unfreezes:** THE REAL A5-SCALE FROZEN-CORE EVENT (both 4a and 4b). New sorts and term
constructors (datatype constructors/selectors/testers; array `select`/`store`) require
`term.mli` + `context.mli` changes — the frozen core (ADR-0003), pairing with parked #147. This
is the one stage touching a frozen surface beyond the additive `Rule_tag`, and it needs the full
unfreeze ritual (fresh-agent adversarial pass with an attack brief, §10). New child adapters
implement the frozen `theory.mli` `THEORY` — the *seam* is unchanged; what unfreezes is the
*term representation*. **Certificate prerequisite (M4, formal — not a deferred OQ).** Stage 4's
acceptance lists a cert gate (datatype/array proof leaves), but that grammar is a *separate
ADR-0013 delta not specified here*, so the gate cannot be constructed until that delta lands.
Rev 2 makes it an explicit **prerequisite**: Stage 4 is blocked on (a) the §B.7 fabric-event
stream (already specified here) AND (b) a datatype/array leaf grammar + checker + emission +
honeypots, scoped as its own ADR-0013 delta (a Stage-4 sub-task, gated like the cert-step work
#153). Stage 4 is **neither independently landable nor fully gated** until (b) exists — stated
plainly rather than left as an under-specified gate line. **Gates:** pre-labeled
datatypes/arrays SMT-LIB suites (fast both-direction regression, = G-sound), the G-equiv
manifest (new-logic files only, no QF_UFLIA transitions), R1, the (b) cert delta above;
4b additionally gates on the fuel-exhaustion fixture (F5(b)).
**Can break:** a new theory's merge rule is a new soundness surface (TCB-grade dual review
each); an ill-typed new constructor is an I1/I8 hazard (smart-constructor discipline); 4b's
term-creating cascade is the F5 hazard (fuel-bounded). **Fallback:** datatypes/arrays are *new
logics*, not a change to QF_UFLIA — an unready stage-4 theory simply is not registered;
QF_UFLIA is unaffected. 4b can be deferred independently of 4a.

**Stage-4 completeness-rule inventory + soundness scoping (Rev4-5 / NEW-HIGH-2).** The core
merge rules above are NOT the whole of each theory; the completeness rules are enumerated and
scoped so a builder does not silently ship a wrong-`sat`:
- **Datatypes:** no-confusion (= injectivity + distinctness, **in-scope**, the 4a core);
  **exhaustiveness** (every datatype value is *some* constructor — needed for finite-domain
  case splits) and **acyclicity** (no term equals a proper subterm of itself, e.g.
  `x = cons(h, x)` is UNSAT) — **out-of-scope for the first cut.**
- **Arrays:** read-over-write (**in-scope**, the 4b core); **extensionality**
  (`a = b` iff `∀i. select(a,i) = select(b,i)`, needing a **difference/diff term** witness for
  the disequality direction) — **out-of-scope for the first cut.**

**Soundness argument for the out-of-scope rules — a MODEL-VALUE WELL-FORMEDNESS VALIDATOR, NOT
R1 (Rev5-B5b, codex CONFIRM Blocker 5b).** Each omitted rule is a *completeness* rule: without it
the solver may fail to derive a `UNSAT` and instead produce a candidate `sat` model that violates
the rule (a cyclic datatype value; two arrays equal in-model but differing at some index). Rev 4
argued "the **R1 model-check gate** is the backstop — a model violating the rule fails R1 →
`unknown`." **That is unsound as written.** R1 (`model_check.mli:1-12`) *evaluates every asserted
term in the candidate model and fail-closes*; it does **not** validate that a datatype/array VALUE
is well-formed. An opaque or cyclic datatype value, or a non-extensional array representation, can
make every asserted equality **evaluate consistently** and pass R1 — R1 never asks "is this value a
finite constructor term / a genuine finite function?" So the safety net has a hole exactly where
the omitted rules live: an R1-ACCEPTED wrong `sat`.

Rev 5's honest scoping (option (c) enforced by (b)): **Stage-4 `sat` promotion is RESTRICTED to
constructor-form / finite-function models, enforced by a model-value well-formedness validator that
runs as a Stage-4 gate — a SEPARATE obligation from R1's formula evaluation.** The validator:
- **datatypes:** every datatype-sorted term's model value is a **finite constructor application**
  (exhaustiveness = it is *some* constructor; acyclicity = an occurs-check finds no term equal to a
  proper subterm of itself). Non-constructor-form / cyclic → not promotable → `unknown`.
- **arrays:** every array-sorted term's value is a **finite function** (a finite store map + a
  default), and any asserted array **disequality** carries an explicit witness index at which the
  two differ (the extensionality diff obligation, discharged by witness rather than by the omitted
  search rule). No well-formed representation / no diff witness → `unknown`.
A model that fails the validator degrades to `unknown`, **never** a wrong `sat`. The validator is
O(model size) (occurs-check + finite-function + witness checks) and **lands WITH Stage 4** (a
prerequisite, folded into the M4 cert-delta prerequisite), not deferred — Stage 4 is neither
independently landable nor soundly gated until it exists. The omitted *search* rules
(full extensionality, DT exhaustiveness case-splitting) stay named follow-ups (each its own row),
landed when the corpus shows demand — the same evidence-gated discipline as ADR-0010's
`bool-compound-uf-args`. This must be stated in the Stage-4 spec so a builder does not read
"datatypes/arrays native on the fabric" as "datatype/array-complete," and does not lean on R1 for
value well-formedness.

**Stage-4 forward-obligations (Rev6-FWD; named as gate items, NOT designed here — the #153/cert-ADR
precedent for carrying a named dependency that gates a later stage).** Rev 5's B1/B5b machinery
opened two Stage-4-shaped residuals that are genuinely Stage-4 design, recorded here as numbered
Stage-4 GATE obligations so the ADR carries them honestly rather than silently:
- **FWD-1 — N-child fabric composition.** `Combine (R) (A) (B)` is BINARY and its result is a
  `Theory.THEORY` + `congruence_state` (`combine.mli:190-202`); the Rev5-B1 `FABRIC_CHILD` widening
  makes the *current* 2-child EUF+LIA path typeable, but a Stage-4 datatype/array child cannot join
  the shared hub as a third `FABRIC_CHILD` through the retained binary-nesting story (a nested
  `Combine` would have to itself be a `FABRIC_CHILD`, which the binary result signature does not
  provide). Stage 4 must specify **either a fabric-capable nested result (the inner `Combine`
  exposes `check_fabric`/`explain_fabric`) or a flat N-child hub registry** — a Stage-4 packaging
  unfreeze/redesign. GATE: Stage 4 does not open until this is designed and dual-reviewed. (No
  soundness impact on Stages 1–3, which are strictly 2-child.)
- **FWD-2 — value-validator SEMANTIC completeness (beyond shape).** Rev5-B5b's model-value validator
  as specified checks *shape* (constructor-form, finite-function). A `sat` can still be wrong if the
  function tables interpret constructors/selectors/testers or `select`/`store` non-canonically while
  every value stays shape-valid. Stage 4 must extend the validator to **canonical/structural
  datatype equality (correct constructor/selector/tester interpretation) and extensional array
  equality with canonical `select`/`store` evaluation** — a Stage-4 obligation on the validator spec,
  co-designed with the DT/array leaf grammar (M4). GATE: the Stage-4 `sat` gate is the
  *semantically-complete* validator, not the shape check alone.

**Corpus gain:** entirely new logics (QF_DT via 4a; QF_AX / QF_AUFLIA via 4b) — the solved-rate
lever for corpora we cannot touch today, at the completeness ceiling above (a `sat` is promoted
only when the semantically-complete model-value validator, not R1, certifies the model is
well-formed; FWD-2).

### Stage dependency and headline protection

Stages 1a → 1b → 2 → 3 → 4 are ordered by increasing blast radius and decreasing headline
protection. Stage 0 (the backtracking substrate) runs on its own lane, in parallel with
Stage 1, and gates Stage 2. Stage 1a is pure corpus win, zero risk. Stages 1b–3 are
combinator-internal (frozen `theory.mli` untouched), each MISMATCH=0-gated against the trunk
it replaces, each with a free fallback to the prior mechanism. Stage 4 is the frozen-core
event and is gated independently on the new-logic corpora. No stage advances the trunk tip
until its gate is green (the freeze-tip-during-review discipline).

Dependency edges: Stage 1b needs Stage 1a's trigger (reuses it, swaps the discharge); Stage 2
needs Stage 0 (cross-module trailed state, F3); Stage 3 needs Stage 2 (per-class data is read
by the callbacks); Stage 4 needs Stage 3 (constructor tags are per-class data) and the
`term.mli`/`context.mli` unfreeze. Stage 0 and Stage 1 have no edge between them.

---

## D. WHAT Z3 DOES THAT WE DELIBERATELY DON'T (scope honesty)

- **Relevancy propagation.** z3 has a relevancy engine that suppresses reasoning about atoms
  not relevant to the current partial model. **Non-goal.** z3 itself turns relevancy OFF for
  QF_UFLIA (`m_relevancy_lvl = 0`, `smt_params.cpp` `setup_QF_UFLIA`,
  `z3-combination-study.md:52-56`), so it is not the lever for our target families, and a
  per-atom relevance filter is the exact banned pattern ADR-0010 / A4-erratum warn against
  (per-occurrence relevance guessing had counterexamples in every guise). Not built.
- **Dynamic ackermannization (`dyn_ack`).** z3's conflict-driven UF congruence-lemma cache.
  **Non-goal for QF_UFLIA** — it is gated by `m_dack` and not what closes Wisa
  (`z3-combination-study.md:60-62`); the eager-fixed-eq mechanism is. Ackermannization
  proper is wired only for BV logics in z3, and we do not add a bounded-Ackermannization
  presolve believing we match z3 (we don't — `z3-combination-study.md:30-32`). Revisit only
  if a measured family demands it.
- **Quantifier instantiation via the fabric.** z3 instantiates quantifiers by E-matching
  against the e-graph. **Deliberately separate.** Our lemma tier is ADR-0012 (E-matching over
  a read-only e-graph query view, `smt/ematch/`), already designed and partly built; it reads
  the hub non-registeringly (`egraph_view`), it does not become a fabric theory. Keeping the
  lemma tier off the fabric preserves ADR-0012's failure-direction analysis (the matcher
  cannot perturb the e-graph, R6). Not merged into this ADR.
- **β-hash candidate generation (NON-GOAL, cut per H2).** z3's mechanism 2 chooses
  model-coincident pairs by hashing shared terms on their β value. We **retain model-based
  combination** — but via the existing **bidirectional `find_disagreement`** Final split
  (unchanged), which already scans the interface set in *both* disagreement directions. β-hash
  is only a candidate-*generation* optimization over that scan, and Rev 1's proposal to make it
  "mechanism 4" was orphaned (unstaged) and one-directional (it finds only "LIA-equal,
  hub-distinct," dropping the "hub-equal, LIA-different" direction `find_disagreement` also
  needs → wrong-`sat`). So β-hash is cut to a non-goal: revisit only if the interface-set scan
  is a *measured* bottleneck, and only with the bidirectional obligation restored. The entailment
  path (mechanism 1, Stage 1) remains the primary lever; the Final `find_disagreement` split is
  the retained model-based second tier.
- **A general "e-graph owns all terms" internalization à la z3's `internalize`.** We retain
  ADR-0010's boundary-node interface set (only boundary + both-used terms are shared), not
  z3's total internalization of every subterm, because A4-erratum's cost-proportionality rule
  (EUF cost proportional to uninterpreted structure, not term count) is a firm invariant here
  and z3's total internalization would violate it. The fabric hub holds exactly the terms
  A4-erratum admits — and §B.5a *shows* it (callbacks fire only on boundary-node classes,
  `theory_var` attaches only to boundary classes, under-inclusion mutant carried), rather than
  merely asserting it as Rev 1 did.

---

## E. COST / RISK

### E.1 Frozen-surface blast radius, per stage

| Stage | `theory.mli` / `theory_view.mli` (seam) | `explanation.mli` | `sat.mli` | `term.mli` / `context.mli` (core) | Non-frozen changed |
|---|---|---|---|---|---|
| 0 (substrate) | none (push/pop contract implemented, not redefined) | none | none | none | new `core` `Trail`/`Backtrackable` (+ `watermark`/`rollback_to`); migrated: `simplex.ml`, `combine.ml` pins, `euf.ml` scope carve-out; `manager.ml` (ematch) EXCLUDED by mechanism mismatch (Rev 4.1) |
| 1a (`v1b`) | none | none | none | none | LIA fix-event surface (`lia.ml`), combinator trigger |
| 1b (`v1a`) | none | `Rule_tag.Shared_eq` (additive, already permitted) | none (no `sat.mli` change — fabric-event stream is combinator-emitted, §B.7 C4) | none | non-frozen `FABRIC_CHILD` interface + `edge_id → Γ` registry + combined-reason cache (`combine.ml`); `justification` token in `euf`/`lia` adapters (no `euf.mli`/`lia.mli` change); `Lia.fixed_bounds` accessor; new recorder fabric-event kind + combinator cert trace hook |
| 2 (callbacks) | none | none | none | none | `FABRIC_CHILD`/`FABRIC_CONGRUENCE_CHILD` sig gains callback registration; `euf.mli`/`lia.mli` engine APIs (non-frozen) |
| 3 (`theory_var`) | none | none | none | none | `euf.mli` attach API, child merge-combine |
| 4 (DT/arrays) | none (new children implement it) | new theory leaves (grammar extension) | new leaves | **YES — new constructors (A5 unfreeze ritual, pairs #147)** | new child adapters; model-value well-formedness validator (Rev5-B5b `sat`-gate) |

**Headline finding:** the frozen CDCL(T)↔`Combine` seam is untouched through Stage 3. The
directive's "A5-scale unfreeze" warning applies only to Stage 4's *term representation*
(datatype/array constructors), not to the theory-interface seam — the fabric is a
re-architecture of `Combine`'s internals, and the engine still drives one `THEORY`.

### E.2 Review burden

- **Stage 0 — SPLIT grade (Rev4-6, resolving the §E.2-vs-§C contradiction; TCB wins for the
  cross-module machinery):** the **scope-consolidation (items 1–3)** is **refactor-grade** — no
  behavior change permitted, the byte-identical MISMATCH=0 gate per migrated module IS the
  review, with the per-module entry-representation choice (functor vs closure) reviewed on
  allocation merits (EUF hot-path carve-out). The **cross-module ordering machinery (items 4–6)**
  — disjointness enforcement, the theory-local-undo-log reversible merge, the queued
  non-reentrant closure — is **TCB-grade dual review** (it is where a backtracking-desync
  wrong-`unsat` breeds). Rev 3's §C already said TCB for items 4–6; this line makes §E.2 agree
  (Rev 1's flat "refactor-grade Stage 0" is corrected).
- **Stage 1a:** ordinary dual review (same-model + codex); zero new soundness surface, so
  the review is about the fix-event trigger's correctness and completeness, plus H1/L1.
- **Stage 1b:** TCB-grade dual review — F1–F7 are the review brief; the attack surface is the
  justification discipline (a wrong `Γ` or ask-time re-derivation is a silent-unsoundness
  bug). This is the single most expensive review in the plan.
- **Stages 2–3:** TCB-grade dual review each, focused on F3 (backtracking of callback/data
  state) and F5 (cascade termination/determinism); MISMATCH=0 against the prior mechanism
  makes each independently verifiable.
- **Stage 4:** each new theory gets TCB-grade dual review of its merge rules + a new-logic
  corpus acceptance; the `term.mli` unfreeze gets the fresh-agent adversarial pass (§10).

Every stage carries the honeypot/mutant discipline: the fabric's mutants are the F1–F7
enforcement tests (precedence, substitution-drop, strand-the-edge, empty-`Γ`, data-merge-drop,
H1-overflow), each required to be KILLED with a *verdict* effect, not merely observed
(the #102/ADR-0010 discrimination standard).

### E.3 Corpus/product gains, per stage (to MEASURE, per A8)

Per R5 (A7-1a): only Stage 1a clears the product bar on its own; **1b/2/3 are Stage-4-enabling
infrastructure** whose acceptance is soundness + equivalence + determinism, not a solve-rate
delta. The decision is binary — the 1b→4 arc's corpus justification rests on Stage 4.

- **Stage 1a (self-justifying):** mathsat/Wisa — the largest untouched sub-family; expect a
  partial sweep (some Wisa files are Boolean-search-bound). **This is the banked corpus win.**
- **Stage 1b (infrastructure):** removes the round-trip's search overhead (throughput on the
  *same* solved set as 1a). Does NOT clear A7-1a as perf work; justified as the first fabric
  primitive proving the F1–F7 contract for Stages 2–4.
- **Stage 2 (infrastructure):** throughput (removes SAT round-trips for entailed equalities);
  #143 is a candidate beneficiary. Solved-rate-neutral, latency-positive. A7-1a: infrastructure.
- **Stage 3 (infrastructure):** modest QF_UFLIA throughput; its value is as Stage 4's enabler.
- **Stage 4 (the solve-rate justification for the whole arc):** new logics — QF_DT (4a),
  QF_AX/QF_AUFLIA (4b); the datatypes/arrays corpora under `corpora/` are the acceptance and
  the solved-rate lever for problem classes we cannot touch today.

### E.4 The dominant risks

1. **Silent unsoundness via a bad `Γ` or an ask-time re-derivation of an injected merge's
   CONSEQUENCE (F1/F4).** The center of gravity, as always (DESIGN §12.1) — and the specific
   surface is the *consequence* path (R1), not the injected edge itself. Mitigated by: Stage 1a
   shipping first (headline-protected baseline), the #102-proven snapshot-at-propagation
   discipline (`euf_adapter.cache_reason`) reused verbatim for consequences, the injection-time
   forward-ref assertion (MED-5), `theory_explain_checked` + self-check + the consequence-driving
   precedence mutant + the substitution-drop mutant, and the whole-VC Lean gate (MISMATCH=0) as
   the uncorrelated backstop (checkable `Shared_eq` replay pending #153).
2. **Backtracking desynchronization (F3).** An injected edge or attached datum outliving its
   frame. Mitigated by trailing the edge/`Γ`/data in the SAME frame as the merge, the
   push/pop-reassert oracle, and the strand-the-edge verdict-effect mutant.
3. **Reversal risk on ADR-0010, and the bounded reversibility of the off-switches (M3).** If
   Stage 1b's TCB review finds the justification discipline too costly to get right, the plan
   degrades gracefully to Stage 1a (banked win, no fabric). BUT (codex M3) the "designed-in
   off-ramp" is only a flag-level rollback **through Stage 1b**; past that, each stage's
   fallback has a **reversibility horizon** (§C.1) — Stage 2's Path-1 flag stops being a
   fallback once Stage 3/4 depend on callbacks, Stage 3's infra-only mode once Stage 4 consumes
   it, Stage 4's non-registration cannot reverse the frozen-core constructor changes. Beyond the
   horizon, isolating a regression is a **staged commit revert**, not a flag. Mitigated by the
   per-stage G-equiv manifest (a regression shows as an out-of-manifest transition at the
   stage that introduced it) and the linear-trunk bisectability (every commit green).
4. **Stage 4 frozen-core churn.** The `term.mli`/`context.mli` unfreeze is the real blast
   radius; it is deferred to the last stage, gated independently, and pairs with the already-
   parked #147 — so it does not sit on the critical path for the Stage-1 corpus win.
5. **Stage-0 migration regression.** Consolidating four private trails risks a pop-ordering
   or truncation regression during migration (the exact bug class the substrate exists to
   eliminate). Mitigated by the byte-identical per-module gate (any behavior change fails it),
   one-site-at-a-time migration, and the EUF hot-path carve-out that keeps the perf-critical
   entry representation unchanged. A site that cannot consolidate without regression stays on
   its private trail; only cross-module fabric state (Stages 2–3) is required to live on the
   substrate.

---

## Acceptance evidence per claim (index)

| Claim | Evidence |
|---|---|
| Stage 0 substrate is behavior-preserving | each migrated site's existing push/pop oracle green byte-for-byte; MISMATCH=0 per module; determinism golden (I6) |
| Frame mechanism is hand-rolled ≥4× | `euf.ml:94,126-136,204-262`; `simplex.ml:50-58,531-539`; `combine.ml:90-92,628-648`; `manager.ml:114-146,173-188` |
| Fabric is combinator-internal; seam frozen through Stage 3 | `check-frozen` (`FROZEN.sha256`) unchanged at each of Stages 1–3's land; §E.1 table |
| Merge-with-justification primitive exists | `euf.mli:60,64,92,113` (`assert_eq`/`assert_neq`/`explain`/`explain_implied`) |
| F1 justification-at-injection (consequence path, R1) | precedence mutant driving a CONSEQUENCE of an injected edge KILLED; the injection-time **assertion-order-ledger** check fires on a seeded forward-ref `Γ` (Rev4-1b, no `trail_pos`); MISMATCH=0 (G-sound) |
| F1-SEM semantic Γ verifier (C1) + witness accessor (Rev4-3, math Rev5-B3) | `Lia.fixed_bounds` (additive, non-frozen) supplies ACTIVE EXACT oriented bounds `(value, lower≥, upper≤)`; the verifier confirms TWO oriented implications (`s≤w` from `{s≤v,w≥v}`, `s≥w` from `{s≥v,w≤v}`) — NOT a four-bound sum (`0≤0`); **weak-Γ** mutant (drop one oriented bound) → false `unsat` only with the verifier disabled → KILLED enabled |
| F1c notify-OUT currency (C3) | `new_eq` carries `(s,t,handle,source)`; callback-derived LIA conflict names the merge premises `Γ`; self-notification-suppression test green |
| F2 currency = NON-FROZEN `FABRIC_CHILD` interface, two layers (C2 + Rev5-B1) | engine `'p`/`'tok` → `justification = Real of Lit.t | Fabric of edge_id` (no `euf.mli`/`lia.mli` change); adapter→`Combine` boundary widened via `FABRIC_CHILD` (`check_fabric`/`explain_fabric` returning `Fabric_explanation.t`), the `CONGRUENCE_CHILD` precedent generalized — a `Fabric edge_id` never crosses the frozen `Theory.THEORY` (which is `Lit.t`-only); disjoint namespace; FIRST-wins dup-`Γ`-across-frames mutant KILLED; handle-leak test green; every type spellable against `theory.mli`/`combine.mli` (verified) |
| F2/F4 expansion at BOTH `Combine.check` AND `Combine.explain`, recursive + cache-at-PROPAGATION (Rev5-B1 + Rev4.1 + Rev6-3/Rev6-4) | both seam-return points expand every `Fabric` handle before the SAT core; recursion acyclicity ENFORCED by monotone `edge_id`s (an edge's `Γ` cites only strictly-smaller live handles, checked at injection; visited-set + fail-closed expansion; Rev6-4) — NOT merely asserted; combined-reason cache + `propagated_by` are FIRST-WINS at PROPAGATION time (the #102 discipline, Rev6-3) ⇒ solve-time and cert-time cite byte-identical `Γ`; no `Fabric` handle reaches `theory_explain_checked`; mutants KILLED: substitution-drop, handle-leak-VIA-EXPLAIN (distinct from check-path), **cite-not-smaller-edge (rejected at injection)**, **cache-at-explanation last-wins (the #102-consequence precedence mutant covers it)**, **pop/re-propagate-DIFFERENT-OWNER (Rev6.1: `propagated_by`+combined-reason cache origin-frame trailed; a grow-only untrailed `propagated_by` strands a stale post-pop owner → `Combine.explain` routes to the wrong child → verdict effect; NOT exercised by the #102-consequence mutant, which has no post-pop lifecycle)** |
| F2 empty-Γ (M1) | Stage-1 tripwire fires on seeded fault; Stage-4 empty-Γ+theory-axiom-witness positive accepted, forged-empty rejected |
| F3 backtracking restorable (+ theory-local undo log, Rev4-4; rewind ordering Rev5-B4) | push/pop-reassert oracle green; disjointness property test green; **theory-local undo-log on the CHILD's own trail** reversed by the child's own frame `pop` (single owner — hub does NOT replay on pop); intra-check `rollback_to watermark` (truncating) for fuel/exception aborts only; invariants DR (no double-rewind, `rollback_to` truncates) + SW (watermark check-local); stale-data / double-rewind / stale-watermark mutants each → verdict effect (inspects BOTH classes AND tableau feasibility); re-entrancy test (nested + nonchrono backjump + `cancel_until 0`) green |
| F5 cascade termination/determinism (H1) + fuel rollback (Rev4-4) | determinism golden byte-identical; no-new-terms assertion (Stages 1–3); diseq-echo + redundant-injection mutants KILLED; Stage-4 fuel exhaustion **rolls back / poisons then** → `unknown` (follow-up `check` sees consistent state; skip-rollback mutant KILLED) |
| F6 poison/atomicity (H5) | H1 overflow fixture no-crash; poison-reuse, L1, and mid-cascade-exception mutants KILLED w/ verdict effect |
| F7 cert path (C4 + checker rule Rev4-2, EMISSION-ONLY until #153; EQUALITY-ONLY Rev5-NEW-HIGH) | `on_fabric_eq` event `{edge_id;s;t;Γ;witness}`; the virtual-proposition `v_st` / polarity / endpoint-binding / assumption-discharge replay rule (§B.7) is ADR content #153 implements; the `¬Γ ∨ v_st` clause is checked by the oriented-implication Farkas extension (Rev5-B3), not a contradiction check; wrong-multiplier honeypot rejected; trace-off bit-identical; **fabric disequality injection + its `v_neq`-analogue replay DEFERRED to the Stage-4 diseq-injection delta** |
| A4-erratum contract (§B.5a, H4 + metric Rev5-NEW-MED/Rev6-FWD-3) | under-inclusion mutant KILLED; boundary-only DOMAIN for callbacks/`theory_var` (soundness); e-graph-SIZE counter proportional to the subterm closure of **every congruence-child registration root** (maximal UF apps AND `Both`-routed shared-equality atoms, `combine.ml:345-347`) — not #boundary-nodes, not only maximal UF apps; a pure-arith non-root term = 0 e-nodes; boundary-marker pruning = named future work; cost-accounting only, no soundness impact |
| Two-gate discipline (M2) | G-sound=0 every stage; G-equiv paired diff within the per-stage transition manifest |
| Stage 1 closes mathsat/Wisa | before/after 2s sweep, partial sweep expected; #117 not regressed |
| Stage 4a datatypes | pre-labeled QF_DT suite passes on the Stages 1b–3 primitives; cert delta (M4) landed; **model-value well-formedness validator (constructor-form + occurs-check acyclicity + FWD-2 canonical DT equality) gates `sat` — NOT R1 (Rev5-B5b)**; an opaque/cyclic DT value → `unknown` |
| Stage 4b arrays | pre-labeled QF_AX/QF_AUFLIA suite passes; fuel-exhaustion fixture → `unknown`; cert delta (M4) landed; **finite-function + diff-witness + FWD-2 extensional-equality validator gates `sat` — NOT R1 (Rev5-B5b)**; a non-extensional model → `unknown` |
| FWD-1 N-child fabric composition (Stage-4 GATE, Rev6) | binary `Combine (R)(A)(B)` result is not a `FABRIC_CHILD`; Stage 4 must design a fabric-capable nested result OR a flat N-child hub before a DT/array child joins the shared hub; dual-reviewed; NO Stages-1–3 impact (2-child only) |
| FWD-2 validator semantic completeness (Stage-4 GATE, Rev6) | the model-value validator must check canonical/structural DT equality + extensional array equality (not just shape), co-designed with the DT/array leaf grammar (M4); the semantically-complete validator is the Stage-4 `sat` gate |

## Open questions (for review to settle)

- **OQ1 — RESOLVED (Rev5-B1, the through-line typeability fix).** The F2 realization is settled at
  BOTH layers: (layer 1) the engine token `'p`/`'tok` → `justification = Real of Lit.t | Fabric of
  edge_id` (no `euf.mli`/`lia.mli` change — both are already polymorphic); (layer 2, the fix Rev 4
  missed) the adapter→`Combine` boundary is widened by a NON-FROZEN `FABRIC_CHILD` interface
  (`check_fabric`/`explain_fabric` returning `Fabric_explanation.t`, premises `justification list`),
  the `CONGRUENCE_CHILD` precedent (`combine.mli:175-183`) generalized to both children — because a
  `Fabric edge_id` cannot cross the frozen `Theory.THEORY` (`Lit.t`-only). `Combine` expands at BOTH
  seam-return points (`Combine.check`, `Combine.explain`), recursively, first-wins-cached. This is
  neither Rev-3's "registry with `'p = Lit.t`" (broke on the LIA side) nor Rev-4's "instantiate the
  engine token and `Combine` expands" (which named only layer 1 and was not typeable at layer 2) nor
  a frozen-surface change. No longer an open choice.
- **OQ2 (Stage 1a vs 1b ship order).** Is banking 1a first worth the throw-away of the
  trichotomy-Split trigger when 1b lands? (The trigger is reused; only the discharge changes —
  so the throw-away is small. Confirm.)
- **OQ3 (fix-event surface).** Does LIA expose an incremental "term newly fixed this round +
  value" event cheaply, or must we scan shared Int terms for `lower==upper` each propagate
  round? (`z3-combination-study.md:334-344` — the plumbing risk. If scanning, gate like z3's
  `arith_propagation_threshold`.)
- **OQ4 — RESOLVED (C4/M4): "specify now."** The `Shared_eq` cert path is now specified as a
  concrete additive delta (§B.7: fabric-event stream + certificate sub-DAG, co-designed with
  #153); the datatype/array *leaf* grammar (Stage 4) is a formal Stage-4 prerequisite (M4), not
  a deferred OQ. No longer open.
- **OQ5 — RESOLVED (H4): the interface set survives, unchanged.** The monotone combinator-side
  interface set is retained as `find_disagreement`'s domain and equals the A4-erratum e-node set
  (§B.5a); hub registration populates it, it is not a separate structure. β-hash (which would
  have needed a distinct domain) is cut (H2), so there is no three-way inconsistency to resolve.
  The A4 under-inclusion mutant is imported into the fabric gates (§B.5a).
- **OQ6 (Stage-0 entry representation).** Typed-undo functor (a) vs closure-entry trail (b),
  per module. ADR mandates a per-module choice against an allocation measurement, with EUF
  keeping int-packed typed entries (share scope bookkeeping only). Review to confirm the
  functor's shared-scope machinery does not itself regress the EUF hot path (the carve-out
  assumes scope-sharing is allocation-neutral; measure it).
