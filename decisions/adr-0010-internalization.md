# ADR (RATIFIED): Internalization-based theory combination (each boundary node is its own proxy)

- **Status:** RATIFIED (version-final) — internalization (supersedes the v1–v3 *explicit-purification* drafts). Convergence: same-model v4 APPROVED-WITH-CHANGES (v5); codex v4 CHANGES-NEEDED, architecture affirmed (v6); C6 Bool-compound unsoundness folded (v7); Bool-constant carve-out (v8); both legs FREEZE-APPROVED-MODULO-author-rulings. **Both design-author rulings are now IN** (DESIGN A2 errata): §3.3a splits = **Option B, lazy ℤ-trichotomy Split clause** (directive's "decisions" overruled by evidence); invariant (ii) **BLESSED** in occurrence-monotone form with two normative riders (§3.4). **STAGE 2 is GO** — implementation on `task/internalization`; this ADR is the spec (spec-by-citation, no co-design).
- **Changes v7→v8:** the Bool-under-UF split becomes THREE cases (§3.6): (i) bare Bool variable → leaf bridge (as v7); **(i′) NEW — Bool constant `true`/`false` → NATIVE EUF `true_const`/`false_const`, no bridge, no degrade**; (ii) STRUCTURED Bool compound (`And`/`Or`/`Not`/`Ite`/Bool-`Eq`) → degrade, with "compound" explicitly defined as a structured node EXCLUDING constants. §3.1 now assigns Bool constants an owner (EUF), so a constant under a UF arg is not a boundary crossing. Load-bearing: the leaf fixture `¬b ∧ h(b) ≠ h(false)` contains `h(false)` — lumping constants into "compound" would degrade it to UNKNOWN, both failing it and masking a broken leaf bridge. Added reviewer-confirmed resolved notes (h(ite)→degrade; Bool-returning UF under UF = native EUF-in-EUF; skeleton-shared bare var shares its tag-keyed literal) so the classification is exhaustive on its face. §0 Bool row + §6 leaf fixture updated. **Editorial strike (not a design change):** deleted the stale v6 leftover in §3.6 (`(b∧c) ∧ h(b∧c) ≠ h(true) → UNSAT` with the `=true_const ⇒ congruence` rationale) — it re-asserted the exact C6-unsound bridge the ruling rejects and contradicted §6 (same fixture → UNKNOWN) and Case (ii); a stage-2 builder reading §3.6 alone could have reimplemented the broken bridge from it.
- **Changes v6→v7 (retained):** C6 — the Bool COMPOUND under a UF arg degrades to `unknown` (author ruling); §6 fixtures: Bool-LEAF stays UNSAT, `¬b ∧ h(b∧c) ≠ h(false)` and the old `(b∧c) ∧ h(b∧c) ≠ h(true)` expect UNKNOWN; §9 follow-up `bool-compound-uf-args`.
- **Changes v5→v6 (codex v4 fold):** P2 completion — the Bool bridge's stable SAT literal + per-assertion provenance, with codex's source citations for WHY the bridge is needed (§3.6); P3 — grow-only COMBINATOR state vs EUF's e-node truncation on pop: a stale member is walked only if BOTH children have re-internalized it, else the walk fails closed for it, and the registry-mutant test strengthened to require a VERDICT effect after backjumping over the instance-creation branch (§3.3/§6); P5 — two honesty fixes: (a) "gate certifies the original trivially" reworded — the wrong-SAT fixtures are the load-bearing check and are NOT retired (§3.5); (b) arrangement eq/order atoms are INTERNAL SAT atoms, excluded from input unsat-cores and origin-classified by certificates (§0 core row + §3.3); P6 — invariant (ii) reformulated (membership is occurrence/use-history-sensitive, a monotone union; only REGISTRATION EVENTS are idempotent/tag-keyed) with the `x≤0; -x≤0; f(x)≠f(0)` transition fixture — **this corrects the letter of the design author's verbatim invariant (ii) while preserving its intent (idempotent, deduplicated, structural); flagged for the author** (§3.4). Invariant (iv) caveat added (uninterpreted-only triggers = heuristic coverage, not a completeness claim). Confirmed-good items untouched (numeral corner, per-ground-check termination, invariants (i)/(iii) verbatim, numeric authority).
- **Scope:** rebuild `smt/combine` around internalization. **The CDCL(T) seam and the frozen `THEORY` interface are KEPT** (no e-graph hub); the EUF/LIA adapters are unchanged. There is **no** preprocess purify pass (the v3 plan): internalization needs none.
- **Owning task:** #23 (combination rebuild by internalization).
- **What transfers from v3:** the bug-family diagnosis (§1), LIA-numeric-authority + negative-eq asymmetry (§4), the W2 guard enumeration (§5), and the acceptance corpus (§6) all carry; the proxy/defining-equation machinery and its purity/conservative-extension apparatus are dissolved by construction (§0).

## 0. Carry-over table — every v3 item → carried / dissolved / reworked

| v3 item | disposition under internalization (v4–v6) |
|---|---|
| Explicit proxy variables + defining equations (v3 §3.1/§3.3) | **DISSOLVED.** Each term node is its own proxy; ownership by head symbol. No fresh `.oxsmt.*` symbols, no defining equations, no preprocess pass. |
| Numeral corner — `1` under `f(1)` must be shared (v3 CRITICAL) | **DISSOLVED by construction; fixture KEPT.** The `1` node is arith-headed (LIA) under an EUF parent `f` → a boundary node by definition → in the interface set automatically. The `f(1)`/numeral fixture stays in acceptance to pin it. |
| Dynamic interface set, structural at registration, option (a) (v3 §3.2) | **CARRIED & SHARPENED.** Still structural, combinator-side, no seam change — now over *original* boundary nodes, not proxies. |
| G-PURITY mechanical purity checker (atom-level) | **REWORKED into interface-totality invariants (ii) + registry mutants.** With no transformation there is no "purity of a rewritten atom" to check; the property that matters becomes "interface registration is a total, idempotent function of the node" (invariant (ii)), guarded by registry-mutant tests. |
| G-INCREMENTAL (frame-scoped proxies retract on pop) | **REWORKED into invariant (i).** Internalized interface state is **grow-only and retraction-free** (asserted at the quantifier's scope, never the current decision level) — simpler than frame-scoped retraction; the push/pop-reassert fixture stays (idempotent re-registration of the same hash-consed node). |
| Conservative-extension contract + two-directional oracle | **DISSOLVED.** No transformation is performed, so there is nothing to be conservative *over*; the Lean gate certifies the ORIGINAL input trivially because the solver reasons over original terms end to end (§3.5). |
| G-MODEL proxy-erasure / reconstruction | **DISSOLVED (no proxies to erase); LIA-numeric-authority + neg-eq asymmetry KEPT.** `Combine.model` is already over original vocabulary. The v3 §4 rules that survive: LIA is the numeric authority for Int values; negative shared equalities route to EUF only. |
| Bool-boundary handling (`h(b∧c)`) | **REWORKED → CORRECTED (C6, v7) → THREE cases (v8).** v5/v6's single `K_bool` bridge was unsound for compounds (`K_bool` names a nullary leaf, so `h(b∧c)` became an opaque atom decoupled from `b`,`c`; missed by both legs, caught in v6 confirmation). v8 §3.6 splits it three ways: (i) Bool LEAF `h(p)` → bridge + value transfer (correct); (i′) Bool CONSTANT `h(true)`/`h(false)` → native EUF, no bridge/no degrade (the carve-out that keeps the leaf fixture UNSAT); (ii) STRUCTURED Bool COMPOUND (`And`/`Or`/`Not`/`Ite`/Bool-`Eq`, excluding constants) → **degrade to `unknown`** (design-author ruling; option (a) Tseitin coupling is the §9 follow-up). None enters the integer arrangement. |
| W2 overflow-guard enumeration | **CARRIED (§5).** Surviving checked add/mul sites enumerated, each fail-closed to `unknown`. |
| G-STABLE stably-infinite/disjoint-signature precondition | **CARRIED (§3, Soundness).** Unchanged — the model-combination completeness precondition, proxy or internalization. |
| DAG-aware interning by `Term.tag` | **CARRIED as invariant (ii)** — registration is a pure function of the hash-consed node, idempotent/deduplicated. |
| Deep-tower split-round caveat; pin/decision-frame discipline | **CARRIED** (§3 Termination; the seam still requests splits and the pin/decision discipline holds). |
| Unsat-core proxy-origin provenance (v3 §3.6) | **REWORKED, sharper than "dissolved" (codex P5b).** No proxies/defining equations, so cores ride the ordinary premise-`Lit` currency — but the arrangement eq/order atoms the seam mints ARE **internal SAT atoms** (even though their operands are original terms). So unsat-core reporting must **EXCLUDE them from input cores**, and certificates must **classify their origin** (combinator-internal, not a user assertion). Not a proxy-origin table, but not a no-op either. (§3.3) |
| `euf_domain` gate; `owner(Le)=Both` | **DELETED** (as v3 promised) — dissolved by construction, with less machinery. |

## 1. Context — a bug family, one missing cause (unchanged from v3)

`smt/combine` went through seven review rounds fixing the same defect in new guises: W1 (`x=y ∧ f(x)<f(y)` wrong-SAT — EUF never saw the shared applications), fresh-sum non-termination (`owner(Le)=Both` dragging opaque sums into the disagreement search), R1 (`x=0 ∧ f(x+1)<f(1)` wrong-SAT — the `euf_domain` gate over-excluding a genuinely shared arithmetic term), and G1 (pure-QF_LIA O(N²) arrangement splits). Every fix was a hand-approximation of the shared-variable set — too small (wrong-SAT) or too large (non-termination). DESIGN §6 names the intended mechanism (Nelson–Oppen with purification); it was never implemented. v1–v3 proposed *explicit* purification; v4 achieves the same shared set by **internalization**, with less machinery.

## 2. Decision

Rebuild `smt/combine` around **internalization**:
- **No fresh proxy variables, no defining equations.** Each term node is its own proxy.
- **Theory ownership is by head symbol.** Uninterpreted function/predicate applications → EUF; arithmetic operators and numerals → LIA; bare variables are theory-neutral leaves.
- **The interface set = boundary-crossing nodes** — a node whose owner differs from its parent's owner (an arith-headed node under `f`; an EUF-headed node inside a sum, which LIA treats as an opaque variable), plus a bare variable used as an operand by both owners (§3.1). Computed **structurally at assertion time**; sharedness is **total by construction**, never the output of a relevance filter.
- **Keep the seam; do NOT adopt an e-graph hub.** Equality exchange between theories stays at the CDCL(T) seam, as SAT-visible atoms over ORIGINAL terms. EUF remains an ordinary plugin behind the frozen interface. Rationale: decoupling (the seam is the parallel-workstream boundary), reasons-purity, and certificates — the exchange over original terms needs no definitional layer in Lean replay. The forcing mechanism is a **lazy ℤ-trichotomy Split clause** (design-author ruling, §3.3a) — no seam unfreeze, so the frozen surface is untouched by construction.

This deletes `euf_domain`, `owner(Le)=Both`, and the whole bug family with less machinery than the purification draft.

## 3. Design

### 3.1 Ownership, boundary nodes, and the interface set
Ownership is a pure function of a node's head: an application of an uninterpreted symbol (arity ≥ 1) or an uninterpreted equality → **EUF**; an arithmetic operator (`+`, `*`, `≤`) or a numeral/Int constant → **LIA**; **the Bool constants `true`/`false` → EUF** (they *are* EUF's anchored `true_const`/`false_const`); a bare variable (nullary) is **neutral** (ownerless). Because a Bool constant is EUF-owned, `h(false)` is EUF-under-EUF — *not* a boundary crossing at all, so it never triggers the Bool bridge or the degrade (§3.6 case (i′)).

The interface set has exactly two, non-overlapping sources:
- **Boundary nodes — the rule ranges over OWNED nodes only.** An owned node is a boundary node when it has a parent EDGE whose parent's owner differs from its own. **Boundary-ness is per-USE, not per-node:** terms are a hash-consed DAG, so a single node has many parents; it is an interface member if *any* of its parent edges crosses ownership (e.g. `x+1` occurring both under `f` — a crossing — and inside another sum — not a crossing — is still an interface member via the first edge). Examples: `x+1` (LIA) under `f` (EUF); the numeral `1` (LIA) under `f` (EUF); `f(a)` (EUF) inside a `+` (LIA).
- **Both-used neutral variables.** A neutral (ownerless) variable enters the interface set ONLY via this clause: it is used as an operand by both an EUF-owned and a LIA-owned node. Neutral variables are *never* reached by the boundary rule (they have no owner to differ), so the two sources do not double-count and none is missed. A variable confined to one theory's atoms is not shared — so a **pure-QF_LIA input has no EUF-owned node, hence no crossing edge and no both-used variable → empty interface set** (⇒ zero arrangement splits; the G1 fix, by construction). *(This resolves the v4 §8 flag as the reviewer adjudicated: owned-node boundary rule + separate neutral-both-used clause.)*

The interface set is computed by a **TOTAL structural walk of each asserted term at assertion time (C2)** — total over ALL node kinds, and in particular **descending BOTH sides of an equality atom** (the `f(x) = x+y` shape: the walk visits `f(x)` on the left and the sum `x+y` on the right, recording each crossing). For each parent→child edge it compares owners and records a crossing; there is no relevance heuristic and no node kind is skipped, which is exactly what dissolves the "too-small/too-large approximation" bug family. (This assert-time interface-registration walk is distinct from §3.3's `Final`-time seam walk over the already-built interface set.)

**A bare variable that is an (dis)equality OPERAND is a use by the congruence child (stage-2 codex H1).** The congruence child decides every equality (merge for `=`, disequality for `≠`, the latter routed EUF-only by S1), so a bare Int variable occurring as an equality side is EUF-*used* even though the `Eq` node itself is neutral. It enters the interface via the both-used clause exactly when it is ALSO used by an arithmetic node — e.g. `(distinct x y) ∧ x≤y ∧ y≤x` (LIA entails `x=y`, EUF holds `x≠y`; without the EUF-use bit the interface is empty and the disagreement is missed → wrong SAT). A bare (dis)equality operand with no arithmetic occurrence stays non-interface (a pure disequality does not, alone, make its operands shared).

**PRECONDITION — preprocessed fragment (stage-2 codex).** The walk assumes every asserted term is in the preprocessed QF_UFLIA fragment: **no Int-sorted `Ite`** and no reserved `div`/`mod` (ADR-0003 invariant 10). An Int `Ite` is a neutral node whose Int branches under a neutral parent would take no use-bit, so combination correctness depends on smt/preprocess lifting it before assertion. (A Bool `Ite` as a UF argument is handled — it degrades via `Incomplete`, §3.6 case (ii).) The combinator carries a debug assertion against a residual Int `Ite`.

**Internalization (why no proxy is needed).** Each theory already treats a foreign-owned boundary node as an opaque atom of its own: EUF treats `x+1` (as an argument of `f`) as an opaque leaf and congruence-closes over it by identity; LIA treats `f(a)` (inside a sum) as an opaque Int variable. So the boundary node *is* the shared variable — no fresh symbol, no defining equation. The combinator's only job is to (a) ensure both theories register the boundary nodes and (b) run equality exchange over the interface set at the seam.

### 3.2 Spec-by-citation (stated verbatim)
> Each boundary node is its own proxy; this implements Nelson–Oppen-with-purification, per de Moura & Bjørner's observation that internalization makes explicit proxies unnecessary.

Reviewers check conformance against the textbook Nelson–Oppen-with-purification account **through that correspondence**: a boundary node here plays the exact role the purification variable plays there.

### 3.3 Seam equality exchange
At `Final`, if both children are `Sat`, the seam **walks the current interface set in canonical term-id order, bucketing interface nodes by their candidate model value**; where the two children's models place two interface nodes in different equality relations, it requests the ℤ-trichotomy over those ORIGINAL terms. EUF and LIA see only original terms; no definitional layer reaches Lean replay. The pin/decision discipline (rounds 5/6) is retained.

**Arrangement atoms are internal SAT atoms (codex P5b).** The eq/order atoms the seam mints have original-term operands but are **combinator-internal**, not user assertions. Unsat-core reporting **excludes them from input cores**, and certificate production **classifies their origin** as combinator-internal (an arrangement decision), never attributing them to the user's assertion set.

**Both-valued skip + child re-internalization (load-bearing for grow-only soundness; codex P3).** The interface set is grow-only (invariant (i)), but the CHILDREN are not: EUF truncates its e-nodes on `pop` (euf.ml:619), so a combinator interface member can outlive the child e-node that backed it. Guard: before the seam compares a stale member, **both children must currently value it (have re-internalized it); otherwise the walk fails closed for that member and skips it.** A stale post-pop member appears in no live atom ⇒ at least one child does not value it ⇒ skipped ⇒ it can never produce a spurious disagreement/split. This is what makes the grow-only combinator set sound against child truncation: keeping stale members costs a walk step, never a wrong verdict, and never a read of a truncated child e-node.

### 3.3a Split mechanism — RESOLVED: Option B, the ℤ-trichotomy Split CLAUSE (design-author ruling; codex P4)
The shared equality is forced by a **trichotomy Split clause**, NOT a theory-requested decision. The directive's "decisions" wording is **overruled by evidence** (codex source-verified that the frozen CDCL(T) seam has no decision channel). The author's reasoning, recorded:

- **The discardability hazard does not apply.** That hazard is about a two-literal `A ∨ ¬A`, which level-0 cleanup drops as a tautology. The ℤ-trichotomy is **three distinct atoms** (`x=y ∨ x<y ∨ x>y`) — propositionally NOT a tautology, it genuinely constrains the boolean space, and no cleanup may discard it.
- **Reasons-purity is preserved.** The trichotomy is a valid LIA lemma, so it enters as **one uniform "theory-valid clause" certificate step**; the branch choice stays an ordinary SAT decision. Fact (the valid clause) and guess (the branch) land in their correct lanes — which was the principle behind "decisions" all along.

**Conditions (normative):**
- Clauses are created **LAZILY** — only for a pair the seam actually questions (a model disagreement), so the number of clauses is bounded by the interface set, not by all pairs.
- Every emitted clause is **multi-literal and non-tautological.**
- For a **non-arithmetic sort**, there is no `<`/`>`, so use the **equality atom with disequality routed via EUF** — the clause exists only where a real third literal does (never a degenerate `A ∨ ¬A`).

**Watch item (recorded; bought on measurement only).** Split promptness rides the `Final`-check re-fail loop (the seam re-discovers the disagreement each `Final`). If the perf corpus shows combination-heavy lag, a decision-request / phase-hint hook is a **Tranche-C candidate** — not built now.

**Author's lesson note (verbatim, ratification record):** *directives state invariants, not mechanisms.* (The directive fixed the invariant — shared equalities are exchanged over original terms at the seam — and named "decisions" as a mechanism; the mechanism was the overrulable part, the invariant was not.)

Acceptance (§6) and soundness (§ Soundness) are unaffected — they concern WHICH pair is exchanged, not the forcing mechanism.

### 3.4 Lemma-readiness invariants (stated verbatim, ADR-worthy)
1. Instantiated instances and all their interface bookkeeping are asserted at the scope of their quantifier, never at the current decision level — all interface state is grow-only and retraction-free; registry mutant: an instance asserted at decision level must be caught by a test where backtracking strands interface state.
2. Interface registration is a pure function of the hash-consed node — idempotent, deduplicated.
3. The seam walks the current interface set in canonical term-id order, bucketing by candidate value; termination is scoped PER GROUND CHECK (finite current set, each pair split once per branch) — do NOT claim it globally across instantiation rounds.
4. Triggers follow the standard discipline (uninterpreted symbols only); arithmetic lives in lemma bodies, handled by the assert-time pipeline like any assertion.

**P6 correction to invariant (ii) — the letter, not the intent (flagged for the design author).** Codex showed that interface *membership* is NOT a pure function of a node in isolation: it is **occurrence/use-history-sensitive**. Trigger: `assert x≤0; assert -x≤0; check` (here `x` is LIA-only ⇒ not an interface member) `; then assert f(x)≠f(0); check` — the second check must flip to **UNSAT** (now `x` is used by both LIA and EUF ⇒ a both-used interface member, and `f(x)=f(0)` follows from `x=0`). A tag-memoized "`x` is not an interface member" classification cached from the first check would miss the transition and wrong-SAT. **Reformulation:** interface *membership* is the **monotone union of observed `(node, parent-owner)` occurrences plus per-owner use bits** — it only ever grows as new occurrences arrive; **registration EVENTS remain idempotent and tag-keyed** (deterministic dedup — re-seeing the same occurrence is a no-op). This preserves the invariant's intent (idempotent, deduplicated, structural) while correcting the claim that membership is a function of the node alone. Acceptance adds the transition fixture (§6).

**BLESSED by the design author, with two normative riders:**
- **(rider i) Over-approximation is the safe error direction.** When a cache or a push/pop boundary leaves membership in doubt, resolve toward **MORE** shared, not less: conservatively **retain** a member until its creating scope pops. An extra member is harmless (the both-valued skip drops it, §3.3); a missing member is a wrong-SAT. So the monotone union always errs large.
- **(rider ii) The debug check has the correct clock: membership is CONSTANT between assertions.** It is snapshot-identical across branching rounds, cut generation, and seam passes, and grows **only AT an assertion**. That is the mechanical tripwire — any membership change observed *between* two assertions (during solving) is a bug. The membership-snapshot check joins the registry-mutant test list (§6).

**Invariant (iv) caveat (codex).** The uninterpreted-symbols-only trigger discipline is a **heuristic coverage restriction, not a completeness claim** — it bounds which quantifier instances E-matching generates (stage 2), and does not assert that all needed instances are found.

### 3.5 Gate meaning (conservative-extension dissolved)
The Lean decide-goal oracle certifies the ORIGINAL VC / corpus input. Because internalization performs **no transformation** — the solver asserts and reasons over the user's original terms — there is no rewritten form to be "conservative over" and no proxy for Lean to see, so the v3 conservative-extension contract and its two-directional oracle are dissolved.

**Honesty fix (codex P5a): the absence of a transformation is NOT a proof of the combination algorithm.** Dissolving the conservative-extension oracle removes a check that was only ever about the *purification rewrite*; it says nothing about whether the internalized combination computes the right verdict. The load-bearing correctness check is therefore the **acceptance corpus (§6)** run through the real stack — the wrong-SAT repros (W1, the tower, R1, `f(x+1)<f(y+1)`, the numeral corner, the Bool leaf), plus the Bool-compound degrade witnesses. **Those fixtures are NOT retired**; they are the primary evidence the algorithm is sound, and the gate over the original input is necessary but not sufficient on its own.

### 3.6 The Bool boundary — three cases (C6 ruling + Bool-constant carve-out; supersedes the v5/v6 bridge)
A Boolean node as an argument of an uninterpreted function (`h(…)` with a Bool argument) does NOT use the ℤ-trichotomy of §3.3 (no LIA, no numeric arrangement; the driver is the SAT core's assignment). It splits into **three cases**:

**Case (i): a Bool LEAF as a UF argument — `h(p)`, `p` a bare Bool variable.** Supported by the bridge WHEN `p` SURFACES as a SAT atom. `p`'s own literal (it is a `Predicate` atom, so it is routed to EUF) asserts `p = true_const` / `p = false_const` into EUF via the adapter's existing `K_bool` / `true ≠ false` encoding on the SAT core's assignment, at the same trail level, retracted with it — normal trail machinery, no bespoke bridge object. EUF then learns `p`'s truth and congruence over `h` fires.

**ERRATUM (stage-2 codex H2) — surfaced vs BURIED.** The bridge works only when `p`'s truth reaches EUF, i.e. when `p` is a SAT atom. A `p` occurring ONLY under the UF argument (buried — e.g. `h(p) ≠ h(true) ∧ h(p) ≠ h(false)`, where `p` is never a top-level literal) is never asserted, so EUF keeps it a third opaque Boolean class and the combinator would wrong-SAT. Ruling (outcome semantics): at the Sat-certification point require every Bool-UF-argument to be EUF-bound to `true`/`false`; a buried-unbound one (bare var OR Bool-returning UF, e.g. `h(g z)`) **degrades via `Incomplete` → unknown**. A surfaced/bound leaf stays decidable (case (i) UNSAT retained).

**The 2-valuedness Split (`p = true_const ∨ p = false_const`) is STRUCTURALLY UNAVAILABLE for a bare Bool variable — do not re-propose it.** `Eq(p, true_const)` has two Bool sides, so `Theory_view.is_atom` is FALSE (a Bool-`Eq` is an iff/connective): it cannot be a CONTRACT-SPLIT disjunct (the split contract requires atoms), and it clausifies to `p ∨ ¬p` — the 2-literal tautology §3.3a says level-0 cleanup drops (the trichotomy's 3-distinct-atom escape does not apply: a single Bool variable has no third atom). So the sound mechanism is the degrade above; a precise coupling is the §9 Tseitin follow-up (`ℓ ↔ (…)` in clausification), not a combinator split.

**Case (i′): a Bool CONSTANT as a UF argument — `h(true)`, `h(false)`.** **Native — no bridge, no degrade.** A Bool constant IS EUF's `true_const`/`false_const` (owned by EUF, §3.1), already anchored in the e-graph, so `h(false)` is a pure EUF term with nothing to transfer. This carve-out is **load-bearing**: the case-(i) leaf fixture `¬b ∧ h(b) ≠ h(false)` contains `h(false)`; if constants were lumped into "compound" (case (ii)), that fixture would degrade to UNKNOWN — both failing it and masking a broken leaf bridge (the exact failure the fixture guards).

**Case (ii): a STRUCTURED Bool COMPOUND as a UF argument — `h(b ∧ c)`, `h(¬b)`, `h(ite …)`, `h(b = c)` (Bool-Eq).** **Degrade the goal to `unknown` (C6 ruling, option b).** "Compound" here means a **structured Boolean node (`And`/`Or`/`Not`/`Ite`/Bool-`Eq`), explicitly EXCLUDING the constants of case (i′) and the leaf of case (i).** *Why the v5/v6 bridge was wrong (C6, missed by BOTH legs, caught in the v6 confirmation round):* the `K_bool` encoding names a **nullary leaf**; applied to a compound `b∧c` it makes the boundary atom OPAQUE — there is no `ℓ ↔ (b∧c)` definition tying the bridge literal to `b` and `c`. So SAT can set the opaque `b∧c` atom **independently of `b`,`c`**. Trigger: `¬b ∧ h(b∧c) ≠ h(false)` is genuinely UNSAT (`¬b ⇒ b∧c = false ⇒ h(b∧c) = h(false)`), but under the v6 bridge SAT set the opaque atom true and returned wrong-SAT. (The v6 fixture `(b∧c) ∧ h(b∧c) ≠ h(true)` asserted the connective directly, so it dodged the hole.) Rather than encode a coupling, the session **detects a structured Bool compound under a UF argument at the same assert-time interface walk and degrades to `unknown`** — sound, matching the v1 SOUNDNESS RULE / degrade-don't-lie pattern.

**Resolved classification notes (reviewer-confirmed clean under this wording; recorded so the split is exhaustive on its face):**
- `h(ite(…))` — the `Ite` is a structured Bool node ⇒ case (ii) ⇒ degrade (named above).
- A **Bool-returning UF under a UF** — `h(g(x))` with `g : … → Bool` — is native EUF-in-EUF (both owned by EUF), not a Bool boundary at all; no bridge, no degrade.
- A **skeleton-shared bare Bool var** (the same `p` occurring both as a top-level literal and as `h(p)`) shares its single tag-keyed bridge literal (invariant (ii), §3.4) — one literal per node, so the top-level truth and the argument truth are the same SAT variable.

**Completeness follow-up (not this ADR).** Option (a) — Tseitin-defining bridge literals (`ℓ ↔ (b∧c)` pushed into clausification) — would make case (ii) precise, but extends clausification for a corpus-rare shape. Recorded as `bool-compound-uf-args` (§9), triggered by corpus/VC evidence of demand.

No case ever enters the §3.3 integer arrangement. (Acceptance fixtures for all three cases are in §6.)

### Soundness
The interface set is total: every term shared across theories surfaces as a boundary node (or a both-used variable), so no shared term is invisible to either theory — the W1/R1 class cannot arise. The numeral corner is closed by construction (a numeral under an uninterpreted function is a boundary node). **Completeness precondition (G-STABLE, carried):** EUF and LIA/ℤ are stably infinite with disjoint signatures, so both children `Sat` + agreement on the interface arrangement ⇒ an amalgamated combined model exists. This is the recorded guard rail for any future theory plugin.

### Termination (terminating, not efficient; scoped per ground check — invariant (iii))
For a single ground check the interface set is finite and each disagreeing pair is split at most once per branch, so the split depth per path is bounded by `|interface|²`; the split tree is finite but worst-case exponential in `|interface|` (inherent arrangement search, not a regression) — **terminating, not efficient**. Per invariant (iii) this bound is claimed **per ground check only**, not globally across instantiation rounds (stage-2 E-matching adds rounds). The trichotomy's `<`/`>` disjuncts are pure LIA over existing interface nodes; the internal sum `p_i − p_j` is LIA-internal (arith under arith), never a boundary node, never a split candidate — so the fresh-sum flooding that motivated `euf_domain` cannot occur.

## 4. Combinator changes

- **Delete `euf_domain`** and its `find_disagreement` `Arith` gate; **delete `owner(Le)=Both`** — both dissolved (the interface set is the boundary nodes, computed structurally).
- **LIA is the numeric authority (carried from v3 §4, NOT deleted).** EUF emits `Uninterp` class-ids even for Int-sorted terms, so the merged model takes an interface member's Int value from LIA and uses EUF only for the equality arrangement. This is explicit theory-role knowledge, not a positional `int_variant` accident.
- **Negative-equality routing STAYS asymmetric (carried from v3 §4).** The LIA adapter still lacks disequality; a negative shared equality routes to EUF only (the S1 polarity contract). Internalization does not change LIA's fragment.
- `find_disagreement` becomes a walk of the interface set (invariant (iii)) with **no per-case exclusion gate**.

## 5. What stays

- **The CDCL(T) seam and the frozen `THEORY` interface** — kept; no e-graph hub. EUF is an ordinary plugin behind the frozen interface; the interface set lives combinator-side, derived from the `register_atom` stream (which already carries `(Atom.t, Term.t)`), so no signature change.
- The EUF/LIA adapters — unchanged; they already internalize foreign-owned nodes as opaque atoms.
- **W2 overflow guards — enumerated surviving sites, each fail-closed to `unknown`:** (1) any residual model-value arithmetic in the seam's value bucketing (`coeff*v`, `Σ`) — checked mul/add, raise→CONTRACT-POISON→unknown; (2) pin/decision satisfaction checks that evaluate an interface node's value. Each keeps the round-7 `add_guard`/`mul_guard` fail-closed behavior; the rebuild must not silently drop a guarded site.
- The rounds-5/6 pin/decision-frame discipline.

## 5a. Mechanism erratum (build-time; escalation-caught) — corrects §4/§5

**This is a labeled correction, not a silent rewrite.** A ratified ADR mechanism claim did not survive contact with the engine; the escalation discipline caught it at build time (task/internalization, stage 2). The design OUTCOME is unchanged — no degrade; R1, `x=y ∧ f(x+1)<f(y+1)`, `k(x+1)`/`p(x+1)` all still reach UNSAT — only the LIA-side valuation *mechanism* is corrected.

- **What §4 got wrong:** "`model_eval` fold → plain lookup where possible; proxies/interface members are leaves LIA values directly." **Inverted.** `Lia.model` returns only simplex PROBLEM VARIABLES (leaves), and `Lia.register_atom` acts only on `Le` atoms (any other term falls through `| _ -> ()`), and `Lia.rational_value` is zero-for-unseen. So LIA never model-values a compound boundary node like `x+1` — no registration makes it. (Sources: `lia.ml` `register_atom` `Le`-only fall-through; `lia.mli` `model : (Term.t * int) list` = problem leaves; `rational_value` zero-for-unseen.)
- **The correct mechanism (central, not residual):** the seam's **`model_eval` FOLD** computes a compound boundary node's LIA value from LIA's leaf values (`x=0 ⇒ x+1 = 0+1 = 1`). This is the load-bearing LIA-side valuation path, and the **W2 `add_guard`/`mul_guard` overflow guards are load-bearing on it** — NOT the "residual" of §5.
- **Unconstrained-leaf defaults (review will ask first):** the fold inherits LIA's reviewed round-6/7 default — an unconstrained/unseen leaf reads as LIA's committed default (**zero for unseen**), so the fold returns a concrete value rather than `None` for such leaves; `None` is reserved for a genuinely-unvalued leaf, which for a child that certified `Sat` is a real contract breach (→ poison → unknown).
- **LIA-side internalization is NOT part of the design.** `LIA.internalize_term` was added, found to be a no-op for compounds, and **removed** (dead API is worse than none). **`EUF.internalize_term` is KEPT** — it is load-bearing (full-closure e-nodes let congruence fire after a split) and replaces the deleted `owner(Le)=Both`/K_foreign; it carries a pin test so an adapter refactor that breaks its effect fails loudly. So §5's "adapters unchanged" is corrected to: **EUF adapter gains `internalize_term` (public, additive, verified non-frozen); LIA adapter unchanged.**
- **Unchanged by this erratum:** `euf_domain` + its gate still DELETE; the fresh-sum explosion is still structurally gone (the interface set excludes non-boundary Arith sums); the interface set is the structural boundary-node set.

## 5b. Review hooks — the fold is load-bearing again (must not be skipped)
Because the `model_eval` fold is central (5a), the dual-review brief MUST treat it as load-bearing and check:
1. **Overflow fail-closed on EVERY path** — every `add_guard`/`mul_guard` site raises (→ CONTRACT-POISON → unknown), never wraps; no fold path bypasses a guard.
2. **Unconstrained-leaf defaults** — the fold's handling of LIA's zero-for-unseen leaves is sound (a defaulted leaf can only make a pin/arrangement check *stricter*, never launder a violation).
3. **No double-counting** — for a single node, the seam must not mix LIA's folded Int value and EUF's `Uninterp` class value into one comparison; `value_equal` compares within one model per side, and the numeric authority rule (LIA for Int values, EUF for arrangement) keeps them separate.

## 6. Acceptance (unchanged corpus)

Through the **real** Combine + EUF + LIA stack (no mocks), the full pinned repro corpus is correct:
- W1 pair `x=y ∧ f(x)<f(y)` → UNSAT.
- Nested tower `x=y ∧ g(f(x))<g(f(y))` → UNSAT.
- codex round-7 R1 `x=0 ∧ f(x+1)<f(1)` → UNSAT.
- **`x=y ∧ f(x+1)<f(y+1)`** → UNSAT (second dual-leg repro: boundary nodes `x+1`,`y+1` under `f`, plus `f(x+1)`,`f(y+1)` under `<`).
- **numeral-corner fixture:** an `f(1)`-shaped input where a numeral under an uninterpreted function is the shared term (e.g. `x=1 ∧ f(x) ≠ f(1)` → UNSAT) — proves the numeral node is a boundary node by construction.
- **Bool-boundary fixtures (§3.6, C6 ruling; ERRATUM — stage-2 codex H2, surfaced-vs-buried):**
  The v7/v8 wording ("the leaf bridge + value transfer must actually work") was too strong:
  a Bool leaf under a UF argument is native ONLY when EUF binds it to `true`/`false`, which
  requires it to SURFACE as a SAT atom (asserted via K_bool). A BURIED leaf (only under the
  UF argument, never a SAT atom) stays a third opaque EUF Boolean class and would wrong-SAT
  (codex H2). Design-author ruling (outcome semantics): buried-unbound → degrade via
  `Incomplete`; a surfaced/bound leaf stays decidable. The **buried-unbound degrade is the
  ruled cost** (a genuinely-SAT buried shape becomes `unknown`). Fixtures:
  - **surfaced/bound leaf, trio (case (i)+(i′)):** `¬b ∧ h(b) ≠ h(false)` → UNSAT (`b` bound
    false, congruence fires; `h(false)` native EUF); `b ∧ h(b) ≠ h(false)` → SAT (`b` bound
    true, args differ, no degrade); `h(b) ≠ h(false)` ALONE → UNKNOWN (`b` buried+unbound).
  - **buried leaf wrong-SAT trigger (H2):** `h(b) ≠ h(true) ∧ h(b) ≠ h(false)` → UNKNOWN
    (was wrong-SAT: EUF kept `b` a third class). **Buried Bool-returning UF sibling:**
    `h(g z) ≠ h(true) ∧ h(g z) ≠ h(false)`, `g : Int→Bool` → UNKNOWN.
  - **compound-under-UF degrade (unchanged):** `¬b ∧ h(b∧c) ≠ h(false)` → UNKNOWN and the old
    v6 `(b∧c) ∧ h(b∧c) ≠ h(true)` → UNKNOWN (structured compound; degrades at walk time).
- The round-6 checklist cases (C1–C4, S1, S8–S10, T1–T6) → unchanged verdicts.
- **push/pop-reassert fixture (invariant (i)/(ii)):** a mixed term asserted in a frame → `pop` → re-assert → identical verdict and model (grow-only, idempotent re-registration of the same hash-consed node).
- **deep-tower fixture:** depth-`d` `g(f(…))` tower → UNSAT within the per-ground-check split budget.
- **pure-QF_LIA perf fixture:** an **EMPTY interface set → zero arrangement splits** — pins that `owner(Le)=Both`'s O(N²) blow-up is dead, not merely relocated.
- **use-history transition fixture (P6, invariant (ii) reformulation):** `assert x≤0; assert -x≤0; check` (SAT — `x` LIA-only) `; assert f(x)≠f(0); check` must flip to **UNSAT** — `x` becomes a both-used interface member on the second check; catches a tag-memoized "not an interface member" classification that misses the transition.
- **registry mutants (strengthened, codex P3):** the mutant test must **backjump OVER the instance-creation branch** and then require the stranded instance's **VERDICT effect** (a wrong SAT/UNSAT), not merely observe a stranded interface member — a stranded member that never changes a verdict is not a proof of the bug. Also: an interface registration performed at the current decision level (instead of quantifier scope) must be caught this way; a non-idempotent registration is caught by a dedup test; and a **mixed-equality totality test** — the walk on `f(x) = x+y` must record BOTH `f(x)` and the crossings inside `x+y` (C2), catching a walk that descends only one side of an equality.
- **membership-snapshot check (invariant (ii) rider ii):** interface membership is snapshot-identical across branching rounds / cut generation / seam passes between two assertions, growing only AT an assertion — a test asserts the snapshot is unchanged across a solve that branches and re-checks, catching any mid-solve membership mutation.
- The seam walk has no per-case exclusion gate; `Combine.model` presents original-vocabulary values (no proxies to leak); determinism byte-identical across two runs (I6); `check-frozen` unchanged.

`combine.ml` is **rebuilt, not patched**: the round-7 tip (`task/combination @2317fe1`) does **not** land as-is; its committed repro tests migrate into this suite.

## 7. Honest costs

- The interface walk runs at each assertion (structural, per node) and the seam walk at each ground `Final` — measurable per-query overhead to quantify on the corpus re-run, though lighter than v3 (no proxy atoms/defining equations added to the problem).
- The split tree is worst-case exponential in `|interface|` per ground check (inherent arrangement search); deep towers cost up to `d` split rounds (no EUF→LIA equality propagation).
- Termination is claimed per ground check only; stage-2 E-matching adds instantiation rounds whose global termination is a separate concern (invariant (iii)).
- **Grow-only stale members.** Interface membership is never retracted (invariant (i)), so after a `pop` the set holds stale entries for popped assertions. Cost: a slightly larger set to walk at `Final` (unbounded across a long push/pop-heavy episode). Sound because the seam's **both-valued skip (§3.3)** drops any member not currently valued by both children — a stale member appears in no live atom, so it is skipped and cannot cause a spurious split. The trade is walk-time for retraction-free simplicity and lemma-readiness.
- The official M4 corpus measurement slips behind this rebuild.

## 8. Resolved — the v4 open point (adjudicated by the same-model pass)

- **Shared bare variables (was v4 §8, now resolved in §3.1).** The reviewer adjudicated: the boundary rule ranges over **owned** nodes; a neutral (ownerless) variable enters the interface set **only** via the separate both-used clause (used as an operand by both an EUF-owned and a LIA-owned node). The two sources are disjoint (owned vs. neutral), so no double-count and no miss; pure-QF_LIA stays empty (no EUF-owned node ⇒ no crossing edge ⇒ no both-used variable). Folded into §3.1; no longer an open question.

## 9. Completeness follow-ups (not this ADR)

- **`bool-compound-uf-args`** — precise handling of a Bool COMPOUND as a UF argument (`h(b∧c)`, `h(ite(...))`), currently degraded to `unknown` (§3.6). Option (a): Tseitin-define a bridge literal `ℓ ↔ (b∧c)` in clausification and feed `ℓ` through the leaf bridge, making the case precise. Deferred because it extends clausification for a corpus-rare shape; **trigger to pick it up = corpus/VC evidence of actual demand** (count degrade-to-unknown hits attributable to this shape on the corpus re-run).
