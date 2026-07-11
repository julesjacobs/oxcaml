# ADR-0006 — Unsat certificates + Lean replay (M5 pulled forward)

Status: **Accepted (design; implementation dispatches post-M4) 2026-07-11** —
adversarial review in `logs/adr-0006-adversarial-review.md`. Revision 3 (final;
design-only, no code in `main/`). Board #118. Waits, accepted, for post-M4 implementation dispatch (per its own
staging: format + OCaml checker + wrong-cert honeypots first). Pulls M5 forward per
DESIGN §2/§7/§12: certificates are *doubly strategic* — a tiny checker removes
per-compile trust from all search code (DESIGN §10 endgame), and replaying
certificates as Lean proof scripts converts the oracle from a `grind` **searcher**
into a **checker** (fast, deterministic, immune to grind incompleteness).

**Revision 4 (2026-07-11) — L6 erratum (docs-only clarification; no code, no
interface, no design change; master-approved):** the `Lia_farkas` leaf's multiplier
convention, disambiguated for **equality premises** (codex cross-model finding L6,
`../logs/codex-review/3086f73-lia.md`). The `{premises; farkas}` vector's
`Σ farkasᵢ · rowᵢ = k > 0` is well-defined for inequality premises but ambiguous for
an equality premise: LIA asserts `a = b` as *two* opposite-signed simplex bounds that
carry the **same** premise `Lit`, so `(token, multiplier ≥ 0)` alone does not say which
half-plane the multiplier scales, and a downstream checker cannot reconstruct the row.
Resolution (textbook Farkas-with-equalities):
- **`Le` premise:** nonnegative multiplier applied to the fixed `Le`-normal row
  `expr ≤ 0` (unchanged).
- **`Eq` premise:** a **free (signed) rational** multiplier applied to the canonical
  oriented row `(lhs − rhs)`; the sign selects orientation (positive → the `≤`-side,
  negative → the `≥`-side). The checker disambiguates by classifying the atom behind
  each premise `Lit` (`Eq` vs `Le`) via the engine's `Atom ⇄ Term` map; `Σᵢ farkasᵢ ·
  rowᵢ` must still cancel every variable to a strictly positive constant.

No frozen-type change: `Lia.conflict`'s `farkas : Rational.t list` already permits
negatives; `Simplex.build_conflict`'s nonneg-multiplier self-check is correct for its
internal oriented representation and stays; the **M5 cert-emitter** (not the M1
conflict struct) owns the translation from `(use_lower, |a|)` to the signed
externalized multiplier. `Explanation`/`Rule_tag`/`THEORY` seam unchanged and still
frozen. (Rejected alternative, equivalent in power but more verbose: an explicit
per-premise `Upper`/`Lower` orientation tag with all-nonneg multipliers.)

**M5 acceptance criterion (added by this erratum):** the LIA cert emitter emits, for a
conflict using one side of an equality, the signed multiplier matching the `use_lower`
bound actually used (contributions from both sides of one equality combine into a
single signed multiplier for that premise, or two entries); the OCaml checker and the
Lean replay classify equality-vs-inequality premises via the `Atom ⇄ Term` map and
verify the signed-row sum cancels to `k > 0`. Fold in **before** the LIA cert emitter
is implemented so the format is right the first time.

**Revision 3 (2026-07-11):** three format-spec precision fixes from the final review
section (DESIGN-APPROVED). **(1)** level-0 simplification resolution steps write
their hint chain as **`[units…, original]`** (synthesized outside 1UIP order;
original-first would false-reject under ordered-RUP). **(2)** an engine-`Split`
clause (B&B `x≤k ∨ x≥k+1`, N-O trichotomy) is a **kind-2 `Lia_farkas` theory lemma**
with an emitter-synthesized trivial `[1,1]`/constant witness — *not* a kind-1
resolution step (it is not RUP-derivable). **(3)** the cert-degrade counter is a
**first-class `uncertified` category in the gate accounting invariant** once
certificates replace `grind`, not merely a STATUS side-counter.

**Revision 2 (2026-07-11):** adversarial-review fixes
(`../logs/adr-0006-adversarial-review.md`, APPROVE-WITH-REQUIRED-CHANGES, 10 items,
all accepted). Load-bearing: **#1** level-0 simplification is now *certified*
(originals via `on_input`; each dropped literal is a resolution step against the
`on_unit` level-0 units) rather than trusted; **#2** `Lia_branch` dropped from the
format — M4 branching is engine-driven `Split` (ADR-0005 D5), so branch atoms are
tokened `Lit`s captured by the resolution skeleton and every LIA leaf is a plain
`Lia_farkas` (the standalone `solve_integer` B&B driver is test-only and
uncertified); **#3/#4** `decide` is the kernel-honest default, `native_decide` only
above a stated size threshold (adds compiler + the `ofReduceBool` axiom — stated),
and the explicit-multiplier Farkas route is *not* a TCB reduction (it still calls
`omega`); **#6** the checker is ordered resolution replay following `on_learned`'s
visit order; **#7** the OCaml checker carries its own checker-local bignum (off-core,
decoupled from #86); **#8** it replays the EUF proof *tree*, not premise sets; **#9**
Lean-replay workstreams are master-only-path (`tests/gate`) tasks and the `sat.mli`
additions ride the #114 Tranche-C event; **#10** certificates stream to disk with a
size cap that degrades to an uncertified run plus a loud count. The Rule_tag
deviation was confirmed by the reviewer to block no consumer — settled in both ADRs.

Cites: DESIGN §7 (certificate-shaped rule tags), §8 (oracle, caching), §10
(oracle-first, N-version, honeypots, TCB), §12 (risks 1/3); INVARIANTS I3
(firewall), I4 (justified inferences), I6 (determinism); ADR-0003 (frozen `Term`,
Farkas-ready `Le`-normal rows, preprocessing TCB / N2); ADR-0005 D3 (lazy
precedence-valid `explain` / CONTRACT-EX), D7 (`Rule_tag`, planned M5 payload
unfreeze), Freeze plan. Raw material: `smt/solver/sat.mli` trace hook
(`on_learned`), `theories/euf/euf.mli` (`explain`, proof forest, `Debug.self_check`),
`theories/lia/lia.mli` (`conflict = {premises; farkas}`, `integer_result`),
`tests/gate/NOTES.md` (grind findings, error-string-marker risk M5 retires).
Spec-by-citation: **LRAT** (Cruz-Filipe–Heule–Hunt–Kaufmann–Schneider-Kamp 2017,
clausal proofs with hints) [LRAT]; **DRAT** (Wetzler–Heule–Hunt 2014) [DRAT];
Nieuwenhuis–Oliveras proof-forest congruence certificates [NO07]; Farkas' lemma /
Dutertre–de Moura conflict rows [DdM06]; **Alethe/veriT** and **cvc5 LFSC** proof
formats (CDCL resolution skeleton + theory-lemma leaves) as the standard SMT-proof
shape [Alethe].

Lean shapes prototyped and **validated** under Lean 4.31.0 core (no Mathlib, no
grind): `../logs/adr-0006-lean-scratch/{euf,farkas,farkas2,resolution}.lean`.

---

## Context

The gate today is asymmetric and slow (DESIGN §8, §12 risk 3): `unsat` is certified
by `grind`, whose failure is *inconclusive* (incompleteness/timeout) and whose
speed forces the content-addressed cache and a nightly triage queue. Trust in the
solver's `unsat` verdicts rests, per-compile, on the entire search stack (SAT core,
EUF, LIA, combination). Certificates collapse both problems: the solver *emits a
proof*, an independent checker *verifies* it. The same artifact feeds two consumers
— a fast in-tree OCaml checker (per-compile) and Lean replay (kernel-grade, CI).

The raw material is already certificate-shaped, by DESIGN §7's foresight:

- **SAT** `sat.mli` has a zero-cost `trace.on_learned` firing `{id; clause;
  antecedents; btlevel}` per learned clause — the antecedent ids resolved in 1UIP.
  That is precisely an **LRAT hint chain**. One documented gap (M5-drat-level0,
  sat-review "most interesting finding"): learned *units* enqueue with
  `reason=None`, and `analyze` skips level-0 unit reasons, so the emitted chain is
  *modulo the level-0 unit closure*. Decision 1 (#1) **certifies** that closure
  rather than reconstructing it heuristically.
- **EUF** `explain`/`explain_implied` return the premise-token *set*; the
  congruence/transitivity *chain* is computed internally and re-verified by
  `Debug.self_check` (a fresh union-find replay). The chain is not yet *exposed*.
- **LIA** `conflict = {premises; farkas : Rational.t list}` already carries the
  Farkas multipliers — the certificate leaf verbatim, and the **only** LIA witness
  the format needs. Integer branching is **not** a theory sub-certificate: in the
  M4 architecture (ADR-0005 D5) B&B branches are engine-driven `Split`s whose branch
  bounds are ordinary tokened `Lit`s, so each closed branch ends in a `Lia_farkas`
  conflict citing that branch `Lit` as a premise, and the branching structure lives
  in the SAT resolution skeleton (see #2 below). The standalone `solve_integer`
  driver in `lia.mli` runs its *own* internal push/pop B&B; it is a **test-only**
  path and is uncertified (its `budget → Int_unknown` outcome is not even a verdict
  a certificate covers).

So v1 certificate work is mostly **exposing witnesses already computed**, plus a
format, checkers, and a Lean backend.

---

## Decision 1 — One format: a CDCL resolution skeleton with theory-lemma leaves — **FIRM**

The unified `unsat` certificate is an ordered list of **clause introductions** over
the *clausified* problem (theory atoms as opaque boolean vars, exactly what the SAT
core sees), ending in the empty clause. Two introduction kinds (the standard SMT
proof shape [Alethe], [LFSC]):

1. **Resolution step (LRAT):** a learned clause with its **antecedent hint chain**
   (`id`s of previously-introduced clauses whose reverse-unit-propagation yields
   it). Checked by RUP replay against the hints — no search.
2. **Theory-lemma step:** a clause `¬p₁ ∨ … ∨ ¬pₙ` (the negation of a theory
   conflict's premise set, ADR-0005 D7) tagged with a **theory sub-certificate**:
   - `Euf_chain` — a proof-forest path: nested `Assumed(atom) | Trans | Cong(f,…)`
     [NO07].
   - `Lia_farkas` — the `{premises; farkas}` vector [DdM06]: `Σ farkasᵢ ·
     (Leᵢ-normal row) = k > 0`, a variable-free positive constant. This is the
     **only** LIA leaf, and it covers two roles: (a) a genuine bound conflict, and
     (b) the **splitting disjunction itself**. When integrality forces a B&B split
     `x ≤ k ∨ x ≥ k+1` (or an N-O trichotomy `x=y ∨ x<y ∨ x>y`) the engine adds that
     clause via `Split` (ADR-0005 D5); it is a valid ℤ-tautology but is **not
     RUP-derivable** from the CNF, so it **cannot** be a kind-1 resolution step (#3).
     It is introduced as a **kind-2 `Lia_farkas` theory lemma** whose witness the
     emitter *synthesizes* — the trivial `[1,1]`/constant Farkas combination
     expressing the split's exhaustiveness over ℤ. Once the clause is in, each branch
     bound is a tokened `Lit` and each closed branch ends in an ordinary
     bound-conflict `Lia_farkas` citing that `Lit`; resolution over the branch
     literals then stitches the branches (kind 1). There is **no `Lia_branch`
     tree** — #2: modelling branching as a theory-internal tree would duplicate
     structure the SAT skeleton already captures and would diverge from the M4
     engine-`Split` architecture.
   - `Shared_eq` — an equality entailed in one child, replayed as the other's
     sub-certificate (combination, ADR-0005 D4).

**LRAT over DRAT — FIRM.** DRAT logs only clauses; the checker re-derives RUP
(re-implements unit propagation — more checker code, slower, and a second thing to
trust). LRAT logs the antecedent hints, which the `on_learned` hook **already
produces at no extra logging cost**, making the checker a near-trivial
hint-verifier. The one-time logging cost (the id list) is exactly the array the
hook hands us. Decision: **LRAT-style with hints.** (DRAT remains a provisional
fallback if hint volume ever dominates log size — it does not for VC-shaped inputs.)

**Level-0 simplification is CERTIFIED, not trusted (M5-drat-level0, #1) — FIRM.**
`add_clause` does level-0 tautology/duplicate/false-literal removal + unit
propagation, so the clause the SAT core *stores and reasons about* differs from the
clause the client *submitted*. Rather than trust that rewrite, we certify it — which
is exactly what LRAT is for:

- `on_input` records each **original** submitted clause with an id.
- `on_unit` records each **level-0 unit** with an id and the antecedents that forced
  it (an input unit's antecedent is its own `on_input` id; a derived unit's are the
  clause it reduced from plus the units that reduced it).
- The **stored (simplified) clause** is then introduced as a **resolution step**
  (kind 1): dropping a false literal `ℓ` is resolution of the original against the
  unit `(¬ℓ)`; duplicate removal and tautology elision are trivially sound and need
  no premise. **Hint order (#1):** these steps are synthesized at `add_clause` time,
  *outside* the 1UIP resolution order, so the emitter writes the hint chain as
  **`[units…, original]`** — the dropping units first, the original clause last. An
  original-first order would false-reject under the checker's ordered-RUP replay
  (Decision 4 #6), which expects each cited clause to be unit at its turn. So the
  hint chain for every learned clause bottoms out in real, id-bearing clauses — no
  implicit "modulo the level-0 closure."

This is strictly stronger than the Revision-1 "emit units as axioms" sketch and
removes the level-0 rewrite from the TCB. (Fallback, *not* taken: declaring the
simplification an explicit TCB item with a honeypot obligation — rejected because
certifying it is cheap and eliminates a trust surface. The concrete `sat.ml` hook
change is in Decision 2 / #5.)

**Theory sub-certificate types live OUTSIDE frozen `core` — FIRM, and a
coordination deviation from ADR-0005 D7 (flagged loudly, below).** D7 sketched the
M5 unfreeze as *fattening `Rule_tag` with payloads* (`Lia_farkas of coeffs`), which
drags LIA's `Rational.t` into `core` (I3 firewall) via a functor or a core rational,
**on the hot 1UIP path**. ADR-0006 proposes instead: **keep `Rule_tag` a
payload-free classifier (no unfreeze)**; the fine-grained witnesses are emitted
through *additive theory-side accessors* into a new `smt/certificate/` module that
sits **above** `core` and reads theory outputs. The search-loop currency
(`Explanation = premises + Rule_tag`, frozen at M1) is **unchanged** — no Rational
on the propagation path, no core unfreeze. This supersedes D7's "planned payload
unfreeze"; see the coordination note in Decision 2.

---

## Decision 2 — Emission is streaming at witness-time; additive API deltas — **FIRM**

**Why streaming, not lazy-at-end.** A theory conflict triggers backtracking, which
`pop`s the theory state that *is* the witness (the proof-forest edges, the simplex
row). So the sub-certificate must be captured **when the conflict/propagation is
explained**, before backtracking — exactly the streaming discipline `on_learned`
already uses. The engine logs each theory lemma's sub-certificate keyed to the
clause id the lemma becomes; the SAT resolution steps stream from `on_learned`. The
certificate log is assembled in derivation order and truncated to the empty-clause
cone at the end (unreferenced clauses dropped, like LRAT trimming).

**Additive-API-delta list (the freeze-coordination deliverable).** Every delta is
*additive* to the ADR-0005 M2/M3 seam (`THEORY` sig + `Atom`/`Lit` + `Explanation`),
or explicitly off it. Nothing here changes an existing frozen signature.

| module | existing | M5 additive delta | freeze status |
|---|---|---|---|
| `solver/sat.mli` | `trace.on_learned {id;clause;antecedents;btlevel}` | + `trace.on_input : id:int -> clause:lit array -> unit` (fires in `add_clause` recording the **original** submitted clause + id, *before* simplification, so the stored form is derivable from it); + `trace.on_unit : id:int -> lit:lit -> antecedents:int list -> unit` (fires wherever a level-0 unit is enqueued — the `reason=None` enqueue site in `add_clause`/level-0 propagation, #5 — carrying the forcing clause id and any prior unit ids); ensure learned **units** also fire `on_learned` citably. The engine emits the simplified stored clause as an ordinary resolution step against these units | `sat.mli` **not yet frozen**; **rides the #114 Tranche-C CDCL(T)-seam event** (sat-review item-8), no separate unfreeze — coordinated, already agreed (#9) |
| `theories/euf/euf.mli` | `explain : 'p t -> Term.t -> Term.t -> 'p list` | + `type 'p proof = Assumed of 'p \| Refl \| Trans of 'p proof * 'p proof \| Cong of Symbol.t * 'p proof list`; + `explain_proof : 'p t -> Term.t -> Term.t -> 'p proof` (structured form of the existing `Debug.self_check` replay) | additive; euf engine is pre-adapter (own `.mli`), not on the frozen seam |
| `theories/lia/lia.mli` | `conflict = {premises; farkas}` | **none required** for the format — the existing `{premises; farkas}` conflict is the whole LIA leaf. Branch bounds arrive as tokened `Lit` premises via engine `Split` (#2), so no `int_proof` tree and no `Int_unsat` payload change. (Only obligation: a branch-bound premise must appear in the conflict's `premises` as its `Lit` token — already true.) | no delta; `solve_integer` stays test-only/uncertified |
| `core/explanation.mli` | `Explanation = {premises; rule}`; `Rule_tag` payload-free | **NO CHANGE** (Decision 1 deviation: `Rule_tag` stays a classifier) | **stays FROZEN**; retires D7's planned unfreeze |
| `certificate/` (new) | — | new module: certificate AST, serializer/deserializer, OCaml checker (+ its own checker-local bignum, #7); depends on `core` only | new; not frozen initially, freezes once the format stabilizes |

**Coordination note to the freeze owner (ADR-0005 / #114):** ADR-0006 recommends
**not** taking D7's planned `Rule_tag` payload unfreeze. Update the ADR-0005 Freeze
plan row "`Rule_tag` … planned unfreeze M5" to "**payload-free, retained**;
certificates flow through `smt/certificate/` off-core." The adversarial reviewer
**confirmed this deviation blocks no consumer** (nothing downstream reads a
`Rule_tag` payload) — settled now in both ADRs; the seam is *more* stable under
ADR-0006, not less, a strict win for M2/M3.

**Sufficiency check of existing hooks.** SAT `on_learned` is sufficient for the
resolution skeleton *modulo* the two new hooks above (`on_input`/`on_unit`). EUF
`explain` returns the set but **not** the chain — the `explain_proof` delta is
required (the chain order is load-bearing for `Trans`/`Cong` replay). LIA `farkas`
is sufficient for every leaf; no further LIA delta is needed (#2).

**Growth policy — stream to disk, cap, degrade loudly (#10) — FIRM.** Certificates
can be large (dense SAT learns ~32k clauses, sat-review). They are **streamed to
disk** (`../logs`, per AGENTS.md digest-first) as emitted, never accumulated in
memory. A per-query **size cap** bounds the artifact; on exceed, the query
**degrades to an uncertified run** — the solver still returns its verdict, but no
certificate is produced and a **loud counter** increments. A degraded query is a
completeness/coverage signal, never a soundness one: it means "this verdict is not
certificate-backed," visible rather than silent.

**Uncertified is a first-class gate category, not a side-counter (#3, review
fix).** Once certificate replay *replaces* `grind` as the gate (Decision 5 step 3),
the gate's accounting invariant gains an explicit **`uncertified`** bucket: every
`unsat` verdict is exactly one of `certificate-checked` (OCaml checker green + Lean
replay green), `uncertified` (degraded per above, or emission not yet wired for that
fragment), or `refuted` (checker/replay rejected — ship-stopping). The count in each
bucket is part of the gate's audited state (STATUS.md outcome metrics, DESIGN §8/§11
— generated by CI, not an agent), so a rising `uncertified` count is a visible
coverage regression that cannot hide behind a green gate, exactly as
`grind`-inconclusive is tracked today. This upgrades the degrade counter from a
STATUS side-metric to a term in the correctness accounting.

---

## Decision 3 — Lean replay: reflected resolution checker + explicit theory terms, no grind — **FIRM (shapes validated)**

A certificate becomes a Lean artifact the **kernel checks**; `grind` is retired
from the replay path (the error-string-marker fragility in tests/gate/NOTES.md that
"replay retires" — outcomes stop depending on parsing grind's stderr). Three
independently validated shapes (scratch files run clean under Lean 4.31.0 core):

- **Resolution skeleton → reflected checker, `decide` by default.** Represent the
  CNF + hint chain as **data**, write the RUP/LRAT checker as a Lean *function*, and
  discharge `checkProof cnf proof = true`. One obligation for the *entire*
  propositional derivation vs O(#clauses) emitted theorems — far smaller, standard
  (this is how formally verified SAT checkers work). `resolution.lean` validates
  that clause evaluation and `decide` over clause/empty-clause data compose in core
  Lean. **Trust honesty (#3):** `decide` reduces in the **kernel** — genuinely
  kernel-checked. `native_decide` compiles the checker to native code and trusts its
  result via the **`Lean.ofReduceBool` axiom + the compiler**; it is *not*
  pure-kernel. So `decide` is the **default**; `native_decide` is used **only above a
  stated size threshold** (pin against real learned-clause counts, ~32k dense —
  open q2) where kernel reduction is too slow, and every query that used it is
  flagged as compiler-trusted in its outcome record.
- **EUF chain → explicit proof term.** `Cong(f,…)`→`congrArg`/`congr`,
  `Trans`→`Eq.trans`, `Assumed`→the hypothesis. `euf.lean` validates
  `a=b→b=c→c=d→f a=f d` as `congrArg f (Eq.trans h1 (Eq.trans h2 h3))` and the
  conflict form `… (hne : f a ≠ f d) : False := hne (…)` — **zero tactics, zero
  Mathlib.**
- **Farkas → core `omega` (default).** Validated routes (`farkas.lean` showed the
  naive `ring`/`add_nonpos` route needs Mathlib — a finding, not the plan;
  `farkas2.lean` is the plan):
  - **`omega`** (Lean *core*, not Mathlib) closes every LIA conflict directly,
    complete and deterministic, and emits a **kernel-checked** proof term (no extra
    axiom, unlike `native_decide`). Unlike `grind` it does not search-and-give-up: no
    inconclusive outcomes. This is the **default LIA leaf checker** and it is
    kernel-honest (#4).
  - **Explicit multipliers** — scale each `Leᵢ` row by `farkasᵢ`, sum, reduce Σ to
    the constant `k`, contradict `k ≤ 0` by `decide`, using a self-contained ~6-line
    Int prelude (`add_nonpos'`, `mul_nonpos'` from core `Int.add_le_add_right` /
    `Int.mul_le_mul_of_nonneg_left` / `Int.le_trans`). **Correction (#4): this route
    as prototyped is *not* omega-free** — `farkas2.lean` still calls `omega` to
    discharge the algebraic identity `Σ cᵢeᵢ = k` over the variable, so it is
    `omega` + a prelude, i.e. **strictly more** trust than plain `omega`, not less.
    It is therefore **not** a minimal-TCB alternative and is not adopted. A genuinely
    omega-free variant would require a hand-rolled ring-free linear normalizer for
    the `Σ = k` step; that is possible but unbuilt (**PROVISIONAL**, only worth it if
    a future audit wants a Farkas leaf depending on nothing but the bare kernel).
  - Since branching is captured by the resolution skeleton (#2), there is no B&B
    proof tree to replay — each branch's leaf is a plain `omega`-closed `Lia_farkas`.

The Lean goal per query is the **theory-lemma conjunction ∧ resolution skeleton →
False**; theory lemmas discharged by the above, the skeleton by the reflected
checker. No `grind`, so no incompleteness and no timeout triage for certified
queries.

---

## Decision 4 — Two consumers, one certificate: in-tree OCaml checker + Lean replay — **FIRM**

DESIGN §10's endgame is a tiny independent checker that removes per-compile trust
from search code. Realized as **the same certificate, two checkers**:

- **OCaml checker (`smt/certificate/check.ml`) — fast, per-compile.** Runs on every
  solve in debug/CI (like `Debug.self_check` today, but end-to-end). Verifies:
  - (i) the resolution skeleton by **ordered RUP replay (#6):** it walks clause
    introductions in emission order and, for each, unit-propagates the negated
    clause through **exactly the antecedent hints in `on_learned`'s recorded visit
    order**, checking that the empty clause falls out. This is **search-free**
    because that order *is* the 1UIP resolution order that produced the clause: the
    checker never has to *choose* which clause to resolve next (the defining
    difference between LRAT-with-hints and DRAT, which must search for the
    propagation order). A hint that fails to be unit at its turn is a rejected
    certificate.
  - (ii) each `Euf_chain` by replaying the **proof tree itself (#8)** — not the
    premise set — into a *fresh naive union-find*: every `Trans`/`Cong`/`Assumed`
    node is checked link-by-link (a `Cong(f,…)` requires its argument sub-proofs to
    have already established the argument equalities; a `Trans` requires its two
    endpoints to meet). So a **broken-link honeypot** (a chain whose premises happen
    to entail the goal but whose *structure* is wrong) dies **in-tree**, not only in
    Lean. (`Debug.self_check`'s replay is the reference algorithm; this is its
    structured, independent re-implementation, N-version.)
  - (iii) each `Lia_farkas` by summing the scaled `Le`-normal rows and checking the
    constant is positive, in **exact arithmetic via a checker-local bignum (#7)** —
    a small module *inside* `smt/certificate/`, off-core, with **no frozen-surface
    impact**, so the checker's arithmetic is decoupled from the core-wide #86 bignum
    question (a valid certificate may carry larger multipliers than the native-int
    search that found it, so the checker needs exact bigints regardless of when core
    gets them).
  - (no `Lia_branch` case — branching is resolution steps, checked by (i).)
- **Lean replay (nightly CI) — kernel-grade.** Decision 3. Slower, uncorrelated
  with the OCaml checker (different language, different author, kernel-rooted), so
  it is the true backstop; the OCaml checker is the fast per-compile line.

**Size budget & isolation (N-version, DESIGN §10):** the OCaml checker is its own
library `smt/certificate/`, **depending only on `core`** (`Term`, `Atom`/`Lit`) plus
its checker-local bignum and the certificate AST — **no `solver`, `euf`, `lia`, or
`interface` dependency**, and **no dependency on core's `Rational`** either
(build-enforced, like `Iarr_unsafe`'s `private_modules`, so any such dep is a compile
error). Budget: **≤ ~600 lines** (ordered RUP ~200, EUF tree replay ~150, Farkas +
checker-local bignum ~150, AST/serde ~100); a checker past ~800 lines is a smell
(DESIGN §10 tripwire). It is authored by a **fresh agent from the format spec only**,
no access to solver internals — the checker and the solver must not share blind spots.

---

## Decision 5 — Staging: format + checker + honeypots FIRST, then emission, then Lean — **FIRM**

Oracle-first (DESIGN §10): the checker is the oracle for emission, so it exists
before anything emits. Order:

1. **Format spec + OCaml checker + honeypots (serialization point).** Freeze the
   certificate AST + serializer; build the OCaml checker; build a **honeypot
   corpus of wrong certificates that must be rejected** — permuted Farkas
   multipliers, a broken `Trans`/`Cong` link (a structurally-wrong EUF tree whose
   premise set nonetheless entails the goal, #8), a resolution hint citing a
   non-unit, a level-0 resolution step dropping a literal no unit falsifies, a
   truncated skeleton not reaching the empty clause. A checker that hasn't proven it can
   go red on these is unaudited (DESIGN §10 honeypot doctrine; also the answer to
   "nothing gates the checker"). **This is the freeze/serialization point** — the
   analogue of the THEORY freeze — after which the rest parallelizes.
2. **Emission (parallel per module against the frozen format):** SAT `on_input`/
   `on_unit` hooks + level-0 resolution emission (rides the #114 Tranche-C
   `sat.mli` event, #9) ∥ EUF `explain_proof` ∥ LIA (no code delta — wire the
   existing `farkas` conflict into the certificate assembler). Each lands with its
   slice of the checker already green on hand-built certificates, then wired to real
   solves (every CI `unsat` emits and self-checks via the OCaml checker).
3. **Lean replay backend (parallel with 2 once format frozen) — MASTER-ONLY PATH.**
   The Lean encoder/replay lives under `tests/gate`, which is gate code and
   therefore **off-limits to child agents** (AGENTS.md "the gate is master-only";
   DESIGN §10). These workstreams — reflected resolution checker (Lean function,
   `decide` default / `native_decide` above threshold) ∥ EUF term emitter ∥ LIA
   `omega` emitter — are scheduled as master-owned tasks, not child tasks. They
   consume the identical certificate and replace the `grind` gate job; the cache
   (DESIGN §8) then stores certificates, replay is cheap, and the triage queue
   shrinks to genuine disagreements (DESIGN §12 risk 3).

Parallelizable: everything after step 1. Step 1 is a single serialized task (format
+ OCaml checker + honeypots must cohere). Step 2 is ~2 child workstreams (SAT hooks,
EUF `explain_proof`) plus a trivial LIA wire-up; step 3 is ~3 master-only-path
workstreams. The child/master split falls on the OCaml-vs-Lean consumer boundary.

---

## Trust story: what certificates do and do NOT remove

Certificates remove trust from **search** (SAT, EUF, LIA, combination): a checked
certificate means the clausified problem is genuinely unsat, independent of the
solver. They do **not** by themselves certify **preprocessing/clausification** —
Tseitin definitions, Int-`Ite` removal, `div`/`mod` elimination, purification, and
ADR-0003's gcd tightening (N2). The certificate is *about the clausified CNF + theory
atoms*, which is exactly what the existing Lean-from-dump oracle already trusts
(ADR-0003 N2: the dump is post-normalization, so oracle and solver see the same
tightened atoms). So certificates **match, not widen** today's TCB boundary; the
residual TCB (DESIGN §10) — smart constructors, preprocessing, the Lean encoder, VC
generation — is unchanged.

- **v1 (FIRM):** Tseitin/preprocessing **trusted-with-N-version-tests as today**
  (benchmark labels computed on the original atoms + round-trip + the N-version
  encoder). Certificates certify search only.
- **Provisional tightening (post-M5):** emit Tseitin definitional equivalences
  (`proxy ↔ boolean-combination`) as replayable `decide`/`Iff` lemmas so the Lean
  goal is about the *original* formula, shrinking the clausification TCB. Feasible
  (definitional extensions are cheap in Lean) but off the v1 critical path.

---

## Firm vs provisional

| # | Decision | Status |
|---|---|---|
| 1 | Unified format: LRAT resolution skeleton + theory-lemma leaves (EUF chain / `Lia_farkas` / shared-eq) | **FIRM** |
| 1 | LRAT (with hints) over DRAT | **FIRM** (DRAT fallback PROVISIONAL) |
| 1 | Level-0 simplification **certified** (originals via `on_input`; drops as resolution against `on_unit` units) | **FIRM** (#1; TCB-declaration fallback rejected) |
| 1 | No `Lia_branch` leaf — branching is engine `Split`, captured by the resolution skeleton; `solve_integer` test-only/uncertified | **FIRM** (#2, aligns ADR-0005 D5) |
| 1 | Theory sub-certs off-`core` in `smt/certificate/`; `Rule_tag` stays payload-free | **FIRM** (deviates from ADR-0005 D7 — reviewer confirmed blocks no consumer) |
| 2 | Streaming emission at witness-time; additive-only API deltas | **FIRM** |
| 2 | SAT delta rides the #114 Tranche-C CDCL(T)-seam event (no separate `sat.mli` unfreeze) | **FIRM** (#9) |
| 2 | Growth: stream to disk, size-cap, degrade to uncertified-run + loud count | **FIRM** (#10) |
| 2 | `uncertified` is a first-class gate-accounting category (not a STATUS side-counter) once certs replace grind | **FIRM** (#3, R3) |
| 1 | Engine-`Split` clause = kind-2 `Lia_farkas` lemma w/ synthesized `[1,1]` witness (not RUP-derivable, so not kind-1) | **FIRM** (#2, R3) |
| 1 | Level-0 resolution hint order is `[units…, original]` (synthesized outside 1UIP order) | **FIRM** (#1, R3) |
| 3 | Lean: reflected resolution checker, `decide` default / `native_decide` only above a size threshold (compiler + `ofReduceBool` axiom, stated) | **FIRM** (#3; shapes validated) |
| 3 | `omega` (core, kernel-honest) as default LIA leaf | **FIRM** (#4) |
| 3 | explicit-multiplier Farkas route — NOT omega-free as prototyped, not adopted; genuine omega-free variant needs a ring-free normalizer | PROVISIONAL (#4) |
| 4 | Two consumers (in-tree OCaml checker + Lean); ≤~600 lines, `core`-only + own checker-local bignum, N-version | **FIRM** (#7) |
| 4 | OCaml checker replays the EUF proof **tree** (broken-link honeypots die in-tree); ordered search-free RUP | **FIRM** (#6, #8) |
| 5 | Staging: format+OCaml-checker+honeypots → emission → Lean; Lean workstreams are master-only-path (`tests/gate`) | **FIRM** (#9) |
| — | Tseitin/preprocessing certification | v1 TRUSTED; PROVISIONAL post-M5 tightening |

---

## Open questions (for the format-freeze checkpoint)

1. **Certificate persistence & cache.** Store full certificates in the
   content-addressed cache (DESIGN §8), or store only the verdict + re-emit on
   demand? Certificates are larger than a verdict; weigh cache size vs re-solve cost.
   Leaning: store the trimmed certificate (enables Lean replay without re-solving).
2. **`native_decide` size threshold.** `decide` is the default (kernel-honest);
   `native_decide` is used only above a threshold (#3), where it adds the compiler +
   `ofReduceBool` axiom. Where is the threshold, in clauses/proof size? Pin against
   real learned-clause counts (~32k on dense SAT, sat-review) — measure kernel
   `decide` reduction time on the reflected checker at those sizes.
3. ~~Farkas coefficient overflow~~ — **resolved (#7):** the OCaml checker carries its
   own checker-local bignum, off-core, so it is correct regardless of the #86
   core-bignum timeline. (Still worth confirming the checker's bignum and the Lean
   `omega` agree on the constant sign on a large-multiplier corpus.)
4. **Combination (`Shared_eq`) sub-certificate shape.** Model-based N-O replays an
   entailed equality in the other child (ADR-0005 D4). Confirm the leaf is just
   "the other child's sub-certificate for the same equality" with no extra glue —
   pin against the M4 `Combine` implementation.
5. **Trimming aggressiveness.** LRAT backward trimming to the empty-clause cone vs
   emitting the full log — affects checker speed and log size. Set with the first
   real corpus.
