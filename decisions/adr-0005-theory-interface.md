# ADR-0005 — THEORY plugin interface

Status: **ACCEPTED (Tranche-A frozen) 2026-07-11.** Reviews + verification recorded
in `logs/adr-0005-adversarial-review.md` (FREEZE-APPROVED for Tranche A). The sanity
pass + freeze plan (tranche schedule) is the companion `adr-0005-freeze-plan.md`.
Every decision is tagged **FIRM** or **PROVISIONAL**.

**Revision 6 (2026-07-11, freeze ruling — supersedes Rev 5's `of_int`):** the
adversarial ruling (`logs/adr-0005-adversarial-review.md`) rejected a **public**
`of_int` as the Iarr-B1 mistake on a frozen file, with concrete harm traced: a
forged id misses the engine's atom⇄var map (or aliases a slot), a forged premise
`Lit` malforms 1UIP, and a hand-chosen id breaks the dense/monotonic invariant that
guarantees I6. Root cause: open-q1's "bare id type" conclusion was **incomplete** —
a bare id type cannot mint ids, so *something* must, and the safe minter is the
fix. `atom.mli` now exposes `type allocator` + `create_allocator` + `fresh`
(the CONTRACT-ATOM minter: "the next id", dense/monotonic/deterministic), **no**
`of_int`. `Lit.atom` unpacks its packed literal through a core-private
`Atom_unsafe.of_int` (dune `private_modules`; compile error outside `core` — the
Iarr_unsafe/B1 pattern). `FROZEN.sha256` re-hashed, `SPINE.md` regenerated.

**Revision 5 (2026-07-11, Tranche-A materialization):** building the freeze branch
surfaced that the approved `atom.mli` sketch exposed **no constructor**, so
`lit.ml` cannot implement `Lit.atom : t -> Atom.t` (it must rebuild an `Atom.t`
from the packed literal) and the M4 engine cannot allocate atoms — the files do not
build without it. Added **`val of_int : int -> t`** to `atom.mli` (CONTRACT-ATOM's
type-level mechanism: the engine allocator wraps a SAT-var id; not for plugin use).
This is a forced, minimal, additive correction to the frozen surface, flagged for
sign-off. Also folded the two adjudicated resolutions: CONTRACT-POISON is an
**engine** obligation (uniform catch-all; EUF needs no mechanism; LIA's flag is
defense-in-depth), and the **recursive-datatype** equality-split caveat is recorded
in the freeze plan (finite/enumerable tester split ✓; recursive `x=y` deferred to
the datatype author).

**Revision 4 (2026-07-11):** M1-end sanity pass (see the freeze package). The
interface survived unchanged; edits here are: added **CONTRACT-POISON** (theory
exception/poisoning discipline, from lia-review item 10); corrected the Rev-3
`sat.mli:53`→`:52` citation (level-0 tautology removal, re-verified on trunk); and
recast the freeze plan into freeze-after-validation **tranches** (A: now — vocabulary
+ env/rank/theory_view; B: M2 — model; C: M4 — sat.mli with the seam implemented).
Also folded in ADR-0006 coordination (no conflicts): `Rule_tag` stays **payload-free
permanently** (M5 cert witnesses route through the off-core `smt/certificate/`
module, not a payload — so `explanation.mli` has no planned unfreeze), and ADR-0006's
DRAT `trace.on_input`/`trace.on_unit` hooks ride the **same one-shot `sat.mli`
freeze** (Tranche C) as the theory seam.

**Revision 3 (2026-07-11):** verification-pass fixes (same review file,
Verification Pass §; 8/10 closed, 2 blockers + 3 minors). **B1:** the N-O split
encoding `[Eq; Not Eq]` was a no-op tautology (`A ∨ ¬A`) — discarded at add-time
by the MiniSat core (confirmed against `smt/solver/sat.mli:52`, level-0 tautology
removal) — so it never forced a branch; replaced with the ℤ **trichotomy**
`[Eq(x,y); lt(x,y); gt(x,y)]` in CONTRACT-SPLIT and D4. **B2:** the Split-term
**grammar** (atom | `Not(atom)`) is now explicit; compound Bool disjuncts route to
engine-side Tseitin, off the frozen type. Minors: (a) CONTRACT-EX failure wording
(back-edge first, unsoundness on cycle-closure); (c) user `(push)`/`(pop)` only at
decision level 0; (d) `model.mli`'s `value` left **unfrozen until M2**; (e)
`Atom.t` allocation restated as an engine obligation. New §M1-end sanity-pass
checklist records the SAT-core dependency.

**Revision 2 (2026-07-11):** incorporated the adversarial review
(APPROVE-WITH-REQUIRED-CHANGES, 10 changes, 2 HIGH). The two HIGH soundness fixes
are numbered interface contracts with inline counterexamples: **CONTRACT-EX
(explain precedence, §D3)** and **CONTRACT-SPLIT (Split = clausified disjunction,
§D5)**. The freeze surface freezes only the FIRM shape (§Freeze plan).

Cites: DESIGN §2 (staging), §5 (engine), §6 (theories + N-O), §7 (reasons), §8
(model self-check); INVARIANTS I3 (firewall), I4 (justified inferences), I6
(determinism); ADR-0003 (frozen `Term`/`Context`, `Theory_view`, Context
threading D6; process-global `Symbol.intern`). Spec-by-citation:
Nieuwenhuis–Oliveras–Tinelli 2006 (DPLL(T), theory propagation, lazy explanation,
explanation precedence) [NOT06]; Dutertre–de Moura 2006 (simplex
Assert/Check/Backtrack, δ-rationals, Farkas) [DdM06]; de Moura–Bjørner
model-based theory combination [MBC]; Conchon–Contejean–Kanig–Lescuyer, "CC(X):
Semantic Combination of Congruence Closure with Solvable Theories" (Alt-Ergo's
functorized combination) [CCX].

---

## Context

M1 delivers the clausifier + CDCL SAT core, propositional, seeing theories only
through a callback interface (DESIGN §5). This ADR freezes that interface: the
signature EUF and LIA implement (M2/M3, in parallel), the currency they speak,
and the services the engine owns.

Two structural facts drive the design:

1. **The DAG forbids `theories → solver`** (`solver`/`euf`/`lia` are siblings
   over `core`). So the shared vocabulary — literal type, explanation type, the
   `THEORY` signature — **must live in `core`**, alongside `Theory_view`, and is
   what the M1 freeze *adds* to `FROZEN.sha256` (AGENTS.md lists
   `env`/`rank`/`theory_view` as freezing here; this ADR extends that set — see
   §Freeze plan for exactly what).
2. **`Term.t` is single-`Context` and unbranded** (ADR-0003 / R3). An interface
   flinging raw `Term.t` across every boundary multiplies the R3 hazard; the
   answer (D2/D6) makes the per-assertion currency a dense engine-assigned
   **`Atom.t`/`Lit.t`** — a theory sees a `Term.t` only at `register_atom` and
   two other narrow boundaries (D6 #9).

The interface is deliberately the NOT06/DdM06 "assert / check / explain /
backtrack" shape, so M2/M3 crib from reference implementations (DESIGN §10).

---

## Decision 1 — One `THEORY` module type; combination is itself a `THEORY` — **FIRM**

**Options.** (a) Engine holds a list of theories and runs N-O itself. (b) A
single `THEORY` signature; EUF and LIA each implement it; combination is a
functor `Combine (A : THEORY) (B : THEORY) : THEORY` running Nelson–Oppen between
them and presenting *one* `THEORY` to the engine.

**Decision: (b).** The engine (M1) is written against exactly one `THEORY`. EUF
(M2) and LIA (M3) implement `THEORY` independently — the parallel workstreams
DESIGN §6 wants. Combination (M4) is a `THEORY`-to-`THEORY` composition, so N-O is
a theory-side obligation (Decision 4), not engine machinery.

**Novelty check (DESIGN §10 novelty-free rule).** The *algorithm* is standard
model-based N-O [MBC] — un-novel. The *functorized packaging* (`Combine` presents
one `THEORY`) is not the engine-integrated shape of Z3/cvc5, but it is exactly
Alt-Ergo's functorized combination [CCX] (`CC(X)` parameterizes congruence
closure over a solvable-theory functor argument). So this is **cited prior art,
not novelty**: functor packaging à la Alt-Ergo, engine-observable semantics
identical to Z3's MBC. **Consequence:** the combinator holds `A.t * B.t` +
shared-term bookkeeping; shared equalities are ordinary `Eq` atoms routed to both
children; `Combine.model` merges child models (D4).

---

## Decision 2 — Assertion currency: `Atom.t`/`Lit.t`, not `Term.t` — **FIRM**

Atoms are asserted **as signed literals referenced by a dense id**, not as
`Term.t`. The clausifier assigns each theory atom an `Atom.t` (1:1 with the
atom's SAT variable) and holds the `Atom ⇄ Term` / `Atom ⇄ Var` maps. A theory
learns the `Term.t` behind an atom **once**, at `register_atom`, and thereafter
every assertion / propagation / conflict names `Atom.t`/`Lit.t` only.

**Subterm indexing (review attack-2 clarification — load-bearing, not a hole).**
EUF congruence needs *subterms* (e.g. `f(x)` inside `Eq(f x, a)`), but only whole
*atoms* are registered. This is sufficient and is contractually spelled out:

- **CONTRACT-REG-1.** All non-atom subterms enter a theory **only** by walking the
  atom `Term.t` handed to `register_atom` (deep `private`/`Theory_view` access).
  There is no other term-firehose.
- **CONTRACT-REG-2.** A theory keys its internal subterm indices on `Term.t`
  *tags* (O(1), ADR-0003), while the SAT-literal currency for the atom is
  `Atom.t`. `register_atom` hands both precisely so the theory can bridge them.

Worked EUF trace (from the review): registering `Eq(f x,a)`, `Eq(x,y)`,
`Eq(f y,a)` mines `f(x),x,a,y,f(y)`; asserting `+Eq(f x,a) +Eq(x,y) −Eq(f y,a)`
then `check Propagate` merges `f(x)~a, x~y`, congruence gives `f(x)~f(y)` hence
`f(y)~a`, contradicting `f(y)≠a` → `Conflict {premises=[Eq(f x,a); Eq(x,y);
¬Eq(f y,a)]; rule=Euf_congruence}`, learned clause
`¬Eq(f x,a) ∨ ¬Eq(x,y) ∨ Eq(f y,a)`. The interface delivers everything EUF needs.

**Polarity.** Atoms are asserted **with polarity** (`Lit = Atom × sign`); `¬(t≤0)`
is the LIA fact `1−t≤0` (ADR-0003 D3), the negation rewrite done internally at
`assert_lit`. `Lit.t` packs atom+sign (MiniSat low-bit).

---

## Decision 3 — `check(effort)` drives propagation; explanations are lazy + precedence-valid — **FIRM**

`assert_lit` is side-effect-only (cheap incremental state update). All
consistency and propagation happen in `check th effort`:

- `effort = Propagate` — cheap, in-search: propagation + fast inconsistency.
  Returns `Propagations ls` or `Conflict`. **Never `Sat`/`Split`** (debug-asserted
  contract violation if it does).
- `effort = Final` — SAT core has a *full* boolean model; the theory must be
  **complete**: LIA runs branch-and-bound for integrality; the combinator runs
  model-based N-O. May additionally return `Sat` or `Split` (D5).

**Lazy explanations (DESIGN §7, NOT06).** Propagation returns bare `Lit.t`s;
`explain th lit` reconstructs the premise set from theory state (EUF proof
forest, LIA Farkas witness) only when 1UIP analysis needs it. Rejected: eager
`(Lit.t × Explanation.t)` per propagation — most propagations never enter a
conflict (NOT06 §4, universal practice).

> **CONTRACT-EX (explanation precedence — HIGH, review #1).** For a literal `l`
> that this theory propagated, `explain th l` MUST return a premise set every
> element of which was assigned on the trail **strictly before `l`** (the reason
> valid *at `l`'s propagation time*), not merely "some currently-asserted set."
>
> *Why (violating counterexample).* EUF propagates `a=c` at level 2 from `a=b`
> (lvl 1) and `b=c` (lvl 2). Search descends; at level 4 `d=a` and `d=c` are
> asserted, opening a *second* path `a=c` via `d`. If `explain(a=c)` returns the
> newer `{d=a, d=c}` (both "still asserted") instead of `{a=b, b=c}`, the reason
> clause `¬d=a ∨ ¬d=c ∨ a=c` is not a valid implication *at the point `a=c` was
> derived*. The **immediate** failure is an implication-graph **back-edge** from
> level-4 literals into a level-2 node → a malformed 1UIP traversal
> (non-terminating analysis); **unsoundness** follows once such a back-edge closes
> a cycle (a learned clause that is not a genuine consequence). EUF's proof forest
> and LIA's Farkas witness both *can* produce precedence-respecting reasons — but
> the frozen contract, which under zero human review *is* the spec, must *require*
> it.

Lifetime corollary: `explain l` is valid only while `l` and its (earlier)
premises are still asserted; conflict analysis runs before backtracking, so this
holds. `explain` self-verifies its certificate in debug builds (§7).

---

## Decision 4 — Nelson–Oppen: model-based, inside the combinator — **FIRM**

Purification (DESIGN §6, a *preprocessing* pass — not the functor's job) splits
mixed terms before atoms reach the theories: `f(x+1)` → `f(t1)` (pure EUF) +
`t1=x+1` (pure LIA), `t1` shared. The combinator tracks, per registered `Term.t`,
which children reference it (walking the atom term at `register_atom`); **shared =
referenced by ≥2**.

LIA over ℤ is non-convex, so we do **not** enumerate entailed shared equalities;
we use **model-based combination** [MBC].

**MBC model interface (review #6 — load-bearing, was unstated).** For an
Int-sorted *shared* variable, EUF's `model` returns `Uninterp <class-id>` (EUF
computes no integer values) while LIA returns `Int n`. The disagreement check is
therefore **not a raw value comparison**; it compares the **induced equality
relation** on shared pairs:

- for each shared pair `(x,y)`: does child A place them equal (same EUF class /
  equal `Int`)? does child B? If the two children's induced relations disagree on
  `(x,y)`, the combinator returns the **ℤ trichotomy** `Split [Eq(x,y); lt(x,y);
  gt(x,y)]` (a forcing disjunction over three distinct atoms, per CONTRACT-SPLIT —
  *not* `[Eq; Not Eq]`, which is a no-op tautology, B1) so the SAT core resolves
  it. No disagreement on any shared pair ⇒ the combined model is consistent.
- **`Combine.model` merges child models:** `Int`/`Bool` from the arithmetic/bool
  child, `Uninterp` witnesses from EUF, keyed by term.

**Model validity (review #4 — tightened).** See CONTRACT-MODEL in D-model below:
a *complete, integer-valued, N-O-agreed* model exists only after a child's
`check Final` returns `Sat`; a `Propagate`-consistent LIA check establishes only
**rational** (δ-)feasibility [DdM06]. So the combinator reads child `model` only
after each child's `Final`→`Sat`, and the δ-rational→integer extraction is a named
step LIA owes (D-model).

**Consequence for a plugin:** the only combination obligations are (i) accept
asserted equalities (ordinary `Equality` atoms) and (ii) expose a correct `model`
after `Final`→`Sat`. Splitting is an engine service (D5). **Provisional:** an
optional `interface_eqs` path (convex EUF can cheaply report entailed shared
equalities) is a later optimization, not frozen.

---

## Decision 5 — The "demand new literal" seam; `Split` semantics; termination — **FIRM**

Mid-solve a theory needs atoms not yet SAT variables: LIA B&B branches, N-O shared
equalities, and (stage 2) E-matching instances. Return-based (no engine callbacks
mid-`check`), for deterministic replay (I6, DESIGN §10): `check Final` returns
`Split terms` (fresh `Term.t`s built through the session `Context`, D6).

> **CONTRACT-SPLIT (Split = clausify-and-assert-disjunction — HIGH, review #2).**
> `Split terms` means: **clausify each term to a `Lit.t` — dedup atoms via the
> `Atom ⇄ Term` map, internalizing genuinely new atoms (fresh `Atom.t`+SAT `Var`)
> and mapping a leading `Not` to the *negative polarity* of the existing atom —
> then assert the *disjunction* of those literals as one clause.** It does NOT
> mean "assert each term as a unit fact."
>
> *Why (violating counterexample), and why the disjunction must have ≥2 DISTINCT
> atoms.* N-O disagreement on `x=y`. (i) Reading `Split [Eq(x,y)]` as "assert the
> unit clause `Eq(x,y)`" **forces `x=y`** — unsound (a model may need `x≠y`). (ii)
> Reading it as "add nothing" leaves the disagreement unresolved, so `Final`
> wrongly reports **`Sat`** — incomplete. (iii) The *one-atom tautology*
> `Split [Eq(x,y); Not(Eq(x,y))]` → `A ∨ ¬A` is **also wrong**: it is a
> propositional tautology, discarded at add-time by the MiniSat-style core
> (`smt/solver/sat.mli:52`, level-0 tautology removal), so it never forces the SAT
> core to assign `A` and the disagreement again survives to a spurious `Sat`.
>
> The correct v1 encoding is the **ℤ trichotomy over three DISTINCT atoms**:
> `Split [Eq(x,y); lt(x,y); gt(x,y)]` → `A_eq ∨ A_lt ∨ A_gt`, a genuine forcing
> disjunction valid over ℤ (shared QF_UFLIA vars are Int-sorted, so it is always
> available). B&B analogously emits `Split [ (x≤k) ; (x≥k+1) ]` — two distinct new
> atoms. (A same-atom polarity case-split would need a *decision*, not a clause;
> not needed in v1.) **General rule: a `Split` disjunction resolves a disagreement
> only if it forces a fresh choice among ≥2 distinct atoms.**

> **CONTRACT-SPLIT-GRAMMAR (v1 Split-term grammar — review B2).** In v1 each term
> in `Split terms` is an **atom or `Not(atom)`** — the clausify mechanism above
> ("dedup atoms; leading `Not` → negative polarity") covers exactly this. A
> compound Bool disjunct (e.g. `(or A (and B C))`, which a stage-2 E-matching
> instance can produce) needs **full Tseitin sub-clausification** (a proxy atom +
> defining clauses) — an *engine-side additive generalization* that runs the
> `Split` terms through the clausifier. That generalization is **off** the frozen
> `check_result` type (the type is still `Term.t list`), so `check_result` stays
> FIRM; only the engine's internalizer grows. This substantiates "stage 2 not
> boxed out": a ground instance is a `Split` disjunction, literal in v1, Tseitin'd
> when compound at M6.

This also makes the earlier provisional `Lemma of Lit.t list` a mere convenience
alias (a disjunction whose atoms already exist), so **`check_result` is FIRM** —
no separate `Lemma` constructor is needed.

Flow: `Split terms` → engine clausifies + asserts the disjunction (internalizing
new atoms) → engine calls `register_atom` for each new atom on every child that
owns it → search continues.

> **CONTRACT-SPLIT-TERM (termination — review #3).** The `Split → internalize →
> re-check` loop has no intrinsic bound. N-O splits terminate (finitely many
> shared pairs, monotonic). **LIA branch-and-bound need not** (classic divergence;
> Gomory cuts deferred, DESIGN §6). v1 rule: the **engine imposes a split/decision
> budget**; on exhaustion the query returns **`unknown`** (a sound incomplete
> answer, DESIGN §1 sat-handling). Gomory cuts are the named completeness fallback
> (DESIGN §6) if real VCs hit the budget. The budget lives in the engine, not the
> theory, and is deterministic (fixed threshold, I6).

---

## Decision 6 — Context threading & lifecycle; push/pop frames; the brand — **FIRM (threading, frames) / RECOMMENDATION (brand)**

**Who owns the `Context`.** One session `Context` (ADR-0003 D6), created by the
engine, threaded into every theory at `create ctx env`. All mid-solve terms a
theory builds (Split disjuncts) go through `ctx`, sharing the tag stream and
hash-consing (I6). The engine owns `Atom` allocation and the `Atom ⇄ Term` map
(open question 1 resolved: allocator is engine-side, so `atom.mli` stays a bare
id type — §Freeze plan). The model evaluator / printer / Lean encoder need no
`Context` (ADR-0003 D6).

> **CONTRACT-ATOM (engine obligation).** The engine holds one `Atom.allocator`
> and calls `Atom.fresh` once per theory atom, pairing each result **1:1 with the
> atom's SAT variable** (I6). `fresh` is the sole minter (Rev 6): it hands out the
> next dense id (0, 1, 2, …), so distinct atoms get distinct ids and a fixed
> clausification order yields identical ids across runs. There is no public
> id-forging constructor — a forged id would miss the atom⇄var map, and a
> hand-chosen id would break the dense/monotonic invariant. `Lit` unpacks its
> packed literal through the core-private `Atom_unsafe.of_int` (Iarr_unsafe/B1
> pattern; invisible outside `core`).

**Push/pop: a unified frame stack, both frame kinds — FIRM (review #5).** A
*frame* is a backtrack checkpoint. `push` opens one; `pop n` undoes the last `n`,
restoring theory state to that checkpoint. A frame is opened at **both**:

1. each **SAT decision level** during search (the classic DdM06 `Backtrack` /
   NOT06 trail-synchronized undo), and
2. each **user assertion frame** — SMT-LIB `(push)` — whose assertions must
   survive `check-sat` and be *retracted* on `(pop)` (DESIGN §5 makes push/pop
   *and* assert-after-check a **day-one** requirement).

The theory does not distinguish the kinds; it only checkpoints on `push` and
restores on `pop`. **Assert-after-check** is then first-class: after a `check`,
the engine `pop`s search scopes back to the current user frame, `assert_lit`s new
atoms there, and re-`check`s — no full reset. Framing push/pop as
"decision-level-only" (rejected) would let an M2/M3 implementer break incremental
benchmarks; the contract explicitly forbids that.

**Frame-interleaving invariant (review minor c).** The stack is LIFO, and search
frames always sit *above* the current user frame, so a user `(push)`/`(pop)` is
honored **only at decision level 0**: the engine first unwinds all search
(decision) frames back to level 0, then opens/closes the user assertion frame.
This keeps `pop n` frame-counting unambiguous — a `pop` never has to skip past a
live decision frame to reach a user frame.

**The per-Context brand (#24 / M1-brand-checkpoint) — RECOMMENDATION: do NOT
unfreeze `Term`/`Context` for a brand at M1 end.** The pressure (R3): the
single-`Context` contract is convention-only; mixing terms across contexts
silently corrupts `Term.equal`/`Set`/`Map`. Mitigation already in the design:
D2's `Atom.t` currency means a theory touches a `Term.t` only at narrow
boundaries, all from the one session `Context`; there is no second `Context` in a
session. Cost of a real brand: a **phantom type** (`'brand Term.t`) gives
compile-time safety but functorizes `Context`/`Theory_view`/`THEORY` over the
brand — a large invasive unfreeze (`term.mli`, `context.mli`, `FROZEN.sha256`,
`SPINE.md` + review), fighting OCaml ergonomics; a **runtime context-id field**
still changes the frozen record and adds a hot-path branch. Recommendation: keep
both frozen; add a **non-freeze** engine-side mitigation instead —

> **CONTRACT-CTX (review #9).** The engine tags its `Context` with an id and, in
> debug builds, asserts that id on **every** `Term.t` crossing the interface —
> `register_atom`'s term, each `Split`-returned term, **and the `Model.value`
> term argument** (a wrong-context term there silently mis-keys) — not only
> `register_atom`.

Revisit only if M2/M3 surfaces a concrete cross-context bug. Input to the
M1-brand-checkpoint (#24), which owns the final call.

---

## Decision 7 — Explanations: premise `Lit` set + `Rule_tag` — **FIRM (v1 shape) / planned unfreeze (payloads, M5)**

`Explanation.t = { premises : Lit.t list; rule : Rule_tag.t }` (I4, DESIGN §7). A
**premise is an asserted theory literal** on the trail (precedence-valid per
CONTRACT-EX); the premises' conjunction T-entails the fact (is T-unsat for a
conflict). The engine resolves premises against the trail for 1UIP +
selector-based unsat cores (§7) and turns a conflict into `¬l₁ ∨ … ∨ ¬lₙ`.

**Initial rule-tag set (FIRM enum; payloads deferred):**

| tag | producer | premise set is… | certificate (theory-internal, self-checked) |
|---|---|---|---|
| `Trivial` | any | ∅ / folded constant | — |
| `Euf_congruence` | EUF | eqs/diseq on the conflicting proof-forest path | congruence/transitivity chain [NOT06] |
| `Lia_bound` | LIA | bounds implying a propagated bound | simplex row |
| `Lia_farkas` | LIA | bounds of an infeasible row | Farkas coefficients [DdM06] |
| `Lia_branch` | LIA | branch literal + justifying bounds | B&B case split |
| `Shared_eq` | Combine | eq entailed in one child, replayed in the other | child certificate |

**How both theories fit one currency:** the `Rule_tag` classifies; the premise
set is the shared currency both produce; the fine-grained certificate
(proof-forest path, Farkas vector) is what the theory uses *internally* to compute
that set and **self-check it** at `explain` time (§7). So the certificate need not
appear in `core`'s `Explanation` in v1 — keeping LIA's rational type out of the
firewalled `core` (I3).

**`Rule_tag` is payload-free PERMANENTLY (ADR-0006 delta, supersedes the Rev-3 "M5
payload unfreeze").** The Rev-3 plan floated adding payloads (`Lia_farkas of
coeffs`, …) under an M5 unfreeze. ADR-0006 removes that: M5 certificate witnesses
(Farkas vectors, congruence chains) live in a new **off-core `smt/certificate/`
module**, never a `Rule_tag` payload — precisely to keep LIA's `Rational` off the
frozen 1UIP path (the same I3 reason this ADR kept certificates theory-internal).
Consequence: `explanation.mli` has **no planned unfreeze at all** — strictly more
stable. (Adding *new tag constructors* for a future theory, e.g. `Datatype_*`, is a
separate additive enum unfreeze, orthogonal to the no-payload decision.)

---

## Proposed `.mli` (verbatim; new `core` modules)

```ocaml
(* ─── core/atom.mli ─── engine-assigned theory-atom id; the assertion currency.
   Minted ONLY by [fresh] (Rev 6): no public id-forging constructor. A core-private
   [Atom_unsafe.of_int] (dune private_modules) lets [Lit] unpack a packed literal
   inside core — the Iarr_unsafe/B1 pattern. *)
module Atom : sig
  type t = private int                       (* dense; 1:1 with the atom's SAT var *)
  type allocator                             (* the engine holds one per session *)
  val create_allocator : unit -> allocator
  val fresh   : allocator -> t               (* the NEXT id (dense, monotonic, deterministic — CONTRACT-ATOM/I6); sole minter *)
  val equal   : t -> t -> bool
  val compare : t -> t -> int                (* by id; total, deterministic *)
  val hash    : t -> int
  module Set   : Set.S     with type elt = t
  module Map   : Map.S     with type key = t
  module Table : Hashtbl.S with type key = t
end

(* ─── core/lit.mli ─── a signed theory literal (atom + polarity), packed. *)
module Lit : sig
  type t = private int
  val make    : Atom.t -> bool -> t          (* sign: true = positive *)
  val atom    : t -> Atom.t
  val sign    : t -> bool
  val negate  : t -> t
  val equal   : t -> t -> bool
  val compare : t -> t -> int
  val hash    : t -> int
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

(* ─── core/explanation.mli ─── the uniform reason currency (§7, I4). *)
module Rule_tag : sig
  (* FIRM enum; payload-free PERMANENTLY (ADR-0006): M5 cert witnesses live in the
     off-core smt/certificate/ module, never here (keeps Rational off frozen core). *)
  type t =
    | Trivial
    | Euf_congruence
    | Lia_bound
    | Lia_farkas
    | Lia_branch
    | Shared_eq
end

module Explanation : sig
  type t =
    { premises : Lit.t list                  (* precedence-valid (CONTRACT-EX); det. order (C2) *)
    ; rule     : Rule_tag.t
    }
end

(* ─── core/model.mli ─── candidate assignment: sat self-check (§8) + N-O (§6).
   [Model.t] is FROZEN at M1; the [value] VARIANT is deliberately left UNFROZEN
   until M2 (open q3: Uninterp encoding), to avoid a freeze-then-unfreeze on the
   very next milestone (review minor d). *)
module Model : sig
  type value =                 (* NOT hash-frozen at M1; frozen at M2 with open q3 *)
    | Int      of int
    | Bool     of bool
    | Uninterp of int          (* opaque class id per uninterp term; encoding pinned at M2 *)
  type t
  (* Total over asserted terms once produced after [Final]->[Sat] (CONTRACT-MODEL);
     [None] only for a term the model does not constrain. *)
  val value : t -> Term.t -> value option
end

(* ─── core/theory.mli ─── the frozen plugin signature (EUF M2, LIA M3). *)
type effort =
  | Propagate                                (* cheap, in-search; never returns Sat/Split *)
  | Final                                    (* full boolean model; complete check *)

type check_result =                          (* FIRM (Split generalized per CONTRACT-SPLIT) *)
  | Sat                                       (* Final only: assignment is T-satisfiable *)
  | Propagations of Lit.t list                (* consistent; T-implied literals (lazy explain) *)
  | Conflict     of Explanation.t             (* asserted set is T-inconsistent *)
  | Split        of Term.t list               (* Final only: clausify to a DISJUNCTION and
                                                 assert it as one clause (CONTRACT-SPLIT):
                                                 B&B branch, N-O case-split, E-matching lemma *)

module type THEORY = sig
  type t

  (** [create ctx env]: empty state bound to the session Context (ADR-0003 D6). *)
  val create : Context.t -> Env.t -> t

  (** Sole point a theory receives a [Term.t] (CONTRACT-REG-1/2, D2). Called as the
      clausifier internalizes each theory atom and for atoms minted from a [Split].
      The theory walks the term for subterms and indexes them by [Term.t] tag;
      builds its structure (EUF: e-graph; LIA: bound/row). Idempotent (C7). *)
  val register_atom : t -> Atom.t -> Term.t -> unit

  (** Assert a signed literal (its atom is registered). Cheap incremental update;
      no output. Asserted in the current frame (see {!push}/{!pop}). *)
  val assert_lit : t -> Lit.t -> unit

  (** Theory reasoning over the asserted set; see {!check_result}. Propagations in
      deterministic order (C1). [Sat]/[Split] only at [Final]; returning either at
      [Propagate] is a debug-asserted contract violation. *)
  val check : t -> effort -> check_result

  (** Premises+tag for a literal THIS theory propagated. Lazy but always available
      (§7). CONTRACT-EX: premises are the reason valid at [lit]'s propagation time
      — all assigned strictly BEFORE [lit] on the trail. Deterministic (C2);
      self-verifies its certificate in debug builds (§7). *)
  val explain : t -> Lit.t -> Explanation.t

  (** [push]/[pop n]: a unified backtrack-frame stack (D6). A frame is opened at
      each SAT decision level AND each user assertion frame ((push)); [pop n]
      restores state to n frames back. This is what makes assert-after-check and
      incremental (push)/(pop) first-class (DESIGN §5). *)
  val push : t -> unit
  val pop  : t -> int -> unit

  (** CONTRACT-MODEL: a complete, integer-valued (LIA), N-O-agreed model — valid
      ONLY after this theory's most recent [check Final] returned [Sat]. After a
      [Propagate]-consistent check the internal candidate may be a δ-rational /
      partial assignment (DdM06) and MUST NOT be read as a model (debug-asserted).
      LIA performs the δ-rational -> integer extraction before returning [Sat].
      Used by [Combine] for MBC (D4) and by the §8 sat evaluator. *)
  val model : t -> Model.t
end
```

Engine loop the interface implies (contract for M1/M2/M3, not frozen code):

```
after SAT propagation reaches a fixpoint (level d):
  assert_lit th l          for each newly-assigned theory literal l (in the current frame)
  loop: match check th Propagate with
        | Conflict e      -> conflict analysis (learn ¬premises via CONTRACT-EX reasons); backtrack
        | Propagations [] -> break
        | Propagations ls -> assign ls as theory-implied (explain on demand); loop
at a full boolean model:
  match check th Final with
        | Sat          -> SAT; emit (model th) to the §8 evaluator
        | Conflict e   -> conflict analysis
        | Propagations ls -> assign; continue
        | Split terms  -> clausify terms to a disjunction (CONTRACT-SPLIT), internalize new
                          atoms, register_atom them, assert the clause; if the split budget
                          is exhausted (CONTRACT-SPLIT-TERM) answer `unknown`; else continue
```

---

## Determinism obligations on plugins (I6) — numbered contracts + enforcement

**Enforcement (review attack-7 reframe).** These are **not** proven "by review."
The mechanical enforcers are (i) the I6 run-twice determinism regression
(identical decisions/verdict/counters, DESIGN §8), (ii) targeted tests (C2: call
`explain` twice; C7: register twice), and (iii) cheap debug assertions (C6: every
returned `Lit` has a registered atom; C8: no `Stdlib.compare`/`Hashtbl`-order in
observable paths). Review checks the tests exist and the ordering discipline
holds; the harness proves determinism.

- **C1** `check` returns `Propagations` in deterministic order (registration/trail
  order — never `Hashtbl` traversal order).
- **C2** `explain lit` returns the same (precedence-valid, CONTRACT-EX) premise
  set, same order, every call in a given state.
- **C3** With several conflicts available, the returned one is chosen
  deterministically (fixed scan order).
- **C4** `Split` selects its disjuncts deterministically (fixed variable order for
  B&B; fixed shared-pair order for N-O).
- **C5** No wall-clock, unseeded PRNG, or address-order in any decision (I6).
- **C6** A theory reasons only about atoms it was `register_atom`-ed with; it never
  fabricates a `Lit.t` for an unregistered atom.
- **C7** `register_atom` is idempotent; re-registering perturbs neither state nor
  ids.
- **C8 (review #7 — the ADR-0003 global-symbol-intern footgun).** `Symbol.t` is a
  *process-global* int in first-encounter order (SPINE `symbol.mli` deviation), so
  a map **keyed on `Symbol.t`** whose iteration reaches observable output is
  **nondeterministic across runs that process queries in a different order**. A
  theory MUST therefore iterate such structures **ordered by symbol *name***
  (`Symbol.name`), never by the raw id and never by `Hashtbl` traversal. (Term
  tags are per-`Context` and the cache key is gate-side canonical, so verdicts are
  otherwise safe; this is the one exposed path.) More generally: order internal
  sets/maps by `Term.compare` (tag) or `Symbol.name`, never `Stdlib.compare` on
  abstract values.

---

## Freeze plan — tranches (Rev 4; freeze package §4a)

The freeze protects the **M2/M3 → M4 seam**. Rev 4 recasts the plan into
**freeze-after-validation tranches**: freeze each file at the milestone that
validates its shape; the ADR *document* is Accepted now, so every tranche builds to
a fixed spec. Rationale for the sat.mli split from sat-review item 8's "joint
freeze" is in freeze package §4b (short: freezing the un-exercised CDCL(T) seam now
guarantees a wasted M4 unfreeze, with no interim protection benefit since no
parallel workstream consumes it before M4).

**Tranche A — M1-end freeze commit (now):** created from the ADR spec (small
concrete modules / a module type; validated by the EUF+LIA adapter-fitness reviews),
plus the three already-scheduled existing files:
| file | frozen content | later |
|---|---|---|
| `smt/core/atom.mli` | id type + `allocator`/`create_allocator`/`fresh` minter (CONTRACT-ATOM) + O(1) ops; no public `of_int` (Rev 6) | — |
| `smt/core/lit.mli` | packed signed literal | — |
| `smt/core/theory.mli` | `THEORY` sig, `effort`, `check_result` (Split general → no `Lemma`) | — |
| `smt/core/explanation.mli` | `Explanation.t` record + `Rule_tag` enum (payload-free) | **no payload unfreeze** (ADR-0006): M5 certs via off-core `smt/certificate/`; only future new tag constructors are additive |
| `smt/core/{env,rank,theory_view}.mli` | as AGENTS.md schedules | — |

**Tranche B — M2 freeze (with the EUF adapter):**
| file | frozen content | why deferred |
|---|---|---|
| `smt/core/model.mli` | `Model.t` + `value` (incl. `Uninterp` encoding) | its `Uninterp` witness (open q3) is pinned by EUF's first real model — avoids a freeze-then-unfreeze (minor d). `theory.mli` freezes in A naming only `Model.t`/`value`-accessor (stable), not the variant. |

**Tranche C — M4 freeze (with CDCL(T) integration):**
| file | frozen content | why deferred |
|---|---|---|
| `smt/solver/sat.mli` | propositional surface **+ the §3 theory-callback seam + ADR-0006's `trace.on_input`/`trace.on_unit`** | the seam is non-additive and un-exercisable without the M4 solve loop; the DRAT trace hooks (additive) are folded into the *same* one-shot `sat.mli` event so it changes once (freeze package §4b). |

`SPINE.md` is regenerated (`make spine`) after each tranche; `make check-frozen`
green after each `FROZEN.sha256` update.

---

## Firm vs provisional

| # | Decision | Status |
|---|---|---|
| 1 | One `THEORY` type; `Combine` is a `THEORY` functor (Alt-Ergo `CC(X)` prior art) | **FIRM** |
| 2 | `Atom`/`Lit` currency; `register_atom` sole `Term` entry (CONTRACT-REG-1/2) | **FIRM** |
| 3 | `check(effort)`; lazy + precedence-valid `explain` (CONTRACT-EX) | **FIRM** |
| 4 | model-based N-O; induced-equality comparison; `Combine.model` merge | **FIRM** |
| 4 | `interface_eqs` for convex theories | PROVISIONAL (optimization) |
| 5 | `Split` = clausify-to-disjunction (CONTRACT-SPLIT); budget→`unknown` (CONTRACT-SPLIT-TERM) | **FIRM** |
| 6 | one `Context` at `create`; unified push/pop frame stack (decision + assertion frames) | **FIRM** |
| 6 | **no** `Term`/`Context` unfreeze for a brand; debug CONTRACT-CTX at every term boundary | RECOMMENDATION |
| 7 | premise-`Lit`-set + `Rule_tag`; theory self-checks certificate | **FIRM (v1 shape)** |
| 7 | `Rule_tag` certificate payloads | NONE — payload-free permanently (ADR-0006); M5 certs off-core `smt/certificate/` |
| — | `Model.value` variant (incl. `Uninterp` encoding) | UNFROZEN until M2 (off-seam, open q3) |

---

## What M2/M3/M4 implementers may push back on, and how

- **LIA (M3) wants eager conflict at `assert_lit`** (DdM06 `Assert`) — v1 keeps
  `assert_lit` side-effect-only; add an eager `Conflict` return if profiling
  demands (additive).
- **LIA wants rationals in the explanation** before M5 — that is the D7 planned
  M5 payload unfreeze, pulled earlier if the self-checker needs it at the `core`
  boundary.
- **EUF (M2) wants to report entailed shared equalities** — optional
  `interface_eqs` (D4), additive.
- **M4 wants a named `Lemma`** — it is `Split` over already-existing atoms
  (CONTRACT-SPLIT); add a thin alias if ergonomics demand, no semantic change.

## Stage-2 E-matching sanity check — documented REQUIREMENTS (not v1 features)

- **R-EM1 (met).** Atom set grows mid-search — `Split`/`register_atom` are the
  funnel; a ground lemma is a `Split` disjunction (CONTRACT-SPLIT).
- **R-EM2 (met).** All ground terms enter via the single `register_atom` funnel
  (CONTRACT-REG-1), so a future E-matching indexer taps that stream.
- **R-EM3 (deferred, additive).** EUF must expose read-only iteration over
  registered ground `App` terms grouped by top symbol, for trigger matching. Not
  in frozen `THEORY` (EUF-specific, QF-unneeded); the freeze leaves room for it as
  an additive EUF query (ordered by `Symbol.name`, C8).
- **R-EM4 (deferred).** Trigger registration + the instantiation loop live in an
  E-matching manager *above* the theories; bound-variable nodes are a future
  `Term` node under the ADR-0003 unfreeze.

Net: **E-matching-ready** — the atom set grows via `register_atom`/`Split` (FIRM),
and the freeze leaves room for the additive EUF iteration hook (R-EM3).

## M1-end sanity-pass checklist (cross-ADR dependency on the SAT core, review B1)

CONTRACT-SPLIT's correctness rests on two properties of the M1 CDCL core,
re-verified at the M1-end sanity pass (Rev 4) against the trunk `smt/solver/sat.mli`
(now merged; freeze package §1a):

1. **The SAT core must RETAIN splitting clauses and branch on them.** Verified:
   `add_clause` is *permanent* and legal between `solve`s (`sat.mli:51,54`), so a
   `Split` clause persists. **Caveat that forces the trichotomy (B1):** `add_clause`
   does level-0 **tautology removal** (`sat.mli:52`), so the one-atom `A ∨ ¬A`
   encoding is *silently dropped* — the trichotomy over ≥2 distinct atoms is what
   actually survives and branches. Re-check this line survives any later core
   revision before the M4 seam freeze.
2. **Completeness needs a FULL assignment over registered atoms at `Final`.**
   Verified: `solve`→`Sat` produces a *total* `var`-indexed model
   (`sat.mli:61-67` `value`/`model`), i.e. MiniSat branches until every allocated
   variable is assigned. So `check Final` is invoked at a complete boolean
   assignment over every registered atom; the engine need **not** handle
   unassigned atoms (recorded resolution: *full assignment*, not "Final tolerates
   don't-cares"). If a future core adds don't-care/pure-literal model compaction,
   this assumption must be revisited.

## CONTRACT-POISON — theory exception / instance-poisoning discipline (Rev 4; lia-review item 10)

The M1-end sanity pass surfaced a real, reproduced hazard in the LIA engine
(lia-review item 10): a `Lia.t` that let a `Rational.Overflow` escape a
state-mutating op is left mid-pivot (INV-EQ broken) and, *on reuse*, returns a
demonstrated **spurious `Sat`** on a truly-UNSAT system — silent unsoundness. This
is a general property of any incremental theory, so it is stated as a frozen
interface contract, not left to a per-theory doc:

> **CONTRACT-POISON (an ENGINE obligation, uniform across theories).** If **any**
> `THEORY` operation raises **any** exception (`Term.Overflow`, `Term.Unsupported`,
> `Stack_overflow`, a theory-specific poison exception, …), the raising `THEORY.t`
> instance is **bricked**: the engine MUST NOT call any further operation on it, and
> MUST degrade the current query to `unknown` (I8 session boundary). A poisoned
> instance's later `check` may report a spurious verdict, so reuse is a soundness
> bug, not merely a completeness loss.

**Locus — engine catch-all, not theory-internal (verifier finding #3, adjudicated).**
The primary enforcement is an **engine catch-all**: the engine wraps every call into
a `THEORY` op and, on *any* escaping exception, discards that theory instance and
returns `unknown` (I8). This is **uniform** — it covers EUF, LIA, and every future
theory identically, so **EUF needs no poison mechanism of its own** (it can still
corrupt mid-mutation, e.g. a partial `pop` or a `Stack_overflow` in `assert_eq`'s
merge; the engine catch-all handles it). LIA's `mutable poisoned` flag (lia-review
item 10) is retained as **defense-in-depth** — it converts LIA's specific
overflow-mid-pivot corruption into a loud failure even against an engine that
mistakenly reuses — but it is *not* the contract's locus. This resolves the earlier
"concrete enforcement = LIA flag" ambiguity: the obligation lives at the engine, the
LIA flag is a belt-and-suspenders extra.

Scope note (pre-commit): CONTRACT-POISON is **ADR-level discipline**; it is
deliberately **not** baked into the frozen `theory.mli` doc-text (which would embed
the ambiguous engine-vs-theory wording into the hash). The frozen `theory.mli`
carries only per-op behavior; the poisoning discipline stays here. Status: the
engine catch-all is an **M4** obligation (the engine is built at M4); LIA's
defense-in-depth flag is a **pending M3** change (task #71). Neither blocks the
Tranche-A freeze.

## Open questions (for the freeze checkpoint)

1. ~~`Atom.allocator` in `core` vs engine~~ — **resolved (Rev 6): the `allocator`
   type + `fresh` minter live in `core`'s `atom.mli`; the engine holds one
   allocator instance.** (The earlier "bare id type" answer was incomplete — a bare
   id type cannot mint ids safely; see Rev 6 / CONTRACT-ATOM.)
2. `push`/`pop` as relative `pop n` vs absolute `backtrack_to level` — absolute is
   harder to desync; pin against the SAT core's actual backtrack call site (M1).
3. `Uninterp` witness encoding: opaque class ids vs caller-assigned distinct
   witnesses — pin against the Lean `sat` encoder (§8, `decide`/`native_decide`)
   before M2 exercises the sat side (planned refinement, §Freeze plan).
4. Split/decision budget threshold (CONTRACT-SPLIT-TERM) — set with the first real
   LIA workload; must stay deterministic (I6).
