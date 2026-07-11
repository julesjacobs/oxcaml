# ADR-0005 — M1-end sanity pass + THEORY freeze package

Author: theory-adr-designer (board #114). Date 2026-07-11. Companion to
`adr-0005-theory-interface-draft.md` (Revision 4, Accepted-pending-commit).
No `main/` writes. Adversarial verification round follows before the freeze commit.

Artifacts checked (real code, as built):
- `smt/solver/sat.mli` (trunk) — SAT core with `trace` hook + assumptions.
- `worktrees/euf/smt/theories/euf/euf.mli` (+ `logs/euf-review.md`, APPROVE).
- `worktrees/lia/smt/theories/lia/lia.mli` (+ `logs/lia-review.md`, APPROVE-conditional).
- `smt/interface/session.mli` (task/wiring, merged/merging; `logs/wiring-review.md`, APPROVE).

---

## 1. Sanity pass

### 1a. ADR §M1-end checklist re-verified against real `sat.mli`

Both properties CONTRACT-SPLIT depends on hold on the trunk `sat.mli` (line numbers
re-checked; the ADR's Rev-3 citation `sat.mli:53` is corrected to `:52`):

1. **Split clauses are retained, and the tautology hazard is confirmed.**
   `add_clause` is "a **permanent** clause … Legal between [solve]s"
   (`sat.mli:51,54`) — split clauses persist. **Crucially**, `add_clause` does
   level-0 "tautology/duplicate/falsified-literal removal" (`sat.mli:52`) — so the
   discarded-N-O encoding `Eq ∨ ¬Eq` (`A ∨ ¬A`) is *silently dropped by the real
   code*. This is direct, code-level corroboration of B1: the ℤ-trichotomy
   (`[Eq; lt; gt]`, three distinct atoms) is not a stylistic choice, it is the only
   encoding that survives `add_clause`. **Sanity-pass action:** any M4/M-later
   revision of `add_clause` must preserve the fact that a non-tautological
   ≥2-distinct-atom clause is retained (it is; nothing to change now).
2. **Full assignment over registered atoms at `Final`.** `solve`→`Sat` yields a
   *total* `var`-indexed model: `value t v` for every var, `model` "as a
   `var`-indexed array (element v is value t v)" (`sat.mli:61–67`). MiniSat branches
   until every allocated variable is assigned, so `check Final` is invoked at a
   complete boolean assignment over every registered theory atom. **Recorded
   resolution: FULL assignment** — the engine need not handle unassigned atoms at
   `Final`. Revisit only if a future core adds pure-literal / don't-care model
   compaction (none today).

### 1b. FIRM decisions vs euf.mli / lia.mli / Session — impedance list

Near-empty, as the reviews' adapter-fitness verdicts predicted. Every FIRM decision
is satisfiable by a thin adapter with **no unfreeze of any theory API or the THEORY
sig**. The bridges are additive, engine/adapter-owned, and off the frozen types:

| FIRM decision | how EUF / LIA / Session meets it | status |
|---|---|---|
| D1 one THEORY; Combine functor | EUF/LIA are `'p`/`'tok`-parametric *engines*; adapter instantiates `'p=Lit.t` and forwards register/assert/check/explain/push/pop (euf-review §6, lia-review §7) | clean |
| D2 Atom/Lit currency; register sole Term entry | `register_term`/`register_atom` mine subterms (euf.mli:47–54, CONTRACT-REG-1/2); assertions carry the opaque token, never a raw `Lit` fabricated by the theory | clean |
| D3 check(effort); lazy explain | EUF `propagate`+`check`/`explain_implied`; LIA `check`(ℚ)/`solve_integer`(ℤ)/`propagate` — adapter folds `propagate`+`check` into `check(effort)` (euf-review §6b) | clean (fold is additive) |
| CONTRACT-EX precedence | EUF forest stores original edges, never rewrites, redundant-merge-skipped (euf.mli:84–91, euf-review §1c — tested `{1;2}` not the newer shortcut) | **verified in code** |
| D4 model-based N-O; induced-equality | EUF `class_of` → `Uninterp` class-id; LIA `model` → `Int`; Combine compares induced relations, merges (euf-review §6, lia-review §7) | clean |
| D5 Split = trichotomy/branch disjunction | LIA `suggest_branch` = `(x≤⌊v⌋, x≥⌊v⌋+1)` via session ctx (lia.mli:85–90); negated Int `Eq`→`Unsupported`, engine does trichotomy (lia.mli:52–56,130) | **matches CONTRACT-SPLIT** |
| CONTRACT-SPLIT-TERM budget→unknown | LIA `solve_integer ?budget` → `Int_unknown` (lia.mli:76–83) | clean |
| D6 one Context, push/pop frames | both `create ctx`, level-granular `push`/`pop n` (euf.mli:124–131, lia.mli:106–110); Session threads exactly one Env+Context (session.mli `create`/`env`/`context`) | clean |
| D7 premise-set + coarse Rule_tag | EUF `Conflict of 'p list`→`{premises;rule=Euf_congruence}`; LIA `{premises;farkas}`→`{premises;rule=Lia_farkas}`, farkas kept theory-internal (self-checked, lia.mli:9) | clean; farkas is the M5 payload, pre-computed |
| C1–C8 determinism | EUF: no Hashtbl iteration, int-id/tag order only, C8 symbol-id footgun not triggered (euf-review §7); LIA: Bland + IntMap id-order (lia-review §8) | **verified in code** |

**The three known bridges (additive, off the frozen sig):**

1. **EUF predicate atoms.** A Bool-codomain `App` predicate `p(x⃗)` has no equality
   form natively. The adapter synthesizes reserved `true`/`false` EUF constants + a
   standing `true ≠ false` disequality and encodes `p(x⃗)=true` / `=false`
   (textbook CC-for-predicates; euf-review §6a). Adapter-owned; EUF's App-only
   congruence handles `p` as an ordinary function.
2. **LIA negated Int equality → engine-side ℤ trichotomy.** LIA `assert_atom`
   raises `Unsupported` on a negated `Eq` (lia.mli:52–56); the engine never asserts
   it — it issues the CONTRACT-SPLIT trichotomy `[Eq; lt; gt]`. Exactly the design
   already fixed in Rev 3; the LIA code was built to it.
3. **Session `is_theory_atom` seam.** Session classifies each frozen-`is_atom` atom
   as theory-vs-propositional (wiring O2), a second exhaustive match beside
   `Theory_view.is_atom`. The M4 adapter's *which-theory* dispatch (EUF | LIA | both,
   by sort/head) extends this same seam. Exhaustive matches make it drift-proof.

**Two non-bridges worth recording (no action):**
- `create ctx` (EUF/LIA) vs `create ctx env` (THEORY): v1 theories need no `Env`; a
  trivial env-ignoring wrap. Keep `Env` in `THEORY.create` for forward-compat
  (datatypes/arrays declare fresh symbols — §2 datatypes lens).
- EUF `class_of` / LIA `model` are exactly the `Model` witnesses (open q3); no
  mismatch, just the pre-M2 encoding pin.

**One genuine interface obligation surfaced (LIA, lia-review item 10): poisoning.**
A LIA instance that let a `Rational.Overflow` escape a state-mutating op is corrupt
and reuse yields a *demonstrated spurious `Sat`* (lia-review reproduced
`Sat_candidate`/`SAT[2,0]` on a truly-UNSAT system). This is not a shape mismatch
but an **exception-discipline contract** the THEORY interface must state — added to
the ADR as **CONTRACT-POISON** (§2, poisoned resolution).

---

## 2. Checkpoint resolutions

- **#24 per-Context brand — RESOLVED: no unfreeze; engine debug-assert (CONTRACT-CTX).**
  Confirmed against the built Session: `Session.create` makes exactly one
  `Env`+`Context`; `env`/`context` hand out *that* one; `assert_term` requires terms
  built through it (session.mli). So the single-`Context` reality holds by
  construction in the shipped wiring — the R3 hazard has no live second-context
  surface. Recommendation stands: keep `Term`/`Context` frozen; the engine tags its
  `Context` with an id and debug-asserts it at every `Term` boundary (register_atom,
  Split return, Model.value arg). Feeds the M1-brand-checkpoint (owns the final call).
- **Datatypes lens — RESOLVED: fits as an N-O-combined plugin; one deferred want.**
  See §2a for the full hour-of-reading finding. Bottom line: EUF's App-only
  congruence + opaque leaves + the Combine functor are datatype-ready; a datatype
  theory adds new `Rule_tag` *constructors* (an additive enum unfreeze — distinct
  from ADR-0006's no-payload decision) and needs **no core
  Sort unfreeze** (model datatypes as uninterpreted sorts + uninterpreted
  constructor/selector/tester functions + a datatype-rule plugin). The single thing
  the frozen interface does not offer is *direct e-graph sharing* (Shostak/CC(X)-style
  tight embedding); N-O loose coupling suffices for v-next, and tighter sharing is a
  known additive EUF hook (same family as R-EM3), deferred.
- **Context.env accessor — RECOMMEND DEFER (no unfreeze now).** Driver check:
  `THEORY.create` takes `Env` explicitly, so the adapter never needs `Context.env`;
  preprocess holds its own `Env`; Session exposes `Session.env`. No consumer is
  actually blocked. Cost of taking: unfreeze `context.mli` (hash + ADR + adversarial
  review) for a one-line accessor — disproportionate. Defer; add it in the next
  `context.mli` unfreeze that has an independent driver.
- **Context.sum convenience (#49) — RECOMMEND DEFER (no unfreeze now).** This is a
  *perf* fix (O(n²) pairwise `add`/`sub` on wide sums), not correctness; the pairwise
  builder is correct. It touches frozen `context.mli`. Defer until STATUS perf
  tracking shows the wide-sum cliff on a real/adversarial VC; when it fires, batch
  the `Context.sum` (+ any other `context.mli` change) into one unfreeze to amortize
  the ritual. Trigger recorded: STATUS §8 wide-sum outlier alert.
- **LIA poisoned-instance flag — INCORPORATED as CONTRACT-POISON (pending M3 land).**
  Generalized to an interface contract: *if any `THEORY` op raises
  (`Overflow`/`Unsupported`/`Poisoned`), the engine must treat that theory instance
  as bricked — never reuse it — and degrade the query to `unknown` (I8 session
  boundary).* The concrete enforcement is lia-review item 10's `mutable poisoned`
  flag bricking every public LIA entry; that is a required M3 change (still pending
  on task #71) and is a hard precondition for the M4 adapter. Marked PENDING in the
  ADR until the flag lands.

### 2a. Datatypes lens (adversarial reading: how would datatypes plug into THEORY + the e-graph)

Reference procedure: Barrett–Shikanian–Tinelli, "An Abstract Decision Procedure for
the Theory of Recursive Data Types" — congruence closure + datatype-specific rules
(constructor injectivity, cross-constructor distinctness, selector-over-constructor
`car(cons(x,y))=x`, tester consistency, acyclicity/occurs-check, finiteness).

How it maps onto the frozen oxsmt seam:

1. **Representation — no core change.** A constructor/selector/tester application is
   already `App(sym, args)` in the frozen `Term`. Model a datatype sort as an
   `Uninterpreted` sort and `cons`/`car`/`cdr`/`is_cons` as ordinary declared
   functions. So datatype *terms are constructible and congruence-closed today* —
   nothing in `Sort`/`Term`/`Context` needs unfreezing. (A first-class datatype
   `Sort` kind is a *nicety*, not a requirement; it would be the `int_kind`-style hook.)
2. **Congruence for free.** EUF congruence-closes the constructor/selector `App`s
   and treats everything else as opaque leaves (euf.mli:20–24) — precisely the
   "App-only congruence + opaque leaves" seam. The datatype plugin does *not*
   re-implement congruence; it consumes EUF's equalities.
3. **Datatype rules as a THEORY plugin.** A `Datatypes` module implements the same
   `THEORY` sig: `register_atom` recognizes datatype-symbol `App`s (needs a
   symbol-tagging convention or a small `Env` datatype registry — hence keep `Env`
   in `create`), runs injectivity/distinctness/selector/acyclicity rules, and reports
   conflicts/propagations in the **same premise-set + Rule_tag currency** with new
   tags `Datatype_inj`, `Datatype_distinct`, `Datatype_selector`, `Datatype_acyclic`
   (additive *constructor* growth of `Rule_tag`, an enum unfreeze — distinct from
   ADR-0006's no-payload decision).
4. **Combination via the Combine functor.** Datatypes + EUF (+ LIA) combine by the
   existing model-based N-O (`Combine`): shared equalities flow as `Eq` atoms; the
   induced-equality comparison (D4) and the ℤ-trichotomy split generalize (for a
   datatype sort the "split" is on `Eq`/`¬Eq` of shared datatype terms — a two-atom
   `[Eq; Not Eq]`… **caution:** for a *finite/enumerable* datatype the `[Eq;¬Eq]`
   pair is again a one-atom tautology and must instead branch on the constructor
   testers `[is_c1; …; is_ck]` (exhaustive + k distinct atoms, valid *modulo the
   plugin's covering axiom* `⋁ is_ci(x)` — the datatype analogue of the ℤ-trichotomy,
   a clean generalization of B1). **Recursive-datatype caveat (verifier finding #4,
   recorded for the future datatype author):** this tester split settles a shared
   `x=y` only for *finite/enumerable* datatypes (nullary constructors, where
   constructor = value). For a *recursive* datatype (constructors with arguments) the
   tester split does **not** settle `x=y` — the equality split there is a separate,
   still-open matter. Datatypes are post-M4 and unfrozen, so this is a note for the
   datatype author, not a freeze issue.
5. **The one deferred want: direct e-graph sharing.** Efficient datatype solvers
   often sit *on* the congruence structure (share the e-graph, Shostak/CC(X)-style)
   rather than N-O-combining at arm's length. The frozen `THEORY`/`Combine` gives
   loose coupling only. That is adequate for a correct v-next datatype theory (BSt
   combines by N-O), but a performance-tighter embedding would need EUF to expose its
   e-graph — an **additive** read-only hook, same family as R-EM3 (E-matching). Not a
   v1 gap; recorded as a post-M4 option.

**Conclusion:** the THEORY sig + Combine functor + App-only congruence accommodate a
datatype theory with *only additive* changes (new `Rule_tag`s; keep `Env`), no
frozen-core unfreeze. The freeze does not box datatypes out.

---

## 3. `sat.mli` theory-callback seam — concrete `.mli` delta (SPEC for M4)

Per sat-review item 8: the CDCL(T) seam (trail-extension notify, theory-literal
enqueue with lazy reason, conflict injection, backtrack notify) changes
`solve`/propagate and is **not additive**. Below is the binding M4 target, modeled on
the existing zero-cost `trace` hook (a settable callback record, `None` = today's
pure propositional core), consistent with the `THEORY` sig and CONTRACT-EX. This
block is **spec only**; M4 implements it, and its landing is when `sat.mli` freezes
(§4).

```ocaml
(** {2 Theory seam — CDCL(T) (SPEC, ADR-0005; implemented + frozen at M4).}

    Modeled on {!trace}: a settable record, [None] by default (pure propositional
    core; zero cost when unset). When set, [solve]'s propagation loop and its
    full-model checkpoint consult it — this modifies [solve]/propagate, so it is NOT
    an additive edit (why [sat.mli] freezes at M4, not M1). Every [lit] crossing the
    seam names a SAT var the adapter registered 1:1 as a theory atom
    (ADR-0005 CONTRACT-ATOM). *)

type theory_result =
  | T_consistent of lit list
      (** consistent; theory-implied literals to enqueue. Reason is LAZY: the core
          calls [explain] only if the literal enters 1UIP analysis (ADR-0005 D3). *)
  | T_conflict of lit list
      (** the falsified theory clause (¬ of the precedence-valid premise set,
          CONTRACT-EX) to inject as a learned clause and drive backjumping. *)
  | T_lemma of lit list list
      (** clauses to add mid-solve: CONTRACT-SPLIT disjunctions (B&B branch, N-O
          ℤ-trichotomy). Each inner list is one clause over atoms the adapter has
          already internalized via {!new_var}. *)

type theory =
  { on_assign    : lit -> unit
      (** trail-extension notify: [lit] was just placed on the trail; the adapter
          forwards to [THEORY.assert_lit]. *)
  ; on_backtrack : level:int -> unit
      (** backjump notify: the core is unwinding to decision [level]; the adapter
          forwards to [THEORY.pop] (frames discarded = levels unwound). *)
  ; check        : final:bool -> theory_result
      (** [final=false]: cheap in-search check (ADR-0005 [Propagate]).
          [final=true]: complete check at a full model ([Final]: B&B, model-based
          N-O). Driven to fixpoint after Boolean propagation. *)
  ; explain      : lit -> lit list
      (** lazy, precedence-valid reason for a [T_consistent] literal
          (CONTRACT-EX: every returned lit was assigned strictly before [lit]).
          Called only during conflict analysis. *)
  }

val set_theory : t -> theory option -> unit
```

Mapping to the `THEORY` sig (adapter, M4): `on_assign` → `assert_lit`;
`check ~final` → `check (if final then Final else Propagate)` with
`Propagations→T_consistent`, `Conflict→T_conflict` (premises externalized to `lit`s),
`Split terms→T_lemma` after the adapter clausifies each disjunct to a `lit`
(internalizing new atoms via `new_var` + `register_atom` back — CONTRACT-SPLIT);
`Sat→` no lemma/conflict (loop terminates SAT); `explain`→`THEORY.explain`;
`on_backtrack`→`pop`. Model output for a SAT verdict is assembled adapter-side from
`THEORY.model` (merged per D4), *not* over this seam. The mid-solve `T_lemma`
addition is the specific non-additive change to `solve` (a permanent clause added
during search), which today's `add_clause` supports only *between* solves.

---

## 4. The freeze package

### 4a. Files entering `FROZEN.sha256`, in tranches (freeze-after-validation)

The freeze protects the **M2/M3 → M4 seam**. Freeze each file at the milestone that
validates its shape; the **ADR-0005 document is Accepted now**, so every tranche
builds to a fixed spec and cannot drift.

**Tranche A — M1-end freeze commit (now):**
- `smt/core/theory_view.mli` — exists on trunk; the App-vs-Arith dispatch (already scheduled).
- `smt/core/env.mli`, `smt/core/rank.mli` — exist on trunk (already scheduled).
- `smt/core/atom.mli`, `smt/core/lit.mli`, `smt/core/explanation.mli`,
  `smt/core/theory.mli` — **created from the ADR's verbatim `.mli`** (small concrete
  modules: `Atom`/`Lit` = packed `private int` + O(1) ops + Set/Map/Table, `Atom`
  minted only by `create_allocator`/`fresh` — no public `of_int` (Rev 6);
  `Explanation` = record + `Rule_tag` enum; `theory.mli` = `effort` + `check_result`
  + `module type THEORY`). These are pure types / a module type, validated by the
  EUF and LIA adapter-fitness reviews (§1b), and locking them is what gives M4 a
  fixed target and keeps the seam stable. Low risk (no algorithm, no `solve` loop).
  - **`Rule_tag` is payload-free PERMANENTLY (ADR-0006 delta, accepted).** The ADR-0005
    Rev-3 plan had "planned M5 unfreeze: `Rule_tag` payloads." ADR-0006 supersedes
    that: certificate witnesses (Farkas vectors, congruence chains) route through a
    new **off-core `smt/certificate/` module**, never a `Rule_tag` payload — because
    a payload would drag LIA's `Rational` into the frozen core on the hot 1UIP path
    (exactly the I3-firewall reason D7 kept certificates theory-internal). This makes
    `explanation.mli` **more** stable: it has *no* planned unfreeze at all. (Adding
    *new tag constructors* for a future theory — e.g. `Datatype_*` — is still an
    additive enum unfreeze; that is orthogonal to the no-payload decision.) No hole
    seen; accepted into the freeze plan.

**Tranche B — M2 freeze (with the EUF adapter):**
- `smt/core/model.mli` — deferred one milestone (review minor d): its `value`
  variant's `Uninterp` encoding (open q3) is pinned by EUF's first real
  class-witness model, avoiding a freeze-then-unfreeze. `theory.mli` may freeze in
  Tranche A referencing only `Model.t` (abstract) + the `value` accessor signature,
  which are stable; the *variant constructors* are not named in `theory.mli`.

**Tranche C — M4 freeze (with CDCL(T) integration):**
- `smt/solver/sat.mli` — frozen once the §3 theory seam is *implemented and
  validated* against a running theory-integrated `solve` loop. See timing below.

`SPINE.md` regeneration note: `make spine` after Tranche A to concatenate the new
frozen set (the master's working-set view now includes `atom`/`lit`/`explanation`/
`theory` alongside `sort`/`symbol`/`term`/`context`/`iarr`/`theory_view`/`env`/`rank`).
Regenerate again at Tranche B (model) and Tranche C (sat). `make check-frozen` must
be green after each `FROZEN.sha256` update (`tools/check_frozen.sh generate`).

### 4b. `sat.mli` timing — RECOMMENDATION: freeze at M4 (Tranche C), not now

Options, with honest cost:

- **Freeze now with the seam SPEC'd-but-unimplemented.** Pro: one "joint freeze"
  moment. Con: the seam is the *one* interface no implementation has exercised
  (EUF/LIA are `'p`-token engines that never touch `sat.mli`; the CDCL(T) loop is
  M4). sat-review item 8 states the seam changes `solve`/propagate and is
  non-additive, so real M4 integration is *likely* to reshape it (e.g. the `check`
  fixpoint contract, `T_lemma` mid-solve semantics). Freezing now therefore nearly
  guarantees an M4 unfreeze ritual (hash + unfreeze ADR + fresh adversarial review)
  — paying the full cost anyway, for a design we could not validate.
- **Freeze at M4 (recommended).** Pro: `sat.mli` freezes once, against a validated
  implementation; the ADR carries the §3 `.mli` delta as the binding M4 target, so
  M4 cannot drift the design even while the file is unfrozen. Con: `sat.mli` lacks
  hash-protection during M1→M4. **But there is no parallel workstream to protect
  against** — M2/M3 (theory engines) do not consume `sat.mli`'s seam; the only
  consumer is the M4 integration itself, and it is governed by the Accepted ADR.
  Ordinary review still covers `sat.mli` (it is on trunk, M1-reviewed).

Net: deferring `sat.mli` costs nothing in seam protection and saves a near-certain
wasted unfreeze. Freeze it at M4 with the seam implemented. (This is a deliberate
*refinement* of sat-review item 8's "freeze jointly at the M1 THEORY freeze": the
THEORY-vocabulary half freezes now, the `sat.mli` half at M4 — flagged for the
verifier.)

**ADR-0006 delta (accepted): one combined `sat.mli` event.** ADR-0006 adds two
DRAT hooks — `trace.on_input` and `trace.on_unit` (closing the level-0 DRAT gap) —
and asks that they land in the **same** unfreeze/freeze event as the CDCL(T) theory
seam, so `sat.mli` changes once, not twice. Folded in: **Tranche C freezes
`sat.mli` with the §3 theory-callback seam AND the two `trace` extensions
together.** The trace extensions are *additive* to the existing `trace` record
(unlike the seam, which changes `solve`/propagate), so bundling them is free —
they piggyback on the ritual the seam already forces. Timing stays M4: certificates
are pulled forward (DESIGN §7/§9 "M5 pulled early"), so the DRAT hooks are expected
to be ready alongside the seam. **One honest coupling to flag for the verifier:**
bundling ties the theory-seam freeze to certificate-work readiness. If certificate
work slips well past M4, revisit — freezing the seam alone at M4 (accepting a second
`sat.mli` touch for the trace hooks later) beats leaving the seam unlocked
indefinitely. Default: one combined event; fall back to seam-first only if the DRAT
hooks are not ready when the seam is.

### 4c. Deliberately NOT frozen (and why)

- **`smt/solver/sat.mli`** — until Tranche C (M4); the theory seam is non-additive
  and un-exercisable before the CDCL(T) loop (§4b).
- **`model.mli`'s `value` variant** — until Tranche B (M2); EUF pins the `Uninterp`
  encoding (open q3).
- **`euf.mli`, `lia.mli` (theory *engine* APIs)** — never frozen. They sit *behind*
  the `THEORY` adapter; only the adapter's conformance to the frozen `theory.mli`
  matters. They stay free to evolve (perf, algorithm) as long as the adapter still
  presents `THEORY`. The three ADR-0006 pre-adapter additive items land here and do
  **not** touch the frozen list: **`Euf.explain_proof`** (a structured
  congruence-chain proof beside the flat premise list) and **`Lia.int_proof` on
  `Int_unsat`** (a B&B refutation certificate) are additive engine-API growth
  consumed by the off-core `smt/certificate/` module; neither changes `theory.mli`,
  `explanation.mli`, or any frozen surface.
- **`smt/certificate/` (ADR-0006)** — a new off-core module; not part of the ADR-0005
  frozen set at all. It is where M5 certificate witnesses live, keeping `Rational`
  and proof structures off the frozen core (§4a Rule_tag note).
- **`context.mli` additions** (`Context.env`, `Context.sum`) — deferred, no driver
  (§2). `context.mli` stays frozen as-is (no brand either, #24).

---

## Report-ready summary

- **Impedance:** near-empty. All FIRM decisions met by a thin adapter, no theory-API
  or THEORY-sig unfreeze. Three additive bridges (EUF predicate→true/false consts;
  LIA negated-Eq→engine trichotomy; Session is_theory_atom dispatch), all
  adapter-owned and off the frozen types. CONTRACT-EX and C1–C8 verified in EUF/LIA
  code, not just claimed. One real obligation: LIA poisoning → CONTRACT-POISON.
- **Sanity pass:** both checklist items hold on real `sat.mli` (add_clause permanent
  `:51,54`; tautology removal `:52` *confirms* the trichotomy; total model
  `:61–67` → full assignment at Final). Rev-3 line cite `:53`→`:52` corrected.
- **Checkpoints:** brand → no unfreeze + debug CONTRACT-CTX (Session threads one
  Context, confirmed); datatypes → fit as N-O plugin, additive Rule_tags, no Sort
  unfreeze, tight e-graph sharing deferred; Context.env → defer (no driver);
  Context.sum(#49) → defer (perf, trigger = STATUS wide-sum alert); LIA poisoned →
  CONTRACT-POISON, pending the M3 flag.
- **Seam spec:** §3 `.mli` delta — `theory` callback record (`on_assign`,
  `on_backtrack ~level`, `check ~final`, lazy `explain`) + `theory_result`
  (`T_consistent`/`T_conflict`/`T_lemma`), modeled on `trace`, mapped to the THEORY
  sig; `T_lemma` carries CONTRACT-SPLIT disjunctions.
- **Freeze files:** Tranche A (now) atom/lit/explanation/theory + env/rank/theory_view;
  Tranche B (M2) model; Tranche C (M4) sat.mli. `make spine` + `check-frozen` each
  tranche.
- **sat.mli timing:** freeze at M4 (implemented seam), not now — avoids a
  near-certain wasted unfreeze; no interim protection lost (no parallel consumer).
