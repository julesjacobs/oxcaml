# ADR (RATIFIED): Lemma tier — stage-2 quantifier instantiation (L1 normative; L2–L6 interface sketches)

- **Status:** RATIFIED (Rev 3.2) — design-only, no code yet; **L1 is normative**, **L2–L6 are interface sketches**. Dual adversarial review complete (same-model + codex, `logs/adr-lemma-tier-*`); ratified by the design author. Promoted verbatim from `logs/adr-lemma-tier-draft.md`; the DRAFT (rev 3) status line and all sections below are preserved as-ratified for the record. Stage-2 implementation is gated behind M4 close + the sat.mli/env.mli freeze rituals; tranche 1 is a future fresh-builder task.

- **Status:** DRAFT (rev 3) — design-only, no code, no `main/` commit. Dual adversarial
  review (same-model + codex) before ratification. **L1 is normative**; **L2–L6 are
  interface sketches**.
- **Rev 3 (this pass):** folds the merged dual-review order
  `logs/adr-lemma-tier-reconciliation.md` (R1–R9) in one pass; O1 hinge RULED by the
  design author. Highlights:
  - **O1 RULED (§3a):** (A) placeholders **+ a scoped `env.mli` unfreeze** — A5 clears
    freeze-weight in both directions, and (A)+env-guard is smaller/better-layered than
    (B)'s binder in every stage-1 matcher.
  - **R1 (CRITICAL, forgeability):** placeholders are unforgeable — public
    `Env.declare_fun`+`declare_sort` reject `.oxsmt.*` (the scoped frozen touch),
    capability mint door, `assert_lemma` is now a **binder-builder** minting `Qvar.t`
    handles *before* the body, parser **rejects nested ∀** (§1.1/§1.3/§6).
  - **R2 (CRITICAL, provenance):** merged my C1 (spec) + codex's code-half —
    `assert_term_at_frame` under the lemma's selector (not innermost, `session.ml:127`),
    dedup scoped to active-clause lifetime; **H-PUSHPOP + H-REPEAT-REFUTE** (§1.4/§7).
  - **R3–R8:** one `check_sat` exit naming uf-models #61 + #88 (R3); budget debited
    **inside** matching (R4) + every-tier hard budget (R5) + fairness (R8), with
    **H-XPROD/H-MUTUAL**; non-registering e-graph view (R6, `are_equal` mutates);
    determinism verdict-affecting under a tight budget (R7). **R9** editorial positives.
  - Rev 2's C1/H1/H2 fixes are retained and extended by R1–R8.
  Re-review is the scoped R1/R2 confirm; the codex leg runs once on this final text.
- **Rev 3.1 (post-confirm polish):** lemma-reviewer CONFIRMED both CRITICALs resolved.
  Two folded edits: (1) fixed the stale Rev-2 "no frozen `.mli` touched" guardrail (§0)
  to state the one scoped `env.mli` unfreeze; (2) stated the residual post-mint injection
  path explicitly — **`Instance.of_subst` is the load-bearing assert-side guarantee,
  namespace unforgeability is defense-in-depth** — and specced the cheap tranche-1
  symmetry hardening (`assert_term` also rejects `.oxsmt.qvar.*`, closing the residual
  entirely). Ratification waits only on the codex final leg.
- **Rev 3.2 (codex final-leg fold; codex `logs/codex-review/adr-lemma-tier-rev3.md`
  CHANGES-NEEDED — no soundness holes, budget family CONFIRMED, deeper trigger-frame cut
  SOUND):** implementability/spec-precision fixes. **POINT 1 (HIGH, the load-bearing
  one):** the two-door mint didn't build — `private_modules` privacy doesn't cross
  library boundaries, so `Env_unsafe` was uncallable from preprocess/ematch. Redesigned
  to a **buildable capability**: abstract `reserved_cap` from `Env.create`,
  `declare_reserved`, threaded session-side; public `Env.declare_fun`+`declare_sort`
  reject `.oxsmt.*` (§1.1/§3a/§6). A5 comparison unchanged (still the env.mli unfreeze).
  **POINT 4 (HIGH):** `Qvar.t` escapes by coercion — the Rev-3.1 `assert_term` gate
  already closes it; made its failure mode a **clean `Unknown`** (not an
  `Instance.of_subst` `Failure` crash), split from the internal bug-catch. **POINT 3:**
  `declare_sort` covered too (shared namespace). **POINT 2:** rewrote H-REPEAT-REFUTE
  (pushed lemma → pop → new equivalent lemma; the base-lemma version tested nothing).
  **Selector type:** `frame : Sat.var`, guard `Sat.neg frame` (`sat.mli:29`).
  **POINT 6:** `assert_term_at_frame` is PRIVATE, takes `Instance.t`. Re-review is the
  scoped R1/R2 confirm **+ an empirical dune buildability check of the mint mechanism**;
  no third codex round.
- **Owning outcome:** TODO "Lemma tier" L1–L7 (stage 2, DESIGN §2). Highest product
  value: OxCaml refinement VCs need user-stated lemmas (quantified facts — the
  liquid-typing "measure lemma" style: `∀xs. len(cons(x,xs)) = 1 + len(xs)`)
  instantiated by E-matching during solving. Board: (assign on dispatch).
- **Scope guardrails (normative):** stdlib-only (I3); deterministic, no
  hash-order-dependent iteration in any user-visible output (I6, ADR-0005 C8);
  every mechanism carries its failure-direction analysis; the ADR-0010 combination
  seam is **not bypassed** — every instantiation is a new **ground** assertion
  entering through the *same* preprocess → clausify → `register_atom` → `Combine`
  pipeline as a user assertion. **Exactly one frozen `.mli` is touched — a scoped
  `env.mli` unfreeze** (R1's forgeability fix, ruled on merits per DESIGN A5, §3a/§6);
  everything else is untouched-frozen or non-frozen-additive.
- **Cites:** DESIGN §2 (staging: "each round asserts ground instances and re-runs
  the ground core"), §5 (engine), §6 (combination), §7 (reasons/certificates), §8
  (gate, honeypots), §10 (oracle-first, honeypots, TCB); INVARIANTS I1/I2 (frozen
  terms), I3 (firewall), I4 (justified), I6 (determinism), I8 (degrade-never-crash);
  ADR-0003 (frozen 9-node `Term`, reserved `.oxsmt.*` namespace); ADR-0005 D2
  (`Atom`/`Lit` currency, `register_atom` sole term entry), D5 (`Split` =
  clausify-to-disjunction; **`check_result` doc names "E-matching lemma"**),
  CONTRACT-SPLIT/-TERM (split budget → `unknown`), R-EM1–R-EM4 (stage-2 sanity
  requirements, already recorded), C8 (`Symbol`-keyed-iteration footgun);
  ADR-0006 §Decision 1 (off-core certificate leaves, valid-lemma class, "Lean replay
  via specialize"); ADR-0010 §3.4 lemma-readiness invariants (i)–(iv), §3.6 (Bool
  boundary degrade), the grow-only/both-valued-skip machinery.

---

## 0. Goal, and the one fact that constrains everything

**Goal.** Let a client state universally-quantified lemmas; instantiate them by
E-matching against the terms the solver already reasons about; feed each ground
instance back through the ground solver; conclude `unsat` when the ground core +
instances is `unsat`. This is refutation-only quantifier reasoning — the standard
SMT E-matching loop (DESIGN §2 stage 2).

**The representation choice (argued on merits, not on freeze-avoidance — DESIGN A5).**
The frozen `Term` type (ADR-0003, I1/I2) is a **9-node set with no quantifier node and
no bound-variable node** (`term.mli:25-35`). Two representations are available: (A)
lemmas *outside* `Term`, ∀-variables as reserved-namespace placeholder constants; (B)
proper binder nodes *in* `Term` (`Forall`/`BoundVar`, the ADR-0005 R-EM4 unfreeze).
**§1.1 recommends (A) on design merits — correctness-by-construction, enforceability,
extensibility — with freeze-avoidance carrying zero weight (A5); O1 is the full
two-column comparison, RULED (A) + a scoped `env.mli` unfreeze.** The recommendation is
*contingent on the target fragment* (single-level ∀ over a ground body); its flip
condition (nested/alternating quantifiers) is stated in O1. (A) still requires one
scoped frozen touch — the `env.mli` door-that-rejects (R1) — which is *less* machinery
than (B)'s binder in every stage-1 matcher, decided on merits with freeze-weight
inadmissible in both directions (A5, §3a). This is the first thing reviewers should
attack, and the comparison is built to be attacked.

**What is sound and what is not (the whole soundness story in three lines).**
(1) A ground instance `φ(σ)` of a valid lemma `∀x̄. φ(x̄)` is a *valid consequence*,
so asserting it removes no models — **`unsat` after instantiation is sound**.
(2) E-matching is *incomplete*: we never prove we generated every needed instance,
so we can never conclude the quantified problem is satisfiable — **`sat` while any
quantifier is live must degrade to `unknown`** (THE SOUNDNESS RULE, §2).
(3) Every failure direction of every mechanism below (budget exhaustion, matching
loop, over/under-triggering, dedup) resolves toward `unknown`, **never** toward a
wrong `sat`/`unsat`. §3 makes this exhaustive.

---

## 1. L1 — the lemma store, the pipeline, the loop (NORMATIVE)

### 1.1 Lemma representation — placeholder constants (NORMATIVE; recommended on merits, O1)

The choice between placeholder constants (A) and binder nodes in `Term` (B) is argued
on merits in **O1**; this section specifies the recommended option (A). A lemma is
stored **outside** `Term.t` in a new `smt/ematch/` library:

```
Qvar.t   = private Term.t   (* an unforgeable placeholder handle: a `.oxsmt.qvar.*`
                               nullary App, mintable ONLY inside smt/ematch/ *)
Lemma.t = { qvars    : Qvar.t array         (* the ∀-bound variables, minted before the body *)
          ; body     : Term.t               (* well-sorted Bool term over qvars + ground syms *)
          ; triggers : Term.t list list     (* multi-triggers; inner list = conjunctive trigger *)
          ; id       : int                  (* dense, deterministic; certificate + dedup key *)
          ; frame    : Sat.var              (* owning frame's SELECTOR VAR; guard = Sat.neg frame (R2) *)
          ; origin   : origin }             (* :named / VC-provenance, for cores + messages *)
```

Each ∀-bound variable is a **fresh nullary constant** in the reserved `.oxsmt.qvar.*`
namespace, wrapped in an abstract `Qvar.t` the caller can *use* (to build the body) but
cannot *forge*.

**Unforgeability (R1, CRITICAL — two doors, mint vs forge).** Dual review verified the
naïve scheme is forgeable: `Symbol.intern` is a **process-global by-name table**
(`symbol.mli:19-22`) and `Session.env` hands out the raw `Env` (`session.mli:99-102`)
whose `Env.declare_fun` rejects only `div`/`mod`, **not** `.oxsmt.*` (`env.mli:30-33`)
— so a client can `Env.declare_fun (Session.env s) ".oxsmt.qvar.0.0"` and collide with a
placeholder by global name (codex's wrong-`unsat` repro). The fix separates the two
doors:

- **Forge door CLOSED — public `Env.declare_fun` AND `Env.declare_sort` reject the
  `.oxsmt.*` prefix** (not just `div`/`mod`). **Both** public declaration entries must
  reject, because sort and function names share one global symbol namespace
  (`env.mli:9-14`; codex POINT 3 — guarding only `declare_fun` leaves
  `Env.declare_sort (Session.env s) ".oxsmt.qvar.0.0"` open). The `reserved_prefix`/
  `is_reserved_name` constant (today in `preprocess.ml:33-38`) **moves into `core`/`Env`**
  as the single source of truth (preprocess and the parser reference `Env`'s copy),
  retiring the two-implementations-must-agree drift of Rev 2. This is the **scoped
  `env.mli` unfreeze** (§6, §3a ruling): the public doors reject *by construction*.
- **Mint door — a CAPABILITY minted by `Env.create`, threaded session-side (buildable;
  codex POINT 1, VERIFIED).** The Rev-3 "core-private `Env_unsafe` module" does **not
  build**: `private_modules` privacy does not cross library boundaries, and
  `Preprocess`/`smt/ematch` are libraries *separate* from `oxsmt_core`
  (`smt/preprocess/dune:8-10`) — they cannot call a core-private module, and making it
  public re-opens forgery. The buildable mechanism uses a **capability token**:
  - `env.mli` gains an **abstract `reserved_cap`** (no public constructor) and
    `val declare_reserved : reserved_cap -> t -> string -> Rank.t -> Symbol.t` (mints a
    reserved name, cap-authorized, skips the prefix check). `Env.create` returns the cap
    once: `val create : unit -> t * reserved_cap`.
  - The **Session** (`oxsmt_interface`) calls `Env.create`, keeps the `reserved_cap` in
    private session state, and **threads it as an ordinary argument** to the two
    legitimate minters — `Preprocess.fresh_symbol ~cap …` (replacing its current bare
    `Env.declare_fun`, `preprocess.ml:40-43`) and `Ematch.mint_qvar ~cap …`. Plain values
    cross library boundaries fine — no privacy gymnastics.
  - `Session.env : t -> Env.t` returns **only** the `Env`, never the cap. A client with
    the raw `Env` therefore cannot `declare_reserved` (no cap) and cannot
    `declare_fun`/`declare_sort` a `.oxsmt.*` name (rejected). Mint and forge are
    different doors: mint = cap-gated (unobtainable through the public Session API),
    forge attempt = public-guarded → rejected. *(Buildability sketch for the reviewer's
    dune check: `reserved_cap` abstract in `env.mli`, e.g. `type reserved_cap = unit`
    internally; `declare_reserved _ t name rank` = raw intern; `fresh_symbol`/`mint_qvar`
    take `~cap`. No cross-library private module — this is the finding's empirical fix.)*
- **Rider:** `.oxsmt.qvar.<lemma-id>.<k>` stays disjoint from preprocess's
  `.oxsmt.<kind>.<n>` counter (distinct `qvar` segment) so no mint clobbers another's
  rank (`Env.declare_fun` overwrites on re-declare, `env.mli:30-33`).

`body` and each `trigger` are then ordinary hash-consed `Term.t`s built through the
session `Context`; an `App` whose head is a qvar placeholder is structurally a nullary
constant. No new `Term` node is needed.

**Instantiation = capture-free substitution = a rebuild through `Context`.** Given
a substitution `σ : placeholder ↦ ground Term.t`, the instance is
`subst σ body` — a bottom-up rebuild via the smart constructors, replacing each
placeholder `App` with `σ`'s ground term. Hash-consing makes the result canonical
and O(1)-comparable; there are no bound-variable capture issues because placeholders
are distinct fresh constants, never reused across lemmas (I1/I2 hold by
construction). Substitution is total and deterministic.

**Leak prevention — the assert-side gate is load-bearing; `Qvar.t` privacy is not
enough (R1 POINT 4).** The hazard option A must rule out is a placeholder reaching the
solver (registered, in a model, matched as ground). `Qvar.t = private Term.t` blocks
*construction* of a qvar handle but **not upward coercion**: a builder can stash its
`Qvar.t`, coerce it to `Term.t`, and call `assert_term (P x)` with the leaked
placeholder (codex POINT 4). So the private alias is *not* the guarantee. The guarantee
is at the assert funnel:
- **`Session.assert_term` rejects any term containing a `.oxsmt.qvar.*` symbol** — a
  one-line prefix walk. This is the load-bearing closer of the coercion escape (and of
  the post-mint injection path below). **Failure mode is a clean verdict, not a crash
  (R1 POINT 4):** a user-supplied placeholder-bearing term degrades to **`Unknown`** via
  the I8 `Unsupported` discipline (a disciplined "you asserted something out of the
  input fragment"), *never* a raw `Failure` exception.
- The manager mints instances only through `Instance.of_subst` (`Instance.t = private
  Term.t`), which applies σ and re-checks no residual `.oxsmt.qvar.*`. Because the
  manager only ever feeds ground bodies, this check firing is an **internal invariant
  violation = a bug**, so *there* a loud `Failure` is correct (a bug-catch, not a
  user-facing path). The two checks share one implementation but have different failure
  modes by design: user door → clean `Unknown`; internal mint → loud `Failure`.
- Optional belt-and-suspenders (tranche 2): the matcher validates a bound `Qvar` belongs
  to the **current lemma** before binding, so a stale/foreign handle cannot even enter a
  substitution.

So "no placeholder reaches the solver" is enforced at the assert funnel by an O(term)
check, not by a whole-program compile guarantee (the residual gap vs a core binder type,
O1 weighs this) — but it is a *verdict-clean* enforcement, closed by construction.

**Why the assert-side gate (not namespace unforgeability) is primary.** R1's env-guard
closes the *declaration* funnels, but two doors stay genuinely public: `Symbol.intern`
interns any name, and `Session.context` hands out the raw `Context` — so a client could
`Symbol.intern` the deterministic placeholder name and build a placeholder-bearing term
directly (pre-mint it fails on a missing rank; post-mint the rank exists). The
`assert_term` `.oxsmt.qvar.*` rejection above closes this the same way it closes the
`Qvar` coercion escape: a placeholder-bearing term never gets past assert, degrading to
a clean `Unknown`. So **the assert-side gate is the load-bearing soundness guarantee;
namespace unforgeability (the env-guard + `Qvar.t` handles) is defense-in-depth** — it
shrinks the ways a placeholder can be *created*, but the gate is what makes *any* leaked
placeholder a clean non-event at the point it would matter. Both land in **tranche 1**
(the gate is a one-line walk; nil cost).

**Intern-table survival is benign, and here is why (M2).** Building `body`/`triggers`
through the session `Context` hash-conses the placeholder-bearing terms into the
shared intern table, and both the intern table and the `Env` symbol set are
**grow-only, never retracted on `pop`** — so placeholder terms/symbols outlive their
lemma's frame in the `Context`. This is a **memory wart, not a soundness issue**:
solver state (SAT/EUF/LIA) only ever sees terms that reach `register_atom` via
`assert_term`, and placeholders never do (only ground `Instance.t`s are asserted);
`get_model` filters `.oxsmt.*` (`build_model`'s `keep`, `session.ml:223`); unsat cores
are over asserted `Lit`s; the corpus cache key is over assertions. So a surviving
placeholder term can influence nothing observable. (This is the positive statement of
what was O1(c)'s open "leaning: sound" — the reason is *never registered*.)

The full comparison against binder nodes in `Term` (option B), and the recommendation,
is **O1** — the section the re-briefed reviewers will attack.

### 1.2 Where the store lives (NORMATIVE)

`smt/ematch/` is a **new library above the theories, beside `interface/`**,
depending on `core` only plus a read-only e-graph *view* (§5, L2). It contains the
lemma store, the trigger index, the matcher, and the instantiation manager. The
`Session` (`smt/interface`) owns one `Ematch.manager`, threaded alongside its
`Context`/`Cdclt`. This placement is exactly R-EM4 ("the instantiation loop lives in
an E-matching manager *above* the theories"): the manager is **not** a `THEORY`, is
**not** composed into `Combine`, and does **not** touch the frozen seam. It reaches
the e-graph only through the additive read-only view of §5.

### 1.3 How lemmas enter (NORMATIVE)

Two funnels, one store:

1. **Dedicated API (primary; what the refinement checker calls). Mint-before-build,
   binder-builder form (R1):**
   ```
   Session.assert_lemma :
     t -> qvars:(string * Sort.t) list
       -> build:(Qvar.t array -> < body : Term.t; triggers : Term.t list list >)
       -> unit
   ```
   The session **mints the `Qvar.t` handles first** (via the cap-gated
   `Env.declare_reserved`, §1.1), hands them to `build`, and the caller constructs `body`/`triggers`
   *using those handles* — so occurrence-binding is **by construction**, not by the
   caller re-spelling a reserved name. The Rev-2 shape (which took an *already-built*
   `Term.t` body, forcing the caller to spell placeholder names — the forgeable path)
   is retired: R1's defect (2) was exactly "the API binds too late." The session
   validates well-sortedness over qvars ∪ declared symbols and adds a `Lemma.t` to the
   store **in the current assertion frame**, recording that frame's selector (§1.5).
   Empty `triggers` requests auto-selection (§5, L4).
2. **SMT-LIB `(assert (forall ((x S) …) body))` with optional `(! body :pattern (…))`
   (corpus ingestion; L7):** handled entirely in the **test-only parser**
   (`smt/smtlib/parser/`, never shipped, ADR-0003 §3). The parser mints `Qvar.t`
   handles, translates the body/patterns against them, and calls `assert_lemma`.
   **`forall` never enters shipped `Term`.** **The parser MUST reject nested/alternating
   quantifiers loudly** (a `forall`/`exists` under a `forall` body, or an un-skolemized
   `exists`) with `Unsupported` → `unknown` (R1) — the placeholder scheme cannot express
   them, so this is O1's flip condition **made mechanical**: out-of-fragment quantifiers
   are refused, never silently flattened into an unsound encoding.

Both paths converge on `assert_lemma`; the store is the single source of truth.

### 1.4 The instantiation loop — one `check_sat` entry, one `Sat` gate (NORMATIVE, tranche 1)

Tranche 1 uses the **outer loop** DESIGN §2 describes verbatim ("each round asserts
ground instances and re-runs the ground core"). **The loop IS `Session.check_sat` —
there is no second entry point** (H1): a client that calls `check_sat` with a lemma
in the store always goes through the loop and the liveness gate.

```
Session.check_sat(session):                 (* THE single public entry point *)
  budget := fresh Budget.t                    (* deterministic, debited DURING matching (R4) *)
  loop:
    v := ground_check(session)               (* ordinary CDCL(T) + model build; see the
                                                Sat-arm note below — ALL Sat sub-arms
                                                funnel here, none returns to the client *)
    match v with
    | Unsat   -> return Unsat                  (* SOUND: instances are valid consequences *)
    | Unknown -> return Unknown                (* poison / Incomplete / split-budget, sticky *)
    | Sat ->
        if store has no live lemma then return Sat   (* the ONLY Sat exit to the client *)
        (* Ematch.round STREAMS: it debits `budget` per candidate/join/subst/assert
           INSIDE enumeration (R4), round-robin across live lemmas (R8), and stops the
           instant the budget is spent — never materializing a whole N² round first. *)
        insts := Ematch.round(manager, egraph_view, store, budget)   (* §5 L3/L5 *)
        if Budget.exhausted budget then return Unknown   (* §3: budget, sticky *)
        if insts = [] then return Unknown       (* SATURATED but quantifier live: SOUNDNESS RULE *)
        for i in insts (deterministic order):
          (* assert_term_at_frame is PRIVATE (session.ml) and takes an Instance.t, not a
             public Term.t (R2/codex POINT 6): it guards by the lemma's selector var. *)
          assert_term_at_frame(session, ~frame:(lemma_of i).frame, Instance.of_subst i)
        continue                               (* re-run the ground core, now incremental *)
```

Load-bearing properties:

- **Instances carry their generating lemma's frame selector — NOT the base frame,
  NOT the current innermost frame (R2, the merged C1 + codex-code-half fix).** An
  instance `φ[σ]` is a valid consequence *of its lemma only* (§0), so it must be active
  exactly while its lemma is. Two coupled facts:
  - *Spec half (my C1):* the earlier "asserted at the base frame, permanent" claim was
    wrong — it produced a wrong `unsat` when a pushed-frame lemma's instance outlived its
    `pop` (H-PUSHPOP, §7). Root cause: conflating CDCL "decision level 0" with push/pop
    "base frame."
  - *Code half (codex, VERIFIED):* the mechanism §1.5 assumed **does not exist today** —
    `assert_term` always guards with the **innermost** selector
    (`current_selector = List.hd t.frames`, `session.ml:127`, applied in
    `assert_clausified` `session.ml:167/175`). So a naïve loop would assert an instance
    under whatever frame is active, not the lemma's.
  - **Fix:** each `Lemma.t` records its owning frame's **selector var** (§1.1 `frame :
    Sat.var`); a new **PRIVATE** (`session.ml`, not a public `session.mli` entry — codex
    POINT 6) **`assert_term_at_frame ~frame:(v:Sat.var) (i:Instance.t)`** asserts the
    instance guarded by **the lemma's** selector (`Sat.neg v` prepended, well-typed since
    `Sat.neg : var -> lit`, `sat.mli:29`), *not* `current_selector`. It takes an
    `Instance.t` (not a `Term.t`) so it cannot be used to bypass the `Instance` gate or
    select an arbitrary SAT var. `Session.pop` of the lemma's frame disables that
    selector, deactivating the lemma **and every instance drawn from it, together**. A
    base-frame lemma's selector is always-active (instances effectively permanent); a
    pushed-frame lemma's instances retract with it.
  - *Why not the current selector:* asserting via ordinary current-frame `assert_term`
    while a frame *inner* to the lemma's is active would retract the instance too early
    when that inner frame pops — a completeness loss.
- **Instance-dedup is scoped to active-clause lifetime, not to permanent body identity
  (R2, codex).** A dedup keyed only on the instance body and *never cleared* suppresses
  re-generation after the instance's clause has been deactivated by a `pop` — so a later
  equivalent lemma that should re-derive the instance is starved and the goal goes
  `unknown` instead of `unsat`. (Codex noted the Rev-3 base-lemma repro did **not**
  exercise this — a base lemma's instance is never deactivated by an inner `pop`; the
  real test is a *pushed* lemma popped, then a *new equivalent* lemma — H-REPEAT-REFUTE,
  §7, rewritten accordingly.) **Fix:** the dedup cache drops an entry when its instance's
  clause is deactivated (frame `pop`), so a retracted instance is **re-generated**, not
  suppressed. H-REPEAT-REFUTE (§7) pins this; it is a distinct degenerate from H-PUSHPOP.
- **Invariant (i) is discharged by the level-0 argument, on a different axis than
  push/pop (M4).** ADR-0010 invariant (i)'s "grow-only, retraction-free … never at the
  current decision level" concerns the **CDCL search trail** (backtracking-stranding),
  a distinct axis from push/pop assertion-frame depth. The loop asserts instances only
  *between* complete `ground_check` runs, when the trail is at **decision level 0** with
  no live search decision — so no instance is stranded by a backjump, which is exactly
  what invariant (i) guards. Push/pop retraction (the selector mechanism above) is the
  orthogonal concern; conflating the two axes is what produced the C1 defect.
- **Incremental, not restart.** Asserting after `check_sat` is first-class
  (assert-after-check, DESIGN §5); learned clauses and theory state persist. A round
  adds clauses and resumes; it does not reset the solver.
- **The combination seam is honored, not bypassed.** Each `φ[σ]` flows through
  preprocess → clausify → `register_atom` → `Combine` identically to a user assertion.
  New boundary nodes an instance introduces (e.g. `f(cons(x,xs))` inside `≤`) enter
  `Combine`'s interface set via ADR-0010's occurrence-monotone rule — the fresh
  `Le`/`App` parent nodes carry new tags, so the assert-time interface walk (re-run
  per `assert_term`, idempotent per `register_term` C7, `euf.mli:47-53`) sees the new
  crossings directly; the placeholder body was never asserted, so it registered nothing
  the walk could have "already" satisfied (M2). "Handled by the assert-time pipeline
  like any other assertion" (ADR-0010 invariant (iv), §3.4). No combinator change is
  required by this ADR. Invariant (ii) is discharged by *that unchanged combinator's*
  occurrence-monotone machinery (ADR-0010 §3.4 P6), not by the tier — the frame-scoped
  trigger/dedup caches of §1.5 are an analogy to invariant (ii)'s idempotence, not its
  discharge (M4).

*Alternative considered — in-search instantiation via `Combine` returning
`Split [instance-clause]` at `Final` (the frozen seam explicitly permits it: D5
"`Split … E-matching lemma`).* Deferred to a later tranche (§4 O2). It is the
higher-performance path real solvers use, but it (a) requires the manager to live
inside `Combine` (the only THEORY with e-graph access), coupling matching to the
combinator; (b) demands the full grow-only-under-backtracking treatment (ADR-0010
invariant (i) registry mutants — the split clause is permanent, but its
`register_atom` bookkeeping must not be stranded by a backjump over the
instance-creation branch). Tranche 1 buys correctness and determinism cheaply by
staying at frame scope; §4 O2 gates the in-search upgrade on measurement.

### 1.5 Push/pop scoping (NORMATIVE)

A lemma is added to the store in the current assertion frame and records that frame's
selector. On `Session.pop`, the lemma **and its generated instances are retracted
together**, because each instance was asserted guarded by the lemma's selector
(§1.4): popping the frame disables that selector, deactivating the lemma's clauses and
every instance drawn from it in one step. This is *soundness-load-bearing*, not just
hygiene — keeping a pushed-frame instance alive after its lemma's `pop` is the C1
wrong-`unsat` (the instance is a consequence of the now-retracted lemma; H-PUSHPOP,
§7, pins it). The store itself is frame-scoped the same way: a `pop` removes the
lemmas added in that frame from the live set, so they stop being "live" for the
soundness rule (§2). The trigger index and the per-lemma instance-dedup cache (§5 L5)
are likewise dropped at the frame's `pop` (grow-only within a frame; this is an
*analogy* to ADR-0010 invariant (ii)'s idempotent-registration discipline, not its
discharge — invariant (ii) governs the combinator, M4/§1.4).

---

## 2. THE SOUNDNESS RULE and its honeypot (NORMATIVE — the center of the ADR)

> **THE SOUNDNESS RULE.** While any lemma is **live** — present in the active
> assertion scope and not proven to be fully instantiated — a `check_sat` result of
> `Sat` is reported to the client as **`Unknown`**. `Unsat` is reported unchanged
> (sound, §0). `Unknown` stays `Unknown`.

"Live" is deliberately coarse in tranche 1: **any lemma in an active (unpopped)
frame is live.** (A future completeness upgrade may declare a *local* lemma at
tier-1 saturation "discharged," licensing a real `Sat`; that requires a fragment
decidability argument and is explicitly out of scope — §4 O7.) The rule composes
with, and does not replace, the existing session degrades: it sits beside (a) the
ADR-0010 `Combine.Incomplete` degrade (structured Bool compound under UF), (b) the
CONTRACT-POISON firewall, and (c) the split-budget degrade. Same "degrade, never lie"
pattern as M1's original theory-atom rule; the lemma-liveness degrade is `unknown` #4.

**Placement — an unconditional wrapper over EVERY `Sat`, one entry point (H1+H2).**
The rule is **not** a per-arm edit; it is the single `Sat`-returning gate of the
`Session.check_sat` loop (§1.4). Every way the ground check can produce `Sat` funnels
through that one gate:

- the trunk table-free `Sat` arm (`build_model → Some m ⇒ Sat`, `session.ml:269-273`), and
- **the uf-models function-table `Sat` arm landing in parallel** (`task/uf-models` #61
  rewrites exactly this arm to add a second `Model_check`-gated `Sat` sub-path,
  `worktrees/uf-models/…/session.ml:296-316`), which consults **no live-lemma flag** of
  its own.

Because the two features both restructure this arm, the liveness degrade **must** be
expressed as a wrapper over the loop's `Sat` result — the loop returns `Sat` to the
client from exactly one line (`store has no live lemma`, §1.4) — **not** bolted onto
one sub-arm. If it were attached only to the table-free arm the draft can see today, a
function-table `Sat` would leak to the client while a quantifier is live → wrong `sat`
(unsound under E-matching incompleteness, §0). There is **one** public entry
(`check_sat`, no `check_sat_with_lemmas`), and the reviewer's check is precise: no code
path returns a `Sat` — from *either* model-build arm — to the client while `store`
holds a live lemma. This ADR's landing must therefore coordinate with #61 (whichever
lands second rebases onto the unified `Sat` gate). **Cross-flag #88** (uf-models
`t.asserted` is grow-only and evaluates retracted assertions) — the same
push/pop-provenance hazard as R2 in a neighboring feature; whoever owns the unified
`Sat` exit should own both.

**Honeypot H-SOUND (proves the rule fires; NORMATIVE, mirrors `degrade_*`).**
A file that is ground-satisfiable with a live quantifier, whose instances never
refute it:

```smt2
(set-logic UFLIA)
(declare-fun f (Int) Int)
(assert (forall ((x Int)) (> (f x) 0)))   ; :pattern ((f x))
(declare-const a Int)
(assert (= (f a) 5))                       ; ground: consistent with the lemma
(check-sat)                                ; :status sat  (a complete solver would say sat)
```

Golden verdict: **`unknown`** (the lemma is live; instantiation of `(f a)` yields
`(> (f a) 0)`, consistent, no refutation → ground core `Sat` → degraded). A
regression that reports `sat` flips the golden label-check **red** — the same
mechanism as the committed `degrade_*.smt2` honeypots (`tests/cases/`, STATUS §8).

**Honeypot H-REFUTE (proves the rule does not over-degrade genuine `unsat`;
NORMATIVE).** The dual: instantiation *does* close the goal, so `unsat` survives.

```smt2
(set-logic UFLIA)
(declare-fun f (Int) Int)
(assert (forall ((x Int)) (> (f x) 0)))   ; :pattern ((f x))
(declare-const a Int)
(assert (< (f a) 0))                       ; contradicts the instance (> (f a) 0)
(check-sat)                                ; :status unsat
```

Golden verdict: **`unsat`** (instance `(> (f a) 0)` + `(< (f a) 0)` is
LIA-inconsistent). This pins that the soundness rule is not "always `unknown` when a
quantifier is present" — a degenerate rule that would make the tier useless *and*
pass H-SOUND. Both honeypots are required; neither alone is sufficient (H-SOUND
alone is satisfied by a solver that never instantiates; H-REFUTE alone is satisfied
by an unsound solver that guesses `sat`).

---

## 3. Failure-direction analysis (NORMATIVE — required per mechanism)

Every mechanism's failure resolves to `unknown`, never to a wrong verdict. This
table is the soundness backbone and a mutation-testing target (DESIGN §10).

| mechanism | failure mode | direction | how bounded |
|---|---|---|---|
| matching loop / generative explosion (assoc axiom, L7) | unbounded instance generation | `unknown` | **generation budget debited INSIDE enumeration (R4)** — a deterministic step budget charged per candidate/join/substitution/assertion *during* `Ematch.round`, not after a materialized round; on exhaustion the loop stops with a live lemma → `unknown`, never hangs (H-LOOP + H-XPROD, §7) |
| conjunctive multi-trigger cross-product | a single round materializes N² substitutions before any round-level check | `unknown` | the per-step budget (R4) fires **mid-enumeration**, before the N² set is built — a round-level cap would not (H-XPROD, §7) |
| mutual generation across lemmas (tier-2, R5) | two "individually non-generative" lemmas loop through each other's triggers | `unknown` | **every tier is budget-bounded (R5)** — `local`/non-generative is scheduling metadata only, never a budget exemption, unless non-generation is proved on the *transitive* trigger-dependency graph (H-MUTUAL, §7) |
| budget starvation across lemmas (R8) | a generative lemma spends the whole budget before a cheap refuting instance from another lemma fires | `unknown` (completeness loss, not unsound) | **deterministic round-robin fairness / per-lemma quotas (R8)** before spending shared overflow — bounds the loss; the verdict stays sound |
| saturation with ground `Sat` | round produces no new instances but a lemma is live | `unknown` | SOUNDNESS RULE (§2): live lemma ⇒ degrade |
| redundancy filter (L5) | drops an instance that was actually needed | `unknown` (completeness loss) | never unsound — a missing valid consequence can only fail to refute, never create a wrong model |
| trigger selection (L4) | auto-trigger too restrictive / too permissive | `unknown` (loss) / slower | invariant (iv): trigger discipline is heuristic coverage, not completeness; extra valid instances cannot mislead; runaway caught by the R4 budget |
| substitution (mint / assert side) | a forged/leaked/coerced placeholder reaches the solver | **caught at the assert gate (R1)**: forge doors closed (`Env.declare_fun`+`declare_sort` reject `.oxsmt.*`), mint is cap-gated; and `assert_term` rejects any `.oxsmt.qvar.*` term → clean `Unknown` (the load-bearing closer, incl. the `Qvar` coercion escape); `Instance.of_subst`'s internal re-check is a bug-catch (`Failure`) |
| e-graph view (L2) | matcher mutates the e-graph | **impossible (R6)**: the view exposes only non-registering accessors (`find_class_opt`/`equal_if_registered`/`class_members`); the registering `are_equal`/`class_of` are NOT on the view |
| budget interaction | lemma rounds + LIA B&B splits both diverge | `unknown` | termination claimed **per ground check only** (ADR-0010 invariant (iii)); global termination across rounds is explicitly *not* claimed; the single deterministic generation budget (R4/R5/R8) is the sole cross-round guarantee |

The single mechanical invariant the reviewer should extract: **there is no code path
in the lemma tier that upgrades a verdict** (nothing turns `unknown`→`sat` or
`sat`→`sat`-past-the-rule, and nothing turns a non-refutation into `unsat`).
Instantiation only *adds ground clauses*; the ground core's soundness does the rest.

---

## 3a. Lemma representation — merits comparison (NORMATIVE; freeze-avoidance excluded per DESIGN A5)

DESIGN A5 rules that a design must not contort itself to stay off a frozen surface,
and that the choice is argued on **correctness-by-construction, enforceability,
extensibility** with freeze-avoidance carrying no weight. A5 names "reserved
namespaces standing in for what a type should express" — exactly option (A) — so this
comparison is mandatory and is the ADR's central argued decision.

The two options both represent `∀x̄. φ(x̄)` and instantiate it by substitution; they
differ in *where the quantifier structure lives*.

- **(A) Placeholder constants (outside `Term`; §1.1).** ∀-variables are fresh
  reserved-namespace nullary constants; `body`/`triggers` are ordinary ground-shaped
  `Term.t`; instantiation is constant-for-term substitution.
- **(B) Binder nodes in `Term` (the R-EM4 unfreeze).** Add `Forall of binder * t` and
  `BoundVar of int (* de Bruijn *) * Sort.t` to the frozen 9-node set; instantiation
  is capture-avoiding substitution / de Bruijn shift.

**Correctness-by-construction.**
- (B) wins on the two classical binder hazards: **capture-safety** (de Bruijn makes
  it structural) and **alpha-equivalence** (alpha-equal lemmas hash-cons to one node,
  so `Term.equal` *is* alpha-equality and lemma dedup is free). Under (A) both are
  matters of discipline: distinct fresh placeholders per lemma, and no alpha-canonical
  form (alpha-equal lemmas are distinct `Lemma.t`).
  **But — decisive for the target fragment — both hazards are *vacuous* under
  single-level ∀ over a ground body.** Capture arises only when a substituted term
  contains a variable bound by an *enclosing* quantifier; with no nesting there is no
  enclosing binder, so nothing can be captured. Alpha-equivalence dedup matters only
  when the same lemma is stated twice with renamed variables; the VC generator emits
  each lemma once (O5/O9). So (B)'s correctness-by-construction edge buys safety
  against hazards that **cannot occur in the fragment this tier targets**. It becomes
  load-bearing precisely at the flip condition (nested/alternating ∀), and O1 makes
  that the STOP trigger.
- (A)'s one genuine correctness surface — a placeholder leaking into the solver — is
  closed at construction by the typed `Instance.of_subst` gate (§1.1): a
  `private Term.t` mintable only through a check that rejects any residual
  `.oxsmt.qvar.*`. This is construction-time enforcement *at the one funnel where open
  terms could escape*, not whole-program.

**Enforceability (the strongest point for B, weighed honestly).**
- (B) makes leak prevention a **compile-time** guarantee across *all* ground code: if
  `preprocess`/`cnf`/`euf`/`lia`/`combine`/`model` match `node` exhaustively (no
  catch-all), adding `Forall`/`BoundVar` forces every one to handle-or-reject a binder
  — a term reaching a theory is statically ground, or the build fails. That is
  strictly stronger than (A)'s single checked funnel.
- The honest counter: the guarantee's *scope* is a single funnel (only
  `Session.assert_term` ingests instances), and (A) covers exactly that funnel with an
  O(term) check already dominated by the preprocessing walk. (B) pays for a
  whole-program guarantee by **smearing a stage-2 concept across all stage-1 ground
  code permanently** — every existing and future `node` match, including the EUF/LIA
  hot paths, carries two dead cases forever. For a one-funnel hazard, (A)'s checked
  boundary is a *proportionate* defense; (B)'s is over-provisioned for the risk. Net:
  (B) is more enforceable, (A) is sufficiently enforceable at far lower blast radius.

**Extensibility.**
- (B) extends cleanly to nested ∀, alternation, and higher-order-ish measure shapes —
  de Bruijn handles arbitrary binding depth. (A) does not: nested quantifiers need
  ad-hoc encoding (skolemization at VC-gen, or flattened placeholder scopes) that gets
  ugly fast. **This is (B)'s real, non-vacuous advantage** — but it is advantage for
  *features not on the roadmap* (measure-style refinement lemmas are prenex
  single-∀). Paying (B)'s permanent cost now to buy extensibility we may never use is
  the wrong trade; O1's flip condition buys it exactly when demanded.

**Total machinery / layering (the strongest point for A).**
- (A) keeps `Term` a **pure ground-term type** — the thing stage-1 froze and every
  theory matcher reasons about — and confines all quantifier structure to the
  stage-2 `smt/ematch/` module. DESIGN §2 stages the solver as *ground core first,
  quantifiers second*; (A) mirrors that staging in the type structure, (B) erases it
  by putting a stage-2 concept in the stage-1 core. ADR-0003 explicitly kept the
  constructor set "minimal and canonical … load-bearing for client matchers"; (B)
  permanently grows it for all clients, (A) does not. Substitution-as-rebuild over
  placeholders is also simpler code than de Bruijn shift/capture-avoidance.
- Engineering cost of (B) (admissible as cost, not as freeze-virtue): the unfreeze
  ritual on the most load-bearing frozen file; new cases in the hash-cons smart
  constructors, `Term.Debug.check` (binder well-formedness / index-in-range), the
  shipped printer (SMT-LIB `forall` rendering — the gate emits it), and the reader;
  and a new invariant (a `BoundVar` carries a sort but no binding context at
  construction). This is real work but is *cost*, not a merits argument, per A5.

**The forgeability CRITICAL (R1) reshapes enforceability — and forces a decision on
the frozen surface (dual review, both legs).** Dual review found a placeholder is
**forgeable**: `Symbol.intern` is a process-global by-name table (`symbol.mli:19-22`),
`Session.env` hands out the raw `Env` (`session.mli:99-102`), and `Env.declare_fun`
raises `Reserved_symbol` only for `div`/`mod`, **not** the `.oxsmt.*` prefix
(`env.mli:30-33`). So a client can `Env.declare_fun (Session.env s) ".oxsmt.qvar.0.0"`,
intern that name globally, and collide with a placeholder the tier later mints —
codex's verified wrong-`unsat` repro. This is a **concrete instance of the exact A5
anti-pattern** ("reserved namespaces standing in for what a type should express"): a
by-name reserved placeholder is forgeable *because* identity is a global name. Under
(B) a de Bruijn `BoundVar` is unforgeable **by construction** — not a name, not
interned, undeclarable — so (B) gets that enforceability property for free, whereas (A)
must add machinery (R1) to recover it. The reconciler framed this as a hinge: *if the
robust unforgeability fix must touch the frozen `Env`, (A) forfeits its off-frozen /
less-machinery claim and the calculus tips to (B).*

**Hinge — RULED (design author, A5): (A) with a scoped `env.mli` unfreeze.** The
tipping logic quietly re-imports freeze-weight into a scale A5 cleared, in **both**
directions: losing "zero frozen touch" costs (A) *nothing*, because that was never
admissible weight — the comparison is machinery-vs-machinery on merits. On those
merits, **(A) + the env-guard is still smaller and better-layered than (B)**:
- (A)'s R1 fix = **one scoped frozen change** — centralize `.oxsmt.*` rejection in the
  public `Env.declare_fun`/`declare_sort` so the public doors reject *by construction*
  (what the A5 anti-pattern note demands), plus a **capability mint door** (abstract
  `reserved_cap` from `Env.create`, `declare_reserved`, threaded session-side —
  buildable across library boundaries, unlike the non-building `private_modules` sketch,
  §1.1), plus the mint-before-build binder-builder API and the `assert_term` gate
  (§1.1/§1.3, R1).
- (B) = a binder node into the **stage-1 core** — `term.mli` unfreeze + hash-cons +
  `Debug.check` + printer + reader + a new case at **every** ground `node` match site,
  permanently — to buy capture/alpha properties the reviewers verified are **vacuous in
  the fragment**.
One scoped `env.mli` unfreeze (a door that *rejects*, at a symbol-declaration choke
point preprocess already depends on) is far less machinery than a binder in every
ground matcher forever, and keeps `Term` a pure ground type. Run the §10 unfreeze
ritual for `env.mli` at implementation time — A5: "run the ritual and take the better
design." The `env.mli` delta is the tier's **one** frozen-surface touch (§6).

**Recommendation: (A) placeholder constants + scoped `env.mli` unfreeze, for the
single-level-∀ target fragment (RULED).** (B)'s decisive advantages (capture-safety,
alpha-equivalence) are **vacuous in the fragment** (both review legs concur); its
genuine advantages (unforgeability, compile-time leak enforcement, nested-∀
extensibility) are recovered by (A) with modest, *scoped* machinery (unforgeability via
the env-guard + unforgeable `Qvar.t` handles, R1; leak enforcement via the `Instance`
gate) or buy roadmap-absent features at permanent whole-program cost. **This
recommendation flips to (B) at O1's stated condition** (nested/alternating ∀), and the
ADR commits to the R-EM4 unfreeze then rather than encoding around it (A5). Reviewers:
the "hazards are vacuous" and "single funnel" claims are the remaining attack surface;
R1's env-guard + `Qvar.t` handle is now the answer to the forgeability line.

---

## 4. Open questions (numbered; the ratification checklist)

- **O1 — lemma representation: RULED (§3a).** The hinge (does R1's unforgeability fix
  touch the frozen surface?) is **ruled by the design author: (A) placeholders + a
  scoped `env.mli` unfreeze**, for the single-level-∀ fragment. Rationale in §3a:
  freeze-weight is inadmissible in both directions (A5), and on machinery-vs-machinery
  merits (A)+env-guard (scoped frozen doors that *reject*, + a capability mint
  door, + unforgeable `Qvar.t` handles, + the `assert_term` gate) is smaller and
  better-layered than (B)'s binder in every stage-1 ground matcher. **Flip condition,
  normative (unchanged):** nested/alternating ∀, ∀-under-∃ not skolemized at VC-gen, or
  a genuine alpha-dedup need → **STOP and run the R-EM4 `Term` unfreeze** (§3a). A
  design trigger, not a freeze-avoidance dodge.
- **O2 — instantiation locus: outer-loop (frame scope) vs in-search (`Split`).**
  Tranche 1 is outer-loop (§1.4). Is level-0/top-level matching too weak for real
  VCs whose trigger terms appear only under a case split? Measure on the L7 corpus;
  if the outer loop leaves solvable UNSATs unrefuted, promote in-search `Split`
  instantiation (with the full ADR-0010 grow-only-under-backjump treatment) as a
  later tranche. Gate: corpus evidence, not preemptive.
- **O3 — e-graph view side-channel.** The manager (above theories) needs EUF's
  e-graph, but EUF is behind the frozen `THEORY` inside `Combine`. Options: (a)
  `Combine` exposes an additive read-only `euf_view` accessor (non-frozen —
  `combine.mli` is not in `FROZEN.sha256`); (b) the session holds a direct EUF-engine
  reference alongside `Combine`. Leaning (a): keeps a single owner of the e-graph and
  does not duplicate registration. Confirm it does not violate ADR-0010's "no e-graph
  hub" (it is a *read* view, not an exchange hub). Pin against the landed `Combine`.
- **O4 — budget shape (RESOLVED in outline by R4/R5/R8; §3 rows).** A **single
  deterministic step budget debited INSIDE matching** (per candidate/join/substitution/
  assertion, R4) — *not* a post-round cap, which a conjunctive multi-trigger's N²
  cross-product would blow past. It binds **every tier** (R5 — `local`/non-generative is
  scheduling metadata, never a budget exemption) and is spent under **deterministic
  round-robin fairness / per-lemma quotas** (R8) so one generative lemma cannot starve a
  cheap refuting instance from another. Reported via a new `Session.lemma_stats`,
  distinct from `splits`. Residual: the exact quota/round-robin schedule and step-cost
  weights (I6-deterministic; pin against the L7 corpus).
- **O5 — redundancy filter granularity.** Dedup by hash-consed instance `body` (two
  substitutions yielding the same ground term) vs by `(lemma_id, σ)` vs by "instance
  already entailed by the current e-graph." Leaning: hash-consed body (cheap, exact,
  O(1) via tag) for tranche-2; entailment-based filtering is a tier-1 optimization
  (L5) measured later.
- **O6 — instantiation certificate leaf (RESOLVED; §5 L6).** The
  `Instance { lemma_id; subst }` leaf shape is **settled with the certificates ADR**
  (`logs/adr-certificates-draft.md §1.6`; its q6 closed against this shape, field name
  reconciled to `lemma_id`): positional `subst` in qvars order; formula-level
  introduction; clausification rides the existing preprocessing TCB;
  substitution-check only, matcher untrusted; off-core in `smt/certificate/` with
  **no `Rule_tag` payload** and no `explanation.mli` change; Lean replay via
  `specialize`. Staging per master ruling: the `Instance` class + its checker + a
  wrong-substitution honeypot are reserved/built at M5 (oracle-first); **emission
  lands with the lemma tier (tranche 4 / L6)**, not at M5. No open sub-question
  remains on the ADR-0006 side.
- **O7 — completeness upgrade (local lemmas).** Can a lemma flagged `local` (L5),
  once tier-1 (non-generative) instantiation saturates, license a real `Sat` (upgrade
  `unknown`→`sat`)? This requires a decidability argument for the fragment
  (local-theory-extensions style). **Out of scope for L1–L6**; recorded so the
  coarse liveness rule (§2) is understood as the conservative floor, not a ceiling.
  The eventual soundness obligation is a proof, not a heuristic.
- **O8 — global termination.** Explicitly *not claimed* (ADR-0010 invariant (iii):
  per-ground-check only). The single deterministic generation budget — debited **inside**
  matching (R4), binding **every** tier (R5), spent under fairness (R8) — is the sole
  cross-round termination guarantee; **no tier runs unbounded.** Confirm it is the only
  thing between the assoc axiom (H-LOOP), the N² cross-product (H-XPROD), and the mutual
  loop (H-MUTUAL) and a hang, and that it is deterministic.
- **O9 — multi-trigger / multi-pattern semantics.** A multi-trigger (conjunctive:
  all patterns must match under one σ) vs alternative triggers (any one suffices).
  v1 scope: support both (`triggers : Term.t list list` — outer = alternatives, inner
  = conjunctive), matching real SMT-LIB `:pattern`. Confirm the matcher (§5 L3) and
  the auto-selector (§5 L4) agree on the shape.
- **O10 — determinism, and it is verdict-affecting under a budget (R7).** Term tags are
  allocation counters, not hash-bucket positions (`node.ml:105`), so a pinned
  construction schedule gives fixed tags — determinism is achievable. Normative riders:
  (a) **sort after every set/dedup/`Hashtbl` boundary** — the trigger index keyed on head
  `Symbol.t` must iterate by `Symbol.name`, never raw id / `Hashtbl` traversal (ADR-0005
  C8, the top nondeterminism risk); (b) because the generation budget can **cut a round
  mid-way** (R4), instance/round order changes **`Unsat` vs `Unknown`** — so I6
  determinism is *semantically* load-bearing here, not cosmetic. The run-twice test (§7)
  must assert byte-identical verdict **and** counters under a **tight** budget (one that
  actually cuts), not only an unbounded run.

---

## 5. L2–L6 interface sketches (shapes pinned, mechanism to the implementing tranche)

### L2 — genuinely non-registering e-graph query API (shared with the EUF ladder E1)
An **additive, genuinely read-only** query surface on the EUF engine (`euf.mli` is
*not* frozen — only `core/*.mli` are, §6), exposed to the manager via the O3 view.

**The obvious reuse mutates — do NOT use it (R6, codex VERIFIED).** `are_equal`/
`class_of` **register-if-new** (`are_equal t a b = find t (register t a) = find t
(register t b)`; `euf.ml:604-605`; `euf.mli:117` even documents "registers them if
new"). Calling them from the matcher — even a benign `are_equal a a` consistency check
— would **grow the e-graph**, violating both the §3 read-only failure guarantee and
the A4 exact-membership rule. So the view must add **new non-registering accessors**:

- `app_terms_by_symbol : view -> Symbol.t -> Term.t list` — ground `App` terms with a
  given head, for trigger-root candidates. **Deterministic order: registration /
  `Term` tag** (never `Hashtbl` traversal, C8). This is R-EM3.
- `find_class_opt : view -> Term.t -> class_id option` — the class of a term **iff
  already registered**, `None` otherwise (no mutation).
- `equal_if_registered : view -> Term.t -> Term.t -> bool` — congruence-equality check
  that treats an unregistered term as its own **singleton class (tag-equality only)**,
  never registering.
- `class_members : view -> Term.t -> Term.t list` — members of a term's congruence
  class, tag-ordered, for matching **modulo EUF-congruence equalities**; an absent term
  is a singleton.

An **absent term is defined as a singleton class matched by tag-equality only** — so
both boundary directions stay sound: a stale or missing class yields either a valid
universal instance or no instance, never a wrong refutation (§3).

**Completeness scope of the view (M3, degrade-safe).** Two limits, both
completeness-only (fewer matches → fewer instances → at worst a missed refutation →
`unknown`, §3 — never unsound), stated so tranche-2's implementer is not surprised:
(1) "modulo the current equalities" is precisely **modulo EUF-congruence equalities**;
`class_of`/`are_equal` are EUF-only and **blind to LIA-entailed equalities** (e.g.
`c = a+b` derived in simplex never merges EUF classes unless the seam propagates it),
so a LIA-equality-dependent instance may never be generated. (2) Under the A4-erratum
membership rule landing in `task/euf-perf` (#65), a **pure-arithmetic term that never
sits under a UF symbol is never `register_term`'d and has no e-node**, so it is
invisible to `class_members`. This is fine for what the matcher needs — trigger *roots*
(UF apps) and their *argument subterms* (boundary nodes) are all UF-adjacent, all
registered — so structural root matching works; only equality-reachability to a
pure-arith term is lost. Tie: pin the view against the landed `task/euf-perf` e-graph.

The **internal merge-listener hook** (TODO E1: "with internal merge-listener hook
placed for datatypes") is the seam for *incremental* re-matching (fire matching when
a new merge activates a trigger) and is shared with datatypes; tranche-2 matching is
**batch** (match the whole index each round), so the hook is placed but not consumed
by the lemma tier until in-search instantiation (O2). The view exposes **no mutating
operation** — the failure-direction table (§3) requires the matcher cannot perturb
the e-graph.

### L3 — matcher v1 (backtracking, uninterpreted-symbol triggers, DETERMINISTIC)
Standard backtracking E-matcher. For each alternative trigger, match its
conjunctive patterns against ground terms: a pattern's root `App(f, p̄)` matches a
candidate from `app_terms_by_symbol f`; recurse on arguments **modulo EUF equalities**
(a pattern argument matches any term in the candidate's class via `class_members`);
a qvar placeholder binds to the ground term (or must be consistent with its existing
binding — via the non-registering `equal_if_registered`/`find_class_opt`, never the
mutating `are_equal`, R6); backtrack on mismatch. A multi-trigger's patterns must share
one consistent σ. Output: a deterministically-ordered set of complete substitutions.
**Every candidate/join/substitution step debits the generation budget (R4)** — the
matcher streams and stops the instant the budget is spent, so a conjunctive
cross-product never fully materializes.
**Determinism:** candidates iterated in tag order; substitutions emitted in a fixed
order derived from (lemma id, trigger index, candidate tags) — a run-twice test pins
it (O10). Triggers contain **only uninterpreted-symbol heads** (ADR-0010 invariant
(iv)); an arithmetic-headed trigger is rejected at `assert_lemma` (arithmetic lives
in the *body*, handled by the assert-time pipeline).

### L4 — trigger selection (annotated-first, auto fallback)
If the lemma carries `triggers` (from `:pattern` or the API), use them verbatim
(annotated-first). Otherwise auto-select: choose minimal subterms of `body` that
(a) together cover every qvar, (b) have only uninterpreted-symbol heads (invariant
(iv)), and (c) are not trivially self-generating (a trigger whose instance rebuilds
the trigger with a strictly larger term — the matching-loop smell; L7). Auto-selection
is a **coverage heuristic, not a completeness claim** (ADR-0010 §3.4 invariant (iv)
caveat) — its failure direction is `unknown` (§3). Determinism: candidate subterms
enumerated in tag order.

### L5 — strategy tiers (redundancy → non-generative → generative), NO tier budget-exempt (R5)
Three tiers, escalating:
1. **Redundancy filter** — never assert an instance already present (dedup by
   hash-consed body, O5), with dedup entries **scoped to active-clause lifetime** (R2,
   §1.4) so a retracted instance re-generates. Bounds a round.
2. **Non-generative** — instances that introduce no new trigger-matchable term. The
   `local` flag marks a lemma as tier-1-preferred; for measure-style lemmas this
   *usually* saturates (`len(cons …)` unfolds finitely over the ground list structure).
3. **Generative** — instances that create new matchable terms (assoc, recursive
   measures) run under the generation budget.

**Critical correction (R5): no tier is budget-exempt.** "Non-generative" analyzed
against a lemma's **own** trigger misses **mutual** generation loops — e.g.
`∀x. P(x)→Q(f(x))` (trigger `P(x)`) + `∀x. Q(x)→P(f(x))` (trigger `Q(x)`), seeded
`P(a)`, generates `Q(f(a)), P(f(f(a))), …` forever while *each lemma looks
non-generative in isolation*. So the hard generation budget (R4) applies to **every**
tier; `local`/non-generative is **scheduling metadata only** (it prefers cheap lemmas
first, R8 fairness), never a budget exemption — unless non-generation is *proved*
against the **transitive trigger-dependency graph** (Leino–Pit-Claudel mutual-loop
taxonomy), which tranche 3 does not attempt. This tightens O8 into a normative "no tier
runs unbounded." H-MUTUAL (§7) pins it.

### L6 — instantiation certificate step (interface agreed with the certificates ADR)
A ground instance is certified by `(lemma_id, σ)`: the Lean goal carries the lemma
as a hypothesis `h : ∀x̄, φ x̄`, and the instance is `h σ` (**"Lean replay via
specialize"**, ADR-0006 §Decision 1 / TODO L6). In the ADR-0006 resolution skeleton
the instance is a **kind-2 valid-lemma introduction** with sub-certificate
`Instance { lemma_id; subst }` — the same class as the ℤ-trichotomy valid-clause
step, introduced (not RUP-derived) and then resolved against by the ground
refutation. **Interface agreed and pinned** with the certificates ADR
(`logs/adr-certificates-draft.md §1.6`, its q6 closed against this shape; field name
reconciled to `lemma_id`). Agreed shape (for ADR-0006's format freeze):

- **`lemma_id` = the store's dense `Lemma.id`**, an index into a certificate-side
  lemma table the emitter records once (id ↦ the quantified formula `∀x̄, φ x̄` +
  the ordered qvar sorts, so Lean can state `h`). The id is already deterministic
  (I6); content-hashing is redundant given the cache keys the whole certificate.
- **`subst` = the ground-term vector in qvars order** (positional, not a keyed map),
  so replay is positional application `h σ₀ σ₁ …` / `specialize h …`. The k-th entry
  is the ground term bound to placeholder `.oxsmt.qvar.<id>.<k>`.
- **Introduced at the FORMULA level, not the clause level.** The step certifies only
  `∀x̄, φ x̄ ⊢ φ[σ]` — a **pure capture-free substitution-application check**
  (`φ[σ] ≟ the emitted instance formula`, re-done independently by the checker over
  hash-cons tags — N-version, uncorrelated with the matcher). The bridge from the
  Bool formula `φ[σ]` to the tokened `Lit`s the SAT core sees is **ordinary
  clausification**, recorded via ADR-0006's `on_input` exactly like a user
  assertion — it rides the **existing Tseitin/preprocessing TCB** (ADR-0006 §Trust
  story) and is **not** re-certified by the `Instance` step. This keeps the
  `Instance` checker to a substitution check and matches, not widens, today's TCB
  boundary.
- **Why-the-trigger-fired is NOT in the TCB.** The checker certifies only that the
  instance is a first-order consequence of the lemma; E-matching heuristics
  (triggers, dedup, budgets) are untrusted (consistent with §3: they only affect
  *which* valid instances appear, never soundness).

Off-core in `smt/certificate/`, no `Rule_tag` payload (O6).

---

## 6. Frozen-surface impact table (NORMATIVE)

**One scoped frozen touch, argued on merits (DESIGN A5).** The tier's **one**
frozen-surface change is `env.mli` — centralizing the `.oxsmt.*` rejection in the public
`Env.declare_fun`/`declare_sort` and adding the capability mint door (R1, the
forgeability fix). This is *not* a freeze-avoidance dodge nor a defeat: A5 clears
freeze-weight in both directions, and on merits these scoped doors-that-reject + the
capability mint door are far less machinery than option (B)'s binder in every stage-1
ground matcher (§3a). Everything else is untouched-frozen or non-frozen-additive. The
env.mli delta runs the §10 unfreeze ritual
at implementation time.

| file | in `FROZEN.sha256`? | this ADR's impact |
|---|---|---|
| `smt/core/env.mli` | **FROZEN** | **SCOPED UNFREEZE (the tier's one frozen touch, R1/§3a).** (i) public `Env.declare_fun` **and** `Env.declare_sort` reject the `.oxsmt.*` prefix — both, since sort/fun share one namespace (POINT 3); (ii) `is_reserved_name`/prefix moves into `Env` as the single source of truth; (iii) a **capability** mint door — abstract `reserved_cap`, `create : unit -> t * reserved_cap`, `declare_reserved : reserved_cap -> …` — threaded session-side to preprocess/ematch (the buildable replacement for the non-building `Env_unsafe private_modules`, codex POINT 1). Runs the §10 ritual. |
| `smt/core/term.mli` | **FROZEN** | **untouched under (A)** (§3a recommendation) — lemmas live outside `Term`. **Under (B):** unfrozen to add `Forall`/`BoundVar` (+ hash-cons, `Debug.check` binder well-formedness, printer `forall`, reader, and a new case in every `node` match); taken at O1's flip condition |
| `smt/core/theory.mli` | **FROZEN** | **untouched** — instances use existing `register_atom`/`assert_lit`; `Split` already names "E-matching lemma" (D5), used only if O2 promotes in-search instantiation |
| `smt/core/atom.mli`, `lit.mli` | **FROZEN** | **untouched** — instance clauses use ordinary `Atom`/`Lit` currency |
| `smt/core/symbol.mli` | **FROZEN** | **untouched** — `Symbol.intern` stays public/as-is (a leaked intern is caught at the `assert_term` gate, §1.1); reservation is enforced one layer up at `Env.declare_fun`/`declare_sort` (both R1 doors), not in `intern` |
| `smt/core/explanation.mli` | **FROZEN** | **untouched** — instantiation certificate routes off-core with no `Rule_tag` payload (O6, resolved with the certificates ADR); no `Rule_tag.Instance` constructor taken |
| `smt/core/model.mli` | **FROZEN** | **untouched** |
| `smt/solver/sat.mli` | freezes Tranche-C (M4) | **untouched by lemmas** — instances use `add_clause`; ADR-0006's `on_input`/`on_unit` (Tranche-C) records them for L6, not a new lemma-tier change |
| `smt/theories/euf/euf.mli` | not frozen | **additive** — non-registering `app_terms_by_symbol`/`find_class_opt`/`equal_if_registered`/`class_members` query API (L2/R6, R-EM3) |
| `smt/combine/combine.mli` | not frozen | **additive** — read-only `euf_view` accessor for the manager (L2/O3) |
| `smt/interface/session.mli` | not frozen | **additive** — `assert_lemma` (binder-builder form, R1); the §2 liveness degrade in `check_sat`; `lemma_stats`; the **`assert_term` `.oxsmt.qvar.*` rejection** degrading to a clean `Unknown` (§1.1, R1 POINT 4). **`assert_term_at_frame` is PRIVATE** (`session.ml`, takes `Instance.t` — POINT 6, NOT a public `.mli` entry) |
| `smt/ematch/*` | new | store, trigger index, matcher, manager, `Qvar.t`, `Instance.t` — `core`-only + the L2 view (I3) |
| `smt/smtlib/parser/*` | not frozen, test-only | **additive** — `forall`/`:pattern` recognition + **nested-∀ rejection** → `assert_lemma` (L7 ingestion) |

Escalation rule (ADR-0010/uf-models precedent): if implementation finds a frozen
surface *beyond* the ruled `env.mli` delta genuinely must change, that is a
**STOP-AND-REPORT to the master**, never a silent addition.

---

## 7. Acceptance criteria (which corpus; NORMATIVE)

Through the **real** Session + Cdclt + Combine + EUF + LIA stack (no mocks):

- **H-SOUND and H-REFUTE (§2)** — the two soundness-rule honeypots, committed to
  `tests/cases/` with `:status`/golden as specified. H-SOUND red on `sat` regression;
  H-REFUTE red on `unknown` regression. Non-negotiable gate. **Tranche caveat (M5):** in
  tranche 1 the matcher is the trivial manual-instances path (§8), so these two
  honeypots exercise the store + liveness degrade + pipeline but **not**
  trigger-finding; **H-REFUTE becomes a matcher test only from tranche 2** (annotated
  `(f x)` against the sole ground `(f a)` is a single deterministic bind — a correct
  matcher finds it, a broken one makes H-REFUTE go red with `unknown`). Do not over-read
  a tranche-1 green as validating the matcher.
- **H-PUSHPOP — the push/pop-provenance honeypot (C1; as load-bearing as H-SOUND,
  NORMATIVE).** A lemma asserted inside a `(push)`, an instance drawn from it, then
  `(pop)`, then a ground assertion that contradicts the (now-retracted) instance:

  ```smt2
  (set-logic UFLIA)
  (declare-fun f (Int) Int)
  (declare-const a Int)
  (push 1)
  (assert (forall ((x Int)) (= (f x) 5)))   ; :pattern ((f x)) — lemma live in the pushed frame
  (assert (> (f a) 0))                        ; makes (f a) a ground trigger → instance (f a)=5 drawn
  (check-sat)                                 ; unknown (live lemma, §2)
  (pop 1)                                     ; retracts the lemma AND its instance (f a)=5
  (assert (= (f a) 7))
  (check-sat)                                 ; :status sat — (f a)=7 alone is SAT
  ```

  Golden verdict of the final `check-sat`: **`sat`** (or `unknown`, never `unsat`). A
  regression that reports **`unsat`** is the C1 bug — a pushed-frame instance strended
  past its lemma's `pop`. This honeypot is the direct test of §1.4's selector-scoping
  fix; without it C1 ships untested (the review's finding). It is the same
  push/pop-provenance family as tasks **#42** (combinator stale-interface skip,
  invariant-(i) tension) and **#88** (uf-models grow-only `t.asserted` evaluating
  retracted assertions) — the eventual reconciliation should treat all three as one
  frame-provenance concern.
- **H-REPEAT-REFUTE — the stranded-dedup honeypot (R2, rewritten per codex; distinct
  degenerate from H-PUSHPOP, NORMATIVE).** The instance must be drawn under a *pushed*
  lemma (so its clause deactivates on `pop`), then a *new equivalent* lemma must
  re-derive it — a **base** lemma's instance would never deactivate on an inner `pop`, so
  the Rev-3 base-lemma phrasing did not test dedup lifetime at all (codex).

  ```smt2
  (set-logic UF) (declare-fun p (Int) Bool) (declare-const a Int)
  (assert (not (p a)))                        ; base goal, survives pops
  (push 1)
  (assert (forall ((x Int)) (p x)))           ; L1 (pushed) → instance p(a) under L1's selector
  (check-sat)                                  ; unsat (p(a) refutes ¬p(a))
  (pop 1)                                      ; L1 + its p(a) clause deactivate; dedup entry for p(a) must DROP
  (assert (forall ((x Int)) (p x)))           ; L2 — new equivalent lemma, base frame
  (check-sat)                                  ; :status unsat — L2 must RE-derive p(a)
  ```

  Golden of the final `check-sat`: **`unsat`.** A dedup keyed on the body `p(a)` and
  never cleared makes L2's regeneration a no-op → `¬p(a)` unrefuted → **`unknown`** (the
  bug). Codex's sharp aside: H-REFUTE alone is passed by an "always-`unsat`" solver, so
  this refutation honeypot is **not** redundant with H-SOUND/H-REFUTE.
- **H-LOOP — the matching-loop honeypot (L7).** The associativity axiom
  `∀x y z. f(f(x,y),z) = f(x,f(y,z))` with a ground seed (`f(f(a,b),c)`): must **hit
  the generation budget and return `unknown`, never hang** (§3, O8). Golden asserts
  `budget_exhausted`-style flag set, verdict `unknown`, and a **deterministic**
  instance/round count (run-twice byte-identical, I6). Exercises a single generative
  chain.
- **H-XPROD — the in-round cross-product honeypot (R4, NORMATIVE).** A conjunctive
  multi-trigger `{p(x), q(y)}` over N ground `p(aᵢ)` and N ground `q(bⱼ)`: the round's
  substitution set is **N²**. Must hit the budget **during** enumeration (before the N²
  set materializes) → `unknown`, never a memory blowup. H-LOOP's single chain does NOT
  exercise this — the budget must be debited *inside* matching (R4), not per-round, and
  only H-XPROD proves it.
- **H-MUTUAL — the mutual-generation honeypot (R5, NORMATIVE).** `∀x. P(x)→Q(f(x))`
  (trigger `P(x)`) + `∀x. Q(x)→P(f(x))` (trigger `Q(x)`), seeded `P(a)`: each lemma is
  non-generative *in isolation*, but together they loop `Q(f(a)), P(f(f(a))), …`. Must
  hit the budget → `unknown`, never hang. Proves R5 ("no tier is budget-exempt") — a
  build that exempts tier-2 from the budget hangs here.
- **Synthetic measure-lemma generator (L7).** Generates the product-shaped workload:
  list-length, tree-size, and similar measures over algebraic-datatype-shaped
  uninterpreted encodings, with both `unsat` goals (instantiation refutes — the
  progress metric) and `sat`-shaped goals (must be `unknown` under §2). The primary
  driver until quantified benchmark families are fetched.
- **UF / UFLIA / AUFLIA SMT-LIB ingestion (L7).** These quantified families are **not
  yet in `corpora/`** (which holds only QF_ sets today — QF_UF/QF_LIA/QF_UFLIA); L7
  fetches them. They are **pre-labeled**, so they carry the fast both-direction
  regression load (DESIGN §8): a pre-labeled `unsat` we call `sat` is ship-stopping;
  an `unsat` we solve via instantiation is progress; a pre-labeled `sat` we call
  `unknown` (forced by §2 while a quantifier is live) is a *completeness* signal
  (files an issue, does not block — DESIGN §8 verdict asymmetry).
- **Determinism regression, verdict-affecting under a tight budget (R7).** Run-twice
  byte-identical verdict **and** counters, exercising O10 (the `Symbol`-keyed trigger
  index). Because the budget can cut a round mid-way (R4), instance/round order flips
  `Unsat` vs `Unknown` — so the test must run under a **tight budget that actually cuts**,
  not only an unbounded run; a hash-order nondeterminism that only surfaces under
  budget-cutting would otherwise pass.
- **Forgeability regression (R1, NORMATIVE).** Codex's repro must be red: a client that
  does `Env.declare_fun (Session.env s) ".oxsmt.qvar.0.0" …` must be **rejected**
  (`Reserved_symbol`) at the public door; a build that lets it through and then
  wrong-`unsat`s the repro (`∀x.p(c)`, `c = .oxsmt.qvar.0.0`, `c=0`, `¬p(1)`) fails this.
- **Mutation targets (DESIGN §10).** Seeded faults the tiered suite must catch: drop the
  liveness check (H-SOUND green-wrong → caught); off-by-one the generation budget (H-LOOP
  hangs → caught); move the budget debit from inside enumeration to per-round (H-XPROD
  OOM/hang → caught); exempt tier-2 from the budget (H-MUTUAL hangs → caught); base-scope
  the dedup (H-REPEAT-REFUTE → `unknown` → caught); a `Hashtbl`-order trigger iteration
  (determinism regression under tight budget → caught); open the public `Env` door to
  `.oxsmt.*` (forgeability regression → caught); use the registering `are_equal` in the
  matcher (e-graph grows during a read → an A4-membership / determinism regression).
- **§3a claim probes (the merits recommendation must be falsifiable).**
  - **Leak-funnel probe (option-A enforcement claim).** Weaken `Instance.of_subst`'s
    residual-placeholder check to a no-op AND open the `Env` door; a fixture where a
    partial σ or a forged symbol leaves a placeholder in an asserted term must then be
    caught — proving mint-door + assert-gate are *together* the single-funnel invariant.
  - **Capture/alpha probe (option-A vacuousness claim).** Attempt to construct a
    target-fragment (single-level ∀, ground body) lemma whose instantiation captures a
    variable or needs alpha-dedup. The claim (§3a) is that none exists; a
    counterexample is the O1 flip trigger and would move the recommendation to (B).

---

## 8. Staging plan (tranches)

- **Tranche 1 (L1 — this ADR's normative core):** `smt/ematch/` skeleton + the store
  + `assert_lemma` (binder-builder, R1) + `Qvar.t`/`Instance.t` + the **scoped
  `env.mli` unfreeze** (public `declare_fun`+`declare_sort` rejection + capability mint
  door, R1) + the private `assert_term_at_frame` frame-scoping (R2) + the outer loop (§1.4) +
  **THE SOUNDNESS RULE + H-SOUND + H-REFUTE + H-PUSHPOP + H-REPEAT-REFUTE +
  forgeability regression**. Ships with a *trivial matcher* (manual-instances path) so
  the store, pipeline, liveness degrade, frame-scoping, and unforgeability are testable
  end-to-end **before** the matcher exists (oracle-first, DESIGN §10). The `env.mli`
  ritual lands here (it is R1, a ratification blocker), as does the cheap
  `assert_term` `.oxsmt.qvar.*` rejection that closes the residual post-mint injection
  path (§1.1) — `Instance.of_subst` remains the load-bearing assert-side guarantee,
  this makes the assert funnel closed-by-construction rather than backstopped-only.
- **Tranche 2 (L2 + L3):** the non-registering e-graph view (R6, R-EM3) + matcher v1 +
  annotated triggers + the **in-matching budget debit (R4)**. First real instantiation;
  H-REFUTE becomes a matcher test here.
- **Tranche 3 (L4 + L5):** auto-trigger selection + strategy tiers + **every-tier hard
  budget (R5) + fairness (R8)** + **H-LOOP + H-XPROD + H-MUTUAL** + the `local` flag +
  the tight-budget determinism test (R7). Generative and mutual-loop instantiation
  become budget-safe.
- **Tranche 4 (L6):** instantiation certificates — the `Instance` leaf coordinated with
  ADR-0006's format freeze (O6); Lean replay via `specialize`.
- **L7 runs alongside from tranche 2:** synthetic generator first, then UF/UFLIA/AUFLIA
  fetch + ingestion.

Oracle-first ordering (DESIGN §10): each tranche's honeypots gate its mechanism (T1:
H-SOUND/H-REFUTE/H-PUSHPOP/H-REPEAT-REFUTE + forgeability; T3: H-LOOP/H-XPROD/H-MUTUAL);
a tranche does not close until its honeypots have demonstrably gone red on the
corresponding seeded faults.
