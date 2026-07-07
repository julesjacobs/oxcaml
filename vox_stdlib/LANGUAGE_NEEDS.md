# vox stdlib — consolidated language/compiler needs (waves 1+2, 8 modules)

*Consolidated by the integrator from the eight module notes
(`vox_stdlib/notes/*.md`), 2026-07-06. Evidence-backed successor to the
*predicted* inventory in `docs/plans/2026-07-06-vox-stdlib-language-needs.md`:
that doc ranked needs from probes; this records what real construction of
Vlist, Voption, Vresult, Vint, Viarray, Vset_bst, Vmap (wave 1) and Vset
(wave 2) actually hit. All eight modules verified green with the real solver.
**Trust ledger (corrected per the Phase-C soundness review):** the
assumed-axiom ledger is EMPTY across all eight — but "empty assumed-axiom
ledger ≠ zero trust". Exactly ONE named, audited trust surface exists: Vint's
`[@@vox.reflect]` body↔def correspondence (the solver does not check that the
OCaml body implements the Lean def; a divergent body verifies and proves
runtime-false facts). Every other module is trust-free end to end.*

## Severity-ranked summary

| # | Need | Severity | Sites | One-line ask |
|---|---|---|---|---|
| 1 | **C1 — dependent-arg must be a variable** | MAJOR-ERG | Vlist, Voption, Vresult, Vset_bst, Vmap, Vset (**all 6 non-trivial**) | auto-name (ANF) a pure call/ctor argument feeding a dependent parameter |
| 2 | **M1 — client law statement typed twice** | MAJOR-ERG | Vlist, Voption, Vset_bst, Vmap (obligation form); **refuted by** Vresult + Vset (M1=0) | a prove-only `.mli` form; AND widen the §4 inline exception (Vresult A/B + Vset op-spec algebra: inline/op-spec pays 0) |
| 3 | **model-dup — model theory in both blocks** | MAJOR-ERG | Vlist, Voption, Vresult, Vset_bst, Vmap, Vset (all 6 block-bearing) | let the `.ml` block import/reuse the `.mli`'s model defs instead of restating them |
| 4 | **M3/attribution — laws silently inert** | MAJOR-ERG | Vint (×2), Vmap, Voption | (a) `@[grind]` must derive a trigger from `<=`-conclusions; (b) lint expose-derivable/no-trigger laws |
| 5 | **higher-order ops unmodelled** | MAJOR-ERG | Voption, Vresult | a spec vocabulary for function-typed op args (`map`/`bind`) |
| 6 | **constructor unnameable in a spec** | MINOR | Vmap (refinement), Vresult (block law) | allow a dotted `.Ctor` in a refinement predicate; resolve a bare ctor in a block law |
| 7 | **via-value sorting at a binder** | MINOR | Vset (top-level value), + the #31 family | a top-level via-typed value binding mis-sorts (image name = skeleton rhs); Vset's `empty` had to become a `fun` |
| 8 | **N2 — array theory is `int iarray`-only** | MINOR | Viarray | reflected `'a iarray` + McCarthy-store mutable `int array` (v1.1 boundary) |
| 9 | **C3 — `*unknownN*` VC placeholder names** | COSMETIC | Vlist, Vresult | source-derived names for VC result atoms |
| 10 | **#31/#32 pre-seed calibration** | COSMETIC | Vset_bst, Vmap, Vset | (evidence, not a gap) the pre-seeded #31/#32 sites do NOT bite the shipped shapes — see below |
| 11 | **dead-law removal-test false positive** | COSMETIC | Vint | reviewer-methodology: inter-derivable mandated laws (min_le_left/right) fail the strict removal test |

**No BLOCKING items across waves 1+2.** The one true blocker (B1, the generic
ordered functor) is a v2 concern and no module needed it.

**Two house-rule amendment candidates surfaced with dual evidence** (§ below):
(A) EXPOSE only what stays load-bearing; (B) widen the inline-theorem default
for definitional leaf ADTs.

---

## Per-gap sections

### C1 — a dependent-parameter argument must be a nameable variable (MAJOR-ERG, pervasive)

The single most frequent friction: **all six** non-trivial modules hit it.
Passing a call result *or* a constructor application directly to an op whose
parameter is named in its result refinement / precondition fails with
`vox: the argument for a dependent parameter must be a variable or a pure
expression the logic can name (let-bind it first)`.
- Vlist notes (nested `Vlist.length (Vlist.cons x l)`), Vresult
  (`is_ok (Vok x)`), Voption (`get_or d (Vsome x)`), Vset_bst
  (`member x (insert x s)`), Vmap (`find k (add k v m)`), Vset (inline-coerce
  a backend call `(Vset_bst.insert x t0 : t{…})` into the via type mis-sorts;
  let-bind the call result first, then inject the variable).
- Workaround everywhere: let-bind the argument, pass the variable. Purely
  mechanical but it defeats the natural nested-application style at *every*
  dependent-op call site.
- **Ask:** ANF a syntactically-pure argument (call result or ctor
  application) automatically, or admit an argument whose result type the
  logic can already name. Voption/Vresult rate this MAJOR-ERGONOMIC because
  it bites client code, not just library code.

### M1 — client law typed twice, and the inline counter-evidence (MAJOR-ERG)

Obligation form (the §4 default) requires each client law's full statement +
`grind_pattern` verbatim in both the `.mli` (`public axiom`) and the `.ml`
(discharging `theorem`): Vlist 5×, Vmap 4×, Voption 3×, Vset_bst 2×.
- **Vresult A/B (the key finding):** a `public theorem` in the `.mli` is
  proved once and is **not** re-demanded by the seal (unlike a `public
  axiom`). Vresult ships its 3 laws inline with a **defs-only `.ml` block**
  and seals green; a control variant restating the theorems in the `.ml` is
  redundant. So for definitional leaf ADTs, inline pays **M1 = 0** at
  identical soundness — obligation form there is pure tax.
- **Vset (second M1=0 witness, different mechanism):** the wave-2 face ships
  **no separate axiom laws at all** — its R5 algebra is carried by the op
  specs `vs_isempty` (empty) and `vs_addspec` (add) composed over a *private*
  `.ml` bridge to the backend's `bmem_insert`. So a via face whose algebra is
  its op postconditions also pays M1 = 0 (a client needs no `vs_mem_add`
  beyond `addspec`). Two independent shapes now avoid the M1 tax: inline
  definitional theorems (Vresult) and op-spec-as-algebra (Vset).
- **Ask (two):** (1) a prove-only `.mli` obligation form (`.ml` names the law
  + supplies only a tactic, no restated statement); (2) **house-rule
  amendment B** — widen the §4 inline exception to all definitional leaf ADTs
  (no scaffolding, one-line proofs), since obligation form buys no extra
  safety there. Phase C has a clean Voption(obligation) vs Vresult(inline)
  A/B on the same shape, plus Vset's op-spec algebra as a third data point.

### model-dup — model theory authored in both blocks (MAJOR-ERG)

Every module with a `[%%vox.lean]` block in both files (Vlist, Voption,
Vresult, Vset_bst, Vmap, Vset) restates its model defs in the `.ml` (without
`public`) because the abstraction fn and the discharging theorems reference
them and the interface's `public def`s are not in scope for the `.ml` block's
elaboration. 3–6 defs duplicated per module (Vset: 1 inductive + 3 defs
duplicated, plus 4 `.ml`-only private scaffolding decls that must NOT leak to
the `.mli`). **Absent** for Vint (reflect, one block) and Viarray (no block)
— the contrast cases.
- **Ask:** a `.ml`-side "import the interface block's model theory" form so
  model defs are authored once and inherited into the seal context.

### M3 / attribution — laws that compile, ride, and silently do nothing (MAJOR-ERG)

The strongest convergent cluster; two distinct mechanisms:
- **Vint — `@[grind]` on a `<=`-conclusion theorem is inert without a
  `grind_pattern`.** The five bound laws (`vi_min_le_left`, …) compiled,
  carried `@[grind]`, and *never fired* at the client (goal = the theorem's
  own statement, `NOT PROVED`); equality-conclusion laws (`comm`/`idem`) fire
  from bare `@[grind]`. Fix: attach a `grind_pattern` per bound law. **Ask:**
  `@[grind]` should derive an E-matching trigger from an inequality
  conclusion (its head application) as it does for an equality's LHS — or
  lint an attributed theorem that produced no trigger.
- **expose-kills-laws (Vint + Vmap, dual evidence → house-rule amendment
  A).** When a model def is `@[grind, expose]` and its op is non-recursive
  (Vmap's `m_add`/`m_find` prepend/head-match; Vint's `vi_min`
  if-then-else), a client's `grind` discharges the law's goal *by unfolding
  the def*, so every shipped law is derivable-without-the-law — silently
  dead. Vmap fixed it by declaring the model ops **opaque `public axiom`s**
  (the oset pattern); Vint by dropping `expose` (keep `public`). Contrast:
  Vlist's `lapp`/`llen` recurse on the abstract arg, so `llen_lapp` is
  un-unfoldable and stays live even when exposed.
  - **House-rule amendment A (Vint+Vmap):** *EXPOSE a model def only when it
    stays load-bearing — recursion over the abstract argument keeps its laws
    live; a non-recursive def must stay opaque (axiom / `public` without
    `expose`) or its algebra ships dead.* Do NOT cargo-cult `reflectbits`'s
    `expose` into a laws-are-the-interface module.
- **Voption** is the mild form (definitional laws over `@[grind, expose]`
  defs: both "law fired" and "def unfolded" verify, so the law is not
  *forceable* as the sole path). **Ask (unifying):** the M3 dead-lemma lint,
  generalized to (i) `<=`-conclusion no-trigger theorems and (ii)
  definitionally-derivable laws — report which named block lemma actually
  fired in a VC.
- **Phase-C escalation to MUST-FIX (soundness review): dead-laws-under-expose
  is an evidence-of-verification hole, THREE sightings, all now RESOLVED.**
  The review proved that with the model defs exposed, deleting **every** law
  from Voption's / Vresult's interface still left their smoke clients GREEN
  (the laws were all dead; the smoke passed purely by unfolding), and Vlist's
  two cons-laws were likewise dead — so the harness's smoke-*consumption*
  check was **insufficient** (it does not distinguish "law fired" from "def
  unfolded"). Fixed by de-exposing the non-recursive defs (`public` w/o
  `expose`) and shipping explicit reduction laws (`vo_get_some`,
  `vr_get_err_err`, Vlist's cons-laws, and the ll_isnil characterization
  laws); each is now **per-law removal-swept LIVE** (delete it → its smoke
  goal breaks). This escalates Amendment A from an ergonomic to a
  **soundness-of-evidence** rule and is why §6.7 is now the **removal test on
  every law**, not smoke-consumption (blueprint §6 item 7, rev.). Mechanized
  as a harness §6.7-liveness WARN (flags exposed non-recursive value defs).
  **Ask (build-vlist's general form, strongest):** a solver-side lint that
  flags a shipped `public axiom`/`theorem` whose `grind_pattern` LHS is
  discharged by **definitional unfolding of an exposed def alone** — i.e. the
  law is redundant given what is exposed. That is the precise, general
  detector for this whole class (feeds the "lint for silently-dead block
  theorems" backlog).
- **Vint — a conditional combining bound cannot fire; the free arithmetic
  variable is uncoverable (same E-matcher family as the `<=` item above).**
  The natural clamp lemmas `vi_max_le : vi_max a b <= c` (given `a<=c ∧ b<=c`)
  and dual `vi_le_min` are **unshippable as firing grind lemmas**: the extra
  variable `c` appears only in `≤` atoms, and grind's E-matcher indexes
  function-*application* terms, not `≤`/`<` atoms, so no trigger can bind `c`
  (conclusion-pattern rejected at the seal; every other attributed form seals
  but is silently inert). Same root cause as `lib/bst.mli`'s `not_mem_lt`,
  which binds its extra var via the *application* `all_lt t b`, not via its
  `<=` hypothesis. **Fix idiom (shipped):** the **cases-law** disjunction
  `vi_max_cases : vi_max a b = a ∨ vi_max a b = b` (pattern `vi_max a b`)
  fires cleanly and **subsumes** the conditional bound by case-split — Vint's
  `s_clamp`/`s_clamp_mirror` smoke goals discharge through it. **Ask:** a way
  to bind a conclusion-only arithmetic variable for a `≤`-conclusion trigger,
  or documentation of the cases-law idiom as the standard workaround.

### higher-order ops unmodelled (MAJOR-ERG, capability gap)

Voption (`map`/`bind`) and Vresult (`map`/`bind`/`map_err`) — the natural
combinators — cannot be shipped: vox does not model function-typed arguments
in the refinement logic, so `{ _ = mapspec f o }` has no way to talk about
`f`. First-order ops only in v1. **Ask:** a spec story for applying a
modelled/reflected function symbol to a value (spec-carrying arrows, or
reflected function symbols with an application axiom). Vresult rates this the
biggest gap in its surface (combinators are the point of a result type).

### constructor unnameable in a spec (MINOR)

Two facets: (a) **Vmap** — a refinement predicate has no form for "returns
constructor C": `mopt{ _ = .MMiss }` / `{ _ = .MFound v }` is a syntax error
(no leading-dot term; bare `MMiss` is not a refinement atom). Worked around
by stating find-law goals as equations between two opaque `m_find`
applications. (b) **Vresult** — a bare constructor in a *block law* fails
(`Function expected at Vok`); must be dotted `.Vok`. **Ask:** allow a dotted
`.Ctor` in a refinement predicate (matching the block-law rule), and resolve
a bare exposed-ADT constructor in a block law against the unit's sort.

### via-value sorting at a binder (MINOR)

**Vset:** the natural nullary constant `let empty : t{ vs_isempty _ } =
(Vset_bst.Leaf : t{…})` — a **top-level** via-typed value binding — records a
mis-sorted definitional fact (the via image name bound to the skeleton rhs;
the documented `triset` finding). Workaround: make `empty` a function
`(u : unit) -> t{…}` and produce the via value *inside* the body. Same
via-injection-sorting family as the #31 `let`-binder story. **Ask:** allow a
top-level via-typed value binding to sort at its image without the
skeleton-fact artifact.

### N2 — reflected array theory is `int iarray`-only (MINOR, v1.1 boundary)

Viarray graduates cleanly because the built-in `int iarray` theory
(`Iarray.length`/`a.(i)` + one nonneg axiom) carries the whole module with no
authored algebra. But it is fixed to `int iarray`: `'a iarray` and mutable
`int array` do not reflect (a mutable read becomes an `assume_unchecked_`,
leaving the zero-trust ledger). Exactly the v1.1 boundary the blueprint
draws. **Ask:** element-parameterized array theory + McCarthy-store mutable
reflection.

### C3 — `*unknownN*` VC placeholder names (COSMETIC)

Vlist (`*unknown7* = ll_len a + ll_len b`) and Vresult
(`*unknown1* = vr_is_ok r`): a failing VC prints the op-result atom as a
synthetic name, harder to map back to source. **Ask:** name the result atom
after the value binding / return.

### #31 / #32 — pre-seed calibration (COSMETIC, evidence not gap)

The blueprint pre-seeded #31 at every recursive via op and #32 at
bind-then-branch. Real results sharpen this:
- **#31 HIT once, as predicted:** Vlist `append` (recursive via-returning) —
  needed the skeleton-thread + inject-once workaround; notes capture the raw
  conflated-`ll_repr` failure baseline. Fixed upstream by the gap-#31 landing.
- **#31 NOT hit:** Vset_bst `insert` (exposed refined ADT keeps its predicate
  across the `let` — the skeleton *is* the tree), Vmap `add` (single prepend
  injection, no threaded recursive via result), and **Vset `add`** (the
  let-bound backend result `r` sits at the backend *skeleton* type
  `Vset_bst.set`, so no via value crosses the binder — the natural
  coerce-a-let-bound-variable form compiles on this pre-#31 clone; the
  mandated inline-ctor re-match is a redundant superset here). The pre-seed
  "Vmap.add / Vset.add if it threads a via result" is **refuted**.
- **#32 NOT hit:** Vset_bst `member` (tail-recursive one-path search, no bool
  binding), Vmap `find` (branch on primitive `k = k'`), and **Vset `mem`** (a
  single tail call to `Vset_bst.member`; the OR-over-two-subtrees membership
  that would trigger #32 lives inside the already-proved backend op, not the
  face). #32 needs a *bind-then-branch on a spec'd bool*, which no shipped
  shape uses. Do not file spurious #32 notes against one-path search.

### dead-law removal test — false positive on inter-derivable algebra (COSMETIC, methodology)

Vint: the §6.7 "delete the law, confirm the goal fails" detector false-flags
`vi_min_le_right` (derivable from `vi_min_comm` + `vi_min_le_left`), yet both
are §3-mandated. So §3's own required set contains mutually-derivable members.
**Reviewer guidance (adopted):** read §6.7 as "every law has a consuming goal
(no orphan)", not "every law is independently necessary". The harness
enforces the former (smoke client must verify); the removal test is a manual
aid only.

---

## Positive / calibration data points (no change requested)

- **Vresult:** `not (Prop)` in a refinement works (`bool{ _ = not (vr_is_ok r) }`).
- **Vmap:** a 3-arg `ACons of int*int*alist` constructor models like Vlist's
  `Cons` (three scalars), NOT a pair value — no bool-field/Prop trouble; the
  pre-flagged pair-as-value hazard did not bite (it moves to v1.1's
  tuple/record-valued map). `m_find_add_ne` disequality needs no hand lemma
  (`grind` decides `k ≠ k'`).
- **Viarray:** built-in theory carries the entire module; the zero-friction
  contrast to every block-bearing module.
- **Vint:** a reflect module pays neither the model-dup nor the M1 tax (one
  block, inline laws).
- **Vset (R7 exhibit):** the in-stdlib cross-module composition works exactly
  as the `triset`/`ptrie` shape predicts — a via-abstract face over an
  exposed-ADT backend, all three ops calling the real backend, algebra carried
  across a *private* bridge theorem (`vs_mem_elems = bmem`), verified against
  the backend's cmi+olean with the backend source never read. The
  `client_set_of_list` R7 gate discharges a single goal from BOTH Vlist's and
  Vset's algebra, cross-unit, with the two models (`LList`, `ISet`) co-imported
  collision-free. Zero trust; M1 = 0.

---

## Phase-C usability-review findings (2026-07-06)

The usability reviewer (13 new realistic clients + a six-module composition,
`scratch_probe/phasec_use/`) found the build solid within its envelope —
composing SIX modules cost +40ms with zero name collisions, a genuine
strength — but flagged three walls for *real algorithms*. Ranked:

### F-2 — via-abstract containers have NO ELIMINATOR (MAJOR, capability)

A via-abstract container (`Vset`, `Vmap`, `Vlist` as a set/map) exposes
**producers** (`empty`/`add`/`cons`) and **point queries** (`mem`/`find`) but
**no way out**: a client cannot fold, traverse, enumerate, or convert the
abstract `t` back to concrete data. Concretely, the reviewer's dedup task
("given a `Vlist`, produce the `Vset` of its elements, then list them") is
**impossible at the interface** — there is no `Vset.elements : t -> …` and no
uncons/iterator. Point queries answer "is x in?", never "what is in?".
- **Not** solved by higher-order `fold` (function-typed args are an
  independent unmodelled gap — see the MAJOR "higher-order ops" entry).
- **Ask / design:** a **first-order eliminator** story — see the design
  addendum `docs/plans/2026-07-06-vox-stdlib-eliminator-addendum.md` (per-module
  recommendation: `elements : t -> int list` with a model-level
  membership-iff-`List.mem` spec; uncons-style views; `remove`). Farms to the
  owning builders on approval as the Phase-C fix wave.

### F-3 — no quantifier in the CLIENT refinement grammar (MAJOR)

A client can only *use* quantified facts that a module **pre-baked into a
shipped def** (e.g. `Vset.vs_addspec`'s `∀ y, …`); a client cannot *write* a
`∀`/`∃` in its own refinement `{ … }`. So a client goal that needs a
universally-quantified property the author did not foresee (e.g. "every
element of this set satisfies P") is unstatable — client expressiveness is
capped at the library author's foresight. Distinct from F-2 (F-2 is "no way
to enumerate"; F-3 is "no way to *say* a quantified property"). **Ask:** admit
`∀`/`∃` binders in the client refinement grammar (bounded over a shipped model
type). Near-term mitigation overlaps F-2's relational defs (the addendum).

### F-1 — literal arguments mis-model the dependent via argument (MAJOR, compiler bug — fix in flight)

`Vmap.add 1 10 m` (integer *literals* in the dependent-arg positions of a via
op) mis-models: a literal is not threaded into the dependent via argument the
way a variable is. Repro in `scratch_probe/phasec_use/`. **Fails closed** (a
verification failure / mismatch, not a false PASS — no soundness hole), so it
blocks writing the client, it does not admit a wrong one. Being fixed
**compiler-side** as task #41 (raise-bot).
- **Workaround until #41 lands:** bind literals to variables first
  (`let k = 1 in let v = 10 in Vmap.add k v m`) — the same C1 let-bind shape.
- **Record:** the literal specialization of C1; #41's fix should subsume it.
  Re-verify the composition client after #41.

### unbounded-`Int` model — no bounded-int / bitvector semantics (systemic)

vox models every OCaml `int` as an **unbounded** Lean `Int` (ideal
arithmetic); machine overflow is out of model. The soundness reviewer's probe
confirms `succ x : int{ _ > x }` verifies via `x + 1` with **no** overflow
obligation — sound *within* the ideal-arithmetic stance, but a real
`succ max_int` wraps and no vox spec can even state the wrap. Not a per-module
gap: it is the foundational arithmetic assumption in the trust ledger,
surfacing wherever a module reasons about `int` (Vint bounds, Viarray indices,
Vmap/Vset keys). **Ask:** a bounded-int / fixed-width (bitvector) model option
so a module can *opt in* to machine semantics when it must (index arithmetic,
hashing, a future `Vbits`). Design is the running non-value-kinds / bounded-int
study (task #40) — cite it; this records the stdlib-construction evidence that
motivates it. **Severity:** MAJOR (systemic), but by design, not a defect — the
ledger already names the ideal-`Int` assumption; Vint's `vi_abs_nonneg` carries
an explicit `min_int` caveat for exactly this reason (`notes/vint.md`).

---

## Phase-C ELIMINATOR-WAVE findings (Commit B, 2026-07-06)

Building the F-2 eliminators (Vset `elements`+`remove`, Vmap `keys`+`remove`,
Vlist `head`/`tail`, Vset_bst `remove`) surfaced these. F-2 is **partially
unblocked**: enumeration works (round-trip verified); guarded destructor
recursion awaits #32 (below).

### RESOLVED: F-2 eliminators ship (first-order, no higher-order fold)
- `Vset.elements : t -> Vlist.t{ vs_elements_spec _ s }` and
  `Vmap.keys : t -> Vlist.t{ m_keys_spec _ m }` enumerate a via-abstract
  container **into the stdlib's own `Vlist`**, bridged by a shipped ∀-spec
  (`∀x, ll_mem x l = <membership>`). This kills F-2's "no way out" AND
  mitigates F-3 (the ∀ lives in the shipped def, not the client). `Vlist`
  gains `head`/`tail` + a reconstruction law (the substitute for the blocked
  view ADT — see BLOCKING below). `remove` (Vset over `Vset_bst.remove`, Vmap
  structural) closes the set/map algebra. The F-2 acceptance client
  `clients/client_set_elements.ml` (add → elements → mem round-trip) and
  Vmap's `key_enumerated` smoke verify the capability cross-unit.
- **Two-model-import capability (POSITIVE, build-vset STEP-0 probe):** a via
  face CAN `open` and import a SECOND upstream via-model into one interface
  block (`Vset.mli` imports `Vlist`'s `LList`/`ll_mem` alongside its own
  `ISet`), and the ∀-bridge both elaborates and discharges by induction. This
  extends the `uset`/`dcount` single-import result; it is the mechanism the
  elements-into-Vlist eliminators ride.

### BLOCKING — exposed ADT with a via-typed FIELD can't build its model
Probe (build-vlist, both auto-derived AND manual-inductive): `type vlist_view
= VNil | VCons of int * t` with `t : refines llist` fails at the seal —
`Constructor field LList of Vox_Vlist_vlist_view.VCons contains universe level
metavariables … Sort ?u`. vox derives the ADT's Lean model with the via-typed
field at an unresolved universe. This **kills the addendum's Mech B (uncons /
pop-style VIEW eliminators over via types)** for every container.
- **Removed by (evidence-backed workaround):** ship `head`/`tail` with an
  `is_cons` precondition + a reconstruction law `ll_cons (ll_head l) (ll_tail
  l) = l` instead of the view ADT — same first-order traversal capability,
  seals green. (Team-lead-approved shape swap; the addendum's "uncons-style"
  meant the capability.) Real fix: derive an inductive whose field sort is a
  custom Lean sort without universe-metavariable leakage.

### #31 rule — DEFINITIVELY SHARPENED (triple-confirmed)
#31 (a via value loses its map at a `let`) fires **only for a PRODUCING
unit's recursive op that threads a transparent-Trefine via result through a
`let`** — `Vlist.append`, `Vmap.remove`. It does **NOT** fire for: a
prepend-only / single-injection op (`Vmap.add`, `Vset.add`); nor a
CLIENT/downstream unit building an *upstream's sealed* via type (`Vmap.keys`
and `Vset.elements` thread `Vlist.t` results through lets with no #31 — the
opaque cross-unit via image rides as an ordinary refinement fact that survives
the let, and the skeleton workaround isn't even available to a client). Three
independent confirmations (build-vset elements, build-vmap add-vs-remove,
Vlist append). Corrects the addendum's "elements → #31 budget".

### The honest boundary of the #31 fix — refined-via binders don't bridge the Nil arm
The bind-at-skeleton landing (#31) made a *plain* `let t0 = l` on a
transparent-via `l` carry the map — so much so that `refine_` on a KNOWN via
type is now **rejected**: `let refine_ t0 = l` where `l : t{ not (ll_isnil _) }`
fails with the NEW message `vox: a refine_ pattern requires the scrutinee to
have a refined type (a plain let binds at the skeleton and carries the fact
already)`. But the residual gap: a *plain* `let` binder of a **refined** via
type (`t{ not (ll_isnil _) }`) binds at the skeleton yet **cannot bridge its
constructor-case skeleton facts (`t0 = Nil`) to the image side (`ll_repr`)** —
on the `Nil` arm grind sort-mismatches (goal `0 = ll_head l` under
`t0 = Nil, t0 = l, ¬ll_isnil l`, ending in a Lean type mismatch). So a
head/tail written with the `not (ll_isnil _)` PRECONDITION is unprovable on
the vacuous `Nil` arm. This is a genuine **#31-family edge the bind-at-skeleton
landing exposed**, not covered by the fix: #31 threads a *transparent-value's*
map through a let, but a *refined*-via binder's constructor-case skeleton
equalities still don't reach the image contract. **Removed by (used here):**
make head/tail **TOTAL** (drop the precondition; `ll_head .LNil = 0`,
`ll_tail .LNil = .LNil` are total on the model), which sidesteps the vacuous
arm entirely — the shipped de-contortion. **Real fix ask:** let a refined-via
`let` (or `refine_` on a refined via type) carry the constructor-arm skeleton
facts across to the `ll_repr` image, so a genuinely-guarded destructor
(`t{ not (ll_isnil _) } -> …`) is provable. Someone will hit this outside
head/tail (any partial op with a via precondition). See notes/vlist.md
("refine_ rejected on a refined via type").

### E-matcher: a conclusion-absent variable is uncoverable (M3 family, ×3)
grind's E-matcher indexes function-APPLICATION terms, not `≤`/`<` atoms or
hypotheses, so a lemma variable that appears ONLY in an inequality/hypothesis
cannot be bound by a single trigger. Three sightings, three idioms:
- **Vint** conditional bound `vi_max a b <= c` (c only in `≤`) → **unshippable**
  as a firing lemma; ship the **cases-law** `vi_max a b = a ∨ = b` (triggers on
  `vi_max a b`) which subsumes it by case split.
- **Vset_bst** `bok_join` (pivot `b` not in the conclusion `bok (bjoin l r)`)
  → a **3-part multi-trigger** `=> bok (bjoin l r), ball_lt l b, ball_gt r b`
  binds `b` via the application terms; + `ball_lt/gt_mono` to slide the bound.
- **bst** `not_mem_lt` (shipped) binds its bound via `all_lt t b`, not its `≤`
  hypothesis — the original instance of the pattern.
- **Ask:** either derive a usable trigger for a `≤`-conclusion, or document
  the multi-trigger / cases-law idioms as standard.

### Dead INVARIANT-obligation — the removal test is the only detector
build-vsetbst: `bok_delete`/`bok_insert` are NOT forced by an op's refined
set-result — the module SEALS with the law deleted (a silently DEAD obligation,
proven-but-unconsumed); the wave-1 `bok_insert` smoke likely didn't force it
either. **Forcing shape (now the §6.7 standard for invariant laws): a
SYMBOLIC-argument goal** `unit{ bok (bdel x s) }` — grind can't induct on a
variable, so it must use the lemma. The mechanized §6.7 harness WARN targets
exposed *non-recursive* defs and does NOT catch dead *recursive* invariant
laws — only the removal test does. Strongest case yet for the "lint
silently-dead block theorems" backlog; §6.7 checklist updated to require the
symbolic-argument liveness goal for every invariant law.

### #32 gates client-side guarded destructor recursion (F-2 partial)
The `head`/`tail` eliminator supports the round-trip (enumeration) but a
client's guarded traversal `if is_empty l then base else (head l …)` FAILS in
this clone: at `head l`, goal `not (ll_isnil l)`, `Hypotheses: <none>` — the
`if` on the refined bool `is_empty l` does not thread `¬ll_isnil l` into the
else branch (#32, not in this clone). So `of_list` (Vlist → Vset dedup by
destructor recursion) is **blocked here**; the round-trip `client_set_elements`
(no branch) verifies. **Removed by:** the #32 landing (already on
vox-proof-pane at 7afa45262). Records that head/tail's *traversal* use, unlike
its round-trip use, needs branch-fact threading.

### Membership-direct eliminator recipe (ergonomic win)
A membership-direct eliminator (`∀y, ll_mem y result = <src membership>` by
induction) seals **without** a structural model-list + bridge **iff** the
enumeration target ships (a) an EQUATIONAL empty spec `empty : { _ = ll_nil }`
(so grind substitutes `IMG := ll_nil`, making `ll_mem y ll_nil` appear
syntactically) AND (b) a base non-membership law `ll_nil_not_mem`. Vmap.keys
uses this (simpler); Vset.elements uses the structural model-list variant
(concrete `.LNil` base). Two proof strategies, one interface — proof-level
representation independence. Retroactively justifies Vlist's `empty →
{_=ll_nil}` strengthening as load-bearing for downstream eliminators.

### Dependency-snapshot discipline (process lesson)
keys/elements thrashed for a cycle because they were tested against DIFFERENT
`_artifacts/Vlist` snapshots (interim vs final) while Vlist was still changing
(de-exposure, empty-spec strengthening, `ll_nil_not_mem`). Fix: an eliminator
wave that depends on a co-evolving module must build against ONE FIXED
dependency artifact; the integrator freezes and refreshes it once. Recorded as
the "one snapshot per dependency wave" rule; drove the check_wave1/2 per-module
`mod_deps` + fail-loud dep-staging (the DAG is now explicit in the harness).

### C1 tally + refine_-on-refined-via (running)
- **C1** (dependent-arg must be a nameable variable) now at **8+ sites** —
  every eliminator call too (`Vset.add h (of_list tl)`, nested `of_list`).
  Unchanged dominant friction; auto-ANF of pure arguments remains the fix.
- **refine_ on a refined via type** (`t{ ll_iscons _ }`) is rejected;
  workaround = alias to the unrefined `t` first (`let lu = (l : t) in let
  refine_ t0 = lu`). Same via-binder-sorting family as #31.

---

## DE-CONTORTION PASS on the origin/vox compiler (2026-07-07, main-branch consolidation)

Ported onto origin/vox (04f02386d — #31/#32, &&/||, deep patterns, F-1
subst, kinds, hygiene refactor all landed), relocated to `vox_stdlib/`,
rebuilt the compiler, re-verified. **All 8 modules + both harnesses + 12
clients green on the new compiler.** Each pre-landing "removed by" claim was
validated against the new compiler; this table supersedes the per-note
`removed by:` fields where they differ.

| Workaround (note) | removed-by claim | VERDICT | Evidence |
|---|---|---|---|
| Vlist head/tail: refined-via arg `t{ not (ll_isnil _) }` + alias-then-`refine_` + vacuous Nil arm | #31 family (refine_-on-refined-via) | **REMOVED** | head/tail are TOTAL (ll_head/ll_tail total on `.LNil`), so the precondition was unneeded; unrefined arg + the inner-`go`-over-tree pattern (as length/mem) verifies. Precondition + alias workaround deleted. |
| Vlist view-ADT (uncons) BLOCKING: exposed ADT with a via-typed field → universe metavar | compiler fix | **NOT REMOVED — claim RETRACTED (2026-07-07 re-probe)** | RE-PROBED on 04f02386d: `type vlist_view = VNil \| VCons of int * t` still fails at the seal with the exact recorded error — `Constructor field LList of Vox_Vlist_vlist_view.VCons contains universe level metavariables … Sort ?u.7`. The interim "bug FIXED" entry was written **without re-probing** and is WRONG; it directly contradicted the accurate BLOCKING section above. The universe bug is UNFIXED — uncons / view-ADT / pop-style eliminators remain blocked for **every** container. Total head/tail is the correct shipped eliminator (not merely a minimal stop-gap) and stays. Team-lead's optional "which commit fixed it" bisect is MOOT (nothing fixed it). |
| Vset add / remove: inline-ctor re-match of the backend result | #31 / (note: "natural already worked") | **REMOVED** | direct coerce `(r : t{ … })` of the let-bound backend result verifies for both add and remove; the triset-era re-match deleted. |
| of_list guarded traversal (Vlist→Vset dedup) blocked by #32 (head precondition not threaded through `if is_empty`) | #32 | **REMOVED (ADDED as client)** | with total head/tail the traversal needs no `¬ll_isnil` fact; `clients/client_dedup.ml` (of_list + dedup_elems, Vlist→Vset→Vlist) verifies — the F-2 dedup the usability review wanted is now expressible. |
| Vlist.append: skeleton-thread (inner `go` over `tree` with `ll_repr` explicit + inject once) | #31 | **KEPT — claim REFUTED** | natural recursion over the via `t` with an image-spec (`go : t -> t{ _ = ll_app u b }`, via raw `Cons` OR the `cons` op) still FAILS at the base: `Goal 0=0 && ll_repr b = ll_app u b` — the base-case image doesn't reduce. This is orthogonal to #31 (a let-bound via value's *fact*): building/recursing a via result at the skeleton with an image-spec still needs the skeleton-thread. **Ask:** reduce a via-recursion image-spec at the base without the explicit-`ll_repr` skeleton helper. |
| C1: dependent-arg must be a let-bound variable (all ops + eliminators + `Vset.add h (of_list tl)`) | task #53 (ANF) | **PENDING-#53** | left as-is per instruction; #53 (reflectable-inline / auto-ANF) in flight will subsume. |

**Net:** 3 workarounds REMOVED (head/tail precondition+alias, Vset add/remove
re-match, of_list traversal → new dedup client), 1 **NOT REMOVED / claim
RETRACTED** (the view-ADT universe bug is UNFIXED at 04f02386d — re-probed
2026-07-07; the interim "FIXED" entry was an un-probed error, now corrected),
1 KEPT with its removed-by claim refuted (append skeleton-thread — #31 does
not cover via-recursion image-specs), 1 PENDING-#53 (C1). The two genuinely
load-bearing NEGATIVE findings are (a) the **view-ADT universe bug** (still
blocks every view/pop eliminator — total head/tail is the shipped alternative)
and (b) the **append via-recursion image-spec base non-reduction**. The one
POSITIVE capability confirmation is the total-head/tail first-order eliminator
(the round-trip + dedup clients ride it); there is NO uncons confirmation.
