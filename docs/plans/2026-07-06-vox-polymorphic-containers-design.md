# Element-polymorphic verified containers for vox

*Design study, 2026-07-06. Probe-grounded options doc for the `'a` frontier
of the vox stdlib. The v1 stdlib (8 modules, all green) is entirely
int-specialized; this study establishes, by probe, what an element-polymorphic
slice can ship on TODAY's compiler and what each further step costs.*

All probes verify with this clone's `_install/bin/ocamlc.opt -vox-solver-path
<lean>` (pinned Lean 4.31), each in a private mktemp dir, ~1s each. Sources
under `scratch_probe/poly/` (indexed in its `README.md`). The three
sub-problems have genuinely different answers and are kept separate.

Positioning against the v2 blocker (design §4, "generic ordered functor
`Make(Ord)`"): this study does NOT re-derive that gap — it confirms it by one
probe (`pfunctor.ml`) and routes around it. The functor is NOT the only path to
element genericity, and two non-functor routes ship real capability now.

---

## 0. The one mechanism that decides everything: how `'a` sorts

`dsort_of_type` (`typing/vox_verify.ml:733`) sorts a type variable by exactly
two rules:

- a variable that is a **parameter of the type declaration being classified**
  → `S_param i` (its position), which at a concrete use resolves through the
  use's `arg_sorts` (so `int t` → the sort at `Int`);
- **any other** type variable — in particular a value-level `'a` in
  `(x : 'a) -> …` — → `S_other`, i.e. the shared uninterpreted sort **VoxU**.

So genericity in vox is carried by **parameterized ghost sorts + parameterized
Lean models** (`type 'a iset [@@vox.sort lean "ISet"]` over `inductive ISet
(a : Type)`), reached through a `Tconstr` head (`int t`) that supplies the
argument sort. A bare value-level `'a` collapses to VoxU; that collapse is
**sound** (VoxU is one model; a VC proved at VoxU instantiates soundly) but it
means the *generic* proof reasons at VoxU, and parameterization only "shines" at
a concrete `Tconstr` instantiation. This is the pre-existing behaviour the via
design's stage-4 "NOT delivered" note records, and every finding below is a
corollary of it.

The consequence that runs through all three sub-problems: an op/law that only
**stores, counts, or concatenates** elements proves generically; an op/law that
must **compute with** elements (decidable equality, order) needs a Lean
`DecidableEq a` / order instance, which does **not** exist at VoxU and fails
closed at the solver.

---

## 1. Probe-established facts

Each fact cites its probe file and verdict. "PASS/FAIL" = the real solver's
verdict.

### Sub-problem A — `'a iarray`

- **F-A1. The built-in iarray theory is `int iarray`-ONLY, by a hard gate.**
  `is_int_iarray` (`typing/vox_reflect.ml:104`) admits `Tconstr(iarray,[int])`
  and nothing else; `dsort_of_type` (`vox_verify.ml:756`) maps only that to
  `S_iarray` ("VoxIA"). Any other element falls through to
  `datatype_sort iarray [arg_sorts]`, which for the abstract predefined
  `iarray` head yields `S_other` (VoxU). *Probe:* `pA1_poly_iarray_len.ml`
  (`'a iarray` length) **FAIL** — Lean "Application type mismatch" (the theory
  symbol `Iarray.length` applied to a VoxU atom). `pA3_float_iarray_len.ml`
  (`float iarray`) **FAIL identically** — this is decisive: the wall is
  `int`-specificity, **not** "abstract element". Baseline `p0_int_iarray.ml`
  **PASS**.
- **F-A2. A polymorphic `get` returning a refined element is doubly blocked.**
  `pA2_poly_iarray_get.ml` (`get : 'a iarray -> … -> 'a{ _ = a.(i) }`) **FAIL**
  at the OCaml *layout* layer: a refined `'a{…}` has layout `any separable`
  (iarray's element layout) and cannot be a function return. Even were the sort
  fixed, `get`'s result is Int-forced by the theory (the `vox_reflect.ml:103`
  comment: "the int gate keeps get's result Int-sorted").

### Sub-problem B — order-free `'a list`

- **F-B1. A parameterized `'a` container over a parameterized Lean model
  verifies, and its algebra instantiates at ANY concrete element.** `pbl.mli` +
  `pbl.ml` (`'a t : refines ('a llist)` over `inductive LList (a:Type)`, ops
  `cons`/`length`/`append`, laws `ll_len_nonneg`/`ll_len_app`) **BOTH PASS**.
  `pblclient.ml` at `int t` **and** `string t`: both laws fire ambiently.
  **PASS at both element types.** This is the load-bearing positive — the
  parameterized route is real, not just for sets (existing `lib/pset`) but for
  the whole "elements only stored/counted/concatenated" op family.
- **F-B2. A polymorphic NULLARY constructor cannot carry a refinement — even at
  a concrete element type.** `pblist.ml` (`empty : unit -> 'a t{ ll_isnil _ }`)
  **FAIL**: `don't know how to synthesize implicit argument a` — the via
  injection of `Nil` leaves the Lean datatype's type parameter an unsolved
  metavariable. `pbl_empi.ml` proves this is **not** fixed by concreteness:
  `empty : unit -> int t{ ll_isnil _ }` **FAILs identically**. Workarounds that
  PASS: an UNSPECCED `empty : unit -> 'a t` (`pbl_emptyplain.ml`, usable as a
  seed but carries no emptiness fact), and `is_empty : 'a t -> bool{ _ =
  ll_isnil l }` (`pemptyq.ml`, the argument pins `a`, so emptiness is
  *observable* by query even though it can't be *stated* on `empty`'s result).
  `singleton x` (`pbl_sing.ml`) PASSes because the outer `Cons(x,Nil)` pins `a`.
- **F-B3. Decidable element operations fail generically.** `pbl_mem.ml`
  (`mem` over `ll_memb {a}[DecidableEq a]`) at generic `'a` **FAIL**
  `synthInstanceFailed` — no `DecidableEq VoxU`. So even *within lists*, `mem`
  is not an order-free op; it belongs to sub-problem C.
- **F-B4. The "VoxU cheat" (a non-parameterized model with an opaque element
  type) is a dead end for a genuine `'a` container.** `pblux.ml`: the
  abstraction function `Vox_tree a -> LList` (LList over an opaque `Elt`) is
  **ill-typed** — the OCaml datatype is genuinely `a`-parameterized, so its
  elements are `a`-typed and cannot inject into an `Elt`/VoxU-typed model
  ("Application type mismatch" at the `.Cons x t => .LCons x …` arm). A
  non-parameterized model can only host a shape/count abstraction that drops
  elements — not a list-of-elements model.
- **F-B5. A refined bare-`'a` result is not returnable; wrap in a value-ADT.**
  `pret_elt.ml` (`hd_or : … -> 'a{ _ = ll_hd d l }`) **FAIL** (occurs/layout).
  The working pattern is to return a `value`-kind ADT wrapping the element
  (see F-C1's `'v mopt`), never a bare refined `'a`.

### Sub-problem C — element-generic set/map

- **F-C1. An `'a`-VALUED, int-keyed map ships TODAY.** `pvmap.mli` + `pvmap.ml`
  (`'v t : refines ('v mlist)` over `inductive MList (v:Type)` with `Int` keys;
  `find`/`add` + `m_find_add_eq`/`m_find_add_ne`) **BOTH PASS**.
  `pvmapclient.ml` at int-valued **and** string-valued maps: both find laws
  (including the different-keys `ne` law, which needs only `k ≠ k'` on `Int`)
  fire. **PASS at both value types.** Keys are `Int` (decidable equality is
  Lean-native); values are only stored → reduces to sub-problem B. `find`
  returns `'v mopt` (a value-ADT) — the F-B5 wrapper pattern. Only `empty`'s
  spec hits F-B2 (ship unspecced).
- **F-C2. `Make(Ord)` is blocked by a hard, explicit error.** `pfunctor.ml` (a
  `[%%vox.lean]` block in a `Make(O:ORD)` functor body) → **"vox: [%%vox.lean]
  blocks are unit-level; move to the file top level"**. Confirms design §4 B1;
  the block is *rejected*, not silently dropped.
- **F-C3. Element-generic ordered set/map via a REFLECTED comparison is
  feasible (model + generic proof), at a named TCB cost.** `preflcmp.mli`: a
  polymorphic reflected `cmp {a} : a -> a -> Int` + `[@@vox.reflect "cmp"]`
  **declares/PASSes**. `preflset.ml`: an opaque `cmp` + an *assumed* order
  axiom (`cmp x y = 0 ↔ x = y`) + a generic membership-decider theorem
  `s_memb_mem` — the block and the **theorem prove generically** (no
  DecidableEq wall: `cmp` is an uninterpreted `Int`-valued function, so
  reasoning is at VoxU and instantiates soundly). What is **UNPROVED here**:
  closing the decidable `mem` *op* (`preflcset.*`/`preflset2.ml`) — the
  reflected result's identity `c = cmp x y` and the branch-guard fact do not
  thread into the match arm (an ordinary fact-threading gap of the #32 family,
  NOT a polymorphism wall). The TCB this route adds: the `cmp` order axioms +
  the `[@@vox.reflect]` correspondence — the same class of assumption as
  `reflectbits`, confined to one companion module.
- **F-C4. An order-FREE generic set (add + Prop-membership specs, no decidable
  query) already exists and is honest.** `lib/pset` (read, not re-probed):
  `'a t : refines ('a iset)`, `add` proved generically (the equation `elems
  (Node..) = ins x s` needs no `DecidableEq`), `mem`/`ins` are `Prop`-valued so
  they *state* membership generically; a decidable `member` is deliberately
  absent (it needs `DecidableEq a`). `mechanics/lean_pset_seal.ml` binds `int
  Pset.t` at `(ISet Int)` and proves a membership fact through the abstraction.

### Cross-cutting

- **F-X1. Prop-membership vs Bool-membership is the exact generic/non-generic
  line.** `mem {a} : ISet a -> Prop` (propositional `x = y`) proves generically
  (`pset`, `param_ghost`); `tmem {a}[DecidableEq a] : ISet a -> Bool` (a
  *decider*) fails at VoxU/opaque and closed at the solver. Existing evidence:
  `mechanics/lean_param_ghost.ml` (the `DecidableEq` fail-closed case).
- **F-X2. The pervasive C1 ergonomic gap (a dependent-arg must be a nameable
  variable) bites poly clients exactly as it bites int clients** (`pvmapclient`
  first cut FAILed on `find k (add k x m)`; the let-bound form PASSes). Not
  polymorphism-specific.

---

## 2. Options per sub-problem

### A — `'a iarray`

The whole module (`Viarray`) rides the compiler-owned theory and authors zero
Lean; genericity therefore is **entirely a compiler question** — there is no
library workaround (F-A1: even `float iarray` is out).

- **A-opt-1 (recommended ask): sort-indexed array theory.** Generalize
  `S_iarray` to `S_iarray of dsort` (element sort), and generalize the emitted
  `VoxIA` theory to `VoxIA α` with `ia_len : VoxIA α → Int`, `ia_get : VoxIA α
  → Int → α`. `is_int_iarray` becomes `is_iarray` returning the element sort;
  the `int`-gate on `get`'s result drops (result sorts at `α`). *Encoding
  sketch:* mirror the parameterized-ghost-sort machinery already shipped
  (`Vs_lean (name, args)`), i.e. treat iarray as a built-in one-argument ghost
  sort whose theory is emitted with a `Type` parameter. *Cost:* moderate — one
  new sort argument threaded through the ~8 `S_iarray` match sites
  (`vox_verify.ml`), the `sort_needs_iarray` recursion (already recurses into
  args, `vox_verify.ml:3528`), and the theory emitter (`Vox_module`); plus the
  layout question for `get` (F-A2) — returning a refined `'a` element needs the
  value-ADT wrapper or a representability relaxation. *Payoff:* `'a iarray`
  length/get for every element type, one theory.
- **A-opt-2 (cheaper, weaker): one theory over VoxU + per-sort instantiation.**
  Keep a single `VoxIA` whose element is VoxU; sort every `'a iarray` (incl.
  `int`/`float`) at that opaque element. Length works (element-agnostic); `get`
  returns a VoxU atom, so `a.(i) = a.(j) → …` reasoning survives but `get`'s
  result cannot be used at a concrete element sort (can't feed an `Int`-needing
  law). *Cost:* small. *Payoff:* length + structural get-equality only; loses
  the int-arithmetic-on-elements that `int iarray` clients rely on. Inferior to
  A-opt-1 for anything but pure length reasoning; not recommended as the target,
  only as a stopgap.
- **A-opt-3 (ship-now, no compiler work): keep `Viarray` int-only; add
  hand-written `Vfarray` (float) / etc. as needed.** Each concrete element is
  its own gated theory. This is the status quo generalized by copy; only worth
  it for one or two hot element types.

### B — order-free `'a list` / `'a option` / iarray clients

The parameterized route (F-B1) is the answer and it works now. Options concern
only the `empty` gap and scope.

- **B-opt-1 (recommended, ships now): parameterized `'a Vplist` with unspecced
  `empty`.** Ship `empty : unit -> 'a t` (no spec), `singleton`, `cons`,
  `is_empty : 'a t -> bool{ _ = ll_isnil _ }`, `length`, `append`, `rev`
  (order-free), and the measure algebra (`ll_len_nonneg`, `ll_len_app`,
  `ll_len_cons`). All PASS (F-B1, F-B2 workarounds). Client cost: to obtain the
  `ll_isnil (empty ())` fact a client uses `is_empty (empty ()) = true`
  (observable) rather than reading it off `empty`'s type. *Encoding:* exactly
  `pbl.{ml,mli}` scaled up. *Compiler work: none.*
- **B-opt-2 (ask that removes the one wart): pin a nullary via-constructor's
  Lean type parameter from the OCaml result type.** F-B2 is a real, isolated
  compiler gap: the via injection of a parameterless constructor emits `ll_repr
  Nil` with the datatype's Lean `a` unsolved, even when the OCaml type is
  concrete `int t`. The fix instantiates that `a` from the expected type's
  argument sort at the injection site (`typecore` via-injection / the
  `dsort_of_type` Trefine path). *Cost:* small-moderate, localized. *Payoff:*
  `empty : unit -> 'a t{ ll_isnil _ }` and every other spec'd nullary
  producer; removes the only ergonomic wart of B-opt-1.
- **B-opt-3 (rejected): the VoxU cheat.** F-B4 — ill-typed abstraction
  function. Do not pursue for genuine `'a` containers.
- **`'a option`/`'a result`:** these are *exposed-ADT* modules whose
  constructors ARE the API (design R2). A parameterized model `Option (a:Type)`
  with `is_some`/`get_or` (the latter returns the element — F-B5 says wrap or
  keep it a stored `'a` argument, not a refined `'a` result) ships by the same
  mechanism. Higher-order `map`/`bind` remain unmodelled (the existing
  "higher-order ops" gap, orthogonal to polymorphism).

### C — element-generic set/map

Three tiers, in increasing power and cost.

- **C-tier-1 (ships now, zero new trust): `'v`-valued int-keyed `Vmap`.** F-C1.
  Values are only stored, keys are `Int`. This is the design's hypothesized
  quick win and it is confirmed end-to-end incl. both find laws at two value
  types. It reduces to sub-problem B. *Encoding:* `pvmap.{ml,mli}`. *Compiler
  work: none* (modulo the F-B2 unspecced-`empty` wart and F-X2 C1 at clients).
- **C-tier-1b (ships now, honest): order-free generic `'a Vpset`.** F-C4 /
  `lib/pset`. `add` + `Prop`-membership specs, `is_empty`, no decidable
  `member`. Useful where membership is a *specification* (invariants,
  postconditions) rather than a runtime query. *Compiler work: none.*
- **C-tier-2 (ships now WITH a named TCB item, once one fact-threading gap is
  closed): element-generic ordered set/map via a reflected comparator.** F-C3.
  A companion `Vord`-style module reflects OCaml `compare`/`equal` to a Lean
  `cmp {a} : a -> a -> Int` (or `eq {a} : a -> a -> Bool`) with **assumed**
  order/equality axioms; the container states membership/ordering over `cmp`
  and proves generically (no `DecidableEq` needed — `cmp` is uninterpreted).
  This is the non-functor route to a *decidable* generic `member`. *Trust:* the
  `cmp` axioms + the reflect correspondence — one auditable companion module,
  same class as `reflectbits`/`Vbits`. *Blocker to close first:* the decidable
  `mem` op's reflected-result/branch-guard fact does not thread (F-C3, an
  ordinary #32-family gap — the exact class the recent bool-branch/bind-skel
  quests targeted); the *model and generic law* already verify. *Compiler work:
  the fact-threading fix (likely already partly landed by the #32 quest — verify
  against current tip); no new sort machinery.*
- **C-tier-3 (v2, blocked): `Make(Ord)` true functor.** F-C2. Needs (a)
  functor-body `[%%vox.lean]` block collection and (b) a per-instantiation
  definable order so an abstract `O.compare` discharges at `int`. This is the
  design's B1 and is genuinely out of reach today. **Delta over C-tier-2:**
  C-tier-2 already gives element-generic ordered containers with a decidable
  query; what `Make(Ord)` adds is (i) removing the reflected-`cmp` TCB (the
  order laws come from the `Ord` argument's *proved* model instead of assumed
  axioms) and (ii) the idiomatic OCaml functor surface. So the functor is a
  *trust-and-ergonomics* upgrade over a working C-tier-2, not the only door to
  generic ordered containers.

---

## 3. Ranked compiler-ask list (most capability per unit of work first)

1. **Close the reflected-result / branch-guard fact-threading gap** (C-tier-2
   blocker, F-C3). Likely small and likely *already partly done* by the
   bool-branch (#32) / bind-skel (#31) quests — **verify against current tip
   before implementing.** Unlocks: element-generic ordered set/map with a
   decidable `member`/`find` TODAY (at a named `cmp` TCB). Highest capability
   per unit of work because the sort machinery is already in place.
2. **Pin a nullary via-constructor's Lean type parameter from the result type**
   (B-opt-2, F-B2). Small, localized. Unlocks: spec'd `empty`/`nil`/`bot`
   producers across *every* parameterized container — removes the single
   recurring wart of the ship-now B and C-tier-1 modules.
3. **Sort-indexed iarray theory `S_iarray of dsort` + `VoxIA α`** (A-opt-1,
   F-A1/F-A2). Moderate; reuses the parameterized-ghost-sort pattern. Unlocks:
   `'a iarray` for all elements. Includes the refined-element-return
   representability question (shared with F-B5).
4. **Functor-body `[%%vox.lean]` block collection + per-instantiation order**
   (C-tier-3 / B1, F-C2). Large, genuinely v2. Unlocks: the idiomatic
   `Make(Ord)` surface and removes C-tier-2's `cmp` TCB. Lowest priority because
   C-tier-2 already delivers the *capability*; this is trust+ergonomics.

(Not a compiler ask but worth flagging as it bites poly clients hardest:
**F-X2 C1** — auto-ANF a pure dependent argument — is the same MAJOR-ERG item
the v1 build already ranked #1; poly containers add no new instance, they just
re-confirm it.)

---

## 4. Bottom line — the v1.5 polymorphic slice shippable on TODAY's compiler

The team-lead hypothesis — "`'a`-valued int-keyed Vmap and possibly `'a Vlist`
over VoxU" — is **half confirmed, half refuted, and net stronger than posed**:

- **CONFIRMED and better than "over VoxU": `'a Vplist` over a *parameterized*
  Lean model.** Not the VoxU cheat (F-B4, refuted — ill-typed), but a genuine
  `'a t : refines ('a llist)` over `LList (a:Type)`. Order-free ops
  (`cons`/`singleton`/`is_empty`/`length`/`append`/`rev`) and the measure
  algebra verify and instantiate at any concrete element (F-B1). The only cut:
  `empty` ships unspecced (F-B2); emptiness stays observable via `is_empty`.
- **CONFIRMED: `'v`-valued int-keyed `Vmap`.** find/add + both find laws, at any
  value type (F-C1). Same `empty` cut.
- **CONFIRMED bonus: order-free generic `'a Vpset`** (add + Prop-membership,
  `lib/pset` shape, F-C4) — honest, zero trust, no decidable `member`.
- **NOT in v1.5 without one small fix: a decidable generic `member`/ordered
  container.** The reflected-`cmp` route (C-tier-2) has its model and generic
  law proved; it needs the ask-#1 fact-threading fix to close the op, plus it
  carries a named `cmp` TCB. Ship it in v1.6 behind that fix.
- **NOT shippable at any near horizon: `'a iarray`** (needs ask-#3) and
  **`Make(Ord)`** (v2, ask-#4).

So a v1.5 build is three modules — `Vplist` (`'a`), `Vpmap` (int-key/`'v`-val),
`Vpset` (order-free `'a`) — all on the current compiler, all zero-trust, each
with an unspecced `empty` until ask-#2 lands. That triples the container
surface from "int only" to "any element you only store, count, or concatenate,"
which is the large majority of real container use.

### UNPROBED / deferred claims (flagged)

- The A-opt-1 cost estimate (thread one sort arg through ~8 sites) is a
  **reading-based estimate, UNPROBED** — no compiler change was attempted.
- Whether ask-#1's fact-threading is *already* fixed on current tip is
  **UNPROBED** (this clone is pinned at the v1 build compiler); verify before
  scheduling C-tier-2.
- `'a option`/`'a result` parameterized-ADT modules are **UNPROBED** here
  (argued by analogy to B-opt-1 / the exposed-ADT v1 modules); the higher-order
  `map`/`bind` gap is orthogonal and pre-existing.
- `rev` and other order-free list ops beyond `append` are **UNPROBED**
  individually (asserted by analogy to `append`, which is the hardest —
  it hits and clears the #31 skeleton-threading pattern).
