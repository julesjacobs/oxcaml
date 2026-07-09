# vox stdlib — build blueprint

*Execution blueprint, 2026-07-06 (rev. 2, post-review). Executes the v0+v1 of
`2026-07-06-vox-stdlib-design.md` under the constraints proven in
`2026-07-06-vox-stdlib-language-needs.md`. Written for parallel builder
agents working in dependency waves with a per-wave integrator and a Phase C
review loop.*

This document is the contract between the architect and the builders. It is
deliberately prescriptive: every builder should be able to produce a
shippable module from its per-module section here plus the two design docs,
without re-deriving conventions. Decisions are stated with reasons.

**Rev. 2 changelog (both adversarial reviews under
`scratch_probe/blueprint_review/`):** (1) `Vset` and its wave-2 probe
rewritten to the honest `triset` shape — membership-based specs, whole-tree
abstraction, real backend calls, bridge theorem (was a degenerate left-spine
structural spec); (2) `Vmap` repr changed to a cons assoc-list so its
structural `add` spec is faithful (ordered-tree map → v1.1); (3) `Vbits`
demoted from wave 1 to a deferred spike — `Vint` (zero-trust `reflectbits`
graduation) takes its wave-1 slot, so the stdlib now has **zero** trust
items; (4) exposed-ADT laws use dotted `.Ctor`; (5) `§6.1` mechanized;
(6) per-module smoke clients added; (7) template paths, dep-injection style,
and the `#32` note corrected. The committed evidence probes were all re-run
green; see `scratch_probe/blueprint/README.md` + `rootcause_matrix.md`.

Compiler baseline: this clone's `_install/bin/ocamlc.opt`, HEAD `8ebeb0808`.
Gaps **#31** (via value loses its map at a `let` binding) and **#32**
(refined-bool branch facts) are **NOT** fixed here — builders use the
documented workarounds and **file a note at every site** (§5). De-contortion
is a later pass; harvesting the pain is half the point.

---

## 0. Feasibility re-probed for this blueprint (evidence)

Sources under `scratch_probe/blueprint/` (README tabulates results; every
PASS was re-run after rev. 2). On today's compiler:

- **Packaging (design Probe B, rerun):** a client verifies against a
  dependency's `.cmi` + `VoxSig_*.olean` **with the source deleted**, given
  `-I <depdir>`. An installed module *is* its cmi + olean; downstream modules
  verify against artifacts, not sources.
- **R7 cross-unit model reference (`uset`+`dcount`):** a downstream unit's
  **interface** block can `open` an upstream via-abstract module and
  reference its `public` model and ship a theorem over it (`dcount.mli`
  compiles). The matching `dcount.ml` **fails as designed**: a downstream
  unit cannot `refine_`-unpack the upstream's *hidden* representation.
  **Load-bearing consequence:** a cross-module *module* (`Vset` over
  `Vset_bst`) must be built over an upstream that **exposes its
  representation** (an exposed-ADT backend, the `triset`-over-`ptrie` shape);
  a cross-module *client* freely composes the upstream's *ops*.
- **Wave-2 `Vset` over `Vset_bst`, honest triset shape (`wave2/*`):** a real
  graduated BST backend (`Vset_bst`, `b*` names, obligation form, honest
  one-path `member`) + a via-abstract face (`Vset`) with **whole-tree**
  `vs_elems`, the bridge `vs_mem_elems = bmem`, **membership-based** specs
  (`addspec`/`isempty`), `add` wrapping `Vset_bst.insert`, `mem` wrapping
  `Vset_bst.member` — all verify against only the backend's cmi+olean. Plus
  `smoke_vset.ml`, a client forcing the quantified `vs_addspec` to fire.
- **Trivial exposed ADT (`vopt_a`/`vopt_b`):** a leaf ADT compiles with **no
  block**; with a model block + definitional laws it also compiles **iff the
  constructor in a law is dotted** (`.Vsome x`, not bare `Vsome x`).
- **No `import` keyword exists.** Cross-unit composition is OCaml `open` +
  naming the upstream's model by its **solver names** (`public` names as
  written; non-`public` auto-prefixed `Vox_<Unit>_…`). `triset.ml` is the
  shipped exemplar.
- **Root cause of the rev.-1 wave-2 compile failure (`rootcause_matrix.md`):**
  a 4-cell matrix isolates it to **OCaml `(* *)` comments inside the
  `[%%vox.lean]` block** — the sole trigger. The via-type declaration
  ordering (before/after the block) is **irrelevant** (refuted by probe: a
  comment-free block passes either way). Now a house rule (§4).

---

## 1. Module list + scope

Eight modules across two build waves, plus cross-module clients and
per-module smoke clients. **Tight on purpose**: every module ships *complete*
algebra (R5) with a **zero-item trust ledger** — no exceptions after the
rev.-2 `Vbits`→`Vint` swap. Three evidence-driven scope adjustments from the
design-doc menu are called out in §8.

### Wave 1 — seven independent modules (no cross-unit model deps)

| Module | Repr | Model (via what) | Interface style | Trust | Graduates |
|---|---|---|---|---|---|
| **Vlist** | cons-list | inductive `LList` | via-abstract, obligation | none | `scratch_probe/vlist.*` |
| **Voption** | `Vnone\|Vsome` | exposed ADT + model block | exposed ADT | none | new (`vopt_b`) |
| **Vresult** | `Vok\|Verror` | exposed ADT + model block | exposed ADT | none | new |
| **Vint** | `int` | reflect to **proved** `def`s | reflect (.mli) | none | `demo/reflectbits.*` |
| **Viarray** | `int iarray` | built-in iarray theory | reflect (built-in) | none | `lib/ia_lib.*` |
| **Vset_bst** | plain BST | exposed `tree`, model `b*` | exposed-ADT **backend** | none | `lib/bst.*` |
| **Vmap** | **cons assoc-list** | inductive `MList` | via-abstract, obligation | none | new (Vlist-shaped) |

All `lib/`/`demo/` paths are under `testsuite/tests/vox/` (see §3).

### Wave 2 — one module (needs `Vset_bst`'s cmi + olean)

| Module | Repr | Model | Interface style | Trust | Graduates |
|---|---|---|---|---|---|
| **Vset** | `Vset_bst.set` | inductive `ISet`, **membership-based** | via-abstract **face** | none | `lib/triset.*` (shape) + `lib/bst.*` |

`Vset`-over-`Vset_bst` is the **deliberate in-stdlib R7 composition**: `Vset.ml`
opens `Vset_bst`, its **whole-tree** abstraction fn maps `Vox_Vset_bst_tree`
to `ISet`, a bridge theorem `vs_mem_elems : vs_mem x (vs_elems t) = bmem x t`
equates the two, and `add`/`mem` **call the real backend ops** — after the
bridge, `Vset_bst.bmem_insert`/`bok_insert` carry `add`'s spec across with no
tree re-proof. This is the `triset`/`ptrie` shape, verified end to end
(`wave2/*`), and sets up v1.1's backend-swap.

### Wave 3 — clients (in `vox_stdlib/clients/`, verify only)

| Client | Owner | Uses | Demonstrates |
|---|---|---|---|
| **client_set_of_list** | integrator | `Vset` + `Vlist` | R7 gate: one goal needing **both** modules' algebra (removing either module's law breaks it) |
| **client_opt_result** | integrator | `Voption` + `Vresult` | exposed-ADT interplay |
| **smoke_<Module>** | that module's builder | one module | forces each shipped law to fire (satisfies §6.7 dead-law check) |

`client_set_of_list` is **required** (the cross-module-client gate). Each
wave-1/2 builder additionally ships a `clients/smoke_<Module>.ml` (a few-line
goal per shipped law); it is part of that builder's "done" (§7) and is
verified by the integrator once the module's artifact exists.

### Explicitly deferred (NOT this build; roadmap §9)

`Vbits` (reflected bitwise ops with **assumed** masking axioms — a dedicated
trust spike, see §8.2 + a LANGUAGE_NEEDS entry), `Vset_rbt`/`Vset_trie`
backends behind `Vset` (v1.1), `htbl` bucket-hashing behind `Vmap` (v1.1),
ordered-tree `Vmap` (v1.1), mutable containers behind the borrow lib (v1.1),
the generic ordered functor `Make(Ord)` (v2 — the one true compiler blocker
B1).

---

## 2. Dependency DAG + wave assignment

```
Wave 1 (parallel, disjoint files, no cross-unit model deps):
   Vlist   Voption   Vresult   Vint   Viarray   Vset_bst   Vmap
                                                    |
Wave 2 (needs Vset_bst.cmi + VoxSig_Vset_bst.olean):|
                                                  Vset
Wave 3 (clients, need the relevant artifacts):
   smoke_<Module> (per module)
   client_set_of_list (Vset, Vlist)   client_opt_result (Voption, Vresult)
```

- **Wave 1 is fully parallel.** Seven builders, each owning one module's
  `.ml` + `.mli` + `notes/<Module>.md` + `clients/smoke_<Module>.ml`. No
  module references another's model. `Vset_bst` is self-contained; only
  `Vset` depends on it.
- **Wave 2 is one builder** (`Vset`), gated on the integrator having placed
  `Vset_bst`'s cmi+olean in `_artifacts/`.
- **Wave 3** is gated on artifacts. The R7 gate `client_set_of_list` needs
  `Vset` + `Vlist`; smoke clients need only their own module.
- **R7 exercised deliberately twice:** the `Vset`→`Vset_bst` module
  composition and `client_set_of_list`'s two-model co-import. The design's F2
  collision hazard (two co-imported units both `public`-declaring the same
  name) is prevented structurally by the §4 naming rule.

---

## 3. Per-module spec sketches

Each: **model**, **ops** (contract intent), **algebra** (R5), **trust
ledger entry**, **graduation effort**. Law names use the §4 per-unit prefix.
Client-facing laws ship in **obligation form** (axiom in `.mli`, same-named
theorem in `.ml`) unless flagged "inline" (§4 exception). **"Graduation" is
never copy-paste** — it is at minimum a prefix-rename plus (for the
`public-theorem-in-.mli` templates) an obligation split; each section states
the delta.

### Wave 1

#### Vlist — via-abstract cons-list (graduation: `scratch_probe/vlist.{ml,mli}`)
- **Model:** `LList` (`LNil | LCons Int LList`), sort `LList`.
- **Model defs (public `.mli`):** `ll_cons`, `ll_isnil`, `ll_len`, `ll_mem`, `ll_app`.
- **Ops:** `empty : unit -> t{ ll_isnil _ }`, `cons x l : t{ _ = ll_cons x l }`,
  `length l : int{ _ = ll_len l }`, `mem x l : bool{ _ = ll_mem x l }`,
  `append a b : t{ _ = ll_app a b }`, `is_empty l : bool{ _ = ll_isnil l }`.
- **Algebra (R5 — 5 laws):** `ll_len_nonneg`; `ll_len_cons`; `ll_len_app`
  (measure-of-combine); `ll_mem_cons` (membership-of-ctor); `ll_mem_app`.
- **Graduation delta:** rename PoC's `lcons`/`llen`/… to `ll_*`; convert its
  inline `public theorem`s to obligation form (§4). The `append` op already
  uses the #31 skeleton-threading workaround — keep it, file the note.
- **Trust:** none.

#### Voption — exposed ADT (graduation: `scratch_probe/blueprint/vopt_b.{ml,mli}`)
- **Repr/model:** `type t = Vnone | Vsome of int`; block defs
  `vo_is_some : t -> Prop`, `vo_get_or (d:Int) : t -> Int`, `vo_get : t -> Int`
  (partial, used under the `is_some` precondition).
- **Ops:** `is_some o : bool{ _ = vo_is_some o }`,
  `is_none o : bool{ _ = not (vo_is_some o) }`,
  `get_or d o : int{ _ = vo_get_or d o }`,
  `get (o : t{ vo_is_some _ }) : int{ _ = vo_get o }`.
- **Algebra (definitional, 3 laws — DOTTED constructors):**
  `vo_is_some_some : vo_is_some (.Vsome x)`;
  `vo_not_some_none : ¬ vo_is_some .Vnone`;
  `vo_get_or_some : vo_get_or d (.Vsome x) = x`.
- **Trust:** none. **No higher-order ops** (`map`/`bind` take function args;
  vox does not model those — file a note, defer).

#### Vresult — exposed ADT (graduation: new; mirrors Voption)
- **Repr/model:** `type t = Vok of int | Verror of int`; defs
  `vr_is_ok`, `vr_get_ok (d:Int)`, `vr_get_err (d:Int)`.
- **Ops:** `is_ok`, `is_error`, `get_ok_or d r`, `get_err_or d r`.
- **Algebra (definitional, 3 laws — DOTTED):** `vr_is_ok_ok : vr_is_ok (.Vok x)`;
  `vr_not_ok_error : ¬ vr_is_ok (.Verror e)`; `vr_get_ok_ok : vr_get_ok d (.Vok x) = x`.
- **Trust:** none.

#### Vint — reflected integer ops (graduation: `testsuite/tests/vox/demo/reflectbits.{ml,mli}`)
- **Model:** **proved** Lean `def`s `vi_min`, `vi_max`, `vi_abs` in the `.mli`
  block (e.g. `def vi_min (x y : Int) : Int := if x <= y then x else y`), each
  with its algebra as **proved `public theorem`s** (`by grind [vi_min]`).
- **Ops:** `imin : int -> int -> int [@@vox.reflect "vi_min"]`, likewise
  `imax`, `iabs`.
- **Algebra (proved, zero trust):** `vi_min_comm`, `vi_min_idem`,
  `vi_min_le_left : vi_min x y <= x`, `vi_min_le_right`, `vi_abs_nonneg`.
- **Trust:** **none by construction.** `reflectbits` proves its laws from
  computable `def`s (`bmin`), so the *only* residual would be the
  `[@@vox.reflect]` correspondence itself — and for min/max/abs the OCaml body
  is visibly identical to the `def`, so the module is honest. (This is why
  `Vbits`, whose ops have no computable `def`, is deferred — §8.2.)
- **Graduation delta:** `reflectbits.mli` ships `imin`/`bmin`; extend to
  max/abs, rename to `vi_*`, add the bound laws. Nearly copy-paste.

#### Viarray — immutable int array (graduation: `testsuite/tests/vox/lib/ia_lib.{ml,mli}`)
- **Model:** built-in `int iarray` theory (`Iarray.length`, `a.(i)`); **no block.**
- **Ops:** `length a : int{ _ = Iarray.length a }`;
  `get a (i : int{ 0 <= _ && _ < Iarray.length a }) : int{ _ = a.(i) }`;
  `unsafe_get` (same bounds precondition, no result eq).
- **Algebra:** none authored — the built-in theory supplies `get`'s spec.
  (If `length_nonneg` does not come for free, add it; verify by probe.)
- **Trust:** none per-module (iarray reflection is foundational TCB, shared
  by every vox program).
- **Graduation delta:** `ia_lib` is 11 lines and already `V*`-neutral; add
  `length`. Copy-paste.

#### Vset_bst — sorted-BST set backend (graduation: `testsuite/tests/vox/lib/bst.{ml,mli}`)
- **Repr:** exposed `type tree = Leaf | Node of tree * int * tree`;
  `type set = tree{ bok _ }`.
- **Model defs (public):** `bmem`, `ball_lt`, `ball_gt`, `bok`, `bins`.
- **Ops:** `empty : set{ _ = Leaf }`; `member x t : bool{ _ = bmem x t }`;
  `insert x t : set{ _ = bins x t && bmem x _ }`.
- **Algebra (R5):** client-facing laws `bok_insert : bok t → bok (bins x t)`
  and `bmem_insert : bmem y (bins x t) ↔ y=x ∨ bmem y t` ship as **`.mli`
  obligations**; the ordering scaffolding `bnot_mem_lt`/`bnot_mem_gt` (make
  one-path search complete) and `ball_lt_insert`/`ball_gt_insert` are
  **private `.ml` theorems** (not in the `.mli`).
- **Graduation delta — the heaviest rename+split in the build (call it out
  to the builder):** `bst.mli` uses the *forbidden bare names*
  `mem`/`insert`/`bst`/`all_lt`/`all_gt` → rename every def, law,
  `grind_pattern`, and reference to `b*`; and `bst.mli` ships **all 6 laws as
  inline `public theorem`s with `by induction` proofs** — move the 4
  scaffolding proofs into the `.ml` (drop `public`) and split the 2
  client laws into `.mli` axiom + `.ml` theorem. Verified achievable
  (`wave2/Vset_bst.*`). `member`/`insert` are the honest one-path forms from
  `bst.ml`; `member`'s `if x<v then member x l else member x r` is
  **tail-recursive and does NOT hit #32** (see §5).
- **Trust:** none. Exposed repr is **by design** (backend tier) — it is what
  lets `Vset` build a via face over it.

#### Vmap — via-abstract finite map, int keys (graduation: new, Vlist-shaped)
- **Model:** inductive `MList` = `MNil | MCons Int Int MList` (key,val entries,
  first-binding-wins), sort `MList`; result `MOpt = MMiss | MFound Int`.
- **Model defs (public):** `m_find (k:Int) : MList -> MOpt`,
  `m_add (k v:Int) : MList -> MList := .MCons k v`, `m_isempty`.
- **Repr:** a **genuine cons assoc-list** of `(key,val)` (NOT a tree — rev. 2).
  This is what makes the structural `add` spec honest: `add` really *is* a
  prepend, so `{ _ = m_add k v m }` is faithful (soundness M-2). An ordered
  tree would make `tree_insert ≠ MCons` and the structural spec a lie — that
  variant is v1.1 with a find-based spec.
- **Ops:** `empty : t{ m_isempty _ }`;
  `find k m : MOpt{ _ = m_find k m }`;
  `add k v m : t{ _ = m_add k v m }` (structural, honest for the list repr).
- **Algebra (R5 — 4 laws):** `m_find_empty : m_find k empty = MMiss`;
  `m_find_add_eq : m_find k (m_add k v m) = MFound v`;
  `m_find_add_ne : k≠k' → m_find k (m_add k' v m) = m_find k m`
  (the map characterization — find-based, needs no ordering);
  `m_isempty_empty`.
- **Graduation delta:** none (new), but effort is **Vlist-level** now that the
  repr is a list — `find`/`add` are structural recursions, no ordering proofs.
- **Trust:** none.

### Wave 2

#### Vset — via-abstract set face over Vset_bst (graduation: `lib/triset.*` shape + `lib/bst.*` backend; worked exemplar `wave2/*`)
- **Model:** inductive `ISet` (list-shaped), sort `ISet`; public defs
  `vs_mem`, `vs_isempty`, `vs_addspec`. **Membership-based**, not structural:
  the `ISet` list model's structural `=` is **not** set equality, so op specs
  are stated as pointwise membership agreement (exactly `triset.mli` /
  `oset.mli`), never `{ _ = vs_ins x s }`.
  - `vs_isempty s := ∀ y, ¬ vs_mem y s`
  - `vs_addspec r x s := ∀ y, vs_mem y r = (y = x ∨ vs_mem y s)`
- **Repr / via:** `open Vset_bst`;
  `type t = Vset_bst.set{ 0 = 0 } [@vox.via (vs_elems : iset)]`. The
  abstraction fn `vs_elems : Vox_Vset_bst_tree -> ISet` **recurses into BOTH
  subtrees** (`Node l v r => scons v (vs_union (vs_elems l) (vs_elems r))`) —
  a left-spine-only `vs_elems` is the degenerate trap (§6.1) and is a FAIL.
  In the `.ml` block: `vs_union` + `vs_mem_union`, then the bridge
  `vs_mem_elems : vs_mem x (vs_elems t) = bmem x t` (by induction).
- **Ops (all call the real backend):**
  `empty : unit -> t{ vs_isempty _ }` (returns `Vset_bst.Leaf` inline);
  `mem x s : bool{ _ = vs_mem x s }` (wraps `Vset_bst.member`);
  `add x s : t{ vs_addspec _ x s }` (wraps `Vset_bst.insert`, re-matches the
  result into constructors for the inline via injection — the `triset.ml`
  pattern; **no `(* *)` in the block**).
- **Algebra (R5 — 2 laws, over the ISet model):**
  `vs_mem_empty : ¬ vs_mem x empty`-equivalent (via `isempty`) and the
  membership characterization is carried by `vs_addspec` + the bridge. Ship
  `vs_mem_add`-style membership law only if a client needs it beyond
  `addspec`. **`vs_card` is dropped from v1** — a cardinality over a
  list-shaped model that admits duplicates is degenerate; defining it
  faithfully needs a whole-tree count or a nodup invariant. Deferred to v1.1
  with a note (better absent than degenerate).
- **Trust:** none (inherits none — `Vset_bst` is fully proved).
- **Naming hazard:** `Vset` co-travels `Vset_bst`'s `b*` names to clients; the
  `b*` prefix keeps `client_set_of_list` (which also pulls `Vlist`'s `ll_*`)
  collision-free.

### Wave 3 clients

- **client_set_of_list.ml** (integrator): a goal mentioning both `ll_mem` and
  `vs_mem` that discharges only from the two shipped algebras — a genuine
  cross-model goal, not two independent one-liners (reviewer removes one
  module's law and confirms the goal fails; §6.6).
- **client_opt_result.ml** (integrator): composes `Vresult.get_ok_or` and
  `Voption`, goal over `vr_is_ok`/`vo_is_some`.
- **smoke_<Module>.ml** (each builder): forces every shipped law of that
  module to fire. Exemplar `wave2/smoke_vset.ml`: `after_add_has_x x s =
  Vset.mem x (Vset.add x s) : bool{ _ = true }` forces the quantified
  `vs_addspec` to instantiate at `y = x`.

---

## 4. House conventions (definitive)

### The interface style: the obligation pattern is the DEFAULT

The user has ruled the *axioms-in-`.mli` / proofs-in-`.ml`* obligation
pattern the correct interface-hygiene mechanism. It is the **default for all
client-facing laws**. Canonical exemplar for a **via-abstract** module:
`testsuite/tests/vox/lib/viaob.{mli,ml}` (restated model defs in the `.ml`
are **not** marked `public`; the laws become `theorem`s, not axioms). For an
**opaque** module: `oset.{mli,ml}`.

- **`.mli` `[%%vox.lean]` block contains ONLY:** (1) the model sort; (2)
  `@[grind, expose] public def`s for the model vocabulary clients compute
  with, only the ones clients need; (3) `public axiom`s for the client-facing
  laws, each with a `grind_pattern`/`@[grind]` (matrix row 5: un-attributed =
  silently inert); (4) nothing else — no proofs, no private lemmas, no
  scaffolding defs.
- **`.ml` `[%%vox.lean]` block contains:** (1) the model defs **restated**
  *without* `public` (the model-duplication tax — file one note/module), the
  via abstraction `def`, any representation predicate/bridge; (2) unlimited
  **private** scaffolding lemmas (no `public`, no `.mli` twin); (3) the
  **same-named, same-typed `theorem`s** discharging each `.mli` axiom, with
  identical attribution (the seal re-elaborates the interface and demands
  these; drift is caught — matrix row 6).

**Inline exception** (a `public theorem` in the `.mli`): permitted **only**
for a leaf module with *no private scaffolding* and *one-line proofs*
(`Voption`/`Vresult`/`Vint` may inline their definitional laws). When in
doubt use obligation form. Reviewers flag inline theorems that carry
non-trivial proofs or drag scaffolding into the `.mli` (the `ptrie`
anti-pattern: 382-line `.mli`, 24 inline theorems, 22 of them scaffolding).

### Rev. 3 amendments (2026-07-06, wave-1 evidence — team-lead approved)

Two house rules, each with dual-module evidence from the wave-1 build
(consolidated in `vox_stdlib/LANGUAGE_NEEDS.md`):

- **(A) EXPOSE a model def only when it stays load-bearing.** *SHARPENED by
  the Phase-C soundness review (2026-07-06) — this is the precise rule, and it
  is a MUST:* a model `def` may be `@[grind, expose] public` **only if it
  recurses over the abstract argument** (so `grind` cannot unfold it past its
  first step and its laws stay live — `Vlist`'s `ll_app`/`ll_len`). A **non-
  recursive** def (a prepend, a head-match, an `if-then-else`, a constructor
  wrapper — `Vmap`'s `m_add`/`m_find`, `Vint`'s `vi_min`, `Vlist`'s `ll_cons`)
  MUST be **`public` WITHOUT `expose`** (or a `public axiom`), so a client can
  still name it in a spec but `grind` treats it opaquely. **AND: for any
  op-spec vocabulary that clients must *compute* with, ship the reduction laws
  explicitly** (e.g. `vo_get_some`, `vr_get_err_err`) — a de-exposed def gives
  the client an opaque symbol, so the reductions it needs (`get (Some x) = x`)
  must be shipped as laws, not left to unfolding. Exposing a non-recursive def
  lets a client's `grind` discharge every law about it *by unfolding*, so the
  shipped algebra is **silently dead** — this is not caught by the smoke-
  client-compiles check (the smoke passes via unfolding); it is caught only by
  the §6.7 **removal test** (delete the law, the goal must then fail). The
  Phase-C review found Voption/Vresult's laws ALL dead and Vlist's two
  cons-laws dead this way. Do **not** cargo-cult `reflectbits`'s
  `@[grind, expose]`. *Evidence: `notes/vint.md`, `notes/vmap.md`, and the
  Phase-C soundness review; `vox_stdlib/LANGUAGE_NEEDS.md` M3.*
- **(B) Widen the inline default to all definitional leaf ADTs.** The inline
  exception above now explicitly covers **any** definitional leaf ADT (no
  scaffolding, one-line proofs), not just the three named. A `public theorem`
  in the `.mli` is proved once and is **not** re-demanded by the seal, so
  inline pays **M1 = 0**; obligation form there duplicates each law statement
  for **no** added soundness. *Evidence: `notes/vresult.md` A/B — Vresult
  inline (M1 = 0) vs the byte-identical obligation control; contrast Voption's
  obligation form paying M1 = 3 on the same shape.* Obligation form remains
  the default whenever there **is** private scaffolding or a multi-line proof.

### Block-syntax rules (probe-grounded gotchas)

- **No OCaml `(* *)` comments inside a `[%%vox.lean]` block.** Lean parses `(`
  as a term and rejects it; the error points at "line N of the block", not
  the comment. Use Lean `--` line or `/- -/` block comments. (Root cause of
  the rev.-1 wave-2 failure; `rootcause_matrix.md`.)
- **In a block law, reference an exposed-ADT constructor DOTTED** — `.Vsome x`
  or `Vox_<Unit>_t.Vsome x`, never bare `Vsome x` ("Function expected at
  Vsome … unknown"). (§3 Voption/Vresult; probe `vopt_b`.)
- **Specs mention `def`s, never model constructors** in a *refinement* —
  `{ _ = ll_cons x l }` needs `def ll_cons := .LCons x l`; a bare model
  constructor there elaborates to "Function expected at".

### Model-fidelity rules for list-shaped set/map models (mechanized in §6.1)

- **Membership/find-based specs, not structural**, for any inductive-list
  set/map model: client-facing op specs are `∀ y, …`-style membership/find
  agreement (`vs_addspec`, `m_find_*`), **never** `{ _ = ins x s }`/`{ _ =
  m_add k v m }` over a repr whose structural `=` is not the model's equality
  (a **tree** repr). *Exception:* when the op genuinely **is** the structural
  operation on a **cons-list** repr (Vmap's `add` = `MCons` prepend), the
  structural spec is faithful and allowed (soundness M-2).
- **The via abstraction fn must recurse into every data-bearing field.** A
  constructor argument bound to `_` that holds elements/keys is a degenerate
  abstraction (the `via_set` trap) — FAIL.
- **A via face claiming a backend must call the backend's ops** and ship the
  bridge theorem. A face that hand-builds constructors and never calls the
  backend is not a face — FAIL.
- **No `.mli` may expose `type t = <backend repr>`.** Keep the face's
  `type t : value refines (iset)` abstract so a client cannot inject a raw
  backend value into it (the abstraction boundary holds precisely because the
  via coercion only type-checks inside the face's `.ml`).

### Naming discipline (R6 + global-uniqueness hardening)

- Model **sort**: `TitleCase` (`LList`, `ISet`, `MList`, `MOpt`).
- Spec/model **def**s: lowercase, **per-unit prefixed** so all co-importable
  public names are globally unique (design F2): Vlist → `ll_*` · Voption →
  `vo_*` · Vresult → `vr_*` · Vint → `vi_*` · Vset_bst → `b*` · Vmap → `m_*`
  · Vset → `vs_*`.
- **Never** name a def after a Lean-core identifier (`repr`, `id`, `min`,
  `max`, bare `length`). The per-unit prefix already avoids this.
- **Share a model by importing the one defining unit** (`open`), never by
  redefining it. `Vset` imports `Vset_bst`'s `b*`; no unit redefines
  another's public names.

### The `total_` caveat (when NOT to use it)

`total_` does **not** compose with a block law (matrix row 11). Any measure
appearing in a client-facing law must be a block `public def`, never
`total_`. None of the v0/v1 modules need `total_`, so it is **unused this
build**; reaching for it signals an imminent row-11 failure — use a block def
and file a note.

### File / directory layout

```
vox_stdlib/
  Vlist.ml  Vlist.mli   ...   Vset.ml Vset.mli
  notes/<Module>.md          # one per module, append-only, builder-owned
  clients/smoke_<Module>.ml  # per-module smoke goal, builder-owned
  clients/client_*.ml        # cross-module clients, integrator-owned
  _artifacts/                # integrator: cmi + VoxSig_*.olean of completed
                             #   modules (the dep surface); NOT committed
  LANGUAGE_NEEDS.md          # Phase C consolidation target (stub to start)
  BUILD.md                   # the verify recipe (§7)
```

Module files are **capitalized** (solver sorts are `Vox_Vlist_…`). A wave-1/2
builder touches **only** `vox_stdlib/<Module>.{ml,mli}`, `notes/<Module>.md`, and
`clients/smoke_<Module>.ml` — all disjoint. `_artifacts/`, the two shared
`.md`s, and `clients/client_*.ml` are integrator-owned.

---

## 5. LANGUAGE-NEEDS note format (per-module, mechanical to consolidate)

Builders **append** one block per pain-site to their own
`vox_stdlib/notes/<Module>.md` (never a shared file). The integrator concatenates
them into `vox_stdlib/LANGUAGE_NEEDS.md` in Phase C. **Exact format:**

```
### <Module> · <short title>
- **site:** vox_stdlib/<Module>.ml:<line>  (or .mli)
- **milestone/gap:** #31 | #32 | M1 | M2 | M3 | N1 | N2 | model-dup | new
- **what I tried:** <the natural code that failed, 1-2 lines>
- **error:** <verbatim compiler/solver message, salient line>
- **workaround used:** <the house pattern applied>
- **removed by:** <language support that would delete the workaround>
- **severity:** BLOCKING | MAJOR-ERGONOMIC | MINOR | COSMETIC
```

Pre-seeded expectations (cite the gap #; a site that "just worked with the
documented workaround" still gets a one-line note — that is the evidence the
workaround is load-bearing):

- **#31** at every recursive via-returning op (`Vlist.append`; `Vmap.add` if
  it threads a via result): recursive result loses its map at the `let`.
  Workaround: helper returns a refined **skeleton** `repr{ absfn _ = <image
  eq> }`, inject into `t` once through a variable (design §7.2; `vlist.ml`
  `append`).
- **#32** *only* at a **bind-then-branch on a spec'd bool** (`let b = go l in
  if b then …`), e.g. an OR-style membership over two subtrees. **NOT** hit by
  a tail-recursive one-path search (`if x<v then member x l else member x r`,
  the `Vset_bst.member` shape) — do not file a #32 note there. Workaround
  where it bites: explicit matching / restructure.
- **M1 (statement typed twice)** once per module: `.mli` axiom + `.ml`
  theorem verbatim-duplicated — cite the count.
- **model-dup** once per module: model `def`s authored in both blocks.
- **C1 named-call-result injection**, **C3 `*unknownN*` placeholders** as met.

---

## 6. Phase C reviewer checklist

Two independent reviewers per module (adversarial). A module **passes** only
if all hold; a reviewer who cannot verify a point marks FAIL, not "looks ok".
Items 1a–1c are **mechanical** (grep/read, not judgement).

1. **Spec honesty (no tautology).** Each shipped law must *constrain* the
   impl (real: `ll_len l >= 0`; vacuous: `ll_len l = ll_len l`, a `{ 0 = 0 }`
   op contract). Plus the three mechanical model-fidelity checks:
   - **1a. Abstraction-fn totality:** the via abstraction `def` recurses into
     **every** data-bearing constructor field; an element/key-bearing field
     bound to `_` is an automatic FAIL (catches `.Node l v _ => … (elems l)`,
     the degenerate `via_set`/rev-1 trap).
   - **1b. Membership-based specs for list models:** a set/map over an
     inductive-list model has `∀ y, …` membership/find specs, **not**
     structural `{ _ = ins x s }` — *unless* the op is a genuine cons-list
     prepend (Vmap), where structural is faithful. A structural set/map spec
     over a **tree** repr is a FAIL.
   - **1c. Face-uses-backend:** a via face claiming a backend calls the
     backend's `insert`/`member` (grep) and ships the bridge theorem; a face
     that hand-builds constructors and never calls the backend is a FAIL.
2. **Algebra completeness (R5).** All required law kinds present (measures:
   nonneg + measure-of-combine; membership: membership-of-ctor + ordering
   not-membership; invariant: preservation at every production site).
   Cross-check the §3 list; a missing law FAILs even if what ships verifies.
3. **Trust ledger empty.** Grep the `.ml` for `assume_unchecked_` and bare
   `axiom` (both → FAIL this build — every module is zero-trust); grep the
   `.mli` for `axiom` (obligations — the module compiling means the seal
   discharged them). No `Vbits` in this build, so **no** module may carry an
   assumed masking axiom; one appearing is a scope violation.
4. **Interface hygiene.** `.mli` block = sort + `public def` vocab +
   attributed `public axiom` laws, nothing else; every `.mli` axiom carries
   `grind_pattern`/`@[grind]`; `.mli` length proportional to the *interface*
   (the `ptrie` 382-line smell test). **No `.mli` exposes `type t = <backend
   repr>`** (§4).
5. **Naming discipline.** Every public def/sort carries the unit prefix; no
   Lean-core collisions; no unit redefines another's public name; shared
   models are `open`-imported.
6. **R7 client gate.** `client_set_of_list` has a goal that *needs* both
   modules' laws — reviewer deletes one module's law and confirms the goal
   fails. Not two independent trivialities.
7. **Every shipped law is LIVE — the removal test, not just consumption.**
   *SHARPENED by the Phase-C soundness review:* a passing `smoke_<Module>.ml`
   is **necessary but NOT sufficient** — an exposed non-recursive def lets
   `grind` discharge the smoke goal *by unfolding*, so the smoke passes while
   the law is dead. The liveness gate is the **removal test on EVERY law**:
   delete the law (its `.mli` axiom/theorem and any `.ml` twin), recompile the
   smoke/consumer, and confirm the goal **now fails**. A law whose removal
   leaves its goal passing is dead — FAIL (de-expose its subject def per
   Amendment A, or drop the law). This is what caught Voption/Vresult (all
   laws dead) and Vlist (two cons-laws dead) after the smoke-consumption check
   passed them. (Caveat, from `notes/vint.md`: for a set of *inter-derivable*
   mandated laws — `vi_min_le_left`/`_le_right` under `vi_min_comm` — removal
   of one may leave the goal provable via the others; that is acceptable when
   both are §3-mandated for ergonomic completeness. Read the removal test as
   "no law is derivable purely by *unfolding an exposed def*", the deadness
   the review targets, not "every law is logically independent".)
   **7a — INVARIANT laws need a SYMBOLIC-argument forcing goal** (Phase-C
   eliminator wave, build-vsetbst): an invariant law like `bok (bins x t)` /
   `bok (bdel x t)` is NOT forced by an op's refined result (the op hands the
   client the invariant for free, and the module discharges its own obligation
   via per-site scaffolding — the module SEALS with the law deleted: a
   silently-dead OBLIGATION). Force it with a goal over a **symbolic** argument
   — `unit{ bok (bins x s) }` for an abstract `s` — so grind cannot induct on
   the variable and must use the law. Every invariant-preservation law gets
   such a smoke goal. (The mechanized §6.7 harness WARN only catches dead
   *non-recursive exposed* defs; dead *recursive invariant* laws are caught
   ONLY by this removal test — the WARN is a proxy, the removal test is the
   gate.)
8. **Verification real, not skipped.** Compiled with `-vox-solver-path <lean>`
   (not `-vox-dry-run`); rejections happen at the intended layer.
9. **Notes filed.** `notes/<Module>.md` covers the module's pre-seeded sites
   (a recursive via op with no #31 note is suspicious; a tail-recursive
   search with a spurious #32 note is miscalibrated — §5).

---

## 7. Build / verify mechanics for builders

### Locate Lean (once, per the testsuite recipe)

```sh
PINNED=/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean
LEAN="${VOX_LEAN:-$(command -v lean 2>/dev/null || echo "$PINNED")}"
OC=/usr/local/home/jujacobs/oxcamls/vox-stdlib/_install/bin/ocamlc.opt
export TMPDIR=/usr/local/home/jujacobs/tmp   # big disk; solver scratch
```

### The ONE dep-injection style: copy artifacts into a private build dir

`.mli` first, then `.ml`, **in a private temp dir** (avoids the shared
`VoxCore.olean` write race when wave-1 builders run concurrently; the
compiler writes `VoxCore.olean` + `VoxSig_<Module>.olean` into the CWD). We
use **copy-into-CWD** (not `-I`) as the single style, and **fail loudly** if a
declared dependency's olean is missing rather than silently compiling without
it:

```sh
B=$(mktemp -d)
cp vox_stdlib/<Module>.mli vox_stdlib/<Module>.ml "$B"/
# For a dep-bearing module / composition client, list each dependency
# explicitly and fail if absent. Resolve the .cmi CASE-INSENSITIVELY: it
# tracks the dep's SOURCE-file casing (Vlist.cmi but voption.cmi), so try the
# module-name casing then the all-lowercase variant. Do NOT lowercase only the
# first letter -- that turns Vlist.cmi into the nonexistent vlist.cmi. The
# VoxSig olean is ALWAYS capitalized (derives from the OCaml module name).
for dep in <Dep1> <Dep2>; do          # e.g. Vset needs Vset_bst; a composition client needs each opened module
  cmi="vox_stdlib/_artifacts/$dep.cmi"
  [ -f "$cmi" ] || cmi="vox_stdlib/_artifacts/$(printf %s "$dep" | tr 'A-Z' 'a-z').cmi"
  cp "$cmi" "$B"/                          || { echo "MISSING cmi for $dep"; exit 1; }
  cp "vox_stdlib/_artifacts/VoxSig_$dep.olean" "$B"/ || { echo "MISSING VoxSig_$dep.olean"; exit 1; }
done
( cd "$B"
  $OC -vox-solver-path "$LEAN" -c <Module>.mli    # declares obligations
  $OC -vox-solver-path "$LEAN" -c <Module>.ml )   # seal discharges them
```

Wave-1 modules have **no** dep loop. A passing `.ml` means the seal
re-elaborated the `.mli` and every axiom was discharged by a same-named
theorem. (Packaging probe: cmi + `VoxSig_<Dep>.olean` in the CWD is the sole,
sufficient dependency surface; no source needed, and **`VoxCore.olean` is NOT
required in the build dir — the compiler regenerates it every run**, verified.
`-I <depdir>` also works but we standardize on copy-into-CWD so there is
exactly one recipe.) The `check_wave1.sh`/`check_wave2.sh` harnesses already
resolve dep cmis case-insensitively via their `find_ci` helper.

### "Done" for a wave-1/2 builder

(a) `.mli` and `.ml` compile clean with the real solver; (b)
`clients/smoke_<Module>.ml` compiles against the module's own artifact and
forces each shipped law; (c) `notes/<Module>.md` filled; (d) wave-2 verified
against `_artifacts/`, not sources.

### Integrator, per wave

1. Re-verify each completed module in a clean temp dir.
2. Copy its `<Module>.cmi` + `VoxSig_<Module>.olean` into `vox_stdlib/_artifacts/`.
3. Verify that module's `smoke_<Module>.ml` against the fresh artifact.
4. Commit the wave (builders do **not** commit): sources + notes + smoke
   clients, on branch `vox-stdlib-v1`. `_artifacts/`, `VoxCore.olean`, and
   `VoxSig_*.olean` are build output — do **not** commit them.
5. After wave 2, verify `client_set_of_list` + `client_opt_result`.

### Cost expectations

Honest via/opaque module ~0.5–1 s of Lean (startup-dominated); `Vset_bst`
(6 by-induction laws) and `Vset` a few seconds. Iterate with direct `-c`; do
**not** run `make test-one` (17 s+ even unchanged).

---

## 8. The three riskiest decisions (stated for attack)

1. **`Vset` = a via face over an exposed-ADT `Vset_bst` backend, in the
   `triset` shape (membership-based specs, whole-tree abstraction, real
   backend calls, bridge theorem).** *Why:* the R7 probe proves a downstream
   unit cannot unpack an upstream's hidden repr, so a via face needs an
   exposed-repr backend beneath it; and the `ISet` list model's structural
   `=` is not set equality, so specs must be membership-based (the rev.-1
   structural `{ _ = vs_ins x s }` was mathematically satisfiable only by a
   degenerate left-spine `add` — the exact trap §6.1 rejects, and it is fixed
   here, verified `wave2/*`). *Risk:* a wave-2 dependency edge + public
   backend repr. *Mitigation:* `Vset_bst` is a backend tier (public repr
   intended), the `b*` prefix prevents co-import collisions, and the `.mli`
   never exposes `type t = <repr>` so the boundary holds. *Alternative
   rejected:* a self-contained full-membership `via_set` upgrade — simpler but
   throws away the v1.1 backend-swap and the R7 demonstration.
2. **`Vbits` is DEFERRED; `Vint` (zero-trust) takes its wave-1 slot; `Vmap`
   is a cons assoc-list, not graduated `htbl`/ordered-tree.** *Why:* an `.mli`
   masking axiom is an **obligation**, not an assumption — and a reflected
   bit-op has no computable `def` to discharge it, so `Vbits`'s laws are
   **undischargeable in the `.mli`** (probe-confirmed) and useless to clients
   if kept `.ml`-local; it is also the only TCB expansion in a
   prove-the-concept v1. `Vint` reflects min/max/abs to *proved* `def`s
   (`reflectbits`), so the stdlib ships **zero** trust. `Vmap` as an
   ordered-tree with a structural `add` spec is the same degenerate trap as
   rev.-1 `Vset`; a cons assoc-list makes the structural spec faithful and
   drops effort to Vlist-level. *Risk:* the stdlib no longer demonstrates the
   reflected-trust story or hashing. *Mitigation:* `Vbits` becomes a dedicated
   spike with a LANGUAGE_NEEDS entry ("need an assumed-axiom `.mli` export
   form so a reflected op's laws can ride to clients") — real evidence for a
   language gap; `htbl`-hashing and ordered `Vmap` move to v1.1 as backends
   behind `Vmap`'s interface (the `Vset` pattern), so nothing is lost, only
   sequenced. *Alternative rejected:* ship `Vbits` with `.ml`-local axioms —
   gives clients an uninterpreted `vb_land` with no laws (useless) and expands
   the TCB.
3. **Obligation pattern mandated as default even where inline `public
   theorem`s work.** *Why:* the user ruled it the correct hygiene mechanism;
   uniformity makes the reviewer checklist mechanical and interfaces lean.
   *Risk:* increases the M1 duplication tax on trivial modules.
   *Mitigation:* the narrow inline exception for scaffolding-free
   one-line-proof leaf modules (`Voption`/`Vresult`/`Vint`), and M1 is a
   first-class LANGUAGE_NEEDS item we *want* evidenced. *Alternative
   rejected:* "inline where trivial, obligation where heavy" — fuzzier,
   inconsistent interfaces.

---

## 9. Roadmap position

This build = design-doc **v0 + v1**, minus the §1 deferrals. On completion:
seven wave-1 + one wave-2 modules verified honestly (**zero** trust items),
per-module smoke clients, two cross-module clients, a consolidated
`LANGUAGE_NEEDS.md`. **v1.1** (separate build): `Vbits` (assumed masking
axioms, gated on the language gap in §8.2), `Vset_rbt`/`Vset_trie` behind
`Vset`, `htbl`-hashing + ordered-tree behind `Vmap` (the backend-swap
showcase), mutable containers behind the borrow lib. **v2** is the generic
ordered functor (`Make(Ord)`), gated on compiler work (B1) and out of reach
today.
