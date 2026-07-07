# Vset — language/compiler needs (build notes)

Module: via-ABSTRACT finite-set FACE over the `Vset_bst` BST backend (wave 2,
the stdlib's in-tree R7 cross-module composition). `t : value refines (iset)`;
model `ISet` (inductive list); whole-tree abstraction `vs_elems`; bridge
`vs_mem_elems : vs_mem x (vs_elems t) = bmem x t`. The core ops CALL the real
backend (`Vset_bst.member`/`Vset_bst.insert`). Verified against ONLY
`Vset_bst.cmi` + `VoxSig_Vset_bst.olean` (backend source never read); smoke
verified against `Vset.cmi` + `VoxSig_Vset.olean` (+ backend artifacts),
sources deleted. Compiler: `_install/bin/ocamlc.opt`, pinned Lean 4.31.0.

Ops as shipped: `empty`, `add`, `remove`, `mem`, `elements`. `remove` (Mech C,
closed add/remove producer algebra) wraps the backend's `Vset_bst.remove`
(model `bdel`), carried across the bridge by `bmem_delete` under the set's `bok`
invariant. `elements` (eliminator addendum Mech A) enumerates into a `Vlist.t`,
membership-bridged. ALL Commit-B Vset steps now shipped (elements + subset/equal
+ remove).
Algebra/vocab as shipped (all LIVE — no separate axiom laws): op specs
`vs_isempty` (empty), `vs_addspec` (add), `vs_removespec` (remove),
`vs_elements_spec` (elements), carried across the in-`.ml` bridges `vs_mem_elems`
(= backend `bmem`) + backend `bmem_insert`/`bmem_delete` and `vs_tolist_spec`;
plus the relational F-3 defs `vs_subset` / `vs_equal` (quantified spec vocabulary
a client consumes as a goal). Each proven load-bearing by deletion (remove the
spec/def ⇒ its smoke goal fails; the `vs_equal→vs_subset` block theorem is
non-vacuous; unconditional `vs_subset`, a `= true` removed_absent, and a
hypothesis-free survivor theorem are all rejected).

Eliminator + relational work imports Vlist's `LList`/`ll_mem` alongside this
face's own `ISet` — TWO upstream via-models in one interface block (a new R7
combination; STEP-0 probe passed before build).

VERIFIED against the FINAL refreshed artifacts (backend with remove/bdel/
bmem_delete; Vlist final with `ll_isnil` de-exposed, `empty : t{ _ = ll_nil }`,
`ll_nil` opaque). This required an adaptation from my first eliminator cut (see
the two Vlist-interface note blocks below) — I re-ran the full seal + smoke +
liveness + negative-control suite against the fresh artifacts.

---

### Vset · #31 pre-seed REFUTED at `add`; simpler via-coerce now compiles
- **site:** vox_stdlib/Vset.ml:79-86 (`add`)
- **milestone/gap:** #31 (NOT hit — calibration; the case the program wants)
- **what I tried:** the blueprint pre-seeds `Vset.add` as a possible #31 site
  ("recursive via-returning op loses its map at the `let`"). I probed the
  natural fix-assuming form directly:
  `let r = Vset_bst.insert x t0 in (r : t{ vs_addspec _ x s })` (Variant A).
- **error:** none — Variant A **COMPILES** on this #31-less clone. `add` does
  not thread a *via* result through a `let`: `r` is bound at the backend
  skeleton type `Vset_bst.set`, and the via injection is the coercion
  expression itself, so no via value ever crosses a binder. (Same structure as
  Vmap's prepend `add`, whose note likewise refutes the #31 pre-seed.)
- **workaround used:** none required for #31. The shipped `add` re-matches the
  backend result into inline constructors (the mandated triset.ml shape, §3),
  which is a superset of Variant A; Variant A (coerce the let-bound backend
  variable) is a valid **de-contortion** — the inline-ctor re-match is no
  longer necessary on this compiler.
- **removed by:** n/a for #31 (does not bite). The inline-ctor re-match is
  removed by: gap #31 fix, landed 7afa45262 — but note that on THIS clone the
  simpler coerce-a-let-bound-variable form already works, so the re-match is
  redundant here independent of the #31 landing.
- **severity:** none (calibration; refutes a pre-seeded #31 expectation)

### Vset · C1 named-call-result injection: inline coerce of a call mis-sorts
- **site:** vox_stdlib/Vset.ml:82 (the `let r = Vset_bst.insert x t0 in` binding)
- **milestone/gap:** C1 (named-call-result injection)
- **what I tried:** inline the backend call directly inside the via coercion,
  skipping the binding: `(Vset_bst.insert x t0 : t{ vs_addspec _ x s })`
  (Variant B).
- **error:** `vox: verification failed -- NOT PROVED` /
  `lean: error: Application type mismatch: The argument` — the call-result
  expression is injected at the via image sort before it is named, and the
  argument to the via coercion mis-elaborates.
- **workaround used:** let-bind the backend result first
  (`let r = Vset_bst.insert x t0 in …`), then inject the **variable** `r`. The
  shipped `add` and `mem` both bind before injecting.
- **removed by:** auto-naming a call-result passed into a via coercion / a
  dependent position (same fix Vmap's C1 note asks for).
- **severity:** MINOR

### Vset · via value produced only inside a function body (empty is a `fun`)
- **site:** vox_stdlib/Vset.ml:64-67 (`empty` is `fun u -> (Vset_bst.Leaf : t{…})`)
- **milestone/gap:** new (via-injection sorting)
- **what I tried:** a top-level via-typed VALUE binding for `empty`
  (`let empty : t{ vs_isempty _ } = (Vset_bst.Leaf : t{…})`), the natural
  spelling of a nullary constant.
- **error:** a top-level via-typed value records a mis-sorted definitional fact
  (image name bound to the skeleton rhs) — the documented triset finding.
- **workaround used:** make `empty` a function `(u : unit) -> t{…}` and produce
  the via value inside the body (matches triset.ml / the wave-2 exemplar).
- **removed by:** allow a top-level via-typed value binding to sort at its
  image without the skeleton-fact artifact.
- **severity:** MINOR

### Vset · no #32 at `mem` (one-path search — pre-seed calibration)
- **site:** vox_stdlib/Vset.ml:69-72 (`mem` wraps `Vset_bst.member`)
- **milestone/gap:** #32 (NOT hit — calibration)
- **what I tried:** `mem` unpacks the via `t` with `refine_` and calls the
  backend's one-path search `Vset_bst.member x t0`.
- **error:** none. The OR-over-two-subtrees membership that would trigger #32
  lives inside `Vset_bst.member` (a wave-1 backend op, already proved), not in
  this face; `mem` here is a single tail call with no bind-then-branch on a
  spec'd bool. Per §5, a tail one-path search gets no #32 note beyond this
  calibration line.
- **workaround used:** none needed — tail one-path call, no bind-then-branch on a spec'd bool.
- **removed by:** n/a.
- **severity:** none (calibration)

### Vset · model theory authored in both blocks
- **site:** vox_stdlib/Vset.mli:39-49 and vox_stdlib/Vset.ml:20-55
- **milestone/gap:** model-dup
- **what I tried:** state the `ISet` inductive + `vs_mem`/`vs_isempty`/
  `vs_addspec` once.
- **error:** none — required by the pattern. The `.mli` declares the inductive
  + the three exposed set-vocabulary defs a client computes with; the `.ml`
  restates all of them (so its private bridge can be proved) and adds the
  private scaffolding (`vs_union`, `vs_elems`, `vs_mem_union`, the bridge
  `vs_mem_elems`) that must NOT leak to the `.mli` (interface hygiene §4). 1
  inductive + 3 defs duplicated; 4 private scaffolding decls `.ml`-only.
- **workaround used:** dual authoring; `.ml` defs register under the same
  solver names.
- **removed by:** a shared model-theory include, or letting the `.ml` import
  the `.mli`'s inductive/decls rather than redeclaring them.
- **severity:** MAJOR-ERGONOMIC

### Vset · M1 = 0 — algebra carried by op specs, no axiom laws typed twice
- **site:** vox_stdlib/Vset.mli:52-54 (val specs), no `public axiom` in the block
- **milestone/gap:** M1 (positive result — recorded to contrast Vmap/Vset_bst)
- **what I tried:** ship the set algebra as separate `public axiom` laws
  (`vs_mem_add`, `vs_mem_empty`) the way Vset_bst ships `bmem_insert`.
- **error:** none, but unnecessary. The membership characterization a client
  needs is exactly `add`'s postcondition `vs_addspec` and `empty`'s
  `vs_isempty`; both are exposed set-vocabulary defs, and the bridge lets the
  backend's `bmem_insert` discharge `add`'s VC in the `.ml`. Shipping extra
  axiom laws would duplicate statements for no added soundness (blueprint §3:
  "ship `vs_mem_add` only if a client needs it beyond `addspec`").
- **workaround used:** none — ship no separate axiom laws; the op specs ARE the
  algebra. So Vset pays **M1 = 0** (unlike Vset_bst's two obligation laws), the
  same win Vresult's inline form records.
- **removed by:** n/a (positive result — a face whose algebra is its op specs
  needs no obligation duplication).
- **severity:** none (positive result)

### Vset · TWO upstream via-models in one face (STEP-0 R7 probe)
- **site:** vox_stdlib/Vset.mli:37 (`open Vlist`) + :61 (`vs_elements_spec` over the
  imported `LList`/`ll_mem` and the own `ISet`); vox_stdlib/Vset.ml:24, :65
- **milestone/gap:** new (R7 two-import extension of uset/dcount)
- **what I tried:** import Vlist's `LList`/`ll_mem` into Vset's interface block
  (via `open Vlist`) alongside Vset's own `ISet`, and define+ship
  `vs_elements_spec (l : LList) (s : ISet) : Prop := ∀ x, ll_mem x l = vs_mem x s`
  — a spec that mentions BOTH models, needed for the `elements` eliminator.
- **error:** none — the `.mli` elaborates and produces `VoxSig_Vset.olean`, the
  `.ml` seal re-elaborates the two-import block, and a round-trip theorem
  `vs_elements_spec (vs_tolist t) (vs_elems t)` discharges by `induction t <;>
  grind` (two-model reasoning under grind, not just elaboration). The uset/dcount
  probe showed ONE upstream via-model can be referenced downstream; this
  confirms the two-model-in-one-face extension works.
- **workaround used:** none needed — reference each unit's public solver names
  as written (`ll_mem`, `ll_cons`, `ll_app` from Vlist; `vs_mem` own).
- **removed by:** n/a (positive result — no language support needed).
- **severity:** none (positive R7 result)

### Vset · #31 NOT hit at `elements` (client-of-Vlist composition) — budget refuted
- **site:** vox_stdlib/Vset.ml:131-138 (`vs_go`: recursive, returns `Vlist.t`,
  threads recursive via results `a`/`b`/`ab` through lets)
- **milestone/gap:** #31 (NOT hit — calibration; refutes the addendum budget)
- **what I tried:** the eliminator addendum budgets `elements` like
  `Vlist.append` — "a recursive via-returning op → the #31 skeleton-thread
  workaround applies, this clone is pre-#31." I wrote the natural recursion:
  `let a = vs_go l in let b = vs_go r in let ab = Vlist.append a b in
  Vlist.cons v ab`, with `vs_go`'s result pinned to `Vlist.t{ _ = vs_tolist t0 }`.
- **error:** none — it compiles. #31 does NOT bite. The reason: `a`/`b`/`ab` are
  OPAQUE cross-unit `Vlist.t` via values (sealed, from another unit), whose via
  image rides as an ordinary refinement FACT that survives a `let` normally.
  #31 is a PRODUCING-unit phenomenon — a *transparent* Trefine let binder losing
  its skeleton map — which is why `Vlist.append` (recursing over Vlist's OWN
  transparent repr, inside Vlist.ml) needed the skeleton-thread workaround but a
  CLIENT composing Vlist's sealed ops does not. The skeleton-thread workaround is
  not available to a client anyway (the backend/Vlist repr is hidden), and it is
  not needed.
- **workaround used:** none for #31. The recursion pins `vs_go`'s result image
  to `vs_tolist t0` (built in Vlist's own `ll_cons`/`ll_app` vocab), so each
  step's VC is local and `elements` reduces to `vs_tolist_spec`.
- **removed by:** n/a (#31 does not bite this shape; recorded to sharpen the rule
  — #31 is producing-unit transparent-via only, not client via-composition).
- **severity:** none (calibration; refutes a pre-seeded #31 budget)

### Vset · C1 at `elements` construction — Vlist.append/cons args must be let-bound
- **site:** vox_stdlib/Vset.ml:135-138 (`let a`/`let b`/`let ab` before `Vlist.cons`)
- **milestone/gap:** C1 (named-call-result injection — the dominant friction)
- **what I tried:** inline the recursive calls and the append result:
  `Vlist.cons v (Vlist.append (vs_go l) (vs_go r))`.
- **error:** `vox: the argument for a dependent parameter must be a variable or a
  pure expression the logic can name (let-bind it first)` (elaboration reject at
  Vset.ml:135) — `Vlist.append`/`Vlist.cons` have dependent params (their result
  spec mentions the args), so a call-result argument must be named first. This is
  an elaboration error, NOT a Lean-proof failure, which also confirms #31 is not
  the gate here.
- **workaround used:** let-bind every dependent-position call result
  (`let a = vs_go l in let b = vs_go r in let ab = Vlist.append a b in
  Vlist.cons v ab`).
- **removed by:** auto-ANF of pure call-result args passed to dependent params
  (the firm-wide cleanest win — this is now the 7th C1 site across the stdlib).
- **severity:** MINOR

### Vset · Vlist empty/nil base case — RESOLVED by Vlist's final interface (version calibration)
- **site:** vox_stdlib/Vset.ml Leaf arm of `vs_go` + `vs_tolist Leaf`
- **milestone/gap:** new (predicate-vs-structural-equality; artifact-version calibration)
- **what I tried:** my FIRST eliminator cut (against the then-current STALE Vlist
  artifact where `empty : t{ ll_isnil _ }` and `ll_isnil` was EXPOSED) used a
  local inversion lemma `vs_isnil_lnil : ll_isnil l → l = .LNil := by cases l <;>
  grind` to turn empty's predicate post into the nil constructor at the Leaf arm.
- **error:** against the FINAL Vlist (build-vlist's shipped interface: `ll_isnil`
  DE-EXPOSED, `empty : t{ _ = ll_nil }`, `ll_nil` opaque) that lemma's proof
  `cases l <;> grind` FAILS — `ll_isnil` no longer unfolds to refute the `.LCons`
  case (this is exactly the version-skew the architect flagged; I had first
  verified against a stale artifact — check artifact MTIME vs upstream source).
- **workaround used:** NONE needed on the final Vlist — the inversion lemma is
  DELETED. Because empty's post is now the structural `{ _ = ll_nil }`, and my
  `vs_go` pins its result to `{ _ = vs_tolist t0 }` with `vs_tolist Leaf = ll_nil`
  (see the opaque-wrapper block below), the Leaf arm's images match DIRECTLY. The
  bridge `vs_tolist_spec`'s base case `ll_mem x ll_nil = False` is discharged by
  Vlist's shipped `ll_nil_not_mem` (ambient by grind_pattern). Net: the final
  Vlist's structural empty spec + `ll_nil_not_mem` REMOVED the scaffolding my
  first cut needed — a clean de-contortion from the upstream fix.
- **removed by:** already removed (Vlist's final `empty : t{ _ = ll_nil }` +
  `ll_nil_not_mem`). Recorded as the calibration that a predicate-post `empty`
  costs a downstream inversion lemma, whereas a structural post does not.
- **severity:** MINOR (resolved; calibration value)

### Vset · vs_tolist must use Vlist's opaque wrappers ll_nil/ll_cons/ll_app, not bare .LNil/.LCons
- **site:** vox_stdlib/Vset.ml `vs_tolist` (`.Leaf => ll_nil`, `.Node => ll_cons v (ll_app …)`)
- **milestone/gap:** new (opaque-constructor-wrapper modeling)
- **what I tried:** naturally spelled `vs_tolist` with bare `LList` constructors
  (`.Leaf => .LNil`, `.Node l v r => .LCons v (ll_app …)`).
- **error:** the ROUND-TRIP would not close for `vs_go`'s image. `Vlist.empty`
  produces the image `ll_nil` and `Vlist.cons` produces `ll_cons v X`; BOTH
  `ll_nil` and `ll_cons` are OPAQUE (Vlist ships them `public` but NOT `expose`d,
  deliberately, to keep their laws live), so grind cannot prove `ll_nil = .LNil`
  or `ll_cons v X = .LCons v X`. A bare-constructor mirror diverges from the
  images the ops actually produce.
- **workaround used:** define `vs_tolist` ENTIRELY in Vlist's own vocabulary —
  `.Leaf => ll_nil`, `.Node l v r => ll_cons v (ll_app (vs_tolist l)
  (vs_tolist r))` — so it matches the images `Vlist.empty/cons/append` emit with
  no unfolding (both sides the same application). The bridge `vs_tolist_spec`
  rewrites `ll_mem` over it via Vlist's imported `ll_nil_not_mem` (base) and
  `ll_mem_cons`/`ll_mem_app` (step).
- **removed by:** n/a (correct modeling discipline; recorded because the natural
  bare-constructor spelling is a subtle trap whenever the upstream ships opaque
  constructor wrappers — and Vlist's final interface makes BOTH nil and cons
  opaque, so the rule now covers the empty case too).
- **severity:** COSMETIC (calibration — the honest form is a one-token change per arm)

### Vset · relational F-3 defs consumable as client goals (positive)
- **site:** vox_stdlib/Vset.mli:66-70 (`vs_subset`/`vs_equal`); consumers
  vox_stdlib/clients/smoke_vset.ml:36 (`add_is_superset`) and :40
  (`smoke_equal_to_subset`)
- **milestone/gap:** F-3 (positive result)
- **what I tried:** ship `vs_subset a b := ∀ x, vs_mem x a → vs_mem x b` and
  `vs_equal a b := ∀ x, vs_mem x a ↔ vs_mem x b` as `@[grind, expose] public`
  defs, and confirm a client consumes them as a goal/hypothesis WITHOUT writing
  its own quantifier (feasibility risk 3).
- **error:** none. `add_is_superset` returns `Vset.t{ vs_subset s _ }` and grind
  discharges it from `add`'s `vs_addspec` (the quantifier lives in `vs_subset`,
  not the client refinement); the client block theorem
  `vs_equal a b → vs_subset a b` discharges by `grind`. Both are proven
  load-bearing (delete `add`'s spec ⇒ superset goal fails; unconditional
  `vs_subset` is rejected — non-vacuous). `↔` in `vs_equal` seals fine.
- **workaround used:** none — exposed ∀-defs are the intended F-3 mechanism and
  are exempt from the non-recursive de-expose rule (a client cannot unfold the
  hidden set to trivialize them; the ∀ over `vs_mem` is the only route).
- **removed by:** n/a (positive result).
- **severity:** none (positive F-3 result)

### Vset · remove (Mech C) — closed add/remove algebra over the backend delete
- **site:** vox_stdlib/Vset.ml:131-138 (`remove`), :67 (`vs_removespec`);
  vox_stdlib/Vset.mli:61,79; smoke vox_stdlib/clients/smoke_vset.ml:43,49
- **milestone/gap:** new (Mech C; backend-delete composition)
- **what I tried:** `remove : (x:int) -> (s:t) -> t{ vs_removespec _ x s }`,
  `vs_removespec r x s := ∀ y, vs_mem y r = (y ≠ x ∧ vs_mem y s)`; impl wraps the
  backend `Vset_bst.remove` (model `bdel`) and re-matches the result into inline
  constructors for the via injection (the `add`-over-`bins` shape).
- **error:** none. The bridge `vs_mem_elems` (= backend `bmem`) carries the
  backend's `bmem_delete : bmem y (bdel x t) ↔ (y ≠ x ∧ bmem y t)` across.
  `bmem_delete` carries a `bok t` hypothesis — supplied automatically because the
  backend `set` repr is `tree{ bok _ }`, so `refine_ t0 = s` puts `bok t0` in
  scope. No re-proof about trees; symmetric to `add`.
- **workaround used:** same inline-ctor re-match as `add` (triset pattern); no
  new friction beyond the already-noted #31-refutation / C1 pattern for producers.
- **removed by:** n/a (clean; the backend's `bmem_delete`/`bok_delete` obligation
  laws did all the work).
- **severity:** none (positive result — closed set algebra add/remove/mem shipped)
