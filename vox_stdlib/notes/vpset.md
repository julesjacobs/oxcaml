# Vpset — language/compiler needs (build notes)

Module: the ORDER-FREE, element-POLYMORPHIC finite set (poly study sub-problem
C-1, the "pset shape"). `'a t : value refines ('a pset)`; model `PSet a`
(parameterized cons-list inductive); structure-preserving abstraction fn
`ps_elems : Vox_Vpset_cell a -> PSet a`. LEAF module (no stdlib deps). Zero trust
(no `[@@vox.reflect]`, no assumed axioms). Compiler: `_install/bin/ocamlc.opt`,
pinned Lean 4.31.0; verified in private temp dirs.

Ops as shipped (every one is element-EQUALITY-FREE): `empty` (unspecced —
F-B2), `singleton`, `is_empty` (structural), `add` (cons, the closed producer
algebra), `union` (append; membership is the OR). Specs are the honest
EXTENSIONAL (pointwise-membership) Vset shape, never structural `{ _ = ps_ins x
s }`. Relational F-3 vocabulary `ps_subset`/`ps_equal` shipped as consumable
client goals; one proved bridge law `ps_isnil_isempty`.

Verified at BOTH `int Vpset.t` and `string Vpset.t` (smoke_vpset.ml). Each op
spec proven load-bearing by deletion (drop `add`'s spec ⇒ `add_superset` fails,
no hypotheses; NC1 `is_empty (singleton x) = true` rejected; NC3 wrong-direction
`ps_subset` rejected; hypothesis-free `ps_subset` rejected when a live VC forces
the block to be checked). No ambient inconsistency (`(1:Int)=2` rejected once a
live VC is present).

## HEADLINE — decidable equality at the opaque element (`S_param`/VoxU) is the wall

This is the module's central probe (poly study F-X1 / C-tier), CONFIRMED by
build. It gates the two ops a set "should" have, at **two different layers**:

### Vpset · the bool `mem` QUERY is blocked at the PROOF layer (headline)
- **site:** attempted `mem : (x:'a) -> (c:'a cell) -> bool{ _ = ps_mem x (ps_elems c) }`
  by the natural recursion `if x = y then true else member x r`.
- **milestone/gap:** decidable-eq-at-`S_param` (NEW language need; the study's
  F-X1 Prop-vs-Bool-membership line).
- **what I tried:** the runtime membership search comparing elements with OCaml
  polymorphic `=`, refined to the Prop model `ps_mem`.
- **error:** `NOT PROVED — Goal: true = ps_mem x (ps_elems c)`, hypotheses only
  `c = PCons (y, r)` — **no `x = y` hypothesis**. The `then`-branch is entered
  because the runtime `x = y` returned true, but at an opaque `'a` the OCaml `=`
  carries NO model fact (there is no `DecidableEq a` / `BEq a` at VoxU), so the
  guard is an unconstrained bool and its truth does not thread `x = y` into the
  goal.
- **DECISIVE control:** the *identical* recursion at concrete `int` (a monomorphic
  `ipset` probe) **PASSES** — at `Int`, OCaml `=` models to a decidable Lean `=`,
  the guard threads `x = y`, and `true = (x = y ∨ …)` closes. So this is NOT the
  #32 branch-threading gap (that machinery works here); it is squarely the
  missing decidable equality at the element sort.
- **workaround used:** none — ship RELATIONAL-only. Membership is the Prop `ps_mem`
  a client REASONS with (in `ps_addspec`/`ps_subset`/etc.), never a bool it RUNS.
- **removed by:** a Lean `DecidableEq`/`BEq` instance at `S_param` — either a
  reflected comparator/equality companion module (study C-tier-2, at a named TCB)
  or a true `Make(EQ)`/`Make(Ord)` functor whose argument supplies the proved
  instance (study C-tier-3, v2). Both are out of reach on today's compiler.
- **severity:** MAJOR (defines the C-1 order-free ceiling — the honest module has
  membership REASONING but no membership QUERY).

### Vpset · `remove` is blocked EARLIER, at the ELABORATION layer
- **site:** attempted model deletion fn `ps_del (x) : PSet a -> PSet a` with
  `.pcons y s => if x = y then ps_del x s else …`.
- **milestone/gap:** decidable-eq-at-`S_param` (same wall, earlier failure).
- **what I tried:** the honest dual of `add` — a model deletion fn `ps_del` that
  drops elements equal to `x` (`if x = y then …`), a runtime `remove` filtering
  the repr list, refined to the relational `ps_removespec r x s := ∀ y, ps_mem y
  r = (y ≠ x ∧ ps_mem y s)`.
- **error:** the `[%%vox.lean]` block itself fails to ELABORATE:
  `error(lean.synthInstanceFailed): failed to synthesize instance` — `if x = y`
  is a Bool DECISION needing `DecidableEq a`, which does not exist at the abstract
  element, so the model deletion function cannot even be DEFINED. (Contrast the
  `mem` query, whose Prop model `ps_mem` elaborates fine — propositional `x = y`
  needs no instance — and only the OP's proof fails.) The relational
  `ps_removespec` Prop is expressible; nothing can compute the deletion.
- **workaround used:** none — `remove` is not shipped. Documented as the dual of
  the `mem` verdict.
- **removed by:** same as the `mem` query (DecidableEq at `S_param`).
- **severity:** MAJOR (same ceiling; recorded separately because it fails at a
  distinct, earlier layer than the query).

## Other findings

### Vpset · spec'd nullary via producer `empty` blocked → ships unspecced (F-B2 confirmed)
- **site:** vox_stdlib/Vpset.ml `empty`.
- **milestone/gap:** F-B2 (nullary-via-constructor Lean type param unsolved).
- **what I tried:** `empty : (u:unit) -> 'a t{ ps_isempty _ }` (and the structural
  `{ ps_isnil _ }` variant) — the natural spelling that would make emptiness a
  readable postcondition.
- **error:** the via injection of the parameterless `PNil` leaves the Lean
  datatype's type parameter an unsolved metavariable (the study's F-B2, which also
  fails at concrete `int t`).
- **workaround used:** ship `empty : unit -> 'a t` UNSPECCED; emptiness is not a
  fact on `empty ()`'s type. Note the downstream cost: `is_empty (empty ())`
  yields NOTHING (no image fact to feed `is_empty`'s spec), so emptiness is only
  observable off a producer that pins the element param (e.g. `is_empty
  (singleton x) = false`, which is what the smoke uses).
- **removed by:** study ask-#2 (pin a nullary via-constructor's Lean type param
  from the expected result type). Small, localized; removes the one wart across
  every parameterized container.
- **severity:** MINOR (ergonomic; isolated).

### Vpset · order-free ops prove GENERICALLY with no DecidableEq (positive — F-C4)
- **site:** `add` (`ps_addspec`), `singleton` (`ps_singletonspec`), `union`
  (`ps_unionspec`), and the whole `ps_mem`/`ps_subset`/`ps_equal` vocabulary.
- **milestone/gap:** F-C4 / F-X1 positive side.
- **what I tried:** the extensional pointwise-membership algebra over the abstract
  element.
- **error:** none. `ps_mem y (pcons x s) = (y = x ∨ ps_mem y s)` holds
  definitionally; the `y = x` is the PROPOSITIONAL membership disjunct (classical,
  needs no instance), so `add`/`singleton` discharge generically. `union` needs
  one membership-over-append lemma `ps_mem_app` (`induction p <;> grind`), also
  DecidableEq-free. This is the load-bearing positive: an element that is only
  STORED / CONCATENATED / shape-inspected proves once for every instantiation.
- **workaround used:** none.
- **removed by:** n/a (positive result).
- **severity:** none.

### Vpset · `is_empty` structural query + the `ps_isnil ↔ ps_isempty` bridge (grind won't refute the negated-∀ inline)
- **site:** vox_stdlib/Vpset.ml `is_empty`; vox_stdlib/Vpset.mli theorem
  `ps_isnil_isempty`.
- **milestone/gap:** grind-instantiation friction (negated universal).
- **what I tried:** ship `is_empty` directly against the membership-emptiness
  `ps_isempty s := ∀ y, ¬ ps_mem y s` (the honest Vset `vs_isempty` shape).
- **error:** the `PCons` arm's VC `false = ps_isempty s` is `NOT PROVED` — grind
  will not self-instantiate the `∀` at the head witness to refute it (even with
  the head element NAMED in the pattern, `PCons (h, _)`). The `empty`/`nil`
  direction (`true = ∀y ¬mem y pnil`) is the EASY, witness-free one that Vset's
  `empty` relies on; a bool `is_empty` needs the HARD refutation direction.
- **workaround used:** answer a STRUCTURAL `ps_isnil` (`pnil => True | pcons =>
  False`, no `∀`) with `is_empty`, and ship a SEPARATE proved bridge
  `ps_isnil_isempty : ps_isnil s ↔ ps_isempty s`. Its backward arm is written
  explicitly (`intro h; exact (h y) (Or.inl rfl)` on the `pcons y s'` case) — the
  head-witness grind won't supply. A client turns `is_empty s = true` into
  `∀ y, ¬ ps_mem y s` via this lemma. In the smoke, forcing the op the other way
  (`is_empty (singleton x) = false`) needed a client helper
  `ps_singleton_not_isnil` supplying the same explicit witness.
  HYGIENE (Amendment A / §6.7): `ps_isnil` is exposed and non-recursive, which the
  acceptance heuristic flags as a dead-law risk — but it is a FALSE POSITIVE here.
  Its only law is the bridge, stated on a SYMBOLIC `s` (`ps_isnil s ↔ ps_isempty
  s`), and grind cannot discharge that by unfolding (no constructor to reduce on a
  variable). Exposure only kills a law stated on CONCRETE constructors (the case
  that bit Vplist's `pl_isnil`, whose laws were `ps_isnil_nil`-style); a symbolic
  bridge sidesteps the trap. Confirmed LIVE by the removal test: deleting the
  bridge leaves the module sealing but makes `sing_not_empty` (via
  `ps_singleton_not_isnil`) fail. So `ps_isnil` correctly ships `[grind, expose]`.
- **removed by:** stronger grind instantiation of negated universals at an
  available constructor witness (would let `is_empty` ship directly against
  `ps_isempty` and drop the bridge + client helper).
- **severity:** MINOR (one explicit-witness lemma; recorded because the negated-∀
  is a recurring grind boundary).

### Vpset · `union` recursion — thread a RAW refined cell, not an in-unit transparent via (#31 avoided)
- **site:** vox_stdlib/Vpset.ml `un` / `union`.
- **milestone/gap:** #31 (avoided by construction).
- **what I tried:** first spelled `un` to recurse RETURNING the via `'a t` and
  rebuild `PCons (x, r0)` after `let refine_ r0 = r`.
- **error:** `a refine_ pattern requires the scrutinee to have a refined type` —
  you cannot `refine_`-unpack an in-unit transparent via result and re-inject its
  underlying cell through the recursion; this is the #31 producing-unit
  skeleton-map territory.
- **workaround used:** recurse over RAW `'a cell` carrying a STRUCTURAL image
  refinement `'a cell{ ps_elems _ = ps_app (ps_elems p) (ps_elems q) }` (a plain
  refined skeleton, whose fact survives a `let` normally), and coerce to the
  abstract `t` only at `union`'s boundary. Clean; `ps_mem_app` closes
  `ps_unionspec`.
- **removed by:** n/a for #31 on this clone (the raw-cell form sidesteps it, as
  Vset's `elements` note also records for client via-composition). Recorded to
  sharpen the rule: an in-unit transparent-via producer should recurse at the
  skeleton with a structural image, not thread the via value.
- **severity:** none (calibration; clean pattern).

### Vpset · silently-dead block theorem defeated a negative control (methodology note)
- **site:** negative-control probing (not a shipped file).
- **milestone/gap:** #5 backlog (lint for silently-dead block theorems) —
  independently reproduced.
- **what I tried:** a stand-alone `theorem nc2 (p q) : ps_subset p q := by grind`
  (should be UNPROVABLE) in a unit whose only OCaml code was `let x = 1`.
- **error:** it COMPILED GREEN — with no OCaml VC using a spec fn, the block is
  never sent to the solver (the `want_spec_text` gate), so a FALSE theorem
  (`(1:Int)=2` too) is silently accepted. The control only becomes meaningful once
  a live op VC (`add_superset`) is present in the same unit, at which point the
  false theorem is correctly rejected — and the SHIPPED smoke's block theorems
  ARE checked (injecting `1=2` into it fails). Confirms the backlog lint is worth
  building: a block theorem in a VC-free unit is a no-op.
- **workaround used:** always co-locate negative-control block theorems with a
  live op VC.
- **removed by:** #5 (warn on block theorems in units with no triggered VC).
- **severity:** MINOR (tooling; a real footgun for authors relying on block
  theorems for coverage).

### Vpset · model theory authored in both blocks (model-dup tax)
- **site:** vox_stdlib/Vpset.mli and vox_stdlib/Vpset.ml.
- **milestone/gap:** model-dup (same as Vset/Vmap).
- **what I tried:** state the `PSet` inductive + the set vocabulary (`ps_mem`,
  `ps_isnil`, `ps_isempty`, the `*spec` defs) once and have both blocks share it.
- **error:** none — required by the pattern. The `.mli` declares `PSet` + the
  public set vocabulary a client computes with; the `.ml` restates all of it (so
  its own op VCs can name the specs) plus the private scaffolding (`ps_elems`,
  `ps_app`, `ps_mem_app`) that must not leak. 1 inductive + ~8 defs duplicated;
  the abstraction fn + append + its lemma are `.ml`-only.
- **workaround used:** dual authoring — the `.ml` re-declares every shared decl
  under the same solver names (so the seal's re-elaborated specs land on the
  concrete type), and keeps the abstraction/scaffolding private to the `.ml`.
- **removed by:** a shared model-theory include, or letting the `.ml` import the
  `.mli`'s decls.
- **severity:** MAJOR-ERGONOMIC.

## Deferred (not a language need)
- An `elements : 'a t -> 'a Vplist.t` eliminator (the R7 cross-module edge Vset
  ships into Vlist) is a natural addition but is DEFERRED: Vplist is being built
  concurrently by a sibling builder and Vpset is kept a LEAF to avoid a race on
  its artifacts. No language obstacle expected — an `'a`-list enumeration stores
  elements only, the same equality-free family as `union`.
