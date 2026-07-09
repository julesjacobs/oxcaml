# vox stdlib HOF kit + exact-law recipe (WP-0, 2026-07-08)

*WP-0 foundation deliverable for the stdlib polish campaign. This is the
mechanical recipe WP-1..WP-4 apply to add higher-order (map/filter/fold/…) and
predicate (for_all/exists/find) operations to any container, with relational
specs and — where the container is exposed — exact-output laws. It generalizes
the `Vrel` mechanism (relation-as-dependent-parameter + pass-whole lifting
defs + lambda reflection) from Vrel's private `ilist` to a via-abstracted
container, using `Vlist` as the worked reference.*

*Status: shipped + verified on the vox tip (920bb5e74) with PLAIN-ARROW
relation/predicate params. `[@vox.total]` had not landed at authoring time; when
it does, spec-position relation/predicate params gain the total annotation
(§7) — the recipe body is otherwise unchanged.*

---

## 0. What the kit is (and why it is a recipe, not a shared module)

The HOF machinery splits into two layers:

- **Container-independent substrate** (≈6 lines): the relation/predicate
  abbrevs `IntRel`/`IntPred`/`IntRel3`, the pass-whole wrappers
  `rHolds`/`pHolds`/`r3Holds`, and (for fuel-style folds) the relation algebra
  `rcomp`/`rand`/`ror`/`rconverse` + `relIter`/`relIterN`. Identical in every
  module.
- **Per-container relational lifts**: `listRel`/`allP`/`exP`/`relFold` defined
  by recursion over *that container's model inductive* (LList, the option model,
  the set/map model, …). These are IRREDUCIBLY per-container — a recursion over
  `LList` cannot be reused for the option model.

Because the reusable part is 6 trivial lines and its cross-unit import is a
documented trap (the `IntRel`-must-be-`abbrev` finding, notes/vrel.md — an
opaque imported `def` fails to unify against a bare `Int -> Int -> Prop`
binder), a **shared imported kit module is a bad trade**: it buys ~6 lines of
reuse at the cost of a build-order dependency on every stdlib module plus the
abbrev-unification trap. The per-container lifts, which are the bulk of the
work, cannot be shared anyway.

**Therefore the kit is a copy-in snippet + this recipe.** `Vrel` is the
reference for the *exposed-container* case; `Vlist` (this WP) is the reference
for the *via-abstracted* case. Copy §1, fill in §2 per container, apply §3–§6.

---

## 1. Substrate: `open Vhof` (do NOT declare the atoms)

The container-independent atoms live ONCE in the shared leaf module `Vhof`
(vox_stdlib/Vhof.{mli,ml}): the `IntRel`/`IntPred`/`IntRel3` abbrevs and the
`@[grind, expose]` `rHolds`/`pHolds`/`r3Holds` wrappers. Your module gets them by
importing Vhof — it must NOT redeclare them (the atoms are `public`, so a second
declaration collides in the shared Lean namespace: `IntRel has already been
declared`).

```ocaml
open Vhof            (* both the .mli and the .ml, before your `type`/block *)
```

Then reference `IntRel`/`rHolds`/etc. directly in your lift signatures and
callback contracts — they resolve against Vhof's imported VoxSig. Add `Vhof` to
your module's dependency list in MODULES.manifest (and note it is staged
transitively by anything that imports your module).

- Vhof's `IntRel`/... are `abbrev`s (reducible), so the S_arrow binder
  `(r : (int -> int -> bool))` unifies against them across the import boundary
  (the original Vrel finding; verified end-to-end through the Vhof import).
- Fuel-style folds (a binary step whose length drives the recursion, cf.
  `Vrel.fold`) additionally need `relIter`/`relIterN` + the `toNat` bridges,
  which live in Vrel (not Vhof — they are Vrel-specific). **Prefer the ternary
  structural `relFold` (§2) for `fold_left`** — element-aware, no fuel/`toNat`,
  and yields exact sum/count laws.

## 2. Per-container lifts (fill in over YOUR model inductive `M` with ctors `Nil`/`Cons`)

```lean
-- map: b is pointwise r-related to a (same length)
@[grind, expose] public def m_listRel (r : IntRel) : M -> M -> Prop
  | .Nil, .Nil => True
  | .Cons a s, .Cons b t => r a b /\ m_listRel r s t
  | _, _ => False
-- filter / for_all / exists
@[grind, expose] public def m_allP (p : IntPred) : M -> Prop
  | .Nil => True
  | .Cons x t => pHolds p x /\ m_allP p t
@[grind, expose] public def m_exP (p : IntPred) : M -> Prop
  | .Nil => False
  | .Cons x t => pHolds p x \/ m_exP p t
-- fold_left: TERNARY element-aware step (acc, elem, acc')
@[grind, expose] public def m_relFold (r : IntRel3) : M -> Int -> Int -> Prop
  | .Nil, init, final => init = final
  | .Cons x t, init, final => exists acc, r init x acc /\ m_relFold r t acc final
-- accessor for the exact sum-law (element sum)
@[grind, expose] public def m_sum : M -> Int
  | .Nil => 0
  | .Cons x t => x + m_sum t
```

Note `m_listRel`/`m_relFold` use the relation **bare** (`r a b`), while
`m_allP`/`m_exP` use `pHolds` — this matches Vrel and is what the reflected
call-site lambda and the callback contract (§4) beta-reduce against.

## 3. Laws — two kinds

**Obligations** (state as `public axiom` + `grind_pattern` in the `.mli`;
discharge with a same-named `theorem` + identical `grind_pattern` in the `.ml`):

```lean
public axiom m_listRel_len (r : IntRel) (a b : M) :
    m_listRel r a b -> m_len a = m_len b
grind_pattern m_listRel_len => m_listRel r a b
```
`.ml` proof: `induction a generalizing b <;> cases b <;> grind`.

**Exact-output laws** (`.mli`-ONLY `public theorem`, proved in-block; they ride
the VoxSig olean to clients and are NOT restated in the `.ml` because no `.ml`
proof uses them). Stated over an ABSTRACT relation with the callback's graph as
a PREMISE — never a lambda in the trigger (grind arithmetic-normalizes lambda
bodies at indexing, so a lambda-containing `grind_pattern` never fires):

```lean
public theorem m_relFold_sum_exact (r : IntRel3) (hr : forall a x c, r a x c -> c = a + x) :
    forall (xs : M) (init final : Int),
      m_relFold r xs init final -> final = init + m_sum xs := by
  intro xs
  induction xs with
  | Nil => intro init final h; simp only [m_relFold, m_sum] at *; omega
  | Cons x t ih =>
      intro init final h
      simp only [m_relFold] at h
      obtain ⟨acc, hacc, hrest⟩ := h
      have h1 := hr init x acc hacc
      have h2 := ih acc final hrest
      simp only [m_sum]; omega
grind_pattern m_relFold_sum_exact => m_relFold r xs init final
```
(`_count_exact` is identical with `c = a + 1` and `m_len` in place of `m_sum`.)

## 4. OCaml surface (val + let)

Relation/predicate params are **parenthesised** dependent binders (the
dependent-binder grammar takes only an atomic inner type); the callback carries
its per-element contract via the wrapper:

```
val map :
  (r : (int -> int -> bool)) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (l : t) -> t{ m_listRel r l _ }
val fold_left :
  (r : (int -> int -> int -> bool)) ->
  (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
  (init : int) -> (l : t) -> int{ m_relFold r l init _ }
val for_all :
  (p : (int -> bool)) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (l : t) -> bool{ _ = m_allP p l }
```

For a **via-abstracted** container (Vlist), the `.ml` uses the append/length
skeleton-threading pattern: `let refine_ t0 = l in`, an inner `go` over the
CONCRETE repr with an image spec (`tree{ m_listRel r (m_repr u) (m_repr _) }`
for a via-returning op, or `int{ m_relFold r (m_repr u) a _ }` for a scalar
result), then a SINGLE via injection at the end through a variable. See
`Vlist.map`/`filter`/`fold_left`/`for_all`/`exists`. For an EXPOSED container
(Vrel-style) the `go` runs directly over the model constructors.

## 5. Client consumption (the demo/smoke goals)

- Relations/predicates are supplied at the CALL SITE as OCaml lambdas
  (`Vlist.filter (fun x -> x > 0) (fun x -> x > 0) l`) — lambdas parse only in
  ARGUMENT position.
- **A lambda may NOT appear in a refinement `{...}`** (L13). To state a goal
  about `m_allP`/`m_exP`/`m_find_result`, the client declares a reducible
  `@[grind, expose] abbrev pPos : Int -> Prop := fun x => x > 0` in its OWN
  block and writes `m_allP pPos _`; grind normalizes the abbrev to its body so
  it and the reflected call-site lambda become the same node.
- **Name every lambda binder** — a wildcard `_` in a reflected relation lambda
  (`fun acc _ acc' -> …`) fails reflection ("cannot be named in the logic").
  Write `fun acc x acc' -> …` even when `x` is unused.

## 6. Acceptance gates (run per op)

1. `.mli` then `.ml` seal green (BUILD.md recipe, private temp dir).
2. Smoke client green (one goal per shipped law).
3. Negative control per op fails closed (wrong constant / wrong predicate).
4. **Per-law deletion-liveness sweep**: strip each obligation (axiom+theorem)
   / exact law and confirm the module still SEALS but the smoke goal turns NOT
   PROVED. A law whose removal leaves the smoke green is DEAD (Amendment A).

## 7. `[@vox.total]` convention (apply once landed)

A relation/predicate param that appears as a **spec symbol** in a refinement
gets the total annotation; a param whose callback is only APPLIED under its own
contract stays plain. Concretely, the `(r : (int -> int -> bool))` /
`(p : (int -> bool))` binders whose lifted form (`m_listRel r`, `m_allP p`,
`m_relFold r`) is named in a postcondition are spec-position ⇒ total-annotate
them (`(r : ((int -> int -> bool) [@vox.total]))` or the `vox_total` former
spelling that actually lands — match the landing). The callback `f`/`test`
binders are applied under their contract, not named as spec symbols, so they
stay plain. WP-0 shipped plain-arrow throughout; annotate at rebase.

## 8. Boundaries hit (route around; don't block)

- **Exact ELEMENT output does not survive a via face.** Through Vlist's
  abstraction the model constructors (`ll_cons`) are OPAQUE, so `m_listRel`
  cannot reduce on a client-built list and per-element exact goals
  (`head (map …) = head l + 1`) do NOT close. What survives the via face is the
  LENGTH law (`m_listRel_len`) and the fold exact laws (stated over the abstract
  accessors `m_sum`/`m_len l`, no constructor reduction needed). Exact element
  output is an EXPOSED-container capability (Vrel/ilist). Ship map RELATIONAL +
  length; ship fold RELATIONAL + exact sum/count.
- **C1 (L8):** a RELATIONAL (∀) call result still needs a `let` before flowing
  into a dependent param; equational results inline post-#53. The via `.ml`
  skeleton-threading already let-binds, so this is transparent there.
- **An op returning another module's type pulls that module into your VoxSig
  transitively.** `Vlist.find_opt : … -> Voption.t{ … }` makes VoxSig_Vlist
  import VoxSig_Voption, so EVERY Vlist consumer (Vmap, Vset) must also stage
  Voption. Keep option/result→list conversions (`to_list`) OUT of the lower
  module to avoid a cycle: with Voption below Vlist, `to_list` belongs in Vlist
  (`of_option`), not Voption. WP-1 must respect this layering.
