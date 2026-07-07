# Vlist — LANGUAGE_NEEDS notes

One block per pain-site (blueprint §5). Sites that "just worked with the
documented workaround" still get a note — that is the evidence the workaround
is load-bearing.

### Vlist · recursive via-returning append loses its map at the recursion
- **site:** vox_stdlib/Vlist.ml:79 (`append`)
- **milestone/gap:** #31
- **what I tried:** the natural shape — recurse over the via type itself,
  `let rec go : (u : t) -> t{ _ = ll_app u b }`, and in `Cons (x, r)` re-inject
  the recursive result: `let r' = (Cons (x, r) : t) in let rest = go r' in
  (Cons (x, rest) : t{ _ = ll_app u b })`.
- **error:** `NOT PROVED ... Goal: 0 = 0 && ll_repr b = ll_app u b` with
  hypotheses `u0 = Nil`, `ll_repr u0 = u`. The via value `b` is simultaneously
  treated as its own model image (`ll_repr b`) and as a model element in
  `ll_app u b`; the abstraction map and the via image are conflated, so grind
  cannot close even the `Nil` base case.
- **workaround used:** thread the SKELETON, inject once (design §7.2). The
  recursive helper stays entirely at the concrete `tree` level with an explicit
  image spec — `let rec go : (u : tree) -> tree{ ll_repr _ = ll_app (ll_repr u)
  (ll_repr tb) }` — and the single via injection `(res : t{ _ = ll_app a b })`
  happens once, at the end, through a variable `res`.
- **removed by:** the gap-#31 fix (transparent-via `let` binders binding at the
  skeleton with the full base predicate; see the `bind-skel` work). With it, the
  natural recursion-over-`t` would carry `ll_repr`'s map across the `let` and
  the skeleton indirection would be unnecessary.
- **severity:** MAJOR-ERGONOMIC

### Vlist · call/coercion result cannot be passed into a dependent parameter
- **site:** vox_stdlib/clients/smoke_Vlist.ml:11,16,23,29 (nested op calls); also
  the natural-`append` attempt `go (r : t)`
- **milestone/gap:** C1
- **what I tried:** pass an op-call result (or a coercion) straight into a
  dependent parameter, e.g. `Vlist.length (Vlist.cons x l)` and `go (r : t)`.
- **error:** `vox: the argument for a dependent parameter must be a variable or
  a pure expression the logic can name (let-bind it first)`.
- **workaround used:** let-bind every nested call result / coercion first
  (`let l' = Vlist.cons x l in Vlist.length l'`), then pass the variable.
- **removed by:** naming call results automatically (ANF the argument, or admit
  a call whose result type the logic can already name).
- **severity:** MINOR (mechanical, but pervasive — hits every nested call).

### Vlist · statement typed twice (obligation pattern M1)
- **site:** vox_stdlib/Vlist.mli:36-58 (5 `public axiom`s) vs vox_stdlib/Vlist.ml:38-63
  (5 same-named `theorem`s)
- **milestone/gap:** M1
- **what I tried:** ship the algebra as obligations (the mandated default).
- **error:** none — works as designed; the cost is the duplication.
- **workaround used:** the axiom/theorem statements are verbatim-duplicated
  across the two blocks (5× here); the seal matches them by name+type.
- **removed by:** an obligation form that states the law once (e.g. a `.mli`
  `obligation`/`goal` keyword the `.ml` discharges without re-typing the
  statement).
- **severity:** MINOR (accepted hygiene tax; first-class evidence item).

### Vlist · model defs authored in both blocks (model-dup)
- **site:** vox_stdlib/Vlist.mli:16-34 vs vox_stdlib/Vlist.ml:15-33
- **milestone/gap:** model-dup
- **what I tried:** define the LList model + `ll_cons/ll_isnil/ll_len/ll_mem/
  ll_app` once and reference from both interface and implementation blocks.
- **error:** none — but the `.ml` block must RESTATE every model def (without
  `public`) because the abstraction fn `ll_repr` and the discharging theorems
  reference them; the interface's `public def`s are not in scope for the `.ml`
  block's own elaboration.
- **workaround used:** the 5 model defs are copied into the `.ml` block sans
  `public` (5 defs duplicated).
- **removed by:** letting the `.ml` block import/reuse the interface block's
  model defs instead of re-declaring them.
- **severity:** MINOR

### Vlist · opaque VC placeholder names (*unknownN*)
- **site:** observed while stress-testing (dropping `ll_len_app`): goal printed
  as `*unknown7* = ll_len a + ll_len b`.
- **milestone/gap:** N1
- **what I tried:** read a failing VC to see which sub-term was unproved.
- **error:** the result value is shown as `*unknown7*` rather than a
  source-derived name, so the goal is harder to map back to the op.
- **workaround used:** none available — cross-reference by the hypotheses
  (`*unknown7* = ll_len ab`, `ab = ll_app a b`).
- **removed by:** readable, source-derived names for VC placeholders.
- **severity:** COSMETIC

### Vlist · exposing a non-recursive model def silently kills its laws
- **site:** vox_stdlib/Vlist.mli:21 (`ll_cons`)
- **milestone/gap:** new (dead-law lint — cf. backlog "lint for silently-dead
  block theorems")
- **what I tried:** ship `ll_cons` as `@[grind, expose] public def` alongside
  the other model defs, with laws `ll_len_cons` / `ll_mem_cons`.
- **error:** no error — and that is the problem.  `ll_cons` is non-recursive
  (`:= .LCons x l`), so with `expose` a client's grind unfolds `ll_cons x l`
  to `.LCons x l` and discharges `ll_len (ll_cons x l)` / `ll_mem x (ll_cons y
  l)` by one-step reduction, WITHOUT ever firing the shipped law.  Both laws
  were dead: deleting them left the smoke client green (Phase-C probe
  confirmed).  Contrast the recursive defs (`ll_len`/`ll_mem`/`ll_app`): even
  exposed, unfolding them one step cannot close their inductive laws, so those
  stay live.
- **workaround used:** drop `expose` from `ll_cons` (keep `@[grind] public
  def`).  Opaque to client unfolding, the two cons laws become the only route
  to `ll_len`/`ll_mem` of a cons and are now LIVE (probe: dropping either now
  breaks the smoke client).  Module + smoke still seal green.
- **removed by:** a solver-side lint that flags a shipped `public axiom` whose
  LHS grind_pattern is discharged by definitional unfolding alone (i.e. the
  law is redundant given the exposed defs) — the author would be told the law
  is dead instead of shipping a vacuous algebra.
- **severity:** MAJOR-SOUNDNESS (ships an interface that advertises laws a
  client can neither observe nor rely on being load-bearing).

### Vlist · ll_isnil de-exposure needs a structural empty spec to stay live
- **site:** vox_stdlib/Vlist.mli (`ll_isnil`, `ll_nil`, `empty`)
- **milestone/gap:** new (same dead-law family as the ll_cons finding above)
- **what I tried:** de-expose `ll_isnil` (`@[grind] public def`, per the
  sharpened Amendment A: non-recursive defs must be opaque) and ship its two
  characterization laws `ll_isnil_nil` / `ll_not_isnil_cons`.
- **error:** no compiler error, but a latent DEAD law: with `empty`'s original
  spec `t{ ll_isnil _ }`, a client already gets `ll_isnil (empty ())` from the
  postcondition, so `is_empty (empty ()) = true` proves WITHOUT `ll_isnil_nil`
  — the law is dead (probe: dropping it left that goal green).
- **workaround used:** add an opaque empty-model def `ll_nil : LList := .LNil`
  and strengthen `empty` to the structural spec `t{ _ = ll_nil }`.  Now
  `is_empty (empty ())` reduces to `ll_isnil ll_nil` with both defs opaque, so
  `ll_isnil_nil` is the only route and is LIVE (probe: dropping it now fails the
  goal `*unknownN* = true`; dropping `ll_not_isnil_cons` fails `... = false`).
- **removed by:** the same dead-axiom lint proposed above would have flagged
  `ll_isnil_nil` as redundant under the weaker `empty` spec, prompting the
  structural strengthening.
- **severity:** MINOR (caught and fixed here; the general form is the
  MAJOR-SOUNDNESS lint item above).

### Vlist · exposed ADT with a via-typed field can't build its solver model
- **site:** vox_stdlib/Vlist.mli (attempted `type vlist_view = VNil | VCons of int * t`)
- **milestone/gap:** new (blocks eliminator Mech B — view/pop-style destructors)
- **what I tried:** the addendum's `uncons` view — an exposed ADT one of whose
  constructor fields is the via-abstract type `t` (sort `LList`):
  `type vlist_view = VNil | VCons of int * t`, with a `ll_view_ok` model def
  matching on it.
- **error:** at the `.mli` seal —
  `VoxSig_Vlist.lean:3:81: error: Constructor field \`LList\` of
  \`Vox_Vlist_vlist_view.VCons\` contains universe level metavariables ...
  Sort ?u.7`. vox auto-derives `Vox_Vlist_vlist_view` from the OCaml ADT and
  emits the `t` field (a block-defined inductive sort) with an unresolved
  universe; the block then `sorry`s and `.VNil` dotted access fails.
  Reproduced two ways: (1) letting vox derive the model; (2) writing
  `public inductive Vox_Vlist_vlist_view ... | VCons : Int -> LList -> ...`
  manually in the block — same universe error.
- **workaround used:** ship the destructor as guarded `head`/`tail` +
  `ll_cons_head_tail` reconstruction instead of a view ADT (same first-order
  traversal capability, no via-typed ADT field). The view shape is unshippable
  until the compiler pins the field sort's universe.
- **removed by:** fixing the derived-inductive elaboration to specialize a
  via/custom-sort field's universe (so an exposed ADT may carry a via-typed
  field). Also unblocks Vset/Vmap `pop`-style views.
- **severity:** BLOCKING (for view-ADT eliminators over via types).

### Vlist · refine_ rejected on a refined via type
- **site:** vox_stdlib/Vlist.ml (`head`, `tail`; arg type `t{ not (ll_isnil _) }`)
- **milestone/gap:** #31 family (via-binder sorting)
- **what I tried:** `let refine_ t0 = l in match t0 with ...` where `l` has the
  refined via type `t{ not (ll_isnil _) }`.
- **error:** `vox: a refine_ pattern requires the scrutinee to have a refined
  type (a plain let binds at the skeleton and carries the fact already)`. But a
  plain `let t0 = l` then loses the `ll_repr t0 = l` map — the vacuous Nil
  branch can't be discharged (goal `0 = ll_head l` under `t0=Nil, t0=l,
  ll_iscons/¬ll_isnil l`, ending in a Lean type mismatch).
- **workaround used:** alias to the unrefined type first, then `refine_`:
  `let lu = (l : t) in let refine_ t0 = lu in ...`. Seals green.
- **removed by:** letting `refine_` peel a *refined* via type directly (bind at
  the skeleton while keeping the `ll_repr` image map), so no unrefined alias is
  needed.
- **severity:** MINOR (mechanical alias; recurs at every via op with a refined
  argument).

### Vlist · head/tail eliminator (Mech B realization)
- **site:** vox_stdlib/Vlist.mli / .ml (`head`, `tail`, `ll_head`, `ll_tail`,
  `ll_head_cons`, `ll_tail_cons`, `ll_cons_head_tail`)
- **milestone/gap:** none (works today) — recorded as the shipped form of the
  blocked view-ADT above.
- **what I tried:** first-order structural destructor for clients.
- **error:** n/a.
- **workaround used:** guarded `head`/`tail` under the `not (ll_isnil _)`
  precondition + reconstruction `ll_cons (ll_head l) (ll_tail l) = l`; the
  reconstruction law's grind_pattern fires only when the client materializes
  `cons (head l) (tail l)` (so a length/mem recursion over the tail must
  rebuild, or use the bonus `ll_head_cons`/`ll_tail_cons` reductions).
- **removed by:** the view-ADT universe fix would let this be the more
  ergonomic `uncons : t -> (VNil | VCons of int * t)` single-match form.
- **severity:** MINOR-ERGONOMIC (reconstruction needs an explicit rebuild to
  trigger; head/tail themselves are clean).

### Vlist · opaque empty needs an explicit non-membership law for eliminators
- **site:** vox_stdlib/Vlist.mli (`ll_nil_not_mem`)
- **milestone/gap:** new (Mech-A eliminator base case; consequence of the
  ll_nil de-exposure above)
- **what I tried:** a downstream Mech-A eliminator (Vmap.keys / Vset.elements)
  builds a Vlist by recursion; at the base case it needs
  `ll_mem x (Vlist.empty ()) = False`.
- **error:** with `empty : t{ _ = ll_nil }` the goal reduces to
  `ll_mem x ll_nil = False`, but `ll_nil` is opaque (de-exposed) so grind
  cannot reduce `ll_mem x ll_nil`, and no empty→non-membership law existed —
  the base case was underivable in the client.
- **workaround used:** ship `ll_nil_not_mem (x) : ¬ ll_mem x ll_nil` (obligation
  form, grind_pattern `ll_mem x ll_nil`). Base case now discharges by this law;
  cons step by the existing `ll_mem_cons`. Live (smoke: `mem x (empty ()) =
  false`; dropping it fails that goal while the module still seals).
- **removed by:** if `ll_nil` were exposed the reduction would be automatic —
  but that reopens the dead-law hazard, so the explicit law is the right call.
  A general "reduction laws for every opaque non-recursive def" convention (or
  the dead-axiom lint that permits them) subsumes this.
- **severity:** MINOR (honest 2-line list law; the cost is authoring it by hand
  rather than getting it from an exposed def).
