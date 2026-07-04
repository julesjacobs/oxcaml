# vox: `via` — abstraction functions in the type algebra

Design, validated in discussion 2026-07-04.  Goal: a type's logical
counterpart may be a FUNCTION of its representation's logical
counterpart — a BST ADT whose external model is a bona fide Lean
`Set Int`, not a tree.

Both goals matter equally: representation independence (swap BST for
AVL without re-verifying clients) and abstract client vocabulary
(clients prove goals with `∈`/`∪`/extensionality, never tree
recursions).

The design is three stacked features, each independently meaningful.

## 1. Ghost sorts: naming a block-defined Lean type from OCaml

Today `[@@vox.sort]` accepts exactly `int` or `bool`
(vox_verify.ml `vox_sort_of_attribute`).  Generalize: an OCaml type
may declare its sort to be any Lean type the blocks define:

```ocaml
[%%vox.lean {lean|
def ISet := Set Int
-- ∈, ∪, extensionality lemmas...
|lean}]

type iset [@@vox.sort lean "ISet"]
```

- `iset` is typically a pure phantom: no constructors, no runtime
  values.  It exists because the sort grammar is written in OCaml
  syntax — `refines (iset)` and `via (elems : iset)` name sorts by
  OCaml type paths, and vox cannot parse Lean text to discover types.
  The ghost declaration is the binding site tying an OCaml path to an
  opaque Lean name.
- Internally: one new sort constructor, `Vs_lean of string * args`,
  opaque to vox; Lean is the grammar police for every use.
- Trust class: same as today's `[@@vox.sort int]` ghosts — a declared
  interpretation (the module's axiom) unless backed by a `via`
  manifest (feature 2), which upgrades it to honest.
- Independently useful without `via`: e.g. a handle type modeled as a
  Lean `Multiset` with API specs stated directly over it.

STAGE 1 STATUS (landed): shipped MONOMORPHIC as `Vs_lean of string`
(no `args` yet — the argument sorts are added with the rest of
parameterization; the constructor comment in types.ml flags this).
Companion solver sort `S_lean of string`, rendered verbatim.
`refines (iset)` by PATH already resolves to the ghost sort through
the existing refines-kind elaboration (only the direct
`refines (lean "...")` string spelling is deferred).  The `Set Int`
model above needs Mathlib; the vendored Lean is core-only, so the
tests use an inductive-list `ISet` instead.  One surface gap noticed:
a bare lowercase name in a refinement is a variable, so 0-ary spec
constants (an empty-set literal) are not referenceable — the set API
uses only applied spec functions.  Interaction with the sealed
interfaces (`vp_sig_module`): none special — the sort travels through
the type's jkind (unchanged by the seal), and the ghost type's Lean
definition travels as an ordinary exported `.mli` block (verified
end-to-end by mechanics/lean_gset_seal.ml: a ghost sort in a sealed
`.mli`, a client proving through it across the unit boundary).

## 2. `via`: generalize Trefine, merge all the way down

Surface: `type set = tree{ bst _ } via (elems : iset)` — a type whose
denotation is `elems` applied to the skeleton's denotation.
`set{ P _ }` means morally `tree{ P (elems _) }`: predicates over the
mapped type precompose the abstraction function.

**Not a new constructor next to Trefine — a generalization of it:**

```
Trefine (skel, maps, pred)
  (* maps : (lean_fn * target_sort) list — [] is today's Trefine *)
  (* denotation = fold maps over deno(skel); pred is over deno(skel) *)
```

The key representation decision: **predicates are stored at the base
sort**.  A layer predicate `q` above `via f` cannot be pushed through
`f` semantically (f is not injective), but it pushes down trivially as
the syntactic application `q (f _)` — a base-sort predicate mentioning
the image explicitly.  Flattening is therefore total, and the existing
nested-refinement rule (typetexp.ml ~1770, `Pand` at one skeleton)
becomes the `maps = []` case of one uniform construction rule:

```
Trefine(Trefine(t, [f], p), [g], q)  ⇝  Trefine(t, [f; g], p ∧ q∘f)
```

Consequences:

- **Single-node readers survive.**  `binder_facts` still substitutes
  the binder for `_`: the runtime value IS the base value, so a binder
  `x : set{P}` contributes `bst x ∧ P (elems x)` with no layer
  recursion.  Subsumption VCs likewise.
- **Rigid unification compares normal forms** (skeleton unifies, maps
  path-equal, predicates equal) — less brittle than comparing spine
  shapes; semantically equal nestings unify.  Coercions happen only at
  the vox channels (binder intro, contracts, subsumption/refine_),
  never inside unify — exactly Trefine's discipline.
- **Two localized changes:** (a) `_`-elaboration in typetexp becomes
  layer-aware — a predicate written over a via type elaborates `_` as
  `maps(x)` at the target sort before storage; (b) `dsort_of_type`
  returns the last map's target instead of deferring to the skeleton
  (which is why maps carry target sorts — vox cannot infer `elems`'s
  Lean type).
- **Coercion rules** at the channels: extra refinement in the expected
  type at layer k → VC of that predicate at the composite image
  (today's rule, per layer).  Extra via layer with nothing above it →
  free, no VC (denotation changes vocabulary; nothing to prove).
  Missing layer (projection `t via f → t`) → NEVER implicit; the
  only projection is the explicit `refine_` unpack, which strips
  maps and predicate together (see the sharp-cases section).
- Kind-level mirror: `Vs_map` next to `Vs_fact`, so a modeling in a
  kind may itself be mapped.

## 3. Module abstraction: the payoff

```ocaml
(* bst.ml *)
type t = tree{ bst _ } via (elems : iset)
(* elems + bridging lemmas (mem x t ↔ x ∈ elems t) in the .ml block *)

(* bst.mli *)
type t : value refines (iset)
val member : (x : int) -> (t : t) -> bool{ _ = (x ∈ t) }
```

- **Inclusion / honesty:** one-line extension of the v2 structural
  rule — a manifest satisfies `refines (S)` iff its denotation sort
  (last map target) equals `S`.  The via manifest is honest;
  no trusted assertion.
- **`.cmi`:** the kind carries only `refines (iset)`; the `.mli` block
  exports the Set theory.  Clients bind `M.t` at sort `ISet`.  The
  tree, `bst`, and `elems` itself are invisible: since `.mli` specs
  are written at the Set sort, the abstraction function never appears
  in the contract.  Swapping BST for AVL is a new `elems` + new
  bridging proofs in the `.ml`; clients re-verify nothing.
- **Boundary coherence:** the impl checks its vals against the `.mli`
  with the spine visible, so the contract's `t` elaborates as
  `elems t_tree` under hypothesis `bst t_tree`; `member`'s VC is
  `result = (x ∈ elems t_tree)`, discharged by the bridging lemma.
  The client's fact is the same statement with the image opaquely
  named.  Substitute-α-at-the-boundary falls out of the
  representation; no separate machinery.
- **Invariants compose:** `bst` stays internal (maintained at
  production sites, as today).  An image-level invariant clients
  should get for free rides the existing `Vs_fact` slot:
  `refines (iset{ finite _ })`, honest iff the manifest's top layer
  carries the rigidly-equal predicate.

## Parameterization

Every ingredient reuses the refines v2 parameterized story
(`Vs_param`, positional binding, declare-generically /
instantiate-per-use, Subst path remapping through functors and cmi):

- `type 'a iset [@@vox.sort lean "ISet"]` with
  `def ISet (α : Type) := Set α`; `Vs_lean` carries argument sorts;
  `int iset` instantiates to `ISet Int`; opaque arguments instantiate
  at `VoxU`.
- Parameterized via: `elems` is a polymorphic Lean function
  (`Vox_tree α → ISet α`); map target sorts may mention `Vs_param`.
  Lean names are global strings — no path remapping; collisions hit
  the existing dedup-by-stable-name rule.
- **Typeclass constraints fail closed:** a model constraining its
  parameter (`LinearOrder α` for sorted lists, `DecidableEq α`) fails
  at the solver for instantiations lacking the instance — the
  invariants commit's doctrine.
- Library-level caveat (not mechanism): a parameterized BST needs an
  order; OCaml polymorphic compare has no Lean counterpart, so a real
  `'a set` takes a comparator whose model carries the order.
- Out of scope, inherited: predefined parameterized types as datatype
  sorts; GADTs.

## Sharp cases (settled 2026-07-04, after seal/oset landed)

- `refine_`/unpack across maps: unpack strips EVERYTHING — maps and
  predicate — binding the plain payload at the base type, with the
  scrutinee's full base-sort predicate transferred verbatim as facts
  (`bst x ∧ P (elems x)`); nothing is lost because predicates are
  stored at base sort.  Rationale: via matters at module boundaries;
  in code you want the representation.  This also fixes the coercion
  asymmetry principledly: INJECTION (`t → t via f`) is implicit and
  free (any refinement above the new map is a VC, discharged from
  the retained facts on re-wrap); PROJECTION is explicit-only, via
  `refine_` — dropping a predicate is weakening (fine implicitly),
  but dropping a map changes the denotation vocabulary, which
  deserves an explicit act.  Visibility falls out: clients cannot
  unpack what they cannot see (abstract types have no visible
  spine); inside the defining module, unpacking to the payload is
  the point.
- Printing: reconstruct the layered surface form (`t{p} via f{q}`)
  from the merged normal form for errors and signatures; fix the
  existing refines-kind printing gap (kinds display without their
  refines component) in the same change.
- Escape checking (`check_binder_escape`) walks the skeleton exactly
  as for `maps = []`; map functions are Lean names, not types, and
  cannot escape.
- Mutable cells of via type: version facts speak at the BASE sort
  (versions name runtime values); image facts derive per read.
  Needs a dedicated test.
- Seal integration (CONCRETE now that sealed interfaces landed as
  cfa88fc50b): a via-bearing unit with a specced .mli compiles its
  interface block to a VoxSig module.  Ghost-sort declarations must
  travel in the sig artifact; the abstraction function and bridging
  lemmas (mem x t ↔ x ∈ elems t) stay in the .ml's own solver input;
  the seal's re-elaboration pass is where the boundary substitution
  (interface t ↦ elems t_tree, under bst t_tree) is enforced for
  sealed units.  Stage 3 specs against this post-seal export path.
- Precedent note: `[@@vox.sort opaque]` (landed with oset) makes the
  attribute family int | bool | opaque; `lean "Name"` is its fourth
  member, and oset's same-solver-name registration is the pattern
  Vs_lean's name binding follows.  Relation of the features: oset is
  the algebraic-spec route (laws about an opaque type); via is the
  model-based route (a bona fide Lean carrier); via subsumes the
  oset pattern when a real model exists.

## Suggested build order

1. Ghost sorts (`Vs_lean`), monomorphic — independently testable
   (trusted handle types with Lean-typed models).
2. `Trefine` maps generalization + typetexp/dsort changes +
   coercion-channel rules, all within one module (no abstraction yet).
3. Inclusion rule + `refines` interop → the BST/Set end-to-end test.
4. Parameterized versions of all three.
