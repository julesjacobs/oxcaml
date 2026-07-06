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
  `maps(x)` at the target sort before storage; (b) `dsort_of_type` on a
  via type.  STAGE-2 CORRECTION (landed): within one module (spine
  visible) the binder IS the base value — the reader contributes
  `bst x && P(elems x)`, and `bst x` needs `x` at the *skeleton* sort —
  so `dsort_of_type` returns the SKELETON sort there (and registers the
  map targets so the map functions' declarations reach the solver).
  The last-map-target sort is what `dsort_of_type` returns only on the
  ABSTRACT path (a `Tconstr` whose kind carries `refines (target)`),
  where the client has no spine and the value simply IS the image —
  which is stage 3.  Maps still carry target sorts (for rigid
  unification and the abstract dsort); vox cannot infer `elems`'s Lean
  type.  IMAGE-BINDER CORRECTION (landed, stage 3, SUPERSEDES the
  stage-2 base-binder above): `dsort_of_type` on a via type returns the
  IMAGE (last map target) EVERYWHERE, unifying the within-module and
  abstract paths.  A via binder denotes the image; the representation
  (tree, `bst`) is reached ONLY through a `refine_` unpack, which binds
  the base tree with its invariant AND the LINK `elems t0 = t`.  Writing
  `bst x` on a raw via binder is no longer valid (it was the stage-2
  deviation that broke the boundary); `bst` lives behind the unpack.
  Consequences: a via type is NOT stripped to its skeleton at a use
  (typecore: value reference, param binding, `vox_strip_param_refinement`
  strip only ordinary refinements, `maps = []`), so `refine_` can unpack
  it and direct skeleton use is a no-implicit-projection error;
  `binder_facts` reads the image (`composite _ := binder`, dropping the
  skeleton-invariant conjuncts); the `refine_` subject-alias `x = s` is
  suppressed for a via unpack (it would be ill-sorted) in favour of the
  `composite x = s` link.
- **Coercion rules** at the channels: extra refinement in the expected
  type at layer k → VC of that predicate at the composite image
  (today's rule, per layer).  Extra via layer with nothing above it →
  free, no VC (denotation changes vocabulary; nothing to prove).
  Missing layer (projection `t via f → t`) → NEVER implicit; the
  only projection is the explicit `refine_` unpack, which strips
  maps and predicate together (see the sharp-cases section).
- Kind-level mirror: `Vs_map` next to `Vs_fact`, so a modeling in a
  kind may itself be mapped.

STAGE-2 STATUS (landed): `Trefine` carries `maps : vox_map list`
(`{ vm_fn; vm_target; vm_sort }` — the OCaml target type is kept for
printing `via (fn : target)` and the stage-3 inclusion rule).  Maps
default `[]` (today's refinement).  Surface: the `[@vox.via (fn :
target)]` attribute (localized in `typetexp.vox_via_attr`; a real
`via` grammar production can replace it there).  No implicit projection
of a via VALUE to its skeleton (enforced at the application-result
coercion).  Both target kinds work: a ghost-sort target (a block-defined
`inductive`, e.g. `iset`) and an OCaml DATATYPE target (`Vs_data`, e.g.
`via (to_list : ilist)`) -- the latter was blocked by a block-datatype
emission-ordering bug, now resolved by the on-sight solver-block
datatype registration that landed with the hash-table stack.  See
mechanics/lean_via.ml.

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

STAGE 3 STATUS (landed — the boundary works, and honest impl proving
across it is achieved via the IMAGE-BINDER pivot; the earlier
base-binder blocker below is RESOLVED):

Shipped and tested:
- **Boundary type coherence** (`typing/ctype.ml`, `vox_flatten_view` /
  `vox_trefine_match`, wired into `moregen`): an interface's
  `Trefine(abstract_t, [], p)` — `type t : value refines (iset)` refined
  by `t{ p }` — reconciles with the implementation's flattened via form
  `Trefine(tree, [elems], bst && push(p))`.  The normalizer expands the
  abstract skeleton to its manifest (visible in the impl's env) and
  flattens exactly as typetexp does at elaboration (pushing the bound
  value `_` through the maps).  A no-op except at an abstraction
  boundary, so every within-module and opaque-seal comparison is
  unchanged.  Net effect: the `.ml` and `.mli` write the SAME contract
  text `t{ _ = ins x s }` — the `.mli` never mentions `elems` (the
  no-leak constraint holds) — and the impl type-checks against the
  abstract interface.
- **Honesty / inclusion** (already in `includecore` from the map-record
  change): a via manifest satisfies `refines (S)` iff its last map's
  target sort equals `S`; a mismatch (`refines (ibag)` over an `iset`
  manifest) is REJECTED, the message showing the manifest's OCaml target
  (`via (elems : iset)`) alongside the sorts.  Fail-closed.
- **Client through the abstraction**: an abstract `M.t : refines (iset)`
  binder's `dsort` is the IMAGE (`S_lean "ISet"`) via the existing
  `refines`-kind path (verified, no new code), so a client binds `M.t`
  at the set sort, imports `ISet`/`mem`/`ins`/`card` from the unit's
  VoxSig, and proves membership facts THROUGH the abstraction with no
  visibility into the tree (`testsuite/tests/vox/mechanics/lean_via_seal.ml`
  + `lib/via_set.mli`/`.ml`).
- **Honest impl proving ACROSS the boundary** (the payoff, image-binder):
  a SEALED unit's `add`/`member` PROVE against the abstract `.mli`'s
  Set-vocab contracts with ZERO `assume_unchecked_`.  The impl unpacks
  the image binder with `refine_ t0 = s` (getting the base tree, `bst t0`,
  and the link `elems t0 = s`), builds/searches the tree behind the
  unpack, and the image-vocab contract (`ins x s`, `mem x s`) is well
  typed directly because `s` is the image.  `add`'s VC
  `elems(Node(t0,x,Leaf)) = ins x s` discharges via the link + `elems`
  def; `member` uses a local tree-recursive helper proving
  `_ = mem x (elems u)`, bridged to `mem x s` by the link
  (`testsuite/tests/vox/lib/via_set.{ml,mli}` + `mechanics/lean_via_seal.ml`).
- **Datatype via targets** (not just ghost sorts): `via (to_list : ilist)`
  where `ilist` is a local variant now verifies — the block-datatype
  emission-ordering bug is unblocked by the hash-table stack's on-sight
  emitter fix (`testsuite/tests/vox/mechanics/lean_via.ml`).

RESOLVED (image-binder pivot) — the base-binder obstacle and how it was
removed:
- The obstacle was a tension with stage 2's binder=BASE choice.  Under
  base-binder a via binder is the tree, its invariant is `bst t` (base),
  and the image is written EXPLICITLY as `elems t` — but the abstract
  `.mli` writes the via binder at the IMAGE (`mem x s`, `s` at `ISet`),
  its only vocabulary.  So the impl had to read its via param at the
  image (contracts) while constructing results from it at the base
  (`Node (s, x, Leaf)`): one via param at BOTH sorts in one VC
  (`elems (Node (s,..)) = ins x s`).  No local push resolves this — a
  substitution cannot split one variable across two sorts, and an
  includemod param-push cannot be gated (abstract `refines` and concrete
  via both present as a `Tconstr` expanding to a via `Trefine` in the
  impl's env).
- The fix (landed): IMAGE-binder.  `dsort` of a via type is the image
  everywhere; the via param IS the image (so `ins x s`/`mem x s` are
  well typed with no push), and its tree is a SEPARATE name obtained by
  `refine_` unpack (base tree + `bst t0` + link `elems t0 = t`).  The
  two sorts never collide because construction uses `t0` (base) and the
  contract uses `s` (image) — different names.  See the IMAGE-BINDER
  CORRECTION under §2 for the exact typecore/vox_verify sites.

Two properties the shipped normalization guarantees (regression-pinned):
- **Directed, not symmetric.**  `vox_flatten_view` is wired only into
  `moregen` (the value-INCLUSION channel), never `unify3`/`eqtype`/
  `mcomp`/`subtype`.  Within a module `Trefine` unification stays rigid,
  and the flatten is a no-op for a CLIENT (whose env holds no manifest
  for the abstract `t`), so a client can never unify `t` with
  `tree{ ... } via ...` — the abstraction stays opaque; only inclusion,
  in one direction, consumes the manifest expansion.
- **Overclaims fail CLOSED.**  A `refines`-over-`via` interface that
  claims more than the implementation proves (e.g. `add` returning
  `ins x (ins x s)` while the code inserts once) is rejected at the
  impl's VC — the solver refutes `elems(Node(t0,x,Leaf)) = ins x (ins x s)`
  — never a silent pass (pinned by `mechanics/lean_via_boundary_fail.ml`).

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

STAGE 4 STATUS (landed): all three ingredients parameterized, end to
end.  Provenance: the types-layer plumbing (`Vs_lean of string *
vox_sort list` + the five matching update sites -- `vox_sort_equal`,
`subst_vox_sort`, typedecl/includecore `subst_sort` and the `of_attr`
generic `Vs_param` binding, `dsort_of_vox_sort`, `dsort_equal`,
`lean_sort`/`show_vox_sort` rendering) was begun by the stage-3 agent
and, on review, adopted here.

Shipped and tested:
- **Parameterized ghost sorts.**  `type 'a iset [@@vox.sort lean
  "ISet"]` carries argument sorts, bound positionally exactly like
  `Vs_data` (`of_attr` declares the sort generically as `Vs_lean (name,
  [Vs_param 0; ...])`; a use `int iset` instantiates through the same
  `subst_sort`/`arg_sorts` path the `refines` heads use).  `int iset`
  renders `(ISet Int)`, `int iset iset` renders `(ISet (ISet Int))`,
  an opaque argument renders `(ISet VoxU)`.  DECISION (spec was silent
  on nested parenthesization): each application is wrapped in parens
  with space-separated args, mirroring `S_data`/`S_poly`, so nesting is
  unambiguous.  cmi agreement: the `[@@vox.sort lean "Name"]` attribute
  still compares by NAME (`check_sort_consistency`); the argument arity
  is enforced by ordinary OCaml type inclusion and the kind's `refines`
  (whose `Vs_param`s match positionally), so a name or arity mismatch is
  rejected.  See mechanics/lean_param_ghost.ml.
- **Parameterized via.**  `type 'a t = 'a tree{ bst _ } via (elems :
  'a iset)` verifies; the map target `vm_target` mentions the type
  parameter, `Subst` instantiates it at a use (`int t`'s manifest
  carries `int iset`).  KEY DECISION (spec framed this as storing
  `Vs_param` in `vm_sort`): the stored `vm_sort` cannot carry the use's
  instantiation -- `dsort_of_type`'s via node has NO argument sorts in
  scope (it passed `[]`, degrading any `Vs_param` to `VoxU`).  So the
  SOLVER path now reads the image from the map's TARGET TYPE
  (`vm_target`, which `Subst` instantiates correctly) via `dsort_of_type`
  -- `int t` renders its image at `(ISet Int)` -- rather than the stored
  `vm_sort` (`vox_verify` `dsort_of_type` + `register_type_specs`).  The
  stored `vm_sort` stays as the monomorphic `vox_target_sort` computes
  it (the target head's OWN parameter indices), used only for rigid
  unification (consistent across instances, which is all unification
  needs).  The one precise-index consumer -- the includecore HONESTY
  rule -- was made parameter-aware there (`sort_of_manifest` gains a
  `param_index` over `decl1.type_params`, a `Tvar` case, and computes
  the via image from `vm_target`), so a non-first-parameter target
  (`type ('a,'b) t = 'b tree via ('b iset)`) reconciles against `refines
  ('b iset)` instead of spuriously reporting `ISet ('a)` vs `ISet ('b)`.
- **Instantiated-image binders, end to end.**  lib/pset + lib/pset.mli
  generalize lib/via_set to `'a`: the sealed abstract `type 'a t : value
  refines ('a iset)` PROVES `add` HONESTLY (zero `assume_unchecked_`) at
  the GENERIC element sort -- the equation `elems (Node ..) = ins x s`
  needs no decidable equality, so one proof serves every instantiation.
  A client (mechanics/lean_pset_seal.ml) binds `int Pset.t` at `(ISet
  Int)` and proves `mem x (add x s)` through the abstraction with no view
  of the tree.
- **Typeclass fail-closed.**  A model constraining its parameter
  (`tmem {a} [DecidableEq a]`) instantiated at an opaque element
  (`opaque_elt iset` = `(ISet VoxU)`) fails at the solver with
  `synthInstanceFailed` (no `DecidableEq VoxU`) -- the RIGHT layer.  This
  relies on the emission fix: `sort_needs_voxu`/`sort_needs_iarray` now
  recurse into a ghost sort's argument sorts, so `VoxU` is DECLARED and
  the failure is typeclass synthesis, not an "unknown VoxU" identifier
  error a layer too early.  Pinned in mechanics/lean_param_ghost.ml.
- **Mutable via-typed record field** (the inherited untested corner).
  UNREACHABLE by construction: vox bars ANY mutable field from
  refinement predicates (mutable state is framed, never named), so a
  mutable via field has no version fact to speak at any sort.  The
  restriction is GENERAL, not via-specific (a mutable `int` field fails
  identically).  Pinned with the error case in mechanics/via_mutfield.ml;
  the reachable case (an IMMUTABLE via field) is nameable and speaks at
  the IMAGE sort, consistent with image-binder.

NOT delivered / honest limits:
- The value-level polymorphic type variable of a POLYMORPHIC value
  (`(x : 'a) -> ...`) collapses to `VoxU`, not a bound `Type` (this is
  pre-existing vox behavior, shared with `type 'a proph : refines ('a)`
  in lib/pvghost): sound (`VoxU` is one model; a generic VC proved at
  `VoxU` instantiates soundly), but it means the within-module generic
  proof reasons at `VoxU`, and parameterization "shines" at a CONCRETE
  instantiation (`int t` -> `(ISet Int)`) reached through a `Tconstr`
  head, not at a bare value type variable.
- `vox_target_sort` (typetexp) was NOT made parameter-aware; the stored
  `vm_sort` therefore carries the target head's own parameter indices.
  This is invisible to the solver (which reads `vm_target`) and to
  honesty (fixed at includecore), and correct for the common single-
  parameter case; a future consumer that reads `vm_sort` for a precise
  index would need the typetexp fix.
- `member` (decidable membership) is deliberately absent from the
  parameterized pset .mli: it needs `DecidableEq a`, which the generic
  proof lacks -- the library-level comparator caveat above.  It is
  demonstrated instead as the typeclass fail-closed case.

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
- Reserved-namespace collision (hardened during the origin/vox rebase):
  a ghost name is rendered VERBATIM, so one in the emitter's own
  namespaces -- `Vox_` (datatypes/tuples/opaques) or `v_` (reflected
  values) -- would silently ALIAS an emitted name (e.g. `lean "Vox_foo"`
  captured by the datatype `foo`'s `Vox_foo`).  `validate_lean_sort_name`
  now rejects those prefixes eagerly (fail closed), pinned by
  `mechanics/lean_sort_reserved.ml`.  This is the S_lean counterpart of
  the datatype `assert_uname_fresh` check.

## Suggested build order

1. Ghost sorts (`Vs_lean`), monomorphic — independently testable
   (trusted handle types with Lean-typed models).
2. `Trefine` maps generalization + typetexp/dsort changes +
   coercion-channel rules, all within one module (no abstraction yet).
3. Inclusion rule + `refines` interop → the BST/Set end-to-end test.
4. Parameterized versions of all three.

## VIA AND BORROWS (settled 2026-07-05)

`via` composes with RustHorn-style mutable borrows to give a MUTABLE
value behind a sealed model.  Reference implementation:
`testsuite/tests/vox/lib/mset_lib.{mli,ml}` (the trusted borrow
library) + `lib/mset.{mli,ml}` (the sealed set), exercised by
`mechanics/lean_mset_seal.ml` and pinned fail-closed by
`mechanics/lean_mset_fail.ml`.  A mutable finite set is mutated IN
PLACE through a borrow and sealed behind `type t : value refines
(iset)`; the payoff module proves its set-vocabulary contracts
(`insert : ... -> (s : t) @ unique -> t{ _ = ins x s } @ unique`) with
ZERO `assume_unchecked_`, the trust confined to the six-function
`mset_lib`.

1. **The key realization: mutation and models were ALREADY joined —
   by an explicit abstraction function.**  `lib/mhtbl` mutates a
   `Bslice.varr` in place while its ghost `bcts : varr -> table` names
   the immutable model; every mhtbl spec writes `bcts` explicitly, and
   `mhtbl.mli` LEAKS it (`type t = varr{ twf (bcts _) 0 && ... }`).
   `via`'s whole contribution is to make that same function the
   IMPLICIT image-binder map and SEAL it away: `type t = varr{ .. }
   via (setof : iset)` in the `.ml`, `type t : value refines (iset)`
   in the `.mli`.  The client then sees only the model (`mem`/`ins`/
   `card`), never the carrier, the abstraction function, or the borrow
   discipline.  Nothing new is proved about mutation; the abstraction
   is relocated from the client-facing type into the seal.

2. **DESIGN RULE — loans do NOT get `via` types.**  A loan stays an
   opaque token (sort `VoxU`); its `now`/`fin`/`pv` are declared as
   trusted functions landing DIRECTLY at the image sort (`snow`/`sfin`/
   `spv : VoxU -> ISet`), exactly the `bslice` pattern with the model
   sort swapped from `Htbl.table` to `ISet`.  `sinsert`'s spec
   `snow _ = ins x (snow m)` is then a borrow spec in pure set
   vocabulary.  The `via` unpack (`refine_`) lives ONLY on the owned
   endpoints: `insert` unpacks `s : t` to the base carrier `r0` with
   the link `setof r0 = s`, borrows `r0`, runs the in-place op,
   resolves the prophecy at the image, and re-injects the residual
   `varr{ setof _ = spv p }` as `t{ _ = ins x s }`.  Consequence — one
   trusted borrow library per image model (as `bslice`/`pslice` are one
   library per model).  This routes AROUND the loan-of-via question
   entirely; it never arises.

3. **The generic alternative (spec'd, NOT built).**  One borrow library
   serving EVERY `via` type would need loans to carry via types:
   `borrow` over a `base via f` value would hand back a loan of
   `base via f`, and the rule "unpacking a loan of `base via f` yields
   a loan of `base` with LINKED now/fin images" (`now(base-loan)` and
   `fin(base-loan)` related to the image loan's `now`/`fin` by `f`).
   Implementing it needs a trusted loan-linking cast (loan-of-via to
   loan-of-base, mirroring `refine_`'s owned-value unpack) plus a
   loan-aware `refine_` in typecore.  It removes the per-model library
   duplication ONLY; the routed-around design of rule 2 shows it is not
   needed for correctness, so it is a convenience/scaling feature, not
   a soundness gap.

4. **Injection caveat (shared with the value-side sighting).**
   Injecting an INLINE anonymous expression into a via type binds its
   subject at the IMAGE sort, but a carried base-carrier refinement
   still applies the map to that binder — a Lean "Application type
   mismatch" (`setof : VoxU -> ISet` applied to an `ISet`-typed
   binder).  `(mk () : t{ card _ = 0 })` fails; `let r = mk () in
   (r : t{ card _ = 0 })` passes (a let-bound value keeps its base
   sort).  Not cross-unit specific (reproduced fully local).
   Workaround throughout `mset.ml`: let-bind before injecting.

## ADDENDUM (2026-07-06): KNOWN-via value bindings bind at the skeleton (gap #31)

The IMAGE-BINDER rule above (§2) makes a via binder denote the image
EVERYWHERE, reached down to the representation only through a `refine_`
unpack.  That is right for a via PARAMETER and for an ABSTRACT
(sealed-`.mli`) value — the client has no spine — but it stranded a
value of a KNOWN (transparent, spine-visible) via type bound by an
ordinary `let`: the binder was registered at the image sort while its
construction fact spoke the base language (`v = Cons ..`, a tree term),
so the two were ill-sorted across the abstraction function (`ISet = tree`
in the emitted Lean).  Seven recorded sightings, one root cause; the
stdlib PoC's `append` and the top-level `zero`/`empty` ordering
contortions were the fallout.

**Ruling (settled 2026-07-06, implemented here):** a binder of KNOWN
`Trefine` type at a *value binding* puts its facts in the logical
context and the variable at the SKELETON sort — exactly what a
transparent `refine_` unpack does, so `refine_` becomes REDUNDANT at
such a binding (it stays supported).

Precisely, at `let v = e` where `e`'s type expands to a transparent
`Trefine(skel, maps, pred)` (`maps ≠ []`, spine visible):

- `v` is registered at `skel`'s dsort (the plain payload), not the
  image.  (`vox_verify.record_name`, guarded by a `~via_skel` flag set
  only at the three value-binding `extend_pat` callers: the single- and
  multi-`let` paths and the top-level structure bindings.  Parameters,
  match-arm binders, record fields and try-handler arms are UNCHANGED —
  they keep the image binding.)
- Injected fact = the FULL base-sort predicate `pred[_ := v]`
  (`<inv> v ∧ <image contract>(map v)`), the same facts `unpack_fact`
  contributes: both the skeleton invariant and the map-link to the image
  the RHS established.  (Not `via_image_facts`, which strips the base
  conjuncts and rebases at the image.)
- **Image rewrite for dependent occurrences.**  When such a skeleton
  binder is passed as a via ARGUMENT, the callee's contract mentions the
  parameter at the image, so the dependent substitution places the bare
  skeleton stamp where an image is expected.  A KNOWN via binder's base
  predicate has other free variables ONLY in its image-layer conjuncts
  (they arrive by that same dependent substitution), never in the
  skeleton invariant (which is closed over the binder), so those
  occurrences are rewritten to the composite map applied to them
  (`once ↦ lrepr once`); the binder itself is EXCLUDED (it is legitimately
  at the skeleton in `bst v`).  This is `rewrite_skel_via_images`, keyed
  off the `via_skel_binders` registry.

Flow-back into a via-expected position (constructor field, result /
ascription, via argument) is the EXISTING entailment/rewrap path — no
new coercion — discharged from the base facts now in context.

**Scope / boundary.**  An ABSTRACT `refines` value (its skeleton hidden)
never reaches the `Trefine` arm and is unchanged (image binder, verified
by the untouched `via_set`/`xset`/`pset`/`mset` seal-client tests).  The
INLINE-injection caveat of §4 above is a DIFFERENT (result-naming)
sighting and is NOT addressed here: `(f x : t{...})` where `f x` is an
application still mis-sorts (its result is a fresh unknown at the image);
the workaround remains "bind to a variable first", which now composes
cleanly with the skeleton binding.

**Workarounds that fell away** (proof the fix landed): `lib/peano.ml` and
`lib/bignum.ml` define `zero` FIRST instead of being forced last; the
stdlib PoC's `append` returns the via type `t` directly with a plain
`let rest = go r` recursion, no refined-skeleton return type and no
`refine_`.  Pinned by `mechanics/lean_via_letbind.ml` (positive: the
sightings) and `mechanics/lean_via_letbind_fail.ml` (soundness: a false
image equation over honest skeleton facts is refuted at grind, not a
silent pass and not an elaboration error).
