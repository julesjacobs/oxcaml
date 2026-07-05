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
