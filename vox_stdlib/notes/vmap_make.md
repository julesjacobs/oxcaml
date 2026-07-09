# Vmap_make — the ORD-functor map (headline) + functor-specific frictions

`Map.Make(ORD)` for int-representable ordered KEYS, int values, productized
from the proven functor probe `testsuite/tests/vox/mechanics/lean_functor_bst.ml`.
Model: characteristic function `MMap := Int -> MOpt` (funext-equality IS map
equality → STRUCTURAL specs `_ = m_add k v m`). OVERWRITE add (idiomatic
Map.add), vs flat `Vmap`'s prepend-shadow assoc list. Ordered BST keyed by
`O.t`; the model `tfind`/`tins` navigate by the SAME ordered comparison the
impl uses, so no `bst` invariant is carried (the calibration below). Verified
green: `.mli` + `.ml` seal + `clients/smoke_vmap_make.ml` (cross-unit
instantiation) + the DISPROVED bad-order instance. Compiler
`vox-kindsfix/_install/bin/ocamlc.opt`, pinned Lean 4.31.0.

Ops shipped: `empty` / `find` (→ `mopt`) / `add` / `mem` / `singleton`.
Laws shipped (all LIVE, opaque-op algebra): `m_find_empty`, `m_find_add_eq`,
`m_find_add_ne`.

## Crisp boundary — what Make CAN and CANNOT express today

CAN: an idiomatic int-keyed ordered map, sound, cross-unit-instantiable, with
a REAL instantiation obligation (lawful `IntOrd` seals; sign-flipped comparator
DISPROVED with a validated counterexample `x=-1,y=0`). Structural specs (map
equality) because the model is a char-function.

CANNOT (this tip): (a) `remove` — an ordered-BST delete (min-extraction /
re-link) is materially more proof than the insert/lookup core; DEFERRED (not
needed for the "Map with an ordered compare" headline). (b) `cardinal`/ordered
`fold`/`bindings` — a char-function model has no support size or enumeration;
those want the inductive-sorted-list model (an upgrade path: sorted assoc list
is canonical, so structural = AND cardinality, at the cost of carrying a
sortedness invariant + canonicity proof). (c) a GENUINELY ABSTRACT key with
total-order axioms — blocked at instantiation (probe `aord`, §6d of the
opaque-unification doc); the poly fallback is `Vpmap_ord` (parameter-style).

---

### Vmap_make · model + tree bridge MUST live at file top level (L4 register)
- **site:** the `[%%vox.lean]` block sits above the functor; the tree type is
  inside `Make` and emits at `Vox_Vmap_make_tree`, which the block references.
- **milestone/gap:** 6a (functor block home)
- **what I tried:** none other — this is forced.
- **error:** a `[%%vox.lean]` block nested in a module/functor body is rejected
  (`vox: [%%vox.lean] blocks are unit-level; move to the file top level`,
  vox_verify `check_no_nested_blocks_*`).
- **workaround used:** declare the whole model (MMap, m_*, the tree model
  tfind/tins, the abstraction mmap, all lemmas) at file top level; reference the
  functor's datatype by its emitted `Vox_<Unit>_tree` name.
- **removed by:** module-type/functor-scoped blocks — a real feature (new
  artifact granularity `VoxSig_<Unit.Functor>` + name resolution + which-clients
  scoping). First sizing: MEDIUM. This is the top L4-register item for functors.
- **severity:** MAJOR-ERGONOMIC (but the workaround is clean and total).

### Vmap_make · char-function sort needs `abbrev`, not `def`, in the VoxSig
- **site:** vox_stdlib/Vmap_make.mli `public abbrev MMap := Int -> Vox_Vmap_make_mopt`
- **milestone/gap:** new (interface-block reducibility)
- **what I tried:** `public def MMap := Int -> ...` (what the single-file demo
  uses for `ISet`).
- **error:** the emitted VoxSig olean fails to typecheck the model ops:
  `Function expected ... but this term has type MMap` / `not unfolded because
  their definition is not exposed: MMap ↦ 1`. A `def` sort is opaque across the
  olean re-import, so `m k` (applying a map) does not see `MMap` reduce to a
  function type.
- **workaround used:** `abbrev` (= `@[reducible] def`), which stays reducible
  through the import so `m k` elaborates.
- **removed by:** n/a — `abbrev` is correct. NOTE: this is INVISIBLE in the
  single-file demo (in-file the def is checked with full unfolding); it only
  bites the `.mli`→olean path, i.e. exactly the productized/cross-unit case.
- **severity:** MINOR (once known); a real trap for anyone porting an in-file
  functor demo to an `.mli`/`.ml` pair.

### Vmap_make · grind will not beta-reduce a char-function point-update
- **site:** vox_stdlib/Vmap_make.mli model ops (opaque) + the 3 laws
- **milestone/gap:** new (char-function algebra liveness)
- **what I tried:** ship `m_find`/`m_add`/`m_empty` as `@[grind, expose]` defs
  (char functions) and let the client's grind compute.
- **error:** the same-key case unfolds, but the DIFFERENT-key case
  `m_find 1 (m_add 2 v m)` is NOT PROVED: grind unfolds `m_add` to a lambda but
  does not BETA-reduce `(fun k' => if k'=2 ...) 1`, so the `if 1=2` never fires.
- **workaround used:** the Vmap/oset OPAQUE-op pattern — declare m_find/m_add/
  m_empty as opaque axioms and ship `m_find_empty`/`m_find_add_eq`/
  `m_find_add_ne` as laws, each proven in the `.ml` by `simp only [m_find,
  m_add]` (simp DOES beta) `; grind`. Opaque ops keep the laws LIVE (a client
  computes only through them) — verified by deletion (each law removed ⇒ exactly
  its smoke goal fails: add_eq→line 33, add_ne→line 39, empty→line 57).
- **removed by:** a grind that beta-reduces applied lambda-defs, OR a
  spec-only/sealed-against-client-unfold export (the Vmap M3 dead-law lint).
- **severity:** MAJOR-ERGONOMIC (it makes the "clean structural char-function
  model" still need the opaque-op ceremony).

### Vmap_make · refinement grammar rejects `!=` / bare exposed-ADT constructor
- **site:** vox_stdlib/Vmap_make.ml `mem`'s inner `go` — first tried
  `bool{ _ = (tfind k u != MNone) }`
- **milestone/gap:** (known Vmap friction, reconfirmed for the map result ADT)
- **error:** `Syntax error` — the refinement predicate grammar has no `!=` and
  no bare/dotted exposed-ADT constructor term.
- **workaround used:** a named Prop def `t_haskey k t := tfind k t ≠ .MNone`
  (`@[grind, expose]`), refine against `bool{ _ = t_haskey k u }`.
- **removed by:** allow exposed-ADT constructors + `≠` in refinements.
- **severity:** MINOR. Also reconfirmed: block defs must be ordered
  (`t_haskey` referencing `tfind` had to move BELOW `tfind`; Lean forward-ref).

### Vmap_make · no `bst` invariant needed (calibration vs the set demo)
- **site:** vox_stdlib/Vmap_make.ml `type t = tree{ 0 = 0 } [@vox.via ...]`
- **milestone/gap:** none (positive calibration)
- **what I tried:** carry `bst` like the set demo.
- **result:** not needed. The set demo's MODEL `tmem` searches the WHOLE tree
  (`x=v ∨ tmem l ∨ tmem r`), so proving impl-search = model needs `bst` + the
  `not_mem_lt/gt` lemmas. Here the MODEL `tfind` navigates by the same ordered
  comparison as the impl, so `tfind_tins` holds STRUCTURALLY (`induction t <;>
  grind`, no invariant). Dropping `bst` removes the `bst_tins`/`all_lt`/`all_gt`
  obligations at zero spec cost. The tree is still a real ordered BST in
  practice (every `add` navigates by `compare`); `bst` would only PROVE that,
  which the map specs don't require.
- **severity:** none (recorded to calibrate exactly when `bst` is load-bearing).

### Vmap_make · cross-unit instantiation is a first-class citizen (F-1)
- **site:** vox_stdlib/clients/smoke_vmap_make.ml (`module M = Make (IntOrd)` in
  a SEPARATE unit) + smoke_vmap_make_bad.ml
- **milestone/gap:** F-1 (confirmed for the full functor, not just Chk)
- **result:** positive smoke EXIT 0 (proves find/mem/add facts THROUGH the
  sealed abstraction, no view of the tree); the sign-flipped comparator is
  DISPROVED at BadOrd.compare's own contract VC (validated counterexample). The
  element-mentioning `compare` contract crosses the `.cmi`. Prior art's "in-file
  only" restriction is genuinely lifted (commit 46e813323, in this tip).
- **severity:** none (positive — the headline works cross-unit).
