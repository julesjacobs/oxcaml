# Viarray — language-needs notes

Viarray is the **zero-friction baseline** of the build: it graduates `ia_lib`
by renaming to `viarray` and adding `length`. It has **no `[%%vox.lean]`
block**, authors **no algebra**, uses **no workaround**, and hit **none** of
the pre-seeded gaps (#31/#32/M1/model-dup do not apply — there is no via
value, no branch-on-bool, no `.mli` law statement, and no model theory to
duplicate). The notes below record that as evidence, plus the one real
limitation the built-in theory imposes.

### Viarray · built-in theory supplies the whole module (no workaround needed)
- **site:** vox_stdlib/viarray.mli:16 (`length`), :18 (`get`), :21 (`unsafe_get`)
- **milestone/gap:** new (baseline / zero-friction data point)
- **what I tried:** state `length a : int{ _ = Iarray.length a }`,
  `get a i : int{ _ = a.(i) }` under `0 <= i && i < Iarray.length a`, bodies
  `Iarray.length`/`Iarray.get`/`Iarray.unsafe_get`.
- **error:** none — compiles green first try, `.ml` seal trivial (each VC is
  `Iarray.length a = Iarray.length a` / `a.(i) = a.(i)`). `length`'s free
  `0 <= _` (smoke `length_nonneg`) discharges from the theory's sole
  length-nonneg axiom with **no authored law**.
- **workaround used:** none. This is the shape a module *should* have when
  its model is a reflected built-in theory rather than an authored inductive.
- **removed by:** n/a — nothing to remove. The load-bearing fact is that the
  built-in `int iarray` theory (reflection of `Iarray.length`/`a.(i)` + the
  one compiler-owned nonneg axiom) carries the entire module. Every other
  wave-1 module pays the block/model-dup/M1 tax precisely because it lacks
  such a theory; Viarray is the contrast case.
- **severity:** COSMETIC

### Viarray · reflected array theory is `int iarray`-only
- **site:** vox_stdlib/viarray.mli:16 (the `int iarray` type in every signature)
- **milestone/gap:** new (adjacent to N2 — element/derived-op story)
- **what I tried:** the module is fixed to `int iarray`. A general
  `'a iarray` (element-polymorphic) or a mutable-`int array` variant of the
  same three ops cannot reuse this theory.
- **error:** not attempted (would not reflect) — per the mechanics test
  `mechanics/iarray.ml`: only `int iarray`'s `Iarray.length`/`a.(i)`
  reflect; "the MUTABLE array's identical primitives do not reflect" and the
  theory is "Gated on `int iarray`". A mutable `int array` read becomes a
  fresh unknown (`assume_unchecked_`), which would move the op out of the
  zero-trust ledger.
- **workaround used:** none possible in-module; stayed `int iarray` per §3.
  Mutable containers are explicitly deferred to v1.1 (behind the borrow lib),
  and non-int element types have no reflected theory today.
- **removed by:** a reflected array theory parameterized over the element
  sort (`'a iarray`), and/or a McCarthy-store reflection for mutable
  `int array` that lets writes/reads reflect without `assume_unchecked_`.
- **severity:** MINOR (a real limit, but exactly the v1.1 boundary the
  blueprint already draws — no v1 module needs more than `int iarray`).

## WP-4 additions (for_all / any / mem — the read-only query surface)

Viarray graduates from zero-block to a small block: three `@[grind, expose]`
window SPEC defs (`ia_all_from` bounded-∀, `ia_ex_from`/`ia_mem_from` bounded-∃
over `Vox_ia_get`) + three read-only index loops. No stores/unique modes (the
array is immutable). This is the queryable surface the opaque theory admits.

### Viarray · a read-only scalar loop over the OPAQUE iarray theory verifies (positive)
- **site:** viarray.mli/ml (`for_all`/`any`/`mem`); probe iaAll (scratch)
- **milestone/gap:** new (the opaque-theory query ceiling — how high is it?)
- **what I tried:** `for_all p test a : bool{ _ = ia_all_from p a 0 (length a) }`
  with an inner `go (i : int{0<=_}) : bool{ _ = ia_all_from p a i n }` recursion.
- **error:** none — the loop seals. `Vox_ia_get` is total in the logic, and the
  window predicate exposed as a bounded quantifier lets grind discharge each
  step/base obligation by UNFOLDING it (the bounds `0<=i` and `i<n=length a` keep
  every `a.(i)` in range). So a per-element read-loop is fully within reach of the
  opaque theory — the theory's limit is CONSTRUCTION, not iteration.
- **workaround used:** none — this is the shippable shape for array queries.
- **removed by:** n/a — positive result; nothing to remove.
- **severity:** COSMETIC (positive: the query ceiling is higher than "get/length only").

### Viarray · the step/done window laws are DEAD (exposed ∀/∃ defs subsume them)
- **site:** viarray.mli (an earlier draft shipped ia_all_step/done etc.)
- **milestone/gap:** §6.7 dead-law (exposed spec def)
- **what I tried:** ship step (`window i = head ∧ window (i+1)`) and done (`empty
  window past the end`) laws as the loop's proof vocabulary, as a via-container
  would.
- **error:** the removal test proved them ALL dead — with the window defs
  `@[grind, expose]`, grind proves every loop obligation by unfolding the ∀/∃, so
  deleting all six step/done laws leaves mli/ml/smoke green.
- **workaround used:** dropped all six laws; keep only the exposed window defs.
  Exposed bounded-∀/∃ SPEC defs are sanctioned (blueprint 6.7 excludes forall/
  exists spec defs from the dead-law warning), so exposure is correct here and
  there is nothing to keep live.
- **removed by:** n/a — the fix WAS the removal (dropped the dead laws).
- **severity:** COSMETIC (guidance — don't ship step/done laws for an exposed
  quantified window predicate; the unfold does the work).

### Viarray · CONSTRUCTION ops are unshippable — the opaque theory has no constructor (L10/N2)
- **site:** viarray.mli (absent: map/sub/append/of_list/fill/blit)
- **milestone/gap:** L10 (int-iarray-only) / N2 (no constructor)
- **what I tried:** the inventory's fill/blit/of_list/to_list/map/sub surface.
- **error:** not attempted to completion — the built-in theory is `opaque VoxIA`
  with `Vox_ia_len`/`Vox_ia_get` and ONE axiom (length nonneg); there is NO
  constructor, so an op that BUILDS a new array cannot state its result's length
  or elements (nothing relates a fresh VoxIA to the input). fill/blit additionally
  need MUTATION, which the immutable `int iarray` theory does not model (a mutable
  `int array` read becomes `assume_unchecked_`, N2). of_list/to_list would need to
  bridge VoxIA to the LList model across a loop with no constructor to anchor on.
- **workaround used:** none — these stay out of Viarray. Queries (for_all/any/mem)
  are the shippable half; construction is the v1.1 boundary the blueprint draws.
- **removed by:** a reflected array theory with a constructor (element+length of a
  built array), a mutable McCarthy `int array` reflection, and an element-poly
  `'a iarray` theory (N2).
- **severity:** MINOR (a real limit, exactly the drawn v1.1 boundary).

### Viarray · `exists_` collides with the vox refinement quantifier keyword
- **site:** viarray.mli (`any`, née `exists_`)
- **milestone/gap:** new (grammar; minor)
- **what I tried:** name the exists-query `exists_` (mirroring `forall_`/`exists_`
  quantifier spelling).
- **error:** `Syntax error` at the `val exists_` — `exists_` is a reserved
  refinement quantifier token in the vox grammar, so it cannot be an OCaml val name.
- **workaround used:** renamed the val to `any`.
- **removed by:** n/a — naming choice, no compiler change wanted.
- **severity:** COSMETIC (naming; the underscore-quantifier tokens are reserved).
