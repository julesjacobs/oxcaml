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
