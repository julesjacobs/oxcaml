# Vhof — shared HOF-kit substrate notes

Vhof carries the HOF-kit substrate atoms ONCE for the whole stdlib
(IntRel/IntPred/IntRel3 abbrevs + rHolds/pHolds/r3Holds wrappers). A pure
[%%vox.lean] leaf: no OCaml types or vals. Every HOF module `open Vhof` and
references these instead of declaring its own. Introduced to kill the
cross-module substrate-collision hazard (below).

### Vhof · one shared substrate module replaces per-module copy-in
- **site:** vox_stdlib/Vhof.{mli,ml}; consumers Vrel/Vlist/Voption/Vresult (open Vhof)
- **milestone/gap:** new (HOF-kit cross-module architecture)
- **what I tried:** the WP-0 recipe had each HOF module COPY the substrate atoms
  into its own block (`public abbrev IntRel`, `public def rHolds`, ...).
- **error:** the atoms are `public`, so two carriers imported together clash in
  the shared Lean namespace: `IntRel has already been declared`. Adding the
  substrate to Voption broke both Vresult (open Voption) and the Vlist.find_opt
  client (open Voption). The copy-in recipe does not compose across a dependency
  chain.
- **workaround used:** factor the substrate into ONE leaf module Vhof; every HOF
  module `open Vhof` and declares none of its own. A pure block-only module DOES
  produce an importable VoxSig olean (verified), and its `abbrev IntRel` stays
  reducible across the import so an S_arrow binder unifies against it (the
  original Vrel abbrev finding, re-verified through the Vhof import).
- **removed by:** n/a — this IS the fix (replaces the transient prefix-or-reuse
  stopgap, which now lives only in git history).
- **severity:** none (positive: removes an entire hazard class; the recipe §1 is
  now "open Vhof", not copy-in).
