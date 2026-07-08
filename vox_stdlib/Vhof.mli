(* Vhof -- the shared HOF-kit substrate, carried ONCE for the whole stdlib.

   A pure [%%vox.lean] leaf: no OCaml types or vals, only the relation/predicate
   ATOMS every higher-order module needs -- the relation/predicate function-type
   abbrevs and the pass-whole wrappers. Every HOF module (Vrel, Vlist, Voption,
   Vresult, ...) does `open Vhof` and references these instead of declaring its
   own copy. That removes the cross-module substrate-collision hazard: the atoms
   are `public`, so two modules each declaring their own would clash in the
   shared Lean namespace when imported together (the WP-1 finding). With Vhof
   there is exactly one declaration.

   House rules baked in:
   - IntRel/IntPred/IntRel3 are `abbrev`s (reducible), NOT `def`s: an S_arrow
     binder `(r : (int -> int -> bool))` emits a bare `Int -> Int -> Prop`, and
     an opaque imported `def` would not unfold to unify against it across the
     artifact boundary (the original Vrel finding). An `abbrev` stays reducible
     through the import, so unification succeeds (verified: a client unfolds
     Vhof.IntRel / Vhof.rHolds across the import).
   - rHolds/pHolds/r3Holds are `@[grind, expose]`: a relation/predicate binder
     is only ever APPLIED through these wrappers, so it appears in the VC as a
     bare hypothesis and grind unfolds the wrapper against the substituted
     lambda. Exposing them is correct (the client reasons THROUGH them), the
     contrast to the opaque-data-op house rule. *)
[%%vox.lean {lean|
public abbrev IntRel := Int -> Int -> Prop
public abbrev IntPred := Int -> Prop
public abbrev IntRel3 := Int -> Int -> Int -> Prop
@[grind, expose] public def rHolds (r : IntRel) (a b : Int) : Prop := r a b
@[grind, expose] public def pHolds (p : IntPred) (x : Int) : Prop := p x
@[grind, expose] public def r3Holds (r : IntRel3) (a b c : Int) : Prop := r a b c
|lean}]
