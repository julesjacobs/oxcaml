(* Pays Vhof.mli: the substrate atoms restated (model-dup tax) without `public`.
   No obligations -- pure defs -- so the seal only re-elaborates the block. *)
[%%vox.lean {lean|
abbrev IntRel := Int -> Int -> Prop
abbrev IntPred := Int -> Prop
abbrev IntRel3 := Int -> Int -> Int -> Prop
@[grind, expose] def rHolds (r : IntRel) (a b : Int) : Prop := r a b
@[grind, expose] def pHolds (p : IntPred) (x : Int) : Prop := p x
@[grind, expose] def r3Holds (r : IntRel3) (a b c : Int) : Prop := r a b c
|lean}]
