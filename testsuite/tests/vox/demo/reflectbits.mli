(* A library that DECLARES a reflected primitive in its interface: the [%%vox.lean] block
   (compiled to VoxSig_Reflectbits.olean) supplies the Lean symbol and its laws, and
   [@@vox.reflect] binds the value to it. Both ride the .cmi, so a client gets the binding
   AND the model with no flag and no local block. The laws are proved [theorem]s (not
   [axiom]s), so nothing is assumed on the Lean side either -- the only TCB is the [imin]
   / [bmin] correspondence itself. *)

[%%vox.lean
  {lean|
@[grind, expose] public def bmin (x y : Int) : Int := if x <= y then x else y
@[grind] public theorem bmin_idem (x : Int) : bmin x x = x := by grind [bmin]
@[grind] public theorem bmin_comm (x y : Int) : bmin x y = bmin y x := by
  grind [bmin]
|lean}]

val imin : int -> int -> int [@@vox.reflect "bmin"]
