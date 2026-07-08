type 'a rel [@@vox.sort lean "PRel"]
[%%vox.lean {lean|
@[grind, expose] def PRel (a : Type) : Type := a -> a -> Prop
@[grind, expose] def prHolds {a : Type} (r : PRel a) (x y : a) : Prop := r x y
@[grind, expose] def prTrans {a : Type} (r : PRel a) : Prop :=
  forall x y z, r x y -> r y z -> r x z
|lean}]

(* transitivity chain over abstract 'a via a threaded relation *)
let chain : (r : 'a rel) -> (x : 'a) -> (y : 'a) -> (z : 'a)
  -> unit{ prTrans r -> prHolds r x y -> prHolds r y z -> prHolds r x z } =
  fun _r _x _y _z -> ()
