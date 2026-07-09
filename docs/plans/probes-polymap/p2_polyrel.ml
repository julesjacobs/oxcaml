type 'a rel [@@vox.sort lean "PRel"]
[%%vox.lean {lean|
@[grind, expose] def PRel (a : Type) : Type := a -> a -> Prop
@[grind, expose] def prHolds {a : Type} (r : PRel a) (x y : a) : Prop := r x y
|lean}]

(* a poly function relating output to input through a threaded relation over 'a *)
let related_pair : (r : 'a rel) -> (x : 'a) -> (y : 'a{ prHolds r x _ })
  -> 'a{ prHolds r x _ } =
  fun r x y -> y
