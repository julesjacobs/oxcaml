type 'a rel [@@vox.sort lean "PRel"]
[%%vox.lean {lean|
@[grind, expose] def PRel (a : Type) : Type := a -> a -> Prop
@[grind, expose] def prHolds {a : Type} (r : PRel a) (x y : a) : Prop := r x y
|lean}]
let use_at_int : (r : int rel) -> (x : int) -> (y : int{ _ = x })
  -> unit{ prHolds r x y } =
  fun _r _x _y -> ()
