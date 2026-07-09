type 'a rel [@@vox.sort lean "PRel"]
[%%vox.lean {lean|
@[grind, expose] def PRel (a : Type) : Type := a -> a -> Prop
@[grind, expose] def prHolds {a : Type} (r : PRel a) (x y : a) : Prop := r x y
|lean}]
let chain : (r : 'a rel) -> (x:'a) -> (y:'a) -> (z:'a)
  -> unit{ (forall a b c, prHolds r a b -> prHolds r b c -> prHolds r a c)
           -> prHolds r x y -> prHolds r y z -> prHolds r x z } =
  fun _ _ _ _ -> ()
(* client: build the relation value to feed 'r' *)
let intlt : int rel = assert false
let _demo : unit = ignore (chain intlt 0 1 2)
