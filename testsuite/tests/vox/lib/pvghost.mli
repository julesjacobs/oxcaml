type ilist =
  | Nil
  | Cons of int * ilist

[%%vox.lean {lean|
@[grind, expose] public def sorted : Vox_Pvghost_ilist -> Prop
  | .Nil => True
  | .Cons _ .Nil => True
  | .Cons x (.Cons y t) => x <= y ∧ sorted (.Cons y t)
|lean}]

(* ONE prophecy type for every sort: a prophecy over 'a denotes an 'a *)
type 'a proph : value refines ('a)

val new_proph : unit -> 'a proph @ unique

val resolve : (p : 'a proph) @ unique -> (v : 'a) -> unit{ p = v }
