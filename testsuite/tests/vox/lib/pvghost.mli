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

(* A prophecy over 'a can be minted only against a WITNESS that 'a is
   inhabited (the RustHorn discipline: a prophecy is born from a live
   value).  [w] is a type-level witness -- discarded operationally --
   so [empty proph] / [(unit{ false }) proph] are simply unwriteable:
   there is no value to pass.  This closes the inhabitation hole that
   [unit -> 'a proph] left open (a prophecy over an uninhabited sort
   would assert its own existence). *)
val new_proph : (w : 'a) -> 'a proph @ unique

val resolve : (p : 'a proph) @ unique -> (v : 'a) -> unit{ p = v }
