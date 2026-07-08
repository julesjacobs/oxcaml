type 'a ord [@@vox.sort lean "POrd"]
type 'a lst = Nil | Cons of 'a * 'a lst
[%%vox.lean {lean|
@[grind, expose] def POrd (a : Type) : Type := a -> a -> Prop
@[grind, expose] def ple {a : Type} (o : POrd a) (x y : a) : Prop := o x y
@[grind, expose] def sorted {a : Type} (o : POrd a) : Vox_P6_sortlist_lst a -> Prop
  | .Nil => True
  | .Cons _ .Nil => True
  | .Cons x (.Cons y r) => ple o x y /\ sorted o (.Cons y r)
|lean}]
(* container-of-'a result refined by an order predicate -- concrete head 'lst' *)
let just_nil : (o : 'a ord) -> 'a lst{ sorted o _ } = fun _o -> Nil
