type 'a ord [@@vox.sort lean "POrd"]
[%%vox.lean {lean|
@[grind, expose] def POrd (a : Type) : Type := a -> a -> Prop
@[grind, expose] def ole {a : Type} (o : POrd a) (x y : a) : Prop := o x y
@[grind, expose] def oRefl {a : Type} (o : POrd a) : Prop := forall x, o x x
|lean}]
(* POSITIVE: with reflexivity threaded, result <= itself proves *)
let pick_ok : (o : 'a ord) -> (a : 'a) -> 'a{ oRefl o -> ole o _ a } = fun _o a -> a
