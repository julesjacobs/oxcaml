type 'a ord [@@vox.sort lean "POrd"]
[%%vox.lean {lean|
@[grind, expose] def POrd (a : Type) : Type := a -> a -> Prop
@[grind, expose] def ole {a : Type} (o : POrd a) (x y : a) : Prop := o x y
|lean}]
(* NEGATIVE: reflexivity not given -> must elaborate then NOT PROVED *)
let pick_bad : (o : 'a ord) -> (a : 'a) -> 'a{ ole o _ a } = fun _o a -> a
