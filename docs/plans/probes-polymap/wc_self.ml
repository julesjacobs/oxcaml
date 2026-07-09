type 'a ord [@@vox.sort lean "POrd"]
[%%vox.lean {lean|
@[grind, expose] def POrd (a : Type) : Type := a -> a -> Prop
@[grind, expose] def refl {a : Type} (o : POrd a) (x : a) : Prop := o x x
|lean}]
let pick : (o : 'a ord) -> (a : 'a) -> 'a{ refl o _ } = fun _o a -> a
