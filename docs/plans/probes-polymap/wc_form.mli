type 'a ord [@@vox.sort lean "POrd"]
[%%vox.lean {lean|
@[grind, expose] public def POrd (a : Type) : Type := a -> a -> Prop
@[grind, expose] public def ple {a : Type} (o : POrd a) (x y : a) : Prop := o x y
|lean}]
val pick : (o : 'a ord) -> (a : 'a) -> 'a{ ple o a _ }
