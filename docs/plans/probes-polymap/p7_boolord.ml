type 'a ord [@@vox.sort lean "POrd"]
[%%vox.lean {lean|
@[grind, expose] def POrd (a : Type) : Type := a -> a -> Prop
@[grind, expose] def ple {a : Type} (o : POrd a) (x y : a) : Prop := o x y
|lean}]
let le_test :
     (o : 'a ord)
  -> (cmp : (x:'a) -> (y:'a) -> int{ (_ <= 0 -> ple o x y) && (_ > 0 -> ple o y x) })
  -> (a:'a) -> (b:'a) -> bool{ _ = true -> ple o a b } =
  fun _o cmp a b -> let c = cmp a b in c <= 0
