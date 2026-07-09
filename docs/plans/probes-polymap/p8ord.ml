[%%vox.lean {lean|
@[grind, expose] public def oleH {a : Type} (o : a -> a -> Prop) (x y : a) : Prop := o x y
|lean}]
let leq
    (o : (('a -> 'a -> bool) [@vox.total]))
    (cmp : ((x:'a) -> (y:'a) -> bool{ _ = oleH o x y }))
    (a:'a) (b:'a) : bool{ _ = oleH o a b } =
  ignore o; cmp a b
