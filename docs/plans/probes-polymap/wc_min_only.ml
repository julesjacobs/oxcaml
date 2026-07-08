[%%vox.lean {lean|
@[grind, expose] def ole {a : Type} (o : a -> a -> Prop) (x y : a) : Prop := o x y
@[grind, expose] def oRefl {a : Type} (o : a -> a -> Prop) : Prop := forall x, o x x
|lean}]
let min2
    (o : (('a -> 'a -> bool) [@vox.total]))
    (cmp : ((x:'a) -> (y:'a) -> bool{ (_ = true -> ole o x y) && (_ = false -> ole o y x) }))
    (a:'a) (b:'a) : 'a{ oRefl o -> (ole o _ a && ole o _ b) } =
  ignore o; if cmp a b then a else b
