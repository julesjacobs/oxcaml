[%%vox.lean {lean|
@[grind, expose] def ole {a : Type} (o : a -> a -> Prop) (x y : a) : Prop := o x y
@[grind, expose] def oRefl {a : Type} (o : a -> a -> Prop) : Prop := forall x, o x x
abbrev intLe : Int -> Int -> Prop := fun a b => a <= b
|lean}]
let min2
    (o : (('a -> 'a -> bool) [@vox.total]))
    (cmp : ((x:'a) -> (y:'a) -> bool{ (_ = true -> ole o x y) && (_ = false -> ole o y x) }))
    (a:'a) (b:'a) : 'a{ oRefl o -> (ole o _ a && ole o _ b) } =
  ignore o; if cmp a b then a else b
(* concrete int: min2 result is <= both (reflexivity of <= discharged by grind) *)
let demo (a:int) (b:int) : int{ ole intLe _ a && ole intLe _ b } =
  min2 (fun p q -> p <= q) (fun x y -> x <= y) a b
