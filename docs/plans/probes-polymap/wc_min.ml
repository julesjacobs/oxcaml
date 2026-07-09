[%%vox.lean {lean|
@[grind, expose] def ole {a : Type} (o : a -> a -> Prop) (x y : a) : Prop := o x y
@[grind, expose] def oRefl {a : Type} (o : a -> a -> Prop) : Prop := forall x, o x x
abbrev intLe : Int -> Int -> Prop := fun a b => a <= b
|lean}]
(* min of two over abstract 'a: result is <= both args (given reflexivity) *)
let min2
    (o : (('a -> 'a -> bool) [@vox.total]))
    (cmp : ((x:'a) -> (y:'a) -> bool{ (_ = true -> ole o x y) && (_ = false -> ole o y x) }))
    (a:'a) (b:'a) : 'a{ oRefl o -> (ole o _ a && ole o _ b) } =
  ignore o; if cmp a b then a else b
(* concrete int client: supply <= as the order + comparator, min discharges *)
let demo (a:int) (b:int) : int{ oRefl intLe -> (ole intLe _ a && ole intLe _ b) } =
  min2 (fun p q -> p <= q) (fun x y -> x <= y) a b
