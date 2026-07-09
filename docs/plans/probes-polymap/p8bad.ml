[%%vox.lean {lean|
@[grind, expose] def oleH {a : Type} (o : a -> a -> Prop) (x y : a) : Prop := o x y
abbrev intLe : Int -> Int -> Prop := fun a b => a <= b
|lean}]
let leq
    (o : (('a -> 'a -> bool) [@vox.total]))
    (cmp : ((x:'a) -> (y:'a) -> bool{ _ = oleH o x y }))
    (a:'a) (b:'a) : bool{ _ = oleH o a b } =
  ignore o; cmp a b
(* WRONG: comparator body a>=b cannot satisfy contract _ = oleH (<=) *)
let demo (a:int) (b:int) : bool{ _ = oleH intLe a b } =
  leq (fun p q -> p <= q) (fun x y -> x >= y) a b
