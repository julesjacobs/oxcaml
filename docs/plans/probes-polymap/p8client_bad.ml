[%%vox.lean {lean|
abbrev intLe : Int -> Int -> Prop := fun a b => a <= b
|lean}]
let cmp_int : (x:int) -> (y:int) -> bool{ _ = P8ord.oleH intLe x y } =
  fun x y -> x <= y
let demo (a:int) (b:int) : bool{ _ = true } =
  P8ord.leq (fun p q -> p <= q) cmp_int a b
