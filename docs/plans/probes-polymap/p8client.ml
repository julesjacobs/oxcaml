[%%vox.lean {lean|
abbrev intLe : Int -> Int -> Prop := fun a b => a <= b
|lean}]
(* comparator names the order via the reducible abbrev *)
let cmp_int : (x:int) -> (y:int) -> bool{ _ = P8ord.oleH intLe x y } =
  fun x y -> x <= y
(* call leq with int's <= as the total order + the comparator; result carries the ordered fact *)
let demo (a:int) (b:int) : bool{ _ = P8ord.oleH intLe a b } =
  P8ord.leq (fun p q -> p <= q) cmp_int a b
