type 'a ord [@@vox.sort lean "POrd"]
[%%vox.lean {lean|
@[grind, expose] def POrd (a : Type) : Type := a -> a -> Prop
@[grind, expose] def ple {a : Type} (o : POrd a) (x y : a) : Prop := o x y
|lean}]

(* the comparator VALUE carries a spec tying its sign to the abstract order o *)
let min2 :
     (o : 'a ord)
  -> (cmp : (x:'a) -> (y:'a) -> int{ (_ <= 0 -> ple o x y) && (_ > 0 -> ple o y x) })
  -> (a : 'a) -> (b : 'a)
  -> 'a{ ple o _ a || ple o _ b } =
  fun _o cmp a b -> let c = cmp a b in if c <= 0 then a else b
