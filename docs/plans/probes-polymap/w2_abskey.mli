type mykey [@@vox.sort lean "MyKey"]
[%%vox.lean {lean|
public opaque MyKey : Type
public axiom klt : MyKey -> MyKey -> Prop
public axiom klt_trans (a b c : MyKey) : klt a b -> klt b c -> klt a c
public axiom klt_irrefl (a : MyKey) : ¬ klt a a
|lean}]
module type ORD = sig
  type t : value refines (mykey)
  val compare : (x:t) -> (y:t) -> int{ (_ < 0 -> klt x y) && (_ = 0 -> x = y) }
end
