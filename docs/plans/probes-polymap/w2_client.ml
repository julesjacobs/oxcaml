open W2_abskey
module IntKey : ORD = struct
  type t = int
  let compare : (x:int) -> (y:int) -> int{ (_ < 0 -> klt x y) && (_ = 0 -> x = y) } =
    fun x y -> if x < y then -1 else if x = y then 0 else 1
end
