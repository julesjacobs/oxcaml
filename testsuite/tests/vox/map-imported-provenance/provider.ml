module Order = struct
  type t = int
  external compare : int -> int -> int @@ total = "%compare"
  let (reflexive @ total) (x : t) :
      {u : unit | compare x x = 0} @ ghost = ghost_ (refine_ ())
  let (antisymmetric @ total) (x : t) (y : t) :
      {u : unit | (compare x y < 0) = (compare y x > 0)
        && (compare x y = 0) = (compare y x = 0)} @ ghost =
    ghost_ (refine_ ())
  let (transitive @ total) (x : t) (y : t) (z : t) :
      {u : unit | not (compare x y <= 0 && compare y z <= 0)
        || compare x z <= 0} @ ghost = ghost_ (refine_ ())
end

module M = Map.MakeTotal (Order)
module Alias = M

module Anonymous = Map.MakeLogical (struct
  type t = int
  let[@def] equal (x : int) (y : int) = x = y
  let (reflexive @ total) x : {u : unit | equal x x} =
    equal_def x x; ()
  let (symmetric @ total) x y : {u : unit | equal x y = equal y x} =
    equal_def x y; equal_def y x; ()
  let (transitive @ total) x y z :
      {u : unit | not (equal x y && equal y z) || equal x z} =
    equal_def x y; equal_def y z; equal_def x z; ()
end)
