(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
*)
module Demo : sig end = struct
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
module S = Set.MakeTotal (Order)

let shared_union x : {b : bool | b} =
  let s = S.Refined.singleton x in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let s = S.Refined.union s s in
  let b = S.mem x s in
  refine_ b

let shared_inter x : {b : bool | b} =
  let s = S.Refined.singleton x in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let s = S.Refined.inter s s in
  let b = S.mem x s in
  refine_ b

let shared_diff x : {b : bool | b === false} =
  let s = S.Refined.singleton x in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let s = S.Refined.diff s s in
  let b = S.mem x s in
  refine_ b

end;;
[%%expect{|
module Demo : sig end
|}]
