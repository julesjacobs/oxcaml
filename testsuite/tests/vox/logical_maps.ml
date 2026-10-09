(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

module Demo : sig end = struct
  module Key = struct
    type t = { group : int; id : int }
    let[@def] equal (x : t) (y : t) = x.group = y.group
    let (reflexive @ total) (x : t) : {u : unit | equal x x} =
      equal_def x x; ()
    let (symmetric @ total) (x : t) (y : t) :
        {u : unit | equal x y = equal y x} =
      equal_def x y; equal_def y x; ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (equal x y && equal y z) || equal x z} =
      equal_def x y; equal_def y z; equal_def x z; ()
  end
  module M = Map.MakeLogical (Key)

  let (empty @ total) (key : Key.t) :
      {u : unit | M.find_opt key (M.empty ()) === None
        && M.cardinal (M.empty ()) = 0Z} @ ghost = ghost_ ()

  let (read_write @ total) (m : int M.t) (k : Key.t) (v : int) :
      {u : unit | M.find_opt k (M.add k v m) === Some v} @ ghost = ghost_ ()

  let (overwrite @ total) (m : int M.t) (k : Key.t) (v : int) (w : int) :
      {u : unit | M.add k v (M.add k w m) === M.add k v m} @ ghost = ghost_ ()

  let (remove_absent @ total) (m : int M.t) (k : Key.t) :
      {u : unit | M.find_opt k m === None} @ ghost ->
      {u : unit | M.remove k m === m} @ ghost = fun _ -> ghost_ ()

  let (sizes @ total) (m : int M.t) (k : Key.t) (v : int) :
      {u : unit | M.cardinal (M.add k v m) =
        (if M.mem k m then M.cardinal m else Bigint.add (M.cardinal m) 1Z)
        && M.cardinal (M.remove k m) =
        (if M.find_opt k m === None then M.cardinal m
         else Bigint.sub (M.cardinal m) 1Z)} @ ghost = ghost_ ()

  let (conditional_cardinal @ total) (k : Key.t)
      (b0 : bool) (b1 : bool) (b2 : bool) (b3 : bool) (b4 : bool)
      (b5 : bool) (b6 : bool) (b7 : bool) (b8 : bool) (b9 : bool)
      (b10 : bool) (b11 : bool) (b12 : bool) (b13 : bool) (b14 : bool)
      (b15 : bool) (b16 : bool) (b17 : bool) : unit @ ghost = ghost_ (
    let m0 = M.empty () in
    let m1 = if b0 then M.add k 1 m0 else M.remove k m0 in
    let m2 = if b1 then M.add k 2 m1 else M.remove k m1 in
    let (_ : {n : Bigint.t | n = if b1 then 1Z else 0Z}) =
      M.cardinal m2 in
    let m3 = if b2 then M.add k 3 m2 else M.remove k m2 in
    let m4 = if b3 then M.add k 4 m3 else M.remove k m3 in
    let m5 = if b4 then M.add k 5 m4 else M.remove k m4 in
    let m6 = if b5 then M.add k 6 m5 else M.remove k m5 in
    let m7 = if b6 then M.add k 7 m6 else M.remove k m6 in
    let m8 = if b7 then M.add k 8 m7 else M.remove k m7 in
    let m9 = if b8 then M.add k 9 m8 else M.remove k m8 in
    let m10 = if b9 then M.add k 10 m9 else M.remove k m9 in
    let m11 = if b10 then M.add k 11 m10 else M.remove k m10 in
    let m12 = if b11 then M.add k 12 m11 else M.remove k m11 in
    let m13 = if b12 then M.add k 13 m12 else M.remove k m12 in
    let m14 = if b13 then M.add k 14 m13 else M.remove k m13 in
    let m15 = if b14 then M.add k 15 m14 else M.remove k m14 in
    let m16 = if b15 then M.add k 16 m15 else M.remove k m15 in
    let m17 = if b16 then M.add k 17 m16 else M.remove k m16 in
    let m18 = if b17 then M.add k 18 m17 else M.remove k m17 in
    let _ = M.cardinal m18 in
    ())

  let (equivalent @ total) (m : int M.t) (x : Key.t) (y : Key.t)
      (v : int) : {u : unit | not (Key.equal x y) ||
        (M.add x v m === M.add y v m
         && M.find_opt y (M.add x v m) === Some v)} @ ghost = ghost_ ()

  let (commute @ total) (m : int M.t) (x : Key.t) (y : Key.t)
      (v : int) (w : int) : {u : unit | Key.equal x y ||
        M.add y w (M.add x v m) === M.add x v (M.add y w m)} @ ghost = ghost_ ()
  let (extensional @ total) (left : int M.t) (right : int M.t)
      (proof : (key : Key.t) ->
        {u : unit | M.find_opt key left === M.find_opt key right} @ ghost)
      : {u : unit | left === right} @ ghost = ghost_ (
    match M.Proof.difference left right with
    | None -> ()
    | Some key -> proof key)

  module By_id = struct
    type t = Key.t
    let[@def] equal (x : t) (y : t) = x.id = y.id
    let (reflexive @ total) x : {u : unit | equal x x} = equal_def x x; ()
    let (symmetric @ total) x y : {u : unit | equal x y = equal y x} =
      equal_def x y; equal_def y x; ()
    let (transitive @ total) x y z :
        {u : unit | not (equal x y && equal y z) || equal x z} =
      equal_def x y; equal_def y z; equal_def x z; ()
  end
  module N = Map.MakeLogical (By_id)

  let (different_equalities @ total) () : {u : unit |
      M.cardinal (M.add {group=0; id=2} 20
        (M.add {group=0; id=1} 10 (M.empty ()))) = 1Z
      && N.cardinal (N.add {group=0; id=2} 20
        (N.add {group=0; id=1} 10 (N.empty ()))) = 2Z} @ ghost = ghost_ (
    Key.equal_def {group=0; id=1} {group=0; id=2};
    By_id.equal_def {group=0; id=1} {group=0; id=2}; ())

  module Alias = M
  module Ascribed : module type of M = M
  let (aliases @ total) (m : int M.t) (k : Key.t) (v : int) :
      {u : unit | Ascribed.find_opt k (Alias.add k v m) === Some v}
      @ ghost = ghost_ ()

  let (value_sorts @ total) (k : Key.t) :
      {u : unit | M.find_opt k (M.add k true (M.empty ())) === Some true
        && M.find_opt k (M.add k 12 (M.empty ())) === Some 12}
      @ ghost = ghost_ ()

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

  let (anonymous @ total) (m : int Anonymous.t) (k : int) (v : int) :
      {u : unit | Anonymous.find_opt k (Anonymous.add k v m) === Some v
        && Anonymous.cardinal (Anonymous.add k v (Anonymous.empty ())) = 1Z}
      @ ghost = ghost_ ()
end;;
[%%expect{|
module Demo : sig end
|}]
