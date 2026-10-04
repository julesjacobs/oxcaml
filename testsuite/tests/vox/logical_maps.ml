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
end;;
[%%expect{|
module Demo : sig end
|}]
