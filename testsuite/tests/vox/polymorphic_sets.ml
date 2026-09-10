(* TEST
 flags = "-extension refinement_types";
 has-z3;
 timeout = "900";
 all_modules = "polymorphic_set_intf.mli polymorphic_list_set.mli polymorphic_list_set.ml polymorphic_sets.ml";
 { bytecode; }
 { flags += " -principal"; bytecode; }
*)

module Key = struct
  type t = { rank : int; payload : int }

  let[@def] (compare @ total) left right =
    if left.rank < right.rank then -1
    else if right.rank < left.rank then 1
    else 0

  let (compare_reflexive @ total) (x : t) :
      {u : unit | compare x x = 0} =
    compare_def x x;
    let u = () in
    refine_ u

  let (compare_reverse @ total) :
      (x : t) ->
      (y : t) ->
      {u : unit | (compare x y <= 0) === (compare y x >= 0)} =
    fun x y ->
    compare_def x y;
    compare_def y x;
    let u = () in
    refine_ u

  let (compare_transitive @ total) :
      (x : t) ->
      (y : t) ->
      (z : t) ->
      {u : unit |
        if compare x y <= 0 && compare y z <= 0
        then compare x z <= 0
        else true} @ immutable contended =
    fun x y z ->
    compare_def x y;
    compare_def y z;
    compare_def x z;
    let u = () in
    refine_ u
end

module Set = Polymorphic_list_set.Make (Key)

let () =
  let first = { Key.rank = 1; payload = 10 } in
  let equivalent = { Key.rank = 1; payload = 20 } in
  let second = { Key.rank = 2; payload = 30 } in
  let empty = Set.empty in
  let left = Set.add first empty in
  let right = Set.add equivalent empty in
  let larger = Set.add second left in
  let combined = Set.union left larger in
  Set.lookup_empty first;
  Set.lookup_add equivalent first empty;
  Set.lookup_union second left larger;
  Set.size_zero empty;
  Set.equal_lookup left right first;
  let (same_lookup @ total) :
      (element : Key.t) ->
      {u : unit |
        Set.lookup element larger === Set.lookup element larger} =
    fun _element ->
    let u = () in
    refine_ u
  in
  Set.extensional larger larger same_lookup;
  Format.printf
    "equivalent member = %b; semantic equal = %b; representation equal = %b; size = %s@."
    (Set.lookup equivalent combined)
    (Set.equal left right)
    (left = right)
    (Bigint.to_string (Set.size combined))
