(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
 }
*)

module Bad_order = struct
  type t = int
  let (compare @ total) (_ : int) (_ : int) = 1
end;;
[%%expect{|
module Bad_order : sig type t = int val compare : int -> int -> int end
|}]

module Bad_set = Set.MakeTotal (Bad_order);;
[%%expect{|
Line 1, characters 17-42:
1 | module Bad_set = Set.MakeTotal (Bad_order);;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match:
       sig type t = int val compare : int -> int -> int end
     is not included in Set.TotalOrderedType
     The value "reflexive" is required but not provided
     File "set.mli", lines 75-76, characters 4-22: Expected declaration
     The value "antisymmetric" is required but not provided
     File "set.mli", lines 77-79, characters 4-66: Expected declaration
     The value "transitive" is required but not provided
     File "set.mli", lines 80-82, characters 4-45: Expected declaration
|}]

module Bad_map = Map.MakeTotal (Bad_order);;
[%%expect{|
Line 1, characters 17-42:
1 | module Bad_map = Map.MakeTotal (Bad_order);;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match:
       sig type t = int val compare : int -> int -> int end
     is not included in Map.TotalOrderedType
     The value "reflexive" is required but not provided
     File "map.mli", lines 74-75, characters 4-22: Expected declaration
     The value "antisymmetric" is required but not provided
     File "map.mli", lines 76-78, characters 4-66: Expected declaration
     The value "transitive" is required but not provided
     File "map.mli", lines 79-81, characters 4-45: Expected declaration
|}]

module Bad_labeled = MoreLabels.Set.MakeTotal (Bad_order);;
[%%expect{|
Line 1, characters 21-57:
1 | module Bad_labeled = MoreLabels.Set.MakeTotal (Bad_order);;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match:
       sig type t = int val compare : int -> int -> int end
     is not included in MoreLabels.Set.TotalOrderedType
     The value "reflexive" is required but not provided
     File "moreLabels.mli", lines 1211-1212, characters 6-24:
       Expected declaration
     The value "antisymmetric" is required but not provided
     File "moreLabels.mli", lines 1213-1215, characters 6-68:
       Expected declaration
     The value "transitive" is required but not provided
     File "moreLabels.mli", lines 1216-1218, characters 6-47:
       Expected declaration
|}]

module False_reflexivity = struct
  let[@def] compare (_x : int) (_y : int) = 1
  let (reflexive @ total) (x : int) :
      {u : unit | compare x x = 0} @ ghost =
    ghost_ (compare_def x x; refine_ ())
end;;
[%%expect{|
Line 5, characters 29-39:
5 |     ghost_ (compare_def x x; refine_ ())
                                 ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module False_antisymmetry = struct
  let[@def] compare (x : int) (y : int) = if x = y then 0 else 1
  let (antisymmetric @ total) (x : int) (y : int) :
      {u : unit | (compare x y < 0) = (compare y x > 0)} @ ghost =
    ghost_ (compare_def x y; compare_def y x; refine_ ())
end;;
[%%expect{|
Line 5, characters 46-56:
5 |     ghost_ (compare_def x y; compare_def y x; refine_ ())
                                                  ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module False_transitivity = struct
  let[@def] compare (x : int) (y : int) =
    if x = y then 0
    else if (x = 0 && y = 1) || (x = 1 && y = 2) || (x = 2 && y = 0)
    then -1 else 1
  let (transitive @ total) (x : int) (y : int) (z : int) :
      {u : unit | not (compare x y <= 0 && compare y z <= 0)
        || compare x z <= 0} @ ghost =
    ghost_ (compare_def x y; compare_def y z; compare_def x z; refine_ ())
end;;
[%%expect{|
Line 9, characters 63-73:
9 |     ghost_ (compare_def x y; compare_def y z; compare_def x z; refine_ ())
                                                                   ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
