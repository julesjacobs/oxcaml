(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
 }
*)

module Demo : sig end = struct
  module Key = struct
    type t = { group : int; id : int }
    let[@def] compare (x : t) (y : t) =
      if x.group < y.group then -1 else if x.group > y.group then 1
      else if x.id < y.id then -1 else if x.id > y.id then 1 else 0
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost =
      ghost_ (compare_def x x; ())
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ (compare_def x y; compare_def y x; ())
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost =
      ghost_ (compare_def x y; compare_def y z; compare_def x z; ())
  end

  module M = Map.MakeTotal (Key)
  module More_labeled = MoreLabels.Map.MakeTotal (Key)
  module Ordinary = Map.Make (Key)
  module Alias = M
  module Ascribed : module type of M = M

  type 'a wrapped = {map : 'a M.t}

  let to_more_labels : 'a. 'a M.t -> 'a More_labeled.t = fun map -> map
  let from_more_labels : 'a. 'a More_labeled.t -> 'a M.t = fun map -> map

  let (joined_lookup @ total) b (key @ total) =
    let left = M.Refined.singleton key 10 in
    let right = M.Refined.singleton key 20 in
    let wrapped = if b then {map = left} else {map = right} in
    let map = wrapped.map in
    let found = M.Refined.find map key in
    let (_ : {r : int | r = 10 || r = 20}) = found in
    ()

  let (constructor_laws @ total) (key @ total) =
    let empty = M.Refined.empty () in
    let singleton = M.Refined.singleton key 10 in
    let added = M.Refined.add key 20 singleton in
    let wrapped = {map = added} in
    let removed = M.Refined.remove key added in
    let found = M.Refined.find added key in
    let result = () in
    let proof :
        {u : unit |
          M.mem key empty = false
          && M.mem key singleton
          && M.mem key added
          && M.mem key wrapped.map
          && found = 20
          && M.mem key removed = false} =
      result
    in
    let _ = proof in
    ()

  let ordinary_find_assumes_normal_return map key =
    let found = M.find key map in
    let present = M.mem key map in
    let proof : {b : bool | b} = present in
    let _ = proof in
    found

  let (multiple_value_sorts @ total) (key @ total) =
    let ints = M.Refined.singleton key 7 in
    let bools = M.Refined.singleton key true in
    let integer = M.Refined.find ints key in
    let boolean = M.Refined.find bools key in
    let result = () in
    let proof : {u : unit | integer = 7 && boolean} = result in
    let _ = proof in
    ()

  let (cross_sort_comparator_class @ total) :
      (key : Key.t) ->
      {other : Key.t | M.mem other (M.Refined.singleton key 0)} ->
      unit =
    fun key equivalent ->
    let bools = M.Refined.singleton key true in
    let equivalent = equivalent in
    let present = M.mem equivalent bools in
    let found = M.Refined.find bools equivalent in
    let result = () in
    let proof : {u : unit | present && found} = result in
    let _ = proof in
    ()

  let (local_aliases @ total) (key @ total) (input @ total) =
    let insert = M.Refined.add in
    let lookup = M.Refined.find in
    let contains = M.mem in
    let map = insert key 31 input in
    let found : int = lookup map key in
    let result = () in
    let proof : {u : unit | contains key map && found = 31} = result in
    let _ = proof in
    ()

  let (module_aliases @ total) (key @ total) =
    let map = Alias.Refined.singleton key false in
    let found = Alias.Refined.find map key in
    let result = () in
    let proof : {u : unit | Alias.mem key map && found = false} =
      result
    in
    let _ = proof in
    ()

  let (ascribed_aliases @ total) (key @ total) =
    let map = Ascribed.Refined.singleton key 42 in
    let found = Ascribed.Refined.find map key in
    let result = () in
    let proof : {u : unit | Ascribed.mem key map && found = 42} =
      result
    in
    let _ = proof in
    ()

  let (more_labels @ total) (key @ total) =
    let empty = More_labeled.Refined.empty () in
    let map = More_labeled.Refined.singleton key 17 in
    let found = More_labeled.Refined.find map key in
    let result = () in
    let proof :
        {u : unit |
          More_labeled.mem key empty = false
          && More_labeled.mem key map
          && found = 17} =
      result
    in
    let _ = proof in
    ()

  module Verify (Order : Map.TotalOrderedType) = struct
    module M = Map.MakeTotal (Order)

    let lookup :
        ('a : value mod separable).
        (map : 'a M.t) ->
        {key : M.key | M.mem key map} ->
        'a @ total =
      fun map key -> M.Refined.find map key

    let (singleton_member @ total)
        (key @ total) (data @ total) =
      let present = M.mem key (M.Refined.singleton key data) in
      let proof : {b : bool | b} = present in
      let _ = proof in
      ()
  end

  module Verified_keys = Verify (Key)

  let ordinary_equality map =
    let result = map in
    let proof : {result : int Ordinary.t | result === map} = result in
    let _ = proof in
    result

  let (total_apis @ total) map =
    ignore (M.add { group = 0; id = 0 } 1 map);
    ignore (M.remove { group = 0; id = 0 } map);
    ignore (M.cardinal map);
    ignore (M.is_empty map);
    ignore (M.map (fun value -> value) map);
    ignore (M.filter (fun _ _ -> true) map)

  let (total_sequence_producers @ total) map =
    M.to_seq map, M.to_rev_seq map, M.to_seq_from { group = 0; id = 0 } map

  let mutable_holder = ref (M.empty : int M.t)
  let ordinary_read_write_call key = M.mem key !mutable_holder
end;;
[%%expect{|
module Demo : sig end
|}]

module Partial_order = struct
  type t = int
  let compare x y = if x < 0 then failwith "negative" else compare x y
end

module Rejected = Map.MakeTotal (Partial_order);;
[%%expect{|
module Partial_order : sig type t = int val compare : int -> int -> int end
Line 6, characters 18-47:
6 | module Rejected = Map.MakeTotal (Partial_order);;
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match:
       sig type t = int val compare : int -> int -> int end @ partial
     is not included in Map.TotalOrderedType @ partial
     The value "reflexive" is required but not provided
     File "map.mli", lines 74-75, characters 4-22: Expected declaration
     The value "antisymmetric" is required but not provided
     File "map.mli", lines 76-78, characters 4-66: Expected declaration
     The value "transitive" is required but not provided
     File "map.mli", lines 79-81, characters 4-45: Expected declaration
|}]

module Sequence_consumers : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let (rejected @ total) sequence = ignore (M.add_seq sequence M.empty)
end;;
[%%expect{|
Line 16, characters 44-53:
16 |   let (rejected @ total) sequence = ignore (M.add_seq sequence M.empty)
                                                 ^^^^^^^^^
Error: The value "M.add_seq" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 16, characters 25-71
         which is expected to be "total".
|}]

module Callback_relative_totality : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let partial _ _ = failwith "callback"
  let ordinary map = M.filter partial map
  let (rejected @ total) map = M.filter partial map
end;;
[%%expect{|
Line 18, characters 40-47:
18 |   let (rejected @ total) map = M.filter partial map
                                             ^^^^^^^
Error: The value "partial" is "partial"
         because it closes over the value "failwith" at line 16, characters 20-28
         which is "partial".
       However, the value "partial" highlighted is expected to be "total"
         because it is used inside the function at line 18, characters 25-51
         which is expected to be "total".
|}]

module Total_equality_rejected : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let rejected map : {result : int M.t | result === map} = map
end;;
[%%expect{|
Line 16, characters 41-55:
16 |   let rejected map : {result : int M.t | result === map} = map
                                              ^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 16, characters 59-62:
16 |   let rejected map : {result : int M.t | result === map} = map
                                                                ^^^
  Required by this refinement introduction
|}]

module Nested_total_equality_rejected : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  type box = Box of int M.t [@@inductive]
  let rejected box : {result : box | result === box} = box
end;;
[%%expect{|
Line 17, characters 37-51:
17 |   let rejected box : {result : box | result === box} = box
                                          ^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 17, characters 55-58:
17 |   let rejected box : {result : box | result === box} = box
                                                            ^^^
  Required by this refinement introduction
|}]

module Ordinary_constructors_unrecognized : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let rejected key =
    let map = M.add key 1 M.empty in
    let present = M.mem key map in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 19, characters 33-40:
19 |     let proof : {b : bool | b} = present in
                                      ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Ordinary_make_operations_unrecognized : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.Make (Order)
  let rejected key =
    let map = M.add key 1 M.empty in
    let present = M.mem key map in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 19, characters 33-40:
19 |     let proof : {b : bool | b} = present in
                                      ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Caught_find_has_no_normal_return_fact : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let rejected map key =
    ignore (try M.find key map with Not_found -> 0);
    let present = M.mem key map in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 19, characters 33-40:
19 |     let proof : {b : bool | b} = present in
                                      ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Overwrite_old_value_rejected : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let rejected key =
    let map = M.Refined.add key 2 (M.Refined.singleton key 1) in
    let found = M.Refined.find map key in
    let proof : {n : int | n = 1} = found in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 19, characters 36-41:
19 |     let proof : {n : int | n = 1} = found in
                                         ^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Different_key_preservation_needs_a_distinct_class : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let rejected key other =
    let map = M.Refined.add key 2 (M.Refined.singleton other 1) in
    let found = M.Refined.find map other in
    let proof : {n : int | n = 1} = found in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 19, characters 36-41:
19 |     let proof : {n : int | n = 1} = found in
                                         ^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Removed_lookup_rejected : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let rejected key =
    let map = M.Refined.remove key (M.Refined.singleton key 1) in
    M.Refined.find map key
end;;
[%%expect{|
Line 18, characters 23-26:
18 |     M.Refined.find map key
                            ^^^
Error: Refinement could not be proved (counterexample)
|}]

module Cross_sort_contents_do_not_leak : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let rejected key =
    let ints = M.Refined.singleton key 1 in
    let bools = M.Refined.singleton key false in
    let _integer = M.Refined.find ints key in
    let found = M.Refined.find bools key in
    let proof : {b : bool | b} = found in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 21, characters 33-38:
21 |     let proof : {b : bool | b} = found in
                                      ^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Separate_functor_classes : sig end = struct
  module First_order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module Second_order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module First = Map.MakeTotal (First_order)
  module Second = Map.MakeTotal (Second_order)
  let rejected key other =
    let first = First.mem other (First.Refined.singleton key 0) in
    let fact : {b : bool | b} = assume_ first in
    let _ = fact in
    let second = Second.mem other (Second.Refined.singleton key false) in
    let proof : {b : bool | b} = second in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 35, characters 33-39:
35 |     let proof : {b : bool | b} = second in
                                      ^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Shadowed_refined_operation_unrecognized : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module Real = Map.MakeTotal (Order)
  module Forged = struct
    include Real
    module Refined : module type of Real.Refined = struct
      include Real.Refined
      external trust_add :
        ('a : value).
        (Real.key -> 'a -> 'a Real.t -> 'a Real.t) ->
        (Real.key @ total ->
         'a @ total ->
         'a Real.t @ total ->
         'a Real.t @ total) @ total = "%identity"
      let bad_add (_ : Real.key) _ map = map
      let add = trust_add bad_add
    end
  end
  let rejected key =
    let map = Forged.Refined.add key 1 Forged.empty in
    let present = Forged.mem key map in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 34, characters 33-40:
34 |     let proof : {b : bool | b} = present in
                                      ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Shadowed_mem_unrecognized : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module Real = Map.MakeTotal (Order)
  module Forged : sig
    type key = Real.key
    type 'a t = 'a Real.t
    val mem : key @ immutable -> 'a t @ immutable -> bool @@ total
    module Refined : sig
      val singleton :
        key @ total ->
        'a @ total ->
        'a t @ total @@ total
    end
  end = struct
    include Real
    external trust_mem :
      ('a : value).
      (Real.key -> 'a Real.t -> bool) ->
      (Real.key @ immutable -> 'a Real.t @ immutable -> bool) @ total =
      "%identity"
    let mem = trust_mem (fun _ _ -> false)
  end
  let rejected key =
    let map = Forged.Refined.singleton key 1 in
    let present = Forged.mem key map in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 38, characters 33-40:
38 |     let proof : {b : bool | b} = present in
                                      ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Shadowed_find_unrecognized : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module Real = Map.MakeTotal (Order)
  module Forged : sig
    type key = Real.key
    type 'a t = 'a Real.t
    val singleton : key -> 'a -> 'a t @@ total
    val mem : key @ immutable -> 'a t @ immutable -> bool @@ total
    val find : key -> 'a t -> 'a
  end = struct
    include Real
    let find _ map = snd (Real.choose map)
  end
  let rejected key other =
    let map = Forged.singleton other 1 in
    ignore (Forged.find key map);
    let present = Forged.mem key map in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 30, characters 33-40:
30 |     let proof : {b : bool | b} = present in
                                      ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Refined_key_constructor_preserves_access : sig end = struct
  module Order = struct
    type t = { mutable key : int }
    let[@def] compare (_x : t @ immutable) (_y : t @ immutable) = 0
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ (compare_def x x; ())
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ (compare_def x y; compare_def y x; ())
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ (compare_def x y; compare_def y z; compare_def x z; ())
  end
  module M = Map.MakeTotal (Order)
  let singleton = M.Refined.singleton
  let update (key @ total) =
    let stored, _ = M.choose (singleton key 1) in
    stored.key <- 1
end;;
[%%expect{|
module Refined_key_constructor_preserves_access : sig end
|}]

module Refined_value_constructor_preserves_access : sig end = struct
  type value = { mutable payload : int }
  module Order = struct
    type t = int
    external compare : t -> t -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let singleton = M.Refined.singleton
  let update (value @ total) =
    let value : value = value in
    let _, stored = M.choose (singleton 0 value) in
    stored.payload <- 1
end;;
[%%expect{|
module Refined_value_constructor_preserves_access : sig end
|}]

module Refined_find_preserves_access : sig end = struct
  type value = { mutable payload : int }
  module Order = struct
    type t = int
    external compare : t -> t -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let ordinary map key = (M.find key map).payload <- 1
  let update :
      (map : value M.t) ->
      {key : int | M.mem key map} ->
      unit =
    fun map key -> (M.Refined.find map key).payload <- 1
end;;
[%%expect{|
module Refined_find_preserves_access : sig end
|}]

module Refined_key_rejects_partial_closure : sig end = struct
  module Order = struct
    type t = unit -> unit
    let[@def] compare (_x : t @ immutable) (_y : t @ immutable) = 0
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ (compare_def x x; ())
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ (compare_def x y; compare_def y x; ())
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ (compare_def x y; compare_def y z; compare_def x z; ())
  end
  module M = Map.MakeTotal (Order)
  let partial_key () = failwith "partial"
  let ordinary = M.singleton partial_key 1
  let removed = M.Refined.remove partial_key M.empty
  let rejected = M.Refined.singleton partial_key 1
end;;
[%%expect{|
Line 19, characters 37-48:
19 |   let rejected = M.Refined.singleton partial_key 1
                                          ^^^^^^^^^^^
Error: This value is "partial"
         because it closes over the value "failwith" at line 16, characters 23-31
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

module Refined_value_rejects_partial_closure : sig end = struct
  module Order = struct
    type t = int
    external compare : t -> t -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let partial_value () = failwith "partial"
  let ordinary = M.singleton 0 partial_value
  let rejected = M.Refined.singleton 0 partial_value
end;;
[%%expect{|
Line 18, characters 39-52:
18 |   let rejected = M.Refined.singleton 0 partial_value
                                            ^^^^^^^^^^^^^
Error: This value is "partial"
         because it closes over the value "failwith" at line 16, characters 25-33
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

module Refined_find_rejects_partial_container : sig end = struct
  module Order = struct
    type t = int
    external compare : t -> t -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module M = Map.MakeTotal (Order)
  let partial_value () = failwith "partial"
  let rejected () =
    let map = M.singleton 0 partial_value in
    let _found = M.find 0 map in
    let key = 0 in
    let member : {key : int | M.mem key map} = key in
    let _found = M.Refined.find map member in
    ()
end;;
[%%expect{|
Line 21, characters 40-43:
21 |     let member : {key : int | M.mem key map} = key in
                                             ^^^
Error: The value "map" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 21, characters 30-43).
|}]

module Refined_accepts_total_closures : sig end = struct
  module Order = struct
    type t = unit -> unit
    let[@def] compare (_x : t @ immutable) (_y : t @ immutable) = 0
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ (compare_def x x; ())
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ (compare_def x y; compare_def y x; ())
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ (compare_def x y; compare_def y z; compare_def x z; ())
  end
  module M = Map.MakeTotal (Order)
  let total_key () = ()
  let total_value () = ()
  let accepted = M.Refined.singleton total_key total_value
end;;
[%%expect{|
module Refined_accepts_total_closures : sig end
|}]

module Immutable_collections : sig end = struct
  module Key = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module S = Map.MakeTotal (Key)
  module L = MoreLabels.Map.MakeTotal (Key)
  module A : Map.TotalS with type key = int = S
  module Unlabeled : sig type t : immutable_data end = struct type t = int S.t end
  module Labeled : sig type t : immutable_data end = struct type t = int L.t end
  module Ascribed : sig type t : immutable_data end = struct type t = int A.t end
end;;
[%%expect{|
module Immutable_collections : sig end
|}]

module Mutable_collection : sig end = struct
  module Key = struct
    type t = int
    external compare : t -> t -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end
  module S = Map.MakeTotal (Key)
  type t : immutable_data = int ref S.t
end;;
[%%expect{|
Line 16, characters 2-39:
16 |   type t : immutable_data = int ref S.t
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int ref S.t" is mutable_data.
       But the kind of type "int ref S.t" must be a subkind of immutable_data
         because of the definition of t at line 16, characters 2-39.
|}]

module Polymorphic_sparse_pair : sig end = struct
  module Index = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
    let (reflexive @ total) (x : t) :
        {u : unit | compare x x = 0} @ ghost = ghost_ ()
    let (antisymmetric @ total) (x : t) (y : t) :
        {u : unit | (compare x y < 0) = (compare y x > 0)
          && (compare x y = 0) = (compare y x = 0)} @ ghost =
      ghost_ ()
    let (transitive @ total) (x : t) (y : t) (z : t) :
        {u : unit | not (compare x y <= 0 && compare y z <= 0)
          || compare x z <= 0} @ ghost = ghost_ ()
  end

  module Updates = Map.MakeTotal (Index)

  let (clear_reads_base @ total) :
      ('a : value mod separable).
      (base : 'a iarray) ->
      'a Updates.t @ total ->
      (index : {index : int |
        0 <= index && index < Iarray.length base}) ->
      {results : 'a * 'a |
        match results with result, base_result -> result === base_result} =
    fun base updates index ->
    let raw_index = index in
    let updates = Updates.Refined.remove raw_index updates in
    let base_result = Iarray.Refined.get base index in
    let result =
      if Updates.mem raw_index updates then
        let member : {key : int | Updates.mem key updates} = raw_index in
        Updates.Refined.find updates member
      else Iarray.Refined.get base index
    in
    let results = result, base_result in
    results

  let () =
    let base = [: 10 :] in
    let updates = Updates.Refined.singleton 0 99 in
    let zero = 0 in
    let index : {i : int | 0 <= i && i < Iarray.length base} = zero in
    let pair = clear_reads_base base updates index in
    let left, right = pair in
    assert (left = 10 && right = 10);
    let base = [: true :] in
    let updates = Updates.Refined.singleton 0 false in
    let index : {i : int | 0 <= i && i < Iarray.length base} = zero in
    let pair = clear_reads_base base updates index in
    let left, right = pair in
    assert (left && right)
end;;
[%%expect{|
module Polymorphic_sparse_pair : sig end
|}]
