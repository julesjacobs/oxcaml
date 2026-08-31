(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }{
   flags = "-extension refinement_types -principal";
   { expect; }
   { expect.opt; }
 }
*)

module Demo : sig end = struct
  module Key = struct
    type t = { group : int; id : int }
    external compare : t -> t -> int @@ total = "%compare"
  end

  module M = Map.MakeTotal (Key)
  module More_labeled = MoreLabels.Map.MakeTotal (Key)
  module Ordinary = Map.Make (Key)
  module Alias = M
  module Ascribed : module type of M = M

  type 'a wrapped = {map : 'a M.t}

  let to_more_labels : 'a. 'a M.t -> 'a More_labeled.t = fun map -> map
  let from_more_labels : 'a. 'a More_labeled.t -> 'a M.t = fun map -> map

  let (constructor_laws @ total) (key @ total) =
    let empty = M.Refined.empty () in
    let singleton = M.Refined.singleton key 10 in
    let added = M.Refined.add key 20 singleton in
    let wrapped = {map = added} in
    let removed = M.Refined.remove key added in
    let found = M.Refined.find added (refine_ key) in
    let result = () in
    let proof :
        {u : unit |
          M.mem key empty = false
          && M.mem key singleton
          && M.mem key added
          && M.mem key wrapped.map
          && found = 20
          && M.mem key removed = false} =
      refine_ result
    in
    let refine_ proof = proof in
    ()

  let ordinary_find_assumes_normal_return map key =
    let found = M.find key map in
    let present = M.mem key map in
    let proof : {b : bool | b} = refine_ present in
    let refine_ proof = proof in
    found

  let (multiple_value_sorts @ total) (key @ total) =
    let ints = M.Refined.singleton key 7 in
    let bools = M.Refined.singleton key true in
    let integer = M.Refined.find ints (refine_ key) in
    let boolean = M.Refined.find bools (refine_ key) in
    let result = () in
    let proof : {u : unit | integer = 7 && boolean} = refine_ result in
    let refine_ proof = proof in
    ()

  let (cross_sort_comparator_class @ total) :
      (key : Key.t) ->
      {other : Key.t | M.mem other (M.Refined.singleton key 0)} ->
      unit =
    fun key equivalent ->
    let bools = M.Refined.singleton key true in
    let refine_ equivalent = equivalent in
    let present = M.mem equivalent bools in
    let found = M.Refined.find bools (refine_ equivalent) in
    let result = () in
    let proof : {u : unit | present && found} = refine_ result in
    let refine_ proof = proof in
    ()

  let (local_aliases @ total) (key @ total) (input @ total) =
    let insert = M.Refined.add in
    let lookup = M.Refined.find in
    let contains = M.mem in
    let map = insert key 31 input in
    let found : int = lookup map (refine_ key) in
    let result = () in
    let proof : {u : unit | contains key map && found = 31} = refine_ result in
    let refine_ proof = proof in
    ()

  let (module_aliases @ total) (key @ total) =
    let map = Alias.Refined.singleton key false in
    let found = Alias.Refined.find map (refine_ key) in
    let result = () in
    let proof : {u : unit | Alias.mem key map && found = false} =
      refine_ result
    in
    let refine_ proof = proof in
    ()

  let (ascribed_aliases @ total) (key @ total) =
    let map = Ascribed.Refined.singleton key 42 in
    let found = Ascribed.Refined.find map (refine_ key) in
    let result = () in
    let proof : {u : unit | Ascribed.mem key map && found = 42} =
      refine_ result
    in
    let refine_ proof = proof in
    ()

  let (more_labels @ total) (key @ total) =
    let empty = More_labeled.Refined.empty () in
    let map = More_labeled.Refined.singleton key 17 in
    let found = More_labeled.Refined.find map (refine_ key) in
    let result = () in
    let proof :
        {u : unit |
          More_labeled.mem key empty = false
          && More_labeled.mem key map
          && found = 17} =
      refine_ result
    in
    let refine_ proof = proof in
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
      let proof : {b : bool | b} = refine_ present in
      let refine_ proof = proof in
      ()
  end

  module Verified_keys = Verify (Key)

  let ordinary_equality map =
    let result = map in
    let proof : {result : int Ordinary.t | result === map} = refine_ result in
    let refine_ proof = proof in
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
     Values do not match:
       val compare : int -> int -> int (* in a structure at partial *)
     is not included in
       val compare : t -> t -> int @@ total (* in a structure at partial *)
     The first is "partial"
       because it closes over the value "failwith" at line 3, characters 34-42
       which is "partial".
     However, the second is "total".
     File "map.mli", line 73, characters 4-40: Expected declaration
|}]

module Sequence_consumers : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let (rejected @ total) sequence = ignore (M.add_seq sequence M.empty)
end;;
[%%expect{|
Line 7, characters 44-53:
7 |   let (rejected @ total) sequence = ignore (M.add_seq sequence M.empty)
                                                ^^^^^^^^^
Error: The value "M.add_seq" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 7, characters 25-71
         which is expected to be "total".
|}]

module Callback_relative_totality : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let partial _ _ = failwith "callback"
  let ordinary map = M.filter partial map
  let (rejected @ total) map = M.filter partial map
end;;
[%%expect{|
Line 9, characters 40-47:
9 |   let (rejected @ total) map = M.filter partial map
                                            ^^^^^^^
Error: The value "partial" is "partial"
         because it closes over the value "failwith" at line 7, characters 20-28
         which is "partial".
       However, the value "partial" highlighted is expected to be "total"
         because it is used inside the function at line 9, characters 25-51
         which is expected to be "total".
|}]

module Total_equality_rejected : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let rejected map : {result : int M.t | result === map} = refine_ map
end;;
[%%expect{|
Line 7, characters 41-55:
7 |   let rejected map : {result : int M.t | result === map} = refine_ map
                                             ^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 7, characters 59-70:
7 |   let rejected map : {result : int M.t | result === map} = refine_ map
                                                               ^^^^^^^^^^^
  Required by this refinement introduction
|}]

module Nested_total_equality_rejected : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  type box = Box of int M.t [@@inductive]
  let rejected box : {result : box | result === box} = refine_ box
end;;
[%%expect{|
Line 8, characters 37-51:
8 |   let rejected box : {result : box | result === box} = refine_ box
                                         ^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 8, characters 55-66:
8 |   let rejected box : {result : box | result === box} = refine_ box
                                                           ^^^^^^^^^^^
  Required by this refinement introduction
|}]

module Ordinary_constructors_unrecognized : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let rejected key =
    let map = M.add key 1 M.empty in
    let present = M.mem key map in
    let proof : {b : bool | b} = refine_ present in
    let refine_ proof = proof in
    ()
end;;
[%%expect{|
Line 10, characters 33-48:
10 |     let proof : {b : bool | b} = refine_ present in
                                      ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Ordinary_make_operations_unrecognized : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.Make (Order)
  let rejected key =
    let map = M.add key 1 M.empty in
    let present = M.mem key map in
    let proof : {b : bool | b} = refine_ present in
    let refine_ proof = proof in
    ()
end;;
[%%expect{|
Line 10, characters 33-48:
10 |     let proof : {b : bool | b} = refine_ present in
                                      ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Caught_find_has_no_normal_return_fact : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let rejected map key =
    ignore (try M.find key map with Not_found -> 0);
    let present = M.mem key map in
    let proof : {b : bool | b} = refine_ present in
    let refine_ proof = proof in
    ()
end;;
[%%expect{|
Line 10, characters 33-48:
10 |     let proof : {b : bool | b} = refine_ present in
                                      ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Overwrite_old_value_rejected : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let rejected key =
    let map = M.Refined.add key 2 (M.Refined.singleton key 1) in
    let found = M.Refined.find map (refine_ key) in
    let proof : {n : int | n = 1} = refine_ found in
    let refine_ proof = proof in
    ()
end;;
[%%expect{|
Line 10, characters 36-49:
10 |     let proof : {n : int | n = 1} = refine_ found in
                                         ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Different_key_preservation_needs_a_distinct_class : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let rejected key other =
    let map = M.Refined.add key 2 (M.Refined.singleton other 1) in
    let found = M.Refined.find map (refine_ other) in
    let proof : {n : int | n = 1} = refine_ found in
    let refine_ proof = proof in
    ()
end;;
[%%expect{|
Line 10, characters 36-49:
10 |     let proof : {n : int | n = 1} = refine_ found in
                                         ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Removed_lookup_rejected : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let rejected key =
    let map = M.Refined.remove key (M.Refined.singleton key 1) in
    M.Refined.find map (refine_ key)
end;;
[%%expect{|
Line 9, characters 23-36:
9 |     M.Refined.find map (refine_ key)
                           ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Cross_sort_contents_do_not_leak : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let rejected key =
    let ints = M.Refined.singleton key 1 in
    let bools = M.Refined.singleton key false in
    let _integer = M.Refined.find ints (refine_ key) in
    let found = M.Refined.find bools (refine_ key) in
    let proof : {b : bool | b} = refine_ found in
    let refine_ proof = proof in
    ()
end;;
[%%expect{|
Line 12, characters 33-46:
12 |     let proof : {b : bool | b} = refine_ found in
                                      ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Separate_functor_classes : sig end = struct
  module First_order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module Second_order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module First = Map.MakeTotal (First_order)
  module Second = Map.MakeTotal (Second_order)
  let rejected key other =
    let first = First.mem other (First.Refined.singleton key 0) in
    let fact : {b : bool | b} = assume_ first in
    let refine_ fact = fact in
    let second = Second.mem other (Second.Refined.singleton key false) in
    let proof : {b : bool | b} = refine_ second in
    let refine_ proof = proof in
    ()
end;;
[%%expect{|
Line 17, characters 33-47:
17 |     let proof : {b : bool | b} = refine_ second in
                                      ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Shadowed_refined_operation_unrecognized : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
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
    let proof : {b : bool | b} = refine_ present in
    let refine_ proof = proof in
    ()
end;;
[%%expect{|
Line 25, characters 33-48:
25 |     let proof : {b : bool | b} = refine_ present in
                                      ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Shadowed_mem_unrecognized : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
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
    let proof : {b : bool | b} = refine_ present in
    let refine_ proof = proof in
    ()
end;;
[%%expect{|
Line 29, characters 33-48:
29 |     let proof : {b : bool | b} = refine_ present in
                                      ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Shadowed_find_unrecognized : sig end = struct
  module Order = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
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
    let proof : {b : bool | b} = refine_ present in
    let refine_ proof = proof in
    ()
end;;
[%%expect{|
Line 21, characters 33-48:
21 |     let proof : {b : bool | b} = refine_ present in
                                      ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Refined_key_constructor_preserves_access : sig end = struct
  module Order = struct
    type t = { mutable key : int }
    let compare _ _ = 0
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
    let compare _ _ = 0
  end
  module M = Map.MakeTotal (Order)
  let partial_key () = failwith "partial"
  let ordinary = M.singleton partial_key 1
  let removed = M.Refined.remove partial_key M.empty
  let rejected = M.Refined.singleton partial_key 1
end;;
[%%expect{|
Line 10, characters 37-48:
10 |   let rejected = M.Refined.singleton partial_key 1
                                          ^^^^^^^^^^^
Error: This value is "partial"
         because it closes over the value "failwith" at line 7, characters 23-31
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

module Refined_value_rejects_partial_closure : sig end = struct
  module Order = struct
    type t = int
    external compare : t -> t -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let partial_value () = failwith "partial"
  let ordinary = M.singleton 0 partial_value
  let rejected = M.Refined.singleton 0 partial_value
end;;
[%%expect{|
Line 9, characters 39-52:
9 |   let rejected = M.Refined.singleton 0 partial_value
                                           ^^^^^^^^^^^^^
Error: This value is "partial"
         because it closes over the value "failwith" at line 7, characters 25-33
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

module Refined_find_rejects_partial_container : sig end = struct
  module Order = struct
    type t = int
    external compare : t -> t -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Order)
  let partial_value () = failwith "partial"
  let rejected () =
    let map = M.singleton 0 partial_value in
    let _found = M.find 0 map in
    let key = 0 in
    let member : {key : int | M.mem key map} = refine_ key in
    let _found = M.Refined.find map member in
    ()
end;;
[%%expect{|
Line 12, characters 40-43:
12 |     let member : {key : int | M.mem key map} = refine_ key in
                                             ^^^
Error: The value "map" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 12, characters 30-43).
|}]

module Refined_accepts_total_closures : sig end = struct
  module Order = struct
    type t = unit -> unit
    let compare _ _ = 0
  end
  module M = Map.MakeTotal (Order)
  let total_key () = ()
  let total_value () = ()
  let accepted = M.Refined.singleton total_key total_value
end;;
[%%expect{|
module Refined_accepts_total_closures : sig end
|}]
