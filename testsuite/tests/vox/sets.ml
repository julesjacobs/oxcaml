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
  module Int_order = struct
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

  module Int_set = Set.MakeTotal (Int_order)
  module More_labeled_set = MoreLabels.Set.MakeTotal (Int_order)
  module Ordinary_set = Set.Make (Int_order)
  module Int_set_alias = Int_set
  module Int_set_ascribed : module type of Int_set = Int_set

  let to_more_labels (set : Int_set.t) : More_labeled_set.t = set
  let from_more_labels (set : More_labeled_set.t) : Int_set.t = set

  let (joined_membership @ total) b x (set @ total) =
    let singleton = Int_set.Refined.singleton x in
    let added = Int_set.Refined.add x set in
    let joined = if b then singleton else added in
    let present = Int_set.mem x joined in
    let (_ : {r : bool | r}) = present in
    ()

  let (membership_laws @ total) x (set @ total) =
    let singleton = Int_set.Refined.singleton x in
    let added = Int_set.Refined.add x set in
    let union = Int_set.Refined.union set singleton in
    let inter = Int_set.Refined.inter added singleton in
    let removed = Int_set.Refined.remove x set in
    let diff = Int_set.Refined.diff set set in
    let result = () in
    let proof :
        {u : unit |
          Int_set.mem x Int_set.empty = false
          && Int_set.mem x singleton
          && Int_set.mem x added
          && Int_set.mem x union
          && Int_set.mem x inter
          && Int_set.mem x removed = false
          && Int_set.mem x diff = false} =
      result
    in
    let _ = proof in
    ()

  let (aliased_operations @ total) x (set @ total) =
    let insert = Int_set.Refined.add in
    let contains = Int_set.mem in
    let present = contains x (insert x set) in
    let unit = () in
    let proof : {u : unit | present} = unit in
    let _ = proof in
    ()

  let (module_aliased_operations @ total) x (set @ total) =
    let present =
      Int_set_alias.mem x
        (Int_set_alias.Refined.add x set)
    in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()

  let (ascribed_operations @ total) x (set @ total) =
    let present =
      Int_set_ascribed.mem x
        (Int_set_ascribed.Refined.add x set)
    in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()

  let (more_labels @ total) x =
    let set = More_labeled_set.Refined.singleton x in
    let present = More_labeled_set.mem x set in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()

  let (refined_find @ total) :
      (set : Int_set.t) ->
      {x : int | Int_set.mem x set} ->
      unit =
    fun set member ->
    let find :
        (set : Int_set.t) ->
        {x : int | Int_set.mem x set} ->
        int @ total =
      Int_set.Refined.find
    in
    let representative = find set member in
    let member = member in
    let singleton = Int_set.Refined.singleton member in
    let unit = () in
    let proof :
        {u : unit |
          Int_set.mem representative set
          && Int_set.mem representative singleton} =
      unit
    in
    let _ = proof in
    ()

  module Verify_singletons (Order : Set.TotalOrderedType) = struct
    module S = Set.MakeTotal (Order)

    let (member @ total) (element @ total) =
      let present = S.mem element (S.Refined.singleton element) in
      let proof : {b : bool | b} = present in
      let _ = proof in
      ()
  end

  module Bool_order = struct
    type t = bool
    external compare : bool -> bool -> int @@ total = "%compare"
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

  module Int_singletons = Verify_singletons (Int_order)
  module Bool_singletons = Verify_singletons (Bool_order)

  let ordinary_equality set =
    let result = set in
    let proof : {result : Ordinary_set.t | result === set} = result in
    let _ = proof in
    result

  let ordinary_find set x =
    ignore (Int_set.find x set);
    let present = Int_set.mem x set in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()

  let (total_apis @ total) set =
    ignore (Int_set.add 0 set);
    ignore (Int_set.union set set);
    ignore (Int_set.cardinal set);
    ignore (Int_set.is_empty set);
    ignore (Int_set.map (fun x -> x) set);
    ignore (Int_set.filter (fun _ -> true) set)

  let (total_sequence_producers @ total) set =
    Int_set.to_seq set,
    Int_set.to_rev_seq set,
    Int_set.to_seq_from 0 set

  let mutable_holder = ref Int_set.empty
  let ordinary_read_write_call x = Int_set.mem x !mutable_holder
end;;
[%%expect{|
module Demo : sig end
|}]

module Partial_order = struct
  type t = int
  let compare x y = if x < 0 then failwith "negative" else compare x y
end

module Rejected = Set.MakeTotal (Partial_order);;
[%%expect{|
module Partial_order : sig type t = int val compare : int -> int -> int end
Line 6, characters 18-47:
6 | module Rejected = Set.MakeTotal (Partial_order);;
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match:
       sig type t = int val compare : int -> int -> int end @ partial
     is not included in Set.TotalOrderedType @ partial
     The value "reflexive" is required but not provided
     File "set.mli", lines 75-76, characters 4-22: Expected declaration
     The value "antisymmetric" is required but not provided
     File "set.mli", lines 77-79, characters 4-66: Expected declaration
     The value "transitive" is required but not provided
     File "set.mli", lines 80-82, characters 4-45: Expected declaration
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
  module S = Set.MakeTotal (Order)
  let (rejected @ total) sequence = ignore (S.add_seq sequence S.empty)
end;;
[%%expect{|
Line 16, characters 44-53:
16 |   let (rejected @ total) sequence = ignore (S.add_seq sequence S.empty)
                                                 ^^^^^^^^^
Error: The value "S.add_seq" is "partial"
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
  module S = Set.MakeTotal (Order)
  let partial _ = failwith "callback"
  let ordinary set = S.filter partial set
  let (rejected @ total) set = S.filter partial set
end;;
[%%expect{|
Line 18, characters 40-47:
18 |   let (rejected @ total) set = S.filter partial set
                                             ^^^^^^^
Error: The value "partial" is "partial"
         because it closes over the value "failwith" at line 16, characters 18-26
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
  module S = Set.MakeTotal (Order)
  let rejected set : {result : S.t | result === set} = set
end;;
[%%expect{|
Line 16, characters 37-51:
16 |   let rejected set : {result : S.t | result === set} = set
                                          ^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 16, characters 55-58:
16 |   let rejected set : {result : S.t | result === set} = set
                                                            ^^^
  Required by this refinement introduction
|}]

module Ordinary_operations_unrecognized : sig end = struct
  module Order = struct type t = int let compare = compare end
  module S = Set.Make (Order)
  let rejected x =
    let set = S.singleton x in
    let present = S.mem x set in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 7, characters 33-40:
7 |     let proof : {b : bool | b} = present in
                                     ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Caught_find_does_not_assume_normal_return : sig end = struct
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
  module S = Set.MakeTotal (Order)
  let rejected set x =
    ignore (try S.find x set with Not_found -> x);
    let present = S.mem x set in
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

module Nested_set_equality_rejected : sig end = struct
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
  module S = Set.MakeTotal (Order)
  type box = Box of S.t [@@inductive]
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

module Lookalike_operations_unrecognized : sig end = struct
  module S = struct
    type t = int
    let (singleton @ total) x = x
    let (mem @ total) _ _ = false
  end
  let rejected x =
    let set = S.singleton x in
    let present = S.mem x set in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 10, characters 33-40:
10 |     let proof : {b : bool | b} = present in
                                      ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Shadowed_operation_unrecognized : sig end = struct
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
  module Real = Set.MakeTotal (Order)
  module Forged = struct
    include Real
    module Refined : module type of Real.Refined = struct
      include Real.Refined
      external trust_add :
        (Real.elt -> Real.t -> Real.t) ->
        (Real.elt @ total ->
         Real.t @ total ->
         Real.t @ total) @ total = "%identity"
      let bad_add (_ : Real.elt) (set : Real.t) = set
      let add = trust_add bad_add
    end
  end
  let rejected x =
    let present = Forged.mem x (Forged.Refined.add x Forged.empty) in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 31, characters 33-40:
31 |     let proof : {b : bool | b} = present in
                                      ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Ordinary_total_constructors_unrecognized : sig end = struct
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
  module S = Set.MakeTotal (Order)
  let rejected x =
    let present = S.mem x (S.add x S.empty) in
    let proof : {b : bool | b} = present in
    let _ = proof in
    ()
end;;
[%%expect{|
Line 18, characters 33-40:
18 |     let proof : {b : bool | b} = present in
                                      ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Separate_classes : sig end = struct
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
  module First = Set.MakeTotal (First_order)
  module Second = Set.MakeTotal (Second_order)

  let rejected () : {u : unit | Second.mem 2 (Second.Refined.singleton 1)} =
    let first = First.mem 2 (First.Refined.singleton 1) in
    let fact : {b : bool | b} = assume_ first in
    let _ = fact in
    ()
end;;
[%%expect{|
Line 35, characters 4-6:
35 |     ()
         ^^
Error: Refinement could not be proved (counterexample)
|}]

module Refined_constructors_preserve_access : sig end = struct
  module Order = struct
    type t = int ref
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
  module S = Set.MakeTotal (Order)
  let ordinary x = S.choose (S.singleton x)
  let ordinary_mem x = S.mem x (S.singleton x)
  let update (x @ total) =
    let stored = S.choose (S.Refined.singleton x) in
    stored := 1
end;;
[%%expect{|
module Refined_constructors_preserve_access : sig end
|}]

module Refined_find_preserves_access : sig end = struct
  module Order = struct
    type t = { mutable payload : int }
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
  module S = Set.MakeTotal (Order)
  let ordinary set element = (S.find element set).payload <- 1
  let update :
      (set : S.t) ->
      {element : Order.t | S.mem element set} ->
      unit =
    fun set element -> (S.Refined.find set element).payload <- 1
end;;
[%%expect{|
module Refined_find_preserves_access : sig end
|}]

module Refined_constructor_rejects_partial_closure : sig end = struct
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
  module S = Set.MakeTotal (Order)
  let partial_element () = failwith "partial"
  let ordinary = S.singleton partial_element
  let removed = S.Refined.remove partial_element S.empty
  let rejected = S.Refined.singleton partial_element
end;;
[%%expect{|
Line 19, characters 37-52:
19 |   let rejected = S.Refined.singleton partial_element
                                          ^^^^^^^^^^^^^^^
Error: This value is "partial"
         because it closes over the value "failwith" at line 16, characters 27-35
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

module Refined_find_rejects_partial_container : sig end = struct
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
  module S = Set.MakeTotal (Order)
  let partial_element () = failwith "partial"
  let total_query () = ()
  let rejected () =
    let set = S.singleton partial_element in
    let _found = S.find total_query set in
    let member : {x : Order.t | S.mem x set} = total_query in
    let _found = S.Refined.find set member in
    ()
end;;
[%%expect{|
Line 21, characters 40-43:
21 |     let member : {x : Order.t | S.mem x set} = total_query in
                                             ^^^
Error: The value "set" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 21, characters 32-43).
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
  module S = Set.MakeTotal (Order)
  let total_element () = ()
  let accepted = S.Refined.singleton total_element
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
  module S = Set.MakeTotal (Key)
  module L = MoreLabels.Set.MakeTotal (Key)
  module A : Set.TotalS with type elt = int = S
  module Unlabeled : sig type t : immutable_data end = struct type t = S.t end
  module Labeled : sig type t : immutable_data end = struct type t = L.t end
  module Ascribed : sig type t : immutable_data end = struct type t = A.t end
end;;
[%%expect{|
module Immutable_collections : sig end
|}]

module Mutable_collection : sig end = struct
  module Key = struct
    type t = { mutable rank : int }
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
  module S = Set.MakeTotal (Key)
  type t : immutable_data = S.t
end;;
[%%expect{|
Line 16, characters 2-31:
16 |   type t : immutable_data = S.t
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "S.t" is mutable_data.
       But the kind of type "S.t" must be a subkind of immutable_data
         because of the definition of t at line 16, characters 2-31.
|}]
