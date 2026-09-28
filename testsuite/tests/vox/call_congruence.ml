(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
*)

(* The verifier encodes a call to a total, stateless function as a function
   of its arguments, so equal calls give equal results. Totality is relative
   to the arguments: [apply f x = f x] is total and stateless, but applied to
   a stateful [tick] it returns a different number each time. A call is
   therefore stable only when every argument is total and stateless as seen
   at the call; otherwise its result is fresh. *)

module C = struct
  let (apply @ total) (f : int -> int) (x : int) : int = f x
  let (inc @ total) (x : int) : int = x + 1
  let counter = ref 0
  let tick (_ : int) : int = incr counter; !counter
  type r = { f : int -> int }
  let (run @ total) (r : r) (x : int) : int = r.f x
end
open C;;
[%%expect{|
module C :
  sig
    val apply : (int -> int) -> int -> int
    val inc : int -> int
    val counter : int ref
    val tick : int -> int
    type r = { f : int -> int; }
    val run : r -> int -> int
  end
|}]

(* Two calls of [apply tick 0] return 1 and 2. *)
let stateful_callback () : {b : bool | b} =
  let a = apply tick 0 in
  let b = apply tick 0 in
  a = b;;
[%%expect{|
Line 4, characters 2-7:
4 |   a = b;;
      ^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 39-40:
1 | let stateful_callback () : {b : bool | b} =
                                           ^
  The refinement is stated here.
|}]

(* The standard library's total [List.map] is no different. *)
let stateful_map () : {b : bool | b} =
  let a = List.map tick [0] in
  let b = List.map tick [0] in
  match a, b with [x], [y] -> x = y | _ -> true;;
[%%expect{|
Line 4, characters 30-35:
4 |   match a, b with [x], [y] -> x = y | _ -> true;;
                                  ^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 34-35:
1 | let stateful_map () : {b : bool | b} =
                                      ^
  The refinement is stated here.
|}]

(* The same mismatch would make a reachable branch look dead. *)
let reachable () : int =
  let a = apply tick 0 in
  let b = apply tick 0 in
  if a = b then 0 else unreachable_ ();;
[%%expect{|
Line 4, characters 23-38:
4 |   if a = b then 0 else unreachable_ ();;
                           ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

(* A partial application captures its arguments. *)
let stateful_partial () : {b : bool | b} =
  let g = apply tick in
  let a = g 0 in
  let b = g 0 in
  a = b;;
[%%expect{|
Line 5, characters 2-7:
5 |   a = b;;
      ^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 38-39:
1 | let stateful_partial () : {b : bool | b} =
                                          ^
  The refinement is stated here.
|}]

(* A local closure, and a parameter that the contract does not mention, are
   partial and stateful. *)
let stateful_local () : {b : bool | b} =
  let local = fun (_ : int) -> incr counter; !counter in
  let a = apply local 0 in
  let b = apply local 0 in
  a = b;;
[%%expect{|
Line 5, characters 2-7:
5 |   a = b;;
      ^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 36-37:
1 | let stateful_local () : {b : bool | b} =
                                        ^
  The refinement is stated here.
|}]

let stateful_parameter (h : int -> int) : {b : bool | b} =
  let a = apply h 0 in
  let b = apply h 0 in
  a = b;;
[%%expect{|
Line 4, characters 2-7:
4 |   a = b;;
      ^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 54-55:
1 | let stateful_parameter (h : int -> int) : {b : bool | b} =
                                                          ^
  The refinement is stated here.
|}]

(* Data can hold a stateful closure. *)
let closure_in_data (l : r list) : {b : bool | b} =
  match l with
  | [r] ->
    let a = run r 0 in
    let b = run r 0 in
    a = b
  | _ -> true;;
[%%expect{|
Line 6, characters 4-9:
6 |     a = b
        ^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 47-48:
1 | let closure_in_data (l : r list) : {b : bool | b} =
                                                   ^
  The refinement is stated here.
|}]

(* So can an abstract type. *)
module M : sig
  type t
  val make : unit -> t
  val run : t -> int -> int @@ total
end = struct
  type t = int -> int
  let counter = ref 0
  let make () = fun (_ : int) -> incr counter; !counter
  let (run @ total) (f : t) (x : int) : int = f x
end

let abstract_closure () : {b : bool | b} =
  let t = M.make () in
  let a = M.run t 0 in
  let b = M.run t 0 in
  a = b;;
[%%expect{|
module M :
  sig type t val make : unit -> t val run : t -> int -> int @@ total end
Line 16, characters 2-7:
16 |   a = b;;
       ^^^^^
Error: Refinement could not be proved (counterexample)
Line 12, characters 38-39:
12 | let abstract_closure () : {b : bool | b} =
                                           ^
  The refinement is stated here.
|}]

(* Labelled arguments. *)
let (apply_labelled @ total) ~(f : int -> int) (x : int) : int = f x

let stateful_labelled () : {b : bool | b} =
  let a = apply_labelled ~f:tick 0 in
  let b = apply_labelled ~f:tick 0 in
  a = b;;
[%%expect{|
val apply_labelled : f:(int -> int) -> int -> int = <fun>
Line 6, characters 2-7:
6 |   a = b;;
      ^^^^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 39-40:
3 | let stateful_labelled () : {b : bool | b} =
                                           ^
  The refinement is stated here.
|}]

(* A transparent definition is unfolded only at a stable call: its lemma's
   parameters are total and stateless, and [tick] is neither. *)
module T = struct
  let[@def transparent] apply_t (f : int -> int) (x : int) = f x
end
open T;;
[%%expect{|
module T :
  sig
    val apply_t : (int -> int) -> int -> int
    val apply_t_def :
      (f : (int -> int)) -> (x : int) -> {u : unit | (apply_t f x) === (f x)}
  end
|}]

let unfolded_stateful () : {b : bool | b} =
  let a = apply_t tick 0 in
  let b = apply_t tick 0 in
  a = b;;
[%%expect{|
Line 4, characters 2-7:
4 |   a = b;;
      ^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 39-40:
1 | let unfolded_stateful () : {b : bool | b} =
                                           ^
  The refinement is stated here.
|}]

(* Nor can the lemma be called at [tick]. *)
let lemma_stateful () = apply_t_def tick 0;;
[%%expect{|
Line 1, characters 36-40:
1 | let lemma_stateful () = apply_t_def tick 0;;
                                        ^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* Controls. A total, stateless argument keeps the call stable, directly, in
   [List.map], through a partial application and after unfolding. *)
let total_callback () : {b : bool | b} =
  let a = apply inc 0 in
  let b = apply inc 0 in
  a = b;;
[%%expect{|
val total_callback : unit -> {b : bool | b} = <fun>
|}]

let total_map () : {b : bool | b} =
  let a = List.map inc [0] in
  let b = List.map inc [0] in
  match a, b with [x], [y] -> x = y | _ -> true;;
[%%expect{|
val total_map : unit -> {b : bool | b} = <fun>
|}]

let total_partial () : {b : bool | b} =
  let g = apply inc in
  let a = g 0 in
  let b = g 0 in
  a = b;;
[%%expect{|
val total_partial : unit -> {b : bool | b} = <fun>
|}]

let unfolded_total () : {b : bool | b} =
  let a = apply_t inc 0 in
  let b = inc 0 in
  a = b;;
[%%expect{|
val unfolded_total : unit -> {b : bool | b} = <fun>
|}]

(* A parameter that the contract mentions must be total and stateless. *)
let contract_parameter (h : int -> int) : {b : bool | b && apply h 0 = apply h 0} =
  let a = apply h 0 in
  let b = apply h 0 in
  a = b;;
[%%expect{|
val contract_parameter :
  (h : (int -> int)) -> {b : bool | b && ((C.apply h 0) = (C.apply h 0))} =
  <fun>
|}]

(* Data holding a total, stateless closure. *)
let total_data () : {b : bool | b} =
  let (l @ total) = [{ f = inc }] in
  match l with
  | [r] ->
    let a = run r 0 in
    let b = run r 0 in
    a = b
  | _ -> true;;
[%%expect{|
val total_data : unit -> {b : bool | b} = <fun>
|}]

(* Set and map operations are modelled through the ordering, which calls into
   the elements. Keys of an abstract type may be effectful closures: two
   membership tests of the same key can differ. *)
module Fn_order = struct
  type t = unit -> int
  external icompare : int -> int -> int @@ total = "%compare"
  let[@def transparent] compare (f : t @ immutable) (g : t @ immutable) =
    icompare (f ()) (g ())
  let (reflexive @ total) (x : t) : {u : unit | compare x x = 0} @ ghost =
    ghost_ (compare_def x x; refine_ ())
  let (antisymmetric @ total) (x : t) (y : t) :
      {u : unit | (compare x y < 0) = (compare y x > 0)
        && (compare x y = 0) = (compare y x = 0)} @ ghost =
    ghost_ (compare_def x y; compare_def y x; refine_ ())
  let (transitive @ total) (x : t) (y : t) (z : t) :
      {u : unit | not (compare x y <= 0 && compare y z <= 0)
        || compare x z <= 0} @ ghost =
    ghost_ (compare_def x y; compare_def y z; compare_def x z; refine_ ())
end

module Membership (O : Set.TotalOrderedType) = struct
  module S = Set.MakeTotal (O)
  let stable (x : O.t) (s : S.t) : {b : bool | b} =
    let a = S.mem x s in
    let b = S.mem x s in
    a = b
end;;
[%%expect{|
module Fn_order :
  sig
    type t = unit -> int
    external icompare : int -> int -> int = "%compare"
    val compare : t @ immutable -> t @ immutable -> int
    val compare_def :
      (f : t) @ immutable ->
      (g : t) @ immutable ->
      {u : unit | (compare f g) === (icompare (f ()) (g ()))}
    val reflexive : (x : t) -> {u : unit | (compare x x) = 0} @ ghost
    val antisymmetric :
      (x : t) ->
      (y : t) ->
      {u : unit
        | (((compare x y) < 0) = ((compare y x) > 0)) &&
            (((compare x y) = 0) = ((compare y x) = 0))} @ ghost
    val transitive :
      (x : t) ->
      (y : t) ->
      (z : t) ->
      {u : unit
        | (not (((compare x y) <= 0) && ((compare y z) <= 0))) ||
            ((compare x z) <= 0)} @ ghost
  end
Line 23, characters 4-9:
23 |     a = b
         ^^^^^
Error: Refinement could not be proved (counterexample)
Line 20, characters 47-48:
20 |   let stable (x : O.t) (s : S.t) : {b : bool | b} =
                                                    ^
  The refinement is stated here.
|}]

(* A set of integers crosses both axes and keeps its model. *)
module Int_order = struct
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
module Int_set = Set.MakeTotal (Int_order)

let int_membership (x : int) (s : Int_set.t) : {b : bool | b} =
  let a = Int_set.mem x s in
  let b = Int_set.mem x s in
  a = b;;
[%%expect{|
module Int_order :
  sig
    type t = int
    external compare : int -> int -> int = "%compare"
    val reflexive : (x : t) -> {u : unit | (compare x x) = 0} @ ghost
    val antisymmetric :
      (x : t) ->
      (y : t) ->
      {u : unit
        | (((compare x y) < 0) = ((compare y x) > 0)) &&
            (((compare x y) = 0) = ((compare y x) = 0))} @ ghost
    val transitive :
      (x : t) ->
      (y : t) ->
      (z : t) ->
      {u : unit
        | (not (((compare x y) <= 0) && ((compare y z) <= 0))) ||
            ((compare x z) <= 0)} @ ghost
  end
module Int_set :
  sig
    type elt = Int_order.t
    type t = Set.MakeTotal(Int_order).t
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val find : elt -> t -> elt
    val find_first : (elt -> bool) -> t -> elt
    val find_last : (elt -> bool) -> t -> elt
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
    val empty : t @@ total
    val add : elt -> t -> t @@ total
    val singleton : elt -> t @@ total
    val remove : elt -> t -> t @@ total
    val union : t -> t -> t @@ total
    val inter : t -> t -> t @@ total
    val disjoint : t -> t -> bool @@ total
    val diff : t -> t -> t @@ total
    val cardinal : t -> int @@ total
    val elements : t -> elt list @@ total
    val min_elt_opt : t -> elt option @@ total
    val max_elt_opt : t -> elt option @@ total
    val choose_opt : t -> elt option @@ total
    val find_opt : elt -> t -> elt option @@ total
    val find_first_opt : (elt -> bool) -> t -> elt option @@ total
    val find_last_opt : (elt -> bool) -> t -> elt option @@ total
    val iter : (elt -> unit) -> t -> unit @@ total
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc @@ total
    val map : (elt -> elt) -> t -> t @@ total
    val filter : (elt -> bool) -> t -> t @@ total
    val filter_map : (elt -> elt option) -> t -> t @@ total
    val partition : (elt -> bool) -> t -> t * t @@ total
    val split : elt -> t -> t * bool * t @@ total
    val is_empty : t -> bool @@ total
    val mem : elt @ immutable -> t @ immutable -> bool @@ total
    val equal : t -> t -> bool @@ total
    val compare : t -> t -> int @@ total
    val subset : t -> t -> bool @@ total
    val for_all : (elt -> bool) -> t -> bool @@ total
    val exists : (elt -> bool) -> t -> bool @@ total
    val to_list : t -> elt list @@ total
    val of_list : elt list -> t @@ total
    val to_seq_from : elt -> t -> elt Seq.t @@ total
    val to_seq : t -> elt Seq.t @@ total
    val to_rev_seq : t -> elt Seq.t @@ total
    module Refined :
      sig
        val singleton : elt @ total -> t @ total @@ total
        val add : elt @ total -> t @ total -> t @ total @@ total
        val remove : elt -> t @ total -> t @ total @@ total
        val union : t @ total -> t @ total -> t @ total @@ total
        val inter : t @ total -> t @ total -> t @ total @@ total
        val diff : t @ total -> t @ total -> t @ total @@ total
        val find : (set : t) -> {elt : elt | mem elt set} -> elt @ total @@
          total
      end
  end
val int_membership : int -> Int_set.t -> {b : bool | b} = <fun>
|}]

(* An immutable field has its record's mode. *)
let total_field () : {b : bool | b} =
  let (r @ total) = { f = inc } in
  let a = apply r.f 0 in
  let b = apply r.f 0 in
  a = b;;
[%%expect{|
val total_field : unit -> {b : bool | b} = <fun>
|}]

(* Mutable data crosses totality and statelessness, so it does not make a call
   opaque. A total function cannot read it: reads are partial. *)
let (read @ total) (r : int ref) : int = !r;;
[%%expect{|
Line 1, characters 41-42:
1 | let (read @ total) (r : int ref) : int = !r;;
                                             ^
Error: The value "(!)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 19-43
         which is expected to be "total".
|}]
