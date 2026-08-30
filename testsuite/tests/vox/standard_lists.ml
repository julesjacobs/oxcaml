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

module Copy = struct
  let rec (copy @ total) :
      (xs : 'a list) -> {r : 'a list | r === xs} @ immutable contended =
    fun xs -> match xs with
    | [] ->
      refine_ xs
    | head :: tail ->
      let copied : {r : 'a list | r === tail} = copy tail in
      let refine_ copied = copied in
      let result = head :: copied in
      refine_ result
end
;;

let (nested @ total) : unit -> int list list @ immutable contended =
  fun () ->
    let input = [[1]; [2]] in
    let copied : {r : int list list | r === input} = Copy.copy input in
    let refine_ copied = copied in
    copied

type 'a list_alias = 'a list

module Uses = struct
  let polymorphic_empty = []

  let (empty_int @ total) () : {r : int list | r === []} =
    let (empty @ total) : int list = polymorphic_empty in
    refine_ empty

  let (refined @ total) l =
    List.Refined.hd l, List.Refined.tl l

  let (labeled_refined @ total) l =
    ListLabels.Refined.hd l, ListLabels.Refined.tl l

  let (alias_nonempty @ total) :
      (xs : int list_alias) ->
      {r : int list_alias | (r === []) === false} =
    fun xs ->
      let value = 0 :: xs in
      refine_ value

  let (literal_refined @ total) () =
    let values = [1; 2; 3] in
    let nonempty : {l : int list | (l === []) === false} = refine_ values in
    List.Refined.hd nonempty, List.Refined.tl nonempty

  let (higher_order @ total) xs =
    let mapped = List.map (fun x -> x + 1) xs in
    List.fold_left ( + ) 0 (mapped @ [])

  let (sequence @ total) xs = List.to_seq xs
end

let fails _ = failwith "callback"
let partial_callback xs = List.map fails xs
;;
[%%expect{|
module Copy :
  sig val copy : (xs : 'a list) -> {r : 'a list | r === xs} @ immutable end
val nested : unit -> int list list @ immutable = <fun>
type 'a list_alias = 'a list
module Uses :
  sig
    val polymorphic_empty : 'a list
    val empty_int : unit -> {r : int list | r === []}
    val refined : {l : 'a list | (l === List.[]) === false} -> 'a * 'a list
    val labeled_refined :
      {l : 'a list | (l === ListLabels.[]) === false} -> 'a * 'a list
    val alias_nonempty :
      int list_alias -> {r : int list_alias | (r === []) === false}
    val literal_refined : unit -> int * int list
    val higher_order : int list -> int
    val sequence : 'a list -> 'a Seq.t
  end
val fails : 'a -> 'b = <fun>
val partial_callback : 'a list -> 'b list = <fun>
|}]

let (bad_refined_domain @ total) (xs : int list) = List.Refined.hd xs;;
[%%expect{|
Line 1, characters 67-69:
1 | let (bad_refined_domain @ total) (xs : int list) = List.Refined.hd xs;;
                                                                       ^^
Error: The value "xs" has type "int list" but an expression was expected of type
         "{l : 'a list | (l === List.[]) === false}"
|}]

let (bad_refined_literal @ total) () = List.Refined.tl [];;
[%%expect{|
Line 1, characters 55-57:
1 | let (bad_refined_literal @ total) () = List.Refined.tl [];;
                                                           ^^
Error: This expression should not be a list literal, the expected type is
       "{l : 'a list | (l === List.[]) === false}"
|}]

type mutable_element = { mutable payload : int }

let update_refined_head
    (xs : {xs : mutable_element list | (xs === []) === false}) =
  (List.Refined.hd xs).payload <- 1;;
[%%expect{|
type mutable_element = { mutable payload : int; }
val update_refined_head :
  {xs : mutable_element list | (xs === []) === false} -> unit = <fun>
|}]

let (bad_callback @ total) xs = List.map fails xs;;
[%%expect{|
Line 1, characters 41-46:
1 | let (bad_callback @ total) xs = List.map fails xs;;
                                             ^^^^^
Error: The value "fails" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 27-49
         which is expected to be "total".
|}]

module User_named_list : sig end = struct
  type 'a list = Nil | Cons of ('a list) list

  let rejected () : {result : int list | (result === Nil) === false} =
    let nested : (int list) list = Nil in
    let result : int list = Cons nested in
    refine_ result
end;;
[%%expect{|
Line 7, characters 4-18:
7 |     refine_ result
        ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module List : sig end = struct
  type 'a t = Nil | Cons of ('a t) t

  let rejected () : {result : int t | (result === Nil) === false} =
    let nested : (int t) t = Nil in
    let result : int t = Cons nested in
    refine_ result
end;;
[%%expect{|
Line 7, characters 4-18:
7 |     refine_ result
        ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
