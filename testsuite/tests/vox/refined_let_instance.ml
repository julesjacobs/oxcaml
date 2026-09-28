(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* A local [let] of a value with a refined type is not generalized, so its
   facts are stated and used at one instance. *)

module M : sig
  type ('a : immutable_data) t
  val size : ('a : immutable_data). 'a t @ immutable -> int @@ total
  val empty : ('a : immutable_data). {q : 'a t | size q = 0} @@ total immutable
end = struct
  type ('a : immutable_data) t = 'a list
  let[@def] (size @ total) (q : 'a list) =
    match q with [] -> 0 | _ :: _ -> 1
  let (empty @ total) : {q : 'a list | size q = 0} =
    let result : 'a list = [] in
    ghost_ (size_def result); result
end;;
[%%expect{|
module M :
  sig
    type ('a : immutable_data) t
    val size : ('a : immutable_data). 'a t @ immutable -> int @@ total
    val empty : ('a : immutable_data). {q : 'a t | (size q) = 0} @@ total
      immutable
  end
|}]

let (size_of_empty @ total) () : {n : int | n = 0} =
  let q = M.empty in M.size q;;
[%%expect{|
val size_of_empty : unit -> {n : int | n = 0} = <fun>
|}]

let (pair_of_empty @ total) () : {n : int | n = 0} =
  let q = (M.empty, 1) in match q with (q, _) -> M.size q;;
[%%expect{|
val pair_of_empty : unit -> {n : int | n = 0} = <fun>
|}]

(* The value has one type. *)
let two_instances () =
  let q = M.empty in (M.size (q : int M.t), M.size (q : bool M.t));;
[%%expect{|
Line 2, characters 52-53:
2 |   let q = M.empty in (M.size (q : int M.t), M.size (q : bool M.t));;
                                                        ^
Error: The value "q" has type "int M.t" but an expression was expected of type
         "bool M.t"
       Type "int" is not compatible with type "bool"
|}]

(* Values without refinements are generalized as in OCaml. *)
let ordinary () = let r = [] in (1 :: r, true :: r);;
[%%expect{|
val ordinary : unit -> int list * bool list = <fun>
|}]
