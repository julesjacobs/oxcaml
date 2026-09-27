(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

module P = struct
  type ('a : immutable_data) t = 'a list
  let[@def] (is_empty @ total) (m : 'a t) =
    match m with [] -> true | _ -> false
  let (empty @ total) : ('a : immutable_data). 'a t = []
  let (is_empty_empty @ total) (m : 'a t)
      : {u : unit | not (m === empty) || is_empty m} =
    is_empty_def m
end
open P;;
[%%expect{|
module P :
  sig
    type ('a : immutable_data) t = 'a list
    val is_empty : ('a : immutable_data). 'a t -> bool
    val is_empty_def :
      ('a : immutable_data).
        (m : 'a t) ->
        {u : unit
          | (is_empty m) === (match m with | [] -> true | _ -> false)}
    val empty : ('a : immutable_data). 'a t
    val is_empty_empty :
      ('a : immutable_data).
        (m : 'a t) -> {u : unit | (not (m === empty)) || (is_empty m)}
  end
|}]

(* A type variable first named in a predicate is the variable of the same
   name in the rest of the binding. *)
let (law @ total) () : {u : unit | is_empty (empty : 'a t)} =
  is_empty_empty (empty : 'a t);;
[%%expect{|
val law : unit -> {u : unit | P.is_empty (P.empty : 'a P.t)} = <fun>
|}]

let (law_at_int @ total) () : {u : unit | is_empty (empty : 'a t)} =
  let _ = ((empty : 'a t) : int t) in
  is_empty_empty (empty : 'a t);;
[%%expect{|
val law_at_int : unit -> {u : unit | P.is_empty (P.empty : int P.t)} = <fun>
|}]

(* An ascription with a refined abbreviation keeps the refined type when the
   expression already has it, so it still fits parameters of that type. *)
type ('a : immutable_data) r = {l : 'a list | l === l};;
let (f @ total) (x : 'a r) = x;;
let (g @ total) (e : int r) : {u : unit | f (e : int r) === f e} = ();;
[%%expect{|
type ('a : immutable_data) r = {l : 'a list | l === l}
val f : ('a : immutable_data). 'a r -> 'a list = <fun>
val g : (e : int r) -> {u : unit | (f (e : int r)) === (f e)} = <fun>
|}]

(* Otherwise the ascription uses the payload, as before. *)
type 'a nonempty = {l : 'a list | not (l === [])};;
let (local @ total) () :
  {u : unit | let l = [1] in (l : int nonempty) === [1]} = ();;
[%%expect{|
type 'a nonempty = {l : 'a list | not (l === [])}
val local : unit -> {u : unit | let l = [1] in (l : int list) === [1]} =
  <fun>
|}]
