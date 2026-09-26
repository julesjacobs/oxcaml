(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* A non-function [let] inside ghost code is not generalized, so every use of
   the bound value is the same instance. *)

let[@def] rec (length @ total) (l : 'a list) : int =
  match l with
  | [] -> 0
  | _ :: rest -> 1 + length rest;;
[%%expect{|
val length : 'a list -> int = <fun>
val length_def :
  (l : 'a list) ->
  {u : unit
    | (length l) ===
        (match l with | [] -> 0 | _::rest -> 1 + (length rest) : int)} =
  <fun>
|}]

let (empty_length @ total) () : {n : int | n = 0} =
  ghost_ (
    let l = [] in
    length_def l;
    length l);;
[%%expect{|
val empty_length : unit -> {n : int | n = 0} @ ghost = <fun>
|}]

let (outside @ total) () =
  let l = [] in
  (1 :: l, true :: l);;
[%%expect{|
val outside : unit -> int list * bool list = <fun>
|}]

let (inside @ total) () =
  ghost_ (
    let l = [] in
    (1 :: l, true :: l));;
[%%expect{|
Line 4, characters 21-22:
4 |     (1 :: l, true :: l));;
                         ^
Error: The value "l" has type "int list" but an expression was expected of type
         "bool list"
       Type "int" is not compatible with type "bool"
|}]

let (functions_still_generalize @ total) () =
  ghost_ (
    let id x = x in
    (id 1, id true));;
[%%expect{|
val functions_still_generalize : unit -> int * bool @ ghost = <fun>
|}]

(* Only type variables are kept back: under -principal, mode crossing of a
   let-bound value still sees its generalized type. *)
module Crossing = struct
  type m = { count : int; state : int list }
  let (cap @ total) (x : int) : int = x + 1
  let (f @ total) (x : int) : unit @ ghost = ghost_ (
    let needed = cap x in
    let model = { count = cap x; state = [] } in
    let _ : {u : unit | model.count = needed && model.state === []} = () in
    ())
end;;
[%%expect{|
module Crossing :
  sig
    type m = { count : int; state : int list; }
    val cap : int -> int
    val f : int -> unit @ ghost
  end
|}]
