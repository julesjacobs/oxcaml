(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* [@def] on constants, [()] and tuple parameters, [function] cases and
   locally abstract types. *)

let[@def] (c @ total) = [1; 2];;
[%%expect{|
val c : int list = [1; 2]
val c_def : unit -> {u : unit | c === [1; 2]} = <fun>
|}]

let[@def] (e @ total) = [];;
[%%expect{|
val e : 'a list = []
val e_def : unit -> {u : unit | e === []} = <fun>
|}]

let[@def] (pair @ total) (x, y) = x + y;;
[%%expect{|
val pair : int * int -> int = <fun>
val pair_def :
  (arg : (int * int)) ->
  {u : unit | (pair arg) === (match arg with | (x, y) -> x + y)} = <fun>
|}]

let[@def] (unit_f @ total) () = 3;;
[%%expect{|
val unit_f : unit -> int = <fun>
val unit_f_def :
  (arg : unit) -> {u : unit | (unit_f arg) === (match arg with | () -> 3)} =
  <fun>
|}]

let[@def] (cases @ total) x = function [] -> x | _ :: _ -> 0;;
[%%expect{|
val cases : int -> 'a list -> int = <fun>
val cases_def :
  (x : int) ->
  (arg : 'a list) ->
  {u : unit | (cases x arg) === (match arg with | [] -> x | _::_ -> 0)} =
  <fun>
|}]

let[@def] (id_list @ total) : type a. a list -> a list = fun x -> x;;
[%%expect{|
val id_list : 'a list -> 'a list = <fun>
val id_list_def : (x : 'a list) -> {u : unit | (id_list x) === x} = <fun>
|}]

let[@def] (id_list2 @ total) (type a) (x : a list) = x;;
[%%expect{|
val id_list2 : 'a list -> 'a list = <fun>
val id_list2_def : (x : 'a list) -> {u : unit | (id_list2 x) === x} = <fun>
|}]

let (use @ total) (l : int list) : {u : unit |
    c === [1; 2] && (e : int list) === [] && pair (1, 2) = 3
    && unit_f () = 3 && cases 5 ([] : int list) = 5 && cases 5 [1] = 0
    && id_list l === l && id_list [1] === [1] && id_list2 l === l} =
  ghost_ (
    c_def ();
    (e_def () : {u : unit | (e : int list) === []});
    pair_def (1, 2);
    unit_f_def ();
    cases_def 5 ([] : int list);
    cases_def 5 [1];
    id_list_def l;
    id_list_def [1];
    id_list2_def l);;
[%%expect{|
val use :
  (l : int list) ->
  {u : unit
    | (c === [1; 2]) &&
        (((e : int list) === []) &&
           (((pair (1, 2)) = 3) &&
              (((unit_f ()) = 3) &&
                 (((cases 5 ([] : int list)) = 5) &&
                    (((cases 5 [1]) = 0) &&
                       (((id_list l) === l) &&
                          (((id_list [1]) === [1]) && ((id_list2 l) === l))))))))} @ ghost =
  <fun>
|}]

(* The equation of a constant crosses units (here, phrases). *)
module Defined = struct let[@def] (e1 @ total) = [None] end;;
let (elsewhere @ total) () :
  {u : unit | (Defined.e1 : int option list) === [None]} =
  ghost_ (Defined.e1_def ());;
[%%expect{|
module Defined :
  sig
    val e1 : 'a option list
    val e1_def : unit -> {u : unit | e1 === [None]}
  end
val elsewhere :
  unit -> {u : unit | (Defined.e1 : int option list) === [None]} @ ghost =
  <fun>
|}]

(* The equations are the definitions. *)
let (wrong @ total) () : {u : unit | pair (1, 2) = 4} =
  ghost_ (pair_def (1, 2));;
[%%expect{|
Line 2, characters 9-26:
2 |   ghost_ (pair_def (1, 2));;
             ^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 37-52:
1 | let (wrong @ total) () : {u : unit | pair (1, 2) = 4} =
                                         ^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* A generated parameter name avoids the names of the definition. *)
let[@def] (shadow @ total) arg (x, y) = arg + x + y;;
[%%expect{|
val shadow : int -> int * int -> int = <fun>
val shadow_def :
  (arg : int) ->
  (arg1 : (int * int)) ->
  {u : unit
    | (shadow arg arg1) === (match arg1 with | (x, y) -> (arg + x) + y)} =
  <fun>
|}]

(* Moving the match after a later parameter would change which [x] the
   body sees, so this stays rejected. *)
let[@def] (shadowed @ total) (x, y) x = x;;
[%%expect{|
Line 1, characters 29-35:
1 | let[@def] (shadowed @ total) (x, y) x = x;;
                                 ^^^^^^
Error: Definition lemmas require a function with simple unlabelled parameters
|}]
