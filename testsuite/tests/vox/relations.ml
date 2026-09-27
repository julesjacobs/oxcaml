(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
*)

(* An inductive relation written by hand: a derivation datatype with one
   constructor per rule, a conclusion function, a [@def] validity predicate
   and a transparent predicate [derives d x z]. Rule induction is structural
   recursion over the derivation. *)

external ( < ) : int -> int -> bool @@ total = "%lessthan"
external ( <= ) : int -> int -> bool @@ total = "%lessequal";;
[%%expect{|
external ( < ) : int -> int -> bool = "%lessthan"
external ( <= ) : int -> int -> bool = "%lessequal"
|}]

(* The reflexive-transitive closure of [step]. *)
module Star = struct
  let[@def transparent] step (x : int) (y : int) = x < y

  (* Refl x proves x ->* x; Step (x, y, z, rest) proves x ->* z from
     step x y and a derivation rest of y ->* z. *)
  type star = Refl of int | Step of int * int * int * star [@@inductive]

  let[@def transparent] concl d =
    match d with Refl x -> (x, x) | Step (x, _, z, _) -> (x, z)

  let[@def] rec valid d = ghost_ (
    match d with
    | Refl _ -> true
    | Step (x, y, z, rest) -> step x y && valid rest && concl rest === (y, z))

  let[@def transparent] derives d (x : int) (z : int) =
    ghost_ (valid d && concl d === (x, z))
end
open Star;;
[%%expect{|
module Star :
  sig
    val step : int -> int -> bool
    val step_def :
      (x : int) -> (y : int) -> {u : unit | (step x y) === (x < y)}
    type star = Refl of int | Step of int * int * int * star
    [@@inductive]
    val concl : star -> int * int
    val concl_def :
      (d : star) ->
      {u : unit
        | (concl d) ===
            (match d with | Refl x' -> (x', x') | Step (x, _, z, _) -> (x, z))}
    val valid : star @ total -> bool @ ghost
    val valid_def :
      (d : star) ->
      {u : unit
        | (valid d) ===
            (ghost_
               (match d with
                | Refl _ -> true
                | Step (x, y, z, rest) ->
                    (step x y) && ((valid rest) && ((concl rest) === (y, z)))))}
    val derives : star @ total -> int -> int -> bool @ ghost
    val derives_def :
      (d : star) ->
      (x : int) ->
      (z : int) ->
      {u : unit
        | (derives d x z) === (ghost_ ((valid d) && ((concl d) === (x, z))))}
  end
|}]

(* Rule induction: [valid_def] gives the premises of the last rule, and the
   recursive call is the induction hypothesis. *)
let rec (monotone @ total) :
    (d : star) -> (x : int) -> (z : int) ->
    {u : unit | if derives d x z then x <= z else true} @ ghost =
  fun d x z -> ghost_ (
    valid_def d;
    match d with
    | Refl _ -> ()
    | Step (_, y, _, rest) -> monotone rest y z);;
[%%expect{|
val monotone :
  (d : Star.star) ->
  ((x : int) ->
   (z : int) ->
   {u : unit | if Star.derives d x z then x <= z else true} @ ghost) @ total
  stateful = <fun>
|}]

(* Building a derivation: transitivity. *)
let rec (trans @ total) :
    (d1 : star) -> (d2 : star) -> (x : int) -> (y : int) -> (z : int) ->
    {d : star | if derives d1 x y && derives d2 y z then derives d x z
      else true} @ ghost =
  fun d1 d2 x y z -> ghost_ (
    valid_def d1;
    match d1 with
    | Refl _ -> d2
    | Step (_, w, _, rest) ->
      let d = Step (x, w, z, trans rest d2 w y z) in
      valid_def d;
      d);;
[%%expect{|
val trans :
  (d1 : Star.star) ->
  ((d2 : Star.star) ->
   (x : int) ->
   (y : int) ->
   (z : int) ->
   {d : Star.star
     | if (Star.derives d1 x y) && (Star.derives d2 y z)
       then Star.derives d x z
       else true} @ ghost) @ total
  stateful = <fun>
|}]

(* A false claim is rejected. *)
let rec (wrong @ total) :
    (d : star) -> (x : int) -> (z : int) ->
    {u : unit | if derives d x z then x < z else true} @ ghost =
  fun d x z -> ghost_ (
    valid_def d;
    match d with
    | Refl _ -> ()
    | Step (_, y, _, rest) -> wrong rest y z);;
[%%expect{|
Line 7, characters 16-18:
7 |     | Refl _ -> ()
                    ^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 16-53:
3 |     {u : unit | if derives d x z then x < z else true} @ ghost =
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]
