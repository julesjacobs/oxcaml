(* TEST
 has-z3;
 flags = "-extension refinement_types -w +unused-proof-step -w -slow-refinement -smt-resource-warning 1";
 { expect; }
*)

(* When a function's goals are not proved in one query (here, the batch's
   resource budget is one unit), each refinement is proved alone, and each of
   those proofs has its own unsat core (warning 227). *)

module Lemmas = struct
  let[@def] (double @ total) (x : int) = x + x

  let (double_even @ total) (x : int) :
      {u : unit | double x = 2 * x} @ ghost =
    ghost_ (double_def x)
end;;
[%%expect{|
module Lemmas :
  sig
    val double : int -> int
    val double_def : (x : int) -> {u : unit | (double x) === (x + x)}
    val double_even : (x : int) -> {u : unit | (double x) = (2 * x)} @ ghost
  end
|}]

module Split = struct
  open Lemmas

  let (f @ total) (x : int) (y : int) (z : int) :
      {r : int | r = 2 * x} @ ghost =
    ghost_ (
      double_even x;
      double_even y;
      double_even z;
      let (_ : {b : int | b = 2 * y}) = double y in
      double x)
end;;
[%%expect{|
Line 9, characters 6-19:
9 |       double_even z;
          ^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "Lemmas.double_even".

module Split :
  sig val f : (x : int) -> int -> int -> {r : int | r = (2 * x)} @ ghost end
|}]
