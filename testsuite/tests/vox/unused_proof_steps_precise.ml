(* TEST
 has-z3;
 flags = "-extension refinement_types -w +unused-proof-step -smt-unused-steps-precise";
 { expect; }
*)

(* With -smt-unused-steps-precise, each unsat core is shrunk by proving its
   query again without each proof step in it (warning 227). *)

module Lemmas = struct
  let[@def] (double @ total) (x : int) = x + x

  let (double_even @ total) (x : int) :
      {u : unit | double x = 2 * x} @ ghost =
    ghost_ (double_def x)

  let (double_positive @ total) (x : {x : int | 0 < x && x < 1000}) :
      {u : unit | double x > x} @ ghost =
    ghost_ (double_def x)
end;;
[%%expect{|
module Lemmas :
  sig
    val double : int -> int
    val double_def : (x : int) -> {u : unit | (double x) === (x + x)}
    val double_even : (x : int) -> {u : unit | (double x) = (2 * x)} @ ghost
    val double_positive :
      (x : {x : int | (0 < x) && (x < 1000)}) ->
      {u : unit | (double x) > x} @ ghost
  end
|}]

module Map_lambda_observations : sig end = struct
  module Probe (Order : Map.TotalOrderedType with type t = int) = struct
    module M = Map.MakeTotal (Order)

    let (same_present @ total)
        (p : (int @ immutable total -> bool @ ghost))
        (key : {k : int | p k}) : {u : unit | p key} @ ghost =
      ghost_ ()

    let (redundant_saved @ total) (key : int) : {b : bool | b} @ ghost =
      ghost_ (
        let p = ghost_ (fun (k : int) ->
          M.mem k (M.Refined.singleton k 7)) in
        let found = p key in
        same_present p key;
        found)
  end
end;;
[%%expect{|
Lines 12-13, characters 16-44:
12 | ................ghost_ (fun (k : int) ->
13 |           M.mem k (M.Refined.singleton k 7))...
Warning 225 [redundant-ghost]: This "ghost_" is redundant: the enclosing code is already ghost.

Line 15, characters 8-26:
15 |         same_present p key;
             ^^^^^^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "same_present".

module Map_lambda_observations : sig end
|}]

(* Both lemmas are in Z3's unsat core for the result, but [double_even x]
   and [0 < x] imply [r >= 2] without [double_positive x]: proving the result
   again without it shows that it is not needed. *)
module Redundant = struct
  open Lemmas

  let (f @ total) (x : {x : int | 0 < x && x < 1000}) :
      {r : int | r >= 2 && r = 2 * x} @ ghost =
    ghost_ (double_positive x; double_even x; double x)
end;;
[%%expect{|
Line 6, characters 12-29:
6 |     ghost_ (double_positive x; double_even x; double x)
                ^^^^^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "Lemmas.double_positive".

module Redundant :
  sig
    val f :
      (x : {x : int | (0 < x) && (x < 1000)}) ->
      {r : int | (r >= 2) && (r = (2 * x))} @ ghost
  end
|}]

(* Steps that are needed are not reported. *)
module Needed = struct
  open Lemmas

  let (f @ total) (x : int) (y : {y : int | 0 < y && y < 100}) :
      {r : int | r = 2 * x && y + 1 > 1} @ ghost =
    ghost_ (double_even x; double_def 7; double x)
end;;
[%%expect{|
Line 6, characters 27-39:
6 |     ghost_ (double_even x; double_def 7; double x)
                               ^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "Lemmas.double_def".

module Needed :
  sig
    val f :
      (x : int) ->
      (y : {y : int | (0 < y) && (y < 100)}) ->
      {r : int | (r = (2 * x)) && ((y + 1) > 1)} @ ghost
  end
|}]

module Map_observations : sig end = struct
  module Probe (Order : Map.TotalOrderedType with type t = int) = struct
    module M = Map.MakeTotal (Order)

    let (singleton_member @ total) (key : int) :
        {u : unit | M.mem key (M.Refined.singleton key 7)} @ ghost =
      ghost_ ()

    let (redundant @ total) (key : int) :
        {u : unit | M.mem key (M.Refined.singleton key 7)} @ ghost =
      ghost_ (singleton_member key; singleton_member key; ())

    let (same_member @ total) (key : int)
        (map : {m : int M.t | M.mem key m}) :
        {u : unit | M.mem key map} @ ghost = ghost_ ()

    let (redundant_saved @ total) (key : int) : {b : bool | b} @ ghost =
      ghost_ (
        let map = M.Refined.singleton key 7 in
        let found = M.mem key map in
        same_member key map;
        found)
  end
end;;
[%%expect{|
Line 11, characters 14-34:
11 |       ghost_ (singleton_member key; singleton_member key; ())
                   ^^^^^^^^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "singleton_member".

Line 11, characters 36-56:
11 |       ghost_ (singleton_member key; singleton_member key; ())
                                         ^^^^^^^^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "singleton_member".

Line 21, characters 8-27:
21 |         same_member key map;
             ^^^^^^^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "same_member".

module Map_observations : sig end
|}]
