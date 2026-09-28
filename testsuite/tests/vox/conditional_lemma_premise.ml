(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

module Lemmas = struct
  let[@def] (step @ total) (x : int) = x - 1

  let (step_pos @ total) (x : {x : int | x >= 2}) :
      {u : unit | step x > 0} @ ghost =
    ghost_ (step_def x)

  (* The premise of an erased conditional lemma holds in its body. *)
  let (step_pos_if @ total) (x : int) :
      {u : unit | if x >= 2 then step x > 0 else true} @ ghost =
    ghost_ (step_pos x)

  let (use @ total) (x : {x : int | x >= 5}) : {u : unit | step x > 0} @ ghost =
    ghost_ (step_pos_if x)
end;;
[%%expect{|
module Lemmas :
  sig
    val step : int -> int
    val step_def : (x : int) -> {u : unit | (step x) === (x - 1)}
    val step_pos :
      (x : {x : int | x >= 2}) -> {u : unit | (step x) > 0} @ ghost
    val step_pos_if :
      (x : int) -> {u : unit | if x >= 2 then (step x) > 0 else true} @ ghost
    val use : (x : {x : int | x >= 5}) -> {u : unit | (step x) > 0} @ ghost
  end
|}]

(* The premise is assumed only for the conclusion's own [if]. *)
module Else_branch = struct
  let[@def] (step @ total) (x : int) = x - 1

  let (step_pos @ total) (x : {x : int | x >= 2}) :
      {u : unit | step x > 0} @ ghost =
    ghost_ (step_def x)

  let (wrong @ total) (x : int) :
      {u : unit | if x >= 2 then true else step x > 0} @ ghost =
    ghost_ (step_pos x)
end;;
[%%expect{|
Line 10, characters 21-22:
10 |     ghost_ (step_pos x)
                          ^
Error: Refinement could not be proved (counterexample: x = 0)
Line 4, characters 41-47:
4 |   let (step_pos @ total) (x : {x : int | x >= 2}) :
                                             ^^^^^^
  The refinement is stated here.
|}]

(* A body that runs cannot rely on the premise: it also runs when the premise
   is false. *)
module Runtime_body = struct
  let[@def] (step @ total) (x : int) = x - 1

  let (step_pos @ total) (x : {x : int | x >= 2}) :
      {u : unit | step x > 0} @ ghost =
    ghost_ (step_def x)

  let (step_pos_if @ total) (x : int) :
      {u : unit | if x >= 2 then step x > 0 else true} @ ghost =
    step_pos x
end;;
[%%expect{|
Line 8, characters 7-18:
8 |   let (step_pos_if @ total) (x : int) :
           ^^^^^^^^^^^
Warning 223 [unerased-ghost-body]: This function's result is ghost, but its body is not wrapped in
  "ghost_", so the body is computed when the function is called and its
  value may be thrown away. Wrap the body in "ghost_ (...)" to erase it.

Line 10, characters 13-14:
10 |     step_pos x
                  ^
Error: Refinement could not be proved (counterexample: x = 0)
Line 4, characters 41-47:
4 |   let (step_pos @ total) (x : {x : int | x >= 2}) :
                                             ^^^^^^
  The refinement is stated here.
|}]

(* Only the conclusion's own premise is assumed: a refinement of the payload
   and an inner annotation must hold without it. *)
module Nested = struct
  let (payload @ total) (x : int) :
      {u : {v : unit | x > 0} | if x > 0 then true else true} @ ghost =
    ghost_ ()
end;;
[%%expect{|
Line 4, characters 11-13:
4 |     ghost_ ()
               ^^
Error: Refinement could not be proved (counterexample: x = 0)
Line 3, characters 23-28:
3 |       {u : {v : unit | x > 0} | if x > 0 then true else true} @ ghost =
                           ^^^^^
  The refinement is stated here.
|}]

module Annotated = struct
  let (inner @ total) (x : int) :
      {u : unit | if x > 0 then true else true} @ ghost =
    (ghost_ () : {u : unit | x > 0})
end;;
[%%expect{|
Line 4, characters 12-14:
4 |     (ghost_ () : {u : unit | x > 0})
                ^^
Error: Refinement could not be proved (counterexample: x = 0)
Line 4, characters 29-34:
4 |     (ghost_ () : {u : unit | x > 0})
                                 ^^^^^
  The refinement is stated here.
|}]
