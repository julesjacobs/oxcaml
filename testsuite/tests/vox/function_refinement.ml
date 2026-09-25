(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

module Adapt = struct
  let transport : (a : int) -> (b : int) ->
      (int -> {r : int | r = a}) ->
      {u : unit | a = b} -> (x : int) -> {r : int | r = b} =
    fun a b f eq ->
    let refine_ eq = eq in
    refine_ f

  let weaken (f : int -> {r : int | r > 0}) : int -> {r : int | r >= 0} =
    refine_ f
end;;
[%%expect{|
module Adapt :
  sig
    val transport :
      (a : int) ->
      (b : int) ->
      (int -> {r : int | r = a}) ->
      {u : unit | a = b} -> int -> {r : int | r = b}
    val weaken : (int -> {r : int | r > 0}) -> int -> {r : int | r >= 0}
  end
|}]

module Wrong = struct
  let strengthen (f : int -> {r : int | r >= 0}) : int -> {r : int | r > 0} =
    refine_ f
end;;
[%%expect{|
Line 3, characters 4-13:
3 |     refine_ f
        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Wrong_domain = struct
  let adapt (f : {x : int | x > 0} -> {r : int | r >= 0}) :
      int -> {r : int | r >= 0} = refine_ f
end;;
[%%expect{|
Line 3, characters 34-43:
3 |       int -> {r : int | r >= 0} = refine_ f
                                      ^^^^^^^^^
Error: The value "( *refine_arg_0* )" has type "int"
       but an expression was expected of type "{x : int | x > 0}"
|}]

module Once_capture = struct
  let adapt (f : (int -> {r : int | r > 0}) @ once) :
      (int -> {r : int | r >= 0}) @ many = refine_ f
end;;
[%%expect{|
Line 3, characters 51-52:
3 |       (int -> {r : int | r >= 0}) @ many = refine_ f
                                                       ^
Error: The value "f" is "once"
       but is expected to be "many"
         because it is used inside the function at line 3, characters 43-52
         which is expected to be "many".
|}]
