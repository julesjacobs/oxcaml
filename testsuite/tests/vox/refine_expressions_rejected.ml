(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
 { expect.opt; }
*)

let bad_value : {r : int | r = 0} = refine_ 1;;
[%%expect{|
Line 1, characters 36-45:
1 | let bad_value : {r : int | r = 0} = refine_ 1;;
                                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Bad_callback = struct
  type callbacks = { callback : int -> {r : int | r > 0} }
  let source _ : {r : int | r > 0} = let r = 1 in refine_ r
  let callbacks = {callback = source}
  let bad : int -> {r : int | r < 0} = refine_ callbacks.callback
end;;
[%%expect{|
Line 5, characters 39-65:
5 |   let bad : int -> {r : int | r < 0} = refine_ callbacks.callback
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Partial_callback = struct
  let rec diverge x = diverge x
  let bad : ((x : int) -> {r : int | r = x}) @ total =
    refine_ (if true then diverge else diverge)
end;;
[%%expect{|
Line 4, characters 4-47:
4 |     refine_ (if true then diverge else diverge)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "( *refine_value* )" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 4, characters 4-47
         which is expected to be "total".
|}]
