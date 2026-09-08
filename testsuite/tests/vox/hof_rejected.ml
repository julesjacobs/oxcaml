(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 { expect; }
 { expect.opt; }
*)

module Functions : sig
  val apply : (int -> int) @ total -> int -> int @@ total
  val offset : int @ total -> (int -> int) @ total @@ total
end = struct
  let (apply @ total) (f : (int -> int) @ total) (x : int) = f x
  let (offset @ total) : int @ total -> (int -> int) @ total =
    fun delta x -> delta + x
end
open Functions;;
[%%expect{|
module Functions :
  sig
    val apply : (int -> int) @ total -> int -> int @@ total
    val offset : int @ total -> (int -> int) @ total @@ total
  end
|}]

let same_capture (delta : int) (x : int) :
    {u : unit | apply (offset delta) x === apply (offset delta) x} =
  let left = offset delta in
  let right = offset delta in
  let u = () in
  let refine_ proof =
    (refine_ u : {u : unit | apply left x === apply right x}) in
  refine_ u;;
[%%expect{|
val same_capture :
  (delta : int) ->
  (x : int) ->
  {u : unit
    | (Functions.apply (Functions.offset delta) x) ===
        (Functions.apply (Functions.offset delta) x)} =
  <fun>
|}]

let different_capture (a : int) (b : int) (x : int) :
    {u : unit | apply (offset a) x === apply (offset b) x} =
  let u = () in refine_ u;;
[%%expect{|
Line 3, characters 16-25:
3 |   let u = () in refine_ u;;
                    ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let false_callback : (x : int) -> {y : int | y > x} = fun x -> refine_ x;;
[%%expect{|
Line 1, characters 63-72:
1 | let false_callback : (x : int) -> {y : int | y > x} = fun x -> refine_ x;;
                                                                   ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let partial_predicate (x : int) = if x = 0 then failwith "partial" else x;;
let invalid_model (x : int) : {y : int | y = apply partial_predicate x} =
  refine_ x;;
[%%expect{|
val partial_predicate : int -> int = <fun>
Line 2, characters 51-68:
2 | let invalid_model (x : int) : {y : int | y = apply partial_predicate x} =
                                                       ^^^^^^^^^^^^^^^^^
Error: The value "partial_predicate" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 41-70).
|}]

module Position (F : sig
  val opaque : 'a @ immutable total -> 'b @ immutable total -> bool @@ total
end) = struct
  let argument_position (x : int) :
      {u : unit | F.opaque (offset x) x === F.opaque x (offset x)} =
    let u = () in refine_ u
end;;
[%%expect{|
Line 6, characters 18-27:
6 |     let u = () in refine_ u
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

type t = { value : int; trace : int list @@ ghost };;
let trace_is_not_empty (x : int) :
    {r : t | r.trace === []} =
  let r = {value = x; trace = ghost_ [x]} in
  refine_ r;;
[%%expect{|
type t = { value : int; trace : int list @@ ghost; }
Line 5, characters 2-11:
5 |   refine_ r;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Frequency_probe = struct
  module Key = struct
    type t = int
    external compare : int -> int -> int @@ total = "%compare"
  end
  module M = Map.MakeTotal (Key)
  let whole_map_model (map : Bigint.t M.t) :
      {result : Bigint.t M.t | result === map} = refine_ map
end;;
[%%expect{|
Line 8, characters 31-45:
8 |       {result : Bigint.t M.t | result === map} = refine_ map
                                   ^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 8, characters 49-60:
8 |       {result : Bigint.t M.t | result === map} = refine_ map
                                                     ^^^^^^^^^^^
  Required by this refinement introduction
|}]

let needs_positive (x : {n : int | n > 0}) = let refine_ n = x in n;;
let missing_precondition (x : int) = needs_positive (refine_ x);;
[%%expect{|
val needs_positive : {n : int | n > 0} -> int = <fun>
Line 2, characters 52-63:
2 | let missing_precondition (x : int) = needs_positive (refine_ x);;
                                                        ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
