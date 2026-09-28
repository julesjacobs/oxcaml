(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
*)

(* [@def transparent] names a non-recursive predicate or function whose
   definition the verifier unfolds at every direct application. *)

module D = struct
  let[@def transparent] inv (x : int) (y : int) = 0 <= x && x < y
  let[@def transparent] small (x : int) = x > 3
  let[@def transparent] window (x : int) = small x && x < 10
  let[@def transparent] same (x : 'a) (y : 'a) = ghost_ (x === y)
  let[@def] opaque (x : int) = x > 3
end
open D;;
[%%expect{|
module D :
  sig
    val inv : int -> int -> bool
    val inv_def :
      (x : int) ->
      (y : int) -> {u : unit | (inv x y) === ((0 <= x) && (x < y))}
    val small : int -> bool
    val small_def : (x : int) -> {u : unit | (small x) === (x > 3)}
    val window : int -> bool
    val window_def :
      (x : int) -> {u : unit | (window x) === ((small x) && (x < 10))}
    val same : 'a @ total -> 'a @ total -> bool @ ghost
    val same_def :
      (x : 'a) -> (y : 'a) -> {u : unit | (same x y) === (ghost_ (x === y))}
    val opaque : int -> bool
    val opaque_def : (x : int) -> {u : unit | (opaque x) === (x > 3)}
  end
|}]

(* A named precondition. *)
let (first @ total) :
    (x : int) -> (y : {y : int | inv x y}) -> {r : int | r >= 0} =
  fun x _ -> x;;
[%%expect{|
val first :
  (x : int) -> ({y : int | D.inv x y} -> {r : int | r >= 0}) @ total stateful =
  <fun>
|}]

(* A call in code is unfolded too. *)
let branch (x : int) (y : int) : {r : int | r >= 0} =
  if inv x y then x else 0;;
[%%expect{|
val branch : int -> int -> {r : int | r >= 0} = <fun>
|}]

(* Nested definitions unfold. *)
let nested (x : {x : int | window x}) : {r : int | r > 3 && r < 10} = x;;
[%%expect{|
val nested : {x : int | D.window x} -> {r : int | (r > 3) && (r < 10)} =
  <fun>
|}]

let nested_wrong (x : {x : int | window x}) : {r : int | r > 4} = x;;
[%%expect{|
Line 1, characters 66-67:
1 | let nested_wrong (x : {x : int | window x}) : {r : int | r > 4} = x;;
                                                                      ^
Error: Refinement could not be proved (counterexample: x = 4)
Line 1, characters 57-62:
1 | let nested_wrong (x : {x : int | window x}) : {r : int | r > 4} = x;;
                                                             ^^^^^
  The refinement is stated here.
|}]

(* Polymorphic definitions are instantiated at the application. *)
let poly (a : int list) (b : {b : int list | same a b}) :
    {r : int list | r === a} = b;;
[%%expect{|
val poly :
  (a : int list) -> {b : int list | D.same a b} -> {r : int list | r === a} =
  <fun>
|}]

(* A plain [@def] still needs its lemma. *)
let plain (x : {x : int | opaque x}) : {r : int | r > 3} = x;;
[%%expect{|
Line 1, characters 59-60:
1 | let plain (x : {x : int | opaque x}) : {r : int | r > 3} = x;;
                                                               ^
Error: Refinement could not be proved (counterexample: x = 0)
Line 1, characters 50-55:
1 | let plain (x : {x : int | opaque x}) : {r : int | r > 3} = x;;
                                                      ^^^^^
  The refinement is stated here.
|}]

let plain_with_lemma (x : {x : int | opaque x}) : {r : int | r > 3} =
  ghost_ (opaque_def x);
  x;;
[%%expect{|
val plain_with_lemma : {x : int | D.opaque x} -> {r : int | r > 3} = <fun>
|}]

let local () : {r : int | r = 5} =
  let[@def transparent] five (x : int) = x = 5 in
  let (y : {y : int | five y}) = 5 in
  y;;
[%%expect{|
val local : unit -> {r : int | r = 5} = <fun>
|}]

(* Transparency must not recurse. *)
module Recursive = struct
  let[@def transparent] rec count (x : int) = if x <= 0 then 0 else count (x - 1)
end;;
[%%expect{|
Line 2, characters 5-23:
2 |   let[@def transparent] rec count (x : int) = if x <= 0 then 0 else count (x - 1)
         ^^^^^^^^^^^^^^^^^^
Error: A transparent definition cannot be recursive
|}]

(* A signature exports the attribute with the lemma. *)
module S : sig
  val pos : int -> bool @@ total [@@def transparent]
  val pos_def : (x : int) -> {u : unit | pos x === (x > 0)} @@ total
end = struct
  let[@def transparent] pos (x : int) = x > 0
end;;
[%%expect{|
module S :
  sig
    val pos : int -> bool @@ total
    val pos_def : (x : int) -> {u : unit | (pos x) === (x > 0)} @@ total
  end
|}]

let exported (x : {x : int | S.pos x}) : {r : int | r > 0} = x;;
[%%expect{|
val exported : {x : int | S.pos x} -> {r : int | r > 0} = <fun>
|}]

module Bad_payload : sig
  val pos : int -> bool @@ total [@@def]
end = struct
  let pos (x : int) = x > 0
end;;
[%%expect{|
Line 2, characters 33-40:
2 |   val pos : int -> bool @@ total [@@def]
                                     ^^^^^^^
Error: In a signature, the def attribute requires the payload transparent
|}]

(* Without a usable lemma the definition cannot be unfolded. *)
module Missing : sig
  val pos : int -> bool @@ total [@@def transparent]
end = struct
  let[@def transparent] pos (x : int) = x > 0
end;;
[%%expect{|
module Missing : sig val pos : int -> bool @@ total end
|}]

let missing (x : {x : int | Missing.pos x}) : {r : int | r > 0} = x;;
[%%expect{|
Line 1, characters 66-67:
1 | let missing (x : {x : int | Missing.pos x}) : {r : int | r > 0} = x;;
                                                                      ^
Error: Refinement could not be proved (counterexample: x = 0)
Line 1, characters 57-62:
1 | let missing (x : {x : int | Missing.pos x}) : {r : int | r > 0} = x;;
                                                             ^^^^^
  The refinement is stated here.
Line 1, characters 13-14:
1 | let missing (x : {x : int | Missing.pos x}) : {r : int | r > 0} = x;;
                 ^
  This refinement premise was omitted because it could not be translated to SMT
Line 1, characters 28-41:
1 | let missing (x : {x : int | Missing.pos x}) : {r : int | r > 0} = x;;
                                ^^^^^^^^^^^^^
  The transparent definition Missing.pos cannot be unfolded: pos_def must be a total lemma stating its definition, with no refinement nested inside a parameter type
Line 1, characters 66-67:
1 | let missing (x : {x : int | Missing.pos x}) : {r : int | r > 0} = x;;
                                                                      ^
  This refinement premise was omitted because it could not be translated to SMT
Line 1, characters 28-41:
1 | let missing (x : {x : int | Missing.pos x}) : {r : int | r > 0} = x;;
                                ^^^^^^^^^^^^^
  The transparent definition Missing.pos cannot be unfolded: pos_def must be a total lemma stating its definition, with no refinement nested inside a parameter type
|}]

(* A partial lemma proves nothing. *)
module Loop : sig
  val pos : int -> bool @@ total [@@def transparent]
  val pos_def : (x : int) -> {u : unit | pos x === (x > 0)}
end = struct
  let (pos @ total) (x : int) = x > 0
  let rec pos_def (x : int) : {u : unit | pos x === (x > 0)} = pos_def x
end;;
[%%expect{|
module Loop :
  sig
    val pos : int -> bool @@ total
    val pos_def : (x : int) -> {u : unit | (pos x) === (x > 0)}
  end
|}]

let looping (x : {x : int | Loop.pos x}) : {r : int | r > 0} = x;;
[%%expect{|
Line 1, characters 63-64:
1 | let looping (x : {x : int | Loop.pos x}) : {r : int | r > 0} = x;;
                                                                   ^
Error: Refinement could not be proved (counterexample: x = 0)
Line 1, characters 54-59:
1 | let looping (x : {x : int | Loop.pos x}) : {r : int | r > 0} = x;;
                                                          ^^^^^
  The refinement is stated here.
Line 1, characters 13-14:
1 | let looping (x : {x : int | Loop.pos x}) : {r : int | r > 0} = x;;
                 ^
  This refinement premise was omitted because it could not be translated to SMT
Line 1, characters 28-38:
1 | let looping (x : {x : int | Loop.pos x}) : {r : int | r > 0} = x;;
                                ^^^^^^^^^^
  The transparent definition Loop.pos cannot be unfolded: pos_def must be a total lemma stating its definition, with no refinement nested inside a parameter type
Line 1, characters 63-64:
1 | let looping (x : {x : int | Loop.pos x}) : {r : int | r > 0} = x;;
                                                                   ^
  This refinement premise was omitted because it could not be translated to SMT
Line 1, characters 28-38:
1 | let looping (x : {x : int | Loop.pos x}) : {r : int | r > 0} = x;;
                                ^^^^^^^^^^
  The transparent definition Loop.pos cannot be unfolded: pos_def must be a total lemma stating its definition, with no refinement nested inside a parameter type
|}]

(* A lemma with refined parameters holds under their refinements. *)
module Conditional : sig
  val pos : int -> bool @@ total [@@def transparent]
  val pos_def : (x : {x : int | x > 5}) -> {u : unit | pos x === true}
    @@ total
end = struct
  let[@def transparent] pos (x : int) = x > 5
  let (pos_def @ total) (x : {x : int | x > 5}) :
      {u : unit | pos x === true} = ()
end;;
[%%expect{|
module Conditional :
  sig
    val pos : int -> bool @@ total
    val pos_def : (x : {x : int | x > 5}) -> {u : unit | (pos x) === true} @@
      total
  end
|}]

let conditional (x : {x : int | x > 7}) : {r : bool | r} = Conditional.pos x;;
[%%expect{|
val conditional : {x : int | x > 7} -> {r : bool | r} = <fun>
|}]

let outside (x : {x : int | Conditional.pos x}) : {r : int | r > 5} = x;;
[%%expect{|
Line 1, characters 70-71:
1 | let outside (x : {x : int | Conditional.pos x}) : {r : int | r > 5} = x;;
                                                                          ^
Error: Refinement could not be proved (counterexample: x = 0)
Line 1, characters 61-66:
1 | let outside (x : {x : int | Conditional.pos x}) : {r : int | r > 5} = x;;
                                                                 ^^^^^
  The refinement is stated here.
|}]

(* A refinement under a type constructor cannot become a premise. *)
module Deep = struct
  let[@def transparent] all (xs : {x : int | x > 0} list) =
    match xs with [] -> true | _ :: _ -> true
end;;
[%%expect{|
module Deep :
  sig
    val all : {x : int | x > 0} list -> bool
    val all_def :
      (xs : {x : int | x > 0} list) ->
      {u : unit | (all xs) === (match xs with | [] -> true | _::_ -> true)}
  end
|}]

let deep (xs : {x : int | x > 0} list) : {r : bool | r} = Deep.all xs;;
[%%expect{|
Line 1, characters 58-69:
1 | let deep (xs : {x : int | x > 0} list) : {r : bool | r} = Deep.all xs;;
                                                              ^^^^^^^^^^^
Error: The transparent definition Deep.all cannot be unfolded: all_def must be a total lemma stating its definition, with no refinement nested inside a parameter type
|}]

(* The lemma must state the definition of this name. *)
let shadowed () : {r : int | r = 5} =
  let[@def transparent] five (x : int) = x = 5 in
  let five_def (_ : int) = () in
  let (y : {y : int | five y}) = 5 in
  y;;
[%%expect{|
Line 3, characters 6-14:
3 |   let five_def (_ : int) = () in
          ^^^^^^^^
Warning 26 [unused-var]: unused variable "five_def".

Line 4, characters 33-34:
4 |   let (y : {y : int | five y}) = 5 in
                                     ^
Error: Refinement could not be proved (counterexample)
Line 4, characters 22-28:
4 |   let (y : {y : int | five y}) = 5 in
                          ^^^^^^
  The refinement is stated here.
|}]
