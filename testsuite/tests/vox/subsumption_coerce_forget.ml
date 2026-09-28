(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Forgetful coercions (DESIGN.md 3.1, subtype_rec): (e :> T) where T only
   forgets refinements, covariantly, or adds them to parameters.  No run-time
   code and no solver query.  (e :> T) needs e's type to be known (closed) or
   given (e : T0 :> T); build_subtype does not invent refinements.  Each block
   is the output after stages 1-4; comments say what trunk does. *)

let (g @ total) (x : int) : {r : int | r > 0} = if x > 0 then x else 1
let (d @ total) (x : int) : {r : int | r >= x} = x
let l : {x : int | x > 0} list = [1; 2]
let p : {x : int | x > 0} * {y : int | y > 1} = (1, 2)
let o : {x : int | x > 0} option = Some 3
type ('a : immutable_data) box = { v : 'a }
let b : {x : int | x > 0} box = { v = 1 };;
[%%expect{|
val g : int -> {r : int | r > 0} = <fun>
val d : (x : int) -> {r : int | r >= x} = <fun>
val l : {x : int | x > 0} list = [1; 2]
val p : {x : int | x > 0} * {y : int | y > 1} = (1, 2)
val o : {x : int | x > 0} option = Some 3
type ('a : immutable_data) box = { v : 'a; }
val b : {x : int | x > 0} box = {v = 1}
|}]

(* [stage 1] Forget a result refinement.
   Currently: 'Type "int -> {r : int | r > 0}" is not a subtype of
   "int -> int"'. *)
let h1 = (g :> int -> int);;
[%%expect{|
val h1 : int -> int = <fun>
|}]

(* [stage 1] ... and pass it to List.map at int.
   Currently: the same error. *)
let h2 xs = List.map (g :> int -> int) xs;;
[%%expect{|
val h2 : int list -> int list = <fun>
|}]

(* [stage 1] A dependent function: the binder is only on the source side.
   Currently: 'Type "(x : int) -> {r : int | r >= x}" is not a subtype of
   "int -> int"' (align_arrow_codomains). *)
let d1 = (d :> int -> int);;
[%%expect{|
val d1 : int -> int = <fun>
|}]

(* [stage 1] A dependent function through List.map, which fails without
   the coercion (see a2 below).
   Currently: the same error as d1. *)
let d2 xs = List.map (d :> int -> int) xs;;
[%%expect{|
val d2 : int list -> int list = <fun>
|}]

(* [stage 1] With an explicit source type.
   Currently: 'Type "(x : int) -> {r : int | r >= x}" is not a subtype of
   "int -> int"'. *)
let d3 = (d : (x : int) -> {r : int | r >= x} :> int -> int);;
[%%expect{|
val d3 : int -> int = <fun>
|}]

(* [stage 1] Refined lists fed to functions at int (the inference problem t2
   and t4 of subsumption.md, solved with an explicit coercion).
   Currently: 'Type "{x : int | x > 0} list" is not a subtype of "int list"'. *)
let s = List.fold_left (fun a b -> a + b) 0 (l :> int list)
let incr = List.map (fun x -> x + 1) (l :> int list);;
[%%expect{|
val s : int = 3
val incr : int list = [2; 3]
|}]

(* [stage 1] Tuples, options and a record with a covariant parameter.
   Currently: each is "not a subtype", e.g. 'Type "{x : int | x > 0} option"
   is not a subtype of "int option"'. *)
let p2 = (p :> int * int)
let o2 = (o :> int option)
let b2 = (b :> int box);;
[%%expect{|
val p2 : int * int = (1, 2)
val o2 : int option = Some 3
val b2 : int box = {v = 1}
|}]

(* [stage 1] Adding a parameter refinement (the target's callers promise
   more); subtype_rec swaps the parameter, so this is a drop.
   Currently: 'Type "int -> int" is not a subtype of "{x : int | x > 0} -> int"
   / Type "{x : int | x > 0}" is not a subtype of "int"'. *)
let k (x : int) = x
let k2 = (k :> {x : int | x > 0} -> int);;
[%%expect{|
val k : int -> int = <fun>
val k2 : {x : int | x > 0} -> int = <fun>
|}]

(* [stage 5, unchanged] An ascription is unification, not subsumption.
   Stage 5 would accept this.  Currently: the same error. *)
let a1 = (g : int -> int);;
[%%expect{|
Line 1, characters 10-11:
1 | let a1 = (g : int -> int);;
              ^
Error: The value "g" has type "int -> {r : int | r > 0}"
       but an expression was expected of type "int -> int"
       Type "{r : int | r > 0}" is not compatible with type "int"
|}]

(* [stage 5, unchanged] A dependent function does not unify with 'a -> 'b.
   Currently: the same error. *)
let a2 xs = List.map d xs;;
[%%expect{|
Line 1, characters 21-22:
1 | let a2 xs = List.map d xs;;
                         ^
Error: The value "d" has type "(x : int) -> {r : int | r >= x}"
       but an expression was expected of type "'a -> 'b"
|}]

(* [stage 5, unchanged] testsuite/tests/vox/implicit_refinements.ml:117-125;
   stage 5 would flip it.  Currently: the same error. *)
let refined_elements : {n : int | n >= 0} list = [0; 1]
let head = function [] -> 0 | x :: _ -> x + 1
let use_elements = head refined_elements;;
[%%expect{|
val refined_elements : {n : int | n >= 0} list = [0; 1]
val head : int list -> int = <fun>
Line 3, characters 24-40:
3 | let use_elements = head refined_elements;;
                            ^^^^^^^^^^^^^^^^
Error: The value "refined_elements" has type "{n : int | n >= 0} list"
       but an expression was expected of type "int list"
       Type "{n : int | n >= 0}" is not compatible with type "int"
|}]
