(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* The verifier's built-in meanings of C primitives belong to the library
   declarations that name them. An external elsewhere that names the same C
   symbol gets none: its native name, here that of subtraction, may differ from
   its bytecode name. *)
external plus : Bigint.t -> Bigint.t -> Bigint.t
  = "caml_bigint_add" "caml_bigint_sub"

let user () : {r : Bigint.t | r = Bigint.of_int 5} =
  plus (Bigint.of_int 2) (Bigint.of_int 3);;
[%%expect{|
external plus : Bigint.t -> Bigint.t -> Bigint.t = "caml_bigint_add"
  "caml_bigint_sub"
Line 5, characters 2-42:
5 |   plus (Bigint.of_int 2) (Bigint.of_int 3);;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 4, characters 30-49:
4 | let user () : {r : Bigint.t | r = Bigint.of_int 5} =
                                  ^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* The library's declaration keeps its meaning, also through an alias. *)
module B = Bigint

let library () : {r : Bigint.t | r = Bigint.of_int 5} =
  B.add (Bigint.of_int 2) (Bigint.of_int 3);;
[%%expect{|
module B = Bigint
val library : unit -> {r : Bigint.t | r = (Bigint.of_int 5)} = <fun>
|}]

external ctz : int -> int = "caml_vox_int_ctz" "caml_vox_int_ctz_untagged"
  [@@untagged] [@@noalloc]

let zero () : {r : int | r = 63} = ctz 0;;
[%%expect{|
external ctz : int -> int = "caml_vox_int_ctz" "caml_vox_int_ctz_untagged"
  [@@untagged] [@@noalloc]
Line 4, characters 35-40:
4 | let zero () : {r : int | r = 63} = ctz 0;;
                                       ^^^^^
Error: Refinement could not be proved (counterexample)
Line 4, characters 25-31:
4 | let zero () : {r : int | r = 63} = ctz 0;;
                             ^^^^^^
  The refinement is stated here.
|}]
