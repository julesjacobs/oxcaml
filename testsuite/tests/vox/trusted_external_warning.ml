(* TEST
 flags = "-extension refinement_types";
 expect;
*)

(* An external whose type states a refinement or totality is an axiom: the
   verifier assumes it and nothing checks it. Outside the verified library,
   declaring one is warned about. *)
external bogus : unit -> {u : unit | false} @@ total = "%identity";;
[%%expect{|
Line 1, characters 0-66:
1 | external bogus : unit -> {u : unit | false} @@ total = "%identity";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 228 [trusted-external]: The verifier assumes this external's refinement;
  nothing checks it.

external bogus : unit -> {u : unit | false} = "%identity"
|}]

external physical : int list -> int list -> bool @@ total = "%eq";;
[%%expect{|
Line 1, characters 0-65:
1 | external physical : int list -> int list -> bool @@ total = "%eq";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 228 [trusted-external]: The verifier assumes this external's totality;
  nothing checks it.

external physical : int list -> int list -> bool = "%eq"
|}]

(* The standard library's totality cast. *)
external trust_total : 'a -> 'a @ total = "%identity";;
[%%expect{|
Line 1, characters 0-53:
1 | external trust_total : 'a -> 'a @ total = "%identity";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 228 [trusted-external]: The verifier assumes this external's cast of its argument to a total function;
  nothing checks it.

external trust_total : 'a -> 'a @ total = "%identity"
|}]

(* A refinement behind an abbreviation or in a record field counts too. *)
type positive = {x : int | x > 0}
type box = {contents : positive}
external positive : int -> positive = "%identity"
external box : int -> box = "%identity";;
[%%expect{|
type positive = {x : int | x > 0}
type box = { contents : positive; }
Line 3, characters 0-49:
3 | external positive : int -> positive = "%identity"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 228 [trusted-external]: The verifier assumes this external's refinement;
  nothing checks it.

external positive : int -> positive = "%identity"
Line 4, characters 0-39:
4 | external box : int -> box = "%identity";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 61 [unboxable-type-in-prim-decl]: This primitive declaration uses type "box",
  whose representation may be either boxed or unboxed. Without an annotation
  to indicate which representation is intended, the boxed representation has
  been selected by default. This default choice may change in future versions
  of the compiler, breaking the primitive implementation. You should
  explicitly annotate the declaration of "box" with "[@@boxed]" or "[@@unboxed]",
  so that its external interface remains stable in the future.

Line 4, characters 0-39:
4 | external box : int -> box = "%identity";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 228 [trusted-external]: The verifier assumes this external's refinement;
  nothing checks it.

external box : int -> box = "%identity"
|}]

module type S = sig
  external f : int -> {x : int | x > 0} = "%identity"
end;;
[%%expect{|
Line 2, characters 2-53:
2 |   external f : int -> {x : int | x > 0} = "%identity"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 228 [trusted-external]: The verifier assumes this external's refinement;
  nothing checks it.

module type S = sig external f : int -> {x : int | x > 0} = "%identity" end
|}]

(* An external with neither is not warned about, and the warning can be
   disabled where the axiom is intended. *)
external plain : int -> int = "%identity"
external intended : unit -> {u : unit | true} = "%identity"
  [@@warning "-trusted-external"];;
[%%expect{|
external plain : int -> int = "%identity"
external intended : unit -> {u : unit | true} = "%identity"
|}]
