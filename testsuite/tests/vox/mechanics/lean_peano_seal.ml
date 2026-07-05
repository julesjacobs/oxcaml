(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/peano.mli ../lib/peano.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Peano numbers specced as Lean's built-in [Nat], across a MODULE
   BOUNDARY.  peano.mli declares [type t : value refines (lnat)] with
   [lnat] a ghost sort naming [Nat] and NO [%%vox.lean] block; peano.ml
   implements [t] as a unary [pnat] via [toN : lnat] and PROVES the
   arithmetic specs (no [assume_unchecked_]).  A client binds [Peano.t]
   at [Nat] and reasons in pure arithmetic with no view of [pnat] and no
   imported theory (Nat is native). *)

open Peano

(* THE AUTOMATION PAYOFF: commutativity through the abstraction, by
   LINEAR ARITHMETIC.  [add a b] denotes [a + b]; the spec demands
   [b + a]; grind closes [a + b = b + a] at the [Nat] image directly.
   On the unary [pnat] representation the same fact needs an induction
   (add_comm) -- the built-in model hands it to the client for free. *)
let add_comm : (a : t) -> (b : t) -> t{ _ = b + a } =
  fun a b -> add a b

(* A CONCRETE COMPUTATION: 2 + 3 = 5, built from [zero]/[succ] and
   discharged by grind evaluating the Nat literals. *)
let five : unit -> t{ _ = 5 } =
  fun () ->
    let z = zero in
    let one = succ z in
    let two = succ one in
    let three = succ two in
    add two three
