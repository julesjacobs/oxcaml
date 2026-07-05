(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/peano.mli ../lib/peano.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* CONCRETE WITNESS at the built-in-Nat interface.  A client calls
   [add a b] -- abstract result at sort [Nat], denoting [a + b] -- and
   overclaims [a + b + 1].  Because the result IS a [Nat] (not a via
   value carrying a skeleton), the solver refutes [a + b = a + b + 1]
   with a CONCRETE counterexample.  This is the witness the [Int -> Prop]
   model (lean_xset.ml) cannot give for a set-level equality: the
   built-in model keeps decidable-arithmetic counterexamples while still
   automating the linear reasoning the unary representation needs
   induction for. *)

open Peano

let bad : (a : t) -> (b : t) -> t{ _ = a + b + 1 } =
  fun a b -> add a b
