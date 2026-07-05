(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/rbt.mli ../lib/rbt_bad_balance/rbt.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A [balance] whose first (left-left) rotation is miswired -- it swaps
   the [lr] and [r] subtrees when rebuilding.  Sealed by the real
   rbt.mli (whose [balance] spec is model equality), the bug is caught
   at the balance layer: the produced tree is not the model [balance],
   with a concrete counterexample.  A miswired rotation never even
   reaches the ordering / colour / black-height obligations of [add];
   the model-equality contract on [balance] stops it first. *)
