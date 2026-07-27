(* TEST
 readonly_files = "differential_cases.py differential_gate.py";
 setup-ocamlc.byte-build-env;
 script = "python3 ${test_source_directory}/differential_gate.py \
           --ocamlrun ${ocamlrun} --ocamlc ${ocamlc_byte} \
           --ocamlc-opt ${ocamlsrcdir}/ocamlc.opt \
           --ocamlopt-opt ${ocamlsrcdir}/ocamlopt.opt \
           --backend oxsmt --profile routine --jobs 4";
 script;
*)

(* The gate compares what the compiled program computes with what the backend
   proves, over the operations whose two meanings could differ.  The driver
   holds the case table and the comparison; this file only chooses a backend
   and a size.

   This arm runs the broad routine table because its solver is in the
   compiler process and an obligation here costs about a third of what one
   costs through an external solver.  The whole table, the interior of the
   range, and Lean run in [differential_sweep.sh].

   This file is deliberately empty of obligations.  Anything written here
   would be checked against an answer someone wrote down, which is the shape
   of test the gate exists to replace.  The obligations are generated from
   what the compiled program produced, and each is compiled on its own,
   because the compiler stops at the first one it cannot discharge and a
   single disagreement would otherwise hide every later case. *)
