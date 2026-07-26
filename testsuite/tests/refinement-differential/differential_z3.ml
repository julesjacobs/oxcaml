(* TEST
 readonly_files = "differential_cases.py differential_gate.py";
 setup-ocamlc.byte-build-env;
 script = "python3 ${test_source_directory}/differential_gate.py \
           --ocamlrun ${ocamlrun} --ocamlc ${ocamlc_byte} \
           --ocamlc-opt ${ocamlsrcdir}/ocamlc.opt \
           --ocamlopt-opt ${ocamlsrcdir}/ocamlopt.opt \
           --backend z3 --profile core --jobs 2";
 script;
*)

(* The gate compares what the compiled program computes with what the backend
   proves, over the operations whose two meanings could differ.  The driver
   holds the case table and the comparison; this file only chooses a backend
   and a size.

   This file is deliberately empty of obligations.  Anything written here
   would be checked against an answer someone wrote down, which is the shape
   of test the gate exists to replace.  The obligations are generated from
   what the compiled program produced, and each is compiled on its own,
   because the compiler stops at the first one it cannot discharge and a
   single disagreement would otherwise hide every later case. *)
