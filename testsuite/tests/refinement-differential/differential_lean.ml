(* TEST
 readonly_files = "differential_cases.py differential_gate.py";
 setup-ocamlc.byte-build-env;
 script = "python3 ${test_source_directory}/differential_gate.py \
           --ocamlrun ${ocamlrun} --ocamlc ${ocamlc_byte} \
           --ocamlc-opt ${ocamlsrcdir}/ocamlc.opt \
           --ocamlopt-opt ${ocamlsrcdir}/ocamlopt.opt \
           --backend lean --profile lean";
 script;
*)

(* Lean costs several times what an SMT obligation costs and is held to one
   process at a time, so it runs a thinner case table: every operator keeps
   the operands where a wrong translation would show, and the rest of the core
   runs against Lean in the offline sweep. *)
