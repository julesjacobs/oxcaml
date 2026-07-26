(* TEST
 readonly_files = "differential_cases.py differential_gate.py";
 setup-ocamlc.byte-build-env;
 script = "python3 ${test_source_directory}/differential_gate.py \
           --ocamlrun ${ocamlrun} --ocamlc ${ocamlc_byte} \
           --ocamlc-opt ${ocamlsrcdir}/ocamlc.opt \
           --ocamlopt-opt ${ocamlsrcdir}/ocamlopt.opt \
           --backend oxsmt --profile core --jobs 2";
 script;
*)

(* The same case table as the z3 gate, against the other SMT path.  Running
   both is the point: the two translate independently, so agreeing separately
   is evidence where agreeing once is not. *)
