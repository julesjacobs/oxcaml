(* TEST
 readonly_files = "\
   smt_nonregular_compiler_test.sh \
   smt_nonregular_marker_solver.sh \
   smt_nonregular_productive.ml \
   smt_nonregular_constructor.ml \
 ";
 setup-ocamlc.byte-build-env;
 script = "sh smt_nonregular_compiler_test.sh \
   ${ocamlrun} ${ocamlc_byte} smt_nonregular_productive.ml productive.json";
 script;
 script = "sh smt_nonregular_compiler_test.sh \
   ${ocamlrun} ${ocamlc_byte} smt_nonregular_constructor.ml constructor.json";
 script;
*)
