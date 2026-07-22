(* TEST
 readonly_files = "\
   polyset.mli polyset.ml ordered_int.mli ordered_int.ml \
   wrong_insert.ml wrong_insert.reference \
 ";
 setup-ocamlc.byte-build-env;
 module = "polyset.mli";
 ocamlc.byte;
 module = "ordered_int.mli";
 ocamlc.byte;
 module = "ordered_int.ml";
 ocamlc.byte;
 module = "polyset.ml";
 ocamlc.byte;
 module = "wrong_insert.ml";
 flags = "-I ocamlc.byte";
 compiler_output = "wrong_insert.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/wrong_insert.reference";
 check-ocamlc.byte-output;
*)
