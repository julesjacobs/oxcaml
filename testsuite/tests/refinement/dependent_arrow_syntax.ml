(* TEST
 readonly_files = "\
   dependent_arrow_syntax_api.mli dependent_arrow_syntax_api.ml \
   dependent_arrow_syntax_client.ml dependent_arrow_print.ml \
   dependent_arrow_print.reference \
 ";
 setup-ocamlc.byte-build-env;

 module = "dependent_arrow_syntax_api.mli";
 ocamlc.byte;
 module = "dependent_arrow_syntax_api.ml";
 ocamlc.byte;
 module = "dependent_arrow_syntax_client.ml";
 ocamlc.byte;

 flags = "-principal";
 module = "dependent_arrow_syntax_api.mli";
 ocamlc.byte;
 module = "dependent_arrow_syntax_api.ml";
 ocamlc.byte;
 module = "dependent_arrow_syntax_client.ml";
 ocamlc.byte;

 module = "dependent_arrow_print.ml";
 flags = "-i";
 compiler_output = "dependent_arrow_print.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_print.reference";
 check-ocamlc.byte-output;
*)
