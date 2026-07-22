(* TEST
 readonly_files = "\
   dependent_arrow_unstable_calls_ppx.ml \
   dependent_arrow_unstable_calls_source.ml \
   dependent_arrow_unstable_calls.reference \
 ";
 setup-ocamlc.byte-build-env;
 all_modules = "dependent_arrow_unstable_calls_ppx.ml";
 program = "unstable_calls_ppx.exe";
 ocamlc.byte with ocamlcommon;
 module = "dependent_arrow_unstable_calls_source.ml";
 flags = "-ppx '${ocamlrun} ${test_build_directory_prefix}/ocamlc.byte/unstable_calls_ppx.exe'";
 compiler_output = "dependent_arrow_unstable_calls.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_unstable_calls.reference";
 check-ocamlc.byte-output;
 flags = "-principal -ppx '${ocamlrun} ${test_build_directory_prefix}/ocamlc.byte/unstable_calls_ppx.exe'";
 compiler_output = "dependent_arrow_unstable_calls.principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_unstable_calls.reference";
 check-ocamlc.byte-output;
*)
