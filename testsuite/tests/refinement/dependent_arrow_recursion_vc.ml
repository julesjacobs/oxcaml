(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-vox-dump-vc -c";
 compiler_output = "dependent_arrow_recursion_vc.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_recursion_vc.reference";
 check-ocamlc.byte-output;
 flags = "-principal -vox-dump-vc -c";
 compiler_output = "dependent_arrow_recursion_vc.principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_recursion_vc.reference";
 check-ocamlc.byte-output;
*)

let rec step (n : int) : int{ _ = n } =
  if n = 0 then 0 else step (n - 1) + 1
