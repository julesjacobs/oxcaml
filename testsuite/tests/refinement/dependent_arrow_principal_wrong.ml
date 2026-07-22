(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-principal -c";
 compiler_output = "dependent_arrow_principal_wrong.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_principal_wrong.reference";
 check-ocamlc.byte-output;
*)

let nested_wrong (outer : int) =
  let inner (value : int) : unit{ outer = value } = () in
  inner
