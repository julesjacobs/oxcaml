(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-principal -vox-backend z3 -c";
 compiler_output = "smt_nonregular_principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/smt_nonregular_principal.reference";
 check-ocamlc.byte-output;
*)

type ('a, 'b) t = C of ('a list, 'a option) t

let reflexive (x : (int, bool) t @ logical) : unit{ x = x } = ()
