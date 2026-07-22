(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-principal -vox-backend z3 -c";
 compiler_output = "smt_nonproductive_principal.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/smt_nonproductive_principal.reference";
 check-ocamlc.byte-output;
*)

type t = Loop of t

let reflexive (x : t @ logical) : unit{ x = x } = ()
