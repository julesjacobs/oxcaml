(* TEST
 has-z3;
 flags = "-extension refinement_types";
 {
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  check-ocamlc.byte-output;
  file = "numerical_no_cmi.cmi";
  file-not-exists;
 }
 {
  setup-ocamlopt.byte-build-env;
  ocamlopt_byte_exit_status = "2";
  ocamlopt.byte;
  check-ocamlopt.byte-output;
  file = "numerical_no_cmi.cmi";
  file-not-exists;
 }
*)

let rec loop n = loop n [@@decreases n]
