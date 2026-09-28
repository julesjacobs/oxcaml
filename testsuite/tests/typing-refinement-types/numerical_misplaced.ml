(* TEST
 flags = "-extension refinement_types";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let misplaced = (fun n -> n) [@decreases 0]
