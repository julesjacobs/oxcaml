(* TEST
 has-z3;
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -principal";
 module = "sat_cdcl_totality_rejected.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let (increment @ total) : int -> int =
  fun value ->
  let cell = ref value in
  cell := !cell + 1;
  !cell
