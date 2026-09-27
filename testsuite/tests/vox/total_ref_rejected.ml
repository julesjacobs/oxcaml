(* TEST
 has-z3;
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -principal";
 module = "sat_cdcl_totality_rejected.ml";
 ocamlc_opt_exit_status = "2";
 ocamlc.opt;
 check-ocamlc.opt-output;
*)

let (increment @ total) : int -> int =
  fun value ->
  let cell = ref value in
  cell := !cell + 1;
  !cell
