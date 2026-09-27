(* TEST
 has-z3;
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -principal";
 module = "sat_cdcl_loop_totality_rejected.ml";
 ocamlc_opt_exit_status = "2";
 ocamlc.opt;
 check-ocamlc.opt-output;
*)

let (bounded_loop @ total) : unit -> unit =
  fun () -> while false do () done
