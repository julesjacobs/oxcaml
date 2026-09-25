(* TEST
 has-z3;
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -principal";
 module = "sat_cdcl_loop_totality_rejected.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let (bounded_loop @ total) : unit -> unit =
  fun () -> while false do () done
