(* TEST
 has-z3;
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -principal";
 module = "sat_cdcl_array_bound_rejected.ml";
 ocamlc_opt_exit_status = "2";
 ocamlc.opt;
 check-ocamlc.opt-output;
*)

external length : int array -> int @@ total = "%array_length"
external get :
  (values : int array) ->
  {index : int | 0 <= index && index < length values} ->
  int @@ total = "%array_safe_get"
