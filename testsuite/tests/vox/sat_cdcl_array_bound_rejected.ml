(* TEST
 has-z3;
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -principal";
 module = "sat_cdcl_array_bound_rejected.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

external length : int array -> int @@ total = "%array_length"
external get :
  (values : int array) ->
  {index : int | 0 <= index && index < length values} ->
  int @@ total = "%array_safe_get"
