(* TEST
 readonly_files = "\
   normal_exit_condition_api.mli normal_exit_condition_api.ml \
   normal_exit_condition_reexport.mli normal_exit_condition_reexport.ml \
   normal_exit_condition_client.ml \
 ";
 setup-ocamlc.byte-build-env;

 module = "normal_exit_condition_api.mli";
 ocamlc.byte;
 module = "normal_exit_condition_api.ml";
 ocamlc.byte;

 module = "normal_exit_condition_reexport.mli";
 ocamlc.byte;
 module = "normal_exit_condition_reexport.ml";
 ocamlc.byte;

 module = "normal_exit_condition_client.ml";
 ocamlc.byte;
*)

(* A total-call result contract remains available to a branch observation
   after both a CMI boundary and a re-export. *)
