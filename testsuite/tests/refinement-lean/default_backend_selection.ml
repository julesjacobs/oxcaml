(* TEST
 readonly_files = "default_backend_selection.sh solver_cache_target.ml";
 setup-ocamlc.byte-build-env;
 script = "sh default_backend_selection.sh ${ocamlrun} ${ocamlc_byte} \
           solver_cache_target.ml";
 script;
*)

(* The script compiles the target with the default and with every explicit
   backend selection, then checks the cache's backend-qualified debug events. *)
