(* TEST
 readonly_files = "solver_cache_process.sh solver_cache_target.ml";
 setup-ocamlc.byte-build-env;
 script = "sh solver_cache_process.sh ${ocamlrun} ${ocamlc_byte} \
           solver_cache_target.ml";
 script;
*)

(* The script invokes a fresh compiler process twice and checks the persistent
   cache's debug events. *)

