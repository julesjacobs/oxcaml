(* TEST
 arch_amd64;
 readonly_files = "poll_statepoint.sh";
 script = "sh ${test_source_directory}/poll_statepoint.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage that a CFG [Poll] reaches LLVM as a poll statepoint and is
   encoded in the frametable. *)
