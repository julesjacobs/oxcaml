(* TEST
 macos;
 arch_arm64;
 readonly_files = "poll_statepoint.sh";
 script = "sh ${test_source_directory}/poll_statepoint.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks that a CFG [Poll] reaches LLVM as a poll statepoint. *)
