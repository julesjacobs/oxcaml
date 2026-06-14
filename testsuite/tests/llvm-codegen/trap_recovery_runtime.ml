(* TEST
 macos;
 arch_arm64;
 readonly_files = "trap_recovery_runtime.sh";
 script = "sh ${test_source_directory}/trap_recovery_runtime.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)
