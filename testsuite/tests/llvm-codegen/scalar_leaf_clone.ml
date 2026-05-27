(* TEST
 macos;
 arch_arm64;
 readonly_files = "scalar_leaf_clone.sh";
 script = "sh ${test_source_directory}/scalar_leaf_clone.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)
