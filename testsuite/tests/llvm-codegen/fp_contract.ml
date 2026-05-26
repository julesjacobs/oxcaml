(* TEST
 macos;
 arch_arm64;
 readonly_files = "fp_contract.sh";
 script = "sh ${test_source_directory}/fp_contract.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)
