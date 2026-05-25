(* TEST
 macos;
 arch_arm64;
 runtime5;
 readonly_files = "compile_challenges.sh";
 script = "sh ${test_source_directory}/compile_challenges.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)
