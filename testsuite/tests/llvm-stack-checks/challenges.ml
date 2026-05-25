(* TEST
 macos;
 arch_arm64;
 runtime5;
 readonly_files = "challenges.sh";
 script = "sh ${test_source_directory}/challenges.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)
