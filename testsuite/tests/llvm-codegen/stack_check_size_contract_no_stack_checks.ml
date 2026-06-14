(* TEST
 macos;
 arch_arm64;
 no-stack-checks;
 readonly_files = "stack_check_size_contract.sh";
 script = "sh ${test_source_directory}/stack_check_size_contract.sh no-stack-checks";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* This variant runs only in no-stack-checks configurations. *)
