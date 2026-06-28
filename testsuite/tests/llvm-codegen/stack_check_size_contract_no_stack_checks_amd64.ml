(* TEST
 arch_amd64;
 no-stack-checks;
 readonly_files = "stack_check_size_contract.sh";
 script = "sh ${test_source_directory}/stack_check_size_contract.sh no-stack-checks";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* This variant checks that no-stack-checks AMD64 builds do not request either
   LLVM prologue checks or ordinary CFG stack checks. *)
